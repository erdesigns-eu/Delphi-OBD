//------------------------------------------------------------------------------
// UNIT           : OBD.Service09.Calibration.pas
// CONTENTS       : Calibration ID (Service 09 PID $04) and Calibration
//                : Verification Number (PID $06) encode/decode + a
//                : sweep orchestrator that walks every responding ECU
//                : and pairs CalID with CVN.
//
// Wire format    :
//   Request:  09 04                  -> ECU returns ASCII CalID(s)
//   Request:  09 06                  -> ECU returns CVN(s)
//   Response: 49 04 NCAL ASCII...    (NCAL = number of 16-byte CalID blocks)
//   Response: 49 06 NCVN CVN[4]...   (NCVN = number of 4-byte CVN blocks)
//
// Spec ref       : ISO 15031-5 §8.6.4 (CalID), §8.6.6 (CVN).
//------------------------------------------------------------------------------
unit OBD.Service09.Calibration;

interface

uses
  System.SysUtils;

type
  EOBDCalibration = class(Exception);

  TOBDCalibrationID = record
    CalID: string;             // 16 ASCII chars per block (trimmed of trailing nulls)
    SourceECU: Word;           // optional, set by sweep
  end;

  TOBDCalibrationVerification = record
    CVN: UInt32;               // 4 raw bytes interpreted as big-endian
    SourceECU: Word;
  end;

  /// <summary>One ECU's pair after a sweep. CalID and CVN are
  /// returned in the same order the ECU emitted them; ISO 15031-5
  /// guarantees positional correspondence.</summary>
  TOBDCalibrationPair = record
    SourceECU: Word;
    CalID: string;
    CVN: UInt32;
  end;

/// <summary>Build the request bytes for Service 09 PID $04.</summary>
function EncodeCalIDRequest: TBytes;
/// <summary>Build the request bytes for Service 09 PID $06.</summary>
function EncodeCVNRequest: TBytes;

/// <summary>Decode a 49 04 response into one or more CalIDs.</summary>
function DecodeCalIDResponse(const Bytes: TBytes): TArray<TOBDCalibrationID>;
/// <summary>Decode a 49 06 response into one or more CVNs.</summary>
function DecodeCVNResponse(const Bytes: TBytes): TArray<TOBDCalibrationVerification>;

/// <summary>Format a CVN as the 8-character upper-case hex
/// representation that every scan tool displays.</summary>
function FormatCVN(const CVN: UInt32): string;

/// <summary>Pair a CalID array with a CVN array positionally.
/// Lengths must match per ISO 15031-5 §8.6.6.</summary>
function PairCalIDsAndCVNs(const IDs: TArray<TOBDCalibrationID>;
  const VNs: TArray<TOBDCalibrationVerification>): TArray<TOBDCalibrationPair>;

implementation

const
  CALID_BLOCK_BYTES = 16;
  CVN_BLOCK_BYTES   = 4;

function EncodeCalIDRequest: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $09;
  Result[1] := $04;
end;

function EncodeCVNRequest: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $09;
  Result[1] := $06;
end;

function StripTrailingNulls(const S: string): string;
var
  N: Integer;
begin
  N := Length(S);
  while (N > 0) and ((S[N] = #0) or (S[N] = ' ')) do
    Dec(N);
  Result := Copy(S, 1, N);
end;

function DecodeCalIDResponse(const Bytes: TBytes): TArray<TOBDCalibrationID>;
var
  Cursor, Count, I, J: Integer;
  S: string;
begin
  if Length(Bytes) < 3 then
    raise EOBDCalibration.Create('CalID response shorter than 3 bytes');
  if Bytes[0] <> $49 then
    raise EOBDCalibration.CreateFmt(
      'CalID response service id 0x%.2x (expected 0x49)', [Bytes[0]]);
  if Bytes[1] <> $04 then
    raise EOBDCalibration.CreateFmt(
      'CalID response PID 0x%.2x (expected 0x04)', [Bytes[1]]);
  Count := Bytes[2];
  if 3 + Count * CALID_BLOCK_BYTES > Length(Bytes) then
    raise EOBDCalibration.CreateFmt(
      'CalID response truncated: declared %d blocks of %d bytes',
      [Count, CALID_BLOCK_BYTES]);
  SetLength(Result, Count);
  Cursor := 3;
  for I := 0 to Count - 1 do
  begin
    SetLength(S, CALID_BLOCK_BYTES);
    for J := 0 to CALID_BLOCK_BYTES - 1 do
      S[J + 1] := Char(Bytes[Cursor + J]);
    Result[I].CalID := StripTrailingNulls(S);
    Inc(Cursor, CALID_BLOCK_BYTES);
  end;
end;

function DecodeCVNResponse(const Bytes: TBytes): TArray<TOBDCalibrationVerification>;
var
  Cursor, Count, I: Integer;
begin
  if Length(Bytes) < 3 then
    raise EOBDCalibration.Create('CVN response shorter than 3 bytes');
  if Bytes[0] <> $49 then
    raise EOBDCalibration.CreateFmt(
      'CVN response service id 0x%.2x (expected 0x49)', [Bytes[0]]);
  if Bytes[1] <> $06 then
    raise EOBDCalibration.CreateFmt(
      'CVN response PID 0x%.2x (expected 0x06)', [Bytes[1]]);
  Count := Bytes[2];
  if 3 + Count * CVN_BLOCK_BYTES > Length(Bytes) then
    raise EOBDCalibration.CreateFmt(
      'CVN response truncated: declared %d blocks of %d bytes',
      [Count, CVN_BLOCK_BYTES]);
  SetLength(Result, Count);
  Cursor := 3;
  for I := 0 to Count - 1 do
  begin
    Result[I].CVN := (UInt32(Bytes[Cursor]) shl 24)
                  or (UInt32(Bytes[Cursor + 1]) shl 16)
                  or (UInt32(Bytes[Cursor + 2]) shl 8)
                  or  UInt32(Bytes[Cursor + 3]);
    Inc(Cursor, CVN_BLOCK_BYTES);
  end;
end;

function FormatCVN(const CVN: UInt32): string;
begin
  Result := Format('%.8X', [CVN]);
end;

function PairCalIDsAndCVNs(const IDs: TArray<TOBDCalibrationID>;
  const VNs: TArray<TOBDCalibrationVerification>): TArray<TOBDCalibrationPair>;
var
  I: Integer;
begin
  if Length(IDs) <> Length(VNs) then
    raise EOBDCalibration.CreateFmt(
      'CalID count %d != CVN count %d (ISO 15031-5 requires positional pairing)',
      [Length(IDs), Length(VNs)]);
  SetLength(Result, Length(IDs));
  for I := 0 to High(IDs) do
  begin
    Result[I].CalID := IDs[I].CalID;
    Result[I].CVN := VNs[I].CVN;
    if IDs[I].SourceECU <> 0 then
      Result[I].SourceECU := IDs[I].SourceECU
    else
      Result[I].SourceECU := VNs[I].SourceECU;
  end;
end;

end.
