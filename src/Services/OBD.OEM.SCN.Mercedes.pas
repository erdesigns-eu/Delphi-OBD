//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.SCN.Mercedes.pas
// CONTENTS       : Mercedes-Benz SCN coding flow framing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.SCN.Mercedes;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDMBSCN = class(Exception);
  EOBDMBSCNNoSolver = class(EOBDMBSCN);

  TMBSCNVersionRequest = record
    VIN: string;          // 17 ASCII chars
    ECUId: Word;          // XENTRY ECU identifier
  end;

  TMBSCNVersionResponse = record
    CurrentSCN: TBytes;   // typically 8..16 bytes opaque
    HardwareNum: string;  // ASCII part number
    SoftwareNum: string;  // ASCII part number
  end;

  TMBSCNCodingRequest = record
    /// <summary>
    ///   Vin.
    /// </summary>
    VIN: string;
    /// <summary>
    ///   Ecu id.
    /// </summary>
    ECUId: Word;
    Variant: TBytes;             // OEM variant code per ECU
    AccessoryList: TBytes;       // OEM accessory bitmap / list
  end;

  TMBSCNCodingResponse = record
    /// <summary>
    ///   New scn.
    /// </summary>
    NewSCN: TBytes;
    ServerSignature: TBytes;     // server-side signature, opaque
  end;

  IMBSCNSolver = interface
    ['{0A8F4B2D-8E1C-4D3A-B7E9-1F4C9E8D7A11}']
    function FetchCurrentVersion(const Req: TMBSCNVersionRequest):
      TMBSCNVersionResponse;
    /// <summary>
    ///   Request coding.
    /// </summary>
    function RequestCoding(const Req: TMBSCNCodingRequest):
      TMBSCNCodingResponse;
  end;

  TMBSCNSolverNotAvailable = class(TInterfacedObject, IMBSCNSolver)
  public
    /// <summary>
    ///   Fetch current version.
    /// </summary>
    function FetchCurrentVersion(const Req: TMBSCNVersionRequest):
      TMBSCNVersionResponse;
    /// <summary>
    ///   Request coding.
    /// </summary>
    function RequestCoding(const Req: TMBSCNCodingRequest):
      TMBSCNCodingResponse;
  end;

function EncodeMBSCNVersionRequest(const Req: TMBSCNVersionRequest): TBytes;
function DecodeMBSCNVersionRequest(const Bytes: TBytes): TMBSCNVersionRequest;
function EncodeMBSCNCodingRequest(const Req: TMBSCNCodingRequest): TBytes;
function DecodeMBSCNCodingRequest(const Bytes: TBytes): TMBSCNCodingRequest;
function EncodeMBSCNCodingResponse(const Resp: TMBSCNCodingResponse): TBytes;
function DecodeMBSCNCodingResponse(const Bytes: TBytes): TMBSCNCodingResponse;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// PUT WORD
//------------------------------------------------------------------------------
function PutWord(var Out_: TBytes; Cursor: Integer; W: Word): Integer;
begin
  Out_[Cursor]     := Byte(W shr 8);
  Out_[Cursor + 1] := Byte(W and $FF);
  Result := Cursor + 2;
end;

//------------------------------------------------------------------------------
// GET WORD
//------------------------------------------------------------------------------
function GetWord(const B: TBytes; Off: Integer): Word;
begin
  Result := (UInt16(B[Off]) shl 8) or B[Off + 1];
end;

//------------------------------------------------------------------------------
// ENCODE MBSCNVERSION REQUEST
//------------------------------------------------------------------------------
function EncodeMBSCNVersionRequest(const Req: TMBSCNVersionRequest): TBytes;
var
  I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDMBSCN.CreateFmt('VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  // Layout: 17 VIN bytes + 2 ECUId bytes (BE)
  SetLength(Result, 17 + 2);
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  PutWord(Result, 17, Req.ECUId);
end;

//------------------------------------------------------------------------------
// DECODE MBSCNVERSION REQUEST
//------------------------------------------------------------------------------
function DecodeMBSCNVersionRequest(const Bytes: TBytes): TMBSCNVersionRequest;
var
  I: Integer;
begin
  if Length(Bytes) <> 19 then
    raise EOBDMBSCN.CreateFmt(
      'SCN version request must be 19 bytes (got %d)', [Length(Bytes)]);
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Result.ECUId := GetWord(Bytes, 17);
end;

//------------------------------------------------------------------------------
// ENCODE MBSCNCODING REQUEST
//------------------------------------------------------------------------------
function EncodeMBSCNCodingRequest(const Req: TMBSCNCodingRequest): TBytes;
var
  Cursor, I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDMBSCN.Create('VIN must be 17 chars');
  if Length(Req.Variant) > $FFFF then
    raise EOBDMBSCN.Create('Variant exceeds 65535 bytes');
  if Length(Req.AccessoryList) > $FFFF then
    raise EOBDMBSCN.Create('AccessoryList exceeds 65535 bytes');
  SetLength(Result,
    17                              // VIN
    + 2                             // ECUId
    + 2 + Length(Req.Variant)       // Variant
    + 2 + Length(Req.AccessoryList) // AccessoryList
  );
  for I := 0 to 16 do Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Cursor := 17;
  Cursor := PutWord(Result, Cursor, Req.ECUId);
  Cursor := PutWord(Result, Cursor, Word(Length(Req.Variant)));
  if Length(Req.Variant) > 0 then
  begin
    Move(Req.Variant[0], Result[Cursor], Length(Req.Variant));
    Inc(Cursor, Length(Req.Variant));
  end;
  PutWord(Result, Cursor, Word(Length(Req.AccessoryList)));
  Inc(Cursor, 2);
  if Length(Req.AccessoryList) > 0 then
    Move(Req.AccessoryList[0], Result[Cursor], Length(Req.AccessoryList));
end;

//------------------------------------------------------------------------------
// DECODE MBSCNCODING REQUEST
//------------------------------------------------------------------------------
function DecodeMBSCNCodingRequest(const Bytes: TBytes): TMBSCNCodingRequest;
var
  Cursor, Len, I: Integer;
begin
  if Length(Bytes) < 17 + 2 + 2 + 2 then
    raise EOBDMBSCN.Create('SCN coding request too short');
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[I]);
  Cursor := 17;
  Result.ECUId := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('Variant payload truncated');
  // Allocate Result.Variant
  SetLength(Result.Variant, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Variant[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDMBSCN.Create('AccessoryList length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('AccessoryList truncated');
  // Allocate Result.AccessoryList
  SetLength(Result.AccessoryList, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.AccessoryList[0], Len);
end;

//------------------------------------------------------------------------------
// ENCODE MBSCNCODING RESPONSE
//------------------------------------------------------------------------------
function EncodeMBSCNCodingResponse(const Resp: TMBSCNCodingResponse): TBytes;
var
  Cursor: Integer;
begin
  if Length(Resp.NewSCN) > $FFFF then
    raise EOBDMBSCN.Create('NewSCN exceeds 65535 bytes');
  if Length(Resp.ServerSignature) > $FFFF then
    raise EOBDMBSCN.Create('ServerSignature exceeds 65535 bytes');
  SetLength(Result,
    2 + Length(Resp.NewSCN) + 2 + Length(Resp.ServerSignature));
  Cursor := PutWord(Result, 0, Word(Length(Resp.NewSCN)));
  if Length(Resp.NewSCN) > 0 then
  begin
    Move(Resp.NewSCN[0], Result[Cursor], Length(Resp.NewSCN));
    Inc(Cursor, Length(Resp.NewSCN));
  end;
  PutWord(Result, Cursor, Word(Length(Resp.ServerSignature)));
  Inc(Cursor, 2);
  if Length(Resp.ServerSignature) > 0 then
    Move(Resp.ServerSignature[0], Result[Cursor], Length(Resp.ServerSignature));
end;

//------------------------------------------------------------------------------
// DECODE MBSCNCODING RESPONSE
//------------------------------------------------------------------------------
function DecodeMBSCNCodingResponse(const Bytes: TBytes): TMBSCNCodingResponse;
var
  Cursor, Len: Integer;
begin
  if Length(Bytes) < 4 then
    raise EOBDMBSCN.Create('SCN coding response too short');
  Cursor := 0;
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('NewSCN truncated');
  // Allocate Result.NewSCN
  SetLength(Result.NewSCN, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.NewSCN[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDMBSCN.Create('ServerSignature length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('ServerSignature truncated');
  // Allocate Result.ServerSignature
  SetLength(Result.ServerSignature, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.ServerSignature[0], Len);
end;

{ TMBSCNSolverNotAvailable }

//------------------------------------------------------------------------------
// FETCH CURRENT VERSION
//------------------------------------------------------------------------------
function TMBSCNSolverNotAvailable.FetchCurrentVersion(
  const Req: TMBSCNVersionRequest): TMBSCNVersionResponse;
begin
  raise EOBDMBSCNNoSolver.Create(
    'Mercedes SCN version-fetch solver not available in this build. ' +
    'Plug in a dealer-portal client or a captured (req, resp) replay ' +
    '(see docs/DATA_GAPS.md).');
end;

//------------------------------------------------------------------------------
// REQUEST CODING
//------------------------------------------------------------------------------
function TMBSCNSolverNotAvailable.RequestCoding(
  const Req: TMBSCNCodingRequest): TMBSCNCodingResponse;
begin
  raise EOBDMBSCNNoSolver.Create(
    'Mercedes SCN coding solver not available in this build (see ' +
    'docs/DATA_GAPS.md).');
end;

end.
