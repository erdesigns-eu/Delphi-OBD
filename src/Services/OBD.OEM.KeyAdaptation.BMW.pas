//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.KeyAdaptation.BMW.pas
// CONTENTS       : BMW key adaptation framing (EWS/CAS/FEM-BDC)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.KeyAdaptation.BMW;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDBMWKey = class(Exception);
  EBMWKeyChallengeNotAvailable = class(EOBDBMWKey);

  TBMWImmoGeneration = (bmwgEWS, bmwgCAS, bmwgFEMBDC);

  /// <summary>
  ///   EWS key slot — 16 bytes per spec. Slot 0..9.
  /// </summary>
  TBMWKeyDataE = record
    SlotIndex: Byte;        // 0..9
    KeyEnabled: Boolean;    // bit set in status flags
    KeyCutCode: TBytes;     // 4 bytes — mechanical cut, OEM-defined
    UsageCounter: UInt16;   // counts ignition cycles using this key
    Reserved: TBytes;       // padding to 16 bytes
  end;

  /// <summary>
  ///   CAS key slot — 16 bytes per spec. Slot 0..9.
  /// </summary>
  TBMWKeyDataCas = record
    /// <summary>
    ///   Slot index.
    /// </summary>
    SlotIndex: Byte;
    /// <summary>
    ///   Key enabled.
    /// </summary>
    KeyEnabled: Boolean;
    KeyCutCode: TBytes;     // 4 bytes
    RemoteId: UInt32;       // remote-control identifier
    KMReadingThousands: UInt16; // odometer captured by this key
    /// <summary>
    ///   Reserved.
    /// </summary>
    Reserved: TBytes;
  end;

  /// <summary>
  ///   FEM-BDC key slot — 32 bytes (F/G-series). Slot 0..7.
  /// </summary>
  TBMWKeyDataFem = record
    /// <summary>
    ///   Slot index.
    /// </summary>
    SlotIndex: Byte;
    /// <summary>
    ///   Key enabled.
    /// </summary>
    KeyEnabled: Boolean;
    PersonalSettingsBank: Byte;  // 1..4 (driver profile binding)
    KeyCutCode: TBytes;          // 4 bytes
    DigitalKeySerial: TBytes;    // 7 bytes (CD UWB key id, 0..) or zero
    /// <summary>
    ///   Usage counter.
    /// </summary>
    UsageCounter: UInt32;
    /// <summary>
    ///   Last km reading.
    /// </summary>
    LastKMReading: UInt32;
    Reserved: TBytes;            // padding to 32 bytes
  end;

  /// <summary>
  ///   Pluggable solver for the proprietary parts:
  ///   ISN derivation per ECU and the EWS/CAS challenge-response
  ///   encryption. Production code wires a dealer-portal client here.
  /// </summary>
  IBMWKeyChallengeSolver = interface
    ['{F2DE8AB1-7DBA-4F1E-A5C0-0F9A2D0D3C50}']
    function ComputeISN(Generation: TBMWImmoGeneration;
      const ECUSerial: TBytes; const VIN: string): TBytes;
    /// <summary>
    ///   Solve challenge.
    /// </summary>
    function SolveChallenge(Generation: TBMWImmoGeneration;
      const Challenge: TBytes): TBytes;
  end;

function EncodeKeyDataE(const Key: TBMWKeyDataE): TBytes;
function DecodeKeyDataE(const Bytes: TBytes): TBMWKeyDataE;
function EncodeKeyDataCas(const Key: TBMWKeyDataCas): TBytes;
function DecodeKeyDataCas(const Bytes: TBytes): TBMWKeyDataCas;
function EncodeKeyDataFem(const Key: TBMWKeyDataFem): TBytes;
function DecodeKeyDataFem(const Bytes: TBytes): TBMWKeyDataFem;

/// <summary>
///   Validate the slot index for a given immobiliser generation.
/// </summary>
function ValidateSlotIndex(Gen: TBMWImmoGeneration; Slot: Byte): Boolean;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  EWS_SLOT_BYTES  = 16;
  CAS_SLOT_BYTES  = 16;
  FEM_SLOT_BYTES  = 32;

//------------------------------------------------------------------------------
// VALIDATE SLOT INDEX
//------------------------------------------------------------------------------
function ValidateSlotIndex(Gen: TBMWImmoGeneration; Slot: Byte): Boolean;
begin
  case Gen of
    bmwgEWS, bmwgCAS: Result := Slot <= 9;
    bmwgFEMBDC:       Result := Slot <= 7;
  else
    // Initialize result
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// WRITE BYTES PAD
//------------------------------------------------------------------------------
procedure WriteBytesPad(var Out_: TBytes; Cursor: Integer; const Src: TBytes;
  Width: Integer);
var
  N: Integer;
begin
  N := Length(Src);
  if N > Width then
    raise EOBDBMWKey.CreateFmt(
      'Source bytes (%d) exceed field width (%d)', [N, Width]);
  if N > 0 then Move(Src[0], Out_[Cursor], N);
  // remainder stays zero
end;

//------------------------------------------------------------------------------
// ENCODE KEY DATA E
//------------------------------------------------------------------------------
function EncodeKeyDataE(const Key: TBMWKeyDataE): TBytes;
var
  Status: Byte;
begin
  if not ValidateSlotIndex(bmwgEWS, Key.SlotIndex) then
    raise EOBDBMWKey.CreateFmt('EWS slot %d out of range', [Key.SlotIndex]);
  if Length(Key.KeyCutCode) <> 4 then
    raise EOBDBMWKey.Create('KeyCutCode must be 4 bytes');
  // Allocate Result
  SetLength(Result, EWS_SLOT_BYTES);
  Result[0] := Key.SlotIndex;
  Status := 0;
  if Key.KeyEnabled then Status := Status or $01;
  Result[1] := Status;
  Move(Key.KeyCutCode[0], Result[2], 4);
  Result[6] := Byte(Key.UsageCounter shr 8);
  Result[7] := Byte(Key.UsageCounter and $FF);
  if Length(Key.Reserved) > 0 then
    WriteBytesPad(Result, 8, Key.Reserved, EWS_SLOT_BYTES - 8);
end;

//------------------------------------------------------------------------------
// DECODE KEY DATA E
//------------------------------------------------------------------------------
function DecodeKeyDataE(const Bytes: TBytes): TBMWKeyDataE;
begin
  if Length(Bytes) <> EWS_SLOT_BYTES then
    raise EOBDBMWKey.CreateFmt(
      'EWS slot must be %d bytes (got %d)', [EWS_SLOT_BYTES, Length(Bytes)]);
  Result := Default(TBMWKeyDataE);
  Result.SlotIndex := Bytes[0];
  Result.KeyEnabled := (Bytes[1] and $01) <> 0;
  // Allocate Result.KeyCutCode
  SetLength(Result.KeyCutCode, 4);
  Move(Bytes[2], Result.KeyCutCode[0], 4);
  Result.UsageCounter := (UInt16(Bytes[6]) shl 8) or Bytes[7];
  // Allocate Result.Reserved
  SetLength(Result.Reserved, EWS_SLOT_BYTES - 8);
  Move(Bytes[8], Result.Reserved[0], EWS_SLOT_BYTES - 8);
end;

//------------------------------------------------------------------------------
// ENCODE KEY DATA CAS
//------------------------------------------------------------------------------
function EncodeKeyDataCas(const Key: TBMWKeyDataCas): TBytes;
var
  Status: Byte;
begin
  if not ValidateSlotIndex(bmwgCAS, Key.SlotIndex) then
    raise EOBDBMWKey.CreateFmt('CAS slot %d out of range', [Key.SlotIndex]);
  if Length(Key.KeyCutCode) <> 4 then
    raise EOBDBMWKey.Create('KeyCutCode must be 4 bytes');
  // Allocate Result
  SetLength(Result, CAS_SLOT_BYTES);
  Result[0] := Key.SlotIndex;
  Status := 0;
  if Key.KeyEnabled then Status := Status or $01;
  Result[1] := Status;
  Move(Key.KeyCutCode[0], Result[2], 4);
  Result[6]  := Byte(Key.RemoteId shr 24);
  Result[7]  := Byte(Key.RemoteId shr 16);
  Result[8]  := Byte(Key.RemoteId shr 8);
  Result[9]  := Byte(Key.RemoteId and $FF);
  Result[10] := Byte(Key.KMReadingThousands shr 8);
  Result[11] := Byte(Key.KMReadingThousands and $FF);
  if Length(Key.Reserved) > 0 then
    WriteBytesPad(Result, 12, Key.Reserved, CAS_SLOT_BYTES - 12);
end;

//------------------------------------------------------------------------------
// DECODE KEY DATA CAS
//------------------------------------------------------------------------------
function DecodeKeyDataCas(const Bytes: TBytes): TBMWKeyDataCas;
begin
  if Length(Bytes) <> CAS_SLOT_BYTES then
    raise EOBDBMWKey.CreateFmt(
      'CAS slot must be %d bytes (got %d)', [CAS_SLOT_BYTES, Length(Bytes)]);
  Result := Default(TBMWKeyDataCas);
  Result.SlotIndex := Bytes[0];
  Result.KeyEnabled := (Bytes[1] and $01) <> 0;
  // Allocate Result.KeyCutCode
  SetLength(Result.KeyCutCode, 4);
  Move(Bytes[2], Result.KeyCutCode[0], 4);
  Result.RemoteId := (UInt32(Bytes[6]) shl 24) or (UInt32(Bytes[7]) shl 16)
                  or (UInt32(Bytes[8]) shl 8)  or UInt32(Bytes[9]);
  Result.KMReadingThousands := (UInt16(Bytes[10]) shl 8) or Bytes[11];
  // Allocate Result.Reserved
  SetLength(Result.Reserved, CAS_SLOT_BYTES - 12);
  Move(Bytes[12], Result.Reserved[0], CAS_SLOT_BYTES - 12);
end;

//------------------------------------------------------------------------------
// ENCODE KEY DATA FEM
//------------------------------------------------------------------------------
function EncodeKeyDataFem(const Key: TBMWKeyDataFem): TBytes;
var
  Status: Byte;
begin
  if not ValidateSlotIndex(bmwgFEMBDC, Key.SlotIndex) then
    raise EOBDBMWKey.CreateFmt('FEM-BDC slot %d out of range', [Key.SlotIndex]);
  if (Key.PersonalSettingsBank < 1) or (Key.PersonalSettingsBank > 4) then
    raise EOBDBMWKey.CreateFmt(
      'PersonalSettingsBank must be 1..4 (got %d)', [Key.PersonalSettingsBank]);
  if Length(Key.KeyCutCode) <> 4 then
    raise EOBDBMWKey.Create('KeyCutCode must be 4 bytes');
  if Length(Key.DigitalKeySerial) <> 7 then
    raise EOBDBMWKey.Create('DigitalKeySerial must be 7 bytes (zero if none)');
  // Allocate Result
  SetLength(Result, FEM_SLOT_BYTES);
  Result[0] := Key.SlotIndex;
  Status := 0;
  if Key.KeyEnabled then Status := Status or $01;
  Result[1] := Status;
  Result[2] := Key.PersonalSettingsBank;
  Move(Key.KeyCutCode[0], Result[3], 4);
  Move(Key.DigitalKeySerial[0], Result[7], 7);
  Result[14] := Byte(Key.UsageCounter shr 24);
  Result[15] := Byte(Key.UsageCounter shr 16);
  Result[16] := Byte(Key.UsageCounter shr 8);
  Result[17] := Byte(Key.UsageCounter and $FF);
  Result[18] := Byte(Key.LastKMReading shr 24);
  Result[19] := Byte(Key.LastKMReading shr 16);
  Result[20] := Byte(Key.LastKMReading shr 8);
  Result[21] := Byte(Key.LastKMReading and $FF);
  if Length(Key.Reserved) > 0 then
    WriteBytesPad(Result, 22, Key.Reserved, FEM_SLOT_BYTES - 22);
end;

//------------------------------------------------------------------------------
// DECODE KEY DATA FEM
//------------------------------------------------------------------------------
function DecodeKeyDataFem(const Bytes: TBytes): TBMWKeyDataFem;
begin
  if Length(Bytes) <> FEM_SLOT_BYTES then
    raise EOBDBMWKey.CreateFmt(
      'FEM-BDC slot must be %d bytes (got %d)', [FEM_SLOT_BYTES, Length(Bytes)]);
  Result := Default(TBMWKeyDataFem);
  Result.SlotIndex := Bytes[0];
  Result.KeyEnabled := (Bytes[1] and $01) <> 0;
  Result.PersonalSettingsBank := Bytes[2];
  // Allocate Result.KeyCutCode
  SetLength(Result.KeyCutCode, 4);
  Move(Bytes[3], Result.KeyCutCode[0], 4);
  // Allocate Result.DigitalKeySerial
  SetLength(Result.DigitalKeySerial, 7);
  Move(Bytes[7], Result.DigitalKeySerial[0], 7);
  Result.UsageCounter := (UInt32(Bytes[14]) shl 24) or (UInt32(Bytes[15]) shl 16)
                      or (UInt32(Bytes[16]) shl 8)  or UInt32(Bytes[17]);
  Result.LastKMReading := (UInt32(Bytes[18]) shl 24) or (UInt32(Bytes[19]) shl 16)
                       or (UInt32(Bytes[20]) shl 8)  or UInt32(Bytes[21]);
  // Allocate Result.Reserved
  SetLength(Result.Reserved, FEM_SLOT_BYTES - 22);
  Move(Bytes[22], Result.Reserved[0], FEM_SLOT_BYTES - 22);
end;

end.
