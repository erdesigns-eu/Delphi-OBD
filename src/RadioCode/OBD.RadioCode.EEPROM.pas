//------------------------------------------------------------------------------
//  OBD.RadioCode.EEPROM
//
//  Component family for car-radio code recovery via direct
//  EEPROM dump inspection.
//
//  A growing number of factory radios store the unlock code as
//  a fixed byte sequence at a known offset inside the radio's
//  serial-EEPROM (24Cxx / 25Cxx / 95xxx series). The host pulls
//  the chip with a programmer (CH341A, Willem, TL866 family, …),
//  saves the dump as a binary file, and feeds it to the matching
//  vendor-specific component. The component reads the code out
//  of the dump — no algorithm, no licensed service, no online
//  call.
//
//  Foundation:
//
//    TOBDRadioCodeEEPROMBase
//      Abstract <c>TComponent</c> base. Published surface:
//        DumpFile   : path to the .bin / .hex / .eep file.
//        Dump       : public read-only bytes once loaded.
//        OnExtract  : optional host-supplied extractor (mirrors
//                     the OnCalculate pattern from the rest of
//                     the radio-code family).
//      Vendors override <c>DoExtract</c> with the offset / decode
//      rule for their EEPROM map.
//
//  Vendor components shipped:
//
//    TOBDRadioCodeEEPROM_VolvoHU       Volvo HU / SC-7xx (24C01).
//    TOBDRadioCodeEEPROM_OpelCD30      Opel CD30 / CD70 (24C32 / 95640).
//    TOBDRadioCodeEEPROM_MercedesBecker  Mercedes Becker BE2xxx+ (24C02).
//
//  All offset values are taken from publicly documented
//  community write-ups (see docs/radio-code-algorithms.md).
//  The components read raw bytes at a known offset and format
//  them as the printable code string the radio's keypad expects.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Offset values from
//                     vauxhallownersnetwork.co.uk (Opel CD30/70),
//                     slomkowski.eu/extracting-security-codes
//                     (Mercedes Becker), and gist.github.com/
//                     klalle/1ae1bfec5e2506918a3f89492180565e
//                     (Volvo HU-601). Hosts that have a
//                     different chip variant override the
//                     OnExtract event.
//------------------------------------------------------------------------------

unit OBD.RadioCode.EEPROM;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types;

type
  /// <summary>One EEPROM-extraction result. Matches the shape of
  /// <see cref="TOBDRadioCodeResult"/> from the calculator
  /// family so a host UI can display either side without
  /// special-casing.</summary>
  TOBDRadioCodeEEPROMResult = record
    Success: Boolean;
    Code:    string;
    Message: string;
    Vendor:  string;
    /// <summary>Hex pretty-print of the bytes the extractor read
    /// out, useful for an "interesting bytes" display alongside
    /// the resolved code.</summary>
    RawHex:  string;
  end;

  /// <summary>Fired by the default <c>DoExtract</c> when the
  /// host wants to override the bundled offset / decode rule
  /// (different chip variant, different dump shape, …).</summary>
  TOBDRadioCodeEEPROMEvent = procedure(Sender: TObject;
    const ADump: TBytes; var AResult: TOBDRadioCodeEEPROMResult) of object;

  /// <summary>Abstract base for every EEPROM-dump extractor
  /// component.</summary>
  TOBDRadioCodeEEPROMBase = class abstract(TComponent)
  strict private
    FDumpFile:  string;
    FDump:      TBytes;
    FResult:    TOBDRadioCodeEEPROMResult;
    FOnExtract: TOBDRadioCodeEEPROMEvent;
    procedure SetDumpFile(const AValue: string);
  protected
    /// <summary>True iff a host has wired <see cref="OnExtract"/>.
    /// Vendor subclasses call this before running their bundled
    /// rule so the host hook can override the offset / decode for
    /// firmware variants the bundled rule doesn't know.</summary>
    function HasHostOverride: Boolean;
    /// <summary>Runs the host-supplied extractor. Caller must
    /// guard with <see cref="HasHostOverride"/>.</summary>
    function RunHostOverride(const ADump: TBytes): TOBDRadioCodeEEPROMResult;
    /// <summary>True when <c>ADump</c> is at least <c>AMinimum</c>
    /// bytes long; sets <c>AReason</c> otherwise.</summary>
    function ValidateDumpSize(const ADump: TBytes;
      AMinimum: Integer; out AReason: string): Boolean;
    /// <summary>Hex pretty-print helper used by every vendor's
    /// <c>RawHex</c>.</summary>
    function HexPrint(const ABytes: TBytes;
      AOffset, ALength: Integer): string;
    /// <summary>Vendor-supplied extraction. Default
    /// implementation fires <see cref="OnExtract"/> if the host
    /// has wired one; otherwise reports
    /// "extractor not bundled". Vendors with a documented
    /// offset override this.</summary>
    function DoExtract(const ADump: TBytes): TOBDRadioCodeEEPROMResult;
      virtual;
  public
    /// <summary>Stable id (e.g. <c>'volvo-hu'</c>).</summary>
    function VendorKey: string; virtual; abstract;
    /// <summary>Display name shown in pickers.</summary>
    function DisplayName: string; virtual; abstract;
    /// <summary>One-line description of the chip + offset.</summary>
    function Description: string; virtual; abstract;

    /// <summary>Loads <c>DumpFile</c> from disk into <c>Dump</c>
    /// and returns the byte count read. Raises
    /// <c>EArgumentException</c> on missing file.</summary>
    function LoadDumpFile: Integer;

    /// <summary>Runs <see cref="DoExtract"/> against the loaded
    /// dump (loads <c>DumpFile</c> first if needed) and returns
    /// the result, also stored in <see cref="LastResult"/>.</summary>
    function Extract: TOBDRadioCodeEEPROMResult;

    /// <summary>Last extraction result.</summary>
    property LastResult: TOBDRadioCodeEEPROMResult read FResult;
    /// <summary>Currently-loaded dump bytes (read-only). Empty
    /// until <see cref="LoadDumpFile"/> succeeds.</summary>
    property Dump: TBytes read FDump;
  published
    /// <summary>Path to the EEPROM dump file (.bin / .hex /
    /// .eep). Setting this clears the previously loaded
    /// <see cref="Dump"/>.</summary>
    property DumpFile: string read FDumpFile write SetDumpFile;
    /// <summary>Optional host-supplied extractor (overrides the
    /// bundled offset rule).</summary>
    property OnExtract: TOBDRadioCodeEEPROMEvent
      read FOnExtract write FOnExtract;
  end;

  /// <summary>Volvo HU / SC-7xx series. EEPROM: 24C01.
  /// Documented offset for the 4-digit ASCII code is 0x90 (HU-601
  /// reference dump). Hosts with a different HU variant wire
  /// <c>OnExtract</c> with their own offset.</summary>
  TOBDRadioCodeEEPROM_VolvoHU = class(TOBDRadioCodeEEPROMBase)
  protected
    function DoExtract(const ADump: TBytes): TOBDRadioCodeEEPROMResult; override;
  public
    function VendorKey:   string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Opel CD30 / CD70. EEPROM: 24C32 (CD30) or 95640
  /// (CD70). Documented offset for the 4-digit BCD code is
  /// 0x2B7 (CD70 — Grundig / Blaupunkt-built).</summary>
  TOBDRadioCodeEEPROM_OpelCD30 = class(TOBDRadioCodeEEPROMBase)
  protected
    function DoExtract(const ADump: TBytes): TOBDRadioCodeEEPROMResult; override;
  public
    function VendorKey:   string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Mercedes-Benz Becker BE2xxx+ factory radios.
  /// EEPROM: 24C02. The 5-digit ASCII code typically lives at
  /// offset 0x76 (slomkowski.eu reference). Hosts with a
  /// different Becker variant wire <c>OnExtract</c>.</summary>
  TOBDRadioCodeEEPROM_MercedesBecker = class(TOBDRadioCodeEEPROMBase)
  protected
    function DoExtract(const ADump: TBytes): TOBDRadioCodeEEPROMResult; override;
  public
    function VendorKey:   string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

uses
  System.IOUtils;

{ ---- TOBDRadioCodeEEPROMBase ----------------------------------------------- }

procedure TOBDRadioCodeEEPROMBase.SetDumpFile(const AValue: string);
begin
  if FDumpFile = AValue then Exit;
  FDumpFile := AValue;
  SetLength(FDump, 0);
end;

function TOBDRadioCodeEEPROMBase.LoadDumpFile: Integer;
begin
  if Trim(FDumpFile) = '' then
    raise EArgumentException.Create(
      'TOBDRadioCodeEEPROM: DumpFile is not set');
  if not TFile.Exists(FDumpFile) then
    raise EArgumentException.CreateFmt(
      'TOBDRadioCodeEEPROM: dump file not found "%s"', [FDumpFile]);
  FDump := TFile.ReadAllBytes(FDumpFile);
  Result := Length(FDump);
end;

function TOBDRadioCodeEEPROMBase.Extract: TOBDRadioCodeEEPROMResult;
begin
  if Length(FDump) = 0 then
    LoadDumpFile;
  FResult := Default(TOBDRadioCodeEEPROMResult);
  FResult.Vendor := VendorKey;
  if Length(FDump) = 0 then
  begin
    FResult.Message := 'Dump is empty';
    Exit(FResult);
  end;
  FResult := DoExtract(FDump);
  if FResult.Vendor = '' then
    FResult.Vendor := VendorKey;
  Result := FResult;
end;

function TOBDRadioCodeEEPROMBase.ValidateDumpSize(const ADump: TBytes;
  AMinimum: Integer; out AReason: string): Boolean;
begin
  AReason := '';
  Result := Length(ADump) >= AMinimum;
  if not Result then
    AReason := Format(
      'EEPROM dump too short: need at least %d bytes, got %d',
      [AMinimum, Length(ADump)]);
end;

function TOBDRadioCodeEEPROMBase.HexPrint(const ABytes: TBytes;
  AOffset, ALength: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ALength - 1 do
    if AOffset + I < Length(ABytes) then
    begin
      if I > 0 then Result := Result + ' ';
      Result := Result + Format('%.2x', [ABytes[AOffset + I]]);
    end;
end;

function TOBDRadioCodeEEPROMBase.HasHostOverride: Boolean;
begin
  Result := Assigned(FOnExtract);
end;

function TOBDRadioCodeEEPROMBase.RunHostOverride(
  const ADump: TBytes): TOBDRadioCodeEEPROMResult;
begin
  Result := Default(TOBDRadioCodeEEPROMResult);
  Result.Vendor := VendorKey;
  FOnExtract(Self, ADump, Result);
  if Result.Vendor = '' then
    Result.Vendor := VendorKey;
end;

function TOBDRadioCodeEEPROMBase.DoExtract(
  const ADump: TBytes): TOBDRadioCodeEEPROMResult;
begin
  if HasHostOverride then
    Exit(RunHostOverride(ADump));
  Result := Default(TOBDRadioCodeEEPROMResult);
  Result.Vendor  := VendorKey;
  Result.Success := False;
  Result.Message :=
    Format('No bundled extractor for "%s" — wire OnExtract.',
           [DisplayName]);
end;

{ ---- TOBDRadioCodeEEPROM_VolvoHU ------------------------------------------ }

function TOBDRadioCodeEEPROM_VolvoHU.VendorKey:   string; begin Result := 'volvo-hu'; end;
function TOBDRadioCodeEEPROM_VolvoHU.DisplayName: string; begin Result := 'Volvo HU / SC-7xx (24C01)'; end;
function TOBDRadioCodeEEPROM_VolvoHU.Description: string;
begin
  Result :=
    'Volvo HU / SC-7xx factory radios. EEPROM: 24C01 (128 bytes). ' +
    'Code is 4 ASCII digits at offset 0x90 in the HU-601 reference ' +
    'dump. Different HU variants store the code at different ' +
    'offsets — wire OnExtract for those.';
end;
function TOBDRadioCodeEEPROM_VolvoHU.DoExtract(
  const ADump: TBytes): TOBDRadioCodeEEPROMResult;
const
  CODE_OFFSET = $90;
  CODE_LENGTH = 4;
var
  Reason: string;
  Buf: TBytes;
  I: Integer;
begin
  if HasHostOverride then Exit(RunHostOverride(ADump));
  Result := Default(TOBDRadioCodeEEPROMResult);
  Result.Vendor := VendorKey;
  if not ValidateDumpSize(ADump, CODE_OFFSET + CODE_LENGTH, Reason) then
  begin
    Result.Message := Reason;
    Exit;
  end;
  SetLength(Buf, CODE_LENGTH);
  for I := 0 to CODE_LENGTH - 1 do
  begin
    Buf[I] := ADump[CODE_OFFSET + I];
    if not ((Buf[I] >= Ord('0')) and (Buf[I] <= Ord('9'))) then
    begin
      Result.Message := Format(
        'Bytes at 0x%.2x..0x%.2x are not 4 ASCII digits — try ' +
        'wiring OnExtract for this HU variant.',
        [CODE_OFFSET, CODE_OFFSET + CODE_LENGTH - 1]);
      Result.RawHex := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
      Exit;
    end;
  end;
  Result.Code    := TEncoding.ASCII.GetString(Buf);
  Result.RawHex  := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
  Result.Success := True;
end;

{ ---- TOBDRadioCodeEEPROM_OpelCD30 ----------------------------------------- }

function TOBDRadioCodeEEPROM_OpelCD30.VendorKey:   string; begin Result := 'opel-cd30'; end;
function TOBDRadioCodeEEPROM_OpelCD30.DisplayName: string; begin Result := 'Opel CD30 / CD70 (24C32 / 95640)'; end;
function TOBDRadioCodeEEPROM_OpelCD30.Description: string;
begin
  Result :=
    'Opel CD30 / CD70 factory radios. EEPROM: 24C32 (CD30) or ' +
    '95640 (CD70). 4-digit BCD code at offset 0x2B7 (CD70 ' +
    'reference). The 4 nibbles encode the digits in big-endian ' +
    'order: 0x12 0x34 -> "1234".';
end;
function TOBDRadioCodeEEPROM_OpelCD30.DoExtract(
  const ADump: TBytes): TOBDRadioCodeEEPROMResult;
const
  CODE_OFFSET = $2B7;
  CODE_LENGTH = 2;        // 2 BCD bytes = 4 digits
var
  Reason: string;
  B0, B1: Byte;
  D: array[0..3] of Byte;
  I: Integer;
begin
  if HasHostOverride then Exit(RunHostOverride(ADump));
  Result := Default(TOBDRadioCodeEEPROMResult);
  Result.Vendor := VendorKey;
  if not ValidateDumpSize(ADump, CODE_OFFSET + CODE_LENGTH, Reason) then
  begin
    Result.Message := Reason;
    Exit;
  end;
  B0 := ADump[CODE_OFFSET];
  B1 := ADump[CODE_OFFSET + 1];
  D[0] := (B0 shr 4) and $0F;
  D[1] :=  B0        and $0F;
  D[2] := (B1 shr 4) and $0F;
  D[3] :=  B1        and $0F;
  for I := 0 to 3 do
    if D[I] > 9 then
    begin
      Result.Message := Format(
        'Bytes at 0x%.3x..0x%.3x are not valid BCD — try wiring ' +
        'OnExtract for this CD30 / CD70 firmware variant.',
        [CODE_OFFSET, CODE_OFFSET + CODE_LENGTH - 1]);
      Result.RawHex := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
      Exit;
    end;
  Result.Code    := Format('%d%d%d%d', [D[0], D[1], D[2], D[3]]);
  Result.RawHex  := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
  Result.Success := True;
end;

{ ---- TOBDRadioCodeEEPROM_MercedesBecker ----------------------------------- }

function TOBDRadioCodeEEPROM_MercedesBecker.VendorKey:   string; begin Result := 'mercedes-becker'; end;
function TOBDRadioCodeEEPROM_MercedesBecker.DisplayName: string; begin Result := 'Mercedes Becker BE2xxx+ (24C02)'; end;
function TOBDRadioCodeEEPROM_MercedesBecker.Description: string;
begin
  Result :=
    'Mercedes-Benz Becker BE2xxx+ factory radios. EEPROM: 24C02 ' +
    '(256 bytes). 5-digit ASCII code at offset 0x76. Different ' +
    'Becker variants relocate the code — wire OnExtract for those.';
end;
function TOBDRadioCodeEEPROM_MercedesBecker.DoExtract(
  const ADump: TBytes): TOBDRadioCodeEEPROMResult;
const
  CODE_OFFSET = $76;
  CODE_LENGTH = 5;
var
  Reason: string;
  Buf: TBytes;
  I: Integer;
begin
  if HasHostOverride then Exit(RunHostOverride(ADump));
  Result := Default(TOBDRadioCodeEEPROMResult);
  Result.Vendor := VendorKey;
  if not ValidateDumpSize(ADump, CODE_OFFSET + CODE_LENGTH, Reason) then
  begin
    Result.Message := Reason;
    Exit;
  end;
  SetLength(Buf, CODE_LENGTH);
  for I := 0 to CODE_LENGTH - 1 do
  begin
    Buf[I] := ADump[CODE_OFFSET + I];
    if not ((Buf[I] >= Ord('0')) and (Buf[I] <= Ord('9'))) then
    begin
      Result.Message := Format(
        'Bytes at 0x%.2x..0x%.2x are not 5 ASCII digits — try ' +
        'wiring OnExtract for this Becker variant.',
        [CODE_OFFSET, CODE_OFFSET + CODE_LENGTH - 1]);
      Result.RawHex := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
      Exit;
    end;
  end;
  Result.Code    := TEncoding.ASCII.GetString(Buf);
  Result.RawHex  := HexPrint(ADump, CODE_OFFSET, CODE_LENGTH);
  Result.Success := True;
end;

end.
