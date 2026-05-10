//------------------------------------------------------------------------------
//  OBD.RadioCode.Aftermarket
//
//  Vendor radio-code calculator components for aftermarket head
//  units:
//
//    TOBDRadioCodeAlpine     Alpine head units (6 digits)
//    TOBDRadioCodeBlaupunkt  Blaupunkt head units (1 letter + 6 digits)
//    TOBDRadioCodeClarion    Clarion C0 / C7 head units (5 digits)
//    TOBDRadioCodeBecker4    Becker 4-digit-serial radios (REAL DB)
//    TOBDRadioCodeBecker5    Becker 5-digit-serial radios (REAL DB)
//
//  Becker4 + Becker5 ship a real bundled database
//  (catalogs/radio-code/becker4.json + becker5.json — 10,000
//  serial->code entries each). The other three are OnCalculate
//  stubs (algorithms commercial-only).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Becker JSON
//                     databases imported verbatim from v1's
//                     catalogs/radiocode-becker{4,5}.json.
//------------------------------------------------------------------------------

unit OBD.RadioCode.Aftermarket;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  TOBDRadioCodeAlpine = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeBlaupunkt = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  TOBDRadioCodeClarion = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Becker 4-digit-serial radios. Algorithm: serial→code
  /// is a flat lookup against a 10,000-entry table loaded from
  /// <c>catalogs/radio-code/becker4.json</c>. Database imported
  /// from the v1 ERDesigns Becker4 calculator.</summary>
  TOBDRadioCodeBecker4 = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

  /// <summary>Becker 5-digit-serial radios. Same database shape as
  /// <see cref="TOBDRadioCodeBecker4"/>; database lives at
  /// <c>catalogs/radio-code/becker5.json</c>.</summary>
  TOBDRadioCodeBecker5 = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey: string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

uses
  System.IOUtils,
  System.JSON,
  System.SyncObjs;

const
  TABLE_SIZE = 10000;

var
  GBecker4: array[0..TABLE_SIZE - 1] of string;
  GBecker5: array[0..TABLE_SIZE - 1] of string;
  GBecker4Loaded: Boolean = False;
  GBecker5Loaded: Boolean = False;
  GLoadLock: TCriticalSection;

function CatalogDir: string;
begin
  Result := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
end;

function LoadBeckerTable(const AFileName: string;
  var ATarget: array of string): Boolean;
var
  Path: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  I: Integer;
begin
  Result := False;
  Path := TPath.Combine(TPath.Combine(CatalogDir, 'radio-code'), AFileName);
  if not TFile.Exists(Path) then Exit;
  Doc := TJSONObject.ParseJSONValue(
    TFile.ReadAllText(Path, TEncoding.UTF8));
  if not (Doc is TJSONObject) then
  begin
    Doc.Free;
    Exit;
  end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('codes');
    if (Arr = nil) or (Arr.Count <> TABLE_SIZE) then Exit;
    for I := 0 to TABLE_SIZE - 1 do
      ATarget[I] := Arr.Items[I].Value;
    Result := True;
  finally
    Doc.Free;
  end;
end;

procedure EnsureBecker4Loaded;
begin
  if GBecker4Loaded then Exit;
  GLoadLock.Enter;
  try
    if not GBecker4Loaded then
      GBecker4Loaded := LoadBeckerTable('becker4.json', GBecker4);
  finally
    GLoadLock.Leave;
  end;
end;

procedure EnsureBecker5Loaded;
begin
  if GBecker5Loaded then Exit;
  GLoadLock.Enter;
  try
    if not GBecker5Loaded then
      GBecker5Loaded := LoadBeckerTable('becker5.json', GBecker5);
  finally
    GLoadLock.Leave;
  end;
end;

{ ---- Alpine --------------------------------------------------------------- }
function TOBDRadioCodeAlpine.BrandKey: string; begin Result := 'alpine'; end;
function TOBDRadioCodeAlpine.DisplayName: string; begin Result := 'Alpine'; end;
function TOBDRadioCodeAlpine.Description: string;
begin
  Result :=
    'Alpine factory radios. Input: 6 digits. Algorithm not bundled — ' +
    'commercial only; wire OnCalculate.';
end;
function TOBDRadioCodeAlpine.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 6, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- Blaupunkt ------------------------------------------------------------ }
function TOBDRadioCodeBlaupunkt.BrandKey: string; begin Result := 'blaupunkt'; end;
function TOBDRadioCodeBlaupunkt.DisplayName: string; begin Result := 'Blaupunkt'; end;
function TOBDRadioCodeBlaupunkt.Description: string;
begin
  Result :=
    'Blaupunkt factory radios. Input: 1 letter + 6 digits (e.g. ' +
    'BP1 / BP2 / BP3 series). Algorithm not bundled — commercial ' +
    'only (BPcalc tools circulate but are closed source). Wire ' +
    'OnCalculate.';
end;
function TOBDRadioCodeBlaupunkt.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 7, AReason);
  if not Result then Exit;
  if not CharInSet(AInput[1], ['A'..'Z']) then
  begin
    AReason := 'First character must be a letter';
    Exit(False);
  end;
  Result := ValidateDigitRange(AInput, 2, 7, AReason);
end;

{ ---- Clarion ------------------------------------------------------------- }
function TOBDRadioCodeClarion.BrandKey: string; begin Result := 'clarion'; end;
function TOBDRadioCodeClarion.DisplayName: string; begin Result := 'Clarion (C0 / C7)'; end;
function TOBDRadioCodeClarion.Description: string;
begin
  Result :=
    'Clarion C0 / C7 / NX / GCX head units. Input: 5 digits. ' +
    'Algorithm not bundled — community-documented but the formula ' +
    'varies per series. Wire OnCalculate.';
end;
function TOBDRadioCodeClarion.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 5, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;

{ ---- Becker4 -------------------------------------------------------------- }
function TOBDRadioCodeBecker4.BrandKey: string; begin Result := 'becker-4'; end;
function TOBDRadioCodeBecker4.DisplayName: string; begin Result := 'Becker (4-digit serial)'; end;
function TOBDRadioCodeBecker4.Description: string;
begin
  Result :=
    'Becker factory radios with a 4-digit serial. Input: 4 digits ' +
    'from the radio''s back label. Algorithm bundled — flat lookup ' +
    'against a 10,000-entry database loaded from ' +
    'catalogs/radio-code/becker4.json.';
end;
function TOBDRadioCodeBecker4.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodeBecker4.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  Idx: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'lookup table (10000 entries)';
  EnsureBecker4Loaded;
  if not GBecker4Loaded then
  begin
    Result.Message :=
      'Becker4 database not loaded (expected ' +
      'catalogs/radio-code/becker4.json next to the executable).';
    Exit;
  end;
  Idx := StrToInt(AInput);
  Result.Code    := GBecker4[Idx];
  Result.Success := True;
end;

{ ---- Becker5 -------------------------------------------------------------- }
function TOBDRadioCodeBecker5.BrandKey: string; begin Result := 'becker-5'; end;
function TOBDRadioCodeBecker5.DisplayName: string; begin Result := 'Becker (5-digit serial)'; end;
function TOBDRadioCodeBecker5.Description: string;
begin
  Result :=
    'Becker factory radios with a 5-digit serial. Input: 4 digits ' +
    'from the radio''s back label (the database is keyed 0..9999). ' +
    'Algorithm bundled — flat lookup against a 10,000-entry database ' +
    'loaded from catalogs/radio-code/becker5.json.';
end;
function TOBDRadioCodeBecker5.DoValidate(const AInput: string; out AReason: string): Boolean;
begin
  Result := ValidateLength(AInput, 4, AReason)
       and  ValidateAllDigits(AInput, AReason);
end;
function TOBDRadioCodeBecker5.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  Idx: Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'lookup table (10000 entries)';
  EnsureBecker5Loaded;
  if not GBecker5Loaded then
  begin
    Result.Message :=
      'Becker5 database not loaded (expected ' +
      'catalogs/radio-code/becker5.json next to the executable).';
    Exit;
  end;
  Idx := StrToInt(AInput);
  Result.Code    := GBecker5[Idx];
  Result.Success := True;
end;

initialization
  GLoadLock := TCriticalSection.Create;
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeAlpine);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeBlaupunkt);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeClarion);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeBecker4);
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeBecker5);

finalization
  GLoadLock.Free;

end.
