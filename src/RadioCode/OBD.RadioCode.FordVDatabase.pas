//------------------------------------------------------------------------------
//  OBD.RadioCode.FordVDatabase
//
//  TOBDRadioCodeFordV — Ford V-series factory radios. Bundled
//  serial->code database (999,999 entries) loaded from
//  <c>catalogs/radio-code/ford-v.json</c> on first <c>Calculate</c>.
//
//  Lives in its own unit (separate from the rest of the
//  American pack) so a host that doesn't need Ford-V doesn't drag
//  the 7 MB database file into its build.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Database extracted
//                     verbatim from the v1 ERDesigns Ford.V
//                     calculator (8.6 MB Pascal source -> 7 MB JSON).
//------------------------------------------------------------------------------

unit OBD.RadioCode.FordVDatabase;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.RadioCode.Types,
  OBD.RadioCode;

type
  /// <summary>Ford V-series factory radios. Input: 6 digits
  /// (optionally with a leading V which is stripped). Algorithm
  /// bundled — flat lookup against a 999,999-entry database
  /// loaded from <c>catalogs/radio-code/ford-v.json</c>.</summary>
  TOBDRadioCodeFordV = class(TOBDRadioCode)
  protected
    function DoValidate(const AInput: string; out AReason: string): Boolean; override;
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  public
    function BrandKey:    string; override;
    function DisplayName: string; override;
    function Description: string; override;
  end;

implementation

uses
  System.IOUtils,
  System.JSON,
  System.SyncObjs;

const
  TABLE_SIZE = 999999;

var
  GFordV:       TArray<string>;
  GFordVLoaded: Boolean = False;
  GLoadLock:    TCriticalSection;

function CatalogDir: string;
begin
  Result := TPath.Combine(
    TPath.GetDirectoryName(ParamStr(0)), 'catalogs');
end;

procedure EnsureLoaded;
var
  Path: string;
  Doc: TJSONValue;
  Arr: TJSONArray;
  I: Integer;
begin
  if GFordVLoaded then Exit;
  GLoadLock.Enter;
  try
    if GFordVLoaded then Exit;
    Path := TPath.Combine(TPath.Combine(CatalogDir, 'radio-code'),
      'ford-v.json');
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
      SetLength(GFordV, TABLE_SIZE);
      for I := 0 to TABLE_SIZE - 1 do
        GFordV[I] := Arr.Items[I].Value;
      GFordVLoaded := True;
    finally
      Doc.Free;
    end;
  finally
    GLoadLock.Leave;
  end;
end;

function TOBDRadioCodeFordV.BrandKey: string; begin Result := 'ford-v'; end;
function TOBDRadioCodeFordV.DisplayName: string; begin Result := 'Ford V-series'; end;
function TOBDRadioCodeFordV.Description: string;
begin
  Result :=
    'Ford V-series factory radios. Input: 6-digit serial (1-based, ' +
    'optionally with a leading V which is stripped). Algorithm ' +
    'bundled — flat lookup against a 999,999-entry database loaded ' +
    'from catalogs/radio-code/ford-v.json on first Calculate.';
end;

function TOBDRadioCodeFordV.DoValidate(const AInput: string;
  out AReason: string): Boolean;
var
  Stripped: string;
begin
  if (Length(AInput) > 0) and (AInput[1] = 'V') then
    Stripped := Copy(AInput, 2, Length(AInput) - 1)
  else
    Stripped := AInput;
  Result := ValidateLength(Stripped, 6, AReason)
       and  ValidateAllDigits(Stripped, AReason);
end;

function TOBDRadioCodeFordV.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
var
  Stripped: string;
  Idx:      Integer;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  Result.Variant  := 'lookup table (999,999 entries)';
  EnsureLoaded;
  if not GFordVLoaded then
  begin
    Result.Message :=
      'Ford-V database not loaded (expected ' +
      'catalogs/radio-code/ford-v.json next to the executable).';
    Exit;
  end;
  if (Length(AInput) > 0) and (AInput[1] = 'V') then
    Stripped := Copy(AInput, 2, Length(AInput) - 1)
  else
    Stripped := AInput;
  Idx := StrToInt(Stripped) - 1;       // serials are 1-based
  if (Idx < 0) or (Idx >= TABLE_SIZE) then
  begin
    Result.Message := 'Serial out of range (must be 1..999999)';
    Exit;
  end;
  Result.Code    := GFordV[Idx];
  Result.Success := True;
end;

initialization
  GLoadLock := TCriticalSection.Create;
  TOBDRadioCodeRegistry.Default.RegisterClass(TOBDRadioCodeFordV);

finalization
  GLoadLock.Free;

end.
