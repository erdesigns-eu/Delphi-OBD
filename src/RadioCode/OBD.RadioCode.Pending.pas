//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Pending.pas
// CONTENTS       : Data-pending stubs for radio-code brands not yet operational
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.RadioCode.Pending;

interface

uses
  System.SysUtils, System.Classes, System.JSON,

  OBD.RadioCode, OBD.RadioCode.Registry, OBD.RadioCode.Variants,
  OBD.Catalog.Path;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>Common base for data-pending calculator stubs. Validate
  /// returns False with a clear message; Calculate raises
  /// EOBDRadioCodeDataMissing.</summary>
  TOBDRadioCodePending = class(TOBDRadioCode)
  private
    FBrandKey: string;
    FDisplayName: string;
    FDataNotes: string;
  public
    /// <summary>Create.</summary>
    constructor Create(const BrandKey, DisplayName, DataNotes: string);
    /// <summary>Get description.</summary>
    function GetDescription: string; override;
    /// <summary>Validate.</summary>
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    /// <summary>Calculate.</summary>
    function Calculate(const Input: string; var Output: string;
      var ErrorMessage: string): Boolean; override;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

{ TOBDRadioCodePending }

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDRadioCodePending.Create(const BrandKey, DisplayName,
  DataNotes: string);
begin
  inherited Create;
  FBrandKey := BrandKey;
  FDisplayName := DisplayName;
  FDataNotes := DataNotes;
end;

//------------------------------------------------------------------------------
// GET DESCRIPTION
//------------------------------------------------------------------------------
function TOBDRadioCodePending.GetDescription: string;
begin
  Result := Format(
    '%s radio-code calculator (DATA-PENDING — algorithm or database not available in this build). %s',
    [FDisplayName, FDataNotes]);
end;

//------------------------------------------------------------------------------
// VALIDATE
//------------------------------------------------------------------------------
function TOBDRadioCodePending.Validate(const Input: string;
  var ErrorMessage: string): Boolean;
begin
  Result := False;
  ErrorMessage := Format(
    '%s calculator is not yet operational. %s',
    [FDisplayName, FDataNotes]);
end;

//------------------------------------------------------------------------------
// CALCULATE
//------------------------------------------------------------------------------
function TOBDRadioCodePending.Calculate(const Input: string;
  var Output: string; var ErrorMessage: string): Boolean;
begin
  Output := '';
  ErrorMessage := Format(
    '%s calculator is data-pending: %s',
    [FDisplayName, FDataNotes]);
  raise EOBDRadioCodeDataMissing.Create(ErrorMessage);
end;

//------------------------------------------------------------------------------
// REGISTRATION (catalog-driven)
//------------------------------------------------------------------------------
function MakePendingFactory(const Key, Name, Notes: string): TOBDRadioCodeFactory;
begin
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodePending.Create(Key, Name, Notes);
    end;
end;

//------------------------------------------------------------------------------
// LOAD PENDING BRANDS
//------------------------------------------------------------------------------
procedure LoadPendingBrands;
var
  Path, Raw, K, N, Notes: string;
  Stream: TStringStream;
  Doc: TJSONValue;
  Arr: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
begin
  Path := ResolveCatalogPath('radiocode-pending-brands.json');
  if Path = '' then Exit;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(Path);
    Raw := Stream.DataString;
  finally
    Stream.Free;
  end;
  Doc := TJSONObject.ParseJSONValue(Raw);
  if not (Doc is TJSONObject) then begin Doc.Free; Exit; end;
  try
    Arr := (Doc as TJSONObject).GetValue<TJSONArray>('entries');
    if Arr = nil then Exit;
    for Item in Arr do
    begin
      if not (Item is TJSONObject) then Continue;
      Obj := Item as TJSONObject;
      K := Obj.GetValue<string>('brand_key', '');
      if K = '' then Continue;
      N := Obj.GetValue<string>('display_name', '');
      Notes := Obj.GetValue<string>('data_notes', '');
      TOBDRadioCodeRegistry.Instance.Register(
        TOBDRadioCodeBrand.Create(K, N, False, Notes,
          MakePendingFactory(K, N, Notes)));
    end;
  finally
    Doc.Free;
  end;
end;

initialization
  LoadPendingBrands;

end.
