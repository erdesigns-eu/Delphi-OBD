//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Registry.pas
// CONTENTS       : Brand registry for radio-code calculators
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.RadioCode.Registry;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs,

  OBD.RadioCode, OBD.RadioCode.Variants;

type
  /// <summary>Raised when a calculator is registered but its underlying
  /// algorithm/database is not available in this build.</summary>
  EOBDRadioCodeDataMissing = class(Exception);

  TOBDRadioCodeFactory = reference to function: IOBDRadioCode;

  /// <summary>One brand entry in the registry.</summary>
  TOBDRadioCodeBrand = class
  private
    FBrandKey: string;
    FDisplayName: string;
    FDataAvailable: Boolean;
    FDataNotes: string;
    FFactory: TOBDRadioCodeFactory;
    FVariants: TRadioCodeVariantManager;
  public
    constructor Create(const BrandKey, DisplayName: string;
      DataAvailable: Boolean; const DataNotes: string;
      const Factory: TOBDRadioCodeFactory);
    destructor Destroy; override;

    /// <summary>Lower-case brand identifier (e.g. 'pioneer', 'philips').</summary>
    property BrandKey: string read FBrandKey;
    /// <summary>Human-readable name shown in UIs.</summary>
    property DisplayName: string read FDisplayName;
    /// <summary>True when a real algorithm/database backs the calculator.
    /// False indicates a data-pending stub that will raise on Calculate.</summary>
    property DataAvailable: Boolean read FDataAvailable;
    /// <summary>For data-pending brands, describes what reference data
    /// would unblock the calculator.</summary>
    property DataNotes: string read FDataNotes;
    /// <summary>Variant manager for region/year/security-version dispatch.</summary>
    property Variants: TRadioCodeVariantManager read FVariants;

    function CreateCalculator: IOBDRadioCode;
  end;

  /// <summary>Process-wide registry. Thread-safe; brands register at init.</summary>
  TOBDRadioCodeRegistry = class
  private
    class var FInstance: TOBDRadioCodeRegistry;
    FLock: TCriticalSection;
    FBrands: TObjectList<TOBDRadioCodeBrand>;
    FByKey: TDictionary<string, TOBDRadioCodeBrand>;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TOBDRadioCodeRegistry;
    class procedure FreeInstance; reintroduce;

    procedure Register(Brand: TOBDRadioCodeBrand);
    function Find(const BrandKey: string): TOBDRadioCodeBrand;
    procedure GetBrandKeys(Keys: TStrings);
    function Count: Integer;
  end;

implementation

{ TOBDRadioCodeBrand }

constructor TOBDRadioCodeBrand.Create(const BrandKey, DisplayName: string;
  DataAvailable: Boolean; const DataNotes: string;
  const Factory: TOBDRadioCodeFactory);
begin
  inherited Create;
  FBrandKey := LowerCase(BrandKey);
  FDisplayName := DisplayName;
  FDataAvailable := DataAvailable;
  FDataNotes := DataNotes;
  FFactory := Factory;
  FVariants := TRadioCodeVariantManager.Create(DisplayName);
end;

destructor TOBDRadioCodeBrand.Destroy;
begin
  FVariants.Free;
  inherited;
end;

function TOBDRadioCodeBrand.CreateCalculator: IOBDRadioCode;
begin
  if not Assigned(FFactory) then
    raise EOBDRadioCodeDataMissing.CreateFmt(
      'No factory registered for brand %s', [FBrandKey]);
  Result := FFactory();
end;

{ TOBDRadioCodeRegistry }

constructor TOBDRadioCodeRegistry.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FBrands := TObjectList<TOBDRadioCodeBrand>.Create(True);
  FByKey := TDictionary<string, TOBDRadioCodeBrand>.Create;
end;

destructor TOBDRadioCodeRegistry.Destroy;
begin
  FByKey.Free;
  FBrands.Free;
  FLock.Free;
  inherited;
end;

class function TOBDRadioCodeRegistry.Instance: TOBDRadioCodeRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDRadioCodeRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDRadioCodeRegistry.FreeInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDRadioCodeRegistry.Register(Brand: TOBDRadioCodeBrand);
begin
  if Brand = nil then Exit;
  FLock.Acquire;
  try
    if FByKey.ContainsKey(Brand.BrandKey) then
    begin
      Brand.Free;
      Exit;
    end;
    FBrands.Add(Brand);
    FByKey.Add(Brand.BrandKey, Brand);
  finally
    FLock.Release;
  end;
end;

function TOBDRadioCodeRegistry.Find(const BrandKey: string): TOBDRadioCodeBrand;
begin
  FLock.Acquire;
  try
    if not FByKey.TryGetValue(LowerCase(BrandKey), Result) then
      Result := nil;
  finally
    FLock.Release;
  end;
end;

procedure TOBDRadioCodeRegistry.GetBrandKeys(Keys: TStrings);
var
  Brand: TOBDRadioCodeBrand;
begin
  Keys.Clear;
  FLock.Acquire;
  try
    for Brand in FBrands do
      Keys.Add(Brand.BrandKey);
  finally
    FLock.Release;
  end;
end;

function TOBDRadioCodeRegistry.Count: Integer;
begin
  FLock.Acquire;
  try
    Result := FBrands.Count;
  finally
    FLock.Release;
  end;
end;

initialization

finalization
  TOBDRadioCodeRegistry.FreeInstance;

end.
