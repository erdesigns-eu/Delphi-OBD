//------------------------------------------------------------------------------
//  ERD.UI.RangeProfiles
//
//  Editable normal-range profiles for OBD Studio freeze-frame and live-data
//  values. Profiles are non-visual component state: each range stores the
//  garage-edited Low..High band together with the shipped default band so the
//  UI can show when a workshop has changed a value.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.RangeProfiles;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  ERD.UI.Types;

type
  /// <summary>Raised when a range profile JSON document is malformed.</summary>
  EOBDRangeProfileError = class(Exception)
  end;

  /// <summary>One OBD value scale and normal band.</summary>
  TOBDValueRange = class(TCollectionItem)
  strict private
    FPID: Integer;
    FKey: string;
    FCaption: string;
    FUnitText: string;
    FMin: Double;
    FMax: Double;
    FLow: Double;
    FHigh: Double;
    FDefaultLow: Double;
    FDefaultHigh: Double;
    FAlarmMargin: Double;
    FDecimals: Integer;
    procedure SetPID(AValue: Integer);
    procedure SetKey(const AValue: string);
    procedure SetCaption(const AValue: string);
    procedure SetUnitText(const AValue: string);
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetLow(AValue: Double);
    procedure SetHigh(AValue: Double);
    procedure SetDefaultLow(AValue: Double);
    procedure SetDefaultHigh(AValue: Double);
    procedure SetAlarmMargin(AValue: Double);
    procedure SetDecimals(AValue: Integer);
  public
    /// <summary>Creates a value range with a 0.5 alarm margin.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies another value range.</summary>
    /// <param name="ASource">Source persistent object.</param>
    procedure Assign(ASource: TPersistent); override;
    /// <summary>True when the editable band differs from the shipped band.</summary>
    /// <returns>True when Low or High was changed.</returns>
    function IsModified: Boolean;
    /// <summary>Restores Low and High from DefaultLow and DefaultHigh.</summary>
    procedure ResetToDefault;
    /// <summary>Classifies a value against this range.</summary>
    /// <param name="AValue">Value in the range unit.</param>
    /// <returns>Normal, warning, or alarm.</returns>
    function Level(AValue: Double): TOBDAlertLevel;
  published
    /// <summary>Mode 01 PID number, or 0 when only Key identifies it.</summary>
    property PID: Integer read FPID write SetPID;
    /// <summary>Stable non-PID identifier such as dpf_dp.</summary>
    property Key: string read FKey write SetKey;
    /// <summary>Human-readable parameter name.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Display unit such as °C, %, rpm, or kPa.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Lower end of the drawn scale.</summary>
    property Min: Double read FMin write SetMin;
    /// <summary>Upper end of the drawn scale.</summary>
    property Max: Double read FMax write SetMax;
    /// <summary>Lower end of the current normal band.</summary>
    property Low: Double read FLow write SetLow;
    /// <summary>Upper end of the current normal band.</summary>
    property High: Double read FHigh write SetHigh;
    /// <summary>Shipped lower normal-band value.</summary>
    property DefaultLow: Double read FDefaultLow write SetDefaultLow;
    /// <summary>Shipped upper normal-band value.</summary>
    property DefaultHigh: Double read FDefaultHigh write SetDefaultHigh;
    /// <summary>Fraction of the normal band outside which warnings become alarms.</summary>
    property AlarmMargin: Double read FAlarmMargin write SetAlarmMargin;
    /// <summary>Number of decimals to show for the value.</summary>
    property Decimals: Integer read FDecimals write SetDecimals;
  end;

  /// <summary>Forward declaration of the owning component.</summary>
  TOBDRangeProfile = class;

  /// <summary>Owned collection of OBD value ranges.</summary>
  TOBDValueRanges = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDValueRange;
    procedure SetItem(AIndex: Integer; AValue: TOBDValueRange);
  protected
    /// <summary>Notifies the owning profile when an item changes.</summary>
    /// <param name="AItem">Changed item, or nil for the whole collection.</param>
    procedure Update(AItem: TCollectionItem); override;
  public
    /// <summary>Creates a collection owned by a range profile.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds an empty value range.</summary>
    /// <returns>The new range item.</returns>
    function Add: TOBDValueRange;
    /// <summary>Finds the first range for a Mode 01 PID.</summary>
    /// <param name="APID">PID number.</param>
    /// <returns>The range, or nil.</returns>
    function FindPID(APID: Integer): TOBDValueRange;
    /// <summary>Finds the first range with a stable key.</summary>
    /// <param name="AKey">Case-insensitive key.</param>
    /// <returns>The range, or nil.</returns>
    function FindKey(const AKey: string): TOBDValueRange;
    /// <summary>Typed access to collection items.</summary>
    property Items[AIndex: Integer]: TOBDValueRange read GetItem
      write SetItem; default;
  end;

  /// <summary>Editable set of normal ranges for one garage or vehicle profile.</summary>
  TOBDRangeProfile = class(TComponent)
  strict private
    FProfileName: string;
    FDescription: string;
    FVehicle: string;
    FEngineCodes: string;
    FRanges: TOBDValueRanges;
    FOnChange: TNotifyEvent;
    FListeners: TArray<TNotifyEvent>;
    FLoading: Boolean;
    function IndexOfListener(const AEvent: TNotifyEvent): Integer;
    procedure SetProfileName(const AValue: string);
    procedure SetDescription(const AValue: string);
    procedure SetVehicle(const AValue: string);
    procedure SetEngineCodes(const AValue: string);
    procedure SetRanges(AValue: TOBDValueRanges);
    procedure DoChange;
    procedure RangesChanged;
  public
    /// <summary>Creates an empty range profile.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases the owned ranges.</summary>
    destructor Destroy; override;
    /// <summary>Loads a range profile from a UTF-8 JSON file.</summary>
    /// <param name="AFileName">JSON file name.</param>
    procedure LoadFromFile(const AFileName: string);
    /// <summary>Saves the profile as UTF-8 JSON without a byte-order mark.</summary>
    /// <param name="AFileName">JSON file name.</param>
    procedure SaveToFile(const AFileName: string);
    /// <summary>Loads a range profile from a JSON string.</summary>
    /// <param name="AJSON">Profile JSON document.</param>
    procedure LoadFromJSON(const AJSON: string);
    /// <summary>Serialises current values and shipped defaults to JSON.</summary>
    /// <returns>Profile JSON document.</returns>
    function ToJSON: string;
    /// <summary>Loads the built-in generic workshop defaults.</summary>
    procedure LoadDefaults;
    /// <summary>Counts ranges whose current band differs from the default band.</summary>
    /// <returns>Number of modified ranges.</returns>
    function ModifiedCount: Integer;
    /// <summary>Resets every range to its shipped default band.</summary>
    procedure ResetAll;
    /// <summary>Registers a handler that runs on every change, next to
    /// <see cref="OnChange"/>. Controls that show the profile use this,
    /// so the host keeps OnChange for itself.</summary>
    /// <param name="AEvent">Handler to add; a handler is added once.</param>
    procedure AddChangeListener(const AEvent: TNotifyEvent);
    /// <summary>Removes a handler added with
    /// <see cref="AddChangeListener"/>.</summary>
    /// <param name="AEvent">Handler to remove.</param>
    procedure RemoveChangeListener(const AEvent: TNotifyEvent);
  published
    /// <summary>Profile identifier independent from TComponent.Name.</summary>
    property ProfileName: string read FProfileName write SetProfileName;
    /// <summary>Human-readable profile description.</summary>
    property Description: string read FDescription write SetDescription;
    /// <summary>Vehicle or family this profile targets.</summary>
    property Vehicle: string read FVehicle write SetVehicle;
    /// <summary>Comma-separated engine codes this profile applies to.</summary>
    property EngineCodes: string read FEngineCodes write SetEngineCodes;
    /// <summary>Editable range definitions.</summary>
    property Ranges: TOBDValueRanges read FRanges write SetRanges;
    /// <summary>Fires after profile metadata or range values change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

/// <summary>Classifies a value against a normal band without using VCL paint code.</summary>
/// <param name="AValue">Value to classify.</param>
/// <param name="ALow">Low end of the normal band.</param>
/// <param name="AHigh">High end of the normal band.</param>
/// <param name="AAlarmMargin">Warning width as a fraction of the band width.</param>
/// <returns>Normal inside the band, warning just outside, alarm further out.</returns>
function OBDRangeLevelOf(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TOBDAlertLevel;

implementation

uses
  System.IOUtils;

const
  RANGE_SCHEMA_URL = 'https://erdesigns.eu/schema/range-profile.json';
  DEFAULT_ALARM_MARGIN = 0.5;

function OBDRangeLevelOf(AValue, ALow, AHigh, AAlarmMargin: Double)
  : TOBDAlertLevel;
var
  Band, Outside: Double;
begin
  if ALow > AHigh then
  begin
    Band := ALow;
    ALow := AHigh;
    AHigh := Band;
  end;
  if (AValue >= ALow) and (AValue <= AHigh) then
    Exit(alvNormal);
  Band := AHigh - ALow;
  if AValue < ALow then
    Outside := ALow - AValue
  else
    Outside := AValue - AHigh;
  if (Band > 0) and (Outside <= Band * AAlarmMargin) then
    Result := alvWarning
  else
    Result := alvAlarm;
end;

function JSONString(AObject: TJSONObject; const AKeys: array of string;
  const ADefault: string): string;
var
  Key: string;
  Value: TJSONValue;
begin
  Result := ADefault;
  if AObject = nil then
    Exit;
  for Key in AKeys do
  begin
    Value := AObject.GetValue(Key);
    if Value is TJSONString then
      Exit(TJSONString(Value).Value);
  end;
end;

function JSONStringOrArray(AObject: TJSONObject; const AKeys: array of string;
  const ADefault: string): string;
var
  Key: string;
  Value: TJSONValue;
  ArrayValue: TJSONArray;
  Index: Integer;
begin
  Result := JSONString(AObject, AKeys, '');
  if Result <> '' then
    Exit;
  if AObject = nil then
    Exit(ADefault);
  for Key in AKeys do
  begin
    Value := AObject.GetValue(Key);
    if Value is TJSONArray then
    begin
      ArrayValue := TJSONArray(Value);
      for Index := 0 to ArrayValue.Count - 1 do
      begin
        if not (ArrayValue.Items[Index] is TJSONString) then
          Continue;
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + TJSONString(ArrayValue.Items[Index]).Value;
      end;
      if Result <> '' then
        Exit;
    end;
  end;
  Result := ADefault;
end;

function JSONNumber(AObject: TJSONObject; const AKeys: array of string;
  ADefault: Double): Double;
var
  Key: string;
  Value: TJSONValue;
begin
  Result := ADefault;
  if AObject = nil then
    Exit;
  for Key in AKeys do
  begin
    Value := AObject.GetValue(Key);
    if Value is TJSONNumber then
      Exit(TJSONNumber(Value).AsDouble);
  end;
end;

function JSONInteger(AObject: TJSONObject; const AKeys: array of string;
  ADefault: Integer): Integer;
begin
  Result := Round(JSONNumber(AObject, AKeys, ADefault));
end;

function IsHexText(const AText: string): Boolean;
var
  Ch: Char;
begin
  Result := AText <> '';
  if not Result then
    Exit;
  for Ch in AText do
    if not CharInSet(Ch, ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
end;

function JSONPID(AObject: TJSONObject; ADefault: Integer): Integer;
var
  Parsed: Integer;
  Text: string;
  Value: TJSONValue;
begin
  Result := ADefault;
  if AObject = nil then
    Exit;
  Value := AObject.GetValue('pid');
  if Value is TJSONNumber then
    Exit(TJSONNumber(Value).AsInt);
  if Value is TJSONString then
  begin
    Text := Trim(TJSONString(Value).Value);
    if SameText(Copy(Text, 1, 2), '0x') then
      Text := '$' + Copy(Text, 3, MaxInt)
    else if IsHexText(Text) then
      Text := '$' + Text;
    if TryStrToInt(Text, Parsed) then
      Result := Parsed;
  end;
end;

function RangeLabel(ARange: TOBDValueRange): string;
begin
  Result := ARange.Caption;
  if Result <> '' then
    Exit;
  if ARange.Key <> '' then
    Exit(ARange.Key);
  if ARange.PID <> 0 then
    Exit('PID $' + IntToHex(ARange.PID, 2));
  Result := '(unnamed)';
end;

procedure ValidateRange(ARange: TOBDValueRange; AIndex: Integer);
begin
  if ARange.Min >= ARange.Max then
    raise EOBDRangeProfileError.CreateFmt(
      'Range %d (%s): Min must be less than Max.',
      [AIndex + 1, RangeLabel(ARange)]);
  if (ARange.Low < ARange.Min) or (ARange.Low > ARange.Max) then
    raise EOBDRangeProfileError.CreateFmt(
      'Range %d (%s): Low must be between Min and Max.',
      [AIndex + 1, RangeLabel(ARange)]);
  if (ARange.High < ARange.Min) or (ARange.High > ARange.Max) then
    raise EOBDRangeProfileError.CreateFmt(
      'Range %d (%s): High must be between Min and Max.',
      [AIndex + 1, RangeLabel(ARange)]);
  if ARange.Low > ARange.High then
    raise EOBDRangeProfileError.CreateFmt(
      'Range %d (%s): Low must not be greater than High.',
      [AIndex + 1, RangeLabel(ARange)]);
end;

procedure AddDefaultRange(AProfile: TOBDRangeProfile; APID: Integer;
  const AKey, ACaption, AUnitText: string; AMin, AMax, ALow, AHigh: Double;
  ADecimals: Integer);
var
  Range: TOBDValueRange;
begin
  Range := AProfile.Ranges.Add;
  Range.PID := APID;
  Range.Key := AKey;
  Range.Caption := ACaption;
  Range.UnitText := AUnitText;
  Range.Min := AMin;
  Range.Max := AMax;
  Range.Low := ALow;
  Range.High := AHigh;
  Range.DefaultLow := ALow;
  Range.DefaultHigh := AHigh;
  Range.AlarmMargin := DEFAULT_ALARM_MARGIN;
  Range.Decimals := ADecimals;
end;

procedure ReadRange(AObject: TJSONObject; ARange: TOBDValueRange;
  AIndex: Integer);
begin
  ARange.PID := JSONPID(AObject, 0);
  ARange.Key := JSONString(AObject, ['key'], '');
  ARange.Caption := JSONString(AObject, ['caption', 'name'], '');
  ARange.UnitText := JSONString(AObject, ['unit', 'unit_text'], '');
  ARange.Min := JSONNumber(AObject, ['min'], 0);
  ARange.Max := JSONNumber(AObject, ['max'], 0);
  ARange.Low := JSONNumber(AObject, ['low'], ARange.Min);
  ARange.High := JSONNumber(AObject, ['high'], ARange.Max);
  ARange.DefaultLow := JSONNumber(AObject, ['default_low', 'defaultLow'],
    ARange.Low);
  ARange.DefaultHigh := JSONNumber(AObject, ['default_high', 'defaultHigh'],
    ARange.High);
  ARange.AlarmMargin := JSONNumber(AObject, ['alarm_margin', 'alarmMargin'],
    DEFAULT_ALARM_MARGIN);
  ARange.Decimals := JSONInteger(AObject, ['decimals'], 0);
  ValidateRange(ARange, AIndex);
end;

function RangeToJSON(ARange: TOBDValueRange): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('pid', TJSONNumber.Create(ARange.PID));
  Result.AddPair('key', ARange.Key);
  Result.AddPair('caption', ARange.Caption);
  Result.AddPair('unit', ARange.UnitText);
  Result.AddPair('min', TJSONNumber.Create(ARange.Min));
  Result.AddPair('max', TJSONNumber.Create(ARange.Max));
  Result.AddPair('low', TJSONNumber.Create(ARange.Low));
  Result.AddPair('high', TJSONNumber.Create(ARange.High));
  Result.AddPair('default_low', TJSONNumber.Create(ARange.DefaultLow));
  Result.AddPair('default_high', TJSONNumber.Create(ARange.DefaultHigh));
  Result.AddPair('alarm_margin', TJSONNumber.Create(ARange.AlarmMargin));
  Result.AddPair('decimals', TJSONNumber.Create(ARange.Decimals));
end;

{ TOBDValueRange ------------------------------------------------------------- }

constructor TOBDValueRange.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAlarmMargin := DEFAULT_ALARM_MARGIN;
end;

procedure TOBDValueRange.Assign(ASource: TPersistent);
var
  Range: TOBDValueRange;
begin
  if ASource is TOBDValueRange then
  begin
    Range := TOBDValueRange(ASource);
    FPID := Range.FPID;
    FKey := Range.FKey;
    FCaption := Range.FCaption;
    FUnitText := Range.FUnitText;
    FMin := Range.FMin;
    FMax := Range.FMax;
    FLow := Range.FLow;
    FHigh := Range.FHigh;
    FDefaultLow := Range.FDefaultLow;
    FDefaultHigh := Range.FDefaultHigh;
    FAlarmMargin := Range.FAlarmMargin;
    FDecimals := Range.FDecimals;
    Changed(False);
  end
  else
    inherited Assign(ASource);
end;

function TOBDValueRange.IsModified: Boolean;
begin
  Result := (not SameValue(FLow, FDefaultLow)) or
    (not SameValue(FHigh, FDefaultHigh));
end;

procedure TOBDValueRange.ResetToDefault;
begin
  Low := FDefaultLow;
  High := FDefaultHigh;
end;

function TOBDValueRange.Level(AValue: Double): TOBDAlertLevel;
begin
  Result := OBDRangeLevelOf(AValue, FLow, FHigh, FAlarmMargin);
end;

procedure TOBDValueRange.SetPID(AValue: Integer);
begin
  if FPID = AValue then
    Exit;
  FPID := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetKey(const AValue: string);
begin
  if FKey = AValue then
    Exit;
  FKey := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetMin(AValue: Double);
begin
  if SameValue(FMin, AValue) then
    Exit;
  FMin := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetMax(AValue: Double);
begin
  if SameValue(FMax, AValue) then
    Exit;
  FMax := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetLow(AValue: Double);
begin
  if SameValue(FLow, AValue) then
    Exit;
  FLow := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetHigh(AValue: Double);
begin
  if SameValue(FHigh, AValue) then
    Exit;
  FHigh := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetDefaultLow(AValue: Double);
begin
  if SameValue(FDefaultLow, AValue) then
    Exit;
  FDefaultLow := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetDefaultHigh(AValue: Double);
begin
  if SameValue(FDefaultHigh, AValue) then
    Exit;
  FDefaultHigh := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetAlarmMargin(AValue: Double);
begin
  if SameValue(FAlarmMargin, AValue) then
    Exit;
  FAlarmMargin := AValue;
  Changed(False);
end;

procedure TOBDValueRange.SetDecimals(AValue: Integer);
begin
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Changed(False);
end;

{ TOBDValueRanges ------------------------------------------------------------ }

constructor TOBDValueRanges.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDValueRange);
end;

function TOBDValueRanges.Add: TOBDValueRange;
begin
  Result := TOBDValueRange(inherited Add);
end;

function TOBDValueRanges.FindPID(APID: Integer): TOBDValueRange;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Count - 1 do
    if Items[Index].PID = APID then
      Exit(Items[Index]);
end;

function TOBDValueRanges.FindKey(const AKey: string): TOBDValueRange;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Count - 1 do
    if SameText(Items[Index].Key, AKey) then
      Exit(Items[Index]);
end;

function TOBDValueRanges.GetItem(AIndex: Integer): TOBDValueRange;
begin
  Result := TOBDValueRange(inherited Items[AIndex]);
end;

procedure TOBDValueRanges.SetItem(AIndex: Integer; AValue: TOBDValueRange);
begin
  inherited Items[AIndex] := AValue;
end;

procedure TOBDValueRanges.Update(AItem: TCollectionItem);
begin
  inherited Update(AItem);
  if GetOwner is TOBDRangeProfile then
    TOBDRangeProfile(GetOwner).RangesChanged;
end;

{ TOBDRangeProfile ----------------------------------------------------------- }

constructor TOBDRangeProfile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TOBDValueRanges.Create(Self);
end;

destructor TOBDRangeProfile.Destroy;
begin
  FRanges.Free;
  inherited Destroy;
end;

procedure TOBDRangeProfile.LoadFromFile(const AFileName: string);
begin
  LoadFromJSON(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

procedure TOBDRangeProfile.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllBytes(AFileName, TEncoding.UTF8.GetBytes(ToJSON));
end;

procedure TOBDRangeProfile.LoadFromJSON(const AJSON: string);
var
  Document, Value: TJSONValue;
  Root, RangeObject: TJSONObject;
  RangeArray: TJSONArray;
  TempRanges: TOBDValueRanges;
  Index: Integer;
begin
  Document := TJSONObject.ParseJSONValue(AJSON);
  if Document = nil then
    raise EOBDRangeProfileError.Create('Range profile JSON is not valid JSON.');
  TempRanges := TOBDValueRanges.Create(nil);
  try
    if not(Document is TJSONObject) then
      raise EOBDRangeProfileError.Create('Range profile JSON root must be an object.');
    Root := TJSONObject(Document);
    Value := Root.GetValue('ranges');
    if Value = nil then
      Value := Root.GetValue('entries');
    if (Value <> nil) and not(Value is TJSONArray) then
      raise EOBDRangeProfileError.Create('Range profile ranges must be an array.');
    if Value is TJSONArray then
    begin
      RangeArray := TJSONArray(Value);
      for Index := 0 to RangeArray.Count - 1 do
      begin
        if not(RangeArray.Items[Index] is TJSONObject) then
          raise EOBDRangeProfileError.CreateFmt(
            'Range %d must be a JSON object.', [Index + 1]);
        RangeObject := TJSONObject(RangeArray.Items[Index]);
        ReadRange(RangeObject, TempRanges.Add, Index);
      end;
    end;
    FLoading := True;
    try
      FProfileName := JSONString(Root, ['profile', 'profile_name', 'name'], '');
      FDescription := JSONString(Root, ['description'], '');
      FVehicle := JSONString(Root, ['vehicle'], '');
      FEngineCodes := JSONStringOrArray(Root,
        ['engine_codes', 'engineCodes', 'engines'], '');
      FRanges.Assign(TempRanges);
    finally
      FLoading := False;
    end;
    DoChange;
  finally
    TempRanges.Free;
    Document.Free;
  end;
end;

function TOBDRangeProfile.ToJSON: string;
var
  Root: TJSONObject;
  RangeArray: TJSONArray;
  EngineArray: TJSONArray;
  EngineCode: string;
  Index: Integer;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('$schema', RANGE_SCHEMA_URL);
    Root.AddPair('schema_version', TJSONNumber.Create(1));
    Root.AddPair('type', 'range-profile');
    Root.AddPair('profile', FProfileName);
    Root.AddPair('description', FDescription);
    Root.AddPair('vehicle', FVehicle);
    EngineArray := TJSONArray.Create;
    for EngineCode in FEngineCodes.Split([',']) do
      if Trim(EngineCode) <> '' then
        EngineArray.Add(Trim(EngineCode));
    Root.AddPair('engine_codes', EngineArray);
    RangeArray := TJSONArray.Create;
    for Index := 0 to FRanges.Count - 1 do
      RangeArray.AddElement(RangeToJSON(FRanges[Index]));
    Root.AddPair('ranges', RangeArray);
    Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

procedure TOBDRangeProfile.LoadDefaults;
begin
  FLoading := True;
  try
    FProfileName := 'generic';
    FDescription := 'Generic petrol/diesel-neutral OBD-II normal ranges. Source: generic workshop defaults.';
    FVehicle := 'Generic OBD-II vehicle';
    FEngineCodes := '';
    FRanges.Clear;
    AddDefaultRange(Self, $04, 'calculated_load', 'Calculated load', '%',
      0, 100, 0, 85, 0);
    AddDefaultRange(Self, $05, 'coolant_temp', 'Coolant temperature', '°C',
      -40, 130, 70, 105, 0);
    AddDefaultRange(Self, $0B, 'intake_map', 'Intake manifold absolute pressure',
      'kPa', 0, 300, 90, 230, 0);
    AddDefaultRange(Self, $0C, 'engine_speed', 'Engine speed', 'rpm',
      0, 8000, 600, 4500, 0);
    AddDefaultRange(Self, $0D, 'vehicle_speed', 'Vehicle speed', 'km/h',
      0, 200, 0, 200, 0);
    AddDefaultRange(Self, $0F, 'intake_air_temp', 'Intake air temperature', '°C',
      -40, 80, -20, 50, 0);
    AddDefaultRange(Self, $06, 'stft_b1', 'Short-term fuel trim bank 1', '%',
      -25, 25, -10, 10, 1);
    AddDefaultRange(Self, $07, 'ltft_b1', 'Long-term fuel trim bank 1', '%',
      -25, 25, -10, 10, 1);
    AddDefaultRange(Self, $10, 'maf', 'Mass air flow', 'g/s',
      0, 250, 2, 200, 1);
    AddDefaultRange(Self, $11, 'throttle_position', 'Throttle position', '%',
      0, 100, 0, 100, 0);
    AddDefaultRange(Self, $2C, 'commanded_egr', 'Commanded EGR', '%',
      0, 100, 0, 60, 0);
    AddDefaultRange(Self, $2D, 'egr_error', 'EGR error', '%',
      -50, 50, -10, 10, 1);
    AddDefaultRange(Self, $42, 'control_module_voltage',
      'Control module voltage', 'V', 10, 16, 12.2, 14.8, 1);
    AddDefaultRange(Self, $7A, 'dpf_dp', 'DPF differential pressure', 'kPa',
      0, 30, 0, 12, 1);
    AddDefaultRange(Self, 0, 'boost_desired', 'Boost desired pressure', 'kPa',
      0, 300, 90, 230, 0);
  finally
    FLoading := False;
  end;
  DoChange;
end;

function TOBDRangeProfile.ModifiedCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to FRanges.Count - 1 do
    if FRanges[Index].IsModified then
      Inc(Result);
end;

procedure TOBDRangeProfile.ResetAll;
var
  Index: Integer;
begin
  FLoading := True;
  try
    for Index := 0 to FRanges.Count - 1 do
      FRanges[Index].ResetToDefault;
  finally
    FLoading := False;
  end;
  DoChange;
end;

procedure TOBDRangeProfile.SetProfileName(const AValue: string);
begin
  if FProfileName = AValue then
    Exit;
  FProfileName := AValue;
  DoChange;
end;

procedure TOBDRangeProfile.SetDescription(const AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
  DoChange;
end;

procedure TOBDRangeProfile.SetVehicle(const AValue: string);
begin
  if FVehicle = AValue then
    Exit;
  FVehicle := AValue;
  DoChange;
end;

procedure TOBDRangeProfile.SetEngineCodes(const AValue: string);
begin
  if FEngineCodes = AValue then
    Exit;
  FEngineCodes := AValue;
  DoChange;
end;

procedure TOBDRangeProfile.SetRanges(AValue: TOBDValueRanges);
begin
  if AValue = nil then
    FRanges.Clear
  else
    FRanges.Assign(AValue);
  DoChange;
end;

procedure TOBDRangeProfile.DoChange;
var
  Listeners: TArray<TNotifyEvent>;
  I: Integer;
begin
  if FLoading then
    Exit;
  if Assigned(FOnChange) then
    FOnChange(Self);
  Listeners := Copy(FListeners);
  for I := 0 to High(Listeners) do
    Listeners[I](Self);
end;

function TOBDRangeProfile.IndexOfListener(const AEvent: TNotifyEvent): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FListeners) do
    if (TMethod(FListeners[I]).Code = TMethod(AEvent).Code) and
      (TMethod(FListeners[I]).Data = TMethod(AEvent).Data) then
      Exit(I);
  Result := -1;
end;

procedure TOBDRangeProfile.AddChangeListener(const AEvent: TNotifyEvent);
begin
  if not Assigned(AEvent) or (IndexOfListener(AEvent) >= 0) then
    Exit;
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[High(FListeners)] := AEvent;
end;

procedure TOBDRangeProfile.RemoveChangeListener(const AEvent: TNotifyEvent);
var
  I: Integer;
begin
  I := IndexOfListener(AEvent);
  if I >= 0 then
    Delete(FListeners, I, 1);
end;

procedure TOBDRangeProfile.RangesChanged;
begin
  DoChange;
end;

end.
