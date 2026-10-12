//------------------------------------------------------------------------------
//  untGarage
//
//  The vehicle garage of OBD-Studio Personal: the cars of the user, each
//  with its own history (scans, codes read and cleared, readiness checks,
//  monitor tests, recordings, performance runs, notes).
//
//  Local first: one JSON file per vehicle in the Garage folder of the user
//  data (Documents\OBD-Studio Personal\Garage\<id>.json). The files are
//  plain JSON so the user can back them up, move or delete them without
//  the program. A damaged file is skipped (and reported in LastError); it
//  is never overwritten. Every write goes to a temporary file first and
//  then replaces the old file.
//
//  The garage is used on the main thread only.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untGarage;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

const
  /// <summary>Oldest history entries are dropped beyond this count.</summary>
  PERSONAL_HISTORY_LIMIT = 2000;
  /// <summary>An entry equal to the last one (same kind, level, title and
  /// detail) within this many minutes only moves the time of the last
  /// one, so opening a page again does not repeat the same result.</summary>
  PERSONAL_HISTORY_REPEAT_MINUTES = 30;

type
  /// <summary>What a history entry records.</summary>
  TPersonalHistoryKind = (
    /// <summary>Overview scan of the whole car.</summary>
    phkScan,
    /// <summary>Trouble codes read.</summary>
    phkCodesRead,
    /// <summary>Trouble codes cleared.</summary>
    phkCodesCleared,
    /// <summary>Readiness monitors or inspection check.</summary>
    phkReadiness,
    /// <summary>Monitor test results (Mode 05, 06, 08, IPT).</summary>
    phkTests,
    /// <summary>Vehicle information read (Mode 09).</summary>
    phkVehicleInfo,
    /// <summary>A recording was saved.</summary>
    phkRecording,
    /// <summary>A performance run finished.</summary>
    phkPerformance,
    /// <summary>EV battery read.</summary>
    phkBattery,
    /// <summary>A note written by the user.</summary>
    phkNote);

  /// <summary>How an entry is judged; the history shows it as a coloured
  /// chip.</summary>
  TPersonalHistoryLevel = (
    /// <summary>Plain information.</summary>
    phlInfo,
    /// <summary>All good (no codes, ready, test passed).</summary>
    phlGood,
    /// <summary>Needs attention (pending codes, not ready).</summary>
    phlWarning,
    /// <summary>A problem (stored codes, check engine light, test
    /// failed).</summary>
    phlProblem);

  /// <summary>One line of a vehicle's history. Title and Detail are stored
  /// in the language the program used when the entry was written.</summary>
  TPersonalHistoryEntry = record
    Time: TDateTime;
    Kind: TPersonalHistoryKind;
    Level: TPersonalHistoryLevel;
    Title: string;
    Detail: string;
  end;

  /// <summary>One car of the user.</summary>
  TPersonalVehicle = class
  strict private
    FId: string;
    FName: string;
    FVIN: string;
    FPlate: string;
    FMake: string;
    FModel: string;
    FModelYear: Integer;
    FFuel: string;
    FEngineCC: Integer;
    FInspectionCountry: string;
    FNotes: string;
    FAdded: TDateTime;
    FLastSeen: TDateTime;
    FHistory: TList<TPersonalHistoryEntry>;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>Name for lists and titles: the name, else make, model and
    /// year, else the VIN; empty when nothing is known.</summary>
    function DisplayName: string;
    /// <summary>Make, model and year in one line ('' when unknown).</summary>
    function Description: string;

    /// <summary>Stable identifier, also the file name.</summary>
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    /// <summary>17-character VIN; empty when the car does not report it.</summary>
    property VIN: string read FVIN write FVIN;
    /// <summary>Licence plate (optional).</summary>
    property Plate: string read FPlate write FPlate;
    property Make: string read FMake write FMake;
    property Model: string read FModel write FModel;
    /// <summary>0 when unknown.</summary>
    property ModelYear: Integer read FModelYear write FModelYear;
    /// <summary>PERSONAL_FUEL_* value; empty when unknown.</summary>
    property Fuel: string read FFuel write FFuel;
    /// <summary>Engine displacement in cc; 0 when unknown.</summary>
    property EngineCC: Integer read FEngineCC write FEngineCC;
    /// <summary>ISO 3166 country of the periodic inspection of this car;
    /// empty means the country in the settings.</summary>
    property InspectionCountry: string read FInspectionCountry
      write FInspectionCountry;
    property Notes: string read FNotes write FNotes;
    property Added: TDateTime read FAdded write FAdded;
    /// <summary>Last connection with this car; 0 when never connected.</summary>
    property LastSeen: TDateTime read FLastSeen write FLastSeen;
    /// <summary>Oldest entry first.</summary>
    property History: TList<TPersonalHistoryEntry> read FHistory;
  end;

  /// <summary>All vehicles of the user, loaded from the Garage folder.</summary>
  TPersonalGarage = class
  strict private
    FFolder: string;
    FVehicles: TObjectList<TPersonalVehicle>;
    FLastError: string;
    function FileNameOf(AVehicle: TPersonalVehicle): string;
    function LoadFile(const AFileName: string): TPersonalVehicle;
    function GetCount: Integer;
    function GetVehicle(AIndex: Integer): TPersonalVehicle;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>Reads every vehicle file in AFolder (created when missing).
    /// Damaged files are skipped and named in LastError.</summary>
    procedure Open(const AFolder: string);
    /// <summary>Writes one vehicle with its history.</summary>
    procedure Save(AVehicle: TPersonalVehicle);
    /// <summary>Adds a vehicle, saves it and returns it.</summary>
    function Add(const AName, AVIN: string): TPersonalVehicle;
    /// <summary>Removes a vehicle and deletes its file.</summary>
    procedure Delete(AVehicle: TPersonalVehicle);
    /// <summary>The vehicle with this Id, or nil.</summary>
    function FindById(const AId: string): TPersonalVehicle;
    /// <summary>The vehicle with this VIN (case-insensitive), or nil.</summary>
    function FindByVIN(const AVIN: string): TPersonalVehicle;
    /// <summary>Index in the list, or -1.</summary>
    function IndexOf(AVehicle: TPersonalVehicle): Integer;
    /// <summary>Appends a history entry (time = now) and saves. The same
    /// result again within PERSONAL_HISTORY_REPEAT_MINUTES updates the
    /// time of the last entry instead; notes are always added.</summary>
    procedure AddHistory(AVehicle: TPersonalVehicle;
      AKind: TPersonalHistoryKind; ALevel: TPersonalHistoryLevel;
      const ATitle, ADetail: string);
    /// <summary>Removes every history entry of the vehicle and saves.</summary>
    procedure ClearHistory(AVehicle: TPersonalVehicle);
    /// <summary>Writes the history as CSV (UTF-8, semicolon separated,
    /// so Excel in European locales opens it in columns).</summary>
    procedure ExportHistoryCSV(AVehicle: TPersonalVehicle;
      const AFileName: string);

    property Folder: string read FFolder;
    property Count: Integer read GetCount;
    property Vehicles[AIndex: Integer]: TPersonalVehicle read GetVehicle;
      default;
    /// <summary>Problems of the last Open or Save ('' = none).</summary>
    property LastError: string read FLastError;
  end;

const
  PERSONAL_FUEL_PETROL   = 'petrol';
  PERSONAL_FUEL_DIESEL   = 'diesel';
  PERSONAL_FUEL_LPG      = 'lpg';
  PERSONAL_FUEL_CNG      = 'cng';
  PERSONAL_FUEL_HYBRID   = 'hybrid';
  PERSONAL_FUEL_ELECTRIC = 'electric';

/// <summary>Stable text of a history kind, as stored in the files.</summary>
function HistoryKindToText(AKind: TPersonalHistoryKind): string;
/// <summary>Kind from its stored text; phkNote when unknown.</summary>
function TextToHistoryKind(const AText: string): TPersonalHistoryKind;

implementation

uses
  System.IOUtils,
  System.JSON,
  System.DateUtils,
  ERD.Types,
  ERD.JSON;

const
  VEHICLE_FORMAT = 'obd-studio-personal-vehicle';
  VEHICLE_FORMAT_VERSION = 1;

  HISTORY_KIND_TEXT: array[TPersonalHistoryKind] of string = (
    'scan', 'codesRead', 'codesCleared', 'readiness', 'tests',
    'vehicleInfo', 'recording', 'performance', 'battery', 'note');
  HISTORY_LEVEL_TEXT: array[TPersonalHistoryLevel] of string = (
    'info', 'good', 'warning', 'problem');

function HistoryKindToText(AKind: TPersonalHistoryKind): string;
begin
  Result := HISTORY_KIND_TEXT[AKind];
end;

function TextToHistoryKind(const AText: string): TPersonalHistoryKind;
var
  Kind: TPersonalHistoryKind;
begin
  for Kind := Low(TPersonalHistoryKind) to High(TPersonalHistoryKind) do
    if SameText(AText, HISTORY_KIND_TEXT[Kind]) then
      Exit(Kind);
  Result := phkNote;
end;

function TextToHistoryLevel(const AText: string): TPersonalHistoryLevel;
var
  Level: TPersonalHistoryLevel;
begin
  for Level := Low(TPersonalHistoryLevel) to High(TPersonalHistoryLevel) do
    if SameText(AText, HISTORY_LEVEL_TEXT[Level]) then
      Exit(Level);
  Result := phlInfo;
end;

// Local time as 'yyyy-mm-ddThh:nn:ss' (no zone: the history is read on
// the same PC). 0 is stored as an empty string.
function DateToText(AValue: TDateTime): string;
begin
  if AValue = 0 then
    Result := ''
  else
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', AValue);
end;

function TextToDate(const AText: string): TDateTime;
var
  Y, M, D, H, N, S: Integer;
begin
  Result := 0;
  if Length(AText) < 10 then
    Exit;
  Y := StrToIntDef(Copy(AText, 1, 4), 0);
  M := StrToIntDef(Copy(AText, 6, 2), 0);
  D := StrToIntDef(Copy(AText, 9, 2), 0);
  H := 0;
  N := 0;
  S := 0;
  if Length(AText) >= 19 then
  begin
    H := StrToIntDef(Copy(AText, 12, 2), 0);
    N := StrToIntDef(Copy(AText, 15, 2), 0);
    S := StrToIntDef(Copy(AText, 18, 2), 0);
  end;
  if not TryEncodeDateTime(Y, M, D, H, N, S, 0, Result) then
    Result := 0;
end;

function NewVehicleId: string;
var
  Guid: TGUID;
begin
  if CreateGUID(Guid) <> 0 then
    Exit(FormatDateTime('yyyymmddhhnnsszzz', Now));
  Result := GUIDToString(Guid);
  // '{XXXXXXXX-...}' -> lower case without braces: a safe file name.
  Result := LowerCase(Copy(Result, 2, Length(Result) - 2));
end;

// A field of a CSV line: quoted when it contains the separator, a quote
// or a line break.
function CsvField(const AText: string): string;
begin
  if (Pos(';', AText) > 0) or (Pos('"', AText) > 0) or
    (Pos(#13, AText) > 0) or (Pos(#10, AText) > 0) then
    Result := '"' + StringReplace(AText, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AText;
end;

{ TPersonalVehicle }

constructor TPersonalVehicle.Create;
begin
  inherited Create;
  FHistory := TList<TPersonalHistoryEntry>.Create;
end;

destructor TPersonalVehicle.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

function TPersonalVehicle.Description: string;
begin
  Result := Trim(FMake + ' ' + FModel);
  if FModelYear > 0 then
    Result := Trim(Result + ' ' + IntToStr(FModelYear));
end;

function TPersonalVehicle.DisplayName: string;
begin
  Result := Trim(FName);
  if Result = '' then
    Result := Description;
  if Result = '' then
    Result := FVIN;
end;

{ TPersonalGarage }

constructor TPersonalGarage.Create;
begin
  inherited Create;
  FVehicles := TObjectList<TPersonalVehicle>.Create(True);
end;

destructor TPersonalGarage.Destroy;
begin
  FVehicles.Free;
  inherited Destroy;
end;

function TPersonalGarage.GetCount: Integer;
begin
  Result := FVehicles.Count;
end;

function TPersonalGarage.GetVehicle(AIndex: Integer): TPersonalVehicle;
begin
  Result := FVehicles[AIndex];
end;

function TPersonalGarage.FileNameOf(AVehicle: TPersonalVehicle): string;
begin
  Result := TPath.Combine(FFolder, AVehicle.Id + '.json');
end;

function TPersonalGarage.LoadFile(const AFileName: string): TPersonalVehicle;
var
  Root: TJSONObject;
  Items: TJSONArray;
  Item: TJSONObject;
  Entry: TPersonalHistoryEntry;
  I: Integer;
begin
  Root := ParseOBDJSONObject(TFile.ReadAllText(AFileName, TEncoding.UTF8));
  try
    if not SameText(Root.GetValue<string>('format', ''), VEHICLE_FORMAT) then
      raise EOBDConfig.Create('not an OBD-Studio vehicle file');
    Result := TPersonalVehicle.Create;
    try
      Result.Id := Root.GetValue<string>('id', '');
      if Result.Id = '' then
        Result.Id := TPath.GetFileNameWithoutExtension(AFileName);
      Result.Name := Root.GetValue<string>('name', '');
      Result.VIN := Root.GetValue<string>('vin', '');
      Result.Plate := Root.GetValue<string>('plate', '');
      Result.Make := Root.GetValue<string>('make', '');
      Result.Model := Root.GetValue<string>('model', '');
      Result.ModelYear := Root.GetValue<Integer>('modelYear', 0);
      Result.Fuel := Root.GetValue<string>('fuel', '');
      Result.EngineCC := Root.GetValue<Integer>('engineCC', 0);
      Result.InspectionCountry := Root.GetValue<string>('inspectionCountry',
        '');
      Result.Notes := Root.GetValue<string>('notes', '');
      Result.Added := TextToDate(Root.GetValue<string>('added', ''));
      Result.LastSeen := TextToDate(Root.GetValue<string>('lastSeen', ''));
      if Root.TryGetValue<TJSONArray>('history', Items) then
        for I := 0 to Items.Count - 1 do
          if Items.Items[I] is TJSONObject then
          begin
            Item := TJSONObject(Items.Items[I]);
            Entry.Time := TextToDate(Item.GetValue<string>('time', ''));
            Entry.Kind := TextToHistoryKind(Item.GetValue<string>('kind', ''));
            Entry.Level := TextToHistoryLevel(Item.GetValue<string>('level',
              ''));
            Entry.Title := Item.GetValue<string>('title', '');
            Entry.Detail := Item.GetValue<string>('detail', '');
            Result.History.Add(Entry);
          end;
    except
      Result.Free;
      raise;
    end;
  finally
    Root.Free;
  end;
end;

procedure TPersonalGarage.Open(const AFolder: string);
var
  FileName: string;
  Vehicle: TPersonalVehicle;
  Problems: TStringList;
begin
  FVehicles.Clear;
  FLastError := '';
  FFolder := AFolder;
  if FFolder = '' then
    Exit;
  Problems := TStringList.Create;
  try
    try
      ForceDirectories(FFolder);
      for FileName in TDirectory.GetFiles(FFolder, '*.json') do
        try
          Vehicle := LoadFile(FileName);
          if FindById(Vehicle.Id) <> nil then
            Vehicle.Free // a copy of a file under another name
          else
            FVehicles.Add(Vehicle);
        except
          on E: Exception do
            Problems.Add(ExtractFileName(FileName) + ': ' + E.Message);
        end;
    except
      on E: Exception do
        Problems.Add(E.Message);
    end;
    FLastError := Trim(Problems.Text);
  finally
    Problems.Free;
  end;
end;

procedure TPersonalGarage.Save(AVehicle: TPersonalVehicle);
var
  Root: TJSONObject;
  Items: TJSONArray;
  Item: TJSONObject;
  Entry: TPersonalHistoryEntry;
  FileName, TempName: string;
begin
  if (AVehicle = nil) or (FFolder = '') then
    Exit;
  Root := TJSONObject.Create;
  try
    Root.AddPair('format', VEHICLE_FORMAT);
    Root.AddPair('version', TJSONNumber.Create(VEHICLE_FORMAT_VERSION));
    Root.AddPair('id', AVehicle.Id);
    Root.AddPair('name', AVehicle.Name);
    Root.AddPair('vin', AVehicle.VIN);
    Root.AddPair('plate', AVehicle.Plate);
    Root.AddPair('make', AVehicle.Make);
    Root.AddPair('model', AVehicle.Model);
    Root.AddPair('modelYear', TJSONNumber.Create(AVehicle.ModelYear));
    Root.AddPair('fuel', AVehicle.Fuel);
    Root.AddPair('engineCC', TJSONNumber.Create(AVehicle.EngineCC));
    Root.AddPair('inspectionCountry', AVehicle.InspectionCountry);
    Root.AddPair('notes', AVehicle.Notes);
    Root.AddPair('added', DateToText(AVehicle.Added));
    Root.AddPair('lastSeen', DateToText(AVehicle.LastSeen));
    Items := TJSONArray.Create;
    Root.AddPair('history', Items);
    for Entry in AVehicle.History do
    begin
      Item := TJSONObject.Create;
      Item.AddPair('time', DateToText(Entry.Time));
      Item.AddPair('kind', HISTORY_KIND_TEXT[Entry.Kind]);
      Item.AddPair('level', HISTORY_LEVEL_TEXT[Entry.Level]);
      Item.AddPair('title', Entry.Title);
      Item.AddPair('detail', Entry.Detail);
      Items.AddElement(Item);
    end;
    ForceDirectories(FFolder);
    FileName := FileNameOf(AVehicle);
    TempName := FileName + '.tmp';
    TFile.WriteAllText(TempName, Root.Format(2), TEncoding.UTF8);
    if TFile.Exists(FileName) then
      TFile.Delete(FileName);
    TFile.Move(TempName, FileName);
  finally
    Root.Free;
  end;
end;

function TPersonalGarage.Add(const AName, AVIN: string): TPersonalVehicle;
begin
  Result := TPersonalVehicle.Create;
  Result.Id := NewVehicleId;
  Result.Name := Trim(AName);
  Result.VIN := UpperCase(Trim(AVIN));
  Result.Added := Now;
  FVehicles.Add(Result);
  Save(Result);
end;

procedure TPersonalGarage.Delete(AVehicle: TPersonalVehicle);
var
  FileName: string;
begin
  if (AVehicle = nil) or (FVehicles.IndexOf(AVehicle) < 0) then
    Exit;
  if FFolder <> '' then
  begin
    FileName := FileNameOf(AVehicle);
    if TFile.Exists(FileName) then
      TFile.Delete(FileName);
  end;
  FVehicles.Remove(AVehicle);
end;

function TPersonalGarage.FindById(const AId: string): TPersonalVehicle;
var
  Vehicle: TPersonalVehicle;
begin
  Result := nil;
  if AId = '' then
    Exit;
  for Vehicle in FVehicles do
    if SameText(Vehicle.Id, AId) then
      Exit(Vehicle);
end;

function TPersonalGarage.FindByVIN(const AVIN: string): TPersonalVehicle;
var
  Vehicle: TPersonalVehicle;
begin
  Result := nil;
  if Trim(AVIN) = '' then
    Exit;
  for Vehicle in FVehicles do
    if SameText(Vehicle.VIN, Trim(AVIN)) then
      Exit(Vehicle);
end;

function TPersonalGarage.IndexOf(AVehicle: TPersonalVehicle): Integer;
begin
  Result := FVehicles.IndexOf(AVehicle);
end;

procedure TPersonalGarage.AddHistory(AVehicle: TPersonalVehicle;
  AKind: TPersonalHistoryKind; ALevel: TPersonalHistoryLevel;
  const ATitle, ADetail: string);
var
  Entry, Last: TPersonalHistoryEntry;
begin
  if AVehicle = nil then
    Exit;
  if (AKind <> phkNote) and (AVehicle.History.Count > 0) then
  begin
    Last := AVehicle.History[AVehicle.History.Count - 1];
    if (Last.Kind = AKind) and (Last.Level = ALevel) and
      (Last.Title = ATitle) and (Last.Detail = ADetail) and
      (Now - Last.Time >= 0) and
      (Now - Last.Time < PERSONAL_HISTORY_REPEAT_MINUTES / MinsPerDay) then
    begin
      Last.Time := Now;
      AVehicle.History[AVehicle.History.Count - 1] := Last;
      Save(AVehicle);
      Exit;
    end;
  end;
  Entry.Time := Now;
  Entry.Kind := AKind;
  Entry.Level := ALevel;
  Entry.Title := ATitle;
  Entry.Detail := ADetail;
  AVehicle.History.Add(Entry);
  while AVehicle.History.Count > PERSONAL_HISTORY_LIMIT do
    AVehicle.History.Delete(0);
  Save(AVehicle);
end;

procedure TPersonalGarage.ClearHistory(AVehicle: TPersonalVehicle);
begin
  if AVehicle = nil then
    Exit;
  AVehicle.History.Clear;
  Save(AVehicle);
end;

procedure TPersonalGarage.ExportHistoryCSV(AVehicle: TPersonalVehicle;
  const AFileName: string);
var
  Lines: TStringList;
  Entry: TPersonalHistoryEntry;
begin
  if AVehicle = nil then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Add('time;kind;level;title;detail');
    for Entry in AVehicle.History do
      Lines.Add(CsvField(DateToText(Entry.Time)) + ';' +
        CsvField(HISTORY_KIND_TEXT[Entry.Kind]) + ';' +
        CsvField(HISTORY_LEVEL_TEXT[Entry.Level]) + ';' +
        CsvField(Entry.Title) + ';' + CsvField(Entry.Detail));
    Lines.WriteBOM := True;
    Lines.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    Lines.Free;
  end;
end;

end.
