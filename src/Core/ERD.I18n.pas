// ------------------------------------------------------------------------------
// ERD.I18n
//
// Runtime translation catalogue and helpers for Delphi OBD UI and services.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.I18n;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.JSON,
  System.IOUtils,
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  ERD.Types;

type
  /// <summary>Metadata for a language catalogue discovered on disk.</summary>
  TOBDLanguageInfo = record
    /// <summary>BCP-47-ish language code, for example <c>en</c> or <c>nl</c>.</summary>
    Code: string;
    /// <summary>Native language name shown in Settings.</summary>
    Name: string;
    /// <summary>English language name used for diagnostics and fallback UIs.</summary>
    EnglishName: string;
    /// <summary>Catalogue file path.</summary>
    FileName: string;
  end;

  /// <summary>One/other plural strings from an i18n catalogue.</summary>
  TOBDPluralText = record
    /// <summary>Text used when the count is one.</summary>
    OneText: string;
    /// <summary>Text used for every other count.</summary>
    OtherText: string;
  end;

  /// <summary>
  /// Process-wide, thread-safe translation catalogue with English built-ins.
  /// </summary>
  TOBDTranslator = class
  strict private
    type
      TLanguageCatalog = class
      strict private
        FStrings: TDictionary<string, string>;
        FPlurals: TDictionary<string, TOBDPluralText>;
      public
        Language: string;
        Name: string;
        EnglishName: string;
        Version: Integer;
        constructor Create;
        destructor Destroy; override;
        procedure AddString(const AKey, AValue: string);
        procedure AddPlural(const AKey, AOne, AOther: string);
        function TryString(const AKey: string; out AValue: string): Boolean;
        function TryPlural(const AKey: string; out AValue: TOBDPluralText): Boolean;
      end;
  strict private
    FLock: TCriticalSection;
    FCatalogs: TObjectDictionary<string, TLanguageCatalog>;
    FCurrentLanguage: string;
    FListeners: TArray<TNotifyEvent>;
    function GetCurrentLanguage: string;
    procedure SetCurrentLanguage(const AValue: string);
    procedure LoadCatalog(ACatalog: TLanguageCatalog);
    function FindStringLocked(const ALanguage, AKey: string; out AValue: string): Boolean;
    function FindPluralLocked(const ALanguage, AKey: string; out AValue: TOBDPluralText): Boolean;
    procedure NotifyChanged;
    procedure LoadBuiltInEnglish;
  public
    /// <summary>Creates a translator and loads built-in English strings.</summary>
    constructor Create;
    /// <summary>Releases catalogues and listener state.</summary>
    destructor Destroy; override;

    /// <summary>Current language code used by lookups.</summary>
    property CurrentLanguage: string read GetCurrentLanguage write SetCurrentLanguage;

    /// <summary>Loads a JSON language catalogue from disk.</summary>
    procedure LoadLanguageFile(const AFileName: string);
    /// <summary>Loads a JSON language catalogue from a UTF-8 string.</summary>
    procedure LoadLanguageFromString(const AText: string);
    /// <summary>Loads an RCDATA language catalogue resource on Windows.</summary>
    procedure LoadFromResource(const AResourceName: string);
    /// <summary>Scans a folder for JSON language catalogues.</summary>
    function AvailableLanguages(const AFolder: string): TArray<TOBDLanguageInfo>;

    /// <summary>Looks up a translation with requested-base-English-default fallback.</summary>
    function Tr(const AKey, ADefault: string): string;
    /// <summary>Looks up and formats a translation using Delphi Format syntax.</summary>
    function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
    /// <summary>Looks up a one/other plural translation.</summary>
    function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
      AOtherDefault: string): string;
    /// <summary>Registers a callback fired after language data changes.</summary>
    procedure AddChangeListener(AListener: TNotifyEvent);
    /// <summary>Unregisters a previously registered callback.</summary>
    procedure RemoveChangeListener(AListener: TNotifyEvent);
  end;

/// <summary>Returns the global translator singleton.</summary>
function OBDI18n: TOBDTranslator;
/// <summary>Convenience wrapper around <see cref="TOBDTranslator.Tr"/>.</summary>
function Tr(const AKey, ADefault: string): string;
/// <summary>Convenience wrapper around <see cref="TOBDTranslator.TrFmt"/>.</summary>
function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
/// <summary>Convenience wrapper around <see cref="TOBDTranslator.TrPlural"/>.</summary>
function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
  AOtherDefault: string): string;
/// <summary>Returns the operating-system UI language when it is supported.</summary>
function OBDDefaultLanguageCode: string;

implementation

uses
  ERD.JSON
{$IFDEF MSWINDOWS}
  , {$IFDEF FPC}Windows{$ELSE}Winapi.Windows{$ENDIF}
{$ENDIF}
  ;

type
  TOBDBuiltInString = record
    Key: string;
    Value: string;
  end;

  TOBDBuiltInPlural = record
    Key: string;
    OneText: string;
    OtherText: string;
  end;

const
  BUILTIN_ENGLISH_STRINGS: array [0 .. 212] of TOBDBuiltInString = (
    (Key: 'app.dashboard.health'; Value: 'Vehicle health'),
    (Key: 'app.dashboard.quickActions'; Value: 'Quick actions'),
    (Key: 'app.dashboard.title'; Value: 'Vehicle dashboard'),
    (Key: 'app.navigation.about'; Value: 'About'),
    (Key: 'app.navigation.dashboard'; Value: 'Dashboard'),
    (Key: 'app.navigation.dtc'; Value: 'Diagnostic trouble codes'),
    (Key: 'app.navigation.freezeFrame'; Value: 'Freeze frame'),
    (Key: 'app.navigation.liveData'; Value: 'Live data'),
    (Key: 'app.navigation.monitorTests'; Value: 'Monitor tests'),
    (Key: 'app.navigation.oxygenSensors'; Value: 'Oxygen sensors'),
    (Key: 'app.navigation.performance'; Value: 'Performance'),
    (Key: 'app.navigation.readiness'; Value: 'Readiness'),
    (Key: 'app.navigation.settings'; Value: 'Settings'),
    (Key: 'app.navigation.vehicleInfo'; Value: 'Vehicle info'),
    (Key: 'app.settings.adapter'; Value: 'Adapter'),
    (Key: 'app.settings.connection'; Value: 'Connection'),
    (Key: 'app.settings.language'; Value: 'Language'),
    (Key: 'app.settings.privacy'; Value: 'Privacy'),
    (Key: 'app.settings.region'; Value: 'Region'),
    (Key: 'app.settings.saveRestart'; Value: 'Save and restart diagnostics'),
    (Key: 'app.settings.theme'; Value: 'Theme'),
    (Key: 'app.settings.title'; Value: 'Settings'),
    (Key: 'app.settings.units'; Value: 'Units'),
    (Key: 'common.apply'; Value: 'Apply'),
    (Key: 'common.back'; Value: 'Back'),
    (Key: 'common.browse'; Value: 'Browse'),
    (Key: 'common.cancel'; Value: 'Cancel'),
    (Key: 'common.clearCodes'; Value: 'Clear codes'),
    (Key: 'common.close'; Value: 'Close'),
    (Key: 'common.connect'; Value: 'Connect'),
    (Key: 'common.disconnect'; Value: 'Disconnect'),
    (Key: 'common.exportCsv'; Value: 'Export CSV'),
    (Key: 'common.finish'; Value: 'Finish'),
    (Key: 'common.next'; Value: 'Next'),
    (Key: 'common.readCodes'; Value: 'Read codes'),
    (Key: 'common.refresh'; Value: 'Refresh'),
    (Key: 'common.reset'; Value: 'Reset'),
    (Key: 'common.save'; Value: 'Save'),
    (Key: 'common.search'; Value: 'Search'),
    (Key: 'service.dtc.defaultDescription'; Value: 'Diagnostic trouble code'),
    (Key: 'service.readiness.catalyst'; Value: 'Catalyst monitor'),
    (Key: 'service.readiness.components'; Value: 'Comprehensive components monitor'),
    (Key: 'service.readiness.fuelSystem'; Value: 'Fuel system monitor'),
    (Key: 'service.readiness.misfire'; Value: 'Misfire monitor'),
    (Key: 'status.busy'; Value: 'Busy'),
    (Key: 'status.connected'; Value: 'Connected'),
    (Key: 'status.connecting'; Value: 'Connecting'),
    (Key: 'status.disconnected'; Value: 'Disconnected'),
    (Key: 'status.error'; Value: 'Error'),
    (Key: 'status.milOff'; Value: 'MIL off'),
    (Key: 'status.milOn'; Value: 'MIL on'),
    (Key: 'status.noData'; Value: 'No data'),
    (Key: 'status.ready'; Value: 'Ready'),
    (Key: 'status.vehicleNotReady'; Value: 'Vehicle not ready'),
    (Key: 'status.vehicleReady'; Value: 'Vehicle ready'),
    (Key: 'ui.color.addCustomColours'; Value: 'Add to custom colours'),
    (Key: 'ui.connection.segment.adapter'; Value: 'Adapter'),
    (Key: 'ui.connection.segment.battery'; Value: 'Battery'),
    (Key: 'ui.connection.segment.protocol'; Value: 'Protocol'),
    (Key: 'ui.connection.segment.vin'; Value: 'VIN'),
    (Key: 'ui.connection.state.connected'; Value: 'Connected'),
    (Key: 'ui.connection.state.connecting'; Value: 'Connecting'),
    (Key: 'ui.connection.state.disconnected'; Value: 'Disconnected'),
    (Key: 'ui.connection.state.error'; Value: 'Error'),
    (Key: 'ui.dtc.cancel'; Value: 'Cancel'),
    (Key: 'ui.dtc.clear'; Value: 'Clear codes...'),
    (Key: 'ui.dtc.clear.noteFrames'; Value: 'Clearing also erases the freeze frames and resets all readiness monitors.'),
    (Key: 'ui.dtc.clear.notePermanent'; Value: 'Permanent codes stay until the ECU has seen the fault cleared on its own.'),
    (Key: 'ui.dtc.clear.noteReadiness'; Value: 'The car will show "not ready" for the emissions test until a full drive cycle is done.'),
    (Key: 'ui.dtc.clear.question'; Value: 'Clear %d diagnostic trouble codes?'),
    (Key: 'ui.dtc.clearConfirm'; Value: 'Clear codes'),
    (Key: 'ui.dtc.column.code'; Value: 'Code'),
    (Key: 'ui.dtc.column.controlUnit'; Value: 'Control unit'),
    (Key: 'ui.dtc.column.description'; Value: 'Description'),
    (Key: 'ui.dtc.column.status'; Value: 'Status'),
    (Key: 'ui.dtc.column.system'; Value: 'System'),
    (Key: 'ui.dtc.defaultDescription'; Value: 'Diagnostic trouble code'),
    (Key: 'ui.dtc.empty.subtitle'; Value: 'Read codes to refresh the vehicle status.'),
    (Key: 'ui.dtc.empty.title'; Value: 'No trouble codes stored'),
    (Key: 'ui.dtc.filter.all'; Value: 'All %d'),
    (Key: 'ui.dtc.filter.pending'; Value: 'Pending %d'),
    (Key: 'ui.dtc.filter.permanent'; Value: 'Permanent %d'),
    (Key: 'ui.dtc.filter.stored'; Value: 'Stored %d'),
    (Key: 'ui.dtc.footer.lastReadAt'; Value: 'Last read %s'),
    (Key: 'ui.dtc.footer.lastReadNever'; Value: 'Last read never'),
    (Key: 'ui.dtc.footer.summary'; Value: '%d codes - %d stored, %d pending, %d permanent'),
    (Key: 'ui.dtc.freeze.empty'; Value: 'No freeze-frame values are linked to this code.'),
    (Key: 'ui.dtc.freeze.open'; Value: 'Open freeze frame'),
    (Key: 'ui.dtc.freeze.title'; Value: 'Freeze frame - when %s was stored'),
    (Key: 'ui.dtc.header.pendingCount'; Value: '%d PENDING'),
    (Key: 'ui.dtc.header.permanentCount'; Value: '%d PERMANENT'),
    (Key: 'ui.dtc.header.storedCount'; Value: '%d STORED'),
    (Key: 'ui.dtc.precheckIgnition'; Value: 'Ignition on, engine off'),
    (Key: 'ui.dtc.precheckReport'; Value: 'Codes saved to the job report'),
    (Key: 'ui.dtc.read'; Value: 'Read codes'),
    (Key: 'ui.dtc.status.pending'; Value: 'PENDING'),
    (Key: 'ui.dtc.status.permanent'; Value: 'PERMANENT'),
    (Key: 'ui.dtc.status.stored'; Value: 'STORED'),
    (Key: 'ui.dtc.system.body'; Value: 'Body'),
    (Key: 'ui.dtc.system.chassis'; Value: 'Chassis'),
    (Key: 'ui.dtc.system.network'; Value: 'Network'),
    (Key: 'ui.dtc.system.powertrain'; Value: 'Powertrain'),
    (Key: 'ui.dtc.title'; Value: 'Diagnostic trouble codes'),
    (Key: 'ui.file.allFiles'; Value: 'All files (*.*)'),
    (Key: 'ui.file.changeName'; Value: 'Change name'),
    (Key: 'ui.file.newFolder'; Value: 'New folder'),
    (Key: 'ui.file.replace'; Value: 'Replace'),
    (Key: 'ui.file.search'; Value: 'Search'),
    (Key: 'ui.find.direction.down'; Value: 'Down'),
    (Key: 'ui.find.direction.up'; Value: 'Up'),
    (Key: 'ui.find.matchCase'; Value: 'Match case'),
    (Key: 'ui.find.noMatches'; Value: 'No matches'),
    (Key: 'ui.find.oneMatch'; Value: '1 match'),
    (Key: 'ui.find.wholeWord'; Value: 'Whole word only'),
    (Key: 'ui.font.noFonts'; Value: 'No fonts'),
    (Key: 'ui.font.searchFonts'; Value: 'Search fonts'),
    (Key: 'ui.font.strikeout'; Value: 'Strikeout'),
    (Key: 'ui.font.underline'; Value: 'Underline'),
    (Key: 'ui.inspector.boolean.off'; Value: 'Off'),
    (Key: 'ui.inspector.boolean.on'; Value: 'On'),
    (Key: 'ui.range.applyEngineCodes'; Value: 'Apply to engine codes'),
    (Key: 'ui.range.cancel'; Value: 'Cancel'),
    (Key: 'ui.range.fixInvalidRows'; Value: 'Fix invalid rows before saving'),
    (Key: 'ui.range.noProfile'; Value: '(no profile)'),
    (Key: 'ui.range.reset'; Value: 'Reset'),
    (Key: 'ui.range.resetAll'; Value: 'Reset all'),
    (Key: 'ui.range.saveProfile'; Value: 'Save profile'),
    (Key: 'ui.readiness.action.open'; Value: 'Open readiness'),
    (Key: 'ui.readiness.banner.allComplete'; Value: 'All %d supported monitors complete'),
    (Key: 'ui.readiness.banner.incomplete'; Value: '%d of %d supported monitors incomplete - drive cycle needed'),
    (Key: 'ui.readiness.banner.milOff'; Value: 'MIL off'),
    (Key: 'ui.readiness.banner.milOn'; Value: 'MIL on'),
    (Key: 'ui.readiness.banner.notReady'; Value: 'Not ready for the %s'),
    (Key: 'ui.readiness.banner.ready'; Value: 'Ready for the %s'),
    (Key: 'ui.readiness.group.continuous'; Value: 'Continuous'),
    (Key: 'ui.readiness.group.nonContinuous'; Value: 'Non-continuous'),
    (Key: 'ui.readiness.inspection.apk'; Value: 'APK'),
    (Key: 'ui.readiness.inspection.controleTechnique'; Value: 'technical inspection'),
    (Key: 'ui.readiness.inspection.emissionsTest'; Value: 'emissions test'),
    (Key: 'ui.readiness.inspection.huAu'; Value: 'HU / AU'),
    (Key: 'ui.readiness.inspection.keuring'; Value: 'vehicle inspection'),
    (Key: 'ui.readiness.inspection.mot'; Value: 'MOT'),
    (Key: 'ui.readiness.inspection.nct'; Value: 'NCT'),
    (Key: 'ui.readiness.mil.off'; Value: 'MIL OFF'),
    (Key: 'ui.readiness.mil.on'; Value: 'MIL ON'),
    (Key: 'ui.readiness.monitor.acRefrigerant'; Value: 'A/C refrigerant'),
    (Key: 'ui.readiness.monitor.boostPressure'; Value: 'Boost pressure'),
    (Key: 'ui.readiness.monitor.catalyst'; Value: 'Catalyst'),
    (Key: 'ui.readiness.monitor.components'; Value: 'Comprehensive components'),
    (Key: 'ui.readiness.monitor.egrSystem'; Value: 'EGR system'),
    (Key: 'ui.readiness.monitor.egrVvtSystem'; Value: 'EGR / VVT system'),
    (Key: 'ui.readiness.monitor.evaporativeSystem'; Value: 'Evaporative system'),
    (Key: 'ui.readiness.monitor.exhaustGasSensor'; Value: 'Exhaust gas sensor'),
    (Key: 'ui.readiness.monitor.fuelSystem'; Value: 'Fuel system'),
    (Key: 'ui.readiness.monitor.heatedCatalyst'; Value: 'Heated catalyst'),
    (Key: 'ui.readiness.monitor.misfire'; Value: 'Misfire'),
    (Key: 'ui.readiness.monitor.nmhcCatalyst'; Value: 'NMHC catalyst'),
    (Key: 'ui.readiness.monitor.noxScrAftertreatment'; Value: 'NOx / SCR aftertreatment'),
    (Key: 'ui.readiness.monitor.oxygenSensor'; Value: 'Oxygen sensor'),
    (Key: 'ui.readiness.monitor.oxygenSensorHeater'; Value: 'Oxygen sensor heater'),
    (Key: 'ui.readiness.monitor.pmFilter'; Value: 'PM filter'),
    (Key: 'ui.readiness.monitor.secondaryAirSystem'; Value: 'Secondary air system'),
    (Key: 'ui.readiness.note.acConditions'; Value: 'Needs A/C monitor conditions'),
    (Key: 'ui.readiness.note.boostCycle'; Value: 'Needs a boost-pressure drive cycle'),
    (Key: 'ui.readiness.note.closedLoopMotorway'; Value: 'Needs closed-loop motorway driving'),
    (Key: 'ui.readiness.note.coldStart'; Value: 'Needs a cold start test'),
    (Key: 'ui.readiness.note.dpfRegen'; Value: 'Needs a completed DPF regeneration'),
    (Key: 'ui.readiness.note.egrOperation'; Value: 'Needs mixed-speed EGR operation'),
    (Key: 'ui.readiness.note.evapDriveCycle'; Value: 'Needs a cold soak and EVAP drive cycle'),
    (Key: 'ui.readiness.note.exhaustWarmup'; Value: 'Needs exhaust sensor warm-up'),
    (Key: 'ui.readiness.note.noxMotorway'; Value: 'Needs ~20 min of motorway driving'),
    (Key: 'ui.readiness.note.steadyCruise'; Value: 'Needs a steady cruise after warm-up'),
    (Key: 'ui.readiness.sinceClear.distance'; Value: 'Since clear: %d km'),
    (Key: 'ui.readiness.sinceClear.distanceAndWarmups'; Value: 'Since clear: %d km - %d warm-ups'),
    (Key: 'ui.readiness.sinceClear.prefix'; Value: 'Since clear: '),
    (Key: 'ui.readiness.sinceClear.preview'; Value: 'Since clear: 42 km - 3 warm-ups'),
    (Key: 'ui.readiness.sinceClear.warmups'; Value: 'Since clear: %d warm-ups'),
    (Key: 'ui.readiness.source.compression'; Value: 'Compression ignition - Mode 01 PID 01'),
    (Key: 'ui.readiness.source.spark'; Value: 'Spark ignition - Mode 01 PID 01'),
    (Key: 'ui.readiness.state.complete'; Value: 'Complete'),
    (Key: 'ui.readiness.state.incomplete'; Value: 'Incomplete'),
    (Key: 'ui.readiness.state.unsupported'; Value: 'Not supported'),
    (Key: 'ui.readiness.title.compact'; Value: 'Readiness'),
    (Key: 'ui.readiness.title.full'; Value: 'Readiness monitors'),
    (Key: 'ui.readiness.updated.preview'; Value: 'Updated 18:42:09'),
    (Key: 'ui.ribbon.capture'; Value: 'Capture'),
    (Key: 'ui.ribbon.codes'; Value: 'Codes'),
    (Key: 'ui.ribbon.connection'; Value: 'Connection'),
    (Key: 'ui.ribbon.copySummary'; Value: 'Copy summary'),
    (Key: 'ui.ribbon.diagnose'; Value: 'Diagnose'),
    (Key: 'ui.ribbon.filter'; Value: 'Filter'),
    (Key: 'ui.ribbon.home'; Value: 'Home'),
    (Key: 'ui.ribbon.liveData'; Value: 'Live data'),
    (Key: 'ui.ribbon.pendingOnly'; Value: 'Pending only'),
    (Key: 'ui.ribbon.play'; Value: 'Play'),
    (Key: 'ui.ribbon.playback'; Value: 'Playback'),
    (Key: 'ui.ribbon.print'; Value: 'Print...'),
    (Key: 'ui.ribbon.record'; Value: 'Record'),
    (Key: 'ui.ribbon.recordingOpen'; Value: 'Recording open'),
    (Key: 'ui.ribbon.report'; Value: 'Report'),
    (Key: 'ui.ribbon.searchCommands'; Value: 'Search commands'),
    (Key: 'ui.ribbon.searchCommandsShortcut'; Value: 'Search commands Alt+Q'),
    (Key: 'ui.ribbon.snapshot'; Value: 'Snapshot'),
    (Key: 'ui.ribbon.stop'; Value: 'Stop'),
    (Key: 'ui.ribbon.vehicle'; Value: 'Vehicle'),
    (Key: 'ui.status.preview.adapterPort'; Value: 'ELM327 v2.2 - COM4'),
    (Key: 'ui.status.preview.codesMilOn'; Value: '%d codes - MIL on'),
    (Key: 'ui.status.preview.connected'; Value: 'Connected'),
    (Key: 'ui.status.preview.protocol'; Value: 'CAN 11/500'),
    (Key: 'ui.status.preview.readingModule'; Value: 'Reading %s'),
    (Key: 'ui.taskDialog.hideDetails'; Value: 'Hide details'),
    (Key: 'ui.taskDialog.progress'; Value: 'Progress'),
    (Key: 'ui.taskDialog.showDetails'; Value: 'Show details'));

  BUILTIN_ENGLISH_PLURALS: array [0 .. 1] of TOBDBuiltInPlural = (
    (Key: 'ui.dtc.codeCount.plural'; OneText: '%d code'; OtherText: '%d codes'),
    (Key: 'ui.find.matches.plural'; OneText: '%d match'; OtherText: '%d matches'));

var
  GTranslator: TOBDTranslator = nil;
  GTranslatorLock: TCriticalSection = nil;

function NormaliseLanguageCode(const ACode: string): string;
var
  S: string;
  P: Integer;
begin
  S := LowerCase(Trim(ACode));
  S := StringReplace(S, '_', '-', [rfReplaceAll]);
  P := Pos('.', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  P := Pos('@', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  if S = '' then
    S := 'en';
  Result := S;
end;

function BaseLanguage(const ACode: string): string;
var
  P: Integer;
begin
  Result := NormaliseLanguageCode(ACode);
  P := Pos('-', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
end;

function SameListener(A, B: TNotifyEvent): Boolean;
begin
  Result := (TMethod(A).Code = TMethod(B).Code) and
    (TMethod(A).Data = TMethod(B).Data);
end;

constructor TOBDTranslator.TLanguageCatalog.Create;
begin
  inherited Create;
  FStrings := TDictionary<string, string>.Create;
  FPlurals := TDictionary<string, TOBDPluralText>.Create;
  Version := 1;
end;

destructor TOBDTranslator.TLanguageCatalog.Destroy;
begin
  FPlurals.Free;
  FStrings.Free;
  inherited;
end;

procedure TOBDTranslator.TLanguageCatalog.AddString(const AKey, AValue: string);
begin
  FStrings.AddOrSetValue(AKey, AValue);
end;

procedure TOBDTranslator.TLanguageCatalog.AddPlural(const AKey, AOne,
  AOther: string);
var
  Item: TOBDPluralText;
begin
  Item.OneText := AOne;
  Item.OtherText := AOther;
  FPlurals.AddOrSetValue(AKey, Item);
end;

function TOBDTranslator.TLanguageCatalog.TryString(const AKey: string;
  out AValue: string): Boolean;
begin
  Result := FStrings.TryGetValue(AKey, AValue);
end;

function TOBDTranslator.TLanguageCatalog.TryPlural(const AKey: string;
  out AValue: TOBDPluralText): Boolean;
begin
  Result := FPlurals.TryGetValue(AKey, AValue);
end;

constructor TOBDTranslator.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCatalogs := TObjectDictionary<string, TLanguageCatalog>.Create([doOwnsValues]);
  FCurrentLanguage := 'en';
  LoadBuiltInEnglish;
end;

destructor TOBDTranslator.Destroy;
begin
  FCatalogs.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDTranslator.LoadBuiltInEnglish;
var
  Catalog: TLanguageCatalog;
  I: Integer;
begin
  Catalog := TLanguageCatalog.Create;
  try
    Catalog.Language := 'en';
    Catalog.Name := 'English';
    Catalog.EnglishName := 'English';
    for I := Low(BUILTIN_ENGLISH_STRINGS) to High(BUILTIN_ENGLISH_STRINGS) do
      Catalog.AddString(BUILTIN_ENGLISH_STRINGS[I].Key,
        BUILTIN_ENGLISH_STRINGS[I].Value);
    for I := Low(BUILTIN_ENGLISH_PLURALS) to High(BUILTIN_ENGLISH_PLURALS) do
      Catalog.AddPlural(BUILTIN_ENGLISH_PLURALS[I].Key,
        BUILTIN_ENGLISH_PLURALS[I].OneText,
        BUILTIN_ENGLISH_PLURALS[I].OtherText);
    LoadCatalog(Catalog);
    Catalog := nil;
  finally
    Catalog.Free;
  end;
end;

function TOBDTranslator.GetCurrentLanguage: string;
begin
  FLock.Enter;
  try
    Result := FCurrentLanguage;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDTranslator.SetCurrentLanguage(const AValue: string);
var
  NewCode: string;
begin
  NewCode := NormaliseLanguageCode(AValue);
  FLock.Enter;
  try
    if FCurrentLanguage = NewCode then
      Exit;
    FCurrentLanguage := NewCode;
  finally
    FLock.Leave;
  end;
  NotifyChanged;
end;

procedure TOBDTranslator.LoadCatalog(ACatalog: TLanguageCatalog);
var
  Code: string;
begin
  Code := NormaliseLanguageCode(ACatalog.Language);
  ACatalog.Language := Code;
  FLock.Enter;
  try
    FCatalogs.AddOrSetValue(Code, ACatalog);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDTranslator.LoadLanguageFile(const AFileName: string);
begin
  if not TFile.Exists(AFileName) then
    raise EOBDConfig.Create('Language file not found: ' + AFileName);
  LoadLanguageFromString(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

procedure TOBDTranslator.LoadLanguageFromString(const AText: string);
var
  Root, StringsObj, PluralObj: TJSONObject;
  Pair: TJSONPair;
  Catalog: TLanguageCatalog;
  CurrentCode: string;
  Changed: Boolean;
begin
  Root := ParseOBDJSONObject(AText);
  Catalog := TLanguageCatalog.Create;
  try
    Catalog.Language := Root.GetValue<string>('language', 'en');
    Catalog.Name := Root.GetValue<string>('name', Catalog.Language);
    Catalog.EnglishName := Root.GetValue<string>('englishName', Catalog.Name);
    Catalog.Version := Root.GetValue<Integer>('version', 1);
    StringsObj := Root.GetValue<TJSONObject>('strings', nil);
    if StringsObj = nil then
      raise EOBDConfig.Create('Language JSON: strings object missing');
    for Pair in StringsObj do
    begin
      if Pair.JsonValue is TJSONString then
        Catalog.AddString(Pair.JsonString.Value, TJSONString(Pair.JsonValue).Value)
      else if Pair.JsonValue is TJSONObject then
      begin
        PluralObj := TJSONObject(Pair.JsonValue);
        Catalog.AddPlural(Pair.JsonString.Value,
          PluralObj.GetValue<string>('one', ''),
          PluralObj.GetValue<string>('other', ''));
      end
      else
        raise EOBDConfig.Create('Language JSON: string or plural object expected for ' +
          Pair.JsonString.Value);
    end;
    CurrentCode := CurrentLanguage;
    Changed := (NormaliseLanguageCode(Catalog.Language) = CurrentCode) or
      (BaseLanguage(CurrentCode) = NormaliseLanguageCode(Catalog.Language));
    LoadCatalog(Catalog);
    Catalog := nil;
  finally
    Catalog.Free;
    Root.Free;
  end;
  if Changed then
    NotifyChanged;
end;

procedure TOBDTranslator.LoadFromResource(const AResourceName: string);
{$IFDEF MSWINDOWS}
var
  Stream: TResourceStream;
  Bytes: TBytes;
begin
  Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Bytes, Stream.Size);
    if Length(Bytes) > 0 then
      Stream.ReadBuffer(Bytes[0], Length(Bytes));
    LoadLanguageFromString(TEncoding.UTF8.GetString(Bytes));
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  raise EOBDConfig.Create('Resource language loading is only available on Windows');
end;
{$ENDIF}

function TOBDTranslator.AvailableLanguages(const AFolder: string)
  : TArray<TOBDLanguageInfo>;
var
  Files: TArray<string>;
  FileName: string;
  Root: TJSONObject;
  Info: TOBDLanguageInfo;
  Acc: TList<TOBDLanguageInfo>;
begin
  Acc := TList<TOBDLanguageInfo>.Create;
  try
    if TDirectory.Exists(AFolder) then
    begin
      Files := TDirectory.GetFiles(AFolder, '*.json');
      for FileName in Files do
      begin
        Root := ParseOBDJSONObject(TFile.ReadAllText(FileName, TEncoding.UTF8));
        try
          Info.Code := NormaliseLanguageCode(Root.GetValue<string>('language', ''));
          Info.Name := Root.GetValue<string>('name', Info.Code);
          Info.EnglishName := Root.GetValue<string>('englishName', Info.Name);
          Info.FileName := FileName;
          Acc.Add(Info);
        finally
          Root.Free;
        end;
      end;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDTranslator.FindStringLocked(const ALanguage, AKey: string;
  out AValue: string): Boolean;
var
  Catalog: TLanguageCatalog;
begin
  Result := FCatalogs.TryGetValue(NormaliseLanguageCode(ALanguage), Catalog) and
    Catalog.TryString(AKey, AValue);
end;

function TOBDTranslator.FindPluralLocked(const ALanguage, AKey: string;
  out AValue: TOBDPluralText): Boolean;
var
  Catalog: TLanguageCatalog;
begin
  Result := FCatalogs.TryGetValue(NormaliseLanguageCode(ALanguage), Catalog) and
    Catalog.TryPlural(AKey, AValue);
end;

function TOBDTranslator.Tr(const AKey, ADefault: string): string;
var
  Code, Base: string;
begin
  FLock.Enter;
  try
    Code := FCurrentLanguage;
    if FindStringLocked(Code, AKey, Result) then
      Exit;
    Base := BaseLanguage(Code);
    if (Base <> Code) and FindStringLocked(Base, AKey, Result) then
      Exit;
    if FindStringLocked('en', AKey, Result) then
      Exit;
    Result := ADefault;
  finally
    FLock.Leave;
  end;
end;

function TOBDTranslator.TrFmt(const AKey, ADefault: string;
  const AArgs: array of const): string;
begin
  Result := Format(Tr(AKey, ADefault), AArgs);
end;

function TOBDTranslator.TrPlural(const AKey: string; ACount: Int64;
  const AOneDefault, AOtherDefault: string): string;
var
  Code, Base, PluralKey: string;
  Text: TOBDPluralText;
begin
  PluralKey := AKey;
  if not SameText(Copy(PluralKey, Length(PluralKey) - 6, 7), '.plural') then
    PluralKey := AKey + '.plural';
  FLock.Enter;
  try
    Code := FCurrentLanguage;
    if FindPluralLocked(Code, PluralKey, Text) then
    begin
      if ACount = 1 then
        Exit(Text.OneText);
      Exit(Text.OtherText);
    end;
    Base := BaseLanguage(Code);
    if (Base <> Code) and FindPluralLocked(Base, PluralKey, Text) then
    begin
      if ACount = 1 then
        Exit(Text.OneText);
      Exit(Text.OtherText);
    end;
    if FindPluralLocked('en', PluralKey, Text) then
    begin
      if ACount = 1 then
        Exit(Text.OneText);
      Exit(Text.OtherText);
    end;
    if ACount = 1 then
      Result := AOneDefault
    else
      Result := AOtherDefault;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDTranslator.AddChangeListener(AListener: TNotifyEvent);
var
  I: Integer;
begin
  if not Assigned(AListener) then
    Exit;
  FLock.Enter;
  try
    for I := 0 to High(FListeners) do
      if SameListener(FListeners[I], AListener) then
        Exit;
    SetLength(FListeners, Length(FListeners) + 1);
    FListeners[High(FListeners)] := AListener;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDTranslator.RemoveChangeListener(AListener: TNotifyEvent);
var
  I, J: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to High(FListeners) do
      if SameListener(FListeners[I], AListener) then
      begin
        for J := I to High(FListeners) - 1 do
          FListeners[J] := FListeners[J + 1];
        SetLength(FListeners, Length(FListeners) - 1);
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDTranslator.NotifyChanged;
var
  Snapshot: TArray<TNotifyEvent>;
  I: Integer;
begin
  FLock.Enter;
  try
    Snapshot := Copy(FListeners);
  finally
    FLock.Leave;
  end;
  for I := 0 to High(Snapshot) do
    if Assigned(Snapshot[I]) then
      Snapshot[I](Self);
end;

function OBDI18n: TOBDTranslator;
begin
  if GTranslatorLock = nil then
    GTranslatorLock := TCriticalSection.Create;
  GTranslatorLock.Enter;
  try
    if GTranslator = nil then
      GTranslator := TOBDTranslator.Create;
    Result := GTranslator;
  finally
    GTranslatorLock.Leave;
  end;
end;

function Tr(const AKey, ADefault: string): string;
begin
  Result := OBDI18n.Tr(AKey, ADefault);
end;

function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
begin
  Result := OBDI18n.TrFmt(AKey, ADefault, AArgs);
end;

function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
  AOtherDefault: string): string;
begin
  Result := OBDI18n.TrPlural(AKey, ACount, AOneDefault, AOtherDefault);
end;

function OBDDefaultLanguageCode: string;
{$IFDEF MSWINDOWS}
var
  Lang: Word;
begin
  Lang := GetUserDefaultUILanguage and $03FF;
  case Lang of
    $13:
      Result := 'nl';
    $07:
      Result := 'de';
    $0C:
      Result := 'fr';
  else
    Result := 'en';
  end;
end;
{$ELSE}
var
  Env: string;
begin
  Env := GetEnvironmentVariable('LC_ALL');
  if Env = '' then
    Env := GetEnvironmentVariable('LC_MESSAGES');
  if Env = '' then
    Env := GetEnvironmentVariable('LANG');
  Env := BaseLanguage(Env);
  if (Env = 'nl') or (Env = 'de') or (Env = 'fr') then
    Result := Env
  else
    Result := 'en';
end;
{$ENDIF}

initialization
  GTranslatorLock := TCriticalSection.Create;

finalization
  FreeAndNil(GTranslator);
  FreeAndNil(GTranslatorLock);

end.
