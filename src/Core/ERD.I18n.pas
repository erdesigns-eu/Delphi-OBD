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
// 2026-10-12  ERD  Built-in English texts for TOBDHistogram.
// 2026-10-12  ERD  CLDR plural categories, all European languages,
//                  catalogue status.
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
    /// <summary>Review status from the catalogue: <c>draft</c> (not yet
    /// reviewed by a native speaker), <c>reviewed</c>, or empty.</summary>
    Status: string;
  end;

  /// <summary>CLDR cardinal plural category.</summary>
  TOBDPluralCategory = (pcZero, pcOne, pcTwo, pcFew, pcMany, pcOther);

  /// <summary>Plural strings from an i18n catalogue, one per CLDR
  /// category. Empty categories fall back to <c>OtherText</c>.</summary>
  TOBDPluralText = record
    /// <summary>Text used when the count is one.</summary>
    OneText: string;
    /// <summary>Text used for every other count.</summary>
    OtherText: string;
    /// <summary>CLDR "zero" form (Latvian).</summary>
    ZeroText: string;
    /// <summary>CLDR "two" form (Slovenian, Irish, Maltese).</summary>
    TwoText: string;
    /// <summary>CLDR "few" form (Slavic, Baltic, Romanian, Irish,
    /// Maltese).</summary>
    FewText: string;
    /// <summary>CLDR "many" form (Polish, Russian, Ukrainian,
    /// Belarusian, Irish, Maltese).</summary>
    ManyText: string;
  end;

  /// <summary>
  /// Process-wide, thread-safe translation catalogue with English built-ins.
  /// </summary>
  TOBDTranslator = class
  strict private
    type
      TLanguageCatalog = class
      strict private
        FLanguage: string;
        FName: string;
        FEnglishName: string;
        FVersion: Integer;
        FStrings: TDictionary<string, string>;
        FPlurals: TDictionary<string, TOBDPluralText>;
      public
        constructor Create;
        destructor Destroy; override;
        procedure AddString(const AKey, AValue: string);
        procedure AddPlural(const AKey: string; const AText: TOBDPluralText);
        function TryString(const AKey: string; out AValue: string): Boolean;
        function TryPlural(const AKey: string; out AValue: TOBDPluralText): Boolean;
        property Language: string read FLanguage write FLanguage;
        property Name: string read FName write FName;
        property EnglishName: string read FEnglishName write FEnglishName;
        property Version: Integer read FVersion write FVersion;
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
    /// <summary>Looks up a plural translation and picks the CLDR plural
    /// form of the language that supplied it.</summary>
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
/// <summary>Maps a Windows LANGID to a supported language code, or
/// <c>en</c>.</summary>
function OBDLanguageCodeFromLangId(ALangId: Word): string;
/// <summary>Returns <c>True</c> for a code in
/// <see cref="OBD_LANGUAGE_CODES"/>.</summary>
function OBDIsSupportedLanguage(const ACode: string): Boolean;
/// <summary>CLDR cardinal plural category of a whole number in a
/// language (the base language of <c>ALanguage</c> is used).</summary>
function OBDPluralCategory(const ALanguage: string; ACount: Int64): TOBDPluralCategory;

const
  /// <summary>Languages shipped in <c>languages/</c>: the official
  /// national languages of the European countries.</summary>
  OBD_LANGUAGE_CODES: array [0 .. 37] of string = (
    'be', 'bg', 'bs', 'ca', 'cs', 'da', 'de', 'el', 'en', 'es', 'et', 'fi',
    'fr', 'ga', 'hr', 'hu', 'is', 'it', 'lb', 'lt', 'lv', 'mk', 'mt', 'nb',
    'nl', 'nn', 'pl', 'pt', 'rm', 'ro', 'ru', 'sk', 'sl', 'sq', 'sr', 'sv',
    'tr', 'uk');

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
  BUILTIN_ENGLISH_STRINGS: array [0 .. 218] of TOBDBuiltInString = (
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
    (Key: 'ui.histogram.marker.mean'; Value: 'mean'),
    (Key: 'ui.histogram.marker.median'; Value: 'median'),
    (Key: 'ui.histogram.marker.now'; Value: 'now'),
    (Key: 'ui.histogram.noSamples'; Value: 'No samples'),
    (Key: 'ui.histogram.summary'; Value: '%d samples | mean %s | min %s | max %s'),
    (Key: 'ui.histogram.title'; Value: 'Histogram'),
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

function IsOneOf(const AValue: string; const AList: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(AList) to High(AList) do
    if AValue = AList[I] then
      Exit(True);
  Result := False;
end;

function PluralTextFor(const AText: TOBDPluralText;
  ACategory: TOBDPluralCategory): string;
begin
  case ACategory of
    pcZero:
      Result := AText.ZeroText;
    pcOne:
      Result := AText.OneText;
    pcTwo:
      Result := AText.TwoText;
    pcFew:
      Result := AText.FewText;
    pcMany:
      Result := AText.ManyText;
  else
    Result := '';
  end;
  if Result = '' then
    Result := AText.OtherText;
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

procedure TOBDTranslator.TLanguageCatalog.AddPlural(const AKey: string;
  const AText: TOBDPluralText);
begin
  FPlurals.AddOrSetValue(AKey, AText);
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
  Plural: TOBDPluralText;
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
    begin
      Plural := Default(TOBDPluralText);
      Plural.OneText := BUILTIN_ENGLISH_PLURALS[I].OneText;
      Plural.OtherText := BUILTIN_ENGLISH_PLURALS[I].OtherText;
      Catalog.AddPlural(BUILTIN_ENGLISH_PLURALS[I].Key, Plural);
    end;
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
  Plural: TOBDPluralText;
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
        Plural.ZeroText := PluralObj.GetValue<string>('zero', '');
        Plural.OneText := PluralObj.GetValue<string>('one', '');
        Plural.TwoText := PluralObj.GetValue<string>('two', '');
        Plural.FewText := PluralObj.GetValue<string>('few', '');
        Plural.ManyText := PluralObj.GetValue<string>('many', '');
        Plural.OtherText := PluralObj.GetValue<string>('other', '');
        Catalog.AddPlural(Pair.JsonString.Value, Plural);
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
          Info.Status := Root.GetValue<string>('status', '');
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
      Exit(PluralTextFor(Text, OBDPluralCategory(Code, ACount)));
    Base := BaseLanguage(Code);
    if (Base <> Code) and FindPluralLocked(Base, PluralKey, Text) then
      Exit(PluralTextFor(Text, OBDPluralCategory(Base, ACount)));
    if FindPluralLocked('en', PluralKey, Text) then
      Exit(PluralTextFor(Text, OBDPluralCategory('en', ACount)));
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

function OBDIsSupportedLanguage(const ACode: string): Boolean;
begin
  Result := IsOneOf(NormaliseLanguageCode(ACode), OBD_LANGUAGE_CODES);
end;

function OBDLanguageCodeFromLangId(ALangId: Word): string;
var
  Primary, Sub: Word;
begin
  Primary := ALangId and $03FF;
  Sub := ALangId shr 10;
  case Primary of
    $02: Result := 'bg';
    $03: Result := 'ca';
    $05: Result := 'cs';
    $06: Result := 'da';
    $07: Result := 'de';
    $08: Result := 'el';
    $0A: Result := 'es';
    $0B: Result := 'fi';
    $0C: Result := 'fr';
    $0E: Result := 'hu';
    $0F: Result := 'is';
    $10: Result := 'it';
    $13: Result := 'nl';
    $14:
      // SUBLANG_NORWEGIAN_NYNORSK = 2, otherwise Bokmal.
      if Sub = $02 then
        Result := 'nn'
      else
        Result := 'nb';
    $15: Result := 'pl';
    $16: Result := 'pt';
    $17: Result := 'rm';
    $18: Result := 'ro';
    $19: Result := 'ru';
    $1A:
      // Croatian (1, 4), Bosnian (5, 8), Serbian and Montenegrin (rest).
      case Sub of
        $01, $04: Result := 'hr';
        $05, $08: Result := 'bs';
      else
        Result := 'sr';
      end;
    $1B: Result := 'sk';
    $1C: Result := 'sq';
    $1D: Result := 'sv';
    $1F: Result := 'tr';
    $22: Result := 'uk';
    $23: Result := 'be';
    $24: Result := 'sl';
    $25: Result := 'et';
    $26: Result := 'lv';
    $27: Result := 'lt';
    $2F: Result := 'mk';
    $3A: Result := 'mt';
    $3C: Result := 'ga';
    $6E: Result := 'lb';
  else
    Result := 'en';
  end;
end;

function OBDPluralCategory(const ALanguage: string; ACount: Int64): TOBDPluralCategory;
var
  Lang: string;
  N: Int64;
  I10, I100: Integer;
begin
  Lang := BaseLanguage(ALanguage);
  N := Abs(ACount);
  I10 := Integer(N mod 10);
  I100 := Integer(N mod 100);
  Result := pcOther;
  if Lang = 'fr' then
  begin
    if N <= 1 then
      Result := pcOne;
  end
  else if IsOneOf(Lang, ['is', 'mk']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne;
  end
  else if Lang = 'lv' then
  begin
    if (I10 = 0) or ((I100 >= 11) and (I100 <= 19)) then
      Result := pcZero
    else if I10 = 1 then
      Result := pcOne;
  end
  else if Lang = 'lt' then
  begin
    if (I100 >= 11) and (I100 <= 19) then
      Result := pcOther
    else if I10 = 1 then
      Result := pcOne
    else if I10 >= 2 then
      Result := pcFew;
  end
  else if Lang = 'pl' then
  begin
    if N = 1 then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew
    else
      Result := pcMany;
  end
  else if IsOneOf(Lang, ['ru', 'uk', 'be']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew
    else
      Result := pcMany;
  end
  else if IsOneOf(Lang, ['cs', 'sk']) then
  begin
    if N = 1 then
      Result := pcOne
    else if (N >= 2) and (N <= 4) then
      Result := pcFew;
  end
  else if IsOneOf(Lang, ['hr', 'sr', 'bs']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew;
  end
  else if Lang = 'sl' then
  begin
    if I100 = 1 then
      Result := pcOne
    else if I100 = 2 then
      Result := pcTwo
    else if (I100 = 3) or (I100 = 4) then
      Result := pcFew;
  end
  else if Lang = 'ro' then
  begin
    if N = 1 then
      Result := pcOne
    else if (N = 0) or ((I100 >= 1) and (I100 <= 19)) then
      Result := pcFew;
  end
  else if Lang = 'ga' then
  begin
    if N = 1 then
      Result := pcOne
    else if N = 2 then
      Result := pcTwo
    else if (N >= 3) and (N <= 6) then
      Result := pcFew
    else if (N >= 7) and (N <= 10) then
      Result := pcMany;
  end
  else if Lang = 'mt' then
  begin
    if N = 1 then
      Result := pcOne
    else if N = 2 then
      Result := pcTwo
    else if (N = 0) or ((I100 >= 3) and (I100 <= 10)) then
      Result := pcFew
    else if (I100 >= 11) and (I100 <= 19) then
      Result := pcMany;
  end
  else if N = 1 then
    Result := pcOne;
end;

function OBDDefaultLanguageCode: string;
{$IFDEF MSWINDOWS}
begin
  Result := OBDLanguageCodeFromLangId(GetUserDefaultUILanguage);
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
  if Env = 'no' then
    Env := 'nb'
  else if Env = 'sh' then
    Env := 'sr';
  if OBDIsSupportedLanguage(Env) then
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
