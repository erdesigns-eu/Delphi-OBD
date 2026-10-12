//------------------------------------------------------------------------------
//  untSettings
//
//  The user settings of OBD-Studio Personal.
//
//  Local first: everything lives in one JSON file under
//  %APPDATA%\ERDesigns\OBD-Studio Personal\settings.json, and recordings,
//  reports and the vehicle garage in the user's Documents folder. Nothing
//  is sent anywhere. A missing or damaged file gives the defaults; the
//  file is written atomically (temporary file, then rename).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untSettings;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  ERD.UI.Types;

const
  /// <summary>Program version shown in the about box and the installer.</summary>
  PERSONAL_VERSION = '0.1.0';

  /// <summary>Version of EULA.md the user has to accept. Bump it when the
  /// licence text changes so the welcome wizard asks again.</summary>
  PERSONAL_EULA_VERSION = '1.0';

  PERSONAL_COMPANY_FOLDER = 'ERDesigns';
  PERSONAL_APP_FOLDER     = 'OBD-Studio Personal';

type
  /// <summary>How OBD-Studio Personal reaches the car.</summary>
  TPersonalConnectionKind = (
    /// <summary>USB or a paired Bluetooth adapter on a COM port.</summary>
    pckSerial,
    /// <summary>Wi-Fi adapter (TCP, usually 192.168.0.10:35000).</summary>
    pckWiFi,
    /// <summary>Bluetooth Low Energy adapter.</summary>
    pckBLE,
    /// <summary>The built-in demo car (no adapter needed).</summary>
    pckDemo);

  /// <summary>The user settings, loaded at start and saved on change.</summary>
  TPersonalSettings = class
  strict private
    FFileName: string;
    FLanguage: string;
    FThemeMode: TOBDThemeMode;
    FDensity: TOBDDensity;
    FUnitSystem: TOBDUnitSystem;
    FInspectionCountry: string;
    FConnectionKind: TPersonalConnectionKind;
    FSerialPort: string;
    FBaudRate: Integer;
    FWiFiHost: string;
    FWiFiPort: Integer;
    FBLEAddress: string;
    FAutoConnect: Boolean;
    FAcceptedEula: string;
    FWelcomeDone: Boolean;
    FLastPage: string;
    FFuelPrice: Double;
    FCurrency: string;
    FDataFolder: string;
    FWindowLeft: Integer;
    FWindowTop: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FWindowMaximized: Boolean;
    procedure SetDefaults;
    function GetDataFolder: string;
  public
    constructor Create;
    /// <summary>Folder of settings.json (created on save).</summary>
    class function SettingsFolder: string; static;
    /// <summary>Default folder for recordings, reports and the garage.</summary>
    class function DefaultDataFolder: string; static;
    /// <summary>Reads settings.json; missing or invalid values keep their
    /// defaults.</summary>
    procedure Load;
    /// <summary>Writes settings.json.</summary>
    procedure Save;
    /// <summary>True when the current EULA version has been accepted.</summary>
    function EulaAccepted: Boolean;
    /// <summary>Folder for one kind of user data (e.g. 'Recordings',
    /// 'Reports', 'Garage'), created when missing.</summary>
    function DataSubFolder(const AName: string): string;

    property FileName: string read FFileName;
    /// <summary>ISO 639-1 code; empty means the Windows display language.</summary>
    property Language: string read FLanguage write FLanguage;
    property ThemeMode: TOBDThemeMode read FThemeMode write FThemeMode;
    property Density: TOBDDensity read FDensity write FDensity;
    property UnitSystem: TOBDUnitSystem read FUnitSystem write FUnitSystem;
    /// <summary>ISO 3166 country of the periodic inspection (APK, TUV, CT,
    /// MOT ...), used by the readiness page.</summary>
    property InspectionCountry: string read FInspectionCountry
      write FInspectionCountry;
    property ConnectionKind: TPersonalConnectionKind read FConnectionKind
      write FConnectionKind;
    property SerialPort: string read FSerialPort write FSerialPort;
    property BaudRate: Integer read FBaudRate write FBaudRate;
    property WiFiHost: string read FWiFiHost write FWiFiHost;
    property WiFiPort: Integer read FWiFiPort write FWiFiPort;
    property BLEAddress: string read FBLEAddress write FBLEAddress;
    /// <summary>Connect with the last adapter when the program starts.</summary>
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect;
    /// <summary>EULA version the user accepted ('' = none).</summary>
    property AcceptedEula: string read FAcceptedEula write FAcceptedEula;
    property WelcomeDone: Boolean read FWelcomeDone write FWelcomeDone;
    property LastPage: string read FLastPage write FLastPage;
    /// <summary>Fuel price per litre (or per gallon in imperial units).</summary>
    property FuelPrice: Double read FFuelPrice write FFuelPrice;
    property Currency: string read FCurrency write FCurrency;
    /// <summary>Root of the user data; empty means DefaultDataFolder.</summary>
    property DataFolder: string read GetDataFolder write FDataFolder;
    property WindowLeft: Integer read FWindowLeft write FWindowLeft;
    property WindowTop: Integer read FWindowTop write FWindowTop;
    property WindowWidth: Integer read FWindowWidth write FWindowWidth;
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;
    property WindowMaximized: Boolean read FWindowMaximized
      write FWindowMaximized;
  end;

function ConnectionKindToText(AKind: TPersonalConnectionKind): string;
function TextToConnectionKind(const AText: string): TPersonalConnectionKind;

implementation

uses
  ERD.JSON;

const
  CONNECTION_KIND_TEXT: array[TPersonalConnectionKind] of string = (
    'serial', 'wifi', 'ble', 'demo');
  THEME_MODE_TEXT: array[TOBDThemeMode] of string = (
    'auto', 'light', 'dark', 'windows');

function ConnectionKindToText(AKind: TPersonalConnectionKind): string;
begin
  Result := CONNECTION_KIND_TEXT[AKind];
end;

function TextToConnectionKind(const AText: string): TPersonalConnectionKind;
var
  Kind: TPersonalConnectionKind;
begin
  for Kind := Low(TPersonalConnectionKind) to High(TPersonalConnectionKind) do
    if SameText(AText, CONNECTION_KIND_TEXT[Kind]) then
      Exit(Kind);
  Result := pckDemo;
end;

function TextToThemeMode(const AText: string): TOBDThemeMode;
var
  Mode: TOBDThemeMode;
begin
  for Mode := Low(TOBDThemeMode) to High(TOBDThemeMode) do
    if SameText(AText, THEME_MODE_TEXT[Mode]) then
      Exit(Mode);
  Result := tmWindows;
end;

{ TPersonalSettings }

constructor TPersonalSettings.Create;
begin
  inherited Create;
  FFileName := TPath.Combine(SettingsFolder, 'settings.json');
  SetDefaults;
end;

class function TPersonalSettings.SettingsFolder: string;
begin
  Result := TPath.Combine(TPath.Combine(TPath.GetHomePath,
    PERSONAL_COMPANY_FOLDER), PERSONAL_APP_FOLDER);
end;

class function TPersonalSettings.DefaultDataFolder: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, PERSONAL_APP_FOLDER);
end;

procedure TPersonalSettings.SetDefaults;
begin
  FLanguage := '';
  FThemeMode := tmWindows;
  FDensity := dnDesktop;
  FUnitSystem := usMetric;
  FInspectionCountry := 'NL';
  FConnectionKind := pckDemo;
  FSerialPort := '';
  FBaudRate := 38400;
  FWiFiHost := '192.168.0.10';
  FWiFiPort := 35000;
  FBLEAddress := '';
  FAutoConnect := False;
  FAcceptedEula := '';
  FWelcomeDone := False;
  FLastPage := 'overview';
  FFuelPrice := 1.95;
  FCurrency := 'EUR';
  FDataFolder := '';
  FWindowLeft := -1;
  FWindowTop := -1;
  FWindowWidth := 1280;
  FWindowHeight := 800;
  FWindowMaximized := False;
end;

function TPersonalSettings.GetDataFolder: string;
begin
  if FDataFolder <> '' then
    Result := FDataFolder
  else
    Result := DefaultDataFolder;
end;

function TPersonalSettings.DataSubFolder(const AName: string): string;
begin
  Result := TPath.Combine(DataFolder, AName);
  ForceDirectories(Result);
end;

function TPersonalSettings.EulaAccepted: Boolean;
begin
  Result := FAcceptedEula = PERSONAL_EULA_VERSION;
end;

procedure TPersonalSettings.Load;
var
  Root: TJSONObject;
begin
  SetDefaults;
  if not TFile.Exists(FFileName) then
    Exit;
  try
    Root := ParseOBDJSONObject(TFile.ReadAllText(FFileName, TEncoding.UTF8));
  except
    // A damaged file must not stop the program; keep the defaults and
    // overwrite the file on the next save.
    Exit;
  end;
  try
    FLanguage := Root.GetValue<string>('language', FLanguage);
    FThemeMode := TextToThemeMode(Root.GetValue<string>('theme',
      THEME_MODE_TEXT[FThemeMode]));
    if SameText(Root.GetValue<string>('density', 'desktop'), 'tablet') then
      FDensity := dnTablet;
    if SameText(Root.GetValue<string>('units', 'metric'), 'imperial') then
      FUnitSystem := usImperial;
    FInspectionCountry := Root.GetValue<string>('inspectionCountry',
      FInspectionCountry);
    FConnectionKind := TextToConnectionKind(Root.GetValue<string>(
      'connection', CONNECTION_KIND_TEXT[FConnectionKind]));
    FSerialPort := Root.GetValue<string>('serialPort', FSerialPort);
    FBaudRate := Root.GetValue<Integer>('baudRate', FBaudRate);
    FWiFiHost := Root.GetValue<string>('wifiHost', FWiFiHost);
    FWiFiPort := Root.GetValue<Integer>('wifiPort', FWiFiPort);
    FBLEAddress := Root.GetValue<string>('bleAddress', FBLEAddress);
    FAutoConnect := Root.GetValue<Boolean>('autoConnect', FAutoConnect);
    FAcceptedEula := Root.GetValue<string>('acceptedEula', FAcceptedEula);
    FWelcomeDone := Root.GetValue<Boolean>('welcomeDone', FWelcomeDone);
    FLastPage := Root.GetValue<string>('lastPage', FLastPage);
    FFuelPrice := Root.GetValue<Double>('fuelPrice', FFuelPrice);
    FCurrency := Root.GetValue<string>('currency', FCurrency);
    FDataFolder := Root.GetValue<string>('dataFolder', FDataFolder);
    FWindowLeft := Root.GetValue<Integer>('windowLeft', FWindowLeft);
    FWindowTop := Root.GetValue<Integer>('windowTop', FWindowTop);
    FWindowWidth := Root.GetValue<Integer>('windowWidth', FWindowWidth);
    FWindowHeight := Root.GetValue<Integer>('windowHeight', FWindowHeight);
    FWindowMaximized := Root.GetValue<Boolean>('windowMaximized',
      FWindowMaximized);
  finally
    Root.Free;
  end;
end;

procedure TPersonalSettings.Save;
const
  DENSITY_TEXT: array[TOBDDensity] of string = ('desktop', 'tablet');
  UNITS_TEXT: array[TOBDUnitSystem] of string = ('metric', 'imperial');
var
  Root: TJSONObject;
  TempName: string;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('language', FLanguage);
    Root.AddPair('theme', THEME_MODE_TEXT[FThemeMode]);
    Root.AddPair('density', DENSITY_TEXT[FDensity]);
    Root.AddPair('units', UNITS_TEXT[FUnitSystem]);
    Root.AddPair('inspectionCountry', FInspectionCountry);
    Root.AddPair('connection', CONNECTION_KIND_TEXT[FConnectionKind]);
    Root.AddPair('serialPort', FSerialPort);
    Root.AddPair('baudRate', TJSONNumber.Create(FBaudRate));
    Root.AddPair('wifiHost', FWiFiHost);
    Root.AddPair('wifiPort', TJSONNumber.Create(FWiFiPort));
    Root.AddPair('bleAddress', FBLEAddress);
    Root.AddPair('autoConnect', TJSONBool.Create(FAutoConnect));
    Root.AddPair('acceptedEula', FAcceptedEula);
    Root.AddPair('welcomeDone', TJSONBool.Create(FWelcomeDone));
    Root.AddPair('lastPage', FLastPage);
    Root.AddPair('fuelPrice', TJSONNumber.Create(FFuelPrice));
    Root.AddPair('currency', FCurrency);
    Root.AddPair('dataFolder', FDataFolder);
    Root.AddPair('windowLeft', TJSONNumber.Create(FWindowLeft));
    Root.AddPair('windowTop', TJSONNumber.Create(FWindowTop));
    Root.AddPair('windowWidth', TJSONNumber.Create(FWindowWidth));
    Root.AddPair('windowHeight', TJSONNumber.Create(FWindowHeight));
    Root.AddPair('windowMaximized', TJSONBool.Create(FWindowMaximized));
    ForceDirectories(ExtractFilePath(FFileName));
    TempName := FFileName + '.tmp';
    TFile.WriteAllText(TempName, Root.Format(2), TEncoding.UTF8);
    if TFile.Exists(FFileName) then
      TFile.Delete(FFileName);
    TFile.Move(TempName, FFileName);
  finally
    Root.Free;
  end;
end;

end.
