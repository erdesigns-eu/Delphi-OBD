//------------------------------------------------------------------------------
// UNIT           : OBD.Application.Settings.pas
// CONTENTS       : OBD Application Settings
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 04/04/2024
//------------------------------------------------------------------------------
unit OBD.Application.Settings;

interface

uses
  WinApi.Windows, System.SysUtils, System.Win.Registry, Classes;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Application settings root key
  /// </summary>
  TOBDApplicationSettingsRootKey = (rkClassesRoot, rkCurrentUser, rkLocalMachine, rkUsers, rkPerformanceData, rkCurrentConfig, rkDynData);
  /// <summary>
  ///   Root key change event
  /// </summary>
  TOBDApplicationSettingsRootKeyEvent = procedure(Sender: TObject; RootKey: TOBDApplicationSettingsRootKey) of object;
  /// <summary>
  ///   Root path change event
  /// </summary>
  TOBDApplicationSettingsRootPathEvent = procedure(Sender: TObject; RootPath: string) of object;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Application Command Line Parameter Item
  /// </summary>
  TOBDApplicationCommandLineParameter = class(TCollectionItem)
  private
    /// <summary>
    ///   Name
    /// </summary>
    FName: string;
    /// <summary>
    ///   Value
    /// </summary>
    FValue: Variant;
  protected
    /// <summary>
    ///   Override GetDisplayName function
    /// </summary>
    function GetDisplayName: string; override;
  public
    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Name
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    ///   Value
    /// </summary>
    property Value: Variant read FValue write FValue;
  end;

  /// <summary>
  ///   OBD Application Command Line Parameter Collection
  /// </summary>
  TOBDApplicationCommandLineParameters = class(TOwnedCollection)
  private
    /// <summary>
    ///   Get Item
    /// </summary>
    function GetItem(Index: Integer): TOBDApplicationCommandLineParameter;
    /// <summary>
    ///   Set Item
    /// </summary>
    procedure SetItem(Index: Integer; Value: TOBDApplicationCommandLineParameter);
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TPersistent); virtual;

    /// <summary>
    ///   Add item
    /// </summary>
    function Add: TOBDApplicationCommandLineParameter;
    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Command Line Parameter Items
    /// </summary>
    property Items[Index: Integer]: TOBDApplicationCommandLineParameter read GetItem write SetItem; default;
  end;

  /// <summary>
  ///   OBD Application Settings
  /// </summary>
  TOBDApplicationSettings = class(TPersistent)
  private
    /// <summary>
    ///   Registry object
    /// </summary>
    FRegistry: TRegistry;

    /// <summary>
    ///   Registry root key (Defaults to CURRENT_USER)
    /// </summary>
    FRootKey: TOBDApplicationSettingsRootKey;
    /// <summary>
    ///   Registry root path (Defaults to \SOFTWARE\ERDesigns\OBD\)
    /// </summary>
    FRootPath: string;
    /// <summary>
    ///   Command Line parameters
    /// </summary>
    FCommandLineParameters: TOBDApplicationCommandLineParameters;

    /// <summary>
    ///   Registry root key change event
    /// </summary>
    FOnRootKeyChange: TOBDApplicationSettingsRootKeyEvent;
    /// <summary>
    ///   Registry root path change event
    /// </summary>
    FOnRootPathChange: TOBDApplicationSettingsRootPathEvent;

    /// <summary>
    ///   Set registry root key
    /// </summary>
    procedure SetRootKey(const Value: TOBDApplicationSettingsRootKey);
    /// <summary>
    ///   Set registry root path
    /// </summary>
    procedure SetRootPath(const Value: string);
    /// <summary>
    ///   Set command line parameters
    /// </summary>
    procedure SetCommandLineParameters(const Value: TOBDApplicationCommandLineParameters);
  protected
    /// <summary>
    ///   Parse command line parameters
    /// </summary>
    procedure ParseCommandLineParameters;
    /// <summary>
    ///   Get relative path
    /// </summary>
    function GetRelativePath(const Value: string): string;
    /// <summary>
    ///   Get index of command line parameter
    /// </summary>
    function IndexOfCommandLineParameter(const Name: string): Integer;
    /// <summary>
    ///   Get command line parameter value
    /// </summary>
    function GetCommandLineParameterValue(Name: string): Variant;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Get a string setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: string): string; overload;
    /// <summary>
    ///   Get a Integer setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: Integer): Integer; overload;
    /// <summary>
    ///   Get a Float setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: Double): Double; overload;
    /// <summary>
    ///   Get a DateTime setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: TDateTime): TDateTime; overload;
    /// <summary>
    ///   Get a Date setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: TDate): TDate; overload;
    /// <summary>
    ///   Get a Time setting
    /// </summary>
    function GetSetting(const Path: string; const Name: string; Default: TTime): TTime; overload;

    /// <summary>
    ///   Set a string setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: string); overload;
    /// <summary>
    ///   Set a Integer setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: Integer); overload;
    /// <summary>
    ///   Set a Float setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: Double); overload;
    /// <summary>
    ///   Set a DateTime setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: TDateTime); overload;
    /// <summary>
    ///   Set a Date setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: TDate); overload;
    /// <summary>
    ///   Set a Time setting
    /// </summary>
    procedure SetSetting(const Path: string; const Name: String; Value: TTime); overload;

    /// <summary>
    ///   Registry object instance
    /// </summary>
    property Reg: TRegistry read FRegistry;
    /// <summary>
    ///   Get the value of a command line parameter
    /// </summary>
    property CommandLineParameter[Name: string]: Variant read GetCommandLineParameterValue;
    /// <summary>
    ///   Command Line parameters
    /// </summary>
    property CommandLineParameters: TOBDApplicationCommandLineParameters read FCommandLineParameters write SetCommandLineParameters;
  published
    /// <summary>
    ///   Registry root key
    /// </summary>
    property RootKey: TOBDApplicationSettingsRootKey read FRootKey write SetRootKey default rkCurrentUser;
    /// <summary>
    ///   Registry root path
    /// </summary>
    property RootPath: string read FRootPath write SetRootPath;

    /// <summary>
    ///   Registry root key change event
    /// </summary>
    property OnRootKeyChange: TOBDApplicationSettingsRootKeyEvent read FOnRootKeyChange write FOnRootKeyChange;
    /// <summary>
    ///   Registry root path change event
    /// </summary>
    property OnRootPathChange: TOBDApplicationSettingsRootPathEvent read FOnRootPathChange write FOnRootPathChange;
  public
    /// <summary>
    ///   Get the singleton instance
    /// </summary>
    class function Instance: TOBDApplicationSettings;
  end;

implementation

//------------------------------------------------------------------------------
// VARIABLES
//------------------------------------------------------------------------------
var
  /// <summary>
  ///   Singleton instance
  /// </summary>
  _ApplicationSettingsInstance: TOBDApplicationSettings = nil;

//------------------------------------------------------------------------------
// CONSTANTES
//------------------------------------------------------------------------------
const ROOT_KEY: array[rkClassesRoot..rkDynData] of HKEY = (
  HKEY_CLASSES_ROOT,
  HKEY_CURRENT_USER,
  HKEY_LOCAL_MACHINE,
  HKEY_USERS,
  HKEY_PERFORMANCE_DATA,
  HKEY_CURRENT_CONFIG,
  HKEY_DYN_DATA
);

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDApplicationCommandLineParameter.GetDisplayName: string;
begin
  if not FName.IsEmpty then Result := FName else Result := inherited;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDApplicationCommandLineParameter.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDApplicationCommandLineParameter) then
  begin
    FName  := (Source as TOBDApplicationCommandLineParameter).Name;
    FValue := (Source as TOBDApplicationCommandLineParameter).Value;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET ITEM
//------------------------------------------------------------------------------
function TOBDApplicationCommandLineParameters.GetItem(Index: Integer): TOBDApplicationCommandLineParameter;
begin
  Result := inherited GetItem(Index) as TOBDApplicationCommandLineParameter;
end;

//------------------------------------------------------------------------------
// SET ITEM
//------------------------------------------------------------------------------
procedure TOBDApplicationCommandLineParameters.SetItem(Index: Integer; Value: TOBDApplicationCommandLineParameter);
begin
  inherited SetItem(Index, Value);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDApplicationCommandLineParameters.Create(AOwner: TPersistent);
begin
  // Call inherited constructor
  inherited Create(AOwner, TOBDApplicationCommandLineParameter);
end;

//------------------------------------------------------------------------------
// ADD ITEM
//------------------------------------------------------------------------------
function TOBDApplicationCommandLineParameters.Add: TOBDApplicationCommandLineParameter;
begin
  Result := TOBDApplicationCommandLineParameter(inherited Add);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDApplicationCommandLineParameters.Assign(Source: TPersistent);
var
  I: Integer;
begin
  // Assign properties
  if (Source is TOBDApplicationCommandLineParameters) then
  begin
    // Clear the collection
    Clear;
    // Loop over the items, and add them to our collection
    for I := 0 to (Source as TOBDApplicationCommandLineParameters).Count -1 do
    begin
      Add.Assign((Source as TOBDApplicationCommandLineParameters).Items[I]);
    end;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET ROOT KEY
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetRootKey(const Value: TOBDApplicationSettingsRootKey);
begin
  if (FRootKey <> Value) then
  begin
    // Set new root key
    FRootKey := Value;
    // Update the registry root key
    Reg.RootKey := ROOT_KEY[FRootKey];
    // Notify the root key changed
    if Assigned(OnRootKeyChange) then OnRootKeyChange(Self, FRootKey);
  end;
end;

//------------------------------------------------------------------------------
// SET ROOT PATH
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetRootPath(const Value: string);
begin
  if (FRootPath <> Value) then
  begin
    // Set new root path
    FRootPath := Value;
    // Notify the root path changed
    if Assigned(OnRootPathChange) then OnRootPathChange(Self, FRootPath);
  end;
end;

//------------------------------------------------------------------------------
// SET COMMAND LINE PARAMETERS
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetCommandLineParameters(const Value: TOBDApplicationCommandLineParameters);
begin
  FCommandLineParameters.Assign(Value);
end;

//------------------------------------------------------------------------------
// PARSE COMMAND LINE PARAMETERS
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.ParseCommandLineParameters;
var
  I, Index: Integer;
  Param, ParamName: string;
  ParamValue: Variant;
begin
  for I := 1 to ParamCount do
  begin
    // Command line parameter
    if ParamStr(I).StartsWith('/') or ParamStr(I).StartsWith('-') then
    begin
      // Remove leading '/' or '-'
      Param := ParamStr(I).Substring(1);

      // Check if there is a value associated with the parameter and extract it
      if Pos('=', Param) > 0 then
      begin
        ParamName  := Copy(Param, 1, Pos('=', Param) - 1);
        ParamValue := Copy(Param, Pos('=', Param) + 1, Length(Param));
      end else
      begin
        ParamName  := Param;
        ParamValue := True;
      end;

      // Update parameter value or add parameter.
      Index := IndexOfCommandLineParameter(ParamName);
      if (Index > -1) then
        FCommandLineParameters.Items[Index].Value := ParamValue
      else
      with FCommandLineParameters.Add do
      begin
        Name  := ParamName;
        Value := ParamValue;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// GET RELATIVE PATH
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetRelativePath(const Value: string): string;
var
  RootPath, RelativePath: string;
begin
  // Set root path and replace / with \ if there are any
  RootPath := StringReplace(FRootPath, '/', '\', [rfReplaceAll, rfIgnoreCase]);
  // Add a \ if its not already there
  if not RootPath.EndsWith('\') then RootPath := RootPath + '\';

  // If the value is empty, just return the root path
  if Value.IsEmpty then Exit(RootPath);

  // Replace / with \ if there are any
  RelativePath := StringReplace(Value, '/', '\', [rfReplaceAll, rfIgnoreCase]);
  // Remove the first \ if there is any
  if RelativePath.StartsWith('\') then RelativePath := Copy(RelativePath, 2, Length(RelativePath) -1);

  // Return the relative path
  Result := RootPath + RelativePath;
end;

//------------------------------------------------------------------------------
// GET INDEX OF COMMAND LINE PARAMETER
//------------------------------------------------------------------------------
function TOBDApplicationSettings.IndexOfCommandLineParameter(const Name: string): Integer;
var
  I: Integer;
begin
  // Initialize result
  Result := -1;
  // Find index of parameter with name
  for I := 0 to FCommandLineParameters.Count -1 do
  if CompareText(FCommandLineParameters.Items[I].Name, Name) = 0 then
  begin
    Result := I;
    Break;
  end;
end;

//------------------------------------------------------------------------------
// GET COMMAND LINE PARAMETER VALUE
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetCommandLineParameterValue(Name: string): Variant;
var
  ParameterIndex: Integer;
begin
  // Initialize result
  Result := false;
  // Find the index of the parameter
  ParameterIndex := IndexOfCommandLineParameter(Name);
  // If the index is found, return the value
  if ParameterIndex > -1 then Result := FCommandLineParameters[ParameterIndex].Value;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDApplicationSettings.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create registry object
  FRegistry := TRegistry.Create;
  // Set default root key
  FRootKey := rkCurrentUser;
  // Set default root path
  FRootPath := '\SOFTWARE\ERDesigns\OBD\';
  // Create command line parameter collection
  FCommandLineParameters := TOBDApplicationCommandLineParameters.Create(Self);
  // Parse command line parameters
  ParseCommandLineParameters;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDApplicationSettings.Destroy;
begin
  // Free registry object
  FRegistry.Free;
  // Free command line parameter collection
  FCommandLineParameters.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDApplicationSettings) then
  begin
    FRootKey := (Source as TOBDApplicationSettings).RootKey;
    FRootPath := (Source as TOBDApplicationSettings).RootPath;
    FCommandLineParameters.Assign((Source as TOBDApplicationSettings).CommandLineParameters);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET STRING SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: string): string;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the string
    if Reg.ValueExists(Name) then Result := Reg.ReadString(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET INTEGER SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: Integer): Integer;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the Integer
    if Reg.ValueExists(Name) then Result := Reg.ReadInteger(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET FLOAT SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: Double): Double;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the Float
    if Reg.ValueExists(Name) then Result := Reg.ReadFloat(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET DATETIME SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: TDateTime): TDateTime;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the DateTime
    if Reg.ValueExists(Name) then Result := Reg.ReadDateTime(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET DATE SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: TDate): TDate;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the Date
    if Reg.ValueExists(Name) then Result := Reg.ReadDate(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET TIME SETTING
//------------------------------------------------------------------------------
function TOBDApplicationSettings.GetSetting(const Path: string; const Name: string; Default: TTime): TTime;
begin
  // Initialize result with default value
  Result := Default;
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Check if the value exists, and if it exists read the Time
    if Reg.ValueExists(Name) then Result := Reg.ReadTime(Name);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET STRING SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: string);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new string setting
    Reg.WriteString(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET INTEGER SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: Integer);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new Integer setting
    Reg.WriteInteger(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET FLOAT SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: Double);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new Float setting
    Reg.WriteFloat(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET DATETIME SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: TDateTime);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new DateTime setting
    Reg.WriteDateTime(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET DATE SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: TDate);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new Date setting
    Reg.WriteDate(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// SET TIME SETTING
//------------------------------------------------------------------------------
procedure TOBDApplicationSettings.SetSetting(const Path: string; const Name: string; Value: TTime);
begin
  // Open key
  if Reg.OpenKey(GetRelativePath(Path), False) then
  begin
    // Write the new Time setting
    Reg.WriteTime(Name, Value);
    // Close the key
    Reg.CloseKey;
  end;
end;

//------------------------------------------------------------------------------
// GET SINGLETON INSTANCE
//------------------------------------------------------------------------------
class function TOBDApplicationSettings.Instance: TOBDApplicationSettings;
begin
  // Return the singleton instance
  Result := _ApplicationSettingsInstance;
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization
  // Create the singleton instance
  _ApplicationSettingsInstance := TOBDApplicationSettings.Create;

//------------------------------------------------------------------------------
// FINALIZATION
//------------------------------------------------------------------------------
finalization
  // Free the singleton instance
  FreeAndNil(_ApplicationSettingsInstance);

end.
