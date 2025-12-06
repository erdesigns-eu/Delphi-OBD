//------------------------------------------------------------------------------
// UNIT           : OBD.Service05.pas
// CONTENTS       : OBD Service 05 (Test results, oxygen sensor monitoring - non CAN only)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 19/03/2024
//------------------------------------------------------------------------------
unit OBD.Service05;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  OBD.Request.Constants, OBD.Request.Encoders, OBD.Response.Decoders, OBD.Service.Types,
  OBD.Service;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service 05
  /// </summary>
  TOBDService05 = class(TOBDService)
  private
    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    FSupportedPID: TBytes;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 1
    /// </summary>
    FRichToLeanBank1Sensor1: Double;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 2
    /// </summary>
    FRichToLeanBank1Sensor2: Double;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 3
    /// </summary>
    FRichToLeanBank1Sensor3: Double;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 4
    /// </summary>
    FRichToLeanBank1Sensor4: Double;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 1
    /// </summary>
    FRichToLeanBank2Sensor1: Double;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 2
    /// </summary>
    FRichToLeanBank2Sensor2: Double;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 3
    /// </summary>
    FRichToLeanBank2Sensor3: Double;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 4
    /// </summary>
    FRichToLeanBank2Sensor4: Double;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 1
    /// </summary>
    FRichToLeanBank3Sensor1: Double;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 2
    /// </summary>
    FRichToLeanBank3Sensor2: Double;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 3
    /// </summary>
    FRichToLeanBank3Sensor3: Double;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 4
    /// </summary>
    FRichToLeanBank3Sensor4: Double;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 1
    /// </summary>
    FRichToLeanBank4Sensor1: Double;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 2
    /// </summary>
    FRichToLeanBank4Sensor2: Double;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 3
    /// </summary>
    FRichToLeanBank4Sensor3: Double;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 4
    /// </summary>
    FRichToLeanBank4Sensor4: Double;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 1
    /// </summary>
    FLeanToRichBank1Sensor1: Double;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 2
    /// </summary>
    FLeanToRichBank1Sensor2: Double;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 3
    /// </summary>
    FLeanToRichBank1Sensor3: Double;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 4
    /// </summary>
    FLeanToRichBank1Sensor4: Double;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 1
    /// </summary>
    FLeanToRichBank2Sensor1: Double;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 2
    /// </summary>
    FLeanToRichBank2Sensor2: Double;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 3
    /// </summary>
    FLeanToRichBank2Sensor3: Double;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 4
    /// </summary>
    FLeanToRichBank2Sensor4: Double;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 1
    /// </summary>
    FLeanToRichBank3Sensor1: Double;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 2
    /// </summary>
    FLeanToRichBank3Sensor2: Double;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 3
    /// </summary>
    FLeanToRichBank3Sensor3: Double;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 4
    /// </summary>
    FLeanToRichBank3Sensor4: Double;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 1
    /// </summary>
    FLeanToRichBank4Sensor1: Double;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 2
    /// </summary>
    FLeanToRichBank4Sensor2: Double;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 3
    /// </summary>
    FLeanToRichBank4Sensor3: Double;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 4
    /// </summary>
    FLeanToRichBank4Sensor4: Double;

    /// <summary>
    ///   Data changed event
    /// </summary>
    FOnData: TOBDServiceDataEvent;
  protected
    /// <summary>
    ///   Get the service id
    /// </summary>
    function GetServiceID: Byte; override;
    /// <summary>
    ///   Parse response with supported PID's
    /// </summary>
    procedure ParseSupportedPID(PID: Byte; Data: TBytes);
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
    ///   Reset (clear all data)
    /// </summary>
    procedure Reset;
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(const Response: TBytes); override;
    /// <summary>
    ///   Is given PID supported?
    /// </summary>
    function IsPIDSupported(PID: Byte): Boolean;

    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    property SupportedPID: TBytes read FSupportedPID;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 1
    /// </summary>
    property RichToLeanBank1Sensor1: Double read FRichToLeanBank1Sensor1;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 2
    /// </summary>
    property RichToLeanBank1Sensor2: Double read FRichToLeanBank1Sensor2;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 3
    /// </summary>
    property RichToLeanBank1Sensor3: Double read FRichToLeanBank1Sensor3;
    /// <summary>
    ///   Rich to lean Bank 1 Sensor 4
    /// </summary>
    property RichToLeanBank1Sensor4: Double read FRichToLeanBank1Sensor4;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 1
    /// </summary>
    property RichToLeanBank2Sensor1: Double read FRichToLeanBank2Sensor1;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 2
    /// </summary>
    property RichToLeanBank2Sensor2: Double read FRichToLeanBank2Sensor2;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 3
    /// </summary>
    property RichToLeanBank2Sensor3: Double read FRichToLeanBank2Sensor3;
    /// <summary>
    ///   Rich to lean Bank 2 Sensor 4
    /// </summary>
    property RichToLeanBank2Sensor4: Double read FRichToLeanBank2Sensor4;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 1
    /// </summary>
    property RichToLeanBank3Sensor1: Double read FRichToLeanBank3Sensor1;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 2
    /// </summary>
    property RichToLeanBank3Sensor2: Double read FRichToLeanBank3Sensor2;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 3
    /// </summary>
    property RichToLeanBank3Sensor3: Double read FRichToLeanBank3Sensor3;
    /// <summary>
    ///   Rich to lean Bank 3 Sensor 4
    /// </summary>
    property RichToLeanBank3Sensor4: Double read FRichToLeanBank3Sensor4;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 1
    /// </summary>
    property RichToLeanBank4Sensor1: Double read FRichToLeanBank4Sensor1;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 2
    /// </summary>
    property RichToLeanBank4Sensor2: Double read FRichToLeanBank4Sensor2;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 3
    /// </summary>
    property RichToLeanBank4Sensor3: Double read FRichToLeanBank4Sensor3;
    /// <summary>
    ///   Rich to lean Bank 4 Sensor 4
    /// </summary>
    property RichToLeanBank4Sensor4: Double read FRichToLeanBank4Sensor4;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 1
    /// </summary>
    property LeanToRichBank1Sensor1: Double read FLeanToRichBank1Sensor1;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 2
    /// </summary>
    property LeanToRichBank1Sensor2: Double read FLeanToRichBank1Sensor2;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 3
    /// </summary>
    property LeanToRichBank1Sensor3: Double read FLeanToRichBank1Sensor3;
    /// <summary>
    ///   Lean to rich Bank 1 Sensor 4
    /// </summary>
    property LeanToRichBank1Sensor4: Double read FLeanToRichBank1Sensor4;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 1
    /// </summary>
    property LeanToRichBank2Sensor1: Double read FLeanToRichBank2Sensor1;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 2
    /// </summary>
    property LeanToRichBank2Sensor2: Double read FLeanToRichBank2Sensor2;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 3
    /// </summary>
    property LeanToRichBank2Sensor3: Double read FLeanToRichBank2Sensor3;
    /// <summary>
    ///   Lean to rich Bank 2 Sensor 4
    /// </summary>
    property LeanToRichBank2Sensor4: Double read FLeanToRichBank2Sensor4;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 1
    /// </summary>
    property LeanToRichBank3Sensor1: Double read FLeanToRichBank3Sensor1;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 2
    /// </summary>
    property LeanToRichBank3Sensor2: Double read FLeanToRichBank3Sensor2;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 3
    /// </summary>
    property LeanToRichBank3Sensor3: Double read FLeanToRichBank3Sensor3;
    /// <summary>
    ///   Lean to rich Bank 3 Sensor 4
    /// </summary>
    property LeanToRichBank3Sensor4: Double read FLeanToRichBank3Sensor4;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 1
    /// </summary>
    property LeanToRichBank4Sensor1: Double read FLeanToRichBank4Sensor1;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 2
    /// </summary>
    property LeanToRichBank4Sensor2: Double read FLeanToRichBank4Sensor2;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 3
    /// </summary>
    property LeanToRichBank4Sensor3: Double read FLeanToRichBank4Sensor3;
    /// <summary>
    ///   Lean to rich Bank 4 Sensor 4
    /// </summary>
    property LeanToRichBank4Sensor4: Double read FLeanToRichBank4Sensor4;

    /// <summary>
    ///   Sensor data changed event
    /// </summary>
    property OnData: TOBDServiceDataEvent read FOnData write FOnData;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 05: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService05.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_05;
end;

//------------------------------------------------------------------------------
// SERVICE 05: PARSE SUPPORTED PID
//------------------------------------------------------------------------------
procedure TOBDService05.ParseSupportedPID(PID: Byte; Data: TBytes);
var
  Decoder: IOBDResponseDecoder;
  Supported: TBytes;
  I: Integer;
  Comparer: IComparer<Byte>;
begin
  // Create decoder
  Decoder := TOBDSupportedPIDDecoder.Create;

  // Parse data
  if (Decoder as TOBDSupportedPIDDecoder).Parse(PID, Data, Supported) then
  begin
    // Loop over the supported pid's
    for I := Low(Supported) to High(Supported) do
    // If the pid is not already in our array, add it
    if not IsPIDSupported(Supported[I]) then
    begin
      SetLength(FSupportedPID, Length(FSupportedPID) + 1);
      FSupportedPID[Length(FSupportedPID) -1] := Supported[I];
    end;
  end;

  // Create an instance of the comparer
  Comparer := TOBDServiceSupportedPIDComparer.Create;
  // Sort the PID's ascending
  TArray.Sort<Byte>(FSupportedPID, Comparer);
end;

//------------------------------------------------------------------------------
// SERVICE 05: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService05.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Clear all data and set to defaults
  Reset;
end;

//------------------------------------------------------------------------------
// SERVICE 05: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService05.Destroy;
begin
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 05: RESET - CLEAR ALL DATA
//------------------------------------------------------------------------------
procedure TOBDService05.Reset;
begin
  // Reset supported PID's
  SetLength(FSupportedPID, 0);
end;

//------------------------------------------------------------------------------
// SERVICE 05: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService05.ParseResponse(const Response: TBytes);
var
  ServiceDecoder: IOBDServiceResponseDecoder;
  ErrorDecoder: IOBDResponseDecoder;
  ResponseDecoder: TOBDResponseDecoder;
  Error: Boolean;
  E: Byte;
  ServiceID, ParameterID: Integer;
  Data, Additional: TBytes;
begin
  // Create decoder
  ServiceDecoder := TOBDServiceResponseDecoder.Create;
  // Decode service response
  ServiceDecoder.DecodeServiceResponse(Response, Error, ServiceID, ParameterID, Data);

  // Check if the response is an error
  if Error then
  begin
    // Create decoder
    ErrorDecoder := TOBDErrorDecoder.Create;
    // Try to parse the error
    if (ErrorDecoder as TOBDErrorDecoder).Parse(Response, ServiceID, E, Additional) then
    begin
      // Notify we have a error response
      if Assigned(OnErrorResponse) then OnErrorResponse(Self, E, Additional);
    end;
    // Exit here because this is not a service response
    Exit;
  end;

  // If this response is not for this service, exit here
  if ServiceID <> GetServiceID then Exit;

  // Parse supported PID's
  if ParameterID = OBD_SERVICE_05_SUPPORTED_PID_0120 then
  begin
    ParseSupportedPID(ParameterID, Data);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 1) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank1Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 2) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank1Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 3) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank1Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 4) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank1Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 1) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank2Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 2) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank2Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 3) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank2Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 4) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank2Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 1) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank3Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 2) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank3Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 3) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank3Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 4) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank3Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 1) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank4Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 2) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank4Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 3) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank4Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 4) Rich to lean
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_RICH_TO_LEAN then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FRichToLeanBank4Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_RICH_TO_LEAN);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 1) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank1Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 2) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank1Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 3) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank1Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 1 Sensor 4) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank1Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 1) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank2Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 2) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank2Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 3) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank2Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 2 Sensor 4) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank2Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 1) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank3Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 2) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank3Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 3) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank3Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 3 Sensor 4) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank3Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 4 Sensor 1) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank4Sensor1);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 4 Sensor 2) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank4Sensor2);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 4 Sensor 3) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank4Sensor3);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_LEAN_TO_RICH);
    Exit;
  end;

  // Parse sensor voltage (Bank 4 Sensor 4) Lean to rich
  if ParameterID = OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_LEAN_TO_RICH then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorMonitoringTestResultsDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorMonitoringTestResultsDecoder).Parse(Data, FLeanToRichBank4Sensor4);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_LEAN_TO_RICH);
    Exit;
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 05: IS PID SUPPORTED
//------------------------------------------------------------------------------
function TOBDService05.IsPIDSupported(PID: Byte): Boolean;
var
  I: Integer;
begin
  // initialize result
  Result := False;
  // Loop over supported pid array
  for I := Low(FSupportedPID) to High(FSupportedPID) do
  begin
    // Check if the byte is in the array
    if FSupportedPID[I] = PID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
