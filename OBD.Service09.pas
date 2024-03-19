//------------------------------------------------------------------------------
// UNIT           : OBD.Service09.pas
// CONTENTS       : OBD Service 09 (Request vehicle information)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 19/03/2024
//------------------------------------------------------------------------------
unit OBD.Service09;

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
  ///   OBD Service 09
  /// </summary>
  TOBDService09 = class(TOBDService)
  private
    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    FSupportedPID: TBytes;
    /// <summary>
    ///   Vehicle Identification Number (VIN)
    /// </summary>
    FVehicleIdentificationNumber: string;
    /// <summary>
    ///   Calibration ID (One per line)
    /// </summary>
    FCalibrationId: TStrings;
    /// <summary>
    ///   Calibration Verification Numbers (One per line)
    /// </summary>
    FCalibrationVerificationNumber: TStrings;
    /// <summary>
    ///   ECU Name
    /// </summary>
    FECUName: string;

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
    procedure ParseResponse(Response: TBytes); override;
    /// <summary>
    ///   Is given PID supported?
    /// </summary>
    function IsPIDSupported(PID: Byte): Boolean;

    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    property SupportedPID: TBytes read FSupportedPID;
    /// <summary>
    ///   Vehicle Identification Number (VIN)
    /// </summary>
    property VehicleIdentificationNumber: string read FVehicleIdentificationNumber;
    /// <summary>
    ///   Calibration ID (One per line)
    /// </summary>
    property CalibrationId: TStrings read FCalibrationId;
    /// <summary>
    ///   Calibration Verification Numbers (One per line)
    /// </summary>
    property CalibrationVerificationNumber: TStrings read FCalibrationVerificationNumber;
    /// <summary>
    ///   ECU Name
    /// </summary>
    property ECUName: string read FECUName;

    /// <summary>
    ///   Sensor data changed event
    /// </summary>
    property OnData: TOBDServiceDataEvent read FOnData write FOnData;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 09: GET SERVIVE ID
//------------------------------------------------------------------------------
function TOBDService09.GetServiceID: Byte;
begin
  Result := OBD_SERVICE_09;
end;

//------------------------------------------------------------------------------
// SERVICE 09: PARSE SUPPORTED PID
//------------------------------------------------------------------------------
procedure TOBDService09.ParseSupportedPID(PID: Byte; Data: TBytes);
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
// SERVICE 09: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService09.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create calibration id list
  FCalibrationId := TStringList.Create;
  // Create calibration verification number list
  FCalibrationVerificationNumber := TStringList.Create;
  // Clear all data and set to defaults
  Reset;
end;

//------------------------------------------------------------------------------
// SERVICE 09: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService09.Destroy;
begin
  // Free calibration id list
  FCalibrationId.Free;
  // Free calibration verification number list
  FCalibrationVerificationNumber.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 09: RESET - CLEAR ALL DATA
//------------------------------------------------------------------------------
procedure TOBDService09.Reset;
begin
  // Reset supported PID's
  SetLength(FSupportedPID, 0);
  // Clear VIN
  FVehicleIdentificationNumber := '';
  // Clear calibration id list
  FCalibrationId.Clear;
  // Clear calibration verification number list
  FCalibrationVerificationNumber.Clear;
  // Reset the ECU name
  FECUName := '';
end;

//------------------------------------------------------------------------------
// SERVICE 09: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService09.ParseResponse(Response: TBytes);
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
  if ParameterID = OBD_SERVICE_09_SUPPORTED_PID_0120 then
  begin
    ParseSupportedPID(ParameterID, Data);
    Exit;
  end;

  // Parse Vehicle Identification Number (VIN) (PID 02)
  if ParameterID = OBD_SERVICE_09_VEHICLE_IDENTIFICATION_NUMBER then
  begin
    ResponseDecoder := TOBDServiceVehicleIdentificationNumberDecoder.Create;
    (ResponseDecoder as TOBDServiceVehicleIdentificationNumberDecoder).Parse(Data, FVehicleIdentificationNumber);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_09_VEHICLE_IDENTIFICATION_NUMBER);
    Exit;
  end;

  // Parse Calibration ID (CalID) (PID 04)
  if ParameterID = OBD_SERVICE_09_CALIBRATION_ID then
  begin
    ResponseDecoder := TOBDServiceCalibrationIdDecoder.Create;
    (ResponseDecoder as TOBDServiceCalibrationIdDecoder).Parse(Data, FCalibrationId);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_09_CALIBRATION_ID);
    Exit;
  end;

  // Parse the Calibration Verification Numbers (CVN) (PID 06)
  if ParameterID = OBD_SERVICE_09_CALIBRATION_VERIFICATION_NUMBERS then
  begin
    ResponseDecoder := TOBDServiceCalibrationVerificationNumberDecoder.Create;
    (ResponseDecoder as TOBDServiceCalibrationVerificationNumberDecoder).Parse(Data, FCalibrationVerificationNumber);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_09_CALIBRATION_VERIFICATION_NUMBERS);
    Exit;
  end;

  // Parse In-use performance tracking for spark ignition vehicles (PID 08)
  if ParameterID = OBD_SERVICE_09_INUSEPERFORMANCE_SPARK_IGNITION then
  begin
    // TODO: Implement
    Exit;
  end;

  // Parse the ECU name (PID 0A)
  if ParameterID = OBD_SERVICE_09_ECU_NAME then
  begin
    ResponseDecoder := TOBDServiceECUNameDecoder.Create;
    (ResponseDecoder as TOBDServiceECUNameDecoder).Parse(Data, FECUName);
    if Assigned(OnData) then OnData(Self, GetServiceID, OBD_SERVICE_09_ECU_NAME);
    Exit;
  end;

  // Parse In-use performance tracking for compression ignition vehicles (PID 0B)
  if ParameterID = OBD_SERVICE_09_INUSEPERFORMANCE_COMPRESSION_IGNITION then
  begin
    // TODO: Implement
    Exit;
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 09: IS PID SUPPORTED
//------------------------------------------------------------------------------
function TOBDService09.IsPIDSupported(PID: Byte): Boolean;
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
