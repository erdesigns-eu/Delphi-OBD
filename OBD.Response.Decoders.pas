//------------------------------------------------------------------------------
// UNIT           : OBD.Response.Decoders.pas
// CONTENTS       : OBD Response Decoders
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Response.Decoders;

interface

uses
  System.SysUtils,

  OBD.Request.Constants, OBD.Response.Constants, OBD.Service.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Response Decoder (INTERFACE)
  /// </summary>
  IOBDServiceResponseDecoder = interface
    ['{508C8798-8B71-4B30-8DB4-B619089E072E}']
    /// <summary>
    ///   Decode service response
    /// </summary>
    function DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Response Decoder (INTERFACE)
  /// </summary>
  IOBDResponseDecoder = interface
    ['{437EC6E2-F7C0-4660-8185-0F0C60DCB439}']
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service Response Decoder (CLASS)
  /// </summary>
  TOBDServiceResponseDecoder = class(TInterfacedObject, IOBDServiceResponseDecoder)
  protected
    /// <summary>
    ///   Decode service response
    /// </summary>
    function DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean; virtual;
  end;

  /// <summary>
  ///   Base OBD Response Decoder (CLASS)
  /// </summary>
  TOBDResponseDecoder = class(TInterfacedObject, IOBDResponseDecoder)
  end;

  /// <summary>
  ///   OBD Error Decoder
  /// </summary>
  TOBDErrorDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the error from the response data
    /// </summary>
    function Parse(Data: TBytes; var ServiceID: Byte; var Error: Byte; var AdditionalData: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Supported PID Decoder
  /// </summary>
  TOBDSupportedPIDDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the supported PID's from the response data
    /// </summary>
    function Parse(PID: Byte; Data: TBytes; var SupportedPID: TBytes): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel System Status Decoder
  /// </summary>
  TOBDFuelSystemStatusDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the fuel system status
    /// </summary>
    function Parse(Data: TBytes; var FuelSystem1: TOBDServiceFuelSystemStatus; var FuelSystem2: TOBDServiceFuelSystemStatus): Boolean;
  end;

  /// <summary>
  ///   OBD Percentage Decoder
  /// </summary>
  TOBDPercentageDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the value to a scale of 0 - 100%
    /// </summary>
    function Parse(Data: TBytes; var Percent: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Temperature Decoder (Degree Celcius)
  /// </summary>
  TOBDTemperatureDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the value to a degree celcius
    /// </summary>
    function Parse(Data: TBytes; var Temperature: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel Trim Decoder
  /// </summary>
  TOBDFuelTrimDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the fuel trim to a scale of -100 - 99.2%
    /// </summary>
    function Parse(Data: TBytes; var Percent: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel Pressure Decoder
  /// </summary>
  TOBDFuelPressureDecoder = class(TOBDResponseDecoder)
  public
    /// <summary>
    ///   Parse the fuel pressure to a scale of 0 - 765kPa
    /// </summary>
    function Parse(Data: TBytes; var Pressure: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Engine RPM Decoder
  /// </summary>
  TOBDEngineRPMDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the engine rpm to a scale of 0 - 16,383.75rpm
    /// </summary>
    function Parse(Data: TBytes; var RPM: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Timing Advance Decoder
  /// </summary>
  TOBDTimingAdvanceDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the timing advance to a scale of -64 -63.5°
    /// </summary>
    function Parse(Data: TBytes; var Degree: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Mass Air Flow Rate Decoder
  /// </summary>
  TOBDMassAirFlowRateDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the mass air flow rate to a scale of 0 - 655.35g/s
    /// </summary>
    function Parse(Data: TBytes; var MAF: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Commanded secondary air status Decoder
  /// </summary>
  TOBDCommandedSecondaryAirStatusDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the commanded secondary air status
    /// </summary>
    function Parse(Data: TBytes; var Status: TOBDServiceCommandedSecondaryAirStatus): Boolean;
  end;

  /// <summary>
  ///   OBD Oxygen sensor reading Decoder
  /// </summary>
  TOBDServiceOxygenSensorReadingDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the oxygen sensor reading (Voltage and Fuel-trim percentage)
    /// </summary>
    function Parse(Data: TBytes; var Voltage: Double; var Percent: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Engine runtime Decoder
  /// </summary>
  TOBDServiceEngineRuntimeDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the engine runtime (Seconds)
    /// </summary>
    function Parse(Data: TBytes; var Seconds: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Distance traveled with MIL on Decoder
  /// </summary>
  TOBDServiceDistanceTraveledWithMILOnDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the distance traveled with MIL on (KM)
    /// </summary>
    function Parse(Data: TBytes; var Distance: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel Rail Pressure Decoder
  /// </summary>
  TOBDServiceFuelRailPressureDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the Fuel Rail Pressure (relative to manifold vacuum)
    /// </summary>
    function Parse(Data: TBytes; var Pressure: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Fuel Rail Gauge Pressure Decoder
  /// </summary>
  TOBDServiceFuelRailGaugePressureDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the Fuel Rail Gauge Pressure (diesel, or gasoline direct injection)
    /// </summary>
    function Parse(Data: TBytes; var Pressure: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Oxygen Sensor reading Decoder
  /// </summary>
  TOBDServiceOxygenSensorReadingRVDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the oxygen sensor reading (Air-Fuel Equivalence Ratio and Voltage)
    /// </summary>
    function Parse(Data: TBytes; var Ratio: Double; var Voltage: Double): Boolean;
  end;

  /// <summary>
  ///   OBD EGR Error Decoder
  /// </summary>
  TOBDServiceEGRErrorDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse EGR error (Percentage)
    /// </summary>
    function Parse(Data: TBytes; var Percent: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Distance since codes cleared Decoder
  /// </summary>
  TOBDServiceDistanceSinceCodesClearedDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse Distance since codes (DTC) cleared (KM)
    /// </summary>
    function Parse(Data: TBytes; var Distance: Integer): Boolean;
  end;

  /// <summary>
  ///   OBD Evap. System Vapor Pressure Decoder
  /// </summary>
  TOBDServiceEvapSystemVaporPressureDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse Evap. System Vapor Pressure (Pa)
    /// </summary>
    function Parse(Data: TBytes; var Pressure: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Oxygen Sensor reading Decoder
  /// </summary>
  TOBDServiceOxygenSensorReadingRCDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the oxygen sensor reading (Air-Fuel Equivalence Ratio and Current)
    /// </summary>
    function Parse(Data: TBytes; var Ratio: Double; var Current: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Catalyst temperature Decoder
  /// </summary>
  TOBDServiceCatalystTemperatureDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse the Catalyst temperature (Degree Celcius)
    /// </summary>
    function Parse(Data: TBytes; var Temperature: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Control Module voltage Decoder
  /// </summary>
  TOBDServiceControlModuleVoltageDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse Control Module voltage
    /// </summary>
    function Parse(Data: TBytes; var Voltage: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Absolute Load value Decoder
  /// </summary>
  TOBDServiceAbsoluteLoadValueDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse Absolute load value (Percentage)
    /// </summary>
    function Parse(Data: TBytes; var Percent: Double): Boolean;
  end;

  /// <summary>
  ///   OBD Commanded Air-Fuel Equivalence Ratio Decoder
  /// </summary>
  TOBDServiceCommandedAirFuelEQRatioDecoder = class(TOBDResponseDecoder)
    /// <summary>
    ///   Parse Commanded Air-Fuel Equivalence Ratio (Ratio)
    /// </summary>
    function Parse(Data: TBytes; var Ratio: Double): Boolean;
  end;

implementation

//------------------------------------------------------------------------------
// DECODE SERVICE RESPONSE
//------------------------------------------------------------------------------
function TOBDServiceResponseDecoder.DecodeServiceResponse(Response: TBytes; var Error: Boolean; var ServiceID: Byte; var PID: Byte; var Data: TBytes): Boolean;
begin
  // initialize result
  Result := False;
  // initialize error
  Error := False;

  // Make sure the response is at least 2 bytes
  if Length(Response) < 2 then Exit;

  // Check if this is a error response
  if Response[0] = OBD_NEGATIVE_RESPONSE then
  begin
    Error := True;
    Exit;
  end;

  // Make sure we get a response to a service (OBD-II starts from 01)
  if Response[0] < $40 then Exit;

  // Get the service PID
  ServiceID := Response[0] - $40;
  // Get the PID
  PID := Response[1];
  // Get the data (if there is any)
  if Length(Response) > 2 then Data := Copy(Response, 2, Length(Response) - 2);

  // If we make it until here, decoding succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ERROR DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDErrorDecoder.Parse(Data: TBytes; var ServiceID: Byte; var Error: Byte; var AdditionalData: TBytes): Boolean;
begin
  // initialize result
  Result := False;
  // initialize error
  Error := OBD_UNKNOWN_ERROR;

  // Make sure we have at least 3 bytes
  if Length(Data) < 3 then Exit;

  // Get the service ID
  ServiceID := Data[1];
  // Get the error
  Error := Data[2];
  // Get additional data (Manufacturer specific)
  if Length(Data) > 3 then AdditionalData := Copy(Data, 3, Length(Data) - 3);

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PID DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDSupportedPIDDecoder.Parse(PID: Byte; Data: TBytes; var SupportedPID: TBytes): Boolean;
var
  I, BitPos, StartPID, Index: Integer;
begin
  // initialize result
  Result := False;
  // Set the length of the array
  SetLength(SupportedPID, 0);

  // Make sure we have at least 4 bytes
  if Length(Data) < 4 then Exit;

  // Calculate the starting PID based on the input PID
  StartPID := (PID div 32) * 32 + 1;

  // Initialize index for supported PIDs
  Index := 0;
  // Loop through each byte of the data
  for I := 0 to High(Data) do
  begin
    // Check each bit in the byte
    for BitPos := 7 downto 0 do
    begin
      if (Data[I] and (1 shl BitPos)) <> 0 then
      begin
        // If the bit is set, this PID is supported, add it to the array
        // of supported PID's.
        SetLength(SupportedPID, Length(SupportedPID) + 1);
        SupportedPID[Index] := StartPID + I * 8 + (7 - BitPos);
        Inc(Index);
      end;
    end;
  end;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL SYSTEM STATUS DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDFuelSystemStatusDecoder.Parse(Data: TBytes; var FuelSystem1: TOBDServiceFuelSystemStatus; var FuelSystem2: TOBDServiceFuelSystemStatus): Boolean;

  function ByteToFuelSystemStatus(Value: Byte): TOBDServiceFuelSystemStatus;
  begin
    case Value of
      0: Result := fssMotorOff;
      1: Result := fssOpenLoopInsufficientTemp;
      2: Result := fssClosedLoop;
      4: Result := fssOpenLoopEngineLoad;
      8: Result := fssOpenLoopSystemFailure;
      16: Result := fssClosedLoopFault;
    else
      Result := fsUnknown;
    end;
  end;

begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;

  // Get status for fuel system 1
  FuelSystem1 := ByteToFuelSystemStatus(Data[0]);
  // Get status for fuel system 2
  FuelSystem2 := ByteToFuelSystemStatus(Data[1]);

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PERCENTAGE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDPercentageDecoder.Parse(Data: TBytes; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the percentage
  Percent := (Data[0] / 255) * 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// TEMPERATURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDTemperatureDecoder.Parse(Data: TBytes; var Temperature: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the temperature in degrees
  Temperature := Data[0] - 40;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL TRIM DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDFuelTrimDecoder.Parse(Data: TBytes; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the percentage
  Percent := (Data[0] / 1.28) - 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL PRESSURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDFuelPressureDecoder.Parse(Data: TBytes; var Pressure: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the pressure
  Pressure := Data[0] * 3;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ENGINE RPM DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDEngineRPMDecoder.Parse(Data: TBytes; var RPM: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the RPM (revolutions per minute)
  RPM := ((Data[0] * 256) + Data[1]) div 4;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// TIMING ADVANCE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDTimingAdvanceDecoder.Parse(Data: TBytes; var Degree: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the timing advance (degrees before top dead center)
  Degree := (Data[0] / 2) - 64;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// MASS AIR FLOW RATE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDMassAirFlowRateDecoder.Parse(Data: TBytes; var MAF: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the mass air flow rate
  MAF := ((Data[0] * 256) + Data[1]) / 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// COMMANDED SECONDARY AIR STATUS DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDCommandedSecondaryAirStatusDecoder.Parse(Data: TBytes; var Status: TOBDServiceCommandedSecondaryAirStatus): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the commanded secondary air status
  case Data[0] of
    $01: Status := sasUpstream;
    $02: Status := sasDownstream;
    $04: Status := sasAtmosphereOff;
    $08: Status := sasDiagnostics;
  else
    Status := sasUnknown;
  end;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// OXYGEN SENSOR READING DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceOxygenSensorReadingDecoder.Parse(Data: TBytes; var Voltage: Double; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;

  // Get the voltage
  Voltage := Data[0] / 200;
  // Get the fuel-trim percentage
  Percent := (Data[1] / 1.28) - 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ENGINE RUNTIME DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceEngineRuntimeDecoder.Parse(Data: TBytes; var Seconds: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the runtime in seconds
  Seconds := (Data[0] * 256) + Data[1];

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// DISTANCE TRAVELED WITH MIL ON DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceDistanceTraveledWithMILOnDecoder.Parse(Data: TBytes; var Distance: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the distance in KiloMeters
  Distance := (Data[0] * 256) + Data[1];

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL RAIL PRESSURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceFuelRailPressureDecoder.Parse(Data: TBytes; var Pressure: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the fuel rail pressure
  Pressure := ((Data[0] * 256) + Data[1]) * 0.079;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// FUEL RAIL GAUGE PRESSURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceFuelRailGaugePressureDecoder.Parse(Data: TBytes; var Pressure: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the fuel rail gauge pressure
  Pressure := ((Data[0] * 256) + Data[1]) * 10;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// OXYGEN SENSOR AIR-FUEL EQ RATIO AND VOLTAGE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceOxygenSensorReadingRVDecoder.Parse(Data: TBytes; var Ratio: Double; var Voltage: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 4 bytes
  if Length(Data) < 4 then Exit;
  // Get the ratio (Lambda)
  Ratio := ((Data[0] * 256) + Data[1]) / 32768;
  // Get the voltage
  Voltage := ((Data[2] * 256) + Data[3]) / 8192;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// EGR ERROR DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceEGRErrorDecoder.Parse(Data: TBytes; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 1 byte
  if Length(Data) < 1 then Exit;
  // Get the EGR error percentage
  Percent := ((Data[0] - 128) / 128) * 100;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// DISTANCE SINCE CODES CLEARED DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceDistanceSinceCodesClearedDecoder.Parse(Data: TBytes; var Distance: Integer): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the distance
  Distance := (Data[0] * 256) + Data[1];

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// EVAP. SYSTEM VAPOR PRESSURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceEvapSystemVaporPressureDecoder.Parse(Data: TBytes; var Pressure: Double): Boolean;
var
  RawValue: Integer;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;

  // Combine the two bytes into a single integer
  RawValue := (Data[0] * 256) + Data[1];
  // Adjust for the value being a signed 16-bit integer
  if RawValue >= 32768 then RawValue := RawValue - 65536;
  // Calculate the EVAP system vapor pressure in Pascals
  Pressure := RawValue / 4;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// OXYGEN SENSOR AIR-FUEL EQ RATIO AND CURRENT DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceOxygenSensorReadingRCDecoder.Parse(Data: TBytes; var Ratio: Double; var Current: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 4 bytes
  if Length(Data) < 4 then Exit;

  // Lambda calculation
  Ratio := ((Data[0] * 256) + Data[1]) / 32768;
  // Current calculation
  Current := (((Data[2] * 256) + Data[3]) - 32768) / 256;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// CATALYST TEMPERATURE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceCatalystTemperatureDecoder.Parse(Data: TBytes; var Temperature: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the temperature
  Temperature := ((Data[0] * 256) + Data[1]) / 10 - 40;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// CONTROL MODULE VOLTAGE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceControlModuleVoltageDecoder.Parse(Data: TBytes; var Voltage: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the voltage
  Voltage := ((Data[0] * 256) + Data[1]) / 1000;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ABSOLUTE LOAD VALUE DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceAbsoluteLoadValueDecoder.Parse(Data: TBytes; var Percent: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the percentage
  Percent := (Data[0] * 256) + Data[1]) * 100 / 255;

  // If we make it until here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
//  COMMANDED AIR-FUEL EQUIVALENCE RATIO DECODER: PARSE
//------------------------------------------------------------------------------
function TOBDServiceCommandedAirFuelEQRatioDecoder.Parse(Data: TBytes; var Ratio: Double): Boolean;
begin
  // initialize result
  Result := False;

  // Make sure we have at least 2 bytes
  if Length(Data) < 2 then Exit;
  // Get the ratio
  Ratio := ((Data[0] * 256) + Data[1]) / 32768;

  // If we make it until here, parsing succeeded
  Result := True;
end;

end.
