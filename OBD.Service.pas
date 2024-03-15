//------------------------------------------------------------------------------
// UNIT           : OBD.Service.pas
// CONTENTS       : OBD Service
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 15/03/2024
//------------------------------------------------------------------------------
unit OBD.Service;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,

  OBD.Request.Constants, OBD.Request.Encoders, OBD.Response.Decoders, OBD.Service.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service (INTERFACE)
  /// </summary>
  IOBDService = interface
    ['{E76AAF33-A74D-4224-ACA1-4D63E5003793}']
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(Response: TBytes);
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Service (CLASS)
  /// </summary>
  TOBDService = class(TInterfacedObject, IOBDService)
  private
    /// <summary>
    ///   Error response event
    /// </summary>
    FOnErrorResponse: TOBDServiceErrorResponseEvent;
  public
    /// <summary>
    ///   Parse service response
    /// </summary>
    procedure ParseResponse(Response: TBytes); virtual; abstract;

    /// <summary>
    ///   Error response event
    /// </summary>
    property OnErrorResponse: TOBDServiceErrorResponseEvent read FOnErrorResponse write FOnErrorResponse;
  end;

  /// <summary>
  ///   OBD Service 01
  /// </summary>
  TOBDService01 = class(TOBDService)
  private type
    /// <summary>
    ///   OBD Service 01 Parameter 01 (Common Tests)
    /// </summary>
    TOBDServiceCommonTest = class
    private
      /// <summary>
      ///   Components: Available
      /// </summary>
      FComponentsAvailable: Boolean;
      /// <summary>
      ///   Components: Completeness
      /// </summary>
      FComponentsCompleteness: Boolean;
      /// <summary>
      ///   Fuel-system: Available
      /// </summary>
      FFuelSystemAvailable: Boolean;
      /// <summary>
      ///   Fuel-system: Completeness
      /// </summary>
      FFuelSystemCompleteness: Boolean;
      /// <summary>
      ///   Misfire: Available
      /// </summary>
      FMisfireAvailable: Boolean;
      /// <summary>
      ///   Misfire: Completeness
      /// </summary>
      FMisfireCompleteness: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Components: Available
      /// </summary>
      property ComponentsAvailable: Boolean read FComponentsAvailable;
      /// <summary>
      ///   Components: Completeness
      /// </summary>
      property ComponentsCompleteness: Boolean read FComponentsCompleteness;
      /// <summary>
      ///   Fuel-system: Available
      /// </summary>
      property FuelSystemAvailable: Boolean read FFuelSystemAvailable;
      /// <summary>
      ///   Fuel-system: Completeness
      /// </summary>
      property FuelSystemCompleteness: Boolean read FFuelSystemCompleteness;
      /// <summary>
      ///   Misfire: Available
      /// </summary>
      property MisfireAvailable: Boolean read FMisfireAvailable;
      /// <summary>
      ///   Misfire: Completeness
      /// </summary>
      property MisfireCompleteness: Boolean read FMisfireCompleteness;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 01 (Engine Type)
    /// </summary>
    TOBDServiceEngineType = (etUnknown, etSparkIgnition, etCompressionIgnition);

    /// <summary>
    ///   OBD Service 01 Parameter 01 (Engine Tests - Spark ignition - Otto/Wankel)
    /// </summary>
    TOBDServiceSparkEngineTest = class
    private
      /// <summary>
      ///   EGR and/or VVT System: Available
      /// </summary>
      FEGRVVTSystemAvailable: Boolean;
      /// <summary>
      ///   EGR and/or VVT System: Completeness
      /// </summary>
      FEGRVVTSystemCompleteness: Boolean;
      /// <summary>
      ///   Oxygen Sensor Heater: Available
      /// </summary>
      FOxygenSensorHeaterAvailable: Boolean;
      /// <summary>
      ///   Oxygen Sensor Heater: Completeness
      /// </summary>
      FOxygenSensorHeaterCompleteness: Boolean;
      /// <summary>
      ///   Oxygen Sensor: Available
      /// </summary>
      FOxygenSensorAvailable: Boolean;
      /// <summary>
      ///   Oxygen Sensor: Completeness
      /// </summary>
      FOxygenSensorCompleteness: Boolean;
      /// <summary>
      ///   Gasoline Particulate Filter: Available
      /// </summary>
      FGasolineParticulateFilterAvailable: Boolean;
      /// <summary>
      ///   Gasoline Particulate Filter: Completeness
      /// </summary>
      FGasolineParticulateFilterCompleteness: Boolean;
      /// <summary>
      ///   Secondary Air System: Available
      /// </summary>
      FSecondaryAirSystemAvailable: Boolean;
      /// <summary>
      ///   Secondary Air System: Completeness
      /// </summary>
      FSecondaryAirSystemCompleteness: Boolean;
      /// <summary>
      ///   Evaporative System: Available
      /// </summary>
      FEvoparitiveSystemAvailable: Boolean;
      /// <summary>
      ///   Evaporative System: Completeness
      /// </summary>
      FEvoparitiveSystemCompleteness: Boolean;
      /// <summary>
      ///   Heated Catalyst: Available
      /// </summary>
      FHeatedCatalystAvailable: Boolean;
      /// <summary>
      ///   Heated Catalyst: Completeness
      /// </summary>
      FHeatedCatalystCompleteness: Boolean;
      /// <summary>
      ///   Catalyst: Available
      /// </summary>
      FCatalystAvailable: Boolean;
      /// <summary>
      ///   Catalyst: Completeness
      /// </summary>
      FCatalystCompleteness: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   EGR and/or VVT System: Available
      /// </summary>
      property EGRVVTSystemAvailable: Boolean read FEGRVVTSystemAvailable;
      /// <summary>
      ///   EGR and/or VVT System: Completeness
      /// </summary>
      property EGRVVTSystemCompleteness: Boolean read FEGRVVTSystemCompleteness;
      /// <summary>
      ///   Oxygen Sensor Heater: Available
      /// </summary>
      property OxygenSensorHeaterAvailable: Boolean read FOxygenSensorHeaterAvailable;
      /// <summary>
      ///   Oxygen Sensor Heater: Completeness
      /// </summary>
      property OxygenSensorHeaterCompleteness: Boolean read FOxygenSensorHeaterCompleteness;
      /// <summary>
      ///   Oxygen Sensor: Available
      /// </summary>
      property OxygenSensorAvailable: Boolean read FOxygenSensorAvailable;
      /// <summary>
      ///   Oxygen Sensor: Completeness
      /// </summary>
      property OxygenSensorCompleteness: Boolean read FOxygenSensorCompleteness;
      /// <summary>
      ///   Gasoline Particulate Filter: Available
      /// </summary>
      property GasolineParticulateFilterAvailable: Boolean read FGasolineParticulateFilterAvailable;
      /// <summary>
      ///   Gasoline Particulate Filter: Completeness
      /// </summary>
      property GasolineParticulateFilterCompleteness: Boolean read FGasolineParticulateFilterCompleteness;
      /// <summary>
      ///   Secondary Air System: Available
      /// </summary>
      property SecondaryAirSystemAvailable: Boolean read FSecondaryAirSystemAvailable;
      /// <summary>
      ///   Secondary Air System: Completeness
      /// </summary>
      property SecondaryAirSystemCompleteness: Boolean read FSecondaryAirSystemCompleteness;
      /// <summary>
      ///   Evaporative System: Available
      /// </summary>
      property EvoparitiveSystemAvailable: Boolean read FEvoparitiveSystemAvailable;
      /// <summary>
      ///   Evaporative System: Completeness
      /// </summary>
      property EvoparitiveSystemCompleteness: Boolean read FEvoparitiveSystemCompleteness;
      /// <summary>
      ///   Heated Catalyst: Available
      /// </summary>
      property HeatedCatalystAvailable: Boolean read FHeatedCatalystAvailable;
      /// <summary>
      ///   Heated Catalyst: Completeness
      /// </summary>
      property HeatedCatalystCompleteness: Boolean read FHeatedCatalystCompleteness;
      /// <summary>
      ///   Catalyst: Available
      /// </summary>
      property CatalystAvailable: Boolean read FHeatedCatalystCompleteness;
      /// <summary>
      ///   Catalyst: Completeness
      /// </summary>
      property CatalystCompleteness: Boolean read FCatalystCompleteness;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 01 (Engine Tests - Compression ignition - Diesel)
    /// </summary>
    TOBDServiceCompressionEngineTest = class
    private
      /// <summary>
      ///   EGR and/or VVT System: Available
      /// </summary>
      FEGRVVTSystemAvailable: Boolean;
      /// <summary>
      ///   EGR and/or VVT System: Completeness
      /// </summary>
      FEGRVVTSystemCompleteness: Boolean;
      /// <summary>
      ///   PM filter monitoring: Available
      /// </summary>
      FPMFilterMonitoringAvailable: Boolean;
      /// <summary>
      ///   PM filter monitoring: Completeness
      /// </summary>
      FPMFilterMonitoringCompleteness: Boolean;
      /// <summary>
      ///   Exhaust Gas Sensor: Available
      /// </summary>
      FExhaustGasSensorAvailable: Boolean;
      /// <summary>
      ///   Exhaust Gas Sensor: Completeness
      /// </summary>
      FExhaustGasSensorCompleteness: Boolean;
      /// <summary>
      ///   Boost Pressure: Available
      /// </summary>
      FBoostPressureAvailable: Boolean;
      /// <summary>
      ///   Boost Pressure: Completeness
      /// </summary>
      FBoostPressureCompleteness: Boolean;
      /// <summary>
      ///   NOx/SCR Monitor: Available
      /// </summary>
      FNOXSCRMonitorAvailable: Boolean;
      /// <summary>
      ///   NOx/SCR Monitor: Completeness
      /// </summary>
      FNOXSCRMonitorCompleteness: Boolean;
      /// <summary>
      ///   NMHC Catalyst: Available
      /// </summary>
      FNMHCCatalystAvailable: Boolean;
      /// <summary>
      ///   NMHC Catalyst: Completeness
      /// </summary>
      FNMHCCatalystCompleteness: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   EGR and/or VVT System: Available
      /// </summary>
      property EGRVVTSystemAvailable: Boolean read FEGRVVTSystemAvailable;
      /// <summary>
      ///   EGR and/or VVT System: Completeness
      /// </summary>
      property EGRVVTSystemCompleteness: Boolean read FEGRVVTSystemCompleteness;
      /// <summary>
      ///   PM filter monitoring: Available
      /// </summary>
      property PMFilterMonitoringAvailable: Boolean read FPMFilterMonitoringAvailable;
      /// <summary>
      ///   PM filter monitoring: Completeness
      /// </summary>
      property PMFilterMonitoringCompleteness: Boolean read FPMFilterMonitoringCompleteness;
      /// <summary>
      ///   Exhaust Gas Sensor: Available
      /// </summary>
      property ExhaustGasSensorAvailable: Boolean read FExhaustGasSensorAvailable;
      /// <summary>
      ///   Exhaust Gas Sensor: Completeness
      /// </summary>
      property ExhaustGasSensorCompleteness: Boolean read FExhaustGasSensorCompleteness;
      /// <summary>
      ///   Boost Pressure: Available
      /// </summary>
      property BoostPressureAvailable: Boolean read FBoostPressureAvailable;
      /// <summary>
      ///   Boost Pressure: Completeness
      /// </summary>
      property BoostPressureCompleteness: Boolean read FBoostPressureCompleteness;
      /// <summary>
      ///   NOx/SCR Monitor: Available
      /// </summary>
      property NOXSCRMonitorAvailable: Boolean read FNOXSCRMonitorAvailable;
      /// <summary>
      ///   NOx/SCR Monitor: Completeness
      /// </summary>
      property NOXSCRMonitorCompleteness: Boolean read FNOXSCRMonitorCompleteness;
      /// <summary>
      ///   NMHC Catalyst: Available
      /// </summary>
      property NMHCCatalystAvailable: Boolean read FNMHCCatalystAvailable;
      /// <summary>
      ///   NMHC Catalyst: Completeness
      /// </summary>
      property NMHCCatalystCompleteness: Boolean read FNMHCCatalystCompleteness;
    end;
  private
    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    FSupportedPID: TBytes;
    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    FMIL: Boolean;
    /// <summary>
    ///   Array with supported PID's
    /// </summary>
    FDTC: Integer;
    /// <summary>
    ///   Common tests availability and completeness
    /// </summary>
    FCommonTest: TOBDServiceCommonTest;
    /// <summary>
    ///   Engine Type (Used for engine type specific tests)
    /// </summary>
    FEngineType: TOBDServiceEngineType;
    /// <summary>
    ///   Engine Type Specific Test (Otto/Wankel)
    /// </summary>
    FSparkEngineTest: TOBDServiceSparkEngineTest;
    /// <summary>
    ///   Engine Type Specific Test (Diesel)
    /// </summary>
    FCompressionEngineTest: TOBDServiceCompressionEngineTest;
    /// <summary>
    ///   Fuel system 1 status
    /// </summary>
    FFuelSystem1Status: TOBDServiceFuelSystemStatus;
    /// <summary>
    ///   Fuel system 2 status
    /// </summary>
    FFuelSystem2Status: TOBDServiceFuelSystemStatus;
    /// <summary>
    ///   Engine Load
    /// </summary>
    FEngineLoad: Double;

    /// <summary>
    ///   Monitor status changed event
    /// </summary>
    FOnMonitorStatus: TNotifyEvent;
  protected
    /// <summary>
    ///   Parse response with supported PID's
    /// </summary>
    procedure ParseSupportedPID(PID: Byte; Data: TBytes);
    /// <summary>
    ///   Parse monitor status (PID 01)
    /// </summary>
    procedure ParseMonitorStatus(Data: TBytes);
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
    ///   MIL (Malfunction Indicator Light) status
    /// </summary>
    property MIL: Boolean read FMIL;
    /// <summary>
    ///   Number of confirmed emissions-related DTCs available for display
    /// </summary>
    property DTC: Integer read FDTC;
    /// <summary>
    ///   Common tests availability and completeness
    /// </summary>
    property CommonTest: TOBDServiceCommonTest read FCommonTest;
    /// <summary>
    ///   Engine Type (Used for engine type specific tests)
    /// </summary>
    property EngineType: TOBDServiceEngineType read FEngineType;
    /// <summary>
    ///   Engine Type Specific Test (Otto/Wankel)
    /// </summary>
    property SparkEngineTest: TOBDServiceSparkEngineTest read FSparkEngineTest;
    /// <summary>
    ///   Engine Type Specific Test (Diesel)
    /// </summary>
    property CompressionEngineTest: TOBDServiceCompressionEngineTest read FCompressionEngineTest;
    /// <summary>
    ///   Fuel system 1 status
    /// </summary>
    property FuelSystem1Status: TOBDServiceFuelSystemStatus read FFuelSystem1Status;
    /// <summary>
    ///   Fuel system 2 status
    /// </summary>
    property FuelSystem2Status: TOBDServiceFuelSystemStatus read FFuelSystem2Status;
    /// <summary>
    ///   Engine Load
    /// </summary>
    property EngineLoad: Double read FEngineLoad;

    /// <summary>
    ///   Monitor status changed event
    /// </summary>
    property OnMonitorStatus: TNotifyEvent read FOnMonitorStatus write FOnMonitorStatus;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 01: PARSE SUPPORTED PID
//------------------------------------------------------------------------------
procedure TOBDService01.ParseSupportedPID(PID: Byte; Data: TBytes);
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
  Comparer := TSupportedPIDComparer.Create;
  // Sort the PID's ascending
  TArray.Sort<Byte>(FSupportedPID, Comparer);
end;

//------------------------------------------------------------------------------
// SERVICE 01: PARSE MONITOR STATUS (PID 01)
//------------------------------------------------------------------------------
procedure TOBDService01.ParseMonitorStatus(Data: TBytes);
begin
  // Make sure we have at least 4 bytes
  if Length(Data) < 4 then Exit;

  // Get the MIL (Malfunction Indicator Light) status
  FMIL := (Data[0] and $80) <> 0;

  // Get the number of confirmed emissions-related DTCs available for display
  FDTC := Data[0] and $7F;

  // Parse engine type
  if (Data[1] and $08) = 0 then
    FEngineType := etSparkIgnition
  else
    FEngineType := etCompressionIgnition;

  // Parse common tests availability
  FCommonTest.FMisfireAvailable := (Data[1] and $01) <> 0;
  FCommonTest.FFuelSystemAvailable := (Data[1] and $02) <> 0;
  FCommonTest.FComponentsAvailable := (Data[1] and $04) <> 0;

  // Parse common tests completeness
  FCommonTest.FMisfireCompleteness := (Data[1] and $20) <> 0;
  FCommonTest.FFuelSystemCompleteness := (Data[1] and $10) <> 0;
  FCommonTest.FComponentsCompleteness := (Data[1] and $08) <> 0;

    // Parse engine type specific tests (Otto/Wankel) availability
  FSparkEngineTest.FEGRVVTSystemAvailable := (Data[2] and $80) <> 0;
  FSparkEngineTest.FOxygenSensorHeaterAvailable := (Data[2] and $40) <> 0;
  FSparkEngineTest.FOxygenSensorAvailable := (Data[2] and $20) <> 0;
  FSparkEngineTest.FGasolineParticulateFilterAvailable := (Data[2] and $10) <> 0;
  FSparkEngineTest.FSecondaryAirSystemAvailable := (Data[2] and $08) <> 0;
  FSparkEngineTest.FEvoparitiveSystemAvailable := (Data[2] and $04) <> 0;
  FSparkEngineTest.FHeatedCatalystAvailable := (Data[2] and $02) <> 0;
  FSparkEngineTest.FCatalystAvailable := (Data[2] and $01) <> 0;

  // Parse engine type specific tests (Otto/Wankel) completeness
  FSparkEngineTest.FEGRVVTSystemCompleteness := (Data[3] and $80) <> 0;
  FSparkEngineTest.FOxygenSensorHeaterCompleteness := (Data[3] and $40) <> 0;
  FSparkEngineTest.FOxygenSensorCompleteness := (Data[3] and $20) <> 0;
  FSparkEngineTest.FGasolineParticulateFilterCompleteness := (Data[3] and $10) <> 0;
  FSparkEngineTest.FSecondaryAirSystemCompleteness := (Data[3] and $08) <> 0;
  FSparkEngineTest.FEvoparitiveSystemCompleteness := (Data[3] and $04) <> 0;
  FSparkEngineTest.FHeatedCatalystCompleteness := (Data[3] and $02) <> 0;
  FSparkEngineTest.FCatalystCompleteness := (Data[3] and $01) <> 0;

  // Parse engine type specific tests (Diesel) availability
  FCompressionEngineTest.FEGRVVTSystemAvailable := (Data[2] and $80) <> 0;
  FCompressionEngineTest.FPMFilterMonitoringAvailable := (Data[2] and $40) <> 0;
  FCompressionEngineTest.FExhaustGasSensorAvailable := (Data[2] and $20) <> 0;
  FCompressionEngineTest.FBoostPressureAvailable := (Data[2] and $08) <> 0;
  FCompressionEngineTest.FNOXSCRMonitorAvailable := (Data[2] and $02) <> 0;
  FCompressionEngineTest.FNMHCCatalystAvailable := (Data[2] and $01) <> 0;

  // Parse engine type specific tests (Diesel) completeness
  FCompressionEngineTest.FEGRVVTSystemCompleteness := (Data[3] and $80) <> 0;
  FCompressionEngineTest.FPMFilterMonitoringCompleteness := (Data[3] and $40) <> 0;
  FCompressionEngineTest.FExhaustGasSensorCompleteness := (Data[3] and $20) <> 0;
  FCompressionEngineTest.FBoostPressureCompleteness := (Data[3] and $08) <> 0;
  FCompressionEngineTest.FNOXSCRMonitorCompleteness := (Data[3] and $02) <> 0;
  FCompressionEngineTest.FNMHCCatalystCompleteness := (Data[3] and $01) <> 0;

  // Notify we have new data
  if Assigned(OnMonitorStatus) then OnMonitorStatus(Self);
end;

//------------------------------------------------------------------------------
// SERVICE 01: COMMON TESTS - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceCommonTest.Reset;
begin
  FComponentsAvailable := False;
  FFuelSystemAvailable := False;
  FFuelSystemCompleteness := False;
  FMisfireAvailable := False;
  FMisfireCompleteness := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: SPARK ENGINE TESTS - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceSparkEngineTest.Reset;
begin
  FEGRVVTSystemAvailable := False;
  FEGRVVTSystemCompleteness := False;
  FOxygenSensorHeaterAvailable := False;
  FOxygenSensorHeaterCompleteness := False;
  FOxygenSensorAvailable := False;
  FOxygenSensorCompleteness := False;
  FGasolineParticulateFilterAvailable := False;
  FGasolineParticulateFilterCompleteness := False;
  FSecondaryAirSystemAvailable := False;
  FSecondaryAirSystemCompleteness := False;
  FEvoparitiveSystemAvailable := False;
  FEvoparitiveSystemCompleteness := False;
  FHeatedCatalystAvailable := False;
  FHeatedCatalystCompleteness := False;
  FCatalystAvailable := False;
  FCatalystCompleteness := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: COMPRESSION ENGINE TESTS - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceCompressionEngineTest.Reset;
begin
  FEGRVVTSystemAvailable := False;
  FEGRVVTSystemCompleteness := False;
  FPMFilterMonitoringAvailable := False;
  FPMFilterMonitoringCompleteness := False;
  FExhaustGasSensorAvailable := False;
  FExhaustGasSensorCompleteness := False;
  FBoostPressureAvailable := False;
  FBoostPressureCompleteness := False;
  FNOXSCRMonitorAvailable := False;
  FNOXSCRMonitorCompleteness := False;
  FNMHCCatalystAvailable := False;
  FNMHCCatalystCompleteness := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService01.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create common tests
  FCommonTest := TOBDServiceCommonTest.Create;
  // Create engine type specific tests - Otto/Wankel
  FSparkEngineTest := TOBDServiceSparkEngineTest.Create;
  // Create engine type specific tests - Diesel
  FCompressionEngineTest := TOBDServiceCompressionEngineTest.Create;
end;

//------------------------------------------------------------------------------
// SERVICE 01: DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService01.Destroy;
begin
  // Free common tests
  FCommonTest.Free;
  // Free engine type specific tests - Otto/Wankel
  FSparkEngineTest.Free;
  // Free engine type specific tests - Diesel
  FCompressionEngineTest.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 01: RESET - CLEAR ALL DATA
//------------------------------------------------------------------------------
procedure TOBDService01.Reset;
begin
  // Clear supported PID's
  SetLength(FSupportedPID, 0);
  // Clear MIL status
  FMIL := False;
  // Clear DTC count
  FDTC := 0;
  // Clear common tests
  FCommonTest.Reset;
  // Clear engine type
  FEngineType := etUnknown;
  // Clear engine tests
  FSparkEngineTest.Reset;
  FCompressionEngineTest.Reset;
  // Reset fuel systems status
  FFuelSystem1Status := fsUnknown;
  FFuelSystem2Status := fsUnknown;
  // Reset engine load
  FEngineLoad := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: PARSE SERVICE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService01.ParseResponse(Response: TBytes);
var
  ServiceDecoder: IOBDServiceResponseDecoder;
  ErrorDecoder: IOBDResponseDecoder;
  ResponseDecoder: TOBDResponseDecoder;
  Error: Boolean;
  ServiceID, PID, E: Byte;
  Data, Additional: TBytes;
begin
  // Create decoder
  ServiceDecoder := TOBDServiceResponseDecoder.Create;
  // Decode service response
  ServiceDecoder.DecodeServiceResponse(Response, Error, ServiceID, PID, Data);

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
  if ServiceID <> OBD_SERVICE_01 then Exit;

  // Parse supported PID's
  if PID in [
    OBD_SERVICE_01_SUPPORTED_PID_0120,
    OBD_SERVICE_01_SUPPORTED_PID_2140,
    OBD_SERVICE_01_SUPPORTED_PID_4160,
    OBD_SERVICE_01_SUPPORTED_PID_6180,
    OBD_SERVICE_01_SUPPORTED_PID_81A0,
    OBD_SERVICE_01_SUPPORTED_PID_A1C0,
    OBD_SERVICE_01_SUPPORTED_PID_C1E0
  ] then ParseSupportedPID(PID, Data);

  // Parse monitor status (PID 01)
  if PID = OBD_SERVICE_01_MONITOR_STATUS then ParseMonitorStatus(Data);

  // Parse fuel system status (PID 03)
  if PID = OBD_SERVICE_01_FUEL_SYSTEM_STATUS then
  begin
    ResponseDecoder := TOBDFuelSystemStatusDecoder.Create;
    (ResponseDecoder as TOBDFuelSystemStatusDecoder).Parse(Data, FFuelSystem1Status, FFuelSystem2Status);
  end;

  // Parse engine load (PID 04)
  if PID = OBD_SERVICE_01_CALCULATED_ENGINE_LOAD then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FEngineLoad);
  end;
end;

//------------------------------------------------------------------------------
// SERVICE 01: IS PID SUPPORTED
//------------------------------------------------------------------------------
function TOBDService01.IsPIDSupported(PID: Byte): Boolean;
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

