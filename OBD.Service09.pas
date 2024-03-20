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
  private type
    /// <summary>
    ///   OBD Service 09 Parameter 08 and 0B Entry
    /// </summary>
    TOBDServiceInUsePerformanceTrackingEntry = class
    private
      /// <summary>
      ///  Number of times that all conditions necessary for a specific monitor
      ///  to detect a malfunction have been encountered.
      /// </summary>
      FConditionCounter: Integer;
      /// <summary>
      ///   Number of times that the vehicle has been operated in the
      ///   specified conditions.
      /// </summary>
      FOperatedCounter: Integer;
    public
      /// <summary>
      ///   Reset (Clear all counters)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///  Number of times that all conditions necessary for a specific monitor
      ///  to detect a malfunction have been encountered.
      /// </summary>
      property ConditionCounter: Integer read FConditionCounter;
      /// <summary>
      ///   Number of times that the vehicle has been operated in the
      ///   specified conditions.
      /// </summary>
      property OperatedCounter: Integer read FOperatedCounter;
    end;

    /// <summary>
    ///   OBD Service 09 Parameter 08
    /// </summary>
    TOBDServiceInUsePerformanceTrackingSpark = class
    private
      /// <summary>
      ///   OBD Monitoring Conditions Encountered Counts
      /// </summary>
      FOBDCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Ignition Counters
      /// </summary>
      FIGNCNTR: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Catalyst Monitor Completion Counts Bank 1
      /// </summary>
      FCATCOMP1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Catalyst Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      FCATCOND1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Catalyst Monitor Completion Counts Bank 2
      /// </summary>
      FCATCOMP2: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Catalyst Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      FCATCOND2: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   O2 Sensor Monitor Completion Counts Bank 1
      /// </summary>
      FO2SCOMP1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   O2 Sensor Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      FO2SCOND1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   O2 Sensor Monitor Completion Counts Bank 2
      /// </summary>
      FO2SCOMP2: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   O2 Sensor Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      FO2SCOND2: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EGR Monitor Completion Condition Counts
      /// </summary>
      FEGRCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EGR Monitor Conditions Encountered Counts
      /// </summary>
      FEGRCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   AIR Monitor Completion Condition Counts (Secondary Air)
      /// </summary>
      FAIRCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   AIR Monitor Conditions Encountered Counts (Secondary Air)
      /// </summary>
      FAIRCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EVAP Monitor Completion Condition Counts
      /// </summary>
      FEVAPCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EVAP Monitor Conditions Encountered Counts
      /// </summary>
      FEVAPCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Completion Counts Bank 1
      /// </summary>
      FSO2SCOMP1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      FSO2SCOND1: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Completion Counts Bank 2
      /// </summary>
      FSO2SCOMP2: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      FSO2SCOND2: TOBDServiceInUsePerformanceTrackingEntry;
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
      ///   Reset (Clear all counters)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   OBD Monitoring Conditions Encountered Counts
      /// </summary>
      property OBDCOND: TOBDServiceInUsePerformanceTrackingEntry read FOBDCOND;
      /// <summary>
      ///   Ignition Counters
      /// </summary>
      property IGNCNTR: TOBDServiceInUsePerformanceTrackingEntry read FIGNCNTR;
      /// <summary>
      ///   Catalyst Monitor Completion Counts Bank 1
      /// </summary>
      property CATCOMP1: TOBDServiceInUsePerformanceTrackingEntry read FCATCOMP1;
      /// <summary>
      ///   Catalyst Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      property CATCOND1: TOBDServiceInUsePerformanceTrackingEntry read FCATCOND1;
      /// <summary>
      ///   Catalyst Monitor Completion Counts Bank 2
      /// </summary>
      property CATCOMP2: TOBDServiceInUsePerformanceTrackingEntry read FCATCOMP2;
      /// <summary>
      ///   Catalyst Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      property CATCOND2: TOBDServiceInUsePerformanceTrackingEntry read FCATCOND2;
      /// <summary>
      ///   O2 Sensor Monitor Completion Counts Bank 1
      /// </summary>
      property O2SCOMP1: TOBDServiceInUsePerformanceTrackingEntry read FO2SCOMP1;
      /// <summary>
      ///   O2 Sensor Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      property O2SCOND1: TOBDServiceInUsePerformanceTrackingEntry read FO2SCOND1;
      /// <summary>
      ///   O2 Sensor Monitor Completion Counts Bank 2
      /// </summary>
      property O2SCOMP2: TOBDServiceInUsePerformanceTrackingEntry read FO2SCOMP2;
      /// <summary>
      ///   O2 Sensor Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      property O2SCOND2: TOBDServiceInUsePerformanceTrackingEntry read FO2SCOND2;
      /// <summary>
      ///   EGR Monitor Completion Condition Counts
      /// </summary>
      property EGRCOMP: TOBDServiceInUsePerformanceTrackingEntry read FEGRCOMP;
      /// <summary>
      ///   EGR Monitor Conditions Encountered Counts
      /// </summary>
      property EGRCOND: TOBDServiceInUsePerformanceTrackingEntry read FEGRCOND;
      /// <summary>
      ///   AIR Monitor Completion Condition Counts (Secondary Air)
      /// </summary>
      property AIRCOMP: TOBDServiceInUsePerformanceTrackingEntry read FAIRCOMP;
      /// <summary>
      ///   AIR Monitor Conditions Encountered Counts (Secondary Air)
      /// </summary>
      property AIRCOND: TOBDServiceInUsePerformanceTrackingEntry read FAIRCOND;
      /// <summary>
      ///   EVAP Monitor Completion Condition Counts
      /// </summary>
      property EVAPCOMP: TOBDServiceInUsePerformanceTrackingEntry read FEVAPCOMP;
      /// <summary>
      ///   EVAP Monitor Conditions Encountered Counts
      /// </summary>
      property EVAPCOND: TOBDServiceInUsePerformanceTrackingEntry read FEVAPCOND;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Completion Counts Bank 1
      /// </summary>
      property SO2SCOMP1: TOBDServiceInUsePerformanceTrackingEntry read FSO2SCOMP1;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Conditions Encountered Counts Bank 1
      /// </summary>
      property SO2SCOND1: TOBDServiceInUsePerformanceTrackingEntry read FSO2SCOND1;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Completion Counts Bank 2
      /// </summary>
      property SO2SCOMP2: TOBDServiceInUsePerformanceTrackingEntry read FSO2SCOMP2;
      /// <summary>
      ///   Secondary O2 Sensor Monitor Conditions Encountered Counts Bank 2
      /// </summary>
      property SO2SCOND2: TOBDServiceInUsePerformanceTrackingEntry read FSO2SCOND2;
    end;

    /// <summary>
    ///   OBD Service 09 Parameter 0B
    /// </summary>
    TOBDServiceInUsePerformanceTrackingCompression = class
    private
      /// <summary>
      ///   OBD Monitoring Conditions Encountered Counts
      /// </summary>
      FOBDCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Ignition Counter
      /// </summary>
      FIGNCNTR: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NMHC Catalyst Monitor Completion Condition Counts
      /// </summary>
      FHCCATCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NMHC Catalyst Monitor Conditions Encountered Counts
      /// </summary>
      FHCCATCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NOx/SCR Catalyst Monitor Completion Condition Counts
      /// </summary>
      FNCATCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NOx/SCR Catalyst Monitor Conditions Encountered Counts
      /// </summary>
      FNCATCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NOx Adsorber Monitor Completion Condition Counts
      /// </summary>
      FNADSCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   NOx Adsorber Monitor Conditions Encountered Counts
      /// </summary>
      FNADSCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   PM Filter Monitor Completion Condition Counts
      /// </summary>
      FPMCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   PM Filter Monitor Conditions Encountered Counts
      /// </summary>
      FPMCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Exhaust Gas Sensor Monitor Completion Condition Counts
      /// </summary>
      FEGSCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Exhaust Gas Sensor Monitor Conditions Encountered Counts
      /// </summary>
      FEGSCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EGR and/or VVT Monitor Completion Condition Counts
      /// </summary>
      FEGRCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   EGR and/or VVT Monitor Conditions Encountered Counts
      /// </summary>
      FEGRCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Boost Pressure Monitor Completion Condition Counts
      /// </summary>
      FBPCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Boost Pressure Monitor Conditions Encountered Counts
      /// </summary>
      FBPCOND: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Fuel Monitor Completion Condition Counts
      /// </summary>
      FFUELCOMP: TOBDServiceInUsePerformanceTrackingEntry;
      /// <summary>
      ///   Fuel Monitor Conditions Encountered Counts
      /// </summary>
      FFUELCOND: TOBDServiceInUsePerformanceTrackingEntry;
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
      ///   Reset (Clear all counters)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   OBD Monitoring Conditions Encountered Counts
      /// </summary>
      property OBDCOND: TOBDServiceInUsePerformanceTrackingEntry read FOBDCOND;
      /// <summary>
      ///   Ignition Counter
      /// </summary>
      property IGNCNTR: TOBDServiceInUsePerformanceTrackingEntry read FIGNCNTR;
      /// <summary>
      ///   NMHC Catalyst Monitor Completion Condition Counts
      /// </summary>
      property HCCATCOMP: TOBDServiceInUsePerformanceTrackingEntry read FHCCATCOMP;
      /// <summary>
      ///   NMHC Catalyst Monitor Conditions Encountered Counts
      /// </summary>
      property HCCATCOND: TOBDServiceInUsePerformanceTrackingEntry read FHCCATCOND;
      /// <summary>
      ///   NOx/SCR Catalyst Monitor Completion Condition Counts
      /// </summary>
      property NCATCOMP: TOBDServiceInUsePerformanceTrackingEntry read FNCATCOMP;
      /// <summary>
      ///   NOx/SCR Catalyst Monitor Conditions Encountered Counts
      /// </summary>
      property NCATCOND: TOBDServiceInUsePerformanceTrackingEntry read FNCATCOND;
      /// <summary>
      ///   NOx Adsorber Monitor Completion Condition Counts
      /// </summary>
      property NADSCOMP: TOBDServiceInUsePerformanceTrackingEntry read FNADSCOMP;
      /// <summary>
      ///   NOx Adsorber Monitor Conditions Encountered Counts
      /// </summary>
      property NADSCOND: TOBDServiceInUsePerformanceTrackingEntry read FNADSCOND;
      /// <summary>
      ///   PM Filter Monitor Completion Condition Counts
      /// </summary>
      property PMCOMP: TOBDServiceInUsePerformanceTrackingEntry read FPMCOMP;
      /// <summary>
      ///   PM Filter Monitor Conditions Encountered Counts
      /// </summary>
      property PMCOND: TOBDServiceInUsePerformanceTrackingEntry read FPMCOND;
      /// <summary>
      ///   Exhaust Gas Sensor Monitor Completion Condition Counts
      /// </summary>
      property EGSCOMP: TOBDServiceInUsePerformanceTrackingEntry read FEGSCOMP;
      /// <summary>
      ///   Exhaust Gas Sensor Monitor Conditions Encountered Counts
      /// </summary>
      property EGSCOND: TOBDServiceInUsePerformanceTrackingEntry read FEGSCOND;
      /// <summary>
      ///   EGR and/or VVT Monitor Completion Condition Counts
      /// </summary>
      property EGRCOMP: TOBDServiceInUsePerformanceTrackingEntry read FEGRCOMP;
      /// <summary>
      ///   EGR and/or VVT Monitor Conditions Encountered Counts
      /// </summary>
      property EGRCOND: TOBDServiceInUsePerformanceTrackingEntry read FEGRCOND;
      /// <summary>
      ///   Boost Pressure Monitor Completion Condition Counts
      /// </summary>
      property BPCOMP: TOBDServiceInUsePerformanceTrackingEntry read FBPCOMP;
      /// <summary>
      ///   Boost Pressure Monitor Conditions Encountered Counts
      /// </summary>
      property BPCOND: TOBDServiceInUsePerformanceTrackingEntry read FBPCOND;
      /// <summary>
      ///   Fuel Monitor Completion Condition Counts
      /// </summary>
      property FUELCOMP: TOBDServiceInUsePerformanceTrackingEntry read FFUELCOMP;
      /// <summary>
      ///   Fuel Monitor Conditions Encountered Counts
      /// </summary>
      property FUELCOND: TOBDServiceInUsePerformanceTrackingEntry read FFUELCOND;
    end;
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
    ///    In-use performance tracking for spark ignition engines
    /// </summary>
    FInPerformanceTrackingSparkEngine: TOBDServiceInUsePerformanceTrackingSpark;
    /// <summary>
    ///    In-use performance tracking for compression ignition engines
    /// </summary>
    FInPerformanceTrackingCompressionEngine: TOBDServiceInUsePerformanceTrackingCompression;

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
    ///    In-use performance tracking for spark ignition engines
    /// </summary>
    property InPerformanceTrackingSparkEngine: TOBDServiceInUsePerformanceTrackingSpark read FInPerformanceTrackingSparkEngine;
    /// <summary>
    ///    In-use performance tracking for compression ignition engines
    /// </summary>
    property InPerformanceTrackingCompressionEngine: TOBDServiceInUsePerformanceTrackingCompression read FInPerformanceTrackingCompressionEngine;

    /// <summary>
    ///   Sensor data changed event
    /// </summary>
    property OnData: TOBDServiceDataEvent read FOnData write FOnData;
  end;

implementation

//------------------------------------------------------------------------------
// SERVICE 09: IN USE PERFORMANCE TRACKING ENTRY - RESET
//------------------------------------------------------------------------------
procedure TOBDService09.TOBDServiceInUsePerformanceTrackingEntry.Reset;
begin
  // Reset condition counter
  FConditionCounter := 0;
  // Reset operated counter
  FOperatedCounter := 0;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService09.TOBDServiceInUsePerformanceTrackingSpark.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create entries
  FOBDCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FIGNCNTR := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FCATCOMP1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FCATCOND1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FCATCOMP2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FCATCOND2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FO2SCOMP1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FO2SCOND1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FO2SCOMP2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FO2SCOND2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGRCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGRCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FAIRCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FAIRCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEVAPCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEVAPCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FSO2SCOMP1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FSO2SCOND1 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FSO2SCOMP2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FSO2SCOND2 := TOBDServiceInUsePerformanceTrackingEntry.Create;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService09.TOBDServiceInUsePerformanceTrackingSpark.Destroy;
begin
  // Free entries
  FOBDCOND.Free;
  FIGNCNTR.Free;
  FCATCOMP1.Free;
  FCATCOND1.Free;
  FCATCOMP2.Free;
  FCATCOND2.Free;
  FO2SCOMP1.Free;
  FO2SCOND1.Free;
  FO2SCOMP2.Free;
  FO2SCOND2.Free;
  FEGRCOMP.Free;
  FEGRCOND.Free;
  FAIRCOMP.Free;
  FAIRCOND.Free;
  FEVAPCOMP.Free;
  FEVAPCOND.Free;
  FSO2SCOMP1.Free;
  FSO2SCOND1.Free;
  FSO2SCOMP2.Free;
  FSO2SCOND2.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 09: IN USE PERFORMANCE TRACKING SPARK - RESET
//------------------------------------------------------------------------------
procedure TOBDService09.TOBDServiceInUsePerformanceTrackingSpark.Reset;
begin
  // Reset entries
  FOBDCOND.Reset;
  FIGNCNTR.Reset;
  FCATCOMP1.Reset;
  FCATCOND1.Reset;
  FCATCOMP2.Reset;
  FCATCOND2.Reset;
  FO2SCOMP1.Reset;
  FO2SCOND1.Reset;
  FO2SCOMP2.Reset;
  FO2SCOND2.Reset;
  FEGRCOMP.Reset;
  FEGRCOND.Reset;
  FAIRCOMP.Reset;
  FAIRCOND.Reset;
  FEVAPCOMP.Reset;
  FEVAPCOND.Reset;
  FSO2SCOMP1.Reset;
  FSO2SCOND1.Reset;
  FSO2SCOMP2.Reset;
  FSO2SCOND2.Reset;
end;

(*










*)

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDService09.TOBDServiceInUsePerformanceTrackingCompression.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create entries
  FOBDCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FIGNCNTR := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FHCCATCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FHCCATCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FNCATCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FNCATCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FNADSCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FNADSCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FPMCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FPMCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGSCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGSCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGRCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FEGRCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FBPCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FBPCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FFUELCOMP := TOBDServiceInUsePerformanceTrackingEntry.Create;
  FFUELCOND := TOBDServiceInUsePerformanceTrackingEntry.Create;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDService09.TOBDServiceInUsePerformanceTrackingCompression.Destroy;
begin
  // Free entries
  FOBDCOND.Free;
  FIGNCNTR.Free;
  FHCCATCOMP.Free;
  FHCCATCOND.Free;
  FNCATCOMP.Free;
  FNCATCOND.Free;
  FNADSCOMP.Free;
  FNADSCOND.Free;
  FPMCOMP.Free;
  FPMCOND.Free;
  FEGSCOMP.Free;
  FEGSCOND.Free;
  FEGRCOMP.Free;
  FEGRCOND.Free;
  FBPCOMP.Free;
  FBPCOND.Free;
  FFUELCOMP.Free;
  FFUELCOND.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 09: IN USE PERFORMANCE TRACKING COMPRESSION - RESET
//------------------------------------------------------------------------------
procedure TOBDService09.TOBDServiceInUsePerformanceTrackingCompression.Reset;
begin
  // Reset entries
  FOBDCOND.Reset;
  FIGNCNTR.Reset;
  FHCCATCOMP.Reset;
  FHCCATCOND.Reset;
  FNCATCOMP.Reset;
  FNCATCOND.Reset;
  FNADSCOMP.Reset;
  FNADSCOND.Reset;
  FPMCOMP.Reset;
  FPMCOND.Reset;
  FEGSCOMP.Reset;
  FEGSCOND.Reset;
  FEGRCOMP.Reset;
  FEGRCOND.Reset;
  FBPCOMP.Reset;
  FBPCOND.Reset;
  FFUELCOMP.Reset;
  FFUELCOND.Reset;
end;


(*











*)

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
  // Create in-performance tracking for spark ignition engines
  FInPerformanceTrackingSparkEngine := TOBDServiceInUsePerformanceTrackingSpark.Create;
  // Create in-performance tracking for compression engines
  FInPerformanceTrackingCompressionEngine := TOBDServiceInUsePerformanceTrackingCompression.Create;
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
  // Free in-performance tracking for spark ignition engines
  FInPerformanceTrackingSparkEngine.Free;
  // Free in-performance tracking for compression engines
  FInPerformanceTrackingCompressionEngine.Free;
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
  // Reset in-performance tracking for spark ignition engines
  FInPerformanceTrackingSparkEngine.Reset;
  // Reset in-performance tracking for compression engines
  FInPerformanceTrackingCompressionEngine.Reset;
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
