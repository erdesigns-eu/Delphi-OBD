//------------------------------------------------------------------------------
// UNIT           : OBD.Service01.pas
// CONTENTS       : OBD Service 01
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 16/03/2024
//------------------------------------------------------------------------------
unit OBD.Service01;

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

    /// <summary>
    ///   OBD Service 01 Parameter 13 (Oxygen sensors present in 2 banks)
    /// </summary>
    TOBDServiceOxygenSensorPresent2Banks = class
    private
      /// <summary>
      ///   Bank 1 Sensor 1
      /// </summary>
      FBank1Sensor1: Boolean;
      /// <summary>
      ///   Bank 1 Sensor 2
      /// </summary>
      FBank1Sensor2: Boolean;
      /// <summary>
      ///   Bank 1 Sensor 3
      /// </summary>
      FBank1Sensor3: Boolean;
      /// <summary>
      ///   Bank 1 Sensor 4
      /// </summary>
      FBank1Sensor4: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 1
      /// </summary>
      FBank2Sensor1: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 2
      /// </summary>
      FBank2Sensor2: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 3
      /// </summary>
      FBank2Sensor3: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 4
      /// </summary>
      FBank2Sensor4: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Bank 1 Sensor 1
      /// </summary>
      property Bank1Sensor1: Boolean read FBank1Sensor1;
      /// <summary>
      ///   Bank 1 Sensor 2
      /// </summary>
      property Bank1Sensor2: Boolean read FBank1Sensor2;
      /// <summary>
      ///   Bank 1 Sensor 3
      /// </summary>
      property Bank1Sensor3: Boolean read FBank1Sensor3;
      /// <summary>
      ///   Bank 1 Sensor 4
      /// </summary>
      property Bank1Sensor4: Boolean read FBank1Sensor4;
      /// <summary>
      ///   Bank 2 Sensor 1
      /// </summary>
      property Bank2Sensor1: Boolean read FBank2Sensor1;
      /// <summary>
      ///   Bank 2 Sensor 2
      /// </summary>
      property Bank2Sensor2: Boolean read FBank2Sensor2;
      /// <summary>
      ///   Bank 2 Sensor 3
      /// </summary>
      property Bank2Sensor3: Boolean read FBank2Sensor3;
      /// <summary>
      ///   Bank 2 Sensor 4
      /// </summary>
      property Bank2Sensor4: Boolean read FBank2Sensor4;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 14 - 1B (Oxygen sensors reading: Voltage, Fuel-trim)
    /// </summary>
    TOBDServiceOxygenSensorVoltageFuelTrim = class
    private
      /// <summary>
      ///   Voltage
      /// </summary>
      FVoltage: Double;
      /// <summary>
      ///   Fuel trim percentage
      /// </summary>
      FFuelTrim: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Voltage
      /// </summary>
      property Voltage: Double read FVoltage;
      /// <summary>
      ///   Fuel trim percentage
      /// </summary>
      property FuelTrim: Double read FFuelTrim;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 1D (Oxygen sensors present in 4 banks)
    /// </summary>
    TOBDServiceOxygenSensorPresent4Banks = class
    private
      /// <summary>
      ///   Bank 1 Sensor 1
      /// </summary>
      FBank1Sensor1: Boolean;
      /// <summary>
      ///   Bank 1 Sensor 2
      /// </summary>
      FBank1Sensor2: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 1
      /// </summary>
      FBank2Sensor1: Boolean;
      /// <summary>
      ///   Bank 2 Sensor 2
      /// </summary>
      FBank2Sensor2: Boolean;
      /// <summary>
      ///   Bank 3 Sensor 1
      /// </summary>
      FBank3Sensor1: Boolean;
      /// <summary>
      ///   Bank 3 Sensor 2
      /// </summary>
      FBank3Sensor2: Boolean;
      /// <summary>
      ///   Bank 4 Sensor 1
      /// </summary>
      FBank4Sensor1: Boolean;
      /// <summary>
      ///   Bank 4 Sensor 2
      /// </summary>
      FBank4Sensor2: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Bank 1 Sensor 1
      /// </summary>
      property Bank1Sensor1: Boolean read FBank1Sensor1;
      /// <summary>
      ///   Bank 1 Sensor 2
      /// </summary>
      property Bank1Sensor2: Boolean read FBank1Sensor2;
      /// <summary>
      ///   Bank 2 Sensor 1
      /// </summary>
      property Bank2Sensor1: Boolean read FBank2Sensor1;
      /// <summary>
      ///   Bank 2 Sensor 2
      /// </summary>
      property Bank2Sensor2: Boolean read FBank2Sensor2;
      /// <summary>
      ///   Bank 3 Sensor 1
      /// </summary>
      property Bank3Sensor1: Boolean read FBank3Sensor1;
      /// <summary>
      ///   Bank 3 Sensor 2
      /// </summary>
      property Bank3Sensor2: Boolean read FBank3Sensor2;
      /// <summary>
      ///   Bank 4 Sensor 1
      /// </summary>
      property Bank4Sensor1: Boolean read FBank4Sensor1;
      /// <summary>
      ///   Bank 4 Sensor 2
      /// </summary>
      property Bank4Sensor2: Boolean read FBank4Sensor2;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 24 - 2B (Oxygen sensors reading: Air-Fuel Equivalence Ratio, Voltage)
    /// </summary>
    TOBDServiceOxygenSensorAirFuelEQRatioVoltage = class
    private
      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      FAirFuelEquivalenceRatio: Double;
      /// <summary>
      ///   Voltage
      /// </summary>
      FVoltage: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      property AirFuelEquivalenceRatio: Double read FAirFuelEquivalenceRatio;
      /// <summary>
      ///   Voltage
      /// </summary>
      property Voltage: Double read FVoltage;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 34 - 2B (Oxygen sensors reading: Air-Fuel Equivalence Ratio, Current)
    /// </summary>
    TOBDServiceOxygenSensorAirFuelEQRatioCurrent = class
    private
      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      FAirFuelEquivalenceRatio: Double;
      /// <summary>
      ///   Current
      /// </summary>
      FCurrent: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      property AirFuelEquivalenceRatio: Double read FAirFuelEquivalenceRatio;
      /// <summary>
      ///   Current
      /// </summary>
      property Current: Double read FCurrent;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 4F, 50 (Maximum value for Fuel–Air equivalence ratio,
    ///   oxygen sensor voltage, oxygen sensor current, and intake manifold absolute pressure,
    ///   air flow rate from mass air flow sensor)
    /// </summary>
    TOBDServiceMaxSensorValues = class
    private
      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      FAirFuelEquivalenceRatio: Double;
      /// <summary>
      ///   Oxygen Sensor voltage
      /// </summary>
      FOxygenSensorVoltage: Double;
      /// <summary>
      ///   Oxygen Sensor current
      /// </summary>
      FOxygenSensorCurrent: Double;
      /// <summary>
      ///   Intake manifold pressure
      /// </summary>
      FIntakeManifoldPressure: Integer;
      /// <summary>
      ///   Air flow rate from MAF sensor
      /// </summary>
      FAirFlowRate: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Air-fuel Equivalence Ratio (Lambda)
      /// </summary>
      property AirFuelEquivalenceRatio: Double read FAirFuelEquivalenceRatio;
      /// <summary>
      ///   Oxygen Sensor voltage
      /// </summary>
      property OxygenSensorVoltage: Double read FOxygenSensorVoltage;
      /// <summary>
      ///   Oxygen Sensor current
      /// </summary>
      property OxygenSensorCurrent: Double read FOxygenSensorCurrent;
      /// <summary>
      ///   Intake manifold pressure
      /// </summary>
      property IntakeManifoldPressure: Integer read FIntakeManifoldPressure;
      /// <summary>
      ///   Air flow rate from MAF sensor
      /// </summary>
      property AirFlowRate: Double read FAirFlowRate;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 55 and 56 (Short/Long term oxygen sensor trim)
    /// </summary>
    TOBDServiceOxygenSensorTrimBank1Bank3 = class
    private
      /// <summary>
      ///   Bank 1
      /// </summary>
      FBank1: Double;
      /// <summary>
      ///   Bank 3
      /// </summary>
      FBank3: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Bank 1
      /// </summary>
      property Bank1: Double read FBank1;
      /// <summary>
      ///   Bank 3
      /// </summary>
      property Bank3: Double read FBank3;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 57 - 58 (Short/Long term oxygen sensor trim)
    /// </summary>
    TOBDServiceOxygenSensorTrimBank2Bank4 = class
    private
      /// <summary>
      ///   Bank 2
      /// </summary>
      FBank2: Double;
      /// <summary>
      ///   Bank 4
      /// </summary>
      FBank4: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Bank 2
      /// </summary>
      property Bank2: Double read FBank2;
      /// <summary>
      ///   Bank 4
      /// </summary>
      property Bank4: Double read FBank4;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 64 (Engine percent torque data)
    /// </summary>
    TOBDServiceEnginePercentTorqueData = class
    private
      /// <summary>
      ///   Idle
      /// </summary>
      FIdle: Integer;
      /// <summary>
      ///   Point 1
      /// </summary>
      FPoint1: Integer;
      /// <summary>
      ///   Point 2
      /// </summary>
      FPoint2: Integer;
      /// <summary>
      ///   Point 3
      /// </summary>
      FPoint3: Integer;
      /// <summary>
      ///   Point 4
      /// </summary>
      FPoint4: Integer;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Idle
      /// </summary>
      property Idle: Integer read FIdle;
      /// <summary>
      ///   Point 1
      /// </summary>
      property Point1: Integer read FPoint1;
      /// <summary>
      ///   Point 2
      /// </summary>
      property Point2: Integer read FPoint2;
      /// <summary>
      ///   Point 3
      /// </summary>
      property Point3: Integer read FPoint3;
      /// <summary>
      ///   Point 4
      /// </summary>
      property Point4: Integer read FPoint4;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 65 (Auxiliary input / output supported)
    /// </summary>
    TOBDServiceAuxiliaryInputOutputSupported = class
    private
      /// <summary>
      ///   Input 1
      /// </summary>
      FInput1: Boolean;
      /// <summary>
      ///   Output 1
      /// </summary>
      FOutput1: Boolean;
      /// <summary>
      ///   Input 2
      /// </summary>
      FInput2: Boolean;
      /// <summary>
      ///   Output 2
      /// </summary>
      FOutput2: Boolean;
      /// <summary>
      ///   Input 3
      /// </summary>
      FInput3: Boolean;
      /// <summary>
      ///   Output 3
      /// </summary>
      FOutput3: Boolean;
      /// <summary>
      ///   Input 4
      /// </summary>
      FInput4: Boolean;
      /// <summary>
      ///   Output 4
      /// </summary>
      FOutput4: Boolean;
      /// <summary>
      ///   Input 5
      /// </summary>
      FInput5: Boolean;
      /// <summary>
      ///   Output 5
      /// </summary>
      FOutput5: Boolean;
      /// <summary>
      ///   Input 6
      /// </summary>
      FInput6: Boolean;
      /// <summary>
      ///   Output 6
      /// </summary>
      FOutput6: Boolean;
      /// <summary>
      ///   Input 7
      /// </summary>
      FInput7: Boolean;
      /// <summary>
      ///   Output 7
      /// </summary>
      FOutput7: Boolean;
      /// <summary>
      ///   Input 8
      /// </summary>
      FInput8: Boolean;
      /// <summary>
      ///   Output 8
      /// </summary>
      FOutput8: Boolean;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Input 1
      /// </summary>
      property Input1: Boolean read FInput1;
      /// <summary>
      ///   Output 1
      /// </summary>
      property Output1: Boolean read FOutput1;
      /// <summary>
      ///   Input 2
      /// </summary>
      property Input2: Boolean read FInput2;
      /// <summary>
      ///   Output 2
      /// </summary>
      property Output2: Boolean read FOutput2;
      /// <summary>
      ///   Input 3
      /// </summary>
      property Input3: Boolean read FInput3;
      /// <summary>
      ///   Output 3
      /// </summary>
      property Output3: Boolean read FOutput3;
      /// <summary>
      ///   Input 4
      /// </summary>
      property Input4: Boolean read FInput4;
      /// <summary>
      ///   Output 4
      /// </summary>
      property Output4: Boolean read Foutput4;
      /// <summary>
      ///   Input 5
      /// </summary>
      property Input5: Boolean read FInput5;
      /// <summary>
      ///   Output 5
      /// </summary>
      property Output5: Boolean read FOutput5;
      /// <summary>
      ///   Input 6
      /// </summary>
      property Input6: Boolean read FInput6;
      /// <summary>
      ///   Output 6
      /// </summary>
      property Output6: Boolean read FOutput6;
      /// <summary>
      ///   Input 7
      /// </summary>
      property Input7: Boolean read FInput7;
      /// <summary>
      ///   Output 7
      /// </summary>
      property Output7: Boolean read FOutput7;
      /// <summary>
      ///   Input 8
      /// </summary>
      property Input8: Boolean read FInput8;
      /// <summary>
      ///   Output 8
      /// </summary>
      property Output8: Boolean read FOutput8;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 66 (Mass air flow sensor)
    /// </summary>
    TOBDServiceMassAirFlowSensor = class
    private
      /// <summary>
      ///   Sensor A supported
      /// </summary>
      FSensorASupported: Boolean;
      /// <summary>
      ///   Sensor B supported
      /// </summary>
      FSensorBSupported: Boolean;
      /// <summary>
      ///   Sensor A flow rate
      /// </summary>
      FSensorARate: Double;
      /// <summary>
      ///   Sensor B flow rate
      /// </summary>
      FSensorBRate: Double;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Sensor A supported
      /// </summary>
      property SensorASupported: Boolean read FSensorASupported;
      /// <summary>
      ///   Sensor B supported
      /// </summary>
      property SensorBSupported: Boolean read FSensorBSupported;
      /// <summary>
      ///   Sensor A flow rate
      /// </summary>
      property SensorARate: Double read FSensorARate;
      /// <summary>
      ///   Sensor B flow rate
      /// </summary>
      property SensorBRate: Double read FSensorBRate;
    end;

    /// <summary>
    ///   OBD Service 01 Parameter 67 and 68 (Engine coolant temperature, Intake air temperature sensor)
    /// </summary>
    TOBDServiceSensorTemperatureData = class
    private
      /// <summary>
      ///   Sensor A supported
      /// </summary>
      FSensorASupported: Boolean;
      /// <summary>
      ///   Sensor B supported
      /// </summary>
      FSensorBSupported: Boolean;
      /// <summary>
      ///   Sensor A temperature
      /// </summary>
      FSensorATemperature: Integer;
      /// <summary>
      ///   Sensor B temperature
      /// </summary>
      FSensorBTemperature: Integer;
    public
      /// <summary>
      ///   Reset (Clear all tests)
      /// </summary>
      procedure Reset;

      /// <summary>
      ///   Sensor A supported
      /// </summary>
      property SensorASupported: Boolean read FSensorASupported;
      /// <summary>
      ///   Sensor B supported
      /// </summary>
      property SensorBSupported: Boolean read FSensorBSupported;
      /// <summary>
      ///   Sensor A temperature
      /// </summary>
      property SensorATemperature: Integer read FSensorATemperature;
      /// <summary>
      ///   Sensor B temperature
      /// </summary>
      property SensorBTemperature: Integer read FSensorBTemperature;
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
    ///   Calculated Engine Load
    /// </summary>
    FCalculatedEngineLoad: Double;
    /// <summary>
    ///   Engine coolant temperature
    /// </summary>
    FEngineCoolantTemperature: Integer;
    /// <summary>
    ///   Short term fuel trim (Bank 1)
    /// </summary>
    FShortTermFuelTrimBank1: Double;
    /// <summary>
    ///   Long term fuel trim (Bank 1)
    /// </summary>
    FLongTermFuelTrimBank1: Double;
    /// <summary>
    ///   Short term fuel trim (Bank 2)
    /// </summary>
    FShortTermFuelTrimBank2: Double;
    /// <summary>
    ///   Long term fuel trim (Bank 1)
    /// </summary>
    FLongTermFuelTrimBank2: Double;
    /// <summary>
    ///   Fuel Pressure
    /// </summary>
    FFuelPressure: Integer;
    /// <summary>
    ///   Intake manifold absolute pressure
    /// </summary>
    FIntakeManifoldAbsolutePressure: Integer;
    /// <summary>
    ///   Engine RPM
    /// </summary>
    FEngineRPM: Integer;
    /// <summary>
    ///   Vehicle Speed
    /// </summary>
    FVehicleSpeed: Integer;
    /// <summary>
    ///   Timing Advance
    /// </summary>
    FTimingAdvance: Double;
    /// <summary>
    ///   Intake air temperature
    /// </summary>
    FIntakeAirTemperature: Integer;
    /// <summary>
    ///   Mass air flow rate
    /// </summary>
    FMassAirFlowRate: Double;
    /// <summary>
    ///   Throttle position
    /// </summary>
    FThrottlePosition: Double;
    /// <summary>
    ///   Commanded secondary air status
    /// </summary>
    FCommandedSecondaryAirStatus: TOBDServiceCommandedSecondaryAirStatus;
    /// <summary>
    ///   Oxygen sensors present in 2 banks
    /// </summary>
    FOxygenSensorPresent2Banks: TOBDServiceOxygenSensorPresent2Banks;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor1Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor2Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor3Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor4Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor1Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor2Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor3Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    FOxygenSensor4Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim;
    /// <summary>
    ///   OBD standards this vehicle conforms to
    /// </summary>
    FOBDStandard: Byte;
    /// <summary>
    ///   Oxygen sensors present in 4 banks
    /// </summary>
    FOxygenSensorPresent4Banks: TOBDServiceOxygenSensorPresent4Banks;
    /// <summary>
    ///   Auxilary input status
    /// </summary>
    FAuxilaryInputStatus: TOBDServiceAuxilaryInputStatus;
    /// <summary>
    ///   Runtime since engine start
    /// </summary>
    FRuntimeSinceEngineStart: Integer;
    /// <summary>
    ///   Distance traveled with MIL (malfunction indicator lamp) on
    /// </summary>
    FDistanceTraveledWithMILOn: Integer;
    /// <summary>
    ///   Fuel Rail Pressure (relative to manifold vacuum)
    /// </summary>
    FFuelRailPressure: Double;
    /// <summary>
    ///   Fuel Rail Gauge Pressure (diesel, or gasoline direct injection)
    /// </summary>
    FFuelRailGaugePressure: Double;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor1Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor2Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor3Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor4Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor1Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor2Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor3Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    FOxygenSensor4Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage;
    /// <summary>
    ///   Commanded EGR (Percentage)
    /// </summary>
    FCommandedEGR: Double;
    /// <summary>
    ///   EGR Error (Percentage)
    /// </summary>
    FEGRError: Double;
    /// <summary>
    ///   Commanded evaporative purge (Percentage)
    /// </summary>
    FCommandedEvaportativePurge: Double;
    /// <summary>
    ///   Fuel Tank Level Input (Percentage)
    /// </summary>
    FFuelLevel: Double;
    /// <summary>
    ///   Warm-ups since codes cleared (0 - 255)
    /// </summary>
    FWarmUpsSinceCodesCleared: Byte;
    /// <summary>
    ///   Distance traveled since codes cleared
    /// </summary>
    FDistanceSinceCodesCleared: Integer;
    /// <summary>
    ///   Evap. System Vapor Pressure
    /// </summary>
    FEvapSystemVaporPressure: Double;
    /// <summary>
    ///   Absolute Barometric Pressure
    /// </summary>
    FAbsoluteBarometricPressure: Integer;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor1Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor2Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor3Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor4Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor1Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor2Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor3Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    FOxygenSensor4Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent;
    /// <summary>
    ///   Catalyst Temperature: Bank 1, Sensor 1 (Degree Celcius)
    /// </summary>
    FCatalystTemperatureSensor1Bank1: Double;
    /// <summary>
    ///   Catalyst Temperature: Bank 2, Sensor 1 (Degree Celcius)
    /// </summary>
    FCatalystTemperatureSensor1Bank2: Double;
    /// <summary>
    ///   Catalyst Temperature: Bank 1, Sensor 2 (Degree Celcius)
    /// </summary>
    FCatalystTemperatureSensor2Bank1: Double;
    /// <summary>
    ///   Catalyst Temperature: Bank 2, Sensor 2 (Degree Celcius)
    /// </summary>
    FCatalystTemperatureSensor2Bank2: Double;
    /// <summary>
    ///   Control Module voltage
    /// </summary>
    FControlModuleVoltage: Double;
    /// <summary>
    ///   Absolute load value
    /// </summary>
    FAbsoluteLoadValue: Double;
    /// <summary>
    ///   Commanded Air-Fuel Equivalence Ratio (Lambda)
    /// </summary>
    FCommandedAirFuelEquivalenceRatio: Double;
    /// <summary>
    ///   Relative throttle position (Percentage)
    /// </summary>
    FRelativeThrottlePosition: Double;
    /// <summary>
    ///   Ambient air temperature (Degree Celcius)
    /// </summary>
    FAmbientAirTemperature: Integer;
    /// <summary>
    ///   Absolute throttle position B (Percentage)
    /// </summary>
    FAbsoluteThrottlePositionB: Double;
    /// <summary>
    ///   Absolute throttle position C (Percentage)
    /// </summary>
    FAbsoluteThrottlePositionC: Double;
    /// <summary>
    ///   Accelerator pedal position D (Percentage)
    /// </summary>
    FAcceleratorPedalPositionD: Double;
    /// <summary>
    ///   Accelerator pedal position E (Percentage)
    /// </summary>
    FAcceleratorPedalPositionE: Double;
    /// <summary>
    ///   Accelerator pedal position F (Percentage)
    /// </summary>
    FAcceleratorPedalPositionF: Double;
    /// <summary>
    ///   Commanded throttle actuator (Percentage)
    /// </summary>
    FCommandedThrottleActuator: Double;
    /// <summary>
    ///   Time run with MIL on (Minutes)
    /// </summary>
    FTimeRunWithMILOn: Integer;
    /// <summary>
    ///   Time since trouble codes cleared (Minutes)
    /// </summary>
    FTimeSinceTroubleCodesCleared: Integer;
    /// <summary>
    ///   Max sensor values (Maximum value for Fuel–Air equivalence ratio,
    ///   oxygen sensor voltage, oxygen sensor current, intake manifold absolute pressure,
    ///   and maximum value for air flow rate from mass air flow sensor)
    /// </summary>
    FMaxSensorValues: TOBDServiceMaxSensorValues;
    /// <summary>
    ///   Fuel Type
    /// </summary>
    FFuelType: Byte;
    /// <summary>
    ///   Ethanol Fuel (Percentage)
    /// </summary>
    FEthanolFuel: Double;
    /// <summary>
    ///   Absolute Evap system Vapor Pressure (kPa)
    /// </summary>
    FAbsoluteEvapSystemVaporPressure: Double;
    /// <summary>
    ///   Evap system Vapor Pressure (Pa)
    /// </summary>
    FEvapSystemVaporPressure2: Double;
    /// <summary>
    ///   Short-term Oxygen sensor trim Bank 1 and 3
    /// </summary>
    FShortTermOxygenSensorTrimBank1Bank3: TOBDServiceOxygenSensorTrimBank1Bank3;
    /// <summary>
    ///   Long-term Oxygen sensor trim Bank 1 and 3
    /// </summary>
    FLongTermOxygenSensorTrimBank1Bank3: TOBDServiceOxygenSensorTrimBank1Bank3;
    /// <summary>
    ///   Short-term Oxygen sensor trim Bank 2 and 4
    /// </summary>
    FShortTermOxygenSensorTrimBank2Bank4: TOBDServiceOxygenSensorTrimBank2Bank4;
    /// <summary>
    ///   Long-term Oxygen sensor trim Bank 2 and 4
    /// </summary>
    FLongTermOxygenSensorTrimBank2Bank4: TOBDServiceOxygenSensorTrimBank2Bank4;
    /// <summary>
    ///   Fuel rail absolute pressure
    /// </summary>
    FFuelRailAbsolutePressure: Double;
    /// <summary>
    ///   Relative accelerator pedal position (Percentage)
    /// </summary>
    FRelativeAcceleratorPedalPosition: Double;
    /// <summary>
    ///   Hybrid battery pack remaining life (Percentage)
    /// </summary>
    FHybridBatteryPackRemainingLife: Double;
    /// <summary>
    ///   Engine oil temperature (Degree Celcius)
    /// </summary>
    FEngineOilTemperature: Integer;
    /// <summary>
    ///   Fuel injection timing (Degree to/from Top Dead Center)
    /// </summary>
    FFuelInjectionTiming: Double;
    /// <summary>
    ///   Engine fuel rate (Liters/hour)
    /// </summary>
    FEngineFuelRate: Double;
    /// <summary>
    ///   Emission requirements to which vehicle is designed
    /// </summary>
    FEmissionRequirements: Byte;
    /// <summary>
    ///   Driver's demand engine - percent torque
    /// </summary>
    FDriversDemandEnginePercentTorque: Double;
    /// <summary>
    ///   Actual engine - percent torque
    /// </summary>
    FActualEnginePercentTorque: Double;
    /// <summary>
    ///   Engine reference torque
    /// </summary>
    FEngineReferenceTorque: Integer;
    /// <summary>
    ///   Engine percent torque data
    /// </summary>
    FEnginePercentTorqueData: TOBDServiceEnginePercentTorqueData;
    /// <summary>
    ///   Auxiliary input / output supported
    /// </summary>
    FAuxiliaryInputOutputSupported: TOBDServiceAuxiliaryInputOutputSupported;
    /// <summary>
    ///   Mass air flow sensor
    /// </summary>
    FMassAirFlowSensor: TOBDServiceMassAirFlowSensor;
    /// <summary>
    ///   Engine coolant temperature
    /// </summary>
    FEngineCoolantTemperatureData: TOBDServiceSensorTemperatureData;
    /// <summary>
    ///   Intake air temperature sensor
    /// </summary>
    FEngineIntakeAirTemperatureData: TOBDServiceSensorTemperatureData;

    /// <summary>
    ///   Live data changed event
    /// </summary>
    FOnLiveData: TOBDLiveDataEvent;
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
    ///   Calculated Engine Load
    /// </summary>
    property CalculatedEngineLoad: Double read FCalculatedEngineLoad;
    /// <summary>
    ///   Engine coolant temperature
    /// </summary>
    property EngineCoolantTemperature: Integer read FEngineCoolantTemperature;
    /// <summary>
    ///   Short term fuel trim (Bank 1)
    /// </summary>
    property ShortTermFuelTrimBank1: Double read FShortTermFuelTrimBank1;
    /// <summary>
    ///   Long term fuel trim (Bank 1)
    /// </summary>
    property LongTermFuelTrimBank1: Double read FLongTermFuelTrimBank1;
    /// <summary>
    ///   Short term fuel trim (Bank 2)
    /// </summary>
    property ShortTermFuelTrimBank2: Double read FShortTermFuelTrimBank2;
    /// <summary>
    ///   Long term fuel trim (Bank 1)
    /// </summary>
    property LongTermFuelTrimBank2: Double read FLongTermFuelTrimBank2;
    /// <summary>
    ///   Fuel Pressure
    /// </summary>
    property FuelPressure: Integer read FFuelPressure;
    /// <summary>
    ///   Intake manifold absolute pressure
    /// </summary>
    property IntakeManifoldAbsolutePressure: Integer read FIntakeManifoldAbsolutePressure;
    /// <summary>
    ///   Engine RPM
    /// </summary>
    property EngineRPM: Integer read FEngineRPM;
    /// <summary>
    ///   Vehicle Speed
    /// </summary>
    property VehicleSpeed: Integer read FVehicleSpeed;
    /// <summary>
    ///   Timing Advance
    /// </summary>
    property TimingAdvance: Double read FTimingAdvance;
    /// <summary>
    ///   Intake air temperature
    /// </summary>
    property IntakeAirTemperature: Integer read FIntakeAirTemperature;
    /// <summary>
    ///   Mass air flow rate
    /// </summary>
    property MassAirFlowRate: Double read FMassAirFlowRate;
    /// <summary>
    ///   Throttle position
    /// </summary>
    property ThrottlePosition: Double read FThrottlePosition;
    /// <summary>
    ///   Secondary air status
    /// </summary>
    property CommandedSecondaryAirStatus: TOBDServiceCommandedSecondaryAirStatus read FCommandedSecondaryAirStatus;
    /// <summary>
    ///   Oxygen sensors present in 2 banks
    /// </summary>
    property OxygenSensorPresent2Banks: TOBDServiceOxygenSensorPresent2Banks read FOxygenSensorPresent2Banks;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor1Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor1Bank1VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor2Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor2Bank1VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor3Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor3Bank1VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor4Bank1VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor4Bank1VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor1Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor1Bank2VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor2Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor2Bank2VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor3Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor3Bank2VoltageFuelTrim;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Voltage, Fuel trim percentage)
    /// </summary>
    property OxygenSensor4Bank2VoltageFuelTrim: TOBDServiceOxygenSensorVoltageFuelTrim read FOxygenSensor4Bank2VoltageFuelTrim;
    /// <summary>
    ///   OBD standards this vehicle conforms to
    /// </summary>
    property OBDStandard: Byte read FOBDStandard;
    /// <summary>
    ///   Oxygen sensors present in 4 banks
    /// </summary>
    property OxygenSensorPresent4Banks: TOBDServiceOxygenSensorPresent4Banks read FOxygenSensorPresent4Banks;
    /// <summary>
    ///   Auxilary input status
    /// </summary>
    property AuxilaryInputStatus: TOBDServiceAuxilaryInputStatus read FAuxilaryInputStatus;
    /// <summary>
    ///   Runtime since engine start
    /// </summary>
    property RuntimeSinceEngineStart: Integer read FRuntimeSinceEngineStart;
    /// <summary>
    ///   Distance traveled with MIL (malfunction indicator lamp) on
    /// </summary>
    property DistanceTraveledWithMILOn: Integer read FDistanceTraveledWithMILOn;
    /// <summary>
    ///   Fuel Rail Pressure (relative to manifold vacuum)
    /// </summary>
    property FuelRailPressure: Double read FFuelRailPressure;
    /// <summary>
    ///   Fuel Rail Gauge Pressure (diesel, or gasoline direct injection)
    /// </summary>
    property FuelRailGaugePressure: Double read FFuelRailGaugePressure;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor1Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor1Bank1AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor2Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor2Bank1AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor3Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor3Bank1AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor4Bank1AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor4Bank1AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor1Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor1Bank2AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor2Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor2Bank2AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor3Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor3Bank2AirFuelEQRatioVoltage;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Air-fuel EQ ratio, Voltage)
    /// </summary>
    property OxygenSensor4Bank2AirFuelEQRatioVoltage: TOBDServiceOxygenSensorAirFuelEQRatioVoltage read FOxygenSensor4Bank2AirFuelEQRatioVoltage;
    /// <summary>
    ///   Commanded EGR (Percentage)
    /// </summary>
    property CommandedEGR: Double read FCommandedEGR;
    /// <summary>
    ///   EGR Error (Percentage)
    /// </summary>
    property EGRError: Double read FEGRError;
    /// <summary>
    ///   Commanded evaporative purge (Percentage)
    /// </summary>
    property CommandedEvaportativePurge: Double read FCommandedEvaportativePurge;
    /// <summary>
    ///   Fuel Tank Level Input (Percentage)
    /// </summary>
    property FuelLevel: Double read FFuelLevel;
    /// <summary>
    ///   Warm-ups since codes cleared (0 - 255)
    /// </summary>
    property WarmUpsSinceCodesCleared: Byte read FWarmUpsSinceCodesCleared;
    /// <summary>
    ///   Distance traveled since codes cleared
    /// </summary>
    property DistanceSinceCodesCleared: Integer read FDistanceSinceCodesCleared;
    /// <summary>
    ///   Evap. System Vapor Pressure (Pascal)
    /// </summary>
    property EvapSystemVaporPressure: Double read FEvapSystemVaporPressure;
    /// <summary>
    ///   Absolute Barometric Pressure
    /// </summary>
    property AbsoluteBarometricPressure: Integer read FAbsoluteBarometricPressure;
    /// <summary>
    ///   Oxygen sensor 1 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor1Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor1Bank1AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 2 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor2Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor2Bank1AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 3 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor3Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor3Bank1AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 4 bank 1 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor4Bank1AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor4Bank1AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 1 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor1Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor1Bank2AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 2 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor2Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor2Bank2AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 3 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor3Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor3Bank2AirFuelEQRatioCurrent;
    /// <summary>
    ///   Oxygen sensor 4 bank 2 reading (Air-fuel EQ ratio, Current)
    /// </summary>
    property OxygenSensor4Bank2AirFuelEQRatioCurrent: TOBDServiceOxygenSensorAirFuelEQRatioCurrent read FOxygenSensor4Bank2AirFuelEQRatioCurrent;
    /// <summary>
    ///   Catalyst Temperature: Bank 1, Sensor 1 (Degree Celcius)
    /// </summary>
    property CatalystTemperatureSensor1Bank1: Double read FCatalystTemperatureSensor1Bank1;
    /// <summary>
    ///   Catalyst Temperature: Bank 2, Sensor 1 (Degree Celcius)
    /// </summary>
    property CatalystTemperatureSensor1Bank2: Double read FCatalystTemperatureSensor1Bank2;
    /// <summary>
    ///   Catalyst Temperature: Bank 1, Sensor 2 (Degree Celcius)
    /// </summary>
    property CatalystTemperatureSensor2Bank1: Double read FCatalystTemperatureSensor2Bank1;
    /// <summary>
    ///   Catalyst Temperature: Bank 2, Sensor 2 (Degree Celcius)
    /// </summary>
    property CatalystTemperatureSensor2Bank2: Double read FCatalystTemperatureSensor2Bank2;
    /// <summary>
    ///   Control Module voltage
    /// </summary>
    property ControlModuleVoltage: Double read FControlModuleVoltage;
    /// <summary>
    ///   Absolute load value
    /// </summary>
    property AbsoluteLoadValue: Double read FAbsoluteLoadValue;
    /// <summary>
    ///   Commanded Air-Fuel Equivalence Ratio (Lambda)
    /// </summary>
    property CommandedAirFuelEquivalenceRatio: Double read FCommandedAirFuelEquivalenceRatio;
    /// <summary>
    ///   Relative throttle position (Percentage)
    /// </summary>
    property RelativeThrottlePosition: Double read FRelativeThrottlePosition;
    /// <summary>
    ///   Ambient air temperature (Degree Celcius)
    /// </summary>
    property AmbientAirTemperature: Integer read FAmbientAirTemperature;
    /// <summary>
    ///   Accelerator pedal position D (Percentage)
    /// </summary>
    property AcceleratorPedalPositionD: Double read FAcceleratorPedalPositionD;
    /// <summary>
    ///   Accelerator pedal position E (Percentage)
    /// </summary>
    property AcceleratorPedalPositionE: Double read FAcceleratorPedalPositionE;
    /// <summary>
    ///   Accelerator pedal position F (Percentage)
    /// </summary>
    property AcceleratorPedalPositionF: Double read FAcceleratorPedalPositionF;
    /// <summary>
    ///   Commanded throttle actuator (Percentage)
    /// </summary>
    property CommandedThrottleActuator: Double read FCommandedThrottleActuator;
    /// <summary>
    ///   Time run with MIL on (Minutes)
    /// </summary>
    property TimeRunWithMILOn: Integer read FTimeRunWithMILOn;
    /// <summary>
    ///   Time since trouble codes cleared (Minutes)
    /// </summary>
    property TimeSinceTroubleCodesCleared: Integer read FTimeSinceTroubleCodesCleared;
    /// <summary>
    ///   Max sensor values (Maximum value for Fuel–Air equivalence ratio,
    ///   oxygen sensor voltage, oxygen sensor current, intake manifold absolute pressure,
    ///   and maximum value for air flow rate from mass air flow sensor)
    /// </summary>
    property MaxSensorValues: TOBDServiceMaxSensorValues read FMaxSensorValues;
    /// <summary>
    ///   Fuel Type
    /// </summary>
    property FuelType: Byte read FFuelType;
    /// <summary>
    ///   Ethanol Fuel (Percentage)
    /// </summary>
    property EthanolFuel: Double read FEthanolFuel;
    /// <summary>
    ///   Absolute Evap system Vapor Pressure (kPa)
    /// </summary>
    property AbsoluteEvapSystemVaporPressure: Double read FAbsoluteEvapSystemVaporPressure;
    /// <summary>
    ///   Evap system Vapor Pressure (Pa)
    /// </summary>
    property EvapSystemVaporPressure2: Double read FEvapSystemVaporPressure2;
    /// <summary>
    ///   Short-term Oxygen sensor trim Bank 1 and 3
    /// </summary>
    property ShortTermOxygenSensorTrimBank1Bank3: TOBDServiceOxygenSensorTrimBank1Bank3 read FShortTermOxygenSensorTrimBank1Bank3;
    /// <summary>
    ///   Long-term Oxygen sensor trim Bank 1 and 3
    /// </summary>
    property LongTermOxygenSensorTrimBank1Bank3: TOBDServiceOxygenSensorTrimBank1Bank3 read FLongTermOxygenSensorTrimBank1Bank3;
    /// <summary>
    ///   Short-term Oxygen sensor trim Bank 2 and 4
    /// </summary>
    property ShortTermOxygenSensorTrimBank2Bank4: TOBDServiceOxygenSensorTrimBank2Bank4 read FShortTermOxygenSensorTrimBank2Bank4;
    /// <summary>
    ///   Long-term Oxygen sensor trim Bank 2 and 4
    /// </summary>
    property LongTermOxygenSensorTrimBank2Bank4: TOBDServiceOxygenSensorTrimBank2Bank4 read FLongTermOxygenSensorTrimBank2Bank4;
    /// <summary>
    ///   Fuel rail absolute pressure
    /// </summary>
    property FuelRailAbsolutePressure: Double read FFuelRailAbsolutePressure;
    /// <summary>
    ///   Relative accelerator pedal position (Percentage)
    /// </summary>
    property RelativeAcceleratorPedalPosition: Double read FRelativeAcceleratorPedalPosition;
    /// <summary>
    ///   Hybrid battery pack remaining life (Percentage)
    /// </summary>
    property HybridBatteryPackRemainingLife: Double read FHybridBatteryPackRemainingLife;
    /// <summary>
    ///   Engine oil temperature (Degree Celcius)
    /// </summary>
    property EngineOilTemperature: Integer read FEngineOilTemperature;
    /// <summary>
    ///   Fuel injection timing (Degree to/from Top Dead Center)
    /// </summary>
    property FuelInjectionTiming: Double read FFuelInjectionTiming;
    /// <summary>
    ///   Engine fuel rate (Liters/hour)
    /// </summary>
    property EngineFuelRate: Double read FEngineFuelRate;
    /// <summary>
    ///   Emission requirements to which vehicle is designed
    /// </summary>
    property EmissionRequirements: Byte read FEmissionRequirements;
    /// <summary>
    ///   Driver's demand engine - percent torque
    /// </summary>
    property DriversDemandEnginePercentTorque: Double read FDriversDemandEnginePercentTorque;
    /// <summary>
    ///   Actual engine - percent torque
    /// </summary>
    property ActualEnginePercentTorque: Double read FActualEnginePercentTorque;
    /// <summary>
    ///   Engine reference torque
    /// </summary>
    property EngineReferenceTorque: Integer read FEngineReferenceTorque;
    /// <summary>
    ///   Engine percent torque data
    /// </summary>
    property EnginePercentTorqueData: TOBDServiceEnginePercentTorqueData read FEnginePercentTorqueData;
    /// <summary>
    ///   Auxiliary input / output supported
    /// </summary>
    property AuxiliaryInputOutputSupported: TOBDServiceAuxiliaryInputOutputSupported read FAuxiliaryInputOutputSupported;
    /// <summary>
    ///   Mass air flow sensor
    /// </summary>
    property MassAirFlowSensor: TOBDServiceMassAirFlowSensor read FMassAirFlowSensor;
    /// <summary>
    ///   Engine coolant temperature
    /// </summary>
    property EngineCoolantTemperatureData: TOBDServiceSensorTemperatureData read FEngineCoolantTemperatureData;
    /// <summary>
    ///   Intake air temperature sensor
    /// </summary>
    property EngineIntakeAirTemperatureData: TOBDServiceSensorTemperatureData read FEngineIntakeAirTemperatureData;

    /// <summary>
    ///   Live data changed event
    /// </summary>
    property OnLiveData: TOBDLiveDataEvent read FOnLiveData write FOnLiveData;
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
// SERVICE 01: OXYGEN SENSOR PRESENT 2 BANKS - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorPresent2Banks.Reset;
begin
  FBank1Sensor1 := False;
  FBank1Sensor2 := False;
  FBank1Sensor3 := False;
  FBank1Sensor4 := False;
  FBank2Sensor1 := False;
  FBank2Sensor2 := False;
  FBank2Sensor3 := False;
  FBank2Sensor4 := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR READING (VOLTAGE, FUEL TRIM) - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorVoltageFuelTrim.Reset;
begin
  FVoltage := 0;
  FFuelTrim := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR PRESENT 4 BANKS - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorPresent4Banks.Reset;
begin
  FBank1Sensor1 := False;
  FBank1Sensor2 := False;
  FBank2Sensor1 := False;
  FBank2Sensor2 := False;
  FBank3Sensor1 := False;
  FBank3Sensor2 := False;
  FBank4Sensor1 := False;
  FBank4Sensor2 := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR READING (AIR FUEL EQUIVALENCE RATIO, VOLTAGE)  - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Reset;
begin
  FAirFuelEquivalenceRatio := 0;
  FVoltage := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR READING (AIR FUEL EQUIVALENCE RATIO, CURRENT)  - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Reset;
begin
  FAirFuelEquivalenceRatio := 0;
  FCurrent := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: MAXIMUM SENSOR VALUES - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceMaxSensorValues.Reset;
begin
  FAirFuelEquivalenceRatio := 0;
  FOxygenSensorVoltage := 0;
  FOxygenSensorCurrent := 0;
  FIntakeManifoldPressure := 0;
  FAirFlowRate := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR TRIM - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorTrimBank1Bank3.Reset;
begin
  FBank1 := 0;
  FBank3 := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: OXYGEN SENSOR TRIM - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceOxygenSensorTrimBank2Bank4.Reset;
begin
  FBank2 := 0;
  FBank4 := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: ENGINE PERCENT TORQUE DATA - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceEnginePercentTorqueData.Reset;
begin
  FIdle := 0;
  FPoint1 := 0;
  FPoint2 := 0;
  FPoint3 := 0;
  FPoint4 := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: AUQILIARY INPUT OUTPUT SUPPORTED - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceAuxiliaryInputOutputSupported.Reset;
begin
  FInput1 := False;
  FOutput1 := False;
  FInput2 := False;
  FOutput2 := False;
  FInput3 := False;
  FOutput3 := False;
  FInput4 := False;
  FOutput4 := False;
  FInput5 := False;
  FOutput5 := False;
  FInput6 := False;
  FOutput6 := False;
  FInput7 := False;
  FOutput7 := False;
  FInput8 := False;
  FOutput8 := False;
end;

//------------------------------------------------------------------------------
// SERVICE 01: MASS AIR FLOW SENSOR - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceMassAirFlowSensor.Reset;
begin
  FSensorASupported := False;
  FSensorBSupported := False;
  FSensorARate := 0;
  FSensorBRate := 0;
end;

//------------------------------------------------------------------------------
// SERVICE 01: SENSOR TEMPERATURE DATA - RESET
//------------------------------------------------------------------------------
procedure TOBDService01.TOBDServiceSensorTemperatureData.Reset;
begin
  FSensorASupported := False;
  FSensorBSupported := False;
  FSensorATemperature := 0;
  FSensorBTemperature := 0;
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
  // Create oxygen sensors present in 2 banks
  FOxygenSensorPresent2Banks := TOBDServiceOxygenSensorPresent2Banks.Create;
  // Create oxygen sensors voltage and fuel trim
  FOxygenSensor1Bank1VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor2Bank1VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor3Bank1VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor4Bank1VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor1Bank2VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor2Bank2VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor3Bank2VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  FOxygenSensor4Bank2VoltageFuelTrim := TOBDServiceOxygenSensorVoltageFuelTrim.Create;
  // Create oxygen sensors present in 4 banks
  FOxygenSensorPresent4Banks := TOBDServiceOxygenSensorPresent4Banks.Create;
  // Create oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor2Bank1AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor3Bank1AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor4Bank1AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor1Bank2AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor2Bank2AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor3Bank2AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  FOxygenSensor4Bank2AirFuelEQRatioVoltage := TOBDServiceOxygenSensorAirFuelEQRatioVoltage.Create;
  // Create oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor2Bank1AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor3Bank1AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor4Bank1AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor1Bank2AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor2Bank2AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor3Bank2AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  FOxygenSensor4Bank2AirFuelEQRatioCurrent := TOBDServiceOxygenSensorAirFuelEQRatioCurrent.Create;
  // Create maximum sensor values
  FMaxSensorValues := TOBDServiceMaxSensorValues.Create;
  // Create oxygen sensors trim
  FShortTermOxygenSensorTrimBank1Bank3 := TOBDServiceOxygenSensorTrimBank1Bank3.Create;
  FLongTermOxygenSensorTrimBank1Bank3 := TOBDServiceOxygenSensorTrimBank1Bank3.Create;
  FShortTermOxygenSensorTrimBank2Bank4 := TOBDServiceOxygenSensorTrimBank2Bank4.Create;
  FLongTermOxygenSensorTrimBank2Bank4 := TOBDServiceOxygenSensorTrimBank2Bank4.Create;
  // Create engine percent torque data
  FEnginePercentTorqueData := TOBDServiceEnginePercentTorqueData.Create;
  // Create auxiliary input / output supported
  FAuxiliaryInputOutputSupported := TOBDServiceAuxiliaryInputOutputSupported.Create;
  // Create mass air flow sensor
  FMassAirFlowSensor := TOBDServiceMassAirFlowSensor.Create;
  // Create engine coolant temperature data
  FEngineCoolantTemperatureData := TOBDServiceSensorTemperatureData.Create;
  // Create intake air temperature sensor data
  FEngineIntakeAirTemperatureData := TOBDServiceSensorTemperatureData.Create;
  // Clear all data and set to defaults
  Reset;
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
  // Free oxygen sensors present in 2 banks
  FOxygenSensorPresent2Banks.Free;
  // Free oxygen sensors voltage and fuel trim
  FOxygenSensor1Bank1VoltageFuelTrim.Free;
  FOxygenSensor2Bank1VoltageFuelTrim.Free;
  FOxygenSensor3Bank1VoltageFuelTrim.Free;
  FOxygenSensor4Bank1VoltageFuelTrim.Free;
  FOxygenSensor1Bank2VoltageFuelTrim.Free;
  FOxygenSensor2Bank2VoltageFuelTrim.Free;
  FOxygenSensor3Bank2VoltageFuelTrim.Free;
  FOxygenSensor4Bank2VoltageFuelTrim.Free;
  // Free oxygen sensors present in 4 banks
  FOxygenSensorPresent4Banks.Free;
  // Free oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioVoltage.Free;
  FOxygenSensor2Bank1AirFuelEQRatioVoltage.Free;
  FOxygenSensor3Bank1AirFuelEQRatioVoltage.Free;
  FOxygenSensor4Bank1AirFuelEQRatioVoltage.Free;
  FOxygenSensor1Bank2AirFuelEQRatioVoltage.Free;
  FOxygenSensor2Bank2AirFuelEQRatioVoltage.Free;
  FOxygenSensor3Bank2AirFuelEQRatioVoltage.Free;
  FOxygenSensor4Bank2AirFuelEQRatioVoltage.Free;
  // Free oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioCurrent.Free;
  FOxygenSensor2Bank1AirFuelEQRatioCurrent.Free;
  FOxygenSensor3Bank1AirFuelEQRatioCurrent.Free;
  FOxygenSensor4Bank1AirFuelEQRatioCurrent.Free;
  FOxygenSensor1Bank2AirFuelEQRatioCurrent.Free;
  FOxygenSensor2Bank2AirFuelEQRatioCurrent.Free;
  FOxygenSensor3Bank2AirFuelEQRatioCurrent.Free;
  FOxygenSensor4Bank2AirFuelEQRatioCurrent.Free;
  // Free maximum sensor values
  FMaxSensorValues.Free;
  // Free oxygen sensors trim
  FShortTermOxygenSensorTrimBank1Bank3.Free;
  FLongTermOxygenSensorTrimBank1Bank3.Free;
  FShortTermOxygenSensorTrimBank2Bank4.Free;
  FLongTermOxygenSensorTrimBank2Bank4.Free;
  // Free engine percent torque data
  FEnginePercentTorqueData.Free;
  // Free auxiliary input / output supported
  FAuxiliaryInputOutputSupported.Free;
  // Free mass air flow sensor
  FMassAirFlowSensor.Free;
  // Create engine coolant temperature data
  FEngineCoolantTemperatureData.Free;
  // Create intake air temperature sensor data
  FEngineIntakeAirTemperatureData.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// SERVICE 01: RESET - CLEAR ALL DATA
//------------------------------------------------------------------------------
procedure TOBDService01.Reset;
begin
  // Reset supported PID's
  SetLength(FSupportedPID, 0);
  // Reset MIL status
  FMIL := False;
  // Reset DTC count
  FDTC := 0;
  // Reset common tests
  FCommonTest.Reset;
  // Reset engine type
  FEngineType := etUnknown;
  // Reset engine tests
  FSparkEngineTest.Reset;
  FCompressionEngineTest.Reset;
  // Reset fuel systems status
  FFuelSystem1Status := fsUnknown;
  FFuelSystem2Status := fsUnknown;
  // Reset calculated engine load
  FCalculatedEngineLoad := 0;
  // Reset engine coolant temperature
  FEngineCoolantTemperature := 0;
  // Reset Short term fuel trim (Bank 1)
  FShortTermFuelTrimBank1 := 0;
  // Reset Long term fuel trim (Bank 1)
  FLongTermFuelTrimBank1 := 0;
  // Reset Short term fuel trim (Bank 2)
  FShortTermFuelTrimBank2 := 0;
  // Reset Long term fuel trim (Bank 1)
  FLongTermFuelTrimBank2 := 0;
  // Reset fuel pressure
  FFuelPressure := 0;
  // Reset intake manifold absolute pressure
  FIntakeManifoldAbsolutePressure := 0;
  // Reset engine rpm
  FEngineRPM := 0;
  // Reset vehicle speed
  FVehicleSpeed := 0;
  // Reset timing advance
  FTimingAdvance := 0;
  // Reset intake air temperature
  FIntakeAirTemperature := 0;
  // Reset mass air flow rate
  FMassAirFlowRate := 0;
  // Reset throttle position
  FThrottlePosition := 0;
  // Reset commanded secondary air status
  FCommandedSecondaryAirStatus := sasUnknown;
  // Reset oxygen sensors present in 2 banks
  FOxygenSensorPresent2Banks.Reset;
  // Reset oxygen sensors reading
  FOxygenSensor1Bank1VoltageFuelTrim.Reset;
  FOxygenSensor2Bank1VoltageFuelTrim.Reset;
  FOxygenSensor3Bank1VoltageFuelTrim.Reset;
  FOxygenSensor4Bank1VoltageFuelTrim.Reset;
  FOxygenSensor1Bank2VoltageFuelTrim.Reset;
  FOxygenSensor2Bank2VoltageFuelTrim.Reset;
  FOxygenSensor3Bank2VoltageFuelTrim.Reset;
  FOxygenSensor4Bank2VoltageFuelTrim.Reset;
  // Reset OBD Standard
  FOBDStandard := $00;
  // Reset oxygen sensors present in 4 banks
  FOxygenSensorPresent4Banks.Reset;
  // Reset auxilary input status
  FAuxilaryInputStatus := aisUnknown;
  // Reset runtime since engine start
  FRuntimeSinceEngineStart := 0;
  // Reset distance traveled with MIL on
  FDistanceTraveledWithMILOn := 0;
  // Reset fuel rail pressure
  FFuelRailPressure := 0;
  // Reset fuel rail gauge pressure
  FFuelRailGaugePressure := 0;
  // Reset oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioVoltage.Reset;
  FOxygenSensor2Bank1AirFuelEQRatioVoltage.Reset;
  FOxygenSensor3Bank1AirFuelEQRatioVoltage.Reset;
  FOxygenSensor4Bank1AirFuelEQRatioVoltage.Reset;
  FOxygenSensor1Bank2AirFuelEQRatioVoltage.Reset;
  FOxygenSensor2Bank2AirFuelEQRatioVoltage.Reset;
  FOxygenSensor3Bank2AirFuelEQRatioVoltage.Reset;
  FOxygenSensor4Bank2AirFuelEQRatioVoltage.Reset;
  // Reset commanded EGR
  FCommandedEGR := 0;
  // Reset EGR error
  FEGRError := 0;
  // Reset commanded evaporative purge
  FCommandedEvaportativePurge := 0;
  // Reset fuel level
  FFuelLevel := 0;
  // Reset warm-ups since codes cleared
  FWarmUpsSinceCodesCleared := 0;
  // Reset distance since codes cleared
  FDistanceSinceCodesCleared := 0;
  // Reset evap system vapor pressure
  FEvapSystemVaporPressure := 0;
  // Reset absolute barometric pressure
  FAbsoluteBarometricPressure := 0;
  // Reset the oxygen sensors reading
  FOxygenSensor1Bank1AirFuelEQRatioCurrent.Reset;
  FOxygenSensor2Bank1AirFuelEQRatioCurrent.Reset;
  FOxygenSensor3Bank1AirFuelEQRatioCurrent.Reset;
  FOxygenSensor4Bank1AirFuelEQRatioCurrent.Reset;
  FOxygenSensor1Bank2AirFuelEQRatioCurrent.Reset;
  FOxygenSensor2Bank2AirFuelEQRatioCurrent.Reset;
  FOxygenSensor3Bank2AirFuelEQRatioCurrent.Reset;
  FOxygenSensor4Bank2AirFuelEQRatioCurrent.Reset;
  // Reset Catalyst Temperatures
  FCatalystTemperatureSensor1Bank1 := 0;
  FCatalystTemperatureSensor1Bank2 := 0;
  FCatalystTemperatureSensor2Bank1 := 0;
  FCatalystTemperatureSensor2Bank2 := 0;
  // Reset the control module voltage
  FControlModuleVoltage := 0;
  // Reset absolute load value
  FAbsoluteLoadValue := 0;
  // Reset commanded air-fuel equivalence ratio
  FCommandedAirFuelEquivalenceRatio := 0;
  // Reset relative throttle position
  FRelativeThrottlePosition := 0;
  // Reset ambient air temperature
  FAmbientAirTemperature := 0;
  // Reset absolute throttle position B
  FAbsoluteThrottlePositionB := 0;
  // Reset absolute throttle position C
  FAbsoluteThrottlePositionC := 0;
  // Reset accelerator pedal position D
  FAcceleratorPedalPositionD := 0;
  // Reset accelerator pedal position E
  FAcceleratorPedalPositionE := 0;
  // Reset accelerator pedal position F
  FAcceleratorPedalPositionF := 0;
  // Reset commanded throttle actuator
  FCommandedThrottleActuator := 0;
  // Reset time run with MIL on
  FTimeRunWithMILOn := 0;
  // Reset time since trouble codes cleared
  FTimeSinceTroubleCodesCleared := 0;
  // Reset maximum sensor values
  FMaxSensorValues.Reset;
  // Reset fuel-type
  FFuelType := $00;
  // Reset ethanol fuel percentage
  FEthanolFuel := 0;
  // Reset absolute evap system vapor pressure
  FAbsoluteEvapSystemVaporPressure := 0;
  // Reset evap system vapor pressure 2
  FEvapSystemVaporPressure2 := 0;
  // Reset oxygen sensors trim
  FShortTermOxygenSensorTrimBank1Bank3.Reset;
  FLongTermOxygenSensorTrimBank1Bank3.Reset;
  FShortTermOxygenSensorTrimBank2Bank4.Reset;
  FLongTermOxygenSensorTrimBank2Bank4.Reset;
  // Reset fuel rail absolute pressure
  FFuelRailAbsolutePressure := 0;
  // Reset relative accelerator pedal position
  FRelativeAcceleratorPedalPosition := 0;
  // Reset hybrid battery pack remaining life
  FHybridBatteryPackRemainingLife := 0;
  // Reset engine oil temperature
  FEngineOilTemperature := 0;
  // Reset fuel injection timing
  FFuelInjectionTiming := 0;
  // Reset engine fuel rate
  FEngineFuelRate := 0;
  // Reset emmision requirements
  FEmissionRequirements := $00;
  // Reset drivers demand engine percent torque
  FDriversDemandEnginePercentTorque := 0;
  // Reset actual engine percent torque
  FActualEnginePercentTorque := 0;
  // Reset engine reference torque
  FEngineReferenceTorque := 0;
  // Reset engine percent torque data
  FEnginePercentTorqueData.Reset;
  // Reset auxiliary input / output supported
  FAuxiliaryInputOutputSupported.Reset;
  // Reset mass airflow sensor data
  FMassAirFlowSensor.Reset;
  // Reset engine coolant temperature data
  FEngineCoolantTemperatureData.Reset;
  // Reset intake air temperature sensor data
  FEngineIntakeAirTemperatureData.Reset;
end;

//------------------------------------------------------------------------------
// SERVICE 01: PARSE RESPONSE
//------------------------------------------------------------------------------
procedure TOBDService01.ParseResponse(Response: TBytes);
var
  ServiceDecoder: IOBDServiceResponseDecoder;
  ErrorDecoder: IOBDResponseDecoder;
  ResponseDecoder: TOBDResponseDecoder;
  Error: Boolean;
  ServiceID, ParameterID, E: Byte;
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
  if ServiceID <> OBD_SERVICE_01 then Exit;

  // Parse supported PID's
  if ParameterID in [
    OBD_SERVICE_01_SUPPORTED_PID_0120,
    OBD_SERVICE_01_SUPPORTED_PID_2140,
    OBD_SERVICE_01_SUPPORTED_PID_4160,
    OBD_SERVICE_01_SUPPORTED_PID_6180,
    OBD_SERVICE_01_SUPPORTED_PID_81A0,
    OBD_SERVICE_01_SUPPORTED_PID_A1C0,
    OBD_SERVICE_01_SUPPORTED_PID_C1E0
  ] then
  begin
    ParseSupportedPID(ParameterID, Data);
    Exit;
  end;

  // Parse monitor status since DTC's cleared (PID 01)
  if ParameterID = OBD_SERVICE_01_MONITOR_STATUS then
  begin
    ParseMonitorStatus(Data);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MONITOR_STATUS);
    Exit;
  end;

  // Parse fuel system status (PID 03)
  if ParameterID = OBD_SERVICE_01_FUEL_SYSTEM_STATUS then
  begin
    ResponseDecoder := TOBDFuelSystemStatusDecoder.Create;
    (ResponseDecoder as TOBDFuelSystemStatusDecoder).Parse(Data, FFuelSystem1Status, FFuelSystem2Status);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_SYSTEM_STATUS);
    Exit;
  end;

  // Parse engine load (PID 04)
  if ParameterID = OBD_SERVICE_01_CALCULATED_ENGINE_LOAD then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FCalculatedEngineLoad);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CALCULATED_ENGINE_LOAD);
    Exit;
  end;

  // Parse engine coolant temperature (PID 05)
  if ParameterID = OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE then
  begin
    ResponseDecoder := TOBDTemperatureDecoder.Create;
    (ResponseDecoder as TOBDTemperatureDecoder).Parse(Data, FEngineCoolantTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE);
    Exit;
  end;

  // Parse short term fuel-trim bank 1 (PID 06)
  if ParameterID = OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK1 then
  begin
    ResponseDecoder := TOBDFuelTrimDecoder.Create;
    (ResponseDecoder as TOBDFuelTrimDecoder).Parse(Data, FShortTermFuelTrimBank1);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK1);
    Exit;
  end;

  // Parse long term fuel-trim bank 1 (PID 07)
  if ParameterID = OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK1 then
  begin
    ResponseDecoder := TOBDFuelTrimDecoder.Create;
    (ResponseDecoder as TOBDFuelTrimDecoder).Parse(Data, FLongTermFuelTrimBank1);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK1);
    Exit;
  end;

  // Parse short term fuel-trim bank 2 (PID 08)
  if ParameterID = OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK2 then
  begin
    ResponseDecoder := TOBDFuelTrimDecoder.Create;
    (ResponseDecoder as TOBDFuelTrimDecoder).Parse(Data, FShortTermFuelTrimBank2);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK2);
    Exit;
  end;

  // Parse long term fuel-trim bank 2 (PID 09)
  if ParameterID = OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK2 then
  begin
    ResponseDecoder := TOBDFuelTrimDecoder.Create;
    (ResponseDecoder as TOBDFuelTrimDecoder).Parse(Data, FLongTermFuelTrimBank2);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK2);
    Exit;
  end;

  // Parse fuelm pressure (PID 0A)
  if ParameterID = OBD_SERVICE_01_FUEL_PRESSURE then
  begin
    ResponseDecoder := TOBDFuelPressureDecoder.Create;
    (ResponseDecoder as TOBDFuelPressureDecoder).Parse(Data, FFuelPressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_PRESSURE);
    Exit;
  end;

  // Parse intake manifold absolute pressure (PID 0B)
  if ParameterID = OBD_SERVICE_01_INTAKE_MANIFOLD_PRESSURE then
  begin
    FIntakeManifoldAbsolutePressure := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_INTAKE_MANIFOLD_PRESSURE);
    Exit;
  end;

  // Parse engine RPM (PID 0C)
  if ParameterID = OBD_SERVICE_01_ENGINE_RPM then
  begin
    ResponseDecoder := TOBDEngineRPMDecoder.Create;
    (ResponseDecoder as TOBDEngineRPMDecoder).Parse(Data, FEngineRPM);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_RPM);
    Exit;
  end;

  // Parse vehicle speed (PID 0D)
  if ParameterID = OBD_SERVICE_01_VEHICLE_SPEED then
  begin
    FVehicleSpeed := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_VEHICLE_SPEED);
    Exit;
  end;

  // Parse timing advance (PID 0E)
  if ParameterID = OBD_SERVICE_01_TIMING_ADVANCE then
  begin
    ResponseDecoder := TOBDTimingAdvanceDecoder.Create;
    (ResponseDecoder as TOBDTimingAdvanceDecoder).Parse(Data, FTimingAdvance);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_TIMING_ADVANCE);
    Exit;
  end;

  // Parse intake air temperature (PID 0F)
  if ParameterID = OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE then
  begin
    ResponseDecoder := TOBDTemperatureDecoder.Create;
    (ResponseDecoder as TOBDTemperatureDecoder).Parse(Data, FIntakeAirTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE);
    Exit;
  end;

  // Parse mass air flow rate (PID 10)
  if ParameterID = OBD_SERVICE_01_MAF_AIR_FLOW_RATE then
  begin
    ResponseDecoder := TOBDMassAirFlowRateDecoder.Create;
    (ResponseDecoder as TOBDMassAirFlowRateDecoder).Parse(Data, FMassAirFlowRate);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MAF_AIR_FLOW_RATE);
    Exit;
  end;

  // Parse throttle position (PID 11)
  if ParameterID = OBD_SERVICE_01_THROTTLE_POSITION then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FThrottlePosition);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_THROTTLE_POSITION);
    Exit;
  end;

  // Parse commanded secondary air status (PID 12)
  if ParameterID = OBD_SERVICE_01_COMMANDED_SECONDARY_AIR_STATUS then
  begin
    ResponseDecoder := TOBDCommandedSecondaryAirStatusDecoder.Create;
    (ResponseDecoder as TOBDCommandedSecondaryAirStatusDecoder).Parse(Data, FCommandedSecondaryAirStatus);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_COMMANDED_SECONDARY_AIR_STATUS);
    Exit;
  end;

  // Parse oxygen sensors present in 2 banks (PID 13)
  if ParameterID = OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_1 then
  begin
    if Length(Data) < 1 then Exit;
    FOxygenSensorPresent2Banks.FBank1Sensor1 := (Data[0] and $01) <> 0;
    FOxygenSensorPresent2Banks.FBank1Sensor2 := (Data[0] and $02) <> 0;
    FOxygenSensorPresent2Banks.FBank1Sensor3 := (Data[0] and $04) <> 0;
    FOxygenSensorPresent2Banks.FBank1Sensor4 := (Data[0] and $08) <> 0;
    FOxygenSensorPresent2Banks.FBank2Sensor1 := (Data[0] and $10) <> 0;
    FOxygenSensorPresent2Banks.FBank2Sensor2 := (Data[0] and $20) <> 0;
    FOxygenSensorPresent2Banks.FBank2Sensor3 := (Data[0] and $40) <> 0;
    FOxygenSensorPresent2Banks.FBank2Sensor4 := (Data[0] and $80) <> 0;
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_1);
    Exit;
  end;

  // Parse oxygen sensor 1 bank 1 reading (PID 14)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR1_BANK1 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor1Bank1VoltageFuelTrim.FVoltage, FOxygenSensor1Bank1VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR1_BANK1);
    Exit;
  end;

  // Parse oxygen sensor 2 bank 1 reading (PID 15)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR2_BANK1 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor2Bank1VoltageFuelTrim.FVoltage, FOxygenSensor2Bank1VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR2_BANK1);
    Exit;
  end;

  // Parse oxygen sensor 3 bank 1 reading (PID 16)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR3_BANK1 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor3Bank1VoltageFuelTrim.FVoltage, FOxygenSensor3Bank1VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR3_BANK1);
    Exit;
  end;

  // Parse oxygen sensor 4 bank 1 reading (PID 17)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR4_BANK1 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor4Bank1VoltageFuelTrim.FVoltage, FOxygenSensor4Bank1VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR4_BANK1);
    Exit;
  end;

  // Parse oxygen sensor 1 bank 2 reading (PID 18)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR1_BANK2 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor1Bank2VoltageFuelTrim.FVoltage, FOxygenSensor1Bank2VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR1_BANK2);
    Exit;
  end;

  // Parse oxygen sensor 2 bank 2 reading (PID 19)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR2_BANK2 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor2Bank2VoltageFuelTrim.FVoltage, FOxygenSensor2Bank2VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR2_BANK2);
    Exit;
  end;

  // Parse oxygen sensor 3 bank 2 reading (PID 1A)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR3_BANK2 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor3Bank2VoltageFuelTrim.FVoltage, FOxygenSensor3Bank2VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR3_BANK2);
    Exit;
  end;

  // Parse oxygen sensor 4 bank 2 reading (PID 1B)
  if ParameterID = OBD_SERVICE_01_O2_SENSOR4_BANK2 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingDecoder).Parse(Data, FOxygenSensor4Bank2VoltageFuelTrim.FVoltage, FOxygenSensor4Bank2VoltageFuelTrim.FFuelTrim);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2_SENSOR4_BANK2);
    Exit;
  end;

  // Parse OBD standard this vehicle conforms to (PID 1C)
  if ParameterID = OBD_SERVICE_01_OBD_STANDARDS then
  begin
    if Length(Data) < 1 then Exit;
    FOBDStandard := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_OBD_STANDARDS);
    Exit;
  end;

  // Parse oxygen sensors present in 4 banks (PID 1D)
  if ParameterID = OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_2 then
  begin
    if Length(Data) < 1 then Exit;
    FOxygenSensorPresent4Banks.FBank1Sensor1 := (Data[0] and $01) <> 0;
    FOxygenSensorPresent4Banks.FBank1Sensor2 := (Data[0] and $02) <> 0;
    FOxygenSensorPresent4Banks.FBank2Sensor1 := (Data[0] and $04) <> 0;
    FOxygenSensorPresent4Banks.FBank2Sensor2 := (Data[0] and $08) <> 0;
    FOxygenSensorPresent4Banks.FBank3Sensor1 := (Data[0] and $10) <> 0;
    FOxygenSensorPresent4Banks.FBank3Sensor2 := (Data[0] and $20) <> 0;
    FOxygenSensorPresent4Banks.FBank4Sensor1 := (Data[0] and $40) <> 0;
    FOxygenSensorPresent4Banks.FBank4Sensor2 := (Data[0] and $80) <> 0;
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_2);
    Exit;
  end;

  // Parse auxilary input status (PID 1E)
  if ParameterID = OBD_SERVICE_01_AUXILIARY_INPUT_STATUS then
  begin
    if Length(Data) < 1 then
    begin
      FAuxilaryInputStatus := aisUnknown;
      Exit;
    end;
    if (Data[0] and $01) <> 0 then
      FAuxilaryInputStatus := aisActive
    else
      FAuxilaryInputStatus := aisPowerTakeOff;
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_AUXILIARY_INPUT_STATUS);
    Exit;
  end;

  // Parse engine runtime (PID 1F)
  if ParameterID = OBD_SERVICE_01_ENGINE_RUN_TIME then
  begin
    ResponseDecoder := TOBDServiceEngineRuntimeDecoder.Create;
    (ResponseDecoder as TOBDServiceEngineRuntimeDecoder).Parse(Data, FRuntimeSinceEngineStart);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_RUN_TIME);
    Exit;
  end;

  // Parse distance traveled with MIL on (PID 21)
  if ParameterID = OBD_SERVICE_01_DISTANCE_TRAVELED_MIL_ON then
  begin
    ResponseDecoder := TOBDServiceDistanceTraveledWithMILOnDecoder.Create;
    (ResponseDecoder as TOBDServiceDistanceTraveledWithMILOnDecoder).Parse(Data, FDistanceTraveledWithMILOn);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_DISTANCE_TRAVELED_MIL_ON);
    Exit;
  end;

  // Parse Fuel Rail Pressure (relative to manifold vacuum) (PID 22)
  if ParameterID = OBD_SERVICE_01_FUEL_RAIL_PRESSURE_VACUUM then
  begin
    ResponseDecoder := TOBDServiceFuelRailPressureDecoder.Create;
    (ResponseDecoder as TOBDServiceFuelRailPressureDecoder).Parse(Data, FFuelRailPressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_RAIL_PRESSURE_VACUUM);
    Exit;
  end;

  // Parse Fuel Rail Gauge Pressure (diesel, or gasoline direct injection) (PID 23)
  if ParameterID = OBD_SERVICE_01_FUEL_RAIL_PRESSURE_DIRECT then
  begin
    ResponseDecoder := TOBDServiceFuelRailGaugePressureDecoder.Create;
    (ResponseDecoder as TOBDServiceFuelRailGaugePressureDecoder).Parse(Data, FFuelRailGaugePressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_RAIL_PRESSURE_DIRECT);
    Exit;
  end;

  // Parse Oxygen Sensor 1 Bank 1 reading (PID 24)
  if ParameterID = OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor1Bank1AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor1Bank1AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 2 Bank 1 reading (PID 25)
  if ParameterID = OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor2Bank1AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor2Bank1AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 3 Bank 1 reading (PID 26)
  if ParameterID = OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor3Bank1AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor3Bank1AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 4 Bank 1 reading (PID 27)
  if ParameterID = OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor4Bank1AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor4Bank1AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 1 Bank 2 reading (PID 28)
  if ParameterID = OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor1Bank2AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor1Bank2AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 2 Bank 2 reading (PID 29)
  if ParameterID = OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor2Bank2AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor2Bank2AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 3 Bank 2 reading (PID 2A)
  if ParameterID = OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor3Bank2AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor3Bank2AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse Oxygen Sensor 4 Bank 2 reading (PID 2B)
  if ParameterID = OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRVDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRVDecoder).Parse(Data, FOxygenSensor4Bank2AirFuelEQRatioVoltage.FAirFuelEquivalenceRatio, FOxygenSensor4Bank2AirFuelEQRatioVoltage.FVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE);
    Exit;
  end;

  // Parse commanded EGR (PID 2C)
  if ParameterID = OBD_SERVICE_01_COMMANDED_EGR then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FCommandedEGR);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_COMMANDED_EGR);
    Exit;
  end;

  // Parse EGR Error (PID 2D)
  if ParameterID = OBD_SERVICE_01_EGR_ERROR then
  begin
    ResponseDecoder := TOBDServiceEGRErrorDecoder.Create;
    (ResponseDecoder as TOBDServiceEGRErrorDecoder).Parse(Data, FEGRError);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_EGR_ERROR);
    Exit;
  end;

  // Parse Commanded Evaportative Purge (PID 2E)
  if ParameterID = OBD_SERVICE_01_COMMANDED_EVAPORATIVE_PURGE then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FCommandedEvaportativePurge);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_COMMANDED_EVAPORATIVE_PURGE);
    Exit;
  end;

  // Parse Fuel Level Input (PID 2F)
  if ParameterID = OBD_SERVICE_01_FUEL_LEVEL_INPUT then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FFuelLevel);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_LEVEL_INPUT);
    Exit;
  end;

  // Parse Warm-ups since codes cleared (PID 30)
  if ParameterID = OBD_SERVICE_01_WARMUPS_SINCE_CODES_CLEARED then
  begin
    if Length(Data) < 1 then Exit;
    FWarmUpsSinceCodesCleared := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_WARMUPS_SINCE_CODES_CLEARED);
    Exit;
  end;

  // Parse Distance traveled since codes cleared (PID 31)
  if ParameterID = OBD_SERVICE_01_DISTANCE_TRAVELED_CODES_CLEARED then
  begin
    ResponseDecoder := TOBDServiceDistanceSinceCodesClearedDecoder.Create;
    (ResponseDecoder as TOBDServiceDistanceSinceCodesClearedDecoder).Parse(Data, FDistanceSinceCodesCleared);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_DISTANCE_TRAVELED_CODES_CLEARED);
    Exit;
  end;

  // Parse Evap. System Vapor Pressure (PID 32)
  if ParameterID = OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE then
  begin
    ResponseDecoder := TOBDServiceEvapSystemVaporPressureDecoder.Create;
    (ResponseDecoder as TOBDServiceEvapSystemVaporPressureDecoder).Parse(Data, FEvapSystemVaporPressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE);
    Exit;
  end;

  // Parse Absolute Barometric Pressure (PID 33)
  if ParameterID = OBD_SERVICE_01_BAROMETRIC_PRESSURE then
  begin
    if Length(Data) < 1 then Exit;
    FAbsoluteBarometricPressure := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_BAROMETRIC_PRESSURE);
    Exit;
  end;

  // Parse Oxygen Sensor 1 Bank 1 reading (PID 34)
  if ParameterID = OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor1Bank1AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor1Bank1AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 2 Bank 1 reading (PID 35)
  if ParameterID = OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor2Bank1AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor2Bank1AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 3 Bank 1 reading (PID 36)
  if ParameterID = OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor3Bank1AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor3Bank1AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 4 Bank 1 reading (PID 37)
  if ParameterID = OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor4Bank1AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor4Bank1AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 1 Bank 2 reading (PID 38)
  if ParameterID = OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor1Bank2AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor1Bank2AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 2 Bank 2 reading (PID 39)
  if ParameterID = OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor2Bank2AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor2Bank2AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 3 Bank 2 reading (PID 3A)
  if ParameterID = OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor3Bank2AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor3Bank2AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Oxygen Sensor 4 Bank 2 reading (PID 3B)
  if ParameterID = OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_CURRENT then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorReadingRCDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorReadingRCDecoder).Parse(Data, FOxygenSensor4Bank2AirFuelEQRatioCurrent.FAirFuelEquivalenceRatio, FOxygenSensor4Bank2AirFuelEQRatioCurrent.FCurrent);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_CURRENT);
    Exit;
  end;

  // Parse Catalyst Temperature Bank 1, Sensor 1 (PID 3C)
  if ParameterID = OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S1 then
  begin
    ResponseDecoder := TOBDServiceCatalystTemperatureDecoder.Create;
    (ResponseDecoder as TOBDServiceCatalystTemperatureDecoder).Parse(Data, FCatalystTemperatureSensor1Bank1);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S1);
    Exit;
  end;

  // Parse Catalyst Temperature Bank 2, Sensor 1 (PID 3D)
  if ParameterID = OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S1 then
  begin
    ResponseDecoder := TOBDServiceCatalystTemperatureDecoder.Create;
    (ResponseDecoder as TOBDServiceCatalystTemperatureDecoder).Parse(Data, FCatalystTemperatureSensor1Bank2);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S1);
    Exit;
  end;

  // Parse Catalyst Temperature Bank 1, Sensor 2 (PID 3E)
  if ParameterID = OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S2 then
  begin
    ResponseDecoder := TOBDServiceCatalystTemperatureDecoder.Create;
    (ResponseDecoder as TOBDServiceCatalystTemperatureDecoder).Parse(Data, FCatalystTemperatureSensor2Bank1);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S2);
    Exit;
  end;

  // Parse Catalyst Temperature Bank 2, Sensor 2 (PID 3F)
  if ParameterID = OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S2 then
  begin
    ResponseDecoder := TOBDServiceCatalystTemperatureDecoder.Create;
    (ResponseDecoder as TOBDServiceCatalystTemperatureDecoder).Parse(Data, FCatalystTemperatureSensor2Bank2);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S2);
    Exit;
  end;

  // Parse monitor status this drive cycle (PID 41)
  if ParameterID = OBD_SERVICE_01_MONITOR_STATUS_DRIVE_CYCLE then
  begin
    ParseMonitorStatus(Data);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MONITOR_STATUS_DRIVE_CYCLE);
    Exit;
  end;

  // Parse Control Module voltage (PID 42)
  if ParameterID = OBD_SERVICE_01_CONTROL_MODULE_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceControlModuleVoltageDecoder.Create;
    (ResponseDecoder as TOBDServiceControlModuleVoltageDecoder).Parse(Data, FControlModuleVoltage);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_CONTROL_MODULE_VOLTAGE);
    Exit;
  end;

  // Parse absolute load value (PID 43)
  if ParameterID = OBD_SERVICE_01_ABSOLUTE_LOAD_VALUE then
  begin
    ResponseDecoder := TOBDServiceAbsoluteLoadValueDecoder.Create;
    (ResponseDecoder as TOBDServiceAbsoluteLoadValueDecoder).Parse(Data, FAbsoluteLoadValue);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ABSOLUTE_LOAD_VALUE);
    Exit;
  end;

  // Parse commanded air-fuel equivalence ratio (PID 44)
  if ParameterID = OBD_SERVICE_01_COMMANDED_EQUIVALENCE_RATIO then
  begin
    ResponseDecoder := TOBDServiceCommandedAirFuelEQRatioDecoder.Create;
    (ResponseDecoder as TOBDServiceCommandedAirFuelEQRatioDecoder).Parse(Data, FCommandedAirFuelEquivalenceRatio);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_COMMANDED_EQUIVALENCE_RATIO);
    Exit;
  end;

  // Parse relative throttle position (PID 45)
  if ParameterID = OBD_SERVICE_01_RELATIVE_THROTTLE_POSITION then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FRelativeThrottlePosition);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_RELATIVE_THROTTLE_POSITION);
    Exit;
  end;

  // Parse Ambient air temperature (PID 46)
  if ParameterID = OBD_SERVICE_01_AMBIENT_AIR_TEMPERATURE then
  begin
    ResponseDecoder := TOBDTemperatureDecoder.Create;
    (ResponseDecoder as TOBDTemperatureDecoder).Parse(Data, FAmbientAirTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_AMBIENT_AIR_TEMPERATURE);
    Exit;
  end;

  // Parse Absolute throttle position B (PID 47)
  if ParameterID = OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_B then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FAbsoluteThrottlePositionB);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_B);
    Exit;
  end;

  // Parse Absolute throttle position C (PID 48)
  if ParameterID = OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_C then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FAbsoluteThrottlePositionC);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_C);
    Exit;
  end;

  // Parse Accelerator pedal position D (PID 49)
  if ParameterID = OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_D then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FAcceleratorPedalPositionD);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_D);
    Exit;
  end;

  // Parse Accelerator pedal position E (PID 4A)
  if ParameterID = OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_E then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FAcceleratorPedalPositionE);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_E);
    Exit;
  end;

  // Parse Accelerator pedal position F (PID 4B)
  if ParameterID = OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_F then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FAcceleratorPedalPositionF);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_F);
    Exit;
  end;

  // Parse Commanded throttle actuator (PID 4C)
  if ParameterID = OBD_SERVICE_01_COMMANDED_THROTTLE_ACTUATOR then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FCommandedThrottleActuator);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_COMMANDED_THROTTLE_ACTUATOR);
    Exit;
  end;

  // Parse Time run with MIL on (PID 4D)
  if ParameterID = OBD_SERVICE_01_TIME_RUN_WITH_MIL_ON then
  begin
    ResponseDecoder := TOBDServiceMinuteDecoder.Create;
    (ResponseDecoder as TOBDServiceMinuteDecoder).Parse(Data, FTimeRunWithMILOn);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_TIME_RUN_WITH_MIL_ON);
    Exit;
  end;

  // Parse Time run with MIL on (PID 4E)
  if ParameterID = OBD_SERVICE_01_TIME_SINCE_TROUBLE_CODES_CLEARED then
  begin
    ResponseDecoder := TOBDServiceMinuteDecoder.Create;
    (ResponseDecoder as TOBDServiceMinuteDecoder).Parse(Data, FTimeSinceTroubleCodesCleared);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_TIME_SINCE_TROUBLE_CODES_CLEARED);
    Exit;
  end;

  // Parse maximum value for Fuel–Air equivalence ratio, oxygen sensor voltage, oxygen sensor current, and intake manifold absolute pressure (PID 4F)
  if ParameterID = OBD_SERVICE_01_MAX_VALUES_EQUIV_RATIO_O2S_VOLTAGE then
  begin
    ResponseDecoder := TOBDServiceMaxSensorValuesDecoder.Create;
    (ResponseDecoder as TOBDServiceMaxSensorValuesDecoder).Parse(Data, FMaxSensorValues.FAirFuelEquivalenceRatio, FMaxSensorValues.FOxygenSensorVoltage, FMaxSensorValues.FOxygenSensorCurrent, FMaxSensorValues.FIntakeManifoldPressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MAX_VALUES_EQUIV_RATIO_O2S_VOLTAGE);
    Exit;
  end;

  // Parse maximum value for air flow rate from mass air flow sensor (PID 50)
  if ParameterID = OBD_SERVICE_01_MAX_VALUE_AIR_FLOW_RATE_FROM_MAF_SENSOR then
  begin
    if Length(Data) < 1 then Exit;
    FMaxSensorValues.FAirFlowRate := Data[0] * 10;
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MAX_VALUE_AIR_FLOW_RATE_FROM_MAF_SENSOR);
    Exit;
  end;

  // Parse Fuel Type (PID 51)
  if ParameterID = OBD_SERVICE_01_FUEL_TYPE then
  begin
    if Length(Data) < 1 then Exit;
    FFuelType := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_TYPE);
    Exit;
  end;

  // Parse Ethanol Fuel (Percentage) (PID 52)
  if ParameterID = OBD_SERVICE_01_ETHANOL_FUEL_PERCENT then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FEthanolFuel);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ETHANOL_FUEL_PERCENT);
    Exit;
  end;

  // Parse Absolute Evap System Vapor Pressure (PID 53)
  if ParameterID = OBD_SERVICE_01_ABSOLUTE_EVAP_SYSTEM_VAPOR_PRESSURE then
  begin
    ResponseDecoder := TOBDServiceAbsoluteEvapSystemVaporPressureDecoder.Create;
    (ResponseDecoder as TOBDServiceAbsoluteEvapSystemVaporPressureDecoder).Parse(Data, FAbsoluteEvapSystemVaporPressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ABSOLUTE_EVAP_SYSTEM_VAPOR_PRESSURE);
    Exit;
  end;

  // Parse Evap. System Vapor Pressure 2 (PID 54)
  if ParameterID = OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE_2 then
  begin
    ResponseDecoder := TOBDServiceEvapSystemVaporPressure2Decoder.Create;
    (ResponseDecoder as TOBDServiceEvapSystemVaporPressure2Decoder).Parse(Data, FEvapSystemVaporPressure2);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE_2);
    Exit;
  end;

  // Parse Short-term Oxygen sensor trim Bank 1 and 3 (PID 55)
  if ParameterID = OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorTrimDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorTrimDecoder).Parse(Data, FShortTermOxygenSensorTrimBank1Bank3.FBank1, FShortTermOxygenSensorTrimBank1Bank3.FBank3);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3);
    Exit;
  end;

  // Parse Long-term Oxygen sensor trim Bank 1 and 3 (PID 56)
  if ParameterID = OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorTrimDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorTrimDecoder).Parse(Data, FLongTermOxygenSensorTrimBank1Bank3.FBank1, FLongTermOxygenSensorTrimBank1Bank3.FBank3);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3);
    Exit;
  end;

  // Parse Short-term Oxygen sensor trim Bank 2 and 4 (PID 57)
  if ParameterID = OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorTrimDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorTrimDecoder).Parse(Data, FShortTermOxygenSensorTrimBank2Bank4.FBank2, FShortTermOxygenSensorTrimBank2Bank4.FBank4);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4);
    Exit;
  end;

  // Parse Long-term Oxygen sensor trim Bank 2 and 4 (PID 58)
  if ParameterID = OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4 then
  begin
    ResponseDecoder := TOBDServiceOxygenSensorTrimDecoder.Create;
    (ResponseDecoder as TOBDServiceOxygenSensorTrimDecoder).Parse(Data, FLongTermOxygenSensorTrimBank2Bank4.FBank2, FLongTermOxygenSensorTrimBank2Bank4.FBank4);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4);
    Exit;
  end;

  // Parse Fuel rail absolute pressure (PID 59)
  if ParameterID = OBD_SERVICE_01_FUEL_RAIL_PRESSURE_ABSOLUTE then
  begin
    ResponseDecoder := TOBDServiceFuelRailAbsolutePressureDecoder.Create;
    (ResponseDecoder as TOBDServiceFuelRailAbsolutePressureDecoder).Parse(Data, FFuelRailAbsolutePressure);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_RAIL_PRESSURE_ABSOLUTE);
    Exit;
  end;

  // Parse Relative accelerator pedal position (PID 5A)
  if ParameterID = OBD_SERVICE_01_RELATIVE_ACCELERATOR_PEDAL_POSITION then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FRelativeAcceleratorPedalPosition);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_RELATIVE_ACCELERATOR_PEDAL_POSITION);
    Exit;
  end;

  // Parse Hybrid battery pack remaining life (PID 5B)
  if ParameterID = OBD_SERVICE_01_HYBRID_BATTERY_PACK_REMAINING_LIFE then
  begin
    ResponseDecoder := TOBDPercentageDecoder.Create;
    (ResponseDecoder as TOBDPercentageDecoder).Parse(Data, FHybridBatteryPackRemainingLife);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_HYBRID_BATTERY_PACK_REMAINING_LIFE);
    Exit;
  end;

  // Parse Engine oil temperature (PID 5C)
  if ParameterID = OBD_SERVICE_01_ENGINE_OIL_TEMPERATURE then
  begin
    ResponseDecoder := TOBDTemperatureDecoder.Create;
    (ResponseDecoder as TOBDTemperatureDecoder).Parse(Data, FEngineOilTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_OIL_TEMPERATURE);
    Exit;
  end;

  // Parse Fuel injection timing (PID 5D)
  if ParameterID = OBD_SERVICE_01_FUEL_INJECTION_TIMING then
  begin
    ResponseDecoder := TOBDServiceFuelInjectionTimingDecoder.Create;
    (ResponseDecoder as TOBDServiceFuelInjectionTimingDecoder).Parse(Data, FFuelInjectionTiming);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_FUEL_INJECTION_TIMING);
    Exit;
  end;

  // Parse engine fuel rate (PID 5E)
  if ParameterID = OBD_SERVICE_01_ENGINE_FUEL_RATE then
  begin
    ResponseDecoder := TOBDServiceEngineFuelRateDecoder.Create;
    (ResponseDecoder as TOBDServiceEngineFuelRateDecoder).Parse(Data, FEngineFuelRate);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_FUEL_RATE);
    Exit;
  end;

  // Parse Emission requirements to which vehicle is designed (PID 5F)
  if ParameterID = OBD_SERVICE_01_EMISSION_REQUIREMENTS then
  begin
    if Length(Data) < 1 then Exit;
    FEmissionRequirements := Data[0];
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_EMISSION_REQUIREMENTS);
    Exit;
  end;

  // Parse Drivers demand engine percent torque (PID 61)
  if ParameterID = OBD_SERVICE_01_DRIVERS_DEMAND_ENGINE_PERCENT_TORQUE then
  begin
    ResponseDecoder := TOBDServiceEngineTorqueDecoder.Create;
    (ResponseDecoder as TOBDServiceEngineTorqueDecoder).Parse(Data, FDriversDemandEnginePercentTorque);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_DRIVERS_DEMAND_ENGINE_PERCENT_TORQUE);
    Exit;
  end;

  // Parse Actual engine percent torque (PID 62)
  if ParameterID = OBD_SERVICE_01_ACTUAL_ENGINE_PERCENT_TORQUE then
  begin
    ResponseDecoder := TOBDServiceEngineTorqueDecoder.Create;
    (ResponseDecoder as TOBDServiceEngineTorqueDecoder).Parse(Data, FActualEnginePercentTorque);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ACTUAL_ENGINE_PERCENT_TORQUE);
    Exit;
  end;

  // Parse engine reference torque (PID 63)
  if ParameterID = OBD_SERVICE_01_ENGINE_REFERENCE_TORQUE then
  begin
    ResponseDecoder := TOBDServiceEngineReferenceTorqueDecoder.Create;
    (ResponseDecoder as TOBDServiceEngineReferenceTorqueDecoder).Parse(Data, FEngineReferenceTorque);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_REFERENCE_TORQUE);
    Exit;
  end;

  // Parse engine percent torque data (PID 64)
  if ParameterID = OBD_SERVICE_01_ENGINE_PERCENT_TORQUE_DATA then
  begin
    ResponseDecoder := TOBDServiceEnginePercentTorqueDataDecoder.Create;
    (ResponseDecoder as TOBDServiceEnginePercentTorqueDataDecoder).Parse(Data, FEnginePercentTorqueData.FIdle, FEnginePercentTorqueData.FPoint1, FEnginePercentTorqueData.FPoint2, FEnginePercentTorqueData.FPoint3, FEnginePercentTorqueData.FPoint4);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_PERCENT_TORQUE_DATA);
    Exit;
  end;

  // Parse Auxiliary input / output supported (PID  65)
  if ParameterID = OBD_SERVICE_01_AUXILIARY_INPUT_OUTPUT_SUPPORTED then
  begin
    // Make sure we have at least 2 bytes
    if Length(Data) < 2 then Exit;
    // Supported input/output's
    FAuxiliaryInputOutputSupported.FInput1 := (Data[0] and $01) <> 0;
    FAuxiliaryInputOutputSupported.FOutput1 := (Data[0] and $02) <> 0;
    FAuxiliaryInputOutputSupported.FInput2 := (Data[0] and $04) <> 0;
    FAuxiliaryInputOutputSupported.FOutput2 := (Data[0] and $08) <> 0;
    FAuxiliaryInputOutputSupported.FInput3 := (Data[0] and $10) <> 0;
    FAuxiliaryInputOutputSupported.FOutput3 := (Data[0] and $20) <> 0;
    FAuxiliaryInputOutputSupported.FInput4 := (Data[0] and $40) <> 0;
    FAuxiliaryInputOutputSupported.FOutput4 := (Data[0] and $80) <> 0;
    FAuxiliaryInputOutputSupported.FInput5 := (Data[1] and $01) <> 0;
    FAuxiliaryInputOutputSupported.FOutput5 := (Data[1] and $02) <> 0;
    FAuxiliaryInputOutputSupported.FInput6 := (Data[1] and $04) <> 0;
    FAuxiliaryInputOutputSupported.FOutput6 := (Data[1] and $08) <> 0;
    FAuxiliaryInputOutputSupported.FInput7 := (Data[1] and $10) <> 0;
    FAuxiliaryInputOutputSupported.FOutput7 := (Data[1] and $20) <> 0;
    FAuxiliaryInputOutputSupported.FInput8 := (Data[1] and $40) <> 0;
    FAuxiliaryInputOutputSupported.FOutput8 := (Data[1] and $80) <> 0;
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_AUXILIARY_INPUT_OUTPUT_SUPPORTED);
    Exit;
  end;

  // Parse mass airflow sensor data (PID 66)
  if ParameterID = OBD_SERVICE_01_MASS_AIR_FLOW_SENSOR then
  begin
    ResponseDecoder := TOBDServiceMassAirflowSensorDecoder.Create;
    (ResponseDecoder as TOBDServiceMassAirflowSensorDecoder).Parse(Data, FMassAirFlowSensor.FSensorASupported, FMassAirFlowSensor.FSensorBSupported, FMassAirFlowSensor.FSensorARate, FMassAirFlowSensor.FSensorBRate);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_MASS_AIR_FLOW_SENSOR);
    Exit;
  end;

  // Parse engine coolant temperature data (PID 67)
  if ParameterID = OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE_ then
  begin
    ResponseDecoder := TOBDServiceSensorTemperatureDataDecoder.Create;
    (ResponseDecoder as TOBDServiceSensorTemperatureDataDecoder).Parse(Data, FEngineCoolantTemperatureData.FSensorASupported, FEngineCoolantTemperatureData.FSensorBSupported, FEngineCoolantTemperatureData.FSensorATemperature, FEngineCoolantTemperatureData.FSensorBTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE_);
    Exit;
  end;

  // Parse intake air temperature sensor data (PID 68)
  if ParameterID = OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE_SENSOR then
  begin
    ResponseDecoder := TOBDServiceSensorTemperatureDataDecoder.Create;
    (ResponseDecoder as TOBDServiceSensorTemperatureDataDecoder).Parse(Data, FEngineIntakeAirTemperatureData.FSensorASupported, FEngineIntakeAirTemperatureData.FSensorBSupported, FEngineIntakeAirTemperatureData.FSensorATemperature, FEngineIntakeAirTemperatureData.FSensorBTemperature);
    if Assigned(OnLiveData) then OnLiveData(Self, OBD_SERVICE_01, OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE_SENSOR);
    Exit;
  end;

  // Todo - add other PID's (69 - C8)
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
