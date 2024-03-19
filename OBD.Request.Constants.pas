//------------------------------------------------------------------------------
// UNIT           : OBD.Request.Constants.pas
// CONTENTS       : OBD Request Constants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/03/2024
//------------------------------------------------------------------------------
unit OBD.Request.Constants;

interface

//------------------------------------------------------------------------------
// OBD-II (SAE J1979) DIAGNOSTIC SERVICES
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   OBD-II Service 01 (Request Live Data)
  /// </summary>
  OBD_SERVICE_01 = $01;
  /// <summary>
  ///   OBD-II Service 02 (Request Freeze Frames)
  /// </summary>
  OBD_SERVICE_02 = $02;
  /// <summary>
  ///   OBD-II Service 03 (Request Stored Trouble Codes)
  /// </summary>
  OBD_SERVICE_03 = $03;
  /// <summary>
  ///   OBD-II Service 04 (Clear/Reset Stored Emissions Related Data)
  /// </summary>
  OBD_SERVICE_04 = $04;
  /// <summary>
  ///   OBD-II Service 05 (Request Oxygen Sensors Test Results)
  /// </summary>
  OBD_SERVICE_05 = $05;
  /// <summary>
  ///   OBD-II Service 06 (Request On-Board System Tests Results)
  /// </summary>
  OBD_SERVICE_06 = $06;
  /// <summary>
  ///   OBD-II Service 07 (Request Pending Trouble Codes)
  /// </summary>
  OBD_SERVICE_07 = $07;
  /// <summary>
  ///   OBD-II Service 08 (Request Control of On-Board Systems)
  /// </summary>
  OBD_SERVICE_08 = $08;
  /// <summary>
  ///   OBD-II Service 09 (Request Vehicle Information)
  /// </summary>
  OBD_SERVICE_09 = $09;
  /// <summary>
  ///   OBD-II Service 0A (Request Permanent Trouble Codes)
  /// </summary>
  OBD_SERVICE_0A = $0A;

//------------------------------------------------------------------------------
// OBD-II (SAE J1979) SERVICE 01 (SUPPORTED PID'S)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   PIDs supported [01 - 20]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_0120 = $00;
  /// <summary>
  ///   PIDs supported [21 - 40]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_2140 = $20;
  /// <summary>
  ///   PIDs supported [41 - 60]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_4160 = $40;
  /// <summary>
  ///   PIDs supported [61 - 80]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_6180 = $60;
  /// <summary>
  ///   PIDs supported [81 - A0]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_81A0 = $80;
  /// <summary>
  ///   PIDs supported [A1 - C0]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_A1C0 = $A0;
  /// <summary>
  ///   PIDs supported [C1 - E0]
  /// </summary>
  OBD_SERVICE_01_SUPPORTED_PID_C1E0 = $C0;

//------------------------------------------------------------------------------
// OBD-II (SAE J1979) SERVICE 01
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Monitor status since DTCs cleared. (Includes malfunction indicator lamp (MIL) status and number of DTCs.)
  /// </summary>
  OBD_SERVICE_01_MONITOR_STATUS = $01;
  /// <summary>
  ///   Freeze DTC
  /// </summary>
  OBD_SERVICE_02_FREEZE_DTC = $02;
  /// <summary>
  ///   Fuel system status
  /// </summary>
  OBD_SERVICE_01_FUEL_SYSTEM_STATUS = $03;
  /// <summary>
  ///   Calculated engine load value
  /// </summary>
  OBD_SERVICE_01_CALCULATED_ENGINE_LOAD = $04;
  /// <summary>
  ///   Engine coolant temperature
  /// </summary>
  OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE = $05;
  /// <summary>
  ///   Short term fuel % trim—Bank 1
  /// </summary>
  OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK1 = $06;
  /// <summary>
  ///   Long term fuel % trim—Bank 1
  /// </summary>
  OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK1 = $07;
  /// <summary>
  ///   Short term fuel % trim—Bank 2
  /// </summary>
  OBD_SERVICE_01_SHORT_TERM_FUEL_TRIM_BANK2 = $08;
  /// <summary>
  ///   Long term fuel % trim—Bank 2
  /// </summary>
  OBD_SERVICE_01_LONG_TERM_FUEL_TRIM_BANK2 = $09;
  /// <summary>
  ///   Fuel pressure
  /// </summary>
  OBD_SERVICE_01_FUEL_PRESSURE = $0A;
  /// <summary>
  ///   Intake manifold absolute pressure
  /// </summary>
  OBD_SERVICE_01_INTAKE_MANIFOLD_PRESSURE = $0B;
  /// <summary>
  ///   Engine RPM
  /// </summary>
  OBD_SERVICE_01_ENGINE_RPM = $0C;
  /// <summary>
  ///   Vehicle speed
  /// </summary>
  OBD_SERVICE_01_VEHICLE_SPEED = $0D;
  /// <summary>
  ///   Timing advance
  /// </summary>
  OBD_SERVICE_01_TIMING_ADVANCE = $0E;
  /// <summary>
  ///   Intake air temperature
  /// </summary>
  OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE = $0F;
  /// <summary>
  ///   MAF air flow rate
  /// </summary>
  OBD_SERVICE_01_MAF_AIR_FLOW_RATE = $10;
  /// <summary>
  ///   Throttle position
  /// </summary>
  OBD_SERVICE_01_THROTTLE_POSITION = $11;
  /// <summary>
  ///   Commanded secondary air status
  /// </summary>
  OBD_SERVICE_01_COMMANDED_SECONDARY_AIR_STATUS = $12;
  /// <summary>
  ///   Oxygen sensors present
  /// </summary>
  OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_1 = $13;
  /// <summary>
  ///   Sensor 1: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR1_BANK1 = $14;
  /// <summary>
  ///   Bank 1, Sensor 2: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR2_BANK1 = $15;
  /// <summary>
  ///   Bank 1, Sensor 3: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR3_BANK1 = $16;
  /// <summary>
  ///   Bank 1, Sensor 4: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR4_BANK1 = $17;
  /// <summary>
  ///   Bank 2, Sensor 1: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR1_BANK2 = $18;
  /// <summary>
  ///   Bank 2, Sensor 2: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR2_BANK2 = $19;
  /// <summary>
  ///   Bank 2, Sensor 3: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR3_BANK2 = $1A;
  /// <summary>
  ///   Bank 2, Sensor 4: Oxygen sensor voltage, Short term fuel trim
  /// </summary>
  OBD_SERVICE_01_O2_SENSOR4_BANK2 = $1B;
  /// <summary>
  ///   OBD standards this vehicle conforms to
  /// </summary>
  OBD_SERVICE_01_OBD_STANDARDS = $1C;
  /// <summary>
  ///   Oxygen sensors present
  /// </summary>
  OBD_SERVICE_01_OXYGEN_SENSORS_PRESENT_2 = $1D;
  /// <summary>
  ///   Auxiliary input status
  /// </summary>
  OBD_SERVICE_01_AUXILIARY_INPUT_STATUS = $1E;
  /// <summary>
  ///   Run time since engine start
  /// </summary>
  OBD_SERVICE_01_ENGINE_RUN_TIME = $1F;
  /// <summary>
  ///   Distance traveled with malfunction indicator lamp (MIL) on
  /// </summary>
  OBD_SERVICE_01_DISTANCE_TRAVELED_MIL_ON = $21;
  /// <summary>
  ///   Fuel Rail Pressure (relative to manifold vacuum)
  /// </summary>
  OBD_SERVICE_01_FUEL_RAIL_PRESSURE_VACUUM = $22;
  /// <summary>
  ///   Fuel Rail Pressure (diesel, or gasoline direct inject)
  /// </summary>
  OBD_SERVICE_01_FUEL_RAIL_PRESSURE_DIRECT = $23;
  /// <summary>
  ///   O2S1_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $24;
  /// <summary>
  ///   O2S2_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $25;
  /// <summary>
  ///   O2S3_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $26;
  /// <summary>
  ///   O2S4_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $27;
  /// <summary>
  ///   O2S5_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $28;
  /// <summary>
  ///   O2S6_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $29;
  /// <summary>
  ///   O2S7_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $2A;
  /// <summary>
  ///   O2S8_WR_lambda(1): Equivalence Ratio, Voltage
  /// </summary>
  OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_VOLTAGE = $2B;
  /// <summary>
  ///   Commanded EGR
  /// </summary>
  OBD_SERVICE_01_COMMANDED_EGR = $2C;
  /// <summary>
  ///   EGR Error
  /// </summary>
  OBD_SERVICE_01_EGR_ERROR = $2D;
  /// <summary>
  ///   Commanded evaporative purge
  /// </summary>
  OBD_SERVICE_01_COMMANDED_EVAPORATIVE_PURGE = $2E;
  /// <summary>
  ///   Fuel Level Input
  /// </summary>
  OBD_SERVICE_01_FUEL_LEVEL_INPUT = $2F;
  /// <summary>
  ///   Number of warm-ups since codes cleared
  /// </summary>
  OBD_SERVICE_01_WARMUPS_SINCE_CODES_CLEARED = $30;
  /// <summary>
  ///   Distance traveled since codes cleared
  /// </summary>
  OBD_SERVICE_01_DISTANCE_TRAVELED_CODES_CLEARED = $31;
  /// <summary>
  ///   Evap. System Vapor Pressure
  /// </summary>
  OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE = $32;
  /// <summary>
  ///   Barometric pressure
  /// </summary>
  OBD_SERVICE_01_BAROMETRIC_PRESSURE = $33;
  /// <summary>
  ///   O2S1_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S1_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $34;
  /// <summary>
  ///   O2S2_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S2_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $35;
  /// <summary>
  ///   O2S3_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S3_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $36;
  /// <summary>
  ///   O2S4_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S4_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $37;
  /// <summary>
  ///   O2S5_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S5_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $38;
  /// <summary>
  ///   O2S6_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S6_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $39;
  /// <summary>
  ///   O2S7_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S7_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $3A;
  /// <summary>
  ///   O2S8_WR_lambda(1): Equivalence Ratio, Current
  /// </summary>
  OBD_SERVICE_01_O2S8_WR_LAMBDA_1_EQUIV_RATIO_CURRENT = $3B;
  /// <summary>
  ///   Catalyst Temperature Bank 1, Sensor 1
  /// </summary>
  OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S1 = $3C;
  /// <summary>
  ///   Catalyst Temperature Bank 2, Sensor 1
  /// </summary>
  OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S1 = $3D;
  /// <summary>
  ///   Catalyst Temperature Bank 1, Sensor 2
  /// </summary>
  OBD_SERVICE_01_CATALYST_TEMPERATURE_B1S2 = $3E;
  /// <summary>
  ///   Catalyst Temperature Bank 2, Sensor 2
  /// </summary>
  OBD_SERVICE_01_CATALYST_TEMPERATURE_B2S2 = $3F;
  /// <summary>
  ///   Monitor status this drive cycle
  /// </summary>
  OBD_SERVICE_01_MONITOR_STATUS_DRIVE_CYCLE = $41;
  /// <summary>
  ///   Control module voltage
  /// </summary>
  OBD_SERVICE_01_CONTROL_MODULE_VOLTAGE = $42;
  /// <summary>
  ///   Absolute load value
  /// </summary>
  OBD_SERVICE_01_ABSOLUTE_LOAD_VALUE = $43;
  /// <summary>
  ///   Command equivalence ratio
  /// </summary>
  OBD_SERVICE_01_COMMANDED_EQUIVALENCE_RATIO = $44;
  /// <summary>
  ///   Relative throttle position
  /// </summary>
  OBD_SERVICE_01_RELATIVE_THROTTLE_POSITION = $45;
  /// <summary>
  ///   Ambient air temperature
  /// </summary>
  OBD_SERVICE_01_AMBIENT_AIR_TEMPERATURE = $46;
  /// <summary>
  ///   Absolute throttle position B
  /// </summary>
  OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_B = $47;
  /// <summary>
  ///   Absolute throttle position C
  /// </summary>
  OBD_SERVICE_01_ABSOLUTE_THROTTLE_POSITION_C = $48;
  /// <summary>
  ///   Accelerator pedal position D
  /// </summary>
  OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_D = $49;
  /// <summary>
  ///   Accelerator pedal position E
  /// </summary>
  OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_E = $4A;
  /// <summary>
  ///   Accelerator pedal position F
  /// </summary>
  OBD_SERVICE_01_ACCELERATOR_PEDAL_POSITION_F = $4B;
  /// <summary>
  ///   Commanded throttle actuator
  /// </summary>
  OBD_SERVICE_01_COMMANDED_THROTTLE_ACTUATOR = $4C;
  /// <summary>
  ///   Time run with MIL on
  /// </summary>
  OBD_SERVICE_01_TIME_RUN_WITH_MIL_ON = $4D;
  /// <summary>
  ///   Time since trouble codes cleared
  /// </summary>
  OBD_SERVICE_01_TIME_SINCE_TROUBLE_CODES_CLEARED = $4E;
  /// <summary>
  ///   Maximum value for equivalence ratio, oxygen sensor voltage, oxygen sensor current, and intake manifold absolute pressure
  /// </summary>
  OBD_SERVICE_01_MAX_VALUES_EQUIV_RATIO_O2S_VOLTAGE = $4F;
  /// <summary>
  ///   Maximum value for air flow rate from mass air flow sensor
  /// </summary>
  OBD_SERVICE_01_MAX_VALUE_AIR_FLOW_RATE_FROM_MAF_SENSOR = $50;
  /// <summary>
  ///   Fuel Type
  /// </summary>
  OBD_SERVICE_01_FUEL_TYPE = $51;
  /// <summary>
  ///   Ethanol fuel %
  /// </summary>
  OBD_SERVICE_01_ETHANOL_FUEL_PERCENT = $52;
  /// <summary>
  ///   Absolute Evap system Vapor Pressure
  /// </summary>
  OBD_SERVICE_01_ABSOLUTE_EVAP_SYSTEM_VAPOR_PRESSURE = $53;
  /// <summary>
  ///   Evap system vapor pressure 2
  /// </summary>
  OBD_SERVICE_01_EVAP_SYSTEM_VAPOR_PRESSURE_2 = $54;
  /// <summary>
  ///   Short term secondary oxygen sensor trim bank 1 and bank 3
  /// </summary>
  OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3 = $55;
  /// <summary>
  ///   Long term secondary oxygen sensor trim bank 1 and bank 3
  /// </summary>
  OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B1_B3 = $56;
  /// <summary>
  ///   Short term secondary oxygen sensor trim bank 2 and bank 4
  /// </summary>
  OBD_SERVICE_01_SHORT_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4 = $57;
  /// <summary>
  ///   Long term secondary oxygen sensor trim bank 2 and bank 4
  /// </summary>
  OBD_SERVICE_01_LONG_TERM_SECONDARY_O2_SENSOR_TRIM_B2_B4 = $58;
  /// <summary>
  ///   Fuel rail pressure (absolute)
  /// </summary>
  OBD_SERVICE_01_FUEL_RAIL_PRESSURE_ABSOLUTE = $59;
  /// <summary>
  ///   Relative accelerator pedal position
  /// </summary>
  OBD_SERVICE_01_RELATIVE_ACCELERATOR_PEDAL_POSITION = $5A;
  /// <summary>
  ///   Hybrid battery pack remaining life
  /// </summary>
  OBD_SERVICE_01_HYBRID_BATTERY_PACK_REMAINING_LIFE = $5B;
  /// <summary>
  ///   Engine oil temperature
  /// </summary>
  OBD_SERVICE_01_ENGINE_OIL_TEMPERATURE = $5C;
  /// <summary>
  ///   Fuel injection timing
  /// </summary>
  OBD_SERVICE_01_FUEL_INJECTION_TIMING = $5D;
  /// <summary>
  ///   Engine fuel rate
  /// </summary>
  OBD_SERVICE_01_ENGINE_FUEL_RATE = $5E;
  /// <summary>
  ///   Emission requirements to which vehicle is designed
  /// </summary>
  OBD_SERVICE_01_EMISSION_REQUIREMENTS = $5F;
  /// <summary>
  ///   Driver's demand engine - percent torque
  /// </summary>
  OBD_SERVICE_01_DRIVERS_DEMAND_ENGINE_PERCENT_TORQUE = $61;
  /// <summary>
  ///   Actual engine - percent torque
  /// </summary>
  OBD_SERVICE_01_ACTUAL_ENGINE_PERCENT_TORQUE = $62;
  /// <summary>
  ///   Engine reference torque
  /// </summary>
  OBD_SERVICE_01_ENGINE_REFERENCE_TORQUE = $63;
  /// <summary>
  ///   Engine percent torque data
  /// </summary>
  OBD_SERVICE_01_ENGINE_PERCENT_TORQUE_DATA = $64;
  /// <summary>
  ///   Auxiliary input / output supported
  /// </summary>
  OBD_SERVICE_01_AUXILIARY_INPUT_OUTPUT_SUPPORTED = $65;
  /// <summary>
  ///   Mass air flow sensor
  /// </summary>
  OBD_SERVICE_01_MASS_AIR_FLOW_SENSOR = $66;
  /// <summary>
  ///   Engine coolant temperature
  /// </summary>
  OBD_SERVICE_01_ENGINE_COOLANT_TEMPERATURE_ = $67;
  /// <summary>
  ///   Intake air temperature sensor
  /// </summary>
  OBD_SERVICE_01_INTAKE_AIR_TEMPERATURE_SENSOR = $68;
  /// <summary>
  ///   Commanded EGR and EGR Error
  /// </summary>
  OBD_SERVICE_01_COMMANDED_EGR_AND_EGR_ERROR = $69;
  /// <summary>
  ///   Commanded Diesel intake air flow control and relative intake air flow position
  /// </summary>
  OBD_SERVICE_01_COMMANDED_DIESEL_INTAKE_AIR_FLOW_CONTROL = $6A;
  /// <summary>
  ///   Exhaust gas recirculation temperature
  /// </summary>
  OBD_SERVICE_01_EXHAUST_GAS_RECIRCULATION_TEMPERATURE = $6B;
  /// <summary>
  ///   Commanded throttle actuator control and relative throttle position
  /// </summary>
  OBD_SERVICE_01_COMMANDED_THROTTLE_ACTUATOR_CONTROL = $6C;
  /// <summary>
  ///   Fuel pressure control system
  /// </summary>
  OBD_SERVICE_01_FUEL_PRESSURE_CONTROL_SYSTEM = $6D;
  /// <summary>
  ///   Injection pressure control system
  /// </summary>
  OBD_SERVICE_01_INJECTION_PRESSURE_CONTROL_SYSTEM = $6E;
  /// <summary>
  ///   Turbocharger compressor inlet pressure
  /// </summary>
  OBD_SERVICE_01_TURBOCHARGER_COMPRESSOR_INLET_PRESSURE = $6F;
  /// <summary>
  ///   Boost pressure control
  /// </summary>
  OBD_SERVICE_01_BOOST_PRESSURE_CONTROL = $70;
  /// <summary>
  ///   Variable Geometry turbo (VGT) control
  /// </summary>
  OBD_SERVICE_01_VARIABLE_GEOMETRY_TURBO_CONTROL = $71;
  /// <summary>
  ///   Wastegate control
  /// </summary>
  OBD_SERVICE_01_WASTEGATE_CONTROL = $72;
  /// <summary>
  ///   Exhaust pressure
  /// </summary>
  OBD_SERVICE_01_EXHAUST_PRESSURE = $73;
  /// <summary>
  ///   Turbocharger RPM
  /// </summary>
  OBD_SERVICE_01_TURBOCHARGER_RPM = $74;
  /// <summary>
  ///   Turbocharger temperature
  /// </summary>
  OBD_SERVICE_01_TURBOCHARGER_TEMPERATURE1 = $75;
  /// <summary>
  ///   Turbocharger temperature
  /// </summary>
  OBD_SERVICE_01_TURBOCHARGER_TEMPERATURE2 = $76;
  /// <summary>
  ///   Charge air cooler temperature (CACT)
  /// </summary>
  OBD_SERVICE_01_CHARGE_AIR_COOLER_TEMPERATURE = $77;
  /// <summary>
  ///   Exhaust Gas temperature (EGT) Bank 1
  /// </summary>
  OBD_SERVICE_01_EXHAUST_GAS_TEMPERATURE_BANK_1 = $78;
  /// <summary>
  ///   Exhaust Gas temperature (EGT) Bank 2
  /// </summary>
  OBD_SERVICE_01_EXHAUST_GAS_TEMPERATURE_BANK_2 = $79;
  /// <summary>
  ///   Diesel particulate filter (DPF)
  /// </summary>
  OBD_SERVICE_01_DIESEL_PARTICULATE_FILTER1 = $7A;
  /// <summary>
  ///   Diesel particulate filter (DPF)
  /// </summary>
  OBD_SERVICE_01_DIESEL_PARTICULATE_FILTER2 = $7B;
  /// <summary>
  ///   Diesel Particulate filter (DPF) temperature
  /// </summary>
  OBD_SERVICE_01_DIESEL_PARTICULATE_FILTER_TEMPERATURE = $7C;
  /// <summary>
  ///   NOx NTE control area status
  /// </summary>
  OBD_SERVICE_01_NOX_NTE_CONTROL_AREA_STATUS = $7D;
  /// <summary>
  ///   PM NTE control area status
  /// </summary>
  OBD_SERVICE_01_PM_NTE_CONTROL_AREA_STATUS = $7E;
  /// <summary>
  ///   Engine run time
  /// </summary>
  OBD_SERVICE_01_ENGINE_RUN_TIME_ = $7F;
  /// <summary>
  ///   Engine run time for AECD
  /// </summary>
  OBD_SERVICE_01_ENGINE_RUN_TIME_FOR_AECD1 = $81;
  /// <summary>
  ///   Engine run time for AECD
  /// </summary>
  OBD_SERVICE_01_ENGINE_RUN_TIME_FOR_AECD2 = $82;
  /// <summary>
  ///   NOx sensor
  /// </summary>
  OBD_SERVICE_01_NOX_SENSOR = $83;
  /// <summary>
  ///   Manifold surface temperature
  /// </summary>
  OBD_SERVICE_01_MANIFOLD_SURFACE_TEMPERATURE = $84;
  /// <summary>
  ///   NOx reagent system
  /// </summary>
  OBD_SERVICE_01_NOX_REAGENT_SYSTEM = $85;
  /// <summary>
  ///   Particulate matter (PM) sensor
  /// </summary>
  OBD_SERVICE_01_PARTICULATE_MATTER_SENSOR = $86;
  /// <summary>
  ///   Intake manifold absolute pressure
  /// </summary>
  OBD_SERVICE_01_INTAKE_MANIFOLD_ABSOLUTE_PRESSURE2 = $87;

//------------------------------------------------------------------------------
// OBD-II (SAE J1979) SERVICE 02
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Freeze frame trouble code BCD
  /// </summary>
  OBD_SERVICE_02_FREEZE_FRAME_TROUBLE_CODE_BCD = $02;

//------------------------------------------------------------------------------
// OBD-II (SAE J1979) SERVICE 05 (SUPPORTED PID'S)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   PIDs supported [01 - 20]
  /// </summary>
  OBD_SERVICE_05_SUPPORTED_PID_0120 = $0100;
  /// <summary>
  ///   Bank 1 Sensor 1 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_RICH_TO_LEAN = $0101;
  /// <summary>
  ///   Bank 1 Sensor 2 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_RICH_TO_LEAN = $0102;
  /// <summary>
  ///   Bank 1 Sensor 3 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_RICH_TO_LEAN = $0103;
  /// <summary>
  ///   Bank 1 Sensor 4 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_RICH_TO_LEAN = $0104;
  /// <summary>
  ///   Bank 2 Sensor 1 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_RICH_TO_LEAN = $0105;
  /// <summary>
  ///   Bank 2 Sensor 2 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_RICH_TO_LEAN = $0106;
  /// <summary>
  ///   Bank 2 Sensor 3 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_RICH_TO_LEAN = $0107;
  /// <summary>
  ///   Bank 2 Sensor 4 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_RICH_TO_LEAN = $0108;
  /// <summary>
  ///   Bank 3 Sensor 1 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_RICH_TO_LEAN = $0109;
  /// <summary>
  ///   Bank 3 Sensor 2 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_RICH_TO_LEAN = $010A;
  /// <summary>
  ///   Bank 3 Sensor 3 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_RICH_TO_LEAN = $010B;
  /// <summary>
  ///   Bank 3 Sensor 4 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_RICH_TO_LEAN = $010C;
  /// <summary>
  ///   Bank 4 Sensor 1 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_RICH_TO_LEAN = $010D;
  /// <summary>
  ///   Bank 4 Sensor 2 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_RICH_TO_LEAN = $010E;
  /// <summary>
  ///   Bank 4 Sensor 3 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_RICH_TO_LEAN = $010F;
  /// <summary>
  ///   Bank 4 Sensor 4 - Rich to lean sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_RICH_TO_LEAN = $0110;
  /// <summary>
  ///   Bank 1 Sensor 1 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S1_LEAN_TO_RICH = $0201;
  /// <summary>
  ///   Bank 1 Sensor 2 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S2_LEAN_TO_RICH = $0202;
  /// <summary>
  ///   Bank 1 Sensor 3 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S3_LEAN_TO_RICH = $0203;
  /// <summary>
  ///   Bank 1 Sensor 4 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B1S4_LEAN_TO_RICH = $0204;
  /// <summary>
  ///   Bank 2 Sensor 1 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S1_LEAN_TO_RICH = $0205;
  /// <summary>
  ///   Bank 2 Sensor 2 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S2_LEAN_TO_RICH = $0206;
  /// <summary>
  ///   Bank 2 Sensor 3 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S3_LEAN_TO_RICH = $0207;
  /// <summary>
  ///   Bank 2 Sensor 4 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B2S4_LEAN_TO_RICH = $0208;
  /// <summary>
  ///   Bank 3 Sensor 1 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S1_LEAN_TO_RICH = $0209;
  /// <summary>
  ///   Bank 3 Sensor 2 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S2_LEAN_TO_RICH = $020A;
  /// <summary>
  ///   Bank 3 Sensor 3 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S3_LEAN_TO_RICH = $020B;
  /// <summary>
  ///   Bank 3 Sensor 4 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B3S4_LEAN_TO_RICH = $020C;
  /// <summary>
  ///   Bank 4 Sensor 1 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S1_LEAN_TO_RICH = $020D;
  /// <summary>
  ///   Bank 4 Sensor 2 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S2_LEAN_TO_RICH = $020E;
  /// <summary>
  ///   Bank 4 Sensor 3 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S3_LEAN_TO_RICH = $020F;
  /// <summary>
  ///   Bank 4 Sensor 4 - Lean to Rich sensor threshold voltage
  /// </summary>
  OBD_SERVICE_05_O2_SENSOR_MONITOR_B4S4_LEAN_TO_RICH = $0210;

implementation

end.
