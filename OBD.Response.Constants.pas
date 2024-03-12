//------------------------------------------------------------------------------
// UNIT           : OBD.Response.Constants.pas
// CONTENTS       : OBD Response Constants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 12/03/2024
//------------------------------------------------------------------------------
unit OBD.Response.Constants;

interface

// Example response to 01 0C request:
// 7F 01 12
// 7F = Negative Response
// 01 = Function we attempt (Mode 01)
// 12 = Sub-function Not supported

//------------------------------------------------------------------------------
// NEGATIVE RESPONSE CODE (FIRST BYTE) - UDS - ISO 14229-1
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Negative Response Code
  /// </summary>
  OBD_NEGATIVE_RESPONSE = $7F;

//------------------------------------------------------------------------------
// NEGATIVE RESPONSE CODE (THIRD BYTE) - UDS - ISO 14229-1
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   General reject
  /// </summary>
  OBD_GENERAL_REJECT = $10;
  /// <summary>
  ///   Service not supported
  /// </summary>
  OBD_SERVICE_NOT_SUPPORTED = $11;
  /// <summary>
  ///   Subfunction not supported
  /// </summary>
  OBD_SUBFUNCTION_NOT_SUPPORTED = $12;
  /// <summary>
  ///   Incorrect message length or invalid format
  /// </summary>
  OBD_INCORRECT_MESSAGE_LENGTH_OR_INVALID_FORMAT = $13;
  /// <summary>
  ///   Response too long
  /// </summary>
  OBD_RESPONSE_TOO_LONG = $14;
  /// <summary>
  ///   Busy, repeat request
  /// </summary>
  OBD_BUSY_REPEAT_REQUEST = $21;
  /// <summary>
  ///   Conditions not correct
  /// </summary>
  OBD_CONDITIONS_NOT_CORRECT = $22;
  /// <summary>
  ///   Request sequence error
  /// </summary>
  OBD_REQUEST_SEQUENCE_ERROR = $24;
  /// <summary>
  ///   No response from subnet component
  /// </summary>
  OBD_NO_RESPONSE_FROM_SUBNET_COMPONENT = $25;
  /// <summary>
  ///   Failure prevents execution of requested action
  /// </summary>
  OBD_FAILURE_PREVENTS_EXECUTION_OF_REQUESTED_ACTION = $26;
  /// <summary>
  ///   Request out of range
  /// </summary>
  OBD_REQUEST_OUT_OF_RANGE = $31;
  /// <summary>
  ///   Security access denied
  /// </summary>
  OBD_SECURITY_ACCESS_DENIED = $33;
  /// <summary>
  ///   Authentication failed
  /// </summary>
  OBD_AUTHENTICATION_FAILED = $34;
  /// <summary>
  ///   Invalid key
  /// </summary>
  OBD_INVALID_KEY = $35;
  /// <summary>
  ///   Exceeded number of attempts
  /// </summary>
  OBD_EXCEEDED_NUMBER_OF_ATTEMPTS = $36;
  /// <summary>
  ///   Required time delay not expired
  /// </summary>
  OBD_REQUIRED_TIME_DELAY_NOT_EXPIRED = $37;
  /// <summary>
  ///   Secure data transmission required
  /// </summary>
  OBD_SECURE_DATA_TRANSMISSION_REQUIRED = $38;
  /// <summary>
  ///   Secure data transmission not allowed
  /// </summary>
  OBD_SECURE_DATA_TRANSMISSION_NOT_ALLOWED = $39;
  /// <summary>
  ///   Secure data verification failed
  /// </summary>
  OBD_SECURE_DATA_VERIFICATION_FAILED = $3A;
  /// <summary>
  ///   Certificate validation failed, invalid time period
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_TIME_PERIOD = $50;
  /// <summary>
  ///   Certificate validation failed, invalid signature
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_SIGNATURE = $51;
  /// <summary>
  ///   Certificate validation failed, invalid chain of trust
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_CHAIN_OF_TRUST = $52;
  /// <summary>
  ///   Certificate validation failed, invalid type
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_TYPE = $53;
  /// <summary>
  ///   Certificate validation failed, invalid format
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_FORMAT = $54;
  /// <summary>
  ///   Certificate validation failed, invalid content
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_CONTENT = $55;
  /// <summary>
  ///   Certificate validation failed, invalid scope
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_SCOPE = $56;
  /// <summary>
  ///   Certificate validation failed, invalid certificate
  /// </summary>
  OBD_CERTIFICATE_VALIDATION_FAILED_INVALID_CERTIFICATE = $57;
  /// <summary>
  ///   Ownership verification failed
  /// </summary>
  OBD_OWNERSHIP_VERIFICATION_FAILED = $58;
  /// <summary>
  ///   Challenge calculation failed
  /// </summary>
  OBD_CHALLENGE_CALCULATION_FAILED = $59;
  /// <summary>
  ///   Setting access right failed
  /// </summary>
  OBD_SETTING_ACCESS_RIGHT_FAILED = $5A;
  /// <summary>
  ///   Session key creation/derivation failed
  /// </summary>
  OBD_SESSION_KEY_CREATION_DERIVATION_FAILED = $5B;
  /// <summary>
  ///   Configuration data usage failed
  /// </summary>
  OBD_CONFIGURATION_DATA_USAGE_FAILED = $5C;
  /// <summary>
  ///   Deauthentication failed
  /// </summary>
  OBD_DEAUTHENTICATION_FAILED = $5D;
  /// <summary>
  ///   Upload download not accepted
  /// </summary>
  OBD_UPLOAD_DOWNLOAD_NOT_ACCEPTED = $70;
  /// <summary>
  ///   Transfer data suspended
  /// </summary>
  OBD_TRANSFER_DATA_SUSPENDED = $71;
  /// <summary>
  ///   General programming failure
  /// </summary>
  OBD_GENERAL_PROGRAMMING_FAILURE = $72;
  /// <summary>
  ///   Wrong block sequence number
  /// </summary>
  OBD_WRONG_BLOCK_SEQUENCE_NUMBER = $73;
  /// <summary>
  ///   Request correctly received, response pending
  /// </summary>
  OBD_REQUEST_CORRECTLY_RECEIVED_RESPONSE_PENDING = $78;
  /// <summary>
  ///   Subfunction not supported in active session
  /// </summary>
  OBD_SUBFUNCTION_NOT_SUPPORTED_IN_ACTIVE_SESSION = $7E;
  /// <summary>
  ///   Service not supported in active session
  /// </summary>
  OBD_SERVICE_NOT_SUPPORTED_IN_ACTIVE_SESSION = $7F;
  /// <summary>
  ///   RPM too high
  /// </summary>
  OBD_RPM_TOO_HIGH = $81;
  /// <summary>
  ///   RPM too low
  /// </summary>
  OBD_RPM_TOO_LOW = $82;
  /// <summary>
  ///   Engine is running
  /// </summary>
  OBD_ENGINE_IS_RUNNING = $83;
  /// <summary>
  ///   Engine is not running
  /// </summary>
  OBD_ENGINE_IS_NOT_RUNNING = $84;
  /// <summary>
  ///   Engine run time too low
  /// </summary>
  OBD_ENGINE_RUN_TIME_TOO_LOW = $85;
  /// <summary>
  ///   Temperature too high
  /// </summary>
  OBD_TEMPERATURE_TOO_HIGH = $86;
  /// <summary>
  ///   Temperature too low
  /// </summary>
  OBD_TEMPERATURE_TOO_LOW = $87;
  /// <summary>
  ///   Vehicle speed too high
  /// </summary>
  OBD_VEHICLE_SPEED_TOO_HIGH = $88;
  /// <summary>
  ///   Vehicle speed too low
  /// </summary>
  OBD_VEHICLE_SPEED_TOO_LOW = $89;
  /// <summary>
  ///   Throttle/pedal too high
  /// </summary>
  OBD_THROTTLE_PEDAL_TOO_HIGH = $8A;
  /// <summary>
  ///   Throttle/pedal too low
  /// </summary>
  OBD_THROTTLE_PEDAL_TOO_LOW = $8B;
  /// <summary>
  ///   Transmission range not in neutral
  /// </summary>
  OBD_TRANSMISSION_RANGE_NOT_IN_NEUTRAL = $8C;
  /// <summary>
  ///   Transmission range not in gear
  /// </summary>
  OBD_TRANSMISSION_RANGE_NOT_IN_GEAR = $8D;
  /// <summary>
  ///   Brake switch not closed
  /// </summary>
  OBD_BRAKE_SWITCH_NOT_CLOSED = $8F;
  /// <summary>
  ///   Shifter lever not in park
  /// </summary>
  OBD_SHIFTER_LEVER_NOT_IN_PARK = $90;
  /// <summary>
  ///   Torque converter clutch locked
  /// </summary>
  OBD_TORQUE_CONVERTER_CLUTCH_LOCKED = $91;
  /// <summary>
  ///   Voltage too high
  /// </summary>
  OBD_VOLTAGE_TOO_HIGH = $92;
  /// <summary>
  ///   Voltage too low
  /// </summary>
  OBD_VOLTAGE_TOO_LOW = $93;
  /// <summary>
  ///   Resource temporary unavailable
  /// </summary>
  OBD_RESOURCE_TEMPORARY_UNAVAILABLE = $94;

//------------------------------------------------------------------------------
// NEGATIVE RESPONSE CODE (FIRST BYTE) DIAGNOSTIC SESSION CONTROL
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Diagnostic Session Control
  /// </summary>
  OBD_DIAGNOSTIC_SESSION_CONTROL = $10;
  /// <summary>
  ///   ECU Reset
  /// </summary>
  OBD_ECU_RESET = $11;
  /// <summary>
  ///   Security Access
  /// </summary>
  OBD_SECURITY_ACCESS = $27;
  /// <summary>
  ///   Communication Control
  /// </summary>
  OBD_COMMUNICATION_CONTROL = $28;
  /// <summary>
  ///   Tester present
  /// </summary>
  OBD_TESTER_PRESENT = $3E;
  /// <summary>
  ///   Access timing parameters
  /// </summary>
  OBD_ACCESS_TIMING_PARAMETERS = $83;
  /// <summary>
  ///   Secured Data Transmission
  /// </summary>
  OBD_SECURED_DATA_TRANSMISSION = $84;
  /// <summary>
  ///   Control DTC Setting
  /// </summary>
  OBD_CONTROL_DTC_SETTING = $85;

//------------------------------------------------------------------------------
// SERVICE 01 PID 03 - FUEL SYSTEM STATUS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   The motor is off
  /// </summary>
  OBD_ENGINE_STATUS_OFF = $00;
  /// <summary>
  ///   Open loop due to insufficient engine temperature
  /// </summary>
  OBD_ENGINE_STATUS_OPEN_LOOP_INSUFFICIENT_TEMP = $01;
  /// <summary>
  ///   Closed loop, using oxygen sensor feedback to determine fuel mix
  /// </summary>
  OBD_ENGINE_STATUS_CLOSED_LOOP = $02;
  /// <summary>
  ///   Open loop due to engine load OR fuel cut due to deceleration
  /// </summary>
  OBD_ENGINE_STATUS_OPEN_LOOP_LOAD_OR_FUEL_CUT = $04;
  /// <summary>
  ///   Open loop due to system failure
  /// </summary>
  OBD_ENGINE_STATUS_OPEN_LOOP_FAILURE = $08;
  /// <summary>
  ///   Closed loop, using at least one oxygen sensor but there is a fault in the feedback system
  /// </summary>
  OBD_ENGINE_STATUS_CLOSED_LOOP_FAULT = $10;

//------------------------------------------------------------------------------
// SERVICE 01 PID 12 - COMMANDED SECONDARY AIR STATUS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Upstream
  /// </summary>
  OBD_AIR_STATUS_UPSTREAM = $01;
  /// <summary>
  ///   Downstream of catalytic converter
  /// </summary>
  OBD_AIR_STATUS_DOWNSTREAM_OF_CATALYTIC_CONVERTER = $02;
  /// <summary>
  ///   From the outside atmosphere or off
  /// </summary>
  OBD_AIR_STATUS_OUTSIDE_ATMOSPHERE_OR_OFF = $04;
  /// <summary>
  ///   Pump commanded on for diagnostics
  /// </summary>
  OBD_AIR_STATUS_PUMP_COMMAND_ON_FOR_DIAGNOSTICS = $08;

//------------------------------------------------------------------------------
// SERVICE 01 PID 1C - OBD STANDARDS THIS VEHICLE CONFORMS TO
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   OBD-II as defined by the CARB
  /// </summary>
  OBD_STANDARDS_CARB = $01;
  /// <summary>
  ///   OBD as defined by the EPA
  /// </summary>
  OBD_STANDARDS_EPA = $02;
  /// <summary>
  ///   OBD and OBD-II
  /// </summary>
  OBD_STANDARDS_OBD_OBDII = $03;
  /// <summary>
  ///   OBD-I
  /// </summary>
  OBD_STANDARDS_OBDI = $04;
  /// <summary>
  ///   Not OBD compliant
  /// </summary>
  OBD_STANDARDS_NOT_OBD_COMPLIANT = $05;
  /// <summary>
  ///   EOBD (Europe)
  /// </summary>
  OBD_STANDARDS_EOBD = $06;
  /// <summary>
  ///   EOBD and OBD-II
  /// </summary>
  OBD_STANDARDS_EOBD_OBDII = $07;
  /// <summary>
  ///   EOBD and OBD
  /// </summary>
  OBD_STANDARDS_EOBD_OBD = $08;
  /// <summary>
  ///   EOBD, OBD and OBD II
  /// </summary>
  OBD_STANDARDS_EOBD_OBD_OBDII = $09;
  /// <summary>
  ///   JOBD (Japan)
  /// </summary>
  OBD_STANDARDS_JOBD = $0A;
  /// <summary>
  ///   JOBD and OBD II
  /// </summary>
  OBD_STANDARDS_JOBD_OBDII = $0B;
  /// <summary>
  ///   JOBD and EOBD
  /// </summary>
  OBD_STANDARDS_JOBD_EOBD = $0C;
  /// <summary>
  ///   JOBD, EOBD, and OBD II
  /// </summary>
  OBD_STANDARDS_JOBD_EOBD_OBDII = $0D;
  /// <summary>
  ///   Reserved
  /// </summary>
  OBD_STANDARDS_RESERVED_14 = $0E;
  /// <summary>
  ///   Reserved
  /// </summary>
  OBD_STANDARDS_RESERVED_15 = $0F;
  /// <summary>
  ///   Reserved
  /// </summary>
  OBD_STANDARDS_RESERVED_16 = $10;
  /// <summary>
  ///   Engine Manufacturer Diagnostics (EMD)
  /// </summary>
  OBD_STANDARDS_EMD = $11;
  /// <summary>
  ///   Engine Manufacturer Diagnostics Enhanced (EMD+)
  /// </summary>
  OBD_STANDARDS_EMD_PLUS = $12;
  /// <summary>
  ///   Heavy Duty On-Board Diagnostics (Child/Partial) (HD OBD-C)
  /// </summary>
  OBD_STANDARDS_HD_OBD_C = $13;
  /// <summary>
  ///   Heavy Duty On-Board Diagnostics (HD OBD)
  /// </summary>
  OBD_STANDARDS_HD_OBD = $14;
  /// <summary>
  ///   World Wide Harmonized OBD (WWH OBD)
  /// </summary>
  OBD_STANDARDS_WWH_OBD = $15;
  /// <summary>
  ///   Reserved
  /// </summary>
  OBD_STANDARDS_RESERVED_22 = $16;
  /// <summary>
  ///   Heavy Duty Euro OBD Stage I without NOx control (HD EOBD-I)
  /// </summary>
  OBD_STANDARDS_HD_EOBD_I = $17;
  /// <summary>
  ///   Heavy Duty Euro OBD Stage I with NOx control (HD EOBD-I N)
  /// </summary>
  OBD_STANDARDS_HD_EOBD_I_N = $18;
  /// <summary>
  ///   Heavy Duty Euro OBD Stage II without NOx control (HD EOBD-II)
  /// </summary>
  OBD_STANDARDS_HD_EOBD_II = $19;
  /// <summary>
  ///   Heavy Duty Euro OBD Stage II with NOx control (HD EOBD-II N)
  /// </summary>
  OBD_STANDARDS_HD_EOBD_II_N = $1A;
  /// <summary>
  ///   Reserved
  /// </summary>
  OBD_STANDARDS_RESERVED_27 = $1B;
  /// <summary>
  ///   Brazil OBD Phase 1 (OBDBr-1)
  /// </summary>
  OBD_STANDARDS_OBDBR_1 = $1C;
  /// <summary>
  ///   Brazil OBD Phase 2 (OBDBr-2)
  /// </summary>
  OBD_STANDARDS_OBDBR_2 = $1D;
  /// <summary>
  ///   Korean OBD (KOBD)
  /// </summary>
  OBD_STANDARDS_KOBD = $1E;
  /// <summary>
  ///   India OBD I (IOBD I)
  /// </summary>
  OBD_STANDARDS_IOBD_I = $1F;
  /// <summary>
  ///   India OBD II (IOBD II)
  /// </summary>
  OBD_STANDARDS_IOBD_II = $20;
  /// <summary>
  ///   Heavy Duty Euro OBD Stage VI (HD EOBD-IV)
  /// </summary>
  OBD_STANDARDS_HD_EOBD_IV = $21;

//------------------------------------------------------------------------------
// SERVICE 01 PID 51 - FUEL TYPE CODING
//------------------------------------------------------------------------------
const
    /// <summary>
  ///   Not available
  /// </summary>
  FUEL_TYPE_NOT_AVAILABLE = $00;
  /// <summary>
  ///   Gasoline
  /// </summary>
  FUEL_TYPE_GASOLINE = $01;
  /// <summary>
  ///   Methanol
  /// </summary>
  FUEL_TYPE_METHANOL = $02;
  /// <summary>
  ///   Ethanol
  /// </summary>
  FUEL_TYPE_ETHANOL = $03;
  /// <summary>
  ///   Diesel
  /// </summary>
  FUEL_TYPE_DIESEL = $04;
  /// <summary>
  ///   LPG
  /// </summary>
  FUEL_TYPE_LPG = $05;
  /// <summary>
  ///   CNG
  /// </summary>
  FUEL_TYPE_CNG = $06;
  /// <summary>
  ///   Propane
  /// </summary>
  FUEL_TYPE_PROPANE = $07;
  /// <summary>
  ///   Electric
  /// </summary>
  FUEL_TYPE_ELECTRIC = $08;
  /// <summary>
  ///   Bifuel running Gasoline
  /// </summary>
  FUEL_TYPE_BIFUEL_GASOLINE = $09;
  /// <summary>
  ///   Bifuel running Methanol
  /// </summary>
  FUEL_TYPE_BIFUEL_METHANOL = $0A;
  /// <summary>
  ///   Bifuel running Ethanol
  /// </summary>
  FUEL_TYPE_BIFUEL_ETHANOL = $0B;
  /// <summary>
  ///   Bifuel running LPG
  /// </summary>
  FUEL_TYPE_BIFUEL_LPG = $0C;
  /// <summary>
  ///   Bifuel running CNG
  /// </summary>
  FUEL_TYPE_BIFUEL_CNG = $0D;
  /// <summary>
  ///   Bifuel running Propane
  /// </summary>
  FUEL_TYPE_BIFUEL_PROPANE = $0E;
  /// <summary>
  ///   Bifuel running Electricity
  /// </summary>
  FUEL_TYPE_BIFUEL_ELECTRICITY = $0F;
  /// <summary>
  ///   Bifuel running electric and combustion engine
  /// </summary>
  OBD_FUEL_BIFUEL_ELECTRIC_COMBUSTION = $10;
  /// <summary>
  ///   Hybrid gasoline
  /// </summary>
  OBD_FUEL_HYBRID_GASOLINE = $11;
  /// <summary>
  ///   Hybrid Ethanol
  /// </summary>
  OBD_FUEL_HYBRID_ETHANOL = $12;
  /// <summary>
  ///   Hybrid Diesel
  /// </summary>
  OBD_FUEL_HYBRID_DIESEL = $13;
  /// <summary>
  ///   Hybrid Electric
  /// </summary>
  OBD_FUEL_HYBRID_ELECTRIC = $14;
  /// <summary>
  ///   Hybrid running electric and combustion engine
  /// </summary>
  OBD_FUEL_HYBRID_ELECTRIC_COMBUSTION = $15;
  /// <summary>
  ///   Hybrid Regenerative
  /// </summary>
  OBD_FUEL_HYBRID_REGENERATIVE = $16;
  /// <summary>
  ///   Bifuel running diesel
  /// </summary>
  OBD_FUEL_BIFUEL_DIESEL = $17;

implementation

end.
