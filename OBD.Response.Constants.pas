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

implementation

end.
