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
  ///   General Reject
  /// </summary>
  OBD_GENERAL_REJECT = $10;
  /// <summary>
  ///   Service not supported
  /// </summary>
  OBD_SERVICE_NOT_SUPPORTED = $11;
  /// <summary>
  ///   Sub-function not supportedd
  /// </summary>
  OBD_SUB_FUNCTION_NOT_SUPPORTED = $12;
  /// <summary>
  ///   Incorrect message length or invalid format
  /// </summary>
  OBD_INCORRECT_MESSAGE_LENGTH_OR_INVALID_FORMAT = $13;
  /// <summary>
  ///   Response too long
  /// </summary>
  OBD_RESPONSE_TOO_LONG = $14;
  /// <summary>
  ///   Busy repeat request
  /// </summary>
  OBD_BUSY_REPEAT_REQUEST = $21;
  /// <summary>
  ///   Conditions not correct or Request Sequence Error
  /// </summary>
  OBD_CONDITIONS_NOT_CORRECT_OR_REQUEST_SEQUENCE_ERROR = $22;
  /// <summary>
  ///   Request Sequence Error
  /// </summary>
  OBD_REQUEST_SEQUENCE_ERROR = $24;
  /// <summary>
  ///   No response from Subnet Component
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
  ///   Security Access Denied
  /// </summary>
  OBD_SECURITY_ACCESS_DENIED = $33;
  /// <summary>
  ///   Invalid Key
  /// </summary>
  OBD_INVALID_KEY = $35;
  /// <summary>
  ///   Exceed number of attempts
  /// </summary>
  OBD_EXCEED_NUMBER_OF_ATTEMPTS = $36;
  /// <summary>
  ///   Required time delay not expired
  /// </summary>
  OBD_REQUIRED_TIME_DELAY_NOT_EXPIRED = $37;
  /// <summary>
  ///   Upload Download not accepted
  /// </summary>
  OBD_UPLOAD_DOWNLOAD_NOT_ACCEPTED = $70;
  /// <summary>
  ///   Transfer Data Suspended
  /// </summary>
  OBD_TRANSFER_DATA_SUSPENDED = $71;
  /// <summary>
  ///   General Programming Failure
  /// </summary>
  OBD_GENERAL_PROGRAMMING_FAILURE = $72;
  /// <summary>
  ///   Wrong Block Sequence Counter
  /// </summary>
  OBD_WRONG_BLOCK_SEQUENCE_COUNTER = $73;
  /// <summary>
  ///   Request correctly received, but response is deferred
  /// </summary>
  OBD_REQUEST_CORRECTLY_RECEIVED_BUT_RESPONSE_IS_DEFERRED = $78;
  /// <summary>
  ///   Sub-function not supported in active session
  /// </summary>
  OBD_SUB_FUNCTION_NOT_SUPPORTED_IN_ACTIVE_SESSION = $7E;
  /// <summary>
  ///   Service not supported in active session
  /// </summary>
  OBD_SERVICE_NOT_SUPPORTED_IN_ACTIVE_SESSION = $7F;

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
