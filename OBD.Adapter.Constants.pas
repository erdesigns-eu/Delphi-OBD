//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.Constants.pas
// CONTENTS       : OBD Adapter Constants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 09/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.Constants;

interface

//------------------------------------------------------------------------------
// CONSTANTS (ADAPTER: ELM)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   ELM Message Delimiter: This character indicates the end of the message
  ///   and is used to accumulate incoming data until we have a full message.
  /// </summary>
  ELM_MESSAGE_DELIMITER = '>';
  /// <summary>
  ///   Unsupported command: This character indicates that the request failed,
  ///   this can be due to an incorrectly formattes message, or unrecognized
  ///   or unsupported command.
  /// </summary>
  ELM_UNSUPPORTED_COMMAND = '?';
  /// <summary>
  ///   No Data: The vehicle's ECU (Engine Control Unit) did not respond to the
  ///   request for data. This can happen if the requested PID (Parameter ID)
  ///   is not supported or if the engine is off.
  /// </summary>
  ELM_NO_DATA = 'NO DATA';
  /// <summary>
  ///   Data Error: Too few bytes received, incorrect header format,
  ///   symbol timing, or framing error.
  /// </summary>
  ELM_DATA_ERROR = 'DATA ERROR';
  /// <summary>
  ///   Bus Init Error: The adapter failed to initialize communication with the
  ///   vehicle's ECU. This could be due to various issues, such as a problem
  ///   with the vehicle's OBD-II system, the adapter not being properly
  ///   connected, or the vehicle being incompatible with the OBD-II standards
  ///   the adapter supports.
  /// </summary>
  ELM_BUS_INIT_ERROR = 'BUS INIT: ERROR';
  /// <summary>
  ///   Bus Busy: Indicates that the vehicle's communication bus is currently
  ///   active or busy, preventing the adapter from executing a command
  ///   at that moment.
  /// </summary>
  ELM_BUS_BUSY = 'BUS BUSY';
  /// <summary>
  ///   Bus Error: The IC made an attempt to send an OBD message, but the bus
  ///   voltage did not change as expected. This is most likely due to
  ///   a circuit problem (a short or an open) or the bus being shorted to
  ///   battery voltage or ground.
  /// </summary>
  ELM_BUS_ERROR = 'BUS ERROR';
  /// <summary>
  ///   CAN Error: Indicates a problem with communicating over a
  ///   CAN (Controller Area Network) bus. This could be due to electrical
  ///   issues or signal interference.
  /// </summary>
  ELM_CAN_ERROR = 'CAN ERROR';
  /// <summary>
  ///   Unable to Connect: The adapter could not establish a connection with
  ///   the vehicle's ECU. This might occur for several reasons, including
  ///   issues with the vehicle's OBD-II port, compatibility problems,
  ///   or the vehicle not being OBD-II compliant.
  /// </summary>
  ELM_UNABLE_TO_CONNECT = 'UNABLE TO CONNECT';
  /// <summary>
  ///   Error: A general error message indicating that the adapter encountered
  ///   an unspecified problem.
  /// </summary>
  ELM_ERROR = 'ERROR';
  /// <summary>
  ///   Stopped: This message indicates that the adapter has stopped the
  ///   current operation, usually in response to a stop command from the user.
  /// </summary>
  ELM_STOPPED = 'STOPPED';
  /// <summary>
  ///   Buffer Full: The adapter's internal buffer for storing data has been
  ///   filled before the command could be fully processed. This might happen
  ///   with commands that return large amounts of data.
  /// </summary>
  ELM_BUFFER_FULL = 'BUFFER FULL';

//------------------------------------------------------------------------------
// CONSTANTS (ADAPTER: OBDLINK)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   ACT Alert: The IC will switch to low power mode in 1 minute,
  ///   unless it detects activity on UART.
  /// </summary>
  OBDLINK_ACT_ALERT = 'ACT ALERT';
  /// <summary>
  ///   FB Error: Feedback error. OBDLink detected a mismatch between the
  ///   commanded transmitter state (high or low), and the signal state
  ///   seen at the receiver.
  /// </summary>
  OBDLINK_FB_ERROR = 'FB ERROR';
  /// <summary>
  ///   FC RX Timeout: Timeout error. OBDLink detected a mismatch between the
  ///   commanded transmitter state (high or low), and the signal state
  ///   seen at the receiver.
  /// </summary>
  OBDLINK_FC_RX_TIMEOUT = 'FC RX TIMEOUT';
  /// <summary>
  ///  LP Alert: OBDLink is 2 seconds away from entering Low Power (standby) mode.
  ///  The purpose of this message is to alert the host and allow it sufficient
  ///  time to perform any housekeeping tasks (e.g., save data to nonvolatile
  ///  memory before the power is cut).
  /// </summary>
  OBDLINK_LP_ALERT = 'LP ALERT';
  /// <summary>
  ///   LV Reset: Low voltage reset (also known as "brown-out reset").
  ///   OBDLink has a built-in brown-out reset feature that resets the device
  ///   when the supply voltage drops too low. After the voltage rises back
  ///   above the trip point, the IC performs a full reset and prints "LV RESET".
  /// </summary>
  OBDLINK_LV_RESET = 'LV RESET';
  /// <summary>
  ///   Out of memory: Not enough available RAM to complete the requested operation.
  /// </summary>
  OBDLINK_OUT_OF_MEMORY = 'OUT OF MEMORY';
  /// <summary>
  ///   RX Error: CAN peripheral detected an error in the received message.
  ///   Incorrect baud rate is the most likely cause.
  /// </summary>
  OBDLINK_RX_ERROR = 'RX ERROR';
  /// <summary>
  ///   UART RX Overflow: UART Rx buffer overflow occurred. This error is most
  ///   likely to happen under ISO 9141 and ISO 14230, when a large amount of
  ///   UART data is sent to the OBDLink at a high baud rate, while the device
  ///   is busy transmitting keep-alive messages.
  /// </summary>
  OBDLINK_UART_RX_OVERFLOW = 'UART RX OVERFLOW';

implementation

end.
