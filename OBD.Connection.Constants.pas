//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Const.pas
// CONTENTS       : Serial (COM Port) OBD Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.Constants;

interface

uses Winapi.Windows, WinApi.Messages, OBD.Connection.Types;

//------------------------------------------------------------------------------
// CONSTANTS (SERIAL - COM PORT)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Handle value checked when manually setting the handle. If that handle
  ///   has this value the receive timer is stopped and the handle declared as
  ///   invalid.
  /// </summary>
  RELEASE_NOCLOSE_PORT = HFILE(INVALID_HANDLE_VALUE-1);

  /// <summary>
  ///   Maximum size of the receive buffer in byte
  /// </summary>
  cMaxRXBufferSize = 65535;
  /// <summary>
  ///   Maximum size of the transmit buffer in byte
  /// </summary>
  cMaxTXBufferSize = 65535;
  /// <summary>
  ///   Minimum size of the receive buffer in byte
  /// </summary>
  cMinRXBufferSize = 128;
  /// <summary>
  ///   Minimum size of the transmit buffer in byte
  /// </summary>
  cMinTXBufferSize = 128;

  /// <summary>
  ///   Baudrate: br230400
  /// </summary>
  CBR_230400 = 38400;
  /// <summary>
  ///   Baudrate: br460800
  /// </summary>
  CBR_460800 = 70800;
  /// <summary>
  ///   Baudrate: br500000
  /// </summary>
  CBR_500000 = $7A120;
  /// <summary>
  ///   Baudrate: br921600
  /// </summary>
  CBR_921600 = $E1000;
  /// <summary>
  ///   Baudrate: br1000000
  /// </summary>
  CBR_1000000 = $F4240;
  /// <summary>
  ///   Baudrate: br2000000
  /// </summary>
  CBR_2000000 = $1E8480;
  /// <summary>
  ///   Baudrate: br8000000
  /// </summary>
  CBR_8000000 = $7A1200;
  /// <summary>
  ///   Baudrate: br10000000
  /// </summary>
  CBR_10000000 = 989680;
  /// <summary>
  ///   Baudrate: br12000000
  /// </summary>
  CBR_12000000 = $B71B00;

  /// <summary>
  ///   Baudrates defined in WinAPI
  /// </summary>
  Win32BaudRates: array[br9600..br12000000] of DWORD = (CBR_9600, CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600, CBR_115200, CBR_230400, CBR_460800, CBR_500000, CBR_921600, CBR_1000000, CBR_2000000, CBR_8000000, CBR_10000000, CBR_12000000);

  /// <summary>
  ///   DCB Flags
  /// </summary>
  DCB_Binary                = $00000001;
  DCB_DtrControlEnable      = $00000010;
  DCB_RtsControlEnable      = $00001000;
  DCB_OutxCtsFlow           = $00000004;
  DCB_RtsControlHandshake   = $00002000;
  DCB_OutX                  = $00000100;
  DCB_InX                   = $00000200;

  /// <summary>
  ///   Minimum Polling Delay (ms)
  /// </summary>
  MIN_POLLING_DELAY = 5;
  /// <summary>
  ///   Minimum XONLim Value (bytes)
  /// </summary>
  MIN_XONLIM = 4096;
  /// <summary>
  ///   XON Character (ASCII 17)
  /// </summary>
  XON_CHAR = #17;
  /// <summary>
  ///   XOFF Character (ASCII 19)
  /// </summary>
  XOFF_CHAR = #19;

  /// <summary>
  ///   Default Buffer Size (bytes)
  /// </summary>
  DEFAULT_BUFFER_SIZE = 2048;
  /// <summary>
  ///   Default Polling Delay (ms)
  /// </summary>
  DEFAULT_POLLING_DELAY = 50;
  /// <summary>
  ///   Default Output Timeout (ms)
  /// </summary>
  DEFAULT_OUTPUT_TIMEOUT = 500;
  /// <summary>
  ///   Default Input Timeout (ms)
  /// </summary>
  DEFAULT_INPUT_TIMEOUT = 200;

//------------------------------------------------------------------------------
// CONSTANTS (FTDI - USB)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   FTDI DLL Filename
  /// </summary>
  FTDI_DLL = 'FTD2XX.DLL';

  /// <summary>
  ///   FTDI Message ID
  /// </summary>
  WM_FTDI_EVENT = WM_USER + 10000;
  /// <summary>
  ///   FTDI RX Char Message ID
  /// </summary>
  WM_FTDI_RX_CHAR = WM_FTDI_EVENT + 1;
  /// <summary>
  ///   FTDI Modem Status Message ID
  /// </summary>
  WM_FTDI_MODEM_STATUS = WM_FTDI_EVENT + 2;

  /// <summary>
  ///   Read Timeout (500ms)
  /// </summary>
  FT_READ_TIMEOUT = 500;
  /// <summary>
  ///   Write Timeout (500ms)
  /// </summary>
  FT_WRITE_TIMEOUT = 500;

  /// <sumary>
  ///   FT_Result Value: OK
  /// </summary>
  FT_OK = 0;
  /// <sumary>
  ///   FT_Result Value: Invalid Handle
  /// </summary>
  FT_INVALID_HANDLE = 1;
  /// <sumary>
  ///   FT_Result Value: Device not found
  /// </summary>
  FT_DEVICE_NOT_FOUND = 2;
  /// <sumary>
  ///   FT_Result Value: Device not opened
  /// </summary>
  FT_DEVICE_NOT_OPENED = 3;
  /// <sumary>
  ///   FT_Result Value: I/O Error
  /// </summary>
  FT_IO_ERROR = 4;
  /// <sumary>
  ///   FT_Result Value: Insufficient resources
  /// </summary>
  FT_INSUFFICIENT_RESOURCES = 5;
  /// <sumary>
  ///   FT_Result Value: Invalid Parameter
  /// </summary>
  FT_INVALID_PARAMETER = 6;
  /// <sumary>
  ///   FT_Result Value: Invalid Baud Rate
  /// </summary>
  FT_INVALID_BAUD_RATE = 7;
  /// <sumary>
  ///   FT_Result Value: Not opened for erasing
  /// </summary>
  FT_DEVICE_NOT_OPENED_FOR_ERASE = 8;
  /// <sumary>
  ///   FT_Result Value: Not opened for writing
  /// </summary>
  FT_DEVICE_NOT_OPENED_FOR_WRITE = 9;
  /// <sumary>
  ///   FT_Result Value: Failed to write to the device
  /// </summary>
  FT_FAILED_TO_WRITE_DEVICE = 10;
  /// <sumary>
  ///   FT_Result Value: EEPROM read failed
  /// </summary>
  FT_EEPROM_READ_FAILED = 11;
  /// <sumary>
  ///   FT_Result Value: EEPROM write failed
  /// </summary>
  FT_EEPROM_WRITE_FAILED = 12;
  /// <sumary>
  ///   FT_Result Value: EEPROM erase failed
  /// </summary>
  FT_EEPROM_ERASE_FAILED = 13;
  /// <sumary>
  ///   FT_Result Value: EEPROM not present
  /// </summary>
  FT_EEPROM_NOT_PRESENT = 14;
  /// <sumary>
  ///   FT_Result Value: EEPROM not programmed
  /// </summary>
  FT_EEPROM_NOT_PROGRAMMED = 15;
  /// <sumary>
  ///   FT_Result Value: Invalid arguments
  /// </summary>
  FT_INVALID_ARGS = 16;
  /// <sumary>
  ///   FT_Result Value: Other / Not specified
  /// </summary>
  FT_OTHER_ERROR = 17;
  /// <sumary>
  ///   FT_Result Value: Success
  /// </summary>
  FT_SUCCESS = FT_OK;
  /// <sumary>
  ///   FT_Open_Ex Flags: Open by serial number
  /// </summary>
  FT_OPEN_BY_SERIAL_NUMBER = 1;
  /// <sumary>
  ///   FT_Open_Ex Flags: Open by description
  /// </summary>
  FT_OPEN_BY_DESCRIPTION = 2;
  /// <sumary>
  ///   FT_Open_Ex Flags: Open by location
  /// </summary>
  FT_OPEN_BY_LOCATION = 4;

  /// <sumary>
  ///   FT_List_Devices Flags: List only serial numbers
  /// </summary>
  FT_LIST_NUMBER_ONLY = $80000000;
  /// <sumary>
  ///   FT_List_Devices Flags: List only indexes (0, 1, 2, ..)
  /// </summary>
  FT_LIST_BY_INDEX = $40000000;
  /// <sumary>
  ///   FT_List_Devices Flags: List all
  /// </summary>
  FT_LIST_ALL = $20000000;

  /// <sumary>
  ///   Parity Selection: None
  /// </summary>
  FT_PARITY_NONE = 0;
  /// <sumary>
  ///   Parity Selection: Odd
  /// </summary>
  FT_PARITY_ODD = 1;
  /// <sumary>
  ///   Parity Selection: Even
  /// </summary>
  FT_PARITY_EVEN = 2;
  /// <sumary>
  ///   Parity Selection: Mark
  /// </summary>
  FT_PARITY_MARK = 3;
  /// <sumary>
  ///   Parity Selection: Space
  /// </summary>
  FT_PARITY_SPACE = 4;
  /// <sumary>
  ///   Flow Control Selection: None
  /// </summary>
  FT_FLOW_NONE = $0000;
  /// <sumary>
  ///   Flow Control Selection: RTS/CTS
  /// </summary>
  FT_FLOW_RTS_CTS = $0100;
  /// <sumary>
  ///   Flow Control Selection: DTR/DSR
  /// </summary>
  FT_FLOW_DTR_DSR = $0200;
  /// <sumary>
  ///   Flow Control Selection: XON/XOFF
  /// </summary>
  FT_FLOW_XON_XOFF = $0400;

  /// <sumary>
  ///   Purge Commands: RX
  /// </summary>
  FT_PURGE_RX = 1;
  /// <sumary>
  ///   Purge Commands: TX
  /// </summary>
  FT_PURGE_TX = 2;

  /// <sumary>
  ///   Notification Events: RX Char
  /// </summary>
  FT_EVENT_RXCHAR = 1;
  /// <sumary>
  ///   Notification Events: Modem Status
  /// </summary>
  FT_EVENT_MODEM_STATUS = 2;
  /// <sumary>
  ///   Notification Events: Line Status
  /// </summary>
  FT_EVENT_LINE_STATUS = 4;

  /// <sumary>
  ///   Modem Status: Clear to send
  /// </summary>
  FT_MODEM_STATUS_CTS = $10;
  /// <sumary>
  ///   Notification Events: Data set ready
  /// </summary>
  FT_MODEM_STATUS_DSR = $20;
  /// <sumary>
  ///   Notification Events: Ring indicator
  /// </summary>
  FT_MODEM_STATUS_RI = $40;
  /// <sumary>
  ///   Notification Events: Data carrier detect
  /// </summary>
  FT_MODEM_STATUS_DCD = $80;

  /// <sumary>
  ///   IO Buffer Sizes: In Buffer (64k)
  /// </summary>
  FT_In_Buffer_Size = $10000;
  /// <sumary>
  ///   IO Buffer Sizes: In Buffer Length
  /// </summary>
  FT_In_Buffer_Length = FT_In_Buffer_Size - 1;
  /// <sumary>
  ///   IO Buffer Sizes: Out Buffer (64k)
  /// </summary>
  FT_Out_Buffer_Size = $10000;
  /// <sumary>
  ///   IO Buffer Sizes: Out Buffer Length
  /// </summary>
  FT_Out_Buffer_Length = FT_Out_Buffer_Size - 1;

//------------------------------------------------------------------------------
// CONSTANTS (Bluetooth)
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   FTDI Message ID
  /// </summary>
  WM_BLUETOOTH_EVENT = WM_USER + 11000;

  /// <sumary>
  ///   IO Buffer Sizes: In Buffer (64k)
  /// </summary>
  BT_In_Buffer_Size = $10000;
  /// <sumary>
  ///   IO Buffer Sizes: In Buffer Length
  /// </summary>
  BT_In_Buffer_Length = FT_In_Buffer_Size - 1;
  /// <sumary>
  ///   IO Buffer Sizes: Out Buffer (64k)
  /// </summary>
  BT_Out_Buffer_Size = $10000;
  /// <sumary>
  ///   IO Buffer Sizes: Out Buffer Length
  /// </summary>
  BT_Out_Buffer_Length = FT_Out_Buffer_Size - 1;

  /// <summary>
  ///   Bluetooth Service GUID (COM PORT)
  /// </summary>
  BT_SERVICE_GUID = '00001101-0000-1000-8000-00805F9B34FB';


implementation

end.
