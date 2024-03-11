//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.STCommands.pas
// CONTENTS       : OBD Adapter ST Commands
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/03/2024
// NOTE           : Commands found in stn1100-frpm.pdf
//------------------------------------------------------------------------------
unit OBD.Adapter.STCommands;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ST Command Exception
  /// </summary>
  TSTCommandException = class(Exception);

//------------------------------------------------------------------------------
// RECORDS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   ST Command Parameter
  /// </summary>
  STCommandParam = record
    /// <summary>
    ///   Parameter Name
    /// </summary>
    Param: string;
    /// <summary>
    ///   Parameter description
    /// </summary>
    Description: string;
  end;

  /// <summary>
  ///   ST Command
  /// </summary>
  STCommand = record
    /// <summary>
    ///   Command
    /// </summary>
    Command: string;
    /// <summary>
    ///   Description of the command
    /// </summary>
    Description: string;
    /// <summary>
    ///   Group the command belongs to
    /// </summary>
    Group: string;
    /// <summary>
    ///   Number of parameters the command has
    /// </summary>
    Params: 0..7;
    /// <summary>
    ///   Parameter 1
    /// </summary>
    Param1: STCommandParam;
    /// <summary>
    ///   Parameter 2
    /// </summary>
    Param2: STCommandParam;
    /// <summary>
    ///   Parameter 3
    /// </summary>
    Param3: STCommandParam;
    /// <summary>
    ///   Parameter 4
    /// </summary>
    Param4: STCommandParam;
    /// <summary>
    ///   Parameter 5
    /// </summary>
    Param5: STCommandParam;
    /// <summary>
    ///   Parameter 6
    /// </summary>
    Param6: STCommandParam;
    /// <summary>
    ///   Parameter 7
    /// </summary>
    Param7: STCommandParam;
  end;

//------------------------------------------------------------------------------
// ST COMMANDS: GENERAL (TABLE 11)
//------------------------------------------------------------------------------
const
  READ_VOLTAGE_CALIBRATION_STATUS: STCommand = (
    Command     : 'CALSTAT';
    Description : 'Read voltage calibration status';
    Group       : 'General';
    Params      : 0;
  );
  RESET_NVM_FACTORY_DEFAULTS: STCommand = (
    Command     : 'RSTNVM';
    Description : 'Reset NVM to factory defaults';
    Group       : 'General';
    Params      : 0;
  );
  SAVE_ALL_CALIBRATION_VALUES: STCommand = (
    Command     : 'SAVCAL';
    Description : 'Save all calibration values';
    Group       : 'General';
    Params      : 0;
  );
  DISABLE_LEDS: STCommand = (
    Command     : 'UIL 0';
    Description : 'Disable leds';
    Group       : 'General';
    Params      : 0;
  );
  ENABLE_LEDS: STCommand = (
    Command     : 'UIL 1';
    Description : 'Enable leds';
    Group       : 'General';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: UART (TABLE 12)
//------------------------------------------------------------------------------
const
  SWITCH_UART_BAUD_RATE_SW_FRIENDLY: STCommand = (
    Command     : 'BR %d';
    Description : 'Switch UART baud rate in software-friendly way';
    Group       : 'UART';
    Params      : 1;
    Param1      : (Param: 'Baud rate'; Description: 'The baud rate is specified as a decimal number in baud.');
  );
  SET_UART_BAUD_RATE_SWITCH_TIMEOUT: STCommand = (
    Command     : 'BRT %d';
    Description : 'Set UART baud rate switch timeout';
    Group       : 'UART';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'The timeout is specified as a decimal value in milliseconds and the maximum timeout is 65535 ms (65.5 seconds).');
  );
  SWITCH_UART_BAUD_RATE_TERMINAL_FRIENDLY: STCommand = (
    Command     : 'SBR %d';
    Description : 'Switch UART baud rate in terminal-friendly way';
    Group       : 'UART';
    Params      : 1;
    Param1      : (Param: 'Baud rate'; Description: 'The baud rate is specified as a decimal number in baud (38 to 10000000).');
  );
  SET_UART_FLOW_CONTROL_MODE_OFF: STCommand = (
    Command     : 'UFC 0';
    Description : 'Set UART flow control mode off';
    Group       : 'UART';
    Params      : 0;
  );
  SET_UART_FLOW_CONTROL_MODE_ON: STCommand = (
    Command     : 'UFC 1';
    Description : 'Set UART flow control mode on';
    Group       : 'UART';
    Params      : 0;
  );
  WRITE_CURRENT_BAUD_RATE_TO_NVM: STCommand = (
    Command     : 'WBR';
    Description : 'Write current UART baud rate to NVM';
    Group       : 'UART';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: DEVICE ID (TABLE 13)
//------------------------------------------------------------------------------
const
  PRINT_DEVICE_HARDWARE: STCommand = (
    Command     : 'DI';
    Description : 'Print device hardware ID string (e.g., "OBDLink r1.7")';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_ENGINE_START_COUNT: STCommand = (
    Command     : 'DICES';
    Description : 'Print engine start count';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_POWER_ON_RESET_COUNT: STCommand = (
    Command     : 'DICPO';
    Description : 'Print POR (Power on Reset) count';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_POWER_ON_RESET_TIMER: STCommand = (
    Command     : 'DITPO';
    Description : 'Print POR (Power on Reset) timer';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_FIRMWARE_ID: STCommand = (
    Command     : 'I';
    Description : 'Print firmware ID string (e.g., "STN1100 v1.2.3")';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_EXTENDED_FIRMWARE_ID: STCommand = (
    Command     : 'IX';
    Description : 'Print extended firmware ID string';
    Group       : 'Device ID';
    Params      : 0;
  );
  PRINT_DEVICE_MANUFACTURER_ID: STCommand = (
    Command     : 'MFR';
    Description : 'Print device manufacturer ID string';
    Group       : 'Device ID';
    Params      : 0;
  );
  SET_ATI_DEVICE_ID: STCommand = (
    Command     : 'SATI';
    Description : 'Set ATI device ID string';
    Group       : 'Device ID';
    Params      : 1;
    Param1      : (Param: 'Device ID'; Description: 'Accepts printable ASCII characters (0x20 to 0x7E). Maximum length is 31 characters. Leading and trailing spaces will be ignored.');
  );
  SET_DEVICE_HARDWARE_ID: STCommand = (
    Command     : 'SDI';
    Description : 'Set device hardware ID string';
    Group       : 'Device ID';
    Params      : 1;
    Param1      : (Param: 'Hardware ID'; Description: 'Accepts printable ASCII characters (0x20 to 0x7E). Maximum length is 47 characters. Leading and trailing spaces will be ignored.');
  );
  PRINT_DEVICE_SERIAL_NUMBER: STCommand = (
    Command     : 'SN';
    Description : 'Print device serial number';
    Group       : 'Device ID';
    Params      : 0;
  );
  SET_AT1_DEVICE_DESCRIPTION: STCommand = (
    Command     : 'S@1';
    Description : 'Set AT@1 device description string';
    Group       : 'Device ID';
    Params      : 1;
    Param1      : (Param: 'Device ID'; Description: 'Accepts printable ASCII characters (0x20 to 0x7E). Maximum length is 47 characters. Leading and trailing spaces will be ignored.');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: VOLTAGE (TABLE 14)
//------------------------------------------------------------------------------
const
  CALIBRATE_VOLTAGE_MEASUREMENT: STCommand = (
    Command     : 'VCAL %s, %s';
    Description : 'Calibrate voltage measurement';
    Group       : 'Voltage';
    Params      : 2;
    Param1      : (Param: 'Volts'; Description: 'Takes current voltage with a maximum value of 65.534, and a maximum precision of three decimal places.');
    Param2      : (Param: 'Offset'; Description: 'The optional offset parameter specifies voltage offset. Some devices have the ANALOG_IN input connected to the measured voltage with a constant voltage offset (e.g.: a series diode).');
  );
  READ_VOLTAGE_IN_VOLTS: STCommand = (
    Command     : 'VR %s';
    Description : 'Read voltage in volts';
    Group       : 'Voltage';
    Params      : 1;
    Param1      : (Param: 'Precision'; Description: 'The optional precision parameter specifies precision in digits after decimal point (0 to 3). Default precision is two decimal points.');
  );
  READ_VOLTAGE_IN_ADC_STEPS: STCommand = (
    Command     : 'VRX';
    Description : 'Read voltage in ADC steps';
    Group       : 'Voltage';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: OBD PROTOCOL (TABLE 15)
//------------------------------------------------------------------------------
const
  SET_CURRENT_PROTOCOL: STCommand = (
    Command     : 'P %d';
    Description : 'Set current protocol';
    Group       : 'OBD Protocol';
    Params      : 1;
    Param1      : (Param: 'OBD Protocol'; Description: 'Set current protocol preset. This command selects the physical transceiver to be used for communication, and sets the attributes such as header size, baud rate, etc.');
  );
  SET_CURRENT_PROTOCOL_BAUD_RATE: STCommand = (
    Command     : 'PBR %d';
    Description : 'Set current OBD protocol baud rate';
    Group       : 'OBD Protocol';
    Params      : 1;
    Param1      : (Param: 'Baud rate'; Description: 'Takes bit rate in bps as a decimal number.');
  );
  REPORT_ACTUAL_PROTOCOL_BAUD_RATE: STCommand = (
    Command     : 'PBRR';
    Description : 'Report actual OBD protocol baud rate';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  CLOSE_CURRENT_PROTOCOL: STCommand = (
    Command     : 'PC';
    Description : 'Close current protocol';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  SET_AUTOMATIC_CHECK_BYTE_CALCULATION_OFF: STCommand = (
    Command     : 'PCB 0';
    Description : 'Turn automatic check byte calculation and checking off';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  SET_AUTOMATIC_CHECK_BYTE_CALCULATION_ON: STCommand = (
    Command     : 'PCB 1';
    Description : 'Turn automatic check byte calculation and checking on';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  OPEN_CURRENT_PROTOCOL: STCommand = (
    Command     : 'PO';
    Description : 'Open current protocol';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  REPORT_CURRENT_PROTOCOL_NUMBER: STCommand = (
    Command     : 'PR';
    Description : 'Report current protocol number';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  REPORT_CURRENT_PROTOCOL_STRING: STCommand = (
    Command     : 'PRS';
    Description : 'Report current protocol string';
    Group       : 'OBD Protocol';
    Params      : 0;
  );
  SET_OBD_REQUEST_TIMEOUT: STCommand = (
    Command     : 'PTO %d';
    Description : 'Set OBD request timeout';
    Group       : 'OBD Protocol';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'Takes a decimal parameter in milliseconds (1 to 65535). 0: timeout is infinite. The default setting is controlled by PP 03. Default is 102 ms.');
  );
  SET_MESSAGE_TRANSMISSION_TIMEOUT: STCommand = (
    Command     : 'PTOT %d';
    Description : 'Set message transmission timeout';
    Group       : 'OBD Protocol';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'Takes a decimal parameter in milliseconds (1 to 65535). The default for SAE J1850 PWM/VPW is 300 ms, ISO 9141-2/ISO 14230-4 is 300 ms andISO 15765-4 (CAN) is 50 ms.');
  );
  SET_MINIMUM_TIME_BETWEEN_LAST_RESPONSE_NEXT_REQUEST: STCommand = (
    Command     : 'PTRQ %d';
    Description : 'Set minimum time between last response and next request';
    Group       : 'OBD Protocol';
    Params      : 1;
    Param1      : (Param: 'Time'; Description: 'Takes a decimal parameter in milliseconds (1 to 65535). For ISO 9141-2 and ISO 14230-4 protocols, this is the P3 timing. The default for ISO 9141-2 and ISO 14230-4 is 56. For all others, it is 0.');
  );
  SEND_ARBITRARY_MESSAGE: STCommand = (
    Command     : 'PX';
    Description : 'Send arbitrary message';
    Group       : 'OBD Protocol';
    Params      : 7;
    Param1      : (Param: 'Header'; Description: 'Header / CAN ID. If omitted, value, set by ATSH command is used.');
    Param2      : (Param: 'Data'; Description: 'If specified, [data length] parameter is not allowed.');
    Param3      : (Param: 'Data length'; Description: 'If specified, [data] parameter is not allowed. Instead, after the command is issued, user is prompted for data.');
    Param4      : (Param: 'Response timeout'; Description: ' If specified, overrides value, set by STPTO command. ATAT setting is still obeyed.');
    Param5      : (Param: 'Expected response count'; Description: 'Overrides ATR command setting.');
    Param6      : (Param: 'Extra data'; Description: 'ISO 15765: extended address (overrides value, set by ATCEA command). ISO 9141: expected response length in bytes.');
    Param7      : (Param: 'Flags'; Description: '(Binary-encoded hex) b0 – auto checksum flag enabled, b16 – auto checksum (0: on, 1: off).');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: ISO (TABLE 16)
//------------------------------------------------------------------------------
const
  SET_ADAPTIVE_MAXIMUM_INTERBYTE_TIMING_OFF: STCommand = (
    Command     : 'IAT 0';
    Description : 'Turn adaptive maximum interbyte timing (P1 max) off';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ADAPTIVE_MAXIMUM_INTERBYTE_TIMING_ON: STCommand = (
    Command     : 'IAT 1';
    Description : 'Turn adaptive maximum interbyte timing (P1 max) on';
    Group       : 'ISO';
    Params      : 0;
  );
  PERFORM_CUSTOM_ISO_FAST_INITIALIZATION: STCommand = (
    Command     : 'IFI %d, %d, %s';
    Description : 'Perform custom ISO fast initialization';
    Group       : 'ISO';
    Params      : 3;
    Param1      : (Param: 'LOW time'; Description: 'Initialization sequence LOW time (in milliseconds).');
    Param2      : (Param: 'HIGH time'; Description: 'HIGH time (in milliseconds).');
    Param3      : (Param: 'Message'; Description: ' HEX string for the init message.');
  );
  SET_MAXIMUM_INTERBYTE_TIME_FOR_RECEIVING_MESSAGES: STCommand = (
    Command     : 'IP1X %d';
    Description : 'Set maximum interbyte time for receiving messages (P1 max)';
    Group       : 'ISO';
    Params      : 1;
    Param1      : (Param: 'Time'; Description: ' Takes a decimal parameter in milliseconds. Maximum is 65535 ms (65.5 seconds). Default is 20 ms.');
  );
  SET_INTERBYTE_TIME_FOR_TRANSMITTING_MESSAGES: STCommand = (
    Command     : 'IP4 %d';
    Description : 'Set interbyte time for transmitting messages (P4)';
    Group       : 'ISO';
    Params      : 1;
    Param1      : (Param: 'Time'; Description: 'Takes a decimal parameter in milliseconds. Maximum is 65535 ms (65.5 seconds). Default is 5 ms');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: CAN (TABLE 17)
//------------------------------------------------------------------------------
const
  SET_CAN_ADDRESSING_FORMAT: STCommand = (
    Command     : 'CAF';
    Description : 'Set CAN addressing format';
    Group       : 'CAN';
    Params      : 2;
    Param1      : (Param: 'Format'; Description: 'CAN addressing format. 0 - normal, 1 - extended with target address, 2 - mixed with target address extension');
    Param2      : (Param: 'Target address extension'; Description: 'Target address extension. (Required for formats 1 and 2).');
  );
  ADD_FLOW_CONTROL_ADDRESS_PAIR: STCommand = (
    Command     : 'CFCPA %s, %s';
    Description : 'Add flow control address pair';
    Group       : 'CAN';
    Params      : 2;
    Param1      : (Param: 'TX address'; Description: 'Three-digit or eight-digit (Optionally five-digit or ten-digit). Transmitter ID (i.e. ID transmitted by the OBDLink).');
    Param2      : (Param: 'RX address'; Description: 'Three-digit or eight-digit (Optionally five-digit or ten-digit). Receiver ID (i.e. ID transmitted by the ECU).');
  );
  CLEAR_ALL_FLOW_CONTROL_ADDRESS_PAIRS: STCommand = (
    Command     : 'CFCPC';
    Description : 'Clear all flow control address pairs';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_MONITORING_MODE: STCommand = (
    Command     : 'CMM %d';
    Description : 'Set CAN monitoring mode';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Mode'; Description: 'Specifies whether OBDLink should acknowledge received frames or remains silent. The factory default is 0. (0 - Receive only – no CAN ACKs (default), 1 - Normal node – with CAN ACKs, 2 - Receive all frames, including frames with errors – no CAN ACKs).');
  );
  SET_CAN_RX_SEGMENTATION_OFF: STCommand = (
    Command     : 'TCSEGR 0';
    Description : 'Turn CAN Rx segmentation off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_RX_SEGMENTATION_ON: STCommand = (
    Command     : 'TCSEGR 1';
    Description : 'Turn CAN Rx segmentation on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_TX_SEGMENTATION_OFF: STCommand = (
    Command     : 'CSEGT 0';
    Description : 'Turn CAN Tx segmentation off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_TX_SEGMENTATION_ON: STCommand = (
    Command     : 'CSEGT 1';
    Description : 'Turn CAN Tx segmentation on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_ST_MIN_DELAY_OFFSET: STCommand = (
    Command     : 'CSTM';
    Description : 'Set delay offset for STmin';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'Set additional time for ISO 15765-2 minimum Separation Time (STmin) during multi-frame message transmission. The combined values have a maximum of 127 milliseconds. Timeout can be set in submilliseconds, with at most 3 decimal places.');
  );
  SET_SINGLE_WIRE_CAN_TRANCEIVER_MODE: STCommand = (
    Command     : 'CSWM';
    Description : 'Set Single Wire CAN transceiver mode';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Mode'; Description: 'Bit-encoded 3-bit number (0 to 8) that controls the three single-wire CAN transceiver control pins.');
  );
  SET_CAN_FC_CF_RX_TIMEOUTS: STCommand = (
    Command     : 'CTOR %d, %d';
    Description : 'Set CAN FC and CF Rx timeouts';
    Group       : 'CAN';
    Params      : 2;
    Param1      : (Param: 'Timeout'; Description: 'Timeouts are specified in milliseconds, default 75 ms.');
    Param2      : (Param: 'Timeout'; Description: 'Timeouts are specified in milliseconds, default 150 ms.');
  );
  SET_CAN_TIMING_CONFIGURATION_REGISTERS: STCommand = (
    Command     : 'CTR';
    Description : 'Set CAN timing configuration registers (This command can be used for custom timing on the CAN module.)';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Configuration'; Description: '6-digit hex value that will be written to the registers.');
  );
  READ_CAN_TIMING_CONFIGURATION: STCommand = (
    Command     : 'CTRR';
    Description : 'Read CAN timing configuration';
    Group       : 'CAN';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: MONITORING (TABLE 18)
//------------------------------------------------------------------------------
const
  MONITOR_BUS_USING_CURRENT_FILTERS: STCommand = (
    Command     : 'M';
    Description : 'Monitor OBD bus using current filters';
    Group       : 'Monitoring';
    Params      : 0;
  );
  MONITOR_ALL_MESSAGES_ON_BUS: STCommand = (
    Command     : 'MA';
    Description : 'Monitor all messages on OBD bus';
    Group       : 'Monitoring';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: FILTERING (TABLE 19)
//------------------------------------------------------------------------------
const
  ENABLE_AUTOMATIC_FILTERING: STCommand = (
    Command     : 'FA';
    Description : 'Enable automatic filtering';
    Group       : 'Filtering';
    Params      : 0;
  );
  CLEAR_ALL_FILTERS: STCommand = (
    Command     : 'FAC';
    Description : 'Clear all filters';
    Group       : 'Filtering';
    Params      : 0;
  );
  ADD_BLOCK_FILTER: STCommand = (
    Command     : 'FBA %s, %s';
    Description : 'Add block filter';
    Group       : 'Filtering';
    Params      : 2;
    Param1      : (Param: 'Pattern'; Description: 'Pattern can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Mask.');
    Param2      : (Param: 'Mask'; Description: 'Mask can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Pattern.');
  );
  CLEAR_ALL_BLOCK_FILTERS: STCommand = (
    Command     : 'FBC';
    Description : 'Clear all block filters';
    Group       : 'Filtering';
    Params      : 0;
  );
  ADD_CAN_CONTROL_FLOW_FILTER: STCommand = (
    Command     : 'FBCA %s, %s';
    Description : 'Add CAN flow control filter';
    Group       : 'Filtering';
    Params      : 2;
    Param1      : (Param: 'Pattern'; Description: 'Pattern can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Mask.');
    Param2      : (Param: 'Mask'; Description: 'Mask can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Pattern.');
  );
  CLEAR_ALL_CAN_CONTROL_FLOW_FILTERS: STCommand = (
    Command     : 'FFCC';
    Description : 'Clear all CAN flow control filters';
    Group       : 'Filtering';
    Params      : 0;
  );
  ADD_PASS_FILTER: STCommand = (
    Command     : 'FPA %s, %s';
    Description : 'Add pass filter';
    Group       : 'Filtering';
    Params      : 2;
    Param1      : (Param: 'Pattern'; Description: 'Pattern can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Mask.');
    Param2      : (Param: 'Mask'; Description: 'Mask can be any length from 0 to 5 bytes (0 to 10 ASCII characters), but has to be the same length as Pattern.');
  );
  CLEAR_ALL_PASS_FILTERS: STCommand = (
    Command     : 'FPC';
    Description : 'Clear all pass filters';
    Group       : 'Filtering';
    Params      : 0;
  );
  ADD_SEA_J1939_PGN_FILTER: STCommand = (
    Command     : 'FPGA %s, %s';
    Description : 'Add SAE J1939 PGN filter';
    Group       : 'Filtering';
    Params      : 2;
    Param1      : (Param: 'PGN'; Description: 'PGN is specified as a hexadecimal number 4 to 6 digits in length. If the specified PGN is shorter than 6 digits, leading 0s will be prepended.');
    Param2      : (Param: 'Target address'; Description: 'See Reference and Programming Manual.');
  );
  CLEAR_ALL_SEA_J1939_PGN_FILTERS: STCommand = (
    Command     : 'FPGC';
    Description : 'Clear all SAE J1939 PGN filters';
    Group       : 'Filtering';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// ST COMMANDS: POWERSAVE (TABLE 20)
//------------------------------------------------------------------------------
const
  PRINT_ACTIVE_POWERSAVE_CONFIGURATION_SUMMARY: STCommand = (
    Command     : 'SLCS';
    Description : 'Print active PowerSave configuration summary';
    Group       : 'PowerSave';
    Params      : 0;
  );
  ENTER_SLEEP_MODE_WITH_DELAY: STCommand = (
    Command     : 'SLEEP %d';
    Description : 'Enter sleep mode with delay';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'Delay'; Description: 'Takes delay parameter in seconds.');
  );
  ENTER_SLEEP_MODE: STCommand = (
    Command     : 'SLEEP';
    Description : 'Enter sleep mode';
    Group       : 'PowerSave';
    Params      : 0;
  );
  REPORT_LAST_SLEEP_WAKEUP_TRIGGERS: STCommand = (
    Command     : 'SLLT';
    Description : 'Report last sleep/wakeup triggers';
    Group       : 'PowerSave';
    Params      : 0;
  );
  SET_PWR_CTRL_OUTPUT_POLARITY: STCommand = (
    Command     : 'SLPCP %d';
    Description : 'Set PWR_CTRL output polarity';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'PWR_CTRL'; Description: '0 - (Normal power = HIGH, Low power mode = LOW), 1 - (Normal power = LOW, Low power mode = HIGH), The default setting is 0.');
  );
  SET_UART_SLEEP_WAKEUP_TRIGGERS: STCommand = (
    Command     : 'SLU %s, %s';
    Description : 'UART sleep/wakeup triggers on/off';
    Group       : 'PowerSave';
    Params      : 2;
    Param1      : (Param: 'Sleep'; Description: 'Sleep trigger (UART inactivity timeout) setting, the default setting is "off". Each of the two parameters can be independently configured as "on" or "off".');
    Param2      : (Param: 'Wakeup'; Description: 'Wakeup trigger (low pulse on UART Rx input) setting, the default setting is "on". Each of the two parameters can be independently configured as "on" or "off".');
  );
  SET_UART_INACTIVITY_TIMEOUT: STCommand = (
    Command     : 'SLUIT %d';
    Description : 'Set UART inactivity timeout';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'Specified in seconds (decimal). The default is 1200 (20 minutes).');
  );
  SET_UART_WAKEUP_PULSE_TIMING: STCommand = (
    Command     : 'SLUWP %d, %d';
    Description : 'Set UART wakeup pulse timing';
    Group       : 'PowerSave';
    Params      : 2;
    Param1      : (Param: 'Min'; Description: 'Specified in microseconds. The defaults are min = 0, max = 30000 (30 milliseconds).');
    Param2      : (Param: 'Max'; Description: 'Specified in microseconds. The defaults are min = 0, max = 30000 (30 milliseconds).');
  );
  VOLTAGE_CHANGE_WAKEUP_TRIGGER_OFF: STCommand = (
    Command     : 'SLVG off';
    Description : 'Voltage change wakeup trigger off';
    Group       : 'PowerSave';
    Params      : 0;
  );
  VOLTAGE_CHANGE_WAKEUP_TRIGGER_ON: STCommand = (
    Command     : 'SLVG on';
    Description : 'Voltage change wakeup trigger on';
    Group       : 'PowerSave';
    Params      : 0;
  );
  SET_VOLTAGE_CHANGE_WAKEUP_TRIGGER_CONFIGURATION: STCommand = (
    Command     : 'SLVGW %s%s, %d';
    Description : 'Set configuration of the voltage change wakeup trigger';
    Group       : 'PowerSave';
    Params      : 3;
    Param1      : (Param: '+/- sign'; Description: 'The optional ‘+’ or ‘-’ sign, preceding the voltage, specifies whether the trigger detects only rising voltage (+), only falling voltage (-), or a voltage change in any direction (no sign)');
    Param2      : (Param: 'Volts'; Description: 'Specifies the voltage change. The default setting is 0.2.');
    Param3      : (Param: 'Time'; Description: 'Specifies the time between the samples in milliseconds. The default setting is 1000, one second between the samples.');
  );
  SET_VOLTAGE_LEVEL_SLEEP_WAKEUP_TRIGGERS: STCommand = (
    Command     : 'SLVL %s, %s';
    Description : 'Voltage level sleep/wakeup triggers on/off';
    Group       : 'PowerSave';
    Params      : 2;
    Param1      : (Param: 'Sleep'; Description: 'Specifies the sleep trigger setting, the default setting is "off". Each of the two parameters can be independently configured as "on" or "off".');
    Param2      : (Param: 'Wakeup'; Description: 'Specifies the wakeup trigger setting, the default setting is "off". Each of the two parameters can be independently configured as "on" or "off".');
  );
  SET_VOLTAGE_LEVEL_SLEEP_TRIGGER_CONFIGURATION: STCommand = (
    Command     : 'LVLS %s%s, %d';
    Description : 'Set configuration of the voltage level sleep trigger';
    Group       : 'PowerSave';
    Params      : 3;
    Param1      : (Param: 'Above/Below'; Description: 'The "<" or ">" character specifies whether the trigger region is above or below the threshold voltage: "<" = below, ">" = above.');
    Param2      : (Param: 'Volts'; Description: 'The threshold voltage can be specified in volts with the maximum precision of two decimal places. It can also be specified in raw ADC steps by prefixing the value with ‘0x’.');
    Param3      : (Param: 'Time'; Description: 'Specifies how long the voltage must remain above or below the threshold before the device will enter sleep mode in seconds.');
  );
  SET_VOLTAGE_LEVEL_WAKEUP_TRIGGER_CONFIGURATION: STCommand = (
    Command     : 'SLVLW %s%s, %d';
    Description : 'Set configuration of the voltage level wakeup trigger';
    Group       : 'PowerSave';
    Params      : 3;
    Param1      : (Param: 'Above/Below'; Description: 'The "<" or ">" character specifies whether the trigger region is above or below the threshold voltage: "<" = below, ">" = above.');
    Param2      : (Param: 'Volts'; Description: 'The threshold voltage can be specified in volts with the maximum precision of two decimal places. It can also be specified in raw ADC steps by prefixing the value with ‘0x’.');
    Param3      : (Param: 'Time'; Description: 'Specifies how long the voltage must remain above or below the threshold before the device will enter sleep mode in seconds.');
  );
  SET_EXTERNAL_SLEEP_TRIGGER: STCommand = (
    Command     : 'SLX %s, %s';
    Description : 'External sleep trigger on/off';
    Group       : 'PowerSave';
    Params      : 2;
    Param1      : (Param: 'Sleep'; Description: 'Sleep trigger setting, the default setting is "off". Each of the two parameters can be independently configured as "on" or "off".');
    Param2      : (Param: 'Wakeup'; Description: 'Wakeup trigger setting, the default setting is "on". Each of the two parameters can be independently configured as "on" or "off".');
  );
  SET_EXTERNAL_SLEEP_CONTROL_INPUT_POLARITY: STCommand = (
    Command     : 'SLXP %d';
    Description : 'Set polarity of the external sleep control input';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'Polarity'; Description: '0 - (LOW = sleep, HIGH = wake up), 1 - (LOW = wake up, HIGH = sleep), The default setting is 0.');
  );
  PRINT_EXTERNAL_SLEEP_INPUT_STATUS: STCommand = (
    Command     : 'SLXS';
    Description : 'Print external SLEEP input status';
    Group       : 'PowerSave';
    Params      : 0;
  );
  SET_MINIMUM_ACTIVE_TIME_EXTERNAL_SLEEP_TRIGGER_BEFORE_ENTERING_SLEEP: STCommand = (
    Command     : 'SLXST %d';
    Description : 'Set minimum active time for external sleep trigger before entering sleep';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'Sleep'; Description: 'Specify how long the SLEEP input must be held in the active ("sleep") state to put the device to sleep.. The default is 3000 (3 seconds).');
  );
  SET_MINIMUM_INACTIVE_TIME_EXTERNAL_SLEEP_TRIGGER_BEFORE_WAKEUP: STCommand = (
    Command     : 'SLXWT %d';
    Description : 'Set minimum inactive time for external sleep trigger before wakeup';
    Group       : 'PowerSave';
    Params      : 1;
    Param1      : (Param: 'Sleep'; Description: 'Specify how long the SLEEP input must be held in the inactive ("wake") state to wake the device from sleep. The default is 2000 (2 seconds).');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: BLUETOOTH (TABLE 21)
//------------------------------------------------------------------------------
const
  SET_BLUETOOTH_MODEM_COD: STCommand = (
    Command     : 'BTCOD %s';
    Description : 'Set Bluetooth modem CoD';
    Group       : 'Bluetooth';
    Params      : 1;
    Param1      : (Param: 'CoD'; Description: 'The input is the 6-digit hex CoD. The device needs to be power-cycled for this command to take effect. This is set in non-volatile memory, so it will survive a power-cycle.');
  );
  SET_BLUETOOTH_BROADCASTING_DEVICE_NAME: STCommand = (
    Command     : 'BTDN %s';
    Description : 'Set Bluetooth broadcasting device name';
    Group       : 'Bluetooth';
    Params      : 1;
    Param1      : (Param: 'Device name'; Description: 'Accepts printable ASCII characters (0x20 to 0x7E). Maximum length is 20 characters. Leading and trailing spaces will be ignored.');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: GENERAL PURPOSE IO (TABLE 22)
//------------------------------------------------------------------------------
const
  CONFIGURE_IO_PINS: STCommand = (
    Command     : 'GPC';
    Description : 'Configure I/O pins';
    Group       : 'General purpose I/O';
    Params      : 1;
    Param1      : (Param: 'Pin/Options'; Description: 'See Reference and Programming Manual.');
  );
  READ_INPUTS: STCommand = (
    Command     : 'GPIR';
    Description : 'Read inputs';
    Group       : 'General purpose I/O';
    Params      : 1;
    Param1      : (Param: 'Pins'; Description: 'See Reference and Programming Manual.');
  );
  READ_INPUTS_AS_HEX: STCommand = (
    Command     : 'GPIRH';
    Description : 'Read inputs, report value as hex';
    Group       : 'General purpose I/O';
    Params      : 1;
    Param1      : (Param: 'Pins'; Description: 'See Reference and Programming Manual.');
  );
  READ_OUTPUT_LATCHES: STCommand = (
    Command     : 'GPOR';
    Description : 'Read output latches';
    Group       : 'General purpose I/O';
    Params      : 1;
    Param1      : (Param: 'Pins'; Description: 'See Reference and Programming Manual.');
  );
  WRITE_OUTPUT_LATCHES: STCommand = (
    Command     : 'GPOW';
    Description : 'Write output latches';
    Group       : 'General purpose I/O';
    Params      : 1;
    Param1      : (Param: 'Pin/State'; Description: 'See Reference and Programming Manual.');
  );

//------------------------------------------------------------------------------
// ST COMMANDS: PERIODIC MESSAGES (TABLE 23)
//------------------------------------------------------------------------------
const
  ADD_PERIODIC_MESSAGE: STCommand = (
    Command     : 'PPMA %d, %s, %s';
    Description : 'Add a periodic message';
    Group       : 'Periodic messages';
    Params      : 3;
    Param1      : (Param: 'Period'; Description: 'Time in milliseconds between messages.');
    Param2      : (Param: 'Header'; Description: 'Header bytes in HEX.');
    Param3      : (Param: 'Message'; Description: 'Message bytes in HEX.');
  );
  CLEAR_ALL_PERIODIC_MESSAGES: STCommand = (
    Command     : 'PPMC';
    Description : 'Clear all periodic messages';
    Group       : 'Periodic messages';
    Params      : 0;
  );
  DELETE_PERIODIC_MESSAGE: STCommand = (
    Command     : 'PPMD %d';
    Description : 'Delete a periodic message';
    Group       : 'Periodic messages';
    Params      : 1;
    Param1      : (Param: 'Handle'; Description: 'Handle returned when adding periodic message.');
  );

function FormatSTCommand(Command: STCommand; Params: Array of const): string;

implementation

function FormatSTCommand(Command: STCommand; Params: Array of const): string;
var
  ExpectedParamCount, I, C: Integer;
  ParamType: TVarType;
  Placeholders: TArray<string>;
begin
  // Extract the placeholders
  SetLength(PlaceHolders, Length(Command.Command) div 2);
  C := 0;
  I := 1;
  while I <= Length(Command.Command) do
  begin
    if Command.Command[I] = '%' then
    begin
      if (i < Length(Command.Command)) and ((Command.Command[I + 1] = 's') or (Command.Command[I + 1] = 'd')  or (Command.Command[I + 1] = 'x')) then
      begin
        Placeholders[C] := Command.Command[I] + Command.Command[I + 1];
        Inc(C);
        Inc(I);
      end;
    end;
    Inc(I);
  end;
  SetLength(Placeholders, C);

  // Check if parameter count matches
  ExpectedParamCount := Command.Params;
  if (ExpectedParamCount <> Length(Params)) or (ExpectedParamCount <> Length(Placeholders)) then
    raise TSTCommandException.Create(Format('Mismatch: %d expected, got %d.', [ExpectedParamCount, Length(Params)]));

  // Loop over params, and make sure they match the expected types.
  for I := 0 to High(Params) do
  begin
    case Params[I].VType of
      vtInteger:
      begin
        if (PlaceHolders[I] <> '%d') and (PlaceHolders[I] <> '%x') then
          raise TSTCommandException.Create('Type mismatch: Expected number.');
      end;
      vtString, vtAnsiString, vtChar, vtWideChar, vtWideString:
      begin
        if (PlaceHolders[I] <> '%s') then
          raise TSTCommandException.Create('Type mismatch: Expected string.');
      end;

      else raise TSTCommandException.Create('Unsupported type.');
    end;
  end;

  // Return formatted AT Command
  Result := Format(Command.Command, Params);
end;

end.
