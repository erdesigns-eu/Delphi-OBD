//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.ATCommands.pas
// CONTENTS       : OBD Adapter AT Commands
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/03/2024
// NOTE           : Commands found in ELM327DSH.pdf
//------------------------------------------------------------------------------
unit OBD.Adapter.ATCommands;

interface

//------------------------------------------------------------------------------
// RECORDS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   AT Command Parameter
  /// </summary>
  ATCommandParam = record
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
  ///   AT Command
  /// </summary>
  ATCommand = record
    /// <summary>
    ///   ELM Version
    /// </summary>
    Version: string;
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
    Params: 0..2;
    /// <summary>
    ///   Parameter 1
    /// </summary>
    Param1: ATCommandParam;
    /// <summary>
    ///   Parameter 2
    /// </summary>
    Param2: ATCommandParam;
  end;

//------------------------------------------------------------------------------
// AT COMMANDS: GENERAL
//------------------------------------------------------------------------------
const
  DISPLAY_DEVICE_DESCRIPTOR: ATCommand = (
    Version     : '1.0';
    Command     : '@1';
    Description : 'Display the device description';
    Group       : 'General';
    Params      : 0;
  );
  DISPLAY_DEVICE_IDENTIFIER: ATCommand = (
    Version     : '1.3';
    Command     : '@2';
    Description : 'Display the device identifier';
    Group       : 'General';
    Params      : 0;
  );
  STORE_DEVICE_IDENTIFIER: ATCommand = (
    Version     : '1.3';
    Command     : '@3 %s';
    Description : 'Store the device identifier';
    Group       : 'General';
    Params      : 1;
    Param1      : (Param: 'Device identifier'; Description: 'Exactly 12 characters must be sent, and once written to memory they can not be changed (ASCII characters 00x21 - 0x5f).');
  );
  REPEAT_LAST_COMMAND: ATCommand = (
    Version     : '1.0';
    Command     : ''#13;
    Description : 'Repeat last command';
    Group       : 'General';
    Params      : 0;
  );
  TRY_BAUD_RATE_DIVISOR: ATCommand = (
    Version     : '1.2';
    Command     : 'BRD %s';
    Description : 'Try baud rate divisor';
    Group       : 'General';
    Params      : 1;
    Param1      : (Param: 'Baud rate divisor'; Description: 'See ELM327 Datasheet.');
  );
  SET_BAUD_RATE_HANDSHAKE_TIMEOUT: ATCommand = (
    Version     : '1.2';
    Command     : 'BRT %s';
    Description : 'Set baud rate timeout';
    Group       : 'General';
    Params      : 1;
    Param1      : (Param: 'Baud rate handshake timeout'; Description: 'See ELM327 Datasheet.');
  );
  SET_ALL_TO_DEFAULTS: ATCommand = (
    Version     : '1.0';
    Command     : 'D';
    Description : 'Set all to defaults';
    Group       : 'General';
    Params      : 0;
  );
  ECHO_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'E0';
    Description : 'Echo off';
    Group       : 'General';
    Params      : 0;
  );
  ECHO_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'E1';
    Description : 'Echo on';
    Group       : 'General';
    Params      : 0;
  );
  FORGET_EVENTS: ATCommand = (
    Version     : '1.3a';
    Command     : 'FE';
    Description : 'Forget events';
    Group       : 'General';
    Params      : 0;
  );
  PRINT_ID: ATCommand = (
    Version     : '1.0';
    Command     : 'I';
    Description : 'Print the ID';
    Group       : 'General';
    Params      : 0;
  );
  LINEFEEDS_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'L0';
    Description : 'Linefeeds off';
    Group       : 'General';
    Params      : 0;
  );
  LINEFEEDS_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'L1';
    Description : 'Linefeeds on';
    Group       : 'General';
    Params      : 0;
  );
  LOW_POWER_MODE: ATCommand = (
    Version     : '1.4';
    Command     : 'LP';
    Description : 'Go to low power mode';
    Group       : 'General';
    Params      : 0;
  );
  MEMORY_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'M0';
    Description : 'Memory off';
    Group       : 'General';
    Params      : 0;
  );
  MEMORY_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'M1';
    Description : 'Memory on';
    Group       : 'General';
    Params      : 0;
  );
  READ_STORED_DATA: ATCommand = (
    Version     : '1.4';
    Command     : 'RD';
    Description : 'Read the stored data';
    Group       : 'General';
    Params      : 0;
  );
  STORE_DATA_BYTE: ATCommand = (
    Version     : '1.4';
    Command     : 'SD %s';
    Description : 'Store data byte';
    Group       : 'General';
    Params      : 0;
  );
  WARM_START: ATCommand = (
    Version     : '1.0';
    Command     : 'WS';
    Description : 'Warm start';
    Group       : 'General';
    Params      : 0;
  );
  RESET_ALL: ATCommand = (
    Version     : '1.0';
    Command     : 'Z';
    Description : 'Reset all';
    Group       : 'General';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// AT COMMANDS: OBD
//------------------------------------------------------------------------------
const
  ALLOW_LONG_MESSAGES: ATCommand = (
    Version     : '1.0';
    Command     : 'AL';
    Description : 'Allow long messages (> 7 bytes)';
    Group       : 'OBD';
    Params      : 0;
  );
  AUTOMATIC_RECEIVE: ATCommand = (
    Version     : '1.2';
    Command     : 'Automatic receive';
    Description : 'AR';
    Group       : 'OBD';
    Params      : 0;
  );
  ADAPTIVE_TIMING_OFF: ATCommand = (
    Version     : '1.2';
    Command     : 'AT0';
    Description : 'Adaptive timing off';
    Group       : 'OBD';
    Params      : 0;
  );
  ADAPTIVE_TIMING_AUTO_1: ATCommand = (
    Version     : '1.2';
    Command     : 'AT1';
    Description : 'Adaptive timing Auto 1';
    Group       : 'OBD';
    Params      : 0;
  );
  ADAPTIVE_TIMING_AUTO_2: ATCommand = (
    Version     : '1.2';
    Command     : 'AT2';
    Description : 'Adaptive timing Auto 2';
    Group       : 'OBD';
    Params      : 0;
  );
  PERFORM_BUFFER_DUMP: ATCommand = (
    Version     : '1.0';
    Command     : 'BD';
    Description : 'Perform buffer dump';
    Group       : 'OBD';
    Params      : 0;
  );
  BYPASS_INITIALIZATION_SEQUENCE: ATCommand = (
    Version     : '1.0';
    Command     : 'BI';
    Description : 'Bypass the initialization sequence';
    Group       : 'OBD';
    Params      : 0;
  );
  DESCRIBE_CURRENT_PROTOCOL: ATCommand = (
    Version     : '1.0';
    Command     : 'DP';
    Description : 'Describe the current protocol';
    Group       : 'OBD';
    Params      : 0;
  );
  DESCRIBE_PROTOCOL_NUMBER: ATCommand = (
    Version     : '1.0';
    Command     : 'DPN';
    Description : 'Describe the protocol by number';
    Group       : 'OBD';
    Params      : 0;
  );
  HEADERS_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'H0';
    Description : 'Headers off';
    Group       : 'OBD';
    Params      : 0;
  );
  HEADERS_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'H1';
    Description : 'Headers on';
    Group       : 'OBD';
    Params      : 0;
  );
  MONITOR_ALL: ATCommand = (
    Version     : '1.0';
    Command     : 'MA';
    Description : 'Monitor all';
    Group       : 'OBD';
    Params      : 0;
  );
  MONITOR_RECEIVER: ATCommand = (
    Version     : '1.0';
    Command     : 'MR %s';
    Description : 'Monitor for receiver hh';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'See ELM327 Datasheet.');
  );
  MONITOR_TRANSMITTER: ATCommand = (
    Version     : '1.0';
    Command     : 'MT %s';
    Description : 'Monitor for transmitter hh';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'See ELM327 Datasheet.');
  );
  NORMAL_MESSAGES_LENGTH: ATCommand = (
    Version     : '1.0';
    Command     : 'NL';
    Description : 'Normal length messages (7 bytes)';
    Group       : 'OBD';
    Params      : 0;
  );
  PROTOCOL_CLOSE: ATCommand = (
    Version     : '1.0';
    Command     : 'PC';
    Description : 'Protocol close';
    Group       : 'OBD';
    Params      : 0;
  );
  RESPONSES_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'R0';
    Description : 'Responses off';
    Group       : 'OBD';
    Params      : 0;
  );
  RESPONES_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'R1';
    Description : 'Responses on';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_SPACES_OFF: ATCommand = (
    Version     : '1.3';
    Command     : 'S0';
    Description : 'Printing spaces off';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_SPACES_ON: ATCommand = (
    Version     : '1.3';
    Command     : 'S1';
    Description : 'Printing spaces on';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_HEADER: ATCommand = (
    Version     : '1.0';
    Command     : 'SH %s';
    Description : 'Set header';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Header'; Description: 'Header address in HEX.');
  );
  SET_PROTOCOL_AUTO_H: ATCommand = (
    Version     : '1.0';
    Command     : 'SP A%d';
    Description : 'Set protocol to Auto, H and save it';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Protocol'; Description: 'Protocol number (See ELM327 Datasheet).');
  );
  SET_PROTOCOL: ATCommand = (
    Version     : '1.0';
    Command     : 'SP %d';
    Description : 'Set protocol and save it';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Protocol'; Description: 'Protocol number (See ELM327 Datasheet).');
  );
  SET_PROTOCOL_AUTO: ATCommand = (
    Version     : '1.3';
    Command     : 'SP 00';
    Description : 'Set protocol to Auto, and save it';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_RECEIVE_ADDRESS: ATCommand = (
    Version     : '1.2';
    Command     : 'SR %s';
    Description : 'Set the receive address to hh';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'Receive address in HEX (See ELM327 Datasheet).');
  );
  SET_STANDARD_SEARCH_ORDER: ATCommand = (
    Version     : '1.4';
    Command     : 'SS';
    Description : 'Set standard search order (J1978)';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_TIMEOUT: ATCommand = (
    Version     : '1.0';
    Command     : 'ST %s';
    Description : 'Set timeout to hh x 4 miliseconds (or 20 miliseconds with J1939 protocol and JTM5 selected)';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Timeout'; Description: 'Timeout in miliseconds x 4 (See ELM327 Datasheet).');
  );
  SET_DEFAULT_TIMEOUT: ATCommand = (
    Version     : '1.0';
    Command     : 'ST 00';
    Description : 'Set timeout to default';
    Group       : 'OBD';
    Params      : 0;
  );
  SET_TESTER_ADDRESS: ATCommand = (
    Version     : '1.4';
    Command     : 'TA %s';
    Description : 'Set tester address to hh';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'Tester address in HEX (See ELM327 Datasheet).');
  );
  TRY_PROTOCOL_AUTO: ATCommand = (
    Version     : '1.0';
    Command     : 'TP A%d';
    Description : 'Try protocol with Auto search';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Protocol'; Description: 'Protocol number (See ELM327 Datasheet).');
  );
  TRY_PROTOCOL: ATCommand = (
    Version     : '1.0';
    Command     : 'TP %d';
    Description : 'Try protocol';
    Group       : 'OBD';
    Params      : 1;
    Param1      : (Param: 'Protocol'; Description: 'Protocol number (See ELM327 Datasheet).');
  );

//------------------------------------------------------------------------------
// AT COMMANDS: CAN
//------------------------------------------------------------------------------
const
  SET_CAN_AUTOMATIC_FORMATTING_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'CAF0';
    Description : 'CAN automatic formatting off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_AUTOMATIC_FORMATTING_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'CAF1';
    Description : 'CAN automatic formatting on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_EXTENDED_ADRESSING_OFF: ATCommand = (
    Version     : '1.4';
    Command     : 'CEA';
    Description : 'CAN extended addressing off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_EXTENDED_ADRESSING_ON: ATCommand = (
    Version     : '1.4';
    Command     : 'CEA %s';
    Description : 'CAN extended addressing on';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'Extended address in HEX (See ELM327 Datasheet).');
  );
  SET_CAN_ID_FILTER: ATCommand = (
    Version     : '1.0';
    Command     : 'CF %s';
    Description : 'Set the CAN ID filter to address hh';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'CAN ID filter address in HEX (See ELM327 Datasheet).');
  );
  SET_CAN_FLOW_CONTROL_OFF: ATCommand = (
    Version     : '1.0';
    Command     : 'CFC0';
    Description : 'CAN flow control off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_FLOW_CONTROL_ON: ATCommand = (
    Version     : '1.0';
    Command     : 'CFC1';
    Description : 'CAN flow control on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_ID_MASK: ATCommand = (
    Version     : '1.0';
    Command     : 'CM %s';
    Description : 'Set the CAN ID mask to hh';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'CAN ID mask in HEX (See ELM327 Datasheet).');
  );
  SET_CAN_PRIORITY: ATCommand = (
    Version     : '1.0';
    Command     : 'CP %s';
    Description : 'Set CAN priority (Only for 29 bits)';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Priority'; Description: 'CAN priority bits in HEX (See ELM327 Datasheet).');
  );
  RESET_CAN_RECEIVE_ADDRESS_FILTERS: ATCommand = (
    Version     : '1.4b';
    Command     : 'CRA';
    Description : 'Reset CAN receive address filters';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_RECEIVE_ADDRESS: ATCommand = (
    Version     : '1.3';
    Command     : 'CRA %s';
    Description : 'Set CAN receive address to hh';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'CAN receive address in HEX (See ELM327 Datasheet).');
  );
  CAN_STATUS: ATCommand = (
    Version     : '1.0';
    Command     : 'CS';
    Description : 'Print CAN status counts';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_SILENT_MODE_OFF: ATCommand = (
    Version     : '1.4';
    Command     : 'CSM0';
    Description : 'CAN silent mode off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_SILENT_MODE_ON: ATCommand = (
    Version     : '1.4';
    Command     : 'CSM1';
    Description : 'CAN silent mode on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_DLC_DISPLAY_OFF: ATCommand = (
    Version     : '1.3';
    Command     : 'D0';
    Description : 'CAN DLC display off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_CAN_DLC_DISPLAY_ON: ATCommand = (
    Version     : '1.3';
    Command     : 'D1';
    Description : 'CAN DLC display on';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_FLOW_CONTROL_DATA: ATCommand = (
    Version     : '1.1';
    Command     : 'FC SD %s';
    Description : 'Set CAN flow control data';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Data'; Description: 'CAN flow control data in HEX (See ELM327 Datasheet).');
  );
  SET_FLOW_CONTROL_HEADER: ATCommand = (
    Version     : '1.1';
    Command     : 'FC SH %s';
    Description : 'Set CAN flow control header';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Header'; Description: 'CAN flow control header in HEX (See ELM327 Datasheet).');
  );
  SET_FLOW_CONTROL_MODE: ATCommand = (
    Version     : '1.1';
    Command     : 'FC SM %d';
    Description : 'Set CAN flow control mode';
    Group       : 'CAN';
    Params      : 1;
    Param1      : (Param: 'Mode'; Description: 'CAN flow control mode: 0, 1, 2 (See ELM327 Datasheet).');
  );
  SET_PROTOCOL_B_OPTIONS_AND_BAUD_RATE: ATCommand = (
    Version     : '1.4';
    Command     : 'PB %s %s';
    Description : 'Set protocol B options and baud rate';
    Group       : 'CAN';
    Params      : 2;
    Param1      : (Param: 'PP 2C'; Description: 'See ELM327 Datasheet.');
    Param2      : (Param: 'PP 2D'; Description: 'See ELM327 Datasheet.');
  );
  RTR_MESSAGE: ATCommand = (
    Version     : '1.3';
    Command     : 'RTR';
    Description : 'RTR message (Remote Transmission Request)';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_VARIABLE_DLC_OFF: ATCommand = (
    Version     : '1.3';
    Command     : 'V0';
    Description : 'Set CAN variable DLC off';
    Group       : 'CAN';
    Params      : 0;
  );
  SET_VARIABLE_DLC_ON: ATCommand = (
    Version     : '1.3';
    Command     : 'V1';
    Description : 'Set CAN variable DLC on';
    Group       : 'CAN';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// AT COMMANDS: VOLTS
//------------------------------------------------------------------------------
const
  CALIBRATE_VOLTAGE: ATCommand = (
    Version     : '1.0';
    Command     : 'CV %s';
    Description : 'Calibrate the voltage';
    Group       : 'Volts';
    Params      : 1;
    Param1      : (Param: 'Voltage'; Description: 'Voltage must always be provided as 4 digits without decimal point. (See ELM327 Datasheet).');
  );
  RESTORE_VOLTAGE_CALIBRATION: ATCommand = (
    Version     : '1.0';
    Command     : 'CV 0000';
    Description : 'Reset the voltage calibration to factory defaults';
    Group       : 'Volts';
    Params      : 0;
  );
  READ_VOLTAGE: ATCommand = (
    Version     : '1.0';
    Command     : 'RV';
    Description : 'Read voltage';
    Group       : 'Volts';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// AT COMMANDS: J1939
//------------------------------------------------------------------------------
const
  MONITOR_DM1_MESSAGES: ATCommand = (
    Version     : '1.2';
    Command     : 'DM1';
    Description : 'Monitor for DM1 messages';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_ELM_DATA_FORMAT: ATCommand = (
    Version     : '1.3';
    Command     : 'JE';
    Description : 'Use J1939 ELM data format';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_HEADER_FORMATTING_OFF: ATCommand = (
    Version     : '1.4b';
    Command     : 'JHF0';
    Description : 'Set J1939 header formatting off';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_HEADER_FORMATTING_ON: ATCommand = (
    Version     : '1.4b';
    Command     : 'JHF1';
    Description : 'Set J1939 header formatting on';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_1939_SEA_DATA_FORMAT: ATCommand = (
    Version     : '1.3';
    Command     : 'JS';
    Description : 'Use J1939 SAE data format';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_TIMER_MULTIPLIER_1: ATCommand = (
    Version     : '1.4b';
    Command     : 'JTM1';
    Description : 'Set the J1939 timer multiplier to 1x';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_TIMER_MULTIPLIER_5: ATCommand = (
    Version     : '1.4b';
    Command     : 'JTM5';
    Description : 'Set the J1939 timer multiplier to 5x';
    Group       : 'J1939';
    Params      : 0;
  );
  SET_J1939_PGN_MONITOR: ATCommand = (
    Version     : '1.2';
    Command     : 'MP %s';
    Description : 'Monitor for PGN hhhh';
    Group       : 'J1939';
    Params      : 1;
    Param1      : (Param: 'PGN'; Description: 'See ELM327 Datasheet.');
  );
  SET_J1939_PGN_MONITOR_MESSAGES: ATCommand = (
    Version     : '1.4b';
    Command     : 'MP %s %d';
    Description : 'Monitor for PGN hhhh, get n messages';
    Group       : 'J1939';
    Params      : 2;
    Param1      : (Param: 'PGN'; Description: 'See ELM327 Datasheet.');
    Param2      : (Param: 'Number of messages'; Description: 'See ELM327 Datasheet.');
  );

//------------------------------------------------------------------------------
// AT COMMANDS: ISO
//------------------------------------------------------------------------------
const
  FAST_INITIALIZATION: ATCommand = (
    Version     : '1.4b';
    Command     : 'FI';
    Description : 'Perform a fast ISO initialization';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_BAUD_RATE_10400: ATCommand = (
    Version     : '1.0';
    Command     : 'IB 10';
    Description : 'Set the ISO baud rate to 10400';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_BAUD_RATE_4800: ATCommand = (
    Version     : '1.4';
    Command     : 'IB 48';
    Description : 'Set the ISO baud rate to 4800';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_BAUD_RATE_9600: ATCommand = (
    Version     : '1.0';
    Command     : 'IB 96';
    Description : 'Set the ISO baud rate to 9600';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_INIT_ADDRESS: ATCommand = (
    Version     : '1.2';
    Command     : 'IIA %s';
    Description : 'Set the ISO (slow init) address';
    Group       : 'ISO';
    Params      : 1;
    Param1      : (Param: 'Address'; Description: 'ECU address in HEX (See ELM327 Datasheet).');
  );
  DISPLAY_ISO_KEYWORDS: ATCommand = (
    Version     : '1.3';
    Command     : 'KW';
    Description : 'Display the ISO keywords';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_KEYWORD_CHECKING_OFF: ATCommand = (
    Version     : '1.2';
    Command     : 'KW0';
    Description : 'Set the ISO keyword checking off';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_KEYWORD_CHECKING_ON: ATCommand = (
    Version     : '1.2';
    Command     : 'KW1';
    Description : 'Set the ISO keyword checking on';
    Group       : 'ISO';
    Params      : 0;
  );
  SLOW_INITIALIZATION: ATCommand = (
    Version     : '1.4';
    Command     : 'SI';
    Description : 'Perform a slow ISO initialization';
    Group       : 'ISO';
    Params      : 0;
  );
  SET_ISO_WAKEUP_INTERVAL: ATCommand = (
    Version     : '1.0';
    Command     : 'SW %s';
    Description : 'Set wakeup interval to hh x 20 miliseconds';
    Group       : 'ISO';
    Params      : 1;
    Param1      : (Param: 'Interval'; Description: 'Interval duration in HEX, 00 - FF (See ELM327 Datasheet).');
  );
  SET_ISO_WAKEUP_MESSAGE: ATCommand = (
    Version     : '1.2';
    Command     : 'WM %s';
    Description : 'Set ISO wakeup message';
    Group       : 'ISO';
    Params      : 1;
    Param1      : (Param: 'Message'; Description: 'Wakeup message - typically 3 header bytes and 1-3 data bytes. (See ELM327 Datasheet).');
  );

//------------------------------------------------------------------------------
// AT COMMANDS: J1850
//------------------------------------------------------------------------------
const
  SET_IFR_VALUE_FROM_HEADER: ATCommand = (
    Version     : '1.2';
    Command     : 'IFR H';
    Description : 'Set IFR value from header';
    Group       : 'J1850';
    Params      : 0;
  );
  SET_IFR_VALUE_FROM_SOURCE: ATCommand = (
    Version     : '1.2';
    Command     : 'IFR S';
    Description : 'Set IFR value from source';
    Group       : 'J1850';
    Params      : 0;
  );
  SET_IFR_OFF: ATCommand = (
    Version     : '1.2';
    Command     : 'IFR0';
    Description : 'Set IFRs off';
    Group       : 'J1850';
    Params      : 0;
  );
  SET_IFR_AUTO: ATCommand = (
    Version     : '1.2';
    Command     : 'IFR1';
    Description : 'Set IFRs Auto';
    Group       : 'J1850';
    Params      : 0;
  );
  SET_IFR_ON: ATCommand = (
    Version     : '1.2';
    Command     : 'IFR2';
    Description : 'Set IFRs on';
    Group       : 'J1850';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// AT COMMANDS: OTHER
//------------------------------------------------------------------------------
const
  IGNITION_MONITOR: ATCommand = (
    Version     : '1.4';
    Command     : 'IGN';
    Description : 'Get the ignition monitor status';
    Group       : 'Other';
    Params      : 0;
  );

//------------------------------------------------------------------------------
// AT COMMANDS: PROGRAMMABLE PARAMETERS
//------------------------------------------------------------------------------
const
  SET_PROGRAMMABLE_PARAMETERS_OFF: ATCommand = (
    Version     : '1.1';
    Command     : 'PP FF OFF';
    Description : 'Set programmable parameters off';
    Group       : 'Programmable Parameters';
    Params      : 0;
  );
  SET_PROGRAMMABLE_PARAMETERS_ON: ATCommand = (
    Version     : '1.1';
    Command     : 'PP FF ON';
    Description : 'Set programmable parameters on';
    Group       : 'Programmable Parameters';
    Params      : 0;
  );
  DISABLE_PROGRAMMABLE_PARAMETER: ATCommand = (
    Version     : '1.1';
    Command     : 'PP %s OFF';
    Description : 'Disable programmable parameter';
    Group       : 'Programmable Parameters';
    Params      : 1;
    Param1      : (Param: 'Parameter'; Description: 'Parameter in HEX (See ELM327 Datasheet).');
  );
  ENABLE_PROGRAMMABLE_PARAMETER: ATCommand = (
    Version     : '1.1';
    Command     : 'PP %s ON';
    Description : 'Enable programmable parameter';
    Group       : 'Programmable Parameters';
    Params      : 1;
    Param1      : (Param: 'Parameter'; Description: 'Parameter in HEX (See ELM327 Datasheet).');
  );
  SET_PROGRAMMABLE_PARAMETER_VALUE: ATCommand = (
    Version     : '1.1';
    Command     : 'PP %s SV %s';
    Description : 'Set programmable parameter value';
    Group       : 'Programmable Parameters';
    Params      : 2;
    Param1      : (Param: 'Parameter'; Description: 'Parameter in HEX (See ELM327 Datasheet).');
    Param2      : (Param: 'Value'; Description: 'Value in HEX (See ELM327 Datasheet).');
  );
  PRINT_PROGRAMMABLE_PARAMETER_SUMMARY: ATCommand = (
    Version     : '1.1';
    Command     : 'PPS';
    Description : 'print programmable parameters summary';
    Group       : 'Programmable Parameters';
    Params      : 0;
  );

implementation

end.
