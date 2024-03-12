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
  OBD_SUPPORTED_PID_0120 = $00;
  /// <summary>
  ///   PIDs supported [21 - 40]
  /// </summary>
  OBD_SUPPORTED_PID_2140 = $20;
  /// <summary>
  ///   PIDs supported [41 - 60]
  /// </summary>
  OBD_SUPPORTED_PID_4160 = $40;
  /// <summary>
  ///   PIDs supported [61 - 80]
  /// </summary>
  OBD_SUPPORTED_PID_6180 = $60;
  /// <summary>
  ///   PIDs supported [81 - A0]
  /// </summary>
  OBD_SUPPORTED_PID_81A0 = $80;
  /// <summary>
  ///   PIDs supported [A1 - C0]
  /// </summary>
  OBD_SUPPORTED_PID_A1C0 = $A0;
  /// <summary>
  ///   PIDs supported [C1 - E0]
  /// </summary>
  OBD_SUPPORTED_PID_C1E0 = $C0;

implementation

end.
