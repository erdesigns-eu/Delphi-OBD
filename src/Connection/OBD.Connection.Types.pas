//------------------------------------------------------------------------------
// UNIT           : OBD.InterfaceConnection.Types.pas
// CONTENTS       : OBD Interface Connection Class Types
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection.Types;

interface

uses WinApi.Windows;

//------------------------------------------------------------------------------
// EVENT TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   RX/receive event (packet mode enabled)
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Packet">
  ///   Pointer to the received data packet
  /// </param>
  /// <param name="DataSize">
  ///   Number of bytes received
  /// </param>
  TPacketReceivedEvent = procedure(Sender: TObject; Packet: pointer; DataSize: DWORD) of object;

  /// <summary>
  ///   TX/send event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="DataPtr">
  ///   Pointer to the send data
  /// </param>
  /// <param name="DataSize">
  ///   Number of bytes send
  /// </param>
  TDataSendEvent = procedure(Sender: TObject; DataPtr: Pointer; DataSize: DWORD) of object;

  /// <summary>
  ///   RX/received event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="DataPtr">
  ///   Pointer to the received data
  /// </param>
  /// <param name="DataSize">
  ///   Number of bytes received
  /// </param>
  TDataReceivedEvent = procedure(Sender: TObject; DataPtr: Pointer; DataSize: DWORD) of object;

  /// <summary>
  ///   Error event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this eventhandler
  /// </param>
  /// <param name="ErrorCode">
  ///   Error code (to be used for debugging and i18n)
  /// </param>
  /// <param name="ErrorMessage">
  ///   Error message text
  /// </param>
  TErrorEvent = procedure(Sender: TObject; ErrorCode: Integer; ErrorMessage: string) of object;

//------------------------------------------------------------------------------
// SERIAL AND FTDI (COM PORT)
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Baud Rates (9600...1.2m bauds)
  /// </summary>
  TBaudRate = (br9600, br14400, br19200, br38400, br56000, br57600, br115200, br230400, br460800, br500000, br921600, br1000000, br2000000, br8000000, br10000000, br12000000);
  /// <summary>
  ///   Data bits (5, 6, 7, 8)
  /// </summary>
  TDataBits = (db5BITS, db6BITS, db7BITS, db8BITS);
  /// <summary>
  ///   Stop bits (1, 1.5, 2)
  /// </summary>
  TStopBits = (sb1BITS, sb1HALFBITS, sb2BITS);
  /// <summary>
  ///   Parity (None, Odd, Even, Mark, Space)
  /// </summary>
  TParity = (ptNONE, ptODD, ptEVEN, ptMARK, ptSPACE);
  /// <summary>
  ///   Hardware Flow Control (None, None + RTS always on, RTS/CTS)
  /// </summary>
  THwFlowControl = (hfNONE, hfNONERTSON, hfRTSCTS);
  /// <summary>
  ///   Software Flow Control (None, XON/XOFF)
  /// </summary>
  TSwFlowControl = (sfNONE, sfXONXOFF);
  /// <summary>
  ///   What to do with incomplete (incoming) packets (Discard, Pass)
  /// </summary>
  TPacketMode = (pmDiscard, pmPass);
  /// <summary>
  ///   Line status (Clear To Send, Data Set Ready, Ring, Carrier Detect)
  /// </summary>
  TLineStatus = (lsClearToSend, lsDataSetReady, lsRING, lsCarrierDetect);
  /// <summary>
  ///   Set of line status
  /// </summary>
  TLineStatusSet = set of TLineStatus;

//------------------------------------------------------------------------------
// FTDI
//------------------------------------------------------------------------------
type
  FT_Result = Integer;

implementation

end.
