//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.Types.pas
// CONTENTS       : OBD Adapter Types
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 09/03/2024
//------------------------------------------------------------------------------
unit OBD.Adapter.Types;

interface

uses
  OBD.Protocol.Types;

//------------------------------------------------------------------------------
// OTHER
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Adapter Status (Not connected .. OBD connected)
  /// </summary>
  TOBDAdapterStatus = (asNotConnected, asAdapterConnected, asCarConnected, asOBDConnected);
  /// <summary>
  ///   ELM Adapter Error (Unsupported Command .. Buffer Full)
  /// </summary>
  TELMAdapterError = (aeUnsupportedCommand, aeNoData, aeDataError, aeBusInit, aeBusBusy, aeBusError, aeCanError, aeUnableToConnect, aeError, aeStopped, aeBufferFull);
  /// <summary>
  ///   OBDLink Adapter Error (ACT Alert .. UART Rx overflow)
  /// </summary>
  TOBDLinkAdapterError = (aeActAlert, aeFbError, aeFcRxTimeout, aeLpAlert, aeLvReset, aeOutOfMemory, aeRxError, aeUartRxOverflow);

//------------------------------------------------------------------------------
// PROTOCOLS SUPPORTED BY ELM (327/329)
//------------------------------------------------------------------------------
type
  TELMProtocol = (
    epAutomatic,
    epSAE_J1850_PWM,
    epSAE_J1850_VPW,
    epISO_9141_2,
    epISO_14230_4_SLOW,
    epISO_14230_4_FAST,
    epISO_15765_4_11_500,
    epISO_15765_4_29_500,
    epISO_15765_4_11_250,
    epISO_15765_4_29_250,
    epSAE_J1939_29_250,
    epSAE_J1939_29_500
  );

//------------------------------------------------------------------------------
// EVENT TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Receive Data Messages event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Messages">
  ///   OBD Data Messages array
  /// </param>
  TReceiveDataMessagesEvent = procedure(Sender: TObject; Messages: TArray<IOBDDataMessage>) of object;

  /// <summary>
  ///   Receive Data Message event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Data">
  ///   OBD Data Message
  /// </param>
  TReceiveDataEvent = procedure(Sender: TObject; Data: AnsiString) of object;

  /// <summary>
  ///   Adapter status change event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Status">
  ///   OBD Adapter Status
  /// </param>
  TAdapterStatusChangeEvent = procedure(Sender: TObject; Status: TOBDAdapterStatus) of object;

  /// <summary>
  ///   Adapter connection change event (On connect/disconnect)
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Connected">
  ///   Is the adapter connected
  /// </param>
  TAdapterConnectionChangeEvent = procedure(Sender: TObject; Connected: Boolean) of object;

  /// <summary>
  ///   ELM Adapter error event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Error">
  ///   ELM Adapter Error type
  /// </param>
  TELMAdapterErrorEvent = procedure(Sender: TObject; Error: TELMAdapterError) of object;

  /// <summary>
  ///   OBDLink Adapter error event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="Error">
  ///   OBDLink Adapter Error type
  /// </param>
  TOBDLinkAdapterErrorEvent = procedure(Sender: TObject; Error: TOBDLinkAdapterError) of object;

implementation

end.
