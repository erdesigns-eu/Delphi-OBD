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
  WinApi.Windows, OBD.Protocol.Types;

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
    epUSER_CAN_1_11_125,
    epUSER_CAN_2_11_50
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
  TReceiveDataMessagesEvent = procedure(Sender: TObject; const Messages: TArray<IOBDDataMessage>) of object;

  /// <summary>
  ///   Receive Data Message event
  /// </summary>
  /// <param name="Sender">
  ///   Object calling this event
  /// </param>
  /// <param name="DataPtr">
  ///   Pointer to the received data buffer
  /// </param>
  /// <param name="DataSize">
  ///   Size of the received data in bytes
  /// </param>
  TReceiveDataEvent = procedure(Sender: TObject; DataPtr: Pointer; DataSize: Cardinal) of object;

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

//------------------------------------------------------------------------------
// ENUMERATE SERIAL (COM) PORT TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Handle to a device information set that contains requested device
  ///   information elements for a local machine.
  /// </summary>
  HDEVINFO = Pointer;

  /// <summary>
  ///   Contains device information data for a device information set.
  /// </summary>
  SP_DEVINFO_DATA = packed record
    /// <summary>
    ///   The size, in bytes, of the SP_DEVINFO_DATA structure.
    /// </summary>
    cbSize: DWORD;
    /// <summary>
    ///   The GUID of the setup class for the device
    ///   represented by this structure.
    /// </summary>
    ClassGuid: TGUID;
    /// <summary>
    ///   An opaque handle to the device instance
    ///   (also known as a handle to the devnode).
    /// </summary>
    DevInst: DWORD;
    /// <summary>
    ///   Reserved; do not use.
    /// </summary>
    Reserved: ULONG_PTR;
  end;

  /// <summary>
  ///   Pointer to SP_DEVINFO_DATA structure.
  /// </summary>
  PSpDevInfoData = ^SP_DEVINFO_DATA;

//------------------------------------------------------------------------------
// ENUMERATE FTDI TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Contains device information for FTDI devices.
  /// </summary>
  FTDIDeviceNode = packed record
    /// <summary>
    /// Flags to provide additional information about the device.
    /// This could include status information like whether the device is open or not.
    /// </summary>
    Flags: DWORD;
    /// <summary>
    /// The type of the device. This can be used to identify the specific model or capabilities of the FTDI device.
    /// </summary>
    DeviceType: DWORD;
    /// <summary>
    /// The identifier of the device. This can be used to distinguish between different devices of the same model.
    /// </summary>
    ID: DWORD;
    /// <summary>
    /// The location identifier for the device. This can help identify the physical connection location, such as a USB port number.
    /// </summary>
    LocationID: DWORD;
    /// <summary>
    /// The serial number of the device. This is typically a unique identifier for each device.
    /// </summary>
    SerialNumber: array[0..15] of AnsiChar;
    /// <summary>
    /// A description of the device. This often includes the product name and might include other details such as the version.
    /// </summary>
    Description: array[0..63] of AnsiChar;
    /// <summary>
    /// A handle to the device. This can be used for further API calls to interact with the device.
    /// Note: This field may not be immediately relevant for device enumeration but is included for completeness.
    /// </summary>
    FTHandle: DWORD;
  end;
  PFTDIDeviceNode = ^FTDIDeviceNode;

//------------------------------------------------------------------------------
// USB DEVICE ARRIVE/REMOVE TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  /// Pointer to a DEV_BROADCAST_HDR structure.
  /// </summary>
  PDevBroadcastHdr = ^DEV_BROADCAST_HDR;

  /// <summary>
  /// Structure used to receive device event notifications. This header is part
  /// of a larger structure that varies depending on the type of event being reported.
  /// </summary>
  DEV_BROADCAST_HDR = packed record
    /// <summary>
    /// Size of this structure, in bytes. This size includes the size of the
    /// header and any additional data that follows the header.
    /// </summary>
    dbch_size: DWORD;
    /// <summary>
    /// The type of device being described.
    /// </summary>
    dbch_devicetype: DWORD;
    /// <summary>
    /// Reserved; do not use.
    /// </summary>
    dbch_reserved: DWORD;
  end;

  /// <summary>
  /// Pointer to a DEV_BROADCAST_DEVICEINTERFACE structure.
  /// </summary>
  PDevBroadcastDeviceInterface = ^DEV_BROADCAST_DEVICEINTERFACE;

  /// <summary>
  /// Structure that defines a device interface class notification filter.
  /// It specifies the class of device interfaces that an application wants
  /// to receive notifications for.
  /// </summary>
  DEV_BROADCAST_DEVICEINTERFACE = record
    /// <summary>
    /// Size of this structure, in bytes. This size includes this header and
    /// the device interface name that follows the header.
    /// </summary>
    dbcc_size: DWORD;
    /// <summary>
    /// Set to DBT_DEVTYP_DEVICEINTERFACE.
    /// </summary>
    dbcc_devicetype: DWORD;
    /// <summary>
    /// Reserved; do not use.
    /// </summary>
    dbcc_reserved: DWORD;
    /// <summary>
    /// The GUID for the interface device class.
    /// </summary>
    dbcc_classguid: TGUID;
    /// <summary>
    /// The first character of the device interface name. This is a variable-length
    /// field that specifies the null-terminated name of the device interface.
    /// </summary>
    dbcc_name: short;
  end;

implementation

end.
