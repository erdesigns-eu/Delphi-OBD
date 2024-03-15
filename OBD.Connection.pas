//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.pas
// CONTENTS       : Base OBD Interface Connection Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/02/2024
//------------------------------------------------------------------------------
unit OBD.Connection;

interface

uses
  Winapi.Windows, System.Bluetooth, OBD.Connection.Types;

//------------------------------------------------------------------------------
// CONNECTION TYPES
//------------------------------------------------------------------------------
type
  TOBDConnectionType = (ctUnknown, ctSerial, ctBluetooth, ctWiFi, ctFTDI);

//------------------------------------------------------------------------------
// CONNECTION PARAMETER TYPES
//------------------------------------------------------------------------------
type
  TOBDConnectionParams = record
    case ConnectionType: TOBDConnectionType of
      ctSerial    : (COMPort: string[255]; COMBaudRate: TBaudRate);       // For Serial (COM PORT)
      ctBluetooth : (Manager: TBluetoothManager; Address: string[255]);   // For Bluetooth
      ctWiFi      : (IPAddress: string[255]; Port: Integer);              // For WiFi
      ctFTDI      : (SerialNumber: string[255]; FTDIBaudRate: TBaudRate); // For FTDI (USB)
  end;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Connection (INTERFACE)
  /// </summary>
  IOBDConnection = interface
    ['{CAA0EC29-EE78-4E31-AC9F-DD8905F5A746}']
    /// <summary>
    ///   Connect to a OBD AdapterInterface (ELM327, OBDLink, ..)
    /// </summary>
    /// <param name="Params">
    ///   TOBDConnectionParams Record with parameters for connecting
    ///   to the OBD Adapter over Serial (COM) Port, Bluetooth, WiFi and FTDI.
    /// </param>
    function Connect(const Params: TOBDConnectionParams): Boolean;
    /// <summary>
    ///   Disconnect the connected OBD Interface
    /// </summary>
    function Disconnect: Boolean;
    /// <summary>
    ///   Write AT Command
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommand(const ATCommand: string): Boolean;
    /// <summary>
    ///   Write ST Command
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommand(const STCommand: string): Boolean;
    /// <summary>
    ///   Write OBD Command
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommand(const OBDCommand: string): Boolean;
    /// <summary>
    ///   On Data Send event handler GETTER
    /// </summary>
    function GetOnDataSend: TDataSendEvent;
    /// <summary>
    ///   On Data Send event handler SETTER
    /// </summary>
    procedure SetOnDataSend(Value: TDataSendEvent);
    /// <summary>
    ///   On Data Received event handler GETTER
    /// </summary>
    function GetOnDataReceived: TDataReceivedEvent;
    /// <summary>
    ///   On Data Received event handler SETTER
    /// </summary>
    procedure SetOnDataReceived(Value: TDataReceivedEvent);
    /// <summary>
    ///   On Error event handler GETTER
    /// </summary>
    function GetOnError: TErrorEvent;
    /// <summary>
    ///   On Error event handler SETTER
    /// </summary>
    procedure SetOnError(Value: TErrorEvent);
    /// <summary>
    ///    Returns true if the interface is connected
    /// </summary>
    function Connected: Boolean;
    /// <summary>
    ///   On Data Send event
    /// </summary>
    property OnDataSend: TDataSendEvent read GetOnDataSend write SetOnDataSend;
    /// <summary>
    ///   On Data Received event
    /// </summary>
    property OnDataReceived: TDataReceivedEvent read GetOnDataReceived write SetOnDataReceived;
    /// <summary>
    ///   On Error event
    /// </summary>
    property OnError: TErrorEvent read GetOnError write SetOnError;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Connection (CLASS)
  /// </summary>
  TOBDConnection = class(TInterfacedObject, IOBDConnection)
  private
    /// <summary>
    ///   Event to emit when data is sent
    /// </summary>
    FOnDataSend: TDataSendEvent;
    /// <summary>
    ///   Event to emit when data is received
    /// </summary>
    FOnDataReceive: TDataReceivedEvent;
    /// <summary>
    ///   Event to emit when an error occured
    /// </summary>
    FOnError: TErrorEvent;
  protected
    /// <summary>
    ///   On Data Send event handler GETTER
    /// </summary>
    function GetOnDataSend: TDataSendEvent;
    /// <summary>
    ///   On Data Send event handler SETTER
    /// </summary>
    procedure SetOnDataSend(Value: TDataSendEvent);
    /// <summary>
    ///   On Data Received event handler GETTER
    /// </summary>
    function GetOnDataReceived: TDataReceivedEvent;
    /// <summary>
    ///   On Data Received event handler SETTER
    /// </summary>
    procedure SetOnDataReceived(Value: TDataReceivedEvent);
    /// <summary>
    ///   On Error event handler GETTER
    /// </summary>
    function GetOnError: TErrorEvent;
    /// <summary>
    ///   On Error event handler SETTER
    /// </summary>
    procedure SetOnError(Value: TErrorEvent);
    /// <summary>
    ///    Returns true if the interface is connected
    /// </summary>
    function Connected: Boolean; virtual; abstract;
  public
    /// <summary>
    ///   Constructor: Allocate resources needed for the OBD Interface.
    /// <summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor: Free internal allocated resources
    /// <summary>
    destructor Destroy; override;

    /// <summary>
    ///   Connect to a OBD Adapter (ELM327, OBDLink, ..)
    /// </summary>
    /// <param name="Params">
    ///   TOBDConnectionParams Record with parameters for connecting
    ///   to the OBD Interface over Serial (COM) Port, Bluetooth, WiFi and FTDI.
    /// </param>
    function Connect(const Params: TOBDConnectionParams): Boolean; virtual; abstract;
    /// <summary>
    ///   Disconnect the connected OBD Interface
    /// </summary>
    function Disconnect: Boolean; virtual; abstract;
    /// <summary>
    ///   Write AT Command
    /// </summary>
    /// <param name="ATCommand">
    ///   The AT command string (AT Z, AT SP 0, AT R, ..)
    /// </param>
    function WriteATCommand(const ATCommand: string): Boolean; virtual; abstract;
    /// <summary>
    ///   Write ST Command
    /// </summary>
    /// <param name="STCommand">
    ///   The AT command string (STDI, STI, STFMR, ..)
    /// </param>
    function WriteSTCommand(const STCommand: string): Boolean; virtual; abstract;
    /// <summary>
    ///   Write OBD Command
    /// </summary>
    /// <param name="OBDCommand">
    ///   The OBD command string (01 00, 01 1C, ..)
    /// </param>
    function WriteOBDCommand(const OBDCommand: string): Boolean; virtual; abstract;
    /// <summary>
    ///   On Data Send event
    /// </summary>
    property OnDataSend: TDataSendEvent read GetOnDataSend write SetOnDataSend;
    /// <summary>
    ///   On Data Received event
    /// </summary>
    property OnDataReceived: TDataReceivedEvent read GetOnDataReceived write SetOnDataReceived;
    /// <summary>
    ///   On Error event
    /// </summary>
    property OnError: TErrorEvent read GetOnError write SetOnError;
  end;

implementation

//------------------------------------------------------------------------------
// GET OnDataSend EVENT HANDLER
//------------------------------------------------------------------------------
function TOBDConnection.GetOnDataSend: TDataSendEvent;
begin
  Result := FOnDataSend;
end;

//------------------------------------------------------------------------------
// SET OnDataSend EVENT HANDLER
//------------------------------------------------------------------------------
procedure TOBDConnection.SetOnDataSend(Value: TDataSendEvent);
begin
  FOnDataSend := Value;
end;

//------------------------------------------------------------------------------
// GET OnDataReceive EVENT HANDLER
//------------------------------------------------------------------------------
function TOBDConnection.GetOnDataReceived: TDataReceivedEvent;
begin
  Result := FOnDataReceive;
end;

//------------------------------------------------------------------------------
// SET OnDataReceived EVENT HANDLER
//------------------------------------------------------------------------------
procedure TOBDConnection.SetOnDataReceived(Value: TDataReceivedEvent);
begin
  FOnDataReceive := Value;
end;

//------------------------------------------------------------------------------
// GET OnError EVENT HANDLER
//------------------------------------------------------------------------------
function TOBDConnection.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

//------------------------------------------------------------------------------
// SET OnError EVENT HANDLER
//------------------------------------------------------------------------------
procedure TOBDConnection.SetOnError(Value: TErrorEvent);
begin
  FOnError := Value;
end;

//------------------------------------------------------------------------------
// OBD CONNECTION CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDConnection.Create;
begin
  inherited;
end;

//------------------------------------------------------------------------------
// OBD CONNECTION DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDConnection.Destroy;
begin
  inherited;
end;

end.
