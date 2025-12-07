//------------------------------------------------------------------------------
// UNIT           : OBD.Subheader.Component.pas
// CONTENTS       : Non-visual subheader controller component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Subheader.Component;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Threading,

  OBD.Adapter.Types,
  OBD.Component.BindingHelpers,
  OBD.Connection.Types, OBD.Connection.Component,
  OBD.Protocol, OBD.Protocol.Component,
  OBD.Touch.Subheader;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Event signature used to resolve connection-driven captions for the
  ///   subheader indicators.
  /// </summary>
  TResolveSubheaderConnectionEvent = procedure(Sender: TObject; const Connected: Boolean;
    const ConnectionType: TOBDConnectionType; const ConnectionComponent: TOBDConnectionComponent;
    out VciCaption: string; out InternetCaption: string; out InternetVisible: Boolean;
    out ApplyCaptions: Boolean) of object;

  /// <summary>
  ///   Event signature used to resolve the protocol caption displayed in the
  ///   subheader when a protocol component is bound.
  /// </summary>
  TResolveSubheaderProtocolEvent = procedure(Sender: TObject; const Protocol: IOBDProtocol;
    const ProtocolClass: TOBDProtocolClass; out Caption: string; out ApplyCaption: Boolean) of object;

  /// <summary>
  ///   Non-visual controller that binds connection and protocol components to a
  ///   touch subheader, applying captions and indicator visibility on the UI
  ///   thread.
  /// </summary>
  TOBDSubheaderComponent = class(TComponent)
  private
    /// <summary>
    ///   Target touch subheader that receives indicator updates.
    /// </summary>
    FSubheader: TOBDTouchSubheader;
    /// <summary>
    ///   Connection component providing connection state changes.
    /// </summary>
    FConnectionComponent: TOBDConnectionComponent;
    /// <summary>
    ///   Protocol component providing protocol metadata and message activity.
    /// </summary>
    FProtocolComponent: TOBDProtocolComponent;
    /// <summary>
    ///   Flag indicating whether connection bindings should be installed
    ///   automatically.
    /// </summary>
    FAutoBindConnection: Boolean;
    /// <summary>
    ///   Flag indicating whether protocol bindings should be installed
    ///   automatically.
    /// </summary>
    FAutoBindProtocol: Boolean;
    /// <summary>
    ///   Flag indicating whether connection captions should be applied
    ///   automatically when states change.
    /// </summary>
    FAutoApplyConnectionCaptions: Boolean;
    /// <summary>
    ///   Flag indicating whether protocol captions should be applied
    ///   automatically when activity is detected.
    /// </summary>
    FAutoApplyProtocolCaption: Boolean;
    /// <summary>
    ///   Synchronization object guarding handler swaps.
    /// </summary>
    FBindingLock: TObject;
    /// <summary>
    ///   Previously assigned connection state handler preserved during swaps.
    /// </summary>
    FChainedOnConnectionStateChanged: TConnectionStateChangedEvent;
    /// <summary>
    ///   Previously assigned protocol message handler preserved during swaps.
    /// </summary>
    FChainedOnMessages: TReceiveDataMessagesEvent;
    /// <summary>
    ///   Optional callback raised when bindings are swapped or restored.
    /// </summary>
    FOnBindingNotification: TOBDBindingNotification;
    /// <summary>
    ///   Event handler used to resolve connection-driven captions.
    /// </summary>
    FOnResolveConnection: TResolveSubheaderConnectionEvent;
    /// <summary>
    ///   Event handler used to resolve protocol captions.
    /// </summary>
    FOnResolveProtocol: TResolveSubheaderProtocolEvent;
    /// <summary>
    ///   Update connection bindings based on the current configuration.
    /// </summary>
    procedure RefreshConnectionBinding;
    /// <summary>
    ///   Update protocol bindings based on the current configuration.
    /// </summary>
    procedure RefreshProtocolBinding;
    /// <summary>
    ///   Apply connection captions and visibility to the target subheader.
    /// </summary>
    procedure ApplyConnectionState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Apply protocol caption to the target subheader.
    /// </summary>
    procedure ApplyProtocolCaption;
    /// <summary>
    ///   Build default connection captions when no resolver is supplied.
    /// </summary>
    procedure BuildDefaultConnectionCaptions(const Connected: Boolean; const ConnectionType: TOBDConnectionType;
      out VciCaption: string; out InternetCaption: string; out InternetVisible: Boolean);
    /// <summary>
    ///   Build a default protocol caption when no resolver is supplied.
    /// </summary>
    function BuildDefaultProtocolCaption: string;
    /// <summary>
    ///   Setter for the published <c>Subheader</c> property.
    /// </summary>
    procedure SetSubheader(const Value: TOBDTouchSubheader);
    /// <summary>
    ///   Setter for the published <c>ConnectionComponent</c> property.
    /// </summary>
    procedure SetConnectionComponent(const Value: TOBDConnectionComponent);
    /// <summary>
    ///   Setter for the published <c>ProtocolComponent</c> property.
    /// </summary>
    procedure SetProtocolComponent(const Value: TOBDProtocolComponent);
    /// <summary>
    ///   Setter for the published <c>AutoBindConnection</c> property.
    /// </summary>
    procedure SetAutoBindConnection(const Value: Boolean);
    /// <summary>
    ///   Setter for the published <c>AutoBindProtocol</c> property.
    /// </summary>
    procedure SetAutoBindProtocol(const Value: Boolean);
    /// <summary>
    ///   Setter for the published <c>AutoApplyConnectionCaptions</c> property.
    /// </summary>
    procedure SetAutoApplyConnectionCaptions(const Value: Boolean);
    /// <summary>
    ///   Setter for the published <c>AutoApplyProtocolCaption</c> property.
    /// </summary>
    procedure SetAutoApplyProtocolCaption(const Value: Boolean);
    /// <summary>
    ///   Handles connection state change notifications.
    /// </summary>
    procedure HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean;
      const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Handles protocol message notifications to refresh the caption.
    /// </summary>
    procedure HandleProtocolMessages(Sender: TObject; const Messages: TArray<IOBDDataMessage>);
  protected
    /// <summary>
    ///   Allocate synchronization primitives and defaults.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Release bindings and synchronization primitives.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///   Validate component bindings after streaming.
    /// </summary>
    procedure Loaded; override;
  public
    /// <summary>
    ///   Manually request application of connection captions.
    /// </summary>
    procedure UpdateConnectionState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Manually request application of the protocol caption.
    /// </summary>
    procedure UpdateProtocolCaption;
  published
    /// <summary>
    ///   Target touch subheader that receives caption and indicator updates.
    /// </summary>
    property Subheader: TOBDTouchSubheader read FSubheader write SetSubheader;
    /// <summary>
    ///   Connection component providing connection state changes for caption
    ///   updates when auto-binding is enabled.
    /// </summary>
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    /// <summary>
    ///   Protocol component providing protocol metadata for caption updates when
    ///   auto-binding is enabled.
    /// </summary>
    property ProtocolComponent: TOBDProtocolComponent read FProtocolComponent write SetProtocolComponent;
    /// <summary>
    ///   Indicates whether the controller should automatically subscribe to
    ///   connection state events.
    /// </summary>
    property AutoBindConnection: Boolean read FAutoBindConnection write SetAutoBindConnection default True;
    /// <summary>
    ///   Indicates whether the controller should automatically subscribe to
    ///   protocol message events.
    /// </summary>
    property AutoBindProtocol: Boolean read FAutoBindProtocol write SetAutoBindProtocol default True;
    /// <summary>
    ///   Indicates whether resolved connection captions should be applied
    ///   automatically.
    /// </summary>
    property AutoApplyConnectionCaptions: Boolean read FAutoApplyConnectionCaptions
      write SetAutoApplyConnectionCaptions default True;
    /// <summary>
    ///   Indicates whether resolved protocol captions should be applied
    ///   automatically.
    /// </summary>
    property AutoApplyProtocolCaption: Boolean read FAutoApplyProtocolCaption
      write SetAutoApplyProtocolCaption default True;
    /// <summary>
    ///   Event invoked to resolve connection captions when auto-application is
    ///   enabled.
    /// </summary>
    property OnResolveConnection: TResolveSubheaderConnectionEvent read FOnResolveConnection
      write FOnResolveConnection;
    /// <summary>
    ///   Event invoked to resolve protocol captions when auto-application is
    ///   enabled.
    /// </summary>
    property OnResolveProtocol: TResolveSubheaderProtocolEvent read FOnResolveProtocol
      write FOnResolveProtocol;
    /// <summary>
    ///   Optional callback raised when bindings are swapped or restored.
    /// </summary>
    property OnBindingNotification: TOBDBindingNotification read FOnBindingNotification
      write FOnBindingNotification;
  end;

implementation

//------------------------------------------------------------------------------
// APPLY CONNECTION STATE
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.ApplyConnectionState(const Connected: Boolean;
  const ConnectionType: TOBDConnectionType);
var
  LVciCaption: string;
  LInternetCaption: string;
  LInternetVisible: Boolean;
  LApply: Boolean;
begin
  if not Assigned(FSubheader) then
    Exit;

  if not Assigned(FConnectionComponent) then
    Exit;

  LVciCaption := '';
  LInternetCaption := '';
  LInternetVisible := False;
  LApply := FAutoApplyConnectionCaptions;

  if Assigned(FOnResolveConnection) then
    FOnResolveConnection(Self, Connected, ConnectionType, FConnectionComponent,
      LVciCaption, LInternetCaption, LInternetVisible, LApply)
  else
    BuildDefaultConnectionCaptions(Connected, ConnectionType, LVciCaption, LInternetCaption, LInternetVisible);

  if not LApply then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      if not Assigned(FSubheader) then
        Exit;

      FSubheader.VciIndicator.Caption := LVciCaption;
      FSubheader.InternetConnectionIndicator.Visible := LInternetVisible;
      FSubheader.InternetConnectionIndicator.Caption := LInternetCaption;
      FSubheader.Invalidate;
    end);
end;

//------------------------------------------------------------------------------
// APPLY PROTOCOL CAPTION
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.ApplyProtocolCaption;
var
  LCaption: string;
  LApply: Boolean;
begin
  if not Assigned(FSubheader) then
    Exit;

  if not Assigned(FProtocolComponent) then
    Exit;

  LCaption := '';
  LApply := FAutoApplyProtocolCaption;

  if Assigned(FOnResolveProtocol) then
    FOnResolveProtocol(Self, FProtocolComponent.Protocol, FProtocolComponent.ProtocolClass,
      LCaption, LApply)
  else
    LCaption := BuildDefaultProtocolCaption;

  if not LApply then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FSubheader) then
      begin
        FSubheader.ProtocolIndicator.Caption := LCaption;
        FSubheader.Invalidate;
      end;
    end);
end;

//------------------------------------------------------------------------------
// BUILD DEFAULT CONNECTION CAPTIONS
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.BuildDefaultConnectionCaptions(const Connected: Boolean;
  const ConnectionType: TOBDConnectionType; out VciCaption: string; out InternetCaption: string;
  out InternetVisible: Boolean);
begin
  if not Connected then
  begin
    VciCaption := 'Disconnected';
    InternetCaption := '';
    InternetVisible := False;
    Exit;
  end;

  case ConnectionType of
    ctSerial:
      VciCaption := Format('COM %s', [FConnectionComponent.SerialPort]);
    ctBluetooth:
      VciCaption := 'Bluetooth';
    ctWiFi:
      VciCaption := 'WiFi';
    ctFTDI:
      VciCaption := 'FTDI';
  else
    VciCaption := 'Unknown';
  end;

  case ConnectionType of
    ctWiFi:
      begin
        InternetCaption := Format('%s:%d', [FConnectionComponent.IPAddress, FConnectionComponent.Port]);
        InternetVisible := True;
      end;
    ctBluetooth:
      begin
        InternetCaption := FConnectionComponent.BluetoothAddress;
        InternetVisible := True;
      end;
  else
    InternetCaption := '';
    InternetVisible := False;
  end;
end;

//------------------------------------------------------------------------------
// BUILD DEFAULT PROTOCOL CAPTION
//------------------------------------------------------------------------------
function TOBDSubheaderComponent.BuildDefaultProtocolCaption: string;
begin
  if Assigned(FProtocolComponent) and Assigned(FProtocolComponent.Protocol) then
    Result := FProtocolComponent.Protocol.DisplayName
  else if Assigned(FProtocolComponent) then
    Result := FProtocolComponent.ProtocolClass.ClassName
  else
    Result := '';
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDSubheaderComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBindingLock := TObject.Create;
  FAutoBindConnection := True;
  FAutoBindProtocol := True;
  FAutoApplyConnectionCaptions := True;
  FAutoApplyProtocolCaption := True;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDSubheaderComponent.Destroy;
begin
  RefreshConnectionBinding;
  RefreshProtocolBinding;
  FreeAndNil(FBindingLock);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// HANDLE CONNECTION STATE CHANGED
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.HandleConnectionStateChanged(Sender: TObject;
  const Connected: Boolean; const ConnectionType: TOBDConnectionType);
begin
  ApplyConnectionState(Connected, ConnectionType);
  if Assigned(FChainedOnConnectionStateChanged) then
    FChainedOnConnectionStateChanged(Sender, Connected, ConnectionType);
end;

//------------------------------------------------------------------------------
// HANDLE PROTOCOL MESSAGES
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.HandleProtocolMessages(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>);
begin
  ApplyProtocolCaption;
  if Assigned(FChainedOnMessages) then
    FChainedOnMessages(Sender, Messages);
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.Loaded;
begin
  inherited Loaded;

  if FAutoApplyConnectionCaptions then
  begin
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FSubheader, 'Subheader');
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FConnectionComponent, 'ConnectionComponent');
  end;

  if FAutoApplyProtocolCaption then
  begin
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FSubheader, 'Subheader');
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FProtocolComponent, 'ProtocolComponent');
  end;

  RefreshConnectionBinding;
  RefreshProtocolBinding;
end;

//------------------------------------------------------------------------------
// REFRESH CONNECTION BINDING
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.RefreshConnectionBinding;
begin
  if FAutoBindConnection and Assigned(FConnectionComponent) then
    TOBDBindingHelpers.SwapHandler<TConnectionStateChangedEvent>(FBindingLock,
      FConnectionComponent.OnConnectionStateChanged, FChainedOnConnectionStateChanged,
      HandleConnectionStateChanged, 'OnConnectionStateChanged', Self, FOnBindingNotification)
  else if Assigned(FConnectionComponent) then
    TOBDBindingHelpers.RestoreHandler<TConnectionStateChangedEvent>(FBindingLock,
      FConnectionComponent.OnConnectionStateChanged, FChainedOnConnectionStateChanged,
      'OnConnectionStateChanged', Self, FOnBindingNotification)
  else
    FChainedOnConnectionStateChanged := nil;
end;

//------------------------------------------------------------------------------
// REFRESH PROTOCOL BINDING
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.RefreshProtocolBinding;
begin
  if FAutoBindProtocol and Assigned(FProtocolComponent) then
    TOBDBindingHelpers.SwapHandler<TReceiveDataMessagesEvent>(FBindingLock,
      FProtocolComponent.OnMessages, FChainedOnMessages, HandleProtocolMessages,
      'OnMessages', Self, FOnBindingNotification)
  else if Assigned(FProtocolComponent) then
    TOBDBindingHelpers.RestoreHandler<TReceiveDataMessagesEvent>(FBindingLock,
      FProtocolComponent.OnMessages, FChainedOnMessages, 'OnMessages', Self,
      FOnBindingNotification)
  else
    FChainedOnMessages := nil;
end;

//------------------------------------------------------------------------------
// SET AUTO APPLY CONNECTION CAPTIONS
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetAutoApplyConnectionCaptions(const Value: Boolean);
begin
  FAutoApplyConnectionCaptions := Value;
end;

//------------------------------------------------------------------------------
// SET AUTO APPLY PROTOCOL CAPTION
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetAutoApplyProtocolCaption(const Value: Boolean);
begin
  FAutoApplyProtocolCaption := Value;
end;

//------------------------------------------------------------------------------
// SET AUTO BIND CONNECTION
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetAutoBindConnection(const Value: Boolean);
begin
  if FAutoBindConnection = Value then
    Exit;

  FAutoBindConnection := Value;
  RefreshConnectionBinding;
end;

//------------------------------------------------------------------------------
// SET AUTO BIND PROTOCOL
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetAutoBindProtocol(const Value: Boolean);
begin
  if FAutoBindProtocol = Value then
    Exit;

  FAutoBindProtocol := Value;
  RefreshProtocolBinding;
end;

//------------------------------------------------------------------------------
// SET CONNECTION COMPONENT
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetConnectionComponent(const Value: TOBDConnectionComponent);
begin
  if FConnectionComponent = Value then
    Exit;

  if Assigned(FConnectionComponent) then
    TOBDBindingHelpers.RestoreHandler<TConnectionStateChangedEvent>(FBindingLock,
      FConnectionComponent.OnConnectionStateChanged, FChainedOnConnectionStateChanged,
      'OnConnectionStateChanged', Self, FOnBindingNotification);

  FConnectionComponent := Value;
  RefreshConnectionBinding;

  if Assigned(FConnectionComponent) and FAutoApplyConnectionCaptions then
    ApplyConnectionState(FConnectionComponent.Connected, FConnectionComponent.ConnectionType);
end;

//------------------------------------------------------------------------------
// SET PROTOCOL COMPONENT
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetProtocolComponent(const Value: TOBDProtocolComponent);
begin
  if FProtocolComponent = Value then
    Exit;

  if Assigned(FProtocolComponent) then
    TOBDBindingHelpers.RestoreHandler<TReceiveDataMessagesEvent>(FBindingLock,
      FProtocolComponent.OnMessages, FChainedOnMessages, 'OnMessages', Self,
      FOnBindingNotification);

  FProtocolComponent := Value;
  RefreshProtocolBinding;

  if Assigned(FProtocolComponent) and FAutoApplyProtocolCaption then
    ApplyProtocolCaption;
end;

//------------------------------------------------------------------------------
// SET SUBHEADER
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.SetSubheader(const Value: TOBDTouchSubheader);
begin
  FSubheader := Value;
end;

//------------------------------------------------------------------------------
// UPDATE CONNECTION STATE
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.UpdateConnectionState(const Connected: Boolean;
  const ConnectionType: TOBDConnectionType);
begin
  ApplyConnectionState(Connected, ConnectionType);
end;

//------------------------------------------------------------------------------
// UPDATE PROTOCOL CAPTION
//------------------------------------------------------------------------------
procedure TOBDSubheaderComponent.UpdateProtocolCaption;
begin
  ApplyProtocolCaption;
end;

end.
