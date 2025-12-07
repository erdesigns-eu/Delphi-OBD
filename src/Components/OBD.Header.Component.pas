//------------------------------------------------------------------------------
// UNIT           : OBD.Header.Component.pas
// CONTENTS       : Non-visual header controller component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : OpenAI Assistant
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/02/2025
//------------------------------------------------------------------------------
unit OBD.Header.Component;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Threading,
  OBD.Connection, OBD.Component.BindingHelpers,
  OBD.Connection.Types, OBD.Connection.Component, OBD.Touch.Header;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Event signature used to resolve header captions and battery values from
  ///   external connection state changes.
  /// </summary>
  TResolveHeaderStateEvent = procedure(Sender: TObject; const Connected: Boolean;
    const ConnectionType: TOBDConnectionType; out Caption: string;
    out BatteryPercentage: Single; out ApplyCaption: Boolean;
    out ApplyBattery: Boolean) of object;

  /// <summary>
  ///   Non-visual controller that binds a connection component to a touch header
  ///   and applies caption and indicator updates on the UI thread.
  /// </summary>
  TOBDHeaderComponent = class(TComponent)
  private
    /// <summary>
    ///   Header control that receives caption and indicator updates.
    /// </summary>
    FHeader: TOBDTouchHeader;
    /// <summary>
    ///   Connection component used to source connection-state changes.
    /// </summary>
    FConnectionComponent: TOBDConnectionComponent;
    /// <summary>
    ///   Indicates whether the controller should auto-bind to the connection
    ///   component's state change events.
    /// </summary>
    FAutoBindConnection: Boolean;
    /// <summary>
    ///   Indicates whether resolved captions should be automatically applied to
    ///   the target header.
    /// </summary>
    FAutoApplyCaption: Boolean;
    /// <summary>
    ///   Indicates whether resolved battery percentages should be automatically
    ///   applied to the target header.
    /// </summary>
    FAutoApplyBattery: Boolean;
    /// <summary>
    ///   Synchronization object guarding event binding swaps.
    /// </summary>
    FBindingLock: TObject;
    /// <summary>
    ///   Previously assigned connection state handler retained when swapping
    ///   bindings, allowing chaining on restore.
    /// </summary>
    FChainedOnConnectionStateChanged: TConnectionStateChangedEvent;
    /// <summary>
    ///   Optional callback raised when connection bindings are swapped or
    ///   restored for observability.
    /// </summary>
    FOnBindingNotification: TOBDBindingNotification;
    /// <summary>
    ///   Event handler used to translate connection state into header updates.
    /// </summary>
    FOnResolveState: TResolveHeaderStateEvent;
    /// <summary>
    ///   Safely swap connection component event bindings based on the auto-bind
    ///   preference.
    /// </summary>
    procedure RefreshConnectionBinding;
    /// <summary>
    ///   Update the header caption and battery indicator based on connection
    ///   state, resolving custom values when provided.
    /// </summary>
    procedure ApplyConnectionState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
    /// <summary>
    ///   Build a default caption from the connection state when no resolver is
    ///   supplied.
    /// </summary>
    function BuildDefaultCaption(const Connected: Boolean; const ConnectionType: TOBDConnectionType): string;
    /// <summary>
    ///   Apply the supplied caption on the UI thread.
    /// </summary>
    procedure ApplyCaption(const Caption: string);
    /// <summary>
    ///   Apply the supplied battery percentage on the UI thread.
    /// </summary>
    procedure ApplyBattery(const Percentage: Single);
    /// <summary>
    ///   Setter for the published <c>Header</c> property.
    /// </summary>
    procedure SetHeader(const Value: TOBDTouchHeader);
    /// <summary>
    ///   Setter for the published <c>ConnectionComponent</c> property that
    ///   refreshes event bindings.
    /// </summary>
    procedure SetConnectionComponent(const Value: TOBDConnectionComponent);
    /// <summary>
    ///   Setter for the published <c>AutoBindConnection</c> property that
    ///   refreshes event bindings.
    /// </summary>
    procedure SetAutoBindConnection(const Value: Boolean);
    /// <summary>
    ///   Setter for the published <c>AutoApplyCaption</c> property.
    /// </summary>
    procedure SetAutoApplyCaption(const Value: Boolean);
    /// <summary>
    ///   Setter for the published <c>AutoApplyBattery</c> property.
    /// </summary>
    procedure SetAutoApplyBattery(const Value: Boolean);
    /// <summary>
    ///   Handles connection state change notifications from the connection
    ///   component.
    /// </summary>
    procedure HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean;
      const ConnectionType: TOBDConnectionType);
  protected
    /// <summary>
    ///   Allocate synchronization primitives and initialize defaults.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Release bindings and synchronization primitives.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///   Validate component bindings after streaming completes.
    /// </summary>
    procedure Loaded; override;
  public
    /// <summary>
    ///   Manually request that the controller apply the given connection state
    ///   to the target header.
    /// </summary>
    procedure UpdateHeaderState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
  published
    /// <summary>
    ///   Header control that receives caption and indicator updates from this
    ///   controller.
    /// </summary>
    property Header: TOBDTouchHeader read FHeader write SetHeader;
    /// <summary>
    ///   Connection component that supplies state change events when
    ///   auto-binding is enabled.
    /// </summary>
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    /// <summary>
    ///   Indicates whether the controller should automatically subscribe to the
    ///   connection component's state change events.
    /// </summary>
    property AutoBindConnection: Boolean read FAutoBindConnection write SetAutoBindConnection default True;
    /// <summary>
    ///   Indicates whether resolved captions should automatically update the
    ///   target header.
    /// </summary>
    property AutoApplyCaption: Boolean read FAutoApplyCaption write SetAutoApplyCaption default True;
    /// <summary>
    ///   Indicates whether resolved battery percentages should automatically
    ///   update the target header when supplied by the resolver event.
    /// </summary>
    property AutoApplyBattery: Boolean read FAutoApplyBattery write SetAutoApplyBattery default False;
    /// <summary>
    ///   Event invoked to translate connection state into caption and indicator
    ///   values when auto-application is enabled.
    /// </summary>
    property OnResolveState: TResolveHeaderStateEvent read FOnResolveState write FOnResolveState;
    /// <summary>
    ///   Optional callback raised when the controller swaps or restores
    ///   connection bindings.
    /// </summary>
    property OnBindingNotification: TOBDBindingNotification read FOnBindingNotification
      write FOnBindingNotification;
  end;

implementation

//------------------------------------------------------------------------------
// APPLY BATTERY
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.ApplyBattery(const Percentage: Single);
begin
  if not Assigned(FHeader) then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FHeader) then
      begin
        FHeader.BatteryIndicator.Percentage := Percentage;
        FHeader.Repaint;
      end;
    end);
end;

//------------------------------------------------------------------------------
// APPLY CAPTION
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.ApplyCaption(const Caption: string);
begin
  if not Assigned(FHeader) then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FHeader) then
      begin
        FHeader.Caption.Caption := Caption;
        FHeader.Repaint;
      end;
    end);
end;

//------------------------------------------------------------------------------
// APPLY CONNECTION STATE
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.ApplyConnectionState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
var
  LCaption: string;
  LBattery: Single;
  LApplyCaption: Boolean;
  LApplyBattery: Boolean;
begin
  LApplyCaption := False;
  LApplyBattery := False;
  LCaption := '';
  LBattery := 0;

  if Assigned(FOnResolveState) then
    FOnResolveState(Self, Connected, ConnectionType, LCaption, LBattery, LApplyCaption, LApplyBattery);

  if FAutoApplyCaption then
  begin
    if not LApplyCaption then
      LCaption := BuildDefaultCaption(Connected, ConnectionType);
    ApplyCaption(LCaption);
  end;

  if FAutoApplyBattery and LApplyBattery then
    ApplyBattery(LBattery);
end;

//------------------------------------------------------------------------------
// BUILD DEFAULT CAPTION
//------------------------------------------------------------------------------
function TOBDHeaderComponent.BuildDefaultCaption(const Connected: Boolean; const ConnectionType: TOBDConnectionType): string;
begin
  if Connected then
    case ConnectionType of
      ctSerial: Result := 'Serial Connected';
      ctBluetooth: Result := 'Bluetooth Connected';
      ctWiFi: Result := 'WiFi Connected';
      ctFTDI: Result := 'FTDI Connected';
    else
      Result := 'Connected';
    end
  else
    Result := 'Disconnected';
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDHeaderComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBindingLock := TObject.Create;
  FAutoBindConnection := True;
  FAutoApplyCaption := True;
  FAutoApplyBattery := False;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDHeaderComponent.Destroy;
var
  CurrentHandler: TConnectionStateChangedEvent;
begin
  if Assigned(FConnectionComponent) then
  begin
    CurrentHandler := FConnectionComponent.OnConnectionStateChanged;
    TOBDBindingHelpers.RestoreHandler<TConnectionStateChangedEvent>(FBindingLock,
      CurrentHandler, FChainedOnConnectionStateChanged,
      'Connection.OnConnectionStateChanged', Self, FOnBindingNotification);
    FConnectionComponent.OnConnectionStateChanged := CurrentHandler;
  end;
  FBindingLock.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.Loaded;
begin
  inherited Loaded;

  if FAutoApplyCaption or FAutoApplyBattery then
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FHeader, 'Header');

  if FAutoBindConnection then
    TOBDBindingHelpers.ValidateRequiredComponent(Self, FConnectionComponent, 'ConnectionComponent');

  RefreshConnectionBinding;
end;

//------------------------------------------------------------------------------
// HANDLE CONNECTION STATE CHANGED
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean;
  const ConnectionType: TOBDConnectionType);
begin
  if not FAutoBindConnection then
    Exit;

  ApplyConnectionState(Connected, ConnectionType);
end;

//------------------------------------------------------------------------------
// REFRESH CONNECTION BINDING
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.RefreshConnectionBinding;
var
  CurrentHandler: TConnectionStateChangedEvent;
begin
  if not Assigned(FConnectionComponent) then
  begin
    FChainedOnConnectionStateChanged := nil;
    Exit;
  end;

  CurrentHandler := FConnectionComponent.OnConnectionStateChanged;
  if FAutoBindConnection then
    TOBDBindingHelpers.SwapHandler<TConnectionStateChangedEvent>(FBindingLock,
      CurrentHandler, FChainedOnConnectionStateChanged,
      HandleConnectionStateChanged, 'Connection.OnConnectionStateChanged', Self,
      FOnBindingNotification)
  else
    TOBDBindingHelpers.RestoreHandler<TConnectionStateChangedEvent>(FBindingLock,
      CurrentHandler, FChainedOnConnectionStateChanged,
      'Connection.OnConnectionStateChanged', Self, FOnBindingNotification);
  FConnectionComponent.OnConnectionStateChanged := CurrentHandler;
end;

//------------------------------------------------------------------------------
// SET AUTO APPLY BATTERY
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.SetAutoApplyBattery(const Value: Boolean);
begin
  if FAutoApplyBattery = Value then
    Exit;
  FAutoApplyBattery := Value;
end;

//------------------------------------------------------------------------------
// SET AUTO APPLY CAPTION
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.SetAutoApplyCaption(const Value: Boolean);
begin
  if FAutoApplyCaption = Value then
    Exit;
  FAutoApplyCaption := Value;
end;

//------------------------------------------------------------------------------
// SET AUTO BIND CONNECTION
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.SetAutoBindConnection(const Value: Boolean);
begin
  if FAutoBindConnection = Value then
    Exit;
  FAutoBindConnection := Value;
  RefreshConnectionBinding;
end;

//------------------------------------------------------------------------------
// SET CONNECTION COMPONENT
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.SetConnectionComponent(const Value: TOBDConnectionComponent);
var
  CurrentHandler: TConnectionStateChangedEvent;
begin
  if FConnectionComponent = Value then
    Exit;

  if Assigned(FConnectionComponent) then
  begin
    CurrentHandler := FConnectionComponent.OnConnectionStateChanged;
    TOBDBindingHelpers.RestoreHandler<TConnectionStateChangedEvent>(FBindingLock,
      CurrentHandler, FChainedOnConnectionStateChanged,
      'Connection.OnConnectionStateChanged', Self, FOnBindingNotification);
    FConnectionComponent.OnConnectionStateChanged := CurrentHandler;
  end;

  FConnectionComponent := Value;
  RefreshConnectionBinding;

  if Assigned(FConnectionComponent) and FAutoBindConnection then
    UpdateHeaderState(FConnectionComponent.Connected, FConnectionComponent.ConnectionType);
end;

//------------------------------------------------------------------------------
// SET HEADER
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.SetHeader(const Value: TOBDTouchHeader);
begin
  FHeader := Value;
end;

//------------------------------------------------------------------------------
// UPDATE HEADER STATE
//------------------------------------------------------------------------------
procedure TOBDHeaderComponent.UpdateHeaderState(const Connected: Boolean; const ConnectionType: TOBDConnectionType);
begin
  ApplyConnectionState(Connected, ConnectionType);
end;

end.
