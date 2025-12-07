//------------------------------------------------------------------------------
// UNIT           : OBD.Gauge.Component.pas
// CONTENTS       : Non-visual gauge controller component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Gauge.Component;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Threading,
  Vcl.Controls,
  OBD.Protocol.Types, OBD.Adapter.Types, OBD.Component.BindingHelpers,
  OBD.Protocol.Component, OBD.CircularGauge, OBD.CircularGauge.Variants, 
  OBD.LinearGauge, OBD.BarGauge;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Event signature used to resolve a gauge value from parsed data messages.
  /// </summary>
  TResolveGaugeValueEvent = procedure(Sender: TObject; const Messages: TArray<IOBDDataMessage>; out Value: Single; out Applied: Boolean) of object;

  /// <summary>
  ///   Non-visual controller that binds protocol messages to any gauge type
  ///   (circular, linear, or bar), optionally auto-applying resolved values on the UI thread.
  /// </summary>
  TOBDGaugeComponent = class(TComponent)
  private
    /// <summary>
    ///   Target control (any gauge type) that receives value updates from resolved messages.
    /// </summary>
    FTargetControl: TControl;
    /// <summary>
    ///   Protocol component supplying parsed message events.
    /// </summary>
    FProtocolComponent: TOBDProtocolComponent;
    /// <summary>
    ///   Flag indicating whether protocol bindings should be applied
    ///   automatically.
    /// </summary>
    FAutoBindProtocol: Boolean;
    /// <summary>
    ///   Flag indicating whether resolved values should be applied to the gauge
    ///   automatically.
    /// </summary>
    FAutoApplyValue: Boolean;
    /// <summary>
    ///   Event handler that translates protocol messages into gauge values.
    /// </summary>
    FOnResolveValue: TResolveGaugeValueEvent;
    /// <summary>
    ///   Synchronization object that guards protocol binding swaps.
    /// </summary>
    FBindingLock: TObject;
    /// <summary>
    ///   Chained protocol message handler stored before binding so existing
    ///   callbacks continue to run.
    /// </summary>
    FChainedOnMessages: TReceiveDataMessagesEvent;
    /// <summary>
    ///   Optional callback raised when protocol bindings are swapped or
    ///   restored.
    /// </summary>
    FOnBindingNotification: TOBDBindingNotification;
    /// <summary>
    ///   Apply the current binding preference to the configured protocol
    ///   component.
    /// </summary>
    procedure RefreshProtocolBinding;
    /// <summary>
    ///   Assign a new protocol component and rebind message handlers when
    ///   enabled.
    /// </summary>
    procedure SetProtocolComponent(const Value: TOBDProtocolComponent);
    /// <summary>
    ///   Assign a target gauge for value application.
    /// </summary>
    procedure SetTargetControl(const Value: TControl);
    /// <summary>
    ///   Toggle the auto-bind flag and refresh bindings accordingly.
    /// </summary>
    procedure SetAutoBindProtocol(const Value: Boolean);
    /// <summary>
    ///   Toggle automatic value application for resolved values.
    /// </summary>
    procedure SetAutoApplyValue(const Value: Boolean);
    /// <summary>
    ///   Handle parsed protocol messages and resolve a gauge value when
    ///   applicable.
    /// </summary>
    procedure HandleMessages(Sender: TObject; const Messages: TArray<IOBDDataMessage>);
    /// <summary>
    ///   Apply the resolved value to the target gauge on the UI thread.
    /// </summary>
    procedure ApplyGaugeValue(const Value: Single);
    /// <summary>
    ///   Restore any active protocol bindings so teardown does not leave stale
    ///   handlers attached.
    /// </summary>
    procedure DetachProtocol;
  public
    /// <summary>
    ///   Allocate synchronization primitives and initialize defaults.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Release bindings and synchronization primitives.
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///   Manually apply a value to the target gauge with UI-thread marshalling.
    /// </summary>
    procedure SetGaugeValue(const Value: Single);
  published
    /// <summary>
    ///   Target gauge control (circular, linear, or bar) that receives value updates from this component.
    /// </summary>
    property TargetControl: TControl read FTargetControl write SetTargetControl;
    /// <summary>
    ///   Protocol component that supplies parsed message events when auto-binding
    ///   is enabled.
    /// </summary>
    property ProtocolComponent: TOBDProtocolComponent read FProtocolComponent write SetProtocolComponent;
    /// <summary>
    ///   Indicates whether the component should automatically subscribe to the
    ///   protocol component's message events.
    /// </summary>
    property AutoBindProtocol: Boolean read FAutoBindProtocol write SetAutoBindProtocol default True;
    /// <summary>
    ///   Indicates whether resolved values should automatically update the
    ///   target gauge.
    /// </summary>
    property AutoApplyValue: Boolean read FAutoApplyValue write SetAutoApplyValue default True;
    /// <summary>
    ///   Event invoked to translate protocol messages into a gauge value when
    ///   AutoApplyValue is enabled.
    /// </summary>
    property OnResolveValue: TResolveGaugeValueEvent read FOnResolveValue write FOnResolveValue;
    /// <summary>
    ///   Optional callback raised when protocol bindings are swapped or
    ///   restored for observability.
    /// </summary>
    property OnBindingNotification: TOBDBindingNotification read FOnBindingNotification
      write FOnBindingNotification;
  end;

implementation

{ TOBDGaugeComponent }

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDGaugeComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBindingLock := TObject.Create;
  FAutoBindProtocol := True;
  FAutoApplyValue := True;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDGaugeComponent.Destroy;
begin
  DetachProtocol;
  FBindingLock.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// APPLY GAUGE VALUE
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.ApplyGaugeValue(const Value: Single);
begin
  if not Assigned(FTargetControl) then
    Exit;
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FTargetControl) then
      begin
        // Apply value to any supported gauge type
        // Note: Check derived classes first (more specific before base)
        if FTargetControl is TOBDCircularGaugeEx then
          TOBDCircularGaugeEx(FTargetControl).Value := Value
        else if FTargetControl is TOBDCircularGauge then
          TOBDCircularGauge(FTargetControl).Value := Value
        else if FTargetControl is TOBDLinearGauge then
          TOBDLinearGauge(FTargetControl).Value := Value
        else if FTargetControl is TOBDBarGauge then
          TOBDBarGauge(FTargetControl).Value := Value;
      end;
    end);
end;

//------------------------------------------------------------------------------
// HANDLE MESSAGES
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.HandleMessages(Sender: TObject; const Messages: TArray<IOBDDataMessage>);
var
  ResolvedValue: Single;
  Applied: Boolean;
begin
  if not FAutoApplyValue then
    Exit;
  Applied := False;
  if Assigned(FOnResolveValue) then
    FOnResolveValue(Self, Messages, ResolvedValue, Applied);
  if Applied then
    ApplyGaugeValue(ResolvedValue);
end;

//------------------------------------------------------------------------------
// REFRESH PROTOCOL BINDING
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.RefreshProtocolBinding;
var
  CurrentHandler: TReceiveDataMessagesEvent;
begin
  if not Assigned(FProtocolComponent) then
  begin
    FChainedOnMessages := nil;
    Exit;
  end;

  CurrentHandler := FProtocolComponent.OnMessages;
  if FAutoBindProtocol then
    TOBDBindingHelpers.SwapHandler<TReceiveDataMessagesEvent>(FBindingLock,
      CurrentHandler, FChainedOnMessages, HandleMessages,
      'Protocol.OnMessages', Self, FOnBindingNotification)
  else
    TOBDBindingHelpers.RestoreHandler<TReceiveDataMessagesEvent>(FBindingLock,
      CurrentHandler, FChainedOnMessages, 'Protocol.OnMessages',
      Self, FOnBindingNotification);
  FProtocolComponent.OnMessages := CurrentHandler;
end;

//------------------------------------------------------------------------------
// SET AUTO APPLY VALUE
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.SetAutoApplyValue(const Value: Boolean);
begin
  if FAutoApplyValue = Value then
    Exit;
  FAutoApplyValue := Value;
end;

//------------------------------------------------------------------------------
// SET AUTO BIND PROTOCOL
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.SetAutoBindProtocol(const Value: Boolean);
begin
  if FAutoBindProtocol = Value then
    Exit;
  FAutoBindProtocol := Value;
  RefreshProtocolBinding;
end;

//------------------------------------------------------------------------------
// SET TARGET CONTROL
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.SetTargetControl(const Value: TControl);
begin
  // Validate that the control is a supported gauge type
  if Assigned(Value) and not (
    (Value is TOBDCircularGauge) or  // Includes all variants (Ex, ThreeQuarter, SemiCircular)
    (Value is TOBDLinearGauge) or
    (Value is TOBDBarGauge)) then
    raise Exception.Create('TargetControl must be a gauge component (Circular, Linear, or Bar gauge)');
  FTargetControl := Value;
end;

//------------------------------------------------------------------------------
// SET PROTOCOL COMPONENT
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.SetProtocolComponent(const Value: TOBDProtocolComponent);
begin
  if FProtocolComponent = Value then
    Exit;
  DetachProtocol;
  FProtocolComponent := Value;
  RefreshProtocolBinding;
end;

//------------------------------------------------------------------------------
// SET GAUGE VALUE
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.SetGaugeValue(const Value: Single);
begin
  ApplyGaugeValue(Value);
end;

//------------------------------------------------------------------------------
// DETACH PROTOCOL
//------------------------------------------------------------------------------
procedure TOBDGaugeComponent.DetachProtocol;
var
  CurrentHandler: TReceiveDataMessagesEvent;
begin
  if Assigned(FProtocolComponent) then
  begin
    CurrentHandler := FProtocolComponent.OnMessages;
    TOBDBindingHelpers.RestoreHandler<TReceiveDataMessagesEvent>(FBindingLock,
      CurrentHandler, FChainedOnMessages, 'Protocol.OnMessages',
      Self, FOnBindingNotification);
    FProtocolComponent.OnMessages := CurrentHandler;
  end
  else
    FChainedOnMessages := nil;
end;

end.

