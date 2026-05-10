//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Base
//
//  TOBDGaugeBase — abstract base for every dial / bar gauge.
//  Owns the value contract, range / clamp, animation, caption /
//  unit text, zones, tick config, and the live-data binding
//  stubs. Subclasses paint.
//
//  The base intentionally exposes BOTH ways of feeding values:
//
//    (a) Direct binding via LiveData + PID properties. Setting
//        both auto-subscribes via TOBDLiveData.Subscribe; the
//        dispatch arrives on the main thread (FireValue queues
//        through TThread.Queue) and HandleLiveValue writes
//        Value. DFM streaming order is irrelevant — Loaded
//        re-subscribes once both properties are streamed.
//
//    (b) Decoupled — host writes Gauge.Value from its own
//        OnValue handler. Both paths coexist on the same
//        component; pick whichever fits the form.
//
//  LiveBindings: SetValue calls TBindings.Notify(Self, 'Value')
//  so any TLinkPropertyToField / TLinkObservableProperty bound
//  to the Value property refreshes automatically.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Base;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Data.Bind.Components,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Anim,
  OBD.UI.Gauges.Types,
  OBD.Service.LiveData;

type
  TOBDGaugeBase = class(TOBDCustomControl)
  strict private
    FMin:                 Double;
    FMax:                 Double;
    FValue:               Double;
    FDisplayValue:        Double;
    FCaption:             string;
    FUnit:                string;
    FDecimals:            Byte;
    FAnimateValueChanges: Boolean;
    FAnim:                TOBDValueAnim;
    FZones:               TOBDGaugeZones;
    FTicks:               TOBDGaugeTickConfig;
    FOnValueChanged:      TOBDGaugeValueEvent;
    FLiveData:            TOBDLiveData;
    FPID:                 Byte;
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure SetCaption(const AValue: string);
    procedure SetUnit(const AValue: string);
    procedure SetDecimals(AValue: Byte);
    procedure SetAnimateValueChanges(AValue: Boolean);
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure SetPID(AValue: Byte);
    procedure HandleAnimFrame(Sender: TObject; AValue: Double);
    procedure HandleAnimDone(Sender: TObject; AFinal: Double);
    procedure HandleLiveValue(Sender: TObject;
      const AValue: TOBDPIDValue);
    procedure SubscribeToLiveData;
    procedure UnsubscribeFromLiveData;
  protected
    /// <summary>Current displayed value. Lags behind
    /// <c>Value</c> during the animation transition; equal to
    /// <c>Value</c> once settled. Subclass paint code reads
    /// this, not <c>Value</c>.</summary>
    property DisplayValue: Double read FDisplayValue;
    /// <summary>Clamped to <c>Min..Max</c>.</summary>
    function Clamp(AValue: Double): Double;
    /// <summary>Convenience: NormaliseValue(Min, Max, Value).</summary>
    function NormalisedValue: Double;
    function NormalisedDisplay: Double;
    /// <summary>Look up the zone colour at the current
    /// <c>DisplayValue</c>. Returns <c>clNone</c> when no zone
    /// applies — paint code falls back to needle / accent
    /// colour.</summary>
    function CurrentZoneColor: TColor;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>Format a value as the gauge displays it
    /// (decimal places + unit suffix). Public so hosts can
    /// share the same formatter for in-form labels next to
    /// the gauge.</summary>
    function FormatValue(AValue: Double): string;

    /// <summary>Replace the zone array. Triggers a repaint.</summary>
    procedure SetZones(const AZones: TOBDGaugeZones);
    function  Zones: TOBDGaugeZones;

    /// <summary>Replace the tick config. Triggers a repaint.</summary>
    procedure SetTickConfig(const ATicks: TOBDGaugeTickConfig);
    function  TickConfig: TOBDGaugeTickConfig;
  published
    /// <summary>Minimum value on the scale.</summary>
    property Min: Double read FMin write SetMin;
    /// <summary>Maximum value on the scale.</summary>
    property Max: Double read FMax write SetMax;
    /// <summary>Current value. Clamped to <c>Min..Max</c>.
    /// Setting fires <c>OnValueChanged</c> and (when
    /// <see cref="AnimateValueChanges"/> is True) animates the
    /// displayed value to the new target.</summary>
    property Value: Double read FValue write SetValue;
    /// <summary>Top-of-gauge text (e.g. "RPM", "Coolant").</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Unit suffix shown next to the value
    /// (e.g. "°C", "rpm", "bar").</summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Decimal places on the displayed value.</summary>
    property Decimals: Byte read FDecimals write SetDecimals default 0;
    /// <summary>When True (default), needle / bar sweeps to
    /// the new value via <see cref="TOBDValueAnim"/>; when
    /// False, snaps instantly.</summary>
    property AnimateValueChanges: Boolean
      read FAnimateValueChanges write SetAnimateValueChanges
      default True;
    /// <summary>Source TOBDLiveData. When set together with
    /// <see cref="PID"/>, the gauge auto-subscribes via
    /// <c>TOBDLiveData.Subscribe</c> and <c>Value</c> updates
    /// itself on every dispatch — host writes no glue code.
    /// nil to switch back to host-driven <c>Value</c>.</summary>
    property LiveData: TOBDLiveData read FLiveData write SetLiveData;
    /// <summary>PID the gauge subscribes to on
    /// <see cref="LiveData"/>. Changing this re-subscribes to
    /// the new PID and unsubscribes from the old.</summary>
    property PID: Byte read FPID write SetPID;

    property OnValueChanged: TOBDGaugeValueEvent
      read FOnValueChanged write FOnValueChanged;
  end;

implementation

constructor TOBDGaugeBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 200;
  Height := 200;
  FMin := 0;
  FMax := 100;
  FValue := 0;
  FDisplayValue := 0;
  FDecimals := 0;
  FAnimateValueChanges := True;
  FTicks := DefaultTickConfig;
  FAnim := TOBDValueAnim.Create;
  FAnim.OnFrame := HandleAnimFrame;
  FAnim.OnDone  := HandleAnimDone;
  FAnim.DurationMs := 300;
  FAnim.Easing := emSpring;
end;

destructor TOBDGaugeBase.Destroy;
begin
  UnsubscribeFromLiveData;
  FAnim.Free;
  inherited;
end;

procedure TOBDGaugeBase.Loaded;
begin
  inherited;
  FDisplayValue := Clamp(FValue);
  // DFM streaming may have set LiveData before PID (or vice
  // versa); ensure the subscription reflects the final state.
  SubscribeToLiveData;
end;

procedure TOBDGaugeBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLiveData) then
    FLiveData := nil;
end;

function TOBDGaugeBase.Clamp(AValue: Double): Double;
begin
  if SameValue(FMin, FMax) then Exit(FMin);
  if AValue < FMin then Result := FMin
  else if AValue > FMax then Result := FMax
  else                       Result := AValue;
end;

function TOBDGaugeBase.NormalisedValue: Double;
begin
  Result := NormaliseValue(FMin, FMax, FValue);
end;

function TOBDGaugeBase.NormalisedDisplay: Double;
begin
  Result := NormaliseValue(FMin, FMax, FDisplayValue);
end;

function TOBDGaugeBase.CurrentZoneColor: TColor;
var Z: TOBDGaugeZone;
begin
  Z := ResolveZone(FZones, FDisplayValue);
  if (Z.EndValue > Z.StartValue) or (Z.Color <> 0) then
    Result := Z.Color
  else
    Result := clNone;
end;

function TOBDGaugeBase.FormatValue(AValue: Double): string;
var FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  if FDecimals = 0 then
    Result := FormatFloat('0', AValue, FS)
  else
    Result := FormatFloat('0.' + StringOfChar('0', FDecimals),
      AValue, FS);
  if FUnit <> '' then
    Result := Result + ' ' + FUnit;
end;

procedure TOBDGaugeBase.SetMin(AValue: Double);
begin
  if SameValue(FMin, AValue) then Exit;
  FMin := AValue;
  FDisplayValue := Clamp(FDisplayValue);
  Repaint;
end;

procedure TOBDGaugeBase.SetMax(AValue: Double);
begin
  if SameValue(FMax, AValue) then Exit;
  FMax := AValue;
  FDisplayValue := Clamp(FDisplayValue);
  Repaint;
end;

procedure TOBDGaugeBase.SetValue(AValue: Double);
var
  Clamped: Double;
begin
  Clamped := Clamp(AValue);
  if SameValue(FValue, Clamped) then Exit;
  FValue := Clamped;
  if FAnimateValueChanges and not (csLoading in ComponentState) and
     not (csDesigning in ComponentState) then
    FAnim.Animate(FDisplayValue, FValue)
  else
  begin
    FAnim.SnapTo(FValue);
    FDisplayValue := FValue;
    Repaint;
  end;
  if Assigned(FOnValueChanged) then
    try
      FOnValueChanged(Self, FValue);
    except
    end;
  // LiveBindings refresh - notifies any TLinkPropertyToField /
  // TLinkObservableProperty bound to 'Value' on this gauge.
  // Best-effort; the call is a no-op when no binding is wired.
  try
    TBindings.Notify(Self, 'Value');
  except
    // Bindings subsystem may not be initialised yet during
    // design-time streaming or in stripped-down runtimes.
  end;
end;

procedure TOBDGaugeBase.SetCaption(const AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue; Repaint;
end;

procedure TOBDGaugeBase.SetUnit(const AValue: string);
begin
  if FUnit = AValue then Exit;
  FUnit := AValue; Repaint;
end;

procedure TOBDGaugeBase.SetDecimals(AValue: Byte);
begin
  if FDecimals = AValue then Exit;
  FDecimals := AValue; Repaint;
end;

procedure TOBDGaugeBase.SetAnimateValueChanges(AValue: Boolean);
begin
  if FAnimateValueChanges = AValue then Exit;
  FAnimateValueChanges := AValue;
end;

procedure TOBDGaugeBase.SetLiveData(AValue: TOBDLiveData);
begin
  if FLiveData = AValue then Exit;
  UnsubscribeFromLiveData;
  if FLiveData <> nil then FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then FLiveData.FreeNotification(Self);
  SubscribeToLiveData;
end;

procedure TOBDGaugeBase.SetPID(AValue: Byte);
begin
  if FPID = AValue then Exit;
  UnsubscribeFromLiveData;
  FPID := AValue;
  SubscribeToLiveData;
end;

procedure TOBDGaugeBase.SubscribeToLiveData;
begin
  if (FLiveData <> nil) and not (csDesigning in ComponentState) then
    FLiveData.Subscribe(FPID, HandleLiveValue);
end;

procedure TOBDGaugeBase.UnsubscribeFromLiveData;
begin
  if FLiveData <> nil then
    FLiveData.Unsubscribe(FPID, HandleLiveValue);
end;

procedure TOBDGaugeBase.HandleLiveValue(Sender: TObject;
  const AValue: TOBDPIDValue);
begin
  // TOBDLiveData.FireValue runs the dispatch on the main thread
  // (TThread.Queue when called from the poll worker), so we can
  // touch VCL state directly here.
  if not IsNan(AValue.Value) then
    Value := AValue.Value;
end;

procedure TOBDGaugeBase.HandleAnimFrame(Sender: TObject; AValue: Double);
begin
  FDisplayValue := AValue;
  Repaint;
end;

procedure TOBDGaugeBase.HandleAnimDone(Sender: TObject; AFinal: Double);
begin
  FDisplayValue := AFinal;
  Repaint;
end;

procedure TOBDGaugeBase.SetZones(const AZones: TOBDGaugeZones);
begin
  FZones := Copy(AZones);
  Repaint;
end;

function TOBDGaugeBase.Zones: TOBDGaugeZones;
begin
  Result := Copy(FZones);
end;

procedure TOBDGaugeBase.SetTickConfig(const ATicks: TOBDGaugeTickConfig);
begin
  FTicks := ATicks;
  Repaint;
end;

function TOBDGaugeBase.TickConfig: TOBDGaugeTickConfig;
begin
  Result := FTicks;
end;

end.
