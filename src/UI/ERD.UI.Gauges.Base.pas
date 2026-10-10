//------------------------------------------------------------------------------
//  ERD.UI.Gauges.Base
//
//  TOBDGaugeBase - shared value contract for the single-channel
//  dashboard controls (dial, bar, value tile).
//
//  Everything that is not drawing lives here, so the three controls
//  behave identically:
//    - one Channel (TOBDChannelBinding) feeds the value; the public
//      Value property is a shortcut for Channel.PushValue;
//    - DataState reports no data / live / stale / out of range and
//      every subclass paints each state distinctly;
//    - Alerts (low/high warning/alarm) drive both the value colour
//      and, unless custom zones are set, the coloured scale bands;
//    - values, ranges and thresholds are stored in the metric unit
//      the decoder delivers and converted to imperial at paint time;
//    - at design time (or with ForcePreview) the control shows a
//      realistic sample value instead of an empty scale;
//    - SaveSettings / LoadSettings persist the user-facing setup for
//      dashboard layouts.
//
//  LiveBindings: setting Value calls TBindings.Notify(Self, 'Value').
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Channel binding, data states, alert thresholds,
//                     unit conversion, design-time preview and JSON
//                     settings.
//------------------------------------------------------------------------------

unit ERD.UI.Gauges.Base;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  Vcl.Controls,
  Vcl.Graphics,
  System.Bindings.Helper,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Control,
  ERD.UI.Anim,
  ERD.UI.Units,
  ERD.UI.Binding,
  ERD.UI.Gauges.Types,
  ERD.Service.LiveData;

type
  /// <summary>One alert threshold slot.</summary>
  TOBDGaugeAlertKind = (
    /// <summary>Value at or below <c>LowAlarm</c>.</summary>
    alkLowAlarm,
    /// <summary>Value at or below <c>LowWarning</c>.</summary>
    alkLowWarning,
    /// <summary>Value at or above <c>HighWarning</c>.</summary>
    alkHighWarning,
    /// <summary>Value at or above <c>HighAlarm</c>.</summary>
    alkHighAlarm);

  /// <summary>Set of enabled alert thresholds.</summary>
  TOBDGaugeAlertKinds = set of TOBDGaugeAlertKind;

  /// <summary>Warning and alarm thresholds of a gauge, in the metric
  /// unit of the channel.</summary>
  /// <remarks>Only thresholds listed in <see cref="Kinds"/> are
  /// evaluated. Typical: coolant <c>HighWarning = 105</c>,
  /// <c>HighAlarm = 115</c>; battery <c>LowWarning = 12.0</c>,
  /// <c>LowAlarm = 11.5</c>, <c>HighAlarm = 15.0</c>.</remarks>
  TOBDGaugeAlerts = class(TPersistent)
  strict private
    FKinds: TOBDGaugeAlertKinds;
    FLowAlarm: Double;
    FLowWarning: Double;
    FHighWarning: Double;
    FHighAlarm: Double;
    FOnChange: TNotifyEvent;
    procedure SetKinds(AValue: TOBDGaugeAlertKinds);
    procedure SetLowAlarm(AValue: Double);
    procedure SetLowWarning(AValue: Double);
    procedure SetHighWarning(AValue: Double);
    procedure SetHighAlarm(AValue: Double);
    procedure Changed;
  public
    /// <summary>Copies all thresholds from another alerts object.
    /// </summary>
    /// <param name="ASource">Alerts to copy.</param>
    procedure Assign(ASource: TPersistent); override;
    /// <summary>Evaluates a value against the enabled thresholds.
    /// </summary>
    /// <param name="AValue">Value in the metric unit.</param>
    /// <returns>Highest matching alert level.</returns>
    function Level(AValue: Double): TOBDAlertLevel;
    /// <summary>Enables a high warning and a high alarm in one call.
    /// </summary>
    /// <param name="AWarning">Warning threshold.</param>
    /// <param name="AAlarm">Alarm threshold.</param>
    procedure SetHigh(AWarning, AAlarm: Double);
    /// <summary>Enables a low warning and a low alarm in one call.
    /// </summary>
    /// <param name="AWarning">Warning threshold.</param>
    /// <param name="AAlarm">Alarm threshold.</param>
    procedure SetLow(AWarning, AAlarm: Double);
    /// <summary>Fires after any threshold changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    /// <summary>Thresholds that are active. Always streamed, because
    /// controls enable different thresholds in their constructors.
    /// </summary>
    property Kinds: TOBDGaugeAlertKinds read FKinds write SetKinds
      nodefault;
    /// <summary>Alarm when the value is at or below this.</summary>
    property LowAlarm: Double read FLowAlarm write SetLowAlarm;
    /// <summary>Warning when the value is at or below this.</summary>
    property LowWarning: Double read FLowWarning write SetLowWarning;
    /// <summary>Warning when the value is at or above this.</summary>
    property HighWarning: Double read FHighWarning write SetHighWarning;
    /// <summary>Alarm when the value is at or above this.</summary>
    property HighAlarm: Double read FHighAlarm write SetHighAlarm;
  end;

  /// <summary>Abstract base for single-channel dashboard controls.
  /// Subclasses override <c>PaintControl</c> and read
  /// <see cref="PaintValue"/>, <see cref="DataState"/>,
  /// <see cref="AlertLevel"/> and the display helpers.</summary>
  TOBDGaugeBase = class(TOBDCustomControl)
  strict private
    FChannel: TOBDChannelBinding;
    FAlerts: TOBDGaugeAlerts;
    FMin: Double;
    FMax: Double;
    FDisplayValue: Double;
    FSessionMin: Double;
    FSessionMax: Double;
    FCaption: string;
    FUnit: string;
    FDecimals: Byte;
    FAnimateValueChanges: Boolean;
    FAnim: TOBDValueAnim;
    FZones: TOBDGaugeZones;
    FOnValueChanged: TOBDGaugeValueEvent;
    procedure SetMin(AValue: Double);
    procedure SetMax(AValue: Double);
    function GetValue: Double;
    procedure SetValue(AValue: Double);
    procedure SetCaption(const AValue: string);
    procedure SetUnit(const AValue: string);
    procedure SetDecimals(AValue: Byte);
    procedure SetChannel(AValue: TOBDChannelBinding);
    procedure SetAlerts(AValue: TOBDGaugeAlerts);
    procedure HandleAnimFrame(Sender: TObject; AValue: Double);
    procedure HandleAnimDone(Sender: TObject; AFinal: Double);
    procedure HandleChannelValue(Sender: TObject);
    procedure HandleChannelState(Sender: TObject);
    procedure HandleAlertsChange(Sender: TObject);
  protected
    /// <summary>Re-subscribes the channel after streaming.</summary>
    procedure Loaded; override;
    /// <summary>Clears the channel source when it is freed.</summary>
    /// <param name="AComponent">Component being inserted/removed.
    /// </param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Hook for subclasses that keep a history (sparkline,
    /// trend arrow). Called for every new value.</summary>
    /// <param name="AValue">New value in the metric unit.</param>
    procedure ValueArrived(AValue: Double); virtual;

    /// <summary>Clamps to <c>Min..Max</c>.</summary>
    /// <param name="AValue">Value to clamp.</param>
    /// <returns>Clamped value.</returns>
    function Clamp(AValue: Double): Double;
    /// <summary>Sample value painted at design time:
    /// 62 percent of the range, or between the warning thresholds.
    /// </summary>
    /// <returns>Preview value in the metric unit.</returns>
    function PreviewValue: Double; virtual;
    /// <summary>Value to draw right now: the preview value in
    /// preview mode without data, otherwise the (animated) display
    /// value. Metric unit, clamped to the range.</summary>
    /// <returns>Value for painting.</returns>
    function PaintValue: Double;
    /// <summary><see cref="PaintValue"/> normalised to 0..1.</summary>
    /// <returns>Fraction of the scale.</returns>
    function PaintFraction: Double;
    /// <summary>Unit conversion for the current unit system.</summary>
    /// <returns>Conversion record.</returns>
    function Conversion: TOBDUnitConversion;
    /// <summary>Formats a metric value in display units with
    /// <see cref="Decimals"/> places, without the unit.</summary>
    /// <param name="AMetric">Value in the metric unit.</param>
    /// <returns>Formatted number.</returns>
    function FormatDisplay(AMetric: Double): string;
    /// <summary>Text for the big readout: the formatted value, or a
    /// dash placeholder when there is no data.</summary>
    /// <returns>Readout text without unit.</returns>
    function ReadoutText: string;
    /// <summary>Short status text for the current data state
    /// (<c>'NO DATA'</c>, <c>'STALE'</c>, <c>'OVER'</c>,
    /// <c>'UNDER'</c>); empty when live.</summary>
    /// <returns>Status text.</returns>
    function StateText: string;
    /// <summary>Colour for the value: alert colour, greyed when stale
    /// or without data, <c>ANormal</c> otherwise.</summary>
    /// <param name="ANormal">Colour for a normal live value.</param>
    /// <returns>Value colour.</returns>
    function ValueColor(ANormal: TColor): TColor;
    /// <summary>Zones to paint: custom zones when set via
    /// <see cref="SetZones"/>, otherwise bands derived from
    /// <see cref="Alerts"/>.</summary>
    /// <returns>Zones in the metric unit.</returns>
    function EffectiveZones: TOBDGaugeZones;
    /// <summary>Picks a font size that fits <c>AText</c> into the
    /// given box.</summary>
    /// <param name="ACanvas">Canvas whose font is adjusted.</param>
    /// <param name="AText">Text to fit.</param>
    /// <param name="AMaxW">Available width in pixels.</param>
    /// <param name="AMaxH">Available height in pixels.</param>
    procedure FitFont(ACanvas: TCanvas; const AText: string;
      AMaxW, AMaxH: Integer);
    /// <summary>Draws text centred in a rectangle.</summary>
    /// <param name="ACanvas">Target canvas (font already set).</param>
    /// <param name="ARect">Box to centre in.</param>
    /// <param name="AText">Text.</param>
    procedure DrawCentred(ACanvas: TCanvas; const ARect: TRect;
      const AText: string);
  public
    /// <summary>Creates the gauge with a 0..100 range.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops animation and releases the channel.</summary>
    destructor Destroy; override;

    /// <summary>Formats a metric value in display units including the
    /// display unit, e.g. <c>'203 &#176;F'</c>.</summary>
    /// <param name="AValue">Value in the metric unit.</param>
    /// <returns>Formatted value with unit.</returns>
    function FormatValue(AValue: Double): string;
    /// <summary>Current data state.</summary>
    /// <returns>No data, live, stale or out of range.</returns>
    function DataState: TOBDDataState;
    /// <summary>Alert level of the current value (or the preview
    /// value in preview mode).</summary>
    /// <returns>Normal, warning or alarm.</returns>
    function AlertLevel: TOBDAlertLevel;
    /// <summary>Unit actually shown (after unit-system conversion).
    /// </summary>
    /// <returns>Display unit text.</returns>
    function DisplayUnit: string;
    /// <summary>Metric unit of the channel: <see cref="&amp;Unit"/>
    /// when set, otherwise the unit reported by the decoder.
    /// </summary>
    /// <returns>Metric unit text.</returns>
    function MetricUnit: string;
    /// <summary>Resets the session minimum / maximum.</summary>
    procedure ResetMinMax;
    /// <summary>Replaces the zones with custom bands. An empty array
    /// returns to alert-derived bands.</summary>
    /// <param name="AZones">Zones in the metric unit.</param>
    procedure SetZones(const AZones: TOBDGaugeZones);
    /// <summary>Custom zones (empty when alert-derived).</summary>
    /// <returns>Copy of the custom zones.</returns>
    function Zones: TOBDGaugeZones;
    /// <summary>Data source of <see cref="Channel"/>.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
    /// <summary>Writes caption, unit, range, decimals, channel and
    /// alerts.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Restores settings written by
    /// <see cref="SaveSettings"/>.</summary>
    /// <param name="AObject">Source object.</param>
    procedure LoadSettings(AObject: TJSONObject); override;

    /// <summary>Last value in the metric unit (NaN before the first).
    /// Writing pushes a value through <see cref="Channel"/>, exactly
    /// like a value from the data source.</summary>
    property Value: Double read GetValue write SetValue;
    /// <summary>Lowest value seen since the last
    /// <see cref="ResetMinMax"/>; NaN when none.</summary>
    property SessionMin: Double read FSessionMin;
    /// <summary>Highest value seen since the last
    /// <see cref="ResetMinMax"/>; NaN when none.</summary>
    property SessionMax: Double read FSessionMax;
    /// <summary>Animated value currently drawn.</summary>
    property DisplayValue: Double read FDisplayValue;
  published
    /// <summary>Data channel: source, PID and stale timeout.</summary>
    property Channel: TOBDChannelBinding read FChannel write SetChannel;
    /// <summary>Warning / alarm thresholds (metric unit).</summary>
    property Alerts: TOBDGaugeAlerts read FAlerts write SetAlerts;
    /// <summary>Scale minimum (metric unit).</summary>
    property Min: Double read FMin write SetMin;
    /// <summary>Scale maximum (metric unit).</summary>
    property Max: Double read FMax write SetMax;
    /// <summary>Label, e.g. <c>'Coolant'</c>.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Metric unit of the values. Leave empty to use the
    /// unit reported by the PID decoder.</summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Decimal places of the readout.</summary>
    property Decimals: Byte read FDecimals write SetDecimals default 0;
    /// <summary>Animate between values (needle sweep). Off snaps.
    /// </summary>
    property AnimateValueChanges: Boolean read FAnimateValueChanges
      write FAnimateValueChanges default True;
    /// <summary>Fires for every new value.</summary>
    property OnValueChanged: TOBDGaugeValueEvent read FOnValueChanged
      write FOnValueChanged;
  end;

implementation

{ ---- TOBDGaugeAlerts ------------------------------------------------------- }

procedure TOBDGaugeAlerts.Assign(ASource: TPersistent);
var
  Src: TOBDGaugeAlerts;
begin
  if ASource is TOBDGaugeAlerts then
  begin
    Src := TOBDGaugeAlerts(ASource);
    FKinds := Src.Kinds;
    FLowAlarm := Src.LowAlarm;
    FLowWarning := Src.LowWarning;
    FHighWarning := Src.HighWarning;
    FHighAlarm := Src.HighAlarm;
    Changed;
  end
  else
    inherited Assign(ASource);
end;

procedure TOBDGaugeAlerts.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TOBDGaugeAlerts.Level(AValue: Double): TOBDAlertLevel;
begin
  Result := alvNormal;
  if IsNan(AValue) then
    Exit;
  if ((alkLowAlarm in FKinds) and (AValue <= FLowAlarm)) or
    ((alkHighAlarm in FKinds) and (AValue >= FHighAlarm)) then
    Result := alvAlarm
  else if ((alkLowWarning in FKinds) and (AValue <= FLowWarning)) or
    ((alkHighWarning in FKinds) and (AValue >= FHighWarning)) then
    Result := alvWarning;
end;

procedure TOBDGaugeAlerts.SetHigh(AWarning, AAlarm: Double);
begin
  FHighWarning := AWarning;
  FHighAlarm := AAlarm;
  FKinds := FKinds + [alkHighWarning, alkHighAlarm];
  Changed;
end;

procedure TOBDGaugeAlerts.SetLow(AWarning, AAlarm: Double);
begin
  FLowWarning := AWarning;
  FLowAlarm := AAlarm;
  FKinds := FKinds + [alkLowWarning, alkLowAlarm];
  Changed;
end;

procedure TOBDGaugeAlerts.SetKinds(AValue: TOBDGaugeAlertKinds);
begin
  if FKinds = AValue then
    Exit;
  FKinds := AValue;
  Changed;
end;

procedure TOBDGaugeAlerts.SetLowAlarm(AValue: Double);
begin
  FLowAlarm := AValue;
  Changed;
end;

procedure TOBDGaugeAlerts.SetLowWarning(AValue: Double);
begin
  FLowWarning := AValue;
  Changed;
end;

procedure TOBDGaugeAlerts.SetHighWarning(AValue: Double);
begin
  FHighWarning := AValue;
  Changed;
end;

procedure TOBDGaugeAlerts.SetHighAlarm(AValue: Double);
begin
  FHighAlarm := AValue;
  Changed;
end;

{ ---- TOBDGaugeBase --------------------------------------------------------- }

constructor TOBDGaugeBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  FMin := 0;
  FMax := 100;
  FDisplayValue := 0;
  FSessionMin := NaN;
  FSessionMax := NaN;
  FDecimals := 0;
  FAnimateValueChanges := True;
  FChannel := TOBDChannelBinding.Create(Self);
  FChannel.OnValue := HandleChannelValue;
  FChannel.OnStateChange := HandleChannelState;
  FAlerts := TOBDGaugeAlerts.Create;
  FAlerts.OnChange := HandleAlertsChange;
  FAnim := TOBDValueAnim.Create;
  FAnim.OnFrame := HandleAnimFrame;
  FAnim.OnDone := HandleAnimDone;
  FAnim.DurationMs := 300;
  FAnim.Easing := emEaseOut;
end;

destructor TOBDGaugeBase.Destroy;
begin
  FAnim.Free;
  FChannel.Free;
  FAlerts.Free;
  inherited;
end;

procedure TOBDGaugeBase.Loaded;
begin
  inherited;
  FChannel.Rebind;
end;

procedure TOBDGaugeBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FChannel <> nil) then
    FChannel.SourceRemoved(AComponent);
end;

procedure TOBDGaugeBase.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    FChannel.Source := TOBDLiveData(ASource)
  else
    FChannel.Source := nil;
end;

function TOBDGaugeBase.Clamp(AValue: Double): Double;
begin
  if IsNan(AValue) then
    Exit(FMin);
  if FMax <= FMin then
    Exit(FMin);
  if AValue < FMin then
    Result := FMin
  else if AValue > FMax then
    Result := FMax
  else
    Result := AValue;
end;

function TOBDGaugeBase.PreviewValue: Double;
begin
  if (alkHighWarning in FAlerts.Kinds) and (FAlerts.HighWarning > FMin) and
    (FAlerts.HighWarning < FMax) then
    Result := FMin + (FAlerts.HighWarning - FMin) * 0.8
  else
    Result := FMin + (FMax - FMin) * 0.62;
end;

function TOBDGaugeBase.PaintValue: Double;
begin
  if IsPreview and not FChannel.HasValue then
    Result := Clamp(PreviewValue)
  else if not FChannel.HasValue then
    Result := FMin
  else
    Result := Clamp(FDisplayValue);
end;

function TOBDGaugeBase.PaintFraction: Double;
begin
  Result := NormaliseValue(FMin, FMax, PaintValue);
end;

function TOBDGaugeBase.GetValue: Double;
begin
  Result := FChannel.Value;
end;

procedure TOBDGaugeBase.SetValue(AValue: Double);
begin
  FChannel.PushValue(AValue);
end;

procedure TOBDGaugeBase.ValueArrived(AValue: Double);
begin
  // Subclasses keep history here.
end;

procedure TOBDGaugeBase.HandleChannelValue(Sender: TObject);
var
  V: Double;
begin
  V := FChannel.Value;
  if IsNan(V) then
  begin
    Invalidate;
    Exit;
  end;
  if IsNan(FSessionMin) or (V < FSessionMin) then
    FSessionMin := V;
  if IsNan(FSessionMax) or (V > FSessionMax) then
    FSessionMax := V;
  ValueArrived(V);
  if FAnimateValueChanges and HandleAllocated and Visible and
    not(csDesigning in ComponentState) then
    FAnim.Animate(FDisplayValue, Clamp(V))
  else
  begin
    FAnim.SnapTo(Clamp(V));
    FDisplayValue := Clamp(V);
  end;
  Invalidate;
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Self, V);
  if ([csDesigning, csDestroying] * ComponentState) = [] then
    try
      TBindings.Notify(Self, 'Value');
    except
      // Bindings subsystem may not be initialised in stripped-down
      // runtimes; the gauge itself is already updated.
    end;
end;

procedure TOBDGaugeBase.HandleChannelState(Sender: TObject);
begin
  if not FChannel.HasValue then
  begin
    FAnim.Stop;
    FDisplayValue := FMin;
  end;
  Invalidate;
end;

procedure TOBDGaugeBase.HandleAlertsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDGaugeBase.HandleAnimFrame(Sender: TObject; AValue: Double);
begin
  FDisplayValue := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.HandleAnimDone(Sender: TObject; AFinal: Double);
begin
  FDisplayValue := AFinal;
  Invalidate;
end;

function TOBDGaugeBase.DataState: TOBDDataState;
var
  V: Double;
begin
  if IsPreview and not FChannel.HasValue then
    Exit(dstLive);
  if not FChannel.HasValue or IsNan(FChannel.Value) then
    Exit(dstNoData);
  if FChannel.IsStale then
    Exit(dstStale);
  V := FChannel.Value;
  if (V < FMin) or (V > FMax) then
    Result := dstOutOfRange
  else
    Result := dstLive;
end;

function TOBDGaugeBase.AlertLevel: TOBDAlertLevel;
begin
  if IsPreview and not FChannel.HasValue then
    Result := FAlerts.Level(PreviewValue)
  else if FChannel.HasValue then
    Result := FAlerts.Level(FChannel.Value)
  else
    Result := alvNormal;
end;

function TOBDGaugeBase.MetricUnit: string;
begin
  if FUnit <> '' then
    Result := FUnit
  else
    Result := FChannel.LastUnit;
end;

function TOBDGaugeBase.Conversion: TOBDUnitConversion;
begin
  Result := OBDUnitConversion(MetricUnit, UnitSystem);
end;

function TOBDGaugeBase.DisplayUnit: string;
begin
  Result := Conversion.DisplayUnit;
end;

function TOBDGaugeBase.FormatDisplay(AMetric: Double): string;
begin
  if IsNan(AMetric) then
    Exit('--');
  Result := OBDFormatNumber(Conversion.ToDisplay(AMetric), FDecimals);
end;

function TOBDGaugeBase.FormatValue(AValue: Double): string;
var
  U: string;
begin
  Result := FormatDisplay(AValue);
  U := DisplayUnit;
  if U <> '' then
    Result := Result + ' ' + U;
end;

function TOBDGaugeBase.ReadoutText: string;
begin
  if IsPreview and not FChannel.HasValue then
    Result := FormatDisplay(PreviewValue)
  else if (not FChannel.HasValue) or IsNan(FChannel.Value) then
    Result := '--'
  else
    Result := FormatDisplay(FChannel.Value);
end;

function TOBDGaugeBase.StateText: string;
begin
  case DataState of
    dstNoData:
      Result := 'NO DATA';
    dstStale:
      Result := 'STALE';
    dstOutOfRange:
      if FChannel.Value > FMax then
        Result := 'OVER'
      else
        Result := 'UNDER';
  else
    Result := '';
  end;
end;

function TOBDGaugeBase.ValueColor(ANormal: TColor): TColor;
begin
  case DataState of
    dstNoData, dstStale:
      Result := Palette.Subtle;
  else
    Result := AlertColor(AlertLevel, ANormal);
  end;
end;

function TOBDGaugeBase.EffectiveZones: TOBDGaugeZones;
var
  N: Integer;
  K: TOBDGaugeAlertKinds;
  Z: TOBDGaugeZones;

  procedure Add(AStart, AEnd: Double; AColor: TColor);
  begin
    if AEnd <= AStart then
      Exit;
    SetLength(Z, N + 1);
    Z[N] := MakeGaugeZone(AStart, AEnd, AColor);
    Inc(N);
  end;

begin
  if Length(FZones) > 0 then
    Exit(Copy(FZones));
  Z := nil;
  N := 0;
  K := FAlerts.Kinds;
  if alkLowAlarm in K then
    Add(FMin, FAlerts.LowAlarm, Palette.Danger);
  if alkLowWarning in K then
  begin
    if alkLowAlarm in K then
      Add(FAlerts.LowAlarm, FAlerts.LowWarning, Palette.Warning)
    else
      Add(FMin, FAlerts.LowWarning, Palette.Warning);
  end;
  if alkHighWarning in K then
  begin
    if alkHighAlarm in K then
      Add(FAlerts.HighWarning, FAlerts.HighAlarm, Palette.Warning)
    else
      Add(FAlerts.HighWarning, FMax, Palette.Warning);
  end;
  if alkHighAlarm in K then
    Add(FAlerts.HighAlarm, FMax, Palette.Danger);
  Result := Z;
end;

procedure TOBDGaugeBase.FitFont(ACanvas: TCanvas; const AText: string;
  AMaxW, AMaxH: Integer);
var
  Size, W, H: Integer;
begin
  Size := System.Math.Max(6, AMaxH);
  ACanvas.Font.Height := -Size;
  W := ACanvas.TextWidth(AText);
  H := ACanvas.TextHeight(AText);
  while (Size > 6) and ((W > AMaxW) or (H > AMaxH)) do
  begin
    Dec(Size, System.Math.Max(1, Size div 10));
    ACanvas.Font.Height := -Size;
    W := ACanvas.TextWidth(AText);
    H := ACanvas.TextHeight(AText);
  end;
end;

procedure TOBDGaugeBase.DrawCentred(ACanvas: TCanvas; const ARect: TRect;
  const AText: string);
var
  W, H: Integer;
begin
  W := ACanvas.TextWidth(AText);
  H := ACanvas.TextHeight(AText);
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(ARect.Left + (ARect.Width - W) div 2,
    ARect.Top + (ARect.Height - H) div 2, AText);
end;

procedure TOBDGaugeBase.ResetMinMax;
begin
  FSessionMin := NaN;
  FSessionMax := NaN;
  Invalidate;
end;

procedure TOBDGaugeBase.SetZones(const AZones: TOBDGaugeZones);
begin
  FZones := Copy(AZones);
  Invalidate;
end;

function TOBDGaugeBase.Zones: TOBDGaugeZones;
begin
  Result := Copy(FZones);
end;

procedure TOBDGaugeBase.SetMin(AValue: Double);
begin
  if SameValue(FMin, AValue) then
    Exit;
  FMin := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.SetMax(AValue: Double);
begin
  if SameValue(FMax, AValue) then
    Exit;
  FMax := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.SetUnit(const AValue: string);
begin
  if FUnit = AValue then
    Exit;
  FUnit := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.SetDecimals(AValue: Byte);
begin
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Invalidate;
end;

procedure TOBDGaugeBase.SetChannel(AValue: TOBDChannelBinding);
begin
  FChannel.Assign(AValue);
end;

procedure TOBDGaugeBase.SetAlerts(AValue: TOBDGaugeAlerts);
begin
  FAlerts.Assign(AValue);
end;

procedure TOBDGaugeBase.SaveSettings(AObject: TJSONObject);
var
  A: TJSONObject;
begin
  AObject.AddPair('caption', FCaption);
  AObject.AddPair('unit', FUnit);
  AObject.AddPair('min', TJSONNumber.Create(FMin));
  AObject.AddPair('max', TJSONNumber.Create(FMax));
  AObject.AddPair('decimals', TJSONNumber.Create(FDecimals));
  AObject.AddPair('pid', TJSONNumber.Create(FChannel.PID));
  AObject.AddPair('staleAfterMs', TJSONNumber.Create(FChannel.StaleAfterMs));
  A := TJSONObject.Create;
  AObject.AddPair('alerts', A);
  if alkLowAlarm in FAlerts.Kinds then
    A.AddPair('lowAlarm', TJSONNumber.Create(FAlerts.LowAlarm));
  if alkLowWarning in FAlerts.Kinds then
    A.AddPair('lowWarning', TJSONNumber.Create(FAlerts.LowWarning));
  if alkHighWarning in FAlerts.Kinds then
    A.AddPair('highWarning', TJSONNumber.Create(FAlerts.HighWarning));
  if alkHighAlarm in FAlerts.Kinds then
    A.AddPair('highAlarm', TJSONNumber.Create(FAlerts.HighAlarm));
end;

procedure TOBDGaugeBase.LoadSettings(AObject: TJSONObject);
var
  S: string;
  D: Double;
  I: Integer;
  AV: TJSONValue;
  A: TJSONObject;
  K: TOBDGaugeAlertKinds;
begin
  S := FCaption;
  if OBDJsonReadStr(AObject, 'caption', S) then
    Caption := S;
  S := FUnit;
  if OBDJsonReadStr(AObject, 'unit', S) then
    &Unit := S;
  D := FMin;
  if OBDJsonReadFloat(AObject, 'min', D) then
    Min := D;
  D := FMax;
  if OBDJsonReadFloat(AObject, 'max', D) then
    Max := D;
  I := FDecimals;
  if OBDJsonReadInt(AObject, 'decimals', I) then
    Decimals := Byte(EnsureRange(I, 0, 6));
  I := FChannel.PID;
  if OBDJsonReadInt(AObject, 'pid', I) then
    FChannel.PID := Byte(EnsureRange(I, 0, 255));
  I := Integer(FChannel.StaleAfterMs);
  if OBDJsonReadInt(AObject, 'staleAfterMs', I) then
    FChannel.StaleAfterMs := Cardinal(System.Math.Max(0, I));
  if AObject = nil then
    Exit;
  AV := AObject.Values['alerts'];
  if not(AV is TJSONObject) then
    Exit;
  A := TJSONObject(AV);
  K := [];
  D := 0;
  if OBDJsonReadFloat(A, 'lowAlarm', D) then
  begin
    Include(K, alkLowAlarm);
    FAlerts.LowAlarm := D;
  end;
  if OBDJsonReadFloat(A, 'lowWarning', D) then
  begin
    Include(K, alkLowWarning);
    FAlerts.LowWarning := D;
  end;
  if OBDJsonReadFloat(A, 'highWarning', D) then
  begin
    Include(K, alkHighWarning);
    FAlerts.HighWarning := D;
  end;
  if OBDJsonReadFloat(A, 'highAlarm', D) then
  begin
    Include(K, alkHighAlarm);
    FAlerts.HighAlarm := D;
  end;
  FAlerts.Kinds := K;
end;

end.
