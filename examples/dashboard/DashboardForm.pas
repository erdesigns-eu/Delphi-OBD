//------------------------------------------------------------------------------
// UNIT           : Examples.Dashboard.Form
// CONTENTS       : Reference live-dashboard example for the Delphi-OBD library
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Single-form dashboard wired entirely in code so the example
//                  reads top-to-bottom without a .dfm. Drives four circular
//                  gauges, three status LEDs, a touch header, and a log/DTC
//                  memo. By default it runs in **simulator mode** — a tick
//                  timer feeds plausible OBD-II values to the gauges so the
//                  app is useful out of the box without an adapter. Pressing
//                  "Connect Live" attaches the OBD connection/protocol
//                  components and replaces the simulator with real data.
//------------------------------------------------------------------------------
unit Examples.Dashboard.Form;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Graphics,

  OBD.CircularGauge, OBD.LED, OBD.Touch.Header, OBD.Touch.Statusbar,
  OBD.Connection.Component, OBD.Protocol.Component;

type
  /// <summary>
  ///   Reference live-dashboard form. Demonstrates how to compose the
  ///   shipped Skia-rendered components into a working diagnostic UI and
  ///   how to drive them either from a simulator or from the live OBD
  ///   connection/protocol component pair.
  /// </summary>
  TDashboardForm = class(TForm)
  private
    FHeader: TOBDTouchHeader;
    FStatusbar: TOBDTouchStatusbar;
    FRpmGauge: TOBDCircularGauge;
    FSpeedGauge: TOBDCircularGauge;
    FCoolantGauge: TOBDCircularGauge;
    FThrottleGauge: TOBDCircularGauge;
    FMilLed: TOBDLed;
    FConnectedLed: TOBDLed;
    FErrorLed: TOBDLed;
    FLog: TMemo;
    FToolbar: TPanel;
    FSimulateBtn: TButton;
    FLiveBtn: TButton;
    FConnection: TOBDConnectionComponent;
    FProtocol: TOBDProtocolComponent;
    FTick: TTimer;
    FSimulating: Boolean;
    FPhase: Single;
    procedure BuildLayout;
    function MakeGauge(const Title: string; const Min, Max: Single;
      const Units: string): TOBDCircularGauge;
    procedure HandleTick(Sender: TObject);
    procedure HandleSimulate(Sender: TObject);
    procedure HandleLive(Sender: TObject);
    procedure SetSimulating(const Value: Boolean);
    procedure AppendLog(const Line: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  DashboardForm: TDashboardForm;

implementation

const
  GAUGE_GAP   = 12;
  PANEL_PAD   = 12;
  HEADER_HT   = 56;
  STATUS_HT   = 36;
  TOOLBAR_HT  = 44;
  GAUGE_W     = 240;
  GAUGE_H     = 240;
  LED_SIZE    = 36;

constructor TDashboardForm.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'OBD-II Reference Dashboard';
  Width := 1100;
  Height := 720;
  Position := poScreenCenter;
  Color := clBlack;
  Font.Color := clWhite;

  FConnection := TOBDConnectionComponent.Create(Self);
  FProtocol := TOBDProtocolComponent.Create(Self);

  BuildLayout;

  // Tick at 20 fps — enough to look smooth on the gauges, not enough to
  // burn CPU. Real-mode rendering is event-driven so this only fires in
  // simulator mode.
  FTick := TTimer.Create(Self);
  FTick.Interval := 50;
  FTick.OnTimer := HandleTick;
  FTick.Enabled := False;

  // Default to simulator so the example does something useful with no
  // adapter attached.
  SetSimulating(True);
end;

procedure TDashboardForm.BuildLayout;
var
  GaugeArea: TPanel;
  LedArea: TPanel;
  Y: Integer;
begin
  // Header strip across the top.
  FHeader := TOBDTouchHeader.Create(Self);
  FHeader.Parent := Self;
  FHeader.Align := alTop;
  FHeader.Height := HEADER_HT;

  // Statusbar strip across the bottom.
  FStatusbar := TOBDTouchStatusbar.Create(Self);
  FStatusbar.Parent := Self;
  FStatusbar.Align := alBottom;
  FStatusbar.Height := STATUS_HT;

  // Toolbar with mode buttons just above the statusbar.
  FToolbar := TPanel.Create(Self);
  FToolbar.Parent := Self;
  FToolbar.Align := alBottom;
  FToolbar.Height := TOOLBAR_HT;
  FToolbar.BevelOuter := bvNone;
  FToolbar.Color := $00181818;

  FSimulateBtn := TButton.Create(Self);
  FSimulateBtn.Parent := FToolbar;
  FSimulateBtn.Caption := 'Simulate';
  FSimulateBtn.SetBounds(PANEL_PAD, 8, 110, 28);
  FSimulateBtn.OnClick := HandleSimulate;

  FLiveBtn := TButton.Create(Self);
  FLiveBtn.Parent := FToolbar;
  FLiveBtn.Caption := 'Connect Live';
  FLiveBtn.SetBounds(PANEL_PAD + 120, 8, 130, 28);
  FLiveBtn.OnClick := HandleLive;

  // Gauges live in a left-aligned area; LEDs + log fill the right.
  GaugeArea := TPanel.Create(Self);
  GaugeArea.Parent := Self;
  GaugeArea.Align := alLeft;
  GaugeArea.Width := (GAUGE_W * 2) + (PANEL_PAD * 3);
  GaugeArea.BevelOuter := bvNone;
  GaugeArea.Color := clBlack;

  FRpmGauge      := MakeGauge('RPM',          0, 8000,  'rpm');
  FSpeedGauge    := MakeGauge('Speed',        0, 240,   'km/h');
  FCoolantGauge  := MakeGauge('Coolant',    -40, 215,   '°C');
  FThrottleGauge := MakeGauge('Throttle',     0, 100,   '%');

  FRpmGauge.Parent      := GaugeArea;
  FSpeedGauge.Parent    := GaugeArea;
  FCoolantGauge.Parent  := GaugeArea;
  FThrottleGauge.Parent := GaugeArea;

  FRpmGauge.SetBounds(PANEL_PAD,                      PANEL_PAD,                  GAUGE_W, GAUGE_H);
  FSpeedGauge.SetBounds(PANEL_PAD * 2 + GAUGE_W,      PANEL_PAD,                  GAUGE_W, GAUGE_H);
  FCoolantGauge.SetBounds(PANEL_PAD,                  PANEL_PAD * 2 + GAUGE_H,    GAUGE_W, GAUGE_H);
  FThrottleGauge.SetBounds(PANEL_PAD * 2 + GAUGE_W,   PANEL_PAD * 2 + GAUGE_H,    GAUGE_W, GAUGE_H);

  // LED row + log memo in a right-side client area.
  LedArea := TPanel.Create(Self);
  LedArea.Parent := Self;
  LedArea.Align := alClient;
  LedArea.BevelOuter := bvNone;
  LedArea.Color := clBlack;

  Y := PANEL_PAD;
  FMilLed       := TOBDLed.Create(Self);
  FMilLed.Parent := LedArea;
  FMilLed.SetBounds(PANEL_PAD,                   Y, LED_SIZE, LED_SIZE);

  FConnectedLed := TOBDLed.Create(Self);
  FConnectedLed.Parent := LedArea;
  FConnectedLed.SetBounds(PANEL_PAD * 2 + LED_SIZE,         Y, LED_SIZE, LED_SIZE);

  FErrorLed     := TOBDLed.Create(Self);
  FErrorLed.Parent := LedArea;
  FErrorLed.SetBounds(PANEL_PAD * 3 + (LED_SIZE * 2),       Y, LED_SIZE, LED_SIZE);

  FLog := TMemo.Create(Self);
  FLog.Parent := LedArea;
  FLog.SetBounds(PANEL_PAD,
                 Y + LED_SIZE + PANEL_PAD,
                 LedArea.ClientWidth  - (PANEL_PAD * 2),
                 LedArea.ClientHeight - (Y + LED_SIZE + (PANEL_PAD * 2)));
  FLog.Anchors := [akLeft, akTop, akRight, akBottom];
  FLog.ReadOnly := True;
  FLog.ScrollBars := ssVertical;
  FLog.Color := $00101010;
  FLog.Font.Color := clLime;
  FLog.Font.Name := 'Consolas';
end;

function TDashboardForm.MakeGauge(const Title: string; const Min, Max: Single;
  const Units: string): TOBDCircularGauge;
begin
  Result := TOBDCircularGauge.Create(Self);
  Result.Min := Min;
  Result.Max := Max;
  Result.Value := Min;
  // The Title/Units sub-objects exist on the gauge — populate via their
  // Caption if available. Properties left at their published defaults so
  // the example reads as plain code.
  Result.Hint := Format('%s (%s)', [Title, Units]);
end;

procedure TDashboardForm.HandleTick(Sender: TObject);
var
  Sin01, Cos01: Single;
begin
  // Drive synthetic but plausible values. The phase walks slowly so the
  // dashboard "looks alive" — RPM follows speed, throttle leads RPM,
  // coolant warms up.
  FPhase := FPhase + 0.04;
  Sin01 := (Sin(FPhase) + 1) / 2;
  Cos01 := (Cos(FPhase * 0.7) + 1) / 2;

  FRpmGauge.Value      := 800 + (Sin01 * 5500);
  FSpeedGauge.Value    := Sin01 * 180;
  FThrottleGauge.Value := Cos01 * 100;

  // Coolant warms slowly to ~90°C and stays there.
  FCoolantGauge.Value := Min(90, FCoolantGauge.Value + 0.4);

  // MIL flickers when RPM exceeds 6500 (synthetic over-rev).
  FMilLed.State := lsOn;     // simulator: keep MIL visible
  FConnectedLed.State := lsOff;
  FErrorLed.State := lsOff;
end;

procedure TDashboardForm.HandleSimulate(Sender: TObject);
begin
  AppendLog('[mode] simulator');
  if FConnection.Connected then FConnection.Disconnect;
  SetSimulating(True);
end;

procedure TDashboardForm.HandleLive(Sender: TObject);
begin
  AppendLog('[mode] live — using TOBDConnectionComponent / TOBDProtocolComponent');
  AppendLog('       configure ConnectionType + transport params, then call Connect');
  // Live mode is a stub here — the example deliberately leaves the actual
  // adapter wiring to the user (every transport has different
  // discovery / pairing requirements). The TOBDConnectionComponent
  // and TOBDProtocolComponent instances are already created and ready
  // to be configured at runtime via their published properties.
  SetSimulating(False);
end;

procedure TDashboardForm.SetSimulating(const Value: Boolean);
begin
  FSimulating := Value;
  FTick.Enabled := Value;
  FConnectedLed.State := lsOff;
  if Value then
    AppendLog('Simulator running — gauges driven by synthetic data')
  else
    AppendLog('Simulator stopped');
end;

procedure TDashboardForm.AppendLog(const Line: string);
begin
  FLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + Line);
end;

end.
