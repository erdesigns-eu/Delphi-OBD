//------------------------------------------------------------------------------
// UNIT           : Examples.Mobile.Dashboard
// CONTENTS       : FMX-based mobile dashboard exercising every v3.1 FMX
//                  component on a single form.
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Single-form layout built entirely in code so the
//                  example reads top-to-bottom without a .fmx blob.
//                  Targets Win32, Win64, macOS, iOS, Android — anywhere
//                  Skia.FMX runs. The simulator timer drives every
//                  component with synthetic data so the form is useful
//                  out of the box without an OBD adapter attached.
//------------------------------------------------------------------------------
unit Examples.Mobile.Dashboard;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Graphics,
  FMX.StdCtrls, FMX.Memo, FMX.Skia,

  OBD.LinearGauge.FMX,
  OBD.Tachometer.FMX,
  OBD.TrendGraph.FMX,
  OBD.DtcList.FMX,
  OBD.Terminal.FMX,
  OBD.Knob.FMX,
  OBD.SegmentedSwitch.FMX,
  OBD.LED.FMX,
  OBD.Render.DtcList,    // gives access to dsCritical / dsWarning constants
  OBD.Render.Terminal,
  OBD.Render.LED;        // lsOn / lsOff / lsGrayed

type
  TMobileDashboardForm = class(TForm)
  private
    FTach: TOBDTachometerFMX;
    FFuel: TOBDLinearGaugeFMX;
    FCoolant: TOBDLinearGaugeFMX;
    FBoost: TOBDLinearGaugeFMX;
    FTrend: TOBDTrendGraphFMX;
    FDtcs: TOBDDtcListFMX;
    FTerminal: TOBDTerminalFMX;
    FMilLed, FConnectedLed: TOBDLedFMX;
    FMode: TOBDSegmentedSwitchFMX;
    FShiftPointKnob: TOBDKnobFMX;

    FRpmSeries, FSpeedSeries: Integer;

    FTick: TTimer;
    FPhase: Single;
    procedure BuildLayout;
    procedure HandleTick(Sender: TObject);
    procedure HandleModeChange(Sender: TObject; const Index: Integer);
    procedure HandleShiftPointChange(Sender: TObject; const Value: Single);
    procedure HandleDtcDoubleClick(Sender: TObject; Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MobileDashboardForm: TMobileDashboardForm;

implementation

constructor TMobileDashboardForm.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'OBD-II Mobile Dashboard';
  Width := 720;
  Height := 1024;
  Fill.Color := TAlphaColors.Black;

  BuildLayout;

  // 30 fps simulator. The same form, parented to a different platform
  // service (iOS / Android / desktop), repaints at this cadence with no
  // changes to the components.
  FTick := TTimer.Create(Self);
  FTick.Interval := 33;
  FTick.OnTimer := HandleTick;
  FTick.Enabled := True;
end;

procedure TMobileDashboardForm.BuildLayout;
var
  Top: Single;
begin
  // ---- Top: tachometer (the visual anchor on a portrait form) ----
  FTach := TOBDTachometerFMX.Create(Self);
  FTach.Parent := Self;
  FTach.SetBounds(40, 40, 320, 320);

  // ---- LEDs to the right of the tach ----
  FMilLed := TOBDLedFMX.Create(Self);
  FMilLed.Parent := Self;
  FMilLed.SetBounds(400, 60, 56, 56);
  FMilLed.OnFromColor := $FFE6A500;
  FMilLed.OnToColor := $FF8B5A00;

  FConnectedLed := TOBDLedFMX.Create(Self);
  FConnectedLed.Parent := Self;
  FConnectedLed.SetBounds(470, 60, 56, 56);

  // ---- Knob: shift-light setpoint ----
  FShiftPointKnob := TOBDKnobFMX.Create(Self);
  FShiftPointKnob.Parent := Self;
  FShiftPointKnob.SetBounds(540, 40, 130, 130);
  FShiftPointKnob.Min := 4000;
  FShiftPointKnob.Max := 8000;
  FShiftPointKnob.Step := 100;
  FShiftPointKnob.Value := 6500;
  FShiftPointKnob.Caption := 'shift';
  FShiftPointKnob.OnChange := HandleShiftPointChange;

  // ---- Mode switch under the knob ----
  FMode := TOBDSegmentedSwitchFMX.Create(Self);
  FMode.Parent := Self;
  FMode.SetBounds(400, 200, 270, 36);
  FMode.Segments.Add('STREET');
  FMode.Segments.Add('TRACK');
  FMode.Segments.Add('TOW');
  FMode.SelectedIndex := 0;
  FMode.OnChange := HandleModeChange;

  Top := 380;

  // ---- Three linear gauges (fuel / coolant / boost) ----
  FFuel := TOBDLinearGaugeFMX.Create(Self);
  FFuel.Parent := Self;
  FFuel.SetBounds(40, Top, 200, 84);
  FFuel.Caption := 'Fuel';
  FFuel.Units := '%';
  FFuel.Min := 0; FFuel.Max := 100; FFuel.Value := 65;

  FCoolant := TOBDLinearGaugeFMX.Create(Self);
  FCoolant.Parent := Self;
  FCoolant.SetBounds(260, Top, 200, 84);
  FCoolant.Caption := 'Coolant';
  FCoolant.Units := 'C';
  FCoolant.Min := -40; FCoolant.Max := 215; FCoolant.Value := 90;

  FBoost := TOBDLinearGaugeFMX.Create(Self);
  FBoost.Parent := Self;
  FBoost.SetBounds(480, Top, 200, 84);
  FBoost.Caption := 'Boost';
  FBoost.Units := 'kPa';
  FBoost.Min := -100; FBoost.Max := 200; FBoost.Value := 0;

  Top := Top + 100;

  // ---- Trend graph ----
  FTrend := TOBDTrendGraphFMX.Create(Self);
  FTrend.Parent := Self;
  FTrend.SetBounds(40, Top, 640, 200);
  FTrend.MaxSamples := 200;
  FRpmSeries := FTrend.AddSeries('RPM', $FFE63333, 0, 8000);
  FSpeedSeries := FTrend.AddSeries('Speed', $FF33C033, 0, 240);

  Top := Top + 220;

  // ---- DTC list and terminal side by side ----
  FDtcs := TOBDDtcListFMX.Create(Self);
  FDtcs.Parent := Self;
  FDtcs.SetBounds(40, Top, 320, 200);
  FDtcs.AddItem('P0301', 'Cylinder 1 misfire detected', dsWarning, dsActive);
  FDtcs.AddItem('P0420', 'Catalyst efficiency below threshold', dsCritical, dsActive);
  FDtcs.AddItem('B1234', 'Body code (sample)', dsInfo, dsHistory);
  FDtcs.OnDtcDoubleClick := HandleDtcDoubleClick;

  FTerminal := TOBDTerminalFMX.Create(Self);
  FTerminal.Parent := Self;
  FTerminal.SetBounds(380, Top, 300, 200);
  FTerminal.LogInfo('Mobile dashboard ready (simulator mode)');
  FTerminal.LogSent('AT Z');
  FTerminal.LogReceived('ELM327 v1.5');
end;

procedure TMobileDashboardForm.HandleTick(Sender: TObject);
var
  Sin01, Cos01: Single;
begin
  FPhase := FPhase + 0.04;
  Sin01 := (Sin(FPhase) + 1) / 2;
  Cos01 := (Cos(FPhase * 0.7) + 1) / 2;

  FTach.Value := 800 + (Sin01 * 6500);
  FFuel.Value := 65 - (Sin01 * 0.05);   // slow drain
  FCoolant.Value := System.Math.Min(95, FCoolant.Value + 0.05);
  FBoost.Value := -50 + (Cos01 * 250);

  FTrend.PushValue(FRpmSeries, FTach.Value);
  FTrend.PushValue(FSpeedSeries, Sin01 * 240);

  if FTach.ShiftLightActive then
    FMilLed.State := lsOn
  else
    FMilLed.State := lsOff;
  FConnectedLed.State := lsOn;
end;

procedure TMobileDashboardForm.HandleModeChange(Sender: TObject;
  const Index: Integer);
begin
  case Index of
    0: FTerminal.LogInfo('Mode: STREET');
    1: FTerminal.LogInfo('Mode: TRACK (shift point lifted)');
    2: FTerminal.LogInfo('Mode: TOW');
  end;
end;

procedure TMobileDashboardForm.HandleShiftPointChange(Sender: TObject;
  const Value: Single);
begin
  FTach.ShiftPoint := Value;
  FTerminal.LogInfo(Format('Shift point: %.0f rpm', [Value]));
end;

procedure TMobileDashboardForm.HandleDtcDoubleClick(Sender: TObject;
  Index: Integer);
begin
  FTerminal.LogInfo(Format('Inspect DTC %s — double-click handler fired',
    [FDtcs.Items[Index].Code]));
end;

end.
