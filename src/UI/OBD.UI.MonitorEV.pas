//------------------------------------------------------------------------------
//  OBD.UI.MonitorEV
//
//  Emissions / EV diagnostic visuals for the A2 inventory:
//
//    TOBDReadinessGrid      17-row OS-native list view bound
//                           to a TOBDMonitorReadiness array.
//                           Host calls Refresh from their
//                           OnReadiness handler.
//    TOBDDriveCycleProgress current-step description + ETA +
//                           progress bar. Host calls
//                           UpdateProgress from OnStep.
//    TOBDCellVoltageHeatmap colour-coded EV cell grid. Cold
//                           (blue) = high voltage, hot (red) =
//                           low voltage. Tooltip-friendly hit
//                           testing via CellAt(X, Y).
//    TOBDChargingFlow       animated charger → pack → motor
//                           power-flow diagram. Direction
//                           inferred from signed power; flow
//                           particles slide along the arrows.
//
//  All four inherit theme / HiDPI / VCL-Style awareness from
//  TOBDCustomControl or TListView, route state mutations
//  through TBindings.Notify, and guard timers + subscriptions
//  with csDesigning so the IDE Designer stays responsive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.MonitorEV;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.Service.DriveCycle.Types,
  OBD.Service.DriveCycle,
  OBD.Service.EVBattery.Types,
  OBD.Service.EVBattery;

type
  /// <summary>OS-native readiness grid: one row per monitor,
  /// columns for Supported / Complete state. Host calls
  /// <see cref="Refresh"/> with the latest snapshot from
  /// <c>TOBDDriveCycleAdvisor.OnReadiness</c>; unmentioned
  /// monitors render with the "not reported" placeholder.
  /// </summary>
  TOBDReadinessGrid = class(TListView)
  strict private
    FAdvisor: TOBDDriveCycleAdvisor;
    procedure SetAdvisor(AValue: TOBDDriveCycleAdvisor);
    procedure NotifyBindings;
    procedure EnsureRows;
  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-renders every row from
    /// <paramref name="AReadiness"/>. Missing monitors fall
    /// back to "not reported".</summary>
    procedure Refresh(
      const AReadiness: TArray<TOBDMonitorReadiness>);
    /// <summary>Convenience: pulls the latest from the bound
    /// advisor via a synchronous <c>ReadReadiness</c>. Raises
    /// when <c>Advisor</c> is nil or its <c>Protocol</c> is
    /// not wired.</summary>
    procedure RefreshFromAdvisor;
  published
    /// <summary>Optional bound advisor. Used by
    /// <see cref="RefreshFromAdvisor"/>; nil = host calls
    /// <see cref="Refresh"/> directly.</summary>
    property Advisor: TOBDDriveCycleAdvisor
      read FAdvisor write SetAdvisor;

    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>Drive-cycle progress strip. Big "Step N of M"
  /// caption, free-text instruction, elapsed / total bar.
  /// Host wires from <c>TOBDDriveCycleAdvisor.OnStep</c>.
  /// </summary>
  TOBDDriveCycleProgress = class(TOBDCustomControl)
  strict private
    FStep:        TOBDDriveCycleStep;
    FElapsedSec:  Cardinal;
    FTotalSec:    Cardinal;
    FStepIndex:   Integer;
    FStepCount:   Integer;
    FCaptionFont: TFont;
    FBodyFont:    TFont;
    procedure SetCaptionFont(AValue: TFont);
    procedure SetBodyFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  FormatSecs(ASec: Cardinal): string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Updates the displayed step + progress. Pass
    /// <paramref name="ATotalSteps"/> = 0 to leave the step
    /// counter at its previous value (useful when the host
    /// only knows the current step).</summary>
    procedure UpdateProgress(const AStep: TOBDDriveCycleStep;
      AElapsedSec, ATotalSec: Cardinal;
      ATotalSteps: Integer = 0);
    /// <summary>Currently-shown step record.</summary>
    property Step: TOBDDriveCycleStep read FStep;
    property ElapsedSec: Cardinal read FElapsedSec;
    property TotalSec:   Cardinal read FTotalSec;
  published
    /// <summary>Caption / "Step N of M" font.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Body / instruction font.</summary>
    property BodyFont: TFont read FBodyFont write SetBodyFont;
  end;

  /// <summary>EV cell-voltage heatmap. Set
  /// <see cref="Voltages"/> + <see cref="Columns"/>; the grid
  /// auto-laysout in <c>Columns</c>-wide rows. Cell colour
  /// scales from <see cref="MinVoltage"/> (red, low) to
  /// <see cref="MaxVoltage"/> (green, healthy). Voltages
  /// outside the range clamp to the edge colour.</summary>
  TOBDCellVoltageHeatmap = class(TOBDCustomControl)
  strict private
    FVoltages:   TArray<Single>;
    FColumns:    Integer;
    FMinVoltage: Single;
    FMaxVoltage: Single;
    FShowText:   Boolean;
    FCellFont:   TFont;
    procedure SetColumns(AValue: Integer);
    procedure SetMinVoltage(AValue: Single);
    procedure SetMaxVoltage(AValue: Single);
    procedure SetShowText(AValue: Boolean);
    procedure SetCellFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  CellColor(AVoltage: Single): TColor;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Replaces every cell. Pass an empty array to
    /// clear.</summary>
    procedure SetVoltages(const AValues: TArray<Single>);
    /// <summary>Updates one cell. Out-of-range indexes
    /// are ignored.</summary>
    procedure SetCell(AIndex: Integer; AVoltage: Single);
    /// <summary>Returns the cell index at the supplied client
    /// coordinate, or -1 when the point isn't on a cell.
    /// </summary>
    function  CellAt(X, Y: Integer): Integer;
    /// <summary>Snapshot of the stored voltages.</summary>
    function  Voltages: TArray<Single>;
    /// <summary>Cell count.</summary>
    function  CellCount: Integer;
  published
    /// <summary>Cells per row. Default 12.</summary>
    property Columns: Integer
      read FColumns write SetColumns default 12;
    /// <summary>Bottom of the colour scale (volts).
    /// Default 3.0.</summary>
    property MinVoltage: Single
      read FMinVoltage write SetMinVoltage;
    /// <summary>Top of the colour scale (volts).
    /// Default 4.2.</summary>
    property MaxVoltage: Single
      read FMaxVoltage write SetMaxVoltage;
    /// <summary>Print each cell's voltage inside its tile.
    /// Default False (clutter-free heatmap look).</summary>
    property ShowText: Boolean
      read FShowText write SetShowText default False;
    /// <summary>Cell-text font when <see cref="ShowText"/> is
    /// True.</summary>
    property CellFont: TFont read FCellFont write SetCellFont;
  end;

  /// <summary>Power flow direction.</summary>
  TOBDFlowDirection = (
    /// <summary>No flow.</summary>
    fdIdle,
    /// <summary>Charger → pack (external AC/DC charge).
    /// </summary>
    fdCharging,
    /// <summary>Pack → motor (drive).</summary>
    fdDriving,
    /// <summary>Motor → pack (regenerative braking).</summary>
    fdRegen
  );

  /// <summary>Animated charger ↔ pack ↔ motor power-flow
  /// diagram. Host sets <see cref="ChargePowerKw"/> (charger →
  /// pack, positive while plugged in) and
  /// <see cref="DrivePowerKw"/> (pack → motor; positive while
  /// drawing, negative while regenerating). The diagram
  /// animates particles along the active arrows.</summary>
  TOBDChargingFlow = class(TOBDCustomControl)
  strict private
    FChargeKw:    Double;
    FDriveKw:     Double;
    FAnimPhase:   Single;
    FTimer:       TTimer;
    FBodyFont:    TFont;
    procedure SetChargeKw(AValue: Double);
    procedure SetDriveKw(AValue: Double);
    procedure SetBodyFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandleTick(Sender: TObject);
    procedure NotifyBindings;
    function  Direction: TOBDFlowDirection;
    procedure PaintBlock(ACanvas: TCanvas; AGraphics: TGPGraphics;
      const ARect: TGPRectF; const ALabel, AValue: string;
      AAccent: TColor);
    procedure PaintArrow(AGraphics: TGPGraphics;
      AX1, AY, AX2: Single; AActive: Boolean;
      AReverse: Boolean; AColor: TColor);
  protected
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Charger → pack power (kW). Positive while
    /// plugged in and charging; zero when unplugged.</summary>
    property ChargePowerKw: Double
      read FChargeKw write SetChargeKw;
    /// <summary>Pack ↔ motor power (kW). Positive = motor is
    /// drawing, negative = regenerating.</summary>
    property DrivePowerKw: Double
      read FDriveKw write SetDriveKw;
    /// <summary>Label font.</summary>
    property BodyFont: TFont read FBodyFont write SetBodyFont;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

function MonitorDisplay(AMonitor: TOBDMonitor): string;
begin
  case AMonitor of
    omMisfire:             Result := 'Misfire';
    omFuelSystem:          Result := 'Fuel system';
    omComprehensive:       Result := 'Comprehensive components';
    omCatalyst:            Result := 'Catalyst';
    omHeatedCatalyst:      Result := 'Heated catalyst';
    omEvaporativeSystem:   Result := 'Evaporative system';
    omSecondaryAirSystem:  Result := 'Secondary air system';
    omACRefrigerant:       Result := 'A/C refrigerant';
    omOxygenSensor:        Result := 'Oxygen sensor';
    omOxygenSensorHeater:  Result := 'Oxygen sensor heater';
    omEGRSystem:           Result := 'EGR system';
    omNMHCCatalyst:        Result := 'NMHC catalyst';
    omNOxAftertreatment:   Result := 'NOx aftertreatment';
    omBoostPressureSystem: Result := 'Boost pressure';
    omExhaustGasSensor:    Result := 'Exhaust gas sensor';
    omPMFilter:            Result := 'Particulate filter';
    omEGRorVVTSystem:      Result := 'EGR / VVT';
  else
    Result := 'Unknown';
  end;
end;

procedure ConfigureColumns(AListView: TListView;
  const ANames: array of string;
  const AWidths: array of Integer);
var
  I: Integer;
  Col: TListColumn;
begin
  AListView.Columns.BeginUpdate;
  try
    AListView.Columns.Clear;
    for I := 0 to High(ANames) do
    begin
      Col := AListView.Columns.Add;
      Col.Caption := ANames[I];
      Col.Width   := AWidths[I];
    end;
  finally
    AListView.Columns.EndUpdate;
  end;
end;

{ ---- TOBDReadinessGrid --------------------------------------------------- }

constructor TOBDReadinessGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle  := vsReport;
  RowSelect  := True;
  ReadOnly   := True;
  GridLines  := True;
  HideSelection := False;
  ShowColumnHeaders := True;
end;

procedure TOBDReadinessGrid.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureColumns(Self,
      ['Monitor', 'Supported', 'Complete'],
      [220, 90, 90]);
  EnsureRows;
end;

procedure TOBDReadinessGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAdvisor) then
    FAdvisor := nil;
end;

procedure TOBDReadinessGrid.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDReadinessGrid.SetAdvisor(AValue: TOBDDriveCycleAdvisor);
begin
  if FAdvisor = AValue then Exit;
  if FAdvisor <> nil then FAdvisor.RemoveFreeNotification(Self);
  FAdvisor := AValue;
  if FAdvisor <> nil then FAdvisor.FreeNotification(Self);
end;

procedure TOBDReadinessGrid.EnsureRows;
var
  M: TOBDMonitor;
  Item: TListItem;
begin
  if Items.Count = Ord(High(TOBDMonitor)) + 1 then Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    for M := Low(TOBDMonitor) to High(TOBDMonitor) do
    begin
      Item := Items.Add;
      Item.Caption := MonitorDisplay(M);
      Item.SubItems.Add('-');
      Item.SubItems.Add('-');
      Item.Data := Pointer(NativeInt(Ord(M)));
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDReadinessGrid.Refresh(
  const AReadiness: TArray<TOBDMonitorReadiness>);
var
  R: TOBDMonitorReadiness;
  I: Integer;
  Item: TListItem;
begin
  EnsureRows;
  Items.BeginUpdate;
  try
    // Reset every row first so a monitor that disappears from
    // the snapshot doesn't carry stale state forward.
    for I := 0 to Items.Count - 1 do
    begin
      Items[I].SubItems[0] := '-';
      Items[I].SubItems[1] := '-';
    end;
    for R in AReadiness do
    begin
      Item := nil;
      for I := 0 to Items.Count - 1 do
        if NativeInt(Items[I].Data) = Ord(R.Monitor) then
        begin
          Item := Items[I];
          Break;
        end;
      if Item = nil then Continue;
      if R.Supported then Item.SubItems[0] := 'yes'
      else                Item.SubItems[0] := 'no';
      if not R.Supported then
        Item.SubItems[1] := '-'
      else if R.Complete then
        Item.SubItems[1] := 'complete'
      else
        Item.SubItems[1] := 'incomplete';
    end;
  finally
    Items.EndUpdate;
  end;
  NotifyBindings;
end;

procedure TOBDReadinessGrid.RefreshFromAdvisor;
begin
  if FAdvisor = nil then
    raise EInvalidOperation.Create(
      'TOBDReadinessGrid.RefreshFromAdvisor: Advisor not assigned');
  Refresh(FAdvisor.ReadReadiness);
end;

{ ---- TOBDDriveCycleProgress --------------------------------------------- }

constructor TOBDDriveCycleProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 90;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [fsBold];
  FCaptionFont.OnChange := HandleFontChange;
  FBodyFont := TFont.Create;
  FBodyFont.Name := 'Segoe UI';
  FBodyFont.Size := 9;
  FBodyFont.OnChange := HandleFontChange;
end;

destructor TOBDDriveCycleProgress.Destroy;
begin
  FCaptionFont.Free;
  FBodyFont.Free;
  inherited;
end;

procedure TOBDDriveCycleProgress.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDriveCycleProgress.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDriveCycleProgress.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDDriveCycleProgress.SetBodyFont(AValue: TFont);
begin
  FBodyFont.Assign(AValue);
end;

procedure TOBDDriveCycleProgress.UpdateProgress(
  const AStep: TOBDDriveCycleStep;
  AElapsedSec, ATotalSec: Cardinal; ATotalSteps: Integer);
begin
  FStep       := AStep;
  FElapsedSec := AElapsedSec;
  FTotalSec   := ATotalSec;
  FStepIndex  := AStep.Index;
  if ATotalSteps > 0 then FStepCount := ATotalSteps;
  NotifyBindings;
  Repaint;
end;

function TOBDDriveCycleProgress.FormatSecs(ASec: Cardinal): string;
var
  Mins, Secs: Cardinal;
begin
  Mins := ASec div 60;
  Secs := ASec mod 60;
  Result := Format('%d:%2.2d', [Mins, Secs]);
end;

procedure TOBDDriveCycleProgress.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, BarH, BarY: Integer;
  Track, Fill: TGPRectF;
  HeaderText, EtaText: string;
  Frac: Single;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);

  // Header: "Step N of M  -  description".
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveAccent;
  if FStepCount > 0 then
    HeaderText := Format('Step %d of %d',
      [System.Math.Max(1, FStepIndex), FStepCount])
  else
    HeaderText := Format('Step %d',
      [System.Math.Max(1, FStepIndex)]);
  ACanvas.TextOut(Pad, Pad, HeaderText);

  // Body: free-text instruction.
  ACanvas.Font := FBodyFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FStep.Description <> '' then
    ACanvas.TextOut(Pad,
      Pad + ACanvas.TextHeight('Mg') * 2 - ScaleValue(2),
      FStep.Description);

  // Progress bar at the bottom.
  BarH := ScaleValue(10);
  BarY := Height - Pad - BarH;
  Track.X := Pad;
  Track.Y := BarY;
  Track.Width  := Width - 2 * Pad - ScaleValue(80);
  Track.Height := BarH;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;

    if FTotalSec > 0 then
    begin
      Frac := System.Math.Min(FElapsedSec / FTotalSec, 1.0);
      Fill := Track;
      Fill.Width := Single(Frac) * Track.Width;
      Brush := TGPSolidBrush.Create(ColorToARGB(EffectiveAccent));
      try
        Graphics.FillRectangle(Brush, Fill);
      finally
        Brush.Free;
      end;
    end;

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;

  // ETA readout on the right.
  ACanvas.Font := FBodyFont;
  ACanvas.Font.Color := EffectiveForeground;
  EtaText := Format('%s / %s',
    [FormatSecs(FElapsedSec), FormatSecs(FTotalSec)]);
  ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(EtaText),
    BarY - ScaleValue(2), EtaText);
end;

{ ---- TOBDCellVoltageHeatmap --------------------------------------------- }

constructor TOBDCellVoltageHeatmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 200;
  FColumns := 12;
  FMinVoltage := 3.0;
  FMaxVoltage := 4.2;
  FShowText := False;
  FCellFont := TFont.Create;
  FCellFont.Name := 'Segoe UI';
  FCellFont.Size := 7;
  FCellFont.OnChange := HandleFontChange;
end;

destructor TOBDCellVoltageHeatmap.Destroy;
begin
  FCellFont.Free;
  inherited;
end;

procedure TOBDCellVoltageHeatmap.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDCellVoltageHeatmap.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetColumns(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 256 then AValue := 256;
  if FColumns = AValue then Exit;
  FColumns := AValue; Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetMinVoltage(AValue: Single);
begin
  if SameValue(FMinVoltage, AValue) then Exit;
  FMinVoltage := AValue; Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetMaxVoltage(AValue: Single);
begin
  if SameValue(FMaxVoltage, AValue) then Exit;
  FMaxVoltage := AValue; Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetShowText(AValue: Boolean);
begin
  if FShowText = AValue then Exit;
  FShowText := AValue; Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetCellFont(AValue: TFont);
begin
  FCellFont.Assign(AValue);
end;

procedure TOBDCellVoltageHeatmap.SetVoltages(
  const AValues: TArray<Single>);
begin
  FVoltages := Copy(AValues);
  NotifyBindings;
  Repaint;
end;

procedure TOBDCellVoltageHeatmap.SetCell(AIndex: Integer;
  AVoltage: Single);
begin
  if (AIndex < 0) or (AIndex >= Length(FVoltages)) then Exit;
  if SameValue(FVoltages[AIndex], AVoltage) then Exit;
  FVoltages[AIndex] := AVoltage;
  NotifyBindings;
  Repaint;
end;

function TOBDCellVoltageHeatmap.Voltages: TArray<Single>;
begin
  Result := Copy(FVoltages);
end;

function TOBDCellVoltageHeatmap.CellCount: Integer;
begin
  Result := Length(FVoltages);
end;

function TOBDCellVoltageHeatmap.CellColor(AVoltage: Single): TColor;
var
  T:        Single;
  Span:     Single;
  R, G, B:  Byte;
begin
  Span := FMaxVoltage - FMinVoltage;
  if Span <= 0 then
    T := 0.5
  else
    T := (AVoltage - FMinVoltage) / Span;
  if T < 0 then T := 0;
  if T > 1 then T := 1;
  // Red (low) → amber → green (high). Three-stop gradient
  // gives a clean "low / medium / healthy" read.
  if T < 0.5 then
  begin
    R := 220;
    G := Round(50 + (T * 2.0) * (170 - 50));   // 50..170
    B := 60;
  end
  else
  begin
    R := Round(220 - ((T - 0.5) * 2.0) * (220 - 60));
    G := 180;
    B := 70;
  end;
  Result := RGB(R, G, B);
end;

function TOBDCellVoltageHeatmap.CellAt(X, Y: Integer): Integer;
var
  Pad, RowCount, CellW, CellH: Integer;
  Col, Row: Integer;
begin
  Result := -1;
  if Length(FVoltages) = 0 then Exit;
  Pad := ScaleValue(4);
  if (X < Pad) or (Y < Pad) then Exit;
  RowCount := (Length(FVoltages) + FColumns - 1) div FColumns;
  if RowCount = 0 then Exit;
  CellW := (Width  - 2 * Pad) div FColumns;
  CellH := (Height - 2 * Pad) div RowCount;
  if (CellW <= 0) or (CellH <= 0) then Exit;
  Col := (X - Pad) div CellW;
  Row := (Y - Pad) div CellH;
  if (Col < 0) or (Col >= FColumns) then Exit;
  if (Row < 0) or (Row >= RowCount) then Exit;
  Result := Row * FColumns + Col;
  if Result >= Length(FVoltages) then Result := -1;
end;

procedure TOBDCellVoltageHeatmap.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, RowCount, CellW, CellH, I, Col, Row: Integer;
  Cell: TGPRectF;
  V: Single;
  Lab: string;
  TextW, TextH: Integer;
begin
  if Length(FVoltages) = 0 then Exit;
  Pad := ScaleValue(4);
  RowCount := (Length(FVoltages) + FColumns - 1) div FColumns;
  if RowCount = 0 then Exit;
  CellW := (Width  - 2 * Pad) div FColumns;
  CellH := (Height - 2 * Pad) div RowCount;
  if (CellW <= 0) or (CellH <= 0) then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to Length(FVoltages) - 1 do
    begin
      Row := I div FColumns;
      Col := I mod FColumns;
      Cell.X := Pad + Col * CellW;
      Cell.Y := Pad + Row * CellH;
      Cell.Width  := CellW - 1;
      Cell.Height := CellH - 1;
      V := FVoltages[I];
      Brush := TGPSolidBrush.Create(ColorToARGB(CellColor(V)));
      try
        Graphics.FillRectangle(Brush, Cell);
      finally
        Brush.Free;
      end;
      Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
      try
        Graphics.DrawRectangle(Pen, Cell);
      finally
        Pen.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;

  if FShowText then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FCellFont;
    ACanvas.Font.Color := clBlack;
    for I := 0 to Length(FVoltages) - 1 do
    begin
      Row := I div FColumns;
      Col := I mod FColumns;
      Lab := Format('%.2f', [FVoltages[I]]);
      TextW := ACanvas.TextWidth(Lab);
      TextH := ACanvas.TextHeight(Lab);
      ACanvas.TextOut(
        Pad + Col * CellW + (CellW - TextW) div 2,
        Pad + Row * CellH + (CellH - TextH) div 2,
        Lab);
    end;
  end;
end;

{ ---- TOBDChargingFlow ---------------------------------------------------- }

constructor TOBDChargingFlow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 140;
  FChargeKw := 0;
  FDriveKw  := 0;
  FAnimPhase := 0;
  FBodyFont := TFont.Create;
  FBodyFont.Name := 'Segoe UI';
  FBodyFont.Size := 9;
  FBodyFont.OnChange := HandleFontChange;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled  := False;
  FTimer.Interval := 60;
  FTimer.OnTimer  := HandleTick;
end;

destructor TOBDChargingFlow.Destroy;
begin
  FTimer.Free;
  FBodyFont.Free;
  inherited;
end;

procedure TOBDChargingFlow.Loaded;
begin
  inherited;
  FTimer.Enabled := not (csDesigning in ComponentState);
end;

procedure TOBDChargingFlow.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDChargingFlow.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDChargingFlow.HandleTick(Sender: TObject);
begin
  FAnimPhase := FAnimPhase + 0.06;
  if FAnimPhase > 1 then FAnimPhase := FAnimPhase - 1;
  if Direction <> fdIdle then Repaint;
end;

procedure TOBDChargingFlow.SetChargeKw(AValue: Double);
begin
  if SameValue(FChargeKw, AValue) then Exit;
  FChargeKw := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDChargingFlow.SetDriveKw(AValue: Double);
begin
  if SameValue(FDriveKw, AValue) then Exit;
  FDriveKw := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDChargingFlow.SetBodyFont(AValue: TFont);
begin
  FBodyFont.Assign(AValue);
end;

function TOBDChargingFlow.Direction: TOBDFlowDirection;
begin
  if FChargeKw > 0.1 then        Result := fdCharging
  else if FDriveKw < -0.1 then   Result := fdRegen
  else if FDriveKw >  0.1 then   Result := fdDriving
  else                           Result := fdIdle;
end;

procedure TOBDChargingFlow.PaintBlock(ACanvas: TCanvas;
  AGraphics: TGPGraphics; const ARect: TGPRectF;
  const ALabel, AValue: string; AAccent: TColor);
var
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
begin
  Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
  try
    AGraphics.FillRectangle(Brush, ARect);
  finally
    Brush.Free;
  end;
  Pen := TGPPen.Create(ColorToARGB(AAccent), ScaleValue(2));
  try
    AGraphics.DrawRectangle(Pen, ARect);
  finally
    Pen.Free;
  end;
  // Labels via the buffer canvas — sharper than GDI+ text.
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FBodyFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(
    Round(ARect.X + (ARect.Width - ACanvas.TextWidth(ALabel)) / 2),
    Round(ARect.Y + ScaleValue(4)),
    ALabel);
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := AAccent;
  ACanvas.TextOut(
    Round(ARect.X + (ARect.Width - ACanvas.TextWidth(AValue)) / 2),
    Round(ARect.Y + ARect.Height -
      ACanvas.TextHeight(AValue) - ScaleValue(4)),
    AValue);
  ACanvas.Font.Style := [];
end;

procedure TOBDChargingFlow.PaintArrow(AGraphics: TGPGraphics;
  AX1, AY, AX2: Single; AActive: Boolean; AReverse: Boolean;
  AColor: TColor);
var
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  HeadX, HeadY, P: Single;
  Pts: array[0..2] of TGPPointF;
  ParticleX: Single;
  ParticleR: Single;
  Dir: Integer;
begin
  if AActive then
    Pen := TGPPen.Create(ColorToARGB(AColor), ScaleValue(3))
  else
    Pen := TGPPen.Create(ColorToARGB(Palette.NeutralLight),
      ScaleValue(2));
  try
    AGraphics.DrawLine(Pen, AX1, AY, AX2, AY);
  finally
    Pen.Free;
  end;

  // Arrowhead.
  if AReverse then Dir := -1 else Dir := 1;
  if AReverse then HeadX := AX1 else HeadX := AX2;
  HeadY := AY;
  Pts[0].X := HeadX;
  Pts[0].Y := HeadY;
  Pts[1].X := HeadX - Dir * ScaleValue(10);
  Pts[1].Y := HeadY - ScaleValue(5);
  Pts[2].X := HeadX - Dir * ScaleValue(10);
  Pts[2].Y := HeadY + ScaleValue(5);
  if AActive then
    Brush := TGPSolidBrush.Create(ColorToARGB(AColor))
  else
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
  try
    AGraphics.FillPolygon(Brush, PGPPointF(@Pts[0]), 3);
  finally
    Brush.Free;
  end;

  if not AActive then Exit;

  // Flow particle — slides along the arrow.
  P := FAnimPhase;
  ParticleR := ScaleValue(4);
  if AReverse then
    ParticleX := AX2 - P * (AX2 - AX1)
  else
    ParticleX := AX1 + P * (AX2 - AX1);
  Brush := TGPSolidBrush.Create(ColorToARGB(AColor));
  try
    AGraphics.FillEllipse(Brush,
      ParticleX - ParticleR, AY - ParticleR,
      ParticleR * 2, ParticleR * 2);
  finally
    Brush.Free;
  end;
end;

procedure TOBDChargingFlow.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  BlockW, BlockH, GapW, BodyY: Integer;
  Charger, Pack, Motor: TGPRectF;
  Dir: TOBDFlowDirection;
  ChargerActive, DriveActive, DriveReverse: Boolean;
  DriveColor: TColor;
begin
  BlockW := Round(Width * 0.22);
  BlockH := Round(Height * 0.55);
  GapW   := (Width - 3 * BlockW) div 2;
  BodyY  := (Height - BlockH) div 2;

  Charger.X := 0;
  Charger.Y := BodyY;
  Charger.Width  := BlockW;
  Charger.Height := BlockH;

  Pack.X := BlockW + GapW;
  Pack.Y := BodyY;
  Pack.Width  := BlockW;
  Pack.Height := BlockH;

  Motor.X := 2 * (BlockW + GapW);
  Motor.Y := BodyY;
  Motor.Width  := BlockW;
  Motor.Height := BlockH;

  Dir := Direction;
  ChargerActive := Dir = fdCharging;
  DriveActive   := (Dir = fdDriving) or (Dir = fdRegen);
  DriveReverse  := Dir = fdRegen;
  if Dir = fdRegen then
    DriveColor := Palette.Success
  else
    DriveColor := Palette.Accent;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    PaintBlock(ACanvas, Graphics,Charger, 'Charger',
      Format('%.1f kW', [FChargeKw]),
      Palette.Accent);
    // Battery block reads the magnitude of the dominant flow
    // — gives a glanceable "how busy is the pack" value
    // without needing the host to wire a separate voltage
    // property.
    PaintBlock(ACanvas, Graphics,Pack, 'Battery',
      Format('%.1f kW',
        [System.Math.Max(Abs(FChargeKw), Abs(FDriveKw))]),
      Palette.Accent);
    PaintBlock(ACanvas, Graphics,Motor, 'Motor',
      Format('%.1f kW', [FDriveKw]),
      DriveColor);

    // Charger → Pack arrow.
    PaintArrow(Graphics,
      Charger.X + Charger.Width  + ScaleValue(2),
      Charger.Y + Charger.Height / 2,
      Pack.X - ScaleValue(2),
      ChargerActive, False, Palette.Accent);

    // Pack ↔ Motor arrow.
    PaintArrow(Graphics,
      Pack.X + Pack.Width + ScaleValue(2),
      Pack.Y + Pack.Height / 2,
      Motor.X - ScaleValue(2),
      DriveActive, DriveReverse, DriveColor);
  finally
    Graphics.Free;
  end;
end;

end.
