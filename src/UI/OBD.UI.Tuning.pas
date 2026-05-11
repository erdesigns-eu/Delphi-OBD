//------------------------------------------------------------------------------
//  OBD.UI.Tuning
//
//  Tuning / calibration visuals:
//
//    TOBDXYHeatmap     One PID vs another as a 2D colour-coded
//                      density map. Push samples in pairs; the
//                      grid auto-bins.
//    TOBDTorqueRPMMap  RPM × load → torque output map. 2D
//                      colour grid for now (3D surface a
//                      future enhancement).
//    TOBDRunRecorder   Event-windowed capture (1 s pre / 5 s
//                      post). Pairs with TOBDDragRun to capture
//                      a sliding-window of samples around the
//                      arming moment.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Tuning;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>2D density heatmap. Host pushes (X, Y) sample
  /// pairs; the visual auto-bins into a grid and shades each
  /// cell by hit count.</summary>
  TOBDXYHeatmap = class(TOBDCustomControl)
  strict private
    FBins:    array of array of Integer;
    FBinsX:   Integer;
    FBinsY:   Integer;
    FXMin:    Double;
    FXMax:    Double;
    FYMin:    Double;
    FYMax:    Double;
    FMaxHits: Integer;
    procedure ResizeBins;
    procedure SetBinsX(AValue: Integer);
    procedure SetBinsY(AValue: Integer);
    procedure SetXMin(AValue: Double);
    procedure SetXMax(AValue: Double);
    procedure SetYMin(AValue: Double);
    procedure SetYMax(AValue: Double);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Bin the (X, Y) sample into the current grid and
    /// trigger a repaint.</summary>
    procedure PushSample(AX, AY: Double);
    /// <summary>Clear all bins and reset the max-hit watermark.
    /// </summary>
    procedure Reset;
  published
    /// <summary>Horizontal bin count. Default 32.</summary>
    property BinsX: Integer
      read FBinsX write SetBinsX default 32;
    /// <summary>Vertical bin count. Default 24.</summary>
    property BinsY: Integer
      read FBinsY write SetBinsY default 24;
    /// <summary>Lower bound on the X axis (sample units).
    /// </summary>
    property XMin: Double read FXMin write SetXMin;
    /// <summary>Upper bound on the X axis (sample units).
    /// </summary>
    property XMax: Double read FXMax write SetXMax;
    /// <summary>Lower bound on the Y axis (sample units).
    /// </summary>
    property YMin: Double read FYMin write SetYMin;
    /// <summary>Upper bound on the Y axis (sample units).
    /// </summary>
    property YMax: Double read FYMax write SetYMax;
  end;

  /// <summary>Torque map cell.</summary>
  TOBDTorqueCell = record
    RPM:      Double;
    Load:     Double;
    TorqueNm: Double;
  end;

  /// <summary>RPM × load → torque output. Renders as a 2D
  /// colour-coded grid; future revisions add 3D surface.
  /// </summary>
  TOBDTorqueRPMMap = class(TOBDCustomControl)
  strict private
    FCells:    TList<TOBDTorqueCell>;
    FRPMMin, FRPMMax:   Double;
    FLoadMin, FLoadMax: Double;
    FMaxTorque: Double;
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Replace the cell list with the supplied array
    /// and repaint. Auto-updates the internal max-torque
    /// watermark used to scale colour intensity.</summary>
    procedure SetCells(const ACells: array of TOBDTorqueCell);
    /// <summary>Remove all cells and repaint.</summary>
    procedure Clear;
    /// <summary>Number of cells currently stored.</summary>
    function  CellCount: Integer;
  published
    /// <summary>Lower bound of the RPM axis.</summary>
    property RPMMin:    Double read FRPMMin    write FRPMMin;
    /// <summary>Upper bound of the RPM axis.</summary>
    property RPMMax:    Double read FRPMMax    write FRPMMax;
    /// <summary>Lower bound of the load axis (typically 0..100 %).
    /// </summary>
    property LoadMin:   Double read FLoadMin   write FLoadMin;
    /// <summary>Upper bound of the load axis (typically 0..100 %).
    /// </summary>
    property LoadMax:   Double read FLoadMax   write FLoadMax;
    /// <summary>Reference torque (Nm) used to scale cell
    /// colours. 0 = auto-derive from the loaded cells.</summary>
    property MaxTorque: Double read FMaxTorque write FMaxTorque;
  end;

  /// <summary>One sample in the rolling pre-trigger buffer.
  /// </summary>
  TOBDRunSample = record
    TimeMs: Cardinal;
    Value:  Double;
  end;

  /// <summary>Sliding-window event recorder. Maintains a
  /// rolling pre-trigger buffer (default 1 s); when
  /// <see cref="Trigger"/> fires, continues recording for the
  /// configured post-window (default 5 s) and then exposes
  /// the captured slice via <see cref="Samples"/>.</summary>
  TOBDRunRecorder = class(TOBDCustomControl)
  strict private
    FPreMs:      Cardinal;
    FPostMs:     Cardinal;
    FBuffer:     TList<TOBDRunSample>;
    FTriggered:  Boolean;
    FTriggerMs:  Cardinal;
    FFinished:   Boolean;
    procedure SetPreMs(AValue: Cardinal);
    procedure SetPostMs(AValue: Cardinal);
    procedure NotifyBindings;
    procedure TrimPreWindow(ATimeMs: Cardinal);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Pushes one (time, value) sample. Triggers the
    /// post-window timer transparently if pre-trigger has
    /// fired.</summary>
    procedure PushSample(ATimeMs: Cardinal; AValue: Double);
    /// <summary>Fires the trigger. Subsequent pushes continue
    /// until the post-window elapses.</summary>
    procedure Trigger;
    /// <summary>Discard the buffer, clear the trigger flag and
    /// repaint.</summary>
    procedure Reset;
    /// <summary>Captured slice (pre + post window). Empty
    /// until <see cref="Finished"/> goes True.</summary>
    function  Samples: TArray<TOBDRunSample>;
    /// <summary>True once <see cref="Trigger"/> has fired and
    /// the post-window is still filling.</summary>
    property Triggered: Boolean read FTriggered;
    /// <summary>True once the post-window has elapsed and
    /// <see cref="Samples"/> is final.</summary>
    property Finished:  Boolean read FFinished;
  published
    /// <summary>Pre-trigger window in milliseconds. Default
    /// 1000 ms.</summary>
    property PreMs:  Cardinal read FPreMs  write SetPreMs default 1000;
    /// <summary>Post-trigger window in milliseconds. Default
    /// 5000 ms.</summary>
    property PostMs: Cardinal read FPostMs write SetPostMs default 5000;
  end;

implementation

function HeatColor(T: Single): TColor; inline;
var
  R, G, B: Byte;
begin
  // Cold (blue) → warm (red) gradient via three stops.
  if T < 0 then T := 0;
  if T > 1 then T := 1;
  if T < 0.5 then
  begin
    R := Round(40 + (T * 2.0) * 200);
    G := Round(80 + (T * 2.0) * 120);
    B := Round(200 - (T * 2.0) * 60);
  end
  else
  begin
    R := 240;
    G := Round(200 - ((T - 0.5) * 2.0) * 160);
    B := Round(140 - ((T - 0.5) * 2.0) * 100);
  end;
  Result := RGB(R, G, B);
end;

{ ---- TOBDXYHeatmap ----------------------------------------------------- }

constructor TOBDXYHeatmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 200;
  FBinsX := 32;
  FBinsY := 24;
  FXMin  := 0; FXMax := 100;
  FYMin  := 0; FYMax := 100;
  ResizeBins;
end;

procedure TOBDXYHeatmap.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDXYHeatmap.ResizeBins;
var I: Integer;
begin
  SetLength(FBins, FBinsX);
  for I := 0 to FBinsX - 1 do
    SetLength(FBins[I], FBinsY);
  FMaxHits := 0;
end;

procedure TOBDXYHeatmap.SetBinsX(AValue: Integer);
begin
  if AValue < 4 then AValue := 4;
  if AValue > 512 then AValue := 512;
  if FBinsX = AValue then Exit;
  FBinsX := AValue; ResizeBins; Repaint;
end;

procedure TOBDXYHeatmap.SetBinsY(AValue: Integer);
begin
  if AValue < 4 then AValue := 4;
  if AValue > 512 then AValue := 512;
  if FBinsY = AValue then Exit;
  FBinsY := AValue; ResizeBins; Repaint;
end;

procedure TOBDXYHeatmap.SetXMin(AValue: Double);
begin
  if SameValue(FXMin, AValue) then Exit;
  FXMin := AValue; Repaint;
end;

procedure TOBDXYHeatmap.SetXMax(AValue: Double);
begin
  if SameValue(FXMax, AValue) then Exit;
  FXMax := AValue; Repaint;
end;

procedure TOBDXYHeatmap.SetYMin(AValue: Double);
begin
  if SameValue(FYMin, AValue) then Exit;
  FYMin := AValue; Repaint;
end;

procedure TOBDXYHeatmap.SetYMax(AValue: Double);
begin
  if SameValue(FYMax, AValue) then Exit;
  FYMax := AValue; Repaint;
end;

procedure TOBDXYHeatmap.PushSample(AX, AY: Double);
var
  Bx, By: Integer;
  Hits: Integer;
begin
  if (FXMax <= FXMin) or (FYMax <= FYMin) then Exit;
  Bx := Trunc((AX - FXMin) / (FXMax - FXMin) * FBinsX);
  By := Trunc((AY - FYMin) / (FYMax - FYMin) * FBinsY);
  if (Bx < 0) or (Bx >= FBinsX) then Exit;
  if (By < 0) or (By >= FBinsY) then Exit;
  Inc(FBins[Bx, By]);
  Hits := FBins[Bx, By];
  if Hits > FMaxHits then FMaxHits := Hits;
  NotifyBindings;
  Repaint;
end;

procedure TOBDXYHeatmap.Reset;
var I, J: Integer;
begin
  for I := 0 to FBinsX - 1 do
    for J := 0 to FBinsY - 1 do
      FBins[I, J] := 0;
  FMaxHits := 0;
  NotifyBindings;
  Repaint;
end;

procedure TOBDXYHeatmap.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  CellW, CellH: Single;
  Pad: Integer;
  I, J: Integer;
  Cell: TGPRectF;
  T: Single;
begin
  if (FBinsX <= 0) or (FBinsY <= 0) then Exit;
  if FMaxHits <= 0 then Exit;
  Pad := ScaleValue(4);
  CellW := (Width  - 2 * Pad) / FBinsX;
  CellH := (Height - 2 * Pad) / FBinsY;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to FBinsX - 1 do
      for J := 0 to FBinsY - 1 do
      begin
        if FBins[I, J] = 0 then Continue;
        T := FBins[I, J] / FMaxHits;
        Cell.X := Pad + I * CellW;
        // Y is inverted so high values render at the top.
        Cell.Y := Pad + (FBinsY - 1 - J) * CellH;
        Cell.Width  := CellW;
        Cell.Height := CellH;
        Brush := TGPSolidBrush.Create(ColorToARGB(HeatColor(T)));
        try
          Graphics.FillRectangle(Brush, Cell);
        finally
          Brush.Free;
        end;
      end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDTorqueRPMMap -------------------------------------------------- }

constructor TOBDTorqueRPMMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 200;
  FCells := TList<TOBDTorqueCell>.Create;
  FRPMMin := 800;  FRPMMax := 7000;
  FLoadMin := 0;   FLoadMax := 100;
  FMaxTorque := 500;
end;

destructor TOBDTorqueRPMMap.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TOBDTorqueRPMMap.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDTorqueRPMMap.SetCells(
  const ACells: array of TOBDTorqueCell);
var I: Integer;
begin
  FCells.Clear;
  for I := 0 to High(ACells) do FCells.Add(ACells[I]);
  NotifyBindings;
  Repaint;
end;

procedure TOBDTorqueRPMMap.Clear;
begin
  FCells.Clear;
  NotifyBindings;
  Repaint;
end;

function TOBDTorqueRPMMap.CellCount: Integer;
begin
  Result := FCells.Count;
end;

procedure TOBDTorqueRPMMap.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad: Integer;
  PlotW, PlotH: Integer;
  C: TOBDTorqueCell;
  X, Y, W, H: Single;
  T: Single;
begin
  if FCells.Count = 0 then Exit;
  if FMaxTorque <= 0 then Exit;
  Pad := ScaleValue(8);
  PlotW := Width - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if (PlotW <= 0) or (PlotH <= 0) then Exit;

  W := PlotW / 16;     // assumed grid size for rendering size
  H := PlotH / 12;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    for C in FCells do
    begin
      if FRPMMax <= FRPMMin then Continue;
      if FLoadMax <= FLoadMin then Continue;
      X := Pad + (C.RPM  - FRPMMin)  /
             (FRPMMax - FRPMMin)   * (PlotW - W);
      Y := Pad + (FLoadMax - C.Load) /
             (FLoadMax - FLoadMin) * (PlotH - H);
      T := Single(C.TorqueNm / FMaxTorque);
      Brush := TGPSolidBrush.Create(ColorToARGB(HeatColor(T)));
      try
        Graphics.FillRectangle(Brush, X, Y, W, H);
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDRunRecorder --------------------------------------------------- }

constructor TOBDRunRecorder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 80;
  FBuffer := TList<TOBDRunSample>.Create;
  FPreMs  := 1000;
  FPostMs := 5000;
end;

destructor TOBDRunRecorder.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TOBDRunRecorder.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDRunRecorder.SetPreMs(AValue: Cardinal);
begin
  if AValue < 100 then AValue := 100;
  if FPreMs = AValue then Exit;
  FPreMs := AValue;
end;

procedure TOBDRunRecorder.SetPostMs(AValue: Cardinal);
begin
  if AValue < 100 then AValue := 100;
  if FPostMs = AValue then Exit;
  FPostMs := AValue;
end;

procedure TOBDRunRecorder.TrimPreWindow(ATimeMs: Cardinal);
var
  Cutoff: Int64;
begin
  if FTriggered then Exit;
  Cutoff := Int64(ATimeMs) - Int64(FPreMs);
  while (FBuffer.Count > 0) and
        (FBuffer[0].TimeMs < Cardinal(System.Math.Max(Cutoff, 0))) do
    FBuffer.Delete(0);
end;

procedure TOBDRunRecorder.PushSample(ATimeMs: Cardinal;
  AValue: Double);
var
  S: TOBDRunSample;
begin
  S.TimeMs := ATimeMs;
  S.Value  := AValue;
  FBuffer.Add(S);
  TrimPreWindow(ATimeMs);
  if FTriggered and not FFinished then
    if ATimeMs >= FTriggerMs + FPostMs then
    begin
      FFinished := True;
      NotifyBindings;
      Repaint;
    end
    else
      Repaint;
end;

procedure TOBDRunRecorder.Trigger;
begin
  if FTriggered then Exit;
  FTriggered := True;
  if FBuffer.Count > 0 then
    FTriggerMs := FBuffer[FBuffer.Count - 1].TimeMs
  else
    FTriggerMs := 0;
  NotifyBindings;
  Repaint;
end;

procedure TOBDRunRecorder.Reset;
begin
  FBuffer.Clear;
  FTriggered := False;
  FFinished  := False;
  FTriggerMs := 0;
  NotifyBindings;
  Repaint;
end;

function TOBDRunRecorder.Samples: TArray<TOBDRunSample>;
begin
  Result := FBuffer.ToArray;
end;

procedure TOBDRunRecorder.PaintControl(ACanvas: TCanvas);
var
  Status: string;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Font;
  ACanvas.Font.Color := EffectiveForeground;
  if FFinished then
    Status := Format('run recorded — %d samples', [FBuffer.Count])
  else if FTriggered then
    Status := Format('recording post-window (%d ms)', [FPostMs])
  else
    Status := Format('pre-trigger %d ms / waiting (%d samples)',
      [FPreMs, FBuffer.Count]);
  ACanvas.TextOut(ScaleValue(8), ScaleValue(8), Status);
end;

end.
