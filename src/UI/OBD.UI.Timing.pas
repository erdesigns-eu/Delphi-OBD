//------------------------------------------------------------------------------
//  OBD.UI.Timing
//
//  Performance-timing visuals for hot-laps and drag runs:
//
//    TOBDDragTimer  captures a 0 → TargetSpeed run. State
//                   machine drives Idle → Armed → Capturing →
//                   Done; tracks last + best time, formats
//                   the readout as M:SS.HH.
//    TOBDLapTimer   continuous lap stopwatch with best-lap +
//                   delta-from-PB memory.
//    TOBDAccelGraph speed-vs-time plot built up sample-by-
//                   sample during a drag run.
//
//  All three are theme- / HiDPI- / VCL-Style-aware via
//  TOBDCustomControl and route every state change through
//  TBindings.Notify.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Timing;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Diagnostics,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>Drag-timer state machine.</summary>
  TOBDDragState = (
    /// <summary>Cleared. No run pending.</summary>
    dsIdle,
    /// <summary>Armed; waiting for movement to start the run.
    /// </summary>
    dsArmed,
    /// <summary>Movement started; clock is running.</summary>
    dsCapturing,
    /// <summary>Hit <c>TargetSpeed</c>; the elapsed time is the
    /// captured run.</summary>
    dsDone
  );

  /// <summary>Fires when a run completes (state transitions to
  /// <c>dsDone</c>) — useful for hosts that want to log /
  /// announce / chart the result.</summary>
  TOBDDragCompleteEvent = procedure(Sender: TObject;
    AElapsedMs: Cardinal; ABestMs: Cardinal) of object;

  /// <summary>0 → target speed run capture. Set
  /// <see cref="TargetSpeed"/>, feed <see cref="Speed"/> on
  /// each PID update, call <c>Arm</c> to start a new run. The
  /// timer starts the moment <c>Speed</c> first crosses 0
  /// and stops the moment <c>Speed &gt;= TargetSpeed</c>.
  /// </summary>
  TOBDDragTimer = class(TOBDCustomControl)
  strict private
    FState:        TOBDDragState;
    FSpeed:        Double;
    FTargetSpeed:  Double;
    FStartSpeed:   Double;
    FElapsedMs:    Cardinal;
    FLastMs:       Cardinal;
    FBestMs:       Cardinal;
    FStopwatch:    TStopwatch;
    FTickTimer:    TTimer;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    FOnComplete:   TOBDDragCompleteEvent;
    procedure SetSpeed(AValue: Double);
    procedure SetTargetSpeed(AValue: Double);
    procedure SetStartSpeed(AValue: Double);
    procedure SetValueFont(AValue: TFont);
    procedure SetCaptionFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure HandleTick(Sender: TObject);
    procedure NotifyBindings;
    procedure GoToState(ANewState: TOBDDragState);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Arms the timer for a new run. Clears the last
    /// elapsed time but keeps the best. The clock starts as
    /// soon as <c>Speed</c> exceeds <c>StartSpeed</c>.</summary>
    procedure Arm;
    /// <summary>Discards the current state, including the last
    /// captured time. Best time is preserved.</summary>
    procedure Reset;
    /// <summary>Wipes the personal-best memory too.</summary>
    procedure ResetBest;
    /// <summary>Formats <paramref name="AMs"/> as a human
    /// drag-time string: <c>"M:SS.HH"</c>.</summary>
    class function FormatTime(AMs: Cardinal): string; static;
    /// <summary>Current state machine position.</summary>
    property State:     TOBDDragState read FState;
    /// <summary>Last completed-run elapsed (ms). 0 when no run
    /// captured yet.</summary>
    property LastMs:    Cardinal read FLastMs;
    /// <summary>Best run-elapsed seen so far (ms).</summary>
    property BestMs:    Cardinal read FBestMs;
    /// <summary>Live elapsed during a capturing run (ms).</summary>
    property ElapsedMs: Cardinal read FElapsedMs;
  published
    /// <summary>Current vehicle speed. Host updates this on
    /// every PID 0x0D dispatch.</summary>
    property Speed: Double read FSpeed write SetSpeed;
    /// <summary>Speed the run captures to. Default 100
    /// (km/h). Hosts that want 0-60 mph set 60 here.</summary>
    property TargetSpeed: Double
      read FTargetSpeed write SetTargetSpeed;
    /// <summary>Speed threshold that starts the clock. Default
    /// 0.5 (km/h). Bumping this above zero filters out noisy
    /// near-stop readings.</summary>
    property StartSpeed: Double
      read FStartSpeed write SetStartSpeed;
    /// <summary>Font for the big elapsed-time readout.</summary>
    property ValueFont: TFont read FValueFont write SetValueFont;
    /// <summary>Font for the caption + best-time line.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Fires when the run completes.</summary>
    property OnComplete: TOBDDragCompleteEvent
      read FOnComplete write FOnComplete;
  end;

  /// <summary>Lap timer with running clock + best-lap + delta.
  /// Hosts call <c>StartLap</c> at the line, <c>EndLap</c> on
  /// the next crossing — the lamp logs the lap, updates the
  /// best, computes a delta-from-PB.</summary>
  TOBDLapTimer = class(TOBDCustomControl)
  strict private
    FStopwatch:    TStopwatch;
    FRunning:      Boolean;
    FLapCount:     Integer;
    FCurrentMs:    Cardinal;
    FLastMs:       Cardinal;
    FBestMs:       Cardinal;
    FDeltaMs:      Integer;
    FTickTimer:    TTimer;
    FValueFont:    TFont;
    FCaptionFont:  TFont;
    procedure HandleTick(Sender: TObject);
    procedure HandleFontChange(Sender: TObject);
    procedure SetValueFont(AValue: TFont);
    procedure SetCaptionFont(AValue: TFont);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Begins (or restarts) lap-time accumulation. Use
    /// at the start line and at every subsequent crossing
    /// (i.e. <c>EndLap</c> implicitly chains to a new lap if
    /// the host calls <c>StartLap</c> again).</summary>
    procedure StartLap;
    /// <summary>Closes the current lap, records the elapsed as
    /// <c>LastMs</c>, updates <c>BestMs</c> and
    /// <c>DeltaMs</c>, increments <c>LapCount</c>. Stops the
    /// running clock.</summary>
    procedure EndLap;
    /// <summary>Clears every accumulated lap.</summary>
    procedure Reset;
    /// <summary>True while the live clock is running.</summary>
    property Running:   Boolean  read FRunning;
    /// <summary>Number of laps completed (incremented on
    /// <c>EndLap</c>).</summary>
    property LapCount:  Integer  read FLapCount;
    /// <summary>Running elapsed of the current lap (ms).
    /// </summary>
    property CurrentMs: Cardinal read FCurrentMs;
    /// <summary>Most recently completed lap (ms).</summary>
    property LastMs:    Cardinal read FLastMs;
    /// <summary>Best lap so far (ms). 0 when none captured.
    /// </summary>
    property BestMs:    Cardinal read FBestMs;
    /// <summary>Signed difference of <c>LastMs - BestMs</c>.
    /// Negative = faster than PB.</summary>
    property DeltaMs:   Integer  read FDeltaMs;
  published
    /// <summary>Big lap-time readout font.</summary>
    property ValueFont: TFont read FValueFont write SetValueFont;
    /// <summary>Caption + best / delta line font.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>One sample on the accel graph.</summary>
  TOBDAccelSample = record
    /// <summary>Milliseconds since the run started.</summary>
    TimeMs: Cardinal;
    /// <summary>Speed at that instant.</summary>
    Speed:  Double;
  end;

  /// <summary>Speed-vs-time plot. Push one sample per PID
  /// update during a drag run; the graph auto-scales the X
  /// axis to <c>MaxTimeMs</c> and the Y axis to
  /// <c>MaxSpeed</c>. Past samples persist in a ring buffer
  /// until the next <c>Reset</c>.</summary>
  TOBDAccelGraph = class(TOBDCustomControl)
  strict private
    FSamples:    TArray<TOBDAccelSample>;
    FCount:      Integer;
    FCapacity:   Integer;
    FMaxTimeMs:  Cardinal;
    FMaxSpeed:   Double;
    FLineColor:  TColor;
    procedure SetCapacity(AValue: Integer);
    procedure SetMaxTimeMs(AValue: Cardinal);
    procedure SetMaxSpeed(AValue: Double);
    procedure SetLineColor(AValue: TColor);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Appends one (time, speed) sample. Old samples
    /// are dropped when the ring fills.</summary>
    procedure PushSample(ATimeMs: Cardinal; ASpeed: Double);
    /// <summary>Drops every sample.</summary>
    procedure Reset;
    /// <summary>Number of samples currently stored.</summary>
    function SampleCount: Integer;
  published
    /// <summary>Ring-buffer capacity. Default 512.</summary>
    property Capacity: Integer
      read FCapacity write SetCapacity default 512;
    /// <summary>X-axis upper bound (ms). Default 10000 ms
    /// (covers a typical 0-100 km/h capture).</summary>
    property MaxTimeMs: Cardinal
      read FMaxTimeMs write SetMaxTimeMs default 10000;
    /// <summary>Y-axis upper bound. Default 200 (km/h).
    /// </summary>
    property MaxSpeed: Double
      read FMaxSpeed write SetMaxSpeed;
    /// <summary>Trace colour. Default theme accent.</summary>
    property LineColor: TColor
      read FLineColor write SetLineColor default clDefault;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

function ResolveColor(ASlot, AFallback: TColor): TColor; inline;
begin
  if ASlot <> clDefault then Result := ASlot else Result := AFallback;
end;

{ ---- TOBDDragTimer ------------------------------------------------------- }

constructor TOBDDragTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 220;
  Height := 100;
  FState        := dsIdle;
  FSpeed        := 0;
  FTargetSpeed  := 100;     // 0-100 km/h is the default capture
  FStartSpeed   := 0.5;
  FElapsedMs    := 0;
  FLastMs       := 0;
  FBestMs       := 0;
  FStopwatch    := TStopwatch.Create;
  FTickTimer := TTimer.Create(Self);
  FTickTimer.Enabled  := False;
  FTickTimer.Interval := 33;
  FTickTimer.OnTimer  := HandleTick;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 26;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDDragTimer.Destroy;
begin
  FTickTimer.Free;
  FValueFont.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDDragTimer.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDragTimer.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDDragTimer.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDDragTimer.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDDragTimer.GoToState(ANewState: TOBDDragState);
begin
  if FState = ANewState then Exit;
  FState := ANewState;
  case FState of
    dsIdle, dsArmed:
      begin
        FTickTimer.Enabled := False;
        FElapsedMs := 0;
      end;
    dsCapturing:
      begin
        FStopwatch := TStopwatch.StartNew;
        FElapsedMs := 0;
        FTickTimer.Enabled := not (csDesigning in ComponentState);
      end;
    dsDone:
      begin
        FTickTimer.Enabled := False;
        FElapsedMs := Cardinal(FStopwatch.ElapsedMilliseconds);
        FLastMs := FElapsedMs;
        if (FBestMs = 0) or (FElapsedMs < FBestMs) then
          FBestMs := FElapsedMs;
        if Assigned(FOnComplete) then
          try
            FOnComplete(Self, FLastMs, FBestMs);
          except
          end;
      end;
  end;
  NotifyBindings;
  Repaint;
end;

procedure TOBDDragTimer.HandleTick(Sender: TObject);
begin
  if FState = dsCapturing then
  begin
    FElapsedMs := Cardinal(FStopwatch.ElapsedMilliseconds);
    Repaint;
  end;
end;

procedure TOBDDragTimer.SetSpeed(AValue: Double);
begin
  if SameValue(FSpeed, AValue) then Exit;
  FSpeed := AValue;
  // State transitions are driven by the speed crossings, not
  // by direct host method calls — that keeps the timer
  // tight against the PID update cadence.
  case FState of
    dsArmed:
      if FSpeed > FStartSpeed then GoToState(dsCapturing);
    dsCapturing:
      if FSpeed >= FTargetSpeed then GoToState(dsDone);
  end;
  Repaint;
end;

procedure TOBDDragTimer.SetTargetSpeed(AValue: Double);
begin
  if SameValue(FTargetSpeed, AValue) then Exit;
  FTargetSpeed := AValue; Repaint;
end;

procedure TOBDDragTimer.SetStartSpeed(AValue: Double);
begin
  if SameValue(FStartSpeed, AValue) then Exit;
  FStartSpeed := AValue;
end;

procedure TOBDDragTimer.Arm;
begin
  FElapsedMs := 0;
  GoToState(dsArmed);
end;

procedure TOBDDragTimer.Reset;
begin
  FLastMs   := 0;
  FElapsedMs := 0;
  GoToState(dsIdle);
end;

procedure TOBDDragTimer.ResetBest;
begin
  FBestMs := 0;
  NotifyBindings;
  Repaint;
end;

class function TOBDDragTimer.FormatTime(AMs: Cardinal): string;
var
  Mins, Secs, Hundredths: Cardinal;
begin
  Hundredths := (AMs mod 1000) div 10;
  Secs := (AMs div 1000) mod 60;
  Mins := AMs div 60000;
  Result := Format('%d:%2.2d.%2.2d', [Mins, Secs, Hundredths]);
end;

procedure TOBDDragTimer.PaintControl(ACanvas: TCanvas);
var
  StateText, BigText, BestText: string;
  R: TRect;
  ValueH: Integer;
begin
  R := ClientRect;
  ACanvas.Brush.Style := bsClear;

  // Caption / state.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  case FState of
    dsIdle:      StateText := Format('0-%g  -  ready', [FTargetSpeed]);
    dsArmed:     StateText := Format('0-%g  -  armed', [FTargetSpeed]);
    dsCapturing: StateText := Format('0-%g  -  capturing', [FTargetSpeed]);
    dsDone:      StateText := Format('0-%g  -  done', [FTargetSpeed]);
  end;
  ACanvas.TextOut(ScaleValue(8), ScaleValue(4), StateText);

  // Big readout.
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveAccent;
  if FState = dsDone then
    BigText := FormatTime(FLastMs)
  else
    BigText := FormatTime(FElapsedMs);
  ValueH := ACanvas.TextHeight('0');
  ACanvas.TextOut(
    (R.Width - ACanvas.TextWidth(BigText)) div 2,
    ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2),
    BigText);

  // Best line.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FBestMs > 0 then
    BestText := 'best: ' + FormatTime(FBestMs)
  else
    BestText := 'best: -';
  ACanvas.TextOut(
    (R.Width - ACanvas.TextWidth(BestText)) div 2,
    ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2) +
      ValueH + ScaleValue(2),
    BestText);
end;

{ ---- TOBDLapTimer -------------------------------------------------------- }

constructor TOBDLapTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 220;
  Height := 110;
  FStopwatch := TStopwatch.Create;
  FRunning := False;
  FLapCount := 0;
  FTickTimer := TTimer.Create(Self);
  FTickTimer.Enabled  := False;
  FTickTimer.Interval := 33;
  FTickTimer.OnTimer  := HandleTick;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 24;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDLapTimer.Destroy;
begin
  FTickTimer.Free;
  FValueFont.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDLapTimer.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLapTimer.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDLapTimer.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDLapTimer.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDLapTimer.HandleTick(Sender: TObject);
begin
  if FRunning then
  begin
    FCurrentMs := Cardinal(FStopwatch.ElapsedMilliseconds);
    Repaint;
  end;
end;

procedure TOBDLapTimer.StartLap;
begin
  if FRunning then EndLap;
  FStopwatch := TStopwatch.StartNew;
  FCurrentMs := 0;
  FRunning   := True;
  FTickTimer.Enabled := not (csDesigning in ComponentState);
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTimer.EndLap;
begin
  if not FRunning then Exit;
  FCurrentMs := Cardinal(FStopwatch.ElapsedMilliseconds);
  FLastMs    := FCurrentMs;
  Inc(FLapCount);
  if (FBestMs = 0) or (FLastMs < FBestMs) then
  begin
    FBestMs := FLastMs;
    FDeltaMs := 0;
  end
  else
    FDeltaMs := Integer(FLastMs) - Integer(FBestMs);
  FRunning := False;
  FTickTimer.Enabled := False;
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTimer.Reset;
begin
  FRunning   := False;
  FLapCount  := 0;
  FCurrentMs := 0;
  FLastMs    := 0;
  FBestMs    := 0;
  FDeltaMs   := 0;
  FTickTimer.Enabled := False;
  NotifyBindings;
  Repaint;
end;

procedure TOBDLapTimer.PaintControl(ACanvas: TCanvas);
var
  R: TRect;
  ShownMs: Cardinal;
  Big, Cap1, Cap2: string;
  ValueH: Integer;
begin
  R := ClientRect;
  ACanvas.Brush.Style := bsClear;

  // Caption row.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(ScaleValue(8), ScaleValue(4),
    Format('lap #%d  -  %s', [FLapCount,
      IfThen(FRunning, 'running', 'stopped')]));

  // Big readout — either the live current lap, or the last
  // completed lap if we're stopped.
  if FRunning then ShownMs := FCurrentMs else ShownMs := FLastMs;
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveAccent;
  Big := TOBDDragTimer.FormatTime(ShownMs);
  ValueH := ACanvas.TextHeight('0');
  ACanvas.TextOut(
    (R.Width - ACanvas.TextWidth(Big)) div 2,
    ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2),
    Big);

  // best / delta row.
  ACanvas.Font := FCaptionFont;
  if FBestMs > 0 then
    Cap1 := 'best: ' + TOBDDragTimer.FormatTime(FBestMs)
  else
    Cap1 := 'best: -';
  if (FLastMs > 0) and (FBestMs > 0) then
  begin
    if FDeltaMs = 0 then
      Cap2 := 'PB!'
    else if FDeltaMs < 0 then
      Cap2 := Format('-%d.%2.2d',
        [Abs(FDeltaMs) div 1000, (Abs(FDeltaMs) mod 1000) div 10])
    else
      Cap2 := Format('+%d.%2.2d',
        [FDeltaMs div 1000, (FDeltaMs mod 1000) div 10]);
  end
  else
    Cap2 := '';

  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(ScaleValue(8),
    ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2) +
      ValueH + ScaleValue(2),
    Cap1);
  if Cap2 <> '' then
  begin
    if FDeltaMs <= 0 then
      ACanvas.Font.Color := Palette.Success
    else
      ACanvas.Font.Color := Palette.Danger;
    ACanvas.TextOut(
      R.Width - ACanvas.TextWidth(Cap2) - ScaleValue(8),
      ScaleValue(4) + ACanvas.TextHeight('Mg') + ScaleValue(2) +
        ValueH + ScaleValue(2),
      Cap2);
  end;
end;

{ ---- TOBDAccelGraph ------------------------------------------------------ }

constructor TOBDAccelGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 260;
  Height := 140;
  FCapacity  := 512;
  FMaxTimeMs := 10000;
  FMaxSpeed  := 200;
  FLineColor := clDefault;
  SetLength(FSamples, FCapacity);
end;

procedure TOBDAccelGraph.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDAccelGraph.SetCapacity(AValue: Integer);
begin
  if AValue < 32 then AValue := 32;
  if AValue > 16384 then AValue := 16384;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  SetLength(FSamples, FCapacity);
  FCount := 0;
  Repaint;
end;

procedure TOBDAccelGraph.SetMaxTimeMs(AValue: Cardinal);
begin
  if AValue < 100 then AValue := 100;
  if FMaxTimeMs = AValue then Exit;
  FMaxTimeMs := AValue; Repaint;
end;

procedure TOBDAccelGraph.SetMaxSpeed(AValue: Double);
begin
  if SameValue(FMaxSpeed, AValue) then Exit;
  if AValue < 1 then AValue := 1;
  FMaxSpeed := AValue; Repaint;
end;

procedure TOBDAccelGraph.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then Exit;
  FLineColor := AValue; Repaint;
end;

procedure TOBDAccelGraph.PushSample(ATimeMs: Cardinal; ASpeed: Double);
begin
  if FCount < FCapacity then
  begin
    FSamples[FCount].TimeMs := ATimeMs;
    FSamples[FCount].Speed  := ASpeed;
    Inc(FCount);
  end
  else
  begin
    // Ring overflow: shift left by one sample. Drag runs end
    // before the ring fills at sensible capacities, so this
    // path is the safety net rather than the common case.
    Move(FSamples[1], FSamples[0],
      (FCapacity - 1) * SizeOf(TOBDAccelSample));
    FSamples[FCapacity - 1].TimeMs := ATimeMs;
    FSamples[FCapacity - 1].Speed  := ASpeed;
  end;
  NotifyBindings;
  Repaint;
end;

procedure TOBDAccelGraph.Reset;
begin
  FCount := 0;
  NotifyBindings;
  Repaint;
end;

function TOBDAccelGraph.SampleCount: Integer;
begin
  Result := FCount;
end;

procedure TOBDAccelGraph.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Points: array of TGPPointF;
  I: Integer;
  Pad: Integer;
  PlotW, PlotH: Integer;
  Col: TColor;
begin
  Pad   := ScaleValue(8);
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if (PlotW <= 0) or (PlotH <= 0) then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // Axes.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawLine(Pen,
        Single(Pad), Single(Pad),
        Single(Pad), Single(Pad + PlotH));
      Graphics.DrawLine(Pen,
        Single(Pad), Single(Pad + PlotH),
        Single(Pad + PlotW), Single(Pad + PlotH));
    finally
      Pen.Free;
    end;

    if FCount < 2 then Exit;

    SetLength(Points, FCount);
    for I := 0 to FCount - 1 do
    begin
      Points[I].X := Pad +
        (FSamples[I].TimeMs / FMaxTimeMs) * PlotW;
      Points[I].Y := Pad + PlotH -
        Single(FSamples[I].Speed / FMaxSpeed) * PlotH;
      // Clamp inside the plot — out-of-range samples just sit
      // on the edge rather than disappearing entirely.
      if Points[I].X > Pad + PlotW then Points[I].X := Pad + PlotW;
      if Points[I].Y < Pad then Points[I].Y := Pad;
      if Points[I].Y > Pad + PlotH then Points[I].Y := Pad + PlotH;
    end;

    Col := ResolveColor(FLineColor, EffectiveAccent);
    Pen := TGPPen.Create(ColorToARGB(Col), ScaleValue(2));
    Pen.SetStartCap(LineCapRound);
    Pen.SetEndCap(LineCapRound);
    Pen.SetLineJoin(LineJoinRound);
    try
      Graphics.DrawLines(Pen, PGPPointF(@Points[0]), FCount);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
