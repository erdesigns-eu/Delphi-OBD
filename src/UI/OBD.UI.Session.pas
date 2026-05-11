//------------------------------------------------------------------------------
//  OBD.UI.Session
//
//  Session / progress visuals for the A2 inventory:
//
//    TOBDFlashProgress       Multi-phase progress strip for
//                            TOBDFlashPipeline. Phase chips +
//                            overall progress bar.
//    TOBDCodingSessionPanel  State lamp + write counter + last
//                            audit line. Host drives state +
//                            count via setters.
//    TOBDXCPProgressBar      XCP upload / download progress
//                            with KB/s + ETA.
//    TOBDRecorderToolbar     Paint-driven record / pause /
//                            stop / load / replay button row,
//                            duration readout + frame badge.
//                            Buttons fire OnXxx events on
//                            click; host owns the I/O.
//
//  All four inherit theme / HiDPI / VCL-Style awareness from
//  TOBDCustomControl, route state mutations through
//  TBindings.Notify, and guard timers + subscriptions with
//  csDesigning so the IDE Designer stays responsive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Session;

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
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.Flash.Phases;

type
  /// <summary>Coding-session state.</summary>
  TOBDCodingState = (
    /// <summary>No session — idle / detached.</summary>
    cdsIdle,
    /// <summary>Live session, no writes yet.</summary>
    cdsActive,
    /// <summary>One or more writes have committed.</summary>
    cdsWriting,
    /// <summary>Rollback in progress.</summary>
    cdsRollingBack,
    /// <summary>Session ended cleanly.</summary>
    cdsCompleted,
    /// <summary>Session aborted on error.</summary>
    cdsError
  );

  /// <summary>Multi-phase flash progress strip. Row of phase
  /// chips at the top (pre / verify / enter / transfer /
  /// verify / reset / finalise), big overall progress bar at
  /// the bottom. Host calls <see cref="Update"/> from
  /// <c>TOBDFlashPipeline</c>'s phase + progress events.
  /// </summary>
  TOBDFlashProgress = class(TOBDCustomControl)
  strict private
    FCurrentPhase:   TOBDFlashPhase;
    FOverallPercent: Single;
    FPhasePercent:   Single;
    FStatusText:     string;
    FErrored:        Boolean;
    FCaptionFont:    TFont;
    FValueFont:      TFont;
    procedure SetCurrentPhase(AValue: TOBDFlashPhase);
    procedure SetOverallPercent(AValue: Single);
    procedure SetPhasePercent(AValue: Single);
    procedure SetStatusText(const AValue: string);
    procedure SetErrored(AValue: Boolean);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  PhaseShortName(APhase: TOBDFlashPhase): string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>One-shot update from a pipeline event. Sets
    /// the current phase, overall %, current-phase %, and
    /// optional status text in one call.</summary>
    procedure Update(APhase: TOBDFlashPhase;
      AOverallPercent, APhasePercent: Single;
      const AStatus: string = '');
  published
    /// <summary>Current pipeline phase. Default
    /// <c>fpPreflight</c>.</summary>
    property CurrentPhase: TOBDFlashPhase
      read FCurrentPhase write SetCurrentPhase
      default fpPreflight;
    /// <summary>Overall progress 0..100. Clamps.</summary>
    property OverallPercent: Single
      read FOverallPercent write SetOverallPercent;
    /// <summary>Current-phase progress 0..100.</summary>
    property PhasePercent: Single
      read FPhasePercent write SetPhasePercent;
    /// <summary>Free-text status line.</summary>
    property StatusText: string
      read FStatusText write SetStatusText;
    /// <summary>True flips the strip into the error palette
    /// (used by hosts that wire OnError to the pipeline).
    /// </summary>
    property Errored: Boolean
      read FErrored write SetErrored default False;
    /// <summary>Caption font (phase chips, status).</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Overall-percent font.</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Compact coding-session control panel: state
  /// lamp, write counter, rollback-available indicator, last
  /// audit-trail line.</summary>
  TOBDCodingSessionPanel = class(TOBDCustomControl)
  strict private
    FState:             TOBDCodingState;
    FWriteCount:        Integer;
    FRollbackAvailable: Boolean;
    FLastAudit:         string;
    FCaptionFont:       TFont;
    FValueFont:         TFont;
    procedure SetState(AValue: TOBDCodingState);
    procedure SetWriteCount(AValue: Integer);
    procedure SetRollbackAvailable(AValue: Boolean);
    procedure SetLastAudit(const AValue: string);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  StateColor: TColor;
    function  StateText: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Session state. Default <c>cdsIdle</c>.</summary>
    property State: TOBDCodingState read FState write SetState
      default cdsIdle;
    /// <summary>Number of writes committed in this session.
    /// </summary>
    property WriteCount: Integer
      read FWriteCount write SetWriteCount default 0;
    /// <summary>True when rollback is available (a snapshot
    /// was captured before the writes). Default False.</summary>
    property RollbackAvailable: Boolean
      read FRollbackAvailable write SetRollbackAvailable
      default False;
    /// <summary>Most recent audit-trail line. One free-text
    /// line shown under the state lamp.</summary>
    property LastAudit: string
      read FLastAudit write SetLastAudit;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>XCP upload / download progress strip. Host
  /// updates from XCP block events.</summary>
  TOBDXCPProgressBar = class(TOBDCustomControl)
  strict private
    FBytesDone:   Int64;
    FBytesTotal:  Int64;
    FKBPerSecond: Double;
    FLabel:       string;
    FCaptionFont: TFont;
    FValueFont:   TFont;
    procedure SetBytesDone(AValue: Int64);
    procedure SetBytesTotal(AValue: Int64);
    procedure SetKBPerSecond(AValue: Double);
    procedure SetLabel(const AValue: string);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  ETASeconds: Double;
    function  FormatBytes(AVal: Int64): string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>One-shot update. <paramref name="ABytesTotal"/>
    /// = 0 leaves the previous total in place.</summary>
    procedure Update(ABytesDone, ABytesTotal: Int64;
      AKBPerSecond: Double; const ALabel: string = '');
  published
    /// <summary>Bytes done so far.</summary>
    property BytesDone: Int64
      read FBytesDone write SetBytesDone default 0;
    /// <summary>Total payload size in bytes.</summary>
    property BytesTotal: Int64
      read FBytesTotal write SetBytesTotal default 0;
    /// <summary>Current throughput (KB/s).</summary>
    property KBPerSecond: Double
      read FKBPerSecond write SetKBPerSecond;
    /// <summary>Caption above the bar (e.g. "Upload" /
    /// "Download" / "Verify").</summary>
    property &Label: string read FLabel write SetLabel;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Identifier for the button hit in
  /// <c>TOBDRecorderToolbar</c>.</summary>
  TOBDRecorderButton = (
    rbRecord,
    rbPause,
    rbStop,
    rbLoad,
    rbReplay
  );

  /// <summary>Fires when one of the recorder buttons is
  /// clicked. The toolbar paints state but doesn't drive the
  /// recorder itself — the host wires this to its
  /// TOBDRecorder / TOBDReplayer.</summary>
  TOBDRecorderButtonEvent = procedure(Sender: TObject;
    AButton: TOBDRecorderButton) of object;

  /// <summary>Top-of-form toolbar for recorder operations.
  /// Paint-driven (no sub-controls). Buttons highlight on
  /// hover; clicks fire <see cref="OnButtonClick"/>. Host
  /// updates <see cref="Recording"/> / <see cref="Paused"/> /
  /// <see cref="DurationMs"/> / <see cref="FrameCount"/> so
  /// the UI tracks the recorder state.</summary>
  TOBDRecorderToolbar = class(TOBDCustomControl)
  strict private
    FRecording:    Boolean;
    FPaused:       Boolean;
    FDurationMs:   Int64;
    FFrameCount:   Int64;
    FHoverButton:  Integer;     // -1 = none
    FButtonRects:  array[TOBDRecorderButton] of TRect;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    FOnClick:      TOBDRecorderButtonEvent;
    procedure SetRecording(AValue: Boolean);
    procedure SetPaused(AValue: Boolean);
    procedure SetDurationMs(AValue: Int64);
    procedure SetFrameCount(AValue: Int64);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure LayoutButtons(out ABounds: TRect);
    function  ButtonAt(X, Y: Integer): Integer;
    function  ButtonCaption(AButton: TOBDRecorderButton): string;
    procedure DrawButton(ACanvas: TCanvas;
      const ARect: TRect; const ACaption: string;
      AActive, AHover: Boolean; AAccent: TColor);
    function  FormatDuration(AMs: Int64): string;
    procedure FireClick(AButton: TOBDRecorderButton);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage);
      message CM_MOUSELEAVE;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>True while the recorder is capturing.</summary>
    property Recording: Boolean
      read FRecording write SetRecording default False;
    /// <summary>True when the capture is paused.</summary>
    property Paused: Boolean
      read FPaused write SetPaused default False;
    /// <summary>Elapsed milliseconds (formatted as
    /// <c>H:MM:SS</c>).</summary>
    property DurationMs: Int64
      read FDurationMs write SetDurationMs default 0;
    /// <summary>Frames captured so far.</summary>
    property FrameCount: Int64
      read FFrameCount write SetFrameCount default 0;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
    /// <summary>Fires when the user clicks a button.</summary>
    property OnButtonClick: TOBDRecorderButtonEvent
      read FOnClick write FOnClick;
  end;

implementation

{ ---- TOBDFlashProgress --------------------------------------------------- }

constructor TOBDFlashProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 480;
  Height := 90;
  FCurrentPhase := fpPreflight;
  FOverallPercent := 0;
  FPhasePercent   := 0;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 8;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 12;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDFlashProgress.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDFlashProgress.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFlashProgress.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDFlashProgress.SetCurrentPhase(AValue: TOBDFlashPhase);
begin
  if FCurrentPhase = AValue then Exit;
  FCurrentPhase := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashProgress.SetOverallPercent(AValue: Single);
begin
  if AValue <   0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FOverallPercent, AValue) then Exit;
  FOverallPercent := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashProgress.SetPhasePercent(AValue: Single);
begin
  if AValue <   0 then AValue := 0;
  if AValue > 100 then AValue := 100;
  if SameValue(FPhasePercent, AValue) then Exit;
  FPhasePercent := AValue;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashProgress.SetStatusText(const AValue: string);
begin
  if FStatusText = AValue then Exit;
  FStatusText := AValue; Repaint;
end;

procedure TOBDFlashProgress.SetErrored(AValue: Boolean);
begin
  if FErrored = AValue then Exit;
  FErrored := AValue; Repaint;
end;

procedure TOBDFlashProgress.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDFlashProgress.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDFlashProgress.Update(APhase: TOBDFlashPhase;
  AOverallPercent, APhasePercent: Single; const AStatus: string);
begin
  FCurrentPhase   := APhase;
  if AOverallPercent <   0 then AOverallPercent := 0;
  if AOverallPercent > 100 then AOverallPercent := 100;
  if APhasePercent   <   0 then APhasePercent   := 0;
  if APhasePercent   > 100 then APhasePercent   := 100;
  FOverallPercent := AOverallPercent;
  FPhasePercent   := APhasePercent;
  FStatusText     := AStatus;
  NotifyBindings;
  Repaint;
end;

function TOBDFlashProgress.PhaseShortName(
  APhase: TOBDFlashPhase): string;
begin
  case APhase of
    fpPreflight:        Result := 'pre';
    fpVerifyImage:      Result := 'verify-img';
    fpEnterProgramming: Result := 'enter';
    fpTransfer:         Result := 'xfer';
    fpVerify:           Result := 'verify';
    fpReset:            Result := 'reset';
    fpFinalise:         Result := 'final';
  else
    Result := '';
  end;
end;

procedure TOBDFlashProgress.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, ChipW, ChipH, ChipY, GapW: Integer;
  P: TOBDFlashPhase;
  Chip: TGPRectF;
  AccentCol, BodyCol: TColor;
  Track, Fill: TGPRectF;
  PercentText: string;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  ChipH := ScaleValue(20);
  ChipY := Pad;

  // Phase chip row.
  GapW := ScaleValue(4);
  ChipW := (Width - 2 * Pad - 6 * GapW) div 7;

  if FErrored then AccentCol := Palette.Danger
  else             AccentCol := EffectiveAccent;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for P := Low(TOBDFlashPhase) to High(TOBDFlashPhase) do
    begin
      Chip.X := Pad + Ord(P) * (ChipW + GapW);
      Chip.Y := ChipY;
      Chip.Width  := ChipW;
      Chip.Height := ChipH;
      if Ord(P) < Ord(FCurrentPhase) then
        BodyCol := Palette.Success
      else if P = FCurrentPhase then
        BodyCol := AccentCol
      else
        BodyCol := Palette.NeutralLight;
      Brush := TGPSolidBrush.Create(ColorToARGB(BodyCol));
      try
        Graphics.FillRectangle(Brush, Chip);
      finally
        Brush.Free;
      end;
      ACanvas.Font := FCaptionFont;
      ACanvas.Font.Color := clWhite;
      ACanvas.TextOut(
        Round(Chip.X + (Chip.Width  -
          ACanvas.TextWidth(PhaseShortName(P))) / 2),
        Round(Chip.Y + (Chip.Height -
          ACanvas.TextHeight('Mg')) / 2),
        PhaseShortName(P));
    end;

    // Overall progress bar.
    Track.X := Pad;
    Track.Y := ChipY + ChipH + ScaleValue(8);
    Track.Width  := Width - 2 * Pad - ScaleValue(80);
    Track.Height := ScaleValue(14);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;
    Fill := Track;
    Fill.Width := Single(FOverallPercent / 100.0) * Track.Width;
    Brush := TGPSolidBrush.Create(ColorToARGB(AccentCol));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
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

  // Percent text + status.
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveForeground;
  PercentText := Format('%.0f %%', [FOverallPercent]);
  ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(PercentText),
    ChipY + ChipH + ScaleValue(8) - ScaleValue(2), PercentText);

  if FStatusText <> '' then
  begin
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, ChipY + ChipH + ScaleValue(8) +
      ScaleValue(14) + ScaleValue(4), FStatusText);
  end;
end;

{ ---- TOBDCodingSessionPanel ---------------------------------------------- }

constructor TOBDCodingSessionPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 100;
  FState      := cdsIdle;
  FWriteCount := 0;
  FRollbackAvailable := False;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 11;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDCodingSessionPanel.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDCodingSessionPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDCodingSessionPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDCodingSessionPanel.SetState(AValue: TOBDCodingState);
begin
  if FState = AValue then Exit;
  FState := AValue; NotifyBindings; Repaint;
end;

procedure TOBDCodingSessionPanel.SetWriteCount(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FWriteCount = AValue then Exit;
  FWriteCount := AValue; NotifyBindings; Repaint;
end;

procedure TOBDCodingSessionPanel.SetRollbackAvailable(AValue: Boolean);
begin
  if FRollbackAvailable = AValue then Exit;
  FRollbackAvailable := AValue; NotifyBindings; Repaint;
end;

procedure TOBDCodingSessionPanel.SetLastAudit(const AValue: string);
begin
  if FLastAudit = AValue then Exit;
  FLastAudit := AValue; NotifyBindings; Repaint;
end;

procedure TOBDCodingSessionPanel.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDCodingSessionPanel.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

function TOBDCodingSessionPanel.StateColor: TColor;
begin
  case FState of
    cdsIdle:        Result := Palette.NeutralLight;
    cdsActive:      Result := EffectiveAccent;
    cdsWriting:     Result := Palette.Warning;
    cdsRollingBack: Result := Palette.Warning;
    cdsCompleted:   Result := Palette.Success;
    cdsError:       Result := Palette.Danger;
  else
    Result := EffectiveForeground;
  end;
end;

function TOBDCodingSessionPanel.StateText: string;
begin
  case FState of
    cdsIdle:        Result := 'idle';
    cdsActive:      Result := 'active';
    cdsWriting:     Result := 'writing';
    cdsRollingBack: Result := 'rolling back';
    cdsCompleted:   Result := 'completed';
    cdsError:       Result := 'error';
  else
    Result := '';
  end;
end;

procedure TOBDCodingSessionPanel.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad, LampD: Integer;
  Lamp: TGPRectF;
  TextX, Y: Integer;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);
  LampD := ScaleValue(14);

  Lamp.X := Pad;
  Lamp.Y := Pad + ScaleValue(2);
  Lamp.Width  := LampD;
  Lamp.Height := LampD;
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(StateColor));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillEllipse(Brush, Lamp);
  finally
    Brush.Free;
    Graphics.Free;
  end;

  TextX := Pad + LampD + ScaleValue(8);

  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := StateColor;
  ACanvas.TextOut(TextX, Pad, StateText);

  Y := Pad + ACanvas.TextHeight(StateText) + ScaleValue(4);
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(TextX, Y,
    Format('writes: %d   rollback: %s',
      [FWriteCount,
       BoolToStr(FRollbackAvailable, True)]));

  if FLastAudit <> '' then
  begin
    Y := Y + ACanvas.TextHeight('M') + ScaleValue(4);
    ACanvas.TextOut(TextX, Y, FLastAudit);
  end;
end;

{ ---- TOBDXCPProgressBar -------------------------------------------------- }

constructor TOBDXCPProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 64;
  FBytesDone   := 0;
  FBytesTotal  := 0;
  FKBPerSecond := 0;
  FLabel := '';
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDXCPProgressBar.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDXCPProgressBar.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDXCPProgressBar.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDXCPProgressBar.SetBytesDone(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FBytesDone = AValue then Exit;
  FBytesDone := AValue; NotifyBindings; Repaint;
end;

procedure TOBDXCPProgressBar.SetBytesTotal(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FBytesTotal = AValue then Exit;
  FBytesTotal := AValue; NotifyBindings; Repaint;
end;

procedure TOBDXCPProgressBar.SetKBPerSecond(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FKBPerSecond, AValue) then Exit;
  FKBPerSecond := AValue; NotifyBindings; Repaint;
end;

procedure TOBDXCPProgressBar.SetLabel(const AValue: string);
begin
  if FLabel = AValue then Exit;
  FLabel := AValue; Repaint;
end;

procedure TOBDXCPProgressBar.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDXCPProgressBar.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDXCPProgressBar.Update(ABytesDone, ABytesTotal: Int64;
  AKBPerSecond: Double; const ALabel: string);
begin
  if ABytesDone   < 0 then ABytesDone   := 0;
  if ABytesTotal  < 0 then ABytesTotal  := 0;
  if AKBPerSecond < 0 then AKBPerSecond := 0;
  FBytesDone   := ABytesDone;
  if ABytesTotal > 0 then FBytesTotal := ABytesTotal;
  FKBPerSecond := AKBPerSecond;
  if ALabel <> '' then FLabel := ALabel;
  NotifyBindings;
  Repaint;
end;

function TOBDXCPProgressBar.ETASeconds: Double;
var
  Remaining: Int64;
begin
  if (FBytesTotal <= FBytesDone) or (FKBPerSecond <= 0) then
    Exit(0);
  Remaining := FBytesTotal - FBytesDone;
  Result := Remaining / 1024.0 / FKBPerSecond;
end;

function TOBDXCPProgressBar.FormatBytes(AVal: Int64): string;
begin
  if AVal < 1024 then
    Result := Format('%d B', [AVal])
  else if AVal < 1024 * 1024 then
    Result := Format('%.1f KiB', [AVal / 1024])
  else
    Result := Format('%.1f MiB', [AVal / (1024 * 1024)]);
end;

procedure TOBDXCPProgressBar.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, BarH: Integer;
  Track, Fill: TGPRectF;
  Caption, ProgText, EtaText: string;
  Frac: Single;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);

  // Caption row.
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FLabel <> '' then Caption := FLabel else Caption := 'XCP';
  ACanvas.TextOut(Pad, Pad, Caption);

  // Bar.
  BarH := ScaleValue(12);
  Track.X := Pad;
  Track.Y := Pad + ACanvas.TextHeight('Mg') + ScaleValue(4);
  Track.Width  := Width - 2 * Pad;
  Track.Height := BarH;

  if FBytesTotal > 0 then
    Frac := System.Math.Min(FBytesDone / FBytesTotal, 1.0)
  else
    Frac := 0;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;
    Fill := Track;
    Fill.Width := Single(Frac) * Track.Width;
    Brush := TGPSolidBrush.Create(ColorToARGB(EffectiveAccent));
    try
      Graphics.FillRectangle(Brush, Fill);
    finally
      Brush.Free;
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

  // Progress + KB/s + ETA.
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveForeground;
  ProgText := Format('%s / %s   %.1f KB/s',
    [FormatBytes(FBytesDone), FormatBytes(FBytesTotal),
     FKBPerSecond]);
  if ETASeconds > 0 then
    EtaText := Format('eta %.0f s', [ETASeconds])
  else
    EtaText := '';

  ACanvas.TextOut(Pad,
    Round(Track.Y + Track.Height + ScaleValue(4)), ProgText);
  if EtaText <> '' then
    ACanvas.TextOut(
      Width - Pad - ACanvas.TextWidth(EtaText),
      Round(Track.Y + Track.Height + ScaleValue(4)), EtaText);
end;

{ ---- TOBDRecorderToolbar ------------------------------------------------- }

constructor TOBDRecorderToolbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 460;
  Height := 48;
  FRecording := False;
  FPaused    := False;
  FDurationMs := 0;
  FFrameCount := 0;
  FHoverButton := -1;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDRecorderToolbar.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDRecorderToolbar.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDRecorderToolbar.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDRecorderToolbar.SetRecording(AValue: Boolean);
begin
  if FRecording = AValue then Exit;
  FRecording := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRecorderToolbar.SetPaused(AValue: Boolean);
begin
  if FPaused = AValue then Exit;
  FPaused := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRecorderToolbar.SetDurationMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FDurationMs = AValue then Exit;
  FDurationMs := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRecorderToolbar.SetFrameCount(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FFrameCount = AValue then Exit;
  FFrameCount := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRecorderToolbar.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDRecorderToolbar.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDRecorderToolbar.LayoutButtons(out ABounds: TRect);
var
  Pad, BtnW, BtnH, Gap, X, Y: Integer;
  B: TOBDRecorderButton;
begin
  Pad := ScaleValue(6);
  Gap := ScaleValue(4);
  BtnW := ScaleValue(72);
  BtnH := Height - 2 * Pad;
  X := Pad;
  Y := Pad;
  for B := Low(TOBDRecorderButton) to High(TOBDRecorderButton) do
  begin
    FButtonRects[B] := Rect(X, Y, X + BtnW, Y + BtnH);
    X := X + BtnW + Gap;
  end;
  ABounds := Rect(X, Y, Width - Pad, Y + BtnH);
end;

function TOBDRecorderToolbar.ButtonAt(X, Y: Integer): Integer;
var
  B: TOBDRecorderButton;
begin
  Result := -1;
  for B := Low(TOBDRecorderButton) to High(TOBDRecorderButton) do
    if PtInRect(FButtonRects[B], Point(X, Y)) then
      Exit(Ord(B));
end;

function TOBDRecorderToolbar.ButtonCaption(
  AButton: TOBDRecorderButton): string;
begin
  case AButton of
    rbRecord: Result := 'Record';
    rbPause:  Result := 'Pause';
    rbStop:   Result := 'Stop';
    rbLoad:   Result := 'Load';
    rbReplay: Result := 'Replay';
  else
    Result := '';
  end;
end;

procedure TOBDRecorderToolbar.DrawButton(ACanvas: TCanvas;
  const ARect: TRect; const ACaption: string;
  AActive, AHover: Boolean; AAccent: TColor);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  R: TGPRectF;
  BodyCol: TColor;
  TextW, TextH: Integer;
begin
  R.X := ARect.Left;
  R.Y := ARect.Top;
  R.Width  := ARect.Width;
  R.Height := ARect.Height;
  if AActive then BodyCol := AAccent
  else if AHover then BodyCol := Palette.NeutralLight
  else BodyCol := Palette.NeutralLight;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(BodyCol));
  Pen   := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillRectangle(Brush, R);
    Graphics.DrawRectangle(Pen, R);
  finally
    Pen.Free;
    Brush.Free;
    Graphics.Free;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FValueFont;
  if AActive then ACanvas.Font.Color := clWhite
  else            ACanvas.Font.Color := EffectiveForeground;
  TextW := ACanvas.TextWidth(ACaption);
  TextH := ACanvas.TextHeight(ACaption);
  ACanvas.TextOut(
    ARect.Left + (ARect.Width  - TextW) div 2,
    ARect.Top  + (ARect.Height - TextH) div 2,
    ACaption);
end;

function TOBDRecorderToolbar.FormatDuration(AMs: Int64): string;
var
  Total, Hours, Mins, Secs: Int64;
begin
  Total := AMs div 1000;
  Hours := Total div 3600;
  Mins  := (Total mod 3600) div 60;
  Secs  := Total mod 60;
  Result := Format('%d:%2.2d:%2.2d', [Hours, Mins, Secs]);
end;

procedure TOBDRecorderToolbar.FireClick(AButton: TOBDRecorderButton);
begin
  if Assigned(FOnClick) then
    try
      FOnClick(Self, AButton);
    except
    end;
end;

procedure TOBDRecorderToolbar.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  Hit: Integer;
  Junk: TRect;
begin
  inherited;
  LayoutButtons(Junk);
  Hit := ButtonAt(X, Y);
  if Hit <> FHoverButton then
  begin
    FHoverButton := Hit;
    Repaint;
  end;
end;

procedure TOBDRecorderToolbar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Hit: Integer;
  Junk: TRect;
begin
  inherited;
  if Button <> mbLeft then Exit;
  LayoutButtons(Junk);
  Hit := ButtonAt(X, Y);
  if Hit >= 0 then
    FireClick(TOBDRecorderButton(Hit));
end;

procedure TOBDRecorderToolbar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverButton <> -1 then
  begin
    FHoverButton := -1;
    Repaint;
  end;
end;

procedure TOBDRecorderToolbar.PaintControl(ACanvas: TCanvas);
var
  RestBounds: TRect;
  B: TOBDRecorderButton;
  Active: Boolean;
  Accent: TColor;
  DurText, FrameText: string;
begin
  LayoutButtons(RestBounds);
  for B := Low(TOBDRecorderButton) to High(TOBDRecorderButton) do
  begin
    case B of
      rbRecord: Active := FRecording and not FPaused;
      rbPause:  Active := FRecording and FPaused;
    else        Active := False;
    end;
    if B = rbRecord then Accent := Palette.Danger
    else if B = rbReplay then Accent := Palette.Success
    else                 Accent := EffectiveAccent;
    DrawButton(ACanvas, FButtonRects[B], ButtonCaption(B),
      Active, FHoverButton = Ord(B), Accent);
  end;

  // Duration + frame badge in the rest bounds.
  if RestBounds.Width <= 0 then Exit;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := EffectiveForeground;
  DurText := FormatDuration(FDurationMs);
  ACanvas.TextOut(RestBounds.Left + ScaleValue(8),
    RestBounds.Top + (RestBounds.Height -
      ACanvas.TextHeight(DurText)) div 2 - ScaleValue(2),
    DurText);

  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  FrameText := Format('%d frames', [FFrameCount]);
  ACanvas.TextOut(RestBounds.Left + ScaleValue(8),
    RestBounds.Top + RestBounds.Height -
      ACanvas.TextHeight(FrameText) - ScaleValue(2),
    FrameText);
end;

end.
