//------------------------------------------------------------------------------
//  OBD.UI.Replay
//
//  EV / service / replay visuals:
//
//    TOBDChargePortIndicator   Plug icon + lock state + level
//                              (L1 / L2 / DCFC).
//    TOBDMaintenanceCard       Miles-since-service + due-in
//                              card.
//    TOBDServiceHistoryTimeline Chronological event list.
//    TOBDPlaybackScrubber      Timeline scrubber for .obdlog
//                              replay (paint-driven slider).
//    TOBDPlaybackTimeline      Visual track of events on a
//                              time axis.
//    TOBDFrameInspector        Single-frame detail panel in
//                              replay mode.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Replay;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  System.DateUtils,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>EV charging level.</summary>
  TOBDChargeLevel = (
    /// <summary>No charger connected.</summary>
    clNoneLevel,
    /// <summary>Level 1 (slow AC, 120 V).</summary>
    clL1,
    /// <summary>Level 2 (AC, 240 V).</summary>
    clL2,
    /// <summary>DC fast charging.</summary>
    clDCFC
  );

  /// <summary>Charge port indicator. Plug glyph + locked
  /// state + level chip.</summary>
  TOBDChargePortIndicator = class(TOBDCustomControl)
  strict private
    FConnected: Boolean;
    FLocked:    Boolean;
    FLevel:     TOBDChargeLevel;
    FFont:      TFont;
    procedure SetConnected(AValue: Boolean);
    procedure SetLocked(AValue: Boolean);
    procedure SetLevel(AValue: TOBDChargeLevel);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  LevelText: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>True when a charger is plugged in. Default
    /// False.</summary>
    property Connected: Boolean
      read FConnected write SetConnected default False;
    /// <summary>True when the connector is latched. Default
    /// False.</summary>
    property Locked: Boolean
      read FLocked write SetLocked default False;
    /// <summary>Active charge level (L1 / L2 / DCFC).
    /// Default <c>clNoneLevel</c>.</summary>
    property Level: TOBDChargeLevel
      read FLevel write SetLevel default clNoneLevel;
    /// <summary>Font used for the level chip caption.</summary>
    property LabelFont: TFont read FFont write SetFontA;
  end;

  /// <summary>Service-due card. Miles-since-service + miles-
  /// until-due readout with a green/amber/red band.</summary>
  TOBDMaintenanceCard = class(TOBDCustomControl)
  strict private
    FMilesSince: Integer;
    FMilesDueIn: Integer;
    FUnit:       string;
    FCaptionFont, FValueFont: TFont;
    procedure SetMilesSince(AValue: Integer);
    procedure SetMilesDueIn(AValue: Integer);
    procedure SetUnit(const AValue: string);
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Distance covered since the last service
    /// (units as per <see cref="Unit"/>). Default 0.</summary>
    property MilesSinceService: Integer
      read FMilesSince write SetMilesSince default 0;
    /// <summary>Distance remaining until the next service
    /// (units as per <see cref="Unit"/>). Default 0.</summary>
    property MilesUntilDue: Integer
      read FMilesDueIn write SetMilesDueIn default 0;
    /// <summary>Distance-unit suffix (e.g. "mi", "km").
    /// </summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Font for the metric captions.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Font for the metric values.</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

  /// <summary>Service-history timeline (TListView). Host
  /// appends event rows; the visual is read-only.</summary>
  TOBDServiceHistoryTimeline = class(TListView)
  strict private
    procedure NotifyBindings;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Append a single service-history row.</summary>
    procedure AppendEvent(ADate: TDateTime;
      const AKind, ADescription: string);
    /// <summary>Remove all rows.</summary>
    procedure ClearEvents;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>Fires when the user drags the scrubber.</summary>
  /// <param name="Sender">The scrubber raising the event.</param>
  /// <param name="APositionMs">New playback position in
  /// milliseconds.</param>
  TOBDScrubEvent = procedure(Sender: TObject;
    APositionMs: Int64) of object;

  /// <summary>Horizontal timeline scrubber. Drag the thumb to
  /// scrub through a recorded log.</summary>
  TOBDPlaybackScrubber = class(TOBDCustomControl)
  strict private
    FPositionMs: Int64;
    FLengthMs:   Int64;
    FDragging:   Boolean;
    FOnScrub:    TOBDScrubEvent;
    procedure SetPositionMs(AValue: Int64);
    procedure SetLengthMs(AValue: Int64);
    procedure NotifyBindings;
    function  XToPosition(X: Integer): Int64;
    function  PositionToX(APos: Int64): Integer;
  protected
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Current scrub position in milliseconds.
    /// Clamped to 0..LengthMs. Default 0.</summary>
    property PositionMs: Int64
      read FPositionMs write SetPositionMs default 0;
    /// <summary>Total log length in milliseconds. Default 0.
    /// </summary>
    property LengthMs: Int64
      read FLengthMs write SetLengthMs default 0;
    /// <summary>Fires while the user drags the thumb.</summary>
    property OnScrub: TOBDScrubEvent
      read FOnScrub write FOnScrub;
  end;

  /// <summary>One event marker on the playback timeline.
  /// </summary>
  TOBDPlaybackMarker = record
    TimeMs: Int64;
    Color:  TColor;
    Label_: string;
  end;

  /// <summary>Visual playback timeline. Host pushes coloured
  /// markers; the visual paints them along the time axis.
  /// </summary>
  TOBDPlaybackTimeline = class(TOBDCustomControl)
  strict private
    FMarkers:  TList<TOBDPlaybackMarker>;
    FLengthMs: Int64;
    procedure SetLengthMs(AValue: Int64);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Append a coloured marker at the given time
    /// (milliseconds from the start of the log).</summary>
    procedure AddMarker(ATimeMs: Int64; AColor: TColor;
      const ALabel: string);
    /// <summary>Drop all markers and repaint.</summary>
    procedure Clear;
    /// <summary>Number of markers currently stored.</summary>
    function  MarkerCount: Integer;
  published
    /// <summary>Total timeline length in milliseconds.
    /// Default 0.</summary>
    property LengthMs: Int64
      read FLengthMs write SetLengthMs default 0;
  end;

  /// <summary>Frame-inspector panel. Drops on a form, the host
  /// pushes one log entry record; the panel renders a
  /// structured view of the frame.</summary>
  TOBDFrameInspector = class(TOBDCustomControl)
  strict private
    FTimestamp: TDateTime;
    FKindText:  string;
    FServiceID: Byte;
    FHasServiceID: Boolean;
    FRawHex:    string;
    FMessage:   string;
    FCaptionFont, FValueFont: TFont;
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Loads one frame for display.</summary>
    procedure LoadFrame(const ATimestamp: TDateTime;
      const AKind: string;
      AHasServiceID: Boolean; AServiceID: Byte;
      const ARaw: TBytes; const AMessage: string);
    /// <summary>Drop the loaded frame and repaint blank.
    /// </summary>
    procedure Clear;
  published
    /// <summary>Font for field captions.</summary>
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    /// <summary>Font for field values.</summary>
    property ValueFont: TFont
      read FValueFont write SetValueFont;
  end;

implementation

function HexBytes(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then Result := Result + ' ';
    Result := Result + Format('%2.2X', [ABytes[I]]);
  end;
end;

{ ---- TOBDChargePortIndicator ----------------------------------------- }

constructor TOBDChargePortIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 160;
  Height := 60;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 10;
  FFont.Style := [fsBold];
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDChargePortIndicator.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDChargePortIndicator.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDChargePortIndicator.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDChargePortIndicator.SetConnected(AValue: Boolean);
begin
  if FConnected = AValue then Exit;
  FConnected := AValue; NotifyBindings; Repaint;
end;

procedure TOBDChargePortIndicator.SetLocked(AValue: Boolean);
begin
  if FLocked = AValue then Exit;
  FLocked := AValue; NotifyBindings; Repaint;
end;

procedure TOBDChargePortIndicator.SetLevel(AValue: TOBDChargeLevel);
begin
  if FLevel = AValue then Exit;
  FLevel := AValue; NotifyBindings; Repaint;
end;

procedure TOBDChargePortIndicator.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TOBDChargePortIndicator.LevelText: string;
begin
  case FLevel of
    clL1:        Result := 'L1';
    clL2:        Result := 'L2';
    clDCFC:      Result := 'DCFC';
  else
    Result := '—';
  end;
end;

procedure TOBDChargePortIndicator.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad, IconSize: Integer;
  Lamp: TGPRectF;
  Col: TColor;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  IconSize := System.Math.Min(Height - 2 * Pad, ScaleValue(36));

  if not FConnected then Col := Palette.NeutralLight
  else if FLocked then Col := Palette.Success
  else                 Col := Palette.Warning;

  Lamp.X := Pad;
  Lamp.Y := (Height - IconSize) / 2;
  Lamp.Width  := IconSize;
  Lamp.Height := IconSize;
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillEllipse(Brush, Lamp);
  finally
    Brush.Free;
    Graphics.Free;
  end;

  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FConnected then
    ACanvas.TextOut(Pad + IconSize + ScaleValue(10),
      Pad, 'connected')
  else
    ACanvas.TextOut(Pad + IconSize + ScaleValue(10),
      Pad, 'unplugged');
  ACanvas.Font.Color := EffectiveAccent;
  ACanvas.TextOut(Pad + IconSize + ScaleValue(10),
    Pad + ACanvas.TextHeight('Mg') + ScaleValue(2),
    LevelText);
end;

{ ---- TOBDMaintenanceCard --------------------------------------------- }

constructor TOBDMaintenanceCard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 280;
  Height := 100;
  FUnit := 'km';
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 16;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDMaintenanceCard.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDMaintenanceCard.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDMaintenanceCard.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDMaintenanceCard.SetMilesSince(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FMilesSince = AValue then Exit;
  FMilesSince := AValue; NotifyBindings; Repaint;
end;

procedure TOBDMaintenanceCard.SetMilesDueIn(AValue: Integer);
begin
  if FMilesDueIn = AValue then Exit;
  FMilesDueIn := AValue; NotifyBindings; Repaint;
end;

procedure TOBDMaintenanceCard.SetUnit(const AValue: string);
begin
  if FUnit = AValue then Exit;
  FUnit := AValue; Repaint;
end;

procedure TOBDMaintenanceCard.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDMaintenanceCard.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDMaintenanceCard.PaintControl(ACanvas: TCanvas);
var
  Pad: Integer;
  Col: TColor;
  Status: string;
begin
  Pad := ScaleValue(10);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FCaptionFont;
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(Pad, Pad,
    Format('since service: %d %s', [FMilesSince, FUnit]));

  if FMilesDueIn <= 0 then
  begin
    Col := Palette.Danger;
    Status := Format('overdue by %d %s',
      [Abs(FMilesDueIn), FUnit]);
  end
  else if FMilesDueIn < 1000 then
  begin
    Col := Palette.Warning;
    Status := Format('due in %d %s', [FMilesDueIn, FUnit]);
  end
  else
  begin
    Col := Palette.Success;
    Status := Format('due in %d %s', [FMilesDueIn, FUnit]);
  end;
  ACanvas.Font := FValueFont;
  ACanvas.Font.Color := Col;
  ACanvas.TextOut(Pad,
    Pad + ACanvas.TextHeight('Mg') + ScaleValue(2), Status);
end;

{ ---- TOBDServiceHistoryTimeline ------------------------------------- }

constructor TOBDServiceHistoryTimeline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDServiceHistoryTimeline.CreateWnd;
var
  C: TListColumn;
begin
  inherited;
  if Columns.Count = 0 then
  begin
    Columns.BeginUpdate;
    try
      C := Columns.Add; C.Caption := 'Date';        C.Width := 100;
      C := Columns.Add; C.Caption := 'Kind';        C.Width := 100;
      C := Columns.Add; C.Caption := 'Description'; C.Width := 300;
    finally
      Columns.EndUpdate;
    end;
  end;
end;

procedure TOBDServiceHistoryTimeline.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDServiceHistoryTimeline.AppendEvent(ADate: TDateTime;
  const AKind, ADescription: string);
var
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Item := Items.Add;
    Item.Caption := FormatDateTime('yyyy-mm-dd', ADate);
    Item.SubItems.Add(AKind);
    Item.SubItems.Add(ADescription);
  finally
    Items.EndUpdate;
  end;
  NotifyBindings;
end;

procedure TOBDServiceHistoryTimeline.ClearEvents;
begin
  Items.Clear;
  NotifyBindings;
end;

{ ---- TOBDPlaybackScrubber -------------------------------------------- }

constructor TOBDPlaybackScrubber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 28;
end;

procedure TOBDPlaybackScrubber.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPlaybackScrubber.SetPositionMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if AValue > FLengthMs then AValue := FLengthMs;
  if FPositionMs = AValue then Exit;
  FPositionMs := AValue; NotifyBindings; Repaint;
end;

procedure TOBDPlaybackScrubber.SetLengthMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FLengthMs = AValue then Exit;
  FLengthMs := AValue;
  if FPositionMs > FLengthMs then FPositionMs := FLengthMs;
  Repaint;
end;

function TOBDPlaybackScrubber.XToPosition(X: Integer): Int64;
var
  Pad: Integer;
  PlotW: Integer;
begin
  Pad := ScaleValue(8);
  PlotW := Width - 2 * Pad;
  if (PlotW <= 0) or (FLengthMs <= 0) then Exit(0);
  if X < Pad then X := Pad;
  if X > Pad + PlotW then X := Pad + PlotW;
  Result := Round((X - Pad) / PlotW * FLengthMs);
end;

function TOBDPlaybackScrubber.PositionToX(APos: Int64): Integer;
var
  Pad: Integer;
  PlotW: Integer;
begin
  Pad := ScaleValue(8);
  PlotW := Width - 2 * Pad;
  if FLengthMs <= 0 then Exit(Pad);
  Result := Pad + Round(APos / FLengthMs * PlotW);
end;

procedure TOBDPlaybackScrubber.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    FDragging := True;
    SetPositionMs(XToPosition(X));
    if Assigned(FOnScrub) then
      try
        FOnScrub(Self, FPositionMs);
      except
      end;
  end;
end;

procedure TOBDPlaybackScrubber.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FDragging then
  begin
    SetPositionMs(XToPosition(X));
    if Assigned(FOnScrub) then
      try
        FOnScrub(Self, FPositionMs);
      except
      end;
  end;
end;

procedure TOBDPlaybackScrubber.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then FDragging := False;
end;

procedure TOBDPlaybackScrubber.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad: Integer;
  Track: TGPRectF;
  ThumbX: Integer;
begin
  Pad := ScaleValue(8);
  Track.X := Pad;
  Track.Y := (Height - ScaleValue(6)) / 2;
  Track.Width := Width - 2 * Pad;
  Track.Height := ScaleValue(6);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Brush := TGPSolidBrush.Create(ColorToARGB(Palette.NeutralLight));
    try
      Graphics.FillRectangle(Brush, Track);
    finally
      Brush.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen, Track);
    finally
      Pen.Free;
    end;
    ThumbX := PositionToX(FPositionMs);
    Brush := TGPSolidBrush.Create(ColorToARGB(EffectiveAccent));
    try
      Graphics.FillEllipse(Brush,
        ThumbX - ScaleValue(7),
        (Height - ScaleValue(14)) / 2,
        ScaleValue(14), ScaleValue(14));
    finally
      Brush.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDPlaybackTimeline -------------------------------------------- }

constructor TOBDPlaybackTimeline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 30;
  FMarkers := TList<TOBDPlaybackMarker>.Create;
end;

destructor TOBDPlaybackTimeline.Destroy;
begin
  FMarkers.Free;
  inherited;
end;

procedure TOBDPlaybackTimeline.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPlaybackTimeline.SetLengthMs(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FLengthMs = AValue then Exit;
  FLengthMs := AValue; Repaint;
end;

procedure TOBDPlaybackTimeline.AddMarker(ATimeMs: Int64;
  AColor: TColor; const ALabel: string);
var
  M: TOBDPlaybackMarker;
begin
  M.TimeMs := ATimeMs;
  M.Color  := AColor;
  M.Label_ := ALabel;
  FMarkers.Add(M);
  NotifyBindings;
  Repaint;
end;

procedure TOBDPlaybackTimeline.Clear;
begin
  FMarkers.Clear;
  NotifyBindings;
  Repaint;
end;

function TOBDPlaybackTimeline.MarkerCount: Integer;
begin
  Result := FMarkers.Count;
end;

procedure TOBDPlaybackTimeline.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pad: Integer;
  PlotW: Integer;
  I: Integer;
  X: Single;
begin
  if FLengthMs <= 0 then Exit;
  Pad := ScaleValue(4);
  PlotW := Width - 2 * Pad;
  if PlotW <= 0 then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to FMarkers.Count - 1 do
    begin
      X := Pad + FMarkers[I].TimeMs / FLengthMs * PlotW;
      Brush := TGPSolidBrush.Create(
        ColorToARGB(FMarkers[I].Color));
      try
        Graphics.FillRectangle(Brush,
          X - 1, Single(Pad),
          2, Height - 2 * Pad);
      finally
        Brush.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDFrameInspector --------------------------------------------- }

constructor TOBDFrameInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 180;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Consolas';
  FValueFont.Size := 9;
  FValueFont.OnChange := HandleFontChange;
end;

destructor TOBDFrameInspector.Destroy;
begin
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDFrameInspector.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFrameInspector.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDFrameInspector.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDFrameInspector.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDFrameInspector.LoadFrame(const ATimestamp: TDateTime;
  const AKind: string; AHasServiceID: Boolean;
  AServiceID: Byte; const ARaw: TBytes; const AMessage: string);
begin
  FTimestamp     := ATimestamp;
  FKindText      := AKind;
  FHasServiceID  := AHasServiceID;
  FServiceID     := AServiceID;
  FRawHex        := HexBytes(ARaw);
  FMessage       := AMessage;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFrameInspector.Clear;
begin
  FTimestamp := 0;
  FKindText := '';
  FRawHex   := '';
  FMessage  := '';
  FHasServiceID := False;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFrameInspector.PaintControl(ACanvas: TCanvas);
var
  Pad, Y: Integer;

  procedure Row(const ALabel, AVal: string);
  begin
    if AVal = '' then Exit;
    ACanvas.Font := FCaptionFont;
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font := FValueFont;
    ACanvas.Font.Color := EffectiveAccent;
    ACanvas.TextOut(Pad + ScaleValue(80), Y, AVal);
    Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(4));
  end;

begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  Y := Pad;
  if FKindText = '' then Exit;
  Row('Time',
    FormatDateTime('hh:nn:ss.zzz', FTimestamp));
  Row('Kind', FKindText);
  if FHasServiceID then
    Row('Service', Format('0x%2.2X', [FServiceID]));
  Row('Raw',     FRawHex);
  Row('Message', FMessage);
end;

end.
