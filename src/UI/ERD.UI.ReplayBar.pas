//------------------------------------------------------------------------------
//  ERD.UI.ReplayBar
//
//  TOBDReplayBar - recording playback bar with transport buttons, scrubber,
//  markers, time read-out and speed selection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit ERD.UI.ReplayBar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Marker colour and meaning on the replay timeline.</summary>
  TOBDReplayMarkerKind = (
    /// <summary>Technician note marker.</summary>
    rpbNote,
    /// <summary>Bookmark marker.</summary>
    rpbBookmark,
    /// <summary>Diagnostic trouble code marker.</summary>
    rpbDtc);

  /// <summary>Event fired when the replay position changes.</summary>
  TOBDReplaySeekEvent = procedure(Sender: TObject; APositionMs: Int64) of object;

  /// <summary>Event fired when replay speed changes.</summary>
  TOBDReplaySpeedEvent = procedure(Sender: TObject; ASpeed: Double) of object;

  /// <summary>One marker drawn on the timeline.</summary>
  TOBDReplayMarkerItem = class(TCollectionItem)
  strict private
    FPositionMs: Int64;
    FKind: TOBDReplayMarkerKind;
    FCaption: string;
    procedure SetPositionMs(AValue: Int64);
    procedure SetKind(AValue: TOBDReplayMarkerKind);
    procedure SetCaption(const AValue: string);
  protected
    /// <summary>Returns the marker caption in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a bookmark marker.</summary>
    constructor Create(Collection: TCollection); override;
  published
    /// <summary>Marker position in milliseconds.</summary>
    property PositionMs: Int64 read FPositionMs write SetPositionMs default 0;
    /// <summary>Marker kind.</summary>
    property Kind: TOBDReplayMarkerKind read FKind write SetKind
      default rpbBookmark;
    /// <summary>Short marker caption.</summary>
    property Caption: string read FCaption write SetCaption;
  end;

  /// <summary>Collection of replay markers.</summary>
  TOBDReplayMarkerCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDReplayMarkerItem;
    procedure SetItem(Index: Integer; AValue: TOBDReplayMarkerItem);
  protected
    /// <summary>Notifies the owner after a marker changes.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a marker collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a marker.</summary>
    /// <returns>New marker item.</returns>
    function Add: TOBDReplayMarkerItem;
    /// <summary>Indexed marker items.</summary>
    property Items[Index: Integer]: TOBDReplayMarkerItem read GetItem
      write SetItem; default;
    /// <summary>Fires after markers change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Recording replay transport bar.</summary>
  TOBDReplayBar = class(TOBDCustomControl, IOBDSurface)
  strict private
    FFileName: string;
    FInfoText: string;
    FPositionMs: Int64;
    FDurationMs: Int64;
    FPlaying: Boolean;
    FSpeed: Double;
    FMarkers: TOBDReplayMarkerCollection;
    FHoverButton: Integer;
    FDownButton: Integer;
    FDraggingSlider: Boolean;
    FOnPlay: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnSeek: TOBDReplaySeekEvent;
    FOnSpeedChange: TOBDReplaySpeedEvent;
    procedure SetFileName(const AValue: string);
    procedure SetInfoText(const AValue: string);
    procedure SetPositionMs(AValue: Int64);
    procedure SetDurationMs(AValue: Int64);
    procedure SetPlaying(AValue: Boolean);
    procedure SetSpeed(AValue: Double);
    procedure SetMarkers(AValue: TOBDReplayMarkerCollection);
    procedure MarkersChanged(Sender: TObject);
    function ButtonSize: Integer;
    function ButtonRect(AIndex: Integer): TRect;
    function SliderRect: TRect;
    function SpeedRect(AIndex: Integer): TRect;
    function MarkerColor(APainter: TOBDPainter; AKind: TOBDReplayMarkerKind)
      : TColor;
    function PositionFromX(X: Integer): Int64;
    procedure DoPlay;
    procedure DoPause;
    procedure DoSeek;
    procedure DoSpeedChange;
    procedure DrawTransport(APainter: TOBDPainter);
    procedure DrawFile(APainter: TOBDPainter);
    procedure DrawSlider(APainter: TOBDPainter);
    procedure DrawSpeed(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the replay bar.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles transport, slider and speed clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Updates hover state and slider dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Finishes clicks and slider dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Keyboard transport and seeking.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a 900 x 88 replay bar.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the marker collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Starts playback and fires OnPlay.</summary>
    procedure Play;
    /// <summary>Pauses playback and fires OnPause.</summary>
    procedure Pause;
    /// <summary>Moves the playhead and fires OnSeek.</summary>
    /// <param name="APositionMs">New position in milliseconds.</param>
    procedure Seek(APositionMs: Int64);
    /// <summary>Adds a marker to the timeline.</summary>
    /// <param name="APositionMs">Marker position.</param>
    /// <param name="AKind">Marker kind.</param>
    /// <param name="ACaption">Caption.</param>
    /// <returns>Created marker.</returns>
    function AddMarker(APositionMs: Int64; AKind: TOBDReplayMarkerKind;
      const ACaption: string): TOBDReplayMarkerItem;
    /// <summary>Clears all markers.</summary>
    procedure ClearMarkers;
    /// <summary>Formats a millisecond value as m:ss.</summary>
    /// <param name="AMs">Milliseconds.</param>
    /// <returns>Time text.</returns>
    function FormatTime(AMs: Int64): string;
  published
    /// <summary>Recording file name.</summary>
    property FileName: string read FFileName write SetFileName;
    /// <summary>Secondary recording information.</summary>
    property InfoText: string read FInfoText write SetInfoText;
    /// <summary>Current playback position in milliseconds.</summary>
    property PositionMs: Int64 read FPositionMs write SetPositionMs default 0;
    /// <summary>Recording duration in milliseconds.</summary>
    property DurationMs: Int64 read FDurationMs write SetDurationMs default 0;
    /// <summary>True while playback is active.</summary>
    property Playing: Boolean read FPlaying write SetPlaying default False;
    /// <summary>Playback speed multiplier.</summary>
    property Speed: Double read FSpeed write SetSpeed;
    /// <summary>Timeline markers.</summary>
    property Markers: TOBDReplayMarkerCollection read FMarkers write SetMarkers;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes density from the current theme.</summary>
    property ParentDensity;
    /// <summary>Fires after playback starts.</summary>
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    /// <summary>Fires after playback pauses.</summary>
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    /// <summary>Fires after seeking.</summary>
    property OnSeek: TOBDReplaySeekEvent read FOnSeek write FOnSeek;
    /// <summary>Fires after the speed changes.</summary>
    property OnSpeedChange: TOBDReplaySpeedEvent read FOnSpeedChange
      write FOnSpeedChange;
  end;

implementation

{ TOBDReplayMarkerItem }

constructor TOBDReplayMarkerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FKind := rpbBookmark;
end;

function TOBDReplayMarkerItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDReplayMarkerItem.SetPositionMs(AValue: Int64);
begin
  if AValue < 0 then
    AValue := 0;
  if FPositionMs = AValue then
    Exit;
  FPositionMs := AValue;
  Changed(False);
end;

procedure TOBDReplayMarkerItem.SetKind(AValue: TOBDReplayMarkerKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TOBDReplayMarkerItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

{ TOBDReplayMarkerCollection }

constructor TOBDReplayMarkerCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDReplayMarkerItem);
end;

function TOBDReplayMarkerCollection.Add: TOBDReplayMarkerItem;
begin
  Result := TOBDReplayMarkerItem(inherited Add);
end;

function TOBDReplayMarkerCollection.GetItem(Index: Integer): TOBDReplayMarkerItem;
begin
  Result := TOBDReplayMarkerItem(inherited GetItem(Index));
end;

procedure TOBDReplayMarkerCollection.SetItem(Index: Integer;
  AValue: TOBDReplayMarkerItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDReplayMarkerCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDReplayBar }

constructor TOBDReplayBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := 'Golf_1.6TDI_2026-10-11_1842.obdrec';
  FInfoText := 'Recorded 18:42 ' + #$00B7 + ' 12:18 ' + #$00B7 +
    ' 41 signals ' + #$00B7 + ' CAN trace';
  FDurationMs := 0;
  FPositionMs := 0;
  FSpeed := 1.0;
  FHoverButton := -1;
  FDownButton := -1;
  FMarkers := TOBDReplayMarkerCollection.Create(Self);
  FMarkers.OnChange := MarkersChanged;
  Width := 900;
  Height := 88;
  TabStop := True;
end;

destructor TOBDReplayBar.Destroy;
begin
  FMarkers.Free;
  inherited;
end;

procedure TOBDReplayBar.DensityChanged;
begin
  inherited DensityChanged;
end;

function TOBDReplayBar.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
  if StyleBackground <> clDefault then
    Result := StyleBackground;
end;

procedure TOBDReplayBar.SetFileName(const AValue: string);
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
  Invalidate;
end;

procedure TOBDReplayBar.SetInfoText(const AValue: string);
begin
  if FInfoText = AValue then
    Exit;
  FInfoText := AValue;
  Invalidate;
end;

procedure TOBDReplayBar.SetPositionMs(AValue: Int64);
begin
  AValue := EnsureRange(AValue, 0, FDurationMs);
  if FPositionMs = AValue then
    Exit;
  FPositionMs := AValue;
  Invalidate;
end;

procedure TOBDReplayBar.SetDurationMs(AValue: Int64);
begin
  if AValue < 0 then
    AValue := 0;
  if FDurationMs = AValue then
    Exit;
  FDurationMs := AValue;
  SetPositionMs(FPositionMs);
  Invalidate;
end;

procedure TOBDReplayBar.SetPlaying(AValue: Boolean);
begin
  if FPlaying = AValue then
    Exit;
  FPlaying := AValue;
  if FPlaying then
    DoPlay
  else
    DoPause;
  Invalidate;
end;

procedure TOBDReplayBar.SetSpeed(AValue: Double);
begin
  if AValue <= 0 then
    AValue := 1.0;
  if Abs(FSpeed - AValue) < 0.0001 then
    Exit;
  FSpeed := AValue;
  DoSpeedChange;
  Invalidate;
end;

procedure TOBDReplayBar.SetMarkers(AValue: TOBDReplayMarkerCollection);
begin
  FMarkers.Assign(AValue);
end;

procedure TOBDReplayBar.MarkersChanged(Sender: TObject);
begin
  Invalidate;
end;

function TOBDReplayBar.ButtonSize: Integer;
begin
  Result := ScaleValue(Metrics.Button);
end;

function TOBDReplayBar.ButtonRect(AIndex: Integer): TRect;
var
  S, X, Y: Integer;
begin
  S := ButtonSize;
  X := ScaleValue(16) + AIndex * (S + ScaleValue(6));
  Y := ClientHeight div 2 - S div 2;
  Result := Rect(X, Y, X + S, Y + S);
end;

function TOBDReplayBar.SliderRect: TRect;
var
  LeftX, RightX, Mid: Integer;
begin
  LeftX := ScaleValue(16) + 4 * (ButtonSize + ScaleValue(6)) + ScaleValue(16) +
    ScaleValue(320);
  RightX := ClientWidth - ScaleValue(330);
  Mid := ClientHeight div 2;
  if RightX < LeftX + ScaleValue(80) then
    RightX := LeftX + ScaleValue(80);
  Result := Rect(LeftX, Mid - ScaleValue(16), RightX, Mid + ScaleValue(26));
end;

function TOBDReplayBar.SpeedRect(AIndex: Integer): TRect;
var
  W, H, X, Y: Integer;
begin
  W := ScaleValue(44);
  H := ScaleValue(Metrics.Segment);
  X := ClientWidth - ScaleValue(16) - W * 4 + AIndex * W;
  Y := ClientHeight div 2 - H div 2;
  Result := Rect(X, Y, X + W, Y + H);
end;

function TOBDReplayBar.MarkerColor(APainter: TOBDPainter;
  AKind: TOBDReplayMarkerKind): TColor;
begin
  case AKind of
    rpbNote:
      Result := Palette.Warning;
    rpbDtc:
      Result := Palette.Danger;
  else
    Result := Palette.Subtle;
  end;
end;

function TOBDReplayBar.PositionFromX(X: Integer): Int64;
var
  R: TRect;
  F: Double;
begin
  R := SliderRect;
  if R.Width <= 0 then
    Exit(0);
  F := EnsureRange((X - R.Left) / R.Width, 0.0, 1.0);
  Result := Round(F * FDurationMs);
end;

procedure TOBDReplayBar.DoPlay;
begin
  if Assigned(FOnPlay) then
    FOnPlay(Self);
end;

procedure TOBDReplayBar.DoPause;
begin
  if Assigned(FOnPause) then
    FOnPause(Self);
end;

procedure TOBDReplayBar.DoSeek;
begin
  if Assigned(FOnSeek) then
    FOnSeek(Self, FPositionMs);
end;

procedure TOBDReplayBar.DoSpeedChange;
begin
  if Assigned(FOnSpeedChange) then
    FOnSpeedChange(Self, FSpeed);
end;

procedure TOBDReplayBar.Play;
begin
  Playing := True;
end;

procedure TOBDReplayBar.Pause;
begin
  Playing := False;
end;

procedure TOBDReplayBar.Seek(APositionMs: Int64);
begin
  SetPositionMs(APositionMs);
  DoSeek;
end;

function TOBDReplayBar.AddMarker(APositionMs: Int64; AKind: TOBDReplayMarkerKind;
  const ACaption: string): TOBDReplayMarkerItem;
begin
  Result := FMarkers.Add;
  Result.PositionMs := APositionMs;
  Result.Kind := AKind;
  Result.Caption := ACaption;
end;

procedure TOBDReplayBar.ClearMarkers;
begin
  FMarkers.Clear;
end;

function TOBDReplayBar.FormatTime(AMs: Int64): string;
var
  S: Int64;
begin
  S := AMs div 1000;
  Result := Format('%d:%.2d', [S div 60, S mod 60]);
end;

procedure TOBDReplayBar.DrawTransport(APainter: TOBDPainter);
var
  I: Integer;
  R: TRect;
  Glyph: TOBDGlyph;
  Fill, Ink: TColor;
begin
  for I := 0 to 3 do
  begin
    R := ButtonRect(I);
    if I = 1 then
      Fill := Palette.Accent
    else
      Fill := Palette.GaugeFace;
    if I = 1 then
      Ink := APainter.OnAccent
    else
      Ink := Palette.ForegroundText;
    if (I = FHoverButton) and (I <> 1) then
      Fill := OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06);
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(6), Fill,
      IfThen(I = 1, clNone, Palette.NeutralLight));
    case I of
      0:
        Glyph := glBack;
      1:
        if FPlaying then
          Glyph := glStop
        else
          Glyph := glPlay;
      2:
        Glyph := glStop;
    else
      Glyph := glForward;
    end;
    APainter.Glyph(Glyph, R.Left + R.Width div 2, R.Top + R.Height div 2,
      Ink, 1);
  end;
end;

procedure TOBDReplayBar.DrawFile(APainter: TOBDPainter);
var
  X, Mid: Integer;
begin
  X := ScaleValue(16) + 4 * (ButtonSize + ScaleValue(6)) + ScaleValue(16);
  Mid := ClientHeight div 2;
  APainter.Glyph(glReplay, X + ScaleValue(8), Mid - ScaleValue(14),
    APainter.AccentText, 1);
  APainter.Text(X + ScaleValue(24), Mid - ScaleValue(14), FFileName,
    OBD_TEXT_BODY, Palette.ForegroundText, twSemibold, taLeftJustify,
    SliderRect.Left - X - ScaleValue(36));
  APainter.Text(X + ScaleValue(24), Mid + ScaleValue(12), FInfoText,
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
    SliderRect.Left - X - ScaleValue(36));
end;

procedure TOBDReplayBar.DrawSlider(APainter: TOBDPainter);
var
  R: TRect;
  I, X, Mid, W: Integer;
  F: Double;
begin
  R := SliderRect;
  Mid := ClientHeight div 2;
  APainter.RoundRect(R.Left, Mid - ScaleValue(3), R.Width, ScaleValue(6),
    ScaleValue(3), Palette.NeutralLight, clNone);
  if FDurationMs > 0 then
    F := FPositionMs / FDurationMs
  else
    F := 0;
  W := Round(R.Width * F);
  APainter.RoundRect(R.Left, Mid - ScaleValue(3), W, ScaleValue(6),
    ScaleValue(3), Palette.Accent, clNone);
  for I := 0 to FMarkers.Count - 1 do
    if FDurationMs > 0 then
    begin
      X := R.Left + Round(R.Width * EnsureRange(FMarkers[I].PositionMs /
        FDurationMs, 0.0, 1.0));
      APainter.FillRect(Rect(X - ScaleValue(1), Mid - ScaleValue(16),
        X + ScaleValue(2), Mid - ScaleValue(7)),
        MarkerColor(APainter, FMarkers[I].Kind));
    end;
  X := R.Left + W;
  APainter.Ellipse(X - ScaleValue(8), Mid - ScaleValue(8), ScaleValue(16),
    ScaleValue(16), Palette.GaugeFace, APainter.AccentText, 2);
  APainter.Text(R.Left, Mid + ScaleValue(20), FormatTime(FPositionMs) + ' / ' +
    FormatTime(FDurationMs), OBD_TEXT_SMALL, Palette.ForegroundText,
    twSemibold);
  APainter.Text(R.Right, Mid + ScaleValue(20), 'Note    Bookmark    DTC',
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taRightJustify);
end;

procedure TOBDReplayBar.DrawSpeed(APainter: TOBDPainter);
const
  TEXTS: array[0..3] of string = ('0.5x', '1x', '2x', '4x');
  SPEEDS: array[0..3] of Double = (0.5, 1.0, 2.0, 4.0);
var
  I: Integer;
  R: TRect;
  Kind: TOBDButtonKind;
begin
  APainter.Caps(SpeedRect(0).Left - ScaleValue(20), ClientHeight div 2,
    'Speed', taRightJustify);
  for I := 0 to 3 do
  begin
    R := SpeedRect(I);
    if Abs(FSpeed - SPEEDS[I]) < 0.01 then
      Kind := bkPrimary
    else
      Kind := bkSecondary;
    APainter.Button(R, TEXTS[I], Kind, cstNormal, False, glNone);
  end;
end;

procedure TOBDReplayBar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawTransport(P);
    DrawFile(P);
    DrawSlider(P);
    DrawSpeed(P);
  finally
    P.Free;
  end;
end;

procedure TOBDReplayBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  for I := 0 to 3 do
    if PtInRect(ButtonRect(I), Point(X, Y)) then
    begin
      FDownButton := I;
      Invalidate;
      Exit;
    end;
  if PtInRect(SliderRect, Point(X, Y)) then
  begin
    FDraggingSlider := True;
    Seek(PositionFromX(X));
    Invalidate;
    Exit;
  end;
  for I := 0 to 3 do
    if PtInRect(SpeedRect(I), Point(X, Y)) then
    begin
      case I of
        0:
          Speed := 0.5;
        2:
          Speed := 2.0;
        3:
          Speed := 4.0;
      else
        Speed := 1.0;
      end;
      Exit;
    end;
end;

procedure TOBDReplayBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if FDraggingSlider then
  begin
    Seek(PositionFromX(X));
    Exit;
  end;
  FHoverButton := -1;
  for I := 0 to 3 do
    if PtInRect(ButtonRect(I), Point(X, Y)) then
      FHoverButton := I;
  if (FHoverButton >= 0) or PtInRect(SliderRect, Point(X, Y)) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDReplayBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FDraggingSlider then
  begin
    FDraggingSlider := False;
    Exit;
  end;
  if (FDownButton >= 0) and PtInRect(ButtonRect(FDownButton), Point(X, Y)) then
    case FDownButton of
      0:
        Seek(System.Math.Max(0, FPositionMs - 1000));
      1:
        Playing := not Playing;
      2:
        Pause;
      3:
        Seek(System.Math.Min(FDurationMs, FPositionMs + 1000));
    end;
  FDownButton := -1;
  Invalidate;
end;

procedure TOBDReplayBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_SPACE:
      Playing := not Playing;
    VK_LEFT:
      Seek(FPositionMs - 1000);
    VK_RIGHT:
      Seek(FPositionMs + 1000);
    VK_HOME:
      Seek(0);
    VK_END:
      Seek(FDurationMs);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDReplayBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverButton := -1;
  FDownButton := -1;
  Cursor := crDefault;
  Invalidate;
end;

end.
