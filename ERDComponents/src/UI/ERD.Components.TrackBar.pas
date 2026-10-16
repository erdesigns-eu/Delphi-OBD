//------------------------------------------------------------------------------
//  ERD.Components.TrackBar
//
//  TERDTrackBar - themed horizontal slider for ranges such as refresh
//  rates, alarm thresholds or chart time windows.
//
//  The track fills in orange up to a round thumb. Click anywhere on the
//  track to jump there and keep dragging; arrow keys step by SmallChange,
//  Page Up / Page Down by LargeChange, Home / End go to the ends and the
//  mouse wheel steps while the slider has focus. Optional tick marks
//  (Frequency) and a value read-out on the right (ShowValue, Suffix).
//  The thumb and the height follow the density so it is finger friendly
//  on a tablet.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.TrackBar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.Components.Types,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>Themed horizontal slider.</summary>
  TERDTrackOrientation = (toHorizontal, toVertical);

  TERDTrackBar = class(TERDCustomControl)
  strict private
    FOrientation: TERDTrackOrientation;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FFrequency: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FShowValue: Boolean;
    FSuffix: string;
    FHot: Boolean;
    FDragging: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetOrientation(Value: TERDTrackOrientation);
    function MousePosition(X, Y: Integer): Integer;
    procedure SetFrequency(AValue: Integer);
    procedure SetShowValue(AValue: Boolean);
    procedure SetSuffix(const AValue: string);
  protected
    procedure SetMin(AValue: Integer); virtual;
    procedure SetMax(AValue: Integer); virtual;
    procedure SetPosition(AValue: Integer); virtual;
    procedure DragTo(Value: Integer); virtual;
    procedure PaintTrackExtras(P: TERDPainter; Center, StartPos, EndPos, ThumbPos: Integer); virtual;
    procedure PaintThumb(P: TERDPainter; CX, CY: Single; Active: Boolean = True);
    function ThumbSize: Integer;
    function ValueWidth: Integer; virtual;
    function TrackLeft: Integer;
    function TrackRight: Integer;
    function PositionToX(APosition: Integer): Integer;
    function XToPosition(X: Integer): Integer;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the track, ticks, thumb and value.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Height follows the density.</summary>
    /// <param name="NewWidth">Proposed width.</param>
    /// <param name="NewHeight">Proposed height; set to the density height.
    /// </param>
    /// <returns>Always True.</returns>
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    /// <summary>Jumps to the clicked position and starts dragging.
    /// </summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Follows the mouse while dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Arrow, page, Home and End keys.</summary>
    /// <param name="Key">Virtual key; 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Steps once per notch while focused.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when the position stepped.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Repaints the focus ring.</summary>
    procedure DoEnter; override;
    /// <summary>Repaints the focus ring.</summary>
    procedure DoExit; override;
    /// <summary>Clamps the loaded position to the loaded range.</summary>
    procedure Loaded; override;
    /// <summary>Fires <see cref="OnChange"/>.</summary>
    procedure Changed; virtual;
  public
    /// <summary>Creates a 0..100 slider.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Applies the density height.</summary>
    procedure DensityChanged; override;
  published
    /// <summary>Lowest position.</summary>
    property Orientation: TERDTrackOrientation read FOrientation write SetOrientation default toHorizontal;
    property Min: Integer read FMin write SetMin default 0;
    /// <summary>Highest position.</summary>
    property Max: Integer read FMax write SetMax default 100;
    /// <summary>Current position, clamped to Min..Max.</summary>
    property Position: Integer read FPosition write SetPosition default 0;
    /// <summary>Distance between tick marks; 0 hides them.</summary>
    property Frequency: Integer read FFrequency write SetFrequency default 0;
    /// <summary>Step of the arrow keys and the wheel.</summary>
    property SmallChange: Integer read FSmallChange write FSmallChange
      default 1;
    /// <summary>Step of Page Up / Page Down.</summary>
    property LargeChange: Integer read FLargeChange write FLargeChange
      default 10;
    /// <summary>Shows the position on the right of the track.</summary>
    property ShowValue: Boolean read FShowValue write SetShowValue
      default False;
    /// <summary>Text after the shown value, e.g. ' ms' or ' %'.</summary>
    property Suffix: string read FSuffix write SetSuffix;
    /// <summary>Height follows the density.</summary>
    property AutoSize default True;
    /// <summary>Takes keyboard focus with Tab.</summary>
    property TabStop default True;
    /// <summary>Desktop or tablet size.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires on the main thread when Position changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TERDTrackBar }

constructor TERDTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks];
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 10;
  Width := 200;
  Height := 26;
  TabStop := True;
  AutoSize := True;
end;

procedure TERDTrackBar.SetMin(AValue: Integer);
begin
  if FMin = AValue then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TERDTrackBar.SetMax(AValue: Integer);
begin
  if FMax = AValue then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TERDTrackBar.SetPosition(AValue: Integer);
begin
  if not (csLoading in ComponentState) then
    AValue := EnsureRange(AValue, FMin, FMax);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Invalidate;
  Changed;
end;

procedure TERDTrackBar.SetFrequency(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FFrequency = AValue then
    Exit;
  FFrequency := AValue;
  Invalidate;
end;

procedure TERDTrackBar.SetShowValue(AValue: Boolean);
begin
  if FShowValue = AValue then
    Exit;
  FShowValue := AValue;
  Invalidate;
end;

procedure TERDTrackBar.SetSuffix(const AValue: string);
begin
  if FSuffix = AValue then
    Exit;
  FSuffix := AValue;
  Invalidate;
end;

procedure TERDTrackBar.SetOrientation(Value: TERDTrackOrientation);
var OldWidth: Integer;
begin
  if FOrientation = Value then Exit;
  FOrientation := Value;
  if not (csLoading in ComponentState) then
  begin
    OldWidth := Width;
    SetBounds(Left, Top, Height, OldWidth);
    if AutoSize then AdjustSize;
  end;
  Invalidate;
end;
function TERDTrackBar.MousePosition(X, Y: Integer): Integer;
begin
  if FOrientation = toVertical then Result := XToPosition(TrackLeft + TrackRight - Y)
  else Result := XToPosition(X);
end;

procedure TERDTrackBar.Loaded;
begin
  inherited Loaded;
  FPosition := EnsureRange(FPosition, FMin, FMax);
end;

procedure TERDTrackBar.Changed;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then
    FOnChange(Self);
end;

function TERDTrackBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if FOrientation = toVertical then NewWidth := ScaleValue(Metrics.Edit)
  else NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TERDTrackBar.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TERDTrackBar.ThumbSize: Integer;
begin
  Result := ScaleValue(Metrics.Check);
end;

function TERDTrackBar.ValueWidth: Integer;
var
  Widest: string;
begin
  if not FShowValue then
    Exit(0);
  // Room for the widest end so the track does not move while sliding.
  if Length(IntToStr(FMin)) > Length(IntToStr(FMax)) then
    Widest := IntToStr(FMin)
  else
    Widest := IntToStr(FMax);
  Result := ERDMeasureText(Widest + FSuffix, ERD_TEXT_BODY, twSemibold,
    ScaleValue(96)) + ScaleValue(12);
end;

function TERDTrackBar.TrackLeft: Integer;
begin
  Result := ThumbSize div 2 + ScaleValue(2);
end;

function TERDTrackBar.TrackRight: Integer;
begin
  if FOrientation = toVertical then
    Result := Height - Ord(FShowValue) * ScaleValue(Metrics.Edit) - ThumbSize div 2 - ScaleValue(2)
  else Result := Width - ValueWidth - ThumbSize div 2 - ScaleValue(2);
end;

function TERDTrackBar.PositionToX(APosition: Integer): Integer;
begin
  if FMax <= FMin then
    Exit(TrackLeft);
  Result := TrackLeft + Round((APosition - FMin) / (FMax - FMin) *
    System.Math.Max(0, TrackRight - TrackLeft));
end;

function TERDTrackBar.XToPosition(X: Integer): Integer;
var
  Span: Integer;
begin
  Span := TrackRight - TrackLeft;
  if (Span <= 0) or (FMax <= FMin) then
    Exit(FMin);
  Result := FMin + Round((X - TrackLeft) / Span * (FMax - FMin));
  Result := EnsureRange(Result, FMin, FMax);
end;

procedure TERDTrackBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
end;

procedure TERDTrackBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
end;

procedure TERDTrackBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TERDTrackBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDTrackBar.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TERDTrackBar.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TERDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if TabStop and CanFocus then
    SetFocus;
  FDragging := True;
  DragTo(MousePosition(X, Y));
  Invalidate;
end;

procedure TERDTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
    DragTo(MousePosition(X, Y));
end;

procedure TERDTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    Invalidate;
  end;
end;

procedure TERDTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT, VK_DOWN:
      SetPosition(FPosition - FSmallChange);
    VK_RIGHT, VK_UP:
      SetPosition(FPosition + FSmallChange);
    VK_PRIOR:
      SetPosition(FPosition + FLargeChange);
    VK_NEXT:
      SetPosition(FPosition - FLargeChange);
    VK_HOME:
      SetPosition(FMin);
    VK_END:
      SetPosition(FMax);
  else
    Exit;
  end;
  Key := 0;
end;

function TERDTrackBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  // Only a focused slider takes the wheel, so scrolling a page past it
  // does not move it.
  if Result or not Focused or (WheelDelta = 0) then
    Exit;
  SetPosition(FPosition + Sign(WheelDelta) * FSmallChange);
  Result := True;
end;

procedure TERDTrackBar.DragTo(Value: Integer);
begin
  SetPosition(Value);
end;
procedure TERDTrackBar.PaintTrackExtras(P: TERDPainter; Center, StartPos, EndPos, ThumbPos: Integer);
begin
  // Descendants add buffered regions before the thumb is painted.
end;
procedure TERDTrackBar.PaintThumb(P: TERDPainter; CX, CY: Single; Active: Boolean);
var T: Integer; Face, Ring: TColor;
begin
  T := ThumbSize; Face := Palette.GaugeFace; Ring := Palette.Accent;
  if not Enabled then begin Face := EffectiveBackground; Ring := ERDMixColor(Palette.Subtle,EffectiveBackground,0.5); end;
  if Active and (FHot or FDragging) and Enabled then
    P.Ellipse(CX-T/2-P.S(3),CY-T/2-P.S(3),T+P.S(6),T+P.S(6),
      ERDMixColor(Palette.Accent,EffectiveBackground,0.18),clNone);
  // Adjacent focus outline: no separate white gap/halo around range thumbs.
  if Active and Focused and FocusCuesVisible and not IsPreview then
    P.FocusRing(CX-T/2-P.S(2),CY-T/2-P.S(2),T+P.S(4),T+P.S(4),T/2+P.S(2));
  P.Ellipse(CX-T/2,CY-T/2,T,T,Face,Ring,P.SF(2));
end;
procedure TERDTrackBar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  CY, X0, X1, TX, T, TrackH, I: Integer;
  Track, Fill: TColor;
  Tick: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    if FOrientation = toVertical then
    begin
      CY := Width div 2;
      X0 := TrackLeft;
      X1 := TrackRight;
      TX := X0 + X1 - PositionToX(FPosition);
      T := ThumbSize;
      Track := Palette.NeutralLight;
      Fill := Palette.Accent;
      if not Enabled then
      begin
        Fill := Palette.Subtle;
      end;
      if X1 > X0 then
      begin
        P.RoundRect(CY - P.S(2), X0, P.S(4), X1 - X0, P.S(2), Track, clNone);
        if X1 > TX then P.RoundRect(CY - P.S(2), TX, P.S(4), X1 - TX, P.S(2), Fill, clNone);
      end;
      PaintTrackExtras(P, CY, X0, X1, TX);
      if (FFrequency > 0) and (FMax > FMin) and ((FMax - FMin) div FFrequency <= 200) then
      begin
        Tick := FMin;
        I := 0;
        while (Tick <= FMax) and (I <= 200) do
        begin
          P.FillRect(Rect(CY + T div 2 + P.S(1), X0 + X1 - PositionToX(Tick),
            CY + T div 2 + P.S(4), X0 + X1 - PositionToX(Tick) + P.S(1)), Palette.Subtle);
          Inc(Tick, FFrequency);
          Inc(I);
        end;
      end;
      PaintThumb(P,CY,TX);
      if FShowValue then P.Text(CY, Height - ScaleValue(Metrics.Edit) div 2,
        IntToStr(FPosition) + FSuffix, ERD_TEXT_BODY, Palette.ForegroundText, twSemibold, taCenter);
      Exit;
    end;
    CY := Height div 2;
    X0 := TrackLeft;
    X1 := TrackRight;
    TX := PositionToX(FPosition);
    T := ThumbSize;
    TrackH := P.S(4);
    if Enabled then
    begin
      Track := Palette.NeutralLight;
      Fill := Palette.Accent;
    end
    else
    begin
      Track := ERDMixColor(Palette.NeutralLight, EffectiveBackground, 0.6);
      Fill := ERDMixColor(Palette.Subtle, EffectiveBackground, 0.5);
    end;
    // Ticks below the track.
    if (FFrequency > 0) and (FMax > FMin) and
      ((FMax - FMin) div FFrequency <= 200) then
    begin
      Tick := FMin;
      I := 0;
      while (Tick <= FMax) and (I <= 200) do
      begin
        P.FillRect(Rect(PositionToX(Tick), CY + T div 2 + P.S(1),
          PositionToX(Tick) + P.S(1), CY + T div 2 + P.S(4)),
          Palette.Subtle);
        Inc(Tick, FFrequency);
        Inc(I);
      end;
    end;
    if X1 > X0 then
    begin
      P.RoundRect(X0, CY - TrackH / 2, X1 - X0, TrackH, TrackH / 2, Track,
        clNone);
      if TX > X0 then
        P.RoundRect(X0, CY - TrackH / 2, TX - X0, TrackH, TrackH / 2, Fill,
          clNone);
    end;
    PaintTrackExtras(P, CY, X0, X1, TX);
    PaintThumb(P,TX,CY);
    if FShowValue then
      P.Text(Width, CY, IntToStr(FPosition) + FSuffix, ERD_TEXT_BODY,
        Palette.ForegroundText, twSemibold, taRightJustify);
  finally
    P.Free;
  end;
end;

end.
