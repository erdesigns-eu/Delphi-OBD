unit ERD.Components.MediaControls;
interface
uses
  Winapi.Windows, System.Classes, System.Types, System.SysUtils, System.Math,
  Vcl.Controls, Vcl.Graphics, ERD.Components.Types, ERD.Components.Paint,
  ERD.Components.TrackBar;
type
  TERDSeekBar = class(TERDTrackBar)
  private
    FBufferedPosition: Integer;
    FSeeking: Boolean;
    FOnSeek: TNotifyEvent;
    procedure SetBufferedPosition(Value: Integer);
  protected
    procedure PaintTrackExtras(P: TERDPainter; Center, StartPos, EndPos, ThumbPos: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  published
    property BufferedPosition: Integer read FBufferedPosition write SetBufferedPosition default 0;
    property OnSeek: TNotifyEvent read FOnSeek write FOnSeek;
  end;
  TERDRangeThumb = (rtLower, rtUpper);
  TERDRangeSlider = class(TERDTrackBar)
  private
    FUpperValue: Integer;
    FActiveThumb: TERDRangeThumb;
    FRangeUpdating: Boolean;
    procedure SetActiveThumb(Value: TERDRangeThumb);
    function GetLowerValue: Integer;
    procedure SetLowerValue(Value: Integer);
    procedure SetUpperValue(Value: Integer);
    function AxisPosition(Value: Integer): Integer;
    function PointerValue(X, Y: Integer): Integer;
    procedure MoveActive(Value: Integer);
    procedure NormalizeUpper;
  protected
    procedure Changed; override;
    procedure DragTo(Value: Integer); override;
    function ValueWidth: Integer; override;
    procedure SetMin(AValue: Integer); override;
    procedure SetMax(AValue: Integer); override;
    procedure SetPosition(AValue: Integer); override;
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LowerValue: Integer read GetLowerValue write SetLowerValue stored False;
    property UpperValue: Integer read FUpperValue write SetUpperValue default 100;
    property ActiveThumb: TERDRangeThumb read FActiveThumb write SetActiveThumb default rtLower;
  end;
implementation
procedure TERDSeekBar.SetBufferedPosition(Value: Integer);
begin
  if FBufferedPosition = Value then Exit;
  FBufferedPosition := Value;
  Invalidate;
end;
procedure TERDSeekBar.PaintTrackExtras(P: TERDPainter; Center, StartPos, EndPos, ThumbPos: Integer);
var BufferPos: Integer; C: TColor;
begin
  C := ERDMixColor(Palette.Accent, Palette.NeutralLight, 0.35);
  BufferPos := PositionToX(EnsureRange(FBufferedPosition, Min, Max));
  if Orientation = toVertical then
  begin
    BufferPos := StartPos + EndPos - BufferPos;
    if BufferPos < ThumbPos then P.FillRect(Rect(Center - P.S(2), BufferPos, Center + P.S(2), ThumbPos), C);
  end
  else if BufferPos > ThumbPos then
    P.FillRect(Rect(ThumbPos, Center - P.S(2), BufferPos, Center + P.S(2)), C);
end;
procedure TERDSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSeeking := Enabled and (Button = mbLeft);
  inherited;
end;
procedure TERDSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Notify: Boolean;
begin
  Notify := FSeeking and Enabled and (Button = mbLeft);
  FSeeking := False;
  inherited;
  if Notify and Assigned(FOnSeek) then FOnSeek(Self);
end;
procedure TERDSeekBar.KeyDown(var Key: Word; Shift: TShiftState);
var Before: Integer;
begin
  Before := Position;
  inherited;
  if (Position <> Before) and Assigned(FOnSeek) then FOnSeek(Self);
end;
function TERDSeekBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var Before: Integer;
begin
  Before := Position;
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if (Position <> Before) and Assigned(FOnSeek) then FOnSeek(Self);
end;
constructor TERDRangeSlider.Create(AOwner: TComponent);
begin
  inherited;
  FUpperValue := 100;
end;
function TERDRangeSlider.GetLowerValue: Integer;
begin
  Result := Position;
end;
procedure TERDRangeSlider.SetLowerValue(Value: Integer);
begin
  Position := Value;
end;
procedure TERDRangeSlider.NormalizeUpper;
begin
  FUpperValue := EnsureRange(FUpperValue, Min, Max);
  if FUpperValue < Position then FUpperValue := Position;
end;
procedure TERDRangeSlider.SetActiveThumb(Value: TERDRangeThumb);
begin
  if FActiveThumb = Value then Exit;
  FActiveThumb := Value;
  Invalidate;
end;
procedure TERDRangeSlider.Changed;
begin
  if FRangeUpdating then Exit;
  if not (csLoading in ComponentState) then NormalizeUpper;
  inherited;
end;
procedure TERDRangeSlider.SetMin(AValue: Integer);
var OldLower, OldUpper: Integer;
begin
  OldLower := Position;
  OldUpper := FUpperValue;
  FRangeUpdating := True;
  try
    inherited;
    NormalizeUpper;
  finally FRangeUpdating := False end;
  if (OldLower <> Position) or (OldUpper <> FUpperValue) then Changed;
end;
procedure TERDRangeSlider.SetMax(AValue: Integer);
var OldLower, OldUpper: Integer;
begin
  OldLower := Position;
  OldUpper := FUpperValue;
  FRangeUpdating := True;
  try
    inherited;
    NormalizeUpper;
  finally FRangeUpdating := False end;
  if (OldLower <> Position) or (OldUpper <> FUpperValue) then Changed;
end;
procedure TERDRangeSlider.SetPosition(AValue: Integer);
begin
  if not (csLoading in ComponentState) then AValue := System.Math.Min(AValue, FUpperValue);
  inherited;
end;
procedure TERDRangeSlider.SetUpperValue(Value: Integer);
begin
  if not (csLoading in ComponentState) then Value := EnsureRange(Value, Position, Max);
  if FUpperValue = Value then Exit;
  FUpperValue := Value;
  Invalidate;
  Changed;
end;
procedure TERDRangeSlider.Loaded;
begin
  inherited;
  NormalizeUpper;
end;
function TERDRangeSlider.AxisPosition(Value: Integer): Integer;
begin
  Result := PositionToX(Value);
  if Orientation = toVertical then Result := TrackLeft + TrackRight - Result;
end;
function TERDRangeSlider.PointerValue(X, Y: Integer): Integer;
begin
  if Orientation = toVertical then Result := XToPosition(TrackLeft + TrackRight - Y)
  else Result := XToPosition(X);
end;
procedure TERDRangeSlider.MoveActive(Value: Integer);
begin
  if FActiveThumb = rtLower then Position := Value else SetUpperValue(Value);
end;
procedure TERDRangeSlider.DragTo(Value: Integer);
begin
  MoveActive(Value);
end;
function TERDRangeSlider.ValueWidth: Integer;
var S: string;
begin
  if not ShowValue then Exit(0);
  S := IntToStr(Min);
  if Length(IntToStr(Max)) > Length(S) then S := IntToStr(Max);
  Result := ERDMeasureText(S + '..' + S + Suffix, ERD_TEXT_BODY, twSemibold, ScaleValue(96)) + ScaleValue(12);
end;
procedure TERDRangeSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Value: Integer;
begin
  if (Button <> mbLeft) or not Enabled then Exit;
  Value := PointerValue(X, Y);
  if Abs(Int64(Value) - Position) < Abs(Int64(Value) - FUpperValue) then SetActiveThumb(rtLower)
  else if Value <> Position then SetActiveThumb(rtUpper);
  inherited;
end;
procedure TERDRangeSlider.KeyDown(var Key: Word; Shift: TShiftState);
var Value: Int64;
begin
  if Assigned(OnKeyDown) then OnKeyDown(Self, Key, Shift);
  if not Enabled then Exit;
  if (Key = VK_SPACE) and (Shift = []) then
  begin
    if FActiveThumb = rtLower then SetActiveThumb(rtUpper) else SetActiveThumb(rtLower);
    Invalidate;
    Key := 0;
    Exit;
  end;
  if FActiveThumb = rtLower then Value := Position else Value := FUpperValue;
  case Key of
    VK_LEFT, VK_DOWN: Dec(Value, SmallChange);
    VK_RIGHT, VK_UP: Inc(Value, SmallChange);
    VK_PRIOR: Inc(Value, LargeChange);
    VK_NEXT: Dec(Value, LargeChange);
    VK_HOME: Value := Min;
    VK_END: Value := Max;
  else Exit;
  end;
  MoveActive(Integer(EnsureRange(Value, Int64(Min), Int64(Max))));
  Key := 0;
end;
function TERDRangeSlider.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var Value: Int64;
begin
  Result := Enabled and Focused and (WheelDelta <> 0);
  if not Result then Exit;
  if FActiveThumb = rtLower then Value := Position else Value := FUpperValue;
  if WheelDelta > 0 then Inc(Value, SmallChange) else Dec(Value, SmallChange);
  MoveActive(Integer(EnsureRange(Value, Int64(Min), Int64(Max))));
end;
procedure TERDRangeSlider.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; CY, A, B, T, StartPos, EndPos: Integer; C: TColor;
  procedure DrawThumb(Pos: Integer; Active: Boolean);
  begin
    if Orientation = toVertical then
    begin
      if Active and Focused and FocusCuesVisible and not IsPreview then P.FocusRing(CY - T / 2 - P.S(3), Pos - T / 2 - P.S(3), T + P.S(6), T + P.S(6), T / 2 + P.S(3));
      P.Ellipse(CY - T / 2, Pos - T / 2, T, T, Palette.GaugeFace, C, P.SF(2));
    end
    else
    begin
      if Active and Focused and FocusCuesVisible and not IsPreview then P.FocusRing(Pos - T / 2 - P.S(3), CY - T / 2 - P.S(3), T + P.S(6), T + P.S(6), T / 2 + P.S(3));
      P.Ellipse(Pos - T / 2, CY - T / 2, T, T, Palette.GaugeFace, C, P.SF(2));
    end;
  end;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    A := AxisPosition(Position);
    B := AxisPosition(EnsureRange(FUpperValue, Min, Max));
    T := ThumbSize;
    StartPos := TrackLeft;
    EndPos := TrackRight;
    C := Palette.Accent;
    if not Enabled then C := Palette.Subtle;
    if Orientation = toVertical then
    begin
      CY := Width div 2;
      P.FillRect(Rect(CY - P.S(2), StartPos, CY + P.S(2), System.Math.Max(StartPos, EndPos)), Palette.NeutralLight);
      P.FillRect(Rect(CY - P.S(2), B, CY + P.S(2), A), C);
    end
    else
    begin
      CY := Height div 2;
      P.FillRect(Rect(StartPos, CY - P.S(2), System.Math.Max(StartPos, EndPos), CY + P.S(2)), Palette.NeutralLight);
      P.FillRect(Rect(A, CY - P.S(2), B, CY + P.S(2)), C);
    end;
    DrawThumb(A, FActiveThumb = rtLower);
    DrawThumb(B, FActiveThumb = rtUpper);
    if ShowValue then
      if Orientation = toVertical then P.Text(CY, Height - ScaleValue(Metrics.Edit) div 2,
        IntToStr(Position) + '..' + IntToStr(FUpperValue) + Suffix, ERD_TEXT_BODY, Palette.ForegroundText, twSemibold, taCenter)
      else P.Text(Width, CY, IntToStr(Position) + '..' + IntToStr(FUpperValue) + Suffix,
        ERD_TEXT_BODY, Palette.ForegroundText, twSemibold, taRightJustify);
  finally P.Free end;
end;
end.
