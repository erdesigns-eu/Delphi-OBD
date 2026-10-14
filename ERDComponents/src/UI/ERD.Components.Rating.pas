unit ERD.Components.Rating;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math,
  Winapi.Windows, Winapi.Messages, Winapi.GDIPAPI, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint,
  ERD.Components.Buttons, ERD.Components.Icons;
type
  TERDRating = class(TERDInteractiveControl)
  private
    FValue, FPreview: Single;
    FMaxStars: Integer;
    FAllowHalf, FReadOnly, FAllowClear: Boolean;
    FIcon: TERDIcon;
    FOnChange: TNotifyEvent;
    procedure SetValue(Value: Single);
    procedure SetMaxStars(Value: Integer);
    procedure SetAllowHalf(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetIcon(Value: TERDIcon);
    procedure DrawStar(P: TERDPainter; CX, CY: Integer; Fraction: Single; Ink: TColor);
    procedure DrawCustomIcon(P: TERDPainter; X, CY: Integer; Fraction: Single; Ink: TColor);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValueAt(X, Y: Integer): Single;
    property PreviewValue: Single read FPreview;
  published
    property Value: Single read FValue write SetValue;
    property MaxStars: Integer read FMaxStars write SetMaxStars default 5;
    property AllowHalf: Boolean read FAllowHalf write SetAllowHalf default False;
    property AllowClear: Boolean read FAllowClear write FAllowClear default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    /// <summary>No icon = filled/half-filled native stars; any icon source replaces the symbols.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
implementation
constructor TERDRating.Create(AOwner: TComponent);
begin
  inherited;
  FMaxStars := 5;
  FAllowClear := True;
  FIcon := TERDIcon.Create(Self);
  SetBounds(Left, Top, ScaleValue(5 * 24) + 2 * RingMargin, ScaleValue(32));
end;
destructor TERDRating.Destroy;
begin FIcon.Free; inherited end;
procedure TERDRating.SetValue(Value: Single);
begin
  if IsNan(Value) or IsInfinite(Value) then raise EArgumentException.Create('Rating must be finite');
  if Value < 0 then Value := 0;
  if Value > FMaxStars then Value := FMaxStars;
  if FAllowHalf then Value := Floor(Value * 2 + 0.5) / 2 else Value := Floor(Value + 0.5);
  if SameValue(FValue, Value) then Exit;
  FValue := Value;
  Invalidate;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then FOnChange(Self);
end;
procedure TERDRating.SetMaxStars(Value: Integer);
begin
  if (Value < 1) or (Value > 100) then raise EArgumentOutOfRangeException.Create('MaxStars must be between 1 and 100');
  if FMaxStars = Value then Exit;
  FMaxStars := Value;
  FPreview := 0;
  if AutoSize then AdjustSize;
  Invalidate;
  SetValue(FValue);
end;
procedure TERDRating.SetAllowHalf(Value: Boolean);
begin
  if FAllowHalf = Value then Exit;
  FAllowHalf := Value;
  FPreview := 0;
  Invalidate;
  SetValue(FValue);
end;
procedure TERDRating.SetReadOnly(Value: Boolean);
begin FReadOnly := Value; FPreview := 0; Invalidate end;
procedure TERDRating.SetIcon(Value: TERDIcon);
begin FIcon.Assign(Value); Invalidate end;
function TERDRating.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewWidth := ScaleValue(FMaxStars * 24) + 2 * RingMargin;
  NewHeight := ScaleValue(32);
end;
function TERDRating.ValueAt(X, Y: Integer): Single;
var Index, Offset, Stride: Integer;
begin
  Result := 0;
  if (X < RingMargin) or (X >= Width) or (Y < 0) or (Y >= Height) then Exit;
  Stride := ScaleValue(24);
  Index := (X - RingMargin) div Stride;
  if Index >= FMaxStars then Exit;
  Offset := (X - RingMargin) mod Stride;
  Result := Index + 1;
  if FAllowHalf and (Offset < ScaleValue(10)) then Result := Index + 0.5;
end;
procedure TERDRating.MouseMove(Shift: TShiftState; X, Y: Integer);
var Next: Single;
begin
  inherited;
  Next := 0;
  if Enabled and not FReadOnly then Next := ValueAt(X, Y);
  if FPreview <> Next then begin FPreview := Next; Invalidate end;
end;
procedure TERDRating.CMMouseLeave(var Message: TMessage);
begin inherited; FPreview := 0; Invalidate end;
procedure TERDRating.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Next: Single;
begin
  if (Button = mbLeft) and Pressed and Enabled and not FReadOnly then
  begin
    Next := ValueAt(X, Y);
    if Next > 0 then
    begin
      if FAllowClear and SameValue(Next, FValue) then Next := 0;
      SetValue(Next);
    end;
  end;
  inherited;
end;
procedure TERDRating.KeyDown(var Key: Word; Shift: TShiftState);
var Next, Step: Single;
begin
  if not Enabled or FReadOnly or (Shift <> []) then begin inherited; Exit end;
  if FAllowHalf then Step := 0.5 else Step := 1;
  case Key of
    VK_RIGHT, VK_UP: Next := FValue + Step;
    VK_LEFT, VK_DOWN: Next := FValue - Step;
    VK_HOME: Next := 0;
    VK_END: Next := FMaxStars;
    VK_DELETE, VK_BACK:
      if FAllowClear then Next := 0 else begin Key := 0; Exit end;
  else inherited; Exit;
  end;
  if not FAllowClear and (Next < Step) then Next := Step;
  Key := 0;
  SetValue(Next);
end;
procedure TERDRating.DrawStar(P: TERDPainter; CX, CY: Integer; Fraction: Single; Ink: TColor);
var Points: array[0..9] of TGPPointF; Closed: array[0..10] of TGPPointF;
  Half: TArray<TGPPointF>; I, Previous, Count: Integer; Angle, Radius, Factor: Single;
  Inside, PreviousInside: Boolean; Border: TColor;
  procedure AddPoint(const Point: TGPPointF);
  begin SetLength(Half, Length(Half) + 1); Half[High(Half)] := Point end;
begin
  for I := 0 to 9 do
  begin
    Radius := P.SF(10);
    if Odd(I) then Radius := Radius * 0.45;
    Angle := -Pi / 2 + I * Pi / 5;
    Points[I] := ERDPointF(CX + Cos(Angle) * Radius, CY + Sin(Angle) * Radius);
    Closed[I] := Points[I];
  end;
  Closed[10] := Points[0];
  P.Polygon(Points, Palette.GaugeFace);
  Border := Palette.NeutralLight;
  if Fraction >= 1 then begin P.Polygon(Points, Ink); Border := Ink end
  else if Fraction > 0 then
  begin
    Half := nil;
    Previous := 9;
    for I := 0 to 9 do
    begin
      Inside := Points[I].X <= CX;
      PreviousInside := Points[Previous].X <= CX;
      if Inside <> PreviousInside then
      begin
        Factor := (CX - Points[Previous].X) / (Points[I].X - Points[Previous].X);
        AddPoint(ERDPointF(CX, Points[Previous].Y + Factor * (Points[I].Y - Points[Previous].Y)));
      end;
      if Inside then AddPoint(Points[I]);
      Previous := I;
    end;
    Count := Length(Half);
    if Count >= 3 then P.Polygon(Half, Ink);
    Border := Ink;
  end;
  P.Lines(Closed, Border, P.SF(1));
end;
procedure TERDRating.DrawCustomIcon(P: TERDPainter; X, CY: Integer; Fraction: Single; Ink: TColor);
var Bitmap: TBitmap; Size, Filled: Integer; R: TRect;
begin
  Size := P.S(20);
  FIcon.Draw(P.Canvas, Palette, ScaleValue(96), X + Size div 2, CY, Palette.Subtle, 1.25, Enabled);
  if Fraction <= 0 then Exit;
  if Fraction >= 1 then
  begin FIcon.Draw(P.Canvas, Palette, ScaleValue(96), X + Size div 2, CY, Ink, 1.25, Enabled); Exit end;
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(Size, Size);
    Bitmap.Canvas.Brush.Color := Palette.GaugeFace;
    Bitmap.Canvas.FillRect(Rect(0, 0, Size, Size));
    FIcon.Draw(Bitmap.Canvas, Palette, ScaleValue(96), Size div 2, Size div 2, Ink, 1.25, Enabled);
    Filled := Round(Size * Fraction);
    R := Rect(X, CY - Size div 2, X + Filled, CY - Size div 2 + Size);
    P.Canvas.CopyRect(R, Bitmap.Canvas, Rect(0, 0, Filled, Size));
  finally Bitmap.Free end;
end;
procedure TERDRating.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Shown, Fraction: Single; I, X: Integer; Ink: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Shown := FValue;
    if Enabled and not FReadOnly and Hover and (FPreview > 0) then Shown := FPreview;
    Ink := Palette.Accent;
    if not Enabled then Ink := P.DisabledText;
    for I := 0 to FMaxStars - 1 do
    begin
      Fraction := Shown - I;
      if Fraction < 0 then Fraction := 0;
      if Fraction > 1 then Fraction := 1;
      X := RingMargin + P.S(I * 24);
      if FIcon.Available then DrawCustomIcon(P, X, Height div 2, Fraction, Ink)
      else DrawStar(P, X + P.S(10), Height div 2, Fraction, Ink);
    end;
    if HasFocus then P.FocusRing(RingMargin, RingMargin, Width - 2 * RingMargin, Height - 2 * RingMargin, P.S(2));
  finally P.Free end;
end;
end.
