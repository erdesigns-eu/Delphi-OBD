//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  Invalidate;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SettingsChanged(Sender: TObject);
begin
  // Paint buffer
  Invalidate;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.PaintSkia(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  BackgroundRect, BorderRect: TRectF;
  MeasureRect: TRect;
  BatteryRect, VciRect, InternetRect, ProtocolRect: TRectF;
  BatteryCaptionRect, VciCaptionRect, InternetCaptionRect, ProtocolCaptionRect: TRect;
  BatteryLabelText, VciLabelText, InternetLabelText, ProtocolLabelText: string;
  HasBattery, HasVci, HasInternet, HasProtocol: Boolean;
  X, Y, Z: Single;
  TextSize: TSizeF;
begin
  // Update the size of the buffer
  Buffer.SetSize(Width, Height);

  // Allocate a Skia surface for the subheader drawing pass
  Surface := TSkSurface.MakeRasterN32Premul(Width, Height);
  Canvas := Surface.Canvas;

  // Clear using the active style's fill color so the Skia surface matches themed backgrounds
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Draw the backround gradient when configured
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    BackgroundRect := TRectF.Create(0.0, 0.0, Width + 0.0, Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Top),
      TSkPoint.Create(BackgroundRect.Left, BackgroundRect.Bottom),
      [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BackgroundRect, Paint);
  end;

  // Draw the border stripe when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TRectF.Create(0.0, Height - Border.Height + 0.0, Width + 0.0, Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BorderRect.Left, BorderRect.Top),
      TSkPoint.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BorderRect, Paint);
  end;

  // Prepare layout state
  X := Width - 8;
  HasBattery := False;
  HasVci := False;
  HasInternet := False;
  HasProtocol := False;

  // Draw the battery indicator
  if BatteryIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (BatteryIndicator.Size / 2);
    BatteryLabelText := Format(BatteryIndicator.Format, [BatteryIndicator.Voltage]);

    TextSize := MeasureSkText(BatteryLabelText, BatteryIndicator.Font);
    MeasureRect := Rect(0, 0, Width - 8, Height - Border.Height);
    BatteryCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 4);

    BatteryRect := TRectF.Create(X - BatteryIndicator.Size, Y, X, Y + BatteryIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Top),
      TSkPoint.Create(BatteryRect.Left, BatteryRect.Bottom),
      [SafeColorRefToSkColor(BatteryIndicator.FromColor), SafeColorRefToSkColor(BatteryIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := BatteryIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(BatteryIndicator.BorderColor);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    X := X - (BatteryIndicator.Size + 4);
    HasBattery := True;
  end;

  // Draw the VCI indicator
  if VciIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (VciIndicator.Size / 2);
    VciLabelText := VciIndicator.Caption;

    TextSize := MeasureSkText(VciLabelText, VciIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    VciCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 2);

    VciRect := TRectF.Create(X - VciIndicator.Size, Y, X, Y + VciIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(VciRect.Left, VciRect.Top),
      TSkPoint.Create(VciRect.Left, VciRect.Bottom),
      [SafeColorRefToSkColor(VciIndicator.FromColor), SafeColorRefToSkColor(VciIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := VciIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(VciIndicator.BorderColor);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    X := X - (VciIndicator.Size + 4);
    HasVci := True;
  end;

  // Draw the internet connection indicator
  if InternetConnectionIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (InternetConnectionIndicator.Size / 2);
    InternetLabelText := InternetConnectionIndicator.Caption;

    TextSize := MeasureSkText(InternetLabelText, InternetConnectionIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    InternetCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - TextSize.cx;

    InternetRect := TRectF.Create(X - InternetConnectionIndicator.Size, Y, X, Y + InternetConnectionIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(InternetRect.Left, InternetRect.Top),
      TSkPoint.Create(InternetRect.Left, InternetRect.Bottom),
      [SafeColorRefToSkColor(InternetConnectionIndicator.FromColor), SafeColorRefToSkColor(InternetConnectionIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(InternetRect, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := InternetConnectionIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(InternetConnectionIndicator.BorderColor);
    Canvas.DrawPath(CreateInternetGlobePath(InternetRect), Paint);

    X := X - (InternetConnectionIndicator.Size + 4);
    HasInternet := True;
  end;

  // Draw the protocol indicator
  if ProtocolIndicator.Visible then
  begin
    Z := X;
    Y := ((Height - Border.Height) / 2) - (ProtocolIndicator.Size / 2);

    ProtocolRect := TRectF.Create(8, Y, 8 + ProtocolIndicator.Size, Y + ProtocolIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeLinearGradient(
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Top),
      TSkPoint.Create(ProtocolRect.Left, ProtocolRect.Bottom),
      [SafeColorRefToSkColor(ProtocolIndicator.FromColor), SafeColorRefToSkColor(ProtocolIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := ProtocolIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(ProtocolIndicator.BorderColor);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    ProtocolLabelText := FitTextToWidth(ProtocolIndicator.Caption, ProtocolIndicator.Font, Round(Z - (12 + ProtocolIndicator.Size)));
    TextSize := MeasureSkText(ProtocolLabelText, ProtocolIndicator.Font);
    MeasureRect := Rect(12 + ProtocolIndicator.Size, 0, Round(Z - (12 + ProtocolIndicator.Size)), Height - Border.Height);
    ProtocolCaptionRect := Rect(
      MeasureRect.Left,
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Left + Min(Ceil(TextSize.cx), MeasureRect.Right - MeasureRect.Left),
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    HasProtocol := True;
  end;

  // Render captions with Skia to keep text drawing inside the Skia pipeline
  if HasBattery then
  begin
    DrawSkTextCentered(Canvas, BatteryLabelText, BatteryIndicator.Font, TRectF.Create(BatteryCaptionRect), BatteryIndicator.Font.Color, TSkTextAlign.Right);
    DrawSkTextCentered(Canvas, '- +', BatteryIndicator.Font, BatteryRect, BatteryIndicator.Font.Color);
  end;

  if HasVci then
    DrawSkTextCentered(Canvas, VciLabelText, VciIndicator.Font, TRectF.Create(VciCaptionRect), VciIndicator.Font.Color, TSkTextAlign.Right);

  if HasInternet then
    DrawSkTextCentered(Canvas, InternetLabelText, InternetConnectionIndicator.Font, TRectF.Create(InternetCaptionRect), InternetConnectionIndicator.Font.Color, TSkTextAlign.Right);

  if HasProtocol then
    DrawSkTextCentered(Canvas, ProtocolLabelText, ProtocolIndicator.Font, TRectF.Create(ProtocolCaptionRect), ProtocolIndicator.Font.Color, TSkTextAlign.Left);

  // Copy the Skia surface into the component buffer
  // Direct rendering to canvas - no conversion needed!
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheader.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
