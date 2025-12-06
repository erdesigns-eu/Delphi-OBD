//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  Invalidate;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// MOUSE DOWN HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousedown
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE UP HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mouseup
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE MOVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousemove
  inherited;
end;

//------------------------------------------------------------------------------
// WND PROC HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WndProc(var Message: TMessage);
var
  P: TPoint;
  Form: TCustomForm;
begin
  // Call inherited WndProc
  inherited WndProc(Message);

  // Get the parent form
  Form := GetParentForm(Self);
  // If there is a parentform and the window state is normal
  if Assigned(Form) and (Form is TForm) and (Form.WindowState = wsNormal) then
  begin
    // Check if we are in the sizegrip rect
    if (Message.Msg = WM_NCHITTEST) and FSizeGrip.Visible then
    begin
      P := ScreenToClient(Mouse.CursorPos);
      if PtInRect(FSizeGrip.SizeGripRect, P) then
      begin
        Message.Result := HTBOTTOMRIGHT;
        Exit;
      end;
    end;

    // Handle the WM_NCLBUTTONDOWN message to start resizing
    if (Message.Msg = WM_NCLBUTTONDOWN) and (Message.WParam = HTBOTTOMRIGHT) then
    begin
      ReleaseCapture;
      Form.Perform(WM_SYSCOMMAND, SC_SIZE + 9, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SettingsChanged(Sender: TObject);
begin
  // Paint buffer
  Invalidate;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.PaintSkia(Canvas: ISkCanvas);
const
  PanelPadding = 4;
  LedSize = 14;
var
  Paint: ISkPaint;
  Typeface: ISkTypeface;
  SkFont: ISkFont;
  Metrics: TSkFontMetrics;
  BackgroundRect, BorderRect, LedRect, LedBorderRect, SizeGripRect: TRectF;
  SizeGripWidth, I, W, S, PanelX: Integer;
  X, Y: Single;

  function MeasureTextWidth(const Text: string; const AFont: TFont): Integer;
  var
    TextPaint: ISkPaint;
    TextFont: ISkFont;
    Bounds: TRectF;
  begin
    TextPaint := TSkPaint.Create;
    TextPaint.AntiAlias := True;
    TextPaint.Style := TSkPaintStyle.Fill;

    TextFont := TSkFont.Create(CreateSkTypeface(AFont), AFont.Size);
    TextFont.MeasureText(Text, Bounds, TextPaint);
    Result := Ceil(Bounds.Width);
  end;

  procedure PaintLed(Panel: TOBDTouchStatusbarPanel);
  begin
    X := Panel.PanelRect.Left;
    Y := (Border.Height + ((ClientRect.Height - Border.Height) / 2)) - (LedSize / 2);

    LedBorderRect := TRectF.Create(X, Y, X + LedSize, Y + LedSize);
    LedRect := TRectF.Create(
      LedBorderRect.Left + Panel.LedBorder.Width + 1,
      LedBorderRect.Top + Panel.LedBorder.Width + 1,
      LedBorderRect.Right - Panel.LedBorder.Width - 1,
      LedBorderRect.Bottom - Panel.LedBorder.Width - 1
    );

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    case Panel.LedState of
      lsGrayed : Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(LedRect.Left, LedRect.Top),
        TSkPoint.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedGrayedColor.FromColor), SafeColorRefToSkColor(Panel.LedGrayedColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
      lsOff    : Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(LedRect.Left, LedRect.Top),
        TSkPoint.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedOffColor.FromColor), SafeColorRefToSkColor(Panel.LedOffColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
      lsOn     : Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(LedRect.Left, LedRect.Top),
        TSkPoint.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedOnColor.FromColor), SafeColorRefToSkColor(Panel.LedOnColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
    end;
    Canvas.DrawOval(LedRect, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1.5;
    case Panel.LedState of
      lsGrayed : Paint.Color := SafeColorRefToSkColor(Panel.LedGrayedColor.FromColor);
      lsOff    : Paint.Color := SafeColorRefToSkColor(Panel.LedOffColor.FromColor);
      lsOn     : Paint.Color := SafeColorRefToSkColor(Panel.LedOnColor.FromColor);
    end;
    Canvas.DrawOval(LedRect, Paint);

    if (Panel.LedBorder.FromColor <> clNone) and (Panel.LedBorder.ToColor <> clNone) and (Panel.LedBorder.Width > 0) then
    begin
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := Panel.LedBorder.Width;
      Paint.Shader := TSkShader.MakeLinearGradient(
        TSkPoint.Create(LedBorderRect.Left, LedBorderRect.Top),
        TSkPoint.Create(LedBorderRect.Left, LedBorderRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedBorder.FromColor), SafeColorRefToSkColor(Panel.LedBorder.ToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawOval(LedBorderRect, Paint);
    end;
  end;
begin
  // Clear canvas with resolved background color
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Paint the background gradient when both colors are provided
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

  // Draw the top border strip when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TRectF.Create(0.0, 0.0, Width + 0.0, Border.Height + 0.0);
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

  // Calculate panel rectangles based on auto-size/text/LED requirements
  PanelX := ClientRect.Left;
  for I := 0 to Panels.Count -1 do
  begin
    W := 0;
    if Panels[I].AutoSize then
    begin
      if Panels[I].ShowLed then W := W + LedSize + PanelPadding + 2;
      if Panels[I].Style = psSimpleText then
        W := W + MeasureTextWidth(Panels[I].Text, Panels[I].Font)
      else
      begin
        W := W + MeasureTextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);
        W := W + MeasureTextWidth(Panels[I].SecondaryText, Panels[I].SecondaryFont);
        W := W + PanelPadding;
      end;
    end else
    begin
      if Panels[I].ShowLed then W := W + LedSize;
      W := W + Panels[I].Width;
    end;
    Panels[I].PanelRect := TRect.Create(
      PanelX + PanelPadding,
      ClientRect.Top,
      PanelX + PanelPadding + W + PanelPadding,
      ClientRect.Bottom
    );
    PanelX := PanelX + Panels[I].PanelRect.Width;
  end;

  // Render LEDs for all panels that need them
  for I := 0 to Panels.Count -1 do
  begin
    if Panels[I].ShowLed then
      PaintLed(Panels[I]);
  end;

  // Draw the resize size grip as a 3x3 matrix of dots
  if SizeGrip.Visible then
  begin
    SizeGripRect := TRectF.Create(0.0, Border.Height + 0.0, Width - 3.0, Height - Border.Height + 0.0);
    SizeGripWidth := Ceil((SizeGrip.DotSize + SizeGrip.DotSpacing) * 3);
    FSizeGrip.SizeGripRect := Rect((Width - 3) - SizeGripWidth, Border.Height, Width - 3, Height);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(SizeGrip.DotColor);
    for W := 0 to 2 do
      for S := 0 to 2 do
      begin
        X := SizeGripRect.Right - SizeGripWidth + (W * (SizeGrip.DotSize + SizeGrip.DotSpacing));
        Y := SizeGripRect.Bottom - ((S + 1) * (SizeGrip.DotSize + SizeGrip.DotSpacing));
        if SizeGrip.DotShape = dsSquare then
          Canvas.DrawRect(TRectF.Create(X, Y, X + SizeGrip.DotSize, Y + SizeGrip.DotSize), Paint)
        else
          Canvas.DrawCircle(TSkPoint.Create(X + (SizeGrip.DotSize / 2), Y + (SizeGrip.DotSize / 2)), SizeGrip.DotSize / 2, Paint);
      end;
  end;

  // Draw captions for each panel, honoring LED offsets and style differences
  for I := 0 to Panels.Count -1 do
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;

    if Panels[I].ShowLed then
      X := LedSize + 2
    else
      X := 0;

    if Panels[I].Style = psSimpleText then
    begin
      Typeface := CreateSkTypeface(Panels[I].Font);
      SkFont := TSkFont.Create(Typeface, Panels[I].Font.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].Font.Color);
      Metrics := SkFont.Metrics;
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].Text, Panels[I].PanelRect.Left + X, Y, SkFont, Paint);
    end else
    begin
      Typeface := CreateSkTypeface(Panels[I].PrimaryFont);
      SkFont := TSkFont.Create(Typeface, Panels[I].PrimaryFont.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].PrimaryFont.Color);
      Metrics := SkFont.Metrics;
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].PrimaryText, Panels[I].PanelRect.Left + X, Y, SkFont, Paint);

      X := X + MeasureTextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);

      Typeface := CreateSkTypeface(Panels[I].SecondaryFont);
      SkFont := TSkFont.Create(Typeface, Panels[I].SecondaryFont.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].SecondaryFont.Color);
      Metrics := SkFont.Metrics;
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].SecondaryText, Panels[I].PanelRect.Left + X + PanelPadding, Y, SkFont, Paint);
    end;
  end;

  // Copy the Skia surface into the component buffer used by the base painter
  // Direct rendering to canvas - no conversion needed!
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbar.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
