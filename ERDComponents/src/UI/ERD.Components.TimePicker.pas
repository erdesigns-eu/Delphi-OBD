unit ERD.Components.TimePicker;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint;
type
  TERDTimeFormat = (tf24Hour, tf12Hour);
  TERDTimeSegment = (tsHour, tsMinute, tsSecond, tsPeriod);
  /// <summary>Segment editor with themed selection; no native edit caret or mask.</summary>
  TERDTimePicker = class(TERDCustomControl)
  private
    FTime: TTime;
    FTimeFormat: TERDTimeFormat;
    FShowSeconds, FReadOnly, FShowSpinButtons: Boolean;
    FSegment: TERDTimeSegment;
    FDigits: string;
    FInputTick: UInt64;
    FHotSpin, FPressedSpin, FWheelRemainder: Integer;
    FRepeatTimer: TTimer;
    FOnChange: TNotifyEvent;
    procedure SetTime(Value: TTime);
    procedure SetTimeFormat(Value: TERDTimeFormat);
    procedure SetShowSeconds(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowSpinButtons(Value: Boolean);
    procedure SetActiveSegment(Value: TERDTimeSegment);
    function VisibleSegment(Value: TERDTimeSegment): Boolean;
    function SegmentText(Value: TERDTimeSegment): string;
    function SegmentBounds(P: TERDPainter; Segment: TERDTimeSegment): TRect;
    function SegmentMaximum: Integer;
    function SpinAt(X, Y: Integer): Integer;
    procedure MoveSegment(Delta: Integer);
    procedure EnterDigit(Digit: Char);
    procedure AssignSegment(Value: Integer);
    procedure ResetInput;
    procedure StopSpin;
    procedure RepeatSpin(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DensityChanged; override;
    /// <summary>Changes the active field with wrap; no carry into other fields.</summary>
    procedure Step(Delta: Integer);
    function SegmentRect(Segment: TERDTimeSegment): TRect;
    function DisplayText: string;
    property ActiveSegment: TERDTimeSegment read FSegment write SetActiveSegment;
  published
    property Time: TTime read FTime write SetTime;
    property TimeFormat: TERDTimeFormat read FTimeFormat write SetTimeFormat default tf24Hour;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds default False;
    property ShowSpinButtons: Boolean read FShowSpinButtons write SetShowSpinButtons default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property AutoSize default True;
    property TabStop default True;
    property Density;
    property ParentDensity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
implementation
constructor TERDTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  FTimeFormat := tf24Hour;
  FShowSpinButtons := True;
  FSegment := tsHour;
  AutoSize := True;
  TabStop := True;
  FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.Enabled := False;
  FRepeatTimer.OnTimer := RepeatSpin;
  SetBounds(Left, Top, 180, ScaleValue(Metrics.Edit));
end;
procedure TERDTimePicker.SetTime(Value: TTime);
var H, M, S, MS: Word;
begin
  if IsNan(Value) or IsInfinite(Value) then
    raise EArgumentException.Create('Time must be finite');
  // TDateTime uses the absolute fractional part for times before its epoch.
  DecodeTime(Frac(Abs(Value)), H, M, S, MS);
  Value := EncodeTime(H, M, S, MS);
  if FTime = Value then Exit;
  FTime := Value;
  Invalidate;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then FOnChange(Self);
end;
procedure TERDTimePicker.ResetInput;
begin
  FDigits := '';
  FInputTick := 0;
end;
function TERDTimePicker.VisibleSegment(Value: TERDTimeSegment): Boolean;
begin
  case Value of
    tsSecond: Result := FShowSeconds;
    tsPeriod: Result := FTimeFormat = tf12Hour;
  else Result := True;
  end;
end;
procedure TERDTimePicker.SetActiveSegment(Value: TERDTimeSegment);
begin
  if not VisibleSegment(Value) then Value := tsMinute;
  FSegment := Value;
  ResetInput;
  Invalidate;
end;
procedure TERDTimePicker.SetTimeFormat(Value: TERDTimeFormat);
begin
  if FTimeFormat = Value then Exit;
  FTimeFormat := Value;
  SetActiveSegment(FSegment);
end;
procedure TERDTimePicker.SetShowSeconds(Value: Boolean);
begin
  if FShowSeconds = Value then Exit;
  FShowSeconds := Value;
  SetActiveSegment(FSegment);
end;
procedure TERDTimePicker.SetReadOnly(Value: Boolean);
begin
  if FReadOnly = Value then Exit;
  FReadOnly := Value;
  StopSpin;
  ResetInput;
  Invalidate;
end;
procedure TERDTimePicker.SetShowSpinButtons(Value: Boolean);
begin
  if FShowSpinButtons = Value then Exit;
  FShowSpinButtons := Value;
  StopSpin;
  Invalidate;
end;
function TERDTimePicker.SegmentText(Value: TERDTimeSegment): string;
var H, M, S, MS: Word; N: Integer;
begin
  DecodeTime(FTime, H, M, S, MS);
  N := 0;
  case Value of
    tsHour:
      begin
        N := H;
        if FTimeFormat = tf12Hour then
        begin N := H mod 12; if N = 0 then N := 12 end;
      end;
    tsMinute: N := M;
    tsSecond: N := S;
    tsPeriod:
      begin
        if H < 12 then Result := FormatSettings.TimeAMString
        else Result := FormatSettings.TimePMString;
        if Result = '' then
          if H < 12 then Result := 'AM' else Result := 'PM';
        Exit;
      end;
  end;
  Result := Format('%.2d', [N]);
end;
function TERDTimePicker.DisplayText: string;
var Separator: string;
begin
  Separator := FormatSettings.TimeSeparator;
  if Separator = '' then Separator := ':';
  Result := SegmentText(tsHour) + Separator + SegmentText(tsMinute);
  if FShowSeconds then Result := Result + Separator + SegmentText(tsSecond);
  if FTimeFormat = tf12Hour then Result := Result + ' ' + SegmentText(tsPeriod);
end;
function TERDTimePicker.SegmentBounds(P: TERDPainter; Segment: TERDTimeSegment): TRect;
var Current: TERDTimeSegment; X, W, Gap: Integer; Separator: string;
begin
  Result := Rect(0, 0, 0, 0);
  if not VisibleSegment(Segment) then Exit;
  X := P.S(8);
  Separator := FormatSettings.TimeSeparator;
  if Separator = '' then Separator := ':';
  for Current := Low(TERDTimeSegment) to High(TERDTimeSegment) do
  begin
    if not VisibleSegment(Current) then Continue;
    W := Ceil(P.TextWidth(SegmentText(Current), ERD_TEXT_BODY, twRegular)) + P.S(6);
    if Current = Segment then
    begin
      Result := Rect(X, P.S(3), X + W, Height - P.S(3));
      Exit;
    end;
    if Current = tsSecond then Gap := P.S(5)
    else if (Current = tsMinute) and not FShowSeconds then Gap := P.S(5)
    else Gap := Ceil(P.TextWidth(Separator, ERD_TEXT_BODY, twRegular));
    Inc(X, W + Gap);
  end;
end;
function TERDTimePicker.SegmentRect(Segment: TERDTimeSegment): TRect;
var Bitmap: TBitmap; P: TERDPainter;
begin
  // Public geometry must also work before a parent/window handle exists.
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(1, 1);
    P := TERDPainter.Create(Bitmap.Canvas, Palette, ScaleValue(96));
    try Result := SegmentBounds(P, Segment) finally P.Free end;
  finally Bitmap.Free end;
end;
function TERDTimePicker.SegmentMaximum: Integer;
begin
  if FSegment = tsHour then
    if FTimeFormat = tf24Hour then Result := 23 else Result := 12
  else Result := 59;
end;
procedure TERDTimePicker.AssignSegment(Value: Integer);
var H, M, S, MS: Word;
begin
  DecodeTime(FTime, H, M, S, MS);
  case FSegment of
    tsHour:
      if FTimeFormat = tf12Hour then H := (H div 12) * 12 + Value mod 12
      else H := Value;
    tsMinute: M := Value;
    tsSecond: S := Value;
    tsPeriod: H := H mod 12 + (Value mod 2) * 12;
  end;
  Time := EncodeTime(H, M, S, MS);
end;
procedure TERDTimePicker.Step(Delta: Integer);
var H, M, S, MS: Word; Value, Limit, Minimum: Integer;
begin
  if not Enabled or FReadOnly then Exit;
  ResetInput;
  DecodeTime(FTime, H, M, S, MS);
  Minimum := 0;
  Limit := SegmentMaximum + 1;
  case FSegment of
    tsHour:
      begin
        Value := H;
        if FTimeFormat = tf12Hour then
        begin Value := H mod 12; if Value = 0 then Value := 12; Minimum := 1; Limit := 12 end;
      end;
    tsMinute: Value := M;
    tsSecond: Value := S;
  else begin Value := H div 12; Limit := 2 end;
  end;
  Value := (Value - Minimum + Delta mod Limit) mod Limit;
  if Value < 0 then Inc(Value, Limit);
  AssignSegment(Value + Minimum);
end;
procedure TERDTimePicker.MoveSegment(Delta: Integer);
var Next: Integer;
begin
  Next := Ord(FSegment);
  repeat
    Next := (Next + Delta + 4) mod 4;
  until VisibleSegment(TERDTimeSegment(Next));
  SetActiveSegment(TERDTimeSegment(Next));
end;
procedure TERDTimePicker.EnterDigit(Digit: Char);
var Value, Minimum: Integer;
begin
  if FSegment = tsPeriod then Exit;
  if (GetTickCount64 - FInputTick > 1500) or (Length(FDigits) >= 2) then ResetInput;
  FInputTick := GetTickCount64;
  if not TryStrToInt(FDigits + Digit, Value) then Exit;
  if Value > SegmentMaximum then Exit;
  FDigits := FDigits + Digit;
  Minimum := 0;
  if (FSegment = tsHour) and (FTimeFormat = tf12Hour) then Minimum := 1;
  if Value >= Minimum then AssignSegment(Value);
  if (Length(FDigits) = 2) or (Value > SegmentMaximum div 10) then MoveSegment(1);
end;
procedure TERDTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Shift <> [] then Exit;
  case Key of
    VK_LEFT: MoveSegment(-1);
    VK_RIGHT: MoveSegment(1);
    VK_UP: Step(1);
    VK_DOWN: Step(-1);
    VK_HOME:
      if not FReadOnly and Enabled then
      begin ResetInput; if (FSegment = tsHour) and (FTimeFormat = tf12Hour) then AssignSegment(1) else AssignSegment(0) end;
    VK_END:
      if not FReadOnly and Enabled then
      begin ResetInput; if FSegment = tsPeriod then AssignSegment(1) else AssignSegment(SegmentMaximum) end;
    VK_ESCAPE, VK_BACK: ResetInput;
  else Exit;
  end;
  Key := 0;
end;
procedure TERDTimePicker.KeyPress(var Key: Char);
var AMText, PMText: string;
begin
  inherited;
  if FReadOnly or not Enabled then Exit;
  if (Key >= '0') and (Key <= '9') then begin EnterDigit(Key); Key := #0 end
  else if (Key = ':') or (Key = FormatSettings.TimeSeparator) then
  begin MoveSegment(1); Key := #0 end
  else if FSegment = tsPeriod then
  begin
    AMText := LowerCase(FormatSettings.TimeAMString);
    PMText := LowerCase(FormatSettings.TimePMString);
    if (Key = 'a') or (Key = 'A') or ((AMText <> '') and SameText(Key, AMText[1])) then
      AssignSegment(0)
    else if (Key = 'p') or (Key = 'P') or ((PMText <> '') and SameText(Key, PMText[1])) then
      AssignSegment(1);
    Key := #0;
  end;
end;
function TERDTimePicker.SpinAt(X, Y: Integer): Integer;
begin
  Result := 0;
  if FShowSpinButtons and (X >= Width - ScaleValue(22)) and (X < Width) and
    (Y >= 0) and (Y < Height) then
    if Y < Height div 2 then Result := 1 else Result := -1;
end;
procedure TERDTimePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Segment: TERDTimeSegment;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then Exit;
  if CanFocus then SetFocus;
  ResetInput;
  FHotSpin := SpinAt(X, Y);
  if (FHotSpin <> 0) and not FReadOnly then
  begin
    FPressedSpin := FHotSpin;
    MouseCapture := True;
    FRepeatTimer.Interval := 400;
    FRepeatTimer.Enabled := True;
    Invalidate;
    Step(FPressedSpin);
    Exit;
  end;
  for Segment := Low(TERDTimeSegment) to High(TERDTimeSegment) do
    if VisibleSegment(Segment) and PtInRect(SegmentRect(Segment), Point(X, Y)) then
    begin SetActiveSegment(Segment); Exit end;
end;
procedure TERDTimePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var Hot: Integer;
begin
  inherited;
  Hot := SpinAt(X, Y);
  if FHotSpin <> Hot then begin FHotSpin := Hot; Invalidate end;
end;
procedure TERDTimePicker.StopSpin;
begin
  FPressedSpin := 0;
  if FRepeatTimer <> nil then FRepeatTimer.Enabled := False;
  if MouseCapture then MouseCapture := False;
end;
procedure TERDTimePicker.RepeatSpin(Sender: TObject);
begin
  FRepeatTimer.Interval := 80;
  if (FPressedSpin <> 0) and (FPressedSpin = FHotSpin) then Step(FPressedSpin);
end;
procedure TERDTimePicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StopSpin;
  inherited;
  Invalidate;
end;
procedure TERDTimePicker.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHotSpin := 0;
  Invalidate;
end;
procedure TERDTimePicker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  StopSpin;
  ResetInput;
  Invalidate;
end;
procedure TERDTimePicker.WMCancelMode(var Message: TMessage);
begin
  StopSpin;
  inherited;
  Invalidate;
end;
procedure TERDTimePicker.WMCaptureChanged(var Message: TMessage);
begin
  FPressedSpin := 0;
  if FRepeatTimer <> nil then FRepeatTimer.Enabled := False;
  inherited;
  Invalidate;
end;
procedure TERDTimePicker.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if not Focused or not Enabled or FReadOnly then begin inherited; Exit end;
  Inc(FWheelRemainder, Message.WheelDelta);
  while Abs(FWheelRemainder) >= WHEEL_DELTA do
  begin
    if FWheelRemainder > 0 then
    begin Dec(FWheelRemainder, WHEEL_DELTA); Step(1) end
    else begin Inc(FWheelRemainder, WHEEL_DELTA); Step(-1) end;
  end;
  Message.Result := 1;
end;
procedure TERDTimePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;
procedure TERDTimePicker.DoEnter;
begin
  inherited;
  ResetInput;
  Invalidate;
end;
procedure TERDTimePicker.DoExit;
begin
  StopSpin;
  ResetInput;
  FWheelRemainder := 0;
  inherited;
  Invalidate;
end;
function TERDTimePicker.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;
procedure TERDTimePicker.DensityChanged;
begin
  inherited;
  if AutoSize then AdjustSize;
end;
procedure TERDTimePicker.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Segment: TERDTimeSegment; R, ButtonRect: TRect;
  Ink, Fill: TColor; CY, Button, X: Integer; Separator: string;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(ClientRect, Focused, Enabled);
    Separator := FormatSettings.TimeSeparator;
    if Separator = '' then Separator := ':';
    for Segment := Low(TERDTimeSegment) to High(TERDTimeSegment) do
    begin
      if not VisibleSegment(Segment) then Continue;
      R := SegmentBounds(P, Segment);
      if FShowSpinButtons then R.Right := Min(R.Right, Width - P.S(22));
      if R.Right <= R.Left then Continue;
      Ink := Palette.ForegroundText;
      if not Enabled then Ink := P.DisabledText
      else if Focused and (Segment = FSegment) then
      begin P.FillRect(R, Palette.Accent); Ink := P.ContrastInk(Palette.Accent) end;
      P.Text(R.Left + P.S(3), Height div 2, SegmentText(Segment), ERD_TEXT_BODY,
        Ink, twRegular, taLeftJustify, R.Width - P.S(6));
      if (Segment = tsHour) or ((Segment = tsMinute) and FShowSeconds) then
        P.Text(R.Right, Height div 2, Separator, ERD_TEXT_BODY,
          Palette.Subtle, twRegular, taLeftJustify);
    end;
    if FShowSpinButtons then
    begin
      X := Width - P.S(22);
      P.VLine(X, P.S(3), Height - P.S(6), Palette.NeutralLight);
      for Button := -1 to 1 do
      begin
        if Button = 0 then Continue;
        if Button = 1 then ButtonRect := Rect(X + 1, 1, Width - 1, Height div 2)
        else ButtonRect := Rect(X + 1, Height div 2, Width - 1, Height - 1);
        CY := (ButtonRect.Top + ButtonRect.Bottom) div 2;
        Ink := Palette.Subtle;
        if not Enabled or FReadOnly then Ink := P.DisabledText
        else if FHotSpin = Button then
        begin
          if FPressedSpin = Button then Fill := P.Tint(Palette.Accent, 0.2)
          else Fill := P.Tint(Palette.Accent, 0.1);
          P.FillRect(ButtonRect, Fill);
          Ink := Palette.Accent;
        end;
        if Button = 1 then
          P.Lines([ERDPointF(Width - P.S(15), CY + P.S(2)),
            ERDPointF(Width - P.S(11), CY - P.S(2)),
            ERDPointF(Width - P.S(7), CY + P.S(2))], Ink, 1)
        else P.GlyphChevron(Width - P.S(11), CY, Ink, True);
      end;
    end;
  finally P.Free end;
end;
end.
