//------------------------------------------------------------------------------
//  ERD.UI.SpinEdit
//
//  TOBDSpinEdit - themed numeric edit with up / down buttons, for
//  thresholds, intervals, counts and other numbers in OBD Studio forms.
//
//  It is a TOBDEdit, so it looks and sizes like the other edits. Value is
//  a Double; Decimals sets how it is shown and typed (0 = whole numbers).
//  MinValue / MaxValue clamp it (both 0 = no limit). The buttons repeat
//  while held; Up / Down step by Increment, Page Up / Page Down by ten
//  steps and the mouse wheel steps while the edit has focus. Typed text
//  is committed on Enter and when focus leaves.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.SpinEdit;

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
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Edits;

type
  /// <summary>Numeric edit with themed up / down buttons.</summary>
  TOBDSpinEdit = class(TOBDEdit)
  strict private
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FIncrement: Double;
    FDecimals: Integer;
    FUpdating: Boolean;
    FHotButton: Integer;
    FDownButton: Integer;
    FRepeat: TTimer;
    FOnValueChange: TNotifyEvent;
    procedure SetValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetIncrement(AValue: Double);
    procedure SetDecimals(AValue: Integer);
    function GetIntValue: Integer;
    procedure SetIntValue(AValue: Integer);
    function Clamp(AValue: Double): Double;
    function ButtonWidth: Integer;
    function ButtonAt(X, Y: Integer): Integer;
    procedure ShowValue;
    procedure Commit;
    procedure RepeatTick(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Reserves the button strip.</summary>
    /// <returns>Width of the button strip.</returns>
    function TrailingSpace: Integer; override;
    /// <summary>Paints the frame and the up / down buttons.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Takes typed text as the value when it parses.</summary>
    procedure Change; override;
    /// <summary>Lets only digits, the sign and the decimal separator
    /// through.</summary>
    /// <param name="Key">Typed character; #0 when rejected.</param>
    procedure KeyPress(var Key: Char); override;
    /// <summary>Steps with the arrow and page keys; Enter commits.
    /// </summary>
    /// <param name="Key">Virtual key; 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Commits typed text when focus leaves.</summary>
    procedure DoExit; override;
    /// <summary>Clamps the loaded value to the loaded limits.</summary>
    procedure Loaded; override;
    /// <summary>Presses a button and starts the auto-repeat.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Tracks the hot button.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Releases a pressed button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Steps once per notch while the edit has focus.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when the value stepped.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Fires <see cref="OnValueChange"/>.</summary>
    procedure ValueChanged; virtual;
  public
    /// <summary>Creates a spin edit with value 0, step 1.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Adds steps of <see cref="Increment"/>, clamped.</summary>
    /// <param name="ASteps">Number of steps; negative steps down.</param>
    procedure Step(ASteps: Integer);
    /// <summary>Value rounded to an integer.</summary>
    property IntValue: Integer read GetIntValue write SetIntValue;
  published
    /// <summary>Current value, clamped to MinValue / MaxValue.</summary>
    property Value: Double read FValue write SetValue;
    /// <summary>Lowest value; MinValue = MaxValue = 0 means no limit.
    /// </summary>
    property MinValue: Double read FMinValue write SetMinValue;
    /// <summary>Highest value; MinValue = MaxValue = 0 means no limit.
    /// </summary>
    property MaxValue: Double read FMaxValue write SetMaxValue;
    /// <summary>Step of the buttons, arrow keys and wheel.</summary>
    property Increment: Double read FIncrement write SetIncrement;
    /// <summary>Digits after the decimal separator (0..6).</summary>
    property Decimals: Integer read FDecimals write SetDecimals default 0;
    /// <summary>Numbers read right-aligned.</summary>
    property Alignment default taRightJustify;
    /// <summary>Fires on the main thread when Value changes, by typing,
    /// stepping or code.</summary>
    property OnValueChange: TNotifyEvent read FOnValueChange
      write FOnValueChange;
  end;

implementation

const
  BTN_NONE = 0;
  BTN_UP = 1;
  BTN_DOWN = 2;

{ TOBDSpinEdit }

constructor TOBDSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 120;
  FIncrement := 1;
  Alignment := taRightJustify;
  FRepeat := TTimer.Create(Self);
  FRepeat.Enabled := False;
  FRepeat.OnTimer := RepeatTick;
  ShowValue;
end;

function TOBDSpinEdit.Clamp(AValue: Double): Double;
begin
  Result := AValue;
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    Result := EnsureRange(Result, FMinValue, Max(FMinValue, FMaxValue));
  Result := RoundTo(Result, -FDecimals);
end;

procedure TOBDSpinEdit.ShowValue;
begin
  FUpdating := True;
  try
    if FDecimals = 0 then
      Text := FormatFloat('0', FValue)
    else
      Text := FormatFloat('0.' + StringOfChar('0', FDecimals), FValue);
  finally
    FUpdating := False;
  end;
end;

procedure TOBDSpinEdit.SetValue(AValue: Double);
begin
  AValue := Clamp(AValue);
  if SameValue(AValue, FValue) then
  begin
    ShowValue;
    Exit;
  end;
  FValue := AValue;
  ShowValue;
  ValueChanged;
end;

procedure TOBDSpinEdit.SetMinValue(AValue: Double);
begin
  FMinValue := AValue;
  if not (csLoading in ComponentState) then
    SetValue(FValue);
end;

procedure TOBDSpinEdit.SetMaxValue(AValue: Double);
begin
  FMaxValue := AValue;
  if not (csLoading in ComponentState) then
    SetValue(FValue);
end;

procedure TOBDSpinEdit.SetIncrement(AValue: Double);
begin
  if AValue > 0 then
    FIncrement := AValue;
end;

procedure TOBDSpinEdit.SetDecimals(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 6);
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  SetValue(FValue);
end;

function TOBDSpinEdit.GetIntValue: Integer;
begin
  Result := Round(FValue);
end;

procedure TOBDSpinEdit.SetIntValue(AValue: Integer);
begin
  SetValue(AValue);
end;

procedure TOBDSpinEdit.ValueChanged;
begin
  if not (csLoading in ComponentState) and Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

procedure TOBDSpinEdit.Step(ASteps: Integer);
begin
  if ReadOnly or not Enabled then
    Exit;
  Commit;
  SetValue(FValue + ASteps * FIncrement);
  SelStart := Length(Text);
end;

procedure TOBDSpinEdit.Commit;
var
  S: string;
  V: Double;
begin
  S := Trim(Text);
  S := S.Replace('.', FormatSettings.DecimalSeparator)
    .Replace(',', FormatSettings.DecimalSeparator);
  if TryStrToFloat(S, V) then
    SetValue(V)
  else
    ShowValue;
end;

procedure TOBDSpinEdit.Change;
var
  S: string;
  V: Double;
begin
  if not FUpdating then
  begin
    // Follow the typed text when it is a number in range; the text is
    // tidied up (clamped, formatted) when it is committed.
    S := Trim(Text).Replace('.', FormatSettings.DecimalSeparator)
      .Replace(',', FormatSettings.DecimalSeparator);
    if TryStrToFloat(S, V) and SameValue(V, Clamp(V)) and
      not SameValue(V, FValue) then
    begin
      FValue := V;
      ValueChanged;
    end;
  end;
  inherited Change;
end;

procedure TOBDSpinEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #1, #3, #8, #22, #24, #26, '0'..'9':
      ;
    '-':
      if ((FMinValue = 0) and (FMaxValue = 0)) or (FMinValue < 0) then
        Exit
      else
        Key := #0;
    '.', ',':
      if FDecimals > 0 then
        Key := FormatSettings.DecimalSeparator
      else
        Key := #0;
    #13:
      Key := #0;
  else
    Key := #0;
  end;
end;

procedure TOBDSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      Step(1);
    VK_DOWN:
      Step(-1);
    VK_PRIOR:
      Step(10);
    VK_NEXT:
      Step(-10);
    VK_RETURN:
      begin
        Commit;
        SelectAll;
        // Let a default button still see Enter.
        Exit;
      end;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDSpinEdit.Loaded;
begin
  inherited Loaded;
  FValue := Clamp(FValue);
  ShowValue;
end;

procedure TOBDSpinEdit.DoExit;
begin
  Commit;
  inherited DoExit;
end;

function TOBDSpinEdit.TrailingSpace: Integer;
begin
  Result := ButtonWidth;
end;

function TOBDSpinEdit.ButtonWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(32)
  else
    Result := ScaleValue(20);
end;

function TOBDSpinEdit.ButtonAt(X, Y: Integer): Integer;
begin
  if (X < Width - ButtonWidth) or (X >= Width) or (Y < 0) or (Y >= Height) then
    Result := BTN_NONE
  else if Y < Height div 2 then
    Result := BTN_UP
  else
    Result := BTN_DOWN;
end;

procedure TOBDSpinEdit.RepeatTick(Sender: TObject);
begin
  FRepeat.Interval := 60;
  if FDownButton = BTN_UP then
    Step(1)
  else if FDownButton = BTN_DOWN then
    Step(-1)
  else
    FRepeat.Enabled := False;
end;

procedure TOBDSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled or ReadOnly then
    Exit;
  FDownButton := ButtonAt(X, Y);
  if FDownButton = BTN_NONE then
    Exit;
  if FDownButton = BTN_UP then
    Step(1)
  else
    Step(-1);
  FRepeat.Interval := 400;
  FRepeat.Enabled := True;
  Invalidate;
end;

procedure TOBDSpinEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Hot: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  Hot := ButtonAt(X, Y);
  if Hot <> FHotButton then
  begin
    FHotButton := Hot;
    Invalidate;
  end;
end;

procedure TOBDSpinEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FRepeat.Enabled := False;
  if FDownButton <> BTN_NONE then
  begin
    FDownButton := BTN_NONE;
    Invalidate;
  end;
end;

procedure TOBDSpinEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotButton <> BTN_NONE then
  begin
    FHotButton := BTN_NONE;
    Invalidate;
  end;
end;

function TOBDSpinEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  // Only a focused spin edit takes the wheel, so scrolling a page past
  // it does not change the value.
  if Result or not Focused or (WheelDelta = 0) then
    Exit;
  Step(Sign(WheelDelta));
  Result := True;
end;

procedure TOBDSpinEdit.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  BW, X0, Mid, CX, H: Integer;
  Ink: TColor;
  B: Integer;
  R: TRect;
  CY, D: Single;
begin
  inherited PaintControl(ACanvas);
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    BW := ButtonWidth;
    X0 := Width - BW;
    Mid := Height div 2;
    H := P.S(1);
    // Divider between the text and the buttons.
    P.FillRect(Rect(X0, H + P.S(3), X0 + H, Height - H - P.S(3)),
      Palette.NeutralLight);
    CX := X0 + BW div 2;
    D := P.SF(3.5);
    for B := BTN_UP to BTN_DOWN do
    begin
      if B = BTN_UP then
        R := Rect(X0 + H, H * 2, Width - H * 2, Mid)
      else
        R := Rect(X0 + H, Mid, Width - H * 2, Height - H * 2);
      if not Enabled or ReadOnly then
        Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6)
      else if FDownButton = B then
      begin
        P.FillRect(R, OBDMixColor(Palette.Accent, Palette.GaugeFace,
          0.25));
        Ink := P.AccentText;
      end
      else if FHotButton = B then
      begin
        P.FillRect(R, OBDMixColor(Palette.NeutralLight, Palette.GaugeFace,
          0.5));
        Ink := Palette.ForegroundText;
      end
      else
        Ink := Palette.Subtle;
      CY := (R.Top + R.Bottom) / 2;
      if B = BTN_UP then
        P.Lines([OBDPointF(CX - D, CY + D / 2), OBDPointF(CX, CY - D / 2),
          OBDPointF(CX + D, CY + D / 2)], Ink, P.SF(1.5))
      else
        P.Lines([OBDPointF(CX - D, CY - D / 2), OBDPointF(CX, CY + D / 2),
          OBDPointF(CX + D, CY - D / 2)], Ink, P.SF(1.5));
    end;
  finally
    P.Free;
  end;
end;

end.
