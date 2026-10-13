//------------------------------------------------------------------------------
//  ERD.Components.Calendar
//
//  TERDCalendar - themed month calendar; TERDDatePicker - date field that
//  drops the calendar down, like a combo box. For service dates, MOT /
//  inspection due dates, log filters and report ranges.
//
//  The calendar shows a month header with previous / next arrows, the
//  weekday initials and a six-week grid. The selected day is filled with
//  the accent colour, today has a ring and days of the neighbouring
//  months are dimmed. Click a day, or use the arrow keys (day / week),
//  Page Up / Page Down (month), Home / End (first / last day of the
//  month) and Enter. FirstDay picks Monday (default) or Sunday weeks.
//  Its size follows the density cell size (32 px desktop, 44 px tablet).
//
//  The date picker shows the date in the edit frame (DisplayFormat, or
//  the short date format of the locale). Click it, press F4 or Alt+Down
//  to open the calendar; Enter or a click on a day takes the date, Esc or
//  a click elsewhere closes it. While closed, Up / Down change the date
//  by a day.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.Calendar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.AppEvnts,
  ERD.Components.Types,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>First day of the calendar week.</summary>
  TERDFirstDayOfWeek = (
    /// <summary>Weeks start on Monday (ISO 8601).</summary>
    fdMonday,
    /// <summary>Weeks start on Sunday.</summary>
    fdSunday
  );

  /// <summary>Themed month calendar.</summary>
  TERDCalendar = class(TERDCustomControl)
  strict private
    FDate: TDate;
    FViewYear: Word;
    FViewMonth: Word;
    FFirstDay: TERDFirstDayOfWeek;
    FHot: Integer;
    FOnChange: TNotifyEvent;
    FOnDateClick: TNotifyEvent;
    procedure SetDate(AValue: TDate);
    procedure SetFirstDay(AValue: TERDFirstDayOfWeek);
    function Pad: Integer;
    function CellSize: Integer;
    function HeaderHeight: Integer;
    function WeekdayHeight: Integer;
    function GridStart: TDate;
    function CellRect(AIndex: Integer): TRect;
    function PrevRect: TRect;
    function NextRect: TRect;
    function HitTest(X, Y: Integer): Integer;
    procedure SetHot(AValue: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Colour the calendar is painted on.</summary>
    /// <returns>The surface colour behind the calendar.</returns>
    function FaceColor: TColor; virtual;
    /// <summary>Paints the header, weekday row and day grid.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Size follows the density cell size.</summary>
    /// <param name="NewWidth">Set to the calendar width.</param>
    /// <param name="NewHeight">Set to the calendar height.</param>
    /// <returns>Always True.</returns>
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    /// <summary>Picks a day or turns the month.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Tracks the hot day or arrow.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Calendar keys; see <see cref="HandleKey"/>.</summary>
    /// <param name="Key">Virtual key; 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Turns the month while the calendar has focus.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when the month turned.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Repaints the focus ring.</summary>
    procedure DoEnter; override;
    /// <summary>Repaints the focus ring.</summary>
    procedure DoExit; override;
    /// <summary>Fires <see cref="OnChange"/>.</summary>
    procedure Changed; virtual;
    /// <summary>Fires <see cref="OnDateClick"/>.</summary>
    procedure DateClicked; virtual;
  public
    /// <summary>Creates a calendar showing today.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Applies the density cell size.</summary>
    procedure DensityChanged; override;
    /// <summary>Shows a month without changing the date.</summary>
    /// <param name="AYear">Year.</param>
    /// <param name="AMonth">Month, 1..12.</param>
    procedure ShowMonth(AYear, AMonth: Word);
    /// <summary>Handles a calendar key: arrows move a day / week, Page
    /// Up / Page Down a month, Home / End go to the first / last day of
    /// the month, Enter and Space click the date.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Shift state.</param>
    /// <returns>True when the key was used.</returns>
    function HandleKey(Key: Word; Shift: TShiftState): Boolean;
    /// <summary>Year of the month shown.</summary>
    property ViewYear: Word read FViewYear;
    /// <summary>Month shown, 1..12.</summary>
    property ViewMonth: Word read FViewMonth;
  published
    /// <summary>Selected date; setting it shows its month.</summary>
    property Date: TDate read FDate write SetDate;
    /// <summary>First day of the week.</summary>
    property FirstDay: TERDFirstDayOfWeek read FFirstDay write SetFirstDay
      default fdMonday;
    /// <summary>Size follows the density.</summary>
    property AutoSize default True;
    /// <summary>Takes keyboard focus with Tab.</summary>
    property TabStop default True;
    /// <summary>Desktop or tablet cell size.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires on the main thread when Date changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires on the main thread when a day is clicked or Enter
    /// is pressed.</summary>
    property OnDateClick: TNotifyEvent read FOnDateClick write FOnDateClick;
  end;

  /// <summary>Date field with a drop-down calendar.</summary>
  TERDDatePicker = class(TERDCustomControl)
  strict private
    FDate: TDate;
    FDisplayFormat: string;
    FFirstDay: TERDFirstDayOfWeek;
    FPopup: TERDCalendar;
    FAppEvents: TApplicationEvents;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure SetDate(AValue: TDate);
    procedure SetDisplayFormat(const AValue: string);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure AppDeactivate(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    /// <summary>Paints the frame, the date and the calendar glyph.
    /// </summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Height follows the density edit height.</summary>
    /// <param name="NewWidth">Proposed width.</param>
    /// <param name="NewHeight">Set to the edit height.</param>
    /// <returns>Always True.</returns>
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    /// <summary>Opens or closes the calendar.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Steps the date, or drives the open calendar.</summary>
    /// <param name="Key">Virtual key; 0 when handled.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Repaints the focus line.</summary>
    procedure DoEnter; override;
    /// <summary>Closes the calendar.</summary>
    procedure DoExit; override;
    /// <summary>Fires <see cref="OnChange"/>.</summary>
    procedure Changed; virtual;
  public
    /// <summary>Creates a picker showing today.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Applies the density height.</summary>
    procedure DensityChanged; override;
    /// <summary>Opens the calendar below (or above) the field.</summary>
    procedure DropDown;
    /// <summary>Closes the calendar.</summary>
    /// <param name="AAccept">True takes the date picked in the calendar.
    /// </param>
    procedure CloseUp(AAccept: Boolean);
    /// <summary>True while the calendar is open.</summary>
    /// <returns>Open flag.</returns>
    function IsOpen: Boolean;
    /// <summary>The date as shown in the field.</summary>
    /// <returns>Formatted date.</returns>
    function DisplayText: string;
  published
    /// <summary>Selected date.</summary>
    property Date: TDate read FDate write SetDate;
    /// <summary>FormatDateTime pattern for the field (e.g.
    /// 'dd-mm-yyyy'); empty uses the short date format of the locale.
    /// </summary>
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    /// <summary>First day of the week in the calendar.</summary>
    property FirstDay: TERDFirstDayOfWeek read FFirstDay write FFirstDay
      default fdMonday;
    /// <summary>Height follows the density.</summary>
    property AutoSize default True;
    /// <summary>Takes keyboard focus with Tab.</summary>
    property TabStop default True;
    /// <summary>Desktop or tablet size.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires on the main thread when Date changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires on the main thread before the calendar opens.
    /// </summary>
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    /// <summary>Fires on the main thread after the calendar closed.
    /// </summary>
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

type
  // The drop-down calendar: a no-activate top-level window on the card
  // face, owned (and themed) by the picker.
  TERDCalendarPopup = class(TERDCalendar)
  strict private
    FPicker: TERDDatePicker;
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function FaceColor: TColor; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure DateClicked; override;
  public
    constructor CreatePopup(APicker: TERDDatePicker);
  end;

const
  HIT_NONE = -1;
  HIT_PREV = -2;
  HIT_NEXT = -3;
  CELL_COUNT = 42;

function TodayDate: TDate;
begin
  Result := Trunc(Now);
end;

{ TERDCalendar }

constructor TERDCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks];
  FHot := HIT_NONE;
  FDate := TodayDate;
  FViewYear := YearOf(FDate);
  FViewMonth := MonthOf(FDate);
  Width := 240;
  Height := 260;
  TabStop := True;
  AutoSize := True;
end;

function TERDCalendar.FaceColor: TColor;
begin
  Result := EffectiveBackground;
end;

procedure TERDCalendar.SetDate(AValue: TDate);
begin
  AValue := Trunc(AValue);
  FViewYear := YearOf(AValue);
  FViewMonth := MonthOf(AValue);
  if SameValue(FDate, AValue) then
  begin
    Invalidate;
    Exit;
  end;
  FDate := AValue;
  Invalidate;
  Changed;
end;

procedure TERDCalendar.SetFirstDay(AValue: TERDFirstDayOfWeek);
begin
  if FFirstDay = AValue then
    Exit;
  FFirstDay := AValue;
  Invalidate;
end;

procedure TERDCalendar.ShowMonth(AYear, AMonth: Word);
begin
  if (AMonth < 1) or (AMonth > 12) or (AYear < 1) or (AYear > 9999) then
    Exit;
  FViewYear := AYear;
  FViewMonth := AMonth;
  FHot := HIT_NONE;
  Invalidate;
end;

procedure TERDCalendar.Changed;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDCalendar.DateClicked;
begin
  if Assigned(FOnDateClick) then
    FOnDateClick(Self);
end;

function TERDCalendar.Pad: Integer;
begin
  Result := ScaleValue(8);
end;

function TERDCalendar.CellSize: Integer;
begin
  Result := ScaleValue(Metrics.Cell);
end;

function TERDCalendar.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Cell + 4);
end;

function TERDCalendar.WeekdayHeight: Integer;
begin
  Result := ScaleValue(Round(Metrics.Cell * 0.75));
end;

function TERDCalendar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewWidth := 2 * Pad + 7 * CellSize;
  NewHeight := 2 * Pad + HeaderHeight + WeekdayHeight + 6 * CellSize;
end;

procedure TERDCalendar.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TERDCalendar.GridStart: TDate;
var
  First: TDate;
  FirstIso, Offset: Integer;
begin
  First := EncodeDate(FViewYear, FViewMonth, 1);
  if FFirstDay = fdSunday then
    FirstIso := 7
  else
    FirstIso := 1;
  Offset := (DayOfTheWeek(First) - FirstIso + 7) mod 7;
  Result := First - Offset;
end;

function TERDCalendar.CellRect(AIndex: Integer): TRect;
var
  C, X0, Y0: Integer;
begin
  C := CellSize;
  // Centre the grid when the control is wider than the auto size.
  X0 := (Width - 7 * C) div 2;
  Y0 := Pad + HeaderHeight + WeekdayHeight;
  Result := Rect(X0 + (AIndex mod 7) * C, Y0 + (AIndex div 7) * C,
    X0 + (AIndex mod 7 + 1) * C, Y0 + (AIndex div 7 + 1) * C);
end;

function TERDCalendar.PrevRect: TRect;
var
  X0: Integer;
begin
  X0 := (Width - 7 * CellSize) div 2;
  Result := Rect(X0, Pad, X0 + CellSize, Pad + HeaderHeight);
end;

function TERDCalendar.NextRect: TRect;
var
  X1: Integer;
begin
  X1 := (Width + 7 * CellSize) div 2;
  Result := Rect(X1 - CellSize, Pad, X1, Pad + HeaderHeight);
end;

function TERDCalendar.HitTest(X, Y: Integer): Integer;
var
  I: Integer;
begin
  if PtInRect(PrevRect, Point(X, Y)) then
    Exit(HIT_PREV);
  if PtInRect(NextRect, Point(X, Y)) then
    Exit(HIT_NEXT);
  for I := 0 to CELL_COUNT - 1 do
    if PtInRect(CellRect(I), Point(X, Y)) then
      Exit(I);
  Result := HIT_NONE;
end;

procedure TERDCalendar.SetHot(AValue: Integer);
begin
  if FHot = AValue then
    Exit;
  FHot := AValue;
  Invalidate;
end;

procedure TERDCalendar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetHot(HIT_NONE);
end;

procedure TERDCalendar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TERDCalendar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDCalendar.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TERDCalendar.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TERDCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hit: Integer;
  Next: TDate;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if TabStop and CanFocus and not Focused then
    SetFocus;
  Hit := HitTest(X, Y);
  case Hit of
    HIT_NONE:
      ;
    HIT_PREV:
      begin
        Next := IncMonth(EncodeDate(FViewYear, FViewMonth, 1), -1);
        ShowMonth(YearOf(Next), MonthOf(Next));
      end;
    HIT_NEXT:
      begin
        Next := IncMonth(EncodeDate(FViewYear, FViewMonth, 1), 1);
        ShowMonth(YearOf(Next), MonthOf(Next));
      end;
  else
    SetDate(GridStart + Hit);
    DateClicked;
  end;
end;

procedure TERDCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  SetHot(HitTest(X, Y));
end;

function TERDCalendar.HandleKey(Key: Word; Shift: TShiftState): Boolean;
var
  D: TDate;
begin
  Result := True;
  D := FDate;
  case Key of
    VK_LEFT:
      D := D - 1;
    VK_RIGHT:
      D := D + 1;
    VK_UP:
      D := D - 7;
    VK_DOWN:
      D := D + 7;
    VK_PRIOR:
      D := IncMonth(D, -1);
    VK_NEXT:
      D := IncMonth(D, 1);
    VK_HOME:
      D := StartOfTheMonth(D);
    VK_END:
      D := Trunc(EndOfTheMonth(D));
    VK_RETURN, VK_SPACE:
      begin
        DateClicked;
        Exit;
      end;
  else
    Exit(False);
  end;
  SetDate(D);
end;

procedure TERDCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key <> 0) and HandleKey(Key, Shift) then
    Key := 0;
end;

function TERDCalendar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Next: TDate;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or not Focused or (WheelDelta = 0) then
    Exit;
  Next := IncMonth(EncodeDate(FViewYear, FViewMonth, 1), -Sign(WheelDelta));
  ShowMonth(YearOf(Next), MonthOf(Next));
  Result := True;
end;

procedure TERDCalendar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Face, Ink, Dim, Fill: TColor;
  I, Col, DayIdx, FirstIso, CY: Integer;
  R: TRect;
  D, Today: TDate;
  CellX, CellY, Size: Single;
  Title, DayName: string;

  procedure Arrow(const AR: TRect; ALeft, AHot: Boolean);
  var
    AX, AY, S: Single;
  begin
    if AHot then
      P.RoundRect(AR.Left + P.S(2), AR.Top + P.S(2), AR.Width - P.S(4),
        AR.Height - P.S(4), P.SF(6), ERDMixColor(Palette.NeutralLight, Face,
        0.6), clNone);
    AX := (AR.Left + AR.Right) / 2;
    AY := (AR.Top + AR.Bottom) / 2;
    S := P.SF(4);
    if ALeft then
      P.Lines([ERDPointF(AX + S / 2, AY - S), ERDPointF(AX - S / 2, AY),
        ERDPointF(AX + S / 2, AY + S)], Ink, P.SF(1.5))
    else
      P.Lines([ERDPointF(AX - S / 2, AY - S), ERDPointF(AX + S / 2, AY),
        ERDPointF(AX - S / 2, AY + S)], Ink, P.SF(1.5));
  end;

begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Face := FaceColor;
    P.FillRect(Rect(0, 0, Width, Height), Face);
    if Enabled then
      Ink := Palette.ForegroundText
    else
      Ink := ERDMixColor(Palette.Subtle, Face, 0.6);
    Dim := ERDMixColor(Palette.Subtle, Face, 0.55);

    // Header: arrows and "Month Year".
    Arrow(PrevRect, True, FHot = HIT_PREV);
    Arrow(NextRect, False, FHot = HIT_NEXT);
    Title := FormatSettings.LongMonthNames[FViewMonth] + ' ' +
      IntToStr(FViewYear);
    P.Text(Width div 2, Pad + HeaderHeight div 2, Title, ERD_TEXT_HEADING,
      Ink, twSemibold, taCenter);

    // Weekday initials.
    if FFirstDay = fdSunday then
      FirstIso := 7
    else
      FirstIso := 1;
    CY := Pad + HeaderHeight + WeekdayHeight div 2;
    for Col := 0 to 6 do
    begin
      // ISO weekday of the column (1 = Monday), then the VCL day index
      // (1 = Sunday).
      DayIdx := ((FirstIso - 1 + Col) mod 7) + 1;
      DayIdx := (DayIdx mod 7) + 1;
      DayName := AnsiUpperCase(Copy(FormatSettings.ShortDayNames[DayIdx],
        1, 2));
      R := CellRect(Col);
      P.Text((R.Left + R.Right) div 2, CY, DayName, ERD_TEXT_CAPS,
        Palette.Subtle, twSemibold, taCenter);
    end;

    // Day grid.
    Today := TodayDate;
    Size := CellSize - P.SF(6);
    for I := 0 to CELL_COUNT - 1 do
    begin
      D := GridStart + I;
      R := CellRect(I);
      CellX := (R.Left + R.Right) / 2;
      CellY := (R.Top + R.Bottom) / 2;
      if SameValue(D, FDate) then
      begin
        if Enabled then
          Fill := Palette.Accent
        else
          Fill := Dim;
        P.Ellipse(CellX - Size / 2, CellY - Size / 2, Size, Size, Fill,
          clNone);
        P.Text(Round(CellX), Round(CellY), IntToStr(DayOf(D)), ERD_TEXT_BODY,
          P.ContrastInk(Fill), twSemibold, taCenter);
        if Focused and not IsPreview then
          P.FocusRing(CellX - Size / 2 - P.S(2), CellY - Size / 2 - P.S(2),
            Size + P.S(4), Size + P.S(4), Size / 2 + P.S(2));
        Continue;
      end;
      if (FHot = I) and Enabled then
        P.Ellipse(CellX - Size / 2, CellY - Size / 2, Size, Size,
          ERDMixColor(Palette.NeutralLight, Face, 0.6), clNone);
      if SameValue(D, Today) then
        P.Ellipse(CellX - Size / 2, CellY - Size / 2, Size, Size, clNone,
          P.AccentText, P.SF(1.5));
      if MonthOf(D) <> FViewMonth then
        P.Text(Round(CellX), Round(CellY), IntToStr(DayOf(D)), ERD_TEXT_BODY,
          Dim, twRegular, taCenter)
      else
        P.Text(Round(CellX), Round(CellY), IntToStr(DayOf(D)), ERD_TEXT_BODY,
          Ink, twRegular, taCenter);
    end;
  finally
    P.Free;
  end;
end;

{ TERDCalendarPopup }

constructor TERDCalendarPopup.CreatePopup(APicker: TERDDatePicker);
begin
  inherited Create(APicker);
  FPicker := APicker;
  TabStop := False;
end;

procedure TERDCalendarPopup.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // A no-activate top-level window, like the combo box list: the picker
  // keeps focus and drives the calendar with its keys. No WS_BORDER:
  // Windows paints it in the accent colour; the frame is painted instead.
  Params.Style := (Params.Style and not (WS_CHILD or WS_BORDER or
    WS_DLGFRAME or WS_THICKFRAME)) or WS_POPUP or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := (Params.ExStyle and not (WS_EX_CLIENTEDGE or
    WS_EX_WINDOWEDGE or WS_EX_STATICEDGE)) or WS_EX_TOOLWINDOW or
    WS_EX_TOPMOST or WS_EX_NOACTIVATE;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS or
    CS_DROPSHADOW;
end;

procedure TERDCalendarPopup.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

function TERDCalendarPopup.FaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TERDCalendarPopup.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  inherited PaintControl(ACanvas);
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FrameRect(Rect(0, 0, Width, Height), Palette.NeutralLight, 1);
  finally
    P.Free;
  end;
end;

procedure TERDCalendarPopup.DateClicked;
begin
  inherited DateClicked;
  FPicker.CloseUp(True);
end;

{ TERDDatePicker }

constructor TERDDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks];
  FDate := TodayDate;
  Width := 160;
  Height := 26;
  TabStop := True;
  AutoSize := True;
end;

procedure TERDDatePicker.SetDate(AValue: TDate);
begin
  AValue := Trunc(AValue);
  if SameValue(FDate, AValue) then
    Exit;
  FDate := AValue;
  Invalidate;
  Changed;
end;

procedure TERDDatePicker.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat = AValue then
    Exit;
  FDisplayFormat := AValue;
  Invalidate;
end;

procedure TERDDatePicker.Changed;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then
    FOnChange(Self);
end;

function TERDDatePicker.DisplayText: string;
begin
  if FDisplayFormat <> '' then
    Result := FormatDateTime(FDisplayFormat, FDate)
  else
    Result := DateToStr(FDate);
end;

function TERDDatePicker.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TERDDatePicker.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TERDDatePicker.IsOpen: Boolean;
begin
  Result := (FPopup <> nil) and FPopup.HandleAllocated and
    IsWindowVisible(FPopup.Handle);
end;

procedure TERDDatePicker.DropDown;
var
  Field, Work: TRect;
  Mon: TMonitor;
  W, H, X, Y: Integer;
begin
  if IsOpen or not Enabled or not HandleAllocated then
    Exit;
  if FPopup = nil then
    FPopup := TERDCalendarPopup.CreatePopup(Self);
  if FAppEvents = nil then
  begin
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnMessage := AppMessage;
    FAppEvents.OnDeactivate := AppDeactivate;
  end;
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
  FPopup.FirstDay := FFirstDay;
  FPopup.Date := FDate;
  FPopup.AdjustSize;
  W := FPopup.Width;
  H := FPopup.Height;
  Field.TopLeft := ClientToScreen(Point(0, 0));
  Field.BottomRight := ClientToScreen(Point(Width, Height));
  Mon := Screen.MonitorFromRect(Field, mdNearest);
  if Mon <> nil then
    Work := Mon.WorkareaRect
  else
    Work := Screen.DesktopRect;
  X := Field.Left;
  Y := Field.Bottom + ScaleValue(2);
  if (Y + H > Work.Bottom) and (Field.Top - H - ScaleValue(2) >= Work.Top) then
    Y := Field.Top - H - ScaleValue(2);
  if X + W > Work.Right then
    X := Work.Right - W;
  if X < Work.Left then
    X := Work.Left;
  // Position before parenting: assigning the parent creates the window
  // at its current Left/Top (a flash in the corner of the desktop).
  FPopup.SetBounds(X, Y, W, H);
  if FPopup.Parent <> Self then
    FPopup.Parent := Self;
  FPopup.HandleNeeded;
  SetWindowPos(FPopup.Handle, HWND_TOP, X, Y, W, H, SWP_NOACTIVATE or
    SWP_SHOWWINDOW);
  FPopup.Invalidate;
  Invalidate;
end;

procedure TERDDatePicker.CloseUp(AAccept: Boolean);
begin
  if not IsOpen then
    Exit;
  ShowWindow(FPopup.Handle, SW_HIDE);
  if AAccept then
    SetDate(FPopup.Date);
  Invalidate;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TERDDatePicker.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if not IsOpen then
    Exit;
  case Msg.message of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK:
      begin
        if (Msg.hwnd = FPopup.Handle) or IsChild(FPopup.Handle, Msg.hwnd) then
          Exit;
        // A press on the field is left to MouseDown, which toggles.
        if Msg.hwnd = Handle then
          Exit;
        // Any other press closes the calendar and is swallowed, like the
        // combo box drop-down.
        CloseUp(False);
        Handled := True;
      end;
  end;
end;

procedure TERDDatePicker.AppDeactivate(Sender: TObject);
begin
  CloseUp(False);
end;

procedure TERDDatePicker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
    CloseUp(False);
  Invalidate;
end;

procedure TERDDatePicker.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPopup) then
    CloseUp(False);
end;

procedure TERDDatePicker.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if IsOpen and ((Message.CharCode = VK_RETURN) or
    (Message.CharCode = VK_ESCAPE)) then
    Message.Result := 1
  else
    inherited;
end;

procedure TERDDatePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TERDDatePicker.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;

procedure TERDDatePicker.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TERDDatePicker.DoExit;
begin
  CloseUp(False);
  inherited DoExit;
  Invalidate;
end;

procedure TERDDatePicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    if CanFocus and not Focused then
      SetFocus;
    if IsOpen then
      CloseUp(False)
    else
      DropDown;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TERDDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled := True;
  if IsOpen then
    case Key of
      VK_ESCAPE:
        CloseUp(False);
      VK_RETURN:
        CloseUp(True);
      VK_F4:
        CloseUp(True);
      VK_UP:
        if ssAlt in Shift then
          CloseUp(True)
        else
          Handled := FPopup.HandleKey(Key, Shift);
    else
      if (Key = VK_DOWN) and (ssAlt in Shift) then
        CloseUp(True)
      else
        // Enter / Space in the calendar click the date and close it.
        Handled := FPopup.HandleKey(Key, Shift);
    end
  else
    case Key of
      VK_DOWN:
        if ssAlt in Shift then
          DropDown
        else
          SetDate(FDate - 1);
      VK_UP:
        SetDate(FDate + 1);
      VK_F4:
        DropDown;
      VK_PRIOR:
        SetDate(IncMonth(FDate, 1));
      VK_NEXT:
        SetDate(IncMonth(FDate, -1));
    else
      Handled := False;
    end;
  if Handled then
    Key := 0
  else
    inherited KeyDown(Key, Shift);
end;

procedure TERDDatePicker.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Ink, Glyph: TColor;
  CY: Integer;
  GX, GY, GW, GH: Single;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height),
      (Focused or IsOpen) and not IsPreview, Enabled);
    if Enabled then
    begin
      Ink := Palette.ForegroundText;
      Glyph := Palette.Subtle;
    end
    else
    begin
      Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
      Glyph := Ink;
    end;
    CY := Height div 2;
    P.Text(P.S(10), CY, DisplayText, ERD_TEXT_BODY, Ink, twRegular,
      taLeftJustify, Width - P.S(40));
    // Calendar glyph: a page with a header bar and two rings.
    GW := P.SF(12);
    GH := P.SF(11);
    GX := Width - P.SF(10) - GW;
    GY := CY - GH / 2 + P.SF(0.5);
    P.RoundRect(GX, GY, GW, GH, P.SF(2), clNone, Glyph, P.SF(1.25));
    P.FillRect(Rect(Round(GX), Round(GY + P.SF(3)), Round(GX + GW),
      Round(GY + P.SF(4.25))), Glyph);
    P.FillRect(Rect(Round(GX + P.SF(3)), Round(GY - P.SF(1.5)),
      Round(GX + P.SF(4.25)), Round(GY + P.SF(1.5))), Glyph);
    P.FillRect(Rect(Round(GX + GW - P.SF(4.25)), Round(GY - P.SF(1.5)),
      Round(GX + GW - P.SF(3)), Round(GY + P.SF(1.5))), Glyph);
  finally
    P.Free;
  end;
end;

end.
