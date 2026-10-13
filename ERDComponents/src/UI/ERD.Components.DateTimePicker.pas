unit ERD.Components.DateTimePicker;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.DateUtils,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Calendar,
  ERD.Components.TimePicker, ERD.Components.Paint;
type
  /// <summary>Composes the shared segmented date/calendar and time editors.</summary>
  TERDDateTimePicker = class(TERDCustomControl)
  private
    FDateEditor: TERDDatePicker;
    FTimeEditor: TERDTimePicker;
    FUpdating: Boolean;
    FOnChange, FOnDropDown, FOnCloseUp: TNotifyEvent;
    function GetDateTime: TDateTime;
    procedure SetDateTime(Value: TDateTime);
    function GetDateFormat: string;
    procedure SetDateFormat(const Value: string);
    function GetTimeFormat: TERDTimeFormat;
    procedure SetTimeFormat(Value: TERDTimeFormat);
    function GetShowSeconds: Boolean;
    procedure SetShowSeconds(Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetFirstDay: TERDFirstDayOfWeek;
    procedure SetFirstDay(Value: TERDFirstDayOfWeek);
    procedure ChildChanged(Sender: TObject);
    procedure CalendarOpened(Sender: TObject);
    procedure CalendarClosed(Sender: TObject);
    procedure LayoutEditors;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DropDown;
    procedure CloseUp(Accept: Boolean);
    function DisplayText: string;
    property DateEditor: TERDDatePicker read FDateEditor;
    property TimeEditor: TERDTimePicker read FTimeEditor;
  published
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property DateFormat: string read GetDateFormat write SetDateFormat;
    property TimeFormat: TERDTimeFormat read GetTimeFormat write SetTimeFormat default tf24Hour;
    property ShowSeconds: Boolean read GetShowSeconds write SetShowSeconds default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property FirstDay: TERDFirstDayOfWeek read GetFirstDay write SetFirstDay default fdMonday;
    property AutoSize default True;
    property TabStop default False;
    property Density;
    property ParentDensity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;
implementation
constructor TERDDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  AutoSize := True;
  TabStop := False;
  FDateEditor := TERDDatePicker.Create(Self);
  FDateEditor.SetSubComponent(True);
  FDateEditor.Parent := Self;
  FDateEditor.AutoSize := False;
  FDateEditor.TabOrder := 0;
  FDateEditor.OnChange := ChildChanged;
  FDateEditor.OnDropDown := CalendarOpened;
  FDateEditor.OnCloseUp := CalendarClosed;
  FTimeEditor := TERDTimePicker.Create(Self);
  FTimeEditor.SetSubComponent(True);
  FTimeEditor.Parent := Self;
  FTimeEditor.AutoSize := False;
  FTimeEditor.TabOrder := 1;
  FTimeEditor.OnChange := ChildChanged;
  SetBounds(Left, Top, 360, ScaleValue(Metrics.Edit));
  ThemeChanged;
end;
function TERDDateTimePicker.GetDateTime: TDateTime;
begin
  Result := FDateEditor.Date;
  if Result < 0 then Result := Result - FTimeEditor.Time
  else Result := Result + FTimeEditor.Time;
end;
procedure TERDDateTimePicker.SetDateTime(Value: TDateTime);
var Previous: TDateTime;
begin
  if IsNan(Value) or IsInfinite(Value) or (Abs(Value) > EncodeDate(9999, 12, 31) + 1) then
    raise EArgumentOutOfRangeException.Create('DateTime must be a finite date in years 1 to 9999');
  if (DateOf(Value) < EncodeDate(1, 1, 1)) or (DateOf(Value) > EncodeDate(9999, 12, 31)) then
    raise EArgumentOutOfRangeException.Create('DateTime must be in years 1 to 9999');
  if SameValue(Value, GetDateTime) then Exit;
  Previous := GetDateTime;
  FUpdating := True;
  try
    FDateEditor.Date := DateOf(Value);
    FTimeEditor.Time := TimeOf(Value);
  finally FUpdating := False end;
  if not SameValue(Previous, GetDateTime) then ChildChanged(Self);
end;
procedure TERDDateTimePicker.ChildChanged(Sender: TObject);
begin
  if not FUpdating and not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDDateTimePicker.CalendarOpened(Sender: TObject);
begin
  if Assigned(FOnDropDown) then FOnDropDown(Self);
end;
procedure TERDDateTimePicker.CalendarClosed(Sender: TObject);
begin
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;
function TERDDateTimePicker.GetDateFormat: string;
begin Result := FDateEditor.DisplayFormat end;
procedure TERDDateTimePicker.SetDateFormat(const Value: string);
begin FDateEditor.DisplayFormat := Value end;
function TERDDateTimePicker.GetTimeFormat: TERDTimeFormat;
begin Result := FTimeEditor.TimeFormat end;
procedure TERDDateTimePicker.SetTimeFormat(Value: TERDTimeFormat);
begin FTimeEditor.TimeFormat := Value; LayoutEditors end;
function TERDDateTimePicker.GetShowSeconds: Boolean;
begin Result := FTimeEditor.ShowSeconds end;
procedure TERDDateTimePicker.SetShowSeconds(Value: Boolean);
begin FTimeEditor.ShowSeconds := Value; LayoutEditors end;
function TERDDateTimePicker.GetReadOnly: Boolean;
begin Result := FDateEditor.ReadOnly end;
procedure TERDDateTimePicker.SetReadOnly(Value: Boolean);
begin FDateEditor.ReadOnly := Value; FTimeEditor.ReadOnly := Value end;
function TERDDateTimePicker.GetFirstDay: TERDFirstDayOfWeek;
begin Result := FDateEditor.FirstDay end;
procedure TERDDateTimePicker.SetFirstDay(Value: TERDFirstDayOfWeek);
begin FDateEditor.FirstDay := Value end;
procedure TERDDateTimePicker.LayoutEditors;
var TimeWidth, Gap, DateWidth: Integer;
begin
  if (FDateEditor = nil) or (FTimeEditor = nil) then Exit;
  Gap := ScaleValue(8);
  TimeWidth := 120;
  if FTimeEditor.ShowSeconds then Inc(TimeWidth, 30);
  if FTimeEditor.TimeFormat = tf12Hour then Inc(TimeWidth, 45);
  TimeWidth := Min(ScaleValue(TimeWidth), Max(0, Width - Gap) div 2);
  DateWidth := Max(0, Width - TimeWidth - Gap);
  FDateEditor.SetBounds(0, 0, DateWidth, Height);
  FTimeEditor.SetBounds(DateWidth + Gap, 0, TimeWidth, Height);
end;
procedure TERDDateTimePicker.Resize;
begin inherited; LayoutEditors end;
procedure TERDDateTimePicker.Loaded;
begin inherited; ThemeChanged; LayoutEditors end;
function TERDDateTimePicker.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin Result := True; NewHeight := ScaleValue(Metrics.Edit) end;
procedure TERDDateTimePicker.ThemeChanged;
begin
  inherited;
  if FDateEditor <> nil then FDateEditor.Theme := Theme;
  if FTimeEditor <> nil then FTimeEditor.Theme := Theme;
  DensityChanged;
end;
procedure TERDDateTimePicker.DensityChanged;
begin
  inherited;
  if FDateEditor <> nil then begin FDateEditor.ParentDensity := False; FDateEditor.Density := Density end;
  if FTimeEditor <> nil then begin FTimeEditor.ParentDensity := False; FTimeEditor.Density := Density end;
  if AutoSize then AdjustSize;
  LayoutEditors;
end;
procedure TERDDateTimePicker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FDateEditor <> nil then FDateEditor.Enabled := Enabled;
  if FTimeEditor <> nil then FTimeEditor.Enabled := Enabled;
end;
procedure TERDDateTimePicker.SetFocus;
begin FDateEditor.SetFocus end;
function TERDDateTimePicker.Focused: Boolean;
begin Result := ((FDateEditor <> nil) and FDateEditor.Focused) or ((FTimeEditor <> nil) and FTimeEditor.Focused) end;
procedure TERDDateTimePicker.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Internal editors are rebuilt by the constructor; stream only public properties.
end;
procedure TERDDateTimePicker.DropDown;
begin FDateEditor.SetFocus; FDateEditor.DropDown end;
procedure TERDDateTimePicker.CloseUp(Accept: Boolean);
begin FDateEditor.CloseUp(Accept) end;
function TERDDateTimePicker.DisplayText: string;
begin Result := FDateEditor.DisplayText + ' ' + FTimeEditor.DisplayText end;
procedure TERDDateTimePicker.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Bitmap: TBitmap;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  Bitmap := TBitmap.Create;
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    if (FDateEditor <> nil) and (FTimeEditor <> nil) then
    begin
      FDateEditor.RenderTo(Bitmap);
      ACanvas.Draw(FDateEditor.Left, FDateEditor.Top, Bitmap);
      FTimeEditor.RenderTo(Bitmap);
      ACanvas.Draw(FTimeEditor.Left, FTimeEditor.Top, Bitmap);
    end;
  finally Bitmap.Free; P.Free end;
end;
end.
