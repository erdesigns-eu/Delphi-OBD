unit ERD.Components.CommonControls;
interface
uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils, System.Math,
  System.Generics.Collections, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Menus,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint,
  ERD.Components.Menus,
  ERD.Components.Edits, ERD.Components.Buttons, ERD.Components.Panel;
const
  CM_ERD_REBUILDTAGS = WM_APP + $4A2;
type
  TERDSearchEdit = class(TERDEdit)
  private
    FTimer: TTimer;
    FSearchDelay: Integer;
    FOnSearch: TNotifyEvent;
    procedure SetSearchDelay(Value: Integer);
    procedure SearchElapsed(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function TrailingSpace: Integer; override;
    procedure Change; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteSearch;
  published
    property WantReturnKey default True;
    property SearchDelay: Integer read FSearchDelay write SetSearchDelay default 300;
    property OnSearch: TNotifyEvent read FOnSearch write FOnSearch;
  end;

  TERDSplitButton = class(TERDButton)
  private
    FDropDownMenu: TPopupMenu;
    FDropClick: Boolean;
    FOnDropDown: TNotifyEvent;
    procedure SetDropDownMenu(Value: TPopupMenu);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DropDown;
  published
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

  TERDTagToken = class(TERDInteractiveControl)
  private
    FCanClose: Boolean;
    FCloseHot, FHandlingMouseUp: Boolean;
    FCloseIconSize, FCloseSpacing, FTagColorStrength: Integer;
    FTagColor: TColor;
    FClosePressed: Boolean;
    FOnClose: TNotifyEvent;
    function CloseBounds: TRect;
    procedure SetCloseIconSize(Value: Integer);
    procedure SetCloseSpacing(Value: Integer);
    procedure SetTagColor(Value: TColor);
    procedure SetTagColorStrength(Value: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property CloseIconSize: Integer read FCloseIconSize write SetCloseIconSize;
    property CloseSpacing: Integer read FCloseSpacing write SetCloseSpacing;
    property TagColor: TColor read FTagColor write SetTagColor;
    property TagColorStrength: Integer read FTagColorStrength write SetTagColorStrength;
    property CanClose: Boolean read FCanClose write FCanClose;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;
  TERDExpanderHeader = class(TERDButton)
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  end;

  TERDTagEdit = class(TERDPanel)
  private
    FCloseIconSize, FCloseSpacing, FTagColorStrength: Integer;
    FTagColor: TColor;
    FTags: TStringList;
    FEditor: TERDEdit;
    FButtons: TObjectList<TERDTagToken>;
    FReadOnly: Boolean;
    FAllowDuplicates: Boolean;
    FLayouting: Boolean;
    FRemovingClick: Boolean;
    FOnChange: TNotifyEvent;
    procedure UpdateTagAppearance;
    procedure SetCloseIconSize(Value: Integer);
    procedure SetCloseSpacing(Value: Integer);
    procedure SetTagColor(Value: TColor);
    procedure SetTagColorStrength(Value: Integer);
    procedure SetTags(Value: TStrings);
    function GetTags: TStrings;
    procedure SetReadOnly(Value: Boolean);
    procedure TagsChanged(Sender: TObject);
    procedure RemoveTagClick(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LayoutTags;
    procedure RebuildTags(var Message: TMessage); message CM_ERD_REBUILDTAGS;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTag(const Value: string): Boolean;
    procedure RemoveTag(Index: Integer);
    procedure CommitText;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    procedure SetFocus; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Editor: TERDEdit read FEditor;
  published
    /// <summary>Close cross size in 96-DPI pixels.</summary>
    property CloseIconSize: Integer read FCloseIconSize write SetCloseIconSize default 8;
    /// <summary>Space between text and close icon in 96-DPI pixels.</summary>
    property CloseSpacing: Integer read FCloseSpacing write SetCloseSpacing default 4;
    /// <summary>clDefault uses the current theme accent.</summary>
    property TagColor: TColor read FTagColor write SetTagColor default clDefault;
    /// <summary>Tag fill accent strength, from 0 to 100 percent.</summary>
    property TagColorStrength: Integer read FTagColorStrength write SetTagColorStrength default 28;
    property Tags: TStrings read GetTags write SetTags;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property AllowDuplicates: Boolean read FAllowDuplicates write FAllowDuplicates default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TERDExpander = class(TERDPanel)
  private
    FHeader: TERDButton;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    FChanging: Boolean;
    FHidden: TList<TControl>;
    FOnExpandedChange: TNotifyEvent;
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(Value: Integer);
    procedure HeaderClick(Sender: TObject);
    procedure UpdateHeader;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DensityChanged; override;
    procedure ThemeChanged; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight default 160;
    property OnExpandedChange: TNotifyEvent read FOnExpandedChange write FOnExpandedChange;
  end;
implementation
procedure TERDTagToken.SetCloseIconSize(Value: Integer);
begin
  Value := EnsureRange(Value, 4, 24);
  if FCloseIconSize = Value then Exit;
  FCloseIconSize := Value;
  Invalidate;
end;
procedure TERDTagToken.SetCloseSpacing(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 64);
  if FCloseSpacing = Value then Exit;
  FCloseSpacing := Value;
  Invalidate;
end;
procedure TERDTagToken.SetTagColor(Value: TColor);
begin
  if FTagColor = Value then Exit;
  FTagColor := Value;
  Invalidate;
end;
procedure TERDTagToken.SetTagColorStrength(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 100);
  if FTagColorStrength = Value then Exit;
  FTagColorStrength := Value;
  Invalidate;
end;
procedure TERDTagEdit.SetCloseIconSize(Value: Integer);
begin
  Value := EnsureRange(Value, 4, 24);
  if FCloseIconSize = Value then Exit;
  FCloseIconSize := Value;
  UpdateTagAppearance;
end;
procedure TERDTagEdit.SetCloseSpacing(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 64);
  if FCloseSpacing = Value then Exit;
  FCloseSpacing := Value;
  UpdateTagAppearance;
end;
procedure TERDTagEdit.SetTagColor(Value: TColor);
begin
  if FTagColor = Value then Exit;
  FTagColor := Value;
  UpdateTagAppearance;
end;
procedure TERDTagEdit.SetTagColorStrength(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 100);
  if FTagColorStrength = Value then Exit;
  FTagColorStrength := Value;
  UpdateTagAppearance;
end;
procedure TERDTagEdit.UpdateTagAppearance;
var Button: TERDTagToken;
begin
  if FButtons = nil then Exit;
  for Button in FButtons do
  begin
    Button.CloseIconSize := FCloseIconSize;
    Button.CloseSpacing := FCloseSpacing;
    Button.TagColor := FTagColor;
    Button.TagColorStrength := FTagColorStrength;
  end;
  LayoutTags;
end;
constructor TERDTagToken.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  FCanClose := True;
  FCloseIconSize := 8;
  FCloseSpacing := 4;
  FTagColor := clDefault;
  FTagColorStrength := 28;
  Height := 26;
end;
function TERDTagToken.CloseBounds: TRect;
var HitHeight: Integer;
begin
  HitHeight := Min(Height - ScaleValue(4), ScaleValue(Max(16, FCloseIconSize + 6)));
  Result := Rect(Max(ScaleValue(8), Width - ScaleValue(8 + FCloseIconSize + Min(FCloseSpacing, 3))),
    (Height - HitHeight) div 2, Width - ScaleValue(4), (Height + HitHeight) div 2);
end;
procedure TERDTagToken.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Fill, Base, Border, Ink: TColor; AvailableTextWidth: Integer;
  R: TRect; CX, CY, Half: Single; Strength: Single;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Base := FTagColor;
    if Base = clDefault then Base := Palette.Accent;
    Strength := FTagColorStrength / 100;
    if Enabled and Hover then Strength := Min(1, Strength + 0.08);
    if not Enabled then Strength := Strength * 0.5;
    Fill := P.Tint(Base, Strength);
    Border := ERDMixColor(Base, Fill, 0.55);
    Ink := P.ContrastInk(Fill);
    if not Enabled then begin Ink := P.DisabledText; Border := Palette.NeutralLight end;
    P.RoundRect(P.S(2), P.S(2), Max(0, Width - P.S(4)), Max(0, Height - P.S(4)),
      (Height - P.S(4)) / 2, Fill, Border);
    AvailableTextWidth := Width - P.S(16);
    if FCanClose then Dec(AvailableTextWidth, P.S(FCloseIconSize + FCloseSpacing));
    if AvailableTextWidth > 0 then P.Text(P.S(8), Height div 2, Caption, ERD_TEXT_BODY, Ink,
      twRegular, taLeftJustify, Max(0, AvailableTextWidth));
    if FCanClose then
    begin
      R := CloseBounds;
      CX := Width - P.SF(8) - P.SF(FCloseIconSize) / 2;
      CY := Height / 2;
      if Enabled and FCloseHot then
      begin
        if FClosePressed then Fill := ERDMixColor(Base, Fill, 0.6)
        else Fill := ERDMixColor(Base, Fill, 0.35);
        P.RoundRect(R.Left, R.Top, R.Width, R.Height, P.SF(4), Fill, clNone);
        Ink := P.ContrastInk(Fill);
      end;
      Half := P.SF(FCloseIconSize) / 2;
      P.Lines([ERDPointF(CX - Half, CY - Half), ERDPointF(CX + Half, CY + Half)], Ink, P.SF(1));
      P.Lines([ERDPointF(CX + Half, CY - Half), ERDPointF(CX - Half, CY + Half)], Ink, P.SF(1));
    end;
    if HasFocus then P.FocusRing(P.S(3), P.S(3), Max(0, Width - P.S(6)),
      Max(0, Height - P.S(6)), (Height - P.S(6)) / 2);
  finally P.Free end;
end;
procedure TERDTagToken.MouseMove(Shift: TShiftState; X, Y: Integer);
var Hot: Boolean;
begin
  inherited;
  Hot := Enabled and FCanClose and PtInRect(CloseBounds, Point(X, Y));
  if Hot <> FCloseHot then begin FCloseHot := Hot; Invalidate end;
end;
procedure TERDTagToken.CMMouseLeave(var Message: TMessage);
begin inherited; FCloseHot := False; Invalidate end;
procedure TERDTagToken.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if not FHandlingMouseUp then FClosePressed := False;
  Invalidate;
end;
procedure TERDTagToken.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONUP then
  begin
    FHandlingMouseUp := True;
    try inherited finally FHandlingMouseUp := False end;
  end
  else inherited;
end;
procedure TERDTagToken.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCloseHot := Enabled and FCanClose and PtInRect(CloseBounds, Point(X, Y));
  FClosePressed := FCloseHot and (Button = mbLeft);
  inherited;
  if FClosePressed then MouseCapture := True;
  Invalidate;
end;
procedure TERDTagToken.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Remove: Boolean;
begin
  Remove := FClosePressed and Enabled and FCanClose and (Button = mbLeft) and
    PtInRect(CloseBounds, Point(X, Y));
  FClosePressed := False;
  inherited;
  Invalidate;
  if Remove and Assigned(FOnClose) then FOnClose(Self);
end;
procedure TERDTagToken.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Enabled and FCanClose and (Key in [VK_DELETE, VK_BACK]) then
  begin
    Key := 0;
    if Assigned(FOnClose) then FOnClose(Self);
  end;
end;
procedure TERDExpanderHeader.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Fill: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Fill := P.HeaderFill;
    if Hover then Fill := P.Tint(Palette.Accent, 0.08);
    if Pressed then Fill := P.Tint(Palette.Accent, 0.16);
    P.FillRect(ClientRect, Fill);
    P.HLine(0, Height - 1, Width, Palette.NeutralLight);
    P.Glyph(Glyph, P.S(14), Height div 2, Palette.Subtle, 0.8);
    P.Text(P.S(30), Height div 2, Caption, ERD_TEXT_BODY, Palette.ForegroundText,
      twSemibold, taLeftJustify, Max(0, Width - P.S(38)));
    if HasFocus then P.FrameRect(Rect(P.S(3), P.S(3), Width - P.S(3), Height - P.S(3)), Palette.Accent);
  finally P.Free end;
end;
constructor TERDSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  FSearchDelay := 300;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := FSearchDelay;
  FTimer.OnTimer := SearchElapsed;
  TextHint := 'Search';
  WantReturnKey := True;
end;
function TERDSearchEdit.TrailingSpace: Integer;
begin
  Result := inherited TrailingSpace + ScaleValue(56);
end;
procedure TERDSearchEdit.SetSearchDelay(Value: Integer);
begin
  if Value < 0 then raise EArgumentOutOfRangeException.Create('SearchDelay must be nonnegative');
  FSearchDelay := Value;
  if FTimer <> nil then
  begin
    FTimer.Interval := Max(1, Value);
    if Value = 0 then FTimer.Enabled := False;
  end;
end;
procedure TERDSearchEdit.Change;
begin
  inherited;
  if (FTimer = nil) or (csLoading in ComponentState) or (csDestroying in ComponentState) or (csDesigning in ComponentState) then Exit;
  FTimer.Enabled := False;
  if FSearchDelay = 0 then ExecuteSearch
  else FTimer.Enabled := True;
  Invalidate;
end;
procedure TERDSearchEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not Enabled then Exit;
  if Key = VK_RETURN then
  begin
    ExecuteSearch;
    Key := 0;
  end
  else if (Key = VK_ESCAPE) and not ReadOnly then
  begin
    Text := '';
    Key := 0;
  end;
end;
procedure TERDSearchEdit.SearchElapsed(Sender: TObject);
begin
  ExecuteSearch;
end;
procedure TERDSearchEdit.ExecuteSearch;
begin
  if FTimer <> nil then FTimer.Enabled := False;
  if Assigned(FOnSearch) and not (csDestroying in ComponentState) then FOnSearch(Self);
end;
procedure TERDSearchEdit.PaintControl(ACanvas: TCanvas);
var P: TERDPainter;
begin
  inherited;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Glyph(glSearch, Width - ScaleValue(16), EditFieldHeight div 2, Palette.Subtle);
    if Text <> '' then P.Glyph(glClose, Width - ScaleValue(42), EditFieldHeight div 2, Palette.Subtle);
  finally P.Free end;
end;
procedure TERDSearchEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button <> mbLeft) or not Enabled or (Y >= EditFieldHeight) then Exit;
  if X >= Width - ScaleValue(28) then ExecuteSearch
  else if (X >= Width - ScaleValue(56)) and not ReadOnly then
  begin
    Text := '';
    SetFocus;
  end;
end;

constructor TERDSplitButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csClickEvents];
end;
procedure TERDSplitButton.SetDropDownMenu(Value: TPopupMenu);
begin
  if FDropDownMenu = Value then Exit;
  if FDropDownMenu <> nil then FDropDownMenu.RemoveFreeNotification(Self);
  FDropDownMenu := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;
procedure TERDSplitButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then FDropDownMenu := nil;
end;
function TERDSplitButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanAutoSize(NewWidth, NewHeight);
  Inc(NewWidth, ScaleValue(28));
end;
procedure TERDSplitButton.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; M, SplitX: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    M := RingMargin;
    SplitX := Max(M, Width - ScaleValue(28));
    P.Button(Rect(M, M, SplitX, Height - M), Caption, Kind, InteractionState,
      HasFocus, Glyph, Icon);
    P.Button(Rect(SplitX, M, Width - M, Height - M), '', Kind, InteractionState,
      False, glChevronDown);
    P.VLine(SplitX, M + P.S(5), Max(0, Height - 2 * M - P.S(10)),
      P.ContrastInk(Palette.Accent));
  finally P.Free end;
end;
procedure TERDSplitButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDropClick := (Button = mbLeft) and (X >= Width - ScaleValue(28));
  inherited;
end;
procedure TERDSplitButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Activate, OpenMenu: Boolean;
begin
  Activate := Pressed and Enabled and (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y));
  OpenMenu := X >= Width - ScaleValue(28);
  Activate := Activate and (OpenMenu = FDropClick);
  FDropClick := False;
  inherited;
  if Activate then
  begin
    if OpenMenu then DropDown else Click;
  end;
end;
procedure TERDSplitButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FDropClick := False;
  if (Key = VK_F4) or ((Key = VK_DOWN) and (ssAlt in Shift)) then
  begin
    DropDown;
    Key := 0;
  end;
  inherited;
end;
procedure TERDSplitButton.DropDown;
var Pt: TPoint; R: TRect; M: Integer;
begin
  if not Enabled then Exit;
  if Assigned(FOnDropDown) then FOnDropDown(Self);
  if FDropDownMenu <> nil then
  begin
    M := RingMargin;
    R := Rect(M, M, Width - M, Height - M);
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);
    Pt := Point(R.Left, R.Bottom);
    FDropDownMenu.PopupComponent := Self;
    if FDropDownMenu is TERDPopupMenu then TERDPopupMenu(FDropDownMenu).PopupAt(R)
    else FDropDownMenu.Popup(Pt.X, Pt.Y);
  end;
end;

constructor TERDTagEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCloseIconSize := 8;
  FCloseSpacing := 4;
  FTagColor := clDefault;
  FTagColorStrength := 28;
  FTags := TStringList.Create;
  FTags.OnChange := TagsChanged;
  FButtons := TObjectList<TERDTagToken>.Create(True);
  FEditor := TERDEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.AutoSize := False;
  FEditor.TextHint := 'Add tag';
  FEditor.WantReturnKey := True;
  FEditor.OnKeyDown := EditorKeyDown;
  FEditor.Theme := Theme;
  Height := 100;
  LayoutTags;
end;
destructor TERDTagEdit.Destroy;
begin
  FTags.OnChange := nil;
  FreeAndNil(FButtons);
  FreeAndNil(FTags);
  inherited;
end;
function TERDTagEdit.GetTags: TStrings;
begin
  Result := FTags;
end;
procedure TERDTagEdit.SetTags(Value: TStrings);
begin
  FTags.Assign(Value);
end;
procedure TERDTagEdit.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  if FEditor <> nil then FEditor.ReadOnly := Value;
  TagsChanged(Self);
end;
function TERDTagEdit.AddTag(const Value: string): Boolean;
var S: string; I: Integer;
begin
  Result := False;
  if FReadOnly then Exit;
  S := Trim(Value);
  if S = '' then Exit;
  if not FAllowDuplicates then
    for I := 0 to FTags.Count - 1 do
      if SameText(FTags[I], S) then Exit;
  FTags.Add(S);
  Result := True;
end;
procedure TERDTagEdit.RemoveTag(Index: Integer);
begin
  if not FReadOnly and (Index >= 0) and (Index < FTags.Count) then FTags.Delete(Index);
end;
procedure TERDTagEdit.CommitText;
begin
  if AddTag(FEditor.Text) then FEditor.Text := '';
end;
procedure TERDTagEdit.RemoveTagClick(Sender: TObject);
begin
  FRemovingClick := True;
  try
    RemoveTag(TControl(Sender).Tag);
    if FEditor <> nil then FEditor.SetFocus;
  finally FRemovingClick := False end;
end;
procedure TERDTagEdit.RebuildTags(var Message: TMessage);
begin
  TagsChanged(nil);
end;
procedure TERDTagEdit.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    CommitText;
    Key := 0;
  end
  else if (Key = VK_BACK) and (FEditor.Text = '') and (Shift = []) then
  begin
    RemoveTag(FTags.Count - 1);
    Key := 0;
  end;
end;
procedure TERDTagEdit.TagsChanged(Sender: TObject);
var I: Integer; Button: TERDTagToken;
begin
  if FButtons = nil then Exit;
  if FRemovingClick then
  begin
    PostMessage(Handle, CM_ERD_REBUILDTAGS, 0, 0);
    if Assigned(FOnChange) and not (csLoading in ComponentState) then FOnChange(Self);
    Exit;
  end;
  FButtons.Clear;
  for I := 0 to FTags.Count - 1 do
  begin
    Button := TERDTagToken.Create(Self);
    Button.Parent := Self;
    Button.Theme := Theme;
    Button.Tag := I;
    Button.Caption := FTags[I];
    Button.CanClose := not FReadOnly;
    Button.CloseIconSize := FCloseIconSize;
    Button.CloseSpacing := FCloseSpacing;
    Button.TagColor := FTagColor;
    Button.TagColorStrength := FTagColorStrength;
    Button.AutoSize := False;
    Button.TabStop := not FReadOnly;
    Button.OnClose := RemoveTagClick;
    FButtons.Add(Button);
  end;
  LayoutTags;
  if (Sender <> nil) and Assigned(FOnChange) and not (csLoading in ComponentState) then FOnChange(Self);
end;
procedure TERDTagEdit.LayoutTags;
var X, Y, H, W, Gap: Integer; Button: TERDTagToken;
begin
  if (FButtons = nil) or (FEditor = nil) or FLayouting then Exit;
  FLayouting := True;
  try
  Gap := ScaleValue(6);
  X := Gap;
  Y := Gap;
  H := ScaleValue(Max(Metrics.Edit, FCloseIconSize + 8));
  for Button in FButtons do
  begin
    W := ERDMeasureText(Button.Caption, ERD_TEXT_BODY, twRegular, ScaleValue(96)) + ScaleValue(16);
    if Button.CanClose then Inc(W, ScaleValue(FCloseIconSize + FCloseSpacing));
    W := Min(W, Max(H, ClientWidth - 2 * Gap));
    if (X > Gap) and (X + W > ClientWidth - Gap) then
    begin
      X := Gap;
      Inc(Y, H + Gap);
    end;
    Button.SetBounds(X, Y, W, H);
    Inc(X, W + Gap);
  end;
  if ClientWidth - X < ScaleValue(100) then
  begin
    X := Gap;
    Inc(Y, H + Gap);
  end;
  FEditor.SetBounds(X, Y, Max(0, ClientWidth - X - Gap), H);
  if Height < Y + H + Gap then Height := Y + H + Gap;
  finally FLayouting := False end;
end;
procedure TERDTagEdit.Resize;
begin
  inherited;
  LayoutTags;
end;
procedure TERDTagEdit.Loaded;
begin
  inherited;
  LayoutTags;
end;
procedure TERDTagEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FEditor) then FEditor := nil;
end;
procedure TERDTagEdit.ThemeChanged;
var Button: TERDTagToken;
begin
  inherited;
  if FEditor <> nil then FEditor.Theme := Theme;
  if FButtons <> nil then
    for Button in FButtons do Button.Theme := Theme;
end;
procedure TERDTagEdit.DensityChanged;
begin
  inherited;
  LayoutTags;
end;
procedure TERDTagEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
var I: Integer;
begin
  // Tags and their editor are recreated from properties, not streamed twice.
  for I := 0 to ControlCount - 1 do
    if (Controls[I] <> FEditor) and not (Controls[I] is TERDTagToken) and
      (Controls[I].Owner = Root) then Proc(Controls[I]);
end;
procedure TERDTagEdit.SetFocus;
begin
  if FEditor <> nil then FEditor.SetFocus else inherited;
end;

constructor TERDExpander.Create(AOwner: TComponent);
begin
  inherited;
  FExpanded := True;
  FExpandedHeight := 160;
  FHidden := TList<TControl>.Create;
  FHeader := TERDExpanderHeader.Create(Self);
  FHeader.Parent := Self;
  FHeader.Align := alTop;
  FHeader.AutoSize := False;
  FHeader.Kind := bkSecondary;
  FHeader.Theme := Theme;
  FHeader.OnClick := HeaderClick;
  Height := ScaleValue(FExpandedHeight);
  UpdateHeader;
end;
destructor TERDExpander.Destroy;
begin
  FreeAndNil(FHidden);
  inherited;
end;
procedure TERDExpander.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateHeader;
end;
procedure TERDExpander.UpdateHeader;
begin
  if FHeader = nil then Exit;
  FHeader.Caption := Caption;
  FHeader.Height := ScaleValue(Metrics.Edit);
  if FExpanded then FHeader.Glyph := glChevronDown else FHeader.Glyph := glChevronRight;
end;
procedure TERDExpander.HeaderClick(Sender: TObject);
begin
  SetExpanded(not FExpanded);
end;
procedure TERDExpander.SetExpandedHeight(Value: Integer);
begin
  if Value < Metrics.Edit then raise EArgumentOutOfRangeException.Create('ExpandedHeight must fit the header');
  FExpandedHeight := Value;
  if FExpanded then Height := ScaleValue(Value);
end;
procedure TERDExpander.SetExpanded(Value: Boolean);
var I: Integer; Child: TControl;
begin
  if FExpanded = Value then Exit;
  FExpanded := Value;
  if (FHeader = nil) or (FHidden = nil) or (csLoading in ComponentState) then Exit;
  FChanging := True;
  try
    if not Value then
    begin
      FExpandedHeight := MulDiv(Height, 96, ScaleValue(96));
      FHidden.Clear;
      for I := 0 to ControlCount - 1 do
      begin
        Child := Controls[I];
        if (Child <> FHeader) and Child.Visible then
        begin
          FHidden.Add(Child);
          Child.FreeNotification(Self);
          Child.Visible := False;
        end;
      end;
      Height := ScaleValue(Metrics.Edit + 2 * BorderWidth);
    end
    else
    begin
      Height := ScaleValue(FExpandedHeight);
      for Child in FHidden do
      begin
        Child.RemoveFreeNotification(Self);
        Child.Visible := True;
      end;
      FHidden.Clear;
    end;
    UpdateHeader;
  finally FChanging := False end;
  if Assigned(FOnExpandedChange) and not (csLoading in ComponentState) then FOnExpandedChange(Self);
end;
procedure TERDExpander.Resize;
begin
  inherited;
  if FExpanded and not FChanging and (FHeader <> nil) then
    FExpandedHeight := MulDiv(Height, 96, ScaleValue(96));
  UpdateHeader;
end;
procedure TERDExpander.GetChildren(Proc: TGetChildProc; Root: TComponent);
var Child: TControl;
begin
  // Collapse is presentation state, not the child's persisted Visible value.
  if (FHidden = nil) or FExpanded then
  begin
    inherited;
    Exit;
  end;
  DisableAlign;
  try
    for Child in FHidden do Child.Visible := True;
    try
      inherited;
    finally
      for Child in FHidden do Child.Visible := False;
    end;
  finally EnableAlign end;
end;
procedure TERDExpander.Loaded;
var WasExpanded: Boolean;
begin
  inherited;
  WasExpanded := FExpanded;
  FExpanded := True;
  if not WasExpanded then
  begin
    FChanging := True;
    try Height := ScaleValue(FExpandedHeight) finally FChanging := False end;
    SetExpanded(False);
  end;
  UpdateHeader;
end;
procedure TERDExpander.PaintControl(ACanvas: TCanvas);
begin
  UpdateHeader;
  inherited;
end;
procedure TERDExpander.DensityChanged;
begin
  inherited;
  UpdateHeader;
end;
procedure TERDExpander.ThemeChanged;
begin
  inherited;
  if FHeader <> nil then FHeader.Theme := Theme;
end;
procedure TERDExpander.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (FHidden <> nil) and (AComponent is TControl) then FHidden.Remove(TControl(AComponent));
    if AComponent = FHeader then FHeader := nil;
  end;
end;
end.
