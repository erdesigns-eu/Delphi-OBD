unit ERD.Components.VirtualTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
  System.Math, System.UITypes, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Forms,
  ERD.Components.Types, ERD.Components.Theme, ERD.Components.Paint,
  ERD.Components.VT, ERD.Components.VT.Types, ERD.Components.VT.Header;

type
  /// <summary>Virtual Treeview V8.5 with ERD palettes and drawing; node/event API is retained.</summary>
  TERDVirtualStringTree = class(TVirtualStringTree, IERDThemeAware)
  private
    FTheme: TERDTheme;
    FApplyingTheme: Boolean;
    FRowHeight: Integer;
    FHeaderHeight: Integer;
    FAlternateRows: Boolean;
    procedure SetTheme(Value: TERDTheme);
    procedure SetRowHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetAlternateRows(Value: Boolean);
    function GetThemePalette: TERDThemePalette;
    procedure ApplyTheme;
    procedure PaintThemedScrollBars;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    function GetVclStyleEnabled: Boolean; override;
    function UseAdvancedHeaderDraw: Boolean; override;
    procedure DoHeaderDrawQueryElements(var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements); override;
    procedure DoAdvancedHeaderDraw(var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements); override;
    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var Color: TColor; var EraseAction: TItemEraseAction); override;
    procedure PaintNodeButton(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const R: TRect; ButtonX, ButtonY: TDimension; BidiMode: TBiDiMode); override;
    procedure PaintCheckImage(Canvas: TCanvas; const ImageInfo: TVTImageInfo;
      Selected: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged;
    property Palette: TERDThemePalette read GetThemePalette;
  published
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Default row height in 96-DPI logical pixels; individual node heights remain available.</summary>
    property RowHeight: Integer read FRowHeight write SetRowHeight default 26;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 30;
    property AlternateRows: Boolean read FAlternateRows write SetAlternateRows default True;
  end;

implementation

constructor TERDVirtualStringTree.Create(AOwner: TComponent);
begin
  inherited;
  FRowHeight := 26;
  FHeaderHeight := 30;
  FAlternateRows := True;
  ParentFont := False;
  Ctl3D := False;
  StyleElements := [];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toThemeAware, toUseExplorerTheme];
  Header.Options := Header.Options + [hoVisible, hoColumnResize, hoShowSortGlyphs];
  SetTheme(TERDTheme.FindOnOwner(Self));
  ApplyTheme;
end;

destructor TERDVirtualStringTree.Destroy;
begin
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
    FTheme := nil;
  end;
  inherited;
end;

procedure TERDVirtualStringTree.SetTheme(Value: TERDTheme);
begin
  if FTheme = Value then Exit;
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
  end;
  FTheme := Value;
  if FTheme <> nil then
  begin
    FTheme.FreeNotification(Self);
    FTheme.Attach(Self);
  end;
  ThemeChanged;
end;

function TERDVirtualStringTree.GetThemePalette: TERDThemePalette;
var Source: TERDTheme;
begin
  Source := FTheme;
  if Source = nil then Source := TERDTheme.FindOnOwner(Self);
  if (Source <> nil) and (csDestroying in Source.ComponentState) then Source := nil;
  if Source = nil then Source := TERDTheme.GetDefault;
  if Source <> nil then Result := Source.Palette else Result := BRAND_PALETTE_LIGHT;
end;

procedure TERDVirtualStringTree.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
  begin
    FTheme := nil;
    if not (csDestroying in ComponentState) then ThemeChanged;
  end;
end;

procedure TERDVirtualStringTree.Loaded;
begin
  inherited;
  if FTheme = nil then SetTheme(TERDTheme.FindOnOwner(Self));
  ApplyTheme;
end;

procedure TERDVirtualStringTree.CreateWnd;
begin
  inherited;
  ApplyTheme;
end;

procedure TERDVirtualStringTree.ApplyTheme;
var P: TERDThemePalette; PPI: Integer;
begin
  if FApplyingTheme or (FRowHeight = 0) or (csDestroying in ComponentState) then Exit;
  FApplyingTheme := True;
  try
    P := Palette;
    PPI := CurrentPPI;
    Color := P.GaugeFace;
    Font.Name := 'Segoe UI';
    Font.Height := -MulDiv(13, PPI, 96);
    Font.Color := P.ForegroundText;
    Header.Font.Assign(Font);
    Header.Height := MulDiv(FHeaderHeight, PPI, 96);
    DefaultNodeHeight := MulDiv(FRowHeight, PPI, 96);
    Colors.BorderColor := P.NeutralLight;
    Colors.DisabledColor := P.Subtle;
    Colors.TreeLineColor := P.NeutralLight;
    Colors.GridLineColor := P.NeutralLight;
    Colors.HotColor := P.ForegroundText;
    Colors.HeaderHotColor := P.Accent;
    Colors.FocusedSelectionColor := ERDMixColor(P.Accent, P.GaugeFace, 0.16);
    Colors.FocusedSelectionBorderColor := P.Accent;
    Colors.UnfocusedSelectionColor := ERDMixColor(P.ForegroundText, P.GaugeFace, 0.06);
    Colors.UnfocusedSelectionBorderColor := P.NeutralLight;
    Colors.SelectionTextColor := P.ForegroundText;
    Colors.UnfocusedColor := P.ForegroundText;
    Colors.DropMarkColor := P.Accent;
    Colors.DropTargetColor := ERDMixColor(P.Accent, P.GaugeFace, 0.2);
    Colors.DropTargetBorderColor := P.Accent;
    Colors.SelectionRectangleBlendColor := P.Accent;
    Colors.SelectionRectangleBorderColor := P.Accent;
  finally FApplyingTheme := False end;
end;

procedure TERDVirtualStringTree.ThemeChanged;
begin
  ApplyTheme;
  Invalidate;
  if HandleAllocated then PaintThemedScrollBars;
end;

procedure TERDVirtualStringTree.SetRowHeight(Value: Integer);
begin
  Value := EnsureRange(Value, 16, 200);
  if FRowHeight = Value then Exit;
  FRowHeight := Value;
  ThemeChanged;
end;

procedure TERDVirtualStringTree.SetHeaderHeight(Value: Integer);
begin
  Value := EnsureRange(Value, 16, 200);
  if FHeaderHeight = Value then Exit;
  FHeaderHeight := Value;
  ThemeChanged;
end;

procedure TERDVirtualStringTree.SetAlternateRows(Value: Boolean);
begin
  if FAlternateRows = Value then Exit;
  FAlternateRows := Value;
  Invalidate;
end;

function TERDVirtualStringTree.GetVclStyleEnabled: Boolean;
begin
  // Explicit ERD profiles take precedence over global VCL styles.
  Result := False;
end;

function TERDVirtualStringTree.UseAdvancedHeaderDraw: Boolean;
begin
  Result := True;
end;

procedure TERDVirtualStringTree.DoHeaderDrawQueryElements(var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  inherited;
  Include(Elements, hpeBackground);
end;

procedure TERDVirtualStringTree.DoAdvancedHeaderDraw(var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);
var P: TERDPainter; R: TRect; Fill: TColor;
begin
  if hpeBackground in Elements then
  begin
    R := PaintInfo.PaintRectangle;
    Fill := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.03);
    if PaintInfo.IsDownIndex then Fill := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.16)
    else if PaintInfo.IsHoverIndex then Fill := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.08);
    P := TERDPainter.Create(PaintInfo.TargetCanvas, Palette, CurrentPPI);
    try
      P.FillRect(R, Fill);
      P.HLine(R.Left, R.Bottom - 1, R.Width, Palette.NeutralLight);
      if PaintInfo.ShowRightBorder then
        P.VLine(R.Right - 1, R.Top + P.S(5), Max(0, R.Height - P.S(10)), Palette.NeutralLight);
    finally P.Free end;
  end;
  // Preserve application callbacks and the engine's text/sort/image/checkbox layout.
  inherited;
end;

procedure TERDVirtualStringTree.DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var Color: TColor; var EraseAction: TItemEraseAction);
begin
  if FAlternateRows and (Node <> nil) and not (vsSelected in Node.States) and Odd(Node.Index) then
  begin
    Color := ERDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.025);
    EraseAction := eaColor;
  end;
  inherited;
end;

procedure TERDVirtualStringTree.PaintNodeButton(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const R: TRect; ButtonX, ButtonY: TDimension; BidiMode: TBiDiMode);
var P: TERDPainter; X, Y, Size: Integer; Glyph: TERDGlyph;
begin
  Size := MulDiv(9, CurrentPPI, 96);
  if BidiMode = bdLeftToRight then X := R.Left + ButtonX else X := R.Right - ButtonX - Size;
  Y := R.Top + ButtonY;
  if vsExpanded in Node.States then Glyph := glChevronDown
  else if BidiMode = bdLeftToRight then Glyph := glChevronRight else Glyph := glChevronLeft;
  P := TERDPainter.Create(Canvas, Palette, CurrentPPI);
  try P.Glyph(Glyph, X + Size div 2, Y + Size div 2, Palette.Subtle, 0.65)
  finally P.Free end;
end;

procedure TERDVirtualStringTree.PaintCheckImage(Canvas: TCanvas; const ImageInfo: TVTImageInfo;
  Selected: Boolean);
var P: TERDPainter; Size, CY, StateIndex: Integer; State: TCheckBoxState;
  ItemEnabled, Hot: Boolean;
begin
  if (CustomCheckImages <> nil) or (ImageInfo.Index < 1) or (ImageInfo.Index > 20) then
  begin
    inherited;
    Exit;
  end;
  Size := 13;
  if CheckImages <> nil then Size := Max(1, MulDiv(CheckImages.Width, 96, CurrentPPI));
  StateIndex := (ImageInfo.Index - 1) mod 4;
  ItemEnabled := Enabled and not ImageInfo.Ghosted and (StateIndex <> 3);
  Hot := StateIndex in [1, 2];
  P := TERDPainter.Create(Canvas, Palette, CurrentPPI);
  try
    CY := ImageInfo.YPos + P.S(Size) div 2;
    if ImageInfo.Index <= 8 then
      P.RadioButton(ImageInfo.XPos, CY, Size, ImageInfo.Index >= 5, ItemEnabled, Hot, False)
    else
    begin
      if ImageInfo.Index >= 17 then State := cbGrayed
      else if ImageInfo.Index >= 13 then State := cbChecked else State := cbUnchecked;
      P.CheckBox(ImageInfo.XPos, CY, Size, State, ItemEnabled, Hot, False);
    end;
  finally P.Free end;
end;

procedure TERDVirtualStringTree.PaintThemedScrollBars;
var DC: HDC; C: TCanvas; P: TERDPainter; WindowRect, VRect, HRect: TRect;
  procedure DrawBar(ObjectId: Longint; Vertical: Boolean);
  var Info: TScrollBarInfo; R, Thumb: TRect; ArrowSize, Centre: Integer;
    FirstGlyph, LastGlyph: TERDGlyph;
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);
    if not GetScrollBarInfo(Handle, ObjectId, Info) or
      ((Info.rgstate[0] and STATE_SYSTEM_INVISIBLE) <> 0) then Exit;
    R := Info.rcScrollBar;
    OffsetRect(R, -WindowRect.Left, -WindowRect.Top);
    if Vertical then VRect := R else HRect := R;
    P.FillRect(R, Palette.Background);
    ArrowSize := Info.dxyLineButton;
    if Vertical then
    begin
      Centre := R.Left + R.Width div 2;
      FirstGlyph := glChevronUp; LastGlyph := glChevronDown;
      P.Glyph(FirstGlyph, Centre, R.Top + ArrowSize div 2, Palette.Subtle, 0.6);
      P.Glyph(LastGlyph, Centre, R.Bottom - ArrowSize div 2, Palette.Subtle, 0.6);
      Thumb := Rect(R.Left + P.S(4), R.Top + Info.xyThumbTop,
        R.Right - P.S(4), R.Top + Info.xyThumbBottom);
    end
    else
    begin
      Centre := R.Top + R.Height div 2;
      P.Glyph(glChevronLeft, R.Left + ArrowSize div 2, Centre, Palette.Subtle, 0.6);
      P.Glyph(glChevronRight, R.Right - ArrowSize div 2, Centre, Palette.Subtle, 0.6);
      Thumb := Rect(R.Left + Info.xyThumbTop, R.Top + P.S(4),
        R.Left + Info.xyThumbBottom, R.Bottom - P.S(4));
    end;
    if not Thumb.IsEmpty and ((Info.rgstate[3] and STATE_SYSTEM_INVISIBLE) = 0) then
      P.RoundRect(Thumb.Left, Thumb.Top, Thumb.Width, Thumb.Height, P.S(4), Palette.Subtle, clNone);
  end;
begin
  if not HandleAllocated or (csDestroying in ComponentState) then Exit;
  DC := GetWindowDC(Handle);
  if DC = 0 then Exit;
  C := TCanvas.Create;
  try
    C.Handle := DC;
    GetWindowRect(Handle, WindowRect);
    VRect := Rect(0, 0, 0, 0); HRect := VRect;
    P := TERDPainter.Create(C, Palette, CurrentPPI);
    try
      DrawBar(Integer(OBJID_VSCROLL), True);
      DrawBar(Integer(OBJID_HSCROLL), False);
      if not VRect.IsEmpty and not HRect.IsEmpty then
        P.FillRect(Rect(VRect.Left, HRect.Top, VRect.Right, HRect.Bottom), Palette.Background);
      if BorderStyle = TFormBorderStyle.bsSingle then
        P.FrameRect(Rect(0, 0, WindowRect.Width, WindowRect.Height), Palette.NeutralLight);
    finally P.Free end;
  finally
    C.Handle := 0;
    C.Free;
    ReleaseDC(Handle, DC);
  end;
end;

procedure TERDVirtualStringTree.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_NCPAINT, WM_PAINT, WM_VSCROLL, WM_HSCROLL, WM_NCMOUSEMOVE,
    WM_NCLBUTTONUP, WM_SIZE:
      PaintThemedScrollBars;
    WM_DPICHANGED:
      ThemeChanged;
  end;
end;

end.
