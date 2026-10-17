unit ERD.Components.VirtualTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, System.SysUtils,
  System.Math, System.UITypes, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Forms, Vcl.ImgList,
  ERD.Components.ScrollBar,
  ERD.Components.Types, ERD.Components.Theme, ERD.Components.Paint,
  ERD.Components.VT, ERD.Components.VT.Types, ERD.Components.VT.Header, ERD.Components.VT.BaseTree;

type
  TERDTreeEditorKind = (tekNone, tekText, tekInteger, tekDecimal, tekCombo,
    tekDate, tekTime, tekDateTime);
  TERDTreeGetEditorEvent = procedure(Sender: TObject; Node: PVirtualNode;
    Column: TColumnIndex; var Kind: TERDTreeEditorKind; Choices: TStrings) of object;
  TERDTreeValidateEditEvent = procedure(Sender: TObject; Node: PVirtualNode;
    Column: TColumnIndex; var Text: string; var Accept: Boolean) of object;

  /// <summary>Virtual Treeview V8.5 with ERD palettes and drawing; node/event API is retained.</summary>
  TERDVirtualStringTree = class(TERDVTVirtualStringTree, IERDThemeAware, IERDDensityAware, IERDDensitySource)
  private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FParentDensity: Boolean;
    FOnGetCellEditor: TERDTreeGetEditorEvent;
    FOnValidateCellEdit: TERDTreeValidateEditEvent;
    FVerticalBar: TERDScrollBar;
    FHorizontalBar: TERDScrollBar;
    FThemedChecks: TImageList;
    FSyncingScroll: Boolean;
    FWheelRemainder: Integer;
    FApplyingTheme: Boolean;
    FRowHeight: Integer;
    FHeaderHeight: Integer;
    FAlternateRows: Boolean;
    function GetDensity: TERDDensity;
    function GetParentDensity: Boolean;
    procedure SetDensity(Value: TERDDensity);
    procedure SetParentDensity(Value: Boolean);
    function IsDensityStored: Boolean;
    procedure SyncScrollBars;
    procedure ScrollChanged(Sender: TObject);
    procedure SetTheme(Value: TERDTheme);
    procedure SetRowHeight(Value: Integer);
    procedure SetHeaderHeight(Value: Integer);
    procedure SetAlternateRows(Value: Boolean);
    function GetThemePalette: TERDThemePalette;
    procedure ApplyTheme;

  protected
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Paint; override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
    function DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions; ClipRect: PRect = nil): Boolean; override;
    function GetClientRect: TRect; override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect); override;
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
    procedure UpdateScrollBars(DoRepaint: Boolean); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged;
    procedure DensityChanged; virtual;
    property Palette: TERDThemePalette read GetThemePalette;
    property VerticalScrollBar: TERDScrollBar read FVerticalBar;
    property HorizontalScrollBar: TERDScrollBar read FHorizontalBar;
  published
    property BorderStyle default TFormBorderStyle.bsNone;
    property Theme: TERDTheme read FTheme write SetTheme;
    property Density: TERDDensity read GetDensity write SetDensity stored IsDensityStored;
    property ParentDensity: Boolean read FParentDensity write SetParentDensity default True;
    /// <summary>Desktop row height in 96-DPI logical pixels; tablet density adds touch spacing. Individual node heights remain available.</summary>
    property RowHeight: Integer read FRowHeight write SetRowHeight default 32;
    /// <summary>Desktop header height in 96-DPI logical pixels; tablet density adds touch spacing.</summary>
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 30;
    /// <summary>Select an ERD editor and populate combo choices per cell.</summary>
    property OnNewText;
    property OnGetNodeDataSize;
    property OnGetCellEditor: TERDTreeGetEditorEvent read FOnGetCellEditor write FOnGetCellEditor;
    /// <summary>Reject or normalize an edit before OnNewText updates application data.</summary>
    property OnValidateCellEdit: TERDTreeValidateEditEvent read FOnValidateCellEdit write FOnValidateCellEdit;
    property AlternateRows: Boolean read FAlternateRows write SetAlternateRows default True;
  end;

implementation

uses
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.Edits,
  ERD.Components.SpinEdit, ERD.Components.Calendar, ERD.Components.TimePicker,
  ERD.Components.DateTimePicker, ERD.Components.VT.EditLink, ERD.Components.Hint;

type
  TERDTreeInputAccess = class(TERDCustomControl)
  public
    property AutoSize;
  end;
  TERDTreeCellEditLink = class;
  TERDTreeEditorHost = class(TERDPanel)
  private
    FLink: TERDTreeCellEditLink;
    procedure ReleaseEditor(var Message: TMessage); message CM_RELEASE;
  public
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorExit(Sender: TObject);
  end;

  TERDTreeCellEditLink = class(TERDVTWinControlEditLink)
  private
    FKind: TERDTreeEditorKind;
    FChoices: TStringList;
    FInput: TERDCustomControl;
    FStartValue: string;
    function ReadValue(out Value: string): Boolean;
  public
    constructor Create(Kind: TERDTreeEditorKind; Choices: TStrings);
    destructor Destroy; override;
    function BeginEdit: Boolean; override; stdcall;
    function PrepareEdit(Tree: TERDVTBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex): Boolean; override; stdcall;
    procedure SetBounds(R: TRect); override; stdcall;
    procedure ProcessMessage(var Message: TMessage); override; stdcall;
    procedure DoEndEdit(var Result: Boolean); override;
  end;

procedure TERDTreeEditorHost.ReleaseEditor(var Message: TMessage);
begin Free end;

procedure TERDTreeEditorHost.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Hold: IVTEditLink; Tree: TERDVTCustomVirtualStringTree;
begin
  if (FLink = nil) or FLink.Stopping then Exit;
  if (Sender is TERDComboBox) and TERDComboBox(Sender).DroppedDown then Exit;
  if (Sender is TERDDatePicker) and TERDDatePicker(Sender).IsOpen then Exit;
  if not ((Key = VK_RETURN) or (Key = VK_ESCAPE)) then Exit;
  Hold := FLink;
  if Hold = nil then Exit;
  Tree := FLink.Tree;
  if Key = VK_ESCAPE then Tree.CancelEditNode
  else if not Tree.EndEditNode then begin Key := 0; Exit end;
  Key := 0;
  if Tree.CanFocus then Tree.SetFocus;
end;

procedure TERDTreeEditorHost.EditorExit(Sender: TObject);
var Hold: IVTEditLink; FocusWindow: HWND;
begin
  if (FLink = nil) or FLink.Stopping then Exit;
  FocusWindow := GetFocus;
  if (FocusWindow = Handle) or IsChild(Handle, FocusWindow) then Exit;
  Hold := FLink;
  if Hold = nil then Exit;
  if toAutoAcceptEditChange in TERDVirtualStringTree(FLink.Tree).TreeOptions.StringOptions then
    FLink.Tree.EndEditNode
  else FLink.Tree.CancelEditNode;
end;

constructor TERDTreeCellEditLink.Create(Kind: TERDTreeEditorKind; Choices: TStrings);
begin
  inherited Create;
  FKind := Kind;
  FChoices := TStringList.Create;
  FChoices.Assign(Choices);
end;

destructor TERDTreeCellEditLink.Destroy;
begin
  if Edit <> nil then TERDTreeEditorHost(Edit).FLink := nil;
  FChoices.Free;
  inherited;
end;

function TERDTreeCellEditLink.PrepareEdit(Tree: TERDVTBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;
var Host: TERDTreeEditorHost; Text: string; Number: Double; Stamp: TDateTime;
  Spin: TERDSpinEdit; Combo: TERDComboBox; Combined: TERDDateTimePicker;
begin
  Result := inherited;
  if not Result then Exit;
  Host := TERDTreeEditorHost.Create(Tree);
  Edit := Host;
  Host.Visible := False;
  Host.Parent := Tree;
  Host.Theme := TERDVirtualStringTree(Tree).Theme;
  Host.BorderWidth := 0;
  Host.FLink := Self;
  Text := Self.Tree.Text[Node, Column];
  case FKind of
    tekInteger, tekDecimal:
      begin
        Spin := TERDSpinEdit.Create(Host);
        FInput := Spin;
        if FKind = tekDecimal then begin Spin.Decimals := 2; Spin.Increment := 0.1 end;
        if TryStrToFloat(Text, Number) then Spin.Value := Number;
      end;
    tekCombo:
      begin
        Combo := TERDComboBox.Create(Host);
        FInput := Combo;
        Combo.Style := ecsDropDown;
        Combo.FilterMode := cfsContains;
        Combo.Items.Assign(FChoices);
        Combo.Text := Text;
      end;
    tekDate:
      begin
        FInput := TERDDatePicker.Create(Host);
        if TryStrToDate(Text, Stamp) then TERDDatePicker(FInput).Date := Stamp;
      end;
    tekTime:
      begin
        FInput := TERDTimePicker.Create(Host);
        if TryStrToTime(Text, Stamp) then TERDTimePicker(FInput).Time := Stamp;
      end;
    tekDateTime:
      begin
        Combined := TERDDateTimePicker.Create(Host);
        FInput := Combined;
        if TryStrToDateTime(Text, Stamp) then Combined.DateTime := Stamp;
        Combined.DateEditor.OnKeyDown := Host.EditorKeyDown;
        Combined.TimeEditor.OnKeyDown := Host.EditorKeyDown;
        Combined.DateEditor.OnExit := Host.EditorExit;
        Combined.TimeEditor.OnExit := Host.EditorExit;
      end;
  else
    FInput := TERDEdit.Create(Host);
    TERDEdit(FInput).Text := Text;
  end;
  FInput.Parent := Host;
  FInput.Theme := Host.Theme;
  TERDTreeInputAccess(FInput).AutoSize := False;
  FInput.Align := alClient;
  FInput.OnKeyDown := Host.EditorKeyDown;
  FInput.OnExit := Host.EditorExit;
  Result := ReadValue(FStartValue);
end;

function TERDTreeCellEditLink.BeginEdit: Boolean;
begin
  Result := inherited;
  if Result then
  begin
    FInput.SetFocus;
    if FInput is TERDEdit then TERDEdit(FInput).SelectAll;
  end;
end;

function TERDTreeCellEditLink.ReadValue(out Value: string): Boolean;
var Number: Double;
begin
  Result := True;
  case FKind of
    tekInteger, tekDecimal:
      begin
        Result := TryStrToFloat(TERDSpinEdit(FInput).Text, Number) and
          not IsNan(Number) and not IsInfinite(Number);
        if not Result then Exit;
        if (FKind = tekInteger) and (Abs(Number) > 9E15) then
        begin Result := False; Exit end;
        if FKind = tekInteger then Number := Round(Number);
        TERDSpinEdit(FInput).Value := Number;
        Value := FloatToStr(TERDSpinEdit(FInput).Value);
      end;
    tekCombo: Value := TERDComboBox(FInput).Text;
    tekDate: Value := DateToStr(TERDDatePicker(FInput).Date);
    tekTime: Value := TimeToStr(TERDTimePicker(FInput).Time);
    tekDateTime: Value := DateTimeToStr(TERDDateTimePicker(FInput).DateTime);
  else Value := TERDEdit(FInput).Text;
  end;
end;

procedure TERDTreeCellEditLink.DoEndEdit(var Result: Boolean);
var Value: string; OwnerTree: TERDVirtualStringTree;
begin
  Result := ReadValue(Value);
  if not Result then Exit;
  OwnerTree := TERDVirtualStringTree(Tree);
  if Assigned(OwnerTree.FOnValidateCellEdit) then
    OwnerTree.FOnValidateCellEdit(OwnerTree, Node, Column, Value, Result);
  if not Result then Exit;
  inherited;
  if Result and (Value <> FStartValue) then Tree.Text[Node, Column] := Value;
end;

procedure TERDTreeCellEditLink.ProcessMessage(var Message: TMessage);
begin
  if FInput is TERDComboBox then
    TERDComboBox(FInput).Editor.WindowProc(Message)
  else if FInput is TERDEdit then
    TERDEdit(FInput).Editor.WindowProc(Message)
  else FInput.WindowProc(Message);
end;

procedure TERDTreeCellEditLink.SetBounds(R: TRect);
var Area: TRect; MinimumHeight: Integer;
begin
  Area := Tree.ClientRect;
  if hoVisible in Tree.Header.Options then Area.Top := Tree.Header.Height;
  if not IntersectRect(R, R, Area) then begin Edit.Hide; Exit end;
  MinimumHeight := MulDiv(24, Tree.CurrentPPI, 96);
  if R.Height < MinimumHeight then R.Bottom := Min(Area.Bottom, R.Top + MinimumHeight);
  Edit.SetBounds(R.Left, R.Top, R.Width, R.Height);
end;

function TERDVirtualStringTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
var Kind: TERDTreeEditorKind; Choices: TStringList;
begin
  Result := nil;
  // Preserve application-provided Virtual Treeview edit links.
  if Assigned(OnCreateEditor) then OnCreateEditor(Self, Node, Column, Result);
  if Result <> nil then Exit;
  Choices := TStringList.Create;
  try
    Kind := tekText;
    if Assigned(FOnGetCellEditor) then FOnGetCellEditor(Self, Node, Column, Kind, Choices);
    if Kind <> tekNone then Result := TERDTreeCellEditLink.Create(Kind, Choices);
  finally Choices.Free end;
end;


procedure TERDVirtualStringTree.CMHintShow(var Message: TCMHintShow);
begin inherited; if Message.HintInfo <> nil then Message.HintInfo.HintWindowClass := TERDHintWindow; end;

constructor TERDVirtualStringTree.Create(AOwner: TComponent);
var Bitmap: TBitmap; I: Integer;
begin
  inherited;
  FDensity := dnDesktop;
  FParentDensity := True;
  FVerticalBar := TERDScrollBar.Create(Self);
  FVerticalBar.Parent := Self;
  FVerticalBar.TabStop := False;
  FVerticalBar.Visible := False;
  FVerticalBar.OnChange := ScrollChanged;
  FHorizontalBar := TERDScrollBar.Create(Self);
  FHorizontalBar.Parent := Self;
  FHorizontalBar.Kind := sbHorizontal;
  FHorizontalBar.TabStop := False;
  FHorizontalBar.Visible := False;
  FHorizontalBar.OnChange := ScrollChanged;
  ScrollBarOptions.ScrollBars := System.UITypes.TScrollStyle.ssNone;
  FThemedChecks := TImageList.Create(Self);
  FThemedChecks.Width := 18;
  FThemedChecks.Height := 18;
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(18, 18);
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, 18, 18));
    for I := 0 to 20 do FThemedChecks.Add(Bitmap, nil);
  finally Bitmap.Free end;
  CustomCheckImages := FThemedChecks;
  FRowHeight := 32;
  FHeaderHeight := 30;
  FAlternateRows := True;
  ParentFont := False;
  Ctl3D := False;
  BorderStyle := TFormBorderStyle.bsNone;
  StyleElements := [];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toThemeAware, toUseExplorerTheme, toShowTreeLines, toFullVertGridLines] + [toHideFocusRect, toShowRoot];
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
  DensityChanged;
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
    if not (csDestroying in ComponentState) then DensityChanged;
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
var P: TERDThemePalette; PPI, CheckSize, I, OldHeight, NewHeight: Integer; Bitmap: TBitmap;
  Node: PVirtualNode;
  CheckPainter: TERDPainter; CheckState: TCheckBoxState; ItemEnabled, Hot: Boolean;
begin
  if FApplyingTheme or (FRowHeight = 0) or (csDestroying in ComponentState) then Exit;
  FApplyingTheme := True;
  try
    P := Palette;
    PPI := CurrentPPI;
    CheckSize := MulDiv(18, PPI, 96);
    if FThemedChecks <> nil then
    begin
      Bitmap := TBitmap.Create;
      CheckPainter := nil;
      BeginUpdate;
      try
        FThemedChecks.Clear;
        FThemedChecks.Width := CheckSize;
        FThemedChecks.Height := CheckSize;
        Bitmap.SetSize(CheckSize, CheckSize);
        CheckPainter := TERDPainter.Create(Bitmap.Canvas, P, PPI);
        for I := 0 to 20 do
        begin
          Bitmap.Canvas.Brush.Color := P.GaugeFace;
          Bitmap.Canvas.FillRect(Rect(0, 0, CheckSize, CheckSize));
          if I > 0 then
          begin
            ItemEnabled := ((I - 1) mod 4) <> 3;
            Hot := ((I - 1) mod 4) in [1, 2];
            if I <= 8 then
              CheckPainter.RadioButton(0, CheckSize div 2, 18, I >= 5, ItemEnabled, Hot, False)
            else
            begin
              if I >= 17 then CheckState := cbGrayed
              else if I >= 13 then CheckState := cbChecked else CheckState := cbUnchecked;
              CheckPainter.CheckBox(0, CheckSize div 2, 18, CheckState, ItemEnabled, Hot, False);
            end;
          end;
          FThemedChecks.AddMasked(Bitmap, P.GaugeFace);
        end;
      finally
        CheckPainter.Free;
        Bitmap.Free;
        EndUpdate;
      end;
    end;
    Color := P.GaugeFace;
    Font.Name := 'Segoe UI';
    Font.Height := -Max(1, Round(ERD_TEXT_BODY * PPI / 96));
    Font.Style := [];
    Font.Color := P.ForegroundText;
    Header.Font.Assign(Font);
    Header.Height := MulDiv(FHeaderHeight+DensityMetrics(Density).Edit-DensityMetrics(dnDesktop).Edit, PPI, 96);
    OldHeight := DefaultNodeHeight;
    NewHeight := MulDiv(FRowHeight+DensityMetrics(Density).Edit-DensityMetrics(dnDesktop).Edit, PPI, 96);
    if OldHeight <> NewHeight then
    begin
      BeginUpdate;
      try
        DefaultNodeHeight := NewHeight;
        Node := GetFirstNoInit;
        while Node <> nil do
        begin
          if Node.NodeHeight = OldHeight then NodeHeight[Node] := NewHeight;
          Node := GetNextNoInit(Node);
        end;
      finally EndUpdate end;
    end;
    Colors.BorderColor := P.NeutralLight;
    Colors.DisabledColor := P.Subtle;
    Colors.TreeLineColor := P.NeutralLight;
    Colors.GridLineColor := P.NeutralLight;
    Colors.HotColor := P.ForegroundText;
    Colors.HeaderHotColor := P.Accent;
    Colors.FocusedSelectionColor := ERDMixColor(P.Accent, P.GaugeFace, 0.12);
    Colors.FocusedSelectionBorderColor := Colors.FocusedSelectionColor;
    Colors.UnfocusedSelectionColor := Colors.FocusedSelectionColor;
    Colors.UnfocusedSelectionBorderColor := Colors.UnfocusedSelectionColor;
    if FVerticalBar <> nil then FVerticalBar.Theme := FTheme;
    if FHorizontalBar <> nil then FHorizontalBar.Theme := FTheme;
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
  if HandleAllocated then SyncScrollBars;
end;

function TERDVirtualStringTree.GetParentDensity: Boolean;
begin Result := FParentDensity; end;

function TERDVirtualStringTree.GetDensity: TERDDensity;
var Source: IERDDensitySource; ThemeSource: TERDTheme;
begin
  if FParentDensity and Supports(Owner, IERDDensitySource, Source) then Exit(Source.GetDensity);
  Result := FDensity; if not FParentDensity then Exit;
  ThemeSource := FTheme; if ThemeSource = nil then ThemeSource := TERDTheme.FindOnOwner(Self);
  if ThemeSource = nil then ThemeSource := TERDTheme.GetDefault;
  if (ThemeSource <> nil) and not (csDestroying in ThemeSource.ComponentState) then Result := ThemeSource.Density;
end;

function TERDVirtualStringTree.IsDensityStored: Boolean;
begin Result := not FParentDensity; end;

procedure TERDVirtualStringTree.SetDensity(Value: TERDDensity);
begin
  if (FDensity = Value) and not FParentDensity then Exit;
  FDensity := Value; FParentDensity := False; DensityChanged;
end;

procedure TERDVirtualStringTree.SetParentDensity(Value: Boolean);
begin
  if FParentDensity = Value then Exit;
  FParentDensity := Value; DensityChanged;
end;

procedure TERDVirtualStringTree.DensityChanged;
var I: Integer; Part: TComponent; Source: IERDDensitySource; Aware: IERDDensityAware;
begin
  if csDestroying in ComponentState then Exit;
  ThemeChanged;
  for I := 0 to ComponentCount-1 do
  begin
    Part := Components[I];
    if not (csDestroying in Part.ComponentState) and
      Supports(Part, IERDDensitySource, Source) and Source.GetParentDensity and
      Supports(Part, IERDDensityAware, Aware) then Aware.DensityChanged;
  end;
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
  Elements := Elements + [hpeBackground, hpeText, hpeSortGlyph];
end;

procedure TERDVirtualStringTree.DoAdvancedHeaderDraw(var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);
var P: TERDPainter; R: TRect; Fill: TColor; X: Integer; SavedFont: TFont;
begin
  SavedFont := TFont.Create;
  SavedFont.Assign(PaintInfo.TargetCanvas.Font);
  try
  if hpeBackground in Elements then
  begin
    R := PaintInfo.PaintRectangle;
    P := TERDPainter.Create(PaintInfo.TargetCanvas, Palette, CurrentPPI);
    try Fill := P.HeaderFill finally P.Free end;
    if PaintInfo.IsDownIndex then Fill := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.16)
    else if PaintInfo.IsHoverIndex then Fill := ERDMixColor(Palette.Accent, Palette.GaugeFace, 0.08);
    P := TERDPainter.Create(PaintInfo.TargetCanvas, Palette, CurrentPPI);
    try
      P.FillRect(R, Fill);
      P.HLine(R.Left, R.Top, R.Width, Palette.NeutralLight);
      if R.Left = 0 then P.VLine(R.Left, R.Top, R.Height, Palette.NeutralLight);
      P.HLine(R.Left, R.Bottom - 1, R.Width, Palette.NeutralLight);
      if PaintInfo.ShowRightBorder then
        P.VLine(R.Right - 1, R.Top + P.S(5), Max(0, R.Height - P.S(10)), Palette.NeutralLight);
    finally P.Free end;
  end;
  P := TERDPainter.Create(PaintInfo.TargetCanvas, Palette, CurrentPPI);
  try
    if (hpeText in Elements) and (PaintInfo.Column <> nil) then
    begin
      // TextRectangle is the measured caption, not the available column width.
      R := PaintInfo.PaintRectangle;
      Inc(R.Left, P.S(8));
      Dec(R.Right, P.S(8));
      if PaintInfo.ShowHeaderGlyph then R.Left := Max(R.Left, PaintInfo.TextRectangle.Left);
      if PaintInfo.ShowSortGlyph then R.Right := Min(R.Right, PaintInfo.SortGlyphPos.X - P.S(4));
      case PaintInfo.Column.Alignment of
        taRightJustify: X := R.Right;
        taCenter: X := (R.Left + R.Right) div 2;
      else X := R.Left;
      end;
      P.Caps(X, (R.Top + R.Bottom) div 2, PaintInfo.Column.Text,
        PaintInfo.Column.Alignment, Palette.ForegroundText, Max(0, R.Width));
    end;
    if (hpeSortGlyph in Elements) and PaintInfo.ShowSortGlyph then
      if Header.SortDirection = sdAscending then
        P.Glyph(glChevronUp, PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx div 2,
          PaintInfo.SortGlyphPos.Y + PaintInfo.SortGlyphSize.cy div 2, Palette.Accent, 0.7)
      else P.Glyph(glChevronDown, PaintInfo.SortGlyphPos.X + PaintInfo.SortGlyphSize.cx div 2,
          PaintInfo.SortGlyphPos.Y + PaintInfo.SortGlyphSize.cy div 2, Palette.Accent, 0.7);
  finally P.Free end;
  finally
    PaintInfo.TargetCanvas.Font.Assign(SavedFont);
    SavedFont.Free;
  end;
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
  if ((CustomCheckImages <> nil) and (CustomCheckImages <> FThemedChecks)) or (ImageInfo.Index < 1) or (ImageInfo.Index > 20) then
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

function TERDVirtualStringTree.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if csDestroying in ComponentState then Exit;
  InflateRect(Result, -1, -1);
  if (FVerticalBar <> nil) and FVerticalBar.Visible then Dec(Result.Right, FVerticalBar.Width);
  if (FHorizontalBar <> nil) and FHorizontalBar.Visible then Dec(Result.Bottom, FHorizontalBar.Height);
  Result.Right := Max(Result.Left, Result.Right);
  Result.Bottom := Max(Result.Top, Result.Bottom);
end;
procedure TERDVirtualStringTree.Paint;
var P: TERDPainter; R: TRect;
begin
  inherited;
  R := inherited GetClientRect;
  P := TERDPainter.Create(Canvas, Palette, CurrentPPI);
  try P.FrameRect(R, Palette.NeutralLight) finally P.Free end;
end;
function TERDVirtualStringTree.DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions; ClipRect: PRect): Boolean;
var View: TRect;
begin
  View := GetClientRect;
  if ClipRect = nil then ClipRect := @View;
  Result := inherited DoSetOffsetXY(Value, Options, ClipRect);
  SyncScrollBars;
end;
procedure TERDVirtualStringTree.ScrollChanged(Sender: TObject);
begin
  if FSyncingScroll then Exit;
  if Sender = FVerticalBar then OffsetY := -FVerticalBar.Position
  else if Sender = FHorizontalBar then OffsetX := -FHorizontalBar.Position;
  SyncScrollBars;
end;
procedure TERDVirtualStringTree.SyncScrollBars;
var R: TRect; Size, ViewWidth, ViewHeight, I: Integer; NeedV, NeedH: Boolean;
begin
  if FSyncingScroll or not HandleAllocated or (FVerticalBar = nil) or
    (FHorizontalBar = nil) or (csDestroying in ComponentState) then Exit;
  FSyncingScroll := True;
  try
    Winapi.Windows.GetClientRect(Handle, R);
    Size := MulDiv(14, CurrentPPI, 96);
    NeedV := False;
    NeedH := False;
    for I := 0 to 2 do
    begin
      ViewWidth := Max(0, R.Width - 2 - Ord(NeedV) * Size);
      ViewHeight := Max(0, R.Height - 2 - Ord(NeedH) * Size);
      NeedV := Int64(RangeY) > ViewHeight;
      NeedH := RangeX > ViewWidth;
    end;
    ViewWidth := Max(0, R.Width - 2 - Ord(NeedV) * Size);
    ViewHeight := Max(0, R.Height - 2 - Ord(NeedH) * Size);
    FVerticalBar.Visible := NeedV;
    FHorizontalBar.Visible := NeedH;
    FVerticalBar.SetBounds(R.Right - Size - 1, 1, Size, ViewHeight);
    FHorizontalBar.SetBounds(1, R.Bottom - Size - 1, ViewWidth, Size);
    FVerticalBar.Max := Integer(Min(Int64(MaxInt), Max(Int64(0), Int64(RangeY) - 1)));
    FVerticalBar.PageSize := Max(1, ViewHeight);
    FVerticalBar.SmallChange := DefaultNodeHeight;
    FVerticalBar.LargeChange := Max(1, ViewHeight);
    FVerticalBar.Position := Integer(Min(Int64(MaxInt), Max(Int64(0), -Int64(OffsetY))));
    if OffsetY <> -FVerticalBar.Position then OffsetY := -FVerticalBar.Position;
    FHorizontalBar.Max := Max(0, RangeX - 1);
    FHorizontalBar.PageSize := Max(1, ViewWidth);
    FHorizontalBar.Position := Integer(Min(Int64(MaxInt), Max(Int64(0), -Int64(OffsetX))));
    if OffsetX <> -FHorizontalBar.Position then OffsetX := -FHorizontalBar.Position;
    FVerticalBar.BringToFront;
    FHorizontalBar.BringToFront;
  finally FSyncingScroll := False end;
end;
procedure TERDVirtualStringTree.UpdateScrollBars(DoRepaint: Boolean);
begin
  inherited;
  SyncScrollBars;
end;
procedure TERDVirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  // Reset each cell before measuring/drawing, then preserve application customization.
  Canvas.Font.Assign(Font);
  inherited;
end;
procedure TERDVirtualStringTree.DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
var P: TERDPainter; AlwaysShow: LongBool; ShowCues: Boolean; R: TRect;
begin
  if (Node <> nil) and Focused then
  begin
    P := TERDPainter.Create(Canvas, Palette, CurrentPPI);
    try
      AlwaysShow := False;
      SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @AlwaysShow, 0);
      ShowCues := AlwaysShow or ((SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0);
      if (Node = FocusedNode) and ShowCues then
      begin
        R := Rect(Max(0, ItemRect.Left) + P.S(5), ItemRect.Top + P.S(4),
          Min(ItemRect.Right, ClientWidth) - P.S(4), ItemRect.Bottom - P.S(4));
        if (R.Width > P.S(8)) and (R.Height > P.S(8)) and
          ((not (hoVisible in Header.Options)) or (R.Top >= Header.Height)) and (R.Bottom <= ClientHeight) then
          P.FocusRing(R.Left, R.Top, R.Width, R.Height, P.S(4));
      end;
      if vsSelected in Node.States then
        P.FillRect(Rect(ItemRect.Left, ItemRect.Top + P.S(4), ItemRect.Left + P.S(3),
        ItemRect.Bottom - P.S(4)), Palette.Accent);
    finally P.Free end;
  end;
  inherited;
end;
procedure TERDVirtualStringTree.WndProc(var Message: TMessage);
var Steps, Distance, UIAction: Integer; Lines: Cardinal; UIRoot: HWND;
begin
  UIAction := 0;
  if Message.Msg = WM_LBUTTONDOWN then UIAction := UIS_SET
  else if (Message.Msg = WM_KEYDOWN) and
    (Message.WParam in [VK_TAB, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
      VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_SPACE, VK_RETURN]) then UIAction := UIS_CLEAR;
  if (UIAction <> 0) and HandleAllocated then
  begin
    UIRoot := GetAncestor(Handle, GA_ROOT);
    if UIRoot = 0 then UIRoot := Handle;
    SendMessage(UIRoot, WM_CHANGEUISTATE, WPARAM(UIAction or (UISF_HIDEFOCUS shl 16)), 0);
  end;
  if (Message.Msg = WM_MOUSEWHEEL) and Enabled and (FVerticalBar <> nil) then
  begin
    Inc(FWheelRemainder, SmallInt(HiWord(Message.WParam)));
    Steps := FWheelRemainder div WHEEL_DELTA;
    FWheelRemainder := FWheelRemainder mod WHEEL_DELTA;
    if Steps <> 0 then
    begin
      Lines := 3;
      SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Lines, 0);
      if Lines = WHEEL_PAGESCROLL then Distance := ClientHeight
      else Distance := Integer(Min(Int64(MaxInt), Int64(DefaultNodeHeight) * Lines));
      if (LoWord(Message.WParam) and MK_SHIFT) <> 0 then
        OffsetX := Integer(EnsureRange(Int64(OffsetX) + Int64(Steps) * Distance, -Int64(MaxInt), Int64(0)))
      else OffsetY := Integer(EnsureRange(Int64(OffsetY) + Int64(Steps) * Distance, -Int64(MaxInt), Int64(0)));
      SyncScrollBars;
    end;
    Message.Result := 1;
    Exit;
  end;
  inherited;
  if (Message.Msg = WM_UPDATEUISTATE) or (Message.Msg = WM_SETTINGCHANGE) or
    (Message.Msg = WM_SETFOCUS) or (Message.Msg = WM_KILLFOCUS) then Invalidate;
  if Message.Msg = WM_SIZE then SyncScrollBars;
  if Message.Msg = WM_DPICHANGED then ThemeChanged;
end;
end.
