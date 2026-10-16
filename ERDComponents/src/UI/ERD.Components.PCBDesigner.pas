unit ERD.Components.PCBDesigner;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.JSON, System.UITypes, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.PCBScene, ERD.Components.PCBLayout, ERD.Components.PCBRoute, ERD.Components.Memo, ERD.Components.Menus, ERD.Components.ScrollBar;

type
  TERDPCBItemGlyphEvent = procedure(Sender: TObject; ACanvas: TCanvas; const Bounds: TRect;
    const Item: TERDPCBDesignItem; var Handled: Boolean) of object;
  TERDPCBAlignment = (paLeft, paHorizontalCenter, paRight, paTop, paVerticalCenter, paBottom);
  TERDPCBPad = record
    ID, Net, Reference, Number: string;
    Position: TPointF;
    Polygon, Drill: TArray<TPointF>;
  end;
  TERDPCBPinCapability = (pcNone, pcAddressableLED, pcDigitalOutput, pcDigitalInput, pcAnalogInput, pcPWM,
    pcUARTTx, pcUARTRx, pcCANTx, pcCANRx, pcI2CSDA, pcI2CSCL, pcSPIMOSI, pcSPIMISO, pcSPIClock, pcSPISelect);
  TERDPCBPinCapabilities = set of TERDPCBPinCapability;
  TERDPCBChannel = record
    Pin, GPIO, Route, Name, Action, LabelText: string;
    Capability: TERDPCBPinCapability;
    Capabilities: TERDPCBPinCapabilities;
    LEDCount: Integer;
  end;
  TERDPCBTerminal = record
    ID, LabelText, SignalPin: string;
    Members: TArray<string>;
  end;
  TERDPCBComponent = record
    Reference, Device, Role: string;
    Body: TArray<TPointF>;
    Position: TPointF;
  end;
  TERDPCBDesignerErrorEvent = procedure(Sender: TObject; const MessageText: string) of object;
  /// Imported geometry is read-only; project assignments have a separate file.
  TERDPCBDesigner = class(TERDCustomControl)
  private
    FOnDrawItemGlyph: TERDPCBItemGlyphEvent;
    FOnValidateDesignItem: TERDPCBValidateItemEvent;
    FConnectionID, FHotConnectionID: string;
    FConnectionRole, FHotConnectionRole: TERDPCBPinRole;
    FSelectingConnection, FRewiring: Boolean;
    FOnConnectionSelectionChange: TNotifyEvent;
    FContextMenu: TERDPopupMenu;
    FDefaultContextMenu, FSuppressContextMenu: Boolean;
    FPlacing, FPlacementPreview: Boolean;
    FPlacementKind: TERDPCBItemKind;
    FPlacementTypeID: string;
    FPlacementPoint: TPoint;
    FOnPlacementChange: TNotifyEvent;
    FRouteCache: TDictionary<string, TERDPCBConnectionGeometry>;
    FNoteEditor: TERDMemo;
    FEditingNote: string;
    FFinishingNote: Boolean;
    FAlignmentGuides: Boolean;
    FGuideX, FGuideY: TERDPCBLayoutGuide;
    FSnapToObjects, FSnapToSpacing: Boolean;
    FSnapTolerance: Integer;
    FMoveBounds: TRect;
    FMoveAnchor: TPointF;
    FMoveTargets: TArray<TRectF>;
    FStaticLayer: TBitmap;
    FStaticDirty: Boolean;
    FAntiAlias: Boolean;
    FScene: TERDPCBScene;
    FScrollH, FScrollV: TERDScrollBar;
    FScrollBars: System.UITypes.TScrollStyle;
    FUpdatingBars: Boolean;
    FBarX, FBarY: Integer;
    FEditMode, FGrip, FDragIndex: Integer;
    FLinkPin, FLastError: string;
    FOnConnectionError: TERDPCBDesignerErrorEvent;
    FStartPoint, FBoxPoint: TPoint;
    FStartWorld: TPointF;
    FOriginalItem: TERDPCBDesignItem;
    FAppliedX, FAppliedY: Double;
    FSnapToGrid, FShowGrid: Boolean;
    FGridSize: Double;
    FOnChange: TNotifyEvent;
    FPads: TArray<TERDPCBPad>;
    FChannels: TArray<TERDPCBChannel>;
    FComponents: TArray<TERDPCBComponent>;
    FTerminals: TArray<TERDPCBTerminal>;
    FPaths, FOutline: TArray<TArray<TPointF>>;
    FBoardHash, FSelected, FHot, FBoardJSON: string;
    FBoardFileName: TFileName;
    FTemplateID, FTemplateName: string;
    FTemplateEditMode: Boolean;
    FBoardColor, FPadColor, FOutlineColor, FSelectionColor: TColor;
    FShowPads, FShowLabels, FShowComponentOutlines, FShowDrills, FShowBodies: Boolean;
    FViewBounds: TRectF;
    FZoom, FPanX, FPanY, FMaxX, FMaxY: Double;
    FScreenScale, FScreenX, FScreenY: Double;
    FDragging, FPanMode, FCompletingGesture: Boolean;
    FLast: TPoint;
    FOnSelectionChange, FOnHotPinChange, FOnViewChange: TNotifyEvent;
    function RolePin(const Item: TERDPCBDesignItem; Role: TERDPCBPinRole): string;
    function ConnectionPin: string;
    function ConnectionAnchors(ItemIndex: Integer; Role: TERDPCBPinRole; out FromPoint, ToPoint: TPoint): Boolean;
    function BuildConnectionGeometry(const FromPoint, ToPoint: TPoint; TargetIndex: Integer): TERDPCBConnectionGeometry;
    function ConnectionGeometry(ItemIndex: Integer; Role: TERDPCBPinRole): TERDPCBConnectionGeometry;
    procedure SetHotConnection(const ID: string; Role: TERDPCBPinRole);
    function ConnectionHint(const ID: string; Role: TERDPCBPinRole): string;
    procedure ReportConnectionError(const Text: string);
    procedure ContextAction(Sender: TObject);
    function SelectionBounds(out Bounds: TRect): Boolean;
    function TypeInsertionPoint(const TypeID: string; const ClientPoint: TPoint): TPointF;
    function InsertionPoint(Kind: TERDPCBItemKind; const ClientPoint: TPoint): TPointF;
    procedure PaintPlacementPreview(ACanvas: TCanvas);
    procedure BeginMoveGuides(AnchorIndex: Integer);
    procedure ClearGuides;
    procedure PaintAlignmentGuides(ACanvas: TCanvas);
    procedure SetSnapTolerance(Value: Integer);
    procedure SnapGuides(var DX, DY: Double);
    procedure NoteEditorExit(Sender: TObject);
    procedure NoteEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetPanMode(Value: Boolean);
    procedure SetAntiAlias(Value: Boolean);
    function BuildTemplateJSON: string;
    function PinPolicy: string;
    function SelectionPinFor(const Pin: string): string;
    function TerminalRect(Index: Integer): TRect;
    function IsSelectablePin(const Pin: string): Boolean;
    procedure SetTemplateEditMode(Value: Boolean);
    function ItemWithPin(Index: Integer; Role: TERDPCBPinRole; const Pin: string): TERDPCBDesignItem;
    procedure ValidateSceneItem(Sender: TObject; const Item: TERDPCBDesignItem);
    procedure SetScene(Value: TERDPCBScene);
    procedure SceneChanged(Sender: TObject);
    procedure SceneSelectionChanged(Sender: TObject);
    procedure SetScrollBars(Value: System.UITypes.TScrollStyle);
    procedure ScrollChanged(Sender: TObject);
    procedure SyncScrollBars;
    procedure SetGridSize(Value: Double);
    procedure SetSnapToGrid(Value: Boolean);
    procedure SetShowGrid(Value: Boolean);
    function ItemRect(Index: Integer): TRect;
    function ItemAt(const P: TPoint): Integer;
    function GripAt(const P: TPoint): Integer;
    function GripRect(const R: TRect; Index: Integer): TRect;
    function Snap(Value: Double): Double;
    procedure PaintBoardLayer(ACanvas: TCanvas);
    procedure PaintGrid(ACanvas: TCanvas);
    procedure PaintBodies(ACanvas: TCanvas);
    procedure SetShowBodies(Value: Boolean);
    procedure PaintScene(ACanvas: TCanvas);
    procedure CancelGesture;
    procedure ConnectPin(Index: Integer; const Pin: string);
    procedure SetBoardFileName(const Value: TFileName);
    procedure LoadBoardJSON(const Data: string);
    procedure ReadBoardData(Stream: TStream);
    procedure WriteBoardData(Stream: TStream);
    procedure SetZoom(Value: Double);
    function IsZoomStored: Boolean;
    procedure SetBoardColor(Value: TColor);
    procedure SetPadColor(Value: TColor);
    procedure SetOutlineColor(Value: TColor);
    procedure SetSelectionColor(Value: TColor);
    procedure SetShowPads(Value: Boolean);
    procedure SetShowLabels(Value: Boolean);
    procedure SetShowComponentOutlines(Value: Boolean);
    procedure SetShowDrills(Value: Boolean);
    procedure SetHot(const Value: string);
    procedure ViewChanged;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    function ViewScale: Double;
    procedure UpdateTransform;
    function ScreenPoint(const Point: TPointF): TPoint;
    function PadAt(X, Y: Integer): string;
    procedure SetSelected(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged; override;
    /// ClientPoint is the visual top-left; the resulting model position is in millimetres.
    function InsertAt(Kind: TERDPCBItemKind; const ClientPoint: TPoint): Integer;
    function InsertTypeAt(const TypeID: string; const ClientPoint: TPoint): Integer;
    function AddRegisteredItem(const TypeID: string; const Position: TPointF): Integer;
    procedure BeginTypePlacement(const TypeID: string);
    property PlacementTypeID: string read FPlacementTypeID;
    procedure BeginPlacement(Kind: TERDPCBItemKind);
    procedure PreviewPlacementAt(const ClientPoint: TPoint);
    procedure CancelPlacement;
    property Placing: Boolean read FPlacing;
    property PlacementKind: TERDPCBItemKind read FPlacementKind;
    function AddDesignItem(Kind: TERDPCBItemKind; const Position: TPointF): Integer;
    procedure AlignSelected(Alignment: TERDPCBAlignment);
    procedure DistributeSelected(Horizontal: Boolean);
    function HitTestConnection(const ClientPoint: TPoint; out ItemID: string; out Role: TERDPCBPinRole): Boolean;
    procedure SelectConnection(const ItemID: string; Role: TERDPCBPinRole);
    procedure ClearConnectionSelection;
    procedure DeleteSelectedConnection;
    procedure ReconnectSelectedPin(const Pin: string);
    procedure MoveSelectedConnection(const TargetItemID: string; TargetRole: TERDPCBPinRole);
    procedure FitSelection;
    property SelectedConnectionItemID: string read FConnectionID;
    property SelectedConnectionRole: TERDPCBPinRole read FConnectionRole;
    property SelectedConnectionPin: string read ConnectionPin;
    procedure BeginNoteEdit;
    procedure FinishNoteEdit(Accept: Boolean);
    function SupportsPinRole(ItemIndex: Integer; Role: TERDPCBPinRole): Boolean;
    /// Nonempty, compatible, unclaimed pin IDs for a specific item role.
    function GetAssignablePins(ItemIndex: Integer; Role: TERDPCBPinRole): TArray<string>;
    /// Validates without mutating the scene. Empty Pin clears a supported role.
    procedure ValidatePinAssignment(ItemIndex: Integer; Role: TERDPCBPinRole; const Pin: string);
    /// Applies a validated connection as one scene undo operation.
    procedure AssignPin(ItemIndex: Integer; Role: TERDPCBPinRole; const Pin: string);
    procedure AssignSelectedItemsToPin(const Pin: string);
    procedure SaveTemplate(const FileName: string);
    procedure ValidatePinCapabilities(const Pin: string; Capabilities: TERDPCBPinCapabilities);
    procedure SetPinCapabilities(const Pin: string; Capabilities: TERDPCBPinCapabilities; const GPIO, LabelText: string);
    procedure SetPinCapability(const Pin: string; Capability: TERDPCBPinCapability;
      const GPIO, LabelText: string);
    property TemplateID: string read FTemplateID;
    property TemplateName: string read FTemplateName;
    procedure CopySelected;
    procedure Paste;
    procedure Undo;
    procedure Redo;
    procedure LoadBoard(const FileName: string);
    procedure FitBoard;
    procedure ClearBoard;
    procedure ZoomAt(Value: Double; const ClientPoint: TPoint);
    procedure PanBy(DX, DY: Integer);
    function BoardToClient(const Point: TPointF): TPoint;
    function ClientToBoard(const Point: TPoint): TPointF;
    function HitTestPin(const ClientPoint: TPoint): string;
    function PadInfo(const Pin: string): TERDPCBPad;
    function SelectedPad: TERDPCBPad;
    function TerminalCount: Integer;
    function Terminal(Index: Integer): TERDPCBTerminal;
    function TerminalIndex(const Selection: string): Integer;
    procedure RemoveTerminal(const ID: string);
    procedure DefineTerminal(const ID, LabelText, SignalPin: string; const Members: TArray<string>);
    function ChannelCount: Integer;
    function ChannelIndex(const Pin: string): Integer;
    function Channel(Index: Integer): TERDPCBChannel;
    procedure SetBinding(Index: Integer; const Name, Action: string; LEDCount: Integer);
    procedure SaveProject(const FileName: string);
    procedure LoadProject(const FileName: string);
    property LastError: string read FLastError;
    property HotPin: string read FHot;
  published
    property OnValidateDesignItem: TERDPCBValidateItemEvent read FOnValidateDesignItem write FOnValidateDesignItem;
    property OnDrawItemGlyph: TERDPCBItemGlyphEvent read FOnDrawItemGlyph write FOnDrawItemGlyph;
    property DefaultContextMenu: Boolean read FDefaultContextMenu write FDefaultContextMenu default True;
    property OnConnectionSelectionChange: TNotifyEvent read FOnConnectionSelectionChange write FOnConnectionSelectionChange;
    property OnPlacementChange: TNotifyEvent read FOnPlacementChange write FOnPlacementChange;
    property AlignmentGuides: Boolean read FAlignmentGuides write FAlignmentGuides default True;
    property SnapToObjects: Boolean read FSnapToObjects write FSnapToObjects default True;
    property SnapToSpacing: Boolean read FSnapToSpacing write FSnapToSpacing default True;
    property SnapTolerance: Integer read FSnapTolerance write SetSnapTolerance default 6;
    property PanMode: Boolean read FPanMode write SetPanMode default False;
    property AntiAlias: Boolean read FAntiAlias write SetAntiAlias default True;
    property TemplateEditMode: Boolean read FTemplateEditMode write SetTemplateEditMode default False;
    property Scene: TERDPCBScene read FScene write SetScene;
    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default System.UITypes.TScrollStyle.ssBoth;
    property SnapToGrid: Boolean read FSnapToGrid write SetSnapToGrid default True;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property GridSize: Double read FGridSize write SetGridSize;
    property OnConnectionError: TERDPCBDesignerErrorEvent read FOnConnectionError write FOnConnectionError;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// Source path loads a board; its data is embedded in the DFM, not the path.
    property BoardFileName: TFileName read FBoardFileName write SetBoardFileName stored False;
    property SelectedPin: string read FSelected write SetSelected stored False;
    property Zoom: Double read FZoom write SetZoom stored IsZoomStored;
    property ShowBodies: Boolean read FShowBodies write SetShowBodies default True;
    property ShowPads: Boolean read FShowPads write SetShowPads default True;
    property ShowLabels: Boolean read FShowLabels write SetShowLabels default True;
    property ShowComponentOutlines: Boolean read FShowComponentOutlines write SetShowComponentOutlines default True;
    property ShowDrills: Boolean read FShowDrills write SetShowDrills default True;
    property BoardColor: TColor read FBoardColor write SetBoardColor default clDefault;
    property PadColor: TColor read FPadColor write SetPadColor default clDefault;
    property OutlineColor: TColor read FOutlineColor write SetOutlineColor default clDefault;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clDefault;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnHotPinChange: TNotifyEvent read FOnHotPinChange write FOnHotPinChange;
    property OnViewChange: TNotifyEvent read FOnViewChange write FOnViewChange;
  end;

function ERDPCBPinCapabilityName(Value: TERDPCBPinCapability): string;
function ERDPCBPinCapabilitiesText(Values: TERDPCBPinCapabilities): string;
function ERDPCBParsePinCapabilities(const Text: string): TERDPCBPinCapabilities;
implementation

uses System.IOUtils, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  Vcl.Forms, Vcl.Clipbrd, Vcl.StdCtrls, ERD.Components.Paint, ERD.Components.GDIP, Vcl.Menus;

function ERDPCBPinCapabilityName(Value: TERDPCBPinCapability): string;
begin
  case Value of
    pcAddressableLED: Result := 'addressable-led-output'; pcDigitalOutput: Result := 'digital-output';
    pcDigitalInput: Result := 'digital-input'; pcAnalogInput: Result := 'analog-input'; pcPWM: Result := 'pwm-output';
    pcUARTTx: Result := 'uart-tx'; pcUARTRx: Result := 'uart-rx'; pcCANTx: Result := 'can-tx'; pcCANRx: Result := 'can-rx';
    pcI2CSDA: Result := 'i2c-sda'; pcI2CSCL: Result := 'i2c-scl'; pcSPIMOSI: Result := 'spi-mosi'; pcSPIMISO: Result := 'spi-miso';
    pcSPIClock: Result := 'spi-clock'; pcSPISelect: Result := 'spi-select';
  else Result := 'none'; end;
end;
function ParseCapability(const Text: string): TERDPCBPinCapability;
var Value: TERDPCBPinCapability;
begin
  for Value := Low(TERDPCBPinCapability) to High(TERDPCBPinCapability) do
    if ERDPCBPinCapabilityName(Value) = Text then Exit(Value);
  raise EConvertError.Create('Unknown pin capability: ' + Text);
end;
function ERDPCBPinCapabilitiesText(Values: TERDPCBPinCapabilities): string;
var C: TERDPCBPinCapability;
begin
  Result := '';
  for C := Succ(Low(TERDPCBPinCapability)) to High(TERDPCBPinCapability) do if C in Values then
  begin if Result <> '' then Result := Result + ','; Result := Result + ERDPCBPinCapabilityName(C); end;
end;
function ERDPCBParsePinCapabilities(const Text: string): TERDPCBPinCapabilities;
var Parts: TStringList; I: Integer; C: TERDPCBPinCapability;
begin
  Result := []; if (Trim(Text) = '') or (Trim(Text) = 'none') then Exit;
  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True; Parts.Delimiter := ','; Parts.DelimitedText := Text;
    for I := 0 to Parts.Count - 1 do
    begin C := ParseCapability(Trim(Parts[I])); if C <> pcNone then System.Include(Result, C); end;
  finally Parts.Free; end;
end;
function PointValue(Value: TJSONValue): TPointF;
var A: TJSONArray;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected coordinate pair');
  A := TJSONArray(Value);
  if (A.Count <> 2) or not (A.Items[0] is TJSONNumber) or not (A.Items[1] is TJSONNumber) then
    raise EConvertError.Create('Expected two numeric coordinates');
  Result := PointF((A.Items[0] as TJSONNumber).AsDouble, (A.Items[1] as TJSONNumber).AsDouble);
end;
function PolygonValue(Value: TJSONValue): TArray<TPointF>;
var A: TJSONArray; I: Integer;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected geometry array');
  A := TJSONArray(Value); SetLength(Result, A.Count);
  for I := 0 to A.Count - 1 do Result[I] := PointValue(A.Items[I]);
end;
function PathsValue(Value: TJSONValue): TArray<TArray<TPointF>>;
var A: TJSONArray; I: Integer;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected geometry array');
  A := TJSONArray(Value); SetLength(Result, A.Count);
  for I := 0 to A.Count - 1 do Result[I] := PolygonValue(A.Items[I]);
end;
constructor TERDPCBDesigner.Create(AOwner: TComponent);
begin
  inherited; FZoom := 1; Width := 700; Height := 400; TabStop := True;
  FDefaultContextMenu := True;
  FContextMenu := TERDPopupMenu.Create(Self); FContextMenu.AutoPopup := False;
  FAlignmentGuides := True;
  FSnapToObjects := True;
  FSnapToSpacing := True;
  FSnapTolerance := 6;
  FShowBodies := True;
  FShowPads := True;
  FShowLabels := True;
  FShowComponentOutlines := True;
  FShowDrills := True;
  FBoardColor := clDefault;
  FPadColor := clDefault;
  FOutlineColor := clDefault;
  FSelectionColor := clDefault;
  FStaticDirty := True;
  FAntiAlias := True;
  FStaticLayer := TBitmap.Create; FStaticLayer.PixelFormat := pf32bit;
  FSnapToGrid := True;
  FShowGrid := True;
  FGridSize := 1;
  FScrollBars := System.UITypes.TScrollStyle.ssBoth;
  FRouteCache := TDictionary<string, TERDPCBConnectionGeometry>.Create;
  FScene := TERDPCBScene.Create(Self); FScene.OnValidateItem := ValidateSceneItem; FScene.OnChange := SceneChanged; FScene.OnSelectionChange := SceneSelectionChanged;
  FScrollH := TERDScrollBar.Create(Self); FScrollH.Parent := Self; FScrollH.Kind := sbHorizontal; FScrollH.Visible := False;
  FScrollH.OnChange := ScrollChanged; FScrollH.TabStop := False;
  FScrollV := TERDScrollBar.Create(Self); FScrollV.Parent := Self; FScrollV.Kind := sbVertical; FScrollV.Visible := False;
  FScrollV.OnChange := ScrollChanged; FScrollV.TabStop := False;
  FNoteEditor := TERDMemo.Create(Self); FNoteEditor.Parent := Self; FNoteEditor.Visible := False;
  FNoteEditor.Editor.OnExit := NoteEditorExit; FNoteEditor.OnKeyDown := NoteEditorKeyDown;
  ShowHint := True; Hint := 'Select a PCB pin or designer component';
end;
destructor TERDPCBDesigner.Destroy;
begin
  FEditingNote := ''; FOnPlacementChange := nil; FOnConnectionSelectionChange := nil; FOnValidateDesignItem := nil; FOnDrawItemGlyph := nil;
  if FNoteEditor <> nil then begin FNoteEditor.Editor.OnExit := nil; FNoteEditor.OnKeyDown := nil; end;
  if FScrollH <> nil then FScrollH.OnChange := nil;
  if FScrollV <> nil then FScrollV.OnChange := nil;
  if FScene <> nil then begin FScene.OnChange := nil; FScene.OnSelectionChange := nil; end;
  CancelGesture;
  if HandleAllocated then DestroyWnd;
  FreeAndNil(FContextMenu); FreeAndNil(FRouteCache); FreeAndNil(FScene); FreeAndNil(FStaticLayer);
  inherited;
end;
function TERDPCBDesigner.RolePin(const Item: TERDPCBDesignItem; Role: TERDPCBPinRole): string;
begin
  case Role of prPrimary: Result := Item.Pin; prSecondary: Result := Item.SecondaryPin;
    prClock: Result := Item.ClockPin; prSelect: Result := Item.SelectPin; else Result := ''; end;
end;
function TERDPCBDesigner.ConnectionPin: string;
var Index: Integer;
begin
  Result := ''; if FScene = nil then Exit;
  Index := FScene.IndexOf(FConnectionID);
  if Index >= 0 then Result := RolePin(FScene.Item(Index), FConnectionRole);
end;
function TERDPCBDesigner.ConnectionAnchors(ItemIndex: Integer; Role: TERDPCBPinRole; out FromPoint, ToPoint: TPoint): Boolean;
var R: TRect; Pad: TERDPCBPad; Count: Integer; Pin: string;
begin
  Result := False; if not SupportsPinRole(ItemIndex, Role) then Exit;
  Pin := RolePin(FScene.Item(ItemIndex), Role); if Pin = '' then Exit;
  UpdateTransform;
  for Pad in FPads do if Pad.ID = Pin then begin FromPoint := ScreenPoint(Pad.Position); Result := True; Break; end;
  if not Result then Exit;
  R := ItemRect(ItemIndex); Count := 1;
  if SupportsPinRole(ItemIndex, prSecondary) then Count := 2;
  if SupportsPinRole(ItemIndex, prSelect) then Count := 4;
  ToPoint := Point(R.Left + R.Width * (Ord(Role) + 1) div (Count + 1), R.Top);
  if (R.Top + R.Bottom) div 2 < FromPoint.Y then ToPoint.Y := R.Bottom;
  Result := True;
end;
function TERDPCBDesigner.BuildConnectionGeometry(const FromPoint, ToPoint: TPoint; TargetIndex: Integer): TERDPCBConnectionGeometry;
var Key: string; Boxes: TArray<TRect>; I, N: Integer;
begin
  Key := IntToStr(FromPoint.X) + ',' + IntToStr(FromPoint.Y) + ':' +
    IntToStr(ToPoint.X) + ',' + IntToStr(ToPoint.Y) + ':' + IntToStr(TargetIndex);
  if FRouteCache.TryGetValue(Key, Result) then Exit;
  SetLength(Boxes, FScene.Count); N := 0;
  for I := 0 to FScene.Count - 1 do if I <> TargetIndex then begin Boxes[N] := ItemRect(I); Inc(N); end;
  SetLength(Boxes, N); Result := ERDBuildPCBConnection(FromPoint, ToPoint, Boxes, ScaleValue(96) / 96);
  if FRouteCache.Count >= 256 then FRouteCache.Clear;
  FRouteCache.AddOrSetValue(Key, Result);
end;
function TERDPCBDesigner.ConnectionGeometry(ItemIndex: Integer; Role: TERDPCBPinRole): TERDPCBConnectionGeometry;
var P, Q: TPoint;
begin
  Result := Default(TERDPCBConnectionGeometry);
  if ConnectionAnchors(ItemIndex, Role, P, Q) then Result := BuildConnectionGeometry(P, Q, ItemIndex);
end;
function TERDPCBDesigner.HitTestConnection(const ClientPoint: TPoint; out ItemID: string; out Role: TERDPCBPinRole): Boolean;
var I: Integer; Candidate: TERDPCBPinRole; Distance, Best: Double; Geometry: TERDPCBConnectionGeometry;
begin
  ItemID := ''; Role := prPrimary; Best := ScaleValue(6) + 0.01; UpdateTransform;
  for I := FScene.Count - 1 downto 0 do for Candidate := Low(TERDPCBPinRole) to High(TERDPCBPinRole) do
  begin
    Geometry := ConnectionGeometry(I, Candidate);
    Distance := ERDPCBConnectionDistance(Geometry, PointF(ClientPoint.X, ClientPoint.Y));
    if Distance < Best then begin Best := Distance; ItemID := FScene.Item(I).ID; Role := Candidate; end;
  end;
  Result := ItemID <> '';
end;
procedure TERDPCBDesigner.SetHotConnection(const ID: string; Role: TERDPCBPinRole);
begin
  if (FHotConnectionID = ID) and ((ID = '') or (FHotConnectionRole = Role)) then Exit;
  FHotConnectionID := ID; FHotConnectionRole := Role; Invalidate; Application.CancelHint;
end;
procedure TERDPCBDesigner.SelectConnection(const ItemID: string; Role: TERDPCBPinRole);
var Index: Integer;
begin
  Index := FScene.IndexOf(ItemID);
  if Index < 0 then raise EArgumentException.Create('Connection component no longer exists');
  if not SupportsPinRole(Index, Role) or (RolePin(FScene.Item(Index), Role) = '') then
    raise EArgumentException.Create('No connection for this role');
  FSelectingConnection := True;
  try
    FConnectionID := ItemID; FConnectionRole := Role; FSelected := ''; SetHot(''); FScene.Select(Index);
  finally FSelectingConnection := False; end;
  FStaticDirty := True; Invalidate;
  if Assigned(FOnConnectionSelectionChange) then FOnConnectionSelectionChange(Self);
end;
procedure TERDPCBDesigner.ClearConnectionSelection;
begin
  if FConnectionID = '' then Exit;
  FConnectionID := ''; FStaticDirty := True; Invalidate;
  if Assigned(FOnConnectionSelectionChange) then FOnConnectionSelectionChange(Self);
end;
procedure TERDPCBDesigner.DeleteSelectedConnection;
var Index: Integer;
begin
  Index := FScene.IndexOf(FConnectionID); if Index < 0 then Exit;
  if FScene.Item(Index).Locked then raise EArgumentException.Create('Unlock the component before disconnecting it');
  AssignPin(Index, FConnectionRole, ''); ClearConnectionSelection;
end;
procedure TERDPCBDesigner.ReconnectSelectedPin(const Pin: string);
var Index: Integer;
begin
  Index := FScene.IndexOf(FConnectionID); if Index < 0 then raise EArgumentException.Create('Select a connection first');
  if FScene.Item(Index).Locked then raise EArgumentException.Create('Unlock the component before reconnecting it');
  if Pin = '' then raise EArgumentException.Create('Drop on a programmable pin');
  AssignPin(Index, FConnectionRole, Pin); FStaticDirty := True; Invalidate;
  if Assigned(FOnConnectionSelectionChange) then FOnConnectionSelectionChange(Self);
end;
procedure TERDPCBDesigner.MoveSelectedConnection(const TargetItemID: string; TargetRole: TERDPCBPinRole);
var SourceIndex, TargetIndex: Integer; SourceID, Pin: string; SourceRole: TERDPCBPinRole;
begin
  SourceID := FConnectionID; SourceRole := FConnectionRole; Pin := ConnectionPin;
  SourceIndex := FScene.IndexOf(SourceID); TargetIndex := FScene.IndexOf(TargetItemID);
  if (SourceIndex < 0) or (TargetIndex < 0) or (Pin = '') then raise EArgumentException.Create('Select a connection and a destination component');
  if (SourceID = TargetItemID) and (SourceRole = TargetRole) then Exit;
  if FScene.Item(SourceIndex).Locked or FScene.Item(TargetIndex).Locked then raise EArgumentException.Create('Unlock both components before reconnecting');
  if not SupportsPinRole(TargetIndex, TargetRole) then raise EArgumentException.Create('Destination does not support this connection role');
  if RolePin(FScene.Item(TargetIndex), TargetRole) <> '' then raise EArgumentException.Create('Destination role is already connected');
  FRewiring := True; FScene.BeginEdit;
  try
    try
      AssignPin(SourceIndex, SourceRole, ''); AssignPin(TargetIndex, TargetRole, Pin); FScene.EndEdit;
    except FScene.CancelEdit; raise; end;
  finally FRewiring := False; end;
  SelectConnection(TargetItemID, TargetRole);
end;
function TERDPCBDesigner.ConnectionHint(const ID: string; Role: TERDPCBPinRole): string;
var Index, ChannelID: Integer; Value: TERDPCBDesignItem; Pin, RoleName: string;
begin
  Result := ''; Index := FScene.IndexOf(ID); if Index < 0 then Exit;
  Value := FScene.Item(Index); Pin := RolePin(Value, Role); if Pin = '' then Exit;
  case Role of prPrimary: RoleName := 'Pin'; prSecondary: RoleName := 'Secondary'; prClock: RoleName := 'Clock'; else RoleName := 'Select'; end;
  case Value.Kind of
    piUART, piCANBus: if Role = prPrimary then RoleName := 'TX' else RoleName := 'RX';
    piI2C: if Role = prPrimary then RoleName := 'SDA' else RoleName := 'SCL';
    piSPI: case Role of prPrimary: RoleName := 'MOSI'; prSecondary: RoleName := 'MISO'; prClock: RoleName := 'Clock'; prSelect: RoleName := 'Chip select'; end;
    piOutputGroup: RoleName := 'LED data';
  end;
  Result := Value.Name + ' / ' + RoleName + ' - ' + Pin;
  ChannelID := ChannelIndex(Pin); if (ChannelID >= 0) and (FChannels[ChannelID].GPIO <> '') then Result := Result + ' / ' + FChannels[ChannelID].GPIO;
end;
procedure TERDPCBDesigner.ReportConnectionError(const Text: string);
begin FLastError := Text; if Assigned(FOnConnectionError) then FOnConnectionError(Self, Text); end;
function TERDPCBDesigner.SelectionBounds(out Bounds: TRect): Boolean;
var I, Index: Integer; R: TRect; P: TPoint; Point: TPointF; Geometry: TERDPCBConnectionGeometry;
  procedure Include(const Rect: TRect);
  begin
    if not Result then begin Bounds := Rect; Result := True; end
    else begin Bounds.Left := Min(Bounds.Left, Rect.Left); Bounds.Top := Min(Bounds.Top, Rect.Top);
      Bounds.Right := Max(Bounds.Right, Rect.Right); Bounds.Bottom := Max(Bounds.Bottom, Rect.Bottom); end;
  end;
begin
  Result := False; Bounds := Rect(0,0,0,0); UpdateTransform;
  Index := FScene.IndexOf(FConnectionID);
  if Index >= 0 then
  begin
    Geometry := ConnectionGeometry(Index, FConnectionRole);
    for Point in Geometry.Samples do Include(Rect(Floor(Point.X), Floor(Point.Y), Ceil(Point.X)+1, Ceil(Point.Y)+1));
    Include(ItemRect(Index)); Exit;
  end;
  for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected then
  begin
    R := ItemRect(I);
    if FScene.Item(I).Kind <> piNote then begin Dec(R.Left, ScaleValue(20)); Inc(R.Right, ScaleValue(20)); Inc(R.Bottom, ScaleValue(22)); end;
    Include(R);
  end;
  if Result or (FSelected = '') then Exit;
  Index := TerminalIndex(FSelected);
  if (Index >= 0) and not FTemplateEditMode then begin Include(TerminalRect(Index)); Exit; end;
  for I := 0 to High(FPads) do if FPads[I].ID = FSelected then
    for Point in FPads[I].Polygon do begin P := ScreenPoint(Point); Include(Rect(P.X,P.Y,P.X+1,P.Y+1)); end;
end;
procedure TERDPCBDesigner.FitSelection;
var Bounds: TRect; Center: TPoint; Ratio, PreviousZoom: Double; I: Integer;
begin
  FinishNoteEdit(True); CancelGesture;
  // Re-evaluate after zooming because nonvisual glyphs keep a fixed pixel size.
  for I := 0 to 2 do
  begin
    if not SelectionBounds(Bounds) then Exit;
    Center := Point((Bounds.Left+Bounds.Right) div 2, (Bounds.Top+Bounds.Bottom) div 2);
    Ratio := Min(Max(1, ClientWidth - ScaleValue(64)) / Max(1, Bounds.Width),
      Max(1, ClientHeight - ScaleValue(64)) / Max(1, Bounds.Height));
    PreviousZoom := FZoom; ZoomAt(FZoom * Ratio, Center);
    PanBy(ClientWidth div 2 - Center.X, ClientHeight div 2 - Center.Y);
    if SameValue(PreviousZoom, FZoom) then Break;
  end;
end;
procedure TERDPCBDesigner.ContextAction(Sender: TObject);
var I: Integer; Value: TERDPCBDesignItem; Lock: Boolean;
begin
  if not (Sender is TMenuItem) then Exit;
  try
    case TMenuItem(Sender).Tag of
      1: Undo; 2: Redo; 3: FitSelection; 4: FitBoard; 5: DeleteSelectedConnection;
      6: CopySelected; 7: Paste; 8: FScene.DeleteSelected; 9: FScene.SelectAll;
      10: BeginNoteEdit;
      11:
        begin
          Lock := False;
          for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected and not FScene.Item(I).Locked then Lock := True;
          FScene.BeginEdit;
          try
            for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected then
            begin Value := FScene.Item(I); Value.Locked := Lock; FScene.UpdateItem(I, Value); end;
            FScene.EndEdit;
          except FScene.CancelEdit; raise; end;
        end;
      12: begin CancelPlacement; PanMode := False; end;
      13: SnapToGrid := not SnapToGrid;
      14: SnapToObjects := not SnapToObjects;
      15: SnapToSpacing := not SnapToSpacing;
      16: begin AlignmentGuides := not AlignmentGuides; Invalidate; end;
    end;
  except on E: Exception do ReportConnectionError(E.Message); end;
end;
procedure TERDPCBDesigner.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var I: Integer; ID, Pin: string; Role: TERDPCBPinRole; Bounds: TRect; ScreenPoint: TPoint;
    Keyboard, CanRemove, CanEditNote: Boolean;
  procedure Entry(const Caption: string; Tag: Integer; Enabled: Boolean = True);
  var Item: TMenuItem;
  begin
    Item := TMenuItem.Create(FContextMenu); Item.Caption := Caption; Item.Tag := Tag;
    Item.Enabled := Enabled; Item.OnClick := ContextAction; FContextMenu.Items.Add(Item);
  end;
begin
  inherited;
  if FSuppressContextMenu then begin FSuppressContextMenu := False; Handled := True; Exit; end;
  if Handled or not FDefaultContextMenu or (PopupMenu <> nil) or not Enabled then Exit;
  FinishNoteEdit(True); CancelGesture; Keyboard := (MousePos.X = -1) and (MousePos.Y = -1);
  if Keyboard then
  begin
    if SelectionBounds(Bounds) then MousePos := Point((Bounds.Left+Bounds.Right) div 2, (Bounds.Top+Bounds.Bottom) div 2)
    else MousePos := Point(ClientWidth div 2, ClientHeight div 2);
  end
  else
  begin
    I := ItemAt(MousePos); Pin := PadAt(MousePos.X, MousePos.Y);
    if (I < 0) and HitTestConnection(MousePos, ID, Role) and ((Pin = '') or (ID = FConnectionID)) then SelectConnection(ID, Role)
    else if I >= 0 then begin ClearConnectionSelection; if not FScene.Item(I).Selected then FScene.Select(I); end
    else if Pin <> '' then begin ClearConnectionSelection; SelectedPin := Pin; end;
  end;
  SetFocus; FContextMenu.Items.Clear; FContextMenu.PopupComponent := Self;
  FContextMenu.Theme := Theme; FContextMenu.Density := Density;
  Entry('Undo', 1, FScene.CanUndo); Entry('Redo', 2, FScene.CanRedo); Entry('-', 0);
  if FConnectionID <> '' then
  begin
    I := FScene.IndexOf(FConnectionID); CanRemove := (I >= 0) and not FScene.Item(I).Locked;
    Entry('Disconnect', 5, CanRemove);
  end
  else
  begin
    CanRemove := False;
    for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected and not FScene.Item(I).Locked then CanRemove := True;
    Entry('Copy', 6, FScene.SelectionCount > 0); Entry('Paste', 7, Clipboard.HasFormat(CF_UNICODETEXT) and not FTemplateEditMode);
    Entry('Delete objects', 8, CanRemove); Entry('Select all objects', 9, FScene.Count > 0);
    I := FScene.PrimaryIndex; CanEditNote := False;
    if I >= 0 then CanEditNote := (FScene.Item(I).Kind = piNote) and not FScene.Item(I).Locked;
    if CanEditNote then Entry('Edit note', 10);
    if FScene.SelectionCount > 0 then Entry('Lock / unlock selection', 11);
  end;
  Entry('-',0); Entry('Zoom to selection (F)', 3, SelectionBounds(Bounds)); Entry('Fit board (Home)', 4);
  if FPlacing or FPanMode then Entry('Select / move', 12);
  Entry('-',0);
  Entry('Snap to grid',13); FContextMenu.Items[FContextMenu.Items.Count-1].Checked := SnapToGrid;
  Entry('Snap to objects',14); FContextMenu.Items[FContextMenu.Items.Count-1].Checked := SnapToObjects;
  Entry('Snap to equal spacing',15); FContextMenu.Items[FContextMenu.Items.Count-1].Checked := SnapToSpacing;
  Entry('Show alignment guides',16); FContextMenu.Items[FContextMenu.Items.Count-1].Checked := AlignmentGuides;
  Handled := True; ScreenPoint := ClientToScreen(MousePos); FContextMenu.Popup(ScreenPoint.X, ScreenPoint.Y);
end;
function TERDPCBDesigner.InsertionPoint(Kind: TERDPCBItemKind; const ClientPoint: TPoint): TPointF;
begin
  Result := ClientToBoard(ClientPoint);
  if Kind = piNote then Result.Y := Result.Y - 18
  else Result.Y := Result.Y - ScaleValue(32) / FScreenScale;
  Result.X := Snap(Result.X); Result.Y := Snap(Result.Y);
end;
function TERDPCBDesigner.TypeInsertionPoint(const TypeID: string; const ClientPoint: TPoint): TPointF;
var Definition: TERDPCBComponentType;
begin
  if not ERDPCBFindComponentType(TypeID, Definition) then raise EArgumentException.Create('Unknown component type: ' + TypeID);
  Result := InsertionPoint(Definition.Kind, ClientPoint);
  if Definition.Kind = piNote then Result.Y := Snap(ClientToBoard(ClientPoint).Y - Definition.Height);
end;
function TERDPCBDesigner.AddRegisteredItem(const TypeID: string; const Position: TPointF): Integer;
begin Result := FScene.AddType(TypeID, PointF(Snap(Position.X), Snap(Position.Y))); end;
function TERDPCBDesigner.InsertTypeAt(const TypeID: string; const ClientPoint: TPoint): Integer;
begin
  if FTemplateEditMode then raise EArgumentException.Create('End template authoring before inserting scene items');
  FinishNoteEdit(True); CancelGesture;
  Result := AddRegisteredItem(TypeID, TypeInsertionPoint(TypeID, ClientPoint));
end;
function TERDPCBDesigner.InsertAt(Kind: TERDPCBItemKind; const ClientPoint: TPoint): Integer;
begin
  if FTemplateEditMode then raise EArgumentException.Create('End template authoring before inserting scene items');
  FinishNoteEdit(True); CancelGesture;
  Result := AddDesignItem(Kind, InsertionPoint(Kind, ClientPoint));
end;
procedure TERDPCBDesigner.BeginPlacement(Kind: TERDPCBItemKind);
begin BeginTypePlacement(ERDPCBBuiltInTypeID(Kind)); end;
procedure TERDPCBDesigner.BeginTypePlacement(const TypeID: string);
var Definition: TERDPCBComponentType;
begin
  if not ERDPCBFindComponentType(TypeID, Definition) then raise EArgumentException.Create('Unknown component type: ' + TypeID);
  if FTemplateEditMode then raise EArgumentException.Create('End template authoring before placing scene items');
  FinishNoteEdit(True); CancelGesture; PanMode := False;
  ClearConnectionSelection; FPlacementKind := Definition.Kind; FPlacementTypeID := Definition.TypeID; FPlacing := True; FPlacementPreview := False;
  Cursor := crCross; SetHot(''); Invalidate;
  if Assigned(FOnPlacementChange) then FOnPlacementChange(Self);
end;
procedure TERDPCBDesigner.PreviewPlacementAt(const ClientPoint: TPoint);
begin
  if not FPlacing then Exit;
  FPlacementPoint := ClientPoint; FPlacementPreview := PtInRect(ClientRect, ClientPoint); Invalidate;
end;
procedure TERDPCBDesigner.CancelPlacement;
begin
  if not FPlacing then Exit;
  FPlacing := False; FPlacementPreview := False;
  if FPanMode then Cursor := crHandPoint else Cursor := crDefault;
  Invalidate;
  if Assigned(FOnPlacementChange) then FOnPlacementChange(Self);
end;
procedure TERDPCBDesigner.PaintPlacementPreview(ACanvas: TCanvas);
var World: TPointF; P, Q: TPoint; R: TRect; Text: string; Definition: TERDPCBComponentType;
begin
  if not FPlacing or not FPlacementPreview or FDragging then Exit;
  if not ERDPCBFindComponentType(FPlacementTypeID, Definition) then Exit;
  World := TypeInsertionPoint(FPlacementTypeID, FPlacementPoint);
  P := ScreenPoint(World);
  if FPlacementKind = piNote then
  begin Q := ScreenPoint(PointF(World.X + Definition.Width, World.Y + Definition.Height)); R := Rect(P.X, Q.Y, Q.X, P.Y); end
  else R := Rect(P.X, P.Y - ScaleValue(32), P.X + ScaleValue(32), P.Y);
  ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := 1; ACanvas.Pen.Style := psDash;
  ACanvas.Rectangle(R); ACanvas.Pen.Style := psSolid;
  ACanvas.Font.Assign(Font); ACanvas.Font.Color := Palette.Accent;
  Text := Definition.DisplayName;
  ACanvas.TextOut(R.Left, R.Bottom + ScaleValue(4), Text);
end;
procedure TERDPCBDesigner.AlignSelected(Alignment: TERDPCBAlignment);
var I: Integer; Bounds, R: TRect; Value: TERDPCBDesignItem; DX, DY: Integer; Found: Boolean;
begin
  FinishNoteEdit(True); CancelGesture; UpdateTransform; Found := False;
  for I := 0 to FScene.Count - 1 do
    if FScene.Item(I).Selected and not FScene.Item(I).Locked then
    begin
      R := ItemRect(I);
      if not Found then begin Bounds := R; Found := True; end
      else begin Bounds.Left := Min(Bounds.Left, R.Left); Bounds.Right := Max(Bounds.Right, R.Right);
        Bounds.Top := Min(Bounds.Top, R.Top); Bounds.Bottom := Max(Bounds.Bottom, R.Bottom); end;
    end;
  if not Found then Exit;
  FScene.BeginEdit;
  try
    for I := 0 to FScene.Count - 1 do
      if FScene.Item(I).Selected and not FScene.Item(I).Locked then
      begin
        R := ItemRect(I); DX := 0; DY := 0;
        case Alignment of
          paLeft: DX := Bounds.Left - R.Left;
          paHorizontalCenter: DX := (Bounds.Left + Bounds.Right - R.Left - R.Right) div 2;
          paRight: DX := Bounds.Right - R.Right;
          paTop: DY := Bounds.Top - R.Top;
          paVerticalCenter: DY := (Bounds.Top + Bounds.Bottom - R.Top - R.Bottom) div 2;
          paBottom: DY := Bounds.Bottom - R.Bottom;
        end;
        Value := FScene.Item(I); Value.X := Value.X + DX / FScreenScale; Value.Y := Value.Y - DY / FScreenScale;
        FScene.UpdateItem(I, Value);
      end;
    FScene.EndEdit;
  except FScene.CancelEdit; raise; end;
end;
procedure TERDPCBDesigner.DistributeSelected(Horizontal: Boolean);
var Items: TArray<Integer>; I, J, Swap, N: Integer; R, First, Last: TRect;
    Total, Gap, Next, Delta: Double; Value: TERDPCBDesignItem;
  function Position(Index: Integer): Integer;
  var Bounds: TRect;
  begin Bounds := ItemRect(Index); if Horizontal then Result := Bounds.Left else Result := Bounds.Top; end;
begin
  FinishNoteEdit(True); CancelGesture; UpdateTransform; N := 0;
  for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected and not FScene.Item(I).Locked then
  begin SetLength(Items, N + 1); Items[N] := I; Inc(N); end;
  if N < 3 then Exit;
  for I := 1 to N - 1 do
  begin
    J := I;
    while J > 0 do
    begin
      if Position(Items[J]) >= Position(Items[J - 1]) then Break;
      Swap := Items[J]; Items[J] := Items[J - 1]; Items[J - 1] := Swap; Dec(J);
    end;
  end;
  Total := 0;
  for I := 0 to N - 1 do begin R := ItemRect(Items[I]); if Horizontal then Total := Total + R.Width else Total := Total + R.Height; end;
  First := ItemRect(Items[0]); Last := ItemRect(Items[N - 1]);
  if Horizontal then begin Gap := (Last.Right - First.Left - Total) / (N - 1); Next := First.Left + First.Width + Gap; end
  else begin Gap := (Last.Bottom - First.Top - Total) / (N - 1); Next := First.Top + First.Height + Gap; end;
  FScene.BeginEdit;
  try
    for I := 1 to N - 2 do
    begin
      R := ItemRect(Items[I]); Value := FScene.Item(Items[I]);
      if Horizontal then begin Delta := Next - R.Left; Value.X := Value.X + Delta / FScreenScale; Next := Next + R.Width + Gap; end
      else begin Delta := Next - R.Top; Value.Y := Value.Y - Delta / FScreenScale; Next := Next + R.Height + Gap; end;
      FScene.UpdateItem(Items[I], Value);
    end;
    FScene.EndEdit;
  except FScene.CancelEdit; raise; end;
end;
procedure TERDPCBDesigner.ClearGuides;
begin FGuideX := Default(TERDPCBLayoutGuide); FGuideY := Default(TERDPCBLayoutGuide); end;
procedure TERDPCBDesigner.SetSnapTolerance(Value: Integer);
begin
  if (Value < 1) or (Value > 32) then raise EArgumentException.Create('Snap tolerance must be 1..32 pixels');
  FSnapTolerance := Value;
end;
procedure TERDPCBDesigner.BeginMoveGuides(AnchorIndex: Integer);
var I, N: Integer; R: TRect; Value: TERDPCBDesignItem; Found: Boolean;
begin
  UpdateTransform; ClearGuides; FMoveTargets := nil; Found := False;
  Value := FScene.Item(AnchorIndex); FMoveAnchor := PointF(Value.X,Value.Y);
  for I := 0 to FScene.Count-1 do
  begin
    Value := FScene.Item(I); R := ItemRect(I);
    if Value.Selected and not Value.Locked then
    begin
      if not Found then begin FMoveBounds := R; Found := True; end
      else begin FMoveBounds.Left := Min(FMoveBounds.Left,R.Left); FMoveBounds.Top := Min(FMoveBounds.Top,R.Top);
        FMoveBounds.Right := Max(FMoveBounds.Right,R.Right); FMoveBounds.Bottom := Max(FMoveBounds.Bottom,R.Bottom); end;
    end
    else begin N := Length(FMoveTargets); SetLength(FMoveTargets,N+1); FMoveTargets[N] := System.Types.RectF(R.Left,R.Top,R.Right,R.Bottom); end;
  end;
end;
procedure TERDPCBDesigner.SnapGuides(var DX, DY: Double);
var Moving, Board: TRectF; P, Q: TPoint; Correction: TPointF; GridDX, GridDY: Double;
begin
  GridDX := Snap(FMoveAnchor.X+DX)-FMoveAnchor.X; GridDY := Snap(FMoveAnchor.Y+DY)-FMoveAnchor.Y;
  Moving := System.Types.RectF(FMoveBounds.Left+DX*FScreenScale,FMoveBounds.Top-DY*FScreenScale,
    FMoveBounds.Right+DX*FScreenScale,FMoveBounds.Bottom-DY*FScreenScale);
  P := ScreenPoint(PointF(0,FMaxY)); Q := ScreenPoint(PointF(FMaxX,0)); Board := System.Types.RectF(P.X,P.Y,Q.X,Q.Y);
  ERDPCBSnapLayout(Moving,FMoveTargets,Board,(FMaxX > 0) and (FMaxY > 0),
    FSnapToObjects,FSnapToSpacing,ScaleValue(FSnapTolerance),Correction,FGuideX,FGuideY);
  if FGuideX.Kind <> lgNone then DX := DX+Correction.X/FScreenScale else DX := GridDX;
  if FGuideY.Kind <> lgNone then DY := DY-Correction.Y/FScreenScale else DY := GridDY;
end;
procedure TERDPCBDesigner.PaintAlignmentGuides(ACanvas: TCanvas);
var Saved: Integer;
  procedure DrawGuide(const Guide: TERDPCBLayoutGuide; Horizontal: Boolean);
  var I, Position, Start, Finish, Tick: Integer;
  begin
    if Guide.Kind = lgNone then Exit;
    Position := Round(Guide.Coordinate); Tick := ScaleValue(3);
    if Guide.Kind = lgAlignment then
    begin
      ACanvas.Pen.Style := psDot;
      if Horizontal then begin ACanvas.MoveTo(Position,Round(Guide.SpanStart)-Tick); ACanvas.LineTo(Position,Round(Guide.SpanEnd)+Tick); end
      else begin ACanvas.MoveTo(Round(Guide.SpanStart)-Tick,Position); ACanvas.LineTo(Round(Guide.SpanEnd)+Tick,Position); end;
    end
    else
    begin
      ACanvas.Pen.Style := psSolid; Inc(Position,ScaleValue(8));
      for I := 0 to 1 do
      begin
        Start := Round(Guide.Gaps[I].X); Finish := Round(Guide.Gaps[I].Y);
        if Horizontal then
        begin
          ACanvas.MoveTo(Start,Position); ACanvas.LineTo(Finish,Position);
          ACanvas.MoveTo(Start,Position-Tick); ACanvas.LineTo(Start,Position+Tick+1);
          ACanvas.MoveTo(Finish,Position-Tick); ACanvas.LineTo(Finish,Position+Tick+1);
        end
        else
        begin
          ACanvas.MoveTo(Position,Start); ACanvas.LineTo(Position,Finish);
          ACanvas.MoveTo(Position-Tick,Start); ACanvas.LineTo(Position+Tick+1,Start);
          ACanvas.MoveTo(Position-Tick,Finish); ACanvas.LineTo(Position+Tick+1,Finish);
        end;
      end;
    end;
  end;
begin
  if (FEditMode <> 1) or not FAlignmentGuides then Exit;
  Saved := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle,0,0,ClientWidth,ClientHeight);
    ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := 1;
    DrawGuide(FGuideX,True); DrawGuide(FGuideY,False);
  finally RestoreDC(ACanvas.Handle,Saved); end;
  // Keep TCanvas's cached pen state in sync with the restored DC.
  ACanvas.Pen.Style := psSolid;
end;
procedure TERDPCBDesigner.BeginNoteEdit;
var Index: Integer; Value: TERDPCBDesignItem; R: TRect;
begin
  CancelPlacement; FinishNoteEdit(True); CancelGesture;
  Index := FScene.PrimaryIndex; if Index < 0 then Exit;
  Value := FScene.Item(Index); if (Value.Kind <> piNote) or Value.Locked then Exit;
  UpdateTransform;
  if not IntersectRect(R, ItemRect(Index), ClientRect) then
  begin FitBoard; UpdateTransform; if not IntersectRect(R, ItemRect(Index), ClientRect) then Exit; end;
  InflateRect(R, -1, -1);
  FNoteEditor.SetBounds(R.Left, R.Top, Max(24, R.Width), Max(24, R.Height));
  FNoteEditor.Text := Value.Caption; FEditingNote := Value.ID;
  FNoteEditor.Visible := True; FNoteEditor.BringToFront; FNoteEditor.SetFocus;
end;
procedure TERDPCBDesigner.FinishNoteEdit(Accept: Boolean);
var Index: Integer; Value: TERDPCBDesignItem; Text, ID: string;
begin
  if FFinishingNote or (FEditingNote = '') then Exit;
  FFinishingNote := True;
  try
    ID := FEditingNote; FEditingNote := ''; Text := FNoteEditor.Text;
    FNoteEditor.Visible := False;
    Index := FScene.IndexOf(ID);
    if Accept and (Index >= 0) then
    begin
      Value := FScene.Item(Index);
      if not Value.Locked and (Value.Kind = piNote) and (Value.Caption <> Text) then
      begin Value.Caption := Text; FScene.UpdateItem(Index, Value); end;
    end;
  finally FFinishingNote := False; end;
  Invalidate;
end;
procedure TERDPCBDesigner.NoteEditorExit(Sender: TObject);
begin FinishNoteEdit(True); end;
procedure TERDPCBDesigner.NoteEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or ((Key = VK_RETURN) and (ssCtrl in Shift)) then
  begin FinishNoteEdit(Key <> VK_ESCAPE); Key := 0; SetFocus; end;
end;
procedure TERDPCBDesigner.DblClick;
begin
  if FPlacing then Exit;
  if FScene.PrimaryIndex >= 0 then
    if FScene.Item(FScene.PrimaryIndex).Kind = piNote then BeginNoteEdit;
  inherited;
end;
function TERDPCBDesigner.TerminalCount: Integer;
begin Result := Length(FTerminals); end;
function TERDPCBDesigner.Terminal(Index: Integer): TERDPCBTerminal;
begin
  if (Index < 0) or (Index >= Length(FTerminals)) then raise EArgumentOutOfRangeException.Create('Unknown terminal');
  Result := FTerminals[Index]; Result.Members := Copy(Result.Members);
end;
function TERDPCBDesigner.TerminalIndex(const Selection: string): Integer;
var I: Integer; Member: string;
begin
  Result := -1;
  for I := 0 to High(FTerminals) do
  begin
    if (FTerminals[I].ID = Selection) or (FTerminals[I].SignalPin = Selection) then Exit(I);
    for Member in FTerminals[I].Members do if Member = Selection then Exit(I);
  end;
end;
function TERDPCBDesigner.SelectionPinFor(const Pin: string): string;
var Index: Integer;
begin
  Result := ''; if Pin = '' then Exit;
  if FTemplateEditMode or (ChannelIndex(Pin) >= 0) then Exit(Pin);
  Index := TerminalIndex(Pin);
  if (Index >= 0) and (ChannelIndex(FTerminals[Index].SignalPin) >= 0) then Result := FTerminals[Index].SignalPin;
end;
function TERDPCBDesigner.TerminalRect(Index: Integer): TRect;
var I: Integer; P: TPoint; Point: TPointF; Pad: TERDPCBPad; Member: string; Found: Boolean;
begin
  Result := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt); Found := False;
  for I := 0 to High(FComponents) do if FComponents[I].Reference = FTerminals[Index].ID then
    for Point in FComponents[I].Body do
    begin
      P := ScreenPoint(Point); Result.Left := Min(Result.Left, P.X); Result.Top := Min(Result.Top, P.Y);
      Result.Right := Max(Result.Right, P.X); Result.Bottom := Max(Result.Bottom, P.Y); Found := True;
    end;
  if Found then Exit;
  for Pad in FPads do for Member in FTerminals[Index].Members do if Pad.ID = Member then
    for Point in Pad.Polygon do
    begin
      P := ScreenPoint(Point); Result.Left := Min(Result.Left, P.X); Result.Top := Min(Result.Top, P.Y);
      Result.Right := Max(Result.Right, P.X); Result.Bottom := Max(Result.Bottom, P.Y); Found := True;
    end;
  if Found then InflateRect(Result, ScaleValue(3), ScaleValue(3)) else Result := Rect(0,0,0,0);
end;
procedure TERDPCBDesigner.RemoveTerminal(const ID: string);
var I, J: Integer;
begin
  if not FTemplateEditMode then raise EArgumentException.Create('Enable template authoring first');
  for I := 0 to High(FTerminals) do if FTerminals[I].ID = ID then
  begin
    for J := I to High(FTerminals) - 1 do FTerminals[J] := FTerminals[J + 1];
    SetLength(FTerminals, Length(FTerminals) - 1);
    FBoardJSON := BuildTemplateJSON; FStaticDirty := True; Invalidate;
    if Assigned(FOnChange) then FOnChange(Self); Exit;
  end;
  raise EArgumentException.Create('Unknown terminal group');
end;
procedure TERDPCBDesigner.DefineTerminal(const ID, LabelText, SignalPin: string; const Members: TArray<string>);
var Index, I, J: Integer; Found, HasSignal: Boolean; Member, Other: string; Pad: TERDPCBPad;
begin
  if not FTemplateEditMode then raise EArgumentException.Create('Enable template authoring first');
  if (ID = '') or (ChannelIndex(SignalPin) < 0) or (Length(Members) = 0) then raise EArgumentException.Create('Terminal needs a name and programmable signal');
  Index := -1; for I := 0 to High(FTerminals) do if FTerminals[I].ID = ID then Index := I;
  HasSignal := False;
  for I := 0 to High(Members) do
  begin
    Member := Members[I]; if Member = SignalPin then HasSignal := True;
    for J := 0 to I - 1 do if Members[J] = Member then raise EArgumentException.Create('Repeated terminal member');
    Found := False; for Pad in FPads do if Pad.ID = Member then begin Found := True; Break; end;
    if not Found then raise EArgumentException.Create('Terminal member has no physical pad');
    for J := 0 to High(FTerminals) do if J <> Index then
      for Other in FTerminals[J].Members do if Member = Other then raise EArgumentException.Create('Pin already belongs to another terminal');
  end;
  if not HasSignal then raise EArgumentException.Create('Signal must be a terminal member');
  if Index < 0 then begin Index := Length(FTerminals); SetLength(FTerminals, Index + 1); end;
  FTerminals[Index].ID := ID; FTerminals[Index].LabelText := LabelText; FTerminals[Index].SignalPin := SignalPin;
  FTerminals[Index].Members := Copy(Members); FBoardJSON := BuildTemplateJSON; FStaticDirty := True; Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;
function TERDPCBDesigner.IsSelectablePin(const Pin: string): Boolean;
begin Result := SelectionPinFor(Pin) <> ''; end;
procedure TERDPCBDesigner.Loaded;
var I: Integer;
begin
  inherited;
  FViewBounds := TRectF.Create(0, 0, FMaxX, FMaxY);
  for I := 0 to FScene.Count - 1 do ValidateSceneItem(Self, FScene.Item(I));
  FScene.ResetHistory; SyncScrollBars;
end;
procedure TERDPCBDesigner.ValidateSceneItem(Sender: TObject; const Item: TERDPCBDesignItem);
var Used: TStringList; Context: TERDPCBScene;
  procedure CheckPin(const Pin: string; Capability: TERDPCBPinCapability);
  var Index, I: Integer; Other: TERDPCBDesignItem;
  begin
    if Pin = '' then Exit;
    if Used.IndexOf(Pin) >= 0 then raise EArgumentException.Create('Bus roles need distinct pins');
    Used.Add(Pin); Index := ChannelIndex(Pin);
    if Index < 0 then raise EArgumentException.Create('Only programmable template pins can be assigned');
    if (Capability <> pcNone) and not (Capability in FChannels[Index].Capabilities) then
      raise EArgumentException.Create(Pin + ' does not support ' + ERDPCBPinCapabilityName(Capability));
    if Item.Enabled and (Item.Kind in [piOutputGroup, piUART, piCANBus, piI2C, piSPI]) then
      for I := 0 to Context.Count - 1 do
      begin
        Other := Context.Item(I);
        if (Other.ID <> Item.ID) and Other.Enabled and (Other.Kind in [piOutputGroup, piUART, piCANBus, piI2C, piSPI]) and
          ((Other.Pin = Pin) or (Other.SecondaryPin = Pin) or (Other.ClockPin = Pin) or (Other.SelectPin = Pin)) then
          raise EArgumentException.Create(Pin + ' is already assigned to ' + Other.Name);
      end;
  end;
begin
  if csLoading in ComponentState then Exit;
  Context := FScene; if Sender is TERDPCBScene then Context := TERDPCBScene(Sender);
  Used := TStringList.Create;
  try
    case Item.Kind of
      piOutputGroup: CheckPin(Item.Pin, pcAddressableLED);
      piUART: begin CheckPin(Item.Pin, pcUARTTx); CheckPin(Item.SecondaryPin, pcUARTRx); end;
      piCANBus: begin CheckPin(Item.Pin, pcCANTx); CheckPin(Item.SecondaryPin, pcCANRx); end;
      piI2C: begin CheckPin(Item.Pin, pcI2CSDA); CheckPin(Item.SecondaryPin, pcI2CSCL); end;
      piSPI: begin CheckPin(Item.Pin, pcSPIMOSI); CheckPin(Item.SecondaryPin, pcSPIMISO); CheckPin(Item.ClockPin, pcSPIClock); CheckPin(Item.SelectPin, pcSPISelect); end;
    else CheckPin(Item.Pin, pcNone);
    end;
  finally Used.Free; end;
  if Assigned(FOnValidateDesignItem) then FOnValidateDesignItem(Self, Item);
end;
procedure TERDPCBDesigner.SetTemplateEditMode(Value: Boolean);
begin
  if FTemplateEditMode = Value then Exit;
  CancelPlacement; CancelGesture; ClearConnectionSelection; SetHotConnection('', prPrimary); FTemplateEditMode := Value;
  if not Value and (FSelected <> '') and (ChannelIndex(FSelected) < 0) then SelectedPin := '';
  SetHot(''); FStaticDirty := True; Invalidate;
end;
function TERDPCBDesigner.PinPolicy: string;
var Root: TJSONObject; Items: TJSONArray; Obj: TJSONObject; C: TERDPCBChannel;
begin
  Root := TJSONObject.Create;
  try
    Items := TJSONArray.Create; Root.AddPair('pins', Items);
    for C in FChannels do
    begin Obj := TJSONObject.Create; Items.AddElement(Obj); Obj.AddPair('pin', C.Pin); Obj.AddPair('gpio', C.GPIO); Obj.AddPair('capabilities', ERDPCBPinCapabilitiesText(C.Capabilities)); end;
    Result := Root.ToJSON;
  finally Root.Free; end;
end;
function TERDPCBDesigner.BuildTemplateJSON: string;
var Root: TJSONValue; Obj, Pin: TJSONObject; Items, Members: TJSONArray; C: TERDPCBChannel; T: TERDPCBTerminal; Member: string;
begin
  Root := TJSONObject.ParseJSONValue(FBoardJSON);
  try
    if not (Root is TJSONObject) then raise EConvertError.Create('Load a board before saving a template');
    Obj := TJSONObject(Root); Obj.RemovePair('template_id').Free; Obj.AddPair('template_id', FTemplateID);
    Obj.RemovePair('programmable_pins').Free; Items := TJSONArray.Create; Obj.AddPair('programmable_pins', Items);
    for C in FChannels do
    begin
      Pin := TJSONObject.Create; Items.AddElement(Pin); Pin.AddPair('pin', C.Pin); Pin.AddPair('gpio', C.GPIO);
      Pin.AddPair('route', C.Route); Pin.AddPair('label', C.LabelText); Pin.AddPair('capabilities', ERDPCBPinCapabilitiesText(C.Capabilities));
    end;
    Obj.RemovePair('terminals').Free; Items := TJSONArray.Create; Obj.AddPair('terminals', Items);
    for T in FTerminals do
    begin
      Pin := TJSONObject.Create; Items.AddElement(Pin); Pin.AddPair('id', T.ID); Pin.AddPair('label', T.LabelText); Pin.AddPair('signal_pin', T.SignalPin);
      Members := TJSONArray.Create; Pin.AddPair('members', Members); for Member in T.Members do Members.Add(Member);
    end;
    Result := Obj.ToJSON;
  finally Root.Free; end;
end;
procedure TERDPCBDesigner.SaveTemplate(const FileName: string);
begin TFile.WriteAllText(FileName, BuildTemplateJSON, TEncoding.UTF8); end;
procedure TERDPCBDesigner.SetPinCapability(const Pin: string; Capability: TERDPCBPinCapability; const GPIO, LabelText: string);
begin if Capability = pcNone then SetPinCapabilities(Pin, [], GPIO, LabelText) else SetPinCapabilities(Pin, [Capability], GPIO, LabelText); end;
procedure TERDPCBDesigner.ValidatePinCapabilities(const Pin: string; Capabilities: TERDPCBPinCapabilities);
var I: Integer; Value: TERDPCBDesignItem; Required: TERDPCBPinCapability; Found: Boolean; P: TERDPCBPad;
  procedure CheckRole(const RolePin: string; Capability: TERDPCBPinCapability);
  begin
    if RolePin <> Pin then Exit;
    if (Capabilities = []) or ((Capability <> pcNone) and not (Capability in Capabilities)) then
      raise EArgumentException.Create('Unassign this pin from its designer component before changing capabilities');
  end;
begin
  if not FTemplateEditMode then raise EArgumentException.Create('Enable template authoring first');
  Found := False; for P in FPads do if P.ID = Pin then begin Found := True; Break; end;
  if not Found then raise EArgumentException.Create('Unknown physical pin');
  for I := 0 to FScene.Count - 1 do
  begin
    Value := FScene.Item(I); Required := pcNone;
    case Value.Kind of piOutputGroup: Required := pcAddressableLED; piUART: Required := pcUARTTx;
      piCANBus: Required := pcCANTx; piI2C: Required := pcI2CSDA; piSPI: Required := pcSPIMOSI; end;
    CheckRole(Value.Pin, Required); Required := pcNone;
    case Value.Kind of piUART: Required := pcUARTRx; piCANBus: Required := pcCANRx; piI2C: Required := pcI2CSCL; piSPI: Required := pcSPIMISO; end;
    CheckRole(Value.SecondaryPin, Required);
    if Value.Kind = piSPI then begin CheckRole(Value.ClockPin, pcSPIClock); CheckRole(Value.SelectPin, pcSPISelect); end
    else begin CheckRole(Value.ClockPin, pcNone); CheckRole(Value.SelectPin, pcNone); end;
  end;
end;
procedure TERDPCBDesigner.SetPinCapabilities(const Pin: string; Capabilities: TERDPCBPinCapabilities; const GPIO, LabelText: string);
var Index, I, J: Integer; Found: Boolean; P: TERDPCBPad; Old: TArray<TERDPCBChannel>; C: TERDPCBPinCapability;
begin
  Exclude(Capabilities, pcNone); ValidatePinCapabilities(Pin, Capabilities);
  Exclude(Capabilities, pcNone); Found := False; for P in FPads do if P.ID = Pin then begin Found := True; Break; end;
  if not Found then raise EArgumentException.Create('Unknown physical pin');
  Old := Copy(FChannels); Index := ChannelIndex(Pin);
  try
    if Capabilities = [] then
    begin
      if Index >= 0 then
      begin for J := Index to High(FChannels) - 1 do FChannels[J] := FChannels[J + 1]; SetLength(FChannels, Length(FChannels) - 1); end;
    end
    else
    begin
      if Index < 0 then begin Index := Length(FChannels); SetLength(FChannels, Index + 1); FChannels[Index].Pin := Pin; FChannels[Index].LEDCount := 1; end;
      FChannels[Index].Capabilities := Capabilities;
      for C := Low(TERDPCBPinCapability) to High(TERDPCBPinCapability) do if C in Capabilities then begin FChannels[Index].Capability := C; Break; end;
      FChannels[Index].GPIO := GPIO; FChannels[Index].LabelText := LabelText;
    end;
    for I := 0 to FScene.Count - 1 do ValidateSceneItem(Self, FScene.Item(I));
    if Capabilities = [] then
      for I := High(FTerminals) downto 0 do if FTerminals[I].SignalPin = Pin then
      begin for J := I to High(FTerminals) - 1 do FTerminals[J] := FTerminals[J + 1]; SetLength(FTerminals, Length(FTerminals) - 1); end;
    FBoardJSON := BuildTemplateJSON;
  except FChannels := Old; raise; end;
  FScene.ResetHistory; FStaticDirty := True; Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDPCBDesigner.SetScene(Value: TERDPCBScene);
begin FScene.Assign(Value); end;
procedure TERDPCBDesigner.SceneChanged(Sender: TObject);
begin
  if FRouteCache <> nil then FRouteCache.Clear;
  if FConnectionID <> '' then
  begin
    FStaticDirty := True;
    if not FRewiring and (ConnectionPin = '') then ClearConnectionSelection;
  end;
  SetHotConnection('', prPrimary);
  Invalidate; SyncScrollBars;
  if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDPCBDesigner.SceneSelectionChanged(Sender: TObject);
begin
  if not FSelectingConnection and not FRewiring then ClearConnectionSelection;
  if (FScene.SelectionCount > 0) and (FSelected <> '') then begin FSelected := ''; FStaticDirty := True; end;
  Invalidate;
  if not (csLoading in ComponentState) and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
procedure TERDPCBDesigner.ThemeChanged;
begin FStaticDirty := True; inherited; end;
procedure TERDPCBDesigner.Resize;
begin
  FinishNoteEdit(True); if FRouteCache <> nil then FRouteCache.Clear;
  FStaticDirty := True; inherited; SyncScrollBars;
end;
procedure TERDPCBDesigner.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin if FScrollBars <> Value then begin FScrollBars := Value; SyncScrollBars; Invalidate; end; end;
procedure TERDPCBDesigner.SyncScrollBars;
var Bounds, R: TRect; P, Q: TPoint; I, W, H, T: Integer;
begin
  if (FScrollH = nil) or (FScrollV = nil) or (FScene = nil) or FUpdatingBars then Exit;
  FUpdatingBars := True;
  try
    UpdateTransform; P := ScreenPoint(PointF(0, FMaxY)); Q := ScreenPoint(PointF(FMaxX, 0));
    Bounds := Rect(P.X, P.Y, Q.X, Q.Y);
    for I := 0 to FScene.Count - 1 do
    begin
      R := ItemRect(I); Bounds.Left := Min(Bounds.Left, R.Left); Bounds.Top := Min(Bounds.Top, R.Top);
      Bounds.Right := Max(Bounds.Right, R.Right); Bounds.Bottom := Max(Bounds.Bottom, R.Bottom);
    end;
    W := ClientWidth; H := ClientHeight; T := ScaleValue(12);
    // Resolve the two axes together: one bar reduces the other axis's viewport.
    for I := 0 to 1 do
    begin
      FScrollH.Visible := (FScrollBars in [System.UITypes.TScrollStyle.ssHorizontal, System.UITypes.TScrollStyle.ssBoth]) and
        ((Bounds.Left < 0) or (Bounds.Right > W));
      if FScrollH.Visible then H := Max(0, ClientHeight - T);
      FScrollV.Visible := (FScrollBars in [System.UITypes.TScrollStyle.ssVertical, System.UITypes.TScrollStyle.ssBoth]) and
        ((Bounds.Top < 0) or (Bounds.Bottom > H));
      if FScrollV.Visible then W := Max(0, ClientWidth - T);
    end;
    Bounds.Left := Min(0, Bounds.Left); Bounds.Top := Min(0, Bounds.Top);
    Bounds.Right := Max(W, Bounds.Right); Bounds.Bottom := Max(H, Bounds.Bottom);
    FScrollH.SetBounds(0, ClientHeight - T, Max(0, W), T);
    FScrollV.SetBounds(ClientWidth - T, 0, T, Max(0, H));
    FBarX := -Bounds.Left; FBarY := -Bounds.Top;
    FScrollH.Min := 0; FScrollH.Max := Max(0, Bounds.Width - 1); FScrollH.PageSize := W;
    FScrollH.Position := FBarX; FScrollH.SmallChange := ScaleValue(16);
    FScrollH.Enabled := Bounds.Width > W;
    FScrollV.Min := 0; FScrollV.Max := Max(0, Bounds.Height - 1); FScrollV.PageSize := H;
    FScrollV.Position := FBarY; FScrollV.SmallChange := ScaleValue(16);
    FScrollV.Enabled := Bounds.Height > H;
    FScrollH.BringToFront; FScrollV.BringToFront;
  finally FUpdatingBars := False; end;
end;
procedure TERDPCBDesigner.ScrollChanged(Sender: TObject);
begin
  if FUpdatingBars then Exit;
  if Sender = FScrollH then PanBy(FBarX - FScrollH.Position, 0)
  else PanBy(0, FBarY - FScrollV.Position);
end;
procedure TERDPCBDesigner.SetGridSize(Value: Double);
begin
  if IsNan(Value) or IsInfinite(Value) or (Value < 0.1) or (Value > 100) then
    raise EArgumentException.Create('Grid size must be 0.1..100 mm');
  FGridSize := Value; FStaticDirty := True; Invalidate;
end;
procedure TERDPCBDesigner.SetSnapToGrid(Value: Boolean);
begin FSnapToGrid := Value; end;
procedure TERDPCBDesigner.SetShowGrid(Value: Boolean);
begin FShowGrid := Value; FStaticDirty := True; Invalidate; end;
function TERDPCBDesigner.Snap(Value: Double): Double;
begin if FSnapToGrid then Result := Round(Value / FGridSize) * FGridSize else Result := Value; end;
function TERDPCBDesigner.AddDesignItem(Kind: TERDPCBItemKind; const Position: TPointF): Integer;
begin Result := FScene.Add(Kind, PointF(Snap(Position.X), Snap(Position.Y))); end;
function TERDPCBDesigner.SupportsPinRole(ItemIndex: Integer; Role: TERDPCBPinRole): Boolean;
var Kind: TERDPCBItemKind;
begin
  Kind := FScene.Item(ItemIndex).Kind;
  case Role of
    prPrimary: Result := Kind <> piNote;
    prSecondary: Result := Kind in [piUART, piCANBus, piI2C, piSPI];
    prClock, prSelect: Result := Kind = piSPI;
  else Result := False;
  end;
end;
function TERDPCBDesigner.ItemWithPin(Index: Integer; Role: TERDPCBPinRole; const Pin: string): TERDPCBDesignItem;
begin
  Result := FScene.Item(Index);
  if not SupportsPinRole(Index, Role) then
    raise EArgumentException.Create('This component does not support that pin role');
  case Role of
    prPrimary: Result.Pin := Pin; prSecondary: Result.SecondaryPin := Pin;
    prClock: Result.ClockPin := Pin; prSelect: Result.SelectPin := Pin;
  end;
end;
procedure TERDPCBDesigner.ValidatePinAssignment(ItemIndex: Integer; Role: TERDPCBPinRole; const Pin: string);
var Candidate, Other: TERDPCBDesignItem; I: Integer;
begin
  Candidate := ItemWithPin(ItemIndex, Role, Pin);
  FScene.ValidateItem(ItemIndex, Candidate);
  if Pin = '' then Exit;
  for I := 0 to FScene.Count - 1 do if I <> ItemIndex then
  begin
    Other := FScene.Item(I);
    if (Other.Pin = Pin) or (Other.SecondaryPin = Pin) or (Other.ClockPin = Pin) or (Other.SelectPin = Pin) then
      raise EArgumentException.Create(Pin + ' is already assigned to ' + Other.Name);
  end;
end;
function TERDPCBDesigner.GetAssignablePins(ItemIndex: Integer; Role: TERDPCBPinRole): TArray<string>;
var I, N: Integer;
begin
  Result := nil;
  // Unsupported roles have no choices, including notes, rather than a clear-only entry.
  if not SupportsPinRole(ItemIndex, Role) then Exit;
  for I := 0 to High(FChannels) do
    try
      ValidatePinAssignment(ItemIndex, Role, FChannels[I].Pin);
      N := Length(Result); SetLength(Result, N + 1); Result[N] := FChannels[I].Pin;
    except on E: EArgumentException do Continue; end;
end;
procedure TERDPCBDesigner.AssignPin(ItemIndex: Integer; Role: TERDPCBPinRole; const Pin: string);
var Candidate: TERDPCBDesignItem;
begin
  ValidatePinAssignment(ItemIndex, Role, Pin); Candidate := ItemWithPin(ItemIndex, Role, Pin);
  case Role of
    prPrimary: if FScene.Item(ItemIndex).Pin = Pin then Exit;
    prSecondary: if FScene.Item(ItemIndex).SecondaryPin = Pin then Exit;
    prClock: if FScene.Item(ItemIndex).ClockPin = Pin then Exit;
    prSelect: if FScene.Item(ItemIndex).SelectPin = Pin then Exit;
  end;
  FScene.UpdateItem(ItemIndex, Candidate);
end;
procedure TERDPCBDesigner.AssignSelectedItemsToPin(const Pin: string);
var I: Integer; Found: Boolean; Value: TERDPCBDesignItem;
begin
  Found := (Pin = '') or (ChannelIndex(Pin) >= 0);
  if not Found then raise EArgumentException.Create('Unknown physical pin');
  FScene.BeginEdit;
  try
    for I := 0 to FScene.Count - 1 do if FScene.Item(I).Selected then
    begin Value := FScene.Item(I); Value.Pin := Pin; FScene.UpdateItem(I, Value); end;
  except FScene.CancelEdit; raise; end;
  FScene.EndEdit;
end;
procedure TERDPCBDesigner.CopySelected;
begin if FScene.SelectionCount > 0 then Clipboard.AsText := FScene.ToJSON(True); end;
procedure TERDPCBDesigner.Paste;
var Text: string; Root: TJSONValue;
begin
  if not Clipboard.HasFormat(CF_UNICODETEXT) then Exit;
  Text := Clipboard.AsText; Root := TJSONObject.ParseJSONValue(Text);
  try
    if not (Root is TJSONObject) then Exit;
    if Root.GetValue<string>('format', '') <> 'erd-pcb-scene' then Exit;
  finally Root.Free; end;
  FScene.PasteJSON(Text, FGridSize);
end;
procedure TERDPCBDesigner.Undo;
begin FinishNoteEdit(True); CancelGesture; ClearConnectionSelection; FScene.Undo; end;
procedure TERDPCBDesigner.Redo;
begin FinishNoteEdit(True); CancelGesture; ClearConnectionSelection; FScene.Redo; end;
procedure TERDPCBDesigner.LoadBoard(const FileName: string);
begin
  LoadBoardJSON(TFile.ReadAllText(FileName, TEncoding.UTF8));
  FBoardFileName := FileName;
end;
procedure TERDPCBDesigner.LoadBoardJSON(const Data: string);
var Root: TJSONValue; Obj: TJSONObject; A: TJSONArray; I, J: Integer;
    Pads: TArray<TERDPCBPad>; Channels: TArray<TERDPCBChannel>;
    Components: TArray<TERDPCBComponent>; Terminals: TArray<TERDPCBTerminal>; MemberArray: TJSONArray; Paths, Outline: TArray<TArray<TPointF>>;
    Hash, TemplateID, TemplateName, Member: string; MaxX, MaxY: Double; PinData: TJSONValue; ExplicitPins, Found, HasSignal: Boolean; K, L, M: Integer; Capability: TERDPCBPinCapability;
begin
  CancelPlacement;
  Root := TJSONObject.ParseJSONValue(Data);
  try
    if not (Root is TJSONObject) then raise EConvertError.Create('Invalid board package');
    if Root.GetValue<Integer>('schema') <> 1 then raise EConvertError.Create('Unsupported board schema');
    if (Root.GetValue<string>('units') <> 'mm') or (Root.GetValue<string>('y_axis') <> 'up') then
      raise EConvertError.Create('Unsupported board coordinate system');
    Hash := Root.GetValue<string>('source_sha256');
    TemplateID := Root.GetValue<string>('template_id', Hash); TemplateName := Root.GetValue<string>('name', 'PCB template');
    A := Root.GetValue<TJSONArray>('pads'); SetLength(Pads, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Pads[I].ID := Obj.GetValue<string>('id'); Pads[I].Net := Obj.GetValue<string>('net');
      Pads[I].Reference := Obj.GetValue<string>('reference'); Pads[I].Number := Obj.GetValue<string>('number');
      Pads[I].Position := PointValue(Obj.GetValue('position'));
      Pads[I].Polygon := PolygonValue(Obj.GetValue('polygon'));
      Pads[I].Drill := PolygonValue(Obj.GetValue('drill'));
    end;
    PinData := TJSONObject(Root).GetValue('programmable_pins'); ExplicitPins := PinData <> nil;
    if not ExplicitPins then PinData := TJSONObject(Root).GetValue('channels');
    if (PinData <> nil) and not (PinData is TJSONArray) then raise EConvertError.Create('Invalid programmable pins');
    if PinData = nil then SetLength(Channels, 0) else
    begin
    A := TJSONArray(PinData); SetLength(Channels, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Channels[I].Pin := Obj.GetValue<string>('pin'); Channels[I].GPIO := Obj.GetValue<string>('gpio', '');
      Channels[I].Route := Obj.GetValue<string>('route', ''); Channels[I].LEDCount := 1;
      Channels[I].LabelText := Obj.GetValue<string>('label', Channels[I].Pin);
      Channels[I].Capability := pcAddressableLED;
      Channels[I].Capabilities := [pcAddressableLED];
      if ExplicitPins then
      begin
        Channels[I].Capabilities := ERDPCBParsePinCapabilities(Obj.GetValue<string>('capabilities', Obj.GetValue<string>('capability', 'none')));
        for Capability := Low(TERDPCBPinCapability) to High(TERDPCBPinCapability) do
          if Capability in Channels[I].Capabilities then begin Channels[I].Capability := Capability; Break; end;
        if Channels[I].Capabilities = [] then Channels[I].Capability := pcNone;
      end;
      if Channels[I].Capability = pcNone then raise EConvertError.Create('Unprogrammable pins must not be listed');
      Found := False; for J := 0 to High(Pads) do if Pads[J].ID = Channels[I].Pin then begin Found := True; Break; end;
      if not Found then raise EConvertError.Create('Programmable pin has no physical pad');
      for J := 0 to I - 1 do if Channels[J].Pin = Channels[I].Pin then raise EConvertError.Create('Repeated programmable pin');
    end;
    end;
    A := Root.GetValue<TJSONArray>('components'); SetLength(Components, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Components[I].Reference := Obj.GetValue<string>('reference');
      Components[I].Device := Obj.GetValue<string>('device'); Components[I].Role := Obj.GetValue<string>('role', 'ic');
      if Obj.GetValue('body') <> nil then Components[I].Body := PolygonValue(Obj.GetValue('body'));
      Components[I].Position := PointValue(Obj.GetValue('position'));
    end;
    PinData := TJSONObject(Root).GetValue('terminals');
    if PinData <> nil then
    begin
      if not (PinData is TJSONArray) then raise EConvertError.Create('Invalid terminal groups');
      A := TJSONArray(PinData); SetLength(Terminals, A.Count);
      for I := 0 to A.Count - 1 do
      begin
        if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Invalid terminal group');
        Obj := TJSONObject(A.Items[I]); Terminals[I].ID := Obj.GetValue<string>('id');
        Terminals[I].LabelText := Obj.GetValue<string>('label', Terminals[I].ID); Terminals[I].SignalPin := Obj.GetValue<string>('signal_pin');
        MemberArray := Obj.GetValue<TJSONArray>('members'); SetLength(Terminals[I].Members, MemberArray.Count);
        if (Terminals[I].ID = '') or (MemberArray.Count = 0) then raise EConvertError.Create('Empty terminal group');
        Found := False; for K := 0 to High(Channels) do if Channels[K].Pin = Terminals[I].SignalPin then Found := True;
        if not Found then raise EConvertError.Create('Terminal signal is not programmable');
        for K := 0 to I - 1 do if Terminals[K].ID = Terminals[I].ID then raise EConvertError.Create('Repeated terminal identity');
        HasSignal := False;
        for J := 0 to MemberArray.Count - 1 do
        begin
          if not (MemberArray.Items[J] is TJSONString) then raise EConvertError.Create('Invalid terminal member');
          Member := MemberArray.Items[J].Value; Terminals[I].Members[J] := Member;
          if Member = Terminals[I].SignalPin then HasSignal := True;
          Found := False; for K := 0 to High(Pads) do if Pads[K].ID = Member then Found := True;
          if not Found then raise EConvertError.Create('Terminal member has no physical pad');
          for K := 0 to J - 1 do if Terminals[I].Members[K] = Member then raise EConvertError.Create('Repeated terminal member');
          for L := 0 to I - 1 do for M := 0 to High(Terminals[L].Members) do
            if Terminals[L].Members[M] = Member then raise EConvertError.Create('Terminal groups overlap');
        end;
        if not HasSignal then raise EConvertError.Create('Terminal must include its signal pin');
      end;
    end;
    Paths := PathsValue(Root.GetValue<TJSONArray>('silkscreen')); Outline := PathsValue(Root.GetValue<TJSONArray>('outline'));
    MaxX := 0; MaxY := 0;
    for I := 0 to High(Outline) do for J := 0 to High(Outline[I]) do
    begin MaxX := Max(MaxX, Outline[I][J].X); MaxY := Max(MaxY, Outline[I][J].Y); end;
    if (MaxX <= 0) or (MaxY <= 0) then raise EConvertError.Create('Board outline is empty');
    FPads := Pads; FChannels := Channels; FComponents := Components;
    FPaths := Paths; FOutline := Outline; FTerminals := Terminals; FBoardHash := Hash; FMaxX := MaxX; FMaxY := MaxY;
    FScene.LoadJSON('{"format":"erd-pcb-scene","version":1,"items":[]}'); FScene.ResetHistory;
    FTemplateID := TemplateID; FTemplateName := TemplateName; FBoardJSON := Data;
    FSelected := ''; SetHot('');
    if not (csLoading in ComponentState) then FitBoard else Invalidate;
    if not (csLoading in ComponentState) and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  finally Root.Free; end;
end;
procedure TERDPCBDesigner.FitBoard;
var I: Integer; Value: TERDPCBDesignItem;
begin
  FViewBounds := TRectF.Create(0, 0, FMaxX, FMaxY);
  if FScene <> nil then for I := 0 to FScene.Count - 1 do
  begin
    Value := FScene.Item(I); FViewBounds.Left := Min(FViewBounds.Left, Value.X); FViewBounds.Top := Min(FViewBounds.Top, Value.Y);
    FViewBounds.Right := Max(FViewBounds.Right, Value.X + Value.Width); FViewBounds.Bottom := Max(FViewBounds.Bottom, Value.Y + Value.Height);
  end;
  FZoom := 1; FPanX := 0; FPanY := 0; ViewChanged;
end;
procedure TERDPCBDesigner.DefineProperties(Filer: TFiler);
begin
  inherited;
  if Filer.Ancestor is TERDPCBDesigner then
    Filer.DefineBinaryProperty('BoardData', ReadBoardData, WriteBoardData, FBoardJSON <> TERDPCBDesigner(Filer.Ancestor).FBoardJSON)
  else Filer.DefineBinaryProperty('BoardData', ReadBoardData, WriteBoardData, FBoardJSON <> '');
end;
procedure TERDPCBDesigner.ReadBoardData(Stream: TStream);
var Bytes: TBytes; Count: Int64;
begin
  Count := Stream.Size - Stream.Position;
  if Count = 0 then begin ClearBoard; Exit; end;
  if (Count < 0) or (Count > 16 * 1024 * 1024) then raise EReadError.Create('Invalid embedded board size');
  SetLength(Bytes, Integer(Count)); Stream.ReadBuffer(Bytes[0], Length(Bytes));
  LoadBoardJSON(TEncoding.UTF8.GetString(Bytes)); FBoardFileName := '';
end;
procedure TERDPCBDesigner.WriteBoardData(Stream: TStream);
var Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(FBoardJSON);
  if Length(Bytes) > 0 then Stream.WriteBuffer(Bytes[0], Length(Bytes));
end;
procedure TERDPCBDesigner.SetBoardFileName(const Value: TFileName);
begin
  if Value = '' then ClearBoard else LoadBoard(Value);
end;
procedure TERDPCBDesigner.ClearBoard;
begin
  CancelPlacement; FinishNoteEdit(False);
  FPads := nil; FChannels := nil; FComponents := nil; FTerminals := nil; FPaths := nil; FOutline := nil;
  FScene.LoadJSON('{"format":"erd-pcb-scene","version":1,"items":[]}'); FScene.ResetHistory;
  FTemplateID := ''; FTemplateName := '';
  FBoardJSON := ''; FBoardHash := ''; FBoardFileName := ''; FMaxX := 0; FMaxY := 0;
  FSelected := ''; SetHot('');
  if not (csLoading in ComponentState) then FitBoard else Invalidate;
  if not (csLoading in ComponentState) and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
function TERDPCBDesigner.IsZoomStored: Boolean;
begin Result := Abs(FZoom - 1) > 0.000001; end;
procedure TERDPCBDesigner.ViewChanged;
begin
  FinishNoteEdit(True); if FRouteCache <> nil then FRouteCache.Clear;
  FStaticDirty := True; SyncScrollBars; Invalidate;
  if not (csLoading in ComponentState) and Assigned(FOnViewChange) then FOnViewChange(Self);
end;
procedure TERDPCBDesigner.SetZoom(Value: Double);
begin ZoomAt(Value, Point(ClientWidth div 2, ClientHeight div 2)); end;
procedure TERDPCBDesigner.ZoomAt(Value: Double; const ClientPoint: TPoint);
var Ratio: Double;
begin
  if IsNan(Value) or IsInfinite(Value) then raise EArgumentException.Create('Zoom must be finite');
  Value := EnsureRange(Value, 0.3, 20.0); if SameValue(Value, FZoom) then Exit;
  Ratio := Value / FZoom; FZoom := Value;
  FPanX := ClientPoint.X - ClientWidth / 2 - (ClientPoint.X - ClientWidth / 2 - FPanX) * Ratio;
  FPanY := ClientPoint.Y - ClientHeight / 2 - (ClientPoint.Y - ClientHeight / 2 - FPanY) * Ratio;
  ViewChanged;
end;
procedure TERDPCBDesigner.PanBy(DX, DY: Integer);
begin FPanX := FPanX + DX; FPanY := FPanY + DY; ViewChanged; end;
function TERDPCBDesigner.BoardToClient(const Point: TPointF): TPoint;
begin UpdateTransform; Result := ScreenPoint(Point); end;
function TERDPCBDesigner.ClientToBoard(const Point: TPoint): TPointF;
begin
  UpdateTransform;
  Result := PointF((Point.X - FScreenX) / FScreenScale, (FScreenY - Point.Y) / FScreenScale);
end;
function TERDPCBDesigner.HitTestPin(const ClientPoint: TPoint): string;
begin Result := PadAt(ClientPoint.X, ClientPoint.Y); end;
procedure TERDPCBDesigner.SetBoardColor(Value: TColor);
begin if FBoardColor <> Value then begin FBoardColor := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetPadColor(Value: TColor);
begin if FPadColor <> Value then begin FPadColor := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetOutlineColor(Value: TColor);
begin if FOutlineColor <> Value then begin FOutlineColor := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetSelectionColor(Value: TColor);
begin if FSelectionColor <> Value then begin FSelectionColor := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetShowBodies(Value: Boolean);
begin FShowBodies := Value; FStaticDirty := True; Invalidate; end;
procedure TERDPCBDesigner.SetShowPads(Value: Boolean);
begin if FShowPads <> Value then begin FShowPads := Value; SetHot(''); FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetShowLabels(Value: Boolean);
begin if FShowLabels <> Value then begin FShowLabels := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetShowComponentOutlines(Value: Boolean);
begin if FShowComponentOutlines <> Value then begin FShowComponentOutlines := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetShowDrills(Value: Boolean);
begin if FShowDrills <> Value then begin FShowDrills := Value; FStaticDirty := True; Invalidate; end; end;
procedure TERDPCBDesigner.SetHot(const Value: string);
begin
  if FHot = Value then Exit;
  FHot := Value; FStaticDirty := True; Invalidate;
  Application.CancelHint;
  if Assigned(FOnHotPinChange) then FOnHotPinChange(Self);
end;
procedure TERDPCBDesigner.CMMouseLeave(var Message: TMessage);
begin
  FPlacementPreview := False; Invalidate; SetHot(''); SetHotConnection('', prPrimary);
  if not FDragging then Cursor := crDefault; inherited;
end;
procedure TERDPCBDesigner.WMLButtonUp(var Message: TWMLButtonUp);
begin
  // VCL may release capture before dispatching the virtual MouseUp method.
  FCompletingGesture := True;
  try
    inherited;
  finally
    FCompletingGesture := False;
  end;
end;
procedure TERDPCBDesigner.WMCaptureChanged(var Message: TMessage);
begin
  if not FCompletingGesture then
  begin FDragging := False; CancelGesture; Cursor := crDefault; end;
  inherited;
end;
procedure TERDPCBDesigner.WMGetDlgCode(var Message: TWMGetDlgCode);
begin Message.Result := DLGC_WANTARROWS; end;
procedure TERDPCBDesigner.CMHintShow(var Message: TCMHintShow);
var Pad: TERDPCBPad; P: TPoint;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then Exit;
  if FHotConnectionID <> '' then
  begin
    Message.HintInfo^.HintStr := ConnectionHint(FHotConnectionID, FHotConnectionRole);
    P := Message.HintInfo^.CursorPos;
    Message.HintInfo^.CursorRect := Rect(P.X-ScaleValue(4),P.Y-ScaleValue(4),P.X+ScaleValue(4),P.Y+ScaleValue(4));
    Exit;
  end;
  if FHot = '' then Exit;
  for Pad in FPads do if Pad.ID = FHot then
  begin
    Message.HintInfo^.HintStr := Pad.ID + ' - ' + Pad.Net;
    if TerminalIndex(Pad.ID) >= 0 then Message.HintInfo^.HintStr := FTerminals[TerminalIndex(Pad.ID)].LabelText + ' - power / data / GND';
    P := Message.HintInfo^.CursorPos;
    Message.HintInfo^.CursorRect := Rect(P.X - ScaleValue(7), P.Y - ScaleValue(7), P.X + ScaleValue(7), P.Y + ScaleValue(7));
    Break;
  end;
end;
function TERDPCBDesigner.ViewScale: Double;
begin Result := FZoom * Max(0.01, Min((ClientWidth - ScaleValue(40)) / Max(1, FViewBounds.Width),
  (ClientHeight - ScaleValue(40)) / Max(1, FViewBounds.Height))); end;
procedure TERDPCBDesigner.UpdateTransform;
begin
  // Compute once per paint/hit test, rather than querying window bounds per vertex.
  FScreenScale := ViewScale;
  FScreenX := (ClientWidth - (FViewBounds.Left + FViewBounds.Right) * FScreenScale) / 2 + FPanX;
  FScreenY := (ClientHeight + (FViewBounds.Top + FViewBounds.Bottom) * FScreenScale) / 2 + FPanY;
end;
function TERDPCBDesigner.ScreenPoint(const Point: TPointF): TPoint;
begin
  Result := System.Types.Point(Round(FScreenX + Point.X * FScreenScale),
    Round(FScreenY - Point.Y * FScreenScale));
end;
procedure TERDPCBDesigner.PaintBoardLayer(ACanvas: TCanvas);
var I, J: Integer; P: TPoint; Poly: TArray<TPoint>; R: TRect; S: Double; Accent, BoardInk, PadInk, OutlineInk: TColor;
  procedure DrawPaths(const Paths: TArray<TArray<TPointF>>; Ink: TColor);
  var A, B: Integer;
  begin
    ACanvas.Pen.Color := Ink; ACanvas.Pen.Width := 1;
    for A := 0 to High(Paths) do
    begin
      SetLength(Poly, Length(Paths[A]));
      for B := 0 to High(Poly) do Poly[B] := ScreenPoint(Paths[A][B]);
      if Length(Poly) > 1 then ACanvas.Polyline(Poly);
    end;
  end;
begin
  ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(ClientRect);
  if Length(FPads) = 0 then
  begin
    ACanvas.Font.Assign(Font); ACanvas.Font.Color := Palette.Subtle; ACanvas.Brush.Style := bsClear;
    ACanvas.TextOut(ScaleValue(12), ScaleValue(12), 'PCB designer - set BoardFileName or call LoadBoard');
    PaintGrid(ACanvas); Exit;
  end;
  BoardInk := FBoardColor; if BoardInk = clDefault then BoardInk := Palette.GaugeFace;
  PadInk := FPadColor; if PadInk = clDefault then PadInk := Palette.Subtle;
  OutlineInk := FOutlineColor; if OutlineInk = clDefault then OutlineInk := Palette.NeutralDark;
  Accent := FSelectionColor; if Accent = clDefault then Accent := Palette.Accent;
  UpdateTransform; S := FScreenScale;
  P := ScreenPoint(PointF(0, FMaxY)); R := Rect(P.X, P.Y, P.X + Round(FMaxX * S), P.Y + Round(FMaxY * S));
  ACanvas.Brush.Color := BoardInk; ACanvas.Pen.Color := Palette.NeutralLight;
  ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Round(4 * S), Round(4 * S));
  PaintGrid(ACanvas);
  if FShowComponentOutlines then DrawPaths(FPaths, OutlineInk); DrawPaths(FOutline, Palette.NeutralDark);
  if FShowPads then for I := 0 to High(FPads) do
  begin
    SetLength(Poly, Length(FPads[I].Polygon));
    for J := 0 to High(Poly) do Poly[J] := ScreenPoint(FPads[I].Polygon[J]);
    ACanvas.Pen.Width := 1; ACanvas.Pen.Color := Palette.NeutralDark;
    ACanvas.Brush.Color := PadInk;
    if FPads[I].ID = FHot then ACanvas.Brush.Color := Accent;
    if (FPads[I].ID = FSelected) or ((FConnectionID <> '') and (FPads[I].ID = ConnectionPin)) then
    begin ACanvas.Brush.Color := Accent; ACanvas.Pen.Color := Palette.ForegroundText; ACanvas.Pen.Width := ScaleValue(2); end;
    if Length(Poly) > 2 then ACanvas.Polygon(Poly);
    if not FShowDrills then Continue;
    SetLength(Poly, Length(FPads[I].Drill));
    for J := 0 to High(Poly) do Poly[J] := ScreenPoint(FPads[I].Drill[J]);
    ACanvas.Brush.Color := Palette.Background; ACanvas.Pen.Color := Palette.Background; ACanvas.Pen.Width := 1;
    if Length(Poly) > 2 then ACanvas.Polygon(Poly);
  end;
  if FShowBodies then PaintBodies(ACanvas);
  ACanvas.Font.Name := 'Segoe UI'; ACanvas.Font.Height := -ScaleValue(10);
  ACanvas.Font.Color := Palette.ForegroundText; ACanvas.Brush.Style := bsClear;
  if FShowLabels then for I := 0 to High(FComponents) do
    if (S >= ScaleValue(8)) or (ChannelIndex(FComponents[I].Reference + '.2') >= 0) or (FComponents[I].Reference = 'U6') then
    begin
      P := ScreenPoint(FComponents[I].Position);
      ACanvas.TextOut(P.X + ScaleValue(3), P.Y + ScaleValue(5), FComponents[I].Reference);
    end;
end;
function TERDPCBDesigner.PadAt(X, Y: Integer): string;
var I, J, K: Integer; P, Q, Center: TPoint; Inside: Boolean; Distance, Best, DX, DY, T: Double;
begin
  UpdateTransform;
  Result := ''; if not FShowPads then Exit;
  Best := Sqr(ScaleValue(7));
  for I := 0 to High(FPads) do
  begin
    if not IsSelectablePin(FPads[I].ID) then Continue;
    Inside := False; K := High(FPads[I].Polygon); Distance := MaxDouble;
    for J := 0 to High(FPads[I].Polygon) do
    begin
      P := ScreenPoint(FPads[I].Polygon[J]); Q := ScreenPoint(FPads[I].Polygon[K]);
      if ((P.Y > Y) <> (Q.Y > Y)) then
        if X < (Q.X - P.X) * (Y - P.Y) / (Q.Y - P.Y) + P.X then Inside := not Inside;
      DX := Q.X - P.X; DY := Q.Y - P.Y;
      T := EnsureRange(((X - P.X) * DX + (Y - P.Y) * DY) / Max(1, DX * DX + DY * DY), 0.0, 1.0);
      Distance := Min(Distance, Sqr(X - P.X - T * DX) + Sqr(Y - P.Y - T * DY)); K := J;
    end;
    if Inside then Distance := 0;
    Center := ScreenPoint(FPads[I].Position);
    // A tiny centre-distance tie break prefers the closest overlapping pad.
    Distance := Distance + 0.000001 * (Sqr(X - Center.X) + Sqr(Y - Center.Y));
    if Distance < Best then begin Best := Distance; Result := SelectionPinFor(FPads[I].ID); end;
  end;
  if (Result = '') and not FTemplateEditMode then
    for I := 0 to High(FTerminals) do
      if (ChannelIndex(FTerminals[I].SignalPin) >= 0) and PtInRect(TerminalRect(I), Point(X, Y)) then Exit(FTerminals[I].SignalPin);
end;
procedure TERDPCBDesigner.SetSelected(const Value: string);
var Found: Boolean; P: TERDPCBPad; Canonical: string;
begin
  Canonical := SelectionPinFor(Value);
  if (Value <> '') and (Canonical = '') then raise EArgumentException.Create('Pin is not programmable');
  if Canonical <> '' then ClearConnectionSelection;
  if FSelected = Canonical then Exit;
  Found := (Value = '');
  if Value <> '' then for P in FPads do if (P.ID = Canonical) and IsSelectablePin(Canonical) then begin Found := True; Break; end;
  if not Found then raise EArgumentException.Create('Unknown PCB pin');
  if Value <> '' then FScene.ClearSelection;
  FSelected := Canonical; FStaticDirty := True; Invalidate;
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
function TERDPCBDesigner.PadInfo(const Pin: string): TERDPCBPad;
var P: TERDPCBPad;
begin
  Result := Default(TERDPCBPad);
  for P in FPads do if P.ID = Pin then
  begin Result := P; Result.Polygon := Copy(P.Polygon); Result.Drill := Copy(P.Drill); Exit; end;
end;
function TERDPCBDesigner.SelectedPad: TERDPCBPad;
begin Result := PadInfo(FSelected); end;
function TERDPCBDesigner.ChannelCount: Integer;
begin Result := Length(FChannels); end;
function TERDPCBDesigner.ChannelIndex(const Pin: string): Integer;
var I: Integer;
begin Result := -1; for I := 0 to High(FChannels) do if FChannels[I].Pin = Pin then Exit(I); end;
function TERDPCBDesigner.Channel(Index: Integer): TERDPCBChannel;
begin Result := FChannels[Index]; end;
procedure TERDPCBDesigner.SetBinding(Index: Integer; const Name, Action: string; LEDCount: Integer);
var I: Integer;
begin
  if (Index < 0) or (Index >= Length(FChannels)) then raise EArgumentOutOfRangeException.Create('Unknown channel');
  if (LEDCount < 1) or (LEDCount > 65535) then raise EArgumentException.Create('LED count must be 1..65535');
  if (Action <> '') and (Action <> 'showPick') then raise EArgumentException.Create('Unsupported action');
  for I := 0 to High(FChannels) do
    if (I <> Index) and (Name <> '') and SameText(Name, FChannels[I].Name) then
      raise EArgumentException.Create('Channel names must be unique');
  FChannels[Index].Name := Name; FChannels[Index].Action := Action; FChannels[Index].LEDCount := LEDCount;
  Invalidate;
end;
function TERDPCBDesigner.ItemRect(Index: Integer): TRect;
var Value: TERDPCBDesignItem; P, Q: TPoint;
begin
  Value := FScene.Item(Index);
  if Value.Kind = piNote then
  begin
    P := ScreenPoint(PointF(Value.X, Value.Y + Value.Height));
    Q := ScreenPoint(PointF(Value.X + Value.Width, Value.Y));
    Exit(Rect(P.X, P.Y, Q.X, Q.Y));
  end;
  P := ScreenPoint(PointF(Value.X, Value.Y));
  // Nonvisual design glyphs have a fixed DPI-scaled size, independent of PCB zoom.
  Result := Rect(P.X, P.Y - ScaleValue(32), P.X + ScaleValue(32), P.Y);
end;
function TERDPCBDesigner.ItemAt(const P: TPoint): Integer;
var I: Integer;
begin
  UpdateTransform; Result := -1;
  for I := FScene.Count - 1 downto 0 do if PtInRect(ItemRect(I), P) then Exit(I);
end;
function TERDPCBDesigner.GripRect(const R: TRect; Index: Integer): TRect;
var X, Y, Size: Integer;
begin
  case Index of 0, 6, 7: X := R.Left; 1, 5: X := (R.Left + R.Right) div 2; else X := R.Right; end;
  case Index of 0, 1, 2: Y := R.Top; 3, 7: Y := (R.Top + R.Bottom) div 2; else Y := R.Bottom; end;
  Size := ScaleValue(3); Result := Rect(X - Size, Y - Size, X + Size + 1, Y + Size + 1);
end;
function TERDPCBDesigner.GripAt(const P: TPoint): Integer;
var Index, I: Integer; R: TRect; Value: TERDPCBDesignItem;
begin
  Result := -1; if FScene.SelectionCount <> 1 then Exit;
  Index := FScene.PrimaryIndex; if Index < 0 then Exit;
  Value := FScene.Item(Index); if (Value.Kind <> piNote) or Value.Locked then Exit;
  UpdateTransform; R := ItemRect(Index);
  for I := 0 to 7 do if PtInRect(GripRect(R, I), P) then Exit(I);
end;
procedure TERDPCBDesigner.PaintBodies(ACanvas: TCanvas);
var I, J, K: Integer; Value: TERDPCBComponent; Poly: TArray<TPoint>; R, Inner: TRect; P: TPoint;
    Saved, Radius, TerminalID: Integer; Pad: TERDPCBPad;
  procedure TintSelectedTerminal;
  var Tint: TBitmap; Blend: TBlendFunction; ClipState, N: Integer; Bounds: TRect;
  begin
    Bounds := Rect(Poly[0].X, Poly[0].Y, Poly[0].X, Poly[0].Y);
    for N := 1 to High(Poly) do
    begin
      Bounds.Left := Min(Bounds.Left, Poly[N].X); Bounds.Top := Min(Bounds.Top, Poly[N].Y);
      Bounds.Right := Max(Bounds.Right, Poly[N].X); Bounds.Bottom := Max(Bounds.Bottom, Poly[N].Y);
    end;
    if (Bounds.Width <= 0) or (Bounds.Height <= 0) then Exit;
    Tint := TBitmap.Create;
    try
      Tint.PixelFormat := pf32bit; Tint.SetSize(1, 1);
      Tint.Canvas.Brush.Color := ACanvas.Pen.Color; Tint.Canvas.FillRect(Rect(0, 0, 1, 1));
      Blend.BlendOp := AC_SRC_OVER; Blend.BlendFlags := 0;
      Blend.SourceConstantAlpha := 56; Blend.AlphaFormat := 0;
      ClipState := SaveDC(ACanvas.Handle);
      try
        // A GDI path preserves the supersampling world transform when clipping.
        BeginPath(ACanvas.Handle); ACanvas.Polygon(Poly); EndPath(ACanvas.Handle);
        SelectClipPath(ACanvas.Handle, RGN_AND);
        Winapi.Windows.AlphaBlend(ACanvas.Handle, Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height,
          Tint.Canvas.Handle, 0, 0, 1, 1, Blend);
      finally RestoreDC(ACanvas.Handle, ClipState); end;
    finally Tint.Free; end;
  end;
begin
  UpdateTransform;
  for I := 0 to High(FComponents) do
  begin
    Value := FComponents[I]; if Length(Value.Body) < 3 then Continue;
    SetLength(Poly, Length(Value.Body)); R := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
    for J := 0 to High(Poly) do
    begin
      Poly[J] := ScreenPoint(Value.Body[J]); R.Left := Min(R.Left, Poly[J].X); R.Right := Max(R.Right, Poly[J].X);
      R.Top := Min(R.Top, Poly[J].Y); R.Bottom := Max(R.Bottom, Poly[J].Y);
    end;
    ACanvas.Brush.Style := bsSolid; ACanvas.Pen.Width := 1; ACanvas.Pen.Color := Palette.NeutralDark;
    ACanvas.Brush.Color := Palette.NeutralLight;
    if Value.Role = 'terminal' then ACanvas.Brush.Color := ERDMixColor(Palette.GaugeFace, Palette.Success, 0.8)
    else if (Value.Role = 'ic') or (Value.Role = 'resistor') then ACanvas.Brush.Color := Palette.Subtle;
    ACanvas.Polygon(Poly);
    Saved := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, R.Left + 1, R.Top + 1, R.Right, R.Bottom);
      if Value.Role = 'terminal' then
      begin
        for Pad in FPads do if Pad.Reference = Value.Reference then
        begin
          P := ScreenPoint(Pad.Position); Radius := Max(ScaleValue(2), Round(0.8 * FScreenScale));
          ACanvas.Brush.Color := Palette.GaugeFace; ACanvas.Pen.Color := Palette.NeutralDark;
          ACanvas.Ellipse(P.X - Radius, P.Y - Radius, P.X + Radius, P.Y + Radius);
          ACanvas.MoveTo(P.X - Radius div 2, P.Y + Radius div 2); ACanvas.LineTo(P.X + Radius div 2, P.Y - Radius div 2);
        end;
      end
      else if Value.Role = 'usb' then
      begin
        Inner := R;
        if R.Width > R.Height then InflateRect(Inner, -Max(2, R.Width div 6), -Max(2, R.Height div 3))
        else InflateRect(Inner, -Max(2, R.Width div 3), -Max(2, R.Height div 6));
        ACanvas.Brush.Color := Palette.NeutralDark; ACanvas.RoundRect(Inner.Left, Inner.Top, Inner.Right, Inner.Bottom, ScaleValue(3), ScaleValue(3));
      end
      else if Value.Role = 'module' then
      begin
        Inner := R; InflateRect(Inner, -Max(1, R.Width div 12), -Max(1, R.Height div 12));
        if R.Height > R.Width then Inner.Bottom := Inner.Bottom - R.Height div 4 else Inner.Right := Inner.Right - R.Width div 4;
        ACanvas.Brush.Color := Palette.GaugeFace; ACanvas.Rectangle(Inner);
        ACanvas.Pen.Color := Palette.NeutralDark;
        for K := 1 to 4 do
        begin
          P := Point(R.Left + R.Width * K div 5, R.Bottom - R.Height div 8);
          ACanvas.MoveTo(P.X, P.Y); ACanvas.LineTo(P.X, R.Bottom - R.Height div 20);
        end;
      end
      else if Value.Role = 'button' then
      begin
        Inner := R; InflateRect(Inner, -Max(1, R.Width div 4), -Max(1, R.Height div 4));
        ACanvas.Brush.Color := Palette.Subtle; ACanvas.Ellipse(Inner);
      end
      else if Value.Role = 'capacitor' then
      begin
        Inner := R; InflateRect(Inner, -Max(1, R.Width div 8), -Max(1, R.Height div 8));
        ACanvas.Brush.Color := Palette.GaugeFace; ACanvas.Ellipse(Inner);
        ACanvas.MoveTo((R.Left + R.Right) div 2, R.Top); ACanvas.LineTo((R.Left + R.Right) div 2, R.Bottom);
      end;
    finally RestoreDC(ACanvas.Handle, Saved); end;
  end;
  // Draw selection outlines after every body, so adjacent terminals cannot cover an edge.
  ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Style := bsClear; ACanvas.Pen.Width := ScaleValue(1);
  ACanvas.Pen.Color := FSelectionColor;
  if FSelectionColor = clDefault then ACanvas.Pen.Color := Palette.Accent;
  for I := 0 to High(FComponents) do
  begin
    Value := FComponents[I]; TerminalID := TerminalIndex(Value.Reference);
    if (TerminalID < 0) or (Length(Value.Body) < 3) then Continue;
    if (FTerminals[TerminalID].SignalPin <> FSelected) and
       (FTerminals[TerminalID].SignalPin <> FHot) and (FTerminals[TerminalID].SignalPin <> ConnectionPin) then Continue;
    SetLength(Poly, Length(Value.Body));
    for J := 0 to High(Poly) do Poly[J] := ScreenPoint(Value.Body[J]);
    if (FTerminals[TerminalID].SignalPin = FSelected) or
       ((FConnectionID <> '') and (FTerminals[TerminalID].SignalPin = ConnectionPin)) then TintSelectedTerminal;
    // Use the exact imported terminal contour, with no inset or second frame.
    ACanvas.Polygon(Poly);
  end;
  // Programmable pads remain clear hit targets on the assembled component layer.
  if FShowPads then for I := 0 to High(FPads) do if IsSelectablePin(FPads[I].ID) then
  begin
    P := ScreenPoint(FPads[I].Position); Radius := Max(ScaleValue(3), Round(0.45 * FScreenScale));
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background; ACanvas.Pen.Color := Palette.NeutralDark;
    if (SelectionPinFor(FPads[I].ID) = FSelected) or (SelectionPinFor(FPads[I].ID) = FHot) or
       ((FConnectionID <> '') and (SelectionPinFor(FPads[I].ID) = ConnectionPin)) then
    begin ACanvas.Brush.Color := Palette.Accent; ACanvas.Pen.Color := Palette.ForegroundText; end;
    ACanvas.Ellipse(P.X - Radius, P.Y - Radius, P.X + Radius, P.Y + Radius);
  end;
end;
procedure TERDPCBDesigner.SetPanMode(Value: Boolean);
begin
  if FPanMode = Value then Exit;
  if Value then CancelPlacement;
  CancelGesture;
  FPanMode := Value; SetHot('');
  if FPanMode then Cursor := crHandPoint else Cursor := crDefault;
end;
procedure TERDPCBDesigner.SetAntiAlias(Value: Boolean);
begin
  if FAntiAlias = Value then Exit;
  FAntiAlias := Value; FStaticDirty := True; Invalidate;
end;
procedure TERDPCBDesigner.PaintControl(ACanvas: TCanvas);
const SampleScale = 3;
var Samples: TBitmap; Transform: TXForm; SavedDC, SavedMode: Integer;
begin
  if FStaticLayer = nil then Exit;
  if (FStaticLayer.Width <> ClientWidth) or (FStaticLayer.Height <> ClientHeight) then
  begin FStaticLayer.SetSize(Max(1, ClientWidth), Max(1, ClientHeight)); FStaticDirty := True; end;
  if FStaticDirty then
  begin
    if FAntiAlias then
    begin
      // Only rebuild the oversampled physical layer when geometry or the view changes.
      // Scene selection and dragging reuse the finished bitmap at native resolution.
      Samples := TBitmap.Create;
      try
        Samples.PixelFormat := pf32bit;
        Samples.SetSize(FStaticLayer.Width * SampleScale, FStaticLayer.Height * SampleScale);
        SavedDC := SaveDC(Samples.Canvas.Handle);
        try
          SetGraphicsMode(Samples.Canvas.Handle, GM_ADVANCED);
          FillChar(Transform, SizeOf(Transform), 0);
          Transform.eM11 := SampleScale; Transform.eM22 := SampleScale;
          SetWorldTransform(Samples.Canvas.Handle, Transform);
          PaintBoardLayer(Samples.Canvas);
        finally
          RestoreDC(Samples.Canvas.Handle, SavedDC);
        end;
        SavedMode := SetStretchBltMode(FStaticLayer.Canvas.Handle, HALFTONE);
        try
          SetBrushOrgEx(FStaticLayer.Canvas.Handle, 0, 0, nil);
          StretchBlt(FStaticLayer.Canvas.Handle, 0, 0, FStaticLayer.Width, FStaticLayer.Height,
            Samples.Canvas.Handle, 0, 0, Samples.Width, Samples.Height, SRCCOPY);
        finally
          SetStretchBltMode(FStaticLayer.Canvas.Handle, SavedMode);
        end;
      finally
        Samples.Free;
      end;
    end
    else PaintBoardLayer(FStaticLayer.Canvas);
    FStaticDirty := False;
  end;
  ACanvas.Draw(0, 0, FStaticLayer); PaintScene(ACanvas); PaintPlacementPreview(ACanvas);
end;
procedure TERDPCBDesigner.PaintGrid(ACanvas: TCanvas);
var X, Y, Step: Integer; GridMM: Double;
begin
  UpdateTransform;
  if FShowGrid then
  begin
    GridMM := FGridSize;
    while GridMM * FScreenScale < ScaleValue(12) do GridMM := GridMM * 2;
    Step := Max(1, Round(GridMM * FScreenScale));
    // Dots need contrast with the surface in both themes, not a divider color
    // that can be almost identical to a light canvas.
    if ERDIsDarkPalette(Palette) then ACanvas.Pen.Color := ERDMixColor(Palette.ForegroundText, Palette.Background, 0.24)
    else ACanvas.Pen.Color := ERDMixColor(Palette.ForegroundText, Palette.Background, 0.34);
    ACanvas.Pen.Style := psSolid; ACanvas.Pen.Width := Max(1, ScaleValue(1));
    Y := Round(FScreenY) mod Step; if Y < 0 then Inc(Y, Step);
    while Y < ClientHeight do
    begin
      X := Round(FScreenX) mod Step; if X < 0 then Inc(X, Step);
      while X < ClientWidth do
      begin ACanvas.MoveTo(X, Y); ACanvas.LineTo(X + 1, Y); Inc(X, Step); end;
      Inc(Y, Step);
    end;
  end;
end;
procedure TERDPCBDesigner.PaintScene(ACanvas: TCanvas);
var I, J, ClipState: Integer; R, LinkRect: TRect; Value: TERDPCBDesignItem;
    P, Q: TPoint; Pad: TERDPCBPad; Text: string; Role, ConnectionIndex: Integer;
    Tint: TBitmap; Blend: TBlendFunction; Ink, NoteColor: TColor;
    Definition: TERDPCBComponentType; Painter: TERDPainter; NoteIcon: TERDGlyph; Header, Badge: TRect; GlyphHandled: Boolean;
  procedure DrawConnection(const FromPoint, ToPoint: TPoint; Color: TColor; Width: Single; TargetIndex: Integer);
  var Graphics: TGPGraphics; Path: TGPGraphicsPath; Pen: TGPPen; Geometry: TERDPCBConnectionGeometry;
      Segment: TERDPCBPathSegment;
  begin
    Geometry := BuildConnectionGeometry(FromPoint, ToPoint, TargetIndex);
    Graphics := TGPGraphics.Create(ACanvas.Handle);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias); Path := TGPGraphicsPath.Create;
      try
        for Segment in Geometry.Segments do
          if Segment.Cubic then Path.AddBezier(Segment.Points[0].X, Segment.Points[0].Y, Segment.Points[1].X,
            Segment.Points[1].Y, Segment.Points[2].X, Segment.Points[2].Y, Segment.Points[3].X, Segment.Points[3].Y)
          else Path.AddLine(Segment.Points[0].X, Segment.Points[0].Y, Segment.Points[3].X, Segment.Points[3].Y);
        Pen := TGPPen.Create(ColorToARGB(Color), Width);
        try
          Pen.SetStartCap(LineCapRound); Pen.SetEndCap(LineCapRound); Pen.SetLineJoin(LineJoinRound); Graphics.DrawPath(Pen, Path);
        finally Pen.Free; end;
      finally Path.Free; end;
    finally Graphics.Free; end;
  end;
begin
  UpdateTransform;
  for I := 0 to FScene.Count - 1 do
  begin
    Value := FScene.Item(I);
    if Value.Kind = piNote then Continue;
    for Role := Ord(Low(TERDPCBPinRole)) to Ord(High(TERDPCBPinRole)) do
    begin
      if not ConnectionAnchors(I, TERDPCBPinRole(Role), P, Q) then Continue;
      if (FEditMode in [5,6]) and (Value.ID = FConnectionID) and (TERDPCBPinRole(Role) = FConnectionRole) then Continue;
      Ink := Palette.Subtle; if Value.Selected then Ink := Palette.Accent;
      if ((Value.ID = FConnectionID) and (TERDPCBPinRole(Role) = FConnectionRole)) or
         ((Value.ID = FHotConnectionID) and (TERDPCBPinRole(Role) = FHotConnectionRole)) then
        DrawConnection(P, Q, Palette.Accent, ScaleValue(2), I)
      else DrawConnection(P, Q, Ink, ScaleValue(1), I);
    end;
  end;
  // Keep every connection behind the component glyphs, regardless of item order.
  for I := 0 to FScene.Count - 1 do
  begin
    Value := FScene.Item(I); R := ItemRect(I);
    if Value.Kind = piNote then
    begin
      ACanvas.Brush.Style := bsSolid;
      NoteColor := Palette.Subtle; NoteIcon := glNone;
      case Value.NoteType of
        ntInfo: begin NoteColor := Palette.Accent; NoteIcon := glInfo; end;
        ntWarning: begin NoteColor := Palette.Warning; NoteIcon := glAlert; end;
        ntError: begin NoteColor := Palette.Danger; NoteIcon := glClose; end;
        ntQuestion: begin NoteColor := Palette.Accent; NoteIcon := glHelp; end;
      end;
      ACanvas.Brush.Color := ERDMixColor(NoteColor, Palette.Background, 0.12);
      ACanvas.Pen.Color := NoteColor; ACanvas.Pen.Width := 1;
      if Value.Selected then ACanvas.Pen.Color := Palette.Accent;
      ACanvas.Rectangle(R);
      ACanvas.Font.Assign(Font); ACanvas.Font.Color := Palette.ForegroundText;
      ACanvas.Brush.Style := bsClear;
      LinkRect := R; InflateRect(LinkRect, -ScaleValue(8), -ScaleValue(8));
      Text := Value.Caption;
      ClipState := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
        if Value.NoteType <> ntNone then
        begin
          Header := LinkRect; Header.Bottom := Header.Top + ScaleValue(20);
          Badge := Rect(Header.Left, Header.Top, Header.Left + ScaleValue(20), Header.Top + ScaleValue(20));
          Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
          try
            Painter.RoundRect(Badge.Left, Badge.Top, Badge.Width, Badge.Height, ScaleValue(4), NoteColor, clNone);
            Painter.GlyphLine(NoteIcon, Badge.Left + Badge.Width/2, Badge.Top + Badge.Height/2,
              ERDContrastInk(NoteColor, Palette.ForegroundText, Palette.Background), ScaleValue(96)/96);
          finally Painter.Free; end;
          Inc(Header.Left, ScaleValue(24)); ACanvas.Font.Style := [fsBold];
          DrawTextW(ACanvas.Handle, PWideChar(ERDPCBNoteTypeName(Value.NoteType)), -1, Header,
            DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
          ACanvas.Font.Style := []; Inc(LinkRect.Top, ScaleValue(26));
        end;
        DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), LinkRect,
          DT_WORDBREAK or DT_NOPREFIX or DT_EDITCONTROL);
      finally RestoreDC(ACanvas.Handle, ClipState); end;
      if Value.Selected and not Value.Locked and (FScene.SelectionCount = 1) then
      begin
        ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background;
        ACanvas.Pen.Color := Palette.Accent;
        for J := 0 to 7 do ACanvas.Rectangle(GripRect(R, J));
      end;
      Continue;
    end;
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background;
    ACanvas.Pen.Color := Palette.NeutralDark; ACanvas.Pen.Width := 1;
    if Value.Selected then begin ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := ScaleValue(2); end;
    ACanvas.Rectangle(R);
    ACanvas.Brush.Style := bsClear; ACanvas.Font.Name := 'Segoe UI';
    ACanvas.Font.Height := -ScaleValue(11); ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := Palette.ForegroundText;
    if not Value.Enabled then ACanvas.Font.Color := Palette.Subtle;
    GlyphHandled := False;
    if Assigned(FOnDrawItemGlyph) then FOnDrawItemGlyph(Self, ACanvas, R, Value, GlyphHandled);
    if not GlyphHandled then
    begin
      case Value.Kind of
        piTimer: Text := 'T'; piCounter: Text := '#'; piOutputGroup: Text := 'LED';
        piNote: Text := 'N'; piUART: Text := 'UART'; piCANBus: Text := 'CAN';
        piI2C: Text := 'I2C'; piSPI: Text := 'SPI';
      end;
      Definition := Default(TERDPCBComponentType);
      if ERDPCBFindComponentType(Value.TypeID, Definition) and (Definition.GlyphText <> '') then Text := Definition.GlyphText;
      if Definition.Icon <> glNone then
      begin
        Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
        try Painter.Glyph(Definition.Icon, (R.Left+R.Right)/2, (R.Top+R.Bottom)/2, ACanvas.Font.Color, ScaleValue(96)/96);
        finally Painter.Free; end;
      end
      else
      begin
        LinkRect := R; InflateRect(LinkRect, -ScaleValue(3), -ScaleValue(3));
        DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), LinkRect,
          DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
      end;
    end;
    ACanvas.Font.Name := 'Segoe UI'; ACanvas.Font.Height := -ScaleValue(11); ACanvas.Font.Style := [];
    ACanvas.Font.Color := Palette.ForegroundText; if not Value.Enabled then ACanvas.Font.Color := Palette.Subtle;
    ACanvas.Brush.Style := bsClear; Text := Value.Name;
    if Value.Locked then Text := Text + ' [locked]';
    LinkRect := Rect(R.Left - ScaleValue(64), R.Bottom + ScaleValue(3),
      R.Right + ScaleValue(64), R.Bottom + ScaleValue(22));
    DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), LinkRect,
      DT_SINGLELINE or DT_CENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
  end;
  ConnectionIndex := FScene.IndexOf(FConnectionID);
  if (ConnectionIndex >= 0) and ConnectionAnchors(ConnectionIndex, FConnectionRole, P, Q) then
  begin
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background;
    ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := ScaleValue(2);
    J := ScaleValue(4); ACanvas.Ellipse(P.X-J, P.Y-J, P.X+J+1, P.Y+J+1);
    ACanvas.Ellipse(Q.X-J, Q.Y-J, Q.X+J+1, Q.Y+J+1);
  end;
  if FEditMode in [5,6] then
  begin
    if (ConnectionIndex >= 0) and ConnectionAnchors(ConnectionIndex, FConnectionRole, P, Q) then
    begin
      I := ConnectionIndex;
      if FEditMode = 5 then P := FBoxPoint
      else begin Q := FBoxPoint; I := ItemAt(Q);
        if I >= 0 then begin R := ItemRect(I); Q := Point((R.Left+R.Right) div 2, R.Top);
          if (R.Top+R.Bottom) div 2 < P.Y then Q.Y := R.Bottom; end; end;
      DrawConnection(P, Q, Palette.Accent, ScaleValue(2), I);
    end;
  end;
  PaintAlignmentGuides(ACanvas);
  if FEditMode = 4 then
  begin
    P := FStartPoint;
    Pad := PadInfo(FLinkPin); if Pad.ID <> '' then P := ScreenPoint(Pad.Position);
    Q := FBoxPoint; I := ItemAt(Q);
    if I >= 0 then
    begin
      R := ItemRect(I); Q := Point((R.Left + R.Right) div 2, R.Top);
      if (R.Top + R.Bottom) div 2 < P.Y then Q.Y := R.Bottom;
    end;
    DrawConnection(P, Q, Palette.Accent, ScaleValue(2), I);
  end;
  if FEditMode = 3 then
  begin
    R := Rect(Min(FStartPoint.X, FBoxPoint.X), Min(FStartPoint.Y, FBoxPoint.Y),
      Max(FStartPoint.X, FBoxPoint.X), Max(FStartPoint.Y, FBoxPoint.Y));
    if (R.Width > 0) and (R.Height > 0) then
    begin
      Tint := TBitmap.Create;
      try
        Tint.PixelFormat := pf32bit; Tint.SetSize(1, 1);
        Tint.Canvas.Brush.Color := Palette.Accent; Tint.Canvas.FillRect(Rect(0, 0, 1, 1));
        Blend.BlendOp := AC_SRC_OVER; Blend.BlendFlags := 0;
        Blend.SourceConstantAlpha := 32; Blend.AlphaFormat := 0;
        Winapi.Windows.AlphaBlend(ACanvas.Handle, R.Left, R.Top, R.Width, R.Height,
          Tint.Canvas.Handle, 0, 0, 1, 1, Blend);
      finally Tint.Free; end;
    end;
    // Force the hollow brush into the DC even after SaveDC/RestoreDC painting.
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Width := 1; ACanvas.Pen.Color := Palette.Accent;
    ACanvas.Rectangle(R);
  end;
end;
procedure TERDPCBDesigner.ConnectPin(Index: Integer; const Pin: string);
var Value: TERDPCBDesignItem; Role: TERDPCBPinRole; Current, Error: string;
begin
  Value := FScene.Item(Index);
  if Value.Kind = piNote then raise EArgumentException.Create('Notes do not have pin connections');
  if Value.Locked then raise EArgumentException.Create('Unlock the component before connecting it');
  Error := 'No compatible unassigned pin role';
  for Role := Low(TERDPCBPinRole) to High(TERDPCBPinRole) do
  begin
    if not SupportsPinRole(Index, Role) then Continue;
    case Role of prPrimary: Current := Value.Pin; prSecondary: Current := Value.SecondaryPin;
      prClock: Current := Value.ClockPin; else Current := Value.SelectPin; end;
    if Current <> '' then Continue;
    try ValidatePinAssignment(Index, Role, Pin);
    except on E: EArgumentException do begin Error := E.Message; Continue; end; end;
    AssignPin(Index, Role, Pin); FScene.Select(Index); Exit;
  end;
  raise EArgumentException.Create(Error);
end;
procedure TERDPCBDesigner.CancelGesture;
var Mode: Integer;
begin
  Mode := FEditMode; FEditMode := 0; ClearGuides; FMoveTargets := nil;
  if (Mode in [1, 2]) and (FScene <> nil) then FScene.CancelEdit;
  if Mode <> 0 then begin MouseCapture := False; Invalidate; end;
end;
procedure TERDPCBDesigner.KeyDown(var Key: Word; Shift: TShiftState);
var I, Current, Candidate, DX, DY, Forward, Across: Integer; P, Q: TPoint; Score, Best: Double;
begin
  inherited;
  if FPlacing and (Key = VK_ESCAPE) then begin CancelPlacement; Key := 0; Exit; end;
  if FEditMode <> 0 then begin if Key = VK_ESCAPE then CancelGesture; Key := 0; Exit; end;
  if (Key = VK_DELETE) and (FConnectionID <> '') then
  begin
    try DeleteSelectedConnection; except on E: Exception do ReportConnectionError(E.Message); end;
    Key := 0; Exit;
  end;
  if (Key = VK_ESCAPE) and (FConnectionID <> '') then begin ClearConnectionSelection; Key := 0; Exit; end;
  if (Key = VK_HOME) or ((Key = Ord('0')) and (ssCtrl in Shift)) then begin FitBoard; Key := 0; Exit; end;
  if (Key = Ord('F')) and (Shift = []) then begin FitSelection; Key := 0; Exit; end;
  if ssCtrl in Shift then
  begin
    case Key of
      Ord('Z'): Undo; Ord('Y'): Redo; Ord('C'): CopySelected;
      Ord('X'): begin CopySelected; FScene.DeleteSelected; end;
      Ord('V'): Paste; Ord('A'): FScene.SelectAll;
    else Exit;
    end;
    Key := 0; Exit;
  end;
  if FScene.SelectionCount > 0 then
  begin
    case Key of
      VK_DELETE: FScene.DeleteSelected;
      VK_ESCAPE: FScene.ClearSelection;
      VK_LEFT: FScene.MoveSelected(-FGridSize, 0); VK_RIGHT: FScene.MoveSelected(FGridSize, 0);
      VK_UP: FScene.MoveSelected(0, FGridSize); VK_DOWN: FScene.MoveSelected(0, -FGridSize);
    else Exit;
    end;
    Key := 0; Exit;
  end;
  if Key = VK_ESCAPE then begin SelectedPin := ''; Key := 0; Exit; end;
  if Key = VK_HOME then begin FitBoard; Key := 0; Exit; end;
  if not (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) or not FShowPads then Exit;
  Current := -1;
  for I := 0 to High(FPads) do if FPads[I].ID = FSelected then begin Current := I; Break; end;
  if Current < 0 then
  begin if Length(FChannels) > 0 then SelectedPin := FChannels[0].Pin; Key := 0; Exit; end;
  UpdateTransform; P := ScreenPoint(FPads[Current].Position); Candidate := -1; Best := MaxDouble;
  for I := 0 to High(FPads) do if (FPads[I].ID <> FSelected) and IsSelectablePin(FPads[I].ID) then
  begin
    Q := ScreenPoint(FPads[I].Position); DX := Q.X - P.X; DY := Q.Y - P.Y;
    if Key in [VK_LEFT, VK_RIGHT] then begin Forward := DX; Across := Abs(DY); end
    else begin Forward := DY; Across := Abs(DX); end;
    if Key in [VK_LEFT, VK_UP] then Forward := -Forward;
    if Forward <= 0 then Continue;
    Score := Sqr(Double(Forward)) + 4 * Sqr(Double(Across));
    if Score < Best then begin Best := Score; Candidate := I; end;
  end;
  if Candidate >= 0 then SelectedPin := FPads[Candidate].ID;
  Key := 0;
end;
procedure TERDPCBDesigner.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer; P, FromPoint, ToPoint: TPoint; ID: string; Role: TERDPCBPinRole;
begin
  inherited; SetFocus; P := Point(X, Y);
  if (Button = mbMiddle) or ((Button = mbLeft) and (FPanMode or (GetKeyState(VK_SPACE) < 0))) then
  begin FDragging := True; FLast := P; MouseCapture := True; Cursor := crSizeAll; Exit; end;
  if FPlacing and (Button = mbRight) then begin FSuppressContextMenu := True; CancelPlacement; Exit; end;
  if Button <> mbLeft then Exit;
  if FPlacing then
  begin
    InsertTypeAt(FPlacementTypeID, P);
    if not (ssShift in Shift) then CancelPlacement else PreviewPlacementAt(P);
    Exit;
  end;
  Index := FScene.IndexOf(FConnectionID);
  if (Index >= 0) and ConnectionAnchors(Index, FConnectionRole, FromPoint, ToPoint) then
  begin
    FEditMode := 0;
    if Hypot(P.X-FromPoint.X, P.Y-FromPoint.Y) <= ScaleValue(8) then FEditMode := 5
    else if Hypot(P.X-ToPoint.X, P.Y-ToPoint.Y) <= ScaleValue(8) then FEditMode := 6;
    if FEditMode <> 0 then
    begin
      if FScene.Item(Index).Locked then begin FEditMode := 0; Exit; end;
      FBoxPoint := P; MouseCapture := True; Exit;
    end;
  end;
  Index := ItemAt(P);
  if not FTemplateEditMode and (Index < 0) and (PadAt(X,Y) = '') and HitTestConnection(P, ID, Role) then
  begin SelectConnection(ID, Role); Exit; end;
  ClearConnectionSelection;
  FGrip := GripAt(P); Index := ItemAt(P);
  if FTemplateEditMode then begin FGrip := -1; Index := -1; end;
  if FGrip >= 0 then
  begin
    FDragIndex := FScene.PrimaryIndex; FOriginalItem := FScene.Item(FDragIndex); FEditMode := 2; FScene.BeginEdit;
  end
  else if Index >= 0 then
  begin
    if (ssCtrl in Shift) or (ssShift in Shift) then
    begin FScene.Select(Index, True, True); Exit; end;
    if not FScene.Item(Index).Selected then FScene.Select(Index);
    if FScene.Item(Index).Locked then Exit;
    BeginMoveGuides(Index); FEditMode := 1; FScene.BeginEdit;
  end
  else
  begin
    SelectedPin := PadAt(X, Y);
    if FSelected <> '' then
    begin
      if not FTemplateEditMode then
      begin FLinkPin := FSelected; FEditMode := 4; FStartPoint := P; FBoxPoint := P; MouseCapture := True; end;
      Exit;
    end;
    if not (ssShift in Shift) then FScene.ClearSelection;
    FEditMode := 3; FBoxPoint := P;
  end;
  FStartPoint := P; FStartWorld := ClientToBoard(P); FAppliedX := 0; FAppliedY := 0; MouseCapture := True;
end;
procedure TERDPCBDesigner.MouseMove(Shift: TShiftState; X, Y: Integer);
var Hot, ID: string; Role: TERDPCBPinRole; P: TPointF; FromPoint, ToPoint: TPoint;
    DX, DY: Double; Value: TERDPCBDesignItem; Grip, ConnectionIndex: Integer;
begin
  inherited;
  if FDragging and ((ssMiddle in Shift) or (ssLeft in Shift)) then
  begin PanBy(X - FLast.X, Y - FLast.Y); FLast := Point(X, Y); Exit; end;
  if FPlacing then begin Cursor := crCross; PreviewPlacementAt(Point(X,Y)); Exit; end;
  if FEditMode in [3, 4, 5, 6] then begin FBoxPoint := Point(X, Y); Invalidate; Exit; end;
  if FEditMode in [1, 2] then
  begin
    if (X = FStartPoint.X) and (Y = FStartPoint.Y) and (FAppliedX = 0) and (FAppliedY = 0) then Exit;
    P := ClientToBoard(Point(X, Y)); DX := P.X - FStartWorld.X; DY := P.Y - FStartWorld.Y;
    if FEditMode = 1 then
    begin
      ClearGuides;
      if not (ssAlt in Shift) then SnapGuides(DX, DY);
      FScene.MoveSelected(DX - FAppliedX, DY - FAppliedY); FAppliedX := DX; FAppliedY := DY; Invalidate; end
    else
    begin
      if not (ssAlt in Shift) then begin DX := Snap(DX); DY := Snap(DY); end;
      Value := FOriginalItem;
      if FGrip in [0, 6, 7] then begin Value.X := Value.X + DX; Value.Width := Value.Width - DX; end;
      if FGrip in [2, 3, 4] then Value.Width := Value.Width + DX;
      if FGrip in [0, 1, 2] then Value.Height := Value.Height + DY;
      if FGrip in [4, 5, 6] then begin Value.Y := Value.Y + DY; Value.Height := Value.Height - DY; end;
      if Value.Width < 2 then
      begin Value.Width := 2; if FGrip in [0, 6, 7] then Value.X := FOriginalItem.X + FOriginalItem.Width - 2; end;
      if Value.Height < 2 then
      begin Value.Height := 2; if FGrip in [4, 5, 6] then Value.Y := FOriginalItem.Y + FOriginalItem.Height - 2; end;
      Value.Width := Min(1000, Value.Width); Value.Height := Min(1000, Value.Height);
      FScene.UpdateItem(FDragIndex, Value);
    end;
    Exit;
  end;
  if FPanMode or (GetKeyState(VK_SPACE) < 0) then
  begin SetHot(''); Cursor := crHandPoint; Exit; end;
  ConnectionIndex := FScene.IndexOf(FConnectionID);
  if (ConnectionIndex >= 0) and ConnectionAnchors(ConnectionIndex, FConnectionRole, FromPoint, ToPoint) then
    if (Hypot(X-FromPoint.X, Y-FromPoint.Y) <= ScaleValue(8)) or (Hypot(X-ToPoint.X, Y-ToPoint.Y) <= ScaleValue(8)) then
    begin
      SetHot(''); SetHotConnection(FConnectionID, FConnectionRole);
      if FScene.Item(ConnectionIndex).Locked then Cursor := crDefault else Cursor := crCross;
      Exit;
    end;
  if not FTemplateEditMode and (ItemAt(Point(X,Y)) < 0) and (PadAt(X,Y) = '') and
    HitTestConnection(Point(X,Y), ID, Role) then
  begin SetHot(''); SetHotConnection(ID, Role); Cursor := crHandPoint; Exit; end;
  SetHotConnection('', prPrimary);
  Grip := GripAt(Point(X, Y));
  case Grip of
    0, 4: Cursor := crSizeNWSE; 2, 6: Cursor := crSizeNESW;
    1, 5: Cursor := crSizeNS; 3, 7: Cursor := crSizeWE;
  else
    if ItemAt(Point(X, Y)) >= 0 then Cursor := crSizeAll
    else begin Hot := PadAt(X, Y); SetHot(Hot); if Hot <> '' then Cursor := crHandPoint else Cursor := crDefault; end;
  end;
end;
procedure TERDPCBDesigner.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Mode, I: Integer; R, Intersection: TRect;
begin
  // VCL releases capture in inherited MouseUp. Mark the gesture complete first
  // so WM_CAPTURECHANGED cannot roll back a successful drop.
  Mode := FEditMode; FEditMode := 0; ClearGuides; FMoveTargets := nil;
  if Mode in [1, 2] then FScene.EndEdit;
  inherited;
  if FDragging then
  begin
    FDragging := False; MouseCapture := False;
    if FPanMode then Cursor := crHandPoint else Cursor := crDefault;
    Exit;
  end;
  if Button <> mbLeft then Exit;
  if Mode = 4 then
  begin
    I := ItemAt(Point(X,Y));
    if I >= 0 then
      try ConnectPin(I, FLinkPin); FLastError := '';
      except on E: Exception do
      begin ReportConnectionError(E.Message); end; end;
    FLinkPin := '';
  end;
  if Mode in [5,6] then
  begin
    try
      if Mode = 5 then ReconnectSelectedPin(PadAt(X,Y))
      else begin I := ItemAt(Point(X,Y));
        if I < 0 then raise EArgumentException.Create('Drop on a component with a free compatible role');
        MoveSelectedConnection(FScene.Item(I).ID, FConnectionRole); end;
      FLastError := '';
    except on E: Exception do ReportConnectionError(E.Message); end;
    Invalidate;
  end;
  if Mode = 3 then
  begin
    R := Rect(Min(FStartPoint.X, X), Min(FStartPoint.Y, Y), Max(FStartPoint.X, X), Max(FStartPoint.Y, Y));
    for I := 0 to FScene.Count - 1 do if IntersectRect(Intersection, R, ItemRect(I)) then FScene.Select(I, True);
  end;
  if Mode <> 0 then begin MouseCapture := False; Invalidate; end;
end;
function TERDPCBDesigner.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if FEditMode <> 0 then Exit(True);
  ZoomAt(FZoom * Power(1.2, WheelDelta / 120), ScreenToClient(MousePos)); Result := True;
end;
procedure TERDPCBDesigner.SaveProject(const FileName: string);
var Root, Item: TJSONObject; Items: TJSONArray; C: TERDPCBChannel;
begin
  FinishNoteEdit(True);
  if FBoardHash = '' then raise EConvertError.Create('Load a board before saving a project');
  Root := TJSONObject.Create;
  try
    Root.AddPair('schema', TJSONNumber.Create(2)); Root.AddPair('scene', TJSONObject.ParseJSONValue(FScene.ToJSON)); Root.AddPair('board', FBoardHash);
    Root.AddPair('template_policy', PinPolicy); Root.AddPair('selected', FSelected); Items := TJSONArray.Create; Root.AddPair('bindings', Items);
    for C in FChannels do
    begin
      Item := TJSONObject.Create; Items.AddElement(Item);
      Item.AddPair('pin', C.Pin); Item.AddPair('name', C.Name); Item.AddPair('action', C.Action);
      Item.AddPair('led_count', TJSONNumber.Create(C.LEDCount));
    end;
    TFile.WriteAllText(FileName, Root.ToJSON, TEncoding.UTF8);
  finally Root.Free; end;
end;
procedure TERDPCBDesigner.LoadProject(const FileName: string);
var Root: TJSONValue; Items: TJSONArray; Item: TJSONObject; Backup: TArray<TERDPCBChannel>;
    I, Index, Schema: Integer; Selection, OldScene: string; Seen: TStringList; TempScene: TERDPCBScene;
begin
  CancelPlacement;
  FinishNoteEdit(True);
  Root := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName, TEncoding.UTF8));
  Backup := Copy(FChannels); OldScene := FScene.ToJSON; Seen := TStringList.Create; TempScene := TERDPCBScene.Create(nil);
  try
    try
      if not (Root is TJSONObject) then raise EConvertError.Create('Invalid PCB project');
      Schema := Root.GetValue<Integer>('schema');
      if not (Schema in [1, 2]) or (Root.GetValue<string>('board') <> FBoardHash) then
        raise EConvertError.Create('Project belongs to a different board or schema');
      if (Schema = 2) and (Root.GetValue<string>('template_policy') <> PinPolicy) then raise EConvertError.Create('Template pin capabilities have changed');
      if Schema = 2 then
      begin
        if not (TJSONObject(Root).GetValue('scene') is TJSONObject) then raise EConvertError.Create('Invalid project scene');
        TempScene.LoadJSON(Root.GetValue<TJSONObject>('scene').ToJSON);
      end;
      Selection := Root.GetValue<string>('selected');
      Items := Root.GetValue<TJSONArray>('bindings');
      if Items.Count <> Length(FChannels) then raise EConvertError.Create('Incomplete channel assignments');
      for I := 0 to High(FChannels) do begin FChannels[I].Name := ''; FChannels[I].Action := ''; FChannels[I].LEDCount := 1; end;
      for I := 0 to Items.Count - 1 do
      begin
        if not (Items.Items[I] is TJSONObject) then raise EConvertError.Create('Expected binding object');
        Item := TJSONObject(Items.Items[I]); Index := ChannelIndex(Item.GetValue<string>('pin'));
        if (Index < 0) or (Seen.IndexOf(IntToStr(Index)) >= 0) then raise EConvertError.Create('Unknown or repeated output');
        Seen.Add(IntToStr(Index));
        SetBinding(Index, Item.GetValue<string>('name'), Item.GetValue<string>('action'), Item.GetValue<Integer>('led_count'));
      end;
      FScene.Assign(TempScene);
      if Selection = FSelected then
      begin if Assigned(FOnSelectionChange) then FOnSelectionChange(Self); end
      else SelectedPin := Selection;
    except
      FChannels := Backup; FScene.LoadJSON(OldScene);
      raise;
    end;
  finally
    TempScene.Free; Seen.Free; Root.Free;
  end;
  FScene.ResetHistory;
end;
end.
