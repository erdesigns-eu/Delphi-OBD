unit ERD.Components.PCBDesigner;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.JSON, System.UITypes,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.PCBScene, ERD.Components.ScrollBar;

type
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
    FDragging, FPanMode: Boolean;
    FLast: TPoint;
    FOnSelectionChange, FOnHotPinChange, FOnViewChange: TNotifyEvent;
    procedure SetPanMode(Value: Boolean);
    procedure SetAntiAlias(Value: Boolean);
    function BuildTemplateJSON: string;
    function PinPolicy: string;
    function SelectionPinFor(const Pin: string): string;
    function TerminalRect(Index: Integer): TRect;
    function IsSelectablePin(const Pin: string): Boolean;
    procedure SetTemplateEditMode(Value: Boolean);
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
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged; override;
    function AddDesignItem(Kind: TERDPCBItemKind; const Position: TPointF): Integer;
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

uses System.IOUtils, Vcl.Forms, Vcl.Clipbrd, Vcl.StdCtrls, ERD.Components.Paint;

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
    begin C := ParseCapability(Trim(Parts[I])); if C <> pcNone then Include(Result, C); end;
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
  FScene := TERDPCBScene.Create(Self); FScene.OnValidateItem := ValidateSceneItem; FScene.OnChange := SceneChanged; FScene.OnSelectionChange := SceneSelectionChanged;
  FScrollH := TERDScrollBar.Create(Self); FScrollH.Parent := Self; FScrollH.Kind := sbHorizontal; FScrollH.Visible := False;
  FScrollH.OnChange := ScrollChanged; FScrollH.TabStop := False;
  FScrollV := TERDScrollBar.Create(Self); FScrollV.Parent := Self; FScrollV.Kind := sbVertical; FScrollV.Visible := False;
  FScrollV.OnChange := ScrollChanged; FScrollV.TabStop := False;
  ShowHint := True; Hint := 'Select a PCB pin or designer component';
end;
destructor TERDPCBDesigner.Destroy;
begin
  if FScrollH <> nil then FScrollH.OnChange := nil;
  if FScrollV <> nil then FScrollV.OnChange := nil;
  if FScene <> nil then begin FScene.OnChange := nil; FScene.OnSelectionChange := nil; end;
  CancelGesture;
  if HandleAllocated then DestroyWnd;
  FreeAndNil(FScene); FreeAndNil(FStaticLayer);
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
end;
procedure TERDPCBDesigner.SetTemplateEditMode(Value: Boolean);
begin
  if FTemplateEditMode = Value then Exit;
  CancelGesture; FTemplateEditMode := Value;
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
  Invalidate; SyncScrollBars;
  if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
end;
procedure TERDPCBDesigner.SceneSelectionChanged(Sender: TObject);
begin
  if (FScene.SelectionCount > 0) and (FSelected <> '') then begin FSelected := ''; FStaticDirty := True; end;
  Invalidate;
  if not (csLoading in ComponentState) and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
procedure TERDPCBDesigner.ThemeChanged;
begin FStaticDirty := True; inherited; end;
procedure TERDPCBDesigner.Resize;
begin FStaticDirty := True; inherited; SyncScrollBars; end;
procedure TERDPCBDesigner.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin if FScrollBars <> Value then begin FScrollBars := Value; SyncScrollBars; Invalidate; end; end;
procedure TERDPCBDesigner.SyncScrollBars;
var Bounds, R: TRect; P, Q: TPoint; I, W, H, T: Integer;
begin
  if (FScrollH = nil) or (FScrollV = nil) or (FScene = nil) or FUpdatingBars then Exit;
  FUpdatingBars := True;
  try
    UpdateTransform; P := ScreenPoint(PointF(0, FMaxY)); Q := ScreenPoint(PointF(FMaxX, 0));
    Bounds := Rect(Min(0, P.X), Min(0, P.Y), Max(ClientWidth, Q.X), Max(ClientHeight, Q.Y));
    for I := 0 to FScene.Count - 1 do
    begin
      R := ItemRect(I); Bounds.Left := Min(Bounds.Left, R.Left); Bounds.Top := Min(Bounds.Top, R.Top);
      Bounds.Right := Max(Bounds.Right, R.Right); Bounds.Bottom := Max(Bounds.Bottom, R.Bottom);
    end;
    FScrollH.Visible := FScrollBars in [System.UITypes.TScrollStyle.ssHorizontal, System.UITypes.TScrollStyle.ssBoth];
    FScrollV.Visible := FScrollBars in [System.UITypes.TScrollStyle.ssVertical, System.UITypes.TScrollStyle.ssBoth];
    W := ClientWidth; H := ClientHeight; T := ScaleValue(12);
    if FScrollV.Visible then Dec(W, T); if FScrollH.Visible then Dec(H, T);
    FScrollH.SetBounds(0, ClientHeight - T, Max(0, W), T);
    FScrollV.SetBounds(ClientWidth - T, 0, T, Max(0, H));
    FBarX := -Bounds.Left; FBarY := -Bounds.Top;
    FScrollH.Min := 0; FScrollH.Max := Max(0, Bounds.Width - 1); FScrollH.PageSize := ClientWidth;
    FScrollH.Position := FBarX; FScrollH.SmallChange := ScaleValue(16);
    FScrollH.Enabled := Bounds.Width > ClientWidth;
    FScrollV.Min := 0; FScrollV.Max := Max(0, Bounds.Height - 1); FScrollV.PageSize := ClientHeight;
    FScrollV.Position := FBarY; FScrollV.SmallChange := ScaleValue(16);
    FScrollV.Enabled := Bounds.Height > ClientHeight;
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
begin CancelGesture; FScene.Undo; end;
procedure TERDPCBDesigner.Redo;
begin CancelGesture; FScene.Redo; end;
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
begin SetHot(''); if not FDragging then Cursor := crDefault; inherited; end;
procedure TERDPCBDesigner.WMCaptureChanged(var Message: TMessage);
begin FDragging := False; CancelGesture; Cursor := crDefault; inherited; end;
procedure TERDPCBDesigner.WMGetDlgCode(var Message: TWMGetDlgCode);
begin Message.Result := DLGC_WANTARROWS; end;
procedure TERDPCBDesigner.CMHintShow(var Message: TCMHintShow);
var Pad: TERDPCBPad; P: TPoint;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint or (FHot = '') then Exit;
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
    if FPads[I].ID = FSelected then
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
  Value := FScene.Item(Index); P := ScreenPoint(PointF(Value.X, Value.Y + Value.Height));
  Q := ScreenPoint(PointF(Value.X + Value.Width, Value.Y)); Result := Rect(P.X, P.Y, Q.X, Q.Y);
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
  case Index of
    0, 6, 7: X := R.Left; 1, 5: X := (R.Left + R.Right) div 2;
  else X := R.Right;
  end;
  case Index of
    0, 1, 2: Y := R.Top; 3, 7: Y := (R.Top + R.Bottom) div 2;
  else Y := R.Bottom;
  end;
  Size := ScaleValue(4); Result := Rect(X - Size, Y - Size, X + Size + 1, Y + Size + 1);
end;
function TERDPCBDesigner.GripAt(const P: TPoint): Integer;
var I, Index: Integer; R: TRect;
begin
  Result := -1; if FScene.SelectionCount <> 1 then Exit;
  Index := FScene.PrimaryIndex; if FScene.Item(Index).Locked then Exit;
  UpdateTransform; R := ItemRect(Index);
  for I := 0 to 7 do if PtInRect(GripRect(R, I), P) then Exit(I);
end;
procedure TERDPCBDesigner.PaintBodies(ACanvas: TCanvas);
var I, J, K: Integer; Value: TERDPCBComponent; Poly: TArray<TPoint>; R, Inner: TRect; P: TPoint;
    Saved, Radius, TerminalID: Integer; Pad: TERDPCBPad;
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
    TerminalID := TerminalIndex(Value.Reference);
    if (TerminalID >= 0) and ((FTerminals[TerminalID].SignalPin = FSelected) or (FTerminals[TerminalID].SignalPin = FHot)) then
    begin ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := ScaleValue(2); end;
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
  // Programmable pads remain clear hit targets on the assembled component layer.
  if FShowPads then for I := 0 to High(FPads) do if IsSelectablePin(FPads[I].ID) then
  begin
    P := ScreenPoint(FPads[I].Position); Radius := Max(ScaleValue(3), Round(0.45 * FScreenScale));
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background; ACanvas.Pen.Color := Palette.NeutralDark;
    if (SelectionPinFor(FPads[I].ID) = FSelected) or (SelectionPinFor(FPads[I].ID) = FHot) then
    begin ACanvas.Brush.Color := Palette.Accent; ACanvas.Pen.Color := Palette.ForegroundText; end;
    ACanvas.Ellipse(P.X - Radius, P.Y - Radius, P.X + Radius, P.Y + Radius);
  end;
end;
procedure TERDPCBDesigner.SetPanMode(Value: Boolean);
begin
  if FPanMode = Value then Exit;
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
  ACanvas.Draw(0, 0, FStaticLayer); PaintScene(ACanvas);
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
    ACanvas.Pen.Color := Palette.NeutralLight;
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
var I, J, Index, Saved: Integer; R, LinkRect: TRect; Value: TERDPCBDesignItem;
    P, Q: TPoint; Pad: TERDPCBPad; Text, Link: string; Role: Integer;
begin
  UpdateTransform;
  for I := 0 to FScene.Count - 1 do
  begin
    Value := FScene.Item(I); R := ItemRect(I);
    for Role := 0 to 3 do
    begin
    case Role of 0: Link := Value.Pin; 1: Link := Value.SecondaryPin; 2: Link := Value.ClockPin; else Link := Value.SelectPin; end;
    if Link <> '' then for Pad in FPads do if Pad.ID = Link then
    begin
      P := ScreenPoint(Pad.Position); Q := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
      ACanvas.Pen.Color := Palette.Subtle; if Value.Selected then ACanvas.Pen.Color := Palette.Accent;
      ACanvas.Pen.Width := 1; ACanvas.MoveTo(P.X, P.Y); ACanvas.LineTo(Q.X, Q.Y); Break;
    end;
    end;
    ACanvas.Brush.Style := bsSolid; ACanvas.Brush.Color := Palette.Background;
    ACanvas.Pen.Color := Palette.NeutralDark; ACanvas.Pen.Width := 1;
    if Value.Selected then begin ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := ScaleValue(2); end;
    ACanvas.Rectangle(R);
    Saved := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
      ACanvas.Brush.Style := bsClear; ACanvas.Font.Name := 'Segoe UI'; ACanvas.Font.Height := -ScaleValue(11);
      ACanvas.Font.Style := [fsBold]; ACanvas.Font.Color := Palette.ForegroundText;
      if not Value.Enabled then ACanvas.Font.Color := Palette.Subtle;
      Text := Value.Name; if Value.Locked then Text := Text + ' [locked]';
      LinkRect := Rect(R.Left + ScaleValue(6), R.Top + ScaleValue(4), R.Right - ScaleValue(5), R.Bottom);
      DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), LinkRect, DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX);
      ACanvas.Font.Style := []; ACanvas.Font.Color := Palette.Subtle;
      Text := ERDPCBItemKindName(Value.Kind); if Value.Pin <> '' then Text := Text + ' - ' + Value.Pin;
      LinkRect.Top := LinkRect.Top + ScaleValue(17);
      DrawTextW(ACanvas.Handle, PWideChar(Text), Length(Text), LinkRect, DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX);
    finally RestoreDC(ACanvas.Handle, Saved); end;
  end;
  Index := FScene.PrimaryIndex;
  if (FScene.SelectionCount = 1) and (Index >= 0) and not FScene.Item(Index).Locked then
  begin
    R := ItemRect(Index); ACanvas.Pen.Width := 1; ACanvas.Pen.Color := Palette.Accent; ACanvas.Brush.Color := Palette.Background;
    for J := 0 to 7 do ACanvas.Rectangle(GripRect(R, J));
  end;
  if FEditMode = 4 then
  begin
    ACanvas.Pen.Color := Palette.Accent; ACanvas.Pen.Width := ScaleValue(2);
    ACanvas.MoveTo(FStartPoint.X, FStartPoint.Y); ACanvas.LineTo(FBoxPoint.X, FBoxPoint.Y);
  end;
  if FEditMode = 3 then
  begin
    ACanvas.Brush.Style := bsClear; ACanvas.Pen.Width := 1; ACanvas.Pen.Color := Palette.Accent;
    ACanvas.Rectangle(Min(FStartPoint.X, FBoxPoint.X), Min(FStartPoint.Y, FBoxPoint.Y),
      Max(FStartPoint.X, FBoxPoint.X), Max(FStartPoint.Y, FBoxPoint.Y));
  end;
end;
procedure TERDPCBDesigner.ConnectPin(Index: Integer; const Pin: string);
var Value: TERDPCBDesignItem; Caps: TERDPCBPinCapabilities; C: Integer;
  function Choose(var Role: string; Capability: TERDPCBPinCapability): Boolean;
  begin
    Result := (Role = '') and ((Capability = pcNone) or (Capability in Caps));
    if Result then Role := Pin;
  end;
begin
  Value := FScene.Item(Index); if Value.Locked then raise EArgumentException.Create('Unlock the component before connecting it');
  C := ChannelIndex(Pin); if C < 0 then raise EArgumentException.Create('Pin is not programmable');
  Caps := FChannels[C].Capabilities;
  case Value.Kind of
    piUART: if not Choose(Value.Pin, pcUARTTx) then if not Choose(Value.SecondaryPin, pcUARTRx) then raise EArgumentException.Create('No compatible unassigned UART role');
    piCANBus: if not Choose(Value.Pin, pcCANTx) then if not Choose(Value.SecondaryPin, pcCANRx) then raise EArgumentException.Create('No compatible unassigned CAN role');
    piI2C: if not Choose(Value.Pin, pcI2CSDA) then if not Choose(Value.SecondaryPin, pcI2CSCL) then raise EArgumentException.Create('No compatible unassigned I2C role');
    piSPI: if not Choose(Value.Pin, pcSPIMOSI) then if not Choose(Value.SecondaryPin, pcSPIMISO) then
      if not Choose(Value.ClockPin, pcSPIClock) then if not Choose(Value.SelectPin, pcSPISelect) then raise EArgumentException.Create('No compatible unassigned SPI role');
  else Value.Pin := Pin;
  end;
  FScene.UpdateItem(Index, Value); FScene.Select(Index);
end;
procedure TERDPCBDesigner.CancelGesture;
var Mode: Integer;
begin
  Mode := FEditMode; FEditMode := 0;
  if (Mode in [1, 2]) and (FScene <> nil) then FScene.CancelEdit;
  if Mode <> 0 then begin MouseCapture := False; Invalidate; end;
end;
procedure TERDPCBDesigner.KeyDown(var Key: Word; Shift: TShiftState);
var I, Current, Candidate, DX, DY, Forward, Across: Integer; P, Q: TPoint; Score, Best: Double;
begin
  inherited;
  if FEditMode <> 0 then begin if Key = VK_ESCAPE then CancelGesture; Key := 0; Exit; end;
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
var Index: Integer; P: TPoint;
begin
  inherited; SetFocus; P := Point(X, Y);
  if (Button = mbMiddle) or ((Button = mbLeft) and (FPanMode or (GetKeyState(VK_SPACE) < 0))) then
  begin FDragging := True; FLast := P; MouseCapture := True; Cursor := crSizeAll; Exit; end;
  if Button <> mbLeft then Exit;
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
    FEditMode := 1; FScene.BeginEdit;
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
var Hot: string; P: TPointF; DX, DY: Double; Value: TERDPCBDesignItem; Grip: Integer;
begin
  inherited;
  if FDragging and ((ssMiddle in Shift) or (ssLeft in Shift)) then
  begin PanBy(X - FLast.X, Y - FLast.Y); FLast := Point(X, Y); Exit; end;
  if FEditMode in [3, 4] then begin FBoxPoint := Point(X, Y); Invalidate; Exit; end;
  if FEditMode in [1, 2] then
  begin
    P := ClientToBoard(Point(X, Y)); DX := P.X - FStartWorld.X; DY := P.Y - FStartWorld.Y;
    if not (ssAlt in Shift) then begin DX := Snap(DX); DY := Snap(DY); end;
    if FEditMode = 1 then
    begin FScene.MoveSelected(DX - FAppliedX, DY - FAppliedY); FAppliedX := DX; FAppliedY := DY; end
    else
    begin
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
  inherited;
  if FDragging then
  begin
    FDragging := False; MouseCapture := False;
    if FPanMode then Cursor := crHandPoint else Cursor := crDefault;
    Exit;
  end;
  if Button <> mbLeft then Exit;
  Mode := FEditMode; FEditMode := 0;
  if Mode in [1, 2] then FScene.EndEdit;
  if Mode = 4 then
  begin
    I := ItemAt(Point(X,Y));
    if I >= 0 then
      try ConnectPin(I, FLinkPin); FLastError := '';
      except on E: Exception do
      begin FLastError := E.Message; if Assigned(FOnConnectionError) then FOnConnectionError(Self, FLastError); end; end;
    FLinkPin := '';
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
