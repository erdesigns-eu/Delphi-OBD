unit ERD.Components.ShapeInspector;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, Winapi.GDIPAPI, Winapi.GDIPOBJ, Vcl.Controls, Vcl.Graphics, ERD.Components.Control, ERD.Components.Types,
  ERD.Components.Paint, ERD.Components.Edits, ERD.Components.ValueTree, ERD.Components.DebugModel;
type
  TERDShapeAvailability = (shaAvailable, shaUnavailable, shaError);
  TERDShapeInfo = record
    ID, ParentID, PrototypeShapeID, Depth, PropertyCount, RefCount, TransitionCount: Cardinal;
    DictionaryMode, TransitionCountAvailable: Boolean;
  end;
  TERDShapeProperty = record
    Name: string;
    Offset: Cardinal;
    Writable, Enumerable, Configurable, IsAccessor: Boolean;
  end;
  TERDShapeTransition = record
    PropertyName: string;
    TargetShapeID: Cardinal;
    IsAccessor: Boolean;
  end;
  TERDShapeRegistryStats = record
    Available, MaxDepthAvailable, DictionaryModeCountAvailable: Boolean;
    TotalShapes, InternedStrings, MaxDepth, DictionaryModeCount: Cardinal;
  end;
  TERDShapeSnapshot = record
    Info: TERDShapeInfo;
    Properties: TArray<TERDShapeProperty>;
    Transitions: TArray<TERDShapeTransition>;
    PropertiesAvailability, TransitionsAvailability: TERDShapeAvailability;
    PropertiesError, TransitionsError: string;
    Registry: TERDShapeRegistryStats;
  end;
  TERDShapeRequest = record
    Session, Generation, RequestID: Int64;
    ShapeID: Cardinal;
  end;
  TERDRequestShapeEvent = procedure(Sender: TObject; const Request: TERDShapeRequest) of object;
  TERDNavigateShapeEvent = procedure(Sender: TObject; const Context: TERDShapeRequest; TargetShapeID: Cardinal) of object;
  TERDInspectShapePropertyEvent = procedure(Sender: TObject; const Context: TERDShapeRequest; const Value: TERDShapeProperty) of object;
  TERDShapeInspector = class;
  TERDShapeMapNode = record
    Bounds: TRect;
    Target: Cardinal;
    Title, Detail: string;
    Current: Boolean;
  end;
  /// Owned diagram, showing only relationships actually supplied by the host.
  TERDShapeMap = class(TERDCustomControl)
  private
    FHost: TERDShapeInspector;
    FNodes: TArray<TERDShapeMapNode>;
    FPage, FFocus: Integer;
    procedure BuildNodes;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangePage(Direction: Integer);
    function NodeCount: Integer;
    function MapNode(Index: Integer; out Node: TERDShapeMapNode): Boolean;
    function ActivateNode(Index: Integer): Boolean;
    property TransitionPage: Integer read FPage;
  end;
  /// Read-only hidden-class layout. The host combines NDB domain replies into one snapshot.
  TERDShapeInspector = class(TERDCustomControl)
  private
    FModel: TERDDebugModel;
    FSummary, FProperties, FTransitions: TERDValueTree;
    FSearch: TERDEdit;
    FMap: TERDShapeMap;
    FSnapshot: TERDShapeSnapshot;
    FRequest, FPublished: TERDShapeRequest;
    FNextRequest, FViewGeneration: Int64;
    FMaxItems: Integer;
    FPending, FStale, FUpdating: Boolean;
    FSearchText, FErrorText, FExecutionUri: string;
    FExecutionLine: Integer;
    FExecutionState: TERDDebugState;
    FOnRequest: TERDRequestShapeEvent;
    FOnNavigate: TERDNavigateShapeEvent;
    FOnInspectProperty: TERDInspectShapePropertyEvent;
    procedure SetDebugModel(Value: TERDDebugModel);
    procedure ModelChanged(Sender: TObject);
    procedure SetSearchText(const Value: string);
    procedure SetMaxItems(Value: Integer);
    procedure SearchChanged(Sender: TObject);
    procedure SummaryInspect(Sender: TObject; const Value: TERDInspectValue);
    procedure PropertyInspect(Sender: TObject; const Value: TERDInspectValue);
    procedure TransitionInspect(Sender: TObject; const Value: TERDInspectValue);
    procedure RebuildViews;
    function RequestMatches(const Request: TERDShapeRequest): Boolean;
    function ValidSnapshot(const Snapshot: TERDShapeSnapshot): Boolean;
    procedure Navigate(Target: Cardinal);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    function RequestShape(Session, Generation: Int64; ShapeID: Cardinal): TERDShapeRequest;
    function PublishSnapshot(const Request: TERDShapeRequest; const Snapshot: TERDShapeSnapshot): Boolean;
    function FailSnapshot(const Request: TERDShapeRequest; const MessageText: string): Boolean;
    procedure MarkStale;
    procedure NavigateParent;
    procedure NavigatePrototype;
    procedure InspectSelectedProperty;
    procedure NavigateSelectedTransition;
    function ShapeInfo(out Info: TERDShapeInfo): Boolean;
    function ShapeProperties: TArray<TERDShapeProperty>;
    function ShapeTransitions: TArray<TERDShapeTransition>;
    property ShapeMap: TERDShapeMap read FMap;
    property SummaryTree: TERDValueTree read FSummary;
    property PropertyTree: TERDValueTree read FProperties;
    property TransitionTree: TERDValueTree read FTransitions;
    property Context: TERDShapeRequest read FPublished;
    property Pending: Boolean read FPending;
    property Stale: Boolean read FStale;
    property ErrorText: string read FErrorText;
  published
    property DebugModel: TERDDebugModel read FModel write SetDebugModel;
    property SearchText: string read FSearchText write SetSearchText stored False;
    property MaxItems: Integer read FMaxItems write SetMaxItems default 10000;
    property OnRequestShape: TERDRequestShapeEvent read FOnRequest write FOnRequest;
    property OnNavigateShape: TERDNavigateShapeEvent read FOnNavigate write FOnNavigate;
    property OnInspectProperty: TERDInspectShapePropertyEvent read FOnInspectProperty write FOnInspectProperty;
  end;
implementation
constructor TERDShapeMap.Create(AOwner: TComponent);
begin
  FHost := AOwner as TERDShapeInspector; inherited;
  TabStop := True; FFocus := 0; ShowHint := True;
  Hint := 'Click a shape to navigate. Left/right selects a card; Enter opens it. Page Up/Down or the corner arrows browse outgoing transitions.';
end;
procedure TERDShapeMap.WMGetDlgCode(var Message: TWMGetDlgCode);
begin inherited; Message.Result := Message.Result or DLGC_WANTARROWS; end;
procedure TERDShapeMap.CMDialogKey(var Message: TCMDialogKey);
begin
  if Focused and (Message.CharCode = VK_RETURN) then begin ActivateNode(FFocus); Message.Result := 1; end else inherited;
end;
procedure TERDShapeMap.DoEnter;
begin inherited; Invalidate; end;
procedure TERDShapeMap.DoExit;
begin inherited; Invalidate; end;
procedure TERDShapeMap.BuildNodes;
var W, H, CardW, CardH, Gap, Center, I, LastPage, Index, X: Integer; R: TRect;
  procedure Add(const Bounds: TRect; Target: Cardinal; const Title, Detail: string; Current: Boolean = False);
  var N: Integer;
  begin
    N := Length(FNodes); SetLength(FNodes,N+1); FNodes[N].Bounds := Bounds;
    FNodes[N].Target := Target; FNodes[N].Title := Title; FNodes[N].Detail := Detail; FNodes[N].Current := Current;
  end;
begin
  FNodes := nil; if (FHost = nil) or (FHost.FPublished.RequestID = 0) then Exit;
  W := ClientWidth; H := ClientHeight; if (W < ScaleValue(240)) or (H < ScaleValue(180)) then Exit; Gap := ScaleValue(8); CardW := Max(1,Min(ScaleValue(160),(W-4*Gap) div 3));
  CardH := Max(1,Min(ScaleValue(54),(H-ScaleValue(96)) div 3)); Center := W div 2;
  R := Rect(Center-CardW div 2,H div 2-CardH div 2,Center-CardW div 2+CardW,H div 2-CardH div 2+CardH);
  Add(R,FHost.FSnapshot.Info.ID,'Shape '+UIntToStr(FHost.FSnapshot.Info.ID),UIntToStr(FHost.FSnapshot.Info.PropertyCount)+' properties',True);
  if FHost.FSnapshot.Info.Depth > 0 then Add(Rect(Gap,ScaleValue(24),Gap+CardW,ScaleValue(24)+CardH),FHost.FSnapshot.Info.ParentID,'Parent '+UIntToStr(FHost.FSnapshot.Info.ParentID),'previous layout');
  if FHost.FSnapshot.Info.PrototypeShapeID <> 0 then Add(Rect(W-Gap-CardW,ScaleValue(24),W-Gap,ScaleValue(24)+CardH),FHost.FSnapshot.Info.PrototypeShapeID,'Prototype '+UIntToStr(FHost.FSnapshot.Info.PrototypeShapeID),'prototype link');
  LastPage := Max(0,(Length(FHost.FSnapshot.Transitions)-1) div 3); FPage := EnsureRange(FPage,0,LastPage);
  if FHost.FSnapshot.TransitionsAvailability = shaAvailable then for I := 0 to 2 do
  begin
    Index := FPage*3+I; if Index >= Length(FHost.FSnapshot.Transitions) then Break;
    X := Center+((I-1)*(CardW+Gap))-CardW div 2;
    Add(Rect(X,H-Gap-CardH-ScaleValue(16),X+CardW,H-Gap-ScaleValue(16)),FHost.FSnapshot.Transitions[Index].TargetShapeID,
      'Shape '+UIntToStr(FHost.FSnapshot.Transitions[Index].TargetShapeID),'add '+FHost.FSnapshot.Transitions[Index].PropertyName);
  end;
  FFocus := EnsureRange(FFocus,0,High(FNodes));
end;
function TERDShapeMap.NodeCount: Integer;
begin BuildNodes; Result := Length(FNodes); end;
function TERDShapeMap.MapNode(Index: Integer; out Node: TERDShapeMapNode): Boolean;
begin
  BuildNodes; Node := Default(TERDShapeMapNode); Result := (Index >= 0) and (Index < Length(FNodes)); if Result then Node := FNodes[Index];
end;
function TERDShapeMap.ActivateNode(Index: Integer): Boolean;
var Node: TERDShapeMapNode;
begin
  Result := MapNode(Index,Node) and not FHost.Stale; if not Result then Exit;
  FFocus := Index; Invalidate; if not Node.Current then FHost.Navigate(Node.Target);
end;
procedure TERDShapeMap.ChangePage(Direction: Integer);
begin
  if FHost.FSnapshot.TransitionsAvailability <> shaAvailable then Exit;
  FPage := EnsureRange(FPage+Direction,0,Max(0,(Length(FHost.FSnapshot.Transitions)-1) div 3)); FFocus := 0; Invalidate;
end;
procedure TERDShapeMap.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var I: Integer;
begin
  inherited; if Button <> mbLeft then Exit; if CanFocus then SetFocus;
  if Y < ScaleValue(22) then
  begin
    if X > ClientWidth-ScaleValue(28) then ChangePage(1)
    else if X > ClientWidth-ScaleValue(56) then ChangePage(-1);
    Exit;
  end;
  BuildNodes; for I := 0 to High(FNodes) do if PtInRect(FNodes[I].Bounds,Point(X,Y)) then begin ActivateNode(I); Exit; end;
end;
procedure TERDShapeMap.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then case Key of
    VK_LEFT: begin BuildNodes; FFocus := Max(0,FFocus-1); Invalidate; Key := 0; end;
    VK_RIGHT: begin BuildNodes; FFocus := Max(0,Min(High(FNodes),FFocus+1)); Invalidate; Key := 0; end;
    VK_RETURN: begin ActivateNode(FFocus); Key := 0; end;
    VK_PRIOR: begin ChangePage(-1); Key := 0; end;
    VK_NEXT: begin ChangePage(1); Key := 0; end;
  end;
  inherited;
end;
procedure TERDShapeMap.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Path: TGPGraphicsPath; I: Integer; FromR, ToR: TRect;
    Ink, Edge, Fill: TColor; X1, Y1, X2, Y2: Single; Caption: string;
begin
  BuildNodes; Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background); Ink := Palette.ForegroundText;
    if FHost.Stale then Ink := Palette.Subtle;
    Painter.TextF(ScaleValue(8),ScaleValue(12),'Shape relationships',11,Ink,twSemibold,taLeftJustify,Max(0,ClientWidth-ScaleValue(70)));
    if Length(FHost.FSnapshot.Transitions) > 3 then
    begin
      Painter.TextF(ClientWidth-ScaleValue(48),ScaleValue(12),'<',12,Ink,twBold);
      Painter.TextF(ClientWidth-ScaleValue(20),ScaleValue(12),'>',12,Ink,twBold);
    end;
    if Length(FNodes) = 0 then
    begin
      if FHost.FPublished.RequestID > 0 then Caption := 'Enlarge the view to show relationships' else Caption := 'Request a shape to show relationships';
      Painter.TextF(ClientWidth/2,ClientHeight/2,Caption,11,Palette.Subtle,twRegular,taCenter,Max(0,ClientWidth-ScaleValue(16))); Exit;
    end;
    FromR := FNodes[0].Bounds;
    for I := 1 to High(FNodes) do
    begin
      ToR := FNodes[I].Bounds; X1 := (FromR.Left+FromR.Right)/2; X2 := (ToR.Left+ToR.Right)/2;
      if ToR.Top < FromR.Top then begin Y1 := FromR.Top; Y2 := ToR.Bottom; end
      else begin Y1 := FromR.Bottom; Y2 := ToR.Top; end;
      Path := TGPGraphicsPath.Create;
      try
        Path.AddBezier(X1,Y1,X1,(Y1+Y2)/2,X2,(Y1+Y2)/2,X2,Y2);
        Painter.DrawPath(Path,clNone,Palette.Subtle,ScaleValue(1));
      finally Path.Free; end;
    end;
    for I := 0 to High(FNodes) do
    begin
      ToR := FNodes[I].Bounds; Edge := Palette.NeutralLight; Fill := Palette.GaugeFace;
      if FNodes[I].Current and not FHost.Stale then begin Edge := Painter.AccentText; Fill := Painter.Tint(Palette.Accent); end;
      if Focused and (I = FFocus) then Edge := Painter.AccentText;
      Painter.RoundRect(ToR.Left+0.5,ToR.Top+0.5,Max(0,ToR.Width-1),Max(0,ToR.Height-1),ScaleValue(4),Fill,Edge);
      Painter.TextF((ToR.Left+ToR.Right)/2,ToR.Top+ToR.Height*0.33,FNodes[I].Title,11,Ink,twSemibold,taCenter,Max(0,ToR.Width-ScaleValue(10)));
      Painter.TextF((ToR.Left+ToR.Right)/2,ToR.Top+ToR.Height*0.72,FNodes[I].Detail,10,Ink,twRegular,taCenter,Max(0,ToR.Width-ScaleValue(10)));
    end;
    Caption := '';
    if FHost.FSnapshot.TransitionsAvailability <> shaAvailable then Caption := 'Outgoing transitions unavailable'
    else if Length(FHost.FSnapshot.Transitions) = 0 then Caption := 'No outgoing transitions'
    else if Length(FHost.FSnapshot.Transitions) > 3 then Caption := 'Page '+IntToStr(FPage+1)+' / '+IntToStr((Length(FHost.FSnapshot.Transitions)+2) div 3);
    if Caption <> '' then Painter.TextF(ClientWidth/2,ClientHeight-ScaleValue(8),Caption,10,Palette.Subtle,twRegular,taCenter,ClientWidth);
  finally Painter.Free; end;
end;

constructor TERDShapeInspector.Create(AOwner: TComponent);
begin
  inherited; Width := 760; Height := 640; TabStop := False;
  FMaxItems := 10000;
  FStale := True; FRequest.Generation := -1;
  FSummary := TERDValueTree.Create(Self); FSummary.Parent := Self; FSummary.OnInspectValue := SummaryInspect;
  FProperties := TERDValueTree.Create(Self); FProperties.Parent := Self; FProperties.OnInspectValue := PropertyInspect;
  FTransitions := TERDValueTree.Create(Self); FTransitions.Parent := Self; FTransitions.OnInspectValue := TransitionInspect;
  FSearch := TERDEdit.Create(Self); FSearch.Parent := Self; FSearch.TextHint := 'Filter property / transition details'; FSearch.OnChange := SearchChanged;
  FSummary.Header.Columns[0].Text := 'Shape / registry'; FSummary.Header.Columns[2].Text := 'Meaning';
  FProperties.Header.Columns[0].Text := 'Property'; FProperties.Header.Columns[1].Text := 'Slot offset'; FProperties.Header.Columns[2].Text := 'Attributes';
  FTransitions.Header.Columns[0].Text := 'Transition'; FTransitions.Header.Columns[1].Text := 'Target shape'; FTransitions.Header.Columns[2].Text := 'Kind';
  FSummary.Header.Columns[0].Width := 140; FSummary.Header.Columns[1].Width := 120;
  FProperties.Header.Columns[0].Width := 140; FProperties.Header.Columns[1].Width := 100; FProperties.Header.Columns[2].Width := 320;
  FTransitions.Header.Columns[0].Width := 140; FTransitions.Header.Columns[1].Width := 100; FTransitions.Header.Columns[2].Width := 100;
  FMap := TERDShapeMap.Create(Self); FMap.Parent := Self;
  ThemeChanged; Resize;
end;
destructor TERDShapeInspector.Destroy;
begin
  SetDebugModel(nil);
  if FSummary <> nil then FSummary.OnInspectValue := nil;
  if FProperties <> nil then FProperties.OnInspectValue := nil;
  if FTransitions <> nil then FTransitions.OnInspectValue := nil;
  if FSearch <> nil then FSearch.OnChange := nil;
  inherited;
end;
procedure TERDShapeInspector.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
procedure TERDShapeInspector.SetDebugModel(Value: TERDDebugModel);
begin
  if FModel = Value then Exit;
  if FModel <> nil then begin FModel.RemoveListener(ModelChanged); FModel.RemoveFreeNotification(Self); end;
  FModel := Value;
  if Value <> nil then begin Value.FreeNotification(Self); Value.AddListener(ModelChanged); end;
  FRequest.Generation := -1; MarkStale;
end;
procedure TERDShapeInspector.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FModel) then begin FModel := nil; MarkStale; end; end;
procedure TERDShapeInspector.ModelChanged(Sender: TObject);
begin
  if (FModel <> nil) and ((FModel.State = dsDisconnected) or (FModel.Session <> FRequest.Session) or
    (FModel.State <> FExecutionState) or (FModel.ExecutionUri <> FExecutionUri) or (FModel.ExecutionLine <> FExecutionLine)) then MarkStale;
end;
procedure TERDShapeInspector.MarkStale;
begin
  FPending := False; FStale := True; FRequest.RequestID := 0;
  if FSummary <> nil then FSummary.MarkStale;
  if FProperties <> nil then FProperties.MarkStale;
  if FTransitions <> nil then FTransitions.MarkStale;
  if FMap <> nil then FMap.Invalidate;
  Invalidate;
end;
function TERDShapeInspector.RequestShape(Session, Generation: Int64; ShapeID: Cardinal): TERDShapeRequest;
begin
  Result := Default(TERDShapeRequest);
  if (Session < 0) or (Generation < 0) then Exit;
  if (FModel <> nil) and ((FModel.State = dsDisconnected) or (FModel.Session <> Session)) then Exit;
  if (Session = FRequest.Session) and (Generation <= FRequest.Generation) then Exit;
  MarkStale; Inc(FNextRequest); FRequest.Session := Session; FRequest.Generation := Generation;
  FRequest.RequestID := FNextRequest; FRequest.ShapeID := ShapeID; FPending := True; FErrorText := '';
  if FModel <> nil then begin FExecutionState := FModel.State; FExecutionUri := FModel.ExecutionUri; FExecutionLine := FModel.ExecutionLine; end;
  Result := FRequest; Invalidate; if Assigned(FOnRequest) then FOnRequest(Self,Result);
end;
function TERDShapeInspector.RequestMatches(const Request: TERDShapeRequest): Boolean;
begin
  Result := FPending and (Request.RequestID > 0) and (Request.RequestID = FRequest.RequestID) and
    (Request.Session = FRequest.Session) and (Request.Generation = FRequest.Generation) and (Request.ShapeID = FRequest.ShapeID);
end;
function TERDShapeInspector.ValidSnapshot(const Snapshot: TERDShapeSnapshot): Boolean;
var Names: TDictionary<string,Boolean>; Offsets: TDictionary<Cardinal,Boolean>;
    PropertyInfo: TERDShapeProperty; Transition: TERDShapeTransition; Key: string;
begin
  Result := False;
  if (Length(Snapshot.Properties) > FMaxItems) or (Length(Snapshot.Transitions) > FMaxItems) then Exit;
  if (Ord(Snapshot.PropertiesAvailability) > Ord(High(TERDShapeAvailability))) or
    (Ord(Snapshot.TransitionsAvailability) > Ord(High(TERDShapeAvailability))) then Exit;
  if Snapshot.PropertiesAvailability = shaAvailable then
  begin if Cardinal(Length(Snapshot.Properties)) <> Snapshot.Info.PropertyCount then Exit; end
  else if Length(Snapshot.Properties) <> 0 then Exit;
  if Snapshot.TransitionsAvailability <> shaAvailable then
  begin if Length(Snapshot.Transitions) <> 0 then Exit; end
  else if Snapshot.Info.TransitionCountAvailable and (Cardinal(Length(Snapshot.Transitions)) <> Snapshot.Info.TransitionCount) then Exit;
  Names := TDictionary<string,Boolean>.Create; Offsets := TDictionary<Cardinal,Boolean>.Create;
  try
    for PropertyInfo in Snapshot.Properties do
    begin
      // Empty JavaScript property names are valid. Names are case-sensitive.
      if Names.ContainsKey(PropertyInfo.Name) or Offsets.ContainsKey(PropertyInfo.Offset) then Exit;
      Names.Add(PropertyInfo.Name,True); Offsets.Add(PropertyInfo.Offset,True);
    end;
    Names.Clear;
    for Transition in Snapshot.Transitions do
    begin
      Key := IntToStr(Ord(Transition.IsAccessor))+':'+UIntToStr(Transition.TargetShapeID)+':'+Transition.PropertyName;
      if Names.ContainsKey(Key) or (Transition.TargetShapeID = Snapshot.Info.ID) then Exit;
      Names.Add(Key,True);
    end;
  finally Names.Free; Offsets.Free; end;
  Result := True;
end;
function TERDShapeInspector.PublishSnapshot(const Request: TERDShapeRequest; const Snapshot: TERDShapeSnapshot): Boolean;
begin
  Result := False; if not RequestMatches(Request) or (Snapshot.Info.ID <> Request.ShapeID) or not ValidSnapshot(Snapshot) then Exit;
  FSnapshot := Snapshot; FSnapshot.Properties := Copy(Snapshot.Properties); FSnapshot.Transitions := Copy(Snapshot.Transitions);
  if (FPublished.Session <> Request.Session) or (FPublished.ShapeID <> Request.ShapeID) then begin FMap.FPage := 0; FMap.FFocus := 0; end;
  FPublished := Request; FPending := False; FStale := False; FErrorText := ''; RebuildViews; Invalidate; Result := True;
end;
function TERDShapeInspector.FailSnapshot(const Request: TERDShapeRequest; const MessageText: string): Boolean;
begin
  Result := RequestMatches(Request); if not Result then Exit;
  MarkStale; FErrorText := MessageText; if FErrorText = '' then FErrorText := 'Shape unavailable'; Invalidate;
end;
procedure TERDShapeInspector.SetMaxItems(Value: Integer);
begin
  if (Value < 1) or (Value > 100000) then raise EArgumentOutOfRangeException.Create('MaxItems must be 1..100000');
  if (Length(FSnapshot.Properties) > Value) or (Length(FSnapshot.Transitions) > Value) then raise EArgumentException.Create('Clear or replace the shape before lowering MaxItems');
  FMaxItems := Value;
end;
procedure TERDShapeInspector.SetSearchText(const Value: string);
begin if FSearchText = Value then Exit; FSearchText := Value; RebuildViews; end;
procedure TERDShapeInspector.SearchChanged(Sender: TObject);
begin if not FUpdating then SetSearchText(FSearch.Text); end;
procedure TERDShapeInspector.RebuildViews;
var Summary, Props, Edges: TArray<TERDInspectValue>; Value: TERDInspectValue;
    I, Count: Integer; Identity, Query, Attributes: string; PropertyInfo: TERDShapeProperty; Transition: TERDShapeTransition;
  procedure AddSummary(const ID, Name, Text, Meaning: string; Availability: TERDValueAvailability = vaAvailable);
  var N: Integer;
  begin
    N := Length(Summary); SetLength(Summary,N+1); Summary[N].ID := ID; Summary[N].Name := Name;
    Summary[N].ValueText := Text; Summary[N].TypeName := Meaning; Summary[N].Availability := Availability;
  end;
  procedure UnavailableRow(var Rows: TArray<TERDInspectValue>; Availability: TERDShapeAvailability; const ErrorText: string);
  begin
    SetLength(Rows,1); Rows[0] := Default(TERDInspectValue); Rows[0].ID := 'status';
    Rows[0].Name := 'Not available'; Rows[0].ValueText := ErrorText; Rows[0].Availability := vaUnavailable;
    if Availability = shaError then begin Rows[0].Name := 'Error'; Rows[0].Availability := vaError; end;
  end;
begin
  if (FSummary = nil) or (FPublished.RequestID = 0) then Exit;
  Identity := IntToStr(FPublished.Session)+':shape:'+UIntToStr(FPublished.ShapeID); Inc(FViewGeneration);
  AddSummary('id','Shape ID',UIntToStr(FSnapshot.Info.ID),'hidden class');
  AddSummary('parent','Parent shape',UIntToStr(FSnapshot.Info.ParentID),'Enter / double-click to navigate');
  AddSummary('prototype','Prototype shape',UIntToStr(FSnapshot.Info.PrototypeShapeID),'0 = none');
  AddSummary('depth','Depth',UIntToStr(FSnapshot.Info.Depth),'shape tree');
  AddSummary('propertyCount','Properties',UIntToStr(FSnapshot.Info.PropertyCount),'layout entries');
  AddSummary('refCount','Live objects',UIntToStr(FSnapshot.Info.RefCount),'heap observation');
  AddSummary('dictionary','Dictionary mode',BoolToStr(FSnapshot.Info.DictionaryMode,True),'shape storage');
  if FSnapshot.Info.TransitionCountAvailable then AddSummary('transitionCount','Transitions',UIntToStr(FSnapshot.Info.TransitionCount),'outgoing')
  else AddSummary('transitionCount','Transitions','<unavailable>','not supplied by provider',vaUnavailable);
  if FSnapshot.Registry.Available then
  begin
    AddSummary('totalShapes','Registry shapes',UIntToStr(FSnapshot.Registry.TotalShapes),'registry');
    AddSummary('internedStrings','Interned strings',UIntToStr(FSnapshot.Registry.InternedStrings),'registry');
    if FSnapshot.Registry.MaxDepthAvailable then AddSummary('maxDepth','Maximum depth',UIntToStr(FSnapshot.Registry.MaxDepth),'registry')
    else AddSummary('maxDepth','Maximum depth','<unavailable>','provider placeholder',vaUnavailable);
    if FSnapshot.Registry.DictionaryModeCountAvailable then AddSummary('dictionaryCount','Dictionary shapes',UIntToStr(FSnapshot.Registry.DictionaryModeCount),'registry')
    else AddSummary('dictionaryCount','Dictionary shapes','<unavailable>','provider placeholder',vaUnavailable);
  end
  else AddSummary('registry','Registry','<unavailable>','not supplied by provider',vaUnavailable);
  Query := UpperCase(FSearchText); SetLength(Props,Length(FSnapshot.Properties)); Count := 0;
  for I := 0 to High(FSnapshot.Properties) do
  begin
    PropertyInfo := FSnapshot.Properties[I];
    if (Query <> '') and (Pos(Query,UpperCase(PropertyInfo.Name+' '+UIntToStr(PropertyInfo.Offset))) = 0) then Continue;
    Attributes := 'W='+BoolToStr(PropertyInfo.Writable,True)+' E='+BoolToStr(PropertyInfo.Enumerable,True)+' C='+BoolToStr(PropertyInfo.Configurable,True);
    if PropertyInfo.IsAccessor then Attributes := 'accessor; '+Attributes else Attributes := 'data; '+Attributes;
    Value := Default(TERDInspectValue); Value.ID := 'property:'+IntToStr(I); Value.Name := PropertyInfo.Name;
    if Value.Name = '' then Value.Name := '<empty name>';
    Value.ValueText := UIntToStr(PropertyInfo.Offset); Value.TypeName := Attributes; Value.Reference := I;
    Props[Count] := Value; Inc(Count);
  end;
  SetLength(Props,Count);
  if FSnapshot.PropertiesAvailability <> shaAvailable then UnavailableRow(Props,FSnapshot.PropertiesAvailability,FSnapshot.PropertiesError);
  SetLength(Edges,Length(FSnapshot.Transitions)); Count := 0;
  for I := 0 to High(FSnapshot.Transitions) do
  begin
    Transition := FSnapshot.Transitions[I];
    if (Query <> '') and (Pos(Query,UpperCase(Transition.PropertyName+' '+UIntToStr(Transition.TargetShapeID))) = 0) then Continue;
    Value := Default(TERDInspectValue); Value.ID := 'transition:'+IntToStr(I); Value.Name := Transition.PropertyName;
    if Value.Name = '' then Value.Name := '<empty name>';
    Value.ValueText := UIntToStr(Transition.TargetShapeID); Value.Reference := I;
    if Transition.IsAccessor then Value.TypeName := 'accessor' else Value.TypeName := 'data';
    Edges[Count] := Value; Inc(Count);
  end;
  SetLength(Edges,Count);
  if FSnapshot.TransitionsAvailability <> shaAvailable then UnavailableRow(Edges,FSnapshot.TransitionsAvailability,FSnapshot.TransitionsError);
  FUpdating := True;
  try
    FSearch.Text := FSearchText;
    FSummary.LoadSnapshot(Identity,FViewGeneration,Summary); FProperties.LoadSnapshot(Identity,FViewGeneration,Props); FTransitions.LoadSnapshot(Identity,FViewGeneration,Edges);
    if FStale then begin FSummary.MarkStale; FProperties.MarkStale; FTransitions.MarkStale; end;
  finally FUpdating := False; end;
  if FMap <> nil then FMap.Invalidate;
end;
procedure TERDShapeInspector.Navigate(Target: Cardinal);
begin if not FStale and Assigned(FOnNavigate) then FOnNavigate(Self,FPublished,Target); end;
procedure TERDShapeInspector.NavigateParent;
begin if FSnapshot.Info.Depth > 0 then Navigate(FSnapshot.Info.ParentID); end;
procedure TERDShapeInspector.NavigatePrototype;
begin if FSnapshot.Info.PrototypeShapeID <> 0 then Navigate(FSnapshot.Info.PrototypeShapeID); end;
procedure TERDShapeInspector.SummaryInspect(Sender: TObject; const Value: TERDInspectValue);
begin if Value.ID = 'parent' then NavigateParent else if Value.ID = 'prototype' then NavigatePrototype; end;
procedure TERDShapeInspector.PropertyInspect(Sender: TObject; const Value: TERDInspectValue);
var Index: Integer;
begin
  if FStale or (Value.Availability <> vaAvailable) or (Pos('property:',Value.ID) <> 1) then Exit;
  Index := StrToIntDef(Copy(Value.ID,10,MaxInt),-1);
  if (Index >= 0) and (Index < Length(FSnapshot.Properties)) and Assigned(FOnInspectProperty) then FOnInspectProperty(Self,FPublished,FSnapshot.Properties[Index]);
end;
procedure TERDShapeInspector.TransitionInspect(Sender: TObject; const Value: TERDInspectValue);
var Index: Integer;
begin
  if FStale or (Value.Availability <> vaAvailable) or (Pos('transition:',Value.ID) <> 1) then Exit;
  Index := StrToIntDef(Copy(Value.ID,12,MaxInt),-1);
  if (Index >= 0) and (Index < Length(FSnapshot.Transitions)) then Navigate(FSnapshot.Transitions[Index].TargetShapeID);
end;
procedure TERDShapeInspector.InspectSelectedProperty;
var Value: TERDInspectValue;
begin if FProperties.SelectedValue(Value) then PropertyInspect(Self,Value); end;
procedure TERDShapeInspector.NavigateSelectedTransition;
var Value: TERDInspectValue;
begin if FTransitions.SelectedValue(Value) then TransitionInspect(Self,Value); end;
function TERDShapeInspector.ShapeInfo(out Info: TERDShapeInfo): Boolean;
begin Info := FSnapshot.Info; Result := not FStale and (FPublished.RequestID > 0); end;
function TERDShapeInspector.ShapeProperties: TArray<TERDShapeProperty>;
begin Result := Copy(FSnapshot.Properties); end;
function TERDShapeInspector.ShapeTransitions: TArray<TERDShapeTransition>;
begin Result := Copy(FSnapshot.Transitions); end;
procedure TERDShapeInspector.ThemeChanged;
begin
  inherited;
  if FMap <> nil then begin FMap.Theme := Theme; FMap.ThemeChanged; end;
  if FSummary <> nil then begin FSummary.Theme := Theme; FSummary.ThemeChanged; end;
  if FProperties <> nil then begin FProperties.Theme := Theme; FProperties.ThemeChanged; end;
  if FTransitions <> nil then begin FTransitions.Theme := Theme; FTransitions.ThemeChanged; end;
  if FSearch <> nil then begin FSearch.Theme := Theme; FSearch.ThemeChanged; end;
end;
procedure TERDShapeInspector.DensityChanged;
begin
  inherited;
  if FMap <> nil then begin FMap.ParentDensity := False; FMap.Density := Density; end;
  if FSummary <> nil then begin FSummary.ParentDensity := False; FSummary.Density := Density; end;
  if FProperties <> nil then begin FProperties.ParentDensity := False; FProperties.Density := Density; end;
  if FTransitions <> nil then begin FTransitions.ParentDensity := False; FTransitions.Density := Density; end;
  if FSearch <> nil then begin FSearch.ParentDensity := False; FSearch.Density := Density; end;
  Resize;
end;
procedure TERDShapeInspector.Resize;
var Top, H, Half, MapW: Integer;
begin
  inherited; if (FSearch = nil) or (FTransitions = nil) or (FMap = nil) then Exit;
  Top := ScaleValue(48); H := Max(0,(ClientHeight-Top-ScaleValue(Metrics.Edit+8)) div 2);
  MapW := Max(0,ClientWidth-Max(ScaleValue(220),ClientWidth div 3));
  FMap.SetBounds(0,Top,MapW,H); FSummary.SetBounds(MapW,Top,ClientWidth-MapW,H); Inc(Top,H+ScaleValue(4));
  FSearch.SetBounds(ScaleValue(6),Top,Max(0,ClientWidth-ScaleValue(12)),ScaleValue(Metrics.Edit)); Inc(Top,ScaleValue(Metrics.Edit+4));
  if ClientWidth >= ScaleValue(600) then
  begin
    Half := ClientWidth div 2; FProperties.SetBounds(0,Top,Half,Max(0,ClientHeight-Top));
    FTransitions.SetBounds(Half,Top,ClientWidth-Half,Max(0,ClientHeight-Top));
  end
  else
  begin
    H := Max(0,(ClientHeight-Top) div 2); FProperties.SetBounds(0,Top,ClientWidth,H);
    FTransitions.SetBounds(0,Top+H,ClientWidth,Max(0,ClientHeight-Top-H));
  end;
end;
procedure TERDShapeInspector.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; Status: string;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background);
    if FPending then Status := 'Loading shape '+UIntToStr(FRequest.ShapeID)+'...'
    else if FErrorText <> '' then Status := 'Error: '+FErrorText
    else if FStale then Status := 'Stale - request a shape to inspect'
    else Status := 'Shape '+UIntToStr(FPublished.ShapeID)+' | double-click parent, prototype or a transition to navigate';
    Painter.TextF(ScaleValue(8),ScaleValue(13),'Shape inspector',12,Palette.ForegroundText,twSemibold,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
    Painter.TextF(ScaleValue(8),ScaleValue(33),Status,11,Palette.Subtle,twRegular,taLeftJustify,Max(0,ClientWidth-ScaleValue(16)));
  finally Painter.Free; end;
end;
end.
