unit ShapePreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, Vcl.Controls, Vcl.StdCtrls,
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.ShapeInspector,
  ERD.Components.DebugModel, ERD.Components.Ribbon;
type
  TShapePreview = class(TERDPanel)
  private
    FInspector: TERDShapeInspector;
    FModel: TERDDebugModel;
    FStatus: TLabel;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FGeneration: Int64;
    FShapeID: Cardinal;
    FPlaceholders, FFailNext: Boolean;
    procedure RequestData(Sender: TObject; const Request: TERDShapeRequest);
    procedure Navigate(Sender: TObject; const Context: TERDShapeRequest; TargetShapeID: Cardinal);
    procedure Inspect(Sender: TObject; const Context: TERDShapeRequest; const Value: TERDShapeProperty);
    procedure Fetch;
    procedure NewSession;
    procedure RibbonClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged; override;
    procedure AttachRibbon(Value: TERDRibbon);
    procedure SetRibbonActive(Value: Boolean);
  end;
implementation
constructor TShapePreview.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TERDDebugModel.Create(Self);
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop; FStatus.AutoSize := False; FStatus.Height := ScaleValue(30);
  FStatus.Caption := 'DEMO DATA - synthetic shape relationships. No TSVM connected; property layouts are not object values.';
  FInspector := TERDShapeInspector.Create(Self); FInspector.Parent := Self; FInspector.Align := alClient; FInspector.DebugModel := FModel;
  FInspector.OnRequestShape := RequestData; FInspector.OnNavigateShape := Navigate; FInspector.OnInspectProperty := Inspect;
  NewSession; ThemeChanged;
end;
destructor TShapePreview.Destroy;
begin
  AttachRibbon(nil);
  if FInspector <> nil then begin FInspector.OnRequestShape := nil; FInspector.OnNavigateShape := nil; FInspector.OnInspectProperty := nil; end;
  inherited;
end;
procedure TShapePreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText;
  if FInspector <> nil then begin FInspector.Theme := Theme; FInspector.ThemeChanged; end;
end;
procedure TShapePreview.NewSession;
begin FModel.BeginSession; FModel.SetExecution(dsPaused); FGeneration := 0; FShapeID := 2; FFailNext := False; Fetch; end;
procedure TShapePreview.Fetch;
begin Inc(FGeneration); FInspector.RequestShape(FModel.Session,FGeneration,FShapeID); end;
procedure TShapePreview.RequestData(Sender: TObject; const Request: TERDShapeRequest);
var Snapshot: TERDShapeSnapshot; I: Integer;
const Names: array[0..2] of string = ('name','ledCount','enabled');
begin
  if FFailNext then begin FFailNext := False; FInspector.FailSnapshot(Request,'Synthetic provider failure - Refresh to retry'); Exit; end;
  Snapshot := Default(TERDShapeSnapshot); Snapshot.Info.ID := Request.ShapeID;
  Snapshot.Info.ParentID := 0; if Request.ShapeID > 0 then Snapshot.Info.Depth := 1;
  if Request.ShapeID <> 6 then Snapshot.Info.PrototypeShapeID := 6;
  Snapshot.Info.PropertyCount := 3; Snapshot.Info.RefCount := 10+Request.ShapeID; Snapshot.Info.DictionaryMode := Request.ShapeID = 5;
  SetLength(Snapshot.Properties,3);
  for I := 0 to 2 do
  begin
    Snapshot.Properties[I].Name := Names[I]; Snapshot.Properties[I].Offset := I;
    Snapshot.Properties[I].Writable := I <> 0; Snapshot.Properties[I].Enumerable := True;
    Snapshot.Properties[I].Configurable := True; Snapshot.Properties[I].IsAccessor := I = 2;
  end;
  Snapshot.Registry.Available := True; Snapshot.Registry.TotalShapes := 7; Snapshot.Registry.InternedStrings := 19;
  Snapshot.Registry.MaxDepthAvailable := not FPlaceholders; Snapshot.Registry.MaxDepth := 1;
  Snapshot.Registry.DictionaryModeCountAvailable := not FPlaceholders; Snapshot.Registry.DictionaryModeCount := 1;
  Snapshot.Info.TransitionCountAvailable := not FPlaceholders;
  if FPlaceholders then begin Snapshot.TransitionsAvailability := shaUnavailable; Snapshot.TransitionsError := 'Current NDB endpoint returns a placeholder empty array'; end
  else
  begin
    Snapshot.Info.TransitionCount := 5; SetLength(Snapshot.Transitions,5);
    for I := 0 to 4 do begin Snapshot.Transitions[I].PropertyName := 'extra'+IntToStr(I+1); Snapshot.Transitions[I].TargetShapeID := (Request.ShapeID+Cardinal(I)+1) mod 7; end;
  end;
  FInspector.PublishSnapshot(Request,Snapshot);
end;
procedure TShapePreview.Navigate(Sender: TObject; const Context: TERDShapeRequest; TargetShapeID: Cardinal);
begin FShapeID := TargetShapeID; Fetch; end;
procedure TShapePreview.Inspect(Sender: TObject; const Context: TERDShapeRequest; const Value: TERDShapeProperty);
begin FStatus.Caption := 'DEMO property '+Value.Name+' | slot '+UIntToStr(Value.Offset)+' | layout metadata only, no object value'; end;
procedure TShapePreview.RibbonClick(Sender: TObject);
begin
  case TERDRibbonItem(Sender).Tag of
    0: Fetch;
    1: FInspector.NavigateParent;
    2: FInspector.NavigatePrototype;
    3: FInspector.ShapeMap.ChangePage(-1);
    4: FInspector.ShapeMap.ChangePage(1);
    5: begin FPlaceholders := not FPlaceholders; Fetch; end;
    6: begin FFailNext := True; Fetch; end;
    7: FModel.SetExecution(dsDisconnected);
    8: NewSession;
    9: FInspector.SearchText := '';
  end;
end;
procedure TShapePreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..9] of string = ('Refresh','Parent','Prototype','Previous edges','Next edges','NDB placeholders','Provider error','Disconnect','New session','Clear filter');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Shapes (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Shape navigation';
  for I := 0 to High(Captions) do
  begin
    if I = 5 then begin Group := FTab.Groups.Add; Group.Caption := 'Synthetic provider'; end;
    Item := Group.Items.Add; Item.Caption := Captions[I]; Item.Size := risSmall; Item.Tag := I; Item.OnClick := RibbonClick;
  end;
end;
procedure TShapePreview.SetRibbonActive(Value: Boolean);
var I, Index: Integer;
begin
  if (FRibbon = nil) or (FTab = nil) then Exit;
  Index := 0; for I := 0 to FTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(Index);
  if not Value and FTab.Visible and (FRibbon.ActiveTab = Index) then FRibbon.ActiveTab := 0;
  FTab.Visible := Value; if Value then FRibbon.ActiveTab := Index;
end;
procedure TShapePreview.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FRibbon) then begin FRibbon := nil; FTab := nil; end; end;
end.
