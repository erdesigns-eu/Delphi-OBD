unit ERD.Components.PCBInspector;
interface
uses System.SysUtils, ERD.Components.Types, ERD.Components.PCBScene, ERD.Components.Inspector;
type
  /// Optional host adapter. Owns no scene or inspector and installs no events.
  TERDPCBInspectorBinding = class
  private
    FScene: TERDPCBScene;
    FInspector: TERDInspector;
    FItemID, FTypeID: string;
    FProperties: TArray<TERDPCBPropertyDefinition>;
    function ItemIndex: Integer;
    function PropertyIndex(Row: TERDInspectorProperty): Integer;
  public
    procedure Clear;
    procedure Bind(Scene: TERDPCBScene; const ItemID: string; Inspector: TERDInspector);
    function Handles(Row: TERDInspectorProperty): Boolean;
    procedure Validate(Row: TERDInspectorProperty; const NewValue: string);
    procedure Apply(Row: TERDInspectorProperty);
    procedure Refresh;
    procedure ResetProperty(Row: TERDInspectorProperty);
    procedure ResetAll;
    property BoundItemID: string read FItemID;
  end;
implementation
const PropertyTagBase = 1000;
procedure TERDPCBInspectorBinding.Clear;
begin FScene := nil; FInspector := nil; FItemID := ''; FTypeID := ''; FProperties := nil; end;
function TERDPCBInspectorBinding.ItemIndex: Integer;
begin
  Result := -1; if FScene <> nil then Result := FScene.IndexOf(FItemID);
  if Result < 0 then raise EArgumentException.Create('The inspected component no longer exists');
  if not SameText(FScene.Item(Result).TypeID, FTypeID) then raise EArgumentException.Create('Rebind the inspector after changing component type');
end;
function TERDPCBInspectorBinding.PropertyIndex(Row: TERDInspectorProperty): Integer;
begin
  Result := -1;
  if (Row <> nil) and (FScene <> nil) and (Row.Tag >= PropertyTagBase) and
    (Row.Tag - PropertyTagBase < Length(FProperties)) then Result := Row.Tag - PropertyTagBase;
end;
function TERDPCBInspectorBinding.Handles(Row: TERDInspectorProperty): Boolean;
begin Result := PropertyIndex(Row) >= 0; end;
procedure TERDPCBInspectorBinding.Bind(Scene: TERDPCBScene; const ItemID: string; Inspector: TERDInspector);
var Definition: TERDPCBComponentType; Value: TERDPCBDesignItem; Index, I, J: Integer;
    Category: TERDInspectorCategory; Row: TERDInspectorProperty; Caption, Choice: string;
    Editor: TERDInspectorValueKind;
begin
  Clear;
  if (Scene = nil) or (Inspector = nil) then Exit;
  Index := Scene.IndexOf(ItemID); if Index < 0 then Exit;
  Value := Scene.Item(Index);
  if not ERDPCBFindComponentType(Value.TypeID, Definition) then Exit;
  FScene := Scene; FInspector := Inspector; FItemID := ItemID; FTypeID := Value.TypeID; FProperties := Definition.Properties;
  Inspector.BeginUpdate;
  try
    for I := 0 to High(FProperties) do
    begin
      Caption := FProperties[I].Category; if Caption = '' then Caption := 'Component properties';
      Category := nil;
      for J := 0 to Inspector.Categories.Count-1 do
        if Inspector.Categories[J].Caption = Caption then begin Category := Inspector.Categories[J]; Break; end;
      if Category = nil then Category := Inspector.AddCategory(Caption);
      Editor := ivText;
      case FProperties[I].Kind of pkBoolean: Editor := ivCheck; pkChoice: Editor := ivPickList; end;
      Row := Category.AddProperty(FProperties[I].Caption, ERDPCBPropertyValue(Value, FProperties[I].Name), Editor);
      Row.Tag := PropertyTagBase + I; Row.Hint := FProperties[I].Hint;
      Row.DefaultValue := FProperties[I].DefaultValue; Row.HasDefault := True;
      Row.ReadOnly := FProperties[I].ReadOnly or Value.Locked;
      for Choice in FProperties[I].Choices do Row.PickList.Add(Choice);
    end;
  finally Inspector.EndUpdate; end;
end;
procedure TERDPCBInspectorBinding.Validate(Row: TERDInspectorProperty; const NewValue: string);
var Index, PropIndex: Integer; Value: TERDPCBDesignItem;
begin
  PropIndex := PropertyIndex(Row); if PropIndex < 0 then raise EArgumentException.Create('Not a registered property row');
  try
    Index := ItemIndex; Value := ERDPCBWithProperty(FScene.Item(Index), FProperties[PropIndex].Name, NewValue);
    FScene.ValidateItem(Index, Value);
    Row.Level := alvNormal; Row.Hint := FProperties[PropIndex].Hint;
  except on E: Exception do begin Row.Level := alvAlarm; Row.Hint := E.Message; raise; end; end;
end;
procedure TERDPCBInspectorBinding.Apply(Row: TERDInspectorProperty);
var Index: Integer;
begin
  Index := PropertyIndex(Row); if Index < 0 then raise EArgumentException.Create('Not a registered property row');
  try
    FScene.SetProperty(ItemIndex, FProperties[Index].Name, Row.Value); Refresh;
  except on E: Exception do
    begin Refresh; Row.Level := alvAlarm; Row.Hint := E.Message; raise; end;
  end;
end;
procedure TERDPCBInspectorBinding.Refresh;
var Index, C, P, PropIndex: Integer; Value: TERDPCBDesignItem; Row: TERDInspectorProperty;
begin
  if (FScene = nil) or (FInspector = nil) then Exit;
  Index := FScene.IndexOf(FItemID); if Index < 0 then Exit;
  Value := FScene.Item(Index); if not SameText(Value.TypeID, FTypeID) then Exit;
  FInspector.BeginUpdate;
  try
    for C := 0 to FInspector.Categories.Count-1 do
      for P := 0 to FInspector.Categories[C].Properties.Count-1 do
      begin
        Row := FInspector.Categories[C].Properties[P]; PropIndex := PropertyIndex(Row); if PropIndex < 0 then Continue;
        Row.Value := ERDPCBPropertyValue(Value, FProperties[PropIndex].Name);
        Row.ReadOnly := FProperties[PropIndex].ReadOnly or Value.Locked;
        Row.Level := alvNormal; Row.Hint := FProperties[PropIndex].Hint;
      end;
  finally FInspector.EndUpdate; end;
  FInspector.RefreshEditor;
end;
procedure TERDPCBInspectorBinding.ResetProperty(Row: TERDInspectorProperty);
var Index, SceneIndex: Integer;
begin
  Index := PropertyIndex(Row); if Index < 0 then raise EArgumentException.Create('Select a registered component property');
  SceneIndex := ItemIndex; FInspector.CancelEdit;
  FScene.ResetProperty(SceneIndex, FProperties[Index].Name); Refresh;
end;
procedure TERDPCBInspectorBinding.ResetAll;
var Index: Integer;
begin Index := ItemIndex; FInspector.CancelEdit; FScene.ResetProperties(Index); Refresh; end;
end.
