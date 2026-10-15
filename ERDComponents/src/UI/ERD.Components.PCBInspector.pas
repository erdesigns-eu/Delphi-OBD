unit ERD.Components.PCBInspector;
interface
uses System.SysUtils, ERD.Components.Types, ERD.Components.PCBScene, ERD.Components.Inspector;
type
  TERDPCBBoundField = (bfCustom, bfCaption, bfEnabled, bfValue, bfNoteType);
  /// Optional host adapter. Owns no scene or inspector and installs no events.
  TERDPCBInspectorBinding = class
  private
    FScene: TERDPCBScene;
    FInspector: TERDInspector;
    FItemID: string;
    FIDs, FTypeIDs: TArray<string>;
    FFields: TArray<TERDPCBBoundField>;
    FProperties: TArray<TERDPCBPropertyDefinition>;
    function ItemIndex(Position: Integer): Integer;
    function ReadValue(const Item: TERDPCBDesignItem; PropIndex: Integer): string;
    function WriteValue(const Item: TERDPCBDesignItem; PropIndex: Integer; const Text: string): TERDPCBDesignItem;
    function DefaultText(const Item: TERDPCBDesignItem; PropIndex: Integer): string;
    function Candidates(PropIndex: Integer; const Text: string; Reset: Boolean): TArray<TERDPCBDesignItem>;
    procedure BuildRows;
    function PropertyIndex(Row: TERDInspectorProperty): Integer;
  public
    procedure Clear;
    procedure Bind(Scene: TERDPCBScene; const ItemID: string; Inspector: TERDInspector);
    procedure BindSelection(Scene: TERDPCBScene; Inspector: TERDInspector);
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
begin FScene := nil; FInspector := nil; FItemID := ''; FIDs := nil; FTypeIDs := nil; FProperties := nil; FFields := nil; end;
function TERDPCBInspectorBinding.ItemIndex(Position: Integer): Integer;
begin
  Result := -1; if FScene <> nil then Result := FScene.IndexOf(FIDs[Position]);
  if Result < 0 then raise EArgumentException.Create('The inspected component no longer exists');
  if not SameText(FScene.Item(Result).TypeID, FTypeIDs[Position]) then raise EArgumentException.Create('Rebind the inspector after changing component type');
end;
function TERDPCBInspectorBinding.ReadValue(const Item: TERDPCBDesignItem; PropIndex: Integer): string;
begin
  case FFields[PropIndex] of
    bfCaption: Result := Item.Caption;
    bfEnabled: Result := BoolToStr(Item.Enabled, True);
    bfValue: Result := IntToStr(Item.Value);
    bfNoteType: Result := ERDPCBNoteTypeName(Item.NoteType);
  else Result := ERDPCBPropertyValue(Item, FProperties[PropIndex].Name); end;
end;
function TERDPCBInspectorBinding.WriteValue(const Item: TERDPCBDesignItem; PropIndex: Integer; const Text: string): TERDPCBDesignItem;
begin
  if FProperties[PropIndex].ReadOnly then raise EArgumentException.Create('This property is read-only for a selected component');
  if Item.Locked then raise EArgumentException.Create('Unlock every selected component before editing this property');
  Result := Item;
  case FFields[PropIndex] of
    bfCaption: Result.Caption := Text;
    bfEnabled:
      begin
        if not SameText(Text, 'True') and not SameText(Text, 'False') then raise EArgumentException.Create('Enabled must be True or False');
        Result.Enabled := SameText(Text, 'True');
      end;
    bfValue: Result.Value := StrToInt(Text);
    bfNoteType: Result.NoteType := ERDPCBParseNoteType(Text);
  else
    begin
      Result := ERDPCBWithProperty(Item, FProperties[PropIndex].Name, Text);
      if ERDPCBPropertyValue(Result, FProperties[PropIndex].Name) = ERDPCBPropertyValue(Item, FProperties[PropIndex].Name) then Result := Item;
    end;
  end;
end;
function TERDPCBInspectorBinding.DefaultText(const Item: TERDPCBDesignItem; PropIndex: Integer): string;
var Definition: TERDPCBComponentType; Prop: TERDPCBPropertyDefinition;
begin
  if not ERDPCBFindComponentType(Item.TypeID, Definition) then Exit(ReadValue(Item, PropIndex));
  case FFields[PropIndex] of
    bfCaption: Exit(Definition.Caption);
    bfEnabled: Exit('True');
    bfValue: Exit(IntToStr(Definition.Value));
    bfNoteType: Exit(ERDPCBNoteTypeName(Definition.NoteType));
  end;
  for Prop in Definition.Properties do if Prop.Name = FProperties[PropIndex].Name then Exit(Prop.DefaultValue);
  raise EArgumentException.Create('Property definition no longer exists');
end;
function TERDPCBInspectorBinding.Candidates(PropIndex: Integer; const Text: string; Reset: Boolean): TArray<TERDPCBDesignItem>;
var I, J: Integer; Value: string;
begin
  SetLength(Result, Length(FIDs));
  for I := 0 to High(FIDs) do
  begin
    Result[I] := FScene.Item(ItemIndex(I));
    if PropIndex >= 0 then
    begin
      Value := Text; if Reset then Value := DefaultText(Result[I], PropIndex);
      Result[I] := WriteValue(Result[I], PropIndex, Value);
    end
    else for J := 0 to High(FProperties) do if not FProperties[J].ReadOnly then
      Result[I] := WriteValue(Result[I], J, DefaultText(Result[I], J));
  end;
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
var Definition: TERDPCBComponentType; Value: TERDPCBDesignItem; Index: Integer;
begin
  Clear; if (Scene = nil) or (Inspector = nil) then Exit;
  Index := Scene.IndexOf(ItemID); if Index < 0 then Exit;
  Value := Scene.Item(Index); if not ERDPCBFindComponentType(Value.TypeID, Definition) then Exit;
  FScene := Scene; FInspector := Inspector; FItemID := ItemID;
  SetLength(FIDs, 1); SetLength(FTypeIDs, 1); FIDs[0] := ItemID; FTypeIDs[0] := Value.TypeID;
  FProperties := Definition.Properties; SetLength(FFields, Length(FProperties)); BuildRows;
end;
procedure TERDPCBInspectorBinding.BindSelection(Scene: TERDPCBScene; Inspector: TERDInspector);
var I, J, K, N, P: Integer; Definition, Other: TERDPCBComponentType; First, Item: TERDPCBDesignItem;
    Prop, Source, Match: TERDPCBPropertyDefinition; Common, AllNotes, AllNonNotes, SameKind: Boolean;
    Choice: string; Choices: TArray<string>; NoteType: TERDPCBNoteType;
  procedure Add(const Value: TERDPCBPropertyDefinition; Field: TERDPCBBoundField);
  var Index: Integer;
  begin Index := Length(FProperties); SetLength(FProperties, Index+1); SetLength(FFields, Index+1); FProperties[Index] := Value; FFields[Index] := Field; end;
  procedure BuiltIn(const Caption: string; Kind: TERDPCBPropertyKind; Field: TERDPCBBoundField);
  begin Prop := Default(TERDPCBPropertyDefinition); Prop.Caption := Caption; Prop.Category := 'Common properties'; Prop.Kind := Kind; Add(Prop, Field); end;
begin
  Clear; if (Scene = nil) or (Inspector = nil) or (Scene.SelectionCount = 0) then Exit;
  FScene := Scene; FInspector := Inspector;
  SetLength(FIDs, Scene.SelectionCount); SetLength(FTypeIDs, Length(FIDs)); N := 0;
  for I := 0 to Scene.Count-1 do if Scene.Item(I).Selected then
  begin Item := Scene.Item(I); FIDs[N] := Item.ID; FTypeIDs[N] := Item.TypeID; Inc(N); end;
  FItemID := FIDs[0]; First := Scene.Item(ItemIndex(0));
  AllNotes := First.Kind = piNote; AllNonNotes := First.Kind <> piNote; SameKind := True;
  for I := 1 to High(FIDs) do
  begin
    Item := Scene.Item(ItemIndex(I)); AllNotes := AllNotes and (Item.Kind = piNote);
    AllNonNotes := AllNonNotes and (Item.Kind <> piNote); SameKind := SameKind and (Item.Kind = First.Kind);
  end;
  BuiltIn('Caption', pkText, bfCaption);
  if AllNonNotes then BuiltIn('Enabled', pkBoolean, bfEnabled);
  if SameKind and AllNonNotes then
  begin
    Choice := 'Value';
    case First.Kind of piTimer: Choice := 'Interval (ms)'; piOutputGroup: Choice := 'LED count';
      piUART, piCANBus: Choice := 'Bit rate'; piI2C, piSPI: Choice := 'Clock (Hz)'; end;
    BuiltIn(Choice, pkInteger, bfValue);
  end;
  if AllNotes then
  begin
    BuiltIn('Type', pkChoice, bfNoteType); P := High(FProperties);
    SetLength(FProperties[P].Choices, Ord(High(TERDPCBNoteType))+1);
    for NoteType := Low(TERDPCBNoteType) to High(TERDPCBNoteType) do FProperties[P].Choices[Ord(NoteType)] := ERDPCBNoteTypeName(NoteType);
  end;
  if ERDPCBFindComponentType(First.TypeID, Definition) then
    for Source in Definition.Properties do
    begin
      Prop := Source; Common := True;
      for I := 1 to High(FIDs) do
      begin
        Item := Scene.Item(ItemIndex(I)); K := -1;
        if ERDPCBFindComponentType(Item.TypeID, Other) then
          for J := 0 to High(Other.Properties) do
            if (Other.Properties[J].Name = Prop.Name) and (Other.Properties[J].Kind = Prop.Kind) then begin K := J; Break; end;
        if K < 0 then begin Common := False; Break; end;
        Match := Other.Properties[K]; Prop.ReadOnly := Prop.ReadOnly or Match.ReadOnly;
        if Prop.Kind = pkChoice then
        begin
          Choices := nil;
          for Choice in Prop.Choices do for J := 0 to High(Match.Choices) do if Choice = Match.Choices[J] then
          begin N := Length(Choices); SetLength(Choices, N+1); Choices[N] := Choice; Break; end;
          Prop.Choices := Choices;
        end;
      end;
      if Common and ((Prop.Kind <> pkChoice) or (Length(Prop.Choices) > 0)) then Add(Prop, bfCustom);
    end;
  BuildRows;
end;
procedure TERDPCBInspectorBinding.BuildRows;
var I, J: Integer; Category: TERDInspectorCategory; Row: TERDInspectorProperty; Caption, Choice: string; Editor: TERDInspectorValueKind;
begin
  FInspector.BeginUpdate;
  try
    for I := 0 to High(FProperties) do
    begin
      Caption := FProperties[I].Category; if Caption = '' then Caption := 'Component properties'; Category := nil;
      for J := 0 to FInspector.Categories.Count-1 do if FInspector.Categories[J].Caption = Caption then begin Category := FInspector.Categories[J]; Break; end;
      if Category = nil then Category := FInspector.AddCategory(Caption);
      Editor := ivText; case FProperties[I].Kind of pkBoolean: Editor := ivCheck; pkChoice: Editor := ivPickList; end;
      Row := Category.AddProperty(FProperties[I].Caption, '', Editor); Row.Tag := PropertyTagBase+I;
      for Choice in FProperties[I].Choices do Row.PickList.Add(Choice);
    end;
  finally FInspector.EndUpdate; end;
  Refresh;
end;
procedure TERDPCBInspectorBinding.Validate(Row: TERDInspectorProperty; const NewValue: string);
var Index: Integer;
begin
  Index := PropertyIndex(Row); if Index < 0 then raise EArgumentException.Create('Not a registered property row');
  try
    FScene.ValidateItems(Candidates(Index, NewValue, False)); Row.Level := alvNormal; Row.Hint := FProperties[Index].Hint;
  except on E: Exception do begin Row.Level := alvAlarm; Row.Hint := E.Message; raise; end; end;
end;
procedure TERDPCBInspectorBinding.Apply(Row: TERDInspectorProperty);
var Index: Integer;
begin
  Index := PropertyIndex(Row); if Index < 0 then raise EArgumentException.Create('Not a registered property row');
  try FScene.UpdateItems(Candidates(Index, Row.Value, False)); Refresh;
  except on E: Exception do begin Refresh; Row.Level := alvAlarm; Row.Hint := E.Message; raise; end; end;
end;
procedure TERDPCBInspectorBinding.Refresh;
var C, P, I, Index: Integer; Item: TERDPCBDesignItem; Row: TERDInspectorProperty; Text, DefaultValue: string;
    Mixed, ReadOnly, SameDefault: Boolean; Definition: TERDPCBComponentType;
begin
  if (FScene = nil) or (FInspector = nil) or (Length(FIDs) = 0) then Exit;
  for I := 0 to High(FIDs) do
  begin Index := FScene.IndexOf(FIDs[I]); if Index < 0 then Exit; if not SameText(FScene.Item(Index).TypeID, FTypeIDs[I]) then Exit; end;
  FInspector.BeginUpdate;
  try
    for C := 0 to FInspector.Categories.Count-1 do for P := 0 to FInspector.Categories[C].Properties.Count-1 do
    begin
      Row := FInspector.Categories[C].Properties[P]; Index := PropertyIndex(Row); if Index < 0 then Continue;
      Item := FScene.Item(ItemIndex(0)); Text := ReadValue(Item, Index); DefaultValue := DefaultText(Item, Index);
      Mixed := False; SameDefault := ERDPCBFindComponentType(Item.TypeID, Definition); ReadOnly := FProperties[Index].ReadOnly or Item.Locked;
      for I := 1 to High(FIDs) do
      begin
        Item := FScene.Item(ItemIndex(I)); Mixed := Mixed or (ReadValue(Item, Index) <> Text);
        SameDefault := SameDefault and ERDPCBFindComponentType(Item.TypeID, Definition) and (DefaultText(Item, Index) = DefaultValue); ReadOnly := ReadOnly or Item.Locked;
      end;
      Row.Value := Text; Row.Mixed := Mixed; Row.DefaultValue := DefaultValue; Row.HasDefault := SameDefault;
      Row.ReadOnly := ReadOnly; Row.Level := alvNormal; Row.Hint := FProperties[Index].Hint;
    end;
  finally FInspector.EndUpdate; end;
  FInspector.RefreshEditor;
end;
procedure TERDPCBInspectorBinding.ResetProperty(Row: TERDInspectorProperty);
var Index: Integer;
begin
  Index := PropertyIndex(Row); if Index < 0 then raise EArgumentException.Create('Select a component property');
  FInspector.CancelEdit; FScene.UpdateItems(Candidates(Index, '', True)); Refresh;
end;
procedure TERDPCBInspectorBinding.ResetAll;
begin
  if FScene = nil then raise EArgumentException.Create('Select a component first');
  FInspector.CancelEdit; FScene.UpdateItems(Candidates(-1, '', True)); Refresh;
end;
end.
