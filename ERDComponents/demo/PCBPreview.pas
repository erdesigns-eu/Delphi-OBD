unit PCBPreview;
interface
uses System.Classes, System.SysUtils, System.Types, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.PCBDesigner, ERD.Components.PCBScene,
  ERD.Components.PCBObjectTree, ERD.Components.PCBInspector, ERD.Components.Inspector, ERD.Components.PCBComponentPalette, ERD.Components.Buttons, ERD.Components.FileDialogs;
type
  /// Demo host only: toolbox and inspector are separate from TERDPCBDesigner.
  TPCBPreview = class(TERDPanel)
  private
    FDesigner: TERDPCBDesigner;
    FInspector: TERDInspector;
    FBinding: TERDPCBInspectorBinding;
    FObjects: TERDPCBObjectTree;
    FToolbox: TERDPCBComponentPalette;
    FStatus: TLabel;
    FAuthoring, FHand: TERDCheckBox;
    FFileName, FTemplateDirectory: string;
    FToolDragging: Boolean;
    procedure ViewOptionChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ConnectionSelectionChanged(Sender: TObject);
    procedure ConnectionError(Sender: TObject; const MessageText: string);
    procedure DesignChanged(Sender: TObject);
    procedure UpdatePinChoices;
    procedure EditNote(Sender: TObject);
    procedure PropertyButtonClicked(Sender: TObject; AProperty: TERDInspectorProperty);
    procedure PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
    procedure PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
    function EditedItem(AProperty: TERDInspectorProperty; const Text: string): TERDPCBDesignItem;
    procedure ToolbarClicked(Sender: TObject);
    procedure ToolboxSelectionChanged(Sender: TObject);
    procedure ToolboxClick(Sender: TObject);
    procedure ToolboxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ToolboxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure PlacementChanged(Sender: TObject);
    procedure SelectToolClicked(Sender: TObject);
    procedure SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged; override;
  end;
implementation
uses System.Math, System.IOUtils, Vcl.Dialogs, PCBTemplateDialog;
procedure RegisterDemoComponentTypes;
var Definition: TERDPCBComponentType; Props: TArray<TERDPCBPropertyDefinition>;
begin
  if ERDPCBFindComponentType('demo.retry-timer', Definition) then Exit;
  ERDPCBFindComponentType('erd.timer', Definition);
  Definition.TypeID := 'demo.retry-timer'; Definition.DisplayName := 'Retry timer'; Definition.NamePrefix := 'RetryTimer';
  Definition.Description := 'Configure retry intervals and backoff settings.';
  SetLength(Props, 5);
  Props[0].Name := 'description'; Props[0].Caption := 'Description'; Props[0].Category := 'Documentation';
  Props[0].DefaultValue := ''; Props[0].Hint := 'A description for this timer.';
  Props[1].Name := 'retries'; Props[1].Caption := 'Retries'; Props[1].Category := 'Retry settings';
  Props[1].Kind := pkInteger; Props[1].DefaultValue := '3'; Props[1].HasMinimum := True; Props[1].Minimum := 0;
  Props[1].HasMaximum := True; Props[1].Maximum := 100;
  Props[2].Name := 'factor'; Props[2].Caption := 'Backoff factor'; Props[2].Category := 'Retry settings';
  Props[2].Kind := pkFloat; Props[2].DefaultValue := '2'; Props[2].HasMinimum := True; Props[2].Minimum := 1;
  Props[2].HasMaximum := True; Props[2].Maximum := 10;
  Props[3].Name := 'auto_start'; Props[3].Caption := 'Auto start'; Props[3].Category := 'Retry settings';
  Props[3].Kind := pkBoolean; Props[3].DefaultValue := 'True';
  Props[4].Name := 'mode'; Props[4].Caption := 'Backoff mode'; Props[4].Category := 'Retry settings';
  Props[4].Kind := pkChoice; Props[4].DefaultValue := 'Fixed'; SetLength(Props[4].Choices, 2);
  Props[4].Choices[0] := 'Fixed'; Props[4].Choices[1] := 'Exponential';
  Definition.Properties := Props; ERDPCBRegisterComponentType(Definition);
end;
constructor TPCBPreview.Create(AOwner: TComponent);
var Header, Tools: TERDPanel; B: TERDButton; I: Integer; Base, BoardFile: string; Choice: TERDCheckBox;
  procedure Button(const Caption: string; Left, Width, Tag: Integer);
  begin
    B := TERDButton.Create(Self); B.Parent := Header; B.Caption := Caption;
    B.SetBounds(ScaleValue(Left), ScaleValue(8), ScaleValue(Width), ScaleValue(28));
    B.Tag := Tag; B.OnClick := ToolbarClicked;
  end;
begin
  inherited;
  FBinding := TERDPCBInspectorBinding.Create; RegisterDemoComponentTypes;
  Header := TERDPanel.Create(Self); Header.Parent := Self; Header.Align := alTop; Header.Height := ScaleValue(112);
  Button('Fit', 8, 54, 0); Button('Undo', 68, 62, 4); Button('Redo', 136, 62, 5);
  Button('Save project', 204, 104, 2); Button('Open project', 314, 104, 3);
  Button('Templates...', 424, 112, 6); Button('Save template', 542, 112, 7);
  Button('Focus selection', 660, 116, 8);
  for I := 0 to 7 do
  begin
    B := TERDButton.Create(Self); B.Parent := Header;
    case I of 0: B.Caption := 'Left'; 1: B.Caption := 'Center X'; 2: B.Caption := 'Right';
      3: B.Caption := 'Top'; 4: B.Caption := 'Center Y'; 5: B.Caption := 'Bottom';
      6: B.Caption := 'Space X'; 7: B.Caption := 'Space Y'; end;
    B.SetBounds(ScaleValue(8 + I * 82), ScaleValue(78), ScaleValue(76), ScaleValue(26));
    B.Tag := 20 + I; B.OnClick := ToolbarClicked;
  end;
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alBottom;
  FStatus.AutoSize := False; FStatus.Height := ScaleValue(25);
  Tools := TERDPanel.Create(Self); Tools.Parent := Self; Tools.Align := alLeft; Tools.Width := ScaleValue(260);
  B := TERDButton.Create(Self); B.Parent := Tools; B.Align := alTop; B.Height := ScaleValue(30);
  B.Caption := 'Select / move'; B.OnClick := SelectToolClicked;
  B := TERDButton.Create(Self); B.Parent := Tools; B.Align := alTop; B.Height := ScaleValue(30);
  B.Caption := 'Reset property'; B.Tag := 9; B.OnClick := ToolbarClicked;
  B := TERDButton.Create(Self); B.Parent := Tools; B.Align := alTop; B.Height := ScaleValue(30);
  B.Caption := 'Reset type properties'; B.Tag := 10; B.OnClick := ToolbarClicked;
  FToolbox := TERDPCBComponentPalette.Create(Self); FToolbox.Parent := Tools; FToolbox.Align := alClient;
  FToolbox.OnChoose := ToolboxClick; FToolbox.OnSelectionChange := ToolboxSelectionChanged;
  FToolbox.OnCancel := SelectToolClicked;
  FToolbox.OnStartDrag := ToolboxStartDrag; FToolbox.OnEndDrag := ToolboxEndDrag;
  FInspector := TERDInspector.Create(Self); FInspector.Parent := Self; FInspector.Align := alRight; FInspector.Width := ScaleValue(310);
  FInspector.OnPropertyButtonClick := PropertyButtonClicked;
  FInspector.OnPropertyChanging := PropertyChanging; FInspector.OnPropertyChanged := PropertyChanged;
  FDesigner := TERDPCBDesigner.Create(Self); FDesigner.Parent := Self; FDesigner.Align := alClient;
  FObjects := TERDPCBObjectTree.Create(Self); FObjects.Parent := Tools;
  FObjects.Align := alBottom; FObjects.Height := ScaleValue(240); FObjects.Designer := FDesigner;
  FDesigner.OnConnectionSelectionChange := ConnectionSelectionChanged;
  FDesigner.OnPlacementChange := PlacementChanged;
  FDesigner.OnSelectionChange := SelectionChanged; FDesigner.OnChange := DesignChanged; FDesigner.OnConnectionError := ConnectionError;
  FDesigner.OnDragOver := SurfaceDragOver; FDesigner.OnDragDrop := SurfaceDragDrop;
  for I := 0 to 4 do
  begin
    Choice := TERDCheckBox.Create(Self); Choice.Parent := Header;
    case I of 0: Choice.Caption := 'Bodies'; 1: Choice.Caption := 'Pads'; 2: Choice.Caption := 'Labels';
      3: Choice.Caption := 'Outlines'; 4: Choice.Caption := 'Grid'; end;
    Choice.SetBounds(ScaleValue(10 + I * 91), ScaleValue(43), ScaleValue(86), ScaleValue(26));
    Choice.Checked := True; Choice.Tag := I; Choice.OnChange := ViewOptionChanged;
  end;
  FAuthoring := TERDCheckBox.Create(Self); FAuthoring.Parent := Header; FAuthoring.Caption := 'Edit template pins';
  FAuthoring.SetBounds(ScaleValue(470), ScaleValue(43), ScaleValue(170), ScaleValue(26)); FAuthoring.Tag := 5; FAuthoring.OnChange := ViewOptionChanged;
  Choice := TERDCheckBox.Create(Self); Choice.Parent := Header; Choice.Caption := 'Hand / pan';
  Choice.SetBounds(ScaleValue(650), ScaleValue(43), ScaleValue(110), ScaleValue(26));
  Choice.Tag := 6; Choice.OnChange := ViewOptionChanged; FHand := Choice;
  BoardFile := ''; Base := ExtractFilePath(ParamStr(0));
  for I := 0 to 3 do
  begin
    if FileExists(IncludeTrailingPathDelimiter(Base) + 'pcb\board.json') then
    begin BoardFile := IncludeTrailingPathDelimiter(Base) + 'pcb\board.json'; Break; end;
    Base := ExtractFileDir(ExcludeTrailingPathDelimiter(Base));
  end;
  if BoardFile <> '' then FTemplateDirectory := ExtractFileDir(BoardFile)
  else FTemplateDirectory := TPath.Combine(ExtractFilePath(ParamStr(0)), 'pcb');
  if BoardFile <> '' then
    try FDesigner.LoadBoard(BoardFile); FDesigner.SelectedPin := 'U7.2';
    except on E: Exception do FStatus.Caption := E.Message; end
  else FStatus.Caption := 'Keep the pcb folder beside the demo or its build folders.';
  ThemeChanged;
end;
destructor TPCBPreview.Destroy;
begin
  if FInspector <> nil then begin FInspector.OnPropertyChanging := nil; FInspector.OnPropertyChanged := nil; FInspector.OnPropertyButtonClick := nil; end;
  if FDesigner <> nil then
  begin
    FDesigner.OnChange := nil; FDesigner.OnSelectionChange := nil; FDesigner.OnPlacementChange := nil;
    FDesigner.OnConnectionSelectionChange := nil; FDesigner.OnConnectionError := nil;
  end;
  if FToolbox <> nil then
  begin FToolbox.OnChoose := nil; FToolbox.OnSelectionChange := nil; FToolbox.OnCancel := nil; FToolbox.OnStartDrag := nil; FToolbox.OnEndDrag := nil; end;
  if FObjects <> nil then FObjects.Designer := nil;
  FreeAndNil(FBinding); inherited;
end;
procedure TPCBPreview.ThemeChanged;
begin inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText; end;
procedure TPCBPreview.ViewOptionChanged(Sender: TObject);
var Choice: TERDCheckBox;
begin
  Choice := Sender as TERDCheckBox;
  case Choice.Tag of
    0: FDesigner.ShowBodies := Choice.Checked; 1: FDesigner.ShowPads := Choice.Checked;
    2: FDesigner.ShowLabels := Choice.Checked; 3: FDesigner.ShowComponentOutlines := Choice.Checked;
    4: FDesigner.ShowGrid := Choice.Checked;
    6: FDesigner.PanMode := Choice.Checked;
    5: begin FObjects.Enabled := not Choice.Checked; FToolbox.Enabled := not Choice.Checked; FDesigner.TemplateEditMode := Choice.Checked; FDesigner.Scene.ClearSelection; SelectionChanged(Self); end;
  end;
end;
procedure TPCBPreview.ConnectionSelectionChanged(Sender: TObject);
begin
  if FDesigner.SelectedConnectionItemID <> '' then
    FStatus.Caption := 'Connection ' + FDesigner.SelectedConnectionPin +
      ' - drag an endpoint to reconnect; Delete disconnects; F focuses the selection.';
end;
procedure TPCBPreview.ConnectionError(Sender: TObject; const MessageText: string);
begin FStatus.Caption := MessageText; end;
procedure TPCBPreview.PropertyButtonClicked(Sender: TObject; AProperty: TERDInspectorProperty);
begin
  if AProperty.Tag = 2 then EditNote(Self)
  else if AProperty.Tag = 200 then
    if EditPCBTerminal(Self, FDesigner, Palette) then SelectionChanged(Self);
end;
procedure TPCBPreview.EditNote(Sender: TObject);
begin FDesigner.BeginNoteEdit; end;
procedure TPCBPreview.UpdatePinChoices;
var C, P, Index: Integer; Row: TERDInspectorProperty; Pin: string;
begin
  Index := FDesigner.Scene.PrimaryIndex; if Index < 0 then Exit;
  FInspector.BeginUpdate;
  try
    for C := 0 to FInspector.Categories.Count - 1 do
      for P := 0 to FInspector.Categories[C].Properties.Count - 1 do
      begin
        Row := FInspector.Categories[C].Properties[P];
        if (Row.Kind <> ivPickList) or (Row.Tag < 8) or (Row.Tag > 11) then Continue;
        Row.PickList.Clear; Row.PickList.Add('');
        for Pin in FDesigner.GetAssignablePins(Index, TERDPCBPinRole(Row.Tag - 8)) do Row.PickList.Add(Pin);
      end;
  finally FInspector.EndUpdate; end;
end;
procedure TPCBPreview.DesignChanged(Sender: TObject);
var C, P, Index: Integer; Value: TERDPCBDesignItem; Row: TERDInspectorProperty;
begin
  if FObjects <> nil then FObjects.Refresh;
  Index := FDesigner.Scene.PrimaryIndex;
  if Index >= 0 then
  begin
    Value := FDesigner.Scene.Item(Index); FInspector.BeginUpdate;
    try
      for C := 0 to FInspector.Categories.Count - 1 do
        for P := 0 to FInspector.Categories[C].Properties.Count - 1 do
        begin
          Row := FInspector.Categories[C].Properties[P];
          case Row.Tag of
            1: Row.Value := Value.Name; 2: Row.Value := Value.Caption;
            3: Row.Value := IntToStr(Value.Value);
            4: Row.Value := FloatToStr(Value.X, TFormatSettings.Invariant);
            5: Row.Value := FloatToStr(Value.Y, TFormatSettings.Invariant);
            6: Row.Value := FloatToStr(Value.Width, TFormatSettings.Invariant);
            7: Row.Value := FloatToStr(Value.Height, TFormatSettings.Invariant);
            14: Row.Value := ERDPCBNoteTypeName(Value.NoteType);
          end;
        end;
    finally FInspector.EndUpdate; end;
  end;
  if FBinding <> nil then FBinding.Refresh;
  FStatus.Caption := 'Designer modified - Ctrl+Z/Y undo/redo; Ctrl+C/V copy/paste; Delete removes unlocked objects.'; end;
procedure TPCBPreview.SelectionChanged(Sender: TObject);
var Category: TERDInspectorCategory; P: TERDPCBPad; Value: TERDPCBDesignItem; Index, I, T: Integer; NoteType: TERDPCBNoteType;
    Channel: TERDPCBChannel; Group: TERDPCBTerminal; Text: string; Row: TERDInspectorProperty; Definition: TERDPCBComponentType;
  procedure Field(const Name, Text: string; Tag: Integer; Kind: TERDInspectorValueKind = ivText);
  begin Row := Category.AddProperty(Name, Text, Kind); Row.Tag := Tag; end;
  procedure PinField(const Name, Text: string; Tag: Integer);
  begin
    Field(Name, Text, Tag, ivPickList);
  end;
begin
  if FObjects <> nil then FObjects.Refresh;
  FBinding.Clear; FInspector.Clear; Index := FDesigner.Scene.PrimaryIndex;
  if FDesigner.Scene.SelectionCount > 1 then
  begin
    Category := FInspector.AddCategory('Selection');
    Category.AddProperty('Components', IntToStr(FDesigner.Scene.SelectionCount), ivReadOnly);
    FBinding.BindSelection(FDesigner.Scene, FInspector);
    FStatus.Caption := 'Shared properties - edits apply to all selected components as one undo step.';
    Exit;
  end;
  if Index >= 0 then
  begin
    Value := FDesigner.Scene.Item(Index); Category := FInspector.AddCategory('Designer component');
    Text := ERDPCBItemKindName(Value.Kind);
    if ERDPCBFindComponentType(Value.TypeID, Definition) then Text := Definition.DisplayName;
    Category.AddProperty('Component type', Text, ivReadOnly);
    Category.AddProperty('ID', Value.ID, ivReadOnly);
    Field('Name', Value.Name, 1);
    if Value.Kind = piNote then
    begin
      Field('Text', Value.Caption, 2, ivEllipsis); Row.ReadOnly := True;
      Field('Type', ERDPCBNoteTypeName(Value.NoteType), 14, ivPickList);
      for NoteType := Low(TERDPCBNoteType) to High(TERDPCBNoteType) do Row.PickList.Add(ERDPCBNoteTypeName(NoteType));
      Field('Locked', '', 13, ivCheck); Row.Checked := Value.Locked;
      Category := FInspector.AddCategory('Position and size (mm)');
      Field('X', FloatToStr(Value.X, TFormatSettings.Invariant), 4); Field('Y', FloatToStr(Value.Y, TFormatSettings.Invariant), 5);
      Field('Width', FloatToStr(Value.Width, TFormatSettings.Invariant), 6); Field('Height', FloatToStr(Value.Height, TFormatSettings.Invariant), 7);
      FBinding.Bind(FDesigner.Scene, Value.ID, FInspector);
      FStatus.Caption := 'Note - double-click to edit text; drag the handles to resize.';
      Exit;
    end;
    Field('Caption', Value.Caption, 2);
    Text := 'Value';
    case Value.Kind of piTimer: Text := 'Interval (ms)'; piOutputGroup: Text := 'LED count';
      piUART, piCANBus: Text := 'Bit rate'; piI2C, piSPI: Text := 'Clock (Hz)'; end;
    Field(Text, IntToStr(Value.Value), 3);
    Field('Enabled', '', 12, ivCheck); Row.Checked := Value.Enabled;
    Field('Locked', '', 13, ivCheck); Row.Checked := Value.Locked;
    Category := FInspector.AddCategory('Position (mm)');
    Field('X', FloatToStr(Value.X, TFormatSettings.Invariant), 4); Field('Y', FloatToStr(Value.Y, TFormatSettings.Invariant), 5);
    Category := FInspector.AddCategory('Programmable connections');
    Text := 'Terminal / pin'; if Value.Kind in [piUART, piCANBus] then Text := 'TX pin' else if Value.Kind = piI2C then Text := 'SDA pin' else if Value.Kind = piSPI then Text := 'MOSI pin';
    PinField(Text, Value.Pin, 8);
    if FDesigner.SupportsPinRole(Index, prSecondary) then
    begin
      Text := 'RX pin'; if Value.Kind = piI2C then Text := 'SCL pin' else if Value.Kind = piSPI then Text := 'MISO pin';
      PinField(Text, Value.SecondaryPin, 9);
    end;
    if FDesigner.SupportsPinRole(Index, prClock) then PinField('Clock pin', Value.ClockPin, 10);
    if FDesigner.SupportsPinRole(Index, prSelect) then PinField('Select pin', Value.SelectPin, 11);
    FBinding.Bind(FDesigner.Scene, Value.ID, FInspector);
    UpdatePinChoices;
    FStatus.Caption := IntToStr(FDesigner.Scene.SelectionCount) + ' object(s) selected - inspector edits the primary object.'; Exit;
  end;
  P := FDesigner.SelectedPad; Category := FInspector.AddCategory('Board template');
  Category.AddProperty('Template', FDesigner.TemplateName, ivReadOnly);
  if P.ID = '' then
  begin Category.AddProperty('Selection', 'Drag a toolbox item or select an output', ivReadOnly); Exit; end;
  T := FDesigner.TerminalIndex(P.ID);
  if (T >= 0) and not FDesigner.TemplateEditMode then
  begin
    Group := FDesigner.Terminal(T); Category.AddProperty('Terminal', Group.LabelText, ivReadOnly);
    for I := 0 to High(Group.Members) do
      begin
        P := FDesigner.PadInfo(Group.Members[I]);
        Category.AddProperty('Contact ' + IntToStr(I + 1), P.ID + ' - ' + P.Net, ivReadOnly);
      end;
    P := FDesigner.SelectedPad;
  end;
  Category.AddProperty('Data / selected pin', P.ID, ivReadOnly); Category.AddProperty('Net', P.Net, ivReadOnly);
  Index := FDesigner.ChannelIndex(P.ID);
  Channel := Default(TERDPCBChannel); if Index >= 0 then Channel := FDesigner.Channel(Index);
  Category.AddProperty('Route', Channel.Route, ivReadOnly);
  if FDesigner.TemplateEditMode then
  begin
    Category := FInspector.AddCategory('Template pin capabilities');
    Field('Capabilities', ERDPCBPinCapabilitiesText(Channel.Capabilities), 101);
    Field('GPIO', Channel.GPIO, 102); Field('Label', Channel.LabelText, 103);
    Field('Terminal group', 'Edit contacts...', 200, ivEllipsis); Row.ReadOnly := True;
    FStatus.Caption := 'Template authoring: comma-separated capabilities, e.g. digital-output,uart-tx,can-tx';
  end
  else
  begin
    Category.AddProperty('GPIO', Channel.GPIO, ivReadOnly);
    Category.AddProperty('Capabilities', ERDPCBPinCapabilitiesText(Channel.Capabilities), ivReadOnly);
    FStatus.Caption := 'Fixed hardware - place a designer component and assign this output in its inspector.';
  end;
end;
function TPCBPreview.EditedItem(AProperty: TERDInspectorProperty; const Text: string): TERDPCBDesignItem;
begin
  Result := FDesigner.Scene.Item(FDesigner.Scene.PrimaryIndex);
  case AProperty.Tag of
    1: Result.Name := Trim(Text); 2: Result.Caption := Text; 3: Result.Value := StrToInt(Text);
    4: Result.X := StrToFloat(Text, TFormatSettings.Invariant); 5: Result.Y := StrToFloat(Text, TFormatSettings.Invariant);
    6: Result.Width := StrToFloat(Text, TFormatSettings.Invariant); 7: Result.Height := StrToFloat(Text, TFormatSettings.Invariant);
    12: Result.Enabled := AProperty.Checked; 13: Result.Locked := AProperty.Checked;
    14: Result.NoteType := ERDPCBParseNoteType(Text);
  end;
end;
procedure TPCBPreview.PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
begin
  try
    if FBinding.Handles(AProperty) then FBinding.Validate(AProperty, NewValue)
    else if (AProperty.Tag >= 8) and (AProperty.Tag <= 11) then
      FDesigner.ValidatePinAssignment(FDesigner.Scene.PrimaryIndex, TERDPCBPinRole(AProperty.Tag - 8), NewValue)
    else if AProperty.Tag < 100 then FDesigner.Scene.ValidateItem(FDesigner.Scene.PrimaryIndex, EditedItem(AProperty, NewValue))
    else
    begin
      if AProperty.Tag = 101 then FDesigner.ValidatePinCapabilities(FDesigner.SelectedPin, ERDPCBParsePinCapabilities(NewValue));
    end;
  except on E: Exception do begin Accept := False; FStatus.Caption := E.Message; end; end;
end;
procedure TPCBPreview.PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
var Channel: TERDPCBChannel; Index: Integer;
begin
  if AProperty.Tag = 200 then Exit;
  try
    if FBinding.Handles(AProperty) then FBinding.Apply(AProperty)
    else if (AProperty.Tag >= 8) and (AProperty.Tag <= 11) then
      FDesigner.AssignPin(FDesigner.Scene.PrimaryIndex, TERDPCBPinRole(AProperty.Tag - 8), AProperty.Value)
    else if AProperty.Tag < 100 then FDesigner.Scene.UpdateItem(FDesigner.Scene.PrimaryIndex, EditedItem(AProperty, AProperty.Value))
    else
    begin
      Index := FDesigner.ChannelIndex(FDesigner.SelectedPin); Channel := Default(TERDPCBChannel);
      if Index >= 0 then Channel := FDesigner.Channel(Index);
      case AProperty.Tag of
        101: Channel.Capabilities := ERDPCBParsePinCapabilities(AProperty.Value);
        102: Channel.GPIO := AProperty.Value; 103: Channel.LabelText := AProperty.Value;
      end;
      FDesigner.SetPinCapabilities(FDesigner.SelectedPin, Channel.Capabilities, Channel.GPIO, Channel.LabelText);
    end;
  except on E: Exception do FStatus.Caption := E.Message; end;
  UpdatePinChoices;
end;
procedure TPCBPreview.ToolboxSelectionChanged(Sender: TObject);
begin
  if (FDesigner <> nil) and FDesigner.Placing and (FDesigner.PlacementTypeID <> FToolbox.SelectedTypeID) then FDesigner.CancelPlacement;
end;
procedure TPCBPreview.ToolboxClick(Sender: TObject);
begin
  FToolDragging := False;
  if (FToolbox.SelectedTypeID <> '') and not FDesigner.TemplateEditMode then
  begin FDesigner.BeginTypePlacement(FToolbox.SelectedTypeID); FDesigner.SetFocus; end;
end;
procedure TPCBPreview.ToolboxStartDrag(Sender: TObject; var DragObject: TDragObject);
begin FToolDragging := True; end;
procedure TPCBPreview.ToolboxEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FToolDragging := False; FDesigner.CancelPlacement; FToolbox.ClearSelection;
end;
procedure TPCBPreview.SelectToolClicked(Sender: TObject);
begin FDesigner.CancelPlacement; FToolbox.ClearSelection; end;
procedure TPCBPreview.PlacementChanged(Sender: TObject);
var Definition: TERDPCBComponentType; Name: string;
begin
  if FDesigner.Placing then
  begin
    if (FHand <> nil) and FHand.Checked then FHand.Checked := False;
    Name := ERDPCBItemKindName(FDesigner.PlacementKind);
    if ERDPCBFindComponentType(FDesigner.PlacementTypeID, Definition) then Name := Definition.DisplayName;
    FStatus.Caption := 'Place ' + Name +
      ' - click canvas; Shift+click repeats; Escape or right-click cancels.';
  end
  else
  begin
    if not FToolDragging and (FToolbox.SelectedTypeID = FDesigner.PlacementTypeID) then FToolbox.ClearSelection;
    FStatus.Caption := 'Select and move objects, or choose another toolbox item.';
  end;
end;
procedure TPCBPreview.SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := FToolbox.IsDragSource(Source) and (FToolbox.SelectedTypeID <> '') and not FDesigner.TemplateEditMode;
  if not Accept then Exit;
  if State = dsDragLeave then begin FDesigner.CancelPlacement; Exit; end;
  if not FDesigner.Placing or (FDesigner.PlacementTypeID <> FToolbox.SelectedTypeID) then
    FDesigner.BeginTypePlacement(FToolbox.SelectedTypeID);
  FDesigner.PreviewPlacementAt(Point(X,Y));
end;
procedure TPCBPreview.SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if FToolbox.IsDragSource(Source) and (FToolbox.SelectedTypeID <> '') then
  begin
    FDesigner.InsertTypeAt(FToolbox.SelectedTypeID, Point(X,Y)); FDesigner.CancelPlacement;
  end;
end;
procedure TPCBPreview.ToolbarClicked(Sender: TObject);
var Save: TERDSaveDialog; Open: TERDOpenDialog; TemplateFile: string;
begin
  try
    case TComponent(Sender).Tag of
      20..25: FDesigner.AlignSelected(TERDPCBAlignment(TComponent(Sender).Tag - 20));
      26, 27: FDesigner.DistributeSelected(TComponent(Sender).Tag = 26);
      8: FDesigner.FitSelection;
      9: if FInspector.Selected is TERDInspectorProperty then FBinding.ResetProperty(TERDInspectorProperty(FInspector.Selected));
      10: FBinding.ResetAll;
      0: FDesigner.FitBoard; 4: FDesigner.Undo; 5: FDesigner.Redo;
      2, 7:
        begin
          Save := TERDSaveDialog.Create(Self);
          try
            if TComponent(Sender).Tag = 2 then begin Save.Filter := 'Designer project|*.pcbproject.json'; Save.DefaultExt := 'pcbproject.json'; Save.FileName := FFileName; end
            else begin Save.Filter := 'Board template|*.pcbtemplate.json'; Save.DefaultExt := 'pcbtemplate.json'; Save.InitialDir := FTemplateDirectory; end;
            if Save.Execute then
              if TComponent(Sender).Tag = 2 then begin FDesigner.SaveProject(Save.FileName); FFileName := Save.FileName; end
              else FDesigner.SaveTemplate(Save.FileName);
          finally Save.Free; end;
        end;
      6:
        if BrowsePCBTemplates(Self, FTemplateDirectory, Palette, TemplateFile) then
        begin FDesigner.LoadBoard(TemplateFile); FDesigner.FitBoard; SelectionChanged(Self); end;
      3:
        begin
          Open := TERDOpenDialog.Create(Self);
          try
            Open.Filter := 'Designer JSON|*.json';
            if Open.Execute then
              if TComponent(Sender).Tag = 3 then begin FDesigner.LoadProject(Open.FileName); FFileName := Open.FileName; end
              else begin FDesigner.LoadBoard(Open.FileName); FDesigner.FitBoard; SelectionChanged(Self); end;
          finally Open.Free; end;
        end;
    end;
  except on E: Exception do FStatus.Caption := E.Message; end;
end;
end.
