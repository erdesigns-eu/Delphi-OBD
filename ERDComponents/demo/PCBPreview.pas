unit PCBPreview;
interface
uses ERD.Components.Types, ERD.Components.ColorDialog, ERD.Components.Ribbon, Vcl.ExtCtrls, ERD.Components.PCBValidation, ERD.Components.PCBValidationPanel, System.Classes, System.SysUtils, System.Types, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
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
    FRibbon: TERDRibbon;
    FLayoutTab: TERDRibbonTab;
    FTemplateColorItem: TERDRibbonItem;
    FLayoutItems: array[0..7] of TERDRibbonItem;
    FViewItems: array[0..6] of TERDRibbonItem;
    FCommandItems: array[0..43] of TERDRibbonItem;
    FValidation: TERDPCBValidationPanel;
    FValidationTimer: TTimer;
    FValidationKey: string;
    FToolbox: TERDPCBComponentPalette;
    FStatus: TLabel;
    FFileName, FTemplateDirectory: string;
    FToolDragging: Boolean;
    procedure ViewOptionChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ConnectionSelectionChanged(Sender: TObject);
    procedure ConnectionError(Sender: TObject; const MessageText: string);
    procedure DesignChanged(Sender: TObject);
    procedure ScheduleValidation;
    procedure RunValidation(Sender: TObject);
    procedure NavigateValidation(Sender: TObject; const Issue: TERDPCBValidationIssue);
    procedure UpdatePinChoices;
    procedure EditNote(Sender: TObject);
    procedure PropertyButtonClicked(Sender: TObject; AProperty: TERDInspectorProperty);
    procedure PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
    procedure PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
    function EditedItem(AProperty: TERDInspectorProperty; const Text: string): TERDPCBDesignItem;
    procedure ToolbarClicked(Sender: TObject);
    procedure UpdateLayoutButtons;
    procedure ToolboxSelectionChanged(Sender: TObject);
    procedure ToolboxClick(Sender: TObject);
    procedure ToolboxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ToolboxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure PlacementChanged(Sender: TObject);
    procedure SelectToolClicked(Sender: TObject);
    procedure SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure AttachRibbon(Value: TERDRibbon);
    procedure SetRibbonActive(Value: Boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ThemeChanged; override;
  end;
implementation
uses System.Math, System.IOUtils, Vcl.Dialogs, ERD.Components.Dialogs, PCBTemplateDialog;
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
var Tools, Center: TERDPanel; B: TERDButton; I: Integer; Base, BoardFile: string;
begin
  inherited;
  FBinding := TERDPCBInspectorBinding.Create; RegisterDemoComponentTypes;
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
  Center := TERDPanel.Create(Self); Center.Parent := Self; Center.BorderWidth := 0; Center.Align := alClient;
  FValidation := TERDPCBValidationPanel.Create(Self); FValidation.Parent := Center;
  FValidation.Align := alBottom; FValidation.Height := ScaleValue(230); FValidation.OnNavigate := NavigateValidation;
  FValidationTimer := TTimer.Create(Self); FValidationTimer.Enabled := False;
  FValidationTimer.Interval := 200; FValidationTimer.OnTimer := RunValidation;
  FDesigner := TERDPCBDesigner.Create(Self); FDesigner.Parent := Center; FDesigner.Align := alClient;
  FObjects := TERDPCBObjectTree.Create(Self); FObjects.Parent := Tools;
  FObjects.Align := alBottom; FObjects.Height := ScaleValue(240); FObjects.Designer := FDesigner;
  FDesigner.OnConnectionSelectionChange := ConnectionSelectionChanged;
  FDesigner.OnPlacementChange := PlacementChanged;
  FDesigner.OnSelectionChange := SelectionChanged; FDesigner.OnChange := DesignChanged; FDesigner.OnConnectionError := ConnectionError;
  FDesigner.OnDragOver := SurfaceDragOver; FDesigner.OnDragDrop := SurfaceDragDrop;
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
  UpdateLayoutButtons; RunValidation(Self); ThemeChanged;
end;
destructor TPCBPreview.Destroy;
begin
  AttachRibbon(nil);
  if FValidationTimer <> nil then begin FValidationTimer.Enabled := False; FValidationTimer.OnTimer := nil; end;
  if FValidation <> nil then begin FValidation.OnNavigate := nil; FValidation.OnValidate := nil; end;
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
var Item: TERDRibbonItem;
begin
  Item := Sender as TERDRibbonItem;
  case Item.Tag of
    100: FDesigner.ShowBodies := Item.Down;
    101: FDesigner.ShowPads := Item.Down;
    102: FDesigner.ShowLabels := Item.Down;
    103: FDesigner.ShowComponentOutlines := Item.Down;
    104: FDesigner.ShowGrid := Item.Down;
    105:
      begin
        FObjects.Enabled := not Item.Down; FToolbox.Enabled := not Item.Down;
        FDesigner.TemplateEditMode := Item.Down; FDesigner.Scene.ClearSelection; SelectionChanged(Self);
      end;
    106: FDesigner.PanMode := Item.Down;
  end;
  UpdateLayoutButtons;
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
procedure TPCBPreview.ScheduleValidation;
begin
  if FValidationTimer <> nil then begin FValidationTimer.Enabled := False; FValidationTimer.Enabled := True; end;
end;
procedure TPCBPreview.RunValidation(Sender: TObject);
var Pins: TArray<TERDPCBValidationPin>; Context: TERDPCBValidationContext; Channel: TERDPCBChannel;
    Capability: TERDPCBPinCapability; I, N: Integer; Key: string; HasTemplate: Boolean;
begin
  FValidationTimer.Enabled := False;
  // Scene notifications can precede EndEdit; wait until the drag completes.
  if Vcl.Controls.GetCaptureControl = FDesigner then begin FValidationTimer.Enabled := True; Exit; end;
  UpdateLayoutButtons;
  HasTemplate := (FDesigner.ChannelCount > 0) or (FDesigner.TemplateID <> '') or (FDesigner.BoardFileName <> '');
  Key := FDesigner.Scene.ContentJSON+#0+BoolToStr(HasTemplate,True); SetLength(Pins,FDesigner.ChannelCount);
  for I := 0 to High(Pins) do
  begin
    Channel := FDesigner.Channel(I); Pins[I].ID := Channel.Pin;
    Key := Key+#0+Channel.Pin+'='+ERDPCBPinCapabilitiesText(Channel.Capabilities);
    for Capability := Low(TERDPCBPinCapability) to High(TERDPCBPinCapability) do if Capability in Channel.Capabilities then
    begin N := Length(Pins[I].Capabilities); SetLength(Pins[I].Capabilities,N+1); Pins[I].Capabilities[N] := ERDPCBPinCapabilityName(Capability); end;
  end;
  if Key = FValidationKey then Exit;
  Context := ERDPCBCaptureValidation(FDesigner.Scene,Pins,HasTemplate);
  FValidation.Validate(Context); FValidationKey := Key;
end;
procedure TPCBPreview.NavigateValidation(Sender: TObject; const Issue: TERDPCBValidationIssue);
begin
  // The host owns navigation; stale/global messages never select a different object.
  if (Issue.ItemID = '') or (FDesigner.Scene.IndexOf(Issue.ItemID) < 0) then Exit;
  FDesigner.CancelPlacement; FDesigner.SelectedPin := ''; FDesigner.Scene.SelectIDs([Issue.ItemID]); FDesigner.FitSelection;
end;
procedure TPCBPreview.DesignChanged(Sender: TObject);
var C, P, Index: Integer; Value: TERDPCBDesignItem; Row: TERDInspectorProperty;
begin
  ScheduleValidation; UpdateLayoutButtons;
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
  UpdateLayoutButtons;
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
    if FDesigner.PanMode then FDesigner.PanMode := False;
    UpdateLayoutButtons;
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
procedure TPCBPreview.AttachRibbon(Value: TERDRibbon);
var ProjectGroup, EditGroup, ViewGroup, Layers, AlignGroup, Distribution, TemplateGroup, OrderGroup: TERDRibbonGroup;
    I: Integer; Item: TERDRibbonItem;
  procedure Command(Group: TERDRibbonGroup; const Caption: string; Tag: Integer; Glyph: TERDGlyph);
  var Button: TERDRibbonItem;
  begin
    Button := Group.Items.Add; Button.Caption := Caption; Button.Tag := Tag;
    Button.Glyph := Glyph; Button.Size := risSmall; Button.OnClick := ToolbarClicked;
    if (Tag >= 0) and (Tag <= 43) then FCommandItems[Tag] := Button;
    if Tag = 11 then Button.Kind := rikToggle;
  end;
begin
  if FRibbon = Value then Exit;
  FTemplateColorItem := nil;
  for I := 0 to 7 do FLayoutItems[I] := nil;
  for I := 0 to 6 do FViewItems[I] := nil;
  for I := 0 to 43 do FCommandItems[I] := nil;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FLayoutTab); end;
  FRibbon := Value; if FRibbon = nil then Exit;
  FRibbon.FreeNotification(Self); FLayoutTab := FRibbon.Tabs.Add;
  FLayoutTab.Caption := 'PCB'; FLayoutTab.Visible := False;
  ProjectGroup := FLayoutTab.Groups.Add; ProjectGroup.Caption := 'Project';
  Command(ProjectGroup,'Open project',3,glOpen); Command(ProjectGroup,'Save project',2,glSave);
  EditGroup := FLayoutTab.Groups.Add; EditGroup.Caption := 'Bewerken';
  Command(EditGroup,'Undo',4,glUndo); Command(EditGroup,'Redo',5,glRedo);
  Command(EditGroup,'Lock selection',30,glLock); Command(EditGroup,'Unlock selection',31,glUnlock);
  ViewGroup := FLayoutTab.Groups.Add; ViewGroup.Caption := 'Weergave';
  Command(ViewGroup,'Fit',0,glSearch); Command(ViewGroup,'Focus selection',8,glPin); Command(ViewGroup,'Validation',11,glAlert);
  Command(ViewGroup,'Grid spacing...',32,glSettings);
  OrderGroup := FLayoutTab.Groups.Add; OrderGroup.Caption := 'Volgorde';
  Command(OrderGroup,'Bring to front',40,glUp); Command(OrderGroup,'Send to back',41,glChevronDown);
  Command(OrderGroup,'Bring forward',42,glForward); Command(OrderGroup,'Send backward',43,glBack);
  Layers := FLayoutTab.Groups.Add; Layers.Caption := 'Lagen';
  AlignGroup := FLayoutTab.Groups.Add; AlignGroup.Caption := 'Uitlijnen';
  Distribution := FLayoutTab.Groups.Add; Distribution.Caption := 'Verdelen';
  TemplateGroup := FLayoutTab.Groups.Add; TemplateGroup.Caption := 'Template';
  Command(TemplateGroup,'Templates...',6,glFolder); Command(TemplateGroup,'Save template',7,glSave);
  for I := 0 to 6 do
  begin
    if I <= 4 then Item := Layers.Items.Add
    else if I = 5 then Item := TemplateGroup.Items.Add else Item := ViewGroup.Items.Add;
    Item.Size := risSmall; Item.Kind := rikToggle; Item.Tag := 100+I; Item.OnClick := ViewOptionChanged;
    case I of 0: Item.Caption := 'Bodies'; 1: Item.Caption := 'Pads'; 2: Item.Caption := 'Labels';
      3: Item.Caption := 'Outlines'; 4: Item.Caption := 'Grid'; 5: Item.Caption := 'Edit template pins'; 6: Item.Caption := 'Hand / pan'; end;
    FViewItems[I] := Item;
  end;
  for I := 0 to 7 do
  begin
    if I < 6 then Item := AlignGroup.Items.Add else Item := Distribution.Items.Add;
    Item.Size := risSmall; Item.Tag := 20+I; Item.OnClick := ToolbarClicked;
    case I of 0: Item.Caption := 'Left'; 1: Item.Caption := 'Center X'; 2: Item.Caption := 'Right';
      3: Item.Caption := 'Top'; 4: Item.Caption := 'Center Y'; 5: Item.Caption := 'Bottom';
      6: Item.Caption := 'Space X'; 7: Item.Caption := 'Space Y'; end;
    if I < 6 then Item.Hint := 'Select at least two unlocked objects to align'
    else Item.Hint := 'Select at least three unlocked objects for equal spacing';
    FLayoutItems[I] := Item;
  end;
  FTemplateColorItem := TemplateGroup.Items.Add; FTemplateColorItem.Caption := 'PCB color';
  FTemplateColorItem.Tag := 12; FTemplateColorItem.OnClick := ToolbarClicked; FTemplateColorItem.Glyph := glPipette;
  FTemplateColorItem.Hint := 'Enable Edit template pins, choose the solder-mask color, then Save template';
  UpdateLayoutButtons;
end;
procedure TPCBPreview.SetRibbonActive(Value: Boolean);
var I, FlatIndex: Integer;
begin
  if (FRibbon = nil) or (FLayoutTab = nil) then Exit;
  FlatIndex := 0;
  for I := 0 to FLayoutTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(FlatIndex);
  if not Value and FLayoutTab.Visible and (FRibbon.ActiveTab = FlatIndex) then FRibbon.ActiveTab := 0;
  FLayoutTab.Visible := Value;
  if Value then begin UpdateLayoutButtons; FRibbon.ActiveTab := FlatIndex; end;
end;
procedure TPCBPreview.Notification(AComponent: TComponent; Operation: TOperation);
var I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRibbon) then
  begin
    FRibbon := nil; FLayoutTab := nil; FTemplateColorItem := nil;
    for I := 0 to 7 do FLayoutItems[I] := nil;
    for I := 0 to 6 do FViewItems[I] := nil;
    for I := 0 to 43 do FCommandItems[I] := nil;
  end;
end;
procedure TPCBPreview.UpdateLayoutButtons;
var I, Count: Integer;
begin
  if FDesigner = nil then Exit; Count := 0;
  if FViewItems[0] <> nil then FViewItems[0].Down := FDesigner.ShowBodies;
  if FViewItems[1] <> nil then FViewItems[1].Down := FDesigner.ShowPads;
  if FViewItems[2] <> nil then FViewItems[2].Down := FDesigner.ShowLabels;
  if FViewItems[3] <> nil then FViewItems[3].Down := FDesigner.ShowComponentOutlines;
  if FViewItems[4] <> nil then FViewItems[4].Down := FDesigner.ShowGrid;
  if FViewItems[5] <> nil then FViewItems[5].Down := FDesigner.TemplateEditMode;
  if FViewItems[6] <> nil then FViewItems[6].Down := FDesigner.PanMode;
  if FCommandItems[4] <> nil then FCommandItems[4].Enabled := FDesigner.Scene.CanUndo;
  if FCommandItems[5] <> nil then FCommandItems[5].Enabled := FDesigner.Scene.CanRedo;
  if FCommandItems[11] <> nil then FCommandItems[11].Down := FValidation.Visible;
  if FTemplateColorItem <> nil then FTemplateColorItem.Enabled := FDesigner.TemplateEditMode and (FDesigner.TemplateID <> '');
  for I := 0 to FDesigner.Scene.Count-1 do
    if FDesigner.Scene.Item(I).Selected and not FDesigner.Scene.Item(I).Locked then Inc(Count);
  if FCommandItems[30] <> nil then FCommandItems[30].Enabled := not FDesigner.TemplateEditMode and (Count > 0);
  if FCommandItems[31] <> nil then FCommandItems[31].Enabled := not FDesigner.TemplateEditMode and (FDesigner.Scene.SelectionCount > Count);
  for I := 40 to 43 do if FCommandItems[I] <> nil then FCommandItems[I].Enabled := not FDesigner.TemplateEditMode and (Count > 0);
  for I := 0 to 7 do if FLayoutItems[I] <> nil then
    FLayoutItems[I].Enabled := not FDesigner.TemplateEditMode and (((I < 6) and (Count >= 2)) or ((I >= 6) and (Count >= 3)));
end;
procedure TPCBPreview.ToolbarClicked(Sender: TObject);
var Save: TERDSaveDialog; Open: TERDOpenDialog; TemplateFile, Before: string; ActionTag: NativeInt; ColorDialog: TERDColorDialog; GridValues: array[0..1] of string; GridX, GridY: Double;
begin
  if Sender is TERDRibbonItem then ActionTag := TERDRibbonItem(Sender).Tag
  else ActionTag := TComponent(Sender).Tag;
  try
    if ActionTag in [20..27] then
    begin
      Before := FDesigner.Scene.ContentJSON;
      if ActionTag <= 25 then FDesigner.AlignSelected(TERDPCBAlignment(ActionTag-20))
      else FDesigner.DistributeSelected(ActionTag = 26);
      if Before = FDesigner.Scene.ContentJSON then FStatus.Caption := 'Selection already matches this layout; locked objects stay fixed.'
      else FStatus.Caption := 'Layout applied to unlocked selected objects; Ctrl+Z to undo.';
      UpdateLayoutButtons; Exit;
    end;
    case ActionTag of
      30: FDesigner.SetSelectionLocked(True); 31: FDesigner.SetSelectionLocked(False);
      40..43: FDesigner.ReorderSelection(TERDPCBZOrder(ActionTag-40));
      32:
        begin
          GridValues[0] := FloatToStr(FDesigner.GridSizeX,TFormatSettings.Invariant);
          GridValues[1] := FloatToStr(FDesigner.GridSizeY,TFormatSettings.Invariant);
          if ERDInputQuery('Grid spacing','Spacing in millimeters (0.1..100)',
            ['X (mm)','Y (mm)'],GridValues,nil,Theme) then
          begin
            GridX := StrToFloat(GridValues[0],TFormatSettings.Invariant); GridY := StrToFloat(GridValues[1],TFormatSettings.Invariant);
            if IsNan(GridX) or IsInfinite(GridX) or IsNan(GridY) or IsInfinite(GridY) or
              (GridX < 0.1) or (GridX > 100) or (GridY < 0.1) or (GridY > 100) then
              raise EArgumentException.Create('Grid spacing must be 0.1..100 mm');
            FDesigner.GridSizeX := GridX; FDesigner.GridSizeY := GridY;
          end;
        end;
      8: FDesigner.FitSelection;
      9: if FInspector.Selected is TERDInspectorProperty then FBinding.ResetProperty(TERDInspectorProperty(FInspector.Selected));
      10: FBinding.ResetAll;
      11: FValidation.Visible := not FValidation.Visible;
      12:
        begin
          ColorDialog := TERDColorDialog.Create(Self);
          try
            ColorDialog.Theme := Theme; ColorDialog.Color := FDesigner.TemplateBoardColor;
            if ColorDialog.Execute then FDesigner.TemplateBoardColor := ColorDialog.Color;
          finally ColorDialog.Free; end;
        end;
      0: FDesigner.FitBoard; 4: FDesigner.Undo; 5: FDesigner.Redo;
      2, 7:
        begin
          Save := TERDSaveDialog.Create(Self);
          try
            if ActionTag = 2 then begin Save.Filter := 'Designer project|*.pcbproject.json'; Save.DefaultExt := 'pcbproject.json'; Save.FileName := FFileName; end
            else begin Save.Filter := 'Board template|*.pcbtemplate.json'; Save.DefaultExt := 'pcbtemplate.json'; Save.InitialDir := FTemplateDirectory; end;
            if Save.Execute then
              if ActionTag = 2 then begin FDesigner.SaveProject(Save.FileName); FFileName := Save.FileName; end
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
              if ActionTag = 3 then begin FDesigner.LoadProject(Open.FileName); FFileName := Open.FileName; end
              else begin FDesigner.LoadBoard(Open.FileName); FDesigner.FitBoard; SelectionChanged(Self); end;
          finally Open.Free; end;
        end;
    end;
  except on E: Exception do FStatus.Caption := E.Message; end;
  UpdateLayoutButtons;
end;
end.
