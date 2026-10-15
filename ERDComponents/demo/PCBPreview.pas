unit PCBPreview;
interface
uses System.Classes, System.SysUtils, System.Types, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows,
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.PCBDesigner, ERD.Components.PCBScene,
  ERD.Components.Inspector, ERD.Components.ItemList, ERD.Components.Buttons, ERD.Components.FileDialogs;
type
  /// Demo host only: toolbox and inspector are separate from TERDPCBDesigner.
  TPCBPreview = class(TERDPanel)
  private
    FDesigner: TERDPCBDesigner;
    FInspector: TERDInspector;
    FToolbox: TERDListBox;
    FStatus: TLabel;
    FAuthoring, FHand: TERDCheckBox;
    FFileName, FTemplateDirectory: string;
    FToolDragging: Boolean;
    procedure ViewOptionChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ConnectionError(Sender: TObject; const MessageText: string);
    procedure DesignChanged(Sender: TObject);
    procedure UpdatePinChoices;
    procedure EditNote(Sender: TObject);
    procedure PropertyButtonClicked(Sender: TObject; AProperty: TERDInspectorProperty);
    procedure PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
    procedure PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
    function EditedItem(AProperty: TERDInspectorProperty; const Text: string): TERDPCBDesignItem;
    procedure ToolbarClicked(Sender: TObject);
    procedure ToolboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToolboxDoubleClick(Sender: TObject);
    procedure ToolboxClick(Sender: TObject);
    procedure ToolboxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ToolboxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure PlacementChanged(Sender: TObject);
    procedure SelectToolClicked(Sender: TObject);
    procedure SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ThemeChanged; override;
  end;
implementation
uses System.Math, System.IOUtils, Vcl.Dialogs, PCBTemplateDialog;
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
  Header := TERDPanel.Create(Self); Header.Parent := Self; Header.Align := alTop; Header.Height := ScaleValue(112);
  Button('Fit', 8, 54, 0); Button('Undo', 68, 62, 4); Button('Redo', 136, 62, 5);
  Button('Save project', 204, 104, 2); Button('Open project', 314, 104, 3);
  Button('Templates...', 424, 112, 6); Button('Save template', 542, 112, 7);
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
  Tools := TERDPanel.Create(Self); Tools.Parent := Self; Tools.Align := alLeft; Tools.Width := ScaleValue(145);
  B := TERDButton.Create(Self); B.Parent := Tools; B.Align := alTop; B.Height := ScaleValue(30);
  B.Caption := 'Select / move'; B.OnClick := SelectToolClicked;
  FToolbox := TERDListBox.Create(Self); FToolbox.Parent := Tools; FToolbox.Align := alClient;
  for I := Ord(Low(TERDPCBItemKind)) to Ord(High(TERDPCBItemKind)) do FToolbox.Items.Add(ERDPCBItemKindName(TERDPCBItemKind(I)));
  FToolbox.ItemIndex := 0; FToolbox.OnMouseDown := ToolboxMouseDown; FToolbox.OnDblClick := ToolboxDoubleClick; FToolbox.OnClick := ToolboxClick;
  FToolbox.OnStartDrag := ToolboxStartDrag; FToolbox.OnEndDrag := ToolboxEndDrag;
  FInspector := TERDInspector.Create(Self); FInspector.Parent := Self; FInspector.Align := alRight; FInspector.Width := ScaleValue(310);
  FInspector.OnPropertyButtonClick := PropertyButtonClicked;
  FInspector.OnPropertyChanging := PropertyChanging; FInspector.OnPropertyChanged := PropertyChanged;
  FDesigner := TERDPCBDesigner.Create(Self); FDesigner.Parent := Self; FDesigner.Align := alClient;
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
    5: begin FDesigner.TemplateEditMode := Choice.Checked; FDesigner.Scene.ClearSelection; SelectionChanged(Self); end;
  end;
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
          end;
        end;
    finally FInspector.EndUpdate; end;
  end;
  FStatus.Caption := 'Designer modified - Ctrl+Z/Y undo/redo; Ctrl+C/V copy/paste; Delete removes unlocked objects.'; end;
procedure TPCBPreview.SelectionChanged(Sender: TObject);
var Category: TERDInspectorCategory; P: TERDPCBPad; Value: TERDPCBDesignItem; Index, I, T: Integer;
    Channel: TERDPCBChannel; Group: TERDPCBTerminal; Text: string; Row: TERDInspectorProperty;
  procedure Field(const Name, Text: string; Tag: Integer; Kind: TERDInspectorValueKind = ivText);
  begin Row := Category.AddProperty(Name, Text, Kind); Row.Tag := Tag; end;
  procedure PinField(const Name, Text: string; Tag: Integer);
  begin
    Field(Name, Text, Tag, ivPickList);
  end;
begin
  FInspector.Clear; Index := FDesigner.Scene.PrimaryIndex;
  if Index >= 0 then
  begin
    Value := FDesigner.Scene.Item(Index); Category := FInspector.AddCategory('Designer component');
    Category.AddProperty('Kind', ERDPCBItemKindName(Value.Kind), ivReadOnly);
    Category.AddProperty('ID', Value.ID, ivReadOnly);
    Field('Name', Value.Name, 1);
    if Value.Kind = piNote then
    begin
      Field('Text', Value.Caption, 2, ivEllipsis); Row.ReadOnly := True;
      Field('Locked', '', 13, ivCheck); Row.Checked := Value.Locked;
      Category := FInspector.AddCategory('Position and size (mm)');
      Field('X', FloatToStr(Value.X, TFormatSettings.Invariant), 4); Field('Y', FloatToStr(Value.Y, TFormatSettings.Invariant), 5);
      Field('Width', FloatToStr(Value.Width, TFormatSettings.Invariant), 6); Field('Height', FloatToStr(Value.Height, TFormatSettings.Invariant), 7);
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
  end;
end;
procedure TPCBPreview.PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
begin
  try
    if (AProperty.Tag >= 8) and (AProperty.Tag <= 11) then
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
    if (AProperty.Tag >= 8) and (AProperty.Tag <= 11) then
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
procedure TPCBPreview.ToolboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  if (Button <> mbLeft) or FDesigner.TemplateEditMode then Exit;
  Index := FToolbox.ItemAt(X,Y);
  if Index >= 0 then begin FToolbox.ItemIndex := Index; FToolbox.BeginDrag(False); end;
end;
procedure TPCBPreview.ToolboxClick(Sender: TObject);
begin
  FToolDragging := False;
  if (FToolbox.ItemIndex >= 0) and not FDesigner.TemplateEditMode then
  begin FDesigner.BeginPlacement(TERDPCBItemKind(FToolbox.ItemIndex)); FDesigner.SetFocus; end;
end;
procedure TPCBPreview.ToolboxStartDrag(Sender: TObject; var DragObject: TDragObject);
begin FToolDragging := True; end;
procedure TPCBPreview.ToolboxEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FToolDragging := False; FDesigner.CancelPlacement; FToolbox.ItemIndex := -1;
end;
procedure TPCBPreview.ToolboxDoubleClick(Sender: TObject);
begin ToolboxClick(Sender); end;
procedure TPCBPreview.SelectToolClicked(Sender: TObject);
begin FDesigner.CancelPlacement; FToolbox.ItemIndex := -1; end;
procedure TPCBPreview.PlacementChanged(Sender: TObject);
begin
  if FDesigner.Placing then
  begin
    if (FHand <> nil) and FHand.Checked then FHand.Checked := False;
    FStatus.Caption := 'Place ' + ERDPCBItemKindName(FDesigner.PlacementKind) +
      ' - click canvas; Shift+click repeats; Escape or right-click cancels.';
  end
  else
  begin
    if not FToolDragging then FToolbox.ItemIndex := -1;
    FStatus.Caption := 'Select and move objects, or choose another toolbox item.';
  end;
end;
procedure TPCBPreview.SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = FToolbox) and (FToolbox.ItemIndex >= 0) and not FDesigner.TemplateEditMode;
  if not Accept then Exit;
  if State = dsDragLeave then begin FDesigner.CancelPlacement; Exit; end;
  if not FDesigner.Placing or (FDesigner.PlacementKind <> TERDPCBItemKind(FToolbox.ItemIndex)) then
    FDesigner.BeginPlacement(TERDPCBItemKind(FToolbox.ItemIndex));
  FDesigner.PreviewPlacementAt(Point(X,Y));
end;
procedure TPCBPreview.SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = FToolbox) and (FToolbox.ItemIndex >= 0) then
  begin
    FDesigner.InsertAt(TERDPCBItemKind(FToolbox.ItemIndex), Point(X,Y)); FDesigner.CancelPlacement;
  end;
end;
procedure TPCBPreview.ToolbarClicked(Sender: TObject);
var Save: TERDSaveDialog; Open: TERDOpenDialog; TemplateFile: string;
begin
  try
    case TComponent(Sender).Tag of
      20..25: FDesigner.AlignSelected(TERDPCBAlignment(TComponent(Sender).Tag - 20));
      26, 27: FDesigner.DistributeSelected(TComponent(Sender).Tag = 26);
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
