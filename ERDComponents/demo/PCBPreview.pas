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
    FAuthoring: TERDCheckBox;
    FFileName: string;
    procedure ViewOptionChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure ConnectionError(Sender: TObject; const MessageText: string);
    procedure DesignChanged(Sender: TObject);
    procedure PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
    procedure PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
    function EditedItem(AProperty: TERDInspectorProperty; const Text: string): TERDPCBDesignItem;
    procedure ToolbarClicked(Sender: TObject);
    procedure ToolboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToolboxDoubleClick(Sender: TObject);
    procedure SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ThemeChanged; override;
  end;
implementation
uses System.Math, Vcl.Dialogs;
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
  Header := TERDPanel.Create(Self); Header.Parent := Self; Header.Align := alTop; Header.Height := ScaleValue(78);
  Button('Fit', 8, 54, 0); Button('Undo', 68, 62, 4); Button('Redo', 136, 62, 5);
  Button('Save project', 204, 104, 2); Button('Open project', 314, 104, 3);
  Button('Load template', 424, 112, 6); Button('Save template', 542, 112, 7);
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alBottom;
  FStatus.AutoSize := False; FStatus.Height := ScaleValue(25);
  Tools := TERDPanel.Create(Self); Tools.Parent := Self; Tools.Align := alLeft; Tools.Width := ScaleValue(145);
  FToolbox := TERDListBox.Create(Self); FToolbox.Parent := Tools; FToolbox.Align := alClient;
  for I := Ord(Low(TERDPCBItemKind)) to Ord(High(TERDPCBItemKind)) do FToolbox.Items.Add(ERDPCBItemKindName(TERDPCBItemKind(I)));
  FToolbox.ItemIndex := 0; FToolbox.OnMouseDown := ToolboxMouseDown; FToolbox.OnDblClick := ToolboxDoubleClick;
  FInspector := TERDInspector.Create(Self); FInspector.Parent := Self; FInspector.Align := alRight; FInspector.Width := ScaleValue(310);
  FInspector.OnPropertyChanging := PropertyChanging; FInspector.OnPropertyChanged := PropertyChanged;
  FDesigner := TERDPCBDesigner.Create(Self); FDesigner.Parent := Self; FDesigner.Align := alClient;
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
  Choice.Tag := 6; Choice.OnChange := ViewOptionChanged;
  BoardFile := ''; Base := ExtractFilePath(ParamStr(0));
  for I := 0 to 3 do
  begin
    if FileExists(IncludeTrailingPathDelimiter(Base) + 'pcb\board.json') then
    begin BoardFile := IncludeTrailingPathDelimiter(Base) + 'pcb\board.json'; Break; end;
    Base := ExtractFileDir(ExcludeTrailingPathDelimiter(Base));
  end;
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
procedure TPCBPreview.DesignChanged(Sender: TObject);
begin FStatus.Caption := 'Designer modified - Ctrl+Z/Y undo/redo; Ctrl+C/V copy/paste; Delete removes unlocked objects.'; end;
procedure TPCBPreview.SelectionChanged(Sender: TObject);
var Category: TERDInspectorCategory; P: TERDPCBPad; Value: TERDPCBDesignItem; Index, I, T: Integer;
    Channel: TERDPCBChannel; Group: TERDPCBTerminal; Text: string; Row: TERDInspectorProperty;
  procedure Field(const Name, Text: string; Tag: Integer; Kind: TERDInspectorValueKind = ivText);
  begin Row := Category.AddProperty(Name, Text, Kind); Row.Tag := Tag; end;
  procedure PinField(const Name, Text: string; Tag: Integer);
  var J: Integer;
  begin
    Field(Name, Text, Tag, ivPickList); Row.PickList.Add('');
    for J := 0 to FDesigner.ChannelCount - 1 do Row.PickList.Add(FDesigner.Channel(J).Pin);
  end;
begin
  FInspector.Clear; Index := FDesigner.Scene.PrimaryIndex;
  if Index >= 0 then
  begin
    Value := FDesigner.Scene.Item(Index); Category := FInspector.AddCategory('Designer component');
    Category.AddProperty('Kind', ERDPCBItemKindName(Value.Kind), ivReadOnly);
    Category.AddProperty('ID', Value.ID, ivReadOnly);
    Field('Name', Value.Name, 1); Field('Caption', Value.Caption, 2);
    Text := 'Value';
    case Value.Kind of piTimer: Text := 'Interval (ms)'; piOutputGroup: Text := 'LED count';
      piUART, piCANBus: Text := 'Bit rate'; piI2C, piSPI: Text := 'Clock (Hz)'; end;
    Field(Text, IntToStr(Value.Value), 3);
    Field('Enabled', '', 12, ivCheck); Row.Checked := Value.Enabled;
    Field('Locked', '', 13, ivCheck); Row.Checked := Value.Locked;
    Category := FInspector.AddCategory('Position and size (mm)');
    Field('X', FloatToStr(Value.X, TFormatSettings.Invariant), 4); Field('Y', FloatToStr(Value.Y, TFormatSettings.Invariant), 5);
    Field('Width', FloatToStr(Value.Width, TFormatSettings.Invariant), 6); Field('Height', FloatToStr(Value.Height, TFormatSettings.Invariant), 7);
    Category := FInspector.AddCategory('Programmable connections');
    Text := 'Terminal / pin'; if Value.Kind in [piUART, piCANBus] then Text := 'TX pin' else if Value.Kind = piI2C then Text := 'SDA pin' else if Value.Kind = piSPI then Text := 'MOSI pin';
    PinField(Text, Value.Pin, 8);
    if Value.Kind in [piUART, piCANBus, piI2C, piSPI] then
    begin
      Text := 'RX pin'; if Value.Kind = piI2C then Text := 'SCL pin' else if Value.Kind = piSPI then Text := 'MISO pin';
      PinField(Text, Value.SecondaryPin, 9);
    end;
    if Value.Kind = piSPI then begin PinField('Clock pin', Value.ClockPin, 10); PinField('Select pin', Value.SelectPin, 11); end;
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
    8: Result.Pin := Text; 9: Result.SecondaryPin := Text; 10: Result.ClockPin := Text; 11: Result.SelectPin := Text;
    12: Result.Enabled := AProperty.Checked; 13: Result.Locked := AProperty.Checked;
  end;
end;
procedure TPCBPreview.PropertyChanging(Sender: TObject; AProperty: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
begin
  try
    if AProperty.Tag < 100 then FDesigner.Scene.ValidateItem(FDesigner.Scene.PrimaryIndex, EditedItem(AProperty, NewValue))
    else
    begin
      if AProperty.Tag = 101 then FDesigner.ValidatePinCapabilities(FDesigner.SelectedPin, ERDPCBParsePinCapabilities(NewValue));
    end;
  except on E: Exception do begin Accept := False; FStatus.Caption := E.Message; end; end;
end;
procedure TPCBPreview.PropertyChanged(Sender: TObject; AProperty: TERDInspectorProperty);
var Channel: TERDPCBChannel; Index: Integer;
begin
  try
    if AProperty.Tag < 100 then FDesigner.Scene.UpdateItem(FDesigner.Scene.PrimaryIndex, EditedItem(AProperty, AProperty.Value))
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
end;
procedure TPCBPreview.ToolboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin if (Button = mbLeft) and (FToolbox.ItemIndex >= 0) then FToolbox.BeginDrag(False); end;
procedure TPCBPreview.ToolboxDoubleClick(Sender: TObject);
begin
  if FToolbox.ItemIndex >= 0 then
    FDesigner.AddDesignItem(TERDPCBItemKind(FToolbox.ItemIndex), FDesigner.ClientToBoard(Point(FDesigner.ClientWidth div 2, FDesigner.ClientHeight div 2)));
end;
procedure TPCBPreview.SurfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin Accept := (Source = FToolbox) and (FToolbox.ItemIndex >= 0) and not FDesigner.TemplateEditMode; end;
procedure TPCBPreview.SurfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin if Source = FToolbox then FDesigner.AddDesignItem(TERDPCBItemKind(FToolbox.ItemIndex), FDesigner.ClientToBoard(Point(X,Y))); end;
procedure TPCBPreview.ToolbarClicked(Sender: TObject);
var Save: TERDSaveDialog; Open: TERDOpenDialog;
begin
  try
    case TComponent(Sender).Tag of
      0: FDesigner.FitBoard; 4: FDesigner.Undo; 5: FDesigner.Redo;
      2, 7:
        begin
          Save := TERDSaveDialog.Create(Self);
          try
            if TComponent(Sender).Tag = 2 then begin Save.Filter := 'Designer project|*.pcbproject.json'; Save.DefaultExt := 'pcbproject.json'; Save.FileName := FFileName; end
            else begin Save.Filter := 'Board template|*.pcbtemplate.json'; Save.DefaultExt := 'pcbtemplate.json'; end;
            if Save.Execute then
              if TComponent(Sender).Tag = 2 then begin FDesigner.SaveProject(Save.FileName); FFileName := Save.FileName; end
              else FDesigner.SaveTemplate(Save.FileName);
          finally Save.Free; end;
        end;
      3, 6:
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
