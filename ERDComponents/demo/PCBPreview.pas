unit PCBPreview;
interface
uses
  System.Classes, System.SysUtils, Vcl.Controls, Winapi.Windows, ERD.Components.Control,
  ERD.Components.Panel, ERD.Components.PCBDesigner, ERD.Components.Inspector,
  ERD.Components.Buttons, ERD.Components.FileDialogs;
type
  TPCBPreview = class(TERDPanel)
  private
    FDesigner: TERDPCBDesigner;
    FInspector: TERDInspector;
    FName, FCount, FAction: TERDInspectorProperty;
    FFileName: string;
    procedure SelectionChanged(Sender: TObject);
    procedure BindingChanging(Sender: TObject; AProperty: TERDInspectorProperty;
      var NewValue: string; var Accept: Boolean);
    procedure BindingChanged(Sender: TObject; AProperty: TERDInspectorProperty);
    procedure ToolbarClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
implementation
uses
  System.IOUtils, Vcl.StdCtrls, Vcl.Dialogs;
constructor TPCBPreview.Create(AOwner: TComponent);
var Header: TERDPanel; B: TERDButton; L: TLabel; I: Integer; Base, BoardFile: string;
  procedure Button(const Caption: string; Left, Width, Tag: Integer);
  begin
    B := TERDButton.Create(Self); B.Parent := Header; B.Caption := Caption;
    B.SetBounds(ScaleValue(Left), ScaleValue(8), ScaleValue(Width), ScaleValue(28));
    B.Tag := Tag; B.OnClick := ToolbarClicked;
  end;
begin
  inherited;
  Header := TERDPanel.Create(Self); Header.Parent := Self; Header.Align := alTop; Header.Height := ScaleValue(70);
  Button('Fit board', 8, 90, 0); Button('Select U7.2', 104, 100, 1);
  Button('Save project', 210, 105, 2); Button('Open project', 321, 105, 3);
  L := TLabel.Create(Self); L.Parent := Header;
  L.SetBounds(ScaleValue(10), ScaleValue(43), ScaleValue(800), ScaleValue(20));
  L.Caption := 'Click a pad to inspect it. Wheel: zoom; middle drag: pan. Assignments are configuration only.';
  FInspector := TERDInspector.Create(Self); FInspector.Parent := Self; FInspector.Align := alRight; FInspector.Width := ScaleValue(320);
  FInspector.OnPropertyChanging := BindingChanging; FInspector.OnPropertyChanged := BindingChanged;
  FDesigner := TERDPCBDesigner.Create(Self); FDesigner.Parent := Self; FDesigner.Align := alClient;
  FDesigner.OnSelectionChange := SelectionChanged;
  BoardFile := ''; Base := ExtractFilePath(ParamStr(0));
  for I := 0 to 3 do
  begin
    if FileExists(IncludeTrailingPathDelimiter(Base) + 'pcb\board.json') then
    begin BoardFile := IncludeTrailingPathDelimiter(Base) + 'pcb\board.json'; Break; end;
    Base := ExtractFileDir(ExcludeTrailingPathDelimiter(Base));
  end;
  if BoardFile <> '' then
  begin
    try FDesigner.LoadBoard(BoardFile); FDesigner.SelectedPin := 'U7.2';
    except on E: Exception do L.Caption := 'Board import: ' + E.Message; end;
  end
  else L.Caption := 'Board package not found: keep the demo pcb folder beside the executable or its build folders.';
end;
procedure TPCBPreview.SelectionChanged(Sender: TObject);
var Category: TERDInspectorCategory; P: TERDPCBPad; C: TERDPCBChannel; Index: Integer;
begin
  FName := nil; FCount := nil; FAction := nil;
  FInspector.Clear;
  P := FDesigner.SelectedPad; Category := FInspector.AddCategory('Physical pin');
  if P.ID = '' then begin Category.AddProperty('Selection', 'Click a pad', ivReadOnly); Exit; end;
  Category.AddProperty('Pin', P.ID, ivReadOnly); Category.AddProperty('Net', P.Net, ivReadOnly);
  Index := FDesigner.ChannelIndex(P.ID);
  if Index < 0 then
  begin Category.AddProperty('Assignment', 'Reserved / not a verified output', ivReadOnly); Exit; end;
  C := FDesigner.Channel(Index);
  Category.AddProperty('MCU signal', C.GPIO, ivReadOnly);
  Category.AddProperty('Direction', 'Buffered output', ivReadOnly);
  Category.AddProperty('Route', C.Route, ivReadOnly);
  Category := FInspector.AddCategory('WS2812 output');
  FName := Category.AddProperty('Name', C.Name);
  FCount := Category.AddProperty('LED count', IntToStr(C.LEDCount));
  FAction := Category.AddProperty('Action', C.Action, ivPickList);
  FAction.PickList.Add(''); FAction.PickList.Add('showPick');
end;
procedure TPCBPreview.BindingChanging(Sender: TObject; AProperty: TERDInspectorProperty;
  var NewValue: string; var Accept: Boolean);
var Count, Index: Integer; C: TERDPCBChannel; Name, Action: string;
begin
  Index := FDesigner.ChannelIndex(FDesigner.SelectedPin); Accept := Index >= 0; if not Accept then Exit;
  C := FDesigner.Channel(Index); Name := C.Name; Action := C.Action; Count := C.LEDCount;
  if AProperty = FName then begin NewValue := Trim(NewValue); Name := NewValue; end;
  if AProperty = FCount then
  begin Accept := TryStrToInt(NewValue, Count) and (Count >= 1) and (Count <= 65535); if not Accept then Exit; end;
  if AProperty = FAction then Action := NewValue;
  // Validate through the same path as project loading; roll back the preview.
  try
    FDesigner.SetBinding(Index, Name, Action, Count);
    FDesigner.SetBinding(Index, C.Name, C.Action, C.LEDCount);
  except on E: Exception do begin Accept := False; ShowMessage(E.Message); end; end;
end;
procedure TPCBPreview.BindingChanged(Sender: TObject; AProperty: TERDInspectorProperty);
var Index: Integer;
begin
  Index := FDesigner.ChannelIndex(FDesigner.SelectedPin);
  if (Index < 0) or (FName = nil) or (FCount = nil) or (FAction = nil) then Exit;
  FDesigner.SetBinding(Index, FName.Value, FAction.Value, StrToInt(FCount.Value));
end;
procedure TPCBPreview.ToolbarClicked(Sender: TObject);
var Save: TERDSaveDialog; Open: TERDOpenDialog;
begin
  try
    case TComponent(Sender).Tag of
      0: FDesigner.FitBoard;
      1: FDesigner.SelectedPin := 'U7.2';
      2:
        begin
          Save := TERDSaveDialog.Create(Self);
          try
            Save.Filter := 'PCB project|*.pcbproject.json'; Save.DefaultExt := 'pcbproject.json';
            Save.FileName := FFileName;
            if Save.Execute then begin FDesigner.SaveProject(Save.FileName); FFileName := Save.FileName; end;
          finally Save.Free; end;
        end;
      3:
        begin
          Open := TERDOpenDialog.Create(Self);
          try
            Open.Filter := 'PCB project|*.pcbproject.json|JSON|*.json';
            if Open.Execute then begin FDesigner.LoadProject(Open.FileName); FFileName := Open.FileName; end;
          finally Open.Free; end;
        end;
    end;
  except on E: Exception do ShowMessage(E.Message); end;
end;
end.
