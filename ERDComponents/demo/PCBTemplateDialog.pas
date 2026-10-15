unit PCBTemplateDialog;
interface
uses System.Classes, ERD.Components.Types, ERD.Components.PCBDesigner;
function BrowsePCBTemplates(Owner: TComponent; const Directory: string; const Palette: TERDThemePalette; out FileName: string): Boolean;
function EditPCBTerminal(Owner: TComponent; Designer: TERDPCBDesigner; const Palette: TERDThemePalette): Boolean;
implementation
uses System.SysUtils, System.Types, System.UITypes, System.IOUtils, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, ERD.Components.Panel, ERD.Components.Buttons, ERD.Components.ItemList,
  ERD.Components.TextInput, ERD.Components.FileDialogs, ERD.Components.PCBLibrary;
type
  TPCBTemplateBrowser = class(TForm)
  private
    FLibrary: TERDPCBTemplateLibrary;
    FList: TERDListBox;
    FPreview: TERDPCBDesigner;
    FInfo: TLabel;
    FLoad: TERDButton;
    FUpdating: Boolean;
    FFileName: string;
    procedure Reload;
    procedure SelectionChanged(Sender: TObject);
    procedure LoadSelected(Sender: TObject);
    procedure ImportClicked(Sender: TObject);
  public
    constructor CreateBrowser(Owner: TComponent; const Directory: string; const Palette: TERDThemePalette);
    destructor Destroy; override;
  end;
  TPCBTerminalEditor = class(TForm)
  private
    FDesigner: TERDPCBDesigner;
    FID, FLabel, FSignal, FMembers: TERDTextInput;
    FError: TLabel;
    procedure SaveClicked(Sender: TObject);
    procedure RemoveClicked(Sender: TObject);
  public
    constructor CreateEditor(Owner: TComponent; Designer: TERDPCBDesigner; const Palette: TERDThemePalette);
  end;
constructor TPCBTemplateBrowser.CreateBrowser(Owner: TComponent; const Directory: string; const Palette: TERDThemePalette);
var Footer: TERDPanel; Button: TERDButton;
begin
  inherited CreateNew(Owner);
  Caption := 'PCB template library'; Position := poOwnerFormCenter;
  ClientWidth := 1000; ClientHeight := 620;
  FLibrary := TERDPCBTemplateLibrary.Create(Directory);
  Footer := TERDPanel.Create(Self); Footer.Parent := Self; Footer.Align := alBottom; Footer.Height := 46;
  Button := TERDButton.Create(Self); Button.Parent := Footer; Button.Caption := 'Import template';
  Button.SetBounds(8, 8, 140, 30); Button.OnClick := ImportClicked;
  FLoad := TERDButton.Create(Self); FLoad.Parent := Footer; FLoad.Caption := 'Load selected';
  FLoad.SetBounds(700, 8, 140, 30); FLoad.Anchors := [akTop, akRight]; FLoad.OnClick := LoadSelected;
  Button := TERDButton.Create(Self); Button.Parent := Footer; Button.Caption := 'Cancel';
  Button.SetBounds(850, 8, 140, 30); Button.Anchors := [akTop, akRight]; Button.ModalResult := mrCancel; Button.Cancel := True;
  FInfo := TLabel.Create(Self); FInfo.Parent := Self; FInfo.Align := alTop;
  FInfo.AutoSize := False; FInfo.Height := 50; FInfo.WordWrap := True;
  FList := TERDListBox.Create(Self); FList.Parent := Self; FList.Align := alLeft; FList.Width := 260;
  FList.OnChange := SelectionChanged; FList.OnDblClick := LoadSelected;
  FPreview := TERDPCBDesigner.Create(Self); FPreview.Parent := Self; FPreview.Align := alClient;
  FPreview.PanMode := True; FPreview.ShowGrid := False;
  Color := Palette.Background; FInfo.Font.Color := Palette.ForegroundText;
  Reload;
end;
destructor TPCBTemplateBrowser.Destroy;
begin
  if FList <> nil then begin FList.OnChange := nil; FList.OnDblClick := nil; end;
  FLibrary.Free; inherited;
end;
procedure TPCBTemplateBrowser.Reload;
var I: Integer; Info: TERDPCBTemplateInfo;
begin
  FUpdating := True;
  try
    FLibrary.Refresh; FList.Items.Clear;
    for I := 0 to FLibrary.Count - 1 do
    begin Info := FLibrary.Item(I); FList.Items.Add(Info.Name); end;
    if FLibrary.Count > 0 then FList.ItemIndex := 0;
  finally FUpdating := False; end;
  SelectionChanged(Self);
end;
procedure TPCBTemplateBrowser.SelectionChanged(Sender: TObject);
var Info: TERDPCBTemplateInfo;
begin
  if FUpdating then Exit;
  FLoad.Enabled := False; FFileName := '';
  if FList.ItemIndex < 0 then
  begin
    FInfo.Caption := 'No templates found. Import a normalized PCB JSON file.';
    if FLibrary.Issues.Count > 0 then FInfo.Caption := FInfo.Caption + sLineBreak + FLibrary.Issues[0];
    Exit;
  end;
  Info := FLibrary.Item(FList.ItemIndex);
  try
    FPreview.LoadBoard(Info.FileName); FPreview.FitBoard;
    FInfo.Caption := Info.Name + '  [' + Info.ID + ']' + sLineBreak +
      IntToStr(Info.PinCount) + ' programmable pins; ' + IntToStr(Info.TerminalCount) + ' terminal groups';
    if FLibrary.Issues.Count > 0 then FInfo.Caption := FInfo.Caption + '; ' + IntToStr(FLibrary.Issues.Count) + ' invalid files skipped';
    FFileName := Info.FileName; FLoad.Enabled := True;
  except on E: Exception do FInfo.Caption := E.Message; end;
end;
procedure TPCBTemplateBrowser.LoadSelected(Sender: TObject);
begin if FLoad.Enabled and (FFileName <> '') then ModalResult := mrOk; end;
procedure TPCBTemplateBrowser.ImportClicked(Sender: TObject);
var Open: TERDOpenDialog; Imported: string; I: Integer;
begin
  Open := TERDOpenDialog.Create(Self);
  try
    Open.Filter := 'PCB template JSON|*.json';
    if Open.Execute then
      try
        Imported := FLibrary.ImportTemplate(Open.FileName); Reload;
        for I := 0 to FLibrary.Count - 1 do if SameText(FLibrary.Item(I).FileName, Imported) then FList.ItemIndex := I;
        SelectionChanged(Self);
      except on E: Exception do FInfo.Caption := E.Message; end;
  finally Open.Free; end;
end;
constructor TPCBTerminalEditor.CreateEditor(Owner: TComponent; Designer: TERDPCBDesigner; const Palette: TERDThemePalette);
var Footer: TERDPanel; Button: TERDButton; Index: Integer; Terminal: TERDPCBTerminal;
  procedure Field(const Caption: string; Y: Integer; out Edit: TERDTextInput);
  var LabelControl: TLabel;
  begin
    LabelControl := TLabel.Create(Self); LabelControl.Parent := Self; LabelControl.Caption := Caption;
    LabelControl.SetBounds(12, Y + 6, 110, 24); LabelControl.Font.Color := Palette.ForegroundText;
    Edit := TERDTextInput.Create(Self); Edit.Parent := Self; Edit.SetBounds(130, Y, 430, 30);
    Edit.Anchors := [akLeft, akTop, akRight];
  end;
begin
  inherited CreateNew(Owner); FDesigner := Designer;
  Caption := 'Terminal group'; Position := poOwnerFormCenter; ClientWidth := 580; ClientHeight := 280;
  Color := Palette.Background;
  Field('Terminal ID', 12, FID); Field('Label', 52, FLabel);
  Field('Signal pin', 92, FSignal); Field('Members (CSV)', 132, FMembers);
  Index := Designer.TerminalIndex(Designer.SelectedPin);
  if Index >= 0 then
  begin
    Terminal := Designer.Terminal(Index); FID.Text := Terminal.ID; FID.ReadOnly := True;
    FLabel.Text := Terminal.LabelText; FSignal.Text := Terminal.SignalPin;
    FMembers.Text := string.Join(',', Terminal.Members);
  end
  else
  begin FID.Text := Designer.SelectedPad.Reference; FSignal.Text := Designer.SelectedPin; FMembers.Text := Designer.SelectedPin; end;
  FError := TLabel.Create(Self); FError.Parent := Self; FError.SetBounds(12, 174, 548, 52);
  FError.AutoSize := False; FError.WordWrap := True; FError.Font.Color := Palette.ForegroundText;
  FError.Caption := 'List every physical contact, including power and ground. The signal must be programmable and included in the members.';
  Footer := TERDPanel.Create(Self); Footer.Parent := Self; Footer.Align := alBottom; Footer.Height := 46;
  Button := TERDButton.Create(Self); Button.Parent := Footer; Button.Caption := 'Remove group';
  Button.SetBounds(12, 8, 134, 30); Button.Enabled := Index >= 0; Button.OnClick := RemoveClicked;
  Button := TERDButton.Create(Self); Button.Parent := Footer; Button.Caption := 'Save';
  Button.SetBounds(366, 8, 90, 30); Button.OnClick := SaveClicked;
  Button := TERDButton.Create(Self); Button.Parent := Footer; Button.Caption := 'Cancel';
  Button.SetBounds(466, 8, 90, 30); Button.ModalResult := mrCancel; Button.Cancel := True;
end;
procedure TPCBTerminalEditor.SaveClicked(Sender: TObject);
var Members: TStringList; Values: TArray<string>; I: Integer;
begin
  Members := TStringList.Create;
  try
    Members.StrictDelimiter := True; Members.Delimiter := ','; Members.DelimitedText := FMembers.Text;
    SetLength(Values, Members.Count); for I := 0 to Members.Count - 1 do Values[I] := Trim(Members[I]);
    try
      FDesigner.DefineTerminal(Trim(FID.Text), FLabel.Text, Trim(FSignal.Text), Values); ModalResult := mrOk;
    except on E: Exception do FError.Caption := E.Message; end;
  finally Members.Free; end;
end;
procedure TPCBTerminalEditor.RemoveClicked(Sender: TObject);
begin
  try FDesigner.RemoveTerminal(FID.Text); ModalResult := mrOk;
  except on E: Exception do FError.Caption := E.Message; end;
end;
function BrowsePCBTemplates(Owner: TComponent; const Directory: string; const Palette: TERDThemePalette; out FileName: string): Boolean;
var Dialog: TPCBTemplateBrowser;
begin
  FileName := ''; Dialog := TPCBTemplateBrowser.CreateBrowser(Owner, Directory, Palette);
  try Result := Dialog.ShowModal = mrOk; if Result then FileName := Dialog.FFileName;
  finally Dialog.Free; end;
end;
function EditPCBTerminal(Owner: TComponent; Designer: TERDPCBDesigner; const Palette: TERDThemePalette): Boolean;
var Dialog: TPCBTerminalEditor;
begin
  if not Designer.TemplateEditMode then raise EArgumentException.Create('Enable template authoring first');
  Dialog := TPCBTerminalEditor.CreateEditor(Owner, Designer, Palette);
  try Result := Dialog.ShowModal = mrOk; finally Dialog.Free; end;
end;
end.
