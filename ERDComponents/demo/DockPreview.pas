unit DockPreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Types, System.IOUtils, Vcl.Controls,
  Vcl.StdCtrls, Vcl.Dialogs, ERD.Components.Panel, ERD.Components.Control,
  ERD.Components.DockHost, ERD.Components.CodeEditor, ERD.Components.DebugConsole,
  ERD.Components.Memo, ERD.Components.ValueTree, ERD.Components.Ribbon;
type
  TDockPreview = class(TERDPanel)
  private
    FHost: TERDDockHost;
    FStatus: TLabel;
    FEditor: TERDCodeEditor;
    FConsole: TERDDebugConsole;
    FDetails, FFiles: TERDMemo;
    FValues: TERDValueTree;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FDefaultLayout, FSavedLayout: string;
    procedure LayoutChanged(Sender: TObject);
    procedure Activated(Sender: TObject; const PanelID: string);
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
constructor TDockPreview.Create(AOwner: TComponent);
var Values: TArray<TERDInspectValue>;
begin
  inherited;
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop; FStatus.AutoSize := False; FStatus.Height := ScaleValue(32);
  FHost := TERDDockHost.Create(Self); FHost.Parent := Self; FHost.Align := alClient;
  FEditor := TERDCodeEditor.Create(Self); FEditor.Text := '// Docking demo: no runtime connected'+sLineBreak+'const status = { enabled: true, count: 12 };';
  FConsole := TERDDebugConsole.Create(Self); FConsole.AddEntry(0,Now,dlsInfo,'demo','Drag a panel tab to an edge or center. Drop outside to float. Right-click a tab to pin, auto-hide or hide.');
  FDetails := TERDMemo.Create(Self); FDetails.ReadOnly := True; FDetails.Text := 'Independent inspector content'+sLineBreak+sLineBreak+'Select panels via tabs. Docking changes presentation only.';
  FFiles := TERDMemo.Create(Self); FFiles.ReadOnly := True; FFiles.Text := 'Demo project'+sLineBreak+'  main.ts'+sLineBreak+'  board.template.json'+sLineBreak+'  README.md';
  FValues := TERDValueTree.Create(Self); SetLength(Values,2); Values[0].ID := 'enabled'; Values[0].Name := 'enabled'; Values[0].ValueText := 'true'; Values[0].TypeName := 'boolean';
  Values[1].ID := 'count'; Values[1].Name := 'count'; Values[1].ValueText := '12'; Values[1].TypeName := 'number'; FValues.LoadSnapshot('dock-demo',0,Values);
  FHost.BeginUpdate;
  try
    FHost.AddPanel('editor','Code editor',FEditor,dzCenter).Closable := False;
    FHost.AddPanel('files','Project',FFiles,dzLeft);
    FHost.AddPanel('inspector','Inspector',FDetails,dzRight);
    FHost.AddPanel('locals','Locals',FValues,dzRight);
    FHost.AddPanel('console','Console',FConsole,dzBottom);
  finally FHost.EndUpdate; end;
  FDefaultLayout := FHost.SaveLayout; FHost.OnLayoutChanged := LayoutChanged; FHost.OnPanelActivated := Activated;
  LayoutChanged(Self); ThemeChanged;
end;
destructor TDockPreview.Destroy;
begin
  AttachRibbon(nil); if FHost <> nil then begin FHost.OnLayoutChanged := nil; FHost.OnPanelActivated := nil; end;
  // Release the host first, so it can restore borrowed controls while they exist.
  FreeAndNil(FHost); inherited;
end;
procedure TDockPreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText;
  if FHost <> nil then begin FHost.Theme := Theme; FHost.ThemeChanged; end;
  if FEditor <> nil then FEditor.Theme := Theme;
  if FConsole <> nil then FConsole.Theme := Theme;
  if FDetails <> nil then FDetails.Theme := Theme;
  if FFiles <> nil then FFiles.Theme := Theme;
  if FValues <> nil then FValues.Theme := Theme;
end;
procedure TDockPreview.LayoutChanged(Sender: TObject);
begin FStatus.Caption := 'Docking demo | drag tabs; right-click for dock / float / auto-hide / hide. Layout contains no document text.'; end;
procedure TDockPreview.Activated(Sender: TObject; const PanelID: string);
begin FStatus.Caption := 'Docking demo | active panel: '+PanelID; end;
procedure TDockPreview.RibbonClick(Sender: TObject);
var Error: string; P: TPoint; OpenDialog: TOpenDialog; SaveDialog: TSaveDialog;
begin
  case TERDRibbonItem(Sender).Tag of
    0: FHost.LoadLayout(FDefaultLayout,Error);
    1: begin FSavedLayout := FHost.SaveLayout; FStatus.Caption := 'Layout saved in memory'; end;
    2: if FSavedLayout <> '' then if not FHost.LoadLayout(FSavedLayout,Error) then FStatus.Caption := Error;
    3: FHost.AutoHidePanel('inspector',dzRight);
    4: FHost.DockPanel('inspector',dzRight);
    5: begin P := ClientToScreen(Point(ScaleValue(40),ScaleValue(80))); FHost.FloatPanel('console',Rect(P.X,P.Y,P.X+ScaleValue(640),P.Y+ScaleValue(420))); end;
    6: FHost.ShowPanel('console');
    7: FHost.DockPanel('console',dzBottom);
    8: FHost.HidePanel('locals');
    9: FHost.ShowPanel('locals');
    10: begin
      SaveDialog := TSaveDialog.Create(nil);
      try SaveDialog.Filter := 'Dock layout (*.json)|*.json'; SaveDialog.DefaultExt := 'json'; SaveDialog.Options := SaveDialog.Options+[ofOverwritePrompt];
        if SaveDialog.Execute then TFile.WriteAllText(SaveDialog.FileName,FHost.SaveLayout,TEncoding.UTF8);
      finally SaveDialog.Free; end;
    end;
    11: begin
      OpenDialog := TOpenDialog.Create(nil);
      try OpenDialog.Filter := 'Dock layout (*.json)|*.json'; if OpenDialog.Execute then if not FHost.LoadLayout(TFile.ReadAllText(OpenDialog.FileName,TEncoding.UTF8),Error) then FStatus.Caption := 'Invalid layout: '+Error;
      finally OpenDialog.Free; end;
    end;
  end;
end;
procedure TDockPreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..11] of string = ('Reset layout','Remember layout','Restore layout','Auto-hide inspector','Pin inspector','Float console','Show console','Dock console','Hide locals','Show locals','Save layout...','Open layout...');
  Descriptions: array[0..11] of string = ('Restores the initial panel arrangement.','Remembers the current panel arrangement for this demo session.','Restores the remembered panel arrangement.','Collapses the inspector into a side tab. Hover or click to reveal it.','Keeps the inspector visible in the right dock group.','Opens the console in a separate themed window.','Shows the console in its previous dock or floating state.','Returns the console to the bottom tab group.','Hides the locals panel without destroying its content.','Shows the locals panel again.','Saves the panel arrangement to a layout file.','Loads a saved panel arrangement.');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Docking (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Workspace layout';
  for I := 0 to High(Captions) do
  begin
    if I = 3 then begin Group := FTab.Groups.Add; Group.Caption := 'Panel actions'; end;
    Item := Group.Items.Add; Item.Caption := Captions[I]; Item.Hint := Captions[I]+'|'+Descriptions[I]; Item.Size := risSmall; Item.Tag := I; Item.OnClick := RibbonClick;
  end;
end;
procedure TDockPreview.SetRibbonActive(Value: Boolean);
var I, Index: Integer;
begin
  if (FRibbon = nil) or (FTab = nil) then Exit;
  Index := 0; for I := 0 to FTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(Index);
  if not Value and FTab.Visible and (FRibbon.ActiveTab = Index) then FRibbon.ActiveTab := 0;
  FTab.Visible := Value; if Value then FRibbon.ActiveTab := Index;
end;
procedure TDockPreview.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FRibbon) then begin FRibbon := nil; FTab := nil; end; end;
end.
