unit DockPreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Types, System.IOUtils, Vcl.Controls,
  System.Math, ERD.Components.FileDialogs, ERD.Components.Panel, ERD.Components.Control,
  ERD.Components.DockHost, ERD.Components.CodeEditor, ERD.Components.DebugConsole,
  ERD.Components.Memo, ERD.Components.ValueTree, ERD.Components.Ribbon,
  ERD.Components.ProjectTree, ERD.Components.DiagnosticList, ERD.Components.CommonControls, ERD.Components.Edits;
type
  TDockPreview = class(TERDPanel)
  private
    FHost: TERDDockHost;
    FStatus: TERDPanel;
    FDiagnostics: TERDDiagnosticList;
    FDiagnosticHost, FFilters: TERDPanel;
    FSearch: TERDSearchEdit;
    FSeverity: TERDComboBox;
    FChangingSource: Boolean;
    FDiagnosticGeneration: Int64;
    FDiagnosticRequest: TERDDiagnosticRequest;
    FEditor: TERDCodeEditor;
    FConsole: TERDDebugConsole;
    FDetails: TERDMemo;
    FFiles: TERDProjectTree;
    FProjectHost, FProjectFilter: TERDPanel;
    FProjectSearch: TERDSearchEdit;
    FProjectItems: TArray<TERDProjectItem>;
    FProjectRevision: Int64;
    FValues: TERDValueTree;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    FDefaultLayout, FSavedLayout: string;
    procedure LayoutChanged(Sender: TObject);
    procedure Activated(Sender: TObject; const PanelID: string);
    procedure RibbonClick(Sender: TObject);
    procedure LoadDiagnostics;
    procedure CreateProject;
    procedure ProjectSearch(Sender: TObject);
    procedure ProjectOpen(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem);
    procedure ProjectRename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const NewName: string);
    procedure ProjectMove(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
    procedure EditorChanged(Sender: TObject);
    procedure DiagnosticFilter(Sender: TObject);
    procedure DiagnosticsChanged(Sender: TObject);
    procedure NavigateDiagnostic(Sender: TObject; const Context: TERDDiagnosticRequest; const Diagnostic: TERDDiagnostic);
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
const MainSource = '// DEMO DATA - no compiler or runtime connected'#13#10+
  'const ledCount: number = "ten";'#13#10+
  'const unused = 12;'#13#10+
  'function start() {'#13#10+
  '  output.show(ledCount);'#13#10+
  '}'#13#10+
  'start();';
  BoardSource = '// DEMO DATA - board.ts'#13#10+
  'export const pin = 48;'#13#10+
  'export const label = "Status";';
constructor TDockPreview.Create(AOwner: TComponent);
var Values: TArray<TERDInspectValue>;
begin
  inherited;
  FStatus := TERDPanel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop; FStatus.BorderWidth := 0; FStatus.HeaderHeight := 32; FStatus.Height := ScaleValue(32);
  FHost := TERDDockHost.Create(Self); FHost.Parent := Self; FHost.Align := alClient;
  FEditor := TERDCodeEditor.Create(Self); FEditor.DocumentUri := 'demo://main.ts'; FEditor.Text := MainSource;
  FConsole := TERDDebugConsole.Create(Self); FConsole.AddEntry(0,Now,dlsInfo,'demo','Drag a panel tab to an edge or center. Drop outside to float. Right-click a tab to pin, auto-hide or hide.');
  FDetails := TERDMemo.Create(Self); FDetails.ReadOnly := True; FDetails.Text := 'Independent inspector content'+sLineBreak+sLineBreak+'Select panels via tabs. Docking changes presentation only.';
  CreateProject;
  FValues := TERDValueTree.Create(Self); SetLength(Values,2); Values[0].ID := 'enabled'; Values[0].Name := 'enabled'; Values[0].ValueText := 'true'; Values[0].TypeName := 'boolean';
  Values[1].ID := 'count'; Values[1].Name := 'count'; Values[1].ValueText := '12'; Values[1].TypeName := 'number'; FValues.LoadSnapshot('dock-demo',0,Values);
  FDiagnosticHost := TERDPanel.Create(Self); FDiagnosticHost.BorderWidth := 0;
  FFilters := TERDPanel.Create(Self); FFilters.Parent := FDiagnosticHost; FFilters.Align := alTop;
  FFilters.BorderWidth := 0; FFilters.Height := ScaleValue(38); FFilters.Padding.SetBounds(4,3,4,3);
  FSeverity := TERDComboBox.Create(Self); FSeverity.Parent := FFilters; FSeverity.Align := alRight; FSeverity.Width := ScaleValue(150);
  FSeverity.Items.Add('All messages'); FSeverity.Items.Add('Errors'); FSeverity.Items.Add('Warnings'); FSeverity.Items.Add('Information'); FSeverity.Items.Add('Hints');
  FSeverity.ItemIndex := 0; FSeverity.OnChange := DiagnosticFilter;
  FSearch := TERDSearchEdit.Create(Self); FSearch.Parent := FFilters; FSearch.Align := alClient;
  FSearch.TextHint := 'Search diagnostics'; FSearch.Hint := 'Search diagnostics|Filter message, code, source file, origin and severity.'; FSearch.OnSearch := DiagnosticFilter;
  FDiagnostics := TERDDiagnosticList.Create(Self); FDiagnostics.Parent := FDiagnosticHost; FDiagnostics.Align := alClient;
  FDiagnostics.OnNavigate := NavigateDiagnostic; FDiagnostics.OnChange := DiagnosticsChanged;
  LoadDiagnostics; FEditor.OnChange := EditorChanged;
  FHost.LeftSize := 300;
  FHost.BeginUpdate;
  try
    FHost.AddPanel('editor','Code editor',FEditor,dzCenter).Closable := False;
    FHost.AddPanel('files','Project',FProjectHost,dzLeft);
    FHost.AddPanel('inspector','Inspector',FDetails,dzRight);
    FHost.AddPanel('locals','Locals',FValues,dzRight);
    FHost.AddPanel('console','Console',FConsole,dzBottom);
    FHost.AddPanel('diagnostics','Diagnostics (demo)',FDiagnosticHost,dzBottom);
    FHost.ActivatePanel('diagnostics');
  finally FHost.EndUpdate; end;
  FDefaultLayout := FHost.SaveLayout; FHost.OnLayoutChanged := LayoutChanged; FHost.OnPanelActivated := Activated;
  LayoutChanged(Self); ThemeChanged;
end;
destructor TDockPreview.Destroy;
begin
  AttachRibbon(nil); if FEditor <> nil then FEditor.OnChange := nil;
  if FFiles <> nil then begin FFiles.OnOpenItem := nil; FFiles.OnRenameItem := nil; FFiles.OnMoveItem := nil; end;
  if FProjectSearch <> nil then FProjectSearch.OnSearch := nil;
  if FDiagnostics <> nil then begin FDiagnostics.OnChange := nil; FDiagnostics.OnNavigate := nil; end;
  if FSearch <> nil then FSearch.OnSearch := nil; if FSeverity <> nil then FSeverity.OnChange := nil;
  if FHost <> nil then begin FHost.OnLayoutChanged := nil; FHost.OnPanelActivated := nil; end;
  // Release the host first, so it can restore borrowed controls while they exist.
  FreeAndNil(FHost); inherited;
end;
procedure TDockPreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Theme := Theme;
  if FHost <> nil then begin FHost.Theme := Theme; FHost.ThemeChanged; end;
  if FEditor <> nil then FEditor.Theme := Theme;
  if FConsole <> nil then FConsole.Theme := Theme;
  if FDetails <> nil then FDetails.Theme := Theme;
  if FFiles <> nil then FFiles.Theme := Theme;
  if FProjectHost <> nil then FProjectHost.Theme := Theme;
  if FProjectFilter <> nil then FProjectFilter.Theme := Theme;
  if FProjectSearch <> nil then FProjectSearch.Theme := Theme;
  if FValues <> nil then FValues.Theme := Theme;
  if FDiagnosticHost <> nil then FDiagnosticHost.Theme := Theme;
  if FFilters <> nil then FFilters.Theme := Theme;
  if FSearch <> nil then FSearch.Theme := Theme;
  if FSeverity <> nil then FSeverity.Theme := Theme;
  if FDiagnostics <> nil then FDiagnostics.Theme := Theme;
end;
procedure TDockPreview.CreateProject;
  procedure Add(Index: Integer; const ID, ParentID, Name, Uri: string; Kind: TERDProjectItemKind; Status: TERDProjectItemStatuses);
  begin
    FProjectItems[Index].ID := ID; FProjectItems[Index].ParentID := ParentID; FProjectItems[Index].Name := Name;
    FProjectItems[Index].Uri := Uri; FProjectItems[Index].Kind := Kind; FProjectItems[Index].Status := Status;
    FProjectItems[Index].CanRename := (Kind <> pikProject); FProjectItems[Index].CanMove := (Kind <> pikProject);
  end;
begin
  FProjectHost := TERDPanel.Create(Self); FProjectHost.BorderWidth := 0;
  FProjectFilter := TERDPanel.Create(Self); FProjectFilter.Parent := FProjectHost; FProjectFilter.Align := alTop;
  FProjectFilter.BorderWidth := 0; FProjectFilter.Height := ScaleValue(38); FProjectFilter.Padding.SetBounds(4,3,4,3);
  FProjectSearch := TERDSearchEdit.Create(Self); FProjectSearch.Parent := FProjectFilter; FProjectSearch.Align := alClient;
  FProjectSearch.TextHint := 'Search project'; FProjectSearch.OnSearch := ProjectSearch;
  FFiles := TERDProjectTree.Create(Self); FFiles.Parent := FProjectHost; FFiles.Align := alClient;
  FFiles.OnOpenItem := ProjectOpen; FFiles.OnRenameItem := ProjectRename; FFiles.OnMoveItem := ProjectMove;
  SetLength(FProjectItems,9);
  Add(0,'project','','Pick-to-light demo','',pikProject,[]);
  Add(1,'src','project','src','',pikFolder,[]); Add(2,'templates','project','templates','',pikFolder,[]);
  Add(3,'resources','project','resources','',pikFolder,[]);
  Add(4,'main','src','main.ts','demo://main.ts',pikTypeScript,[pisError]);
  Add(5,'board','src','board.ts','demo://board.ts',pikTypeScript,[pisModified]);
  Add(6,'template','templates','board.template.json','demo://board.template.json',pikPCBTemplate,[pisReadOnly]);
  Add(7,'readme','resources','README.md','demo://README.md',pikResource,[]);
  Add(8,'missing','resources','labels.json','demo://labels.json',pikResource,[pisMissing]);
  FProjectRevision := 1; FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
end;
procedure TDockPreview.ProjectSearch(Sender: TObject);
begin FFiles.SearchText := FProjectSearch.Text; end;
procedure TDockPreview.ProjectOpen(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem);
var Source: string;
begin
  if Item.Uri = 'demo://main.ts' then Source := MainSource
  else if Item.Uri = 'demo://board.ts' then Source := BoardSource
  else if Item.Uri = 'demo://board.template.json' then Source := '{ "demo": true, "board": "pick-to-light" }'
  else Source := '# Demo project'+sLineBreak+'These are synthetic resources. No files are read or written.';
  FChangingSource := True;
  try FEditor.ReadOnly := pisReadOnly in Item.Status; FEditor.DocumentUri := Item.Uri; FEditor.Text := Source;
  finally FChangingSource := False; end;
  FHost.ActivatePanel('editor'); if FEditor.CanFocus then FEditor.SetFocus;
  FStatus.Caption := 'DEMO DATA | opened '+Item.Name;
end;
procedure TDockPreview.ProjectRename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const NewName: string);
var I: Integer;
begin
  for I := 0 to High(FProjectItems) do if FProjectItems[I].ID = Item.ID then FProjectItems[I].Name := NewName;
  Inc(FProjectRevision); FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
  FStatus.Caption := 'DEMO DATA | renamed '+Item.Name+' to '+NewName+' in memory';
end;
procedure TDockPreview.ProjectMove(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
var I: Integer;
begin
  for I := 0 to High(FProjectItems) do if FProjectItems[I].ID = Item.ID then FProjectItems[I].ParentID := ParentID;
  Inc(FProjectRevision); FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
  FStatus.Caption := 'DEMO DATA | moved '+Item.Name+' in memory';
end;
procedure TDockPreview.EditorChanged(Sender: TObject);
begin if not FChangingSource and (FDiagnostics <> nil) then FDiagnostics.MarkStale; end;
procedure TDockPreview.LoadDiagnostics;
var Values: TArray<TERDDiagnostic>;
  procedure Add(Index: Integer; Severity: TERDDiagnosticSeverity; const Code, MessageText, Uri: string; Line, Column: Integer);
  begin
    Values[Index].ID := 'sample-'+IntToStr(Index); Values[Index].Severity := Severity;
    Values[Index].Code := Code; Values[Index].MessageText := MessageText; Values[Index].Origin := 'Demo fixture';
    Values[Index].DocumentUri := Uri; Values[Index].Line := Line; Values[Index].Column := Column;
  end;
begin
  FChangingSource := True;
  try FEditor.ReadOnly := False; FEditor.DocumentUri := 'demo://main.ts'; FEditor.Text := MainSource;
  finally FChangingSource := False; end;
  Inc(FDiagnosticGeneration); FDiagnosticRequest := FDiagnostics.BeginSnapshot(1,FDiagnosticGeneration);
  SetLength(Values,6);
  Add(0,dgsError,'DEMO2322','Type string is not assignable to type number.','demo://main.ts',2,26);
  Add(1,dgsWarning,'DEMO6133','unused is declared but its value is never read.','demo://main.ts',3,7);
  Add(2,dgsError,'DEMO2304','Cannot find name output.','demo://main.ts',5,3);
  Add(3,dgsInformation,'DEMO001','Board pin is configurable through the template.','demo://board.ts',2,14);
  Add(4,dgsHint,'DEMO002','Consider documenting this label.','demo://board.ts',3,14);
  Add(5,dgsInformation,'DEMO000','Synthetic diagnostics: no compiler was invoked.','',0,0);
  FDiagnostics.PublishSnapshot(FDiagnosticRequest,Values);
end;
procedure TDockPreview.DiagnosticFilter(Sender: TObject);
begin
  if FDiagnostics = nil then Exit;
  FDiagnostics.SearchText := FSearch.Text;
  if FSeverity.ItemIndex <= 0 then FDiagnostics.Severities := [dgsError,dgsWarning,dgsInformation,dgsHint]
  else FDiagnostics.Severities := [TERDDiagnosticSeverity(FSeverity.ItemIndex-1)];
end;
procedure TDockPreview.DiagnosticsChanged(Sender: TObject);
var State: string;
begin
  State := 'ready'; if FDiagnostics.Pending then State := 'loading' else if FDiagnostics.ErrorText <> '' then State := 'unavailable' else if FDiagnostics.Stale then State := 'stale';
  FStatus.Caption := Format('DEMO DATA | %d errors, %d warnings | %d/%d visible | %s',
    [FDiagnostics.SeverityCount(dgsError),FDiagnostics.SeverityCount(dgsWarning),FDiagnostics.VisibleDiagnosticCount,FDiagnostics.DiagnosticCount,State]);
end;
procedure TDockPreview.NavigateDiagnostic(Sender: TObject; const Context: TERDDiagnosticRequest; const Diagnostic: TERDDiagnostic);
var Position, Offset: Integer; Source: string;
begin
  if Diagnostic.DocumentUri = 'demo://main.ts' then Source := MainSource
  else if Diagnostic.DocumentUri = 'demo://board.ts' then Source := BoardSource else Exit;
  FEditor.ReadOnly := False;
  if FEditor.DocumentUri <> Diagnostic.DocumentUri then
  begin
    FChangingSource := True;
    try FEditor.DocumentUri := Diagnostic.DocumentUri; FEditor.Text := Source;
    finally FChangingSource := False; end;
  end;
  FHost.ActivatePanel('editor');
  if FEditor.RevealDebugLine(Diagnostic.Line) then
  begin
    Source := FEditor.Text; Position := FEditor.SelStart; Offset := 0;
    while (Offset < Max(0,Diagnostic.Column-1)) and (Position+Offset < Length(Source)) do
    begin if CharInSet(Source[Position+Offset+1],[#10,#13]) then Break; Inc(Offset); end;
    FEditor.SelStart := Position+Offset;
  end;
  if FEditor.CanFocus then FEditor.SetFocus;
  FStatus.Caption := 'DEMO DATA | '+Diagnostic.DocumentUri+':'+IntToStr(Diagnostic.Line)+' - '+Diagnostic.MessageText;
end;
procedure TDockPreview.LayoutChanged(Sender: TObject);
begin FStatus.Caption := 'Docking demo | drag tabs; right-click for dock / float / auto-hide / hide. Layout contains no document text.'; end;
procedure TDockPreview.Activated(Sender: TObject; const PanelID: string);
begin FStatus.Caption := 'Docking demo | active panel: '+PanelID; end;
procedure TDockPreview.RibbonClick(Sender: TObject);
var Error: string; P: TPoint; OpenDialog: TERDOpenDialog; SaveDialog: TERDSaveDialog;
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
      SaveDialog := TERDSaveDialog.Create(nil);
      try SaveDialog.Theme := Theme; SaveDialog.Filter := 'Dock layout (*.json)|*.json'; SaveDialog.DefaultExt := 'json'; SaveDialog.Options := SaveDialog.Options+[foOverwritePrompt];
        if SaveDialog.Execute then TFile.WriteAllText(SaveDialog.FileName,FHost.SaveLayout,TEncoding.UTF8);
      finally SaveDialog.Free; end;
    end;
    11: begin
      OpenDialog := TERDOpenDialog.Create(nil);
      try OpenDialog.Theme := Theme; OpenDialog.Filter := 'Dock layout (*.json)|*.json'; if OpenDialog.Execute then if not FHost.LoadLayout(TFile.ReadAllText(OpenDialog.FileName,TEncoding.UTF8),Error) then FStatus.Caption := 'Invalid layout: '+Error;
      finally OpenDialog.Free; end;
    end;
    12: begin LoadDiagnostics; FHost.ShowPanel('diagnostics'); FHost.ActivatePanel('diagnostics'); end;
    13: begin Inc(FDiagnosticGeneration); FDiagnosticRequest := FDiagnostics.BeginSnapshot(1,FDiagnosticGeneration); end;
    14: FDiagnostics.FailSnapshot(FDiagnosticRequest,'Demo compiler unavailable');
    15: FDiagnostics.ClearDiagnostics;
    16: FDiagnostics.GroupByFile := not FDiagnostics.GroupByFile;
    17: FDiagnostics.NavigateNext;
    18: FDiagnostics.NavigateNext(True);
    19: FDiagnostics.CopyVisibleDiagnostics;
  end;
end;
procedure TDockPreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..19] of string = ('Reset layout','Remember layout','Restore layout','Auto-hide inspector','Pin inspector','Float console','Show console','Dock console','Hide locals','Show locals','Save layout...','Open layout...','Load diagnostics','Loading','Unavailable','Clear diagnostics','Group by file','Next message','Previous message','Copy messages');
  Descriptions: array[0..19] of string = ('Restores the initial panel arrangement.','Remembers the current panel arrangement for this demo session.','Restores the remembered panel arrangement.','Collapses the inspector into a side tab. Hover or click to reveal it.','Keeps the inspector visible in the right dock group.','Opens the console in a separate themed window.','Shows the console in its previous dock or floating state.','Returns the console to the bottom tab group.','Hides the locals panel without destroying its content.','Shows the locals panel again.','Saves the panel arrangement to a layout file.','Loads a saved panel arrangement.','Loads synthetic errors, warnings, information and hints.','Starts a new diagnostics request and marks previous results stale.','Fails the pending request while retaining previous results.','Clears the diagnostics view.','Toggles file grouping.','Navigates to the next visible source diagnostic.','Navigates to the previous visible source diagnostic.','Copies all matching diagnostic messages.');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Docking (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Workspace layout';
  for I := 0 to High(Captions) do
  begin
    if I = 3 then begin Group := FTab.Groups.Add; Group.Caption := 'Panel actions'; end;
    if I = 12 then begin Group := FTab.Groups.Add; Group.Caption := 'Diagnostics (demo)'; end;
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
