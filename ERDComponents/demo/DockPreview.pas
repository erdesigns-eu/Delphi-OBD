unit DockPreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Types, System.IOUtils, Vcl.Controls,
  System.Math, System.Generics.Collections, ERD.Components.FileDialogs, ERD.Components.Panel, ERD.Components.Control,
  ERD.Components.DebugModel, ERD.Components.BreakpointPanel, ERD.Components.DocumentHost, ERD.Components.DockHost, ERD.Components.CodeEditor, ERD.Components.DebugConsole,
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
    FDiagnosticGeneration: Int64;
    FDiagnosticRequest: TERDDiagnosticRequest;
    FDocuments: TERDDocumentHost;
    FDebugModel: TERDDebugModel;
    FBreakpoints: TERDBreakpointPanel;
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
    FSavedDocuments: TDictionary<string,string>;
    FDefaultLayout, FSavedLayout: string;
    function GetEditor: TERDCodeEditor;
    property FEditor: TERDCodeEditor read GetEditor;
    procedure LayoutChanged(Sender: TObject);
    procedure Activated(Sender: TObject; const PanelID: string);
    procedure RibbonClick(Sender: TObject);
    procedure LoadDiagnostics;
    procedure CreateProject;
    procedure ProjectSearch(Sender: TObject);
    procedure ProjectOpen(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem);
    procedure ProjectRename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const NewName: string);
    procedure ProjectMove(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
    procedure EditorChanged(Sender: TObject; Document: TERDDocument);
    procedure ResolveDocument(Sender: TObject; const DocumentID: string);
    procedure BreakpointNavigate(Sender: TObject; const DocumentUri: string; Line: Integer);
    procedure DebugCommand(Sender: TObject; Command: TERDDebugCommand);
    procedure SaveDocument(Sender: TObject; const Request: TERDDocumentSaveRequest);
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
  FSavedDocuments := TDictionary<string,string>.Create;
  FDocuments := TERDDocumentHost.Create(Self);
  FDocuments.OpenCode('demo://main.ts','main.ts',MainSource);
  FDebugModel := TERDDebugModel.Create(Self); FDocuments.DebugModel := FDebugModel;
  FDebugModel.OnCommand := DebugCommand;
  FDocuments.OnResolveDocument := ResolveDocument;
  FDebugModel.AddBreakpoint('demo://main.ts',5); FDebugModel.AddBreakpoint('demo://board.ts',2);
  FBreakpoints := TERDBreakpointPanel.Create(Self); FBreakpoints.DebugModel := FDebugModel;
  FBreakpoints.OnNavigate := BreakpointNavigate;
  FDocuments.OnSave := SaveDocument; FDocuments.ConfirmClose := True;
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
  LoadDiagnostics; FDocuments.OnDocumentChanged := EditorChanged;
  FHost.LeftSize := 300;
  FHost.BeginUpdate;
  try
    FHost.AddPanel('editor','Documents',FDocuments,dzCenter).Closable := False;
    FHost.AddPanel('files','Project',FProjectHost,dzLeft);
    FHost.AddPanel('inspector','Inspector',FDetails,dzRight);
    FHost.AddPanel('locals','Locals',FValues,dzRight);
    FHost.AddPanel('console','Console',FConsole,dzBottom);
    FHost.AddPanel('diagnostics','Diagnostics (demo)',FDiagnosticHost,dzBottom);
    FHost.AddPanel('breakpoints','Breakpoints (demo)',FBreakpoints,dzBottom);
    FHost.ActivatePanel('diagnostics');
  finally FHost.EndUpdate; end;
  FDefaultLayout := FHost.SaveLayout; FHost.OnLayoutChanged := LayoutChanged; FHost.OnPanelActivated := Activated;
  LayoutChanged(Self); ThemeChanged;
end;
destructor TDockPreview.Destroy;
begin
  AttachRibbon(nil); if FDocuments <> nil then begin FDocuments.OnDocumentChanged := nil; FDocuments.OnSave := nil; FDocuments.OnClosing := nil; FDocuments.OnResolveDocument := nil; end;
  if FDebugModel <> nil then FDebugModel.OnCommand := nil;
  if FBreakpoints <> nil then FBreakpoints.OnNavigate := nil;
  if FFiles <> nil then begin FFiles.OnOpenItem := nil; FFiles.OnRenameItem := nil; FFiles.OnMoveItem := nil; end;
  if FProjectSearch <> nil then FProjectSearch.OnSearch := nil;
  if FDiagnostics <> nil then begin FDiagnostics.OnChange := nil; FDiagnostics.OnNavigate := nil; end;
  if FSearch <> nil then FSearch.OnSearch := nil; if FSeverity <> nil then FSeverity.OnChange := nil;
  if FHost <> nil then begin FHost.OnLayoutChanged := nil; FHost.OnPanelActivated := nil; end;
  // Release the host first, so it can restore borrowed controls while they exist.
  FreeAndNil(FHost); FreeAndNil(FSavedDocuments); inherited;
end;
procedure TDockPreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Theme := Theme;
  if FHost <> nil then begin FHost.Theme := Theme; FHost.ThemeChanged; end;
  if FDocuments <> nil then FDocuments.Theme := Theme;
  if FBreakpoints <> nil then FBreakpoints.Theme := Theme;
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
  Add(5,'board','src','board.ts','demo://board.ts',pikTypeScript,[]);
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
  if FSavedDocuments.ContainsKey(Item.Uri) then Source := FSavedDocuments[Item.Uri];
  FDocuments.OpenCode(Item.Uri,Item.Name,Source,pisReadOnly in Item.Status);
  FHost.ActivatePanel('editor'); if FEditor.CanFocus then FEditor.SetFocus;
  FStatus.Caption := 'DEMO DATA | opened '+Item.Name;
end;
procedure TDockPreview.ProjectRename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const NewName: string);
var I: Integer;
begin
  for I := 0 to High(FProjectItems) do if FProjectItems[I].ID = Item.ID then FProjectItems[I].Name := NewName;
  Inc(FProjectRevision); FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
  FDocuments.SetDocumentCaption(Item.Uri,NewName);
  FStatus.Caption := 'DEMO DATA | renamed '+Item.Name+' to '+NewName+' in memory';
end;
procedure TDockPreview.ProjectMove(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
var I: Integer;
begin
  for I := 0 to High(FProjectItems) do if FProjectItems[I].ID = Item.ID then FProjectItems[I].ParentID := ParentID;
  Inc(FProjectRevision); FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
  FStatus.Caption := 'DEMO DATA | moved '+Item.Name+' in memory';
end;
function TDockPreview.GetEditor: TERDCodeEditor;
begin
  Result := nil; if (FDocuments <> nil) and (FDocuments.ActiveDocument <> nil) then Result := FDocuments.ActiveDocument.Editor;
end;
procedure TDockPreview.ResolveDocument(Sender: TObject; const DocumentID: string);
var Item: TERDProjectItem;
begin
  for Item in FProjectItems do
    if (Item.Uri = DocumentID) and not (pisMissing in Item.Status) then
    begin ProjectOpen(Self,Default(TERDProjectContext),Item); Exit; end;
end;
procedure TDockPreview.BreakpointNavigate(Sender: TObject; const DocumentUri: string; Line: Integer);
begin
  FHost.ActivatePanel('editor');
  if FDocuments.NavigateToSource(DocumentUri,Line) then
    FStatus.Caption := 'DEMO DATA | breakpoint source: '+DocumentUri;
end;
procedure TDockPreview.DebugCommand(Sender: TObject; Command: TERDDebugCommand);
begin
  case Command of
    dcContinue: FDebugModel.SetExecution(dsRunning);
    dcStop: FDebugModel.SetExecution(dsDisconnected);
  else
    FStatus.Caption := 'DEMO DATA | no runtime attached; use Pause main / Pause board fixtures';
    Exit;
  end;
  FStatus.Caption := 'DEMO DATA | debug state changed; no runtime attached';
end;
procedure TDockPreview.SaveDocument(Sender: TObject; const Request: TERDDocumentSaveRequest);
begin
  // Acknowledge the in-memory fixture only; the demo deliberately performs no document I/O.
  FSavedDocuments.AddOrSetValue(Request.DocumentID,Request.Text);
  FDocuments.CompleteSave(Request);
  FStatus.Caption := 'DEMO DATA | saved in memory: '+Request.DocumentID;
end;
procedure TDockPreview.EditorChanged(Sender: TObject; Document: TERDDocument);
var I: Integer;
begin
  if FDiagnostics <> nil then FDiagnostics.MarkStale;
  for I := 0 to High(FProjectItems) do
    if (FProjectItems[I].Uri = Document.ID) and
      ((pisModified in FProjectItems[I].Status) <> Document.Modified) then
    begin
      if Document.Modified then Include(FProjectItems[I].Status,pisModified)
      else Exclude(FProjectItems[I].Status,pisModified);
      Inc(FProjectRevision); FFiles.SetItems('dock-demo',FProjectRevision,FProjectItems);
      Break;
    end;
end;
procedure TDockPreview.LoadDiagnostics;
var Values: TArray<TERDDiagnostic>;
  procedure Add(Index: Integer; Severity: TERDDiagnosticSeverity; const Code, MessageText, Uri: string; Line, Column: Integer);
  begin
    Values[Index].ID := 'sample-'+IntToStr(Index); Values[Index].Severity := Severity;
    Values[Index].Code := Code; Values[Index].MessageText := MessageText; Values[Index].Origin := 'Demo fixture';
    Values[Index].DocumentUri := Uri; Values[Index].Line := Line; Values[Index].Column := Column;
  end;
begin
  FDocuments.OpenCode('demo://main.ts','main.ts',MainSource);
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
begin
  FHost.ActivatePanel('editor');
  if not FDocuments.NavigateToSource(Diagnostic.DocumentUri,Diagnostic.Line,Diagnostic.Column) then Exit;
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
    20: if FDocuments.ActiveDocument <> nil then FDocuments.SaveDocument(FDocuments.ActiveDocument.ID);
    21: if FDocuments.ActiveDocument <> nil then FDocuments.CloseDocument(FDocuments.ActiveDocument.ID);
    22: begin FDebugModel.SetExecution(dsPaused,'demo://main.ts',5); FHost.ActivatePanel('editor'); FDocuments.RevealExecutionLocation; end;
    23: begin FDebugModel.SetExecution(dsPaused,'demo://board.ts',2); FHost.ActivatePanel('editor'); FDocuments.RevealExecutionLocation; end;
    24: FDebugModel.RequestCommand(dcContinue);
    25: FDebugModel.RequestCommand(dcStop);
    27: FDocuments.SaveAllDocuments;
    28: if FDocuments.ActiveDocument <> nil then FDocuments.CloseOtherDocuments(FDocuments.ActiveDocument.ID);
    29: if FDocuments.ActiveDocument <> nil then FDocuments.CloseDocumentsToRight(FDocuments.ActiveDocument.ID);
    30: FDocuments.CloseAllDocuments;
    26: begin FHost.ShowPanel('breakpoints'); FHost.ActivatePanel('breakpoints'); end;
  end;
end;
procedure TDockPreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..30] of string = ('Reset layout','Remember layout','Restore layout','Auto-hide inspector','Pin inspector','Float console','Show console','Dock console','Hide locals','Show locals','Save layout...','Open layout...','Load diagnostics','Loading','Unavailable','Clear diagnostics','Group by file','Next message','Previous message','Copy messages','Save document','Close document','Pause main','Pause board','Resume fixture','Stop fixture','Breakpoints',
    'Save all','Close others','Close to right','Close all');
Descriptions: array[0..30] of string = ('Restores the initial panel arrangement.',
    'Remembers the current panel arrangement for this demo session.',
    'Restores the remembered panel arrangement.',
    'Collapses the inspector into a side tab. Hover or click to reveal it.',
    'Keeps the inspector visible in the right dock group.',
    'Opens the console in a separate themed window.',
    'Shows the console in its previous dock or floating state.',
    'Returns the console to the bottom tab group.',
    'Hides the locals panel without destroying its content.',
    'Shows the locals panel again.',
    'Saves the panel arrangement to a layout file.',
    'Loads a saved panel arrangement.',
    'Loads synthetic errors, warnings, information and hints.',
    'Starts a new diagnostics request and marks previous results stale.',
    'Fails the pending request while retaining previous results.',
    'Clears the diagnostics view.',
    'Toggles file grouping.',
    'Navigates to the next visible source diagnostic.',
    'Navigates to the previous visible source diagnostic.',
    'Copies all matching diagnostic messages.',
    'Accepts this fixture document in memory; no disk I/O.',
    'Closes the current document; unsaved changes veto closing.',
    'Shows a synthetic paused location in main.ts.',
    'Opens board.ts if necessary and shows a synthetic pause.',
    'Clears execution markers and simulates running.',
    'Clears execution markers and simulates disconnect.',
    'Shows shared breakpoints; double-click to navigate to source.',
    'Requests saving all modified writable documents.',
    'Closes all documents except the active tab.',
    'Closes documents to the right of the active tab.',
    'Closes all documents; Cancel stops the remaining close requests.');
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
    if I = 20 then begin Group := FTab.Groups.Add; Group.Caption := 'Documents (demo)'; end;
    if I = 22 then begin Group := FTab.Groups.Add; Group.Caption := 'Debug (demo)'; end;
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
