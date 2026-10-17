unit ERD.Components.DocumentHost;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils, System.Types,
  System.Generics.Collections, Vcl.Controls, ERD.Components.DebugModel,
  ERD.Components.Panel, ERD.Components.Tabs, ERD.Components.CodeEditor;

type
  TERDDocumentCloseAction = (dcaCancel, dcaSave, dcaDiscard);
  TERDDocumentHost = class;
  TERDDocumentSaveRequest = record
    DocumentID: string;
    Instance, Revision: Int64;
    Text: UnicodeString;
  end;
  TERDDocument = class
  private
    FID, FCaption: string;
    FInstance, FRevision: Int64;
    FControl: TWinControl;
    FEditor: TERDCodeEditor;
    FTab: TERDTabItem;
    FReadOnly, FModified, FOwned, FPendingSave, FCloseAfterSave: Boolean;
    FSavedText: UnicodeString;
    FSave: TERDDocumentSaveRequest;
    FParent: TWinControl;
    FBounds: TRect;
    FAlign: TAlign;
    FAnchors: TAnchors;
    FVisible: Boolean;
    FTabOrder: Integer;
  public
    property ID: string read FID;
    property Caption: string read FCaption;
    property Instance: Int64 read FInstance;
    property Revision: Int64 read FRevision;
    property Control: TWinControl read FControl;
    property Editor: TERDCodeEditor read FEditor;
    property Modified: Boolean read FModified;
    property ReadOnly: Boolean read FReadOnly;
  end;
  TERDDocumentResolveEvent = procedure(Sender: TObject; const DocumentID: string) of object;
  TERDDocumentEvent = procedure(Sender: TObject; Document: TERDDocument) of object;
  TERDDocumentCloseEvent = procedure(Sender: TObject; Document: TERDDocument;
    var AllowClose: Boolean) of object;
  TERDDocumentSaveEvent = procedure(Sender: TObject;
    const Request: TERDDocumentSaveRequest) of object;

  /// <summary>Document tabs with persistent editor views and host-controlled saves.</summary>
  TERDDocumentHost = class(TERDPanel)
  private
    FTabs: TERDTabs;
    FBody: TERDPanel;
    FDocuments: TObjectList<TERDDocument>;
    FCloseQueue: TList<Int64>;
    FNextInstance, FWaitingSave: Int64;
    FProcessingClose, FConfirmClose: Boolean;
    FReleasing, FUpdating, FResolving: Boolean;
    FDebugModel: TERDDebugModel;
    FOnResolveDocument: TERDDocumentResolveEvent;
    FOnDocumentChanged, FOnActiveDocumentChanged: TERDDocumentEvent;
    FOnClosing: TERDDocumentCloseEvent;
    FOnSave: TERDDocumentSaveEvent;
    function DocumentByInstance(Serial: Int64): TERDDocument;
    function QueueDocuments(const KeepID: string; RightOnly: Boolean): Integer;
    procedure ScheduleClose;
    procedure SetDebugModel(Value: TERDDebugModel);
    function GetCount: Integer;
    function GetDocument(Index: Integer): TERDDocument;
    function GetActive: TERDDocument;
    function AddPage(const ID, Caption: string; Control: TWinControl;
      Owned: Boolean): TERDDocument;
    procedure EditorChanged(Sender: TObject);
    procedure TabChanged(Sender: TObject);
    procedure TabClose(Sender: TObject; Index: Integer; var CanClose: Boolean);
    procedure ReleaseDocument(Document: TERDDocument);
    procedure ProcessClose(var Message: TMessage); message WM_APP + $4D3;
  protected
    function ConfirmDocumentClose(Document: TERDDocument): TERDDocumentCloseAction; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindDocument(const ID: string): TERDDocument;
    function OpenCode(const ID, Caption: string; const Text: UnicodeString;
      ReadOnly: Boolean = False): TERDDocument;
    function AddDocument(const ID, Caption: string; Control: TWinControl): TERDDocument;
    function ActivateDocument(const ID: string): Boolean;
    function NavigateToSource(const ID: string; Line: Integer; Column: Integer = 1;
      FocusEditor: Boolean = True): Boolean;
    function RevealExecutionLocation(FocusEditor: Boolean = True): Boolean;
    function CloseDocument(const ID: string): Boolean;
    function CloseAllDocuments: Integer;
    function CloseOtherDocuments(const ID: string): Integer;
    function CloseDocumentsToRight(const ID: string): Integer;
    function SaveAllDocuments: Integer;
    function SaveDocument(const ID: string): Boolean;
    function CompleteSave(const Request: TERDDocumentSaveRequest): Boolean;
    function FailSave(const Request: TERDDocumentSaveRequest): Boolean;
    function SetDocumentReadOnly(const ID: string; Value: Boolean): Boolean;
    function SetDocumentModified(const ID: string; Value: Boolean): Boolean;
    function SetDocumentCaption(const ID, Value: string): Boolean;
    procedure ThemeChanged; override;
    property DocumentCount: Integer read GetCount;
    property Documents[Index: Integer]: TERDDocument read GetDocument;
    property ActiveDocument: TERDDocument read GetActive;
    property TabStrip: TERDTabs read FTabs;
  published
    property DebugModel: TERDDebugModel read FDebugModel write SetDebugModel;
    property OnResolveDocument: TERDDocumentResolveEvent read FOnResolveDocument write FOnResolveDocument;
    property ConfirmClose: Boolean read FConfirmClose write FConfirmClose default False;
    property Density;
    property ParentDensity;
    property OnDocumentChanged: TERDDocumentEvent read FOnDocumentChanged write FOnDocumentChanged;
    property OnActiveDocumentChanged: TERDDocumentEvent read FOnActiveDocumentChanged write FOnActiveDocumentChanged;
    property OnClosing: TERDDocumentCloseEvent read FOnClosing write FOnClosing;
    property OnSave: TERDDocumentSaveEvent read FOnSave write FOnSave;
  end;

implementation

uses System.UITypes, ERD.Components.TaskDialog;

constructor TERDDocumentHost.Create(AOwner: TComponent);
begin
  inherited;
  BorderWidth := 0; Width := 640; Height := 400;
  FDocuments := TObjectList<TERDDocument>.Create(True);
  FCloseQueue := TList<Int64>.Create;
  FBody := TERDPanel.Create(Self); FBody.Parent := Self;
  FBody.BorderWidth := 0; FBody.Align := alClient;
  FTabs := TERDTabs.Create(Self); FTabs.Parent := Self; FTabs.Align := alTop;
  FTabs.TabStyle := tsDocument; FTabs.AllowReorder := True;
  FTabs.OnChange := TabChanged; FTabs.OnClose := TabClose;
end;

destructor TERDDocumentHost.Destroy;
begin
  FReleasing := True; SetDebugModel(nil);
  FTabs.OnChange := nil; FTabs.OnClose := nil;
  while FDocuments.Count > 0 do ReleaseDocument(FDocuments.Last);
  FreeAndNil(FDocuments); FreeAndNil(FCloseQueue);
  inherited;
end;

procedure TERDDocumentHost.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  if Root <> Self then inherited;
end;

function TERDDocumentHost.GetCount: Integer;
begin Result := FDocuments.Count; end;
function TERDDocumentHost.GetDocument(Index: Integer): TERDDocument;
begin Result := FDocuments[Index]; end;
function TERDDocumentHost.GetActive: TERDDocument;
var D: TERDDocument;
begin
  Result := nil;
  if (FTabs.TabIndex < 0) or (FTabs.TabIndex >= FTabs.Tabs.Count) then Exit;
  for D in FDocuments do if D.FTab = FTabs.Tabs[FTabs.TabIndex] then Exit(D);
end;
function TERDDocumentHost.FindDocument(const ID: string): TERDDocument;
var D: TERDDocument;
begin
  Result := nil;
  for D in FDocuments do if D.ID = ID then Exit(D);
end;

function TERDDocumentHost.AddPage(const ID, Caption: string;
  Control: TWinControl; Owned: Boolean): TERDDocument;
var D: TERDDocument; Ancestor: TWinControl;
begin
  if Trim(ID) = '' then raise EArgumentException.Create('Document ID is required');
  if FindDocument(ID) <> nil then raise EArgumentException.Create('Document is already open');
  if Control = nil then raise EArgumentNilException.Create('Control');
  if (Control = FBody) or (Control = FTabs) then
    raise EArgumentException.Create('Cannot host internal controls');
  Ancestor := Self;
  while Ancestor <> nil do
  begin
    if Ancestor = Control then raise EArgumentException.Create('Document cannot contain its host');
    Ancestor := Ancestor.Parent;
  end;
  for D in FDocuments do if D.Control = Control then
    raise EArgumentException.Create('Control is already hosted');
  Result := TERDDocument.Create;
  Result.FID := ID; Result.FCaption := Caption;
  Inc(FNextInstance); Result.FInstance := FNextInstance;
  Result.FControl := Control; Result.FOwned := Owned;
  Result.FParent := Control.Parent; Result.FBounds := Control.BoundsRect;
  Result.FAlign := Control.Align; Result.FAnchors := Control.Anchors;
  Result.FVisible := Control.Visible; Result.FTabOrder := Control.TabOrder;
  Control.FreeNotification(Self);
  if Result.FParent <> nil then Result.FParent.FreeNotification(Self);
  FDocuments.Add(Result);
  Control.Visible := False; Control.Parent := FBody; Control.Align := alClient;
  FUpdating := True;
  try
  Result.FTab := FTabs.Tabs.Add; Result.FTab.Caption := Caption;
  Result.FTab.Hint := Caption+'|'+ID; Result.FTab.Page := Control;
  finally FUpdating := False; end;
end;

function TERDDocumentHost.OpenCode(const ID, Caption: string;
  const Text: UnicodeString; ReadOnly: Boolean): TERDDocument;
var Editor: TERDCodeEditor;
begin
  Result := FindDocument(ID);
  if Result <> nil then
  begin
    if Result.Editor = nil then raise EArgumentException.Create('Document is not a code document');
    ActivateDocument(ID); Exit;
  end;
  Editor := TERDCodeEditor.Create(Self);
  try
    Editor.DocumentUri := ID; Editor.Text := Text; Editor.ReadOnly := ReadOnly;
    Result := AddPage(ID,Caption,Editor,True);
  except Editor.Free; raise; end;
  Editor.DebugModel := FDebugModel;
  Result.FEditor := Editor; Result.FReadOnly := ReadOnly; Result.FSavedText := Editor.Text;
  Editor.OnChange := EditorChanged; SetDocumentReadOnly(ID,ReadOnly);
  ThemeChanged;
  FUpdating := True;
  try ActivateDocument(ID); finally FUpdating := False; end;
  TabChanged(Self);
end;
function TERDDocumentHost.AddDocument(const ID, Caption: string; Control: TWinControl): TERDDocument;
begin
  Result := AddPage(ID,Caption,Control,False);
  FUpdating := True;
  try ActivateDocument(ID); finally FUpdating := False; end;
  TabChanged(Self);
end;
function TERDDocumentHost.ActivateDocument(const ID: string): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(ID); Result := D <> nil; if not Result then Exit;
  FTabs.TabIndex := D.FTab.Index;
end;
procedure TERDDocumentHost.SetDebugModel(Value: TERDDebugModel);
var D: TERDDocument;
begin
  if FDebugModel = Value then Exit;
  if FDebugModel <> nil then FDebugModel.RemoveFreeNotification(Self);
  FDebugModel := Value;
  if Value <> nil then Value.FreeNotification(Self);
  if FDocuments <> nil then
    for D in FDocuments do if D.Editor <> nil then D.Editor.DebugModel := Value;
end;
function TERDDocumentHost.NavigateToSource(const ID: string; Line, Column: Integer;
  FocusEditor: Boolean): Boolean;
var D: TERDDocument; Source: UnicodeString; Position, Offset: Integer;
begin
  Result := False;
  if (Trim(ID) = '') or (Line < 1) or (Column < 1) or FReleasing then Exit;
  D := FindDocument(ID);
  if (D = nil) and Assigned(FOnResolveDocument) and not FResolving then
  begin
    FResolving := True;
    try FOnResolveDocument(Self,ID); finally FResolving := False; end;
    D := FindDocument(ID);
  end;
  if (D = nil) or (D.Editor = nil) then Exit;
  if not D.Editor.RevealDebugLine(Line) then Exit;
  Source := D.Editor.Text; Position := D.Editor.SelStart; Offset := 0;
  while (Offset < Column-1) and (Position+Offset < Length(Source)) do
  begin
    if CharInSet(Source[Position+Offset+1],[#10,#13]) then Break;
    Inc(Offset);
  end;
  D.Editor.SelStart := Position+Offset;
  ActivateDocument(ID);
  // An active-document callback may remove content. Resolve the page again before focusing.
  D := FindDocument(ID);
  Result := (D <> nil) and (D.Editor <> nil);
  if Result and FocusEditor and D.Editor.CanFocus then D.Editor.SetFocus;
end;
function TERDDocumentHost.RevealExecutionLocation(FocusEditor: Boolean): Boolean;
var ID: string; Line: Integer;
begin
  Result := (FDebugModel <> nil) and (FDebugModel.State = dsPaused);
  if not Result then Exit;
  ID := FDebugModel.ExecutionUri; Line := FDebugModel.ExecutionLine;
  Result := NavigateToSource(ID,Line,1,FocusEditor);
end;
procedure TERDDocumentHost.TabChanged(Sender: TObject);
begin
  if not FReleasing and not FUpdating and Assigned(FOnActiveDocumentChanged) then
    FOnActiveDocumentChanged(Self,ActiveDocument);
end;
procedure TERDDocumentHost.EditorChanged(Sender: TObject);
var D: TERDDocument;
begin
  for D in FDocuments do if D.Editor = Sender then
  begin
    Inc(D.FRevision); D.FModified := D.Editor.Text <> D.FSavedText;
    D.FTab.Modified := D.Modified;
    if Assigned(FOnDocumentChanged) then FOnDocumentChanged(Self,D);
    Exit;
  end;
end;
function TERDDocumentHost.SetDocumentReadOnly(const ID: string; Value: Boolean): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(ID); Result := D <> nil; if not Result then Exit;
  D.FReadOnly := Value;
  if D.Editor <> nil then D.Editor.ReadOnly := Value;
  if Value then D.FTab.Badge := 'RO' else D.FTab.Badge := '';
end;
function TERDDocumentHost.SetDocumentModified(const ID: string; Value: Boolean): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(ID); Result := (D <> nil) and (D.Editor = nil);
  if not Result then Exit;
  Inc(D.FRevision); D.FModified := Value; D.FTab.Modified := Value;
  if Assigned(FOnDocumentChanged) then FOnDocumentChanged(Self,D);
end;
function TERDDocumentHost.SetDocumentCaption(const ID, Value: string): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(ID); Result := D <> nil; if not Result then Exit;
  D.FCaption := Value; D.FTab.Caption := Value; D.FTab.Hint := Value+'|'+ID;
end;
function TERDDocumentHost.DocumentByInstance(Serial: Int64): TERDDocument;
var D: TERDDocument;
begin
  Result := nil;
  for D in FDocuments do if D.Instance = Serial then Exit(D);
end;
procedure TERDDocumentHost.ScheduleClose;
begin
  if not FReleasing and not FProcessingClose and (FWaitingSave = 0) and
    (FCloseQueue.Count > 0) then PostMessage(Handle,WM_APP+$4D3,0,0);
end;
function TERDDocumentHost.QueueDocuments(const KeepID: string; RightOnly: Boolean): Integer;
var I, First: Integer; D, Keep: TERDDocument;
begin
  Result := 0; First := 0; Keep := nil;
  if KeepID <> '' then
  begin
    Keep := FindDocument(KeepID); if Keep = nil then Exit;
    if RightOnly then First := Keep.FTab.Index+1;
  end;
  for I := First to FTabs.Tabs.Count-1 do
    for D in FDocuments do
      if (D.FTab = FTabs.Tabs[I]) and (D <> Keep) then
      begin if CloseDocument(D.ID) then Inc(Result); Break; end;
end;
function TERDDocumentHost.CloseAllDocuments: Integer;
begin Result := QueueDocuments('',False); end;
function TERDDocumentHost.CloseOtherDocuments(const ID: string): Integer;
begin
  Result := 0; if Trim(ID) <> '' then Result := QueueDocuments(ID,False);
end;
function TERDDocumentHost.CloseDocumentsToRight(const ID: string): Integer;
begin
  Result := 0; if Trim(ID) <> '' then Result := QueueDocuments(ID,True);
end;
function TERDDocumentHost.SaveAllDocuments: Integer;
var Serials: TArray<Int64>; I: Integer; D: TERDDocument;
begin
  Result := 0; SetLength(Serials,FDocuments.Count);
  for I := 0 to FDocuments.Count-1 do Serials[I] := FDocuments[I].Instance;
  for I := 0 to High(Serials) do
  begin
    D := DocumentByInstance(Serials[I]);
    if (D <> nil) and D.Modified and not D.ReadOnly then
      if SaveDocument(D.ID) then Inc(Result);
  end;
end;
function TERDDocumentHost.ConfirmDocumentClose(Document: TERDDocument): TERDDocumentCloseAction;
var Dialog: TERDTaskDialog;
begin
  Result := dcaCancel; Dialog := TERDTaskDialog.Create(nil);
  try
    Dialog.Theme := EffectiveTheme; Dialog.Density := Density; Dialog.Title := 'Unsaved changes';
    Dialog.Caption := 'Save changes to '+Document.Caption+'?';
    Dialog.Text := 'Your changes will be lost if you close this document without saving.';
    Dialog.MainIcon := tdiWarning; Dialog.CommonButtons := [otcbCancel];
    Dialog.DefaultButton := otcbCancel;
    if not Document.ReadOnly and Assigned(FOnSave) then
      with Dialog.Buttons.Add do begin Caption := 'Save'; ModalResult := mrYes; Kind := otbkPrimary; end;
    with Dialog.Buttons.Add do begin Caption := 'Don''t save'; ModalResult := mrNo; end;
    if not Dialog.Execute then Exit;
    case Dialog.ModalResult of mrYes: Result := dcaSave; mrNo: Result := dcaDiscard; end;
  finally Dialog.Free; end;
end;
function TERDDocumentHost.SaveDocument(const ID: string): Boolean;
var D: TERDDocument; Request: TERDDocumentSaveRequest;
begin
  D := FindDocument(ID);
  Result := (D <> nil) and not D.ReadOnly and Assigned(FOnSave);
  if not Result then Exit;
  Request.DocumentID := D.ID; Request.Instance := D.Instance; Request.Revision := D.Revision;
  if D.Editor <> nil then Request.Text := D.Editor.Text else Request.Text := '';
  D.FSave := Request; D.FPendingSave := True;
  try FOnSave(Self,Request);
  except FailSave(Request); raise; end;
end;
function TERDDocumentHost.CompleteSave(const Request: TERDDocumentSaveRequest): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(Request.DocumentID);
  Result := (D <> nil) and D.FPendingSave and (D.Instance = Request.Instance) and
    (D.FSave.Revision = Request.Revision) and (D.FSave.Text = Request.Text);
  if not Result then Exit;
  D.FPendingSave := False;
  if D.Editor <> nil then
  begin D.FSavedText := Request.Text; D.FModified := D.Editor.Text <> D.FSavedText; end
  else D.FModified := D.Revision <> Request.Revision;
  D.FTab.Modified := D.Modified;
  if D.FCloseAfterSave then
  begin
    D.FCloseAfterSave := False; FWaitingSave := 0;
    if not D.Modified then
    begin if not FCloseQueue.Contains(D.Instance) then FCloseQueue.Insert(0,D.Instance); end
    else FCloseQueue.Clear;
    ScheduleClose;
  end;
  if Assigned(FOnDocumentChanged) then FOnDocumentChanged(Self,D);
end;
function TERDDocumentHost.FailSave(const Request: TERDDocumentSaveRequest): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(Request.DocumentID);
  Result := (D <> nil) and D.FPendingSave and (D.Instance = Request.Instance) and
    (D.FSave.Revision = Request.Revision) and (D.FSave.Text = Request.Text);
  if not Result then Exit;
  D.FPendingSave := False;
  if D.FCloseAfterSave then
  begin D.FCloseAfterSave := False; FWaitingSave := 0; FCloseQueue.Clear; end;
end;
procedure TERDDocumentHost.TabClose(Sender: TObject; Index: Integer; var CanClose: Boolean);
var D: TERDDocument;
begin
  CanClose := False;
  for D in FDocuments do if D.FTab.Index = Index then begin CloseDocument(D.ID); Exit; end;
end;
function TERDDocumentHost.CloseDocument(const ID: string): Boolean;
var D: TERDDocument;
begin
  D := FindDocument(ID); Result := D <> nil; if not Result then Exit;
  if FCloseQueue.Contains(D.Instance) then Exit;
  FCloseQueue.Add(D.Instance);
  ScheduleClose;
end;
procedure TERDDocumentHost.ProcessClose(var Message: TMessage);
var Serial, Revision: Int64; D: TERDDocument; AllowClose: Boolean; Action: TERDDocumentCloseAction;
begin
  if FReleasing or FProcessingClose or (FWaitingSave <> 0) or (FCloseQueue.Count = 0) then Exit;
  FProcessingClose := True;
  try
    try
    Serial := FCloseQueue[0]; FCloseQueue.Delete(0); D := DocumentByInstance(Serial);
    if D = nil then Exit;
    Revision := D.Revision; AllowClose := not D.Modified; Action := dcaCancel;
    if Assigned(FOnClosing) then FOnClosing(Self,D,AllowClose)
    else if D.Modified and FConfirmClose then Action := ConfirmDocumentClose(D);
    D := DocumentByInstance(Serial); if D = nil then Exit;
    if (D.Revision <> Revision) and D.Modified then
    begin FCloseQueue.Clear; Exit; end;
    if Action = dcaDiscard then AllowClose := True;
    if Action = dcaSave then
    begin
      if SaveDocument(D.ID) then
      begin
        D := DocumentByInstance(Serial); if D = nil then Exit;
        AllowClose := not D.Modified;
        if not AllowClose and D.FPendingSave then
        begin D.FCloseAfterSave := True; FWaitingSave := Serial; end;
      end;
    end;
    if AllowClose then ReleaseDocument(D)
    else if FWaitingSave = 0 then FCloseQueue.Clear;
    except FCloseQueue.Clear; raise; end;
  finally FProcessingClose := False; ScheduleClose; end;
end;
procedure TERDDocumentHost.ReleaseDocument(Document: TERDDocument);
var Control: TWinControl;
begin
  if FWaitingSave = Document.Instance then FWaitingSave := 0;
  Control := Document.Control;
  if Document.Editor <> nil then Document.Editor.OnChange := nil;
  FUpdating := True;
  try
  Document.FTab.Page := nil; FreeAndNil(Document.FTab);
  if Control <> nil then
  begin
    Control.RemoveFreeNotification(Self);
    if Document.FOwned then Control.Free
    else
    begin
      Control.Visible := False; Control.Parent := Document.FParent;
      Control.Align := alNone; Control.Anchors := Document.FAnchors;
      Control.BoundsRect := Document.FBounds; Control.Align := Document.FAlign;
      if Control.Parent <> nil then Control.TabOrder := Document.FTabOrder;
      Control.Visible := Document.FVisible;
    end;
  end;
  FDocuments.Remove(Document);
  finally FUpdating := False; end;
  TabChanged(Self); ScheduleClose;
end;
procedure TERDDocumentHost.Notification(AComponent: TComponent; Operation: TOperation);
var I: Integer; D: TERDDocument;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDebugModel) then FDebugModel := nil;
  if (Operation <> opRemove) or (FDocuments = nil) then Exit;
  for I := FDocuments.Count-1 downto 0 do
  begin
    D := FDocuments[I];
    if D.FParent = AComponent then D.FParent := nil;
    if D.Control = AComponent then
    begin D.FControl := nil; D.FEditor := nil; ReleaseDocument(D); end;
  end;
end;
procedure TERDDocumentHost.ThemeChanged;
begin
  inherited;
  if FTabs <> nil then FTabs.Theme := Theme;
  if FBody <> nil then FBody.Theme := Theme;
end;

end.
