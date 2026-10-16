unit ERD.Components.DebugModel;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Generics.Collections;
type
  TERDDebugState = (dsDisconnected, dsRunning, dsPaused);
  TERDBreakpointStatus = (bpsPending, bpsVerified, bpsRejected);
  TERDDebugCommand = (dcContinue, dcPause, dcStepOver, dcStepInto, dcStepOut, dcStop);
  TERDDebugCommandEvent = procedure(Sender: TObject; Command: TERDDebugCommand) of object;
  TERDDebugBreakpoint = record
    ID, Revision: Int64;
    DocumentUri, Condition, HitCondition, LogMessage, MessageText: string;
    Line, ResolvedLine: Integer;
    Enabled: Boolean;
    Status: TERDBreakpointStatus;
  end;
  /// Shared UI state. Transport, execution and persistence belong to the host.
  TERDDebugModel = class(TComponent)
  private
    FBreakpoints: TList<TERDDebugBreakpoint>;
    FListeners: TList<TNotifyEvent>;
    FDocumentTexts: TDictionary<string,string>;
    FNextID, FRevision, FSession: Int64;
    FState: TERDDebugState;
    FExecutionUri: string;
    FExecutionLine: Integer;
    FOnChange: TNotifyEvent;
    FOnCommand: TERDDebugCommandEvent;
    procedure Changed;
    function FindID(ID: Int64): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddListener(const Listener: TNotifyEvent);
    procedure RemoveListener(const Listener: TNotifyEvent);
    function Breakpoints: TArray<TERDDebugBreakpoint>;
    function BreakpointAt(const DocumentUri: string; Line: Integer; out Value: TERDDebugBreakpoint): Boolean;
    function DisplayBreakpointAt(const DocumentUri: string; Line: Integer; out Value: TERDDebugBreakpoint): Boolean;
    function AddBreakpoint(const DocumentUri: string; Line: Integer): Int64;
    procedure RemoveBreakpoint(ID: Int64);
    procedure ToggleBreakpoint(const DocumentUri: string; Line: Integer; ToggleEnabled: Boolean = False);
    procedure ConfigureBreakpoint(ID: Int64; Enabled: Boolean; const Condition, HitCondition, LogMessage: string);
    function PublishBreakpoint(Session, ID, Revision: Int64; Status: TERDBreakpointStatus;
      ResolvedLine: Integer; const MessageText: string): Boolean;
    procedure DocumentEdited(const DocumentUri, BeforeText, AfterText: string);
    procedure BeginSession;
    procedure SetExecution(State: TERDDebugState; const DocumentUri: string = ''; Line: Integer = 0);
    procedure RequestCommand(Command: TERDDebugCommand);
    property Session: Int64 read FSession;
    property State: TERDDebugState read FState;
    property ExecutionUri: string read FExecutionUri;
    property ExecutionLine: Integer read FExecutionLine;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCommand: TERDDebugCommandEvent read FOnCommand write FOnCommand;
  end;
implementation
constructor TERDDebugModel.Create(AOwner: TComponent);
begin
  inherited; FBreakpoints := TList<TERDDebugBreakpoint>.Create;
  FListeners := TList<TNotifyEvent>.Create; FDocumentTexts := TDictionary<string,string>.Create;
end;
destructor TERDDebugModel.Destroy;
begin FDocumentTexts.Free; FListeners.Free; FBreakpoints.Free; inherited; end;
procedure TERDDebugModel.AddListener(const Listener: TNotifyEvent);
var Existing: TNotifyEvent;
begin
  for Existing in FListeners do
    if (TMethod(Existing).Code = TMethod(Listener).Code) and (TMethod(Existing).Data = TMethod(Listener).Data) then Exit;
  FListeners.Add(Listener);
end;
procedure TERDDebugModel.RemoveListener(const Listener: TNotifyEvent);
var I: Integer;
begin
  for I := FListeners.Count-1 downto 0 do
    if (TMethod(FListeners[I]).Code = TMethod(Listener).Code) and
      (TMethod(FListeners[I]).Data = TMethod(Listener).Data) then FListeners.Delete(I);
end;
procedure TERDDebugModel.Changed;
var List: TArray<TNotifyEvent>; Listener, Current: TNotifyEvent; Present: Boolean;
begin
  List := FListeners.ToArray;
  for Listener in List do
  begin
    Present := False;
    for Current in FListeners do
      if (TMethod(Current).Code = TMethod(Listener).Code) and (TMethod(Current).Data = TMethod(Listener).Data) then
      begin Present := True; Break; end;
    if Present and Assigned(Listener) then Listener(Self);
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;
function TERDDebugModel.FindID(ID: Int64): Integer;
var I: Integer;
begin Result := -1; for I := 0 to FBreakpoints.Count-1 do if FBreakpoints[I].ID = ID then Exit(I); end;
function TERDDebugModel.Breakpoints: TArray<TERDDebugBreakpoint>;
begin Result := FBreakpoints.ToArray; end;
function TERDDebugModel.BreakpointAt(const DocumentUri: string; Line: Integer; out Value: TERDDebugBreakpoint): Boolean;
var Item: TERDDebugBreakpoint;
begin
  Value := Default(TERDDebugBreakpoint);
  for Item in FBreakpoints do if (Item.DocumentUri = DocumentUri) and (Item.Line = Line) then
  begin Value := Item; Exit(True); end;
  Result := False;
end;
function TERDDebugModel.DisplayBreakpointAt(const DocumentUri: string; Line: Integer; out Value: TERDDebugBreakpoint): Boolean;
var Item: TERDDebugBreakpoint; DisplayLine: Integer;
begin
  Value := Default(TERDDebugBreakpoint);
  for Item in FBreakpoints do
  begin
    DisplayLine := Item.Line; if (Item.Status = bpsVerified) and (Item.ResolvedLine > 0) then DisplayLine := Item.ResolvedLine;
    if (Item.DocumentUri = DocumentUri) and (DisplayLine = Line) then begin Value := Item; Exit(True); end;
  end;
  Result := False;
end;
function TERDDebugModel.AddBreakpoint(const DocumentUri: string; Line: Integer): Int64;
var Item: TERDDebugBreakpoint;
begin
  if (DocumentUri = '') or (Line < 1) then raise EArgumentException.Create('Breakpoints need a document URI and a one-based line');
  if BreakpointAt(DocumentUri,Line,Item) then Exit(Item.ID);
  Item := Default(TERDDebugBreakpoint); Inc(FNextID); Inc(FRevision);
  Item.ID := FNextID; Item.Revision := FRevision; Item.DocumentUri := DocumentUri;
  Item.Line := Line; Item.Enabled := True; FBreakpoints.Add(Item); Result := Item.ID; Changed;
end;
procedure TERDDebugModel.RemoveBreakpoint(ID: Int64);
var Index: Integer;
begin Index := FindID(ID); if Index < 0 then Exit; FBreakpoints.Delete(Index); Changed; end;
procedure TERDDebugModel.ToggleBreakpoint(const DocumentUri: string; Line: Integer; ToggleEnabled: Boolean);
var Item: TERDDebugBreakpoint;
begin
  if not DisplayBreakpointAt(DocumentUri,Line,Item) then AddBreakpoint(DocumentUri,Line)
  else if ToggleEnabled then ConfigureBreakpoint(Item.ID,not Item.Enabled,Item.Condition,Item.HitCondition,Item.LogMessage)
  else RemoveBreakpoint(Item.ID);
end;
procedure TERDDebugModel.ConfigureBreakpoint(ID: Int64; Enabled: Boolean; const Condition, HitCondition, LogMessage: string);
var Index: Integer; Item: TERDDebugBreakpoint;
begin
  Index := FindID(ID); if Index < 0 then raise EArgumentException.Create('Unknown breakpoint');
  Item := FBreakpoints[Index];
  if (Item.Enabled = Enabled) and (Item.Condition = Condition) and (Item.HitCondition = HitCondition) and (Item.LogMessage = LogMessage) then Exit;
  Inc(FRevision); Item.Revision := FRevision; Item.Enabled := Enabled; Item.Condition := Condition;
  Item.HitCondition := HitCondition; Item.LogMessage := LogMessage; Item.Status := bpsPending;
  Item.ResolvedLine := 0; Item.MessageText := ''; FBreakpoints[Index] := Item; Changed;
end;
function TERDDebugModel.PublishBreakpoint(Session, ID, Revision: Int64; Status: TERDBreakpointStatus;
  ResolvedLine: Integer; const MessageText: string): Boolean;
var Index: Integer; Item: TERDDebugBreakpoint;
begin
  Result := False; if (Session <> FSession) or (ResolvedLine < 0) then Exit;
  Index := FindID(ID); if Index < 0 then Exit; Item := FBreakpoints[Index];
  if Item.Revision <> Revision then Exit;
  Item.Status := Status; Item.ResolvedLine := ResolvedLine; Item.MessageText := MessageText;
  FBreakpoints[Index] := Item; Changed; Result := True;
end;
procedure TERDDebugModel.DocumentEdited(const DocumentUri, BeforeText, AfterText: string);
var Prefix, Suffix, I, J, FirstLine, OldLast, Delta, Position: Integer; Item: TERDDebugBreakpoint; Any: Boolean; CurrentText: string;
  function Lines(const Text: string; Finish: Integer): Integer;
  var K: Integer;
  begin Result := 0; for K := 1 to Finish do if Text[K] = #10 then Inc(Result); end;
begin
  if BeforeText = AfterText then Exit;
  // Mirrored editors may report the same document edit twice.
  if FDocumentTexts.TryGetValue(DocumentUri,CurrentText) and (CurrentText = AfterText) then Exit;
  FDocumentTexts.AddOrSetValue(DocumentUri,AfterText);
  Prefix := 0;
  while (Prefix < Length(BeforeText)) and (Prefix < Length(AfterText)) and (BeforeText[Prefix+1] = AfterText[Prefix+1]) do Inc(Prefix);
  Suffix := 0;
  while (Suffix < Length(BeforeText)-Prefix) and (Suffix < Length(AfterText)-Prefix) and
    (BeforeText[Length(BeforeText)-Suffix] = AfterText[Length(AfterText)-Suffix]) do Inc(Suffix);
  FirstLine := Lines(BeforeText,Prefix)+1; OldLast := Lines(BeforeText,Length(BeforeText)-Suffix)+1;
  Delta := Lines(AfterText,Length(AfterText))-Lines(BeforeText,Length(BeforeText)); Any := False;
  for I := 0 to FBreakpoints.Count-1 do
  begin
    Item := FBreakpoints[I]; if Item.DocumentUri <> DocumentUri then Continue;
    Position := Item.Line;
    if (Length(BeforeText)-Prefix-Suffix = 0) and ((Prefix = 0) or (BeforeText[Prefix] = #10)) and (Position >= FirstLine) then Inc(Position,Delta)
    else if Position > OldLast then Inc(Position,Delta)
    else if Position >= FirstLine then Position := FirstLine;
    Item.Line := Position; Item.Status := bpsPending; Item.ResolvedLine := 0; Item.MessageText := '';
    Inc(FRevision); Item.Revision := FRevision; FBreakpoints[I] := Item; Any := True;
  end;
  // Joining lines may merge anchors; retain the oldest breakpoint identity.
  for I := FBreakpoints.Count-1 downto 0 do for J := 0 to I-1 do
    if (FBreakpoints[I].DocumentUri = DocumentUri) and (FBreakpoints[J].DocumentUri = DocumentUri) and
      (FBreakpoints[I].Line = FBreakpoints[J].Line) then begin FBreakpoints.Delete(I); Break; end;
  if (FExecutionUri = DocumentUri) and (FExecutionLine <> 0) then begin FExecutionUri := ''; FExecutionLine := 0; Any := True; end;
  if Any then Changed;
end;
procedure TERDDebugModel.BeginSession;
var I: Integer; Item: TERDDebugBreakpoint;
begin
  Inc(FSession); FState := dsDisconnected; FExecutionUri := ''; FExecutionLine := 0;
  for I := 0 to FBreakpoints.Count-1 do
  begin Item := FBreakpoints[I]; Item.Status := bpsPending; Item.ResolvedLine := 0; Item.MessageText := ''; FBreakpoints[I] := Item; end;
  Changed;
end;
procedure TERDDebugModel.SetExecution(State: TERDDebugState; const DocumentUri: string; Line: Integer);
begin
  if (State = dsPaused) and ((DocumentUri = '') or (Line < 1)) then raise EArgumentException.Create('Paused execution needs a source location');
  FState := State; FExecutionUri := ''; FExecutionLine := 0;
  if State = dsPaused then begin FExecutionUri := DocumentUri; FExecutionLine := Line; end;
  Changed;
end;
procedure TERDDebugModel.RequestCommand(Command: TERDDebugCommand);
begin if Assigned(FOnCommand) then FOnCommand(Self,Command); end;
end.
