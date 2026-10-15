unit ERD.Components.LSP;
// Persistent stdio language-service adapter. Process I/O stays off the VCL thread.
interface
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Types, System.Math, System.JSON,
  System.SyncObjs, System.Generics.Collections, ERD.Components.LanguageServices,
  ERD.Components.CodeEditor;
type
  TERDStdioLanguageService = class;
  TERDLSPDiagnosticsEvent = procedure(Sender: TObject; const DocumentUri, SourceText: UnicodeString;
    const Diagnostics: TArray<TERDCodeDiagnostic>) of object;
  IERDLSPDelivery = interface
    ['{A39C5376-8397-4A29-B979-7A5B1A2037B9}']
    procedure Deliver(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses;
      const Reply, Diagnostics, ErrorText: UnicodeString);
    procedure Detach;
  end;
  TERDLSPJob = record
    Request: TERDLanguageRequest;
    SyncOnly: Boolean;
    Responses: IERDLanguageResponses;
  end;
  TERDLSPWorker = class(TThread)
  private
    FExecutable: string;
    FDelivery: IERDLSPDelivery;
    FLock: TCriticalSection;
    FWake: TEvent;
    FJobs: TList<TERDLSPJob>;
    FActiveID: Int64;
    FCancelled: Boolean;
    FProcess: TProcessInformation;
    FInput, FOutput, FErrors: THandle;
    FBytes: TBytes;
    FErrorText, FDocumentUri, FDocumentText: UnicodeString;
    FWireVersion, FRpcID: Int64;
    procedure ResetProcess;
    procedure StartProcess;
    procedure WriteMessage(Value: TJSONObject);
    function ReadMessage: UnicodeString;
    function ReadReply(ID: Int64; out Diagnostics: UnicodeString): UnicodeString;
    procedure RunJob(const Job: TERDLSPJob);
    procedure QueueDelivery(const Job: TERDLSPJob; const Reply, Diagnostics, ErrorText: UnicodeString);
    procedure DrainErrors;
  protected
    procedure Execute; override;
  public
    constructor Create(const Executable: string; const Delivery: IERDLSPDelivery);
    destructor Destroy; override;
    procedure Enqueue(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses);
    procedure SyncDocument(const Uri, Text: UnicodeString; Version: Int64);
    procedure Cancel(ID: Int64);
    procedure Stop;
  end;
  TERDStdioLanguageService = class(TInterfacedObject, IERDCodeLanguageService, IERDCodeDocumentService)
  private
    FWorker: TERDLSPWorker;
    FDelivery: IERDLSPDelivery;
    FLastError: UnicodeString;
    FOnError: TNotifyEvent;
    FOnDiagnostics: TERDLSPDiagnosticsEvent;
    procedure Deliver(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses;
      const Reply, Diagnostics, ErrorText: UnicodeString);
  public
    constructor Create(const CompilerExecutable: string);
    destructor Destroy; override;
    procedure Request(const Snapshot: TERDLanguageRequest; const Responses: IERDLanguageResponses);
    procedure DocumentChanged(const Uri, Text: UnicodeString; Version: Int64);
    procedure Cancel(RequestID: Int64);
    property LastError: UnicodeString read FLastError;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnDiagnostics: TERDLSPDiagnosticsEvent read FOnDiagnostics write FOnDiagnostics;
  end;
implementation
type
  TERDLSPDelivery = class(TInterfacedObject, IERDLSPDelivery)
  private
    FOwner: TERDStdioLanguageService;
  public
    constructor Create(Owner: TERDStdioLanguageService);
    procedure Deliver(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses;
      const Reply, Diagnostics, ErrorText: UnicodeString);
    procedure Detach;
  end;
function JsonText(Obj: TJSONValue; const Name: string; const Fallback: string = ''): string;
var Value: TJSONValue;
begin
  Result := Fallback; if not (Obj is TJSONObject) then Exit;
  Value := TJSONObject(Obj).GetValue(Name); if Value is TJSONString then Result := Value.Value;
end;
function JsonInt(Obj: TJSONValue; const Name: string; Fallback: Int64 = 0): Int64;
var Value: TJSONValue;
begin
  Result := Fallback; if not (Obj is TJSONObject) then Exit;
  Value := TJSONObject(Obj).GetValue(Name);
  if (Value is TJSONNumber) and not TryStrToInt64(Value.Value, Result) then Result := Fallback;
end;
function JsonValue(Obj: TJSONValue; const Name: string): TJSONValue;
begin
  Result := nil; if Obj is TJSONObject then Result := TJSONObject(Obj).GetValue(Name);
end;
function DocumentUri(const Request: TERDLanguageRequest): string;
begin Result := Request.DocumentUri; if Result = '' then Result := 'file:///erd-editor.ts'; end;
function PositionJSON(const Text: UnicodeString; Position: Integer): TJSONObject;
var I, Line, Start: Integer;
begin
  Position := EnsureRange(Position, 0, Length(Text)); Line := 0; Start := 0;
  for I := 1 to Position do if Text[I] = #10 then begin Inc(Line); Start := I; end;
  Result := TJSONObject.Create;
  Result.AddPair('line', TJSONNumber.Create(Line)); Result.AddPair('character', TJSONNumber.Create(Position - Start));
end;
function PositionOffset(const Text: UnicodeString; Value: TJSONValue): Integer;
var Line, Character, I, Start, Finish: Integer;
begin
  Line := JsonInt(Value, 'line', -1); Character := JsonInt(Value, 'character', -1);
  if (Line < 0) or (Character < 0) then raise Exception.Create('Invalid LSP position');
  Start := 0; I := 0;
  while (I < Length(Text)) and (Line > 0) do
  begin Inc(I); if Text[I] = #10 then begin Dec(Line); Start := I; end; end;
  if Line <> 0 then raise Exception.Create('LSP line outside document');
  Finish := Start;
  while (Finish < Length(Text)) and not CharInSet(Text[Finish + 1], [#13, #10]) do Inc(Finish);
  if Character > Finish - Start then raise Exception.Create('LSP character outside line');
  Result := Start + Character;
end;
function MessageObject(const Method: string; Params: TJSONValue; ID: Int64 = -1): TJSONObject;
begin
  Result := TJSONObject.Create; Result.AddPair('jsonrpc', '2.0'); Result.AddPair('method', Method);
  if ID >= 0 then Result.AddPair('id', TJSONNumber.Create(ID));
  if Params <> nil then Result.AddPair('params', Params);
end;
constructor TERDLSPDelivery.Create(Owner: TERDStdioLanguageService);
begin inherited Create; FOwner := Owner; end;
procedure TERDLSPDelivery.Detach;
begin FOwner := nil; end;
procedure TERDLSPDelivery.Deliver(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses;
  const Reply, Diagnostics, ErrorText: UnicodeString);
begin
  if FOwner = nil then Exit;
  try FOwner.Deliver(Request, Responses, Reply, Diagnostics, ErrorText);
  except
    on E: Exception do
      if FOwner <> nil then
      begin
        FOwner.FLastError := E.Message;
        if Assigned(FOwner.FOnError) then FOwner.FOnError(FOwner);
      end;
  end;
end;
constructor TERDStdioLanguageService.Create(const CompilerExecutable: string);
begin
  inherited Create;
  if not FileExists(CompilerExecutable) then raise Exception.Create('Language compiler executable not found: ' + CompilerExecutable);
  FDelivery := TERDLSPDelivery.Create(Self);
  FWorker := TERDLSPWorker.Create(ExpandFileName(CompilerExecutable), FDelivery);
  FWorker.Start;
end;
destructor TERDStdioLanguageService.Destroy;
begin
  if FDelivery <> nil then FDelivery.Detach;
  if FWorker <> nil then begin FWorker.Stop; FWorker.WaitFor; FreeAndNil(FWorker); end;
  FDelivery := nil; inherited;
end;
procedure TERDStdioLanguageService.Request(const Snapshot: TERDLanguageRequest; const Responses: IERDLanguageResponses);
begin
  if Length(Snapshot.Text) > 1024 * 1024 then raise Exception.Create('Document exceeds language-server size limit');
  FWorker.Enqueue(Snapshot, Responses);
end;
procedure TERDStdioLanguageService.DocumentChanged(const Uri, Text: UnicodeString; Version: Int64);
begin
  if Length(Text) > 1024 * 1024 then raise Exception.Create('Document exceeds language-server size limit');
  FWorker.SyncDocument(Uri, Text, Version);
end;
procedure TERDStdioLanguageService.Cancel(RequestID: Int64);
begin if FWorker <> nil then FWorker.Cancel(RequestID); end;
constructor TERDLSPWorker.Create(const Executable: string; const Delivery: IERDLSPDelivery);
begin
  inherited Create(True); FreeOnTerminate := False;
  FExecutable := Executable; FDelivery := Delivery;
  FLock := TCriticalSection.Create; FWake := TEvent.Create(nil, False, False, '');
  FJobs := TList<TERDLSPJob>.Create;
end;
destructor TERDLSPWorker.Destroy;
begin
  if FLock <> nil then ResetProcess;
  FJobs.Free; FWake.Free; FLock.Free; inherited;
end;
procedure TERDLSPWorker.Stop;
begin
  Terminate; FWake.SetEvent;
  FLock.Acquire;
  try if FProcess.hProcess <> 0 then TerminateProcess(FProcess.hProcess, 0);
  finally FLock.Release; end;
end;
procedure TERDLSPWorker.Enqueue(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses);
var Job: TERDLSPJob;
begin
  Job := Default(TERDLSPJob); Job.Request := Request; Job.Responses := Responses;
  FLock.Acquire; try FJobs.Add(Job); finally FLock.Release; end; FWake.SetEvent;
end;
procedure TERDLSPWorker.SyncDocument(const Uri, Text: UnicodeString; Version: Int64);
var Job: TERDLSPJob; I: Integer;
begin
  Job := Default(TERDLSPJob); Job.SyncOnly := True;
  Job.Request.DocumentUri := Uri; Job.Request.Text := Text; Job.Request.Version := Version;
  FLock.Acquire;
  try
    for I := FJobs.Count - 1 downto 0 do
      if FJobs[I].SyncOnly and (FJobs[I].Request.DocumentUri = Uri) then FJobs.Delete(I);
    FJobs.Add(Job);
  finally FLock.Release; end;
  FWake.SetEvent;
end;
procedure TERDLSPWorker.Cancel(ID: Int64);
var I: Integer;
begin
  FLock.Acquire;
  try
    if FActiveID = ID then FCancelled := True;
    for I := FJobs.Count - 1 downto 0 do if FJobs[I].Request.ID = ID then FJobs.Delete(I);
  finally FLock.Release; end;
end;
procedure TERDLSPWorker.ResetProcess;
begin
  FLock.Acquire;
  try
    if FProcess.hProcess <> 0 then
    begin
      if WaitForSingleObject(FProcess.hProcess, 0) = WAIT_TIMEOUT then TerminateProcess(FProcess.hProcess, 0);
      CloseHandle(FProcess.hProcess); FProcess.hProcess := 0;
    end;
    if FProcess.hThread <> 0 then begin CloseHandle(FProcess.hThread); FProcess.hThread := 0; end;
    if FInput <> 0 then begin CloseHandle(FInput); FInput := 0; end;
    if FOutput <> 0 then begin CloseHandle(FOutput); FOutput := 0; end;
    if FErrors <> 0 then begin CloseHandle(FErrors); FErrors := 0; end;
  finally FLock.Release; end;
  FBytes := nil; FDocumentUri := ''; FDocumentText := ''; FErrorText := ''; FWireVersion := 0;
end;
procedure TERDLSPWorker.StartProcess;
var SA: TSecurityAttributes; SI: TStartupInfo; ChildInput, ChildOutput, ChildErrors: THandle;
    Command, Diagnostics, Reply: UnicodeString; Params: TJSONObject; JSON: TJSONValue;
begin
  if FProcess.hProcess <> 0 then Exit;
  ChildInput := 0; ChildOutput := 0; ChildErrors := 0;
  SA := Default(TSecurityAttributes); SA.nLength := SizeOf(SA); SA.bInheritHandle := True;
  try
    if not CreatePipe(ChildInput, FInput, @SA, 0) then RaiseLastOSError;
    if not CreatePipe(FOutput, ChildOutput, @SA, 0) then RaiseLastOSError;
    if not CreatePipe(FErrors, ChildErrors, @SA, 0) then RaiseLastOSError;
    if not SetHandleInformation(FInput, HANDLE_FLAG_INHERIT, 0) then RaiseLastOSError;
    if not SetHandleInformation(FOutput, HANDLE_FLAG_INHERIT, 0) then RaiseLastOSError;
    if not SetHandleInformation(FErrors, HANDLE_FLAG_INHERIT, 0) then RaiseLastOSError;
    SI := Default(TStartupInfo); SI.cb := SizeOf(SI); SI.dwFlags := STARTF_USESTDHANDLES;
    SI.hStdInput := ChildInput; SI.hStdOutput := ChildOutput; SI.hStdError := ChildErrors;
    Command := '"' + FExecutable + '" lsp --stdio'; UniqueString(Command);
    FLock.Acquire;
    try
      if Terminated then Exit;
      if not CreateProcess(PChar(FExecutable), PChar(Command), nil, nil, True,
        CREATE_NO_WINDOW, nil, nil, SI, FProcess) then RaiseLastOSError;
    finally FLock.Release; end;
  finally
    if ChildInput <> 0 then CloseHandle(ChildInput);
    if ChildOutput <> 0 then CloseHandle(ChildOutput);
    if ChildErrors <> 0 then CloseHandle(ChildErrors);
  end;
  FRpcID := 1; Params := TJSONObject.Create;
  Params.AddPair('capabilities', TJSONObject.Create);
  WriteMessage(MessageObject('initialize', Params, FRpcID)); Reply := ReadReply(FRpcID, Diagnostics);
  JSON := TJSONObject.ParseJSONValue(Reply);
  try if JsonValue(JSON, 'error') <> nil then raise Exception.Create('Language server initialization failed');
  finally JSON.Free; end;
  WriteMessage(MessageObject('initialized', TJSONObject.Create));
end;
procedure TERDLSPWorker.WriteMessage(Value: TJSONObject);
var Bytes: TBytes; Payload: UnicodeString; Header: AnsiString; At: Integer; Written: DWORD;
begin
  try Payload := Value.ToJSON; finally Value.Free; end;
  Bytes := TEncoding.UTF8.GetBytes(Payload);
  Header := AnsiString('Content-Length: ' + IntToStr(Length(Bytes)) + #13#10#13#10);
  if not WriteFile(FInput, Header[1], Length(Header), Written, nil) or (Written <> DWORD(Length(Header))) then RaiseLastOSError;
  At := 0;
  while At < Length(Bytes) do
  begin
    if Terminated then raise Exception.Create('Language server stopped');
    if not WriteFile(FInput, Bytes[At], Min(4096, Length(Bytes) - At), Written, nil) then RaiseLastOSError;
    if Written = 0 then raise Exception.Create('Language server pipe closed');
    Inc(At, Written);
  end;
end;
procedure TERDLSPWorker.DrainErrors;
var Available, Read: DWORD; Bytes: TBytes;
begin
  if (FErrors = 0) or not PeekNamedPipe(FErrors, nil, 0, nil, @Available, nil) or (Available = 0) then Exit;
  SetLength(Bytes, Min(Available, DWORD(4096)));
  if ReadFile(FErrors, Bytes[0], Length(Bytes), Read, nil) and (Read > 0) then
    FErrorText := Copy(FErrorText + TEncoding.UTF8.GetString(Bytes, 0, Read), 1, 65536);
end;
function TERDLSPWorker.ReadMessage: UnicodeString;
var Deadline: UInt64; I, Boundary, Size, Count, Original: Integer; Available, Read: DWORD;
    Header, Line: string; Lines: TArray<string>;
begin
  Deadline := GetTickCount64 + 60000;
  while not Terminated do
  begin
    Boundary := -1;
    for I := 0 to Length(FBytes) - 4 do
      if (FBytes[I] = 13) and (FBytes[I + 1] = 10) and (FBytes[I + 2] = 13) and (FBytes[I + 3] = 10) then
      begin Boundary := I; Break; end;
    if Boundary >= 0 then
    begin
      if Boundary > 8192 then raise Exception.Create('LSP header too large');
      Header := TEncoding.ASCII.GetString(FBytes, 0, Boundary); Lines := Header.Split([#13#10]); Size := -1;
      for Line in Lines do
        if SameText(Copy(Line, 1, 15), 'Content-Length:') then
        begin
          if (Size >= 0) or not TryStrToInt(Trim(Copy(Line, 16, MaxInt)), Size) then raise Exception.Create('Invalid LSP length');
        end;
      if (Size <= 0) or (Size > 16 * 1024 * 1024) then raise Exception.Create('LSP frame exceeds limit');
      if Length(FBytes) >= Boundary + 4 + Size then
      begin
        Result := TEncoding.UTF8.GetString(FBytes, Boundary + 4, Size);
        FBytes := Copy(FBytes, Boundary + 4 + Size, Length(FBytes)); Exit;
      end;
    end
    else if Length(FBytes) > 8192 then raise Exception.Create('Invalid LSP header');
    DrainErrors;
    if GetTickCount64 >= Deadline then raise Exception.Create('Language server response timed out');
    if not PeekNamedPipe(FOutput, nil, 0, nil, @Available, nil) then RaiseLastOSError;
    if Available > 0 then
    begin
      Count := Min(Available, DWORD(65536)); Original := Length(FBytes); SetLength(FBytes, Original + Count);
      if not ReadFile(FOutput, FBytes[Original], Count, Read, nil) then RaiseLastOSError;
      if Read = 0 then raise Exception.Create('Language server pipe closed');
      SetLength(FBytes, Original + Integer(Read));
    end
    else
    begin
      if WaitForSingleObject(FProcess.hProcess, 0) <> WAIT_TIMEOUT then
        raise Exception.Create('Language server exited: ' + FErrorText);
      FWake.WaitFor(10);
    end;
  end;
  raise Exception.Create('Language server stopped');
end;
function TERDLSPWorker.ReadReply(ID: Int64; out Diagnostics: UnicodeString): UnicodeString;
var JSON, Params: TJSONValue; Reply: UnicodeString;
begin
  Diagnostics := '';
  while not Terminated do
  begin
    Reply := ReadMessage; JSON := TJSONObject.ParseJSONValue(Reply);
    try
      if not (JSON is TJSONObject) then raise Exception.Create('Invalid LSP JSON response');
      if JsonInt(JSON, 'id', -1) = ID then Exit(Reply);
      if JsonText(JSON, 'method') = 'textDocument/publishDiagnostics' then
      begin
        Params := JsonValue(JSON, 'params');
        if (JsonText(Params, 'uri') = FDocumentUri) and (JsonInt(Params, 'version', -1) = FWireVersion) then
        begin Diagnostics := Reply; if ID < 0 then Exit(''); end;
      end;
    finally JSON.Free; end;
  end;
  raise Exception.Create('Language server stopped');
end;
procedure TERDLSPWorker.QueueDelivery(const Job: TERDLSPJob; const Reply, Diagnostics, ErrorText: UnicodeString);
var Delivery: IERDLSPDelivery; Pending: TERDLSPJob;
    ReplyText, DiagnosticText, FailureText: UnicodeString;
begin
  Delivery := FDelivery; Pending := Job;
  ReplyText := Reply; DiagnosticText := Diagnostics; FailureText := ErrorText;
  TThread.Queue(nil,
    procedure
    begin Delivery.Deliver(Pending.Request, Pending.Responses, ReplyText, DiagnosticText, FailureText); end);
end;
procedure TERDLSPWorker.RunJob(const Job: TERDLSPJob);
var Params, Document, Change: TJSONObject; Changes: TJSONArray;
    Uri, Method, Diagnostics, Reply: UnicodeString; Position: Integer; Cancelled, Changed: Boolean;
begin
  StartProcess; Uri := DocumentUri(Job.Request); Changed := False;
  if Uri <> FDocumentUri then
  begin
    if FDocumentUri <> '' then
    begin
      Params := TJSONObject.Create; Params.AddPair('textDocument', TJSONObject.Create.AddPair('uri', FDocumentUri));
      WriteMessage(MessageObject('textDocument/didClose', Params));
    end;
    Changed := True; FDocumentUri := Uri; FDocumentText := Job.Request.Text; FWireVersion := 1;
    Document := TJSONObject.Create; Document.AddPair('uri', Uri); Document.AddPair('languageId', 'typescript');
    Document.AddPair('version', TJSONNumber.Create(FWireVersion)); Document.AddPair('text', FDocumentText);
    Params := TJSONObject.Create; Params.AddPair('textDocument', Document);
    WriteMessage(MessageObject('textDocument/didOpen', Params));
  end
  else if FDocumentText <> Job.Request.Text then
  begin
    Changed := True; Inc(FWireVersion); FDocumentText := Job.Request.Text;
    Document := TJSONObject.Create; Document.AddPair('uri', Uri); Document.AddPair('version', TJSONNumber.Create(FWireVersion));
    Change := TJSONObject.Create; Change.AddPair('text', FDocumentText); Changes := TJSONArray.Create; Changes.AddElement(Change);
    Params := TJSONObject.Create; Params.AddPair('textDocument', Document); Params.AddPair('contentChanges', Changes);
    WriteMessage(MessageObject('textDocument/didChange', Params));
  end;
  if Job.SyncOnly then
  begin
    Diagnostics := ''; if Changed then ReadReply(-1, Diagnostics);
    QueueDelivery(Job, '', Diagnostics, ''); Exit;
  end;
  case Job.Request.Kind of
    lrCompletion: Method := 'textDocument/completion';
    lrHover: Method := 'textDocument/hover';
  else Method := 'textDocument/definition';
  end;
  Position := Job.Request.Position; if Position < 0 then Position := Job.Request.Caret;
  Params := TJSONObject.Create; Params.AddPair('textDocument', TJSONObject.Create.AddPair('uri', Uri));
  Params.AddPair('position', PositionJSON(Job.Request.Text, Position));
  Inc(FRpcID); WriteMessage(MessageObject(Method, Params, FRpcID)); Reply := ReadReply(FRpcID, Diagnostics);
  FLock.Acquire; try Cancelled := FCancelled; finally FLock.Release; end;
  if not Cancelled then QueueDelivery(Job, Reply, Diagnostics, '');
end;
procedure TERDLSPWorker.Execute;
var Job: TERDLSPJob; HaveJob, Cancelled: Boolean; ErrorText: UnicodeString;
begin
  try
    while not Terminated do
    begin
      FLock.Acquire;
      try
        HaveJob := FJobs.Count > 0;
        if HaveJob then begin Job := FJobs[0]; FJobs.Delete(0); FActiveID := Job.Request.ID; FCancelled := False; end;
      finally FLock.Release; end;
      if not HaveJob then begin FWake.WaitFor(500); Continue; end;
      try RunJob(Job);
      except
        on E: Exception do
        begin
          ErrorText := E.Message; ResetProcess;
          FLock.Acquire; try Cancelled := FCancelled; finally FLock.Release; end;
          if not Terminated and not Cancelled then QueueDelivery(Job, '', '', ErrorText);
        end;
      end;
      Job.Responses := nil; FLock.Acquire; try FActiveID := 0; finally FLock.Release; end;
    end;
  finally ResetProcess; end;
end;
procedure TERDStdioLanguageService.Deliver(const Request: TERDLanguageRequest; const Responses: IERDLanguageResponses;
  const Reply, Diagnostics, ErrorText: UnicodeString);
var JSON, ResultValue, Item, Edit, Range, Params: TJSONValue; Items: TJSONArray;
    Completion: TERDCompletionItem; Completions: TList<TERDCompletionItem>;
    Target: TERDDefinitionTarget; Values: TList<TERDCodeDiagnostic>; Diagnostic: TERDCodeDiagnostic;
    ErrorMessage: UnicodeString; KeepAlive: IERDCodeLanguageService;
begin
  KeepAlive := Self;
  ErrorMessage := ErrorText; JSON := nil;
  try
    if Reply <> '' then JSON := TJSONObject.ParseJSONValue(Reply);
    if JsonValue(JSON, 'error') <> nil then ErrorMessage := JsonText(JsonValue(JSON, 'error'), 'message', 'Language service error');
    FLastError := ErrorMessage;
    if (ErrorMessage <> '') and Assigned(FOnError) then FOnError(Self);
    ResultValue := JsonValue(JSON, 'result');
    if Responses <> nil then case Request.Kind of
      lrCompletion:
        begin
          Completions := TList<TERDCompletionItem>.Create;
          try
            Items := nil;
            if ResultValue is TJSONArray then Items := TJSONArray(ResultValue)
            else if JsonValue(ResultValue, 'items') is TJSONArray then Items := TJSONArray(JsonValue(ResultValue, 'items'));
            if Items <> nil then for Item in Items do
            begin
              if JsonInt(Item, 'insertTextFormat', 1) <> 1 then Continue;
              Completion := Default(TERDCompletionItem); Completion.LabelText := JsonText(Item, 'label');
              Completion.Kind := Integer(EnsureRange(JsonInt(Item, 'kind'), Int64(0), Int64(25)));
              Completion.Detail := JsonText(Item, 'detail');
              if Completion.Detail = '' then Completion.Detail := JsonText(JsonValue(Item, 'labelDetails'), 'detail');
              Completion.Documentation := JsonText(Item, 'documentation');
              if Completion.Documentation = '' then
                Completion.Documentation := JsonText(JsonValue(Item, 'documentation'), 'value');
              Edit := JsonValue(Item, 'textEdit'); Range := JsonValue(Edit, 'range');
              if Range <> nil then
              begin
                Completion.Start := PositionOffset(Request.Text, JsonValue(Range, 'start'));
                Completion.Count := PositionOffset(Request.Text, JsonValue(Range, 'end')) - Completion.Start;
                Completion.InsertText := JsonText(Edit, 'newText', Completion.LabelText);
              end
              else
              begin
                Completion.Start := Request.Caret; Completion.Count := 0;
                Completion.InsertText := JsonText(Item, 'insertText', Completion.LabelText);
              end;
              if Completion.LabelText <> '' then Completions.Add(Completion);
            end;
            Responses.Completion(Request, Completions.ToArray);
          finally Completions.Free; end;
        end;
      lrHover:
        begin
          Item := JsonValue(ResultValue, 'contents');
          if Item is TJSONString then Responses.Hover(Request, Item.Value)
          else Responses.Hover(Request, JsonText(Item, 'value'));
        end;
      lrDefinition:
        begin
          Target := Default(TERDDefinitionTarget);
          if (ResultValue is TJSONArray) and (TJSONArray(ResultValue).Count > 0) then ResultValue := TJSONArray(ResultValue).Items[0];
          Range := JsonValue(ResultValue, 'range'); Target.DocumentUri := JsonText(ResultValue, 'uri');
          Target.Found := (Range <> nil) and (Target.DocumentUri = DocumentUri(Request));
          if Target.Found then
          begin
            Target.DocumentUri := Request.DocumentUri;
            Target.Start := PositionOffset(Request.Text, JsonValue(Range, 'start'));
            Target.Count := PositionOffset(Request.Text, JsonValue(Range, 'end')) - Target.Start;
          end;
          Responses.Definition(Request, Target);
        end;
    end;
  finally JSON.Free; end;
  if (Diagnostics = '') or not Assigned(FOnDiagnostics) then Exit;
  JSON := TJSONObject.ParseJSONValue(Diagnostics); Values := TList<TERDCodeDiagnostic>.Create;
  try
    Params := JsonValue(JSON, 'params'); Item := JsonValue(Params, 'diagnostics');
    if Item is TJSONArray then for ResultValue in TJSONArray(Item) do
    begin
      Range := JsonValue(ResultValue, 'range'); Diagnostic := Default(TERDCodeDiagnostic);
      Diagnostic.Start := PositionOffset(Request.Text, JsonValue(Range, 'start'));
      Diagnostic.Count := PositionOffset(Request.Text, JsonValue(Range, 'end')) - Diagnostic.Start;
      case JsonInt(ResultValue, 'severity', 1) of
        1: Diagnostic.Severity := dsError;
        2: Diagnostic.Severity := dsWarning;
        3: Diagnostic.Severity := dsInformation;
      else Diagnostic.Severity := dsHint;
      end;
      Diagnostic.MessageText := JsonText(ResultValue, 'message'); Diagnostic.Source := JsonText(ResultValue, 'source');
      Values.Add(Diagnostic);
    end;
    FOnDiagnostics(Self, DocumentUri(Request), Request.Text, Values.ToArray);
  finally Values.Free; JSON.Free; end;
end;
end.
