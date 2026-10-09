program Runtime;
{$MODE DELPHI}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$CODEPAGE UTF8}
uses
  cthreads, cwstring, SysUtils, Classes, SyncObjs, System.Diagnostics, System.JSON, System.IOUtils,
  ERD.Compat.Functions, ERD.Compat.Socket,
  ERD.Connection, ERD.Connection.Mock, ERD.Connection.Types, ERD.Adapter,
  ERD.Protocol, ERD.Protocol.Types, ERD.Coding.DataIdentifierIO, ERD.OEM.SeedKey,
  ERD.Flash.VoltageGate, ERD.Service.EVBattery, ERD.Service.EVBattery.Catalog,
  ERD.Service.EVBattery.Types,
  ERD.OEM.UdsClient, ERD.OEM.UdsClient.Async, ERD.Diagnostics.KWP,
  ERD.Async.Task, ERD.Service.VehicleHealth, ERD.Async, ERD.Collections.ThreadedQueue, ERD.Types, ERD.JSON,
  ERD.Recorder, ERD.Replayer, ERD.Flash.Checkpoint, ERD.UDS.Transfer, ERD.Flash.Pipeline,
  ERD.Protocol.DoIP.TLS.OpenSSL, ERD.Protocol.J1939,
  ERD.Connection.WiFi, ERD.Connection.UDP, ERD.Connection.Settings;
var
  Checks: Integer;
procedure Check(Condition: Boolean; const Message_: string);
begin
  Inc(Checks);
  if not Condition then raise Exception.Create(Message_);
end;
procedure TestJSON;
var Root: TJSONValue; Obj: TJSONObject; Value: Integer;
begin
  Root := TJSONObject.ParseJSONValue('{"answer":42,"text":"\u00e9","items":[1,2]}');
  try
    Check(Root is TJSONObject, 'JSON object');
    Obj := TJSONObject(Root);
    Check(Obj.GetValue<Integer>('answer') = 42, 'JSON integer');
    Check(Obj.GetValue<Integer>('missing', -7) = -7, 'JSON missing default');
    Check(Obj.GetValue<TJSONArray>('items').Count = 2, 'JSON array');
    Check(Obj.GetValue<UnicodeString>('text') = WideChar($E9), 'JSON Unicode');
    Value := Obj.GetValue<Integer>('answer');
    Check(Value = 42, 'JSON typed lookup');
  finally Root.Free end;
  try
    Obj := ParseOBDJSONObject('broken JSON'); Obj.Free;
    Check(False, 'Malformed catalog JSON accepted');
  except on E: EOBDConfig do Check(True, 'Malformed catalog JSON raises configuration error') end;
  try
    Obj := ParseOBDJSONObject('[]'); Obj.Free;
    Check(False, 'Array catalog root accepted');
  except on E: EOBDConfig do Check(True, 'Non-object catalog root rejected and cleaned up') end;
end;
procedure TestFutures;
var P: IOBDPromise<Integer>; Ready: IOBDFuture<Integer>; Calls: Integer; Worker: TThread; Token: IOBDCancellationToken;
begin
  Ready := TOBDAsync.FromResult<Integer>(17);
  Check(Ready.IsCompleted and (Ready.Await(0) = 17), 'Static factory returns completed future');
  Ready := TOBDAsync.FromError<Integer>(Exception.Create('factory error'));
  Check(Ready.IsFaulted, 'Static factory returns faulted future');
  try Ready.Await(0); Check(False, 'Faulted factory must raise')
  except on E: Exception do Check(E.Message = 'factory error', 'Faulted factory preserves exception') end;
  Calls := 0;
  P := TOBDAsync.NewPromise<Integer>();
  P.OnComplete(procedure(F: IOBDFuture<Integer>) begin Inc(Calls); Check(F.Await(0) = 42, 'Future handler value') end);
  P.OnComplete(procedure(F: IOBDFuture<Integer>) begin Inc(Calls) end);
  Worker := TThread.CreateAnonymousThread(procedure begin P.SetResult(42) end);
  Worker.FreeOnTerminate := False;
  Worker.Start;
  try
    Check(P.Await(3000) = 42, 'Future cross-thread result');
    Worker.WaitFor;
    Check(Calls = 2, 'Every future handler runs exactly once');
  finally Worker.Free end;
  P := TOBDAsync.NewPromise<Integer>();
  P.SetError(Exception.Create('worker failed'));
  Check(P.IsFaulted, 'Fault state');
  try P.Await(0); Check(False, 'Faulted await must raise')
  except on E: Exception do Check(E.Message = 'worker failed', 'Faulted await preserves error') end;
  Token := NewCancellationToken;
  P := TOBDAsync.NewPromise<Integer>(Token);
  Token.Cancel;
  P.SignalCancelled;
  Check(P.IsCancelled, 'Cancellation state');
  try P.Await(0); Check(False, 'Cancelled await must raise')
  except on E: EOBDOperationCancelled do Check(True, 'Cancelled await raises') end;
end;
procedure TestDeferredThreadStart;
var Client: IOBDUdsClientAsync; Future: IOBDFuture<TOBDDecodedValue>;
  Hub: TOBDKWP; Failed: Boolean; I: Integer;
begin
  Client := CreateUdsClientAsync;
  for I := 1 to 2 do
  begin
    Future := Client.ReadDIDAsync('0xF190', nil);
    Failed := False;
    try Future.Await(2000)
    except on E: Exception do Failed := Future.IsFaulted end;
    Check(Failed, 'UDS worker starts after construction and settles missing-session errors');
  end;
  Future := nil; Client := nil;
  Hub := TOBDKWP.Create(nil);
  try
    for I := 1 to 3 do
    begin Hub.KeepAlive := True; Hub.KeepAlive := False end;
    Check(not Hub.KeepAlive, 'KWP keepalive starts after construction and can restart/stop');
  finally Hub.Free end;
end;
procedure TestQueue;
var Q: TOBDThreadedQueue<Integer>; V: Integer; Worker: TThread; Wait: TWaitResult;
begin
  Q := TOBDThreadedQueue<Integer>.Create(2, 0, 0);
  try
    Check(Q.PopItem(V) = wrTimeout, 'Empty queue');
    Check(Q.PushItem(17) = wrSignaled, 'Queue push');
    Check(Q.PushItem(18) = wrSignaled, 'Second queue push');
    Check(Q.PushItem(19) = wrTimeout, 'Bounded queue');
    Check((Q.PopItem(V) = wrSignaled) and (V = 17), 'Queue FIFO');
    Check((Q.PopItem(V) = wrSignaled) and (V = 18), 'Second FIFO item');
    Check(Q.PopItem(V, 10) = wrTimeout, 'Finite queue timeout');
    Wait := wrError;
    Worker := TThread.CreateAnonymousThread(procedure var Item: Integer; begin Wait := Q.PopItem(Item, INFINITE) end);
    Worker.FreeOnTerminate := False;
    Worker.Start;
    Q.DoShutDown;
    try Worker.WaitFor; Check(Wait = wrAbandoned, 'Shutdown wakes blocked consumer') finally Worker.Free end;
    Check(Q.PushItem(1) = wrAbandoned, 'Shutdown rejects producer');
  finally Q.Free end;
end;
procedure TestRecorder;
var Recorder: TOBDRecorder; Entry: TOBDLogEntry; Entries: TArray<TOBDLogEntry>;
  FileName: string; Bytes: TBytes;
begin
  FileName := IncludeTrailingPathDelimiter(ParamStr(1)) + 'recording.obdlog.gz';
  Recorder := TOBDRecorder.Create(nil);
  try
    Recorder.Open(FileName);
    Entry := Default(TOBDLogEntry);
    Entry.Timestamp := Now; Entry.Kind := leFrame;
    Entry.Raw := TBytes.Create($00,$80,$FF); Entry.FrameID := $7E8; Entry.HasFrameID := True;
    Recorder.Append(Entry);
    Recorder.Close;
  finally Recorder.Free end;
  Bytes := TFile.ReadAllBytes(FileName);
  Check((Length(Bytes) > 2) and (Bytes[0] = $1F) and (Bytes[1] = $8B), 'Real gzip header');
  Entries := TOBDReplayer.LoadAll(FileName);
  Check(Length(Entries) = 1, 'Gzip replay count');
  Check((Length(Entries[0].Raw) = 3) and (Entries[0].Raw[1] = $80) and (Entries[0].Raw[2] = $FF), 'Binary Base64 roundtrip');
  Check(Entries[0].FrameID = $7E8, 'Frame identity roundtrip');
end;
procedure TestCheckpoint;
var Info, Loaded: TOBDFlashCheckpointInfo; FileName: string; I: Integer;
begin
  FileName := IncludeTrailingPathDelimiter(ParamStr(1)) + 'checkpoint.json';
  Info := Default(TOBDFlashCheckpointInfo);
  Info.SessionID := 'checkpoint-regression';
  Info.Cursor.TotalBytes := 100; Info.Cursor.NextBSC := 1; Info.Cursor.MaxChunkBytes := 10;
  Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(TBytes.Create(1,2,3));
  for I := 1 to 100 do
  begin
    Info.Cursor.BytesSent := I;
    TOBDFlashCheckpoint.Save(FileName, Info);
    Loaded := TOBDFlashCheckpoint.Load(FileName);
    Check(Loaded.Cursor.BytesSent = Cardinal(I), 'Checkpoint replacement preserves newest cursor');
  end;
  Info.Cursor.Address := High(UInt64); TOBDFlashCheckpoint.Save(FileName, Info);
  Loaded := TOBDFlashCheckpoint.Load(FileName);
  Check(Loaded.Cursor.Address = High(UInt64), 'Checkpoint preserves full unsigned memory address');
  TFile.WriteAllText(FileName, '{"version":1,"address":0,"total_bytes":8,"bytes_sent":-1,"next_bsc":1,"max_chunk_bytes":4}', TEncoding.UTF8);
  try TOBDFlashCheckpoint.Load(FileName); Check(False, 'Negative checkpoint cursor accepted')
  except on E: EOBDProtocol do Check(True, 'Negative checkpoint cursor rejected without narrowing wrap') end;
  TFile.WriteAllText(FileName, '{"version":1,"address":0,"total_bytes":8,"bytes_sent":4,"next_bsc":256,"max_chunk_bytes":4}', TEncoding.UTF8);
  try TOBDFlashCheckpoint.Load(FileName); Check(False, 'Oversized checkpoint counter accepted')
  except on E: EOBDProtocol do Check(True, 'Oversized checkpoint counter rejected without masking') end;
end;
type
  TScriptedTransport = class(TOBDMockTransport)
    Pending, Reply: string;
    Calls, Commands: Integer;
    Stall, TransferMode, CheckBSC, SawZero: Boolean;
    ExpectedBSC: Byte;
    function WriteBytes(const Bytes: TBytes): Integer; override;
  end;
function TScriptedTransport.WriteBytes(const Bytes: TBytes): Integer;
var Text: string;
begin
  Inc(Calls);
  if Stall then Exit(0);
  Result := Length(Bytes); if Result > 2 then Result := 2;
  inherited WriteBytes(Copy(Bytes, 0, Result));
  Text := TEncoding.ASCII.GetString(Bytes, 0, Result);
  Pending := Pending + Text;
  if Pos(#13, Pending) > 0 then
  begin
    Pending := StringReplace(Pending, ' ', '', [rfReplaceAll]);
    Inc(Commands);
    if TransferMode then
    begin
      if Copy(Pending,1,2) = '34' then FeedString('74 20 00 06' + #13 + '>')
      else if Copy(Pending,1,2) = '36' then
      begin
        if CheckBSC then
        begin
          if Copy(Pending,3,2) <> IntToHex(ExpectedBSC,2) then raise Exception.Create('Incorrect UDS block sequence');
          if ExpectedBSC = 0 then SawZero := True;
          ExpectedBSC := Byte((Integer(ExpectedBSC) + 1) and $FF);
        end;
        FeedString('76 ' + Copy(Pending,3,2) + #13 + '>');
      end
      else if Copy(Pending,1,2) = '37' then FeedString('77' + #13 + '>')
      else raise Exception.Create('Unexpected transfer request: ' + Pending);
    end
    else if Reply <> '' then FeedString(Reply + #13 + '>')
    else if Pos('F190', Pending) > 0 then FeedString('62 F1 90 AA F1 91 BB' + #13 + '>')
    else FeedString('62 F1 91 CC' + #13 + '>');
    Pending := '';
  end;
end;
procedure TestDiagnosticWire;
var Mock: TScriptedTransport; Transport: IOBDConnectionTransport;
  Connection: TOBDConnection; Adapter: TOBDAdapter; Protocol: TOBDProtocol;
  Reader: TOBDDataIdentifierIO; Values: TArray<TOBDDIDValue>; Before: Integer;
  procedure RejectStrict(const Reply: string; Len1, Len2: Integer);
  begin
    Mock.Reply := Reply;
    try Reader.ReadStrict([$F190,$F191], [Len1,Len2]); Check(False, 'Invalid strict DID reply accepted')
    except on E: EOBDProtocolErr do Check(True, 'Invalid strict DID reply rejected') end;
  end;
begin
  Mock := TScriptedTransport.Create; Transport := Mock; Mock.SimulateOpen;
  Connection := TOBDConnection.Create(nil); Adapter := TOBDAdapter.Create(nil);
  Protocol := TOBDProtocol.Create(nil); Reader := TOBDDataIdentifierIO.Create(nil);
  try
    Connection.CustomTransport := Transport; Connection.Open;
    Adapter.Connection := Connection; Protocol.Adapter := Adapter;
    Protocol.Application := apUDS; Reader.Protocol := Protocol;
    Values := Reader.Read([$F190,$F191]);
    Check((Length(Values) = 2) and (Length(Values[0].Data) = 4) and
      (Values[0].Data[1] = $F1) and (Values[0].Data[2] = $91), 'DID payload preserves embedded next identifier');
    Check((Mock.Commands = 2) and (Values[1].Data[0] = $CC), 'Unknown DID lengths use separate requests');
    Check(Mock.Calls > Mock.Commands, 'Adapter handles partial writes through actual protocol path');
    Mock.Reply := '62 F1 90 AA F1 91 CC';
    Values := Reader.ReadStrict([$F190,$F191], [1,1]);
    Check((Length(Values) = 2) and (Values[0].Data[0] = $AA), 'Known-length batch response');
    RejectStrict('62 F1 90 AA F1 91 CC DD', 1,1);
    RejectStrict('62 F1 90 AA F1 91', 1,1);
    RejectStrict('62 F1 90 AA F1 92 CC', 1,1);
    RejectStrict('7F 22 31', 1,1);
    Before := Mock.Commands;
    try Reader.ReadStrict([$F190], [-1]); Check(False, 'Negative DID length accepted')
    except on E: EOBDConfig do Check(Mock.Commands = Before, 'Invalid length rejected before wire access') end;
    Mock.Stall := True;
    try Connection.WriteAll(TBytes.Create(1,2,3), 100); Check(False, 'Stalled write accepted')
    except on E: EOBDError do Check(True, 'Stalled write rejected') end;
  finally Reader.Free; Protocol.Free; Adapter.Free; Connection.Free; Transport := nil end;
end;
procedure TestFlashRecovery;
var Mock: TScriptedTransport; Transport: IOBDConnectionTransport;
  Connection: TOBDConnection; Adapter: TOBDAdapter; Protocol: TOBDProtocol;
  Pipeline: TOBDFlashPipeline; Info: TOBDFlashCheckpointInfo;
  Image: TBytes; Path: string; ConfirmCalls: Integer; CheckpointFailed: Boolean;
begin
  Mock := TScriptedTransport.Create; Transport := Mock; Mock.SimulateOpen; Mock.TransferMode := True;
  Connection := TOBDConnection.Create(nil); Adapter := TOBDAdapter.Create(nil);
  Protocol := TOBDProtocol.Create(nil); Pipeline := TOBDFlashPipeline.Create(nil);
  Image := TBytes.Create(1,2,3,4,5,6,7,8);
  try
    Connection.CustomTransport := Transport; Connection.Open; Adapter.Connection := Connection;
    Protocol.Adapter := Adapter; Protocol.Application := apUDS;
    Pipeline.Protocol := Protocol; Pipeline.AutoExecute := True; Pipeline.ResetAfterFlash := False;
    Pipeline.TargetVendor := 'fixture'; Pipeline.TargetModule := 'engine'; Pipeline.ECUIdentity := 'ECU-1';
    Pipeline.CheckpointFile := IncludeTrailingPathDelimiter(ParamStr(1)) + 'missing-dir/checkpoint.json';
    CheckpointFailed := False;
    try Pipeline.Flash($1000, Image)
    except on E: Exception do CheckpointFailed := True end;
    Check(CheckpointFailed and (Mock.Commands = 2), 'Checkpoint error raises and aborts before next block or transfer exit');
    Info := Default(TOBDFlashCheckpointInfo);
    Info.SessionID := 'session'; Info.Vendor := 'fixture'; Info.Module := 'engine'; Info.ECUIdentity := 'ECU-1';
    Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(Image);
    Info.Cursor.Address := $1000; Info.Cursor.TotalBytes := 8; Info.Cursor.BytesSent := 4;
    Info.Cursor.NextBSC := 2; Info.Cursor.MaxChunkBytes := 4;
    Path := IncludeTrailingPathDelimiter(ParamStr(1)) + 'resume.json';
    TOBDFlashCheckpoint.Save(Path, Info); Pipeline.CheckpointFile := Path;
    ConfirmCalls := 0; Mock.Commands := 0;
    Pipeline.ResumeFromCheckpoint(Path, Image, 'session',
      function(const CP: TOBDFlashCheckpointInfo): Boolean
      begin Inc(ConfirmCalls); Result := (CP.ECUIdentity = 'ECU-1') and (CP.Cursor.BytesSent = 4) end);
    Check((ConfirmCalls = 1) and (Mock.Commands = 2), 'Resume sends remaining block and exit without fresh RequestDownload');
    Info := TOBDFlashCheckpoint.Load(Path);
    Check((Info.Cursor.BytesSent = 8) and (Info.SessionID = 'session'), 'Resumed checkpoint preserves session and accepted cursor');
    Info.Cursor.BytesSent := 4; TOBDFlashCheckpoint.Save(Path, Info); Mock.Commands := 0;
    Image[0] := 99;
    try Pipeline.ResumeFromCheckpoint(Path, Image, 'session', nil); Check(False, 'Different firmware accepted')
    except on E: EOBDConfig do Check(Mock.Commands = 0, 'Different firmware rejected before wire access') end;
    Image[0] := 1;
    Pipeline.ECUIdentity := 'other';
    try Pipeline.ResumeFromCheckpoint(Path, Image, 'session', nil); Check(False, 'Different ECU accepted')
    except on E: EOBDConfig do Check(Mock.Commands = 0, 'Different ECU rejected before wire access') end;
    Pipeline.ECUIdentity := 'ECU-1';
    try Pipeline.ResumeFromCheckpoint(Path, Image, 'session',
      function(const CP: TOBDFlashCheckpointInfo): Boolean begin Result := False end);
      Check(False, 'Lost ECU transfer accepted')
    except on E: EOBDConfig do Check(Mock.Commands = 0, 'ECU state confirmation failure prevents resume') end;
    Mock.CheckBSC := True; Mock.ExpectedBSC := 1; Mock.Commands := 0;
    Pipeline.CheckpointFile := ''; SetLength(Image, 1024);
    Pipeline.Flash($1000, Image);
    Check((Mock.Commands = 258) and Mock.SawZero and (Mock.ExpectedBSC = 1), '256 accepted UDS blocks wrap FF to 00');
    Info.Cursor.TotalBytes := 1024; Info.Cursor.BytesSent := 1020; Info.Cursor.NextBSC := 0;
    Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(Image); TOBDFlashCheckpoint.Save(Path, Info);
    Mock.Commands := 0; Mock.ExpectedBSC := 0; Mock.SawZero := False;
    Pipeline.ResumeFromCheckpoint(Path, Image, 'session',
      function(const CP: TOBDFlashCheckpointInfo): Boolean begin Result := CP.Cursor.NextBSC = 0 end);
    Check((Mock.Commands = 2) and Mock.SawZero, 'Resume preserves valid block counter zero at wrap boundary');
  finally Pipeline.Free; Protocol.Free; Adapter.Free; Connection.Free; Transport := nil end;
end;
procedure TestSeedKeyPolicy;
var Registry: TOBDSeedKeyRegistry; Bytes: TBytes;
begin
  Registry := TOBDSeedKeyRegistry.Create;
  try
    Registry.RegisterAlgorithm(1, IOBDSeedKeyAlgorithm(TOBDSeedKeyConstant.Create(TBytes.Create($AA))));
    try Registry.ComputeKey(1, TBytes.Create(1)); Check(False, 'Unverified production key accepted')
    except on E: EOBDSeedKey do Check(True, 'Unverified providers excluded by default') end;
    Registry.AllowUnverified := True;
    Bytes := Registry.ComputeKey(1, TBytes.Create(1));
    Check((Length(Bytes) = 1) and (Bytes[0] = $AA), 'Explicit lab opt-in works');
    Registry.AllowUnverified := False;
    Registry.RegisterAlgorithm(1, IOBDSeedKeyAlgorithm(TOBDSeedKeyConstant.Create(TBytes.Create($BB), 'fixture', 'test', True)));
    Registry.RegisterAlgorithm(1, IOBDSeedKeyAlgorithm(TOBDSeedKeyConstant.Create(TBytes.Create($CC))));
    Bytes := Registry.ComputeKey(1, TBytes.Create(1));
    Check(Bytes[0] = $BB, 'Unverified newest provider cannot shadow eligible provider');
  finally Registry.Free end;
end;
procedure TestReplayLimits;
var Player: TOBDReplayer; Path: string; Lines: TStringList;
begin
  Path := IncludeTrailingPathDelimiter(ParamStr(1)) + 'limits.obdlog';
  Player := TOBDReplayer.Create(nil);
  try
    TFile.WriteAllText(Path, StringOfChar('x', 40), TEncoding.UTF8);
    Player.FileName := Path; Player.MaxLineBytes := 8;
    try Player.Play; Check(False, 'Replay oversized line accepted')
    except on E: EOBDProtocolErr do Check(True, 'Replay line bound enforced') end;
    Player.MaxLineBytes := 100; Player.MaxDecodedBytes := 8;
    try Player.Play; Check(False, 'Replay oversized decoded input accepted')
    except on E: EOBDProtocolErr do Check(True, 'Replay decoded byte bound enforced') end;
    Player.FileName := IncludeTrailingPathDelimiter(ParamStr(1)) + 'recording.obdlog.gz';
    try Player.Play; Check(False, 'Gzip expanded byte limit accepted')
    except on E: EOBDProtocolErr do Check(True, 'Real gzip expansion bound enforced') end;
    TFile.WriteAllText(Path, 'broken JSON', TEncoding.UTF8); Player.MaxDecodedBytes := 100;
    try TOBDReplayer.LoadAll(Path); Check(False, 'Malformed replay accepted')
    except on E: EOBDProtocolErr do Check(True, 'Malformed replay rejected') end;
    TFile.WriteAllText(Path, 'a' + #10 + 'b' + #10, TEncoding.UTF8); Player.MaxEntries := 1;
    try Lines := Player.LoadLines(Path); Lines.Free; Check(False, 'Replay excess entries accepted')
    except on E: EOBDProtocolErr do Check(True, 'Replay entry count bound enforced') end;
  finally Player.Free end;
end;
procedure TestOwnedTask;
var Task: TOBDOwnedTask; Started: TEvent; Calls: Integer; Owner: TOBDVehicleHealth; I: Integer;
begin
  Calls := 0;
  Started := TEvent.Create(nil, True, False, '');
  Task := TOBDOwnedTask.Create;
  try
    Task.Start(procedure begin Task.Post(procedure begin Inc(Calls) end); Started.SetEvent end);
    Check(Started.WaitFor(1000) = wrSignaled, 'Worker queued callback');
    Task.Cancel;
    CheckSynchronize;
    Check(Calls = 0, 'Cancel suppresses queued owner callback');
    Task.Start(procedure begin Task.Post(procedure begin Inc(Calls) end) end);
    for I := 1 to 100 do begin CheckSynchronize(1); if Calls > 0 then Break end;
    Check(Calls = 1, 'Task can restart after cancel');
  finally Task.Free; Started.Free end;
  for I := 1 to 100 do
  begin
    Owner := TOBDVehicleHealth.Create(nil);
    Owner.SnapshotAsync;
    Owner.Free;
    CheckSynchronize;
  end;
  Check(True, 'Destroy waits for async snapshots');
end;
type
  TEVDecoderProbe = class(TOBDEVBattery)
  public
    procedure Decode(const Rule: TOBDEVBatteryRule; const Bytes: TBytes; var Snapshot: TOBDEVBatterySnapshot);
  end;
procedure TEVDecoderProbe.Decode(const Rule: TOBDEVBatteryRule; const Bytes: TBytes; var Snapshot: TOBDEVBatterySnapshot);
begin ApplyDecoded(Rule, Bytes, Snapshot) end;
procedure TestBMWCurrent;
var Catalog: TOBDEVBatteryVendorCatalog; Rule: TOBDEVBatteryRule;
  Decoder: TEVDecoderProbe; Snapshot: TOBDEVBatterySnapshot; Found: Boolean;
begin
  TOBDEVBatteryCatalog.CatalogDir := ParamStr(4);
  TOBDEVBatteryCatalog.Reload;
  Check(Length(TOBDEVBatteryCatalog.VendorKeys) = 16, 'All 15 shipped EV vendors plus the fixture load through production parser');
  Check(TOBDEVBatteryCatalog.TryGet('bmw', Catalog), 'BMW catalog loads');
  Found := False;
  for Rule in Catalog.Rules do if Rule.FieldName = 'pack_current' then begin Found := True; Break end;
  Check(Found and (Rule.DIDOrPID = $DD69) and (Rule.Length = 4) and Rule.Signed, 'BMW matches pinned OVMS wire definition');
  Decoder := TEVDecoderProbe.Create(nil);
  try
    Snapshot := Default(TOBDEVBatterySnapshot);
    Decoder.Decode(Rule, TBytes.Create(0,0,0,0), Snapshot);
    Check(Abs(Snapshot.PackCurrent) < 0.00001, 'BMW zero current');
    Decoder.Decode(Rule, TBytes.Create(0,0,$27,$10), Snapshot);
    Check(Abs(Snapshot.PackCurrent + 100) < 0.00001, 'BMW 100 A charge');
    Decoder.Decode(Rule, TBytes.Create($FF,$FF,$D8,$F0), Snapshot);
    Check(Abs(Snapshot.PackCurrent - 100) < 0.00001, 'BMW 100 A discharge');
    Check(TOBDEVBatteryCatalog.TryGet('hmg', Catalog), 'HMG catalog loads');
    for Rule in Catalog.Rules do if Rule.FieldName = 'cell_voltages_97_98' then Break;
    Snapshot := Default(TOBDEVBatterySnapshot);
    Decoder.Decode(Rule, TBytes.Create(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,50,$FF), Snapshot);
    Check((Length(Snapshot.CellVoltages) = 2) and (Abs(Snapshot.CellVoltages[1] - 1) < 0.00001), 'Array decoding respects declared slice and excludes trailing bytes');
    try Decoder.Decode(Rule, TBytes.Create(1,2), Snapshot); Check(False, 'Truncated EV array accepted')
    except on E: EOBDProtocolErr do Check(True, 'Truncated EV array rejected') end;
  finally Decoder.Free end;
end;
procedure TestCancellation;
var Task: TOBDOwnedTask; Started: TEvent; Before: TStopwatch; Finished: Boolean;
  Gate: TOBDVoltageGate;
begin
  Task := TOBDOwnedTask.Create; Started := TEvent.Create(nil, True, False, ''); Finished := False;
  try
    Task.Start(procedure begin Started.SetEvent; try Task.Delay(60000) except on E: EAbort do Finished := True end end);
    Check(Started.WaitFor(1000) = wrSignaled, 'Long worker delay started');
    Before := TStopwatch.StartNew; Task.Cancel;
    Check(Finished and (Before.ElapsedMilliseconds < 1000), 'Cancellation interrupts long worker delay');
    Started.ResetEvent; Finished := False;
    Task.Start(procedure begin Started.SetEvent; try Task.Synchronize(procedure begin Check(False, 'Cancelled consent invoked') end) except on E: EAbort do Finished := True end end);
    Check(Started.WaitFor(1000) = wrSignaled, 'Consent worker started');
    Task.Cancel; CheckSynchronize;
    Check(Finished, 'Cancellation releases worker waiting for consent');
  finally Task.Free; Started.Free end;
  Gate := TOBDVoltageGate.Create(nil);
  try
    Gate.SourceFunc := function: Double begin Result := 13 end;
    Gate.PollIntervalMs := 60000;
    Gate.Start;
    Before := TStopwatch.StartNew; Gate.Stop;
    Check(Before.ElapsedMilliseconds < 1000, 'Voltage gate stop wakes long poll interval');
  finally Gate.Free end;
end;
type
  TReplayObserver = class
    Owner: TOBDReplayer;
    Calls: Integer;
    procedure Entry(Sender: TObject; const Value: TOBDLogEntry);
  end;
procedure TReplayObserver.Entry(Sender: TObject; const Value: TOBDLogEntry);
var Player: TOBDReplayer;
begin
  Inc(Calls); Player := Owner; Owner := nil; Player.Free;
end;
procedure TestReplayDestroyInCallback;
var Observer: TReplayObserver; Recorder: TOBDRecorder; Entry: TOBDLogEntry;
  FileName: string; Watch: TStopwatch;
begin
  FileName := IncludeTrailingPathDelimiter(ParamStr(1)) + 'realtime.obdlog';
  Recorder := TOBDRecorder.Create(nil);
  try
    Recorder.Open(FileName); Entry := Default(TOBDLogEntry);
    Entry.Kind := leInfo; Entry.Timestamp := Now; Recorder.Append(Entry);
    Entry.Timestamp := Entry.Timestamp + 1 / 1440; Recorder.Append(Entry); Recorder.Close;
  finally Recorder.Free end;
  Observer := TReplayObserver.Create;
  Observer.Owner := TOBDReplayer.Create(nil);
  try
    Observer.Owner.FileName := FileName; Observer.Owner.Mode := rmRealTime;
    Observer.Owner.OnEntry := Observer.Entry; Observer.Owner.PlayAsync;
    Watch := TStopwatch.StartNew;
    while (Observer.Owner <> nil) and (Watch.ElapsedMilliseconds < 2000) do CheckSynchronize(10);
    Check((Observer.Owner = nil) and (Observer.Calls = 1), 'Replay can be destroyed inside first queued entry during long timestamp gap');
    CheckSynchronize;
    Check(Observer.Calls = 1, 'Queued replay entries suppressed after destruction');
  finally Observer.Owner.Free; Observer.Free end;
end;
type
  TVoltageObserver = class
    Ready: TEvent;
    UICalls: Integer;
    procedure SafetyAbort(Sender: TObject; Voltage: Double; Reason: string);
    procedure UIAbort(Sender: TObject; Voltage: Double; Reason: string);
  end;
procedure TVoltageObserver.SafetyAbort(Sender: TObject; Voltage: Double; Reason: string);
begin Ready.SetEvent end;
procedure TVoltageObserver.UIAbort(Sender: TObject; Voltage: Double; Reason: string);
begin Inc(UICalls) end;
procedure TestVoltageSafetyHook;
var Gate: TOBDVoltageGate; Observer: TVoltageObserver; Watch: TStopwatch;
begin
  Gate := TOBDVoltageGate.Create(nil); Observer := TVoltageObserver.Create;
  Observer.Ready := TEvent.Create(nil, True, False, '');
  try
    Gate.SourceFunc := function: Double begin Result := 9 end;
    Gate.HoldTimeMs := 0; Gate.OnAbortExecutingThread := Observer.SafetyAbort;
    Gate.OnAbort := Observer.UIAbort; Gate.Start;
    Check(Observer.Ready.WaitFor(1000) = wrSignaled, 'Voltage safety abort executes without pumping the UI queue');
    Check(Observer.UICalls = 0, 'UI voltage notification remains marshalled');
    CheckSynchronize;
    // The safety event is signalled before the worker queues the UI callback.
    Watch := TStopwatch.StartNew;
    while (Observer.UICalls = 0) and (Watch.ElapsedMilliseconds < 1000) do CheckSynchronize(10);
    Check(Observer.UICalls = 1, 'Voltage UI abort delivered separately');
    Gate.Stop;
  finally Gate.Free; Observer.Ready.Free; Observer.Free end;
end;
procedure TestHash;
var Hash: TBytes; Hex: string; B: Byte;
begin
  Hash := TOBDFlashCheckpoint.ComputeImageHash(TBytes.Create(97,98,99));
  Hex := ''; for B in Hash do Hex := Hex + LowerCase(IntToHex(B,2));
  Check(Hex = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad', 'SHA256 known vector');
end;
type
  TReceiver = class
    Ready: TEvent;
    Data: TBytes;
    procedure Receive(Sender: TObject; const Bytes: TBytes);
  end;
procedure TReceiver.Receive(Sender: TObject; const Bytes: TBytes);
begin
  Data := Copy(Bytes);
  Ready.SetEvent;
end;
procedure TestTCP;
var Transport: TOBDWiFiTransport; Settings: TOBDWiFiSettings; Receiver: TReceiver;
begin
  Transport := TOBDWiFiTransport.Create;
  Settings := TOBDWiFiSettings.Create;
  Receiver := TReceiver.Create;
  Receiver.Ready := TEvent.Create(nil, True, False, '');
  try
    Settings.Host := '127.0.0.1'; Settings.Port := StrToInt(ParamStr(2));
    Settings.ConnectTimeout := 1000;
    Transport.SetOnDataReceived(Receiver.Receive);
    Transport.Open(Settings);
    Check(Transport.WriteBytes(TBytes.Create(80,73,78,71)) = 4, 'TCP send');
    Check(Receiver.Ready.WaitFor(2000) = wrSignaled, 'WiFi delivers short response while peer stays open');
    Check((Length(Receiver.Data) = 3) and (Receiver.Data[0] = 79) and (Receiver.Data[2] = 62), 'WiFi received short response');
    Transport.Close;
  finally
    Transport.Free; Settings.Free; Receiver.Ready.Free; Receiver.Free;
  end;
end;

procedure TestNativeWriteDeadline;
var Connection: TOBDConnection; Data: TBytes; Watch: TStopwatch;
begin
  Connection := TOBDConnection.Create(nil);
  try
    Connection.Transport := otWiFi; Connection.WiFiSettings.Host := '127.0.0.1';
    Connection.WiFiSettings.Port := StrToInt(ParamStr(5)); Connection.Open;
    SetLength(Data, 32 * 1024 * 1024); Watch := TStopwatch.StartNew;
    try Connection.WriteAll(Data, 100); Check(False, 'Blocked native TCP write completed without a reading peer')
    except on E: EOBDError do Check(Watch.ElapsedMilliseconds < 2000, 'Native write deadline bounds blocked send') end;
  finally Connection.Free end;
end;
procedure TestUDP;
var Transport: TOBDUDPTransport; Settings: TOBDUDPSettings; Receiver: TReceiver;
begin
  Check(TIPAddress.LookupName('localhost').IPv4Address.s_addr = TIPAddress.LookupName('127.0.0.1').IPv4Address.s_addr, 'Hostname and numeric IP byte order');
  Transport := TOBDUDPTransport.Create;
  Settings := TOBDUDPSettings.Create;
  Receiver := TReceiver.Create;
  Receiver.Ready := TEvent.Create(nil, True, False, '');
  try
    Settings.Host := '127.0.0.1'; Settings.Port := StrToInt(ParamStr(3));
    Settings.BindLocal := True; Settings.LocalPort := 0;
    Transport.SetOnDataReceived(Receiver.Receive);
    Transport.Open(Settings);
    Check(Transport.WriteBytes(TBytes.Create($00,$80,$FF)) = 3, 'UDP send');
    Check(Receiver.Ready.WaitFor(2000) = wrSignaled, 'UDP response');
    Check((Length(Receiver.Data) = 3) and (Receiver.Data[1] = $80) and (Receiver.Data[2] = $FF), 'UDP binary roundtrip');
    Transport.Close;
  finally Transport.Free; Settings.Free; Receiver.Ready.Free; Receiver.Free end;
end;

begin
  TestDeferredThreadStart; TestTCP; TestUDP; TestNativeWriteDeadline; TestJSON; TestFutures; TestQueue; TestRecorder; TestDiagnosticWire; TestFlashRecovery; TestSeedKeyPolicy; TestReplayLimits; TestCheckpoint; TestOwnedTask; TestCancellation; TestReplayDestroyInCallback; TestVoltageSafetyHook; TestBMWCurrent; TestHash;
  Check(J1939_PGN_DM27 = $FD82, 'DM27 all pending PGN');
  Check(J1939_PGN_DM28 = $FD80, 'DM28 permanent PGN');
  Check(J1939_PGN_DM24 = $FDB6, 'DM24 supported SPNs PGN');
  EnsureOpenSSLLoaded;
  Check(DefaultDoIPTLSOptions.VerifyMode = vmRequire, 'TLS verification defaults');
  Writeln(Checks, ' runtime checks passed.');
end.
