program Runtime;
{$MODE DELPHI}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$CODEPAGE UTF8}
uses
  cthreads, cwstring, SysUtils, Classes, SyncObjs, System.JSON, System.IOUtils,
  ERD.Compat.Functions, ERD.Compat.Socket,
  ERD.Async, ERD.Collections.ThreadedQueue, ERD.Types,
  ERD.Recorder, ERD.Replayer, ERD.Flash.Checkpoint,
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
end;
procedure TestFutures;
var P: IOBDPromise<Integer>; Calls: Integer; Worker: TThread; Token: IOBDCancellationToken;
begin
  Calls := 0;
  P := NewPromise<Integer>;
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
  P := NewPromise<Integer>;
  P.SetError(Exception.Create('worker failed'));
  Check(P.IsFaulted, 'Fault state');
  try P.Await(0); Check(False, 'Faulted await must raise')
  except on E: Exception do Check(E.Message = 'worker failed', 'Faulted await preserves error') end;
  Token := NewCancellationToken;
  P := NewPromise<Integer>(Token);
  Token.Cancel;
  P.SignalCancelled;
  Check(P.IsCancelled, 'Cancellation state');
  try P.Await(0); Check(False, 'Cancelled await must raise')
  except on E: EOBDOperationCancelled do Check(True, 'Cancelled await raises') end;
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
  TestJSON; TestFutures; TestQueue; TestRecorder; TestHash; TestTCP; TestUDP;
  Check(J1939_PGN_DM27 = $FD82, 'DM27 all pending PGN');
  Check(J1939_PGN_DM28 = $FD80, 'DM28 permanent PGN');
  Check(J1939_PGN_DM24 = $FDB6, 'DM24 supported SPNs PGN');
  EnsureOpenSSLLoaded;
  Check(DefaultDoIPTLSOptions.VerifyMode = vmRequire, 'TLS verification defaults');
  Writeln(Checks, ' runtime checks passed.');
end.
