//------------------------------------------------------------------------------
// UNIT           : Tests.Audit
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Audit;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAuditTests = class
  public
    [Test] procedure SuccessRoutesToInfoLevel_AndJsonHasOutcome;
    [Test] procedure FailureRoutesToErrorLevel;
    [Test] procedure DeniedRoutesToWarningLevel;
    [Test] procedure SourceTagSetToAudit;
    [Test] procedure SourceTagRestoredAfterRecord;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  OBD.Logger, OBD.Logger.Sinks, OBD.Audit;

type
  TCapturingSink = class(TInterfacedObject, IOBDLogSink)
  private
    FEvents: TList<TOBDLogEvent>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(const Event: TOBDLogEvent);
    procedure Flush;
    function At(I: Integer): TOBDLogEvent;
    function Count: Integer;
  end;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TCapturingSink.Create;
begin
  inherited Create;
  FEvents := TList<TOBDLogEvent>.Create;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TCapturingSink.Destroy;
begin
  FEvents.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// WRITE
//------------------------------------------------------------------------------
procedure TCapturingSink.Write(const Event: TOBDLogEvent);
begin
  FEvents.Add(Event);
end;

//------------------------------------------------------------------------------
// FLUSH
//------------------------------------------------------------------------------
procedure TCapturingSink.Flush;
begin
end;

//------------------------------------------------------------------------------
// AT
//------------------------------------------------------------------------------
function TCapturingSink.At(I: Integer): TOBDLogEvent;
begin
  Result := FEvents[I];
end;

//------------------------------------------------------------------------------
// COUNT
//------------------------------------------------------------------------------
function TCapturingSink.Count: Integer;
begin
  Result := FEvents.Count;
end;

//------------------------------------------------------------------------------
// SCRATCH LOG PATH
//------------------------------------------------------------------------------
function ScratchLogPath: string;
begin
  Result := TPath.Combine(TPath.GetTempPath,
    Format('obdaudit-%d-%d.log', [GetCurrentProcessId, GetTickCount]));
end;

//------------------------------------------------------------------------------
// SUCCESS ROUTES TO INFO LEVEL_AND JSON HAS OUTCOME
//------------------------------------------------------------------------------
procedure TAuditTests.SuccessRoutesToInfoLevel_AndJsonHasOutcome;
var
  Path: string;
  Logger: TOBDLogger;
  Sink: TCapturingSink;
  Audit: TOBDAuditRecorder;
  Json: TJSONObject;
begin
  Path := ScratchLogPath;
  Logger := TOBDLogger.Create(Path);
  try
    Logger.MinLogLevel := llDebug;
    Sink := TCapturingSink.Create;
    Logger.RegisterSink(Sink);

    Audit := TOBDAuditRecorder.Create(Logger);
    try
      Audit.Success('user:erdesigns', 'flash.start', 'audi.mmi3g',
        'detail-text');
    finally
      Audit.Free;
    end;

    Assert.AreEqual(1, Sink.Count);
    Assert.AreEqual(Ord(lsInfo), Ord(Sink.At(0).Level));

    Json := TJSONObject.ParseJSONValue(Sink.At(0).Message) as TJSONObject;
    try
      Assert.AreEqual('success', Json.GetValue('outcome').Value);
      Assert.AreEqual('user:erdesigns', Json.GetValue('actor').Value);
      Assert.AreEqual('flash.start', Json.GetValue('action').Value);
      Assert.AreEqual('audi.mmi3g', Json.GetValue('resource').Value);
      Assert.AreEqual('detail-text', Json.GetValue('detail').Value);
    finally
      Json.Free;
    end;
  finally
    Logger.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// FAILURE ROUTES TO ERROR LEVEL
//------------------------------------------------------------------------------
procedure TAuditTests.FailureRoutesToErrorLevel;
var
  Path: string;
  Logger: TOBDLogger;
  Sink: TCapturingSink;
begin
  Path := ScratchLogPath;
  Logger := TOBDLogger.Create(Path);
  try
    Logger.MinLogLevel := llDebug;
    Sink := TCapturingSink.Create;
    Logger.RegisterSink(Sink);

    TOBDAuditRecorder.Create(Logger).Failure('actor', 'a', 'r');
    Assert.AreEqual(1, Sink.Count);
    Assert.AreEqual(Ord(lsError), Ord(Sink.At(0).Level));
  finally
    Logger.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// DENIED ROUTES TO WARNING LEVEL
//------------------------------------------------------------------------------
procedure TAuditTests.DeniedRoutesToWarningLevel;
var
  Path: string;
  Logger: TOBDLogger;
  Sink: TCapturingSink;
begin
  Path := ScratchLogPath;
  Logger := TOBDLogger.Create(Path);
  try
    Logger.MinLogLevel := llDebug;
    Sink := TCapturingSink.Create;
    Logger.RegisterSink(Sink);
    TOBDAuditRecorder.Create(Logger).Denied('actor', 'a', 'r');
    Assert.AreEqual(Ord(lsWarning), Ord(Sink.At(0).Level));
  finally
    Logger.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// SOURCE TAG SET TO AUDIT
//------------------------------------------------------------------------------
procedure TAuditTests.SourceTagSetToAudit;
var
  Path: string;
  Logger: TOBDLogger;
  Sink: TCapturingSink;
begin
  Path := ScratchLogPath;
  Logger := TOBDLogger.Create(Path);
  try
    Logger.MinLogLevel := llDebug;
    Sink := TCapturingSink.Create;
    Logger.RegisterSink(Sink);
    TOBDAuditRecorder.Create(Logger).Success('actor', 'a', 'r');
    Assert.AreEqual('audit', Sink.At(0).Source);
  finally
    Logger.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// SOURCE TAG RESTORED AFTER RECORD
//------------------------------------------------------------------------------
procedure TAuditTests.SourceTagRestoredAfterRecord;
var
  Path: string;
  Logger: TOBDLogger;
begin
  Path := ScratchLogPath;
  Logger := TOBDLogger.Create(Path);
  try
    Logger.SourceTag := 'connection';
    TOBDAuditRecorder.Create(Logger).Success('a', 'b', 'c');
    Assert.AreEqual('connection', Logger.SourceTag,
      'audit recorder must restore the previous SourceTag');
  finally
    Logger.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAuditTests);

end.
