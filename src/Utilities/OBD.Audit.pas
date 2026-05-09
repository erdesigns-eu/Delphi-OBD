//------------------------------------------------------------------------------
// UNIT           : OBD.Audit.pas
// CONTENTS       : Audit-log helper sitting on the IOBDLogSink stack
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Audit events go through the regular logger but
//                  with a "audit" source tag and a structured payload
//                  (actor, action, resource, outcome). When paired with
//                  TJsonLineSink the resulting file is ready for SIEM
//                  ingestion.
//------------------------------------------------------------------------------
unit OBD.Audit;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  OBD.Logger, OBD.Logger.Sinks;

type
  TOBDAuditOutcome = (aoSuccess, aoFailure, aoDenied);

  TOBDAuditEvent = record
    Actor: string;       // who triggered it ("user:erdesigns", "service:flash-bot")
    Action: string;      // what they did ("read.dealercode", "flash.start")
    Resource: string;    // the target ("vw.t5.gateway", "audi.mmi3g")
    Outcome: TOBDAuditOutcome;
    Detail: string;      // optional free-form
  end;

  /// <summary>
  ///   Audit recorder. Construct once with the logger you want events to
  ///   land in; call <c>Record</c> for each notable security-relevant
  ///   action.
  /// </summary>
  TOBDAuditRecorder = class
  strict private
    FLogger: TOBDLogger;
    function OutcomeName(O: TOBDAuditOutcome): string;
    function Serialize(const Event: TOBDAuditEvent): string;
  public
    constructor Create(ALogger: TOBDLogger);

    /// <summary>
    ///   Record an audit event at the appropriate log level.
    /// </summary>
    procedure RecordEvent(const Event: TOBDAuditEvent);

    /// <summary>
    ///   Convenience: record a success.
    /// </summary>
    procedure Success(const Actor, Action, Resource: string;
      const Detail: string = '');
    /// <summary>
    ///   Convenience: record a failure.
    /// </summary>
    procedure Failure(const Actor, Action, Resource: string;
      const Detail: string = '');
    /// <summary>
    ///   Convenience: record a denied (policy violation).
    /// </summary>
    procedure Denied(const Actor, Action, Resource: string;
      const Detail: string = '');
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDAuditRecorder.Create(ALogger: TOBDLogger);
begin
  inherited Create;
  if not Assigned(ALogger) then
    raise EArgumentNilException.Create('Audit recorder requires a TOBDLogger');
  FLogger := ALogger;
end;

//------------------------------------------------------------------------------
// OUTCOME NAME
//------------------------------------------------------------------------------
function TOBDAuditRecorder.OutcomeName(O: TOBDAuditOutcome): string;
begin
  case O of
    aoSuccess: Result := 'success';
    aoFailure: Result := 'failure';
    aoDenied:  Result := 'denied';
  else         Result := 'unknown';
  end;
end;

//------------------------------------------------------------------------------
// SERIALIZE
//------------------------------------------------------------------------------
function TOBDAuditRecorder.Serialize(const Event: TOBDAuditEvent): string;
var
  Obj: TJSONObject;
begin
  // Single-line JSON keeps the audit trail grep-able and ready for
  // any JSON-aware ingester downstream of TJsonLineSink.
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('actor',    Event.Actor);
    Obj.AddPair('action',   Event.Action);
    Obj.AddPair('resource', Event.Resource);
    Obj.AddPair('outcome',  OutcomeName(Event.Outcome));
    if Event.Detail <> '' then Obj.AddPair('detail', Event.Detail);
    Result := Obj.ToJSON;
  finally
    Obj.Free;
  end;
end;

//------------------------------------------------------------------------------
// RECORD EVENT
//------------------------------------------------------------------------------
procedure TOBDAuditRecorder.RecordEvent(const Event: TOBDAuditEvent);
var
  PreviousTag: string;
  Level: TLogLevel;
begin
  // Audit events route through the configured logger so a single
  // file/json/console sink fan-out covers both ordinary logs and audits.
  PreviousTag := FLogger.SourceTag;
  FLogger.SourceTag := 'audit';
  try
    case Event.Outcome of
      aoSuccess: Level := llInfo;
      aoFailure: Level := llError;
      aoDenied:  Level := llWarning;
    else         Level := llInfo;
    end;
    FLogger.Log(Level, Serialize(Event));
  finally
    FLogger.SourceTag := PreviousTag;
  end;
end;

//------------------------------------------------------------------------------
// SUCCESS
//------------------------------------------------------------------------------
procedure TOBDAuditRecorder.Success(const Actor, Action, Resource, Detail: string);
var
  E: TOBDAuditEvent;
begin
  E.Actor := Actor; E.Action := Action; E.Resource := Resource;
  E.Outcome := aoSuccess; E.Detail := Detail;
  RecordEvent(E);
end;

//------------------------------------------------------------------------------
// FAILURE
//------------------------------------------------------------------------------
procedure TOBDAuditRecorder.Failure(const Actor, Action, Resource, Detail: string);
var
  E: TOBDAuditEvent;
begin
  E.Actor := Actor; E.Action := Action; E.Resource := Resource;
  E.Outcome := aoFailure; E.Detail := Detail;
  RecordEvent(E);
end;

//------------------------------------------------------------------------------
// DENIED
//------------------------------------------------------------------------------
procedure TOBDAuditRecorder.Denied(const Actor, Action, Resource, Detail: string);
var
  E: TOBDAuditEvent;
begin
  E.Actor := Actor; E.Action := Action; E.Resource := Resource;
  E.Outcome := aoDenied; E.Detail := Detail;
  RecordEvent(E);
end;

end.
