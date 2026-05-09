//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.Async.pas
// CONTENTS       : Async wrapper over the protocol layer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Issues OBD requests and resolves with parsed
//                  IOBDDataMessage arrays. PollAsync chains requests
//                  sequentially (the OBD bus is single-tester so
//                  parallelisation would corrupt response association).
//------------------------------------------------------------------------------
unit OBD.Protocol.Async;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  OBD.Async, OBD.Connection.Async, OBD.Protocol, OBD.Protocol.Types;

type
  TOBDProtocolAsync = class
  strict private
    FProtocol: IOBDProtocol;
    FConnection: TOBDConnectionAsync;
    function ParseResponse(const RawText: string): TArray<IOBDDataMessage>;
  public
    constructor Create(const AProtocol: IOBDProtocol;
      const AConnection: TOBDConnectionAsync);

    /// <summary>
    ///   Issue a typed Service / PID request and resolve with parsed
    ///   messages. <c>Service</c> is the byte mode (e.g. $01), <c>PID</c>
    ///   is the parameter id.
    /// </summary>
    function RequestAsync(Service, PID: Byte;
      TimeoutMs: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Token: IOBDCancellationToken = nil
      ): IOBDFuture<TArray<IOBDDataMessage>>;

    /// <summary>
    ///   Issue an arbitrary hex command (e.g. <c>'01 0C'</c>).
    /// </summary>
    function RequestRawAsync(const HexCommand: string;
      TimeoutMs: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Token: IOBDCancellationToken = nil
      ): IOBDFuture<TArray<IOBDDataMessage>>;

    /// <summary>
    ///   Sequentially request every PID in <c>PIDs</c> from Service 01
    ///   and resolve with one message-array per PID, in order. Cancelling
    ///   the supplied token aborts mid-batch.
    /// </summary>
    function PollAsync(const PIDs: TArray<Byte>;
      TimeoutMsPerCall: Cardinal = CONN_DEFAULT_TIMEOUT_MS;
      const Token: IOBDCancellationToken = nil
      ): IOBDFuture<TArray<TArray<IOBDDataMessage>>>;

    property Protocol: IOBDProtocol read FProtocol;
    property Connection: TOBDConnectionAsync read FConnection;
  end;

implementation

uses
  System.StrUtils;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDProtocolAsync.Create(const AProtocol: IOBDProtocol;
  const AConnection: TOBDConnectionAsync);
begin
  inherited Create;
  if not Assigned(AProtocol) then
    raise EArgumentNilException.Create('Protocol must not be nil');
  if not Assigned(AConnection) then
    raise EArgumentNilException.Create('Connection must not be nil');
  FProtocol := AProtocol;
  FConnection := AConnection;
end;

//------------------------------------------------------------------------------
// PARSE RESPONSE
//------------------------------------------------------------------------------
function TOBDProtocolAsync.ParseResponse(const RawText: string): TArray<IOBDDataMessage>;
var
  Lines: TStringList;
  S: string;
begin
  Lines := TStringList.Create;
  try
    // Adapter responses use \r as the line separator; fall back to \n if
    // the device terminates with both.
    for S in SplitString(RawText, #13#10) do
      if Trim(S) <> '' then
        Lines.Add(Trim(S));
    Result := FProtocol.Invoke(Lines);
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------
// REQUEST ASYNC
//------------------------------------------------------------------------------
function TOBDProtocolAsync.RequestAsync(Service, PID: Byte;
  TimeoutMs: Cardinal; const Token: IOBDCancellationToken
  ): IOBDFuture<TArray<IOBDDataMessage>>;
begin
  Result := RequestRawAsync(Format('%.2X %.2X', [Service, PID]), TimeoutMs, Token);
end;

//------------------------------------------------------------------------------
// REQUEST RAW ASYNC
//------------------------------------------------------------------------------
function TOBDProtocolAsync.RequestRawAsync(const HexCommand: string;
  TimeoutMs: Cardinal; const Token: IOBDCancellationToken
  ): IOBDFuture<TArray<IOBDDataMessage>>;
var
  Outer: IOBDPromise<TArray<IOBDDataMessage>>;
  Inner: IOBDFuture<string>;
  ParseFn: TFunc<string, TArray<IOBDDataMessage>>;
begin
  Outer := NewPromise<TArray<IOBDDataMessage>>(Token);
  Result := Outer;

  // Snapshot the parser so the closure doesn't accidentally close over
  // Self if it gets freed before the inner future settles.
  ParseFn := function(const RawText: string): TArray<IOBDDataMessage>
    begin
      Result := ParseResponse(RawText);
    end;

  Inner := FConnection.OBDAsync(HexCommand, TimeoutMs, Token);
  Inner.OnComplete(
    procedure(const F: IOBDFuture<string>)
    var
      Raw: string;
    begin
      try
        if F.IsCancelled then
        begin
          Outer.SignalCancelled;
          Exit;
        end;
        Raw := F.Await(0); // already settled — Await returns immediately or throws
        Outer.SetResult(ParseFn(Raw));
      except
        on E: Exception do Outer.SetError(Exception.Create(E.Message));
      end;
    end);
end;

//------------------------------------------------------------------------------
// POLL ASYNC
//------------------------------------------------------------------------------
function TOBDProtocolAsync.PollAsync(const PIDs: TArray<Byte>;
  TimeoutMsPerCall: Cardinal; const Token: IOBDCancellationToken
  ): IOBDFuture<TArray<TArray<IOBDDataMessage>>>;
var
  Outer: IOBDPromise<TArray<TArray<IOBDDataMessage>>>;
  Results: TArray<TArray<IOBDDataMessage>>;
  Index: Integer;
  StartNext: TProc;
begin
  Outer := NewPromise<TArray<TArray<IOBDDataMessage>>>(Token);
  Result := Outer;

  if Length(PIDs) = 0 then
  begin
    Outer.SetResult(nil);
    Exit;
  end;

  SetLength(Results, Length(PIDs));
  Index := 0;

  StartNext :=
    procedure
    var
      Step: IOBDFuture<TArray<IOBDDataMessage>>;
    begin
      if Assigned(Token) and Token.IsCancelled then
      begin
        Outer.SignalCancelled;
        Exit;
      end;
      if Index >= Length(PIDs) then
      begin
        Outer.SetResult(Results);
        Exit;
      end;

      Step := RequestAsync($01, PIDs[Index], TimeoutMsPerCall, Token);
      Step.OnComplete(
        procedure(const F: IOBDFuture<TArray<IOBDDataMessage>>)
        begin
          try
            if F.IsCancelled then
            begin
              Outer.SignalCancelled;
              Exit;
            end;
            Results[Index] := F.Await(0);
            Inc(Index);
            StartNext;
          except
            on E: Exception do Outer.SetError(Exception.Create(E.Message));
          end;
        end);
    end;

  StartNext;
end;

end.
