//------------------------------------------------------------------------------
//  OBD.OEM.SecurityAccess
//
//  Drives the full UDS Service 0x27 SecurityAccess exchange so a
//  caller only has to register the seed-key algorithm for their
//  level. The client handles:
//
//    1. Sending the seed-request (<c>27 LL</c>) and parsing the
//       <c>67 LL seed-bytes</c> response.
//    2. Resolving the matching <see cref="IOBDSeedKeyAlgorithm"/>
//       from a <see cref="TOBDSeedKeyRegistry"/> and computing
//       the key.
//    3. Sending the key (<c>27 (LL+1) key-bytes</c>) and parsing
//       the <c>67 (LL+1)</c> reply.
//    4. Honouring the ISO 14229 §10 negative-response rules:
//         - <c>0x35 invalidKey</c>      → surface as failure.
//         - <c>0x36 exceededNumberOfAttempts</c> → ECU has locked
//           out; the client refuses further attempts (the lockout
//           is the ECU's, not ours).
//         - <c>0x37 requiredTimeDelayNotExpired</c> → wait the
//           caller-configured delay then retry, up to a configured
//           max-retry budget.
//
//  Algorithms stay user-supplied. Production callers register
//  their OEM implementations through
//  <see cref="TOBDSeedKeyRegistry.RegisterAlgorithm"/>; the client
//  only sequences the protocol exchange around them.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1 §10 SecurityAccess
//    - ISO 14229-1 Annex A Negative Response Code Definitions
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.SecurityAccess;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Protocol,
  OBD.Protocol.Types,
  OBD.OEM.SeedKey;

const
  /// <summary>NRC 0x35 — invalidKey.</summary>
  NRC_INVALID_KEY                  = $35;
  /// <summary>NRC 0x36 — exceededNumberOfAttempts.</summary>
  NRC_EXCEEDED_ATTEMPTS             = $36;
  /// <summary>NRC 0x37 — requiredTimeDelayNotExpired.</summary>
  NRC_REQUIRED_TIME_DELAY_NOT_EXPIRED = $37;

type
  /// <summary>Outcome of one SecurityAccess attempt.</summary>
  TOBDSecurityAccessOutcome = (
    /// <summary>Not yet attempted.</summary>
    saoUnknown,
    /// <summary>Key accepted; the ECU is unlocked.</summary>
    saoUnlocked,
    /// <summary>Already unlocked at this level — the ECU returned
    /// a zero-length seed.</summary>
    saoAlreadyUnlocked,
    /// <summary>Key was rejected
    /// (<c>NRC 0x35 invalidKey</c>).</summary>
    saoInvalidKey,
    /// <summary>ECU is locked out due to too many failed
    /// attempts (<c>NRC 0x36</c>); the lockout is the ECU's, not
    /// ours, and is usually reset by an ignition cycle.</summary>
    saoEcuLockedOut,
    /// <summary>Required time delay never expired during the
    /// retry budget (<c>NRC 0x37</c>).</summary>
    saoDelayNotExpired,
    /// <summary>No algorithm is registered for the requested
    /// level.</summary>
    saoAlgorithmMissing,
    /// <summary>Transport failure / malformed response /
    /// timeout.</summary>
    saoTransportError);

  /// <summary>Detailed result returned by
  /// <see cref="TOBDSecurityAccessClient.Unlock"/>.</summary>
  TOBDSecurityAccessResult = record
    /// <summary>Coarse outcome.</summary>
    Outcome: TOBDSecurityAccessOutcome;
    /// <summary>Access level the client tried.</summary>
    Level: Byte;
    /// <summary>Seed bytes returned by the ECU
    /// (empty when no seed was read).</summary>
    Seed: TBytes;
    /// <summary>Key bytes the client computed and sent
    /// (empty when no key was sent).</summary>
    Key: TBytes;
    /// <summary>NRC reported by the ECU; 0 when no NRC.</summary>
    NRC: Byte;
    /// <summary>Human-readable detail. Empty on success.</summary>
    Message: string;
    /// <summary>Number of seed/key attempts the client made
    /// (counts only the round-trips it issued; not the ECU's
    /// internal attempt counter).</summary>
    Attempts: Integer;
  end;

  /// <summary>Raised on configuration errors that prevent the
  /// client from running. NRCs and transport failures are
  /// reported via <see cref="TOBDSecurityAccessResult.Outcome"/>
  /// instead.</summary>
  EOBDSecurityAccess = class(Exception);

  /// <summary>
  ///   Synchronous SecurityAccess client. Thread-safe — multiple
  ///   instances bound to the same protocol or registry are
  ///   independent.
  /// </summary>
  TOBDSecurityAccessClient = class
  strict private
    FProtocol: TOBDProtocol;
    FRegistry: TOBDSeedKeyRegistry;
    FRequestTimeoutMs: Cardinal;
    FRetryDelayMs: Cardinal;
    FMaxDelayRetries: Integer;
  public
    /// <summary>Constructs the client.</summary>
    /// <param name="AProtocol">Bound protocol; must have an
    /// active adapter.</param>
    /// <param name="ARegistry">Algorithm registry. The same
    /// registry can be shared across levels.</param>
    /// <exception cref="EOBDSecurityAccess"><c>AProtocol</c> or
    /// <c>ARegistry</c> is nil.</exception>
    constructor Create(AProtocol: TOBDProtocol;
      ARegistry: TOBDSeedKeyRegistry);
    /// <summary>UDS request timeout for the seed-request and
    /// key-send round trips. Defaults to 2000 ms.</summary>
    property RequestTimeoutMs: Cardinal
      read FRequestTimeoutMs write FRequestTimeoutMs;
    /// <summary>Delay between retries after
    /// <c>NRC 0x37 requiredTimeDelayNotExpired</c>. Defaults to
    /// 1000 ms.</summary>
    property RetryDelayMs: Cardinal
      read FRetryDelayMs write FRetryDelayMs;
    /// <summary>Maximum number of times the client retries on
    /// <c>NRC 0x37</c> before giving up. Defaults to 5
    /// (~5 seconds at the default <see cref="RetryDelayMs"/>).
    /// Set to 0 to disable retries entirely.</summary>
    property MaxDelayRetries: Integer
      read FMaxDelayRetries write FMaxDelayRetries;
    /// <summary>
    ///   Runs the full seed → compute → send-key dance for
    ///   <c>ALevel</c>. <c>ALevel</c> must be odd (the request-
    ///   seed sub-function). The send-key sub-function (<c>ALevel
    ///   + 1</c>) is computed automatically.
    /// </summary>
    /// <param name="ALevel">Request-seed level
    /// (<c>0x01</c>, <c>0x03</c>, …).</param>
    /// <returns>Detailed outcome.</returns>
    /// <exception cref="EOBDSecurityAccess">
    ///   <c>ALevel</c> is even (request-seed levels must be odd).
    /// </exception>
    function Unlock(ALevel: Byte): TOBDSecurityAccessResult;
  end;

implementation

const
  DEFAULT_TIMEOUT_MS       = 2000;
  DEFAULT_RETRY_DELAY_MS   = 1000;
  DEFAULT_MAX_DELAY_RETRIES = 5;

constructor TOBDSecurityAccessClient.Create(AProtocol: TOBDProtocol;
  ARegistry: TOBDSeedKeyRegistry);
begin
  inherited Create;
  if AProtocol = nil then
    raise EOBDSecurityAccess.Create(
      'TOBDSecurityAccessClient requires a non-nil TOBDProtocol');
  if ARegistry = nil then
    raise EOBDSecurityAccess.Create(
      'TOBDSecurityAccessClient requires a non-nil ' +
      'TOBDSeedKeyRegistry');
  FProtocol := AProtocol;
  FRegistry := ARegistry;
  FRequestTimeoutMs := DEFAULT_TIMEOUT_MS;
  FRetryDelayMs := DEFAULT_RETRY_DELAY_MS;
  FMaxDelayRetries := DEFAULT_MAX_DELAY_RETRIES;
end;

function TOBDSecurityAccessClient.Unlock(
  ALevel: Byte): TOBDSecurityAccessResult;
var
  Algo: IOBDSeedKeyAlgorithm;
  SeedResp, KeyResp: TOBDResponse;
  Retries: Integer;
  Stop: TEvent;
begin
  Result := Default(TOBDSecurityAccessResult);
  Result.Level := ALevel;
  Result.Outcome := saoUnknown;

  if (ALevel and 1) = 0 then
    raise EOBDSecurityAccess.CreateFmt(
      'Request-seed levels must be odd (got 0x%.2X)', [ALevel]);

  Algo := FRegistry.Find(ALevel);
  if Algo = nil then
  begin
    Result.Outcome := saoAlgorithmMissing;
    Result.Message := Format(
      'No seed-key algorithm registered for level 0x%.2X', [ALevel]);
    Exit;
  end;

  // Wrap the retry loop around the seed-request — NRC 0x37 from
  // the ECU's request-seed handler is the documented "wait and
  // retry" path. Some ECUs also issue 0x37 from the send-key
  // handler when a previous failed attempt is still cooling
  // down; the same loop handles both round trips.
  Retries := 0;
  Stop := TEvent.Create(nil, True, False, '');
  try
    while True do
    begin
      Inc(Result.Attempts);

      // -- Seed request -------------------------------------------------
      try
        SeedResp := FProtocol.Request($27, TBytes.Create(ALevel),
          FRequestTimeoutMs);
      except
        on E: Exception do
        begin
          Result.Outcome := saoTransportError;
          Result.Message := 'Seed request: ' + E.ClassName +
            ': ' + E.Message;
          Exit;
        end;
      end;

      if SeedResp.IsNegative then
      begin
        Result.NRC := SeedResp.NRC;
        case SeedResp.NRC of
          NRC_REQUIRED_TIME_DELAY_NOT_EXPIRED:
            begin
              Inc(Retries);
              if Retries > FMaxDelayRetries then
              begin
                Result.Outcome := saoDelayNotExpired;
                Result.Message :=
                  'NRC 0x37 requiredTimeDelayNotExpired ' +
                  'persisted past the retry budget';
                Exit;
              end;
              // Sleep on a TEvent so a caller-driven cancel can
              // break the wait by signalling it (future hook).
              Stop.WaitFor(FRetryDelayMs);
              Continue;
            end;
          NRC_EXCEEDED_ATTEMPTS:
            begin
              Result.Outcome := saoEcuLockedOut;
              Result.Message :=
                'NRC 0x36 exceededNumberOfAttempts — ECU is ' +
                'locked out; usually resets on ignition cycle';
              Exit;
            end;
        else
          Result.Outcome := saoTransportError;
          Result.Message := Format(
            'Seed request rejected: NRC 0x%.2X %s',
            [SeedResp.NRC, SeedResp.NRCText]);
          Exit;
        end;
      end;

      Result.Seed := SeedResp.Data;

      // Zero-length seed = the ECU is already unlocked at this
      // level (ISO 14229 §10.5.2 note). No key send needed.
      if Length(Result.Seed) = 0 then
      begin
        Result.Outcome := saoAlreadyUnlocked;
        Exit;
      end;

      // -- Key computation + send ---------------------------------------
      try
        Result.Key := Algo.ComputeKey(Result.Seed, ALevel);
      except
        on E: Exception do
        begin
          Result.Outcome := saoTransportError;
          Result.Message :=
            'Algorithm raised while computing key: ' +
            E.ClassName + ': ' + E.Message;
          Exit;
        end;
      end;

      try
        KeyResp := FProtocol.Request($27,
          TBytes.Create(ALevel + 1) + Result.Key,
          FRequestTimeoutMs);
      except
        on E: Exception do
        begin
          Result.Outcome := saoTransportError;
          Result.Message := 'Send key: ' + E.ClassName +
            ': ' + E.Message;
          Exit;
        end;
      end;

      if KeyResp.IsNegative then
      begin
        Result.NRC := KeyResp.NRC;
        case KeyResp.NRC of
          NRC_INVALID_KEY:
            begin
              Result.Outcome := saoInvalidKey;
              Result.Message :=
                'NRC 0x35 invalidKey — algorithm produced the ' +
                'wrong key for the registered level';
              Exit;
            end;
          NRC_EXCEEDED_ATTEMPTS:
            begin
              Result.Outcome := saoEcuLockedOut;
              Result.Message :=
                'NRC 0x36 exceededNumberOfAttempts after key ' +
                'send — ECU is locked out';
              Exit;
            end;
          NRC_REQUIRED_TIME_DELAY_NOT_EXPIRED:
            begin
              // Some ECUs throttle after a previous failure
              // even on the send-key half; retry the whole
              // exchange (seed reads after 0x37 typically
              // return a fresh seed once the delay elapses).
              Inc(Retries);
              if Retries > FMaxDelayRetries then
              begin
                Result.Outcome := saoDelayNotExpired;
                Result.Message :=
                  'NRC 0x37 on key send persisted past the ' +
                  'retry budget';
                Exit;
              end;
              Stop.WaitFor(FRetryDelayMs);
              Continue;
            end;
        else
          Result.Outcome := saoTransportError;
          Result.Message := Format(
            'Send key rejected: NRC 0x%.2X %s',
            [KeyResp.NRC, KeyResp.NRCText]);
          Exit;
        end;
      end;

      Result.Outcome := saoUnlocked;
      Exit;
    end;
  finally
    Stop.Free;
  end;
end;

end.
