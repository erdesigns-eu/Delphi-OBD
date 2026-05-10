//------------------------------------------------------------------------------
//  OBD.Coding.SecurityAccess
//
//  TOBDSecurityAccess — non-visual component that drives the
//  ISO 14229-1 § 9.4 SecurityAccess service (SID 0x27).
//
//  Workflow:
//    1. Tester sends "27 SUBFUNC" requestSeed.
//    2. ECU returns "67 SUBFUNC <seed bytes>".
//    3. Tester computes key = f(seed) using the OEM secret.
//    4. Tester sends "27 SUBFUNC+1 <key bytes>" sendKey.
//    5. ECU returns "67 SUBFUNC+1" on success or NRC on failure.
//
//  The seed → key transform is OEM- and level-specific. The
//  component does not ship one; the host either:
//    - registers a callback via <c>SeedToKey</c> (TFunc), or
//    - subscribes to <c>OnComputeKey</c> for a procedural API.
//
//  Sync + Async + Progress per the dual-method rule.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 14229-1:2020 § 9.4 (SecurityAccess)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.SecurityAccess;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

type
  /// <summary>Procedural seed → key transform.</summary>
  TOBDSeedToKeyEvent = procedure(Sender: TObject;
    ALevel: Byte; const ASeed: TBytes; var AKey: TBytes) of object;

  /// <summary>Functional seed → key transform.</summary>
  TOBDSeedToKeyFunc = reference to function(ALevel: Byte;
    const ASeed: TBytes): TBytes;

  /// <summary>
  ///   ISO 14229-1 SecurityAccess (0x27) component.
  /// </summary>
  TOBDSecurityAccess = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FRequestSeedLevel: Byte;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FSeedToKey: TOBDSeedToKeyFunc;
    FOnComputeKey: TOBDSeedToKeyEvent;
    FOnUnlocked: TNotifyEvent;
    FOnError: TOBDConnectionErrorEvent;
    FOnProgress: TOBDProgressEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoUnlock(ALevel: Byte): Boolean;
    function ComputeKey(ALevel: Byte; const ASeed: TBytes): TBytes;
    procedure FireUnlocked;
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure FireProgress(AIndex, ACount: Cardinal;
      const AName, ADetail: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Runs the SecurityAccess sequence at <c>ALevel</c>
    ///   (default <c>0x01</c>). Synchronous.
    /// </summary>
    /// <param name="ALevel">requestSeed sub-function — must be
    /// odd (0x01, 0x03, 0x05, …); the sendKey sub-function is
    /// computed automatically as <c>ALevel + 1</c>.</param>
    /// <returns>True when the ECU acknowledges the key; raises
    /// on NRC or transport failure.</returns>
    /// <exception cref="EOBDConfig">Even <c>ALevel</c>, or no
    /// seed→key transform configured.</exception>
    /// <exception cref="EOBDProtocolErr">ECU returned NRC.</exception>
    function Unlock(ALevel: Byte = $01): Boolean;
    /// <summary>Non-blocking <see cref="Unlock"/>.</summary>
    procedure UnlockAsync(ALevel: Byte = $01);

    /// <summary>Functional seed → key transform. Set this OR
    /// <see cref="OnComputeKey"/>; the functional form takes
    /// precedence when both are assigned.</summary>
    property SeedToKey: TOBDSeedToKeyFunc read FSeedToKey write FSeedToKey;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Default requestSeed sub-function for design-time
    /// configuration. Overridden by the parameter to <c>Unlock</c>.</summary>
    property RequestSeedLevel: Byte read FRequestSeedLevel
      write FRequestSeedLevel default $01;

    /// <summary>Procedural seed → key transform. Fires on the
    /// thread that called <c>Unlock</c> / <c>UnlockAsync</c>; the
    /// handler must be thread-safe.</summary>
    property OnComputeKey: TOBDSeedToKeyEvent read FOnComputeKey
      write FOnComputeKey;
    /// <summary>Fires on the main thread when the ECU
    /// acknowledges the key.</summary>
    property OnUnlocked: TNotifyEvent read FOnUnlocked write FOnUnlocked;
    /// <summary>Fires on transient I/O errors (main thread).</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
    /// <summary>Fires per phase (request seed, compute key, send
    /// key) on the main thread.</summary>
    property OnProgress: TOBDProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  System.Math;

function AllZero(const ABytes: TBytes): Boolean;
var I: Integer;
begin
  for I := 0 to High(ABytes) do
    if ABytes[I] <> 0 then Exit(False);
  Result := True;
end;

constructor TOBDSecurityAccess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FRequestSeedLevel := $01;
end;

destructor TOBDSecurityAccess.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDSecurityAccess.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDSecurityAccess.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDSecurityAccess.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDSecurityAccess: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDSecurityAccess.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDSecurityAccess.ComputeKey(ALevel: Byte;
  const ASeed: TBytes): TBytes;
begin
  if Assigned(FSeedToKey) then
    Result := FSeedToKey(ALevel, ASeed)
  else if Assigned(FOnComputeKey) then
  begin
    SetLength(Result, 0);
    FOnComputeKey(Self, ALevel, ASeed, Result);
  end
  else
    raise EOBDConfig.Create(
      'TOBDSecurityAccess: SeedToKey / OnComputeKey not configured');
  if Length(Result) = 0 then
    raise EOBDConfig.Create(
      'TOBDSecurityAccess: seed → key transform returned no bytes');
end;

function TOBDSecurityAccess.DoUnlock(ALevel: Byte): Boolean;
var
  Resp: TOBDResponse;
  Seed, Key: TBytes;
  Off: Integer;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDSecurityAccess: Protocol not assigned');
  if (ALevel and $01) = 0 then
    raise EOBDConfig.CreateFmt(
      'TOBDSecurityAccess: requestSeed sub-function 0x%2.2X must be odd',
      [ALevel]);

  // 1. requestSeed
  FireProgress(1, 3, 'Security access', 'requestSeed');
  Resp := FProtocol.Request(UDS_SID_SecurityAccess, TBytes.Create(ALevel));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'SecurityAccess requestSeed (0x%2.2X) negative: %s',
      [ALevel, Resp.NRCText]);
  // Response: SUBFUNC echo + seed bytes.
  if (Length(Resp.Data) >= 1) and (Resp.Data[0] = ALevel) then Off := 1
  else Off := 0;
  SetLength(Seed, Length(Resp.Data) - Off);
  if Length(Seed) > 0 then
    Move(Resp.Data[Off], Seed[0], Length(Seed));

  // ISO 14229-1 §9.4.5.2: ECU reports "already unlocked" via either
  // a zero-byte seed OR an all-zero seed (vendor-dependent). Either
  // pattern short-circuits the sendKey step.
  if (Length(Seed) = 0) or AllZero(Seed) then
  begin
    FireProgress(3, 3, 'Security access', 'already unlocked');
    Result := True;
    FireUnlocked;
    Exit;
  end;

  // 2. compute key
  FireProgress(2, 3, 'Security access', 'computing key');
  Key := ComputeKey(ALevel, Seed);

  // 3. sendKey
  FireProgress(3, 3, 'Security access', 'sendKey');
  SetLength(Resp.Data, 0);
  var Body: TBytes;
  SetLength(Body, 1 + Length(Key));
  Body[0] := ALevel + 1;
  Move(Key[0], Body[1], Length(Key));
  Resp := FProtocol.Request(UDS_SID_SecurityAccess, Body);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'SecurityAccess sendKey (0x%2.2X) negative: %s',
      [ALevel + 1, Resp.NRCText]);

  Result := True;
  FireUnlocked;
end;

function TOBDSecurityAccess.Unlock(ALevel: Byte): Boolean;
begin
  Result := DoUnlock(ALevel);
end;

procedure TOBDSecurityAccess.UnlockAsync(ALevel: Byte);
var
  Self_: TOBDSecurityAccess;
  Level: Byte;
begin
  GuardSingleAsync;
  Self_ := Self; Level := ALevel;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.DoUnlock(Level);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDSecurityAccess.FireUnlocked;
var
  Self_: TOBDSecurityAccess;
begin
  if not Assigned(FOnUnlocked) then Exit;
  Self_ := Self;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnUnlocked(Self_)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnUnlocked) then Self_.FOnUnlocked(Self_);
    end);
end;

procedure TOBDSecurityAccess.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDSecurityAccess; Code: TOBDErrorCode; Msg: string;
begin
  if not Assigned(FOnError) then Exit;
  Self_ := Self; Code := ACode; Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil, procedure
      var Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

procedure TOBDSecurityAccess.FireProgress(AIndex, ACount: Cardinal;
  const AName, ADetail: string);
var
  Self_: TOBDSecurityAccess;
  Step: TOBDProgressStep;
begin
  if not Assigned(FOnProgress) then Exit;
  Self_ := Self;
  Step := TOBDProgressStep.MakeStep(AIndex, ACount, AName, ADetail);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnProgress(Self_, Step)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnProgress) then Self_.FOnProgress(Self_, Step);
    end);
end;

end.
