//------------------------------------------------------------------------------
// UNIT           : OBD.Security.AttemptCounter.pas
// CONTENTS       : Brute-force / anti-replay primitive
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tracks failed-attempt counts per identity (radio
//                  serial, ECU id, …) and applies an exponential
//                  back-off lockout so brute-forcing a 4-digit code
//                  from a stolen radio takes geological time. Pairs
//                  with TOBDSecureSettings to persist counts across
//                  process restarts.
//------------------------------------------------------------------------------
unit OBD.Security.AttemptCounter;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.DateUtils,
  System.Generics.Collections;

type
  TOBDAttemptState = record
    Failures: Integer;
    LockedUntil: TDateTime;
  end;

  TOBDAttemptCounter = class
  strict private
    FLock: TCriticalSection;
    FStates: TDictionary<string, TOBDAttemptState>;
    FBaseLockoutSeconds: Integer;
    FMaxLockoutSeconds: Integer;
    FFreeAttempts: Integer;
    function ComputeLockout(Failures: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Returns True if a new attempt is allowed for <c>Identity</c>.
    ///   Sets <c>WaitSeconds</c> to the lockout remainder when False.
    /// </summary>
    function IsAllowed(const Identity: string; out WaitSeconds: Integer): Boolean;

    /// <summary>
    ///   Record a failed attempt — applies exponential back-off.
    /// </summary>
    procedure RegisterFailure(const Identity: string);
    /// <summary>
    ///   Record a success — resets the counter.
    /// </summary>
    procedure RegisterSuccess(const Identity: string);
    /// <summary>
    ///   Reset the counter without recording a success.
    /// </summary>
    procedure Reset(const Identity: string);

    function State(const Identity: string): TOBDAttemptState;

  published
    /// <summary>
    ///   Number of failed attempts allowed before the back-off kicks in.
    /// </summary>
    property FreeAttempts: Integer read FFreeAttempts write FFreeAttempts;
    /// <summary>
    ///   Initial lockout in seconds (doubles per subsequent failure).
    /// </summary>
    property BaseLockoutSeconds: Integer read FBaseLockoutSeconds write FBaseLockoutSeconds;
    /// <summary>
    ///   Cap on the lockout (default 1 day).
    /// </summary>
    property MaxLockoutSeconds: Integer read FMaxLockoutSeconds write FMaxLockoutSeconds;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDAttemptCounter.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FStates := TDictionary<string, TOBDAttemptState>.Create;
  FFreeAttempts := 3;
  FBaseLockoutSeconds := 30;
  FMaxLockoutSeconds := 86400;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDAttemptCounter.Destroy;
begin
  FStates.Free;
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// COMPUTE LOCKOUT
//------------------------------------------------------------------------------
function TOBDAttemptCounter.ComputeLockout(Failures: Integer): Integer;
var
  Steps, Lockout: Int64;
begin
  // Doubles per failure beyond FreeAttempts. Capped at MaxLockoutSeconds
  // so the back-off plateaus rather than overflowing.
  Steps := Failures - FFreeAttempts;
  if Steps <= 0 then Exit(0);
  Lockout := Int64(FBaseLockoutSeconds) shl (Steps - 1);
  if Lockout > FMaxLockoutSeconds then Lockout := FMaxLockoutSeconds;
  if Lockout < 0 then Lockout := FMaxLockoutSeconds; // shl overflow
  Result := Lockout;
end;

//------------------------------------------------------------------------------
// IS ALLOWED
//------------------------------------------------------------------------------
function TOBDAttemptCounter.IsAllowed(const Identity: string;
  out WaitSeconds: Integer): Boolean;
var
  S: TOBDAttemptState;
  Now_: TDateTime;
begin
  WaitSeconds := 0;
  FLock.Enter;
  try
    if not FStates.TryGetValue(Identity, S) then Exit(True);
    Now_ := Now;
    if S.LockedUntil <= Now_ then Exit(True);
    WaitSeconds := SecondsBetween(Now_, S.LockedUntil);
    Result := False;
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// REGISTER FAILURE
//------------------------------------------------------------------------------
procedure TOBDAttemptCounter.RegisterFailure(const Identity: string);
var
  S: TOBDAttemptState;
  Lockout: Integer;
begin
  FLock.Enter;
  try
    if not FStates.TryGetValue(Identity, S) then
    begin
      S.Failures := 0;
      S.LockedUntil := 0;
    end;
    Inc(S.Failures);
    Lockout := ComputeLockout(S.Failures);
    if Lockout > 0 then
      S.LockedUntil := IncSecond(Now, Lockout)
    else
      S.LockedUntil := 0;
    FStates.AddOrSetValue(Identity, S);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// REGISTER SUCCESS
//------------------------------------------------------------------------------
procedure TOBDAttemptCounter.RegisterSuccess(const Identity: string);
begin
  Reset(Identity);
end;

//------------------------------------------------------------------------------
// RESET
//------------------------------------------------------------------------------
procedure TOBDAttemptCounter.Reset(const Identity: string);
begin
  FLock.Enter;
  try FStates.Remove(Identity); finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// STATE
//------------------------------------------------------------------------------
function TOBDAttemptCounter.State(const Identity: string): TOBDAttemptState;
begin
  FLock.Enter;
  try
    if not FStates.TryGetValue(Identity, Result) then
    begin
      Result.Failures := 0;
      Result.LockedUntil := 0;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
