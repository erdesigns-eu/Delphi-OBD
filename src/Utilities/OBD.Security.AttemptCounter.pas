//------------------------------------------------------------------------------
//  OBD.Security.AttemptCounter
//
//  TOBDAttemptCounter — sliding-window failed-attempt tracker.
//  Useful for security-access loops (UDS 0x27) that lock out
//  after N bad seed-key answers, or for any host-side rate-
//  limited workflow.
//
//  Each call to <see cref="RegisterFailure"/> records a failure
//  with the current timestamp. The counter rolls older entries
//  out of the configured <see cref="WindowSeconds"/> window so
//  the count reflects only recent failures. A successful
//  attempt resets the counter to zero.
//
//  Reentrant; safe to call from any thread.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Security.AttemptCounter.
//------------------------------------------------------------------------------

unit OBD.Security.AttemptCounter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections;

type
  /// <summary>
  ///   Sliding-window failed-attempt counter.
  /// </summary>
  /// <remarks>
  ///   Construct with the maximum failure count and the window
  ///   length in seconds. Call <see cref="RegisterFailure"/> on
  ///   every bad attempt, <see cref="RegisterSuccess"/> on a
  ///   good one, and <see cref="IsLockedOut"/> before allowing
  ///   the next attempt.
  /// </remarks>
  TOBDAttemptCounter = class
  strict private
    FLock: TCriticalSection;
    FFailures: TList<TDateTime>;
    FMaxFailures: Integer;
    FWindowSeconds: Integer;
    procedure PruneOlderThan(ANow: TDateTime);
  public
    /// <summary>Constructs the counter.</summary>
    /// <param name="AMaxFailures">Failure count that triggers
    /// the lockout (default 5).</param>
    /// <param name="AWindowSeconds">Sliding-window length
    /// (default 300 — five minutes).</param>
    constructor Create(AMaxFailures: Integer = 5;
      AWindowSeconds: Integer = 300);
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>Records a failed attempt with the current
    /// timestamp.</summary>
    procedure RegisterFailure;
    /// <summary>Clears the failure list.</summary>
    procedure RegisterSuccess;

    /// <summary>Returns the failure count inside the current
    /// window.</summary>
    function FailureCount: Integer;
    /// <summary>Returns <c>True</c> when the failure count has
    /// reached <see cref="MaxFailures"/>.</summary>
    function IsLockedOut: Boolean;
    /// <summary>Seconds until the oldest in-window failure
    /// rolls out (0 when not locked out).</summary>
    function SecondsUntilUnlock: Integer;

    /// <summary>Read-only maximum-failures threshold.</summary>
    property MaxFailures: Integer read FMaxFailures;
    /// <summary>Read-only window length in seconds.</summary>
    property WindowSeconds: Integer read FWindowSeconds;
  end;

implementation

constructor TOBDAttemptCounter.Create(AMaxFailures: Integer;
  AWindowSeconds: Integer);
begin
  inherited Create;
  if AMaxFailures < 1 then
    AMaxFailures := 1;
  if AWindowSeconds < 1 then
    AWindowSeconds := 1;
  FMaxFailures := AMaxFailures;
  FWindowSeconds := AWindowSeconds;
  FLock := TCriticalSection.Create;
  FFailures := TList<TDateTime>.Create;
end;

destructor TOBDAttemptCounter.Destroy;
begin
  FFailures.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDAttemptCounter.PruneOlderThan(ANow: TDateTime);
var
  Cutoff: TDateTime;
begin
  Cutoff := IncSecond(ANow, -FWindowSeconds);
  while (FFailures.Count > 0) and (FFailures[0] < Cutoff) do
    FFailures.Delete(0);
end;

procedure TOBDAttemptCounter.RegisterFailure;
var
  N: TDateTime;
begin
  N := Now;
  FLock.Enter;
  try
    PruneOlderThan(N);
    FFailures.Add(N);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDAttemptCounter.RegisterSuccess;
begin
  FLock.Enter;
  try
    FFailures.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDAttemptCounter.FailureCount: Integer;
begin
  FLock.Enter;
  try
    PruneOlderThan(Now);
    Result := FFailures.Count;
  finally
    FLock.Leave;
  end;
end;

function TOBDAttemptCounter.IsLockedOut: Boolean;
begin
  Result := FailureCount >= FMaxFailures;
end;

function TOBDAttemptCounter.SecondsUntilUnlock: Integer;
var
  N: TDateTime;
  Oldest: TDateTime;
  Elapsed: Integer;
begin
  FLock.Enter;
  try
    N := Now;
    PruneOlderThan(N);
    if FFailures.Count < FMaxFailures then
      Exit(0);
    Oldest := FFailures[0];
    Elapsed := SecondsBetween(N, Oldest);
    if Elapsed >= FWindowSeconds then
      Exit(0);
    Result := FWindowSeconds - Elapsed;
  finally
    FLock.Leave;
  end;
end;

end.
