//------------------------------------------------------------------------------
//  OBD.Connection.Retry
//
//  TOBDRetryPolicy — TPersistent sub-object on TOBDConnection that
//  controls automatic retry of an Open() call when the underlying
//  transport fails transiently (target busy, network glitch, dialled
//  Bluetooth not yet ready).
//
//  Implements exponential backoff with optional jitter:
//     delay(n) = min(MaxDelay, InitialDelay * (Multiplier ^ n))
//
//  Adapter / detection retry (the AT-level handshake) is a separate
//  concern and lives on TOBDAdapter (Phase 3).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation: TPersistent policy + delay
//                     calculator + tests-friendly seedable jitter.
//------------------------------------------------------------------------------

unit OBD.Connection.Retry;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  ///   Auto-reopen policy for a transport.
  /// </summary>
  /// <remarks>
  ///   Defaults: 3 attempts, 200 ms initial delay, ×2 multiplier capped
  ///   at 5 s, 10 % jitter. Set <c>MaxAttempts := 0</c> to disable
  ///   retry entirely (the first failure raises / surfaces directly).
  /// </remarks>
  TOBDRetryPolicy = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FMaxAttempts: Cardinal;
    FInitialDelayMs: Cardinal;
    FMaxDelayMs: Cardinal;
    FMultiplier: Double;
    FJitterPercent: Byte;
    FRandSeed: Cardinal;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Computes the delay for the n-th retry (1-based). Applies the
    ///   exponential curve and clamps to <c>MaxDelayMs</c>.
    /// </summary>
    /// <param name="AAttempt">1-based attempt index. Must be ≥ 1.</param>
    /// <returns>Delay in milliseconds, including jitter.</returns>
    /// <exception cref="EArgumentOutOfRangeException">
    ///   <c>AAttempt</c> is zero.</exception>
    function DelayForAttempt(AAttempt: Cardinal): Cardinal;

    /// <summary>
    ///   Seeds the internal jitter RNG. Call from tests for repeatable
    ///   delay values; do not call from production code.
    /// </summary>
    procedure SeedJitter(ASeed: Cardinal);
  published
    /// <summary>Master switch. When False the policy is bypassed and
    /// open() fails on the first error.</summary>
    property Enabled: Boolean read FEnabled write FEnabled default True;
    /// <summary>Total number of attempts including the first. Zero
    /// disables retry; one means "try, no retry".</summary>
    property MaxAttempts: Cardinal read FMaxAttempts write FMaxAttempts
      default 3;
    /// <summary>Delay before the first retry (after the first failure)
    /// in milliseconds.</summary>
    property InitialDelayMs: Cardinal read FInitialDelayMs
      write FInitialDelayMs default 200;
    /// <summary>Cap on per-attempt delay in milliseconds.</summary>
    property MaxDelayMs: Cardinal read FMaxDelayMs write FMaxDelayMs
      default 5000;
    /// <summary>Exponential growth factor between attempts.</summary>
    property Multiplier: Double read FMultiplier write FMultiplier;
    /// <summary>Random jitter as a percentage of the computed delay
    /// (0–100). 10 means ±10 %.</summary>
    property JitterPercent: Byte read FJitterPercent write FJitterPercent
      default 10;
  end;

implementation

{ ---- TOBDRetryPolicy --------------------------------------------------------- }

constructor TOBDRetryPolicy.Create;
begin
  inherited;
  FEnabled := True;
  FMaxAttempts := 3;
  FInitialDelayMs := 200;
  FMaxDelayMs := 5000;
  FMultiplier := 2.0;
  FJitterPercent := 10;
  FRandSeed := Cardinal(GetTickCount);
end;

procedure TOBDRetryPolicy.Assign(Source: TPersistent);
var
  S: TOBDRetryPolicy;
begin
  if Source is TOBDRetryPolicy then
  begin
    S := TOBDRetryPolicy(Source);
    FEnabled := S.FEnabled;
    FMaxAttempts := S.FMaxAttempts;
    FInitialDelayMs := S.FInitialDelayMs;
    FMaxDelayMs := S.FMaxDelayMs;
    FMultiplier := S.FMultiplier;
    FJitterPercent := S.FJitterPercent;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDRetryPolicy.SeedJitter(ASeed: Cardinal);
begin
  FRandSeed := ASeed;
end;

function TOBDRetryPolicy.DelayForAttempt(AAttempt: Cardinal): Cardinal;
var
  Base: Double;
  I: Cardinal;
  Jitter: Double;
  Range: Cardinal;
  RandValue: Cardinal;
  Multiplier: Double;
begin
  if AAttempt = 0 then
    raise EArgumentOutOfRangeException.Create(
      'DelayForAttempt: AAttempt must be >= 1');

  Base := FInitialDelayMs;
  Multiplier := FMultiplier;
  if Multiplier < 1.0 then
    Multiplier := 1.0;
  for I := 2 to AAttempt do
    Base := Base * Multiplier;

  if Base > FMaxDelayMs then
    Base := FMaxDelayMs;

  // Linear-congruential generator for repeatable jitter under SeedJitter.
  if FJitterPercent > 0 then
  begin
    FRandSeed := FRandSeed * 1103515245 + 12345;
    RandValue := (FRandSeed shr 16) and $7FFF;
    Range := Round(Base * FJitterPercent / 100);
    if Range > 0 then
    begin
      Jitter := (Integer(RandValue mod (2 * Range + 1)) - Integer(Range));
      Base := Base + Jitter;
      if Base < 0 then
        Base := 0;
    end;
  end;

  Result := Round(Base);
end;

end.
