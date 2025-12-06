//------------------------------------------------------------------------------
// UNIT           : OBD.Connection.Retry.pas
// CONTENTS       : Connection Retry Logic with Exponential Backoff
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.Connection.Retry;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, Winapi.Windows,
  OBD.Logger;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Retry strategy type
  /// </summary>
  TRetryStrategy = (rsFixed, rsLinear, rsExponential);

  /// <summary>
  ///   Retry status event
  /// </summary>
  TRetryStatusEvent = procedure(Sender: TObject; const Attempt: Integer; const MaxAttempts: Integer; const NextRetryIn: Integer) of object;

  /// <summary>
  ///   Connection retry manager with exponential backoff
  /// </summary>
  TOBDConnectionRetry = class
  private
    /// <summary>
    ///   Maximum number of retry attempts
    /// </summary>
    FMaxAttempts: Integer;
    /// <summary>
    ///   Initial retry delay in milliseconds
    /// </summary>
    FInitialDelay: Integer;
    /// <summary>
    ///   Maximum retry delay in milliseconds
    /// </summary>
    FMaxDelay: Integer;
    /// <summary>
    ///   Backoff multiplier for exponential strategy
    /// </summary>
    FBackoffMultiplier: Double;
    /// <summary>
    ///   Current retry attempt
    /// </summary>
    FCurrentAttempt: Integer;
    /// <summary>
    ///   Retry strategy
    /// </summary>
    FStrategy: TRetryStrategy;
    /// <summary>
    ///   Enable auto-retry on connection loss
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Event handler for retry status updates
    /// </summary>
    FOnRetryStatus: TRetryStatusEvent;

    /// <summary>
    ///   Calculate next retry delay based on strategy
    /// </summary>
    function CalculateDelay: Integer;

  public
    /// <summary>
    ///   Constructor with default settings
    /// </summary>
    constructor Create;

    /// <summary>
    ///   Reset retry counter
    /// </summary>
    procedure Reset;

    /// <summary>
    ///   Execute connection attempt with retry logic
    /// </summary>
    /// <param name="ConnectionFunc">
    ///   Function that attempts connection and returns True on success
    /// </param>
    /// <returns>
    ///   True if connection successful, False if all retries exhausted
    /// </returns>
    function Execute(ConnectionFunc: TFunc<Boolean>): Boolean;

    /// <summary>
    ///   Check if should retry
    /// </summary>
    function ShouldRetry: Boolean;

    /// <summary>
    ///   Wait for next retry with calculated delay
    /// </summary>
    procedure WaitForNextRetry;

    /// <summary>
    ///   Maximum number of retry attempts (default: 5)
    /// </summary>
    property MaxAttempts: Integer read FMaxAttempts write FMaxAttempts;
    /// <summary>
    ///   Initial retry delay in milliseconds (default: 1000)
    /// </summary>
    property InitialDelay: Integer read FInitialDelay write FInitialDelay;
    /// <summary>
    ///   Maximum retry delay in milliseconds (default: 30000)
    /// </summary>
    property MaxDelay: Integer read FMaxDelay write FMaxDelay;
    /// <summary>
    ///   Backoff multiplier for exponential strategy (default: 2.0)
    /// </summary>
    property BackoffMultiplier: Double read FBackoffMultiplier write FBackoffMultiplier;
    /// <summary>
    ///   Current retry attempt number
    /// </summary>
    property CurrentAttempt: Integer read FCurrentAttempt;
    /// <summary>
    ///   Retry strategy (default: rsExponential)
    /// </summary>
    property Strategy: TRetryStrategy read FStrategy write FStrategy;
    /// <summary>
    ///   Enable auto-retry (default: True)
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;
    /// <summary>
    ///   Event handler for retry status updates
    /// </summary>
    property OnRetryStatus: TRetryStatusEvent read FOnRetryStatus write FOnRetryStatus;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDConnectionRetry.Create;
begin
  inherited Create;
  FMaxAttempts := 5;
  FInitialDelay := 1000;  // 1 second
  FMaxDelay := 30000;     // 30 seconds
  FBackoffMultiplier := 2.0;
  FCurrentAttempt := 0;
  FStrategy := rsExponential;
  FEnabled := True;
end;

//------------------------------------------------------------------------------
// CALCULATE DELAY
//------------------------------------------------------------------------------
function TOBDConnectionRetry.CalculateDelay: Integer;
var
  Delay: Integer;
begin
  case FStrategy of
    rsFixed:
      Result := FInitialDelay;

    rsLinear:
      Result := FInitialDelay * (FCurrentAttempt + 1);

    rsExponential:
      begin
        // Exponential backoff: InitialDelay * (Multiplier ^ Attempt)
        Delay := Round(FInitialDelay * Power(FBackoffMultiplier, FCurrentAttempt));
        Result := Delay;
      end;
  else
    Result := FInitialDelay;
  end;

  // Cap at maximum delay
  if Result > FMaxDelay then
    Result := FMaxDelay;
end;

//------------------------------------------------------------------------------
// RESET
//------------------------------------------------------------------------------
procedure TOBDConnectionRetry.Reset;
begin
  FCurrentAttempt := 0;
end;

//------------------------------------------------------------------------------
// SHOULD RETRY
//------------------------------------------------------------------------------
function TOBDConnectionRetry.ShouldRetry: Boolean;
begin
  Result := FEnabled and (FCurrentAttempt < FMaxAttempts);
end;

//------------------------------------------------------------------------------
// WAIT FOR NEXT RETRY
//------------------------------------------------------------------------------
procedure TOBDConnectionRetry.WaitForNextRetry;
var
  Delay: Integer;
begin
  Delay := CalculateDelay;

  // Trigger status event
  if Assigned(FOnRetryStatus) then
    FOnRetryStatus(Self, FCurrentAttempt + 1, FMaxAttempts, Delay);

  // Log retry attempt
  if Assigned(GlobalLogger) then
    GlobalLogger.Info('Connection retry attempt %d/%d in %d ms', 
      [FCurrentAttempt + 1, FMaxAttempts, Delay]);

  // Wait for delay
  Sleep(Delay);
end;

//------------------------------------------------------------------------------
// EXECUTE
//------------------------------------------------------------------------------
function TOBDConnectionRetry.Execute(ConnectionFunc: TFunc<Boolean>): Boolean;
begin
  Result := False;
  Reset;

  // First attempt (not a retry)
  if Assigned(GlobalLogger) then
    GlobalLogger.Info('Attempting connection...');

  Result := ConnectionFunc();
  if Result then
  begin
    if Assigned(GlobalLogger) then
      GlobalLogger.Info('Connection successful');
    Exit;
  end;

  // Retry attempts
  while ShouldRetry do
  begin
    Inc(FCurrentAttempt);
    WaitForNextRetry;

    Result := ConnectionFunc();
    if Result then
    begin
      if Assigned(GlobalLogger) then
        GlobalLogger.Info('Connection successful after %d retries', [FCurrentAttempt]);
      Exit;
    end;

    if Assigned(GlobalLogger) then
      GlobalLogger.Warning('Connection attempt %d/%d failed', [FCurrentAttempt, FMaxAttempts]);
  end;

  // All retries exhausted
  if not Result then
  begin
    if Assigned(GlobalLogger) then
      GlobalLogger.Error('Connection failed after %d attempts', [FMaxAttempts]);
  end;
end;

end.
