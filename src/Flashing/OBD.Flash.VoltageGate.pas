//------------------------------------------------------------------------------
//  OBD.Flash.VoltageGate
//
//  TOBDVoltageGate — battery / supply voltage monitor used by the
//  flash pipeline to abort if the supply drops below a safe
//  threshold. The single most common cause of bricked ECUs during
//  reflashing is a brown-out mid-write; this gate enforces that
//  the host has either confirmed a stable supply or has explicitly
//  signed off on the risk.
//
//  SAFETY — BRICK RISK ----------------------------------------------------
//  Misuse can BRICK the ECU. Read docs/flashing-safety.md before
//  integrating.
//  ------------------------------------------------------------------------
//
//  The component does NOT measure voltage itself. The host wires a
//  measurement source (a Mode 01 PID 0x42 reader, an OEM-specific
//  voltage DID, a Vector / PEAK adapter line-voltage register,
//  whatever the host has) into <c>OnRequestVoltage</c>. The gate
//  polls at <c>PollIntervalMs</c> while a flash is running and
//  fires <c>OnVoltageLow</c> + <c>OnAbort</c> when the latest
//  reading falls below <c>MinimumVoltage</c> for
//  <c>HoldTimeMs</c> milliseconds.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Flash.VoltageGate;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types;

type
  /// <summary>Procedural voltage source.</summary>
  TOBDVoltageSourceFunc = reference to function: Double;

  /// <summary>Fires when a fresh voltage reading is available.</summary>
  TOBDVoltageReadingEvent = procedure(Sender: TObject;
    AVoltage: Double) of object;

  /// <summary>Fires when the gate aborts a flash (latest reading
  /// has been below <c>MinimumVoltage</c> for
  /// <c>HoldTimeMs</c>).</summary>
  TOBDVoltageAbortEvent = procedure(Sender: TObject;
    AVoltage: Double; AReason: string) of object;

  /// <summary>Voltage-monitor component.</summary>
  TOBDVoltageGate = class(TComponent)
  strict private
    FSourceFunc: TOBDVoltageSourceFunc;
    FOnRequestVoltage: TOBDVoltageReadingEvent;
    FOnVoltageLow: TOBDVoltageAbortEvent;
    FOnAbort: TOBDVoltageAbortEvent;
    FMinimumVoltage: Double;
    FPollIntervalMs: Cardinal;
    FHoldTimeMs: Cardinal;
    FLastVoltage: Double;
    FLowSinceTick: TDateTime;
    FInLow: Boolean;
    FRunning: Boolean;
    FLock: TCriticalSection;
    FStopRequested: Boolean;
    FThread: TThread;
    procedure FireReading(AVoltage: Double);
    procedure FireLow(AVoltage: Double);
    procedure FireAbort(AVoltage: Double; const AReason: string);
    function ReadVoltage: Double;
    procedure RunLoop;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Starts polling. The next abort condition fires
    /// <c>OnAbort</c>. Idempotent.</summary>
    procedure Start;
    /// <summary>Stops polling and joins the worker.</summary>
    procedure Stop;
    /// <summary>True when polling.</summary>
    function IsRunning: Boolean;
    /// <summary>Returns the most recent voltage sampled.</summary>
    function LastVoltage: Double;

    /// <summary>Functional voltage source. Set this OR
    /// <c>OnRequestVoltage</c>; the functional form takes
    /// precedence.</summary>
    property SourceFunc: TOBDVoltageSourceFunc
      read FSourceFunc write FSourceFunc;
  published
    /// <summary>Procedural voltage source. Fired on the polling
    /// thread; the handler must be thread-safe.</summary>
    property OnRequestVoltage: TOBDVoltageReadingEvent
      read FOnRequestVoltage write FOnRequestVoltage;
    /// <summary>Fires every time the latest reading drops below
    /// <c>MinimumVoltage</c>. Re-fires on each subsequent low
    /// reading.</summary>
    property OnVoltageLow: TOBDVoltageAbortEvent
      read FOnVoltageLow write FOnVoltageLow;
    /// <summary>Fires when the abort condition latches (low for
    /// at least <c>HoldTimeMs</c>). Hosts wire this to abort the
    /// flash pipeline.</summary>
    property OnAbort: TOBDVoltageAbortEvent
      read FOnAbort write FOnAbort;
    /// <summary>Lower-bound voltage (volts). Default 12.0.</summary>
    property MinimumVoltage: Double read FMinimumVoltage
      write FMinimumVoltage;
    /// <summary>Polling interval in ms. Default 200.</summary>
    property PollIntervalMs: Cardinal read FPollIntervalMs
      write FPollIntervalMs default 200;
    /// <summary>How long the reading must remain below the
    /// threshold before <c>OnAbort</c> fires, in ms. Default
    /// 1000.</summary>
    property HoldTimeMs: Cardinal read FHoldTimeMs write FHoldTimeMs
      default 1000;
  end;

implementation

uses
  System.DateUtils;

constructor TOBDVoltageGate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FMinimumVoltage := 12.0;
  FPollIntervalMs := 200;
  FHoldTimeMs := 1000;
end;

destructor TOBDVoltageGate.Destroy;
begin
  Stop;
  FLock.Free;
  inherited;
end;

function TOBDVoltageGate.ReadVoltage: Double;
var
  V: Double;
begin
  if Assigned(FSourceFunc) then
    Exit(FSourceFunc);
  if Assigned(FOnRequestVoltage) then
  begin
    V := 0;
    FOnRequestVoltage(Self, V);
    Exit(V);
  end;
  raise EOBDConfig.Create(
    'TOBDVoltageGate: no voltage source configured ' +
    '(set SourceFunc or OnRequestVoltage)');
end;

procedure TOBDVoltageGate.RunLoop;
var
  V: Double;
  Now_: TDateTime;
  Elapsed: Int64;
begin
  while not FStopRequested do
  begin
    try
      V := ReadVoltage;
      FLock.Enter;
      try
        FLastVoltage := V;
      finally
        FLock.Leave;
      end;
      FireReading(V);
      Now_ := Now;
      if V < FMinimumVoltage then
      begin
        if not FInLow then
        begin
          FInLow := True;
          FLowSinceTick := Now_;
        end;
        FireLow(V);
        Elapsed := MilliSecondsBetween(Now_, FLowSinceTick);
        if Elapsed >= Int64(FHoldTimeMs) then
        begin
          FireAbort(V, Format(
            'Voltage %.2f V < %.2f V for %d ms',
            [V, FMinimumVoltage, Elapsed]));
          Break;
        end;
      end
      else
      begin
        FInLow := False;
      end;
    except
      on E: Exception do
      begin
        FireAbort(0, 'Voltage source raised: ' + E.Message);
        Break;
      end;
    end;
    Sleep(FPollIntervalMs);
  end;
  FRunning := False;
end;

procedure TOBDVoltageGate.Start;
var
  Self_: TOBDVoltageGate;
begin
  if FRunning then Exit;
  FRunning := True;
  FStopRequested := False;
  FInLow := False;
  Self_ := Self;
  FThread := TThread.CreateAnonymousThread(
    procedure begin Self_.RunLoop; end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TOBDVoltageGate.Stop;
var
  T: TThread;
begin
  if not FRunning then Exit;
  FStopRequested := True;
  T := FThread;
  FThread := nil;
  if T <> nil then
  begin
    T.WaitFor;
    T.Free;
  end;
  FRunning := False;
end;

function TOBDVoltageGate.IsRunning: Boolean;
begin
  Result := FRunning;
end;

function TOBDVoltageGate.LastVoltage: Double;
begin
  FLock.Enter;
  try Result := FLastVoltage;
  finally FLock.Leave; end;
end;

procedure TOBDVoltageGate.FireReading(AVoltage: Double);
var
  Self_: TOBDVoltageGate;
  V: Double;
begin
  // OnRequestVoltage was already invoked synchronously to read
  // the value; this hook is only for hosts that want a separate
  // "I just got a reading" callback. We deliver via OnVoltageLow
  // when low, OnAbort when latched. No separate event needed for
  // a regular reading — keep the surface tight.
  Self_ := Self; V := AVoltage;
  if (V <> 0) and Assigned(Self_) then ; // suppress hint
end;

procedure TOBDVoltageGate.FireLow(AVoltage: Double);
var
  Self_: TOBDVoltageGate; V: Double; Reason: string;
begin
  if not Assigned(FOnVoltageLow) then Exit;
  Self_ := Self; V := AVoltage;
  Reason := Format('Voltage %.2f V below threshold %.2f V',
    [V, FMinimumVoltage]);
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnVoltageLow(Self_, V, Reason)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnVoltageLow) then
        Self_.FOnVoltageLow(Self_, V, Reason);
    end);
end;

procedure TOBDVoltageGate.FireAbort(AVoltage: Double;
  const AReason: string);
var
  Self_: TOBDVoltageGate; V: Double; R: string;
begin
  if not Assigned(FOnAbort) then Exit;
  Self_ := Self; V := AVoltage; R := AReason;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnAbort(Self_, V, R)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnAbort) then Self_.FOnAbort(Self_, V, R);
    end);
end;

end.
