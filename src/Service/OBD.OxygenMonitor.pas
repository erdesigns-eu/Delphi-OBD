//------------------------------------------------------------------------------
//  OBD.OxygenMonitor
//
//  TOBDOxygenMonitor — OBD-II Service 0x05 oxygen-sensor monitoring
//  test results. Service 0x05 is the legacy non-CAN counterpart of
//  Service 0x06 (TOBDOnBoardMonitor): on ISO 9141-2 / KWP2000 and
//  J1850 PWM/VPW the ECU reports oxygen-sensor monitor test results
//  by 2-byte Test-ID + 1-byte Component-ID, returning Test Value,
//  Min Limit and Max Limit (1 byte each).
//
//  Vehicles using ISO 15765-4 (CAN) carry oxygen-sensor monitors
//  inside Mode 0x06 (MID/TID structure) and report ZERO supported
//  test IDs here — that is the expected, spec-compliant behaviour
//  on a CAN car, NOT an error. TOBDOnBoardMonitor covers them.
//
//  Wire format per ISO 15031-5 § 7.7:
//    Request : 05 <TID> [<O2-Sensor>]
//    Response: 45 <TID> <O2-Sensor> <Test> <Min> <Max>
//
//  Supported-TID discovery walks the same bitmap convention as
//  Mode 0x01 / Mode 0x06: 0x00, 0x20, 0x40 — each request returns
//  a 4-byte mask where bit (32 - N) selects TID N + 1, plus bit 0
//  (LSB) signalling "the next bank is supported".
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 15031-5 § 7.7 (Service $05 oxygen-sensor monitoring)
//    - SAE J1979 Appendix A (Service $05 test-ID catalogue)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OxygenMonitor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol;

const
  /// <summary>OBD-II Mode 0x05 — oxygen-sensor monitoring.</summary>
  OBD_MODE_O2_MONITOR = $05;
  /// <summary>"Supported TIDs" bitmap request — TID 0x00.</summary>
  OBD_O2_TID_SUPPORTED_BLOCK = $00;

type
  /// <summary>
  ///   One oxygen-sensor test result.
  /// </summary>
  /// <remarks>
  ///   <c>Pass</c> is a derived field — <c>True</c> iff
  ///   <c>TestValue</c> falls inside <c>[MinLimit, MaxLimit]</c>.
  /// </remarks>
  TOBDOxygenMonitorResult = record
    /// <summary>Test-ID byte per SAE J1979 Appendix A.</summary>
    TestID:    Byte;
    /// <summary>O2-sensor number (0x01..0x08 per ISO 15031-5);
    /// 0x00 when the ECU did not address a specific sensor.</summary>
    O2Sensor:  Byte;
    /// <summary>Measured test value.</summary>
    TestValue: Byte;
    /// <summary>Allowed minimum.</summary>
    MinLimit:  Byte;
    /// <summary>Allowed maximum.</summary>
    MaxLimit:  Byte;
    /// <summary>Derived pass flag (in-range check).</summary>
    Pass:      Boolean;
  end;

  /// <summary>Fires after a bulk read. Main thread.</summary>
  TOBDOxygenMonitorResultsEvent = procedure(Sender: TObject;
    const AResults: TArray<TOBDOxygenMonitorResult>) of object;

  /// <summary>Fires after supported-TID discovery. Main thread.</summary>
  TOBDOxygenSupportedTIDsEvent = procedure(Sender: TObject;
    const ATIDs: TArray<Byte>) of object;

  /// <summary>
  ///   OBD-II Mode 0x05 oxygen-sensor monitoring component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected <see cref="TOBDProtocol"/>. <see cref="ReadAll"/>
  ///   walks the supported-TID bitmap and returns every available
  ///   monitor; <see cref="Read"/> reads one specific (TID, sensor)
  ///   pair.
  ///
  ///   On CAN cars every call returns an empty result set because
  ///   oxygen monitors are reported under Mode 0x06 instead. That
  ///   is correct, spec-compliant behaviour.
  /// </remarks>
  TOBDOxygenMonitor = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnResults: TOBDOxygenMonitorResultsEvent;
    FOnSupported: TOBDOxygenSupportedTIDsEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoReadOne(ATID: Byte; AO2Sensor: Byte): TOBDOxygenMonitorResult;
    function DoReadSupported: TArray<Byte>;
    procedure FireResults(const AResults: TArray<TOBDOxygenMonitorResult>);
    procedure FireSupported(const ATIDs: TArray<Byte>);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Constructs the component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Reads one test result.
    /// </summary>
    /// <param name="ATID">Test-ID byte per SAE J1979.</param>
    /// <param name="AO2Sensor">O2-sensor number (0x01..0x08), or
    /// 0x00 when not addressing a specific sensor.</param>
    /// <returns>One decoded result; <c>Pass</c> is set from the
    /// returned limits.</returns>
    /// <remarks>Blocks. From GUI code use the
    /// <see cref="ReadAllAsync"/> sweep instead.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or truncated response.
    /// </exception>
    function Read(ATID: Byte; AO2Sensor: Byte = 0): TOBDOxygenMonitorResult;

    /// <summary>
    ///   Reads every supported (TID, sensor) pair.
    /// </summary>
    /// <returns>Decoded results in TID order; empty on CAN cars.</returns>
    /// <remarks>
    ///   Walks the supported-TID bitmap then issues one read per
    ///   supported TID. A single TID failure is logged and
    ///   skipped — the sweep continues. Fires
    ///   <c>OnResults</c> on completion.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    function ReadAll: TArray<TOBDOxygenMonitorResult>;

    /// <summary>
    ///   Discovers the supported-TID set.
    /// </summary>
    /// <returns>TID bytes the ECU advertises as supported.</returns>
    /// <remarks>
    ///   Walks the 0x00, 0x20, 0x40 supported-bitmap blocks until
    ///   the chain terminator bit clears. Fires
    ///   <c>OnSupported</c> on completion.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    function ReadSupportedTIDs: TArray<Byte>;

    /// <summary>
    ///   Non-blocking <see cref="ReadAll"/>.
    /// </summary>
    /// <remarks>
    ///   Spawns a worker thread; reports completion via
    ///   <c>OnResults</c> or failure via <c>OnError</c> on the main
    ///   thread. Only one async sweep may be in flight at a time.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   Another async sweep is already in flight.
    /// </exception>
    procedure ReadAllAsync;

    /// <summary>
    ///   Non-blocking <see cref="ReadSupportedTIDs"/>.
    /// </summary>
    /// <exception cref="EOBDConfig">
    ///   Another async sweep is already in flight.
    /// </exception>
    procedure ReadSupportedTIDsAsync;
  published
    /// <summary>Protocol stack to read through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Fires after a bulk read. Main thread.</summary>
    property OnResults: TOBDOxygenMonitorResultsEvent read FOnResults
      write FOnResults;
    /// <summary>Fires after supported-TID discovery. Main thread.</summary>
    property OnSupported: TOBDOxygenSupportedTIDsEvent read FOnSupported
      write FOnSupported;
    /// <summary>Fires on transient I/O errors. Main thread.</summary>
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDOxygenMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDOxygenMonitor.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDOxygenMonitor.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
end;

procedure TOBDOxygenMonitor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDOxygenMonitor.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDOxygenMonitor: async already in flight');
    FAsyncInFlight := True;
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDOxygenMonitor.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
end;

function TOBDOxygenMonitor.DoReadOne(ATID: Byte;
  AO2Sensor: Byte): TOBDOxygenMonitorResult;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDOxygenMonitor: Protocol not assigned');
  if AO2Sensor = 0 then
  begin
    SetLength(Req, 1);
    Req[0] := ATID;
  end
  else
  begin
    SetLength(Req, 2);
    Req[0] := ATID;
    Req[1] := AO2Sensor;
  end;

  Resp := FProtocol.Request(OBD_MODE_O2_MONITOR, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 05 TID 0x%.2x negative: %s', [ATID, Resp.NRCText]);
  if Length(Resp.Data) < 5 then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 05 TID 0x%.2x: short response (%d bytes)',
      [ATID, Length(Resp.Data)]);

  Result.TestID    := Resp.Data[0];
  Result.O2Sensor  := Resp.Data[1];
  Result.TestValue := Resp.Data[2];
  Result.MinLimit  := Resp.Data[3];
  Result.MaxLimit  := Resp.Data[4];
  Result.Pass := (Result.TestValue >= Result.MinLimit) and
                 (Result.TestValue <= Result.MaxLimit);
end;

function TOBDOxygenMonitor.DoReadSupported: TArray<Byte>;
var
  Block: Integer;
  BitIdx: Integer;
  Resp: TOBDResponse;
  Mask: UInt32;
  Acc: TList<Byte>;
  Req: TBytes;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDOxygenMonitor: Protocol not assigned');
  Acc := TList<Byte>.Create;
  try
    Block := OBD_O2_TID_SUPPORTED_BLOCK;
    while True do
    begin
      SetLength(Req, 1);
      Req[0] := Byte(Block);
      Resp := FProtocol.Request(OBD_MODE_O2_MONITOR, Req);
      if Resp.IsNegative or (Length(Resp.Data) < 4) then
        Break;
      // The supported-bitmap reply carries the 32-bit mask in the
      // four "result" bytes. Some ECUs prefix the response with the
      // requested TID + sensor byte (5 bytes total); others drop
      // the sensor and emit only 4 bytes. Accept both shapes.
      if Length(Resp.Data) >= 5 then
        Mask := (UInt32(Resp.Data[1]) shl 24) or
                (UInt32(Resp.Data[2]) shl 16) or
                (UInt32(Resp.Data[3]) shl 8) or
                 UInt32(Resp.Data[4])
      else
        Mask := (UInt32(Resp.Data[0]) shl 24) or
                (UInt32(Resp.Data[1]) shl 16) or
                (UInt32(Resp.Data[2]) shl 8) or
                 UInt32(Resp.Data[3]);

      for BitIdx := 1 to 31 do
        if (Mask and (UInt32(1) shl (32 - BitIdx))) <> 0 then
          Acc.Add(Byte(Block + BitIdx));
      if (Mask and $00000001) = 0 then
        Break;
      Inc(Block, $20);
      if Block >= $E0 then
        Break;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDOxygenMonitor.Read(ATID: Byte;
  AO2Sensor: Byte): TOBDOxygenMonitorResult;
begin
  Result := DoReadOne(ATID, AO2Sensor);
end;

function TOBDOxygenMonitor.ReadSupportedTIDs: TArray<Byte>;
begin
  Result := DoReadSupported;
  FireSupported(Result);
end;

function TOBDOxygenMonitor.ReadAll: TArray<TOBDOxygenMonitorResult>;
var
  TIDs: TArray<Byte>;
  Acc: TList<TOBDOxygenMonitorResult>;
  I: Integer;
  R: TOBDOxygenMonitorResult;
begin
  TIDs := DoReadSupported;
  Acc := TList<TOBDOxygenMonitorResult>.Create;
  try
    for I := 0 to High(TIDs) do
    begin
      try
        R := DoReadOne(TIDs[I], 0);
        Acc.Add(R);
      except
        // A single bad TID must not kill the sweep — log via the
        // OnError surface and move on.
        on E: Exception do
          FireError(oeIO, E.Message);
      end;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
  FireResults(Result);
end;

procedure TOBDOxygenMonitor.ReadAllAsync;
var
  Self_: TOBDOxygenMonitor;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadAll;
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDOxygenMonitor.ReadSupportedTIDsAsync;
var
  Self_: TOBDOxygenMonitor;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var
      TIDs: TArray<Byte>;
    begin
      try
        try
          TIDs := Self_.DoReadSupported;
          Self_.FireSupported(TIDs);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDOxygenMonitor.FireResults(
  const AResults: TArray<TOBDOxygenMonitorResult>);
var
  Self_: TOBDOxygenMonitor;
  Snap: TArray<TOBDOxygenMonitorResult>;
begin
  if not Assigned(FOnResults) then
    Exit;
  Self_ := Self;
  Snap := Copy(AResults, 0, Length(AResults));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResults(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnResults) then
          Self_.FOnResults(Self_, Snap);
      end);
end;

procedure TOBDOxygenMonitor.FireSupported(const ATIDs: TArray<Byte>);
var
  Self_: TOBDOxygenMonitor;
  Snap: TArray<Byte>;
begin
  if not Assigned(FOnSupported) then
    Exit;
  Self_ := Self;
  Snap := Copy(ATIDs, 0, Length(ATIDs));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSupported(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnSupported) then
          Self_.FOnSupported(Self_, Snap);
      end);
end;

procedure TOBDOxygenMonitor.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDOxygenMonitor;
  Code: TOBDErrorCode;
  Msg: string;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    var Handled: Boolean;
    Handled := False;
    FOnError(Self_, Code, Msg, Handled);
  end
  else
    TThread.Queue(nil,
      procedure
      var
        Handled: Boolean;
      begin
        Handled := False;
        if Assigned(Self_.FOnError) then
          Self_.FOnError(Self_, Code, Msg, Handled);
      end);
end;

end.
