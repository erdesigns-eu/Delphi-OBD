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
//  Mode 01 / Mode 06: 0x00, 0x20, 0x40 — each request returns a
//  4-byte mask where bit (32-N) selects TID N+1, plus bit 0 (LSB)
//  signalling "supports the next bank".
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
  OBD_MODE_O2_MONITOR        = $05;
  /// <summary>"Supported TIDs" bitmap request — TID 0x00.</summary>
  OBD_O2_TID_SUPPORTED_BLOCK = $00;

type
  /// <summary>One oxygen-sensor test result.</summary>
  TOBDOxygenMonitorResult = record
    TestID:      Byte;
    O2Sensor:    Byte;
    TestValue:   Byte;
    MinLimit:    Byte;
    MaxLimit:    Byte;
    /// <summary><c>True</c> when the value falls inside the
    /// <c>[MinLimit, MaxLimit]</c> band reported by the ECU.</summary>
    Pass:        Boolean;
  end;

  /// <summary>Bulk read result event.</summary>
  TOBDOxygenMonitorResultsEvent = procedure(Sender: TObject;
    const AResults: TArray<TOBDOxygenMonitorResult>) of object;

  /// <summary>Supported-TID discovery result event.</summary>
  TOBDOxygenSupportedTIDsEvent = procedure(Sender: TObject;
    const ATIDs: TArray<Byte>) of object;

  /// <summary>OBD-II Mode 0x05 oxygen-sensor monitoring
  /// component.</summary>
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
    function DoReadOne(ATID, AO2Sensor: Byte): TOBDOxygenMonitorResult;
    function DoReadSupported: TArray<Byte>;
    procedure FireResults(const AResults: TArray<TOBDOxygenMonitorResult>);
    procedure FireSupported(const ATIDs: TArray<Byte>);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads one test result. <c>AO2Sensor</c>: 0x01..0x08
    /// per ISO 15031-5; pass 0x00 when not addressing a specific
    /// sensor.</summary>
    function Read(ATID, AO2Sensor: Byte = 0): TOBDOxygenMonitorResult;
    /// <summary>Reads every supported (TID, sensor) pair from the
    /// supported-TID bitmaps. Returns an empty array on CAN cars
    /// (which is correct: those report monitors under Mode 0x06).
    /// </summary>
    function ReadAll: TArray<TOBDOxygenMonitorResult>;
    /// <summary>Discovers the supported TID set by walking 0x00,
    /// 0x20, 0x40 supported-bitmap blocks.</summary>
    function ReadSupportedTIDs: TArray<Byte>;
    /// <summary>Non-blocking <see cref="ReadAll"/>.</summary>
    procedure ReadAllAsync;
    /// <summary>Non-blocking <see cref="ReadSupportedTIDs"/>.</summary>
    procedure ReadSupportedTIDsAsync;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property OnResults: TOBDOxygenMonitorResultsEvent read FOnResults
      write FOnResults;
    property OnSupported: TOBDOxygenSupportedTIDsEvent read FOnSupported
      write FOnSupported;
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
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
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
  finally FAsyncLock.Leave; end;
end;

procedure TOBDOxygenMonitor.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDOxygenMonitor.DoReadOne(ATID, AO2Sensor: Byte): TOBDOxygenMonitorResult;
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
  Block, BitIdx: Integer;
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
      if Resp.IsNegative or (Length(Resp.Data) < 5) then Break;
      // Resp.Data: [TID, sensor, test, min, max]. For the
      // supported-bitmap query the four "result" bytes carry the
      // 32-bit mask; some ECUs echo TID + the 4 mask bytes
      // directly, with no sensor byte. Accept either: if the byte
      // count is exactly 5 we use bytes 1..4, when it's 4 (no
      // echo) bytes 0..3.
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

      // Bits 31..1 select TID (Block+1)..(Block+31). Bit 0 = next
      // block supported.
      for BitIdx := 1 to 31 do
        if (Mask and (UInt32(1) shl (32 - BitIdx))) <> 0 then
          Acc.Add(Byte(Block + BitIdx));
      if (Mask and $00000001) = 0 then Break;
      Inc(Block, $20);
      if Block >= $E0 then Break;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDOxygenMonitor.Read(ATID, AO2Sensor: Byte): TOBDOxygenMonitorResult;
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
        // A single bad TID shouldn't kill the sweep — skip it.
      end;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
  FireResults(Result);
end;

procedure TOBDOxygenMonitor.ReadAllAsync;
var Self_: TOBDOxygenMonitor;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var R: TArray<TOBDOxygenMonitorResult>;
    begin
      try
        try
          R := Self_.ReadAll;
          // ReadAll already fires the event on whichever thread
          // we're on. Marshal to main from FireResults.
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDOxygenMonitor.ReadSupportedTIDsAsync;
var Self_: TOBDOxygenMonitor;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var TIDs: TArray<Byte>;
    begin
      try
        try
          TIDs := Self_.DoReadSupported;
          Self_.FireSupported(TIDs);
        except
          on E: Exception do Self_.FireError(oeIO, E.Message);
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
  if not Assigned(FOnResults) then Exit;
  Self_ := Self;
  Snap := Copy(AResults, 0, Length(AResults));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnResults(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnResults) then Self_.FOnResults(Self_, Snap);
    end);
end;

procedure TOBDOxygenMonitor.FireSupported(const ATIDs: TArray<Byte>);
var
  Self_: TOBDOxygenMonitor; Snap: TArray<Byte>;
begin
  if not Assigned(FOnSupported) then Exit;
  Self_ := Self;
  Snap := Copy(ATIDs, 0, Length(ATIDs));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSupported(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnSupported) then Self_.FOnSupported(Self_, Snap);
    end);
end;

procedure TOBDOxygenMonitor.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDOxygenMonitor; Code: TOBDErrorCode; Msg: string;
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

end.
