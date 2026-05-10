//------------------------------------------------------------------------------
//  OBD.Service.OnBoardMonitor
//
//  TOBDOnBoardMonitor — non-visual component that reads OBD-II
//  Mode 06 on-board monitoring test results.
//
//  Mode 06 returns one or more (TID, ComponentID, UnitAndScale,
//  TestValue, Min, Max) tuples per Monitor ID (MID). On CAN
//  controllers each tuple is 9 bytes wide:
//
//    TID (1) | CID (1) | Unit/Scale (1) | TestValue (2 BE)
//                                       | Min (2 BE) | Max (2 BE)
//
//  The component returns a flat list of TOBDMonitorResult records;
//  hosts decide whether each entry passed (Min ≤ Value ≤ Max).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1979 Mode 06 — On-board monitoring test results
//    - SAE J1979-2 Annex B (CAN-specific Mode 06 layout)
//
//  History     :
//    2026-05-09  ERD  Follow-up — close-out deferrals.
//------------------------------------------------------------------------------

unit OBD.Service.OnBoardMonitor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Data.Bind.Components,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol;

const
  /// <summary>OBD-II Mode 06 — on-board monitoring test results.</summary>
  OBD_MODE_ON_BOARD_MONITORING = $06;

type
  /// <summary>One on-board-monitor test tuple.</summary>
  TOBDMonitorResult = record
    /// <summary>Monitor ID this tuple belongs to.</summary>
    MID: Byte;
    /// <summary>Test ID (TID).</summary>
    TID: Byte;
    /// <summary>Component ID (CID, sometimes "OBDMID-bound TID").</summary>
    ComponentID: Byte;
    /// <summary>Unit and scaling code.</summary>
    UnitAndScale: Byte;
    /// <summary>Measured test value (raw 16-bit signed).</summary>
    TestValue: SmallInt;
    /// <summary>Minimum permitted value (raw 16-bit signed).</summary>
    MinLimit: SmallInt;
    /// <summary>Maximum permitted value (raw 16-bit signed).</summary>
    MaxLimit: SmallInt;
  end;

  /// <summary>Fires when a Mode 06 read completes.</summary>
  TOBDMonitorEvent = procedure(Sender: TObject; AMID: Byte;
    const AResults: TArray<TOBDMonitorResult>) of object;

  /// <summary>Mode 06 service component.</summary>
  TOBDOnBoardMonitor = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnResults: TOBDMonitorEvent;
    FOnError: TOBDConnectionErrorEvent;
    function DoRead(AMID: Byte): TArray<TOBDMonitorResult>;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    procedure FireResults(AMID: Byte;
      const AResults: TArray<TOBDMonitorResult>);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads test results for monitor <c>AMID</c>.</summary>
    /// <param name="AMID">Monitor ID. <c>0x00</c> requests the
    /// supported-MID bitmap; <c>0x01..0xFF</c> request a specific
    /// monitor.</param>
    function Read(AMID: Byte): TArray<TOBDMonitorResult>;
    /// <summary>Non-blocking <see cref="Read"/>.</summary>
    procedure ReadAsync(AMID: Byte);
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property OnResults: TOBDMonitorEvent read FOnResults write FOnResults;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDOnBoardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDOnBoardMonitor.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDOnBoardMonitor.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDOnBoardMonitor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDOnBoardMonitor.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDOnBoardMonitor: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDOnBoardMonitor.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDOnBoardMonitor.DoRead(AMID: Byte): TArray<TOBDMonitorResult>;
var
  Resp: TOBDResponse;
  Off, N, I: Integer;
  Entry: TOBDMonitorResult;
  Acc: TArray<TOBDMonitorResult>;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDOnBoardMonitor: Protocol not assigned');
  Resp := FProtocol.Request(OBD_MODE_ON_BOARD_MONITORING, TBytes.Create(AMID));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'Mode 06 MID 0x%2.2X negative: %s', [AMID, Resp.NRCText]);

  // Response Data: <MID echo> [TID CID UnitScale Val(2) Min(2) Max(2)]+
  if Length(Resp.Data) < 1 then Exit(nil);
  Off := 0;
  if Resp.Data[0] = AMID then Off := 1;
  N := (Length(Resp.Data) - Off) div 9;
  if N <= 0 then Exit(nil);
  SetLength(Acc, N);
  for I := 0 to N - 1 do
  begin
    Entry := Default(TOBDMonitorResult);
    Entry.MID          := AMID;
    Entry.TID          := Resp.Data[Off + 0];
    Entry.ComponentID  := Resp.Data[Off + 1];
    Entry.UnitAndScale := Resp.Data[Off + 2];
    Entry.TestValue := SmallInt(
      (Word(Resp.Data[Off + 3]) shl 8) or Word(Resp.Data[Off + 4]));
    Entry.MinLimit  := SmallInt(
      (Word(Resp.Data[Off + 5]) shl 8) or Word(Resp.Data[Off + 6]));
    Entry.MaxLimit  := SmallInt(
      (Word(Resp.Data[Off + 7]) shl 8) or Word(Resp.Data[Off + 8]));
    Acc[I] := Entry;
    Inc(Off, 9);
  end;
  Result := Acc;
end;

function TOBDOnBoardMonitor.Read(AMID: Byte): TArray<TOBDMonitorResult>;
begin
  Result := DoRead(AMID);
  FireResults(AMID, Result);
end;

procedure TOBDOnBoardMonitor.ReadAsync(AMID: Byte);
var
  Self_: TOBDOnBoardMonitor;
  MID: Byte;
begin
  GuardSingleAsync;
  Self_ := Self; MID := AMID;
  TThread.CreateAnonymousThread(
    procedure
    var
      Acc: TArray<TOBDMonitorResult>;
    begin
      try
        try
          Acc := Self_.DoRead(MID);
          Self_.FireResults(MID, Acc);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDOnBoardMonitor.FireResults(AMID: Byte;
  const AResults: TArray<TOBDMonitorResult>);
var
  Self_: TOBDOnBoardMonitor;
  MID: Byte;
  Snap: TArray<TOBDMonitorResult>;
begin
  Self_ := Self; MID := AMID; Snap := Copy(AResults, 0, Length(AResults));
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    try TBindings.Notify(Self_, ''); except end;
    if Assigned(FOnResults) then FOnResults(Self_, MID, Snap);
  end
  else
    TThread.Queue(nil, procedure begin
      try TBindings.Notify(Self_, ''); except end;
      if Assigned(Self_.FOnResults) then
        Self_.FOnResults(Self_, MID, Snap);
    end);
end;

procedure TOBDOnBoardMonitor.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDOnBoardMonitor; Code: TOBDErrorCode; Msg: string;
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
