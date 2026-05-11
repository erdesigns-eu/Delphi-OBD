//------------------------------------------------------------------------------
//  OBD.WWHOBD.Readiness
//
//  TOBDWWHReadiness — WWH-OBD monitor-completion reporting per
//  ISO 27145-3:2012. Walks the readiness DIDs and reports a
//  structured snapshot: which monitor groups are supported, which
//  have completed at least one full evaluation since the last DTC
//  clear, and the OBD-monitoring-conditions encountered ratio
//  (numerator / denominator) per supported group.
//
//  DIDs walked:
//    0xF411  — Major group readiness summary (supported / complete)
//    0xF412  — Per-group readiness completion flags (optional)
//    0xF40C  — Monitoring-conditions encountered counters (each
//              entry: <groupId> <numerator-hi/lo> <denominator-hi/lo>)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 27145-3:2012 § 7 (Common message dictionary — readiness)
//    - SAE J1939-73 Appendix B (group ID reference)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.WWHOBD.Readiness;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol,
  OBD.WWHOBD;

type
  /// <summary>Per-monitor-group readiness entry.</summary>
  TOBDWWHGroupReadiness = record
    GroupId:     Byte;
    Supported:   Boolean;
    Complete:    Boolean;
    /// <summary>OBD-monitoring conditions encountered counter
    /// numerator (count of times the group's enabling conditions
    /// were met). 0xFFFF when not reported.</summary>
    Numerator:   UInt16;
    /// <summary>OBD-monitoring conditions encountered counter
    /// denominator (count of times the group was eligible to run).
    /// 0xFFFF when not reported.</summary>
    Denominator: UInt16;
  end;

  /// <summary>Readiness snapshot returned by a Read call.</summary>
  TOBDWWHReadinessSnapshot = record
    /// <summary>The 16-bit major-group readiness summary.
    /// Bit set in <c>Supported</c> ↔ group is implemented on
    /// this ECU; bit set in <c>Complete</c> ↔ monitor has
    /// completed.</summary>
    MajorGroupSupported: UInt16;
    MajorGroupComplete:  UInt16;
    /// <summary>Per-group detail (only populated when 0xF412 and
    /// 0xF40C come back successfully).</summary>
    Groups: TArray<TOBDWWHGroupReadiness>;
    /// <summary>True when at least the major-group summary was
    /// read successfully.</summary>
    Valid: Boolean;
  end;

  TOBDWWHReadinessEvent = procedure(Sender: TObject;
    const ASnapshot: TOBDWWHReadinessSnapshot) of object;

  /// <summary>WWH-OBD readiness component.</summary>
  TOBDWWHReadiness = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnSnapshot: TOBDWWHReadinessEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function ReadDID(ADID: Word): TBytes;
    function DoRead: TOBDWWHReadinessSnapshot;
    procedure FireSnapshot(const ASnapshot: TOBDWWHReadinessSnapshot);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads the WWH-OBD readiness snapshot. Returns a
    /// <c>Valid = False</c> record when the ECU rejects 0xF411 —
    /// some HD ECUs only expose readiness through alternative DIDs
    /// the host may need to add via overlay.</summary>
    function Read: TOBDWWHReadinessSnapshot;
    procedure ReadAsync;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    property OnSnapshot: TOBDWWHReadinessEvent read FOnSnapshot
      write FOnSnapshot;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDWWHReadiness.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
end;

destructor TOBDWWHReadiness.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDWWHReadiness.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDWWHReadiness.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDWWHReadiness.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDWWHReadiness: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDWWHReadiness.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

function TOBDWWHReadiness.ReadDID(ADID: Word): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHReadiness: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := Byte((ADID shr 8) and $FF);
  Req[1] := Byte(ADID and $FF);
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDID 0x%.4x negative: %s', [ADID, Resp.NRCText]);
  if Length(Resp.Data) < 2 then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDID 0x%.4x: short response', [ADID]);
  Result := Copy(Resp.Data, 2, Length(Resp.Data) - 2);
end;

function TOBDWWHReadiness.DoRead: TOBDWWHReadinessSnapshot;
var
  MajorBody, GroupBody, CondBody: TBytes;
  I, Off: Integer;
  GroupId: Byte;
  GroupMap: TDictionary<Byte, TOBDWWHGroupReadiness>;
  G: TOBDWWHGroupReadiness;
  Pair: TPair<Byte, TOBDWWHGroupReadiness>;
  Tmp: TList<TOBDWWHGroupReadiness>;
begin
  Result := Default(TOBDWWHReadinessSnapshot);

  try
    MajorBody := ReadDID(WWHOBD_DID_MajorGroupReady);
    if Length(MajorBody) >= 4 then
    begin
      Result.MajorGroupSupported :=
        (UInt16(MajorBody[0]) shl 8) or UInt16(MajorBody[1]);
      Result.MajorGroupComplete :=
        (UInt16(MajorBody[2]) shl 8) or UInt16(MajorBody[3]);
      Result.Valid := True;
    end;
  except
    on E: Exception do
    begin
      // Major-group DID rejected by ECU. Some HD platforms expose
      // readiness through OEM-overlay DIDs — caller can read those
      // directly.
      Result.Valid := False;
      Exit;
    end;
  end;

  GroupMap := TDictionary<Byte, TOBDWWHGroupReadiness>.Create;
  try
    // 0xF412 — per-group readiness completion flags (optional).
    try
      GroupBody := ReadDID(WWHOBD_DID_ReadinessGroupStat);
      Off := 0;
      while Off + 2 <= Length(GroupBody) do
      begin
        GroupId := GroupBody[Off];
        G := Default(TOBDWWHGroupReadiness);
        G.GroupId   := GroupId;
        G.Supported := (GroupBody[Off + 1] and $01) <> 0;
        G.Complete  := (GroupBody[Off + 1] and $02) <> 0;
        G.Numerator := $FFFF;
        G.Denominator := $FFFF;
        GroupMap.AddOrSetValue(GroupId, G);
        Inc(Off, 2);
      end;
    except
      // 0xF412 is optional. Skip on rejection.
    end;

    // 0xF40C — monitoring-conditions encountered ratios. Each
    // entry is 5 bytes: <groupId> <numHi> <numLo> <denHi> <denLo>.
    try
      CondBody := ReadDID(WWHOBD_DID_OBDMonCondCount);
      Off := 0;
      while Off + 5 <= Length(CondBody) do
      begin
        GroupId := CondBody[Off];
        if not GroupMap.TryGetValue(GroupId, G) then
        begin
          G := Default(TOBDWWHGroupReadiness);
          G.GroupId := GroupId;
          G.Supported := True;
        end;
        G.Numerator := (UInt16(CondBody[Off + 1]) shl 8) or
                        UInt16(CondBody[Off + 2]);
        G.Denominator := (UInt16(CondBody[Off + 3]) shl 8) or
                          UInt16(CondBody[Off + 4]);
        GroupMap.AddOrSetValue(GroupId, G);
        Inc(Off, 5);
      end;
    except
      // 0xF40C optional too.
    end;

    Tmp := TList<TOBDWWHGroupReadiness>.Create;
    try
      for Pair in GroupMap do
        Tmp.Add(Pair.Value);
      Result.Groups := Tmp.ToArray;
    finally
      Tmp.Free;
    end;
  finally
    GroupMap.Free;
  end;
end;

function TOBDWWHReadiness.Read: TOBDWWHReadinessSnapshot;
begin
  Result := DoRead;
  FireSnapshot(Result);
end;

procedure TOBDWWHReadiness.ReadAsync;
var Self_: TOBDWWHReadiness;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.Read
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally Self_.ReleaseAsync; end;
    end).Start;
end;

procedure TOBDWWHReadiness.FireSnapshot(
  const ASnapshot: TOBDWWHReadinessSnapshot);
var
  Self_: TOBDWWHReadiness;
  Snap: TOBDWWHReadinessSnapshot;
begin
  if not Assigned(FOnSnapshot) then Exit;
  Self_ := Self;
  Snap := ASnapshot;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnSnapshot(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnSnapshot) then Self_.FOnSnapshot(Self_, Snap);
    end);
end;

procedure TOBDWWHReadiness.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDWWHReadiness; Code: TOBDErrorCode; Msg: string;
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
