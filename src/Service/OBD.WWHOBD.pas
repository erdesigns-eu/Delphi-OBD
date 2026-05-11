//------------------------------------------------------------------------------
//  OBD.WWHOBD
//
//  TOBDWWHOBD — World-Wide Harmonised OBD (heavy-duty OBD per
//  ISO 27145-1..5). On modern HD/diesel vehicles WWH-OBD overlays
//  the classic OBD-II surface with a UDS-based DTC and readiness
//  surface that distinguishes DTC severity classes A / B1 / B2 / C
//  (ISO 14229-1 §11.3.5.4 DTCSeverityMask) — Class A is emission-
//  critical and lights the MIL immediately; Class B1/B2 may need
//  two-trip confirmation; Class C is informational.
//
//  This component covers the WWH-OBD diagnostic surface:
//    - Read DTCs by severity mask (0x19 sub 0x42)
//    - Read DTCs by readiness-group ID (0x19 sub 0x55)
//    - DTC class counters (DIDs 0xF418 / 0xF419)
//    - Distance / time-with-MIL (DIDs 0xF402 / 0xF407)
//    - VIN (DID 0xF190 — same as Phase 5 TOBDVIN; we expose it
//      here too because HD techs frequently want the bundle in
//      one call)
//
//  TOBDWWHReadiness in a sibling unit covers the monitor-completion
//  flags (DIDs 0xF411 / 0xF412 / 0xF40C).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 27145-1:2012 — General requirements for WWH-OBD
//    - ISO 27145-2:2012 — Common data dictionary
//    - ISO 27145-3:2012 — Common message dictionary
//    - ISO 14229-1:2020 § 11.3 (ReadDTCInformation) — sub-functions
//      0x42 (DTC by severity mask) and 0x55 (WWH-OBD by readiness
//      group ID)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.WWHOBD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol;

const
  /// <summary>UDS 0x19 sub-function 0x42 — report DTC by severity
  /// information.</summary>
  UDS_DTC_REPORT_BY_SEVERITY_MASK = $42;
  /// <summary>UDS 0x19 sub-function 0x55 — report WWH-OBD DTC by
  /// readiness-group ID.</summary>
  UDS_DTC_REPORT_WWHOBD_BY_GROUP  = $55;

  /// <summary>WWH-OBD DTC severity bits per ISO 14229-1
  /// §11.3.5.4.</summary>
  WWHOBD_SEV_CHECK_AT_NEXT_HALT = $20;
  WWHOBD_SEV_CHECK_IMMEDIATELY  = $40;
  WWHOBD_SEV_MAINTENANCE_ONLY   = $80;

  /// <summary>UDS DID — distance travelled with MIL on (km).</summary>
  WWHOBD_DID_DistanceMILOn      = $F402;
  /// <summary>UDS DID — time run with MIL on (minutes).</summary>
  WWHOBD_DID_TimeMILOn          = $F407;
  /// <summary>UDS DID — major group readiness summary.</summary>
  WWHOBD_DID_MajorGroupReady    = $F411;
  /// <summary>UDS DID — readiness-group-specific status.</summary>
  WWHOBD_DID_ReadinessGroupStat = $F412;
  /// <summary>UDS DID — monitoring conditions encountered counter.
  /// </summary>
  WWHOBD_DID_OBDMonCondCount    = $F40C;
  /// <summary>UDS DID — DTC counter (class A by default; varies
  /// per OEM).</summary>
  WWHOBD_DID_DTCCounter         = $F418;
  /// <summary>UDS DID — second DTC counter byte (e.g. B1
  /// pending).</summary>
  WWHOBD_DID_DTCCounter2        = $F419;
  /// <summary>UDS DID — VIN (ISO 27145-2 alias of the standard
  /// VIN DID).</summary>
  WWHOBD_DID_VIN                = $F190;

type
  /// <summary>WWH-OBD DTC severity class. Maps to the high three
  /// bits of the severity byte returned by sub-function 0x42.
  /// </summary>
  TOBDWWHDtcClass = (
    wcUnknown,    // severity byte = 0
    wcMaintenance,// severity byte high bit 0x80 set, classes "no MIL"
    wcCheckHalt,  // 0x20 — Class B
    wcCheckNow,   // 0x40 — Class A (immediate MIL)
    wcCheckBoth   // 0x20 | 0x40 — Class A escalation
  );

  /// <summary>One WWH-OBD DTC entry.</summary>
  TOBDWWHDtcEntry = record
    /// <summary>5-character J2012-format DTC code (e.g. P0420).</summary>
    Code:           string;
    /// <summary>Severity byte verbatim (high nibble carries the
    /// class bits, low nibble carries the failure class).</summary>
    SeverityByte:   Byte;
    /// <summary>UDS status mask for this DTC (testFailed bits).</summary>
    StatusByte:     Byte;
    /// <summary>Decoded class.</summary>
    DtcClass:       TOBDWWHDtcClass;
    /// <summary>Functional unit byte (ISO 14229-1 §11.3.5.4 — the
    /// hi-byte that follows the severity byte).</summary>
    FunctionalUnit: Byte;
    /// <summary>Raw 3-byte DTC plus the severity / functional
    /// pair / status byte exactly as returned by the ECU.</summary>
    Raw:            TBytes;
  end;

  /// <summary>Fires after a successful DTC sweep.</summary>
  TOBDWWHDtcsEvent = procedure(Sender: TObject;
    const AEntries: TArray<TOBDWWHDtcEntry>) of object;

  /// <summary>Fires when the MIL distance / time has been read.</summary>
  TOBDWWHMILUsageEvent = procedure(Sender: TObject;
    ADistanceKm: UInt32; ATimeMinutes: UInt32) of object;

  /// <summary>WWH-OBD HD component.</summary>
  TOBDWWHOBD = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FSeverityMask: Byte;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnDTCs: TOBDWWHDtcsEvent;
    FOnMILUsage: TOBDWWHMILUsageEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoReadBySeverity(AMask: Byte): TArray<TOBDWWHDtcEntry>;
    function DoReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;
    function DoReadDID(ADID: Word; AExpectedBytes: Integer): TBytes;
    function DecodeBigEndian(const AData: TBytes;
      AOffset, ABytes: Integer): UInt32;
    procedure FireDTCs(const AEntries: TArray<TOBDWWHDtcEntry>);
    procedure FireMILUsage(ADistance, ATime: UInt32);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);
    procedure SetProtocol(AValue: TOBDProtocol);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Reads every DTC whose severity byte intersects
    /// <see cref="SeverityMask"/>. The mask is a bitwise OR of
    /// <c>WWHOBD_SEV_*</c> values.</summary>
    function ReadBySeverity: TArray<TOBDWWHDtcEntry>;
    /// <summary>Reads every WWH-OBD DTC for readiness group
    /// <c>AGroupId</c> (ISO 27145-3).</summary>
    function ReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;
    /// <summary>Reads the MIL-on usage counters (km and minutes).
    /// </summary>
    procedure ReadMILUsage(out ADistanceKm: UInt32;
      out ATimeMinutes: UInt32);
    /// <summary>Reads the Class A / Class B1 DTC counters
    /// (DIDs 0xF418 / 0xF419).</summary>
    procedure ReadDTCCounters(out AClassA, AClassB1: UInt32);
    /// <summary>Reads the VIN as exposed via DID 0xF190.</summary>
    function ReadVIN: string;
    /// <summary>Maps a severity byte to a <see cref="TOBDWWHDtcClass"/>.
    /// </summary>
    class function ClassifySeverity(AByte: Byte): TOBDWWHDtcClass; static;

    procedure ReadBySeverityAsync;
    procedure ReadByGroupAsync(AGroupId: Byte);
    procedure ReadMILUsageAsync;
  published
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Default severity-mask filter applied to
    /// <see cref="ReadBySeverity"/>. Default <c>0xE0</c> (every
    /// MIL-eligible severity bit).</summary>
    property SeverityMask: Byte read FSeverityMask write FSeverityMask
      default WWHOBD_SEV_MAINTENANCE_ONLY or WWHOBD_SEV_CHECK_AT_NEXT_HALT
            or WWHOBD_SEV_CHECK_IMMEDIATELY;
    property OnDTCs: TOBDWWHDtcsEvent read FOnDTCs write FOnDTCs;
    property OnMILUsage: TOBDWWHMILUsageEvent read FOnMILUsage
      write FOnMILUsage;
    property OnError: TOBDConnectionErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TOBDWWHOBD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAsyncLock := TCriticalSection.Create;
  FSeverityMask := WWHOBD_SEV_MAINTENANCE_ONLY or
                   WWHOBD_SEV_CHECK_AT_NEXT_HALT or
                   WWHOBD_SEV_CHECK_IMMEDIATELY;
end;

destructor TOBDWWHOBD.Destroy;
begin
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDWWHOBD.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDWWHOBD.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDWWHOBD.GuardSingleAsync;
begin
  FAsyncLock.Enter;
  try
    if FAsyncInFlight then
      raise EOBDConfig.Create('TOBDWWHOBD: async already in flight');
    FAsyncInFlight := True;
  finally FAsyncLock.Leave; end;
end;

procedure TOBDWWHOBD.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try FAsyncInFlight := False;
  finally FAsyncLock.Leave; end;
end;

class function TOBDWWHOBD.ClassifySeverity(AByte: Byte): TOBDWWHDtcClass;
const
  HighBits = WWHOBD_SEV_MAINTENANCE_ONLY or
             WWHOBD_SEV_CHECK_AT_NEXT_HALT or
             WWHOBD_SEV_CHECK_IMMEDIATELY;
var
  Hi: Byte;
begin
  Hi := AByte and HighBits;
  if Hi = 0 then
    Exit(wcUnknown);
  if (Hi and WWHOBD_SEV_CHECK_IMMEDIATELY) <> 0 then
  begin
    if (Hi and WWHOBD_SEV_CHECK_AT_NEXT_HALT) <> 0 then
      Exit(wcCheckBoth)
    else
      Exit(wcCheckNow);
  end;
  if (Hi and WWHOBD_SEV_CHECK_AT_NEXT_HALT) <> 0 then
    Exit(wcCheckHalt);
  Result := wcMaintenance;
end;

function TOBDWWHOBD.DecodeBigEndian(const AData: TBytes;
  AOffset, ABytes: Integer): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABytes - 1 do
    Result := (Result shl 8) or AData[AOffset + I];
end;

function DecodeWWHJ2012(AHi, ALo: Byte): string;
const
  Prefix: array[0..3] of Char = ('P','C','B','U');
var
  PrefixIdx: Integer;
begin
  PrefixIdx := (AHi shr 6) and $03;
  Result := Prefix[PrefixIdx] +
            IntToHex((AHi shr 4) and $03, 1) +
            IntToHex(AHi and $0F, 1) +
            IntToHex((ALo shr 4) and $0F, 1) +
            IntToHex(ALo and $0F, 1);
end;

function TOBDWWHOBD.DoReadBySeverity(AMask: Byte): TArray<TOBDWWHDtcEntry>;
var
  Resp: TOBDResponse;
  Req: TBytes;
  Off: Integer;
  Acc: TList<TOBDWWHDtcEntry>;
  Entry: TOBDWWHDtcEntry;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := UDS_DTC_REPORT_BY_SEVERITY_MASK;
  Req[1] := AMask;
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDTCBySeverity negative: %s', [Resp.NRCText]);

  // Response body per ISO 14229-1 §11.3.5.4:
  //   <subFunc> <DTCStatusAvailMask> [ <severityByte>
  //                                    <functionalUnit>
  //                                    <DTC hi> <DTC mid> <DTC lo>
  //                                    <statusOfDTC> ]*
  // We skip the subFunc + status-avail-mask preamble. Each record
  // is exactly 6 bytes.
  Acc := TList<TOBDWWHDtcEntry>.Create;
  try
    Off := 2;
    while Off + 6 <= Length(Resp.Data) do
    begin
      Entry := Default(TOBDWWHDtcEntry);
      Entry.SeverityByte   := Resp.Data[Off + 0];
      Entry.FunctionalUnit := Resp.Data[Off + 1];
      Entry.Code := DecodeWWHJ2012(Resp.Data[Off + 2], Resp.Data[Off + 3]);
      Entry.StatusByte := Resp.Data[Off + 5];
      Entry.DtcClass := ClassifySeverity(Entry.SeverityByte);
      Entry.Raw := Copy(Resp.Data, Off, 6);
      Acc.Add(Entry);
      Inc(Off, 6);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDWWHOBD.DoReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;
var
  Resp: TOBDResponse;
  Req: TBytes;
  Off: Integer;
  Acc: TList<TOBDWWHDtcEntry>;
  Entry: TOBDWWHDtcEntry;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := UDS_DTC_REPORT_WWHOBD_BY_GROUP;
  Req[1] := AGroupId;
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDTCByGroup(0x%.2x) negative: %s', [AGroupId, Resp.NRCText]);

  // Per ISO 14229-1 §11.3.5.13:
  //   <subFunc> <functionalGroupId> <severityAvailMask>
  //   <statusAvailMask> [ <DTC hi mid lo> <statusOfDTC>
  //                       <severityByte> ]*
  // Preamble = 4 bytes. Each record = 5 bytes.
  Acc := TList<TOBDWWHDtcEntry>.Create;
  try
    Off := 4;
    while Off + 5 <= Length(Resp.Data) do
    begin
      Entry := Default(TOBDWWHDtcEntry);
      Entry.Code := DecodeWWHJ2012(Resp.Data[Off + 0], Resp.Data[Off + 1]);
      Entry.StatusByte   := Resp.Data[Off + 3];
      Entry.SeverityByte := Resp.Data[Off + 4];
      Entry.FunctionalUnit := AGroupId;
      Entry.DtcClass := ClassifySeverity(Entry.SeverityByte);
      Entry.Raw := Copy(Resp.Data, Off, 5);
      Acc.Add(Entry);
      Inc(Off, 5);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDWWHOBD.DoReadDID(ADID: Word;
  AExpectedBytes: Integer): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  SetLength(Req, 2);
  Req[0] := Byte((ADID shr 8) and $FF);
  Req[1] := Byte(ADID and $FF);
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDID 0x%.4x negative: %s', [ADID, Resp.NRCText]);
  // Response: <DID-hi> <DID-lo> <data...>
  if Length(Resp.Data) < 2 + AExpectedBytes then
    raise EOBDProtocolErr.CreateFmt(
      'ReadDID 0x%.4x: short response (%d, wanted >= %d)',
      [ADID, Length(Resp.Data), 2 + AExpectedBytes]);
  Result := Copy(Resp.Data, 2, Length(Resp.Data) - 2);
end;

function TOBDWWHOBD.ReadBySeverity: TArray<TOBDWWHDtcEntry>;
begin
  Result := DoReadBySeverity(FSeverityMask);
  FireDTCs(Result);
end;

function TOBDWWHOBD.ReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;
begin
  Result := DoReadByGroup(AGroupId);
  FireDTCs(Result);
end;

procedure TOBDWWHOBD.ReadMILUsage(out ADistanceKm: UInt32;
  out ATimeMinutes: UInt32);
var
  Body: TBytes;
begin
  // DID 0xF402: 2 bytes (km), big-endian.
  Body := DoReadDID(WWHOBD_DID_DistanceMILOn, 2);
  ADistanceKm := DecodeBigEndian(Body, 0, 2);
  // DID 0xF407: 2 bytes (minutes), big-endian.
  Body := DoReadDID(WWHOBD_DID_TimeMILOn, 2);
  ATimeMinutes := DecodeBigEndian(Body, 0, 2);
  FireMILUsage(ADistanceKm, ATimeMinutes);
end;

procedure TOBDWWHOBD.ReadDTCCounters(out AClassA, AClassB1: UInt32);
var
  Body: TBytes;
begin
  // Each counter is reported as a 2-byte big-endian field per
  // ISO 27145-2 §6.1.
  Body := DoReadDID(WWHOBD_DID_DTCCounter, 2);
  AClassA := DecodeBigEndian(Body, 0, 2);
  Body := DoReadDID(WWHOBD_DID_DTCCounter2, 2);
  AClassB1 := DecodeBigEndian(Body, 0, 2);
end;

function TOBDWWHOBD.ReadVIN: string;
var
  Body: TBytes;
  I: Integer;
begin
  Body := DoReadDID(WWHOBD_DID_VIN, 17);
  // ISO 27145-2: ASCII string padded to 17 chars.
  SetLength(Result, Length(Body));
  for I := 0 to High(Body) do
    Result[I + 1] := Char(Body[I]);
  Result := Trim(Result);
end;

procedure TOBDWWHOBD.ReadBySeverityAsync;
var Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.ReadBySeverity
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally Self_.ReleaseAsync; end;
    end).Start;
end;

procedure TOBDWWHOBD.ReadByGroupAsync(AGroupId: Byte);
var Self_: TOBDWWHOBD; G: Byte;
begin
  GuardSingleAsync;
  Self_ := Self; G := AGroupId;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try Self_.ReadByGroup(G)
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally Self_.ReleaseAsync; end;
    end).Start;
end;

procedure TOBDWWHOBD.ReadMILUsageAsync;
var Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var D, T: UInt32;
    begin
      try
        try Self_.ReadMILUsage(D, T)
        except on E: Exception do Self_.FireError(oeIO, E.Message); end;
      finally Self_.ReleaseAsync; end;
    end).Start;
end;

procedure TOBDWWHOBD.FireDTCs(const AEntries: TArray<TOBDWWHDtcEntry>);
var
  Self_: TOBDWWHOBD;
  Snap: TArray<TOBDWWHDtcEntry>;
begin
  if not Assigned(FOnDTCs) then Exit;
  Self_ := Self;
  Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDTCs(Self_, Snap)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnDTCs) then Self_.FOnDTCs(Self_, Snap);
    end);
end;

procedure TOBDWWHOBD.FireMILUsage(ADistance, ATime: UInt32);
var
  Self_: TOBDWWHOBD; D, T: UInt32;
begin
  if not Assigned(FOnMILUsage) then Exit;
  Self_ := Self; D := ADistance; T := ATime;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnMILUsage(Self_, D, T)
  else
    TThread.Queue(nil, procedure begin
      if Assigned(Self_.FOnMILUsage) then Self_.FOnMILUsage(Self_, D, T);
    end);
end;

procedure TOBDWWHOBD.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDWWHOBD; Code: TOBDErrorCode; Msg: string;
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
