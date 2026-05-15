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
//    - VIN (DID 0xF190 — same as the generic TOBDVIN; we expose
//      it here too because HD techs frequently want the bundle
//      in one call)
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
  UDS_DTC_REPORT_WWHOBD_BY_GROUP = $55;

  /// <summary>"Check at next halt" severity bit per ISO 14229-1
  /// §11.3.5.4.</summary>
  WWHOBD_SEV_CHECK_AT_NEXT_HALT = $20;
  /// <summary>"Check immediately" severity bit (Class A escalation).
  /// </summary>
  WWHOBD_SEV_CHECK_IMMEDIATELY = $40;
  /// <summary>"Maintenance only" severity bit (no MIL).</summary>
  WWHOBD_SEV_MAINTENANCE_ONLY = $80;

  /// <summary>UDS DID — distance travelled with MIL on (km).</summary>
  WWHOBD_DID_DistanceMILOn      = $F402;
  /// <summary>UDS DID — time run with MIL on (minutes).</summary>
  WWHOBD_DID_TimeMILOn          = $F407;
  /// <summary>UDS DID — major group readiness summary.</summary>
  WWHOBD_DID_MajorGroupReady    = $F411;
  /// <summary>UDS DID — readiness-group-specific status.</summary>
  WWHOBD_DID_ReadinessGroupStat = $F412;
  /// <summary>UDS DID — monitoring conditions encountered
  /// counter.</summary>
  WWHOBD_DID_OBDMonCondCount    = $F40C;
  /// <summary>UDS DID — DTC counter (class A by default; varies
  /// per OEM).</summary>
  WWHOBD_DID_DTCCounter         = $F418;
  /// <summary>UDS DID — second DTC counter byte (e.g. Class B1
  /// pending).</summary>
  WWHOBD_DID_DTCCounter2        = $F419;
  /// <summary>UDS DID — VIN (ISO 27145-2 alias of the standard
  /// VIN DID).</summary>
  WWHOBD_DID_VIN                = $F190;

type
  /// <summary>
  ///   WWH-OBD DTC severity class.
  /// </summary>
  /// <remarks>
  ///   Maps the high three bits of the severity byte returned by
  ///   sub-function 0x42 to a coarser human-readable label.
  /// </remarks>
  TOBDWWHDtcClass = (
    /// <summary>Severity byte = 0 — class not reported.</summary>
    wcUnknown,
    /// <summary>"Maintenance only" bit set — no MIL impact.</summary>
    wcMaintenance,
    /// <summary>"Check at next halt" only — Class B.</summary>
    wcCheckHalt,
    /// <summary>"Check immediately" only — Class A.</summary>
    wcCheckNow,
    /// <summary>Both "check halt" and "check now" set — Class A
    /// escalation.</summary>
    wcCheckBoth
  );

  /// <summary>
  ///   One WWH-OBD DTC entry.
  /// </summary>
  TOBDWWHDtcEntry = record
    /// <summary>5-character J2012 DTC code (e.g. P0420).</summary>
    Code: string;
    /// <summary>Severity byte verbatim.</summary>
    SeverityByte: Byte;
    /// <summary>UDS status mask (testFailed bits).</summary>
    StatusByte: Byte;
    /// <summary>Decoded severity class.</summary>
    DtcClass: TOBDWWHDtcClass;
    /// <summary>Functional-unit byte that follows the severity
    /// byte in sub 0x42 records.</summary>
    FunctionalUnit: Byte;
    /// <summary>Raw bytes for this record exactly as returned by
    /// the ECU.</summary>
    Raw: TBytes;
  end;

  /// <summary>Fires after a successful DTC sweep. Main thread.</summary>
  TOBDWWHDtcsEvent = procedure(Sender: TObject;
    const AEntries: TArray<TOBDWWHDtcEntry>) of object;

  /// <summary>Fires after MIL distance / time read. Main thread.</summary>
  TOBDWWHMILUsageEvent = procedure(Sender: TObject;
    ADistanceKm: UInt32; ATimeMinutes: UInt32) of object;

  /// <summary>
  ///   WWH-OBD heavy-duty diagnostic component.
  /// </summary>
  /// <remarks>
  ///   Drop the component on a form and assign <c>Protocol</c> to a
  ///   connected, UDS-capable <see cref="TOBDProtocol"/>.
  ///   <see cref="ReadBySeverity"/> enumerates every DTC whose
  ///   severity byte intersects <see cref="SeverityMask"/>;
  ///   <see cref="ReadByGroup"/> walks a single ISO 27145-3
  ///   readiness-group ID. The DID-driven helpers
  ///   (<see cref="ReadMILUsage"/>, <see cref="ReadDTCCounters"/>,
  ///   <see cref="ReadVIN"/>) wrap a single UDS ReadDataByIdentifier
  ///   per call.
  /// </remarks>
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
    function DecodeBigEndian(const AData: TBytes; AOffset: Integer;
      ABytes: Integer): UInt32;
    procedure FireDTCs(const AEntries: TArray<TOBDWWHDtcEntry>);
    procedure FireMILUsage(ADistance: UInt32; ATime: UInt32);
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
    ///   Reads every DTC whose severity byte intersects
    ///   <see cref="SeverityMask"/>.
    /// </summary>
    /// <returns>Decoded DTC entries in wire order.</returns>
    /// <remarks>Blocks. From GUI code prefer
    /// <see cref="ReadBySeverityAsync"/>.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function ReadBySeverity: TArray<TOBDWWHDtcEntry>;

    /// <summary>
    ///   Reads every WWH-OBD DTC for readiness group
    ///   <c>AGroupId</c>.
    /// </summary>
    /// <param name="AGroupId">ISO 27145-3 functional-group ID.</param>
    /// <returns>Decoded DTC entries.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative response.
    /// </exception>
    function ReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;

    /// <summary>
    ///   Reads the MIL-on usage counters.
    /// </summary>
    /// <param name="ADistanceKm">Out: kilometres driven with MIL
    /// illuminated (DID 0xF402).</param>
    /// <param name="ATimeMinutes">Out: minutes run with MIL
    /// illuminated (DID 0xF407).</param>
    /// <remarks>Fires <c>OnMILUsage</c> on completion.</remarks>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   One of the DID reads returned a negative or short
    ///   response.
    /// </exception>
    procedure ReadMILUsage(out ADistanceKm: UInt32;
      out ATimeMinutes: UInt32);

    /// <summary>
    ///   Reads the Class A and Class B1 DTC counters.
    /// </summary>
    /// <param name="AClassA">Out: Class A counter (DID
    /// 0xF418).</param>
    /// <param name="AClassB1">Out: Class B1 counter (DID
    /// 0xF419).</param>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   One of the DID reads returned a negative or short
    ///   response.
    /// </exception>
    procedure ReadDTCCounters(out AClassA: UInt32;
      out AClassB1: UInt32);

    /// <summary>
    ///   Reads the VIN via DID 0xF190.
    /// </summary>
    /// <returns>The VIN as ASCII; trimmed of trailing padding.</returns>
    /// <exception cref="EOBDConfig">
    ///   <c>Protocol</c> is not assigned.
    /// </exception>
    /// <exception cref="EOBDProtocolErr">
    ///   ECU returned a negative or short response.
    /// </exception>
    function ReadVIN: string;

    /// <summary>
    ///   Maps a severity byte to a <see cref="TOBDWWHDtcClass"/>.
    /// </summary>
    /// <param name="AByte">Severity byte from a sub-0x42 entry.</param>
    /// <returns>Coarse severity class.</returns>
    class function ClassifySeverity(AByte: Byte): TOBDWWHDtcClass; static;

    /// <summary>Non-blocking <see cref="ReadBySeverity"/>.</summary>
    /// <remarks>
    ///   Spawns a worker thread; reports completion via
    ///   <c>OnDTCs</c> or failure via <c>OnError</c> on the main
    ///   thread. Only one async read may be in flight at a time.
    /// </remarks>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadBySeverityAsync;

    /// <summary>Non-blocking <see cref="ReadByGroup"/>.</summary>
    /// <param name="AGroupId">ISO 27145-3 functional-group ID.</param>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadByGroupAsync(AGroupId: Byte);

    /// <summary>Non-blocking <see cref="ReadMILUsage"/>.</summary>
    /// <exception cref="EOBDConfig">
    ///   Another async read is already in flight.
    /// </exception>
    procedure ReadMILUsageAsync;
  published
    /// <summary>Protocol stack to read through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>
    ///   Default severity-mask filter applied to
    ///   <see cref="ReadBySeverity"/>. Default <c>0xE0</c> (every
    ///   MIL-eligible severity bit).
    /// </summary>
    property SeverityMask: Byte read FSeverityMask write FSeverityMask
      default WWHOBD_SEV_MAINTENANCE_ONLY or WWHOBD_SEV_CHECK_AT_NEXT_HALT
            or WWHOBD_SEV_CHECK_IMMEDIATELY;

    /// <summary>Fires on DTC-sweep completion. Main thread.</summary>
    property OnDTCs: TOBDWWHDtcsEvent read FOnDTCs write FOnDTCs;

    /// <summary>Fires after a MIL-usage read. Main thread.</summary>
    property OnMILUsage: TOBDWWHMILUsageEvent read FOnMILUsage
      write FOnMILUsage;

    /// <summary>Fires on transient I/O errors. Main thread.</summary>
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
  if FProtocol = AValue then
    Exit;
  if FProtocol <> nil then
    FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then
    FProtocol.FreeNotification(Self);
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
  finally
    FAsyncLock.Leave;
  end;
end;

procedure TOBDWWHOBD.ReleaseAsync;
begin
  FAsyncLock.Enter;
  try
    FAsyncInFlight := False;
  finally
    FAsyncLock.Leave;
  end;
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
  AOffset: Integer; ABytes: Integer): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABytes - 1 do
    Result := (Result shl 8) or AData[AOffset + I];
end;

function DecodeWWHJ2012(AHi: Byte; ALo: Byte): string;
const
  Prefix: array[0..3] of Char = ('P', 'C', 'B', 'U');
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

  // Response per ISO 14229-1 §11.3.5.4:
  //   <subFunc> <DTCStatusAvailMask> [ <severityByte>
  //                                    <functionalUnit>
  //                                    <DTC hi> <DTC mid> <DTC lo>
  //                                    <statusOfDTC> ]*
  // Skip the subFunc + status-avail-mask preamble (2 bytes); each
  // record is exactly 6 bytes long.
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
  // Preamble = 4 bytes; each record is 5 bytes.
  Acc := TList<TOBDWWHDtcEntry>.Create;
  try
    Off := 4;
    while Off + 5 <= Length(Resp.Data) do
    begin
      Entry := Default(TOBDWWHDtcEntry);
      Entry.Code := DecodeWWHJ2012(Resp.Data[Off + 0], Resp.Data[Off + 1]);
      Entry.StatusByte := Resp.Data[Off + 3];
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
  // Response is <DID-hi> <DID-lo> <data...>.
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

procedure TOBDWWHOBD.ReadDTCCounters(out AClassA: UInt32;
  out AClassB1: UInt32);
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
  // ISO 27145-2: ASCII padded to 17 chars.
  SetLength(Result, Length(Body));
  for I := 0 to High(Body) do
    Result[I + 1] := Char(Body[I]);
  Result := Trim(Result);
end;

procedure TOBDWWHOBD.ReadBySeverityAsync;
var
  Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadBySeverity;
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDWWHOBD.ReadByGroupAsync(AGroupId: Byte);
var
  Self_: TOBDWWHOBD;
  GID: Byte;
begin
  GuardSingleAsync;
  Self_ := Self;
  GID := AGroupId;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Self_.ReadByGroup(GID);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDWWHOBD.ReadMILUsageAsync;
var
  Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  Self_ := Self;
  TThread.CreateAnonymousThread(
    procedure
    var
      Distance: UInt32;
      Time: UInt32;
    begin
      try
        try
          Self_.ReadMILUsage(Distance, Time);
        except
          on E: Exception do
            Self_.FireError(oeIO, E.Message);
        end;
      finally
        Self_.ReleaseAsync;
      end;
    end).Start;
end;

procedure TOBDWWHOBD.FireDTCs(const AEntries: TArray<TOBDWWHDtcEntry>);
var
  Self_: TOBDWWHOBD;
  Snap: TArray<TOBDWWHDtcEntry>;
begin
  if not Assigned(FOnDTCs) then
    Exit;
  Self_ := Self;
  Snap := Copy(AEntries, 0, Length(AEntries));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnDTCs(Self_, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnDTCs) then
          Self_.FOnDTCs(Self_, Snap);
      end);
end;

procedure TOBDWWHOBD.FireMILUsage(ADistance: UInt32; ATime: UInt32);
var
  Self_: TOBDWWHOBD;
  Distance: UInt32;
  Time: UInt32;
begin
  if not Assigned(FOnMILUsage) then
    Exit;
  Self_ := Self;
  Distance := ADistance;
  Time := ATime;
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnMILUsage(Self_, Distance, Time)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnMILUsage) then
          Self_.FOnMILUsage(Self_, Distance, Time);
      end);
end;

procedure TOBDWWHOBD.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
var
  Self_: TOBDWWHOBD;
  Code: TOBDErrorCode;
  Msg: string;
  Handled: Boolean;
begin
  if not Assigned(FOnError) then
    Exit;
  Self_ := Self;
  Code := ACode;
  Msg := AMessage;
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
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
