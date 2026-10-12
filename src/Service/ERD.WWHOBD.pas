// ------------------------------------------------------------------------------
// ERD.WWHOBD
//
// TOBDWWHOBD - World-Wide Harmonised OBD / SAE J1979-2 diagnostic
// surface over UDS. WWH-OBD uses ISO 14229-1 ReadDTCInformation
// sub-functions and J1979-2 data identifiers for OBD-style data.
//
// This component covers:
// - Read WWH-OBD DTCs by mask record (0x19 sub 0x42)
// - Read permanent WWH-OBD DTCs (0x19 sub 0x55)
// - Read DTCs for a readiness-group identifier (0x19 sub 0x56)
// - Distance / time-with-MIL using J1979-2 DIDs F421 / F44D
// - VIN using J1979-2 DID F802 with UDS F190 fallback
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - ISO 27145-1:2012 - General requirements for WWH-OBD
// - ISO 27145-3:2012 - Common message dictionary
// - ISO 14229-1:2020 section 11.3 (ReadDTCInformation)
// - SAE J1979-2 - UDS data identifiers for OBD information
//
// History     :
// 2026-05-11  ERD  Initial implementation.
// 2026-10-12  ERD  Correct J1979-2 DIDs and WWH-OBD DTC parsing.
// ------------------------------------------------------------------------------

unit ERD.WWHOBD;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.Async.Task,
  ERD.Connection,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  ERD.Types,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Protocol,
  ERD.Service.DTCs;

const
  /// <summary>UDS 0x19 sub-function 0x42 - reportWWHOBDDTCByMaskRecord.</summary>
  UDS_DTC_REPORT_WWHOBD_BY_MASK_RECORD = $42;
  /// <summary>Compatibility alias for sub-function 0x42.</summary>
  UDS_DTC_REPORT_BY_SEVERITY_MASK = UDS_DTC_REPORT_WWHOBD_BY_MASK_RECORD;

  /// <summary>UDS 0x19 sub-function 0x55 - reportWWHOBDDTCWithPermanentStatus.</summary>
  UDS_DTC_REPORT_WWHOBD_WITH_PERMANENT_STATUS = $55;
  /// <summary>UDS 0x19 sub-function 0x56 - reportDTCInformationByDTCReadinessGroupIdentifier.</summary>
  UDS_DTC_REPORT_DTC_BY_READINESS_GROUP = $56;
  /// <summary>Compatibility alias for the corrected readiness-group sub-function.</summary>
  UDS_DTC_REPORT_WWHOBD_BY_GROUP = UDS_DTC_REPORT_DTC_BY_READINESS_GROUP;

  /// <summary>WWH-OBD functional group identifier for emissions-related systems.</summary>
  WWHOBD_FUNCTIONAL_GROUP_EMISSIONS = $33;
  /// <summary>Default DTC status mask requesting every status bit.</summary>
  WWHOBD_DTC_STATUS_MASK_ALL = $FF;

  /// <summary>DTCSeverityMask bit 5: maintenanceOnly.</summary>
  WWHOBD_SEV_MAINTENANCE_ONLY = $20;
  /// <summary>DTCSeverityMask bit 6: checkAtNextHalt.</summary>
  WWHOBD_SEV_CHECK_AT_NEXT_HALT = $40;
  /// <summary>DTCSeverityMask bit 7: checkImmediately.</summary>
  WWHOBD_SEV_CHECK_IMMEDIATELY = $80;
  /// <summary>Mask for the ISO 14229-1 DTC severity bits.</summary>
  WWHOBD_SEV_MASK = WWHOBD_SEV_MAINTENANCE_ONLY or
    WWHOBD_SEV_CHECK_AT_NEXT_HALT or WWHOBD_SEV_CHECK_IMMEDIATELY;

  /// <summary>DTC class bit 0: class 0 / no WWH emission class.</summary>
  WWHOBD_DTC_CLASS_0 = $01;
  /// <summary>DTC class bit 1: WWH-OBD Class A.</summary>
  WWHOBD_DTC_CLASS_A = $02;
  /// <summary>DTC class bit 2: WWH-OBD Class B1.</summary>
  WWHOBD_DTC_CLASS_B1 = $04;
  /// <summary>DTC class bit 3: WWH-OBD Class B2.</summary>
  WWHOBD_DTC_CLASS_B2 = $08;
  /// <summary>DTC class bit 4: WWH-OBD Class C.</summary>
  WWHOBD_DTC_CLASS_C = $10;
  /// <summary>Mask for the ISO 14229-1 DTC class bits.</summary>
  WWHOBD_DTC_CLASS_MASK = WWHOBD_DTC_CLASS_0 or WWHOBD_DTC_CLASS_A or
    WWHOBD_DTC_CLASS_B1 or WWHOBD_DTC_CLASS_B2 or WWHOBD_DTC_CLASS_C;

  /// <summary>J1979-2 DID F401 - monitor status since DTCs cleared (PID 01 payload).</summary>
  WWHOBD_DID_MonitorStatusSinceClear = $F401;
  /// <summary>J1979-2 DID F421 - distance travelled with MIL on (PID 21 payload).</summary>
  WWHOBD_DID_DistanceMILOn = $F421;
  /// <summary>J1979-2 DID F430 - warm-ups since DTCs cleared (PID 30 payload).</summary>
  WWHOBD_DID_WarmUpsSinceClear = $F430;
  /// <summary>J1979-2 DID F431 - distance since DTCs cleared (PID 31 payload).</summary>
  WWHOBD_DID_DistanceSinceClear = $F431;
  /// <summary>J1979-2 DID F441 - monitor status this drive cycle (PID 41 payload).</summary>
  WWHOBD_DID_MonitorStatusThisCycle = $F441;
  /// <summary>J1979-2 DID F44D - engine run time with MIL on (PID 4D payload).</summary>
  WWHOBD_DID_TimeMILOn = $F44D;
  /// <summary>J1979-2 DID F44E - time since DTCs cleared (PID 4E payload).</summary>
  WWHOBD_DID_TimeSinceClear = $F44E;
  /// <summary>J1979-2 DID F802 - VIN (Mode 09 InfoType 02 payload).</summary>
  WWHOBD_DID_VIN = $F802;
  /// <summary>UDS DID F190 - VIN fallback for vehicles that omit F802.</summary>
  WWHOBD_DID_UDS_VIN = $F190;
  /// <summary>J1979-2 DID F804 - calibration identification.</summary>
  WWHOBD_DID_CALID = $F804;
  /// <summary>J1979-2 DID F806 - calibration verification number.</summary>
  WWHOBD_DID_CVN = $F806;
  /// <summary>J1979-2 DID F808 - in-use performance tracking, spark ignition.</summary>
  WWHOBD_DID_IPT_Spark = $F808;
  /// <summary>J1979-2 DID F80A - ECU name.</summary>
  WWHOBD_DID_ECUName = $F80A;
  /// <summary>J1979-2 DID F80B - in-use performance tracking, compression ignition.</summary>
  WWHOBD_DID_IPT_Compression = $F80B;

  /// <summary>Compatibility alias: use WWHOBD_DID_MonitorStatusSinceClear.</summary>
  WWHOBD_DID_MajorGroupReady = WWHOBD_DID_MonitorStatusSinceClear;
  /// <summary>Compatibility alias: use WWHOBD_DID_MonitorStatusThisCycle.</summary>
  WWHOBD_DID_ReadinessGroupStat = WWHOBD_DID_MonitorStatusThisCycle;

type
  /// <summary>
  /// WWH-OBD DTC severity or class label.
  /// </summary>
  /// <remarks>
  /// The first five values are the legacy severity labels returned by
  /// <see cref="ClassifySeverity"/>. The appended values represent the
  /// DTC class bits carried in severity-byte bits 0..4.
  /// </remarks>
  TOBDWWHDtcClass = (
    /// <summary>No severity or class bit was reported.</summary>
    wcUnknown,
    /// <summary>maintenanceOnly severity bit set.</summary>
    wcMaintenance,
    /// <summary>checkAtNextHalt severity bit set.</summary>
    wcCheckHalt,
    /// <summary>checkImmediately severity bit set.</summary>
    wcCheckNow,
    /// <summary>Both checkAtNextHalt and checkImmediately bits set.</summary>
    wcCheckBoth,
    /// <summary>DTCClass_0 bit set.</summary>
    wcClass0,
    /// <summary>WWH-OBD DTC Class A.</summary>
    wcClassA,
    /// <summary>WWH-OBD DTC Class B1.</summary>
    wcClassB1,
    /// <summary>WWH-OBD DTC Class B2.</summary>
    wcClassB2,
    /// <summary>WWH-OBD DTC Class C.</summary>
    wcClassC);

  /// <summary>
  /// One WWH-OBD DTC entry.
  /// </summary>
  TOBDWWHDtcEntry = record
    /// <summary>UDS/J1979-2 DTC string including failure-type suffix, e.g. P0101-1C.</summary>
    Code: string;
    /// <summary>Plain two-byte SAE J2012 DTC string, e.g. P0101.</summary>
    PlainCode: string;
    /// <summary>Failure type byte, the third byte of the UDS DTC.</summary>
    FailureTypeByte: Byte;
    /// <summary>Severity byte verbatim; zero for sub-functions that do not return it.</summary>
    SeverityByte: Byte;
    /// <summary>UDS status mask (statusOfDTC).</summary>
    StatusByte: Byte;
    /// <summary>Legacy coarse severity classification from bits 7..5.</summary>
    DtcClass: TOBDWWHDtcClass;
    /// <summary>WWH-OBD DTC class decoded from bits 4..0.</summary>
    WWHClass: TOBDWWHDtcClass;
    /// <summary>Functional group identifier from the response preamble.</summary>
    FunctionalGroupIdentifier: Byte;
    /// <summary>Compatibility alias of FunctionalGroupIdentifier.</summary>
    FunctionalUnit: Byte;
    /// <summary>Readiness group identifier for sub-function 0x56 responses.</summary>
    ReadinessGroupIdentifier: Byte;
    /// <summary>DTCFormatIdentifier from the response preamble.</summary>
    DTCFormatIdentifier: Byte;
    /// <summary>Header of the ECU that reported the code.</summary>
    Ecu: string;
    /// <summary>Numeric CAN identifier of the reporting ECU, or zero when unknown.</summary>
    EcuId: Cardinal;
    /// <summary>Raw record bytes exactly as returned by the ECU.</summary>
    Raw: TBytes;
  end;

  /// <summary>Fires after a successful DTC sweep. Main thread.</summary>
  TOBDWWHDtcsEvent = procedure(Sender: TObject;
    const AEntries: TArray<TOBDWWHDtcEntry>) of object;

  /// <summary>Fires after MIL distance / time read. Main thread.</summary>
  TOBDWWHMILUsageEvent = procedure(Sender: TObject; ADistanceKm: UInt32;
    ATimeMinutes: UInt32) of object;

  /// <summary>
  /// WWH-OBD heavy-duty diagnostic component.
  /// </summary>
  TOBDWWHOBD = class(TComponent)
  strict private
    FProtocol: TOBDProtocol;
    FSeverityMask: Byte;
    FStatusMask: Byte;
    FFunctionalGroupIdentifier: Byte;
    FOwnedTask: TOBDOwnedTask;
    FAsyncLock: TCriticalSection;
    FAsyncInFlight: Boolean;
    FOnDTCs: TOBDWWHDtcsEvent;
    FOnMILUsage: TOBDWWHMILUsageEvent;
    FOnError: TOBDConnectionErrorEvent;
    procedure GuardSingleAsync;
    procedure ReleaseAsync;
    function DoReadBySeverity(AStatusMask, ASeverityMask: Byte)
      : TArray<TOBDWWHDtcEntry>;
    function DoReadPermanent: TArray<TOBDWWHDtcEntry>;
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
    /// Reads WWH-OBD DTCs with 0x19 sub-function 0x42.
    /// </summary>
    /// <returns>Decoded DTC entries in wire order, across all answering ECUs.</returns>
    function ReadBySeverity: TArray<TOBDWWHDtcEntry>;

    /// <summary>
    /// Reads WWH-OBD DTCs with permanent status using 0x19 sub-function 0x55.
    /// </summary>
    /// <returns>Decoded permanent DTC entries.</returns>
    function ReadPermanent: TArray<TOBDWWHDtcEntry>;

    /// <summary>
    /// Reads WWH-OBD DTCs for a DTC readiness-group identifier using
    /// 0x19 sub-function 0x56.
    /// </summary>
    /// <param name="AGroupId">DTCReadinessGroupIdentifier.</param>
    /// <returns>Decoded DTC entries.</returns>
    function ReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;

    /// <summary>
    /// Reads the MIL-on usage counters from J1979-2 DIDs F421 and F44D.
    /// </summary>
    /// <param name="ADistanceKm">Out: kilometres driven with MIL illuminated.</param>
    /// <param name="ATimeMinutes">Out: minutes run with MIL illuminated.</param>
    procedure ReadMILUsage(out ADistanceKm: UInt32; out ATimeMinutes: UInt32);

    /// <summary>
    /// Counts Class A and Class B1 DTCs from the 0x42 WWH-OBD DTC list.
    /// </summary>
    /// <param name="AClassA">Out: number of Class A DTCs.</param>
    /// <param name="AClassB1">Out: number of Class B1 DTCs.</param>
    procedure ReadDTCCounters(out AClassA: UInt32; out AClassB1: UInt32);

    /// <summary>
    /// Reads the VIN via J1979-2 DID F802, falling back to UDS DID F190.
    /// </summary>
    /// <returns>The VIN as ASCII; trimmed of trailing padding.</returns>
    function ReadVIN: string;

    /// <summary>Maps severity byte bits 7..5 to a coarse severity label.</summary>
    class function ClassifySeverity(AByte: Byte): TOBDWWHDtcClass; static;
    /// <summary>Maps severity byte bits 4..0 to the WWH-OBD DTC class.</summary>
    class function ClassifyWWHClass(AByte: Byte): TOBDWWHDtcClass; static;
    /// <summary>Formats the two-byte SAE J2012 portion of a WWH-OBD DTC.</summary>
    class function PlainDTCCode(AHi, AMid: Byte): string; static;
    /// <summary>Formats a three-byte UDS DTC as P0101-1C.</summary>
    class function FormatDTC(AHi, AMid, AFailureType: Byte): string; static;

    /// <summary>Parses all positive 0x59/0x42 ECU answers.</summary>
    class function ParseBySeverityResponse(const AResponse: TOBDResponse)
      : TArray<TOBDWWHDtcEntry>; static;
    /// <summary>Parses all positive 0x59/0x55 ECU answers.</summary>
    class function ParsePermanentResponse(const AResponse: TOBDResponse)
      : TArray<TOBDWWHDtcEntry>; static;
    /// <summary>Parses all positive 0x59/0x56 ECU answers.</summary>
    class function ParseReadinessGroupResponse(const AResponse: TOBDResponse)
      : TArray<TOBDWWHDtcEntry>; static;

    /// <summary>Non-blocking <see cref="ReadBySeverity"/>.</summary>
    procedure ReadBySeverityAsync;
    /// <summary>Non-blocking <see cref="ReadPermanent"/>.</summary>
    procedure ReadPermanentAsync;
    /// <summary>Non-blocking <see cref="ReadByGroup"/>.</summary>
    /// <param name="AGroupId">DTCReadinessGroupIdentifier.</param>
    procedure ReadByGroupAsync(AGroupId: Byte);
    /// <summary>Non-blocking <see cref="ReadMILUsage"/>.</summary>
    procedure ReadMILUsageAsync;
  published
    /// <summary>Protocol stack to read through. Required.</summary>
    property Protocol: TOBDProtocol read FProtocol write SetProtocol;

    /// <summary>Functional group identifier. Default 0x33 for emissions systems.</summary>
    property FunctionalGroupIdentifier: Byte read FFunctionalGroupIdentifier
      write FFunctionalGroupIdentifier default WWHOBD_FUNCTIONAL_GROUP_EMISSIONS;

    /// <summary>Status-mask filter used by 0x19/0x42. Default 0xFF.</summary>
    property StatusMask: Byte read FStatusMask write FStatusMask
      default WWHOBD_DTC_STATUS_MASK_ALL;

    /// <summary>Severity-mask filter used by 0x19/0x42. Default 0xE0.</summary>
    property SeverityMask: Byte read FSeverityMask write FSeverityMask
      default WWHOBD_SEV_MASK;

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
  FOwnedTask := TOBDOwnedTask.Create;
  FSeverityMask := WWHOBD_SEV_MASK;
  FStatusMask := WWHOBD_DTC_STATUS_MASK_ALL;
  FFunctionalGroupIdentifier := WWHOBD_FUNCTIONAL_GROUP_EMISSIONS;
end;

destructor TOBDWWHOBD.Destroy;
begin
  if FOwnedTask <> nil then
    FOwnedTask.Cancel;
  FreeAndNil(FOwnedTask);
  FAsyncLock.Free;
  inherited;
end;

procedure TOBDWWHOBD.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then
    Exit;
  if FOwnedTask <> nil then
    FOwnedTask.Quiesce;
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
  begin
    if FOwnedTask <> nil then
      FOwnedTask.Quiesce;
    FProtocol := nil;
  end;
end;

procedure TOBDWWHOBD.GuardSingleAsync;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EOBDConfig.Create('Async start requires the main thread');
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
var
  Hi: Byte;
begin
  Hi := AByte and WWHOBD_SEV_MASK;
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

class function TOBDWWHOBD.ClassifyWWHClass(AByte: Byte): TOBDWWHDtcClass;
var
  Lo: Byte;
begin
  Lo := AByte and WWHOBD_DTC_CLASS_MASK;
  if (Lo and WWHOBD_DTC_CLASS_A) <> 0 then
    Exit(wcClassA);
  if (Lo and WWHOBD_DTC_CLASS_B1) <> 0 then
    Exit(wcClassB1);
  if (Lo and WWHOBD_DTC_CLASS_B2) <> 0 then
    Exit(wcClassB2);
  if (Lo and WWHOBD_DTC_CLASS_C) <> 0 then
    Exit(wcClassC);
  if (Lo and WWHOBD_DTC_CLASS_0) <> 0 then
    Exit(wcClass0);
  Result := wcUnknown;
end;

class function TOBDWWHOBD.PlainDTCCode(AHi, AMid: Byte): string;
begin
  Result := TOBDDTCs.DecodeJ2012(AHi, AMid);
end;

class function TOBDWWHOBD.FormatDTC(AHi, AMid, AFailureType: Byte): string;
begin
  Result := PlainDTCCode(AHi, AMid) + '-' + IntToHex(AFailureType, 2);
end;

function TOBDWWHOBD.DecodeBigEndian(const AData: TBytes; AOffset: Integer;
  ABytes: Integer): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABytes - 1 do
    Result := (Result shl 8) or AData[AOffset + I];
end;

procedure AppendWWHFromEcu(var AAcc: TArray<TOBDWWHDtcEntry>;
  const APart: TArray<TOBDWWHDtcEntry>; const AMsg: TOBDEcuMessage);
var
  I, Base: Integer;
begin
  Base := Length(AAcc);
  SetLength(AAcc, Base + Length(APart));
  for I := 0 to High(APart) do
  begin
    AAcc[Base + I] := APart[I];
    AAcc[Base + I].Ecu := AMsg.Header;
    AAcc[Base + I].EcuId := AMsg.CanId;
  end;
end;

function ParseSeverityPayload(const AData: TBytes): TArray<TOBDWWHDtcEntry>;
var
  Off, Count: Integer;
  FGID, FormatId: Byte;
  Entry: TOBDWWHDtcEntry;
begin
  Result := nil;
  if (Length(AData) < 5) or (AData[0] <> UDS_DTC_REPORT_WWHOBD_BY_MASK_RECORD) then
    Exit;
  FGID := AData[1];
  FormatId := AData[4];
  Off := 5;
  Count := 0;
  while Off + 5 <= Length(AData) do
  begin
    Entry := Default (TOBDWWHDtcEntry);
    Entry.SeverityByte := AData[Off];
    Entry.PlainCode := TOBDWWHOBD.PlainDTCCode(AData[Off + 1], AData[Off + 2]);
    Entry.FailureTypeByte := AData[Off + 3];
    Entry.Code := TOBDWWHOBD.FormatDTC(AData[Off + 1], AData[Off + 2],
      AData[Off + 3]);
    Entry.StatusByte := AData[Off + 4];
    Entry.DtcClass := TOBDWWHOBD.ClassifySeverity(Entry.SeverityByte);
    Entry.WWHClass := TOBDWWHOBD.ClassifyWWHClass(Entry.SeverityByte);
    Entry.FunctionalGroupIdentifier := FGID;
    Entry.FunctionalUnit := FGID;
    Entry.DTCFormatIdentifier := FormatId;
    Entry.Raw := Copy(AData, Off, 5);
    SetLength(Result, Count + 1);
    Result[Count] := Entry;
    Inc(Count);
    Inc(Off, 5);
  end;
end;

function ParseFourByteDtcPayload(const AData: TBytes; ASubFunction: Byte;
  AHasReadinessGroup: Boolean): TArray<TOBDWWHDtcEntry>;
var
  Off, Count: Integer;
  FGID, FormatId, GroupId: Byte;
  Entry: TOBDWWHDtcEntry;
begin
  Result := nil;
  if (Length(AData) < 4) or (AData[0] <> ASubFunction) then
    Exit;
  FGID := AData[1];
  FormatId := AData[3];
  GroupId := 0;
  if AHasReadinessGroup then
  begin
    if Length(AData) < 5 then
      Exit;
    GroupId := AData[4];
    Off := 5;
  end
  else
    Off := 4;
  Count := 0;
  while Off + 4 <= Length(AData) do
  begin
    Entry := Default (TOBDWWHDtcEntry);
    Entry.PlainCode := TOBDWWHOBD.PlainDTCCode(AData[Off], AData[Off + 1]);
    Entry.FailureTypeByte := AData[Off + 2];
    Entry.Code := TOBDWWHOBD.FormatDTC(AData[Off], AData[Off + 1],
      AData[Off + 2]);
    Entry.StatusByte := AData[Off + 3];
    Entry.DtcClass := wcUnknown;
    Entry.WWHClass := wcUnknown;
    Entry.FunctionalGroupIdentifier := FGID;
    Entry.FunctionalUnit := FGID;
    Entry.ReadinessGroupIdentifier := GroupId;
    Entry.DTCFormatIdentifier := FormatId;
    Entry.Raw := Copy(AData, Off, 4);
    SetLength(Result, Count + 1);
    Result[Count] := Entry;
    Inc(Count);
    Inc(Off, 4);
  end;
end;

class function TOBDWWHOBD.ParseBySeverityResponse(const AResponse: TOBDResponse)
  : TArray<TOBDWWHDtcEntry>;
var
  Msgs: TArray<TOBDEcuMessage>;
  Part: TArray<TOBDWWHDtcEntry>;
  I: Integer;
begin
  Result := nil;
  Msgs := OBDPositiveMessages(AResponse, UDS_SID_ReadDTCInformation +
    UDS_POSITIVE_RESPONSE_OFFSET);
  for I := 0 to High(Msgs) do
  begin
    Part := ParseSeverityPayload(Copy(Msgs[I].Data, 1, MaxInt));
    AppendWWHFromEcu(Result, Part, Msgs[I]);
  end;
end;

class function TOBDWWHOBD.ParsePermanentResponse(const AResponse: TOBDResponse)
  : TArray<TOBDWWHDtcEntry>;
var
  Msgs: TArray<TOBDEcuMessage>;
  Part: TArray<TOBDWWHDtcEntry>;
  I: Integer;
begin
  Result := nil;
  Msgs := OBDPositiveMessages(AResponse, UDS_SID_ReadDTCInformation +
    UDS_POSITIVE_RESPONSE_OFFSET);
  for I := 0 to High(Msgs) do
  begin
    Part := ParseFourByteDtcPayload(Copy(Msgs[I].Data, 1, MaxInt),
      UDS_DTC_REPORT_WWHOBD_WITH_PERMANENT_STATUS, False);
    AppendWWHFromEcu(Result, Part, Msgs[I]);
  end;
end;

class function TOBDWWHOBD.ParseReadinessGroupResponse(
  const AResponse: TOBDResponse): TArray<TOBDWWHDtcEntry>;
var
  Msgs: TArray<TOBDEcuMessage>;
  Part: TArray<TOBDWWHDtcEntry>;
  I: Integer;
begin
  Result := nil;
  Msgs := OBDPositiveMessages(AResponse, UDS_SID_ReadDTCInformation +
    UDS_POSITIVE_RESPONSE_OFFSET);
  for I := 0 to High(Msgs) do
  begin
    Part := ParseFourByteDtcPayload(Copy(Msgs[I].Data, 1, MaxInt),
      UDS_DTC_REPORT_DTC_BY_READINESS_GROUP, True);
    AppendWWHFromEcu(Result, Part, Msgs[I]);
  end;
end;

function TOBDWWHOBD.DoReadBySeverity(AStatusMask, ASeverityMask: Byte)
  : TArray<TOBDWWHDtcEntry>;
var
  Resp: TOBDResponse;
  Req: TBytes;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  Req := TBytes.Create(UDS_DTC_REPORT_WWHOBD_BY_MASK_RECORD,
    FFunctionalGroupIdentifier, AStatusMask, ASeverityMask);
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('WWH-OBD 19 42 negative: %s',
      [Resp.NRCText]);
  Result := ParseBySeverityResponse(Resp);
end;

function TOBDWWHOBD.DoReadPermanent: TArray<TOBDWWHDtcEntry>;
var
  Resp: TOBDResponse;
  Req: TBytes;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  Req := TBytes.Create(UDS_DTC_REPORT_WWHOBD_WITH_PERMANENT_STATUS,
    FFunctionalGroupIdentifier);
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('WWH-OBD 19 55 negative: %s',
      [Resp.NRCText]);
  Result := ParsePermanentResponse(Resp);
end;

function TOBDWWHOBD.DoReadByGroup(AGroupId: Byte): TArray<TOBDWWHDtcEntry>;
var
  Resp: TOBDResponse;
  Req: TBytes;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  Req := TBytes.Create(UDS_DTC_REPORT_DTC_BY_READINESS_GROUP,
    FFunctionalGroupIdentifier, AGroupId);
  Resp := FProtocol.Request(UDS_SID_ReadDTCInformation, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('WWH-OBD 19 56 group 0x%.2x negative: %s',
      [AGroupId, Resp.NRCText]);
  Result := ParseReadinessGroupResponse(Resp);
end;

function TOBDWWHOBD.DoReadDID(ADID: Word; AExpectedBytes: Integer): TBytes;
var
  Req: TBytes;
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('TOBDWWHOBD: Protocol not assigned');
  Req := TBytes.Create(Hi(ADID), Lo(ADID));
  Resp := FProtocol.Request(UDS_SID_ReadDataByIdentifier, Req);
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt('ReadDID 0x%.4x negative: %s',
      [ADID, Resp.NRCText]);
  if (Length(Resp.Data) < 2) or (Resp.Data[0] <> Hi(ADID)) or
    (Resp.Data[1] <> Lo(ADID)) then
    raise EOBDProtocolErr.CreateFmt('ReadDID 0x%.4x: mismatched response',
      [ADID]);
  if Length(Resp.Data) < 2 + AExpectedBytes then
    raise EOBDProtocolErr.CreateFmt
      ('ReadDID 0x%.4x: short response (%d, wanted >= %d)',
      [ADID, Length(Resp.Data), 2 + AExpectedBytes]);
  Result := Copy(Resp.Data, 2, Length(Resp.Data) - 2);
end;

function TOBDWWHOBD.ReadBySeverity: TArray<TOBDWWHDtcEntry>;
begin
  Result := DoReadBySeverity(FStatusMask, FSeverityMask);
  FireDTCs(Result);
end;

function TOBDWWHOBD.ReadPermanent: TArray<TOBDWWHDtcEntry>;
begin
  Result := DoReadPermanent;
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
  Body := DoReadDID(WWHOBD_DID_DistanceMILOn, 2);
  ADistanceKm := DecodeBigEndian(Body, 0, 2);
  Body := DoReadDID(WWHOBD_DID_TimeMILOn, 2);
  ATimeMinutes := DecodeBigEndian(Body, 0, 2);
  FireMILUsage(ADistanceKm, ATimeMinutes);
end;

procedure TOBDWWHOBD.ReadDTCCounters(out AClassA: UInt32; out AClassB1: UInt32);
var
  Entries: TArray<TOBDWWHDtcEntry>;
  I: Integer;
begin
  AClassA := 0;
  AClassB1 := 0;
  Entries := DoReadBySeverity(WWHOBD_DTC_STATUS_MASK_ALL, WWHOBD_SEV_MASK);
  for I := 0 to High(Entries) do
    case Entries[I].WWHClass of
      wcClassA:
        Inc(AClassA);
      wcClassB1:
        Inc(AClassB1);
    end;
end;

function TOBDWWHOBD.ReadVIN: string;
var
  Body: TBytes;
  I, Offset, Count: Integer;
begin
  try
    Body := DoReadDID(WWHOBD_DID_VIN, 17);
  except
    on E: EOBDProtocolErr do
      Body := DoReadDID(WWHOBD_DID_UDS_VIN, 17);
  end;
  Offset := 0;
  Count := Length(Body);
  if (Length(Body) >= 18) and (Body[0] > 0) then
  begin
    Offset := 1;
    Count := 17;
  end
  else if Count > 17 then
    Count := 17;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I + 1] := Char(Body[Offset + I]);
  Result := Trim(Result);
end;

procedure TOBDWWHOBD.ReadBySeverityAsync;
var
  Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    FOwnedTask.Start(
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
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDWWHOBD.ReadPermanentAsync;
var
  Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    FOwnedTask.Start(
      procedure
      begin
        try
          try
            Self_.ReadPermanent;
          except
            on E: Exception do
              Self_.FireError(oeIO, E.Message);
          end;
        finally
          Self_.ReleaseAsync;
        end;
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDWWHOBD.ReadByGroupAsync(AGroupId: Byte);
var
  Self_: TOBDWWHOBD;
  GID: Byte;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    GID := AGroupId;
    FOwnedTask.Start(
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
      end);
  except
    ReleaseAsync;
    raise;
  end;
end;

procedure TOBDWWHOBD.ReadMILUsageAsync;
var
  Self_: TOBDWWHOBD;
begin
  GuardSingleAsync;
  try
    Self_ := Self;
    FOwnedTask.Start(
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
      end);
  except
    ReleaseAsync;
    raise;
  end;
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
    FOwnedTask.Post(
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
    FOwnedTask.Post(
      procedure
      begin
        if Assigned(Self_.FOnMILUsage) then
          Self_.FOnMILUsage(Self_, Distance, Time);
      end);
end;

procedure TOBDWWHOBD.FireError(ACode: TOBDErrorCode; const AMessage: string);
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
    FOwnedTask.Post(
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
