//------------------------------------------------------------------------------
// UNIT           : OBD.VehicleHealth.pas
// CONTENTS       : High-level "give me the vehicle's overall state"
//                  helper that aggregates VIN + OEM + DTCs +
//                  readiness + key live values into one TOBDHealthSnapshot
//                  record. The single call most diagnostic apps want.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The snapshot is async-capable but presented here
//                  in a synchronous form for simplicity — the
//                  underlying calls go through TOBDConnectionAsync's
//                  IOBDFuture<string> and Await. Each step is
//                  best-effort: a failed read populates the
//                  matching error field but doesn't abort the rest
//                  of the snapshot. Tools surface the partial
//                  result as "we got X but Y failed".
//------------------------------------------------------------------------------
unit OBD.VehicleHealth;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.Connection.Async,
  OBD.OEM, OBD.OEM.DTC, OBD.ReadinessMonitor;

type
  EOBDHealthError = class(Exception);

  /// <summary>
  ///   One DTC + its catalog metadata.
  /// </summary>
  TOBDHealthDTC = record
    Code: string;
    Catalogued: Boolean;
    Description: string;
    Severity: TOBDDtcSeverity;
  end;

  /// <summary>
  ///   Aggregated diagnostic snapshot. Every field that
  ///   couldn't be read carries an error string; callers display
  ///   what's present and surface the missing pieces.
  /// </summary>
  TOBDHealthSnapshot = record
    /// <summary>
    ///   Wall-clock timestamp when the snapshot was taken.
    /// </summary>
    Timestamp: TDateTime;
    /// <summary>
    ///   Vehicle identification number (Service 09 PID 02).
    /// </summary>
    VIN: string;
    VINError: string;
    /// <summary>
    ///   Resolved OEM extension (or nil if no extension matched).
    /// </summary>
    OEM: IOBDOEMExtension;
    OEMDisplayName: string;
    /// <summary>
    ///   Active DTCs (Service 03) annotated with catalog metadata.
    /// </summary>
    ActiveDTCs: TArray<TOBDHealthDTC>;
    DTCError: string;
    /// <summary>
    ///   Pending DTCs (Service 07) annotated with catalog metadata.
    /// </summary>
    PendingDTCs: TArray<TOBDHealthDTC>;
    PendingError: string;
    /// <summary>
    ///   Readiness monitor status (Service 01 PID 01).
    /// </summary>
    Readiness: TOBDReadinessReport;
    ReadinessKnown: Boolean;
    ReadinessError: string;
    /// <summary>
    ///   Key live values pulled from Service 01.
    /// </summary>
    BatteryVoltage: Double;
    BatteryVoltageKnown: Boolean;
    EngineRPM: Word;
    EngineRPMKnown: Boolean;
    VehicleSpeed: Byte;
    VehicleSpeedKnown: Boolean;
    CoolantTemperature: Integer;
    CoolantTemperatureKnown: Boolean;
    EngineLoad: Double;
    EngineLoadKnown: Boolean;
    /// <summary>
    ///   Computed health score 0..100 — 100 = healthy, 0 =
    ///   every catalogued DTC is critical with the MIL on. The
    ///   scoring rubric is documented in <c>ComputeHealthScore</c>.
    /// </summary>
    HealthScore: Byte;
    /// <summary>
    ///   One-line summary suitable for status bars.
    /// </summary>
    SummaryLine: string;
  end;

  /// <summary>
  ///   Snapshot capture orchestrator. Wraps an async connection
  ///   + auto-resolves the OEM extension by VIN. Each public method
  ///   is best-effort — failures land in the snapshot's *Error
  ///   fields rather than raising.
  /// </summary>
  TOBDHealthCapture = class
  strict private
    FConnection: TOBDConnectionAsync;
    function ReadVIN(out VIN: string): Boolean;
    function ReadOBDPid(const Pid: Word; out Bytes: TBytes;
      const TimeoutMs: Cardinal = 2000): Boolean;
    function ReadDTCsService(const Service: Byte;
      out Codes: TArray<string>): Boolean;
    procedure FillDTCMetadata(const OEM: IOBDOEMExtension;
      const Codes: TArray<string>;
      out Annotated: TArray<TOBDHealthDTC>);
    procedure ReadReadiness(var Snap: TOBDHealthSnapshot);
    procedure ReadLiveValues(var Snap: TOBDHealthSnapshot);
    procedure ComputeHealthScore(var Snap: TOBDHealthSnapshot);
    procedure ComposeSummary(var Snap: TOBDHealthSnapshot);
  public
    constructor Create(const Conn: TOBDConnectionAsync);

    /// <summary>
    ///   One-shot capture. Walks: VIN → OEM resolution →
    ///   active DTCs → pending DTCs → readiness → live values →
    ///   health score + summary line. Returns the populated
    ///   snapshot regardless of partial failures.
    /// </summary>
    function Capture: TOBDHealthSnapshot;
  end;

implementation

uses
  System.StrUtils,
  OBD.Async, OBD.OEM.Coding;

//------------------------------------------------------------------------------
// IF THEN STR
//------------------------------------------------------------------------------
function IfThenStr(const Cond: Boolean; const A, B: string): string;
begin
  if Cond then Result := A else Result := B;
end;

const
  PID_MONITOR_STATUS    = $01;
  PID_ENGINE_LOAD       = $04;
  PID_COOLANT_TEMP      = $05;
  PID_RPM               = $0C;
  PID_VEHICLE_SPEED     = $0D;
  PID_BATTERY_VOLTAGE   = $42;

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDHealthCapture.Create(const Conn: TOBDConnectionAsync);
begin
  if Conn = nil then
    raise EOBDHealthError.Create('TOBDHealthCapture requires a connection');
  inherited Create;
  FConnection := Conn;
end;

//------------------------------------------------------------------------------
// READ OBDPID
//------------------------------------------------------------------------------
function TOBDHealthCapture.ReadOBDPid(const Pid: Word; out Bytes: TBytes;
  const TimeoutMs: Cardinal): Boolean;
var
  Future: IOBDFuture<string>;
  Reply: string;
begin
  Bytes := nil;
  try
    Future := FConnection.OBDAsync(Format('01 %.2X', [Pid]), TimeoutMs);
    Reply := Future.Await(TimeoutMs + 500);
    Bytes := HexStringToBytes(Reply);
    Result := Length(Bytes) >= 2;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// READ VIN
//------------------------------------------------------------------------------
function TOBDHealthCapture.ReadVIN(out VIN: string): Boolean;
var
  Future: IOBDFuture<string>;
  Reply: string;
  Bytes: TBytes;
  i: Integer;
begin
  VIN := '';
  Result := False;
  try
    Future := FConnection.OBDAsync('09 02', 3000);
    Reply := Future.Await(3500);
    Bytes := HexStringToBytes(Reply);
    // Service 09 PID 02 reply: 49 02 01 + 17 ASCII bytes (frames
    // reassembled by the adapter). Treat anything printable past
    // index 3 as VIN payload.
    for i := 3 to High(Bytes) do
      if (Bytes[i] >= 32) and (Bytes[i] < 127) then
        VIN := VIN + Char(Bytes[i]);
    VIN := VIN.Trim;
    Result := Length(VIN) = 17;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// READ DTCS SERVICE
//------------------------------------------------------------------------------
function TOBDHealthCapture.ReadDTCsService(const Service: Byte;
  out Codes: TArray<string>): Boolean;
var
  Future: IOBDFuture<string>;
  Reply: string;
  Bytes: TBytes;
  Idx: Integer;
  Hi, Lo: Byte;
  Code: string;
const
  SYSTEMS: array[0..3] of Char = ('P', 'C', 'B', 'U');
begin
  Codes := nil;
  Result := False;
  try
    Future := FConnection.OBDAsync(Format('%.2X', [Service]), 3000);
    Reply := Future.Await(3500);
    Bytes := HexStringToBytes(Reply);
    // Positive reply SID = request SID + 0x40. Reply layout:
    //   <SID+40> <count> <DTC1 hi> <DTC1 lo> <DTC2 hi> <DTC2 lo> …
    if (Length(Bytes) < 2) or (Bytes[0] <> Service + $40) then Exit;
    Idx := 2;
    while Idx + 1 < Length(Bytes) do
    begin
      Hi := Bytes[Idx];
      Lo := Bytes[Idx + 1];
      Inc(Idx, 2);
      if (Hi = 0) and (Lo = 0) then Break;
      // ISO 15031-5 / SAE J2012 packed-DTC encoding.
      Code := Format('%s%d%d%.2X',
        [SYSTEMS[(Hi shr 6) and 3],
         ((Hi shr 5) and 1) * 2 + ((Hi shr 4) and 1),
         Hi and $0F, Lo]);
      Codes := Codes + [Code];
    end;
    Result := True;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// FILL DTCMETADATA
//------------------------------------------------------------------------------
procedure TOBDHealthCapture.FillDTCMetadata(const OEM: IOBDOEMExtension;
  const Codes: TArray<string>; out Annotated: TArray<TOBDHealthDTC>);
var
  Code: string;
  Entry: TOBDDtcCatalogEntry;
  H: TOBDHealthDTC;
begin
  Annotated := nil;
  for Code in Codes do
  begin
    H := Default(TOBDHealthDTC);
    H.Code := Code;
    if Assigned(OEM) and OEM.DescribeDTC(Code, Entry) then
    begin
      H.Catalogued := True;
      H.Description := Entry.Description;
      H.Severity := Entry.Severity;
    end
    else
    begin
      H.Catalogued := False;
      H.Severity := dtcSeverityUnknown;
    end;
    Annotated := Annotated + [H];
  end;
end;

//------------------------------------------------------------------------------
// READ READINESS
//------------------------------------------------------------------------------
procedure TOBDHealthCapture.ReadReadiness(var Snap: TOBDHealthSnapshot);
var
  Bytes, Payload: TBytes;
begin
  Snap.ReadinessKnown := False;
  if not ReadOBDPid(PID_MONITOR_STATUS, Bytes) then
  begin
    Snap.ReadinessError := 'PID 0x01 read failed';
    Exit;
  end;
  // Strip the "41 01" reply header to expose the 4-byte payload.
  if (Length(Bytes) >= 6) and (Bytes[0] = $41) and (Bytes[1] = $01) then
    Payload := Copy(Bytes, 2, 4)
  else if Length(Bytes) >= 4 then
    Payload := Copy(Bytes, Length(Bytes) - 4, 4)
  else
  begin
    Snap.ReadinessError := 'PID 0x01 reply too short';
    Exit;
  end;
  try
    Snap.Readiness := DecodeReadinessReport(Payload);
    Snap.ReadinessKnown := True;
  except
    on E: Exception do
      Snap.ReadinessError := E.Message;
  end;
end;

//------------------------------------------------------------------------------
// READ LIVE VALUES
//------------------------------------------------------------------------------
procedure TOBDHealthCapture.ReadLiveValues(var Snap: TOBDHealthSnapshot);
var
  Bytes: TBytes;
begin
  // Battery voltage (PID 0x42): A*256 + B / 1000 volts.
  Snap.BatteryVoltageKnown := ReadOBDPid(PID_BATTERY_VOLTAGE, Bytes) and
                              (Length(Bytes) >= 4);
  if Snap.BatteryVoltageKnown then
    Snap.BatteryVoltage := ((Bytes[2] shl 8) or Bytes[3]) / 1000.0;

  // RPM (PID 0x0C): ((A*256)+B) / 4 = rpm.
  Snap.EngineRPMKnown := ReadOBDPid(PID_RPM, Bytes) and
                         (Length(Bytes) >= 4);
  if Snap.EngineRPMKnown then
    Snap.EngineRPM := ((Bytes[2] shl 8) or Bytes[3]) div 4;

  // Speed (PID 0x0D): A = km/h.
  Snap.VehicleSpeedKnown := ReadOBDPid(PID_VEHICLE_SPEED, Bytes) and
                            (Length(Bytes) >= 3);
  if Snap.VehicleSpeedKnown then
    Snap.VehicleSpeed := Bytes[2];

  // Coolant temp (PID 0x05): A - 40 = °C.
  Snap.CoolantTemperatureKnown := ReadOBDPid(PID_COOLANT_TEMP, Bytes) and
                                   (Length(Bytes) >= 3);
  if Snap.CoolantTemperatureKnown then
    Snap.CoolantTemperature := Integer(Bytes[2]) - 40;

  // Engine load (PID 0x04): A * 100/255 = percent.
  Snap.EngineLoadKnown := ReadOBDPid(PID_ENGINE_LOAD, Bytes) and
                          (Length(Bytes) >= 3);
  if Snap.EngineLoadKnown then
    Snap.EngineLoad := Bytes[2] * (100 / 255);
end;

//------------------------------------------------------------------------------
// COMPUTE HEALTH SCORE
//------------------------------------------------------------------------------
procedure TOBDHealthCapture.ComputeHealthScore(var Snap: TOBDHealthSnapshot);
var
  Penalty: Integer;
  D: TOBDHealthDTC;
  M: TOBDMonitorStatus;
begin
  // Start at 100 and subtract per active issue. Rubric (chosen so a
  // 'fresh from the factory' snapshot scores 100 and a 'MIL on with
  // critical engine DTCs' scores near 0):
  //   - MIL on:                        -10
  //   - Each critical active DTC:      -20
  //   - Each warning active DTC:       -8
  //   - Each info active DTC:          -3
  //   - Each unknown active DTC:       -10
  //   - Each pending DTC:              -2
  //   - Readiness has any not_ready:   -1 per monitor
  // Lower bound clamps to 0.
  Penalty := 0;
  if Snap.ReadinessKnown and Snap.Readiness.MILOn then
    Inc(Penalty, 10);
  for D in Snap.ActiveDTCs do
    case D.Severity of
      dtcSeverityCritical: Inc(Penalty, 20);
      dtcSeverityWarning:  Inc(Penalty, 8);
      dtcSeverityInfo:     Inc(Penalty, 3);
    else
      Inc(Penalty, 10);   // dtcSeverityUnknown
    end;
  Inc(Penalty, Length(Snap.PendingDTCs) * 2);
  if Snap.ReadinessKnown then
    for M in Snap.Readiness.Monitors do
      if M.State = msNotReady then Inc(Penalty);
  if Penalty >= 100 then Snap.HealthScore := 0
  else Snap.HealthScore := Byte(100 - Penalty);
end;

//------------------------------------------------------------------------------
// COMPOSE SUMMARY
//------------------------------------------------------------------------------
procedure TOBDHealthCapture.ComposeSummary(var Snap: TOBDHealthSnapshot);
var
  Buf: TStringBuilder;
begin
  Buf := TStringBuilder.Create;
  try
    Buf.AppendFormat('VIN %s — health %d/100 — ',
      [IfThenStr(Snap.VIN = '', '(unknown)', Snap.VIN), Snap.HealthScore]);
    if Length(Snap.ActiveDTCs) = 0 then
      Buf.Append('no active DTCs')
    else
      Buf.AppendFormat('%d active DTCs', [Length(Snap.ActiveDTCs)]);
    if Length(Snap.PendingDTCs) > 0 then
      Buf.AppendFormat(' (%d pending)', [Length(Snap.PendingDTCs)]);
    if Snap.ReadinessKnown then
      Buf.AppendFormat(' — %s', [FormatReadinessSummary(Snap.Readiness)]);
    Snap.SummaryLine := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

//------------------------------------------------------------------------------
// CAPTURE
//------------------------------------------------------------------------------
function TOBDHealthCapture.Capture: TOBDHealthSnapshot;
var
  Codes: TArray<string>;
begin
  Result := Default(TOBDHealthSnapshot);
  Result.Timestamp := Now;

  if ReadVIN(Result.VIN) then
  begin
    Result.OEM := TOBDOEMRegistry.FindByVIN(Result.VIN);
    if Result.OEM <> nil then
      Result.OEMDisplayName := Result.OEM.DisplayName
    else
      Result.OEMDisplayName := '(no OEM extension matched VIN)';
  end
  else
  begin
    Result.VINError := 'VIN read failed (Service 09 PID 02)';
    Result.OEMDisplayName := '(VIN unknown)';
  end;

  if ReadDTCsService($03, Codes) then
    FillDTCMetadata(Result.OEM, Codes, Result.ActiveDTCs)
  else
    Result.DTCError := 'Service 03 read failed';

  if ReadDTCsService($07, Codes) then
    FillDTCMetadata(Result.OEM, Codes, Result.PendingDTCs)
  else
    Result.PendingError := 'Service 07 read failed';

  ReadReadiness(Result);
  ReadLiveValues(Result);
  ComputeHealthScore(Result);
  ComposeSummary(Result);
end;

end.
