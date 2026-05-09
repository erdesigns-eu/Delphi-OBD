//------------------------------------------------------------------------------
// UNIT           : OBD.ReadinessMonitor.pas
// CONTENTS       : Decode SAE J1979 PID 0x01 — Monitor Status Since
//                  Codes Cleared (MIL state + DTC count + continuous
//                  + non-continuous readiness monitor flags).
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The 4-byte payload encodes:
//                    A = MIL on (bit 7) + DTC count (bits 6-0)
//                    B = continuous monitor support (high nibble) +
//                        completion bits (low nibble) + spark/diesel
//                        flag (bit 3)
//                    C = non-continuous monitor support bitmask
//                    D = non-continuous monitor completion bitmask
//                  Spark and diesel engines map the C/D bits to a
//                  different list of monitors (catalyst vs NOx
//                  aftertreatment, heated catalyst vs PM filter,
//                  etc.) — both layouts are decoded here.
//------------------------------------------------------------------------------
unit OBD.ReadinessMonitor;

interface

uses
  System.SysUtils, System.Classes;

type
  EOBDReadinessError = class(Exception);

  /// <summary>
  ///   SAE J1979 / ISO 15031-5 monitor types. The
  ///   continuous three are common to all engines; the
  ///   non-continuous set diverges between spark-ignition (gasoline /
  ///   flex / hybrid) and compression-ignition (diesel).
  /// </summary>
  TOBDMonitorKind = (
    monMisfire,
    monFuelSystem,
    monComponents,
    // Spark-ignition non-continuous monitors:
    monCatalyst,
    monHeatedCatalyst,
    monEvap,
    monSecondaryAir,
    monACRefrigerant,
    monOxygenSensor,
    monOxygenSensorHeater,
    monEGR,
    // Compression-ignition (diesel) non-continuous monitors:
    monNMHCCatalyst,
    monNOxAftertreatment,
    monBoostPressure,
    monExhaustGasSensor,
    monPMFilter,
    monEGRorVVTDiesel
  );

  TOBDMonitorState = (msNotSupported, msNotReady, msReady);

  TOBDMonitorStatus = record
    Kind: TOBDMonitorKind;
    State: TOBDMonitorState;
  end;

  /// <summary>
  ///   Decoded result of PID 0x01.
  /// </summary>
  TOBDReadinessReport = record
    /// <summary>
    ///   True if the malfunction indicator lamp is illuminated.
    /// </summary>
    MILOn: Boolean;
    /// <summary>
    ///   Number of confirmed DTCs the ECU is currently holding.
    /// </summary>
    DtcCount: Byte;
    /// <summary>
    ///   True for compression-ignition (diesel) layout; false
    ///   for spark-ignition. Determined from byte B bit 3.
    /// </summary>
    IsDiesel: Boolean;
    /// <summary>
    ///   Per-monitor readiness state. Always 11 entries for
    ///   SI engines (3 continuous + 8 non-continuous) and 11 entries
    ///   for CI engines (3 continuous + 8 non-continuous diesel set).
    ///   Monitors that the ECU doesn't support report
    ///   <c>msNotSupported</c>.
    /// </summary>
    Monitors: TArray<TOBDMonitorStatus>;
  end;

/// <summary>
///   Decode the 4-byte PID 0x01 payload. Throws
///   <c>EOBDReadinessError</c> on a payload shorter than 4 bytes.
/// </summary>
function DecodeReadinessReport(const Bytes: TBytes): TOBDReadinessReport;

/// <summary>
///   Render a one-line summary suitable for logs / status
///   bars: <c>"MIL off, 0 DTCs, 5/8 readiness monitors complete"</c>.
/// </summary>
function FormatReadinessSummary(const Report: TOBDReadinessReport): string;

/// <summary>
///   Human-readable name for a monitor kind.
/// </summary>
function MonitorKindName(const Kind: TOBDMonitorKind): string;

/// <summary>
///   Human-readable name for a monitor state.
/// </summary>
function MonitorStateName(const State: TOBDMonitorState): string;

implementation

uses
  System.StrUtils;

const
  // Bit indices in C / D bytes for SI-engine non-continuous monitors
  // (per SAE J1979 §6.5).
  BIT_CATALYST            = 0;
  BIT_HEATED_CATALYST     = 1;
  BIT_EVAP                = 2;
  BIT_SECONDARY_AIR       = 3;
  BIT_AC_REFRIGERANT      = 4;
  BIT_OXYGEN_SENSOR       = 5;
  BIT_OXYGEN_SENSOR_HEATER = 6;
  BIT_EGR_SYSTEM          = 7;
  // Diesel mapping uses the same C/D bits but different semantics:
  BIT_NMHC_CATALYST       = 0;
  BIT_NOX_AFTERTREATMENT  = 1;
  // Bit 2 reserved for diesel
  BIT_BOOST_PRESSURE      = 3;
  // Bit 4 reserved for diesel
  BIT_EXHAUST_GAS_SENSOR  = 5;
  BIT_PM_FILTER           = 6;
  BIT_EGR_VVT_DIESEL      = 7;

//------------------------------------------------------------------------------
// MONITOR KIND NAME
//------------------------------------------------------------------------------
function MonitorKindName(const Kind: TOBDMonitorKind): string;
begin
  case Kind of
    monMisfire:             Result := 'misfire';
    monFuelSystem:          Result := 'fuel_system';
    monComponents:          Result := 'components';
    monCatalyst:            Result := 'catalyst';
    monHeatedCatalyst:      Result := 'heated_catalyst';
    monEvap:                Result := 'evap';
    monSecondaryAir:        Result := 'secondary_air';
    monACRefrigerant:       Result := 'ac_refrigerant';
    monOxygenSensor:        Result := 'oxygen_sensor';
    monOxygenSensorHeater:  Result := 'oxygen_sensor_heater';
    monEGR:                 Result := 'egr';
    monNMHCCatalyst:        Result := 'nmhc_catalyst';
    monNOxAftertreatment:   Result := 'nox_aftertreatment';
    monBoostPressure:       Result := 'boost_pressure';
    monExhaustGasSensor:    Result := 'exhaust_gas_sensor';
    monPMFilter:            Result := 'pm_filter';
    monEGRorVVTDiesel:      Result := 'egr_vvt_diesel';
  else
    Result := 'unknown';
  end;
end;

//------------------------------------------------------------------------------
// MONITOR STATE NAME
//------------------------------------------------------------------------------
function MonitorStateName(const State: TOBDMonitorState): string;
begin
  case State of
    msNotSupported: Result := 'not_supported';
    msNotReady:     Result := 'not_ready';
    msReady:        Result := 'ready';
  else
    Result := 'unknown';
  end;
end;

//------------------------------------------------------------------------------
// STATE FROM BITS
//------------------------------------------------------------------------------
function StateFromBits(const SupportSet, NotCompleteSet: Boolean): TOBDMonitorState;
begin
  if not SupportSet then Exit(msNotSupported);
  // 'NotCompleteSet' = 'this monitor has NOT yet run since the last
  // codes-cleared event'. Inverted vs. our enum.
  if NotCompleteSet then Result := msNotReady else Result := msReady;
end;

//------------------------------------------------------------------------------
// ADD MONITOR
//------------------------------------------------------------------------------
procedure AddMonitor(var Arr: TArray<TOBDMonitorStatus>;
  const Kind: TOBDMonitorKind; const State: TOBDMonitorState);
var
  N: Integer;
begin
  N := Length(Arr);
  SetLength(Arr, N + 1);
  Arr[N].Kind := Kind;
  Arr[N].State := State;
end;

//------------------------------------------------------------------------------
// DECODE READINESS REPORT
//------------------------------------------------------------------------------
function DecodeReadinessReport(const Bytes: TBytes): TOBDReadinessReport;
var
  A, B, C, D: Byte;
  i: Integer;
begin
  if Length(Bytes) < 4 then
    raise EOBDReadinessError.CreateFmt(
      'PID 0x01 payload must be 4 bytes, got %d', [Length(Bytes)]);
  Result := Default(TOBDReadinessReport);
  A := Bytes[0];
  B := Bytes[1];
  C := Bytes[2];
  D := Bytes[3];

  Result.MILOn := (A and $80) <> 0;
  Result.DtcCount := A and $7F;
  Result.IsDiesel := (B and $08) <> 0;

  // Continuous monitors live in byte B:
  //   bit 0 (low nibble) = misfire support; bit 4 = misfire complete-
  //   not-required (per SAE J1979 §6.5 the high nibble is "test not
  //   complete this monitoring cycle").
  AddMonitor(Result.Monitors, monMisfire,
    StateFromBits((B and $01) <> 0, (B and $10) <> 0));
  AddMonitor(Result.Monitors, monFuelSystem,
    StateFromBits((B and $02) <> 0, (B and $20) <> 0));
  AddMonitor(Result.Monitors, monComponents,
    StateFromBits((B and $04) <> 0, (B and $40) <> 0));

  // Non-continuous monitors: support bitmask in C, completion in D.
  // 'D bit set' means 'NOT yet completed this monitoring cycle'.
  if not Result.IsDiesel then
  begin
    // Spark-ignition (gasoline / flex / hybrid) layout.
    AddMonitor(Result.Monitors, monCatalyst,
      StateFromBits((C and (1 shl BIT_CATALYST)) <> 0,
                    (D and (1 shl BIT_CATALYST)) <> 0));
    AddMonitor(Result.Monitors, monHeatedCatalyst,
      StateFromBits((C and (1 shl BIT_HEATED_CATALYST)) <> 0,
                    (D and (1 shl BIT_HEATED_CATALYST)) <> 0));
    AddMonitor(Result.Monitors, monEvap,
      StateFromBits((C and (1 shl BIT_EVAP)) <> 0,
                    (D and (1 shl BIT_EVAP)) <> 0));
    AddMonitor(Result.Monitors, monSecondaryAir,
      StateFromBits((C and (1 shl BIT_SECONDARY_AIR)) <> 0,
                    (D and (1 shl BIT_SECONDARY_AIR)) <> 0));
    AddMonitor(Result.Monitors, monACRefrigerant,
      StateFromBits((C and (1 shl BIT_AC_REFRIGERANT)) <> 0,
                    (D and (1 shl BIT_AC_REFRIGERANT)) <> 0));
    AddMonitor(Result.Monitors, monOxygenSensor,
      StateFromBits((C and (1 shl BIT_OXYGEN_SENSOR)) <> 0,
                    (D and (1 shl BIT_OXYGEN_SENSOR)) <> 0));
    AddMonitor(Result.Monitors, monOxygenSensorHeater,
      StateFromBits((C and (1 shl BIT_OXYGEN_SENSOR_HEATER)) <> 0,
                    (D and (1 shl BIT_OXYGEN_SENSOR_HEATER)) <> 0));
    AddMonitor(Result.Monitors, monEGR,
      StateFromBits((C and (1 shl BIT_EGR_SYSTEM)) <> 0,
                    (D and (1 shl BIT_EGR_SYSTEM)) <> 0));
  end
  else
  begin
    // Compression-ignition (diesel) layout.
    AddMonitor(Result.Monitors, monNMHCCatalyst,
      StateFromBits((C and (1 shl BIT_NMHC_CATALYST)) <> 0,
                    (D and (1 shl BIT_NMHC_CATALYST)) <> 0));
    AddMonitor(Result.Monitors, monNOxAftertreatment,
      StateFromBits((C and (1 shl BIT_NOX_AFTERTREATMENT)) <> 0,
                    (D and (1 shl BIT_NOX_AFTERTREATMENT)) <> 0));
    AddMonitor(Result.Monitors, monBoostPressure,
      StateFromBits((C and (1 shl BIT_BOOST_PRESSURE)) <> 0,
                    (D and (1 shl BIT_BOOST_PRESSURE)) <> 0));
    AddMonitor(Result.Monitors, monExhaustGasSensor,
      StateFromBits((C and (1 shl BIT_EXHAUST_GAS_SENSOR)) <> 0,
                    (D and (1 shl BIT_EXHAUST_GAS_SENSOR)) <> 0));
    AddMonitor(Result.Monitors, monPMFilter,
      StateFromBits((C and (1 shl BIT_PM_FILTER)) <> 0,
                    (D and (1 shl BIT_PM_FILTER)) <> 0));
    AddMonitor(Result.Monitors, monEGRorVVTDiesel,
      StateFromBits((C and (1 shl BIT_EGR_VVT_DIESEL)) <> 0,
                    (D and (1 shl BIT_EGR_VVT_DIESEL)) <> 0));
  end;
  i := 0; if i = 0 then ; // suppress unused-var warning
end;

//------------------------------------------------------------------------------
// FORMAT READINESS SUMMARY
//------------------------------------------------------------------------------
function FormatReadinessSummary(const Report: TOBDReadinessReport): string;
var
  M: TOBDMonitorStatus;
  Supported, Ready: Integer;
  MILText: string;
begin
  Supported := 0;
  Ready := 0;
  for M in Report.Monitors do
    if M.State <> msNotSupported then
    begin
      Inc(Supported);
      if M.State = msReady then Inc(Ready);
    end;
  if Report.MILOn then MILText := 'on' else MILText := 'off';
  Result := Format('MIL %s, %d DTC%s, %d/%d readiness monitors complete (%s)',
    [MILText,
     Report.DtcCount, IfThen(Report.DtcCount = 1, '', 's'),
     Ready, Supported,
     IfThen(Report.IsDiesel, 'diesel', 'spark-ignition')]);
end;

end.
