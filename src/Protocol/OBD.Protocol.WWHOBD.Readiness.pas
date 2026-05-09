//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.WWHOBD.Readiness.pas
// CONTENTS       : WWH-OBD readiness monitor decoder (ISO 27145-3)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Protocol.WWHOBD.Readiness;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDWWHOBDReadiness = class(Exception);

  /// <summary>One monitor's state. Supported = the ECU has the monitor;
  /// Complete = the monitor has run and reported a result this drive
  /// cycle.</summary>
  TWWHOBDMonitorState = record
    /// <summary>Supported.</summary>
    Supported: Boolean;
    /// <summary>Complete.</summary>
    Complete: Boolean;
  end;

  /// <summary>Full readiness picture decoded from the FD05 payload.</summary>
  TWWHOBDReadinessSet = record
    /// <summary>Mil active.</summary>
    MILActive: Boolean;
    DTCCount: Byte;             // 0..127

    // Continuous monitors (ISO 15031-5 §8.6.1 byte B)
    /// <summary>Misfire.</summary>
    Misfire:        TWWHOBDMonitorState;
    /// <summary>Fuel system.</summary>
    FuelSystem:     TWWHOBDMonitorState;
    /// <summary>Comprehensive.</summary>
    Comprehensive:  TWWHOBDMonitorState;

    // Non-continuous monitors (ISO 27145-3 §6.4 + 15031-5 §8.6.1)
    /// <summary>Catalyst.</summary>
    Catalyst:                TWWHOBDMonitorState;
    /// <summary>Heated catalyst.</summary>
    HeatedCatalyst:          TWWHOBDMonitorState;
    /// <summary>Evaporative system.</summary>
    EvaporativeSystem:       TWWHOBDMonitorState;
    /// <summary>Secondary air system.</summary>
    SecondaryAirSystem:      TWWHOBDMonitorState;
    /// <summary>Ac refrigerant.</summary>
    ACRefrigerant:           TWWHOBDMonitorState;
    /// <summary>Oxygen sensor.</summary>
    OxygenSensor:            TWWHOBDMonitorState;
    /// <summary>Oxygen sensor heater.</summary>
    OxygenSensorHeater:      TWWHOBDMonitorState;
    /// <summary>Eg ror vvt system.</summary>
    EGRorVVTSystem:          TWWHOBDMonitorState;

    // ISO 27145-3 additions for diesel / Euro 6+
    /// <summary>Nmhc catalyst.</summary>
    NMHCCatalyst:            TWWHOBDMonitorState;
    /// <summary>N ox aftertreatment.</summary>
    NOxAftertreatment:       TWWHOBDMonitorState;
    /// <summary>Boost pressure system.</summary>
    BoostPressureSystem:     TWWHOBDMonitorState;
    /// <summary>Exhaust gas sensor.</summary>
    ExhaustGasSensor:        TWWHOBDMonitorState;
    /// <summary>Pm filter.</summary>
    PMFilter:                TWWHOBDMonitorState;
    /// <summary>Egr system.</summary>
    EGRSystem:               TWWHOBDMonitorState;

    /// <summary>True iff every supported monitor reports Complete.</summary>
    function AllReady: Boolean;
    /// <summary>List of monitor short-names that are supported but
    /// not yet complete (the workshop "drive cycle" target list).</summary>
    function PendingMonitors: TArray<string>;
  end;

/// <summary>Decode a 4-byte readiness payload. Spark-ignition (SI) and
/// compression-ignition (CI) layouts share the continuous-monitor byte
/// but differ on the non-continuous one; this decoder produces both
/// fleet sets and the caller picks per-vehicle.</summary>
function DecodeWWHOBDReadiness(const Bytes: TBytes): TWWHOBDReadinessSet;

/// <summary>Inverse encoder for round-trip / fixture testing.</summary>
function EncodeWWHOBDReadiness(const Set_: TWWHOBDReadinessSet): TBytes;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  // ISO 27145-3 §6.4 / ISO 15031-5 §8.6.1 — non-continuous monitor
  // bit positions, byte 2 (Supported) / byte 3 (NotComplete = bit set
  // means NOT complete; we invert when storing as Complete).
  BIT_CATALYST            = 0;
  BIT_HEATED_CATALYST     = 1;
  BIT_EVAP                = 2;
  BIT_SECONDARY_AIR       = 3;
  BIT_AC_REFRIGERANT      = 4;
  BIT_OXYGEN_SENSOR       = 5;
  BIT_OXYGEN_SENSOR_HEAT  = 6;
  BIT_EGR_OR_VVT          = 7;

  // ISO 27145-3 §6.4 extension byte (would be byte 4 on 5-byte form).
  // For the 4-byte form, the diesel monitors share bits with the SI
  // monitors per the engine-type indicator. We expose them as
  // separate fields so the caller decides which to surface.

//------------------------------------------------------------------------------
// SET MONITOR
//------------------------------------------------------------------------------
procedure SetMonitor(var M: TWWHOBDMonitorState; SupportByte, StatusByte: Byte;
  Bit: Integer);
begin
  M.Supported := (SupportByte and (1 shl Bit)) <> 0;
  // In the spec, status bit set means NOT complete.
  M.Complete := M.Supported and ((StatusByte and (1 shl Bit)) = 0);
end;

//------------------------------------------------------------------------------
// PACK MONITOR
//------------------------------------------------------------------------------
function PackMonitor(const M: TWWHOBDMonitorState; Bit: Integer;
  var SupportByte, StatusByte: Byte): Boolean;
begin
  if M.Supported then
  begin
    SupportByte := SupportByte or Byte(1 shl Bit);
    if not M.Complete then
      StatusByte := StatusByte or Byte(1 shl Bit);
  end;
  // Initialize result
  Result := True;
end;

{ TWWHOBDReadinessSet }

//------------------------------------------------------------------------------
// ALL READY
//------------------------------------------------------------------------------
function TWWHOBDReadinessSet.AllReady: Boolean;

  function MonitorReady(const M: TWWHOBDMonitorState): Boolean;
  begin
    Result := (not M.Supported) or M.Complete;
  end;

begin
  Result :=
    MonitorReady(Misfire) and MonitorReady(FuelSystem) and
    MonitorReady(Comprehensive) and MonitorReady(Catalyst) and
    MonitorReady(HeatedCatalyst) and MonitorReady(EvaporativeSystem) and
    MonitorReady(SecondaryAirSystem) and MonitorReady(ACRefrigerant) and
    MonitorReady(OxygenSensor) and MonitorReady(OxygenSensorHeater) and
    MonitorReady(EGRorVVTSystem) and MonitorReady(NMHCCatalyst) and
    MonitorReady(NOxAftertreatment) and MonitorReady(BoostPressureSystem) and
    MonitorReady(ExhaustGasSensor) and MonitorReady(PMFilter) and
    MonitorReady(EGRSystem);
end;

//------------------------------------------------------------------------------
// PENDING MONITORS
//------------------------------------------------------------------------------
function TWWHOBDReadinessSet.PendingMonitors: TArray<string>;

  procedure AddIfPending(var Out_: TArray<string>;
    const M: TWWHOBDMonitorState; const Name: string);
  begin
    if M.Supported and (not M.Complete) then
      Out_ := Out_ + [Name];
  end;

begin
  AddIfPending(Result, Misfire, 'Misfire');
  AddIfPending(Result, FuelSystem, 'FuelSystem');
  AddIfPending(Result, Comprehensive, 'Comprehensive');
  AddIfPending(Result, Catalyst, 'Catalyst');
  AddIfPending(Result, HeatedCatalyst, 'HeatedCatalyst');
  AddIfPending(Result, EvaporativeSystem, 'EvaporativeSystem');
  AddIfPending(Result, SecondaryAirSystem, 'SecondaryAirSystem');
  AddIfPending(Result, ACRefrigerant, 'ACRefrigerant');
  AddIfPending(Result, OxygenSensor, 'OxygenSensor');
  AddIfPending(Result, OxygenSensorHeater, 'OxygenSensorHeater');
  AddIfPending(Result, EGRorVVTSystem, 'EGRorVVTSystem');
  AddIfPending(Result, NMHCCatalyst, 'NMHCCatalyst');
  AddIfPending(Result, NOxAftertreatment, 'NOxAftertreatment');
  AddIfPending(Result, BoostPressureSystem, 'BoostPressureSystem');
  AddIfPending(Result, ExhaustGasSensor, 'ExhaustGasSensor');
  AddIfPending(Result, PMFilter, 'PMFilter');
  AddIfPending(Result, EGRSystem, 'EGRSystem');
end;

//------------------------------------------------------------------------------
// DECODE WWHOBDREADINESS
//------------------------------------------------------------------------------
function DecodeWWHOBDReadiness(const Bytes: TBytes): TWWHOBDReadinessSet;
var
  ContByte, NCSupport, NCStatus: Byte;
begin
  if Length(Bytes) < 4 then
    raise EOBDWWHOBDReadiness.CreateFmt(
      'Readiness payload must be >= 4 bytes (got %d)', [Length(Bytes)]);

  Result := Default(TWWHOBDReadinessSet);
  Result.MILActive := (Bytes[0] and $80) <> 0;
  Result.DTCCount  := Bytes[0] and $7F;

  ContByte := Bytes[1];
  // Continuous monitor encoding per ISO 15031-5 §8.6.1: bit positions
  // 0/1/2 = supported (Misfire/FuelSystem/Comprehensive),
  // 4/5/6 = NOT-complete.
  Result.Misfire.Supported       := (ContByte and $01) <> 0;
  Result.FuelSystem.Supported    := (ContByte and $02) <> 0;
  Result.Comprehensive.Supported := (ContByte and $04) <> 0;
  Result.Misfire.Complete        := Result.Misfire.Supported       and ((ContByte and $10) = 0);
  Result.FuelSystem.Complete     := Result.FuelSystem.Supported    and ((ContByte and $20) = 0);
  Result.Comprehensive.Complete  := Result.Comprehensive.Supported and ((ContByte and $40) = 0);

  NCSupport := Bytes[2];
  NCStatus  := Bytes[3];
  SetMonitor(Result.Catalyst,           NCSupport, NCStatus, BIT_CATALYST);
  SetMonitor(Result.HeatedCatalyst,     NCSupport, NCStatus, BIT_HEATED_CATALYST);
  SetMonitor(Result.EvaporativeSystem,  NCSupport, NCStatus, BIT_EVAP);
  SetMonitor(Result.SecondaryAirSystem, NCSupport, NCStatus, BIT_SECONDARY_AIR);
  SetMonitor(Result.ACRefrigerant,      NCSupport, NCStatus, BIT_AC_REFRIGERANT);
  SetMonitor(Result.OxygenSensor,       NCSupport, NCStatus, BIT_OXYGEN_SENSOR);
  SetMonitor(Result.OxygenSensorHeater, NCSupport, NCStatus, BIT_OXYGEN_SENSOR_HEAT);
  SetMonitor(Result.EGRorVVTSystem,     NCSupport, NCStatus, BIT_EGR_OR_VVT);

  // ISO 27145-3 §6.4 diesel monitors: when the payload extends to 5+
  // bytes the dedicated extension byte at offset 4 carries the diesel
  // monitors; for the 4-byte form the spec overlays them onto the
  // existing slots per engine-type indicator. We expose both groups
  // and let the caller pick per vehicle.
  if Length(Bytes) >= 6 then
  begin
    SetMonitor(Result.NMHCCatalyst,        Bytes[4], Bytes[5], 0);
    SetMonitor(Result.NOxAftertreatment,   Bytes[4], Bytes[5], 1);
    SetMonitor(Result.BoostPressureSystem, Bytes[4], Bytes[5], 2);
    SetMonitor(Result.ExhaustGasSensor,    Bytes[4], Bytes[5], 3);
    SetMonitor(Result.PMFilter,            Bytes[4], Bytes[5], 4);
    SetMonitor(Result.EGRSystem,           Bytes[4], Bytes[5], 5);
  end;
end;

//------------------------------------------------------------------------------
// ENCODE WWHOBDREADINESS
//------------------------------------------------------------------------------
function EncodeWWHOBDReadiness(const Set_: TWWHOBDReadinessSet): TBytes;
var
  ContByte, NCSupport, NCStatus, NCSupport2, NCStatus2: Byte;
  HasDieselGroup: Boolean;

  procedure PackContMonitor(const M: TWWHOBDMonitorState;
    SupportBit, StatusBit: Integer);
  begin
    if M.Supported then ContByte := ContByte or Byte(1 shl SupportBit);
    if M.Supported and (not M.Complete) then
      ContByte := ContByte or Byte(1 shl StatusBit);
  end;

begin
  ContByte := 0;
  NCSupport := 0; NCStatus := 0;
  NCSupport2 := 0; NCStatus2 := 0;

  PackContMonitor(Set_.Misfire,       0, 4);
  PackContMonitor(Set_.FuelSystem,    1, 5);
  PackContMonitor(Set_.Comprehensive, 2, 6);

  PackMonitor(Set_.Catalyst,           BIT_CATALYST,           NCSupport, NCStatus);
  PackMonitor(Set_.HeatedCatalyst,     BIT_HEATED_CATALYST,    NCSupport, NCStatus);
  PackMonitor(Set_.EvaporativeSystem,  BIT_EVAP,               NCSupport, NCStatus);
  PackMonitor(Set_.SecondaryAirSystem, BIT_SECONDARY_AIR,      NCSupport, NCStatus);
  PackMonitor(Set_.ACRefrigerant,      BIT_AC_REFRIGERANT,     NCSupport, NCStatus);
  PackMonitor(Set_.OxygenSensor,       BIT_OXYGEN_SENSOR,      NCSupport, NCStatus);
  PackMonitor(Set_.OxygenSensorHeater, BIT_OXYGEN_SENSOR_HEAT, NCSupport, NCStatus);
  PackMonitor(Set_.EGRorVVTSystem,     BIT_EGR_OR_VVT,         NCSupport, NCStatus);

  HasDieselGroup :=
    Set_.NMHCCatalyst.Supported or Set_.NOxAftertreatment.Supported or
    Set_.BoostPressureSystem.Supported or Set_.ExhaustGasSensor.Supported or
    Set_.PMFilter.Supported or Set_.EGRSystem.Supported;
  if HasDieselGroup then
  begin
    PackMonitor(Set_.NMHCCatalyst,        0, NCSupport2, NCStatus2);
    PackMonitor(Set_.NOxAftertreatment,   1, NCSupport2, NCStatus2);
    PackMonitor(Set_.BoostPressureSystem, 2, NCSupport2, NCStatus2);
    PackMonitor(Set_.ExhaustGasSensor,    3, NCSupport2, NCStatus2);
    PackMonitor(Set_.PMFilter,            4, NCSupport2, NCStatus2);
    PackMonitor(Set_.EGRSystem,           5, NCSupport2, NCStatus2);
    // Allocate Result
    SetLength(Result, 6);
    Result[4] := NCSupport2;
    Result[5] := NCStatus2;
  end
  else
    // Allocate Result
    SetLength(Result, 4);

  Result[0] := (Set_.DTCCount and $7F);
  if Set_.MILActive then Result[0] := Result[0] or $80;
  Result[1] := ContByte;
  Result[2] := NCSupport;
  Result[3] := NCStatus;
end;

end.
