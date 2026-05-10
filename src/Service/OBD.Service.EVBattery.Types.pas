//------------------------------------------------------------------------------
//  OBD.Service.EVBattery.Types
//
//  Value types for the EV battery health reader (P-A5).
//
//  TOBDEVBatteryField enumerates the data points we know how
//  to decode across the supported BEV / PHEV platforms.
//  TOBDEVBatterySnapshot is the result record - one populated
//  copy per poll. TOBDEVBatteryRule is one decode rule loaded
//  from the per-vendor JSON catalogue under
//  catalogs/ev-battery/.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.EVBattery.Types;

interface

uses
  System.SysUtils;

type
  /// <summary>One decoded battery field. The enum is open
  /// (efkUnknown for entries the catalogue introduces but the
  /// runtime doesn't recognise yet).</summary>
  TOBDEVBatteryField = (
    efkUnknown,

    // Core
    efkSOC,                  // %
    efkSOH,                  // %
    efkPackVoltage,          // V
    efkPackCurrent,          // A (positive = discharge)
    efkPackPower,            // kW (= V * I / 1000)
    efkCapacityRemainingKwh, // kWh
    efkCapacityNominalKwh,   // kWh

    // Cells
    efkCellVoltageMin,       // V
    efkCellVoltageMax,       // V
    efkCellVoltageAvg,       // V
    efkCellVoltagesArray,    // V[]

    // Thermal
    efkPackTempMin,          // C
    efkPackTempMax,          // C
    efkModuleTempArray,      // C[]
    efkInletCoolantTemp,     // C
    efkOutletCoolantTemp,    // C

    // Driving
    efkRangeKm,              // km
    efkOdometerKm,           // km
    efkChargeState,          // enum-ish string
    efkChargePortTemp,       // C
    efkChargingPowerKw,      // kW (positive = charging)

    // Auxiliary / extended (added when vendor catalogues
    // expose them - not all platforms do).
    efkAuxBatteryVoltage,    // V (12V starter battery)
    efkAvailableChargePowerKw,
    efkAvailableDischargePowerKw,
    efkCumulativeEnergyChargedKwh,
    efkCumulativeEnergyDischargedKwh
  );

  TOBDEVChargeState = (
    csUnknown,
    csIdle,
    csACCharging,
    csDCFastCharging,
    csDriving,
    csRegenBraking
  );

  /// <summary>One BMS poll result. Every field is best-effort:
  /// when the vendor catalogue doesn't define a rule for it,
  /// the field stays at its zero value and the matching
  /// <c>HasXxx</c> flag is False so the host can tell "not
  /// present" from "zero".</summary>
  TOBDEVBatterySnapshot = record
    /// <summary>Wall-clock timestamp of the snapshot.</summary>
    Timestamp:           TDateTime;
    /// <summary>Vendor key the rules were loaded from
    /// (e.g. <c>"hmg"</c>, <c>"nissan-leaf"</c>).</summary>
    Vendor:              string;

    // Core
    HasSOC:              Boolean;  SOC:               Single;
    HasSOH:              Boolean;  SOH:               Single;
    HasPackVoltage:      Boolean;  PackVoltage:       Single;
    HasPackCurrent:      Boolean;  PackCurrent:       Single;
    HasPackPower:        Boolean;  PackPower:         Single;
    HasCapacityRemaining:Boolean;  CapacityRemainingKwh: Single;
    HasCapacityNominal:  Boolean;  CapacityNominalKwh:   Single;

    // Cells
    HasCellVoltageMin:   Boolean;  CellVoltageMin:    Single;
    HasCellVoltageMax:   Boolean;  CellVoltageMax:    Single;
    HasCellVoltageAvg:   Boolean;  CellVoltageAvg:    Single;
    HasCellVoltages:     Boolean;  CellVoltages:      TArray<Single>;

    // Thermal
    HasPackTempMin:      Boolean;  PackTempMinC:      Single;
    HasPackTempMax:      Boolean;  PackTempMaxC:      Single;
    HasModuleTemps:      Boolean;  ModuleTempsC:      TArray<Single>;
    HasInletCoolant:     Boolean;  InletCoolantTempC: Single;
    HasOutletCoolant:    Boolean;  OutletCoolantTempC:Single;

    // Driving
    HasRangeKm:          Boolean;  RangeKm:           Single;
    HasOdometerKm:       Boolean;  OdometerKm:        Cardinal;
    HasChargeState:      Boolean;  ChargeState:       TOBDEVChargeState;
    HasChargePortTemp:   Boolean;  ChargePortTempC:   Single;
    HasChargingPower:    Boolean;  ChargingPowerKw:   Single;

    // Auxiliary / extended
    HasAuxBatteryVoltage:        Boolean; AuxBatteryVoltage:        Single;
    HasAvailableChargePower:     Boolean; AvailableChargePowerKw:   Single;
    HasAvailableDischargePower:  Boolean; AvailableDischargePowerKw:Single;
    HasCumulativeChargedKwh:     Boolean; CumulativeChargedKwh:     Single;
    HasCumulativeDischargedKwh:  Boolean; CumulativeDischargedKwh:  Single;

    /// <summary>Per-field error messages for fields the
    /// catalogue declared but the read failed on. Keys are the
    /// JSON field name (e.g. "soc"); values are the NRC text /
    /// timeout reason.</summary>
    Errors:              TArray<string>;
  end;

  /// <summary>One decode rule from the per-vendor JSON. Maps
  /// a UDS / Mode-21 read at a known DID/PID to one
  /// <see cref="TOBDEVBatteryField"/> via byte slicing + linear
  /// scaling.</summary>
  TOBDEVBatteryRule = record
    /// <summary>The output field this rule populates.</summary>
    Field:        TOBDEVBatteryField;
    /// <summary>JSON field name (carried for error reporting).</summary>
    FieldName:    string;
    /// <summary>UDS service id - 0x21 (Mode 21 LBC, Nissan /
    /// Renault), 0x22 (UDS ReadDataByIdentifier - everyone
    /// else), or 0x01 (legacy Mode 01 PID).</summary>
    Service:      Byte;
    /// <summary>DID (when Service=0x22) or PID (when 0x21/0x01)
    /// in 16-bit form. The codec emits the right byte width on
    /// the wire.</summary>
    DIDOrPID:     Word;
    /// <summary>0-based byte offset within the response data
    /// payload (after the service / DID echo).</summary>
    Offset:       Integer;
    /// <summary>Number of bytes to slice. 1, 2, 4 are common.
    /// 0 = "rest of payload" (used for variable-length
    /// arrays).</summary>
    Length:       Integer;
    /// <summary>True for signed integer interpretation (current,
    /// temperature). Default unsigned.</summary>
    Signed:       Boolean;
    /// <summary>Linear scale: physical = raw * Scale + OffsetVal.</summary>
    Scale:        Double;
    OffsetVal:    Double;
    /// <summary>Free-form unit string for logs / display
    /// (the runtime doesn't validate it).</summary>
    Unit_:        string;
    /// <summary>Free-form citation URL or document reference.</summary>
    Source:       string;

    // Cell-array specific:
    /// <summary>True when this rule decodes a per-cell or
    /// per-module array (one value per <c>ElementSize</c>
    /// bytes after Offset).</summary>
    IsArray:      Boolean;
    /// <summary>Bytes per element when <c>IsArray</c>. Typical:
    /// 1 (LBC cell V * 80mV resolution), 2 (HMG cell V in mV).</summary>
    ElementSize:  Integer;
  end;

  /// <summary>Loaded vendor catalogue: ECU CAN IDs +
  /// ordered list of decode rules.</summary>
  TOBDEVBatteryVendorCatalog = record
    Vendor:           string;
    Label_:           string;
    RequestId:        Cardinal;   // CAN ID for outbound diag req
    ResponseId:       Cardinal;   // CAN ID for inbound diag rsp
    ApplicableModels: TArray<string>;
    Rules:            TArray<TOBDEVBatteryRule>;
  end;

function FieldKindFromName(const AName: string): TOBDEVBatteryField;
function FieldKindName(AField: TOBDEVBatteryField): string;
function ChargeStateFromText(const AText: string): TOBDEVChargeState;

implementation

function FieldKindFromName(const AName: string): TOBDEVBatteryField;
var L: string;
begin
  L := LowerCase(AName);
  // Canonical names + the aliases each vendor's catalogue
  // tends to use. Cells_1_32 / 33_64 / etc. all funnel into
  // efkCellVoltagesArray; ApplyDecoded appends rather than
  // overwrites for that field. Module_temp_N / pack_temp_N
  // funnel into efkModuleTempArray (one element each).

  // SOC variants
  if (L = 'soc') or (L = 'soc_bms') or
     (L = 'soc_display') or (L = 'soc_displayed') then Exit(efkSOC);
  // SOH variants
  if (L = 'soh') or (L = 'soh_pct') or
     (L = 'soh_capacity') or (L = 'soh_factory') then Exit(efkSOH);

  if (L = 'pack_voltage') or (L = 'bus_voltage') then Exit(efkPackVoltage);
  if  L = 'pack_current'           then Exit(efkPackCurrent);
  if  L = 'pack_power'             then Exit(efkPackPower);
  if  L = 'capacity_remaining_kwh' then Exit(efkCapacityRemainingKwh);
  if  L = 'capacity_nominal_kwh'   then Exit(efkCapacityNominalKwh);

  // Cell V min / max / avg
  if (L = 'cell_voltage_min') or (L = 'min_cell_voltage') then Exit(efkCellVoltageMin);
  if (L = 'cell_voltage_max') or (L = 'max_cell_voltage') then Exit(efkCellVoltageMax);
  if  L = 'cell_voltage_avg'       then Exit(efkCellVoltageAvg);
  // Cell V arrays (HMG splits across 4 DIDs)
  if (L = 'cell_voltages') or
     (L = 'cell_voltages_1_32') or (L = 'cell_voltages_33_64') or
     (L = 'cell_voltages_65_96') or (L = 'cell_voltages_97_98') then
    Exit(efkCellVoltagesArray);

  // Thermal
  if (L = 'pack_temp_min') or (L = 'battery_min_temp') or
     (L = 'battery_temp_min')                              then Exit(efkPackTempMin);
  if (L = 'pack_temp_max') or (L = 'battery_max_temp') or
     (L = 'battery_temp_max')                              then Exit(efkPackTempMax);
  if  L = 'module_temps'                                   then Exit(efkModuleTempArray);
  // Per-module / per-pack scalar temps -> append into
  // ModuleTempsC.
  if (L = 'module_temp_1') or (L = 'module_temp_2') or
     (L = 'module_temp_3') or (L = 'module_temp_4') or
     (L = 'pack_temp_1')   or (L = 'pack_temp_2')   or
     (L = 'pack_temp_3')   or (L = 'pack_temp_4')          then Exit(efkModuleTempArray);
  if (L = 'inlet_coolant_temp') or
     (L = 'battery_inlet_temp')                            then Exit(efkInletCoolantTemp);
  if  L = 'outlet_coolant_temp'                            then Exit(efkOutletCoolantTemp);

  // Driving
  if  L = 'range_km'               then Exit(efkRangeKm);
  if  L = 'odometer_km'            then Exit(efkOdometerKm);
  if (L = 'charge_state') or (L = 'charging_state_bits') then Exit(efkChargeState);
  if  L = 'charge_port_temp'       then Exit(efkChargePortTemp);
  if  L = 'charging_power_kw'      then Exit(efkChargingPowerKw);

  // Auxiliary / extended
  if  L = 'aux_battery_voltage'             then Exit(efkAuxBatteryVoltage);
  if  L = 'available_charge_power'          then Exit(efkAvailableChargePowerKw);
  if  L = 'available_discharge_power'       then Exit(efkAvailableDischargePowerKw);
  if  L = 'cumulative_energy_charged_kwh'   then Exit(efkCumulativeEnergyChargedKwh);
  if  L = 'cumulative_energy_discharged_kwh' then Exit(efkCumulativeEnergyDischargedKwh);

  Result := efkUnknown;
end;

function FieldKindName(AField: TOBDEVBatteryField): string;
begin
  case AField of
    efkSOC:                   Result := 'soc';
    efkSOH:                   Result := 'soh';
    efkPackVoltage:           Result := 'pack_voltage';
    efkPackCurrent:           Result := 'pack_current';
    efkPackPower:             Result := 'pack_power';
    efkCapacityRemainingKwh:  Result := 'capacity_remaining_kwh';
    efkCapacityNominalKwh:    Result := 'capacity_nominal_kwh';
    efkCellVoltageMin:        Result := 'cell_voltage_min';
    efkCellVoltageMax:        Result := 'cell_voltage_max';
    efkCellVoltageAvg:        Result := 'cell_voltage_avg';
    efkCellVoltagesArray:     Result := 'cell_voltages';
    efkPackTempMin:           Result := 'pack_temp_min';
    efkPackTempMax:           Result := 'pack_temp_max';
    efkModuleTempArray:       Result := 'module_temps';
    efkInletCoolantTemp:      Result := 'inlet_coolant_temp';
    efkOutletCoolantTemp:     Result := 'outlet_coolant_temp';
    efkRangeKm:               Result := 'range_km';
    efkOdometerKm:            Result := 'odometer_km';
    efkChargeState:           Result := 'charge_state';
    efkChargePortTemp:        Result := 'charge_port_temp';
    efkChargingPowerKw:               Result := 'charging_power_kw';
    efkAuxBatteryVoltage:             Result := 'aux_battery_voltage';
    efkAvailableChargePowerKw:        Result := 'available_charge_power';
    efkAvailableDischargePowerKw:     Result := 'available_discharge_power';
    efkCumulativeEnergyChargedKwh:    Result := 'cumulative_energy_charged_kwh';
    efkCumulativeEnergyDischargedKwh: Result := 'cumulative_energy_discharged_kwh';
  else                        Result := 'unknown';
  end;
end;

function ChargeStateFromText(const AText: string): TOBDEVChargeState;
var L: string;
begin
  L := LowerCase(Trim(AText));
  if (L = 'idle') or (L = 'parked') or (L = '0') then Exit(csIdle);
  if (L = 'ac') or (L = 'ac_charging') or (L = 'l1') or (L = 'l2') then Exit(csACCharging);
  if (L = 'dc') or (L = 'dc_fast') or (L = 'dcfc') then Exit(csDCFastCharging);
  if (L = 'drive') or (L = 'driving') then Exit(csDriving);
  if (L = 'regen') or (L = 'regen_braking') then Exit(csRegenBraking);
  Result := csUnknown;
end;

end.
