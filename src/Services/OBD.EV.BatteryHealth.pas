//------------------------------------------------------------------------------
// UNIT           : OBD.EV.BatteryHealth.pas
// CONTENTS       : EV battery state-of-health and cell-imbalance helpers
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.EV.BatteryHealth;

interface

uses
  System.SysUtils, System.Math;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDBatteryHealth = class(Exception);

  /// <summary>Cell-imbalance summary computed from the per-cell array.</summary>
  TOBDCellImbalance = record
    /// <summary>Cell count.</summary>
    CellCount: Integer;
    /// <summary>Min voltage.</summary>
    MinVoltage: Single;
    /// <summary>Max voltage.</summary>
    MaxVoltage: Single;
    /// <summary>Mean voltage.</summary>
    MeanVoltage: Single;
    StdDev: Single;          // population standard deviation
    SpreadVolts: Single;     // Max - Min, the workshop-friendly figure
    OutlierIndex: Integer;   // -1 if no cell deviates > 3 sigma; else its index
    /// <summary>Outlier delta sigma.</summary>
    OutlierDeltaSigma: Single;
  end;

  /// <summary>State-of-health computed from observed pack capacity vs
  /// rated capacity. SoHFromCapacity is the canonical form; the other
  /// fields are intermediate values shown to the workshop UI.</summary>
  TOBDBatterySoH = record
    /// <summary>Rated capacity kwh.</summary>
    RatedCapacityKwh: Single;
    /// <summary>Observed capacity kwh.</summary>
    ObservedCapacityKwh: Single;
    SoHFromCapacity: Single;     // 0..1 (1.0 = brand new)
    /// <summary>Equivalent full cycles.</summary>
    EquivalentFullCycles: Integer;
    DeratingFromTemperature: Single; // 0..1 multiplier; 1.0 = no derating
    CompositeSoH: Single;        // SoHFromCapacity * DeratingFromTemperature
  end;

  /// <summary>One charging-session record. The OEM catalog DIDs that
  /// feed this come in slightly different units across OEMs; the
  /// caller normalises before constructing.</summary>
  TOBDChargingSession = record
    /// <summary>Start so c percent.</summary>
    StartSoCPercent: Single;
    /// <summary>End so c percent.</summary>
    EndSoCPercent: Single;
    /// <summary>Energy delivered kwh.</summary>
    EnergyDeliveredKwh: Single;
    /// <summary>Peak power kw.</summary>
    PeakPowerKw: Single;
    /// <summary>Average battery temp c.</summary>
    AverageBatteryTempC: Single;
    /// <summary>Duration seconds.</summary>
    DurationSeconds: Integer;
    SessionType: string;          // 'AC', 'DC', 'V2L', 'V2G', etc.
  end;

/// <summary>Compute imbalance metrics across the per-cell voltage
/// array (volts). Raises on empty input.</summary>
function ComputeCellImbalance(const CellVolts: array of Single): TOBDCellImbalance;

/// <summary>Compute SoH from a (rated, observed) capacity pair. Both
/// must be positive. Optional temperature derating multiplier in
/// 0..1; default 1.0 (no derating).</summary>
function ComputeBatterySoH(const RatedKwh, ObservedKwh: Single;
  /// <summary>Equivalent full cycles.</summary>
  EquivalentFullCycles: Integer = 0;
  /// <summary>Derating from temperature.</summary>
  DeratingFromTemperature: Single = 1.0): TOBDBatterySoH;

/// <summary>Normalise a charging-session record. Validates the
/// SoC pair (start < end, 0..100) and the duration.</summary>
function NormaliseChargingSession(const Raw: TOBDChargingSession):
  TOBDChargingSession;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// COMPUTE CELL IMBALANCE
//------------------------------------------------------------------------------
function ComputeCellImbalance(const CellVolts: array of Single): TOBDCellImbalance;
var
  I: Integer;
  Sum, SumSq, Diff, Sigma: Double;
  MaxAbs: Double;
begin
  if Length(CellVolts) = 0 then
    raise EOBDBatteryHealth.Create('Need at least one cell voltage');
  Result := Default(TOBDCellImbalance);
  Result.CellCount := Length(CellVolts);
  Result.MinVoltage := CellVolts[0];
  Result.MaxVoltage := CellVolts[0];
  Sum := 0;
  for I := 0 to High(CellVolts) do
  begin
    if CellVolts[I] < Result.MinVoltage then Result.MinVoltage := CellVolts[I];
    if CellVolts[I] > Result.MaxVoltage then Result.MaxVoltage := CellVolts[I];
    Sum := Sum + CellVolts[I];
  end;
  Result.MeanVoltage := Sum / Length(CellVolts);
  Result.SpreadVolts := Result.MaxVoltage - Result.MinVoltage;

  SumSq := 0;
  for I := 0 to High(CellVolts) do
  begin
    Diff := CellVolts[I] - Result.MeanVoltage;
    SumSq := SumSq + Diff * Diff;
  end;
  Result.StdDev := Sqrt(SumSq / Length(CellVolts));

  Result.OutlierIndex := -1;
  Result.OutlierDeltaSigma := 0;
  if Result.StdDev > 0 then
  begin
    MaxAbs := 0;
    for I := 0 to High(CellVolts) do
    begin
      Diff := Abs(CellVolts[I] - Result.MeanVoltage) / Result.StdDev;
      if Diff > MaxAbs then
      begin
        MaxAbs := Diff;
        Result.OutlierIndex := I;
        Result.OutlierDeltaSigma := Diff;
      end;
    end;
    // Only flag if >3-sigma; below that it's noise.
    if MaxAbs <= 3.0 then
    begin
      Result.OutlierIndex := -1;
      Result.OutlierDeltaSigma := MaxAbs;
    end;
  end;
end;

//------------------------------------------------------------------------------
// COMPUTE BATTERY SO H
//------------------------------------------------------------------------------
function ComputeBatterySoH(const RatedKwh, ObservedKwh: Single;
  EquivalentFullCycles: Integer; DeratingFromTemperature: Single): TOBDBatterySoH;
begin
  if RatedKwh <= 0 then
    raise EOBDBatteryHealth.Create('Rated capacity must be positive');
  if ObservedKwh < 0 then
    raise EOBDBatteryHealth.Create('Observed capacity cannot be negative');
  if (DeratingFromTemperature < 0) or (DeratingFromTemperature > 1) then
    raise EOBDBatteryHealth.Create(
      'DeratingFromTemperature must be in [0, 1]');
  Result.RatedCapacityKwh := RatedKwh;
  Result.ObservedCapacityKwh := ObservedKwh;
  Result.SoHFromCapacity := ObservedKwh / RatedKwh;
  if Result.SoHFromCapacity > 1 then
    Result.SoHFromCapacity := 1;
  Result.EquivalentFullCycles := EquivalentFullCycles;
  Result.DeratingFromTemperature := DeratingFromTemperature;
  Result.CompositeSoH := Result.SoHFromCapacity * DeratingFromTemperature;
end;

//------------------------------------------------------------------------------
// NORMALISE CHARGING SESSION
//------------------------------------------------------------------------------
function NormaliseChargingSession(const Raw: TOBDChargingSession):
  TOBDChargingSession;
begin
  Result := Raw;
  if (Raw.StartSoCPercent < 0) or (Raw.StartSoCPercent > 100) then
    raise EOBDBatteryHealth.CreateFmt(
      'StartSoC out of range: %.2f', [Raw.StartSoCPercent]);
  if (Raw.EndSoCPercent < 0) or (Raw.EndSoCPercent > 100) then
    raise EOBDBatteryHealth.CreateFmt(
      'EndSoC out of range: %.2f', [Raw.EndSoCPercent]);
  if Raw.EndSoCPercent < Raw.StartSoCPercent then
    raise EOBDBatteryHealth.Create(
      'EndSoC must be >= StartSoC for a charging session');
  if Raw.DurationSeconds < 0 then
    raise EOBDBatteryHealth.Create('Duration cannot be negative');
end;

end.
