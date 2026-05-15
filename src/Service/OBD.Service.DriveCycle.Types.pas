//------------------------------------------------------------------------------
//  OBD.Service.DriveCycle.Types
//
//  Value types used by the drive-cycle advisor.
//
//  TOBDMonitor mirrors the readiness-monitor set defined by
//  SAE J1979 / ISO 15031-5 Mode 01 PID 01 plus the
//  diesel-specific monitors from Table A2. The same enum is
//  used for catalogue lookups (per-monitor cycle steps) and for
//  decoding the supported / completed bitmaps in
//  TOBDMILStatus.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Service.DriveCycle.Types;

interface

type
  TOBDMonitor = (
    // ---- continuous (always supported on OBD-II) ----------
    omMisfire,
    omFuelSystem,
    omComprehensive,           // "Components" in SAE-speak

    // ---- non-continuous (spark / SI) ----------------------
    omCatalyst,
    omHeatedCatalyst,
    omEvaporativeSystem,
    omSecondaryAirSystem,
    omACRefrigerant,
    omOxygenSensor,
    omOxygenSensorHeater,
    omEGRSystem,

    // ---- non-continuous (diesel / CI) ---------------------
    omNMHCCatalyst,
    omNOxAftertreatment,
    omBoostPressureSystem,
    omExhaustGasSensor,
    omPMFilter,
    omEGRorVVTSystem
  );

  TOBDMonitorSet = set of TOBDMonitor;

  /// <summary>One monitor's readiness state at a point in time.</summary>
  TOBDMonitorReadiness = record
    Monitor:   TOBDMonitor;
    /// <summary>True when the ECU reports the monitor is
    /// supported on this vehicle.</summary>
    Supported: Boolean;
    /// <summary>True when the monitor has completed since the
    /// last DTC clear.</summary>
    Complete:  Boolean;
  end;

  /// <summary>One step in a drive cycle (e.g.
  /// "Cruise at 80 km/h for 5 minutes").</summary>
  TOBDDriveCycleStep = record
    /// <summary>1-based step number for human display.</summary>
    Index:       Integer;
    /// <summary>Free-text instruction shown to the driver.</summary>
    Description: string;
    /// <summary>Estimated duration in seconds. Hosts use it to
    /// drive a progress bar; the actual readiness flag is the
    /// authoritative completion signal.</summary>
    DurationSec: Cardinal;
  end;

  /// <summary>Complete drive cycle for one monitor: the ordered
  /// steps the driver should perform plus the rolled-up total
  /// duration.</summary>
  TOBDDriveCycle = record
    Monitor:     TOBDMonitor;
    /// <summary>Source citation, free text (e.g.
    /// "ISO 15031-7", "VW SSP-326").</summary>
    Source:      string;
    Steps:       TArray<TOBDDriveCycleStep>;
    /// <summary>Sum of step durations in seconds.</summary>
    TotalSec:    Cardinal;
  end;

/// <summary>String form for diagnostic logs and JSON-loader
/// matches.</summary>
function MonitorName(AMonitor: TOBDMonitor): string;

/// <summary>Reverse of <see cref="MonitorName"/>; case-
/// insensitive. Returns False when no enum value matches.</summary>
function TryParseMonitor(const AName: string;
  out AMonitor: TOBDMonitor): Boolean;

implementation

uses
  System.SysUtils;

const
  MONITOR_NAMES: array[TOBDMonitor] of string = (
    'Misfire',
    'FuelSystem',
    'Comprehensive',
    'Catalyst',
    'HeatedCatalyst',
    'EvaporativeSystem',
    'SecondaryAirSystem',
    'ACRefrigerant',
    'OxygenSensor',
    'OxygenSensorHeater',
    'EGRSystem',
    'NMHCCatalyst',
    'NOxAftertreatment',
    'BoostPressureSystem',
    'ExhaustGasSensor',
    'PMFilter',
    'EGRorVVTSystem'
  );

function MonitorName(AMonitor: TOBDMonitor): string;
begin
  Result := MONITOR_NAMES[AMonitor];
end;

function TryParseMonitor(const AName: string;
  out AMonitor: TOBDMonitor): Boolean;
var M: TOBDMonitor;
begin
  for M := Low(TOBDMonitor) to High(TOBDMonitor) do
    if SameText(MONITOR_NAMES[M], AName) then
    begin
      AMonitor := M;
      Exit(True);
    end;
  Result := False;
end;

end.
