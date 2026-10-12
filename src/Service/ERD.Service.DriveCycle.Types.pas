// ------------------------------------------------------------------------------
// ERD.Service.DriveCycle.Types
//
// Value types used by the drive-cycle advisor.
//
// TOBDMonitor mirrors the readiness-monitor set defined by
// SAE J1979 / ISO 15031-5 Mode 01 PID 01 plus the
// diesel-specific monitors from Table A2. The same enum is
// used for catalogue lookups (per-monitor cycle steps) and for
// decoding the supported / completed bitmaps in
// TOBDMILStatus.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Shared monitor-status decoder (PID 01 / 41, DIDs F401 /
//                  F441) and PID-echo stripping.
// ------------------------------------------------------------------------------

unit ERD.Service.DriveCycle.Types;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF};

type
  TOBDMonitor = (
    // ---- continuous (always supported on OBD-II) ----------
    omMisfire, omFuelSystem, omComprehensive, // "Components" in SAE-speak

    // ---- non-continuous (spark / SI) ----------------------
    omCatalyst, omHeatedCatalyst, omEvaporativeSystem, omSecondaryAirSystem,
    omACRefrigerant, omOxygenSensor, omOxygenSensorHeater, omEGRSystem,

    // ---- non-continuous (diesel / CI) ---------------------
    omNMHCCatalyst, omNOxAftertreatment, omBoostPressureSystem,
    omExhaustGasSensor, omPMFilter, omEGRorVVTSystem);

  TOBDMonitorSet = set of TOBDMonitor;

  /// <summary>One monitor's readiness state at a point in time.</summary>
  TOBDMonitorReadiness = record
    Monitor: TOBDMonitor;
    /// <summary>True when the ECU reports the monitor is
    /// supported on this vehicle.</summary>
    Supported: Boolean;
    /// <summary>True when the monitor has completed since the
    /// last DTC clear.</summary>
    Complete: Boolean;
  end;

  /// <summary>
  /// Decoded SAE J1979 monitor-status bytes A B C D.
  /// </summary>
  /// <remarks>
  /// Same layout for Mode 01 PID 01 (since DTCs were cleared), PID 41
  /// (this drive cycle) and the J1979-2 / WWH-OBD DIDs F401 / F441.
  /// For PID 41 / F441 <c>Supported</c> means "enabled this drive
  /// cycle", and byte A is reserved (<c>MILOn</c> / <c>DTCCount</c>
  /// are then meaningless).
  /// </remarks>
  TOBDMonitorStatus = record
    /// <summary>Malfunction indicator lamp commanded on (A bit 7).</summary>
    MILOn: Boolean;
    /// <summary>Number of emission-related confirmed DTCs (A bits 0-6).</summary>
    DTCCount: Integer;
    /// <summary>Compression-ignition (diesel) monitor layout (B bit 3).</summary>
    CompressionIgnition: Boolean;
    /// <summary>One entry per monitor of the engine type's layout.</summary>
    Monitors: TArray<TOBDMonitorReadiness>;
  end;

  /// <summary>One step in a drive cycle (e.g.
  /// "Cruise at 80 km/h for 5 minutes").</summary>
  TOBDDriveCycleStep = record
    /// <summary>1-based step number for human display.</summary>
    Index: Integer;
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
    Monitor: TOBDMonitor;
    /// <summary>Source citation, free text (e.g.
    /// "ISO 15031-7", "VW SSP-326").</summary>
    Source: string;
    Steps: TArray<TOBDDriveCycleStep>;
    /// <summary>Sum of step durations in seconds.</summary>
    TotalSec: Cardinal;
  end;

  /// <summary>String form for diagnostic logs and JSON-loader
  /// matches.</summary>
function MonitorName(AMonitor: TOBDMonitor): string;

/// <summary>Reverse of <see cref="MonitorName"/>; case-
/// insensitive. Returns False when no enum value matches.</summary>
function TryParseMonitor(const AName: string;
  out AMonitor: TOBDMonitor): Boolean;

/// <summary>
/// Returns the data bytes of a Mode 01 / 02 PID answer without the
/// PID echo.
/// </summary>
/// <param name="AData">Response data after the service ID (the protocol
/// layer keeps the PID echo: <c>01 A B C D</c>).</param>
/// <param name="APID">Requested PID.</param>
/// <param name="ALength">Number of data bytes the PID carries.</param>
/// <returns>The first <c>ALength</c> data bytes (fewer when the answer is
/// short). The echo is removed only when the answer is longer than
/// <c>ALength</c> and starts with <c>APID</c>, so payloads that were
/// already stripped pass through unchanged.</returns>
function OBDPIDPayload(const AData: TBytes; APID: Byte;
  ALength: Integer): TBytes;

/// <summary>
/// Decodes monitor-status bytes A B C D (SAE J1979 PID 01 / 41).
/// </summary>
/// <param name="AData">At least 4 bytes: A B C D (no PID echo).</param>
/// <param name="AStatus">Decoded status.</param>
/// <param name="AError">Reason when the result is False.</param>
/// <returns>False when fewer than 4 bytes were given.</returns>
function DecodeMonitorStatus(const AData: TBytes;
  out AStatus: TOBDMonitorStatus; out AError: string): Boolean;

/// <summary>Number of supported monitors that are not complete.</summary>
/// <param name="AMonitors">Decoded monitors.</param>
/// <returns>Count of entries with <c>Supported</c> and not
/// <c>Complete</c>.</returns>
function CountIncompleteMonitors(
  const AMonitors: TArray<TOBDMonitorReadiness>): Integer;

implementation

const
  MONITOR_NAMES: array [TOBDMonitor] of string = ('Misfire', 'FuelSystem',
    'Comprehensive', 'Catalyst', 'HeatedCatalyst', 'EvaporativeSystem',
    'SecondaryAirSystem', 'ACRefrigerant', 'OxygenSensor', 'OxygenSensorHeater',
    'EGRSystem', 'NMHCCatalyst', 'NOxAftertreatment', 'BoostPressureSystem',
    'ExhaustGasSensor', 'PMFilter', 'EGRorVVTSystem');

function MonitorName(AMonitor: TOBDMonitor): string;
begin
  Result := MONITOR_NAMES[AMonitor];
end;

function TryParseMonitor(const AName: string;
  out AMonitor: TOBDMonitor): Boolean;
var
  M: TOBDMonitor;
begin
  for M := Low(TOBDMonitor) to High(TOBDMonitor) do
    if SameText(MONITOR_NAMES[M], AName) then
    begin
      AMonitor := M;
      Exit(True);
    end;
  Result := False;
end;

function OBDPIDPayload(const AData: TBytes; APID: Byte;
  ALength: Integer): TBytes;
var
  Start: Integer;
begin
  Start := 0;
  if (Length(AData) > ALength) and (AData[0] = APID) then
    Start := 1;
  Result := Copy(AData, Start, ALength);
end;

function DecodeMonitorStatus(const AData: TBytes;
  out AStatus: TOBDMonitorStatus; out AError: string): Boolean;

  procedure Add(AMon: TOBDMonitor; ASup, ACom: Boolean);
  var
    Idx: Integer;
  begin
    Idx := Length(AStatus.Monitors);
    SetLength(AStatus.Monitors, Idx + 1);
    AStatus.Monitors[Idx].Monitor := AMon;
    AStatus.Monitors[Idx].Supported := ASup;
    AStatus.Monitors[Idx].Complete := ACom;
  end;

var
  A, B, C, D: Byte;
begin
  AStatus.MILOn := False;
  AStatus.DTCCount := 0;
  AStatus.CompressionIgnition := False;
  AStatus.Monitors := nil;
  AError := '';
  if Length(AData) < 4 then
  begin
    AError := Format('Monitor status too short (%d bytes)', [Length(AData)]);
    Exit(False);
  end;
  A := AData[0];
  B := AData[1];
  C := AData[2];
  D := AData[3];
  AStatus.MILOn := (A and $80) <> 0;
  AStatus.DTCCount := A and $7F;
  AStatus.CompressionIgnition := (B and $08) <> 0;

  // Continuous monitors: B bits 0-2 supported, bits 4-6 incomplete.
  Add(omMisfire, (B and $01) <> 0, (B and $10) = 0);
  Add(omFuelSystem, (B and $02) <> 0, (B and $20) = 0);
  Add(omComprehensive, (B and $04) <> 0, (B and $40) = 0);

  // Non-continuous monitors: C supported, D incomplete; the bit
  // meaning depends on the engine type (SAE J1979 PID 01 table).
  if AStatus.CompressionIgnition then
  begin
    Add(omNMHCCatalyst, (C and $01) <> 0, (D and $01) = 0);
    Add(omNOxAftertreatment, (C and $02) <> 0, (D and $02) = 0);
    Add(omBoostPressureSystem, (C and $08) <> 0, (D and $08) = 0);
    Add(omExhaustGasSensor, (C and $20) <> 0, (D and $20) = 0);
    Add(omPMFilter, (C and $40) <> 0, (D and $40) = 0);
    Add(omEGRorVVTSystem, (C and $80) <> 0, (D and $80) = 0);
  end
  else
  begin
    Add(omCatalyst, (C and $01) <> 0, (D and $01) = 0);
    Add(omHeatedCatalyst, (C and $02) <> 0, (D and $02) = 0);
    Add(omEvaporativeSystem, (C and $04) <> 0, (D and $04) = 0);
    Add(omSecondaryAirSystem, (C and $08) <> 0, (D and $08) = 0);
    Add(omACRefrigerant, (C and $10) <> 0, (D and $10) = 0);
    Add(omOxygenSensor, (C and $20) <> 0, (D and $20) = 0);
    Add(omOxygenSensorHeater, (C and $40) <> 0, (D and $40) = 0);
    Add(omEGRSystem, (C and $80) <> 0, (D and $80) = 0);
  end;
  Result := True;
end;

function CountIncompleteMonitors(
  const AMonitors: TArray<TOBDMonitorReadiness>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AMonitors) do
    if AMonitors[I].Supported and not AMonitors[I].Complete then
      Inc(Result);
end;

end.
