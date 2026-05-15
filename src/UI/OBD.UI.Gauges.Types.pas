//------------------------------------------------------------------------------
//  OBD.UI.Gauges.Types
//
//  Shared records used across the gauge family: zone-coloured
//  value bands, tick configuration, gauge events.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Gauges.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics;

type
  /// <summary>A coloured band on the gauge scale (e.g. red
  /// redline above 7000 RPM, blue cold range below 60°C).
  /// Zones are drawn as a thin arc / strip along the gauge
  /// scale. Multiple zones may overlap; later entries paint
  /// on top.</summary>
  TOBDGaugeZone = record
    StartValue: Double;
    EndValue:   Double;
    Color:      TColor;
  end;

  TOBDGaugeZones = TArray<TOBDGaugeZone>;

  /// <summary>Tick-mark configuration. Major ticks have
  /// labels (when <c>ShowLabels</c> is True); minor ticks are
  /// drawn between majors without labels.</summary>
  TOBDGaugeTickConfig = record
    /// <summary>Value units between major ticks
    /// (e.g. 1000 for an RPM scale 0..8000).</summary>
    MajorInterval:      Double;
    /// <summary>Minor ticks drawn per major span. 0 to skip.</summary>
    MinorTicksPerMajor: Byte;
    /// <summary>Print numeric labels at major ticks.</summary>
    ShowLabels:         Boolean;
    /// <summary>Decimal places on labels.</summary>
    LabelDecimals:      Byte;
  end;

  /// <summary>Fired on every committed value change (after
  /// clamping to <c>Min..Max</c>). Hosts using direct LiveData
  /// binding don't see this event; it's for hosts driving
  /// <c>Value</c> by hand.</summary>
  TOBDGaugeValueEvent = procedure(Sender: TObject;
    AValue: Double) of object;

/// <summary>Builds a one-element zone array (helper).</summary>
function MakeGaugeZone(AStart, AEnd: Double; AColor: TColor): TOBDGaugeZone;

/// <summary>Returns the zone covering <c>AValue</c>, or the
/// zero zone (Start = End = 0) when no zone covers the
/// value. Zones tested in order; first match wins.</summary>
function ResolveZone(const AZones: TOBDGaugeZones;
  AValue: Double): TOBDGaugeZone;

/// <summary>Linear-interpolate <c>AValue</c> (clamped to
/// <c>AMin..AMax</c>) onto the unit interval 0..1.</summary>
function NormaliseValue(AMin, AMax, AValue: Double): Double;

/// <summary>Default tick config for a 0..100 scale.</summary>
function DefaultTickConfig: TOBDGaugeTickConfig;

implementation

uses System.Math;

function MakeGaugeZone(AStart, AEnd: Double; AColor: TColor): TOBDGaugeZone;
begin
  Result.StartValue := AStart;
  Result.EndValue   := AEnd;
  Result.Color      := AColor;
end;

function ResolveZone(const AZones: TOBDGaugeZones;
  AValue: Double): TOBDGaugeZone;
var Z: TOBDGaugeZone;
begin
  for Z in AZones do
    if (AValue >= Z.StartValue) and (AValue <= Z.EndValue) then
      Exit(Z);
  Result := Default(TOBDGaugeZone);
end;

function NormaliseValue(AMin, AMax, AValue: Double): Double;
begin
  if SameValue(AMin, AMax) then Exit(0);
  Result := (AValue - AMin) / (AMax - AMin);
  if Result < 0 then Result := 0
  else if Result > 1 then Result := 1;
end;

function DefaultTickConfig: TOBDGaugeTickConfig;
begin
  Result.MajorInterval      := 10;
  Result.MinorTicksPerMajor := 5;
  Result.ShowLabels         := True;
  Result.LabelDecimals      := 0;
end;

end.
