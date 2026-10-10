//------------------------------------------------------------------------------
//  ERD.UI.Units
//
//  Metric / imperial display conversion for the dashboard controls.
//
//  PID decoders always deliver metric engineering units. Dashboard
//  controls keep every stored value, range and threshold in that
//  metric unit and convert only when painting, so switching the
//  unit system never loses precision or changes alert behaviour.
//  Units without an imperial counterpart (rpm, %, V, s, ...) pass
//  through unchanged.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.Units;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.UI.Types;

type
  /// <summary>Linear conversion from a metric unit to its display
  /// unit: <c>Display = Metric * Scale + Offset</c>.</summary>
  TOBDUnitConversion = record
    /// <summary>Unit label shown to the user.</summary>
    DisplayUnit: string;
    /// <summary>Multiplier applied to the metric value.</summary>
    Scale: Double;
    /// <summary>Offset added after scaling (non-zero only for
    /// temperatures).</summary>
    Offset: Double;
    /// <summary>Converts a metric value to the display unit.</summary>
    /// <param name="AValue">Value in the metric unit.</param>
    /// <returns>Value in <see cref="DisplayUnit"/>.</returns>
    function ToDisplay(AValue: Double): Double;
    /// <summary>Converts a display-unit value back to metric.</summary>
    /// <param name="AValue">Value in <see cref="DisplayUnit"/>.</param>
    /// <returns>Value in the metric unit.</returns>
    function FromDisplay(AValue: Double): Double;
    /// <summary>Converts a span (difference between two values).
    /// Ignores the offset.</summary>
    /// <param name="ASpan">Span in the metric unit.</param>
    /// <returns>Span in the display unit.</returns>
    function SpanToDisplay(ASpan: Double): Double;
  end;

const
  /// <summary>Degree sign (U+00B0), kept as a code point so this
  /// source stays pure ASCII.</summary>
  OBD_DEGREE_SIGN = #$00B0;

/// <summary>Resolves how a metric unit is displayed in a unit
/// system.</summary>
/// <param name="AMetricUnit">Unit as delivered by the PID decoder,
/// e.g. <c>'km/h'</c>, <c>'kPa'</c> or the Celsius sign.</param>
/// <param name="ASystem">Target unit system.</param>
/// <returns>Conversion record. Identity (scale 1, offset 0, same
/// unit) when the unit has no imperial counterpart or
/// <c>ASystem = usMetric</c>.</returns>
function OBDUnitConversion(const AMetricUnit: string;
  ASystem: TOBDUnitSystem): TOBDUnitConversion;

/// <summary>Formats a value with a fixed number of decimals using
/// invariant (dot) formatting, independent of the Windows locale so
/// workshop screenshots and saved layouts read the same everywhere.
/// </summary>
/// <param name="AValue">Value to format.</param>
/// <param name="ADecimals">Number of decimals (0..6).</param>
/// <returns>Formatted number without unit.</returns>
function OBDFormatNumber(AValue: Double; ADecimals: Integer): string;

implementation

function TOBDUnitConversion.ToDisplay(AValue: Double): Double;
begin
  Result := AValue * Scale + Offset;
end;

function TOBDUnitConversion.FromDisplay(AValue: Double): Double;
begin
  if Scale = 0 then
    Result := AValue
  else
    Result := (AValue - Offset) / Scale;
end;

function TOBDUnitConversion.SpanToDisplay(ASpan: Double): Double;
begin
  Result := ASpan * Scale;
end;

function Conversion(const AUnit: string; AScale, AOffset: Double)
  : TOBDUnitConversion;
begin
  Result.DisplayUnit := AUnit;
  Result.Scale := AScale;
  Result.Offset := AOffset;
end;

function OBDUnitConversion(const AMetricUnit: string;
  ASystem: TOBDUnitSystem): TOBDUnitConversion;
var
  U: string;
begin
  Result := Conversion(AMetricUnit, 1, 0);
  if ASystem <> usImperial then
    Exit;
  U := LowerCase(Trim(AMetricUnit));
  if (U = OBD_DEGREE_SIGN + 'c') or (U = 'degc') or (U = 'c') then
    Result := Conversion(OBD_DEGREE_SIGN + 'F', 1.8, 32)
  else if U = 'km/h' then
    Result := Conversion('mph', 0.621371192, 0)
  else if U = 'km' then
    Result := Conversion('mi', 0.621371192, 0)
  else if U = 'm' then
    Result := Conversion('ft', 3.280839895, 0)
  else if U = 'mm' then
    Result := Conversion('in', 0.0393700787, 0)
  else if U = 'kpa' then
    Result := Conversion('psi', 0.145037738, 0)
  else if U = 'bar' then
    Result := Conversion('psi', 14.5037738, 0)
  else if U = 'l' then
    Result := Conversion('gal', 0.264172052, 0)
  else if U = 'l/h' then
    Result := Conversion('gal/h', 0.264172052, 0)
  else if U = 'g/s' then
    Result := Conversion('lb/min', 0.132277357, 0)
  else if U = 'nm' then
    Result := Conversion('lb-ft', 0.737562149, 0)
  else if U = 'kw' then
    Result := Conversion('hp', 1.341022090, 0)
  else if U = 'kg' then
    Result := Conversion('lb', 2.204622622, 0);
end;

function OBDFormatNumber(AValue: Double; ADecimals: Integer): string;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  if ADecimals < 0 then
    ADecimals := 0;
  if ADecimals > 6 then
    ADecimals := 6;
  if ADecimals = 0 then
    Result := FormatFloat('0', AValue, FS)
  else
    Result := FormatFloat('0.' + StringOfChar('0', ADecimals), AValue, FS);
end;

end.
