//------------------------------------------------------------------------------
//  OBD.Decoders
//
//  Shared scaling primitives. Every catalogue entry references a decoder
//  by name (<c>'linear'</c>, <c>'percentage'</c>, <c>'temperature'</c>,
//  <c>'fueltrim'</c>, <c>'rpm'</c>, <c>'speed'</c>, <c>'maf'</c>,
//  <c>'ascii'</c>, <c>'bitfield'</c>, <c>'raw'</c>); a global registry
//  resolves the name to a function that turns raw bytes into a
//  <see cref="TOBDValue"/>.
//
//  Decoders are pure: same input bytes + descriptor always produce the
//  same output. This makes them trivial to unit-test against captured
//  fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 15031-5 §7.6 (Scaling and Offset for OBD-II PIDs)
//    - SAE J1979 Appendix A (formula reference)
//
//  History     :
//    2026-05-09  ERD  Initial Phase 1 set: linear, percentage, temperature,
//                     fueltrim, rpm, speed, maf, ascii, bitfield, raw.
//
//  Future work :
//    - Add big-endian-3, signed-2 helpers for UDS DIDs in Phase 6.
//    - Add CCP/XCP scalar decoders driven by A2L conversions in Phase 7.
//------------------------------------------------------------------------------

unit OBD.Decoders;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>
  ///   Signature of a decoder function. Receives raw bytes and the
  ///   catalogue descriptor that triggered the decode; returns a fully
  ///   populated <see cref="TOBDValue"/>.
  /// </summary>
  /// <param name="ARaw">Raw payload returned by the ECU. May be shorter
  /// than <c>ADescriptor.Length</c> on a partial response — decoders
  /// must guard against under-length input.</param>
  /// <param name="ADescriptor">Catalogue descriptor for this entry.</param>
  /// <returns>Decoded value; <c>Kind = vkEmpty</c> when the input is
  /// too short to decode.</returns>
  TOBDDecoderFunc = reference to function(
    const ARaw: TBytes;
    const ADescriptor: TOBDPIDDescriptor): TOBDValue;

  /// <summary>
  ///   Process-wide registry of named decoders.
  /// </summary>
  /// <remarks>
  ///   Initialised in this unit's <c>initialization</c> section with the
  ///   built-in decoders. Other units (OEM packages) may register
  ///   additional decoders at startup. Lookup is case-insensitive.
  /// </remarks>
  TOBDDecoderRegistry = class
  strict private
    class var FInstance: TOBDDecoderRegistry;
    FMap: TDictionary<string, TOBDDecoderFunc>;
    function NormaliseName(const AName: string): string;
  public
    /// <summary>Creates the registry. Use <c>Default</c> for the global
    /// instance.</summary>
    constructor Create;
    destructor Destroy; override;

    /// <summary>The shared process-wide registry.</summary>
    class function Default: TOBDDecoderRegistry;
    /// <summary>Releases the shared instance. Called from this unit's
    /// finalisation.</summary>
    class procedure ReleaseDefault;

    /// <summary>Registers (or replaces) a decoder under a given name.</summary>
    /// <param name="AName">Registry key. Case-insensitive; whitespace
    /// trimmed.</param>
    /// <param name="ADecoder">Decoder function. Must not be <c>nil</c>.</param>
    /// <exception cref="EOBDConfig"><c>AName</c> is empty or
    /// <c>ADecoder</c> is <c>nil</c>.</exception>
    procedure Register(const AName: string; const ADecoder: TOBDDecoderFunc);

    /// <summary>Removes a decoder from the registry. Has no effect if
    /// the name is not present.</summary>
    /// <param name="AName">Registry key to remove.</param>
    procedure Unregister(const AName: string);

    /// <summary>Resolves a name to its decoder function.</summary>
    /// <param name="AName">Registry key. Case-insensitive.</param>
    /// <param name="ADecoder">Output decoder; valid only when the
    /// function returns <c>True</c>.</param>
    /// <returns><c>True</c> if a decoder is registered under
    /// <c>AName</c>.</returns>
    function TryGet(const AName: string; out ADecoder: TOBDDecoderFunc): Boolean;

    /// <summary>Decodes a raw payload by looking up
    /// <c>ADescriptor.DecoderName</c> in the registry.</summary>
    /// <param name="ARaw">Raw bytes from the ECU.</param>
    /// <param name="ADescriptor">Catalogue descriptor with a registered
    /// <c>DecoderName</c>.</param>
    /// <returns>Decoded value. Falls back to the <c>'raw'</c> decoder
    /// if <c>DecoderName</c> is unknown.</returns>
    function Decode(const ARaw: TBytes;
      const ADescriptor: TOBDPIDDescriptor): TOBDValue;
  end;

/// <summary>
///   Built-in linear decoder: <c>scaled = (Raw[0] * Scale) + Offset</c>
///   for one-byte payloads, <c>scaled = (BE16 * Scale) + Offset</c> for
///   two-byte payloads, <c>scaled = (BE32 * Scale) + Offset</c> for
///   four-byte payloads. The byte length is taken from
///   <c>ADescriptor.Length</c>.
/// </summary>
function DecodeLinear(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in percentage decoder: <c>scaled = Raw[0] * 100 / 255</c>.
///   Unit defaults to <c>'%'</c> if the descriptor leaves it empty.
/// </summary>
function DecodePercentage(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in temperature decoder: <c>scaled = Raw[0] - 40</c> (°C).
/// </summary>
function DecodeTemperature(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in fuel trim decoder:
///   <c>scaled = (Raw[0] - 128) * 100 / 128</c> (%).
/// </summary>
function DecodeFuelTrim(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in RPM decoder: <c>scaled = ((Raw[0] * 256) + Raw[1]) / 4</c>
///   (rpm).
/// </summary>
function DecodeRPM(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in vehicle-speed decoder: <c>scaled = Raw[0]</c> (km/h).
/// </summary>
function DecodeSpeed(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in MAF decoder: <c>scaled = ((Raw[0] * 256) + Raw[1]) / 100</c>
///   (g/s).
/// </summary>
function DecodeMAF(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in ASCII decoder. Trims trailing NULs and whitespace.
/// </summary>
function DecodeASCII(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in bit-field decoder: packs the first up to eight bytes of
///   <c>ARaw</c> into <c>UInt64</c>, big-endian.
/// </summary>
function DecodeBitField(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

/// <summary>
///   Built-in passthrough decoder. Sets <c>Kind = vkRawOnly</c> and
///   copies <c>Raw</c>.
/// </summary>
function DecodeRaw(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;

implementation

{ ---- helpers ----------------------------------------------------------------- }

function ApplyClamp(AValue: Double;
  const ADescriptor: TOBDPIDDescriptor): Double;
begin
  Result := AValue;
  // Clamp only when explicitly requested. Min == Max == 0 disables.
  if (ADescriptor.Min <> 0) or (ADescriptor.Max <> 0) then
  begin
    if Result < ADescriptor.Min then
      Result := ADescriptor.Min;
    if Result > ADescriptor.Max then
      Result := ADescriptor.Max;
  end;
end;

function ReadBE(const ARaw: TBytes; AStart, ALen: Integer): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALen - 1 do
    Result := (Result shl 8) or ARaw[AStart + I];
end;

function FillCommon(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := MakeOBDValue;
  Result.Raw := Copy(ARaw);
  Result.UnitName := ADescriptor.UnitName;
  Result.Name := ADescriptor.Name;
end;

{ ---- decoders ---------------------------------------------------------------- }

function DecodeLinear(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
var
  Required: Integer;
  RawBE: UInt64;
  Scale: Double;
begin
  Result := FillCommon(ARaw, ADescriptor);
  Required := ADescriptor.Length;
  if Required = 0 then
    Required := 1;
  if Length(ARaw) < Required then
    Exit;
  RawBE := ReadBE(ARaw, 0, Required);
  Scale := ADescriptor.Scale;
  if Scale = 0 then
    Scale := 1;
  Result.Kind := vkFloat;
  Result.AsFloat := ApplyClamp(RawBE * Scale + ADescriptor.Offset, ADescriptor);
end;

function DecodePercentage(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 1 then
    Exit;
  Result.Kind := vkFloat;
  Result.AsFloat := ApplyClamp(ARaw[0] * 100.0 / 255.0, ADescriptor);
  if Result.UnitName = '' then
    Result.UnitName := '%';
end;

function DecodeTemperature(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 1 then
    Exit;
  Result.Kind := vkInteger;
  Result.AsInteger := ARaw[0] - 40;
  if Result.UnitName = '' then
    Result.UnitName := '°C';
end;

function DecodeFuelTrim(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 1 then
    Exit;
  Result.Kind := vkFloat;
  Result.AsFloat := ApplyClamp((ARaw[0] - 128.0) * 100.0 / 128.0, ADescriptor);
  if Result.UnitName = '' then
    Result.UnitName := '%';
end;

function DecodeRPM(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 2 then
    Exit;
  Result.Kind := vkFloat;
  Result.AsFloat := ApplyClamp(((ARaw[0] shl 8) or ARaw[1]) / 4.0, ADescriptor);
  if Result.UnitName = '' then
    Result.UnitName := 'rpm';
end;

function DecodeSpeed(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 1 then
    Exit;
  Result.Kind := vkInteger;
  Result.AsInteger := ARaw[0];
  if Result.UnitName = '' then
    Result.UnitName := 'km/h';
end;

function DecodeMAF(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) < 2 then
    Exit;
  Result.Kind := vkFloat;
  Result.AsFloat := ApplyClamp(((ARaw[0] shl 8) or ARaw[1]) / 100.0, ADescriptor);
  if Result.UnitName = '' then
    Result.UnitName := 'g/s';
end;

function DecodeASCII(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
var
  S: string;
  I: Integer;
begin
  Result := FillCommon(ARaw, ADescriptor);
  SetLength(S, Length(ARaw));
  for I := 0 to Length(ARaw) - 1 do
    S[I + 1] := Char(ARaw[I]);
  // Strip control / NUL pad bytes that some ECUs append.
  while (Length(S) > 0) and ((S[Length(S)] = #0) or (S[Length(S)] = ' ')) do
    SetLength(S, Length(S) - 1);
  Result.Kind := vkString;
  Result.AsString := S;
end;

function DecodeBitField(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
var
  Take: Integer;
begin
  Result := FillCommon(ARaw, ADescriptor);
  if Length(ARaw) = 0 then
    Exit;
  Take := Length(ARaw);
  if Take > 8 then
    Take := 8;
  Result.Kind := vkBitField;
  Result.AsBitField := ReadBE(ARaw, 0, Take);
end;

function DecodeRaw(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
begin
  Result := FillCommon(ARaw, ADescriptor);
  Result.Kind := vkRawOnly;
end;

{ ---- TOBDDecoderRegistry ----------------------------------------------------- }

constructor TOBDDecoderRegistry.Create;
begin
  inherited Create;
  FMap := TDictionary<string, TOBDDecoderFunc>.Create;
end;

destructor TOBDDecoderRegistry.Destroy;
begin
  FMap.Free;
  inherited;
end;

class function TOBDDecoderRegistry.Default: TOBDDecoderRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDDecoderRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDDecoderRegistry.ReleaseDefault;
begin
  FreeAndNil(FInstance);
end;

function TOBDDecoderRegistry.NormaliseName(const AName: string): string;
begin
  Result := LowerCase(Trim(AName));
end;

procedure TOBDDecoderRegistry.Register(const AName: string;
  const ADecoder: TOBDDecoderFunc);
var
  Key: string;
begin
  Key := NormaliseName(AName);
  if Key = '' then
    raise EOBDConfig.Create('Decoder name cannot be empty');
  if not Assigned(ADecoder) then
    raise EOBDConfig.CreateFmt('Decoder for "%s" is nil', [AName]);
  FMap.AddOrSetValue(Key, ADecoder);
end;

procedure TOBDDecoderRegistry.Unregister(const AName: string);
begin
  FMap.Remove(NormaliseName(AName));
end;

function TOBDDecoderRegistry.TryGet(const AName: string;
  out ADecoder: TOBDDecoderFunc): Boolean;
begin
  Result := FMap.TryGetValue(NormaliseName(AName), ADecoder);
end;

function TOBDDecoderRegistry.Decode(const ARaw: TBytes;
  const ADescriptor: TOBDPIDDescriptor): TOBDValue;
var
  Decoder: TOBDDecoderFunc;
begin
  if not TryGet(CanonicalDecoderName(ADescriptor), Decoder) then
    if not TryGet('raw', Decoder) then
      raise EOBDInternal.Create('Built-in "raw" decoder is not registered');
  Result := Decoder(ARaw, ADescriptor);
end;

{ ---- registration ------------------------------------------------------------ }

procedure RegisterBuiltins;
var
  R: TOBDDecoderRegistry;
begin
  R := TOBDDecoderRegistry.Default;
  R.Register('linear',      DecodeLinear);
  R.Register('percentage',  DecodePercentage);
  R.Register('temperature', DecodeTemperature);
  R.Register('fueltrim',    DecodeFuelTrim);
  R.Register('rpm',         DecodeRPM);
  R.Register('speed',       DecodeSpeed);
  R.Register('maf',         DecodeMAF);
  R.Register('ascii',       DecodeASCII);
  R.Register('bitfield',    DecodeBitField);
  R.Register('raw',         DecodeRaw);
end;

initialization
  RegisterBuiltins;

finalization
  TOBDDecoderRegistry.ReleaseDefault;

end.
