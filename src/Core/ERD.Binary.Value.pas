// ------------------------------------------------------------------------------
// ERD.Binary.Value
//
// Checked integer packing for catalog-driven diagnostics and coding.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-10-08  ERD  Integer width, signedness and big-endian validation.
// ------------------------------------------------------------------------------
unit ERD.Binary.Value;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, ERD.Types;

/// <summary>Encode an integer after validating its physical representation.</summary>
/// <param name="AValue">Integer to encode.</param>
/// <param name="ABytes">Width from one to eight bytes.</param>
/// <param name="ASigned">Use two's-complement signed representation.</param>
/// <returns>Big-endian bytes.</returns>
/// <exception cref="EOBDConfig">Invalid width or value outside its physical range.</exception>
function EncodeIntegerBE(AValue: Int64; ABytes: Integer;
  ASigned: Boolean): TBytes;

/// <summary>Sign-extend an integer bit pattern.</summary>
/// <param name="AValue">Raw bits.</param>
/// <param name="ABits">Width from one to 64 bits.</param>
/// <returns>Signed integer with high bits extended.</returns>
/// <exception cref="EOBDConfig">Invalid width.</exception>
function SignExtendBits(AValue: UInt64; ABits: Integer): Int64;

/// <summary>Decode a bounded, big-endian integer slice.</summary>
/// <param name="AData">Source payload.</param>
/// <param name="AOffset">First byte offset.</param>
/// <param name="ABytes">Width from one to eight bytes.</param>
/// <param name="ASigned">Sign-extend the encoded value.</param>
/// <returns>Integer value.</returns>
/// <exception cref="EOBDConfig">Invalid width, offset, or truncated slice.</exception>
function DecodeIntegerBE(const AData: TBytes; AOffset, ABytes: Integer;
  ASigned: Boolean): Int64;

implementation

function EncodeIntegerBE(AValue: Int64; ABytes: Integer;
  ASigned: Boolean): TBytes;
var
  I, Bits: Integer;
  Bound: Int64;
begin
  if (ABytes < 1) or (ABytes > 8) then
    raise EOBDConfig.Create('Integer byte width must be 1..8');
  Bits := ABytes * 8;
  if ASigned then
  begin
    if Bits < 64 then
    begin
      Bound := Int64(1) shl (Bits - 1);
      if (AValue < -Bound) or (AValue >= Bound) then
        raise EOBDConfig.Create('Signed integer does not fit encoded width');
    end;
  end
  else if (AValue < 0) or
    ((Bits < 64) and (UInt64(AValue) >= (UInt64(1) shl Bits))) then
    raise EOBDConfig.Create('Unsigned integer does not fit encoded width');
  SetLength(Result, ABytes);
  for I := 0 to ABytes - 1 do
    Result[I] := Byte((UInt64(AValue) shr ((ABytes - I - 1) * 8)) and $FF);
end;

function SignExtendBits(AValue: UInt64; ABits: Integer): Int64;
begin
  if (ABits < 1) or (ABits > 64) then
    raise EOBDConfig.Create('Integer bit width must be 1..64');
  if ABits < 64 then
  begin
    AValue := AValue and ((UInt64(1) shl ABits) - 1);
    if (AValue and (UInt64(1) shl (ABits - 1))) <> 0 then
      AValue := AValue or (not UInt64(0) shl ABits);
  end;
  Result := Int64(AValue);
end;

function DecodeIntegerBE(const AData: TBytes; AOffset, ABytes: Integer;
  ASigned: Boolean): Int64;
var
  I: Integer;
  Value: UInt64;
begin
  if (ABytes < 1) or (ABytes > 8) or (AOffset < 0) or (AOffset > Length(AData))
    or (ABytes > Length(AData) - AOffset) then
    raise EOBDConfig.Create
      ('Integer slice exceeds payload or has invalid width');
  Value := 0;
  for I := 0 to ABytes - 1 do
    Value := (Value shl 8) or AData[AOffset + I];
  if ASigned then
    Result := SignExtendBits(Value, ABytes * 8)
  else
  begin
    if Value > UInt64(High(Int64)) then
      raise EOBDConfig.Create('Unsigned value exceeds Int64 result range');
    Result := Int64(Value);
  end;
end;

end.
