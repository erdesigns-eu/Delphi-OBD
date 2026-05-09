//------------------------------------------------------------------------------
//  OBD.Protocol.J1850
//
//  SAE J1850 PWM (Ford) / VPW (GM) foundation. Both flavours share
//  the same application-layer frame shape: a 3-byte header
//  (priority/type + target + source) + service + data + 1-byte CRC.
//  The wire-level differences (PWM vs VPW symbol encoding) are the
//  adapter's responsibility.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1850-2015 (Class B Data Communications Network Interface)
//
//  History     :
//    2026-05-09  ERD  Phase 4 initial: header build + CRC8 +
//                     encode helper.
//------------------------------------------------------------------------------

unit OBD.Protocol.J1850;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.Types;

const
  /// <summary>Functional priority + type byte (61h = priority 3,
  /// functional addressing).</summary>
  J1850_HEADER_FUNC = $61;
  /// <summary>OBD-II functional response broadcast target.</summary>
  J1850_TARGET_BROADCAST = $6A;
  /// <summary>Tester source address.</summary>
  J1850_SOURCE_TESTER = $F1;

type
  /// <summary>Stateless J1850 codec.</summary>
  TOBDJ1850Codec = class
  public
    /// <summary>Computes the J1850 CRC-8 over the supplied bytes.</summary>
    /// <param name="ABytes">Bytes to checksum.</param>
    /// <returns>CRC-8 byte (polynomial 0x1D, init 0xFF, post-XOR
    /// 0xFF).</returns>
    class function CRC8(const ABytes: TBytes): Byte; static;

    /// <summary>
    ///   Encodes a request into a full J1850 frame as a hex string.
    /// </summary>
    /// <param name="ARequest">Request.</param>
    /// <returns>Hex string suitable for an ELM327 J1850-mode write.</returns>
    /// <exception cref="EOBDProtocolErr"><c>ServiceID</c> is zero.</exception>
    class function Encode(const ARequest: TOBDRequest): string; static;
  end;

implementation

{ ---- TOBDJ1850Codec ---------------------------------------------------------- }

class function TOBDJ1850Codec.CRC8(const ABytes: TBytes): Byte;
const
  Poly = $1D;
var
  I, J: Integer;
  CRC: Byte;
begin
  CRC := $FF;
  for I := 0 to High(ABytes) do
  begin
    CRC := CRC xor ABytes[I];
    for J := 0 to 7 do
      if (CRC and $80) <> 0 then
        CRC := Byte((CRC shl 1) xor Poly)
      else
        CRC := Byte(CRC shl 1);
  end;
  Result := CRC xor $FF;
end;

class function TOBDJ1850Codec.Encode(const ARequest: TOBDRequest): string;
var
  Body: TBytes;
  HeadLen: Integer;
begin
  if ARequest.ServiceID = 0 then
    raise EOBDProtocolErr.Create('J1850 Encode: ServiceID is 0');
  HeadLen := 3;
  SetLength(Body, HeadLen + 1 + Length(ARequest.Data) + 1);
  Body[0] := J1850_HEADER_FUNC;
  Body[1] := J1850_TARGET_BROADCAST;
  Body[2] := J1850_SOURCE_TESTER;
  Body[HeadLen] := ARequest.ServiceID;
  if Length(ARequest.Data) > 0 then
    Move(ARequest.Data[0], Body[HeadLen + 1], Length(ARequest.Data));
  Body[High(Body)] := CRC8(Copy(Body, 0, Length(Body) - 1));
  Result := BytesToHex(Body);
end;

end.
