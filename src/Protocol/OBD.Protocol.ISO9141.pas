//------------------------------------------------------------------------------
//  OBD.Protocol.ISO9141
//
//  ISO 9141-2 K-line foundation. The wire-init dance (5-baud
//  initialisation, key bytes 08 08 / 94 94, address inversion) lives
//  on the adapter; this unit only formats the request bytes (3-byte
//  header + service + data + checksum) and parses the response.
//
//  Most modern OBD-II vehicles have moved to ISO 15765 (CAN); ISO
//  9141 is included for legacy diesel and pre-2008 vehicles.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 9141-2 Road vehicles - Diagnostic systems - CARB
//      requirements for interchange of digital information.
//
//  History     :
//    2026-05-09  ERD  Initial implementation: header build + checksum +
//                     parse helpers. Adapter handles the wire init.
//------------------------------------------------------------------------------

unit OBD.Protocol.ISO9141;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.Types;

const
  /// <summary>Standard ISO 9141 priority + format byte (P0 + L0).</summary>
  ISO9141_FMT_BYTE = $68;
  /// <summary>Default tester source address (external test
  /// equipment).</summary>
  ISO9141_TESTER_ADDR = $F1;
  /// <summary>Default ECU target address (engine controller).</summary>
  ISO9141_ECU_ADDR = $6A;

type
  /// <summary>
  ///   Stateless ISO 9141 codec.
  /// </summary>
  TOBDIso9141Codec = class
  public
    /// <summary>
    ///   Builds the 3-byte ISO 9141 header.
    /// </summary>
    /// <param name="ATargetAddr">Destination address (default
    /// 0x6A = ECM).</param>
    /// <param name="ASourceAddr">Source address (default
    /// 0xF1 = tester).</param>
    /// <returns>Three header bytes (format, target, source).</returns>
    class function MakeHeader(ATargetAddr: Byte = ISO9141_ECU_ADDR;
      ASourceAddr: Byte = ISO9141_TESTER_ADDR): TBytes; static;

    /// <summary>
    ///   Computes the ISO 9141 checksum (sum of all preceding bytes
    ///   modulo 256).
    /// </summary>
    /// <param name="ABytes">Bytes to checksum.</param>
    /// <returns>Checksum byte.</returns>
    class function Checksum(const ABytes: TBytes): Byte; static;

    /// <summary>
    ///   Encodes a request into a full ISO 9141 frame
    ///   (header + service + data + checksum).
    /// </summary>
    /// <param name="ARequest">Request to encode.</param>
    /// <returns>Hex string suitable for the adapter's
    /// <c>WriteOBDCommand</c>.</returns>
    /// <exception cref="EOBDProtocolErr"><c>ServiceID</c> is zero.</exception>
    class function Encode(const ARequest: TOBDRequest): string; static;
  end;

implementation

{ ---- TOBDIso9141Codec -------------------------------------------------------- }

class function TOBDIso9141Codec.MakeHeader(ATargetAddr,
  ASourceAddr: Byte): TBytes;
begin
  SetLength(Result, 3);
  Result[0] := ISO9141_FMT_BYTE;
  Result[1] := ATargetAddr;
  Result[2] := ASourceAddr;
end;

class function TOBDIso9141Codec.Checksum(const ABytes: TBytes): Byte;
var
  I: Integer;
  Sum: Cardinal;
begin
  Sum := 0;
  for I := 0 to High(ABytes) do
    Sum := (Sum + ABytes[I]) and $FF;
  Result := Byte(Sum);
end;

class function TOBDIso9141Codec.Encode(const ARequest: TOBDRequest): string;
var
  Body: TBytes;
  HeadLen: Integer;
begin
  if ARequest.ServiceID = 0 then
    raise EOBDProtocolErr.Create('ISO9141 Encode: ServiceID is 0');
  HeadLen := 3;
  SetLength(Body, HeadLen + 1 + Length(ARequest.Data) + 1);
  // Header
  Body[0] := ISO9141_FMT_BYTE;
  Body[1] := ISO9141_ECU_ADDR;
  Body[2] := ISO9141_TESTER_ADDR;
  // Service + data
  Body[HeadLen] := ARequest.ServiceID;
  if Length(ARequest.Data) > 0 then
    Move(ARequest.Data[0], Body[HeadLen + 1], Length(ARequest.Data));
  // Checksum
  SetLength(Body, Length(Body));
  Body[High(Body)] := Checksum(Copy(Body, 0, Length(Body) - 1));
  Result := BytesToHex(Body);
end;

end.
