//------------------------------------------------------------------------------
//  OBD.OEM.SCN.Mercedes
//
//  Mercedes-Benz SCN (Software Calibration Number) coding flow
//  framing. Defines the wire-format records the XENTRY back-end
//  exchanges with the diagnostic tool for SCN version-fetch +
//  coding-grant transactions, plus the <see cref="IMBSCNSolver"/>
//  contract a host implements to talk to a real Mercedes dealer
//  portal (or a captured-replay).
//
//  The shipped <see cref="TMBSCNSolverNotAvailable"/>
//  implementation throws on use — production builds plug in their
//  own solver.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial port from v1 OBD.OEM.SCN.Mercedes.
//------------------------------------------------------------------------------

unit OBD.OEM.SCN.Mercedes;

interface

uses
  System.SysUtils;

type
  /// <summary>Raised on SCN framing / encoding errors.</summary>
  EOBDMBSCN = class(Exception);
  /// <summary>Raised when no SCN solver is plugged in.</summary>
  EOBDMBSCNNoSolver = class(EOBDMBSCN);

  /// <summary>SCN version-fetch request.</summary>
  TMBSCNVersionRequest = record
    /// <summary>17-character VIN.</summary>
    VIN: string;
    /// <summary>XENTRY ECU identifier.</summary>
    ECUId: Word;
  end;

  /// <summary>SCN version-fetch response.</summary>
  TMBSCNVersionResponse = record
    /// <summary>Opaque current SCN bytes (typically 8..16).</summary>
    CurrentSCN: TBytes;
    /// <summary>ASCII hardware part number.</summary>
    HardwareNum: string;
    /// <summary>ASCII software part number.</summary>
    SoftwareNum: string;
  end;

  /// <summary>SCN coding-grant request.</summary>
  TMBSCNCodingRequest = record
    /// <summary>17-character VIN.</summary>
    VIN: string;
    /// <summary>XENTRY ECU identifier.</summary>
    ECUId: Word;
    /// <summary>OEM variant code per ECU.</summary>
    Variant: TBytes;
    /// <summary>OEM accessory bitmap / list.</summary>
    AccessoryList: TBytes;
  end;

  /// <summary>SCN coding-grant response.</summary>
  TMBSCNCodingResponse = record
    /// <summary>New opaque SCN bytes.</summary>
    NewSCN: TBytes;
    /// <summary>Server-side signature (opaque).</summary>
    ServerSignature: TBytes;
  end;

  /// <summary>
  ///   Solver contract. Hosts implement this against the
  ///   Mercedes XENTRY back-end (or a captured-replay).
  /// </summary>
  IMBSCNSolver = interface
    ['{0A8F4B2D-8E1C-4D3A-B7E9-1F4C9E8D7A11}']
    /// <summary>Returns the current SCN + part-number triple for
    /// (VIN, ECU).</summary>
    /// <param name="Req">Version-fetch request.</param>
    /// <exception cref="EOBDMBSCNNoSolver">No solver wired
    /// up.</exception>
    function FetchCurrentVersion(
      const Req: TMBSCNVersionRequest): TMBSCNVersionResponse;
    /// <summary>Requests a coding grant for the (VIN, ECU,
    /// variant, accessories) tuple.</summary>
    /// <param name="Req">Coding request.</param>
    /// <exception cref="EOBDMBSCNNoSolver">No solver wired
    /// up.</exception>
    function RequestCoding(
      const Req: TMBSCNCodingRequest): TMBSCNCodingResponse;
  end;

  /// <summary>
  ///   Stand-in solver that raises on use. Useful as a default
  ///   when production builds haven't wired in their real
  ///   dealer-portal client yet.
  /// </summary>
  TMBSCNSolverNotAvailable = class(TInterfacedObject, IMBSCNSolver)
  public
    function FetchCurrentVersion(
      const Req: TMBSCNVersionRequest): TMBSCNVersionResponse;
    function RequestCoding(
      const Req: TMBSCNCodingRequest): TMBSCNCodingResponse;
  end;

/// <summary>Encodes a version-fetch request as
/// <c>VIN(17) + ECUId(BE16)</c>.</summary>
/// <param name="Req">Request.</param>
/// <exception cref="EOBDMBSCN">VIN length is not 17.</exception>
function EncodeMBSCNVersionRequest(
  const Req: TMBSCNVersionRequest): TBytes;
/// <summary>Decodes a 19-byte version-fetch request.</summary>
/// <param name="Bytes">Wire bytes.</param>
/// <exception cref="EOBDMBSCN">Length is not 19.</exception>
function DecodeMBSCNVersionRequest(
  const Bytes: TBytes): TMBSCNVersionRequest;
/// <summary>Encodes a coding request as
/// <c>VIN(17) + ECUId(BE16) + Variant(BE16-LP) +
/// AccessoryList(BE16-LP)</c>.</summary>
/// <param name="Req">Request.</param>
/// <exception cref="EOBDMBSCN">VIN length is not 17 or a payload
/// exceeds 65535 bytes.</exception>
function EncodeMBSCNCodingRequest(
  const Req: TMBSCNCodingRequest): TBytes;
/// <summary>Decodes a coding request.</summary>
/// <param name="Bytes">Wire bytes.</param>
/// <exception cref="EOBDMBSCN">Bytes are short or
/// truncated.</exception>
function DecodeMBSCNCodingRequest(
  const Bytes: TBytes): TMBSCNCodingRequest;
/// <summary>Encodes a coding response as
/// <c>NewSCN(BE16-LP) + ServerSignature(BE16-LP)</c>.</summary>
/// <param name="Resp">Response.</param>
/// <exception cref="EOBDMBSCN">A payload exceeds 65535
/// bytes.</exception>
function EncodeMBSCNCodingResponse(
  const Resp: TMBSCNCodingResponse): TBytes;
/// <summary>Decodes a coding response.</summary>
/// <param name="Bytes">Wire bytes.</param>
/// <exception cref="EOBDMBSCN">Bytes are short or
/// truncated.</exception>
function DecodeMBSCNCodingResponse(
  const Bytes: TBytes): TMBSCNCodingResponse;

implementation

function PutWord(var Out_: TBytes; Cursor: Integer; W: Word): Integer;
begin
  Out_[Cursor]     := Byte(W shr 8);
  Out_[Cursor + 1] := Byte(W and $FF);
  Result := Cursor + 2;
end;

function GetWord(const B: TBytes; Off: Integer): Word;
begin
  Result := (UInt16(B[Off]) shl 8) or B[Off + 1];
end;

function EncodeMBSCNVersionRequest(
  const Req: TMBSCNVersionRequest): TBytes;
var
  I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDMBSCN.CreateFmt(
      'VIN must be 17 chars (got %d)', [Length(Req.VIN)]);
  SetLength(Result, 17 + 2);
  for I := 0 to 16 do
    Result[I] := Byte(Ord(Req.VIN[I + 1]));
  PutWord(Result, 17, Req.ECUId);
end;

function DecodeMBSCNVersionRequest(
  const Bytes: TBytes): TMBSCNVersionRequest;
var
  I: Integer;
begin
  if Length(Bytes) <> 19 then
    raise EOBDMBSCN.CreateFmt(
      'SCN version request must be 19 bytes (got %d)',
      [Length(Bytes)]);
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do
    Result.VIN[I + 1] := Char(Bytes[I]);
  Result.ECUId := GetWord(Bytes, 17);
end;

function EncodeMBSCNCodingRequest(
  const Req: TMBSCNCodingRequest): TBytes;
var
  Cursor, I: Integer;
begin
  if Length(Req.VIN) <> 17 then
    raise EOBDMBSCN.Create('VIN must be 17 chars');
  if Length(Req.Variant) > $FFFF then
    raise EOBDMBSCN.Create('Variant exceeds 65535 bytes');
  if Length(Req.AccessoryList) > $FFFF then
    raise EOBDMBSCN.Create('AccessoryList exceeds 65535 bytes');
  SetLength(Result,
    17 + 2 + 2 + Length(Req.Variant) + 2 + Length(Req.AccessoryList));
  for I := 0 to 16 do
    Result[I] := Byte(Ord(Req.VIN[I + 1]));
  Cursor := 17;
  Cursor := PutWord(Result, Cursor, Req.ECUId);
  Cursor := PutWord(Result, Cursor, Word(Length(Req.Variant)));
  if Length(Req.Variant) > 0 then
  begin
    Move(Req.Variant[0], Result[Cursor], Length(Req.Variant));
    Inc(Cursor, Length(Req.Variant));
  end;
  PutWord(Result, Cursor, Word(Length(Req.AccessoryList)));
  Inc(Cursor, 2);
  if Length(Req.AccessoryList) > 0 then
    Move(Req.AccessoryList[0], Result[Cursor],
      Length(Req.AccessoryList));
end;

function DecodeMBSCNCodingRequest(
  const Bytes: TBytes): TMBSCNCodingRequest;
var
  Cursor, Len, I: Integer;
begin
  if Length(Bytes) < 17 + 2 + 2 + 2 then
    raise EOBDMBSCN.Create('SCN coding request too short');
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do
    Result.VIN[I + 1] := Char(Bytes[I]);
  Cursor := 17;
  Result.ECUId := GetWord(Bytes, Cursor);
  Inc(Cursor, 2);
  Len := GetWord(Bytes, Cursor);
  Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('Variant payload truncated');
  SetLength(Result.Variant, Len);
  if Len > 0 then
    Move(Bytes[Cursor], Result.Variant[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDMBSCN.Create('AccessoryList length missing');
  Len := GetWord(Bytes, Cursor);
  Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('AccessoryList truncated');
  SetLength(Result.AccessoryList, Len);
  if Len > 0 then
    Move(Bytes[Cursor], Result.AccessoryList[0], Len);
end;

function EncodeMBSCNCodingResponse(
  const Resp: TMBSCNCodingResponse): TBytes;
var
  Cursor: Integer;
begin
  if Length(Resp.NewSCN) > $FFFF then
    raise EOBDMBSCN.Create('NewSCN exceeds 65535 bytes');
  if Length(Resp.ServerSignature) > $FFFF then
    raise EOBDMBSCN.Create('ServerSignature exceeds 65535 bytes');
  SetLength(Result,
    2 + Length(Resp.NewSCN) + 2 + Length(Resp.ServerSignature));
  Cursor := PutWord(Result, 0, Word(Length(Resp.NewSCN)));
  if Length(Resp.NewSCN) > 0 then
  begin
    Move(Resp.NewSCN[0], Result[Cursor], Length(Resp.NewSCN));
    Inc(Cursor, Length(Resp.NewSCN));
  end;
  PutWord(Result, Cursor, Word(Length(Resp.ServerSignature)));
  Inc(Cursor, 2);
  if Length(Resp.ServerSignature) > 0 then
    Move(Resp.ServerSignature[0], Result[Cursor],
      Length(Resp.ServerSignature));
end;

function DecodeMBSCNCodingResponse(
  const Bytes: TBytes): TMBSCNCodingResponse;
var
  Cursor, Len: Integer;
begin
  if Length(Bytes) < 4 then
    raise EOBDMBSCN.Create('SCN coding response too short');
  Cursor := 0;
  Len := GetWord(Bytes, Cursor);
  Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('NewSCN truncated');
  SetLength(Result.NewSCN, Len);
  if Len > 0 then
    Move(Bytes[Cursor], Result.NewSCN[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDMBSCN.Create('ServerSignature length missing');
  Len := GetWord(Bytes, Cursor);
  Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDMBSCN.Create('ServerSignature truncated');
  SetLength(Result.ServerSignature, Len);
  if Len > 0 then
    Move(Bytes[Cursor], Result.ServerSignature[0], Len);
end;

{ TMBSCNSolverNotAvailable }

function TMBSCNSolverNotAvailable.FetchCurrentVersion(
  const Req: TMBSCNVersionRequest): TMBSCNVersionResponse;
begin
  raise EOBDMBSCNNoSolver.Create(
    'Mercedes SCN version-fetch solver not available in this build. ' +
    'Plug in a dealer-portal client or a captured (req, resp) replay.');
end;

function TMBSCNSolverNotAvailable.RequestCoding(
  const Req: TMBSCNCodingRequest): TMBSCNCodingResponse;
begin
  raise EOBDMBSCNNoSolver.Create(
    'Mercedes SCN coding solver not available in this build.');
end;

end.
