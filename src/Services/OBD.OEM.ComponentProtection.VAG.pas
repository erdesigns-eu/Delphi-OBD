//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.ComponentProtection.VAG.pas
// CONTENTS       : VAG Component Protection request/response framing
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.ComponentProtection.VAG;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDVAGCP = class(Exception);
  EOBDVAGCPNoSolver = class(EOBDVAGCP);

  TVAGCPRequest = record
    /// <summary>
    ///   Ecu type.
    /// </summary>
    ECUType: Word;
    /// <summary>
    ///   Component serial.
    /// </summary>
    ComponentSerial: TBytes;
    VIN: string;              // 17 ASCII chars, validated
    /// <summary>
    ///   Nonce.
    /// </summary>
    Nonce: TBytes;
  end;

  TVAGCPResponse = record
    /// <summary>
    ///   Response.
    /// </summary>
    Response: TBytes;
    /// <summary>
    ///   Signature.
    /// </summary>
    Signature: TBytes;
  end;

  IVAGCPSolver = interface
    ['{72D7E9F1-6A8D-4D6A-9FBE-9A5B2B8E4C10}']
    /// <summary>
    ///   Forward the challenge envelope to the SVM portal and
    ///   return the activation envelope. Production hosts plug in their
    ///   dealer-portal client here.
    /// </summary>
    function Solve(const Request: TVAGCPRequest): TVAGCPResponse;
  end;

  /// <summary>
  ///   Default solver that fails closed.
  /// </summary>
  TVAGCPSolverNotAvailable = class(TInterfacedObject, IVAGCPSolver)
  public
    /// <summary>
    ///   Solve.
    /// </summary>
    function Solve(const Request: TVAGCPRequest): TVAGCPResponse;
  end;

function EncodeVAGCPRequest(const Request: TVAGCPRequest): TBytes;
function DecodeVAGCPRequest(const Bytes: TBytes): TVAGCPRequest;
function EncodeVAGCPResponse(const Response: TVAGCPResponse): TBytes;
function DecodeVAGCPResponse(const Bytes: TBytes): TVAGCPResponse;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// PUT WORD
//------------------------------------------------------------------------------
function PutWord(var Out_: TBytes; Cursor: Integer; W: Word): Integer;
begin
  Out_[Cursor]     := Byte(W shr 8);
  Out_[Cursor + 1] := Byte(W and $FF);
  Result := Cursor + 2;
end;

//------------------------------------------------------------------------------
// GET WORD
//------------------------------------------------------------------------------
function GetWord(const B: TBytes; Off: Integer): Word;
begin
  Result := (UInt16(B[Off]) shl 8) or B[Off + 1];
end;

//------------------------------------------------------------------------------
// ENCODE VAGCPREQUEST
//------------------------------------------------------------------------------
function EncodeVAGCPRequest(const Request: TVAGCPRequest): TBytes;
var
  Total, Cursor, I: Integer;
begin
  if Length(Request.VIN) <> 17 then
    raise EOBDVAGCP.CreateFmt(
      'VIN must be 17 chars (got %d)', [Length(Request.VIN)]);
  if Length(Request.ComponentSerial) > $FFFF then
    raise EOBDVAGCP.Create('ComponentSerial exceeds 65535 bytes');
  if Length(Request.Nonce) > $FFFF then
    raise EOBDVAGCP.Create('Nonce exceeds 65535 bytes');

  Total := 2                                    // ECUType
         + 2 + Length(Request.ComponentSerial)  // serial-len + serial
         + 1 + 17                               // VIN length + VIN
         + 2 + Length(Request.Nonce);           // nonce-len + nonce
  // Allocate Result
  SetLength(Result, Total);

  Cursor := 0;
  Cursor := PutWord(Result, Cursor, Request.ECUType);
  Cursor := PutWord(Result, Cursor, Word(Length(Request.ComponentSerial)));
  if Length(Request.ComponentSerial) > 0 then
  begin
    Move(Request.ComponentSerial[0], Result[Cursor], Length(Request.ComponentSerial));
    Inc(Cursor, Length(Request.ComponentSerial));
  end;
  Result[Cursor] := 17;
  Inc(Cursor);
  for I := 0 to 16 do
    Result[Cursor + I] := Byte(Ord(Request.VIN[I + 1]));
  Inc(Cursor, 17);
  PutWord(Result, Cursor, Word(Length(Request.Nonce)));
  Inc(Cursor, 2);
  if Length(Request.Nonce) > 0 then
    Move(Request.Nonce[0], Result[Cursor], Length(Request.Nonce));
end;

//------------------------------------------------------------------------------
// DECODE VAGCPREQUEST
//------------------------------------------------------------------------------
function DecodeVAGCPRequest(const Bytes: TBytes): TVAGCPRequest;
var
  Cursor, Len, I: Integer;
begin
  if Length(Bytes) < 5 then
    raise EOBDVAGCP.Create('CP request too short');
  Cursor := 0;
  Result.ECUType := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('ComponentSerial truncated');
  // Allocate Result.ComponentSerial
  SetLength(Result.ComponentSerial, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.ComponentSerial[0], Len);
  Inc(Cursor, Len);
  if Cursor + 1 > Length(Bytes) then
    raise EOBDVAGCP.Create('VIN length byte missing');
  if Bytes[Cursor] <> 17 then
    raise EOBDVAGCP.CreateFmt(
      'VIN length must be 17 (got %d)', [Bytes[Cursor]]);
  Inc(Cursor);
  if Cursor + 17 > Length(Bytes) then
    raise EOBDVAGCP.Create('VIN bytes truncated');
  // Allocate Result.VIN
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[Cursor + I]);
  Inc(Cursor, 17);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDVAGCP.Create('Nonce length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('Nonce truncated');
  // Allocate Result.Nonce
  SetLength(Result.Nonce, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Nonce[0], Len);
end;

//------------------------------------------------------------------------------
// ENCODE VAGCPRESPONSE
//------------------------------------------------------------------------------
function EncodeVAGCPResponse(const Response: TVAGCPResponse): TBytes;
var
  Cursor: Integer;
begin
  if Length(Response.Response) > $FFFF then
    raise EOBDVAGCP.Create('Response exceeds 65535 bytes');
  if Length(Response.Signature) > $FFFF then
    raise EOBDVAGCP.Create('Signature exceeds 65535 bytes');
  SetLength(Result, 2 + Length(Response.Response)
                  + 2 + Length(Response.Signature));
  Cursor := 0;
  Cursor := PutWord(Result, Cursor, Word(Length(Response.Response)));
  if Length(Response.Response) > 0 then
  begin
    Move(Response.Response[0], Result[Cursor], Length(Response.Response));
    Inc(Cursor, Length(Response.Response));
  end;
  PutWord(Result, Cursor, Word(Length(Response.Signature)));
  Inc(Cursor, 2);
  if Length(Response.Signature) > 0 then
    Move(Response.Signature[0], Result[Cursor], Length(Response.Signature));
end;

//------------------------------------------------------------------------------
// DECODE VAGCPRESPONSE
//------------------------------------------------------------------------------
function DecodeVAGCPResponse(const Bytes: TBytes): TVAGCPResponse;
var
  Cursor, Len: Integer;
begin
  if Length(Bytes) < 4 then
    raise EOBDVAGCP.Create('CP response too short');
  Cursor := 0;
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('Response payload truncated');
  // Allocate Result.Response
  SetLength(Result.Response, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Response[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDVAGCP.Create('Signature length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('Signature payload truncated');
  // Allocate Result.Signature
  SetLength(Result.Signature, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Signature[0], Len);
end;

{ TVAGCPSolverNotAvailable }

//------------------------------------------------------------------------------
// SOLVE
//------------------------------------------------------------------------------
function TVAGCPSolverNotAvailable.Solve(const Request: TVAGCPRequest): TVAGCPResponse;
begin
  raise EOBDVAGCPNoSolver.Create(
    'VAG Component Protection solver not available in this build. ' +
    'Plug in a dealer-portal SVM client to authorise component ' +
    'replacements (see docs/DATA_GAPS.md).');
end;

end.
