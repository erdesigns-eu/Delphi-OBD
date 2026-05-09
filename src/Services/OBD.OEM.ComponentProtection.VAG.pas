//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.ComponentProtection.VAG.pas
// CONTENTS       : VAG Component Protection (CP) request/response framing.
//                : Used by ODIS / VCDS to authorise a replaced component
//                : (radio, cluster, AC/HVAC, gateway) against the vehicle
//                : via the dealer-side SVM (Service Verification Manager).
//
// Wire format    :
//   Challenge envelope (component -> tester):
//     uint16 ECUType  uint16 ComponentSerialLength  bytes ComponentSerial
//     uint8  VINLength (always 17)                  bytes VIN
//     uint16 NonceLength                            bytes Nonce
//
//   Activation envelope (tester -> component, after SVM):
//     uint16 ResponseLength  bytes Response
//     uint16 SignatureLength bytes Signature
//
// Solver         : The challenge -> response transform is dealer-portal
//                : proprietary. IVAGCPSolver decouples it; the default
//                : TVAGCPSolverNotAvailable raises EOBDVAGCPNoSolver so
//                : code that calls Solve without wiring fails closed.
//------------------------------------------------------------------------------
unit OBD.OEM.ComponentProtection.VAG;

interface

uses
  System.SysUtils;

type
  EOBDVAGCP = class(Exception);
  EOBDVAGCPNoSolver = class(EOBDVAGCP);

  TVAGCPRequest = record
    ECUType: Word;
    ComponentSerial: TBytes;
    VIN: string;              // 17 ASCII chars, validated
    Nonce: TBytes;
  end;

  TVAGCPResponse = record
    Response: TBytes;
    Signature: TBytes;
  end;

  IVAGCPSolver = interface
    ['{72D7E9F1-6A8D-4D6A-9FBE-9A5B2B8E4C10}']
    /// <summary>Forward the challenge envelope to the SVM portal and
    /// return the activation envelope. Production hosts plug in their
    /// dealer-portal client here.</summary>
    function Solve(const Request: TVAGCPRequest): TVAGCPResponse;
  end;

  /// <summary>Default solver that fails closed.</summary>
  TVAGCPSolverNotAvailable = class(TInterfacedObject, IVAGCPSolver)
  public
    function Solve(const Request: TVAGCPRequest): TVAGCPResponse;
  end;

function EncodeVAGCPRequest(const Request: TVAGCPRequest): TBytes;
function DecodeVAGCPRequest(const Bytes: TBytes): TVAGCPRequest;
function EncodeVAGCPResponse(const Response: TVAGCPResponse): TBytes;
function DecodeVAGCPResponse(const Bytes: TBytes): TVAGCPResponse;

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
  SetLength(Result.VIN, 17);
  for I := 0 to 16 do Result.VIN[I + 1] := Char(Bytes[Cursor + I]);
  Inc(Cursor, 17);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDVAGCP.Create('Nonce length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('Nonce truncated');
  SetLength(Result.Nonce, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Nonce[0], Len);
end;

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
  SetLength(Result.Response, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Response[0], Len);
  Inc(Cursor, Len);
  if Cursor + 2 > Length(Bytes) then
    raise EOBDVAGCP.Create('Signature length missing');
  Len := GetWord(Bytes, Cursor); Inc(Cursor, 2);
  if Cursor + Len > Length(Bytes) then
    raise EOBDVAGCP.Create('Signature payload truncated');
  SetLength(Result.Signature, Len);
  if Len > 0 then Move(Bytes[Cursor], Result.Signature[0], Len);
end;

{ TVAGCPSolverNotAvailable }

function TVAGCPSolverNotAvailable.Solve(const Request: TVAGCPRequest): TVAGCPResponse;
begin
  raise EOBDVAGCPNoSolver.Create(
    'VAG Component Protection solver not available in this build. ' +
    'Plug in a dealer-portal SVM client to authorise component ' +
    'replacements (see docs/DATA_GAPS.md).');
end;

end.
