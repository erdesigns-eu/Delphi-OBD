//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.ComponentProtection.VAG
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.ComponentProtection.VAG;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVAGCPTests = class
  public
    [Test] procedure RequestRoundTrip;
    [Test] procedure ResponseRoundTrip;
    [Test] procedure RequestRejectsBadVIN;
    [Test] procedure RequestDecodeRejectsTruncatedSerial;
    [Test] procedure RequestDecodeRejectsBadVINLength;
    [Test] procedure ResponseDecodeRejectsTruncatedResponse;
    [Test] procedure DefaultSolverFailsClosed;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.ComponentProtection.VAG;

procedure TVAGCPTests.RequestRoundTrip;
var
  In_, Out_: TVAGCPRequest;
  Bytes: TBytes;
begin
  In_.ECUType := $0042;
  In_.ComponentSerial := TBytes.Create($AA, $BB, $CC, $DD);
  In_.VIN := 'WVWZZZ8N8Z1234567';
  In_.Nonce := TBytes.Create($11, $22, $33, $44, $55);
  Bytes := EncodeVAGCPRequest(In_);
  Out_ := DecodeVAGCPRequest(Bytes);
  Assert.AreEqual(Word($0042), Out_.ECUType);
  Assert.AreEqual(4, Length(Out_.ComponentSerial));
  Assert.AreEqual(Integer($AA), Integer(Out_.ComponentSerial[0]));
  Assert.AreEqual('WVWZZZ8N8Z1234567', Out_.VIN);
  Assert.AreEqual(5, Length(Out_.Nonce));
  Assert.AreEqual(Integer($55), Integer(Out_.Nonce[4]));
end;

procedure TVAGCPTests.ResponseRoundTrip;
var
  In_, Out_: TVAGCPResponse;
  Bytes: TBytes;
begin
  In_.Response := TBytes.Create($DE, $AD);
  In_.Signature := TBytes.Create($BE, $EF, $00, $11);
  Bytes := EncodeVAGCPResponse(In_);
  Out_ := DecodeVAGCPResponse(Bytes);
  Assert.AreEqual(2, Length(Out_.Response));
  Assert.AreEqual(4, Length(Out_.Signature));
  Assert.AreEqual(Integer($DE), Integer(Out_.Response[0]));
  Assert.AreEqual(Integer($11), Integer(Out_.Signature[3]));
end;

procedure TVAGCPTests.RequestRejectsBadVIN;
var Req: TVAGCPRequest;
begin
  Req.ECUType := 0;
  Req.VIN := 'TOO-SHORT';
  Assert.WillRaise(
    procedure begin EncodeVAGCPRequest(Req); end, EOBDVAGCP);
end;

procedure TVAGCPTests.RequestDecodeRejectsTruncatedSerial;
var Bytes: TBytes;
begin
  // ECUType=0x0042, serial-len=0x0010, but no body bytes
  Bytes := TBytes.Create($00, $42, $00, $10);
  Assert.WillRaise(
    procedure begin DecodeVAGCPRequest(Bytes); end, EOBDVAGCP);
end;

procedure TVAGCPTests.RequestDecodeRejectsBadVINLength;
var
  Bytes: TBytes;
  I: Integer;
begin
  // Serial empty, VIN length byte declares 16 (must be 17)
  SetLength(Bytes, 5);
  Bytes[0] := $00; Bytes[1] := $00;
  Bytes[2] := $00; Bytes[3] := $00;
  Bytes[4] := $10;
  for I := 0 to 15 do Bytes := Bytes + [Byte(Ord('A'))];
  Assert.WillRaise(
    procedure begin DecodeVAGCPRequest(Bytes); end, EOBDVAGCP);
end;

procedure TVAGCPTests.ResponseDecodeRejectsTruncatedResponse;
begin
  // Declares 4 response bytes but only 2 follow
  Assert.WillRaise(
    procedure
    begin
      DecodeVAGCPResponse(TBytes.Create($00, $04, $AA, $BB));
    end,
    EOBDVAGCP);
end;

procedure TVAGCPTests.DefaultSolverFailsClosed;
var
  Solver: IVAGCPSolver;
  Req: TVAGCPRequest;
begin
  Solver := TVAGCPSolverNotAvailable.Create;
  Req.VIN := 'WVWZZZ8N8Z1234567';
  Assert.WillRaise(
    procedure begin Solver.Solve(Req); end, EOBDVAGCPNoSolver);
end;

initialization
  TDUnitX.RegisterTestFixture(TVAGCPTests);

end.
