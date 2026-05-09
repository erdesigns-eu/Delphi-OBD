//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SCN.Mercedes.pas
// CONTENTS       : Tests for OBD.OEM.SCN.Mercedes
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.SCN.Mercedes;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMBSCNTests = class
  public
    /// <summary>Version request round trip.</summary>
    [Test] procedure VersionRequestRoundTrip;
    /// <summary>Version request bad length raises.</summary>
    [Test] procedure VersionRequestBadLengthRaises;
    /// <summary>Coding request round trip.</summary>
    [Test] procedure CodingRequestRoundTrip;
    /// <summary>Coding request rejects bad v i n.</summary>
    [Test] procedure CodingRequestRejectsBadVIN;
    /// <summary>Coding response round trip.</summary>
    [Test] procedure CodingResponseRoundTrip;
    /// <summary>Coding response truncated new s c n raises.</summary>
    [Test] procedure CodingResponseTruncatedNewSCNRaises;
    /// <summary>Default solver fetch fails closed.</summary>
    [Test] procedure DefaultSolverFetchFailsClosed;
    /// <summary>Default solver coding fails closed.</summary>
    [Test] procedure DefaultSolverCodingFailsClosed;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.SCN.Mercedes;

procedure TMBSCNTests.VersionRequestRoundTrip;
var
  In_, Out_: TMBSCNVersionRequest;
  Bytes: TBytes;
begin
  In_.VIN := 'WDD2050461R234567';
  In_.ECUId := $00CA;
  Bytes := EncodeMBSCNVersionRequest(In_);
  Assert.AreEqual(19, Length(Bytes));
  Out_ := DecodeMBSCNVersionRequest(Bytes);
  Assert.AreEqual('WDD2050461R234567', Out_.VIN);
  Assert.AreEqual(Word($00CA), Out_.ECUId);
end;

procedure TMBSCNTests.VersionRequestBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeMBSCNVersionRequest(TBytes.Create($00, $00)); end,
    EOBDMBSCN);
end;

procedure TMBSCNTests.CodingRequestRoundTrip;
var
  In_, Out_: TMBSCNCodingRequest;
  Bytes: TBytes;
begin
  In_.VIN := 'WDD2050461R234567';
  In_.ECUId := $0042;
  In_.Variant := TBytes.Create($AA, $BB);
  In_.AccessoryList := TBytes.Create($01, $02, $03);
  Bytes := EncodeMBSCNCodingRequest(In_);
  Out_ := DecodeMBSCNCodingRequest(Bytes);
  Assert.AreEqual('WDD2050461R234567', Out_.VIN);
  Assert.AreEqual(Word($0042), Out_.ECUId);
  Assert.AreEqual(2, Length(Out_.Variant));
  Assert.AreEqual(3, Length(Out_.AccessoryList));
  Assert.AreEqual(Integer($02), Integer(Out_.AccessoryList[1]));
end;

procedure TMBSCNTests.CodingRequestRejectsBadVIN;
var Req: TMBSCNCodingRequest;
begin
  Req.VIN := 'TOO-SHORT';
  Assert.WillRaise(
    procedure begin EncodeMBSCNCodingRequest(Req); end, EOBDMBSCN);
end;

procedure TMBSCNTests.CodingResponseRoundTrip;
var
  In_, Out_: TMBSCNCodingResponse;
  Bytes: TBytes;
begin
  In_.NewSCN := TBytes.Create($DE, $AD, $BE, $EF);
  In_.ServerSignature := TBytes.Create($CA, $FE);
  Bytes := EncodeMBSCNCodingResponse(In_);
  Out_ := DecodeMBSCNCodingResponse(Bytes);
  Assert.AreEqual(4, Length(Out_.NewSCN));
  Assert.AreEqual(2, Length(Out_.ServerSignature));
  Assert.AreEqual(Integer($DE), Integer(Out_.NewSCN[0]));
  Assert.AreEqual(Integer($CA), Integer(Out_.ServerSignature[0]));
end;

procedure TMBSCNTests.CodingResponseTruncatedNewSCNRaises;
var Bytes: TBytes;
begin
  // Declares 4 NewSCN bytes but only 2 follow
  Bytes := TBytes.Create($00, $04, $AA, $BB);
  Assert.WillRaise(
    procedure begin DecodeMBSCNCodingResponse(Bytes); end, EOBDMBSCN);
end;

procedure TMBSCNTests.DefaultSolverFetchFailsClosed;
var
  Solver: IMBSCNSolver;
  Req: TMBSCNVersionRequest;
begin
  Solver := TMBSCNSolverNotAvailable.Create;
  Req.VIN := 'WDD2050461R234567';
  Req.ECUId := 0;
  Assert.WillRaise(
    procedure begin Solver.FetchCurrentVersion(Req); end, EOBDMBSCNNoSolver);
end;

procedure TMBSCNTests.DefaultSolverCodingFailsClosed;
var
  Solver: IMBSCNSolver;
  Req: TMBSCNCodingRequest;
begin
  Solver := TMBSCNSolverNotAvailable.Create;
  Req.VIN := 'WDD2050461R234567';
  Assert.WillRaise(
    procedure begin Solver.RequestCoding(Req); end, EOBDMBSCNNoSolver);
end;

initialization
  TDUnitX.RegisterTestFixture(TMBSCNTests);

end.
