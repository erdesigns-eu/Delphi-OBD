//------------------------------------------------------------------------------
//  Tests.OBD.OEM.Support
//
//  Coverage for the smaller OEM support modules:
//    - GoldenVector / CheckGoldenVectors  (OBD.OEM.GoldenCheck)
//    - TOBDHDSessionNegotiator,
//      FormatSPNFMI, ParseDM1DTC          (OBD.OEM.HD)
//    - Mercedes SCN framing               (OBD.OEM.SCN.Mercedes)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.OEM.Support;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.OEM.Types,
  OBD.OEM.DTC,
  OBD.OEM.Session,
  OBD.OEM.Extensions,
  OBD.OEM.GoldenCheck,
  OBD.OEM.HD,
  OBD.OEM.SCN.Mercedes;

type
  /// <summary>Extension used as a golden-check target.</summary>
  TGoldenExt = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(
      var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word;
      const Payload: TBytes): string; override;
  end;

  /// <summary>DUnitX fixture for the support modules.</summary>
  [TestFixture]
  TOEMSupportTests = class
  public
    // ---- GoldenCheck ----
    [Test] procedure Golden_VectorBuilderRoundTrip;
    [Test] procedure Golden_AllPassReturnsEmpty;
    [Test] procedure Golden_EmptyOutputFails;
    [Test] procedure Golden_MissingSubstringFails;
    [Test] procedure Golden_EmptyExpectedAcceptsAnyOutput;

    // ---- HD ----
    [Test] procedure HD_NegotiatorTesterPresentIsThreeSeconds;
    [Test] procedure HD_NegotiatorDisplayName;
    [Test] procedure HD_FormatSPNFMIPadsTo4Digits;
    [Test] procedure HD_ParseDM1DecodesPackedDTC;
    [Test] procedure HD_ParseDM1ShortPayloadReturnsEmpty;
    [Test] procedure HD_SourceAddressConstants;

    // ---- SCN Mercedes ----
    [Test] procedure SCN_VersionRequestRoundTrip;
    [Test] procedure SCN_VersionRequestWrongVINLengthRaises;
    [Test] procedure SCN_VersionRequestWrongByteLengthRaises;
    [Test] procedure SCN_CodingRequestRoundTrip;
    [Test] procedure SCN_CodingResponseRoundTrip;
    [Test] procedure SCN_CodingRequestTruncatedRaises;
    [Test] procedure SCN_NotAvailableSolverRaises;
  end;

implementation

{ TGoldenExt }

function TGoldenExt.ManufacturerKey: string;
begin
  Result := 'GOLD';
end;

function TGoldenExt.DisplayName: string;
begin
  Result := 'Golden Test Extension';
end;

function TGoldenExt.ApplicableToVIN(const VIN: string): Boolean;
begin
  Result := False;
end;

procedure TGoldenExt.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // No catalogue entries — the test extension overrides DecodeDID
  // directly.
end;

function TGoldenExt.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
begin
  case DID of
    $F190: Result := 'VIN: WVWZZZ1KZAW123456';
    $F18C: Result := 'Serial: ABCDEF';
    $DEAD: Result := '';                              // empty path
  else
    Result := Format('DID 0x%.4X', [DID]);
  end;
end;

{ ---- GoldenCheck -------------------------------------------------------- }

procedure TOEMSupportTests.Golden_VectorBuilderRoundTrip;
var
  V: TOBDGoldenVector;
begin
  V := GoldenVector($F190, TBytes.Create($01, $02), 'WVW', 'VIN');
  Assert.AreEqual($F190, Integer(V.DID));
  Assert.AreEqual(2, Length(V.Payload));
  Assert.AreEqual('WVW', V.ExpectedSubstring);
  Assert.AreEqual('VIN', V.Description);
end;

procedure TOEMSupportTests.Golden_AllPassReturnsEmpty;
var
  Ext: IOBDOEMExtension;
  Failures: TArray<TOBDGoldenFailure>;
begin
  Ext := TGoldenExt.Create;
  Failures := CheckGoldenVectors(Ext, TArray<TOBDGoldenVector>.Create(
    GoldenVector($F190, nil, 'WVW', 'VIN'),
    GoldenVector($F18C, nil, 'Serial', 'ECU serial')));
  Assert.AreEqual(0, Length(Failures));
end;

procedure TOEMSupportTests.Golden_EmptyOutputFails;
var
  Ext: IOBDOEMExtension;
  Failures: TArray<TOBDGoldenFailure>;
begin
  Ext := TGoldenExt.Create;
  Failures := CheckGoldenVectors(Ext, TArray<TOBDGoldenVector>.Create(
    GoldenVector($DEAD, nil, 'anything', 'empty path')));
  Assert.AreEqual(1, Length(Failures));
  Assert.Contains(Failures[0].Reason, 'empty output');
end;

procedure TOEMSupportTests.Golden_MissingSubstringFails;
var
  Ext: IOBDOEMExtension;
  Failures: TArray<TOBDGoldenFailure>;
begin
  Ext := TGoldenExt.Create;
  Failures := CheckGoldenVectors(Ext, TArray<TOBDGoldenVector>.Create(
    GoldenVector($F190, nil, 'BMW', 'wrong substring')));
  Assert.AreEqual(1, Length(Failures));
  Assert.Contains(Failures[0].Reason, 'expected output to contain');
end;

procedure TOEMSupportTests.Golden_EmptyExpectedAcceptsAnyOutput;
var
  Ext: IOBDOEMExtension;
  Failures: TArray<TOBDGoldenFailure>;
begin
  Ext := TGoldenExt.Create;
  Failures := CheckGoldenVectors(Ext, TArray<TOBDGoldenVector>.Create(
    GoldenVector($1234, nil, '', 'any non-empty passes')));
  Assert.AreEqual(0, Length(Failures));
end;

{ ---- HD ----------------------------------------------------------------- }

procedure TOEMSupportTests.HD_NegotiatorTesterPresentIsThreeSeconds;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDHDSessionNegotiator.Create;
  Assert.AreEqual<Cardinal>(3000, Neg.DefaultTesterPresentMs);
end;

procedure TOEMSupportTests.HD_NegotiatorDisplayName;
var
  Neg: IOBDSessionNegotiator;
begin
  Neg := TOBDHDSessionNegotiator.Create;
  Assert.Contains(Neg.DisplayName, 'J1939');
end;

procedure TOEMSupportTests.HD_FormatSPNFMIPadsTo4Digits;
begin
  Assert.AreEqual('SPN0094-FMI4', FormatSPNFMI(94, 4));
  Assert.AreEqual('SPN1234-FMI31', FormatSPNFMI(1234, 31));
  Assert.AreEqual('SPN0000-FMI0', FormatSPNFMI(0, 0));
end;

procedure TOEMSupportTests.HD_ParseDM1DecodesPackedDTC;
var
  Code: string;
begin
  // SPN 94 (decimal) = 0x5E = bytes 5E 00 04 00 (FMI=4, no high SPN bits)
  Code := ParseDM1DTC(TBytes.Create($5E, $00, $04, $00), 0);
  Assert.AreEqual('SPN0094-FMI4', Code);
end;

procedure TOEMSupportTests.HD_ParseDM1ShortPayloadReturnsEmpty;
begin
  Assert.AreEqual('', ParseDM1DTC(TBytes.Create($00, $00, $00), 0));
end;

procedure TOEMSupportTests.HD_SourceAddressConstants;
begin
  Assert.AreEqual($00, Integer(J1939_ADDR_ENGINE_1));
  Assert.AreEqual($03, Integer(J1939_ADDR_TRANSMISSION_1));
  Assert.AreEqual($F9, Integer(J1939_ADDR_OFFBOARD_TOOL));
  Assert.AreEqual($FF, Integer(J1939_ADDR_GLOBAL));
end;

{ ---- SCN Mercedes ------------------------------------------------------- }

procedure TOEMSupportTests.SCN_VersionRequestRoundTrip;
var
  Req, Decoded: TMBSCNVersionRequest;
  Wire: TBytes;
begin
  Req := Default(TMBSCNVersionRequest);
  Req.VIN := 'WDD2050461A123456';
  Req.ECUId := $0210;
  Wire := EncodeMBSCNVersionRequest(Req);
  Assert.AreEqual(19, Length(Wire));
  Decoded := DecodeMBSCNVersionRequest(Wire);
  Assert.AreEqual(Req.VIN, Decoded.VIN);
  Assert.AreEqual($0210, Integer(Decoded.ECUId));
end;

procedure TOEMSupportTests.SCN_VersionRequestWrongVINLengthRaises;
begin
  Assert.WillRaise(
    procedure
    var
      Req: TMBSCNVersionRequest;
    begin
      Req.VIN := 'TOO-SHORT';
      Req.ECUId := $0210;
      EncodeMBSCNVersionRequest(Req);
    end,
    EOBDMBSCN);
end;

procedure TOEMSupportTests.SCN_VersionRequestWrongByteLengthRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      DecodeMBSCNVersionRequest(TBytes.Create($00, $01, $02));
    end,
    EOBDMBSCN);
end;

procedure TOEMSupportTests.SCN_CodingRequestRoundTrip;
var
  Req, Decoded: TMBSCNCodingRequest;
  Wire: TBytes;
begin
  Req := Default(TMBSCNCodingRequest);
  Req.VIN := 'WDD2050461A123456';
  Req.ECUId := $0210;
  Req.Variant := TBytes.Create($01, $02, $03);
  Req.AccessoryList := TBytes.Create($FF, $00, $AA, $55);
  Wire := EncodeMBSCNCodingRequest(Req);
  Decoded := DecodeMBSCNCodingRequest(Wire);
  Assert.AreEqual(Req.VIN, Decoded.VIN);
  Assert.AreEqual($0210, Integer(Decoded.ECUId));
  Assert.AreEqual(3, Length(Decoded.Variant));
  Assert.AreEqual($01, Integer(Decoded.Variant[0]));
  Assert.AreEqual($03, Integer(Decoded.Variant[2]));
  Assert.AreEqual(4, Length(Decoded.AccessoryList));
  Assert.AreEqual($AA, Integer(Decoded.AccessoryList[2]));
end;

procedure TOEMSupportTests.SCN_CodingResponseRoundTrip;
var
  Resp, Decoded: TMBSCNCodingResponse;
  Wire: TBytes;
begin
  Resp := Default(TMBSCNCodingResponse);
  Resp.NewSCN := TBytes.Create($DE, $AD, $BE, $EF);
  Resp.ServerSignature := TBytes.Create($CA, $FE);
  Wire := EncodeMBSCNCodingResponse(Resp);
  Decoded := DecodeMBSCNCodingResponse(Wire);
  Assert.AreEqual(4, Length(Decoded.NewSCN));
  Assert.AreEqual($DE, Integer(Decoded.NewSCN[0]));
  Assert.AreEqual($EF, Integer(Decoded.NewSCN[3]));
  Assert.AreEqual(2, Length(Decoded.ServerSignature));
  Assert.AreEqual($CA, Integer(Decoded.ServerSignature[0]));
end;

procedure TOEMSupportTests.SCN_CodingRequestTruncatedRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      DecodeMBSCNCodingRequest(TBytes.Create($00, $01, $02));
    end,
    EOBDMBSCN);
end;

procedure TOEMSupportTests.SCN_NotAvailableSolverRaises;
var
  Solver: IMBSCNSolver;
  Req: TMBSCNVersionRequest;
  CodingReq: TMBSCNCodingRequest;
begin
  Solver := TMBSCNSolverNotAvailable.Create;
  Req := Default(TMBSCNVersionRequest);
  Req.VIN := 'WDD2050461A123456';
  Req.ECUId := $0210;
  Assert.WillRaise(
    procedure
    begin
      Solver.FetchCurrentVersion(Req);
    end,
    EOBDMBSCNNoSolver);
  CodingReq := Default(TMBSCNCodingRequest);
  CodingReq.VIN := Req.VIN;
  CodingReq.ECUId := Req.ECUId;
  Assert.WillRaise(
    procedure
    begin
      Solver.RequestCoding(CodingReq);
    end,
    EOBDMBSCNNoSolver);
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMSupportTests);

end.
