//------------------------------------------------------------------------------
//  Tests.OBD.Tachograph
//
//  Coverage for the EU 165/2014 tachograph signature + workshop
//  surfaces ported from v1:
//    - TOBDTachographSignatureChecker (TLV parse + chain
//      verification against a permissive in-test verifier).
//    - OBD.Speciality.Tachograph.Workshop encoder / decoder
//      round-trips for every record type.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Tachograph;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Signature,
  OBD.Speciality.Tachograph.Signature,
  OBD.Speciality.Tachograph.Workshop;

type
  /// <summary>
  ///   Test-only signature verifier — accepts every call.
  ///   Drops in for the OpenSSL ECDSA backend so the chain-walk
  ///   logic can be unit-tested without a real key + signature.
  /// </summary>
  TPermissiveVerifier = class(TInterfacedObject, IOBDSignatureVerifier)
  public
    function Supports(AAlgorithm: TOBDSignatureAlgorithm): Boolean;
    function Name: string;
    function Verify(const AArgs: TOBDSignatureVerifyArgs): Boolean;
  end;

  /// <summary>DUnitX fixture for the tachograph surfaces.</summary>
  [TestFixture]
  TTachographTests = class
  public
    [Test] procedure SignatureChecker_ParseBlocksWalksTLV;
    [Test] procedure SignatureChecker_TruncatedRaises;
    [Test] procedure SignatureChecker_ChainWithoutSignatureFails;
    [Test] procedure SignatureChecker_PermissiveChainPasses;

    [Test] procedure Workshop_UTCSyncRoundTrip;
    [Test] procedure Workshop_UTCSyncRejectsBadCardId;
    [Test] procedure Workshop_KLWRoundTrip;
    [Test] procedure Workshop_TyreSizeRoundTrip;
    [Test] procedure Workshop_VINRoundTrip;
    [Test] procedure Workshop_VINRejectsShortString;
    [Test] procedure Workshop_VRPlateRoundTrip;
    [Test] procedure Workshop_VRPlateRejectsOverflow;
    [Test] procedure Workshop_SpeedSourceEncode;
    [Test] procedure Workshop_SealedActivationEncodesNote;
    [Test] procedure Workshop_TimeRealRoundTrip;
  end;

implementation

uses
  System.DateUtils;

{ TPermissiveVerifier }

function TPermissiveVerifier.Supports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  Result := True;
end;

function TPermissiveVerifier.Name: string;
begin
  Result := 'TPermissiveVerifier';
end;

function TPermissiveVerifier.Verify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
begin
  Result := True;
end;

{ ---- helpers -------------------------------------------------------------- }

function BuildTLV(ATag: Word; const AData: TBytes): TBytes;
var
  Len: Integer;
begin
  Len := Length(AData);
  SetLength(Result, 4 + Len);
  Result[0] := Byte((ATag shr 8) and $FF);
  Result[1] := Byte(ATag and $FF);
  Result[2] := Byte((Len shr 8) and $FF);
  Result[3] := Byte(Len and $FF);
  if Len > 0 then
    Move(AData[0], Result[4], Len);
end;

function Concat(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then
    Move(B[0], Result[Length(A)], Length(B));
end;

{ ---- TTachographTests ---------------------------------------------------- }

procedure TTachographTests.SignatureChecker_ParseBlocksWalksTLV;
var
  Checker: TOBDTachographSignatureChecker;
  Buf: TBytes;
  Blocks: TArray<TOBDDDDBlock>;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    // Two consecutive TLV blocks: an Overview ($0501) + a Signature
    // ($050E) — both with 2-byte payloads.
    Buf := Concat(
      BuildTLV($0501, TBytes.Create($AA, $BB)),
      BuildTLV($050E, TBytes.Create($CC, $DD)));
    Blocks := Checker.ParseBlocks(Buf);
    Assert.AreEqual(2, Length(Blocks));
    Assert.AreEqual(Ord(bkOverview),       Ord(Blocks[0].Kind));
    Assert.AreEqual(Ord(bkSignatureBlock), Ord(Blocks[1].Kind));
    Assert.AreEqual(2, Blocks[0].Length_);
    Assert.AreEqual(2, Length(Blocks[0].Data));
    Assert.AreEqual($AA, Integer(Blocks[0].Data[0]));
  finally
    Checker.Free;
  end;
end;

procedure TTachographTests.SignatureChecker_TruncatedRaises;
var
  Checker: TOBDTachographSignatureChecker;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        // Declares 32-byte payload but only ships 4 bytes after the
        // 4-byte TLV header.
        Checker.ParseBlocks(
          TBytes.Create($05, $01, $00, $20, $01, $02, $03, $04));
      end,
      EOBDTachographSignature);
  finally
    Checker.Free;
  end;
end;

procedure TTachographTests.SignatureChecker_ChainWithoutSignatureFails;
var
  Checker: TOBDTachographSignatureChecker;
  Buf: TBytes;
  Outcome: TOBDDDDChainResult;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Checker.SetCardVerifier(TPermissiveVerifier.Create, TBytes.Create($00));
    // Overview block followed by another Overview block (no
    // signature) — chain walk must flag the first failure.
    Buf := Concat(
      BuildTLV($0501, TBytes.Create($01, $02)),
      BuildTLV($0501, TBytes.Create($03, $04)));
    Outcome := Checker.VerifyChain(Buf);
    Assert.IsFalse(Outcome.Verified);
    Assert.AreEqual(0, Outcome.FirstFailureBlockIndex);
    Assert.Contains(Outcome.Reason, 'not followed by a signature');
  finally
    Checker.Free;
  end;
end;

procedure TTachographTests.SignatureChecker_PermissiveChainPasses;
var
  Checker: TOBDTachographSignatureChecker;
  Buf: TBytes;
  Outcome: TOBDDDDChainResult;
begin
  Checker := TOBDTachographSignatureChecker.Create;
  try
    Checker.SetCardVerifier(TPermissiveVerifier.Create, TBytes.Create($00));
    Checker.SetVUVerifier(TPermissiveVerifier.Create, TBytes.Create($00));
    // Two valid pairs: <data><signature><data><signature>.
    Buf := Concat(
      Concat(
        BuildTLV($0501, TBytes.Create($01)),
        BuildTLV($050E, TBytes.Create($AA, $BB))),
      Concat(
        BuildTLV($0521, TBytes.Create($02)),
        BuildTLV($050E, TBytes.Create($CC, $DD))));
    Outcome := Checker.VerifyChain(Buf);
    Assert.IsTrue(Outcome.Verified);
    Assert.AreEqual(2, Outcome.SignaturesVerified);
    Assert.AreEqual(4, Outcome.BlocksParsed);
  finally
    Checker.Free;
  end;
end;

procedure TTachographTests.Workshop_UTCSyncRoundTrip;
var
  Op: TOBDTachoUTCSync;
  Wire: TBytes;
  Back: TOBDTachoUTCSync;
  I: Integer;
begin
  Op := Default(TOBDTachoUTCSync);
  Op.UTCTimestamp := $12345678;
  SetLength(Op.WorkshopCardId, 16);
  for I := 0 to 15 do
    Op.WorkshopCardId[I] := Byte(I * 17);
  Wire := EncodeUTCSync(Op);
  Assert.AreEqual(20, Length(Wire));
  Back := DecodeUTCSync(Wire);
  Assert.AreEqual(Integer($12345678), Integer(Back.UTCTimestamp));
  Assert.AreEqual(16, Length(Back.WorkshopCardId));
  Assert.AreEqual($11, Integer(Back.WorkshopCardId[1]));
end;

procedure TTachographTests.Workshop_UTCSyncRejectsBadCardId;
var
  Op: TOBDTachoUTCSync;
begin
  Op := Default(TOBDTachoUTCSync);
  Op.UTCTimestamp := 0;
  SetLength(Op.WorkshopCardId, 8);                     // wrong length
  Assert.WillRaise(
    procedure
    begin
      EncodeUTCSync(Op);
    end,
    EOBDTachoWorkshop);
end;

procedure TTachographTests.Workshop_KLWRoundTrip;
var
  Op: TOBDTachoKLWFactors;
  Wire: TBytes;
  Back: TOBDTachoKLWFactors;
begin
  Op.K := 8000;
  Op.L := 2050;
  Op.W := 9123;
  Wire := EncodeKLW(Op);
  Assert.AreEqual(6, Length(Wire));
  Back := DecodeKLW(Wire);
  Assert.AreEqual(8000, Integer(Back.K));
  Assert.AreEqual(2050, Integer(Back.L));
  Assert.AreEqual(9123, Integer(Back.W));
end;

procedure TTachographTests.Workshop_TyreSizeRoundTrip;
var
  Op: TOBDTachoTyreSize;
  Wire: TBytes;
  Back: TOBDTachoTyreSize;
begin
  Op.CircumferenceMm := 2150;
  Wire := EncodeTyreSize(Op);
  Assert.AreEqual(2, Length(Wire));
  Back := DecodeTyreSize(Wire);
  Assert.AreEqual(2150, Integer(Back.CircumferenceMm));
end;

procedure TTachographTests.Workshop_VINRoundTrip;
var
  Op: TOBDTachoVINUpdate;
  Wire: TBytes;
  Back: TOBDTachoVINUpdate;
begin
  Op.VIN := 'WVWZZZ1KZAW123456';
  Wire := EncodeVIN(Op);
  Assert.AreEqual(17, Length(Wire));
  Back := DecodeVIN(Wire);
  Assert.AreEqual('WVWZZZ1KZAW123456', Back.VIN);
end;

procedure TTachographTests.Workshop_VINRejectsShortString;
var
  Op: TOBDTachoVINUpdate;
begin
  Op.VIN := 'TOO-SHORT';
  Assert.WillRaise(
    procedure
    begin
      EncodeVIN(Op);
    end,
    EOBDTachoWorkshop);
end;

procedure TTachographTests.Workshop_VRPlateRoundTrip;
var
  Op: TOBDTachoVRPlate;
  Wire: TBytes;
  Back: TOBDTachoVRPlate;
begin
  Op.PlateText := 'AB-123-CD';
  Op.NationalSymbol := 49;                              // FR
  Wire := EncodeVRPlate(Op);
  Assert.AreEqual(2 + Length('AB-123-CD'), Length(Wire));
  Back := DecodeVRPlate(Wire);
  Assert.AreEqual('AB-123-CD', Back.PlateText);
  Assert.AreEqual(49, Integer(Back.NationalSymbol));
end;

procedure TTachographTests.Workshop_VRPlateRejectsOverflow;
var
  Op: TOBDTachoVRPlate;
begin
  Op.PlateText := '12345678901234';                     // 14 chars
  Op.NationalSymbol := 1;
  Assert.WillRaise(
    procedure
    begin
      EncodeVRPlate(Op);
    end,
    EOBDTachoWorkshop);
end;

procedure TTachographTests.Workshop_SpeedSourceEncode;
var
  Op: TOBDTachoSpeedSource;
  Wire: TBytes;
begin
  Op.PulsesPerRevolution := 4096;
  Wire := EncodeSpeedSource(Op);
  Assert.AreEqual(2, Length(Wire));
  Assert.AreEqual($10, Integer(Wire[0]));
  Assert.AreEqual($00, Integer(Wire[1]));
end;

procedure TTachographTests.Workshop_SealedActivationEncodesNote;
var
  Op: TOBDTachoSealedActivation;
  Wire: TBytes;
  I: Integer;
begin
  Op := Default(TOBDTachoSealedActivation);
  Op.UTCTimestamp := $01020304;
  SetLength(Op.WorkshopCardId, 16);
  for I := 0 to 15 do
    Op.WorkshopCardId[I] := $A0 + Byte(I);
  Op.PostSealNote := 'OK';
  Wire := EncodeSealedActivation(Op);
  // 4 + 16 + 2 (note length prefix) + 2 (note bytes).
  Assert.AreEqual(24, Length(Wire));
  Assert.AreEqual($01, Integer(Wire[0]));
  Assert.AreEqual($A0, Integer(Wire[4]));
  Assert.AreEqual($00, Integer(Wire[20]));               // hi byte of note len
  Assert.AreEqual($02, Integer(Wire[21]));               // lo byte = 2
  Assert.AreEqual(Ord('O'), Integer(Wire[22]));
end;

procedure TTachographTests.Workshop_TimeRealRoundTrip;
var
  DT: TDateTime;
  TR: UInt32;
begin
  DT := EncodeDate(2026, 5, 11) + EncodeTime(14, 30, 0, 0);
  TR := DateTimeToTimeReal(DT);
  Assert.IsTrue(TR > 0);
  Assert.AreEqual(0, CompareDateTime(DT, TimeRealToDateTime(TR)));
end;

initialization
  TDUnitX.RegisterTestFixture(TTachographTests);

end.
