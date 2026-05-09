//------------------------------------------------------------------------------
// UNIT           : Tests.Service09.Calibration.pas
// CONTENTS       : Tests for OBD.Service09.Calibration
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Service09.Calibration;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCalibrationTests = class
  public
    [Test] procedure CalIDRequestIsTwoBytes;
    [Test] procedure CVNRequestIsTwoBytes;
    [Test] procedure DecodeCalIDStripsTrailingNulls;
    [Test] procedure DecodeMultiBlockCalIDs;
    [Test] procedure DecodeCalIDRejectsBadServiceId;
    [Test] procedure DecodeCalIDRejectsTruncated;
    [Test] procedure DecodeCVNBigEndianFourBytes;
    [Test] procedure DecodeMultiBlockCVNs;
    [Test] procedure DecodeCVNRejectsBadPID;
    [Test] procedure FormatCVNUpperHex;
    [Test] procedure PairMatchesPositionally;
    [Test] procedure PairMismatchedLengthsRaises;
  end;

implementation

uses
  System.SysUtils, OBD.Service09.Calibration;

procedure TCalibrationTests.CalIDRequestIsTwoBytes;
var R: TBytes;
begin
  R := EncodeCalIDRequest;
  Assert.AreEqual(2, Length(R));
  Assert.AreEqual($09, Integer(R[0]));
  Assert.AreEqual($04, Integer(R[1]));
end;

procedure TCalibrationTests.CVNRequestIsTwoBytes;
var R: TBytes;
begin
  R := EncodeCVNRequest;
  Assert.AreEqual($09, Integer(R[0]));
  Assert.AreEqual($06, Integer(R[1]));
end;

procedure TCalibrationTests.DecodeCalIDStripsTrailingNulls;
var
  Resp: TBytes;
  IDs: TArray<TOBDCalibrationID>;
begin
  Resp := TBytes.Create($49, $04, $01,
    Ord('A'), Ord('B'), Ord('C'), Ord('1'), Ord('2'), Ord('3'),
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  IDs := DecodeCalIDResponse(Resp);
  Assert.AreEqual(1, Length(IDs));
  Assert.AreEqual('ABC123', IDs[0].CalID);
end;

procedure TCalibrationTests.DecodeMultiBlockCalIDs;
var
  Resp: TBytes;
  IDs: TArray<TOBDCalibrationID>;
  I: Integer;
begin
  SetLength(Resp, 3 + 2 * 16);
  Resp[0] := $49; Resp[1] := $04; Resp[2] := 2;
  for I := 0 to 5 do Resp[3 + I] := Ord('1') + I;
  for I := 0 to 5 do Resp[3 + 16 + I] := Ord('A') + I;
  IDs := DecodeCalIDResponse(Resp);
  Assert.AreEqual(2, Length(IDs));
  Assert.AreEqual('123456', IDs[0].CalID);
  Assert.AreEqual('ABCDEF', IDs[1].CalID);
end;

procedure TCalibrationTests.DecodeCalIDRejectsBadServiceId;
begin
  Assert.WillRaise(
    procedure begin DecodeCalIDResponse(TBytes.Create($00, $04, $01)); end,
    EOBDCalibration);
end;

procedure TCalibrationTests.DecodeCalIDRejectsTruncated;
begin
  // Declares 1 block of 16 bytes but only 4 follow
  Assert.WillRaise(
    procedure
    begin
      DecodeCalIDResponse(TBytes.Create($49, $04, $01, $41, $42, $43, $44));
    end,
    EOBDCalibration);
end;

procedure TCalibrationTests.DecodeCVNBigEndianFourBytes;
var
  VNs: TArray<TOBDCalibrationVerification>;
begin
  VNs := DecodeCVNResponse(TBytes.Create($49, $06, $01, $DE, $AD, $BE, $EF));
  Assert.AreEqual(1, Length(VNs));
  Assert.AreEqual(UInt32($DEADBEEF), VNs[0].CVN);
end;

procedure TCalibrationTests.DecodeMultiBlockCVNs;
var
  VNs: TArray<TOBDCalibrationVerification>;
begin
  VNs := DecodeCVNResponse(TBytes.Create(
    $49, $06, $02,
    $11, $22, $33, $44,
    $55, $66, $77, $88));
  Assert.AreEqual(2, Length(VNs));
  Assert.AreEqual(UInt32($11223344), VNs[0].CVN);
  Assert.AreEqual(UInt32($55667788), VNs[1].CVN);
end;

procedure TCalibrationTests.DecodeCVNRejectsBadPID;
begin
  Assert.WillRaise(
    procedure begin DecodeCVNResponse(TBytes.Create($49, $07, $01)); end,
    EOBDCalibration);
end;

procedure TCalibrationTests.FormatCVNUpperHex;
begin
  Assert.AreEqual('DEADBEEF', FormatCVN(UInt32($DEADBEEF)));
  Assert.AreEqual('00000001', FormatCVN(UInt32(1)));
end;

procedure TCalibrationTests.PairMatchesPositionally;
var
  IDs: TArray<TOBDCalibrationID>;
  VNs: TArray<TOBDCalibrationVerification>;
  Pairs: TArray<TOBDCalibrationPair>;
begin
  SetLength(IDs, 2);
  IDs[0].CalID := 'CAL1'; IDs[1].CalID := 'CAL2';
  SetLength(VNs, 2);
  VNs[0].CVN := $AA; VNs[1].CVN := $BB;
  Pairs := PairCalIDsAndCVNs(IDs, VNs);
  Assert.AreEqual('CAL1', Pairs[0].CalID);
  Assert.AreEqual(UInt32($AA), Pairs[0].CVN);
  Assert.AreEqual('CAL2', Pairs[1].CalID);
end;

procedure TCalibrationTests.PairMismatchedLengthsRaises;
var
  IDs: TArray<TOBDCalibrationID>;
  VNs: TArray<TOBDCalibrationVerification>;
begin
  SetLength(IDs, 2);
  SetLength(VNs, 1);
  Assert.WillRaise(
    procedure begin PairCalIDsAndCVNs(IDs, VNs); end,
    EOBDCalibration);
end;

initialization
  TDUnitX.RegisterTestFixture(TCalibrationTests);

end.
