//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.WWHOBD.pas
// CONTENTS       : Tests for OBD.Protocol.WWHOBD
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Protocol.WWHOBD;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TWWHOBDTests = class
  public
    /// <summary>Dtc round trips through pack unpack.</summary>
    [Test] procedure DtcRoundTripsThroughPackUnpack;
    /// <summary>Dtc s p n top bits are preserved.</summary>
    [Test] procedure DtcSPNTopBitsArePreserved;
    /// <summary>Dtc oversized s p n raises.</summary>
    [Test] procedure DtcOversizedSPNRaises;
    /// <summary>Dtc oversized f m i raises.</summary>
    [Test] procedure DtcOversizedFMIRaises;
    /// <summary>Dtc oversized o c raises.</summary>
    [Test] procedure DtcOversizedOCRaises;
    /// <summary>Dtc conversion method only zero or one.</summary>
    [Test] procedure DtcConversionMethodOnlyZeroOrOne;
    /// <summary>Unpack bad length raises.</summary>
    [Test] procedure UnpackBadLengthRaises;
    /// <summary>Unpack stream multiple dtcs.</summary>
    [Test] procedure UnpackStreamMultipleDtcs;
    /// <summary>Unpack stream ragged raises.</summary>
    [Test] procedure UnpackStreamRaggedRaises;
    /// <summary>Dtc as string formats expected shape.</summary>
    [Test] procedure DtcAsStringFormatsExpectedShape;
    /// <summary>Find d i d by v i n returns name.</summary>
    [Test] procedure FindDIDByVINReturnsName;
    /// <summary>Find d i d unknown returns hex label.</summary>
    [Test] procedure FindDIDUnknownReturnsHexLabel;
  end;

implementation

uses
  System.SysUtils, OBD.Protocol.WWHOBD;

procedure TWWHOBDTests.DtcRoundTripsThroughPackUnpack;
var
  In_, Out_: TWWHDtc;
  Bytes: TBytes;
begin
  In_.SPN := 4794;
  In_.FMI := 4;
  In_.OccurrenceCount := 12;
  In_.ConversionMethod := 0;
  Bytes := PackWWHDtc(In_);
  Assert.AreEqual(4, Length(Bytes));
  Out_ := UnpackWWHDtc(Bytes);
  Assert.AreEqual(UInt32(4794), Out_.SPN);
  Assert.AreEqual(Integer(4), Integer(Out_.FMI));
  Assert.AreEqual(Integer(12), Integer(Out_.OccurrenceCount));
  Assert.AreEqual(Integer(0), Integer(Out_.ConversionMethod));
end;

procedure TWWHOBDTests.DtcSPNTopBitsArePreserved;
var
  In_, Out_: TWWHDtc;
begin
  In_.SPN := UInt32($7FFFF); // max 19-bit
  In_.FMI := 0;
  In_.OccurrenceCount := 0;
  In_.ConversionMethod := 1;
  Out_ := UnpackWWHDtc(PackWWHDtc(In_));
  Assert.AreEqual(UInt32($7FFFF), Out_.SPN);
  Assert.AreEqual(Integer(1), Integer(Out_.ConversionMethod));
end;

procedure TWWHOBDTests.DtcOversizedSPNRaises;
var Dtc: TWWHDtc;
begin
  Dtc.SPN := UInt32($80000); // 20-bit
  Dtc.FMI := 0;
  Dtc.OccurrenceCount := 0;
  Dtc.ConversionMethod := 0;
  Assert.WillRaise(procedure begin PackWWHDtc(Dtc); end, EOBDWWHOBD);
end;

procedure TWWHOBDTests.DtcOversizedFMIRaises;
var Dtc: TWWHDtc;
begin
  Dtc.SPN := 100;
  Dtc.FMI := $20;
  Dtc.OccurrenceCount := 0;
  Dtc.ConversionMethod := 0;
  Assert.WillRaise(procedure begin PackWWHDtc(Dtc); end, EOBDWWHOBD);
end;

procedure TWWHOBDTests.DtcOversizedOCRaises;
var Dtc: TWWHDtc;
begin
  Dtc.SPN := 100;
  Dtc.FMI := 0;
  Dtc.OccurrenceCount := $80;
  Dtc.ConversionMethod := 0;
  Assert.WillRaise(procedure begin PackWWHDtc(Dtc); end, EOBDWWHOBD);
end;

procedure TWWHOBDTests.DtcConversionMethodOnlyZeroOrOne;
var Dtc: TWWHDtc;
begin
  Dtc.SPN := 100;
  Dtc.FMI := 0;
  Dtc.OccurrenceCount := 0;
  Dtc.ConversionMethod := 2;
  Assert.WillRaise(procedure begin PackWWHDtc(Dtc); end, EOBDWWHOBD);
end;

procedure TWWHOBDTests.UnpackBadLengthRaises;
begin
  Assert.WillRaise(
    procedure begin UnpackWWHDtc(TBytes.Create($00, $00, $00)); end,
    EOBDWWHOBD);
end;

procedure TWWHOBDTests.UnpackStreamMultipleDtcs;
var
  Stream: TBytes;
  Out_: TArray<TWWHDtc>;
  D1, D2: TWWHDtc;
begin
  D1.SPN := 100; D1.FMI := 4; D1.OccurrenceCount := 1; D1.ConversionMethod := 0;
  D2.SPN := 4794; D2.FMI := 7; D2.OccurrenceCount := 12; D2.ConversionMethod := 0;
  Stream := PackWWHDtc(D1) + PackWWHDtc(D2);
  Out_ := UnpackWWHDtcStream(Stream);
  Assert.AreEqual(2, Length(Out_));
  Assert.AreEqual(UInt32(100), Out_[0].SPN);
  Assert.AreEqual(UInt32(4794), Out_[1].SPN);
end;

procedure TWWHOBDTests.UnpackStreamRaggedRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      UnpackWWHDtcStream(TBytes.Create($00, $00, $00, $00, $00));
    end,
    EOBDWWHOBD);
end;

procedure TWWHOBDTests.DtcAsStringFormatsExpectedShape;
var Dtc: TWWHDtc;
begin
  Dtc.SPN := 4794;
  Dtc.FMI := 4;
  Dtc.OccurrenceCount := 12;
  Dtc.ConversionMethod := 0;
  Assert.AreEqual('SPN 4794, FMI 4 (CM=0, OC=12)', Dtc.AsString);
end;

procedure TWWHOBDTests.FindDIDByVINReturnsName;
var Info: TWWHOBDDataIdentifier;
begin
  Info := FindWWHOBDDataIdentifier(WWHOBD_DID_VIN);
  Assert.AreEqual('VIN', Info.Name);
  Assert.IsNotEmpty(Info.Description);
end;

procedure TWWHOBDTests.FindDIDUnknownReturnsHexLabel;
var Info: TWWHOBDDataIdentifier;
begin
  Info := FindWWHOBDDataIdentifier($1234);
  Assert.IsTrue(Info.Name.Contains('1234'));
end;

initialization
  TDUnitX.RegisterTestFixture(TWWHOBDTests);

end.
