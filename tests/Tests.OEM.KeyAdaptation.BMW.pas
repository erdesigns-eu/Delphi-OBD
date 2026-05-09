//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.KeyAdaptation.BMW.pas
// CONTENTS       : Tests for OBD.OEM.KeyAdaptation.BMW
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.KeyAdaptation.BMW;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBMWKeyAdaptationTests = class
  public
    /// <summary>Slot validation per generation.</summary>
    [Test] procedure SlotValidationPerGeneration;
    /// <summary>E w s round trip.</summary>
    [Test] procedure EWSRoundTrip;
    /// <summary>C a s round trip.</summary>
    [Test] procedure CASRoundTrip;
    /// <summary>F e m round trip.</summary>
    [Test] procedure FEMRoundTrip;
    /// <summary>E w s bad slot raises.</summary>
    [Test] procedure EWSBadSlotRaises;
    /// <summary>F e m bad slot raises.</summary>
    [Test] procedure FEMBadSlotRaises;
    /// <summary>F e m bad settings bank raises.</summary>
    [Test] procedure FEMBadSettingsBankRaises;
    /// <summary>Decode wrong length raises.</summary>
    [Test] procedure DecodeWrongLengthRaises;
    /// <summary>Digital key serial must be seven bytes.</summary>
    [Test] procedure DigitalKeySerialMustBeSevenBytes;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.KeyAdaptation.BMW;

procedure TBMWKeyAdaptationTests.SlotValidationPerGeneration;
begin
  Assert.IsTrue(ValidateSlotIndex(bmwgEWS, 9));
  Assert.IsFalse(ValidateSlotIndex(bmwgEWS, 10));
  Assert.IsTrue(ValidateSlotIndex(bmwgCAS, 9));
  Assert.IsTrue(ValidateSlotIndex(bmwgFEMBDC, 7));
  Assert.IsFalse(ValidateSlotIndex(bmwgFEMBDC, 8));
end;

procedure TBMWKeyAdaptationTests.EWSRoundTrip;
var
  In_, Out_: TBMWKeyDataE;
  Bytes: TBytes;
begin
  In_.SlotIndex := 3;
  In_.KeyEnabled := True;
  In_.KeyCutCode := TBytes.Create($AA, $BB, $CC, $DD);
  In_.UsageCounter := 1234;
  In_.Reserved := TBytes.Create($00);  // padded by encoder
  Bytes := EncodeKeyDataE(In_);
  Assert.AreEqual(16, Length(Bytes));
  Out_ := DecodeKeyDataE(Bytes);
  Assert.AreEqual(Integer(3), Integer(Out_.SlotIndex));
  Assert.IsTrue(Out_.KeyEnabled);
  Assert.AreEqual(Integer($AA), Integer(Out_.KeyCutCode[0]));
  Assert.AreEqual(Integer($DD), Integer(Out_.KeyCutCode[3]));
  Assert.AreEqual(Word(1234), Out_.UsageCounter);
end;

procedure TBMWKeyAdaptationTests.CASRoundTrip;
var
  In_, Out_: TBMWKeyDataCas;
  Bytes: TBytes;
begin
  In_.SlotIndex := 2;
  In_.KeyEnabled := True;
  In_.KeyCutCode := TBytes.Create($11, $22, $33, $44);
  In_.RemoteId := UInt32($DEADBEEF);
  In_.KMReadingThousands := 87;
  Bytes := EncodeKeyDataCas(In_);
  Assert.AreEqual(16, Length(Bytes));
  Out_ := DecodeKeyDataCas(Bytes);
  Assert.AreEqual(UInt32($DEADBEEF), Out_.RemoteId);
  Assert.AreEqual(Word(87), Out_.KMReadingThousands);
  Assert.AreEqual(Integer($11), Integer(Out_.KeyCutCode[0]));
end;

procedure TBMWKeyAdaptationTests.FEMRoundTrip;
var
  In_, Out_: TBMWKeyDataFem;
  Bytes: TBytes;
begin
  In_.SlotIndex := 5;
  In_.KeyEnabled := True;
  In_.PersonalSettingsBank := 2;
  In_.KeyCutCode := TBytes.Create($11, $22, $33, $44);
  In_.DigitalKeySerial := TBytes.Create($AA, $BB, $CC, $DD, $EE, $FF, $11);
  In_.UsageCounter := UInt32(1500);
  In_.LastKMReading := UInt32(123456);
  Bytes := EncodeKeyDataFem(In_);
  Assert.AreEqual(32, Length(Bytes));
  Out_ := DecodeKeyDataFem(Bytes);
  Assert.AreEqual(Integer(5), Integer(Out_.SlotIndex));
  Assert.AreEqual(Integer(2), Integer(Out_.PersonalSettingsBank));
  Assert.AreEqual(UInt32(1500), Out_.UsageCounter);
  Assert.AreEqual(UInt32(123456), Out_.LastKMReading);
  Assert.AreEqual(Integer($AA), Integer(Out_.DigitalKeySerial[0]));
  Assert.AreEqual(Integer($11), Integer(Out_.DigitalKeySerial[6]));
end;

procedure TBMWKeyAdaptationTests.EWSBadSlotRaises;
var Key: TBMWKeyDataE;
begin
  Key.SlotIndex := 10;
  Key.KeyCutCode := TBytes.Create($00, $00, $00, $00);
  Assert.WillRaise(procedure begin EncodeKeyDataE(Key); end, EOBDBMWKey);
end;

procedure TBMWKeyAdaptationTests.FEMBadSlotRaises;
var Key: TBMWKeyDataFem;
begin
  Key.SlotIndex := 8;
  Key.PersonalSettingsBank := 1;
  Key.KeyCutCode := TBytes.Create($00, $00, $00, $00);
  Key.DigitalKeySerial := TBytes.Create($00, $00, $00, $00, $00, $00, $00);
  Assert.WillRaise(procedure begin EncodeKeyDataFem(Key); end, EOBDBMWKey);
end;

procedure TBMWKeyAdaptationTests.FEMBadSettingsBankRaises;
var Key: TBMWKeyDataFem;
begin
  Key.SlotIndex := 0;
  Key.PersonalSettingsBank := 5;
  Key.KeyCutCode := TBytes.Create($00, $00, $00, $00);
  Key.DigitalKeySerial := TBytes.Create($00, $00, $00, $00, $00, $00, $00);
  Assert.WillRaise(procedure begin EncodeKeyDataFem(Key); end, EOBDBMWKey);
end;

procedure TBMWKeyAdaptationTests.DecodeWrongLengthRaises;
begin
  Assert.WillRaise(
    procedure begin DecodeKeyDataE(TBytes.Create($00, $01)); end,
    EOBDBMWKey);
end;

procedure TBMWKeyAdaptationTests.DigitalKeySerialMustBeSevenBytes;
var Key: TBMWKeyDataFem;
begin
  Key.SlotIndex := 0;
  Key.PersonalSettingsBank := 1;
  Key.KeyCutCode := TBytes.Create($00, $00, $00, $00);
  Key.DigitalKeySerial := TBytes.Create($00, $00, $00);  // wrong length
  Assert.WillRaise(procedure begin EncodeKeyDataFem(Key); end, EOBDBMWKey);
end;

initialization
  TDUnitX.RegisterTestFixture(TBMWKeyAdaptationTests);

end.
