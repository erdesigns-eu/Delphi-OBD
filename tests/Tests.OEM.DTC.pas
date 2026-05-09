//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.DTC
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.DTC;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDtcEncodingTests = class
  public
    /// <summary>Format powertrain code.</summary>
    [Test] procedure FormatPowertrainCode;
    /// <summary>Format chassis code.</summary>
    [Test] procedure FormatChassisCode;
    /// <summary>Format body code.</summary>
    [Test] procedure FormatBodyCode;
    /// <summary>Format network code.</summary>
    [Test] procedure FormatNetworkCode;
    /// <summary>Format manufacturer code.</summary>
    [Test] procedure FormatManufacturerCode;
    /// <summary>Encode round trips p0301.</summary>
    [Test] procedure EncodeRoundTripsP0301;
    /// <summary>Encode round trips manufacturer.</summary>
    [Test] procedure EncodeRoundTripsManufacturer;
    /// <summary>Encode rejects short input.</summary>
    [Test] procedure EncodeRejectsShortInput;
    /// <summary>Encode rejects bad letter.</summary>
    [Test] procedure EncodeRejectsBadLetter;
    /// <summary>Encode rejects bad group digit.</summary>
    [Test] procedure EncodeRejectsBadGroupDigit;
    /// <summary>Is manufacturer dtc recognises p1 and p3.</summary>
    [Test] procedure IsManufacturerDtcRecognisesP1AndP3;
    /// <summary>Is manufacturer dtc rejects s a e.</summary>
    [Test] procedure IsManufacturerDtcRejectsSAE;
    /// <summary>Severity round trip.</summary>
    [Test] procedure SeverityRoundTrip;
  end;

  [TestFixture]
  TDtcCatalogTests = class
  public
    /// <summary>Loads top level dtc array.</summary>
    [Test] procedure LoadsTopLevelDtcArray;
    /// <summary>Loads bare j s o n array.</summary>
    [Test] procedure LoadsBareJSONArray;
    /// <summary>Lookup is case insensitive.</summary>
    [Test] procedure LookupIsCaseInsensitive;
    /// <summary>Replaces duplicate code.</summary>
    [Test] procedure ReplacesDuplicateCode;
    /// <summary>Captures possible causes and hints.</summary>
    [Test] procedure CapturesPossibleCausesAndHints;
    /// <summary>Default source propagates to entries.</summary>
    [Test] procedure DefaultSourcePropagatesToEntries;
    /// <summary>Verified flag defaults to false.</summary>
    [Test] procedure VerifiedFlagDefaultsToFalse;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.JSON,
  OBD.OEM.DTC;

//==============================================================================
// Encoding
//==============================================================================
procedure TDtcEncodingTests.FormatPowertrainCode;
begin
  // 0x03 0x01 → P0301 (cylinder 1 misfire).
  Assert.AreEqual('P0301', FormatDtc($03, $01));
end;

procedure TDtcEncodingTests.FormatChassisCode;
begin
  Assert.AreEqual('C0561', FormatDtc($45, $61));
end;

procedure TDtcEncodingTests.FormatBodyCode;
begin
  Assert.AreEqual('B1318', FormatDtc($93, $18));
end;

procedure TDtcEncodingTests.FormatNetworkCode;
begin
  Assert.AreEqual('U0100', FormatDtc($C1, $00));
end;

procedure TDtcEncodingTests.FormatManufacturerCode;
begin
  // P1296 — first digit 1 (manufacturer): bit5=0, bit4=1 → high byte 0x12.
  Assert.AreEqual('P1296', FormatDtc($12, $96));
  // P3000 — first digit 3 (mfr): bit5=1, bit4=1 → high byte 0x30.
  Assert.AreEqual('P3000', FormatDtc($30, $00));
end;

procedure TDtcEncodingTests.EncodeRoundTripsP0301;
var
  B: TBytes;
begin
  B := EncodeDtc('P0301');
  Assert.AreEqual(Byte($03), B[0]);
  Assert.AreEqual(Byte($01), B[1]);
end;

procedure TDtcEncodingTests.EncodeRoundTripsManufacturer;
var
  B: TBytes;
begin
  B := EncodeDtc('P1296');
  Assert.AreEqual('P1296', FormatDtc(B));
end;

procedure TDtcEncodingTests.EncodeRejectsShortInput;
begin
  Assert.WillRaise(
    procedure begin EncodeDtc('P030'); end, EOBDDtcError);
end;

procedure TDtcEncodingTests.EncodeRejectsBadLetter;
begin
  Assert.WillRaise(
    procedure begin EncodeDtc('Z0301'); end, EOBDDtcError);
end;

procedure TDtcEncodingTests.EncodeRejectsBadGroupDigit;
begin
  Assert.WillRaise(
    procedure begin EncodeDtc('P9999'); end, EOBDDtcError);
end;

procedure TDtcEncodingTests.IsManufacturerDtcRecognisesP1AndP3;
begin
  Assert.IsTrue(IsManufacturerDtc('P1296'));
  Assert.IsTrue(IsManufacturerDtc('B3000'));
end;

procedure TDtcEncodingTests.IsManufacturerDtcRejectsSAE;
begin
  Assert.IsFalse(IsManufacturerDtc('P0301'));
  Assert.IsFalse(IsManufacturerDtc('P2173'));
end;

procedure TDtcEncodingTests.SeverityRoundTrip;
begin
  Assert.AreEqual(Ord(dtcSeverityCritical), Ord(ParseSeverity('critical')));
  Assert.AreEqual(Ord(dtcSeverityWarning),  Ord(ParseSeverity('Warning')));
  Assert.AreEqual(Ord(dtcSeverityInfo),     Ord(ParseSeverity('info')));
  Assert.AreEqual(Ord(dtcSeverityUnknown),  Ord(ParseSeverity('made-up')));
  Assert.AreEqual('critical', FormatSeverity(dtcSeverityCritical));
end;

//==============================================================================
// Catalog
//==============================================================================
const
  TOP_LEVEL_DTCS: string =
    '{"version": 1, "default_source": "test-source", "dtcs": [' +
    '{"code": "P0301", "severity": "critical", "description": "Cyl 1 misfire", ' +
    ' "possible_causes": ["bad coil", "bad plug"], "repair_hints": "swap to confirm",' +
    ' "verified": true},' +
    '{"code": "P1296", "severity": "warning", "description": "VAG cooling fault"}' +
    ']}';

  BARE_ARRAY: string =
    '[' +
    '{"code": "P0420", "description": "Cat efficiency low"},' +
    '{"code": "U0100", "description": "Lost comm with ECM"}' +
    ']';

procedure TDtcCatalogTests.LoadsTopLevelDtcArray;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Assert.AreEqual(2, Cat.Count);
    Assert.IsTrue(Cat.FindByCode('P0301', Entry));
    Assert.AreEqual('Cyl 1 misfire', Entry.Description);
    Assert.AreEqual(Ord(dtcSeverityCritical), Ord(Entry.Severity));
    Assert.IsTrue(Entry.Verified);
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.LoadsBareJSONArray;
var
  Cat: TOBDDtcCatalog;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(BARE_ARRAY);
    Assert.AreEqual(2, Cat.Count);
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.LookupIsCaseInsensitive;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Assert.IsTrue(Cat.FindByCode('p0301', Entry));
    Assert.IsTrue(Cat.FindByCode('  P0301  ', Entry));
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.ReplacesDuplicateCode;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    // Re-load same JSON text: must NOT duplicate entries.
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Assert.AreEqual(2, Cat.Count);
    Assert.IsTrue(Cat.FindByCode('P0301', Entry));
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.CapturesPossibleCausesAndHints;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Cat.FindByCode('P0301', Entry);
    Assert.AreEqual(2, Length(Entry.PossibleCauses));
    Assert.AreEqual('bad coil', Entry.PossibleCauses[0]);
    Assert.AreEqual('swap to confirm', Entry.RepairHints);
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.DefaultSourcePropagatesToEntries;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Cat.FindByCode('P1296', Entry);
    Assert.AreEqual('test-source', Entry.Source);
  finally
    Cat.Free;
  end;
end;

procedure TDtcCatalogTests.VerifiedFlagDefaultsToFalse;
var
  Cat: TOBDDtcCatalog;
  Entry: TOBDDtcCatalogEntry;
begin
  Cat := TOBDDtcCatalog.Create;
  try
    Cat.LoadFromText(TOP_LEVEL_DTCS);
    Cat.FindByCode('P1296', Entry);
    Assert.IsFalse(Entry.Verified);
  finally
    Cat.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDtcEncodingTests);
  TDUnitX.RegisterTestFixture(TDtcCatalogTests);

end.
