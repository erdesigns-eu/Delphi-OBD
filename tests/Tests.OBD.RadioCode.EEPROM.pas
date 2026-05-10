//------------------------------------------------------------------------------
//  Tests.OBD.RadioCode.EEPROM
//
//  Pinned-vector tests for the EEPROM-dump extractor family
//  (P-A1.X). Each vendor test builds a synthetic dump in memory,
//  writes it to a temp file, points the component at it, and
//  asserts the parsed code matches the documented offset.
//------------------------------------------------------------------------------

unit Tests.OBD.RadioCode.EEPROM;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  DUnitX.TestFramework,
  OBD.RadioCode.EEPROM;

type
  [TestFixture]
  TEEPROMExtractorTests = class
  strict private
    FTempFile: string;
    function WriteDump(const ABytes: TBytes): string;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure VolvoHU_ReadsFourASCIIDigitsAt0x90;
    [Test] procedure VolvoHU_RejectsNonDigitBytes;
    [Test] procedure VolvoHU_RejectsShortDump;

    [Test] procedure OpelCD30_DecodesBCDAt0x2B7;
    [Test] procedure OpelCD30_RejectsInvalidBCDNibble;

    [Test] procedure MercedesBecker_ReadsFiveASCIIDigitsAt0x76;

    [Test] procedure OnExtract_OverridesBundledRule;
  end;

implementation

function TEEPROMExtractorTests.WriteDump(const ABytes: TBytes): string;
begin
  Result := TPath.Combine(TPath.GetTempPath,
    'obd-eeprom-test-' + TGUID.NewGuid.ToString + '.bin');
  TFile.WriteAllBytes(Result, ABytes);
  FTempFile := Result;
end;

procedure TEEPROMExtractorTests.Setup;
begin
  FTempFile := '';
end;

procedure TEEPROMExtractorTests.TearDown;
begin
  if (FTempFile <> '') and TFile.Exists(FTempFile) then
    TFile.Delete(FTempFile);
end;

procedure TEEPROMExtractorTests.VolvoHU_ReadsFourASCIIDigitsAt0x90;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_VolvoHU;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, 128);
  Dump[$90] := Ord('1');
  Dump[$91] := Ord('2');
  Dump[$92] := Ord('3');
  Dump[$93] := Ord('4');
  C := TOBDRadioCodeEEPROM_VolvoHU.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1234', R.Code);
    Assert.AreEqual('volvo-hu', R.Vendor);
    Assert.AreEqual('31 32 33 34', R.RawHex);
  finally
    C.Free;
  end;
end;

procedure TEEPROMExtractorTests.VolvoHU_RejectsNonDigitBytes;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_VolvoHU;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, 128);
  Dump[$90] := $FF;  // not an ASCII digit
  C := TOBDRadioCodeEEPROM_VolvoHU.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.RawHex <> '');
  finally
    C.Free;
  end;
end;

procedure TEEPROMExtractorTests.VolvoHU_RejectsShortDump;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_VolvoHU;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, 16);  // too short to reach 0x90
  C := TOBDRadioCodeEEPROM_VolvoHU.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsFalse(R.Success);
    Assert.IsTrue(R.Message.Contains('too short'));
  finally
    C.Free;
  end;
end;

procedure TEEPROMExtractorTests.OpelCD30_DecodesBCDAt0x2B7;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_OpelCD30;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, $400);
  Dump[$2B7] := $12;  // -> "12"
  Dump[$2B8] := $34;  // -> "34"
  C := TOBDRadioCodeEEPROM_OpelCD30.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('1234', R.Code);
    Assert.AreEqual('opel-cd30', R.Vendor);
  finally
    C.Free;
  end;
end;

procedure TEEPROMExtractorTests.OpelCD30_RejectsInvalidBCDNibble;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_OpelCD30;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, $400);
  Dump[$2B7] := $1A;  // 'A' is not a valid BCD nibble
  Dump[$2B8] := $34;
  C := TOBDRadioCodeEEPROM_OpelCD30.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsFalse(R.Success);
  finally
    C.Free;
  end;
end;

procedure TEEPROMExtractorTests.MercedesBecker_ReadsFiveASCIIDigitsAt0x76;
var
  Dump: TBytes;
  C: TOBDRadioCodeEEPROM_MercedesBecker;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, 256);
  Dump[$76] := Ord('1');
  Dump[$77] := Ord('2');
  Dump[$78] := Ord('3');
  Dump[$79] := Ord('4');
  Dump[$7A] := Ord('5');
  C := TOBDRadioCodeEEPROM_MercedesBecker.Create(nil);
  try
    C.DumpFile := WriteDump(Dump);
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('12345', R.Code);
    Assert.AreEqual('mercedes-becker', R.Vendor);
  finally
    C.Free;
  end;
end;

type
  THostHook = class
  public
    procedure DoExtract(Sender: TObject; const ADump: TBytes;
      var AResult: TOBDRadioCodeEEPROMResult);
  end;

procedure THostHook.DoExtract(Sender: TObject; const ADump: TBytes;
  var AResult: TOBDRadioCodeEEPROMResult);
begin
  AResult.Success := True;
  AResult.Code    := 'OVERRIDDEN';
end;

procedure TEEPROMExtractorTests.OnExtract_OverridesBundledRule;
var
  Dump: TBytes;
  // Use the abstract base via one concrete subclass; the goal is
  // to prove OnExtract takes precedence over the bundled rule.
  // The Volvo extractor's bundled rule would *fail* here (no
  // ASCII digits at 0x90) — so a successful result can only come
  // from the host hook.
  C: TOBDRadioCodeEEPROM_VolvoHU;
  Hook: THostHook;
  R: TOBDRadioCodeEEPROMResult;
begin
  SetLength(Dump, 256);  // dump full of $00 — bundled rule would reject
  Hook := THostHook.Create;
  C := TOBDRadioCodeEEPROM_VolvoHU.Create(nil);
  try
    C.DumpFile  := WriteDump(Dump);
    C.OnExtract := Hook.DoExtract;
    R := C.Extract;
    Assert.IsTrue(R.Success, R.Message);
    Assert.AreEqual('OVERRIDDEN', R.Code);
    Assert.AreEqual('volvo-hu', R.Vendor);
  finally
    C.Free;
    Hook.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TEEPROMExtractorTests);

end.
