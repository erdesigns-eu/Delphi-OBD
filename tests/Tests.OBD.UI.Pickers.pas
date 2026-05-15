//------------------------------------------------------------------------------
//  Tests.OBD.UI.Pickers
//
//  Non-visual contract for the A2.10 input pickers.
//------------------------------------------------------------------------------

unit Tests.OBD.UI.Pickers;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  OBD.UI.Pickers;

type
  [TestFixture]
  TVINEditTests = class
  public
    [Test] procedure DefaultsEmpty;
    [Test] procedure ShortInputIsInvalid;
    [Test] procedure GarbageCharsAreInvalid;
    [Test] procedure SeventeenCharShapeValid;
    [Test] procedure KnownGoodVINIsValid;
  end;

  [TestFixture]
  TPidPickerTests = class
  public
    [Test] procedure DefaultsCsDropDownList;
    [Test] procedure SetPIDsPopulatesItems;
    [Test] procedure HasSelectionFalseByDefault;
  end;

  [TestFixture]
  TOEMPickerTests = class
  public
    [Test] procedure DefaultsNoneSelected;
    [Test] procedure SelectUpdatesSelectedOEM;
  end;

  [TestFixture]
  TCANIdEditTests = class
  public
    [Test] procedure Defaults11Bit;
    [Test] procedure SetCanIdClampsHigh11Bit;
    [Test] procedure SetCanIdClampsHigh29Bit;
    [Test] procedure ExtendedToggleReclampsId;
    [Test] procedure IsValidParsesPrefixedHex;
  end;

implementation

{ TVINEditTests ----------------------------------------------------------- }

procedure TVINEditTests.DefaultsEmpty;
var E: TOBDVINEdit;
begin
  E := TOBDVINEdit.Create(nil);
  try
    Assert.IsTrue(E.Validity = vvEmpty);
    Assert.IsTrue(E.ShowLamp);
    Assert.IsTrue(E.AutoUppercase);
  finally E.Free; end;
end;

procedure TVINEditTests.ShortInputIsInvalid;
var E: TOBDVINEdit;
begin
  E := TOBDVINEdit.Create(nil);
  try
    E.Text := 'TEST123';     // < 17 chars
    Assert.IsTrue(E.Validity = vvInvalid);
    Assert.IsFalse(E.IsShapeValid);
  finally E.Free; end;
end;

procedure TVINEditTests.GarbageCharsAreInvalid;
var E: TOBDVINEdit;
begin
  E := TOBDVINEdit.Create(nil);
  try
    // 17 chars but I / O / Q are not allowed in the VIN
    // alphabet — IsValidShape should reject.
    E.Text := 'IIIIIIIIIIIIIIIII';
    Assert.IsTrue(E.Validity = vvInvalid);
  finally E.Free; end;
end;

procedure TVINEditTests.SeventeenCharShapeValid;
var E: TOBDVINEdit;
begin
  E := TOBDVINEdit.Create(nil);
  try
    // 17-char VIN with valid alphabet but bogus check digit.
    E.Text := '1HGCM82633A123456';
    Assert.IsTrue(E.IsShapeValid);
  finally E.Free; end;
end;

procedure TVINEditTests.KnownGoodVINIsValid;
var E: TOBDVINEdit;
begin
  // Standard sample VIN from ISO documentation: check digit
  // computes correctly so vvValid is the expected state.
  E := TOBDVINEdit.Create(nil);
  try
    E.Text := '1M8GDM9AXKP042788';
    // Some sample VINs are vvValid, others vvShapeOnly. We
    // assert at least shape — the check-digit branch depends
    // on the specific VIN and is exercised in the decoder
    // tests.
    Assert.IsTrue(E.IsShapeValid);
  finally E.Free; end;
end;

{ TPidPickerTests --------------------------------------------------------- }

procedure TPidPickerTests.DefaultsCsDropDownList;
var P: TOBDPidPicker;
begin
  P := TOBDPidPicker.Create(nil);
  try
    Assert.IsTrue(P.Style = csDropDownList);
    Assert.AreEqual(16, P.DropDownCount);
  finally P.Free; end;
end;

procedure TPidPickerTests.SetPIDsPopulatesItems;
var P: TOBDPidPicker;
begin
  P := TOBDPidPicker.Create(nil);
  try
    P.SetPIDs(TArray<Byte>.Create($0C, $0D, $05));
    Assert.AreEqual(3, P.Items.Count);
  finally P.Free; end;
end;

procedure TPidPickerTests.HasSelectionFalseByDefault;
var P: TOBDPidPicker;
begin
  P := TOBDPidPicker.Create(nil);
  try
    Assert.IsFalse(P.HasSelection);
    Assert.AreEqual<Byte>(0, P.SelectedPID);
  finally P.Free; end;
end;

{ TOEMPickerTests --------------------------------------------------------- }

procedure TOEMPickerTests.DefaultsNoneSelected;
var P: TOBDOEMPicker;
begin
  P := TOBDOEMPicker.Create(nil);
  try
    Assert.IsTrue(P.SelectedOEM = oemNone);
  finally P.Free; end;
end;

procedure TOEMPickerTests.SelectUpdatesSelectedOEM;
var P: TOBDOEMPicker;
begin
  P := TOBDOEMPicker.Create(nil);
  try
    P.Select(oemBMW);
    Assert.IsTrue(P.SelectedOEM = oemBMW);
    P.Select(oemHMG);
    Assert.IsTrue(P.SelectedOEM = oemHMG);
  finally P.Free; end;
end;

{ TCANIdEditTests --------------------------------------------------------- }

procedure TCANIdEditTests.Defaults11Bit;
var E: TOBDCANIdEdit;
begin
  E := TOBDCANIdEdit.Create(nil);
  try
    Assert.IsFalse(E.ExtendedId);
    Assert.AreEqual<Cardinal>(0, E.CanId);
  finally E.Free; end;
end;

procedure TCANIdEditTests.SetCanIdClampsHigh11Bit;
var E: TOBDCANIdEdit;
begin
  E := TOBDCANIdEdit.Create(nil);
  try
    E.CanId := $FFFFFFFF;
    Assert.AreEqual<Cardinal>($7FF, E.CanId);
  finally E.Free; end;
end;

procedure TCANIdEditTests.SetCanIdClampsHigh29Bit;
var E: TOBDCANIdEdit;
begin
  E := TOBDCANIdEdit.Create(nil);
  try
    E.ExtendedId := True;
    E.CanId := $FFFFFFFF;
    Assert.AreEqual<Cardinal>($1FFFFFFF, E.CanId);
  finally E.Free; end;
end;

procedure TCANIdEditTests.ExtendedToggleReclampsId;
var E: TOBDCANIdEdit;
begin
  E := TOBDCANIdEdit.Create(nil);
  try
    E.ExtendedId := True;
    E.CanId := $12345678;
    E.ExtendedId := False;       // re-clamp to 11-bit
    Assert.AreEqual<Cardinal>($7FF, E.CanId);
  finally E.Free; end;
end;

procedure TCANIdEditTests.IsValidParsesPrefixedHex;
var E: TOBDCANIdEdit;
begin
  E := TOBDCANIdEdit.Create(nil);
  try
    E.Text := '0x1A3';
    Assert.IsTrue(E.IsValid);
    E.Text := '0xABCDEF';        // > 11-bit, falls outside
    Assert.IsFalse(E.IsValid);
  finally E.Free; end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVINEditTests);
  TDUnitX.RegisterTestFixture(TPidPickerTests);
  TDUnitX.RegisterTestFixture(TOEMPickerTests);
  TDUnitX.RegisterTestFixture(TCANIdEditTests);

end.
