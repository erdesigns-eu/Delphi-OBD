//------------------------------------------------------------------------------
// UNIT           : Tests.Adapter.ELM327
// CONTENTS       : Tests for ELM327-related parsers and formatters that don't
//                  require a live serial/BT connection.
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The ELM327 wire-protocol behaviour itself (responses,
//                  prompts) is exercised through the protocol layer in
//                  Tests.Protocol.IsoTp. This unit covers the static helpers:
//                  FormatATCommand and the chip-type descriptor.
//------------------------------------------------------------------------------
unit Tests.Adapter.ELM327;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFormatATCommandTests = class
  public
    [Test]
    procedure NoParam_Command_FormatsLiterally;

    [Test]
    procedure SingleStringParam_FormatsExpected;

    [Test]
    procedure SetHeader_AT_SH_FormatsHexHeader;

    [Test]
    procedure SetProtocol_AT_SP_FormatsProtocolDigit;

    [Test]
    procedure ParamCountMismatch_TooFew_RaisesException;

    [Test]
    procedure ParamCountMismatch_TooMany_RaisesException;
  end;

  [TestFixture]
  TElm327ChipTypeTests = class
  public
    [Test]
    [TestCase('Genuine',     '0,Genuine')]
    [TestCase('CloneV12',    '1,Chinese Clone')]
    [TestCase('CloneV13',    '2,Chinese Clone')]
    [TestCase('CloneV14',    '3,Chinese Clone')]
    [TestCase('CloneV15',    '4,Chinese Clone')]
    [TestCase('CloneV21Fake','5,FAKE')]
    [TestCase('CloneV22Fake','6,FAKE')]
    [TestCase('CloneV23Fake','7,FAKE')]
    [TestCase('STN1xxx',     '8,OBDLink')]
    procedure Description_ContainsExpectedSubstring(
      const ChipOrdinal: Integer; const ExpectedSubstring: string);

    [Test]
    procedure Description_NeverEmpty_ForKnownTypes;
  end;

implementation

uses
  System.SysUtils,
  OBD.Adapter.ATCommands,
  OBD.Adapter.ELM327.Detection;

{ TFormatATCommandTests }

procedure TFormatATCommandTests.NoParam_Command_FormatsLiterally;
begin
  Assert.AreEqual('E0', FormatATCommand(ECHO_OFF, []));
  Assert.AreEqual('D',  FormatATCommand(SET_ALL_TO_DEFAULTS, []));
  Assert.AreEqual('I',  FormatATCommand(PRINT_ID, []));
  Assert.AreEqual('Z',  FormatATCommand(RESET_ALL, []));
end;

procedure TFormatATCommandTests.SingleStringParam_FormatsExpected;
var
  Cmd: string;
begin
  // STORE_DEVICE_IDENTIFIER → "@3 %s"
  Cmd := FormatATCommand(STORE_DEVICE_IDENTIFIER, ['MYTAG']);
  Assert.AreEqual('@3 MYTAG', Cmd);
end;

procedure TFormatATCommandTests.SetHeader_AT_SH_FormatsHexHeader;
var
  Cmd: string;
begin
  // SET_HEADER → "SH %s"
  Cmd := FormatATCommand(SET_HEADER, ['7E0']);
  Assert.AreEqual('SH 7E0', Cmd);
end;

procedure TFormatATCommandTests.SetProtocol_AT_SP_FormatsProtocolDigit;
var
  Cmd: string;
begin
  // SET_PROTOCOL → "SP %s" (protocol 6 = ISO 15765-4 CAN 11/500)
  Cmd := FormatATCommand(SET_PROTOCOL, ['6']);
  Assert.AreEqual('SP 6', Cmd);
end;

procedure TFormatATCommandTests.ParamCountMismatch_TooFew_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FormatATCommand(SET_HEADER, []);
    end,
    TATCommandException);
end;

procedure TFormatATCommandTests.ParamCountMismatch_TooMany_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      FormatATCommand(ECHO_OFF, ['unexpected']);
    end,
    TATCommandException);
end;

{ TElm327ChipTypeTests }

procedure TElm327ChipTypeTests.Description_ContainsExpectedSubstring(
  const ChipOrdinal: Integer; const ExpectedSubstring: string);
var
  Description: string;
begin
  Description := TELM327Detector.GetChipTypeDescription(
    TELM327ChipType(ChipOrdinal));
  Assert.IsTrue(Description.Contains(ExpectedSubstring),
    Format('Description "%s" did not contain "%s"',
      [Description, ExpectedSubstring]));
end;

procedure TElm327ChipTypeTests.Description_NeverEmpty_ForKnownTypes;
var
  ChipType: TELM327ChipType;
begin
  for ChipType := Low(TELM327ChipType) to High(TELM327ChipType) do
    Assert.IsNotEmpty(TELM327Detector.GetChipTypeDescription(ChipType),
      'Empty description for chip ordinal ' + IntToStr(Ord(ChipType)));
end;

initialization
  TDUnitX.RegisterTestFixture(TFormatATCommandTests);
  TDUnitX.RegisterTestFixture(TElm327ChipTypeTests);

end.
