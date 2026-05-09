//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.MOST
//
//  MOST control-message frame coverage.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4f initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.MOST;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.MOST.Control;

type
  /// <summary>MOST control message coverage.</summary>
  [TestFixture]
  TMOSTTests = class
  public
    [Test] procedure ControlMessageRoundTrip;
    [Test] procedure DecodeRejectsTooShortBuffer;
    [Test] procedure EncodeRejectsOversizedFktID;
    [Test] procedure EncodeRejectsOversizedDataMOST25;
    [Test] procedure MOST50AcceptsLongerData;
  end;

implementation

procedure TMOSTTests.ControlMessageRoundTrip;
var
  Msg, Decoded: TOBDMOSTControlMessage;
  Bytes: TBytes;
begin
  Msg := Default(TOBDMOSTControlMessage);
  Msg.SourceAddress := $0102;
  Msg.DestinationAddress := $0304;
  Msg.FBlockID := MOST_FBLOCK_AudioAmplifier;
  Msg.InstID := $01;
  Msg.FktID := $321;
  Msg.OPType := MOST_OP_Set;
  Msg.TelID := 0;
  Msg.TelLen := 0;
  Msg.Data := TBytes.Create($AA, $BB, $CC);

  Bytes := MOSTEncodeControl(Msg, msMOST25);
  Assert.AreEqual(9 + 3, Length(Bytes));
  Assert.IsTrue(MOSTDecodeControl(Bytes, Decoded));
  Assert.AreEqual($0102, Integer(Decoded.SourceAddress));
  Assert.AreEqual($0304, Integer(Decoded.DestinationAddress));
  Assert.AreEqual(Integer(MOST_FBLOCK_AudioAmplifier),
                  Integer(Decoded.FBlockID));
  Assert.AreEqual($321, Integer(Decoded.FktID));
  Assert.AreEqual(Integer(MOST_OP_Set), Integer(Decoded.OPType));
  Assert.AreEqual(3, Length(Decoded.Data));
  Assert.AreEqual($CC, Integer(Decoded.Data[2]));
end;

procedure TMOSTTests.DecodeRejectsTooShortBuffer;
var
  Out_: TOBDMOSTControlMessage;
  Buf: TBytes;
begin
  SetLength(Buf, 5);
  Assert.IsFalse(MOSTDecodeControl(Buf, Out_));
end;

procedure TMOSTTests.EncodeRejectsOversizedFktID;
var
  Msg: TOBDMOSTControlMessage;
begin
  Msg := Default(TOBDMOSTControlMessage);
  Msg.FktID := $1000;
  Assert.WillRaise(
    procedure begin MOSTEncodeControl(Msg, msMOST25); end, EOBDConfig);
end;

procedure TMOSTTests.EncodeRejectsOversizedDataMOST25;
var
  Msg: TOBDMOSTControlMessage;
begin
  Msg := Default(TOBDMOSTControlMessage);
  SetLength(Msg.Data, MOST25_CONTROL_DATA_MAX + 1);
  Assert.WillRaise(
    procedure begin MOSTEncodeControl(Msg, msMOST25); end, EOBDConfig);
end;

procedure TMOSTTests.MOST50AcceptsLongerData;
var
  Msg: TOBDMOSTControlMessage;
  Bytes: TBytes;
begin
  Msg := Default(TOBDMOSTControlMessage);
  Msg.FBlockID := MOST_FBLOCK_NetBlock;
  SetLength(Msg.Data, 30);
  Bytes := MOSTEncodeControl(Msg, msMOST50);
  Assert.AreEqual(9 + 30, Length(Bytes));
end;

initialization
  TDUnitX.RegisterTestFixture(TMOSTTests);

end.
