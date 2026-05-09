//------------------------------------------------------------------------------
// UNIT           : Tests.Service06.Mode06
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Service06.Mode06;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMode06Tests = class
  public
    [Test] procedure RequestIsTwoBytes;
    [Test] procedure ParseSingleRecordResponse;
    [Test] procedure ParseMultiRecordResponse;
    [Test] procedure ParseRejectsBadServiceId;
    [Test] procedure ParseRejectsRaggedPayload;
    [Test] procedure ParseRejectsTooShort;
    [Test] procedure RecordPassedTestWhenWithinLimits;
    [Test] procedure RecordFailedTestWhenAboveMax;
    [Test] procedure ScaleFactorReturnsUnitScale;
    [Test] procedure FindUCSIDReturnsUnknownDefault;
    [Test] procedure FindOBDMIDReturnsCatalystName;
    [Test] procedure FindTestIDReturnsCatalystName;
  end;

implementation

uses
  System.SysUtils, OBD.Service06.Mode06;

procedure TMode06Tests.RequestIsTwoBytes;
var Req: TBytes;
begin
  Req := BuildMode06Request($21);
  Assert.AreEqual(2, Length(Req));
  Assert.AreEqual($46, Integer(Req[0]));
  Assert.AreEqual($21, Integer(Req[1]));
end;

procedure TMode06Tests.ParseSingleRecordResponse;
var
  Resp: TOBDMode06Response;
  Bytes: TBytes;
begin
  // 46 21 [TID=81 UCSID=24 Val=0064 Min=0050 Max=0078]
  Bytes := TBytes.Create($46, $21, $81, $24, $00, $64, $00, $50, $00, $78);
  Resp := ParseMode06Response(Bytes);
  Assert.AreEqual($21, Integer(Resp.OBDMID));
  Assert.AreEqual(1, Length(Resp.Records));
  Assert.AreEqual($81, Integer(Resp.Records[0].TestId));
  Assert.AreEqual($24, Integer(Resp.Records[0].UnitsAndScalingId));
  Assert.AreEqual(Word($0064), Resp.Records[0].TestValue);
  Assert.AreEqual(Word($0050), Resp.Records[0].MinLimit);
  Assert.AreEqual(Word($0078), Resp.Records[0].MaxLimit);
end;

procedure TMode06Tests.ParseMultiRecordResponse;
var
  Resp: TOBDMode06Response;
  Bytes: TBytes;
begin
  // 46 31 + 2 records of 8 bytes each
  Bytes := TBytes.Create(
    $46, $31,
    $A1, $24, $00, $50, $00, $30, $00, $80,
    $A2, $24, $00, $40, $00, $20, $00, $90);
  Resp := ParseMode06Response(Bytes);
  Assert.AreEqual(2, Length(Resp.Records));
  Assert.AreEqual($A1, Integer(Resp.Records[0].TestId));
  Assert.AreEqual($A2, Integer(Resp.Records[1].TestId));
end;

procedure TMode06Tests.ParseRejectsBadServiceId;
var Bytes: TBytes;
begin
  Bytes := TBytes.Create($00, $21);
  Assert.WillRaise(
    procedure begin ParseMode06Response(Bytes); end, EOBDMode06);
end;

procedure TMode06Tests.ParseRejectsRaggedPayload;
var Bytes: TBytes;
begin
  // 46 21 + 5 bytes (not multiple of 8)
  Bytes := TBytes.Create($46, $21, $81, $24, $00, $64, $00);
  Assert.WillRaise(
    procedure begin ParseMode06Response(Bytes); end, EOBDMode06);
end;

procedure TMode06Tests.ParseRejectsTooShort;
begin
  Assert.WillRaise(
    procedure begin ParseMode06Response(TBytes.Create($46)); end, EOBDMode06);
end;

procedure TMode06Tests.RecordPassedTestWhenWithinLimits;
var R: TOBDMode06TestRecord;
begin
  R.TestValue := 100;
  R.MinLimit := 80;
  R.MaxLimit := 120;
  Assert.IsTrue(R.PassedTest);
end;

procedure TMode06Tests.RecordFailedTestWhenAboveMax;
var R: TOBDMode06TestRecord;
begin
  R.TestValue := 200;
  R.MinLimit := 80;
  R.MaxLimit := 120;
  Assert.IsFalse(R.PassedTest);
end;

procedure TMode06Tests.ScaleFactorReturnsUnitScale;
var R: TOBDMode06TestRecord;
begin
  R.UnitsAndScalingId := $24; // °C, scale 1.0
  Assert.AreEqual(Single(1.0), R.ScaleFactor, 0.0001);
  Assert.AreEqual('°C', R.UnitName);
end;

procedure TMode06Tests.FindUCSIDReturnsUnknownDefault;
var Info: TOBDMode06UnitInfo;
begin
  Info := FindMode06Unit($FF);
  Assert.AreEqual(Single(1.0), Info.Scale, 0.0001);
  Assert.IsTrue(Info.Description.Contains('Unknown'));
end;

procedure TMode06Tests.FindOBDMIDReturnsCatalystName;
begin
  Assert.AreEqual('Catalyst Monitor Bank 1', FindMode06OBDMIDName($21));
  Assert.AreEqual('Catalyst Monitor Bank 2', FindMode06OBDMIDName($22));
end;

procedure TMode06Tests.FindTestIDReturnsCatalystName;
begin
  Assert.IsTrue(FindMode06TestIdName($81).Contains('Catalyst'));
  Assert.IsTrue(FindMode06TestIdName($A1).Contains('EGR'));
end;

initialization
  TDUnitX.RegisterTestFixture(TMode06Tests);

end.
