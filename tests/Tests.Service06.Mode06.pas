//------------------------------------------------------------------------------
// UNIT           : Tests.Service06.Mode06.pas
// CONTENTS       : Tests for OBD.Service06.Mode06
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
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
    /// <summary>
    ///   Request is two bytes.
    /// </summary>
    [Test] procedure RequestIsTwoBytes;
    /// <summary>
    ///   Parse single record response.
    /// </summary>
    [Test] procedure ParseSingleRecordResponse;
    /// <summary>
    ///   Parse multi record response.
    /// </summary>
    [Test] procedure ParseMultiRecordResponse;
    /// <summary>
    ///   Parse rejects bad service id.
    /// </summary>
    [Test] procedure ParseRejectsBadServiceId;
    /// <summary>
    ///   Parse rejects ragged payload.
    /// </summary>
    [Test] procedure ParseRejectsRaggedPayload;
    /// <summary>
    ///   Parse rejects too short.
    /// </summary>
    [Test] procedure ParseRejectsTooShort;
    /// <summary>
    ///   Record passed test when within limits.
    /// </summary>
    [Test] procedure RecordPassedTestWhenWithinLimits;
    /// <summary>
    ///   Record failed test when above max.
    /// </summary>
    [Test] procedure RecordFailedTestWhenAboveMax;
    /// <summary>
    ///   Scale factor returns unit scale.
    /// </summary>
    [Test] procedure ScaleFactorReturnsUnitScale;
    /// <summary>
    ///   Find u c s i d returns unknown default.
    /// </summary>
    [Test] procedure FindUCSIDReturnsUnknownDefault;
    /// <summary>
    ///   Find o b d m i d returns catalyst name.
    /// </summary>
    [Test] procedure FindOBDMIDReturnsCatalystName;
    /// <summary>
    ///   Find test i d returns catalyst name.
    /// </summary>
    [Test] procedure FindTestIDReturnsCatalystName;
  end;

implementation

uses
  System.SysUtils, OBD.Service06.Mode06;

//------------------------------------------------------------------------------
// REQUEST IS TWO BYTES
//------------------------------------------------------------------------------
procedure TMode06Tests.RequestIsTwoBytes;
var
  Req: TBytes;
begin
  Req := BuildMode06Request($21);
  Assert.AreEqual(2, Length(Req));
  Assert.AreEqual($46, Integer(Req[0]));
  Assert.AreEqual($21, Integer(Req[1]));
end;

//------------------------------------------------------------------------------
// PARSE SINGLE RECORD RESPONSE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// PARSE MULTI RECORD RESPONSE
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// PARSE REJECTS BAD SERVICE ID
//------------------------------------------------------------------------------
procedure TMode06Tests.ParseRejectsBadServiceId;
var
  Bytes: TBytes;
begin
  Bytes := TBytes.Create($00, $21);
  Assert.WillRaise(
    procedure begin ParseMode06Response(Bytes); end, EOBDMode06);
end;

//------------------------------------------------------------------------------
// PARSE REJECTS RAGGED PAYLOAD
//------------------------------------------------------------------------------
procedure TMode06Tests.ParseRejectsRaggedPayload;
var
  Bytes: TBytes;
begin
  // 46 21 + 5 bytes (not multiple of 8)
  Bytes := TBytes.Create($46, $21, $81, $24, $00, $64, $00);
  Assert.WillRaise(
    procedure begin ParseMode06Response(Bytes); end, EOBDMode06);
end;

//------------------------------------------------------------------------------
// PARSE REJECTS TOO SHORT
//------------------------------------------------------------------------------
procedure TMode06Tests.ParseRejectsTooShort;
begin
  Assert.WillRaise(
    procedure begin ParseMode06Response(TBytes.Create($46)); end, EOBDMode06);
end;

//------------------------------------------------------------------------------
// RECORD PASSED TEST WHEN WITHIN LIMITS
//------------------------------------------------------------------------------
procedure TMode06Tests.RecordPassedTestWhenWithinLimits;
var
  R: TOBDMode06TestRecord;
begin
  R.TestValue := 100;
  R.MinLimit := 80;
  R.MaxLimit := 120;
  Assert.IsTrue(R.PassedTest);
end;

//------------------------------------------------------------------------------
// RECORD FAILED TEST WHEN ABOVE MAX
//------------------------------------------------------------------------------
procedure TMode06Tests.RecordFailedTestWhenAboveMax;
var
  R: TOBDMode06TestRecord;
begin
  R.TestValue := 200;
  R.MinLimit := 80;
  R.MaxLimit := 120;
  Assert.IsFalse(R.PassedTest);
end;

//------------------------------------------------------------------------------
// SCALE FACTOR RETURNS UNIT SCALE
//------------------------------------------------------------------------------
procedure TMode06Tests.ScaleFactorReturnsUnitScale;
var
  R: TOBDMode06TestRecord;
begin
  R.UnitsAndScalingId := $24; // °C, scale 1.0
  Assert.AreEqual(Single(1.0), R.ScaleFactor, 0.0001);
  Assert.AreEqual('°C', R.UnitName);
end;

//------------------------------------------------------------------------------
// FIND UCSIDRETURNS UNKNOWN DEFAULT
//------------------------------------------------------------------------------
procedure TMode06Tests.FindUCSIDReturnsUnknownDefault;
var
  Info: TOBDMode06UnitInfo;
begin
  Info := FindMode06Unit($FF);
  Assert.AreEqual(Single(1.0), Info.Scale, 0.0001);
  Assert.IsTrue(Info.Description.Contains('Unknown'));
end;

//------------------------------------------------------------------------------
// FIND OBDMIDRETURNS CATALYST NAME
//------------------------------------------------------------------------------
procedure TMode06Tests.FindOBDMIDReturnsCatalystName;
begin
  Assert.AreEqual('Catalyst Monitor Bank 1', FindMode06OBDMIDName($21));
  Assert.AreEqual('Catalyst Monitor Bank 2', FindMode06OBDMIDName($22));
end;

//------------------------------------------------------------------------------
// FIND TEST IDRETURNS CATALYST NAME
//------------------------------------------------------------------------------
procedure TMode06Tests.FindTestIDReturnsCatalystName;
begin
  Assert.IsTrue(FindMode06TestIdName($81).Contains('Catalyst'));
  Assert.IsTrue(FindMode06TestIdName($A1).Contains('EGR'));
end;

initialization
  TDUnitX.RegisterTestFixture(TMode06Tests);

end.
