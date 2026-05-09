//------------------------------------------------------------------------------
// UNIT           : Tests.OBD.Helpers
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for v3.23 OBD-II application
//                  helpers — ReadinessMonitor + FreezeFrame.
//                  TOBDHealthCapture is connection-coupled; the
//                  computation pieces (ComputeHealthScore /
//                  ComposeSummary) are private and exercised
//                  end-to-end by the reference desktop tool.
//------------------------------------------------------------------------------
unit Tests.OBD.Helpers;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TReadinessMonitorTests = class
  public
    [Test] procedure DecodesAllZerosCleanly;
    [Test] procedure DecodesMILOnAndDtcCount;
    [Test] procedure DecodesContinuousMonitorReady;
    [Test] procedure DecodesContinuousMonitorNotReady;
    [Test] procedure DecodesGasolineNonContinuousReady;
    [Test] procedure DecodesDieselFlagAndMonitors;
    [Test] procedure RejectsTooShort;
    [Test] procedure SummaryFormatsCorrectly;
    [Test] procedure MonitorKindNamesAreCanonical;
    [Test] procedure MonitorStateNamesAreCanonical;
  end;

  [TestFixture]
  TFreezeFrameTests = class
  public
    [Test] procedure BuildRequestEncodesPidAndFrame;
    [Test] procedure ParsePositiveResponse;
    [Test] procedure ParseRejectsTooShort;
    [Test] procedure ParseRejectsWrongSID;
    [Test] procedure ParseRejectsWrongPID;
    [Test] procedure ParseHandlesNegativeNRC;
    [Test] procedure FormatTriggerDtcRoundTrips;
  end;

implementation

uses
  System.SysUtils,
  OBD.ReadinessMonitor, OBD.FreezeFrame;

//==============================================================================
// Readiness monitor
//==============================================================================

//------------------------------------------------------------------------------
// DECODES ALL ZEROS CLEANLY
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesAllZerosCleanly;
var
  Report: TOBDReadinessReport;
begin
  Report := DecodeReadinessReport(TBytes.Create($00, $00, $00, $00));
  Assert.IsFalse(Report.MILOn);
  Assert.AreEqual(Byte(0), Report.DtcCount);
  Assert.IsFalse(Report.IsDiesel);
  // 11 monitors total for spark-ignition (3 continuous + 8 non-cont).
  Assert.AreEqual(11, Length(Report.Monitors));
end;

//------------------------------------------------------------------------------
// DECODES MILON AND DTC COUNT
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesMILOnAndDtcCount;
var
  Report: TOBDReadinessReport;
begin
  // Byte A: MIL on (bit 7) + 5 DTCs.
  Report := DecodeReadinessReport(TBytes.Create($85, $00, $00, $00));
  Assert.IsTrue(Report.MILOn);
  Assert.AreEqual(Byte(5), Report.DtcCount);
end;

//------------------------------------------------------------------------------
// DECODES CONTINUOUS MONITOR READY
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesContinuousMonitorReady;
var
  Report: TOBDReadinessReport;
  M: TOBDMonitorStatus;
begin
  // Byte B: misfire supported (bit 0) + complete (bit 4 NOT set).
  Report := DecodeReadinessReport(TBytes.Create($00, $01, $00, $00));
  for M in Report.Monitors do
    if M.Kind = monMisfire then
    begin
      Assert.AreEqual(Ord(msReady), Ord(M.State));
      Exit;
    end;
  Assert.Fail('misfire monitor missing from report');
end;

//------------------------------------------------------------------------------
// DECODES CONTINUOUS MONITOR NOT READY
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesContinuousMonitorNotReady;
var
  Report: TOBDReadinessReport;
  M: TOBDMonitorStatus;
begin
  // Byte B: misfire supported (bit 0) + NOT-yet-complete (bit 4 SET).
  Report := DecodeReadinessReport(TBytes.Create($00, $11, $00, $00));
  for M in Report.Monitors do
    if M.Kind = monMisfire then
    begin
      Assert.AreEqual(Ord(msNotReady), Ord(M.State));
      Exit;
    end;
end;

//------------------------------------------------------------------------------
// DECODES GASOLINE NON CONTINUOUS READY
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesGasolineNonContinuousReady;
var
  Report: TOBDReadinessReport;
  M: TOBDMonitorStatus;
  HasCatalyst: Boolean;
begin
  // Byte C bit 0 = catalyst supported. Byte D bit 0 = NOT complete.
  // We want catalyst supported + complete → C=01, D=00.
  Report := DecodeReadinessReport(TBytes.Create($00, $00, $01, $00));
  HasCatalyst := False;
  for M in Report.Monitors do
    if M.Kind = monCatalyst then
    begin
      HasCatalyst := True;
      Assert.AreEqual(Ord(msReady), Ord(M.State));
    end;
  Assert.IsTrue(HasCatalyst, 'catalyst monitor should appear for SI engines');
end;

//------------------------------------------------------------------------------
// DECODES DIESEL FLAG AND MONITORS
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.DecodesDieselFlagAndMonitors;
var
  Report: TOBDReadinessReport;
  M: TOBDMonitorStatus;
  HasNOxAftertreatment, HasCatalyst: Boolean;
begin
  // Byte B bit 3 = compression ignition. Byte C bit 1 = NOx
  // aftertreatment supported.
  Report := DecodeReadinessReport(TBytes.Create($00, $08, $02, $00));
  Assert.IsTrue(Report.IsDiesel);
  HasNOxAftertreatment := False;
  HasCatalyst := False;
  for M in Report.Monitors do
  begin
    if M.Kind = monNOxAftertreatment then HasNOxAftertreatment := True;
    if M.Kind = monCatalyst then HasCatalyst := True;
  end;
  Assert.IsTrue(HasNOxAftertreatment,
    'diesel report must include NOx aftertreatment monitor');
  Assert.IsFalse(HasCatalyst,
    'diesel report must NOT include the SI catalyst monitor');
end;

//------------------------------------------------------------------------------
// REJECTS TOO SHORT
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.RejectsTooShort;
begin
  Assert.WillRaise(
    procedure begin DecodeReadinessReport(TBytes.Create($00, $00, $00)); end,
    EOBDReadinessError);
end;

//------------------------------------------------------------------------------
// SUMMARY FORMATS CORRECTLY
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.SummaryFormatsCorrectly;
var
  Report: TOBDReadinessReport;
  Summary: string;
begin
  // MIL on, 3 DTCs, gasoline, no monitors supported.
  Report := DecodeReadinessReport(TBytes.Create($83, $00, $00, $00));
  Summary := FormatReadinessSummary(Report);
  Assert.IsTrue(Pos('MIL on', Summary) > 0);
  Assert.IsTrue(Pos('3 DTCs', Summary) > 0);
  Assert.IsTrue(Pos('spark-ignition', Summary) > 0);
end;

//------------------------------------------------------------------------------
// MONITOR KIND NAMES ARE CANONICAL
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.MonitorKindNamesAreCanonical;
begin
  Assert.AreEqual('misfire',           MonitorKindName(monMisfire));
  Assert.AreEqual('fuel_system',       MonitorKindName(monFuelSystem));
  Assert.AreEqual('nox_aftertreatment',MonitorKindName(monNOxAftertreatment));
  Assert.AreEqual('pm_filter',         MonitorKindName(monPMFilter));
end;

//------------------------------------------------------------------------------
// MONITOR STATE NAMES ARE CANONICAL
//------------------------------------------------------------------------------
procedure TReadinessMonitorTests.MonitorStateNamesAreCanonical;
begin
  Assert.AreEqual('not_supported', MonitorStateName(msNotSupported));
  Assert.AreEqual('not_ready',     MonitorStateName(msNotReady));
  Assert.AreEqual('ready',         MonitorStateName(msReady));
end;

//==============================================================================
// Freeze frame
//==============================================================================

//------------------------------------------------------------------------------
// BUILD REQUEST ENCODES PID AND FRAME
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.BuildRequestEncodesPidAndFrame;
var
  Req: TBytes;
begin
  Req := BuildFreezeFrameRequest($0C, 0);
  Assert.AreEqual(3, Length(Req));
  Assert.AreEqual(Byte($02), Req[0]);
  Assert.AreEqual(Byte($0C), Req[1]);
  Assert.AreEqual(Byte($00), Req[2]);
end;

//------------------------------------------------------------------------------
// PARSE POSITIVE RESPONSE
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.ParsePositiveResponse;
var
  Entry: TOBDFreezeFrameEntry;
begin
  Entry := ParseFreezeFrameResponse(
    TBytes.Create($42, $0C, $00, $1A, $F8), $0C);
  Assert.AreEqual(Byte($0C), Entry.PID);
  Assert.AreEqual(Byte($00), Entry.FrameNumber);
  Assert.AreEqual(2, Length(Entry.Payload));
  Assert.AreEqual(Byte($1A), Entry.Payload[0]);
  Assert.AreEqual(Byte($F8), Entry.Payload[1]);
end;

//------------------------------------------------------------------------------
// PARSE REJECTS TOO SHORT
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.ParseRejectsTooShort;
begin
  Assert.WillRaise(
    procedure begin
      ParseFreezeFrameResponse(TBytes.Create($42), $0C);
    end,
    EOBDFreezeFrameError);
end;

//------------------------------------------------------------------------------
// PARSE REJECTS WRONG SID
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.ParseRejectsWrongSID;
begin
  Assert.WillRaise(
    procedure begin
      ParseFreezeFrameResponse(TBytes.Create($41, $0C, $00, $00), $0C);
    end,
    EOBDFreezeFrameError);
end;

//------------------------------------------------------------------------------
// PARSE REJECTS WRONG PID
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.ParseRejectsWrongPID;
begin
  Assert.WillRaise(
    procedure begin
      ParseFreezeFrameResponse(TBytes.Create($42, $05, $00, $00), $0C);
    end,
    EOBDFreezeFrameError);
end;

//------------------------------------------------------------------------------
// PARSE HANDLES NEGATIVE NRC
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.ParseHandlesNegativeNRC;
begin
  Assert.WillRaise(
    procedure begin
      ParseFreezeFrameResponse(TBytes.Create($7F, $02, $11), $0C);
    end,
    EOBDFreezeFrameError);
end;

//------------------------------------------------------------------------------
// FORMAT TRIGGER DTC ROUND TRIPS
//------------------------------------------------------------------------------
procedure TFreezeFrameTests.FormatTriggerDtcRoundTrips;
begin
  // 0x03 0x01 → P0301 (cylinder 1 misfire).
  Assert.AreEqual('P0301',
    FormatFreezeFrameTriggerDTC(TBytes.Create($03, $01)));
  Assert.AreEqual('',
    FormatFreezeFrameTriggerDTC(TBytes.Create($03)));
end;

initialization
  TDUnitX.RegisterTestFixture(TReadinessMonitorTests);
  TDUnitX.RegisterTestFixture(TFreezeFrameTests);

end.
