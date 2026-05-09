//------------------------------------------------------------------------------
// UNIT           : Tests.Protocol.WWHOBD.Readiness.pas
// CONTENTS       : Tests for OBD.Protocol.WWHOBD.Readiness
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.Protocol.WWHOBD.Readiness;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TWWHOBDReadinessTests = class
  public
    /// <summary>
    ///   Decode rejects too short.
    /// </summary>
    [Test] procedure DecodeRejectsTooShort;
    /// <summary>
    ///   M i l bit decodes.
    /// </summary>
    [Test] procedure MILBitDecodes;
    /// <summary>
    ///   D t c count from lower seven bits.
    /// </summary>
    [Test] procedure DTCCountFromLowerSevenBits;
    /// <summary>
    ///   Continuous misfire supported not complete.
    /// </summary>
    [Test] procedure ContinuousMisfireSupportedNotComplete;
    /// <summary>
    ///   Non continuous catalyst complete.
    /// </summary>
    [Test] procedure NonContinuousCatalystComplete;
    /// <summary>
    ///   Round trip four byte form.
    /// </summary>
    [Test] procedure RoundTripFourByteForm;
    /// <summary>
    ///   Round trip six byte form with diesel monitors.
    /// </summary>
    [Test] procedure RoundTripSixByteFormWithDieselMonitors;
    /// <summary>
    ///   All ready true when everything complete.
    /// </summary>
    [Test] procedure AllReadyTrueWhenEverythingComplete;
    /// <summary>
    ///   All ready true when unsupported.
    /// </summary>
    [Test] procedure AllReadyTrueWhenUnsupported;
    /// <summary>
    ///   Pending monitors lists incomplete.
    /// </summary>
    [Test] procedure PendingMonitorsListsIncomplete;
  end;

implementation

uses
  System.SysUtils, OBD.Protocol.WWHOBD.Readiness;

//------------------------------------------------------------------------------
// DECODE REJECTS TOO SHORT
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.DecodeRejectsTooShort;
begin
  Assert.WillRaise(
    procedure begin DecodeWWHOBDReadiness(TBytes.Create($00, $00)); end,
    EOBDWWHOBDReadiness);
end;

//------------------------------------------------------------------------------
// MILBIT DECODES
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.MILBitDecodes;
var
  R: TWWHOBDReadinessSet;
begin
  R := DecodeWWHOBDReadiness(TBytes.Create($85, $00, $00, $00));
  Assert.IsTrue(R.MILActive);
  Assert.AreEqual(Integer(5), Integer(R.DTCCount));
end;

//------------------------------------------------------------------------------
// DTCCOUNT FROM LOWER SEVEN BITS
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.DTCCountFromLowerSevenBits;
var
  R: TWWHOBDReadinessSet;
begin
  R := DecodeWWHOBDReadiness(TBytes.Create($0A, $00, $00, $00));
  Assert.IsFalse(R.MILActive);
  Assert.AreEqual(Integer(10), Integer(R.DTCCount));
end;

//------------------------------------------------------------------------------
// CONTINUOUS MISFIRE SUPPORTED NOT COMPLETE
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.ContinuousMisfireSupportedNotComplete;
var
  R: TWWHOBDReadinessSet;
begin
  // bit 0 set in low nibble (Misfire supported), bit 4 set in high
  // nibble (Misfire NotComplete).
  R := DecodeWWHOBDReadiness(TBytes.Create($00, $11, $00, $00));
  Assert.IsTrue(R.Misfire.Supported);
  Assert.IsFalse(R.Misfire.Complete);
end;

//------------------------------------------------------------------------------
// NON CONTINUOUS CATALYST COMPLETE
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.NonContinuousCatalystComplete;
var
  R: TWWHOBDReadinessSet;
begin
  // Catalyst supported (byte 2 bit 0), Catalyst Complete (byte 3 bit 0 NOT set)
  R := DecodeWWHOBDReadiness(TBytes.Create($00, $00, $01, $00));
  Assert.IsTrue(R.Catalyst.Supported);
  Assert.IsTrue(R.Catalyst.Complete);
end;

//------------------------------------------------------------------------------
// ROUND TRIP FOUR BYTE FORM
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.RoundTripFourByteForm;
var
  In_, Out_: TWWHOBDReadinessSet;
  Bytes: TBytes;
begin
  In_ := Default(TWWHOBDReadinessSet);
  In_.MILActive := True;
  In_.DTCCount := 7;
  In_.Misfire.Supported := True;          In_.Misfire.Complete := False;
  In_.FuelSystem.Supported := True;       In_.FuelSystem.Complete := True;
  In_.Comprehensive.Supported := True;    In_.Comprehensive.Complete := True;
  In_.Catalyst.Supported := True;         In_.Catalyst.Complete := False;
  In_.OxygenSensor.Supported := True;     In_.OxygenSensor.Complete := True;
  In_.EvaporativeSystem.Supported := True;In_.EvaporativeSystem.Complete := False;
  Bytes := EncodeWWHOBDReadiness(In_);
  Assert.AreEqual(4, Length(Bytes));
  Out_ := DecodeWWHOBDReadiness(Bytes);
  Assert.IsTrue(Out_.MILActive);
  Assert.AreEqual(Integer(7), Integer(Out_.DTCCount));
  Assert.IsTrue(Out_.Misfire.Supported);
  Assert.IsFalse(Out_.Misfire.Complete);
  Assert.IsTrue(Out_.FuelSystem.Complete);
  Assert.IsTrue(Out_.Catalyst.Supported);
  Assert.IsFalse(Out_.Catalyst.Complete);
  Assert.IsTrue(Out_.EvaporativeSystem.Supported);
  Assert.IsFalse(Out_.EvaporativeSystem.Complete);
end;

//------------------------------------------------------------------------------
// ROUND TRIP SIX BYTE FORM WITH DIESEL MONITORS
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.RoundTripSixByteFormWithDieselMonitors;
var
  In_, Out_: TWWHOBDReadinessSet;
  Bytes: TBytes;
begin
  In_ := Default(TWWHOBDReadinessSet);
  In_.PMFilter.Supported := True;
  In_.PMFilter.Complete := True;
  In_.NOxAftertreatment.Supported := True;
  In_.NOxAftertreatment.Complete := False;
  Bytes := EncodeWWHOBDReadiness(In_);
  Assert.AreEqual(6, Length(Bytes), 'Should extend to 6 bytes for diesel set');
  Out_ := DecodeWWHOBDReadiness(Bytes);
  Assert.IsTrue(Out_.PMFilter.Supported);
  Assert.IsTrue(Out_.PMFilter.Complete);
  Assert.IsTrue(Out_.NOxAftertreatment.Supported);
  Assert.IsFalse(Out_.NOxAftertreatment.Complete);
end;

//------------------------------------------------------------------------------
// ALL READY TRUE WHEN EVERYTHING COMPLETE
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.AllReadyTrueWhenEverythingComplete;
var
  R: TWWHOBDReadinessSet;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Misfire.Supported := True; R.Misfire.Complete := True;
  R.Catalyst.Supported := True; R.Catalyst.Complete := True;
  Assert.IsTrue(R.AllReady);
end;

//------------------------------------------------------------------------------
// ALL READY TRUE WHEN UNSUPPORTED
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.AllReadyTrueWhenUnsupported;
var
  R: TWWHOBDReadinessSet;
begin
  R := Default(TWWHOBDReadinessSet);
  // No monitors supported -> AllReady is trivially true.
  Assert.IsTrue(R.AllReady);
end;

//------------------------------------------------------------------------------
// PENDING MONITORS LISTS INCOMPLETE
//------------------------------------------------------------------------------
procedure TWWHOBDReadinessTests.PendingMonitorsListsIncomplete;
var
  R: TWWHOBDReadinessSet;
  Pending: TArray<string>;
  S: string;
  HasCatalyst, HasMisfire: Boolean;
begin
  R := Default(TWWHOBDReadinessSet);
  R.Misfire.Supported := True;        R.Misfire.Complete := True;
  R.Catalyst.Supported := True;       R.Catalyst.Complete := False;
  R.OxygenSensor.Supported := True;   R.OxygenSensor.Complete := False;
  Pending := R.PendingMonitors;
  Assert.AreEqual(2, Length(Pending));
  HasCatalyst := False; HasMisfire := False;
  for S in Pending do
  begin
    if S = 'Catalyst' then HasCatalyst := True;
    if S = 'Misfire' then HasMisfire := True;
  end;
  Assert.IsTrue(HasCatalyst);
  Assert.IsFalse(HasMisfire, 'Complete monitors must not appear');
end;

initialization
  TDUnitX.RegisterTestFixture(TWWHOBDReadinessTests);

end.
