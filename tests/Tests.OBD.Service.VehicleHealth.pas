//------------------------------------------------------------------------------
//  Tests.OBD.Service.VehicleHealth
//
//  DUnitX coverage for TOBDVehicleHealthReport's IsHealthy
//  decision rule and for TOBDVehicleHealth's snapshot fan-out
//  to unassigned children (graceful skip).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.VehicleHealth;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Service.DTCs,
  OBD.Service.VehicleHealth;

type
  [TestFixture]
  TVehicleHealthReportTests = class
  public
    [Test] procedure IsHealthyTrueWhenMILOffAndNoConfirmedDTCs;
    [Test] procedure IsHealthyFalseWhenMILOn;
    [Test] procedure IsHealthyFalseWhenConfirmedDTCsPresent;
    [Test] procedure IsHealthyTrueIgnoresPendingDTCs;
    [Test] procedure IsHealthyTrueWhenMILUnknown;
  end;

  [TestFixture]
  TVehicleHealthGracefulTests = class
  public
    [Test] procedure SnapshotPopulatesErrorWhenNothingAssigned;
  end;

implementation

function MakeDTCEntry(const ACode: string): TOBDDtcEntry;
begin
  Result := Default(TOBDDtcEntry);
  Result.Code := ACode;
end;

{ ---- TVehicleHealthReportTests --------------------------------------------- }

procedure TVehicleHealthReportTests.IsHealthyTrueWhenMILOffAndNoConfirmedDTCs;
var
  R: TOBDVehicleHealthReport;
begin
  R := Default(TOBDVehicleHealthReport);
  R.MILValid := True;
  R.MIL.MILOn := False;
  Assert.IsTrue(R.IsHealthy);
end;

procedure TVehicleHealthReportTests.IsHealthyFalseWhenMILOn;
var
  R: TOBDVehicleHealthReport;
begin
  R := Default(TOBDVehicleHealthReport);
  R.MILValid := True;
  R.MIL.MILOn := True;
  Assert.IsFalse(R.IsHealthy);
end;

procedure TVehicleHealthReportTests.IsHealthyFalseWhenConfirmedDTCsPresent;
var
  R: TOBDVehicleHealthReport;
begin
  R := Default(TOBDVehicleHealthReport);
  R.MILValid := True;
  R.MIL.MILOn := False;
  R.ConfirmedDTCs := TArray<TOBDDtcEntry>.Create(MakeDTCEntry('P0420'));
  Assert.IsFalse(R.IsHealthy);
end;

procedure TVehicleHealthReportTests.IsHealthyTrueIgnoresPendingDTCs;
var
  R: TOBDVehicleHealthReport;
begin
  R := Default(TOBDVehicleHealthReport);
  R.MILValid := True;
  R.MIL.MILOn := False;
  R.PendingDTCs := TArray<TOBDDtcEntry>.Create(MakeDTCEntry('P0301'));
  Assert.IsTrue(R.IsHealthy,
    'Pending DTCs are informational; they should not flip IsHealthy');
end;

procedure TVehicleHealthReportTests.IsHealthyTrueWhenMILUnknown;
var
  R: TOBDVehicleHealthReport;
begin
  R := Default(TOBDVehicleHealthReport);
  // MILValid stays False (default) — read failed; IsHealthy should
  // still report True if no confirmed DTCs.
  Assert.IsFalse(R.MILValid);
  Assert.IsTrue(R.IsHealthy);
end;

{ ---- TVehicleHealthGracefulTests ------------------------------------------- }

procedure TVehicleHealthGracefulTests.SnapshotPopulatesErrorWhenNothingAssigned;
var
  Health: TOBDVehicleHealth;
  R: TOBDVehicleHealthReport;
begin
  Health := TOBDVehicleHealth.Create(nil);
  try
    R := Health.Snapshot;
    Assert.IsNotEmpty(R.VINError, 'VINError must be set when VIN child is nil');
    Assert.IsNotEmpty(R.MILError,
      'MILError must be set when Protocol is nil');
    Assert.IsNotEmpty(R.ConfirmedError,
      'ConfirmedError must be set when DTCs child is nil');
    Assert.AreEqual(0, Length(R.ConfirmedDTCs));
  finally
    Health.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVehicleHealthReportTests);
  TDUnitX.RegisterTestFixture(TVehicleHealthGracefulTests);

end.
