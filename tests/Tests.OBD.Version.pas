//------------------------------------------------------------------------------
//  Tests.OBD.Version
//
//  Smoke test for the OBD.Version unit.
//
//  Verifies the version constants are populated and consistent. Exists
//  primarily to give CI something to run early on before real
//  components land.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial placeholder.
//------------------------------------------------------------------------------

unit Tests.OBD.Version;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Smoke tests for the package version constants.
  /// </summary>
  [TestFixture]
  TVersionTests = class
  public
    /// <summary>The full version string is non-empty.</summary>
    [Test]
    procedure VersionStringIsPopulated;

    /// <summary>The major version is 2.</summary>
    [Test]
    procedure MajorVersionIsTwo;

    /// <summary>The copyright line names the project.</summary>
    [Test]
    procedure CopyrightContainsProjectName;
  end;

implementation

uses
  System.SysUtils,
  OBD.Version;

procedure TVersionTests.VersionStringIsPopulated;
begin
  Assert.IsNotEmpty(OBD_VERSION);
end;

procedure TVersionTests.MajorVersionIsTwo;
begin
  Assert.AreEqual(2, OBD_VERSION_MAJOR);
end;

procedure TVersionTests.CopyrightContainsProjectName;
begin
  Assert.Contains(OBD_COPYRIGHT, 'Delphi-OBD');
end;

initialization
  TDUnitX.RegisterTestFixture(TVersionTests);

end.
