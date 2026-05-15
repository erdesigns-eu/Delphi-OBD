//------------------------------------------------------------------------------
//  Tests.OBD.Adapter
//
//  Lifecycle and configuration tests for TOBDAdapter that don't need
//  real hardware. Real-I/O coverage lives in the hardware-loop CI job.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Adapter;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Coverage for <c>TOBDAdapter</c>'s public surface that
  /// does not require an opened transport.</summary>
  [TestFixture]
  TAdapterTests = class
  public
    /// <summary>Defaults: family is afELM327, timeout 5000.</summary>
    [Test] procedure DefaultsAreSane;
    /// <summary>InitCommands TStrings is allocated.</summary>
    [Test] procedure InitCommandsAllocated;
    /// <summary>Detect raises EOBDNotConnected without a connection.</summary>
    [Test] procedure DetectWithoutConnectionRaises;
    /// <summary>Init raises EOBDNotConnected without a connection.</summary>
    [Test] procedure InitWithoutConnectionRaises;
    /// <summary>WriteATCommand raises EOBDNotConnected without a
    /// connection.</summary>
    [Test] procedure WriteATCommandWithoutConnectionRaises;
    /// <summary>WriteSTCommand raises EOBDUnsupported when the
    /// adapter has not declared acSTCommands.</summary>
    [Test] procedure WriteSTWithoutCapabilityRaises;
    /// <summary>Connection FreeNotification clears the binding when
    /// the connection is freed.</summary>
    [Test] procedure ConnectionFreeNotificationCleared;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection,
  OBD.Adapter.Types,
  OBD.Adapter;

procedure TAdapterTests.DefaultsAreSane;
var
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  try
    Assert.AreEqual(Ord(afELM327), Ord(A.Family));
    Assert.AreEqual<Cardinal>(5000, A.CommandTimeoutMs);
    Assert.IsNotNull(A.InitCommands);
  finally
    A.Free;
  end;
end;

procedure TAdapterTests.InitCommandsAllocated;
var
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  try
    A.InitCommands.Add('ATAL');
    Assert.AreEqual(1, A.InitCommands.Count);
  finally
    A.Free;
  end;
end;

procedure TAdapterTests.DetectWithoutConnectionRaises;
var
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        A.Detect;
      end,
      EOBDNotConnected);
  finally
    A.Free;
  end;
end;

procedure TAdapterTests.InitWithoutConnectionRaises;
var
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        A.Init;
      end,
      EOBDNotConnected);
  finally
    A.Free;
  end;
end;

procedure TAdapterTests.WriteATCommandWithoutConnectionRaises;
var
  A: TOBDAdapter;
begin
  A := TOBDAdapter.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        A.WriteATCommand('ATZ');
      end,
      EOBDNotConnected);
  finally
    A.Free;
  end;
end;

procedure TAdapterTests.WriteSTWithoutCapabilityRaises;
var
  A: TOBDAdapter;
  C: TOBDConnection;
begin
  C := TOBDConnection.Create(nil);
  A := TOBDAdapter.Create(nil);
  try
    A.Connection := C;
    // Capabilities default empty (no Detect run).
    Assert.WillRaise(
      procedure
      begin
        A.WriteSTCommand('STI');
      end,
      EOBDUnsupported);
  finally
    A.Free;
    C.Free;
  end;
end;

procedure TAdapterTests.ConnectionFreeNotificationCleared;
var
  A: TOBDAdapter;
  C: TOBDConnection;
begin
  A := TOBDAdapter.Create(nil);
  C := TOBDConnection.Create(nil);
  try
    A.Connection := C;
    Assert.IsNotNull(A.Connection);
    C.Free;
    C := nil;
    Assert.IsNull(A.Connection,
      'FreeNotification should have cleared Connection');
  finally
    A.Free;
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterTests);

end.
