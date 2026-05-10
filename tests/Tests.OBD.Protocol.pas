//------------------------------------------------------------------------------
//  Tests.OBD.Protocol
//
//  TOBDProtocol component lifecycle and configuration tests. Real-I/O
//  coverage lives in the hardware-loop CI job.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Lifecycle and guard coverage for TOBDProtocol.</summary>
  [TestFixture]
  TProtocolComponentTests = class
  public
    /// <summary>Defaults: Mode=pmAuto, Manual=pidAuto,
    /// Application=apOBD2, DefaultTimeoutMs=5000.</summary>
    [Test] procedure DefaultsAreSane;
    /// <summary>Send raises EOBDNotConnected without an adapter.</summary>
    [Test] procedure SendWithoutAdapterRaises;
    /// <summary>Request raises EOBDNotConnected without an adapter.</summary>
    [Test] procedure RequestWithoutAdapterRaises;
    /// <summary>FreeNotification clears Adapter when the bound
    /// adapter is freed.</summary>
    [Test] procedure FreeNotificationClearsAdapter;
    /// <summary>Free of a protocol with no in-flight async is
    /// clean.</summary>
    [Test] procedure FreeIsClean;
    /// <summary>MakeRequest helper builds the right shape.</summary>
    [Test] procedure MakeRequestShape;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Adapter,
  OBD.Protocol.Types,
  OBD.Protocol;

procedure TProtocolComponentTests.DefaultsAreSane;
var
  P: TOBDProtocol;
begin
  P := TOBDProtocol.Create(nil);
  try
    Assert.AreEqual(Ord(pmAuto), Ord(P.Mode));
    Assert.AreEqual(Ord(pidAuto), Ord(P.Manual));
    Assert.AreEqual(Ord(apOBD2), Ord(P.Application));
    Assert.AreEqual<Cardinal>(5000, P.DefaultTimeoutMs);
  finally
    P.Free;
  end;
end;

procedure TProtocolComponentTests.SendWithoutAdapterRaises;
var
  P: TOBDProtocol;
  Req: TOBDRequest;
begin
  P := TOBDProtocol.Create(nil);
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $09;
    Req.Data := TBytes.Create($02);
    Assert.WillRaise(
      procedure
      begin
        P.Send(Req);
      end,
      EOBDNotConnected);
  finally
    P.Free;
  end;
end;

procedure TProtocolComponentTests.RequestWithoutAdapterRaises;
var
  P: TOBDProtocol;
begin
  P := TOBDProtocol.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        P.Request($09, TBytes.Create($02));
      end,
      EOBDNotConnected);
  finally
    P.Free;
  end;
end;

procedure TProtocolComponentTests.FreeNotificationClearsAdapter;
var
  P: TOBDProtocol;
  A: TOBDAdapter;
begin
  P := TOBDProtocol.Create(nil);
  A := TOBDAdapter.Create(nil);
  try
    P.Adapter := A;
    Assert.IsNotNull(P.Adapter);
    A.Free;
    A := nil;
    Assert.IsNull(P.Adapter);
  finally
    P.Free;
    A.Free;
  end;
end;

procedure TProtocolComponentTests.FreeIsClean;
var
  P: TOBDProtocol;
begin
  P := TOBDProtocol.Create(nil);
  P.Free;
  Assert.Pass;
end;

procedure TProtocolComponentTests.MakeRequestShape;
var
  R: TOBDRequest;
begin
  R := MakeRequest(apUDS, $22, TBytes.Create($F1, $90), 1234);
  Assert.AreEqual(Ord(apUDS), Ord(R.Protocol));
  Assert.AreEqual<Byte>($22, R.ServiceID);
  Assert.AreEqual<NativeInt>(2, Length(R.Data));
  Assert.AreEqual<Byte>($F1, R.Data[0]);
  Assert.AreEqual<Cardinal>(1234, R.TimeoutMs);
end;

initialization
  TDUnitX.RegisterTestFixture(TProtocolComponentTests);

end.
