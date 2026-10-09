//------------------------------------------------------------------------------
//  Tests.ERD.Flashing.OEMHandshakes
//
//  Coverage for the per-OEM bootloader handshakes: safety surface +
//  vendor-default sub-function / security-level / RID values.
//  Wire-side coverage is gated on hardware (the standing
//  deferral); the configuration surface is what hosts depend on.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.Flashing.OEMHandshakes;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  DUnitX.TestFramework,
  ERD.Types,
  ERD.Flash.OEM.Common,
  ERD.Flash.OEM.VAG,
  ERD.Flash.OEM.BMW,
  ERD.Flash.OEM.Ford,
  ERD.Flash.OEM.HMG,
  ERD.Flash.OEM.Mercedes,
  ERD.Flash.OEM.Stellantis,
  ERD.Flash.OEM.Toyota;

type
  [TestFixture]
  TOEMHandshakeTests = class
  public
    [Test] procedure RunRaisesWhenAutoExecuteFalse;
    [Test] procedure RunRaisesWithoutProtocol;
    [Test] procedure VAGDefaults;
    [Test] procedure BMWDefaults;
    [Test] procedure FordDefaults;
    [Test] procedure HMGDefaults;
    [Test] procedure MercedesDefaults;
    [Test] procedure StellantisDefaults;
    [Test] procedure ToyotaDefaults;
    [Test] procedure VendorNamesPresent;
  end;

implementation

procedure TOEMHandshakeTests.RunRaisesWhenAutoExecuteFalse;
var
  H: TOBDFlashHandshakeVAG;
begin
  H := TOBDFlashHandshakeVAG.Create(nil);
  try
    Assert.IsFalse(H.AutoExecute);
    Assert.WillRaise(procedure begin H.Run; end, EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.RunRaisesWithoutProtocol;
var
  H: TOBDFlashHandshakeVAG;
begin
  H := TOBDFlashHandshakeVAG.Create(nil);
  try
    H.AutoExecute := True;
    Assert.WillRaise(procedure begin H.Run; end, EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.VAGDefaults;
var
  H: TOBDFlashHandshakeVAG;
begin
  H := TOBDFlashHandshakeVAG.Create(nil);
  try
    Assert.AreEqual($11,    Integer(H.SecurityLevel));
    Assert.AreEqual($FF00,  Integer(H.EraseRoutineID));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.BMWDefaults;
var
  H: TOBDFlashHandshakeBMW;
begin
  H := TOBDFlashHandshakeBMW.Create(nil);
  try
    Assert.IsTrue(H.ExtendedFirst);
    Assert.AreEqual($09, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.FordDefaults;
var
  H: TOBDFlashHandshakeFord;
begin
  H := TOBDFlashHandshakeFord.Create(nil);
  try
    Assert.AreEqual($01, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.HMGDefaults;
var
  H: TOBDFlashHandshakeHMG;
begin
  H := TOBDFlashHandshakeHMG.Create(nil);
  try
    Assert.AreEqual($01, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.MercedesDefaults;
var
  H: TOBDFlashHandshakeMercedes;
begin
  H := TOBDFlashHandshakeMercedes.Create(nil);
  try
    Assert.AreEqual($01, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.StellantisDefaults;
var
  H: TOBDFlashHandshakeStellantis;
begin
  H := TOBDFlashHandshakeStellantis.Create(nil);
  try
    Assert.AreEqual($01, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.ToyotaDefaults;
var
  H: TOBDFlashHandshakeToyota;
begin
  H := TOBDFlashHandshakeToyota.Create(nil);
  try
    Assert.AreEqual($01, Integer(H.SecurityLevel));
  finally
    H.Free;
  end;
end;

procedure TOEMHandshakeTests.VendorNamesPresent;
var
  V: TOBDFlashHandshakeVAG;
  B: TOBDFlashHandshakeBMW;
begin
  V := TOBDFlashHandshakeVAG.Create(nil);
  B := TOBDFlashHandshakeBMW.Create(nil);
  try
    Assert.IsTrue(V.VendorName.Contains('VAG'));
    Assert.IsTrue(B.VendorName.Contains('BMW'));
  finally
    B.Free;
    V.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOEMHandshakeTests);

end.
