//------------------------------------------------------------------------------
//  Tests.OBD.Diagnostics.KWP
//
//  Lifecycle + safety-gate coverage for the KWP-diagnostic
//  components:
//    - TOBDKWP            (session hub)
//    - TOBDKWPReadID      (services 0x1A / 0x21 / 0x22)
//    - TOBDKWPReadDTC     (services 0x18 / 0x19)
//    - TOBDKWPIOControl   (services 0x2F / 0x30)
//    - TOBDKWPRoutine     (services 0x31 / 0x32 / 0x33)
//
//  Wire-level encode / decode is covered by the KWP protocol
//  fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Diagnostics.KWP;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.ClearDTC,
  OBD.Diagnostics.KWP,
  OBD.Diagnostics.KWP.ReadID,
  OBD.Diagnostics.KWP.ReadDTC,
  OBD.Diagnostics.KWP.IOControl,
  OBD.Diagnostics.KWP.Routine;

type
  /// <summary>
  ///   DUnitX fixture for the KWP-diagnostic components.
  /// </summary>
  [TestFixture]
  TPhase6KWPTests = class
  public
    [Test] procedure KWP_DefaultsCurrentSessionIsDefault;
    [Test] procedure KWP_StartSessionWithoutProtocolRaises;
    [Test] procedure KWP_TesterPresentWithoutProtocolRaises;

    [Test] procedure KWPReadID_ReadECUIDWithoutProtocolRaises;
    [Test] procedure KWPReadID_ReadByLocalIDWithoutProtocolRaises;
    [Test] procedure KWPReadID_ReadByCommonIDWithoutProtocolRaises;

    [Test] procedure KWPReadDTC_WithoutProtocolRaises;
    [Test] procedure KWPReadDTC_DecodeJ2012AllPrefixes;

    [Test] procedure KWPIOControl_DefaultsAutoExecuteFalse;
    [Test] procedure KWPIOControl_WithoutAutoExecuteRaises;
    [Test] procedure KWPIOControl_WithoutProtocolRaises;

    [Test] procedure KWPRoutine_DefaultsAutoExecuteFalse;
    [Test] procedure KWPRoutine_StartWithoutAutoExecuteRaises;
    [Test] procedure KWPRoutine_StopWithoutAutoExecuteRaises;
    [Test] procedure KWPRoutine_RequestResultsWithoutProtocolRaises;

    [Test] procedure ClearDTC_KWPDialectAcceptsKWPGroup;
  end;

implementation

{ ---- TOBDKWP ------------------------------------------------------------- }

procedure TPhase6KWPTests.KWP_DefaultsCurrentSessionIsDefault;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.AreEqual(Integer(KWP_SESSION_DEFAULT),
      Integer(H.CurrentSession));
    Assert.IsFalse(H.KeepAlive);
  finally
    H.Free;
  end;
end;

procedure TPhase6KWPTests.KWP_StartSessionWithoutProtocolRaises;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.StartSession(KWP_SESSION_STANDARD);
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TPhase6KWPTests.KWP_TesterPresentWithoutProtocolRaises;
var
  H: TOBDKWP;
begin
  H := TOBDKWP.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.TesterPresent;
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

{ ---- TOBDKWPReadID ------------------------------------------------------ }

procedure TPhase6KWPTests.KWPReadID_ReadECUIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadECUID($9A);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPReadID_ReadByLocalIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByLocalID($01);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPReadID_ReadByCommonIDWithoutProtocolRaises;
var
  R: TOBDKWPReadID;
begin
  R := TOBDKWPReadID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByCommonID($F190);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDKWPReadDTC ----------------------------------------------------- }

procedure TPhase6KWPTests.KWPReadDTC_WithoutProtocolRaises;
var
  R: TOBDKWPReadDTC;
begin
  R := TOBDKWPReadDTC.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByStatus($FF, KWP_DTC_GROUP_ALL);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPReadDTC_DecodeJ2012AllPrefixes;
begin
  Assert.AreEqual('P0143', TOBDKWPReadDTC.DecodeJ2012($01, $43));
  Assert.AreEqual('C0220', TOBDKWPReadDTC.DecodeJ2012($42, $20));
  Assert.AreEqual('B0300', TOBDKWPReadDTC.DecodeJ2012($83, $00));
  Assert.AreEqual('U0000', TOBDKWPReadDTC.DecodeJ2012($C0, $00));
end;

{ ---- TOBDKWPIOControl --------------------------------------------------- }

procedure TPhase6KWPTests.KWPIOControl_DefaultsAutoExecuteFalse;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    Assert.IsFalse(C.AutoExecute);
  finally
    C.Free;
  end;
end;

procedure TPhase6KWPTests.KWPIOControl_WithoutAutoExecuteRaises;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.SendLocal($05, KWP_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TPhase6KWPTests.KWPIOControl_WithoutProtocolRaises;
var
  C: TOBDKWPIOControl;
begin
  C := TOBDKWPIOControl.Create(nil);
  try
    C.AutoExecute := True;
    Assert.WillRaise(
      procedure
      begin
        C.SendCommon($F123, KWP_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

{ ---- TOBDKWPRoutine ----------------------------------------------------- }

procedure TPhase6KWPTests.KWPRoutine_DefaultsAutoExecuteFalse;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.IsFalse(R.AutoExecute);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPRoutine_StartWithoutAutoExecuteRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Start($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPRoutine_StopWithoutAutoExecuteRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Stop($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6KWPTests.KWPRoutine_RequestResultsWithoutProtocolRaises;
var
  R: TOBDKWPRoutine;
begin
  R := TOBDKWPRoutine.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.RequestResults($05);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDClearDTC cdKWP dialect ----------------------------------------- }

procedure TPhase6KWPTests.ClearDTC_KWPDialectAcceptsKWPGroup;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    C.Dialect := cdKWP;
    C.UDSGroup := KWP_DTC_GROUP_ALL;
    Assert.AreEqual(Ord(cdKWP), Ord(C.Dialect));
    Assert.AreEqual(Integer($FFFF), Integer(C.UDSGroup));
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPhase6KWPTests);

end.
