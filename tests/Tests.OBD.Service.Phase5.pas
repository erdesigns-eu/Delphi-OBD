//------------------------------------------------------------------------------
//  Tests.OBD.Service.Phase5
//
//  Coverage for the Phase 5 close-out components:
//    - TOBDClearDTC          (Mode 0x04 / UDS 0x14)
//    - TOBDOxygenMonitor     (Mode 0x05)
//    - TOBDPIDList           (design-time collection)
//    - TOBDDataSource        (bridge)
//    - TOBDWWHOBD            (WWH-OBD diagnostic surface)
//    - TOBDWWHReadiness      (WWH-OBD readiness surface)
//
//  Lifecycle + safety-gate + classification coverage. Codec / protocol
//  round trips for these components are covered by the Phase 4 / Phase 5
//  protocol fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.Phase5;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.ClearDTC,
  OBD.OxygenMonitor,
  OBD.PIDList,
  OBD.DataSource,
  OBD.WWHOBD,
  OBD.WWHOBD.Readiness;

type
  /// <summary>
  ///   DUnitX fixture for the Phase 5 close-out components.
  /// </summary>
  /// <remarks>
  ///   Tests are grouped by component (ClearDTC → OxygenMonitor →
  ///   PIDList → DataSource → WWHOBD → WWHReadiness). Each test
  ///   constructs its component on the stack, exercises one
  ///   behaviour, and frees the component in a <c>try…finally</c>.
  /// </remarks>
  [TestFixture]
  TPhase5Tests = class
  public
    [Test] procedure ClearDTC_DefaultsAutoExecuteFalse;
    [Test] procedure ClearDTC_ClearWithoutProtocolRaises;
    [Test] procedure ClearDTC_ClearWithoutAutoExecuteRaises;
    [Test] procedure ClearDTC_DialectAndGroupDefaults;

    [Test] procedure OxygenMonitor_ReadWithoutProtocolRaises;

    [Test] procedure PIDList_AddAndFind;
    [Test] procedure PIDList_EnabledForFiltersDisabledAndOtherMode;
    [Test] procedure PIDList_DisplayName;

    [Test] procedure DataSource_KindForUnknownIsNone;
    [Test] procedure DataSource_KindForAnonymousComponentIsNone;
    [Test] procedure DataSource_NotifyForwardsToHandler;
    [Test] procedure DataSource_ActiveToggleFiresStateChange;

    [Test] procedure WWHOBD_ClassifySeverityZeroIsUnknown;
    [Test] procedure WWHOBD_ClassifySeverityImmediateIsCheckNow;
    [Test] procedure WWHOBD_ClassifySeverityBothIsCheckBoth;
    [Test] procedure WWHOBD_ClassifySeverityHaltIsCheckHalt;
    [Test] procedure WWHOBD_ClassifySeverityMaintenanceOnly;
    [Test] procedure WWHOBD_ReadBySeverityWithoutProtocolRaises;

    [Test] procedure WWHReadiness_ReadWithoutProtocolRaises;
  end;

implementation

{ ---- ClearDTC ------------------------------------------------------------- }

procedure TPhase5Tests.ClearDTC_DefaultsAutoExecuteFalse;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    Assert.IsFalse(C.AutoExecute);
    Assert.AreEqual(Ord(cdOBDII), Ord(C.Dialect));
    Assert.AreEqual($FFFFFF, Integer(C.UDSGroup));
  finally
    C.Free;
  end;
end;

procedure TPhase5Tests.ClearDTC_ClearWithoutProtocolRaises;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    C.AutoExecute := True;
    Assert.WillRaise(
      procedure
      begin
        C.Clear;
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TPhase5Tests.ClearDTC_ClearWithoutAutoExecuteRaises;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.Clear;
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TPhase5Tests.ClearDTC_DialectAndGroupDefaults;
var
  C: TOBDClearDTC;
begin
  C := TOBDClearDTC.Create(nil);
  try
    C.Dialect := cdUDS;
    C.UDSGroup := $123456;
    Assert.AreEqual(Ord(cdUDS), Ord(C.Dialect));
    Assert.AreEqual($123456, Integer(C.UDSGroup));
  finally
    C.Free;
  end;
end;

{ ---- OxygenMonitor -------------------------------------------------------- }

procedure TPhase5Tests.OxygenMonitor_ReadWithoutProtocolRaises;
var
  C: TOBDOxygenMonitor;
begin
  C := TOBDOxygenMonitor.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.Read($01, $01);
      end,
      EOBDConfig);
    Assert.WillRaise(
      procedure
      begin
        C.ReadSupportedTIDs;
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

{ ---- PIDList -------------------------------------------------------------- }

procedure TPhase5Tests.PIDList_AddAndFind;
var
  L: TOBDPIDList;
  It: TOBDPIDItem;
begin
  L := TOBDPIDList.Create(nil);
  try
    L.Add($01, $0C, 'Engine RPM');
    L.Add($01, $0D, 'Vehicle Speed');
    Assert.AreEqual(2, L.Count);
    It := L.Find($01, $0D);
    Assert.IsNotNull(It);
    Assert.AreEqual('Vehicle Speed', It.Name);
    Assert.IsNull(L.Find($01, $FF));
  finally
    L.Free;
  end;
end;

procedure TPhase5Tests.PIDList_EnabledForFiltersDisabledAndOtherMode;
var
  L: TOBDPIDList;
  Bytes: TArray<Byte>;
begin
  L := TOBDPIDList.Create(nil);
  try
    L.Add($01, $0C);
    L.Add($01, $0D);
    L.Add($01, $11, '', False);           // disabled
    L.Add($22, $F1, '');                  // wrong mode
    Bytes := L.EnabledFor($01);
    Assert.AreEqual(2, Length(Bytes));
    Assert.AreEqual($0C, Integer(Bytes[0]));
    Assert.AreEqual($0D, Integer(Bytes[1]));
  finally
    L.Free;
  end;
end;

procedure TPhase5Tests.PIDList_DisplayName;
var
  L: TOBDPIDList;
  It: TOBDPIDItem;
begin
  L := TOBDPIDList.Create(nil);
  try
    It := L.Add($01, $0C, 'Engine RPM');
    Assert.Contains(It.DisplayName, 'Engine RPM');
    Assert.Contains(It.DisplayName, '0c');
  finally
    L.Free;
  end;
end;

{ ---- DataSource ----------------------------------------------------------- }

procedure TPhase5Tests.DataSource_KindForUnknownIsNone;
var
  DS: TOBDDataSource;
begin
  DS := TOBDDataSource.Create(nil);
  try
    Assert.AreEqual(Ord(dsNone), Ord(DS.Kind));
  finally
    DS.Free;
  end;
end;

procedure TPhase5Tests.DataSource_KindForAnonymousComponentIsNone;
var
  DS: TOBDDataSource;
  Comp: TComponent;
begin
  DS := TOBDDataSource.Create(nil);
  Comp := TComponent.Create(nil);
  try
    DS.Source := Comp;
    Assert.AreEqual(Ord(dsNone), Ord(DS.Kind));
  finally
    DS.Free;
    Comp.Free;
  end;
end;

procedure TPhase5Tests.DataSource_NotifyForwardsToHandler;
var
  DS: TOBDDataSource;
  Hit: Integer;
  P: TOBDDataSourcePayload;
begin
  DS := TOBDDataSource.Create(nil);
  try
    Hit := 0;
    DS.OnDataChange :=
      procedure(Sender: TObject; const APayload: TOBDDataSourcePayload)
      begin
        Inc(Hit);
      end;
    P := Default(TOBDDataSourcePayload);
    P.Kind := dsLiveData;
    P.PID := $0C;
    DS.Notify(P);
    Assert.AreEqual(1, Hit);
  finally
    DS.Free;
  end;
end;

procedure TPhase5Tests.DataSource_ActiveToggleFiresStateChange;
var
  DS: TOBDDataSource;
  StateHit: Integer;
begin
  DS := TOBDDataSource.Create(nil);
  try
    StateHit := 0;
    DS.OnStateChange :=
      procedure(Sender: TObject; AActive: Boolean)
      begin
        Inc(StateHit);
      end;
    DS.Active := False;
    DS.Active := True;
    Assert.AreEqual(2, StateHit);
  finally
    DS.Free;
  end;
end;

{ ---- WWH-OBD severity classification ------------------------------------- }

procedure TPhase5Tests.WWHOBD_ClassifySeverityZeroIsUnknown;
begin
  Assert.AreEqual(Ord(wcUnknown),
    Ord(TOBDWWHOBD.ClassifySeverity($00)));
end;

procedure TPhase5Tests.WWHOBD_ClassifySeverityImmediateIsCheckNow;
begin
  Assert.AreEqual(Ord(wcCheckNow),
    Ord(TOBDWWHOBD.ClassifySeverity(WWHOBD_SEV_CHECK_IMMEDIATELY)));
end;

procedure TPhase5Tests.WWHOBD_ClassifySeverityBothIsCheckBoth;
begin
  Assert.AreEqual(
    Ord(wcCheckBoth),
    Ord(TOBDWWHOBD.ClassifySeverity(
      WWHOBD_SEV_CHECK_IMMEDIATELY or WWHOBD_SEV_CHECK_AT_NEXT_HALT)));
end;

procedure TPhase5Tests.WWHOBD_ClassifySeverityHaltIsCheckHalt;
begin
  Assert.AreEqual(Ord(wcCheckHalt),
    Ord(TOBDWWHOBD.ClassifySeverity(WWHOBD_SEV_CHECK_AT_NEXT_HALT)));
end;

procedure TPhase5Tests.WWHOBD_ClassifySeverityMaintenanceOnly;
begin
  Assert.AreEqual(Ord(wcMaintenance),
    Ord(TOBDWWHOBD.ClassifySeverity(WWHOBD_SEV_MAINTENANCE_ONLY)));
end;

procedure TPhase5Tests.WWHOBD_ReadBySeverityWithoutProtocolRaises;
var
  W: TOBDWWHOBD;
begin
  W := TOBDWWHOBD.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        W.ReadBySeverity;
      end,
      EOBDConfig);
  finally
    W.Free;
  end;
end;

procedure TPhase5Tests.WWHReadiness_ReadWithoutProtocolRaises;
var
  R: TOBDWWHReadiness;
begin
  R := TOBDWWHReadiness.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Read;
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPhase5Tests);

end.
