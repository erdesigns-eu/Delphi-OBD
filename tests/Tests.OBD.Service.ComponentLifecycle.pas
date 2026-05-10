//------------------------------------------------------------------------------
//  Tests.OBD.Service.ComponentLifecycle
//
//  Smoke coverage for the six service components that don't
//  have their own dedicated lifecycle fixture: LiveData,
//  DTCs, FreezeFrame, OnBoardMonitor, Actuator, VIN.
//
//  The decoder formulas for LiveData / DTCs are exercised in
//  Tests.OBD.Service. This unit covers the component shell:
//  defaults, the Protocol-not-assigned guard raises
//  EOBDConfig, Free is clean, FreeNotification clears the
//  Protocol reference. Real I/O lives behind a connected
//  protocol and is out of unit-test scope.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.ComponentLifecycle;

interface

uses
  System.SysUtils, System.Classes,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor,
  OBD.Service.Actuator,
  OBD.Service.VIN;

type
  [TestFixture]
  TServiceComponentLifecycleTests = class
  public
    [Test] procedure LiveData_ReadWithoutProtocolRaises;
    [Test] procedure DTCs_ReadConfirmedWithoutProtocolRaises;
    [Test] procedure FreezeFrame_ReadWithoutProtocolRaises;
    [Test] procedure OnBoardMonitor_ReadWithoutProtocolRaises;
    [Test] procedure Actuator_DefaultsAutoExecuteFalse;
    [Test] procedure Actuator_SendWithoutProtocolRaises;
    [Test] procedure Actuator_SendWithoutAutoExecuteRaises;
    [Test] procedure VIN_ReadWithoutProtocolRaises;
    [Test] procedure VIN_ReadUDSWithoutProtocolRaises;
    [Test] procedure FreeNotification_ClearsProtocolOnLiveData;
    [Test] procedure FreeNotification_ClearsProtocolOnDTCs;
    [Test] procedure FreeNotification_ClearsProtocolOnFreezeFrame;
    [Test] procedure FreeNotification_ClearsProtocolOnOnBoardMonitor;
    [Test] procedure FreeNotification_ClearsProtocolOnActuator;
    [Test] procedure FreeNotification_ClearsProtocolOnVIN;
    [Test] procedure FreeIsClean_AllSixComponents;
  end;

implementation

procedure TServiceComponentLifecycleTests.LiveData_ReadWithoutProtocolRaises;
var C: TOBDLiveData;
begin
  C := TOBDLiveData.Create(nil);
  try
    Assert.WillRaise(procedure begin C.Read($0C) end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.DTCs_ReadConfirmedWithoutProtocolRaises;
var C: TOBDDTCs;
begin
  C := TOBDDTCs.Create(nil);
  try
    Assert.WillRaise(procedure begin C.ReadConfirmed end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreezeFrame_ReadWithoutProtocolRaises;
var C: TOBDFreezeFrame;
begin
  C := TOBDFreezeFrame.Create(nil);
  try
    Assert.WillRaise(procedure begin C.Read($0C, 0) end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.OnBoardMonitor_ReadWithoutProtocolRaises;
var C: TOBDOnBoardMonitor;
begin
  C := TOBDOnBoardMonitor.Create(nil);
  try
    Assert.WillRaise(procedure begin C.Read($00) end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.Actuator_DefaultsAutoExecuteFalse;
var C: TOBDActuator;
begin
  C := TOBDActuator.Create(nil);
  try
    Assert.IsFalse(C.AutoExecute,
      'Actuator must ship AutoExecute=False (destructive surface)');
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.Actuator_SendWithoutProtocolRaises;
var C: TOBDActuator;
begin
  C := TOBDActuator.Create(nil);
  try
    Assert.WillRaise(procedure begin C.Send($01) end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.Actuator_SendWithoutAutoExecuteRaises;
var
  C: TOBDActuator;
  P: TOBDProtocol;
begin
  // Wire a Protocol so the first guard passes; the second guard
  // (AutoExecute=False) should still refuse.
  P := TOBDProtocol.Create(nil);
  C := TOBDActuator.Create(nil);
  try
    C.Protocol := P;
    Assert.WillRaise(procedure begin C.Send($01) end, EOBDConfig);
  finally
    C.Free;
    P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.VIN_ReadWithoutProtocolRaises;
var C: TOBDVIN;
begin
  C := TOBDVIN.Create(nil);
  try
    Assert.WillRaise(procedure begin C.ReadOBDII end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.VIN_ReadUDSWithoutProtocolRaises;
var C: TOBDVIN;
begin
  C := TOBDVIN.Create(nil);
  try
    Assert.WillRaise(procedure begin C.ReadUDS end, EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnLiveData;
var
  P: TOBDProtocol;
  C: TOBDLiveData;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDLiveData.Create(nil);
  try
    C.Protocol := P;
    Assert.IsNotNull(C.Protocol);
    P.Free;
    P := nil;
    Assert.IsNull(C.Protocol,
      'FreeNotification must clear Protocol reference');
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnDTCs;
var P: TOBDProtocol; C: TOBDDTCs;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDDTCs.Create(nil);
  try
    C.Protocol := P;
    P.Free;  P := nil;
    Assert.IsNull(C.Protocol);
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnFreezeFrame;
var P: TOBDProtocol; C: TOBDFreezeFrame;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDFreezeFrame.Create(nil);
  try
    C.Protocol := P;
    P.Free;  P := nil;
    Assert.IsNull(C.Protocol);
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnOnBoardMonitor;
var P: TOBDProtocol; C: TOBDOnBoardMonitor;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDOnBoardMonitor.Create(nil);
  try
    C.Protocol := P;
    P.Free;  P := nil;
    Assert.IsNull(C.Protocol);
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnActuator;
var P: TOBDProtocol; C: TOBDActuator;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDActuator.Create(nil);
  try
    C.Protocol := P;
    P.Free;  P := nil;
    Assert.IsNull(C.Protocol);
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeNotification_ClearsProtocolOnVIN;
var P: TOBDProtocol; C: TOBDVIN;
begin
  P := TOBDProtocol.Create(nil);
  C := TOBDVIN.Create(nil);
  try
    C.Protocol := P;
    P.Free;  P := nil;
    Assert.IsNull(C.Protocol);
  finally
    C.Free;
    if P <> nil then P.Free;
  end;
end;

procedure TServiceComponentLifecycleTests.FreeIsClean_AllSixComponents;
begin
  // Lifecycle smoke: create + free in order shouldn't leak or crash.
  TOBDLiveData.Create(nil).Free;
  TOBDDTCs.Create(nil).Free;
  TOBDFreezeFrame.Create(nil).Free;
  TOBDOnBoardMonitor.Create(nil).Free;
  TOBDActuator.Create(nil).Free;
  TOBDVIN.Create(nil).Free;
  Assert.Pass('Six components round-tripped through Create + Free.');
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceComponentLifecycleTests);

end.
