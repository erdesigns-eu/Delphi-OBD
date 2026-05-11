//------------------------------------------------------------------------------
//  Tests.OBD.Diagnostics.UDS
//
//  Coverage for the Phase 6 UDS-diagnostic components:
//    - TOBDUDS              (session hub)
//    - TOBDUDSReset         (ECU reset, SID 0x11)
//    - TOBDUDSReadMemory    (ReadMemoryByAddress, SID 0x23)
//    - TOBDUDSIOControl     (IOControlByIdentifier, SID 0x2F)
//
//  Lifecycle + safety-gate + default-property coverage. Wire-level
//  encode / decode is covered by the Phase 4 protocol fixtures.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.OBD.Diagnostics.UDS;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Errors,
  OBD.Types,
  OBD.Diagnostics.UDS,
  OBD.Diagnostics.UDS.Reset,
  OBD.Diagnostics.UDS.ReadMemory,
  OBD.Diagnostics.UDS.IOControl;

type
  /// <summary>
  ///   DUnitX fixture for the Phase 6 UDS-diagnostic components.
  /// </summary>
  [TestFixture]
  TPhase6UDSTests = class
  public
    [Test] procedure UDS_DefaultsCurrentSessionIsDefault;
    [Test] procedure UDS_DefaultsKeepAliveFalseInterval2000;
    [Test] procedure UDS_OpenWithoutProtocolRaises;
    [Test] procedure UDS_TesterPresentWithoutProtocolRaises;
    [Test] procedure UDS_CommunicationControlWithoutProtocolRaises;

    [Test] procedure Reset_DefaultsAutoExecuteFalse;
    [Test] procedure Reset_WithoutAutoExecuteRaises;
    [Test] procedure Reset_WithoutProtocolRaises;

    [Test] procedure ReadMemory_DefaultsFour4ByteFormats;
    [Test] procedure ReadMemory_WithoutProtocolRaises;
    [Test] procedure ReadMemory_ZeroLengthRaises;
    [Test] procedure ReadMemory_BadFormatBytesRaises;

    [Test] procedure IOControl_DefaultsAutoExecuteFalse;
    [Test] procedure IOControl_WithoutAutoExecuteRaises;
    [Test] procedure IOControl_WithoutProtocolRaises;
  end;

implementation

{ ---- TOBDUDS ------------------------------------------------------------- }

procedure TPhase6UDSTests.UDS_DefaultsCurrentSessionIsDefault;
var
  H: TOBDUDS;
begin
  H := TOBDUDS.Create(nil);
  try
    Assert.AreEqual(Integer(UDS_SESSION_DEFAULT),
      Integer(H.CurrentSession));
  finally
    H.Free;
  end;
end;

procedure TPhase6UDSTests.UDS_DefaultsKeepAliveFalseInterval2000;
var
  H: TOBDUDS;
begin
  H := TOBDUDS.Create(nil);
  try
    Assert.IsFalse(H.KeepAlive);
    Assert.AreEqual(Cardinal(2000), H.KeepAliveIntervalMs);
  finally
    H.Free;
  end;
end;

procedure TPhase6UDSTests.UDS_OpenWithoutProtocolRaises;
var
  H: TOBDUDS;
begin
  H := TOBDUDS.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.Open(UDS_SESSION_EXTENDED);
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TPhase6UDSTests.UDS_TesterPresentWithoutProtocolRaises;
var
  H: TOBDUDS;
begin
  H := TOBDUDS.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.TesterPresent(True);
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

procedure TPhase6UDSTests.UDS_CommunicationControlWithoutProtocolRaises;
var
  H: TOBDUDS;
begin
  H := TOBDUDS.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        H.CommunicationControl($00, $01);
      end,
      EOBDConfig);
  finally
    H.Free;
  end;
end;

{ ---- TOBDUDSReset ------------------------------------------------------- }

procedure TPhase6UDSTests.Reset_DefaultsAutoExecuteFalse;
var
  R: TOBDUDSReset;
begin
  R := TOBDUDSReset.Create(nil);
  try
    Assert.IsFalse(R.AutoExecute);
  finally
    R.Free;
  end;
end;

procedure TPhase6UDSTests.Reset_WithoutAutoExecuteRaises;
var
  R: TOBDUDSReset;
begin
  R := TOBDUDSReset.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Reset(UDS_RESET_SOFT);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TPhase6UDSTests.Reset_WithoutProtocolRaises;
var
  R: TOBDUDSReset;
begin
  R := TOBDUDSReset.Create(nil);
  try
    R.AutoExecute := True;
    Assert.WillRaise(
      procedure
      begin
        R.Reset(UDS_RESET_SOFT);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDUDSReadMemory -------------------------------------------------- }

procedure TPhase6UDSTests.ReadMemory_DefaultsFour4ByteFormats;
var
  M: TOBDUDSReadMemory;
begin
  M := TOBDUDSReadMemory.Create(nil);
  try
    Assert.AreEqual(4, Integer(M.AddressFormatBytes));
    Assert.AreEqual(4, Integer(M.LengthFormatBytes));
  finally
    M.Free;
  end;
end;

procedure TPhase6UDSTests.ReadMemory_WithoutProtocolRaises;
var
  M: TOBDUDSReadMemory;
begin
  M := TOBDUDSReadMemory.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        M.Read($1000, 16);
      end,
      EOBDConfig);
  finally
    M.Free;
  end;
end;

procedure TPhase6UDSTests.ReadMemory_ZeroLengthRaises;
var
  M: TOBDUDSReadMemory;
begin
  M := TOBDUDSReadMemory.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        M.Read($1000, 0);
      end,
      EOBDConfig);
  finally
    M.Free;
  end;
end;

procedure TPhase6UDSTests.ReadMemory_BadFormatBytesRaises;
var
  M: TOBDUDSReadMemory;
begin
  M := TOBDUDSReadMemory.Create(nil);
  try
    M.AddressFormatBytes := 0;
    Assert.WillRaise(
      procedure
      begin
        M.Read($1000, 4);
      end,
      EOBDConfig);
  finally
    M.Free;
  end;
end;

{ ---- TOBDUDSIOControl --------------------------------------------------- }

procedure TPhase6UDSTests.IOControl_DefaultsAutoExecuteFalse;
var
  C: TOBDUDSIOControl;
begin
  C := TOBDUDSIOControl.Create(nil);
  try
    Assert.IsFalse(C.AutoExecute);
  finally
    C.Free;
  end;
end;

procedure TPhase6UDSTests.IOControl_WithoutAutoExecuteRaises;
var
  C: TOBDUDSIOControl;
begin
  C := TOBDUDSIOControl.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        C.Send($2030, UDS_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

procedure TPhase6UDSTests.IOControl_WithoutProtocolRaises;
var
  C: TOBDUDSIOControl;
begin
  C := TOBDUDSIOControl.Create(nil);
  try
    C.AutoExecute := True;
    Assert.WillRaise(
      procedure
      begin
        C.Send($2030, UDS_IOCTL_RETURN_CONTROL_TO_ECU);
      end,
      EOBDConfig);
  finally
    C.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPhase6UDSTests);

end.
