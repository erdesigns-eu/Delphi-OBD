//------------------------------------------------------------------------------
//  Tests.OBD.Diagnostics.UDS
//
//  Coverage for the UDS-diagnostic components:
//    - TOBDUDS              (session hub)
//    - TOBDUDSReset         (ECU reset, SID 0x11)
//    - TOBDUDSReadMemory    (ReadMemoryByAddress, SID 0x23)
//    - TOBDUDSIOControl     (IOControlByIdentifier, SID 0x2F)
//
//  Lifecycle + safety-gate + default-property coverage. Wire-level
//  encode / decode is covered by the protocol fixtures.
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
  OBD.Diagnostics.UDS.IOControl,
  OBD.Diagnostics.UDS.ReadDID,
  OBD.Diagnostics.UDS.ReadDTC,
  OBD.Diagnostics.UDS.Periodic,
  OBD.Diagnostics.UDS.DynamicDID;

type
  /// <summary>
  ///   DUnitX fixture for the UDS-diagnostic components.
  /// </summary>
  [TestFixture]
  TUDSDiagnosticsTests = class
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

    [Test] procedure ReadDID_WithoutProtocolRaises;
    [Test] procedure ReadDID_LengthArrayMismatchRaises;

    [Test] procedure ReadDTC_WithoutProtocolRaises;
    [Test] procedure ReadDTC_DecodeJ2012AllPrefixes;

    [Test] procedure Periodic_StartWithoutProtocolRaises;
    [Test] procedure Periodic_StartWithStopRateRaises;
    [Test] procedure Periodic_DispatchSampleFiresHandler;

    [Test] procedure DynamicDID_DefineWithoutProtocolRaises;
  end;

implementation

{ ---- TOBDUDS ------------------------------------------------------------- }

procedure TUDSDiagnosticsTests.UDS_DefaultsCurrentSessionIsDefault;
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

procedure TUDSDiagnosticsTests.UDS_DefaultsKeepAliveFalseInterval2000;
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

procedure TUDSDiagnosticsTests.UDS_OpenWithoutProtocolRaises;
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

procedure TUDSDiagnosticsTests.UDS_TesterPresentWithoutProtocolRaises;
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

procedure TUDSDiagnosticsTests.UDS_CommunicationControlWithoutProtocolRaises;
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

procedure TUDSDiagnosticsTests.Reset_DefaultsAutoExecuteFalse;
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

procedure TUDSDiagnosticsTests.Reset_WithoutAutoExecuteRaises;
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

procedure TUDSDiagnosticsTests.Reset_WithoutProtocolRaises;
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

procedure TUDSDiagnosticsTests.ReadMemory_DefaultsFour4ByteFormats;
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

procedure TUDSDiagnosticsTests.ReadMemory_WithoutProtocolRaises;
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

procedure TUDSDiagnosticsTests.ReadMemory_ZeroLengthRaises;
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

procedure TUDSDiagnosticsTests.ReadMemory_BadFormatBytesRaises;
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

procedure TUDSDiagnosticsTests.IOControl_DefaultsAutoExecuteFalse;
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

procedure TUDSDiagnosticsTests.IOControl_WithoutAutoExecuteRaises;
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

procedure TUDSDiagnosticsTests.IOControl_WithoutProtocolRaises;
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

{ ---- TOBDUDSReadDID ----------------------------------------------------- }

procedure TUDSDiagnosticsTests.ReadDID_WithoutProtocolRaises;
var
  R: TOBDUDSReadDID;
begin
  R := TOBDUDSReadDID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Read([$F190], [17]);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TUDSDiagnosticsTests.ReadDID_LengthArrayMismatchRaises;
var
  R: TOBDUDSReadDID;
begin
  R := TOBDUDSReadDID.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.Read([$F190, $F18C], [17]);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

{ ---- TOBDUDSReadDTC ----------------------------------------------------- }

procedure TUDSDiagnosticsTests.ReadDTC_WithoutProtocolRaises;
var
  R: TOBDUDSReadDTC;
begin
  R := TOBDUDSReadDTC.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        R.ReadByStatusMask($FF);
      end,
      EOBDConfig);
  finally
    R.Free;
  end;
end;

procedure TUDSDiagnosticsTests.ReadDTC_DecodeJ2012AllPrefixes;
begin
  Assert.AreEqual('P0143', TOBDUDSReadDTC.DecodeJ2012($01, $43));
  Assert.AreEqual('C0220', TOBDUDSReadDTC.DecodeJ2012($42, $20));
  Assert.AreEqual('B0300', TOBDUDSReadDTC.DecodeJ2012($83, $00));
  Assert.AreEqual('U0000', TOBDUDSReadDTC.DecodeJ2012($C0, $00));
end;

{ ---- TOBDUDSReadByPeriodic --------------------------------------------- }

procedure TUDSDiagnosticsTests.Periodic_StartWithoutProtocolRaises;
var
  P: TOBDUDSReadByPeriodic;
begin
  P := TOBDUDSReadByPeriodic.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        P.Start(UDS_PER_RATE_MEDIUM, [$01, $02]);
      end,
      EOBDConfig);
  finally
    P.Free;
  end;
end;

procedure TUDSDiagnosticsTests.Periodic_StartWithStopRateRaises;
var
  P: TOBDUDSReadByPeriodic;
begin
  P := TOBDUDSReadByPeriodic.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        P.Start(UDS_PER_RATE_STOP, [$01]);
      end,
      EOBDConfig);
  finally
    P.Free;
  end;
end;

procedure TUDSDiagnosticsTests.Periodic_DispatchSampleFiresHandler;
var
  P: TOBDUDSReadByPeriodic;
  Hit: Integer;
  GotPID: Byte;
  GotLen: Integer;
begin
  P := TOBDUDSReadByPeriodic.Create(nil);
  try
    Hit := 0;
    GotPID := 0;
    GotLen := -1;
    P.OnSample :=
      procedure(Sender: TObject; APID: Byte; const AData: TBytes)
      begin
        Inc(Hit);
        GotPID := APID;
        GotLen := Length(AData);
      end;
    P.DispatchSample($42, TBytes.Create($AA, $BB, $CC));
    Assert.AreEqual(1, Hit);
    Assert.AreEqual($42, Integer(GotPID));
    Assert.AreEqual(3, GotLen);
  finally
    P.Free;
  end;
end;

{ ---- TOBDUDSDynamicDID -------------------------------------------------- }


procedure TUDSDiagnosticsTests.DynamicDID_DefineWithoutProtocolRaises;
var
  D: TOBDUDSDynamicDID;
  S: TOBDUDSDDDSlice;
begin
  D := TOBDUDSDynamicDID.Create(nil);
  try
    S.SourceDID := $F190;
    S.Position := 1;
    S.MemorySize := 4;
    Assert.WillRaise(
      procedure
      begin
        D.DefineByDID($F300, [S]);
      end,
      EOBDConfig);
  finally
    D.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUDSDiagnosticsTests);

end.
