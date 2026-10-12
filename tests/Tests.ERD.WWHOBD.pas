//------------------------------------------------------------------------------
//  Tests.ERD.WWHOBD
//
//  Coverage for TOBDWWHOBD WWH-OBD / SAE J1979-2 decoding and request
//  construction: severity bits, DTC class bits, 0x19 sub-functions
//  0x42 / 0x55 / 0x56, multi-ECU parsing and J1979 monitor status.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.WWHOBD;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  DUnitX.TestFramework,
  ERD.Adapter,
  ERD.Adapter.Types,
  ERD.Connection,
  ERD.Connection.Mock,
  ERD.Connection.Types,
  ERD.Protocol,
  ERD.Protocol.Types,
  ERD.Protocol.UDS,
  ERD.Service.DriveCycle.Types,
  ERD.WWHOBD;

type
  [TestFixture]
  TWWHOBDTests = class
  public
    /// <summary>ISO 14229 severity bits are decoded in bits 7..5.</summary>
    [Test] procedure SeverityBitsMatchISO14229;
    /// <summary>DTC class bits 0..4 are decoded separately from severity.</summary>
    [Test] procedure DTCClassBitsDecode;
    /// <summary>0x59/0x42 responses keep ECU headers and failure-type bytes.</summary>
    [Test] procedure MaskRecordMultiEcuParses;
    /// <summary>Trailing short records are ignored.</summary>
    [Test] procedure PartialMaskRecordIgnored;
    /// <summary>0x55 permanent DTC responses are four-byte records.</summary>
    [Test] procedure PermanentStatusParses;
    /// <summary>0x56 readiness-group DTC responses carry the group id.</summary>
    [Test] procedure ReadinessGroupParses;
    /// <summary>Component requests use the corrected 19 42 / 19 55 / 19 56 shapes.</summary>
    [Test] procedure ComponentBuildsCorrectRequests;
    /// <summary>J1979-2 readiness DIDs carry PID 01 / 41 monitor-status bytes.</summary>
    [Test] procedure J1979MonitorStatusPayloadDecodes;
  end;

implementation

type
  TWWHStubAdapter = class(TOBDAdapter)
  public
    LastCommand: string;
    ResponseRaw: string;
  protected
    function ExecuteCommand(const ACommand: string; ATimeoutMs: Cardinal)
      : TOBDAdapterResponse; override;
  end;

function TWWHStubAdapter.ExecuteCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  LastCommand := ACommand;
  Result.Command := ACommand;
  Result.Raw := ResponseRaw;
  SetLength(Result.Lines, 1);
  Result.Lines[0] := ResponseRaw;
  Result.Elapsed := 1;
  Result.IsError := False;
  Result.ErrorKeyword := '';
end;

function DecodeRaw(ASid: Byte; const AData: TBytes;
  const ARaw: string): TOBDResponse;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.Protocol := apWWHOBD;
  Req.ServiceID := ASid;
  Req.Data := AData;
  TOBDUDSCodec.Decode(Req, ARaw, Result);
end;

procedure TWWHOBDTests.SeverityBitsMatchISO14229;
begin
  Assert.AreEqual($20, Integer(WWHOBD_SEV_MAINTENANCE_ONLY));
  Assert.AreEqual($40, Integer(WWHOBD_SEV_CHECK_AT_NEXT_HALT));
  Assert.AreEqual($80, Integer(WWHOBD_SEV_CHECK_IMMEDIATELY));
  Assert.IsTrue(TOBDWWHOBD.ClassifySeverity($20) = wcMaintenance);
  Assert.IsTrue(TOBDWWHOBD.ClassifySeverity($40) = wcCheckHalt);
  Assert.IsTrue(TOBDWWHOBD.ClassifySeverity($80) = wcCheckNow);
  Assert.IsTrue(TOBDWWHOBD.ClassifySeverity($C0) = wcCheckBoth);
end;

procedure TWWHOBDTests.DTCClassBitsDecode;
begin
  Assert.IsTrue(TOBDWWHOBD.ClassifyWWHClass($01) = wcClass0);
  Assert.IsTrue(TOBDWWHOBD.ClassifyWWHClass($02) = wcClassA);
  Assert.IsTrue(TOBDWWHOBD.ClassifyWWHClass($04) = wcClassB1);
  Assert.IsTrue(TOBDWWHOBD.ClassifyWWHClass($08) = wcClassB2);
  Assert.IsTrue(TOBDWWHOBD.ClassifyWWHClass($10) = wcClassC);
end;

procedure TWWHOBDTests.MaskRecordMultiEcuParses;
var
  E: TArray<TOBDWWHDtcEntry>;
begin
  E := TOBDWWHOBD.ParseBySeverityResponse(DecodeRaw($19,
    TBytes.Create($42, $33, $FF, $E0),
    '7E8 10 0B 59 42 33 FF E0 01'#13 +
    '7E8 21 82 01 01 1C 09 00 00'#13 +
    '7E9 10 0B 59 42 33 FF E0 01'#13 +
    '7E9 21 24 41 23 2A 08 00 00'#13'>'));
  Assert.AreEqual(2, Integer(Length(E)));
  Assert.AreEqual('P0101-1C', E[0].Code);
  Assert.AreEqual('P0101', E[0].PlainCode);
  Assert.AreEqual($1C, Integer(E[0].FailureTypeByte));
  Assert.AreEqual('7E8', E[0].Ecu);
  Assert.AreEqual(Int64($7E8), Int64(E[0].EcuId));
  Assert.IsTrue(E[0].DtcClass = wcCheckNow);
  Assert.IsTrue(E[0].WWHClass = wcClassA);
  Assert.AreEqual('C0123-2A', E[1].Code);
  Assert.IsTrue(E[1].DtcClass = wcMaintenance);
  Assert.IsTrue(E[1].WWHClass = wcClassB1);
end;

procedure TWWHOBDTests.PartialMaskRecordIgnored;
var
  R: TOBDResponse;
  E: TArray<TOBDWWHDtcEntry>;
begin
  R := MakeOBDResponse;
  R.ServiceID := $59;
  R.Data := TBytes.Create($42, $33, $FF, $E0, $01, $82, $01);
  E := TOBDWWHOBD.ParseBySeverityResponse(R);
  Assert.AreEqual(0, Integer(Length(E)));
end;

procedure TWWHOBDTests.PermanentStatusParses;
var
  R: TOBDResponse;
  E: TArray<TOBDWWHDtcEntry>;
begin
  R := MakeOBDResponse;
  R.ServiceID := $59;
  R.Data := TBytes.Create($55, $33, $FF, $01, $01, $01, $1C, $09);
  E := TOBDWWHOBD.ParsePermanentResponse(R);
  Assert.AreEqual(1, Integer(Length(E)));
  Assert.AreEqual('P0101-1C', E[0].Code);
  Assert.AreEqual($09, Integer(E[0].StatusByte));
  Assert.AreEqual($33, Integer(E[0].FunctionalGroupIdentifier));
end;

procedure TWWHOBDTests.ReadinessGroupParses;
var
  R: TOBDResponse;
  E: TArray<TOBDWWHDtcEntry>;
begin
  R := MakeOBDResponse;
  R.ServiceID := $59;
  R.Data := TBytes.Create($56, $33, $FF, $01, $05, $01, $01, $1C, $09);
  E := TOBDWWHOBD.ParseReadinessGroupResponse(R);
  Assert.AreEqual(1, Integer(Length(E)));
  Assert.AreEqual('P0101-1C', E[0].Code);
  Assert.AreEqual($05, Integer(E[0].ReadinessGroupIdentifier));
end;

procedure TWWHOBDTests.ComponentBuildsCorrectRequests;
var
  Transport: TOBDMockTransport;
  TransportIntf: IOBDConnectionTransport;
  Conn: TOBDConnection;
  Adapter: TWWHStubAdapter;
  Protocol: TOBDProtocol;
  WWH: TOBDWWHOBD;
  E: TArray<TOBDWWHDtcEntry>;
begin
  Transport := TOBDMockTransport.Create;
  TransportIntf := Transport;
  Transport.SimulateOpen;
  Conn := TOBDConnection.Create(nil);
  Adapter := TWWHStubAdapter.Create(nil);
  Protocol := TOBDProtocol.Create(nil);
  WWH := TOBDWWHOBD.Create(nil);
  try
    Conn.CustomTransport := TransportIntf;
    Conn.Open;
    Adapter.Connection := Conn;
    Protocol.Adapter := Adapter;
    Protocol.Application := apWWHOBD;
    WWH.Protocol := Protocol;

    Adapter.ResponseRaw := '59 42 33 FF E0 01 82 01 01 1C 09';
    E := WWH.ReadBySeverity;
    Assert.AreEqual('19 42 33 FF E0', Adapter.LastCommand);
    Assert.AreEqual(1, Integer(Length(E)));

    Adapter.ResponseRaw := '59 55 33 FF 01 01 01 1C 09';
    E := WWH.ReadPermanent;
    Assert.AreEqual('19 55 33', Adapter.LastCommand);
    Assert.AreEqual(1, Integer(Length(E)));

    Adapter.ResponseRaw := '59 56 33 FF 01 05 01 01 1C 09';
    E := WWH.ReadByGroup($05);
    Assert.AreEqual('19 56 33 05', Adapter.LastCommand);
    Assert.AreEqual(1, Integer(Length(E)));
  finally
    WWH.Free;
    Protocol.Free;
    Adapter.Free;
    Conn.Free;
  end;
end;

procedure TWWHOBDTests.J1979MonitorStatusPayloadDecodes;
var
  Status: TOBDMonitorStatus;
  ErrorText: string;
begin
  Assert.IsTrue(DecodeMonitorStatus(OBDPIDPayload(TBytes.Create($01, $81, $07,
    $65, $24), $01, 4), Status, ErrorText));
  Assert.IsTrue(Status.MILOn);
  Assert.AreEqual(1, Status.DTCCount);
  Assert.IsFalse(Status.CompressionIgnition);
  Assert.AreEqual(11, Integer(Length(Status.Monitors)));
  Assert.IsTrue(Status.Monitors[5].Supported);
  Assert.IsFalse(Status.Monitors[5].Complete);
end;

initialization
  TDUnitX.RegisterTestFixture(TWWHOBDTests);

end.
