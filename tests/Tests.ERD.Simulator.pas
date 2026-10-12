//------------------------------------------------------------------------------
//  Tests.ERD.Simulator
//
//  Unit tests for the demo vehicle model and ELM327 emulator text formatting.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//    2026-10-12  ERD  Cover freeze-frame, IPT, Mode 06 and diesel readiness fixes.
//------------------------------------------------------------------------------

unit Tests.ERD.Simulator;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Coverage for the simulator vehicle and ELM327 formatter.</summary>
  [TestFixture]
  TSimulatorTests = class
  public
    /// <summary>The engine support bitmap is computed from implemented PIDs.</summary>
    [Test] procedure VehicleMode01SupportBitmap;
    /// <summary>Default ATZ includes echo, banner and prompt.</summary>
    [Test] procedure ATZBannerIncludesPrompt;
    /// <summary>CAF1, headers off emits payload-only lines.</summary>
    [Test] procedure HeadersOffMode0100;
    /// <summary>ATS0 removes spaces from frame-like response lines.</summary>
    [Test] procedure SpacesOffMode0100;
    /// <summary>CAF1, headers off emits ELM multi-frame VIN blocks.</summary>
    [Test] procedure HeadersOffVINMultiframe;
    /// <summary>CAF1, headers on emits CAN frames with PCI bytes.</summary>
    [Test] procedure HeadersOnVINMultiframe;
    /// <summary>Functional Mode 03 shows engine and transmission ECU responses.</summary>
    [Test] procedure Mode03MultiECU;
    /// <summary>Unknown physical ECU requests produce NO DATA.</summary>
    [Test] procedure PhysicalNoData;
    /// <summary>Mode 02 replies echo the requested freeze-frame number.</summary>
    [Test] procedure Mode02FreezeFrameEchoesFrame;
    /// <summary>Mode 09 advertises and returns compression-ignition IPT.</summary>
    [Test] procedure Mode09CompressionIptNodi;
    /// <summary>Mode 06 records use the CAN MID/TID/UASID layout.</summary>
    [Test] procedure Mode06RecordsUseCanLayout;
    /// <summary>PID 01 / 41 report diesel readiness bits after a clear.</summary>
    [Test] procedure PID01DieselReadiness;
    /// <summary>The service parser accepts CAN Mode 06 records.</summary>
    [Test] procedure OnBoardMonitorParsesCanMode06Record;
  end;

implementation

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.Adapter,
  ERD.Protocol,
  ERD.Service.OnBoardMonitor,
  ERD.Simulator.Vehicle,
  ERD.Simulator.ELM327;

const
  CR = #13;

procedure DisableEcho(AEmu: TOBDELM327Emulator);
begin
  AEmu.Execute('ATE0');
end;

procedure TSimulatorTests.VehicleMode01SupportBitmap;
var
  Vehicle: TOBDSimVehicle;
begin
  Vehicle := TOBDSimVehicle.Create;
  try
    Assert.AreEqual('41 00 BE 3F A8 13', Vehicle.Response($00));
  finally
    Vehicle.Free;
  end;
end;

procedure TSimulatorTests.ATZBannerIncludesPrompt;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    Assert.AreEqual('ATZ' + CR + 'ELM327 v2.3' + CR + '>', Emu.Execute('ATZ'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.HeadersOffMode0100;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Assert.AreEqual('41 00 BE 3F A8 13' + CR + '41 00 00 18 00 13' + CR + '>',
      Emu.Execute('0100'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.SpacesOffMode0100;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Emu.Execute('ATS0');
    Assert.AreEqual('4100BE3FA813' + CR + '410000180013' + CR + '>',
      Emu.Execute('0100'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.HeadersOffVINMultiframe;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Assert.AreEqual('014' + CR + '0: 49 02 01 57 56 57' + CR +
      '1: 5A 5A 5A 41 55 5A 47' + CR +
      '2: 57 31 32 33 34 35 36' + CR + '>', Emu.Execute('0902'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.HeadersOnVINMultiframe;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Emu.Execute('ATH1');
    Assert.AreEqual('7E8 10 14 49 02 01 57 56 57' + CR +
      '7E8 21 5A 5A 5A 41 55 5A 47' + CR +
      '7E8 22 57 31 32 33 34 35 36' + CR + '>', Emu.Execute('0902'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.Mode03MultiECU;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Assert.AreEqual('43 01 04 01' + CR + '43 00' + CR + '>', Emu.Execute('03'));
    Emu.Execute('ATH1');
    Assert.AreEqual('7E8 04 43 01 04 01' + CR + '7E9 02 43 00' + CR + '>',
      Emu.Execute('03'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.PhysicalNoData;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Emu.Execute('ATSH7E2');
    Assert.AreEqual('NO DATA' + CR + '>', Emu.Execute('0100'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.Mode02FreezeFrameEchoesFrame;
var
  Emu: TOBDELM327Emulator;
begin
  Emu := TOBDELM327Emulator.Create;
  try
    DisableEcho(Emu);
    Assert.AreEqual('42 0C 00 21 70' + CR + '>', Emu.Execute('020C00'));
    Assert.AreEqual('NO DATA' + CR + '>', Emu.Execute('020C01'));
    Assert.AreEqual('NO DATA' + CR + '>', Emu.Execute('020C'));
  finally
    Emu.Free;
  end;
end;

procedure TSimulatorTests.Mode09CompressionIptNodi;
var
  Vehicle: TOBDSimVehicle;
  Responses: TOBDSimECUResponses;
begin
  Vehicle := TOBDSimVehicle.Create;
  try
    Responses := Vehicle.HandleRequest(OBD_SIM_ENGINE_REQUEST_ID,
      TBytes.Create($09, $00));
    Assert.AreEqual(1, Integer(Length(Responses)));
    Assert.AreEqual($54, Integer(Responses[0].Payload[2]));
    Assert.AreEqual($60, Integer(Responses[0].Payload[3]));

    Responses := Vehicle.HandleRequest(OBD_SIM_TRANSMISSION_REQUEST_ID,
      TBytes.Create($09, $00));
    Assert.AreEqual(1, Integer(Length(Responses)));
    Assert.AreEqual($14, Integer(Responses[0].Payload[2]));
    Assert.AreEqual($40, Integer(Responses[0].Payload[3]));

    Responses := Vehicle.HandleRequest(OBD_SIM_ENGINE_REQUEST_ID,
      TBytes.Create($09, $0B));
    Assert.AreEqual(1, Integer(Length(Responses)));
    Assert.AreEqual($0B, Integer(Responses[0].Payload[1]));
    Assert.AreEqual($12, Integer(Responses[0].Payload[2]));
    Assert.AreEqual(39, Integer(Length(Responses[0].Payload)));

    Responses := Vehicle.HandleRequest(OBD_SIM_ENGINE_REQUEST_ID,
      TBytes.Create($09, $08));
    Assert.AreEqual(0, Integer(Length(Responses)));
  finally
    Vehicle.Free;
  end;
end;

procedure TSimulatorTests.Mode06RecordsUseCanLayout;
var
  Vehicle: TOBDSimVehicle;
  Responses: TOBDSimECUResponses;
begin
  Vehicle := TOBDSimVehicle.Create;
  try
    Responses := Vehicle.HandleRequest(OBD_SIM_ENGINE_REQUEST_ID,
      TBytes.Create($06, $01));
    Assert.AreEqual(1, Integer(Length(Responses)));
    Assert.AreEqual(10, Integer(Length(Responses[0].Payload)));
    Assert.AreEqual($46, Integer(Responses[0].Payload[0]));
    Assert.AreEqual($01, Integer(Responses[0].Payload[1]));
    Assert.AreEqual($01, Integer(Responses[0].Payload[2]));
    Assert.AreEqual($0A, Integer(Responses[0].Payload[3]));
    Assert.AreEqual($00C8, Integer((Word(Responses[0].Payload[8]) shl 8) or
      Word(Responses[0].Payload[9])));
  finally
    Vehicle.Free;
  end;
end;

procedure TSimulatorTests.PID01DieselReadiness;
var
  Vehicle: TOBDSimVehicle;
begin
  Vehicle := TOBDSimVehicle.Create;
  try
    Assert.AreEqual('41 01 81 0F EB 00',
      Vehicle.Response(OBD_SIM_PID_MONITOR_STATUS));
    Assert.AreEqual('41 41 00 0F EB 00',
      Vehicle.Response(OBD_SIM_PID_MONITOR_DRIVE));

    Vehicle.ClearDTCs;

    Assert.AreEqual('41 01 00 0F EB EB',
      Vehicle.Response(OBD_SIM_PID_MONITOR_STATUS));
    Assert.AreEqual('41 41 00 0F EB EB',
      Vehicle.Response(OBD_SIM_PID_MONITOR_DRIVE));
  finally
    Vehicle.Free;
  end;
end;

procedure TSimulatorTests.OnBoardMonitorParsesCanMode06Record;
var
  Connection: TOBDEmulatorConnection;
  Adapter: TOBDAdapter;
  Protocol: TOBDProtocol;
  Monitor: TOBDOnBoardMonitor;
  Results: TArray<TOBDMonitorResult>;
begin
  Connection := TOBDEmulatorConnection.Create(nil);
  Adapter := TOBDAdapter.Create(nil);
  Protocol := TOBDProtocol.Create(nil);
  Monitor := TOBDOnBoardMonitor.Create(nil);
  try
    Connection.ResponseDelayMs := 0;
    Connection.Open;
    Adapter.Connection := Connection;
    Protocol.Adapter := Adapter;
    Monitor.Protocol := Protocol;

    Results := Monitor.Read($01);

    Assert.AreEqual(1, Integer(Length(Results)));
    Assert.AreEqual($01, Integer(Results[0].MID));
    Assert.AreEqual($01, Integer(Results[0].TID));
    Assert.AreEqual(0, Integer(Results[0].ComponentID));
    Assert.AreEqual($0A, Integer(Results[0].UnitAndScale));
    Assert.AreEqual(100, Integer(Results[0].TestValue));
    Assert.AreEqual(0, Integer(Results[0].MinLimit));
    Assert.AreEqual(200, Integer(Results[0].MaxLimit));
  finally
    Monitor.Free;
    Protocol.Free;
    Adapter.Free;
    Connection.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSimulatorTests);

end.
