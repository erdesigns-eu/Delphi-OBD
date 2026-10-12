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
  end;

implementation

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
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

initialization
  TDUnitX.RegisterTestFixture(TSimulatorTests);

end.
