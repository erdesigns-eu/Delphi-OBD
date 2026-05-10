//------------------------------------------------------------------------------
//  DriveCycleAdvise — sample 11
//
//  Offline drive-cycle advice demo. Loads the per-monitor
//  catalogue (catalogs/drive-cycle-generic.json) and for each
//  registered monitor prints the cycle the driver should
//  perform plus the rolled-up duration.
//
//  Also demonstrates the readiness-payload decoder
//  (TOBDDriveCycleAdvisor.DecodePID01) by feeding it a
//  synthetic 4-byte Mode 01 PID 01 response.
//
//  No vehicle / adapter required.
//
//  Build & run:
//    dcc32 -B DriveCycleAdvise.dpr
//    DriveCycleAdvise
//------------------------------------------------------------------------------

program DriveCycleAdvise;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Errors,
  OBD.Service.DriveCycle.Types   in '..\..\src\Service\OBD.Service.DriveCycle.Types.pas',
  OBD.Service.DriveCycle.Catalog in '..\..\src\Service\OBD.Service.DriveCycle.Catalog.pas',
  OBD.Service.DriveCycle         in '..\..\src\Service\OBD.Service.DriveCycle.pas';

procedure DumpCatalog;
var
  Mon: TOBDMonitor;
  Cyc: TOBDDriveCycle;
  Step: TOBDDriveCycleStep;
begin
  Writeln('Registered drive cycles');
  Writeln(StringOfChar('-', 60));
  for Mon in TOBDDriveCycleCatalog.RegisteredMonitors do
  begin
    if not TOBDDriveCycleCatalog.TryGetCycle(Mon, Cyc) then Continue;
    Writeln(Format('%-22s  %4d s total', [MonitorName(Mon), Cyc.TotalSec]));
    for Step in Cyc.Steps do
      Writeln(Format('  step %d (%4d s):  %s',
        [Step.Index, Step.DurationSec, Step.Description]));
    Writeln;
  end;
end;

procedure DemoReadinessDecode;
var
  Bytes: TBytes;
  Err:   string;
  Snap:  TArray<TOBDMonitorReadiness>;
  R:     TOBDMonitorReadiness;
begin
  Writeln('Readiness decode demo (synthetic Mode 01 PID 01)');
  Writeln(StringOfChar('-', 60));
  // SI engine; B = $07 (all continuous supported, all
  // complete = bits 4..6 clear); C = $05 (catalyst + EVAP
  // supported); D = $01 (catalyst NOT complete, EVAP IS).
  Bytes := TBytes.Create($00, $07, $05, $01);
  Snap := TOBDDriveCycleAdvisor.DecodePID01(Bytes, Err);
  if Err <> '' then
  begin
    Writeln('decode error: ', Err);
    Exit;
  end;
  for R in Snap do
    if R.Supported then
      Writeln(Format('  %-22s supported=yes  complete=%s',
        [MonitorName(R.Monitor),
         BoolToStr(R.Complete, True)]));
end;

begin
  try
    TOBDDriveCycleCatalog.CatalogDir :=
      TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\catalogs');
    TOBDDriveCycleCatalog.Reload;

    Writeln('Delphi-OBD drive-cycle advisor demo');
    Writeln;
    DumpCatalog;
    DemoReadinessDecode;
    Writeln;
    Writeln('Done.');
  except
    on E: Exception do
      Writeln('error: ', E.ClassName, ': ', E.Message);
  end;
end.
