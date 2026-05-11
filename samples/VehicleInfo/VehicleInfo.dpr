//------------------------------------------------------------------------------
//  VehicleInfo — Phase 5 Service 0x09 demo
//
//  Demonstrates wiring TOBDVIN — Service 0x09 vehicle-info surface
//  (VIN PID 02, CalIDs PID 04, CVNs PID 06, In-Use Performance
//  Tracking PID 08/0B, ECU Name PID 0A, Engine Serial Number PID
//  0D). Also shows the TOBDCalibrationHelper sub-object.
//
//  Build & run:
//    dcc32 -B VehicleInfo.dpr
//    VehicleInfo
//------------------------------------------------------------------------------

program VehicleInfo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types         in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors        in '..\..\src\Core\OBD.Errors.pas',
  OBD.Service.VIN   in '..\..\src\Service\OBD.Service.VIN.pas';

var
  V: TOBDVIN;
begin
  Writeln('Delphi-OBD Service 0x09 vehicle-info demo');
  Writeln(StringOfChar('-', 60));

  V := TOBDVIN.Create(nil);
  try
    Writeln('  TOBDVIN surface:');
    Writeln('    Read              — Mode 0x09 PID 0x02 VIN');
    Writeln('    ReadUDS           — UDS DID 0xF190 VIN');
    Writeln('    Without a Protocol, both raise EOBDConfig:');
    try
      V.Read;
      Writeln('      unexpected: read returned without raising');
    except
      on E: EOBDConfig do
        Writeln('      EOBDConfig — OK');
    end;
  finally V.Free; end;

  Writeln;
  Writeln('  Wire TOBDVIN.Protocol then call Read or ReadAsync to fetch.');
  Writeln('  The result record (TOBDVINResult) carries RawVIN, the');
  Writeln('  ISO 3779 check-digit Valid flag, and the read Source.');
  Writeln;
  Writeln('Done.');
end.
