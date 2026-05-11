//------------------------------------------------------------------------------
//  FreezeFrame — freeze-frame snapshot demo
//
//  Demonstrates how to wire TOBDFreezeFrame (Mode 0x02) to capture
//  the snapshot of vehicle state taken when an emission DTC was
//  stored.
//
//  Build & run:
//    dcc32 -B FreezeFrame.dpr
//    FreezeFrame
//------------------------------------------------------------------------------

program FreezeFrame;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Service.FreezeFrame in '..\..\src\Service\OBD.Service.FreezeFrame.pas';

var
  FF: TOBDFreezeFrame;
begin
  Writeln('Delphi-OBD freeze-frame snapshot demo');
  Writeln(StringOfChar('-', 60));

  FF := TOBDFreezeFrame.Create(nil);
  try
    // Without a protocol, reading raises EOBDConfig — the
    // standard safety-gate behaviour verified in
    // Tests.OBD.Service.ComponentLifecycle.
    try
      FF.Read($00, $0C);  // frame 0, PID 0x0C (RPM)
      Writeln('  unexpected: read returned without raising');
    except
      on E: EOBDConfig do
        Writeln('  Read with no Protocol raised EOBDConfig — OK.');
    end;
  finally FF.Free; end;

  Writeln;
  Writeln('  To capture a freeze frame in a real app:');
  Writeln('    1. Assign FreezeFrame.Protocol to a connected TOBDProtocol.');
  Writeln('    2. Wire FreezeFrame.OnValue to a handler.');
  Writeln('    3. Call FreezeFrame.Read(AFrameIndex, APID).');
  Writeln('       The frame index is the FRZF field from the DTC.');
  Writeln;
  Writeln('Done.');
end.
