//------------------------------------------------------------------------------
//  XCPMeasurement — Phase 7 sample
//
//  Demonstrates wiring TOBDXCP for DAQ-list measurement: connect,
//  free / allocate DAQ + ODT structure, point at an address,
//  start streaming. No transport is opened — the sample documents
//  the call sequence.
//
//  Build & run:
//    dcc32 -B XCPMeasurement.dpr
//    XCPMeasurement
//------------------------------------------------------------------------------

program XCPMeasurement;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Calibration.XCP.Transport in '..\..\src\Calibration\OBD.Calibration.XCP.Transport.pas',
  OBD.Calibration.XCP    in '..\..\src\Calibration\OBD.Calibration.XCP.pas';

var
  X: TOBDXCP;
begin
  Writeln('Delphi-OBD XCP measurement (DAQ) demo');
  Writeln(StringOfChar('-', 60));

  X := TOBDXCP.Create(nil);
  try
    Writeln('  Canonical DAQ-list-up sequence:');
    Writeln('    1. X.Transport := MyXCPTransport;     // CAN / Ethernet / etc.');
    Writeln('    2. X.Connect                           — handshake + capabilities');
    Writeln('    3. X.GetSeed + X.Unlock(resource=DAQ)  — security-access');
    Writeln('    4. X.FreeDAQ                           — wipe existing tables');
    Writeln('    5. X.AllocDAQ(N)                       — N DAQ lists');
    Writeln('    6. X.AllocODT(list, K)                 — K ODTs per list');
    Writeln('    7. X.AllocODTEntry(list, odt, M)       — M entries per ODT');
    Writeln('    8. X.SetDAQPtr(list, odt, entry)');
    Writeln('    9. X.WriteDAQ(bit-offset, size, ext, address)');
    Writeln('   10. X.SetDAQListMode(mode, list, eventChannel, prescaler, prio)');
    Writeln('   11. X.StartStopDAQList(START, list)     — begin streaming');
    Writeln;
    Writeln('  Hosts consume the DAQ stream via Transport.OnDaqDto.');
  finally
    X.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
