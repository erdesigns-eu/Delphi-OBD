//------------------------------------------------------------------------------
//  MonitorResults — on-board monitor demo
//
//  Demonstrates the two monitor surfaces:
//    - TOBDOnBoardMonitor — Service 0x06 (CAN cars) MID/TID/UASID
//      results.
//    - TOBDOxygenMonitor — Service 0x05 (non-CAN cars) oxygen-
//      sensor monitor results.
//
//  Build & run:
//    dcc32 -B MonitorResults.dpr
//    MonitorResults
//------------------------------------------------------------------------------

program MonitorResults;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Service.OnBoardMonitor in '..\..\src\Service\OBD.Service.OnBoardMonitor.pas',
  OBD.OxygenMonitor          in '..\..\src\Service\OBD.OxygenMonitor.pas';

var
  M: TOBDOnBoardMonitor;
  O: TOBDOxygenMonitor;
begin
  Writeln('Delphi-OBD on-board monitor demo');
  Writeln(StringOfChar('-', 60));

  M := TOBDOnBoardMonitor.Create(nil);
  O := TOBDOxygenMonitor.Create(nil);
  try
    Writeln('  Service 0x06 (CAN cars): TOBDOnBoardMonitor');
    Writeln('    Read(AMID) returns the array of test results.');
    Writeln('    Without a protocol it raises EOBDConfig:');
    try
      M.Read($01);
      Writeln('      unexpected: read returned without raising');
    except
      on E: EOBDConfig do
        Writeln('      EOBDConfig raised — OK');
    end;

    Writeln;
    Writeln('  Service 0x05 (non-CAN cars): TOBDOxygenMonitor');
    Writeln('    ReadAll() walks the supported-TID bitmap and reads each.');
    Writeln('    On a CAN car the bitmap is empty (oxygen monitors live');
    Writeln('    under Mode 0x06 instead) — that is correct, not an error.');
    try
      O.ReadSupportedTIDs;
      Writeln('      unexpected: read returned without raising');
    except
      on E: EOBDConfig do
        Writeln('      EOBDConfig raised — OK');
    end;
  finally
    M.Free;
    O.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
