//------------------------------------------------------------------------------
//  XCPCalibration — Phase 7 sample
//
//  Documents how to drive online parameter editing with TOBDXCP:
//  page selection, short download, full download, and the
//  optional calibration-page get/set surface.
//
//  Build & run:
//    dcc32 -B XCPCalibration.dpr
//    XCPCalibration
//------------------------------------------------------------------------------

program XCPCalibration;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                     in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                    in '..\..\src\Core\OBD.Errors.pas',
  OBD.Calibration.XCP.Transport in '..\..\src\Calibration\OBD.Calibration.XCP.Transport.pas',
  OBD.Calibration.XCP           in '..\..\src\Calibration\OBD.Calibration.XCP.pas';

var
  X: TOBDXCP;
begin
  Writeln('Delphi-OBD XCP calibration demo');
  Writeln(StringOfChar('-', 60));

  X := TOBDXCP.Create(nil);
  try
    Writeln('  Canonical online-calibration sequence:');
    Writeln('    1. X.Connect                          — handshake');
    Writeln('    2. X.GetSeed + X.Unlock(resource=CAL) — security-access for CAL');
    Writeln('    3. X.SetCalPage(segment, page, mode)  — switch active page');
    Writeln('    4. X.SetMTA(address, extension)       — point at the symbol');
    Writeln('    5. X.Download(data)  or               — write the new bytes');
    Writeln('       X.ShortDownload(address, ext, data)');
    Writeln('    6. (optional) X.GetCalPage(segment, mode) to verify');
    Writeln('    7. X.Disconnect');
    Writeln;
    Writeln('  No measurement DAQ is required for calibration — they share');
    Writeln('  the TOBDXCP component but operate on different XCP resources.');
  finally
    X.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
