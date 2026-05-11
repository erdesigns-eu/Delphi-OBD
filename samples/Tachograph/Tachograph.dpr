//------------------------------------------------------------------------------
//  Tachograph — Phase 7 sample
//
//  Demonstrates the TOBDTachograph + TOBDTachoPCSC surfaces from
//  the EU 165/2014 digital-tachograph component family. The
//  sample walks the canonical "read VU + driver card" sequence
//  without driving real hardware.
//
//  Build & run:
//    dcc32 -B Tachograph.dpr
//    Tachograph
//------------------------------------------------------------------------------

program Tachograph;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                       in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                      in '..\..\src\Core\OBD.Errors.pas',
  OBD.Speciality.Tachograph       in '..\..\src\Speciality\OBD.Speciality.Tachograph.pas',
  OBD.Speciality.Tachograph.PCSC  in '..\..\src\Speciality\OBD.Speciality.Tachograph.PCSC.pas';

begin
  Writeln('Delphi-OBD Digital Tachograph (EU 165/2014) demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  TOBDTachograph covers the VU diagnostic surface:');
  Writeln('    - Card hierarchy: Workshop / Control / Company / Driver.');
  Writeln('    - Activity, event, fault and calibration records.');
  Writeln('    - VU overview + downloadable-period query.');
  Writeln;
  Writeln('  TOBDTachoPCSC covers card-reader authentication via PC/SC:');
  Writeln('    - Driver / workshop card insertion + ATR validation.');
  Writeln('    - Mutual authentication with the VU (smart-tacho gen-2).');
  Writeln;
  Writeln('  Data-type catalogue: catalogs/tacho/data-types.json');
  Writeln('    Covers ActivityChangeInfo, EventFaultType, CalibrationPurpose,');
  Writeln('    GeoCoordinates, VuOverviewBlock, CardActivityDailyRecord.');
  Writeln;
  Writeln('Done.');
end.
