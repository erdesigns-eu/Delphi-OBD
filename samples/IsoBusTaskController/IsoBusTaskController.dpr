//------------------------------------------------------------------------------
//  IsoBusTaskController
//
//  Demonstrates the TOBDIsoBusTC Task Controller surface from
//  ISO 11783 part 10: DDOP upload, process-data exchange,
//  TASKDATA.XML handling.
//
//  Build & run:
//    dcc32 -B IsoBusTaskController.dpr
//    IsoBusTaskController
//------------------------------------------------------------------------------

program IsoBusTaskController;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ERD.Types                in '..\..\src\Core\ERD.Types.pas',
  ERD.Errors               in '..\..\src\Core\ERD.Errors.pas',
  ERD.Speciality.IsoBus.TC in '..\..\src\Speciality\ERD.Speciality.IsoBus.TC.pas';

begin
  Writeln('Delphi-OBD ISO 11783 Task Controller demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  TOBDIsoBusTC covers the TC-client surface (ISO 11783-10):');
  Writeln('    - DDOP (Device Description Object Pool) upload.');
  Writeln('    - Process-data value get / set / value-period.');
  Writeln('    - TASKDATA.XML import / export.');
  Writeln('    - Task lifecycle (start / stop / pause / resume).');
  Writeln;
  Writeln('  See src/Speciality/ERD.Speciality.IsoBus.TC.pas for the full API.');
  Writeln;
  Writeln('Done.');
end.
