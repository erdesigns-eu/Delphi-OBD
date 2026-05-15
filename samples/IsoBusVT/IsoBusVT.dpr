//------------------------------------------------------------------------------
//  IsoBusVT
//
//  Demonstrates the TOBDIsoBusVT Virtual Terminal surface from
//  ISO 11783 part 6: object-pool upload, soft-key activation, and
//  input-field updates.
//
//  Build & run:
//    dcc32 -B IsoBusVT.dpr
//    IsoBusVT
//------------------------------------------------------------------------------

program IsoBusVT;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Speciality.IsoBus.VT   in '..\..\src\Speciality\OBD.Speciality.IsoBus.VT.pas';

begin
  Writeln('Delphi-OBD ISO 11783 Virtual Terminal demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  TOBDIsoBusVT covers the VT-client surface (ISO 11783-6):');
  Writeln('    - Object-pool upload (Load Version / Get Memory).');
  Writeln('    - Soft-key registration and activation events.');
  Writeln('    - Mask and input-field updates.');
  Writeln('    - Input-attribute responses.');
  Writeln;
  Writeln('  See src/Speciality/OBD.Speciality.IsoBus.VT.pas for the full API.');
  Writeln;
  Writeln('Done.');
end.
