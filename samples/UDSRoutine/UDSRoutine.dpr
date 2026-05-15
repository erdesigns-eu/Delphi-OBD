//------------------------------------------------------------------------------
//  UDSRoutine
//
//  PLAN cross-reference: the dedicated UDS routine surface is
//  covered by TOBDRoutineControl (src/Coding/OBD.Coding.RoutineControl.pas).
//  This sample wires the component and demonstrates the safety
//  contract without driving the bus.
//
//  Build & run:
//    dcc32 -B UDSRoutine.dpr
//    UDSRoutine
//------------------------------------------------------------------------------

program UDSRoutine;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Coding.RoutineControl  in '..\..\src\Coding\OBD.Coding.RoutineControl.pas';

var
  R: TOBDRoutineControl;
begin
  Writeln('Delphi-OBD UDS RoutineControl demo');
  Writeln(StringOfChar('-', 60));

  R := TOBDRoutineControl.Create(nil);
  try
    Writeln('  TOBDRoutineControl ships AutoExecute = False by default.');
    Writeln('  Start without consent raises EOBDConfig:');
    try
      R.Start($0202, nil);  // 0x0202 = Erase Memory
    except
      on E: EOBDConfig do
        Writeln('    EOBDConfig — OK');
    end;
  finally
    R.Free;
  end;

  Writeln;
  Writeln('  Real-app wiring:');
  Writeln('    R.Protocol    := MyProtocol;');
  Writeln('    R.AutoExecute := True;     // operator consented');
  Writeln('    R.OnRoutine   := HandleRoutineEvent;');
  Writeln('    R.Start($0202, nil);       // Erase Memory');
  Writeln;
  Writeln('Done.');
end.
