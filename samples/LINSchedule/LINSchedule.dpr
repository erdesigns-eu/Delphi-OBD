//------------------------------------------------------------------------------
//  LINSchedule — Phase 7 sample
//
//  Demonstrates the LIN schedule structures parsed by the Phase
//  4f LDF parser (TOBDLDFCluster). Builds a synthetic schedule
//  entry list in-memory and prints the slot timing.
//
//  Build & run:
//    dcc32 -B LINSchedule.dpr
//    LINSchedule
//------------------------------------------------------------------------------

program LINSchedule;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors               in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.LIN.Frame   in '..\..\src\Protocol\OBD.Protocol.LIN.Frame.pas',
  OBD.Protocol.LIN.LDF     in '..\..\src\Protocol\OBD.Protocol.LIN.LDF.pas';

var
  Cluster: TOBDLDFCluster;
  Entry: TOBDLDFScheduleEntry;
  Sched: TOBDLDFSchedule;
begin
  Writeln('Delphi-OBD LIN schedule demo');
  Writeln(StringOfChar('-', 60));

  Cluster := Default(TOBDLDFCluster);
  Sched := Default(TOBDLDFSchedule);
  Sched.Name := 'Normal';
  SetLength(Sched.Entries, 3);
  for var I := 0 to 2 do
  begin
    Entry := Default(TOBDLDFScheduleEntry);
    Entry.FrameName := Format('Frame_%d', [I]);
    Entry.DelayMicros := 10000;          // 10 ms
    Sched.Entries[I] := Entry;
  end;
  SetLength(Cluster.Schedules, 1);
  Cluster.Schedules[0] := Sched;

  Writeln(Format('  Schedule "%s" with %d entries:',
    [Sched.Name, Length(Sched.Entries)]));
  for var I := 0 to High(Sched.Entries) do
    Writeln(Format('    [%d] %s  +%d us',
      [I, Sched.Entries[I].FrameName,
       Sched.Entries[I].DelayMicros]));
  Writeln;
  Writeln('  Real LDF clusters load via TOBDLDFParser. See sample 04 for a');
  Writeln('  full parser walk-through.');
  Writeln;
  Writeln('Done.');
end.
