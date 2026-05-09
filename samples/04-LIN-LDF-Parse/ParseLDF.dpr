//------------------------------------------------------------------------------
//  ParseLDF — sample 04
//
//  Loads a LIN Description File (.ldf) from disk and prints its
//  structural contents: protocol version, bus speed, master / slave
//  nodes, signals, frames (with signal placement) and schedule
//  tables (with microsecond delays).
//
//  Usage:
//    ParseLDF <path-to-ldf>
//
//  Demonstrates the Phase 4f LDF parser. No bus / hardware needed.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4g initial.
//------------------------------------------------------------------------------

program ParseLDF;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.LIN.LDF in '..\..\src\Protocol\OBD.Protocol.LIN.LDF.pas';

procedure PrintCluster(const ACluster: TOBDLDFCluster);
var
  I, J: Integer;
begin
  Writeln('LIN protocol version : ', ACluster.ProtocolVersion);
  Writeln('LIN language version : ', ACluster.LanguageVersion);
  Writeln('Bus speed (bps)      : ', ACluster.Speed);
  Writeln('Master               : ', ACluster.Master);
  Write  ('Slaves               : ');
  for I := 0 to High(ACluster.Slaves) do
  begin
    if I > 0 then Write(', ');
    Write(ACluster.Slaves[I]);
  end;
  Writeln;

  Writeln;
  Writeln('Signals (', Length(ACluster.Signals), '):');
  for I := 0 to High(ACluster.Signals) do
    Writeln(Format('  %-24s  %3d bits  init=%d  pub=%s',
      [ACluster.Signals[I].Name, ACluster.Signals[I].SizeBits,
       ACluster.Signals[I].InitValue, ACluster.Signals[I].Publisher]));

  Writeln;
  Writeln('Frames (', Length(ACluster.Frames), '):');
  for I := 0 to High(ACluster.Frames) do
  begin
    Writeln(Format('  %-24s  ID=0x%2.2X  size=%d bytes  pub=%s',
      [ACluster.Frames[I].Name, ACluster.Frames[I].FrameID,
       ACluster.Frames[I].SizeBytes, ACluster.Frames[I].Publisher]));
    for J := 0 to High(ACluster.Frames[I].Signals) do
      Writeln(Format('      + %s @ bit %d',
        [ACluster.Frames[I].Signals[J].SignalName,
         ACluster.Frames[I].Signals[J].OffsetBits]));
  end;

  Writeln;
  Writeln('Schedule tables (', Length(ACluster.Schedules), '):');
  for I := 0 to High(ACluster.Schedules) do
  begin
    Writeln('  ', ACluster.Schedules[I].Name);
    for J := 0 to High(ACluster.Schedules[I].Entries) do
      Writeln(Format('      %-24s delay %.3f ms',
        [ACluster.Schedules[I].Entries[J].FrameName,
         ACluster.Schedules[I].Entries[J].DelayMicros / 1000.0]));
  end;
end;

var
  Path: string;
  Cluster: TOBDLDFCluster;
begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: ParseLDF <path-to-ldf>');
    Halt(2);
  end;
  Path := ParamStr(1);
  try
    Cluster := TOBDLDFParser.ParseFile(Path);
    PrintCluster(Cluster);
  except
    on E: Exception do
    begin
      Writeln(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
