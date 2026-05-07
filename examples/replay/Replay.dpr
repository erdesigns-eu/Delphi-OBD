program Replay;

//------------------------------------------------------------------------------
// PROGRAM        : Replay
// CONTENTS       : Replay a recorded `.obdlog` file to stdout
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// USAGE          : Replay <path-to-.obdlog> [speed]
//                    speed: 1.0 = real-time (default), 0 = as fast as possible
//------------------------------------------------------------------------------

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Service.Recorder in '..\..\src\Services\OBD.Service.Recorder.pas';

procedure HandleEntry(Sender: TObject; const Entry: TOBDRecordedEntry);
const
  Letter: array[TOBDRecorderDirection] of Char = ('S', 'R', 'I', 'E');
begin
  Writeln(Format('[%6dms] %s %s', [Entry.ElapsedMs,
    Letter[Entry.Direction], Entry.Text]));
end;

var
  Replayer: TOBDReplayer;
  Path: string;
  Speed: Single;
begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: Replay <path-to-.obdlog> [speed]');
    ExitCode := 1;
    Exit;
  end;

  Path := ParamStr(1);
  if ParamCount >= 2 then
    Speed := StrToFloatDef(ParamStr(2), 1.0)
  else
    Speed := 1.0;

  Replayer := TOBDReplayer.Create;
  try
    try
      Replayer.LoadFromFile(Path);
      Replayer.Speed := Speed;
      Replayer.OnEntry := HandleEntry;
      Writeln(Format('Replaying %d entries from %s @ %.2fx',
        [Replayer.Count, Path, Speed]));
      Replayer.Run;
    except
      on E: Exception do
      begin
        Writeln('Error: ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    Replayer.Free;
  end;
end.
