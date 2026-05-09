//------------------------------------------------------------------------------
//  RecordReplay — sample 07
//
//  Records three synthetic events with TOBDRecorder, then replays
//  the file through TOBDReplayer. No bus connection needed.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 10 initial.
//------------------------------------------------------------------------------

program RecordReplay;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  System.TypInfo,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Recorder in '..\..\src\Recorder\OBD.Recorder.pas',
  OBD.Replayer in '..\..\src\Recorder\OBD.Replayer.pas';

procedure RecordSample(const APath: string);
var
  R: TOBDRecorder;
  E: TOBDLogEntry;
begin
  R := TOBDRecorder.Create(nil);
  try
    R.Open(APath);
    Writeln('Opened recorder → ', APath);

    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leInfo;
    E.Message := 'session start';
    R.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leResponse;
    E.ServiceID := $62;
    E.HasServiceID := True;
    E.Raw := TBytes.Create($F1, $90, $31, $48, $47, $43, $4D);
    E.ElapsedMs := 32;
    R.Append(E);

    E := Default(TOBDLogEntry);
    E.Timestamp := Now;
    E.Kind := leNRC;
    E.ServiceID := $22;
    E.HasServiceID := True;
    E.NRC := $33;
    E.HasNRC := True;
    E.NRCText := 'security access denied';
    R.Append(E);

    Writeln('Wrote 3 entries.');
  finally
    R.Free;
  end;
end;

procedure ReplaySample(const APath: string);
var
  Replayer: TOBDReplayer;
begin
  Replayer := TOBDReplayer.Create(nil);
  try
    Replayer.FileName := APath;
    Replayer.Mode := rmAsFastAsPossible;
    Replayer.OnEntry :=
      procedure(Sender: TObject; const E: TOBDLogEntry)
      begin
        Write(Format('  [%s] kind=%-9s',
          [FormatDateTime('hh:nn:ss.zzz', E.Timestamp),
           GetEnumName(TypeInfo(TOBDLogEntryKind), Ord(E.Kind))]));
        if E.HasServiceID then
          Write(Format(' sid=0x%2.2X', [E.ServiceID]));
        if Length(E.Raw) > 0 then
        begin
          Write(' raw=');
          for var I := 0 to High(E.Raw) do
            Write(IntToHex(E.Raw[I], 2));
        end;
        if E.HasNRC then
          Write(Format(' nrc=0x%2.2X (%s)', [E.NRC, E.NRCText]));
        if E.Message <> '' then
          Write(' msg=', E.Message);
        Writeln;
      end;
    Replayer.Play;
  finally
    Replayer.Free;
  end;
end;

var
  Tmp: string;
begin
  Writeln('07-RecordReplay');
  Writeln('================');
  Tmp := TPath.Combine(TPath.GetTempPath, 'sample07.obdlog');
  try
    RecordSample(Tmp);
    Writeln;
    Writeln('Replaying:');
    ReplaySample(Tmp);
    Writeln;
    Writeln('Done. Log file kept at ', Tmp);
  except
    on E: Exception do
    begin
      Writeln(StdErr, E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
