//------------------------------------------------------------------------------
//  OBD.Recorder.Redactor
//
//  TOBDLogRedactor — streams a `.obdlog` (plain or gzip) through
//  a host-supplied callback and writes a redacted copy. Use when
//  a captured session needs to be shared (bug report, support
//  ticket, fixture in a public repo) but contains data that
//  should not leave the device:
//    - VIN bytes (0x09 PID 02, UDS DID F190).
//    - SecOC keys / freshness counters.
//    - Personally identifying information that ended up in
//      info-class entries (operator name, location).
//
//  Redaction is line-by-line at the JSON-record level, so the
//  output remains a valid `.obdlog` and replays cleanly through
//  TOBDReplayer.
//
//  Two redaction strategies the host can mix and match in one
//  callback:
//    - Drop the whole entry  → set <c>AKeep := False</c>.
//    - Mutate the entry      → edit fields in-place and leave
//                              <c>AKeep := True</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Phase 10 follow-up: shareable captures.
//------------------------------------------------------------------------------

unit OBD.Recorder.Redactor;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Recorder,
  OBD.Replayer;

type
  /// <summary>Host-supplied filter callback. Set <c>AKeep</c> to
  /// False to drop the entry; mutate <c>AEntry</c> to rewrite
  /// it.</summary>
  TOBDRedactFilter = reference to procedure(
    var AEntry: TOBDLogEntry; var AKeep: Boolean);

  /// <summary>Streaming `.obdlog` redactor. Reads
  /// <c>ASource</c> (plain or `.gz`), runs each record through
  /// <c>AFilter</c>, writes the survivors to <c>ADest</c>
  /// (plain or `.gz`).</summary>
  TOBDLogRedactor = class
  public
    /// <summary>Returns the number of records written.</summary>
    class function Redact(const ASource, ADest: string;
      const AFilter: TOBDRedactFilter): Integer; static;

    /// <summary>Convenience filter that wipes the
    /// <c>raw</c> payload bytes of every entry whose
    /// <c>service_id</c> matches one of <c>AServiceIDs</c>.
    /// Useful for stripping VIN reads, key material, etc.
    /// without dropping the call/response pair.</summary>
    class function MakeServiceIDPayloadWiper(
      const AServiceIDs: array of Byte): TOBDRedactFilter; static;
  end;

implementation

class function TOBDLogRedactor.Redact(const ASource, ADest: string;
  const AFilter: TOBDRedactFilter): Integer;
var
  Replayer: TOBDReplayer;
  Recorder: TOBDRecorder;
  Lines: TStringList;
  I: Integer;
  Entry: TOBDLogEntry;
  Keep: Boolean;
begin
  if not Assigned(AFilter) then
    raise EArgumentNilException.Create(
      'TOBDLogRedactor.Redact: AFilter required');
  Result := 0;
  Replayer := TOBDReplayer.Create(nil);
  Recorder := TOBDRecorder.Create(nil);
  try
    Lines := Replayer.LoadLines(ASource);
    try
      Recorder.Open(ADest);
      try
        for I := 0 to Lines.Count - 1 do
        begin
          if not Replayer.ParseLine(Lines[I], Entry) then Continue;
          Keep := True;
          AFilter(Entry, Keep);
          if Keep then
          begin
            Recorder.Append(Entry);
            Inc(Result);
          end;
        end;
      finally
        Recorder.Close;
      end;
    finally
      Lines.Free;
    end;
  finally
    Recorder.Free;
    Replayer.Free;
  end;
end;

class function TOBDLogRedactor.MakeServiceIDPayloadWiper(
  const AServiceIDs: array of Byte): TOBDRedactFilter;
var
  Sids: TBytes;
  I: Integer;
begin
  SetLength(Sids, Length(AServiceIDs));
  for I := 0 to High(AServiceIDs) do
    Sids[I] := AServiceIDs[I];
  Result :=
    procedure(var AEntry: TOBDLogEntry; var AKeep: Boolean)
    var
      J: Integer;
    begin
      AKeep := True;
      if not AEntry.HasServiceID then Exit;
      for J := 0 to High(Sids) do
        if Sids[J] = AEntry.ServiceID then
        begin
          SetLength(AEntry.Raw, 0);
          AEntry.Message := '[redacted]';
          Exit;
        end;
    end;
end;

end.
