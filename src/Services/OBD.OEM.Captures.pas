//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Captures.pas
// CONTENTS       : Replay-driven validation: walks a recorded
//                  .obdlog conversation, pairs requests with their
//                  responses, and runs each pair through an OEM
//                  extension's catalog + DID decoder.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : The matcher is conservative — it only treats a
//                  request as a "DID read" when it starts with the
//                  ISO 14229 ReadDataByIdentifier SID (0x22). Other
//                  service IDs (0x21 ReadDataByLocalId, 0x10 session
//                  control, 0x31 routine) are surfaced verbatim so
//                  callers can post-process them.
//------------------------------------------------------------------------------
unit OBD.OEM.Captures;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.OEM, OBD.Service.Recorder;

type
  EOBDCaptureError = class(Exception);

  /// <summary>One paired request/response in a capture.</summary>
  TOBDCapturePair = record
    /// <summary>Wall-clock millisecond when the request was sent
    /// (relative to the capture's Start).</summary>
    SentAt: Int64;
    /// <summary>Raw request text exactly as the recorder saw it
    /// (whitespace + ELM327 prompts stripped).</summary>
    RequestText: string;
    /// <summary>Raw response text.</summary>
    ResponseText: string;
    /// <summary>Best-effort UDS service ID inferred from the request
    /// (0 if the request didn't start with a valid hex byte).</summary>
    ServiceID: Byte;
    /// <summary>For a 0x22 ReadDataByIdentifier request, the DID
    /// extracted from bytes 1-2; 0 otherwise.</summary>
    DID: Word;
    /// <summary>Response payload past the SID + DID echo. For a
    /// 0x22 reply, this is the bytes after <c>62 HiDID LoDID</c>;
    /// for everything else it's the bytes after the SID echo.</summary>
    PayloadBytes: TBytes;
    /// <summary>True if the response is a <c>7F &lt;SID&gt; NRC</c>
    /// negative reply.</summary>
    IsNegative: Boolean;
    /// <summary>NRC value when <c>IsNegative</c> is True.</summary>
    NegativeResponseCode: Byte;
  end;

  TOBDCaptureDecoded = record
    Pair: TOBDCapturePair;
    /// <summary>True if the OEM extension catalogues this DID.</summary>
    DidIsCatalogued: Boolean;
    /// <summary>The catalogued name when <c>DidIsCatalogued</c>;
    /// empty otherwise.</summary>
    DidName: string;
    /// <summary>The OEM's <c>DecodeDID</c> output. Always populated
    /// when the request was a 0x22 (the base implementation falls
    /// back to a hex dump for unknown DIDs).</summary>
    Display: string;
  end;

/// <summary>Walk <c>Entries</c> and emit one <c>TOBDCapturePair</c>
/// per Sent → next-Received pair. Info / Error lines are skipped.
/// Sent lines without a matching Received before the next Sent are
/// emitted with an empty <c>ResponseText</c>.</summary>
function ExtractCapturePairs(
  const Entries: TArray<TOBDRecordedEntry>): TArray<TOBDCapturePair>;

/// <summary>Run every <c>22 HiDID LoDID</c> pair through
/// <c>Ext.DecodeDID</c> and report the results.</summary>
function ValidateAgainstExtension(
  const Pairs: TArray<TOBDCapturePair>;
  const Ext: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;

/// <summary>Convenience: load <c>.obdlog</c>, extract pairs, and
/// validate against <c>Ext</c>.</summary>
function ValidateCaptureFile(const FilePath: string;
  const Ext: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;

/// <summary>Strip ELM327-style framing (whitespace, prompts,
/// <c>SEARCHING...</c>, multi-line response prefixes like
/// <c>0:</c> / <c>1:</c>) and return the contiguous hex payload.</summary>
function NormalizeResponseText(const Raw: string): string;

implementation

uses
  System.Character, OBD.OEM.Coding;

function NormalizeResponseText(const Raw: string): string;
var
  Lines: TArray<string>;
  Line, Trimmed: string;
  Buf: TStringBuilder;
  Colon: Integer;
begin
  // ELM327 multi-line responses look like:
  //   0: 62 F1 90 31 32 33 34 35
  //   1: 36 37 38 39 30 31 32 ...
  // We strip the "<digit>:" line prefix and concatenate.
  Buf := TStringBuilder.Create;
  try
    Lines := Raw.Split([#13, #10], TStringSplitOptions.ExcludeEmpty);
    for Line in Lines do
    begin
      Trimmed := Line.Trim;
      if (Trimmed = '') or
         SameText(Trimmed, 'SEARCHING...') or
         SameText(Trimmed, 'OK') or
         SameText(Trimmed, '?') or
         (Trimmed = '>') then Continue;
      // Drop a leading ELM "<digit>:" line marker.
      Colon := Pos(':', Trimmed);
      if (Colon > 0) and (Colon <= 2) and
         (Trimmed[1].IsDigit) then
        Trimmed := Trim(Copy(Trimmed, Colon + 1, MaxInt));
      Buf.Append(Trimmed);
      Buf.Append(' ');
    end;
    Result := Buf.ToString.Trim;
  finally
    Buf.Free;
  end;
end;

function TryHexBytes(const S: string; out Bytes: TBytes): Boolean;
begin
  try
    Bytes := HexStringToBytes(S);
    Result := True;
  except
    Bytes := nil;
    Result := False;
  end;
end;

function BuildPair(const SentEntry, ReceivedEntry: TOBDRecordedEntry): TOBDCapturePair;
var
  ReqBytes, RespBytes: TBytes;
  Normalized: string;
begin
  Result := Default(TOBDCapturePair);
  Result.SentAt := SentEntry.ElapsedMs;
  Result.RequestText := SentEntry.Text.Trim;
  Result.ResponseText := ReceivedEntry.Text;

  if not TryHexBytes(Result.RequestText, ReqBytes) or (Length(ReqBytes) = 0) then
    Exit;
  Result.ServiceID := ReqBytes[0];
  if (ReqBytes[0] = $22) and (Length(ReqBytes) >= 3) then
    Result.DID := (Word(ReqBytes[1]) shl 8) or ReqBytes[2];

  Normalized := NormalizeResponseText(ReceivedEntry.Text);
  if not TryHexBytes(Normalized, RespBytes) or (Length(RespBytes) = 0) then Exit;

  if RespBytes[0] = $7F then
  begin
    Result.IsNegative := True;
    if Length(RespBytes) >= 3 then
      Result.NegativeResponseCode := RespBytes[2];
    Exit;
  end;

  // Strip the SID echo (positive reply = SID + 0x40).
  if (RespBytes[0] = ReqBytes[0] + $40) then
  begin
    if (ReqBytes[0] = $22) and (Length(RespBytes) >= 3) and
       (Length(ReqBytes) >= 3) and
       (RespBytes[1] = ReqBytes[1]) and (RespBytes[2] = ReqBytes[2]) then
      Result.PayloadBytes := Copy(RespBytes, 3, Length(RespBytes) - 3)
    else
      Result.PayloadBytes := Copy(RespBytes, 1, Length(RespBytes) - 1);
  end
  else
    // Out-of-spec response — surface it verbatim so callers can
    // post-process.
    Result.PayloadBytes := RespBytes;
end;

function ExtractCapturePairs(
  const Entries: TArray<TOBDRecordedEntry>): TArray<TOBDCapturePair>;
var
  Pairs: TList<TOBDCapturePair>;
  I: Integer;
  Pending: TOBDRecordedEntry;
  HasPending: Boolean;
  Empty: TOBDRecordedEntry;
begin
  Pairs := TList<TOBDCapturePair>.Create;
  Pending := Default(TOBDRecordedEntry);
  HasPending := False;
  Empty := Default(TOBDRecordedEntry);
  try
    for I := 0 to High(Entries) do
    begin
      case Entries[I].Direction of
        rdSent:
          begin
            // Flush a hanging Sent without response.
            if HasPending then
              Pairs.Add(BuildPair(Pending, Empty));
            Pending := Entries[I];
            HasPending := True;
          end;
        rdReceived:
          begin
            if HasPending then
            begin
              Pairs.Add(BuildPair(Pending, Entries[I]));
              HasPending := False;
            end;
          end;
        // Info / Error lines don't pair.
      end;
    end;
    if HasPending then
      Pairs.Add(BuildPair(Pending, Empty));
    Result := Pairs.ToArray;
  finally
    Pairs.Free;
  end;
end;

function ValidateAgainstExtension(
  const Pairs: TArray<TOBDCapturePair>;
  const Ext: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;
var
  Decoded: TList<TOBDCaptureDecoded>;
  Pair: TOBDCapturePair;
  Item: TOBDCaptureDecoded;
  Entry: TOBDOEMDataIdentifier;
begin
  if Ext = nil then
    raise EOBDCaptureError.Create('OEM extension required');
  Decoded := TList<TOBDCaptureDecoded>.Create;
  try
    for Pair in Pairs do
    begin
      Item := Default(TOBDCaptureDecoded);
      Item.Pair := Pair;
      if Pair.IsNegative or (Pair.ServiceID <> $22) then
      begin
        Decoded.Add(Item);
        Continue;
      end;
      Item.DidIsCatalogued := Ext.FindDID(Pair.DID, Entry);
      if Item.DidIsCatalogued then Item.DidName := Entry.Name;
      Item.Display := Ext.DecodeDID(Pair.DID, Pair.PayloadBytes);
      Decoded.Add(Item);
    end;
    Result := Decoded.ToArray;
  finally
    Decoded.Free;
  end;
end;

function ValidateCaptureFile(const FilePath: string;
  const Ext: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;
var
  Replay: TOBDReplayer;
  Pairs: TArray<TOBDCapturePair>;
begin
  Replay := TOBDReplayer.Create;
  try
    Replay.LoadFromFile(FilePath);
    Pairs := ExtractCapturePairs(Replay.Entries);
  finally
    Replay.Free;
  end;
  Result := ValidateAgainstExtension(Pairs, Ext);
end;

end.
