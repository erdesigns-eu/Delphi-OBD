//------------------------------------------------------------------------------
//  OBD.OEM.Captures
//
//  Replay-driven validation: walks a recorded <c>.obdlog</c>
//  conversation, pairs Response / NRC entries with the matching
//  preceding Frame entry, and runs each pair through an OEM
//  extension's catalogue + DID decoder.
//
//  v2's recorder produces typed <see cref="TOBDLogEntry"/>
//  records (Frame / Response / NRC / Error) — pairing is by
//  ServiceID match, not by line-direction guesswork. The
//  validator surfaces every Service 0x22 ReadDataByIdentifier
//  pair so a host can replay a real workshop capture against a
//  catalogue and report which DIDs decoded cleanly, which were
//  uncatalogued, and which fell back to a hex dump.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Captures;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.OEM,
  OBD.Recorder,
  OBD.Replayer;

type
  /// <summary>Raised on misuse (no OEM extension supplied,
  /// malformed file).</summary>
  EOBDCaptureError = class(Exception);

  /// <summary>One paired request / response in a capture.</summary>
  TOBDCapturePair = record
    /// <summary>Recorder elapsed time at which the request
    /// went on the wire (ms relative to capture start).</summary>
    SentAt: Cardinal;
    /// <summary>Request frame raw bytes.</summary>
    RequestBytes: TBytes;
    /// <summary>Response Raw bytes (negative-response wrap
    /// already removed by the protocol decoder when the
    /// recorder captured it).</summary>
    ResponseBytes: TBytes;
    /// <summary>UDS service ID echoed by the request.</summary>
    ServiceID: Byte;
    /// <summary>For a 0x22 ReadDataByIdentifier request, the
    /// DID extracted from bytes 1-2; 0 otherwise.</summary>
    DID: Word;
    /// <summary>Response payload past the SID + DID echo for
    /// a 0x22 reply; the bytes after the positive-response SID
    /// otherwise; the whole response Raw if no echo was
    /// found.</summary>
    PayloadBytes: TBytes;
    /// <summary><c>True</c> when the pair carries an NRC entry
    /// instead of a positive Response.</summary>
    IsNegative: Boolean;
    /// <summary>NRC value when <c>IsNegative</c>.</summary>
    NegativeResponseCode: Byte;
  end;

  /// <summary>Validator result for one pair.</summary>
  TOBDCaptureDecoded = record
    /// <summary>The pair.</summary>
    Pair: TOBDCapturePair;
    /// <summary><c>True</c> when the OEM extension's catalogue
    /// has an entry for the request DID.</summary>
    DidIsCatalogued: Boolean;
    /// <summary>The catalogued name when <c>DidIsCatalogued</c>;
    /// empty otherwise.</summary>
    DidName: string;
    /// <summary>The OEM extension's <c>DecodeDID</c> output.
    /// Always populated for 0x22 pairs (the base implementation
    /// falls back to a hex dump for unknown DIDs).</summary>
    Display: string;
  end;

/// <summary>Walks <c>AEntries</c> and emits one
/// <see cref="TOBDCapturePair"/> per Frame → next-Response /
/// next-NRC pair. Info / Error entries are skipped. Frame
/// entries without a matching response before the next Frame
/// emit a pair with <c>ResponseBytes</c> empty.</summary>
function ExtractCapturePairs(
  const AEntries: TArray<TOBDLogEntry>): TArray<TOBDCapturePair>;

/// <summary>Runs every <c>22 hi lo</c> pair through
/// <c>AExtension.DecodeDID</c> and reports the
/// results.</summary>
/// <exception cref="EOBDCaptureError"><c>AExtension</c> is
/// nil.</exception>
function ValidateAgainstExtension(
  const APairs: TArray<TOBDCapturePair>;
  const AExtension: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;

/// <summary>Convenience: load <c>.obdlog</c>, extract pairs,
/// validate against <c>AExtension</c>.</summary>
function ValidateCaptureFile(const AFileName: string;
  const AExtension: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;

implementation

function BuildPair(const AFrame: TOBDLogEntry;
  const AResponse: TOBDLogEntry; AResponseValid: Boolean):
  TOBDCapturePair;
var
  ReqBytes, RespBytes: TBytes;
begin
  Result := Default(TOBDCapturePair);
  Result.SentAt := AFrame.ElapsedMs;
  Result.RequestBytes := AFrame.Raw;
  ReqBytes := AFrame.Raw;
  if Length(ReqBytes) = 0 then
    Exit;
  Result.ServiceID := ReqBytes[0];
  if (ReqBytes[0] = $22) and (Length(ReqBytes) >= 3) then
    Result.DID := (Word(ReqBytes[1]) shl 8) or ReqBytes[2];

  if not AResponseValid then
    Exit;

  Result.ResponseBytes := AResponse.Raw;
  if AResponse.Kind = leNRC then
  begin
    Result.IsNegative := True;
    Result.NegativeResponseCode := AResponse.NRC;
    Exit;
  end;

  RespBytes := AResponse.Raw;
  if Length(RespBytes) = 0 then
    Exit;
  // Strip the SID echo on positive replies.
  if RespBytes[0] = ReqBytes[0] + $40 then
  begin
    if (ReqBytes[0] = $22) and (Length(RespBytes) >= 3) and
       (Length(ReqBytes) >= 3) and
       (RespBytes[1] = ReqBytes[1]) and
       (RespBytes[2] = ReqBytes[2]) then
      Result.PayloadBytes :=
        Copy(RespBytes, 3, Length(RespBytes) - 3)
    else
      Result.PayloadBytes :=
        Copy(RespBytes, 1, Length(RespBytes) - 1);
  end
  else
    // Out-of-spec response — surface verbatim.
    Result.PayloadBytes := RespBytes;
end;

function ExtractCapturePairs(
  const AEntries: TArray<TOBDLogEntry>): TArray<TOBDCapturePair>;
var
  Acc: TList<TOBDCapturePair>;
  I: Integer;
  Pending: TOBDLogEntry;
  HasPending: Boolean;
  Empty: TOBDLogEntry;
begin
  Acc := TList<TOBDCapturePair>.Create;
  Pending := Default(TOBDLogEntry);
  HasPending := False;
  Empty := Default(TOBDLogEntry);
  try
    for I := 0 to High(AEntries) do
    begin
      case AEntries[I].Kind of
        leFrame:
          begin
            if HasPending then
              Acc.Add(BuildPair(Pending, Empty, False));
            Pending := AEntries[I];
            HasPending := True;
          end;
        leResponse, leNRC:
          if HasPending then
          begin
            Acc.Add(BuildPair(Pending, AEntries[I], True));
            HasPending := False;
          end;
      // leInfo / leError do not pair.
      end;
    end;
    if HasPending then
      Acc.Add(BuildPair(Pending, Empty, False));
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function ValidateAgainstExtension(
  const APairs: TArray<TOBDCapturePair>;
  const AExtension: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;
var
  Acc: TList<TOBDCaptureDecoded>;
  Pair: TOBDCapturePair;
  Item: TOBDCaptureDecoded;
  Entry: TOBDOEMDataIdentifier;
begin
  if AExtension = nil then
    raise EOBDCaptureError.Create('OEM extension required');
  Acc := TList<TOBDCaptureDecoded>.Create;
  try
    for Pair in APairs do
    begin
      Item := Default(TOBDCaptureDecoded);
      Item.Pair := Pair;
      if Pair.IsNegative or (Pair.ServiceID <> $22) then
      begin
        Acc.Add(Item);
        Continue;
      end;
      Item.DidIsCatalogued := AExtension.FindDID(Pair.DID, Entry);
      if Item.DidIsCatalogued then
        Item.DidName := Entry.Name;
      Item.Display := AExtension.DecodeDID(
        Pair.DID, Pair.PayloadBytes);
      Acc.Add(Item);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function ValidateCaptureFile(const AFileName: string;
  const AExtension: IOBDOEMExtension): TArray<TOBDCaptureDecoded>;
var
  Entries: TArray<TOBDLogEntry>;
begin
  Entries := TOBDReplayer.LoadAll(AFileName);
  Result := ValidateAgainstExtension(
    ExtractCapturePairs(Entries), AExtension);
end;

end.
