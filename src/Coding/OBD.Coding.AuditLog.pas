//------------------------------------------------------------------------------
//  OBD.Coding.AuditLog
//
//  TOBDCodingAuditLog — append-only JSONL audit log for coding
//  sessions. Every entry is one JSON object on its own line:
//
//    { "ts":"2026-05-09T12:34:56.789Z",
//      "session":"GUID",
//      "kind":"snapshot|write|verify|rollback|error|info",
//      "did":"0xF190",
//      "before":"BASE64",
//      "after":"BASE64",
//      "message":"...",
//      "hmac":"BASE64" }
//
//  Tamper-evident option: each line carries an HMAC-SHA-256 over
//  the JSON-without-hmac plus the previous line's HMAC. Verifying
//  the chain catches tampering with any historical entry.
//
//  HMAC is built on the AES-128 / CMAC primitives shipped in
//  Phase 4e — same dependency surface, no new crypto.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Coding.AuditLog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.JSON,
  System.NetEncoding,
  System.IOUtils,
  OBD.Types,
  OBD.Protocol.SecOC.AES,
  OBD.Protocol.SecOC.CMAC;

type
  /// <summary>One entry kind.</summary>
  TOBDAuditKind = (
    akInfo,
    akSnapshot,
    akWrite,
    akVerify,
    akRollback,
    akError
  );

  /// <summary>One audit entry pre-serialisation.</summary>
  TOBDAuditEntry = record
    Timestamp: TDateTime;
    SessionID: string;
    Kind: TOBDAuditKind;
    Target: string;            // free-form (DID hex, address, OEM key)
    Before: TBytes;
    After: TBytes;
    Message: string;
  end;

  /// <summary>
  ///   Append-only JSONL audit log. Thread-safe; one file at a time.
  /// </summary>
  TOBDCodingAuditLog = class(TComponent)
  strict private
    FLock: TCriticalSection;
    FFileName: string;
    FStream: TFileStream;
    FSessionID: string;
    FKey: TAES128Key;
    FHasKey: Boolean;
    FPrevHmac: TBytes;
    procedure WriteLine(const ALine: string);
    function ComputeChainHmac(const ASerialisedNoHmac: TBytes): TBytes;
    function KindToText(AKind: TOBDAuditKind): string;
    function TextToKind(const AText: string): TOBDAuditKind;
    function FormatTimestamp(ADt: TDateTime): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Opens (or creates) <c>AFileName</c> for append.
    /// Generates a fresh session GUID. <c>AKey</c> is the optional
    /// HMAC key — pass an all-zero key to disable signing.</summary>
    procedure Open(const AFileName: string; const AKey: TAES128Key);
    /// <summary>Closes the current log file.</summary>
    procedure Close;
    /// <summary>Appends one entry.</summary>
    procedure Append(const AEntry: TOBDAuditEntry);

    /// <summary>Loads every entry in <c>AFileName</c> into a flat
    /// array. Useful for offline analysis. Does not verify the
    /// chain — call <see cref="Verify"/> for that.</summary>
    class function LoadAll(const AFileName: string): TArray<TOBDAuditEntry>; static;

    /// <summary>Verifies the HMAC chain of an existing log file
    /// under <c>AKey</c>. Returns the line index of the first
    /// mismatch; <c>-1</c> when every line passes.</summary>
    class function Verify(const AFileName: string;
      const AKey: TAES128Key): Integer; static;

    /// <summary>Currently-open log file.</summary>
    property FileName: string read FFileName;
    /// <summary>Session GUID. Stable for the lifetime of an
    /// <c>Open</c>/<c>Close</c> cycle.</summary>
    property SessionID: string read FSessionID;
  end;

implementation

constructor TOBDCodingAuditLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
end;

destructor TOBDCodingAuditLog.Destroy;
begin
  Close;
  FLock.Free;
  inherited;
end;

function TOBDCodingAuditLog.KindToText(AKind: TOBDAuditKind): string;
begin
  case AKind of
    akInfo:     Result := 'info';
    akSnapshot: Result := 'snapshot';
    akWrite:    Result := 'write';
    akVerify:   Result := 'verify';
    akRollback: Result := 'rollback';
    akError:    Result := 'error';
  else
    Result := 'info';
  end;
end;

function TOBDCodingAuditLog.TextToKind(const AText: string): TOBDAuditKind;
begin
  if      SameText(AText, 'snapshot') then Result := akSnapshot
  else if SameText(AText, 'write')    then Result := akWrite
  else if SameText(AText, 'verify')   then Result := akVerify
  else if SameText(AText, 'rollback') then Result := akRollback
  else if SameText(AText, 'error')    then Result := akError
  else                                     Result := akInfo;
end;

function TOBDCodingAuditLog.FormatTimestamp(ADt: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"', ADt);
end;

procedure TOBDCodingAuditLog.Open(const AFileName: string;
  const AKey: TAES128Key);
var
  GUID: TGUID;
  KeyHasContent: Boolean;
  I: Integer;
begin
  FLock.Enter;
  try
    Close;
    FFileName := AFileName;
    if TFile.Exists(AFileName) then
      FStream := TFileStream.Create(AFileName,
        fmOpenReadWrite or fmShareDenyWrite)
    else
      FStream := TFileStream.Create(AFileName,
        fmCreate or fmShareDenyWrite);
    FStream.Seek(0, soEnd);
    CreateGUID(GUID);
    FSessionID := GUIDToString(GUID);
    KeyHasContent := False;
    for I := 0 to High(AKey) do
      if AKey[I] <> 0 then begin KeyHasContent := True; Break; end;
    FHasKey := KeyHasContent;
    if FHasKey then FKey := AKey;
    SetLength(FPrevHmac, 0);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDCodingAuditLog.Close;
begin
  FLock.Enter;
  try
    if FStream <> nil then
    begin
      FStream.Free;
      FStream := nil;
    end;
    FFileName := '';
    FSessionID := '';
  finally
    FLock.Leave;
  end;
end;

procedure TOBDCodingAuditLog.WriteLine(const ALine: string);
var
  Bytes: TBytes;
  NL: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(ALine);
  NL := TEncoding.UTF8.GetBytes(#10);
  FStream.WriteBuffer(Bytes[0], Length(Bytes));
  FStream.WriteBuffer(NL[0], 1);
end;

function TOBDCodingAuditLog.ComputeChainHmac(
  const ASerialisedNoHmac: TBytes): TBytes;
var
  Combined: TBytes;
begin
  // Chain: HMAC = CMAC(key, prev_hmac || serialised_without_hmac).
  // For the first entry, prev_hmac is the empty string so the
  // input is just the serialised entry.
  SetLength(Combined, Length(FPrevHmac) + Length(ASerialisedNoHmac));
  if Length(FPrevHmac) > 0 then
    Move(FPrevHmac[0], Combined[0], Length(FPrevHmac));
  if Length(ASerialisedNoHmac) > 0 then
    Move(ASerialisedNoHmac[0], Combined[Length(FPrevHmac)],
      Length(ASerialisedNoHmac));
  Result := TOBDCMACAES.ComputeTruncated(FKey, Combined, 128);
end;

procedure TOBDCodingAuditLog.Append(const AEntry: TOBDAuditEntry);
var
  Obj: TJSONObject;
  Serialised: string;
  HmacBytes, SerialBytes: TBytes;
  Final: string;
  Sid: string;
begin
  FLock.Enter;
  try
    if FStream = nil then
      raise EOBDConfig.Create('TOBDCodingAuditLog: log not open');
    Sid := AEntry.SessionID;
    if Sid = '' then Sid := FSessionID;

    Obj := TJSONObject.Create;
    try
      Obj.AddPair('ts', FormatTimestamp(AEntry.Timestamp));
      Obj.AddPair('session', Sid);
      Obj.AddPair('kind', KindToText(AEntry.Kind));
      if AEntry.Target <> '' then
        Obj.AddPair('target', AEntry.Target);
      if Length(AEntry.Before) > 0 then
        Obj.AddPair('before',
          TNetEncoding.Base64.EncodeBytesToString(AEntry.Before));
      if Length(AEntry.After) > 0 then
        Obj.AddPair('after',
          TNetEncoding.Base64.EncodeBytesToString(AEntry.After));
      if AEntry.Message <> '' then
        Obj.AddPair('message', AEntry.Message);
      Serialised := Obj.ToJSON;
    finally
      Obj.Free;
    end;

    if FHasKey then
    begin
      SerialBytes := TEncoding.UTF8.GetBytes(Serialised);
      HmacBytes := ComputeChainHmac(SerialBytes);
      // Splice the HMAC field into the JSON. We append a comma
      // before the closing brace.
      Final := Copy(Serialised, 1, Length(Serialised) - 1) +
        ',"hmac":"' +
        TNetEncoding.Base64.EncodeBytesToString(HmacBytes) + '"}';
      FPrevHmac := HmacBytes;
    end
    else
      Final := Serialised;

    WriteLine(Final);
  finally
    FLock.Leave;
  end;
end;

class function TOBDCodingAuditLog.LoadAll(
  const AFileName: string): TArray<TOBDAuditEntry>;
var
  Lines: TStringList;
  I: Integer;
  Doc: TJSONValue;
  Obj: TJSONObject;
  Entry: TOBDAuditEntry;
  Acc: TList<TOBDAuditEntry>;
  V: TJSONValue;
  TmpInst: TOBDCodingAuditLog;
begin
  Lines := TStringList.Create;
  Acc := TList<TOBDAuditEntry>.Create;
  TmpInst := TOBDCodingAuditLog.Create(nil);
  try
    Lines.LoadFromFile(AFileName, TEncoding.UTF8);
    for I := 0 to Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = '' then Continue;
      Doc := TJSONObject.ParseJSONValue(Lines[I]);
      if not (Doc is TJSONObject) then begin Doc.Free; Continue; end;
      Obj := Doc as TJSONObject;
      try
        Entry := Default(TOBDAuditEntry);
        V := Obj.GetValue('ts');
        if V <> nil then
          Entry.Timestamp := ISO8601ToDate(V.Value);
        V := Obj.GetValue('session');
        if V <> nil then Entry.SessionID := V.Value;
        V := Obj.GetValue('kind');
        if V <> nil then Entry.Kind := TmpInst.TextToKind(V.Value);
        V := Obj.GetValue('target');
        if V <> nil then Entry.Target := V.Value;
        V := Obj.GetValue('before');
        if V <> nil then
          Entry.Before := TNetEncoding.Base64.DecodeStringToBytes(V.Value);
        V := Obj.GetValue('after');
        if V <> nil then
          Entry.After := TNetEncoding.Base64.DecodeStringToBytes(V.Value);
        V := Obj.GetValue('message');
        if V <> nil then Entry.Message := V.Value;
        Acc.Add(Entry);
      finally
        Doc.Free;
      end;
    end;
    Result := Acc.ToArray;
  finally
    TmpInst.Free;
    Acc.Free;
    Lines.Free;
  end;
end;

class function TOBDCodingAuditLog.Verify(const AFileName: string;
  const AKey: TAES128Key): Integer;
var
  Lines: TStringList;
  I, HmacPos, EndPos: Integer;
  Body, ExpectedB64: string;
  Combined, Calc: TBytes;
  Prev: TBytes;
  ExpectedTag: TBytes;
begin
  Result := -1;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName, TEncoding.UTF8);
    SetLength(Prev, 0);
    for I := 0 to Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = '' then Continue;
      // Locate the chain field. We always emit it as
      // ',"hmac":"<base64>"}' at the very end of each line, so an
      // ordinary substring search suffices.
      HmacPos := Pos(',"hmac":"', Lines[I]);
      if HmacPos = 0 then Exit(I);
      EndPos := Pos('"}', Lines[I], HmacPos + 9);
      if EndPos = 0 then Exit(I);
      ExpectedB64 := Copy(Lines[I], HmacPos + 9, EndPos - HmacPos - 9);
      // Reconstruct the body without the hmac field — append the
      // closing brace back in place of the spliced ,"hmac":".
      Body := Copy(Lines[I], 1, HmacPos - 1) + '}';
      Combined := Concat(Prev, TEncoding.UTF8.GetBytes(Body));
      Calc := TOBDCMACAES.ComputeTruncated(AKey, Combined, 128);
      ExpectedTag := TNetEncoding.Base64.DecodeStringToBytes(ExpectedB64);
      if (Length(ExpectedTag) <> Length(Calc)) then Exit(I);
      // Constant-time compare.
      var Diff: Integer;
      Diff := 0;
      for var J := 0 to High(Calc) do
        Diff := Diff or (Calc[J] xor ExpectedTag[J]);
      if Diff <> 0 then Exit(I);
      Prev := Calc;
    end;
    Result := -1;
  finally
    Lines.Free;
  end;
end;

end.
