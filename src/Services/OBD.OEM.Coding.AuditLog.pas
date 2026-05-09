//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Coding.AuditLog.pas
// CONTENTS       : Tamper-evident, append-only audit log for coding writes.
//                : One JSON record per line. Each record carries an
//                : HMAC-SHA256 chained signature: HMAC = HMAC(K, Prev || Body)
//                : where Prev is the previous record's HMAC (zero-bytes for
//                : the first). Verifying the chain detects any insert /
//                : delete / mutation; the tampered position is reported.
//
// Why            : When a workshop bricks a coding session, you need a
//                : forensic trail that can't be quietly edited. Plain
//                : log files don't survive a determined operator; signed
//                : per-record audit chains do.
//
// Key handling   : The HMAC key is supplied at construction. Apps will
//                : typically pull it from TOBDSecureSettings (DPAPI-
//                : encrypted on Windows). Rotating the key starts a new
//                : chain on a fresh file; old chains remain verifiable
//                : with the old key.
//------------------------------------------------------------------------------
unit OBD.OEM.Coding.AuditLog;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  System.DateUtils, System.Hash;

type
  EOBDCodingAuditLog = class(Exception);

  TOBDCodingAuditRecord = record
    Timestamp: TDateTime;
    VIN: string;
    ECU: string;
    Block: string;
    BeforeHex: string;     // hex-encoded current bytes
    AfterHex: string;      // hex-encoded target bytes
    Operator: string;
    Reason: string;
  end;

  TOBDCodingAuditChainResult = record
    TotalRecords: Integer;
    Verified: Boolean;
    FirstTamperLine: Integer;   // 1-based; 0 if Verified
    Reason: string;
  end;

  TOBDCodingAuditLog = class
  private
    FPath: string;
    FKey: TBytes;
    FPrevHmac: TBytes;
    FInitialised: Boolean;
    procedure EnsureInitialised;
    function CanonicalBody(const Rec: TOBDCodingAuditRecord): string;
    function ComputeHmac(const Prev: TBytes; const Body: string): TBytes;
    function HexEncode(const Bytes: TBytes): string;
    function HexDecode(const S: string): TBytes;
    function LoadLastHmac: TBytes;
  public
    constructor Create(const APath: string; const AKey: TBytes);
    destructor Destroy; override;

    /// <summary>Append a record. The HMAC binds it to the previous
    /// record's HMAC, forming a chain.</summary>
    procedure Append(const Rec: TOBDCodingAuditRecord);

    /// <summary>Walk the file from the start; returns success only when
    /// every record's HMAC matches the recomputed value.</summary>
    function Verify: TOBDCodingAuditChainResult;

    property Path: string read FPath;
  end;

implementation

constructor TOBDCodingAuditLog.Create(const APath: string; const AKey: TBytes);
begin
  inherited Create;
  if Length(AKey) = 0 then
    raise EOBDCodingAuditLog.Create('Audit log requires a non-empty HMAC key');
  FPath := APath;
  FKey := Copy(AKey);
end;

destructor TOBDCodingAuditLog.Destroy;
begin
  inherited;
end;

procedure TOBDCodingAuditLog.EnsureInitialised;
begin
  if FInitialised then Exit;
  if TFile.Exists(FPath) then
    FPrevHmac := LoadLastHmac
  else
  begin
    SetLength(FPrevHmac, 32);
    FillChar(FPrevHmac[0], 32, 0);
  end;
  FInitialised := True;
end;

function TOBDCodingAuditLog.HexEncode(const Bytes: TBytes): string;
const
  HexChars: array[0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  I: Integer;
begin
  SetLength(Result, Length(Bytes) * 2);
  for I := 0 to High(Bytes) do
  begin
    Result[I * 2 + 1] := HexChars[Bytes[I] shr 4];
    Result[I * 2 + 2] := HexChars[Bytes[I] and $0F];
  end;
end;

function TOBDCodingAuditLog.HexDecode(const S: string): TBytes;

  function NibbleOf(C: Char): Byte;
  begin
    case UpCase(C) of
      '0'..'9': Result := Ord(C) - Ord('0');
      'A'..'F': Result := Ord(UpCase(C)) - Ord('A') + 10;
    else
      raise EOBDCodingAuditLog.CreateFmt('Bad hex character: %s', [C]);
    end;
  end;

var
  I: Integer;
begin
  if Odd(Length(S)) then
    raise EOBDCodingAuditLog.Create('Hex string has odd length');
  SetLength(Result, Length(S) div 2);
  for I := 0 to High(Result) do
    Result[I] := (NibbleOf(S[I * 2 + 1]) shl 4) or NibbleOf(S[I * 2 + 2]);
end;

function TOBDCodingAuditLog.CanonicalBody(const Rec: TOBDCodingAuditRecord): string;
var
  Json: TJSONObject;
begin
  // Field order is fixed by the source code so the canonicalisation is
  // deterministic across builds. Adding new fields is a breaking change
  // by design; rotate keys / start a new chain when extending.
  Json := TJSONObject.Create;
  try
    Json.AddPair('ts', DateToISO8601(Rec.Timestamp, True));
    Json.AddPair('vin', Rec.VIN);
    Json.AddPair('ecu', Rec.ECU);
    Json.AddPair('block', Rec.Block);
    Json.AddPair('before', Rec.BeforeHex);
    Json.AddPair('after', Rec.AfterHex);
    Json.AddPair('operator', Rec.Operator);
    Json.AddPair('reason', Rec.Reason);
    Result := Json.ToJSON;
  finally
    Json.Free;
  end;
end;

function TOBDCodingAuditLog.ComputeHmac(const Prev: TBytes; const Body: string): TBytes;
var
  Input: TBytes;
  BodyBytes: TBytes;
  Hex: string;
begin
  BodyBytes := TEncoding.UTF8.GetBytes(Body);
  SetLength(Input, Length(Prev) + Length(BodyBytes));
  if Length(Prev) > 0 then
    Move(Prev[0], Input[0], Length(Prev));
  if Length(BodyBytes) > 0 then
    Move(BodyBytes[0], Input[Length(Prev)], Length(BodyBytes));
  Hex := THashSHA2.GetHMAC(TEncoding.UTF8.GetString(Input),
                           TEncoding.UTF8.GetString(FKey),
                           SHA256);
  Result := HexDecode(Hex);
end;

function TOBDCodingAuditLog.LoadLastHmac: TBytes;
var
  Reader: TStreamReader;
  Last, Line: string;
  Json: TJSONObject;
  HmacStr: string;
begin
  SetLength(Result, 32);
  FillChar(Result[0], 32, 0);
  Last := '';
  Reader := TStreamReader.Create(FPath, TEncoding.UTF8);
  try
    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;
      if Trim(Line) <> '' then Last := Line;
    end;
  finally
    Reader.Free;
  end;
  if Last = '' then Exit;
  Json := TJSONObject.ParseJSONValue(Last) as TJSONObject;
  if Json = nil then Exit;
  try
    if Json.TryGetValue<string>('hmac', HmacStr) then
      Result := HexDecode(HmacStr);
  finally
    Json.Free;
  end;
end;

procedure TOBDCodingAuditLog.Append(const Rec: TOBDCodingAuditRecord);
var
  Body, Line: string;
  Hmac: TBytes;
  Json: TJSONObject;
begin
  EnsureInitialised;
  Body := CanonicalBody(Rec);
  Hmac := ComputeHmac(FPrevHmac, Body);
  Json := TJSONObject.Create;
  try
    // Embed the body inline so the file is one canonical document per
    // line. Verify recomputes the body from the embedded fields.
    Json.AddPair('ts', DateToISO8601(Rec.Timestamp, True));
    Json.AddPair('vin', Rec.VIN);
    Json.AddPair('ecu', Rec.ECU);
    Json.AddPair('block', Rec.Block);
    Json.AddPair('before', Rec.BeforeHex);
    Json.AddPair('after', Rec.AfterHex);
    Json.AddPair('operator', Rec.Operator);
    Json.AddPair('reason', Rec.Reason);
    Json.AddPair('hmac', HexEncode(Hmac));
    Line := Json.ToJSON;
  finally
    Json.Free;
  end;
  TFile.AppendAllText(FPath, Line + sLineBreak, TEncoding.UTF8);
  FPrevHmac := Hmac;
end;

function TOBDCodingAuditLog.Verify: TOBDCodingAuditChainResult;
var
  Reader: TStreamReader;
  Line: string;
  LineNum: Integer;
  Json: TJSONObject;
  Rec: TOBDCodingAuditRecord;
  Body: string;
  Stored, Computed: TBytes;
  Prev: TBytes;
  HmacStr: string;
  TS: string;
begin
  Result := Default(TOBDCodingAuditChainResult);
  Result.Verified := True;

  if not TFile.Exists(FPath) then
  begin
    Result.Reason := 'log file does not exist';
    Result.TotalRecords := 0;
    Exit;
  end;

  SetLength(Prev, 32);
  FillChar(Prev[0], 32, 0);
  LineNum := 0;
  Reader := TStreamReader.Create(FPath, TEncoding.UTF8);
  try
    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;
      Inc(LineNum);
      if Trim(Line) = '' then Continue;
      Json := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      if Json = nil then
      begin
        Result.Verified := False;
        Result.FirstTamperLine := LineNum;
        Result.Reason := 'malformed JSON on line ' + IntToStr(LineNum);
        Exit;
      end;
      try
        if not Json.TryGetValue<string>('hmac', HmacStr) then
        begin
          Result.Verified := False;
          Result.FirstTamperLine := LineNum;
          Result.Reason := 'missing hmac on line ' + IntToStr(LineNum);
          Exit;
        end;
        Json.TryGetValue<string>('ts', TS);
        Rec.Timestamp := ISO8601ToDate(TS, True);
        Rec.VIN := Json.GetValue<string>('vin', '');
        Rec.ECU := Json.GetValue<string>('ecu', '');
        Rec.Block := Json.GetValue<string>('block', '');
        Rec.BeforeHex := Json.GetValue<string>('before', '');
        Rec.AfterHex := Json.GetValue<string>('after', '');
        Rec.Operator := Json.GetValue<string>('operator', '');
        Rec.Reason := Json.GetValue<string>('reason', '');
        Body := CanonicalBody(Rec);
        Stored := HexDecode(HmacStr);
        Computed := ComputeHmac(Prev, Body);
        if (Length(Stored) <> Length(Computed)) or
           not CompareMem(@Stored[0], @Computed[0], Length(Stored)) then
        begin
          Result.Verified := False;
          Result.FirstTamperLine := LineNum;
          Result.Reason := 'hmac mismatch on line ' + IntToStr(LineNum);
          Exit;
        end;
        Prev := Computed;
        Inc(Result.TotalRecords);
      finally
        Json.Free;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

end.
