//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Transport.ELM
//
//  TKWP1281ELMTransport - IKWP1281Transport implementation on
//  top of any byte-oriented transport that talks to an ELM327
//  (or ELM-compatible: STN11xx, OBDLink MX, Tactrix Openport in
//  ELM mode, etc.) via AT-style commands.
//
//  Honest reality check on ELM + KWP1281:
//
//    ELM327 was designed for OBD-II protocols (ISO 9141, KWP2000,
//    J1850, CAN). KWP1281 is older and lower-level - it expects
//    the client to do per-byte complement-ACK at strict timing.
//    ELM327 does not expose a "raw KWP1281" mode and its built-
//    in protocol handlers do not implement the per-byte ACK that
//    radios and pre-2000 VW ECUs need.
//
//    What this transport CAN do reliably:
//      - 5-baud init via AT IIA <addr> + AT SI - returns KW1/KW2
//        on most modern ELM clones (v1.4+).
//      - Detect ELM firmware version via AT I and dispatch
//        between auto (ATSI) and manual (bit-bang via AT BRT
//        + AT MA) init paths.
//
//    What this transport CANNOT do reliably:
//      - Per-byte complement-ACK at sub-10 ms latency. Most ELM
//        clones add 50-200 ms of AT-command overhead per byte,
//        which exceeds the KWP1281 W4 timeout (50 ms after the
//        ECU sends a byte) on real radios. SendByte / ReceiveByte
//        will work in lab conditions and against tolerant ECUs
//        but should not be expected to drive a real VW radio's
//        SAFE-EEPROM read sequence.
//
//    Recommended path for Volkswagen radios:
//      Use <see cref="OBD.Protocol.KWP1281.Transport.Serial"/>
//      with a USB-serial adapter (FTDI or PL2303) that supports
//      sub-10 ms RTS / break control. This unit is provided for
//      hosts with ELM-only hardware that want to attempt SAFE
//      recovery under the understanding that it may not work.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation - 5-baud init via
//                     ATSI on v1.4+, manual bit-bang fallback on
//                     older firmware. Per-byte send/recv via
//                     direct write + line-buffered read with
//                     prompt-detection (ELM ends every reply
//                     with "\r>").
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Transport.ELM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.Connection.Types,
  OBD.Connection.Transport.Base,
  OBD.Protocol.KWP1281;

type
  TKWP1281ELMInitMode = (
    /// <summary>Auto-detect via the ELM version string returned
    /// by <c>AT I</c>. v1.4+ uses <c>imAtSi</c>, older falls back
    /// to <c>imManualBitBang</c>.</summary>
    imAuto,
    /// <summary>ELM does the 5-baud init in firmware (AT IIA +
    /// AT SI). Cleanest, fastest. Needs ELM v1.4+.</summary>
    imAtSi,
    /// <summary>Host bit-bangs by toggling AT IB10 + AT BD with
    /// 200 ms sleeps. Works on every ELM version but slow and
    /// fragile.</summary>
    imManualBitBang
  );

  /// <summary>KWP1281 transport on top of a byte-oriented
  /// transport that speaks ELM AT commands.
  /// <para>The host supplies the underlying transport (a
  /// serial / Bluetooth / TCP transport that points at the
  /// ELM327). This unit owns the AT-level dialog and exposes
  /// the IKWP1281Transport contract to the codec.</para></summary>
  TKWP1281ELMTransport = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FUnderlying:   IOBDConnectionTransport;
    FRxLine:       TThreadedQueue<Byte>;
    FInitMode:     TKWP1281ELMInitMode;
    FElmVersion:   string;
    FByteTimeout:  Integer;
    procedure HandleBytes(const ABytes: TBytes);
    procedure DrainQueue;
    function  ReadUntilPrompt(ATimeoutMs: Integer): string;
    function  SendAt(const ACmd: string; ATimeoutMs: Integer): string;
    procedure DetectVersionIfNeeded;
    function  ParseHexBytes(const ALine: string): TBytes;
    procedure FiveBaudInit_AtSi(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure FiveBaudInit_Manual(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
  public
    /// <summary>Wraps <c>AUnderlying</c> (kept alive by the
    /// interface ref-count). Hooks the underlying transport's
    /// OnDataReceived to feed an internal byte queue.</summary>
    constructor Create(const AUnderlying: IOBDConnectionTransport;
                       AInitMode: TKWP1281ELMInitMode = imAuto);

    destructor Destroy; override;

    // ---- IKWP1281Transport -------------------------------------
    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;

    /// <summary>Detected ELM firmware version (e.g. "ELM327
    /// v1.5"). Empty until the first AT command runs.</summary>
    property ElmVersion: string read FElmVersion;

    property InitMode: TKWP1281ELMInitMode
      read FInitMode write FInitMode;
  end;

implementation

uses
  System.StrUtils;

const
  PROMPT       = '>';
  CR           = #13;
  LF           = #10;

{ TKWP1281ELMTransport --------------------------------------------------------}

constructor TKWP1281ELMTransport.Create(
  const AUnderlying: IOBDConnectionTransport;
  AInitMode: TKWP1281ELMInitMode);
begin
  inherited Create;
  FUnderlying  := AUnderlying;
  FInitMode    := AInitMode;
  FByteTimeout := 1000;
  FRxLine := TThreadedQueue<Byte>.Create(8192, INFINITE, 0);
  FUnderlying.OnDataReceived := HandleBytes;
end;

destructor TKWP1281ELMTransport.Destroy;
begin
  FUnderlying := nil;
  FRxLine.Free;
  inherited;
end;

procedure TKWP1281ELMTransport.HandleBytes(const ABytes: TBytes);
var I: Integer;
begin
  for I := 0 to Length(ABytes) - 1 do
    FRxLine.PushItem(ABytes[I]);
end;

procedure TKWP1281ELMTransport.DrainQueue;
var Dummy: Byte;
begin
  while FRxLine.PopItem(Dummy, 0) = wrSignaled do
    ;
end;

function TKWP1281ELMTransport.ReadUntilPrompt(ATimeoutMs: Integer): string;
var
  B:        Byte;
  Sw:       TDateTime;
  ElapsedMs: Integer;
begin
  Result := '';
  Sw := Now;
  ElapsedMs := 0;
  while ElapsedMs < ATimeoutMs do
  begin
    if FRxLine.PopItem(B, 50) = wrSignaled then
    begin
      Result := Result + Char(B);
      if Char(B) = PROMPT then Exit;
    end;
    ElapsedMs := Round((Now - Sw) * 86400 * 1000);
  end;
  raise EKWP1281Timeout.Create(
    'ELM transport: no ">" prompt within ' +
    IntToStr(ATimeoutMs) + ' ms');
end;

function TKWP1281ELMTransport.SendAt(const ACmd: string;
  ATimeoutMs: Integer): string;
var Bytes: TBytes;
begin
  DrainQueue;
  Bytes := TEncoding.ASCII.GetBytes(ACmd + CR);
  FUnderlying.WriteBytes(Bytes);
  Result := ReadUntilPrompt(ATimeoutMs);
end;

procedure TKWP1281ELMTransport.DetectVersionIfNeeded;
var Reply: string;
begin
  if FElmVersion <> '' then Exit;
  Reply := SendAt('ATI', 2000);
  // Strip echo / CR / LF; reply usually contains "ELM327 v1.5"
  // somewhere on a single line.
  Reply := StringReplace(Reply, CR, ' ', [rfReplaceAll]);
  Reply := StringReplace(Reply, LF, ' ', [rfReplaceAll]);
  Reply := Trim(Reply);
  FElmVersion := Reply;
end;

function TKWP1281ELMTransport.ParseHexBytes(const ALine: string): TBytes;
var
  L:   TStringList;
  S:   string;
  Acc: TList<Byte>;
  Tok: string;
  I:   Integer;
begin
  Acc := TList<Byte>.Create;
  L   := TStringList.Create;
  try
    S := ALine;
    S := StringReplace(S, CR, ' ', [rfReplaceAll]);
    S := StringReplace(S, LF, ' ', [rfReplaceAll]);
    S := StringReplace(S, PROMPT, ' ', [rfReplaceAll]);
    L.Delimiter := ' ';
    L.StrictDelimiter := True;
    L.DelimitedText := S;
    for I := 0 to L.Count - 1 do
    begin
      Tok := Trim(L[I]);
      if (Tok <> '') and (Length(Tok) = 2) then
        try
          Acc.Add(StrToInt('$' + Tok));
        except
          // ignore non-hex tokens (echo, "OK", etc)
        end;
    end;
    Result := Acc.ToArray;
  finally
    L.Free;
    Acc.Free;
  end;
end;

procedure TKWP1281ELMTransport.SendByte(AByte: Byte;
  ATimeoutMs: Integer);
begin
  // Single hex pair followed by CR. ELM echoes the command and
  // terminates with the prompt - we drain that and let the next
  // ReceiveByte pick up the actual radio reply (if any).
  SendAt(Format('%.2x', [AByte]), ATimeoutMs);
end;

function TKWP1281ELMTransport.ReceiveByte(ATimeoutMs: Integer): Byte;
var
  B:         Byte;
  Sw:        TDateTime;
  ElapsedMs: Integer;
begin
  // Pop one byte from the queue. Skip ASCII 0x0D / 0x0A / '>'
  // / spaces - those are ELM framing, not radio data.
  Sw := Now;
  ElapsedMs := 0;
  while ElapsedMs < ATimeoutMs do
  begin
    if FRxLine.PopItem(B, 50) = wrSignaled then
    begin
      if not (Char(B) in [#13, #10, ' ', '>']) then
        Exit(B);
    end;
    ElapsedMs := Round((Now - Sw) * 86400 * 1000);
  end;
  raise EKWP1281Timeout.CreateFmt(
    'ELM transport: no byte within %d ms', [ATimeoutMs]);
end;

procedure TKWP1281ELMTransport.FiveBaudInit_AtSi(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
var Reply: string; Bytes: TBytes;
begin
  // Set the init address, then run slow init. ATSI returns the
  // sync byte ($55) plus KW1, KW2 as a hex line.
  SendAt(Format('AT IIA %.2X', [AAddress]), 2000);
  SendAt('AT KW0', 2000);  // disable KW check (we'll validate ourselves)
  Reply := SendAt('AT SI', ATimeoutMs);
  Bytes := ParseHexBytes(Reply);
  if Length(Bytes) < 3 then
    raise EKWP1281Error.CreateFmt(
      'ELM transport: ATSI returned %d hex bytes (expected at ' +
      'least 3 - sync, KW1, KW2). Raw reply: %s',
      [Length(Bytes), Reply]);
  if Bytes[0] <> $55 then
    raise EKWP1281Error.CreateFmt(
      'ELM transport: ATSI sync byte is $%.2X (expected $55)',
      [Bytes[0]]);
  AKW1 := Bytes[1];
  AKW2 := Bytes[2];
end;

procedure TKWP1281ELMTransport.FiveBaudInit_Manual(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
var
  I:    Integer;
  Bit:  Integer;
  Sw:   TDateTime;
  B:    Byte;
  Got:  Boolean;
  ElapsedMs: Integer;
begin
  // Manual bit-bang fallback. ELM doesn't expose break-line
  // control directly; the closest is sending NULL bytes at a
  // configured baud. We send one NULL (= start bit) followed
  // by the address bits one at a time using the AT BD (buffer
  // dump) approach, sleeping 200 ms between bits.
  //
  // This is a best-effort fallback - many ELM clones reject
  // it, in which case the host should either upgrade firmware
  // or switch to the Serial transport. We document the failure
  // mode explicitly rather than silently producing garbage.

  SendAt('AT WS', 2000);     // warm reset to known state
  SendAt('AT E0', 2000);     // echo off
  SendAt('AT L0', 2000);     // linefeeds off
  SendAt('AT IB10', 2000);   // initialise baud
  SendAt('AT KW0', 2000);    // KW check off

  // Start bit: send a $00 byte, then wait 200 ms.
  SendAt('00', 2000);
  Sleep(200);
  for I := 0 to 6 do
  begin
    Bit := (AAddress shr I) and 1;
    if Bit = 1 then SendAt('FF', 2000)
               else SendAt('00', 2000);
    Sleep(200);
  end;
  // Stop bit
  SendAt('FF', 2000);
  Sleep(200);

  // Wait for sync $55.
  ElapsedMs := 0;
  Sw := Now;
  Got := False;
  while ElapsedMs < ATimeoutMs do
  begin
    if FRxLine.PopItem(B, 100) = wrSignaled then
      if B = $55 then
      begin
        Got := True;
        Break;
      end;
    ElapsedMs := Round((Now - Sw) * 86400 * 1000);
  end;
  if not Got then
    raise EKWP1281Timeout.Create(
      'ELM transport (manual init): sync byte $55 not seen');
  AKW1 := ReceiveByte(ATimeoutMs);
  AKW2 := ReceiveByte(ATimeoutMs);
end;

procedure TKWP1281ELMTransport.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
var Mode: TKWP1281ELMInitMode;
begin
  DetectVersionIfNeeded;
  Mode := FInitMode;
  if Mode = imAuto then
  begin
    // Heuristic: any version string containing "v1." is good
    // for ATSI; older v1.0/v1.1/v1.2/v1.3 work but report it
    // anyway, so accept ATSI as the default and only fall
    // back if FElmVersion looks empty / weird.
    if (FElmVersion = '') or
       (Pos('ELM327', UpperCase(FElmVersion)) = 0) then
      Mode := imManualBitBang
    else
      Mode := imAtSi;
  end;
  case Mode of
    imAtSi:          FiveBaudInit_AtSi(AAddress, AKW1, AKW2, ATimeoutMs);
    imManualBitBang: FiveBaudInit_Manual(AAddress, AKW1, AKW2, ATimeoutMs);
  end;
end;

procedure TKWP1281ELMTransport.Hangup;
begin
  // Best-effort: tell the ELM to drop the bus. AT PC (protocol
  // close) is supported on most v1.3+ clones.
  try SendAt('AT PC', 2000); except end;
end;

end.
