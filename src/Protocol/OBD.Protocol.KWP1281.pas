//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281
//
//  Full KWP1281 ("KW1281") block-protocol codec.
//
//  KWP1281 is the pre-KWP2000 VW group diagnostic protocol used
//  by ECUs and radios from roughly 1990 through the mid-2000s,
//  spoken at 9600 / 10400 / 12500 baud over the K-line after a
//  5-baud init wake-up at the diagnostic address. The protocol
//  is block-oriented: every block is L+1 bytes where L is the
//  first byte and counts everything that follows including the
//  block-end marker (0x03). Every byte except the final 0x03 is
//  acknowledged by the receiver with the bitwise complement of
//  the byte just transmitted.
//
//  This unit ships the complete codec - transport-agnostic, no
//  I/O, host injects an <see cref="IKWP1281Transport"/> for the
//  K-line. Layer this on top of the host's serial / KKL / ELM
//  K-line transport and you have a working KWP1281 stack.
//
//  What's in here:
//
//    - <see cref="TKWP1281Block"/>           one block (title + data)
//    - <see cref="IKWP1281Transport"/>       host-supplied K-line
//    - <see cref="TKWP1281Codec"/>           the codec itself
//    - Per-byte complement-ACK send + receive
//    - Block framing (SendBlock / ReceiveBlock) with
//      auto-incrementing counter and counter-mismatch detection
//    - Connect (5-baud init + KW1/KW2 + drain initial ECU
//      announcement blocks) and Disconnect (End Output)
//    - ACK pump (SendAckPumpUntil) to walk multi-block replies
//    - Convenience wrappers for every well-known block title:
//      ACK, End Output, Read DTCs, Clear DTCs, Read RAM, Read
//      ROM/EEPROM, Login, Group Reading, Adaptation, Actuator
//      Test, ASCII reply, etc.
//
//  Errors are reported via three exception classes
//  (<see cref="EKWP1281Timeout"/>, <see cref="EKWP1281Mismatch"/>,
//  <see cref="EKWP1281BadFrame"/>) - the host catches whichever
//  it cares about. The codec never throws "wrong title" -
//  unexpected titles are returned to the caller as-is so the
//  host can react (some ECUs interleave ASCII-data blocks before
//  the answer the caller asked for).
//
//  Reference :
//    https://github.com/mnaberez/vwradio
//    Blau electronic VAG-COM KWP1281 documentation
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Block framing +
//                     counter logic verified against the test
//                     vectors in mnaberez/vwradio/docs.
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  /// <summary>Base for every KWP1281 codec error.</summary>
  EKWP1281Error = class(Exception);
  /// <summary>Raised when the K-line goes silent past the
  /// per-byte timeout. The host typically resets the link and
  /// re-runs <see cref="TKWP1281Codec.Connect"/>.</summary>
  EKWP1281Timeout = class(EKWP1281Error);
  /// <summary>Raised when the byte the receiver echoed back as
  /// complement-ACK doesn't match the byte just sent.</summary>
  EKWP1281Mismatch = class(EKWP1281Error);
  /// <summary>Raised when block framing is wrong (bad length,
  /// missing 0x03 end-marker, counter out of sequence).</summary>
  EKWP1281BadFrame = class(EKWP1281Error);

  /// <summary>One KWP1281 block. The on-wire frame is:
  /// <c>L | counter | title | data... | 0x03</c>, where L counts
  /// everything that follows itself including the end byte.
  /// This record stores only the semantically interesting parts
  /// (title + data) - the codec owns the counter.</summary>
  TKWP1281Block = record
    Title: Byte;
    Data:  TBytes;
    /// <summary>Hex pretty-print useful for logs and tests.</summary>
    function AsString: string;
  end;

  /// <summary>Host-supplied K-line transport. KWP1281 is byte-
  /// oriented at ~9600 baud after the 5-baud init wakes the
  /// ECU. The codec drives this interface for every byte it
  /// sends or receives, including the per-byte complement-ACK
  /// exchange.
  /// <para>Implementations: a serial-port wrapper, an ELM327 in
  /// K-line raw-byte mode, a Bluetooth KKL adapter, ...
  /// </para></summary>
  IKWP1281Transport = interface
    ['{C5C7E1D2-7F2B-4E3A-9C2F-1D8B6F7B0C3F}']

    /// <summary>Sends one byte on the K-line. The default
    /// timeout is the codec's per-byte timeout. Implementations
    /// must block until the byte has been clocked out.</summary>
    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);

    /// <summary>Receives one byte from the K-line. Raises
    /// <see cref="EKWP1281Timeout"/> if no byte arrives inside
    /// <c>ATimeoutMs</c> ms.</summary>
    function  ReceiveByte(ATimeoutMs: Integer): Byte;

    /// <summary>Performs the 5-baud init at <c>AAddress</c> on
    /// the K-line and returns the radio's two key-bytes
    /// (KW1, KW2). The codec will complement-ACK KW2 over
    /// SendByte/ReceiveByte after this call returns - the
    /// transport must NOT do that itself.
    /// <para>Some ELM-style adapters expose 5-baud init as a
    /// single AT command; serial / FTDI hosts implement it by
    /// bit-banging TX low for 200 ms (start bit) then 1/0/0/0/0/1
    /// for the address bits at 200 ms each.</para></summary>
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);

    /// <summary>Hangs up the link. Optional - many transports
    /// just toggle DTR / send a break.</summary>
    procedure Hangup;
  end;

  /// <summary>Optional log sink. Wire it to capture the on-wire
  /// trace for offline analysis.</summary>
  TKWP1281LogEvent = procedure(Sender: TObject;
    const ALine: string) of object;

  /// <summary>Full KWP1281 codec. Stateful - one instance per
  /// session.</summary>
  TKWP1281Codec = class
  strict private
    FTransport:        IKWP1281Transport;
    FCounter:          Byte;
    FByteTimeoutMs:    Integer;
    FInitTimeoutMs:    Integer;
    FAckTimeoutMs:     Integer;
    FConnected:        Boolean;
    FKW1:              Byte;
    FKW2:              Byte;
    FInitialBlocks:    TList<TKWP1281Block>;
    FOnLog:            TKWP1281LogEvent;
    FStrictCounter:    Boolean;
    procedure Log(const ALine: string); overload;
    procedure Log(const AFmt: string;
      const AArgs: array of const); overload;
    procedure SendAcked(AByte: Byte);
    function  ReceiveAcked: Byte;
    function  ReceiveBlock: TKWP1281Block;
    procedure SendBlockRaw(const ABlock: TKWP1281Block);
    procedure DrainInitialBlocks;
  public
    constructor Create(ATransport: IKWP1281Transport;
                       AByteTimeoutMs: Integer = 1000);
    destructor  Destroy; override;

    // ---- session management ------------------------------------

    /// <summary>5-baud init at the given diagnostic address
    /// (e.g. 0x56 for VW radios, 0x01 for engine, 0x17 for
    /// instruments), reads KW1/KW2, complement-ACKs KW2, then
    /// drains the radio's initial ASCII-identity blocks until
    /// it ACKs back. After this returns the session is open
    /// and the counter starts at 1.</summary>
    procedure Connect(AAddress: Byte);

    /// <summary>Sends an End-Output block and tears down the
    /// link. Idempotent - safe to call when not connected.</summary>
    procedure Disconnect;

    // ---- low-level block I/O -----------------------------------

    /// <summary>Sends one block and returns the ECU's reply
    /// block. Auto-increments the counter on send AND on
    /// receive. Throws <see cref="EKWP1281BadFrame"/> if the
    /// counter the ECU sends back doesn't match.</summary>
    function Exchange(const ABlock: TKWP1281Block): TKWP1281Block;

    /// <summary>Sends a bare ACK (title 0x09, no data) and
    /// returns the ECU's reply. Used to step through multi-block
    /// answers (the ECU keeps sending blocks; we keep ACKing).</summary>
    function SendAck: TKWP1281Block;

    /// <summary>Sends an ACK / receives blocks until either the
    /// ECU sends an ACK (title 0x09) or a block whose title
    /// matches <c>AStopTitle</c>. Returns the full block list
    /// (excluding the trailing ACK from the ECU). Useful for
    /// blocks that fan out into multiple ASCII / data blocks.</summary>
    function PumpUntil(AStopTitle: Byte): TArray<TKWP1281Block>;

    // ---- standard block conveniences ---------------------------

    /// <summary>End-Output block (title 0x06).</summary>
    function SendEndOutput: TKWP1281Block;

    /// <summary>Read DTCs (title 0x07). Returns the raw reply -
    /// most ECUs answer with a 0xFC block per fault.</summary>
    function ReadDTCs: TArray<TKWP1281Block>;

    /// <summary>Clear DTCs (title 0x05).</summary>
    function ClearDTCs: TKWP1281Block;

    /// <summary>Read RAM. Title 0x01 (per VAG conventions);
    /// data: 1 byte length, 2 bytes big-endian address. Reply
    /// title 0xFD with the requested bytes.</summary>
    function ReadRAM(AAddress: Word; ALength: Byte): TBytes;

    /// <summary>Read ROM / EEPROM. Title 0x03 with the same
    /// payload shape as <see cref="ReadRAM"/>; reply 0xFD.
    /// Radios use this title to expose the SAFE-EEPROM region;
    /// the same title is used by ECUs to expose the bootloader
    /// ROM. <c>ALength</c> max 64 bytes (radios) / 16 bytes
    /// (engine ECUs) - check the variant.</summary>
    function ReadROM(AAddress: Word; ALength: Byte): TBytes;

    /// <summary>Read EEPROM with explicit title (some VW
    /// radios use 0x19 instead of 0x03 for EEPROM access).
    /// Picks the right title for the firmware in front of you
    /// rather than failing with a "no such block" reply.</summary>
    function ReadEEPROM(AAddress: Word; ALength: Byte;
      ATitle: Byte = $03): TBytes;

    /// <summary>Read Group Reading (measuring blocks). Title
    /// 0x29 with one data byte (group number 1..255). Reply
    /// 0xE7 with a 4 x (type, A, B) triplet payload that the
    /// caller decodes per VAG measuring-value tables.</summary>
    function ReadGroup(AGroup: Byte): TBytes;

    /// <summary>Read Single Value. Title 0x2A; data = 1 byte
    /// channel number. Reply 0xE7. Some ECUs use this in place
    /// of group readings.</summary>
    function ReadSingleValue(AChannel: Byte): TBytes;

    /// <summary>Read Adaptation channel (title 0x21, data: 1
    /// byte channel). Reply 0xE6 with the current value.</summary>
    function ReadAdaptation(AChannel: Byte): TBytes;

    /// <summary>Write Adaptation channel (title 0x10). Data: 1
    /// byte channel + 2 bytes big-endian value. Reply 0xE6.</summary>
    function WriteAdaptation(AChannel: Byte;
      AValue: Word): TBytes;

    /// <summary>Save Adaptation (title 0x10 with channel 0 - the
    /// "store" command). Required after WriteAdaptation on most
    /// VAG ECUs to commit the new value to EEPROM.</summary>
    function SaveAdaptation: TKWP1281Block;

    /// <summary>Login. Title 0x2B; data = workshop / SAFE code,
    /// usually 5 bytes (1 byte workshop ID + 2 bytes import +
    /// 2 bytes equipment, but layout varies). Use this for the
    /// classic 5-byte VAG login.</summary>
    function Login(const ASecret: TBytes): TKWP1281Block;

    /// <summary>Long-form login used by some Audi nav / Phaeton
    /// modules. Same title (0x2B) as <see cref="Login"/>, but
    /// the payload is 7+ bytes - the ECU validates the whole
    /// thing as one opaque secret. Pass the bytes the dealer
    /// tool would send.</summary>
    function LoginLong(const ASecret: TBytes): TKWP1281Block;

    /// <summary>Request Seed for security-access challenge-
    /// response. Title <c>ASeedTitle</c> (default 0x2C - the
    /// most common KWP1281 seed title; some Audi modules use
    /// 0x18). Returns the seed bytes the ECU sends back; the
    /// caller computes the key and hands it to
    /// <see cref="SendKey"/>.</summary>
    function RequestSeed(ASeedTitle: Byte = $2C): TBytes;

    /// <summary>Sends the key computed from a seed. Title
    /// <c>AKeyTitle</c> (default 0x2D; Audi nav uses 0x17).
    /// Returns the ECU's reply - typically an ACK on success
    /// or a fault block on bad-key.</summary>
    function SendKey(const AKey: TBytes;
      AKeyTitle: Byte = $2D): TKWP1281Block;

    /// <summary>End-to-end seed/key challenge: requests a seed,
    /// runs <c>AAlgo</c> against it to compute the key, sends
    /// the key, returns the final reply. Saves the host one
    /// round-trip of state management.</summary>
    function SecurityAccess(
      AAlgo: TFunc<TBytes, TBytes>;
      ASeedTitle: Byte = $2C;
      AKeyTitle:  Byte = $2D): TKWP1281Block;

    /// <summary>Recoding. Title 0x08; data = the new coding
    /// payload (variable length per ECU).</summary>
    function Recode(const ACoding: TBytes): TKWP1281Block;

    /// <summary>Actuator Test. Title 0x04, no data. Reply
    /// 0xF5 advances the test sequencer; the next ACK steps to
    /// the next actuator.</summary>
    function ActuatorTest: TKWP1281Block;

    // ---- introspection -----------------------------------------

    /// <summary>Initial blocks the ECU sent right after the key-
    /// byte exchange (typically ASCII identification: part
    /// number, software version, coding, dealer ID).</summary>
    function InitialBlocks: TArray<TKWP1281Block>;

    property Connected:     Boolean read FConnected;
    property KW1:           Byte    read FKW1;
    property KW2:           Byte    read FKW2;
    property Counter:       Byte    read FCounter;
    /// <summary>When True (default) the codec raises on counter
    /// drift. Some buggy radio firmwares restart the counter
    /// mid-session - set False to swallow drift.</summary>
    property StrictCounter: Boolean
      read FStrictCounter write FStrictCounter;
    property OnLog:         TKWP1281LogEvent
      read FOnLog write FOnLog;
  end;

const
  // ---- block titles (subset of well-known) --------------------
  KWP1281_TITLE_READ_RAM           = $01;
  KWP1281_TITLE_READ_ROM_OR_EEPROM = $03;
  KWP1281_TITLE_ACTUATOR_TEST      = $04;
  KWP1281_TITLE_CLEAR_DTCS         = $05;
  KWP1281_TITLE_END_OUTPUT         = $06;
  KWP1281_TITLE_READ_DTCS          = $07;
  KWP1281_TITLE_RECODING           = $08;
  KWP1281_TITLE_ACK                = $09;
  KWP1281_TITLE_ADAPT_WRITE        = $10;
  KWP1281_TITLE_READ_EEPROM_RADIO  = $19;
  KWP1281_TITLE_ADAPT_READ         = $21;
  KWP1281_TITLE_READ_GROUP         = $29;
  KWP1281_TITLE_READ_SINGLE        = $2A;
  KWP1281_TITLE_LOGIN              = $2B;
  /// <summary>Most common KWP1281 seed-request title. Some
  /// Audi nav modules use 0x18 instead.</summary>
  KWP1281_TITLE_REQUEST_SEED       = $2C;
  /// <summary>Most common KWP1281 send-key title. Audi nav: 0x17.</summary>
  KWP1281_TITLE_SEND_KEY           = $2D;

  // Reply titles
  KWP1281_TITLE_ASCII_DATA         = $F6;
  KWP1281_TITLE_DATA_REPLY         = $FD;
  KWP1281_TITLE_DTC_REPLY          = $FC;
  KWP1281_TITLE_GROUP_REPLY        = $E7;
  KWP1281_TITLE_ADAPT_REPLY        = $E6;
  KWP1281_TITLE_ACTUATOR_REPLY     = $F5;

  KWP1281_BLOCK_END                = $03;

  // ---- diagnostic addresses (subset, VW group) ----------------
  KWP1281_ADDR_ENGINE              = $01;
  KWP1281_ADDR_AUTO_TRANS          = $02;
  KWP1281_ADDR_ABS                 = $03;
  KWP1281_ADDR_INSTRUMENTS         = $17;
  KWP1281_ADDR_AIRBAG              = $15;
  KWP1281_ADDR_RADIO               = $56;
  KWP1281_ADDR_NAV                 = $7C;

implementation

{ TKWP1281Block ---------------------------------------------------------------}

function TKWP1281Block.AsString: string;
var I: Integer;
begin
  Result := Format('title=%.2x len=%d data=', [Title, Length(Data)]);
  for I := 0 to Length(Data) - 1 do
  begin
    if I > 0 then Result := Result + ' ';
    Result := Result + Format('%.2x', [Data[I]]);
  end;
end;

{ TKWP1281Codec ---------------------------------------------------------------}

constructor TKWP1281Codec.Create(ATransport: IKWP1281Transport;
  AByteTimeoutMs: Integer);
begin
  inherited Create;
  FTransport     := ATransport;
  FByteTimeoutMs := AByteTimeoutMs;
  FInitTimeoutMs := 5000;          // 5-baud init can take 2.5 s
  FAckTimeoutMs  := AByteTimeoutMs;
  FCounter       := 0;
  FStrictCounter := True;
  FInitialBlocks := TList<TKWP1281Block>.Create;
end;

destructor TKWP1281Codec.Destroy;
begin
  if FConnected then
    try Disconnect; except end;
  FInitialBlocks.Free;
  FTransport := nil;
  inherited;
end;

procedure TKWP1281Codec.Log(const ALine: string);
begin
  if Assigned(FOnLog) then
    try FOnLog(Self, ALine); except end;
end;

procedure TKWP1281Codec.Log(const AFmt: string;
  const AArgs: array of const);
begin
  if Assigned(FOnLog) then
    Log(Format(AFmt, AArgs));
end;

// ---- per-byte complement-ACK -------------------------------------------------

procedure TKWP1281Codec.SendAcked(AByte: Byte);
var Echo, Expected: Byte;
begin
  FTransport.SendByte(AByte, FByteTimeoutMs);
  Echo := FTransport.ReceiveByte(FAckTimeoutMs);
  Expected := Byte(not AByte);
  if Echo <> Expected then
  begin
    Log('TX ACK mismatch: sent %.2x expected echo %.2x got %.2x',
      [AByte, Expected, Echo]);
    raise EKWP1281Mismatch.CreateFmt(
      'KWP1281: TX complement-ACK mismatch (sent $%.2x, ' +
      'expected echo $%.2x, got $%.2x)',
      [AByte, Expected, Echo]);
  end;
end;

function TKWP1281Codec.ReceiveAcked: Byte;
begin
  Result := FTransport.ReceiveByte(FByteTimeoutMs);
  FTransport.SendByte(Byte(not Result), FAckTimeoutMs);
end;

// ---- block framing -----------------------------------------------------------

procedure TKWP1281Codec.SendBlockRaw(const ABlock: TKWP1281Block);
var
  L: Byte;
  I: Integer;
begin
  // L counts everything after itself: counter (1) + title (1) +
  // data (n) + end (1) = n + 3.
  if Length(ABlock.Data) > 252 then
    raise EKWP1281BadFrame.Create(
      'KWP1281: block data too long (max 252 bytes)');
  L := Byte(Length(ABlock.Data) + 3);

  Inc(FCounter);
  Log('TX block: L=%d cnt=%d %s',
    [L, FCounter, ABlock.AsString]);

  SendAcked(L);
  SendAcked(FCounter);
  SendAcked(ABlock.Title);
  for I := 0 to Length(ABlock.Data) - 1 do
    SendAcked(ABlock.Data[I]);
  // End byte 0x03 is sent without an ACK.
  FTransport.SendByte(KWP1281_BLOCK_END, FByteTimeoutMs);
end;

function TKWP1281Codec.ReceiveBlock: TKWP1281Block;
var
  L, RxCounter, T, EndByte: Byte;
  I, DataLen: Integer;
begin
  L := ReceiveAcked;
  if L < 3 then
    raise EKWP1281BadFrame.CreateFmt(
      'KWP1281: block length byte %d < 3 (must include counter, ' +
      'title, end)', [L]);
  RxCounter := ReceiveAcked;
  Inc(FCounter);
  if FStrictCounter and (RxCounter <> FCounter) then
  begin
    Log('RX counter drift: expected %d got %d', [FCounter, RxCounter]);
    raise EKWP1281BadFrame.CreateFmt(
      'KWP1281: counter drift (expected %d, got %d)',
      [FCounter, RxCounter]);
  end;
  // Even when not strict, slip our counter to whatever the ECU
  // claims so the next outbound block lines up.
  FCounter := RxCounter;

  T := ReceiveAcked;
  Result := Default(TKWP1281Block);
  Result.Title := T;

  DataLen := L - 3;
  SetLength(Result.Data, DataLen);
  for I := 0 to DataLen - 1 do
    Result.Data[I] := ReceiveAcked;

  // End byte is NOT acked.
  EndByte := FTransport.ReceiveByte(FByteTimeoutMs);
  if EndByte <> KWP1281_BLOCK_END then
    raise EKWP1281BadFrame.CreateFmt(
      'KWP1281: expected block-end $03, got $%.2x', [EndByte]);

  Log('RX block: L=%d cnt=%d %s',
    [L, RxCounter, Result.AsString]);
end;

// ---- session management ------------------------------------------------------

procedure TKWP1281Codec.Connect(AAddress: Byte);
var Echo, ExpectedKW2: Byte;
begin
  if FConnected then Exit;

  FCounter := 0;
  FInitialBlocks.Clear;

  Log('5-baud init at addr $%.2x', [AAddress]);
  FTransport.FiveBaudInit(AAddress, FKW1, FKW2, FInitTimeoutMs);
  Log('KW1=$%.2x KW2=$%.2x', [FKW1, FKW2]);

  // Complement-ACK KW2 back to the ECU.
  FTransport.SendByte(Byte(not FKW2), FAckTimeoutMs);
  // Some ECUs / radios echo our ack back; others don't. Read
  // opportunistically with a short timeout and ignore mismatch.
  try
    Echo := FTransport.ReceiveByte(FByteTimeoutMs);
    ExpectedKW2 := FKW2;
    if Echo <> ExpectedKW2 then
      Log('Note: KW2-ack echo mismatch (got $%.2x, expected $%.2x)',
        [Echo, ExpectedKW2]);
  except
    on EKWP1281Timeout do
      Log('No KW2-ack echo (some ECUs skip this).');
  end;

  FConnected := True;
  DrainInitialBlocks;
end;

procedure TKWP1281Codec.DrainInitialBlocks;
var Block: TKWP1281Block; Reply: TKWP1281Block;
begin
  // After init, the ECU sends one or more ASCII-identification
  // blocks, each followed by us ACKing. Loop until the ECU's
  // reply is itself an ACK (title 0x09).
  Block := ReceiveBlock;
  FInitialBlocks.Add(Block);
  while Block.Title <> KWP1281_TITLE_ACK do
  begin
    Reply := SendAck;
    Block := Reply;
    FInitialBlocks.Add(Block);
    // Defensive cap: never sit here forever.
    if FInitialBlocks.Count > 64 then
      raise EKWP1281BadFrame.Create(
        'KWP1281: ECU never sent an ACK to close the initial ' +
        'block sequence (>64 blocks).');
  end;
end;

procedure TKWP1281Codec.Disconnect;
begin
  if not FConnected then Exit;
  try
    SendEndOutput;
  except
    // Best-effort - the ECU may have hung up first.
  end;
  try FTransport.Hangup; except end;
  FConnected := False;
end;

// ---- low-level block I/O -----------------------------------------------------

function TKWP1281Codec.Exchange(
  const ABlock: TKWP1281Block): TKWP1281Block;
begin
  if not FConnected then
    raise EKWP1281Error.Create('KWP1281: not connected');
  SendBlockRaw(ABlock);
  Result := ReceiveBlock;
end;

function TKWP1281Codec.SendAck: TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_ACK;
  Result := Exchange(B);
end;

function TKWP1281Codec.PumpUntil(
  AStopTitle: Byte): TArray<TKWP1281Block>;
var
  Acc: TList<TKWP1281Block>;
  R:   TKWP1281Block;
begin
  Acc := TList<TKWP1281Block>.Create;
  try
    R := SendAck;
    while (R.Title <> KWP1281_TITLE_ACK) and
          (R.Title <> AStopTitle) do
    begin
      Acc.Add(R);
      R := SendAck;
    end;
    if R.Title = AStopTitle then
      Acc.Add(R);
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

// ---- standard block conveniences --------------------------------------------

function TKWP1281Codec.SendEndOutput: TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_END_OUTPUT;
  // End Output may not get a meaningful reply on every ECU.
  try
    Result := Exchange(B);
  except
    on E: EKWP1281Error do
    begin
      Log('End-output: no clean reply (%s)', [E.Message]);
      Result := Default(TKWP1281Block);
    end;
  end;
end;

function TKWP1281Codec.ReadDTCs: TArray<TKWP1281Block>;
var
  B:   TKWP1281Block;
  Acc: TList<TKWP1281Block>;
  R:   TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_READ_DTCS;
  R := Exchange(B);
  Acc := TList<TKWP1281Block>.Create;
  try
    while R.Title = KWP1281_TITLE_DTC_REPLY do
    begin
      Acc.Add(R);
      R := SendAck;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TKWP1281Codec.ClearDTCs: TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_CLEAR_DTCS;
  Result := Exchange(B);
end;

function TKWP1281Codec.ReadRAM(AAddress: Word; ALength: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_READ_RAM;
  B.Data  := TBytes.Create(ALength, Hi(AAddress), Lo(AAddress));
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.ReadROM(AAddress: Word; ALength: Byte): TBytes;
begin
  Result := ReadEEPROM(AAddress, ALength,
                       KWP1281_TITLE_READ_ROM_OR_EEPROM);
end;

function TKWP1281Codec.ReadEEPROM(AAddress: Word; ALength: Byte;
  ATitle: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := ATitle;
  B.Data  := TBytes.Create(ALength, Hi(AAddress), Lo(AAddress));
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.ReadGroup(AGroup: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_READ_GROUP;
  B.Data  := TBytes.Create(AGroup);
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.ReadSingleValue(AChannel: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_READ_SINGLE;
  B.Data  := TBytes.Create(AChannel);
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.ReadAdaptation(AChannel: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_ADAPT_READ;
  B.Data  := TBytes.Create(AChannel);
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.WriteAdaptation(AChannel: Byte;
  AValue: Word): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_ADAPT_WRITE;
  B.Data  := TBytes.Create(AChannel, Hi(AValue), Lo(AValue));
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.SaveAdaptation: TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_ADAPT_WRITE;
  B.Data  := TBytes.Create(0, 0, 0);  // channel 0 = save / commit
  Result := Exchange(B);
end;

function TKWP1281Codec.Login(const ASecret: TBytes): TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_LOGIN;
  B.Data  := Copy(ASecret);
  Result := Exchange(B);
end;

function TKWP1281Codec.LoginLong(const ASecret: TBytes): TKWP1281Block;
var B: TKWP1281Block;
begin
  // Same title as classic Login - ECU treats the payload as
  // an opaque secret; only the length differs (7+ bytes vs 5).
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_LOGIN;
  B.Data  := Copy(ASecret);
  Result := Exchange(B);
end;

function TKWP1281Codec.RequestSeed(ASeedTitle: Byte): TBytes;
var B: TKWP1281Block; R: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := ASeedTitle;
  R := Exchange(B);
  Result := R.Data;
end;

function TKWP1281Codec.SendKey(const AKey: TBytes;
  AKeyTitle: Byte): TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := AKeyTitle;
  B.Data  := Copy(AKey);
  Result := Exchange(B);
end;

function TKWP1281Codec.SecurityAccess(
  AAlgo: TFunc<TBytes, TBytes>;
  ASeedTitle, AKeyTitle: Byte): TKWP1281Block;
var Seed, Key: TBytes;
begin
  if not Assigned(AAlgo) then
    raise EKWP1281Error.Create(
      'KWP1281.SecurityAccess: AAlgo must be assigned ' +
      '(host-supplied seed -> key transform).');
  Seed := RequestSeed(ASeedTitle);
  Key  := AAlgo(Seed);
  Result := SendKey(Key, AKeyTitle);
end;

function TKWP1281Codec.Recode(const ACoding: TBytes): TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_RECODING;
  B.Data  := Copy(ACoding);
  Result := Exchange(B);
end;

function TKWP1281Codec.ActuatorTest: TKWP1281Block;
var B: TKWP1281Block;
begin
  B := Default(TKWP1281Block);
  B.Title := KWP1281_TITLE_ACTUATOR_TEST;
  Result := Exchange(B);
end;

function TKWP1281Codec.InitialBlocks: TArray<TKWP1281Block>;
begin
  Result := FInitialBlocks.ToArray;
end;

end.
