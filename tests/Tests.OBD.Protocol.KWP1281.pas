//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.KWP1281
//
//  Pinned-vector tests for the KWP1281 codec. A fake transport
//  records every byte sent and replays scripted responses.
//
//  Design note - no auto-ACK: the fake does not synthesize the
//  per-byte complement-ACK echo; tests script the full expected
//  byte stream via ScriptOutbound (queues the radio's
//  complement-ACK for each byte the codec is about to send) and
//  ScriptInbound (queues the radio's reply block bytes). This
//  keeps the fake simple and the test intent obvious.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.KWP1281;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Protocol.KWP1281;

type
  TFakeKLine = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FTx:           TList<Byte>;
    FRx:           TQueue<Byte>;
    FInitAddr:     Byte;
    FInitKW1:      Byte;
    FInitKW2:      Byte;
    FInitCalled:   Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   QueueRx(const ABytes: array of Byte);
    procedure   SetInit(AKW1, AKW2: Byte);
    function    TxBytes: TArray<Byte>;
    property    InitCalled: Boolean read FInitCalled;
    property    InitAddr:   Byte    read FInitAddr;

    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;
  end;

  [TestFixture]
  TKWP1281CodecTests = class
  public
    [Test] procedure ConnectDrainsAsciiBlocksUntilAck;
    [Test] procedure ReadEEPROMSendsLengthAndAddress;
    [Test] procedure CounterDriftRaisesUnderStrictMode;
    [Test] procedure CounterDriftSwallowedUnderRelaxedMode;
    [Test] procedure SecurityAccessRunsSeedThenKey;
    [Test] procedure LoginLongUsesSameTitle;
  end;

implementation

{ TFakeKLine ------------------------------------------------------------------}

constructor TFakeKLine.Create;
begin
  inherited Create;
  FTx := TList<Byte>.Create;
  FRx := TQueue<Byte>.Create;
end;

destructor TFakeKLine.Destroy;
begin
  FTx.Free;
  FRx.Free;
  inherited;
end;

procedure TFakeKLine.QueueRx(const ABytes: array of Byte);
var I: Integer;
begin
  for I := 0 to High(ABytes) do
    FRx.Enqueue(ABytes[I]);
end;

procedure TFakeKLine.SetInit(AKW1, AKW2: Byte);
begin
  FInitKW1 := AKW1;
  FInitKW2 := AKW2;
end;

function TFakeKLine.TxBytes: TArray<Byte>;
begin
  Result := FTx.ToArray;
end;

procedure TFakeKLine.SendByte(AByte: Byte; ATimeoutMs: Integer);
begin
  FTx.Add(AByte);
end;

function TFakeKLine.ReceiveByte(ATimeoutMs: Integer): Byte;
begin
  if FRx.Count = 0 then
    raise EKWP1281Timeout.Create('FakeKLine: RX queue empty');
  Result := FRx.Dequeue;
end;

procedure TFakeKLine.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
begin
  FInitAddr   := AAddress;
  FInitCalled := True;
  AKW1 := FInitKW1;
  AKW2 := FInitKW2;
end;

procedure TFakeKLine.Hangup;
begin
end;

// --- Scripting helpers --------------------------------------------------------

/// Queue the radio's complement-ACKs for one outbound block.
/// Codec sends L, cnt, title, data..., end. Radio echoes the
/// complement of every byte EXCEPT end.
procedure ScriptOutbound(AFake: TFakeKLine;
  ACounter, ATitle: Byte; const AData: array of Byte);
var
  L, I: Integer;
  Acks: TBytes;
begin
  L := Length(AData) + 3;
  SetLength(Acks, 3 + Length(AData));
  Acks[0] := Byte(not Byte(L));
  Acks[1] := Byte(not ACounter);
  Acks[2] := Byte(not ATitle);
  for I := 0 to Length(AData) - 1 do
    Acks[3 + I] := Byte(not AData[I]);
  AFake.QueueRx(Acks);
end;

/// Queue the radio's reply block bytes (codec will ack each
/// one except the end byte; those acks land in TX).
procedure ScriptInbound(AFake: TFakeKLine;
  ACounter, ATitle: Byte; const AData: array of Byte);
var
  Bytes: TBytes;
  I: Integer;
begin
  SetLength(Bytes, 4 + Length(AData));
  Bytes[0] := Byte(Length(AData) + 3);  // L
  Bytes[1] := ACounter;
  Bytes[2] := ATitle;
  for I := 0 to Length(AData) - 1 do
    Bytes[3 + I] := AData[I];
  Bytes[Length(Bytes) - 1] := $03;
  AFake.QueueRx(Bytes);
end;

/// Helper: queue the KW2-ack echo many real radios send back
/// after the codec's NOT(KW2) byte. (Codec is opportunistic
/// here - if absent, codec just times out and continues.)
procedure ScriptKW2Echo(AFake: TFakeKLine; AKW2: Byte);
begin
  AFake.QueueRx([AKW2]);
end;

{ TKWP1281CodecTests ----------------------------------------------------------}

procedure TKWP1281CodecTests.ConnectDrainsAsciiBlocksUntilAck;
var
  Fake: TFakeKLine;
  Codec: TKWP1281Codec;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    // Two ASCII blocks (counter 1, 2) then ACK (counter 3).
    // Between each radio block, codec sends an ACK block back -
    // we must script the radio's complement-acks for that codec
    // outbound. Counter sequence after init: codec sends cnt=2,
    // radio replies cnt=3 (the second radio block); then codec
    // sends cnt=4, radio replies cnt=5 (the final ACK).
    ScriptInbound (Fake, 1, $F6, [Ord('V'), Ord('W')]);
    ScriptOutbound(Fake, 2, $09, []);
    ScriptInbound (Fake, 3, $F6, [Ord('Z')]);
    ScriptOutbound(Fake, 4, $09, []);
    ScriptInbound (Fake, 5, $09, []);

    Codec.Connect($56);
    Assert.IsTrue(Fake.InitCalled);
    Assert.AreEqual(Byte($56), Fake.InitAddr);
    Assert.IsTrue(Codec.Connected);
    Assert.AreEqual(3, Length(Codec.InitialBlocks));
    Assert.AreEqual(Byte($F6), Codec.InitialBlocks[0].Title);
    Assert.AreEqual(Byte($09), Codec.InitialBlocks[2].Title);
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

procedure TKWP1281CodecTests.ReadEEPROMSendsLengthAndAddress;
var
  Fake:  TFakeKLine;
  Codec: TKWP1281Codec;
  Data:  TBytes;
  Tx:    TArray<Byte>;
  I, BlockStart: Integer;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    ScriptInbound(Fake, 1, $09, []);  // immediate ACK from radio
    // Codec's ReadEEPROM sends cnt=2 with title=$03, data=[len, hi, lo].
    ScriptOutbound(Fake, 2, $03, [$04, $00, $83]);
    // Radio reply: cnt=3, title=$FD, 4 EEPROM bytes.
    ScriptInbound(Fake, 3, $FD, [$31, $32, $33, $34]);

    Codec.Connect($56);
    Data := Codec.ReadEEPROM($0083, 4);
    Assert.AreEqual(4, Length(Data));
    Assert.AreEqual(Byte($31), Data[0]);
    Assert.AreEqual(Byte($34), Data[3]);

    // Verify the block we sent: L=$06, cnt=$02, title=$03,
    // data=[$04, $00, $83], end=$03.
    Tx := Fake.TxBytes;
    BlockStart := -1;
    for I := 0 to Length(Tx) - 7 do
      if (Tx[I]     = $06) and (Tx[I + 1] = $02) and
         (Tx[I + 2] = $03) and (Tx[I + 3] = $04) and
         (Tx[I + 4] = $00) and (Tx[I + 5] = $83) and
         (Tx[I + 6] = $03) then
      begin
        BlockStart := I;
        Break;
      end;
    Assert.IsTrue(BlockStart >= 0,
      'ReadEEPROM block not found in TX trace');
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

procedure TKWP1281CodecTests.CounterDriftRaisesUnderStrictMode;
var
  Fake:  TFakeKLine;
  Codec: TKWP1281Codec;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    ScriptInbound (Fake, 1, $09, []);
    ScriptOutbound(Fake, 2, $09, []);
    ScriptInbound (Fake, 9, $09, []);  // wrong counter (should be 3)
    Codec.Connect($56);
    Assert.WillRaise(
      procedure begin Codec.SendAck end,
      EKWP1281BadFrame);
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

procedure TKWP1281CodecTests.CounterDriftSwallowedUnderRelaxedMode;
var
  Fake:  TFakeKLine;
  Codec: TKWP1281Codec;
  R:     TKWP1281Block;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Codec.StrictCounter := False;
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    ScriptInbound (Fake, 1, $09, []);
    ScriptOutbound(Fake, 2, $09, []);
    ScriptInbound (Fake, 9, $09, []);  // wrong counter, swallowed
    Codec.Connect($56);
    R := Codec.SendAck;
    Assert.AreEqual(Byte($09), R.Title);
    Assert.AreEqual(Byte($09), Codec.Counter);
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

procedure TKWP1281CodecTests.SecurityAccessRunsSeedThenKey;
var
  Fake:  TFakeKLine;
  Codec: TKWP1281Codec;
  R:     TKWP1281Block;
  Algo:  TFunc<TBytes, TBytes>;
  CapturedSeed: TBytes;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    ScriptInbound (Fake, 1, $09, []);
    // Seed request: codec sends cnt=2, title=$2C; radio replies
    // cnt=3, title=$2C with seed bytes $AA $BB.
    ScriptOutbound(Fake, 2, $2C, []);
    ScriptInbound (Fake, 3, $2C, [$AA, $BB]);
    // Send key: codec sends cnt=4, title=$2D, data=[$CC, $DD];
    // radio replies cnt=5 ACK.
    ScriptOutbound(Fake, 4, $2D, [$CC, $DD]);
    ScriptInbound (Fake, 5, $09, []);

    Codec.Connect($56);
    Algo := function(Seed: TBytes): TBytes
      begin
        CapturedSeed := Copy(Seed);
        Result := TBytes.Create($CC, $DD);
      end;
    R := Codec.SecurityAccess(Algo);
    Assert.AreEqual(Byte($09), R.Title);
    Assert.AreEqual(2, Length(CapturedSeed));
    Assert.AreEqual(Byte($AA), CapturedSeed[0]);
    Assert.AreEqual(Byte($BB), CapturedSeed[1]);
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

procedure TKWP1281CodecTests.LoginLongUsesSameTitle;
var
  Fake:  TFakeKLine;
  Codec: TKWP1281Codec;
  R:     TKWP1281Block;
  Tx:    TArray<Byte>;
  I, At: Integer;
begin
  Fake  := TFakeKLine.Create;
  Codec := TKWP1281Codec.Create(Fake);
  try
    Fake.SetInit($01, $8A);
    ScriptKW2Echo(Fake, $8A);
    ScriptInbound (Fake, 1, $09, []);
    // 7-byte secret, codec sends cnt=2, title=$2B, data=1..7
    ScriptOutbound(Fake, 2, $2B, [1, 2, 3, 4, 5, 6, 7]);
    ScriptInbound (Fake, 3, $09, []);

    Codec.Connect($56);
    R := Codec.LoginLong(TBytes.Create(1, 2, 3, 4, 5, 6, 7));
    Assert.AreEqual(Byte($09), R.Title);
    Tx := Fake.TxBytes;
    At := -1;
    for I := 0 to Length(Tx) - 4 do
      if (Tx[I] = $0A) and (Tx[I + 1] = $02) and
         (Tx[I + 2] = $2B) then
      begin
        At := I;
        Break;
      end;
    Assert.IsTrue(At >= 0, 'LoginLong block not found in TX');
  finally
    Codec.Free;
    Fake := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TKWP1281CodecTests);

end.
