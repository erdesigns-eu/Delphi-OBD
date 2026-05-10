//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Transport.TP20
//
//  TKWP1281TP20Transport - adapts a TP2.0-connected channel
//  to the IKWP1281Transport contract. Used for VAG cars from
//  ~2003 onward where the K-line is gone and KWP1281 payloads
//  ride inside TP2.0 segments on CAN.
//
//  Important shape difference vs the K-line transports:
//
//    K-line KWP1281 is byte-oriented with per-byte
//    complement-ACK. TP2.0 is block-oriented - the per-byte
//    ACK protocol is not on the wire any more. This transport
//    emulates byte-level semantics by buffering: each
//    TKWP1281Codec.Send/Receive call is satisfied by
//    serialising into / out of an internal block-shaped
//    buffer; the actual bus traffic is one TP2.0 block per
//    KWP1281 block.
//
//    Concretely: SendByte appends to FTxBuf; when the codec
//    sends 0x03 (KWP1281 block-end marker) we flush the buffer
//    as one TP2.0 block. ReceiveByte pops from FRxBuf; when
//    empty, we receive a TP2.0 block and refill. Per-byte
//    complement-ACK echoes are synthesized internally so the
//    codec's framing logic stays unchanged.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Transport.TP20;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Protocol.KWP1281,
  OBD.Protocol.TP20;

type
  TKWP1281TP20Transport = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FTP20:    TTP20Codec;
    FOwnsTP20: Boolean;
    FAppId:   TTP20AppId;
    FTxBuf:   TList<Byte>;
    FRxBuf:   TQueue<Byte>;
    FAckEcho: TQueue<Byte>;
    FInitDone: Boolean;
    FFakeKW1, FFakeKW2: Byte;
    procedure FlushBlockIfComplete(ALastByte: Byte);
    procedure RefillRxFromTP20;
  public
    /// <summary>Wraps an existing TP2.0 codec; caller retains
    /// ownership.</summary>
    constructor Wrap(ATP20: TTP20Codec; AAppId: TTP20AppId);

    destructor  Destroy; override;

    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;
  end;

implementation

const
  KWP1281_BLOCK_END = $03;

{ TKWP1281TP20Transport -------------------------------------------------------}

constructor TKWP1281TP20Transport.Wrap(ATP20: TTP20Codec;
  AAppId: TTP20AppId);
begin
  inherited Create;
  FTP20    := ATP20;
  FAppId   := AAppId;
  FTxBuf   := TList<Byte>.Create;
  FRxBuf   := TQueue<Byte>.Create;
  FAckEcho := TQueue<Byte>.Create;
end;

destructor TKWP1281TP20Transport.Destroy;
begin
  FTxBuf.Free;
  FRxBuf.Free;
  FAckEcho.Free;
  inherited;
end;

procedure TKWP1281TP20Transport.FlushBlockIfComplete(ALastByte: Byte);
var Block: TTP20Block;
begin
  // KWP1281 block ends with 0x03. Drop that end byte before
  // sending - TP2.0 is block-framed, the marker is redundant.
  if ALastByte <> KWP1281_BLOCK_END then Exit;
  if FTxBuf.Count = 0 then Exit;
  SetLength(Block.Data, FTxBuf.Count - 1);  // strip end marker
  if FTxBuf.Count > 1 then
    Move(FTxBuf.List[0], Block.Data[0], FTxBuf.Count - 1);
  FTxBuf.Clear;
  FTP20.SendBlock(Block);
end;

procedure TKWP1281TP20Transport.SendByte(AByte: Byte;
  ATimeoutMs: Integer);
begin
  FTxBuf.Add(AByte);
  if AByte = KWP1281_BLOCK_END then
    FlushBlockIfComplete(AByte);
  // Emulate the radio's complement-ACK echo - the codec is
  // about to ReceiveByte expecting it.
  if AByte <> KWP1281_BLOCK_END then
    FAckEcho.Enqueue(Byte(not AByte));
end;

procedure TKWP1281TP20Transport.RefillRxFromTP20;
var Block: TTP20Block; I: Integer;
begin
  Block := FTP20.ReceiveBlock(1000);
  // Re-emit as KWP1281 framing: bytes followed by 0x03 end.
  for I := 0 to Length(Block.Data) - 1 do
    FRxBuf.Enqueue(Block.Data[I]);
  FRxBuf.Enqueue(KWP1281_BLOCK_END);
end;

function TKWP1281TP20Transport.ReceiveByte(ATimeoutMs: Integer): Byte;
begin
  if FAckEcho.Count > 0 then Exit(FAckEcho.Dequeue);
  if FRxBuf.Count = 0 then
    RefillRxFromTP20;
  if FRxBuf.Count = 0 then
    raise EKWP1281Timeout.Create(
      'KWP1281TP20Transport: nothing buffered after refill');
  Result := FRxBuf.Dequeue;
end;

procedure TKWP1281TP20Transport.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
begin
  // TP2.0 has no 5-baud init - the channel is opened via
  // CAN-level setup. We do that here on first call and then
  // synthesize plausible KW1/KW2 (0x01, 0x8A) so the codec's
  // counter / log paths behave the same as on K-line.
  if not FInitDone then
  begin
    FTP20.Connect(FAppId);
    FInitDone := True;
    FFakeKW1  := $01;
    FFakeKW2  := $8A;
  end;
  AKW1 := FFakeKW1;
  AKW2 := FFakeKW2;
end;

procedure TKWP1281TP20Transport.Hangup;
begin
  if FInitDone then
  begin
    try FTP20.Disconnect; except end;
    FInitDone := False;
  end;
end;

end.
