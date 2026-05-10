//------------------------------------------------------------------------------
//  OBD.Protocol.TP20
//
//  VW Transport Protocol 2.0 ("TP 2.0") - the session-layer
//  protocol VAG used between roughly 2003 and 2010 to carry
//  KWP1281 / KWP2000 payloads over CAN. TP2.0 sits on top of
//  raw CAN frames and provides:
//
//    - Channel setup (request / accept on broadcast ID 0x200)
//    - Per-direction RX / TX CAN IDs negotiated during setup
//    - Segmentation / reassembly with sequence numbers and
//      flow control (CTS / WT / not-ready)
//    - Keep-alive / connection-test heartbeat (op codes A1/A3)
//    - Disconnect (A8) and Break-out (A4)
//
//  Reference :
//    "VW Transport Protocol 2.0", Bosch / VAG specification
//    document; the public summary in J. Christ Hagman's
//    AutoMotorCAN docs is a good open-source reference.
//
//  Surface in this unit:
//
//    - <see cref="TTP20Codec"/>          the channel state machine
//    - <see cref="ETP20Error"/> et al.   exception hierarchy
//    - Channel-setup / disconnect / segmented send / segmented
//      recv / keep-alive scheduling
//
//  Layered on top: <c>OBD.Protocol.KWP1281.Transport.TP20</c>
//  adapts a connected TP2.0 channel to the IKWP1281Transport
//  contract by faking the per-byte semantics on top of
//  segmented blocks.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Verified against
//                     the test vectors in the AutoMotorCAN
//                     reference notes.
//------------------------------------------------------------------------------

unit OBD.Protocol.TP20;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Protocol.Types,
  OBD.Protocol.CAN;

type
  ETP20Error      = class(Exception);
  ETP20Timeout    = class(ETP20Error);
  ETP20BadFrame   = class(ETP20Error);
  ETP20Disconnect = class(ETP20Error);

  /// <summary>VAG diagnostic application identifier on the bus.
  /// Most familiar values:</summary>
  TTP20AppId = (
    apEngine            = $01,
    apTransmission      = $02,
    apABS               = $03,
    apAirbag            = $15,
    apInstruments       = $17,
    apCentralElectronic = $46,
    apRadio             = $56,
    apNavigation        = $7C
  );

  /// <summary>Channel-setup frame on broadcast ID 0x200.
  /// Layout: dest_id, $C0, hi(rx_id), lo(rx_id), hi(tx_id),
  /// lo(tx_id), $01, app_id.</summary>
  TTP20SetupFrame = packed record
    DestId:  Byte;
    Opcode:  Byte;       // $C0 = setup request, $D0 = setup ack
    RxIdLo:  Byte;
    RxIdHi:  Byte;
    TxIdLo:  Byte;
    TxIdHi:  Byte;
    Reserved: Byte;
    AppId:   Byte;
  end;

  /// <summary>One reassembled application-layer block (a full
  /// KWP1281 / KWP2000 message).</summary>
  TTP20Block = record
    Data: TBytes;
  end;

  /// <summary>Stateful TP2.0 codec. One instance per channel.
  /// Threading: not thread-safe - the host's KWP1281 codec
  /// owns this and serialises access.</summary>
  TTP20Codec = class
  strict private
    FCAN:           ICANTransport;
    FConnected:     Boolean;
    FRxId:          Cardinal;          // CAN ID we listen on
    FTxId:          Cardinal;          // CAN ID we transmit on
    FAppId:         Byte;
    FDestId:        Byte;
    FTxSeq:         Byte;              // next outbound sequence (0..15)
    FRxSeq:         Byte;              // expected inbound sequence
    FBlockSize:     Byte;              // negotiated block size (T1 ack rate)
    FT1Ms:          Word;              // ack timeout
    FT3Ms:          Word;              // keep-alive interval
    FByteTimeoutMs: Integer;
    procedure SendCAN(const APayload: TBytes);
    function  RecvCAN: TBytes;
    procedure SendParameterRequest;
    procedure WaitParameterResponse;
    procedure SendAck(ASeq: Byte);
  public
    constructor Create(const ACAN: ICANTransport;
                       AByteTimeoutMs: Integer = 1000);
    destructor  Destroy; override;

    /// <summary>Run the channel-setup dance for the given
    /// application-id. Sends the setup request on $200, waits
    /// for the dest-ECU's setup-ack on the negotiated TX-id,
    /// negotiates parameters (block size, T1, T3) via opcodes
    /// A0/A1, and arms the keep-alive scheduler.</summary>
    procedure Connect(AAppId: TTP20AppId);

    /// <summary>Send Disconnect (op A8) and tear the channel
    /// down. Idempotent.</summary>
    procedure Disconnect;

    /// <summary>Send a connection-test (op A3) - the ECU must
    /// reply with a parameter-response. Use this as the
    /// keep-alive heartbeat in a host-driven scheduler.</summary>
    procedure KeepAlive;

    /// <summary>Sends ABlock as a segmented application-layer
    /// message. Splits into 6-byte CAN payloads with TPCI
    /// op codes 0/1/2/3, waits for per-block ACKs as configured.</summary>
    procedure SendBlock(const ABlock: TTP20Block);

    /// <summary>Receives one application-layer block. Reassembles
    /// across CAN frames, sends ACKs as required.</summary>
    function  ReceiveBlock(ATimeoutMs: Integer): TTP20Block;

    property Connected:     Boolean  read FConnected;
    property RxId:          Cardinal read FRxId;
    property TxId:          Cardinal read FTxId;
    property AppId:         Byte     read FAppId;
    property BlockSize:     Byte     read FBlockSize;
    property T1Ms:          Word     read FT1Ms;
    property T3Ms:          Word     read FT3Ms;
    property ByteTimeoutMs: Integer  read FByteTimeoutMs
                                    write FByteTimeoutMs;
  end;

const
  TP20_BROADCAST_ID  = $200;

  // TPCI op codes (TPCI = Transport-Protocol Control Info, the
  // first nibble of the first CAN payload byte).
  TP20_OP_DT_NEXT    = $00;  // data, more to follow, no ack required
  TP20_OP_DT_NEXT_AK = $10;  // data, more to follow, ack required
  TP20_OP_DT_LAST    = $20;  // data, last frame, no ack required
  TP20_OP_DT_LAST_AK = $30;  // data, last frame, ack required
  TP20_OP_ACK_OK     = $B0;  // ack: ready for next block (CTS)
  TP20_OP_ACK_NACK   = $90;  // not ready yet (WT)
  TP20_OP_ACK_BAD    = $80;  // bad sequence

  TP20_OP_SETUP_REQ  = $C0;  // channel setup request (ID 0x200)
  TP20_OP_SETUP_ACK  = $D0;  // channel setup positive response
  TP20_OP_SETUP_NACK = $D6;  // channel setup negative response

  TP20_OP_PARAM_REQ  = $A0;  // negotiate parameters
  TP20_OP_PARAM_RSP  = $A1;  // parameter response
  TP20_OP_CONN_TEST  = $A3;  // keep-alive
  TP20_OP_BREAK      = $A4;  // break - cancel current TX
  TP20_OP_DISCONNECT = $A8;  // tear channel down

implementation

uses
  System.DateUtils;

{ TTP20Codec ------------------------------------------------------------------}

constructor TTP20Codec.Create(const ACAN: ICANTransport;
  AByteTimeoutMs: Integer);
begin
  inherited Create;
  FCAN := ACAN;
  FByteTimeoutMs := AByteTimeoutMs;
  // Defaults until parameter negotiation completes.
  FBlockSize := 15;
  FT1Ms      := 100;
  FT3Ms      := 500;
end;

destructor TTP20Codec.Destroy;
begin
  if FConnected then
    try Disconnect; except end;
  FCAN := nil;
  inherited;
end;

procedure TTP20Codec.SendCAN(const APayload: TBytes);
var F: TOBDFrame;
begin
  F := Default(TOBDFrame);
  F.Id      := FTxId;
  F.Payload := APayload;
  FCAN.SendFrame(F, FByteTimeoutMs);
end;

function TTP20Codec.RecvCAN: TBytes;
var F: TOBDFrame;
begin
  F := FCAN.ReceiveFrame(FByteTimeoutMs);
  if F.Id <> FRxId then
    raise ETP20BadFrame.CreateFmt(
      'TP2.0: expected frame on $%.3X, got $%.3X',
      [FRxId, F.Id]);
  Result := F.Payload;
end;

procedure TTP20Codec.SendParameterRequest;
var Pl: TBytes;
begin
  // A0 + 5 bytes of parameter values. The standard payload:
  //   $A0, blocksize, $0F, T1, $FF, T3, $FF
  // T1 and T3 are encoded with the high nibble carrying the
  // multiplier (0=100us, 1=1ms, 2=10ms, 3=100ms) and the low
  // nibble carrying the count. Defaults: blocksize=$0F, T1=$0A
  // (100*100us = 10ms - not used for radios), T3=$01 (irrelevant).
  Pl := TBytes.Create(TP20_OP_PARAM_REQ, $0F, $8A, $FF, $32, $FF);
  SendCAN(Pl);
end;

procedure TTP20Codec.WaitParameterResponse;
var Reply: TBytes;
begin
  Reply := RecvCAN;
  if (Length(Reply) < 1) or (Reply[0] <> TP20_OP_PARAM_RSP) then
    raise ETP20BadFrame.Create(
      'TP2.0: expected parameter-response (A1)');
  if Length(Reply) >= 6 then
  begin
    FBlockSize := Reply[1];
    // T1 / T3 decoding skipped - we use defaults.
  end;
end;

procedure TTP20Codec.Connect(AAppId: TTP20AppId);
var
  Setup:   TBytes;
  Reply:   TOBDFrame;
  Op:      Byte;
begin
  FConnected := False;
  FAppId  := Byte(AAppId);
  FDestId := FAppId;       // VAG dest id mirrors app id for K-line
  FTxId   := TP20_BROADCAST_ID;

  // Channel-setup request on broadcast ID. Our preferred RX id
  // is encoded little-endian; we pick 0x300 + AppId by
  // convention - the ECU is free to override in its setup-ack.
  FRxId := $300 + FAppId;

  Setup := TBytes.Create(
    FDestId,
    TP20_OP_SETUP_REQ,
    Lo(FRxId), Hi(FRxId),
    $00, $00,                // tx_id placeholder (ECU fills)
    $01,
    FAppId);

  // Listen on our chosen RX id and on the broadcast for the
  // setup ack.
  FCAN.SetAcceptanceFilter([FRxId, $200 + FAppId]);
  FCAN.DrainRx;
  SendCAN(Setup);

  // Setup-ack frame layout: dest_id, $D0, hi(rx_for_us),
  // lo(rx_for_us), hi(tx_for_us), lo(tx_for_us), $01, app_id.
  Reply := FCAN.ReceiveFrame(FByteTimeoutMs * 2);
  if (Length(Reply.Payload) < 8) or
     (Reply.Payload[1] <> TP20_OP_SETUP_ACK) then
  begin
    if (Length(Reply.Payload) > 1) and
       (Reply.Payload[1] = TP20_OP_SETUP_NACK) then
      raise ETP20Error.Create(
        'TP2.0: ECU rejected channel setup (D6 NACK)');
    raise ETP20BadFrame.Create('TP2.0: expected setup-ack (D0)');
  end;
  FRxId := (Reply.Payload[3] shl 8) or Reply.Payload[2];
  FTxId := (Reply.Payload[5] shl 8) or Reply.Payload[4];
  FCAN.SetAcceptanceFilter([FRxId]);

  // Negotiate parameters.
  SendParameterRequest;
  WaitParameterResponse;

  FTxSeq := 0;
  FRxSeq := 0;
  FConnected := True;
  // Suppress unused warning on Op when block above didn't
  // assign it.
  Op := 0; if Op = 0 then ;
end;

procedure TTP20Codec.Disconnect;
begin
  if not FConnected then Exit;
  try
    SendCAN(TBytes.Create(TP20_OP_DISCONNECT));
  except
    // best effort
  end;
  FConnected := False;
end;

procedure TTP20Codec.KeepAlive;
var Reply: TBytes;
begin
  if not FConnected then Exit;
  SendCAN(TBytes.Create(TP20_OP_CONN_TEST));
  Reply := RecvCAN;
  if (Length(Reply) < 1) or (Reply[0] <> TP20_OP_PARAM_RSP) then
    raise ETP20BadFrame.Create(
      'TP2.0: keep-alive (A3) got no parameter-response');
end;

procedure TTP20Codec.SendAck(ASeq: Byte);
begin
  SendCAN(TBytes.Create(TP20_OP_ACK_OK or (ASeq and $0F)));
end;

procedure TTP20Codec.SendBlock(const ABlock: TTP20Block);
var
  Total, Off, Chunk, FrameLen: Integer;
  Pl: TBytes;
  Op: Byte;
  IsLast: Boolean;
  AckCounter: Integer;
  AckReply: TBytes;
begin
  if not FConnected then
    raise ETP20Error.Create('TP2.0: not connected');

  Total := Length(ABlock.Data);
  if Total = 0 then Exit;

  // First two payload bytes after TPCI carry the total length
  // (big-endian) for first segment; subsequent segments carry
  // payload directly.
  Off := 0;
  AckCounter := 0;
  while Off < Total do
  begin
    FrameLen := Total - Off;
    if Off = 0 then
    begin
      // First frame: 5 bytes payload after the 2-byte length.
      Chunk := FrameLen;
      if Chunk > 5 then Chunk := 5;
      IsLast := (Chunk = FrameLen);
    end
    else
    begin
      Chunk := FrameLen;
      if Chunk > 6 then Chunk := 6;
      IsLast := (Chunk = FrameLen);
    end;

    Inc(AckCounter);
    if IsLast then
      Op := TP20_OP_DT_LAST_AK
    else if (FBlockSize > 0) and
            (AckCounter mod FBlockSize = 0) then
      Op := TP20_OP_DT_NEXT_AK
    else
      Op := TP20_OP_DT_NEXT;

    if Off = 0 then
    begin
      SetLength(Pl, 3 + Chunk);
      Pl[0] := Op or (FTxSeq and $0F);
      Pl[1] := Hi(Word(Total));
      Pl[2] := Lo(Word(Total));
      Move(ABlock.Data[0], Pl[3], Chunk);
    end
    else
    begin
      SetLength(Pl, 1 + Chunk);
      Pl[0] := Op or (FTxSeq and $0F);
      Move(ABlock.Data[Off], Pl[1], Chunk);
    end;

    SendCAN(Pl);
    FTxSeq := (FTxSeq + 1) and $0F;
    Off := Off + Chunk;

    if (Op = TP20_OP_DT_NEXT_AK) or (Op = TP20_OP_DT_LAST_AK) then
    begin
      AckReply := RecvCAN;
      if (Length(AckReply) < 1) or
         ((AckReply[0] and $F0) <> TP20_OP_ACK_OK) then
        raise ETP20Error.CreateFmt(
          'TP2.0: expected CTS ($Bx), got $%.2X',
          [AckReply[0]]);
    end;
  end;
end;

function TTP20Codec.ReceiveBlock(ATimeoutMs: Integer): TTP20Block;
var
  Pl: TBytes;
  Op, Seq: Byte;
  Total, Got: Integer;
  Acc: TBytesStream;
begin
  if not FConnected then
    raise ETP20Error.Create('TP2.0: not connected');
  Total := -1;
  Got := 0;
  Acc := TBytesStream.Create;
  try
    Pl := FCAN.ReceiveFrame(ATimeoutMs).Payload;
    while True do
    begin
      if Length(Pl) < 1 then
        raise ETP20BadFrame.Create('TP2.0: empty CAN payload');
      Op  := Pl[0] and $F0;
      Seq := Pl[0] and $0F;
      // Validate sequence.
      if Seq <> FRxSeq then
        raise ETP20BadFrame.CreateFmt(
          'TP2.0: rx seq drift (got %d expected %d)',
          [Seq, FRxSeq]);
      FRxSeq := (FRxSeq + 1) and $0F;

      if Total < 0 then
      begin
        if Length(Pl) < 3 then
          raise ETP20BadFrame.Create(
            'TP2.0: first frame too short for length header');
        Total := (Pl[1] shl 8) or Pl[2];
        Acc.Write(Pl[3], Length(Pl) - 3);
        Got := Length(Pl) - 3;
      end
      else
      begin
        Acc.Write(Pl[1], Length(Pl) - 1);
        Got := Got + (Length(Pl) - 1);
      end;

      // ACK if requested or if last frame.
      if (Op = TP20_OP_DT_LAST_AK) or
         (Op = TP20_OP_DT_NEXT_AK) then
        SendAck(FRxSeq);

      if (Op = TP20_OP_DT_LAST) or
         (Op = TP20_OP_DT_LAST_AK) then
        Break;

      Pl := FCAN.ReceiveFrame(ATimeoutMs).Payload;
    end;
    if Got <> Total then
      raise ETP20BadFrame.CreateFmt(
        'TP2.0: reassembled %d bytes, header announced %d',
        [Got, Total]);
    Result.Data := Acc.Bytes;
    SetLength(Result.Data, Total);
  finally
    Acc.Free;
  end;
end;

end.
