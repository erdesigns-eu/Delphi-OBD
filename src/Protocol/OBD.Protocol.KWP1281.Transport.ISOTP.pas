//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Transport.ISOTP
//
//  TKWP1281ISOTPTransport - adapts an ICANTransport + ISO 15765-2
//  reassembler to the IKWP1281Transport contract for cars that
//  gateway K-line traffic to CAN via ISO-TP rather than via
//  TP2.0. Uncommon in production but ships some BMW / Audi
//  retrofit gateways and many test rigs.
//
//  Like the TP2.0 variant: byte-level KWP1281 semantics are
//  emulated on top of block-shaped ISO-TP messages. SendByte
//  buffers until the codec emits 0x03 (block end), then flushes
//  one ISO-TP message; ReceiveByte refills from the next
//  reassembled message.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Transport.ISOTP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Protocol.Types,
  OBD.Protocol.CAN,
  OBD.Protocol.ISO15765,
  OBD.Protocol.KWP1281;

type
  TKWP1281ISOTPTransport = class(TInterfacedObject, IKWP1281Transport)
  strict private
    FCAN:        ICANTransport;
    FTxId:       Cardinal;
    FRxId:       Cardinal;
    FFlowId:     Cardinal;
    FReassembler: TOBDIso15765Reassembler;
    FTxBuf:       TList<Byte>;
    FRxBuf:       TQueue<Byte>;
    FAckEcho:     TQueue<Byte>;
    FFakeKW1, FFakeKW2: Byte;
    FBlockSize:   Byte;
    FStMin:       Byte;
    procedure FlushAsIsoTpMessage(const AData: TBytes);
    procedure RefillFromIsoTp;
    procedure SendFlowControl;
  public
    /// <summary>Wraps an ICAN transport and configures the ISO-TP
    /// IDs.</summary>
    /// <param name="ATxId">CAN ID we transmit on (request ID).</param>
    /// <param name="ARxId">CAN ID we listen on (response ID).</param>
    /// <param name="AFlowId">CAN ID for flow-control frames we
    /// send back when receiving multi-frame messages.</param>
    constructor Create(const ACAN: ICANTransport;
                       ATxId, ARxId, AFlowId: Cardinal);

    destructor Destroy; override;

    procedure SendByte(AByte: Byte; ATimeoutMs: Integer);
    function  ReceiveByte(ATimeoutMs: Integer): Byte;
    procedure FiveBaudInit(AAddress: Byte;
      out AKW1, AKW2: Byte; ATimeoutMs: Integer);
    procedure Hangup;

    /// <summary>ISO-TP block size for our flow-control frames
    /// (0 = no limit). Default 0.</summary>
    property BlockSize: Byte read FBlockSize write FBlockSize;
    /// <summary>ISO-TP STmin (separation time, ms) for our flow-
    /// control frames. Default 0.</summary>
    property STmin: Byte read FStMin write FStMin;
  end;

implementation

const
  KWP1281_BLOCK_END = $03;

{ TKWP1281ISOTPTransport ------------------------------------------------------}

constructor TKWP1281ISOTPTransport.Create(const ACAN: ICANTransport;
  ATxId, ARxId, AFlowId: Cardinal);
begin
  inherited Create;
  FCAN          := ACAN;
  FTxId         := ATxId;
  FRxId         := ARxId;
  FFlowId       := AFlowId;
  FReassembler  := TOBDIso15765Reassembler.Create;
  FTxBuf        := TList<Byte>.Create;
  FRxBuf        := TQueue<Byte>.Create;
  FAckEcho      := TQueue<Byte>.Create;
  FFakeKW1      := $01;
  FFakeKW2      := $8A;
  FCAN.SetAcceptanceFilter([FRxId]);
end;

destructor TKWP1281ISOTPTransport.Destroy;
begin
  FReassembler.Free;
  FTxBuf.Free;
  FRxBuf.Free;
  FAckEcho.Free;
  FCAN := nil;
  inherited;
end;

procedure TKWP1281ISOTPTransport.SendFlowControl;
var F: TOBDFrame;
begin
  F := Default(TOBDFrame);
  F.Id      := FFlowId;
  F.Payload := TOBDIso15765Reassembler.EncodeFlowControlFrame(
                 0, FBlockSize, FStMin);
  FCAN.SendFrame(F, 1000);
end;

procedure TKWP1281ISOTPTransport.FlushAsIsoTpMessage(
  const AData: TBytes);
var
  Frames: TArray<TBytes>;
  I:      Integer;
  F:      TOBDFrame;
  Seq:    Byte;
  Off:    Integer;
  Reply:  TOBDFrame;
  Pl:     TBytes;
  ChunkLen: Integer;
begin
  if Length(AData) <= 7 then
  begin
    F := Default(TOBDFrame);
    F.Id      := FTxId;
    F.Payload := TOBDIso15765Reassembler.EncodeSingleFrame(AData);
    FCAN.SendFrame(F, 1000);
    Exit;
  end;

  // First frame.
  F := Default(TOBDFrame);
  F.Id      := FTxId;
  F.Payload := TOBDIso15765Reassembler.EncodeFirstFrame(
                 Cardinal(Length(AData)), Copy(AData, 0, 6));
  FCAN.SendFrame(F, 1000);

  // Wait for flow-control from the receiver.
  Reply := FCAN.ReceiveFrame(1000);
  if (Length(Reply.Payload) < 1) or
     ((Reply.Payload[0] and $F0) <> $30) then
    raise EKWP1281Error.Create(
      'KWP1281ISOTPTransport: expected flow-control frame');

  // Consecutive frames.
  Off := 6;
  Seq := 1;
  while Off < Length(AData) do
  begin
    ChunkLen := Length(AData) - Off;
    if ChunkLen > 7 then ChunkLen := 7;
    Pl := TOBDIso15765Reassembler.EncodeConsecutiveFrame(
            Seq, Copy(AData, Off, ChunkLen));
    F := Default(TOBDFrame);
    F.Id      := FTxId;
    F.Payload := Pl;
    FCAN.SendFrame(F, 1000);
    Off := Off + ChunkLen;
    Seq := (Seq + 1) and $0F;
  end;

  // Compiler-stub for unused locals when single-frame branch
  // taken (Frames / I).
  Frames := nil; if Length(Frames) = 0 then ;
  I := 0; if I = 0 then ;
end;

procedure TKWP1281ISOTPTransport.RefillFromIsoTp;
var
  F:    TOBDFrame;
  Out_: TBytes;
  I:    Integer;
  GotFlow: Boolean;
begin
  GotFlow := False;
  while True do
  begin
    F := FCAN.ReceiveFrame(1000);
    case TOBDIso15765Reassembler.ClassifyFrame(F.Payload) of
      fkSingle, fkFirst:
        begin
          if FReassembler.Feed(F.Payload, Out_) then
          begin
            for I := 0 to Length(Out_) - 1 do
              FRxBuf.Enqueue(Out_[I]);
            FRxBuf.Enqueue(KWP1281_BLOCK_END);
            Exit;
          end
          else if not GotFlow then
          begin
            SendFlowControl;
            GotFlow := True;
          end;
        end;
      fkConsecutive:
        if FReassembler.Feed(F.Payload, Out_) then
        begin
          for I := 0 to Length(Out_) - 1 do
            FRxBuf.Enqueue(Out_[I]);
          FRxBuf.Enqueue(KWP1281_BLOCK_END);
          Exit;
        end;
      fkFlowControl:
        ; // ignore - we're the receiver here
    end;
  end;
end;

procedure TKWP1281ISOTPTransport.SendByte(AByte: Byte;
  ATimeoutMs: Integer);
var Block: TBytes;
begin
  FTxBuf.Add(AByte);
  if AByte = KWP1281_BLOCK_END then
  begin
    SetLength(Block, FTxBuf.Count - 1);
    if FTxBuf.Count > 1 then
      Move(FTxBuf.List[0], Block[0], FTxBuf.Count - 1);
    FTxBuf.Clear;
    FlushAsIsoTpMessage(Block);
  end
  else
    FAckEcho.Enqueue(Byte(not AByte));
end;

function TKWP1281ISOTPTransport.ReceiveByte(ATimeoutMs: Integer): Byte;
begin
  if FAckEcho.Count > 0 then Exit(FAckEcho.Dequeue);
  if FRxBuf.Count = 0 then
    RefillFromIsoTp;
  if FRxBuf.Count = 0 then
    raise EKWP1281Timeout.Create(
      'KWP1281ISOTPTransport: nothing buffered after refill');
  Result := FRxBuf.Dequeue;
end;

procedure TKWP1281ISOTPTransport.FiveBaudInit(AAddress: Byte;
  out AKW1, AKW2: Byte; ATimeoutMs: Integer);
begin
  // No 5-baud init on CAN; expose the canonical key bytes so
  // the codec's logging stays useful.
  AKW1 := FFakeKW1;
  AKW2 := FFakeKW2;
end;

procedure TKWP1281ISOTPTransport.Hangup;
begin
  // Nothing to tear down at the ISO-TP layer.
end;

end.
