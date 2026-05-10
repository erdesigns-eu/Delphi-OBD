//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.TP20
//
//  Pinned-vector tests for the TP2.0 codec. A fake CAN
//  transport scripts setup-ack and parameter-response frames so
//  the channel-setup dance + segmentation can be verified
//  without hardware.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.TP20;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DUnitX.TestFramework,
  OBD.Protocol.Types,
  OBD.Protocol.CAN,
  OBD.Protocol.TP20;

type
  TFakeCAN = class(TInterfacedObject, ICANTransport)
  strict private
    FTx:      TList<TOBDFrame>;
    FRx:      TQueue<TOBDFrame>;
    FOnFrame: TCANFrameEvent;
    FFilter:  TArray<Cardinal>;
    function  GetOnFrame: TCANFrameEvent;
    procedure SetOnFrame(const AValue: TCANFrameEvent);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   QueueRxFrame(AId: Cardinal; const APayload: array of Byte);
    function    TxFrames: TArray<TOBDFrame>;
    procedure SendFrame(const AFrame: TOBDFrame; ATimeoutMs: Integer);
    function  ReceiveFrame(ATimeoutMs: Integer): TOBDFrame;
    procedure SetAcceptanceFilter(const AIds: TArray<Cardinal>;
      AExtended: Boolean = False);
    procedure DrainRx;
    property  OnFrame: TCANFrameEvent read GetOnFrame write SetOnFrame;
  end;

  [TestFixture]
  TTP20CodecTests = class
  public
    [Test] procedure ConnectNegotiatesIdsAndParameters;
    [Test] procedure SendBlockSegmentsAndAcks;
  end;

implementation

{ TFakeCAN --------------------------------------------------------------------}

constructor TFakeCAN.Create;
begin
  inherited Create;
  FTx := TList<TOBDFrame>.Create;
  FRx := TQueue<TOBDFrame>.Create;
end;

destructor TFakeCAN.Destroy;
begin
  FTx.Free;
  FRx.Free;
  inherited;
end;

procedure TFakeCAN.QueueRxFrame(AId: Cardinal;
  const APayload: array of Byte);
var F: TOBDFrame; I: Integer;
begin
  F := Default(TOBDFrame);
  F.Id := AId;
  SetLength(F.Payload, Length(APayload));
  for I := 0 to High(APayload) do
    F.Payload[I] := APayload[I];
  FRx.Enqueue(F);
end;

function TFakeCAN.TxFrames: TArray<TOBDFrame>;
begin
  Result := FTx.ToArray;
end;

function TFakeCAN.GetOnFrame: TCANFrameEvent;
begin
  Result := FOnFrame;
end;

procedure TFakeCAN.SetOnFrame(const AValue: TCANFrameEvent);
begin
  FOnFrame := AValue;
end;

procedure TFakeCAN.SendFrame(const AFrame: TOBDFrame; ATimeoutMs: Integer);
begin
  FTx.Add(AFrame);
end;

function TFakeCAN.ReceiveFrame(ATimeoutMs: Integer): TOBDFrame;
begin
  if FRx.Count = 0 then
    raise EOBDCANTimeout.Create('FakeCAN: RX queue empty');
  Result := FRx.Dequeue;
end;

procedure TFakeCAN.SetAcceptanceFilter(const AIds: TArray<Cardinal>;
  AExtended: Boolean);
begin
  FFilter := AIds;
end;

procedure TFakeCAN.DrainRx;
begin
  FRx.Clear;
end;

{ TTP20CodecTests -------------------------------------------------------------}

procedure TTP20CodecTests.ConnectNegotiatesIdsAndParameters;
var
  Fake:  TFakeCAN;
  Iface: ICANTransport;
  Codec: TTP20Codec;
begin
  Fake  := TFakeCAN.Create;
  Iface := Fake;
  // Setup-ack: dest=$56, op=$D0, rx=hi/lo big-endian (codec
  // expects little-endian: byte2=lo, byte3=hi -> RX=$0301),
  // tx=byte4 lo, byte5 hi -> TX=$0240, then $01, app=$56.
  Fake.QueueRxFrame($340,
    [$56, $D0, $01, $03, $40, $02, $01, $56]);
  // Parameter response: $A1 + 5 dummy bytes
  Fake.QueueRxFrame($0301,
    [$A1, $0F, $8A, $FF, $32, $FF]);
  Codec := TTP20Codec.Create(Iface);
  try
    Codec.Connect(apRadio);
    Assert.IsTrue(Codec.Connected);
    Assert.AreEqual(Cardinal($0301), Codec.RxId);
    Assert.AreEqual(Cardinal($0240), Codec.TxId);
    Assert.AreEqual(Byte($56), Codec.AppId);
  finally
    Codec.Free;
    Iface := nil;
  end;
end;

procedure TTP20CodecTests.SendBlockSegmentsAndAcks;
var
  Fake:  TFakeCAN;
  Iface: ICANTransport;
  Codec: TTP20Codec;
  Block: TTP20Block;
  Sent:  TArray<TOBDFrame>;
begin
  Fake  := TFakeCAN.Create;
  Iface := Fake;
  Fake.QueueRxFrame($340,
    [$56, $D0, $01, $03, $40, $02, $01, $56]);
  Fake.QueueRxFrame($0301,
    [$A1, $0F, $8A, $FF, $32, $FF]);
  // Single-frame block (<=5 bytes after the length header) - no
  // intermediate ACK expected, just the final ACK after
  // DT_LAST_AK. Queue that ACK ($B0 with seq nibble).
  Fake.QueueRxFrame($0301, [$B1]);
  Codec := TTP20Codec.Create(Iface);
  try
    Codec.Connect(apRadio);
    SetLength(Block.Data, 4);
    Block.Data[0] := $03;
    Block.Data[1] := $00;
    Block.Data[2] := $03;
    Block.Data[3] := $03;
    Codec.SendBlock(Block);
    Sent := Fake.TxFrames;
    // Expect at least 3 TX frames: setup, param-req, data-block.
    Assert.IsTrue(Length(Sent) >= 3);
    // Last data frame should have op $30 (DT_LAST_AK) | seq=0
    // = $30, plus length header + payload.
    Assert.AreEqual(Byte($30), Sent[High(Sent)].Payload[0]);
    Assert.AreEqual(Byte($00), Sent[High(Sent)].Payload[1]); // hi(len)
    Assert.AreEqual(Byte($04), Sent[High(Sent)].Payload[2]); // lo(len)=4
  finally
    Codec.Free;
    Iface := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTP20CodecTests);

end.
