//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.J1939.TP
//
//  J1939 transport protocol coverage. Encoders, decoders, BAM
//  reassembly, RTS-CTS reassembly, ETP, abort flow, transmitter.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4c initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.J1939.TP;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>TP control-message encoder / decoder coverage.</summary>
  [TestFixture]
  TJ1939TPEncoderTests = class
  public
    /// <summary>RTS layout matches J1939-21 §5.10.4.</summary>
    [Test] procedure RTSLayout;
    /// <summary>BAM layout matches J1939-21 §5.10.4.</summary>
    [Test] procedure BAMLayout;
    /// <summary>CTS layout matches J1939-21 §5.10.4.</summary>
    [Test] procedure CTSLayout;
    /// <summary>EOMA layout matches J1939-21 §5.10.4.</summary>
    [Test] procedure EOMALayout;
    /// <summary>Abort layout matches J1939-21 §5.10.4.</summary>
    [Test] procedure AbortLayout;
    /// <summary>DT layout: sequence + 7 bytes; padded with 0xFF when
    /// chunk shorter than 7.</summary>
    [Test] procedure DTLayoutAndPadding;
    /// <summary>ETP RTS uses a 4-byte size field.</summary>
    [Test] procedure ETPRTSLayout;
    /// <summary>ETP CTS uses a 3-byte next-offset field.</summary>
    [Test] procedure ETPCTSLayout;
    /// <summary>ETP DPO layout matches J1939-21 §5.10.5.</summary>
    [Test] procedure ETPDPOLayout;
    /// <summary>ExtractPGN reads the trailing 3-byte little-endian
    /// PGN.</summary>
    [Test] procedure ExtractPGNCorrect;
  end;

  /// <summary>Receive-side BAM and RTS-CTS reassembly coverage.</summary>
  [TestFixture]
  TJ1939SessionRXTests = class
  public
    /// <summary>BAM round-trip: CM + 3 DT frames assemble a 17-byte
    /// payload.</summary>
    [Test] procedure BAMRoundTrip;
    /// <summary>RTS-CTS round-trip: CM + DT frames; manager emits
    /// CTS and EOMA.</summary>
    [Test] procedure RTSCTSRoundTrip;
    /// <summary>Out-of-order DT triggers an abort with reason
    /// arBadSequence.</summary>
    [Test] procedure BadSequenceAborts;
    /// <summary>Concurrent sessions on different (SA, DA, PGN)
    /// keys remain independent.</summary>
    [Test] procedure ConcurrentSessionsIndependent;
    /// <summary>Manager honours peer abort by clearing the session
    /// and firing OnAbort with the peer's reason.</summary>
    [Test] procedure PeerAbortClearsSession;
  end;

  /// <summary>Transmitter coverage.</summary>
  [TestFixture]
  TJ1939TransmitterTests = class
  public
    /// <summary>Broadcast send emits BAM CM + N DT frames.</summary>
    [Test] procedure BroadcastEmitsBAMAndDTs;
    /// <summary>Unicast send emits RTS, then sends DT bursts on
    /// each CTS, then completes on EOMA.</summary>
    [Test] procedure UnicastRTSCTSEOMACycle;
    /// <summary>Send below 9 bytes raises (payload too small for
    /// multi-packet).</summary>
    [Test] procedure TooSmallPayloadRaises;
    /// <summary>ETP broadcast not allowed (raises).</summary>
    [Test] procedure ETPBroadcastRaises;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.J1939,
  OBD.Protocol.J1939.TP;

{ ---- TJ1939TPEncoderTests ---------------------------------------------------- }

procedure TJ1939TPEncoderTests.RTSLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeRTS($FECA, 17, 3, $FF);
  Assert.AreEqual<NativeInt>(8, Length(B));
  Assert.AreEqual<Byte>(J1939_TPCM_RTS, B[0]);
  Assert.AreEqual<Byte>(17, B[1]);  // size LSB
  Assert.AreEqual<Byte>(0, B[2]);   // size MSB
  Assert.AreEqual<Byte>(3, B[3]);   // packets
  Assert.AreEqual<Byte>($FF, B[4]); // max per CTS
  // PGN little-endian: CA FE 00
  Assert.AreEqual<Byte>($CA, B[5]);
  Assert.AreEqual<Byte>($FE, B[6]);
  Assert.AreEqual<Byte>($00, B[7]);
end;

procedure TJ1939TPEncoderTests.BAMLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeBAM($FECB, 1000, 143);
  Assert.AreEqual<Byte>(J1939_TPCM_BAM, B[0]);
  Assert.AreEqual<Byte>(232, B[1]);  // 1000 = 0x03E8 -> LSB E8
  Assert.AreEqual<Byte>(3, B[2]);
  Assert.AreEqual<Byte>(143, B[3]);
  Assert.AreEqual<Byte>($FF, B[4]);
  Assert.AreEqual<Byte>($CB, B[5]);
  Assert.AreEqual<Byte>($FE, B[6]);
end;

procedure TJ1939TPEncoderTests.CTSLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeCTS($FECA, 4, 5);
  Assert.AreEqual<Byte>(J1939_TPCM_CTS, B[0]);
  Assert.AreEqual<Byte>(4, B[1]);
  Assert.AreEqual<Byte>(5, B[2]);
  Assert.AreEqual<Byte>($FF, B[3]);
  Assert.AreEqual<Byte>($FF, B[4]);
end;

procedure TJ1939TPEncoderTests.EOMALayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeEOMA($FECA, 17, 3);
  Assert.AreEqual<Byte>(J1939_TPCM_EOMA, B[0]);
  Assert.AreEqual<Byte>(17, B[1]);
  Assert.AreEqual<Byte>(3, B[3]);
end;

procedure TJ1939TPEncoderTests.AbortLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeAbort($FECA, Byte(arBadSequence));
  Assert.AreEqual<Byte>(J1939_TPCM_ABORT, B[0]);
  Assert.AreEqual<Byte>(Byte(arBadSequence), B[1]);
  Assert.AreEqual<Byte>($FF, B[2]);
end;

procedure TJ1939TPEncoderTests.DTLayoutAndPadding;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeDT(2, TBytes.Create($AA, $BB));
  Assert.AreEqual<NativeInt>(8, Length(B));
  Assert.AreEqual<Byte>(2, B[0]);
  Assert.AreEqual<Byte>($AA, B[1]);
  Assert.AreEqual<Byte>($BB, B[2]);
  // Remaining 5 bytes padded with 0xFF.
  Assert.AreEqual<Byte>($FF, B[3]);
  Assert.AreEqual<Byte>($FF, B[7]);
end;

procedure TJ1939TPEncoderTests.ETPRTSLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeETPRTS($CB00, 5000);
  Assert.AreEqual<Byte>(J1939_ETPCM_RTS, B[0]);
  // Size little-endian: 5000 = 0x00001388
  Assert.AreEqual<Byte>($88, B[1]);
  Assert.AreEqual<Byte>($13, B[2]);
  Assert.AreEqual<Byte>($00, B[3]);
  Assert.AreEqual<Byte>($00, B[4]);
end;

procedure TJ1939TPEncoderTests.ETPCTSLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeETPCTS($CB00, 200, 1);
  Assert.AreEqual<Byte>(J1939_ETPCM_CTS, B[0]);
  Assert.AreEqual<Byte>(200, B[1]);
  // Next offset little-endian: 1 = 01 00 00
  Assert.AreEqual<Byte>($01, B[2]);
  Assert.AreEqual<Byte>($00, B[3]);
  Assert.AreEqual<Byte>($00, B[4]);
end;

procedure TJ1939TPEncoderTests.ETPDPOLayout;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeETPDPO($CB00, 100, 256);
  Assert.AreEqual<Byte>(J1939_ETPCM_DPO, B[0]);
  Assert.AreEqual<Byte>(100, B[1]);
  Assert.AreEqual<Byte>($00, B[2]);
  Assert.AreEqual<Byte>($01, B[3]);
end;

procedure TJ1939TPEncoderTests.ExtractPGNCorrect;
var
  B: TBytes;
begin
  B := TOBDJ1939TPCodec.EncodeRTS($1ABCDE, 30, 5, $FF);
  Assert.AreEqual<Cardinal>($1ABCDE, TOBDJ1939TPCodec.ExtractPGN(B));
end;

{ ---- TJ1939SessionRXTests ---------------------------------------------------- }

type
  TJ1939Capture = class
  public
    Frames: TList<TOBDFrame>;
    CompletedSessions: TList<TJ1939Session>;
    CompletedPayloads: TList<TBytes>;
    AbortedSessions: TList<TJ1939Session>;
    AbortReasons: TList<TJ1939AbortReason>;
    constructor Create;
    destructor Destroy; override;
    procedure HandleFrame(const AFrame: TOBDFrame);
    procedure HandleComplete(const ASession: TJ1939Session;
      const APayload: TBytes);
    procedure HandleAbort(const ASession: TJ1939Session;
      AReason: TJ1939AbortReason);
  end;

constructor TJ1939Capture.Create;
begin
  inherited;
  Frames := TList<TOBDFrame>.Create;
  CompletedSessions := TList<TJ1939Session>.Create;
  CompletedPayloads := TList<TBytes>.Create;
  AbortedSessions := TList<TJ1939Session>.Create;
  AbortReasons := TList<TJ1939AbortReason>.Create;
end;

destructor TJ1939Capture.Destroy;
begin
  Frames.Free;
  CompletedSessions.Free;
  CompletedPayloads.Free;
  AbortedSessions.Free;
  AbortReasons.Free;
  inherited;
end;

procedure TJ1939Capture.HandleFrame(const AFrame: TOBDFrame);
begin
  Frames.Add(AFrame);
end;

procedure TJ1939Capture.HandleComplete(const ASession: TJ1939Session;
  const APayload: TBytes);
begin
  CompletedSessions.Add(ASession);
  CompletedPayloads.Add(Copy(APayload));
end;

procedure TJ1939Capture.HandleAbort(const ASession: TJ1939Session;
  AReason: TJ1939AbortReason);
begin
  AbortedSessions.Add(ASession);
  AbortReasons.Add(AReason);
end;

procedure TJ1939SessionRXTests.BAMRoundTrip;
var
  M: TOBDJ1939SessionManager;
  Cap: TJ1939Capture;
  PayloadIn, PayloadOut: TBytes;
  I: Integer;
begin
  M := TOBDJ1939SessionManager.Create;
  Cap := TJ1939Capture.Create;
  try
    M.OnComplete := Cap.HandleComplete;
    M.OnAbort := Cap.HandleAbort;
    M.OnFrameSend := Cap.HandleFrame;

    SetLength(PayloadIn, 17);
    for I := 0 to 16 do
      PayloadIn[I] := Byte($A0 + I);

    // Feed BAM CM (size 17, 3 packets) from SA 5 to broadcast.
    M.FeedTPCM(5, $FF, TOBDJ1939TPCodec.EncodeBAM($FECA, 17, 3));
    // Feed DT 1 (bytes 0..6).
    M.FeedTPDT(5, $FF,
      TOBDJ1939TPCodec.EncodeDT(1, Copy(PayloadIn, 0, 7)));
    // Feed DT 2 (bytes 7..13).
    M.FeedTPDT(5, $FF,
      TOBDJ1939TPCodec.EncodeDT(2, Copy(PayloadIn, 7, 7)));
    // Feed DT 3 (bytes 14..16, padded).
    M.FeedTPDT(5, $FF,
      TOBDJ1939TPCodec.EncodeDT(3, Copy(PayloadIn, 14, 3)));

    Assert.AreEqual(1, Cap.CompletedSessions.Count);
    PayloadOut := Cap.CompletedPayloads[0];
    Assert.AreEqual<NativeInt>(17, Length(PayloadOut));
    for I := 0 to 16 do
      Assert.AreEqual<Byte>(PayloadIn[I], PayloadOut[I]);
    Assert.AreEqual(0, M.SessionCount);
  finally
    Cap.Free;
    M.Free;
  end;
end;

procedure TJ1939SessionRXTests.RTSCTSRoundTrip;
var
  M: TOBDJ1939SessionManager;
  Cap: TJ1939Capture;
  PayloadIn: TBytes;
  I: Integer;
  CTSFound, EOMAFound: Boolean;
begin
  M := TOBDJ1939SessionManager.Create;
  Cap := TJ1939Capture.Create;
  try
    M.OnComplete := Cap.HandleComplete;
    M.OnAbort := Cap.HandleAbort;
    M.OnFrameSend := Cap.HandleFrame;

    SetLength(PayloadIn, 14);
    for I := 0 to 13 do
      PayloadIn[I] := Byte($10 + I);

    // RTS from peer SA 0x0A to us at 0xF1, PGN 0xFEEE, 14 bytes,
    // 2 packets.
    M.FeedTPCM($0A, $F1, TOBDJ1939TPCodec.EncodeRTS($FEEE, 14, 2, $FF));
    // Manager should have emitted a CTS already.
    CTSFound := False;
    for I := 0 to Cap.Frames.Count - 1 do
      if (Length(Cap.Frames[I].Payload) >= 1) and
         (Cap.Frames[I].Payload[0] = J1939_TPCM_CTS) then
        CTSFound := True;
    Assert.IsTrue(CTSFound, 'CTS should have been emitted on RTS');

    // Feed DT 1 + 2.
    M.FeedTPDT($0A, $F1,
      TOBDJ1939TPCodec.EncodeDT(1, Copy(PayloadIn, 0, 7)));
    M.FeedTPDT($0A, $F1,
      TOBDJ1939TPCodec.EncodeDT(2, Copy(PayloadIn, 7, 7)));

    Assert.AreEqual(1, Cap.CompletedSessions.Count);
    Assert.AreEqual<NativeInt>(14, Length(Cap.CompletedPayloads[0]));

    // EOMA should have been emitted.
    EOMAFound := False;
    for I := 0 to Cap.Frames.Count - 1 do
      if (Length(Cap.Frames[I].Payload) >= 1) and
         (Cap.Frames[I].Payload[0] = J1939_TPCM_EOMA) then
        EOMAFound := True;
    Assert.IsTrue(EOMAFound, 'EOMA should have been emitted on completion');
  finally
    Cap.Free;
    M.Free;
  end;
end;

procedure TJ1939SessionRXTests.BadSequenceAborts;
var
  M: TOBDJ1939SessionManager;
  Cap: TJ1939Capture;
  AbortFound: Boolean;
  I: Integer;
begin
  M := TOBDJ1939SessionManager.Create;
  Cap := TJ1939Capture.Create;
  try
    M.OnAbort := Cap.HandleAbort;
    M.OnFrameSend := Cap.HandleFrame;

    M.FeedTPCM(5, $F1, TOBDJ1939TPCodec.EncodeRTS($FECA, 14, 2, $FF));
    // Feed sequence 5 instead of expected 1.
    M.FeedTPDT(5, $F1,
      TOBDJ1939TPCodec.EncodeDT(5, TBytes.Create(1, 2, 3, 4, 5, 6, 7)));
    Assert.AreEqual(1, Cap.AbortedSessions.Count);
    Assert.AreEqual(Ord(arBadSequence), Ord(Cap.AbortReasons[0]));
    AbortFound := False;
    for I := 0 to Cap.Frames.Count - 1 do
      if (Length(Cap.Frames[I].Payload) >= 1) and
         (Cap.Frames[I].Payload[0] = J1939_TPCM_ABORT) then
        AbortFound := True;
    Assert.IsTrue(AbortFound);
  finally
    Cap.Free;
    M.Free;
  end;
end;

procedure TJ1939SessionRXTests.ConcurrentSessionsIndependent;
var
  M: TOBDJ1939SessionManager;
  Cap: TJ1939Capture;
begin
  M := TOBDJ1939SessionManager.Create;
  Cap := TJ1939Capture.Create;
  try
    M.OnComplete := Cap.HandleComplete;
    M.OnFrameSend := Cap.HandleFrame;

    M.FeedTPCM(5, $FF, TOBDJ1939TPCodec.EncodeBAM($FECA, 14, 2));
    M.FeedTPCM(8, $FF, TOBDJ1939TPCodec.EncodeBAM($FECB, 14, 2));
    Assert.AreEqual(2, M.SessionCount);

    M.FeedTPDT(5, $FF, TOBDJ1939TPCodec.EncodeDT(1, TBytes.Create(1, 2, 3, 4, 5, 6, 7)));
    M.FeedTPDT(8, $FF, TOBDJ1939TPCodec.EncodeDT(1, TBytes.Create(8, 9, 10, 11, 12, 13, 14)));
    M.FeedTPDT(5, $FF, TOBDJ1939TPCodec.EncodeDT(2, TBytes.Create(15, 16, 17, 18, 19, 20, 21)));
    M.FeedTPDT(8, $FF, TOBDJ1939TPCodec.EncodeDT(2, TBytes.Create(22, 23, 24, 25, 26, 27, 28)));

    Assert.AreEqual(2, Cap.CompletedSessions.Count);
    Assert.AreEqual(0, M.SessionCount);
  finally
    Cap.Free;
    M.Free;
  end;
end;

procedure TJ1939SessionRXTests.PeerAbortClearsSession;
var
  M: TOBDJ1939SessionManager;
  Cap: TJ1939Capture;
begin
  M := TOBDJ1939SessionManager.Create;
  Cap := TJ1939Capture.Create;
  try
    M.OnAbort := Cap.HandleAbort;
    M.OnFrameSend := Cap.HandleFrame;
    M.FeedTPCM(5, $F1, TOBDJ1939TPCodec.EncodeRTS($FECA, 14, 2, $FF));
    Assert.AreEqual(1, M.SessionCount);
    M.FeedTPCM(5, $F1, TOBDJ1939TPCodec.EncodeAbort($FECA,
      Byte(arResourcesNeeded)));
    Assert.AreEqual(0, M.SessionCount);
    Assert.AreEqual(1, Cap.AbortedSessions.Count);
    Assert.AreEqual(Ord(arResourcesNeeded), Ord(Cap.AbortReasons[0]));
  finally
    Cap.Free;
    M.Free;
  end;
end;

{ ---- TJ1939TransmitterTests -------------------------------------------------- }

procedure TJ1939TransmitterTests.BroadcastEmitsBAMAndDTs;
var
  Tx: TOBDJ1939Transmitter;
  Cap: TJ1939Capture;
  Payload: TBytes;
  I: Integer;
  BAMFrames, DTFrames: Integer;
begin
  Tx := TOBDJ1939Transmitter.Create;
  Cap := TJ1939Capture.Create;
  try
    Tx.Manager.OnFrameSend := Cap.HandleFrame;

    SetLength(Payload, 14);
    for I := 0 to 13 do Payload[I] := Byte(I);

    Tx.Send(5, $FF, $FECA, Payload);
    BAMFrames := 0;
    DTFrames := 0;
    for I := 0 to Cap.Frames.Count - 1 do
    begin
      if Length(Cap.Frames[I].Payload) < 1 then Continue;
      if Cap.Frames[I].Payload[0] = J1939_TPCM_BAM then Inc(BAMFrames)
      else if Cap.Frames[I].Payload[0] in [1..255] then Inc(DTFrames);
    end;
    Assert.AreEqual(1, BAMFrames, 'Exactly one BAM CM');
    Assert.AreEqual(2, DTFrames, 'Two DT frames for 14 bytes');
  finally
    Cap.Free;
    Tx.Free;
  end;
end;

procedure TJ1939TransmitterTests.UnicastRTSCTSEOMACycle;
var
  Tx: TOBDJ1939Transmitter;
  Cap: TJ1939Capture;
  Payload: TBytes;
  I: Integer;
  RTSFound: Boolean;
  DTCount: Integer;
begin
  Tx := TOBDJ1939Transmitter.Create;
  Cap := TJ1939Capture.Create;
  try
    Tx.Manager.OnFrameSend := Cap.HandleFrame;

    SetLength(Payload, 14);
    for I := 0 to 13 do Payload[I] := Byte($A0 + I);

    Tx.Send(5, $0A, $FECA, Payload);
    RTSFound := False;
    for I := 0 to Cap.Frames.Count - 1 do
      if (Length(Cap.Frames[I].Payload) >= 1) and
         (Cap.Frames[I].Payload[0] = J1939_TPCM_RTS) then
        RTSFound := True;
    Assert.IsTrue(RTSFound, 'Unicast Send should emit RTS');

    // Simulate the peer's CTS — manager should send the DT chunks.
    Tx.Manager.FeedTPCM($0A, 5, TOBDJ1939TPCodec.EncodeCTS($FECA, 2, 1));
    DTCount := 0;
    for I := 0 to Cap.Frames.Count - 1 do
      if (Length(Cap.Frames[I].Payload) >= 1) and
         (Cap.Frames[I].Payload[0] in [1..255]) and
         not (Cap.Frames[I].Payload[0] in [J1939_TPCM_RTS, J1939_TPCM_CTS,
              J1939_TPCM_BAM, J1939_TPCM_EOMA, J1939_TPCM_ABORT]) then
        Inc(DTCount);
    Assert.IsTrue(DTCount >= 2, 'Two DT frames should follow CTS');

    // Simulate peer EOMA — session should complete.
    Tx.Manager.OnComplete :=
      procedure(const ASession: TJ1939Session; const APayload: TBytes)
      begin
        Cap.CompletedSessions.Add(ASession);
        Cap.CompletedPayloads.Add(Copy(APayload));
      end;
    Tx.Manager.FeedTPCM($0A, 5, TOBDJ1939TPCodec.EncodeEOMA($FECA, 14, 2));
    Assert.AreEqual(1, Cap.CompletedSessions.Count);
  finally
    Cap.Free;
    Tx.Free;
  end;
end;

procedure TJ1939TransmitterTests.TooSmallPayloadRaises;
var
  Tx: TOBDJ1939Transmitter;
begin
  Tx := TOBDJ1939Transmitter.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Tx.Send(5, $0A, $FECA, TBytes.Create(1, 2, 3, 4));
      end,
      EOBDProtocolErr);
  finally
    Tx.Free;
  end;
end;

procedure TJ1939TransmitterTests.ETPBroadcastRaises;
var
  Tx: TOBDJ1939Transmitter;
  Big: TBytes;
begin
  Tx := TOBDJ1939Transmitter.Create;
  try
    SetLength(Big, 2000);
    Assert.WillRaise(
      procedure
      begin
        Tx.Send(5, $FF, $FECA, Big);
      end,
      EOBDProtocolErr);
  finally
    Tx.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJ1939TPEncoderTests);
  TDUnitX.RegisterTestFixture(TJ1939SessionRXTests);
  TDUnitX.RegisterTestFixture(TJ1939TransmitterTests);

end.
