//------------------------------------------------------------------------------
//  OBD.Protocol.J1939.TP
//
//  J1939 transport layer (SAE J1939-21). Implements the multi-packet
//  Transport Protocol — TP.CM (PGN 0xEC00) + TP.DT (PGN 0xEB00) for
//  9..1785-byte messages, plus the Extended Transport Protocol —
//  ETP.CM (PGN 0xC800) + ETP.DT (PGN 0xC700) for messages larger
//  than 1785 bytes (up to 117,440,505 bytes).
//
//  Two flow patterns:
//    - BAM (Broadcast Announce Message): 9..1785 bytes, broadcast,
//      no flow control. CM is followed by DT chunks at fixed
//      cadence.
//    - RTS-CTS (Peer-to-peer): 9..1785 bytes (TP) or > 1785
//      (ETP), unicast, full flow control.
//
//  This unit ships:
//    - All TP.CM / TP.DT control-message encoders + decoders.
//    - All ETP.CM / ETP.DT control-message encoders + decoders.
//    - <see cref="TOBDJ1939SessionManager"/> — concurrent RX +
//      TX sessions keyed by (SA, DA, PGN); drives BAM and
//      RTS-CTS state machines including timeouts and abort.
//    - <see cref="TOBDJ1939Transmitter"/> — high-level TX
//      driver that splits a payload into the right frame
//      sequence and surfaces per-frame send callbacks the host
//      wires to its CAN bus driver.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1939-21:2024 §5.10 (Transport Protocol)
//    - SAE J1939-21 Appendix B (state diagrams)
//
//  History     :
//    2026-05-09  ERD  Initial implementation: full TP / ETP implementation
//                     with concurrent session manager.
//------------------------------------------------------------------------------

unit OBD.Protocol.J1939.TP;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.J1939;

const
  // ---- TP.CM control bytes (J1939-21 §5.10.4) ----
  J1939_TPCM_RTS    = $10;
  J1939_TPCM_CTS    = $11;
  J1939_TPCM_EOMA   = $13;
  J1939_TPCM_BAM    = $20;
  J1939_TPCM_ABORT  = $FF;

  // ---- ETP.CM control bytes (J1939-21 §5.10.5) ----
  J1939_ETPCM_RTS   = $14;
  J1939_ETPCM_CTS   = $15;
  J1939_ETPCM_DPO   = $16;
  J1939_ETPCM_EOMA  = $17;
  J1939_ETPCM_ABORT = $FF;

  // ---- Limits (J1939-21 §5.10.3) ----
  J1939_TP_MIN_BYTES   = 9;     // 8 bytes still fits in a single CAN frame
  J1939_TP_MAX_BYTES   = 1785;  // 255 packets × 7 bytes
  J1939_ETP_MIN_BYTES  = 1786;
  J1939_ETP_MAX_BYTES  = 117_440_505; // 16,777,215 × 7

  // ---- Standard J1939-21 timing constants (milliseconds) ----
  J1939_T1_MS = 750;   // Time between TP.CM and first TP.DT (BAM)
  J1939_T2_MS = 1250;  // Receiver timeout waiting for next TP.DT
  J1939_T3_MS = 1250;  // Sender timeout waiting for CTS/EOMA
  J1939_T4_MS = 1050;  // Time between consecutive CTSes
  J1939_TR_MS = 200;   // Inter-frame timing for BAM / DT bursts

type
  /// <summary>
  ///   Connection-abort reason codes (J1939-21 §5.10.6).
  /// </summary>
  TJ1939AbortReason = (
    /// <summary>Already in one or more connection-managed sessions
    /// and cannot support another.</summary>
    arAlreadyInSession = 1,
    /// <summary>System resources needed for another task; aborted
    /// for now.</summary>
    arResourcesNeeded = 2,
    /// <summary>Timeout while waiting for the expected next
    /// frame.</summary>
    arTimeout = 3,
    /// <summary>CTS received while transferring data.</summary>
    arCTSWhileSending = 4,
    /// <summary>Maximum retransmit-request limit reached.</summary>
    arMaxRetransmits = 5,
    /// <summary>Unexpected data packet.</summary>
    arUnexpectedDataPacket = 6,
    /// <summary>Bad sequence number (out of order).</summary>
    arBadSequence = 7,
    /// <summary>Duplicate sequence number.</summary>
    arDuplicateSequence = 8,
    /// <summary>Unexpected EDPO PGN.</summary>
    arUnexpectedEDPOPGN = 9,
    /// <summary>Unexpected EDPO message size.</summary>
    arUnexpectedEDPOSize = 10,
    /// <summary>Bad EDPO offset.</summary>
    arBadEDPOOffset = 11,
    /// <summary>Number of packets in CTS exceeds DPO max.</summary>
    arPacketsExceedDPO = 12,
    /// <summary>EDPO before CTS.</summary>
    arEDPOBeforeCTS = 13,
    /// <summary>Unspecified abort.</summary>
    arOther = 250,
    /// <summary>Synthetic — host code timed out the session.</summary>
    arHostTimeout = 251
  );

  /// <summary>State of a single connection-managed session.</summary>
  TJ1939SessionState = (
    /// <summary>Session not started.</summary>
    ssIdle,
    /// <summary>RX side: BAM CM seen, awaiting DT chunks.</summary>
    ssReceivingBAM,
    /// <summary>RX side: RTS seen, sent CTS, awaiting DT chunks.</summary>
    ssReceivingRTS,
    /// <summary>TX side: RTS sent, awaiting first CTS.</summary>
    ssAwaitingCTS,
    /// <summary>TX side: CTS received, sending DT chunks.</summary>
    ssSendingDT,
    /// <summary>TX side: all DT sent, awaiting EOMA.</summary>
    ssAwaitingEOMA,
    /// <summary>TX side: BAM CM sent, sending DT chunks at cadence.</summary>
    ssSendingBAM,
    /// <summary>Session completed successfully.</summary>
    ssCompleted,
    /// <summary>Session aborted (peer-initiated or host timeout).</summary>
    ssAborted
  );

  /// <summary>Direction tag.</summary>
  TJ1939SessionDirection = (sdReceive, sdTransmit);

  /// <summary>
  ///   Snapshot of a transport session.
  /// </summary>
  TJ1939Session = record
    /// <summary>Source address (the originator of the multi-packet
    /// message).</summary>
    SA: Byte;
    /// <summary>Destination address. <c>0xFF</c> for broadcast.</summary>
    DA: Byte;
    /// <summary>Embedded PGN (the multi-packet message's own PGN, not
    /// the TP.CM PGN).</summary>
    PGN: Cardinal;
    /// <summary>Direction tag.</summary>
    Direction: TJ1939SessionDirection;
    /// <summary>State machine cursor.</summary>
    State: TJ1939SessionState;
    /// <summary>Total expected (or sent) byte count.</summary>
    TotalSize: Cardinal;
    /// <summary>Total packet count.</summary>
    TotalPackets: Cardinal;
    /// <summary>Next packet number to expect (RX) or send (TX),
    /// 1-based.</summary>
    NextPacket: Cardinal;
    /// <summary>True when this is an Extended Transport (ETP) session.</summary>
    IsETP: Boolean;
    /// <summary>True when this is a BAM (broadcast) session.</summary>
    IsBAM: Boolean;
    /// <summary>For ETP: current Data Packet Offset (1-based of the
    /// first packet in the current burst).</summary>
    ETPOffset: Cardinal;
    /// <summary>Reassembly / payload buffer.</summary>
    Buffer: TBytes;
    /// <summary>Last activity timestamp (ms-precision wall clock).</summary>
    LastActivity: TDateTime;
  end;

  /// <summary>
  ///   Callback the manager invokes when a session completes
  ///   successfully.
  /// </summary>
  /// <param name="ASession">Final session record.</param>
  /// <param name="APayload">Reassembled application payload (RX) or
  /// echo of the sent bytes (TX). Owned by the manager; copy if
  /// retained.</param>
  TJ1939SessionCompleteEvent = procedure(const ASession: TJ1939Session;
    const APayload: TBytes) of object;

  /// <summary>Callback fired when a session aborts.</summary>
  TJ1939SessionAbortEvent = procedure(const ASession: TJ1939Session;
    AReason: TJ1939AbortReason) of object;

  /// <summary>
  ///   Callback the transmitter invokes for each frame it wants to
  ///   put on the bus. The host wires this to its CAN driver
  ///   (J2534 / DoIP / raw-CAN). The frame's <c>Id</c> field carries
  ///   the assembled 29-bit J1939 ID; <c>Payload</c> is exactly 8
  ///   bytes.
  /// </summary>
  TJ1939FrameSendEvent = procedure(const AFrame: TOBDFrame) of object;

  /// <summary>
  ///   Stateless TP / ETP control-message encoders and decoders.
  /// </summary>
  /// <remarks>
  ///   The 8-byte payload of every TP.CM / ETP.CM frame is built or
  ///   parsed by these helpers. PGN is little-endian in the trailing
  ///   3 bytes for both TP and ETP.
  /// </remarks>
  TOBDJ1939TPCodec = class
  public
    // ---- TP.CM ----
    /// <summary>Encodes an RTS control frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="ASize">Total message size 9..1785.</param>
    /// <param name="APackets">Total packet count 2..255.</param>
    /// <param name="AMaxPerCTS">Maximum packets per CTS, or 0xFF for
    /// no limit.</param>
    /// <returns>8 bytes ready for the CAN payload of TP.CM.</returns>
    class function EncodeRTS(APGN: Cardinal; ASize: Word;
      APackets, AMaxPerCTS: Byte): TBytes; static;

    /// <summary>Encodes a CTS control frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="APackets">Number of packets allowed in the next
    /// burst.</param>
    /// <param name="ANextPacket">1-based sequence number the sender
    /// should resume at.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeCTS(APGN: Cardinal; APackets,
      ANextPacket: Byte): TBytes; static;

    /// <summary>Encodes an EndOfMsgAck frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="ASize">Total message size.</param>
    /// <param name="APackets">Total packets received.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeEOMA(APGN: Cardinal; ASize: Word;
      APackets: Byte): TBytes; static;

    /// <summary>Encodes a BAM control frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="ASize">Total message size 9..1785.</param>
    /// <param name="APackets">Total packets.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeBAM(APGN: Cardinal; ASize: Word;
      APackets: Byte): TBytes; static;

    /// <summary>Encodes a connection-abort frame.</summary>
    /// <param name="APGN">PGN of the aborted session.</param>
    /// <param name="AReason">Abort reason byte.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeAbort(APGN: Cardinal;
      AReason: Byte): TBytes; static;

    /// <summary>Encodes a TP.DT data frame.</summary>
    /// <param name="ASequence">1-based sequence number 1..255.</param>
    /// <param name="AChunk">Up to 7 bytes of payload. Padded with
    /// 0xFF to 7 bytes when shorter.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeDT(ASequence: Byte;
      const AChunk: TBytes): TBytes; static;

    // ---- ETP.CM ----
    /// <summary>Encodes an ETP RTS frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="ASize">Total message size > 1785.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeETPRTS(APGN: Cardinal;
      ASize: Cardinal): TBytes; static;

    /// <summary>Encodes an ETP CTS frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="APackets">Packets allowed in the next burst.</param>
    /// <param name="ANextOffset">1-based packet offset for resume.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeETPCTS(APGN: Cardinal; APackets: Byte;
      ANextOffset: Cardinal): TBytes; static;

    /// <summary>Encodes an ETP DPO (Data Packet Offset) frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="APackets">Packets that follow this DPO.</param>
    /// <param name="AOffset">1-based packet offset of the first
    /// following packet.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeETPDPO(APGN: Cardinal; APackets: Byte;
      AOffset: Cardinal): TBytes; static;

    /// <summary>Encodes an ETP EOMA frame.</summary>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="ASize">Total bytes received.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeETPEOMA(APGN: Cardinal;
      ASize: Cardinal): TBytes; static;

    /// <summary>Encodes an ETP.DT data frame.</summary>
    /// <param name="ASequence">1-based per-burst sequence
    /// number.</param>
    /// <param name="AChunk">Up to 7 bytes; padded with 0xFF.</param>
    /// <returns>8-byte payload.</returns>
    class function EncodeETPDT(ASequence: Byte;
      const AChunk: TBytes): TBytes; static;

    /// <summary>Reads the embedded PGN from a TP / ETP control
    /// payload (last 3 bytes, little-endian).</summary>
    /// <param name="ABytes">Control payload (≥ 8 bytes).</param>
    /// <returns>Embedded PGN.</returns>
    class function ExtractPGN(const ABytes: TBytes): Cardinal; static;
  end;

  /// <summary>
  ///   Concurrent J1939 transport session manager.
  /// </summary>
  /// <remarks>
  ///   Owns one record per active session keyed by
  ///   <c>(SA, DA, PGN)</c>. Feed inbound TP.CM / TP.DT / ETP.CM
  ///   / ETP.DT frames via the <c>FeedXxx</c> methods; the manager
  ///   advances each session and fires <see cref="OnComplete"/> or
  ///   <see cref="OnAbort"/> as appropriate.
  ///
  ///   For TX sessions started via
  ///   <see cref="TOBDJ1939Transmitter.BeginTransmit"/>, the
  ///   manager is also responsible for emitting the right control
  ///   frames in response to peer CTS / EOMA frames.
  ///
  ///   Thread-safe: methods may be called from any thread; the
  ///   internal lock serialises access. Event callbacks fire on
  ///   the calling thread (typically the bus rx thread). Higher
  ///   layers should marshal to the main thread if needed.
  /// </remarks>
  TOBDJ1939SessionManager = class
  strict private
    FLock: TCriticalSection;
    FSessions: TList<TJ1939Session>;
    FOnComplete: TJ1939SessionCompleteEvent;
    FOnAbort: TJ1939SessionAbortEvent;
    FOnFrameSend: TJ1939FrameSendEvent;
    FTxPriority: Byte;
    FInterFramePaceMs: Cardinal;
    FTimeoutMs: Cardinal;
    FAutoSweepEnabled: Boolean;
    FSweepIntervalMs: Cardinal;
    FSweepThread: TThread;
    FSweepStop: TEvent;
    procedure SetAutoSweepEnabled(AValue: Boolean);
    procedure StartSweeper;
    procedure StopSweeper;

    function FindIndex(ASA, ADA: Byte; APGN: Cardinal): Integer;
    procedure UpdateSession(AIndex: Integer; const ASession: TJ1939Session);
    function CommitDT(AIndex: Integer; ASequence: Byte;
      const AChunk: TBytes; out ACompleted: Boolean): Boolean;
    procedure FireComplete(const ASession: TJ1939Session;
      const APayload: TBytes);
    procedure FireAbort(const ASession: TJ1939Session;
      AReason: TJ1939AbortReason);
    procedure SendOutbound(ASA, ADA: Byte; APGN: Cardinal;
      const APayload: TBytes);
  public
    /// <summary>Creates an empty manager.</summary>
    constructor Create;
    /// <summary>Releases internal storage.</summary>
    destructor Destroy; override;

    /// <summary>Number of active sessions (RX + TX combined).</summary>
    function SessionCount: Integer;
    /// <summary>Snapshot of an active session by index.</summary>
    /// <param name="AIndex">0-based index 0..SessionCount-1.</param>
    /// <returns>Session record copy.</returns>
    function GetSession(AIndex: Integer): TJ1939Session;

    /// <summary>
    ///   Feeds an inbound TP.CM frame.
    /// </summary>
    /// <param name="ASA">Source address (frame originator).</param>
    /// <param name="ADA">Destination address (us, or 0xFF for
    /// broadcast).</param>
    /// <param name="ABytes">8-byte TP.CM payload.</param>
    /// <returns>True when the frame advanced or started a session.</returns>
    function FeedTPCM(ASA, ADA: Byte; const ABytes: TBytes): Boolean;

    /// <summary>Feeds an inbound TP.DT frame.</summary>
    /// <param name="ASA">Source address.</param>
    /// <param name="ADA">Destination address.</param>
    /// <param name="ABytes">8-byte TP.DT payload (sequence + 7 data
    /// bytes).</param>
    /// <returns>True when the frame matched an active session.</returns>
    function FeedTPDT(ASA, ADA: Byte; const ABytes: TBytes): Boolean;

    /// <summary>Feeds an inbound ETP.CM frame.</summary>
    function FeedETPCM(ASA, ADA: Byte; const ABytes: TBytes): Boolean;

    /// <summary>Feeds an inbound ETP.DT frame.</summary>
    function FeedETPDT(ASA, ADA: Byte; const ABytes: TBytes): Boolean;

    /// <summary>Aborts an active session.</summary>
    /// <param name="ASA">Source address of the session.</param>
    /// <param name="ADA">Destination address.</param>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="AReason">Reason byte to send.</param>
    procedure AbortSession(ASA, ADA: Byte; APGN: Cardinal;
      AReason: TJ1939AbortReason);

    /// <summary>Times-out sessions whose <c>LastActivity</c> is older
    /// than the configured limit. Call from a periodic timer
    /// (every ~250 ms is fine).</summary>
    /// <param name="ANowMs">Reference now-time in milliseconds since
    /// some monotonic origin (typically <c>GetTickCount64</c>).</param>
    procedure SweepTimeouts(ANowMs: UInt64);

    /// <summary>Starts a TX session by sending RTS or BAM and
    /// optionally the first DT burst.</summary>
    /// <param name="ASA">Our source address.</param>
    /// <param name="ADA">Destination address; <c>0xFF</c> for
    /// broadcast (BAM).</param>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="APayload">Application payload to ship.</param>
    /// <exception cref="EOBDProtocolErr">Payload too small (< 9
    /// bytes) or too large (> ETP max).</exception>
    procedure BeginTransmit(ASA, ADA: Byte; APGN: Cardinal;
      const APayload: TBytes);

    /// <summary>Fires when a session reassembles successfully.</summary>
    property OnComplete: TJ1939SessionCompleteEvent
      read FOnComplete write FOnComplete;
    /// <summary>Fires when a session aborts (peer-initiated or
    /// host-timed-out).</summary>
    property OnAbort: TJ1939SessionAbortEvent
      read FOnAbort write FOnAbort;
    /// <summary>Wired by the host to its bus driver. The manager
    /// emits each outbound CAN frame here.</summary>
    property OnFrameSend: TJ1939FrameSendEvent
      read FOnFrameSend write FOnFrameSend;
    /// <summary>J1939 priority bits used when emitting outbound
    /// frames. Default 6.</summary>
    property TxPriority: Byte read FTxPriority write FTxPriority;

    /// <summary>
    ///   Inter-frame pace in milliseconds. When > 0, the manager
    ///   sleeps for this duration between consecutive
    ///   <c>SendOutbound</c> calls inside a TX burst (BAM cadence,
    ///   CTS-driven DT burst). Default <c>0</c> — no pacing.
    /// </summary>
    /// <remarks>
    ///   The manager releases its internal lock during the sleep so
    ///   other threads can still feed frames or query session
    ///   state. J1939-21 §5.10.4 conventional value is 50 ms (Tr).
    /// </remarks>
    property InterFramePaceMs: Cardinal read FInterFramePaceMs
      write FInterFramePaceMs default 0;

    /// <summary>
    ///   Per-session inactivity timeout in milliseconds. A session
    ///   whose <c>LastActivity</c> is older than this is aborted by
    ///   the next <see cref="SweepTimeouts"/> call. Default
    ///   <c>1250</c> (J1939-21 T2).
    /// </summary>
    property TimeoutMs: Cardinal read FTimeoutMs write FTimeoutMs
      default J1939_T2_MS;

    /// <summary>
    ///   When True, the manager spawns an internal thread that
    ///   calls <see cref="SweepTimeouts"/> every
    ///   <see cref="SweepIntervalMs"/> milliseconds. When False,
    ///   the host is responsible for invoking SweepTimeouts on its
    ///   own timer. Default False.
    /// </summary>
    /// <remarks>
    ///   Setting to True spawns a single background thread with
    ///   FreeOnTerminate := False. Setting to False stops and
    ///   joins the thread before returning. The destructor also
    ///   stops the thread.
    /// </remarks>
    property AutoSweepEnabled: Boolean read FAutoSweepEnabled
      write SetAutoSweepEnabled default False;

    /// <summary>
    ///   Interval between automatic sweep calls when
    ///   <see cref="AutoSweepEnabled"/> is True. Default
    ///   <c>250</c> ms.
    /// </summary>
    property SweepIntervalMs: Cardinal read FSweepIntervalMs
      write FSweepIntervalMs default 250;
  end;

  /// <summary>
  ///   Convenience wrapper around <see cref="TOBDJ1939SessionManager"/>
  ///   for transmit-only callers.
  /// </summary>
  TOBDJ1939Transmitter = class
  strict private
    FManager: TOBDJ1939SessionManager;
    FOwnsManager: Boolean;
  public
    /// <summary>Creates a transmitter with an internally-owned
    /// session manager.</summary>
    constructor Create; overload;
    /// <summary>Creates a transmitter that shares the supplied
    /// manager.</summary>
    /// <param name="AManager">Existing manager. Caller owns its
    /// lifetime.</param>
    constructor Create(AManager: TOBDJ1939SessionManager); overload;
    /// <summary>Releases the internal manager (when owned).</summary>
    destructor Destroy; override;

    /// <summary>Sends a multi-packet J1939 message via the
    /// underlying manager.</summary>
    /// <param name="ASA">Source address.</param>
    /// <param name="ADA">Destination (0xFF for broadcast).</param>
    /// <param name="APGN">Embedded PGN.</param>
    /// <param name="APayload">Application payload.</param>
    procedure Send(ASA, ADA: Byte; APGN: Cardinal;
      const APayload: TBytes);

    /// <summary>The shared / owned manager.</summary>
    property Manager: TOBDJ1939SessionManager read FManager;
  end;

implementation

uses
  Math;

const
  PADDING_BYTE = $FF;

{ ---- helpers ----------------------------------------------------------------- }

function PadToEight(const AChunk: TBytes; AOffsetIntoFrame: Integer): TBytes;
var
  CopyLen: Integer;
begin
  SetLength(Result, 8);
  FillChar(Result[0], 8, PADDING_BYTE);
  CopyLen := Length(AChunk);
  if CopyLen > 8 - AOffsetIntoFrame then
    CopyLen := 8 - AOffsetIntoFrame;
  if CopyLen > 0 then
    Move(AChunk[0], Result[AOffsetIntoFrame], CopyLen);
end;

procedure WritePGN(var ABytes: TBytes; AOffset: Integer; APGN: Cardinal);
begin
  ABytes[AOffset]     := Byte(APGN and $FF);
  ABytes[AOffset + 1] := Byte((APGN shr 8) and $FF);
  ABytes[AOffset + 2] := Byte((APGN shr 16) and $FF);
end;

function ReadLEU16(const ABytes: TBytes; AOffset: Integer): Word;
begin
  Result := Word(ABytes[AOffset]) or (Word(ABytes[AOffset + 1]) shl 8);
end;

function ReadLEU24(const ABytes: TBytes; AOffset: Integer): Cardinal;
begin
  Result := Cardinal(ABytes[AOffset]) or
            (Cardinal(ABytes[AOffset + 1]) shl 8) or
            (Cardinal(ABytes[AOffset + 2]) shl 16);
end;

function ReadLEU32(const ABytes: TBytes; AOffset: Integer): Cardinal;
begin
  Result := Cardinal(ABytes[AOffset]) or
            (Cardinal(ABytes[AOffset + 1]) shl 8) or
            (Cardinal(ABytes[AOffset + 2]) shl 16) or
            (Cardinal(ABytes[AOffset + 3]) shl 24);
end;

function NowMs: UInt64;
begin
  Result := UInt64(GetTickCount64);
end;

{ ---- TOBDJ1939TPCodec -------------------------------------------------------- }

class function TOBDJ1939TPCodec.EncodeRTS(APGN: Cardinal; ASize: Word;
  APackets, AMaxPerCTS: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_TPCM_RTS;
  Result[1] := Byte(ASize and $FF);
  Result[2] := Byte((ASize shr 8) and $FF);
  Result[3] := APackets;
  Result[4] := AMaxPerCTS;
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeCTS(APGN: Cardinal; APackets,
  ANextPacket: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_TPCM_CTS;
  Result[1] := APackets;
  Result[2] := ANextPacket;
  Result[3] := PADDING_BYTE;
  Result[4] := PADDING_BYTE;
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeEOMA(APGN: Cardinal; ASize: Word;
  APackets: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_TPCM_EOMA;
  Result[1] := Byte(ASize and $FF);
  Result[2] := Byte((ASize shr 8) and $FF);
  Result[3] := APackets;
  Result[4] := PADDING_BYTE;
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeBAM(APGN: Cardinal; ASize: Word;
  APackets: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_TPCM_BAM;
  Result[1] := Byte(ASize and $FF);
  Result[2] := Byte((ASize shr 8) and $FF);
  Result[3] := APackets;
  Result[4] := PADDING_BYTE;
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeAbort(APGN: Cardinal;
  AReason: Byte): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_TPCM_ABORT;
  Result[1] := AReason;
  Result[2] := PADDING_BYTE;
  Result[3] := PADDING_BYTE;
  Result[4] := PADDING_BYTE;
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeDT(ASequence: Byte;
  const AChunk: TBytes): TBytes;
begin
  Result := PadToEight(AChunk, 1);
  Result[0] := ASequence;
end;

class function TOBDJ1939TPCodec.EncodeETPRTS(APGN: Cardinal;
  ASize: Cardinal): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_ETPCM_RTS;
  Result[1] := Byte(ASize and $FF);
  Result[2] := Byte((ASize shr 8) and $FF);
  Result[3] := Byte((ASize shr 16) and $FF);
  Result[4] := Byte((ASize shr 24) and $FF);
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeETPCTS(APGN: Cardinal;
  APackets: Byte; ANextOffset: Cardinal): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_ETPCM_CTS;
  Result[1] := APackets;
  Result[2] := Byte(ANextOffset and $FF);
  Result[3] := Byte((ANextOffset shr 8) and $FF);
  Result[4] := Byte((ANextOffset shr 16) and $FF);
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeETPDPO(APGN: Cardinal;
  APackets: Byte; AOffset: Cardinal): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_ETPCM_DPO;
  Result[1] := APackets;
  Result[2] := Byte(AOffset and $FF);
  Result[3] := Byte((AOffset shr 8) and $FF);
  Result[4] := Byte((AOffset shr 16) and $FF);
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeETPEOMA(APGN: Cardinal;
  ASize: Cardinal): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := J1939_ETPCM_EOMA;
  Result[1] := Byte(ASize and $FF);
  Result[2] := Byte((ASize shr 8) and $FF);
  Result[3] := Byte((ASize shr 16) and $FF);
  Result[4] := Byte((ASize shr 24) and $FF);
  WritePGN(Result, 5, APGN);
end;

class function TOBDJ1939TPCodec.EncodeETPDT(ASequence: Byte;
  const AChunk: TBytes): TBytes;
begin
  Result := PadToEight(AChunk, 1);
  Result[0] := ASequence;
end;

class function TOBDJ1939TPCodec.ExtractPGN(const ABytes: TBytes): Cardinal;
begin
  if Length(ABytes) < 8 then Exit(0);
  Result := ReadLEU24(ABytes, 5);
end;

{ ---- TOBDJ1939SessionManager ------------------------------------------------- }

constructor TOBDJ1939SessionManager.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSessions := TList<TJ1939Session>.Create;
  FTxPriority := 6;
  FTimeoutMs := J1939_T2_MS;
  FSweepIntervalMs := 250;
  FSweepStop := TEvent.Create(nil, True, False, '');
end;

destructor TOBDJ1939SessionManager.Destroy;
begin
  StopSweeper;
  FSweepStop.Free;
  FSessions.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDJ1939SessionManager.SetAutoSweepEnabled(AValue: Boolean);
begin
  if FAutoSweepEnabled = AValue then Exit;
  FAutoSweepEnabled := AValue;
  if FAutoSweepEnabled then
    StartSweeper
  else
    StopSweeper;
end;

procedure TOBDJ1939SessionManager.StartSweeper;
var
  Self_: TOBDJ1939SessionManager;
begin
  if FSweepThread <> nil then Exit;
  FSweepStop.ResetEvent;
  Self_ := Self;
  FSweepThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while Self_.FSweepStop.WaitFor(Self_.FSweepIntervalMs) <> wrSignaled do
      begin
        try
          Self_.SweepTimeouts(UInt64(GetTickCount64));
        except
          // Don't let a transient handler exception kill the sweeper.
        end;
      end;
    end);
  FSweepThread.FreeOnTerminate := False;
  FSweepThread.Start;
end;

procedure TOBDJ1939SessionManager.StopSweeper;
var
  Worker: TThread;
begin
  Worker := FSweepThread;
  FSweepThread := nil;
  if Worker = nil then Exit;
  FSweepStop.SetEvent;
  Worker.WaitFor;
  Worker.Free;
end;

function TOBDJ1939SessionManager.SessionCount: Integer;
begin
  FLock.Enter;
  try
    Result := FSessions.Count;
  finally
    FLock.Leave;
  end;
end;

function TOBDJ1939SessionManager.GetSession(AIndex: Integer): TJ1939Session;
begin
  FLock.Enter;
  try
    Result := FSessions[AIndex];
  finally
    FLock.Leave;
  end;
end;

function TOBDJ1939SessionManager.FindIndex(ASA, ADA: Byte;
  APGN: Cardinal): Integer;
var
  I: Integer;
  S: TJ1939Session;
begin
  // Caller holds FLock.
  for I := 0 to FSessions.Count - 1 do
  begin
    S := FSessions[I];
    if (S.SA = ASA) and (S.DA = ADA) and (S.PGN = APGN) then
      Exit(I);
  end;
  Result := -1;
end;

procedure TOBDJ1939SessionManager.UpdateSession(AIndex: Integer;
  const ASession: TJ1939Session);
begin
  // Caller holds FLock.
  FSessions[AIndex] := ASession;
end;

procedure TOBDJ1939SessionManager.FireComplete(const ASession: TJ1939Session;
  const APayload: TBytes);
begin
  if Assigned(FOnComplete) then
    FOnComplete(ASession, APayload);
end;

procedure TOBDJ1939SessionManager.FireAbort(const ASession: TJ1939Session;
  AReason: TJ1939AbortReason);
begin
  if Assigned(FOnAbort) then
    FOnAbort(ASession, AReason);
end;

procedure TOBDJ1939SessionManager.SendOutbound(ASA, ADA: Byte;
  APGN: Cardinal; const APayload: TBytes);
var
  Frame: TOBDFrame;
begin
  if not Assigned(FOnFrameSend) then Exit;
  Frame := Default(TOBDFrame);
  Frame.Id := TOBDJ1939Codec.EncodeId(FTxPriority, APGN, ADA, ASA);
  Frame.IsExtendedId := True;
  Frame.Payload := Copy(APayload);
  Frame.Kind := fkRaw;
  Frame.Timestamp := Now;
  FOnFrameSend(Frame);
end;

function TOBDJ1939SessionManager.CommitDT(AIndex: Integer; ASequence: Byte;
  const AChunk: TBytes; out ACompleted: Boolean): Boolean;
var
  S: TJ1939Session;
  StartOff: Integer;
  Take: Integer;
  Payload: TBytes;
begin
  ACompleted := False;
  Result := False;
  S := FSessions[AIndex];
  if ASequence <> S.NextPacket then
  begin
    // Drop session on out-of-order DT — peer should have aborted
    // earlier. Caller will fire the abort.
    Exit(False);
  end;

  StartOff := Integer(S.NextPacket - 1) * 7;
  if S.IsETP then
    StartOff := Integer(S.ETPOffset - 1 + (S.NextPacket - 1)) * 7;
  Take := Length(AChunk);
  if StartOff + Take > Length(S.Buffer) then
    Take := Length(S.Buffer) - StartOff;
  if Take > 0 then
    Move(AChunk[0], S.Buffer[StartOff], Take);

  Inc(S.NextPacket);
  S.LastActivity := Now;
  Result := True;

  // BAM completes when the last packet arrives. RTS-CTS sessions
  // also complete when all packets have been received.
  if Cardinal(StartOff + Take) >= S.TotalSize then
  begin
    ACompleted := True;
    S.State := ssCompleted;
    UpdateSession(AIndex, S);
    Payload := Copy(S.Buffer, 0, S.TotalSize);
    if not S.IsBAM then
      // RTS-CTS sessions: respond with EOMA before completion.
      SendOutbound(S.DA, S.SA, J1939_PGN_TP_CM,
        TOBDJ1939TPCodec.EncodeEOMA(S.PGN, S.TotalSize, Byte(S.TotalPackets)));
    FireComplete(S, Payload);
    FSessions.Delete(AIndex);
  end
  else
    UpdateSession(AIndex, S);
end;

function TOBDJ1939SessionManager.FeedTPCM(ASA, ADA: Byte;
  const ABytes: TBytes): Boolean;
var
  Control: Byte;
  PGN: Cardinal;
  Idx: Integer;
  S: TJ1939Session;
  Size: Word;
  Packets: Byte;
  Reason: Byte;
  Payload: TBytes;
begin
  if Length(ABytes) < 8 then Exit(False);
  Control := ABytes[0];
  PGN := TOBDJ1939TPCodec.ExtractPGN(ABytes);
  Result := False;

  FLock.Enter;
  try
    case Control of
      J1939_TPCM_RTS:
        begin
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx >= 0 then
            FSessions.Delete(Idx);
          S := Default(TJ1939Session);
          S.SA := ASA;
          S.DA := ADA;
          S.PGN := PGN;
          S.Direction := sdReceive;
          S.State := ssReceivingRTS;
          S.TotalSize := ReadLEU16(ABytes, 1);
          S.TotalPackets := ABytes[3];
          S.NextPacket := 1;
          SetLength(S.Buffer, S.TotalSize);
          S.LastActivity := Now;
          FSessions.Add(S);
          // Respond with CTS for all packets in one burst (most-permissive).
          SendOutbound(ADA, ASA, J1939_PGN_TP_CM,
            TOBDJ1939TPCodec.EncodeCTS(PGN, Byte(S.TotalPackets), 1));
          Result := True;
        end;
      J1939_TPCM_BAM:
        begin
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx >= 0 then
            FSessions.Delete(Idx);
          S := Default(TJ1939Session);
          S.SA := ASA;
          S.DA := ADA;
          S.PGN := PGN;
          S.Direction := sdReceive;
          S.State := ssReceivingBAM;
          S.IsBAM := True;
          S.TotalSize := ReadLEU16(ABytes, 1);
          S.TotalPackets := ABytes[3];
          S.NextPacket := 1;
          SetLength(S.Buffer, S.TotalSize);
          S.LastActivity := Now;
          FSessions.Add(S);
          Result := True;
        end;
      J1939_TPCM_CTS:
        begin
          // TX-side: peer is granting us a window of packets.
          Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          if S.Direction <> sdTransmit then Exit(False);
          Packets := ABytes[1];
          S.NextPacket := ABytes[2];
          S.LastActivity := Now;
          S.State := ssSendingDT;
          UpdateSession(Idx, S);
          // Send the granted burst inline so a host without a
          // real-time scheduler still completes the transfer.
          // Pacing (when configured) releases the lock between
          // emits so other threads can continue to interact with
          // the manager.
          while (S.NextPacket <= S.TotalPackets) and (Packets > 0) do
          begin
            Payload := Copy(S.Buffer,
              Integer(S.NextPacket - 1) * 7,
              Min(7, Integer(S.TotalSize) - Integer(S.NextPacket - 1) * 7));
            SendOutbound(ADA, ASA, J1939_PGN_TP_DT,
              TOBDJ1939TPCodec.EncodeDT(Byte(S.NextPacket), Payload));
            Inc(S.NextPacket);
            Dec(Packets);
            if (Packets > 0) and (FInterFramePaceMs > 0) then
            begin
              UpdateSession(Idx, S);
              FLock.Leave;
              try
                TThread.Sleep(FInterFramePaceMs);
              finally
                FLock.Enter;
              end;
              // Re-resolve in case the session list mutated.
              Idx := FindIndex(ADA, ASA, PGN);
              if Idx < 0 then Break;
              S := FSessions[Idx];
            end;
          end;
          if S.NextPacket > S.TotalPackets then
            S.State := ssAwaitingEOMA;
          UpdateSession(Idx, S);
          Result := True;
        end;
      J1939_TPCM_EOMA:
        begin
          Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          S.State := ssCompleted;
          Payload := Copy(S.Buffer);
          FSessions.Delete(Idx);
          FireComplete(S, Payload);
          Result := True;
        end;
      J1939_TPCM_ABORT:
        begin
          Reason := ABytes[1];
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx < 0 then
            Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          S.State := ssAborted;
          FSessions.Delete(Idx);
          FireAbort(S, TJ1939AbortReason(Reason));
          Result := True;
        end;
    end;
  finally
    FLock.Leave;
  end;

  if Control = J1939_TPCM_RTS then
  begin
    // No further action; CTS already sent.
  end
  else if Control = J1939_TPCM_BAM then
  begin
    // Wait for DT frames at the BAM cadence.
    Size := 0; // suppress unused-warning when no work follows
    Inc(Size);
  end;
end;

function TOBDJ1939SessionManager.FeedTPDT(ASA, ADA: Byte;
  const ABytes: TBytes): Boolean;
var
  Idx: Integer;
  S: TJ1939Session;
  Sequence: Byte;
  Chunk: TBytes;
  Completed: Boolean;
begin
  Result := False;
  if Length(ABytes) < 8 then Exit;
  Sequence := ABytes[0];
  Chunk := Copy(ABytes, 1, 7);

  FLock.Enter;
  try
    Idx := FindIndex(ASA, ADA, 0);
    if Idx < 0 then
    begin
      // No session for explicit (SA,DA,*) — try matching any RX
      // session from this SA to this DA whose state expects DT.
      var I: Integer;
      Idx := -1;
      for I := 0 to FSessions.Count - 1 do
      begin
        S := FSessions[I];
        if (S.SA = ASA) and (S.DA = ADA) and
           (S.Direction = sdReceive) and
           (S.State in [ssReceivingBAM, ssReceivingRTS]) then
        begin
          Idx := I;
          Break;
        end;
      end;
    end;
    if Idx < 0 then Exit(False);
    if not CommitDT(Idx, Sequence, Chunk, Completed) then
    begin
      S := FSessions[Idx];
      FSessions.Delete(Idx);
      // Send abort on bad sequence.
      SendOutbound(S.DA, S.SA, J1939_PGN_TP_CM,
        TOBDJ1939TPCodec.EncodeAbort(S.PGN, Byte(arBadSequence)));
      FireAbort(S, arBadSequence);
      Exit(False);
    end;
    Result := True;
  finally
    FLock.Leave;
  end;
end;

function TOBDJ1939SessionManager.FeedETPCM(ASA, ADA: Byte;
  const ABytes: TBytes): Boolean;
var
  Control: Byte;
  PGN: Cardinal;
  Idx: Integer;
  S: TJ1939Session;
  Size: Cardinal;
  Reason: Byte;
  Payload: TBytes;
  Packets: Byte;
  NextOffset: Cardinal;
begin
  Result := False;
  if Length(ABytes) < 8 then Exit;
  Control := ABytes[0];
  PGN := TOBDJ1939TPCodec.ExtractPGN(ABytes);

  FLock.Enter;
  try
    case Control of
      J1939_ETPCM_RTS:
        begin
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx >= 0 then
            FSessions.Delete(Idx);
          S := Default(TJ1939Session);
          S.SA := ASA;
          S.DA := ADA;
          S.PGN := PGN;
          S.Direction := sdReceive;
          S.State := ssReceivingRTS;
          S.IsETP := True;
          S.TotalSize := ReadLEU32(ABytes, 1);
          S.TotalPackets := (S.TotalSize + 6) div 7;
          S.NextPacket := 1;
          S.ETPOffset := 1;
          SetLength(S.Buffer, S.TotalSize);
          S.LastActivity := Now;
          FSessions.Add(S);
          // Generous CTS — request all remaining packets in one
          // burst (255 packets per DPO max per spec).
          Packets := Min(255, Integer(S.TotalPackets));
          SendOutbound(ADA, ASA, J1939_PGN_ETP_CM,
            TOBDJ1939TPCodec.EncodeETPCTS(PGN, Packets, 1));
          Result := True;
        end;
      J1939_ETPCM_DPO:
        begin
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          if not S.IsETP then Exit(False);
          S.ETPOffset := ReadLEU24(ABytes, 2);
          S.NextPacket := 1; // packets in this DPO start at sequence 1
          S.LastActivity := Now;
          UpdateSession(Idx, S);
          Result := True;
        end;
      J1939_ETPCM_CTS:
        begin
          Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          if S.Direction <> sdTransmit then Exit(False);
          Packets := ABytes[1];
          NextOffset := ReadLEU24(ABytes, 2);
          S.ETPOffset := NextOffset;
          S.LastActivity := Now;
          // Send DPO + DT chunks.
          SendOutbound(ADA, ASA, J1939_PGN_ETP_CM,
            TOBDJ1939TPCodec.EncodeETPDPO(PGN, Packets, NextOffset));
          var SeqByte: Byte;
          var P: Byte;
          for P := 0 to Packets - 1 do
          begin
            SeqByte := P + 1;
            var StartOff: Integer;
            StartOff := Integer(NextOffset - 1 + P) * 7;
            if StartOff >= Length(S.Buffer) then Break;
            var Take: Integer;
            Take := Min(7, Length(S.Buffer) - StartOff);
            SendOutbound(ADA, ASA, J1939_PGN_ETP_DT,
              TOBDJ1939TPCodec.EncodeETPDT(SeqByte,
                Copy(S.Buffer, StartOff, Take)));
          end;
          UpdateSession(Idx, S);
          Result := True;
        end;
      J1939_ETPCM_EOMA:
        begin
          Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          S.State := ssCompleted;
          Payload := Copy(S.Buffer);
          FSessions.Delete(Idx);
          FireComplete(S, Payload);
          Result := True;
        end;
      J1939_ETPCM_ABORT:
        begin
          Reason := ABytes[1];
          Idx := FindIndex(ASA, ADA, PGN);
          if Idx < 0 then
            Idx := FindIndex(ADA, ASA, PGN);
          if Idx < 0 then Exit(False);
          S := FSessions[Idx];
          FSessions.Delete(Idx);
          FireAbort(S, TJ1939AbortReason(Reason));
          Result := True;
        end;
    end;
  finally
    FLock.Leave;
  end;

  // Suppress unused warning on Size in some compiler versions.
  Size := 0;
  Inc(Size);
end;

function TOBDJ1939SessionManager.FeedETPDT(ASA, ADA: Byte;
  const ABytes: TBytes): Boolean;
var
  Idx: Integer;
  S: TJ1939Session;
  Sequence: Byte;
  Chunk: TBytes;
  Completed: Boolean;
  I: Integer;
begin
  Result := False;
  if Length(ABytes) < 8 then Exit;
  Sequence := ABytes[0];
  Chunk := Copy(ABytes, 1, 7);
  FLock.Enter;
  try
    Idx := -1;
    for I := 0 to FSessions.Count - 1 do
    begin
      S := FSessions[I];
      if S.IsETP and (S.SA = ASA) and (S.DA = ADA) and
         (S.Direction = sdReceive) then
      begin
        Idx := I;
        Break;
      end;
    end;
    if Idx < 0 then Exit(False);
    if not CommitDT(Idx, Sequence, Chunk, Completed) then
    begin
      S := FSessions[Idx];
      FSessions.Delete(Idx);
      SendOutbound(S.DA, S.SA, J1939_PGN_ETP_CM,
        TOBDJ1939TPCodec.EncodeAbort(S.PGN, Byte(arBadSequence)));
      FireAbort(S, arBadSequence);
      Exit(False);
    end;
    if Completed then
    begin
      // RTS-CTS ETP sessions ack with ETP EOMA when complete.
      // CommitDT already deleted the session; we need to send the
      // ack manually as it doesn't know about ETP-specific framing.
      SendOutbound(ADA, ASA, J1939_PGN_ETP_CM,
        TOBDJ1939TPCodec.EncodeETPEOMA(S.PGN, S.TotalSize));
    end;
    Result := True;
  finally
    FLock.Leave;
  end;
end;

procedure TOBDJ1939SessionManager.AbortSession(ASA, ADA: Byte;
  APGN: Cardinal; AReason: TJ1939AbortReason);
var
  Idx: Integer;
  S: TJ1939Session;
  CMpgn: Cardinal;
begin
  FLock.Enter;
  try
    Idx := FindIndex(ASA, ADA, APGN);
    if Idx < 0 then Exit;
    S := FSessions[Idx];
    if S.IsETP then
      CMpgn := J1939_PGN_ETP_CM
    else
      CMpgn := J1939_PGN_TP_CM;
    SendOutbound(ASA, ADA, CMpgn,
      TOBDJ1939TPCodec.EncodeAbort(APGN, Byte(AReason)));
    FSessions.Delete(Idx);
    FireAbort(S, AReason);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDJ1939SessionManager.SweepTimeouts(ANowMs: UInt64);
var
  I: Integer;
  S: TJ1939Session;
  AgeMs: Int64;
  CMpgn: Cardinal;
begin
  FLock.Enter;
  try
    I := 0;
    while I < FSessions.Count do
    begin
      S := FSessions[I];
      AgeMs := MilliSecondsBetween(Now, S.LastActivity);
      if AgeMs > FTimeoutMs then
      begin
        if S.IsETP then CMpgn := J1939_PGN_ETP_CM
        else CMpgn := J1939_PGN_TP_CM;
        SendOutbound(S.DA, S.SA, CMpgn,
          TOBDJ1939TPCodec.EncodeAbort(S.PGN, Byte(arHostTimeout)));
        FSessions.Delete(I);
        FireAbort(S, arHostTimeout);
      end
      else
        Inc(I);
    end;
  finally
    FLock.Leave;
  end;
  // Suppress unused parameter in builds that don't use it.
  ANowMs := 0; Inc(ANowMs);
end;

procedure TOBDJ1939SessionManager.BeginTransmit(ASA, ADA: Byte;
  APGN: Cardinal; const APayload: TBytes);
var
  S: TJ1939Session;
  IsBroadcast: Boolean;
  IsETP: Boolean;
  Size: Cardinal;
  Packets: Cardinal;
  Idx: Integer;
  P: Cardinal;
  StartOff, Take: Integer;
begin
  Size := Length(APayload);
  if (Size < J1939_TP_MIN_BYTES) or (Size > J1939_ETP_MAX_BYTES) then
    raise EOBDProtocolErr.CreateFmt(
      'BeginTransmit: payload size %d out of range (%d..%d)',
      [Size, J1939_TP_MIN_BYTES, J1939_ETP_MAX_BYTES]);

  IsBroadcast := ADA = $FF;
  IsETP := Size > J1939_TP_MAX_BYTES;
  if IsBroadcast and IsETP then
    raise EOBDProtocolErr.Create(
      'BeginTransmit: ETP cannot be broadcast (payload > 1785 bytes)');

  Packets := (Size + 6) div 7;

  FLock.Enter;
  try
    Idx := FindIndex(ASA, ADA, APGN);
    if Idx >= 0 then
      FSessions.Delete(Idx);

    S := Default(TJ1939Session);
    S.SA := ASA;
    S.DA := ADA;
    S.PGN := APGN;
    S.Direction := sdTransmit;
    S.IsETP := IsETP;
    S.IsBAM := IsBroadcast;
    S.TotalSize := Size;
    S.TotalPackets := Packets;
    S.NextPacket := 1;
    S.ETPOffset := 1;
    S.Buffer := Copy(APayload);
    S.LastActivity := Now;

    if IsBroadcast then
    begin
      S.State := ssSendingBAM;
      FSessions.Add(S);
      // Send BAM CM.
      SendOutbound(ASA, ADA, J1939_PGN_TP_CM,
        TOBDJ1939TPCodec.EncodeBAM(APGN, Word(Size), Byte(Packets)));
      if FInterFramePaceMs > 0 then
      begin
        FLock.Leave;
        try
          TThread.Sleep(FInterFramePaceMs);
        finally
          FLock.Enter;
        end;
      end;
      // Send DT packets, optionally paced. The lock is released
      // during each sleep so other threads can interact with the
      // manager. J1939-21 §5.10.4 conventional cadence is 50 ms.
      for P := 1 to Packets do
      begin
        StartOff := Integer(P - 1) * 7;
        Take := Min(7, Integer(Size) - StartOff);
        SendOutbound(ASA, ADA, J1939_PGN_TP_DT,
          TOBDJ1939TPCodec.EncodeDT(Byte(P),
            Copy(S.Buffer, StartOff, Take)));
        if (P < Packets) and (FInterFramePaceMs > 0) then
        begin
          FLock.Leave;
          try
            TThread.Sleep(FInterFramePaceMs);
          finally
            FLock.Enter;
          end;
        end;
      end;
      S.State := ssCompleted;
      // BAM has no EOMA; complete the session immediately.
      FireComplete(S, S.Buffer);
      Idx := FindIndex(ASA, ADA, APGN);
      if Idx >= 0 then FSessions.Delete(Idx);
    end
    else if IsETP then
    begin
      S.State := ssAwaitingCTS;
      FSessions.Add(S);
      SendOutbound(ASA, ADA, J1939_PGN_ETP_CM,
        TOBDJ1939TPCodec.EncodeETPRTS(APGN, Size));
    end
    else
    begin
      S.State := ssAwaitingCTS;
      FSessions.Add(S);
      SendOutbound(ASA, ADA, J1939_PGN_TP_CM,
        TOBDJ1939TPCodec.EncodeRTS(APGN, Word(Size), Byte(Packets), $FF));
    end;
  finally
    FLock.Leave;
  end;
end;

{ ---- TOBDJ1939Transmitter ---------------------------------------------------- }

constructor TOBDJ1939Transmitter.Create;
begin
  inherited Create;
  FManager := TOBDJ1939SessionManager.Create;
  FOwnsManager := True;
end;

constructor TOBDJ1939Transmitter.Create(AManager: TOBDJ1939SessionManager);
begin
  inherited Create;
  FManager := AManager;
  FOwnsManager := False;
end;

destructor TOBDJ1939Transmitter.Destroy;
begin
  if FOwnsManager then
    FManager.Free;
  inherited;
end;

procedure TOBDJ1939Transmitter.Send(ASA, ADA: Byte; APGN: Cardinal;
  const APayload: TBytes);
begin
  FManager.BeginTransmit(ASA, ADA, APGN, APayload);
end;

end.
