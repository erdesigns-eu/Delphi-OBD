//------------------------------------------------------------------------------
//  OBD.Diagnostics.J1939
//
//  TOBDJ1939 — non-visual J1939 (SAE J1939-21) bus-client
//  component. Sits between the codec / transport layers (the
//  <see cref="TOBDJ1939Codec"/> helpers and the
//  <see cref="TOBDJ1939SessionManager"/> state machine) and the
//  application, exposing:
//
//    - SourceAddress + NAME properties for address-claim
//      broadcasts.
//    - The owned TP / ETP / BAM session manager, accessible as
//      a published sub-object so hosts can wire OnFrameSend to
//      their preferred transmit path (TOBDProtocol's raw-frame
//      hook, a J2534 channel, a direct CAN driver, …).
//    - Convenience helpers that build the byte payloads for the
//      Address Claimed and Request PGNs (PGN 0x00EE00 and
//      PGN 0x00EA00).
//    - DispatchInbound: route a received frame into the session
//      manager and re-fire it through OnFrame after any TP / ETP
//      reassembly completes.
//
//  This component does NOT own a CAN transport. J1939 over ELM-
//  style adapters is rarely a clean fit, so the integration point
//  is intentionally explicit: the host wires
//  <c>Sessions.OnFrameSend</c> to whatever moves the bytes out
//  the door, and calls <see cref="DispatchInbound"/> for every
//  inbound frame.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - SAE J1939-21:2024 §5.4 (Address claim) — PGN 0x00EE00
//    - SAE J1939-21:2024 §5.5 (Request) — PGN 0x00EA00
//    - SAE J1939-21:2024 §5.10 (Transport protocol)
//    - SAE J1939-81 (Network management — NAME field)
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//
//  Future work :
//    - Concurrent-address-claim arbitration helper (compare
//      received NAMEs, drop to NULL_ADDRESS = 254 on loss).
//------------------------------------------------------------------------------

unit OBD.Diagnostics.J1939;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.J1939,
  OBD.Protocol.J1939.TP;

const
  /// <summary>J1939 NULL address — host has not claimed an
  /// address yet.</summary>
  J1939_NULL_ADDRESS         = $FE;
  /// <summary>J1939 GLOBAL destination — broadcast to all
  /// ECUs.</summary>
  J1939_GLOBAL_ADDRESS       = $FF;
  /// <summary>Address Claimed PGN.</summary>
  J1939_PGN_ADDRESS_CLAIMED  = $00EE00;
  /// <summary>Request PGN (used to request another PGN).</summary>
  J1939_PGN_REQUEST          = $00EA00;
  /// <summary>Maximum single-frame J1939 payload (8 bytes for
  /// classic CAN).</summary>
  J1939_SINGLE_FRAME_LIMIT   = 8;

type
  /// <summary>
  ///   Inbound J1939 frame event. Main thread.
  /// </summary>
  /// <remarks>
  ///   Reassembled multi-frame messages are delivered as a
  ///   single event — <c>AData</c> is the fully concatenated
  ///   payload after the session manager finishes any
  ///   TP / ETP / BAM reassembly. Single-frame messages fire
  ///   immediately.
  /// </remarks>
  TOBDJ1939FrameEvent = procedure(Sender: TObject; APriority: Byte;
    APGN: Cardinal; ASA: Byte; ADA: Byte;
    const AData: TBytes) of object;

  /// <summary>
  ///   J1939 bus-client component.
  /// </summary>
  /// <remarks>
  ///   Drop on a form, set <c>SourceAddress</c> and <c>NAME</c>,
  ///   wire <c>Sessions.OnFrameSend</c> to your CAN transmit path
  ///   and <c>Sessions.OnComplete</c> to a handler that calls
  ///   <see cref="FireFrame"/> through this component (or just
  ///   forward through <c>OnFrame</c> directly). Call
  ///   <see cref="DispatchInbound"/> for every received J1939
  ///   frame so multi-frame messages reassemble correctly.
  /// </remarks>
  TOBDJ1939 = class(TComponent)
  strict private
    FSourceAddress: Byte;
    FName: array[0..7] of Byte;
    FSessions: TOBDJ1939SessionManager;
    FOnFrame: TOBDJ1939FrameEvent;
    function GetName(AIndex: Integer): Byte;
    procedure SetName(AIndex: Integer; AValue: Byte);
  public
    /// <summary>Constructs the component with NULL source
    /// address and a fresh session manager.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees state and the inner session manager.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Builds the 8-byte payload for an Address Claimed
    ///   broadcast (PGN 0x00EE00). The caller is responsible for
    ///   transmitting the frame.
    /// </summary>
    /// <returns>NAME bytes, big-endian (high byte first).</returns>
    function BuildAddressClaimedPayload: TBytes;

    /// <summary>
    ///   Builds the 3-byte payload for a Request PGN (0x00EA00)
    ///   asking for <c>ARequestedPGN</c>.
    /// </summary>
    /// <param name="ARequestedPGN">PGN to request.</param>
    /// <returns>3-byte little-endian PGN payload.</returns>
    function BuildRequestPayload(ARequestedPGN: Cardinal): TBytes;

    /// <summary>
    ///   Encodes a 29-bit J1939 CAN identifier for sending
    ///   <c>APGN</c> from <c>SourceAddress</c> to <c>ADA</c>.
    /// </summary>
    /// <param name="APriority">3-bit priority (0..7).</param>
    /// <param name="APGN">Parameter Group Number.</param>
    /// <param name="ADA">Destination address.</param>
    /// <returns>29-bit CAN identifier.</returns>
    function EncodeId(APriority: Byte; APGN: Cardinal;
      ADA: Byte): Cardinal;

    /// <summary>
    ///   Routes an inbound J1939 frame into the session manager
    ///   and fires <c>OnFrame</c> for single-frame messages.
    /// </summary>
    /// <param name="ACanId">29-bit CAN identifier of the
    /// inbound frame.</param>
    /// <param name="APayload">Up to 8 bytes of payload.</param>
    /// <remarks>
    ///   Multi-frame TP / ETP / BAM segments are routed through
    ///   the session manager; the host listens to
    ///   <c>Sessions.OnComplete</c> for full-message events.
    ///   Single-frame messages fire <c>OnFrame</c> immediately
    ///   from this call (on the calling thread).
    /// </remarks>
    procedure DispatchInbound(ACanId: Cardinal;
      const APayload: TBytes);

    /// <summary>
    ///   Public path used by <see cref="DispatchInbound"/> and by
    ///   host code that wires <c>Sessions.OnComplete</c> directly.
    /// </summary>
    /// <param name="APriority">Decoded priority.</param>
    /// <param name="APGN">Reassembled PGN.</param>
    /// <param name="ASA">Source address.</param>
    /// <param name="ADA">Destination address (or
    /// <c>J1939_GLOBAL_ADDRESS</c>).</param>
    /// <param name="AData">Reassembled payload bytes.</param>
    procedure FireFrame(APriority: Byte; APGN: Cardinal; ASA: Byte;
      ADA: Byte; const AData: TBytes);

    /// <summary>
    ///   Sets the 8-byte NAME field used by Address Claimed
    ///   broadcasts.
    /// </summary>
    /// <param name="ANAME">Big-endian J1939 NAME — caller is
    /// responsible for packing the function / ECU instance / etc.
    /// bit fields per SAE J1939-81 §4.2.1.</param>
    procedure SetNAME(const ANAME: array of Byte);

    /// <summary>Reads back the configured NAME byte at
    /// <c>AIndex</c>.</summary>
    /// <param name="AIndex">Byte index 0..7 (high byte first).</param>
    property NAME[AIndex: Integer]: Byte read GetName write SetName;

    /// <summary>
    ///   Underlying TP / ETP / BAM session manager. Host wires
    ///   <c>OnFrameSend</c> here to a CAN transmit path and
    ///   <c>OnComplete</c> to a handler that calls
    ///   <see cref="FireFrame"/>.
    /// </summary>
    property Sessions: TOBDJ1939SessionManager read FSessions;
  published
    /// <summary>
    ///   This node's 8-bit J1939 source address. Default
    ///   <c>J1939_NULL_ADDRESS</c> (0xFE).
    /// </summary>
    property SourceAddress: Byte read FSourceAddress
      write FSourceAddress default J1939_NULL_ADDRESS;

    /// <summary>Fires on every inbound PGN. Main thread.</summary>
    property OnFrame: TOBDJ1939FrameEvent read FOnFrame write FOnFrame;
  end;

implementation

constructor TOBDJ1939.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FSessions := TOBDJ1939SessionManager.Create;
  FSourceAddress := J1939_NULL_ADDRESS;
  for I := 0 to 7 do
    FName[I] := $00;
end;

destructor TOBDJ1939.Destroy;
begin
  FSessions.Free;
  inherited;
end;

function TOBDJ1939.GetName(AIndex: Integer): Byte;
begin
  if (AIndex < 0) or (AIndex > 7) then
    raise EOBDConfig.CreateFmt(
      'TOBDJ1939.NAME: index %d out of range', [AIndex]);
  Result := FName[AIndex];
end;

procedure TOBDJ1939.SetName(AIndex: Integer; AValue: Byte);
begin
  if (AIndex < 0) or (AIndex > 7) then
    raise EOBDConfig.CreateFmt(
      'TOBDJ1939.NAME: index %d out of range', [AIndex]);
  FName[AIndex] := AValue;
end;

procedure TOBDJ1939.SetNAME(const ANAME: array of Byte);
var
  I: Integer;
  Count: Integer;
begin
  Count := Length(ANAME);
  if Count > 8 then
    Count := 8;
  for I := 0 to Count - 1 do
    FName[I] := ANAME[I];
  for I := Count to 7 do
    FName[I] := $00;
end;

function TOBDJ1939.BuildAddressClaimedPayload: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 0 to 7 do
    Result[I] := FName[I];
end;

function TOBDJ1939.BuildRequestPayload(ARequestedPGN: Cardinal): TBytes;
begin
  // The Request PGN body is the requested PGN encoded LE in 3
  // bytes (J1939-21 §5.5).
  SetLength(Result, 3);
  Result[0] := Byte( ARequestedPGN         and $FF);
  Result[1] := Byte((ARequestedPGN shr  8) and $FF);
  Result[2] := Byte((ARequestedPGN shr 16) and $FF);
end;

function TOBDJ1939.EncodeId(APriority: Byte; APGN: Cardinal;
  ADA: Byte): Cardinal;
begin
  Result := TOBDJ1939Codec.EncodeId(APriority, APGN, FSourceAddress, ADA);
end;

procedure TOBDJ1939.DispatchInbound(ACanId: Cardinal;
  const APayload: TBytes);
var
  Id: TOBDJ1939Id;
  Routed: Boolean;
begin
  Id := TOBDJ1939Codec.DecodeId(ACanId);
  Routed := False;
  case Id.PGN of
    J1939_PGN_TP_CM:
      Routed := FSessions.FeedTPCM(Id.SA, Id.DA, APayload);
    J1939_PGN_TP_DT:
      Routed := FSessions.FeedTPDT(Id.SA, Id.DA, APayload);
    J1939_PGN_ETP_CM:
      Routed := FSessions.FeedETPCM(Id.SA, Id.DA, APayload);
    J1939_PGN_ETP_DT:
      Routed := FSessions.FeedETPDT(Id.SA, Id.DA, APayload);
  end;
  if not Routed then
    FireFrame(Id.Priority, Id.PGN, Id.SA, Id.DA, APayload);
end;

procedure TOBDJ1939.FireFrame(APriority: Byte; APGN: Cardinal; ASA: Byte;
  ADA: Byte; const AData: TBytes);
var
  Self_: TOBDJ1939;
  Prio: Byte;
  PGN: Cardinal;
  SA: Byte;
  DA: Byte;
  Snap: TBytes;
begin
  if not Assigned(FOnFrame) then
    Exit;
  Self_ := Self;
  Prio := APriority;
  PGN := APGN;
  SA := ASA;
  DA := ADA;
  Snap := Copy(AData, 0, Length(AData));
  if TThread.CurrentThread.ThreadID = MainThreadID then
    FOnFrame(Self_, Prio, PGN, SA, DA, Snap)
  else
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Self_.FOnFrame) then
          Self_.FOnFrame(Self_, Prio, PGN, SA, DA, Snap);
      end);
end;

end.
