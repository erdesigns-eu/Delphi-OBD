//------------------------------------------------------------------------------
//  OBD.Protocol.ISO15765
//
//  Helpers for the ISO 15765-2 (CAN-TP) wire format. When the bound
//  adapter is an ELM327 / OBDLink (the v1 default path), the chip
//  performs SF / FF / CF / FC framing on its own — the protocol layer
//  only needs to drive the chip's command set and reassemble the
//  text-mode response. When the bound adapter is a J2534 PassThru or
//  a raw-CAN device, the framing happens here.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 15765-2:2024 Network layer services
//    - ELM327 datasheet rev 2.3 §6 (auto-formatting / multi-frame)
//
//  History     :
//    2026-05-09  ERD  Initial implementation: SF / FF / CF / FC encoders +
//                     decoders + multi-frame reassembler.
//------------------------------------------------------------------------------

unit OBD.Protocol.ISO15765;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types;

type
  /// <summary>
  ///   Reassembles ISO-TP multi-frame responses observed as raw CAN
  ///   frames. ELM327 adapters with <c>ATAL</c> / <c>ATCAF1</c>
  ///   already do this on-chip; the reassembler is for the
  ///   J2534 / DoIP path where the host sees raw frames.
  /// </summary>
  /// <remarks>
  ///   One reassembler instance per <c>(rx-id)</c>. Concurrent
  ///   transactions on different IDs need separate instances.
  ///   Not thread-safe.
  /// </remarks>
  TOBDIso15765Reassembler = class
  strict private
    FBuffer: TBytes;
    FExpected: Integer;
    FNextSeq: Byte;
    FInFlight: Boolean;
    procedure Reset;
  public
    /// <summary>Constructs an idle reassembler.</summary>
    constructor Create;

    /// <summary>
    ///   Feeds a single CAN payload (the bytes after the CAN ID).
    /// </summary>
    /// <param name="ABytes">Frame payload, 8 bytes for classic CAN
    /// or up to 64 for CAN-FD.</param>
    /// <param name="AOut">Reassembled application data when this
    /// frame completes the message.</param>
    /// <returns>True when <c>AOut</c> holds the complete message.</returns>
    function Feed(const ABytes: TBytes; out AOut: TBytes): Boolean;

    /// <summary>True when a multi-frame transaction is in progress.</summary>
    function InFlight: Boolean;

    /// <summary>Encodes a single-frame ISO-TP payload.</summary>
    /// <param name="AData">Application bytes to send. Must be ≤ 7
    /// bytes for classic CAN.</param>
    /// <returns>Bytes to put on the wire (without the CAN ID).</returns>
    /// <exception cref="EOBDProtocolErr">Data length too large for
    /// a single frame.</exception>
    class function EncodeSingleFrame(const AData: TBytes): TBytes; static;

    /// <summary>
    ///   Encodes a first-frame ISO-TP frame. Caller is responsible
    ///   for the consecutive frames; use the reassembler / a higher
    ///   layer to drive transmission.
    /// </summary>
    /// <param name="ATotalLen">Total application length.</param>
    /// <param name="AFirstChunk">First chunk (must be 6 bytes for
    /// classic CAN).</param>
    /// <returns>First-frame payload bytes.</returns>
    /// <exception cref="EOBDProtocolErr">Total length exceeds 4095
    /// (the classic-CAN limit).</exception>
    class function EncodeFirstFrame(ATotalLen: Cardinal;
      const AFirstChunk: TBytes): TBytes; static;

    /// <summary>Encodes a consecutive frame.</summary>
    /// <param name="ASequence">Sequence number 0..15 (low nibble).</param>
    /// <param name="AChunk">Up to 7 payload bytes.</param>
    /// <returns>Consecutive-frame payload bytes.</returns>
    class function EncodeConsecutiveFrame(ASequence: Byte;
      const AChunk: TBytes): TBytes; static;

    /// <summary>Encodes a flow-control frame.</summary>
    /// <param name="AFlow">0 = ContinueToSend, 1 = Wait, 2 =
    /// Overflow.</param>
    /// <param name="ABlockSize">Block size (0 = no chunking).</param>
    /// <param name="ASTmin">Separation time min (0 = none).</param>
    /// <returns>Flow-control payload bytes.</returns>
    class function EncodeFlowControlFrame(AFlow, ABlockSize,
      ASTmin: Byte): TBytes; static;

    /// <summary>
    ///   Categorises an inbound CAN payload.
    /// </summary>
    /// <param name="ABytes">Frame payload.</param>
    /// <returns>Frame kind. Returns <c>fkRaw</c> when the leading
    /// nibble does not match an ISO-TP frame type.</returns>
    class function ClassifyFrame(const ABytes: TBytes): TOBDFrameKind; static;
  end;

/// <summary>
///   Standard OBD-II tester CAN ID. <c>0x7DF</c> is the broadcast
///   request ID used when no specific ECU header has been set.
/// </summary>
const
  ISO15765_OBDII_BROADCAST_ID    = $7DF;
  /// <summary>OBD-II ECU response ID range start (0x7E8..0x7EF).</summary>
  ISO15765_OBDII_RESPONSE_BASE   = $7E8;
  /// <summary>OBD-II ECU response ID range end.</summary>
  ISO15765_OBDII_RESPONSE_LAST   = $7EF;

implementation

const
  // ISO-TP PCI nibbles (high nibble of the first payload byte).
  PCI_SF = $0;
  PCI_FF = $1;
  PCI_CF = $2;
  PCI_FC = $3;

{ ---- TOBDIso15765Reassembler ------------------------------------------------- }

constructor TOBDIso15765Reassembler.Create;
begin
  inherited;
  Reset;
end;

procedure TOBDIso15765Reassembler.Reset;
begin
  SetLength(FBuffer, 0);
  FExpected := 0;
  FNextSeq := 1;
  FInFlight := False;
end;

function TOBDIso15765Reassembler.InFlight: Boolean;
begin
  Result := FInFlight;
end;

function TOBDIso15765Reassembler.Feed(const ABytes: TBytes;
  out AOut: TBytes): Boolean;
var
  Pci: Byte;
  Len: Integer;
  Take: Integer;
  Seq: Byte;
  StartOff: Integer;
begin
  Result := False;
  AOut := nil;
  if Length(ABytes) = 0 then Exit;
  Pci := (ABytes[0] shr 4) and $0F;

  case Pci of
    PCI_SF:
      begin
        Reset;
        Len := ABytes[0] and $0F;
        if Len = 0 then Exit; // ignore malformed
        if Len > Length(ABytes) - 1 then
          Len := Length(ABytes) - 1;
        SetLength(AOut, Len);
        if Len > 0 then
          Move(ABytes[1], AOut[0], Len);
        Result := True;
      end;
    PCI_FF:
      begin
        Reset;
        if Length(ABytes) < 2 then Exit;
        FExpected := ((ABytes[0] and $0F) shl 8) or ABytes[1];
        if FExpected = 0 then Exit;
        StartOff := 2;
        Take := Length(ABytes) - StartOff;
        if Take > FExpected then
          Take := FExpected;
        SetLength(FBuffer, Take);
        if Take > 0 then
          Move(ABytes[StartOff], FBuffer[0], Take);
        FNextSeq := 1;
        FInFlight := True;
      end;
    PCI_CF:
      begin
        if not FInFlight then Exit;
        Seq := ABytes[0] and $0F;
        if Seq <> FNextSeq then
        begin
          // Sequence error — abort reassembly so the caller can
          // surface oeReassemblyFailed.
          Reset;
          Exit;
        end;
        Take := Length(ABytes) - 1;
        if Length(FBuffer) + Take > FExpected then
          Take := FExpected - Length(FBuffer);
        if Take > 0 then
        begin
          StartOff := Length(FBuffer);
          SetLength(FBuffer, StartOff + Take);
          Move(ABytes[1], FBuffer[StartOff], Take);
        end;
        FNextSeq := (FNextSeq + 1) and $0F;
        if Length(FBuffer) >= FExpected then
        begin
          AOut := Copy(FBuffer);
          Reset;
          Result := True;
        end;
      end;
    PCI_FC:
      ; // Flow control frames are not consumed here; the sender
        // side honours them. Caller may still see them via OnFrame.
  else
    // fkRaw — leave alone.
  end;
end;

class function TOBDIso15765Reassembler.EncodeSingleFrame(
  const AData: TBytes): TBytes;
var
  Len: Integer;
begin
  Len := Length(AData);
  if Len > 7 then
    raise EOBDProtocolErr.CreateFmt(
      'EncodeSingleFrame: %d bytes exceeds classic-CAN single-frame limit (7).',
      [Len]);
  SetLength(Result, Len + 1);
  Result[0] := (PCI_SF shl 4) or Byte(Len);
  if Len > 0 then
    Move(AData[0], Result[1], Len);
end;

class function TOBDIso15765Reassembler.EncodeFirstFrame(
  ATotalLen: Cardinal; const AFirstChunk: TBytes): TBytes;
var
  Take: Integer;
begin
  if ATotalLen > $0FFF then
    raise EOBDProtocolErr.CreateFmt(
      'EncodeFirstFrame: %d bytes exceeds classic-CAN long-frame limit (4095).',
      [ATotalLen]);
  Take := Length(AFirstChunk);
  if Take > 6 then
    Take := 6;
  SetLength(Result, 2 + Take);
  Result[0] := (PCI_FF shl 4) or Byte((ATotalLen shr 8) and $0F);
  Result[1] := Byte(ATotalLen and $FF);
  if Take > 0 then
    Move(AFirstChunk[0], Result[2], Take);
end;

class function TOBDIso15765Reassembler.EncodeConsecutiveFrame(
  ASequence: Byte; const AChunk: TBytes): TBytes;
var
  Take: Integer;
begin
  Take := Length(AChunk);
  if Take > 7 then Take := 7;
  SetLength(Result, 1 + Take);
  Result[0] := (PCI_CF shl 4) or (ASequence and $0F);
  if Take > 0 then
    Move(AChunk[0], Result[1], Take);
end;

class function TOBDIso15765Reassembler.EncodeFlowControlFrame(
  AFlow, ABlockSize, ASTmin: Byte): TBytes;
begin
  SetLength(Result, 3);
  Result[0] := (PCI_FC shl 4) or (AFlow and $0F);
  Result[1] := ABlockSize;
  Result[2] := ASTmin;
end;

class function TOBDIso15765Reassembler.ClassifyFrame(
  const ABytes: TBytes): TOBDFrameKind;
begin
  if Length(ABytes) = 0 then Exit(fkRaw);
  case (ABytes[0] shr 4) and $0F of
    PCI_SF: Result := fkSingle;
    PCI_FF: Result := fkFirst;
    PCI_CF: Result := fkConsecutive;
    PCI_FC: Result := fkFlowControl;
  else
    Result := fkRaw;
  end;
end;

end.
