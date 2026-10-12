// ------------------------------------------------------------------------------
// ERD.Protocol.ELMResponse
//
// Turns the text an ELM327-compatible adapter prints for one request into
// complete per-ECU messages.
//
// Handles every output shape the chip produces with CAN auto-formatting on:
// headers off (single line, or a 3-digit length line followed by "0:",
// "1:" ... segment lines), headers on (11-bit or 29-bit identifiers, each
// CAN frame on its own line with its ISO-TP PCI byte, frames of several
// ECUs possibly interleaved), spaces on or off, and legacy 3-byte headers
// with a trailing checksum (J1850 / ISO 9141 / KWP). Status lines such as
// "SEARCHING..." or "BUS INIT: ...OK" are skipped.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - ELM327 datasheet (ELM327DSL), "Multiline Responses" and "CAN Messages
//   and Filtering"
// - ISO 15765-2 (ISO-TP single / first / consecutive frame PCI)
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Protocol.ELMResponse;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.Protocol.Types;

type
  /// <summary>Classification of one line of adapter output.</summary>
  TOBDELMLineKind = (
    /// <summary>Blank line.</summary>
    lkEmpty,
    /// <summary>Status or error text (not hex), e.g. <c>SEARCHING...</c>.</summary>
    lkText,
    /// <summary>Headers-off multi-frame length line (3 hex digits).</summary>
    lkLength,
    /// <summary>Headers-off multi-frame segment (<c>N:</c> prefix).</summary>
    lkSegment,
    /// <summary>A frame with a header in front of it.</summary>
    lkFrame,
    /// <summary>Headers-off data line.</summary>
    lkData);

  /// <summary>One parsed line of adapter output.</summary>
  TOBDELMLine = record
    /// <summary>Line classification.</summary>
    Kind: TOBDELMLineKind;
    /// <summary>Header text (upper case, no spaces) for <c>lkFrame</c>.</summary>
    Header: string;
    /// <summary>Numeric header: CAN identifier, or the legacy source
    /// address byte.</summary>
    HeaderId: Cardinal;
    /// <summary>True for a 29-bit CAN identifier.</summary>
    IsExtendedId: Boolean;
    /// <summary>True when the header is a legacy 3-byte header.</summary>
    IsLegacy: Boolean;
    /// <summary>Segment index for <c>lkSegment</c>; announced length
    /// for <c>lkLength</c>.</summary>
    Number: Integer;
    /// <summary>Bytes after the header (CAN: PCI first). Legacy frames
    /// exclude the trailing checksum byte.</summary>
    Bytes: TBytes;
  end;

  /// <summary>
  /// Stateless parser for ELM327-style response text.
  /// </summary>
  TOBDELMResponseParser = class
  public
    /// <summary>
    /// Parses one line of adapter output.
    /// </summary>
    /// <param name="ALine">One line, without the line terminator.</param>
    /// <param name="AMode">Header mode; <c>ehAuto</c> detects CAN headers.</param>
    /// <returns>The parsed line.</returns>
    class function ParseLine(const ALine: string;
      AMode: TOBDELMHeaderMode = ehAuto): TOBDELMLine; static;

    /// <summary>
    /// Splits a complete adapter reply into per-ECU messages.
    /// </summary>
    /// <param name="ARaw">Adapter text for one request (prompt and echo
    /// already removed; lines separated by CR and/or LF).</param>
    /// <param name="AExpectedSID">Positive-response service ID the
    /// caller expects, or -1. With headers off, a data line that starts
    /// with neither this SID nor <c>7F</c> continues the previous line's
    /// message (adapters that wrap long replies).</param>
    /// <param name="AMode">Header mode; <c>ehAuto</c> detects CAN headers.</param>
    /// <returns>Messages in order of their first frame, including
    /// response-pending (<c>7F xx 78</c>) messages.</returns>
    class function Split(const ARaw: string; AExpectedSID: Integer = -1;
      AMode: TOBDELMHeaderMode = ehAuto): TArray<TOBDEcuMessage>; static;

    /// <summary>
    /// True for a "request correctly received, response pending"
    /// negative response (<c>7F xx 78</c>).
    /// </summary>
    /// <param name="AData">Message payload, service ID first.</param>
    /// <returns>True when the message only says "please wait".</returns>
    class function IsResponsePending(const AData: TBytes): Boolean; static;
  end;

implementation

const
  NRC_RESPONSE_PENDING = $78;
  NEGATIVE_RESPONSE = $7F;

function IsHexChar(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0' .. '9', 'A' .. 'F', 'a' .. 'f']);
end;

function NibbleOf(C: Char): Byte;
begin
  case C of
    '0' .. '9':
      Result := Ord(C) - Ord('0');
    'A' .. 'F':
      Result := Ord(C) - Ord('A') + 10;
    'a' .. 'f':
      Result := Ord(C) - Ord('a') + 10;
  else
    Result := 0;
  end;
end;

/// Removes blanks; returns False when anything but hex digits remains.
function CompactHex(const AText: string; out AHex: string): Boolean;
var
  I: Integer;
begin
  AHex := '';
  for I := 1 to Length(AText) do
  begin
    if CharInSet(AText[I], [' ', #9]) then
      Continue;
    if not IsHexChar(AText[I]) then
      Exit(False);
    AHex := AHex + UpCase(AText[I]);
  end;
  Result := True;
end;

/// Converts an even run of hex digits (starting at AFrom) into bytes.
function PairsToBytes(const AHex: string; AFrom: Integer): TBytes;
var
  Count, I: Integer;
begin
  Count := (Length(AHex) - AFrom + 1) div 2;
  if Count < 0 then
    Count := 0;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := (NibbleOf(AHex[AFrom + 2 * I]) shl 4) or
      NibbleOf(AHex[AFrom + 2 * I + 1]);
end;

function HexValue(const AHex: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AHex) do
    Result := (Result shl 4) or NibbleOf(AHex[I]);
end;

procedure AppendBytes(var ATarget: TBytes; const ASource: TBytes;
  AFrom: Integer);
var
  Old, Count: Integer;
begin
  Count := Length(ASource) - AFrom;
  if Count <= 0 then
    Exit;
  Old := Length(ATarget);
  SetLength(ATarget, Old + Count);
  Move(ASource[AFrom], ATarget[Old], Count);
end;

{ ---- TOBDELMResponseParser --------------------------------------------------- }

class function TOBDELMResponseParser.IsResponsePending
  (const AData: TBytes): Boolean;
begin
  Result := (Length(AData) >= 3) and (AData[0] = NEGATIVE_RESPONSE) and
    (AData[2] = NRC_RESPONSE_PENDING);
end;

class function TOBDELMResponseParser.ParseLine(const ALine: string;
  AMode: TOBDELMHeaderMode): TOBDELMLine;
var
  Line, Hex: string;
  HeaderLen: Integer;
begin
  Result.Kind := lkEmpty;
  Result.Header := '';
  Result.HeaderId := 0;
  Result.IsExtendedId := False;
  Result.IsLegacy := False;
  Result.Number := 0;
  Result.Bytes := nil;

  Line := Trim(ALine);
  if Line = '' then
    Exit;

  // Headers-off multi-frame segment: one hex digit, a colon, data.
  if (Length(Line) >= 2) and (Line[2] = ':') and IsHexChar(Line[1]) then
  begin
    if not CompactHex(Copy(Line, 3, MaxInt), Hex) or Odd(Length(Hex)) then
    begin
      Result.Kind := lkText;
      Exit;
    end;
    Result.Kind := lkSegment;
    Result.Number := NibbleOf(Line[1]);
    Result.Bytes := PairsToBytes(Hex, 1);
    Exit;
  end;

  if not CompactHex(Line, Hex) or (Hex = '') then
  begin
    Result.Kind := lkText;
    Exit;
  end;

  // Headers-off multi-frame length line ("014").
  if (Length(Hex) = 3) and (AMode in [ehAuto, ehOff]) then
  begin
    Result.Kind := lkLength;
    Result.Number := Integer(HexValue(Hex));
    Exit;
  end;

  HeaderLen := 0;
  case AMode of
    ehCAN11:
      HeaderLen := 3;
    ehCAN29:
      HeaderLen := 8;
    ehLegacy:
      HeaderLen := 6;
    ehAuto:
      if Odd(Length(Hex)) and (Length(Hex) >= 5) then
        HeaderLen := 3
      else if (Length(Hex) >= 10) and ((Copy(Hex, 1, 4) = '18DA') or
        (Copy(Hex, 1, 4) = '18DB')) then
        HeaderLen := 8;
  end;

  if HeaderLen = 0 then
  begin
    if Odd(Length(Hex)) then
    begin
      Result.Kind := lkText;
      Exit;
    end;
    Result.Kind := lkData;
    Result.Bytes := PairsToBytes(Hex, 1);
    Exit;
  end;

  if (Length(Hex) <= HeaderLen) or Odd(Length(Hex) - HeaderLen) then
  begin
    Result.Kind := lkText;
    Exit;
  end;

  Result.Kind := lkFrame;
  Result.Header := Copy(Hex, 1, HeaderLen);
  Result.Bytes := PairsToBytes(Hex, HeaderLen + 1);
  if AMode = ehLegacy then
  begin
    // Legacy header = priority, target, source; last byte = checksum.
    Result.IsLegacy := True;
    Result.HeaderId := HexValue(Copy(Hex, 5, 2));
    if Length(Result.Bytes) > 0 then
      SetLength(Result.Bytes, Length(Result.Bytes) - 1);
  end
  else
  begin
    Result.HeaderId := HexValue(Result.Header);
    Result.IsExtendedId := HeaderLen = 8;
  end;
end;

class function TOBDELMResponseParser.Split(const ARaw: string;
  AExpectedSID: Integer; AMode: TOBDELMHeaderMode): TArray<TOBDEcuMessage>;
type
  TAssembly = record
    Header: string;
    Index: Integer;
    Expected: Integer;
  end;
var
  Lines: TArray<string>;
  Assemblies: TArray<TAssembly>;
  Msgs: TArray<TOBDEcuMessage>;
  Count, I, J, Start, Len, BlockIndex, BlockExpected, PlainIndex: Integer;
  Line: TOBDELMLine;
  Msg: TOBDEcuMessage;
  Pci: Byte;

  procedure SplitLines;
  var
    P, From: Integer;
  begin
    Lines := nil;
    From := 1;
    for P := 1 to Length(ARaw) + 1 do
      if (P > Length(ARaw)) or CharInSet(ARaw[P], [#10, #13]) then
      begin
        if P > From then
        begin
          SetLength(Lines, Length(Lines) + 1);
          Lines[High(Lines)] := Copy(ARaw, From, P - From);
        end;
        From := P + 1;
      end;
  end;

  function Add(const AHeader: string; AId: Cardinal; AExtended: Boolean;
    const AData: TBytes; AComplete: Boolean): Integer;
  begin
    Msg.Header := AHeader;
    Msg.CanId := AId;
    Msg.IsExtendedId := AExtended;
    Msg.Data := AData;
    Msg.Complete := AComplete;
    if Count = Length(Msgs) then
      SetLength(Msgs, Count * 2 + 4);
    Msgs[Count] := Msg;
    Inc(Count);
    Exit(Count - 1);
  end;

  procedure FinishBlock;
  begin
    if BlockIndex < 0 then
      Exit;
    if BlockExpected >= 0 then
    begin
      if Length(Msgs[BlockIndex].Data) >= BlockExpected then
      begin
        SetLength(Msgs[BlockIndex].Data, BlockExpected);
        Msgs[BlockIndex].Complete := True;
      end
      else
        Msgs[BlockIndex].Complete := False;
    end;
    BlockIndex := -1;
    BlockExpected := -1;
  end;

  function FindAssembly(const AHeader: string): Integer;
  var
    K: Integer;
  begin
    for K := 0 to High(Assemblies) do
      if Assemblies[K].Header = AHeader then
        Exit(K);
    Result := -1;
  end;

  procedure DropAssembly(AIndex: Integer);
  var
    K: Integer;
  begin
    for K := AIndex to High(Assemblies) - 1 do
      Assemblies[K] := Assemblies[K + 1];
    SetLength(Assemblies, Length(Assemblies) - 1);
  end;

  procedure FrameLine;
  var
    A: Integer;
    Payload: TBytes;
  begin
    if Line.IsLegacy or (Length(Line.Bytes) = 0) then
    begin
      Add(Line.Header, Line.HeaderId, Line.IsExtendedId, Line.Bytes, True);
      Exit;
    end;
    Pci := Line.Bytes[0] shr 4;
    case Pci of
      0:
        begin
          // Single frame; CAN FD escape (length nibble 0) uses byte 2.
          Len := Line.Bytes[0] and $0F;
          Start := 1;
          if (Len = 0) and (Length(Line.Bytes) > 1) then
          begin
            Len := Line.Bytes[1];
            Start := 2;
          end;
          if Len > Length(Line.Bytes) - Start then
            Len := Length(Line.Bytes) - Start;
          Payload := Copy(Line.Bytes, Start, Len);
          Add(Line.Header, Line.HeaderId, Line.IsExtendedId, Payload, True);
        end;
      1:
        begin
          if Length(Line.Bytes) < 2 then
            Exit;
          Len := ((Line.Bytes[0] and $0F) shl 8) or Line.Bytes[1];
          Start := 2;
          if (Len = 0) and (Length(Line.Bytes) >= 6) then
          begin
            // Escape sequence for messages longer than 4095 bytes.
            Len := (Line.Bytes[2] shl 24) or (Line.Bytes[3] shl 16) or
              (Line.Bytes[4] shl 8) or Line.Bytes[5];
            Start := 6;
          end;
          A := FindAssembly(Line.Header);
          if A >= 0 then
          begin
            Msgs[Assemblies[A].Index].Complete := False;
            DropAssembly(A);
          end;
          Payload := Copy(Line.Bytes, Start, MaxInt);
          SetLength(Assemblies, Length(Assemblies) + 1);
          Assemblies[High(Assemblies)].Header := Line.Header;
          Assemblies[High(Assemblies)].Expected := Len;
          Assemblies[High(Assemblies)].Index :=
            Add(Line.Header, Line.HeaderId, Line.IsExtendedId, Payload, False);
          if Length(Payload) >= Len then
          begin
            SetLength(Msgs[Count - 1].Data, Len);
            Msgs[Count - 1].Complete := True;
            DropAssembly(High(Assemblies));
          end;
        end;
      2:
        begin
          A := FindAssembly(Line.Header);
          if A < 0 then
            Exit;
          AppendBytes(Msgs[Assemblies[A].Index].Data, Line.Bytes, 1);
          if Length(Msgs[Assemblies[A].Index].Data) >= Assemblies[A].Expected
          then
          begin
            SetLength(Msgs[Assemblies[A].Index].Data, Assemblies[A].Expected);
            Msgs[Assemblies[A].Index].Complete := True;
            DropAssembly(A);
          end;
        end;
      3:
        ; // Flow control from the tester side; not part of a reply.
    else
      // Not ISO-TP (CAN auto-formatting off or a raw frame): keep as is.
      Add(Line.Header, Line.HeaderId, Line.IsExtendedId, Line.Bytes, True);
    end;
  end;

begin
  Msgs := nil;
  Count := 0;
  Assemblies := nil;
  BlockIndex := -1;
  BlockExpected := -1;
  PlainIndex := -1;
  Msg.Header := '';
  SplitLines;

  for I := 0 to High(Lines) do
  begin
    Line := ParseLine(Lines[I], AMode);
    case Line.Kind of
      lkLength:
        begin
          FinishBlock;
          PlainIndex := -1;
          BlockIndex := Add('', 0, False, nil, False);
          BlockExpected := Line.Number;
        end;
      lkSegment:
        begin
          PlainIndex := -1;
          if BlockIndex < 0 then
          begin
            // Segment without a length line: start a message of
            // unknown length.
            BlockIndex := Add('', 0, False, nil, True);
            BlockExpected := -1;
          end;
          AppendBytes(Msgs[BlockIndex].Data, Line.Bytes, 0);
          if (BlockExpected >= 0) and
            (Length(Msgs[BlockIndex].Data) >= BlockExpected) then
            FinishBlock;
        end;
      lkFrame:
        begin
          FinishBlock;
          PlainIndex := -1;
          FrameLine;
        end;
      lkData:
        begin
          FinishBlock;
          if (PlainIndex >= 0) and (AExpectedSID >= 0) and
            (Line.Bytes[0] <> AExpectedSID) and
            (Line.Bytes[0] <> NEGATIVE_RESPONSE) then
            AppendBytes(Msgs[PlainIndex].Data, Line.Bytes, 0)
          else
            PlainIndex := Add('', 0, False, Line.Bytes, True);
        end;
    end;
  end;
  FinishBlock;

  // Multi-frame messages whose consecutive frames never arrived.
  for J := 0 to High(Assemblies) do
    Msgs[Assemblies[J].Index].Complete := False;

  SetLength(Msgs, Count);
  Result := Msgs;
end;

end.
