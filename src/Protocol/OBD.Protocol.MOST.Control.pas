//------------------------------------------------------------------------------
//  OBD.Protocol.MOST.Control
//
//  MOST (Media Oriented Systems Transport) control-message
//  primitives. Covers the MOST25 / MOST50 / MOST150 control
//  channel frame layout used for Function Block (FBlock) signalling
//  on the bus. Multimedia streaming (synchronous + isochronous
//  channels) is out of scope for an OBD diagnostics package.
//
//  Control message wire layout (MOST 1.5 / MOST 3.0 specs):
//
//    Source address            16 bits
//    Destination address       16 bits
//    FBlockID                   8 bits
//    InstID                     8 bits
//    FktID + OPType            12 + 4 bits
//    TelID + TelLen             4 +  4 bits
//    Data                      0..12 bytes (MOST25),
//                              0..45 bytes (MOST50/150)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - MOST Specification 3.0 § 4.2 (Control message format)
//    - MOST Cooperation Function Block / Property catalog
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.MOST.Control;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>Maximum data bytes per MOST25 control message.</summary>
  MOST25_CONTROL_DATA_MAX  = 12;
  /// <summary>Maximum data bytes per MOST50/150 control message.</summary>
  MOST50_CONTROL_DATA_MAX  = 45;

  // ---- Common OPType codes (MOST Function Block Class spec) ----
  MOST_OP_Set            = $00;
  MOST_OP_Get            = $01;
  MOST_OP_SetGet         = $02;
  MOST_OP_Increment      = $03;
  MOST_OP_Decrement      = $04;
  MOST_OP_GetInterface   = $05;
  MOST_OP_Status         = $0C;
  MOST_OP_Error          = $0F;

  // ---- Common FBlockIDs ----
  MOST_FBLOCK_NetBlock                = $01;
  MOST_FBLOCK_NetworkMaster           = $02;
  MOST_FBLOCK_ConnectionMaster        = $03;
  MOST_FBLOCK_PowerMaster             = $04;
  MOST_FBLOCK_Diagnosis               = $20;
  MOST_FBLOCK_AudioAmplifier          = $22;
  MOST_FBLOCK_Tuner_AmFm              = $40;

type
  /// <summary>Decoded MOST control message.</summary>
  TOBDMOSTControlMessage = record
    /// <summary>16-bit logical source address.</summary>
    SourceAddress: Word;
    /// <summary>16-bit logical destination address. Broadcast is
    /// 0x03C8; group addresses occupy 0x0300..0x03BF.</summary>
    DestinationAddress: Word;
    /// <summary>FBlock identifier.</summary>
    FBlockID: Byte;
    /// <summary>FBlock instance ID (1..n).</summary>
    InstID: Byte;
    /// <summary>12-bit function ID within the FBlock.</summary>
    FktID: Word;
    /// <summary>4-bit operation type (<c>MOST_OP_*</c>).</summary>
    OPType: Byte;
    /// <summary>Telegram ID (0..15). Single-segment messages use
    /// <c>TelID = 0, TelLen = 0</c>; multi-segment use <c>TelID</c>
    /// = segment index and <c>TelLen</c> = total segments - 1.</summary>
    TelID: Byte;
    /// <summary>Telegram length (number of segments minus 1, 0..15).</summary>
    TelLen: Byte;
    /// <summary>Application data bytes.</summary>
    Data: TBytes;
  end;

  /// <summary>Wire-format selector — controls which max-data ceiling
  /// applies during encoding.</summary>
  TOBDMOSTSpeed = (
    /// <summary>MOST25 control message (12-byte data ceiling).</summary>
    msMOST25,
    /// <summary>MOST50 control message (45-byte data ceiling).</summary>
    msMOST50,
    /// <summary>MOST150 control message (45-byte data ceiling).</summary>
    msMOST150
  );

/// <summary>
///   Encodes a control message into its wire bytes.
/// </summary>
/// <param name="AMessage">Message fields.</param>
/// <param name="ASpeed">MOST variant selector (controls the data
/// ceiling).</param>
/// <returns>Encoded bytes.</returns>
/// <exception cref="EOBDConfig">Field out of range or data length
/// exceeds the ceiling for <c>ASpeed</c>.</exception>
function MOSTEncodeControl(const AMessage: TOBDMOSTControlMessage;
  ASpeed: TOBDMOSTSpeed = msMOST25): TBytes;

/// <summary>
///   Decodes wire bytes into a control message. Returns False on
///   buffer too short or field overflow.
/// </summary>
/// <param name="ABytes">Bytes from the bus.</param>
/// <param name="AMessage">Output message.</param>
function MOSTDecodeControl(const ABytes: TBytes;
  out AMessage: TOBDMOSTControlMessage): Boolean;

/// <summary>
///   Maximum data length for a control message at the given speed.
/// </summary>
function MOSTControlDataMax(ASpeed: TOBDMOSTSpeed): Integer;

implementation

function MOSTControlDataMax(ASpeed: TOBDMOSTSpeed): Integer;
begin
  case ASpeed of
    msMOST25:               Result := MOST25_CONTROL_DATA_MAX;
    msMOST50, msMOST150:    Result := MOST50_CONTROL_DATA_MAX;
  else
    Result := MOST25_CONTROL_DATA_MAX;
  end;
end;

function MOSTEncodeControl(const AMessage: TOBDMOSTControlMessage;
  ASpeed: TOBDMOSTSpeed): TBytes;
var
  N, MaxData, Off: Integer;
  W: Word;
begin
  if AMessage.OPType > $0F then
    raise EOBDConfig.CreateFmt('MOST: OPType 0x%2.2X exceeds 4 bits',
      [AMessage.OPType]);
  if AMessage.FktID > $0FFF then
    raise EOBDConfig.CreateFmt('MOST: FktID 0x%4.4X exceeds 12 bits',
      [AMessage.FktID]);
  if (AMessage.TelID > $0F) or (AMessage.TelLen > $0F) then
    raise EOBDConfig.Create('MOST: TelID / TelLen exceed 4 bits');

  N := Length(AMessage.Data);
  MaxData := MOSTControlDataMax(ASpeed);
  if N > MaxData then
    raise EOBDConfig.CreateFmt(
      'MOST control: data length %d exceeds ceiling %d for selected speed',
      [N, MaxData]);

  // Layout: 4 (SA+DA) + 1 (FBlock) + 1 (Inst) + 2 (Fkt+Op) + 1 (Tel)
  // + N (data).
  SetLength(Result, 9 + N);
  Result[0] := Byte((AMessage.SourceAddress shr 8) and $FF);
  Result[1] := Byte(AMessage.SourceAddress and $FF);
  Result[2] := Byte((AMessage.DestinationAddress shr 8) and $FF);
  Result[3] := Byte(AMessage.DestinationAddress and $FF);
  Result[4] := AMessage.FBlockID;
  Result[5] := AMessage.InstID;
  W := Word(((AMessage.FktID and $0FFF) shl 4) or (AMessage.OPType and $0F));
  Result[6] := Byte((W shr 8) and $FF);
  Result[7] := Byte(W and $FF);
  Result[8] := Byte(((AMessage.TelID and $0F) shl 4) or
                    (AMessage.TelLen and $0F));
  Off := 9;
  if N > 0 then
    Move(AMessage.Data[0], Result[Off], N);
end;

function MOSTDecodeControl(const ABytes: TBytes;
  out AMessage: TOBDMOSTControlMessage): Boolean;
var
  W: Word;
  N: Integer;
begin
  Result := False;
  AMessage := Default(TOBDMOSTControlMessage);
  if Length(ABytes) < 9 then Exit;
  AMessage.SourceAddress      := (Word(ABytes[0]) shl 8) or Word(ABytes[1]);
  AMessage.DestinationAddress := (Word(ABytes[2]) shl 8) or Word(ABytes[3]);
  AMessage.FBlockID           := ABytes[4];
  AMessage.InstID             := ABytes[5];
  W := (Word(ABytes[6]) shl 8) or Word(ABytes[7]);
  AMessage.FktID  := (W shr 4) and $0FFF;
  AMessage.OPType := Byte(W and $0F);
  AMessage.TelID  := (ABytes[8] shr 4) and $0F;
  AMessage.TelLen := ABytes[8] and $0F;
  N := Length(ABytes) - 9;
  if N > 0 then
  begin
    SetLength(AMessage.Data, N);
    Move(ABytes[9], AMessage.Data[0], N);
  end;
  Result := True;
end;

end.
