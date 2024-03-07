//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.CAN.pas
// CONTENTS       : OBD Protocol CAN Classes
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.CAN;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  OBD.Protocol, OBD.Protocol.Types;

type
  /// <summary>
  ///   CAN OBD Protocol
  /// </summary>
  TCANOBDProtocol = class(TOBDProtocol)
  public
    /// <summary>
    ///   Parse a Data Frame
    /// </summary>
    function ParseFrame(Frame: IOBDDataFrame): Boolean; override;
    /// <summary>
    ///   Parse a Data Message
    /// </summary>
    function ParseMessage(Msg: IOBDDataMessage): Boolean; override;
  end;

  /// <summary>
  ///   ISO 15765-4 (CAN 11/500) OBD Protocol
  /// </summary>
  TISO_15765_4_11BIT_500K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

  /// <summary>
  ///   ISO 15765-4 (CAN 29/500) OBD Protocol
  /// </summary>
  TISO_15765_4_29BIT_500K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

  /// <summary>
  ///   ISO 15765-4 (CAN 11/250) OBD Protocol
  /// </summary>
  TISO_15765_4_11BIT_250K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

  /// <summary>
  ///   ISO 15765-4 (CAN 29/250) OBD Protocol
  /// </summary>
  TISO_15765_4_29BIT_250K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

  /// <summary>
  ///   SAE J1939 (CAN 29/250) OBD Protocol
  /// </summary>
  TSAE_J1939_250K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

  /// <summary>
  ///   SAE J1939 (CAN 29/500) OBD Protocol
  /// </summary>
  TSAE_J1939_500K_OBDProtocol = class(TCANOBDProtocol)
  protected
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string; override;
    /// <summary>
    ///   Get the OBD Protocol name with aditional data
    /// </summary>
    function GetDisplayName: string; override;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; override;
  end;

implementation

uses System.Generics.Defaults, System.Generics.Collections;

const
  FRAME_TYPE_SF = $00; // Single frame
  FRAME_TYPE_FF = $10; // First frame of multi-frame message
  FRAME_TYPE_CF = $20; // Consecutive frame(s) of multi-frame message

//------------------------------------------------------------------------------
// PARSE FRAME
//------------------------------------------------------------------------------
function TCANOBDProtocol.ParseFrame(Frame: IOBDDataFrame): Boolean;
var
  Raw: string;
  RawBytes: TBytes;
begin
  // initialize result
  Result := False;

  // Set Raw
  Raw := Frame.Raw;

  // Pad 11-bit CAN headers out to 32 bits for consistency,
  // since ELM already does this for 29-bit CAN headers
  if (Self is TISO_15765_4_11BIT_500K_OBDProtocol) or
     (Self is TISO_15765_4_11BIT_250K_OBDProtocol) then
  Raw := '00000' + Raw;

  // If the frame length is ODD, drop the frame
  if Length(Raw) mod 2 <> 0 then Exit;

  // Convert to bytes
  RawBytes := TEncoding.UTF8.GetBytes(Raw);

  // If the frame length is too short, drop the frame
  if Length(RawBytes) < 6 then Exit;

  // If the frame length is too long, drop the frame
  if Length(RawBytes) > 12 then Exit;

  // Read header information (11 Bits)
  if (Self is TISO_15765_4_11BIT_500K_OBDProtocol) or
     (Self is TISO_15765_4_11BIT_250K_OBDProtocol) then
  begin
    // Always 7
    Frame.Priority := RawBytes[2] and $0F;
    // 0xD0 = functional, 0xE0 = physical
    Frame.AddrMode := RawBytes[3] and $F0;

    if Frame.AddrMode = $D0 then
    begin
      // Usually (always?) 0x0F for broadcast
      Frame.RxId := RawBytes[3] and $0F;
      // Made-up to mimic all other protocols
      Frame.TxId := $F1;
    end
    else if RawBytes[3] and $08 <> 0 then
    begin
      // Made-up to mimic all other protocols
      Frame.RxId := $F1;
      Frame.TxId := RawBytes[3] and $07;
    end
    else
    begin
      // made-up to mimic all other protocols
      Frame.TxId := $F1;
      Frame.RxId := RawBytes[3] and $07;
    end;
  end;

  // Read header information (29 Bits)
  if (Self is TISO_15765_4_29BIT_500K_OBDProtocol) or
     (Self is TISO_15765_4_29BIT_250K_OBDProtocol) or
     (Self is TSAE_J1939_250K_OBDProtocol) or
     (Self is TSAE_J1939_500K_OBDProtocol) then
  begin
    // Usually (always?) 0x18
    Frame.Priority := RawBytes[0];
    // DB = functional, DA = physical
    Frame.AddrMode := RawBytes[1];
    // 0x33 = broadcast (functional)
    Frame.RxId := RawBytes[2];
    // 0xF1 = tester ID
    Frame.TxId := RawBytes[3];
  end;

  // Extract the frame data
  Frame.Data := Copy(RawBytes, 4, Length(RawBytes) - 3);

  // Read PCI byte (always first byte in the data section)
  Frame.FrameType := Frame.Data[0] and $F0;
  // Exit here when the frame has a unknown PCI frame type
  if not (Frame.FrameType in [FRAME_TYPE_SF, FRAME_TYPE_FF, FRAME_TYPE_CF]) then Exit;

  // Single Frame
  if Frame.FrameType = FRAME_TYPE_SF then
  begin
    // Single frames have 4 bit length codes
    Frame.DataLength := Frame.Data[0] and $0F;
    // Drop frames with no data
    if Frame.DataLength = 0 then Exit;
  end;

  // First frame of multi-frame message
  if Frame.FrameType = FRAME_TYPE_FF then
  begin
    // First frames have 12 bit length codes
    Frame.DataLength := (Frame.Data[0] and $0F) shl 8;
    Frame.DataLength := Frame.DataLength or Frame.Data[1];
    // Drop frames with no data
    if Frame.DataLength = 0 then Exit
  end;

  // Consecutive frame
  if Frame.FrameType = FRAME_TYPE_CF then
  begin
    // Consecutive frames have 4 bit sequence indices
    Frame.SeqIndex := Frame.Data[0] and $0F;
  end;

  // If we get here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PARSE MESSAGE
//------------------------------------------------------------------------------
function TCANOBDProtocol.ParseMessage(Msg: IOBDDataMessage): Boolean;

  function Contiguous(Indices: TArray<Byte>; S, E: Integer): Boolean;
  var
    I: Integer;
  begin
    // initialize result
    Result := False;

    // Exit when indices cout is zero
    if Length(Indices) = 0 then Exit;
    // Exit here when the start indice is not the first
    if Indices[0] <> S then Exit;
    // Exit here when the end indice is not the last
    if Indices[High(Indices)] <> E then Exit;

    for I := Low(Indices) to High(Indices) - 1 do
    begin
      if Indices[I] + 1 <> Indices[I + 1] then Exit;
    end;

    // If we get here, the result is true
    Result := True;
  end;

var
  Frames: TArray<IOBDDataFrame>;
  Frame: IOBDDataFrame;
  FF: TArray<IOBDDataFrame>;
  CF: TArray<IOBDDataFrame>;
  Prev, Curr: IOBDDataFrame;
  Seq: Byte;
  Indices: TArray<Byte>;
  I: Integer;
  F: IOBDDataFrame;
  Comparer: IComparer<IOBDDataFrame>;
  NumDtcBytes: Integer;
begin
  // initialize result
  Result := False;

  // Set frames
  Frames := Msg.Frames;

  // Single frame
  if Length(Frames) = 1 then
  begin
    Frame := Frames[0];
    // Exit here when received frame is not marked as single frame
    if Frame.FrameType <> FRAME_TYPE_SF then Exit;
    // Extract data, ignore PCI byte and anything after the marked length
    Msg.Data := Copy(Frame.Data, 2, Frame.DataLength);
  end;

  // Multiple frames
  if Length(Frames) > 1 then
  begin
    // Sort First fame and Consecutive frame into their own lists
    SetLength(FF, 0);
    SetLength(CF, 0);

    // Loop over frames
    for F in Frames do
    begin
      // First frame
      if F.FrameType = FRAME_TYPE_FF then FF := FF + [F];
      // Consecutive frame
      if F.FrameType = FRAME_TYPE_CF then CF := CF + [F];
    end;

    // Exit here if we captured more than one first frame
    if Length(FF) > 1 then Exit;
    // Exit here if we never captured a first frame
    if Length(FF) = 0 then Exit;
    // Exit here if we never captured a consecutive frame
    if Length(CF) = 0 then Exit;

    // Calculate proper sequence indices from the lower 4 bits given
    for Prev in CF do
    begin
      for Curr in CF do
      begin
        Seq := (Prev.SeqIndex and not $0F) or Curr.SeqIndex;
        // If this is more than 7 frames away, we probably just wrapped (e.g.,
        // last=0x0F current=0x01 should mean 0x11, not 0x01)
        if Seq < Prev.SeqIndex - 7 then Seq := Seq + $10;
        Curr.SeqIndex := Seq;
      end;
    end;

    // Create an instance of the comparer
    Comparer := TOBDDataFrameComparer.Create;
    // Sort the frames by the order byte
    TArray.Sort<IOBDDataFrame>(Frames, Comparer);

    // Check contiguity, and that we aren't missing any frames
    SetLength(Indices, Length(CF));
    for I := 0 to High(CF) do Indices[I] := CF[I].SeqIndex;
    if not Contiguous(Indices, 1, Length(CF)) then Exit;

    // On the first frame, skip PCI byte AND length code
    Msg.Data := Copy(FF[0].Data, 3, Length(FF[0].Data) - 2);

    // Now that they're in order, load/accumulate the data from each CF frame
    for F in CF do Msg.Data := Msg.Data + Copy(F.Data, 2, Length(F.Data) - 1);
    // Chop to the correct size (as specified in the first frame)
    Msg.Data := Copy(Msg.Data, 1, FF[0].DataLength);

    // Mode 03
    if Msg.Data[1] = $43 then
    begin
      NumDtcBytes := Msg.Data[2] * 2;
      Msg.Data := Copy(Msg.Data, 1, NumDtcBytes + 2);
    end;
  end;

  // If we get here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/500): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_500K_OBDProtocol.GetName: string;
begin
  Result := 'ISO 15765-4 / SAE J2480 (CAN 11/500)';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/500): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_500K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 15765-4 (2 Wire CANH + CANL) / SAE J2480 (Single Wire CAN) - 11 bit identifier / 500 kbit/s - CAN 2.0A';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/500): GET ELM ID
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_500K_OBDProtocol.GetELMID: string;
begin
  Result := '6';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/500): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_500K_OBDProtocol.GetName: string;
begin
  Result := 'ISO 15765-4 / SAE J2480 (CAN 29/500)';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/500): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_500K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 15765-4 (2 Wire CANH + CANL) / SAE J2480 (Single Wire CAN) - 29 bit identifier / 500 kbit/s - CAN 2.0B';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/500): GET ELM ID
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_500K_OBDProtocol.GetELMID: string;
begin
  Result := '7';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/250): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_250K_OBDProtocol.GetName: string;
begin
  Result := 'ISO 15765-4 / SAE J2480 (CAN 11/250)';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/250): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_250K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 15765-4 (2 Wire CANH + CANL) / SAE J2480 (Single Wire CAN) - 11 bit identifier / 250 kbit/s - CAN 2.0A';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 11/250): GET ELM ID
//------------------------------------------------------------------------------
function TISO_15765_4_11BIT_250K_OBDProtocol.GetELMID: string;
begin
  Result := '8';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/250): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_250K_OBDProtocol.GetName: string;
begin
  Result := 'ISO 15765-4 / SAE J2480 (CAN 29/250)';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/250): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_250K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 15765-4 (2 Wire CANH + CANL) / SAE J2480 (Single Wire CAN) - 29 bit identifier / 250 kbit/s - CAN 2.0B';
end;

//------------------------------------------------------------------------------
// ISO 15765-4 (CAN 29/250): GET ELM ID
//------------------------------------------------------------------------------
function TISO_15765_4_29BIT_250K_OBDProtocol.GetELMID: string;
begin
  Result := '9';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/250): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TSAE_J1939_250K_OBDProtocol.GetName: string;
begin
  Result := 'SAE J1939 (CAN 29/250)';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/250): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TSAE_J1939_250K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'SAE J1939 (2 Wire CANH + CANL) - 29 bit identifier / 250 kbit/s - CAN 2.0B';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/250): GET ELM ID
//------------------------------------------------------------------------------
function TSAE_J1939_250K_OBDProtocol.GetELMID: string;
begin
  Result := 'A';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/500): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TSAE_J1939_500K_OBDProtocol.GetName: string;
begin
  Result := 'SAE J1939 (CAN 29/500)';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/500): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TSAE_J1939_500K_OBDProtocol.GetDisplayName: string;
begin
  Result := 'SAE J1939 (2 Wire CANH + CANL) - 29 bit identifier / 500 kbit/s - CAN 2.0B';
end;

//------------------------------------------------------------------------------
// SAE J1939 (CAN 29/500): GET ELM ID
//------------------------------------------------------------------------------
function TSAE_J1939_500K_OBDProtocol.GetELMID: string;
begin
  Result := 'B';
end;

end.
