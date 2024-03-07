//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.Legacy.pas
// CONTENTS       : OBD Protocol Legacy Classes
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.Legacy;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,

  OBD.Protocol, OBD.Protocol.Types;

type
  /// <summary>
  ///   Legacy OBD Protocol
  /// </summary>
  TLegacyOBDProtocol = class(TOBDProtocol)
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
  ///   SAE J1850 PWM OBD Protocol
  /// </summary>
  TSAE_J1850_PWM_OBDProtocol = class(TLegacyOBDProtocol)
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
  ///   SAE J1850 VPW OBD Protocol
  /// </summary>
  TSAE_J1850_VPW_OBDProtocol = class(TLegacyOBDProtocol)
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
  ///   ISO 9141-2 OBD Protocol
  /// </summary>
  TISO_9141_2_OBDProtocol = class(TLegacyOBDProtocol)
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
  ///   ISO 14230-4 (KWP 5BAUD) OBD Protocol
  /// </summary>
  TISO_14230_4_5BAUD_OBDProtocol = class(TLegacyOBDProtocol)
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
  ///   ISO 14230-4 (KWP FAST) OBD Protocol
  /// </summary>
  TISO_14230_4_FAST_OBDProtocol = class(TLegacyOBDProtocol)
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

//------------------------------------------------------------------------------
// PARSE FRAME
//------------------------------------------------------------------------------
function TLegacyOBDProtocol.ParseFrame(Frame: IOBDDataFrame): Boolean;
var
  RawBytes: TBytes;
begin
  // initialize result
  Result := False;

  // If the frame length is ODD, drop the frame
  if Length(Frame.Raw) mod 2 <> 0 then Exit;

  // Convert to bytes
  RawBytes := TEncoding.UTF8.GetBytes(Frame.Raw);

  // If the frame length is too short, drop the frame
  if Length(RawBytes) < 6 then Exit;

  // If the frame length is too long, drop the frame
  if Length(RawBytes) > 11 then Exit;

  // Exclude header and trailing checksum (handled by ELM adapter)
  Frame.Data := Copy(RawBytes, 4, Length(RawBytes) - 4);

  // Read header information
  Frame.Priority := RawBytes[0];
  Frame.RxID := RawBytes[1];
  Frame.TxID := RawBytes[2];

  // If we get here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// PARSE MESSAGE
//------------------------------------------------------------------------------
function TLegacyOBDProtocol.ParseMessage(Msg: IOBDDataMessage): Boolean;
var
  Frames: TArray<IOBDDataFrame>;
  Mode: Byte;
  Frame: IOBDDataFrame;
  I: Integer;
  FrameData: TBytes;
  Comparer: IComparer<IOBDDataFrame>;
begin
  // initialize result
  Result := False;

  // Set frames
  Frames := Msg.Frames;

  // Set the mode
  Mode := Frames[0].Data[0];

  // Make sure that all frames are responses to the same Mode (SID)
  if Length(Frames) > 1 then
  begin
    for I := 1 to High(Frames) do if Mode <> Frames[I].Data[0] then Exit;
  end;

  // Mode 03 contains a single command GET_DTC which requests all diagnostic
  // trouble codes from the vehicle.
  if Mode = $43 then
  begin
    // Forge the mode byte and CAN's DTC_count byte
    Msg.SetDataLength(2);
    // Loop over frames
    for Frame in Frames do
    begin
      // Exclude mode byte
      FrameData := Copy(Frame.Data, 1, Length(Frame.Data) - 1);
      // Append frame to data
      Msg.Data := Msg.Data + FrameData;
    end;
  end else

  // Other modes
  begin
    // Single frame
    if Length(Frames) = 1 then
    begin
      // Return data, excluding the mode/pid bytes
      Msg.Data := Frames[0].Data;
    end else

    // Multiple frames
    begin
      // Create an instance of the comparer
      Comparer := TOBDDataFrameComparer.Create;
      // Sort the frames by the order byte
      TArray.Sort<IOBDDataFrame>(Frames, Comparer);

      // Check contiguity
      for I := 1 to High(Frames) do
      begin
        // Exit if we received multiline response with missing frames
        if Frames[I].Data[2] <> Frames[I - 1].Data[2] + 1 then Exit;
      end;

      // Frames are now in order, so lets accumulate the data from each frame.
      // Preserve the first frame's mode and PID bytes (for consistency with CAN)

      // Exclude mode byte
      Msg.SetDataLength(Length(Frames[0].Data) - 1);
      // Copy the data
      Move(Frames[0].Data[1], Msg.Data[0], Length(Msg.Data));

      // Add the data from the remaining frames
      for I := 1 to High(Frames) do
      begin
        // Exclude mode/pid bytes
        FrameData := Copy(Frames[I].Data, 3, Length(Frames[I].Data) - 2);
        // Append frame to data
        Msg.Data := Msg.Data + FrameData;
      end;
    end;
  end;

  // If we get here, parsing succeeded
  Result := True;
end;

//------------------------------------------------------------------------------
// SAE J1850 PWM: GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TSAE_J1850_PWM_OBDProtocol.GetName: string;
begin
  Result := 'SAE J1850 PWM';
end;

//------------------------------------------------------------------------------
// SAE J1850 PWM: GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TSAE_J1850_PWM_OBDProtocol.GetDisplayName: string;
begin
  Result := 'SAE J1850 Pulse Width Modulated (41.6 kbit/s, 2 Wire K + L-Line)';
end;

//------------------------------------------------------------------------------
// SAE J1850 PWM: GET ELM ID
//------------------------------------------------------------------------------
function TSAE_J1850_PWM_OBDProtocol.GetELMID: string;
begin
  Result := '1';
end;

//------------------------------------------------------------------------------
// SAE J1850 VPW: GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TSAE_J1850_VPW_OBDProtocol.GetName: string;
begin
  Result := 'SAE J1850 VPW';
end;

//------------------------------------------------------------------------------
// SAE-J1850 VPW: GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TSAE_J1850_VPW_OBDProtocol.GetDisplayName: string;
begin
  Result := 'SAE J1850 Variable Pulse Width (10.4 kbit/s, Single Wire K-Line)';
end;

//------------------------------------------------------------------------------
// SAE-J1850 VPW: GET ELM ID
//------------------------------------------------------------------------------
function TSAE_J1850_VPW_OBDProtocol.GetELMID: string;
begin
  Result := '2';
end;

//------------------------------------------------------------------------------
// ISO 9141-2: GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_9141_2_OBDProtocol.GetName: string;
begin
  Result := 'ISO 9141-2';
end;

//------------------------------------------------------------------------------
// ISO 9141-2: GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_9141_2_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 9141-2 (5-baud init, 10.4 kbit/s, Single Wire K-Line)';
end;

//------------------------------------------------------------------------------
// ISO 9141-2: GET ELM ID
//------------------------------------------------------------------------------
function TISO_9141_2_OBDProtocol.GetELMID: string;
begin
  Result := '3';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP 5BAUD): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_14230_4_5BAUD_OBDProtocol.GetName: string;
begin
  Result := 'ISO 14230-4 (KWP 5BAUD)';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP 5BAUD): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_14230_4_5BAUD_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 14230-4 (Keyword Protocol 2000, 5-baud init, 10.4 kbit/s, Single Wire K-Line)';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP 5BAUD): GET ELM ID
//------------------------------------------------------------------------------
function TISO_14230_4_5BAUD_OBDProtocol.GetELMID: string;
begin
  Result := '4';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP FAST): GET PROTOCOL NAME
//------------------------------------------------------------------------------
function TISO_14230_4_FAST_OBDProtocol.GetName: string;
begin
  Result := 'ISO 14230-4 (KWP FAST)';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP FAST): GET PROTOCOL NAME WITH ADDITIONAL DATA
//------------------------------------------------------------------------------
function TISO_14230_4_FAST_OBDProtocol.GetDisplayName: string;
begin
  Result := 'ISO 14230-4 (Keyword Protocol 2000, Fast init, 10.4 kbit/s, Single Wire K-Line)';
end;

//------------------------------------------------------------------------------
// ISO 14230-4 (KWP FAST): GET ELM ID
//------------------------------------------------------------------------------
function TISO_14230_4_FAST_OBDProtocol.GetELMID: string;
begin
  Result := '5';
end;

end.
