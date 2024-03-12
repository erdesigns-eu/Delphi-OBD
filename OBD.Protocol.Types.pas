//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.Types.pas
// CONTENTS       : OBD Protocol Types
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Protocol.Types;

interface

uses WinApi.Windows, System.Classes, System.SysUtils;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Data Frame (INTERFACE)
  /// </summary>
  IOBDDataFrame = interface
    ['{4F4318BD-2108-43D4-BA4D-7428E21C9710}']
    /// <summary>
    ///   Gets the raw data of the frame.
    /// </summary>
    function GetRaw: string;
    /// <summary>
    ///   Sets the raw data of the frame.
    /// </summary>
    procedure SetRaw(Value: string);
    /// <summary>
    ///   Gets the data payload of the frame.
    /// </summary>
    function GetData: TBytes;
    /// <summary>
    ///   Sets the data payload of the frame.
    /// </summary>
    procedure SetData(Value: TBytes);
    /// <summary>
    ///   Gets the priority of the frame.
    /// </summary>
    function GetPriority: Integer;
    /// <summary>
    ///   Sets the priority of the frame.
    /// </summary>
    procedure SetPriority(Value: Integer);
    /// <summary>
    ///   Gets the address mode of the frame.
    /// </summary>
    function GetAddrMode: Integer;
    /// <summary>
    ///   Sets the address mode of the frame.
    /// </summary>
    procedure SetAddrMode(Value: Integer);
    /// <summary>
    ///   Gets the receiver identifier of the frame.
    /// </summary>
    function GetRxId: Integer;
    /// <summary>
    ///   Sets the receiver identifier of the frame.
    /// </summary>
    procedure SetRxId(Value: Integer);
    /// <summary>
    ///   Gets the transmitter identifier of the frame.
    /// </summary>
    function GetTxId: Integer;
    /// <summary>
    ///   Sets the transmitter identifier of the frame.
    /// </summary>
    procedure SetTxId(Value: Integer);
    /// <summary>
    ///   Gets the type of the frame.
    /// </summary>
    function GetFrameType: Byte;
    /// <summary>
    ///   Sets the type of the frame.
    /// </summary>
    procedure SetFrameType(Value: Byte);
    /// <summary>
    ///   Gets the sequence index of the frame.
    /// </summary>
    function GetSeqIndex: Integer;
    /// <summary>
    ///   Sets the sequence index of the frame.
    /// </summary>
    procedure SetSeqIndex(Value: Integer);
    /// <summary>
    ///   Gets the length of the data in the frame.
    /// </summary>
    function GetDataLength: Integer;
    /// <summary>
    ///   Sets the length of the data in the frame.
    /// </summary>
    procedure SetDataLength(Value: Integer);
    /// <summary>
    ///   Raw data of the frame.
    /// </summary>
    property Raw: string read GetRaw;
    /// <summary>
    ///   Data payload of the frame.
    /// </summary>
    property Data: TBytes read GetData write SetData;
    /// <summary>
    ///   Priority of the frame.
    /// </summary>
    property Priority: Integer read GetPriority write SetPriority;
    /// <summary>
    ///   Address mode of the frame.
    /// </summary>
    property AddrMode: Integer read GetAddrMode write SetAddrMode;
    /// <summary>
    ///   Receiver identifier of the frame.
    /// </summary>
    property RxId: Integer read GetRxId write SetRxId;
    /// <summary>
    ///   Transmitter identifier of the frame.
    /// </summary>
    property TxId: Integer read GetTxId write SetTxId;
    /// <summary>
    ///   Type of the frame.
    /// </summary>
    property FrameType: Byte read GetFrameType write SetFrameType;
    /// <summary>
    ///   Sequence index of the frame.
    /// </summary>
    property SeqIndex: Integer read GetSeqIndex write SetSeqIndex;
    /// <summary>
    ///   Length of the data in the frame.
    /// </summary>
    property DataLength: Integer read GetDataLength write SetDataLength;
  end;

  /// <summary>
  ///   OBD Data Message (INTERFACE)
  /// </summary>
  IOBDDataMessage = interface
    ['{24207F32-9B74-4D4C-8824-0A7E0F01B3F3}']
    /// <summary>
    ///   Gets the frames associated with the message.
    /// </summary>
    function GetFrames: TArray<IOBDDataFrame>;
    /// <summary>
    ///   Gets the ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    function GetEcu: string;
    /// <summary>
    ///   Sets the ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    procedure SetEcu(Value: string);
    /// <summary>
    ///   Gets the data payload of the message.
    /// </summary>
    function GetData: TBytes;
    /// <summary>
    ///   Sets the data payload of the message.
    /// </summary>
    procedure SetData(Value: TBytes);
    /// <summary>
    ///   Sets the length of the data payload of the message.
    /// </summary>
    procedure SetDataLength(Value: Integer);
    /// <summary>
    ///   Gets the transmitter ID of the message.
    /// </summary>
    function GetTxId: Integer;
    /// <summary>
    ///   Sets the transmitter ID of the message.
    /// </summary>
    procedure SetTxId(Value: Integer);
    /// <summary>
    ///   Converts the message data to hexadecimal format.
    /// </summary>
    function Hex: string;
    /// <summary>
    ///   Returns the original raw input string from the adapter.
    /// </summary>
    function Raw: string;
    /// <summary>
    ///   Indicates whether the message was successfully parsed.
    /// </summary>
    function Parsed: Boolean;
    /// <summary>
    ///   Checks if the message is equal to another message.
    /// </summary>
    function Equals(Msg: IOBDDataMessage): Boolean;
    /// <summary>
    ///   Frames associated with the message.
    /// </summary>
    property Frames: TArray<IOBDDataFrame> read GetFrames;
    /// <summary>
    ///   ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    property ECU: string read GetECU write SetECU;
    /// <summary>
    ///   Data payload of the message.
    /// </summary>
    property Data: TBytes read GetData write SetData;
    /// <summary>
    ///   Transmitter ID of the message.
    /// </summary>
    property TxId: Integer read GetTxId write SetTxId;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Data Frame (CLASS)
  /// </summary>
  TOBDDataFrame = class(TInterfacedObject, IOBDDataFrame)
  private
    /// <summary>
    ///   Raw data of the frame.
    /// </summary>
    FRaw: string;
    /// <summary>
    ///   Data payload of the frame.
    /// </summary>
    FData: TBytes;
    /// <summary>
    ///   Priority of the frame.
    /// </summary>
    FPriority: Integer;
    /// <summary>
    ///   Address mode of the frame.
    /// </summary>
    FAddrMode: Integer;
    /// <summary>
    ///   Receiver identifier of the frame.
    /// </summary>
    FRxId: Integer;
    /// <summary>
    ///   Transmitter identifier of the frame.
    /// </summary>
    FTxId: Integer;
    /// <summary>
    ///   Type of the frame.
    /// </summary>
    FFrameType: Byte;
    /// <summary>
    ///   Sequence index of the frame.
    /// </summary>
    FSeqIndex: Integer;
    /// <summary>
    ///   Length of the data.
    /// </summary>
    FDataLength: Integer;
  protected
    /// <summary>
    ///   Gets the raw data of the frame.
    /// </summary>
    function GetRaw: string;
    /// <summary>
    ///   Sets the raw data of the frame.
    /// </summary>
    procedure SetRaw(Value: string);
    /// <summary>
    ///   Gets the data payload of the frame.
    /// </summary>
    function GetData: TBytes;
    /// <summary>
    ///   Sets the data payload of the frame.
    /// </summary>
    procedure SetData(Value: TBytes);
    /// <summary>
    ///   Gets the priority of the frame.
    /// </summary>
    function GetPriority: Integer;
    /// <summary>
    ///   Sets the priority of the frame.
    /// </summary>
    procedure SetPriority(Value: Integer);
    /// <summary>
    ///   Gets the address mode of the frame.
    /// </summary>
    function GetAddrMode: Integer;
    /// <summary>
    ///   Sets the address mode of the frame.
    /// </summary>
    procedure SetAddrMode(Value: Integer);
    /// <summary>
    ///   Gets the receiver identifier of the frame.
    /// </summary>
    function GetRxId: Integer;
    /// <summary>
    ///   Sets the receiver identifier of the frame.
    /// </summary>
    procedure SetRxId(Value: Integer);
    /// <summary>
    ///   Gets the transmitter identifier of the frame.
    /// </summary>
    function GetTxId: Integer;
    /// <summary>
    ///   Sets the transmitter identifier of the frame.
    /// </summary>
    procedure SetTxId(Value: Integer);
    /// <summary>
    ///   Gets the type of the frame.
    /// </summary>
    function GetFrameType: Byte;
    /// <summary>
    ///   Sets the type of the frame.
    /// </summary>
    procedure SetFrameType(Value: Byte);
    /// <summary>
    ///   Gets the sequence index of the frame.
    /// </summary>
    function GetSeqIndex: Integer;
    /// <summary>
    ///   Sets the sequence index of the frame.
    /// </summary>
    procedure SetSeqIndex(Value: Integer);
    /// <summary>
    ///   Gets the length of the data in the frame.
    /// </summary>
    function GetDataLength: Integer;
    /// <summary>
    ///   Sets the length of the data in the frame.
    /// </summary>
    procedure SetDataLength(Value: Integer);
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Raw: string); virtual;

    /// <summary>
    ///   Raw data of the frame.
    /// </summary>
    property Raw: string read GetRaw write SetRaw;
    /// <summary>
    ///   Data payload of the frame.
    /// </summary>
    property Data: TBytes read GetData write SetData;
    /// <summary>
    ///   Priority of the frame.
    /// </summary>
    property Priority: Integer read GetPriority write SetPriority;
    /// <summary>
    ///   Address mode of the frame.
    /// </summary>
    property AddrMode: Integer read GetAddrMode write SetAddrMode;
    /// <summary>
    ///   Receiver identifier of the frame.
    /// </summary>
    property RxId: Integer read GetRxId write SetRxId;
    /// <summary>
    ///   Transmitter identifier of the frame.
    /// </summary>
    property TxId: Integer read GetTxId write SetTxId;
    /// <summary>
    ///   Type of the frame.
    /// </summary>
    property FrameType: Byte read GetFrameType write SetFrameType;
    /// <summary>
    ///   Sequence index of the frame.
    /// </summary>
    property SeqIndex: Integer read GetSeqIndex write SetSeqIndex;
    /// <summary>
    ///   Length of the data in the frame.
    /// </summary>
    property DataLength: Integer read GetDataLength write SetDataLength;
  end;

  /// <summary>
  ///   OBD Data Message (CLASS)
  /// </summary>
  TOBDDataMessage = class(TInterfacedObject, IOBDDataMessage)
  private
    /// <summary>
    ///   Frames associated with the message.
    /// </summary>
    FFrames: TArray<IOBDDataFrame>;
    /// <summary>
    ///   ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    FECU: string;
    /// <summary>
    ///   Data payload of the message.
    /// </summary>
    FData: TBytes;
    /// <summary>
    ///   Transmitter ID of the message.
    /// </summary>
    FTxId: Integer;
  protected
    /// <summary>
    ///   Gets the frames associated with the message.
    /// </summary>
    function GetFrames: TArray<IOBDDataFrame>;
    /// <summary>
    ///   Gets the ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    function GetEcu: string;
    /// <summary>
    ///   Sets the ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    procedure SetEcu(Value: string);
    /// <summary>
    ///   Gets the data payload of the message.
    /// </summary>
    function GetData: TBytes;
    /// <summary>
    ///   Sets the data payload of the message.
    /// </summary>
    procedure SetData(Value: TBytes);
    /// <summary>
    ///   Sets the length of the data payload of the message.
    /// </summary>
    procedure SetDataLength(Value: Integer);
    /// <summary>
    ///   Gets the transmitter ID of the message.
    /// </summary>
    function GetTxId: Integer;
    /// <summary>
    ///   Sets the transmitter ID of the message.
    /// </summary>
    procedure SetTxId(Value: Integer);
    /// <summary>
    ///   Converts the message data to hexadecimal format.
    /// </summary>
    function Hex: string;
    /// <summary>
    ///   Returns the original raw input string from the interface.
    /// </summary>
    function Raw: string;
    /// <summary>
    ///   Indicates whether the message was successfully parsed.
    /// </summary>
    function Parsed: Boolean;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Frames: TArray<IOBDDataFrame>); virtual;

    /// <summary>
    ///   Checks if the message is equal to another message.
    /// </summary>
    function Equals(Msg: IOBDDataMessage): Boolean;

    /// <summary>
    ///   Frames associated with the message.
    /// </summary>
    property Frames: TArray<IOBDDataFrame> read GetFrames;
    /// <summary>
    ///   ECU (Electronic Control Unit) associated with the message.
    /// </summary>
    property ECU: string read GetECU write SetECU;
    /// <summary>
    ///   Data payload of the message.
    /// </summary>
    property Data: TBytes read GetData write SetData;
    /// <summary>
    ///   Transmitter ID of the message.
    /// </summary>
    property TxId: Integer read GetTxId write SetTxId;
  end;

implementation

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDDataFrame.Create(Raw: string);
begin
  FRaw := Raw;
  FData := nil;
  FPriority := 0;
  FAddrMode := 0;
  FRxId := 0;
  FTxId := 0;
  FFrameType := $00;
  FSeqIndex := 0;
end;

//------------------------------------------------------------------------------
// GET RAW DATA
//------------------------------------------------------------------------------
function TOBDDataFrame.GetRaw: string;
begin
  Result := FRaw;
end;

//------------------------------------------------------------------------------
// SET RAW DATA
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetRaw(Value: string);
begin
  FRaw := Value;
end;

//------------------------------------------------------------------------------
// GET DATA
//------------------------------------------------------------------------------
function TOBDDataFrame.GetData: TBytes;
begin
  Result := FData;
end;

//------------------------------------------------------------------------------
// SET DATA
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetData(Value: TBytes);
begin
  FData := Value;
end;

//------------------------------------------------------------------------------
// GET PRIORITY
//------------------------------------------------------------------------------
function TOBDDataFrame.GetPriority: Integer;
begin
  Result := FPriority;
end;

//------------------------------------------------------------------------------
// SET PRIORITY
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetPriority(Value: Integer);
begin
  FPriority := Value;
end;

//------------------------------------------------------------------------------
// GET ADDRESS MODE
//------------------------------------------------------------------------------
function TOBDDataFrame.GetAddrMode: Integer;
begin
  Result := FAddrMode;
end;

//------------------------------------------------------------------------------
// SET ADDRESS MODE
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetAddrMode(Value: Integer);
begin
  FAddrMode := Value;
end;

//------------------------------------------------------------------------------
// GET RX ID
//------------------------------------------------------------------------------
function TOBDDataFrame.GetRxId: Integer;
begin
  Result := FRxId;
end;

//------------------------------------------------------------------------------
// SET RX ID
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetRxId(Value: Integer);
begin
  FRxId := Value;
end;

//------------------------------------------------------------------------------
// GET TX ID
//------------------------------------------------------------------------------
function TOBDDataFrame.GetTxId: Integer;
begin
  Result := FTxId;
end;

//------------------------------------------------------------------------------
// SET TX ID
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetTxId(Value: Integer);
begin
  FTxId := Value;
end;

//------------------------------------------------------------------------------
// GET FRAME TYPE
//------------------------------------------------------------------------------
function TOBDDataFrame.GetFrameType: Byte;
begin
  Result := FFrameType;
end;

//------------------------------------------------------------------------------
// SET FRAME TYPE
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetFrameType(Value: Byte);
begin
  FFrameType := Value;
end;

//------------------------------------------------------------------------------
// GET SEQUENCE INDEX
//------------------------------------------------------------------------------
function TOBDDataFrame.GetSeqIndex: Integer;
begin
  Result := FSeqIndex;
end;

//------------------------------------------------------------------------------
// SET SEQUENCE INDEX
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetSeqIndex(Value: Integer);
begin
  FSeqIndex := Value;
end;

//------------------------------------------------------------------------------
// GET DATA LENGTH
//------------------------------------------------------------------------------
function TOBDDataFrame.GetDataLength: Integer;
begin
  Result := FDataLength;
end;

//------------------------------------------------------------------------------
// SET DATA LENGTH
//------------------------------------------------------------------------------
procedure TOBDDataFrame.SetDataLength(Value: Integer);
begin
  FDataLength := Value;
end;

//------------------------------------------------------------------------------
// GET FRAMES
//------------------------------------------------------------------------------
function TOBDDataMessage.GetFrames: TArray<IOBDDataFrame>;
begin
  Result := FFrames;
end;

//------------------------------------------------------------------------------
// GET ECU
//------------------------------------------------------------------------------
function TOBDDataMessage.GetEcu: string;
begin
  Result := FECU;
end;

//------------------------------------------------------------------------------
// SET ECU
//------------------------------------------------------------------------------
procedure TOBDDataMessage.SetEcu(Value: string);
begin
  FECU := Value;
end;

//------------------------------------------------------------------------------
// GET DATA
//------------------------------------------------------------------------------
function TOBDDataMessage.GetData: TBytes;
begin
  Result := FData;
end;

//------------------------------------------------------------------------------
// SET DATA
//------------------------------------------------------------------------------
procedure TOBDDataMessage.SetData(Value: TBytes);
begin
  FData := Value;
end;

//------------------------------------------------------------------------------
// SET DATA LENGTH
//------------------------------------------------------------------------------
procedure TOBDDataMessage.SetDataLength(Value: Integer);
begin
  SetLength(FData, Value);
end;

//------------------------------------------------------------------------------
// GET TRANSMITTER ID
//------------------------------------------------------------------------------
function TOBDDataMessage.GetTxId: Integer;
begin
  Result := FTxId;
end;

//------------------------------------------------------------------------------
// SET TRANSMITTER ID
//------------------------------------------------------------------------------
procedure TOBDDataMessage.SetTxId(Value: Integer);
begin
  FTxId := Value;
end;

//------------------------------------------------------------------------------
// GET DATA AS HEX
//------------------------------------------------------------------------------
function TOBDDataMessage.Hex: string;
var
  B: Byte;
begin
  Result := '';
  for B in FData do Result := Result + IntToHex(B, 2);
end;

//------------------------------------------------------------------------------
// GET RAW DATA
//------------------------------------------------------------------------------
function TOBDDataMessage.Raw: string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(FFrames) to High(FFrames) do Result := Result + FFrames[I].Raw + #10;
end;

//------------------------------------------------------------------------------
// GET PARSED SUCCESS
//------------------------------------------------------------------------------
function TOBDDataMessage.Parsed: Boolean;
begin
  Result := Length(FData) > 0;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDDataMessage.Create(Frames: TArray<IOBDDataFrame>);
begin
  FFrames := Frames;
end;

//------------------------------------------------------------------------------
// CHECK IF MESSAGE IS EQUAL
//------------------------------------------------------------------------------
function TOBDDataMessage.Equals(Msg: IOBDDataMessage): Boolean;

  function CompareData(Data1, Data2: TBytes): Integer;
  var
    I: Integer;
  begin
    // Initialize with 0 differences
    Result := 0;
    // Loop over the data
    for I := Low(Data1) to High(Data1) do
    if Data1[I] <> Data2[I] then Inc(Result);
  end;

var
  IsSameECU, IsSameData, IsSameFrames: Boolean;
begin
  // Check if the ECU is the same
  IsSameECU := CompareText(Msg.ECU, FECU) = 0;
  // Check if the data is the same
  IsSameData := (Length(Msg.Data) = Length(FData)) and (CompareData(Msg.Data, FData) = 0);
  // Check the frames are the same
  IsSameFrames := Length(Msg.Frames) = Length(FFrames);
  // Set the result
  Result := IsSameECU and IsSameData and IsSameFrames;
end;

end.
