//------------------------------------------------------------------------------
// UNIT           : OBD.Protocol.pas
// CONTENTS       : OBD Protocol Class
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/03/2024
//------------------------------------------------------------------------------
unit OBD.Protocol;

interface

uses
  WinApi.Windows, System.Classes, System.SysUtils, System.Generics.Defaults,
  System.Generics.Collections,

  OBD.Protocol.Types;

//------------------------------------------------------------------------------
// INTERFACES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Thread-aware protocol contract used by parser implementations to convert
  ///   adapter lines into decoded messages while exposing read-only snapshots of
  ///   parsed ECU data.
  /// </summary>
  IOBDProtocol = interface
    ['{D603AC35-7A02-4723-A52A-67809258AD0F}']
    /// <summary>
    ///   Parses the supplied text lines into data messages. Callers should pass
    ///   immutable snapshots gathered under a lock when running on background
    ///   threads.
    /// </summary>
    function Invoke(const Lines: TStrings): TArray<IOBDDataMessage>;
    /// <summary>
    ///   Consumes parsed messages to populate the ECU list. This method is
    ///   expected to run under the parser's internal lock.
    /// </summary>
    procedure LoadECUList(const Messages: TArray<IOBDDataMessage>);
    /// <summary>
    ///   Gets the OBD protocol name.
    /// </summary>
    function GetName: string;
    /// <summary>
    ///   Gets the OBD protocol name with additional data.
    /// </summary>
    function GetDisplayName: string;
    /// <summary>
    ///   Gets the OBD protocol ID (for ELM compatible interfaces).
    /// </summary>
    function GetELMID: string;
    /// <summary>
    ///   Parses a single data frame, returning true when it could be decoded
    ///   successfully.
    /// </summary>
    function ParseFrame(Frame: IOBDDataFrame): Boolean;
    /// <summary>
    ///   Parses a single data message, returning true when successful.
    /// </summary>
    function ParseMessage(Msg: IOBDDataMessage): Boolean;
    /// <summary>
    ///   Creates a thread-safe snapshot of loaded ECUs for read-only consumption.
    /// </summary>
    function SnapshotECUList: TArray<string>;
    /// <summary>
    ///   OBD Protocol name
    /// </summary>
    property Name: string read GetName;
    /// <summary>
    ///   OBD Protocol name with additional data
    /// </summary>
    property DisplayName: string read GetDisplayName;
    /// <summary>
    ///   ELM OBD ID
    /// </summary>
    property ELMID: string read GetELMID;
    /// <summary>
    ///   Thread-safe snapshot of loaded ECUs.
    /// </summary>
    property ECUListSnapshot: TArray<string> read SnapshotECUList;
  end;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Protocol (CLASS)
  /// </summary>
  TOBDProtocol = class(TInterfacedObject, IOBDProtocol)
  private
    /// <summary>
    ///   Monitor used to guard ECU list access and parsing scratch buffers.
    /// </summary>
    FStateLock: TObject;
    /// <summary>
    ///   Used in the invoke function
    /// </summary>
    FOBDLines: TStringList;
    /// <summary>
    ///   Used in the invoke function
    /// </summary>
    FNonOBDLines: TStringList;
    /// <summary>
    ///   List of available ECU's
    /// </summary>
    FECUList: TStringList;
    /// <summary>
    ///   Allow long messages (> 7 bytes)
    /// </summary>
    FAllowLongMessages: Boolean;
    /// <summary>
    ///   Partition incoming lines into OBD and non-OBD buckets while guarding shared lists.
    /// </summary>
    /// <param name="Lines">
    ///   Raw message lines returned from the adapter.
    /// </param>
    /// <param name="OBDLines">
    ///   Output array of validated hexadecimal OBD lines.
    /// </param>
    /// <param name="NonOBDLines">
    ///   Output array of non-OBD lines for diagnostic handling.
    /// </param>
    procedure BucketizeLines(const Lines: TStrings; out OBDLines, NonOBDLines: TArray<string>);
    /// <summary>
    ///   Invoke
    /// </summary>
    function Invoke(const Lines: TStrings): TArray<IOBDDataMessage>;
    /// <summary>
    ///   Load ECU list
    /// </summary>
    procedure LoadECUList(const Messages: TArray<IOBDDataMessage>);
    /// <summary>
    ///   Creates a thread-safe snapshot of loaded ECUs for read-only consumption.
    /// </summary>
    function SnapshotECUList: TArray<string>;
  protected
    /// <summary>
    ///   Convert a hex string to TBytes
    /// </summary>
    function HexStringToBytes(const HexString: string): TBytes;
    /// <summary>
    ///   Get the OBD Protocol friendlyname
    /// </summary>
    function GetName: string; virtual; abstract;
    /// <summary>
    ///   Get the OBD Protocol name with additional data
    /// </summary>
    function GetDisplayName: string; virtual; abstract;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string; virtual; abstract;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Lines: TStrings; AllowLongMessages: Boolean); virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Parse a Data Frame
    /// </summary>
    function ParseFrame(Frame: IOBDDataFrame): Boolean; virtual; abstract;
    /// <summary>
    ///   Parse a Data Message
    /// </summary>
    function ParseMessage(Msg: IOBDDataMessage): Boolean; virtual; abstract;

    /// <summary>
    ///   List of available ECU's
    /// </summary>
    property ECUList: TStringList read FECUList;
    /// <summary>
    ///   Allow long messages (> 7 bytes)
    /// </summary>
    property AllowLongMessages: Boolean read FAllowLongMessages write FAllowLongMessages;
    /// <summary>
    ///   OBD Protocol name
    /// </summary>
    property Name: string read GetName;
    /// <summary>
    ///   OBD Protocol name with additional data
    /// </summary>
    property DisplayName: string read GetDisplayName;
    /// <summary>
    ///   ELM OBD ID
    /// </summary>
    property ELMID: string read GetELMID;
  end;

  /// <summary>
  ///   Comparer for sorting Data Frames (Legacy)
  /// </summary>
  TOBDDataFrameComparer = class(TInterfacedObject, IComparer<IOBDDataFrame>)
  public
    function Compare(const Left, Right: IOBDDataFrame): Integer;
  end;

  /// <summary>
  ///   Comparer for sorting Data Frames (CAN)
  /// </summary>
  TOBDDataFrameSequenceComparer = class(TInterfacedObject, IComparer<IOBDDataFrame>)
  public
    function Compare(const Left, Right: IOBDDataFrame): Integer;
  end;

implementation

//------------------------------------------------------------------------------
// INVOKE
//------------------------------------------------------------------------------
procedure TOBDProtocol.BucketizeLines(const Lines: TStrings; out OBDLines, NonOBDLines: TArray<string>);

  function IsHex(const Line: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if Line = '' then Exit;
    for I := 1 to Length(Line) do
      if not CharInSet(Line[I], ['0'..'9', 'A'..'F', 'a'..'f']) then Exit;
    Result := True;
  end;

var
  Line: string;
  LineNoSpaces: string;
begin
  OBDLines := nil;
  NonOBDLines := nil;
  TMonitor.Enter(FStateLock);
  try
    FOBDLines.Clear;
    FNonOBDLines.Clear;
    for Line in Lines do
    begin
      LineNoSpaces := StringReplace(Line, ' ', '', [rfReplaceAll]);
      if IsHex(LineNoSpaces) then
      begin
        FOBDLines.Add(Line);
        OBDLines := OBDLines + [Line];
      end
      else
      begin
        FNonOBDLines.Add(Line);
        NonOBDLines := NonOBDLines + [Line];
      end;
    end;
  finally
    TMonitor.Exit(FStateLock);
  end;
end;

function TOBDProtocol.Invoke(const Lines: TStrings): TArray<IOBDDataMessage>;

var
  Line: string;
  OBDLines: TArray<string>;
  NonOBDLines: TArray<string>;
  Frame: IOBDDataFrame;
  Frames: TArray<IOBDDataFrame>;
  FramesByECU: TDictionary<Integer, TArray<IOBDDataFrame>>;
  ECU: Integer;
  Msg: IOBDDataMessage;
begin
  BucketizeLines(Lines, OBDLines, NonOBDLines);

  // Initialize frames length
  SetLength(Frames, 0);

  // Loop over OBD lines
  for Line in OBDLines do
  begin
    // Create frame from line
    Frame := TOBDDataFrame.Create(Line);
    // Parse lines into frames, and drop lines that couldn't be parsed
    if ParseFrame(Frame) then Frames := Frames + [Frame];
  end;

  // Map frames by ECU
  FramesByECU := TDictionary<Integer, TArray<IOBDDataFrame>>.Create;
  try
    // Loop over frames
    for Frame in Frames do
    begin
      if not FramesByECU.ContainsKey(Frame.TxId) then
        FramesByECU.Add(Frame.TxId, [Frame])
      else
        FramesByECU[Frame.TxId] := FramesByECU[Frame.TxId] + [Frame];
    end;

    // Parse frames into whole messages
    for ECU in FramesByECU.Keys do
    begin
      Msg := TOBDDataMessage.Create(FramesByECU[ECU]);

      // Assemble frames into Messages
      if ParseMessage(Msg) then
      begin
        // Mark with the appropriate ECU ID
        Msg.ECU := IntToHex(ECU, 2);
        Result := Result + [Msg];
      end;
    end;

    // Handle invalid lines (probably from the ELM)
    for Line in NonOBDLines do
    begin
      // Give each line its own message object
      Result := Result + [TOBDDataMessage.Create([TOBDDataFrame.Create(Line)])];
    end;
  finally
    FramesByECU.Free;
  end;
end;

//------------------------------------------------------------------------------
// HEX STRING TO BYTES
//------------------------------------------------------------------------------
function TOBDProtocol.HexStringToBytes(const HexString: string): TBytes;
var
  I, Value: Integer;
begin
  // Exit here if the length is odd
  if Length(HexString) mod 2 <> 0 then Exit;
  // Set length of result
  SetLength(Result, Length(HexString) div 2);
  // Loop over hex string and convert to bytes
  for I := 0 to Length(Result) - 1 do
  begin
    Value := StrToInt('$' + Copy(HexString, I * 2 + 1, 2));
    Result[I] := Value;
  end;
end;

//------------------------------------------------------------------------------
// LOAD ECU LIST
//------------------------------------------------------------------------------
procedure TOBDProtocol.LoadECUList(const Messages: TArray<IOBDDataMessage>);
var
  Msg: IOBDDataMessage;
begin
  // Clear ECU list
  TMonitor.Enter(FStateLock);
  try
    FECUList.Clear;
    // Loop over messages
    for Msg in Messages do
    begin
      // If the message doesnt contain any data, continue
      if not Msg.Parsed then Continue;
      // Convert the TxId to hexadecimal string and add to the ECU list
      FECUList.Add(Msg.ECU);
    end;
  finally
    TMonitor.Exit(FStateLock);
  end;
end;

//------------------------------------------------------------------------------
// SNAPSHOT ECU LIST
//------------------------------------------------------------------------------
function TOBDProtocol.SnapshotECUList: TArray<string>;
var
  Index: Integer;
begin
  TMonitor.Enter(FStateLock);
  try
    SetLength(Result, FECUList.Count);
    for Index := 0 to FECUList.Count - 1 do
      Result[Index] := FECUList[Index];
  finally
    TMonitor.Exit(FStateLock);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDProtocol.Create(Lines: TStrings; AllowLongMessages: Boolean);
var
  Messages: TArray<IOBDDataMessage>;
begin
  // Call inherited Constructor
  inherited Create;
  // Create monitor lock
  FStateLock := TObject.Create;
  // Create ECU list
  FECUList := TStringList.Create;
  // Create lists
  FOBDLines := TStringList.Create;
  FNonOBDLines := TStringList.Create;
  // Set allow long messages
  FAllowLongMessages := AllowLongMessages;
  // Parse the 0100 data into messages
  Messages := Invoke(Lines);
  // Load ECU list
  LoadECUList(Messages);
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDProtocol.Destroy;
begin
  // Free ECU list
  TMonitor.Enter(FStateLock);
  try
    FECUList.Free;
    // Free lists
    FOBDLines.Free;
    FNonOBDLines.Free;
  finally
    TMonitor.Exit(FStateLock);
  end;
  // Release monitor lock
  FStateLock.Free;
  // Call inherited Destructor
  inherited;
end;

//------------------------------------------------------------------------------
// COMPARE FRAMES (LEGACY)
//------------------------------------------------------------------------------
function TOBDDataFrameComparer.Compare(const Left, Right: IOBDDataFrame): Integer;
begin
  Result := Left.Data[2] - Right.Data[2];
end;

//------------------------------------------------------------------------------
// COMPARE FRAMES (CAN)
//------------------------------------------------------------------------------
function TOBDDataFrameSequenceComparer.Compare(const Left, Right: IOBDDataFrame): Integer;
begin
  Result := Left.SeqIndex - Right.SeqIndex;
end;

end.
