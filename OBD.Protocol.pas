﻿//------------------------------------------------------------------------------
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
  ///   OBD Protocol (INTERFACE)
  /// </summary>
  IOBDProtocol = interface
    ['{D603AC35-7A02-4723-A52A-67809258AD0F}']
    /// <summary>
    ///   Invoke
    /// </summary>
    function Invoke(Lines: TStrings): TArray<IOBDDataMessage>;
    /// <summary>
    ///   Load ECU list
    /// </summary>
    procedure LoadECUList(Messages: TArray<IOBDDataMessage>);
    /// <summary>
    ///   Get the OBD Protocol name
    /// </summary>
    function GetName: string;
    /// <summary>
    ///   Get the OBD Protocol name with additional data
    /// </summary>
    function GetDisplayName: string;
    /// <summary>
    ///   Get the OBD Protocol ID (for ELM compatible interfaces)
    /// </summary>
    function GetELMID: string;
    /// <summary>
    ///   Parse a Data Frame
    /// </summary>
    function ParseFrame(Frame: IOBDDataFrame): Boolean;
    /// <summary>
    ///   Parse a Data Message
    /// </summary>
    function ParseMessage(Msg: IOBDDataMessage): Boolean;
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
    ///   List of available ECU's
    /// </summary>
    FECUList: TStringList;
    /// <summary>
    ///   Invoke
    /// </summary>
    function Invoke(Lines: TStrings): TArray<IOBDDataMessage>;
    /// <summary>
    ///   Load ECU list
    /// </summary>
    procedure LoadECUList(Messages: TArray<IOBDDataMessage>);
  protected
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
    constructor Create(Lines: TStrings); virtual;
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
function TOBDProtocol.Invoke(Lines: TStrings): TArray<IOBDDataMessage>;

  function IsHex(Line: string): Boolean;
  var
    Hex: Integer;
  begin
    Result := TryStrToInt('$' + Line, Hex);
  end;

var
  OBDLines, NonOBDLines: TStringList;
  Line: string;
  LineNoSpaces: string;
  Frame: IOBDDataFrame;
  Frames: TArray<IOBDDataFrame>;
  FramesByECU: TDictionary<Integer, TArray<IOBDDataFrame>>;
  ECU: Integer;
  Msg: IOBDDataMessage;
begin
  // Create temp stringlists for lines
  OBDLines := TStringList.Create;
  NonOBDLines := TStringList.Create;
  try
    // Loop over lines
    for Line in Lines do
    begin
      // Remove spaces
      LineNoSpaces := StringReplace(Line, ' ', '', [rfReplaceAll]);
      // If the string is valid HEX add it to the OBD lines stringlist
      if IsHex(LineNoSpaces) then
        OBDLines.Add(Line)
      // Otherwise add it to the non OBD lines stringlist
      else
        NonOBDLines.Add(Line);
    end;

    // Initialize frames length
    SetLength(Frames, 0);

    // Loop over OBD lines
    for Line in ObdLines do
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
      for Line in NonObdLines do
      begin
        // Give each line its own message object
        Result := Result + [TOBDDataMessage.Create([TOBDDataFrame.Create(Line)])];
      end;
    finally
      FramesByECU.Free;
    end;
  finally
    OBDLines.Free;
    NonOBDLines.Free;
  end;
end;

//------------------------------------------------------------------------------
// LOAD ECU LIST
//------------------------------------------------------------------------------
procedure TOBDProtocol.LoadECUList(Messages: TArray<IOBDDataMessage>);
var
  Msg: IOBDDataMessage;
begin
  // Clear ECU list
  FECUList.Clear;
  // Loop over messages
  for Msg in Messages do
  begin
    // If the message doesnt contain any data, continue
    if not Msg.Parsed then Continue;
    // Convert the TxId to hexadecimal string and add to the ECU list
    FECUList.Add(IntToHex(Msg.TxId, 2));
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDProtocol.Create(Lines: TStrings);
var
  Messages: TArray<IOBDDataMessage>;
begin
  // Call inherited Constructor
  inherited Create;
  // Create ECU list
  FECUList := TStringList.Create;
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
  FECUList.Free;
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