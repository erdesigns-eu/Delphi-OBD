//------------------------------------------------------------------------------
// UNIT           : OBD.StringHelpers.pas
// CONTENTS       : String Helper Functions for Memory Optimization
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/12/2025
//------------------------------------------------------------------------------
unit OBD.StringHelpers;

interface

uses
  System.SysUtils, System.Classes;

//------------------------------------------------------------------------------
// STRING BUILDER HELPERS
//------------------------------------------------------------------------------

/// <summary>
///   Build a hex string from byte array efficiently using TStringBuilder
/// </summary>
function BytesToHexString(const Bytes: TBytes): string; overload;

/// <summary>
///   Build a hex string from byte array with separator efficiently
/// </summary>
function BytesToHexString(const Bytes: TBytes; const Separator: string): string; overload;

/// <summary>
///   Convert hex string to byte array efficiently
/// </summary>
function HexStringToBytes(const HexStr: string): TBytes;

/// <summary>
///   Build a formatted message efficiently using TStringBuilder
/// </summary>
function BuildFormattedMessage(const Parts: array of string; const Separator: string = ' '): string;

/// <summary>
///   Join strings with separator efficiently
/// </summary>
function JoinStrings(const Strings: TArray<string>; const Separator: string): string;

/// <summary>
///   Split string by delimiter efficiently (avoid multiple allocations)
/// </summary>
function SplitString(const Text: string; const Delimiter: Char): TArray<string>;

//------------------------------------------------------------------------------
// CACHED STRING OPERATIONS
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Simple string cache for frequently used strings (like hex conversions)
  /// </summary>
  TStringCache = class
  private
    /// <summary>
    ///   Cache storage (key-value pairs)
    /// </summary>
    FCache: TStringList;
    /// <summary>
    ///   Maximum cache size
    /// </summary>
    FMaxSize: Integer;

  public
    /// <summary>
    ///   Constructor with optional max size
    /// </summary>
    constructor Create(MaxSize: Integer = 100);
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Get cached value or compute and cache it
    /// </summary>
    function GetOrCompute(const Key: string; ComputeFunc: TFunc<string>): string;

    /// <summary>
    ///   Clear cache
    /// </summary>
    procedure Clear;
  end;

implementation

//------------------------------------------------------------------------------
// BYTES TO HEX STRING
//------------------------------------------------------------------------------
function BytesToHexString(const Bytes: TBytes): string;
var
  Builder: TStringBuilder;
  I: Integer;
begin
  if Length(Bytes) = 0 then
    Exit('');

  Builder := TStringBuilder.Create(Length(Bytes) * 2);
  try
    for I := 0 to High(Bytes) do
      Builder.Append(IntToHex(Bytes[I], 2));
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//------------------------------------------------------------------------------
// BYTES TO HEX STRING WITH SEPARATOR
//------------------------------------------------------------------------------
function BytesToHexString(const Bytes: TBytes; const Separator: string): string;
var
  Builder: TStringBuilder;
  I: Integer;
begin
  if Length(Bytes) = 0 then
    Exit('');

  Builder := TStringBuilder.Create(Length(Bytes) * (2 + Length(Separator)));
  try
    for I := 0 to High(Bytes) do
    begin
      if I > 0 then
        Builder.Append(Separator);
      Builder.Append(IntToHex(Bytes[I], 2));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//------------------------------------------------------------------------------
// HEX STRING TO BYTES
//------------------------------------------------------------------------------
function HexStringToBytes(const HexStr: string): TBytes;
var
  Cleaned: string;
  I, Len: Integer;
begin
  // Remove spaces and non-hex characters
  Cleaned := '';
  for I := 1 to Length(HexStr) do
  begin
    if CharInSet(HexStr[I], ['0'..'9', 'A'..'F', 'a'..'f']) then
      Cleaned := Cleaned + HexStr[I];
  end;

  Len := Length(Cleaned) div 2;
  SetLength(Result, Len);

  for I := 0 to Len - 1 do
    Result[I] := StrToInt('$' + Copy(Cleaned, I * 2 + 1, 2));
end;

//------------------------------------------------------------------------------
// BUILD FORMATTED MESSAGE
//------------------------------------------------------------------------------
function BuildFormattedMessage(const Parts: array of string; const Separator: string): string;
var
  Builder: TStringBuilder;
  I, TotalLen: Integer;
begin
  if Length(Parts) = 0 then
    Exit('');

  // Calculate approximate size
  TotalLen := 0;
  for I := 0 to High(Parts) do
    Inc(TotalLen, Length(Parts[I]));
  Inc(TotalLen, Length(Parts) * Length(Separator));

  Builder := TStringBuilder.Create(TotalLen);
  try
    for I := 0 to High(Parts) do
    begin
      if I > 0 then
        Builder.Append(Separator);
      Builder.Append(Parts[I]);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//------------------------------------------------------------------------------
// JOIN STRINGS
//------------------------------------------------------------------------------
function JoinStrings(const Strings: TArray<string>; const Separator: string): string;
var
  Builder: TStringBuilder;
  I, TotalLen: Integer;
begin
  if Length(Strings) = 0 then
    Exit('');

  // Calculate approximate size
  TotalLen := 0;
  for I := 0 to High(Strings) do
    Inc(TotalLen, Length(Strings[I]));
  Inc(TotalLen, Length(Strings) * Length(Separator));

  Builder := TStringBuilder.Create(TotalLen);
  try
    for I := 0 to High(Strings) do
    begin
      if I > 0 then
        Builder.Append(Separator);
      Builder.Append(Strings[I]);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//------------------------------------------------------------------------------
// SPLIT STRING
//------------------------------------------------------------------------------
function SplitString(const Text: string; const Delimiter: Char): TArray<string>;
var
  List: TStringList;
  I: Integer;
begin
  List := TStringList.Create;
  try
    List.Delimiter := Delimiter;
    List.StrictDelimiter := True;
    List.DelimitedText := Text;
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  finally
    List.Free;
  end;
end;

//------------------------------------------------------------------------------
// STRING CACHE - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TStringCache.Create(MaxSize: Integer);
begin
  inherited Create;
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.Duplicates := dupIgnore;
  FMaxSize := MaxSize;
end;

//------------------------------------------------------------------------------
// STRING CACHE - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TStringCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// STRING CACHE - GET OR COMPUTE
//------------------------------------------------------------------------------
function TStringCache.GetOrCompute(const Key: string; ComputeFunc: TFunc<string>): string;
var
  Index: Integer;
begin
  Index := FCache.IndexOf(Key);
  if Index >= 0 then
  begin
    // Return cached value
    Result := FCache.ValueFromIndex[Index];
  end
  else
  begin
    // Compute and cache
    Result := ComputeFunc();
    
    // Check cache size and remove oldest if needed
    if FCache.Count >= FMaxSize then
      FCache.Delete(0);
    
    FCache.Values[Key] := Result;
  end;
end;

//------------------------------------------------------------------------------
// STRING CACHE - CLEAR
//------------------------------------------------------------------------------
procedure TStringCache.Clear;
begin
  FCache.Clear;
end;

end.
