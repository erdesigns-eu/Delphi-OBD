//------------------------------------------------------------------------------
//  OBD.Coding.LabelFile.VAG
//
//  TOBDLabelFileVAG — parser for the VAG ".LBL" label-file format
//  used by VCDS, VCP, OBDeleven and similar VAG diagnostic tools.
//  An .LBL file describes a control unit's coding bytes in human
//  terms:
//
//    7,0,Front Fog Light Assist active
//    7,1,Cornering Light active
//    7,2-3,(0=Off, 1=Position Light, 2=Side Marker)
//    8,B0,Daytime Running Lights        ; byte 8, single byte field
//    Adp;1;Idle Speed Setpoint           ; adaptation channel 1
//
//  The format is line-oriented; each line is one of:
//
//    - Bit-position label:  byte,bit,description
//    - Bit-range label:     byte,bit-bit,description (or with values
//      "(0=Off, 1=On)" mixed in)
//    - Whole-byte label:    byte,Bx,description (value-table follows)
//    - Adaptation channel:  Adp;channel;description
//    - Comment:             ;...
//
//  This unit produces a flat array of TOBDVAGLabelEntry records;
//  hosts feed them into TOBDCodingVAG to drive a coding-edit UI.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - Ross-Tech "VAG-COM Label File Format" technical note
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit OBD.Coding.LabelFile.VAG;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Label entry kind.</summary>
  TOBDVAGLabelKind = (
    lkBitField,
    lkByteField,
    lkAdaptationChannel,
    lkLongCodingHelper,
    lkComment
  );

  /// <summary>Decoded label entry.</summary>
  TOBDVAGLabelEntry = record
    Kind: TOBDVAGLabelKind;
    /// <summary>Byte index (lkBitField / lkByteField).</summary>
    Byte_: Integer;
    /// <summary>Starting bit (lkBitField).</summary>
    BitStart: Integer;
    /// <summary>Ending bit (inclusive). Equals BitStart for a
    /// single-bit label.</summary>
    BitEnd: Integer;
    /// <summary>Adaptation channel number (lkAdaptationChannel).</summary>
    Channel: Integer;
    /// <summary>Description text.</summary>
    Description: string;
    /// <summary>Optional inline value table — pairs of
    /// <c>(raw, label)</c> parsed from "(0=Off, 1=On)".</summary>
    Values: TArray<TPair<Integer, string>>;
  end;

  /// <summary>Parsed label-file content.</summary>
  TOBDVAGLabelFile = record
    Header: string;
    Entries: TArray<TOBDVAGLabelEntry>;
  end;

  /// <summary>Stateless VAG label-file parser.</summary>
  TOBDLabelFileVAG = class
  public
    /// <summary>Parses the file contents.</summary>
    class function Parse(const ASource: string): TOBDVAGLabelFile; static;
    /// <summary>Reads a file from disk and parses it.</summary>
    class function ParseFile(const AFileName: string): TOBDVAGLabelFile; static;
  end;

implementation

uses
  System.StrUtils;

function TryParseInline(const AText: string;
  out AValues: TArray<TPair<Integer, string>>): Boolean;
var
  Open, Close: Integer;
  Inner, Pair: string;
  Tokens: TArray<string>;
  EqPos: Integer;
  Acc: TList<TPair<Integer, string>>;
  Raw: Integer;
begin
  Result := False;
  AValues := nil;
  Open := Pos('(', AText);
  Close := Pos(')', AText);
  if (Open = 0) or (Close <= Open + 1) then Exit;
  Inner := Copy(AText, Open + 1, Close - Open - 1);
  Tokens := SplitString(Inner, ',');
  Acc := TList<TPair<Integer, string>>.Create;
  try
    for Pair in Tokens do
    begin
      EqPos := Pos('=', Pair);
      if EqPos = 0 then Continue;
      if not TryStrToInt(Trim(Copy(Pair, 1, EqPos - 1)), Raw) then Continue;
      Acc.Add(TPair<Integer, string>.Create(
        Raw, Trim(Copy(Pair, EqPos + 1, MaxInt))));
    end;
    AValues := Acc.ToArray;
    Result := Length(AValues) > 0;
  finally
    Acc.Free;
  end;
end;

function ParseLine(const ARaw: string; out AOut: TOBDVAGLabelEntry): Boolean;
var
  Trimmed, ByteToken, BitToken, RestOfLine: string;
  Tokens: TArray<string>;
  DashPos, EndPos: Integer;
begin
  Result := False;
  AOut := Default(TOBDVAGLabelEntry);
  Trimmed := TrimLeft(ARaw);
  if Trimmed = '' then Exit;

  // Adaptation channel: "Adp;<channel>;<desc>"
  if StartsText('Adp', Trimmed) then
  begin
    Tokens := SplitString(Trimmed, ';');
    if Length(Tokens) < 3 then Exit;
    AOut.Kind := lkAdaptationChannel;
    if not TryStrToInt(Trim(Tokens[1]), AOut.Channel) then Exit;
    AOut.Description := Trim(Tokens[2]);
    Exit(True);
  end;

  // Long-coding helper marker: "LC,<byte>,<bit>,<desc>"
  if StartsText('LC,', Trimmed) then
  begin
    Tokens := SplitString(Trimmed, ',');
    if Length(Tokens) < 4 then Exit;
    AOut.Kind := lkLongCodingHelper;
    if not TryStrToInt(Trim(Tokens[1]), AOut.Byte_) then Exit;
    if not TryStrToInt(Trim(Tokens[2]), AOut.BitStart) then Exit;
    AOut.BitEnd := AOut.BitStart;
    AOut.Description := Trim(Tokens[3]);
    Exit(True);
  end;

  // Standard line: byte,bit-or-Bx,description
  Tokens := SplitString(Trimmed, ',');
  if Length(Tokens) < 3 then Exit;

  ByteToken := Trim(Tokens[0]);
  BitToken  := Trim(Tokens[1]);

  if not TryStrToInt(ByteToken, AOut.Byte_) then Exit;

  if (Length(BitToken) > 0) and CharInSet(BitToken[1], ['B', 'b']) then
  begin
    // Whole-byte label: BX, BY (not really hex — just a marker).
    AOut.Kind := lkByteField;
    AOut.BitStart := 0;
    AOut.BitEnd := 7;
  end
  else
  begin
    AOut.Kind := lkBitField;
    DashPos := Pos('-', BitToken);
    if DashPos > 0 then
    begin
      if not TryStrToInt(Copy(BitToken, 1, DashPos - 1), AOut.BitStart) then Exit;
      if not TryStrToInt(Copy(BitToken, DashPos + 1, MaxInt), AOut.BitEnd) then Exit;
    end
    else
    begin
      if not TryStrToInt(BitToken, AOut.BitStart) then Exit;
      AOut.BitEnd := AOut.BitStart;
    end;
  end;

  // Re-join the remaining tokens — descriptions can contain commas
  // inside parenthesised value tables, so we also strip any line-
  // ending semicolon comment.
  RestOfLine := '';
  for var I := 2 to High(Tokens) do
  begin
    if I > 2 then RestOfLine := RestOfLine + ',';
    RestOfLine := RestOfLine + Tokens[I];
  end;
  EndPos := Pos(';', RestOfLine);
  if EndPos > 0 then RestOfLine := Copy(RestOfLine, 1, EndPos - 1);

  AOut.Description := Trim(RestOfLine);
  TryParseInline(RestOfLine, AOut.Values);
  Result := True;
end;

class function TOBDLabelFileVAG.Parse(
  const ASource: string): TOBDVAGLabelFile;
var
  Lines: TArray<string>;
  Line: string;
  Trimmed: string;
  Acc: TList<TOBDVAGLabelEntry>;
  Entry: TOBDVAGLabelEntry;
  HeaderLines: TStringList;
begin
  Result := Default(TOBDVAGLabelFile);
  Lines := SplitString(StringReplace(ASource, #13#10, #10, [rfReplaceAll]), #10);
  Acc := TList<TOBDVAGLabelEntry>.Create;
  HeaderLines := TStringList.Create;
  try
    for Line in Lines do
    begin
      Trimmed := TrimLeft(Line);
      if Trimmed = '' then Continue;
      if (Length(Trimmed) > 0) and (Trimmed[1] = ';') then
      begin
        // Header lines / comments at the top of the file.
        if Acc.Count = 0 then
          HeaderLines.Add(Trim(Copy(Trimmed, 2, MaxInt)));
        Continue;
      end;
      if ParseLine(Line, Entry) then
        Acc.Add(Entry);
    end;
    Result.Header := HeaderLines.Text.Trim;
    Result.Entries := Acc.ToArray;
  finally
    HeaderLines.Free;
    Acc.Free;
  end;
end;

class function TOBDLabelFileVAG.ParseFile(
  const AFileName: string): TOBDVAGLabelFile;
begin
  Result := Parse(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

end.
