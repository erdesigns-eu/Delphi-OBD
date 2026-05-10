//------------------------------------------------------------------------------
//  OBD.Protocol.LIN.LDF
//
//  Parser for the LIN Description File (LDF). LDF is the static
//  configuration carrier for a LIN cluster — it lists nodes,
//  signals, frames and schedule tables, plus signal encoding /
//  diagnostic info. This parser covers the structural sections that
//  drive the runtime (Nodes, Signals, Frames, Schedule_tables) and
//  skips unknown sections so a future spec extension does not break
//  parsing.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - LIN Specification 2.2A § 9 (LIN Description File)
//    - ISO 17987-1:2016 Annex B
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.LIN.LDF;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>Decoded LDF signal definition.</summary>
  TOBDLDFSignal = record
    /// <summary>Signal name.</summary>
    Name: string;
    /// <summary>Length in bits (1..64).</summary>
    SizeBits: Integer;
    /// <summary>Initial value (decimal / hex from the LDF).</summary>
    InitValue: Int64;
    /// <summary>Publisher node name.</summary>
    Publisher: string;
    /// <summary>Subscriber node names.</summary>
    Subscribers: TArray<string>;
  end;

  /// <summary>Signal placement inside a frame (signal name + bit
  /// offset from the start of the data field).</summary>
  TOBDLDFSignalPlacement = record
    SignalName: string;
    OffsetBits: Integer;
  end;

  /// <summary>Decoded LDF frame definition.</summary>
  TOBDLDFFrame = record
    /// <summary>Frame name.</summary>
    Name: string;
    /// <summary>Frame ID (0..63).</summary>
    FrameID: Byte;
    /// <summary>Publisher node name.</summary>
    Publisher: string;
    /// <summary>Frame data length in bytes (1..8).</summary>
    SizeBytes: Byte;
    /// <summary>Per-signal placement.</summary>
    Signals: TArray<TOBDLDFSignalPlacement>;
  end;

  /// <summary>One entry in a schedule table.</summary>
  TOBDLDFScheduleEntry = record
    /// <summary>Frame name to send.</summary>
    FrameName: string;
    /// <summary>Slot delay in milliseconds (LDF carries
    /// floating-point ms; we round to the nearest 0.001 ms and store
    /// as integer microseconds).</summary>
    DelayMicros: Integer;
  end;

  /// <summary>Decoded LDF schedule table.</summary>
  TOBDLDFSchedule = record
    Name: string;
    Entries: TArray<TOBDLDFScheduleEntry>;
  end;

  /// <summary>Decoded LDF cluster.</summary>
  TOBDLDFCluster = record
    /// <summary>LIN protocol version string (e.g. "2.2").</summary>
    ProtocolVersion: string;
    /// <summary>LIN language (LDF syntax) version string.</summary>
    LanguageVersion: string;
    /// <summary>Bus speed in bits per second.</summary>
    Speed: Cardinal;
    /// <summary>Master node name.</summary>
    Master: string;
    /// <summary>Slave node names.</summary>
    Slaves: TArray<string>;
    /// <summary>Signals indexed by name (case-sensitive, per LDF).</summary>
    Signals: TArray<TOBDLDFSignal>;
    /// <summary>Frames indexed by name.</summary>
    Frames: TArray<TOBDLDFFrame>;
    /// <summary>Schedule tables indexed by name.</summary>
    Schedules: TArray<TOBDLDFSchedule>;
  end;

  /// <summary>Stateless LDF parser.</summary>
  TOBDLDFParser = class
  public
    /// <summary>Parses an LDF source string into a cluster
    /// description.</summary>
    /// <param name="ASource">LDF text.</param>
    /// <returns>Decoded cluster.</returns>
    /// <exception cref="EOBDProtocol">Parse error (with line
    /// number).</exception>
    class function Parse(const ASource: string): TOBDLDFCluster; static;
    /// <summary>Reads a file from disk and parses it.</summary>
    /// <param name="AFileName">Path to .ldf.</param>
    class function ParseFile(const AFileName: string): TOBDLDFCluster; static;
  end;

implementation

type
  TTokenKind = (
    tkEOF,
    tkIdent,
    tkNumber,
    tkString,
    tkSymbol     // single-character punctuation: { } ; , : =
  );

  TToken = record
    Kind: TTokenKind;
    Text: string;
    Line: Integer;
  end;

  TLDFLexer = class
  strict private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
  public
    constructor Create(const ASource: string);
    function Next: TToken;
    function Peek: TToken;
  strict private
    FPeeked: Boolean;
    FPeek: TToken;
    procedure SkipWhitespaceAndComments;
  end;

constructor TLDFLexer.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
  FPos := 1;
  FLine := 1;
  FPeeked := False;
end;

procedure TLDFLexer.SkipWhitespaceAndComments;
begin
  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = #10 then begin Inc(FLine); Inc(FPos); Continue; end;
    if FSource[FPos] = #13 then begin Inc(FPos); Continue; end;
    if CharInSet(FSource[FPos], [' ', #9]) then begin Inc(FPos); Continue; end;
    if (FPos + 1 <= Length(FSource)) and (FSource[FPos] = '/') and
       (FSource[FPos + 1] = '/') then
    begin
      while (FPos <= Length(FSource)) and (FSource[FPos] <> #10) do Inc(FPos);
      Continue;
    end;
    if (FPos + 1 <= Length(FSource)) and (FSource[FPos] = '/') and
       (FSource[FPos + 1] = '*') then
    begin
      Inc(FPos, 2);
      while (FPos + 1 <= Length(FSource)) and
            not ((FSource[FPos] = '*') and (FSource[FPos + 1] = '/')) do
      begin
        if FSource[FPos] = #10 then Inc(FLine);
        Inc(FPos);
      end;
      if FPos + 1 <= Length(FSource) then Inc(FPos, 2);
      Continue;
    end;
    Break;
  end;
end;

function TLDFLexer.Next: TToken;
var
  StartPos: Integer;
  C: Char;
begin
  if FPeeked then
  begin
    Result := FPeek;
    FPeeked := False;
    Exit;
  end;
  SkipWhitespaceAndComments;
  Result.Line := FLine;
  if FPos > Length(FSource) then
  begin
    Result.Kind := tkEOF;
    Result.Text := '';
    Exit;
  end;
  C := FSource[FPos];
  // Identifier or keyword.
  if CharInSet(C, ['A'..'Z', 'a'..'z', '_']) then
  begin
    StartPos := FPos;
    Inc(FPos);
    while (FPos <= Length(FSource)) and
          CharInSet(FSource[FPos], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(FPos);
    Result.Kind := tkIdent;
    Result.Text := Copy(FSource, StartPos, FPos - StartPos);
    Exit;
  end;
  // Number (decimal / hex).
  if CharInSet(C, ['0'..'9']) or
     ((C = '-') and (FPos < Length(FSource)) and
      CharInSet(FSource[FPos + 1], ['0'..'9'])) then
  begin
    StartPos := FPos;
    if C = '-' then Inc(FPos);
    if (FPos + 1 <= Length(FSource)) and (FSource[FPos] = '0') and
       (CharInSet(FSource[FPos + 1], ['x', 'X'])) then
    begin
      Inc(FPos, 2);
      while (FPos <= Length(FSource)) and
            CharInSet(FSource[FPos], ['0'..'9', 'A'..'F', 'a'..'f']) do
        Inc(FPos);
    end
    else
    begin
      while (FPos <= Length(FSource)) and
            CharInSet(FSource[FPos], ['0'..'9', '.']) do
        Inc(FPos);
    end;
    Result.Kind := tkNumber;
    Result.Text := Copy(FSource, StartPos, FPos - StartPos);
    Exit;
  end;
  // String.
  if C = '"' then
  begin
    Inc(FPos);
    StartPos := FPos;
    while (FPos <= Length(FSource)) and (FSource[FPos] <> '"') do
    begin
      if FSource[FPos] = #10 then Inc(FLine);
      Inc(FPos);
    end;
    Result.Kind := tkString;
    Result.Text := Copy(FSource, StartPos, FPos - StartPos);
    if FPos <= Length(FSource) then Inc(FPos); // skip closing quote
    Exit;
  end;
  // Single-character symbol.
  Result.Kind := tkSymbol;
  Result.Text := C;
  Inc(FPos);
end;

function TLDFLexer.Peek: TToken;
begin
  if not FPeeked then
  begin
    FPeek := Next;
    FPeeked := True;
  end;
  Result := FPeek;
end;

procedure Expect(const ATok: TToken; AKind: TTokenKind; const AText: string);
begin
  if (ATok.Kind <> AKind) or
     ((AText <> '') and (not SameText(ATok.Text, AText))) then
    raise EOBDProtocol.CreateFmt(
      'LDF parse error at line %d: expected "%s", got "%s"',
      [ATok.Line, AText, ATok.Text]);
end;

procedure ExpectSymbol(const ATok: TToken; AChar: Char);
begin
  if (ATok.Kind <> tkSymbol) or (ATok.Text <> string(AChar)) then
    raise EOBDProtocol.CreateFmt(
      'LDF parse error at line %d: expected "%s", got "%s"',
      [ATok.Line, AChar, ATok.Text]);
end;

function ParseInt64(const AText: string): Int64;
var
  Cleaned: string;
  Negative: Boolean;
begin
  Cleaned := AText;
  Negative := (Length(Cleaned) > 0) and (Cleaned[1] = '-');
  if Negative then Cleaned := Copy(Cleaned, 2, MaxInt);
  if (Length(Cleaned) >= 2) and (Cleaned[1] = '0') and
     CharInSet(Cleaned[2], ['x', 'X']) then
    Result := StrToInt64('$' + Copy(Cleaned, 3, MaxInt))
  else
    Result := StrToInt64(Cleaned);
  if Negative then Result := -Result;
end;

function ParseFloatGeneric(const AText: string): Double;
var
  Fmt: TFormatSettings;
begin
  Fmt := TFormatSettings.Create('en-US');
  Result := StrToFloat(AText, Fmt);
end;

procedure SkipBalancedBraces(ALex: TLDFLexer);
var
  Depth: Integer;
  Tok: TToken;
begin
  Depth := 1;
  while Depth > 0 do
  begin
    Tok := ALex.Next;
    if Tok.Kind = tkEOF then
      raise EOBDProtocol.Create('LDF parse error: unbalanced "{"');
    if (Tok.Kind = tkSymbol) and (Tok.Text = '{') then Inc(Depth)
    else if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Dec(Depth);
  end;
end;

procedure SkipUntilSemicolon(ALex: TLDFLexer);
var
  Tok: TToken;
begin
  while True do
  begin
    Tok := ALex.Next;
    if Tok.Kind = tkEOF then Exit;
    if (Tok.Kind = tkSymbol) and (Tok.Text = ';') then Exit;
  end;
end;

procedure ParseHeader(ALex: TLDFLexer; var ACluster: TOBDLDFCluster);
var
  Tok: TToken;
  Speed: Double;
begin
  // Optional opening "LIN_description_file ;"
  Tok := ALex.Peek;
  if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'LIN_description_file') then
  begin
    ALex.Next;
    Tok := ALex.Next; ExpectSymbol(Tok, ';');
  end;
  while True do
  begin
    Tok := ALex.Peek;
    if Tok.Kind = tkEOF then Exit;
    if (Tok.Kind = tkIdent) and
       (SameText(Tok.Text, 'Nodes') or
        SameText(Tok.Text, 'Signals') or
        SameText(Tok.Text, 'Diagnostic_signals') or
        SameText(Tok.Text, 'Frames') or
        SameText(Tok.Text, 'Sporadic_frames') or
        SameText(Tok.Text, 'Event_triggered_frames') or
        SameText(Tok.Text, 'Diagnostic_frames') or
        SameText(Tok.Text, 'Node_attributes') or
        SameText(Tok.Text, 'Schedule_tables') or
        SameText(Tok.Text, 'Signal_encoding_types') or
        SameText(Tok.Text, 'Signal_representation')) then
      Exit;
    ALex.Next;
    if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'LIN_protocol_version') then
    begin
      Tok := ALex.Next; ExpectSymbol(Tok, '=');
      Tok := ALex.Next;
      if Tok.Kind = tkString then ACluster.ProtocolVersion := Tok.Text;
      SkipUntilSemicolon(ALex);
    end
    else if (Tok.Kind = tkIdent) and
            SameText(Tok.Text, 'LIN_language_version') then
    begin
      Tok := ALex.Next; ExpectSymbol(Tok, '=');
      Tok := ALex.Next;
      if Tok.Kind = tkString then ACluster.LanguageVersion := Tok.Text;
      SkipUntilSemicolon(ALex);
    end
    else if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'LIN_speed') then
    begin
      Tok := ALex.Next; ExpectSymbol(Tok, '=');
      Tok := ALex.Next;
      Speed := 0;
      if Tok.Kind = tkNumber then Speed := ParseFloatGeneric(Tok.Text);
      // Optional unit "kbps".
      Tok := ALex.Peek;
      if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'kbps') then
      begin
        ALex.Next;
        ACluster.Speed := Round(Speed * 1000);
      end
      else
        ACluster.Speed := Round(Speed);
      SkipUntilSemicolon(ALex);
    end
    else if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'Channel_name') then
    begin
      Tok := ALex.Next; ExpectSymbol(Tok, '=');
      SkipUntilSemicolon(ALex);
    end
    else
      SkipUntilSemicolon(ALex);
  end;
end;

procedure ParseNodes(ALex: TLDFLexer; var ACluster: TOBDLDFCluster);
var
  Tok: TToken;
  Slaves: TList<string>;
begin
  Tok := ALex.Next; Expect(Tok, tkIdent, 'Nodes');
  Tok := ALex.Next; ExpectSymbol(Tok, '{');
  Slaves := TList<string>.Create;
  try
    while True do
    begin
      Tok := ALex.Next;
      if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
      if Tok.Kind = tkEOF then
        raise EOBDProtocol.Create('LDF: unterminated Nodes block');
      if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'Master') then
      begin
        Tok := ALex.Next; ExpectSymbol(Tok, ':');
        Tok := ALex.Next; Expect(Tok, tkIdent, '');
        ACluster.Master := Tok.Text;
        SkipUntilSemicolon(ALex);
      end
      else if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'Slaves') then
      begin
        Tok := ALex.Next; ExpectSymbol(Tok, ':');
        while True do
        begin
          Tok := ALex.Next;
          if (Tok.Kind = tkSymbol) and (Tok.Text = ';') then Break;
          if (Tok.Kind = tkSymbol) and (Tok.Text = ',') then Continue;
          if Tok.Kind = tkIdent then Slaves.Add(Tok.Text);
          if Tok.Kind = tkEOF then
            raise EOBDProtocol.Create('LDF: unterminated Slaves entry');
        end;
      end
      else
        SkipUntilSemicolon(ALex);
    end;
    ACluster.Slaves := Slaves.ToArray;
  finally
    Slaves.Free;
  end;
end;

procedure ParseSignals(ALex: TLDFLexer; var ACluster: TOBDLDFCluster);
var
  Tok: TToken;
  Sig: TOBDLDFSignal;
  Subs: TList<string>;
  Sigs: TList<TOBDLDFSignal>;
begin
  Tok := ALex.Next; // "Signals"
  Tok := ALex.Next; ExpectSymbol(Tok, '{');
  Sigs := TList<TOBDLDFSignal>.Create;
  Subs := TList<string>.Create;
  try
    while True do
    begin
      Tok := ALex.Next;
      if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
      if Tok.Kind = tkEOF then
        raise EOBDProtocol.Create('LDF: unterminated Signals block');
      if Tok.Kind <> tkIdent then Continue;

      Sig := Default(TOBDLDFSignal);
      Sig.Name := Tok.Text;
      Tok := ALex.Next; ExpectSymbol(Tok, ':');
      Tok := ALex.Next; Expect(Tok, tkNumber, '');
      Sig.SizeBits := Integer(ParseInt64(Tok.Text));
      Tok := ALex.Next; ExpectSymbol(Tok, ',');
      Tok := ALex.Next;
      if Tok.Kind = tkNumber then
        Sig.InitValue := ParseInt64(Tok.Text)
      else if (Tok.Kind = tkSymbol) and (Tok.Text = '{') then
      begin
        // array initialiser; skip to closing brace
        SkipBalancedBraces(ALex);
      end;
      Tok := ALex.Next; ExpectSymbol(Tok, ',');
      Tok := ALex.Next; Expect(Tok, tkIdent, '');
      Sig.Publisher := Tok.Text;

      Subs.Clear;
      while True do
      begin
        Tok := ALex.Next;
        if (Tok.Kind = tkSymbol) and (Tok.Text = ';') then Break;
        if (Tok.Kind = tkSymbol) and (Tok.Text = ',') then Continue;
        if Tok.Kind = tkIdent then Subs.Add(Tok.Text);
        if Tok.Kind = tkEOF then
          raise EOBDProtocol.Create('LDF: unterminated signal entry');
      end;
      Sig.Subscribers := Subs.ToArray;
      Sigs.Add(Sig);
    end;
    ACluster.Signals := Sigs.ToArray;
  finally
    Subs.Free;
    Sigs.Free;
  end;
end;

procedure ParseFrames(ALex: TLDFLexer; var ACluster: TOBDLDFCluster);
var
  Tok: TToken;
  Frame: TOBDLDFFrame;
  Place: TOBDLDFSignalPlacement;
  Frames: TList<TOBDLDFFrame>;
  Places: TList<TOBDLDFSignalPlacement>;
begin
  Tok := ALex.Next; // "Frames"
  Tok := ALex.Next; ExpectSymbol(Tok, '{');
  Frames := TList<TOBDLDFFrame>.Create;
  Places := TList<TOBDLDFSignalPlacement>.Create;
  try
    while True do
    begin
      Tok := ALex.Next;
      if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
      if Tok.Kind = tkEOF then
        raise EOBDProtocol.Create('LDF: unterminated Frames block');
      if Tok.Kind <> tkIdent then Continue;

      Frame := Default(TOBDLDFFrame);
      Frame.Name := Tok.Text;
      Tok := ALex.Next; ExpectSymbol(Tok, ':');
      Tok := ALex.Next; Expect(Tok, tkNumber, '');
      Frame.FrameID := Byte(ParseInt64(Tok.Text));
      Tok := ALex.Next; ExpectSymbol(Tok, ',');
      Tok := ALex.Next; Expect(Tok, tkIdent, '');
      Frame.Publisher := Tok.Text;
      Tok := ALex.Next; ExpectSymbol(Tok, ',');
      Tok := ALex.Next; Expect(Tok, tkNumber, '');
      Frame.SizeBytes := Byte(ParseInt64(Tok.Text));
      Tok := ALex.Next; ExpectSymbol(Tok, '{');

      Places.Clear;
      while True do
      begin
        Tok := ALex.Next;
        if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
        if Tok.Kind = tkEOF then
          raise EOBDProtocol.Create('LDF: unterminated frame body');
        if Tok.Kind <> tkIdent then Continue;
        Place.SignalName := Tok.Text;
        Tok := ALex.Next; ExpectSymbol(Tok, ',');
        Tok := ALex.Next; Expect(Tok, tkNumber, '');
        Place.OffsetBits := Integer(ParseInt64(Tok.Text));
        SkipUntilSemicolon(ALex);
        Places.Add(Place);
      end;
      Frame.Signals := Places.ToArray;
      Frames.Add(Frame);
    end;
    ACluster.Frames := Frames.ToArray;
  finally
    Places.Free;
    Frames.Free;
  end;
end;

procedure ParseSchedules(ALex: TLDFLexer; var ACluster: TOBDLDFCluster);
var
  Tok: TToken;
  Sched: TOBDLDFSchedule;
  Entry: TOBDLDFScheduleEntry;
  Scheds: TList<TOBDLDFSchedule>;
  Entries: TList<TOBDLDFScheduleEntry>;
  DelayMs: Double;
begin
  Tok := ALex.Next; // "Schedule_tables"
  Tok := ALex.Next; ExpectSymbol(Tok, '{');
  Scheds := TList<TOBDLDFSchedule>.Create;
  Entries := TList<TOBDLDFScheduleEntry>.Create;
  try
    while True do
    begin
      Tok := ALex.Next;
      if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
      if Tok.Kind = tkEOF then
        raise EOBDProtocol.Create('LDF: unterminated Schedule_tables block');
      if Tok.Kind <> tkIdent then Continue;

      Sched := Default(TOBDLDFSchedule);
      Sched.Name := Tok.Text;
      Tok := ALex.Next; ExpectSymbol(Tok, '{');

      Entries.Clear;
      while True do
      begin
        Tok := ALex.Next;
        if (Tok.Kind = tkSymbol) and (Tok.Text = '}') then Break;
        if Tok.Kind = tkEOF then
          raise EOBDProtocol.Create('LDF: unterminated schedule body');
        if Tok.Kind <> tkIdent then Continue;
        Entry := Default(TOBDLDFScheduleEntry);
        Entry.FrameName := Tok.Text;
        // Expect "delay <ms> ms ;"
        Tok := ALex.Next;
        if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'delay') then
        begin
          Tok := ALex.Next;
          DelayMs := 0;
          if Tok.Kind = tkNumber then DelayMs := ParseFloatGeneric(Tok.Text);
          // Optional unit "ms"
          Tok := ALex.Peek;
          if (Tok.Kind = tkIdent) and SameText(Tok.Text, 'ms') then
            ALex.Next;
          Entry.DelayMicros := Round(DelayMs * 1000);
          SkipUntilSemicolon(ALex);
        end
        else
          SkipUntilSemicolon(ALex);
        Entries.Add(Entry);
      end;
      Sched.Entries := Entries.ToArray;
      Scheds.Add(Sched);
    end;
    ACluster.Schedules := Scheds.ToArray;
  finally
    Entries.Free;
    Scheds.Free;
  end;
end;

class function TOBDLDFParser.Parse(const ASource: string): TOBDLDFCluster;
var
  Lex: TLDFLexer;
  Tok: TToken;
begin
  Result := Default(TOBDLDFCluster);
  Lex := TLDFLexer.Create(ASource);
  try
    ParseHeader(Lex, Result);
    while True do
    begin
      Tok := Lex.Peek;
      if Tok.Kind = tkEOF then Break;
      if Tok.Kind <> tkIdent then begin Lex.Next; Continue; end;

      if SameText(Tok.Text, 'Nodes') then
        ParseNodes(Lex, Result)
      else if SameText(Tok.Text, 'Signals') or
              SameText(Tok.Text, 'Diagnostic_signals') then
      begin
        if SameText(Tok.Text, 'Diagnostic_signals') then
          // skip diagnostic signals block — same shape but separate table
          begin
            Lex.Next;
            Tok := Lex.Next; ExpectSymbol(Tok, '{');
            SkipBalancedBraces(Lex);
            Continue;
          end;
        ParseSignals(Lex, Result);
      end
      else if SameText(Tok.Text, 'Frames') then
        ParseFrames(Lex, Result)
      else if SameText(Tok.Text, 'Schedule_tables') then
        ParseSchedules(Lex, Result)
      else
      begin
        // Unknown structural section: skip it cleanly.
        Lex.Next; // section name
        Tok := Lex.Peek;
        if (Tok.Kind = tkSymbol) and (Tok.Text = '{') then
        begin
          Lex.Next;
          SkipBalancedBraces(Lex);
        end
        else
          SkipUntilSemicolon(Lex);
      end;
    end;
  finally
    Lex.Free;
  end;
end;

class function TOBDLDFParser.ParseFile(
  const AFileName: string): TOBDLDFCluster;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    Result := Parse(SL.Text);
  finally
    SL.Free;
  end;
end;

end.
