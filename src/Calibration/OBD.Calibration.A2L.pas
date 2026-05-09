//------------------------------------------------------------------------------
//  OBD.Calibration.A2L
//
//  A2L (ASAM MCD-2 MC) parser. Consumes the variable / characteristic
//  description files produced by every modern ECU build pipeline
//  (Vector ASAP2, ETAS INCA, dSPACE, etc.) and exposes them to the
//  XCP / CCP master so a host can refer to ECU memory by symbol
//  rather than by address.
//
//  This subset covers the three sections that drive measurement +
//  calibration:
//
//    /begin MEASUREMENT  name desc datatype conv lower upper
//        ECU_ADDRESS / ECU_ADDRESS_EXTENSION / FORMAT / UNIT / ...
//    /end MEASUREMENT
//
//    /begin CHARACTERISTIC  name desc kind ecu_addr deposit max_diff
//                            conv lower upper
//        ...
//    /end CHARACTERISTIC
//
//    /begin COMPU_METHOD  name desc kind format unit
//        COEFFS a b c d e f
//    /end COMPU_METHOD
//
//  Other sections (RECORD_LAYOUT, AXIS_PTS, FUNCTION, GROUP, etc.)
//  are tolerated and skipped so a future expansion can extend the
//  parser without breaking existing files.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ASAM MCD-2 MC v1.7 (A2L specification)
//
//  History     :
//    2026-05-09  ERD  Phase 7 initial.
//------------------------------------------------------------------------------

unit OBD.Calibration.A2L;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  OBD.Types;

type
  /// <summary>A2L data type (verbatim ASAM MCD-2 MC enum).</summary>
  TOBDA2LDataType = (
    a2lUByte, a2lSByte,
    a2lUWord, a2lSWord,
    a2lULong, a2lSLong,
    a2lAUInt64, a2lAInt64,
    a2lFloat32, a2lFloat64
  );

  /// <summary>Characteristic kind.</summary>
  TOBDA2LCharKind = (
    ckValue, ckCurve, ckMap, ckCuboid, ckAscii, ckUnknown
  );

  /// <summary>Conversion method kind.</summary>
  TOBDA2LCompuKind = (
    cmIdentity, cmLinear, cmRatFunc, cmTabIntp, cmTabNointp, cmTabVerb,
    cmFormula, cmUnknown
  );

  /// <summary>Linear / RAT_FUNC coefficients (a..f). For LINEAR only
  /// (a, b) are meaningful; for RAT_FUNC: physical = (a*x^2 + b*x + c)/
  /// (d*x^2 + e*x + f).</summary>
  TOBDA2LCoeffs = record
    A, B, C, D, E, F: Double;
  end;

  /// <summary>Decoded conversion method.</summary>
  TOBDA2LCompuMethod = record
    Name: string;
    Description: string;
    Kind: TOBDA2LCompuKind;
    Format: string;
    Unit_: string;
    Coeffs: TOBDA2LCoeffs;
  end;

  /// <summary>Decoded MEASUREMENT (ECU variable to read).</summary>
  TOBDA2LMeasurement = record
    Name: string;
    Description: string;
    DataType: TOBDA2LDataType;
    ConvName: string;
    LowerLimit: Double;
    UpperLimit: Double;
    EcuAddress: UInt64;
    EcuAddressExtension: UInt32;
  end;

  /// <summary>Decoded CHARACTERISTIC (ECU calibratable parameter).</summary>
  TOBDA2LCharacteristic = record
    Name: string;
    Description: string;
    Kind: TOBDA2LCharKind;
    EcuAddress: UInt64;
    Deposit: string;
    MaxDiff: Double;
    ConvName: string;
    LowerLimit: Double;
    UpperLimit: Double;
  end;

  /// <summary>Result of parsing an A2L file.</summary>
  TOBDA2LCluster = record
    Project: string;
    Module: string;
    Measurements: TArray<TOBDA2LMeasurement>;
    Characteristics: TArray<TOBDA2LCharacteristic>;
    CompuMethods: TArray<TOBDA2LCompuMethod>;
  end;

  /// <summary>Stateless A2L parser.</summary>
  TOBDA2L = class
  public
    /// <summary>Parses an A2L source string.</summary>
    /// <exception cref="EOBDProtocol">Parse error with line
    /// number.</exception>
    class function Parse(const ASource: string): TOBDA2LCluster; static;
    /// <summary>Reads a file and parses it.</summary>
    class function ParseFile(const AFileName: string): TOBDA2LCluster; static;
    /// <summary>Maps an A2L data-type token to the enum.</summary>
    class function ParseDataType(const AText: string): TOBDA2LDataType; static;
    /// <summary>Applies a CompuMethod to a raw integer value, in
    /// the spec convention (LINEAR: phys = a*raw + b; RAT_FUNC:
    /// phys = (a*r^2 + b*r + c) / (d*r^2 + e*r + f)).</summary>
    class function Convert(const ACompu: TOBDA2LCompuMethod;
      ARaw: Double): Double; static;
  end;

implementation

uses
  System.StrUtils, System.Math;

{ ---- token stream ----------------------------------------------------------- }

type
  TA2LTokenKind = (atEOF, atIdent, atNumber, atString, atSlashKw);

  TA2LToken = record
    Kind: TA2LTokenKind;
    Text: string;
    Line: Integer;
  end;

  TA2LLexer = class
  strict private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FPeeked: Boolean;
    FPeek: TA2LToken;
    procedure SkipWhitespaceAndComments;
  public
    constructor Create(const ASource: string);
    function Next: TA2LToken;
    function Peek: TA2LToken;
  end;

constructor TA2LLexer.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
  FPos := 1;
  FLine := 1;
end;

procedure TA2LLexer.SkipWhitespaceAndComments;
begin
  while FPos <= Length(FSource) do
  begin
    if FSource[FPos] = #10 then begin Inc(FLine); Inc(FPos); Continue; end;
    if CharInSet(FSource[FPos], [#13, ' ', #9]) then begin Inc(FPos); Continue; end;
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

function TA2LLexer.Next: TA2LToken;
var
  Start: Integer;
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
    Result.Kind := atEOF;
    Result.Text := '';
    Exit;
  end;
  C := FSource[FPos];
  // /begin /end
  if (C = '/') and (FPos < Length(FSource)) then
  begin
    Start := FPos;
    Inc(FPos);
    while (FPos <= Length(FSource)) and
          CharInSet(FSource[FPos], ['A'..'Z', 'a'..'z']) do
      Inc(FPos);
    Result.Kind := atSlashKw;
    Result.Text := Copy(FSource, Start, FPos - Start);
    Exit;
  end;
  // String
  if C = '"' then
  begin
    Inc(FPos);
    Start := FPos;
    while (FPos <= Length(FSource)) and (FSource[FPos] <> '"') do
    begin
      if FSource[FPos] = #10 then Inc(FLine);
      Inc(FPos);
    end;
    Result.Kind := atString;
    Result.Text := Copy(FSource, Start, FPos - Start);
    if FPos <= Length(FSource) then Inc(FPos);
    Exit;
  end;
  // Number / negative number
  if CharInSet(C, ['0'..'9']) or
     ((C = '-') and (FPos < Length(FSource)) and
      CharInSet(FSource[FPos + 1], ['0'..'9'])) then
  begin
    Start := FPos;
    if C = '-' then Inc(FPos);
    if (FPos + 1 <= Length(FSource)) and (FSource[FPos] = '0') and
       CharInSet(FSource[FPos + 1], ['x', 'X']) then
    begin
      Inc(FPos, 2);
      while (FPos <= Length(FSource)) and
            CharInSet(FSource[FPos], ['0'..'9', 'A'..'F', 'a'..'f']) do
        Inc(FPos);
    end
    else
    begin
      while (FPos <= Length(FSource)) and
            CharInSet(FSource[FPos], ['0'..'9', '.', 'e', 'E', '+', '-']) do
        Inc(FPos);
    end;
    Result.Kind := atNumber;
    Result.Text := Copy(FSource, Start, FPos - Start);
    Exit;
  end;
  // Identifier / keyword
  if CharInSet(C, ['A'..'Z', 'a'..'z', '_', '.']) then
  begin
    Start := FPos;
    Inc(FPos);
    while (FPos <= Length(FSource)) and
          CharInSet(FSource[FPos], ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
      Inc(FPos);
    Result.Kind := atIdent;
    Result.Text := Copy(FSource, Start, FPos - Start);
    Exit;
  end;
  // Single character; treat as identifier so the parser resyncs.
  Result.Kind := atIdent;
  Result.Text := C;
  Inc(FPos);
end;

function TA2LLexer.Peek: TA2LToken;
begin
  if not FPeeked then
  begin
    FPeek := Next;
    FPeeked := True;
  end;
  Result := FPeek;
end;

{ ---- helpers ---------------------------------------------------------------- }

function ParseDouble(const AText: string): Double;
var
  Cleaned: string;
  Negative: Boolean;
  Fmt: TFormatSettings;
begin
  Cleaned := AText;
  Negative := (Length(Cleaned) > 0) and (Cleaned[1] = '-');
  if Negative then Cleaned := Copy(Cleaned, 2, MaxInt);
  if (Length(Cleaned) >= 2) and (Cleaned[1] = '0') and
     CharInSet(Cleaned[2], ['x', 'X']) then
  begin
    Result := StrToInt64('$' + Copy(Cleaned, 3, MaxInt));
    if Negative then Result := -Result;
  end
  else
  begin
    Fmt := TFormatSettings.Invariant;
    Result := StrToFloat(Cleaned, Fmt);
    if Negative then Result := -Result;
  end;
end;

function ParseUInt64(const AText: string): UInt64;
var
  Cleaned: string;
begin
  Cleaned := AText;
  if (Length(Cleaned) >= 2) and (Cleaned[1] = '0') and
     CharInSet(Cleaned[2], ['x', 'X']) then
    Result := StrToInt64('$' + Copy(Cleaned, 3, MaxInt))
  else
    Result := StrToInt64(Cleaned);
end;

class function TOBDA2L.ParseDataType(const AText: string): TOBDA2LDataType;
begin
  if SameText(AText, 'UBYTE')   then Result := a2lUByte
  else if SameText(AText, 'SBYTE')   then Result := a2lSByte
  else if SameText(AText, 'UWORD')   then Result := a2lUWord
  else if SameText(AText, 'SWORD')   then Result := a2lSWord
  else if SameText(AText, 'ULONG')   then Result := a2lULong
  else if SameText(AText, 'SLONG')   then Result := a2lSLong
  else if SameText(AText, 'A_UINT64') then Result := a2lAUInt64
  else if SameText(AText, 'A_INT64')  then Result := a2lAInt64
  else if SameText(AText, 'FLOAT32_IEEE') then Result := a2lFloat32
  else if SameText(AText, 'FLOAT64_IEEE') then Result := a2lFloat64
  else
    raise EOBDProtocol.CreateFmt('A2L: unknown DATA_TYPE "%s"', [AText]);
end;

function ParseCharKind(const AText: string): TOBDA2LCharKind;
begin
  if      SameText(AText, 'VALUE')   then Result := ckValue
  else if SameText(AText, 'CURVE')   then Result := ckCurve
  else if SameText(AText, 'MAP')     then Result := ckMap
  else if SameText(AText, 'CUBOID')  then Result := ckCuboid
  else if SameText(AText, 'ASCII')   then Result := ckAscii
  else                                    Result := ckUnknown;
end;

function ParseCompuKind(const AText: string): TOBDA2LCompuKind;
begin
  if      SameText(AText, 'IDENTICAL')   then Result := cmIdentity
  else if SameText(AText, 'LINEAR')      then Result := cmLinear
  else if SameText(AText, 'RAT_FUNC')    then Result := cmRatFunc
  else if SameText(AText, 'TAB_INTP')    then Result := cmTabIntp
  else if SameText(AText, 'TAB_NOINTP')  then Result := cmTabNointp
  else if SameText(AText, 'TAB_VERB')    then Result := cmTabVerb
  else if SameText(AText, 'FORM')        then Result := cmFormula
  else                                        Result := cmUnknown;
end;

procedure SkipBlock(ALex: TA2LLexer; const ABlockName: string);
var
  Tok: TA2LToken;
  Depth: Integer;
begin
  Depth := 1;
  while Depth > 0 do
  begin
    Tok := ALex.Next;
    if Tok.Kind = atEOF then
      raise EOBDProtocol.CreateFmt(
        'A2L: unterminated /begin %s', [ABlockName]);
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/begin') then
      Inc(Depth)
    else if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/end') then
    begin
      Dec(Depth);
      if Depth = 0 then
      begin
        // Eat the end-tag name.
        ALex.Next;
        Exit;
      end;
    end;
  end;
end;

{ ---- per-block parsers ------------------------------------------------------ }

procedure ParseMeasurement(ALex: TA2LLexer; out AOut: TOBDA2LMeasurement);
var
  Tok: TA2LToken;
begin
  AOut := Default(TOBDA2LMeasurement);
  // Header: name desc datatype conv lower upper
  Tok := ALex.Next; AOut.Name := Tok.Text;
  Tok := ALex.Next; AOut.Description := Tok.Text;
  Tok := ALex.Next; AOut.DataType := TOBDA2L.ParseDataType(Tok.Text);
  Tok := ALex.Next; AOut.ConvName := Tok.Text;
  Tok := ALex.Next; // resolution (ignored)
  Tok := ALex.Next; // accuracy (ignored)
  Tok := ALex.Next; AOut.LowerLimit := ParseDouble(Tok.Text);
  Tok := ALex.Next; AOut.UpperLimit := ParseDouble(Tok.Text);

  while True do
  begin
    Tok := ALex.Next;
    if Tok.Kind = atEOF then
      raise EOBDProtocol.Create('A2L: unterminated MEASUREMENT');
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/end') then
    begin
      ALex.Next; // MEASUREMENT
      Exit;
    end;
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/begin') then
    begin
      Tok := ALex.Next;
      SkipBlock(ALex, Tok.Text);
      Continue;
    end;
    if (Tok.Kind = atIdent) and SameText(Tok.Text, 'ECU_ADDRESS') then
    begin
      Tok := ALex.Next;
      AOut.EcuAddress := ParseUInt64(Tok.Text);
    end
    else if (Tok.Kind = atIdent) and
            SameText(Tok.Text, 'ECU_ADDRESS_EXTENSION') then
    begin
      Tok := ALex.Next;
      AOut.EcuAddressExtension := UInt32(ParseUInt64(Tok.Text));
    end;
  end;
end;

procedure ParseCharacteristic(ALex: TA2LLexer;
  out AOut: TOBDA2LCharacteristic);
var
  Tok: TA2LToken;
begin
  AOut := Default(TOBDA2LCharacteristic);
  Tok := ALex.Next; AOut.Name := Tok.Text;
  Tok := ALex.Next; AOut.Description := Tok.Text;
  Tok := ALex.Next; AOut.Kind := ParseCharKind(Tok.Text);
  Tok := ALex.Next; AOut.EcuAddress := ParseUInt64(Tok.Text);
  Tok := ALex.Next; AOut.Deposit := Tok.Text;
  Tok := ALex.Next; AOut.MaxDiff := ParseDouble(Tok.Text);
  Tok := ALex.Next; AOut.ConvName := Tok.Text;
  Tok := ALex.Next; AOut.LowerLimit := ParseDouble(Tok.Text);
  Tok := ALex.Next; AOut.UpperLimit := ParseDouble(Tok.Text);

  while True do
  begin
    Tok := ALex.Next;
    if Tok.Kind = atEOF then
      raise EOBDProtocol.Create('A2L: unterminated CHARACTERISTIC');
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/end') then
    begin
      ALex.Next;
      Exit;
    end;
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/begin') then
    begin
      Tok := ALex.Next;
      SkipBlock(ALex, Tok.Text);
    end;
  end;
end;

procedure ParseCompuMethod(ALex: TA2LLexer; out AOut: TOBDA2LCompuMethod);
var
  Tok: TA2LToken;
begin
  AOut := Default(TOBDA2LCompuMethod);
  Tok := ALex.Next; AOut.Name := Tok.Text;
  Tok := ALex.Next; AOut.Description := Tok.Text;
  Tok := ALex.Next; AOut.Kind := ParseCompuKind(Tok.Text);
  Tok := ALex.Next; AOut.Format := Tok.Text;
  Tok := ALex.Next; AOut.Unit_ := Tok.Text;

  while True do
  begin
    Tok := ALex.Next;
    if Tok.Kind = atEOF then
      raise EOBDProtocol.Create('A2L: unterminated COMPU_METHOD');
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/end') then
    begin
      ALex.Next;
      Exit;
    end;
    if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/begin') then
    begin
      Tok := ALex.Next;
      SkipBlock(ALex, Tok.Text);
      Continue;
    end;
    if (Tok.Kind = atIdent) and SameText(Tok.Text, 'COEFFS') then
    begin
      Tok := ALex.Next; AOut.Coeffs.A := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.B := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.C := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.D := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.E := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.F := ParseDouble(Tok.Text);
    end
    else if (Tok.Kind = atIdent) and SameText(Tok.Text, 'COEFFS_LINEAR') then
    begin
      // LINEAR uses A, B; phys = A * raw + B per ASAM MCD-2.
      Tok := ALex.Next; AOut.Coeffs.A := ParseDouble(Tok.Text);
      Tok := ALex.Next; AOut.Coeffs.B := ParseDouble(Tok.Text);
    end;
  end;
end;

class function TOBDA2L.Parse(const ASource: string): TOBDA2LCluster;
var
  Lex: TA2LLexer;
  Tok: TA2LToken;
  Measurements: TList<TOBDA2LMeasurement>;
  Characteristics: TList<TOBDA2LCharacteristic>;
  CompuMethods: TList<TOBDA2LCompuMethod>;
  Meas: TOBDA2LMeasurement;
  Charact: TOBDA2LCharacteristic;
  Compu: TOBDA2LCompuMethod;
  BlockName: string;
begin
  Result := Default(TOBDA2LCluster);
  Measurements := TList<TOBDA2LMeasurement>.Create;
  Characteristics := TList<TOBDA2LCharacteristic>.Create;
  CompuMethods := TList<TOBDA2LCompuMethod>.Create;
  Lex := TA2LLexer.Create(ASource);
  try
    while True do
    begin
      Tok := Lex.Next;
      if Tok.Kind = atEOF then Break;
      if (Tok.Kind = atSlashKw) and SameText(Tok.Text, '/begin') then
      begin
        Tok := Lex.Next;
        BlockName := Tok.Text;
        if SameText(BlockName, 'MEASUREMENT') then
        begin
          ParseMeasurement(Lex, Meas);
          Measurements.Add(Meas);
        end
        else if SameText(BlockName, 'CHARACTERISTIC') then
        begin
          ParseCharacteristic(Lex, Charact);
          Characteristics.Add(Charact);
        end
        else if SameText(BlockName, 'COMPU_METHOD') then
        begin
          ParseCompuMethod(Lex, Compu);
          CompuMethods.Add(Compu);
        end
        else if SameText(BlockName, 'PROJECT') then
        begin
          Tok := Lex.Next; Result.Project := Tok.Text;
          // skip until /end PROJECT
          SkipBlock(Lex, BlockName);
        end
        else if SameText(BlockName, 'MODULE') then
        begin
          Tok := Lex.Next; Result.Module := Tok.Text;
          // Continue parsing inside the module — don't skip.
        end
        else
          SkipBlock(Lex, BlockName);
      end;
    end;
    Result.Measurements := Measurements.ToArray;
    Result.Characteristics := Characteristics.ToArray;
    Result.CompuMethods := CompuMethods.ToArray;
  finally
    Lex.Free;
    Measurements.Free;
    Characteristics.Free;
    CompuMethods.Free;
  end;
end;

class function TOBDA2L.ParseFile(const AFileName: string): TOBDA2LCluster;
begin
  Result := Parse(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

class function TOBDA2L.Convert(const ACompu: TOBDA2LCompuMethod;
  ARaw: Double): Double;
var
  Num, Den: Double;
begin
  case ACompu.Kind of
    cmIdentity: Result := ARaw;
    cmLinear:   Result := ACompu.Coeffs.A * ARaw + ACompu.Coeffs.B;
    cmRatFunc:
    begin
      // ASAM convention: phys-from-raw inverts the ratfunc, so raw =
      // (a*phys^2 + b*phys + c) / (d*phys^2 + e*phys + f). For the
      // common "(a, b, c) = (0, scale, offset), (d, e, f) = (0, 0, 1)"
      // case the formula reduces to phys = (raw - offset) / scale.
      Den := ACompu.Coeffs.D * ARaw * ARaw + ACompu.Coeffs.E * ARaw +
             ACompu.Coeffs.F;
      Num := ACompu.Coeffs.A * ARaw * ARaw + ACompu.Coeffs.B * ARaw +
             ACompu.Coeffs.C;
      if Den = 0 then Result := NaN
      else Result := Num / Den;
    end;
  else
    Result := ARaw;
  end;
end;

end.
