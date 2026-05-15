//------------------------------------------------------------------------------
//  VAGLongCoding — sample 23
//
//  Demonstrates TOBDCodingVAG long-coding-string helpers:
//    - Parse a long-coding hex string (with any number of dashes /
//      spaces / blanks) into a TBytes buffer.
//    - Read / write individual bits at (ByteIndex, Bit).
//    - Read / write whole bytes.
//    - Round-trip the buffer back to a normalised hex string for
//      pasting into VCDS / OBDeleven and friends.
//
//  Build & run:
//    dcc32 -B VAGLongCoding.dpr
//    VAGLongCoding
//------------------------------------------------------------------------------

program VAGLongCoding;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types         in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors        in '..\..\src\Core\OBD.Errors.pas',
  OBD.Coding.VAG    in '..\..\src\Coding\OBD.Coding.VAG.pas';

procedure Dump(const ALabel: string; const ABuf: TBytes);
begin
  Writeln(Format('  %-30s %s', [ALabel, TOBDCodingVAG.FormatLongCoding(ABuf, 1)]));
end;

var
  Buf: TBytes;
begin
  Writeln('Delphi-OBD VAG long-coding demo');
  Writeln(StringOfChar('-', 60));

  // Some MQB Central Electronics (09) long-coding-shaped string.
  Buf := TOBDCodingVAG.ParseLongCoding('01-23 45-67 89AB CDEF 0011');
  Dump('parsed', Buf);

  // Flip byte 4 bit 5 (turn the 'Daytime Running Lights' option on,
  // for example — value here is illustrative).
  TOBDCodingVAG.SetBit(Buf, 4, 5, True);
  Dump('after SetBit(4, 5, True)', Buf);

  Writeln(Format('  byte 4 bit 5 reads:           %s',
    [BoolToStr(TOBDCodingVAG.GetBit(Buf, 4, 5), True)]));

  // Replace byte 0 wholesale.
  TOBDCodingVAG.SetByte(Buf, 0, $FF);
  Dump('after SetByte(0, $FF)', Buf);

  Writeln;
  Writeln('Done.');
end.
