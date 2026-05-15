//------------------------------------------------------------------------------
//  BMWCAFD — sample 24
//
//  Demonstrates TOBDCodingBMW CAFD (CodierAusFahrzeugDaten) TLV
//  helpers — the encoded coding data each BMW ECU stores.
//
//    - Parse a CAFD buffer into individual TLV entries.
//    - Find an entry by parameter-id.
//    - Read / write the entry's value bytes.
//    - Read / write a single bit on a multi-byte value.
//
//  Build & run:
//    dcc32 -B BMWCAFD.dpr
//    BMWCAFD
//------------------------------------------------------------------------------

program BMWCAFD;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types       in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors      in '..\..\src\Core\OBD.Errors.pas',
  OBD.Coding.BMW  in '..\..\src\Coding\OBD.Coding.BMW.pas';

procedure DumpHex(const ALabel: string; const ABuf: TBytes);
var I: Integer; S: string;
begin
  S := '';
  for I := 0 to High(ABuf) do
    S := S + Format(' %.2x', [ABuf[I]]);
  Writeln(Format('  %-26s%s', [ALabel + ':', S]));
end;

var
  Buf: TBytes;
  Entries: TArray<TOBDBMWCAFDEntry>;
  E: TOBDBMWCAFDEntry;
begin
  Writeln('Delphi-OBD BMW CAFD demo');
  Writeln(StringOfChar('-', 60));

  // Two TLVs: id 0x3000 length 1 = 0x55 ; id 0x3001 length 2 = 0x12 0x34.
  Buf := TBytes.Create(
    $30, $00, $00, $01, $55,
    $30, $01, $00, $02, $12, $34);
  DumpHex('initial CAFD buffer', Buf);

  Entries := TOBDCodingBMW.ParseEntries(Buf);
  Writeln(Format('  parsed entries:           %d', [Length(Entries)]));
  for var I := 0 to High(Entries) do
    Writeln(Format('    [%d] id=%.4x  len=%d  offset=%d',
      [I, Entries[I].ParameterID, Entries[I].Length_, Entries[I].Offset]));

  if not TOBDCodingBMW.FindEntry(Entries, $3001, E) then
  begin
    Writeln('  parameter id 0x3001 not found.');
    Exit;
  end;

  // Rewrite the 2-byte value.
  TOBDCodingBMW.WriteValue(Buf, E, TBytes.Create($AA, $BB));
  DumpHex('after WriteValue(0x3001)', Buf);

  // Flip a single bit on parameter id 0x3001.
  Entries := TOBDCodingBMW.ParseEntries(Buf);
  TOBDCodingBMW.FindEntry(Entries, $3001, E);
  TOBDCodingBMW.SetBit(E, 0, 0, True);
  DumpHex('SetBit(byte 0, bit 0)', E.Value);

  Writeln;
  Writeln('Done.');
end.
