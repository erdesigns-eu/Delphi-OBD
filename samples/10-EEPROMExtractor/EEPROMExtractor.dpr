//------------------------------------------------------------------------------
//  EEPROMExtractor — sample 10
//
//  Demonstrates the EEPROM-dump extractor family by writing a
//  synthetic 256-byte dump that mimics what a Volvo HU's 24C01
//  serial-EEPROM looks like, then handing it to
//  TOBDRadioCodeEEPROM_VolvoHU. The extractor reads the 4
//  ASCII digits at offset 0x90 and returns the unlock code.
//
//  The same flow applies to TOBDRadioCodeEEPROM_OpelCD30 and
//  TOBDRadioCodeEEPROM_MercedesBecker - swap the class.
//
//  Build & run:
//    dcc32 -B EEPROMExtractor.dpr
//    EEPROMExtractor
//------------------------------------------------------------------------------

program EEPROMExtractor;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.RadioCode.Types  in '..\..\src\RadioCode\OBD.RadioCode.Types.pas',
  OBD.RadioCode.EEPROM in '..\..\src\RadioCode\OBD.RadioCode.EEPROM.pas';

function MakeSyntheticVolvoHUDump(const ACode4: string): TBytes;
const
  CODE_OFFSET = $90;
var
  I: Integer;
begin
  SetLength(Result, 128);
  // Volvo HU = 24C01, 128 bytes. Pad with $FF.
  for I := 0 to High(Result) do Result[I] := $FF;
  // 4 ASCII digits at offset 0x90.
  for I := 0 to 3 do
    Result[CODE_OFFSET + I] := Ord(ACode4[I + 1]);
end;

var
  DumpPath: string;
  Extractor: TOBDRadioCodeEEPROM_VolvoHU;
  R: TOBDRadioCodeEEPROMResult;
begin
  Writeln('Delphi-OBD EEPROM extractor demo');
  Writeln(StringOfChar('-', 60));

  DumpPath := TPath.Combine(TPath.GetTempPath, 'volvo-hu-demo.bin');
  TFile.WriteAllBytes(DumpPath,
    MakeSyntheticVolvoHUDump('4271'));
  Writeln('Wrote synthetic dump (128 bytes, code=4271 at 0x90):');
  Writeln('  ', DumpPath);

  Extractor := TOBDRadioCodeEEPROM_VolvoHU.Create(nil);
  try
    Extractor.DumpFile := DumpPath;
    R := Extractor.Extract;
    if R.Success then
    begin
      Writeln;
      Writeln('Decoded:');
      Writeln('  vendor : ', R.Vendor);
      Writeln('  code   : ', R.Code);
      Writeln('  raw    : ', R.RawHex);
    end
    else
      Writeln('Extract failed: ', R.Message);
  finally
    Extractor.Free;
    if TFile.Exists(DumpPath) then TFile.Delete(DumpPath);
  end;

  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
