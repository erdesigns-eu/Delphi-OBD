//------------------------------------------------------------------------------
//  RadioCodeMulti — sample 09
//
//  Demonstrates three of the bundled radio-code calculators
//  with their real algorithms / databases:
//    Peugeot   - 4-digit C7-barcode -> code (algorithmic)
//    Hyundai   - 4-digit pre-code   -> code (algorithmic, 2002+)
//    Becker4   - 4-digit serial     -> code (10,000-entry DB)
//
//  Build & run:
//    dcc32 -B RadioCodeMulti.dpr
//    RadioCodeMulti
//------------------------------------------------------------------------------

program RadioCodeMulti;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.StrUtils,
  OBD.RadioCode.Types       in '..\..\src\RadioCode\OBD.RadioCode.Types.pas',
  OBD.RadioCode             in '..\..\src\RadioCode\OBD.RadioCode.pas',
  OBD.RadioCode.FrenchItalian in '..\..\src\RadioCode\OBD.RadioCode.FrenchItalian.pas',
  OBD.RadioCode.Asian       in '..\..\src\RadioCode\OBD.RadioCode.Asian.pas',
  OBD.RadioCode.Aftermarket in '..\..\src\RadioCode\OBD.RadioCode.Aftermarket.pas';

procedure Try_(ACalc: TOBDRadioCode; const AInput: string);
var R: TOBDRadioCodeResult;
begin
  ACalc.Input := AInput;
  R := ACalc.Calculate;
  Writeln(Format('  %-20s in=%-8s  ->  %s',
    [ACalc.DisplayName, AInput,
     IfThen(R.Success, 'code=' + R.Code, R.Message)]));
end;

var
  Peugeot: TOBDRadioCodePeugeot;
  Hyundai: TOBDRadioCodeHyundai;
  Becker4: TOBDRadioCodeBecker4;
begin
  Writeln('Delphi-OBD radio-code calculator demo');
  Writeln(StringOfChar('-', 60));

  Peugeot := TOBDRadioCodePeugeot.Create(nil);
  Hyundai := TOBDRadioCodeHyundai.Create(nil);
  Becker4 := TOBDRadioCodeBecker4.Create(nil);
  try
    // Peugeot: input is the 4-digit suffix of the C7 barcode.
    Try_(Peugeot, '1234');
    Try_(Peugeot, '0000');
    Try_(Peugeot, 'ABCD');  // negative - not digits

    // Hyundai 2002+: code = (input + 1212) mod 10000.
    Try_(Hyundai, '1234');  // -> 2446
    Try_(Hyundai, '9000');  // -> 0212  (wraps)

    // Becker4: 10K-entry table lookup.
    // Catalog must live at catalogs/radio-code/becker4.json
    // next to the executable - if missing, the calculator
    // returns a "database not loaded" message.
    Try_(Becker4, '0000');
    Try_(Becker4, '1234');
    Try_(Becker4, '9999');
  finally
    Becker4.Free;
    Hyundai.Free;
    Peugeot.Free;
  end;
  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
