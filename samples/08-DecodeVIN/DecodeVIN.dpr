//------------------------------------------------------------------------------
//  DecodeVIN — sample 08
//
//  Offline VIN decoder demo: feeds a curated set of real-world
//  VINs through the v2 VIN decoder + vPIC rules and prints the
//  full breakdown (region / country / manufacturer / model
//  year / plant / serial / vehicle features).
//
//  No vehicle / adapter required - the decoder runs against
//  the catalogues under catalogs/vin/.
//
//  Build & run:
//    dcc32 -B DecodeVIN.dpr
//    DecodeVIN
//------------------------------------------------------------------------------

program DecodeVIN;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Service.VINDecoder.Types in '..\..\src\Service\OBD.Service.VINDecoder.Types.pas',
  OBD.Service.VINDecoder      in '..\..\src\Service\OBD.Service.VINDecoder.pas';

procedure PrintVIN(const AVIN: string);
var
  Info: TOBDVINInfo;
  Y:    TOBDVINYear;
begin
  Info := TOBDVINDecoder.Decode(AVIN);
  Writeln(StringOfChar('-', 60));
  Writeln('VIN: ', Info.VIN);
  if not Info.Valid then
  begin
    Writeln('  invalid: ', Info.InvalidReason);
    Exit;
  end;
  Writeln('  WMI / VDS / VIS  : ', Info.WMI, ' / ', Info.VDS, ' / ', Info.VIS);
  Writeln('  region           : ', Info.Region.Name);
  Writeln('  country          : ', Info.Country.Name);
  Writeln('  manufacturer     : ', Info.Manufacturer.Name);
  Writeln('  check digit      : ', Info.CheckDigit,
          '  valid? ', BoolToStr(Info.CheckDigitValid, True));
  Write  ('  model year       : ', Info.ModelYear, '  (candidates:');
  for Y in Info.YearCandidates do
    Write(' ', Y.Year);
  Writeln(')');
  Writeln('  plant            : ', Info.Plant.Name,
          '  ', Info.Plant.City, ', ', Info.Plant.Country);
  Writeln('  serial           : ', Info.Serial);
  Writeln('  body style       : ', Info.Features.BodyStyle);
  Writeln('  drive type       : ', Info.Features.DriveType);
  Writeln('  engine           : ', Info.Features.EngineDisplacement, '  ',
                                   Info.Features.EngineType);
  Writeln('  transmission     : ', Info.Features.Transmission);
  Writeln('  restraint        : ', Info.Features.RestraintSystem);
  if Info.Features.IsCommercial then
    Writeln('  flagged commercial');
end;

begin
  try
    // Point the decoder at the catalogues under catalogs/vin/.
    TOBDVINDecoder.CatalogDir :=
      TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\catalogs');
    TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir);

    Writeln('Delphi-OBD VIN decoder demo');
    PrintVIN('WVWZZZ1KZ7W123456');  // VW Golf, Wolfsburg
    PrintVIN('1FTFW1ETJDFC10312');  // Ford F-150, USA
    PrintVIN('1M8GDM9AXKP042788');  // ISO 3779 reference VIN
    PrintVIN('5UXWX7C58BL000000');  // BMW X3 2011, Munich
    PrintVIN('JTDBV40K7F1234567');  // Toyota
    PrintVIN('TOO_SHORT');          // negative case
    Writeln(StringOfChar('-', 60));
    Writeln('Done.');
  except
    on E: Exception do
      Writeln('error: ', E.ClassName, ': ', E.Message);
  end;
end.
