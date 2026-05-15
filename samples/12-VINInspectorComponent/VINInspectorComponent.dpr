//------------------------------------------------------------------------------
//  VINInspectorComponent — sample 12
//
//  Demonstrates TOBDVINInspector — the non-visual VIN-decode
//  component. Set VIN, read every detail via the per-field
//  shortcut properties (no need to touch TOBDVINInfo directly).
//
//  Same as drop-on-form usage but in a console driver so the
//  example runs without a UI.
//------------------------------------------------------------------------------

program VINInspectorComponent;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Service.VINDecoder.Types in '..\..\src\Service\OBD.Service.VINDecoder.Types.pas',
  OBD.Service.VINDecoder       in '..\..\src\Service\OBD.Service.VINDecoder.pas',
  OBD.Service.VINInspector     in '..\..\src\Service\OBD.Service.VINInspector.pas';

procedure Inspect(Insp: TOBDVINInspector; const AVIN: string);
begin
  Insp.VIN := AVIN;   // AutoDecode triggers a decode here
  Writeln(StringOfChar('-', 60));
  Writeln('VIN: ', AVIN);
  if not Insp.Valid then
  begin
    Writeln('  invalid: ', Insp.Info.InvalidReason);
    Exit;
  end;
  Writeln('  manufacturer : ', Insp.ManufacturerName);
  Writeln('  region       : ', Insp.RegionName);
  Writeln('  country      : ', Insp.CountryName);
  Writeln('  model year   : ', Insp.ModelYear);
  Writeln('  plant        : ', Insp.PlantName);
  Writeln('  body style   : ', Insp.BodyStyle);
  Writeln('  vehicle type : ', Insp.VehicleTypeText);
  if Insp.IsCommercial then
    Writeln('  commercial flag set');
end;

var
  Insp: TOBDVINInspector;
begin
  TOBDVINDecoder.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\catalogs');
  TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir);

  Writeln('TOBDVINInspector demo (component-style decode)');

  Insp := TOBDVINInspector.Create(nil);
  try
    Insp.OnDecoded := procedure(Sender: TObject;
                                const AInfo: TOBDVINInfo)
                     begin
                       Writeln('  [OnDecoded fired]');
                     end;
    Inspect(Insp, '1FTFW1ETJDFC10312');
    Inspect(Insp, 'WVWZZZ1KZ7W123456');
    Inspect(Insp, '5UXWX7C58BL000000');
  finally
    Insp.Free;
  end;
  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
