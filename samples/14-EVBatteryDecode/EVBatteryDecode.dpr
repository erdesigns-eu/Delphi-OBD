//------------------------------------------------------------------------------
//  EVBatteryDecode — sample 14
//
//  Loads the per-vendor BMS catalogue and inventories what's
//  decodable per platform. For each shipped vendor, prints
//  the ECU CAN IDs and the field list (which DIDs / PIDs the
//  catalogue knows how to decode into TOBDEVBatterySnapshot).
//
//  Live polling needs a real EV; this sample is the offline
//  catalogue inspector that helps you pick a vendor key
//  before you wire Protocol.
//------------------------------------------------------------------------------

program EVBatteryDecode;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Service.EVBattery.Types   in '..\..\src\Service\OBD.Service.EVBattery.Types.pas',
  OBD.Service.EVBattery.Catalog in '..\..\src\Service\OBD.Service.EVBattery.Catalog.pas';

procedure DumpVendor(const AVendor: string);
var
  Cat:  TOBDEVBatteryVendorCatalog;
  Rule: TOBDEVBatteryRule;
begin
  if not TOBDEVBatteryCatalog.TryGet(AVendor, Cat) then Exit;
  Writeln(StringOfChar('-', 60));
  Writeln(Format('vendor=%s  (%s)', [Cat.Vendor, Cat.Label_]));
  Writeln(Format('  ECU req=$%.4x  rsp=$%.4x',
    [Cat.RequestId, Cat.ResponseId]));
  if Length(Cat.ApplicableModels) > 0 then
    Writeln('  models   : ', String.Join(', ', Cat.ApplicableModels));
  Writeln(Format('  rules    : %d', [Length(Cat.Rules)]));
  for Rule in Cat.Rules do
    Writeln(Format('    %-26s svc=$%.2x  did/pid=$%.4x  +%d/%d  %s',
      [Rule.FieldName, Rule.Service, Rule.DIDOrPID,
       Rule.Offset, Rule.Length, Rule.Unit_]));
end;

var
  Vendor: string;
begin
  TOBDEVBatteryCatalog.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\catalogs');
  TOBDEVBatteryCatalog.Reload;

  Writeln('EV battery catalogue inventory');
  for Vendor in TOBDEVBatteryCatalog.VendorKeys do
  begin
    if Vendor.StartsWith('_') then Continue; // skip _stub-test, _manifest
    DumpVendor(Vendor);
  end;
  Writeln(StringOfChar('-', 60));
  Writeln('Done. Set TOBDEVBattery.Vendor to one of the keys above.');
end.
