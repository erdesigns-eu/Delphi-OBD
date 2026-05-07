//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.VW.pas
// CONTENTS       : Reference VW Group OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Covers the four common WMIs in the VW group (VW,
//                  Audi, Skoda, SEAT). Catalogs a small set of common
//                  DIDs as a starting point — production users add
//                  more as they encounter them.
//------------------------------------------------------------------------------
unit OBD.OEM.VW;

interface

uses
  System.SysUtils, OBD.OEM;

type
  TOBDOEMExtensionVW = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>); override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers;

function TOBDOEMExtensionVW.ManufacturerKey: string; begin Result := 'VAG'; end;
function TOBDOEMExtensionVW.DisplayName: string; begin Result := 'Volkswagen Audi Group'; end;

function TOBDOEMExtensionVW.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // VW: WVW (passenger), WV1/WV2 (commercial)
  // Audi: WAU, TRU
  // Skoda: TMB
  // SEAT: VSS
  Result := (WMI = 'WVW') or (WMI = 'WV1') or (WMI = 'WV2') or
            (WMI = 'WAU') or (WMI = 'TRU') or
            (WMI = 'TMB') or (WMI = 'VSS');
end;

procedure TOBDOEMExtensionVW.BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>);
begin
  // Common VAG UDS DIDs. These are documented in the public ODX-D
  // descriptors that ship with VAG diagnostic tools (reference only —
  // exact semantics vary by ECU and model year).
  DIDs := [
    DID($F186, 'active_diagnostic_session',         'Currently active UDS session'),
    DID($F187, 'spare_part_number',                 'VAG hardware part number'),
    DID($F189, 'sw_version_number',                 'Software version'),
    DID($F18A, 'system_supplier_identifier',        'ECU supplier id'),
    DID($F18C, 'ecu_serial_number',                 'ECU serial'),
    DID($F190, 'vin',                               'Vehicle identification number'),
    DID($F191, 'vehicle_manufacturer_ecu_hw_number','VAG hardware revision'),
    DID($F195, 'system_supplier_ecu_sw_version',    'Supplier software version'),
    DID($F197, 'system_name',                       'ECU long name'),
    DID($F1A0, 'long_coding',                       'Long-coding bytes'),
    DID($F40D, 'vehicle_speed',                     'Speed in km/h'),
    DID($F405, 'battery_voltage',                   'Battery voltage (mV)')
  ];

  Routines := [
    Routine($0203, 'reset_long_term_fuel_trim',  'Reset adaptive trims'),
    Routine($0301, 'basic_setting',              'VAG basic setting routine'),
    Routine($0302, 'output_test',                'VAG output test'),
    Routine($FF00, 'erase_memory',               'Pre-flash erase')
  ];
end;

function TOBDOEMExtensionVW.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Voltage: Word;
  Speed: Word;
begin
  // Specialise the decoders that have known unit conversions; everything
  // else falls through to the base hex-dump.
  case DID of
    $F405:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Payload[0] shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
    $F40D:
      if Length(Payload) >= 2 then
      begin
        Speed := (Payload[0] shl 8) or Payload[1];
        Result := Format('vehicle_speed = %d km/h', [Speed]);
        Exit;
      end;
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionVW.Create);

end.
