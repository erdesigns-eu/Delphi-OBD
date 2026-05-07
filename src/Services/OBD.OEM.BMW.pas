//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.BMW.pas
// CONTENTS       : Reference BMW OEM extension
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Catalogs a small set of common BMW DIDs as a starting
//                  point. Real-world BMW coverage requires per-FA / per-
//                  E-Sys fingerprint data — production users add via
//                  subclass + extra catalog entries.
//------------------------------------------------------------------------------
unit OBD.OEM.BMW;

interface

uses
  System.SysUtils, OBD.OEM;

type
  TOBDOEMExtensionBMW = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionBMW.ManufacturerKey: string; begin Result := 'BMW'; end;
function TOBDOEMExtensionBMW.DisplayName: string; begin Result := 'Bayerische Motoren Werke'; end;

function TOBDOEMExtensionBMW.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // BMW: WBA (sedan/coupe), WBS (M-cars), WBY (i3/i8/iX),
  // WMW (MINI), 5UX (US-built X1/X3/X5), 4US (US-built sedan/coupe).
  Result := (WMI = 'WBA') or (WMI = 'WBS') or (WMI = 'WBY') or
            (WMI = 'WMW') or (WMI = '5UX') or (WMI = '4US');
end;

procedure TOBDOEMExtensionBMW.BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>);
begin
  DIDs := [
    DID($F100, 'i_stufe_werks',           'Factory I-Stufe (build version)'),
    DID($F101, 'i_stufe_aktuell',         'Current I-Stufe'),
    DID($F1A2, 'fa_assembly',             'Vehicle order (FA, freight assembly)'),
    DID($F186, 'active_diagnostic_session','Currently active UDS session'),
    DID($F18B, 'manufacturing_date',      'ECU manufacturing date'),
    DID($F18C, 'ecu_serial_number',       'ECU serial number'),
    DID($F190, 'vin',                     'Vehicle identification number'),
    DID($F195, 'sw_version',              'Software version'),
    DID($F197, 'system_name',             'ECU long name'),
    DID($D050, 'mileage',                 'Mileage in km'),
    DID($D051, 'battery_voltage',         'Battery voltage in mV')
  ];

  Routines := [
    Routine($0F03, 'fa_write',               'Write FA / vehicle order'),
    Routine($0202, 'service_function',       'Service-mode flag'),
    Routine($FF00, 'erase_memory',           'Pre-flash erase')
  ];
end;

function TOBDOEMExtensionBMW.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Voltage: Word;
  Mileage: Cardinal;
begin
  case DID of
    $D051:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Payload[0] shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
    $D050:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
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
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionBMW.Create);

end.
