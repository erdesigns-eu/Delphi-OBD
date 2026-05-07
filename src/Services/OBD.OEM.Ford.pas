//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Ford.pas
// CONTENTS       : Reference Ford OEM extension (FoCCCAN / IDS)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.OEM.Ford;

interface

uses
  System.SysUtils, OBD.OEM;

type
  TOBDOEMExtensionFord = class(TOBDOEMExtensionBase)
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

function TOBDOEMExtensionFord.ManufacturerKey: string;
begin Result := 'FORD'; end;
function TOBDOEMExtensionFord.DisplayName: string;
begin Result := 'Ford Motor Company'; end;

function TOBDOEMExtensionFord.ApplicableToVIN(const VIN: string): Boolean;
var
  WMI: string;
begin
  if Length(VIN) < 3 then Exit(False);
  WMI := UpperCase(Copy(VIN, 1, 3));
  // Ford passenger cars + trucks: 1FA / 1FB / 1FC / 1FD / 1FM / 1FT
  // (US-built), 2FA / 2FT (Canada), 3FA / 3FT (Mexico).
  // Lincoln: 1LN, 5LM. Mercury (legacy): 1MR.
  // Australia (Falcon, defunct): 6FP. UK Transit: WF0 (Ford-Werke).
  Result :=
    (WMI = '1FA') or (WMI = '1FB') or (WMI = '1FC') or (WMI = '1FD') or
    (WMI = '1FM') or (WMI = '1FT') or
    (WMI = '2FA') or (WMI = '2FT') or
    (WMI = '3FA') or (WMI = '3FT') or
    (WMI = '1LN') or (WMI = '5LM') or (WMI = '1MR') or
    (WMI = '6FP') or (WMI = 'WF0');
end;

procedure TOBDOEMExtensionFord.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>);
begin
  // Ford IDS / FDRS exposes ECU identity through the standard F1xx range
  // plus Ford-specific D-block DIDs.
  DIDs := [
    DID($F186, 'active_diagnostic_session', 'Currently active UDS session'),
    DID($F187, 'spare_part_number',         'Ford service part number'),
    DID($F188, 'ecu_part_number',           'ECU part number'),
    DID($F189, 'sw_version',                'Software version'),
    DID($F18A, 'system_supplier_id',        'ECU supplier id'),
    DID($F18C, 'ecu_serial_number',         'ECU serial number'),
    DID($F190, 'vin',                       'Vehicle identification number'),
    DID($F195, 'system_supplier_sw',        'Supplier software version'),
    DID($F197, 'system_name',               'ECU long name'),
    DID($DD00, 'mileage',                   'Vehicle mileage in km'),
    DID($DE00, 'fuel_level',                'Fuel level (%)'),
    DID($DE01, 'engine_run_time',           'Engine run time (seconds)'),
    DID($DE02, 'battery_voltage',           'Battery voltage (mV)'),
    DID($DF00, 'pcm_strategy_id',           'Powertrain control strategy id'),
    DID($DF01, 'calibration_id',            'Engine calibration id (CALID)'),
    DID($DF02, 'calibration_verification',  'CVN — calibration verification number')
  ];

  Routines := [
    Routine($0202, 'kam_reset',              'Keep-Alive Memory reset'),
    Routine($0203, 'fuel_trim_reset',        'Reset adaptive fuel trim'),
    Routine($0207, 'sas_calibration',        'Steering angle sensor reset'),
    Routine($0208, 'tpms_relearn',           'Tire-pressure monitoring relearn'),
    Routine($FF00, 'erase_memory',           'Pre-flash erase'),
    Routine($FF01, 'check_programming_deps', 'Cross-ECU dependency check'),
    Routine($FF02, 'verify_checksum',        'Post-flash checksum verification')
  ];
end;

function TOBDOEMExtensionFord.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Mileage, Seconds, Voltage: Cardinal;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $DD00:
      if Length(Payload) >= 4 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $DE00:
      if Length(Payload) >= 1 then
      begin
        Result := Format('fuel_level = %d %%', [Payload[0]]);
        Exit;
      end;
    $DE01:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_run_time = %d s', [Seconds]);
        Exit;
      end;
    $DE02:
      if Length(Payload) >= 2 then
      begin
        Voltage := (Cardinal(Payload[0]) shl 8) or Payload[1];
        Result := Format('battery_voltage = %.3f V', [Voltage / 1000.0]);
        Exit;
      end;
    $DF01, $DF02, $DF00:
      if Length(Payload) > 0 then
      begin
        Result := Format('%s = %s',
          [inherited DecodeDID(DID, []), TEncoding.ASCII.GetString(Payload)]);
        // Strip the trailing 'inherited' empty hex by re-formatting:
        Result := Format('%s = "%s"',
          [string(case DID of
                    $DF00: 'pcm_strategy_id';
                    $DF01: 'calibration_id';
                    $DF02: 'calibration_verification';
                  else  'unknown';
                  end),
           TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionFord.Create);

end.
