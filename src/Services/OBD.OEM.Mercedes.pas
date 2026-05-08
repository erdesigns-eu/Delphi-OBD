//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Mercedes.pas
// CONTENTS       : Reference Mercedes-Benz OEM extension (XENTRY-style)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Covers the WMIs Mercedes-Benz issues for passenger
//                  cars (WDB / WDC / WDD / WDF) plus Sprinter
//                  commercials (WD3 / WD4 / 4JG). Catalogues a starter
//                  set of XENTRY-visible DIDs and routines. Production
//                  XENTRY coverage requires per-FIN data files; this
//                  is a structural starting point.
//------------------------------------------------------------------------------
unit OBD.OEM.Mercedes;

interface

uses
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   XENTRY-style Mercedes negotiator. Reads F198 (workshop code)
  ///   immediately after the session-control reply because Daimler
  ///   ECUs gate routines on a known last-writer ID. Tester-present
  ///   interval is 1500 ms (matches XENTRY default).
  /// </summary>
  TOBDMercedesSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionMercedes = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(
      var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); override;
    function CreateSessionNegotiator: IOBDSessionNegotiator; override;
    procedure SeedDefaultSeedKeyAlgorithms(Reg: TOBDSeedKeyRegistry); override;
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); override;
    function DtcCatalogFileName: string; override;
  public
    function ManufacturerKey: string; override;
    function DisplayName: string; override;
    function ApplicableToVIN(const VIN: string): Boolean; override;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; override;
  end;

implementation

uses
  OBD.OEM.Helpers, OBD.OEM.Catalog.Loader, OBD.OEM.DTC.Loader;

function TOBDMercedesSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
begin
  Result := inherited BeginSessionPlan(SessionType, ECUAddress);
  if SessionType = sstDefault then Exit;

  // Append a 22 F198 read — XENTRY relies on the response to log the
  // last writer; many ECUs refuse subsequent routines if it's
  // skipped after entering extended/programming.
  Result.Steps := Result.Steps + [
    UDSStep(TBytes.Create($22, $F1, $98), TBytes.Create($62, $F1, $98),
      'XENTRY workshop-code probe (DID F198)')
  ];
end;

function TOBDMercedesSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;
end;

function TOBDMercedesSessionNegotiator.DisplayName: string;
begin
  Result := 'Mercedes XENTRY';
end;

function TOBDOEMExtensionMercedes.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDMercedesSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Pre-XENTRY HHTwin / Star Diagnosis modules accepted the textbook
  // two's-complement on legacy KWP2000 ECUs (e.g. EZS / EIS readers).
  // XENTRY's modern algorithm is NDA-protected — replace at startup.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionMercedes.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionMercedes.DtcCatalogFileName: string;
begin Result := 'dtc-mercedes.json'; end;

function TOBDOEMExtensionMercedes.ManufacturerKey: string;
begin Result := 'MB'; end;
function TOBDOEMExtensionMercedes.DisplayName: string;
begin Result := 'Mercedes-Benz Group'; end;

function TOBDOEMExtensionMercedes.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in mercedes.json.
  Result := VINMatchesCatalog('mercedes.json', VIN);
end;
procedure TOBDOEMExtensionMercedes.BuildCatalog(
  var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are mercedes.json
  // + uds-standard.json. Hardcoded entries removed.


  MergeCatalogJSON('mercedes.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;


procedure TOBDOEMExtensionMercedes.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  MergeExtendedCatalogJSON('mercedes.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;
function TOBDOEMExtensionMercedes.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  Mileage: Cardinal;
  Seconds: Cardinal;
begin
  case DID of
    $F190:
      if Length(Payload) > 0 then
      begin
        Result := Format('vin = %s', [TEncoding.ASCII.GetString(Payload)]);
        Exit;
      end;
    $F18B:
      if Length(Payload) >= 3 then
      begin
        // BCD-encoded YYMMDD.
        Result := Format('manufacturing_date = 20%.2x-%.2x-%.2x',
          [Payload[0], Payload[1], Payload[2]]);
        Exit;
      end;
    $0202:
      if Length(Payload) >= 3 then
      begin
        Mileage := (Cardinal(Payload[0]) shl 16) or
                   (Cardinal(Payload[1]) shl 8)  or
                    Cardinal(Payload[2]);
        Result := Format('mileage = %d km', [Mileage]);
        Exit;
      end;
    $0203:
      if Length(Payload) >= 4 then
      begin
        Seconds := (Cardinal(Payload[0]) shl 24) or
                   (Cardinal(Payload[1]) shl 16) or
                   (Cardinal(Payload[2]) shl 8)  or
                    Cardinal(Payload[3]);
        Result := Format('engine_running = %d s (%.1f h)',
          [Seconds, Seconds / 3600.0]);
        Exit;
      end;
    $F19E:
      if Length(Payload) >= 1 then
      begin
        case Payload[0] of
          $00: Result := 'programming_status = OK';
          $01: Result := 'programming_status = INTERRUPTED';
          $02: Result := 'programming_status = CHECKSUM_INVALID';
          $03: Result := 'programming_status = HW_INCOMPATIBLE';
        else
          Result := Format('programming_status = 0x%.2X', [Payload[0]]);
        end;
        Exit;
      end;
  end;
  Result := inherited DecodeDID(DID, Payload);
end;

initialization
  TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMercedes.Create);

end.
