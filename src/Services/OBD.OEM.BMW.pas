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
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>
  ///   BMW E-Sys / ISTA choreography. Both the extended-diagnostic
  ///   and programming sessions expect a SecurityAccess (27 01) right
  ///   after the session-control reply for any coding or programming.
  ///   The negotiator itself only emits 10 03 / 10 02 — the actual
  ///   security-access seed/key dance happens in Phase 1.4. Tester-
  ///   present interval is 2000 ms by default but BMW recommends 1500
  ///   for older (E-series) DMEs.
  /// </summary>
  TOBDBMWSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function RequiresSecurityAccess(SessionType: TOBDSessionType): Boolean; override;
    function DefaultTesterPresentMs: Cardinal; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionBMW = class(TOBDOEMExtensionBase)
  protected
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var
        Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); override;
    procedure BuildExtendedCatalog(
      var
        CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var
        Adaptations: TArray<TOBDOEMAdaptation>;
      var
        ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var
        LivePIDs: TArray<TOBDOEMLivePID>;
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

//------------------------------------------------------------------------------
// REQUIRES SECURITY ACCESS
//------------------------------------------------------------------------------
function TOBDBMWSessionNegotiator.RequiresSecurityAccess(
  SessionType: TOBDSessionType): Boolean;
begin
  // E-Sys treats both extended-diagnostic and programming as
  // privileged: most coding writes need 27 01 success first.
  Result := SessionType in [sstExtendedDiagnostic, sstProgramming,
                            sstOEMSpecific1, sstOEMSpecific2];
end;

//------------------------------------------------------------------------------
// DEFAULT TESTER PRESENT MS
//------------------------------------------------------------------------------
function TOBDBMWSessionNegotiator.DefaultTesterPresentMs: Cardinal;
begin
  Result := 1500;  // E-series DMEs occasionally drop sessions at 2000 ms.
end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDBMWSessionNegotiator.DisplayName: string;
begin
  Result := 'BMW E-Sys / ISTA';
end;

//------------------------------------------------------------------------------
// CREATE SESSION NEGOTIATOR
//------------------------------------------------------------------------------
function TOBDOEMExtensionBMW.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDBMWSessionNegotiator.Create;
end;

//------------------------------------------------------------------------------
// SEED DEFAULT SEED KEY ALGORITHMS
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBMW.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
const
  // Documented placeholder XOR mask — used by the bimmer-utility
  // tutorial flow to demonstrate the SecurityAccess exchange against
  // a simulated DME. Real E-Sys algorithms ship inside ISTA and are
  // NDA-protected; production users replace this entry.
  PUBLIC_MASK: array[0..3] of Byte = ($A5, $5A, $C3, $3C);
var
  Mask: TBytes;
begin
  SetLength(Mask, Length(PUBLIC_MASK));
  Move(PUBLIC_MASK[0], Mask[0], Length(PUBLIC_MASK));
  Reg.RegisterAlgorithm($01, TOBDSeedKeyXorMask.Create(Mask,
    'BMW community XOR-mask placeholder', 'community-pr', False));
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBMW.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionBMW.DtcCatalogFileName: string;
begin
  Result := 'dtc-bmw.json';
end;

//------------------------------------------------------------------------------
// MANUFACTURER KEY
//------------------------------------------------------------------------------
function TOBDOEMExtensionBMW.ManufacturerKey: string; begin Result := 'BMW'; end;

//------------------------------------------------------------------------------
// DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionBMW.DisplayName: string; begin Result := 'Bayerische Motoren Werke'; end;

//------------------------------------------------------------------------------
// APPLICABLE TO VIN
//------------------------------------------------------------------------------
function TOBDOEMExtensionBMW.ApplicableToVIN(const VIN: string): Boolean;
begin
  // JSON-only: applicable_wmis lives in bmw.json.
  Result := VINMatchesCatalog('bmw.json', VIN);
end;

//------------------------------------------------------------------------------
// BUILD CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBMW.BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
  var
    Routines: TArray<TOBDOEMRoutine>;
  var
    ECUs: TArray<TOBDOEMECU>);
begin
  // JSON-only — sole sources of truth are bmw.json + uds-standard.json.
  MergeCatalogJSON('bmw.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBMW.BuildExtendedCatalog(
  var
    CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var
    Adaptations: TArray<TOBDOEMAdaptation>;
  var
    ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var
    LivePIDs: TArray<TOBDOEMLivePID>;
  var
    DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  // Coding blocks, adaptations, actuator tests, live PIDs and DTC
  // extended-data records all live in bmw.json.
  MergeExtendedCatalogJSON('bmw.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
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
