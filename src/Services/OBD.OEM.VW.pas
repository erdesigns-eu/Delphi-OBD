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
  System.SysUtils, OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
  OBD.OEM.DTC;

type
  /// <summary>VAG-specific session negotiator. Adds the
  /// <c>AT SH &lt;ECU&gt;</c> + <c>AT CRA &lt;ECU+8&gt;</c> handshake
  /// before the UDS request because VCDS and ODIS have always set
  /// both, and some MQB-platform ECUs reject the session-control
  /// request if CRA is wrong. Tester-present interval is 2000 ms
  /// (matches ODIS service mode 2).</summary>
  TOBDVWSessionNegotiator = class(TOBDStandardSessionNegotiator)
  public
    function BeginSessionPlan(SessionType: TOBDSessionType;
      const ECUAddress: Word): TOBDSessionPlan; override;
    function DisplayName: string; override;
  end;

  TOBDOEMExtensionVW = class(TOBDOEMExtensionBase)
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

function TOBDVWSessionNegotiator.BeginSessionPlan(
  SessionType: TOBDSessionType;
  const ECUAddress: Word): TOBDSessionPlan;
var
  Sub: Byte;
  Steps: TArray<TOBDSessionStep>;
  ResponseAddr: Word;
begin
  Result := Default(TOBDSessionPlan);
  Sub := SessionTypeByte(SessionType);
  Steps := nil;

  if ECUAddress <> 0 then
  begin
    ResponseAddr := ECUAddress + 8;  // ISO 15765-4: response = request + 8
    Steps := Steps + [
      ATStep('SH ' + FormatHeader(ECUAddress),
             Format('Set request header to 0x%s', [FormatHeader(ECUAddress)])),
      ATStep('CRA ' + FormatHeader(ResponseAddr),
             Format('Filter response to 0x%s', [FormatHeader(ResponseAddr)]))
    ];
  end;

  Steps := Steps + [
    UDSStep(TBytes.Create($10, Sub), TBytes.Create($50, Sub),
      Format('VAG DiagnosticSessionControl 0x%.2X', [Sub]))
  ];

  Result.Steps := Steps;
  if SessionType = sstDefault then
    Result.TesterPresentMs := 0
  else
    Result.TesterPresentMs := DefaultTesterPresentMs;
  Result.TesterPresentRequest := TBytes.Create($3E, $80);
end;

function TOBDVWSessionNegotiator.DisplayName: string;
begin
  Result := 'VAG (ODIS / VCDS)';
end;

function TOBDOEMExtensionVW.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDVWSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionVW.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Legacy VAG KWP2000 components (pre-2008 instrument clusters,
  // some MED9 ECMs) accept the textbook two's-complement at Level 1.
  // Verified=False — production users plug in their proprietary
  // algorithm via RegisterAlgorithm.
  Reg.RegisterAlgorithm($01, TOBDSeedKeyKWP2000TwosComplement.Create);
end;

procedure TOBDOEMExtensionVW.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  inherited;
  MergeDtcCatalog('dtc-iso-15031.json', Cat);
  MergeDtcCatalog(DtcCatalogFileName, Cat);
end;

function TOBDOEMExtensionVW.DtcCatalogFileName: string;
begin Result := 'dtc-vw.json'; end;

function TOBDOEMExtensionVW.ManufacturerKey: string; begin Result := 'VAG'; end;
function TOBDOEMExtensionVW.DisplayName: string; begin Result := 'Volkswagen Audi Group'; end;

function TOBDOEMExtensionVW.ApplicableToVIN(const VIN: string): Boolean;
begin
  // v3.31 — JSON-only. The applicable_wmis list lives in vw.json,
  // so adding / removing a brand WMI is a JSON edit, no recompile.
  Result := VINMatchesCatalog('vw.json', VIN);
end;

procedure TOBDOEMExtensionVW.BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
  var Routines: TArray<TOBDOEMRoutine>;
  var ECUs: TArray<TOBDOEMECU>);
begin
  // v3.31 — JSON-only. No hardcoded ECU / DID / Routine arrays in
  // Pascal. The vw.json catalog is the sole source of truth so
  // updates ship as JSON edits, no recompile needed, and porting to
  // other languages only requires a JSON parser.
  MergeCatalogJSON('vw.json', DIDs, Routines, ECUs);
  MergeCatalogJSON('uds-standard.json', DIDs, Routines, ECUs);
end;

procedure TOBDOEMExtensionVW.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  // v3.31 — coding blocks, adaptations, actuator tests, live PIDs
  // and DTC extended-data records all live in vw.json.
  MergeExtendedCatalogJSON('vw.json',
    CodingBlocks, Adaptations, ActuatorTests, LivePIDs, DtcExtended);
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
