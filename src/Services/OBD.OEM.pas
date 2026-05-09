//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.pas
// CONTENTS       : OEM extension framework
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Manufacturer-specific UDS extensions, DID maps and
//                  routine-control catalogs live behind the
//                  IOBDOEMExtension contract. The registry resolves an
//                  extension from a VIN's WMI prefix or an explicit
//                  manufacturer name. The shipped reference extensions
//                  (VW group, BMW) cover only a handful of common DIDs;
//                  production users compose by inheriting and adding
//                  their own.
//------------------------------------------------------------------------------
unit OBD.OEM;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  OBD.OEM.Session, OBD.OEM.SeedKey, OBD.OEM.DTC;

type
  /// <summary>
  ///   One entry in an OEM's Data Identifier (DID) catalog.
  /// </summary>
  TOBDOEMDataIdentifier = record
    DID: Word;
    Name: string;        // human-readable ("battery_voltage")
    Description: string; // longer prose
    /// <summary>
    ///   UDS request address that owns this DID. 0 = global /
    ///   applies across all ECUs (the v3.3 flat-catalog default).
    /// </summary>
    EcuAddress: Word;
  end;

  /// <summary>
  ///   One entry in an OEM's RoutineControl (SID $31) catalog.
  /// </summary>
  TOBDOEMRoutine = record
    Identifier: Word;
    Name: string;
    Description: string;
    /// <summary>
    ///   UDS request address that owns this routine. 0 = global.
    /// </summary>
    EcuAddress: Word;
  end;

  /// <summary>
  ///   One ECU on the vehicle bus. <c>Address</c> is the UDS physical
  ///   request CAN-ID (e.g. <c>0x7E0</c> for engine on 11-bit ISO
  ///   15765-4). <c>Name</c> is the snake_case key (<c>engine</c>,
  ///   <c>transmission</c>, <c>abs</c>, <c>cluster</c>); <c>CommonName</c>
  ///   is the display label (<c>Engine ECU</c>, <c>Motor Steuergerät</c>).
  /// </summary>
  TOBDOEMECU = record
    Address: Word;
    Name: string;
    CommonName: string;
  end;

  /// <summary>
  ///   The subset of an OEM catalog that applies to a single ECU.
  ///   Returned by <c>IOBDOEMExtension.CatalogForECU</c>; entries with
  ///   <c>EcuAddress = 0</c> in the parent catalog are inherited as
  ///   global, ECU-scoped entries are added on top.
  /// </summary>
  TOBDOEMSubCatalog = record
    EcuAddress: Word;
    DIDs: TArray<TOBDOEMDataIdentifier>;
    Routines: TArray<TOBDOEMRoutine>;
  end;

  //----------------------------------------------------------------------------
  // v3.29 Phase A — extended catalog schema
  //----------------------------------------------------------------------------

  /// <summary>
  ///   Decoder kinds shared between live-PID and DTC extended-data
  ///   records. Mirrors the JSON loader's <c>TOBDDecoderKind</c> so the
  ///   extended schema can express its own decoders without pulling the
  ///   JSON-layer unit into <c>OBD.OEM</c>.
  /// </summary>
  TOBDOEMDecoderKind = (
    dkUnknown, dkAscii, dkHex, dkUInt8, dkUInt16BE, dkUInt32BE,
    dkInt16BE, dkInt32BE, dkBcdDate, dkEnum, dkBitmask, dkSeconds);

  /// <summary>
  ///   Field-type tag inside a writeable coding block. Sub-byte
  ///   fields use <c>cfkBit</c> (one bit) or <c>cfkEnum</c> /
  ///   <c>cfkBitmask</c> with <c>BitWidth</c>.
  /// </summary>
  TOBDCodingFieldKind = (
    cfkUnknown, cfkBit, cfkUInt8, cfkUInt16BE, cfkUInt32BE,
    cfkInt16BE, cfkInt32BE, cfkAscii, cfkEnum, cfkBitmask);

  /// <summary>
  ///   One field inside a writeable coding block. <c>ByteOffset</c>
  ///   is the byte position from the start of the block payload;
  ///   <c>BitOffset</c> + <c>BitWidth</c> are used for sub-byte fields. UI
  ///   renders bit/bool fields as checkboxes, enum as combo, numeric as
  ///   spinner, ASCII as text input.
  /// </summary>
  TOBDCodingField = record
    Name: string;
    Label_: string;            // human-readable for UI
    Description: string;
    Kind: TOBDCodingFieldKind;
    ByteOffset: Integer;
    BitOffset: Integer;        // 0..7 (only meaningful for cfkBit / sub-byte cfkEnum)
    BitWidth: Integer;         // 1..N — when 0, defaults from Kind
    DefaultValue: Int64;       // baseline / "factory" value (numeric kinds)
    DefaultAscii: string;      // for cfkAscii
    MinValue: Int64;
    MaxValue: Int64;
    EnumValues: TArray<TPair<Integer, string>>;
  end;

  /// <summary>
  ///   Writeable DID with a known bit-field structure. Coding
  ///   tools render this as a form: read the current payload, surface the
  ///   fields, capture edits, write the modified payload back via
  ///   <c>2E DID-hi DID-lo …</c>.
  /// </summary>
  TOBDOEMCodingBlock = record
    DataIdentifier: Word;
    Name: string;
    Description: string;
    EcuAddress: Word;
    PayloadSize: Integer;      // expected length of the writeable block
    Fields: TArray<TOBDCodingField>;
  end;

  TOBDAdaptationKind = (
    adkUnknown, adkUInt8, adkUInt16BE, adkUInt32BE,
    adkInt16BE, adkInt32BE, adkEnum);

  /// <summary>
  ///   One numbered adaptation channel (VAG-style). Read with
  ///   SID 0x22, write with SID 0x2E. <c>MinValue</c> / <c>MaxValue</c> /
  ///   <c>DefaultValue</c> let a coding tool clamp inputs and offer a
  ///   "reset to factory" affordance.
  /// </summary>
  TOBDOEMAdaptation = record
    Channel: Word;
    Name: string;
    Description: string;
    EcuAddress: Word;
    Kind: TOBDAdaptationKind;
    MinValue: Int64;
    MaxValue: Int64;
    DefaultValue: Int64;
    Unit_: string;
    EnumValues: TArray<TPair<Integer, string>>;
  end;

  TOBDActuatorResponseKind = (
    arkNone, arkBoolean, arkUInt8, arkUInt16BE, arkAscii);

  /// <summary>
  ///   Forced-output actuation step ("cycle the cooling fan",
  ///   "fire injector 3 once"). Most OEMs bind these to RoutineControl
  ///   (SID 0x31) — <c>Identifier</c> is then the RID. <c>SafetyWarning</c>
  ///   surfaces in the UI before the tool fires the actuation.
  /// </summary>
  TOBDOEMActuatorTest = record
    Identifier: Word;
    Name: string;
    Description: string;
    EcuAddress: Word;
    DurationMs: Cardinal;
    SafetyWarning: string;
    ExpectedResponseKind: TOBDActuatorResponseKind;
    ExpectedResponseLabel: string;
  end;

  TOBDLivePIDMode = (lpmUnknown, lpmService01, lpmService22);

  /// <summary>
  ///   One streamable PID. <c>Service01</c> PIDs follow J1979 /
  ///   ISO 15031-5 framing (<c>01 PID</c>); <c>Service22</c> PIDs are
  ///   16-bit OEM PIDs (<c>22 PID-hi PID-lo</c>) typical for OBD-II
  ///   extended modes. <c>FrameOffset</c> is the byte offset into the
  ///   response payload at which this signal starts.
  /// </summary>
  TOBDOEMLivePID = record
    Mode: TOBDLivePIDMode;
    PID: Word;
    Name: string;
    Description: string;
    EcuAddress: Word;
    FrameOffset: Integer;
    DecoderKind: TOBDOEMDecoderKind;
    Scale: Double;
    Offset: Double;
    Unit_: string;
  end;

  TOBDDtcExtendedDataKind = (
    xdkUnknown, xdkOccurrenceCounter, xdkAgingCounter,
    xdkMilesSinceCleared, xdkFreezeFrameTemplate,
    xdkOemStatusByte, xdkEnvironmentalData);

  /// <summary>
  ///   One extended-data record attached to a DTC. UDS service
  ///   0x19 sub-function 0x06 retrieves these on demand. The catalog
  ///   describes the layout so a tool can render the record after
  ///   reading it.
  /// </summary>
  TOBDDtcExtendedDataRecord = record
    DtcCode: string;
    RecordNumber: Byte;        // sub-record number for SID 19 06
    Kind: TOBDDtcExtendedDataKind;
    Description: string;
    DecoderKind: TOBDOEMDecoderKind;
    Scale: Double;
    Offset: Double;
    Unit_: string;
  end;

  /// <summary>
  ///   Companion to <c>IOBDOEMExtension</c>. Adds accessors for
  ///   the v3.29 extended catalog: coding blocks, adaptations, actuator
  ///   tests, live PIDs, DTC extended-data records. Implemented by
  ///   <c>TOBDOEMExtensionBase</c> on every OEM extension; tooling
  ///   queries via <c>Supports(Ext, IOBDOEMExtensionV2, V2)</c>.
  /// </summary>
  IOBDOEMExtensionV2 = interface
    ['{2C8B6F0E-7A3D-4C5E-8B9A-1F4E6D2A8C90}']
    function CodingBlocks: TArray<TOBDOEMCodingBlock>;
    function Adaptations: TArray<TOBDOEMAdaptation>;
    function ActuatorTests: TArray<TOBDOEMActuatorTest>;
    function LivePIDs: TArray<TOBDOEMLivePID>;
    function DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>;
  end;

  /// <summary>
  ///   Implemented by every manufacturer-specific extension. The
  ///   contract is intentionally narrow: the OBD framework calls the
  ///   extension to look up DIDs, routines, and any custom session
  ///   negotiation; it does NOT delegate flashing, security access, or
  ///   bus arbitration through this interface (those stay in
  ///   <c>OBD.ECU.Flashing</c>).
  /// </summary>
  IOBDOEMExtension = interface
    ['{A2C5F4C6-4D71-4E8F-9C5B-3E4A8B1D6C2F}']
    /// <summary>
    ///   Manufacturer key — short ASCII tag, e.g. "VAG", "BMW".
    /// </summary>
    function ManufacturerKey: string;
    /// <summary>
    ///   Display-friendly name, e.g. "Volkswagen Audi Group".
    /// </summary>
    function DisplayName: string;

    /// <summary>
    ///   True if this extension claims a VIN. The framework probes
    ///   every registered extension; the first that returns True wins.
    /// </summary>
    function ApplicableToVIN(const VIN: string): Boolean;

    /// <summary>
    ///   True if this extension claims an ECU based on its component
    ///   identification. Engine OEMs (Cummins, Detroit Diesel) and
    ///   any OEM that ships engines / modules into other vehicle
    ///   manufacturers' chassis use this branch since they don't
    ///   issue WMIs of their own. <c>SupplierID</c> is what the ECU
    ///   returns from J1939 PGN 65259 'Make' or ISO 14229 DID 0xF18A
    ///   (system_supplier_identifier). The default base implementation
    ///   returns False, so the contract is upward-compatible: only
    ///   extensions that opt in respond to this probe.
    /// </summary>
    function ApplicableToECUSupplier(const SupplierID: string): Boolean;

    /// <summary>
    ///   Catalog of DIDs the extension knows how to interpret.
    /// </summary>
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
    /// <summary>
    ///   Catalog of RoutineControl identifiers.
    /// </summary>
    function Routines: TArray<TOBDOEMRoutine>;

    /// <summary>
    ///   Decode a raw DID payload to a human-readable string. Default
    ///   implementations hex-dump unknown DIDs; specialised ones return
    ///   structured data (e.g. battery voltage in volts).
    /// </summary>
    function DecodeDID(const DID: Word; const Payload: TBytes): string;

    /// <summary>
    ///   Lookup helpers — return False if the entry isn't catalogued.
    /// </summary>
    function FindDID(const DID: Word; out Entry: TOBDOEMDataIdentifier): Boolean;
    function FindRoutine(const Id: Word; out Entry: TOBDOEMRoutine): Boolean;

    /// <summary>
    ///   The ECUs this manufacturer's diagnostics target. May be
    ///   empty for OEMs that haven't been ECU-mapped yet — callers then
    ///   fall back to the flat catalog.
    /// </summary>
    function ECUs: TArray<TOBDOEMECU>;

    /// <summary>
    ///   Catalog filtered to one ECU. Includes globally-scoped
    ///   entries (EcuAddress=0 in the flat catalog) plus entries that
    ///   match <c>Address</c> exactly.
    /// </summary>
    function CatalogForECU(const Address: Word): TOBDOEMSubCatalog;

    /// <summary>
    ///   The session-negotiation choreography this OEM expects.
    ///   The default is <c>TOBDStandardSessionNegotiator</c> (plain ISO
    ///   14229); OEMs that diverge return their own implementation.
    /// </summary>
    function SessionNegotiator: IOBDSessionNegotiator;

    /// <summary>
    ///   Per-OEM seed-key algorithm registry keyed by
    ///   SecurityAccess level (the odd byte in <c>27 LL</c>). Production
    ///   users replace the default starter algorithm with their NDA-
    ///   protected real one via <c>RegisterAlgorithm</c>.
    /// </summary>
    function SeedKeyRegistry: TOBDSeedKeyRegistry;

    /// <summary>
    ///   Per-OEM DTC catalog. The standard SAE J2012 / ISO
    ///   15031-6 P0xxx range is loaded as a baseline overlay; OEM
    ///   units add their P1xxx / B / C / U entries. Returns the same
    ///   catalog instance across calls — callers can register
    ///   additional entries at runtime.
    /// </summary>
    function DtcCatalog: TOBDDtcCatalog;

    /// <summary>
    ///   Convenience: look up <c>Code</c> in <c>DtcCatalog</c>.
    /// </summary>
    function DescribeDTC(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean;
  end;

  /// <summary>
  ///   Process-wide registry. Extensions register themselves at unit
  ///   initialization so that simply adding an extension's unit to a
  ///   project's uses clause activates it.
  /// </summary>
  TOBDOEMRegistry = class
  strict private class var
    FLock: TCriticalSection;
    FExtensions: TList<IOBDOEMExtension>;
  private
    class procedure EnsureInitialized;
  public
    class procedure RegisterExtension(const Ext: IOBDOEMExtension); static;
    class procedure UnregisterExtension(const Ext: IOBDOEMExtension); static;
    class function FindByVIN(const VIN: string): IOBDOEMExtension; static;
    class function FindByKey(const ManufacturerKey: string): IOBDOEMExtension; static;
    /// <summary>
    ///   Probe every extension's <c>ApplicableToECUSupplier</c>
    ///   (J1939 PGN 65259 'Make' / ISO 14229 DID 0xF18A). Returns the
    ///   first claimant or nil. Use this when the chassis VIN doesn't
    ///   identify the ECU manufacturer (engine OEMs in mixed fleets,
    ///   supplier modules in OEM chassis).
    /// </summary>
    class function FindByECUSupplier(const SupplierID: string): IOBDOEMExtension; static;
    class function All: TArray<IOBDOEMExtension>; static;
    class function Count: Integer; static;
    class procedure Clear; static;
  end;

  /// <summary>
  ///   Convenience base class with sensible defaults for the lookup,
  ///   decode, and catalog plumbing. Concrete OEMs override
  ///   <c>ManufacturerKey</c>, <c>DisplayName</c>, <c>ApplicableToVIN</c>,
  ///   and the catalog loaders.
  /// </summary>
  TOBDOEMExtensionBase = class(TInterfacedObject, IOBDOEMExtension, IOBDOEMExtensionV2)
  strict private
    FDIDs: TArray<TOBDOEMDataIdentifier>;
    FRoutines: TArray<TOBDOEMRoutine>;
    FECUs: TArray<TOBDOEMECU>;
    FCodingBlocks: TArray<TOBDOEMCodingBlock>;
    FAdaptations: TArray<TOBDOEMAdaptation>;
    FActuatorTests: TArray<TOBDOEMActuatorTest>;
    FLivePIDs: TArray<TOBDOEMLivePID>;
    FDtcExtended: TArray<TOBDDtcExtendedDataRecord>;
    FCatalogLoaded: Boolean;
    FCatalogLock: TCriticalSection;
    FSessionNegotiator: IOBDSessionNegotiator;
    FSessionLock: TCriticalSection;
    FSeedKeyRegistry: TOBDSeedKeyRegistry;
    FSeedKeyLock: TCriticalSection;
    FDtcCatalog: TOBDDtcCatalog;
    FDtcLock: TCriticalSection;
    procedure EnsureCatalog;
    procedure EnsureSeedKeyRegistry;
    procedure EnsureDtcCatalog;
  protected
    /// <summary>
    ///   Subclasses populate <c>DIDs</c>, <c>Routines</c>, and the
    ///   optional <c>ECUs</c> bus map on first access. Lazy so unit-init
    ///   isn't slowed by building catalogs that may never be queried.
    ///   The third parameter was added in v3.4 (Phase 1.2) for per-ECU
    ///   sub-catalogs; subclasses that don't need it just leave it
    ///   untouched (default <c>nil</c>).
    /// </summary>
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var
        Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); virtual; abstract;
    /// <summary>
    ///   v3.29 Phase A — override-point for the extended
    ///   catalog. Default is a no-op so the 46 v3.28-era OEM extensions
    ///   continue to compile unchanged. Subclasses that opt in populate
    ///   the arrays from JSON via <c>MergeExtendedCatalogJSON</c> in
    ///   <c>OBD.OEM.Catalog.Loader</c>.
    /// </summary>
    procedure BuildExtendedCatalog(
      var
        CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var
        Adaptations: TArray<TOBDOEMAdaptation>;
      var
        ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var
        LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function ManufacturerKey: string; virtual; abstract;
    function DisplayName: string; virtual; abstract;
    function ApplicableToVIN(const VIN: string): Boolean; virtual; abstract;
    function ApplicableToECUSupplier(const SupplierID: string): Boolean; virtual;
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>; virtual;
    function Routines: TArray<TOBDOEMRoutine>; virtual;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; virtual;
    function FindDID(const DID: Word; out Entry: TOBDOEMDataIdentifier): Boolean; virtual;
    function FindRoutine(const Id: Word; out Entry: TOBDOEMRoutine): Boolean; virtual;
    function ECUs: TArray<TOBDOEMECU>; virtual;
    function CatalogForECU(const Address: Word): TOBDOEMSubCatalog; virtual;
    function SessionNegotiator: IOBDSessionNegotiator; virtual;
    function SeedKeyRegistry: TOBDSeedKeyRegistry; virtual;
    function DtcCatalog: TOBDDtcCatalog; virtual;
    function DescribeDTC(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean; virtual;
    // IOBDOEMExtensionV2
    function CodingBlocks: TArray<TOBDOEMCodingBlock>; virtual;
    function Adaptations: TArray<TOBDOEMAdaptation>; virtual;
    function ActuatorTests: TArray<TOBDOEMActuatorTest>; virtual;
    function LivePIDs: TArray<TOBDOEMLivePID>; virtual;
    function DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>; virtual;
  protected
    /// <summary>
    ///   Override-point: subclasses return their OEM-specific
    ///   negotiator. Default returns a fresh
    ///   <c>TOBDStandardSessionNegotiator</c>.
    /// </summary>
    function CreateSessionNegotiator: IOBDSessionNegotiator; virtual;
    /// <summary>
    ///   Override-point: subclasses populate <c>Reg</c> with
    ///   their default starter algorithms. Called once on first access
    ///   to <c>SeedKeyRegistry</c>. Default is a no-op (empty registry).
    /// </summary>
    procedure SeedDefaultSeedKeyAlgorithms(Reg: TOBDSeedKeyRegistry); virtual;
    /// <summary>
    ///   Override-point: subclasses load their per-OEM DTC
    ///   catalog into <c>Cat</c>. Default loads the universal SAE J2012
    ///   / ISO 15031-6 baseline (<c>catalogs/dtc-iso-15031.json</c>);
    ///   OEM units chain to <c>inherited</c> and append their own.
    /// </summary>
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); virtual;
    /// <summary>
    ///   The catalog filename loaded by the default
    ///   <c>SeedDefaultDtcCatalog</c>; OEMs override to point at their
    ///   own per-OEM file (e.g. <c>'dtc-vw.json'</c>).
    /// </summary>
    function DtcCatalogFileName: string; virtual;
  end;

/// <summary>
///   Builder helper used by JSON catalog readers.
/// </summary>
function MakeOEMECU(const Address: Word; const Name, CommonName: string): TOBDOEMECU;

implementation

//==============================================================================
// TOBDOEMRegistry
//==============================================================================

//------------------------------------------------------------------------------
// ENSURE INITIALIZED
//------------------------------------------------------------------------------
class procedure TOBDOEMRegistry.EnsureInitialized;
begin
  if FLock = nil then
    FLock := TCriticalSection.Create;
  if FExtensions = nil then
    FExtensions := TList<IOBDOEMExtension>.Create;
end;

//------------------------------------------------------------------------------
// REGISTER EXTENSION
//------------------------------------------------------------------------------
class procedure TOBDOEMRegistry.RegisterExtension(const Ext: IOBDOEMExtension);
begin
  if not Assigned(Ext) then Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    if not FExtensions.Contains(Ext) then
      FExtensions.Add(Ext);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// UNREGISTER EXTENSION
//------------------------------------------------------------------------------
class procedure TOBDOEMRegistry.UnregisterExtension(const Ext: IOBDOEMExtension);
begin
  if not Assigned(Ext) then Exit;
  EnsureInitialized;
  FLock.Enter;
  try FExtensions.Remove(Ext); finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// FIND BY VIN
//------------------------------------------------------------------------------
class function TOBDOEMRegistry.FindByVIN(const VIN: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try Snapshot := FExtensions.ToArray; finally FLock.Leave; end;
  // Snapshot under the lock so a concurrent register/unregister can't
  // invalidate the iteration.
  for Ext in Snapshot do
    if Ext.ApplicableToVIN(VIN) then Exit(Ext);
end;

//------------------------------------------------------------------------------
// FIND BY KEY
//------------------------------------------------------------------------------
class function TOBDOEMRegistry.FindByKey(
  const ManufacturerKey: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try Snapshot := FExtensions.ToArray; finally FLock.Leave; end;
  for Ext in Snapshot do
    if SameText(Ext.ManufacturerKey, ManufacturerKey) then Exit(Ext);
end;

//------------------------------------------------------------------------------
// FIND BY ECUSUPPLIER
//------------------------------------------------------------------------------
class function TOBDOEMRegistry.FindByECUSupplier(
  const SupplierID: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  if SupplierID = '' then Exit;
  EnsureInitialized;
  FLock.Enter;
  try Snapshot := FExtensions.ToArray; finally FLock.Leave; end;
  for Ext in Snapshot do
    if Ext.ApplicableToECUSupplier(SupplierID) then Exit(Ext);
end;

//------------------------------------------------------------------------------
// ALL
//------------------------------------------------------------------------------
class function TOBDOEMRegistry.All: TArray<IOBDOEMExtension>;
begin
  EnsureInitialized;
  FLock.Enter;
  try Result := FExtensions.ToArray; finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// COUNT
//------------------------------------------------------------------------------
class function TOBDOEMRegistry.Count: Integer;
begin
  EnsureInitialized;
  FLock.Enter;
  try Result := FExtensions.Count; finally FLock.Leave; end;
end;

//------------------------------------------------------------------------------
// CLEAR
//------------------------------------------------------------------------------
class procedure TOBDOEMRegistry.Clear;
begin
  EnsureInitialized;
  FLock.Enter;
  try FExtensions.Clear; finally FLock.Leave; end;
end;

//==============================================================================
// TOBDOEMExtensionBase
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDOEMExtensionBase.Create;
begin
  inherited Create;
  FCatalogLock := TCriticalSection.Create;
  FSessionLock := TCriticalSection.Create;
  FSeedKeyLock := TCriticalSection.Create;
  FDtcLock := TCriticalSection.Create;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDOEMExtensionBase.Destroy;
begin
  FSessionNegotiator := nil;
  FDtcCatalog.Free;
  FDtcLock.Free;
  FSeedKeyRegistry.Free;
  FSeedKeyLock.Free;
  FSessionLock.Free;
  FCatalogLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SEED DEFAULT SEED KEY ALGORITHMS
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Default: no algorithms. Subclasses override to register their
  // starter set; production users call RegisterAlgorithm after.
end;

//------------------------------------------------------------------------------
// ENSURE SEED KEY REGISTRY
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.EnsureSeedKeyRegistry;
begin
  FSeedKeyLock.Enter;
  try
    if FSeedKeyRegistry = nil then
    begin
      FSeedKeyRegistry := TOBDSeedKeyRegistry.Create;
      SeedDefaultSeedKeyAlgorithms(FSeedKeyRegistry);
    end;
  finally
    FSeedKeyLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// SEED KEY REGISTRY
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.SeedKeyRegistry: TOBDSeedKeyRegistry;
begin
  EnsureSeedKeyRegistry;
  Result := FSeedKeyRegistry;
end;

//------------------------------------------------------------------------------
// DTC CATALOG FILE NAME
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DtcCatalogFileName: string;
begin
  // Override in OEM subclasses to point at the per-OEM DTC catalog;
  // empty string means "no OEM overlay, baseline only".
  Result := '';
end;

//------------------------------------------------------------------------------
// SEED DEFAULT DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  // Subclasses chain to inherited and append their own entries; the
  // base behaviour is to leave the catalog empty here. The actual
  // file loading lives in OBD.OEM.DTC.Loader so this unit stays
  // dependency-free.
end;

//------------------------------------------------------------------------------
// ENSURE DTC CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.EnsureDtcCatalog;
begin
  FDtcLock.Enter;
  try
    if FDtcCatalog = nil then
    begin
      FDtcCatalog := TOBDDtcCatalog.Create;
      SeedDefaultDtcCatalog(FDtcCatalog);
    end;
  finally
    FDtcLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// DTC CATALOG
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DtcCatalog: TOBDDtcCatalog;
begin
  EnsureDtcCatalog;
  Result := FDtcCatalog;
end;

//------------------------------------------------------------------------------
// DESCRIBE DTC
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DescribeDTC(const Code: string;
  out Entry: TOBDDtcCatalogEntry): Boolean;
begin
  Result := DtcCatalog.FindByCode(Code, Entry);
end;

//------------------------------------------------------------------------------
// CREATE SESSION NEGOTIATOR
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDStandardSessionNegotiator.Create;
end;

//------------------------------------------------------------------------------
// SESSION NEGOTIATOR
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.SessionNegotiator: IOBDSessionNegotiator;
begin
  // Lazy + cached. Negotiators are immutable and cheap, but caching
  // means callers can keep a reference across many calls without us
  // re-allocating; matches the catalog's lazy-build semantics.
  FSessionLock.Enter;
  try
    if FSessionNegotiator = nil then
      FSessionNegotiator := CreateSessionNegotiator;
    Result := FSessionNegotiator;
  finally
    FSessionLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// ENSURE CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.EnsureCatalog;
begin
  FCatalogLock.Enter;
  try
    if FCatalogLoaded then Exit;
    BuildCatalog(FDIDs, FRoutines, FECUs);
    BuildExtendedCatalog(FCodingBlocks, FAdaptations, FActuatorTests,
                         FLivePIDs, FDtcExtended);
    FCatalogLoaded := True;
  finally
    FCatalogLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// BUILD EXTENDED CATALOG
//------------------------------------------------------------------------------
procedure TOBDOEMExtensionBase.BuildExtendedCatalog(
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
  // Default: no-op. Subclasses (v3.29 Phase B onward) populate by
  // calling MergeExtendedCatalogJSON('xxx.json', ...) — same shape as
  // BuildCatalog's MergeCatalogJSON pattern.
end;

//------------------------------------------------------------------------------
// CODING BLOCKS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.CodingBlocks: TArray<TOBDOEMCodingBlock>;
begin
  EnsureCatalog;
  Result := FCodingBlocks;
end;

//------------------------------------------------------------------------------
// ADAPTATIONS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.Adaptations: TArray<TOBDOEMAdaptation>;
begin
  EnsureCatalog;
  Result := FAdaptations;
end;

//------------------------------------------------------------------------------
// ACTUATOR TESTS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.ActuatorTests: TArray<TOBDOEMActuatorTest>;
begin
  EnsureCatalog;
  Result := FActuatorTests;
end;

//------------------------------------------------------------------------------
// LIVE PIDS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.LivePIDs: TArray<TOBDOEMLivePID>;
begin
  EnsureCatalog;
  Result := FLivePIDs;
end;

//------------------------------------------------------------------------------
// DTC EXTENDED DATA RECORDS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>;
begin
  EnsureCatalog;
  Result := FDtcExtended;
end;

//------------------------------------------------------------------------------
// ECUS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.ECUs: TArray<TOBDOEMECU>;
begin
  EnsureCatalog;
  Result := FECUs;
end;

//------------------------------------------------------------------------------
// CATALOG FOR ECU
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.CatalogForECU(
  const Address: Word): TOBDOEMSubCatalog;
var
  D: TOBDOEMDataIdentifier;
  R: TOBDOEMRoutine;
begin
  EnsureCatalog;
  Result := Default(TOBDOEMSubCatalog);
  Result.EcuAddress := Address;
  // Globals (EcuAddress = 0) flow through to every ECU; ECU-scoped
  // entries are added when the address matches. Address = 0 returns
  // everything (callers asking "what's catalogued at all?").
  for D in FDIDs do
    if (D.EcuAddress = 0) or (Address = 0) or (D.EcuAddress = Address) then
      Result.DIDs := Result.DIDs + [D];
  for R in FRoutines do
    if (R.EcuAddress = 0) or (Address = 0) or (R.EcuAddress = Address) then
      Result.Routines := Result.Routines + [R];
end;

//------------------------------------------------------------------------------
// MAKE OEMECU
//------------------------------------------------------------------------------
function MakeOEMECU(const Address: Word; const Name, CommonName: string): TOBDOEMECU;
begin
  Result.Address := Address;
  Result.Name := Name;
  Result.CommonName := CommonName;
end;

//------------------------------------------------------------------------------
// APPLICABLE TO ECUSUPPLIER
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.ApplicableToECUSupplier(
  const SupplierID: string): Boolean;
begin
  // Default: extensions identify by VIN. Engine OEMs (Cummins,
  // Detroit Diesel) and supplier-only modules override this to
  // claim by component identification when the chassis VIN routes
  // elsewhere.
  Result := False;
end;

//------------------------------------------------------------------------------
// DATA IDENTIFIERS
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
begin
  EnsureCatalog;
  Result := FDIDs;
end;

//------------------------------------------------------------------------------
// ROUTINES
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.Routines: TArray<TOBDOEMRoutine>;
begin
  EnsureCatalog;
  Result := FRoutines;
end;

//------------------------------------------------------------------------------
// DECODE DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  I: Integer;
  Hex: TStringBuilder;
  Entry: TOBDOEMDataIdentifier;
begin
  // Default: format as "<name>: HEX BYTES" — concrete OEMs override
  // for known DIDs to apply unit conversions.
  Hex := TStringBuilder.Create;
  try
    for I := 0 to High(Payload) do
    begin
      if I > 0 then Hex.Append(' ');
      Hex.Append(IntToHex(Payload[I], 2));
    end;
    if FindDID(DID, Entry) then
      Result := Format('%s = %s', [Entry.Name, Hex.ToString])
    else
      Result := Format('DID 0x%.4X = %s', [DID, Hex.ToString]);
  finally
    Hex.Free;
  end;
end;

//------------------------------------------------------------------------------
// FIND DID
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.FindDID(const DID: Word;
  out Entry: TOBDOEMDataIdentifier): Boolean;
var
  E: TOBDOEMDataIdentifier;
begin
  EnsureCatalog;
  for E in FDIDs do
    if E.DID = DID then
    begin
      Entry := E;
      Exit(True);
    end;
  Result := False;
end;

//------------------------------------------------------------------------------
// FIND ROUTINE
//------------------------------------------------------------------------------
function TOBDOEMExtensionBase.FindRoutine(const Id: Word;
  out Entry: TOBDOEMRoutine): Boolean;
var
  R: TOBDOEMRoutine;
begin
  EnsureCatalog;
  for R in FRoutines do
    if R.Identifier = Id then
    begin
      Entry := R;
      Exit(True);
    end;
  Result := False;
end;

initialization

finalization
  if Assigned(TOBDOEMRegistry.FExtensions) then
  begin
    TOBDOEMRegistry.FExtensions.Free;
    TOBDOEMRegistry.FExtensions := nil;
  end;
  if Assigned(TOBDOEMRegistry.FLock) then
  begin
    TOBDOEMRegistry.FLock.Free;
    TOBDOEMRegistry.FLock := nil;
  end;

end.
