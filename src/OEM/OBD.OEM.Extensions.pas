//------------------------------------------------------------------------------
//  OBD.OEM.Extensions
//
//  IOBDOEMExtension contract + TOBDOEMExtensionBase convenience
//  base class + TOBDOEMExtensionRegistry process-wide registry.
//
//  Each manufacturer ships an <see cref="IOBDOEMExtension"/>
//  implementation (typically descending from
//  <see cref="TOBDOEMExtensionBase"/>) carrying its
//  ManufacturerKey, DisplayName, VIN-WMI prefixes / ECU-supplier
//  patterns, DID + Routine + ECU catalogues, coding blocks,
//  adaptations, actuator tests, live PIDs, DTC extended-data
//  records, a per-OEM seed-key registry, the OEM's session
//  negotiator and its DTC catalogue.
//
//  Extensions register themselves in their unit's
//  <c>initialization</c> block so simply adding the unit to a
//  host's <c>uses</c> clause activates the vendor.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//    2026-05-12  ERD  Build-catalog hooks + session-plan model.
//------------------------------------------------------------------------------

unit OBD.OEM.Extensions;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.OEM.Types,
  OBD.OEM.SeedKey,
  OBD.OEM.DTC,
  OBD.OEM.Session;

type
  /// <summary>Raised on extension-framework errors.</summary>
  EOBDOEMExtension = class(Exception);

  /// <summary>
  ///   Manufacturer-extension contract. One interface covering
  ///   identity, applicability, catalogues (DIDs, routines, ECUs,
  ///   coding blocks, adaptations, actuator tests, live PIDs and
  ///   DTC extended-data records), the session negotiator, the
  ///   seed-key registry and the DTC catalogue.
  /// </summary>
  /// <remarks>
  ///   <para>The framework calls the extension to look up DIDs,
  ///   routines and any custom session negotiation; it does NOT
  ///   delegate flashing, security access or bus arbitration
  ///   through this interface — those stay in
  ///   <c>OBD.ECU.Flashing</c>.</para>
  /// </remarks>
  IOBDOEMExtension = interface
    ['{A2C5F4C6-4D71-4E8F-9C5B-3E4A8B1D6C2F}']
    /// <summary>Short ASCII manufacturer tag (e.g. <c>'VAG'</c>,
    /// <c>'BMW'</c>, <c>'FORD'</c>).</summary>
    function ManufacturerKey: string;
    /// <summary>Display-friendly name.</summary>
    function DisplayName: string;

    /// <summary>
    ///   <c>True</c> when this extension claims <c>VIN</c>. The
    ///   framework probes every registered extension; the first
    ///   that returns <c>True</c> wins.
    /// </summary>
    /// <param name="VIN">17-character ISO 3779 VIN.</param>
    function ApplicableToVIN(const VIN: string): Boolean;
    /// <summary>
    ///   <c>True</c> when this extension claims an ECU based on
    ///   its component identification (J1939 PGN 65259 'Make' or
    ///   ISO 14229 DID 0xF18A system_supplier_identifier).
    ///   Engine OEMs and supplier-only modules opt in here.
    /// </summary>
    /// <param name="SupplierID">Supplier identifier.</param>
    function ApplicableToECUSupplier(const SupplierID: string): Boolean;

    /// <summary>Full DID catalogue.</summary>
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
    /// <summary>Full Routine catalogue.</summary>
    function Routines: TArray<TOBDOEMRoutine>;
    /// <summary>
    ///   Decodes a raw DID payload to a human-readable string.
    ///   Base implementation prints <c>"&lt;name&gt; = HEX"</c>;
    ///   subclasses override for structured decoding.
    /// </summary>
    /// <param name="DID">DID being decoded.</param>
    /// <param name="Payload">Response bytes.</param>
    function DecodeDID(const DID: Word; const Payload: TBytes): string;
    /// <summary>Looks up a DID.</summary>
    /// <param name="DID">DID value.</param>
    /// <param name="Entry">Out: matching entry on success.</param>
    function FindDID(const DID: Word;
      out Entry: TOBDOEMDataIdentifier): Boolean;
    /// <summary>Looks up a routine.</summary>
    /// <param name="Id">Routine ID.</param>
    /// <param name="Entry">Out: matching entry on success.</param>
    function FindRoutine(const Id: Word;
      out Entry: TOBDOEMRoutine): Boolean;

    /// <summary>ECUs this manufacturer's diagnostics target.
    /// May be empty for OEMs that have not been ECU-mapped.</summary>
    function ECUs: TArray<TOBDOEMECU>;
    /// <summary>Catalogue slice for one ECU (globals
    /// + ECU-scoped). <c>Address = 0</c> returns everything.</summary>
    /// <param name="Address">ECU CAN-ID.</param>
    function CatalogForECU(const Address: Word): TOBDOEMSubCatalog;

    /// <summary>Session-negotiation choreography for this
    /// OEM.</summary>
    function SessionNegotiator: IOBDSessionNegotiator;
    /// <summary>Per-OEM seed-key registry keyed by SecurityAccess
    /// level.</summary>
    function SeedKeyRegistry: TOBDSeedKeyRegistry;
    /// <summary>Per-OEM DTC catalogue.</summary>
    function DtcCatalog: TOBDDtcCatalog;
    /// <summary>Convenience: look up <c>Code</c> in
    /// <see cref="DtcCatalog"/>.</summary>
    /// <param name="Code">5-character DTC code.</param>
    /// <param name="Entry">Out: matching entry on success.</param>
    function DescribeDTC(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean;

    /// <summary>Writeable coding blocks.</summary>
    function CodingBlocks: TArray<TOBDOEMCodingBlock>;
    /// <summary>Adaptation channels.</summary>
    function Adaptations: TArray<TOBDOEMAdaptation>;
    /// <summary>Forced-output actuator tests.</summary>
    function ActuatorTests: TArray<TOBDOEMActuatorTest>;
    /// <summary>Streamable live PIDs.</summary>
    function LivePIDs: TArray<TOBDOEMLivePID>;
    /// <summary>DTC extended-data records (UDS 0x19 / 0x06).</summary>
    function DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>;
  end;

  /// <summary>
  ///   Process-wide registry. Extensions register themselves at
  ///   unit initialisation so simply adding a vendor's unit to a
  ///   project's <c>uses</c> clause activates it.
  /// </summary>
  /// <remarks>
  ///   <para>Reentrant. All accessors take a snapshot under the
  ///   internal lock so concurrent register / unregister calls
  ///   cannot invalidate an iteration.</para>
  /// </remarks>
  TOBDOEMExtensionRegistry = class
  strict private
    class var FLock: TCriticalSection;
    class var FExtensions: TList<IOBDOEMExtension>;
    class procedure EnsureInitialized; static;
  public
    /// <summary>Registers an extension. Duplicate registration
    /// is a no-op.</summary>
    /// <param name="Ext">Extension to add.</param>
    class procedure RegisterExtension(const Ext: IOBDOEMExtension); static;
    /// <summary>Removes an extension.</summary>
    /// <param name="Ext">Extension to drop.</param>
    class procedure UnregisterExtension(
      const Ext: IOBDOEMExtension); static;
    /// <summary>First extension whose
    /// <c>ApplicableToVIN(VIN)</c> returns <c>True</c>.</summary>
    /// <param name="VIN">17-character VIN.</param>
    class function FindByVIN(const VIN: string): IOBDOEMExtension; static;
    /// <summary>First extension whose <c>ManufacturerKey</c>
    /// matches (case-insensitive).</summary>
    /// <param name="ManufacturerKey">Manufacturer key.</param>
    class function FindByKey(
      const ManufacturerKey: string): IOBDOEMExtension; static;
    /// <summary>First extension that claims
    /// <c>SupplierID</c>.</summary>
    /// <param name="SupplierID">Supplier identifier.</param>
    class function FindByECUSupplier(
      const SupplierID: string): IOBDOEMExtension; static;
    /// <summary>Snapshot of every registered extension.</summary>
    class function All: TArray<IOBDOEMExtension>; static;
    /// <summary>Number of registered extensions.</summary>
    class function Count: Integer; static;
    /// <summary>Drops every registered extension.</summary>
    class procedure Clear; static;
    /// <summary>Releases registry state. Called from this unit's
    /// <c>finalization</c>; safe to call multiple times.</summary>
    class procedure Shutdown; static;
  end;

  /// <summary>
  ///   Convenience base class with sensible defaults for the
  ///   lookup, decode and catalogue plumbing. Concrete OEMs
  ///   override <c>ManufacturerKey</c>, <c>DisplayName</c>,
  ///   <c>ApplicableToVIN</c> and <c>BuildCatalog</c>;
  ///   <c>BuildExtendedCatalog</c>, <c>CreateSessionNegotiator</c>,
  ///   <c>SeedDefaultSeedKeyAlgorithms</c> and
  ///   <c>SeedDefaultDtcCatalog</c> are optional override-points.
  /// </summary>
  TOBDOEMExtensionBase = class(TInterfacedObject, IOBDOEMExtension)
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
    ///   Subclasses populate the three arrays on first access.
    ///   Lazy so unit-init isn't slowed by catalogues that may
    ///   never be queried. <c>ECUs</c> may be left empty.
    /// </summary>
    /// <param name="DIDs">Out: DID catalogue.</param>
    /// <param name="Routines">Out: routine catalogue.</param>
    /// <param name="ECUs">Out: bus map.</param>
    procedure BuildCatalog(
      var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>;
      var ECUs: TArray<TOBDOEMECU>); virtual; abstract;
    /// <summary>
    ///   Subclasses populate the extended catalogue (coding
    ///   blocks, adaptations, actuator tests, live PIDs, DTC
    ///   extended-data records). Default is a no-op so OEMs that
    ///   only need the base catalogue compile unchanged.
    /// </summary>
    /// <param name="CodingBlocks">Out: writeable blocks.</param>
    /// <param name="Adaptations">Out: adaptation channels.</param>
    /// <param name="ActuatorTests">Out: actuator tests.</param>
    /// <param name="LivePIDs">Out: streamable PIDs.</param>
    /// <param name="DtcExtended">Out: DTC extended-data records.</param>
    procedure BuildExtendedCatalog(
      var CodingBlocks: TArray<TOBDOEMCodingBlock>;
      var Adaptations: TArray<TOBDOEMAdaptation>;
      var ActuatorTests: TArray<TOBDOEMActuatorTest>;
      var LivePIDs: TArray<TOBDOEMLivePID>;
      var DtcExtended: TArray<TOBDDtcExtendedDataRecord>); virtual;
    /// <summary>Override-point: supply this OEM's session
    /// negotiator. Default returns a fresh
    /// <see cref="TOBDStandardSessionNegotiator"/>.</summary>
    function CreateSessionNegotiator: IOBDSessionNegotiator; virtual;
    /// <summary>Override-point: populate <c>Reg</c> with this
    /// OEM's default starter algorithms. Default no-op.</summary>
    /// <param name="Reg">Empty registry to populate.</param>
    procedure SeedDefaultSeedKeyAlgorithms(
      Reg: TOBDSeedKeyRegistry); virtual;
    /// <summary>Override-point: populate <c>Cat</c> with this
    /// OEM's default DTC catalogue. Default no-op.</summary>
    /// <param name="Cat">Empty catalogue to populate.</param>
    procedure SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog); virtual;
    /// <summary>Catalogue filename loaded by
    /// <see cref="SeedDefaultDtcCatalog"/>. Empty = no
    /// overlay.</summary>
    function DtcCatalogFileName: string; virtual;
  public
    /// <summary>Constructs the base extension.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    function ManufacturerKey: string; virtual; abstract;
    function DisplayName: string; virtual; abstract;
    function ApplicableToVIN(const VIN: string): Boolean; virtual; abstract;
    function ApplicableToECUSupplier(
      const SupplierID: string): Boolean; virtual;
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>; virtual;
    function Routines: TArray<TOBDOEMRoutine>; virtual;
    function DecodeDID(const DID: Word;
      const Payload: TBytes): string; virtual;
    function FindDID(const DID: Word;
      out Entry: TOBDOEMDataIdentifier): Boolean; virtual;
    function FindRoutine(const Id: Word;
      out Entry: TOBDOEMRoutine): Boolean; virtual;
    function ECUs: TArray<TOBDOEMECU>; virtual;
    function CatalogForECU(
      const Address: Word): TOBDOEMSubCatalog; virtual;
    function SessionNegotiator: IOBDSessionNegotiator; virtual;
    function SeedKeyRegistry: TOBDSeedKeyRegistry; virtual;
    function DtcCatalog: TOBDDtcCatalog; virtual;
    function DescribeDTC(const Code: string;
      out Entry: TOBDDtcCatalogEntry): Boolean; virtual;
    function CodingBlocks: TArray<TOBDOEMCodingBlock>; virtual;
    function Adaptations: TArray<TOBDOEMAdaptation>; virtual;
    function ActuatorTests: TArray<TOBDOEMActuatorTest>; virtual;
    function LivePIDs: TArray<TOBDOEMLivePID>; virtual;
    function DtcExtendedDataRecords:
      TArray<TOBDDtcExtendedDataRecord>; virtual;
  end;

/// <summary>Builder helper used by JSON catalogue readers and
/// vendor <c>BuildCatalog</c> implementations.</summary>
/// <param name="Address">CAN-ID.</param>
/// <param name="Name">Short snake_case key.</param>
/// <param name="CommonName">Display label.</param>
function MakeOEMECU(const Address: Word;
  const Name, CommonName: string): TOBDOEMECU;

implementation

{ TOBDOEMExtensionRegistry }

class procedure TOBDOEMExtensionRegistry.EnsureInitialized;
begin
  if FLock = nil then
    FLock := TCriticalSection.Create;
  if FExtensions = nil then
    FExtensions := TList<IOBDOEMExtension>.Create;
end;

class procedure TOBDOEMExtensionRegistry.RegisterExtension(
  const Ext: IOBDOEMExtension);
begin
  if not Assigned(Ext) then
    Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    if not FExtensions.Contains(Ext) then
      FExtensions.Add(Ext);
  finally
    FLock.Leave;
  end;
end;

class procedure TOBDOEMExtensionRegistry.UnregisterExtension(
  const Ext: IOBDOEMExtension);
begin
  if not Assigned(Ext) then
    Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    FExtensions.Remove(Ext);
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.FindByVIN(
  const VIN: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try
    Snapshot := FExtensions.ToArray;
  finally
    FLock.Leave;
  end;
  for Ext in Snapshot do
    if Ext.ApplicableToVIN(VIN) then
      Exit(Ext);
end;

class function TOBDOEMExtensionRegistry.FindByKey(
  const ManufacturerKey: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  EnsureInitialized;
  FLock.Enter;
  try
    Snapshot := FExtensions.ToArray;
  finally
    FLock.Leave;
  end;
  for Ext in Snapshot do
    if SameText(Ext.ManufacturerKey, Trim(ManufacturerKey)) then
      Exit(Ext);
end;

class function TOBDOEMExtensionRegistry.FindByECUSupplier(
  const SupplierID: string): IOBDOEMExtension;
var
  Snapshot: TArray<IOBDOEMExtension>;
  Ext: IOBDOEMExtension;
begin
  Result := nil;
  if SupplierID = '' then
    Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    Snapshot := FExtensions.ToArray;
  finally
    FLock.Leave;
  end;
  for Ext in Snapshot do
    if Ext.ApplicableToECUSupplier(SupplierID) then
      Exit(Ext);
end;

class function TOBDOEMExtensionRegistry.All: TArray<IOBDOEMExtension>;
begin
  EnsureInitialized;
  FLock.Enter;
  try
    Result := FExtensions.ToArray;
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.Count: Integer;
begin
  EnsureInitialized;
  FLock.Enter;
  try
    Result := FExtensions.Count;
  finally
    FLock.Leave;
  end;
end;

class procedure TOBDOEMExtensionRegistry.Clear;
begin
  EnsureInitialized;
  FLock.Enter;
  try
    FExtensions.Clear;
  finally
    FLock.Leave;
  end;
end;

class procedure TOBDOEMExtensionRegistry.Shutdown;
begin
  if FExtensions <> nil then
  begin
    FLock.Enter;
    try
      FExtensions.Clear;
    finally
      FLock.Leave;
    end;
    FreeAndNil(FExtensions);
  end;
  FreeAndNil(FLock);
end;

{ TOBDOEMExtensionBase }

constructor TOBDOEMExtensionBase.Create;
begin
  inherited Create;
  FCatalogLock := TCriticalSection.Create;
  FSessionLock := TCriticalSection.Create;
  FSeedKeyLock := TCriticalSection.Create;
  FDtcLock := TCriticalSection.Create;
end;

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

procedure TOBDOEMExtensionBase.EnsureCatalog;
begin
  if FCatalogLoaded then
    Exit;
  FCatalogLock.Enter;
  try
    if FCatalogLoaded then
      Exit;
    BuildCatalog(FDIDs, FRoutines, FECUs);
    BuildExtendedCatalog(FCodingBlocks, FAdaptations, FActuatorTests,
      FLivePIDs, FDtcExtended);
    FCatalogLoaded := True;
  finally
    FCatalogLock.Leave;
  end;
end;

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

procedure TOBDOEMExtensionBase.BuildExtendedCatalog(
  var CodingBlocks: TArray<TOBDOEMCodingBlock>;
  var Adaptations: TArray<TOBDOEMAdaptation>;
  var ActuatorTests: TArray<TOBDOEMActuatorTest>;
  var LivePIDs: TArray<TOBDOEMLivePID>;
  var DtcExtended: TArray<TOBDDtcExtendedDataRecord>);
begin
  // Default: no extended catalogue. Subclasses populate from JSON
  // via OBD.OEM.Catalog.Loader.MergeExtendedCatalogJSON.
end;

function TOBDOEMExtensionBase.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDStandardSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionBase.SeedDefaultSeedKeyAlgorithms(
  Reg: TOBDSeedKeyRegistry);
begin
  // Default: empty. Subclasses register their starter algorithms;
  // production users replace them with their NDA-protected real
  // ones via Reg.RegisterAlgorithm.
end;

procedure TOBDOEMExtensionBase.SeedDefaultDtcCatalog(Cat: TOBDDtcCatalog);
begin
  // Default: empty. Vendor units chain to inherited and append
  // their P1xxx / B / C / U entries. The actual file loading lives
  // in OBD.OEM.DTC.Loader so this unit stays dependency-free.
end;

function TOBDOEMExtensionBase.DtcCatalogFileName: string;
begin
  Result := '';
end;

function TOBDOEMExtensionBase.ApplicableToECUSupplier(
  const SupplierID: string): Boolean;
begin
  Result := False;
end;

function TOBDOEMExtensionBase.DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
begin
  EnsureCatalog;
  Result := FDIDs;
end;

function TOBDOEMExtensionBase.Routines: TArray<TOBDOEMRoutine>;
begin
  EnsureCatalog;
  Result := FRoutines;
end;

function TOBDOEMExtensionBase.ECUs: TArray<TOBDOEMECU>;
begin
  EnsureCatalog;
  Result := FECUs;
end;

function TOBDOEMExtensionBase.DecodeDID(const DID: Word;
  const Payload: TBytes): string;
var
  I: Integer;
  Hex: TStringBuilder;
  Entry: TOBDOEMDataIdentifier;
begin
  Hex := TStringBuilder.Create;
  try
    for I := 0 to High(Payload) do
    begin
      if I > 0 then
        Hex.Append(' ');
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

function TOBDOEMExtensionBase.CatalogForECU(
  const Address: Word): TOBDOEMSubCatalog;
var
  D: TOBDOEMDataIdentifier;
  R: TOBDOEMRoutine;
begin
  EnsureCatalog;
  Result := Default(TOBDOEMSubCatalog);
  Result.EcuAddress := Address;
  for D in FDIDs do
    if (D.EcuAddress = 0) or (Address = 0) or
       (D.EcuAddress = Address) then
      Result.DIDs := Result.DIDs + [D];
  for R in FRoutines do
    if (R.EcuAddress = 0) or (Address = 0) or
       (R.EcuAddress = Address) then
      Result.Routines := Result.Routines + [R];
end;

function TOBDOEMExtensionBase.SessionNegotiator: IOBDSessionNegotiator;
begin
  FSessionLock.Enter;
  try
    if FSessionNegotiator = nil then
      FSessionNegotiator := CreateSessionNegotiator;
    Result := FSessionNegotiator;
  finally
    FSessionLock.Leave;
  end;
end;

function TOBDOEMExtensionBase.SeedKeyRegistry: TOBDSeedKeyRegistry;
begin
  EnsureSeedKeyRegistry;
  Result := FSeedKeyRegistry;
end;

function TOBDOEMExtensionBase.DtcCatalog: TOBDDtcCatalog;
begin
  EnsureDtcCatalog;
  Result := FDtcCatalog;
end;

function TOBDOEMExtensionBase.DescribeDTC(const Code: string;
  out Entry: TOBDDtcCatalogEntry): Boolean;
begin
  Result := DtcCatalog.FindByCode(Code, Entry);
end;

function TOBDOEMExtensionBase.CodingBlocks: TArray<TOBDOEMCodingBlock>;
begin
  EnsureCatalog;
  Result := FCodingBlocks;
end;

function TOBDOEMExtensionBase.Adaptations: TArray<TOBDOEMAdaptation>;
begin
  EnsureCatalog;
  Result := FAdaptations;
end;

function TOBDOEMExtensionBase.ActuatorTests: TArray<TOBDOEMActuatorTest>;
begin
  EnsureCatalog;
  Result := FActuatorTests;
end;

function TOBDOEMExtensionBase.LivePIDs: TArray<TOBDOEMLivePID>;
begin
  EnsureCatalog;
  Result := FLivePIDs;
end;

function TOBDOEMExtensionBase.DtcExtendedDataRecords:
  TArray<TOBDDtcExtendedDataRecord>;
begin
  EnsureCatalog;
  Result := FDtcExtended;
end;

function MakeOEMECU(const Address: Word;
  const Name, CommonName: string): TOBDOEMECU;
begin
  Result.Address := Address;
  Result.Name := Name;
  Result.CommonName := CommonName;
end;

initialization

finalization
  TOBDOEMExtensionRegistry.Shutdown;

end.
