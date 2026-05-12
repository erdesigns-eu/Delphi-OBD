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
//    2026-05-11  ERD  Initial port from v1 OBD.OEM.
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
  ///   coding blocks, adaptations, actuator tests, live PIDs,
  ///   DTC extended-data records), the session negotiator, the
  ///   seed-key registry and the DTC catalogue.
  /// </summary>
  IOBDOEMExtension = interface
    ['{A2C5F4C6-4D71-4E8F-9C5B-3E4A8B1D6C2F}']
    /// <summary>Short ASCII manufacturer tag (e.g. <c>'VAG'</c>,
    /// <c>'BMW'</c>, <c>'FORD'</c>).</summary>
    function ManufacturerKey: string;
    /// <summary>Display name.</summary>
    function DisplayName: string;
    /// <summary>Returns <c>True</c> when this extension claims
    /// <c>AVIN</c> via WMI / VDS pattern matching.</summary>
    /// <param name="AVIN">17-character ISO 3779 VIN.</param>
    function ApplicableToVIN(const AVIN: string): Boolean;
    /// <summary>Returns <c>True</c> when this extension claims an
    /// ECU based on its <c>SupplierID</c> (J1939 PGN 65259 'Make'
    /// or UDS DID 0xF18A).</summary>
    /// <param name="ASupplierID">Supplier-identifier string.</param>
    function ApplicableToECUSupplier(const ASupplierID: string): Boolean;
    /// <summary>The full DID catalogue.</summary>
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
    /// <summary>The full Routine catalogue.</summary>
    function Routines: TArray<TOBDOEMRoutine>;
    /// <summary>The full ECU catalogue.</summary>
    function ECUs: TArray<TOBDOEMECU>;
    /// <summary>Decodes a raw DID payload to a human-readable
    /// string.</summary>
    /// <param name="ADID">DID being decoded.</param>
    /// <param name="APayload">Raw response bytes.</param>
    /// <returns>Decoded label.</returns>
    function DecodeDID(ADID: Word; const APayload: TBytes): string;
    /// <summary>Looks up a DID.</summary>
    /// <param name="ADID">DID value.</param>
    /// <param name="AEntry">Out: matching entry on success.</param>
    /// <returns><c>True</c> when found.</returns>
    function FindDID(ADID: Word;
      out AEntry: TOBDOEMDataIdentifier): Boolean;
    /// <summary>Looks up a routine.</summary>
    /// <param name="AId">Routine ID.</param>
    /// <param name="AEntry">Out: matching entry on success.</param>
    /// <returns><c>True</c> when found.</returns>
    function FindRoutine(AId: Word;
      out AEntry: TOBDOEMRoutine): Boolean;
    /// <summary>Catalogue filtered to one ECU address.</summary>
    /// <param name="AAddress">ECU CAN-ID.</param>
    /// <returns>Filtered slice.</returns>
    function CatalogForECU(AAddress: Word): TOBDOEMSubCatalog;
    /// <summary>This OEM's session negotiator.</summary>
    function SessionNegotiator: IOBDSessionNegotiator;
    /// <summary>This OEM's seed-key registry.</summary>
    function SeedKeyRegistry: TOBDSeedKeyRegistry;
    /// <summary>This OEM's DTC catalogue.</summary>
    function DtcCatalog: TOBDDtcCatalog;
    /// <summary>Looks up a DTC code in <see cref="DtcCatalog"/>.</summary>
    /// <param name="ACode">5-character DTC code.</param>
    /// <param name="AEntry">Out: matching entry on success.</param>
    /// <returns><c>True</c> when found.</returns>
    function DescribeDTC(const ACode: string;
      out AEntry: TOBDDtcCatalogEntry): Boolean;
    /// <summary>Writeable coding blocks.</summary>
    function CodingBlocks: TArray<TOBDOEMCodingBlock>;
    /// <summary>Adaptation channels (VAG-style).</summary>
    function Adaptations: TArray<TOBDOEMAdaptation>;
    /// <summary>Forced-output actuator tests.</summary>
    function ActuatorTests: TArray<TOBDOEMActuatorTest>;
    /// <summary>Streamable live PIDs (Mode 01 + Mode 22).</summary>
    function LivePIDs: TArray<TOBDOEMLivePID>;
    /// <summary>DTC extended-data records (UDS 0x19 / 0x06).</summary>
    function DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>;
  end;

  /// <summary>
  ///   Process-wide registry.
  /// </summary>
  /// <remarks>
  ///   Extensions register themselves at unit initialisation so
  ///   that simply adding a vendor's unit to the project's
  ///   <c>uses</c> clause activates it.
  /// </remarks>
  TOBDOEMExtensionRegistry = class
  strict private
    class var FLock: TCriticalSection;
    class var FExtensions: TList<IOBDOEMExtension>;
    class procedure EnsureInitialized; static;
  public
    /// <summary>Registers an extension.</summary>
    /// <param name="AExtension">Extension to add.</param>
    class procedure RegisterExtension(const AExtension: IOBDOEMExtension); static;
    /// <summary>Unregisters an extension.</summary>
    /// <param name="AExtension">Extension to remove.</param>
    class procedure UnregisterExtension(const AExtension: IOBDOEMExtension); static;
    /// <summary>Resolves the first extension that claims
    /// <c>AVIN</c>.</summary>
    /// <param name="AVIN">17-character VIN.</param>
    /// <returns>Matching extension or <c>nil</c>.</returns>
    class function FindByVIN(const AVIN: string): IOBDOEMExtension; static;
    /// <summary>Resolves the first extension whose
    /// <c>ManufacturerKey</c> matches (case-insensitive).</summary>
    /// <param name="AManufacturerKey">Manufacturer key.</param>
    /// <returns>Matching extension or <c>nil</c>.</returns>
    class function FindByKey(const AManufacturerKey: string): IOBDOEMExtension; static;
    /// <summary>Resolves the first extension that claims
    /// <c>ASupplierID</c>.</summary>
    /// <param name="ASupplierID">Supplier identifier.</param>
    /// <returns>Matching extension or <c>nil</c>.</returns>
    class function FindByECUSupplier(const ASupplierID: string): IOBDOEMExtension; static;
    /// <summary>Snapshot of every registered extension.</summary>
    class function All: TArray<IOBDOEMExtension>; static;
    /// <summary>Number of registered extensions.</summary>
    class function Count: Integer; static;
    /// <summary>Drops every registered extension.</summary>
    class procedure Clear; static;
    /// <summary>Releases all registry state. Called from this
    /// unit's <c>finalization</c>; safe to call multiple
    /// times.</summary>
    class procedure Shutdown; static;
  end;

  /// <summary>
  ///   Convenience base class with sensible defaults for the
  ///   lookup, decode and catalogue plumbing. Concrete OEMs
  ///   override <c>ManufacturerKey</c>, <c>DisplayName</c>,
  ///   <c>ApplicableToVIN</c> and the catalogue-loader hooks.
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
    procedure EnsureCatalogLoaded;
  protected
    /// <summary>Subclass hook — load the DID / Routine / ECU
    /// catalogues. Called once on first access.</summary>
    procedure LoadCatalog; virtual;
    /// <summary>Subclass hook — populate
    /// <c>FCodingBlocks</c>.</summary>
    procedure LoadCodingBlocks; virtual;
    /// <summary>Subclass hook — populate
    /// <c>FAdaptations</c>.</summary>
    procedure LoadAdaptations; virtual;
    /// <summary>Subclass hook — populate
    /// <c>FActuatorTests</c>.</summary>
    procedure LoadActuatorTests; virtual;
    /// <summary>Subclass hook — populate <c>FLivePIDs</c>.</summary>
    procedure LoadLivePIDs; virtual;
    /// <summary>Subclass hook — populate
    /// <c>FDtcExtended</c>.</summary>
    procedure LoadDtcExtended; virtual;
    /// <summary>Subclass hook — seed the DTC catalogue (called
    /// once when <see cref="DtcCatalog"/> is first accessed).</summary>
    /// <param name="ACatalog">Empty catalogue to populate.</param>
    procedure SeedDtcCatalog(ACatalog: TOBDDtcCatalog); virtual;
    /// <summary>Subclass hook — supply the session
    /// negotiator. Default returns a fresh
    /// <see cref="TOBDStandardSessionNegotiator"/>.</summary>
    function CreateSessionNegotiator: IOBDSessionNegotiator; virtual;

    /// <summary>Adds one DID row to the in-memory cache.</summary>
    procedure AddDID(const AEntry: TOBDOEMDataIdentifier);
    /// <summary>Adds one routine row to the in-memory cache.</summary>
    procedure AddRoutine(const AEntry: TOBDOEMRoutine);
    /// <summary>Adds one ECU row to the in-memory cache.</summary>
    procedure AddECU(const AEntry: TOBDOEMECU);
    /// <summary>Adds one coding block.</summary>
    procedure AddCodingBlock(const AEntry: TOBDOEMCodingBlock);
    /// <summary>Adds one adaptation channel.</summary>
    procedure AddAdaptation(const AEntry: TOBDOEMAdaptation);
    /// <summary>Adds one actuator test.</summary>
    procedure AddActuatorTest(const AEntry: TOBDOEMActuatorTest);
    /// <summary>Adds one live PID.</summary>
    procedure AddLivePID(const AEntry: TOBDOEMLivePID);
    /// <summary>Adds one DTC extended-data record.</summary>
    procedure AddDtcExtendedRecord(const AEntry: TOBDDtcExtendedDataRecord);
  public
    /// <summary>Constructs the base extension.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    // ---- IOBDOEMExtension ----
    function ManufacturerKey: string; virtual; abstract;
    function DisplayName: string; virtual; abstract;
    function ApplicableToVIN(const AVIN: string): Boolean; virtual;
    function ApplicableToECUSupplier(const ASupplierID: string): Boolean; virtual;
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
    function Routines: TArray<TOBDOEMRoutine>;
    function ECUs: TArray<TOBDOEMECU>;
    function DecodeDID(ADID: Word; const APayload: TBytes): string; virtual;
    function FindDID(ADID: Word;
      out AEntry: TOBDOEMDataIdentifier): Boolean;
    function FindRoutine(AId: Word;
      out AEntry: TOBDOEMRoutine): Boolean;
    function CatalogForECU(AAddress: Word): TOBDOEMSubCatalog;
    function SessionNegotiator: IOBDSessionNegotiator;
    function SeedKeyRegistry: TOBDSeedKeyRegistry;
    function DtcCatalog: TOBDDtcCatalog;
    function DescribeDTC(const ACode: string;
      out AEntry: TOBDDtcCatalogEntry): Boolean;
    function CodingBlocks: TArray<TOBDOEMCodingBlock>;
    function Adaptations: TArray<TOBDOEMAdaptation>;
    function ActuatorTests: TArray<TOBDOEMActuatorTest>;
    function LivePIDs: TArray<TOBDOEMLivePID>;
    function DtcExtendedDataRecords: TArray<TOBDDtcExtendedDataRecord>;
  end;

implementation

uses
  System.StrUtils;

{ TOBDOEMExtensionRegistry }

class procedure TOBDOEMExtensionRegistry.EnsureInitialized;
begin
  if FLock = nil then
    FLock := TCriticalSection.Create;
  if FExtensions = nil then
    FExtensions := TList<IOBDOEMExtension>.Create;
end;

class procedure TOBDOEMExtensionRegistry.RegisterExtension(
  const AExtension: IOBDOEMExtension);
begin
  if AExtension = nil then
    Exit;
  EnsureInitialized;
  FLock.Enter;
  try
    if FExtensions.IndexOf(AExtension) < 0 then
      FExtensions.Add(AExtension);
  finally
    FLock.Leave;
  end;
end;

class procedure TOBDOEMExtensionRegistry.UnregisterExtension(
  const AExtension: IOBDOEMExtension);
begin
  if (AExtension = nil) or (FExtensions = nil) then
    Exit;
  FLock.Enter;
  try
    FExtensions.Remove(AExtension);
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.FindByVIN(
  const AVIN: string): IOBDOEMExtension;
var
  I: Integer;
begin
  Result := nil;
  if FExtensions = nil then
    Exit;
  FLock.Enter;
  try
    for I := 0 to FExtensions.Count - 1 do
      if FExtensions[I].ApplicableToVIN(AVIN) then
        Exit(FExtensions[I]);
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.FindByKey(
  const AManufacturerKey: string): IOBDOEMExtension;
var
  I: Integer;
  Target: string;
begin
  Result := nil;
  if FExtensions = nil then
    Exit;
  Target := UpperCase(Trim(AManufacturerKey));
  FLock.Enter;
  try
    for I := 0 to FExtensions.Count - 1 do
      if UpperCase(FExtensions[I].ManufacturerKey) = Target then
        Exit(FExtensions[I]);
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.FindByECUSupplier(
  const ASupplierID: string): IOBDOEMExtension;
var
  I: Integer;
begin
  Result := nil;
  if FExtensions = nil then
    Exit;
  FLock.Enter;
  try
    for I := 0 to FExtensions.Count - 1 do
      if FExtensions[I].ApplicableToECUSupplier(ASupplierID) then
        Exit(FExtensions[I]);
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.All: TArray<IOBDOEMExtension>;
begin
  if FExtensions = nil then
    Exit(nil);
  FLock.Enter;
  try
    Result := FExtensions.ToArray;
  finally
    FLock.Leave;
  end;
end;

class function TOBDOEMExtensionRegistry.Count: Integer;
begin
  if FExtensions = nil then
    Exit(0);
  FLock.Enter;
  try
    Result := FExtensions.Count;
  finally
    FLock.Leave;
  end;
end;

class procedure TOBDOEMExtensionRegistry.Clear;
begin
  if FExtensions = nil then
    Exit;
  FLock.Enter;
  try
    FExtensions.Clear;
  finally
    FLock.Leave;
  end;
end;

{ TOBDOEMExtensionBase }

constructor TOBDOEMExtensionBase.Create;
begin
  inherited;
  FCatalogLock := TCriticalSection.Create;
  FSessionLock := TCriticalSection.Create;
  FSeedKeyLock := TCriticalSection.Create;
  FDtcLock := TCriticalSection.Create;
end;

destructor TOBDOEMExtensionBase.Destroy;
begin
  FSeedKeyRegistry.Free;
  FDtcCatalog.Free;
  FCatalogLock.Free;
  FSessionLock.Free;
  FSeedKeyLock.Free;
  FDtcLock.Free;
  inherited;
end;

procedure TOBDOEMExtensionBase.EnsureCatalogLoaded;
begin
  if FCatalogLoaded then
    Exit;
  FCatalogLock.Enter;
  try
    if FCatalogLoaded then
      Exit;
    LoadCatalog;
    LoadCodingBlocks;
    LoadAdaptations;
    LoadActuatorTests;
    LoadLivePIDs;
    LoadDtcExtended;
    FCatalogLoaded := True;
  finally
    FCatalogLock.Leave;
  end;
end;

procedure TOBDOEMExtensionBase.LoadCatalog;
begin
  // Subclass hook; the base implementation publishes an empty
  // catalogue.
end;

procedure TOBDOEMExtensionBase.LoadCodingBlocks;
begin
end;

procedure TOBDOEMExtensionBase.LoadAdaptations;
begin
end;

procedure TOBDOEMExtensionBase.LoadActuatorTests;
begin
end;

procedure TOBDOEMExtensionBase.LoadLivePIDs;
begin
end;

procedure TOBDOEMExtensionBase.LoadDtcExtended;
begin
end;

procedure TOBDOEMExtensionBase.SeedDtcCatalog(ACatalog: TOBDDtcCatalog);
begin
  // Subclass hook; default leaves the catalogue empty so the
  // host can populate it from JSON / database / etc.
end;

function TOBDOEMExtensionBase.CreateSessionNegotiator: IOBDSessionNegotiator;
begin
  Result := TOBDStandardSessionNegotiator.Create;
end;

procedure TOBDOEMExtensionBase.AddDID(const AEntry: TOBDOEMDataIdentifier);
var
  N: Integer;
begin
  N := Length(FDIDs);
  SetLength(FDIDs, N + 1);
  FDIDs[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddRoutine(const AEntry: TOBDOEMRoutine);
var
  N: Integer;
begin
  N := Length(FRoutines);
  SetLength(FRoutines, N + 1);
  FRoutines[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddECU(const AEntry: TOBDOEMECU);
var
  N: Integer;
begin
  N := Length(FECUs);
  SetLength(FECUs, N + 1);
  FECUs[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddCodingBlock(
  const AEntry: TOBDOEMCodingBlock);
var
  N: Integer;
begin
  N := Length(FCodingBlocks);
  SetLength(FCodingBlocks, N + 1);
  FCodingBlocks[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddAdaptation(
  const AEntry: TOBDOEMAdaptation);
var
  N: Integer;
begin
  N := Length(FAdaptations);
  SetLength(FAdaptations, N + 1);
  FAdaptations[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddActuatorTest(
  const AEntry: TOBDOEMActuatorTest);
var
  N: Integer;
begin
  N := Length(FActuatorTests);
  SetLength(FActuatorTests, N + 1);
  FActuatorTests[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddLivePID(const AEntry: TOBDOEMLivePID);
var
  N: Integer;
begin
  N := Length(FLivePIDs);
  SetLength(FLivePIDs, N + 1);
  FLivePIDs[N] := AEntry;
end;

procedure TOBDOEMExtensionBase.AddDtcExtendedRecord(
  const AEntry: TOBDDtcExtendedDataRecord);
var
  N: Integer;
begin
  N := Length(FDtcExtended);
  SetLength(FDtcExtended, N + 1);
  FDtcExtended[N] := AEntry;
end;

function TOBDOEMExtensionBase.ApplicableToVIN(const AVIN: string): Boolean;
begin
  Result := False;
end;

function TOBDOEMExtensionBase.ApplicableToECUSupplier(
  const ASupplierID: string): Boolean;
begin
  Result := False;
end;

function TOBDOEMExtensionBase.DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
begin
  EnsureCatalogLoaded;
  Result := FDIDs;
end;

function TOBDOEMExtensionBase.Routines: TArray<TOBDOEMRoutine>;
begin
  EnsureCatalogLoaded;
  Result := FRoutines;
end;

function TOBDOEMExtensionBase.ECUs: TArray<TOBDOEMECU>;
begin
  EnsureCatalogLoaded;
  Result := FECUs;
end;

function TOBDOEMExtensionBase.DecodeDID(ADID: Word;
  const APayload: TBytes): string;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  I: Integer;
  Acc: string;
begin
  // Base implementation: hex-dump the payload. Subclasses can
  // override with structured decoding.
  SetLength(Acc, Length(APayload) * 3);
  for I := 0 to High(APayload) do
  begin
    Acc[1 + 3 * I]     := HexDigits[(APayload[I] shr 4) and $0F];
    Acc[1 + 3 * I + 1] := HexDigits[APayload[I] and $0F];
    Acc[1 + 3 * I + 2] := ' ';
  end;
  Result := TrimRight(Acc);
end;

function TOBDOEMExtensionBase.FindDID(ADID: Word;
  out AEntry: TOBDOEMDataIdentifier): Boolean;
var
  I: Integer;
begin
  EnsureCatalogLoaded;
  for I := 0 to High(FDIDs) do
    if FDIDs[I].DID = ADID then
    begin
      AEntry := FDIDs[I];
      Exit(True);
    end;
  Result := False;
end;

function TOBDOEMExtensionBase.FindRoutine(AId: Word;
  out AEntry: TOBDOEMRoutine): Boolean;
var
  I: Integer;
begin
  EnsureCatalogLoaded;
  for I := 0 to High(FRoutines) do
    if FRoutines[I].Identifier = AId then
    begin
      AEntry := FRoutines[I];
      Exit(True);
    end;
  Result := False;
end;

function TOBDOEMExtensionBase.CatalogForECU(
  AAddress: Word): TOBDOEMSubCatalog;
var
  I: Integer;
  DIDList: TList<TOBDOEMDataIdentifier>;
  RoutineList: TList<TOBDOEMRoutine>;
begin
  EnsureCatalogLoaded;
  Result := Default(TOBDOEMSubCatalog);
  Result.EcuAddress := AAddress;
  DIDList := TList<TOBDOEMDataIdentifier>.Create;
  RoutineList := TList<TOBDOEMRoutine>.Create;
  try
    for I := 0 to High(FDIDs) do
      if (FDIDs[I].EcuAddress = 0) or
         (FDIDs[I].EcuAddress = AAddress) then
        DIDList.Add(FDIDs[I]);
    for I := 0 to High(FRoutines) do
      if (FRoutines[I].EcuAddress = 0) or
         (FRoutines[I].EcuAddress = AAddress) then
        RoutineList.Add(FRoutines[I]);
    Result.DIDs := DIDList.ToArray;
    Result.Routines := RoutineList.ToArray;
  finally
    DIDList.Free;
    RoutineList.Free;
  end;
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
  FSeedKeyLock.Enter;
  try
    if FSeedKeyRegistry = nil then
      FSeedKeyRegistry := TOBDSeedKeyRegistry.Create;
    Result := FSeedKeyRegistry;
  finally
    FSeedKeyLock.Leave;
  end;
end;

function TOBDOEMExtensionBase.DtcCatalog: TOBDDtcCatalog;
begin
  FDtcLock.Enter;
  try
    if FDtcCatalog = nil then
    begin
      FDtcCatalog := TOBDDtcCatalog.Create;
      SeedDtcCatalog(FDtcCatalog);
    end;
    Result := FDtcCatalog;
  finally
    FDtcLock.Leave;
  end;
end;

function TOBDOEMExtensionBase.DescribeDTC(const ACode: string;
  out AEntry: TOBDDtcCatalogEntry): Boolean;
begin
  Result := DtcCatalog.TryFind(ACode, AEntry);
end;

function TOBDOEMExtensionBase.CodingBlocks: TArray<TOBDOEMCodingBlock>;
begin
  EnsureCatalogLoaded;
  Result := FCodingBlocks;
end;

function TOBDOEMExtensionBase.Adaptations: TArray<TOBDOEMAdaptation>;
begin
  EnsureCatalogLoaded;
  Result := FAdaptations;
end;

function TOBDOEMExtensionBase.ActuatorTests: TArray<TOBDOEMActuatorTest>;
begin
  EnsureCatalogLoaded;
  Result := FActuatorTests;
end;

function TOBDOEMExtensionBase.LivePIDs: TArray<TOBDOEMLivePID>;
begin
  EnsureCatalogLoaded;
  Result := FLivePIDs;
end;

function TOBDOEMExtensionBase.DtcExtendedDataRecords:
  TArray<TOBDDtcExtendedDataRecord>;
begin
  EnsureCatalogLoaded;
  Result := FDtcExtended;
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

initialization

finalization
  TOBDOEMExtensionRegistry.Shutdown;

end.
