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
  System.Generics.Collections, System.Generics.Defaults;

type
  /// <summary>One entry in an OEM's Data Identifier (DID) catalog.</summary>
  TOBDOEMDataIdentifier = record
    DID: Word;
    Name: string;        // human-readable ("battery_voltage")
    Description: string; // longer prose
  end;

  /// <summary>One entry in an OEM's RoutineControl (SID $31) catalog.</summary>
  TOBDOEMRoutine = record
    Identifier: Word;
    Name: string;
    Description: string;
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
    /// <summary>Manufacturer key — short ASCII tag, e.g. "VAG", "BMW".</summary>
    function ManufacturerKey: string;
    /// <summary>Display-friendly name, e.g. "Volkswagen Audi Group".</summary>
    function DisplayName: string;

    /// <summary>
    ///   True if this extension claims a VIN. The framework probes
    ///   every registered extension; the first that returns True wins.
    /// </summary>
    function ApplicableToVIN(const VIN: string): Boolean;

    /// <summary>Catalog of DIDs the extension knows how to interpret.</summary>
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>;
    /// <summary>Catalog of RoutineControl identifiers.</summary>
    function Routines: TArray<TOBDOEMRoutine>;

    /// <summary>
    ///   Decode a raw DID payload to a human-readable string. Default
    ///   implementations hex-dump unknown DIDs; specialised ones return
    ///   structured data (e.g. battery voltage in volts).
    /// </summary>
    function DecodeDID(const DID: Word; const Payload: TBytes): string;

    /// <summary>Lookup helpers — return False if the entry isn't catalogued.</summary>
    function FindDID(const DID: Word; out Entry: TOBDOEMDataIdentifier): Boolean;
    function FindRoutine(const Id: Word; out Entry: TOBDOEMRoutine): Boolean;
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
  TOBDOEMExtensionBase = class(TInterfacedObject, IOBDOEMExtension)
  strict private
    FDIDs: TArray<TOBDOEMDataIdentifier>;
    FRoutines: TArray<TOBDOEMRoutine>;
    FCatalogLoaded: Boolean;
    FCatalogLock: TCriticalSection;
    procedure EnsureCatalog;
  protected
    /// <summary>
    ///   Subclasses populate <c>DIDs</c> and <c>Routines</c> the first
    ///   time the catalog is read. Lazy so unit-init isn't slowed by
    ///   building catalogs that may never be queried.
    /// </summary>
    procedure BuildCatalog(var DIDs: TArray<TOBDOEMDataIdentifier>;
      var Routines: TArray<TOBDOEMRoutine>); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function ManufacturerKey: string; virtual; abstract;
    function DisplayName: string; virtual; abstract;
    function ApplicableToVIN(const VIN: string): Boolean; virtual; abstract;
    function DataIdentifiers: TArray<TOBDOEMDataIdentifier>; virtual;
    function Routines: TArray<TOBDOEMRoutine>; virtual;
    function DecodeDID(const DID: Word; const Payload: TBytes): string; virtual;
    function FindDID(const DID: Word; out Entry: TOBDOEMDataIdentifier): Boolean; virtual;
    function FindRoutine(const Id: Word; out Entry: TOBDOEMRoutine): Boolean; virtual;
  end;

implementation

//==============================================================================
// TOBDOEMRegistry
//==============================================================================
class procedure TOBDOEMRegistry.EnsureInitialized;
begin
  if FLock = nil then
    FLock := TCriticalSection.Create;
  if FExtensions = nil then
    FExtensions := TList<IOBDOEMExtension>.Create;
end;

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

class procedure TOBDOEMRegistry.UnregisterExtension(const Ext: IOBDOEMExtension);
begin
  if not Assigned(Ext) then Exit;
  EnsureInitialized;
  FLock.Enter;
  try FExtensions.Remove(Ext); finally FLock.Leave; end;
end;

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

class function TOBDOEMRegistry.All: TArray<IOBDOEMExtension>;
begin
  EnsureInitialized;
  FLock.Enter;
  try Result := FExtensions.ToArray; finally FLock.Leave; end;
end;

class function TOBDOEMRegistry.Count: Integer;
begin
  EnsureInitialized;
  FLock.Enter;
  try Result := FExtensions.Count; finally FLock.Leave; end;
end;

class procedure TOBDOEMRegistry.Clear;
begin
  EnsureInitialized;
  FLock.Enter;
  try FExtensions.Clear; finally FLock.Leave; end;
end;

//==============================================================================
// TOBDOEMExtensionBase
//==============================================================================
constructor TOBDOEMExtensionBase.Create;
begin
  inherited Create;
  FCatalogLock := TCriticalSection.Create;
end;

destructor TOBDOEMExtensionBase.Destroy;
begin
  FCatalogLock.Free;
  inherited;
end;

procedure TOBDOEMExtensionBase.EnsureCatalog;
begin
  FCatalogLock.Enter;
  try
    if FCatalogLoaded then Exit;
    BuildCatalog(FDIDs, FRoutines);
    FCatalogLoaded := True;
  finally
    FCatalogLock.Leave;
  end;
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
