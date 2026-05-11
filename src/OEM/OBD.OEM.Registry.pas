//------------------------------------------------------------------------------
//  OBD.OEM.Registry
//
//  TOBDOEMRegistry — process-wide singleton that resolves DID,
//  PID, DTC and SPN identifiers to human-readable names against
//  any number of OEM overlays. Generic Phase 5 / Phase 6
//  components (TOBDDataIdentifierIO, TOBDDTCs, TOBDUDSReadDID,
//  TOBDJ1939DM, …) consult the registry whenever they have raw
//  numeric input and want a friendly label.
//
//  Overlays are layered: a registered overlay either contains a
//  name for a given identifier or it doesn't. The registry walks
//  overlays in registration order until one resolves the lookup,
//  returning a sensible "<kind> 0xNNNN" fallback when nothing
//  matches. <c>TOBDOEMCatalog</c> (sibling unit) reads JSON
//  files into <c>TOBDOEMOverlay</c> records and pushes them into
//  the registry.
//
//  The registry is intentionally narrow — it solves "byte 0xF190
//  → 'Vehicle Identification Number'", not full property
//  decoding. Components that decode bytes into typed values
//  (scaling, units, ranges) still own that logic.
//
//  Threading
//
//    The registry is reentrant via an internal critical section.
//    Overlay registration and lookup are safe from any thread.
//    Components that walk the registry on a worker should treat
//    returned strings as immutable snapshots.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Registry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

type
  /// <summary>
  ///   Identifier kind. Determines which overlay table the
  ///   registry searches.
  /// </summary>
  TOBDOEMIdentifierKind = (
    /// <summary>UDS / KWP Data Identifier (16-bit).</summary>
    ikDID,
    /// <summary>OBD-II / Mode 06 PID (8-bit) under a mode.</summary>
    ikPID,
    /// <summary>Diagnostic Trouble Code (5-char J2012 string).</summary>
    ikDTC,
    /// <summary>J1939 Suspect Parameter Number (19-bit).</summary>
    ikSPN,
    /// <summary>J1939 Failure Mode Identifier (5-bit).</summary>
    ikFMI,
    /// <summary>J1939 Parameter Group Number (18-bit).</summary>
    ikPGN
  );

  /// <summary>
  ///   One overlay record. Owned by the caller (typically a
  ///   <c>TOBDOEMCatalog</c>) — the registry holds a reference
  ///   only and never frees it.
  /// </summary>
  TOBDOEMOverlay = class
  strict private
    FOEM: string;
    FDIDs: TDictionary<Word, string>;
    FDTCs: TDictionary<string, string>;
    FSPNs: TDictionary<Cardinal, string>;
    FFMIs: TDictionary<Byte, string>;
    FPGNs: TDictionary<Cardinal, string>;
    FPIDs: TDictionary<Word, string>;
  public
    /// <summary>Constructs an empty overlay attributed to
    /// <c>AOEM</c>.</summary>
    /// <param name="AOEM">OEM identifier (e.g. <c>'vag'</c>,
    /// <c>'bmw'</c>).</param>
    constructor Create(const AOEM: string);
    /// <summary>Frees every internal map.</summary>
    destructor Destroy; override;

    /// <summary>Adds (or replaces) a DID → name entry.</summary>
    /// <param name="ADID">16-bit DID.</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddDID(ADID: Word; const AName: string);
    /// <summary>Adds (or replaces) a DTC code → name entry.</summary>
    /// <param name="ACode">5-character J2012 DTC code (e.g.
    /// <c>'P0420'</c>).</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddDTC(const ACode: string; const AName: string);
    /// <summary>Adds (or replaces) an SPN → name entry.</summary>
    /// <param name="ASPN">19-bit SPN.</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddSPN(ASPN: Cardinal; const AName: string);
    /// <summary>Adds (or replaces) an FMI → name entry.</summary>
    /// <param name="AFMI">5-bit FMI.</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddFMI(AFMI: Byte; const AName: string);
    /// <summary>Adds (or replaces) a PGN → name entry.</summary>
    /// <param name="APGN">18-bit PGN.</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddPGN(APGN: Cardinal; const AName: string);
    /// <summary>
    ///   Adds (or replaces) a Mode-PID composite entry. The high
    ///   byte of the key is the mode, the low byte is the PID
    ///   (so Mode 01 PID 0x0C → 0x010C).
    /// </summary>
    /// <param name="AMode">OBD service / mode byte.</param>
    /// <param name="APID">PID byte.</param>
    /// <param name="AName">Human-readable name.</param>
    procedure AddPID(AMode: Byte; APID: Byte; const AName: string);

    /// <summary>Reads the OEM tag.</summary>
    function OEM: string;

    /// <summary>
    ///   Looks up an identifier; returns <c>True</c> when found.
    /// </summary>
    /// <param name="AKind">Identifier kind.</param>
    /// <param name="AKey">Identifier key as a <c>Cardinal</c>
    /// (DTC codes go through <see cref="LookupDTC"/>).</param>
    /// <param name="AName">Out: the resolved name, untouched
    /// when not found.</param>
    /// <returns><c>True</c> if found.</returns>
    function Lookup(AKind: TOBDOEMIdentifierKind; AKey: Cardinal;
      out AName: string): Boolean;

    /// <summary>String-keyed DTC lookup.</summary>
    /// <param name="ACode">5-character DTC code.</param>
    /// <param name="AName">Out: resolved name.</param>
    /// <returns><c>True</c> if found.</returns>
    function LookupDTC(const ACode: string;
      out AName: string): Boolean;
  end;

  /// <summary>
  ///   Process-wide OEM registry singleton.
  /// </summary>
  TOBDOEMRegistry = class
  strict private
    class var FInstance: TOBDOEMRegistry;
  strict private
    FLock: TCriticalSection;
    FOverlays: TList<TOBDOEMOverlay>;
  public
    /// <summary>Constructs the registry. Use
    /// <see cref="Instance"/> instead of constructing
    /// directly.</summary>
    constructor Create;
    /// <summary>Frees the registry. Overlays themselves are NOT
    /// freed — ownership stays with the registrant.</summary>
    destructor Destroy; override;

    /// <summary>Singleton accessor.</summary>
    /// <returns>The shared registry instance.</returns>
    class function Instance: TOBDOEMRegistry; static;

    /// <summary>Releases the singleton (test hook).</summary>
    class procedure ReleaseInstance; static;

    /// <summary>
    ///   Registers an overlay. The registry keeps a reference
    ///   only; the caller frees the overlay when finished.
    /// </summary>
    /// <param name="AOverlay">Overlay to add.</param>
    procedure Register(AOverlay: TOBDOEMOverlay);
    /// <summary>Removes an overlay reference.</summary>
    /// <param name="AOverlay">Overlay to remove.</param>
    procedure Unregister(AOverlay: TOBDOEMOverlay);

    /// <summary>
    ///   Resolves a numeric identifier by walking the registered
    ///   overlays in registration order.
    /// </summary>
    /// <param name="AKind">Identifier kind.</param>
    /// <param name="AKey">Identifier key (mode-PID composites
    /// use <c>(mode shl 8) or PID</c>).</param>
    /// <returns>Resolved name, or a generic
    /// <c>'&lt;kind&gt; 0xNNNN'</c> fallback when nothing matches.</returns>
    function Resolve(AKind: TOBDOEMIdentifierKind;
      AKey: Cardinal): string;
    /// <summary>String-keyed DTC resolve.</summary>
    /// <param name="ACode">5-character DTC code.</param>
    /// <returns>Resolved name or the code itself when no
    /// overlay matches.</returns>
    function ResolveDTC(const ACode: string): string;

    /// <summary>Count of registered overlays.</summary>
    function OverlayCount: Integer;
  end;

implementation

{ TOBDOEMOverlay }

constructor TOBDOEMOverlay.Create(const AOEM: string);
begin
  inherited Create;
  FOEM := AOEM;
  FDIDs := TDictionary<Word, string>.Create;
  FDTCs := TDictionary<string, string>.Create;
  FSPNs := TDictionary<Cardinal, string>.Create;
  FFMIs := TDictionary<Byte, string>.Create;
  FPGNs := TDictionary<Cardinal, string>.Create;
  FPIDs := TDictionary<Word, string>.Create;
end;

destructor TOBDOEMOverlay.Destroy;
begin
  FDIDs.Free;
  FDTCs.Free;
  FSPNs.Free;
  FFMIs.Free;
  FPGNs.Free;
  FPIDs.Free;
  inherited;
end;

procedure TOBDOEMOverlay.AddDID(ADID: Word; const AName: string);
begin
  FDIDs.AddOrSetValue(ADID, AName);
end;

procedure TOBDOEMOverlay.AddDTC(const ACode: string; const AName: string);
begin
  FDTCs.AddOrSetValue(UpperCase(ACode), AName);
end;

procedure TOBDOEMOverlay.AddSPN(ASPN: Cardinal; const AName: string);
begin
  FSPNs.AddOrSetValue(ASPN, AName);
end;

procedure TOBDOEMOverlay.AddFMI(AFMI: Byte; const AName: string);
begin
  FFMIs.AddOrSetValue(AFMI, AName);
end;

procedure TOBDOEMOverlay.AddPGN(APGN: Cardinal; const AName: string);
begin
  FPGNs.AddOrSetValue(APGN, AName);
end;

procedure TOBDOEMOverlay.AddPID(AMode: Byte; APID: Byte;
  const AName: string);
begin
  FPIDs.AddOrSetValue((Word(AMode) shl 8) or APID, AName);
end;

function TOBDOEMOverlay.OEM: string;
begin
  Result := FOEM;
end;

function TOBDOEMOverlay.Lookup(AKind: TOBDOEMIdentifierKind;
  AKey: Cardinal; out AName: string): Boolean;
begin
  case AKind of
    ikDID:
      Result := FDIDs.TryGetValue(Word(AKey and $FFFF), AName);
    ikSPN:
      Result := FSPNs.TryGetValue(AKey, AName);
    ikFMI:
      Result := FFMIs.TryGetValue(Byte(AKey and $FF), AName);
    ikPGN:
      Result := FPGNs.TryGetValue(AKey, AName);
    ikPID:
      Result := FPIDs.TryGetValue(Word(AKey and $FFFF), AName);
  else
    Result := False;
  end;
end;

function TOBDOEMOverlay.LookupDTC(const ACode: string;
  out AName: string): Boolean;
begin
  Result := FDTCs.TryGetValue(UpperCase(ACode), AName);
end;

{ TOBDOEMRegistry }

constructor TOBDOEMRegistry.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FOverlays := TList<TOBDOEMOverlay>.Create;
end;

destructor TOBDOEMRegistry.Destroy;
begin
  FOverlays.Free;
  FLock.Free;
  inherited;
end;

class function TOBDOEMRegistry.Instance: TOBDOEMRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDOEMRegistry.Create;
  Result := FInstance;
end;

class procedure TOBDOEMRegistry.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDOEMRegistry.Register(AOverlay: TOBDOEMOverlay);
begin
  if AOverlay = nil then
    Exit;
  FLock.Enter;
  try
    if FOverlays.IndexOf(AOverlay) < 0 then
      FOverlays.Add(AOverlay);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDOEMRegistry.Unregister(AOverlay: TOBDOEMOverlay);
begin
  if AOverlay = nil then
    Exit;
  FLock.Enter;
  try
    FOverlays.Remove(AOverlay);
  finally
    FLock.Leave;
  end;
end;

function TOBDOEMRegistry.Resolve(AKind: TOBDOEMIdentifierKind;
  AKey: Cardinal): string;
var
  I: Integer;
  Found: Boolean;
  Name: string;
begin
  Found := False;
  FLock.Enter;
  try
    for I := 0 to FOverlays.Count - 1 do
      if FOverlays[I].Lookup(AKind, AKey, Name) then
      begin
        Found := True;
        Break;
      end;
  finally
    FLock.Leave;
  end;
  if Found then
    Result := Name
  else
  begin
    case AKind of
      ikDID: Result := Format('DID 0x%.4x', [AKey and $FFFF]);
      ikSPN: Result := Format('SPN %u', [AKey]);
      ikFMI: Result := Format('FMI %u', [AKey and $FF]);
      ikPGN: Result := Format('PGN 0x%.4x', [AKey]);
      ikPID: Result := Format('Mode 0x%.2x PID 0x%.2x',
                              [(AKey shr 8) and $FF, AKey and $FF]);
    else
      Result := Format('Identifier 0x%x', [AKey]);
    end;
  end;
end;

function TOBDOEMRegistry.ResolveDTC(const ACode: string): string;
var
  I: Integer;
  Found: Boolean;
  Name: string;
begin
  Found := False;
  FLock.Enter;
  try
    for I := 0 to FOverlays.Count - 1 do
      if FOverlays[I].LookupDTC(ACode, Name) then
      begin
        Found := True;
        Break;
      end;
  finally
    FLock.Leave;
  end;
  if Found then
    Result := Name
  else
    Result := ACode;
end;

function TOBDOEMRegistry.OverlayCount: Integer;
begin
  FLock.Enter;
  try
    Result := FOverlays.Count;
  finally
    FLock.Leave;
  end;
end;

initialization

finalization
  TOBDOEMRegistry.ReleaseInstance;

end.
