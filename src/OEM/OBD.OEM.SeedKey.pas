//------------------------------------------------------------------------------
//  OBD.OEM.SeedKey
//
//  Seed-key algorithm registry for OEM SecurityAccess (UDS
//  SID 0x27) flows. Every OEM extension owns a
//  <see cref="TOBDSeedKeyRegistry"/> mapping access-level bytes
//  (the LL in <c>27 LL</c>) to host-supplied algorithm callbacks.
//
//  No production seed-key algorithms ship in the public library —
//  those are NDA-bound per OEM. Hosts register their real
//  implementations at startup via <c>RegisterAlgorithm</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.OEM.SeedKey.
//------------------------------------------------------------------------------

unit OBD.OEM.SeedKey;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

type
  /// <summary>Raised on registry errors.</summary>
  EOBDSeedKey = class(Exception);

  /// <summary>
  ///   Seed-to-key function signature. Host-supplied algorithms
  ///   take a seed (1..N bytes) and return the expected key.
  /// </summary>
  TOBDSeedKeyAlgorithm = reference to function(
    const ASeed: TBytes): TBytes;

  /// <summary>
  ///   Per-OEM seed-key algorithm registry keyed by access level.
  /// </summary>
  /// <remarks>
  ///   Each level (typically 0x01 = programming, 0x03 = extended,
  ///   0x05 = developer / OEM-specific) carries one algorithm. The
  ///   default registry is empty; hosts register their real
  ///   algorithms via <see cref="RegisterAlgorithm"/>.
  /// </remarks>
  TOBDSeedKeyRegistry = class
  strict private
    FLock: TCriticalSection;
    FAlgorithms: TDictionary<Byte, TOBDSeedKeyAlgorithm>;
  public
    /// <summary>Constructs an empty registry.</summary>
    constructor Create;
    /// <summary>Frees state.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Registers (or replaces) an algorithm for an access
    ///   level.
    /// </summary>
    /// <param name="ALevel">Access-level byte (LL in
    /// <c>27 LL</c>).</param>
    /// <param name="AAlgorithm">Seed-to-key function.</param>
    procedure RegisterAlgorithm(ALevel: Byte;
      const AAlgorithm: TOBDSeedKeyAlgorithm);

    /// <summary>Removes the algorithm for <c>ALevel</c>.</summary>
    /// <param name="ALevel">Access-level byte.</param>
    procedure UnregisterAlgorithm(ALevel: Byte);

    /// <summary>
    ///   Computes the key for a (level, seed) pair.
    /// </summary>
    /// <param name="ALevel">Access-level byte.</param>
    /// <param name="ASeed">Seed bytes returned by the ECU.</param>
    /// <returns>Computed key.</returns>
    /// <exception cref="EOBDSeedKey">
    ///   No algorithm registered for <c>ALevel</c>.
    /// </exception>
    function ComputeKey(ALevel: Byte;
      const ASeed: TBytes): TBytes;

    /// <summary>
    ///   Returns <c>True</c> when an algorithm is registered for
    ///   <c>ALevel</c>.
    /// </summary>
    /// <param name="ALevel">Access-level byte.</param>
    function HasAlgorithm(ALevel: Byte): Boolean;

    /// <summary>Snapshot of every registered level.</summary>
    /// <returns>Sorted array of access-level bytes.</returns>
    function RegisteredLevels: TArray<Byte>;
  end;

implementation

constructor TOBDSeedKeyRegistry.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FAlgorithms := TDictionary<Byte, TOBDSeedKeyAlgorithm>.Create;
end;

destructor TOBDSeedKeyRegistry.Destroy;
begin
  FAlgorithms.Free;
  FLock.Free;
  inherited;
end;

procedure TOBDSeedKeyRegistry.RegisterAlgorithm(ALevel: Byte;
  const AAlgorithm: TOBDSeedKeyAlgorithm);
begin
  if not Assigned(AAlgorithm) then
    raise EOBDSeedKey.CreateFmt(
      'RegisterAlgorithm: nil algorithm for level 0x%.2x',
      [ALevel]);
  FLock.Enter;
  try
    FAlgorithms.AddOrSetValue(ALevel, AAlgorithm);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDSeedKeyRegistry.UnregisterAlgorithm(ALevel: Byte);
begin
  FLock.Enter;
  try
    FAlgorithms.Remove(ALevel);
  finally
    FLock.Leave;
  end;
end;

function TOBDSeedKeyRegistry.ComputeKey(ALevel: Byte;
  const ASeed: TBytes): TBytes;
var
  Algo: TOBDSeedKeyAlgorithm;
begin
  FLock.Enter;
  try
    if not FAlgorithms.TryGetValue(ALevel, Algo) then
      raise EOBDSeedKey.CreateFmt(
        'No seed-key algorithm registered for level 0x%.2x',
        [ALevel]);
  finally
    FLock.Leave;
  end;
  Result := Algo(ASeed);
end;

function TOBDSeedKeyRegistry.HasAlgorithm(ALevel: Byte): Boolean;
begin
  FLock.Enter;
  try
    Result := FAlgorithms.ContainsKey(ALevel);
  finally
    FLock.Leave;
  end;
end;

function TOBDSeedKeyRegistry.RegisteredLevels: TArray<Byte>;
var
  Keys: TArray<Byte>;
  I, J: Integer;
  Temp: Byte;
begin
  FLock.Enter;
  try
    Keys := FAlgorithms.Keys.ToArray;
  finally
    FLock.Leave;
  end;
  // Selection sort — N tiny (at most a handful of levels).
  for I := 0 to High(Keys) - 1 do
    for J := I + 1 to High(Keys) do
      if Keys[J] < Keys[I] then
      begin
        Temp := Keys[I];
        Keys[I] := Keys[J];
        Keys[J] := Temp;
      end;
  Result := Keys;
end;

end.
