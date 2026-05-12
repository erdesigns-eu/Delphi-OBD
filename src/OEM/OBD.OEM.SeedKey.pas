//------------------------------------------------------------------------------
//  OBD.OEM.SeedKey
//
//  Pluggable seed-key algorithms for UDS service 0x27
//  SecurityAccess. Per-OEM, per-level registry plus a small set of
//  publicly-documented reference algorithms (KWP2000 two's-
//  complement, XOR mask, byte rotation, fixed-key).
//
//  Real production seed-key algorithms are NDA-protected by every
//  OEM. The reference algorithms here are the publicly-documented
//  starter set — useful for unit tests, lab ECUs, and aftermarket
//  modules. Production users register their own implementations
//  at startup via <see cref="TOBDSeedKeyRegistry.RegisterAlgorithm"/>;
//  the registry is LIFO so the proprietary algorithm shadows the
//  public starter without an explicit unregister.
//
//  Frame helpers (<c>RequestSeedFrame</c>, <c>SendKeyFrame</c>,
//  <c>ExtractSeed</c>) build / parse the on-wire bytes so callers
//  don't reinvent the ISO 14229 §10 envelope.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial implementation (lambda registry).
//    2026-05-12  ERD  Interface-based algorithm contract,
//                     per-level multi-algorithm LIFO registry,
//                     four reference algorithm classes, ISO 14229
//                     §10 frame helpers. Vendor extensions register
//                     their defaults via
//                     SeedDefaultSeedKeyAlgorithms.
//------------------------------------------------------------------------------
unit OBD.OEM.SeedKey;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults;

type
  EOBDSeedKeyError = class(Exception);
  /// <summary>Shorter alias for callers that prefer the bare
  /// name.</summary>
  EOBDSeedKey = EOBDSeedKeyError;

  /// <summary>
  ///   Lambda-style algorithm: takes the seed, returns the key.
  ///   Convenience surface used by tests and small adapters; the
  ///   interface form (<see cref="IOBDSeedKeyAlgorithm"/>) is the
  ///   canonical contract because it carries provenance metadata
  ///   (<c>Description</c>, <c>Source</c>, <c>Verified</c>).
  ///   Lambdas registered through the overload below are wrapped
  ///   in an internal anonymous-algorithm adapter.
  /// </summary>
  TOBDSeedKeyAlgorithm = reference to function(const Seed: TBytes): TBytes;

  /// <summary>
  ///   Computes the key bytes for a SecurityAccess seed at a given
  ///   level. Implementations are pure functions — same seed + level
  ///   always yields the same key — so the registry can cache them.
  /// </summary>
  IOBDSeedKeyAlgorithm = interface
    ['{C7F9D2A4-1B5E-4F8C-9D3A-6E7B2F4D8C1A}']
    /// <summary>
    ///   Compute the response key for <c>Seed</c> at <c>Level</c>.
    ///   Throws <c>EOBDSeedKeyError</c> on invalid seed length.
    /// </summary>
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes;
    /// <summary>
    ///   Display label for logs / audit trails.
    /// </summary>
    function Description: string;
    /// <summary>
    ///   Provenance: <c>"public-domain"</c>, <c>"iso-14229-1"</c>,
    ///   <c>"oem-spec"</c>, <c>"community-pr"</c>, …
    /// </summary>
    function Source: string;
    /// <summary>
    ///   True only when matched against an OEM spec or
    ///   reproducible capture fixture. Production callers filter
    ///   unverified algorithms out of flashing paths.
    /// </summary>
    function Verified: Boolean;
  end;

  /// <summary>
  ///   Per-OEM, per-level registry. Each level can have multiple
  ///   candidate algorithms — useful when an OEM rolled the algorithm
  ///   across model years and the caller hasn't yet picked the right
  ///   variant.
  /// </summary>
  TOBDSeedKeyRegistry = class
  strict private
    FLock: TCriticalSection;
    FByLevel: TObjectDictionary<Byte, TList<IOBDSeedKeyAlgorithm>>;
    function ListFor(const Level: Byte; CreateIfMissing: Boolean):
      TList<IOBDSeedKeyAlgorithm>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Register <c>Algo</c> for <c>Level</c>. Newer registrations
    ///   take precedence (LIFO) — production users register their NDA-
    ///   algorithm last so it shadows the public starter.
    /// </summary>
    procedure RegisterAlgorithm(const Level: Byte;
      const Algo: IOBDSeedKeyAlgorithm); overload;
    /// <summary>Lambda overload — wraps <c>Algo</c> in an
    /// internal anonymous-algorithm adapter and registers it
    /// at <c>Level</c>. <c>nil</c> raises
    /// <see cref="EOBDSeedKey"/>.</summary>
    procedure RegisterAlgorithm(const Level: Byte;
      const Algo: TOBDSeedKeyAlgorithm); overload;
    procedure UnregisterAlgorithm(const Level: Byte;
      const Algo: IOBDSeedKeyAlgorithm); overload;
    /// <summary>Drop every algorithm registered for
    /// <c>Level</c>.</summary>
    procedure UnregisterAlgorithm(const Level: Byte); overload;

    /// <summary>
    ///   Convenience: looks up the primary algorithm for
    ///   <c>Level</c> and computes the key for <c>Seed</c>.
    /// </summary>
    /// <exception cref="EOBDSeedKey">No algorithm is registered
    /// for <c>Level</c>.</exception>
    function ComputeKey(const Level: Byte;
      const Seed: TBytes): TBytes;

    /// <summary>
    ///   The primary algorithm for the level (most-recently
    ///   registered). Returns nil when none is registered.
    /// </summary>
    function Find(const Level: Byte): IOBDSeedKeyAlgorithm;
    /// <summary>
    ///   All algorithms registered for the level, newest first.
    /// </summary>
    function FindAll(const Level: Byte): TArray<IOBDSeedKeyAlgorithm>;
    /// <summary>
    ///   True if at least one algorithm is registered.
    /// </summary>
    function HasAlgorithm(const Level: Byte): Boolean;
    /// <summary>
    ///   Levels that have at least one algorithm registered.
    /// </summary>
    function Levels: TArray<Byte>;
    /// <summary>Like <see cref="Levels"/> but the result is
    /// sorted ascending.</summary>
    function RegisteredLevels: TArray<Byte>;
    /// <summary>
    ///   Drop every registration. Test-only helper; production
    ///   callers replace specific levels via Unregister.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  ///   Convenience base for algorithm implementations.
  /// </summary>
  TOBDSeedKeyAlgorithmBase = class(TInterfacedObject, IOBDSeedKeyAlgorithm)
  strict private
    FDescription: string;
    FSource: string;
    FVerified: Boolean;
  protected
    /// <summary>
    ///   Validate the seed length expected by this algorithm.
    ///   Default: any non-empty length is acceptable. Subclasses
    ///   override for fixed-width seeds.
    /// </summary>
    procedure CheckSeed(const Seed: TBytes); virtual;
  public
    constructor Create(const ADescription, ASource: string;
      const AVerified: Boolean = False);
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes;
      virtual; abstract;
    function Description: string;
    function Source: string;
    function Verified: Boolean;
  end;

  //----------------------------------------------------------------------------
  //  Reference algorithms — publicly documented. Verified=False until
  //  cross-validated against an OEM spec or capture fixture.
  //----------------------------------------------------------------------------

  /// <summary>
  ///   key[i] = NOT seed[i] + 1 (two's-complement of the seed,
  ///   computed byte-wise with carry). Documented as the "ISO 14229
  ///   example" Level 1 algorithm; some pre-UDS KWP2000 ECUs accept
  ///   it verbatim.
  /// </summary>
  TOBDSeedKeyKWP2000TwosComplement = class(TOBDSeedKeyAlgorithmBase)
  public
    constructor Create;
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes; override;
  end;

  /// <summary>
  ///   key = seed XOR Mask. Mask length must equal seed length;
  ///   shorter masks are tiled. Used by some aftermarket modules
  ///   (universal seed-key bypass dongles) and as a teaching example.
  /// </summary>
  TOBDSeedKeyXorMask = class(TOBDSeedKeyAlgorithmBase)
  strict private
    FMask: TBytes;
  public
    constructor Create(const Mask: TBytes;
      const Description: string = 'XOR mask seed-key';
      const Source: string = 'public-domain';
      const Verified: Boolean = False);
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes; override;
    property Mask: TBytes read FMask;
  end;

  /// <summary>
  ///   key[i] = (seed[(i + Shift) mod N] ROL Rotate) XOR Mask[i].
  ///   Approximates a class of legacy GM / Ford pre-UDS algorithms.
  ///   Shift, Rotate (0..7) and Mask are all caller-supplied so the
  ///   class can model many variants without subclassing.
  /// </summary>
  TOBDSeedKeyByteRotate = class(TOBDSeedKeyAlgorithmBase)
  strict private
    FShift: Integer;
    FRotate: Byte;
    FMask: TBytes;
  public
    constructor Create(const Shift: Integer; const Rotate: Byte;
      const Mask: TBytes;
      const Description: string = 'Byte-rotate + XOR seed-key';
      const Source: string = 'public-domain';
      const Verified: Boolean = False);
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes; override;
  end;

  /// <summary>
  ///   Returns a fixed key independent of the seed. Some pre-2010
  ///   dealer modules accept Level 1 with a constant. Useful for
  ///   testing the broader flow without coupling to a real algorithm.
  /// </summary>
  TOBDSeedKeyConstant = class(TOBDSeedKeyAlgorithmBase)
  strict private
    FKey: TBytes;
  public
    constructor Create(const Key: TBytes;
      const Description: string = 'Constant key (test fixture)';
      const Source: string = 'public-domain';
      const Verified: Boolean = False);
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes; override;
  end;

  //----------------------------------------------------------------------------
  //  Helpers used by SecurityAccess flow code.
  //----------------------------------------------------------------------------

  /// <summary>
  ///   Build the UDS request frame for a seed read at the given
  ///   level — returns SID 0x27 followed by the level byte.
  /// </summary>
function RequestSeedFrame(const Level: Byte): TBytes;

  /// <summary>
  ///   Build the UDS request frame for a key send — SID 0x27 +
  ///   (Level + 1) + key bytes (ISO 14229 §10.5).
  /// </summary>
function SendKeyFrame(const Level: Byte; const Key: TBytes): TBytes;

  /// <summary>
  ///   Extract the seed bytes from a positive 0x67 0xLL response
  ///   payload. Throws <c>EOBDSeedKeyError</c> on a malformed reply.
  /// </summary>
function ExtractSeed(const Response: TBytes; const Level: Byte): TBytes;

implementation

type
  /// <summary>Internal adapter that wraps a
  /// <see cref="TOBDSeedKeyAlgorithm"/> lambda in the
  /// <see cref="IOBDSeedKeyAlgorithm"/> contract so the lambda
  /// overload can register through the same per-level list as
  /// the interface form.</summary>
  TOBDLambdaSeedKeyAlgorithm = class(TOBDSeedKeyAlgorithmBase)
  strict private
    FFunc: TOBDSeedKeyAlgorithm;
  public
    constructor Create(const AFunc: TOBDSeedKeyAlgorithm);
    function ComputeKey(const Seed: TBytes;
      const Level: Byte): TBytes; override;
  end;

constructor TOBDLambdaSeedKeyAlgorithm.Create(
  const AFunc: TOBDSeedKeyAlgorithm);
begin
  inherited Create('Anonymous lambda seed-key algorithm',
                   'caller-supplied', False);
  FFunc := AFunc;
end;

function TOBDLambdaSeedKeyAlgorithm.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
begin
  CheckSeed(Seed);
  Result := FFunc(Seed);
end;

//==============================================================================
//  TOBDSeedKeyRegistry
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyRegistry.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FByLevel := TObjectDictionary<Byte, TList<IOBDSeedKeyAlgorithm>>.Create([doOwnsValues]);
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDSeedKeyRegistry.Destroy;
begin
  FByLevel.Free;
  FLock.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// LIST FOR
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.ListFor(const Level: Byte;
  CreateIfMissing: Boolean): TList<IOBDSeedKeyAlgorithm>;
begin
  if not FByLevel.TryGetValue(Level, Result) then
  begin
    if not CreateIfMissing then Exit(nil);
    Result := TList<IOBDSeedKeyAlgorithm>.Create;
    FByLevel.Add(Level, Result);
  end;
end;

//------------------------------------------------------------------------------
// REGISTER ALGORITHM
//------------------------------------------------------------------------------
procedure TOBDSeedKeyRegistry.RegisterAlgorithm(const Level: Byte;
  const Algo: IOBDSeedKeyAlgorithm);
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  if Algo = nil then
    raise EOBDSeedKey.Create('Algorithm must not be nil');
  FLock.Enter;
  try
    L := ListFor(Level, True);
    // Insert at head so newer registrations win.
    L.Insert(0, Algo);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// UNREGISTER ALGORITHM
//------------------------------------------------------------------------------
procedure TOBDSeedKeyRegistry.UnregisterAlgorithm(const Level: Byte;
  const Algo: IOBDSeedKeyAlgorithm);
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  if Algo = nil then Exit;
  FLock.Enter;
  try
    L := ListFor(Level, False);
    if Assigned(L) then L.Remove(Algo);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// FIND
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.Find(
  const Level: Byte): IOBDSeedKeyAlgorithm;
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  Result := nil;
  FLock.Enter;
  try
    L := ListFor(Level, False);
    if Assigned(L) and (L.Count > 0) then Result := L[0];
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// FIND ALL
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.FindAll(
  const Level: Byte): TArray<IOBDSeedKeyAlgorithm>;
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  Result := nil;
  FLock.Enter;
  try
    L := ListFor(Level, False);
    if Assigned(L) then Result := L.ToArray;
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// HAS ALGORITHM
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.HasAlgorithm(const Level: Byte): Boolean;
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  FLock.Enter;
  try
    L := ListFor(Level, False);
    Result := Assigned(L) and (L.Count > 0);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// LEVELS
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.Levels: TArray<Byte>;
var
  Pair: TPair<Byte, TList<IOBDSeedKeyAlgorithm>>;
  Tmp: TList<Byte>;
begin
  Tmp := TList<Byte>.Create;
  try
    FLock.Enter;
    try
      for Pair in FByLevel do
        if Pair.Value.Count > 0 then Tmp.Add(Pair.Key);
    finally
      FLock.Leave;
    end;
    Result := Tmp.ToArray;
  finally
    Tmp.Free;
  end;
end;

//------------------------------------------------------------------------------
// REGISTER ALGORITHM (lambda overload)
//------------------------------------------------------------------------------
procedure TOBDSeedKeyRegistry.RegisterAlgorithm(const Level: Byte;
  const Algo: TOBDSeedKeyAlgorithm);
begin
  if not Assigned(Algo) then
    raise EOBDSeedKey.Create('Lambda algorithm must not be nil');
  RegisterAlgorithm(Level, IOBDSeedKeyAlgorithm(
    TOBDLambdaSeedKeyAlgorithm.Create(Algo)));
end;

//------------------------------------------------------------------------------
// UNREGISTER ALGORITHM (level-only overload)
//------------------------------------------------------------------------------
procedure TOBDSeedKeyRegistry.UnregisterAlgorithm(const Level: Byte);
begin
  FLock.Enter;
  try
    FByLevel.Remove(Level);
  finally
    FLock.Leave;
  end;
end;

//------------------------------------------------------------------------------
// COMPUTE KEY
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.ComputeKey(const Level: Byte;
  const Seed: TBytes): TBytes;
var
  Algo: IOBDSeedKeyAlgorithm;
begin
  Algo := Find(Level);
  if Algo = nil then
    raise EOBDSeedKey.CreateFmt(
      'No seed-key algorithm registered for level 0x%.2X', [Level]);
  Result := Algo.ComputeKey(Seed, Level);
end;

//------------------------------------------------------------------------------
// REGISTERED LEVELS
//------------------------------------------------------------------------------
function TOBDSeedKeyRegistry.RegisteredLevels: TArray<Byte>;
var
  Tmp: TArray<Byte>;
  I, J: Integer;
  Min: Byte;
begin
  Tmp := Levels;
  // Selection sort — registry size is tiny (≤ a dozen levels in
  // practice) so an in-place O(n²) sort is fine and avoids pulling
  // in a generic comparator.
  for I := 0 to High(Tmp) - 1 do
  begin
    Min := I;
    for J := I + 1 to High(Tmp) do
      if Tmp[J] < Tmp[Min] then Min := J;
    if Min <> I then
    begin
      Tmp[I] := Tmp[Min] xor Tmp[I];
      Tmp[Min] := Tmp[Min] xor Tmp[I];
      Tmp[I] := Tmp[Min] xor Tmp[I];
    end;
  end;
  Result := Tmp;
end;

//------------------------------------------------------------------------------
// CLEAR
//------------------------------------------------------------------------------
procedure TOBDSeedKeyRegistry.Clear;
begin
  FLock.Enter;
  try
    FByLevel.Clear;
  finally
    FLock.Leave;
  end;
end;

//==============================================================================
//  TOBDSeedKeyAlgorithmBase
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyAlgorithmBase.Create(
  const ADescription, ASource: string; const AVerified: Boolean);
begin
  inherited Create;
  FDescription := ADescription;
  FSource := ASource;
  FVerified := AVerified;
end;

//------------------------------------------------------------------------------
// CHECK SEED
//------------------------------------------------------------------------------
procedure TOBDSeedKeyAlgorithmBase.CheckSeed(const Seed: TBytes);
begin
  if Length(Seed) = 0 then
    raise EOBDSeedKeyError.Create('Seed must not be empty');
end;

//------------------------------------------------------------------------------
// DESCRIPTION
//------------------------------------------------------------------------------
function TOBDSeedKeyAlgorithmBase.Description: string;
begin
  Result := FDescription;
end;

//------------------------------------------------------------------------------
// SOURCE
//------------------------------------------------------------------------------
function TOBDSeedKeyAlgorithmBase.Source: string;
begin
  Result := FSource;
end;

//------------------------------------------------------------------------------
// VERIFIED
//------------------------------------------------------------------------------
function TOBDSeedKeyAlgorithmBase.Verified: Boolean;
begin
  Result := FVerified;
end;

//==============================================================================
//  TOBDSeedKeyKWP2000TwosComplement
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyKWP2000TwosComplement.Create;
begin
  inherited Create('KWP2000 two''s-complement (NOT seed + 1)',
                   'iso-14229-1-example', False);
end;

//------------------------------------------------------------------------------
// COMPUTE KEY
//------------------------------------------------------------------------------
function TOBDSeedKeyKWP2000TwosComplement.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
var
  I: Integer;
  Carry: Integer;
  Sum: Integer;
begin
  CheckSeed(Seed);
  SetLength(Result, Length(Seed));
  // Produce the byte-wise two's complement: invert every byte, then
  // add 1 starting from the least-significant byte (last index, big-
  // endian convention used by the KWP2000 example).
  for I := 0 to High(Seed) do
    Result[I] := (not Seed[I]) and $FF;
  Carry := 1;
  for I := High(Result) downto 0 do
  begin
    if Carry = 0 then Break;
    Sum := Result[I] + Carry;
    Result[I] := Sum and $FF;
    Carry := Sum shr 8;
  end;
end;

//==============================================================================
//  TOBDSeedKeyXorMask
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyXorMask.Create(const Mask: TBytes;
  const Description, Source: string; const Verified: Boolean);
begin
  inherited Create(Description, Source, Verified);
  if Length(Mask) = 0 then
    raise EOBDSeedKeyError.Create('XOR mask must not be empty');
  FMask := Copy(Mask, 0, Length(Mask));
end;

//------------------------------------------------------------------------------
// COMPUTE KEY
//------------------------------------------------------------------------------
function TOBDSeedKeyXorMask.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
var
  I: Integer;
begin
  CheckSeed(Seed);
  SetLength(Result, Length(Seed));
  for I := 0 to High(Seed) do
    Result[I] := Seed[I] xor FMask[I mod Length(FMask)];
end;

//==============================================================================
//  TOBDSeedKeyByteRotate
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyByteRotate.Create(const Shift: Integer;
  const Rotate: Byte; const Mask: TBytes;
  const Description, Source: string; const Verified: Boolean);
begin
  inherited Create(Description, Source, Verified);
  if (Rotate > 7) then
    raise EOBDSeedKeyError.Create('Rotate must be 0..7');
  if Length(Mask) = 0 then
    raise EOBDSeedKeyError.Create('Mask must not be empty');
  FShift := Shift;
  FRotate := Rotate;
  FMask := Copy(Mask, 0, Length(Mask));
end;

//------------------------------------------------------------------------------
// COMPUTE KEY
//------------------------------------------------------------------------------
function TOBDSeedKeyByteRotate.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
var
  N, I, SrcIdx: Integer;
  B: Byte;
begin
  CheckSeed(Seed);
  N := Length(Seed);
  SetLength(Result, N);
  for I := 0 to N - 1 do
  begin
    SrcIdx := ((I + FShift) mod N + N) mod N;  // signed-safe modulo
    B := Seed[SrcIdx];
    if FRotate > 0 then
      B := ((B shl FRotate) or (B shr (8 - FRotate))) and $FF;
    Result[I] := B xor FMask[I mod Length(FMask)];
  end;
end;

//==============================================================================
//  TOBDSeedKeyConstant
//==============================================================================

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSeedKeyConstant.Create(const Key: TBytes;
  const Description, Source: string; const Verified: Boolean);
begin
  inherited Create(Description, Source, Verified);
  if Length(Key) = 0 then
    raise EOBDSeedKeyError.Create('Constant key must not be empty');
  FKey := Copy(Key, 0, Length(Key));
end;

//------------------------------------------------------------------------------
// COMPUTE KEY
//------------------------------------------------------------------------------
function TOBDSeedKeyConstant.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
begin
  CheckSeed(Seed);
  Result := Copy(FKey, 0, Length(FKey));
end;

//==============================================================================
//  Frame helpers
//==============================================================================

//------------------------------------------------------------------------------
// REQUEST SEED FRAME
//------------------------------------------------------------------------------
function RequestSeedFrame(const Level: Byte): TBytes;
begin
  if (Level and 1) = 0 then
    raise EOBDSeedKeyError.CreateFmt(
      'Seed-request levels must be odd (got 0x%.2X)', [Level]);
  Result := TBytes.Create($27, Level);
end;

//------------------------------------------------------------------------------
// SEND KEY FRAME
//------------------------------------------------------------------------------
function SendKeyFrame(const Level: Byte; const Key: TBytes): TBytes;
var
  I: Integer;
begin
  // ISO 14229 §10.5: SendKey level = RequestSeed level + 1.
  if (Level and 1) = 0 then
    raise EOBDSeedKeyError.CreateFmt(
      'SendKey expects the matching seed-request (odd) level, got 0x%.2X', [Level]);
  if Length(Key) = 0 then
    raise EOBDSeedKeyError.Create('Key bytes must not be empty');
  SetLength(Result, 2 + Length(Key));
  Result[0] := $27;
  Result[1] := Level + 1;
  for I := 0 to High(Key) do Result[2 + I] := Key[I];
end;

//------------------------------------------------------------------------------
// EXTRACT SEED
//------------------------------------------------------------------------------
function ExtractSeed(const Response: TBytes; const Level: Byte): TBytes;
begin
  if Length(Response) < 2 then
    raise EOBDSeedKeyError.Create('Seed response too short');
  if Response[0] <> $67 then
    raise EOBDSeedKeyError.CreateFmt(
      'Expected positive seed response (0x67), got 0x%.2X', [Response[0]]);
  if Response[1] <> Level then
    raise EOBDSeedKeyError.CreateFmt(
      'Seed level mismatch: requested 0x%.2X, ECU returned 0x%.2X',
      [Level, Response[1]]);
  Result := Copy(Response, 2, Length(Response) - 2);
end;

end.
