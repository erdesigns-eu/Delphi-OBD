//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.SeedKey.pas
// CONTENTS       : Pluggable seed-key algorithms for UDS service 0x27
//                  SecurityAccess. Per-OEM, per-level registry plus
//                  a small set of publicly-documented reference
//                  algorithms.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Real production seed-key algorithms are NDA-
//                  protected by every OEM. The reference algorithms
//                  shipped here are the publicly-documented starter
//                  set (KWP2000 two's-complement, XOR mask, byte
//                  rotation, fixed-key) — useful for unit tests, lab
//                  ECUs, and aftermarket modules. Production users
//                  register their own implementations at startup
//                  via the per-extension registry.
//------------------------------------------------------------------------------
unit OBD.OEM.SeedKey;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults;

type
  EOBDSeedKeyError = class(Exception);

  /// <summary>
  ///   Computes the key bytes for a SecurityAccess seed at a given
  ///   level. Implementations are pure functions — same seed + level
  ///   always yields the same key — so the registry can cache them.
  /// </summary>
  IOBDSeedKeyAlgorithm = interface
    ['{C7F9D2A4-1B5E-4F8C-9D3A-6E7B2F4D8C1A}']
    /// <summary>Compute the response key for <c>Seed</c> at <c>Level</c>.
    /// Throws <c>EOBDSeedKeyError</c> on invalid seed length.</summary>
    function ComputeKey(const Seed: TBytes; const Level: Byte): TBytes;
    /// <summary>Display label for logs / audit trails.</summary>
    function Description: string;
    /// <summary>Provenance: <c>"public-domain"</c>, <c>"iso-14229-1"</c>,
    /// <c>"oem-spec"</c>, <c>"community-pr"</c>, …</summary>
    function Source: string;
    /// <summary>True only when matched against an OEM spec or
    /// reproducible capture fixture. Production callers filter
    /// unverified algorithms out of flashing paths.</summary>
    function Verified: Boolean;
  end;

  /// <summary>Per-OEM, per-level registry. Each level can have multiple
  /// candidate algorithms — useful when an OEM rolled the algorithm
  /// across model years and the caller hasn't yet picked the right
  /// variant.</summary>
  TOBDSeedKeyRegistry = class
  strict private
    FLock: TCriticalSection;
    FByLevel: TObjectDictionary<Byte, TList<IOBDSeedKeyAlgorithm>>;
    function ListFor(const Level: Byte; CreateIfMissing: Boolean):
      TList<IOBDSeedKeyAlgorithm>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Register <c>Algo</c> for <c>Level</c>. Newer registrations
    /// take precedence (LIFO) — production users register their NDA-
    /// algorithm last so it shadows the public starter.</summary>
    procedure RegisterAlgorithm(const Level: Byte;
      const Algo: IOBDSeedKeyAlgorithm);
    procedure UnregisterAlgorithm(const Level: Byte;
      const Algo: IOBDSeedKeyAlgorithm);

    /// <summary>The primary algorithm for the level (most-recently
    /// registered). Returns nil when none is registered.</summary>
    function Find(const Level: Byte): IOBDSeedKeyAlgorithm;
    /// <summary>All algorithms registered for the level, newest first.</summary>
    function FindAll(const Level: Byte): TArray<IOBDSeedKeyAlgorithm>;
    /// <summary>True if at least one algorithm is registered.</summary>
    function HasAlgorithm(const Level: Byte): Boolean;
    /// <summary>Levels that have at least one algorithm registered.</summary>
    function Levels: TArray<Byte>;
    /// <summary>Drop every registration. Test-only helper; production
    /// callers replace specific levels via Unregister.</summary>
    procedure Clear;
  end;

  /// <summary>Convenience base for algorithm implementations.</summary>
  TOBDSeedKeyAlgorithmBase = class(TInterfacedObject, IOBDSeedKeyAlgorithm)
  strict private
    FDescription: string;
    FSource: string;
    FVerified: Boolean;
  protected
    /// <summary>Validate the seed length expected by this algorithm.
    /// Default: any non-empty length is acceptable. Subclasses
    /// override for fixed-width seeds.</summary>
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

  /// <summary>Build the UDS request frame for a seed read at the given
  /// level — returns SID 0x27 followed by the level byte.</summary>
function RequestSeedFrame(const Level: Byte): TBytes;

  /// <summary>Build the UDS request frame for a key send — SID 0x27 +
  /// (Level + 1) + key bytes (ISO 14229 §10.5).</summary>
function SendKeyFrame(const Level: Byte; const Key: TBytes): TBytes;

  /// <summary>Extract the seed bytes from a positive 0x67 0xLL response
  /// payload. Throws <c>EOBDSeedKeyError</c> on a malformed reply.</summary>
function ExtractSeed(const Response: TBytes; const Level: Byte): TBytes;

implementation

//==============================================================================
//  TOBDSeedKeyRegistry
//==============================================================================
constructor TOBDSeedKeyRegistry.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FByLevel := TObjectDictionary<Byte, TList<IOBDSeedKeyAlgorithm>>.Create([doOwnsValues]);
end;

destructor TOBDSeedKeyRegistry.Destroy;
begin
  FByLevel.Free;
  FLock.Free;
  inherited;
end;

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

procedure TOBDSeedKeyRegistry.RegisterAlgorithm(const Level: Byte;
  const Algo: IOBDSeedKeyAlgorithm);
var
  L: TList<IOBDSeedKeyAlgorithm>;
begin
  if Algo = nil then Exit;
  FLock.Enter;
  try
    L := ListFor(Level, True);
    // Insert at head so newer registrations win.
    L.Insert(0, Algo);
  finally
    FLock.Leave;
  end;
end;

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
constructor TOBDSeedKeyAlgorithmBase.Create(
  const ADescription, ASource: string; const AVerified: Boolean);
begin
  inherited Create;
  FDescription := ADescription;
  FSource := ASource;
  FVerified := AVerified;
end;

procedure TOBDSeedKeyAlgorithmBase.CheckSeed(const Seed: TBytes);
begin
  if Length(Seed) = 0 then
    raise EOBDSeedKeyError.Create('Seed must not be empty');
end;

function TOBDSeedKeyAlgorithmBase.Description: string;
begin Result := FDescription; end;
function TOBDSeedKeyAlgorithmBase.Source: string;
begin Result := FSource; end;
function TOBDSeedKeyAlgorithmBase.Verified: Boolean;
begin Result := FVerified; end;

//==============================================================================
//  TOBDSeedKeyKWP2000TwosComplement
//==============================================================================
constructor TOBDSeedKeyKWP2000TwosComplement.Create;
begin
  inherited Create('KWP2000 two''s-complement (NOT seed + 1)',
                   'iso-14229-1-example', False);
end;

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
constructor TOBDSeedKeyXorMask.Create(const Mask: TBytes;
  const Description, Source: string; const Verified: Boolean);
begin
  inherited Create(Description, Source, Verified);
  if Length(Mask) = 0 then
    raise EOBDSeedKeyError.Create('XOR mask must not be empty');
  FMask := Copy(Mask, 0, Length(Mask));
end;

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
constructor TOBDSeedKeyConstant.Create(const Key: TBytes;
  const Description, Source: string; const Verified: Boolean);
begin
  inherited Create(Description, Source, Verified);
  if Length(Key) = 0 then
    raise EOBDSeedKeyError.Create('Constant key must not be empty');
  FKey := Copy(Key, 0, Length(Key));
end;

function TOBDSeedKeyConstant.ComputeKey(const Seed: TBytes;
  const Level: Byte): TBytes;
begin
  CheckSeed(Seed);
  Result := Copy(FKey, 0, Length(FKey));
end;

//==============================================================================
//  Frame helpers
//==============================================================================
function RequestSeedFrame(const Level: Byte): TBytes;
begin
  if (Level and 1) = 0 then
    raise EOBDSeedKeyError.CreateFmt(
      'Seed-request levels must be odd (got 0x%.2X)', [Level]);
  Result := TBytes.Create($27, Level);
end;

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
