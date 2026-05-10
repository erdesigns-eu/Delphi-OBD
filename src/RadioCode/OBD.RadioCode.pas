//------------------------------------------------------------------------------
//  OBD.RadioCode
//
//  Foundation for the Delphi-OBD radio-code subsystem:
//
//    - <see cref="TOBDRadioCode"/> — abstract <c>TComponent</c>
//      base every vendor calculator inherits from. Drop a vendor
//      component on a form, set <c>Input</c>, call
//      <c>Calculate</c>, read <c>Result_</c>.
//
//    - <see cref="TOBDRadioCodeRegistry"/> — process-wide
//      registry of vendor classes (not instances). Vendor units
//      register their class at unit initialization so the
//      starter wizard and the VIN resolver can enumerate or pick
//      a calculator without a hard-coded list.
//
//  Per-vendor calculators live in <c>OBD.RadioCode.&lt;Vendor&gt;.pas</c>
//  and each is its own palette component.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation. Component-first
//                     redesign: each vendor is a TComponent
//                     subclass, registered on the 'OBD Radio'
//                     palette tab. The class registry holds
//                     metaclasses so hosts (the wizard, the VIN
//                     resolver) can discover available vendors
//                     without instantiating them.
//------------------------------------------------------------------------------

unit OBD.RadioCode;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  OBD.RadioCode.Types;

type
  /// <summary>Fires from <c>DoCalculate</c> for vendor components
  /// whose proprietary algorithm is not bundled in this
  /// distribution. The host supplies the calculation.</summary>
  TOBDRadioCodeCalcEvent = procedure(Sender: TObject;
    const AInput: string; const AContext: TOBDRadioCodeContext;
    var AResult: TOBDRadioCodeResult) of object;

  /// <summary>Abstract base for every vendor radio-code
  /// calculator component. Vendors inherit, override
  /// <c>BrandKey</c> / <c>DisplayName</c> / <c>Description</c>,
  /// optionally override <c>DoCalculate</c> (when a real
  /// algorithm is available), and call
  /// <c>TOBDRadioCodeRegistry.Default.RegisterClass</c> at unit
  /// initialization.
  ///
  /// <para>Many production radio-code algorithms are
  /// proprietary / licensed and are NOT bundled with this
  /// open-source distribution. For those vendors, the default
  /// <c>DoCalculate</c> fires the <c>OnCalculate</c> event so the
  /// host can supply the algorithm (their own implementation, a
  /// licensed code-service call, or a network round-trip). If
  /// no handler is wired, the result reports a clear
  /// "algorithm not bundled" message.</para></summary>
  TOBDRadioCode = class abstract(TComponent)
  strict private
    FInput:       string;
    FVIN:         string;
    FModelHint:   string;
    FRegion:      TOBDRadioCodeRegion;
    FResult:      TOBDRadioCodeResult;
    FOnCalculate: TOBDRadioCodeCalcEvent;
  protected
    /// <summary>Trim + uppercase. Vendors override for
    /// vendor-specific normalisation (some need the original
    /// case preserved).</summary>
    function SanitizeInput(const AInput: string): string; virtual;

    /// <summary>Helper: input must be exactly <c>AExpected</c>
    /// characters. Sets <c>AReason</c> on failure.</summary>
    function ValidateLength(const AInput: string;
      AExpected: Integer; out AReason: string): Boolean;
    /// <summary>Helper: input must be all decimal digits.</summary>
    function ValidateAllDigits(const AInput: string;
      out AReason: string): Boolean;
    /// <summary>Helper: input must be all alpha letters.</summary>
    function ValidateAllAlpha(const AInput: string;
      out AReason: string): Boolean;
    /// <summary>Helper: input must be all hex digits.</summary>
    function ValidateHex(const AInput: string;
      out AReason: string): Boolean;
    /// <summary>Helper: input must start with <c>APrefix</c>.</summary>
    function ValidatePrefix(const AInput, APrefix: string;
      out AReason: string): Boolean;
    /// <summary>Helper: characters in the closed range
    /// <c>AStart..AEnd</c> must all be alphanumeric.</summary>
    function ValidateAlphanumericRange(const AInput: string;
      AStart, AEnd: Integer; out AReason: string): Boolean;
    /// <summary>Helper: characters in the closed range
    /// <c>AStart..AEnd</c> must all be decimal digits.</summary>
    function ValidateDigitRange(const AInput: string;
      AStart, AEnd: Integer; out AReason: string): Boolean;

    /// <summary>Vendor-supplied input check. Default: accept any
    /// non-empty string.</summary>
    function DoValidate(const AInput: string;
      out AReason: string): Boolean; virtual;

    /// <summary>Vendor-supplied calculation. <c>AInput</c> has
    /// already been sanitised and validated. Default
    /// implementation fires <c>OnCalculate</c> when a host has
    /// wired one; otherwise returns a result whose
    /// <c>Success</c> is False and <c>Message</c> reports that
    /// the bundled distribution does not include this vendor's
    /// algorithm. Vendors that do ship a real algorithm
    /// (database lookup, public spec) override this method.</summary>
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
      virtual;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Brand identifier (lower-case, e.g.
    /// <c>'volkswagen'</c>).</summary>
    function BrandKey: string; virtual; abstract;
    /// <summary>Display name shown in pickers.</summary>
    function DisplayName: string; virtual; abstract;
    /// <summary>One-line description of the input + output.</summary>
    function Description: string; virtual; abstract;

    /// <summary>Validates <c>AInput</c> through the vendor's
    /// rules.</summary>
    function Validate(const AInput: string;
      out AReason: string): Boolean;

    /// <summary>Sanitises + validates <c>Input</c>, calls
    /// <c>DoCalculate</c>, stores + returns the result. Hosts
    /// can also pass an explicit <c>AInput</c> to override
    /// <c>Input</c> for one shot.</summary>
    function Calculate: TOBDRadioCodeResult; overload;
    function Calculate(const AInput: string): TOBDRadioCodeResult; overload;

    /// <summary>Last result emitted by <c>Calculate</c>.</summary>
    property Result_: TOBDRadioCodeResult read FResult;
  published
    /// <summary>Serial number / pre-code / hex string the
    /// calculator consumes. Format depends on the vendor; check
    /// <c>Description</c>.</summary>
    property Input: string read FInput write FInput;
    /// <summary>Optional VIN. Vendors that switch algorithm by
    /// region / year read it from the context.</summary>
    property VIN: string read FVIN write FVIN;
    /// <summary>Optional radio-model hint
    /// (e.g. <c>'Concert'</c>).</summary>
    property ModelHint: string read FModelHint write FModelHint;
    /// <summary>Optional region override.</summary>
    property Region: TOBDRadioCodeRegion read FRegion write FRegion
      default rcrUnknown;
    /// <summary>Host-supplied calculation. Fired by the default
    /// <c>DoCalculate</c> for every vendor whose proprietary
    /// algorithm is not bundled. The handler fills <c>AResult</c>
    /// (set <c>Success</c>, <c>Code</c>, optionally
    /// <c>Variant</c>); leave <c>BrandKey</c> alone — the base
    /// fills it. Vendors with a built-in algorithm (database
    /// lookup, public spec) ignore this event.</summary>
    property OnCalculate: TOBDRadioCodeCalcEvent
      read FOnCalculate write FOnCalculate;
  end;

  /// <summary>Metaclass for runtime instantiation by
  /// <see cref="TOBDRadioCodeRegistry"/>.</summary>
  TOBDRadioCodeClass = class of TOBDRadioCode;

  /// <summary>Process-wide registry of calculator classes.
  /// Vendors register their class at unit initialization. Threaded
  /// reads after population.</summary>
  TOBDRadioCodeRegistry = class
  strict private
    class var FInstance: TOBDRadioCodeRegistry;
    FByBrandKey: TDictionary<string, TOBDRadioCodeClass>;
    FLock:       TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    class function Default: TOBDRadioCodeRegistry;
    class destructor ClassDestroy;

    /// <summary>Adds a calculator class. Looks up the brand key
    /// by instantiating the class once on register (cheap — no
    /// state populated yet). Duplicate brand keys silently
    /// overwrite.</summary>
    procedure RegisterClass(AClass: TOBDRadioCodeClass);
    /// <summary>Returns True + assigns out-param when a
    /// calculator class is registered for <c>ABrandKey</c>.</summary>
    function TryResolve(const ABrandKey: string;
      out AClass: TOBDRadioCodeClass): Boolean;
    /// <summary>Returns every registered class, sorted by
    /// display name. Each class is instantiated once for
    /// metadata then freed; hosts that need a usable instance
    /// call <see cref="TOBDRadioCodeClass.Create"/> on the
    /// returned class.</summary>
    function All: TArray<TOBDRadioCodeClass>;
    /// <summary>True when at least one class is registered for
    /// <c>ABrandKey</c>.</summary>
    function HasBrand(const ABrandKey: string): Boolean;
  end;

implementation

{ ---- TOBDRadioCode --------------------------------------------------------- }

constructor TOBDRadioCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegion := rcrUnknown;
end;

function TOBDRadioCode.SanitizeInput(const AInput: string): string;
begin
  Result := UpperCase(Trim(AInput));
end;

function TOBDRadioCode.ValidateLength(const AInput: string;
  AExpected: Integer; out AReason: string): Boolean;
begin
  AReason := '';
  Result := Length(AInput) = AExpected;
  if not Result then
    AReason := Format('Input must be exactly %d characters (got %d)',
      [AExpected, Length(AInput)]);
end;

function TOBDRadioCode.ValidateAllDigits(const AInput: string;
  out AReason: string): Boolean;
var
  C: Char;
begin
  AReason := '';
  for C in AInput do
    if not CharInSet(C, ['0'..'9']) then
    begin
      AReason := 'Input must contain only digits';
      Exit(False);
    end;
  Result := True;
end;

function TOBDRadioCode.ValidateAllAlpha(const AInput: string;
  out AReason: string): Boolean;
var
  C: Char;
begin
  AReason := '';
  for C in AInput do
    if not CharInSet(C, ['A'..'Z', 'a'..'z']) then
    begin
      AReason := 'Input must contain only letters';
      Exit(False);
    end;
  Result := True;
end;

function TOBDRadioCode.ValidateHex(const AInput: string;
  out AReason: string): Boolean;
var
  C: Char;
begin
  AReason := '';
  for C in AInput do
    if not CharInSet(C, ['0'..'9', 'A'..'F', 'a'..'f']) then
    begin
      AReason := 'Input must contain only hex digits';
      Exit(False);
    end;
  Result := True;
end;

function TOBDRadioCode.DoValidate(const AInput: string;
  out AReason: string): Boolean;
begin
  // Default: accept any non-empty string. Vendor calculators
  // override with their own length / format rules.
  AReason := '';
  Result := Trim(AInput) <> '';
  if not Result then
    AReason := 'Input is empty';
end;

function TOBDRadioCode.ValidatePrefix(const AInput, APrefix: string;
  out AReason: string): Boolean;
begin
  AReason := '';
  Result := Copy(AInput, 1, Length(APrefix)) = APrefix;
  if not Result then
    AReason := Format('Serial must start with %s', [APrefix]);
end;

function TOBDRadioCode.ValidateAlphanumericRange(const AInput: string;
  AStart, AEnd: Integer; out AReason: string): Boolean;
var
  I: Integer;
begin
  AReason := '';
  for I := AStart to AEnd do
    if not CharInSet(AInput[I], ['0'..'9', 'A'..'Z']) then
    begin
      AReason := Format('Character %d must be alphanumeric', [I]);
      Exit(False);
    end;
  Result := True;
end;

function TOBDRadioCode.ValidateDigitRange(const AInput: string;
  AStart, AEnd: Integer; out AReason: string): Boolean;
var
  I: Integer;
begin
  AReason := '';
  for I := AStart to AEnd do
    if not CharInSet(AInput[I], ['0'..'9']) then
    begin
      AReason := Format('Character %d must be a digit', [I]);
      Exit(False);
    end;
  Result := True;
end;

function TOBDRadioCode.Validate(const AInput: string;
  out AReason: string): Boolean;
begin
  Result := DoValidate(SanitizeInput(AInput), AReason);
end;

function TOBDRadioCode.DoCalculate(const AInput: string;
  const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult;
begin
  Result := Default(TOBDRadioCodeResult);
  Result.BrandKey := BrandKey;
  if Assigned(FOnCalculate) then
  begin
    FOnCalculate(Self, AInput, AContext, Result);
    if Result.BrandKey = '' then
      Result.BrandKey := BrandKey;
  end
  else
  begin
    Result.Success := False;
    Result.Message :=
      Format('Algorithm for "%s" is not bundled in this open-source ' +
             'distribution. Wire OnCalculate to supply your own ' +
             '(reverse-engineered, licensed, or network-service) ' +
             'implementation.',
             [DisplayName]);
  end;
end;

function TOBDRadioCode.Calculate: TOBDRadioCodeResult;
begin
  Result := Calculate(FInput);
end;

function TOBDRadioCode.Calculate(
  const AInput: string): TOBDRadioCodeResult;
var
  Sanitised: string;
  Reason:    string;
  Ctx:       TOBDRadioCodeContext;
begin
  FResult := Default(TOBDRadioCodeResult);
  FResult.BrandKey := BrandKey;
  Sanitised := SanitizeInput(AInput);
  if not DoValidate(Sanitised, Reason) then
  begin
    FResult.Message := Reason;
    Result := FResult;
    Exit;
  end;
  Ctx := Default(TOBDRadioCodeContext);
  Ctx.BrandKey       := BrandKey;
  Ctx.VIN            := FVIN;
  Ctx.ModelHint      := FModelHint;
  Ctx.RegionOverride := FRegion;
  FResult := DoCalculate(Sanitised, Ctx);
  // Defensive — vendors may forget to fill BrandKey.
  if FResult.BrandKey = '' then
    FResult.BrandKey := BrandKey;
  Result := FResult;
end;

{ ---- TOBDRadioCodeRegistry ------------------------------------------------- }

constructor TOBDRadioCodeRegistry.Create;
begin
  inherited Create;
  FByBrandKey := TDictionary<string, TOBDRadioCodeClass>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TOBDRadioCodeRegistry.Destroy;
begin
  FByBrandKey.Free;
  FLock.Free;
  inherited;
end;

class function TOBDRadioCodeRegistry.Default: TOBDRadioCodeRegistry;
begin
  if FInstance = nil then
    FInstance := TOBDRadioCodeRegistry.Create;
  Result := FInstance;
end;

class destructor TOBDRadioCodeRegistry.ClassDestroy;
begin
  FreeAndNil(FInstance);
end;

procedure TOBDRadioCodeRegistry.RegisterClass(
  AClass: TOBDRadioCodeClass);
var
  Probe: TOBDRadioCode;
  Key:   string;
begin
  if AClass = nil then
    raise EArgumentNilException.Create(
      'TOBDRadioCodeRegistry.RegisterClass: nil class');
  // One throw-away instance to read the brand key — no real
  // state populated yet so the cost is trivial.
  Probe := AClass.Create(nil);
  try
    Key := LowerCase(Probe.BrandKey);
  finally
    Probe.Free;
  end;
  FLock.Enter;
  try
    FByBrandKey.AddOrSetValue(Key, AClass);
  finally
    FLock.Leave;
  end;
end;

function TOBDRadioCodeRegistry.TryResolve(const ABrandKey: string;
  out AClass: TOBDRadioCodeClass): Boolean;
begin
  FLock.Enter;
  try
    Result := FByBrandKey.TryGetValue(LowerCase(ABrandKey), AClass);
  finally
    FLock.Leave;
  end;
end;

function TOBDRadioCodeRegistry.HasBrand(const ABrandKey: string): Boolean;
var
  Tmp: TOBDRadioCodeClass;
begin
  Result := TryResolve(ABrandKey, Tmp);
end;

function TOBDRadioCodeRegistry.All: TArray<TOBDRadioCodeClass>;
var
  Acc:   TList<TOBDRadioCodeClass>;
  Cls:   TOBDRadioCodeClass;
  Probe: TOBDRadioCode;
  Names: TDictionary<TOBDRadioCodeClass, string>;
begin
  Acc := TList<TOBDRadioCodeClass>.Create;
  Names := TDictionary<TOBDRadioCodeClass, string>.Create;
  try
    FLock.Enter;
    try
      for Cls in FByBrandKey.Values do
        Acc.Add(Cls);
    finally
      FLock.Leave;
    end;
    // Cache display names so the comparator doesn't allocate
    // throwaway instances on every comparison.
    for Cls in Acc do
    begin
      Probe := Cls.Create(nil);
      try
        Names.Add(Cls, Probe.DisplayName);
      finally
        Probe.Free;
      end;
    end;
    Acc.Sort(TComparer<TOBDRadioCodeClass>.Construct(
      function(const L, R: TOBDRadioCodeClass): Integer
      begin
        Result := CompareText(Names[L], Names[R]);
      end));
    Result := Acc.ToArray;
  finally
    Names.Free;
    Acc.Free;
  end;
end;

end.
