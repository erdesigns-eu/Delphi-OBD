//------------------------------------------------------------------------------
// UNIT           : OBD.Security.Nonce.pas
// CONTENTS       : Anti-replay nonce primitive on top of UDS SecurityAccess
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : UDS SecurityAccess (SID $27) is challenge-response,
//                  but the protocol itself doesn't enforce
//                  freshness — a captured request/response pair can
//                  be replayed if the seed-key algorithm is
//                  deterministic. This primitive issues a
//                  cryptographically random nonce and tracks
//                  consumption so a replayed nonce is rejected.
//
//                  Pair with a verifier that mixes the nonce into
//                  the signed payload (e.g. <c>HMAC(K, nonce || cmd)</c>)
//                  to get an end-to-end anti-replay channel.
//------------------------------------------------------------------------------
unit OBD.Security.Nonce;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.DateUtils,
  System.Generics.Collections, System.Hash;

type
  EOBDNonceError = class(Exception);
  EOBDNonceReplay = class(EOBDNonceError);
  EOBDNonceExpired = class(EOBDNonceError);
  EOBDNonceUnknown = class(EOBDNonceError);

  /// <summary>
  ///   Issue + validate single-use nonces. Construct once per security
  ///   session; each nonce is valid for <c>TtlSeconds</c> and may be
  ///   redeemed exactly once.
  /// </summary>
  TOBDNonceVault = class
  strict private
    FLock: TCriticalSection;
    FActive: TDictionary<string, TDateTime>;
    FUsed: TDictionary<string, TDateTime>;
    FTtlSeconds: Integer;
    FNonceLength: Integer;
    function GenerateNonceHex: string;
    procedure SweepExpired;
  public
    constructor Create(ATtlSeconds: Integer = 30; ANonceLength: Integer = 16);
    destructor Destroy; override;

    /// <summary>
    ///   Mint a fresh nonce. Returns the hex-encoded value caller must
    ///   transmit and present back via <c>Redeem</c>.
    /// </summary>
    function Issue: string;

    /// <summary>
    ///   Consume a nonce. Raises <c>EOBDNonceUnknown</c> if not issued
    ///   (or already swept), <c>EOBDNonceExpired</c> if the TTL has
    ///   lapsed, or <c>EOBDNonceReplay</c> if it's already been
    ///   redeemed once.
    /// </summary>
    procedure Redeem(const Nonce: string);

    /// <summary>True if <c>Nonce</c> is currently valid + unredeemed.</summary>
    function IsValid(const Nonce: string): Boolean;

    /// <summary>Drop every issued and used nonce (e.g. on session reset).</summary>
    procedure Reset;

    /// <summary>Number of currently-valid nonces.</summary>
    function PendingCount: Integer;

    property TtlSeconds: Integer read FTtlSeconds write FTtlSeconds;
    /// <summary>Nonce byte length before hex encoding (default 16 → 32 hex chars).</summary>
    property NonceLength: Integer read FNonceLength write FNonceLength;
  end;

implementation

uses
  WinApi.Windows;

// Crypto-quality random bytes via Windows RtlGenRandom (advapi32!SystemFunction036).
function SystemFunction036(RandomBuffer: Pointer; RandomBufferLength: ULONG): BOOL;
  stdcall; external 'advapi32.dll' name 'SystemFunction036';

procedure FillSecureRandom(var Buffer: TBytes);
begin
  if Length(Buffer) = 0 then Exit;
  if not SystemFunction036(@Buffer[0], Length(Buffer)) then
    raise EOBDNonceError.CreateFmt(
      'RtlGenRandom failed (SystemFunction036), GetLastError=%d',
      [GetLastError]);
end;

function BytesToHex(const Bytes: TBytes): string;
const
  HexDigits: array[0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create(Length(Bytes) * 2);
  try
    for I := 0 to High(Bytes) do
    begin
      Builder.Append(HexDigits[Bytes[I] shr 4]);
      Builder.Append(HexDigits[Bytes[I] and $0F]);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

//==============================================================================
// TOBDNonceVault
//==============================================================================
constructor TOBDNonceVault.Create(ATtlSeconds, ANonceLength: Integer);
begin
  inherited Create;
  if ATtlSeconds <= 0 then
    raise EArgumentException.Create('TtlSeconds must be > 0');
  if ANonceLength < 8 then
    raise EArgumentException.Create('NonceLength must be >= 8 bytes');
  FLock := TCriticalSection.Create;
  FActive := TDictionary<string, TDateTime>.Create;
  FUsed := TDictionary<string, TDateTime>.Create;
  FTtlSeconds := ATtlSeconds;
  FNonceLength := ANonceLength;
end;

destructor TOBDNonceVault.Destroy;
begin
  FActive.Free;
  FUsed.Free;
  FLock.Free;
  inherited;
end;

function TOBDNonceVault.GenerateNonceHex: string;
var
  Buffer: TBytes;
begin
  SetLength(Buffer, FNonceLength);
  FillSecureRandom(Buffer);
  Result := BytesToHex(Buffer);
end;

procedure TOBDNonceVault.SweepExpired;
var
  Now_: TDateTime;
  ToDrop: TArray<string>;
  Key: string;
  Issued: TDateTime;
begin
  Now_ := Now;
  // Drop active entries whose TTL has elapsed; sweep the redeemed-set
  // 10× the TTL so a late replay attempt still hits the right exception.
  for Key in FActive.Keys do
  begin
    if FActive.TryGetValue(Key, Issued) and
       (SecondsBetween(Now_, Issued) > FTtlSeconds) then
      ToDrop := ToDrop + [Key];
  end;
  for Key in ToDrop do FActive.Remove(Key);

  ToDrop := nil;
  for Key in FUsed.Keys do
  begin
    if FUsed.TryGetValue(Key, Issued) and
       (SecondsBetween(Now_, Issued) > FTtlSeconds * 10) then
      ToDrop := ToDrop + [Key];
  end;
  for Key in ToDrop do FUsed.Remove(Key);
end;

function TOBDNonceVault.Issue: string;
begin
  FLock.Enter;
  try
    SweepExpired;
    Result := GenerateNonceHex;
    FActive.AddOrSetValue(Result, Now);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDNonceVault.Redeem(const Nonce: string);
var
  Issued: TDateTime;
begin
  FLock.Enter;
  try
    SweepExpired;
    if FUsed.ContainsKey(Nonce) then
      raise EOBDNonceReplay.Create('Nonce already redeemed');
    if not FActive.TryGetValue(Nonce, Issued) then
      raise EOBDNonceUnknown.Create('Nonce was never issued or has been swept');
    if SecondsBetween(Now, Issued) > FTtlSeconds then
    begin
      FActive.Remove(Nonce);
      raise EOBDNonceExpired.Create('Nonce TTL has lapsed');
    end;
    FActive.Remove(Nonce);
    FUsed.AddOrSetValue(Nonce, Now);
  finally
    FLock.Leave;
  end;
end;

function TOBDNonceVault.IsValid(const Nonce: string): Boolean;
var
  Issued: TDateTime;
begin
  FLock.Enter;
  try
    SweepExpired;
    Result := FActive.TryGetValue(Nonce, Issued) and
              (SecondsBetween(Now, Issued) <= FTtlSeconds);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDNonceVault.Reset;
begin
  FLock.Enter;
  try
    FActive.Clear;
    FUsed.Clear;
  finally
    FLock.Leave;
  end;
end;

function TOBDNonceVault.PendingCount: Integer;
begin
  FLock.Enter;
  try
    SweepExpired;
    Result := FActive.Count;
  finally
    FLock.Leave;
  end;
end;

end.
