//------------------------------------------------------------------------------
//  OBD.Security.Nonce
//
//  Cryptographically-strong nonce / random-byte helpers. Wraps
//  Win32 BCryptGenRandom on Windows and falls back to a
//  CSPRNG-grade source on POSIX. Useful for SecOC freshness
//  counters, seed-key replay protection, audit-log salts and any
//  other place that needs unpredictable bytes.
//
//  Hosts should never call <c>Random</c> from the RTL for
//  security-sensitive code — it seeds from <c>RandSeed</c> which
//  is predictable across runs without explicit seeding.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Security.Nonce.
//------------------------------------------------------------------------------

unit OBD.Security.Nonce;

interface

uses
  System.SysUtils,
  System.Classes;

type
  /// <summary>Raised when the underlying CSPRNG fails.</summary>
  EOBDNonce = class(Exception);

/// <summary>
///   Returns <c>ALength</c> bytes drawn from a CSPRNG.
/// </summary>
/// <param name="ALength">Byte count (must be &gt; 0).</param>
/// <returns>Random bytes.</returns>
/// <exception cref="EOBDNonce">
///   <c>ALength</c> is non-positive or the OS CSPRNG returned
///   an error.
/// </exception>
function NonceBytes(ALength: Integer): TBytes;

/// <summary>Generates a fresh 32-bit nonce.</summary>
/// <returns>Random 32-bit value.</returns>
function Nonce32: UInt32;

/// <summary>Generates a fresh 64-bit nonce.</summary>
/// <returns>Random 64-bit value.</returns>
function Nonce64: UInt64;

/// <summary>
///   Hex-encodes <c>ABytes</c> as a lowercase string. Convenient
///   for stamping nonces into log lines or audit blobs.
/// </summary>
/// <param name="ABytes">Bytes to encode.</param>
/// <returns>Lowercase hex string (no <c>0x</c> prefix).</returns>
function HexEncode(const ABytes: TBytes): string;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;

const
  BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;

function BCryptGenRandom(hAlgorithm: THandle; pbBuffer: PByte;
  cbBuffer: ULONG; dwFlags: ULONG): LongInt; stdcall;
  external 'Bcrypt.dll' name 'BCryptGenRandom';

function CSPRNG(ALength: Integer): TBytes;
var
  Status: LongInt;
begin
  SetLength(Result, ALength);
  Status := BCryptGenRandom(0, PByte(Result), ALength,
    BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if Status <> 0 then
    raise EOBDNonce.CreateFmt(
      'BCryptGenRandom failed: 0x%.8x', [Status]);
end;
{$ELSE}
function CSPRNG(ALength: Integer): TBytes;
var
  Stream: TFileStream;
begin
  SetLength(Result, ALength);
  Stream := TFileStream.Create('/dev/urandom', fmOpenRead);
  try
    Stream.ReadBuffer(Result[0], ALength);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

function NonceBytes(ALength: Integer): TBytes;
begin
  if ALength <= 0 then
    raise EOBDNonce.CreateFmt(
      'NonceBytes: bad length %d', [ALength]);
  Result := CSPRNG(ALength);
end;

function Nonce32: UInt32;
var
  B: TBytes;
begin
  B := NonceBytes(4);
  Result := (UInt32(B[0]) shl 24) or
            (UInt32(B[1]) shl 16) or
            (UInt32(B[2]) shl  8) or
             UInt32(B[3]);
end;

function Nonce64: UInt64;
var
  B: TBytes;
  Lo, Hi: UInt32;
begin
  B := NonceBytes(8);
  Hi := (UInt32(B[0]) shl 24) or (UInt32(B[1]) shl 16) or
        (UInt32(B[2]) shl  8) or  UInt32(B[3]);
  Lo := (UInt32(B[4]) shl 24) or (UInt32(B[5]) shl 16) or
        (UInt32(B[6]) shl  8) or  UInt32(B[7]);
  Result := (UInt64(Hi) shl 32) or UInt64(Lo);
end;

function HexEncode(const ABytes: TBytes): string;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes) * 2);
  for I := 0 to High(ABytes) do
  begin
    Result[1 + 2 * I]     := Digits[(ABytes[I] shr 4) and $0F];
    Result[1 + 2 * I + 1] := Digits[ ABytes[I]        and $0F];
  end;
end;

end.
