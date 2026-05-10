//------------------------------------------------------------------------------
//  OBD.Protocol.SecOC.CMAC
//
//  CMAC-AES128 per RFC 4493 / NIST SP 800-38B. Used by SecOC to
//  authenticate Original PDU || Data ID || Full Freshness Value.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - RFC 4493 (The AES-CMAC Algorithm)
//    - NIST SP 800-38B
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.SecOC.CMAC;

interface

uses
  System.SysUtils,
  OBD.Types,
  OBD.Protocol.SecOC.AES;

type
  /// <summary>Stateless AES-CMAC primitive.</summary>
  TOBDCMACAES = class
  public
    /// <summary>
    ///   Computes the full 128-bit CMAC tag of <c>AMessage</c>
    ///   under <c>AKey</c>.
    /// </summary>
    /// <param name="AKey">128-bit AES key.</param>
    /// <param name="AMessage">Bytes to authenticate. May be empty.</param>
    /// <returns>16-byte tag.</returns>
    class function Compute(const AKey: TAES128Key;
      const AMessage: TBytes): TAESBlock; static;

    /// <summary>
    ///   Computes a truncated CMAC tag (<c>ATagBits</c> bits, big-
    ///   endian, taken from the most-significant end of the full
    ///   tag).
    /// </summary>
    /// <param name="AKey">128-bit AES key.</param>
    /// <param name="AMessage">Bytes to authenticate. May be empty.</param>
    /// <param name="ATagBits">Tag length in bits — must be in
    /// <c>[1..128]</c>. SecOC commonly uses 24, 28, 56, 64.</param>
    /// <returns>Truncated tag as a byte array of
    /// <c>ceil(ATagBits/8)</c> bytes; the unused low bits of the
    /// last byte are zero-padded.</returns>
    /// <exception cref="EOBDConfig"><c>ATagBits</c> out of range.</exception>
    class function ComputeTruncated(const AKey: TAES128Key;
      const AMessage: TBytes; ATagBits: Integer): TBytes; static;
  end;

implementation

procedure XorBlock(var ADest: TAESBlock; const ASrc: TAESBlock); inline;
var
  I: Integer;
begin
  for I := 0 to AES_BLOCK_SIZE - 1 do
    ADest[I] := ADest[I] xor ASrc[I];
end;

procedure ShiftLeft1(const AIn: TAESBlock; out AOut: TAESBlock); inline;
var
  I: Integer;
  Carry: Byte;
begin
  Carry := 0;
  for I := AES_BLOCK_SIZE - 1 downto 0 do
  begin
    AOut[I] := Byte((AIn[I] shl 1) or Carry);
    Carry := (AIn[I] shr 7) and $01;
  end;
end;

procedure GenerateSubkeys(const ASchedule: TAES128Schedule;
  out AK1, AK2: TAESBlock);
const
  Rb: Byte = $87;
var
  L, Zero: TAESBlock;
begin
  FillChar(Zero, SizeOf(Zero), 0);
  AES128EncryptBlock(ASchedule, Zero, L);

  ShiftLeft1(L, AK1);
  if (L[0] and $80) <> 0 then
    AK1[AES_BLOCK_SIZE - 1] := AK1[AES_BLOCK_SIZE - 1] xor Rb;

  ShiftLeft1(AK1, AK2);
  if (AK1[0] and $80) <> 0 then
    AK2[AES_BLOCK_SIZE - 1] := AK2[AES_BLOCK_SIZE - 1] xor Rb;
end;

class function TOBDCMACAES.Compute(const AKey: TAES128Key;
  const AMessage: TBytes): TAESBlock;
var
  Schedule: TAES128Schedule;
  K1, K2, X, Y, M: TAESBlock;
  N, I, MsgLen, BlockOffset, Remainder: Integer;
  LastIsComplete: Boolean;
begin
  AES128KeyExpand(AKey, Schedule);
  GenerateSubkeys(Schedule, K1, K2);

  MsgLen := Length(AMessage);
  if MsgLen = 0 then
  begin
    N := 1;
    LastIsComplete := False;
  end
  else
  begin
    N := (MsgLen + AES_BLOCK_SIZE - 1) div AES_BLOCK_SIZE;
    LastIsComplete := (MsgLen mod AES_BLOCK_SIZE) = 0;
  end;

  FillChar(X, SizeOf(X), 0);

  // Process all blocks except the last.
  for I := 0 to N - 2 do
  begin
    BlockOffset := I * AES_BLOCK_SIZE;
    Move(AMessage[BlockOffset], M[0], AES_BLOCK_SIZE);
    XorBlock(M, X);
    AES128EncryptBlock(Schedule, M, X);
  end;

  // Last block — pad if incomplete, XOR with K1 (complete) or K2 (padded).
  FillChar(M, SizeOf(M), 0);
  if LastIsComplete then
  begin
    BlockOffset := (N - 1) * AES_BLOCK_SIZE;
    Move(AMessage[BlockOffset], M[0], AES_BLOCK_SIZE);
    XorBlock(M, K1);
  end
  else
  begin
    BlockOffset := (N - 1) * AES_BLOCK_SIZE;
    Remainder := MsgLen - BlockOffset;
    if Remainder > 0 then
      Move(AMessage[BlockOffset], M[0], Remainder);
    M[Remainder] := $80; // 1-bit + zero pad
    XorBlock(M, K2);
  end;

  XorBlock(M, X);
  AES128EncryptBlock(Schedule, M, Y);
  Result := Y;
end;

class function TOBDCMACAES.ComputeTruncated(const AKey: TAES128Key;
  const AMessage: TBytes; ATagBits: Integer): TBytes;
var
  Full: TAESBlock;
  ByteCount, KeepBits, RemainingBits: Integer;
  Mask: Byte;
begin
  if (ATagBits < 1) or (ATagBits > 128) then
    raise EOBDConfig.CreateFmt('CMAC: tag bits %d out of range', [ATagBits]);
  Full := Compute(AKey, AMessage);

  ByteCount := (ATagBits + 7) div 8;
  SetLength(Result, ByteCount);
  Move(Full[0], Result[0], ByteCount);

  // Mask off bits in the final byte if the tag is not byte-aligned.
  RemainingBits := ATagBits mod 8;
  if RemainingBits <> 0 then
  begin
    KeepBits := RemainingBits;
    Mask := Byte($FF shl (8 - KeepBits));
    Result[ByteCount - 1] := Result[ByteCount - 1] and Mask;
  end;
end;

end.
