//------------------------------------------------------------------------------
//  OBD.Protocol.SecOC.AES
//
//  Pure-Pascal AES-128 single-block encryption. Sufficient for the
//  CMAC primitive used by SecOC (AUTOSAR Secure Onboard
//  Communication / ISO 21434). Encryption-only — CMAC never
//  decrypts, so we don't ship inverse round logic.
//
//  Implementation follows FIPS-197. Byte-oriented (no T-tables) for
//  clarity; SecOC PDUs are short so per-message latency is
//  irrelevant compared to the network round-trip.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - FIPS PUB 197 (Advanced Encryption Standard)
//    - NIST SP 800-38B (CMAC)
//
//  History     :
//    2026-05-09  ERD  Phase 4e initial.
//------------------------------------------------------------------------------

unit OBD.Protocol.SecOC.AES;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>AES block size in bytes.</summary>
  AES_BLOCK_SIZE = 16;
  /// <summary>AES-128 key size in bytes.</summary>
  AES_128_KEY_SIZE = 16;
  /// <summary>Number of AES-128 rounds.</summary>
  AES_128_ROUNDS = 10;

type
  /// <summary>Fixed 128-bit AES block.</summary>
  TAESBlock = array[0..AES_BLOCK_SIZE - 1] of Byte;
  /// <summary>Fixed 128-bit AES-128 key.</summary>
  TAES128Key = array[0..AES_128_KEY_SIZE - 1] of Byte;
  /// <summary>Expanded AES-128 round-key schedule
  /// (11 × 16 bytes).</summary>
  TAES128Schedule = array[0..(AES_128_ROUNDS + 1) * AES_BLOCK_SIZE - 1] of Byte;

/// <summary>
///   Expands a 128-bit key into the round-key schedule used by
///   <see cref="AES128EncryptBlock"/>.
/// </summary>
/// <param name="AKey">16-byte cipher key.</param>
/// <param name="ASchedule">Output schedule (176 bytes).</param>
procedure AES128KeyExpand(const AKey: TAES128Key;
  out ASchedule: TAES128Schedule);

/// <summary>
///   Encrypts a single 16-byte block under an expanded key.
/// </summary>
/// <param name="ASchedule">Round-key schedule from
/// <see cref="AES128KeyExpand"/>.</param>
/// <param name="AInput">Plaintext block.</param>
/// <param name="AOutput">Ciphertext block.</param>
procedure AES128EncryptBlock(const ASchedule: TAES128Schedule;
  const AInput: TAESBlock; out AOutput: TAESBlock);

/// <summary>
///   Convenience: expand the key and encrypt a single block.
///   Equivalent to <c>AES128KeyExpand</c> + <c>AES128EncryptBlock</c>.
/// </summary>
procedure AES128Encrypt(const AKey: TAES128Key;
  const AInput: TAESBlock; out AOutput: TAESBlock);

implementation

const
  // FIPS-197 §5.1.1, Table 4 — S-Box.
  SBox: array[0..255] of Byte = (
    $63, $7C, $77, $7B, $F2, $6B, $6F, $C5, $30, $01, $67, $2B, $FE, $D7, $AB, $76,
    $CA, $82, $C9, $7D, $FA, $59, $47, $F0, $AD, $D4, $A2, $AF, $9C, $A4, $72, $C0,
    $B7, $FD, $93, $26, $36, $3F, $F7, $CC, $34, $A5, $E5, $F1, $71, $D8, $31, $15,
    $04, $C7, $23, $C3, $18, $96, $05, $9A, $07, $12, $80, $E2, $EB, $27, $B2, $75,
    $09, $83, $2C, $1A, $1B, $6E, $5A, $A0, $52, $3B, $D6, $B3, $29, $E3, $2F, $84,
    $53, $D1, $00, $ED, $20, $FC, $B1, $5B, $6A, $CB, $BE, $39, $4A, $4C, $58, $CF,
    $D0, $EF, $AA, $FB, $43, $4D, $33, $85, $45, $F9, $02, $7F, $50, $3C, $9F, $A8,
    $51, $A3, $40, $8F, $92, $9D, $38, $F5, $BC, $B6, $DA, $21, $10, $FF, $F3, $D2,
    $CD, $0C, $13, $EC, $5F, $97, $44, $17, $C4, $A7, $7E, $3D, $64, $5D, $19, $73,
    $60, $81, $4F, $DC, $22, $2A, $90, $88, $46, $EE, $B8, $14, $DE, $5E, $0B, $DB,
    $E0, $32, $3A, $0A, $49, $06, $24, $5C, $C2, $D3, $AC, $62, $91, $95, $E4, $79,
    $E7, $C8, $37, $6D, $8D, $D5, $4E, $A9, $6C, $56, $F4, $EA, $65, $7A, $AE, $08,
    $BA, $78, $25, $2E, $1C, $A6, $B4, $C6, $E8, $DD, $74, $1F, $4B, $BD, $8B, $8A,
    $70, $3E, $B5, $66, $48, $03, $F6, $0E, $61, $35, $57, $B9, $86, $C1, $1D, $9E,
    $E1, $F8, $98, $11, $69, $D9, $8E, $94, $9B, $1E, $87, $E9, $CE, $55, $28, $DF,
    $8C, $A1, $89, $0D, $BF, $E6, $42, $68, $41, $99, $2D, $0F, $B0, $54, $BB, $16
  );

  // Round constants. Only the first 10 entries are needed for AES-128.
  Rcon: array[0..10] of Byte =
    ($00, $01, $02, $04, $08, $10, $20, $40, $80, $1B, $36);

function XTime(B: Byte): Byte; inline;
begin
  if (B and $80) <> 0 then
    Result := Byte((B shl 1) xor $1B)
  else
    Result := Byte(B shl 1);
end;

procedure AES128KeyExpand(const AKey: TAES128Key;
  out ASchedule: TAES128Schedule);
var
  I: Integer;
  Temp: array[0..3] of Byte;
  T: Byte;
begin
  // First round key = the cipher key.
  for I := 0 to AES_128_KEY_SIZE - 1 do
    ASchedule[I] := AKey[I];

  I := AES_128_KEY_SIZE;
  while I < SizeOf(TAES128Schedule) do
  begin
    Temp[0] := ASchedule[I - 4];
    Temp[1] := ASchedule[I - 3];
    Temp[2] := ASchedule[I - 2];
    Temp[3] := ASchedule[I - 1];

    if (I mod AES_128_KEY_SIZE) = 0 then
    begin
      // RotWord
      T       := Temp[0];
      Temp[0] := Temp[1];
      Temp[1] := Temp[2];
      Temp[2] := Temp[3];
      Temp[3] := T;
      // SubWord
      Temp[0] := SBox[Temp[0]];
      Temp[1] := SBox[Temp[1]];
      Temp[2] := SBox[Temp[2]];
      Temp[3] := SBox[Temp[3]];
      // Round constant XOR
      Temp[0] := Temp[0] xor Rcon[I div AES_128_KEY_SIZE];
    end;

    ASchedule[I    ] := ASchedule[I - AES_128_KEY_SIZE    ] xor Temp[0];
    ASchedule[I + 1] := ASchedule[I - AES_128_KEY_SIZE + 1] xor Temp[1];
    ASchedule[I + 2] := ASchedule[I - AES_128_KEY_SIZE + 2] xor Temp[2];
    ASchedule[I + 3] := ASchedule[I - AES_128_KEY_SIZE + 3] xor Temp[3];
    Inc(I, 4);
  end;
end;

procedure AddRoundKey(var AState: TAESBlock;
  const ASchedule: TAES128Schedule; ARound: Integer); inline;
var
  I, Off: Integer;
begin
  Off := ARound * AES_BLOCK_SIZE;
  for I := 0 to AES_BLOCK_SIZE - 1 do
    AState[I] := AState[I] xor ASchedule[Off + I];
end;

procedure SubBytes(var AState: TAESBlock); inline;
var
  I: Integer;
begin
  for I := 0 to AES_BLOCK_SIZE - 1 do
    AState[I] := SBox[AState[I]];
end;

procedure ShiftRows(var AState: TAESBlock); inline;
var
  T: Byte;
begin
  // Row 1: shift left by 1
  T := AState[1];
  AState[1] := AState[5]; AState[5] := AState[9];
  AState[9] := AState[13]; AState[13] := T;
  // Row 2: shift left by 2
  T := AState[2]; AState[2] := AState[10]; AState[10] := T;
  T := AState[6]; AState[6] := AState[14]; AState[14] := T;
  // Row 3: shift left by 3
  T := AState[3];
  AState[3] := AState[15]; AState[15] := AState[11];
  AState[11] := AState[7]; AState[7] := T;
end;

procedure MixColumns(var AState: TAESBlock); inline;
var
  C: Integer;
  S0, S1, S2, S3, T: Byte;
begin
  for C := 0 to 3 do
  begin
    S0 := AState[C * 4    ];
    S1 := AState[C * 4 + 1];
    S2 := AState[C * 4 + 2];
    S3 := AState[C * 4 + 3];
    T  := S0 xor S1 xor S2 xor S3;
    AState[C * 4    ] := S0 xor T xor XTime(S0 xor S1);
    AState[C * 4 + 1] := S1 xor T xor XTime(S1 xor S2);
    AState[C * 4 + 2] := S2 xor T xor XTime(S2 xor S3);
    AState[C * 4 + 3] := S3 xor T xor XTime(S3 xor S0);
  end;
end;

procedure AES128EncryptBlock(const ASchedule: TAES128Schedule;
  const AInput: TAESBlock; out AOutput: TAESBlock);
var
  State: TAESBlock;
  Round: Integer;
begin
  State := AInput;
  AddRoundKey(State, ASchedule, 0);
  for Round := 1 to AES_128_ROUNDS - 1 do
  begin
    SubBytes(State);
    ShiftRows(State);
    MixColumns(State);
    AddRoundKey(State, ASchedule, Round);
  end;
  // Final round (no MixColumns).
  SubBytes(State);
  ShiftRows(State);
  AddRoundKey(State, ASchedule, AES_128_ROUNDS);
  AOutput := State;
end;

procedure AES128Encrypt(const AKey: TAES128Key;
  const AInput: TAESBlock; out AOutput: TAESBlock);
var
  Schedule: TAES128Schedule;
begin
  AES128KeyExpand(AKey, Schedule);
  AES128EncryptBlock(Schedule, AInput, AOutput);
end;

end.
