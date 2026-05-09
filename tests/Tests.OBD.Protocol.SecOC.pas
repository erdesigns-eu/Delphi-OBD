//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.SecOC
//
//  Coverage for the SecOC stack:
//
//    - AES-128 single-block encryption against the FIPS-197 known
//      answer (Appendix B).
//    - CMAC-AES128 against every test vector in RFC 4493 §4.
//    - Key store register / lookup / unregister / clear / replace.
//    - Freshness counter monotonicity, truncation reconstruction,
//      replay rejection, jump-window rejection.
//    - SecOC wrap / unwrap round-trip; tampering detection;
//      replay rejection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4e initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.SecOC;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.SecOC.AES,
  OBD.Protocol.SecOC.CMAC,
  OBD.Protocol.SecOC.Keys,
  OBD.Protocol.SecOC.Freshness,
  OBD.Protocol.SecOC;

type
  /// <summary>AES-128 known-answer coverage.</summary>
  [TestFixture]
  TAESTests = class
  public
    [Test] procedure FIPS197AppendixBKnownAnswer;
    [Test] procedure RoundTripExpandedKey;
    /// <summary>Verifies the constant-time S-box returns FIPS-197
    /// Table 4 values for every one of the 256 possible inputs.</summary>
    [Test] procedure ConstantTimeSBoxMatchesFIPS197;
  end;

  /// <summary>RFC 4493 CMAC test vectors.</summary>
  [TestFixture]
  TCMACTests = class
  public
    [Test] procedure RFC4493_EmptyMessage;
    [Test] procedure RFC4493_16ByteMessage;
    [Test] procedure RFC4493_40ByteMessage;
    [Test] procedure RFC4493_64ByteMessage;
    [Test] procedure TruncatedTagMatchesPrefix;
  end;

  /// <summary>Key store coverage.</summary>
  [TestFixture]
  TKeyStoreTests = class
  public
    [Test] procedure RegisterAndLookup;
    [Test] procedure UnregisterRemovesBinding;
    [Test] procedure ClearRemovesAllBindings;
    [Test] procedure RegisterRejectsOutOfRange;
    [Test] procedure ReplacesExistingBinding;
  end;

  /// <summary>Freshness manager coverage.</summary>
  [TestFixture]
  TFreshnessTests = class
  public
    [Test] procedure NextTxIsMonotonic;
    [Test] procedure TryAcceptInitialMessage;
    [Test] procedure TryAcceptHandlesWrap;
    [Test] procedure TryAcceptRejectsReplay;
    [Test] procedure TryAcceptRejectsLargeJump;
  end;

  /// <summary>Wrap / unwrap end-to-end coverage.</summary>
  [TestFixture]
  TSecOCCodecTests = class
  public
    [Test] procedure WrapUnwrapRoundTrip;
    [Test] procedure UnwrapDetectsBitFlipInPDU;
    [Test] procedure UnwrapDetectsBitFlipInMAC;
    [Test] procedure UnwrapRejectsReplay;
    [Test] procedure UnwrapRaisesOnUnknownDataID;
    [Test] procedure WrapRaisesWithoutKeys;
  end;

implementation

// ---- helpers ----------------------------------------------------------------

function HexToBytesLocal(const AHex: string): TBytes;
var
  S: string;
  I: Integer;
begin
  S := StringReplace(AHex, ' ', '', [rfReplaceAll]);
  S := StringReplace(S, '-', '', [rfReplaceAll]);
  SetLength(Result, Length(S) div 2);
  for I := 0 to Length(Result) - 1 do
    Result[I] := StrToInt('$' + Copy(S, I * 2 + 1, 2));
end;

function BlockToHex(const ABlock: TAESBlock): string;
var
  I: Integer;
  Tmp: TBytes;
begin
  SetLength(Tmp, AES_BLOCK_SIZE);
  for I := 0 to AES_BLOCK_SIZE - 1 do
    Tmp[I] := ABlock[I];
  Result := '';
  for I := 0 to High(Tmp) do
    Result := Result + IntToHex(Tmp[I], 2);
end;

function BytesToHex(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
    Result := Result + IntToHex(ABytes[I], 2);
end;

procedure SetKey(out AKey: TAES128Key; const AHex: string);
var
  Bytes: TBytes;
  I: Integer;
begin
  Bytes := HexToBytesLocal(AHex);
  Assert.AreEqual(AES_128_KEY_SIZE, Length(Bytes));
  for I := 0 to AES_128_KEY_SIZE - 1 do
    AKey[I] := Bytes[I];
end;

procedure SetBlock(out ABlock: TAESBlock; const AHex: string);
var
  Bytes: TBytes;
  I: Integer;
begin
  Bytes := HexToBytesLocal(AHex);
  Assert.AreEqual(AES_BLOCK_SIZE, Length(Bytes));
  for I := 0 to AES_BLOCK_SIZE - 1 do
    ABlock[I] := Bytes[I];
end;

// ---- AES tests --------------------------------------------------------------

procedure TAESTests.FIPS197AppendixBKnownAnswer;
var
  Key: TAES128Key;
  Plain, Cipher: TAESBlock;
begin
  // FIPS-197 Appendix B
  SetKey(Key,    '2b7e151628aed2a6abf7158809cf4f3c');
  SetBlock(Plain,'3243f6a8885a308d313198a2e0370734');
  AES128Encrypt(Key, Plain, Cipher);
  Assert.AreEqual(
    UpperCase('3925841d02dc09fbdc118597196a0b32'),
    BlockToHex(Cipher));
end;

procedure TAESTests.ConstantTimeSBoxMatchesFIPS197;
const
  // FIPS-197 §5.1.1, Table 4 — verbatim, kept independent of the
  // table inside the unit so a regression in either side surfaces.
  ExpectedSBox: array[0..255] of Byte = (
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
var
  I: Integer;
begin
  for I := 0 to 255 do
    Assert.AreEqual(
      Integer(ExpectedSBox[I]),
      Integer(AESConstantTimeSBox(Byte(I))),
      Format('SBox(0x%2.2X) mismatch', [I]));
end;

procedure TAESTests.RoundTripExpandedKey;
var
  Key: TAES128Key;
  Schedule: TAES128Schedule;
  Plain, A, B: TAESBlock;
begin
  SetKey(Key,    '2b7e151628aed2a6abf7158809cf4f3c');
  SetBlock(Plain,'6bc1bee22e409f96e93d7e117393172a');
  AES128Encrypt(Key, Plain, A);
  AES128KeyExpand(Key, Schedule);
  AES128EncryptBlock(Schedule, Plain, B);
  Assert.AreEqual(BlockToHex(A), BlockToHex(B));
end;

// ---- CMAC tests (RFC 4493 §4) ----------------------------------------------

const
  RFC4493_KEY = '2b7e151628aed2a6abf7158809cf4f3c';
  RFC4493_M   = '6bc1bee22e409f96e93d7e117393172a' +
                'ae2d8a571e03ac9c9eb76fac45af8e51' +
                '30c81c46a35ce411e5fbc1191a0a52ef' +
                'f69f2445df4f9b17ad2b417be66c3810';

procedure TCMACTests.RFC4493_EmptyMessage;
var
  Key: TAES128Key;
  Tag: TAESBlock;
begin
  SetKey(Key, RFC4493_KEY);
  Tag := TOBDCMACAES.Compute(Key, nil);
  Assert.AreEqual(
    UpperCase('bb1d6929e95937287fa37d129b756746'),
    BlockToHex(Tag));
end;

procedure TCMACTests.RFC4493_16ByteMessage;
var
  Key: TAES128Key;
  Msg: TBytes;
  Tag: TAESBlock;
begin
  SetKey(Key, RFC4493_KEY);
  Msg := HexToBytesLocal(Copy(RFC4493_M, 1, 32));
  Tag := TOBDCMACAES.Compute(Key, Msg);
  Assert.AreEqual(
    UpperCase('070a16b46b4d4144f79bdd9dd04a287c'),
    BlockToHex(Tag));
end;

procedure TCMACTests.RFC4493_40ByteMessage;
var
  Key: TAES128Key;
  Msg: TBytes;
  Tag: TAESBlock;
begin
  SetKey(Key, RFC4493_KEY);
  Msg := HexToBytesLocal(Copy(RFC4493_M, 1, 80));
  Tag := TOBDCMACAES.Compute(Key, Msg);
  Assert.AreEqual(
    UpperCase('dfa66747de9ae63030ca32611497c827'),
    BlockToHex(Tag));
end;

procedure TCMACTests.RFC4493_64ByteMessage;
var
  Key: TAES128Key;
  Msg: TBytes;
  Tag: TAESBlock;
begin
  SetKey(Key, RFC4493_KEY);
  Msg := HexToBytesLocal(RFC4493_M);
  Tag := TOBDCMACAES.Compute(Key, Msg);
  Assert.AreEqual(
    UpperCase('51f0bebf7e3b9d92fc49741779363cfe'),
    BlockToHex(Tag));
end;

procedure TCMACTests.TruncatedTagMatchesPrefix;
var
  Key: TAES128Key;
  Msg: TBytes;
  Trunc: TBytes;
  Full: TAESBlock;
  I: Integer;
begin
  SetKey(Key, RFC4493_KEY);
  Msg := HexToBytesLocal(RFC4493_M);
  Full := TOBDCMACAES.Compute(Key, Msg);
  Trunc := TOBDCMACAES.ComputeTruncated(Key, Msg, 64);
  Assert.AreEqual(8, Length(Trunc));
  for I := 0 to 7 do
    Assert.AreEqual(Integer(Full[I]), Integer(Trunc[I]));
end;

// ---- key store tests --------------------------------------------------------

procedure TKeyStoreTests.RegisterAndLookup;
var
  Store: TOBDSecOCKeyStore;
  Key: TAES128Key;
  Out_: TOBDSecOCBinding;
begin
  Store := TOBDSecOCKeyStore.Create;
  try
    SetKey(Key, RFC4493_KEY);
    Store.RegisterKey($1234, Key, 64, 16);
    Assert.IsTrue(Store.TryGet($1234, Out_));
    Assert.AreEqual(64, Integer(Out_.TagBits));
    Assert.AreEqual(16, Integer(Out_.FreshnessBits));
  finally
    Store.Free;
  end;
end;

procedure TKeyStoreTests.UnregisterRemovesBinding;
var
  Store: TOBDSecOCKeyStore;
  Key: TAES128Key;
  Dummy: TOBDSecOCBinding;
begin
  Store := TOBDSecOCKeyStore.Create;
  try
    SetKey(Key, RFC4493_KEY);
    Store.RegisterKey($1234, Key);
    Store.Unregister($1234);
    Assert.IsFalse(Store.TryGet($1234, Dummy));
  finally
    Store.Free;
  end;
end;

procedure TKeyStoreTests.ClearRemovesAllBindings;
var
  Store: TOBDSecOCKeyStore;
  Key: TAES128Key;
begin
  Store := TOBDSecOCKeyStore.Create;
  try
    SetKey(Key, RFC4493_KEY);
    Store.RegisterKey($0001, Key);
    Store.RegisterKey($0002, Key);
    Assert.AreEqual(2, Store.Count);
    Store.Clear;
    Assert.AreEqual(0, Store.Count);
  finally
    Store.Free;
  end;
end;

procedure TKeyStoreTests.RegisterRejectsOutOfRange;
var
  Store: TOBDSecOCKeyStore;
  Key: TAES128Key;
begin
  Store := TOBDSecOCKeyStore.Create;
  try
    SetKey(Key, RFC4493_KEY);
    Assert.WillRaise(procedure begin Store.RegisterKey($1, Key, 4, 16); end,
      EOBDConfig);
    Assert.WillRaise(procedure begin Store.RegisterKey($1, Key, 64, 0); end,
      EOBDConfig);
    Assert.WillRaise(procedure begin Store.RegisterKey($1, Key, 64, 64); end,
      EOBDConfig);
    // Non-byte-aligned truncation is rejected eagerly.
    Assert.WillRaise(procedure begin Store.RegisterKey($1, Key, 28, 16); end,
      EOBDConfig);
    Assert.WillRaise(procedure begin Store.RegisterKey($1, Key, 64, 12); end,
      EOBDConfig);
  finally
    Store.Free;
  end;
end;

procedure TKeyStoreTests.ReplacesExistingBinding;
var
  Store: TOBDSecOCKeyStore;
  K1, K2: TAES128Key;
  Out_: TOBDSecOCBinding;
begin
  Store := TOBDSecOCKeyStore.Create;
  try
    SetKey(K1, RFC4493_KEY);
    SetKey(K2, '00112233445566778899aabbccddeeff');
    Store.RegisterKey($1234, K1, 64, 16);
    Store.RegisterKey($1234, K2, 32, 8);
    Assert.IsTrue(Store.TryGet($1234, Out_));
    Assert.AreEqual(32, Integer(Out_.TagBits));
    Assert.AreEqual(8,  Integer(Out_.FreshnessBits));
    Assert.AreEqual(Integer(K2[0]), Integer(Out_.Key[0]));
  finally
    Store.Free;
  end;
end;

// ---- freshness tests --------------------------------------------------------

procedure TFreshnessTests.NextTxIsMonotonic;
var
  F: TOBDSecOCFreshness;
  A, B, C: UInt64;
begin
  F := TOBDSecOCFreshness.Create;
  try
    A := F.NextTx($1);
    B := F.NextTx($1);
    C := F.NextTx($1);
    Assert.AreEqual(UInt64(1), A);
    Assert.AreEqual(UInt64(2), B);
    Assert.AreEqual(UInt64(3), C);
  finally
    F.Free;
  end;
end;

procedure TFreshnessTests.TryAcceptInitialMessage;
var
  F: TOBDSecOCFreshness;
  Out_: UInt64;
begin
  F := TOBDSecOCFreshness.Create;
  try
    Assert.IsTrue(F.TryAccept($1, 1, 16, Out_));
    Assert.AreEqual(UInt64(1), Out_);
  finally
    F.Free;
  end;
end;

procedure TFreshnessTests.TryAcceptHandlesWrap;
var
  F: TOBDSecOCFreshness;
  Out_: UInt64;
begin
  F := TOBDSecOCFreshness.Create;
  try
    F.MaxJump := 10000;
    F.SeedRx($1, $0FF);     // RX last accepted = 255
    Assert.IsTrue(F.TryAccept($1, $001, 8, Out_));
    Assert.AreEqual(UInt64($101), Out_);
  finally
    F.Free;
  end;
end;

procedure TFreshnessTests.TryAcceptRejectsReplay;
var
  F: TOBDSecOCFreshness;
  Out_: UInt64;
begin
  F := TOBDSecOCFreshness.Create;
  try
    F.SeedRx($1, 5);
    Assert.IsFalse(F.TryAccept($1, 5, 16, Out_));
    Assert.IsFalse(F.TryAccept($1, 4, 16, Out_));
  finally
    F.Free;
  end;
end;

procedure TFreshnessTests.TryAcceptRejectsLargeJump;
var
  F: TOBDSecOCFreshness;
  Out_: UInt64;
begin
  F := TOBDSecOCFreshness.Create;
  try
    F.MaxJump := 4;
    F.SeedRx($1, 10);
    Assert.IsTrue(F.TryAccept($1, 12, 16, Out_));
    F.SeedRx($1, 10);
    Assert.IsFalse(F.TryAccept($1, 100, 16, Out_));
  finally
    F.Free;
  end;
end;

// ---- codec end-to-end -------------------------------------------------------

function MakeCodec(out AStore: TOBDSecOCKeyStore;
  out AFresh: TOBDSecOCFreshness; out AKey: TAES128Key;
  out ACodec: TOBDSecOCCodec): IInterface;
var
  KeysIface: IOBDSecOCKeyProvider;
  FreshIface: IOBDSecOCFreshnessProvider;
begin
  AStore := TOBDSecOCKeyStore.Create;
  AFresh := TOBDSecOCFreshness.Create;
  KeysIface := AStore;
  FreshIface := AFresh;
  SetKey(AKey, RFC4493_KEY);
  AStore.RegisterKey($ABCD, AKey, 64, 16);
  ACodec := TOBDSecOCCodec.Create(nil);
  ACodec.Keys := KeysIface;
  ACodec.Freshness := FreshIface;
  // Keep the interfaces alive — the codec doesn't AddRef them via
  // property setters; we return one to the test which keeps the
  // store alive for the duration of the test.
  Result := KeysIface;
end;

procedure TSecOCCodecTests.WrapUnwrapRoundTrip;
var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  Key: TAES128Key;
  Wire: TBytes;
  Result_: TOBDSecOCVerification;
  Hold: IInterface;
  Original: TBytes;
begin
  Hold := MakeCodec(Store, Fresh, Key, Codec);
  try
    Original := TBytes.Create($22, $F1, $90, $DE, $AD, $BE, $EF);
    Wire := Codec.Wrap($ABCD, Original);
    // PDU(7) + FV(2) + Tag(8) = 17 bytes
    Assert.AreEqual(17, Length(Wire));
    Result_ := Codec.Unwrap($ABCD, Wire);
    Assert.AreEqual(Length(Original), Length(Result_.OriginalPDU));
    Assert.AreEqual(BytesToHex(Original), BytesToHex(Result_.OriginalPDU));
    Assert.AreEqual(UInt64(1), Result_.FreshnessValue);
  finally
    Codec.Free;
  end;
end;

procedure TSecOCCodecTests.UnwrapDetectsBitFlipInPDU;
var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  Key: TAES128Key;
  Wire: TBytes;
  Hold: IInterface;
begin
  Hold := MakeCodec(Store, Fresh, Key, Codec);
  try
    Wire := Codec.Wrap($ABCD, TBytes.Create($22, $F1, $90));
    Wire[0] := Wire[0] xor $01;
    Assert.WillRaise(
      procedure
      begin
        Codec.Unwrap($ABCD, Wire);
      end,
      EOBDSecOCError);
  finally
    Codec.Free;
  end;
end;

procedure TSecOCCodecTests.UnwrapDetectsBitFlipInMAC;
var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  Key: TAES128Key;
  Wire: TBytes;
  Hold: IInterface;
begin
  Hold := MakeCodec(Store, Fresh, Key, Codec);
  try
    Wire := Codec.Wrap($ABCD, TBytes.Create($22, $F1, $90));
    Wire[High(Wire)] := Wire[High(Wire)] xor $80;
    Assert.WillRaise(
      procedure
      begin
        Codec.Unwrap($ABCD, Wire);
      end,
      EOBDSecOCError);
  finally
    Codec.Free;
  end;
end;

procedure TSecOCCodecTests.UnwrapRejectsReplay;
var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  Key: TAES128Key;
  Wire: TBytes;
  Hold: IInterface;
begin
  Hold := MakeCodec(Store, Fresh, Key, Codec);
  try
    Wire := Codec.Wrap($ABCD, TBytes.Create($01, $02, $03));
    Codec.Unwrap($ABCD, Wire); // first time: ok
    Assert.WillRaise(
      procedure
      begin
        Codec.Unwrap($ABCD, Wire);
      end,
      EOBDSecOCError);
  finally
    Codec.Free;
  end;
end;

procedure TSecOCCodecTests.UnwrapRaisesOnUnknownDataID;
var
  Store: TOBDSecOCKeyStore;
  Fresh: TOBDSecOCFreshness;
  Codec: TOBDSecOCCodec;
  Key: TAES128Key;
  Hold: IInterface;
begin
  Hold := MakeCodec(Store, Fresh, Key, Codec);
  try
    Assert.WillRaise(
      procedure
      begin
        Codec.Unwrap($DEAD, TBytes.Create($00, $00, $00, $00, $00,
          $00, $00, $00, $00, $00, $00));
      end,
      EOBDSecOCError);
  finally
    Codec.Free;
  end;
end;

procedure TSecOCCodecTests.WrapRaisesWithoutKeys;
var
  Codec: TOBDSecOCCodec;
begin
  Codec := TOBDSecOCCodec.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        Codec.Wrap($1234, TBytes.Create($AA));
      end,
      EOBDConfig);
  finally
    Codec.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAESTests);
  TDUnitX.RegisterTestFixture(TCMACTests);
  TDUnitX.RegisterTestFixture(TKeyStoreTests);
  TDUnitX.RegisterTestFixture(TFreshnessTests);
  TDUnitX.RegisterTestFixture(TSecOCCodecTests);

end.
