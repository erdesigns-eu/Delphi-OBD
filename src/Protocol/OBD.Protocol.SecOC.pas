//------------------------------------------------------------------------------
//  OBD.Protocol.SecOC
//
//  AUTOSAR SecOC wrap / unwrap codec. Composes the AES-128 + CMAC
//  primitive with the key store and the freshness manager to
//  produce / verify Authentic PDUs:
//
//    | Original PDU | Truncated FV (BE) | Truncated MAC |
//
//  The MAC is computed over (DataID-16-BE || Original PDU || Full
//  FV-64-BE) as required by AUTOSAR SecOC SWS §7.2.
//
//  This v1 codec restricts both the truncated-MAC and truncated-FV
//  lengths to multiples of 8 bits. Sub-byte truncation (e.g. 4-bit
//  FV on a 64-bit CAN frame) requires bit-packing on the wire and
//  is tracked as a Follow-up — every production OBD use
//  case (UDS, J1939, DoIP) carries 8/16/24/64-bit lengths.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - AUTOSAR SecOC SWS R23-11
//    - NIST SP 800-38B (CMAC)
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Protocol.SecOC;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.SecOC.AES,
  OBD.Protocol.SecOC.CMAC,
  OBD.Protocol.SecOC.Keys,
  OBD.Protocol.SecOC.Freshness;

type
  /// <summary>Raised when SecOC verification fails (replay,
  /// MAC mismatch, unknown Data ID, malformed PDU).</summary>
  EOBDSecOCError = class(Exception);

  /// <summary>Outcome of a successful unwrap operation.</summary>
  TOBDSecOCVerification = record
    /// <summary>Authenticated original PDU bytes.</summary>
    OriginalPDU: TBytes;
    /// <summary>Reconstructed full freshness counter used for
    /// MAC verification.</summary>
    FreshnessValue: UInt64;
    /// <summary>Data ID resolved from the inputs.</summary>
    DataID: Word;
  end;

  /// <summary>
  ///   Stateless SecOC wrap / unwrap codec. Holds a key provider and
  ///   a freshness provider; the rest is per-call.
  /// </summary>
  /// <remarks>
  ///   The codec is component-friendly (descends from
  ///   <c>TComponent</c>) so a host can drop it on a form, point
  ///   <c>Keys</c> at a <c>TOBDSecOCKeyStore</c> and
  ///   <c>Freshness</c> at a <c>TOBDSecOCFreshness</c>, and call
  ///   <c>Wrap</c> / <c>Unwrap</c> from anywhere — including from
  ///   a worker thread. Both providers are guaranteed thread-safe
  ///   by the unit-level implementations.
  /// </remarks>
  TOBDSecOCCodec = class(TComponent)
  strict private
    FKeys: IOBDSecOCKeyProvider;
    FFreshness: IOBDSecOCFreshnessProvider;

    function ResolveBinding(ADataID: Word): TOBDSecOCBinding;
    function ComposeAADAndTag(const ABinding: TOBDSecOCBinding;
      const AOriginal: TBytes; AFV: UInt64; ATagBytes: Integer): TBytes;
  public
    /// <summary>Creates the codec.</summary>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Authenticates <c>AOriginalPDU</c> under the key bound to
    ///   <c>ADataID</c>, advances the freshness counter, and returns
    ///   the Authentic PDU (Original || Truncated FV || Truncated
    ///   MAC).
    /// </summary>
    /// <param name="ADataID">Data ID resolved at the application
    /// layer (UDS DID, CAN ID, J1939 PGN — host-defined).</param>
    /// <param name="AOriginalPDU">Plain bytes to authenticate. May
    /// be empty.</param>
    /// <returns>Wire bytes.</returns>
    /// <exception cref="EOBDConfig">Key or freshness provider not
    /// assigned.</exception>
    /// <exception cref="EOBDSecOCError">Data ID unknown or
    /// truncation lengths not a multiple of 8 bits.</exception>
    function Wrap(ADataID: Word; const AOriginalPDU: TBytes): TBytes;

    /// <summary>
    ///   Verifies an Authentic PDU and returns the original bytes
    ///   plus the reconstructed freshness counter.
    /// </summary>
    /// <param name="ADataID">Data ID expected for this binding.</param>
    /// <param name="AAuthenticPDU">Wire bytes (Original || Trunc FV
    /// || Trunc MAC).</param>
    /// <returns>Verification result.</returns>
    /// <exception cref="EOBDSecOCError">PDU too short, freshness
    /// rejected (replay / out-of-window) or MAC mismatch.</exception>
    function Unwrap(ADataID: Word;
      const AAuthenticPDU: TBytes): TOBDSecOCVerification;

    /// <summary>Bound key provider. Must be assigned before
    /// <c>Wrap</c> / <c>Unwrap</c>.</summary>
    property Keys: IOBDSecOCKeyProvider read FKeys write FKeys;
    /// <summary>Bound freshness provider. Must be assigned before
    /// <c>Wrap</c> / <c>Unwrap</c>.</summary>
    property Freshness: IOBDSecOCFreshnessProvider read FFreshness
      write FFreshness;
  end;

implementation

constructor TOBDSecOCCodec.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TOBDSecOCCodec.ResolveBinding(ADataID: Word): TOBDSecOCBinding;
begin
  if FKeys = nil then
    raise EOBDConfig.Create('TOBDSecOCCodec: Keys provider not assigned');
  if FFreshness = nil then
    raise EOBDConfig.Create('TOBDSecOCCodec: Freshness provider not assigned');
  if not FKeys.TryGet(ADataID, Result) then
    raise EOBDSecOCError.CreateFmt(
      'SecOC: no binding for Data ID 0x%4.4X', [ADataID]);
  if (Result.TagBits mod 8) <> 0 then
    raise EOBDSecOCError.CreateFmt(
      'SecOC v1: TagBits %d must be a multiple of 8 (Data ID 0x%4.4X)',
      [Result.TagBits, ADataID]);
  if (Result.FreshnessBits mod 8) <> 0 then
    raise EOBDSecOCError.CreateFmt(
      'SecOC v1: FreshnessBits %d must be a multiple of 8 (Data ID 0x%4.4X)',
      [Result.FreshnessBits, ADataID]);
end;

function TOBDSecOCCodec.ComposeAADAndTag(const ABinding: TOBDSecOCBinding;
  const AOriginal: TBytes; AFV: UInt64; ATagBytes: Integer): TBytes;
var
  Aad: TBytes;
  AadLen, OrigLen: Integer;
  I: Integer;
  FullTag: TBytes;
begin
  OrigLen := Length(AOriginal);
  AadLen := 2 + OrigLen + 8;
  SetLength(Aad, AadLen);
  Aad[0] := Byte((ABinding.DataID shr 8) and $FF);
  Aad[1] := Byte(ABinding.DataID and $FF);
  if OrigLen > 0 then
    Move(AOriginal[0], Aad[2], OrigLen);
  // Full FV — 64-bit big-endian.
  for I := 0 to 7 do
    Aad[2 + OrigLen + I] :=
      Byte((AFV shr (8 * (7 - I))) and $FF);

  FullTag := TOBDCMACAES.ComputeTruncated(ABinding.Key, Aad,
    ABinding.TagBits);
  // ComputeTruncated already sized to ceil(TagBits/8); for byte-
  // aligned tags this matches ATagBytes exactly.
  if Length(FullTag) <> ATagBytes then
    raise EOBDInternal.CreateFmt(
      'SecOC: tag length mismatch (got %d, expected %d)',
      [Length(FullTag), ATagBytes]);
  Result := FullTag;
end;

function TOBDSecOCCodec.Wrap(ADataID: Word;
  const AOriginalPDU: TBytes): TBytes;
var
  Binding: TOBDSecOCBinding;
  FV: UInt64;
  TagBytes, FvBytes, OrigLen: Integer;
  TruncFV, Tag: TBytes;
  I: Integer;
begin
  Binding := ResolveBinding(ADataID);
  FV := FFreshness.NextTx(ADataID);

  TagBytes := Binding.TagBits div 8;
  FvBytes  := Binding.FreshnessBits div 8;
  OrigLen  := Length(AOriginalPDU);

  // Truncated FV — low <FreshnessBits> bits, big-endian.
  SetLength(TruncFV, FvBytes);
  for I := 0 to FvBytes - 1 do
    TruncFV[I] := Byte((FV shr (8 * (FvBytes - 1 - I))) and $FF);

  Tag := ComposeAADAndTag(Binding, AOriginalPDU, FV, TagBytes);

  SetLength(Result, OrigLen + FvBytes + TagBytes);
  if OrigLen > 0 then
    Move(AOriginalPDU[0], Result[0], OrigLen);
  if FvBytes > 0 then
    Move(TruncFV[0], Result[OrigLen], FvBytes);
  if TagBytes > 0 then
    Move(Tag[0], Result[OrigLen + FvBytes], TagBytes);
end;

function TOBDSecOCCodec.Unwrap(ADataID: Word;
  const AAuthenticPDU: TBytes): TOBDSecOCVerification;
var
  Binding: TOBDSecOCBinding;
  TagBytes, FvBytes, OrigLen, TotalLen: Integer;
  TruncFV: UInt64;
  ReconFV: UInt64;
  Original, ReceivedTag, ExpectedTag: TBytes;
  I, J, Diff: Integer;
begin
  Binding := ResolveBinding(ADataID);

  TagBytes := Binding.TagBits div 8;
  FvBytes  := Binding.FreshnessBits div 8;
  TotalLen := Length(AAuthenticPDU);
  if TotalLen < TagBytes + FvBytes then
    raise EOBDSecOCError.Create(
      'SecOC unwrap: Authentic PDU shorter than FV + tag');
  OrigLen := TotalLen - FvBytes - TagBytes;

  SetLength(Original, OrigLen);
  if OrigLen > 0 then
    Move(AAuthenticPDU[0], Original[0], OrigLen);

  TruncFV := 0;
  for I := 0 to FvBytes - 1 do
    TruncFV := (TruncFV shl 8) or AAuthenticPDU[OrigLen + I];

  if not FFreshness.TryAccept(ADataID, TruncFV,
    Binding.FreshnessBits, ReconFV) then
    raise EOBDSecOCError.CreateFmt(
      'SecOC unwrap: freshness rejected (Data ID 0x%4.4X)', [ADataID]);

  SetLength(ReceivedTag, TagBytes);
  if TagBytes > 0 then
    Move(AAuthenticPDU[OrigLen + FvBytes], ReceivedTag[0], TagBytes);

  ExpectedTag := ComposeAADAndTag(Binding, Original, ReconFV, TagBytes);

  // Constant-time compare.
  Diff := 0;
  J := Length(ReceivedTag);
  if J <> Length(ExpectedTag) then
    Diff := 1
  else
    for I := 0 to J - 1 do
      Diff := Diff or (ReceivedTag[I] xor ExpectedTag[I]);
  if Diff <> 0 then
    raise EOBDSecOCError.CreateFmt(
      'SecOC unwrap: MAC verification failed (Data ID 0x%4.4X)', [ADataID]);

  Result.OriginalPDU    := Original;
  Result.FreshnessValue := ReconFV;
  Result.DataID         := ADataID;
end;

end.
