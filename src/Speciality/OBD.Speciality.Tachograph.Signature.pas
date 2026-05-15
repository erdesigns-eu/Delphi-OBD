//------------------------------------------------------------------------------
//  OBD.Speciality.Tachograph.Signature
//
//  TOBDTachographSignatureChecker — DDD-file signature-chain
//  verifier per EU 165/2014 Annex 1C Appendix 7. Walks the
//  TLV-encoded blocks in a downloaded DDD file, pairs each data
//  block with the signature block that immediately follows it,
//  and hands the pair to a host-supplied <see cref="IOBDSignatureVerifier"/>
//  (typically an ECDSA P-256 / P-384 verifier wired to an OpenSSL
//  backend with the card / VU public key extracted from the
//  corresponding certificate).
//
//  The verifier contract is the same one used by the flashing
//  pipeline so a host can share one OpenSSL plug across firmware
//  flashes and tachograph downloads.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - EU 2016/799 Annex 1C Appendix 7 (DDD format)
//    - EU 2016/799 Annex 1C Appendix 11 (Common Security Mechanisms)
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Tachograph.Signature,
//                     rewired to v2's IOBDSignatureVerifier surface.
//------------------------------------------------------------------------------

unit OBD.Speciality.Tachograph.Signature;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  OBD.Types,
  OBD.Signature;

type
  /// <summary>Raised on malformed DDD input.</summary>
  EOBDTachographSignature = class(Exception);

  /// <summary>
  ///   Classified kind of a DDD TLV block.
  /// </summary>
  TOBDDDDBlockKind = (
    /// <summary>Tag not recognised — typically vendor-specific
    /// extension.</summary>
    bkUnknown,
    /// <summary>Overview block (card / VU header).</summary>
    bkOverview,
    /// <summary>Driver activities.</summary>
    bkActivities,
    /// <summary>Events log.</summary>
    bkEvents,
    /// <summary>Faults log.</summary>
    bkFaults,
    /// <summary>Vehicle-unit metadata block.</summary>
    bkVehicleUnit,
    /// <summary>Speed traces.</summary>
    bkSpeeds,
    /// <summary>Technical-data block.</summary>
    bkTechnicalData,
    /// <summary>Card-chip block.</summary>
    bkCardChip,
    /// <summary>Signature block — carries the signed digest over
    /// the preceding data block.</summary>
    bkSignatureBlock
  );

  /// <summary>One TLV block from a DDD file.</summary>
  TOBDDDDBlock = record
    /// <summary>Decoded block kind.</summary>
    Kind: TOBDDDDBlockKind;
    /// <summary>Raw 2-byte TLV tag.</summary>
    Tag: Word;
    /// <summary>Declared block length (data bytes after the
    /// 4-byte TLV header).</summary>
    Length_: Integer;
    /// <summary>Byte offset of the block in the source file
    /// (TLV header start).</summary>
    Offset: Integer;
    /// <summary>Block payload bytes (length =
    /// <c>Length_</c>).</summary>
    Data: TBytes;
  end;

  /// <summary>
  ///   Result of a chain verification.
  /// </summary>
  TOBDDDDChainResult = record
    /// <summary>True when every data block in the chain was
    /// successfully verified.</summary>
    Verified: Boolean;
    /// <summary>Total blocks parsed from the file.</summary>
    BlocksParsed: Integer;
    /// <summary>Number of signature blocks that passed
    /// verification.</summary>
    SignaturesVerified: Integer;
    /// <summary>Index of the first data block whose signature
    /// failed; <c>-1</c> on success.</summary>
    FirstFailureBlockIndex: Integer;
    /// <summary>Human-readable failure reason; empty on
    /// success.</summary>
    Reason: string;
  end;

  /// <summary>
  ///   DDD signature-chain verifier.
  /// </summary>
  /// <remarks>
  ///   Configure <c>CardVerifier</c> and <c>VUVerifier</c> with
  ///   <see cref="IOBDSignatureVerifier"/> implementations (the
  ///   bundled <c>OBD.Signature.OpenSSL</c> covers the standard
  ///   ECDSA-P256-SHA256 algorithm used by Gen2 tachograph cards
  ///   and VUs). Set <c>CardKey</c> / <c>VUKey</c> with the
  ///   public keys extracted from the respective certificates.
  ///   <c>VerifyChain</c> walks the file and returns a structured
  ///   pass / fail report.
  /// </remarks>
  TOBDTachographSignatureChecker = class
  strict private
    FCardVerifier: IOBDSignatureVerifier;
    FVUVerifier: IOBDSignatureVerifier;
    FCardKey: TBytes;
    FVUKey: TBytes;
    FAlgorithm: TOBDSignatureAlgorithm;
    function ClassifyTag(ATag: Word): TOBDDDDBlockKind;
    function VerifierAndKeyFor(AKind: TOBDDDDBlockKind;
      out AVerifier: IOBDSignatureVerifier;
      out AKey: TBytes): Boolean;
  public
    /// <summary>Constructs the checker with ECDSA-P256-SHA256 as
    /// the default algorithm (the Gen2 spec choice).</summary>
    constructor Create;

    /// <summary>Wires the card-side verifier.</summary>
    /// <param name="AVerifier">Backend implementation.</param>
    /// <param name="APublicKey">Public-key bytes in the
    /// algorithm's standard encoding.</param>
    procedure SetCardVerifier(AVerifier: IOBDSignatureVerifier;
      const APublicKey: TBytes);

    /// <summary>Wires the vehicle-unit-side verifier.</summary>
    /// <param name="AVerifier">Backend implementation.</param>
    /// <param name="APublicKey">Public-key bytes.</param>
    procedure SetVUVerifier(AVerifier: IOBDSignatureVerifier;
      const APublicKey: TBytes);

    /// <summary>Reads / writes the signature algorithm.</summary>
    property Algorithm: TOBDSignatureAlgorithm read FAlgorithm
      write FAlgorithm;

    /// <summary>
    ///   Parses <c>ABytes</c> into structured TLV blocks. Does
    ///   not verify any signature.
    /// </summary>
    /// <param name="ABytes">Source DDD file bytes.</param>
    /// <returns>Parsed blocks in file order.</returns>
    /// <exception cref="EOBDTachographSignature">
    ///   Truncated TLV header or declared length runs past end
    ///   of file.
    /// </exception>
    function ParseBlocks(const ABytes: TBytes): TArray<TOBDDDDBlock>;

    /// <summary>
    ///   Verifies the signature chain across the blocks.
    /// </summary>
    /// <param name="ABytes">Source DDD file bytes.</param>
    /// <returns>Pass / fail report.</returns>
    /// <remarks>
    ///   Each data block must be followed by a signature block
    ///   whose payload, fed to the matching verifier with the
    ///   configured public key, returns <c>True</c>. Unknown
    ///   blocks are skipped without affecting the verdict.
    /// </remarks>
    function VerifyChain(const ABytes: TBytes): TOBDDDDChainResult;
  end;

implementation

const
  // EU 2016/799 Annex 1C Appendix 7 tag identifiers — stable across
  // every published reference implementation (JRC, libtacho, …).
  TAG_OVERVIEW       = $0501;
  TAG_ACTIVITIES     = $0504;
  TAG_EVENTS         = $0502;
  TAG_FAULTS         = $0503;
  TAG_VEHICLE_UNIT   = $0521;
  TAG_SPEEDS         = $0506;
  TAG_TECHNICAL_DATA = $0505;
  TAG_CARD_CHIP      = $0508;
  TAG_SIGNATURE      = $050E;

constructor TOBDTachographSignatureChecker.Create;
begin
  inherited Create;
  FAlgorithm := saECDSA_P256_SHA256;
end;

procedure TOBDTachographSignatureChecker.SetCardVerifier(
  AVerifier: IOBDSignatureVerifier; const APublicKey: TBytes);
begin
  FCardVerifier := AVerifier;
  FCardKey := Copy(APublicKey, 0, Length(APublicKey));
end;

procedure TOBDTachographSignatureChecker.SetVUVerifier(
  AVerifier: IOBDSignatureVerifier; const APublicKey: TBytes);
begin
  FVUVerifier := AVerifier;
  FVUKey := Copy(APublicKey, 0, Length(APublicKey));
end;

function TOBDTachographSignatureChecker.ClassifyTag(
  ATag: Word): TOBDDDDBlockKind;
begin
  case ATag of
    TAG_OVERVIEW:       Result := bkOverview;
    TAG_ACTIVITIES:     Result := bkActivities;
    TAG_EVENTS:         Result := bkEvents;
    TAG_FAULTS:         Result := bkFaults;
    TAG_VEHICLE_UNIT:   Result := bkVehicleUnit;
    TAG_SPEEDS:         Result := bkSpeeds;
    TAG_TECHNICAL_DATA: Result := bkTechnicalData;
    TAG_CARD_CHIP:      Result := bkCardChip;
    TAG_SIGNATURE:      Result := bkSignatureBlock;
  else
    Result := bkUnknown;
  end;
end;

function TOBDTachographSignatureChecker.VerifierAndKeyFor(
  AKind: TOBDDDDBlockKind;
  out AVerifier: IOBDSignatureVerifier;
  out AKey: TBytes): Boolean;
var
  IsCardSide: Boolean;
begin
  IsCardSide := AKind in [bkCardChip, bkOverview, bkActivities,
    bkEvents, bkFaults, bkSpeeds, bkTechnicalData];
  if IsCardSide then
  begin
    AVerifier := FCardVerifier;
    AKey := FCardKey;
  end
  else
  begin
    AVerifier := FVUVerifier;
    AKey := FVUKey;
  end;
  Result := AVerifier <> nil;
end;

function TOBDTachographSignatureChecker.ParseBlocks(
  const ABytes: TBytes): TArray<TOBDDDDBlock>;
var
  Cursor: Integer;
  TagWord: Word;
  Len: Integer;
  Block: TOBDDDDBlock;
  Acc: TList<TOBDDDDBlock>;
begin
  Acc := TList<TOBDDDDBlock>.Create;
  try
    Cursor := 0;
    while Cursor + 4 <= Length(ABytes) do
    begin
      TagWord := (Word(ABytes[Cursor]) shl 8) or ABytes[Cursor + 1];
      Len := (Integer(ABytes[Cursor + 2]) shl 8) or ABytes[Cursor + 3];
      if Cursor + 4 + Len > Length(ABytes) then
        raise EOBDTachographSignature.CreateFmt(
          'DDD truncated at offset %d: declared %d data bytes',
          [Cursor, Len]);
      Block := Default(TOBDDDDBlock);
      Block.Tag := TagWord;
      Block.Kind := ClassifyTag(TagWord);
      Block.Length_ := Len;
      Block.Offset := Cursor;
      SetLength(Block.Data, Len);
      if Len > 0 then
        Move(ABytes[Cursor + 4], Block.Data[0], Len);
      Acc.Add(Block);
      Inc(Cursor, 4 + Len);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TOBDTachographSignatureChecker.VerifyChain(
  const ABytes: TBytes): TOBDDDDChainResult;
var
  Blocks: TArray<TOBDDDDBlock>;
  I: Integer;
  Verifier: IOBDSignatureVerifier;
  Key: TBytes;
  Args: TOBDSignatureVerifyArgs;
begin
  Result := Default(TOBDDDDChainResult);
  Result.FirstFailureBlockIndex := -1;
  Blocks := ParseBlocks(ABytes);
  Result.BlocksParsed := Length(Blocks);

  I := 0;
  while I < High(Blocks) do
  begin
    if Blocks[I].Kind in [bkUnknown, bkSignatureBlock] then
    begin
      Inc(I);
      Continue;
    end;
    if Blocks[I + 1].Kind <> bkSignatureBlock then
    begin
      Result.Reason := Format(
        'Block %d (kind=%d) not followed by a signature block',
        [I, Ord(Blocks[I].Kind)]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    if not VerifierAndKeyFor(Blocks[I].Kind, Verifier, Key) then
    begin
      Result.Reason := Format(
        'No verifier configured for block %d (kind=%d)',
        [I, Ord(Blocks[I].Kind)]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    Args := Default(TOBDSignatureVerifyArgs);
    Args.Algorithm := FAlgorithm;
    Args.Message := Blocks[I].Data;
    Args.Signature := Blocks[I + 1].Data;
    Args.PublicKey := Key;
    if not Verifier.Verify(Args) then
    begin
      Result.Reason := Format(
        'Signature for block %d (offset 0x%x) failed verification',
        [I, Blocks[I].Offset]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    Inc(Result.SignaturesVerified);
    Inc(I, 2);
  end;
  Result.Verified := True;
end;

end.
