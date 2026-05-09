//------------------------------------------------------------------------------
// UNIT           : OBD.Tachograph.Signature.pas
// CONTENTS       : EU tachograph DDD signature chain verification
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Tachograph.Signature;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Generics.Collections,

  OBD.ECU.Signature;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDTachographSignature = class(Exception);

  TDDDBlockKind = (
    dbkUnknown,
    dbkOverview,
    dbkActivities,
    dbkEvents,
    dbkFaults,
    dbkVehicleUnit,
    dbkSpeeds,
    dbkTechnicalData,
    dbkCardChip,
    dbkSignatureBlock      // contains a signed digest over the prior block
  );

  TDDDBlock = record
    /// <summary>Kind.</summary>
    Kind: TDDDBlockKind;
    Tag: Word;             // raw 2-byte TLV tag from the file
    /// <summary>Length.</summary>
    Length: Integer;
    Offset: Integer;       // byte offset within the file
    /// <summary>Data.</summary>
    Data: TBytes;
  end;

  TDDDChainResult = record
    /// <summary>Verified.</summary>
    Verified: Boolean;
    /// <summary>Blocks parsed.</summary>
    BlocksParsed: Integer;
    /// <summary>Signatures verified.</summary>
    SignaturesVerified: Integer;
    FirstFailureBlockIndex: Integer; // -1 on success
    /// <summary>Reason.</summary>
    Reason: string;
  end;

  TOBDTachographSignatureChecker = class
  private
    FVerifierForCard: IFirmwareSignatureVerifier;
    FVerifierForVU: IFirmwareSignatureVerifier;
    /// <summary>Classify tag.</summary>
    function ClassifyTag(Tag: Word): TDDDBlockKind;
  public
    /// <summary>Set the verifier used for the card-side signature
    /// block. Production code wires an OpenSSL ECDSA verifier here;
    /// unit tests can pass TOBDPermissiveSignatureVerifier.</summary>
    procedure SetCardVerifier(const V: IFirmwareSignatureVerifier);
    /// <summary>Set the verifier used for the vehicle-unit signature
    /// block. Same wiring story as the card verifier.</summary>
    procedure SetVUVerifier(const V: IFirmwareSignatureVerifier);

    /// <summary>Parse a DDD file into its TLV blocks. Doesn't verify.</summary>
    function ParseBlocks(const Bytes: TBytes): TArray<TDDDBlock>;

    /// <summary>Verify the signature chain across the parsed blocks.
    /// Each data block must be immediately followed by a signature
    /// block whose body, when fed to the configured verifier
    /// alongside the data block bytes, returns True.</summary>
    function VerifyChain(const Bytes: TBytes): TDDDChainResult;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  // Tags seen in the wild on Gen1 / Gen2 driver cards. Source: EU
  // 2016/799 Annex 1C Appendix 7. Values are spec-stable and
  // documented in publicly downloadable tooling (e.g. JRC reference
  // implementation, libtacho).
  TAG_OVERVIEW         = $0501;
  TAG_ACTIVITIES       = $0504;
  TAG_EVENTS           = $0502;
  TAG_FAULTS           = $0503;
  TAG_VEHICLE_UNIT     = $0521;
  TAG_SPEEDS           = $0506;
  TAG_TECHNICAL_DATA   = $0505;
  TAG_CARD_CHIP        = $0508;
  TAG_SIGNATURE        = $050E;

//------------------------------------------------------------------------------
// SET CARD VERIFIER
//------------------------------------------------------------------------------
procedure TOBDTachographSignatureChecker.SetCardVerifier(
  const V: IFirmwareSignatureVerifier);
begin
  FVerifierForCard := V;
end;

//------------------------------------------------------------------------------
// SET VUVERIFIER
//------------------------------------------------------------------------------
procedure TOBDTachographSignatureChecker.SetVUVerifier(
  const V: IFirmwareSignatureVerifier);
begin
  FVerifierForVU := V;
end;

//------------------------------------------------------------------------------
// CLASSIFY TAG
//------------------------------------------------------------------------------
function TOBDTachographSignatureChecker.ClassifyTag(Tag: Word): TDDDBlockKind;
begin
  case Tag of
    TAG_OVERVIEW:       Result := dbkOverview;
    TAG_ACTIVITIES:     Result := dbkActivities;
    TAG_EVENTS:         Result := dbkEvents;
    TAG_FAULTS:         Result := dbkFaults;
    TAG_VEHICLE_UNIT:   Result := dbkVehicleUnit;
    TAG_SPEEDS:         Result := dbkSpeeds;
    TAG_TECHNICAL_DATA: Result := dbkTechnicalData;
    TAG_CARD_CHIP:      Result := dbkCardChip;
    TAG_SIGNATURE:      Result := dbkSignatureBlock;
  else
    Result := dbkUnknown;
  end;
end;

//------------------------------------------------------------------------------
// PARSE BLOCKS
//------------------------------------------------------------------------------
function TOBDTachographSignatureChecker.ParseBlocks(
  const Bytes: TBytes): TArray<TDDDBlock>;
var
  Cursor: Integer;
  Block: TDDDBlock;
  List: TList<TDDDBlock>;
  TagWord: Word;
  Len: Integer;
begin
  List := TList<TDDDBlock>.Create;
  try
    Cursor := 0;
    while Cursor + 4 <= Length(Bytes) do
    begin
      TagWord := (UInt32(Bytes[Cursor]) shl 8) or Bytes[Cursor + 1];
      Len := (UInt32(Bytes[Cursor + 2]) shl 8) or Bytes[Cursor + 3];
      if Cursor + 4 + Len > Length(Bytes) then
        raise EOBDTachographSignature.CreateFmt(
          'DDD truncated at offset %d: declared %d data bytes',
          [Cursor, Len]);
      Block := Default(TDDDBlock);
      Block.Tag := TagWord;
      Block.Kind := ClassifyTag(TagWord);
      Block.Length := Len;
      Block.Offset := Cursor;
      SetLength(Block.Data, Len);
      if Len > 0 then
        Move(Bytes[Cursor + 4], Block.Data[0], Len);
      List.Add(Block);
      Inc(Cursor, 4 + Len);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

//------------------------------------------------------------------------------
// VERIFY CHAIN
//------------------------------------------------------------------------------
function TOBDTachographSignatureChecker.VerifyChain(
  const Bytes: TBytes): TDDDChainResult;
var
  Blocks: TArray<TDDDBlock>;
  I: Integer;
  Verifier: IFirmwareSignatureVerifier;
  IsCardSide: Boolean;
begin
  Result := Default(TDDDChainResult);
  Result.FirstFailureBlockIndex := -1;
  Blocks := ParseBlocks(Bytes);
  Result.BlocksParsed := Length(Blocks);

  I := 0;
  while I < High(Blocks) do
  begin
    if Blocks[I].Kind in [dbkUnknown, dbkSignatureBlock] then
    begin
      Inc(I);
      Continue;
    end;
    // Expect the next block to be a signature over the current one.
    if Blocks[I + 1].Kind <> dbkSignatureBlock then
    begin
      Result.Reason := Format(
        'Block %d (kind=%d) not followed by a signature block',
        [I, Ord(Blocks[I].Kind)]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    IsCardSide := Blocks[I].Kind in [dbkCardChip, dbkOverview, dbkActivities,
      dbkEvents, dbkFaults, dbkSpeeds, dbkTechnicalData];
    if IsCardSide then
      Verifier := FVerifierForCard
    else
      Verifier := FVerifierForVU;
    if Verifier = nil then
    begin
      Result.Reason := Format(
        'No verifier configured for %s block (index %d)',
        [BoolToStr(IsCardSide, True), I]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    if not Verifier.Verify(Blocks[I].Data, Blocks[I + 1].Data) then
    begin
      Result.Reason := Format(
        'Signature for block %d (offset 0x%x) failed verification with %s',
        [I, Blocks[I].Offset, Verifier.AlgorithmName]);
      Result.FirstFailureBlockIndex := I;
      Exit;
    end;
    Inc(Result.SignaturesVerified);
    Inc(I, 2);
  end;
  Result.Verified := True;
end;

end.
