//------------------------------------------------------------------------------
//  Tests.OBD.Flashing.Followups
//
//  Phase 9 follow-up coverage: atomic checkpoint write,
//  image-applicability cross-check, PKCS#11 vendor-shim
//  configuration surface, OEM-handshake catalogue loader +
//  ApplyTo overlays.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9 follow-up.
//------------------------------------------------------------------------------

unit Tests.OBD.Flashing.Followups;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Hash,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.UDS.Transfer,
  OBD.Flash.Checkpoint,
  OBD.Flash.ImageApplicability,
  OBD.Signature.HSM.PKCS11,
  OBD.Flash.OEM.Catalog,
  OBD.Flash.OEM.VAG,
  OBD.Flash.OEM.BMW;

type
  /// <summary>Atomic checkpoint write coverage.</summary>
  [TestFixture]
  TCheckpointAtomicTests = class
  public
    [Test] procedure SaveOverwriteSurvivesExistingFile;
    [Test] procedure SaveCreatesNewFile;
  end;

  /// <summary>Image-applicability descriptor.</summary>
  [TestFixture]
  TImageApplicabilityTests = class
  public
    [Test] procedure ParsesExactConstraint;
    [Test] procedure ParsesAnyOfConstraint;
    [Test] procedure ParsesHexPrefixConstraint;
    [Test] procedure RejectsWrongVersion;
    [Test] procedure RejectsBadShaLength;
    [Test] procedure MatchesImageDetectsTamper;
  end;

  /// <summary>PKCS#11 vendor shim configuration surface.</summary>
  [TestFixture]
  TPKCS11ShimTests = class
  public
    [Test] procedure SupportsExpectedAlgorithms;
    [Test] procedure IsAvailableFalseWithoutLibraryPath;
    [Test] procedure IsAvailableFalseWithBadPath;
    [Test] procedure DoesNotSupportPQCAlgorithms;
  end;

  /// <summary>OEM-handshake catalogue.</summary>
  [TestFixture]
  TOEMHandshakeCatalogTests = class
  public
    [Test] procedure LoadsPlatformAndApplies;
    [Test] procedure ApplyToBMWHonoursExtendedFirst;
    [Test] procedure ReplaceSameVendor;
    [Test] procedure RejectsWrongVersion;
  end;

implementation

{ ---- Checkpoint atomic ----------------------------------------------------- }

procedure TCheckpointAtomicTests.SaveOverwriteSurvivesExistingFile;
var
  Tmp: string;
  Image: TBytes;
  Info: TOBDFlashCheckpointInfo;
  Loaded: TOBDFlashCheckpointInfo;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cp-' +
    TGUID.NewGuid.ToString + '.json');
  Image := TBytes.Create($AA, $BB, $CC);
  Info := Default(TOBDFlashCheckpointInfo);
  Info.SessionID := 'first';
  Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(Image);
  Info.Cursor.TotalBytes := UInt32(Length(Image));
  Info.Cursor.NextBSC := 1;
  try
    TOBDFlashCheckpoint.Save(Tmp, Info);
    Info.SessionID := 'second';
    TOBDFlashCheckpoint.Save(Tmp, Info);  // ← exercises ReplaceFile path
    Loaded := TOBDFlashCheckpoint.Load(Tmp);
    Assert.AreEqual('second', Loaded.SessionID);
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TCheckpointAtomicTests.SaveCreatesNewFile;
var
  Tmp: string;
  Info: TOBDFlashCheckpointInfo;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cp-new-' +
    TGUID.NewGuid.ToString + '.json');
  Info := Default(TOBDFlashCheckpointInfo);
  Info.SessionID := 'fresh';
  Info.ImageSha256 := TOBDFlashCheckpoint.ComputeImageHash(TBytes.Create($00));
  Info.Cursor.NextBSC := 1;
  try
    TOBDFlashCheckpoint.Save(Tmp, Info);
    Assert.IsTrue(TFile.Exists(Tmp));
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

{ ---- Image applicability --------------------------------------------------- }

function HashHex(const ABytes: TBytes): string;
var
  H: TBytes;
  I: Integer;
begin
  H := THashSHA2.GetHashBytes(ABytes, THashSHA2.TSHA2Version.SHA256);
  Result := '';
  for I := 0 to High(H) do
    Result := Result + LowerCase(IntToHex(H[I], 2));
end;

procedure TImageApplicabilityTests.ParsesExactConstraint;
var
  Json: string;
  Desc: TOBDApplicabilityDescriptor;
begin
  Json :=
    '{ "version": 1, ' +
    '  "image_sha256_hex": "' + HashHex(TBytes.Create($00, $01)) + '",' +
    '  "constraints": [' +
    '    { "did": "0xF150", "name": "ECU_HW_PartNumber",' +
    '      "match": { "kind": "exact", "value": "1234567890" } }' +
    '  ] }';
  Desc := TOBDFlashImageApplicability.Parse(Json);
  Assert.AreEqual(1, Length(Desc.Constraints));
  Assert.AreEqual($F150, Integer(Desc.Constraints[0].DID));
  Assert.AreEqual(Ord(akExact), Ord(Desc.Constraints[0].Kind));
  Assert.AreEqual('1234567890', Desc.Constraints[0].Value);
end;

procedure TImageApplicabilityTests.ParsesAnyOfConstraint;
var
  Json: string;
  Desc: TOBDApplicabilityDescriptor;
begin
  Json :=
    '{ "version": 1, ' +
    '  "image_sha256_hex": "' + HashHex(TBytes.Create($00)) + '",' +
    '  "constraints": [' +
    '    { "did": 100,' +
    '      "match": { "kind": "any_of",' +
    '                 "values": ["A", "B", "C"] } }' +
    '  ] }';
  Desc := TOBDFlashImageApplicability.Parse(Json);
  Assert.AreEqual(Ord(akAnyOf), Ord(Desc.Constraints[0].Kind));
  Assert.AreEqual(3, Length(Desc.Constraints[0].Values));
  Assert.AreEqual('B', Desc.Constraints[0].Values[1]);
end;

procedure TImageApplicabilityTests.ParsesHexPrefixConstraint;
var
  Json: string;
  Desc: TOBDApplicabilityDescriptor;
begin
  Json :=
    '{ "version": 1, ' +
    '  "image_sha256_hex": "' + HashHex(TBytes.Create($00)) + '",' +
    '  "constraints": [' +
    '    { "did": "0xF18A",' +
    '      "match": { "kind": "hex_prefix", "value": "DE AD BE" } }' +
    '  ] }';
  Desc := TOBDFlashImageApplicability.Parse(Json);
  Assert.AreEqual(Ord(akHexPrefix), Ord(Desc.Constraints[0].Kind));
  Assert.AreEqual('DE AD BE', Desc.Constraints[0].Value);
end;

procedure TImageApplicabilityTests.RejectsWrongVersion;
var
  Json: string;
begin
  Json := '{ "version": 2, "image_sha256_hex":"' +
    StringOfChar('0', 64) + '" }';
  Assert.WillRaise(
    procedure begin TOBDFlashImageApplicability.Parse(Json); end,
    EOBDProtocol);
end;

procedure TImageApplicabilityTests.RejectsBadShaLength;
var
  Json: string;
begin
  Json := '{ "version": 1, "image_sha256_hex":"deadbeef" }';
  Assert.WillRaise(
    procedure begin TOBDFlashImageApplicability.Parse(Json); end,
    EOBDProtocol);
end;

procedure TImageApplicabilityTests.MatchesImageDetectsTamper;
var
  Image, Tampered: TBytes;
  Desc: TOBDApplicabilityDescriptor;
begin
  Image := TBytes.Create($AA, $BB, $CC);
  Tampered := TBytes.Create($AA, $BB, $00);
  Desc := Default(TOBDApplicabilityDescriptor);
  Desc.ImageSha256Hex := HashHex(Image);
  Assert.IsTrue (TOBDFlashImageApplicability.MatchesImage(Desc,
    THashSHA2.GetHashBytes(Image, THashSHA2.TSHA2Version.SHA256)));
  Assert.IsFalse(TOBDFlashImageApplicability.MatchesImage(Desc,
    THashSHA2.GetHashBytes(Tampered, THashSHA2.TSHA2Version.SHA256)));
end;

{ ---- PKCS#11 shim ---------------------------------------------------------- }

procedure TPKCS11ShimTests.SupportsExpectedAlgorithms;
var
  P: TOBDSignaturePKCS11;
begin
  P := TOBDSignaturePKCS11.Create;
  try
    Assert.IsTrue (P.Supports(saRSA_PSS_SHA256));
    Assert.IsTrue (P.Supports(saECDSA_P256_SHA256));
    Assert.IsTrue (P.Supports(saED25519));
  finally
    P := nil;
  end;
end;

procedure TPKCS11ShimTests.IsAvailableFalseWithoutLibraryPath;
var
  P: TOBDSignaturePKCS11;
begin
  P := TOBDSignaturePKCS11.Create;
  try
    P.LibraryPath := '';
    Assert.IsFalse(P.IsAvailable);
  finally
    P := nil;
  end;
end;

procedure TPKCS11ShimTests.IsAvailableFalseWithBadPath;
var
  P: TOBDSignaturePKCS11;
begin
  P := TOBDSignaturePKCS11.Create;
  try
    P.LibraryPath := 'C:\definitely-does-not-exist\softhsm2.dll';
    Assert.IsFalse(P.IsAvailable);
  finally
    P := nil;
  end;
end;

procedure TPKCS11ShimTests.DoesNotSupportPQCAlgorithms;
var
  P: TOBDSignaturePKCS11;
begin
  P := TOBDSignaturePKCS11.Create;
  try
    Assert.IsFalse(P.Supports(saDilithium2));
    Assert.IsFalse(P.Supports(saSPHINCSPlusSHA2_128f));
  finally
    P := nil;
  end;
end;

{ ---- OEM handshake catalogue ----------------------------------------------- }

procedure TOEMHandshakeCatalogTests.LoadsPlatformAndApplies;
var
  Tmp: string;
  Plat: TOBDOEMPlatform;
  V: TOBDFlashHandshakeVAG;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'oem-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "vag",' +
    '  "platforms": [' +
    '    { "name": "MED17.5.20",' +
    '      "session_subfunction": 2,' +
    '      "security_level": 27,' +
    '      "erase_routine_id": "0xFF01" }' +
    '  ] }', TEncoding.UTF8);
  try
    TOBDFlashOEMCatalog.Default.Clear;
    TOBDFlashOEMCatalog.Default.LoadFile(Tmp);
    Assert.IsTrue(TOBDFlashOEMCatalog.Default.TryGetPlatform(
      'vag', 'MED17.5.20', Plat));
    Assert.IsTrue(Plat.HasSecurityLevel);
    Assert.AreEqual(27, Integer(Plat.SecurityLevel));
    Assert.AreEqual($FF01, Integer(Plat.EraseRoutineID));

    V := TOBDFlashHandshakeVAG.Create(nil);
    try
      Assert.AreEqual($11, Integer(V.SecurityLevel),
        'sanity: pre-overlay default');
      TOBDFlashOEMCatalog.ApplyTo(V, Plat);
      Assert.AreEqual(27, Integer(V.SecurityLevel),
        'overlay applied');
      Assert.AreEqual($FF01, Integer(V.EraseRoutineID));
    finally
      V.Free;
    end;
  finally
    TFile.Delete(Tmp);
    TOBDFlashOEMCatalog.Default.Clear;
  end;
end;

procedure TOEMHandshakeCatalogTests.ApplyToBMWHonoursExtendedFirst;
var
  Tmp: string;
  Plat: TOBDOEMPlatform;
  B: TOBDFlashHandshakeBMW;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'oem-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "bmw",' +
    '  "platforms": [' +
    '    { "name": "G05",' +
    '      "extended_first": false,' +
    '      "security_level": 9 }' +
    '  ] }', TEncoding.UTF8);
  try
    TOBDFlashOEMCatalog.Default.Clear;
    TOBDFlashOEMCatalog.Default.LoadFile(Tmp);
    Assert.IsTrue(TOBDFlashOEMCatalog.Default.TryGetPlatform(
      'bmw', 'G05', Plat));
    B := TOBDFlashHandshakeBMW.Create(nil);
    try
      Assert.IsTrue(B.ExtendedFirst, 'sanity: pre-overlay default');
      TOBDFlashOEMCatalog.ApplyTo(B, Plat);
      Assert.IsFalse(B.ExtendedFirst, 'overlay turned it off');
    finally
      B.Free;
    end;
  finally
    TFile.Delete(Tmp);
    TOBDFlashOEMCatalog.Default.Clear;
  end;
end;

procedure TOEMHandshakeCatalogTests.ReplaceSameVendor;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'oem-' +
    TGUID.NewGuid.ToString + '.json');
  try
    TOBDFlashOEMCatalog.Default.Clear;
    TFile.WriteAllText(Tmp,
      '{ "version": 1, "vendor": "vag",' +
      '  "platforms": [{ "name": "first" }] }', TEncoding.UTF8);
    TOBDFlashOEMCatalog.Default.LoadFile(Tmp);
    TFile.WriteAllText(Tmp,
      '{ "version": 1, "vendor": "vag",' +
      '  "platforms": [{ "name": "second" }] }', TEncoding.UTF8);
    TOBDFlashOEMCatalog.Default.LoadFile(Tmp);
    Assert.AreEqual(1, TOBDFlashOEMCatalog.Default.Count);
  finally
    TFile.Delete(Tmp);
    TOBDFlashOEMCatalog.Default.Clear;
  end;
end;

procedure TOEMHandshakeCatalogTests.RejectsWrongVersion;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'oem-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 2, "vendor": "vag", "platforms": [] }',
    TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure begin TOBDFlashOEMCatalog.Default.LoadFile(Tmp); end,
      EOBDProtocol);
  finally
    TFile.Delete(Tmp);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCheckpointAtomicTests);
  TDUnitX.RegisterTestFixture(TImageApplicabilityTests);
  TDUnitX.RegisterTestFixture(TPKCS11ShimTests);
  TDUnitX.RegisterTestFixture(TOEMHandshakeCatalogTests);

end.
