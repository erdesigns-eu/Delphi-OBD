//------------------------------------------------------------------------------
// UNIT           : Tests.SecureSettings
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Exercises TOBDSecureSettings end-to-end against the real
//                  Windows DPAPI. Requires the build-and-test job to run on
//                  Windows (the self-hosted Delphi runner). The lint-only
//                  CI job on Ubuntu doesn't compile this code.
//------------------------------------------------------------------------------
unit Tests.SecureSettings;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSecureSettingsTests = class
  public
    [Test] procedure DPAPI_RoundTrip_PreservesBytes;
    [Test] procedure DPAPI_EmptyInput_RoundTripsCleanly;
    [Test] procedure DPAPI_DecryptOfTamperedCiphertext_Raises;

    [Test] procedure Settings_WriteReadRoundTrip;
    [Test] procedure Settings_MissingKeyReturnsDefault;
    [Test] procedure Settings_ReadString_FallsBackOnGarbageCipher;
    [Test] procedure Settings_DeleteKeyClearsValue;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Classes,
  OBD.SecureSettings;

//------------------------------------------------------------------------------
// SCRATCH PATH
//------------------------------------------------------------------------------
function ScratchPath(const Suffix: string): string;
begin
  Result := TPath.Combine(TPath.GetTempPath,
    Format('obdsec-%d-%d-%s', [GetCurrentProcessId, GetTickCount, Suffix]));
end;

//------------------------------------------------------------------------------
// DPAPI_ROUND TRIP_PRESERVES BYTES
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.DPAPI_RoundTrip_PreservesBytes;
var
  Plain, Cipher, Decoded: TBytes;
begin
  Plain := TEncoding.UTF8.GetBytes('Dealer code: 1234-AUDI-VW-2026');
  Cipher := DPAPIEncrypt(Plain);
  Assert.IsTrue(Length(Cipher) > 0, 'cipher must be non-empty');
  Assert.AreNotEqual(Length(Plain), Length(Cipher),
    'cipher should differ in length from plaintext');

  Decoded := DPAPIDecrypt(Cipher);
  Assert.AreEqual(Length(Plain), Length(Decoded));
  Assert.AreEqual(TEncoding.UTF8.GetString(Plain),
                  TEncoding.UTF8.GetString(Decoded));
end;

//------------------------------------------------------------------------------
// DPAPI_EMPTY INPUT_ROUND TRIPS CLEANLY
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.DPAPI_EmptyInput_RoundTripsCleanly;
var
  Empty, Cipher, Decoded: TBytes;
begin
  SetLength(Empty, 0);
  Cipher := DPAPIEncrypt(Empty);
  // Empty input ≡ empty output by contract.
  Assert.AreEqual(0, Length(Cipher));
  Decoded := DPAPIDecrypt(Cipher);
  Assert.AreEqual(0, Length(Decoded));
end;

//------------------------------------------------------------------------------
// DPAPI_DECRYPT OF TAMPERED CIPHERTEXT_RAISES
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.DPAPI_DecryptOfTamperedCiphertext_Raises;
var
  Plain, Cipher: TBytes;
begin
  Plain := TEncoding.UTF8.GetBytes('something secret');
  Cipher := DPAPIEncrypt(Plain);
  // Flip a byte in the middle to invalidate the MAC DPAPI ships with
  // every blob.
  Cipher[Length(Cipher) div 2] := Cipher[Length(Cipher) div 2] xor $FF;
  Assert.WillRaise(
    procedure begin DPAPIDecrypt(Cipher); end,
    EOBDDpapiError);
end;

//------------------------------------------------------------------------------
// SETTINGS_WRITE READ ROUND TRIP
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.Settings_WriteReadRoundTrip;
var
  Path: string;
  Settings: TOBDSecureSettings;
  Round: string;
  RawIni: TStringList;
  RawText: string;
begin
  Path := ScratchPath('rt.ini');
  if TFile.Exists(Path) then TFile.Delete(Path);

  Settings := TOBDSecureSettings.Create(Path);
  try
    Settings.WriteString('Dealer', 'AudiCode',  '1234-ABCDEF');
    Settings.WriteString('Dealer', 'BMWCode',   'BMW-PIN-987654');
    Settings.Save;
  finally
    Settings.Free;
  end;

  // Round-trip via a fresh instance so we exercise the on-disk path.
  Settings := TOBDSecureSettings.Create(Path);
  try
    Round := Settings.ReadString('Dealer', 'AudiCode', '');
    Assert.AreEqual('1234-ABCDEF', Round);
    Round := Settings.ReadString('Dealer', 'BMWCode', '');
    Assert.AreEqual('BMW-PIN-987654', Round);
  finally
    Settings.Free;
  end;

  // The file content must NOT contain the plaintext anywhere.
  RawIni := TStringList.Create;
  try
    RawIni.LoadFromFile(Path);
    RawText := RawIni.Text;
    Assert.IsTrue(Pos('1234-ABCDEF', RawText) = 0,
      'plaintext leaked into the on-disk file');
    Assert.IsTrue(Pos('BMW-PIN-987654', RawText) = 0,
      'plaintext leaked into the on-disk file');
  finally
    RawIni.Free;
    TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS_MISSING KEY RETURNS DEFAULT
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.Settings_MissingKeyReturnsDefault;
var
  Path: string;
  Settings: TOBDSecureSettings;
begin
  Path := ScratchPath('missing.ini');
  if TFile.Exists(Path) then TFile.Delete(Path);
  Settings := TOBDSecureSettings.Create(Path);
  try
    Assert.AreEqual('FALLBACK',
      Settings.ReadString('Nope', 'Nada', 'FALLBACK'));
  finally
    Settings.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS_READ STRING_FALLS BACK ON GARBAGE CIPHER
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.Settings_ReadString_FallsBackOnGarbageCipher;
var
  Path: string;
  Settings: TOBDSecureSettings;
  Raw: TStringList;
begin
  Path := ScratchPath('garbage.ini');
  if TFile.Exists(Path) then TFile.Delete(Path);

  // Write a value that's valid Base64 but isn't actually DPAPI-encrypted.
  Raw := TStringList.Create;
  try
    Raw.Add('[Dealer]');
    Raw.Add('Code=Tm90IGEgcmVhbCBjaXBoZXIgYmxvYg==');
    Raw.SaveToFile(Path);
  finally
    Raw.Free;
  end;

  Settings := TOBDSecureSettings.Create(Path);
  try
    // Decrypt fails internally; ReadString returns the supplied default
    // rather than raising.
    Assert.AreEqual('DEFAULT',
      Settings.ReadString('Dealer', 'Code', 'DEFAULT'));
  finally
    Settings.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS_DELETE KEY CLEARS VALUE
//------------------------------------------------------------------------------
procedure TSecureSettingsTests.Settings_DeleteKeyClearsValue;
var
  Path: string;
  Settings: TOBDSecureSettings;
begin
  Path := ScratchPath('del.ini');
  if TFile.Exists(Path) then TFile.Delete(Path);

  Settings := TOBDSecureSettings.Create(Path);
  try
    Settings.WriteString('S', 'K', 'value');
    Settings.Save;
    Assert.AreEqual('value', Settings.ReadString('S', 'K', ''));
    Settings.DeleteKey('S', 'K');
    Settings.Save;
    Assert.AreEqual('FALLBACK', Settings.ReadString('S', 'K', 'FALLBACK'));
  finally
    Settings.Free;
    if TFile.Exists(Path) then TFile.Delete(Path);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSecureSettingsTests);

end.
