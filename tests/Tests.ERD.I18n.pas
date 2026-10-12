// ------------------------------------------------------------------------------
// Tests.ERD.I18n
//
// Runtime localisation regressions for ERD.I18n.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  CLDR plural rules, LANGID mapping, all shipped languages.
// ------------------------------------------------------------------------------

unit Tests.ERD.I18n;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TI18nTests = class
  public
    [Test] procedure BuiltInEnglishFallbackWorks;
    [Test] procedure BaseLanguageFallbackWorks;
    [Test] procedure FormatsAndPluralsWork;
    [Test] procedure ChangeNotificationIsMulticast;
    [Test] procedure ShippedLanguagesLoad;
    [Test] procedure PluralCategoriesFollowCldr;
    [Test] procedure PluralFormsAreSelectedPerLanguage;
    [Test] procedure WindowsLanguageIdsMapToShippedLanguages;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  ERD.I18n;

type
  TListener = class
  public
    Calls: Integer;
    procedure Changed(Sender: TObject);
  end;

procedure TListener.Changed(Sender: TObject);
begin
  Inc(Calls);
end;

function FindLanguagesDir: string;
var
  Dir, Candidate: string;
  I: Integer;
begin
  Dir := TDirectory.GetCurrentDirectory;
  for I := 0 to 6 do
  begin
    Candidate := TPath.Combine(Dir, 'languages');
    if TDirectory.Exists(Candidate) and TFile.Exists(TPath.Combine(Candidate, 'en.json')) then
      Exit(Candidate);
    Dir := TPath.GetDirectoryName(Dir);
    if Dir = '' then
      Break;
  end;
  Result := '';
end;

procedure TI18nTests.BuiltInEnglishFallbackWorks;
var
  T: TOBDTranslator;
begin
  T := TOBDTranslator.Create;
  try
    Assert.AreEqual<string>('Dashboard', T.Tr('app.navigation.dashboard', ''));
    Assert.AreEqual<string>('Default text', T.Tr('ui.missing.key', 'Default text'));
  finally
    T.Free;
  end;
end;

procedure TI18nTests.BaseLanguageFallbackWorks;
var
  T: TOBDTranslator;
begin
  T := TOBDTranslator.Create;
  try
    T.LoadLanguageFromString('{"language":"nl","name":"Nederlands","englishName":"Dutch","version":1,"strings":{"ui.test.value":"Waarde"}}');
    T.CurrentLanguage := 'nl-BE';
    Assert.AreEqual<string>('Waarde', T.Tr('ui.test.value', 'Value'));
  finally
    T.Free;
  end;
end;

procedure TI18nTests.FormatsAndPluralsWork;
var
  T: TOBDTranslator;
begin
  T := TOBDTranslator.Create;
  try
    T.LoadLanguageFromString('{"language":"fr","name":"Francais","englishName":"French","version":1,"strings":{"ui.test.hello":"Bonjour %s","ui.test.count.plural":{"one":"%d code","other":"%d codes"}}}');
    T.CurrentLanguage := 'fr';
    Assert.AreEqual<string>('Bonjour ECU', T.TrFmt('ui.test.hello', 'Hello %s', ['ECU']));
    Assert.AreEqual<string>('%d code', T.TrPlural('ui.test.count', 1, '%d item', '%d items'));
    Assert.AreEqual<string>('%d codes', T.TrPlural('ui.test.count', 2, '%d item', '%d items'));
  finally
    T.Free;
  end;
end;

procedure TI18nTests.ChangeNotificationIsMulticast;
var
  T: TOBDTranslator;
  A, B: TListener;
begin
  T := TOBDTranslator.Create;
  A := TListener.Create;
  B := TListener.Create;
  try
    T.AddChangeListener(A.Changed);
    T.AddChangeListener(B.Changed);
    T.CurrentLanguage := 'de';
    Assert.AreEqual(1, A.Calls);
    Assert.AreEqual(1, B.Calls);
    T.RemoveChangeListener(A.Changed);
    T.CurrentLanguage := 'fr';
    Assert.AreEqual(1, A.Calls);
    Assert.AreEqual(2, B.Calls);
  finally
    B.Free;
    A.Free;
    T.Free;
  end;
end;

procedure TI18nTests.ShippedLanguagesLoad;
var
  T: TOBDTranslator;
  Dir, Code: string;
  Info: TArray<TOBDLanguageInfo>;
begin
  Dir := FindLanguagesDir;
  Assert.IsNotEmpty(Dir, 'languages directory not found');
  T := TOBDTranslator.Create;
  try
    for Code in OBD_LANGUAGE_CODES do
      T.LoadLanguageFile(TPath.Combine(Dir, Code + '.json'));
    T.CurrentLanguage := 'de';
    Assert.AreEqual<string>('Fehlercodes', T.Tr('ui.dtc.title', 'Diagnostic trouble codes'));
    Info := T.AvailableLanguages(Dir);
    Assert.AreEqual(Length(OBD_LANGUAGE_CODES), Length(Info));
  finally
    T.Free;
  end;
end;

procedure TI18nTests.PluralCategoriesFollowCldr;
begin
  Assert.IsTrue(OBDPluralCategory('en', 1) = pcOne);
  Assert.IsTrue(OBDPluralCategory('en', 0) = pcOther);
  Assert.IsTrue(OBDPluralCategory('fr', 0) = pcOne);
  Assert.IsTrue(OBDPluralCategory('fr-BE', 2) = pcOther);
  Assert.IsTrue(OBDPluralCategory('pl', 1) = pcOne);
  Assert.IsTrue(OBDPluralCategory('pl', 3) = pcFew);
  Assert.IsTrue(OBDPluralCategory('pl', 13) = pcMany);
  Assert.IsTrue(OBDPluralCategory('pl', 22) = pcFew);
  Assert.IsTrue(OBDPluralCategory('ru', 21) = pcOne);
  Assert.IsTrue(OBDPluralCategory('ru', 11) = pcMany);
  Assert.IsTrue(OBDPluralCategory('uk', 5) = pcMany);
  Assert.IsTrue(OBDPluralCategory('cs', 4) = pcFew);
  Assert.IsTrue(OBDPluralCategory('cs', 5) = pcOther);
  Assert.IsTrue(OBDPluralCategory('hr', 24) = pcFew);
  Assert.IsTrue(OBDPluralCategory('sr', 11) = pcOther);
  Assert.IsTrue(OBDPluralCategory('sl', 102) = pcTwo);
  Assert.IsTrue(OBDPluralCategory('sl', 3) = pcFew);
  Assert.IsTrue(OBDPluralCategory('lv', 0) = pcZero);
  Assert.IsTrue(OBDPluralCategory('lv', 21) = pcOne);
  Assert.IsTrue(OBDPluralCategory('lv', 11) = pcZero);
  Assert.IsTrue(OBDPluralCategory('lt', 2) = pcFew);
  Assert.IsTrue(OBDPluralCategory('lt', 12) = pcOther);
  Assert.IsTrue(OBDPluralCategory('ro', 0) = pcFew);
  Assert.IsTrue(OBDPluralCategory('ro', 20) = pcOther);
  Assert.IsTrue(OBDPluralCategory('ga', 2) = pcTwo);
  Assert.IsTrue(OBDPluralCategory('ga', 7) = pcMany);
  Assert.IsTrue(OBDPluralCategory('ga', 11) = pcOther);
  Assert.IsTrue(OBDPluralCategory('mt', 0) = pcFew);
  Assert.IsTrue(OBDPluralCategory('mt', 15) = pcMany);
  Assert.IsTrue(OBDPluralCategory('is', 21) = pcOne);
  Assert.IsTrue(OBDPluralCategory('is', 11) = pcOther);
end;

procedure TI18nTests.PluralFormsAreSelectedPerLanguage;
var
  T: TOBDTranslator;
begin
  T := TOBDTranslator.Create;
  try
    T.LoadLanguageFromString('{"language":"pl","name":"Polski","englishName":"Polish","version":1,"strings":{"ui.test.count.plural":{"one":"%d kod","few":"%d kody","many":"%d kodow","other":"%d kodu"}}}');
    T.CurrentLanguage := 'pl';
    Assert.AreEqual<string>('%d kod', T.TrPlural('ui.test.count', 1, '', ''));
    Assert.AreEqual<string>('%d kody', T.TrPlural('ui.test.count', 2, '', ''));
    Assert.AreEqual<string>('%d kodow', T.TrPlural('ui.test.count', 5, '', ''));
    T.LoadLanguageFromString('{"language":"cs","name":"Cestina","englishName":"Czech","version":1,"strings":{"ui.test.count.plural":{"one":"%d kod","other":"%d kodu"}}}');
    T.CurrentLanguage := 'cs';
    // A missing "few" form falls back to "other".
    Assert.AreEqual<string>('%d kodu', T.TrPlural('ui.test.count', 3, '', ''));
  finally
    T.Free;
  end;
end;

procedure TI18nTests.WindowsLanguageIdsMapToShippedLanguages;
begin
  Assert.AreEqual<string>('nl', OBDLanguageCodeFromLangId($0413));
  Assert.AreEqual<string>('nl', OBDLanguageCodeFromLangId($0813));
  Assert.AreEqual<string>('nb', OBDLanguageCodeFromLangId($0414));
  Assert.AreEqual<string>('nn', OBDLanguageCodeFromLangId($0814));
  Assert.AreEqual<string>('hr', OBDLanguageCodeFromLangId($041A));
  Assert.AreEqual<string>('sr', OBDLanguageCodeFromLangId($0C1A));
  Assert.AreEqual<string>('bs', OBDLanguageCodeFromLangId($141A));
  Assert.AreEqual<string>('lb', OBDLanguageCodeFromLangId($046E));
  Assert.AreEqual<string>('en', OBDLanguageCodeFromLangId($0411));
  Assert.IsTrue(OBDIsSupportedLanguage('pt-PT'));
  Assert.IsFalse(OBDIsSupportedLanguage('ja'));
end;

initialization
  TDUnitX.RegisterTestFixture(TI18nTests);

end.
