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
    for Code in TArray<string>.Create('en', 'nl', 'de', 'fr') do
      T.LoadLanguageFile(TPath.Combine(Dir, Code + '.json'));
    T.CurrentLanguage := 'de';
    Assert.AreEqual<string>('Fehlercodes', T.Tr('ui.dtc.title', 'Diagnostic trouble codes'));
    Info := T.AvailableLanguages(Dir);
    Assert.AreEqual(4, Length(Info));
  finally
    T.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TI18nTests);

end.
