// ------------------------------------------------------------------------------
// ERD.I18n
//
// Runtime translation catalogue and helpers for Delphi ERD UI and services.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  Built-in English texts for TERDHistogram.
// 2026-10-12  ERD  CLDR plural categories, all European languages,
//                  catalogue status.
// ------------------------------------------------------------------------------

unit ERD.Components.I18n;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.JSON,
  System.IOUtils,
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  System.Types;

function ParseERDJSONObject(const Text: string): TJSONObject;
var Value: TJSONValue;
begin
  Value := TJSONObject.ParseJSONValue(Text);
  if not (Value is TJSONObject) then
  begin
    Value.Free;
    raise EConvertError.Create('Expected a JSON object');
  end;
  Result := TJSONObject(Value);
end;

type
  /// <summary>Metadata for a language catalogue discovered on disk.</summary>
  TERDLanguageInfo = record
    /// <summary>BCP-47-ish language code, for example <c>en</c> or <c>nl</c>.</summary>
    Code: string;
    /// <summary>Native language name shown in Settings.</summary>
    Name: string;
    /// <summary>English language name used for diagnostics and fallback UIs.</summary>
    EnglishName: string;
    /// <summary>Catalogue file path.</summary>
    FileName: string;
    /// <summary>Review status from the catalogue: <c>draft</c> (not yet
    /// reviewed by a native speaker), <c>reviewed</c>, or empty.</summary>
    Status: string;
  end;

  /// <summary>CLDR cardinal plural category.</summary>
  TERDPluralCategory = (pcZero, pcOne, pcTwo, pcFew, pcMany, pcOther);

  /// <summary>Plural strings from an i18n catalogue, one per CLDR
  /// category. Empty categories fall back to <c>OtherText</c>.</summary>
  TERDPluralText = record
    /// <summary>Text used when the count is one.</summary>
    OneText: string;
    /// <summary>Text used for every other count.</summary>
    OtherText: string;
    /// <summary>CLDR "zero" form (Latvian).</summary>
    ZeroText: string;
    /// <summary>CLDR "two" form (Slovenian, Irish, Maltese).</summary>
    TwoText: string;
    /// <summary>CLDR "few" form (Slavic, Baltic, Romanian, Irish,
    /// Maltese).</summary>
    FewText: string;
    /// <summary>CLDR "many" form (Polish, Russian, Ukrainian,
    /// Belarusian, Irish, Maltese).</summary>
    ManyText: string;
  end;

  /// <summary>
  /// Process-wide, thread-safe translation catalogue with English built-ins.
  /// </summary>
  TERDTranslator = class
  strict private
    type
      TLanguageCatalog = class
      strict private
        FLanguage: string;
        FName: string;
        FEnglishName: string;
        FVersion: Integer;
        FStrings: TDictionary<string, string>;
        FPlurals: TDictionary<string, TERDPluralText>;
      public
        constructor Create;
        destructor Destroy; override;
        procedure AddString(const AKey, AValue: string);
        procedure AddPlural(const AKey: string; const AText: TERDPluralText);
        function TryString(const AKey: string; out AValue: string): Boolean;
        function TryPlural(const AKey: string; out AValue: TERDPluralText): Boolean;
        property Language: string read FLanguage write FLanguage;
        property Name: string read FName write FName;
        property EnglishName: string read FEnglishName write FEnglishName;
        property Version: Integer read FVersion write FVersion;
      end;
  strict private
    FLock: TCriticalSection;
    FCatalogs: TObjectDictionary<string, TLanguageCatalog>;
    FCurrentLanguage: string;
    FListeners: TArray<TNotifyEvent>;
    function GetCurrentLanguage: string;
    procedure SetCurrentLanguage(const AValue: string);
    procedure LoadCatalog(ACatalog: TLanguageCatalog);
    function FindStringLocked(const ALanguage, AKey: string; out AValue: string): Boolean;
    function FindPluralLocked(const ALanguage, AKey: string; out AValue: TERDPluralText): Boolean;
    procedure NotifyChanged;
    procedure LoadBuiltInEnglish;
  public
    /// <summary>Creates a translator and loads built-in English strings.</summary>
    constructor Create;
    /// <summary>Releases catalogues and listener state.</summary>
    destructor Destroy; override;

    /// <summary>Current language code used by lookups.</summary>
    property CurrentLanguage: string read GetCurrentLanguage write SetCurrentLanguage;

    /// <summary>Loads a JSON language catalogue from disk.</summary>
    procedure LoadLanguageFile(const AFileName: string);
    /// <summary>Loads a JSON language catalogue from a UTF-8 string.</summary>
    procedure LoadLanguageFromString(const AText: string);
    /// <summary>Loads an RCDATA language catalogue resource on Windows.</summary>
    procedure LoadFromResource(const AResourceName: string);
    /// <summary>Scans a folder for JSON language catalogues.</summary>
    function AvailableLanguages(const AFolder: string): TArray<TERDLanguageInfo>;

    /// <summary>Looks up a translation with requested-base-English-default fallback.</summary>
    function Tr(const AKey, ADefault: string): string;
    /// <summary>Looks up and formats a translation using Delphi Format syntax.</summary>
    function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
    /// <summary>Looks up a plural translation and picks the CLDR plural
    /// form of the language that supplied it.</summary>
    function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
      AOtherDefault: string): string;
    /// <summary>Registers a callback fired after language data changes.</summary>
    procedure AddChangeListener(AListener: TNotifyEvent);
    /// <summary>Unregisters a previously registered callback.</summary>
    procedure RemoveChangeListener(AListener: TNotifyEvent);
  end;

/// <summary>Returns the global translator singleton.</summary>
function ERDI18n: TERDTranslator;
/// <summary>Convenience wrapper around <see cref="TERDTranslator.Tr"/>.</summary>
function Tr(const AKey, ADefault: string): string;
/// <summary>Convenience wrapper around <see cref="TERDTranslator.TrFmt"/>.</summary>
function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
/// <summary>Convenience wrapper around <see cref="TERDTranslator.TrPlural"/>.</summary>
function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
  AOtherDefault: string): string;
/// <summary>Returns the operating-system UI language when it is supported.</summary>
function ERDDefaultLanguageCode: string;
/// <summary>Maps a Windows LANGID to a supported language code, or
/// <c>en</c>.</summary>
function ERDLanguageCodeFromLangId(ALangId: Word): string;
/// <summary>Returns <c>True</c> for a code in
/// <see cref="ERD_LANGUAGE_CODES"/>.</summary>
function ERDIsSupportedLanguage(const ACode: string): Boolean;
/// <summary>CLDR cardinal plural category of a whole number in a
/// language (the base language of <c>ALanguage</c> is used).</summary>
function ERDPluralCategory(const ALanguage: string; ACount: Int64): TERDPluralCategory;

const
  /// <summary>Languages shipped in <c>languages/</c>: the official
  /// national languages of the European countries.</summary>
  ERD_LANGUAGE_CODES: array [0 .. 37] of string = (
    'be', 'bg', 'bs', 'ca', 'cs', 'da', 'de', 'el', 'en', 'es', 'et', 'fi',
    'fr', 'ga', 'hr', 'hu', 'is', 'it', 'lb', 'lt', 'lv', 'mk', 'mt', 'nb',
    'nl', 'nn', 'pl', 'pt', 'rm', 'ro', 'ru', 'sk', 'sl', 'sq', 'sr', 'sv',
    'tr', 'uk');

implementation

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}
{$IFDEF MSWINDOWS}
  , {$IFDEF FPC}Windows{$ELSE}Winapi.Windows{$ENDIF}
{$ENDIF}
  ;

type
  TERDBuiltInString = record
    Key: string;
    Value: string;
  end;

  TERDBuiltInPlural = record
    Key: string;
    OneText: string;
    OtherText: string;
  end;

const
  BUILTIN_ENGLISH_STRINGS: array [0 .. 13] of TERDBuiltInString = (
    (Key: 'status.noData'; Value: 'No data'),
    (Key: 'ui.histogram.marker.mean'; Value: 'mean'),
    (Key: 'ui.histogram.marker.median'; Value: 'median'),
    (Key: 'ui.histogram.marker.now'; Value: 'now'),
    (Key: 'ui.histogram.noSamples'; Value: 'No samples'),
    (Key: 'ui.histogram.summary'; Value: '%d samples | mean %s | min %s | max %s'),
    (Key: 'ui.histogram.title'; Value: 'Histogram'),
    (Key: 'ui.inspector.boolean.off'; Value: 'Off'),
    (Key: 'ui.inspector.boolean.on'; Value: 'On'),
    (Key: 'ui.status.preview.ready'; Value: 'Ready'),
    (Key: 'ui.status.preview.document'; Value: 'Untitled document'),
    (Key: 'ui.status.preview.selection'; Value: 'Selection: 0 items'),
    (Key: 'ui.status.preview.processing'; Value: 'Processing'),
    (Key: 'ui.status.preview.help'; Value: 'Help'));

  BUILTIN_ENGLISH_PLURALS: array [0 .. 0] of TERDBuiltInPlural = (
    (Key: 'ui.find.matches.plural'; OneText: '%d match'; OtherText: '%d matches'));

var
  GTranslator: TERDTranslator = nil;
  GTranslatorLock: TCriticalSection = nil;

function NormaliseLanguageCode(const ACode: string): string;
var
  S: string;
  P: Integer;
begin
  S := LowerCase(Trim(ACode));
  S := StringReplace(S, '_', '-', [rfReplaceAll]);
  P := Pos('.', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  P := Pos('@', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  if S = '' then
    S := 'en';
  Result := S;
end;

function BaseLanguage(const ACode: string): string;
var
  P: Integer;
begin
  Result := NormaliseLanguageCode(ACode);
  P := Pos('-', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
end;

function IsOneOf(const AValue: string; const AList: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(AList) to High(AList) do
    if AValue = AList[I] then
      Exit(True);
  Result := False;
end;

function PluralTextFor(const AText: TERDPluralText;
  ACategory: TERDPluralCategory): string;
begin
  case ACategory of
    pcZero:
      Result := AText.ZeroText;
    pcOne:
      Result := AText.OneText;
    pcTwo:
      Result := AText.TwoText;
    pcFew:
      Result := AText.FewText;
    pcMany:
      Result := AText.ManyText;
  else
    Result := '';
  end;
  if Result = '' then
    Result := AText.OtherText;
end;

function SameListener(A, B: TNotifyEvent): Boolean;
begin
  Result := (TMethod(A).Code = TMethod(B).Code) and
    (TMethod(A).Data = TMethod(B).Data);
end;

constructor TERDTranslator.TLanguageCatalog.Create;
begin
  inherited Create;
  FStrings := TDictionary<string, string>.Create;
  FPlurals := TDictionary<string, TERDPluralText>.Create;
  Version := 1;
end;

destructor TERDTranslator.TLanguageCatalog.Destroy;
begin
  FPlurals.Free;
  FStrings.Free;
  inherited;
end;

procedure TERDTranslator.TLanguageCatalog.AddString(const AKey, AValue: string);
begin
  FStrings.AddOrSetValue(AKey, AValue);
end;

procedure TERDTranslator.TLanguageCatalog.AddPlural(const AKey: string;
  const AText: TERDPluralText);
begin
  FPlurals.AddOrSetValue(AKey, AText);
end;

function TERDTranslator.TLanguageCatalog.TryString(const AKey: string;
  out AValue: string): Boolean;
begin
  Result := FStrings.TryGetValue(AKey, AValue);
end;

function TERDTranslator.TLanguageCatalog.TryPlural(const AKey: string;
  out AValue: TERDPluralText): Boolean;
begin
  Result := FPlurals.TryGetValue(AKey, AValue);
end;

constructor TERDTranslator.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCatalogs := TObjectDictionary<string, TLanguageCatalog>.Create([doOwnsValues]);
  FCurrentLanguage := 'en';
  LoadBuiltInEnglish;
end;

destructor TERDTranslator.Destroy;
begin
  FCatalogs.Free;
  FLock.Free;
  inherited;
end;

procedure TERDTranslator.LoadBuiltInEnglish;
var
  Catalog: TLanguageCatalog;
  Plural: TERDPluralText;
  I: Integer;
begin
  Catalog := TLanguageCatalog.Create;
  try
    Catalog.Language := 'en';
    Catalog.Name := 'English';
    Catalog.EnglishName := 'English';
    for I := Low(BUILTIN_ENGLISH_STRINGS) to High(BUILTIN_ENGLISH_STRINGS) do
      Catalog.AddString(BUILTIN_ENGLISH_STRINGS[I].Key,
        BUILTIN_ENGLISH_STRINGS[I].Value);
    for I := Low(BUILTIN_ENGLISH_PLURALS) to High(BUILTIN_ENGLISH_PLURALS) do
    begin
      Plural := Default(TERDPluralText);
      Plural.OneText := BUILTIN_ENGLISH_PLURALS[I].OneText;
      Plural.OtherText := BUILTIN_ENGLISH_PLURALS[I].OtherText;
      Catalog.AddPlural(BUILTIN_ENGLISH_PLURALS[I].Key, Plural);
    end;
    LoadCatalog(Catalog);
    Catalog := nil;
  finally
    Catalog.Free;
  end;
end;

function TERDTranslator.GetCurrentLanguage: string;
begin
  FLock.Enter;
  try
    Result := FCurrentLanguage;
  finally
    FLock.Leave;
  end;
end;

procedure TERDTranslator.SetCurrentLanguage(const AValue: string);
var
  NewCode: string;
begin
  NewCode := NormaliseLanguageCode(AValue);
  FLock.Enter;
  try
    if FCurrentLanguage = NewCode then
      Exit;
    FCurrentLanguage := NewCode;
  finally
    FLock.Leave;
  end;
  NotifyChanged;
end;

procedure TERDTranslator.LoadCatalog(ACatalog: TLanguageCatalog);
var
  Code: string;
begin
  Code := NormaliseLanguageCode(ACatalog.Language);
  ACatalog.Language := Code;
  FLock.Enter;
  try
    FCatalogs.AddOrSetValue(Code, ACatalog);
  finally
    FLock.Leave;
  end;
end;

procedure TERDTranslator.LoadLanguageFile(const AFileName: string);
begin
  if not TFile.Exists(AFileName) then
    raise Exception.Create('Language file not found: ' + AFileName);
  LoadLanguageFromString(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

procedure TERDTranslator.LoadLanguageFromString(const AText: string);
var
  Root, StringsObj, PluralObj: TJSONObject;
  Pair: TJSONPair;
  Plural: TERDPluralText;
  Catalog: TLanguageCatalog;
  CurrentCode: string;
  Changed: Boolean;
begin
  Root := ParseERDJSONObject(AText);
  Catalog := TLanguageCatalog.Create;
  try
    Catalog.Language := Root.GetValue<string>('language', 'en');
    Catalog.Name := Root.GetValue<string>('name', Catalog.Language);
    Catalog.EnglishName := Root.GetValue<string>('englishName', Catalog.Name);
    Catalog.Version := Root.GetValue<Integer>('version', 1);
    StringsObj := Root.GetValue<TJSONObject>('strings', nil);
    if StringsObj = nil then
      raise Exception.Create('Language JSON: strings object missing');
    for Pair in StringsObj do
    begin
      if Pair.JsonValue is TJSONString then
        Catalog.AddString(Pair.JsonString.Value, TJSONString(Pair.JsonValue).Value)
      else if Pair.JsonValue is TJSONObject then
      begin
        PluralObj := TJSONObject(Pair.JsonValue);
        Plural.ZeroText := PluralObj.GetValue<string>('zero', '');
        Plural.OneText := PluralObj.GetValue<string>('one', '');
        Plural.TwoText := PluralObj.GetValue<string>('two', '');
        Plural.FewText := PluralObj.GetValue<string>('few', '');
        Plural.ManyText := PluralObj.GetValue<string>('many', '');
        Plural.OtherText := PluralObj.GetValue<string>('other', '');
        Catalog.AddPlural(Pair.JsonString.Value, Plural);
      end
      else
        raise Exception.Create('Language JSON: string or plural object expected for ' +
          Pair.JsonString.Value);
    end;
    CurrentCode := CurrentLanguage;
    Changed := (NormaliseLanguageCode(Catalog.Language) = CurrentCode) or
      (BaseLanguage(CurrentCode) = NormaliseLanguageCode(Catalog.Language));
    LoadCatalog(Catalog);
    Catalog := nil;
  finally
    Catalog.Free;
    Root.Free;
  end;
  if Changed then
    NotifyChanged;
end;

procedure TERDTranslator.LoadFromResource(const AResourceName: string);
{$IFDEF MSWINDOWS}
var
  Stream: TResourceStream;
  Bytes: TBytes;
begin
  Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Bytes, Stream.Size);
    if Length(Bytes) > 0 then
      Stream.ReadBuffer(Bytes[0], Length(Bytes));
    LoadLanguageFromString(TEncoding.UTF8.GetString(Bytes));
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  raise Exception.Create('Resource language loading is only available on Windows');
end;
{$ENDIF}

function TERDTranslator.AvailableLanguages(const AFolder: string)
  : TArray<TERDLanguageInfo>;
var
  Files: TArray<string>;
  FileName: string;
  Root: TJSONObject;
  Info: TERDLanguageInfo;
  Acc: TList<TERDLanguageInfo>;
begin
  Acc := TList<TERDLanguageInfo>.Create;
  try
    if TDirectory.Exists(AFolder) then
    begin
      Files := TDirectory.GetFiles(AFolder, '*.json');
      for FileName in Files do
      begin
        Root := ParseERDJSONObject(TFile.ReadAllText(FileName, TEncoding.UTF8));
        try
          Info.Code := NormaliseLanguageCode(Root.GetValue<string>('language', ''));
          Info.Name := Root.GetValue<string>('name', Info.Code);
          Info.EnglishName := Root.GetValue<string>('englishName', Info.Name);
          Info.FileName := FileName;
          Info.Status := Root.GetValue<string>('status', '');
          Acc.Add(Info);
        finally
          Root.Free;
        end;
      end;
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function TERDTranslator.FindStringLocked(const ALanguage, AKey: string;
  out AValue: string): Boolean;
var
  Catalog: TLanguageCatalog;
begin
  Result := FCatalogs.TryGetValue(NormaliseLanguageCode(ALanguage), Catalog) and
    Catalog.TryString(AKey, AValue);
end;

function TERDTranslator.FindPluralLocked(const ALanguage, AKey: string;
  out AValue: TERDPluralText): Boolean;
var
  Catalog: TLanguageCatalog;
begin
  Result := FCatalogs.TryGetValue(NormaliseLanguageCode(ALanguage), Catalog) and
    Catalog.TryPlural(AKey, AValue);
end;

function TERDTranslator.Tr(const AKey, ADefault: string): string;
var
  Code, Base: string;
begin
  FLock.Enter;
  try
    Code := FCurrentLanguage;
    if FindStringLocked(Code, AKey, Result) then
      Exit;
    Base := BaseLanguage(Code);
    if (Base <> Code) and FindStringLocked(Base, AKey, Result) then
      Exit;
    if FindStringLocked('en', AKey, Result) then
      Exit;
    Result := ADefault;
  finally
    FLock.Leave;
  end;
end;

function TERDTranslator.TrFmt(const AKey, ADefault: string;
  const AArgs: array of const): string;
begin
  Result := Format(Tr(AKey, ADefault), AArgs);
end;

function TERDTranslator.TrPlural(const AKey: string; ACount: Int64;
  const AOneDefault, AOtherDefault: string): string;
var
  Code, Base, PluralKey: string;
  Text: TERDPluralText;
begin
  PluralKey := AKey;
  if not SameText(Copy(PluralKey, Length(PluralKey) - 6, 7), '.plural') then
    PluralKey := AKey + '.plural';
  FLock.Enter;
  try
    Code := FCurrentLanguage;
    if FindPluralLocked(Code, PluralKey, Text) then
      Exit(PluralTextFor(Text, ERDPluralCategory(Code, ACount)));
    Base := BaseLanguage(Code);
    if (Base <> Code) and FindPluralLocked(Base, PluralKey, Text) then
      Exit(PluralTextFor(Text, ERDPluralCategory(Base, ACount)));
    if FindPluralLocked('en', PluralKey, Text) then
      Exit(PluralTextFor(Text, ERDPluralCategory('en', ACount)));
    if ACount = 1 then
      Result := AOneDefault
    else
      Result := AOtherDefault;
  finally
    FLock.Leave;
  end;
end;

procedure TERDTranslator.AddChangeListener(AListener: TNotifyEvent);
var
  I: Integer;
begin
  if not Assigned(AListener) then
    Exit;
  FLock.Enter;
  try
    for I := 0 to High(FListeners) do
      if SameListener(FListeners[I], AListener) then
        Exit;
    SetLength(FListeners, Length(FListeners) + 1);
    FListeners[High(FListeners)] := AListener;
  finally
    FLock.Leave;
  end;
end;

procedure TERDTranslator.RemoveChangeListener(AListener: TNotifyEvent);
var
  I, J: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to High(FListeners) do
      if SameListener(FListeners[I], AListener) then
      begin
        for J := I to High(FListeners) - 1 do
          FListeners[J] := FListeners[J + 1];
        SetLength(FListeners, Length(FListeners) - 1);
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TERDTranslator.NotifyChanged;
var
  Snapshot: TArray<TNotifyEvent>;
  I: Integer;
begin
  FLock.Enter;
  try
    Snapshot := Copy(FListeners);
  finally
    FLock.Leave;
  end;
  for I := 0 to High(Snapshot) do
    if Assigned(Snapshot[I]) then
      Snapshot[I](Self);
end;

function ERDI18n: TERDTranslator;
begin
  if GTranslatorLock = nil then
    GTranslatorLock := TCriticalSection.Create;
  GTranslatorLock.Enter;
  try
    if GTranslator = nil then
      GTranslator := TERDTranslator.Create;
    Result := GTranslator;
  finally
    GTranslatorLock.Leave;
  end;
end;

function Tr(const AKey, ADefault: string): string;
begin
  Result := ERDI18n.Tr(AKey, ADefault);
end;

function TrFmt(const AKey, ADefault: string; const AArgs: array of const): string;
begin
  Result := ERDI18n.TrFmt(AKey, ADefault, AArgs);
end;

function TrPlural(const AKey: string; ACount: Int64; const AOneDefault,
  AOtherDefault: string): string;
begin
  Result := ERDI18n.TrPlural(AKey, ACount, AOneDefault, AOtherDefault);
end;

function ERDIsSupportedLanguage(const ACode: string): Boolean;
begin
  Result := IsOneOf(NormaliseLanguageCode(ACode), ERD_LANGUAGE_CODES);
end;

function ERDLanguageCodeFromLangId(ALangId: Word): string;
var
  Primary, Sub: Word;
begin
  Primary := ALangId and $03FF;
  Sub := ALangId shr 10;
  case Primary of
    $02: Result := 'bg';
    $03: Result := 'ca';
    $05: Result := 'cs';
    $06: Result := 'da';
    $07: Result := 'de';
    $08: Result := 'el';
    $0A: Result := 'es';
    $0B: Result := 'fi';
    $0C: Result := 'fr';
    $0E: Result := 'hu';
    $0F: Result := 'is';
    $10: Result := 'it';
    $13: Result := 'nl';
    $14:
      // SUBLANG_NORWEGIAN_NYNORSK = 2, otherwise Bokmal.
      if Sub = $02 then
        Result := 'nn'
      else
        Result := 'nb';
    $15: Result := 'pl';
    $16: Result := 'pt';
    $17: Result := 'rm';
    $18: Result := 'ro';
    $19: Result := 'ru';
    $1A:
      // Croatian (1, 4), Bosnian (5, 8), Serbian and Montenegrin (rest).
      case Sub of
        $01, $04: Result := 'hr';
        $05, $08: Result := 'bs';
      else
        Result := 'sr';
      end;
    $1B: Result := 'sk';
    $1C: Result := 'sq';
    $1D: Result := 'sv';
    $1F: Result := 'tr';
    $22: Result := 'uk';
    $23: Result := 'be';
    $24: Result := 'sl';
    $25: Result := 'et';
    $26: Result := 'lv';
    $27: Result := 'lt';
    $2F: Result := 'mk';
    $3A: Result := 'mt';
    $3C: Result := 'ga';
    $6E: Result := 'lb';
  else
    Result := 'en';
  end;
end;

function ERDPluralCategory(const ALanguage: string; ACount: Int64): TERDPluralCategory;
var
  Lang: string;
  N: Int64;
  I10, I100: Integer;
begin
  Lang := BaseLanguage(ALanguage);
  N := Abs(ACount);
  I10 := Integer(N mod 10);
  I100 := Integer(N mod 100);
  Result := pcOther;
  if Lang = 'fr' then
  begin
    if N <= 1 then
      Result := pcOne;
  end
  else if IsOneOf(Lang, ['is', 'mk']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne;
  end
  else if Lang = 'lv' then
  begin
    if (I10 = 0) or ((I100 >= 11) and (I100 <= 19)) then
      Result := pcZero
    else if I10 = 1 then
      Result := pcOne;
  end
  else if Lang = 'lt' then
  begin
    if (I100 >= 11) and (I100 <= 19) then
      Result := pcOther
    else if I10 = 1 then
      Result := pcOne
    else if I10 >= 2 then
      Result := pcFew;
  end
  else if Lang = 'pl' then
  begin
    if N = 1 then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew
    else
      Result := pcMany;
  end
  else if IsOneOf(Lang, ['ru', 'uk', 'be']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew
    else
      Result := pcMany;
  end
  else if IsOneOf(Lang, ['cs', 'sk']) then
  begin
    if N = 1 then
      Result := pcOne
    else if (N >= 2) and (N <= 4) then
      Result := pcFew;
  end
  else if IsOneOf(Lang, ['hr', 'sr', 'bs']) then
  begin
    if (I10 = 1) and (I100 <> 11) then
      Result := pcOne
    else if (I10 >= 2) and (I10 <= 4) and not ((I100 >= 12) and (I100 <= 14)) then
      Result := pcFew;
  end
  else if Lang = 'sl' then
  begin
    if I100 = 1 then
      Result := pcOne
    else if I100 = 2 then
      Result := pcTwo
    else if (I100 = 3) or (I100 = 4) then
      Result := pcFew;
  end
  else if Lang = 'ro' then
  begin
    if N = 1 then
      Result := pcOne
    else if (N = 0) or ((I100 >= 1) and (I100 <= 19)) then
      Result := pcFew;
  end
  else if Lang = 'ga' then
  begin
    if N = 1 then
      Result := pcOne
    else if N = 2 then
      Result := pcTwo
    else if (N >= 3) and (N <= 6) then
      Result := pcFew
    else if (N >= 7) and (N <= 10) then
      Result := pcMany;
  end
  else if Lang = 'mt' then
  begin
    if N = 1 then
      Result := pcOne
    else if N = 2 then
      Result := pcTwo
    else if (N = 0) or ((I100 >= 3) and (I100 <= 10)) then
      Result := pcFew
    else if (I100 >= 11) and (I100 <= 19) then
      Result := pcMany;
  end
  else if N = 1 then
    Result := pcOne;
end;

function ERDDefaultLanguageCode: string;
{$IFDEF MSWINDOWS}
begin
  Result := ERDLanguageCodeFromLangId(GetUserDefaultUILanguage);
end;
{$ELSE}
var
  Env: string;
begin
  Env := GetEnvironmentVariable('LC_ALL');
  if Env = '' then
    Env := GetEnvironmentVariable('LC_MESSAGES');
  if Env = '' then
    Env := GetEnvironmentVariable('LANG');
  Env := BaseLanguage(Env);
  if Env = 'no' then
    Env := 'nb'
  else if Env = 'sh' then
    Env := 'sr';
  if ERDIsSupportedLanguage(Env) then
    Result := Env
  else
    Result := 'en';
end;
{$ENDIF}

initialization
  GTranslatorLock := TCriticalSection.Create;

finalization
  FreeAndNil(GTranslator);
  FreeAndNil(GTranslatorLock);

end.
