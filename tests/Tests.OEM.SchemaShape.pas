//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.SchemaShape
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Per-field type/format validation against the
//                  v2 catalog schema. Complements the structural
//                  checks in Tests.OEM.CatalogIntegrity:
//                    * Tests.OEM.CatalogIntegrity asserts the
//                      catalog is *internally consistent* (no
//                      duplicate keys, fields fit inside payloads,
//                      ECU references resolve).
//                    * This unit asserts each leaf field carries
//                      a valid value (WMI codes are 3 chars
//                      alphanumeric, DTC codes are 5 chars
//                      P/C/B/U + 4 hex, decoder.kind is one of
//                      the recognised tags, etc.).
//                  Together they cover what a JSON-Schema runtime
//                  validator would catch at load time. See
//                  catalogs/_schema/oem-catalog-v2.json for the
//                  spec these tests track.
//------------------------------------------------------------------------------
unit Tests.OEM.SchemaShape;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSchemaShapeTests = class
  public
    [Test] procedure WMICodesAreThreeCharsAlphanumeric;
    [Test] procedure DecoderKindsUseRecognisedTags;
    [Test] procedure CodingFieldKindsUseRecognisedTags;
    [Test] procedure AdaptationKindsUseRecognisedTags;
    [Test] procedure DTCCodesMatchSAEorJ1939OrOemFormat;
    [Test] procedure ManufacturerKeysAreNonEmpty;
    [Test] procedure VersionFieldIsOneOrTwo;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Classes, System.JSON,
  System.Generics.Collections, System.RegularExpressions,
  OBD.OEM.Catalog.Loader;

//------------------------------------------------------------------------------
// Helpers
//------------------------------------------------------------------------------

function CatalogsRoot: string;
var
  Candidate: string;
begin
  Candidate := TPath.Combine(GetCurrentDir, 'catalogs');
  if TDirectory.Exists(Candidate) then Exit(Candidate);
  Candidate := TPath.Combine(TPath.Combine(GetCurrentDir, '..'), 'catalogs');
  if TDirectory.Exists(Candidate) then Exit(TPath.GetFullPath(Candidate));
  Result := '';
end;

function CollectAllJson(const Root: string): TArray<string>;
var
  All: TArray<string>;
  Path, Name, Dir: string;
  Acc: TList<string>;
begin
  Acc := TList<string>.Create;
  try
    All := TDirectory.GetFiles(
      Root, '*.json', TSearchOption.soAllDirectories);
    for Path in All do
    begin
      Name := TPath.GetFileName(Path);
      if Name.StartsWith('test-', True) then Continue;
      Dir := TPath.GetDirectoryName(Path);
      if Dir.EndsWith(PathDelim + '_schema') then Continue;
      Acc.Add(Path);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function IsOemCatalog(const Path: string): Boolean;
var
  Name: string;
begin
  Name := TPath.GetFileName(Path);
  Result := not (Name.StartsWith('dtc-', True) or
                 Name.StartsWith('iso-', True) or
                 Name.StartsWith('uds-', True) or
                 Name.StartsWith('obd2-', True));
end;

function IsDtcCatalog(const Path: string): Boolean;
begin
  Result := TPath.GetFileName(Path).StartsWith('dtc-', True);
end;

function ParseJsonObj(const Path: string): TJSONObject;
var
  V: TJSONValue;
begin
  V := TJSONObject.ParseJSONValue(TFile.ReadAllText(Path, TEncoding.UTF8));
  if V is TJSONObject then
    Result := TJSONObject(V)
  else
  begin
    V.Free;
    Result := nil;
  end;
end;

//------------------------------------------------------------------------------
// Tests
//------------------------------------------------------------------------------

procedure TSchemaShapeTests.WMICodesAreThreeCharsAlphanumeric;
const
  WmiPattern = '^[A-Z0-9]{3}$';
var
  Root, Path, Wmi: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectAllJson(Root);
  for Path in Catalogs do
  begin
    if not IsOemCatalog(Path) then Continue;
    Doc := ParseJsonObj(Path);
    if Doc = nil then Continue;
    try
      Arr := Doc.GetValue<TJSONArray>('applicable_wmis');
      if Arr = nil then Continue;
      for I := 0 to Arr.Count - 1 do
      begin
        if not (Arr.Items[I] is TJSONString) then Continue;
        Wmi := TJSONString(Arr.Items[I]).Value;
        Assert.IsTrue(TRegEx.IsMatch(Wmi, WmiPattern),
          Format('%s: applicable_wmis[%d] = %s does not match %s',
                  [TPath.GetFileName(Path), I, Wmi, WmiPattern]));
      end;
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSchemaShapeTests.DecoderKindsUseRecognisedTags;
const
  RecognisedKinds: array[0..13] of string = (
    'ascii', 'hex', 'uint8', 'uint16_be', 'uint32_be',
    'int8', 'int16_be', 'int32_be', 'bool', 'bytes',
    'enum', 'bitmask', 'bcd_date', 'seconds');
var
  Root, Path, Kind: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  DidsArr, LpArr, DxArr: TJSONArray;
  I: Integer;
  DidObj, Decoder: TJSONObject;
  KnownSet: TList<string>;
  S: string;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  KnownSet := TList<string>.Create;
  try
    for S in RecognisedKinds do KnownSet.Add(S);
    Catalogs := CollectAllJson(Root);
    for Path in Catalogs do
    begin
      if not IsOemCatalog(Path) then Continue;
      Doc := ParseJsonObj(Path);
      if Doc = nil then Continue;
      try
        for DidsArr in TArray<TJSONArray>.Create(
          Doc.GetValue<TJSONArray>('dids'),
          Doc.GetValue<TJSONArray>('live_pids'),
          Doc.GetValue<TJSONArray>('dtc_extended_data')) do
        begin
          if DidsArr = nil then Continue;
          for I := 0 to DidsArr.Count - 1 do
          begin
            if not (DidsArr.Items[I] is TJSONObject) then Continue;
            DidObj := TJSONObject(DidsArr.Items[I]);
            Decoder := DidObj.GetValue<TJSONObject>('decoder');
            if Decoder = nil then Continue;
            Kind := Decoder.GetValue<string>('kind', '');
            if Kind = '' then Continue;
            Assert.IsTrue(KnownSet.Contains(Kind),
              Format('%s: decoder.kind = %s not in recognised set',
                      [TPath.GetFileName(Path), Kind]));
          end;
        end;
      finally
        Doc.Free;
      end;
    end;
    LpArr := nil; DxArr := nil; // suppress hints
  finally
    KnownSet.Free;
  end;
end;

procedure TSchemaShapeTests.CodingFieldKindsUseRecognisedTags;
const
  RecognisedKinds: array[0..8] of string = (
    'bit', 'uint8', 'uint16_be', 'uint32_be',
    'int16_be', 'int32_be', 'ascii', 'enum', 'bitmask');
var
  Root, Path, Kind: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  Blocks, Fields: TJSONArray;
  I, J: Integer;
  Block, Field: TJSONObject;
  KnownSet: TList<string>;
  S: string;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  KnownSet := TList<string>.Create;
  try
    for S in RecognisedKinds do KnownSet.Add(S);
    Catalogs := CollectAllJson(Root);
    for Path in Catalogs do
    begin
      if not IsOemCatalog(Path) then Continue;
      Doc := ParseJsonObj(Path);
      if Doc = nil then Continue;
      try
        Blocks := Doc.GetValue<TJSONArray>('coding_blocks');
        if Blocks = nil then Continue;
        for I := 0 to Blocks.Count - 1 do
        begin
          if not (Blocks.Items[I] is TJSONObject) then Continue;
          Block := TJSONObject(Blocks.Items[I]);
          Fields := Block.GetValue<TJSONArray>('fields');
          if Fields = nil then Continue;
          for J := 0 to Fields.Count - 1 do
          begin
            if not (Fields.Items[J] is TJSONObject) then Continue;
            Field := TJSONObject(Fields.Items[J]);
            Kind := Field.GetValue<string>('kind', '');
            if Kind = '' then Continue;
            Assert.IsTrue(KnownSet.Contains(Kind),
              Format('%s: coding_block %s field %s kind=%s not recognised',
                      [TPath.GetFileName(Path),
                       Block.GetValue<string>('name', '?'),
                       Field.GetValue<string>('name', '?'), Kind]));
          end;
        end;
      finally
        Doc.Free;
      end;
    end;
  finally
    KnownSet.Free;
  end;
end;

procedure TSchemaShapeTests.AdaptationKindsUseRecognisedTags;
const
  RecognisedKinds: array[0..6] of string = (
    'uint8', 'uint16_be', 'uint32_be',
    'int16_be', 'int32_be', 'enum', 'bool');
var
  Root, Path, Kind: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  Adaptations: TJSONArray;
  Item: TJSONObject;
  I: Integer;
  KnownSet: TList<string>;
  S: string;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  KnownSet := TList<string>.Create;
  try
    for S in RecognisedKinds do KnownSet.Add(S);
    Catalogs := CollectAllJson(Root);
    for Path in Catalogs do
    begin
      if not IsOemCatalog(Path) then Continue;
      Doc := ParseJsonObj(Path);
      if Doc = nil then Continue;
      try
        Adaptations := Doc.GetValue<TJSONArray>('adaptations');
        if Adaptations = nil then Continue;
        for I := 0 to Adaptations.Count - 1 do
        begin
          if not (Adaptations.Items[I] is TJSONObject) then Continue;
          Item := TJSONObject(Adaptations.Items[I]);
          Kind := Item.GetValue<string>('kind', '');
          if Kind = '' then Continue;
          Assert.IsTrue(KnownSet.Contains(Kind),
            Format('%s: adaptation %s kind=%s not recognised',
                    [TPath.GetFileName(Path),
                     Item.GetValue<string>('name', '?'), Kind]));
        end;
      finally
        Doc.Free;
      end;
    end;
  finally
    KnownSet.Free;
  end;
end;

procedure TSchemaShapeTests.DTCCodesMatchSAEorJ1939OrOemFormat;
const
  // SAE J2012 + J1939 SPN + Volvo MID + OEM module-prefixed
  // formats. Mirrors the lint job in .github/workflows/ci.yml
  // (the bash one-liner), kept in sync here so a Delphi-only
  // contributor catches the same issues without leaving the IDE.
  AcceptedPrefixes: array[0..21] of string = (
    'SPN', 'MID', 'DF', 'BMS_', 'DI_', 'APP_', 'UI_', 'CC_',
    'GTW_', 'VCFRONT_', 'VCRIGHT_', 'VCLEFT_', 'EPAS_', 'AMP_',
    'PCS_', 'BC_', 'PARKBR_', 'AUTOPILOT_', 'FCM_', 'RCM_',
    'ECM-', 'BCM-');
  SaePattern = '^[PCBU][0-9A-Fa-f]{4}$';
var
  Root, Path, Code, Prefix: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  DtcsArr: TJSONArray;
  I: Integer;
  Item: TJSONObject;
  Accepted: Boolean;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectAllJson(Root);
  for Path in Catalogs do
  begin
    if not IsDtcCatalog(Path) then Continue;
    Doc := ParseJsonObj(Path);
    if Doc = nil then Continue;
    try
      DtcsArr := Doc.GetValue<TJSONArray>('dtcs');
      if DtcsArr = nil then Continue;
      for I := 0 to DtcsArr.Count - 1 do
      begin
        if not (DtcsArr.Items[I] is TJSONObject) then Continue;
        Item := TJSONObject(DtcsArr.Items[I]);
        Code := Item.GetValue<string>('code', '');
        if Code = '' then Continue;
        // Accepted if SAE-shaped or one of the OEM module prefixes.
        Accepted := TRegEx.IsMatch(Code, SaePattern);
        if not Accepted then
          for Prefix in AcceptedPrefixes do
            if Code.ToUpper.StartsWith(Prefix) then
            begin
              Accepted := True;
              Break;
            end;
        Assert.IsTrue(Accepted,
          Format('%s: DTC code %s does not match any recognised format',
                  [TPath.GetFileName(Path), Code]));
      end;
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSchemaShapeTests.ManufacturerKeysAreNonEmpty;
var
  Root, Path, Key: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectAllJson(Root);
  for Path in Catalogs do
  begin
    if not IsOemCatalog(Path) then Continue;
    Doc := ParseJsonObj(Path);
    if Doc = nil then Continue;
    try
      Key := Doc.GetValue<string>('manufacturer_key', '');
      Assert.IsTrue(Key <> '',
        Format('%s: manufacturer_key must be non-empty',
                [TPath.GetFileName(Path)]));
    finally
      Doc.Free;
    end;
  end;
end;

procedure TSchemaShapeTests.VersionFieldIsOneOrTwo;
var
  Root, Path: string;
  Catalogs: TArray<string>;
  Doc: TJSONObject;
  Ver: Integer;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectAllJson(Root);
  for Path in Catalogs do
  begin
    if not IsOemCatalog(Path) then Continue;
    Doc := ParseJsonObj(Path);
    if Doc = nil then Continue;
    try
      Ver := Doc.GetValue<Integer>('version', 0);
      Assert.IsTrue((Ver = 1) or (Ver = 2),
        Format('%s: version=%d, expected 1 or 2',
                [TPath.GetFileName(Path), Ver]));
    finally
      Doc.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSchemaShapeTests);

end.
