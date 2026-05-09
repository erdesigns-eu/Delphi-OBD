//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.CatalogIntegrity
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Catalog structural integrity. Walks every shipped
//                  JSON catalog (top level + per-vehicle-class
//                  subdirectories) and asserts:
//                    1) Every coding_blocks[] field's
//                       (byte_offset * 8 + bit_offset + width) does
//                       not exceed payload_size * 8 — i.e. fields
//                       fit inside the declared block. ASCII fields
//                       use bit_width as a byte length and check
//                       against payload_size in bytes.
//                    2) When the catalog declares an ecus[] array,
//                       every coding/adaptation/actuator/live PID
//                       entry's ecu_address resolves to a declared
//                       ECU (ecu_address = 0 is "global" and is
//                       skipped).
//                  These are the structural checks the Python lint
//                  script used to do — ported to Delphi so the
//                  build-time tooling stays inside the repo's
//                  language. Duplicate-primary-key checks live in
//                  the GitHub Actions workflow as a bash one-liner
//                  to keep the cost of "is the catalog
//                  well-formed?" low.
//------------------------------------------------------------------------------
unit Tests.OEM.CatalogIntegrity;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCatalogIntegrityTests = class
  public
    /// <summary>Coding block bit fields fit within payload.</summary>
    [Test] procedure CodingBlockBitFieldsFitWithinPayload;
    /// <summary>Cross section ecu references resolve.</summary>
    [Test] procedure CrossSectionEcuReferencesResolve;
    /// <summary>No duplicate primary keys.</summary>
    [Test] procedure NoDuplicatePrimaryKeys;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Generics.Collections,
  OBD.OEM.Catalog.JSON;

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

/// <summary>Collect every OEM catalog JSON path under <c>catalogs/</c>,
/// recursing into vehicle-class subdirectories. Excludes
/// <c>dtc-*.json</c> (different schema), <c>iso-*</c> /
/// <c>uds-*</c> / <c>obd2-*</c> universal catalogs, the
/// <c>_schema/</c> directory, and any <c>test-*.json</c>
/// fixture.</summary>
function CollectOemCatalogs(const Root: string): TArray<string>;
var
  All: TArray<string>;
  Path, Name: string;
  Acc: TList<string>;
begin
  Acc := TList<string>.Create;
  try
    All := TDirectory.GetFiles(
      Root, '*.json', TSearchOption.soAllDirectories);
    for Path in All do
    begin
      Name := TPath.GetFileName(Path);
      if Name.StartsWith('dtc-', True)  then Continue;
      if Name.StartsWith('iso-', True)  then Continue;
      if Name.StartsWith('uds-', True)  then Continue;
      if Name.StartsWith('obd2-', True) then Continue;
      if Name.StartsWith('test-', True) then Continue;
      // Skip the JSON schema document itself.
      if TPath.GetDirectoryName(Path).EndsWith(
           PathDelim + '_schema') then Continue;
      Acc.Add(Path);
    end;
    Result := Acc.ToArray;
  finally
    Acc.Free;
  end;
end;

function ParseAddress(const S: string): Integer;
var
  Tmp: string;
begin
  Result := -1;
  Tmp := Trim(S);
  if Tmp = '' then Exit;
  if (Length(Tmp) > 2) and (Tmp[1] = '0') and
     ((Tmp[2] = 'x') or (Tmp[2] = 'X')) then
  begin
    if not TryStrToInt('$' + Copy(Tmp, 3, MaxInt), Result) then
      Result := -1;
  end
  else if not TryStrToInt(Tmp, Result) then
    Result := -1;
end;

/// <summary>Decode a JSON address-shaped value (int or "0xHHHH") to
/// an integer, or -1 if absent / malformed.</summary>
function JsonAddrFromObject(Obj: TJSONObject;
                             const Field: string): Integer;
var
  V: TJSONValue;
  N: TJSONNumber;
begin
  Result := -1;
  if Obj = nil then Exit;
  V := Obj.GetValue(Field);
  if V = nil then Exit;
  if V is TJSONString then
    Result := ParseAddress(TJSONString(V).Value)
  else if V is TJSONNumber then
  begin
    N := TJSONNumber(V);
    Result := N.AsInt;
  end;
end;

/// <summary>Conservative bit width per coding-field kind. Mirrors
/// the JSON loader's <c>ParseCodingFieldKind</c> mapping but expressed
/// here so this test doesn't depend on the loader for sizing.</summary>
function FieldBitWidth(const KindStr: string;
                       BitWidthOverride: Integer): Integer;
begin
  if BitWidthOverride > 0 then Exit(BitWidthOverride);
  if SameText(KindStr, 'bit')       then Exit(1);
  if SameText(KindStr, 'uint8')     then Exit(8);
  if SameText(KindStr, 'int8')      then Exit(8);
  if SameText(KindStr, 'uint16_be') then Exit(16);
  if SameText(KindStr, 'int16_be')  then Exit(16);
  if SameText(KindStr, 'uint32_be') then Exit(32);
  if SameText(KindStr, 'int32_be')  then Exit(32);
  if SameText(KindStr, 'enum')      then Exit(8);
  if SameText(KindStr, 'bitmask')   then Exit(8);
  Result := 8;
end;

//------------------------------------------------------------------------------
// CodingBlockBitFieldsFitWithinPayload
//------------------------------------------------------------------------------
procedure TCatalogIntegrityTests.CodingBlockBitFieldsFitWithinPayload;
var
  Root, Path, Name, Kind: string;
  Catalogs: TArray<string>;
  Doc: TJSONValue;
  RootObj, Block, Field: TJSONObject;
  Blocks, Fields: TJSONArray;
  Size, ByteOff, BitOff, BitWidth, EndBit, EndByte, AsciiLen: Integer;
  I, J: Integer;
  Checked: Integer;
  KindOverride: TJSONValue;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectOemCatalogs(Root);
  Checked := 0;
  for Path in Catalogs do
  begin
    Name := Path.Substring(Root.Length + 1);
    Doc := TJSONObject.ParseJSONValue(TFile.ReadAllText(Path, TEncoding.UTF8));
    try
      if not (Doc is TJSONObject) then Continue;
      RootObj := Doc as TJSONObject;
      Blocks := RootObj.GetValue<TJSONArray>('coding_blocks');
      if Blocks = nil then Continue;
      for I := 0 to Blocks.Count - 1 do
      begin
        if not (Blocks.Items[I] is TJSONObject) then Continue;
        Block := TJSONObject(Blocks.Items[I]);
        Size := Block.GetValue<Integer>('payload_size', 0);
        if Size <= 0 then Continue;
        Fields := Block.GetValue<TJSONArray>('fields');
        if Fields = nil then Continue;
        for J := 0 to Fields.Count - 1 do
        begin
          if not (Fields.Items[J] is TJSONObject) then Continue;
          Field := TJSONObject(Fields.Items[J]);
          KindOverride := Field.GetValue('kind');
          Kind := '';
          if KindOverride is TJSONString then
            Kind := TJSONString(KindOverride).Value;
          ByteOff := Field.GetValue<Integer>('byte_offset', 0);
          BitOff  := Field.GetValue<Integer>('bit_offset', 0);
          BitWidth := Field.GetValue<Integer>('bit_width', 0);
          if SameText(Kind, 'ascii') then
          begin
            // ASCII: bit_width is interpreted as a byte length.
            AsciiLen := BitWidth;
            if AsciiLen <= 0 then AsciiLen := 1;
            EndByte := ByteOff + AsciiLen;
            Assert.IsTrue(EndByte <= Size,
              Format('%s: coding_block %s field %s ASCII overflows ' +
                     'payload (end_byte=%d > size=%d)',
                     [Name,
                      Block.GetValue<string>('name', '?'),
                      Field.GetValue<string>('name', '?'),
                      EndByte, Size]));
          end
          else
          begin
            BitWidth := FieldBitWidth(Kind, BitWidth);
            EndBit := ByteOff * 8 + BitOff + BitWidth;
            Assert.IsTrue(EndBit <= Size * 8,
              Format('%s: coding_block %s field %s overruns payload ' +
                     '(end_bit=%d > size_bits=%d)',
                     [Name,
                      Block.GetValue<string>('name', '?'),
                      Field.GetValue<string>('name', '?'),
                      EndBit, Size * 8]));
          end;
          Inc(Checked);
        end;
      end;
    finally
      Doc.Free;
    end;
  end;
  Assert.IsTrue(Checked > 0,
    'expected at least one coding-block field across all catalogs');
end;

//------------------------------------------------------------------------------
// CrossSectionEcuReferencesResolve
//------------------------------------------------------------------------------
procedure TCatalogIntegrityTests.CrossSectionEcuReferencesResolve;
var
  Root, Path, Name: string;
  Catalogs: TArray<string>;
  Doc: TJSONValue;
  RootObj: TJSONObject;
  Section: TJSONArray;
  Item: TJSONObject;
  EcuArr: TJSONArray;
  Declared: TList<Integer>;
  I, J: Integer;
  Addr: Integer;
  SectionName: string;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectOemCatalogs(Root);
  for Path in Catalogs do
  begin
    Name := Path.Substring(Root.Length + 1);
    Doc := TJSONObject.ParseJSONValue(TFile.ReadAllText(Path, TEncoding.UTF8));
    try
      if not (Doc is TJSONObject) then Continue;
      RootObj := Doc as TJSONObject;
      EcuArr := RootObj.GetValue<TJSONArray>('ecus');
      if (EcuArr = nil) or (EcuArr.Count = 0) then
        Continue; // No ECU map → nothing to cross-reference against.

      Declared := TList<Integer>.Create;
      try
        for I := 0 to EcuArr.Count - 1 do
        begin
          if not (EcuArr.Items[I] is TJSONObject) then Continue;
          Addr := JsonAddrFromObject(
            TJSONObject(EcuArr.Items[I]), 'address');
          if Addr >= 0 then Declared.Add(Addr);
        end;

        for SectionName in TArray<string>.Create(
          'coding_blocks', 'adaptations',
          'actuator_tests', 'live_pids') do
        begin
          Section := RootObj.GetValue<TJSONArray>(SectionName);
          if Section = nil then Continue;
          for J := 0 to Section.Count - 1 do
          begin
            if not (Section.Items[J] is TJSONObject) then Continue;
            Item := TJSONObject(Section.Items[J]);
            Addr := JsonAddrFromObject(Item, 'ecu_address');
            if Addr <= 0 then Continue; // 0 / absent = global
            Assert.IsTrue(Declared.Contains(Addr),
              Format('%s: %s entry %s references undeclared ECU 0x%.4x',
                     [Name, SectionName,
                      Item.GetValue<string>('name', '?'),
                      Addr]));
          end;
        end;
      finally
        Declared.Free;
      end;
    finally
      Doc.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------
// NoDuplicatePrimaryKeys
//------------------------------------------------------------------------------
type
  TIntSet = TList<Integer>;

procedure CheckNoDuplicateAddrField(
  RootObj: TJSONObject; const Name, SectionName, KeyField: string);
var
  Section: TJSONArray;
  Item: TJSONObject;
  I, Addr: Integer;
  Seen: TIntSet;
begin
  Section := RootObj.GetValue<TJSONArray>(SectionName);
  if Section = nil then Exit;
  Seen := TIntSet.Create;
  try
    for I := 0 to Section.Count - 1 do
    begin
      if not (Section.Items[I] is TJSONObject) then Continue;
      Item := TJSONObject(Section.Items[I]);
      Addr := JsonAddrFromObject(Item, KeyField);
      if Addr < 0 then Continue;
      Assert.IsFalse(Seen.Contains(Addr),
        Format('%s: %s.%s = 0x%.4x duplicated',
               [Name, SectionName, KeyField, Addr]));
      Seen.Add(Addr);
    end;
  finally
    Seen.Free;
  end;
end;

procedure TCatalogIntegrityTests.NoDuplicatePrimaryKeys;
var
  Root, Path, Name: string;
  Catalogs: TArray<string>;
  Doc: TJSONValue;
  RootObj: TJSONObject;
begin
  Root := CatalogsRoot;
  if Root = '' then Assert.Pass('catalogs/ not on path; skipping');
  Catalogs := CollectOemCatalogs(Root);
  for Path in Catalogs do
  begin
    Name := Path.Substring(Root.Length + 1);
    Doc := TJSONObject.ParseJSONValue(TFile.ReadAllText(Path, TEncoding.UTF8));
    try
      if not (Doc is TJSONObject) then Continue;
      RootObj := Doc as TJSONObject;
      CheckNoDuplicateAddrField(RootObj, Name, 'ecus', 'address');
      CheckNoDuplicateAddrField(RootObj, Name, 'dids', 'did');
      CheckNoDuplicateAddrField(RootObj, Name, 'routines', 'id');
      CheckNoDuplicateAddrField(RootObj, Name, 'coding_blocks', 'did');
      CheckNoDuplicateAddrField(RootObj, Name, 'adaptations', 'channel');
      CheckNoDuplicateAddrField(RootObj, Name, 'actuator_tests', 'id');
    finally
      Doc.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCatalogIntegrityTests);

end.
