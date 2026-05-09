//------------------------------------------------------------------------------
//  OBD.Flash.ImageApplicability
//
//  TOBDFlashImageApplicability — loads a sidecar JSON descriptor
//  that declares which ECUs a firmware image is allowed to flash.
//  The pipeline calls <see cref="Verify"/> from an fpVerifyImage
//  check; the helper reads the identifying DIDs declared in the
//  descriptor's constraints, compares each against the rule
//  (exact / any-of / regex / hex-prefix), and refuses the flash
//  on any mismatch.
//
//  This closes Phase 9 honest-review flag 5: "no image-sanity
//  check beyond signature." A signed image with the wrong target
//  ECU type would otherwise pass signature verification and
//  proceed.
//
//  Schema lives in <c>data/schemas/flash-image-applicability.schema.json</c>.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 9 follow-up.
//------------------------------------------------------------------------------

unit OBD.Flash.ImageApplicability;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  System.RegularExpressions,
  OBD.Types,
  OBD.Coding.DataIdentifierIO;

type
  /// <summary>Match rule kind.</summary>
  TOBDApplicabilityKind = (
    akExact,
    akAnyOf,
    akRegex,
    akHexPrefix
  );

  /// <summary>One declared constraint.</summary>
  TOBDApplicabilityConstraint = record
    DID: Word;
    Name: string;
    Kind: TOBDApplicabilityKind;
    Value: string;
    Values: TArray<string>;
  end;

  /// <summary>Decoded descriptor.</summary>
  TOBDApplicabilityDescriptor = record
    ImageSha256Hex: string;
    Vendor: string;
    VendorModule: string;
    ImagePartNumber: string;
    Constraints: TArray<TOBDApplicabilityConstraint>;
  end;

  /// <summary>One result entry from <see cref="Verify"/>.</summary>
  TOBDApplicabilityResult = record
    Constraint: TOBDApplicabilityConstraint;
    EcuValueAscii: string;
    EcuValueHex: string;
    Passed: Boolean;
    Reason: string;
  end;

  /// <summary>Helper component / class methods for image
  /// applicability cross-check.</summary>
  TOBDFlashImageApplicability = class
  public
    /// <summary>Loads + parses a descriptor file.</summary>
    /// <exception cref="EOBDProtocol">Schema violation.</exception>
    class function Load(const AFileName: string): TOBDApplicabilityDescriptor; static;
    /// <summary>Same as <see cref="Load"/> from a JSON string.</summary>
    class function Parse(const AJson: string): TOBDApplicabilityDescriptor; static;

    /// <summary>Cross-checks <c>ADescriptor</c> against the live
    /// ECU. Reads each declared DID via <c>AIO</c>, evaluates the
    /// rule, and returns one result entry per constraint.
    /// <c>AAllPassed</c> is True only when every entry passes.</summary>
    class function Verify(const ADescriptor: TOBDApplicabilityDescriptor;
      AIO: TOBDDataIdentifierIO;
      out AAllPassed: Boolean): TArray<TOBDApplicabilityResult>; static;

    /// <summary>Validates the image's SHA-256 against the
    /// descriptor's <c>image_sha256_hex</c>. Pipeline integrators
    /// call this before <see cref="Verify"/>.</summary>
    class function MatchesImage(const ADescriptor: TOBDApplicabilityDescriptor;
      const AImageSha256: TBytes): Boolean; static;

    /// <summary>Convenience: hex-encodes a byte buffer.</summary>
    class function HexEncode(const AData: TBytes): string; static;
  end;

implementation

class function TOBDFlashImageApplicability.HexEncode(
  const AData: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AData) do
    Result := Result + LowerCase(IntToHex(AData[I], 2));
end;

function ParseDIDValue(const AValue: TJSONValue): Word;
var
  S: string;
begin
  if AValue is TJSONNumber then
    Exit(Word(TJSONNumber(AValue).AsInt64))
  else if AValue is TJSONString then
  begin
    S := Trim(TJSONString(AValue).Value);
    if (Length(S) >= 2) and (S[1] = '0') and
       CharInSet(S[2], ['x', 'X']) then
      Exit(Word(StrToInt('$' + Copy(S, 3, MaxInt))));
    Exit(Word(StrToInt(S)));
  end
  else
    raise EOBDProtocol.Create('applicability: did must be int or hex string');
end;

function ParseKind(const AText: string): TOBDApplicabilityKind;
begin
  if      SameText(AText, 'exact')      then Result := akExact
  else if SameText(AText, 'any_of')     then Result := akAnyOf
  else if SameText(AText, 'regex')      then Result := akRegex
  else if SameText(AText, 'hex_prefix') then Result := akHexPrefix
  else
    raise EOBDProtocol.CreateFmt(
      'applicability: match.kind "%s" not in schema', [AText]);
end;

function CleanHex(const AText: string): string;
begin
  Result := UpperCase(StringReplace(AText, ' ', '', [rfReplaceAll]));
end;

class function TOBDFlashImageApplicability.Parse(
  const AJson: string): TOBDApplicabilityDescriptor;
var
  Doc: TJSONValue;
  Root: TJSONObject;
  V: TJSONValue;
  Arr, ValuesArr: TJSONArray;
  ConObj, MatchObj: TJSONObject;
  I, J: Integer;
  Cons: TList<TOBDApplicabilityConstraint>;
  Item: TOBDApplicabilityConstraint;
  ValuesAcc: TList<string>;
begin
  Result := Default(TOBDApplicabilityDescriptor);
  Doc := TJSONObject.ParseJSONValue(AJson);
  if not (Doc is TJSONObject) then
  begin
    if Doc <> nil then Doc.Free;
    raise EOBDProtocol.Create('applicability: root is not an object');
  end;
  try
    Root := Doc as TJSONObject;
    V := Root.GetValue('version');
    if not (V is TJSONNumber) or (TJSONNumber(V).AsInt64 <> 1) then
      raise EOBDProtocol.Create('applicability: version must be 1');

    V := Root.GetValue('image_sha256_hex');
    if not (V is TJSONString) then
      raise EOBDProtocol.Create('applicability: image_sha256_hex required');
    Result.ImageSha256Hex := LowerCase(TJSONString(V).Value);
    if Length(Result.ImageSha256Hex) <> 64 then
      raise EOBDProtocol.Create(
        'applicability: image_sha256_hex must be 64 hex chars');

    V := Root.GetValue('vendor');
    if V is TJSONString then Result.Vendor := V.Value;
    V := Root.GetValue('vendor_module');
    if V is TJSONString then Result.VendorModule := V.Value;
    V := Root.GetValue('image_part_number');
    if V is TJSONString then Result.ImagePartNumber := V.Value;

    Cons := TList<TOBDApplicabilityConstraint>.Create;
    ValuesAcc := TList<string>.Create;
    try
      V := Root.GetValue('constraints');
      if V is TJSONArray then
      begin
        Arr := V as TJSONArray;
        for I := 0 to Arr.Count - 1 do
        begin
          if not (Arr.Items[I] is TJSONObject) then
            raise EOBDProtocol.CreateFmt(
              'applicability: constraints[%d] not an object', [I]);
          ConObj := Arr.Items[I] as TJSONObject;
          Item := Default(TOBDApplicabilityConstraint);
          V := ConObj.GetValue('did');
          if V = nil then
            raise EOBDProtocol.CreateFmt(
              'applicability: constraints[%d].did missing', [I]);
          Item.DID := ParseDIDValue(V);
          V := ConObj.GetValue('name');
          if V is TJSONString then Item.Name := V.Value;
          V := ConObj.GetValue('match');
          if not (V is TJSONObject) then
            raise EOBDProtocol.CreateFmt(
              'applicability: constraints[%d].match missing', [I]);
          MatchObj := V as TJSONObject;
          V := MatchObj.GetValue('kind');
          if not (V is TJSONString) then
            raise EOBDProtocol.CreateFmt(
              'applicability: constraints[%d].match.kind missing', [I]);
          Item.Kind := ParseKind(V.Value);
          case Item.Kind of
            akExact, akRegex, akHexPrefix:
              begin
                V := MatchObj.GetValue('value');
                if not (V is TJSONString) then
                  raise EOBDProtocol.CreateFmt(
                    'applicability: constraints[%d].match.value missing', [I]);
                Item.Value := V.Value;
              end;
            akAnyOf:
              begin
                V := MatchObj.GetValue('values');
                if not (V is TJSONArray) then
                  raise EOBDProtocol.CreateFmt(
                    'applicability: constraints[%d].match.values missing', [I]);
                ValuesArr := V as TJSONArray;
                ValuesAcc.Clear;
                for J := 0 to ValuesArr.Count - 1 do
                  if ValuesArr.Items[J] is TJSONString then
                    ValuesAcc.Add(TJSONString(ValuesArr.Items[J]).Value);
                Item.Values := ValuesAcc.ToArray;
              end;
          end;
          Cons.Add(Item);
        end;
      end;
      Result.Constraints := Cons.ToArray;
    finally
      ValuesAcc.Free;
      Cons.Free;
    end;
  finally
    Doc.Free;
  end;
end;

class function TOBDFlashImageApplicability.Load(
  const AFileName: string): TOBDApplicabilityDescriptor;
begin
  if not TFile.Exists(AFileName) then
    raise EOBDProtocol.CreateFmt(
      'applicability descriptor not found: %s', [AFileName]);
  Result := Parse(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

class function TOBDFlashImageApplicability.MatchesImage(
  const ADescriptor: TOBDApplicabilityDescriptor;
  const AImageSha256: TBytes): Boolean;
begin
  Result := SameText(ADescriptor.ImageSha256Hex,
    HexEncode(AImageSha256));
end;

class function TOBDFlashImageApplicability.Verify(
  const ADescriptor: TOBDApplicabilityDescriptor;
  AIO: TOBDDataIdentifierIO;
  out AAllPassed: Boolean): TArray<TOBDApplicabilityResult>;
var
  I, K: Integer;
  Item: TOBDApplicabilityResult;
  Bytes: TBytes;
  Cleaned: string;
begin
  if AIO = nil then
    raise EOBDConfig.Create(
      'applicability.Verify: TOBDDataIdentifierIO is nil');
  AAllPassed := True;
  SetLength(Result, Length(ADescriptor.Constraints));
  for I := 0 to High(ADescriptor.Constraints) do
  begin
    Item := Default(TOBDApplicabilityResult);
    Item.Constraint := ADescriptor.Constraints[I];
    try
      Bytes := AIO.ReadOne(Item.Constraint.DID);
      Item.EcuValueHex := HexEncode(Bytes);
      Item.EcuValueAscii := TEncoding.ASCII.GetString(Bytes);
      // Some DIDs return ASCII, some return raw bytes. Trim ASCII
      // padding for the comparison; the raw hex stays available
      // for hex_prefix matches.
      Item.EcuValueAscii := TrimRight(Item.EcuValueAscii);
    except
      on E: Exception do
      begin
        Item.Passed := False;
        Item.Reason := Format('Read DID 0x%4.4X failed: %s',
          [Item.Constraint.DID, E.Message]);
        Result[I] := Item;
        AAllPassed := False;
        Continue;
      end;
    end;

    case Item.Constraint.Kind of
      akExact:
        Item.Passed := SameText(Item.EcuValueAscii, Item.Constraint.Value);
      akAnyOf:
      begin
        Item.Passed := False;
        for K := 0 to High(Item.Constraint.Values) do
          if SameText(Item.EcuValueAscii, Item.Constraint.Values[K]) then
          begin
            Item.Passed := True;
            Break;
          end;
      end;
      akRegex:
      begin
        try
          Item.Passed := TRegEx.IsMatch(Item.EcuValueAscii,
            Item.Constraint.Value);
        except
          on E: Exception do
          begin
            Item.Passed := False;
            Item.Reason := 'regex error: ' + E.Message;
          end;
        end;
      end;
      akHexPrefix:
      begin
        Cleaned := CleanHex(Item.Constraint.Value);
        Item.Passed := StartsText(Cleaned, UpperCase(Item.EcuValueHex));
      end;
    end;

    if not Item.Passed and (Item.Reason = '') then
      Item.Reason := Format(
        'DID 0x%4.4X mismatch — ECU returned "%s"',
        [Item.Constraint.DID, Item.EcuValueAscii]);

    if not Item.Passed then AAllPassed := False;
    Result[I] := Item;
  end;
end;

end.
