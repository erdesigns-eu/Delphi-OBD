//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.CodingCommon
// COPYRIGHT      : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Coverage tests for the v3.28 unified coding /
//                  WriteDataByIdentifier API.
//------------------------------------------------------------------------------
unit Tests.OEM.CodingCommon;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCodingRegistryTests = class
  public
    /// <summary>
    ///   Name matches kind is case insensitive.
    /// </summary>
    [Test] procedure NameMatchesKindIsCaseInsensitive;
    /// <summary>
    ///   Classify vehicle order tokens.
    /// </summary>
    [Test] procedure ClassifyVehicleOrderTokens;
    /// <summary>
    ///   Classify as built code tokens.
    /// </summary>
    [Test] procedure ClassifyAsBuiltCodeTokens;
    /// <summary>
    ///   Classify fca proxi tokens.
    /// </summary>
    [Test] procedure ClassifyFcaProxiTokens;
    /// <summary>
    ///   Classify market region tokens.
    /// </summary>
    [Test] procedure ClassifyMarketRegionTokens;
    /// <summary>
    ///   Classify starlight tokens.
    /// </summary>
    [Test] procedure ClassifyStarlightTokens;
    /// <summary>
    ///   Classify unknown returns cf unknown.
    /// </summary>
    [Test] procedure ClassifyUnknownReturnsCfUnknown;
  end;

  [TestFixture]
  TCodingLookupTests = class
  public
    /// <summary>
    ///   Rolls royce resolves vehicle order.
    /// </summary>
    [Test] procedure RollsRoyceResolvesVehicleOrder;
    /// <summary>
    ///   Rolls royce resolves starlight pattern.
    /// </summary>
    [Test] procedure RollsRoyceResolvesStarlightPattern;
    /// <summary>
    ///   Mazda resolves as built code.
    /// </summary>
    [Test] procedure MazdaResolvesAsBuiltCode;
    /// <summary>
    ///   Mazda resolves market region.
    /// </summary>
    [Test] procedure MazdaResolvesMarketRegion;
    /// <summary>
    ///   Unsupported kind returns false.
    /// </summary>
    [Test] procedure UnsupportedKindReturnsFalse;
    /// <summary>
    ///   Nil extension returns false.
    /// </summary>
    [Test] procedure NilExtensionReturnsFalse;
  end;

  [TestFixture]
  TCodingFrameTests = class
  public
    /// <summary>
    ///   Write data by identifier wraps sid and d i d.
    /// </summary>
    [Test] procedure WriteDataByIdentifierWrapsSidAndDID;
    /// <summary>
    ///   Write data by identifier appends payload.
    /// </summary>
    [Test] procedure WriteDataByIdentifierAppendsPayload;
    /// <summary>
    ///   Parse accepts positive response.
    /// </summary>
    [Test] procedure ParseAcceptsPositiveResponse;
    /// <summary>
    ///   Parse rejects wrong sid.
    /// </summary>
    [Test] procedure ParseRejectsWrongSid;
    /// <summary>
    ///   Parse rejects wrong d i d.
    /// </summary>
    [Test] procedure ParseRejectsWrongDID;
    /// <summary>
    ///   Kind name produces human label.
    /// </summary>
    [Test] procedure KindNameProducesHumanLabel;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.Coding.Common,
  OBD.OEM.RollsRoyce, OBD.OEM.Mazda;

//------------------------------------------------------------------------------
// NAME MATCHES KIND IS CASE INSENSITIVE
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.NameMatchesKindIsCaseInsensitive;
begin
  Assert.IsTrue(TOBDCodingFunctionRegistry.NameMatchesKind(
    'FA_ASSEMBLY', cfVehicleOrder));
  Assert.IsTrue(TOBDCodingFunctionRegistry.NameMatchesKind(
    'Fa_Assembly', cfVehicleOrder));
end;

//------------------------------------------------------------------------------
// CLASSIFY VEHICLE ORDER TOKENS
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyVehicleOrderTokens;
begin
  Assert.AreEqual(Ord(cfVehicleOrder), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('fa_assembly')));
  Assert.AreEqual(Ord(cfVehicleOrder), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('bentley_commission_no')));
  Assert.AreEqual(Ord(cfVehicleOrder), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('vehicle_order')));
end;

//------------------------------------------------------------------------------
// CLASSIFY AS BUILT CODE TOKENS
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyAsBuiltCodeTokens;
begin
  Assert.AreEqual(Ord(cfAsBuiltCode), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('mazda_as_built_code')));
  Assert.AreEqual(Ord(cfAsBuiltCode), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('ford_as_built_block')));
end;

//------------------------------------------------------------------------------
// CLASSIFY FCA PROXI TOKENS
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyFcaProxiTokens;
begin
  Assert.AreEqual(Ord(cfFcaProxi), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('witech_proxi_align')));
end;

//------------------------------------------------------------------------------
// CLASSIFY MARKET REGION TOKENS
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyMarketRegionTokens;
begin
  Assert.AreEqual(Ord(cfMarketRegion), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('mazda_market_code')));
  Assert.AreEqual(Ord(cfMarketRegion), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('subaru_market_code')));
end;

//------------------------------------------------------------------------------
// CLASSIFY STARLIGHT TOKENS
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyStarlightTokens;
begin
  Assert.AreEqual(Ord(cfStarlightPattern), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('rr_starlight_pattern')));
end;

//------------------------------------------------------------------------------
// CLASSIFY UNKNOWN RETURNS CF UNKNOWN
//------------------------------------------------------------------------------
procedure TCodingRegistryTests.ClassifyUnknownReturnsCfUnknown;
begin
  Assert.AreEqual(Ord(cfUnknown), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('battery_voltage_12v')));
  Assert.AreEqual(Ord(cfUnknown), Ord(
    TOBDCodingFunctionRegistry.ClassifyName('engine_oil_pressure')));
end;

//==============================================================================
// Lookup against shipped OEM catalogs
//==============================================================================

//------------------------------------------------------------------------------
// ROLLS ROYCE RESOLVES VEHICLE ORDER
//------------------------------------------------------------------------------
procedure TCodingLookupTests.RollsRoyceResolvesVehicleOrder;
var
  Ext: IOBDOEMExtension;
  Func: TOBDCodingFunction;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  Assert.IsTrue(FindCodingFunction(Ext, cfVehicleOrder, Func));
  Assert.AreEqual(Ord(cfVehicleOrder), Ord(Func.Kind));
  Assert.AreEqual('fa_assembly', Func.DidName);
end;

//------------------------------------------------------------------------------
// ROLLS ROYCE RESOLVES STARLIGHT PATTERN
//------------------------------------------------------------------------------
procedure TCodingLookupTests.RollsRoyceResolvesStarlightPattern;
var
  Ext: IOBDOEMExtension;
  Func: TOBDCodingFunction;
begin
  Ext := TOBDOEMExtensionRollsRoyce.Create;
  Assert.IsTrue(FindCodingFunction(Ext, cfStarlightPattern, Func));
  Assert.AreEqual('rr_starlight_pattern', Func.DidName);
end;

//------------------------------------------------------------------------------
// MAZDA RESOLVES AS BUILT CODE
//------------------------------------------------------------------------------
procedure TCodingLookupTests.MazdaResolvesAsBuiltCode;
var
  Ext: IOBDOEMExtension;
  Func: TOBDCodingFunction;
begin
  Ext := TOBDOEMExtensionMazda.Create;
  Assert.IsTrue(FindCodingFunction(Ext, cfAsBuiltCode, Func));
  Assert.AreEqual('mazda_as_built_code', Func.DidName);
end;

//------------------------------------------------------------------------------
// MAZDA RESOLVES MARKET REGION
//------------------------------------------------------------------------------
procedure TCodingLookupTests.MazdaResolvesMarketRegion;
var
  Ext: IOBDOEMExtension;
  Func: TOBDCodingFunction;
begin
  Ext := TOBDOEMExtensionMazda.Create;
  Assert.IsTrue(FindCodingFunction(Ext, cfMarketRegion, Func));
  Assert.AreEqual('mazda_market_code', Func.DidName);
end;

//------------------------------------------------------------------------------
// UNSUPPORTED KIND RETURNS FALSE
//------------------------------------------------------------------------------
procedure TCodingLookupTests.UnsupportedKindReturnsFalse;
var
  Ext: IOBDOEMExtension;
  Func: TOBDCodingFunction;
begin
  // Mazda does not ship a Rolls-Royce Starlight programming DID.
  Ext := TOBDOEMExtensionMazda.Create;
  Assert.IsFalse(FindCodingFunction(Ext, cfStarlightPattern, Func));
  Assert.AreEqual(Ord(cfUnknown), Ord(Func.Kind));
end;

//------------------------------------------------------------------------------
// NIL EXTENSION RETURNS FALSE
//------------------------------------------------------------------------------
procedure TCodingLookupTests.NilExtensionReturnsFalse;
var
  Func: TOBDCodingFunction;
begin
  Assert.IsFalse(FindCodingFunction(nil, cfVehicleOrder, Func));
end;

//==============================================================================
// Frame builder + display labels
//==============================================================================

//------------------------------------------------------------------------------
// WRITE DATA BY IDENTIFIER WRAPS SID AND DID
//------------------------------------------------------------------------------
procedure TCodingFrameTests.WriteDataByIdentifierWrapsSidAndDID;
var
  Frame: TBytes;
begin
  Frame := BuildWriteDataByIdentifier($F1A2, nil);
  Assert.AreEqual(3, Length(Frame));
  Assert.AreEqual($2E, Integer(Frame[0]), 'SID = 0x2E');
  Assert.AreEqual($F1, Integer(Frame[1]), 'DID hi byte');
  Assert.AreEqual($A2, Integer(Frame[2]), 'DID lo byte');
end;

//------------------------------------------------------------------------------
// WRITE DATA BY IDENTIFIER APPENDS PAYLOAD
//------------------------------------------------------------------------------
procedure TCodingFrameTests.WriteDataByIdentifierAppendsPayload;
var
  Frame, Data: TBytes;
begin
  Data := TBytes.Create($AA, $BB, $CC, $DD);
  Frame := BuildWriteDataByIdentifier($1234, Data);
  Assert.AreEqual(7, Length(Frame));
  Assert.AreEqual($AA, Integer(Frame[3]));
  Assert.AreEqual($BB, Integer(Frame[4]));
  Assert.AreEqual($CC, Integer(Frame[5]));
  Assert.AreEqual($DD, Integer(Frame[6]));
end;

//------------------------------------------------------------------------------
// PARSE ACCEPTS POSITIVE RESPONSE
//------------------------------------------------------------------------------
procedure TCodingFrameTests.ParseAcceptsPositiveResponse;
var
  Resp: TBytes;
begin
  // 0x6E = 0x2E + 0x40 (positive WriteDataByIdentifier response)
  Resp := TBytes.Create($6E, $F1, $A2);
  Assert.IsTrue(ParseCodingResponse(Resp, $F1A2));
end;

//------------------------------------------------------------------------------
// PARSE REJECTS WRONG SID
//------------------------------------------------------------------------------
procedure TCodingFrameTests.ParseRejectsWrongSid;
var
  Resp: TBytes;
begin
  Resp := TBytes.Create($62, $F1, $A2);   // 0x62 = ReadData response
  Assert.IsFalse(ParseCodingResponse(Resp, $F1A2));
end;

//------------------------------------------------------------------------------
// PARSE REJECTS WRONG DID
//------------------------------------------------------------------------------
procedure TCodingFrameTests.ParseRejectsWrongDID;
var
  Resp: TBytes;
begin
  Resp := TBytes.Create($6E, $F1, $A0);
  Assert.IsFalse(ParseCodingResponse(Resp, $F1A2));
end;

//------------------------------------------------------------------------------
// KIND NAME PRODUCES HUMAN LABEL
//------------------------------------------------------------------------------
procedure TCodingFrameTests.KindNameProducesHumanLabel;
begin
  Assert.AreEqual('Vehicle Order / FA / Commission',
    CodingFunctionKindName(cfVehicleOrder));
  Assert.AreEqual('FCA wiTech Proxi Alignment',
    CodingFunctionKindName(cfFcaProxi));
  Assert.AreEqual('Unknown Coding Function',
    CodingFunctionKindName(cfUnknown));
end;

initialization
  TDUnitX.RegisterTestFixture(TCodingRegistryTests);
  TDUnitX.RegisterTestFixture(TCodingLookupTests);
  TDUnitX.RegisterTestFixture(TCodingFrameTests);

end.
