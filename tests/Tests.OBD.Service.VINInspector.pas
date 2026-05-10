//------------------------------------------------------------------------------
//  Tests.OBD.Service.VINInspector
//
//  Coverage for the TOBDVINInspector wrapper component.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.VINInspector;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  DUnitX.TestFramework,
  OBD.Service.VINDecoder,
  OBD.Service.VINDecoder.Types,
  OBD.Service.VINInspector;

type
  [TestFixture]
  TVINInspectorTests = class
  strict private
    FDecoded: Boolean;
    FInvalid: Boolean;
    FCapturedReason: string;
    procedure HandleDecoded(Sender: TObject; const AInfo: TOBDVINInfo);
    procedure HandleInvalid(Sender: TObject; const AReason: string);
  public
    [Setup] procedure Setup;

    [Test] procedure SettingVINTriggersAutoDecode;
    [Test] procedure ShortcutPropertiesMirrorInfo;
    [Test] procedure InvalidVIN_FiresOnInvalid;
    [Test] procedure AutoDecodeOff_DoesNothingUntilDecodeNow;
  end;

implementation

procedure TVINInspectorTests.HandleDecoded(Sender: TObject;
  const AInfo: TOBDVINInfo);
begin
  FDecoded := True;
end;

procedure TVINInspectorTests.HandleInvalid(Sender: TObject;
  const AReason: string);
begin
  FInvalid := True;
  FCapturedReason := AReason;
end;

procedure TVINInspectorTests.Setup;
begin
  FDecoded := False;
  FInvalid := False;
  FCapturedReason := '';
  TOBDVINDecoder.CatalogDir :=
    TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\catalogs');
  TOBDVINDecoder.LoadCatalogs(TOBDVINDecoder.CatalogDir);
end;

procedure TVINInspectorTests.SettingVINTriggersAutoDecode;
var I: TOBDVINInspector;
begin
  I := TOBDVINInspector.Create(nil);
  try
    I.OnDecoded := HandleDecoded;
    I.VIN := 'WVWZZZ1KZ7W123456';
    Assert.IsTrue(FDecoded, 'OnDecoded should fire after VIN assignment');
    Assert.IsTrue(I.Valid, I.Info.InvalidReason);
    Assert.AreEqual('WVW', I.WMI);
  finally
    I.Free;
  end;
end;

procedure TVINInspectorTests.ShortcutPropertiesMirrorInfo;
var I: TOBDVINInspector;
begin
  I := TOBDVINInspector.Create(nil);
  try
    I.VIN := '1FTFW1ETJDFC10312';
    Assert.IsTrue(I.Valid);
    Assert.AreEqual('1FT',           I.WMI);
    Assert.AreEqual('North America', I.RegionName);
    Assert.AreEqual(I.Info.CheckDigit, I.CheckDigit);
    Assert.AreEqual(I.Info.ModelYear,  I.ModelYear);
    // Real vPIC says GVWR class 2F -> IsCommercial via heuristic.
    Assert.IsTrue(I.IsCommercial);
  finally
    I.Free;
  end;
end;

procedure TVINInspectorTests.InvalidVIN_FiresOnInvalid;
var I: TOBDVINInspector;
begin
  I := TOBDVINInspector.Create(nil);
  try
    I.OnInvalid := HandleInvalid;
    I.VIN := 'TOO_SHORT';
    Assert.IsTrue(FInvalid);
    Assert.IsNotEmpty(FCapturedReason);
    Assert.IsFalse(I.Valid);
  finally
    I.Free;
  end;
end;

procedure TVINInspectorTests.AutoDecodeOff_DoesNothingUntilDecodeNow;
var I: TOBDVINInspector;
begin
  I := TOBDVINInspector.Create(nil);
  try
    I.AutoDecode := False;
    I.OnDecoded  := HandleDecoded;
    I.VIN := 'WVWZZZ1KZ7W123456';
    Assert.IsFalse(FDecoded, 'OnDecoded must NOT fire with AutoDecode=False');
    Assert.IsFalse(I.Valid);
    I.DecodeNow;
    Assert.IsTrue(FDecoded);
    Assert.IsTrue(I.Valid);
    Assert.AreEqual('WVW', I.WMI);
  finally
    I.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVINInspectorTests);

end.
