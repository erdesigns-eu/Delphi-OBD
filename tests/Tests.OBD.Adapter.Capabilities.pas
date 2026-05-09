//------------------------------------------------------------------------------
//  Tests.OBD.Adapter.Capabilities
//
//  DUnitX coverage for TOBDAdapterCapabilityRegistry: built-in seed,
//  TryParseCapability tolerance, JSON loader.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Adapter.Capabilities;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Capability registry coverage.</summary>
  [TestFixture]
  TAdapterCapabilitiesTests = class
  strict private
    FTempFile: string;
  public
    [TearDown] procedure TearDown;

    /// <summary>Built-in elm327 entry exists with sane caps.</summary>
    [Test] procedure ElmBuiltinPresent;
    /// <summary>OBDLink CX entry advertises CAN-FD + ISO-TP-LF.</summary>
    [Test] procedure OBDLinkCXSupportsCANFD;
    /// <summary>TryParseCapability handles common synonyms.</summary>
    [Test] procedure ParseCapabilitySynonyms;
    /// <summary>TryParseCapability returns False for nonsense.</summary>
    [Test] procedure ParseCapabilityNonsense;
    /// <summary>LoadFromJSON merges entries from a temp file.</summary>
    [Test] procedure LoadFromJSONMerges;
    /// <summary>Registering with empty key raises.</summary>
    [Test] procedure RegisterEmptyKeyRaises;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  OBD.Types,
  OBD.Adapter.Types,
  OBD.Adapter.Capabilities;

procedure TAdapterCapabilitiesTests.TearDown;
begin
  if (FTempFile <> '') and TFile.Exists(FTempFile) then
    TFile.Delete(FTempFile);
  FTempFile := '';
end;

procedure TAdapterCapabilitiesTests.ElmBuiltinPresent;
var
  E: TOBDAdapterCapabilityEntry;
begin
  Assert.IsTrue(TOBDAdapterCapabilityRegistry.Default.TryFind('elm327', E));
  Assert.AreEqual('elm327', E.AdapterKey);
  Assert.IsTrue(acCAN in E.Capabilities);
  Assert.IsTrue(acVoltageMonitor in E.Capabilities);
end;

procedure TAdapterCapabilitiesTests.OBDLinkCXSupportsCANFD;
var
  E: TOBDAdapterCapabilityEntry;
begin
  Assert.IsTrue(TOBDAdapterCapabilityRegistry.Default.TryFind('obdlink_cx', E));
  Assert.IsTrue(acCANFD in E.Capabilities);
  Assert.IsTrue(acISOTPLongFrame in E.Capabilities);
  Assert.AreEqual<Cardinal>(62, E.MaxIsoTpFrameBytes);
end;

procedure TAdapterCapabilitiesTests.ParseCapabilitySynonyms;
var
  Cap: TOBDAdapterCapability;
begin
  Assert.IsTrue(TryParseCapability('ISO-TP', Cap));
  Assert.AreEqual(Ord(acISOTP), Ord(Cap));
  Assert.IsTrue(TryParseCapability('iso_tp', Cap));
  Assert.AreEqual(Ord(acISOTP), Ord(Cap));
  Assert.IsTrue(TryParseCapability(' KLine ', Cap));
  Assert.AreEqual(Ord(acKLine), Ord(Cap));
  Assert.IsTrue(TryParseCapability('K-Line', Cap));
  Assert.AreEqual(Ord(acKLine), Ord(Cap));
end;

procedure TAdapterCapabilitiesTests.ParseCapabilityNonsense;
var
  Cap: TOBDAdapterCapability;
begin
  Assert.IsFalse(TryParseCapability('not-a-capability', Cap));
end;

procedure TAdapterCapabilitiesTests.LoadFromJSONMerges;
var
  Loaded: Integer;
  E: TOBDAdapterCapabilityEntry;
begin
  FTempFile := TPath.Combine(TPath.GetTempPath,
    'caps-' + TGuid.NewGuid.ToString + '.json');
  TFile.WriteAllText(FTempFile,
    '{ "version": 1, "type": "adapter-capabilities", "entries": [' +
    '{ "adapter_key": "test_chip_42", "display_name": "Test 42", ' +
    '  "capabilities": ["CAN", "ISO-TP"], "max_iso_tp_frame_bytes": 7 }' +
    '] }', TEncoding.UTF8);
  Loaded := TOBDAdapterCapabilityRegistry.Default.LoadFromJSON(FTempFile);
  Assert.AreEqual(1, Loaded);
  Assert.IsTrue(TOBDAdapterCapabilityRegistry.Default.TryFind(
    'test_chip_42', E));
  Assert.AreEqual('Test 42', E.DisplayName);
  Assert.IsTrue(acCAN in E.Capabilities);
end;

procedure TAdapterCapabilitiesTests.RegisterEmptyKeyRaises;
var
  E: TOBDAdapterCapabilityEntry;
begin
  E.AdapterKey := '';
  E.DisplayName := 'x';
  E.Family := afELM327;
  E.Capabilities := [];
  E.MaxIsoTpFrameBytes := 0;
  Assert.WillRaise(
    procedure
    begin
      TOBDAdapterCapabilityRegistry.Default.Register(E);
    end,
    EOBDConfig);
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterCapabilitiesTests);

end.
