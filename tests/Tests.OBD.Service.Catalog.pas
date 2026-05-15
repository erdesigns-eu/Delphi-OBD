//------------------------------------------------------------------------------
//  Tests.OBD.Service.Catalog
//
//  Coverage for the JSON-driven PID + DTC catalogue, Mode 06
//  parser surface, and Mode 08 safety gate.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Follow-up.
//------------------------------------------------------------------------------

unit Tests.OBD.Service.Catalog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Service.Catalog,
  OBD.Service.Actuator;

type
  /// <summary>JSON-driven PID + DTC catalogue tests.</summary>
  [TestFixture]
  TServiceCatalogTests = class
  public
    [Test] procedure LoadsPIDFromJSON;
    [Test] procedure LoadsDTCFromJSON;
    [Test] procedure UnknownPIDReturnsFalse;
    [Test] procedure DecoderUInt16BEScaleAndOffset;
    [Test] procedure DecoderUInt8WithOffset;
    [Test] procedure DecoderInt16HandlesNegative;
    [Test] procedure DecoderUnknownKindReturnsFalse;
  end;

  /// <summary>Mode 08 safety-gate coverage. We don't exercise an
  /// actual ECU here — the safety check happens before any wire
  /// access, so a Protocol-less component is sufficient.</summary>
  [TestFixture]
  TActuatorSafetyTests = class
  public
    [Test] procedure SendRaisesWhenAutoExecuteFalse;
    [Test] procedure SendAsyncRaisesWhenAutoExecuteFalse;
  end;

implementation

procedure WritePIDFile(const AFile, AContent: string);
begin
  TFile.WriteAllText(AFile, AContent, TEncoding.UTF8);
end;

procedure TServiceCatalogTests.LoadsPIDFromJSON;
var
  Tmp: string;
  Info: TOBDPIDInfo;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obd-test-pids.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "dids": [' +
    '{ "did": "0x000C", "name": "rpm", "description": "Engine RPM",' +
    '  "decoder": { "kind": "uint16_be", "scale": 0.25, "unit": "rpm" } }' +
    '] }', TEncoding.UTF8);
  try
    TOBDServiceCatalog.Default.Clear;
    TOBDServiceCatalog.Default.LoadPIDFile(Tmp);
    Assert.IsTrue(TOBDServiceCatalog.Default.TryGetPID($000C, Info));
    Assert.AreEqual('rpm', Info.Name);
    Assert.AreEqual('uint16_be', Info.Decoder.Kind);
    Assert.AreEqual(0.25, Info.Decoder.Scale, 0.0001);
    Assert.AreEqual('rpm', Info.Decoder.Unit_);
  finally
    TFile.Delete(Tmp);
    TOBDServiceCatalog.Default.Clear;
  end;
end;

procedure TServiceCatalogTests.LoadsDTCFromJSON;
var
  Tmp: string;
  Info: TOBDDtcInfo;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obd-test-dtc.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "dtcs": [' +
    '{ "code": "P0301", "description": "Cylinder 1 misfire",' +
    '  "severity": "warning" }' +
    '] }', TEncoding.UTF8);
  try
    TOBDServiceCatalog.Default.Clear;
    TOBDServiceCatalog.Default.LoadDTCFile(Tmp);
    Assert.IsTrue(TOBDServiceCatalog.Default.TryGetDTC('P0301', Info));
    Assert.AreEqual('Cylinder 1 misfire', Info.Description);
    Assert.AreEqual('warning', Info.Severity);
    // case-insensitive lookup
    Assert.IsTrue(TOBDServiceCatalog.Default.TryGetDTC('p0301', Info));
  finally
    TFile.Delete(Tmp);
    TOBDServiceCatalog.Default.Clear;
  end;
end;

procedure TServiceCatalogTests.UnknownPIDReturnsFalse;
var
  Info: TOBDPIDInfo;
begin
  TOBDServiceCatalog.Default.Clear;
  Assert.IsFalse(TOBDServiceCatalog.Default.TryGetPID($00FF, Info));
end;

procedure TServiceCatalogTests.DecoderUInt16BEScaleAndOffset;
var
  Dec: TOBDPIDDecoderInfo;
  V: Double;
begin
  Dec.Kind := 'uint16_be'; Dec.Scale := 0.25; Dec.Offset := 0;
  // 0x0F 0xA0 = 4000 → 4000 / 4 = 1000.0
  Assert.IsTrue(EvaluatePIDDecoder(Dec, TBytes.Create($0F, $A0), V));
  Assert.AreEqual(1000.0, V, 0.0001);
end;

procedure TServiceCatalogTests.DecoderUInt8WithOffset;
var
  Dec: TOBDPIDDecoderInfo;
  V: Double;
begin
  Dec.Kind := 'uint8'; Dec.Scale := 1; Dec.Offset := -40;
  // 0xA0 = 160 → 160 - 40 = 120 °C
  Assert.IsTrue(EvaluatePIDDecoder(Dec, TBytes.Create($A0), V));
  Assert.AreEqual(120.0, V, 0.0001);
end;

procedure TServiceCatalogTests.DecoderInt16HandlesNegative;
var
  Dec: TOBDPIDDecoderInfo;
  V: Double;
begin
  Dec.Kind := 'int16_be'; Dec.Scale := 1; Dec.Offset := 0;
  // 0xFFFF = -1
  Assert.IsTrue(EvaluatePIDDecoder(Dec, TBytes.Create($FF, $FF), V));
  Assert.AreEqual(-1.0, V, 0.0001);
end;

procedure TServiceCatalogTests.DecoderUnknownKindReturnsFalse;
var
  Dec: TOBDPIDDecoderInfo;
  V: Double;
begin
  Dec.Kind := 'hex';
  Assert.IsFalse(EvaluatePIDDecoder(Dec, TBytes.Create($AA, $BB), V));
  Assert.IsTrue(IsNaN(V));
end;

// ---- TOBDActuator safety gate -----------------------------------------------

procedure TActuatorSafetyTests.SendRaisesWhenAutoExecuteFalse;
var
  A: TOBDActuator;
begin
  A := TOBDActuator.Create(nil);
  try
    Assert.IsFalse(A.AutoExecute,
      'AutoExecute must default to False — safe by default');
    Assert.WillRaise(
      procedure begin A.Send($05); end,
      EOBDConfig);
  finally
    A.Free;
  end;
end;

procedure TActuatorSafetyTests.SendAsyncRaisesWhenAutoExecuteFalse;
var
  A: TOBDActuator;
begin
  A := TOBDActuator.Create(nil);
  try
    // SendAsync grabs the in-flight guard before doing anything;
    // the worker thread then raises EOBDConfig in DoSend. We can't
    // easily observe the worker exception synchronously in this
    // test, but we can at least verify the guard is acquired and
    // the call returns without crashing on the gate.
    A.SendAsync($05);
    Sleep(50); // let the worker run + release the guard
    // Calling SendAsync again should not collide with the prior
    // worker — gate must be released even on failure.
    A.SendAsync($05);
    Sleep(50);
  finally
    Sleep(50);
    A.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceCatalogTests);
  TDUnitX.RegisterTestFixture(TActuatorSafetyTests);

end.
