//------------------------------------------------------------------------------
//  Tests.OBD.Coding.Phase8b
//
//  Phase 8 follow-up coverage: schema-validated option catalogue,
//  TOBDCodingSession dry-run mode, RLE diff round-trip, BMW /
//  Mercedes / Stellantis component-protection safety surface, and
//  the VAG .lbl label-file parser.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 follow-up.
//------------------------------------------------------------------------------

unit Tests.OBD.Coding.Phase8b;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Coding.OptionCatalog,
  OBD.Coding.DiffRLE,
  OBD.Coding.LabelFile.VAG,
  OBD.OEM.ComponentProtection.BMW,
  OBD.OEM.ComponentProtection.Mercedes,
  OBD.OEM.ComponentProtection.Stellantis;

type
  /// <summary>OEM coding-option catalogue loader.</summary>
  [TestFixture]
  TOptionCatalogTests = class
  public
    [Test] procedure LoadsByteBitOption;
    [Test] procedure LoadsTLVOption;
    [Test] procedure LoadsValueLabels;
    [Test] procedure RejectsWrongVersion;
    [Test] procedure RejectsMissingRequiredField;
    [Test] procedure UnknownAddressingKindRejected;
    [Test] procedure ReplaceSameVendorModule;
  end;

  /// <summary>Run-length-encoded diff coverage.</summary>
  [TestFixture]
  TDiffRLETests = class
  public
    [Test] procedure ComputesContiguousRun;
    [Test] procedure SeparateRunsForGaps;
    [Test] procedure GapMergeWhenAGapAllows;
    [Test] procedure ApplyRoundTrip;
    [Test] procedure RevertRollsBack;
    [Test] procedure ApplyRejectsLengthMismatch;
    [Test] procedure TransferSizeSums;
  end;

  /// <summary>BMW / Mercedes / Stellantis CP safety paths.</summary>
  [TestFixture]
  TComponentProtectionTests = class
  public
    [Test] procedure BMWUnlockRaisesWithoutAuthFunc;
    [Test] procedure BMWUnlockRaisesWithoutDataIO;
    [Test] procedure MercedesUnlockRaisesWithoutAuthFunc;
    [Test] procedure StellantisUnlockRaisesWithoutAuthFunc;
    [Test] procedure DefaultDIDsMatchVendor;
  end;

  /// <summary>VAG .lbl parser.</summary>
  [TestFixture]
  TVAGLabelFileTests = class
  public
    [Test] procedure ParsesBitField;
    [Test] procedure ParsesBitRangeWithValueTable;
    [Test] procedure ParsesAdaptationChannel;
    [Test] procedure ParsesByteFieldB0;
    [Test] procedure SkipsCommentsAndEmptyLines;
  end;

implementation

{ ---- Option catalogue ------------------------------------------------------- }

procedure TOptionCatalogTests.LoadsByteBitOption;
var
  Tmp: string;
  Doc: TOBDCodingOptionDoc;
  Entry: TOBDOptionEntry;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "vag", "module": "17_instruments",' +
    '  "options": [' +
    '    { "name": "needle_sweep_on_start",' +
    '      "addressing": { "kind": "byte_bit", "byte": 5, "bit": 3 } }' +
    '  ] }', TEncoding.UTF8);
  try
    TOBDCodingOptionCatalog.Default.Clear;
    TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
    Assert.IsTrue(TOBDCodingOptionCatalog.Default.TryGet(
      ovVAG, '17_instruments', Doc));
    Assert.AreEqual(1, Length(Doc.Options));
    Assert.IsTrue(TOBDCodingOptionCatalog.Default.TryFindOption(
      ovVAG, '17_instruments', 'needle_sweep_on_start', Entry));
    Assert.AreEqual(Ord(oaByteBit), Ord(Entry.Addressing.Kind));
    Assert.AreEqual(5, Entry.Addressing.Byte_);
    Assert.AreEqual(3, Entry.Addressing.Bit);
  finally
    TFile.Delete(Tmp);
    TOBDCodingOptionCatalog.Default.Clear;
  end;
end;

procedure TOptionCatalogTests.LoadsTLVOption;
var
  Tmp: string;
  Entry: TOBDOptionEntry;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "bmw", "module": "FEM_BODY",' +
    '  "options": [' +
    '    { "name": "DRL_enable",' +
    '      "addressing": { "kind": "tlv_id", "id": 12345, "bit": 0 } }' +
    '  ] }', TEncoding.UTF8);
  try
    TOBDCodingOptionCatalog.Default.Clear;
    TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
    Assert.IsTrue(TOBDCodingOptionCatalog.Default.TryFindOption(
      ovBMW, 'FEM_BODY', 'DRL_enable', Entry));
    Assert.AreEqual(Ord(oaTlvID), Ord(Entry.Addressing.Kind));
    Assert.AreEqual(UInt32(12345), Entry.Addressing.ID);
    Assert.AreEqual(0, Entry.Addressing.Bit);
  finally
    TFile.Delete(Tmp);
    TOBDCodingOptionCatalog.Default.Clear;
  end;
end;

procedure TOptionCatalogTests.LoadsValueLabels;
var
  Tmp: string;
  Entry: TOBDOptionEntry;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "mercedes", "module": "EZS",' +
    '  "options": [' +
    '    { "name": "key_repeat_count",' +
    '      "addressing": { "kind": "byte_field", "byte": 1, "shift": 0, "width": 3 },' +
    '      "values": [' +
    '        { "raw": 0, "label": "Off" },' +
    '        { "raw": 1, "label": "Single" },' +
    '        { "raw": 2, "label": "Double" }' +
    '      ] }' +
    '  ] }', TEncoding.UTF8);
  try
    TOBDCodingOptionCatalog.Default.Clear;
    TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
    Assert.IsTrue(TOBDCodingOptionCatalog.Default.TryFindOption(
      ovMercedes, 'EZS', 'key_repeat_count', Entry));
    Assert.AreEqual(3, Length(Entry.Values));
    Assert.AreEqual('Double',
      TOBDCodingOptionCatalog.LookupLabel(Entry, 2));
    Assert.AreEqual('', TOBDCodingOptionCatalog.LookupLabel(Entry, 99));
  finally
    TFile.Delete(Tmp);
    TOBDCodingOptionCatalog.Default.Clear;
  end;
end;

procedure TOptionCatalogTests.RejectsWrongVersion;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 2, "vendor": "vag", "module": "x", "options": [] }',
    TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure begin
        TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
      end,
      EOBDConfig);
  finally
    TFile.Delete(Tmp);
  end;
end;

procedure TOptionCatalogTests.RejectsMissingRequiredField;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "vag", "module": "x",' +
    '  "options": [' +
    '    { "addressing": { "kind": "byte_bit", "byte": 0, "bit": 0 } }' +
    '  ] }', TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure begin
        TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
      end,
      EOBDConfig);
  finally
    TFile.Delete(Tmp);
  end;
end;

procedure TOptionCatalogTests.UnknownAddressingKindRejected;
var
  Tmp: string;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  TFile.WriteAllText(Tmp,
    '{ "version": 1, "vendor": "vag", "module": "x",' +
    '  "options": [' +
    '    { "name": "foo",' +
    '      "addressing": { "kind": "magic_word", "id": 1 } }' +
    '  ] }', TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure begin
        TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
      end,
      EOBDConfig);
  finally
    TFile.Delete(Tmp);
  end;
end;

procedure TOptionCatalogTests.ReplaceSameVendorModule;
var
  Tmp: string;
  Doc: TOBDCodingOptionDoc;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'cat-' +
    TGUID.NewGuid.ToString + '.json');
  try
    TOBDCodingOptionCatalog.Default.Clear;
    TFile.WriteAllText(Tmp,
      '{ "version": 1, "vendor": "vag", "module": "x",' +
      '  "options": [' +
      '    { "name": "first",' +
      '      "addressing": { "kind": "byte_bit", "byte": 0, "bit": 0 } }' +
      '  ] }', TEncoding.UTF8);
    TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
    TFile.WriteAllText(Tmp,
      '{ "version": 1, "vendor": "vag", "module": "x",' +
      '  "options": [' +
      '    { "name": "second",' +
      '      "addressing": { "kind": "byte_bit", "byte": 0, "bit": 1 } }' +
      '  ] }', TEncoding.UTF8);
    TOBDCodingOptionCatalog.Default.LoadFile(Tmp);
    Assert.AreEqual(1, TOBDCodingOptionCatalog.Default.Count,
      'reload of same (vendor, module) replaces, not appends');
    Assert.IsTrue(TOBDCodingOptionCatalog.Default.TryGet(
      ovVAG, 'x', Doc));
    Assert.AreEqual('second', Doc.Options[0].Name);
  finally
    TFile.Delete(Tmp);
    TOBDCodingOptionCatalog.Default.Clear;
  end;
end;

{ ---- RLE diff -------------------------------------------------------------- }

procedure TDiffRLETests.ComputesContiguousRun;
var
  D: TOBDCodingDiffRLEResult;
begin
  D := TOBDCodingDiffRLE.Compute(
    TBytes.Create($00, $11, $22, $33, $44),
    TBytes.Create($00, $AA, $BB, $CC, $44));
  Assert.AreEqual(1, Length(D.Runs));
  Assert.AreEqual(1, D.Runs[0].Offset);
  Assert.AreEqual(3, Length(D.Runs[0].After));
  Assert.AreEqual($AA, Integer(D.Runs[0].After[0]));
  Assert.AreEqual($CC, Integer(D.Runs[0].After[2]));
end;

procedure TDiffRLETests.SeparateRunsForGaps;
var
  D: TOBDCodingDiffRLEResult;
begin
  D := TOBDCodingDiffRLE.Compute(
    TBytes.Create($00, $11, $22, $33, $44),
    TBytes.Create($AA, $11, $22, $33, $BB),
    0);
  Assert.AreEqual(2, Length(D.Runs));
  Assert.AreEqual(0, D.Runs[0].Offset);
  Assert.AreEqual(4, D.Runs[1].Offset);
end;

procedure TDiffRLETests.GapMergeWhenAGapAllows;
var
  D: TOBDCodingDiffRLEResult;
begin
  D := TOBDCodingDiffRLE.Compute(
    TBytes.Create($AA, $00, $00, $BB),
    TBytes.Create($CC, $00, $00, $DD),
    4);
  // With a gap budget of 4, the two single-byte changes merge
  // into one run of length 4.
  Assert.AreEqual(1, Length(D.Runs));
  Assert.AreEqual(4, Length(D.Runs[0].After));
end;

procedure TDiffRLETests.ApplyRoundTrip;
var
  Before, After, Out_: TBytes;
  D: TOBDCodingDiffRLEResult;
  I: Integer;
begin
  Before := TBytes.Create($00, $11, $22, $33);
  After  := TBytes.Create($00, $AA, $22, $BB);
  D := TOBDCodingDiffRLE.Compute(Before, After);
  Out_ := TOBDCodingDiffRLE.Apply(Before, D);
  Assert.AreEqual(Length(After), Length(Out_));
  for I := 0 to High(After) do
    Assert.AreEqual(Integer(After[I]), Integer(Out_[I]));
end;

procedure TDiffRLETests.RevertRollsBack;
var
  Before, After, Recovered: TBytes;
  D: TOBDCodingDiffRLEResult;
begin
  Before := TBytes.Create($AA, $BB, $CC);
  After  := TBytes.Create($11, $22, $33);
  D := TOBDCodingDiffRLE.Compute(Before, After);
  Recovered := TOBDCodingDiffRLE.Revert(After, D);
  Assert.AreEqual(3, Length(Recovered));
  Assert.AreEqual($AA, Integer(Recovered[0]));
  Assert.AreEqual($CC, Integer(Recovered[2]));
end;

procedure TDiffRLETests.ApplyRejectsLengthMismatch;
var
  D: TOBDCodingDiffRLEResult;
begin
  D := TOBDCodingDiffRLE.Compute(
    TBytes.Create($00, $00),
    TBytes.Create($00, $01));
  Assert.WillRaise(
    procedure begin TOBDCodingDiffRLE.Apply(TBytes.Create($00), D); end,
    EOBDProtocol);
end;

procedure TDiffRLETests.TransferSizeSums;
var
  D: TOBDCodingDiffRLEResult;
begin
  D := TOBDCodingDiffRLE.Compute(
    TBytes.Create($00, $11, $22, $33, $44),
    TBytes.Create($AA, $11, $22, $33, $BB),
    0);
  Assert.AreEqual(2, TOBDCodingDiffRLE.TransferSize(D));
end;

{ ---- BMW / Mercedes / Stellantis CP ---------------------------------------- }

procedure TComponentProtectionTests.BMWUnlockRaisesWithoutAuthFunc;
var
  CP: TOBDComponentProtectionBMW;
begin
  CP := TOBDComponentProtectionBMW.Create(nil);
  try
    Assert.WillRaise(
      procedure begin CP.Unlock; end,
      EOBDConfig);
  finally
    CP.Free;
  end;
end;

procedure TComponentProtectionTests.BMWUnlockRaisesWithoutDataIO;
var
  CP: TOBDComponentProtectionBMW;
begin
  CP := TOBDComponentProtectionBMW.Create(nil);
  try
    CP.AuthFunc :=
      function(const C: TBytes): TBytes
      begin
        Result := TBytes.Create($00);
      end;
    Assert.WillRaise(
      procedure begin CP.Unlock; end,
      EOBDConfig);
  finally
    CP.Free;
  end;
end;

procedure TComponentProtectionTests.MercedesUnlockRaisesWithoutAuthFunc;
var
  CP: TOBDComponentProtectionMercedes;
begin
  CP := TOBDComponentProtectionMercedes.Create(nil);
  try
    Assert.WillRaise(
      procedure begin CP.Unlock; end,
      EOBDConfig);
  finally
    CP.Free;
  end;
end;

procedure TComponentProtectionTests.StellantisUnlockRaisesWithoutAuthFunc;
var
  CP: TOBDComponentProtectionStellantis;
begin
  CP := TOBDComponentProtectionStellantis.Create(nil);
  try
    Assert.WillRaise(
      procedure begin CP.Unlock; end,
      EOBDConfig);
  finally
    CP.Free;
  end;
end;

procedure TComponentProtectionTests.DefaultDIDsMatchVendor;
var
  BMW: TOBDComponentProtectionBMW;
  Mer: TOBDComponentProtectionMercedes;
  Stel: TOBDComponentProtectionStellantis;
begin
  BMW := TOBDComponentProtectionBMW.Create(nil);
  Mer := TOBDComponentProtectionMercedes.Create(nil);
  Stel := TOBDComponentProtectionStellantis.Create(nil);
  try
    Assert.AreEqual(BMW_CP_DID_STATUS,        Integer(BMW.StatusDID));
    Assert.AreEqual(MERCEDES_CP_DID_CHALLENGE,Integer(Mer.ChallengeDID));
    Assert.AreEqual(STELLANTIS_SGW_DID_AUTHORISATION,
      Integer(Stel.AuthorisationDID));
  finally
    Stel.Free;
    Mer.Free;
    BMW.Free;
  end;
end;

{ ---- VAG .lbl parser ------------------------------------------------------- }

procedure TVAGLabelFileTests.ParsesBitField;
const
  Src = '0,1,Front Fog Light Assist active';
var
  Lbl: TOBDVAGLabelFile;
begin
  Lbl := TOBDLabelFileVAG.Parse(Src);
  Assert.AreEqual(1, Length(Lbl.Entries));
  Assert.AreEqual(Ord(lkBitField), Ord(Lbl.Entries[0].Kind));
  Assert.AreEqual(0, Lbl.Entries[0].Byte_);
  Assert.AreEqual(1, Lbl.Entries[0].BitStart);
  Assert.AreEqual(1, Lbl.Entries[0].BitEnd);
  Assert.AreEqual('Front Fog Light Assist active',
    Lbl.Entries[0].Description);
end;

procedure TVAGLabelFileTests.ParsesBitRangeWithValueTable;
const
  Src = '7,2-3,Cornering Mode (0=Off, 1=City, 2=Highway)';
var
  Lbl: TOBDVAGLabelFile;
begin
  Lbl := TOBDLabelFileVAG.Parse(Src);
  Assert.AreEqual(1, Length(Lbl.Entries));
  Assert.AreEqual(2, Lbl.Entries[0].BitStart);
  Assert.AreEqual(3, Lbl.Entries[0].BitEnd);
  Assert.AreEqual(3, Length(Lbl.Entries[0].Values));
  Assert.AreEqual('Off',     Lbl.Entries[0].Values[0].Value);
  Assert.AreEqual('Highway', Lbl.Entries[0].Values[2].Value);
end;

procedure TVAGLabelFileTests.ParsesAdaptationChannel;
const
  Src = 'Adp;1;Idle Speed Setpoint';
var
  Lbl: TOBDVAGLabelFile;
begin
  Lbl := TOBDLabelFileVAG.Parse(Src);
  Assert.AreEqual(1, Length(Lbl.Entries));
  Assert.AreEqual(Ord(lkAdaptationChannel), Ord(Lbl.Entries[0].Kind));
  Assert.AreEqual(1, Lbl.Entries[0].Channel);
end;

procedure TVAGLabelFileTests.ParsesByteFieldB0;
const
  Src = '8,B0,Daytime Running Lights';
var
  Lbl: TOBDVAGLabelFile;
begin
  Lbl := TOBDLabelFileVAG.Parse(Src);
  Assert.AreEqual(Ord(lkByteField), Ord(Lbl.Entries[0].Kind));
  Assert.AreEqual(8, Lbl.Entries[0].Byte_);
  Assert.AreEqual(0, Lbl.Entries[0].BitStart);
  Assert.AreEqual(7, Lbl.Entries[0].BitEnd);
end;

procedure TVAGLabelFileTests.SkipsCommentsAndEmptyLines;
const
  Src =
    ';Header line 1' + sLineBreak +
    ';Header line 2' + sLineBreak +
    '' + sLineBreak +
    '0,0,Setting A' + sLineBreak +
    ';comment between rows' + sLineBreak +
    '0,1,Setting B';
var
  Lbl: TOBDVAGLabelFile;
begin
  Lbl := TOBDLabelFileVAG.Parse(Src);
  Assert.AreEqual(2, Length(Lbl.Entries));
  Assert.IsTrue(Lbl.Header.Contains('Header line 1'));
end;

initialization
  TDUnitX.RegisterTestFixture(TOptionCatalogTests);
  TDUnitX.RegisterTestFixture(TDiffRLETests);
  TDUnitX.RegisterTestFixture(TComponentProtectionTests);
  TDUnitX.RegisterTestFixture(TVAGLabelFileTests);

end.
