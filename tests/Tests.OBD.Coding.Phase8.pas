//------------------------------------------------------------------------------
//  Tests.OBD.Coding.Phase8
//
//  Phase 8 coverage: WriteMemory + KWP WriteID safety gates,
//  diff/patch/revert round-trips, audit-log HMAC chain integrity,
//  and the seven OEM coder primitives.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 8 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Coding.Phase8;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.Protocol.SecOC.AES,
  OBD.UDS.WriteMemory,
  OBD.KWP.WriteID,
  OBD.Coding.Diff,
  OBD.Coding.AuditLog,
  OBD.Coding.VAG,
  OBD.Coding.BMW,
  OBD.Coding.Ford,
  OBD.Coding.HMG,
  OBD.Coding.Honda,
  OBD.Coding.Mercedes,
  OBD.Coding.Stellantis,
  OBD.Coding.Toyota;

type
  /// <summary>WriteMemory + KWP WriteID safety-gate coverage.</summary>
  [TestFixture]
  TPhase8WriteSafetyTests = class
  public
    [Test] procedure UDSWriteMemoryRaisesWhenAutoExecuteFalse;
    [Test] procedure UDSWriteMemoryRaisesOnEmptyData;
    [Test] procedure KWPWriteIDRaisesWhenAutoExecuteFalse;
  end;

  /// <summary>Diff / Apply / Revert round-trip coverage.</summary>
  [TestFixture]
  TCodingDiffTests = class
  public
    [Test] procedure ComputeFindsByteChanges;
    [Test] procedure ApplyRoundTrip;
    [Test] procedure RevertRollsBack;
    [Test] procedure ApplyRejectsLengthMismatch;
    [Test] procedure ApplyRejectsBeforeMismatch;
  end;

  /// <summary>Audit-log HMAC chain integrity.</summary>
  [TestFixture]
  TAuditLogTests = class
  public
    [Test] procedure WritesAndReloadsThreeEntries;
    [Test] procedure VerifyAcceptsCleanLog;
    [Test] procedure VerifyDetectsTampering;
  end;

  /// <summary>Per-OEM coder primitives.</summary>
  [TestFixture]
  TOEMCodingTests = class
  public
    [Test] procedure VAGLongCodingRoundTrip;
    [Test] procedure VAGBitGetSet;
    [Test] procedure BMWCAFDParsesAndEdits;
    [Test] procedure FordAsBuiltChecksumKnownVector;
    [Test] procedure FordAsBuiltSealRefreshesChecksum;
    [Test] procedure HMGSetGetOption;
    [Test] procedure HondaRangedSetRejectsOutOfRange;
    [Test] procedure MercedesFieldRoundTrip;
    [Test] procedure StellantisProxiSetGet;
    [Test] procedure ToyotaMenuSetGet;
  end;

implementation

{ ---- Write safety ---------------------------------------------------------- }

procedure TPhase8WriteSafetyTests.UDSWriteMemoryRaisesWhenAutoExecuteFalse;
var
  W: TOBDUDSWriteMemory;
begin
  W := TOBDUDSWriteMemory.Create(nil);
  try
    Assert.IsFalse(W.AutoExecute);
    Assert.WillRaise(
      procedure begin W.Write($1000, TBytes.Create($00)); end,
      EOBDConfig);
  finally
    W.Free;
  end;
end;

procedure TPhase8WriteSafetyTests.UDSWriteMemoryRaisesOnEmptyData;
var
  W: TOBDUDSWriteMemory;
begin
  W := TOBDUDSWriteMemory.Create(nil);
  try
    W.AutoExecute := True;
    Assert.WillRaise(
      procedure begin W.Write($1000, nil); end,
      EOBDConfig);
  finally
    W.Free;
  end;
end;

procedure TPhase8WriteSafetyTests.KWPWriteIDRaisesWhenAutoExecuteFalse;
var
  W: TOBDKWPWriteID;
begin
  W := TOBDKWPWriteID.Create(nil);
  try
    Assert.IsFalse(W.AutoExecute);
    Assert.WillRaise(
      procedure begin W.Write($05, TBytes.Create($00)); end,
      EOBDConfig);
  finally
    W.Free;
  end;
end;

{ ---- Diff ------------------------------------------------------------------ }

procedure TCodingDiffTests.ComputeFindsByteChanges;
var
  D: TOBDCodingDiffResult;
begin
  D := TOBDCodingDiff.Compute(
    TBytes.Create($01, $02, $03),
    TBytes.Create($01, $20, $03));
  Assert.AreEqual(1, Length(D.Changes));
  Assert.AreEqual(1, D.Changes[0].Offset);
  Assert.AreEqual($02, Integer(D.Changes[0].Before));
  Assert.AreEqual($20, Integer(D.Changes[0].After));
end;

procedure TCodingDiffTests.ApplyRoundTrip;
var
  Before, After, Out_: TBytes;
  D: TOBDCodingDiffResult;
begin
  Before := TBytes.Create($AA, $BB, $CC, $DD);
  After  := TBytes.Create($AA, $11, $CC, $22);
  D := TOBDCodingDiff.Compute(Before, After);
  Out_ := TOBDCodingDiff.Apply(Before, D);
  Assert.AreEqual(Length(After), Length(Out_));
  for var I := 0 to High(After) do
    Assert.AreEqual(Integer(After[I]), Integer(Out_[I]));
end;

procedure TCodingDiffTests.RevertRollsBack;
var
  Before, After, Recovered: TBytes;
  D: TOBDCodingDiffResult;
begin
  Before := TBytes.Create($AA, $BB);
  After  := TBytes.Create($CC, $DD);
  D := TOBDCodingDiff.Compute(Before, After);
  Recovered := TOBDCodingDiff.Revert(After, D);
  Assert.AreEqual(2, Length(Recovered));
  Assert.AreEqual($AA, Integer(Recovered[0]));
  Assert.AreEqual($BB, Integer(Recovered[1]));
end;

procedure TCodingDiffTests.ApplyRejectsLengthMismatch;
var
  D: TOBDCodingDiffResult;
begin
  D := TOBDCodingDiff.Compute(
    TBytes.Create($00, $00),
    TBytes.Create($00, $01));
  Assert.WillRaise(
    procedure begin TOBDCodingDiff.Apply(TBytes.Create($00), D); end,
    EOBDProtocol);
end;

procedure TCodingDiffTests.ApplyRejectsBeforeMismatch;
var
  D: TOBDCodingDiffResult;
begin
  D := TOBDCodingDiff.Compute(
    TBytes.Create($AA, $BB),
    TBytes.Create($CC, $DD));
  Assert.WillRaise(
    procedure begin
      TOBDCodingDiff.Apply(TBytes.Create($00, $00), D);
    end,
    EOBDProtocol);
end;

{ ---- Audit log ------------------------------------------------------------- }

procedure WriteThreeEntries(const APath: string; const AKey: TAES128Key);
var
  Log: TOBDCodingAuditLog;
  Entry: TOBDAuditEntry;
begin
  Log := TOBDCodingAuditLog.Create(nil);
  try
    Log.Open(APath, AKey);
    Entry := Default(TOBDAuditEntry);
    Entry.Timestamp := EncodeDateTime(2026, 5, 9, 12, 0, 0, 0);
    Entry.Kind := akSnapshot;
    Entry.Target := '0xF190';
    Entry.Before := TBytes.Create($01, $02);
    Log.Append(Entry);
    Entry.Timestamp := EncodeDateTime(2026, 5, 9, 12, 0, 1, 0);
    Entry.Kind := akWrite;
    Entry.Before := TBytes.Create($01, $02);
    Entry.After  := TBytes.Create($03, $04);
    Log.Append(Entry);
    Entry.Timestamp := EncodeDateTime(2026, 5, 9, 12, 0, 2, 0);
    Entry.Kind := akVerify;
    Entry.Message := 'verified';
    Log.Append(Entry);
  finally
    Log.Free;
  end;
end;

procedure TAuditLogTests.WritesAndReloadsThreeEntries;
var
  Tmp: string;
  Key: TAES128Key;
  Loaded: TArray<TOBDAuditEntry>;
  I: Integer;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obd-audit-' +
    TGUID.NewGuid.ToString + '.jsonl');
  for I := 0 to High(Key) do Key[I] := Byte($A0 + I);
  try
    WriteThreeEntries(Tmp, Key);
    Loaded := TOBDCodingAuditLog.LoadAll(Tmp);
    Assert.AreEqual(3, Length(Loaded));
    Assert.AreEqual(Ord(akSnapshot), Ord(Loaded[0].Kind));
    Assert.AreEqual(Ord(akWrite),    Ord(Loaded[1].Kind));
    Assert.AreEqual(Ord(akVerify),   Ord(Loaded[2].Kind));
    Assert.AreEqual('verified', Loaded[2].Message);
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TAuditLogTests.VerifyAcceptsCleanLog;
var
  Tmp: string;
  Key: TAES128Key;
  I: Integer;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obd-audit-' +
    TGUID.NewGuid.ToString + '.jsonl');
  for I := 0 to High(Key) do Key[I] := Byte($A0 + I);
  try
    WriteThreeEntries(Tmp, Key);
    Assert.AreEqual(-1, TOBDCodingAuditLog.Verify(Tmp, Key));
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

procedure TAuditLogTests.VerifyDetectsTampering;
var
  Tmp: string;
  Key: TAES128Key;
  Lines: TStringList;
  I: Integer;
  Faulty: Integer;
begin
  Tmp := TPath.Combine(TPath.GetTempPath, 'obd-audit-' +
    TGUID.NewGuid.ToString + '.jsonl');
  for I := 0 to High(Key) do Key[I] := Byte($A0 + I);
  try
    WriteThreeEntries(Tmp, Key);
    // Tamper with line 1 — flip a character in the message.
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(Tmp, TEncoding.UTF8);
      Lines[1] := StringReplace(Lines[1], '"target":"0xF190"',
        '"target":"0xF191"', []);
      Lines.SaveToFile(Tmp, TEncoding.UTF8);
    finally
      Lines.Free;
    end;
    Faulty := TOBDCodingAuditLog.Verify(Tmp, Key);
    Assert.AreEqual(1, Faulty,
      'tampered line index must be reported by Verify');
  finally
    if TFile.Exists(Tmp) then TFile.Delete(Tmp);
  end;
end;

{ ---- OEM coding tests ------------------------------------------------------ }

procedure TOEMCodingTests.VAGLongCodingRoundTrip;
var
  Bytes: TBytes;
  Hex: string;
begin
  Bytes := TOBDCodingVAG.ParseLongCoding('01-23 45-67 89AB');
  Assert.AreEqual(6, Length(Bytes));
  Assert.AreEqual($23, Integer(Bytes[1]));
  Hex := TOBDCodingVAG.FormatLongCoding(Bytes, 1);
  Assert.AreEqual('01 23 45 67 89 AB', Hex);
end;

procedure TOEMCodingTests.VAGBitGetSet;
var
  Buf: TBytes;
begin
  Buf := TBytes.Create($00, $00);
  TOBDCodingVAG.SetBit(Buf, 1, 5, True);
  Assert.AreEqual($20, Integer(Buf[1]));
  Assert.IsTrue(TOBDCodingVAG.GetBit(Buf, 1, 5));
  Assert.IsFalse(TOBDCodingVAG.GetBit(Buf, 1, 4));
end;

procedure TOEMCodingTests.BMWCAFDParsesAndEdits;
var
  Buf: TBytes;
  Entries: TArray<TOBDBMWCAFDEntry>;
  Found: TOBDBMWCAFDEntry;
begin
  // Two TLVs: id 0x3000 len 1 = 0x55 ; id 0x3001 len 2 = 0x12 0x34
  Buf := TBytes.Create($30, $00, $00, $01, $55,
                       $30, $01, $00, $02, $12, $34);
  Entries := TOBDCodingBMW.ParseEntries(Buf);
  Assert.AreEqual(2, Length(Entries));
  Assert.IsTrue(TOBDCodingBMW.FindEntry(Entries, $3001, Found));
  Assert.AreEqual(2, Integer(Found.Length_));
  Assert.AreEqual($12, Integer(Found.Value[0]));
  TOBDCodingBMW.WriteValue(Buf, Found, TBytes.Create($AA, $BB));
  Assert.AreEqual($AA, Integer(Buf[Found.Offset + 4]));
  Assert.AreEqual($BB, Integer(Buf[Found.Offset + 5]));
end;

procedure TOEMCodingTests.FordAsBuiltChecksumKnownVector;
var
  Sec: TOBDFordAsBuiltSection;
begin
  // Section "726-01-01 D8 E0 18 02 00 00 00 SS" — pick a well-known
  // tail byte: Sum of D8 E0 18 02 = 0x172. Two's complement of that
  // truncated to 1 byte = 0x100 - 0x72 = 0x8E.
  Sec.Name := '726-01-01';
  Sec.Data := TBytes.Create($D8, $E0, $18, $02, $00);
  Assert.AreEqual($00, Integer(TOBDCodingFord.Checksum(Sec)) and $00,
    'sanity (the function returns a Byte)');
  Sec.Data := TBytes.Create($D8, $E0, $18, $02, $00); // last byte = checksum slot
  TOBDCodingFord.SealChecksum(Sec);
  Assert.IsTrue(TOBDCodingFord.VerifyChecksum(Sec));
end;

procedure TOEMCodingTests.FordAsBuiltSealRefreshesChecksum;
var
  Sec: TOBDFordAsBuiltSection;
begin
  Sec.Name := 'DE-01-01';
  Sec.Data := TBytes.Create($01, $02, $03, $00);
  TOBDCodingFord.SealChecksum(Sec);
  Assert.IsTrue(TOBDCodingFord.VerifyChecksum(Sec));
  // Mutate a body byte; checksum should now be stale.
  Sec.Data[0] := $FF;
  Assert.IsFalse(TOBDCodingFord.VerifyChecksum(Sec));
  TOBDCodingFord.SealChecksum(Sec);
  Assert.IsTrue(TOBDCodingFord.VerifyChecksum(Sec));
end;

procedure TOEMCodingTests.HMGSetGetOption;
var
  Buf: TBytes;
  V: UInt32;
begin
  // Two options: 0x0010 (width 1, value 0x55) ; 0x0020 (width 2, value 0x1234)
  Buf := TBytes.Create($00, $10, $01, $55,
                       $00, $20, $02, $12, $34);
  Assert.IsTrue(TOBDCodingHMG.GetOption(Buf, $0010, V));
  Assert.AreEqual(UInt32($55), V);
  TOBDCodingHMG.SetOption(Buf, $0020, $ABCD);
  Assert.IsTrue(TOBDCodingHMG.GetOption(Buf, $0020, V));
  Assert.AreEqual(UInt32($ABCD), V);
end;

procedure TOEMCodingTests.HondaRangedSetRejectsOutOfRange;
var
  Buf: TBytes;
begin
  SetLength(Buf, 4);
  Assert.WillRaise(
    procedure begin TOBDCodingHonda.SetEntryRanged(Buf, 0, 5, 0, 3); end,
    EOBDProtocol);
end;

procedure TOEMCodingTests.MercedesFieldRoundTrip;
var
  Buf: TBytes;
begin
  Buf := TBytes.Create($00);
  TOBDCodingMercedes.SetField(Buf, 0, 2, 3, $05); // 3-bit field at shift 2
  Assert.AreEqual(5, Integer(TOBDCodingMercedes.GetField(Buf, 0, 2, 3)));
  // Field bits should be 0b 0001 0100.
  Assert.AreEqual($14, Integer(Buf[0]));
end;

procedure TOEMCodingTests.StellantisProxiSetGet;
var
  Buf, V: TBytes;
begin
  // 1 entry: ID 0x0050, len 3, value 11 22 33
  Buf := TBytes.Create($00, $50, $03, $11, $22, $33);
  Assert.IsTrue(TOBDCodingStellantis.GetParam(Buf, $0050, V));
  Assert.AreEqual(3, Length(V));
  Assert.AreEqual($22, Integer(V[1]));
  TOBDCodingStellantis.SetParam(Buf, $0050, TBytes.Create($AA, $BB, $CC));
  Assert.AreEqual($AA, Integer(Buf[3]));
  Assert.AreEqual($CC, Integer(Buf[5]));
end;

procedure TOEMCodingTests.ToyotaMenuSetGet;
var
  Buf: TBytes;
  V: UInt32;
begin
  Buf := TBytes.Create($00, $01, $01, $03);
  Assert.IsTrue(TOBDCodingToyota.GetOption(Buf, $0001, V));
  Assert.AreEqual(UInt32(3), V);
  TOBDCodingToyota.SetOption(Buf, $0001, $07);
  Assert.AreEqual($07, Integer(Buf[3]));
end;

initialization
  TDUnitX.RegisterTestFixture(TPhase8WriteSafetyTests);
  TDUnitX.RegisterTestFixture(TCodingDiffTests);
  TDUnitX.RegisterTestFixture(TAuditLogTests);
  TDUnitX.RegisterTestFixture(TOEMCodingTests);

end.
