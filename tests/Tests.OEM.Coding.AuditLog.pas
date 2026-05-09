//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Coding.AuditLog
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Coding.AuditLog;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCodingAuditLogTests = class
  strict private
    FPath: string;
    FKey: TBytes;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure AppendCreatesVerifiableSingleRecord;
    [Test] procedure AppendChainsAcrossMultipleRecords;
    [Test] procedure TamperingByteFlipFlagsCorrectLine;
    [Test] procedure TamperingDeleteFlagsTheNextLine;
    [Test] procedure RestartFromExistingFileContinuesChain;
    [Test] procedure EmptyKeyAtConstructionRaises;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.IOUtils,
  OBD.OEM.Coding.AuditLog;

procedure TCodingAuditLogTests.Setup;
begin
  FPath := TPath.Combine(TPath.GetTempPath,
    'obd-audit-' + TGUID.NewGuid.ToString + '.log');
  FKey := TEncoding.UTF8.GetBytes('test-key-32-bytes-long-padding-x');
end;

procedure TCodingAuditLogTests.TearDown;
begin
  if TFile.Exists(FPath) then
    TFile.Delete(FPath);
end;

function MakeRec(const VIN: string; const Index: Integer): TOBDCodingAuditRecord;
begin
  Result := Default(TOBDCodingAuditRecord);
  Result.Timestamp := EncodeDate(2026, 5, 9) + EncodeTime(12, 0, Index, 0);
  Result.VIN := VIN;
  Result.ECU := 'BCM';
  Result.Block := 'LongCoding';
  Result.BeforeHex := '0102030405';
  Result.AfterHex := '01020304FF';
  Result.Operator := 'tester';
  Result.Reason := 'test#' + IntToStr(Index);
end;

procedure TCodingAuditLogTests.AppendCreatesVerifiableSingleRecord;
var
  Log: TOBDCodingAuditLog;
  Res: TOBDCodingAuditChainResult;
begin
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    Log.Append(MakeRec('VIN-001', 1));
    Res := Log.Verify;
    Assert.IsTrue(Res.Verified, Res.Reason);
    Assert.AreEqual(1, Res.TotalRecords);
  finally
    Log.Free;
  end;
end;

procedure TCodingAuditLogTests.AppendChainsAcrossMultipleRecords;
var
  Log: TOBDCodingAuditLog;
  Res: TOBDCodingAuditChainResult;
  I: Integer;
begin
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    for I := 1 to 5 do
      Log.Append(MakeRec('VIN-00' + IntToStr(I), I));
    Res := Log.Verify;
    Assert.IsTrue(Res.Verified, Res.Reason);
    Assert.AreEqual(5, Res.TotalRecords);
  finally
    Log.Free;
  end;
end;

procedure TCodingAuditLogTests.TamperingByteFlipFlagsCorrectLine;
var
  Log: TOBDCodingAuditLog;
  Lines: TStringList;
  Res: TOBDCodingAuditChainResult;
  I: Integer;
begin
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    for I := 1 to 3 do
      Log.Append(MakeRec('VIN-' + IntToStr(I), I));
  finally
    Log.Free;
  end;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FPath, TEncoding.UTF8);
    // Flip a digit in line 2's "after" field; the JSON stays valid but
    // the body no longer matches the stored hmac.
    Lines[1] := StringReplace(Lines[1], '"01020304FF"',
                              '"01020304FE"', [rfReplaceAll]);
    Lines.SaveToFile(FPath, TEncoding.UTF8);
  finally
    Lines.Free;
  end;

  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    Res := Log.Verify;
    Assert.IsFalse(Res.Verified);
    Assert.AreEqual(2, Res.FirstTamperLine);
  finally
    Log.Free;
  end;
end;

procedure TCodingAuditLogTests.TamperingDeleteFlagsTheNextLine;
var
  Log: TOBDCodingAuditLog;
  Lines: TStringList;
  Res: TOBDCodingAuditChainResult;
  I: Integer;
begin
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    for I := 1 to 3 do
      Log.Append(MakeRec('VIN-' + IntToStr(I), I));
  finally
    Log.Free;
  end;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FPath, TEncoding.UTF8);
    Lines.Delete(1); // remove the second record
    Lines.SaveToFile(FPath, TEncoding.UTF8);
  finally
    Lines.Free;
  end;

  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    Res := Log.Verify;
    // After deletion, line 2 (formerly line 3) chains against the wrong
    // previous hmac.
    Assert.IsFalse(Res.Verified);
    Assert.AreEqual(2, Res.FirstTamperLine);
  finally
    Log.Free;
  end;
end;

procedure TCodingAuditLogTests.RestartFromExistingFileContinuesChain;
var
  Log: TOBDCodingAuditLog;
  Res: TOBDCodingAuditChainResult;
begin
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    Log.Append(MakeRec('VIN-A', 1));
    Log.Append(MakeRec('VIN-B', 2));
  finally
    Log.Free;
  end;
  // New instance reads the last hmac and chains the next record onto it.
  Log := TOBDCodingAuditLog.Create(FPath, FKey);
  try
    Log.Append(MakeRec('VIN-C', 3));
    Res := Log.Verify;
    Assert.IsTrue(Res.Verified, Res.Reason);
    Assert.AreEqual(3, Res.TotalRecords);
  finally
    Log.Free;
  end;
end;

procedure TCodingAuditLogTests.EmptyKeyAtConstructionRaises;
begin
  Assert.WillRaise(
    procedure
    var Log: TOBDCodingAuditLog;
    begin
      Log := TOBDCodingAuditLog.Create(FPath, nil);
      Log.Free;
    end,
    EOBDCodingAuditLog);
end;

initialization
  TDUnitX.RegisterTestFixture(TCodingAuditLogTests);

end.
