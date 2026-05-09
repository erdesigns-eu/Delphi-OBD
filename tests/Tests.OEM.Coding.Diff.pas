//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.Coding.Diff
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.Coding.Diff;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCodingDiffTests = class
  public
    [Test] procedure NoOpWhenCurrentEqualsTarget;
    [Test] procedure ByteLevelDiffSpotsChangedBytes;
    [Test] procedure FieldSchemaProducesNamedDiff;
    [Test] procedure ApplyWithoutConfirmRaises;
    [Test] procedure ApplyWithConfirmInvokesWriter;
    [Test] procedure NoOpApplyDoesNotInvokeWriter;
    [Test] procedure MismatchedLengthRaises;
    [Test] procedure UInt16FieldDiffsCorrectly;
  end;

implementation

uses
  System.SysUtils, OBD.OEM.Coding, OBD.OEM.Coding.Diff;

procedure TCodingDiffTests.NoOpWhenCurrentEqualsTarget;
var
  Bytes: TBytes;
  Plan: TOBDCodingPlan;
begin
  Bytes := TBytes.Create($01, $02, $03, $04);
  Plan := TOBDCodingPlan.Create(Bytes, Bytes);
  try
    Assert.IsTrue(Plan.IsNoOp);
    Assert.AreEqual(0, Length(Plan.Diff));
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.ByteLevelDiffSpotsChangedBytes;
var
  A, B: TBytes;
  Plan: TOBDCodingPlan;
begin
  A := TBytes.Create($00, $00, $00, $00);
  B := TBytes.Create($00, $FF, $00, $7F);
  Plan := TOBDCodingPlan.Create(A, B);
  try
    Assert.AreEqual(2, Length(Plan.Diff));
    Assert.AreEqual(1, Plan.Diff[0].ByteIndex);
    Assert.AreEqual(UInt32($FF), Plan.Diff[0].AfterValue);
    Assert.AreEqual(3, Plan.Diff[1].ByteIndex);
    Assert.AreEqual(UInt32($7F), Plan.Diff[1].AfterValue);
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.FieldSchemaProducesNamedDiff;
var
  A, B: TBytes;
  Schema: TOBDCodingSchema;
  Plan: TOBDCodingPlan;
begin
  A := TBytes.Create($00, $00);
  B := TBytes.Create($00, $08);  // bit 3 of byte 1 flipped
  SetLength(Schema, 1);
  Schema[0].Name := 'CornerLights';
  Schema[0].Description := 'Enable corner lighting on low beam';
  Schema[0].Kind := cfkBit;
  Schema[0].ByteIndex := 1;
  Schema[0].BitIndex := 3;
  Plan := TOBDCodingPlan.Create(A, B, Schema);
  try
    Assert.AreEqual(1, Length(Plan.Diff));
    Assert.AreEqual('CornerLights', Plan.Diff[0].FieldName);
    Assert.AreEqual(UInt32(0), Plan.Diff[0].BeforeValue);
    Assert.AreEqual(UInt32(1), Plan.Diff[0].AfterValue);
    Assert.IsTrue(Plan.AsText.Contains('CornerLights'));
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.ApplyWithoutConfirmRaises;
var
  Plan: TOBDCodingPlan;
begin
  Plan := TOBDCodingPlan.Create(
    TBytes.Create($00), TBytes.Create($01));
  try
    Assert.WillRaise(
      procedure begin Plan.Apply(False, procedure(const B: TBytes) begin end); end,
      EOBDCodingDiffError);
    Assert.IsFalse(Plan.Applied);
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.ApplyWithConfirmInvokesWriter;
var
  Plan: TOBDCodingPlan;
  Captured: TBytes;
  Called: Boolean;
begin
  Called := False;
  Plan := TOBDCodingPlan.Create(
    TBytes.Create($00, $00), TBytes.Create($AA, $BB));
  try
    Plan.Apply(True,
      procedure(const Bytes: TBytes)
      begin
        Called := True;
        Captured := Copy(Bytes);
      end);
    Assert.IsTrue(Called);
    Assert.IsTrue(Plan.Applied);
    Assert.AreEqual($AA, Integer(Captured[0]));
    Assert.AreEqual($BB, Integer(Captured[1]));
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.NoOpApplyDoesNotInvokeWriter;
var
  Plan: TOBDCodingPlan;
  Called: Boolean;
begin
  Called := False;
  Plan := TOBDCodingPlan.Create(TBytes.Create($00), TBytes.Create($00));
  try
    Plan.Apply(True,
      procedure(const Bytes: TBytes) begin Called := True; end);
    Assert.IsFalse(Called, 'Writer must not run for a no-op plan');
    Assert.IsTrue(Plan.Applied);
  finally
    Plan.Free;
  end;
end;

procedure TCodingDiffTests.MismatchedLengthRaises;
begin
  Assert.WillRaise(
    procedure
    var P: TOBDCodingPlan;
    begin
      P := TOBDCodingPlan.Create(TBytes.Create($00),
                                 TBytes.Create($00, $00));
      P.Free;
    end,
    EOBDCodingDiffError);
end;

procedure TCodingDiffTests.UInt16FieldDiffsCorrectly;
var
  A, B: TBytes;
  Schema: TOBDCodingSchema;
  Plan: TOBDCodingPlan;
begin
  A := TBytes.Create($01, $02, $00, $00);  // field@0 = 0x0102
  B := TBytes.Create($AB, $CD, $00, $00);  // field@0 = 0xABCD
  SetLength(Schema, 1);
  Schema[0].Name := 'TopSpeedLimit';
  Schema[0].Kind := cfkUInt16;
  Schema[0].ByteIndex := 0;
  Plan := TOBDCodingPlan.Create(A, B, Schema);
  try
    Assert.AreEqual(1, Length(Plan.Diff));
    Assert.AreEqual(UInt32($0102), Plan.Diff[0].BeforeValue);
    Assert.AreEqual(UInt32($ABCD), Plan.Diff[0].AfterValue);
  finally
    Plan.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCodingDiffTests);

end.
