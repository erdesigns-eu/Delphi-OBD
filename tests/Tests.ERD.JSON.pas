//------------------------------------------------------------------------------
//  Tests.ERD.JSON
//
//  Checked JSON shape and ownership regressions.
//------------------------------------------------------------------------------
unit Tests.ERD.JSON;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TJSONShapeTests = class
  public
    [Test] procedure ObjectRootIsReturned;
    [Test] procedure ArrayRootIsRejected;
    [Test] procedure InvalidDocumentIsRejected;
    [Test] procedure BorrowedArrayIsRejectedAndStillOwnedByCaller;
    [Test] procedure NonStringValueIsRejected;
    [Test] procedure StringValueIsReturned;
  end;

implementation

uses
  System.JSON, ERD.Types, ERD.JSON;

procedure TJSONShapeTests.ObjectRootIsReturned;
var Obj: TJSONObject;
begin
  Obj := ParseOBDJSONObject('{"vendor":"test"}');
  try
    Assert.AreEqual('test', Obj.GetValue<string>('vendor'));
  finally
    Obj.Free;
  end;
end;

procedure TJSONShapeTests.ArrayRootIsRejected;
begin
  Assert.WillRaise(procedure begin ParseOBDJSONObject('[]'); end, EOBDConfig);
end;

procedure TJSONShapeTests.InvalidDocumentIsRejected;
begin
  Assert.WillRaise(procedure begin ParseOBDJSONObject('{'); end, EOBDConfig);
end;

procedure TJSONShapeTests.BorrowedArrayIsRejectedAndStillOwnedByCaller;
var Arr: TJSONArray;
begin
  Arr := TJSONArray.Create;
  try
    Assert.WillRaise(procedure begin RequireOBDJSONObject(Arr); end, EOBDConfig);
    Assert.AreEqual(0, Arr.Count);
  finally
    Arr.Free;
  end;
end;

procedure TJSONShapeTests.NonStringValueIsRejected;
var Value: TJSONNumber;
begin
  Value := TJSONNumber.Create(42);
  try
    Assert.WillRaise(procedure begin RequireOBDJSONString(Value); end, EOBDConfig);
  finally
    Value.Free;
  end;
end;

procedure TJSONShapeTests.StringValueIsReturned;
var Value: TJSONString;
begin
  Value := TJSONString.Create('test');
  try
    Assert.AreEqual('test', RequireOBDJSONString(Value));
  finally
    Value.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TJSONShapeTests);

end.
