//------------------------------------------------------------------------------
//  Tests.OBD.Errors
//
//  Smoke coverage for OBD.Errors. Verifies every defined error code
//  has a non-empty message and a Pascal-style identifier.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Errors;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Asserts the error-code resolver returns sane text for every
  ///   defined enum value.
  /// </summary>
  [TestFixture]
  TErrorTests = class
  public
    /// <summary>Every defined code maps to a non-empty message.</summary>
    [Test] procedure AllCodesHaveMessages;
    /// <summary>Every defined code maps to a non-empty identifier
    /// starting with 'oe'.</summary>
    [Test] procedure AllCodesHaveIdentifiers;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  OBD.Types,
  OBD.Errors;

procedure TErrorTests.AllCodesHaveMessages;
var
  Code: TOBDErrorCode;
begin
  for Code := Low(TOBDErrorCode) to High(TOBDErrorCode) do
    Assert.IsNotEmpty(OBDErrorCodeToMessage(Code),
      Format('Empty message for %s',
        [GetEnumName(TypeInfo(TOBDErrorCode), Ord(Code))]));
end;

procedure TErrorTests.AllCodesHaveIdentifiers;
var
  Code: TOBDErrorCode;
  Ident: string;
begin
  for Code := Low(TOBDErrorCode) to High(TOBDErrorCode) do
  begin
    Ident := OBDErrorCodeToIdent(Code);
    Assert.IsNotEmpty(Ident);
    Assert.IsTrue(Copy(Ident, 1, 2) = 'oe',
      Format('Identifier "%s" does not start with "oe"', [Ident]));
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TErrorTests);

end.
