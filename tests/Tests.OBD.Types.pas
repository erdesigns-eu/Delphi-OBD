//------------------------------------------------------------------------------
//  Tests.OBD.Types
//
//  Smoke coverage for OBD.Types helpers.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Types;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Coverage for type-level helpers.</summary>
  [TestFixture]
  TTypesTests = class
  public
    /// <summary><c>MakeOBDValue</c> returns an empty value with a
    /// timestamp.</summary>
    [Test] procedure MakeValueIsEmptyAndTimestamped;
    /// <summary>The exception hierarchy descends from
    /// <c>EOBDError</c>.</summary>
    [Test] procedure ExceptionHierarchy;
    /// <summary><c>CanonicalDecoderName</c> normalises and falls back
    /// to <c>'raw'</c>.</summary>
    [Test] procedure CanonicalDecoderName;
  end;

implementation

uses
  System.SysUtils,
  OBD.Types;

procedure TTypesTests.MakeValueIsEmptyAndTimestamped;
var
  V: TOBDValue;
begin
  V := MakeOBDValue;
  Assert.AreEqual(Ord(vkEmpty), Ord(V.Kind));
  Assert.IsTrue(V.Timestamp > 0);
  Assert.AreEqual('', V.Name);
end;

procedure TTypesTests.ExceptionHierarchy;
begin
  Assert.IsTrue(EOBDConfig.InheritsFrom(EOBDError));
  Assert.IsTrue(EOBDNotConnected.InheritsFrom(EOBDError));
  Assert.IsTrue(EOBDProtocol.InheritsFrom(EOBDError));
  Assert.IsTrue(EOBDUnsupported.InheritsFrom(EOBDError));
  Assert.IsTrue(EOBDInternal.InheritsFrom(EOBDError));
end;

procedure TTypesTests.CanonicalDecoderName;
var
  D: TOBDPIDDescriptor;
begin
  FillChar(D, SizeOf(D), 0);
  D.DecoderName := '  Linear  ';
  Assert.AreEqual('linear', OBD.Types.CanonicalDecoderName(D));

  D.DecoderName := '';
  Assert.AreEqual('raw', OBD.Types.CanonicalDecoderName(D));
end;

initialization
  TDUnitX.RegisterTestFixture(TTypesTests);

end.
