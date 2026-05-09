//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.Types
//
//  Hex helper round-trips and request / response defaults.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4a initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.Types;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>Hex round-trip and record-default coverage.</summary>
  [TestFixture]
  TProtocolTypesTests = class
  public
    /// <summary>BytesToHex emits space-separated upper-case hex.</summary>
    [Test] procedure BytesToHexUpperCaseSpaces;
    /// <summary>HexToBytes parses spaces, CR/LF and tabs as
    /// separators.</summary>
    [Test] procedure HexToBytesTolerantOfWhitespace;
    /// <summary>HexToBytes returns empty for an empty / whitespace
    /// string.</summary>
    [Test] procedure HexToBytesEmptyOnNoBytes;
    /// <summary>HexToBytes is case-insensitive.</summary>
    [Test] procedure HexToBytesCaseInsensitive;
    /// <summary>BytesToHex / HexToBytes round-trip a 16-byte payload.</summary>
    [Test] procedure RoundTrip16Bytes;
    /// <summary>MakeOBDRequest defaults are sane.</summary>
    [Test] procedure RequestDefaultsAreSane;
    /// <summary>MakeOBDResponse defaults are sane.</summary>
    [Test] procedure ResponseDefaultsAreSane;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types;

procedure TProtocolTypesTests.BytesToHexUpperCaseSpaces;
begin
  Assert.AreEqual('01 0C',  BytesToHex(TBytes.Create($01, $0C)));
  Assert.AreEqual('AB CD EF', BytesToHex(TBytes.Create($AB, $CD, $EF)));
  Assert.AreEqual('',       BytesToHex(nil));
end;

procedure TProtocolTypesTests.HexToBytesTolerantOfWhitespace;
var
  B: TBytes;
begin
  B := HexToBytes('22 F1' + #13#10 + '90');
  Assert.AreEqual<NativeInt>(3, Length(B));
  Assert.AreEqual<Byte>($22, B[0]);
  Assert.AreEqual<Byte>($F1, B[1]);
  Assert.AreEqual<Byte>($90, B[2]);

  B := HexToBytes(#9'7F'#9'22'#9'31');
  Assert.AreEqual<NativeInt>(3, Length(B));
  Assert.AreEqual<Byte>($7F, B[0]);
  Assert.AreEqual<Byte>($22, B[1]);
  Assert.AreEqual<Byte>($31, B[2]);
end;

procedure TProtocolTypesTests.HexToBytesEmptyOnNoBytes;
begin
  Assert.AreEqual<NativeInt>(0, Length(HexToBytes('')));
  Assert.AreEqual<NativeInt>(0, Length(HexToBytes('   ')));
  Assert.AreEqual<NativeInt>(0, Length(HexToBytes('?? > >')));
end;

procedure TProtocolTypesTests.HexToBytesCaseInsensitive;
var
  Up, Lo, Mix: TBytes;
begin
  Up  := HexToBytes('AB CD');
  Lo  := HexToBytes('ab cd');
  Mix := HexToBytes('Ab cD');
  Assert.AreEqual<NativeInt>(2, Length(Up));
  Assert.AreEqual<Byte>($AB, Up[0]);
  Assert.AreEqual<Byte>($AB, Lo[0]);
  Assert.AreEqual<Byte>($AB, Mix[0]);
  Assert.AreEqual<Byte>($CD, Up[1]);
  Assert.AreEqual<Byte>($CD, Lo[1]);
  Assert.AreEqual<Byte>($CD, Mix[1]);
end;

procedure TProtocolTypesTests.RoundTrip16Bytes;
var
  Original, Decoded: TBytes;
  Hex: string;
  I: Integer;
begin
  SetLength(Original, 16);
  for I := 0 to 15 do
    Original[I] := Byte((I * 17 + 5) and $FF);
  Hex := BytesToHex(Original);
  Decoded := HexToBytes(Hex);
  Assert.AreEqual<NativeInt>(16, Length(Decoded));
  for I := 0 to 15 do
    Assert.AreEqual<Byte>(Original[I], Decoded[I]);
end;

procedure TProtocolTypesTests.RequestDefaultsAreSane;
var
  R: TOBDRequest;
begin
  R := MakeOBDRequest;
  Assert.AreEqual(Ord(apOBD2), Ord(R.Protocol));
  Assert.AreEqual<Byte>(0, R.ServiceID);
  Assert.AreEqual<NativeInt>(0, Length(R.Data));
  Assert.AreEqual('', R.HeaderOverride);
  Assert.AreEqual<Cardinal>(0, R.TimeoutMs);
end;

procedure TProtocolTypesTests.ResponseDefaultsAreSane;
var
  Resp: TOBDResponse;
begin
  Resp := MakeOBDResponse;
  Assert.AreEqual<Byte>(0, Resp.ServiceID);
  Assert.IsFalse(Resp.IsNegative);
  Assert.AreEqual<Byte>(0, Resp.NRC);
  Assert.AreEqual('', Resp.NRCText);
  Assert.AreEqual<Cardinal>(0, Resp.Elapsed);
end;

initialization
  TDUnitX.RegisterTestFixture(TProtocolTypesTests);

end.
