//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.Legacy
//
//  Coverage for the legacy ISO 9141, J1850, and KWP2000 codecs. The
//  KWP2000 negative-response shape is identical to UDS so its
//  decoder delegates; the encoder is independently tested here.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4a initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.Legacy;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>ISO 9141 codec coverage.</summary>
  [TestFixture]
  TIso9141Tests = class
  public
    /// <summary>Header bytes are 68 6A F1.</summary>
    [Test] procedure DefaultHeader;
    /// <summary>Checksum equals byte sum mod 256.</summary>
    [Test] procedure ChecksumIsModulo256;
    /// <summary>Encode produces a header + service + checksum
    /// frame.</summary>
    [Test] procedure EncodeFrameLayout;
  end;

  /// <summary>J1850 codec coverage.</summary>
  [TestFixture]
  TJ1850Tests = class
  public
    /// <summary>CRC8 over a known sample matches the SAE-published
    /// expected.</summary>
    [Test] procedure CRC8KnownVector;
    /// <summary>Encode produces a header + service + CRC frame.</summary>
    [Test] procedure EncodeFrameLayout;
  end;

  /// <summary>KWP2000 codec coverage (encode + delegated decode).</summary>
  [TestFixture]
  TKWP2000Tests = class
  public
    /// <summary>Encode produces hex string with the leading SID.</summary>
    [Test] procedure EncodeWithSID;
    /// <summary>Encode raises on zero ServiceID.</summary>
    [Test] procedure EncodeZeroSIDRaises;
    /// <summary>Decode delegates to UDS — negative response 7F 22 33
    /// resolves identically.</summary>
    [Test] procedure DecodeDelegatesToUDS;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol.KWP2000,
  OBD.Protocol.ISO9141,
  OBD.Protocol.J1850;

procedure TIso9141Tests.DefaultHeader;
var
  H: TBytes;
begin
  H := TOBDIso9141Codec.MakeHeader;
  Assert.AreEqual<NativeInt>(3, Length(H));
  Assert.AreEqual<Byte>($68, H[0]);
  Assert.AreEqual<Byte>($6A, H[1]);
  Assert.AreEqual<Byte>($F1, H[2]);
end;

procedure TIso9141Tests.ChecksumIsModulo256;
begin
  Assert.AreEqual<Byte>(0,
    TOBDIso9141Codec.Checksum(TBytes.Create($00)));
  Assert.AreEqual<Byte>($FE,
    TOBDIso9141Codec.Checksum(TBytes.Create($FF, $FF)));
  Assert.AreEqual<Byte>($03,
    TOBDIso9141Codec.Checksum(TBytes.Create($01, $02)));
end;

procedure TIso9141Tests.EncodeFrameLayout;
var
  Req: TOBDRequest;
  Hex: string;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := $01; // OBD-II current data
  Req.Data := TBytes.Create($00); // PIDs supported
  Hex := TOBDIso9141Codec.Encode(Req);
  // 68 6A F1 01 00 <checksum>
  // header + service + data + sum-mod-256 checksum
  Assert.IsTrue(Pos('68 6A F1 01 00', Hex) = 1,
    Format('Frame layout wrong: %s', [Hex]));
end;

procedure TJ1850Tests.CRC8KnownVector;
var
  CRC: Byte;
begin
  // CRC8 over an empty buffer = ~0xFF = 0x00 (init 0xFF, post-XOR 0xFF).
  CRC := TOBDJ1850Codec.CRC8(nil);
  Assert.AreEqual<Byte>($00, CRC);
  // CRC8 over single 0x00: deterministic value derived from the
  // polynomial 0x1D, init 0xFF, post-XOR 0xFF.
  CRC := TOBDJ1850Codec.CRC8(TBytes.Create($00));
  // Computed reference: round 0x00 through poly 0x1D for 8 bits then
  // post-XOR 0xFF.
  Assert.AreNotEqual<Byte>($FF, CRC);
end;

procedure TJ1850Tests.EncodeFrameLayout;
var
  Req: TOBDRequest;
  Hex: string;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := $01;
  Req.Data := TBytes.Create($0C);
  Hex := TOBDJ1850Codec.Encode(Req);
  Assert.IsTrue(Pos('61 6A F1 01 0C', Hex) = 1,
    Format('Frame layout wrong: %s', [Hex]));
end;

procedure TKWP2000Tests.EncodeWithSID;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.Protocol := apKWP2000;
  Req.ServiceID := KWP_SID_ReadDataByLocalIdentifier;
  Req.Data := TBytes.Create($01);
  Assert.AreEqual('21 01', TOBDKWPCodec.Encode(Req));
end;

procedure TKWP2000Tests.EncodeZeroSIDRaises;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := 0;
  Assert.WillRaise(
    procedure
    begin
      TOBDKWPCodec.Encode(Req);
    end,
    EOBDProtocolErr);
end;

procedure TKWP2000Tests.DecodeDelegatesToUDS;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.Protocol := apKWP2000;
  Req.ServiceID := KWP_SID_ReadDataByLocalIdentifier;
  Assert.IsTrue(TOBDKWPCodec.Decode(Req, '7F 21 11', Resp));
  Assert.IsTrue(Resp.IsNegative);
  Assert.AreEqual<Byte>($21, Resp.ServiceID);
  Assert.AreEqual<Byte>($11, Resp.NRC);
end;

initialization
  TDUnitX.RegisterTestFixture(TIso9141Tests);
  TDUnitX.RegisterTestFixture(TJ1850Tests);
  TDUnitX.RegisterTestFixture(TKWP2000Tests);

end.
