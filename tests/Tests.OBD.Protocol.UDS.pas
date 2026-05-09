//------------------------------------------------------------------------------
//  Tests.OBD.Protocol.UDS
//
//  UDS codec coverage including NRC catalogue resolution.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 4a initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Protocol.UDS;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>UDS encoder / decoder coverage.</summary>
  [TestFixture]
  TUDSCodecTests = class
  public
    /// <summary>Encode of ReadDataByIdentifier(F190) is "22 F1 90".</summary>
    [Test] procedure EncodeReadDataByIdentifier;
    /// <summary>Encode raises when ServiceID is zero.</summary>
    [Test] procedure EncodeServiceZeroRaises;
    /// <summary>Decode positive response strips the SID into
    /// ServiceID and copies remainder to Data.</summary>
    [Test] procedure DecodePositive;
    /// <summary>Decode negative response 7F 22 33 = NRC 0x33
    /// (security access denied).</summary>
    [Test] procedure DecodeNegativeWithKnownNRC;
    /// <summary>Decode negative response with unknown NRC produces
    /// synthetic text.</summary>
    [Test] procedure DecodeNegativeWithUnknownNRC;
    /// <summary>ExpectedPositiveResponse adds 0x40.</summary>
    [Test] procedure ExpectedPositiveResponseOffset;
    /// <summary>Decode of empty hex returns False.</summary>
    [Test] procedure DecodeEmptyReturnsFalse;
    /// <summary>Decode tolerates noisy hex (whitespace, line
    /// endings).</summary>
    [Test] procedure DecodeTolerantOfNoise;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  OBD.Types,
  OBD.Catalog,
  OBD.Protocol.Types,
  OBD.Protocol.UDS;

procedure EnsureNRCCatalog;
var
  Root, Path: string;
  Dir: string;
begin
  // The shipped catalogue lives under <repo>/catalogs/obd2/nrc.json.
  // Walk up to find it; idempotent if already loaded.
  Dir := TPath.GetDirectoryName(ParamStr(0));
  while Dir <> '' do
  begin
    Root := TPath.Combine(Dir, 'catalogs');
    if TDirectory.Exists(Root) then
    begin
      Path := TPath.Combine(Root, 'obd2' + PathDelim + 'nrc.json');
      if TFile.Exists(Path) then
        TOBDCatalogStore.Default.LoadFile(Path);
      Exit;
    end;
    if Dir = TPath.GetDirectoryName(Dir) then Break;
    Dir := TPath.GetDirectoryName(Dir);
  end;
end;

procedure TUDSCodecTests.EncodeReadDataByIdentifier;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.Protocol := apUDS;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Req.Data := TBytes.Create($F1, $90); // VIN
  Assert.AreEqual('22 F1 90', TOBDUDSCodec.Encode(Req));
end;

procedure TUDSCodecTests.EncodeServiceZeroRaises;
var
  Req: TOBDRequest;
begin
  Req := MakeOBDRequest;
  Req.Protocol := apUDS;
  Req.ServiceID := 0;
  Assert.WillRaise(
    procedure
    begin
      TOBDUDSCodec.Encode(Req);
    end,
    EOBDProtocolErr);
end;

procedure TUDSCodecTests.DecodePositive;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req,
    '62 F1 90 31 48 47 43 4D 38 32 36 33 33 41 30 30 34 33 35 32',
    Resp));
  Assert.IsFalse(Resp.IsNegative);
  Assert.AreEqual<Byte>($62, Resp.ServiceID);
  // First two payload bytes are the DID (F1 90); rest is the VIN.
  Assert.AreEqual<NativeInt>(19, Length(Resp.Data));
  Assert.AreEqual<Byte>($F1, Resp.Data[0]);
  Assert.AreEqual<Byte>($90, Resp.Data[1]);
  Assert.AreEqual<Byte>($31, Resp.Data[2]); // '1' (VIN first char)
end;

procedure TUDSCodecTests.DecodeNegativeWithKnownNRC;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  EnsureNRCCatalog;
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '7F 22 33', Resp));
  Assert.IsTrue(Resp.IsNegative);
  Assert.AreEqual<Byte>($22, Resp.ServiceID);
  Assert.AreEqual<Byte>($33, Resp.NRC);
  Assert.IsNotEmpty(Resp.NRCText);
end;

procedure TUDSCodecTests.DecodeNegativeWithUnknownNRC;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_TesterPresent;
  // 0xCC is not in the standard catalogue.
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '7F 3E CC', Resp));
  Assert.IsTrue(Resp.IsNegative);
  Assert.AreEqual<Byte>($CC, Resp.NRC);
  // Synthetic text starts with 'NRC 0x'.
  Assert.IsTrue(Pos('NRC 0x', Resp.NRCText) = 1,
    Format('Expected synthetic text "NRC 0x..", got "%s"', [Resp.NRCText]));
end;

procedure TUDSCodecTests.ExpectedPositiveResponseOffset;
begin
  Assert.AreEqual<Byte>($62,
    TOBDUDSCodec.ExpectedPositiveResponse(UDS_SID_ReadDataByIdentifier));
  Assert.AreEqual<Byte>($50,
    TOBDUDSCodec.ExpectedPositiveResponse(
      UDS_SID_DiagnosticSessionControl));
end;

procedure TUDSCodecTests.DecodeEmptyReturnsFalse;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_TesterPresent;
  Assert.IsFalse(TOBDUDSCodec.Decode(Req, '', Resp));
  Assert.IsFalse(TOBDUDSCodec.Decode(Req, '   ', Resp));
end;

procedure TUDSCodecTests.DecodeTolerantOfNoise;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '62 F1 90' + #13#10 + '31', Resp));
  Assert.AreEqual<NativeInt>(3, Length(Resp.Data));
end;

initialization
  TDUnitX.RegisterTestFixture(TUDSCodecTests);

end.
