//------------------------------------------------------------------------------
//  Tests.ERD.Protocol.ELMResponse
//
//  ELM327 response text -> per-ECU message assembly: headers off / on,
//  spaces off / on, multi-frame segments, interleaved ECUs, 29-bit and
//  legacy headers, response-pending skipping, and the UDS codec's
//  primary-message selection.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.ERD.Protocol.ELMResponse;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>ELM327 response assembly coverage.</summary>
  [TestFixture]
  TELMResponseTests = class
  public
    /// <summary>One headers-off line with spaces is one message.</summary>
    [Test] procedure HeadersOffSingleLine;
    /// <summary>One headers-off line without spaces is one message.</summary>
    [Test] procedure HeadersOffNoSpaces;
    /// <summary>Length line plus "0:" / "1:" / "2:" segments form one
    /// message of exactly the announced length.</summary>
    [Test] procedure HeadersOffMultiFrame;
    /// <summary>Segment padding beyond the announced length is cut.</summary>
    [Test] procedure HeadersOffMultiFrameTrimsPadding;
    /// <summary>Spaces-off segments ("0:490201...") parse too.</summary>
    [Test] procedure HeadersOffMultiFrameNoSpaces;
    /// <summary>Two 11-bit ECUs answering give two messages.</summary>
    [Test] procedure HeadersOnTwoEcus;
    /// <summary>Interleaved multi-frame and single-frame answers are
    /// reassembled per header.</summary>
    [Test] procedure HeadersOnInterleavedMultiFrame;
    /// <summary>Headers on with spaces off (odd digit count).</summary>
    [Test] procedure HeadersOnNoSpaces;
    /// <summary>29-bit identifiers are recognised.</summary>
    [Test] procedure Headers29Bit;
    /// <summary>Legacy header and checksum are removed.</summary>
    [Test] procedure LegacyHeader;
    /// <summary>A multi-frame message without its consecutive frames
    /// is marked incomplete.</summary>
    [Test] procedure TruncatedMultiFrameIncomplete;
    /// <summary>Status lines are skipped.</summary>
    [Test] procedure StatusLinesSkipped;
    /// <summary>Response-pending messages are skipped by the codec.</summary>
    [Test] procedure CodecSkipsResponsePending;
    /// <summary>Only a pending message gives NRC 0x78.</summary>
    [Test] procedure CodecPendingOnlyIsNegative;
    /// <summary>The codec picks the first positive message as primary.</summary>
    [Test] procedure CodecPrefersPositiveMessage;
    /// <summary>The codec decodes a headers-off multi-frame VIN.</summary>
    [Test] procedure CodecMultiFrameVIN;
  end;

implementation

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.Protocol.Types,
  ERD.Protocol.ELMResponse,
  ERD.Protocol.UDS;

const
  CR = #13;
  VIN_TEXT = 'WVWZZZAUZGW123456';

function AsText(const ABytes: TBytes; AFrom: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := AFrom to High(ABytes) do
    Result := Result + Char(ABytes[I]);
end;

procedure TELMResponseTests.HeadersOffSingleLine;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('41 00 BE 3F A8 13');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('41 00 BE 3F A8 13', BytesToHex(M[0].Data));
  Assert.AreEqual('', M[0].Header);
  Assert.IsTrue(M[0].Complete);
end;

procedure TELMResponseTests.HeadersOffNoSpaces;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('4100BE3FA813' + CR + CR);
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('41 00 BE 3F A8 13', BytesToHex(M[0].Data));
end;

procedure TELMResponseTests.HeadersOffMultiFrame;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('014' + CR + '0: 49 02 01 57 56 57' + CR +
    '1: 5A 5A 5A 41 55 5A 47' + CR + '2: 57 31 32 33 34 35 36');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual<NativeInt>(20, Length(M[0].Data));
  Assert.IsTrue(M[0].Complete);
  Assert.AreEqual(VIN_TEXT, AsText(M[0].Data, 3));
end;

procedure TELMResponseTests.HeadersOffMultiFrameTrimsPadding;
var
  M: TArray<TOBDEcuMessage>;
begin
  // 19 bytes: 49 04 01 + 16-character CALID; last segment padded.
  M := TOBDELMResponseParser.Split('013' + CR + '0: 49 04 01 30 33 4C' + CR +
    '1: 39 30 36 30 32 31 41' + CR + '2: 4C 20 20 20 20 20 00');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual<NativeInt>(19, Length(M[0].Data));
  Assert.AreEqual<Byte>($20, M[0].Data[18]);
end;

procedure TELMResponseTests.HeadersOffMultiFrameNoSpaces;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('014'#13#10'0:490201575657'#13#10 +
    '1:5A5A5A41555A47'#13#10'2:57313233343536'#13#10);
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual(VIN_TEXT, AsText(M[0].Data, 3));
end;

procedure TELMResponseTests.HeadersOnTwoEcus;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('7E8 06 41 00 BE 3F A8 13' + CR +
    '7E9 06 41 00 98 18 80 01');
  Assert.AreEqual<NativeInt>(2, Length(M));
  Assert.AreEqual('7E8', M[0].Header);
  Assert.AreEqual<Cardinal>($7E8, M[0].CanId);
  Assert.AreEqual('41 00 BE 3F A8 13', BytesToHex(M[0].Data));
  Assert.AreEqual('7E9', M[1].Header);
  Assert.AreEqual('41 00 98 18 80 01', BytesToHex(M[1].Data));
end;

procedure TELMResponseTests.HeadersOnInterleavedMultiFrame;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('7E8 10 14 49 02 01 57 56 57' + CR +
    '7E9 03 41 0D 00' + CR + '7E8 21 5A 5A 5A 41 55 5A 47' + CR +
    '7E8 22 57 31 32 33 34 35 36');
  Assert.AreEqual<NativeInt>(2, Length(M));
  Assert.AreEqual('7E8', M[0].Header);
  Assert.IsTrue(M[0].Complete);
  Assert.AreEqual(VIN_TEXT, AsText(M[0].Data, 3));
  Assert.AreEqual('41 0D 00', BytesToHex(M[1].Data));
end;

procedure TELMResponseTests.HeadersOnNoSpaces;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('7E8064100BE3FA813');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('7E8', M[0].Header);
  Assert.AreEqual('41 00 BE 3F A8 13', BytesToHex(M[0].Data));
end;

procedure TELMResponseTests.Headers29Bit;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('18 DA F1 10 03 41 0D 32');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('18DAF110', M[0].Header);
  Assert.AreEqual<Cardinal>($18DAF110, M[0].CanId);
  Assert.IsTrue(M[0].IsExtendedId);
  Assert.AreEqual('41 0D 32', BytesToHex(M[0].Data));
end;

procedure TELMResponseTests.LegacyHeader;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('48 6B 10 41 0C 1A F8 5E', -1, ehLegacy);
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('486B10', M[0].Header);
  Assert.AreEqual<Cardinal>($10, M[0].CanId);
  Assert.AreEqual('41 0C 1A F8', BytesToHex(M[0].Data));
end;

procedure TELMResponseTests.TruncatedMultiFrameIncomplete;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('7E8 10 14 49 02 01 57 56 57');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.IsFalse(M[0].Complete);
  M := TOBDELMResponseParser.Split('014' + CR + '0: 49 02 01 57 56 57');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.IsFalse(M[0].Complete);
end;

procedure TELMResponseTests.StatusLinesSkipped;
var
  M: TArray<TOBDEcuMessage>;
begin
  M := TOBDELMResponseParser.Split('SEARCHING...' + CR + '41 0C 1A F8');
  Assert.AreEqual<NativeInt>(1, Length(M));
  M := TOBDELMResponseParser.Split('BUS INIT: ...OK' + CR + '41 0C 1A F8');
  Assert.AreEqual<NativeInt>(1, Length(M));
  Assert.AreEqual('41 0C 1A F8', BytesToHex(M[0].Data));
end;

procedure TELMResponseTests.CodecSkipsResponsePending;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '7F 22 78' + CR + '62 F1 90 41',
    Resp));
  Assert.IsFalse(Resp.IsNegative);
  Assert.AreEqual<Byte>($62, Resp.ServiceID);
  Assert.AreEqual('F1 90 41', BytesToHex(Resp.Data));
  Assert.AreEqual<NativeInt>(1, Length(Resp.Messages));
end;

procedure TELMResponseTests.CodecPendingOnlyIsNegative;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := UDS_SID_ReadDataByIdentifier;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '7F 22 78', Resp));
  Assert.IsTrue(Resp.IsNegative);
  Assert.AreEqual<Byte>(UDS_NRC_ResponsePending, Resp.NRC);
end;

procedure TELMResponseTests.CodecPrefersPositiveMessage;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := $01;
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '7F 01 12' + CR +
    '41 00 BE 3F A8 13', Resp));
  Assert.IsFalse(Resp.IsNegative);
  Assert.AreEqual<Byte>($41, Resp.ServiceID);
  Assert.AreEqual('00 BE 3F A8 13', BytesToHex(Resp.Data));
  Assert.AreEqual<NativeInt>(2, Length(Resp.Messages));
end;

procedure TELMResponseTests.CodecMultiFrameVIN;
var
  Req: TOBDRequest;
  Resp: TOBDResponse;
begin
  Req := MakeOBDRequest;
  Req.ServiceID := $09;
  Req.Data := TBytes.Create($02);
  Assert.IsTrue(TOBDUDSCodec.Decode(Req, '014' + CR +
    '0: 49 02 01 57 56 57' + CR + '1: 5A 5A 5A 41 55 5A 47' + CR +
    '2: 57 31 32 33 34 35 36', Resp));
  Assert.AreEqual<Byte>($49, Resp.ServiceID);
  Assert.AreEqual<NativeInt>(19, Length(Resp.Data));
  Assert.AreEqual(VIN_TEXT, AsText(Resp.Data, 2));
end;

initialization
  TDUnitX.RegisterTestFixture(TELMResponseTests);

end.
