//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.DoIP
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit Tests.OEM.DoIP;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDoIPHeaderTests = class
  public
    /// <summary>
    ///   Build header emits version inversion.
    /// </summary>
    [Test] procedure BuildHeaderEmitsVersionInversion;
    /// <summary>
    ///   Build header encodes payload type and length big endian.
    /// </summary>
    [Test] procedure BuildHeaderEncodesPayloadTypeAndLengthBigEndian;
    /// <summary>
    ///   Parse header rejects bad inversion.
    /// </summary>
    [Test] procedure ParseHeaderRejectsBadInversion;
    /// <summary>
    ///   Parse header rejects short buffer.
    /// </summary>
    [Test] procedure ParseHeaderRejectsShortBuffer;
    /// <summary>
    ///   Parse header round trips all fields.
    /// </summary>
    [Test] procedure ParseHeaderRoundTripsAllFields;
  end;

  [TestFixture]
  TDoIPRoutingTests = class
  public
    /// <summary>
    ///   Build activation request emits19 bytes with default activation.
    /// </summary>
    [Test] procedure BuildActivationRequestEmits19BytesWithDefaultActivation;
    /// <summary>
    ///   Build activation request carries o e m specific.
    /// </summary>
    [Test] procedure BuildActivationRequestCarriesOEMSpecific;
    /// <summary>
    ///   Parse activation response v2010.
    /// </summary>
    [Test] procedure ParseActivationResponseV2010;
    /// <summary>
    ///   Parse activation response v2012 with o e m tail.
    /// </summary>
    [Test] procedure ParseActivationResponseV2012WithOEMTail;
    /// <summary>
    ///   Parse activation rejects truncated.
    /// </summary>
    [Test] procedure ParseActivationRejectsTruncated;
    /// <summary>
    ///   Parse activation returns false on wrong type.
    /// </summary>
    [Test] procedure ParseActivationReturnsFalseOnWrongType;
  end;

  [TestFixture]
  TDoIPVehicleTests = class
  public
    /// <summary>
    ///   Build vehicle ident empty payload.
    /// </summary>
    [Test] procedure BuildVehicleIdentEmptyPayload;
    /// <summary>
    ///   Build vehicle ident by v i n rejects bad length.
    /// </summary>
    [Test] procedure BuildVehicleIdentByVINRejectsBadLength;
    /// <summary>
    ///   Build vehicle ident by v i n round trips.
    /// </summary>
    [Test] procedure BuildVehicleIdentByVINRoundTrips;
    /// <summary>
    ///   Parse vehicle announcement extracts fields.
    /// </summary>
    [Test] procedure ParseVehicleAnnouncementExtractsFields;
  end;

  [TestFixture]
  TDoIPDiagMessageTests = class
  public
    /// <summary>
    ///   Build diag wraps u d s.
    /// </summary>
    [Test] procedure BuildDiagWrapsUDS;
    /// <summary>
    ///   Build diag rejects empty user data.
    /// </summary>
    [Test] procedure BuildDiagRejectsEmptyUserData;
    /// <summary>
    ///   Parse diag extracts addresses and user data.
    /// </summary>
    [Test] procedure ParseDiagExtractsAddressesAndUserData;
    /// <summary>
    ///   Round trips via build and parse.
    /// </summary>
    [Test] procedure RoundTripsViaBuildAndParse;
    /// <summary>
    ///   Alive check pair.
    /// </summary>
    [Test] procedure AliveCheckPair;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM.DoIP;

//==============================================================================
// Header
//==============================================================================

//------------------------------------------------------------------------------
// BUILD HEADER EMITS VERSION INVERSION
//------------------------------------------------------------------------------
procedure TDoIPHeaderTests.BuildHeaderEmitsVersionInversion;
var
  H: TBytes;
begin
  H := BuildDoIPHeader(dpv2012, dptAliveCheckRequest, 0);
  Assert.AreEqual(8, Length(H));
  Assert.AreEqual(Byte($02), H[0]);
  Assert.AreEqual(Byte($FD), H[1]);  // not $02 = $FD
end;

//------------------------------------------------------------------------------
// BUILD HEADER ENCODES PAYLOAD TYPE AND LENGTH BIG ENDIAN
//------------------------------------------------------------------------------
procedure TDoIPHeaderTests.BuildHeaderEncodesPayloadTypeAndLengthBigEndian;
var
  H: TBytes;
begin
  H := BuildDoIPHeader(dpv2012, dptDiagnosticMessage, $01020304);
  // Payload type 0x8001, length 0x01020304.
  Assert.AreEqual(Byte($80), H[2]);
  Assert.AreEqual(Byte($01), H[3]);
  Assert.AreEqual(Byte($01), H[4]);
  Assert.AreEqual(Byte($02), H[5]);
  Assert.AreEqual(Byte($03), H[6]);
  Assert.AreEqual(Byte($04), H[7]);
end;

//------------------------------------------------------------------------------
// PARSE HEADER REJECTS BAD INVERSION
//------------------------------------------------------------------------------
procedure TDoIPHeaderTests.ParseHeaderRejectsBadInversion;
var
  Header: TOBDDoIPHeader;
begin
  Assert.WillRaise(
    procedure begin
      ParseDoIPHeader(TBytes.Create($02, $FF, $00, $05, 0, 0, 0, 0), Header);
    end,
    EOBDDoIPError);
end;

//------------------------------------------------------------------------------
// PARSE HEADER REJECTS SHORT BUFFER
//------------------------------------------------------------------------------
procedure TDoIPHeaderTests.ParseHeaderRejectsShortBuffer;
var
  Header: TOBDDoIPHeader;
begin
  Assert.WillRaise(
    procedure begin ParseDoIPHeader(TBytes.Create($02, $FD), Header); end,
    EOBDDoIPError);
end;

//------------------------------------------------------------------------------
// PARSE HEADER ROUND TRIPS ALL FIELDS
//------------------------------------------------------------------------------
procedure TDoIPHeaderTests.ParseHeaderRoundTripsAllFields;
var
  Built: TBytes;
  Parsed: TOBDDoIPHeader;
begin
  Built := BuildDoIPHeader(dpv2012, dptVehicleAnnouncement, $00112233);
  ParseDoIPHeader(Built, Parsed);
  Assert.AreEqual(Byte($02), Parsed.Version);
  Assert.AreEqual(Word($0004), Parsed.PayloadType);
  Assert.AreEqual(Cardinal($00112233), Parsed.PayloadLength);
end;

//==============================================================================
// Routing activation
//==============================================================================

//------------------------------------------------------------------------------
// BUILD ACTIVATION REQUEST EMITS19 BYTES WITH DEFAULT ACTIVATION
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.BuildActivationRequestEmits19BytesWithDefaultActivation;
var
  Bytes: TBytes;
begin
  Bytes := BuildRoutingActivationRequest($0E80);
  Assert.AreEqual(19, Length(Bytes));   // 8 header + 11 payload
  Assert.AreEqual(Byte($00), Bytes[2]); // payload type hi
  Assert.AreEqual(Byte($05), Bytes[3]); // payload type lo (RoutingActivationRequest)
  // Tester address is at offset 8 + 0.
  Assert.AreEqual(Byte($0E), Bytes[8]);
  Assert.AreEqual(Byte($80), Bytes[9]);
  Assert.AreEqual(Byte($00), Bytes[10]); // activation type = default
end;

//------------------------------------------------------------------------------
// BUILD ACTIVATION REQUEST CARRIES OEMSPECIFIC
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.BuildActivationRequestCarriesOEMSpecific;
var
  Bytes: TBytes;
begin
  Bytes := BuildRoutingActivationRequest($0E00, datOEMSpecific, $DEADBEEF);
  Assert.AreEqual(Byte($E0), Bytes[10]);   // activation type = OEM
  // Reserved (4 bytes of 0) + OEM-specific 0xDEADBEEF at offset 8+7=15.
  Assert.AreEqual(Byte($DE), Bytes[15]);
  Assert.AreEqual(Byte($EF), Bytes[18]);
end;

//------------------------------------------------------------------------------
// PARSE ACTIVATION RESPONSE V2010
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.ParseActivationResponseV2010;
var
  Activation: TOBDDoIPRoutingActivation;
  Frame: TBytes;
begin
  // Header: 02 FD 00 06 00 00 00 09 (length = 9)
  // Payload: tester=0E80 entity=1234 code=10 reserved=00000000
  Frame := TBytes.Create(
    $02, $FD, $00, $06, $00, $00, $00, $09,
    $0E, $80, $12, $34, $10, $00, $00, $00, $00);
  Assert.IsTrue(ParseRoutingActivationResponse(Frame, Activation));
  Assert.AreEqual(Word($0E80), Activation.TesterLogicalAddress);
  Assert.AreEqual(Word($1234), Activation.EntityLogicalAddress);
  Assert.AreEqual(Ord(drrSuccess), Ord(Activation.ResponseCode));
  Assert.AreEqual(Cardinal(0), Activation.OEMSpecific);
end;

//------------------------------------------------------------------------------
// PARSE ACTIVATION RESPONSE V2012 WITH OEMTAIL
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.ParseActivationResponseV2012WithOEMTail;
var
  Activation: TOBDDoIPRoutingActivation;
  Frame: TBytes;
begin
  Frame := TBytes.Create(
    $02, $FD, $00, $06, $00, $00, $00, $0D,
    $0E, $80, $12, $34, $10, $00, $00, $00, $00,
    $CA, $FE, $BA, $BE);
  Assert.IsTrue(ParseRoutingActivationResponse(Frame, Activation));
  Assert.AreEqual(Cardinal($CAFEBABE), Activation.OEMSpecific);
end;

//------------------------------------------------------------------------------
// PARSE ACTIVATION REJECTS TRUNCATED
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.ParseActivationRejectsTruncated;
var
  Activation: TOBDDoIPRoutingActivation;
begin
  Assert.WillRaise(
    procedure begin
      ParseRoutingActivationResponse(
        TBytes.Create($02, $FD, $00, $06, $00, $00, $00, $09, $0E, $80),
        Activation);
    end,
    EOBDDoIPError);
end;

//------------------------------------------------------------------------------
// PARSE ACTIVATION RETURNS FALSE ON WRONG TYPE
//------------------------------------------------------------------------------
procedure TDoIPRoutingTests.ParseActivationReturnsFalseOnWrongType;
var
  Activation: TOBDDoIPRoutingActivation;
begin
  // VehicleIdentResponse (0x0004) instead of routing-activation.
  Assert.IsFalse(ParseRoutingActivationResponse(
    BuildDoIPHeader(dpv2012, dptVehicleAnnouncement, 0), Activation));
end;

//==============================================================================
// Vehicle ident
//==============================================================================

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT EMPTY PAYLOAD
//------------------------------------------------------------------------------
procedure TDoIPVehicleTests.BuildVehicleIdentEmptyPayload;
var
  Bytes: TBytes;
begin
  Bytes := BuildVehicleIdentRequest;
  Assert.AreEqual(8, Length(Bytes));
  Assert.AreEqual(Byte($01), Bytes[3]);   // payload type lo = 0x0001
end;

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT BY VINREJECTS BAD LENGTH
//------------------------------------------------------------------------------
procedure TDoIPVehicleTests.BuildVehicleIdentByVINRejectsBadLength;
begin
  Assert.WillRaise(
    procedure begin BuildVehicleIdentRequestByVIN('SHORTVIN'); end,
    EOBDDoIPError);
end;

//------------------------------------------------------------------------------
// BUILD VEHICLE IDENT BY VINROUND TRIPS
//------------------------------------------------------------------------------
procedure TDoIPVehicleTests.BuildVehicleIdentByVINRoundTrips;
var
  Bytes: TBytes;
  Header: TOBDDoIPHeader;
begin
  Bytes := BuildVehicleIdentRequestByVIN('WVWZZZ8N8Z1234567');
  ParseDoIPHeader(Bytes, Header);
  Assert.AreEqual(Word($0003), Header.PayloadType);
  Assert.AreEqual(Cardinal(17), Header.PayloadLength);
  Assert.AreEqual(Byte(Ord('W')), Bytes[8]);
end;

//------------------------------------------------------------------------------
// PARSE VEHICLE ANNOUNCEMENT EXTRACTS FIELDS
//------------------------------------------------------------------------------
procedure TDoIPVehicleTests.ParseVehicleAnnouncementExtractsFields;
var
  Frame: TBytes;
  Announcement: TOBDDoIPVehicleAnnouncement;
  VinBytes: TBytes;
  I: Integer;
begin
  // Header 02 FD 00 04 00 00 00 21 (length = 33)
  VinBytes := TEncoding.ASCII.GetBytes('WVWZZZ8N8Z1234567');
  SetLength(Frame, 8 + 33);
  Frame[0] := $02; Frame[1] := $FD;
  Frame[2] := $00; Frame[3] := $04;
  Frame[4] := $00; Frame[5] := $00; Frame[6] := $00; Frame[7] := $21;
  for I := 0 to 16 do Frame[8 + I] := VinBytes[I];
  Frame[25] := $0E; Frame[26] := $80;          // logical address
  for I := 0 to 5 do Frame[27 + I] := Byte($A0 + I);    // EID
  for I := 0 to 5 do Frame[33 + I] := Byte($B0 + I);    // GID
  Frame[39] := $00;                            // FurtherActionRequired
  Frame[40] := $10;                            // SyncStatus

  Assert.IsTrue(ParseVehicleAnnouncement(Frame, Announcement));
  Assert.AreEqual('WVWZZZ8N8Z1234567', Announcement.VIN);
  Assert.AreEqual(Word($0E80), Announcement.LogicalAddress);
  Assert.AreEqual(6, Length(Announcement.EID));
  Assert.AreEqual(Byte($A0), Announcement.EID[0]);
  Assert.AreEqual(Byte($10), Announcement.SyncStatus);
end;

//==============================================================================
// Diagnostic message
//==============================================================================

//------------------------------------------------------------------------------
// BUILD DIAG WRAPS UDS
//------------------------------------------------------------------------------
procedure TDoIPDiagMessageTests.BuildDiagWrapsUDS;
var
  UDS, Built: TBytes;
begin
  UDS := TBytes.Create($22, $F1, $90);
  Built := BuildDiagnosticMessage($0E80, $1234, UDS);
  // Header (8) + 4 addr + 3 UDS = 15 bytes.
  Assert.AreEqual(15, Length(Built));
  Assert.AreEqual(Byte($80), Built[2]);
  Assert.AreEqual(Byte($01), Built[3]);
  Assert.AreEqual(Byte($0E), Built[8]);
  Assert.AreEqual(Byte($80), Built[9]);
  Assert.AreEqual(Byte($12), Built[10]);
  Assert.AreEqual(Byte($34), Built[11]);
  Assert.AreEqual(Byte($22), Built[12]);
end;

//------------------------------------------------------------------------------
// BUILD DIAG REJECTS EMPTY USER DATA
//------------------------------------------------------------------------------
procedure TDoIPDiagMessageTests.BuildDiagRejectsEmptyUserData;
begin
  Assert.WillRaise(
    procedure begin BuildDiagnosticMessage($0E80, $1234, nil); end,
    EOBDDoIPError);
end;

//------------------------------------------------------------------------------
// PARSE DIAG EXTRACTS ADDRESSES AND USER DATA
//------------------------------------------------------------------------------
procedure TDoIPDiagMessageTests.ParseDiagExtractsAddressesAndUserData;
var
  Frame: TBytes;
  Msg: TOBDDoIPDiagnosticMessage;
begin
  Frame := BuildDiagnosticMessage($0E80, $1234, TBytes.Create($22, $F1, $90));
  Assert.IsTrue(ParseDiagnosticMessage(Frame, Msg));
  Assert.AreEqual(Word($0E80), Msg.SourceAddress);
  Assert.AreEqual(Word($1234), Msg.TargetAddress);
  Assert.AreEqual(3, Length(Msg.UserData));
  Assert.AreEqual(Byte($22), Msg.UserData[0]);
end;

//------------------------------------------------------------------------------
// ROUND TRIPS VIA BUILD AND PARSE
//------------------------------------------------------------------------------
procedure TDoIPDiagMessageTests.RoundTripsViaBuildAndParse;
var
  Original, Built: TBytes;
  Msg: TOBDDoIPDiagnosticMessage;
begin
  Original := TBytes.Create($31, $01, $0F, $00, $DE, $AD, $BE, $EF);
  Built := BuildDiagnosticMessage($0E80, $1234, Original);
  ParseDiagnosticMessage(Built, Msg);
  Assert.AreEqual(Length(Original), Length(Msg.UserData));
  Assert.AreEqual(Byte($EF), Msg.UserData[High(Msg.UserData)]);
end;

//------------------------------------------------------------------------------
// ALIVE CHECK PAIR
//------------------------------------------------------------------------------
procedure TDoIPDiagMessageTests.AliveCheckPair;
var
  Req, Resp: TBytes;
begin
  Req := BuildAliveCheckRequest;
  Resp := BuildAliveCheckResponse($0E80);
  Assert.AreEqual(8, Length(Req));
  Assert.AreEqual(10, Length(Resp));
  Assert.AreEqual(Byte($08), Resp[3]);   // payload type lo = 0x0008
end;

initialization
  TDUnitX.RegisterTestFixture(TDoIPHeaderTests);
  TDUnitX.RegisterTestFixture(TDoIPRoutingTests);
  TDUnitX.RegisterTestFixture(TDoIPVehicleTests);
  TDUnitX.RegisterTestFixture(TDoIPDiagMessageTests);

end.
