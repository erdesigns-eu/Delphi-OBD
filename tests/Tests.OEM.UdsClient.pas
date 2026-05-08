//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.UdsClient
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tests the catalog-driven UDS client. Uses an
//                  in-memory mock transport (TMockTransport) that
//                  replays canned responses and asserts the right
//                  request bytes were emitted.
//------------------------------------------------------------------------------
unit Tests.OEM.UdsClient;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUdsClientTests = class
  public
    [Test] procedure ReadDID_ResolvesByName_ReturnsDecodedValue;
    [Test] procedure ReadDID_ResolvesByHex_ReturnsDecodedValue;
    [Test] procedure ReadDID_AppliesScaleAndOffset;
    [Test] procedure ReadDID_DecodesEnum;
    [Test] procedure ReadDID_DecodesAscii;
    [Test] procedure ReadDID_RaisesWhenCatalogMiss;
    [Test] procedure ReadDID_RaisesWhenNoSession;

    [Test] procedure WriteAdaptation_PacksUInt8;
    [Test] procedure WriteAdaptation_PacksUInt16BE;
    [Test] procedure WriteAdaptation_RejectsOutOfRange;
    [Test] procedure WriteAdaptation_RaisesOnUnknownChannel;
    /// <summary>Regression for G6 — when a catalog declares
    /// min=0, max=0 explicitly (e.g. an enum pinned to a single
    /// legal value), only Value=0 must be accepted. The earlier
    /// implementation skipped validation entirely when both
    /// bounds were zero and would have let any value through.</summary>
    [Test] procedure WriteAdaptation_FixedZeroEnforced;

    [Test] procedure ExecuteRoutine_StartsAndReturnsOk;
    [Test] procedure ExecuteRoutine_ReportsUnexpectedResponse;

    [Test] procedure RunActuatorTest_GatesOnSafetyWarning;
    [Test] procedure RunActuatorTest_AcknowledgedSafetyRuns;
    [Test] procedure RunActuatorTest_NoSafetyRunsFreely;

    [Test] procedure ReadCodingBlock_UnpacksBitFields;
    [Test] procedure WriteCodingBlock_PreservesUncoveredBits;

    [Test] procedure ReadDtcs_DecodesPCodes;
    [Test] procedure ReadDtcs_DecodesUCodes;
  end;

implementation

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  OBD.OEM, OBD.OEM.Catalog.JSON, OBD.OEM.UdsClient;

//==============================================================================
// Mock transport
//==============================================================================

type
  TMockTransport = class(TInterfacedObject, IOBDDiagnosticTransport)
  strict private
    FECUAddress: Word;
    FCannedResponses: TList<TBytes>;   // FIFO queue
    FRequests: TList<TBytes>;          // recorded for assertions
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnqueueResponse(const Bytes: TBytes);
    function  RequestCount: Integer;
    function  Request(Index: Integer): TBytes;
    function  SendReceive(const Request: TBytes;
                          TimeoutMs: Cardinal = 1500): TBytes;
    procedure SetTargetECU(Address: Word);
    function  TargetECU: Word;
  end;

constructor TMockTransport.Create;
begin
  inherited Create;
  FCannedResponses := TList<TBytes>.Create;
  FRequests := TList<TBytes>.Create;
end;

destructor TMockTransport.Destroy;
begin
  FCannedResponses.Free;
  FRequests.Free;
  inherited;
end;

procedure TMockTransport.EnqueueResponse(const Bytes: TBytes);
begin
  FCannedResponses.Add(Bytes);
end;

function TMockTransport.RequestCount: Integer;
begin
  Result := FRequests.Count;
end;

function TMockTransport.Request(Index: Integer): TBytes;
begin
  Result := FRequests[Index];
end;

function TMockTransport.SendReceive(const Request: TBytes;
  TimeoutMs: Cardinal): TBytes;
begin
  FRequests.Add(Request);
  if FCannedResponses.Count = 0 then
    raise Exception.Create('mock: no canned response queued');
  Result := FCannedResponses.First;
  FCannedResponses.Delete(0);
end;

procedure TMockTransport.SetTargetECU(Address: Word);
begin
  FECUAddress := Address;
end;

function TMockTransport.TargetECU: Word;
begin
  Result := FECUAddress;
end;

//==============================================================================
// Helpers
//==============================================================================

function MakeCatalog(const Json: string): TOBDOEMJSONCatalog;
begin
  Result := TOBDOEMJSONCatalog.CreateFromText(Json);
end;

const
  CATALOG_JSON =
    '{"version": 1, "manufacturer_key": "TEST", "display_name": "Test",' +
    ' "applicable_wmis": ["TST"],' +
    ' "ecus": [{"address": "0x7E0", "name": "ecm", "common_name": "ECM"}],' +
    ' "dids": [' +
    '   {"did": "0x4022", "name": "battery_voltage",  "description": "12V battery",' +
    '    "ecu_address": "0x7E0",' +
    '    "decoder": {"kind": "uint16_be", "scale": 0.001, "offset": 0.0, "unit": "V"}},' +
    '   {"did": "0x4031", "name": "drive_mode", "description": "ECM drive mode",' +
    '    "ecu_address": "0x7E0",' +
    '    "decoder": {"kind": "enum", "size": 1, "values": {"0": "Eco", "1": "Normal", "2": "Sport"}}},' +
    '   {"did": "0xF110", "name": "ecu_serial",  "description": "ECU serial",' +
    '    "decoder": {"kind": "ascii", "length": 8}}' +
    ' ],' +
    ' "routines": [' +
    '   {"id": "0x0210", "name": "idle_adapt", "description": "Idle adapt",' +
    '    "ecu_address": "0x7E0"}' +
    ' ],' +
    ' "adaptations": [' +
    '   {"channel": "0x0100", "name": "idle_target", "description": "Idle target",' +
    '    "ecu_address": "0x7E0",' +
    '    "kind": "uint16_be", "min": 600, "max": 1500, "default": 800, "unit": "rpm"},' +
    '   {"channel": "0x0103", "name": "torque_limit", "description": "Torque limit",' +
    '    "ecu_address": "0x7E0",' +
    '    "kind": "uint8", "min": 50, "max": 100, "default": 100, "unit": "%"}' +
    ' ],' +
    ' "actuator_tests": [' +
    '   {"id": "0x0301", "name": "throttle_test", "description": "Throttle motor",' +
    '    "ecu_address": "0x7E0", "duration_ms": 5000, "safety_warning": "",' +
    '    "response_kind": "uint8", "response_label": "Status"},' +
    '   {"id": "0x0307", "name": "starter_test", "description": "Starter motor",' +
    '    "ecu_address": "0x7E0", "duration_ms": 3000,' +
    '    "safety_warning": "Disconnect injection",' +
    '    "response_kind": "uint8", "response_label": "Status"}' +
    ' ],' +
    ' "coding_blocks": [' +
    '   {"did": "0xC100", "name": "bcm_general",' +
    '    "description": "BCM general", "ecu_address": "0x7E0", "payload_size": 2,' +
    '    "fields": [' +
    '      {"name": "autolock_drive", "label": "Auto-lock when driving",' +
    '       "kind": "bit", "byte_offset": 0, "bit_offset": 0, "default": 1},' +
    '      {"name": "autounlock_park", "label": "Auto-unlock park",' +
    '       "kind": "bit", "byte_offset": 0, "bit_offset": 1, "default": 0},' +
    '      {"name": "autolock_speed", "label": "Auto-lock speed (km/h)",' +
    '       "kind": "uint8", "byte_offset": 1, "bit_offset": 0,' +
    '       "default": 15, "min": 5, "max": 30}' +
    '    ]}' +
    ' ]}';

function CatalogFromJson: TOBDOEMJSONCatalog;
begin
  Result := MakeCatalog(CATALOG_JSON);
end;

procedure OpenWithCatalog(const Client: IOBDUdsClient;
                          const Catalog: TOBDOEMJSONCatalog;
                          const Transport: IOBDDiagnosticTransport);
begin
  Client.OpenSession(Catalog, Transport, $7E0);
end;

/// <summary>Convenience: build a mock transport and return both the
/// concrete class ref (for EnqueueResponse / Request inspection) and
/// the interface ref (for injection). Caller holds <c>ITransport</c>
/// in a local var to keep the object alive; <c>Mock</c> is a borrowed
/// alias — never call <c>Free</c> on it.</summary>
procedure NewMock(out Mock: TMockTransport;
                  out ITransport: IOBDDiagnosticTransport);
begin
  Mock := TMockTransport.Create;
  ITransport := Mock;  // single owning ref, count = 1
end;

//==============================================================================
// Tests
//==============================================================================

procedure TUdsClientTests.ReadDID_ResolvesByName_ReturnsDecodedValue;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Response: 62 40 22 [12V × 1000 = 12000 = 0x2EE0]
    Transport.EnqueueResponse([$62, $40, $22, $2E, $E0]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Decoded := Client.ReadDID('battery_voltage');
    Assert.AreEqual<Cardinal>(12000, Cardinal(Decoded.AsInteger));
    Assert.AreEqual<Double>(12.0, Decoded.AsFloat, 0.001);
    Assert.AreEqual('V', Decoded.Unit_);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_ResolvesByHex_ReturnsDecodedValue;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    Transport.EnqueueResponse([$62, $40, $22, $2E, $E0]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Decoded := Client.ReadDID('0x4022');
    Assert.AreEqual<Cardinal>(12000, Cardinal(Decoded.AsInteger));
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_AppliesScaleAndOffset;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Raw 0x0BB8 = 3000; scale 0.001 → 3.0 V
    Transport.EnqueueResponse([$62, $40, $22, $0B, $B8]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Decoded := Client.ReadDID('battery_voltage');
    Assert.AreEqual<Double>(3.0, Decoded.AsFloat, 0.001);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_DecodesEnum;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Drive mode = 2 (Sport)
    Transport.EnqueueResponse([$62, $40, $31, $02]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Decoded := Client.ReadDID('drive_mode');
    Assert.AreEqual('Sport', Decoded.AsString);
    Assert.AreEqual('Sport', Decoded.Formatted);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_DecodesAscii;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Decoded: TOBDDecodedValue;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // ASCII "ABC12345" = 0x41 0x42 0x43 0x31 0x32 0x33 0x34 0x35
    Transport.EnqueueResponse([$62, $F1, $10,
                               $41, $42, $43, $31, $32, $33, $34, $35]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Decoded := Client.ReadDID('ecu_serial');
    Assert.AreEqual('ABC12345', Decoded.AsString);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_RaisesWhenCatalogMiss;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.WillRaise(procedure
    begin
      Client.ReadDID('absolutely_nonexistent_did');
    end, EOBDUdsCatalogMiss);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDID_RaisesWhenNoSession;
var
  Client: IOBDUdsClient;
begin
  Client := CreateUdsClient;
  Assert.WillRaise(procedure
  begin
    Client.ReadDID('battery_voltage');
  end, EOBDUdsNoSession);
end;

procedure TUdsClientTests.WriteAdaptation_PacksUInt8;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Req: TBytes;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Positive WDBI response: 6E 01 03
    Transport.EnqueueResponse([$6E, $01, $03]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.IsTrue(Client.WriteAdaptation('torque_limit', 75));
    Req := Transport.Request(0);
    // 2E 01 03 4B  (4B = 75)
    Assert.AreEqual<Cardinal>(4, Cardinal(Length(Req)));
    Assert.AreEqual<Byte>($2E, Req[0]);
    Assert.AreEqual<Byte>($01, Req[1]);
    Assert.AreEqual<Byte>($03, Req[2]);
    Assert.AreEqual<Byte>($4B, Req[3]);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.WriteAdaptation_PacksUInt16BE;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Req: TBytes;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    Transport.EnqueueResponse([$6E, $01, $00]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.IsTrue(Client.WriteAdaptation('idle_target', 850));
    Req := Transport.Request(0);
    // 2E 01 00 03 52  (0x0352 = 850)
    Assert.AreEqual<Cardinal>(5, Cardinal(Length(Req)));
    Assert.AreEqual<Byte>($03, Req[3]);
    Assert.AreEqual<Byte>($52, Req[4]);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.WriteAdaptation_RejectsOutOfRange;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.WillRaise(procedure
    begin
      // idle_target min=600 max=1500
      Client.WriteAdaptation('idle_target', 5000);
    end, EOBDUdsValidation);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.WriteAdaptation_FixedZeroEnforced;
const
  PINNED_CATALOG =
    '{"version": 1, "manufacturer_key": "PIN", "display_name": "Pin",' +
    ' "applicable_wmis": [],' +
    ' "ecus": [{"address": "0x7E0", "name": "ecm", "common_name": "ECM"}],' +
    ' "adaptations": [' +
    '   {"channel": "0x0500", "name": "factory_pin",' +
    '    "description": "Pinned to 0",' +
    '    "ecu_address": "0x7E0",' +
    '    "kind": "uint8", "min": 0, "max": 0, "default": 0}' +
    ' ]}';
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;
  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  Catalog := MakeCatalog(PINNED_CATALOG);
  Client := CreateUdsClient;
  try
    NewMock(Transport, ITransport);
    Client.OpenSession(Catalog, ITransport, $7E0);
    // Value=0 must be accepted (with a positive WDBI response).
    Transport.EnqueueResponse([$6E, $05, $00]);
    Assert.IsTrue(Client.WriteAdaptation('factory_pin', 0));
    // Value=1 must be rejected — even though both bounds are 0,
    // the catalog explicitly declared them.
    Assert.WillRaise(procedure
    begin
      Client.WriteAdaptation('factory_pin', 1);
    end, EOBDUdsValidation);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.WriteAdaptation_RaisesOnUnknownChannel;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.WillRaise(procedure
    begin
      Client.WriteAdaptation('unknown_channel', 1);
    end, EOBDUdsCatalogMiss);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ExecuteRoutine_StartsAndReturnsOk;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Result_: TOBDActuatorResult;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Routine positive response: 71 01 02 10 (start ack)
    Transport.EnqueueResponse([$71, $01, $02, $10]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Result_ := Client.ExecuteRoutine('idle_adapt', []);
    Assert.AreEqual<Byte>(0, Result_.Status);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ExecuteRoutine_ReportsUnexpectedResponse;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Result_: TOBDActuatorResult;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Negative or odd response (not 0x71 + RID echo)
    Transport.EnqueueResponse([$7F, $31, $33]); // generalProgrammingFailure
    OpenWithCatalog(Client, Catalog, ITransport);
    Result_ := Client.ExecuteRoutine('idle_adapt', []);
    Assert.AreEqual<Byte>(1, Result_.Status);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.RunActuatorTest_GatesOnSafetyWarning;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    OpenWithCatalog(Client, Catalog, ITransport);
    Assert.WillRaise(procedure
    begin
      // starter_test has safety_warning='Disconnect injection' — no ack
      Client.RunActuatorTest('starter_test');
    end, EOBDUdsActuatorSafety);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.RunActuatorTest_AcknowledgedSafetyRuns;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Result_: TOBDActuatorResult;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    Transport.EnqueueResponse([$71, $01, $03, $07]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Result_ := Client.RunActuatorTest('starter_test',
                                       True {AcknowledgeSafetyWarning});
    Assert.AreEqual<Byte>(0, Result_.Status);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.RunActuatorTest_NoSafetyRunsFreely;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Result_: TOBDActuatorResult;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    Transport.EnqueueResponse([$71, $01, $03, $01]);
    OpenWithCatalog(Client, Catalog, ITransport);
    // throttle_test has no safety_warning
    Result_ := Client.RunActuatorTest('throttle_test');
    Assert.AreEqual<Byte>(0, Result_.Status);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadCodingBlock_UnpacksBitFields;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Values: TOBDCodingValues;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // RDBI response for 0xC100: 62 C1 00 [byte0] [byte1]
    // byte0 = 0b00000001 → autolock_drive=1, autounlock_park=0
    // byte1 = 25 → autolock_speed=25 km/h
    Transport.EnqueueResponse([$62, $C1, $00, $01, $19]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Values := Client.ReadCodingBlock('bcm_general');
    try
      Assert.AreEqual<Int64>(1, Values.GetInt('autolock_drive'));
      Assert.AreEqual<Int64>(0, Values.GetInt('autounlock_park'));
      Assert.AreEqual<Int64>(25, Values.GetInt('autolock_speed'));
    finally
      Values.Free;
    end;
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.WriteCodingBlock_PreservesUncoveredBits;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Values: TOBDCodingValues;
  WrittenReq: TBytes;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // Read response: byte0 = 0xFD (1111 1101 — sets bits 0,2,3,4,5,6,7)
    //                              we cover bits 0,1; bits 2-7 must survive
    //                byte1 = 15
    Transport.EnqueueResponse([$62, $C1, $00, $FD, $0F]);
    // Write response (expect 0x6E for positive WDBI):
    Transport.EnqueueResponse([$6E, $C1, $00]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Values := Client.ReadCodingBlock('bcm_general');
    try
      // Toggle: autolock_drive 1→0, autounlock_park 0→1
      Values.SetInt('autolock_drive', 0);
      Values.SetInt('autounlock_park', 1);
      // Don't touch autolock_speed — should be preserved.
      Client.WriteCodingBlock('bcm_general', Values);
    finally
      Values.Free;
    end;
    // Inspect the WDBI request bytes (request index 1 because index 0 was the read).
    WrittenReq := Transport.Request(1);
    // 2E C1 00 [byte0_modified] [byte1_unchanged]
    Assert.AreEqual<Byte>($2E, WrittenReq[0]);
    Assert.AreEqual<Byte>($C1, WrittenReq[1]);
    Assert.AreEqual<Byte>($00, WrittenReq[2]);
    // byte0: original was 0xFD = 1111 1101.
    // Bit 0 = 0 (autolock_drive cleared), bit 1 = 1 (autounlock_park set).
    // Result: 1111 1110 = 0xFE.
    Assert.AreEqual<Byte>($FE, WrittenReq[3]);
    // byte1: unchanged 0x0F = 15.
    Assert.AreEqual<Byte>($0F, WrittenReq[4]);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDtcs_DecodesPCodes;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Dtcs: TArray<TOBDDtcInstance>;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // 59 02 FF [P0301 03 01 0F] [P0420 04 20 09]
    // P0301 = 0x03 0x01 → bits 0301
    // P0420 = 0x04 0x20 → bits 0420
    Transport.EnqueueResponse([
      $59, $02, $FF,
      $03, $01, $00, $0F,
      $04, $20, $00, $09]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Dtcs := Client.ReadDtcs;
    Assert.AreEqual<Cardinal>(2, Cardinal(Length(Dtcs)));
    Assert.AreEqual('P0301', Dtcs[0].Code);
    Assert.AreEqual<Byte>($0F, Dtcs[0].StatusByte);
    Assert.AreEqual('P0420', Dtcs[1].Code);
  finally
    Catalog.Free;
  end;
end;

procedure TUdsClientTests.ReadDtcs_DecodesUCodes;
var
  Catalog: TOBDOEMJSONCatalog;
  Transport: TMockTransport;

  ITransport: IOBDDiagnosticTransport;
  Client: IOBDUdsClient;
  Dtcs: TArray<TOBDDtcInstance>;
begin
  Catalog := CatalogFromJson;
  NewMock(Transport, ITransport);
  Client := CreateUdsClient;
  try
    // U0100 (network code) — bits 15-14 = 11 → U letter
    //   Byte0 = 0xC0 (11000000), Byte1 = 0x01 → after letter mask (& $3F00): 0x0100
    Transport.EnqueueResponse([
      $59, $02, $FF,
      $C0, $01, $00, $0A]);
    OpenWithCatalog(Client, Catalog, ITransport);
    Dtcs := Client.ReadDtcs;
    Assert.AreEqual<Cardinal>(1, Cardinal(Length(Dtcs)));
    Assert.AreEqual('U0100', Dtcs[0].Code);
  finally
    Catalog.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUdsClientTests);

end.
