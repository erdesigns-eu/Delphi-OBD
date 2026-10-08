program Smoke;
uses SysUtils, ERD.Binary.Value, ERD.CAN.Route, ERD.Protocol.Types,
  ERD.Service.EVBattery.Types, ERD.Service.EVBattery.Request, ERD.Types, ERD.Version, ERD.Errors,
  ERD.Protocol.LIN.Frame, ERD.Protocol.FlexRay.Frame, ERD.Protocol.MOST.Control;
var
  I, Checks: Integer;
  PID, ID: Byte;
  L, L2: TOBDLINFrame;
  M, M2: TOBDMOSTControlMessage;
  F, F2: TOBDFlexRayFrame;
  Bytes: TBytes;
  Commands: TArray<string>;
  Rule: TOBDEVBatteryRule;
  Request: TOBDRequest;
  Response: TOBDResponse;
procedure Check(AValue: Boolean; const AName: string);
begin
  Inc(Checks);
  if not AValue then raise Exception.Create(AName);
end;
begin
  Checks := 0;
  Check(OBD_VERSION_MAJOR = 2, 'version');
  for I := Ord(Low(TOBDErrorCode)) to Ord(High(TOBDErrorCode)) do
    Check(OBDErrorCodeToMessage(TOBDErrorCode(I)) <> '', 'error message');
  for I := 0 to 63 do begin
    PID := LINMakePID(I);
    Check(LINDecodePID(PID, ID) and (ID = I), 'LIN PID round trip');
    Check(not LINDecodePID(PID xor $40, ID), 'LIN corrupt parity');
  end;
  Check(LINChecksum(csClassic, 0, TBytes.Create($55, $33, $11)) = $66, 'LIN classic golden');
  Check(LINChecksum(csEnhanced, LINMakePID($05), TBytes.Create(1, 2, 3, 4)) = $70, 'LIN enhanced golden');
  L := Default(TOBDLINFrame);
  L.FrameID := $12;
  L.Checksum := csEnhanced;
  L.Data := TBytes.Create($11, $22, $33);
  Bytes := LINEncodeFrame(L);
  Check(LINDecodeFrame(Bytes, 3, L2), 'LIN round trip');
  Check((L2.FrameID = $12) and (Length(L2.Data) = 3), 'LIN contents');
  SetLength(Bytes, Length(Bytes) + 1);
  Check(not LINDecodeFrame(Bytes, 3, L2), 'LIN trailing byte');
  SetLength(Bytes, Length(Bytes) - 1);
  Bytes[High(Bytes)] := Bytes[High(Bytes)] xor 1;
  Check(not LINDecodeFrame(Bytes, 3, L2), 'LIN corrupt checksum');
  M := Default(TOBDMOSTControlMessage);
  M.SourceAddress := $1234; M.DestinationAddress := $2345;
  M.FBlockID := $42; M.FktID := $123; M.OPType := MOST_OP_Get;
  M.Data := TBytes.Create(1, 2, 3);
  Bytes := MOSTEncodeControl(M, msMOST25);
  Check(MOSTDecodeControl(Bytes, M2), 'MOST round trip');
  Check((M2.SourceAddress = $1234) and (M2.FktID = $123)
    and (Length(M2.Data) = 3), 'MOST contents');
  F := Default(TOBDFlexRayFrame);
  F.Header.FrameID := 12; F.Header.CycleCount := 4;
  F.Header.PayloadLengthWords := 2;
  F.Payload := TBytes.Create(1, 2, 3, 4);
  Bytes := FlexRayEncodeFrame(F);
  Check(FlexRayDecodeFrame(Bytes, F2), 'FlexRay round trip');
  Check((F2.Header.FrameID = 12) and (Length(F2.Payload) = 4), 'FlexRay contents');
  SetLength(Bytes, Length(Bytes) + 1);
  Check(not FlexRayDecodeFrame(Bytes, F2), 'FlexRay trailing byte');
  SetLength(Bytes, Length(Bytes) - 1);
  Bytes[High(Bytes)] := Bytes[High(Bytes)] xor 1;
  Check(not FlexRayDecodeFrame(Bytes, F2), 'FlexRay corrupt CRC');
  Check(CANHeader($7E4) = '7E4', 'CAN 11-bit formatting');
  Check(CANHeader($18DA10F1) = '18DA10F1', 'CAN 29-bit formatting');
  Commands := CANRouteCommands('7e4', '7ec', False, 0, 0);
  Check((Length(Commands) = 3) and (Commands[0] = 'ATSH7E4') and
    (Commands[1] = 'ATCRA7EC') and (Commands[2] = 'ATCEA'), 'normal CAN route golden');
  Commands := CANRouteCommands('6F1', '607', True, $07, $F1);
  Check((Length(Commands) = 4) and (Commands[2] = 'ATCEA07') and
    (Commands[3] = 'ATCERF1'), 'BMW extended route golden');
  Check(Length(CANRouteCommands('', '', False, 0, 0)) = 0, 'preserve implicit route');
  try
    CANRouteCommands('800', '', False, 0, 0);
    Check(False, 'invalid 11-bit header accepted');
  except on E: EOBDConfig do Check(True, 'reject 11-bit overflow'); end;
  try
    CANRouteCommands('7E4' + #13 + 'ATZ', '', False, 0, 0);
    Check(False, 'command injection accepted');
  except on E: EOBDConfig do Check(True, 'reject injected route'); end;
  try
    CANRouteCommands('6F1', '', True, 7, $F1);
    Check(False, 'extended route without filter accepted');
  except on E: EOBDConfig do Check(True, 'require extended receive ID'); end;
  Rule := Default(TOBDEVBatteryRule);
  Rule.Service := $22; Rule.DIDOrPID := $DDB7;
  Rule.RequestId := $6F1; Rule.ResponseId := $607;
  Rule.UseExtendedAddressing := True; Rule.ExtendedTarget := 7; Rule.ExtendedTester := $F1;
  Request := MakeEVBatteryRequest(Rule);
  Check((Request.Protocol = apUDS) and (Request.HeaderOverride = '6F1') and
    (Request.ResponseHeaderOverride = '607') and Request.UseExtendedAddressing,
    'EV routed UDS request');
  Check((Length(Request.Data) = 2) and (Request.Data[0] = $DD) and
    (Request.Data[1] = $B7), 'EV DID request golden');
  Response := MakeOBDResponse;
  Response.ServiceID := $62; Response.Data := TBytes.Create($DD, $B7, $9C, $40);
  Bytes := EVBatteryResponseData(Rule, Response);
  Check((Length(Bytes) = 2) and (Bytes[0] = $9C) and (Bytes[1] = $40),
    'EV response strips DID echo before scaling');
  Response.Data[1] := $B8;
  try
    EVBatteryResponseData(Rule, Response);
    Check(False, 'wrong DID accepted');
  except on E: EOBDProtocolErr do Check(True, 'reject wrong DID'); end;
  Response.Data := TBytes.Create($DD);
  try
    EVBatteryResponseData(Rule, Response);
    Check(False, 'truncated echo accepted');
  except on E: EOBDProtocolErr do Check(True, 'reject truncated echo'); end;
  Rule.Service := $21; Rule.DIDOrPID := 1;
  Request := MakeEVBatteryRequest(Rule);
  Check((Request.Protocol = apKWP2000) and (Length(Request.Data) = 1) and
    (Request.Data[0] = 1), 'Nissan Mode 21 request');
  Response.ServiceID := $61; Response.Data := TBytes.Create(1, $64);
  Bytes := EVBatteryResponseData(Rule, Response);
  Check((Length(Bytes) = 1) and (Bytes[0] = $64), 'Mode 21 PID echo stripped');
  Bytes := EncodeIntegerBE(-128, 1, True);
  Check((Length(Bytes) = 1) and (Bytes[0] = $80), 'signed int8 minimum');
  Check(DecodeIntegerBE(Bytes, 0, 1, True) = -128, 'signed int8 decode');
  Bytes := EncodeIntegerBE(-32768, 2, True);
  Check((Bytes[0] = $80) and (Bytes[1] = 0), 'signed int16 endian golden');
  Check(DecodeIntegerBE(Bytes, 0, 2, True) = -32768, 'signed int16 decode');
  Bytes := EncodeIntegerBE($12345678, 4, False);
  Check((Bytes[0] = $12) and (Bytes[1] = $34) and (Bytes[2] = $56) and
    (Bytes[3] = $78), 'uint32 big endian golden');
  Check(DecodeIntegerBE(Bytes, 0, 4, False) = $12345678, 'uint32 decode');
  Check(SignExtendBits($FF, 8) = -1, 'sign extension int8');
  Check(SignExtendBits($80, 8) = -128, 'sign extension minimum');
  Check(SignExtendBits($7F, 8) = 127, 'sign extension positive');
  try
    EncodeIntegerBE(128, 1, True);
    Check(False, 'signed int8 overflow accepted');
  except on E: EOBDConfig do Check(True, 'reject signed overflow'); end;
  try
    EncodeIntegerBE(-1, 1, False);
    Check(False, 'unsigned negative accepted');
  except on E: EOBDConfig do Check(True, 'reject unsigned negative'); end;
  try
    DecodeIntegerBE(TBytes.Create(1), 0, 2, False);
    Check(False, 'truncated integer accepted');
  except on E: EOBDConfig do Check(True, 'reject truncated integer'); end;
  Rule := Default(TOBDEVBatteryRule);
  Rule.MinModelYear := 2020;
  Rule.MaxModelYear := 9999;
  Check(EVBatteryRuleApplies(Rule, 2020), 'EV model-year inclusive boundary');
  Check(not EVBatteryRuleApplies(Rule, 2019), 'EV previous generation excluded');
  try
    EVBatteryRuleApplies(Rule, 0);
    Check(False, 'EV unspecified model year accepted');
  except on E: EOBDConfig do Check(True, 'EV model-dependent rule requires year'); end;
  Check(FieldKindFromName('capacity_remaining_ah') = efkCapacityRemainingAh,
    'ampere-hour capacity remains distinct from SOH percentage');
  Writeln(Checks, ' checks passed');
end.
