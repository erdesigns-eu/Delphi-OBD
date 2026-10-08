program Smoke;
uses SysUtils, OBD.Types, OBD.Version, OBD.Errors,
  OBD.Protocol.LIN.Frame, OBD.Protocol.FlexRay.Frame, OBD.Protocol.MOST.Control;
var
  I, Checks: Integer;
  PID, ID: Byte;
  L, L2: TOBDLINFrame;
  M, M2: TOBDMOSTControlMessage;
  F, F2: TOBDFlexRayFrame;
  Bytes: TBytes;
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
  Writeln(Checks, ' checks passed');
end.
