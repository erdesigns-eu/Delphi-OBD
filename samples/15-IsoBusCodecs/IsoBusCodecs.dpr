//------------------------------------------------------------------------------
//  IsoBusCodecs — sample 15
//
//  Exercises the ISO 11783 (ISOBUS) codec helpers in
//  src/Speciality/. These are pure class-static functions
//  that build / decode 8-byte CAN payloads — useful in any
//  ag-CAN host without needing a bus.
//
//  Demonstrates:
//    TOBDIsoBus      - NAME encode/decode, address claim
//    TOBDIsoBusVT    - Virtual Terminal request frames
//    TOBDIsoBusTC    - Task Controller value / set-value frames
//    TOBDIsoBusFS    - File Server request frames
//    TOBDIsoBusGNSS  - PGN 129025 / 129026 decode
//------------------------------------------------------------------------------

program IsoBusCodecs;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Speciality.IsoBus      in '..\..\src\Speciality\OBD.Speciality.IsoBus.pas',
  OBD.Speciality.IsoBus.VT   in '..\..\src\Speciality\OBD.Speciality.IsoBus.VT.pas',
  OBD.Speciality.IsoBus.TC   in '..\..\src\Speciality\OBD.Speciality.IsoBus.TC.pas',
  OBD.Speciality.IsoBus.FS   in '..\..\src\Speciality\OBD.Speciality.IsoBus.FS.pas',
  OBD.Speciality.IsoBus.GNSS in '..\..\src\Speciality\OBD.Speciality.IsoBus.GNSS.pas';

procedure DumpHex(const ALabel: string; const ABytes: TBytes);
var I: Integer;
begin
  Write(ALabel:32, ' ');
  for I := 0 to High(ABytes) do
    Write(Format('%.2x ', [ABytes[I]]));
  Writeln;
end;

procedure NameRoundTrip;
var
  N1, N2: TOBDIsoBusName;
  B:      TBytes;
begin
  Writeln('-- ISO 11783 NAME round-trip --');
  N1 := Default(TOBDIsoBusName);
  N1.IdentityNumber  := $123456;
  N1.ManufacturerCode := $0042;
  N1.Function_ := 130;
  N1.DeviceClass := 5;
  N1.IndustryGroup := 2;          // Agricultural & Forestry
  N1.SelfConfigurableAddress := True;
  B := TOBDIsoBus.EncodeName(N1);
  DumpHex('  encoded NAME', B);
  if TOBDIsoBus.DecodeName(B, N2) then
    Writeln(Format('  decoded.IdentityNumber=$%.6x  manuf=$%.3x  func=%d',
      [N2.IdentityNumber, N2.ManufacturerCode, N2.Function_]));
end;

procedure VTAndTC;
begin
  Writeln('-- Virtual Terminal frames --');
  DumpHex('  VT.GetMemory(65536)',     TOBDIsoBusVT.BuildGetMemory(65536));
  DumpHex('  VT.AudioSignal(3,1k)',    TOBDIsoBusVT.BuildAudioSignal(3, 1000, 200, 100));
  DumpHex('  VT.EndOfObjectPool',      TOBDIsoBusVT.BuildEndOfObjectPool);
  Writeln('-- Task Controller frames --');
  DumpHex('  TC.Value(1,100,12345)',   TOBDIsoBusTC.BuildValue(1, 100, 12345));
  DumpHex('  TC.SetValue(1,100,...)',  TOBDIsoBusTC.BuildSetValue(1, 100, 12345));
  DumpHex('  TC.RequestVersion',       TOBDIsoBusTC.BuildRequestVersion);
end;

procedure FSAndGNSS;
var
  Bytes:    TBytes;
  Lat, Lon: Double;
  Cog, Sog: Double;
begin
  Writeln('-- File Server frames --');
  DumpHex('  FS.GetProperties(1)', TOBDIsoBusFS.BuildGetProperties(1));
  DumpHex('  FS.CloseFile(1, h7)', TOBDIsoBusFS.BuildCloseFile(1, 7));

  Writeln('-- GNSS PGN 129025 (Position Rapid) decode --');
  // 50.0 deg lat, 5.0 deg lon (LE, 1e-7 deg units).
  Bytes := TBytes.Create($00,$65,$CD,$1D, $80,$F0,$FA,$02);
  if TOBDIsoBusGNSS.DecodePositionRapid(Bytes, Lat, Lon) then
    Writeln(Format('  lat=%.4f lon=%.4f', [Lat, Lon]));

  Writeln('-- GNSS PGN 129026 (COG/SOG) decode --');
  Bytes := TBytes.Create($00, $00, $FF,
                         $10, $27, $F4, $01, $FF);
  if TOBDIsoBusGNSS.DecodeCogSog(Bytes, Cog, Sog) then
    Writeln(Format('  cog=%.3f rad  sog=%.2f m/s', [Cog, Sog]));
end;

begin
  Writeln('ISO 11783 (ISOBUS) codec demo');
  Writeln(StringOfChar('-', 60));
  NameRoundTrip;
  VTAndTC;
  FSAndGNSS;
  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
