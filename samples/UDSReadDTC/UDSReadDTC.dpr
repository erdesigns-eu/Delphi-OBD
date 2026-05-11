//------------------------------------------------------------------------------
//  UDSReadDTC — Phase 6 sample
//
//  Demonstrates TOBDUDSReadDTC sub-function helpers and the
//  J2012 5-character decoder shared with the J1939 DM surface.
//
//  Build & run:
//    dcc32 -B UDSReadDTC.dpr
//    UDSReadDTC
//------------------------------------------------------------------------------

program UDSReadDTC;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                    in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                   in '..\..\src\Core\OBD.Errors.pas',
  OBD.Diagnostics.UDS.ReadDTC  in '..\..\src\Diagnostics\OBD.Diagnostics.UDS.ReadDTC.pas';

procedure DecodeOne(AHi: Byte; ALo: Byte);
begin
  Writeln(Format('  %.2x %.2x  ->  %s',
    [AHi, ALo, TOBDUDSReadDTC.DecodeJ2012(AHi, ALo)]));
end;

var
  R: TOBDUDSReadDTC;
begin
  Writeln('Delphi-OBD UDS ReadDTCInformation demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  J2012 decoder samples:');
  DecodeOne($01, $43);
  DecodeOne($42, $20);
  DecodeOne($83, $00);
  DecodeOne($C0, $00);

  Writeln;
  R := TOBDUDSReadDTC.Create(nil);
  try
    Writeln('  Without a Protocol, ReadByStatusMask raises EOBDConfig:');
    try
      R.ReadByStatusMask($FF);
    except
      on E: EOBDConfig do
        Writeln('    EOBDConfig — OK');
    end;
  finally
    R.Free;
  end;

  Writeln;
  Writeln('  Available sub-functions:');
  Writeln('    ReadByStatusMask (0x02), ReadSupportedDTCs (0x0A),');
  Writeln('    ReadByDTCNumber  (0x06), Send(subFunc, body) escape hatch.');
  Writeln;
  Writeln('Done.');
end.
