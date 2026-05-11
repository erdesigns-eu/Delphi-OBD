//------------------------------------------------------------------------------
//  DoIPDiagnostics
//
//  Demonstrates wiring TOBDDoIPClient — the ISO 13400 DoIP
//  surface (PLAN-spec'd as "TOBDDoIP"). The client owns the
//  routing-activation lifecycle and the diagnostic-message
//  request/response exchange over TCP (plain or TLS).
//
//  This sample stages the configuration without opening a
//  network socket; real apps wire the transport, call Connect,
//  and start exchanging diagnostic messages.
//
//  Build & run:
//    dcc32 -B DoIPDiagnostics.dpr
//    DoIPDiagnostics
//------------------------------------------------------------------------------

program DoIPDiagnostics;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types         in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.DoIP.Header   in '..\..\src\Protocol\OBD.Protocol.DoIP.Header.pas',
  OBD.Protocol.DoIP.Messages in '..\..\src\Protocol\OBD.Protocol.DoIP.Messages.pas',
  OBD.Protocol.DoIP.Transport in '..\..\src\Protocol\OBD.Protocol.DoIP.Transport.pas',
  OBD.Protocol.DoIP.Client   in '..\..\src\Protocol\OBD.Protocol.DoIP.Client.pas';

var
  C: TOBDDoIPClient;
begin
  Writeln('Delphi-OBD DoIP diagnostics demo');
  Writeln(StringOfChar('-', 60));

  C := TOBDDoIPClient.Create(nil);
  try
    Writeln(Format('  Default status: %d', [Ord(C.Status)]));
    Writeln;
    Writeln('  Surface (see OBD.Protocol.DoIP.Client):');
    Writeln('    - Routing activation lifecycle (0x0005 / 0x0006)');
    Writeln('    - Diagnostic message exchange (0x8001 / ACK / NACK)');
    Writeln('    - Alive-check round-trips (0x0007 / 0x0008)');
    Writeln('    - Power-mode + entity-status queries');
    Writeln('    - OnVehicleAnnouncement for UDP discovery announcements');
    Writeln('    - OpenSSL DLL drop-in for TLS 1.2 / 1.3 transports');
  finally
    C.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
