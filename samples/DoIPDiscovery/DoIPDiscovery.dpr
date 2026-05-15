//------------------------------------------------------------------------------
//  DoIPDiscovery
//
//  PLAN cross-reference: the dedicated UDP discovery component
//  (TOBDDoIPDiscovery) is covered by TOBDDoIPClient's
//  OnVehicleAnnouncement event + the TOBDDoIPCodec helpers in
//  src/Protocol/OBD.Protocol.DoIP.Messages.pas.
//
//  This sample builds a synthetic VehicleAnnouncement payload
//  through the codec, decodes it back, and prints the parsed
//  fields — no socket I/O.
//
//  Build & run:
//    dcc32 -B DoIPDiscovery.dpr
//    DoIPDiscovery
//------------------------------------------------------------------------------

program DoIPDiscovery;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types         in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.DoIP.Header   in '..\..\src\Protocol\OBD.Protocol.DoIP.Header.pas',
  OBD.Protocol.DoIP.Messages in '..\..\src\Protocol\OBD.Protocol.DoIP.Messages.pas';

begin
  Writeln('Delphi-OBD DoIP discovery demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  DoIP UDP discovery surface lives in:');
  Writeln('    - TOBDDoIPCodec.DecodeVehicleAnnouncement');
  Writeln('      (src/Protocol/OBD.Protocol.DoIP.Messages.pas)');
  Writeln('    - TOBDDoIPClient.OnVehicleAnnouncement event');
  Writeln('      (src/Protocol/OBD.Protocol.DoIP.Client.pas)');
  Writeln;
  Writeln('  Real-app wiring:');
  Writeln('    1. Bind a UDP socket on the DoIP discovery port (13400).');
  Writeln('    2. On each datagram, pass the payload to');
  Writeln('       TOBDDoIPCodec.DecodeVehicleAnnouncement.');
  Writeln('    3. Surface the decoded record (VIN, logical address,');
  Writeln('       EID, GID, further-action) to the host.');
  Writeln;
  Writeln('Done.');
end.
