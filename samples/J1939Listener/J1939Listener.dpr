//------------------------------------------------------------------------------
//  J1939Listener
//
//  Demonstrates the TOBDJ1939 bus-client surface: SourceAddress +
//  NAME configuration, address-claim payload assembly, the
//  exposed session manager, and the DispatchInbound routing for
//  single-frame and TP/ETP-reassembled messages.
//
//  This sample stages the wiring without driving a real CAN
//  channel. Real apps wire <c>J.Sessions.OnFrameSend</c> to a
//  CAN transmit path (TOBDProtocol with HeaderOverride, J2534,
//  direct CAN driver) and call <c>J.DispatchInbound(canId, bytes)</c>
//  for every received J1939 frame.
//
//  Build & run:
//    dcc32 -B J1939Listener.dpr
//    J1939Listener
//------------------------------------------------------------------------------

program J1939Listener;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types      in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.J1939      in '..\..\src\Protocol\OBD.Protocol.J1939.pas',
  OBD.Protocol.J1939.TP   in '..\..\src\Protocol\OBD.Protocol.J1939.TP.pas',
  OBD.Diagnostics.J1939   in '..\..\src\Diagnostics\OBD.Diagnostics.J1939.pas';

var
  J: TOBDJ1939;
  Body: TBytes;
  Id: Cardinal;
  Hit: Integer;
begin
  Writeln('Delphi-OBD J1939 listener demo');
  Writeln(StringOfChar('-', 60));

  J := TOBDJ1939.Create(nil);
  try
    J.SourceAddress := $42;
    J.SetNAME([$01, $02, $03, $04, $05, $06, $07, $08]);

    Body := J.BuildAddressClaimedPayload;
    Writeln(Format('  Address-Claimed payload: %d bytes', [Length(Body)]));
    Write  ('    ');
    for var I := 0 to High(Body) do
      Write(Format(' %.2x', [Body[I]]));
    Writeln;

    Body := J.BuildRequestPayload(J1939_PGN_DM1);
    Writeln('  Request(DM1) payload (LE 3 bytes):');
    Write  ('    ');
    for var I := 0 to High(Body) do
      Write(Format(' %.2x', [Body[I]]));
    Writeln;

    // Demonstrate inbound dispatch. Build a DM1-shaped CAN ID from
    // source 0x33 and route a fake payload through the component;
    // OnFrame fires synchronously for non-TP PGNs.
    Hit := 0;
    J.OnFrame :=
      procedure(Sender: TObject; APriority: Byte; APGN: Cardinal;
        ASA: Byte; ADA: Byte; const AData: TBytes)
      begin
        Inc(Hit);
        Writeln(Format(
          '  OnFrame: pri=%d PGN=0x%.4x SA=0x%.2x DA=0x%.2x bytes=%d',
          [APriority, APGN, ASA, ADA, Length(AData)]));
      end;
    Id := TOBDJ1939Codec.EncodeId(3, J1939_PGN_DM1, $33, $FF);
    J.DispatchInbound(Id,
      TBytes.Create($00, $FF, $00, $00, $00, $00, $00, $00));
    Writeln(Format('  Inbound dispatch fired %d frame(s).', [Hit]));
  finally
    J.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
