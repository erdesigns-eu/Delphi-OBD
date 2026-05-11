//------------------------------------------------------------------------------
//  FlexRaySlot
//
//  Demonstrates building a TOBDFlexRayFrame for a single static-
//  segment slot and encoding it through the FlexRay codec.
//
//  Build & run:
//    dcc32 -B FlexRaySlot.dpr
//    FlexRaySlot
//------------------------------------------------------------------------------

program FlexRaySlot;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                    in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                   in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.FlexRay.Frame   in '..\..\src\Protocol\OBD.Protocol.FlexRay.Frame.pas';

var
  Frame: TOBDFlexRayFrame;
  Wire: TBytes;
  I: Integer;
begin
  Writeln('Delphi-OBD FlexRay slot encode demo');
  Writeln(StringOfChar('-', 60));

  Frame := Default(TOBDFlexRayFrame);
  Frame.Header.SyncFrame := True;
  Frame.Header.FrameID := 5;                  // static-segment slot 5
  Frame.Header.PayloadLengthWords := 2;       // 4 bytes payload
  Frame.Header.CycleCount := 12;
  Frame.Payload := TBytes.Create($DE, $AD, $BE, $EF);

  Wire := FlexRayEncodeFrame(Frame);
  Writeln(Format('  Encoded frame: %d wire bytes', [Length(Wire)]));
  Write  ('    ');
  for I := 0 to High(Wire) do
    Write(Format(' %.2x', [Wire[I]]));
  Writeln;

  Writeln;
  Writeln('Done.');
end.
