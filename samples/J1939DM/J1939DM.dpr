//------------------------------------------------------------------------------
//  J1939DM
//
//  Demonstrates TOBDJ1939DM — structured SPN/FMI/CM/OC decoding
//  of a DM1 body. Builds a synthetic two-DTC payload and dumps
//  the decoded entries.
//
//  Build & run:
//    dcc32 -B J1939DM.dpr
//    J1939DM
//------------------------------------------------------------------------------

program J1939DM;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors               in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types       in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.J1939       in '..\..\src\Protocol\OBD.Protocol.J1939.pas',
  OBD.Diagnostics.J1939.DM in '..\..\src\Diagnostics\OBD.Diagnostics.J1939.DM.pas';

var
  DM: TOBDJ1939DM;
  Body: TBytes;
begin
  Writeln('Delphi-OBD J1939 DM1 decoder demo');
  Writeln(StringOfChar('-', 60));

  DM := TOBDJ1939DM.Create(nil);
  try
    DM.OnDTCs :=
      procedure(Sender: TObject; APGN: Cardinal;
        const ALamps: TOBDJ1939Lamps;
        const AEntries: TArray<TOBDJ1939DtcEntry>)
      var
        I: Integer;
      begin
        Writeln(Format('  PGN 0x%.4x   MIL=%s  entries=%d',
          [APGN, BoolToStr(ALamps.MIL, True), Length(AEntries)]));
        for I := 0 to High(AEntries) do
          Writeln(Format('    [%d] SPN=%u  FMI=%d  CM=%d  OC=%d',
            [I, AEntries[I].SPN, AEntries[I].FMI,
             AEntries[I].CM, AEntries[I].OC]));
      end;

    // Synthetic DM1 body: lamps byte 0 = 0x40 (MIL on), byte 1 = 0xFF
    // (reserved). Two DTC records:
    //   - SPN 100 (Engine Oil Pressure), FMI 1, CM=0, OC=3
    //   - SPN 190 (Engine Speed),        FMI 0, CM=0, OC=1
    // SPN 100 = 0x064 -> lo 0x64, mid 0x00, hi 0; byte 2 = 0x01
    // SPN 190 = 0x0BE -> lo 0xBE, mid 0x00, hi 0; byte 2 = 0x00
    Body := TBytes.Create(
      $40, $FF,
      $64, $00, $01, $03,
      $BE, $00, $00, $01);
    DM.DispatchDM(J1939_PGN_DM1, Body);
  finally
    DM.Free;
  end;

  Writeln;
  Writeln('Done.');
end.
