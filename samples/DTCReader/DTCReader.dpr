//------------------------------------------------------------------------------
//  DTCReader — Phase 5 DTC reader / clear demo
//
//  Demonstrates the J2012 decode helper on TOBDDTCs (shared with
//  TOBDWWHOBD) and the TOBDClearDTC safety contract.
//
//  Build & run:
//    dcc32 -B DTCReader.dpr
//    DTCReader
//------------------------------------------------------------------------------

program DTCReader;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types      in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Catalog             in '..\..\src\Core\OBD.Catalog.pas',
  OBD.Decoders            in '..\..\src\Core\OBD.Decoders.pas',
  OBD.Service.Catalog     in '..\..\src\Service\OBD.Service.Catalog.pas',
  OBD.Service.DTCs        in '..\..\src\Service\OBD.Service.DTCs.pas',
  OBD.ClearDTC            in '..\..\src\Service\OBD.ClearDTC.pas';

procedure Decode(AHi, ALo: Byte);
begin
  Writeln(Format('  %.2x %.2x  ->  %s',
    [AHi, ALo, TOBDDTCs.DecodeJ2012(AHi, ALo)]));
end;

procedure ShowClearContract;
var
  C: TOBDClearDTC;
begin
  Writeln;
  Writeln('  ClearDTC safety contract:');
  C := TOBDClearDTC.Create(nil);
  try
    Writeln(Format('    AutoExecute (default):  %s',
      [BoolToStr(C.AutoExecute, True)]));
    Writeln(Format('    Dialect (default):      %d (cdOBDII)',
      [Ord(C.Dialect)]));
    Writeln(Format('    UDSGroup (default):     $%.6x',
      [C.UDSGroup]));
    Writeln('    Calling Clear without AutoExecute=True or without');
    Writeln('    a Protocol raises EOBDConfig — verified in');
    Writeln('    Tests.OBD.Service.Phase5.');
  finally C.Free; end;
end;

begin
  Writeln('Delphi-OBD DTC reader / clear demo');
  Writeln(StringOfChar('-', 60));

  Writeln('  J2012 decode samples:');
  Decode($01, $43);  // P0143
  Decode($42, $20);  // C0220
  Decode($83, $00);  // B0300
  Decode($C0, $00);  // U0000

  ShowClearContract;

  Writeln;
  Writeln('Done.');
end.
