//------------------------------------------------------------------------------
//  WWHOBD — World-Wide Harmonised OBD demo
//
//  Demonstrates the HD-OBD diagnostic surface for vehicles compliant
//  with ISO 27145:
//    - TOBDWWHOBD: read DTCs by severity / readiness group, MIL
//      usage counters, DTC class A / B1 counters, VIN.
//    - TOBDWWHReadiness: read the major-group readiness summary
//      and per-group conditions-encountered ratios.
//
//  Build & run:
//    dcc32 -B WWHOBD.dpr
//    WWHOBD
//------------------------------------------------------------------------------

program WWHOBD;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.Protocol.Types      in '..\..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.WWHOBD              in '..\..\src\Service\OBD.WWHOBD.pas',
  OBD.WWHOBD.Readiness    in '..\..\src\Service\OBD.WWHOBD.Readiness.pas';

procedure ShowSeverityMap;
const
  Bits: array[0..3] of Byte = (
    $00,
    WWHOBD_SEV_MAINTENANCE_ONLY,
    WWHOBD_SEV_CHECK_AT_NEXT_HALT,
    WWHOBD_SEV_CHECK_IMMEDIATELY);
  Names: array[0..3] of string = (
    'zero', 'MaintenanceOnly', 'CheckAtNextHalt', 'CheckImmediately');
var
  I: Integer;
  Cls: TOBDWWHDtcClass;
begin
  Writeln('  Severity-byte classification:');
  for I := Low(Bits) to High(Bits) do
  begin
    Cls := TOBDWWHOBD.ClassifySeverity(Bits[I]);
    Writeln(Format('    %.2x (%s) -> %d',
      [Bits[I], Names[I], Ord(Cls)]));
  end;
  Cls := TOBDWWHOBD.ClassifySeverity(
    WWHOBD_SEV_CHECK_IMMEDIATELY or WWHOBD_SEV_CHECK_AT_NEXT_HALT);
  Writeln(Format('    %.2x (CheckBoth) -> %d',
    [WWHOBD_SEV_CHECK_IMMEDIATELY or WWHOBD_SEV_CHECK_AT_NEXT_HALT,
     Ord(Cls)]));
end;

var
  W: TOBDWWHOBD;
  R: TOBDWWHReadiness;
begin
  Writeln('Delphi-OBD WWH-OBD (ISO 27145) demo');
  Writeln(StringOfChar('-', 60));

  ShowSeverityMap;
  Writeln;

  W := TOBDWWHOBD.Create(nil);
  R := TOBDWWHReadiness.Create(nil);
  try
    Writeln(Format('  TOBDWWHOBD.SeverityMask default = 0x%.2x',
      [W.SeverityMask]));
    Writeln('  Without a Protocol, ReadBySeverity raises EOBDConfig:');
    try
      W.ReadBySeverity;
    except
      on E: EOBDConfig do Writeln('    EOBDConfig — OK');
    end;
    Writeln('  Without a Protocol, TOBDWWHReadiness.Read raises EOBDConfig:');
    try
      R.Read;
    except
      on E: EOBDConfig do Writeln('    EOBDConfig — OK');
    end;
  finally
    W.Free;
    R.Free;
  end;

  Writeln;
  Writeln('  Real-vehicle wiring:');
  Writeln('    1. Assign W.Protocol and R.Protocol to a TOBDProtocol');
  Writeln('       bound to a UDS-capable adapter on the HD vehicle.');
  Writeln('    2. W.ReadBySeverity to enumerate every Class A/B DTC.');
  Writeln('    3. R.Read to get the major-group readiness flags.');
  Writeln;
  Writeln('Done.');
end.
