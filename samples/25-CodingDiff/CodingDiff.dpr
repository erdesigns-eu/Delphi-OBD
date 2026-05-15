//------------------------------------------------------------------------------
//  CodingDiff — sample 25
//
//  Demonstrates TOBDCodingDiff:
//    - Compute a byte-level diff between a Before and an After buffer.
//    - Apply that diff to a Before buffer to recreate After.
//    - Revert it to roll After back to Before.
//
//  Build & run:
//    dcc32 -B CodingDiff.dpr
//    CodingDiff
//------------------------------------------------------------------------------

program CodingDiff;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types         in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors        in '..\..\src\Core\OBD.Errors.pas',
  OBD.Coding.Diff   in '..\..\src\Coding\OBD.Coding.Diff.pas';

procedure DumpHex(const ALabel: string; const ABuf: TBytes);
var I: Integer; S: string;
begin
  S := '';
  for I := 0 to High(ABuf) do
    S := S + Format(' %.2x', [ABuf[I]]);
  Writeln(Format('  %-12s%s', [ALabel + ':', S]));
end;

var
  Before, After, Applied, Reverted: TBytes;
  D: TOBDCodingDiffResult;
begin
  Writeln('Delphi-OBD coding-diff demo');
  Writeln(StringOfChar('-', 60));

  Before := TBytes.Create($AA, $BB, $CC, $DD, $EE);
  After  := TBytes.Create($AA, $11, $CC, $22, $EE);
  DumpHex('before', Before);
  DumpHex('after',  After);

  D := TOBDCodingDiff.Compute(Before, After);
  Writeln(Format('  diff:        %d change(s), OldLength=%d, NewLength=%d',
    [Length(D.Changes), D.OldLength, D.NewLength]));
  for var I := 0 to High(D.Changes) do
    Writeln(Format('    offset %d:  %.2x -> %.2x',
      [D.Changes[I].Offset, D.Changes[I].Before, D.Changes[I].After]));

  Applied := TOBDCodingDiff.Apply(Before, D);
  DumpHex('apply(B,D)', Applied);

  Reverted := TOBDCodingDiff.Revert(After, D);
  DumpHex('revert(A,D)', Reverted);

  Writeln;
  Writeln('Done.');
end.
