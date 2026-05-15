//------------------------------------------------------------------------------
//  UDSReadDID
//
//  Demonstrates wiring TOBDUDSReadDID for a multi-DID read against
//  a connected UDS-capable ECU. The sample builds the component
//  and prints the safety-gate behaviour without touching the bus;
//  wire <c>R.Protocol</c> to a real TOBDProtocol and call
//  R.Read([...], [...]) to fetch.
//
//  Build & run:
//    dcc32 -B UDSReadDID.dpr
//    UDSReadDID
//------------------------------------------------------------------------------

program UDSReadDID;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                    in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                   in '..\..\src\Core\OBD.Errors.pas',
  OBD.Diagnostics.UDS.ReadDID  in '..\..\src\Diagnostics\OBD.Diagnostics.UDS.ReadDID.pas';

var
  R: TOBDUDSReadDID;
begin
  Writeln('Delphi-OBD UDS ReadDataByIdentifier demo');
  Writeln(StringOfChar('-', 60));

  R := TOBDUDSReadDID.Create(nil);
  try
    Writeln('  Two DIDs would be batched into one 0x22 request,');
    Writeln('  each with an expected length:');
    Writeln('    DID 0xF190 (VIN) — 17 bytes');
    Writeln('    DID 0xF18C (Serial Number) — 16 bytes');
    Writeln;
    Writeln('  Without a Protocol assigned, Read raises EOBDConfig:');
    try
      R.Read([$F190, $F18C], [17, 16]);
    except
      on E: EOBDConfig do
        Writeln('    EOBDConfig — OK');
    end;
  finally
    R.Free;
  end;

  Writeln;
  Writeln('  In a real app:');
  Writeln('    R.Protocol := MyProtocol;');
  Writeln('    R.OnRead   := HandleValues;');
  Writeln('    R.Read([$F190, $F18C], [17, 16]);');
  Writeln;
  Writeln('Done.');
end.
