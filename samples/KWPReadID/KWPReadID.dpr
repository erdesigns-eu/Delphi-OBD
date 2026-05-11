//------------------------------------------------------------------------------
//  KWPReadID — Phase 6 sample
//
//  Demonstrates TOBDKWPReadID — the KWP2000 read-identifier trio:
//
//    Service 0x1A — ReadECUIdentification        (1-byte ID)
//    Service 0x21 — ReadDataByLocalIdentifier    (1-byte LocalID)
//    Service 0x22 — ReadDataByCommonIdentifier   (2-byte DID)
//
//  Build & run:
//    dcc32 -B KWPReadID.dpr
//    KWPReadID
//------------------------------------------------------------------------------

program KWPReadID;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                  in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                 in '..\..\src\Core\OBD.Errors.pas',
  OBD.Diagnostics.KWP.ReadID in '..\..\src\Diagnostics\OBD.Diagnostics.KWP.ReadID.pas';

var
  R: TOBDKWPReadID;
begin
  Writeln('Delphi-OBD KWP2000 read-identifier demo');
  Writeln(StringOfChar('-', 60));

  R := TOBDKWPReadID.Create(nil);
  try
    Writeln('  Without a Protocol, every read raises EOBDConfig:');
    try
      R.ReadECUID($9A);                  // ECU identification
    except
      on E: EOBDConfig do Writeln('    ReadECUID         — EOBDConfig OK');
    end;
    try
      R.ReadByLocalID($01);              // Local identifier 0x01
    except
      on E: EOBDConfig do Writeln('    ReadByLocalID     — EOBDConfig OK');
    end;
    try
      R.ReadByCommonID($F190);           // VIN
    except
      on E: EOBDConfig do Writeln('    ReadByCommonID    — EOBDConfig OK');
    end;
  finally
    R.Free;
  end;

  Writeln;
  Writeln('  In a real app, wire R.Protocol and R.OnRead. The OnRead');
  Writeln('  handler receives (Kind, ID, Data) where Kind is the SID');
  Writeln('  byte that produced the response (0x1A / 0x21 / 0x22).');
  Writeln;
  Writeln('Done.');
end.
