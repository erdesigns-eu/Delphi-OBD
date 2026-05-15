//------------------------------------------------------------------------------
//  Hello — placeholder sample
//
//  Smoke-test sample that prints the package version. Exists to verify
//  the runtime package can be linked against by an external program; will
//  be replaced by real samples (01-ConnectAndPing onwards) starting in
//  the connection layer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial placeholder.
//------------------------------------------------------------------------------

program Hello;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Version in '..\..\src\Core\OBD.Version.pas';

begin
  Writeln('Delphi-OBD ', OBD_VERSION);
  Writeln(OBD_COPYRIGHT);
  Writeln(OBD_HOMEPAGE);
end.
