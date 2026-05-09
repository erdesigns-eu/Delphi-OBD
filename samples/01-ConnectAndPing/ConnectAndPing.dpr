//------------------------------------------------------------------------------
//  ConnectAndPing — sample 01
//
//  Drops a TOBDConnection (code-only since this is a console sample),
//  configures it for a Wi-Fi ELM327 clone at 192.168.0.10:35000 by
//  default, opens the connection, sends 'ATZ' (the soft-reset command
//  every ELM327 understands), and prints what comes back until the
//  ELM '>' prompt or a 5-second timeout.
//
//  Override host / port via the first two command-line arguments.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

program ConnectAndPing;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Connection.Types in '..\..\src\Connection\OBD.Connection.Types.pas',
  OBD.Connection.Settings in '..\..\src\Connection\OBD.Connection.Settings.pas',
  OBD.Connection.Retry in '..\..\src\Connection\OBD.Connection.Retry.pas',
  OBD.Connection.Mock in '..\..\src\Connection\OBD.Connection.Mock.pas',
  OBD.Connection.Bluetooth in '..\..\src\Connection\OBD.Connection.Bluetooth.pas',
  OBD.Connection.BLE in '..\..\src\Connection\OBD.Connection.BLE.pas',
  OBD.Connection.WiFi in '..\..\src\Connection\OBD.Connection.WiFi.pas',
  OBD.Connection.UDP in '..\..\src\Connection\OBD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  OBD.Connection.Serial in '..\..\src\Connection\OBD.Connection.Serial.pas',
  OBD.Connection.FTDI in '..\..\src\Connection\OBD.Connection.FTDI.pas',
  {$ENDIF}
  OBD.Connection in '..\..\src\Connection\OBD.Connection.pas';

var
  Connection: TOBDConnection;
  ResponseDone: TEvent;
  ResponseText: string;

procedure HandleData(Sender: TObject; const ABytes: TBytes);
begin
  ResponseText := ResponseText + TEncoding.ASCII.GetString(ABytes);
  // ELM327 terminates every response with '>'.
  if Pos('>', ResponseText) > 0 then
    ResponseDone.SetEvent;
end;

procedure HandleError(Sender: TObject; ACode: TOBDErrorCode;
  const AMessage: string; var AHandled: Boolean);
begin
  Writeln(ErrOutput, Format('[error %d] %s', [Ord(ACode), AMessage]));
end;

var
  Host: string;
  PortStr: string;
  Port: Integer;

begin
  Host := '192.168.0.10';
  Port := 35000;
  if ParamCount >= 1 then Host := ParamStr(1);
  if ParamCount >= 2 then
  begin
    PortStr := ParamStr(2);
    if not TryStrToInt(PortStr, Port) then
    begin
      Writeln(ErrOutput, 'Invalid port: ', PortStr);
      Halt(2);
    end;
  end;

  Connection := TOBDConnection.Create(nil);
  ResponseDone := TEvent.Create(nil, True, False, '');
  try
    Connection.Transport := otWiFi;
    Connection.WiFiSettings.Host := Host;
    Connection.WiFiSettings.Port := Port;
    Connection.OnDataReceived := HandleData;
    Connection.OnError := HandleError;

    Writeln(Format('Connecting to %s:%d…', [Host, Port]));
    try
      Connection.Open;
    except
      on E: Exception do
      begin
        Writeln(ErrOutput, 'Open failed: ', E.Message);
        Halt(3);
      end;
    end;

    Writeln('Connected. Sending ATZ.');
    ResponseText := '';
    Connection.WriteString('ATZ' + #13);

    if ResponseDone.WaitFor(5000) = wrSignaled then
      Writeln('Response: ', ResponseText)
    else
      Writeln(ErrOutput, 'Timed out waiting for response.');

    Connection.Close;
  finally
    ResponseDone.Free;
    Connection.Free;
  end;
end.
