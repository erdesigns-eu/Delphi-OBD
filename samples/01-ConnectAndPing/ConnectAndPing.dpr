//------------------------------------------------------------------------------
//  ConnectAndPing — sample 01
//
//  Drops a TOBDConnection (code-only since this is a console sample),
//  configures it for a Wi-Fi ELM327 clone at 192.168.0.10:35000 by
//  default, opens the connection, sends 'ATZ' (the soft-reset command
//  every ELM327 understands), and prints what comes back until the
//  ELM '>' prompt or a 5-second timeout.
//
//  Demonstrates the dual-method rule (PLAN §3.7): pass --async to use
//  the non-blocking OpenAsync variant; without the flag the sample
//  uses the blocking Open form.
//
//  Override host / port via the first two command-line arguments.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//    2026-05-09  ERD  Follow-up: --async flag for OpenAsync demo.
//------------------------------------------------------------------------------

program ConnectAndPing;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  ERD.Types in '..\..\src\Core\ERD.Types.pas',
  ERD.Connection.Types in '..\..\src\Connection\ERD.Connection.Types.pas',
  ERD.Connection.Settings in '..\..\src\Connection\ERD.Connection.Settings.pas',
  ERD.Connection.Retry in '..\..\src\Connection\ERD.Connection.Retry.pas',
  ERD.Connection.Transport.Base in '..\..\src\Connection\ERD.Connection.Transport.Base.pas',
  ERD.Connection.Mock in '..\..\src\Connection\ERD.Connection.Mock.pas',
  ERD.Connection.Bluetooth in '..\..\src\Connection\ERD.Connection.Bluetooth.pas',
  ERD.Connection.BLE in '..\..\src\Connection\ERD.Connection.BLE.pas',
  ERD.Connection.WiFi in '..\..\src\Connection\ERD.Connection.WiFi.pas',
  ERD.Connection.UDP in '..\..\src\Connection\ERD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  ERD.Connection.Serial in '..\..\src\Connection\ERD.Connection.Serial.pas',
  ERD.Connection.FTDI in '..\..\src\Connection\ERD.Connection.FTDI.pas',
  {$ENDIF}
  ERD.Connection in '..\..\src\Connection\ERD.Connection.pas';

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

procedure HandleProgress(Sender: TObject; const AStep: TOBDProgressStep);
var
  Pct: Integer;
begin
  Pct := Round(AStep.Percent * 100);
  if AStep.Detail <> '' then
    Writeln(Format('  [%d/%d %3d%%] %s — %s',
      [AStep.Index, AStep.Count, Pct, AStep.Name, AStep.Detail]))
  else
    Writeln(Format('  [%d/%d %3d%%] %s',
      [AStep.Index, AStep.Count, Pct, AStep.Name]));
end;

var
  ConnectDone: TEvent;
  ConnectError: string;

procedure HandleConnect(Sender: TObject);
begin
  ConnectDone.SetEvent;
end;

procedure HandleAsyncError(Sender: TObject; ACode: TOBDErrorCode;
  const AMessage: string; var AHandled: Boolean);
begin
  ConnectError := AMessage;
  ConnectDone.SetEvent;
end;

var
  Host: string;
  PortStr: string;
  Port: Integer;
  UseAsync: Boolean;
  I: Integer;

begin
  Host := '192.168.0.10';
  Port := 35000;
  UseAsync := False;
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = '--async') or (ParamStr(I) = '-a') then
      UseAsync := True
    else if Host = '192.168.0.10' then
      Host := ParamStr(I)
    else
    begin
      PortStr := ParamStr(I);
      if not TryStrToInt(PortStr, Port) then
      begin
        Writeln(ErrOutput, 'Invalid port: ', PortStr);
        Halt(2);
      end;
    end;
  end;

  Connection := TOBDConnection.Create(nil);
  ResponseDone := TEvent.Create(nil, True, False, '');
  ConnectDone := TEvent.Create(nil, True, False, '');
  try
    Connection.Transport := otWiFi;
    Connection.WiFiSettings.Host := Host;
    Connection.WiFiSettings.Port := Port;
    Connection.OnDataReceived := HandleData;
    Connection.OnProgress := HandleProgress;

    if UseAsync then
    begin
      Connection.OnConnect := HandleConnect;
      Connection.OnError := HandleAsyncError;
      Writeln(Format('Connecting to %s:%d (async)…', [Host, Port]));
      Connection.OpenAsync;
      // Returned immediately. Pump messages while waiting.
      while ConnectDone.WaitFor(50) <> wrSignaled do
        CheckSynchronize(50);
      if ConnectError <> '' then
      begin
        Writeln(ErrOutput, 'Open failed: ', ConnectError);
        Halt(3);
      end;
    end
    else
    begin
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
    ConnectDone.Free;
    ResponseDone.Free;
    Connection.Free;
  end;
end.
