//------------------------------------------------------------------------------
//  DetectAdapter — sample 02
//
//  Connects to a Wi-Fi adapter, runs TOBDAdapter.Detect, prints the
//  identity (family, version, description, identifier) and the
//  resolved capability set. Demonstrates the dual-method rule by
//  running detection through DetectAsync; the main thread pumps
//  CheckSynchronize while waiting.
//
//  Override host / port via the first two command-line arguments.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 3 initial.
//------------------------------------------------------------------------------

program DetectAdapter;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.TypInfo,
  OBD.Types in '..\..\src\Core\OBD.Types.pas',
  OBD.Connection.Types in '..\..\src\Connection\OBD.Connection.Types.pas',
  OBD.Connection.Settings in '..\..\src\Connection\OBD.Connection.Settings.pas',
  OBD.Connection.Retry in '..\..\src\Connection\OBD.Connection.Retry.pas',
  OBD.Connection.Transport.Base in '..\..\src\Connection\OBD.Connection.Transport.Base.pas',
  OBD.Connection.Mock in '..\..\src\Connection\OBD.Connection.Mock.pas',
  OBD.Connection.Bluetooth in '..\..\src\Connection\OBD.Connection.Bluetooth.pas',
  OBD.Connection.BLE in '..\..\src\Connection\OBD.Connection.BLE.pas',
  OBD.Connection.WiFi in '..\..\src\Connection\OBD.Connection.WiFi.pas',
  OBD.Connection.UDP in '..\..\src\Connection\OBD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  OBD.Connection.Serial in '..\..\src\Connection\OBD.Connection.Serial.pas',
  OBD.Connection.FTDI in '..\..\src\Connection\OBD.Connection.FTDI.pas',
  {$ENDIF}
  OBD.Connection in '..\..\src\Connection\OBD.Connection.pas',
  OBD.Adapter.Types in '..\..\src\Adapter\OBD.Adapter.Types.pas',
  OBD.Adapter.Capabilities in '..\..\src\Adapter\OBD.Adapter.Capabilities.pas',
  OBD.Adapter.Commands in '..\..\src\Adapter\OBD.Adapter.Commands.pas',
  OBD.Adapter.Detection in '..\..\src\Adapter\OBD.Adapter.Detection.pas',
  OBD.Adapter.Init in '..\..\src\Adapter\OBD.Adapter.Init.pas',
  OBD.Adapter in '..\..\src\Adapter\OBD.Adapter.pas';

var
  Connection: TOBDConnection;
  Adapter: TOBDAdapter;
  Done: TEvent;

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

procedure HandleIdentity(Sender: TObject;
  const AIdentity: TOBDAdapterIdentity);
var
  Caps: TOBDAdapterCapabilities;
  Cap: TOBDAdapterCapability;
  CapName: string;
  First: Boolean;
begin
  Writeln;
  Writeln('Identity:');
  Writeln('  Family       : ', GetEnumName(TypeInfo(TOBDAdapterFamily),
    Ord(AIdentity.Family)));
  Writeln('  AdapterKey   : ', AIdentity.AdapterKey);
  Writeln('  DisplayName  : ', AIdentity.DisplayName);
  Writeln('  Firmware     : ', AIdentity.FirmwareVersion);
  Writeln('  Description  : ', AIdentity.Description);
  Writeln('  Identifier   : ', AIdentity.DeviceIdentifier);
  if AIdentity.STInfo <> '' then
    Writeln('  ST info      : ', AIdentity.STInfo);
  Writeln('  Likely clone : ', BoolToStr(AIdentity.IsClone, True));

  Writeln('Capabilities :');
  Caps := Adapter.Capabilities;
  First := True;
  for Cap := Low(TOBDAdapterCapability) to High(TOBDAdapterCapability) do
    if Cap in Caps then
    begin
      CapName := GetEnumName(TypeInfo(TOBDAdapterCapability), Ord(Cap));
      if (Length(CapName) > 2) and (CapName[1] = 'a') and (CapName[2] = 'c') then
        CapName := Copy(CapName, 3, MaxInt);
      if First then
      begin
        Write('  ', CapName);
        First := False;
      end
      else
        Write(', ', CapName);
    end;
  if First then Write('  (none reported)');
  Writeln;
  Done.SetEvent;
end;

procedure HandleError(Sender: TObject; ACode: TOBDErrorCode;
  const AMessage: string; var AHandled: Boolean);
begin
  Writeln(ErrOutput, Format('[error %d] %s', [Ord(ACode), AMessage]));
  Done.SetEvent;
end;

var
  Host: string;
  Port: Integer;
  PortStr: string;
  I: Integer;
begin
  Host := '192.168.0.10';
  Port := 35000;
  for I := 1 to ParamCount do
    if Host = '192.168.0.10' then
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

  Connection := TOBDConnection.Create(nil);
  Adapter := TOBDAdapter.Create(nil);
  Done := TEvent.Create(nil, True, False, '');
  try
    Connection.Transport := otWiFi;
    Connection.WiFiSettings.Host := Host;
    Connection.WiFiSettings.Port := Port;
    Adapter.Connection := Connection;
    Adapter.OnProgress := HandleProgress;
    Adapter.OnIdentityChanged := HandleIdentity;
    Adapter.OnError := HandleError;

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

    Writeln('Detecting adapter (async)…');
    Adapter.DetectAsync;
    while Done.WaitFor(50) <> wrSignaled do
      CheckSynchronize(50);

    Connection.Close;
  finally
    Done.Free;
    Adapter.Free;
    Connection.Free;
  end;
end.
