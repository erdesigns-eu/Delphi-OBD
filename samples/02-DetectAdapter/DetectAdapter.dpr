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
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

program DetectAdapter;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.TypInfo,
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
  ERD.Connection in '..\..\src\Connection\ERD.Connection.pas',
  ERD.Adapter.Types in '..\..\src\Adapter\ERD.Adapter.Types.pas',
  ERD.Adapter.Capabilities in '..\..\src\Adapter\ERD.Adapter.Capabilities.pas',
  ERD.Adapter.Commands in '..\..\src\Adapter\ERD.Adapter.Commands.pas',
  ERD.Adapter.Detection in '..\..\src\Adapter\ERD.Adapter.Detection.pas',
  ERD.Adapter.Init in '..\..\src\Adapter\ERD.Adapter.Init.pas',
  ERD.Adapter in '..\..\src\Adapter\ERD.Adapter.pas';

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
