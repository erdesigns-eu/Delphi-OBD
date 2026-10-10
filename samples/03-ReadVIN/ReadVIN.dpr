//------------------------------------------------------------------------------
//  ReadVIN — sample 03
//
//  Connects, detects the chip, runs the family init sequence, then
//  asks the ECU for its VIN via OBD-II Service 09 PID 02. The ECU
//  responds with a multi-line answer that the protocol layer
//  reassembles into a 17-character VIN string.
//
//  Demonstrates the full connection → adapter → protocol stack end-to-end:
//    TOBDConnection -> TOBDAdapter -> TOBDProtocol
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

program ReadVIN;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.TypInfo,
  ERD.Types in '..\..\src\Core\ERD.Types.pas',
  ERD.Errors in '..\..\src\Core\ERD.Errors.pas',
  ERD.Decoders in '..\..\src\Core\ERD.Decoders.pas',
  ERD.Catalog in '..\..\src\Core\ERD.Catalog.pas',
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
  ERD.Adapter in '..\..\src\Adapter\ERD.Adapter.pas',
  ERD.Protocol.Types in '..\..\src\Protocol\ERD.Protocol.Types.pas',
  ERD.Protocol.UDS in '..\..\src\Protocol\ERD.Protocol.UDS.pas',
  ERD.Protocol.KWP2000 in '..\..\src\Protocol\ERD.Protocol.KWP2000.pas',
  ERD.Protocol.ISO9141 in '..\..\src\Protocol\ERD.Protocol.ISO9141.pas',
  ERD.Protocol.J1850 in '..\..\src\Protocol\ERD.Protocol.J1850.pas',
  ERD.Protocol.J1939 in '..\..\src\Protocol\ERD.Protocol.J1939.pas',
  ERD.Protocol.ISO15765 in '..\..\src\Protocol\ERD.Protocol.ISO15765.pas',
  ERD.Protocol.VIN in '..\..\src\Protocol\ERD.Protocol.VIN.pas',
  ERD.Protocol in '..\..\src\Protocol\ERD.Protocol.pas';

var
  Connection: TOBDConnection;
  Adapter: TOBDAdapter;
  Protocol: TOBDProtocol;

procedure HandleProgress(Sender: TObject; const AStep: TOBDProgressStep);
begin
  Writeln(Format('  [%d/%d %3d%%] %s%s',
    [AStep.Index, AStep.Count, Round(AStep.Percent * 100), AStep.Name,
     IfThen(AStep.Detail <> '', ' — ' + AStep.Detail, '')]));
end;

function ExtractVIN(const AData: TBytes; out AStrictlyValid: Boolean): string;
begin
  // Lenient extractor finds the rightmost 17-character VIN-shaped
  // substring; strict ISO 3779 validation (alphabet + check digit)
  // runs alongside.
  Result := TOBDVINValidator.ExtractFromOBDResponse(AData);
  AStrictlyValid := (Result <> '') and TOBDVINValidator.IsValid(Result);
end;

var
  Host: string;
  PortStr: string;
  Port: Integer;
  I: Integer;
  Resp: TOBDResponse;
  VIN: string;
  StrictlyValid: Boolean;
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
  Protocol := TOBDProtocol.Create(nil);
  try
    Connection.Transport := otWiFi;
    Connection.WiFiSettings.Host := Host;
    Connection.WiFiSettings.Port := Port;
    Adapter.Connection := Connection;
    Adapter.OnProgress := HandleProgress;
    Protocol.Adapter := Adapter;
    Protocol.OnProgress := HandleProgress;

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

    Writeln('Detecting adapter…');
    Adapter.Detect;
    Writeln(Format('  Adapter: %s, firmware %s',
      [Adapter.Identity.DisplayName, Adapter.Identity.FirmwareVersion]));
    Writeln(Format('  Max ISO-TP frame: %d bytes',
      [Adapter.MaxIsoTpFrameBytes]));

    Writeln('Initialising adapter…');
    Adapter.Init;

    Writeln('Requesting VIN (Service 09 PID 02)…');
    Resp := Protocol.Request($09, TBytes.Create($02), 5000);
    if Resp.IsNegative then
    begin
      Writeln(ErrOutput,
        Format('Negative response: NRC 0x%2.2X (%s)',
          [Resp.NRC, Resp.NRCText]));
      Halt(4);
    end;

    VIN := ExtractVIN(Resp.Data, StrictlyValid);
    if VIN = '' then
      Writeln(ErrOutput, 'No 17-character VIN-shaped substring found in response.')
    else
    begin
      Writeln('VIN: ', VIN);
      if StrictlyValid then
        Writeln('  ISO 3779 valid (alphabet + check digit OK)')
      else
        Writeln('  ISO 3779 check failed — VIN extracted but not validated.');
    end;
    Writeln(Format('Round-trip: %d ms', [Resp.Elapsed]));

    Connection.Close;
  finally
    Protocol.Free;
    Adapter.Free;
    Connection.Free;
  end;
end.
