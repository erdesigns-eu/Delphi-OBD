//------------------------------------------------------------------------------
// PROGRAM        : DiagSessionDemo
// CONTENTS       : Reference console tool that drives the high-level
//                  TOBDDiagSession (v3.11) end-to-end against any
//                  ELM327-compatible adapter on a serial port.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// USAGE          : DiagSessionDemo <COM-port> <baud-rate> [VIN-prefix]
//                  Examples:
//                    DiagSessionDemo COM3 38400 WVW
//                    DiagSessionDemo COM5 115200
//------------------------------------------------------------------------------
program DiagSessionDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.StrUtils,
  OBD.Connection, OBD.Connection.Types, OBD.Connection.Serial,
  OBD.Connection.Async,
  OBD.OEM, OBD.OEM.Session, OBD.OEM.DiagSession,
  // Make every shipped OEM extension self-register so VIN-based
  // routing works without a static dependency on a specific OEM:
  OBD.OEM.VW, OBD.OEM.BMW, OBD.OEM.Mercedes,
  OBD.OEM.Ford, OBD.OEM.GM, OBD.OEM.Stellantis;

procedure Demo(const PortName: string; const BaudRate: Cardinal;
  const VINPrefix: string);
var
  Conn: IOBDConnection;
  Async: TOBDConnectionAsync;
  Params: TOBDConnectionParams;
  Ext: IOBDOEMExtension;
  Session: TOBDDiagSession;
  Vin, Sw, Mileage: string;
begin
  Conn := TOBDConnectionSerial.Create;
  Params := Default(TOBDConnectionParams);
  Params.ConnectionType := ctSerial;
  Params.SerialPort := PortName;
  Params.SerialBaudRate := BaudRate;
  if not Conn.Connect(Params) then
  begin
    Writeln('ERROR: failed to open ', PortName);
    Exit;
  end;
  try
    Async := TOBDConnectionAsync.Create(Conn);
    try
      // The framework picks an OEM extension from a 3-character WMI;
      // the demo accepts a prefix on the command line so it works
      // even before VIN auto-detection runs.
      if VINPrefix <> '' then
        Ext := TOBDOEMRegistry.FindByVIN(VINPrefix + '00000000000000');
      if Ext = nil then
      begin
        Writeln('No OEM extension matched. Defaulting to BMW for the demo.');
        Ext := TOBDOEMRegistry.FindByKey('BMW');
      end;
      if Ext = nil then
      begin
        Writeln('FATAL: no OEM extension registered.');
        Exit;
      end;

      Writeln('OEM extension: ', Ext.DisplayName);
      Writeln('Session negotiator: ', Ext.SessionNegotiator.DisplayName);

      Session := TOBDDiagSession.Create(Async, Ext);
      try
        if not Session.BeginSession(sstExtendedDiagnostic, $7E0) then
        begin
          Writeln('BeginSession FAILED: ', Session.LastError);
          Exit;
        end;
        Writeln('Extended session entered.');

        if Session.ReadDID($F190, Vin) then
          Writeln('VIN              : ', Vin)
        else
          Writeln('VIN read failed: ', Session.LastError);

        if Session.ReadDID($F189, Sw) then
          Writeln('Software version : ', Sw);

        if Session.ReadDID($D050, Mileage) then
          Writeln('Mileage          : ', Mileage);

        Session.EndSession;
        Writeln('Session closed.');
      finally
        Session.Free;
      end;
    finally
      Async.Free;
    end;
  finally
    Conn.Disconnect;
  end;
end;

begin
  if ParamCount < 2 then
  begin
    Writeln('Usage: DiagSessionDemo <COM-port> <baud-rate> [VIN-prefix]');
    Halt(1);
  end;
  try
    Demo(ParamStr(1), StrToInt(ParamStr(2)),
         IfThen(ParamCount >= 3, ParamStr(3), ''));
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(2);
    end;
  end;
end.
