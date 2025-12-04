unit Examples.ServiceDemo;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils,
  Vcl.StdCtrls,
  OBD.Protocol.Types, OBD.Connection.Component, OBD.Protocol.Component,
  OBD.Request.Encoders,
  OBD.Service.Types, OBD.Service01, OBD.Service02, OBD.Service03, OBD.Service04,
  OBD.Service05, OBD.Service06, OBD.Service07, OBD.Service08, OBD.Service09,
  OBD.Service0A;

/// <summary>
///   Helper that issues OBD-II mode 01 through 0A requests using the shared
///   request encoders and parses responses into the service decoders for
///   display inside the example dashboards.
/// </summary>
TOBDServiceDemo = class
private
  FConnection: TOBDConnectionComponent;
  FProtocol: TOBDProtocolComponent;
  FLog: TMemo;
  FService01: TOBDService01;
  FService02: TOBDService02;
  FService03: TOBDService03;
  FService04: TOBDService04;
  FService05: TOBDService05;
  FService06: TOBDService06;
  FService07: TOBDService07;
  FService08: TOBDService08;
  FService09: TOBDService09;
  FService0A: TOBDService0A;
  FEncoder01: TOBDService01RequestEncoder;
  FEncoder02: TOBDService02RequestEncoder;
  FEncoder03: TOBDService03RequestEncoder;
  FEncoder04: TOBDService04RequestEncoder;
  FEncoder05: TOBDService05RequestEncoder;
  FEncoder06: TOBDService06RequestEncoder;
  FEncoder07: TOBDService07RequestEncoder;
  FEncoder08: TOBDService08RequestEncoder;
  FEncoder09: TOBDService09RequestEncoder;
  FEncoder0A: TOBDService0ARequestEncoder;
  FChainedMessages: TReceiveDataMessagesEvent;
  procedure ProtocolMessages(Sender: TObject; const Messages: TArray<IOBDDataMessage>);
  procedure ProcessMessage(const Msg: IOBDDataMessage);
  procedure Log(const Text: string);
  function ConnectionReady: Boolean;
  procedure SendEncoded(const Encoded: string; const Description: string);
public
  /// <summary>
  ///   Constructs the helper with the connection, protocol, and target log
  ///   memo used by the examples.
  /// </summary>
  constructor Create(const AConnection: TOBDConnectionComponent;
    const AProtocol: TOBDProtocolComponent; const ALog: TMemo);
  /// <summary>
  ///   Releases service decoders, request encoders, and detaches from the
  ///   protocol event stream.
  /// </summary>
  destructor Destroy; override;

  /// <summary>
  ///   Subscribes to protocol message delivery while preserving any existing
  ///   handlers so decoding occurs alongside other listeners.
  /// </summary>
  procedure Attach;
  /// <summary>
  ///   Issues a mode 01 request for monitor status and supported PIDs.
  /// </summary>
  procedure RequestMonitorStatus;
  /// <summary>
  ///   Issues a mode 02 freeze-frame snapshot request for the first stored
  ///   frame.
  /// </summary>
  procedure RequestFreezeFrame;
  /// <summary>
  ///   Issues a mode 03 stored DTC request.
  /// </summary>
  procedure RequestStoredDTCs;
  /// <summary>
  ///   Issues a mode 04 clear DTC command.
  /// </summary>
  procedure ClearStoredDTCs;
  /// <summary>
  ///   Issues a mode 05 oxygen sensor monitoring request.
  /// </summary>
  procedure RequestOxygenSensorTests;
  /// <summary>
  ///   Issues a mode 06 onboard monitoring test request.
  /// </summary>
  procedure RequestOnBoardMonitoring;
  /// <summary>
  ///   Issues a mode 07 pending DTC request.
  /// </summary>
  procedure RequestPendingDTCs;
  /// <summary>
  ///   Issues a mode 08 control systems test request.
  /// </summary>
  procedure RequestControlSystemTest;
  /// <summary>
  ///   Issues a mode 09 vehicle information request for the VIN.
  /// </summary>
  procedure RequestVehicleInformation;
  /// <summary>
  ///   Issues a mode 0A permanent DTC request.
  /// </summary>
  procedure RequestPermanentDTCs;
end;

implementation

{ TOBDServiceDemo }

procedure TOBDServiceDemo.Attach;
begin
  FChainedMessages := FProtocol.OnMessages;
  FProtocol.OnMessages := ProtocolMessages;
end;

procedure TOBDServiceDemo.ClearStoredDTCs;
begin
  SendEncoded(FEncoder04.EncodeServiceRequest($00), 'Mode 04: clear stored DTCs');
end;

function TOBDServiceDemo.ConnectionReady: Boolean;
begin
  Result := Assigned(FConnection) and Assigned(FConnection.ConnectionInstance) and FConnection.Connected;
  if not Result then
    Log('Connection not active; connect before sending OBD requests.');
end;

constructor TOBDServiceDemo.Create(const AConnection: TOBDConnectionComponent;
  const AProtocol: TOBDProtocolComponent; const ALog: TMemo);
begin
  inherited Create;
  FConnection := AConnection;
  FProtocol := AProtocol;
  FLog := ALog;
  FService01 := TOBDService01.Create;
  FService02 := TOBDService02.Create;
  FService03 := TOBDService03.Create;
  FService04 := TOBDService04.Create;
  FService05 := TOBDService05.Create;
  FService06 := TOBDService06.Create;
  FService07 := TOBDService07.Create;
  FService08 := TOBDService08.Create;
  FService09 := TOBDService09.Create;
  FService0A := TOBDService0A.Create;
  FEncoder01 := TOBDService01RequestEncoder.Create;
  FEncoder02 := TOBDService02RequestEncoder.Create;
  FEncoder03 := TOBDService03RequestEncoder.Create;
  FEncoder04 := TOBDService04RequestEncoder.Create;
  FEncoder05 := TOBDService05RequestEncoder.Create;
  FEncoder06 := TOBDService06RequestEncoder.Create;
  FEncoder07 := TOBDService07RequestEncoder.Create;
  FEncoder08 := TOBDService08RequestEncoder.Create;
  FEncoder09 := TOBDService09RequestEncoder.Create;
  FEncoder0A := TOBDService0ARequestEncoder.Create;
end;

destructor TOBDServiceDemo.Destroy;
begin
  FProtocol.OnMessages := FChainedMessages;
  FService0A.Free;
  FService09.Free;
  FService08.Free;
  FService07.Free;
  FService06.Free;
  FService05.Free;
  FService04.Free;
  FService03.Free;
  FService02.Free;
  FService01.Free;
  FEncoder0A.Free;
  FEncoder09.Free;
  FEncoder08.Free;
  FEncoder07.Free;
  FEncoder06.Free;
  FEncoder05.Free;
  FEncoder04.Free;
  FEncoder03.Free;
  FEncoder02.Free;
  FEncoder01.Free;
  inherited Destroy;
end;

procedure TOBDServiceDemo.Log(const Text: string);
begin
  if Assigned(FLog) then
    FLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Text);
end;

procedure TOBDServiceDemo.ProcessMessage(const Msg: IOBDDataMessage);
var
  Data: TBytes;
  ServiceId: Byte;
  Codes: TArray<TOBDServiceDiagnosticTroubleCode>;
  Code: TOBDServiceDiagnosticTroubleCode;
begin
  Data := Msg.Data;
  if Length(Data) = 0 then
    Exit;

  ServiceId := Data[0];
  case ServiceId of
    $41:
      begin
        FService01.ParseResponse(Data);
        Log(Format('Mode 01 parsed: MIL=%s, DTCs=%d', [BoolToStr(FService01.MIL, True), FService01.DTC]));
      end;
    $42:
      begin
        FService02.ParseResponse(Data);
        Log(Format('Mode 02 parsed: Freeze-frame DTC=%s', [FService02.StoredFreezeFrameDTC]));
      end;
    $43:
      begin
        FService03.ParseResponse(Data);
        Codes := FService03.DTC;
        if Length(Codes) = 0 then
          Log('Mode 03 parsed: no stored DTCs reported')
        else
          for Code in Codes do
            Log('Mode 03 parsed DTC: ' + Code.DTC);
      end;
    $44:
      begin
        FService04.ParseResponse(Data);
        Log('Mode 04 parsed: clear DTC acknowledged by ECU');
      end;
    $45:
      begin
        FService05.ParseResponse(Data);
        Log('Mode 05 parsed: oxygen sensor test results updated');
      end;
    $46:
      begin
        FService06.ParseResponse(Data);
        Log('Mode 06 parsed: onboard monitor test results updated');
      end;
    $47:
      begin
        FService07.ParseResponse(Data);
        Codes := FService07.DTC;
        if Length(Codes) = 0 then
          Log('Mode 07 parsed: no pending DTCs reported')
        else
          for Code in Codes do
            Log('Mode 07 parsed pending DTC: ' + Code.DTC);
      end;
    $48:
      begin
        FService08.ParseResponse(Data);
        Log('Mode 08 parsed: control system test response received');
      end;
    $49:
      begin
        FService09.ParseResponse(Data);
        Log(Format('Mode 09 parsed: VIN=%s', [FService09.VehicleIdentificationNumber]));
      end;
    $4A:
      begin
        FService0A.ParseResponse(Data);
        Codes := FService0A.DTC;
        if Length(Codes) = 0 then
          Log('Mode 0A parsed: no permanent DTCs reported')
        else
          for Code in Codes do
            Log('Mode 0A parsed permanent DTC: ' + Code.DTC);
      end;
  end;
end;

procedure TOBDServiceDemo.ProtocolMessages(Sender: TObject;
  const Messages: TArray<IOBDDataMessage>);
var
  Msg: IOBDDataMessage;
begin
  for Msg in Messages do
    ProcessMessage(Msg);

  if Assigned(FChainedMessages) then
    FChainedMessages(Sender, Messages);
end;

procedure TOBDServiceDemo.RequestControlSystemTest;
begin
  SendEncoded(FEncoder08.EncodeServiceRequest($00), 'Mode 08: control system test');
end;

procedure TOBDServiceDemo.RequestFreezeFrame;
begin
  SendEncoded(FEncoder02.EncodeServiceRequest($02), 'Mode 02: request freeze-frame data for DTC');
end;

procedure TOBDServiceDemo.RequestMonitorStatus;
begin
  SendEncoded(FEncoder01.EncodeServiceRequest($00), 'Mode 01: supported PIDs');
  SendEncoded(FEncoder01.EncodeServiceRequest($01), 'Mode 01: monitor status since DTCs cleared');
end;

procedure TOBDServiceDemo.RequestOxygenSensorTests;
begin
  SendEncoded(FEncoder05.EncodeServiceRequest($02), 'Mode 05: oxygen sensor monitoring results');
end;

procedure TOBDServiceDemo.RequestOnBoardMonitoring;
begin
  SendEncoded(FEncoder06.EncodeServiceRequest($00), 'Mode 06: onboard monitoring test results');
end;

procedure TOBDServiceDemo.RequestPendingDTCs;
begin
  SendEncoded(FEncoder07.EncodeServiceRequest($00), 'Mode 07: pending DTCs');
end;

procedure TOBDServiceDemo.RequestPermanentDTCs;
begin
  SendEncoded(FEncoder0A.EncodeServiceRequest($00), 'Mode 0A: permanent DTCs');
end;

procedure TOBDServiceDemo.RequestStoredDTCs;
begin
  SendEncoded(FEncoder03.EncodeServiceRequest($00), 'Mode 03: stored DTCs');
end;

procedure TOBDServiceDemo.RequestVehicleInformation;
begin
  SendEncoded(FEncoder09.EncodeServiceRequest($02), 'Mode 09: vehicle identification number');
end;

procedure TOBDServiceDemo.SendEncoded(const Encoded: string; const Description: string);
begin
  if not ConnectionReady then
    Exit;
  if Encoded = '' then
    Exit;
  FConnection.ConnectionInstance.WriteOBDCommand(Encoded);
  Log('Sent ' + Description + ' [' + Encoded + ']');
end;

end.
