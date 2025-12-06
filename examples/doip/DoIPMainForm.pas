unit DoIPMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  
  OBD.Connection.UDP, OBD.Protocol.DoIP;

type
  TFormDoIPMain = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    GroupBoxConnection: TGroupBox;
    LabelHost: TLabel;
    EditHost: TEdit;
    LabelPort: TLabel;
    EditPort: TEdit;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    GroupBoxDiscovery: TGroupBox;
    ButtonDiscover: TButton;
    GroupBoxRouting: TGroupBox;
    LabelSourceAddr: TLabel;
    EditSourceAddr: TEdit;
    LabelTargetAddr: TLabel;
    EditTargetAddr: TEdit;
    ButtonActivateRouting: TButton;
    GroupBoxDiagnostics: TGroupBox;
    ButtonReadVIN: TButton;
    ButtonReadDTC: TButton;
    ButtonClearDTC: TButton;
    MemoLog: TMemo;
    StatusBar: TStatusBar;
    ButtonReadRPM: TButton;
    ButtonReadSpeed: TButton;
    LabelInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonDiscoverClick(Sender: TObject);
    procedure ButtonActivateRoutingClick(Sender: TObject);
    procedure ButtonReadVINClick(Sender: TObject);
    procedure ButtonReadDTCClick(Sender: TObject);
    procedure ButtonClearDTCClick(Sender: TObject);
    procedure ButtonReadRPMClick(Sender: TObject);
    procedure ButtonReadSpeedClick(Sender: TObject);
  private
    FConnection: TUDPConnection;
    FProtocol: TOBDProtocolDoIP;
    FConnected: Boolean;
    FRoutingActive: Boolean;
    
    procedure Log(const Msg: string);
    procedure UpdateUI;
    function SendUDSRequest(const Request: TBytes; var Response: TBytes; Timeout: Integer = 2000): Boolean;
  public
    { Public declarations }
  end;

var
  FormDoIPMain: TFormDoIPMain;

implementation

{$R *.dfm}

procedure TFormDoIPMain.FormCreate(Sender: TObject);
begin
  // Initialize default values
  EditHost.Text := '192.168.0.10';
  EditPort.Text := '13400';
  EditSourceAddr.Text := '0E00';
  EditTargetAddr.Text := '0010';
  
  FConnection := nil;
  FProtocol := nil;
  FConnected := False;
  FRoutingActive := False;
  
  UpdateUI;
  
  Log('DoIP Example Application Started');
  Log('Configure connection settings and click Connect');
  Log('');
  Log('Typical settings:');
  Log('  BMW ENET: 192.168.0.10:13400');
  Log('  Source: 0E00 (Tester)');
  Log('  Target: 0010 (Engine ECU) or specific ECU address');
end;

procedure TFormDoIPMain.FormDestroy(Sender: TObject);
begin
  if FConnected then
    ButtonDisconnectClick(nil);
    
  if Assigned(FProtocol) then
    FProtocol.Free;
    
  if Assigned(FConnection) then
    FConnection.Free;
end;

procedure TFormDoIPMain.ButtonConnectClick(Sender: TObject);
var
  Host: string;
  Port: Word;
begin
  Host := Trim(EditHost.Text);
  Port := StrToIntDef(EditPort.Text, 13400);
  
  if Host = '' then
  begin
    ShowMessage('Please enter a host IP address');
    Exit;
  end;
  
  Log('Connecting to ' + Host + ':' + IntToStr(Port) + '...');
  
  try
    // Create connection
    FConnection := TUDPConnection.Create(Host, Port);
    FConnection.ReceiveTimeout := 2000;
    FConnection.SendTimeout := 1000;
    
    // Create protocol
    FProtocol := TOBDProtocolDoIP.Create;
    
    // Try to connect
    if FConnection.Connect then
    begin
      FConnected := True;
      Log('Connected successfully!');
      StatusBar.SimpleText := 'Connected to ' + Host + ':' + IntToStr(Port);
    end
    else
    begin
      Log('Connection failed!');
      FConnection.Free;
      FProtocol.Free;
      FConnection := nil;
      FProtocol := nil;
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      if Assigned(FConnection) then
        FConnection.Free;
      if Assigned(FProtocol) then
        FProtocol.Free;
      FConnection := nil;
      FProtocol := nil;
    end;
  end;
  
  UpdateUI;
end;

procedure TFormDoIPMain.ButtonDisconnectClick(Sender: TObject);
begin
  if FConnected then
  begin
    Log('Disconnecting...');
    
    FConnection.Disconnect;
    FConnection.Free;
    FProtocol.Free;
    
    FConnection := nil;
    FProtocol := nil;
    FConnected := False;
    FRoutingActive := False;
    
    Log('Disconnected');
    StatusBar.SimpleText := 'Disconnected';
  end;
  
  UpdateUI;
end;

procedure TFormDoIPMain.ButtonDiscoverClick(Sender: TObject);
var
  Request, Response: TBytes;
  BytesReceived: Integer;
begin
  if not FConnected then
  begin
    ShowMessage('Please connect first');
    Exit;
  end;
  
  Log('Sending vehicle identification request...');
  
  try
    // Build vehicle identification request
    SetLength(Request, 8);
    Request[0] := $02; // Protocol version
    Request[1] := $FD; // Inverse protocol version
    Request[2] := $00; // Payload type high
    Request[3] := $01; // Payload type low (0x0001 = Vehicle ID request)
    Request[4] := $00; // Payload length
    Request[5] := $00;
    Request[6] := $00;
    Request[7] := $00;
    
    // Send request
    if FConnection.SendData(Request) then
    begin
      Log('Request sent, waiting for response...');
      
      // Wait for response
      SetLength(Response, 8192);
      BytesReceived := FConnection.ReceiveData(Response, 5000);
      
      if BytesReceived > 0 then
      begin
        SetLength(Response, BytesReceived);
        Log('Received ' + IntToStr(BytesReceived) + ' bytes');
        
        // Parse response (simplified)
        if (BytesReceived >= 8) and (Response[0] = $02) then
        begin
          var PayloadType := (Response[2] shl 8) or Response[3];
          
          if PayloadType = $0004 then
          begin
            Log('Vehicle announcement received!');
            
            // Extract VIN if present (typically at offset 8, 17 bytes)
            if BytesReceived >= 25 then
            begin
              var VIN := '';
              for var I := 8 to 24 do
                VIN := VIN + Chr(Response[I]);
              Log('VIN: ' + VIN);
            end;
          end
          else
            Log('Payload type: 0x' + IntToHex(PayloadType, 4));
        end;
      end
      else
        Log('No response received (timeout)');
    end
    else
      Log('Failed to send request');
      
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TFormDoIPMain.ButtonActivateRoutingClick(Sender: TObject);
var
  Request, Response: TBytes;
  SourceAddr, TargetAddr: Word;
  BytesReceived: Integer;
begin
  if not FConnected then
  begin
    ShowMessage('Please connect first');
    Exit;
  end;
  
  // Parse addresses
  SourceAddr := StrToIntDef('$' + EditSourceAddr.Text, $0E00);
  TargetAddr := StrToIntDef('$' + EditTargetAddr.Text, $0010);
  
  FProtocol.SourceAddress := SourceAddr;
  FProtocol.TargetAddress := TargetAddr;
  
  Log('Activating routing...');
  Log('Source: 0x' + IntToHex(SourceAddr, 4));
  Log('Target: 0x' + IntToHex(TargetAddr, 4));
  
  try
    // Build routing activation request (simplified)
    SetLength(Request, 15);
    Request[0] := $02;    // Protocol version
    Request[1] := $FD;    // Inverse protocol version
    Request[2] := $00;    // Payload type high
    Request[3] := $05;    // Payload type low (0x0005 = Routing activation)
    Request[4] := $00;    // Payload length
    Request[5] := $00;
    Request[6] := $00;
    Request[7] := $07;    // 7 bytes payload
    Request[8] := Hi(SourceAddr);   // Source address high
    Request[9] := Lo(SourceAddr);   // Source address low
    Request[10] := $00;   // Activation type
    Request[11] := $00;   // Reserved
    Request[12] := $00;
    Request[13] := $00;
    Request[14] := $00;
    
    // Send request
    if FConnection.SendData(Request) then
    begin
      Log('Request sent, waiting for response...');
      
      // Wait for response
      SetLength(Response, 8192);
      BytesReceived := FConnection.ReceiveData(Response, 2000);
      
      if BytesReceived > 0 then
      begin
        SetLength(Response, BytesReceived);
        Log('Received ' + IntToStr(BytesReceived) + ' bytes');
        
        // Parse response
        if (BytesReceived >= 13) and (Response[0] = $02) then
        begin
          var PayloadType := (Response[2] shl 8) or Response[3];
          
          if PayloadType = $0006 then
          begin
            var ResponseCode := Response[10];
            
            case ResponseCode of
              $00: 
              begin
                Log('✓ Routing activated successfully!');
                FRoutingActive := True;
              end;
              $01: Log('✗ Unknown source address');
              $02: Log('✗ All sockets in use');
              $03: Log('✗ Source address already active');
              $04: Log('✗ Authentication missing');
              $05: Log('✗ Confirmation rejected');
              else Log('✗ Activation failed: 0x' + IntToHex(ResponseCode, 2));
            end;
          end
          else
            Log('Unexpected payload type: 0x' + IntToHex(PayloadType, 4));
        end;
      end
      else
        Log('No response received (timeout)');
    end
    else
      Log('Failed to send request');
      
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
  
  UpdateUI;
end;

procedure TFormDoIPMain.ButtonReadVINClick(Sender: TObject);
var
  Request, Response: TBytes;
  VIN: string;
begin
  if not FRoutingActive then
  begin
    ShowMessage('Please activate routing first');
    Exit;
  end;
  
  Log('Reading VIN...');
  
  // UDS request: Read Data By Identifier - VIN (DID 0xF190)
  SetLength(Request, 3);
  Request[0] := $22;  // Service: Read Data By Identifier
  Request[1] := $F1;  // DID high byte
  Request[2] := $90;  // DID low byte
  
  if SendUDSRequest(Request, Response) then
  begin
    // Check for positive response ($62 = positive response to $22)
    if (Length(Response) > 0) and (Response[0] = $62) then
    begin
      // Extract VIN (17 characters starting at byte 3)
      VIN := '';
      for var I := 3 to Min(19, Length(Response) - 1) do
        VIN := VIN + Chr(Response[I]);
        
      Log('✓ VIN: ' + VIN);
    end
    else if (Length(Response) > 0) and (Response[0] = $7F) then
    begin
      // Negative response
      if Length(Response) >= 3 then
        Log('✗ Error code: 0x' + IntToHex(Response[2], 2))
      else
        Log('✗ Negative response received');
    end
    else
      Log('✗ Unexpected response');
  end;
end;

procedure TFormDoIPMain.ButtonReadDTCClick(Sender: TObject);
var
  Request, Response: TBytes;
  NumDTCs, I: Integer;
  DTC: string;
begin
  if not FRoutingActive then
  begin
    ShowMessage('Please activate routing first');
    Exit;
  end;
  
  Log('Reading DTCs...');
  
  // UDS request: Read DTC Information - Report DTC by status mask
  SetLength(Request, 3);
  Request[0] := $19;  // Service: Read DTC Information
  Request[1] := $02;  // Sub-function: Report DTC by status mask
  Request[2] := $08;  // Status mask: confirmed DTCs
  
  if SendUDSRequest(Request) then
  begin
    if (Length(Response) > 0) and (Response[0] = $59) then
    begin
      // Positive response
      if Length(Response) >= 6 then
      begin
        NumDTCs := (Length(Response) - 3) div 4;
        Log('✓ Found ' + IntToStr(NumDTCs) + ' DTC(s)');
        
        for I := 0 to NumDTCs - 1 do
        begin
          var Offset := 3 + (I * 4);
          if Offset + 3 <= Length(Response) then
          begin
            // Format DTC code
            var B1 := Response[Offset];
            var B2 := Response[Offset + 1];
            var B3 := Response[Offset + 2];
            
            DTC := '';
            case (B1 shr 6) and $03 of
              0: DTC := 'P'; // Powertrain
              1: DTC := 'C'; // Chassis
              2: DTC := 'B'; // Body
              3: DTC := 'U'; // Network
            end;
            
            DTC := DTC + IntToHex((B1 shr 4) and $03, 1);
            DTC := DTC + IntToHex(B1 and $0F, 1);
            DTC := DTC + IntToHex(B2, 2);
            
            Log('  ' + DTC + ' (Status: 0x' + IntToHex(B3, 2) + ')');
          end;
        end;
      end
      else
        Log('✓ No DTCs found');
    end
    else if (Length(Response) > 0) and (Response[0] = $7F) then
      Log('✗ Error reading DTCs')
    else
      Log('✗ Unexpected response');
  end;
end;

procedure TFormDoIPMain.ButtonClearDTCClick(Sender: TObject);
var
  Request, Response: TBytes;
begin
  if not FRoutingActive then
  begin
    ShowMessage('Please activate routing first');
    Exit;
  end;
  
  if MessageDlg('Clear all diagnostic trouble codes?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
    
  Log('Clearing DTCs...');
  
  // UDS request: Clear Diagnostic Information
  SetLength(Request, 4);
  Request[0] := $14;      // Service: Clear Diagnostic Information
  Request[1] := $FF;      // Group of DTC (0xFFFFFF = all)
  Request[2] := $FF;
  Request[3] := $FF;
  
  if SendUDSRequest(Request, Response) then
  begin
    if (Length(Response) > 0) and (Response[0] = $54) then
      Log('✓ DTCs cleared successfully')
    else if (Length(Response) > 0) and (Response[0] = $7F) then
      Log('✗ Failed to clear DTCs')
    else
      Log('✗ Unexpected response');
  end;
end;

procedure TFormDoIPMain.ButtonReadRPMClick(Sender: TObject);
var
  Request, Response: TBytes;
  RPM: Word;
begin
  if not FRoutingActive then
  begin
    ShowMessage('Please activate routing first');
    Exit;
  end;
  
  // UDS request: Read Current Data - Engine RPM (PID 0x0C)
  SetLength(Request, 2);
  Request[0] := $01;  // Service: Show current data
  Request[1] := $0C;  // PID: Engine RPM
  
  if SendUDSRequest(Request, Response) then
  begin
    if (Length(Response) >= 4) and (Response[0] = $41) then
    begin
      // Calculate RPM: ((A * 256) + B) / 4
      RPM := ((Response[2] * 256) + Response[3]) div 4;
      Log('✓ Engine RPM: ' + IntToStr(RPM) + ' rpm');
    end
    else
      Log('✗ Failed to read RPM');
  end;
end;

procedure TFormDoIPMain.ButtonReadSpeedClick(Sender: TObject);
var
  Request, Response: TBytes;
  Speed: Byte;
begin
  if not FRoutingActive then
  begin
    ShowMessage('Please activate routing first');
    Exit;
  end;
  
  // UDS request: Read Current Data - Vehicle Speed (PID 0x0D)
  SetLength(Request, 2);
  Request[0] := $01;  // Service: Show current data
  Request[1] := $0D;  // PID: Vehicle speed
  
  if SendUDSRequest(Request, Response) then
  begin
    if (Length(Response) >= 3) and (Response[0] = $41) then
    begin
      Speed := Response[2];
      Log('✓ Vehicle Speed: ' + IntToStr(Speed) + ' km/h');
    end
    else
      Log('✗ Failed to read speed');
  end;
end;

function TFormDoIPMain.SendUDSRequest(const Request: TBytes; var Response: TBytes; Timeout: Integer): Boolean;
var
  DoIPMessage, RawResponse: TBytes;
  PayloadLength: Cardinal;
  BytesReceived: Integer;
begin
  Result := False;
  
  try
    // Build DoIP diagnostic message
    PayloadLength := Length(Request) + 4; // 4 bytes for addresses
    SetLength(DoIPMessage, 8 + PayloadLength);
    
    // DoIP header
    DoIPMessage[0] := $02;    // Protocol version
    DoIPMessage[1] := $FD;    // Inverse protocol version
    DoIPMessage[2] := $80;    // Payload type high (0x8001 = diagnostic message)
    DoIPMessage[3] := $01;    // Payload type low
    DoIPMessage[4] := (PayloadLength shr 24) and $FF;  // Payload length
    DoIPMessage[5] := (PayloadLength shr 16) and $FF;
    DoIPMessage[6] := (PayloadLength shr 8) and $FF;
    DoIPMessage[7] := PayloadLength and $FF;
    
    // Source and target addresses
    DoIPMessage[8] := Hi(FProtocol.SourceAddress);
    DoIPMessage[9] := Lo(FProtocol.SourceAddress);
    DoIPMessage[10] := Hi(FProtocol.TargetAddress);
    DoIPMessage[11] := Lo(FProtocol.TargetAddress);
    
    // UDS payload
    Move(Request[0], DoIPMessage[12], Length(Request));
    
    // Send request
    if FConnection.SendData(DoIPMessage) then
    begin
      // Wait for response
      SetLength(RawResponse, 8192);
      BytesReceived := FConnection.ReceiveData(RawResponse, Timeout);
      
      if BytesReceived > 0 then
      begin
        SetLength(RawResponse, BytesReceived);
        
        // Parse DoIP response
        if (BytesReceived >= 12) and (RawResponse[0] = $02) then
        begin
          var PayloadType := (RawResponse[2] shl 8) or RawResponse[3];
          
          if PayloadType = $8001 then
          begin
            // Extract UDS data (skip 12 byte DoIP header)
            var UDSLength := BytesReceived - 12;
            SetLength(Response, UDSLength);
            Move(RawResponse[12], Response[0], UDSLength);
            Result := True;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TFormDoIPMain.Log(const Msg: string);
begin
  MemoLog.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + Msg);
  
  // Auto-scroll to bottom
  SendMessage(MemoLog.Handle, EM_LINESCROLL, 0, MemoLog.Lines.Count);
end;

procedure TFormDoIPMain.UpdateUI;
begin
  ButtonConnect.Enabled := not FConnected;
  ButtonDisconnect.Enabled := FConnected;
  
  GroupBoxDiscovery.Enabled := FConnected;
  GroupBoxRouting.Enabled := FConnected;
  GroupBoxDiagnostics.Enabled := FRoutingActive;
  
  if FConnected then
    LabelInfo.Caption := 'Status: Connected'
  else if FRoutingActive then
    LabelInfo.Caption := 'Status: Routing Active'
  else
    LabelInfo.Caption := 'Status: Disconnected';
end;

end.
