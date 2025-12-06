# DoIP (Diagnostics over IP) Example

This example demonstrates how to use the **UDP connection** and **DoIP protocol** for automotive Ethernet diagnostics (ISO 13400).

## What is DoIP?

**Diagnostics over IP (DoIP)** is a modern diagnostic protocol that uses **Ethernet/UDP** instead of CAN bus for vehicle diagnostics. It's commonly used in:

- **BMW ENET cables** (Ethernet diagnostic interface)
- **Modern vehicles** (2015+ with Ethernet backbone)
- **Workshop diagnostics** requiring high-speed data transfer
- **Vehicle ECU programming** and updates

DoIP operates over **UDP/IP** on **port 13400** (default) and provides:
- Vehicle discovery and identification
- Routing activation for specific ECUs
- UDS (Unified Diagnostic Services) message transport
- Keep-alive and power mode monitoring

## Key Components

### UDP Connection (`TUDPConnection`)
```delphi
uses
  OBD.Connection.UDP;

var
  Connection: TUDPConnection;
begin
  // Create UDP connection to DoIP gateway
  Connection := TUDPConnection.Create('192.168.0.10', 13400);
  
  // Configure timeouts
  Connection.ReceiveTimeout := 2000; // 2 seconds
  Connection.SendTimeout := 1000;    // 1 second
  
  // Connect to vehicle
  if Connection.Connect then
    ShowMessage('Connected to DoIP gateway!');
end;
```

### DoIP Protocol (`TOBDProtocolDoIP`)
```delphi
uses
  OBD.Protocol.DoIP;

var
  Protocol: TOBDProtocolDoIP;
begin
  Protocol := TOBDProtocolDoIP.Create;
  
  // Set source and target addresses
  Protocol.SourceAddress := $0E00;  // Tester address
  Protocol.TargetAddress := $0010;  // ECU address (example)
  
  // Vehicle discovery
  Protocol.VehicleIdentificationRequest;
  
  // Routing activation (must be done before diagnostics)
  Protocol.RoutingActivationRequest;
end;
```

## Example 1: Vehicle Discovery

```delphi
uses
  OBD.Connection.UDP, OBD.Protocol.DoIP;

procedure TForm1.DiscoverVehicles;
var
  Connection: TUDPConnection;
  Protocol: TOBDProtocolDoIP;
  Response: TBytes;
  VIN: string;
  EID: string;
  GID: string;
begin
  // Create connection (broadcast or specific IP)
  Connection := TUDPConnection.Create('192.168.0.255', 13400); // Broadcast
  
  try
    if Connection.Connect then
    begin
      Protocol := TOBDProtocolDoIP.Create;
      try
        // Send vehicle identification request
        Protocol.VehicleIdentificationRequest;
        
        // Wait for vehicle announcement
        if Connection.ReceiveData(Response, 5000) > 0 then
        begin
          // Parse vehicle announcement
          if Protocol.ParseVehicleAnnouncement(Response, VIN, EID, GID) then
          begin
            Memo1.Lines.Add('Vehicle Found:');
            Memo1.Lines.Add('  VIN: ' + VIN);
            Memo1.Lines.Add('  EID: ' + EID);
            Memo1.Lines.Add('  GID: ' + GID);
          end;
        end;
      finally
        Protocol.Free;
      end;
    end;
  finally
    Connection.Disconnect;
    Connection.Free;
  end;
end;
```

## Example 2: Routing Activation

Before sending diagnostic messages, you must activate routing to the target ECU:

```delphi
procedure TForm1.ActivateRouting;
var
  Connection: TUDPConnection;
  Protocol: TOBDProtocolDoIP;
  Request, Response: TBytes;
  ActivationCode: Byte;
begin
  Connection := TUDPConnection.Create('192.168.0.10', 13400);
  
  try
    if Connection.Connect then
    begin
      Protocol := TOBDProtocolDoIP.Create;
      try
        // Set addresses
        Protocol.SourceAddress := $0E00; // Tester
        Protocol.TargetAddress := $0010; // Target ECU
        
        // Build routing activation request
        Request := Protocol.BuildRoutingActivationRequest($00); // Default activation
        
        // Send request
        if Connection.SendData(Request) then
        begin
          // Wait for response
          if Connection.ReceiveData(Response, 2000) > 0 then
          begin
            // Parse response
            if Protocol.ParseRoutingActivationResponse(Response, ActivationCode) then
            begin
              case ActivationCode of
                $00: Memo1.Lines.Add('Routing activated successfully!');
                $01: Memo1.Lines.Add('Unknown source address');
                $02: Memo1.Lines.Add('All sockets in use');
                $03: Memo1.Lines.Add('Source address already active');
                else Memo1.Lines.Add('Activation failed: ' + IntToHex(ActivationCode, 2));
              end;
            end;
          end;
        end;
      finally
        Protocol.Free;
      end;
    end;
  finally
    Connection.Disconnect;
    Connection.Free;
  end;
end;
```

## Example 3: Send UDS Diagnostic Request

```delphi
procedure TForm1.ReadVIN;
var
  Connection: TUDPConnection;
  Protocol: TOBDProtocolDoIP;
  UDSRequest, DoIPMessage, Response: TBytes;
  VIN: string;
begin
  Connection := TUDPConnection.Create('192.168.0.10', 13400);
  
  try
    if Connection.Connect then
    begin
      Protocol := TOBDProtocolDoIP.Create;
      try
        // Activate routing first (see Example 2)
        ActivateRouting;
        
        // Build UDS request: Read Data By Identifier (Service $22)
        // DID $F190 = VIN
        SetLength(UDSRequest, 3);
        UDSRequest[0] := $22; // Service: Read Data By Identifier
        UDSRequest[1] := $F1; // DID high byte
        UDSRequest[2] := $90; // DID low byte
        
        // Wrap UDS request in DoIP diagnostic message
        DoIPMessage := Protocol.BuildDiagnosticMessage(UDSRequest);
        
        // Send diagnostic message
        if Connection.SendData(DoIPMessage) then
        begin
          // Wait for response
          if Connection.ReceiveData(Response, 2000) > 0 then
          begin
            // Parse DoIP response and extract UDS data
            if Protocol.ParseDiagnosticMessage(Response, UDSRequest) then
            begin
              // Check for positive response ($62 = positive response to $22)
              if (Length(UDSRequest) > 0) and (UDSRequest[0] = $62) then
              begin
                // Extract VIN (17 characters starting at byte 3)
                VIN := '';
                for var I := 3 to Min(19, Length(UDSRequest) - 1) do
                  VIN := VIN + Chr(UDSRequest[I]);
                  
                Memo1.Lines.Add('VIN: ' + VIN);
              end
              else if (Length(UDSRequest) > 0) and (UDSRequest[0] = $7F) then
              begin
                // Negative response
                Memo1.Lines.Add('Error: ' + IntToHex(UDSRequest[2], 2));
              end;
            end;
          end;
        end;
      finally
        Protocol.Free;
      end;
    end;
  finally
    Connection.Disconnect;
    Connection.Free;
  end;
end;
```

## Example 4: Complete DoIP Session

```delphi
type
  TDoIPSession = class
  private
    FConnection: TUDPConnection;
    FProtocol: TOBDProtocolDoIP;
    FConnected: Boolean;
  public
    constructor Create(const AHost: string; APort: Word = 13400);
    destructor Destroy; override;
    
    function Connect: Boolean;
    procedure Disconnect;
    function ActivateRouting(TargetECU: Word): Boolean;
    function SendUDSRequest(const Request: TBytes; var Response: TBytes): Boolean;
    function ReadDTC(var DTCs: TArray<string>): Boolean;
    function ClearDTC: Boolean;
  end;

constructor TDoIPSession.Create(const AHost: string; APort: Word);
begin
  inherited Create;
  FConnection := TUDPConnection.Create(AHost, APort);
  FProtocol := TOBDProtocolDoIP.Create;
  FConnected := False;
end;

destructor TDoIPSession.Destroy;
begin
  Disconnect;
  FProtocol.Free;
  FConnection.Free;
  inherited;
end;

function TDoIPSession.Connect: Boolean;
begin
  Result := FConnection.Connect;
  FConnected := Result;
end;

procedure TDoIPSession.Disconnect;
begin
  if FConnected then
  begin
    FConnection.Disconnect;
    FConnected := False;
  end;
end;

function TDoIPSession.ActivateRouting(TargetECU: Word): Boolean;
var
  Request, Response: TBytes;
  ActivationCode: Byte;
begin
  Result := False;
  
  FProtocol.SourceAddress := $0E00;
  FProtocol.TargetAddress := TargetECU;
  
  Request := FProtocol.BuildRoutingActivationRequest($00);
  
  if FConnection.SendData(Request) then
  begin
    if FConnection.ReceiveData(Response, 2000) > 0 then
    begin
      if FProtocol.ParseRoutingActivationResponse(Response, ActivationCode) then
        Result := (ActivationCode = $00);
    end;
  end;
end;

function TDoIPSession.SendUDSRequest(const Request: TBytes; var Response: TBytes): Boolean;
var
  DoIPMessage, RawResponse: TBytes;
begin
  Result := False;
  
  DoIPMessage := FProtocol.BuildDiagnosticMessage(Request);
  
  if FConnection.SendData(DoIPMessage) then
  begin
    if FConnection.ReceiveData(RawResponse, 2000) > 0 then
      Result := FProtocol.ParseDiagnosticMessage(RawResponse, Response);
  end;
end;

// Usage:
procedure TForm1.FullDiagnosticSession;
var
  Session: TDoIPSession;
  DTCs: TArray<string>;
  DTC: string;
begin
  Session := TDoIPSession.Create('192.168.0.10');
  try
    // Connect
    if Session.Connect then
    begin
      Memo1.Lines.Add('Connected to DoIP gateway');
      
      // Activate routing to engine ECU
      if Session.ActivateRouting($0010) then
      begin
        Memo1.Lines.Add('Routing activated');
        
        // Read diagnostic trouble codes
        if Session.ReadDTC(DTCs) then
        begin
          Memo1.Lines.Add('Found ' + IntToStr(Length(DTCs)) + ' DTCs:');
          for DTC in DTCs do
            Memo1.Lines.Add('  ' + DTC);
        end;
      end;
    end;
  finally
    Session.Free;
  end;
end;
```

## Configuration Guide

### Network Setup

1. **Connect ENET cable** to vehicle OBD port
2. **Connect Ethernet** to your PC
3. **Configure network adapter**:
   - IP Address: `192.168.0.100` (or auto-DHCP)
   - Subnet Mask: `255.255.255.0`
   - Gateway: `192.168.0.1` (if needed)

### Common IP Addresses

| Device | IP Address | Port |
|--------|-----------|------|
| BMW ENET | 192.168.0.10 | 13400 |
| Generic DoIP | 169.254.x.x (Auto-IP) | 13400 |
| Broadcast | 192.168.0.255 | 13400 |

### Firewall Configuration

Ensure Windows Firewall allows:
- **Outbound UDP** on port 13400
- **Inbound UDP** on port 13400
- Or add application exception for your diagnostic tool

## DoIP Message Structure

### Generic Header (8 bytes)
```
Byte 0:    Protocol Version (0x02)
Byte 1:    Inverse Protocol Version (0xFD)
Bytes 2-3: Payload Type (e.g., 0x8001 for diagnostic message)
Bytes 4-7: Payload Length (32-bit big-endian)
```

### Diagnostic Message Payload
```
Bytes 0-1: Source Address (e.g., 0x0E00 for tester)
Bytes 2-3: Target Address (e.g., 0x0010 for ECU)
Bytes 4+:  UDS data (service + parameters)
```

## Troubleshooting

### Cannot Connect
- Check Ethernet cable connection
- Verify IP address configuration
- Ensure vehicle is powered on (ignition on)
- Try broadcast discovery first (`192.168.0.255`)

### Routing Activation Failed
- Verify correct target ECU address
- Ensure no other diagnostic tool is connected
- Check vehicle allows diagnostic access (some vehicles require unlock)

### No Response to Diagnostic Requests
- Ensure routing is activated first
- Verify target ECU address is correct
- Check UDS service is supported by ECU
- Increase timeout values

### Connection Drops
- Implement keep-alive (alive check request/response)
- Monitor power mode status
- Handle routing deactivation gracefully

## Performance Considerations

- **UDP is connectionless** - no TCP overhead
- **Typical latency**: 10-50ms per request/response
- **Max throughput**: ~10-100 diagnostic messages/second
- **Recommended polling rate**: 100-500ms for real-time data

## Security Notes

- DoIP may require **authentication** (vehicle-specific)
- Some services require **security access** (seed/key)
- **Workshop access** may need special credentials
- Always follow manufacturer diagnostic procedures

## Related Standards

- **ISO 13400**: Road vehicles — Diagnostic communication over Internet Protocol (DoIP)
- **ISO 14229**: Unified diagnostic services (UDS)
- **ISO 15765**: Diagnostic communication over CAN (DoCAN)

## Additional Resources

- [Main README](../../README.md) - Full API documentation
- [Examples Overview](../README.md) - All examples
- [OBD.Connection.UDP.pas](../../OBD.Connection.UDP.pas) - UDP connection implementation
- [OBD.Protocol.DoIP.pas](../../OBD.Protocol.DoIP.pas) - DoIP protocol implementation
