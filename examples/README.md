# Delphi OBD Examples

This directory contains full Delphi projects (DPR + PAS + DFM) that illustrate how to use the non-visual components alongside the Skia-based visuals. Open any DPR in the IDE and press **Run** to explore the wiring.

## Quick Start

1. **Choose an example** based on your connection type or complexity needs
2. **Open the .dpr file** in Delphi IDE (Delphi 11 or higher)
3. **Configure connection settings** (COM port, Bluetooth device, WiFi IP, etc.)
4. **Press F9** to compile and run
5. **Connect to your OBD adapter** and explore the functionality

## Examples Overview

**Total Examples**: 8 (7 connection types + 1 advanced dashboard)

### 📦 minimal/
**Complexity**: Beginner  
**Connection**: Serial (COM port)  
**What it demonstrates**:
- Smallest possible OBD application
- Connect to adapter
- Read stored diagnostic trouble codes (DTCs)
- Clear MIL (Malfunction Indicator Lamp)

**Best for**: Learning the basics, testing adapter connectivity

---

### 📊 simple/
**Complexity**: Beginner  
**Connection**: Serial (COM port)  
**What it demonstrates**:
- Basic dashboard with circular gauge
- Serial transport with default settings
- Real-time RPM display
- Simple connection flow

**Best for**: First dashboard implementation, understanding component bindings

---

### 🔌 serial/
**Complexity**: Intermediate  
**Connection**: Serial (COM port)  
**What it demonstrates**:
- Explicit serial configuration
- Custom baud rate settings (38400, 115200, etc.)
- Custom header captions
- Connection status indicators

**Best for**: Production serial implementations, custom baud rates

---

### 📡 bluetooth/
**Complexity**: Intermediate  
**Connection**: Bluetooth  
**What it demonstrates**:
- Bluetooth adapter pairing
- VCI (Vehicle Communication Interface) status monitoring
- Connection state management
- Wireless diagnostics

**Best for**: Wireless/mobile implementations, Bluetooth adapters

---

### 🌐 wifi/
**Complexity**: Intermediate  
**Connection**: WiFi (TCP/IP)  
**What it demonstrates**:
- WiFi transport configuration
- IP address and port setup
- Reconnect-friendly architecture
- Network-based adapters

**Best for**: WiFi OBD adapters, network diagnostics

---

### 🔧 ftdi/
**Complexity**: Intermediate  
**Connection**: FTDI USB  
**What it demonstrates**:
- FTDI cable configuration
- USB serial communication
- Guarded reconnect calls
- FTDI-specific settings

**Best for**: FTDI-based adapters, USB connections

---

### 🌐 doip/
**Complexity**: Advanced  
**Connection**: UDP/Ethernet (Diagnostics over IP)  
**What it demonstrates**:
- DoIP protocol (ISO 13400) implementation
- UDP connection for automotive Ethernet
- Vehicle discovery and identification
- Routing activation
- UDS diagnostic messages over IP
- BMW ENET cable support
- Modern automotive Ethernet diagnostics

**Best for**: BMW diagnostics, modern vehicles with Ethernet, high-speed diagnostics

**Key Features**:
- Vehicle discovery via broadcast
- Routing activation to specific ECUs
- UDS request/response handling
- Keep-alive and power mode monitoring
- Network-based diagnostics

---

### 🎛️ advanced/
**Complexity**: Advanced  
**Connection**: Multiple (configurable)  
**What it demonstrates**:
- Multi-component dashboard
- Custom data resolvers
- Live PID (Parameter ID) visualization
- Freeze-frame data capture
- VIN (Vehicle Identification Number) display
- DTC (Diagnostic Trouble Code) viewer
- Multiple gauges and indicators
- Advanced binding patterns

**Best for**: Production applications, full-featured diagnostic tools

## Common Configuration Steps

### Serial Connection
```delphi
OBDConnection1.ConnectionType := ctSerial;
OBDConnection1.SerialPort := 'COM3';  // Check Device Manager for your port
OBDConnection1.BaudRate := br38400;   // Common: 38400, 115200, 9600
```

### Bluetooth Connection
```delphi
OBDConnection1.ConnectionType := ctBluetooth;
OBDConnection1.BluetoothDeviceName := 'OBDII';  // Your adapter's Bluetooth name
```

### WiFi Connection
```delphi
OBDConnection1.ConnectionType := ctWifi;
OBDConnection1.WifiHost := '192.168.0.10';  // Your adapter's IP
OBDConnection1.WifiPort := 35000;           // Common: 35000, 23
```

### DoIP/UDP Connection (New in v2.0)
```delphi
uses
  OBD.Connection.UDP, OBD.Protocol.DoIP;
  
var
  Connection: TUDPConnection;
  Protocol: TOBDProtocolDoIP;
begin
  // Create UDP connection for DoIP
  Connection := TUDPConnection.Create('192.168.0.10', 13400);
  
  if Connection.Connect then
  begin
    Protocol := TOBDProtocolDoIP.Create;
    try
      // Set addresses
      Protocol.SourceAddress := $0E00; // Tester
      Protocol.TargetAddress := $0010; // Target ECU
      
      // Activate routing
      Protocol.RoutingActivationRequest;
      
      // Send UDS diagnostic request
      // ... see doip/ example for details
    finally
      Protocol.Free;
    end;
  end;
  
  Connection.Disconnect;
  Connection.Free;
end;
```

## Tips and Best Practices

### Before Running Examples
1. **Ensure your OBD adapter is connected** (via USB, Bluetooth, or WiFi)
2. **Vehicle should be running** (most adapters require engine power)
3. **Check correct port/device name** in Windows Device Manager
4. **Install Skia4Delphi** from GetIt Package Manager if not already installed

### Troubleshooting
- **"Cannot open COM port"**: Check if another application is using the port
- **"Connection timeout"**: Verify adapter is powered and engine is running
- **"No data received"**: Ensure vehicle supports OBD-II (1996+ for US vehicles)
- **Blank gauges**: Check protocol auto-detection or try manual protocol selection

### Performance Tips
- Start with 30 FPS for gauges, increase to 60 only if needed
- Use reasonable update intervals (100-500ms) to avoid overwhelming the adapter
- Disable animations on non-critical components to reduce CPU usage

## Next Steps

After exploring the examples:
1. **Modify an existing example** to match your specific needs
2. **Use the wizards** (File → New → Other → ERDesigns OBD) to scaffold new forms
3. **Read the main documentation** in [README.md](../README.md) for detailed API reference
4. **Check QuickStart.md** for wizard usage and binding patterns

## Requirements

- **Delphi**: Version 11 Alexandria or higher
- **Skia4Delphi**: Install from GetIt Package Manager
- **OBD Adapter**: ELM327-compatible or OBDLink adapter
- **Vehicle**: OBD-II compliant (1996+ for US, 2001+ for EU)

## Support

- **Issues**: [GitHub Issues](https://github.com/erdesigns-eu/Delphi-OBD/issues)
- **Documentation**: [README.md](../README.md)
- **Quick Start**: [QuickStart.md](../QuickStart.md)
