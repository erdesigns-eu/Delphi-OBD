# Delphi OBD Examples

This directory contains full Delphi projects (DPR + PAS + DFM) that illustrate how to use the non-visual components alongside the Skia-based visuals. Open any DPR in the IDE and press **Run** to explore the wiring.

## Quick Start

1. **Choose an example** based on your connection type or complexity needs
2. **Open the .dpr file** in Delphi IDE (Delphi 11 or higher)
3. **Configure connection settings** (COM port, Bluetooth device, WiFi IP, etc.)
4. **Press F9** to compile and run
5. **Connect to your OBD adapter** and explore the functionality

## Examples Overview

**Total Examples**: 23 (connection types, dashboards, protocol stacks, OEM/UDS/DoIP tooling, ECU flashing, replay, tachograph)

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

### 🚀 dashboard/
**Complexity**: Intermediate
**Connection**: Simulator by default; live mode via the connection component
**What it demonstrates**:
- Multi-pane diagnostic dashboard built **entirely in code** (no `.dfm`)
- Four `TOBDCircularGauge` instances (RPM, Speed, Coolant Temp, Throttle)
- Three `TOBDLed` indicators (MIL, Connected, Error)
- Touch header + statusbar + log memo
- 20 fps simulator tick that drives gauges with no adapter attached
- "Simulate" / "Connect Live" mode toggle

**Best for**: Reference end-to-end app; reads top-to-bottom as a tutorial.

---

### 📱 mobile_dashboard/
**Complexity**: Intermediate
**Connection**: Simulator (live transports tracked in docs/PROPOSALS.md)
**What it demonstrates**:
- FMX dashboard exercising **every** shipped FMX component
  (LinearGauge, Tachometer, TrendGraph, DtcList, Terminal, Knob,
  SegmentedSwitch, LED)
- Runs on Win32, Win64, macOS, iOS, Android — same source, same
  shared renderers as the VCL components

**Best for**: Mobile / cross-platform diagnostic apps; reference for
the renderer-extract pattern proven across all v3.1 FMX bindings.

---

### ⚙️ ecuflashing_console/
**Complexity**: Intermediate
**Connection**: Simulated ECU
**What it demonstrates**:
- End-to-end `TOBDECUFlashing` pipeline against a fake ECU
- Real RSA-PKCS1-SHA256 / ECDSA-P256 signature verification via
  Windows BCrypt — no external DLLs
- Pre-check, snapshot, signature gate, blocked write, finalise,
  post-flash verify, automatic rollback on failure
- Snapshot persistence to disk

**Best for**: Wiring the flashing pipeline to your own OEM-specific
UDS sequence; shows what the four host-side callbacks need to
return.

---

### 🏷️ oem_demo/
**Complexity**: Beginner
**Connection**: None (registry lookup only)
**What it demonstrates**:
- `IOBDOEMExtension` framework + `TOBDOEMRegistry` lookup by VIN
- Reference VW group + BMW extensions
- DID + RoutineControl catalog inspection
- Per-DID payload decoding with unit conversion

**Best for**: Adding manufacturer-specific UDS coverage to your project.

---

### ⏯️ replay/
**Complexity**: Beginner
**Connection**: None (reads `.obdlog` files)
**What it demonstrates**:
- `TOBDRecorder` / `TOBDReplayer` round-trip
- Console replay with configurable speed multiplier
- File format documentation for capturing real adapter sessions

**Best for**: Building deterministic test fixtures, debugging parsers without a vehicle, recording demo sessions.

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
- Enhanced VIN decoding with check digit validation and plant locations
- DTC (Diagnostic Trouble Code) viewer
- Multiple gauges and indicators
- Advanced binding patterns
- **NEW**: Radio code calculators for 40+ vehicle brands with multi-variant support
- **NEW**: Advanced protocol support (KWP2000, UDS, LIN, FlexRay, MOST)
- **NEW**: J2534 pass-through interface support
- **NEW**: Chinese ELM327 clone detection and quirk management

**Best for**: Production applications, full-featured diagnostic tools

---

## Protocol-Specific Examples

### 🔐 kwp2000/
**Complexity**: Advanced  
**Protocol**: KWP2000 (ISO 14230)  
**What it demonstrates**:
- Keyword Protocol 2000 diagnostic services
- Diagnostic session control (Default, Programming, Extended)
- Security access with seed/key mechanism
- Read/clear diagnostic trouble codes
- ECU identification and programming
- Data transfer for ECU flashing
- Tester present keep-alive

**Best for**: European and Asian vehicles (1990s-2010s), ECU programming

---

### 🔓 uds/
**Complexity**: Advanced  
**Protocol**: UDS (ISO 14229)  
**What it demonstrates**:
- Unified Diagnostic Services (modern standard)
- Complete diagnostic session management
- Multi-level security access (Diagnostic, Programming, Developer, Manufacturer)
- Read DTCs with advanced sub-functions
- Read/write data by identifier
- Memory operations and routine control
- ECU reset and firmware management

**Best for**: Modern vehicles (2010+), advanced diagnostics, firmware updates

---

### 🔌 lin/
**Complexity**: Intermediate  
**Protocol**: LIN (ISO 17987)  
**What it demonstrates**:
- Local Interconnect Network (LIN 1.3, 2.0, 2.1, 2.2A)
- Read/write data by identifier
- Frame ID assignment and node configuration
- Protected identifier with parity calculation
- Classic and enhanced checksum support

**Best for**: Body electronics, sensors, low-speed sub-systems

---

### ⚡ flexray/
**Complexity**: Advanced  
**Protocol**: FlexRay (ISO 17458)  
**What it demonstrates**:
- High-speed deterministic network (2.5-10 Mbps)
- Static and dynamic segment communication
- Dual-channel fault-tolerant operation (A/B channels)
- Cluster configuration
- Cycle-based frame scheduling

**Best for**: Safety-critical systems, X-by-wire, ADAS, high-performance vehicles

---

### 🎵 most/
**Complexity**: Advanced  
**Protocol**: MOST (Media Oriented Systems Transport)  
**What it demonstrates**:
- MOST25/50/150 infotainment network
- Control messages and property get/set
- Function blocks (Audio, Video, Phone, Navigation)
- Streaming channel management

**Best for**: Infotainment systems, audio/video distribution, premium vehicles

---

### 🚛 tachograph/
**Complexity**: Advanced  
**Feature**: Digital Tachograph / Odometer  
**What it demonstrates**:
- EU Gen1/Gen2 Smart Tachograph support
- Support for 8 international standards (EU, Korea, Russia, China, Brazil, Japan, Australia)
- Support for 20+ commercial vehicle makes (Mercedes, Volvo, Scania, MAN, DAF, Freightliner, etc.)
- Auto-detect tachograph manufacturer (VDO, Stoneridge, Denso, etc.)
- Read odometer data (total/trip distance, speed)
- Driver activity recording (28-day history)
- Workshop card authentication
- Odometer calibration and trip reset
- VIN reading and events/faults
- Download tachograph data (DDD format)

**Best for**: Commercial vehicle fleet management, regulatory compliance, odometer verification

---

### 💾 ecuflashing/
**Complexity**: Expert  
**Feature**: ECU Security & Firmware Management  
**What it demonstrates**:
- Multi-level ECU security (Diagnostic, Programming, Developer, Manufacturer)
- Security algorithms (Seed/Key, RSA, AES, HMAC)
- Flash memory operations (erase, write, read, verify)
- Firmware flashing with progress tracking
- Firmware validation and backup
- Programming voltage control
- ECU identification (SW/HW version, serial, part number)

**Best for**: ECU tuning, firmware updates, dealership programming, aftermarket calibration

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

### DoIP/UDP Connection
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
