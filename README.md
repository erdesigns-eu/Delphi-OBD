# Delphi-OBD

Comprehensive Delphi library for OBD-II diagnostics, vehicle protocols, and automotive utilities.

## Repository Structure

The repository is organized into logical folders for better maintainability:

```
├── src/                    # Source code organized by functionality
│   ├── Adapters/          # OBD adapter implementations (ELM327, OBDLink, AT/ST commands)
│   ├── Components/        # Visual components (gauges, displays, LEDs, headers)
│   ├── Connection/        # Connection types (Serial, Bluetooth, WiFi, FTDI, UDP)
│   ├── CustomControls/    # Custom control framework and animations
│   ├── Forms/             # Form templates and base classes
│   ├── Protocol/          # Vehicle protocols (CAN, DoIP, J1939, Legacy)
│   ├── RadioCode/         # Radio code calculators (32+ brands)
│   ├── Services/          # OBD services 01-0A (SAE J1979 compliant)
│   ├── Utilities/         # Logger, string helpers, settings, data modules
│   ├── VIN/               # VIN decoder and utilities
│   └── Wizards/           # IDE wizards for project creation
├── docs/                   # Documentation
│   ├── README.md          # Main documentation
│   ├── TASKS.md           # Development tasks and roadmap
│   └── RADIO_CALCULATORS.md  # Radio code calculator guide
├── examples/               # Example applications
│   ├── minimal/           # Minimal OBD connection example
│   ├── simple/            # Simple diagnostic example
│   ├── advanced/          # Advanced features demo
│   ├── serial/            # Serial connection example
│   ├── bluetooth/         # Bluetooth connection example
│   ├── wifi/              # WiFi connection example
│   ├── ftdi/              # FTDI connection example
│   └── doip/              # DoIP protocol example
├── Packages/               # Delphi package files
└── Resources/              # Images, icons, and other resources
```

## Quick Start

### Installation

1. **Install Skia4Delphi**: First, install Skia4Delphi from GetIt Package Manager (required dependency)
2. **Add to Library Path**: Add the `src` folder and its subfolders to your Delphi library path
3. **Install Packages**: 
   - First, compile and install `Packages\RunTime.dpk`
   - Then, compile and install `Packages\DesignTime.dpk`
   - The packages are configured to output to the standard Delphi BPL directory (`$(BDSCOMMONDIR)\Bpl`)
4. Start using the components and classes in your project

**Note**: The packages require Skia4Delphi to be installed first. The package output directory is set to the standard Delphi BPL folder, ensuring that the Skia DLL is accessible when loading the packages.

### Basic Usage

```delphi
uses
  OBD.Connection,
  OBD.Adapter,
  OBD.Service01;

var
  Connection: TOBDConnection;
  Adapter: TOBDAdapter;
  Service01: TOBDService01;
begin
  // Create connection
  Connection := TOBDConnectionSerial.Create;
  Connection.Connect('COM3', 38400);
  
  // Create adapter
  Adapter := TOBDELM327.Create(Connection);
  
  // Use Service 01
  Service01 := TOBDService01.Create;
  // Request engine RPM, temperature, etc.
end;
```

See the `examples` folder for complete working examples.

## Features

### OBD-II Services (SAE J1979)
- **Service 01**: Live data (PIDs 00-FF)
- **Service 02**: Freeze frame data
- **Service 03**: Stored diagnostic trouble codes
- **Service 04**: Clear DTCs and MIL
- **Service 05**: Oxygen sensor test results
- **Service 06**: On-board monitoring test results
- **Service 07**: Pending trouble codes
- **Service 08**: Control of on-board systems
- **Service 09**: Vehicle information
- **Service 0A**: Permanent trouble codes

### Protocols
- **CAN** (ISO 15765-4)
- **DoIP** (ISO 13400)
- **J1939** (SAE J1939 for heavy-duty vehicles)
- **Legacy** (ISO 9141-2, ISO 14230 KWP2000)

### Adapters
- **ELM327** (complete AT command set)
- **OBDLink** (ST command support: SX, MX, EX models)
- Voltage monitoring
- Connection retry with exponential backoff

### Radio Code Calculators
32 brand-specific radio code calculators:
- **Japanese**: Nissan, Toyota, Honda, Mazda, Mitsubishi, Subaru, Suzuki, Hyundai/Kia
- **European**: Mercedes, BMW, Opel, Volvo, VW, Audi, SEAT, Skoda, Renault, Peugeot, Citroen, Fiat
- **American**: Ford, Chrysler/Jeep/Dodge, GM (Chevrolet, Cadillac, GMC, Buick)
- **Universal**: Becker, Blaupunkt, Alpine, Clarion, Visteon

See `docs/RADIO_CALCULATORS.md` for details.

### Visual Components
- Circular gauges with animations
- Matrix displays
- LED indicators
- Touch-friendly headers and status bars
- Customizable styling with Skia rendering

### Connection Types
- Serial (RS-232)
- Bluetooth
- WiFi/UDP
- FTDI USB
- DoIP over Ethernet

### Utilities
- Production-ready logger (with rotation, severity levels, thread-safe)
- String optimization helpers
- VIN decoder (manufacturer, model year, check digit validation)
- Application settings management

## Documentation

- **[Main Documentation](docs/README.md)** - Comprehensive guide
- **[Radio Calculator Guide](docs/RADIO_CALCULATORS.md)** - Radio code algorithms and usage
- **[Development Tasks](docs/TASKS.md)** - Roadmap and task tracking

## Examples

Browse the `examples` folder for working demonstrations:
- `minimal` - Bare minimum connection setup
- `simple` - Basic diagnostic operations
- `advanced` - Advanced features and protocols
- `serial/bluetooth/wifi/ftdi` - Connection-specific examples
- `doip` - Diagnostic over IP example

## Requirements

- Embarcadero Delphi 11 or higher
- Windows 7, 8/8.1, 10, 11
- Skia4Delphi (for visual components)

## License

Open source under Apache 2.0 License

## Author

Ernst Reidinga (ERDesigns)

## Contributing

Contributions are welcome! Please see `docs/TASKS.md` for current development priorities.

## Support

For issues, questions, or contributions, please visit the GitHub repository.
