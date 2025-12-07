# Delphi-OBD

**Professional OBD-II Diagnostics Library for Delphi**

A comprehensive, high-performance OBD-II diagnostics suite with beautiful Skia-rendered UI components. Build professional automotive diagnostic applications with minimal code!

![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)
![Delphi](https://img.shields.io/badge/Delphi-11%2B-red.svg)
![Platform](https://img.shields.io/badge/platform-Windows-lightgrey.svg)

---

## ✨ Features

### 🎨 Beautiful UI Components
- **Circular Gauges** - Animated, customizable gauges with rich styling
- **LED Indicators** - Multi-state LED displays with smooth transitions
- **Matrix Displays** - Scrolling text displays for diagnostic codes
- **Touch Controls** - Modern touch-friendly headers, status bars, and navigation
- **All powered by Skia4Delphi** - High-performance, GPU-accelerated rendering

### 🔌 Multiple Connection Types
- **Serial/COM** - Traditional RS232/USB adapters
- **Bluetooth** - Wireless OBD adapters
- **WiFi** - Network-based adapters
- **FTDI** - FTDI chip-based interfaces

### 🚗 OBD Protocol Support
- **ELM327** - Industry-standard protocol
- **OBDLink** - Advanced OBDLink adapters
- **Multiple protocols** - CAN, ISO 9141, KWP2000, J1850
- **Auto-detection** - Automatic adapter and protocol detection

### 📊 OBD Services
- **Service 01** - Real-time data (RPM, speed, temperature, etc.)
- **Service 02** - Freeze frame data
- **Service 03** - Diagnostic trouble codes (DTCs)
- **Service 05** - Oxygen sensor monitoring
- **Service 07** - Pending trouble codes
- **Service 09** - Vehicle information (VIN, calibration ID, etc.)

### 🎬 Smooth Animations
- 25+ easing functions (bounce, elastic, sine, etc.)
- Frame-independent animations
- Optimized timer system
- Zero-copy Skia rendering

---

## 🚀 Quick Start (5 Minutes)

### Prerequisites
1. **Delphi 11 or higher** (RAD Studio 11 Alexandria+)
2. **Skia4Delphi** - Install from GetIt Package Manager or [GitHub](https://github.com/skia4delphi/skia4delphi)

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/erdesigns-eu/Delphi-OBD.git
cd Delphi-OBD
```

2. **Add library path to Delphi**
   - Tools → Options → Language → Delphi → Library
   - Add `Delphi-OBD` folder to Library Path

3. **Install packages**
   - Open `Packages/RunTime.dpk` and compile
   - Open `Packages/DesignTime.dpk` and install

4. **Run an example**
   - Open `examples/simple/SimpleDashboard.dpr`
   - Press F9 to run

---

## 📖 Documentation

### Table of Contents
- [Quick Start Guide](#-quick-start-5-minutes)
- [Component Overview](#-component-overview)
- [Usage Examples](#-usage-examples)
- [Architecture](#-architecture)
- [Examples Guide](#-examples)
- [API Reference](#-api-reference)
- [Performance](#-performance)
- [Troubleshooting](#-troubleshooting)
- [Contributing](#-contributing)

---

## 🧩 Component Overview

### Visual Components (Skia-Rendered)

#### TOBDCircularGauge
High-performance circular gauge with animations.

**Key Properties:**
- `Min`, `Max`, `Value` - Range and current value
- `StartAngle`, `EndAngle` - Gauge arc definition
- `Animation` - Smooth value transitions with easing
- `Scale` - Customizable tick marks and labels
- `Needle` - Style, color, and dimensions

**Features:**
- 60 FPS smooth animations
- Cached background rendering
- GPU-accelerated via Skia
- Thread-safe value updates

#### TOBDLED
Multi-state LED indicator with gradients.

**States:** Grayed, Off, On
**Features:**
- Smooth color transitions
- Border and glow effects
- Cached rendering for performance

#### TOBDMatrixDisplay
Scrolling text display for codes and messages.

**Features:**
- Multiple scroll modes (left, right, up, down, invert)
- Customizable cell size and spacing
- Background gradients
- Efficient character caching

#### TOBDTouchHeader
Modern touch-friendly header with tabs and buttons.

**Features:**
- Gradient backgrounds
- Tab navigation
- Back/Action buttons
- Battery indicator
- Caption and image support

#### TOBDTouchStatusbar / TOBDTouchSubheader
Status indicators and information panels.

**Features:**
- Multiple panels
- LED indicators per panel
- Size grip for resizing
- Connection status indicators

---

## 💡 Usage Examples

### Example 1: Simple Gauge Dashboard

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configure circular gauge
  CircularGauge1.Min := 0;
  CircularGauge1.Max := 8000;
  CircularGauge1.Value := 0;
  
  // Enable smooth animations
  CircularGauge1.Animation.Enabled := True;
  CircularGauge1.Animation.Duration := 500; // milliseconds
  CircularGauge1.Animation.&Type := anSineEaseOut;
end;

procedure TForm1.UpdateRPM(RPM: Integer);
begin
  // Value changes are automatically animated
  CircularGauge1.Value := RPM;
end;
```

### Example 2: Serial Connection

```delphi
procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  // Configure connection
  OBDConnection1.ConnectionType := ctSerial;
  OBDConnection1.SerialPort := 'COM3';
  OBDConnection1.BaudRate := br38400;
  
  // Connect
  if OBDConnection1.Connect then
  begin
    // Auto-detect protocol
    OBDProtocol1.AutoDetect;
    
    // Start reading data
    Timer1.Enabled := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  RPM: Integer;
begin
  // Read RPM (Service 01, PID 0C)
  if OBDProtocol1.ReadParameter($01, $0C, RPM) then
    CircularGauge1.Value := RPM;
end;
```

### Example 3: Read Diagnostic Codes

```delphi
procedure TForm1.ReadCodesButtonClick(Sender: TObject);
var
  Codes: TArray<string>;
  Code: string;
begin
  // Read DTCs (Service 03)
  Codes := OBDProtocol1.ReadTroubleCodes;
  
  // Display in matrix display
  MatrixDisplay1.Clear;
  for Code in Codes do
    MatrixDisplay1.AddText(Code);
    
  // Start scrolling
  MatrixDisplay1.StartAnimation(atScrollLeft);
end;
```

### Example 4: Bluetooth Connection

```delphi
procedure TForm1.ConnectBluetooth;
begin
  OBDConnection1.ConnectionType := ctBluetooth;
  OBDConnection1.BluetoothDeviceName := 'OBDII';
  
  if OBDConnection1.Connect then
    ShowMessage('Connected via Bluetooth!');
end;
```

---

## 🏗️ Architecture

### Rendering Pipeline (v2.0)
```
┌─────────────────────────────────────────────┐
│  TOBDCustomControl (Base Class)            │
│  - Timer management                         │
│  - Frame rate control                       │
│  - PaintSkia(Canvas: ISkCanvas) virtual    │
└─────────────────────────────────────────────┘
                    │
                    ├── Paint() override
                    │   ├── TSkSurface.MakeFromHDC(Canvas.Handle)
                    │   ├── PaintSkia(Surface.Canvas) 
                    │   └── Surface.Flush
                    │
          ┌─────────┴──────────┐
          │                    │
    Child Classes         Zero-Copy
    Override PaintSkia    Direct Rendering
    - CircularGauge       No TBitmap buffer
    - LED                 No format conversion
    - MatrixDisplay       GPU accelerated
    - Touch.*
```

### Component Hierarchy
```
TCustomControl (VCL)
  └── TOBDCustomControl (Base with Skia)
        ├── TOBDCircularGauge
        ├── TOBDLED
        ├── TOBDMatrixDisplay
        ├── TOBDTouchHeader
        ├── TOBDTouchStatusbar
        └── TOBDTouchSubheader
```

### Animation System
```
Timer (30-60 FPS)
  └── AnimationTimerProc
        ├── Calculate elapsed time
        ├── Apply easing function
        ├── Interpolate value
        └── Invalidate (trigger repaint)
```

**Performance Optimizations:**
- Cached background rendering
- On-demand repaints
- Frame rate control
- Zero-copy Skia rendering
- GPU acceleration where available

---

## 📁 Examples

### Available Examples

| Example | Description | Key Features |
|---------|-------------|--------------|
| **minimal** | Bare minimum connection | Basic connect/read cycle |
| **simple** | Simple dashboard | One gauge, basic UI |
| **advanced** | Full-featured dashboard | Multiple gauges, all services |
| **serial** | Serial/COM connection | RS232/USB adapter |
| **bluetooth** | Bluetooth connection | Wireless adapter |
| **wifi** | WiFi connection | Network adapter |
| **ftdi** | FTDI connection | FTDI chip adapter |

### Running Examples

1. Navigate to `examples/[example-name]/`
2. Open the `.dpr` file
3. Configure connection settings if needed
4. Run (F9)

**Note:** Ensure your OBD adapter is connected before running examples.

---

## 📚 API Reference

### Key Classes

#### TOBDConnectionComponent
Manages connection to OBD adapter.

**Properties:**
- `ConnectionType`: ctSerial, ctBluetooth, ctWifi, ctFTDI
- `SerialPort`: COM port name (e.g., 'COM3')
- `BaudRate`: Communication speed
- `BluetoothDeviceName`: BT device name
- `WifiHost`, `WifiPort`: WiFi settings

**Methods:**
- `Connect: Boolean` - Establish connection
- `Disconnect` - Close connection
- `IsConnected: Boolean` - Check status

#### TOBDProtocolComponent
Handles OBD protocol communication.

**Properties:**
- `Protocol`: Auto, ISO9141, KWP2000, CAN, etc.
- `AutoDetect: Boolean` - Auto-detect protocol

**Methods:**
- `ReadParameter(Service, PID: Byte; out Value): Boolean`
- `ReadTroubleCodes: TArray<string>`
- `ClearTroubleCodes: Boolean`
- `ReadVIN: string`

#### TOBDGaugeComponent
Binds gauge to OBD data.

**Properties:**
- `Service`, `PID`: OBD parameter to read
- `UpdateInterval`: Refresh rate (ms)
- `AutoApplyValue`: Automatic value updates

---

## ⚡ Performance

### Benchmarks (Windows 10, i7-8700K, GTX 1070)

| Metric | Value |
|--------|-------|
| **Rendering FPS** | 60 FPS (vsync limited) |
| **CPU per component** | <2% |
| **Memory per component** | ~2-5 MB |
| **Animation smoothness** | Butter smooth |
| **Latency** | <16ms frame time |

### Optimization Tips

1. **Use caching** - Background images are automatically cached
2. **Limit FPS** - Set `FramesPerSecond` to 30 for static content
3. **Batch updates** - Update multiple values before invalidating
4. **Disable animations** - For very frequent updates, disable Animation.Enabled

---

## 🔧 Troubleshooting

### Common Issues

**Q: Components don't show up in Tool Palette**
- A: Reinstall DesignTime.dpk package
- A: Check library path includes source folder

**Q: "ToBitmap undeclared identifier" error**
- A: This was fixed in v2.0. Update to latest version.

**Q: Skia rendering errors**
- A: Ensure Skia4Delphi is properly installed from GetIt
- A: Check that Skia.Vcl unit is in uses clause

**Q: Connection fails**
- A: Check COM port/device name is correct
- A: Verify adapter is powered and connected
- A: Try different baud rate (38400, 115200)

**Q: No data received**
- A: Ensure engine is running (some adapters need this)
- A: Check protocol is correctly detected
- A: Try manual protocol selection

**Q: Animations stuttering**
- A: Reduce `FramesPerSecond` property
- A: Check CPU usage isn't maxed out
- A: Disable other animations temporarily

### Debug Mode

Enable debug logging:
```delphi
OBDConnection1.DebugMode := True;
OBDConnection1.OnDebugLog := DebugLogHandler;
```

---

## 🤝 Contributing

We welcome contributions! Here's how:

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/AmazingFeature`)
3. **Commit** your changes (`git commit -m 'Add AmazingFeature'`)
4. **Push** to the branch (`git push origin feature/AmazingFeature`)
5. **Open** a Pull Request

### Code Style
- Follow existing naming conventions
- Comment complex logic
- Update documentation
- Test thoroughly

### Areas We Need Help
- Additional protocol support
- More examples
- Unit tests
- Documentation improvements
- Bug fixes

---

## 📜 License

This project is licensed under the Apache 2.0 License - see the [LICENSE.md](LICENSE.md) file for details.

**Copyright © 2024-2026 Ernst Reidinga (ERDesigns)**

---

## 🙏 Acknowledgments

- **Skia4Delphi Team** - For the amazing Skia graphics library
- **ELM327 Community** - For protocol documentation
- **Contributors** - Thank you for your contributions!

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/erdesigns-eu/Delphi-OBD/issues)
- **Discussions**: [GitHub Discussions](https://github.com/erdesigns-eu/Delphi-OBD/discussions)
- **Email**: ernst@erdesigns.eu

---

## 🗺️ Roadmap

### v2.0 (Current)
- ✅ Skia4Delphi rendering
- ✅ Zero-copy architecture
- ✅ All components refactored
- ✅ Performance optimizations

### v2.1 (Planned)
- [ ] Additional OBD services
- [ ] More examples
- [ ] Video tutorials
- [ ] Unit tests

### v3.0 (Future)
- [ ] Mobile support (FMX)
- [ ] Cloud logging
- [ ] Advanced analytics
- [ ] Plugin system

---

**⭐ Star this repo if you find it useful!**

**For detailed getting started guide, see [QuickStart.md](QuickStart.md)**
