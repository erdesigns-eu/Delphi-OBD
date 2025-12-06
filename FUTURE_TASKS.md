# Future Development Tasks

This document outlines planned enhancements and features for future development sessions.

---

## 1. Radio Code Calculator Expansion

### Priority: HIGH

### Task 1.1: Add Volkswagen/Audi Radio Calculators
**Estimated Effort:** 2-3 hours  
**Description:** Implement calculators for VW/Audi radio systems (most requested feature)

**Subtasks:**
- [ ] Research VW RCD/RNS radio code algorithms
- [ ] Research Audi Concert/Symphony/RNS-E algorithms
- [ ] Implement `OBD.RadioCode.VW.RCD.pas`
- [ ] Implement `OBD.RadioCode.VW.RNS.pas`
- [ ] Implement `OBD.RadioCode.Audi.Concert.pas`
- [ ] Implement `OBD.RadioCode.Audi.Symphony.pas`
- [ ] Add validation for VW serial number formats (starts with VWZ)
- [ ] Test with known good VIN/serial combinations

**References:**
- VW radio serial format: VWZxxxxx or similar
- Audi format varies by model year

---

### Task 1.2: Add Base Class Helper Methods
**Estimated Effort:** 2 hours  
**Description:** Reduce code duplication by adding common validation and calculation helpers

**Subtasks:**
- [ ] Add `SanitizeInput()` - trim, uppercase, remove separators
- [ ] Add `ValidateLength()` - check expected length with clear error
- [ ] Add `ValidateDigits()` - validate digit ranges
- [ ] Add `ValidateLetters()` - validate letter ranges
- [ ] Add `ValidateAlphanumeric()` - combined validation
- [ ] Add `ApplyModularTransform()` - common modulo operations
- [ ] Add `CalculateChecksum()` - Luhn, Verhoeff, etc.
- [ ] Refactor existing calculators to use helpers
- [ ] Update unit tests

**Implementation:**
```pascal
type
  TOBDRadioCode = class(TInterfacedObject, IOBDRadioCode)
  protected
    function SanitizeInput(const Input: string): string; virtual;
    function ValidateLength(const Input: string; Expected: Integer; 
      var ErrorMsg: string): Boolean; virtual;
    function ValidateDigits(const Input: string; StartPos, EndPos: Integer; 
      var ErrorMsg: string): Boolean; virtual;
    function ValidateLetters(const Input: string; StartPos, EndPos: Integer; 
      var ErrorMsg: string): Boolean; virtual;
    function ApplyModularTransform(Value: Integer; Modulo: Integer): Integer; virtual;
  public
    // ... existing methods
  end;
```

---

### Task 1.3: Implement Additional Manufacturers
**Estimated Effort:** 4-6 hours  
**Description:** Add support for commonly requested radio brands

**Priority List:**
1. [ ] **Nissan** (BP series)
   - File: `OBD.RadioCode.Nissan.BP.pas`
   - Multiple variants: BP234x, BP438x, BP530x
   - Algorithm varies by series

2. [ ] **Toyota/Lexus**
   - File: `OBD.RadioCode.Toyota.pas`
   - File: `OBD.RadioCode.Lexus.pas`
   - Usually letter prefix + digits
   - Position-weighted calculation

3. [ ] **Mercedes-Benz**
   - File: `OBD.RadioCode.Mercedes.Audio.pas`
   - Similar to Becker (many are Becker-made)
   - Device ID or serial number input

4. [ ] **Opel/Vauxhall**
   - File: `OBD.RadioCode.Opel.pas`
   - GM radio systems
   - Multiple format variants

5. [ ] **Honda**
   - File: `OBD.RadioCode.Honda.pas`
   - Serial and VIN-based methods
   - Different algorithms by year

6. [ ] **Hyundai/Kia**
   - File: `OBD.RadioCode.Hyundai.pas`
   - Modern radio systems
   - May require dealer codes

**Research Required:**
- Document format requirements
- Verify algorithms with test data
- Check for regional variations

---

### Task 1.4: Lookup Table Framework
**Estimated Effort:** 3-4 hours  
**Description:** Implement efficient lookup table support for large databases

**Subtasks:**
- [ ] Design database schema for lookup tables
- [ ] Implement binary search for large tables
- [ ] Add table compression/encoding
- [ ] Create `OBD.RadioCode.LookupTable.pas` base class
- [ ] Migrate Ford.V.pas to use framework (currently 8.6MB!)
- [ ] Add caching mechanism
- [ ] Support SQLite backend option
- [ ] Memory-map large files for efficiency

**Architecture:**
```pascal
type
  TLookupTableFormat = (ltBinary, ltSQLite, ltMemoryMapped);
  
  TOBDRadioCodeLookupTable = class(TOBDRadioCode)
  private
    FTableFormat: TLookupTableFormat;
    FTablePath: string;
    FCache: TDictionary<string, string>;
  protected
    function LoadTable: Boolean; virtual;
    function SearchTable(const Key: string): string; virtual;
  public
    property TableFormat: TLookupTableFormat read FTableFormat write FTableFormat;
    property TablePath: string read FTablePath write FTablePath;
  end;
```

---

### Task 1.5: Input Format Auto-Detection
**Estimated Effort:** 2 hours  
**Description:** Automatically detect and suggest correct format

**Subtasks:**
- [ ] Add format detection heuristics
- [ ] Implement similarity checking (Levenshtein distance)
- [ ] Provide suggestions when format is close
- [ ] Add format examples in error messages
- [ ] Support multiple valid formats per calculator

**Example:**
```pascal
function TOBDRadioCode.DetectFormat(const Input: string): TRadioCodeFormat;
function TOBDRadioCode.SuggestCorrection(const Input: string): string;
```

---

## 2. OBD Adapter Support Expansion

### Priority: HIGH

### Task 2.1: OBDLink SX/MX Advanced Features
**Estimated Effort:** 3-4 hours  
**Description:** Fully implement OBDLink-specific ST commands and features

**Subtasks:**
- [ ] Implement voltage monitoring (`STVR`, `STVSM`)
- [ ] Add battery alerts (`STSLVA`, `STCSLVA`)
- [ ] Implement CAN filtering (`STFAP`, `STFAP OFF`)
- [ ] Add J1939 support (`STJ1939 ...`)
- [ ] Implement firmware update capability
- [ ] Add advanced error detection
- [ ] Create `OBD.Adapter.OBDLink.Advanced.pas`
- [ ] Document all ST commands comprehensively

**ST Commands to Implement:**
```
STDI      - Device Identification
STI       - Interface Information  
STIX      - Extended Interface Info
STVR      - Voltage Reading
STVSM     - Voltage Status Monitor
STSLVA    - Set Low Voltage Alert
STFMR     - Factory Mode Reset
STFAP     - Flow Adjust Parameters
STJ1939   - J1939 Protocol Support
STPX      - Protocol Explorer
```

---

### Task 2.2: Add OBDII ELM327 Clone Detection
**Estimated Effort:** 2 hours  
**Description:** Detect and handle Chinese ELM327 clones with quirks

**Subtasks:**
- [ ] Add version string parsing and validation
- [ ] Detect clone chips (CH340, PL2303, etc.)
- [ ] Implement workarounds for common clone issues
- [ ] Add compatibility mode selection
- [ ] Log warnings for problematic adapters
- [ ] Create adapter identification database

**Known Clone Issues:**
- Incorrect voltage readings
- Protocol misdetection
- CAN timing issues
- Missing commands
- Fake version strings

---

### Task 2.3: Add OBDLink LX/EX Support
**Estimated Effort:** 2-3 hours  
**Description:** Support professional-grade OBDLink adapters

**Subtasks:**
- [ ] Implement enhanced CAN features
- [ ] Add multi-ECU simultaneous monitoring
- [ ] Implement hardware filtering
- [ ] Add DTC freeze frame support
- [ ] Implement enhanced diagnostic modes
- [ ] Support WiFi models (LX WiFi)

---

### Task 2.4: Add VGate vLinker Support
**Estimated Effort:** 2 hours  
**Description:** Support VGate vLinker BLE adapters

**Subtasks:**
- [ ] Research vLinker protocol differences
- [ ] Implement vLinker-specific commands
- [ ] Add BLE 4.0/5.0 connection handling
- [ ] Handle vLinker firmware variations
- [ ] Add model detection (FS, MC, FD)
- [ ] Create `OBD.Adapter.VGate.pas`

---

### Task 2.5: Add J2534 PassThru Support
**Estimated Effort:** 5-6 hours  
**Description:** Support professional SAE J2534 interfaces

**Subtasks:**
- [ ] Research J2534 API specification
- [ ] Implement J2534 DLL wrapper
- [ ] Add device enumeration
- [ ] Support multiple protocols (CAN, ISO, etc.)
- [ ] Implement PassThru message handling
- [ ] Add filtering and flow control
- [ ] Create `OBD.Connection.J2534.pas`
- [ ] Support major brands (Drew Tech, Tactrix, etc.)

**J2534 Devices to Support:**
- Drew Tech (Mongoose, CarDAQ)
- Tactrix OpenPort
- Intrepid neoVI
- Kvaser interfaces

---

## 3. DoIP/UDP Enhancement

### Priority: MEDIUM

### Task 3.1: Complete UDP Adapter Integration
**Estimated Effort:** 3-4 hours  
**Description:** Finish integrating DoIP with existing architecture

**Subtasks:**
- [ ] Create `OBD.Adapter.UDP.pas`
- [ ] Integrate with adapter factory pattern
- [ ] Add UDP connection to connection enumerator
- [ ] Implement UDS service translation layer
- [ ] Add session control (Service 0x10)
- [ ] Implement security access (Service 0x27)
- [ ] Test with existing OBD services
- [ ] Create example applications

---

### Task 3.2: Add BMW-Specific Features
**Estimated Effort:** 4-5 hours  
**Description:** Implement BMW-specific diagnostic features

**Subtasks:**
- [ ] Add BMW ECU address database
  - Gateway: 0x1000
  - DME (Engine): 0x12
  - DDE (Diesel): 0x12
  - EGS (Trans): 0x18
  - ABS/DSC: 0x34
  - CAS (Key): 0x6F1
  - Kombi (Instrument): 0xBF
- [ ] Implement BMW FA (vehicle order) reading
- [ ] Add BMW VIN lookup
- [ ] Implement BMW coding/programming protection
- [ ] Add ISN (immobilizer) reading support
- [ ] Create BMW-specific service classes

---

### Task 3.3: Add DoIP Vehicle Discovery
**Estimated Effort:** 2-3 hours  
**Description:** Automatic discovery of DoIP-capable vehicles on network

**Subtasks:**
- [ ] Implement UDP broadcast discovery (port 13400)
- [ ] Parse vehicle announcement messages
- [ ] Extract VIN, EID, GID from announcements
- [ ] Create vehicle list UI component
- [ ] Add connection wizard
- [ ] Cache discovered vehicles

---

### Task 3.4: Implement DoIP TLS Support
**Estimated Effort:** 4-5 hours  
**Description:** Add secure diagnostics support

**Subtasks:**
- [ ] Implement TLS 1.2/1.3 for DoIP (port 3496)
- [ ] Add certificate handling
- [ ] Implement authentication mechanisms
- [ ] Add encrypted diagnostic messages
- [ ] Support vehicle-specific certificates
- [ ] Create secure session management

---

### Task 3.5: Add Mercedes-Benz DoIP Support
**Estimated Effort:** 3-4 hours  
**Description:** Support Mercedes XENTRY-compatible DoIP

**Subtasks:**
- [ ] Research Mercedes DoIP specifics
- [ ] Add Mercedes ECU addressing
- [ ] Implement Mercedes-specific services
- [ ] Add ODX (diagnostic descriptions) parsing
- [ ] Support XENTRY passthrough mode

---

## 4. Protocol Enhancements

### Priority: MEDIUM

### Task 4.1: Add ISO 9141 Enhanced Support
**Estimated Effort:** 3 hours  
**Description:** Better support for older vehicles

**Subtasks:**
- [ ] Implement ISO 9141 5-baud init
- [ ] Add fast init support
- [ ] Improve timing accuracy
- [ ] Add protocol-specific error handling
- [ ] Test with older vehicles (pre-2000)

---

### Task 4.2: Add J1850 PWM/VPW Improvements
**Estimated Effort:** 2-3 hours  
**Description:** Better GM/Ford support

**Subtasks:**
- [ ] Improve frame parsing
- [ ] Add manufacturer-specific extensions
- [ ] Implement GM-specific modes
- [ ] Add Ford-specific commands
- [ ] Better error detection

---

### Task 4.3: Implement CAN-FD Support
**Estimated Effort:** 4-5 hours  
**Description:** Support modern CAN with Flexible Data-rate

**Subtasks:**
- [ ] Research CAN-FD protocol differences
- [ ] Implement extended frame parsing (up to 64 bytes)
- [ ] Add bit rate switching support
- [ ] Update parsers for larger payloads
- [ ] Test with CAN-FD capable adapters

---

### Task 4.4: Add UDS (ISO 14229) Complete Implementation
**Estimated Effort:** 6-8 hours  
**Description:** Full Unified Diagnostic Services support

**Services to Implement:**
- [ ] 0x10 - Diagnostic Session Control
- [ ] 0x11 - ECU Reset
- [ ] 0x14 - Clear Diagnostic Information
- [ ] 0x19 - Read DTC Information (enhanced)
- [ ] 0x22 - Read Data By Identifier
- [ ] 0x23 - Read Memory By Address
- [ ] 0x24 - Read Scaling Data By Identifier
- [ ] 0x27 - Security Access
- [ ] 0x28 - Communication Control
- [ ] 0x2E - Write Data By Identifier
- [ ] 0x2F - Input Output Control By Identifier
- [ ] 0x31 - Routine Control
- [ ] 0x34/0x35/0x36/0x37 - Download/Upload
- [ ] 0x3D - Write Memory By Address
- [ ] 0x3E - Tester Present
- [ ] 0x85 - Control DTC Setting

---

## 5. Testing and Quality

### Priority: HIGH

### Task 5.1: Add Unit Tests
**Estimated Effort:** 6-8 hours  
**Description:** Comprehensive test coverage

**Subtasks:**
- [ ] Set up DUnit testing framework
- [ ] Add protocol parsing tests
  - CAN frame parsing
  - Legacy frame parsing
  - DoIP message parsing
- [ ] Add radio calculator tests
  - Known good input/output pairs
  - Edge cases
  - Error conditions
- [ ] Add service decoder tests
- [ ] Add connection tests (mocked)
- [ ] Achieve >80% code coverage

---

### Task 5.2: Add Integration Tests
**Estimated Effort:** 4-6 hours  
**Description:** End-to-end testing

**Subtasks:**
- [ ] Create test harness with mock adapters
- [ ] Simulate real OBD responses
- [ ] Test complete diagnostic sessions
- [ ] Add performance benchmarks
- [ ] Create test reports

---

### Task 5.3: Add Continuous Integration
**Estimated Effort:** 3-4 hours  
**Description:** Automated build and test pipeline

**Subtasks:**
- [ ] Set up GitHub Actions workflow
- [ ] Add automated compilation
- [ ] Add automated test execution
- [ ] Add code quality checks
- [ ] Add documentation generation

---

## 6. Documentation

### Priority: MEDIUM

### Task 6.1: Create User Documentation
**Estimated Effort:** 4-5 hours  
**Description:** Comprehensive user guide

**Subtasks:**
- [ ] Write getting started guide
- [ ] Document each connection type
- [ ] Document each adapter type
- [ ] Create radio calculator guide
- [ ] Add troubleshooting section
- [ ] Create FAQ
- [ ] Add code examples

---

### Task 6.2: Create Developer Documentation
**Estimated Effort:** 3-4 hours  
**Description:** API reference and architecture docs

**Subtasks:**
- [ ] Document class hierarchy
- [ ] Create architecture diagrams
- [ ] Document protocols
- [ ] API reference
- [ ] Extension guide
- [ ] Contributing guidelines

---

### Task 6.3: Create Example Applications
**Estimated Effort:** 6-8 hours  
**Description:** Ready-to-run examples

**Examples to Create:**
- [ ] Simple DTC reader
- [ ] Live data monitor
- [ ] Radio code calculator app
- [ ] BMW ENET diagnostic tool
- [ ] Multi-adapter scanner
- [ ] Advanced dashboard with gauges

---

## 7. Performance Optimization

### Priority: LOW

### Task 7.1: Optimize String Operations
**Estimated Effort:** 2-3 hours  
**Description:** Improve hot path performance

**Subtasks:**
- [ ] Replace TStringList with TList<string> where appropriate
- [ ] Use StringBuilder for concatenation
- [ ] Optimize hex string conversions
- [ ] Cache frequently used strings
- [ ] Profile and optimize IsHex function

---

### Task 7.2: Add Memory Pooling
**Estimated Effort:** 3-4 hours  
**Description:** Reduce allocations in parsing

**Subtasks:**
- [ ] Implement TBytes pool
- [ ] Add frame object pool
- [ ] Add message object pool
- [ ] Benchmark improvements

---

## 8. UI Components

### Priority: LOW

### Task 8.1: Create Visual Components
**Estimated Effort:** 8-10 hours  
**Description:** Ready-to-use UI components

**Components to Create:**
- [ ] Adapter selector component
- [ ] Connection status indicator
- [ ] Live data display grid
- [ ] DTC list viewer
- [ ] Protocol indicator
- [ ] Signal strength meter
- [ ] Radio code calculator UI

---

## Summary

**Total Estimated Effort:** 100-130 hours across all tasks

**Recommended Priority Order:**
1. Radio code calculators (high user demand)
2. OBD adapter support expansion
3. Complete DoIP integration
4. Unit testing
5. Documentation
6. Performance optimization

**Quick Wins (< 2 hours each):**
- Add base class helpers
- Fix remaining typos
- Add input sanitization
- Implement auto-detection
- Add clone detection

**Long-term Projects (> 6 hours):**
- J2534 support
- Complete UDS implementation
- Lookup table framework
- Full test coverage
- Example applications

---

## Contributing

When working on these tasks:
1. Create a new branch for each major task
2. Add unit tests for new features
3. Update documentation
4. Follow existing code style
5. Submit PR with clear description
6. Reference this document in commit messages

**Example:**
```
git checkout -b feature/vw-radio-calculator
# ... make changes ...
git commit -m "Add VW/Audi radio calculators (Task 1.1)"
```

---

*Last Updated: December 6, 2024*
