# Source layout

| Folder | Purpose | UI-allowed? |
|---|---|---|
| `Core/` | Types, constants, errors, decoders, JSON catalogue store, binary values, CAN routes, version. | No |
| `Collections/` | Threaded queue. | No |
| `Compat/` | Delphi / FPC compatibility functions and sockets. | No |
| `Utilities/` | Async tasks, logger and sinks, secure settings, attempt counter, nonce, string helpers. | No |
| `Connection/` | `TOBDConnection` + transport implementations (Serial/BT/BLE/WiFi/UDP/FTDI/mock). | No |
| `Adapter/` | `TOBDAdapter`, AT/ST command tables, init scripts, capability detection. | No |
| `Protocol/` | `TOBDProtocol` and the wire protocols: ISO 9141, KWP2000, KWP1281, TP 2.0, ISO 15765, J1850, J1939, UDS, DoIP, SecOC, LIN, FlexRay, MOST; J2534 pass-thru. | No |
| `Service/` | OBD-II service-mode components (Mode 01–0A), WWH-OBD, VIN decoder/inspector, vehicle health, drive cycle, EV battery, dyno calculators, catalogue components, VW RadioSAFE. | No |
| `Diagnostics/` | UDS, KWP2000 and J1939 read-side components. | No |
| `Coding/` | UDS/KWP write surface, per-OEM coding helpers and label files, component protection, audit log, diff. | No |
| `Flashing/` | Flash pipeline, UDS transfer state machine, voltage gate, checkpoint, image applicability, OEM flash handshakes, signature backends (`ERD.Signature.*`: BCrypt / OpenSSL / HSM / PQC), J1939 memory access. | No |
| `Calibration/` | XCP, CCP, A2L parser. | No |
| `Speciality/` | ISOBUS (ISO 11783), digital tachograph. | No |
| `OEM/` | OEM extensions and registries, OEM catalogues, OEM UDS client and sessions, key adaptation. | No |
| `RadioCode/` | Radio-code calculators and EEPROM extractors. | No |
| `Recorder/` | Recorder, replayer, protocol mock, log redactor. | No |
| `DesignTime/` | Component palette registration, palette icons, property editors, component editors. | **Yes** (the IDE itself is VCL). |
| `UI/` | Delphi VCL controls: theme, dashboard and gauges, OBD Visual, OBD Studio and OBD Studio Pro controls. | **Yes** (VCL only). |

`Vcl.*` references are allowed only in `UI/` and `DesignTime/`.
FireMonkey is outside scope: `FMX.*` imports are forbidden throughout `src/`.
`tools/pascalcheck` (`check_runtime.py`) enforces both rules in CI. FPC builds only the nonvisual library.

The file header template lives at [`HEADER.template.pas`](HEADER.template.pas).
Every `.pas` file in this tree starts with that header, filled in. All source
is proprietary to ERDesigns; see [`../LICENSE`](../LICENSE). See
[`../STYLE.md`](../STYLE.md) for the full code style.
