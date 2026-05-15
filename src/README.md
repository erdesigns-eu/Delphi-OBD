# Source layout

| Folder | Purpose | UI-allowed? |
|---|---|---|
| `Core/` | Types, constants, decoders, JSON catalogue loader, errors. | No |
| `Connection/` | `TOBDConnection` + transport implementations (Serial/BT/BLE/WiFi/UDP/FTDI). | No |
| `Adapter/` | `TOBDAdapter`, AT/ST command tables, capability detection. | No |
| `Protocol/` | Wire protocols: ISO 9141, KWP2000, ISO 15765, J1850, J1939, UDS, DoIP, SecOC, LIN, FlexRay, MOST. | No |
| `Services/` | OBD-II service-mode components (Mode 01–0A), WWH-OBD, recorder/replayer. | No |
| `Coding/` | UDS/KWP write surface, per-OEM coding helpers, audit log, diff. | No |
| `Flashing/` | UDS transfer state machine, flasher orchestrator, voltage gate, checkpoint, J1939 memory access. | No |
| `Signature/` | Signature verification backends (BCrypt / OpenSSL / HSM / PQC). | No |
| `DesignTime/` | Component palette registration, property editors, component editors. | **Yes** (the IDE itself is VCL). |
| `UI/` | *Reserved.* No visual companions in v1 (headless decision); folder is a placeholder for post-1.0 visual add-ons. | n/a |

`Vcl.*` and `FMX.*` references are forbidden in every folder except
`DesignTime/`. CI enforces this.

The file header template lives at [`HEADER.template.pas`](HEADER.template.pas).
Every `.pas` file in this tree starts with that header, filled in. See
[`../STYLE.md`](../STYLE.md) for the full code style.
