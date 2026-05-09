# 02-DetectAdapter

Identifies an attached OBD adapter — chip family, firmware version,
description, identifier, capability set — using
`TOBDAdapter.DetectAsync`.

## Build & run

```cmd
dcc32 -B DetectAdapter.dpr
DetectAdapter.exe                          REM defaults: 192.168.0.10:35000
DetectAdapter.exe 192.168.4.1 35000
```

## Expected output (genuine ELM327 v2.3)

```
Connecting to 192.168.0.10:35000…
Detecting adapter (async)…
  [1/6   17%] Resetting — ATZ
  [2/6   33%] Echo off — ATE0
  [3/6   50%] Reading ATI
  [4/6   67%] Reading AT@1
  [5/6   83%] Reading AT@2
  [6/6  100%] Reading STI

Identity:
  Family       : afELM327
  AdapterKey   : elm327
  DisplayName  : ELM Electronics ELM327 OBD Diagnostics
  Firmware     : 2.3
  Description  : ELM Electronics ELM327 OBD Diagnostics
  Identifier   : ELM-ECU-2026-001
  Likely clone : False
Capabilities :
  CAN, ISOTP, KLine, VoltageMonitor, ProgrammableHeaders
```

## What it demonstrates

- Binding `TOBDAdapter` to `TOBDConnection`
- Running asynchronous detection without blocking the message loop
- Wiring `OnProgress` for live phase reporting (six steps)
- Reading `Identity` and `Capabilities` after detection completes
- Clone-heuristic flag (`IsClone`) for v1.5 chips with empty AT@1/AT@2

## Out of scope

- Running the post-detect init sequence → `Adapter.Init` /
  `Adapter.InitAsync` (also Phase 3, see PLAN §3.7 dual-method rule)
- Actual OBD reads → Phase 5 (`TOBDLiveData`)
