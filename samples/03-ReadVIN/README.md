# 03-ReadVIN

End-to-end demo of the connection → adapter → protocol stack:
init → request VIN via **OBD-II Service 09 PID 02**.

## Build & run

```cmd
dcc32 -B ReadVIN.dpr
ReadVIN.exe                     REM defaults: 192.168.0.10:35000
ReadVIN.exe 192.168.4.1 35000   REM common ESP-Link host
```

## Expected output (genuine ELM327 v2.3 + Wi-Fi clone)

```
Connecting to 192.168.0.10:35000…
Detecting adapter…
  [1/6  17%] Resetting — ATZ
  [2/6  33%] Echo off — ATE0
  [3/6  50%] Reading ATI
  [4/6  67%] Reading AT@1
  [5/6  83%] Reading AT@2
  [6/6 100%] Reading STI
  Adapter: ELM Electronics ELM327 OBD Diagnostics, firmware 2.3
  Max ISO-TP frame: 7 bytes
Initialising adapter…
  [1/7  14%] Reset — ATZ
  [2/7  29%] Echo off — ATE0
  [3/7  43%] Linefeeds off — ATL0
  [4/7  57%] Spaces off — ATS0
  [5/7  71%] Headers off — ATH0
  [6/7  86%] Adaptive timing on — ATAT1
  [7/7 100%] Auto-protocol — ATSP0
Requesting VIN (Service 09 PID 02)…
  [1/3  33%] Encoding
  [2/3  67%] Adapter exchange — 09 02
  [3/3 100%] Decoding
VIN: WAUZZZ8K9HA123456
Round-trip: 187 ms
```

## What it demonstrates

- The full component chain: `TOBDConnection` → `TOBDAdapter` →
  `TOBDProtocol`.
- Both layers' progress events bound to a single console printer
  via `TOBDProgressStep.Percent` / `Index` / `Count` / `Name`.
- `MaxIsoTpFrameBytes` populated from the
  capability registry on `Detect`.
- Synchronous `TOBDProtocol.Request` with a per-call timeout,
  returning a fully decoded `TOBDResponse`.

## Out of scope

- VIN is parsed leniently (printable ASCII filter + 17-char trim).
  A proper VIN decoder (WMI / VDS / VIS, region, year) lands in
  v2.x if added.
- For modes that return data via multi-frame ISO-TP, the chip
  reassembles the stream when `ATAL` is on; the protocol layer
  receives a single concatenated text response. The raw-CAN
  reassembler (`TOBDIso15765Reassembler`) is also available for
  J2534 / DoIP paths.
