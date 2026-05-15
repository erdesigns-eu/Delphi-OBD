# 01-ConnectAndPing

Smoke-test sample. Connects to a Wi-Fi ELM327 (or compatible) adapter,
sends the `ATZ` soft-reset command, and prints the response.

## Build

```cmd
dcc32 -B ConnectAndPing.dpr
```

## Run

```cmd
ConnectAndPing.exe                          REM defaults: 192.168.0.10:35000
ConnectAndPing.exe 192.168.4.1 35000        REM common ESP-Link host
ConnectAndPing.exe elm327.local 35000       REM mDNS host
ConnectAndPing.exe --async 192.168.0.10 35000  REM use OpenAsync (non-blocking)
```

The `--async` (or `-a`) flag switches the sample from `Open` (blocking)
to `OpenAsync` (non-blocking). In a console sample the visible
behaviour is similar; in a GUI app `OpenAsync` keeps the message loop
running so the form stays responsive while the connect attempt is in
flight.

## Expected output

```
Connecting to 192.168.0.10:35000…
Connected. Sending ATZ.
Response:

ELM327 v1.5

>
```

## What it demonstrates

- Allocating `TOBDConnection` from code (no form needed)
- Selecting the Wi-Fi transport via the `Transport` enum
- Wiring `OnDataReceived` and `OnError` events
- Synchronous `Open`, blocking on a `TEvent` for the response
- Clean shutdown via `Close` + `Free`

## Out of scope

- Adapter detection / chip-family identification → `TOBDAdapter`
- AT-command framing with terminator-aware response collection →
  `TOBDAdapter` + `TOBDProtocol`
