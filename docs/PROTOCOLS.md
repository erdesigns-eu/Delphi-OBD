# Protocols

A reference for the protocol stack the framework speaks. For the
high-level architecture see [`ARCHITECTURE.md`](ARCHITECTURE.md).

---

## OBD-II transport protocols (ISO 15031-5 mandated)

Every modern light-duty vehicle in EU/US/JP uses one of these on the
DLC pins:

| Protocol | ISO / SAE | Speed | Pins | When you'll see it |
|---|---|---|---|---|
| ISO 15765-4 CAN, 11-bit, 500 kbps | ISO 15765 | 500 kbps | 6, 14 | Most cars 2008+ (US-mandated 2008) |
| ISO 15765-4 CAN, 29-bit, 500 kbps | ISO 15765 | 500 kbps | 6, 14 | Heavy-duty + some EU vehicles |
| ISO 15765-4 CAN, 11-bit, 250 kbps | ISO 15765 | 250 kbps | 6, 14 | Older CAN vehicles |
| ISO 15765-4 CAN, 29-bit, 250 kbps | ISO 15765 | 250 kbps | 6, 14 | Older heavy-duty |
| SAE J1850 PWM | J1850 | 41.6 kbps | 2, 10 | Ford, pre-2008 |
| SAE J1850 VPW | J1850 | 10.4 kbps | 2 | GM, pre-2008 |
| ISO 9141-2 | ISO 9141 | 10.4 kbps | 7, 15 | EU + Asian, 2000-2007 |
| ISO 14230-4 KWP2000 (Fast init) | ISO 14230 | 10.4 kbps | 7, 15 | EU vehicles 2003-2007 |
| ISO 14230-4 KWP2000 (5-baud) | ISO 14230 | 10.4 kbps | 7, 15 | Older variant |

Concrete classes in `src/Protocol/OBD.Protocol.CAN.pas` and
`OBD.Protocol.Legacy.pas` map one-to-one. Selecting the right one is
either explicit (`AT SP n`) or automatic (`AT SP 0`).

---

## ISO-TP framing (ISO 15765-2)

CAN frames cap at 8 data bytes. Anything longer (VIN reads,
multi-DTC responses) is split using ISO-TP framing:

```
PCI nibble  │ Frame type
            │
   0  Single Frame      data length in low nibble (0..7)
   1  First Frame       data length in 12 bits (low nibble + next byte)
   2  Consecutive Frame sequence index in low nibble (0..15, wraps)
   3  Flow Control      tester → ECU only; RX side ignores
```

`TCANOBDProtocol.ParseFrame` reads the PCI byte; `ParseMessage` sorts
consecutive frames by sequence index (with wrap-around handling) and
concatenates the payload into a single `IOBDDataMessage`.

```
Multi-frame VIN response (Service 09 PID 02, 20-byte payload):

  7E8 10 14 49 02 01 31 47 31    ← FF: length=0x014, "1G1"
  7E8 21 4A 43 35 33 35 38 34    ← CF#1 seq=1: "JC53584"
  7E8 22 52 37 31 32 33 34 35    ← CF#2 seq=2: "R712345"
                                   final VIN: "1G1JC53584R712345"
```

See `tests/Tests.Protocol.IsoTp.pas` for golden tests of this exact
scenario.

---

## OBD-II services (SAE J1979)

| Mode | Purpose | Unit |
|---|---|---|
| `01` | Show current data (live PIDs)         | `OBD.Service01` |
| `02` | Show freeze-frame data                | `OBD.Service02` |
| `03` | Show stored DTCs                      | `OBD.Service03` |
| `04` | Clear DTCs and stored values          | `OBD.Service04` |
| `05` | Test results, oxygen sensor monitoring | `OBD.Service05` |
| `06` | Test results, on-board monitoring      | `OBD.Service06` |
| `07` | Show pending DTCs                     | `OBD.Service07` |
| `08` | Control on-board systems              | `OBD.Service08` |
| `09` | Vehicle info (VIN, calibration ID, …) | `OBD.Service09` |
| `0A` | Show permanent DTCs                   | `OBD.Service0A` |

Encoders live in `OBD.Request.Encoders`; decoders in
`OBD.Response.Decoders`. Every PID's formula is documented
inline; tests are in `tests/Tests.Service.{Encoders,Decoders}.pas`.

---

## UDS (ISO 14229)

Unified Diagnostic Services — modern (post-2007) replacement for
KWP2000. Defined in `OBD.Protocol.UDS.pas`. Common services:

| SID | Name | Use |
|---|---|---|
| `10` | DiagnosticSessionControl | Switch session (default / programming / extended) |
| `11` | ECUReset | Hard / key-off-on / soft reset |
| `22` | ReadDataByIdentifier | Read DID (modern alternative to OBD PIDs) |
| `27` | SecurityAccess | Seed-and-key challenge |
| `2E` | WriteDataByIdentifier | Write DID |
| `31` | RoutineControl | Run a built-in routine |
| `34/36/37` | RequestDownload/TransferData/RequestTransferExit | ECU flashing |
| `3E` | TesterPresent | Keep session alive |

The ECU flashing example (`examples/ecuflashing/`) is the canonical
walk-through of a real UDS session.

---

## KWP2000 (ISO 14230)

Older European cars (pre-2008). Lives in `OBD.Protocol.KWP2000.pas`.
Conceptually similar to UDS but with a smaller service set and 8-bit
service IDs.

---

## DoIP (ISO 13400)

Diagnostics over IP — UDS over Ethernet, used by EVs and modern
European vehicles. `OBD.Protocol.DoIP.pas` + `OBD.Connection.UDP.pas`.
The example in `examples/doip/` shows how to discover an ECU on the
local subnet and run a UDS session.

---

## Heavy-duty: SAE J1939

29-bit CAN with PGN-based messaging. `OBD.Protocol.J1939.pas`. Used by
trucks, buses, agricultural equipment. The PGN naming + transport
protocol differs noticeably from light-duty OBD-II — read the unit
header before reaching for it.

---

## In-vehicle bus protocols (non-diagnostic)

These don't run over the OBD-II port but the framework can talk to
them when an appropriate adapter is connected:

| Bus | ISO / Standard | Unit | Use |
|---|---|---|---|
| FlexRay | ISO 17458 | `OBD.Protocol.FlexRay` | High-speed deterministic body / chassis nets |
| LIN     | ISO 17987 | `OBD.Protocol.LIN`     | Low-speed body modules (door handles, mirrors) |
| MOST    | MOST Cooperation | `OBD.Protocol.MOST`  | Infotainment, audio |

---

## Tachograph

`OBD.Protocol.Tacho.pas` covers the EU-mandated digital tachograph
protocol used in commercial vehicles >3.5t.

---

## Adapter dialects

The protocol layer talks to the adapter layer in ASCII hex with
optional formatting:

| Adapter family | Prompt | Echo | Spaces | Headers |
|---|---|---|---|---|
| ELM327 v1.0+   | `>`    | configurable (`AT E0/E1`) | `AT S0/S1` | `AT H0/H1` |
| OBDLink STN1xx | `>`    | as ELM | as ELM | as ELM, plus extended `AT` set |
| Genuine ELM327 | `>`    | identical | identical | identical |

The async wrapper (`TOBDConnectionAsync`) waits for the `>` prompt by
default — works for every ELM-compatible adapter on the market.
