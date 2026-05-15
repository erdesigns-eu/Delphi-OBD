# 05-SecOC-WrapUnwrap

End-to-end SecOC demo without any bus or hardware. Registers an
AES-128 key against a Data ID, wraps a UDS request as an Authentic
PDU, prints the wire bytes, then unwraps and verifies. Also shows
that:

- a **single-bit tamper** in the original PDU trips the MAC check
  (raises `EOBDSecOCError`)
- a **replayed buffer** trips the freshness check (raises
  `EOBDSecOCError`) — even with the bytes byte-for-byte identical
  to the previous valid one, because the receiver's freshness
  counter has moved on.

Demonstrates the SecOC stack:

- `OBD.Protocol.SecOC.AES` — constant-time AES-128
- `OBD.Protocol.SecOC.CMAC` — RFC 4493 / NIST SP 800-38B
- `OBD.Protocol.SecOC.Keys` — in-memory key store
- `OBD.Protocol.SecOC.Freshness` — in-memory freshness manager
- `OBD.Protocol.SecOC` — wrap / unwrap codec

In production, replace the in-memory providers with NVM- /
HSM-backed implementations (the codec only depends on the
`IOBDSecOCKeyProvider` and `IOBDSecOCFreshnessProvider` interfaces).

## Build

```
dcc32 -B SecOCDemo.dpr
```

## Run

```
SecOCDemo
```

Expected output (truncated):

```
--- Wrap ---
   Original PDU  (  3 B):  22F190
  Authentic PDU  ( 13 B):  22F19000 01XXXXXX XXXXXXXX XX
Layout         : <Original 3> | <Truncated FV 2> | <Truncated MAC 8>

--- Unwrap ---
   Recovered PDU  (  3 B):  22F190
Freshness V    : 1
Data ID        : 0x0E80

--- Tamper detection ---
OK: SecOC unwrap: MAC verification failed (Data ID 0x0E80)

--- Replay detection ---
OK: SecOC unwrap: freshness rejected (Data ID 0x0E80)

Demo complete.
```
