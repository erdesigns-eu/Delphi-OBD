# 28-FlashSignedFirmware

> ⚠️ **REAL FLASH SAMPLE — RUN THIS AGAINST A SALVAGE ECU FIRST.**
> Read `docs/flashing-safety.md` end-to-end before touching the
> button. The author is not liable for bricked ECUs, voided
> warranties, or vehicles stranded in customer driveways.

End-to-end production-shape flash:

1. Loads a signed firmware image + its detached signature
2. Wires `TOBDSignatureRegistry` with BCrypt + OpenSSL backends
3. Wires `TOBDVoltageGate` against a host-supplied probe
4. Wires the appropriate `TOBDFlashHandshake*` for the target OEM
5. Wires `TOBDCodingAuditLog` with HMAC chain
6. Configures `TOBDFlashPipeline.CheckpointFile` for resume
7. Runs `Flash` after operator confirmation

The sample is a **template** — the host fills in:

- The transport (TOBDConnection / adapter / TOBDProtocol)
- The voltage probe source (often a dedicated battery monitor
  on the bench)
- The seed → key callback for the chosen OEM
- The verify routine (RID, expected checksum)
- The OEM-specific signing public key

## Build

```
dcc32 -B FlashSignedFirmware.dpr
```

## Run

```
FlashSignedFirmware <vendor> <image.bin> <signature.bin> <pubkey.pem>
```

Vendors: `vag` / `bmw` / `ford` / `hmg` / `mercedes` / `stellantis` /
`toyota`.

## Safety banner

The sample prints a banner at start that the operator must
acknowledge by typing **YES** on the console before any wire
access. The banner restates the brick risk and references
`docs/flashing-safety.md`. Do not remove the banner in
production hosts; replace it with an equivalent UI flow.
