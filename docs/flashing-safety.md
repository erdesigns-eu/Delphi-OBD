# Flashing safety

> ⚠️ **Misuse of the flashing components in this package can permanently
> brick a vehicle's electronic control unit.** Damage may include disabled
> accessories, an undriveable car, or unrecoverable ECU failure. The
> copyright holder accepts no responsibility (see sections 9 to 11 of
> the [LICENSE](../LICENSE)). Read this document in full before using
> `TOBDFlashPipeline`, `TOBDFlasher`, `TOBDUDSTransfer` or any
> `TOBDCodingSession` against a real vehicle.

## What "bricking" means

An ECU is bricked when its firmware is left in a state where the
bootloader cannot start the application image. Recovery typically
requires a physical bench tool (BDM, JTAG, or vendor-specific
flash-recovery hardware) and in worst cases a replacement unit.

## Pre-conditions for any real flash

The flashing components *log warnings* and *refuse to run without an
explicit go-ahead*, but they do not enforce these for you. The developer using the component is
responsible for verifying:

1. **Stable power.** Battery voltage 12.5 V – 14.4 V (passenger car) or
   manufacturer-specified range (heavy-duty / EV). Use `TOBDVoltageGate`
   to monitor live during transfer.
2. **No additional electrical load.** Lights off, infotainment off, HVAC
   off. Doors closed (some BCMs power down accessories during flash).
3. **Engine off, ignition on (KL15).** Or vehicle-specific service mode.
4. **Cabin temperature 10 °C – 35 °C** unless OEM specifies otherwise.
5. **Stable bus.** No tester other than the flasher on the diagnostic
   bus. Disable any sniffers, gateways, or proxy tools.
6. **Verified firmware.** Always pass a `TOBDSignatureVerifier` and
   reject any unsigned image. The OEM's signing key is not an option;
   it is a requirement.
7. **Transfer time.** Some ECUs require uninterrupted power for 20+
   minutes. Plan accordingly.

## Confirmation pattern

Every destructive component (`TOBDFlashPipeline`, `TOBDUDSTransfer`,
`TOBDFlasher`, `TOBDFlashSession`, `TOBDUDSWriteMemory`,
`TOBDUDSWriteDID`, `TOBDKWPWriteID`, `TOBDDataIdentifierIO`,
`TOBDRoutineControl`, `TOBDCodingSession`, the per-OEM
`TOBDFlashHandshake*`, the key-adaptation components, …) publishes
`AutoExecute: Boolean`, which defaults to `False`. While it is `False`
the write is refused (usually with `EOBDConfig`).

`TOBDFlashPipeline` additionally publishes
`OnConfirmExecute(Sender; AAddress; ASize; var AAllow: Boolean)`. The
developer must either:

1. **Wire `OnConfirmExecute`** to a real UI button / dialog / voice
   confirmation, or
2. **Set `AutoExecute := True`** explicitly, taking responsibility for
   the operation.

If neither is done, the flash aborts with `EOBDConfig`. The
key-adaptation components require both: `AutoExecute = True` *and* an
`OnConfirmExecute` handler that allows the operation.

For a ready-made confirmation UI, `TOBDWriteConfirm` (OBD Studio Pro
palette page) is an inline confirmation strip that asks for a backup
first while `BackupDone` is `False`.

## Pipeline shape

`TOBDFlashPipeline` is the recommended entry point — it composes
the lower-level components into one safe-by-default end-to-end run:

```
Pipeline
  ├─ Protocol         := <TOBDProtocol bound to the bus>
  ├─ AuditLog         := <TOBDCodingAuditLog with an HMAC chain key>
  ├─ VoltageGate      := <TOBDVoltageGate with SourceFunc wired>
  ├─ CheckpointFile   := 'C:\Logs\flash-<vin>-<ts>.json'
  ├─ AddressFormatBytes := 4
  ├─ LengthFormatBytes  := 4
  ├─ ResetAfterFlash    := True
  ├─ AutoExecute        := False             // <-- DO NOT FLIP UNLESS YOU MEAN IT
  ├─ OnConfirmExecute   := <UI confirmation>
  ├─ OnEnterProgramming := <vendor handshake — 9e units>
  ├─ OnVerifyRoutine    := <verify checksum>
  ├─ Checks
  │    ├─ Engine off       (csError, fpPreflight)
  │    ├─ Voltage floor    (csError, fpPreflight)
  │    ├─ Image signature  (csError, fpVerifyImage)
  │    └─ ...
  └─ Flash(address, image)
```

A pipeline with NO `VoltageGate` assigned logs a WARN audit-log
entry at start-of-flash but proceeds — by design, the package surfaces
the safety primitive but does not force it on you. Wire one.

## Signature verification

The package ships four verifier backends behind one registry:

- `TOBDSignatureBCrypt` — Windows CNG (RSA-PSS / RSA-PKCS#1 /
  ECDSA P256 / P384)
- `TOBDSignatureOpenSSL` — OpenSSL 3.x (same set + Ed25519)
- `TOBDSignatureHSM` — PKCS#11 (vendor driver shim)
- `TOBDSignaturePQC` — liboqs (Dilithium / Falcon / SPHINCS+)

Hosts register backends at startup and feed a `csError` check
into `Pipeline.Checks` for `fpVerifyImage` that calls
`TOBDSignatureRegistry.Default.Verify` on the image bytes. The
pipeline aborts before any wire access if the signature fails.

## Recovery procedures

### Soft recovery (most common)

A flash interrupted mid-transfer often leaves the ECU in
**bootloader-only** mode. Symptoms: the application does not boot
(e.g. engine does not start, instrument cluster shows fault), but the
ECU still answers to UDS in session 0x02 (programmingSession).

`TOBDFlashCheckpoint` records the last accepted chunk + a SHA-256 of
the firmware image. To resume:

```
var Info := TOBDFlashCheckpoint.Load(CheckpointFile);
if not TOBDFlashCheckpoint.MatchesImage(Info, Image) then
  raise Exception.Create('image mismatches checkpoint — refusing to resume');
Transfer.Resume(Info.Cursor, Image);
```

The image-hash check refuses to resume against a different image —
that mismatch is one of the known brick paths.

### Hard recovery (vendor-specific)

If the bootloader itself is corrupted, recovery requires a BDM/JTAG tool
or vendor service mode. This is **not** something Delphi-OBD or any
diagnostic adapter can do. Document with the OEM service procedure.

## Audit log

`TOBDFlashPipeline` and `TOBDCodingSession` write `TOBDCodingAuditLog`
entries as they run: timestamp, session ID, entry kind, target (address,
DID or OEM key), before / after bytes and a message — for a flash that
covers the phase changes, check results and the transfer outcome. With
a key set, every line carries an HMAC-SHA-256 chained to the previous
line, so tampering with any historical entry is detectable. Keep these
logs — they are how you reconstruct what happened if something goes
wrong.

## Liability

The [LICENSE](../LICENSE) carries the legal terms, including the
safety notice, the warranty disclaimer and the limitation of liability.
Reading and understanding this document is part of the responsibility
you accept by using the flashing components.
