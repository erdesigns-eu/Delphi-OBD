# Flashing safety

> ⚠️ **Misuse of the flashing components in this package can permanently
> brick a vehicle's electronic control unit.** Damage may include disabled
> accessories, an undriveable car, or unrecoverable ECU failure. The
> authors and copyright holders accept no responsibility. Read this
> document in full before using `TOBDFlasher` or any `TOBDCodingSession`
> against a real vehicle.

## What "bricking" means

An ECU is bricked when its firmware is left in a state where the
bootloader cannot start the application image. Recovery typically
requires a physical bench tool (BDM, JTAG, or vendor-specific
flash-recovery hardware) and in worst cases a replacement unit.

## Pre-conditions for any real flash

`TOBDFlasher` will *log warnings* and *abort with confirmation* but does
not enforce these for you. The developer using the component is
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

Every destructive component (`TOBDFlasher`, `TOBDUDSWriteDID`,
`TOBDClearDTC`, `TOBDUDSReset`, `TOBDSystemControl`, …) publishes:

- `AutoExecute: Boolean` — defaults `False`.
- `OnConfirmExecute(Sender; const Description: string; var Allow: Boolean)` —
  fired before the destructive action.

The developer must either:

1. **Wire `OnConfirmExecute`** to a real UI button / dialog / voice
   confirmation, or
2. **Set `AutoExecute := True`** explicitly, taking responsibility for
   the operation.

If neither is done, the operation aborts with `EOBDConfig`.

## Recovery procedures

### Soft recovery (most common)

A flash interrupted mid-transfer often leaves the ECU in
**bootloader-only** mode. Symptoms: the application does not boot
(e.g. engine does not start, instrument cluster shows fault), but the
ECU still answers to UDS in session 0x02 (programmingSession).

`TOBDFlashCheckpoint` records the last good block; resuming the flash
from the checkpoint will usually complete the transfer and restore the
ECU. Re-run the flash from the same checkpoint file.

### Hard recovery (vendor-specific)

If the bootloader itself is corrupted, recovery requires a BDM/JTAG tool
or vendor service mode. This is **not** something Delphi-OBD or any
diagnostic adapter can do. Document with the OEM service procedure.

## Audit log

`TOBDFlasher` writes a `TOBDCodingAuditLog` entry per flash: timestamp,
VIN, ECU address, firmware hash, signature outcome, transfer result,
operator (if your application authenticates a user), and per-block
checksum trail. Keep these — they are how you reconstruct what
happened if something goes wrong.

## Liability

The `LICENSE` file carries the legal disclaimer. Reading and
understanding this document is part of the responsibility you accept by
using the flashing components.
