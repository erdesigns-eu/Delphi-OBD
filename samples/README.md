# Samples

One sample per concept. Each sample is a self-contained `.dpr` project
demonstrating one component or one workflow.

> **Looking for a starting point for your own project?** Use the
> design-time wizard instead — File → New → Other → **Delphi-OBD**.
> The wizard ships 26 starter templates with options (DTC reader,
> live data, coding session, flash session, OEM component
> protection, full diagnostics suite, …). It scaffolds a real
> project, components pre-wired. The samples below are reference
> implementations of specific scenarios; the wizard is the fast
> path to "running tool".

## Built and ready to run

| # | Sample | What it shows |
|---|---|---|
| 00 | [`00-Hello`](00-Hello/) | Smoke test — prints the package version. Confirms the runtime package compiles and links. |
| 01 | [`01-ConnectAndPing`](01-ConnectAndPing/) | Connects to a Wi-Fi ELM327, sends `ATZ`, prints the reply. End-to-end transport + adapter handshake. |
| 02 | [`02-DetectAdapter`](02-DetectAdapter/) | Runs `Adapter.Detect` and prints the detected family + capabilities. |
| 03 | [`03-ReadVIN`](03-ReadVIN/) | Connection → adapter → protocol → `TOBDVIN.Read`. Auto-selects between OBD-II Service 09 PID 02 and UDS DID 0xF190. |
| 04 | [`04-LIN-LDF-Parse`](04-LIN-LDF-Parse/) | Standalone LDF parser. No bus / hardware needed. |
| 05 | [`05-SecOC-WrapUnwrap`](05-SecOC-WrapUnwrap/) | End-to-end SecOC demo without any bus. AES-128 CMAC wrap + tamper / replay detection. |
| 07 | [`07-RecordReplay`](07-RecordReplay/) | Recorder → `.obdlog` → Replayer round-trip. |
| 27 | [`27-FlashDryRun`](27-FlashDryRun/) | Walks the entire `TOBDFlashPipeline` phase chain without touching the wire. "Review before commit" tool for flash configs. |
| 28 | [`28-FlashSignedFirmware`](28-FlashSignedFirmware/) | ⚠️ brick risk — signature-verified flash against a real ECU. Requires hardware + signed image + voltage source. |
| 29 | [`29-J1939Flash`](29-J1939Flash/) | DM14 / DM15 / DM16 memory access on a heavy-duty J1939 bus. |

## Build & run

Each sample compiles with the same toolchain as the rest of the
package:

```cmd
dcc32 -B <SampleName>.dpr
<SampleName>.exe
```

Or open the `.dpr` in RAD Studio and press F9.

The hardware-dependent samples (`01`, `02`, `03`, `28`, `29`) need an
actual adapter or vehicle. The catalog / parser / dry-run samples
(`00`, `04`, `05`, `27`) run headless on any machine.

## Use the wizard for the rest

The starters in File → New → Other → **Delphi-OBD** scaffold real
projects with components pre-wired:

| Category | Starter | Closest sample |
|---|---|---|
| Service-mode | DTC reader / reset | (wizard only) |
| Service-mode | VIN reader | [`03-ReadVIN`](03-ReadVIN/) |
| Service-mode | Live data dashboard | (wizard only) |
| Service-mode | Freeze frame inspector | (wizard only) |
| Service-mode | Onboard monitor | (wizard only) |
| Service-mode | Actuator test | (wizard only) |
| Service-mode | Full diagnostics suite | (wizard only, multi-tab) |
| Coding | Coding session (DIDs + security access) | (wizard only) |
| Coding | Security access harness | (wizard only) |
| Coding | Routine control runner | (wizard only) |
| Coding | UDS WriteMemory | (wizard only) |
| Coding | KWP WriteID | (wizard only) |
| Coding | OEM component protection (multi-OEM) | (wizard only) |
| Flashing | Flash session (UDS pipeline) | [`27-FlashDryRun`](27-FlashDryRun/) and [`28-FlashSignedFirmware`](28-FlashSignedFirmware/) |
| Flashing | Simple flasher | (wizard only) |
| Flashing | ECU uploader | (wizard only) |
| Flashing | J1939 memory access | [`29-J1939Flash`](29-J1939Flash/) |
| Calibration | XCP master | (wizard only) |
| Calibration | CCP master | (wizard only) |
| Calibration | IsoBus implement | (wizard only) |
| Network | DoIP client | (wizard only) |
| Network | SecOC wrap / unwrap | [`05-SecOC-WrapUnwrap`](05-SecOC-WrapUnwrap/) |
| Tooling | Recorder / replayer workbench | [`07-RecordReplay`](07-RecordReplay/) |
| Tooling | Protocol mock from recording | (wizard only) |
| Suite | Full Delphi-OBD suite (kitchen sink) | (wizard only) |

Drop one onto a fresh project, the wizard wires the components and
hands you back a running form to extend.

## Hardware notes

- **Wi-Fi ELM327**: defaults assume `192.168.0.10:35000`. Override
  in each sample's source if your adapter is on a different IP.
- **Bluetooth**: pair the adapter first via the OS Bluetooth
  settings, then pass the device address to
  `Connection.BluetoothSettings.DeviceAddress`.
- **Serial**: see the COM-port property editor in the design-time
  package — it enumerates live ports automatically.
- **Flashing**: never start a sample 28 / 29 run without a stable
  external charger and a confirmed-known-good signed image. **Read
  [`../docs/flashing-safety.md`](../docs/flashing-safety.md).**
