# 27-FlashDryRun

Walks the entire `TOBDFlashPipeline` phase chain **without touching
the wire**. Set `OnConfirmExecute(Allow := False)` and the pipeline
runs every preflight check, fires every phase event, captures the
full audit trail — but aborts before any `0x34 RequestDownload`
leaves the host.

Use this as a "review before commit" tool for new flash
configurations: wire your preflight checks, voltage source,
vendor handshake, audit log, and verify routine the same way as
in production, then run the dry-run to see exactly what the
real flash would have done.

## Build

```
dcc32 -B FlashDryRun.dpr
```

## Run

```
FlashDryRun
```

No bus connection needed. Expected output:

```
--- Pre-flight checks ---
  phase → fpPreflight
  check fpPreflight/engine_off = True
  check fpPreflight/voltage_floor = True
  ...

--- OnConfirmExecute → cancel ---
  Pipeline aborted: TOBDFlashPipeline: cancelled by OnConfirmExecute / AutoExecute

Confirmation handler fired: True
```

Read **`docs/flashing-safety.md`** before pointing the same
configuration at a real ECU with `Allow := True`.
