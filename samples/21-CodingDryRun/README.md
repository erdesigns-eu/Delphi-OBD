# 21 — Coding dry-run

Demonstrates wiring `TOBDCodingSession` in `DryRun = True` mode.
The session walks every queued step, fires the `OnStepWritten`
event, writes a JSONL audit-log entry, but **does not** send
anything on the wire. Useful for inspecting what a coding apply
would do before flipping `AutoExecute` against a real ECU.

What the sample shows:

- Constructing a `TOBDCodingAuditLog` and opening it (HMAC key
  zeroed → no signing).
- Setting the session safety knobs: `DryRun`, `AutoExecute`,
  `VerifyAfterWrite`, `RollbackOnFail`.
- Building a `TArray<TOBDCodingStep>` with `DID` and `NewValue`.

```sh
dcc32 -B CodingDryRun.dpr
CodingDryRun
```

To actually run the dry-run, assign `Session.Protocol` to a real
`TOBDProtocol` and call `Session.Apply(Steps)`. The sample stops
short of that step so it stays bus-free.
