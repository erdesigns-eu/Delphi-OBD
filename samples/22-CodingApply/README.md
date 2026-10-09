# 22 — Coding apply (canonical production wiring)

Companion to sample 21. Same session wiring, but with
`AutoExecute = True` and `DryRun = False` — the configuration you
ship to production. The sample stops short of calling
`Session.Apply(...)` so it remains hardware-free; the developer
must assign `Session.Protocol` to a real `TOBDProtocol` bound to
a vehicle before doing so.

Key safety properties demonstrated:

- `AutoExecute` defaults to `False`; setting it to `True` is
  explicit informed consent.
- `VerifyAfterWrite` re-reads each DID after writing to confirm
  the new value landed.
- `RollbackOnFail` restores the previously-captured snapshot on
  the first verify-fail.
- `AuditLog` records every step (snapshot, write, verify,
  rollback) as a JSONL row with an optional HMAC chain.

```sh
dcc32 -B CodingApply.dpr
CodingApply
```
