# 26 — Coding rollback semantics

Explains how `TOBDCodingSession` rolls back on verify-failure.
The session runs each step as

```
snapshot(DID)  →  write(DID, NewValue)  →  verify(DID)
```

If any verify fails (or any write returns a negative response),
the session walks every previously-committed step in reverse and
writes each step's captured `OldValue` back. The outcome property
moves to `coRolledBack` and the audit log records every action.

This sample does not drive a real ECU — it documents the contract
and prints the property values needed to enable the behaviour.
See sample 21 for dry-run wiring and sample 22 for production
wiring with `AutoExecute = True`.

```sh
dcc32 -B CodingRollback.dpr
CodingRollback
```
