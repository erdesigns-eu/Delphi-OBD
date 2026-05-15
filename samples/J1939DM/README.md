# J1939DM — DM1..DM32 decoder

Demonstrates `TOBDJ1939DM` decoding a synthetic two-DTC DM1
payload into structured SPN/FMI/CM/OC entries with a decoded
MIL flag.

```sh
dcc32 -B J1939DM.dpr
J1939DM
```

In a real app, wire your bus listener (via `TOBDJ1939.OnFrame`
or `Sessions.OnComplete`) to call `DM.DispatchDM(PGN, body)` for
every DM-class PGN.
