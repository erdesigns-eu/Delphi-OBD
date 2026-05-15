# DTCReader — DTC decode + clear-safety demo

- Exercises the `TOBDDTCs.DecodeJ2012` helper on each prefix
  (P / C / B / U).
- Prints the `TOBDClearDTC` safety-contract defaults — `AutoExecute`
  starts `False`, the dialect defaults to OBD-II Mode 0x04, and
  the UDS group selector defaults to all-groups.

```sh
dcc32 -B DTCReader.dpr
DTCReader
```

To actually read DTCs, wire `TOBDDTCs.Protocol` to a connected
`TOBDProtocol` and call `ReadConfirmed` / `ReadPending` /
`ReadPermanent`. To clear, drop a `TOBDClearDTC`, set
`AutoExecute := True`, and call `Clear`.
