# UDSReadDID — Service 0x22

Demonstrates `TOBDUDSReadDID` for batched multi-DID reads.

```sh
dcc32 -B UDSReadDID.dpr
UDSReadDID
```

In a real app, wire `R.Protocol`, set `R.OnRead`, and call
`R.Read([DID1, DID2], [Len1, Len2])` — the component encodes the
multi-DID request and splits the variable-length response per
expected-length entry.
