# 16-TachographDecode

Demonstrates `TOBDTachograph` — decoder helpers for the
digital-tachograph data records mandated by EU Regulation
165/2014. All helpers are pure class-static functions on
`TBytes`; the sample feeds them synthetic record payloads so
the example runs without a smartcard reader.

In production hosts read records from a driver / company /
workshop / control card via PCSC (`TOBDTachoPCSC`,
Windows-only) or directly from a vehicle unit, then pipe the
bytes through the same decoders.

## Build & run

```cmd
dcc32 -B TachographDecode.dpr
TachographDecode
```

## Decoder surface

| Function | Decodes |
|---|---|
| `EncodeTimeReal / DecodeTimeReal` | ISO C `time_t` (seconds since 1970 UTC) |
| `DecodeActivity` | Activity slots (driving / rest / available / work) |
| `DecodeEvent / DecodeFault` | Faults and events log |
| `DecodeCalibration` | Workshop calibration block |
| `DecodeString` | Codepage-aware string fields |
