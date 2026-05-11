# UDSReadDTC — Service 0x19

Demonstrates `TOBDUDSReadDTC` and the J2012 P / C / B / U decoder.

```sh
dcc32 -B UDSReadDTC.dpr
UDSReadDTC
```

The component handles every sub-function of Service 0x19; the
convenience helpers (`ReadByStatusMask`, `ReadSupportedDTCs`,
`ReadByDTCNumber`) cover the common cases. Use `Send(subFunc,
body)` for anything else and listen on `OnRaw`.
