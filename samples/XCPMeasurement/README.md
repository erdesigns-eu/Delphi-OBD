# XCPMeasurement — XCP DAQ list streaming

Documents the canonical "set up a DAQ list and start streaming"
call sequence on `TOBDXCP`. PLAN line 703 (TOBDXCPMeasurement)
is covered by `TOBDXCP`'s DAQ helpers — no separate measurement
component.

```sh
dcc32 -B XCPMeasurement.dpr
XCPMeasurement
```
