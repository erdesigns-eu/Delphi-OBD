# XCPCalibration — XCP online parameter editing

Walks the SetCalPage / SetMTA / Download sequence on `TOBDXCP`.
PLAN line 704 (TOBDXCPCalibration) is covered by `TOBDXCP`'s
SetCalPage / GetCalPage / Download / ShortDownload methods —
no separate calibration component.

```sh
dcc32 -B XCPCalibration.dpr
XCPCalibration
```
