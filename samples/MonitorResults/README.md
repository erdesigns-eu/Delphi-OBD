# MonitorResults — Mode 0x05 / 0x06 monitor surfaces

- `TOBDOnBoardMonitor` (Mode 0x06) for ISO 15765-4 / CAN cars.
- `TOBDOxygenMonitor` (Mode 0x05) for the legacy non-CAN
  protocols (ISO 9141-2, KWP2000, J1850 PWM/VPW).

Use the right one for the vehicle's physical layer — on a CAN
car Mode 0x05 returns an empty supported-TID bitmap and oxygen-
sensor data flows through Mode 0x06 MIDs instead.

```sh
dcc32 -B MonitorResults.dpr
MonitorResults
```
