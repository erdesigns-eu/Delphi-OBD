# 11-DriveCycleAdvise

Offline demo of the drive-cycle advisor — two parts:

1. **Catalogue dump.** Loads
   `catalogs/drive-cycle-generic.json` (ISO 15031-7) and
   prints the cycle steps + total duration for every
   registered monitor (Catalyst, EVAP, Oxygen Sensor,
   PM Filter, etc.).
2. **Readiness decoder.** Feeds a synthetic 4-byte Mode 01
   PID 01 payload to `TOBDDriveCycleAdvisor.DecodePID01` and
   prints the supported / complete state of each monitor.

Both run offline; no vehicle / adapter required.

## Build & run

```cmd
dcc32 -B DriveCycleAdvise.dpr
DriveCycleAdvise
```

## Expected output (truncated)

```
Delphi-OBD drive-cycle advisor demo

Registered drive cycles
------------------------------------------------------------
Misfire                 600 s total
  step 1 ( 600 s):  Cold start, idle 30 s, accelerate to 90 km/h, ...

Catalyst                420 s total
  step 1 ( 420 s):  Two stabilised cruises at 65 km/h for 3 min ...

...

Readiness decode demo (synthetic Mode 01 PID 01)
------------------------------------------------------------
  Misfire                supported=yes  complete=True
  FuelSystem             supported=yes  complete=True
  Comprehensive          supported=yes  complete=True
  Catalyst               supported=yes  complete=False
  EvaporativeSystem      supported=yes  complete=True
```

## In a real app

Drop a `TOBDDriveCycleAdvisor` on a form, wire it to a
`TOBDProtocol`, set `TargetMonitor`, and call `Start`. The
advisor polls Mode 01 PID 01 on `PollIntervalMs` and fires
`OnReadiness` / `OnReady` / `OnStep` events as the driver
runs the cycle. See `samples/03-ReadVIN` for the full
connection chain.
