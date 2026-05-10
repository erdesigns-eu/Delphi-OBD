# 08-DecodeVIN

Offline VIN decoder demo. Feeds a curated set of real-world
VINs through `TOBDVINDecoder` + the vPIC rules under
`catalogs/vin/` and prints the full breakdown (region /
country / manufacturer / model year / plant / serial / vehicle
features).

No vehicle / adapter required.

## Build & run

```cmd
dcc32 -B DecodeVIN.dpr
DecodeVIN
```

## Expected output (truncated)

```
Delphi-OBD VIN decoder demo
------------------------------------------------------------
VIN: WVWZZZ1KZ7W123456
  WMI / VDS / VIS  : WVW / ZZZ1KZ / 7W123456
  region           : Europe
  country          : Germany
  manufacturer     : Volkswagen
  check digit      : Z  valid? False
  model year       : 2007  (candidates: 1987 2007)
  plant            : Wolfsburg Plant  Wolfsburg, Germany
  serial           : 123456
...
```

## Why useful

- Shows the canonical `Decode → TOBDVINInfo` flow.
- Demonstrates that VINs without a valid NA check digit still
  decode (Europe-built VINs commonly use `Z` as a filler).
- Demonstrates VDS feature decode against the bundled
  ~24,000 vPIC schemas.
