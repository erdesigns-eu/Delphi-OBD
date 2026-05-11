# Tachograph — EU 165/2014 digital tachograph

Walk-through of `TOBDTachograph` (VU diagnostics) and
`TOBDTachoPCSC` (driver / workshop card authentication via
PC/SC).

```sh
dcc32 -B Tachograph.dpr
Tachograph
```

See `catalogs/tacho/data-types.json` for the ASN.1-derived
field schemas used to decode activity / event / fault /
calibration records.
