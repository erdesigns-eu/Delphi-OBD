# 09-RadioCodeMulti

Demonstrates three of the bundled radio-code calculators —
each with a different style of algorithm:

| Calculator | Algorithm |
|---|---|
| `TOBDRadioCodePeugeot` | per-digit transform with offset + wrap |
| `TOBDRadioCodeHyundai` | `(serial + 1212) mod 10000` (2002+) |
| `TOBDRadioCodeBecker4` | flat lookup against a 10,000-entry JSON DB |

The Becker4 lookup needs `catalogs/radio-code/becker4.json`
reachable from the working directory; copy `catalogs/` next to
the executable, or point the host process there at start-up.

## Build & run

```cmd
dcc32 -B RadioCodeMulti.dpr
RadioCodeMulti
```

## Expected output

```
Delphi-OBD radio-code calculator demo
------------------------------------------------------------
  Peugeot              in=1234     ->  code=2345
  Peugeot              in=0000     ->  code=1111
  Peugeot              in=ABCD     ->  Input must be 4 digits
  Hyundai              in=1234     ->  code=2446
  Hyundai              in=9000     ->  code=0212
  Becker (4-digit ...) in=0000     ->  code=1010
  Becker (4-digit ...) in=1234     ->  code=2435
  Becker (4-digit ...) in=9999     ->  code=7532
------------------------------------------------------------
Done.
```

The other 44 vendors live in the same family
(`OBD.RadioCode.{EuropeanPremium,British,American,Aftermarket,
Volvo,FordV}`). Most ship as validate-only stubs — the host
wires `OnCalculate` against a paid-service or licensed
algorithm.
