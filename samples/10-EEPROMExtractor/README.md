# 10-EEPROMExtractor

Demonstrates the EEPROM-dump extractor family by building a
synthetic 128-byte dump that mimics a Volvo HU's 24C01
serial-EEPROM, then feeding it to
`TOBDRadioCodeEEPROM_VolvoHU`. The extractor reads the
4 ASCII digits at offset 0x90 and returns the unlock code.

In production the dump comes from a chip programmer
(CH341A, TL866, Willem, etc.) — this sample fakes the
physical step so the example runs without hardware.

## Build & run

```cmd
dcc32 -B EEPROMExtractor.dpr
EEPROMExtractor
```

## Expected output

```
Delphi-OBD EEPROM extractor demo
------------------------------------------------------------
Wrote synthetic dump (128 bytes, code=4271 at 0x90):
  C:\Users\...\Local\Temp\volvo-hu-demo.bin

Decoded:
  vendor : volvo-hu
  code   : 4271
  raw    : 34 32 37 31
------------------------------------------------------------
Done.
```

## Two more extractors ship in the same family

| Component | EEPROM | Offset | Encoding |
|---|---|---|---|
| `TOBDRadioCodeEEPROM_VolvoHU` (this sample) | 24C01 | 0x90 | 4 ASCII digits |
| `TOBDRadioCodeEEPROM_OpelCD30` | 24C32/95640 | 0x2B7 | 2 BCD bytes |
| `TOBDRadioCodeEEPROM_MercedesBecker` | 24C02 | 0x76 | 5 ASCII digits |

Hosts whose radio variant uses a different offset wire
`OnExtract` and decode themselves.
