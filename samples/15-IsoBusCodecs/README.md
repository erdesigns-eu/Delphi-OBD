# 15-IsoBusCodecs

Exercises the ISO 11783 (ISOBUS) codec helpers from
`src/Speciality/`. Pure class-static functions that build /
decode 8-byte CAN payloads — useful in any agricultural-CAN
host without needing a bus.

Demonstrates:

| Helper | What it does |
|---|---|
| `TOBDIsoBus.EncodeName / DecodeName` | 64-bit ISO NAME packing |
| `TOBDIsoBusVT.BuildGetMemory / BuildAudioSignal / ...` | Virtual Terminal request frames |
| `TOBDIsoBusTC.BuildValue / BuildSetValue / ...` | Task Controller process-data frames |
| `TOBDIsoBusFS.BuildGetProperties / BuildCloseFile / ...` | File Server request frames |
| `TOBDIsoBusGNSS.DecodePositionRapid / DecodeCogSog` | NMEA 2000 PGN decode |

## Build & run

```cmd
dcc32 -B IsoBusCodecs.dpr
IsoBusCodecs
```
