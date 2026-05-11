# WWHOBD — World-Wide Harmonised OBD (ISO 27145)

Demonstrates the two heavy-duty OBD components:

- `TOBDWWHOBD` — read DTCs by severity (UDS 0x19 sub 0x42) and
  by readiness-group (sub 0x55); MIL distance + time counters
  (DIDs 0xF402 / 0xF407); Class A / Class B1 DTC counters
  (DIDs 0xF418 / 0xF419); VIN via 0xF190.
- `TOBDWWHReadiness` — major-group readiness summary
  (DID 0xF411) and OBD-monitoring-conditions encountered ratios
  (DID 0xF40C) per group.

The sample exercises the `ClassifySeverity` helper for every
severity-byte combination defined by ISO 14229-1 §11.3.5.4 and
shows that the components reject calls without a connected
`TOBDProtocol`.

```sh
dcc32 -B WWHOBD.dpr
WWHOBD
```
