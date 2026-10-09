# FreezeFrame — Mode 0x02 freeze-frame snapshot

Tiny walkthrough for `TOBDFreezeFrame`. Shows the safety-gate
behaviour (read with no Protocol raises `EOBDConfig`) and the
canonical wiring pattern for capturing the freeze-frame state of
a stored emission DTC.

```sh
dcc32 -B FreezeFrame.dpr
FreezeFrame
```
