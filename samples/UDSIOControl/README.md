# UDSIOControl — Service 0x2F

Demonstrates `TOBDUDSIOControl`. Destructive — drives ECU outputs;
ships with `AutoExecute = False` and an `OnBeforeSend` hook for
final confirmation.

```sh
dcc32 -B UDSIOControl.dpr
UDSIOControl
```
