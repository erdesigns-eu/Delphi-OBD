# 14-EVBatteryDecode

Loads the per-vendor EV battery management decode catalogues
and inventories what's decodable per platform. For each
shipped vendor, prints the ECU CAN IDs and the field list
(which DIDs / PIDs the catalogue knows how to decode into
`TOBDEVBatterySnapshot`).

Live polling needs a real EV; this sample is the offline
catalogue inspector that helps you pick a vendor key before
you wire `TOBDEVBattery.Protocol`.

## Build & run

```cmd
dcc32 -B EVBatteryDecode.dpr
EVBatteryDecode
```

## Expected output (truncated)

```
EV battery catalogue inventory
------------------------------------------------------------
vendor=hmg  (Hyundai / Kia EV (Kona EV, Niro EV, ...))
  ECU req=$07e4  rsp=$07ec
  models   : Hyundai Kona EV, Hyundai Ioniq 5, ...
  rules    : 31
    soc_bms                    svc=$22  did/pid=$0101  +4/1  %
    soh                        svc=$22  did/pid=$0105  +25/2 %
    pack_voltage               svc=$22  did/pid=$0101  +12/2 V
    ...
------------------------------------------------------------
vendor=nissan  (Nissan Leaf (Gen 1 + Gen 2))
  ECU req=$079b  rsp=$07bb
  ...
```

## In a real app

```pascal
EVBattery.Vendor   := 'hmg';
EVBattery.Protocol := MyOBDProtocol;
Snap := EVBattery.ReadSnapshot;
if Snap.HasSOC      then lblSOC.Caption    := Format('%.1f %%', [Snap.SOC]);
if Snap.HasSOH      then lblSOH.Caption    := Format('%.1f %%', [Snap.SOH]);
if Snap.HasCellVoltages then GridCells.LoadFromArray(Snap.CellVoltages);
```
