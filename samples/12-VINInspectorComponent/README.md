# 12-VINInspectorComponent

Drop-on-form demo of `TOBDVINInspector` — the non-visual VIN
decoder component. Set `VIN`, read each detail via the
per-field shortcut properties (`ManufacturerName`,
`ModelYear`, `PlantName`, `BodyStyle`, `IsCommercial`, …).
`AutoDecode = True` (default) triggers `OnDecoded` whenever
`VIN` changes.

Mirrors what a real form would do, but in a console so the
sample runs without a UI.

## Build & run

```cmd
dcc32 -B VINInspectorComponent.dpr
VINInspectorComponent
```

## In a real form

```pascal
procedure TForm1.VINInspector1Decoded(Sender: TObject;
  const AInfo: TOBDVINInfo);
begin
  lblManufacturer.Caption := VINInspector1.ManufacturerName;
  lblYear.Caption         := IntToStr(VINInspector1.ModelYear);
  lblBody.Caption         := VINInspector1.BodyStyle;
end;
```
