# Migration from v1

v1 of Delphi-OBD shipped as a class library — `TOBDInterface`,
`TOBDProtocol`, …, hand-wired in code. v2 is a component-first RAD
package: drop on a form, set properties in the Object Inspector.
This page maps v1 idioms to v2 components.

## Conceptual shift

| v1 | v2 |
|---|---|
| `TOBDInterface.Create` + manual transport setup | Drop a `TOBDConnection`, set `*Settings.*` in the Inspector |
| Hand-written AT command sequence | `TOBDAdapter.Detect` + `InitCommands: TStrings` |
| Raw text parsing | `TOBDProtocol.Send(Request)` returns `TOBDResponse` |
| One-shot value reads | `TOBDLiveData.Poll([…], interval)` polls and fires `OnValue` |
| OBD log file format | `TOBDRecorder` / `TOBDReplayer` |
| Per-class PID/DTC tables in source | JSON catalogs under `catalogs/` |

## Field-by-field cookbook

### Opening the wire

**v1:**
```pascal
OBD := TOBDInterface.Create;
OBD.Port := 'COM3';
OBD.Open;
```

**v2:**
```pascal
Connection.SerialSettings.Port := 'COM3';
Connection.Active := True;
```

The async equivalent:

```pascal
Connection.OnOpenComplete := HandleOpened;
Connection.OnError := HandleError;
Connection.OpenAsync;
```

### Adapter detection

**v1:** Manually issue `ATZ`, `ATE0`, `ATI`, parse strings, populate
`Self.Adapter` based on regex matches.

**v2:**
```pascal
Adapter.Connection := Connection;
Adapter.Detect;                // sync; or Adapter.DetectAsync
// Adapter.Family, Adapter.Capabilities, Adapter.MaxIsoTpFrameBytes are now populated
```

### Reading a PID

**v1:**
```pascal
Reply := OBD.SendCommand('0105');
ParseAndDecodeCoolantTemp(Reply);  // host code
```

**v2:**
```pascal
Protocol.Adapter := Adapter;

LiveData.Protocol := Protocol;
LiveData.OnValue := procedure(Sender: TObject; APid: Byte;
                               const AValue: TOBDValue)
begin
  if APid = $05 then
    edtCoolant.Text := Format('%.0f °C', [AValue.AsFloat]);
end;
LiveData.Poll(TBytes.Create($05), 250);
```

The PID is decoded via the `obd-pids.json` catalog. Adding a custom
PID is a JSON edit; see [`catalogs.md`](catalogs.md).

### Reading DTCs

**v1:**
```pascal
Reply := OBD.SendCommand('03');
DTCs := ParseDTCResponse(Reply);
```

**v2:**
```pascal
DTCs.Protocol := Protocol;
Codes := DTCs.Read;            // TArray<string>: P0420, B1234, ...
DTCs.Clear;                     // Mode 04
```

### Reading the VIN

**v1:**
```pascal
Reply := OBD.SendCommand('0902');
VIN := ExtractVIN(Reply);
```

**v2:**
```pascal
VIN.Protocol := Protocol;
edtVIN.Text := VIN.Read;        // legacy 0902 OR UDS DID F190 — auto
```

### UDS coding

**v1:** Hand-written security-access seed/key + WriteDataByIdentifier.

**v2:**
```pascal
SecurityAccess.Protocol := Protocol;
SecurityAccess.RequestSeedLevel := $01;
SecurityAccess.OnComputeKey := procedure(Sender: TObject;
                                         const ASeed: TBytes;
                                         var AKey: TBytes)
begin
  AKey := MyHostSeedToKey(ASeed);
end;
SecurityAccess.Unlock;

DataIO.Protocol := Protocol;
DataIO.AutoExecute := False;
DataIO.OnConfirmExecute := procedure(Sender: TObject; var Allow: Boolean)
begin
  Allow := UserConfirmedInUI;
end;
DataIO.WriteDID($F190, NewVINBytes);
```

### Flashing

**v1:** Hand-rolled 0x34 / 0x36 / 0x37 sequence.

**v2:** `TOBDFlashPipeline` does the whole thing safely:

```pascal
Pipeline.Protocol    := Protocol;
Pipeline.AuditLog    := AuditLog;
Pipeline.VoltageGate := VoltageGate;
Pipeline.AutoExecute := False;
Pipeline.OnConfirmExecute := HandleConfirm;
Pipeline.OnPhaseChange    := HandlePhase;
Pipeline.Run(ImageStream, ImageHash);
```

The pipeline runs preflight checks (engine off, voltage floor, ambient
temperature, ignition, user confirmation), verifies the image
(hash + signature + applicability), enters programming, transfers,
verifies, resets, and finalises an audit-log envelope. **Read
[`flashing-safety.md`](flashing-safety.md) before going live.**

## Removed v1 features

| Removed | Why | Where it lives now |
|---|---|---|
| `TOBDInterface` (god class) | Replaced by chain of single-purpose components | `TOBDConnection` + `TOBDAdapter` + `TOBDProtocol` |
| `TOBDDataObject` | Per-PID classes replaced by JSON catalog descriptors | `catalogs/obd-pids.json` |
| Globals (`OBDx: TOBDInterface`) | RAD components are non-singleton by default | Drop on a form |
| In-source DTC / NRC text | JSON catalogs are easier to extend without recompile | `catalogs/obd-dtcs.json`, `catalogs/uds-nrcs.json` |
| `OnDataReceived` at the top level | Routed through the chain, not exposed at the top | `TOBDConnection.OnDataReceived` only |
| Sync-only API | Sync + async dual surface across every blocking operation | `Open` / `OpenAsync`, `Detect` / `DetectAsync`, `Send` / `SendAsync` |

## Things that didn't change

- **Catalog file format** — v1's `.obd` files load into v2 with the
  same field names. Existing PID / DTC contributions carry over.
- **AT / ST command vocabulary** — `Adapter.InitCommands` accepts the
  same strings v1 hand-rolled.
- **Wire formats** — same chips, same protocols, same NRC bytes.
  v2 is purely a re-skinning of the host API; the bus side is unchanged.

## When in doubt

- Open a sample under [`samples/`](../samples/). Each demonstrates a
  specific component or starter scenario end-to-end.
- Check the design-time wizard (File → New → Other → **Delphi-OBD**).
  Pick a starter close to your use case and inspect the generated
  code.
- Read the XMLDoc on the component in source — every public symbol
  is documented.
