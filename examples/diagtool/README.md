# Delphi-OBD Diagnostic Tool

A reference VCL application built on top of the framework. Demonstrates the
end-to-end flow a real diagnostic tool follows:

1. **Connect** to an ELM327-compatible adapter on a COM port.
2. **Read VIN** via OBD-II Service 09 PID 02; the OEM extension is
   auto-detected from the WMI prefix using `TOBDOEMRegistry.FindByVIN`.
3. **Enter extended session** via `TOBDDiagSession.BeginSession` —
   the OEM-specific choreography (VW SH+CRA / BMW E-Sys / Mercedes
   XENTRY F198 / Ford ST 32 / GM SP 6 / Stellantis F198) is applied
   transparently.
4. **Live data** — pull battery voltage / RPM / speed / coolant
   temperature via standard Service 01 PIDs.
5. **DTCs** — read stored codes via Service 03; selecting a code in the
   list shows the OEM catalog entry (description + severity + possible
   causes + repair hints + source + verified flag).
6. **DIDs** — pick a DID from the OEM's catalog, read it, and see the
   decoded result. The combo-box lists every DID the OEM extension knows
   about (uds-standard.json universal entries + per-OEM overlay).
7. **Routines** — pick a routine from the catalog, send `31 01` start.
8. **End session** — back to default; heartbeat thread stops cleanly.

## Build

1. Add `src/Services` and `src/Connection` to the project's library path.
2. Add the runtime + design-time packages (`Packages/RunTime.dpk`,
   `Packages/DesignTime.dpk`) per the main README.
3. Open `examples/diagtool/DiagTool.dpr` in the IDE and build.

The form is constructed **programmatically** (no `.dfm`) so the project
has just two files: `DiagTool.dpr` + `DiagTool.MainForm.pas`. Drop them
into any Delphi 11/12 VCL project to use as a starter template.

## Architecture

```
TDiagToolMainForm
  ├── TPanel         (connection: port + baud + Connect/Disconnect)
  ├── TPanel         (info: VIN + OEM + session state + Read VIN /
  │                   Extended → / End Session buttons)
  └── TPageControl
        ├── Live Data       (battery / RPM / speed / coolant via
        │                    OBD-II Service 01 PIDs)
        ├── DTCs            (Service 03 read + per-DTC catalog detail)
        ├── DIDs            (combo-box of OEM-catalogued DIDs +
        │                    DecodeDID output)
        └── Routines        (combo-box of OEM-catalogued routines +
                             RoutineControl 31 01 start)
```

## Limitations

- Single-ECU model: targets the engine ECU at `0x7E0` for extended
  session and DID reads. Multi-ECU UI is a natural follow-up.
- No SecurityAccess UI: the framework's `TOBDDiagSession.UnlockSecurityAccess`
  is exposed but the tool doesn't surface a "Unlock Level X" button. Add
  one if your workflow needs it.
- Synchronous reads on the UI thread for simplicity. Production tools
  use the async API the framework provides (`TOBDConnectionAsync`)
  but route results through `TThread.Synchronize`.
- All catalog entries surface as-is; production tools filter
  `Entry.Verified` for safety-critical operations.
