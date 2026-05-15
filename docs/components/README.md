# Per-component documentation index

This directory holds per-component documentation for every shipped
Delphi-OBD component. The canonical reference is two-fold:

1. **XMLDoc in source.** Every public symbol on every component
   carries a `<summary>` / `<param>` / `<returns>` / `<remarks>` /
   `<exception cref="…">` block at its declaration. Press Ctrl+Shift+G
   in the RAD Studio editor on any component name to jump to the
   declaration and read the live contract.

2. **`../components.md`.** One-paragraph summary per component on a
   single page, grouped by family (Foundation, Service-mode,
   Coding, Flashing, Calibration & Speciality, Diagnostics,
   Recorder / Replayer). Useful for skimming the surface before
   choosing what to drop on the form.

Per-component anchors below link straight into the consolidated
page.

## Index

### Foundation
- [`TOBDConnection`](../components.md#tobdconnection--obdconnection)
- [`TOBDAdapter`](../components.md#tobdadapter--obdadapter)
- [`TOBDProtocol`](../components.md#tobdprotocol--obdprotocol)

### Service-mode
- [`TOBDLiveData`](../components.md#tobdlivedata--obdservicelivedata)
- [`TOBDDTCs`](../components.md#tobddtcs--obdservicedtcs)
- [`TOBDVIN`](../components.md#tobdvin--obdservicevin)
- [`TOBDFreezeFrame`](../components.md#tobdfreezeframe--obdservicefreezeframe)
- [`TOBDOnBoardMonitor`](../components.md#tobdonboardmonitor--obdserviceonboardmonitor)
- [`TOBDActuator`](../components.md#tobdactuator--obdserviceactuator)
- [`TOBDClearDTC`](../components.md#tobdcleardtc--obdcleardtc)
- [`TOBDOxygenMonitor`](../components.md#tobdoxygenmonitor--obdoxygenmonitor)
- [`TOBDPIDList`](../components.md#tobdpidlist--obdpidlist)
- [`TOBDDataSource`](../components.md#tobddatasource--obddatasource)
- [`TOBDWWHOBD`](../components.md#tobdwwhobd--obdwwhobd)
- [`TOBDWWHReadiness`](../components.md#tobdwwhreadiness--obdwwhobdreadiness)

### Diagnostics (Phase 6)
- [`TOBDUDS`](../components.md#tobduds--obddiagnosticsuds)
- [`TOBDUDSReset`](../components.md#tobdudsreset--obddiagnosticsudsreset)
- [`TOBDUDSReadMemory`](../components.md#tobdudsreadmemory--obddiagnosticsudsreadmemory)
- [`TOBDUDSIOControl`](../components.md#tobdudsiocontrol--obddiagnosticsudsiocontrol)
- [`TOBDUDSReadDID`](../components.md#tobdudsreaddid--obddiagnosticsudsreaddid)
- [`TOBDUDSReadDTC`](../components.md#tobdudsreaddtc--obddiagnosticsudsreaddtc)
- [`TOBDUDSReadByPeriodic`](../components.md#tobdudsreadbyperiodic--obddiagnosticsudsperiodic)
- [`TOBDUDSDynamicDID`](../components.md#tobdudsdynamicdid--obddiagnosticsudsdynamicdid)
- [`TOBDKWP`](../components.md#tobdkwp--obddiagnosticskwp)
- [`TOBDKWPReadID`](../components.md#tobdkwpreadid--obddiagnosticskwpreadid)
- [`TOBDKWPReadDTC`](../components.md#tobdkwpreaddtc--obddiagnosticskwpreaddtc)
- [`TOBDKWPIOControl`](../components.md#tobdkwpiocontrol--obddiagnosticskwpiocontrol)
- [`TOBDKWPRoutine`](../components.md#tobdkwproutine--obddiagnosticskwproutine)
- [`TOBDJ1939`](../components.md#tobdj1939--obddiagnosticsj1939)
- [`TOBDJ1939DM`](../components.md#tobdj1939dm--obddiagnosticsj1939dm)
- [`TOBDOEMCatalog`](../components.md#tobdoemcatalog--obdoemcatalog)

### Coding
- [`TOBDSecurityAccess`](../components.md#tobdsecurityaccess--obdcodingsecurityaccess)
- [`TOBDDataIdentifierIO`](../components.md#tobddataidentifierio--obdcodingdataidentifierio)
- [`TOBDRoutineControl`](../components.md#tobdroutinecontrol--obdcodingroutinecontrol)
- [`TOBDFlasher`](../components.md#tobdflasher--obdcodingflasher)
- [`TOBDUploader`](../components.md#tobduploader--obdcodinguploader)
- [`TOBDFlashSession`](../components.md#tobdflashsession--obdcodingflashsession)
- [`TOBDUDSWriteMemory`](../components.md#tobdudswritememory--obdudswritememory)
- [`TOBDKWPWriteID`](../components.md#tobdkwpwriteid--obdkwpwriteid)
- [`TOBDCodingAuditLog`](../components.md#tobdcodingauditlog--obdcodingauditlog)
- [`TOBDCodingSession`](../components.md#tobdcodingsession--obdcodingsession)
- [`TOBDComponentProtectionVAG / BMW / Mercedes / Stellantis`](../components.md#tobdcomponentprotectionvag--bmw--mercedes--stellantis)

### Flashing
- [`TOBDUDSTransfer`](../components.md#tobdudstransfer--obdudstransfer)
- [`TOBDVoltageGate`](../components.md#tobdvoltagegate--obdflashvoltagegate)
- [`TOBDFlashPipeline`](../components.md#tobdflashpipeline--obdflashpipeline)

### Calibration & Speciality
- [`TOBDXCP`](../components.md#tobdxcp--obdcalibrationxcp)
- [`TOBDCCP`](../components.md#tobdccp--obdcalibrationccp)
- [`TOBDIsoBus`](../components.md#tobdisobus--obdspecialityisobus)

### Networking
- [`TOBDDoIPClient`](../components.md#tobddoipclient--obdprotocoldoipclient)
- [`TOBDSecOCCodec`](../components.md#tobdsecoccodec--obdprotocolsecoc)

### Recorder / Replayer
- [`TOBDRecorder`](../components.md#tobdrecorder--obdrecorder)
- [`TOBDReplayer`](../components.md#tobdreplayer--obdreplayer)
- [`TOBDProtocolMock`](../components.md#tobdprotocolmock--obdrecorderprotocolmock)
- [`TOBDLogRedactor`](../components.md#tobdlogredactor--obdrecorderredactor)

## Why a single page rather than 47 files?

The component surface is small (one paragraph per component covers
the contract) but cross-references are heavy: most components plug
into the same `TOBDProtocol` / `TOBDConnection` / `TOBDAdapter`
backbone, and the safety-gate / async pattern is shared. A single
page lets readers see the full picture in one scroll. The
`<ClassName>` anchor convention keeps each component
deep-linkable, both from this index and from the help-keyword
collection registered by `OBD.Design.Help`
(`delphi-obd:<ClassName>` →
`docs/components.md#<classname>--<unit>`).
