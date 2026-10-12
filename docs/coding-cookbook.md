# Coding cookbook

Walkthroughs for ECU coding with Delphi-OBD. The run-time components
(`TOBDUDS`, `TOBDSecurityAccess`, `TOBDDataIdentifierIO`,
`TOBDRoutineControl`, `TOBDCodingSession`, `TOBDCodingAuditLog`, the
component-protection and key-adaptation components) handle the wire side.
The per-OEM helper classes in `src/Coding/` (`TOBDCodingVAG`,
`TOBDCodingBMW`, `TOBDCodingMercedes`, `TOBDCodingStellantis`,
`TOBDCodingFord`, `TOBDCodingHMG`, `TOBDCodingHonda`, `TOBDCodingToyota`)
only parse and edit coding buffers in memory; they never touch the bus.

> **Heads-up.** Coding writes to the ECU. Mistakes can immobilize the
> vehicle, store new DTCs, or break safety functions. Test on a bench
> first whenever practical. Every write-side component ships with
> `AutoExecute = False` and refuses to write until you set it to `True`
> — do that only right before a write the user has confirmed.

DIDs, routine IDs and security levels differ per ECU, model year and
software version. The constants in the examples below (`CodingDID`,
`ApplyRoutine`, `CodingLevel`, …) are placeholders: take the real values
from the OEM catalogue (`catalogs/<vendor>.json`, `TOBDOEMCatalog`) or
the ECU's documentation.

## The shared scaffolding

Every coding flow starts with the usual chain, an extended diagnostic
session and security access:

```pascal
Connection.SerialSettings.Port := 'COM3';
Connection.Active := True;

Adapter.Connection := Connection;
Adapter.Detect;

Protocol.Adapter := Adapter;

UDS.Protocol := Protocol;
UDS.KeepAlive := True;               //  TesterPresent while the session is open
UDS.Open($03);                       //  extendedDiagnosticSession

SecurityAccess.Protocol := Protocol;
SecurityAccess.OnComputeKey := ComputeKey;
SecurityAccess.Unlock(CodingLevel);  //  odd requestSeed level; sendKey = level + 1

DataIO.Protocol := Protocol;
Routine.Protocol := Protocol;
```

The seed → key transform is vendor-specific and is never shipped with
Delphi-OBD. Supply it through `OnComputeKey` (or the functional
`SeedToKey` property, which takes precedence):

```pascal
procedure TForm1.ComputeKey(Sender: TObject; ALevel: Byte;
  const ASeed: TBytes; var AKey: TBytes);
begin
  AKey := MySeedToKey(ALevel, ASeed);  //  your licensed algorithm
end;
```

`OnComputeKey` runs on the thread that called `Unlock` / `UnlockAsync`,
so keep it thread-safe. `Unlock` raises `EOBDProtocolErr` when the ECU
returns a negative response.

## Writing coding safely: `TOBDCodingSession`

For anything beyond a single experimental write, use `TOBDCodingSession`.
It snapshots the current values, writes the new ones, reads them back and
rolls back to the snapshot when a write or verify fails. With an
`AuditLog` attached every step is logged.

```pascal
var
  Steps: TArray<TOBDCodingStep>;
begin
  SetLength(Steps, 1);
  Steps[0].DID := CodingDID;
  Steps[0].NewValue := NewCoding;

  Coding.Protocol := Protocol;
  Coding.AuditLog := AuditLog;          //  TOBDCodingAuditLog
  Coding.VerifyAfterWrite := True;
  Coding.RollbackOnFail := True;
  Coding.AutoExecute := True;           //  only after the user confirmed
  Coding.Apply(Steps);                  //  Steps[i].OldValue holds the snapshot

  case Coding.Outcome of
    coCommitted:  ShowMessage('Coding written and verified');
    coRolledBack: ShowMessage('Write failed; original coding restored');
    coFailed:     ShowMessage('Write failed; check the audit log');
  end;
end;
```

Set `DryRun := True` to take the snapshot and log the planned writes to
the audit log without writing. For a
plain write without the transaction, use `DataIO.Write(CodingDID,
NewCoding)` (also gated by `AutoExecute`). `TOBDWriteConfirm` (OBD
Studio Pro palette page) is a ready-made confirmation strip for the UI.

## Volkswagen / Audi / Skoda / SEAT / Cupra (VAG)

VAG UDS ECUs keep their long coding in a DID as a byte buffer;
`TOBDCodingVAG` edits it by byte and bit (bit 0 = LSB), and
`TOBDLabelFileVAG` parses VCDS-style `.LBL` label files that name the
bytes and bits.

```pascal
var
  Coding: TBytes;
  Labels: TOBDVAGLabelFile;
begin
  Coding := DataIO.ReadOne(CodingDID);
  Labels := TOBDLabelFileVAG.ParseFile('C:\Labels\5Q0-937-08x.lbl');
  //  Labels.Entries[i]: Byte_, BitStart / BitEnd, Description, Values

  TOBDCodingVAG.SetBit(Coding, 4, 0, True);   //  byte 4, bit 0
  Memo1.Text := TOBDCodingVAG.FormatLongCoding(Coding);
  //  write it back through TOBDCodingSession (see above)
end;
```

`TOBDCodingVAG.EncodeAdaptation` / `DecodeAdaptation` cover the
channel-based adaptations of the Mk4–Mk6 era. KWP1281 and KWP2000 ECUs
use `TOBDKWP1281Session` / `TOBDKWPWriteID` instead of UDS DIDs.

Component Protection (instrument cluster, infotainment, comfort module)
uses `TOBDComponentProtectionVAG`. It reads the CP status and challenge
and writes the authorisation through a `TOBDDataIdentifierIO`; the
challenge → authorisation transform comes from the dealer back-end
(GeKo / SVM) and is supplied by the host:

```pascal
CP_VAG.DataIO := DataIO;
CP_VAG.AuthFunc :=
  function(const AChallenge: TBytes): TBytes
  begin
    Result := MyDealerBackend.Authorise(AChallenge);
  end;
if CP_VAG.ReadStatus <> CP_STATUS_OK then  //  0 = not locked
  CP_VAG.Unlock;  //  raises EOBDProtocolErr if the lock does not clear
```

## BMW

BMW coding data (NCS / CAFD) is a list of parameter entries.
`TOBDCodingBMW` parses the entries, finds one by parameter ID and writes
a new value in place; `ParseVehicleOrder` reads the vehicle order (FA).

```pascal
var
  Buffer: TBytes;
  Entries: TArray<TOBDBMWCAFDEntry>;
  Entry: TOBDBMWCAFDEntry;
begin
  Buffer := DataIO.ReadOne(CodingDID);
  Entries := TOBDCodingBMW.ParseEntries(Buffer);
  if TOBDCodingBMW.FindEntry(Entries, ParameterID, Entry) then
    TOBDCodingBMW.WriteValue(Buffer, Entry, NewValue);
  //  write Buffer back through TOBDCodingSession, then run the
  //  ECU's apply routine if it needs one:
  Routine.AutoExecute := True;
  Routine.Start(ApplyRoutine);
end;
```

F / G / U-series vehicles are reached over Ethernet: put a
`TOBDDoIPClient` in front of the protocol instead of an ELM327 / OBDLink
chain. `TOBDComponentProtectionBMW` (status / challenge / authorisation
DIDs configurable) and `TOBDKeyAdaptationBMW` cover the immobiliser
side.

## Mercedes-Benz

`TOBDCodingMercedes` edits variant coding by bit and by field and
decodes / encodes the SCN (Software Calibration Number) field.
`TOBDComponentProtectionMercedes` handles the CP exchange.

```pascal
Buffer := DataIO.ReadOne(VariantCodingDID);
TOBDCodingMercedes.SetBit(Buffer, BitIndex, True);
ScnText := TOBDCodingMercedes.DecodeSCN(DataIO.ReadOne(ScnDID));
```

Recent models require online SCN authorisation from the OEM back-end;
Delphi-OBD only transports the bytes.

## Stellantis (FCA / PSA / Opel)

FCA / Stellantis modules share a per-vehicle configuration map
(Proxi). After a module swap, `TOBDCodingStellantis` reads and edits the
Proxi parameters so the new module can be aligned:

```pascal
Buffer := DataIO.ReadOne(ProxiDID);
TOBDCodingStellantis.SetParam(Buffer, ParameterID, NewValue);
```

`TOBDComponentProtectionStellantis` covers the protected modules.

## Ford / Lincoln

Ford modules carry AsBuilt data: named hex sections with a checksum at
the end of each line. Edit a byte and re-seal the checksum before you
write the section back:

```pascal
var
  Section: TOBDFordAsBuiltSection;
begin
  Section := TOBDCodingFord.ParseSection(AsBuiltLine);  //  e.g. '726-01-01 ...'
  TOBDCodingFord.SetByte(Section, 2, $1A);
  TOBDCodingFord.SealChecksum(Section);
  //  Section.Data is the new block; write it through TOBDCodingSession
end;
```

`TOBDKeyAdaptationFord` programs and erases keys (see below).

## Hyundai / Kia / Genesis (HMG)

HMG modules expose options as configuration words (1, 2 or 4 bytes)
addressed by a 16-bit option ID. `TOBDCodingHMG.GetOption` /
`SetOption` read and change them in the buffer.
`TOBDKeyAdaptationHMG` handles key programming.

## Honda / Acura

Honda body and cluster ECUs expose a flat array of one-byte
personalisation entries. `TOBDCodingHonda.GetEntry` / `SetEntry` (and
`SetEntryRanged` for range-checked writes) edit them by index.

## Toyota / Lexus

Toyota customisation menus are paged option tables with 16-bit option
IDs. `TOBDCodingToyota.ParseMenu` lists them; `GetOption` / `SetOption`
read and change values. `TOBDKeyAdaptationToyota` handles key
programming.

## Coding-option catalogues

To show options by name instead of by byte and bit, load a host-supplied
coding-option catalogue (format in [catalogs.md](catalogs.md)) with
`TOBDCodingOptionCatalog.Default.LoadFile`, look an option up with
`TryFindOption(Vendor, Module, Name, …)` and pass its addressing record
to the matching helper above.

## Key adaptation

`TOBDKeyAdaptationFord`, `TOBDKeyAdaptationHMG`, `TOBDKeyAdaptationBMW`
and `TOBDKeyAdaptationToyota` share one surface: `ListSlots`, `AddKey`,
`ClearOneSlot`, `ClearAllKeys` and `CheckPin`, each returning a
`TOBDKeyAdaptResult` (`Success`, `Message`, `Slots`). Changing operations
need **both** `AutoExecute = True` and an `OnConfirmExecute` handler that
allows the operation:

```pascal
procedure TForm1.KeyConfirm(Sender: TObject; AOp: TOBDKeyAdaptOp;
  ASlotIndex: Byte; var ACanProceed: Boolean);
begin
  ACanProceed := MessageDlg('Program a new key?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

KeyFord.Protocol := Protocol;
KeyFord.PIN := EditPin.Text;
KeyFord.OnConfirmExecute := KeyConfirm;
KeyFord.AutoExecute := True;
R := KeyFord.AddKey;  //  R: TOBDKeyAdaptResult
```

Some platforms also need `ChassisCode`.

## Common pitfalls

- **Wrong session.** UDS coding runs in `extendedDiagnosticSession`
  (`TOBDUDS.Open($03)`), not the default session. Programming runs in
  `programmingSession` (`$02`).
- **Stale security access.** Most ECUs drop the unlocked state after a
  few seconds of bus silence or a session change. Keep `UDS.KeepAlive`
  on and follow `Unlock` with the write.
- **Coding without an audit log.** Attach a `TOBDCodingAuditLog` to
  `TOBDCodingSession`; with a key set it is tamper-evident. Even on a
  bench you will want it later.
- **`AutoExecute` left on.** Set it right before a confirmed write and
  back to `False` afterwards. Never leave it on in unattended code that
  runs against customer vehicles.

## Where the wire-level reference lives

- UDS write services: `src/Coding/ERD.UDS.WriteDID.pas`,
  `src/Coding/ERD.UDS.WriteMemory.pas`, `src/Coding/ERD.Coding.*.pas`.
- KWP2000 write services: `src/Coding/ERD.KWP.WriteID.pas`.
- Per-OEM coding helpers: `src/Coding/ERD.Coding.<Vendor>.pas`.
- Component protection: `src/Coding/ERD.OEM.ComponentProtection.*.pas`.
- Key adaptation and OEM sessions: `src/OEM/ERD.OEM.*.pas`.

Every public surface is XMLDoc'd at declaration. The walkthroughs above
show the component-level wiring; the wire-level details are in source.
