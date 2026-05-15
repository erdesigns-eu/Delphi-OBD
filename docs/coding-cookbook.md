# Coding cookbook

End-to-end walkthroughs for vendor-specific coding flows. The runtime
components (`TOBDDataIdentifierIO`, `TOBDSecurityAccess`,
`TOBDRoutineControl`, `TOBDCodingSession`, plus the OEM-specific
component-protection helpers) handle the wire side; this page shows
how to wire them up for each platform.

> **Heads-up.** Coding writes to the ECU. Mistakes can immobilize
> the vehicle, store new DTCs, or break safety functions. Test on a
> bench first whenever practical. The destructive components in
> Delphi-OBD ship `AutoExecute = False` by default — keep them that
> way, wire `OnConfirmExecute`, and only flip the flag when you mean
> it.

## The shared scaffolding

Every coding flow starts the same:

```pascal
Connection.SerialSettings.Port := 'COM3';
Connection.Active := True;

Adapter.Connection := Connection;
Adapter.Detect;

Protocol.Adapter := Adapter;
```

From here the per-vendor wiring picks up.

## Volkswagen / Audi / Skoda / SEAT (VAG)

VAG ECUs use long coding (`0x22 / 0x2E` against DIDs in the
`0x01xx` range) plus a separate "coding" service via short codes for
older platforms. The `TOBDCodingCatalogVAG` loader (in
`OBD.Coding.Catalog.VAG`) reads `.lbl` label files and surfaces
the option list to the host.

```pascal
SecurityAccess.Protocol := Protocol;
SecurityAccess.RequestSeedLevel := $03;     //  long-coding level
SecurityAccess.OnComputeKey := procedure(Sender: TObject;
                                         const ASeed: TBytes;
                                         var AKey: TBytes)
begin
  AKey := VAGSeedToKeyLevel3(ASeed);
end;
SecurityAccess.Unlock;

DataIO.Protocol := Protocol;
DataIO.AutoExecute := False;
DataIO.OnConfirmExecute := HandleUserConfirm;
DataIO.WriteDID($0107, LongCodingBytes);    //  module 09 (BCM) coding string

CP_VAG.Protocol := Protocol;                //  immobilizer pairing
CP_VAG.AdaptKey(NewKeyTransponderID);       //  destructive — confirm path
```

The `TOBDComponentProtectionVAG` component handles the immobilizer
challenge-response. For ME7 / Simos / EDC engine ECUs the VAG flow is
the same shape; the security-access level changes (`$03` for coding,
`$05` for adaptation, `$09` for service routines).

## BMW

BMW ECUs use FA / SVT (Fahrzeugauftrag / SVT vehicle order) coding,
delivered as binary blobs written to specific control units via
`0x2E`. Newer F/G/U-series chassis use ISTA-style flash + code via
DoIP; older E-series use K-line / DCAN.

```pascal
SecurityAccess.RequestSeedLevel := $03;
SecurityAccess.OnComputeKey := procedure(Sender: TObject;
                                         const ASeed: TBytes;
                                         var AKey: TBytes)
begin
  AKey := BMWSeedToKey(ASeed);              //  vendor-specific
end;
SecurityAccess.Unlock;

DataIO.WriteDID($CFA0, FABytes);            //  Fahrzeugauftrag
DataIO.WriteDID($CFA1, SVTBytes);           //  Soll-Verbau-Tabelle

Routine.Protocol := Protocol;
Routine.Start($FF00);                       //  apply coding routine

CP_BMW.Protocol := Protocol;
CP_BMW.PairISN(KombiISN);                   //  immobilizer pairing
```

For F/G/U chassis use a `TOBDDoIPClient` instead of an
ELM327 / OBDLink chain; the rest is identical.

## Mercedes-Benz

Mercedes uses SCN (Software Calibration Number) coding via XENTRY-style
service routines. Modern (W213+) cars require online SCN authorization;
legacy (W211 / W212) coding is offline. The
`TOBDComponentProtectionMercedes` component handles the variant-specific
ceremony.

```pascal
SecurityAccess.RequestSeedLevel := $07;     //  SCN level
SecurityAccess.Unlock;

Routine.Start($02FF, ScnPayload);           //  apply SCN

CP_Mercedes.Protocol := Protocol;
CP_Mercedes.RegisterControlUnit(ECUSerial); //  variant coding
```

## Stellantis (FCA / PSA / Opel)

Stellantis platforms run a unified ECU-coding flow across the FCA / PSA
brands post-merger. The key surface is `0x2E` writes against `0xF1xx`
identifiers plus security-access levels `$01` / `$02` / `$03` /
`$05` per platform.

```pascal
SecurityAccess.RequestSeedLevel := $02;
SecurityAccess.Unlock;

DataIO.WriteDID($F190, VINBytes);
DataIO.WriteDID($F1A0, ConfigBytes);

CP_Stellantis.Protocol := Protocol;
CP_Stellantis.PairBSI(BSISerial);           //  body computer pairing
```

## Ford / Lincoln

Ford uses As-Built data — a ~30-byte coding string per module. Ford
service-mode is `$03` (defaults to engineering / dev); production
coding tools use `$01` and write via `0x2E` against
`0xDExx` / `0xDFxx` DIDs.

```pascal
SecurityAccess.RequestSeedLevel := $01;
SecurityAccess.OnComputeKey := procedure(Sender: TObject;
                                         const ASeed: TBytes;
                                         var AKey: TBytes)
begin
  AKey := FordAsBuiltSeedToKey(ASeed);
end;
SecurityAccess.Unlock;

DataIO.WriteDID($DE00, AsBuiltBlock);
```

## Hyundai / Kia / Genesis (HMG)

HMG uses CDS (Component Diagnostic Specification) variant coding via
short variant blocks. Recent HMG ECUs additionally require an SVK
(Software Version Keying) handshake at session start.

```pascal
SecurityAccess.RequestSeedLevel := $03;
SecurityAccess.Unlock;

Routine.Start($DD01, VariantBytes);         //  variant coding
```

## Honda / Acura

Honda historically used K-line / FCS-style flat coding. Newer HDS-
era Honda runs UDS over CAN with `0x2E` against vendor-specific DIDs.

```pascal
DataIO.WriteDID($0100, HondaConfigByteArray);
```

## Toyota / Lexus

Toyota CARSCAN runs UDS over CAN with `0x2E` against `0xF1xx` DIDs.
Toyota security-access on `$01` / `$05` is by design straightforward —
the seed/key is published in the GTS service docs.

```pascal
SecurityAccess.RequestSeedLevel := $01;
SecurityAccess.Unlock;

DataIO.WriteDID($F190, ConfigBytes);
```

## Common pitfalls

- **Wrong session.** UDS coding is in `extendedDiagnosticSession`
  (`0x10 0x03`), not `defaultSession` (`0x10 0x01`). Use
  `TOBDCodingSession` to put the ECU in the right session for you.
- **Stale security access.** Most ECUs time out the unlocked window
  after ~5 s of bus silence. Either keep traffic flowing or
  immediately follow `Unlock` with the write.
- **Coding without an audit log.** Every coding starter wires a
  `TOBDCodingAuditLog`. Don't disable it — even on a bench, you'll
  thank yourself later.
- **Auto-execute on production hardware.** `AutoExecute = True`
  skips the host's `OnConfirmExecute`. Only enable it for
  unattended automation against a bench, and never on a customer
  vehicle.

## Where the wire-level reference lives

- UDS coding services: `OBD.Protocol.UDS.pas` + `OBD.UDS.WriteMemory.pas`.
- KWP2000 coding services: `OBD.Protocol.KWP2000.pas` + `OBD.KWP.WriteID.pas`.
- Vendor specifics: `src/OEM/OBD.OEM.*.pas`.
- Component protection: `src/OEM/OBD.OEM.ComponentProtection.*.pas`.

Every public surface is XMLDoc'd at declaration. The walkthroughs
above only show the component-level wiring — the wire-level details
are in source.
