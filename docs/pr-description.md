## What & why

Deze PR introduceert de componentgerichte v2 van Delphi-OBD en brengt de branch
naar een basis die in Delphi gebouwd en getest kan worden. De library bestaat
uit niet-visuele diagnose-/transportcomponenten, Delphi VCL-controls en een
beperkt design-time package voor componentregistratie en editors.

**Scope:** Delphi VCL op Windows; de niet-visuele library ook in FPC op Linux.
FMX en een Lazarus-UI zijn buiten scope. Project-/formwizards, startergeneratie,
IDE-help-hooks en About/Splash-integratie zijn verwijderd. De packages behouden
alle **229 geregistreerde componenten**, palette-iconen en property/component-editors.

### Library en componenten

- Nieuwe componentarchitectuur met verbinding → adapter → protocol → services,
  configuratie via properties en synchrone/async-methoden met resultaat-, fout-
  en progress-events. De runtime en het IDE-package zijn gescheiden.
- Transporten en protocolcomponenten voor onder meer Serial, Bluetooth/BLE,
  Wi-Fi/UDP, FTDI, ELM327/OBDLink, J2534, DoIP/TLS, OBD-II, UDS, KWP2000,
  KWP1281 en J1939. Daarnaast coding, calibration, recording/replay,
  voertuig-/VIN-diagnose, EV-data en specialistische componenten.
- VCL-controls voor gauges, live-data, DTC's, terminal/logging, verbinding,
  EV-monitoring en flashstatus. Dit beschrijft de aanwezige code, niet
  bench-gevalideerde ondersteuning van iedere adapter, ECU of protocolvariant.

### Stabiliteits- en correctheidsreparaties

- KWP-keepalive en de OEM UDS-asyncworker starten na `Create`, zodat Delphi
  `AfterConstruction` eerst afrondt; constructor-starts hebben een statische
  regressie en beide workers hebben linked startup-/stoptests.
- Owned async-workers met cancellation/join en lifetime-tokens voor queued
  callbacks; invoer wordt vastgelegd en overlappende acties worden geweigerd.
  Lifecycle-/consent-/destroy-scenario's hebben uitvoerbare regressies.
- Betrouwbare gedeeltelijke TCP-writes en bounded timeouts, korte TCP-responses,
  UDP-fixtures en strikte DID/PID-echo- en responsvalidatie. Onbekende DID-lengtes
  worden niet meer met een heuristische multi-DID-split geïnterpreteerd.
- Atomic CAN-routing/header/filter/extended-address configuratie en routing per
  EV-regel; VIN-matching, J1939-definities en numerieke OEM-coding gecorrigeerd.
- Replay wordt begrensd op gedecomprimeerde bytes, regels en entries, ruimt
  foutpaden op en kan lange realtime-wachttijden annuleren.
- De TLS-optie voor self-signed certificaten behoudt controles op geldigheid en
  hostname/IP-identiteit. Een foutieve naam of verlopen certificaat wordt geweigerd.
- Catalogus-JSON geeft gecontroleerde configuratiefouten bij ongeldige input,
  met offline schema- en manifestvalidatie voor alle 248 gegevenscatalogi.

### Flashing, herstel en security

- Checkpoints worden atomair vervangen en bevatten gecontroleerde image/cursor/
  sessie-/ECU-informatie. Een checkpointfout na een geaccepteerd blok stopt de
  transfer voordat het volgende blok wordt verzonden.
- De geïntegreerde resume-pipeline controleert image en lokale checkpointdata
  en vereist expliciete bevestiging van ECU-state door de host. Tellerwrap
  `FF → 00`, foutieve ACK's en mismatch-/herstelpaden zijn getest met simulators.
- De voltage-gate kan de transfer onafhankelijk van GUI-callbackafhandeling
  stoppen. Destructieve workflows behouden hun consent/configuratievoorwaarden.
- Seed/key-selectie kiest standaard uitsluitend geverifieerde providers;
  ongeteste providers vereisen expliciete lab-opt-in.
- De HSM-facade gebruikt een host-aangeleverde signature-driver. Alleen een
  bestaand librarybestand geldt niet als beschikbare HSM. OpenSSL-verificatie
  werkt via een echte POSIX-loader en is getest met RSA-handtekeningen.
  Een specifieke PKCS#11-hardwaredriver wordt niet gebundeld of als getest geclaimd.

### Packages, catalogi en tooling

- Alle `OBD.*`-units zijn hernoemd naar `ERD.*`; componentklassen blijven `TOBD*`.
  FPC/Delphi gebruiken conditionele mode- en RTL-imports.
- De volledige niet-visuele FPC-runtime gebruikt een gepinde officiële FPC
  3.3.1-developmentcompiler en echte RTL/FCL/nonvisual compatibility packages,
  zonder API-stubs. FPC 3.2.2 blijft het portable-codecprofiel.
- De CI-runner kiest bij `--source-tree` de gebouwde compiler in die tree,
  ook als de 3.2.2-bootstrap op PATH staat. Expliciete `--compiler` heeft voorrang.
- Tracked Delphi-projecten voor RT, DT en DUnitX hebben expliciete IDE-personality/
  projecttype-metadata en Base/Debug/Release-configuraties met platforminheritance.
  BOM/CRLF bij checkout en gescheiden outputs voorkomen project/configuratiedrift;
  zij zijn geen bewijs dat een Delphi-IDE-crash al is opgelost.
- DT blijft afhankelijk van RT zodat componentunits één keer in de IDE geladen
  worden. Een standalone DT-variant zonder RT is niet ingevoerd. PNG/LiveBindings-
  dependencies worden behouden waar de componenten ze nodig hebben.
- Alle componenticonen hebben reproduceerbare resources uit 202 native PNG's;
  27 eerder ontbrekende iconen delen expliciet artwork met verwante componenten.
  VCL-status bij detach, ontbrekende celmetingen en late IDE-dialogcallbacks zijn
  gecorrigeerd.
- Een gegenereerde EV-supportmatrix onderscheidt modellen/ECU's/veldregels,
  brondata en validatielimieten. Zeven lege vendorcatalogi zijn niet ondersteund;
  de vijftien catalogusbestanden betekenen geen volledige merkdekking.
- Ongebruikte gekopieerde media-/playlist-/translation-/artwork-scripts en
  app-specifieke analyzers zijn verwijderd. ERD-validatie en Delphi-overdracht
  zijn gedocumenteerd en opgenomen in CI.

## How did you test it?

| Controle | Uitgevoerde validatie |
|---|---|
| FPC 3.3.1 Linux runtime | 273 niet-visuele units compileren rechtstreeks |
| Runtime-regressies | 196 checks, inclusief echte transportketen, lokale TCP/UDP, cancellation, checkpoints, resume, replay en catalogusvectors |
| Native cryptografie | 6 checks met echte OpenSSL RSA-verificatie |
| TLS | 7 echte handshakes voor DNS/IP-match, mismatch, expired, untrusted en expliciete CA-trust |
| FPC 3.2.2 portable | 185 codec/request/routechecks |
| Statische Pascal-analyse | 97 actieve checkers, nul bevindingen of ontbrekende-input-skips |
| Python-regressies | 28 catalogue-/repositorytooltests; 13 analyzer-fixtures |
| Catalogi | 248 schema-gecontroleerd, nul uncovered/violations |
| Delphi-projecten/resources | XML, IDE-metadata/configuratie/encoding, lokale references en 229 icon-resources offline gecontroleerd |

De volledige FPC-run is ook geslaagd met de 3.2.2-bootstrap op PATH, als
regressie voor de gemelde CI-compilerselectiefout. Dit zijn lokale uitgevoerde
controles; zij zijn geen verklaring dat iedere remote CI-run al groen is.

**Nog niet uitgevoerd:** echte RAD Studio/MSBuild-builds, DUnitX op Delphi,
VCL-/DPI-/IDE-installatietests en tests met fysieke adapters/ECU's. De Delphi-CI-job
blijft bewust uitgeschakeld totdat een geschikte Windows-runner beschikbaar is.
Windows-/Bluetooth-backends zijn expliciet uitgesloten van het Linux FPC-profiel.
Er is geen gemeten codecoveragepercentage of warning-vrije Delphi-build geclaimd.

## Documentation

- [Compilerprofielen en FPC-grenzen](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/fpc-compatibility.md)
- [P0/P1-reparaties en regressies](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/high-priority-fixes-2026-10-08.md)
- [Opschoning en overdracht](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/cleanup-handover-2026-10-08.md)
- [Delphi-build/installatie/benchchecklist](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/delphi-validation.md)
- [EV-supportmatrix](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/ev-support-matrix.md)
- [Tooling en opdrachten](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/tools/README.md)
- `CHANGELOG.md`, `PLAN.md`, installatie- en componentdocumentatie zijn bijgewerkt.

## Hardware risk (flashing / coding only)

De flash-/coding-reparaties zijn getest met simulators en gerichte vectors,
**niet op een fysieke ECU**. Er is daarom geen voertuig-/ECU-benchresultaat.
Voor echt gebruik moeten adapter/driver/firmware, ECU-compatibiliteit, consent,
voltage, signatures en herstelstate afzonderlijk worden gevalideerd volgens
[de flashhandleiding](https://github.com/erdesigns-eu/Delphi-OBD/blob/claude/v2-phase-1/docs/flashing-safety.md).

## Reviewer notes

**Breaking migration:** wijzig application `uses` en bronpaden van `OBD.*` naar
`ERD.*`, verwijder oude DCU/DCP/BPL-artifacts en rebuild RT/DT/applicaties.
`TOBD*`-componentnamen en package-identiteiten `DelphiOBD_RT`/`DelphiOBD_DT` blijven
behouden. De UI-scope is VCL-only en de IDE-integratie bestaat uitsluitend uit
componentregistratie en editors.

De volgende acceptatiefase is een daadwerkelijke Delphi-build/installatie en
DUnitX-run, gevolgd door VCL-rendering en adapter-/ECU-benchtests. Nieuwe fouten
uit die fase zijn nog mogelijk; de huidige validatie vervangt die fase niet.
