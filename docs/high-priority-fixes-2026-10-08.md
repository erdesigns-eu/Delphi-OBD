# High-priority fixes voor claude/v2-phase-1

Dit is de voortgang na het [onderzoek van 8 oktober](gap-analysis-2026-10-08.md).
De drie P0- en negen P1-punten zijn aangepakt in de code en de bijbehorende
validatie. De Delphi-builds en hardwaretests blijven, zoals afgesproken,
uitgesteld. Dit rapport betekent dus geen productiecertificering van alle
protocollen, OEM-algoritmen of Windows-backends.

## Opgeloste punten

| Punt | Implementatie | Uitvoerbaar bewijs / grens |
|---|---|---|
| G01, P0 | Gemeenschappelijk `TOBDOwnedTask` met owned workers, join vóór vernietiging, afzonderlijk callback-lifetimetoken, onderbreekbare delays en annuleerbare main-thread consent. Startupfouten herstellen de in-flight-status. 42 niet-visuele componenten gebruiken de lifetime-helper; bestaande owned transportworkers blijven behouden. Protocolwijzigingen wachten op hun requestworker. Poll/replay/voltage-waits worden wakker bij stoppen. | Herhaald direct vernietigen van async snapshots, callbackonderdrukking en herstart, annuleren van lange delays/consent, replay vernietigen vanuit zijn eerste event tijdens een lange timestamp-gap en voltage-stop getest. Hosts moeten custom I/O/callbacks begrenzen en console-apps moeten `CheckSynchronize` pompen. |
| G02, P0 | Unieke tijdelijke bestanden, bestand flushen, atomair vervangen met POSIX rename / Windows MoveFileEx. FPC/Linux flusht ook de directory. De pipeline bewaart checkpoints synchroon na een bevestigd blok en stopt bij een schrijffout. Imagehash wordt eenmaal over een eigen imagesnapshot berekend. | 100 echte Save/Load-vervangingen; een ongeldige opslaglocatie stopt de gesimuleerde flash na de eerste ACK, vóór het volgende blok. De Windows-implementatie is nog niet op Windows uitgevoerd. |
| G03, P0 | `vmAllowSelfSigned` gebruikt peer-verificatie en accepteert uitsluitend de specifieke self-signed-leaf-fout op depth 0. Geldigheid en identiteit blijven gecontroleerd. Numerieke IPv4-adressen krijgen IP-SAN-validatie. | Zeven echte OpenSSL-handshakes: DNS/IP-match, verkeerde naam, verlopen certificaat, onbekende trust, geldige expliciete CA en verkeerde identiteit ondanks vertrouwde CA. Geen ECU-hardware gebruikt. |
| G04, P1 | DID-reads zonder lengtes sturen afzonderlijke requests; payloadbytes worden niet als een volgend DID geïnterpreteerd. `ReadStrict` gebruikt opgegeven lengtes en weigert negatieve lengtes, NRC's, verkeerde SID/echo, truncatie en trailing bytes. | Tests door de echte Connection → Adapter → Protocol → DataIdentifierIO-keten, inclusief embedded DID-bytes, batchread, truncatie, verkeerde echo, trailing bytes en NRC. |
| G05, P1 | Adaptercommando's gebruiken `WriteAll` met deadline en volledige verwerking van gedeeltelijke writes. Stalled/ongeldige aantallen falen. Wi-Fi biedt een native send-timeout, zonder idle receive-timeout. `CustomTransport` maakt hostproviders en een ECU-simulator bruikbaar; Close ontkoppelt callbacks. | Simulator schrijft maximaal twee bytes per aanroep. Een echte TCP-peer die zijn receive-window niet leegt bewijst dat een grote write binnen de deadline faalt. De complete-bufferfunctie is bedoeld voor streams, niet voor het opdelen van UDP-datagrammen. |
| G06, P1 | Replay leest streaming, met limieten voor gedecomprimeerde bytes, regellengte en aantal entries. `LoadAll` bezit en ruimt alle resources ook op foutpaden op. Malformed JSONL faalt expliciet; timestamps wachten onderbreekbaar. | Echte gzip-roundtrip, grenzen voor gewone en gzip-invoer, malformed JSONL, aantal entries en vernietiging tijdens realtime replay. `LoadAll` materialiseert de resultaatarray binnen de limieten; `Play` materialiseert het volledige bestand niet. |
| G07, P1 | `ResumeFromCheckpoint` voert de preflight/imagechecks uit en hervat zonder verse programming-entry of RequestDownload. Lokale validatie controleert hash, grootte, cursor, session, vendor, module en ECU-identiteit vóór wire-access. Een verplichte hostvalidator bevestigt dat de ECU nog in die transfer zit. Hervatte ACK's worden in hetzelfde checkpoint opgeslagen. Load weigert integer-wrap/masking. | Simulator bewijst remaining-block + exit, behoud van session/cursor, blokkering van ander image/andere ECU en afgewezen ECU-state. Volledige UInt64-adressen blijven behouden; waarden boven Int64 worden als decimale JSON-string opgeslagen vanwege een beperking in FPC System.JSON. De hostvalidator moet actuele ECU/session/transfer-state werkelijk bevestigen; ISO 14229 biedt geen universele checkpoint-query. |
| G08, P1 | Foutieve BMW DDB8/twee-byte/bias-definitie vervangen door de daadwerkelijk onderbouwde OVMS-definitie: DD69, big-endian signed int32, schaal −0,01, offset 0. Positief betekent ontladen. | Productiecatalogus en decoder getest met nul, +100 A laden en +100 A ontladen. Bron: [OVMS, vastgezette commit](https://github.com/openvehicles/Open-Vehicle-Monitoring-System-3/blob/ebb8275bdd6c52bda07c30af5e5df41860d7a554/vehicle/OVMS.V3/components/vehicle_bmwi3/src/vehicle_bmwi3.cpp#L459). Dit is bron-/vectorvalidatie voor i3, geen nieuwere BMW-modelclaim. |
| G09, P1 | HSM-facade delegeert beschikbaarheid, algoritmen en verificatie aan een geconfigureerde `IOBDSignatureVerifier`-driver. Een bestaand LibraryPath-bestand is onvoldoende. De always-raise-implementatie is verwijderd. Ook de OpenSSL-signaturebackend heeft nu een echte POSIX-loader, consistente initialisatie en beschikbaarheidscontrole. | Zes controles met een echt gegenereerde RSA-handtekening: bestand zonder driver, capabilities, geldig bericht, gewijzigd bericht en verwijderde driver. Native OpenSSL verifieert de cryptografie. Een specifieke PKCS#11-tokenimplementatie wordt door de host geleverd; er wordt geen gebundelde hardwaredriver geclaimd. |
| G10, P1 | Seed-keyregistry selecteert standaard alleen `Verified` providers. Unverified starters en lambda's vereisen expliciet `AllowUnverified := True` voor labgebruik. Een nieuwe unverified registratie kan een oudere verified provider niet verdringen. | Defaultweigering, expliciete lab-opt-in en selectie van een eligible provider getest. Provenance blijft beschikbaar via Find/FindAll. `Verified` is metadata van de leverancier; geen verzonnen OEM-algoritmen toegevoegd. |
| G13, P1 | Drie echte, tracked `.dproj`-entrypoints met search paths, namespaces, DCCReference-lijsten en outputpaden. Package-dependencies voor VCL/PNG/LiveBindings vermeld; onbestaande package-RES-verwijzingen verwijderd. CI controleert BDS en DUnitX-prerequisites. | XML, lokale references en project/package/search-path-analyzers gecontroleerd. De Delphi-job blijft bewust disabled tot een gelicentieerde Windows-runner en de afgesproken Delphi-validatiefase beschikbaar zijn. MSBuild/RAD Studio zijn hier niet uitgevoerd. |
| G14, P1 | Gedragsdekking uitgebreid met de echte wire-keten, flash-/recovery-simulator, native TCP/UDP, TLS-certificaatmatrix, native RSA-verificatie, lifetimes, replaylimieten en catalogusvectors. Assertions blijven actief. | 273 Linux nonvisual units compileren; 193 runtimechecks, zes signaturechecks en zeven TLS-handshakes slagen, naast 185 portable checks. Dit is gerichte regressiedekking, geen claim dat elke unit of elk protocol op hardware is getest. |

## Extra fouten gevonden tijdens de regressies

- De UDS-blokteller sloeg `00` over na `FF`. Download, upload en resume gebruiken
  nu modulo 256. Een simulator controleert alle 256 blokken en hervatten precies
  bij counter `00`. Een ontbrekende BSC-echo en verkeerde positieve SID falen.
- Het spanningsalarm liep uitsluitend via de UI-wachtrij. De safety-hook wordt
  nu synchroon op de pollingthread uitgevoerd; de pipeline controleert de abort
  vóór iedere transferrequest. De UI-notificatie blijft afzonderlijk gemarshald.
- HMG- en Nissan-arrayvelden misten metadata, waardoor het laden van de
  EV-catalogi faalde. De metadata is toegevoegd; arraydecoding gebruikt de
  opgegeven slice en weigert truncatie/misalignment. Alle vijftien vendorcatalogi
  plus de testfixture laden nu via de productieparser.
- Malformed JSON zonder parserexceptions kon met de gebruikte officiële FPC
  System.JSON een ongeldige resultaatpointer opleveren. Replay, checkpoint en de centrale catalogushelper
  schakelen expliciete parserexceptions in en behandelen het foutpad. Ongeldige
  catalogus-JSON en niet-object-roots krijgen gecontroleerde configuratiefouten.

## Validatie

```sh
python3 tools/fpc_runtime.py --source-tree /workspace/onboarding-delphi-obd/fpc-source
python3 tools/fpc_smoke.py --compiler /workspace/onboarding-delphi-obd/fpc/usr/lib/x86_64-linux-gnu/fpc/3.2.2/ppcx64
python3 tools/pascalcheck/run.py -v
python3 -m unittest discover -s tools/pascalcheck -p test_checkers.py -v
python3 -m unittest discover -s tools -p test_validate_catalogs.py -v
python3 tools/validate_catalogs.py --require-coverage
```

De officiële FPC-sourcepin en reproduceerbare installatie staan in
[fpc-compatibility.md](fpc-compatibility.md). De compilecheck gebruikt echte
FPC-bibliotheken en compileert de oorspronkelijke units, zonder RTL-stubs.

| Controle | Resultaat |
|---|---|
| FPC 3.3.1, Linux x86-64 | 273 niet-visuele units gecompileerd |
| Runtime | 193 checks geslaagd |
| Handtekeningen | 6 native RSA/driverchecks geslaagd |
| TLS | 7 echte handshake-scenario's geslaagd |
| Portable FPC 3.2.2 | 185 checks geslaagd |
| Pascal-analyzers | 107 uitgevoerd, geen bevindingen; 5 expliciet niet van toepassing |
| Analyzerregressies | 9 tests geslaagd |
| Catalogusauditorregressies | 12 tests geslaagd |
| Catalogusschema's | 248 gecontroleerd; 0 uncovered, 0 violations |
| Delphi-projecten | XML/references/package/search paths gecontroleerd; Delphi-build niet uitgevoerd |
| Whitespace | `git diff --check` schoon |

## Resterende fase

De oorspronkelijke P2-punten (G11/G12/G15/G16: supportmatrix, FMX-claims,
afbeeldingen/UI/iconen en opgeschoonde tooling) zijn geen onderdeel van deze
functionele reparatieronde. Nieuwe afbeeldingen, UI-ontwerp en toolingcleanup
zijn niet uitgevoerd. De compilerprofielen/documentatie zijn wel bijgewerkt
om de huidige validatie en vereiste hostconfiguratie correct te beschrijven.

Windows-backends, VCL/IDE-installatie, DUnitX-uitvoering, fysieke transports en
ECU-specifieke flash/security-/recovery-workflows moeten in de afgesproken
Delphi- en benchfase worden gecontroleerd. De bestaande lage-level `Resume`
blijft beschikbaar voor hosts die zelf alle herstelvalidatie uitvoeren; gebruik
voor het normale pad de geïntegreerde pipeline met expliciete ECU-statevalidator.
