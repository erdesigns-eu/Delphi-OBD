# Onderzoek naar gaten in claude/v2-phase-1

> Dit rapport beschrijft de oorspronkelijke audit. Zie het
> [vervolgrapport met de high-priority fixes](high-priority-fixes-2026-10-08.md)
> voor de huidige implementatie en validatie.

**Datum:** 8 oktober 2026. **Onderzochte commit:** `d8e844af24aed98ab754458caccc9bc129cbf740`.
**Scope:** implementaties, async-lifecycle, transports, coding/flashing, cryptografie,
gegevenscatalogi, UI/assets, tooling en validatie. Productiecode is tijdens dit
onderzoek niet gewijzigd.

De branch heeft een brede API en een bruikbare compilerbasis, maar is nog niet
voldoende onderbouwd als volledig werkende productieversie. Een geslaagde
compilatie bewijst niet dat alle uitvoeringspaden werken. Dit onderzoek vindt
**16 concrete verbeterpunten: drie P0, negen P1 en vier P2**. Twee fouten zijn met
uitvoerbare proeven bevestigd; verschillende andere volgen rechtstreeks uit de
implementatie. Ontbrekende hardwarevalidatie blijft een beperking van het onderzoek.

## Methode en bestaande basis

- Brononderzoek van de runtime, geselecteerde UI-units, CI en catalogi; 343
  Pascalbronbestanden in `src` inclusief het header-template, 103 testunits.
- Gericht zoeken naar onvoltooide code, daarna lezen van de daadwerkelijke
  implementaties. Woorden als `placeholder` betekenen niet automatisch een defect.
- Twee tijdelijke Pascalproeven, gecompileerd met de echte FPC 3.3.1-toolchain
  en originele repository-units: checkpoint herschrijven en TLS-hostnaamcontrole.
- Inventarisatie van de 15 EV-vendorcatalogi en 214 PNG-assets; visuele inspectie
  van `about.png` en het connection-icoon. Geen volledige UI-inspectie in Delphi.
- De voorafgaande validatie op deze commit: 272 Linux nonvisual units gecompileerd,
  38 runtimecontroles, 185 codeccontroles, 107 toepasselijke analyzers zonder
  bevindingen, negen analyzer-regressies, twaalf catalogus-regressies en 248
  catalogi met schema-dekking zonder overtredingen. Die volledige suite is voor
  dit brononderzoek niet opnieuw uitgevoerd. Geen Delphi-build of hardwaretest.

**P0:** eerst herstellen voor een stabiele release. **P1:** belangrijk voor
betrouwbare functionaliteit en aantoonbare ondersteuning. **P2:** gerichte
uitbreiding, productafwerking of onderhoud. Dit zijn technische prioriteiten,
geen bewijs dat ieder onderdeel of iedere niet-geteste configuratie defect is.

## Overzicht

| ID | Prioriteit | Bevinding | Bewijs |
|---|---|---|---|
| G01 | P0 | Async-worker kan na vernietiging eigenaar gebruiken | Broncode |
| G02 | P0 | Checkpoint kan op FPC niet opnieuw worden opgeslagen | Uitgevoerd |
| G03 | P0 | TLS self-signed-modus accepteert verkeerde hostnaam | Uitgevoerd |
| G04 | P1 | Multi-DID-reader kan payload verkeerd splitsen | Broncode en bytevoorbeeld |
| G05 | P1 | Gedeeltelijke TCP-write wordt niet afgehandeld in adapterpad | Broncode |
| G06 | P1 | Replay heeft foutpad-lek en onbegrensde geheugeninname | Broncode |
| G07 | P1 | Resume mist geïntegreerde image- en ECU-contextcontrole | Integratiegat |
| G08 | P1 | BMW-stroomdecoder heeft tegenstrijdige signed/bias-instellingen | Catalogus/decodercontract |
| G09 | P1 | HSM-backend adverteert ondersteuning zonder verificatie-implementatie | Broncode |
| G10 | P1 | OEM-seed-key starters zijn geen gevalideerde productie-algoritmen | Expliciete bronmetadata |
| G11 | P2 | Zeven EV-catalogi hebben geen uitvoerbare velden | Catalogusinventaris |
| G12 | P2 | FMX wordt genoemd, maar een FMX-UI ontbreekt | Bestandsinventaris |
| G13 | P1 | Voorbereide Delphi-CI verwijst naar ontbrekende projecten | CI/bestandsinventaris |
| G14 | P1 | Gedragsdekking blijft veel kleiner dan compileerdekking | Testinventaris en nieuwe fouten |
| G15 | P2 | Assets missen aantoonbare controle op kleine maten en UI-context | Assets/inspectie |
| G16 | P2 | Toolmap bevat nog meegekopieerde media-appfunctionaliteit | Toolbroncode |

## Eerst herstellen

### G01 — Async-lifecycle is niet overal voltooid

[VehicleHealth](../src/Service/ERD.Service.VehicleHealth.pas) maakt in
`SnapshotAsync` een anonieme worker met een verwijzing naar `Self`, terwijl de
destructor alleen de lock vrijgeeft. [Replayer](../src/Recorder/ERD.Replayer.pas)
roept `Stop` aan, maar wacht niet op `PlayAsync`; de worker gebruikt daarna nog
`Self` en `ReleaseAsync`. [LiveData](../src/Service/ERD.Service.LiveData.pas)
joinet de pollingthread, maar dat dekt de afzonderlijke `ReadAsync`-worker niet.
Een single-in-flight-vlag voorkomt overlap, maar houdt de eigenaar niet in leven.

**Effect:** mogelijke toegang tot vrijgegeven componenten/locks en callbacks naar
vernietigde objecten, afhankelijk van timing. Geen crashproef uitgevoerd.

**Oplossing:** één gedeeld ownership-patroon voor requestworkers: worker bewaren,
annuleren, wachten en bijbehorende queued callbacks verwijderen. Vernietiging
mag niet deadlocken op een worker die op main-thread `Synchronize` wacht.

**Acceptatie:** deterministische tests voor destroy tijdens geblokkeerde I/O,
queued callback vóór destroy, callback die zelf stopt, herhaald start/stop en
annulering. Pas het patroon daarna op alle async-componenten toe.

### G02 — Herhaald opslaan van een flash-checkpoint faalt op FPC

[Checkpoint.Save](../src/Flashing/ERD.Flash.Checkpoint.pas) schrijft `.tmp` en
roept op POSIX `TFile.Move` aan. De officiële FPC `System.IOUtils.TFile.Move`
weigert een bestaande bestemming. Een tweede opslag gaf daadwerkelijk:

```text
CHECKPOINT second save failed: File "…/checkpoint.json" already exists
```

De [pipeline](../src/Flashing/ERD.Flash.Pipeline.pas), `WriteCheckpointSafe`,
vangt deze fout en schrijft auditinformatie; de transfer kan doorgaan terwijl
het checkpoint veroudert. Zonder auditlog is die melding mogelijk niet zichtbaar.
Ook gebruikt elke schrijver dezelfde `.tmp`-naam. De Windows-fallback verwijdert
het oude bestand vóór de move en biedt daardoor niet dezelfde atomiciteit.

**Oplossing:** platformgerichte atomic replace, unieke tijdelijke bestanden,
expliciet beleid voor checkpointfalen en opruiming van tijdelijke bestanden.
Maak apart onderscheid tussen atomische vervanging en duurzaamheid bij stroomuitval.

**Acceptatie:** honderden opeenvolgende updates, gelijktijdige schrijvers volgens
het gekozen contract, foutinjectie rond schrijven/rename, en bewaren van het
vorige goede checkpoint als vervanging mislukt.

### G03 — TLS-hostnaamcontrole klopt niet in vmAllowSelfSigned

[OpenSSL transport](../src/Protocol/ERD.Protocol.DoIP.TLS.OpenSSL.pas) configureert
`SSL_VERIFY_NONE` voor `vmAllowSelfSigned`. `SSL_set1_host` wordt wel ingesteld,
maar de code controleert het verificatieresultaat alleen bij `vmRequire`.
De commentaarbelofte dat self-signed-modus de hostnaam blijft afdwingen wordt
niet waargemaakt.

**Uitgevoerde proef:** lokale TLS-server met self-signed certificaat en SAN
`DNS:wrong.example`; de client verbond met `127.0.0.1` in `vmAllowSelfSigned`:

```text
TLS wrong hostname ACCEPTED
```

Dit betreft deze optionele modus; het resultaat toont geen bypass in de
standaardmodus `vmRequire` aan.

**Oplossing:** host/IP-identiteit afzonderlijk afdwingen en precies definiëren
welke chain-fout self-signed-modus toestaat, of vertrouwen via een expliciete
lokale CA/pinning. Controleer ook DNS-SAN versus IP-SAN.

**Acceptatie:** lokale TLS-matrix met juiste/verkeerde DNS-naam en IP-SAN,
vertrouwde/onvertrouwde chain, verlopen certificaat, self-signed-policy,
handshake-timeout en voortijdig gesloten verbinding.

## Betrouwbaarheid en ontbrekende functionaliteit

### G04 — Multi-DID-responsen hebben geen betrouwbare lengtegrenzen

[DataIdentifierIO.DoRead](../src/Coding/ERD.Coding.DataIdentifierIO.pas) zoekt
voor elke grens naar de bytewaarde van de volgende gevraagde DID. Die waarde
mag gewoon in de data van de vorige DID voorkomen. Bijvoorbeeld gevraagde DIDs
`F190/F191` en responsdata `F190 AA F191 BB F191 CC`: als de eerste payload
`AA F191 BB` is, splitst de huidige zoekactie al op de eerste `F191`.

**Aanpassen:** lengtemetadata/decodercontract per DID; bij onbekende lengtes
single-DID-requests of expliciet weigeren van ambigu multi-DID-gebruik. Test
embedded DID-bytes, nul-lengte, truncatie, verkeerde echo en trailing data.

### G05 — TCP kan een gedeeltelijke write opleveren

[FPC socket.Send](../src/Compat/ERD.Compat.Socket.pas) doet één `fpSend` en
retourneert het aantal bytes. Dat is een geldig low-level contract.
[WiFi.WriteBytes](../src/Connection/ERD.Connection.WiFi.pas) geeft dat aantal
door, maar [Adapter.DoSendCommand](../src/Adapter/ERD.Adapter.pas) negeert de
retourwaarde van `WriteString`. Een gedeeltelijke opdracht wordt niet afgemaakt.

**Aanpassen:** een `WriteAll`-pad met offset, deadline en annulering voor
streamtransports. UDP-datagrammen mogen niet op dezelfde manier worden gesplitst.
Test dit met een transport dat bewust slechts enkele bytes per write accepteert.

### G06 — Replay-foutpaden en grote logs

In [Replayer.LoadAll](../src/Recorder/ERD.Replayer.pas) staat `LoadLines` vóór het
`try/finally` dat het gemaakte replayer-object vrijgeeft. Als openen/decompressie
mislukt, wordt dat object niet via deze cleanup vrijgegeven. Gzip-invoer wordt
bovendien volledig naar buffers, een stream en vervolgens regels ingelezen;
`LoadAll` maakt daarna nog een verzameling entries.

**Aanpassen:** ownership direct na constructie beschermen; streaming replay en
configureerbare grenzen voor uitgepakte bytes, regelgrootte en entry-aantal.
Test ontbrekende/corrupte bestanden, CRC-fouten en grote uitgepakte logs. Een
streaming optie is nodig om langdurige captures zonder grote geheugenpiek af te spelen.

### G07 — Resume heeft geen geïntegreerde herstelworkflow

[UDS.Transfer.Resume](../src/Flashing/ERD.UDS.Transfer.pas) controleert onder meer
imagegrootte, cursor en chunkgrootte, maar niet de imagehash of ECU-identiteit.
De checkpointmodule biedt `MatchesImage`; de caller moet die zelf gebruiken.
Resume slaat `RequestDownload` over en veronderstelt dat de ECU nog midden in
dezelfde transfer zit. Dit is een expliciet low-level contract, geen bewijs
van een defect als een host het correct toepast.

**Toevoegen:** een hogere `ResumeFromCheckpoint`-workflow die hash, ECU/vendor/
module, adres, sessie en ECU-transferstatus controleert. Hervatten na een
procesherstart of ECU-reset mag niet alleen op een passende bestandsgrootte vertrouwen.

### G08 — BMW pack_current heeft een semantisch conflict

[BMW-catalogus](../catalogs/ev-battery/bmw.json), DID `DDB8`, bevat `signed=true`,
`scale=0.1` en `offset_value=-3276.8`, terwijl de toelichting `signed int16 / 10`
beschrijft. De [EV-decoder](../src/Service/ERD.Service.EVBattery.pas) gebruikt
`raw * scale + offset`. Daarmee wordt een signed raw nul `-3276.8 A`.

**Aanpassen:** tegen de primaire bytebeschrijving vaststellen of het wireformaat
signed of biased unsigned is, en één consistente omzetting kiezen. Geen
hardwarebevestiging uitgevoerd. Golden vectors moeten minimaal nul, positieve
stroom, negatieve stroom en grenzen dekken. Schema-validiteit alleen detecteert
dit semantische conflict niet.

### G09 — PKCS#11/HSM is momenteel een interface-opzet

[Signature.HSM](../src/Flashing/ERD.Signature.HSM.pas) meldt verschillende
algoritmen als ondersteund. `IsAvailable` controleert of een pad bestaat;
`DoVerify` werpt vervolgens altijd een niet-geïmplementeerd-exception.
Een bestaande DLL bewijst geen bruikbare verifier.

**Aanpassen:** echte PKCS#11-integratie en sessielifecycle, of een expliciete
host-verifierinjectie met correcte availability/capability-status. Test met een
software-HSM zoals SoftHSM voordat fysieke tokens nodig zijn. Advertenties van
ondersteuning moeten de daadwerkelijk beschikbare implementatie volgen.

### G10 — OEM-registraties zijn niet gelijk aan ECU-ondersteuning

Onder meer [BMW](../src/OEM/ERD.OEM.BMW.pas), Ford, HMG, Honda, Renault en andere
extensies registreren expliciet placeholder/reference seed-key-algoritmen.
`Verified=False` is metadata; [Registry.ComputeKey](../src/OEM/ERD.OEM.SeedKey.pas)
controleert die status niet. Dat is een extensiepunt, geen bewezen productie-
unlock voor ieder merk. Verschillende radiocodecomponenten bieden eveneens
validatie en `OnCalculate` in plaats van een ingebouwd berekeningsalgoritme.

**Aanpassen:** zichtbare capability-status: implemented, host-provided,
reference-only, bench-verified. Productiepaden kunnen een expliciete policy
voor ongeteste algoritmen krijgen. Voeg uitsluitend onderbouwde algoritmen met
vectors toe; verzin geen merkalgoritmen om een componentlijst volledig te laten lijken.

### G11 — EV-dekking is modelgebonden en gedeeltelijk

| Vendorcatalogus | Aantal veldregels |
|---|---:|
| VW | 229 |
| HMG | 31 |
| Nissan | 12 |
| BMW | 9 |
| GM, Renault | ieder 8 |
| Ford | 2 |
| Polestar | 1 |
| Honda, Mazda, Mercedes, Porsche, Stellantis, Tesla, Toyota | ieder 0 |

Een veldregel is geen afzonderlijk ondersteund model of compleet beschikbare
meting. De lege catalogi zijn expliciete beperkingen, geen kapotte JSON. VW
betreft de gedocumenteerde kleine EV-platforms; BMW de i3-familie, niet alle
nieuwere modellen.

**Toevoegen:** capability-overzicht per model/jaar/ECU met beschikbare velden,
bron en validatieniveau. Begin uitbreidingen met onderbouwde datasets. Tesla
vraagt een afzonderlijke passieve CAN-telemetrie-integratie; lege UDS-regels
invullen zou daar geen correcte ondersteuning bieden.

### G12 — FMX-claim versus aanwezige UI

README en compatibiliteitsdocumentatie noemen VCL/FMX. Er is een VCL-UI onder
`src/UI`, maar geen `src/UI.FMX` en geen FMX-bronunits in de onderzochte tree.

**Aanpassen:** huidige UI als Delphi VCL beschrijven. FMX alleen als toekomstige
port noemen totdat er echte rendering, binding en tests bestaan. Een FMX-port
is een aparte productkeuze; hij is niet nodig voor de afgesproken FPC-library.

## Build, tests en afwerking

### G13 — Delphi-CI kan nog niet direct worden ingeschakeld

[CI](../.github/workflows/ci.yml) schakelt Delphi-builds bewust uit met
`if: false`, conform het uitstel in deze ontwikkelfase. De opdrachten verwijzen
naar `.dproj`-bestanden voor beide packages en de tests, die niet aanwezig zijn.
Alleen een runner toevoegen en de gate verwijderen is dus niet voldoende.

**Voorbereiden:** concrete buildprojecten of reproduceerbare generatie/directe
compileropdrachten, DUnitX-dependencybeheer en een echte package-install-smoke.
Controleer VCL-rendering en de daadwerkelijk ondersteunde Delphi-versies. Deze
voorbereiding kan nu; echte Delphi-builds blijven in de afgesproken latere fase.

### G14 — Vergroot de gedragsdekking gericht

De 272 gecompileerde units hebben gezamenlijk 38 FPC-runtimecontroles; de 185
andere checks zijn codecs. De 103 DUnitX-units zijn aanwezig maar niet uitgevoerd.
Geen percentage codecoverage is gemeten. De nieuw gevonden checkpoint- en
TLS-fouten tonen waarom compilatie en statische analyse onvoldoende zijn.
De volledige FPC-runtime gebruikt bovendien een gepinde 3.3.1-developmentcompiler;
FPC 3.2.2 dekt alleen de codecsubset. Windows FPC is nog geen geverifieerd profiel.
Leg deze compiler- en platformgrenzen vast als expliciete releasevoorwaarden.

**Toevoegen:** regressies voor G01–G08; daarna een simulator met transportfouten,
UDS response-pending/NRC's, timeouts, reconnect, gesegmenteerde responses en
transfer-recovery. Voeg een afzonderlijk benchplan toe voor Windows-backends,
Bluetooth, ECU-specifieke sessies en schrijfacties. Maak rapportages onderscheidend:
compile-pass, gedrag getest, bench-getest en nog niet getest.

### G15 — Afbeeldingen en UI verdienen gerichte afwerking

Er zijn 212 PNG's van 256×256 en twee van 1024×1024. Het geïnspecteerde about-
beeld en connection-icoon delen een donkere, gedetailleerde 3D-stijl. Dat biedt
een herkenbare basis; de leesbaarheid op IDE-paletmaten 16/24/32 px is niet
bewezen door de grote bronafbeeldingen. Geen volledige Delphi-UI bekeken.

**Aanbevolen:** vereenvoudigde kleine icoonvarianten met duidelijke silhouetten,
consistent contrast op lichte/donkere achtergronden en een gecontroleerde
resource-export. Voeg echte screenshots toe van dashboard, DTC-lijst,
EV-overzicht en flashworkflow op 100/150/200% DPI. Prioriteit ligt bij functionele
statusweergave: verbonden, time-out, onbeschikbare meting en ongeteste capability.
Nieuwe grote merkafbeeldingen hebben minder waarde dan die controle.

### G16 — Verwijder of isoleer niet-relevante gekopieerde tools

Voorbeelden: `tools/playerlog.py`, `make-trakticon.py` en media-/wizardicon-
generatoren bevatten player/Trakt-functionaliteit die niet bij deze library hoort.
Vijf statische checkers zijn reeds expliciet niet van toepassing. Die skips
zijn eerlijk, maar de toolmap is nog geen heldere repo-specifieke gereedschapsset.

**Aanpassen:** actieve ERD-tools documenteren met input/output/dependencies;
legacy-tools verplaatsen of verwijderen na referentiecontrole. Houd de
analyzers, catalogusauditor en beide FPC-profielen. Voeg ontbrekende controles
op ownership, schrijflengte en semantische catalogusvectors toe als regressies;
maak heuristieken niet de vervanger van uitvoerbare tests.

## Aanbevolen uitvoeringsvolgorde

1. **Betrouwbare basis:** G01–G03 herstellen met regressies die eerst de fouten
   aantonen. Daarna G04–G08, met nadruk op data-integriteit en recovery.
2. **Eerlijke functionaliteit:** G09/G10 capability-contracten herstellen;
   G11/G12 documentatie en supportmatrix laten aansluiten op daadwerkelijk gedrag.
3. **Stabiele validatiefase:** G13 voorbereiden en G14 simulator/benchdekking
   uitbreiden; echte Delphi-builds uitvoeren wanneer de stabiele versie gereed is.
4. **Productafwerking:** G15/G16: UI-statussen, kleine iconen, screenshots en
   onderhoudbare tooling. Nieuwe protocollen of bredere OEM-claims pas toevoegen
   wanneer de bestaande paden betrouwbaar getest zijn.

Een stabiele-releasebeslissing hoort minstens de drie P0's te sluiten, de
risicovolle P1-paden aantoonbaar te testen en resterende productbeperkingen in
de supportmatrix vast te leggen. Dit rapport is een onderbouwde inventarisatie,
geen volledige security-audit of certificering van voertuigcompatibiliteit.
