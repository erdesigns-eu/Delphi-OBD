# Branchbeoordeling — claude/v2-phase-1

Beoordeeld op 8 oktober 2026, vanaf `df09531` (toevoeging van de tools).
Wijzigingen en controles zijn lokaal uitgevoerd. Een GitHub-run, Windows-build,
IDE-installatie en benchtest zijn hiermee niet aangetoond.

## Wat is aanwezig?

Deze branch is inmiddels de v2-herschrijving, veel verder dan de branchnaam
suggereert: 337 bronunits, 101 testunits met 1.268 `[Test]`-methoden,
48 voorbeeldprojecten en 259 JSON-bestanden onder `catalogs/`. Alle testunits
staan in de runner. Er zijn 42 VCL UI-units; `src/UI.FMX/` ontbreekt.
De bron bevat 47 OEM-extension-classdeclaraties, inclusief infrastructuur;
dat is geen claim van 47 volledig gevalideerde fabrikanten.

Het plan markeert fase 0–12 grotendeels als geïmplementeerd. De belangrijke
releasevoorwaarden zijn niet bewezen: builds op de ondersteunde Delphi-versies,
DUnitX-uitvoering, coverage, IDE-installatie, screenshots en hardwarevalidatie.
Fase 13 (beta/RC, bug bash, GetIt en release) staat nog open. Er is daarom geen
onderbouwd percentage voor productiegeschiktheid.

## Aangetoonde resultaten

| Controle | Resultaat |
|---|---|
| FPC 3.2.2, `-Mdelphi` | 154 uitvoerbare controles geslaagd |
| Regressies voor de Python-checkers | 7 analyzer-tests en 5 catalogusaudit-tests geslaagd |
| Pascalcheck | 107 checkers uitgevoerd; geen foutmeldingen; 3 private-method-hints |
| Niet-toepasselijke checks | 5 expliciet overgeslagen: vertaalcatalogi en de expliciete crypto-lijst uit het oorspronkelijke project |
| Runtime/VCL-FMX-grens | 90 runtime-units gecontroleerd; geen verboden imports |
| Catalogusbestanden | Alle 259 bestanden parsen als JSON; dit is geen volledige schemavalidatie |
| Git-diff | Geen whitespacefouten |
| Volledige Delphi-build/DUnitX | Niet uitgevoerd: RAD Studio ontbreekt in deze Linuxomgeving |

FPC compileert `OBD.Version` ongewijzigd. Voor de overige geselecteerde units
worden uitsluitend `System.SysUtils`/`System.Variants`-namen aangepast in
tijdelijke kopieën. Er worden geen Delphi-, VCL- of hardware-API's nagebootst.
De FPC-controles omvatten foutcodes, LIN-pariteit en checksum-goldens,
LIN/MOST/FlexRay-roundtrips, corrupte checksums/CRC's en extra framebytes.

## Herstelde fouten

- LIN en FlexRay accepteerden extra bytes achter complete frames. De nieuwe
  tests faalden vóór de reparatie en slagen erna. Ook DUnitX-regressies toegevoegd.
- KWP1281 ELM-eventhandler miste de `Sender`-parameter.
- OEM-catalogusreader bevatte een syntactisch ongeldige `out`-parameter.
- Sommige catalogusklassen overschaduwden de intrinsieke `Default`-functie.
- Directe imports ontbraken voor gebruikte types, enumwaarden en RTL-functies.
- J1939 NAME-byte-accessors botsten met de case-insensitieve bulk-setternaam.
- Winsock-connectsetup gebruikte problematische macro-/constantvormen.
- De fuel-trim-weergave gebruikte een C-formatflag die Delphi niet ondersteunt.
- Managed callbackparameters zijn behouden bij capture; de DTC-editor heeft
  nu een loopvariabele binnen zijn callback.
- 39 DUnitX-eventassignments koppelden anonymous methods aan `of object`-events.
  Fixtures gebruiken nu object-method-adapters met fixture-owned callbacks.
- Tests riepen private VIN-methoden aan; ze gebruiken nu de publieke `Read`-API.
- Catalogusreaders controleren JSON-object-/stringvormen met `OBD.JSON`.
  Niet-objectroots worden vrijgegeven en afgewezen met `EOBDConfig`.
  Zes DUnitX-tests toegevoegd voor deze vormen en ownership; niet uitgevoerd.

De tools zijn aangepast voor bronpaden, dotted unitnames, lege exceptionklassen,
abstracte klassen/attributes, conditionele compilerbranches, optional arguments,
lege arrayargumenten en scoped exceptionvariabelen. Een positieve en negatieve
fixture beschermt de relevante parsercorrecties. Homogene LF/CRLF-bestanden zijn
geaccepteerd; deze repository heeft geen CRLF-pinning in `.gitattributes`.

CI voert voortaan de analyzerregressies, `pascalcheck --errors-only` en FPC-smoke
uit. Hints blijven zichtbaar; de standaard pascalcheck-opdracht blijft strikt.
De echte Delphi-buildjob blijft uitgeschakeld en vereist een Windows-runner.

## Wat ontbreekt of verdient prioriteit?

De mergevoorbereiding en de latere stabiele-versievalidatie zijn nu apart
geprioriteerd. Op verzoek van de eigenaar stellen we echte Delphi-builds en
DUnitX-uitvoering uit tot de stabiele-versiefase. Onderstaande items vormen
geen bewijs dat de branch nu al mergeklaar is.

| Volgorde | Werk | Status / acceptatie |
|---|---|---|
| 1 | KWP IOControl/Routine async-API en workerbeheer | Geïmplementeerd; 10 DUnitX-regressies toegevoegd, nog niet uitgevoerd |
| 2 | Bestaande KWP async-lifecycle controleren/herstellen | Geïmplementeerd: owned workers en cleanup in ReadID, ReadDTC en session hub; 15 extra DUnitX-regressies, uitvoering uitgesteld |
| 3 | Catalogus-schema's, referenties en provenance | Audittool en 5 regressies toegevoegd; 11.045 schema-afwijkingen in 17 bestanden nog te triëren; 193 bestanden zonder lokaal opgelost schema |
| 4 | Testkwaliteit en merge-CI | Portable gates aanwezig; controleer lege/stub-helpers en zelfgekopieerde parserlogica in tests voordat die als bewijs dienen |
| 5 | Backlog en scope voor de merge | Bijgewerkt: bronimplementatie onderscheiden van platform-, data- en benchacceptatie |
| Later | FMX, nieuwe palettecomponenten en productbeelden | Functionele uitbreiding; apart plannen, echte UI-screenshots na een werkende UI-build |
| Stabiele versie | Delphi/DUnitX, IDE-installatie, coverage en bench/beta/RC | Uitgesteld; nodig vóór een productieclaim |

### Eerste implementatie: KWP async

`TOBDKWPIOControl` heeft nu `SendLocalAsync` en `SendCommonAsync`;
`TOBDKWPRoutine` heeft `StartAsync`, `StopAsync` en `RequestResultsAsync`.
De bestaande synchronous methoden blijven de requestopbouw en veiligheidsregels
uitvoeren. Invoerarrays worden vóór het starten gekopieerd. Eén operatie per
instantie mag tegelijk actief zijn; een tweede start geeft `EOBDConfig`.

De twee componenten bewaren een worker met `FreeOnTerminate = False`.
De hoofdthread ruimt die na afronding op. `CancelAsync` en de destructor
annuleren aflevering, wachten op de worker en verwijderen diens queued events.
Protocol-vervanging tijdens een operatie wordt afgewezen. Resultaten, fouten
en request/response-voortgang worden op de hoofdthread afgeleverd; I/O-consent
blijft een synchronisatiepunt op de hoofdthread.

Annulering kan een al verstuurd ECU-commando niet ongedaan maken; wachten is
begrensd door de onderliggende protocol-/adaptertimeout. Starten, annuleren en
vernietigen van een actieve async-component horen op de hoofdthread. Een
`OnBeforeSend`-handler geeft afwijzing via `ACancel`; hij mag de component niet
vernietigen of een blokkerende `CancelAsync` uitvoeren terwijl de worker op
zijn toestemming wacht.

Tien DUnitX-tests dekken main-threadfouten, beide safety gates, afwijzing door
de consentcallback, overlap, annulering/herstart en vernietiging zonder latere
callbacks. Dit zijn hardwarevrije regressies; uitvoering is uitgesteld samen
met de Delphi-build. De huidige FPC-smoke compileert deze Delphi anonymous-
method/threadcomponenten niet en bewijst hun gedrag dus niet.

### Vervolg: bestaande KWP-workers

ReadID, ReadDTC en de session hub gebruiken nu hetzelfde owned-workerpatroon
als IOControl/Routine: main-thread-start, één actieve async-operatie,
`FreeOnTerminate = False`, automatische cleanup, cancellatie vóór het
versturen en verwijdering van workergebonden events bij annulering/destructie.
Ze leveren request/response-voortgang op de hoofdthread. Een Protocol-wijziging
wordt geweigerd zolang een async request actief is.

De keepalive-thread wordt eerst volledig geïnitialiseerd en daarna gestart.
Stoppen wacht de thread af en verwijdert zijn queued callbacks. Protocol-
vervanging stopt eerst de keepalive-thread en start hem met de nieuwe binding
opnieuw wanneer keepalive nog ingeschakeld is. De session hub onderdrukt
callbacks tijdens destructie; annulering van een session request dempt geen
losstaande keepalive-events.

Vijftien extra hardwarevrije DUnitX-regressies dekken voor deze drie componenten
async-fouten op de hoofdthread, overlap, annulering/herstart, vernietiging en
automatische cleanup die een volgende operatie vrijgeeft. Deze tests zijn
nog niet uitgevoerd; FPC compileert ook deze threadcomponenten niet.
De volgende prioriteit is catalogus-schema-/referentie-/provenancevalidatie.

### Catalogus-schema-audit

`tools/validate_catalogs.py` valideert offline tegen expliciet aangewezen,
lokaal beschikbare schema's. De tool weigert dubbele JSON-objectkeys en
netwerkretrieval van `$ref`-schema's. Een onbekend of ontbrekend schema wordt
als **uncovered** gerapporteerd. `--require-coverage` maakt ook ontbrekende
dekking een fout; zonder die optie blijven eventuele schemafouten exitcode 1.
De vijf regressies testen geldige/ongeldige data, onbekende schema's, dubbele
keys, relatieve schemapaden en geweigerde externe referenties.

De eerste volledige audit levert **55 gecontroleerde data-bestanden**, **193
uncovered-bestanden** en **11.045 schema-afwijkingen in 17 bestanden** op;
11 schema-definitiebestanden worden apart geladen. Verdeling:

- VIN: 10.988 afwijkingen, waaronder zes tekens lange extended-WMI-identifiers
  die een drie-tekens-schema afwijst. Readergedrag en low-volume-VIN-regels
  moeten samen worden beoordeeld; afkappen kan verschillende fabrikanten
  samenvoegen.
- EV battery: 55 afwijkingen, waaronder extra metadata/ECU-velden en een
  manifest dat het vendorschema aanwijst. Onderscheid manifest-/vendorvormen
  en controleer of de reader de ECU-velden werkelijk gebruikt.
- Agricultural: 2 afwijkingen voor J1939-SPN-codes in John Deere-data die een
  uitsluitend J2012-codepatroon afwijst. Bepaal de ondersteunde codefamilie
  expliciet in schema en reader.

Deze meldingen betekenen een mismatch tussen schema en data, niet automatisch
11.045 foute ECU-waarden. Er zijn nog geen data verwijderd, identifiers
verkort of schemastrictheden versoepeld. De audit blijft rood totdat deze
verschillen zijn opgelost. CI draait voorlopig de regressies van de audittool;
de volledige catalogusaudit is nog geen groene merge-gate. CI wordt nu ook
voor pull requests naar `main` geactiveerd.

Reproduceerbaar uitvoeren:

```sh
python3 -m pip install -r tools/requirements-validation.txt
python3 -m unittest discover -s tools -p test_validate_catalogs.py -v
python3 tools/validate_catalogs.py --report /tmp/catalog-schema-audit.json
```

### Overige bevindingen

De zes ongebruikte KWP-helperhints zijn vervallen doordat de async-API ze nu
gebruikt. De drie resterende hints betreffen XCP.SendCommand,
UI.Control.StyleStorageWriter en UI.Knob.AngleToValue; ze zijn geen
compilefouten. Bij aanvullende review zijn types uit fixturecallbacks naar
interface-uses verplaatst: implementation-uses maken types niet zichtbaar in
de interface. De statische checker zag die scopegrens nog niet voldoende.

P-B1/B4/B5/B6 hebben code op de branch en zijn in het quick-pick-overzicht
als gedeeltelijk geaccepteerd gemarkeerd. Vendorcorrectheid, tachograafcaptures,
platform/security-tests en IDE-acceptatie blijven open. Mode 07/0A bestaan als
DTC-leespaden; aparte palettecomponenten zijn voorgestelde uitbreidingen.
Radio-vendor-`OnCalculate`-stubs blijven gedocumenteerd in
`docs/radio-code-algorithms.md`.

De Delphi-CI gebruikt `.dproj`-bestanden die volgens repositorybeleid niet
worden ingecheckt. Reproduceerbare generatie en Windows-runnerconfiguratie
horen bij het latere activeren van die job. De 48 samples, FMX/HiDPI/thema's,
coverage en benchreleasecriteria zijn daarmee nog niet gevalideerd.

## Afbeeldingen

`assets/designtime/about.png` en `splash.png` delen al een consistente donkere
stijl met een oranje diagnoselijn. De About-afbeelding heeft veel marge; de
splash is compacter. Ze kunnen behouden blijven als decoratieve branding.

De grootste verbetering is echte productbeeldvorming: een screenshot van een
live-data-dashboard, van de componentpalette en van de coding/flash-workflow.
Gebruik voor documentatie een technisch correct 16-pins OBD-symbool en een
architectuurdiagram. Ontwerp kleine, consistente SVG/PNG-iconen voor de
palette/wizards met lichte en donkere varianten. Genereer geen fictieve
screenshots alsof het geteste interfacebeelden zijn. Nieuwe visuele features
zijn voorstellen; er zijn in deze ronde geen nieuwe afbeeldingen gegenereerd.

## Opnieuw controleren

```sh
python3 -m unittest discover -s tools/pascalcheck -p test_checkers.py -v
python3 tools/pascalcheck/run.py --errors-only -v
python3 tools/fpc_smoke.py
```

In deze cloudmachine staat de lokaal uitgepakte compiler onder
`/workspace/onboarding-delphi-obd/fpc/usr/lib/x86_64-linux-gnu/fpc/3.2.2/ppcx64`;
geef dat pad mee met `--compiler`. De volledige statische uitvoer staat buiten
de checkout in `/workspace/onboarding-delphi-obd/v2-check-verified.txt`.
