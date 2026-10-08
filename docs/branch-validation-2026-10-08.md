# Branchvalidatie — claude/v2-phase-1

Beoordeeld op 8 oktober 2026. De eerste reparaties staan in commit `1009218`;
de volgende reparatieronde omvat onderstaande wijzigingen. De controles zijn
lokaal uitgevoerd. Een GitHub-run of Windows-build is daarmee niet aangetoond.

## Uitgevoerde reparaties

- KWP ReadID, ReadDTC, I/O-control, routines en session hub hebben beheerde,
  annuleerbare workers, progress-events en cleanup van queued callbacks.
  Keepalive wordt eerst geïnitialiseerd en bij stoppen gejoined.
- VIN-decodering ondersteunt exact drieletterige WMI's, tweeletterige
  fabrikantprefixen en zesletterige low-volume WMI's met het verplichte `9`.
  Generieke en extended VDS-regels worden gecombineerd zonder fabrikanten te mengen.
- CAN-routing wordt daadwerkelijk uitgevoerd: header, ontvangstfilter en
  extended adresbytes worden samen met de diagnostische aanvraag geserialiseerd.
  Een afgewezen adapterinstelling verhindert verzending van de aanvraag.
- EV-regels erven ECU-routing en kunnen die per veld overschrijven. Antwoorden
  worden op SID en DID/PID-echo gecontroleerd; de echo wordt vóór decodering
  verwijderd. Alle gedecodeerde vendorvelden blijven beschikbaar in
  `Snapshot.DecodedFields`, naast de standaardvelden.
- Resterende capaciteit in Ah heeft een eigen veld en wordt geen SOH-percentage.
  VW e-Up DIDs, routing, signedness, schaalfactoren en cellayouts zijn gecontroleerd
  tegen de openbare OVMS decoder/header. Gen1 en Gen2 hebben afzonderlijke regels;
  stel `ModelYear` in voor modelafhankelijke catalogi.
- OEM coding/adaptations ondersteunen signed int8 en expliciete bytepayloads,
  inclusief async bytewrites. Numerieke breedtes worden gecontroleerd vóór
  verzending. Big-endian waarden worden als big-endian gelezen en geschreven;
  ASCII `bit_width=80` betekent tien bytes. Onaangeroerde codingbytes blijven behouden.
  Write-antwoorden moeten hun DID correct echoën.
- Een afgekapt XCP ERR-antwoord wordt gecontroleerd vóór het lezen van de foutbyte.
  Twee ongebruikte private UI-methoden zijn verwijderd.
- Alle 248 datacatalogi hebben een expliciet lokaal schema. Dertig schemas dekken
  de werkelijk gebruikte catalogusfamilies. De auditor controleert ook dubbele
  JSON-keys, niet-eindige getallen en EV-manifestverwijzingen. Externe schema's
  worden nooit via het netwerk opgehaald.
- CI voert strikte Pascalanalyse uit, inclusief hints, en vereist volledige
  catalogusdekking. De FPC-smoke compileert elf echte bronunits in Delphi-mode;
  alleen RTL-namespaces worden in tijdelijke kopieën aangepast.

## Uitgevoerde controles

| Controle | Resultaat |
|---|---|
| Strikte Pascalcheck | 107 toepasselijke checkers; nul fouten en nul hints |
| Runtime/VCL-FMX-grens | 93 runtime-units; nul verboden imports |
| Catalogusaudit met verplichte dekking | 248 gecontroleerd; nul ongedekt; nul overtredingen |
| Python-regressies | 7 analyzer-tests en 12 catalogusauditor-tests geslaagd |
| FPC 3.2.2, `-Mdelphi` | 185 uitvoerbare controles geslaagd |
| Git whitespacecontrole | `git diff --check` geslaagd |

Vijf meegekopieerde checkers voor appvertalingen en een projectspecifieke
crypto-lijst zijn expliciet niet van toepassing op deze library. Ze worden
als zodanig gerapporteerd; er zijn geen bevindingen onderdrukt met `--errors-only`.

De branch bevat 340 bronunits en 103 DUnitX-testunits met 1.285 testmethoden.
De nieuwe regressies behandelen VIN-matching, adapterrouting en serialisatie,
OEM schrijfbytes/coding en de EV catalogi. Ze staan in de runner maar zijn
**nog niet onder Delphi uitgevoerd**. FPC valideert de portable units en
hun gedrag; het vervangt Delphi's anonymous methods, VCL, FMX of DUnitX niet.

## Brondata en productondersteuning

Twee oorspronkelijke vPIC-schema's bevatten de verboden VIN-letter `O` en
konden geen geldige VIN matchen. Ze zijn uit de actieve data gehaald; de
volledige oorspronkelijke records en reden blijven bewaard in
`catalog-source-normalization.json`. De legacy wildcard `2Gx` is genormaliseerd
naar de ondersteunde fabrikantprefix `2G`. Er zijn geen fabrikantidentiteiten
of vervangende DIDs geraden.

EV-catalogi zonder publiek gedocumenteerde DIDs blijven expliciet zonder regels.
Teksten zoals `unknown-public` en `n/a` staan niet langer in uitvoerbare
CAN-ID-velden. Een geslaagde schema-audit bewijst geen voertuigondersteuning.
De VW-catalogus beschrijft nu de e-Up/Citigo-e/Mii en claimt geen MEB-ondersteuning.
De geraadpleegde bron-URLs en content-hashes staan in het normalisatiedocument.

## Verificatie voor release

Volgens de afgesproken volgorde komen echte Delphi-builds zodra de branch
stabiel is. Daarna zijn DUnitX, package-installatie, UI-controle en benchtests
nodig voordat productiegeschiktheid kan worden vastgesteld. FMX-mirrors en
ongedocumenteerde voertuigspecifieke DIDs blijven productuitbreidingen.

Benchplan zonder schrijfoperaties: gebruik eerst een simulator met verwachte
SID/DID-echo's; controleer afgekapt en foutief antwoord, normale/extended routing,
SME/KOMBI wisseling, en twee gelijktijdige aanvragen. Vergelijk daarna op een
compatibele ELM327/OBDLink de adaptertrace met de catalogus-CAN-IDs. Controleer
beide e-Up modelgeneraties en celvolgorde. Een adapter die `ATCEA`/`ATCER` niet
ondersteunt moet vóór de diagnostische aanvraag stoppen. OEM writes worden
uitsluitend op een simulator/bench gevalideerd met een bekende herstelprocedure;
er zijn tijdens dit werk geen echte ECU-writes uitgevoerd.
