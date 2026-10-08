# Opschoning en Delphi-overdracht — 2026-10-08

Vervolg op de [P0/P1-reparaties](high-priority-fixes-2026-10-08.md).
De huidige scope is expliciet **Delphi VCL**, met een **niet-visuele FPC-library**.
FMX is op verzoek buiten scope; er wordt geen FMX-port toegevoegd.

## Uitgevoerd

| Punt | Resultaat |
|---|---|
| G11: EV-dekking | [Gegenereerde supportmatrix](ev-support-matrix.md) met alle 15 vendorcatalogi, gedeclareerde modellen, ECU-adressen, uitvoerbare veldnamen/aantallen en primaire bron. Zeven lege catalogi zijn expliciet niet ondersteund. Modeljaren staan alleen vermeld waar de bron ze bevat; geen benchvalidatie geclaimd. |
| G12: UI-scope | README, compilerdocumentatie, source-layout en actieve plannen beschrijven VCL-only. De dependencyguard controleert alle source-lagen en verbiedt FMX-imports ook in UI/design-time. Historische v1-inventarissen blijven als vergelijking herkenbaar. |
| G15: IDE-assets | Alle 229 geregistreerde componenten hebben nu een named PNG-resource. Voor 27 ontbrekende iconen zijn expliciete aliases naar verwante bestaande iconen toegevoegd. De 204 bestaande native PNG-payloads zijn byte-voor-byte uit de vorige resource overgenomen, zonder conversie of nieuw artwork. PNG-integriteit, afmetingen, resourcetypen, registratiecoverage en reproduceerbare RES-output worden gecontroleerd. |
| G16: tooling | 23 ongebruikte media/playlist/translation/artwork-scripts verwijderd na referentiecontrole. Vijftien app-specifieke checkers verwijderd; de platformchecker is vervangen door een controle van de drie ERD-projecten. De overgebleven 97 checkers draaien zonder ontbrekende-input-skips. [Toolinventaris](../tools/README.md) beschrijft input/output, dependencies en opdrachten. |
| Delphi-projecten | Tracked projecten declareren RT/test Win32+Win64 en DT Win32; Debug/Release-flags zijn expliciet en DCU/BPL/DCP-output is gescheiden per platform/configuratie en compileerdoel. Alle 644 lokale DCCReferences bestaan. |
| Delphi-testworkflow | [PowerShell-script](../tools/validate_delphi.ps1) voor clean rebuild van packages/tests, foutcodes, root-werkdirectory en een niet-leeg NUnit-resultaat. CI gebruikt dezelfde workflow zodra de bewust uitgeschakelde Windows-job wordt geactiveerd. Niet-aanwezige lint/coverage-toolbeloftes zijn gecorrigeerd. |

## Aanvullende VCL-correcties

- De connection-state-lamp gaat terug naar disconnected bij detach/removal;
  een eerdere connected/error-status blijft niet hangen.
- De celspanningsheatmap toont bij een lege dataset `No cell data`.
  NaN/infinity/nonpositieve celspanningen worden neutraal en, met ShowText,
  `N/A`; ze worden niet als een gezonde meting weergegeven. Niet-eindige
  schaalgrenzen worden afgewezen en updates van ontbrekende cellen zijn veilig.
- Retained log/status-callbacks van de IDE live-testdialog controleren een
  onafhankelijke lifetime-token voordat zij de dialoog benaderen.
- Een mislukte PNG-load ruimt het gedeeltelijk aangemaakte TPngImage op.
- De terminal bevat geen nutteloze self-assignment meer.

DUnitX-regressies voor verbindingsstatus en ongeldige heatmapmetingen zijn
bijgewerkt/toegevoegd. Deze VCL-tests worden pas tijdens de Delphi-run uitgevoerd.

## Validatie in deze ronde

| Controle | Uitkomst |
|---|---|
| Statische Pascal-checkers | 97 uitgevoerd, nul bevindingen, nul skips |
| Analyzer-regressies | 11 geslaagd, inclusief flash-layer/VCL en FMX-UI-afwijzing plus verkeerde DT-platformen |
| Catalogue-/repositorytooltests | 18 geslaagd; ontbrekende iconen, corrupte PNGs en vendor-mismatch worden afgewezen |
| Catalogusschema’s | 248 gecontroleerd, nul uncovered, nul violations |
| FPC 3.2.2 portable profiel | 185 checks geslaagd |
| EV-matrix | Actueel volgens alle 15 catalogusbestanden en manifest |
| IDE-resource | 229 componenticonen plus About/Splash, exacte reproduceerbare output |
| Delphi-projecten | XML en 320 RT + 20 DT + 304 testreferences gecontroleerd |
| Diff | Geen whitespacefouten |

De niet-visuele Pascal-bronnen en catalogusdata zijn in deze ronde niet gewijzigd.
De eerdere volledige FPC-validatie (273 units, 193 runtimechecks, zes signaturechecks,
zeven TLS-scenario’s) blijft van toepassing op diezelfde bronnen. Die complete
build is hier niet opnieuw uitgevoerd voor uitsluitend VCL-/documentatie-/toolingwijzigingen.

## Overdracht

De resterende werkstap is daadwerkelijke Windows/Delphi-validatie, IDE-installatie,
rendering/DPI-review en daarna adapter-/ECU-benchtests. Daar kunnen nieuwe fouten
uit komen; deze opschoning is geen claim dat die ongevoerde tests al slagen.

[De concrete checklist en opdrachten](delphi-validation.md) staan klaar.
Nieuwe artwork is optioneel: de huidige resources zijn compleet, maar visuele
kwaliteit op alle IDE-maten/stijlen moet in Delphi beoordeeld worden. Er zijn
geen fictieve VCL-screenshots of nieuwe onbewezen EV-decoders toegevoegd.
