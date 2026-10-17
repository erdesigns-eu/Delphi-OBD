# TERDLEDView

Zelfstandige, geregistreerde ERD-control voor RGB-pixels. Geen IDE-, TSVM-,
USB-, boardtemplate- of animatietimerafhankelijkheid. De component gebruikt
`TERDInteractiveControl` en de gedeelde antialiased GDI+-painter.

## Layout en kleurframes

```pascal
LEDView.LEDCount := 24;
LEDView.Layout := ldlRing;
LEDView.StartAngle := -90;
LEDView.Clockwise := True;
LEDView.PixelColors[0] := RGB(255, 80, 0);
LEDView.SetColors(Frame); // precies LEDCount TColor-waarden
```

- `Layout`: `ldlHorizontalStrip` (standaard), `ldlVerticalStrip`, `ldlRing`.
- `LEDCount`: 1–4096; standaard 16. Indexen zijn overal **nul-gebaseerd**.
- `PixelSize`: logische 96-DPI diameter (4–128, standaard 24).
- `Spacing`: logische tussenruimte (0–128, standaard 8).
- De geometrie past zichzelf binnen de clientruimte; pixels schalen kleiner
  wanneer dat nodig is. Er is geen automatische vergroting boven PixelSize.
- De ring gebruikt graden, met 0 rechts en -90 boven. Richting en starthoek zijn
  instelbaar. Eén pixel wordt gecentreerd. Een te klein viewport levert lege
  bounds en geen hit targets op.
- `Brightness`: 0–255; schaalt uitsluitend de gerenderde RGB-waarden. Het
  oorspronkelijke frame blijft behouden. Dit is geen elektrisch stroommodel,
  gammacorrectie of RGBW-protocolconversie.
- `ShowIndices`: tekent indexen binnen pixels die daarvoor groot genoeg zijn.
- `clBlack` is uit, `clNone` toont een neutraal, niet-toegewezen pixelvlak.

`Colors` geeft een kopie van het frame. `SetColors` valideert de lengte voordat
iets verandert. Frames worden gekopieerd, niet geleend. Verkleinen van LEDCount
bewaart de overblijvende kleuren en verwijdert indexen die niet meer bestaan;
nieuw toegevoegde pixels starten zwart.

`OnChange` meldt kleur- of aantalwijzigingen. Identieke frames geven geen event.
`BeginUpdate` / `EndUpdate` voegen meerdere updates samen tot één kleur-event en
hoogstens één selectie-event. Gebruik try/finally. Een losse EndUpdate is een
fout. `FillColor(Color, SelectedOnly)` kan alle of alleen geselecteerde pixels
wijzigen. Layout en weergavehelderheid veranderen het bronframe niet.

## Selectie en inspectie

Klik selecteert één pixel. Ctrl-klik toggelt, Shift-klik selecteert een bereik;
Ctrl+Shift bewaart ook de vorige selectie. De pijlen, Home/End, Shift, Ctrl+A,
Spatie en Escape ondersteunen bediening met het toetsenbord.

```pascal
LEDView.SetSelection([0, 3, 7]);
LEDView.FillColor(clLime, True);
Selection := LEDView.SelectedIndices; // kopie, oplopend
```

`Selected[Index]`, `SelectionCount` en `CurrentIndex` zijn read-only.
CurrentIndex kan een niet-geselecteerde pixel aanwijzen voor toetsenbordfocus.
`OnSelectionChange` meldt selectie-/focusindexwijzigingen; `OnPixelClick` geeft
index en modifiers aan de host door. De control bevat geen inspector of
pinassignment. `SelectionEnabled=False` blokkeert gebruikersselectie en
pixelacties; programmatische selectie en updates blijven mogelijk.

`PixelBounds(Index)` geeft een `System.Types.TRectF` in clientcoördinaten.
`PixelAt(X,Y)` gebruikt de ronde pixelvorm en geeft -1 voor lege ruimte. Tekenen
en hit testing gebruiken dezelfde geometrie. Office-style ERD-hints tonen het
pixelnummer, de bronkleur en de weergavehelderheid met de actuele theme/density.
Bij grote aantallen kunnen pixels erg klein worden; programmatische selectie en
toetsenbordbediening blijven beschikbaar.

## Opslag en integratie

DFM bewaart layout/aantal/weergave-instellingen. Runtime-kleuren en selectie
worden niet opgeslagen: projectopslag hoort bij de toekomstige host. Alle API en
events draaien op de UI-thread; marshal ontvangen hardwareframes naar die thread.
Animatie, simulatieregels en hardwarecommunicatie worden door de host bepaald.
Een LED-ring kan daardoor later zowel previews als echte hardwarestatus tonen.

Er zijn voor deze component geen demo-uitbreidingen toegevoegd. Plaats
`TERDLEDView` vanaf het Delphi-palet op een eigen form of instantieer hem direct.

## Verificatie

De Windows-smoketest `TestLEDView` dekt framekopieën en validatie, batchupdates,
selectie/range/toggle, selectie na resize, toetsenbord/muis, cirkel-hit testing,
strip-/ringgeometrie, richting/starthoek, RGB/helderheid in light en dark theme,
4096 pixels, lege viewports en DFM zonder runtime-data. Deze VCL-test moet op
Delphi Windows worden uitgevoerd; de Linux-checks kunnen geen VCL renderen.
