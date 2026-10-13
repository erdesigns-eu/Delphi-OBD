# ERD Components

Zelfstandige algemene **Delphi VCL**-componentlibrary. Dit zijn onafhankelijke
kopieën: Delphi-OBD wordt niet aangepast en heeft geen afhankelijkheid van deze
library. De hele map kan naar een eigen repository worden verplaatst.

- **60 paletcomponenten**: algemene invoer, navigatie, containers, grids, dialogen,
  applicatiechrome, theming en feedback. Zie `assets/components.json`.
- **320 ingebouwde vectoriconen**, met stabiele identifiers en schaalbare SVGs.
- Eigen namespace `ERD.Components.*`, classes `TERD*`, interfaces `IERD*`.
- VCL/Windows; geen FMX- of Lazarus-UI-port.

## Installatie

1. Open `packages/ERDComponents_DT.dproj` in Delphi.
2. Selecteer Win32 voor de 32-bit IDE, of Win64x voor een compatible 64-bit IDE.
3. Build en Install. DT bevat alle componentbronunits; geen eigen runtime-BPL nodig.
4. De componenten verschijnen op `ERDesigns Components`.
5. Open `demo/ERDComponentsDemo.dproj` voor het voorbeeldprogramma.
   `Showcase.dfm` bevat de componenten, ribboncollecties en alle 320 iconknoppen;
   je kunt ze rechtstreeks in de Delphi-formdesigner bekijken en bewerken.

`ERDComponents_RT` is apart beschikbaar voor applicaties die runtime-packages
gebruiken (Win32/Win64). Installeer RT niet daarnaast in dezelfde IDE: standalone
DT bevat dezelfde units. Bronapplicaties voegen `src/UI` aan hun zoekpad toe.
BPL-uitvoer: `$(BDSCOMMONDIR)/Bpl/ERDComponents/<platform>`.

## Themes en ribbonvorm

Standaard is **ERDesigns Light**, ook als er geen theme-controller is. Een
`TERDTheme` biedt ERDesigns Light/Dark, Windows, Office Light/Dark, Ocean
Light/Dark, Forest Light/Dark en High Contrast. Automatic is optioneel.
`ERDThemeName` levert de weergavenaam; custom palettes blijven ondersteund.

De ribbon heeft drie onafhankelijke keuzes:

- `Theme`: kleuren en licht/donker.
- `Appearance`: `raERDesigns`, `raOffice`, `raFlat`.
- `RibbonStyle`: `rsClassic` of `rsSimplified` (layout).

Office geeft een afgerond commandopaneel, subtiele rand/schaduw en neutrale
File-tab, zonder Microsoft-logo's of productspecifieke acties. De demo bevat
keuzelijsten voor themes, vorm en layout.

## Iconen, ImageLists en eigen glyphs

Buttons en navigation-/ribbon-/toolbar-/tab-/status-/titlebar-/tile-items hebben
`Icon`, met een expliciete `Source`: `eisNone`, `eisBuiltIn`, `eisImageList`,
`eisGlyph`. `Icon.BuiltIn` selecteert één van de 320 catalogusiconen.

```pascal
Button.Icon.Source := eisBuiltIn;
Button.Icon.BuiltIn := ei161Afspelen;

Button.Icon.Source := eisImageList;
Button.Icon.Images := MyImages;
Button.Icon.ImageIndex := 3;

Button.Icon.Glyph.LoadFromFile('my-glyph.png');
Button.Icon.Source := eisGlyph;
```

Bronkeuze is expliciet: een ongeldige ImageList-index valt niet stilzwijgend
terug op een ander icoon. Items kunnen de ImageList van hun container gebruiken
als `Icon.Images` leeg is. Images worden niet owned; free-notifications houden
referenties veilig. Een eigen glyph wordt als `TPicture` opgeslagen en gekopieerd.
Iconen behouden aspectratio, schalen met DPI en ondersteunen disabled-weergave.
De kleinere `Glyph`-enum is een fallback voor UI-decoraties, niet de catalogus.

`TERDMenuItem` biedt dezelfde Icon-eigenschap binnen een gewone TMainMenu-tree
of een TERDPopupMenu. Standaard TMenuItems blijven via Images/Bitmap/OnGetItemStyle
bruikbaar. Gebruik TERDMenuItems voor de uitgebreide ingebouwde catalogus.

## Statuspanel met badges en knoppen

Een `TERDStatusPanel` biedt tekst, status, progress, link, image, custom drawing
of een gehoste control. Voor meerdere badges/knoppen gebruik je een `TERDPanel`
als container en koppel je die via `StatusPanel.Control`:

```pascal
Host := TERDPanel.Create(Form);
Host.Height := 32;
Host.BorderWidth := 0;
Badge1.Parent := Host;
Badge2.Parent := Host;
ActionButton.Parent := Host;
StatusPanel.Width := 300;
StatusPanel.Control := Host; // selects spkControl
```

De statusbar positioneert de host in het panel. De childcontrols houden hun
normale events. Bij loskoppelen worden parent, bounds, align en visibility
hersteld; ownership blijft bij de oorspronkelijke componentowner. Eén control
mag maar één panel vullen; ancestor-hosting is verboden. `AutoHeight` is standaard
True en houdt rekening met hosts; zet het uit voor een handmatige statusbarhoogte.
`OnDrawPanel` tekent binnen de geclipte bounds van `spkCustom`.

De demo toont daadwerkelijk één statuspanel met **twee badges en een knop**.

## Dialogen

Algemene dialoog, TaskDialog, Open/Save/Folder, OpenPicture/SavePicture,
Find/Replace, Color/Font en Print/PageSetup zijn meegenomen, inclusief de
benodigde ondersteunende controls. Voorbeelden en fallbackteksten zijn algemeen;
geen ECU-write, diagnose- of andere OBD-workflowdialogen.

## Validatie

```sh
python -m pip install Pillow
python tools/validate.py
python tools/pascalcheck/run.py -v
python -m unittest discover -s tools -p 'test_*.py'
```

`CatalogSmoke.dpr` kan met FPC in Delphi-mode de gecompileerde iconenum en de
vectorstreams controleren; dit is **geen** FPC-build van de VCL-library.
`tests/ERDComponentsSmoke.dproj` controleert in Delphi de iconbronkeuze, ImageList-
lifetime, DFM-streaming, statuspanel-hosting en alle theme/ribboncombinaties.

De bron/resource/projectvalidatie en de cataloguschecks zijn hier uitvoerbaar.
Een echte Delphi-build, IDE-installatie en VCL-runtimechecks moeten op Windows
worden uitgevoerd; die zijn voor deze nieuwe library nog niet bevestigd.

## Iconen verfijnen

De PNG-mockups in `docs` zijn de goedgekeurde ontwerprichting, geen productie-
assets. `docs/vector-preview.html` toont de **daadwerkelijk geïmplementeerde**
vectoren op licht/donker, uit dezelfde data als de renderer. SVGs staan in
`assets/svg`; visuele verfijningen kunnen daar via de generatorbron worden gedaan.
De huidige 320 catalogusconcepten bevatten 297 verschillende vectorgeometrieën;
semantische synoniemen delen bewust een symbool.

Generatoren: `tools/build_icons.py` (Pascal), `tools/export_icons.py` (SVGs, preview
met dezelfde geometrie en de afzonderlijke componentpalet-bitmaps). Beide werken
zonder de oorspronkelijke OBD-repository. Bronlicentie: [LICENSE](LICENSE).
