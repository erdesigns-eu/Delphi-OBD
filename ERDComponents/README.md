# ERD Components

Zelfstandige algemene **Delphi VCL**-componentlibrary. Dit zijn onafhankelijke
kopieën: Delphi-OBD wordt niet aangepast en heeft geen afhankelijkheid van deze
library. De hele map kan naar een eigen repository worden verplaatst.

- **66 paletcomponenten**: algemene invoer, navigatie, containers, grids, dialogen,
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
   De demo gebruikt `TERDTitleBar`; met de keuze Ribbon/Menu wissel je van commandosurface.
   De File-knop opent een echte backstage met eigen Information- en Settings-pagina’s.
   De voorbeeldpagina’s gebruiken `TERDTabs` en `TERDPanel`, zonder standaard PageControl.
   Alle 66 geregistreerde componenten staan in de DFM: de sidebar navigeert naar
   iconen, invoer, layout, data, bestanden/kleuren en dialogs/notificaties.
   Dialogknoppen openen de bijbehorende component; toast, hints en popupmenu hebben eigen voorbeelden.

`ERDComponents_RT` is apart beschikbaar voor applicaties die runtime-packages
gebruiken (Win32/Win64). Installeer RT niet daarnaast in dezelfde IDE: standalone
DT bevat dezelfde units. Bronapplicaties voegen `src/UI` aan hun zoekpad toe.
BPL-uitvoer: `$(BDSCOMMONDIR)/Bpl` voor Win32, en
`$(BDSCOMMONDIR)/Bpl/<platform>` voor de 64-bit targets. Win32 gebruikt de
standaard Delphi-locatie zodat de 32-bit IDE de package op naam kan laden.

## Themes en ribbonvorm

Standaard is **ERDesigns Light**, ook als er geen theme-controller is. Een
`TERDTheme` biedt ERDesigns Light/Dark, Windows en Office Light/Dark, met
Office Word, Excel, Access, PowerPoint, Outlook en OneNote elk in Light/Dark.
Automatic is optioneel. Focusranden volgen de Windows focus-cue-instellingen.
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

De titlebar heeft een optionele AutoSave-toggle (`AutoSaveVisible`, standaard False).
`AutoSave` bewaart de aan/uit-status en `OnAutoSaveChange` meldt wijzigingen aan
je applicatie. `AutoSaveCaption` bepaalt de tekst. De breedte volgt de gemeten tekst;
`AutoSavePadding` is de ruimte tot de toggle (standaard 8 logische pixels, DPI-geschaald).
`AutoSaveWidth = 0` kiest automatische breedte; een hogere waarde begrenst de maximale breedte.
De toggle slaat zelf geen bestanden op. Met `TabStop = True` kun je hem via Tab
focussen en met Space/Enter schakelen. De demo toont hem naast Save/Undo/Redo.

Zie [de Office-vergelijking](docs/office-ribbon-review.md) voor de resterende
algemene verschillen met de aangeleverde Office-screenshot.

## Virtual Treeview

`TERDVirtualStringTree` gebruikt de officiële JAM Software **Virtual Treeview V8.5**
VCL-engine. De engine is meegeleverd onder `ERD.Components.VT.*`: je hoeft de
originele Virtual Treeview-package niet te installeren en beide varianten kunnen
naast elkaar bestaan. Ook de engineclasses hebben eigen RTTI-namen met prefix
`TERDVT`; de palettecomponent heet `TERDVirtualStringTree`. Dit is een ERD-themevariant van de bestaande engine;
virtualisatie, node-data, selectie, sorting, drag/drop en bestaande events blijven
onderdeel van die engine.

De component bindt aan `TERDTheme` en stijlt header, rijachtergrond, selectie,
checkboxes/radio, uitklappijlen, rand en native scrollbar-presentatie. De originele
Windows scrollbar-geometrie en bediening blijven behouden. Eigen draw-events en
custom checkbox-images blijven bruikbaar. De `RowHeight`, `HeaderHeight` en
`AlternateRows` zijn instelbaar; de hoogtes schalen met DPI.

De DFM-demo bevat twintig playlistgroepen met elk duizend lazy child-items,
checkboxes, meerdere kolommen en sortering via een headerklik. De data wordt door
de gebruikelijke `OnGetText` / `OnInitNode` / `OnInitChildren` callbacks geleverd.
Gebruik in migratiecode `ERD.Components.VirtualTree` en, waar nodig, de
namespaced `ERD.Components.VT.Types`, `.BaseTree` en `.Header` units. Native
nodepointers van de twee verschillende engines moeten niet worden uitgewisseld.

Herkomst, pin, hashes, licentie en de beperkte engine-aanpassingen staan in
[vendor/VirtualTreeView](vendor/VirtualTreeView/README.md). Die bronbestanden
vallen onder MPL 1.1 en behouden de oorspronkelijke credits; ze vallen niet
onder de licentie van onze eigen units. De core gebruikt alleen de VCL-runtime,
zonder FMX- of upstream-designtime-units.

## Algemene controls en media

- `TERDSearchEdit`: `TERDEdit` met zoeken/wissen, Enter en `SearchDelay`
  (standaard 300 ms; 0 = direct). `OnChange` volgt elke tekstwijziging;
  `OnSearch` levert de uiteindelijke query. `ExecuteSearch` zoekt onmiddellijk.
- `TERDSplitButton`: hoofdactie via `OnClick`, aparte menuactie via
  `DropDownMenu` / `OnDropDown`. F4 of Alt+Down opent het menu;
  Enter/Space voert de hoofdactie uit. Gebruikt dezelfde iconbronnen als Button.
- `TERDTagEdit`: `Tags`-collectie, Enter om tekst toe te voegen, Backspace op
  een leeg invoerveld om de laatste tag te verwijderen. Tags hebben een
  verwijderknop; `ReadOnly` en `AllowDuplicates` zijn instelbaar.
- `TERDExpander`: bewerkbare container met inklapbare header. `ExpandedHeight`
  bewaart de uitgeklapte hoogte in logische pixels. Reeds verborgen controls
  blijven verborgen wanneer je opnieuw uitklapt; uitklappen herstelt de inhoud.
- `TERDTrackBar.Orientation`: horizontaal of verticaal; verticaal stijgt de
  waarde naar boven. Muis, toetsenbord en muiswiel blijven beschikbaar.
- `TERDBanner`: `ShowClose`, `ActionCaption`, `OnClose` en `OnAction`.
  Sluiten zet `Visible=False`; met `Visible=True` kan de applicatie hem terug
  tonen. Eigen child-controls blijven mogelijk.
- `TERDBreadcrumb.Items`: caption/path-paren voor algemene navigatie. De `Path`
  van een item is een applicatie-ID, geen verplicht bestandspad. `OnNavigate`
  ontvangt dat ID; de applicatie beheert de navigatie. Zonder Items blijft de
  bestaande bestandspadmodus actief. Pijlen/Home/End en Enter/Space bedienen de items.
- `TERDSeekBar`: TrackBar met `BufferedPosition`; `OnSeek` volgt een afgeronde
  muisactie of een wijziging via toetsenbord/muiswiel. `OnChange` blijft
  beschikbaar tijdens slepen. De applicatie levert duur, positie en buffering.
- `TERDRangeSlider`: twee handles binnen `Min..Max`. `Position` / `LowerValue`
  is de ondergrens, `UpperValue` de bovengrens. Handles kunnen elkaar niet
  passeren. `ActiveThumb` bepaalt welke handle toetsenbord/muiswiel bedient;
  Space wisselt handle. Beide oriëntaties worden ondersteund.

De DFM-demo toont alle aanvullingen op de invoerpagina. Windows-smoketests
controleren onder andere zoeken, tagopslag, splitbediening, inklappen en slidergrenzen.
Deze VCL-runtimechecks moeten in Delphi op Windows worden uitgevoerd; de
cloudcontrole valideert broncode, DFM, packages en resources.
