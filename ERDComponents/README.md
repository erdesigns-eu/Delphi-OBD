# ERD Components

Zelfstandige algemene **Delphi VCL**-componentlibrary. Dit zijn onafhankelijke
kopieën: Delphi-OBD wordt niet aangepast en heeft geen afhankelijkheid van deze
library. De hele map kan naar een eigen repository worden verplaatst.

- **87 paletcomponenten**: algemene invoer, navigatie, containers, grids, dialogen,
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
   De 87 geregistreerde componenten staan in de DFM of runtime-voorbeeldpagina's: de sidebar navigeert naar
   iconen, invoer, layout, data, bestanden/kleuren en dialogs/notificaties.
   Dialogknoppen openen de bijbehorende component; toast, hints en popupmenu hebben eigen voorbeelden.

`ERDComponents_RT` is apart beschikbaar voor applicaties die runtime-packages
gebruiken (Win32/Win64). Installeer RT niet daarnaast in dezelfde IDE: standalone
DT bevat dezelfde units. Bronapplicaties voegen `src/UI` aan hun zoekpad toe.
BPL-uitvoer: `$(BDSCOMMONDIR)/Bpl` voor Win32, en
`$(BDSCOMMONDIR)/Bpl/<platform>` voor de 64-bit targets. Win32 gebruikt de
standaard Delphi-locatie zodat de 32-bit IDE de package op naam kan laden.

## PCB-designer

De demo bevat een zelfstandige PCB-designer met terminals, componenten,
notities, verbindingen, selectie en een losse toolbox/inspector. Kies een
toolbox-item en klik op het canvas, of sleep het naar de gewenste plek.

- [Componenthandleiding](docs/pcb-designer-component.md)
- [API en integratie met een eigen host](docs/pcb-designer-api.md)
- [Windows-testlijst](docs/pcb-designer-windows-checks.md)

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
checkboxes/radio, uitklappijlen, rand en eigen `TERDScrollBar`-controls. Native Windows-scrollbars worden
uitgeschakeld; de eigen controls en het muiswiel gebruiken de scrolloffsets van
de engine. Het muiswiel volgt de Windows-instelling voor regels per notch. Eigen draw-events en
custom checkbox-images blijven bruikbaar. De `RowHeight`, `HeaderHeight` en
`AlternateRows` zijn instelbaar; de hoogtes schalen met DPI.

De DFM-demo bevat twintig playlistgroepen met elk duizend lazy child-items,
checkboxes, meerdere kolommen en sortering via een headerklik. De data wordt door
de gebruikelijke `OnGetText` / `OnInitNode` / `OnInitChildren` callbacks geleverd.
Gebruik in migratiecode `ERD.Components.VirtualTree` en, waar nodig, de
namespaced `ERD.Components.VT.Types`, `.BaseTree` en `.Header` units. Native
nodepointers van de twee verschillende engines moeten niet worden uitgewisseld.

Celbewerking gebruikt standaard `TERDEdit` zodra `toEditable` aanstaat. Met
`OnGetCellEditor` kies je per node/kolom `tekText`, `tekInteger`, `tekDecimal`,
`tekCombo`, `tekDate`, `tekTime`, `tekDateTime` of `tekNone`. Vul `Choices` voor de
zoekbare combo. `OnValidateCellEdit` kan tekst normaliseren of `Accept := False`
zetten; `OnNewText` bewaart geaccepteerde wijzigingen in je eigen model. Enter
accepteert, Escape annuleert. Eigen `OnCreateEditor`-links blijven voorrang krijgen.
De demo toont tekst- en combo-editors; de Windows-smoketest controleert opslaan,
annuleren en kolomgebonden keuzes.

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

## Selectie en navigatie

Edit, SpinEdit, SearchEdit en Memo behouden native Windows-tekstbewerking, maar
tekenen de geselecteerde tekst met de ERD-accentkleur en contrasterende tekst.
De selectie wordt samen met de native inhoud gebufferd weergegeven. Memo gebruikt
native regel- en karakterposities voor multiline-selectie, wrapping en scrollen.

Breadcrumb-chevrons openen een ERD-popup. In bestandspadmodus bevat die de
submappen; bij algemene Items vult `OnDropDown(Sender, Path, Choices)` de keuzes
met caption/path-paren. Selectie levert het pad of applicatie-ID via `OnNavigate`.
Alt+Down/F4 opent het menu van het gefocuste segment. De demo toont playlist-
navigatie met landen, Live TV/VOD/Series en kanaalcategorieën.

TagEdit gebruikt verwijderbare chips met een kruisje rechts. Klikken op de caption
verwijdert niets; het kruisje of Delete/Backspace op een gefocuste chip verwijdert
de tag. Expander gebruikt een themed, links uitgelijnde header in plaats van een
gewone knop. SplitButton heeft een verticale scheidingslijn tussen beide acties.

## Ribbon-invoer en galleries

Koppel `TERDRibbonGroup.Control` aan een bestaande ERD-input, een panel met
meerdere inputs of `TERDTileGrid` als gallery. `ControlWidth` is de breedte in
96-DPI pixels (standaard 180). De control staat naast de gewone commands; de
Ribbon verwerkt tabwissels, classic/simplified, collapsen en het tijdelijk
uitgeklapte ribbonpaneel. Controls die niet passen worden verborgen, zodat ze
niet over andere commands vallen. In classic layout kunnen galleries hun volle
hoogte gebruiken; simplified gebruikt één compacte rij.

De control blijft eigendom van de applicatie. Verwijderen van de koppeling of
groep herstelt parent, bounds, align, anchors en oorspronkelijke zichtbaarheid.
Eenzelfde control kan niet aan meerdere groepen worden gekoppeld. Een groepskopie
kopieert metadata en commands, maar neemt de controlbinding niet over. Controls
zonder expliciete theme volgen de Ribbon-theme tijdens de koppeling.

De DFM-demo bevat een groep met SearchEdit, een zoekbare ComboBox en SpinEdit,
plus een gallery met Normal/Heading/Subtitle. De gallery synchroniseert de
stijlkeuze; wijzigingen worden in de statusbar getoond.

## Ribbon-states en IDE-keuzelijsten

Ribbon-items met `Kind=rikToggle` werken als losse checkbox-toggles. Met dezelfde
positieve `GroupIndex` binnen één ribbon-groep werken ze als radioknoppen.
`AllowAllUp` bepaalt of de gekozen radioknop opnieuw uit kan. Hover, pressed,
checked en disabled hebben afzonderlijke weergaven.

De designtime-package registreert expliciete keuzelijsten voor enum-properties
en componentverwijzingen, ook op collection-items en geneste properties.
`Icon.BuiltIn` heeft daarnaast een zoekbare dialoog met iconvoorbeelden.
Backstage `Page` biedt compatibele controls uit het formulier aan; themes,
imagelists, actions en popupmenu's gebruiken dezelfde typecontrole.

De titlebar biedt hover en pressed voor alle systeemknoppen. De maximize-knop
gebruikt de non-clientroute van het formulier en DWM voor Windows 11 Snap.
Snap en de visuele VCL-weergave moeten op Windows worden gecontroleerd;
de Python-controles vervangen geen Delphi-build of IDE-installatietest.

## CheckListBox

`TERDCheckListBox` gebruikt dezelfde lijstbasis, themed scrollbar, selectie en
Windows-focuscues als `TERDListBox`. `Items` is een DFM-bewerkbare collectie:
elk item heeft `Caption`, `State` (`cbUnchecked`, `cbChecked`, `cbGrayed`) en
`Enabled`. Check-state blijft bij het item wanneer je items verplaatst of verwijdert.

- `Checked[Index]`, `State[Index]` en `CheckedCount` lezen/wijzigen de checks.
- Een klik op de caption selecteert; klikken op de checkbox of Space togglet.
  `CheckOnClick=True` laat ook de hele rij togglen.
- `AllowGrayed=True` maakt de gebruikerscyclus unchecked → checked → grayed.
- `OnClickCheck(Sender, Index)` meldt een gebruikerscheck, los van `OnChange`
  voor rijselectie. Programmatic state-wijzigingen roepen dit event niet aan.
- `CheckAll(State, IncludeDisabled=False)` past meerdere items in één batch aan.
  Disabled items worden standaard overgeslagen; de applicatie kan ze expliciet wijzigen.

De Inputs-pagina van de DFM-demo bevat checked, unchecked, grayed en disabled
voorbeelden. De Windows-smoketest controleert muis/Space, annuleren van een klik,
disabled items, events, batchwijzigingen, verplaatsen, streaming, thema's en scrollen.
Die VCL-tests vereisen Delphi/Windows en zijn niet uitgevoerd in de cloudomgeving.

Een losse toggle-switch is niet nodig: `TERDCheckBox.Style=csSwitch` biedt die al.

## TimePicker

`TERDTimePicker` biedt bewerkbare tijdsegmenten in plaats van een vrije tekstmasker.
`Time` is de tijdfractie van een Delphi `TDateTime`; een meegegeven datumdeel wordt
verwijderd. Milliseconden blijven behouden, ook als ze niet worden getoond.

- `TimeFormat`: `tf24Hour` (standaard) of `tf12Hour`, met een apart AM/PM-segment.
- `ShowSeconds` toont het optionele secondenveld. De locale bepaalt het
  scheidingsteken en de AM/PM-labels.
- Klik op een segment of gebruik Left/Right. Typ cijfers; na twee cijfers of
  een ondubbelzinnig eerste cijfer gaat de selectie door naar het volgende veld.
  Ongeldige cijfers worden geweigerd. Een invoerpauze van 1,5 s begint een nieuwe invoer.
- Up/Down en de optionele spin-knoppen wijzigen het geselecteerde veld.
  Elk veld loopt rond zonder carry naar andere velden. Home/End kiest de veldgrens.
  Het muiswiel wijzigt alleen een gefocuste, bewerkbare picker.
- `ReadOnly` laat segmentselectie toe maar blokkeert gebruikerswijzigingen.
  De applicatie kan `Time` nog steeds wijzigen. `OnChange` meldt een gewijzigde tijd.

De Inputs-pagina toont zowel 24h met seconden als 12h. Windows-smoketests omvatten
invoer, wrap, AM/PM, locale, readonly, disabled, muis, spin, DFM en renderen zonder
window-handle. Deze VCL-tests moeten nog in Delphi op Windows worden uitgevoerd.

## DateTimePicker en datumsegmenten

`TERDDatePicker` heeft nu bewerkbare dag-, maand- en jaarvelden. Hun volgorde
volgt `DisplayFormat` of het locale korte datumformaat; maandnamen blijven
met cijfers te wijzigen. Weekdagtekst en quoted literals zijn geen invoervelden.
Left/Right wisselt velden, cijfers vullen het actieve veld, Up/Down wijzigt het
veld met wrap en Home/End kiest de veldgrens. Een ongeldige dag wordt bij een
maand-/jaarwijziging geclamped naar de laatste dag. De kalenderknop, F4 of
Alt+Down opent de bestaande kalender. `ReadOnly` blokkeert gebruikerswijzigingen;
`ShowCalendarButton` kan de knop verbergen.

`TERDDateTimePicker` combineert die date-editor met `TERDTimePicker` in één
component. `DateTime` bewaart de volledige datum en tijd; datumwijzigingen behouden
de tijd en omgekeerd. Een programmatische wijziging van beide velden levert één
`OnChange` op. `DateFormat`, `TimeFormat`, `ShowSeconds`, `ReadOnly` en `FirstDay`
zijn in de IDE instelbaar. De velden zijn via `DateEditor` en `TimeEditor` bereikbaar
voor focus/segmentselectie. Tab wisselt tussen de velden; de kalender-events lopen
via de samengestelde component. De interne editors worden niet apart gestreamd.

De Inputs-demo toont de DateTimePicker. Windows-smoketests voor schrikkeljaren,
invoer, datumvolgorde, readonly, events, tijdbehoud en DFM zijn toegevoegd, maar
vereisen nog uitvoering in Delphi op Windows.

## LinkLabel

`TERDLinkLabel` biedt themed linktekst met hover/pressed/disabled/visited states,
Windows-focuscues, Enter/Space en een optioneel `Icon` (built-in, imagelist of glyph).
`AutoOpen=False` is standaard: de applicatie handelt de klik af via `OnClick`.
Met `AutoOpen=True` of `OpenLink` opent Windows de `URL` met de bijbehorende app.
`Visited` en `AlwaysUnderline` zijn instelbaar. De demo meldt klikken in de statusbar.

## Tag appearance

`TERDTagEdit` uses stronger theme-accent fills and borders. `TagColor=clDefault`
follows the theme; `TagColorStrength` sets the fill strength in percent (default
28). `CloseIconSize` sets the cross size in logical pixels (default 8), and
`CloseSpacing` sets the exact text-to-icon gap (default 4). Changes update existing
tokens and their layout. The close button has independent hover/pressed states;
releasing away cancels deletion. These settings are available in the IDE and DFM.

## LoadingIndicator

`TERDLoadingIndicator` is een themed spinner voor onbekende voortgang, met optionele
`Caption` en `Active` (standaard True). De timer draait alleen wanneer de control
zichtbaar en enabled is, en nooit in de designer. `RespectAnimations=True` volgt
Windows' instelling voor client-area-animaties; bij uitgeschakelde animaties blijft
een statische indicator zichtbaar. `IsAnimating` laat zien of de timer actief is.
`Style` kiest `lisSpinner`, `lisRing`, `lisDots`, `lisBars` of `lisPulse`.
`IndicatorSize` is de indicatorbox in 96-DPI pixels (8–256, standaard 32);
`AnimationInterval` bepaalt de framesnelheid (16–2000 ms, standaard 80).
AutoSize houdt ook bij kleine indicators voldoende hoogte voor de caption.
De Inputs-demo bevat style- en size-keuzes naast de switch voor `Active`.

Windows-smoketests voor LinkLabel en LoadingIndicator zijn toegevoegd: activeren,
disabled state, renderen zonder window, DFM en het stoppen/hervatten van de timer
bij zichtbaarheidswijzigingen. Ook deze tests zijn hier niet op Windows uitgevoerd.

## DropZone

`TERDDropZone` accepteert bestaande bestanden vanuit Explorer via OLE `CF_HDROP`,
met themed drag-feedback en een copy-cursor. Hij verplaatst of opent bestanden niet.
`FileMasks` bevat wildcard-masks gescheiden met `;` (leeg of `*` = alle bestanden).
Niet-passende bestanden worden uitgefilterd. `AllowDirectories=False` en
`AllowMultiple=True` zijn standaard; bij single-select wordt een drop met meerdere
passende items geweigerd. `AcceptDrops=False` of disabled blokkeert de drop.
`OnFilesDropped` ontvangt tijdelijk de gefilterde `TStrings`; kopieer deze als je ze
wilt bewaren. `DropFiles` gebruikt hetzelfde pad voor bestanden uit een dialoog.
Klikken/Space levert `OnClick` voor een eigen browse-actie; de demo gebruikt onze OpenDialog.
OLE wordt op de UI-thread geregistreerd en bij handle-recreatie/opruimen vrijgegeven.

## CommandPalette

`TERDCommandPalette` opent via `Execute` een compacte command-popup bovenaan het
actieve venster, met een SearchEdit en scrollende resultaten. Er is geen titelbalk
of buttonfooter. `PaletteWidth` (620) en `MaxVisibleResults` (8) bepalen de grootte;
de hoogte volgt het aantal resultaten. Het hoofdvenster blijft ingeschakeld en
een klik buiten de popup sluit hem. `Commands` is een DFM-collectie met `Key`, `Caption`,
`Category`, `Keywords`, `ShortcutText`, `Icon`, `Enabled`, `Visible` en optionele
`Action`. Een lege caption/shortcut valt terug op de Action. Niet-beschikbare acties
worden weggelaten. Zoeken is case-insensitive en vereist dat alle zoekwoorden ergens
in caption, categorie, keywords of key voorkomen; volgorde volgt de collectie.

Up/Down en PageUp/PageDown selecteren vanuit het zoekveld; Enter of een enkele
klik voert uit, Escape annuleert. De popup sluit vóór uitvoering van de Action of `OnExecute`, zodat een
commando veilig een volgende dialog kan openen. Action-backed commando's voeren
hun Action uit; alleen commando's zonder Action gebruiken `OnExecute`.
`Search` levert stabiele collection-IDs; `ExecuteCommand(ID)` valideert opnieuw.
`LastCommand` bevat de gekozen Key. De applicatie kiest zelf een shortcut om de
palette te openen; de DFM-demo heeft Ctrl+K, een knop en drie werkende voorbeeldcommando's.

## Rating

`TERDRating` toont standaard vijf gevulde sterren, met `Value`, `MaxStars`,
`AllowHalf`, `AllowClear` en `ReadOnly`. Hover geeft een preview; een geldige
muis-release kiest de waarde, release buiten de control annuleert. Op dezelfde
waarde klikken wist de rating als `AllowClear=True`. Left/Right/Up/Down wijzigen
de waarde, Home/End kiest de grens en Delete/Backspace wist indien toegestaan.
`OnChange` meldt een gewijzigde waarde. Waarden worden geclamped en afgerond naar
hele of halve stappen. `ReadOnly` blokkeert gebruikersinvoer, niet API-wijzigingen.

Zonder `Icon` worden echte volle/halve sterren getekend. Een built-in icon,
imagelist of eigen glyph kan de symbolen vervangen. De Inputs-demo toont zowel
bewerkbare halve sterren als readonly ratings. De component gebruikt Windows-focuscues.

Windows-smoketests zijn toegevoegd voor DropZone-filtering (incl. Unicode-paden),
CommandPalette-search/actions en Rating-invoer/streaming. Deze tests vereisen nog
uitvoering in Delphi op Windows; OLE-drag-feedback en de zoekdialoog zijn niet
visueel getest in de cloudomgeving.

## Bewerkbare ComboBox

`TERDComboBox.Style=ecsDropDownList` blijft de standaard. Met `ecsDropDown` krijgt
hij native tekstinvoer (incl. selectie/clipboard) met onze themed selectie.
`FilterMode` kiest geen filtering, prefix of contains; filtering is case-insensitive.
`AutoComplete=True` vult een prefix aan en selecteert de suffix zodat je verder kunt
typen. Backspace/Delete vullen niet meteen opnieuw aan. `ReadOnly` blokkeert typen,
maar laat kiezen uit de dropdown toe. `Text` kan in editable stijl ook vrije tekst zijn.

De popup behoudt een mapping naar de oorspronkelijke `Items`: `ItemIndex` verandert
niet van betekenis bij filtering. `FilterText`, `FilteredCount` en `FilteredIndex`
bieden dezelfde filtering aan de applicatie. De native editor is bereikbaar via
`Editor`; de DFM bewaart de openbare properties en vrije tekst, niet die interne control.
`OnChange` meldt gebruikersinvoer of een keuze; programmatische setters zijn stil.
De Inputs-demo bevat een zoekcombo en Windows-smoketests voor filtering, indexmapping,
autocomplete, Backspace en DFM zijn toegevoegd (nog niet op Windows uitgevoerd).

## Wachtwoord en validatie in Edit

`TERDEdit.PasswordChar` gebruikt native password-masking. `ShowPasswordButton`
toont de eye-knop; klikken of Alt+V wisselt `PasswordVisible`. Tonen/verbergen
behoudt tekst en selectie en roept `OnChange` niet aan. De zichtbaarheidsstatus
wordt niet in de DFM opgeslagen; het masker en de knopinstelling wel.

`ValidationState` kiest normaal/success/warning/error en `ValidationText` toont een
wrapped melding onder het invoerveld. `AutoSize` vergroot de hoogte; bij
`AutoSize=False` reserveert de applicatie zelf genoeg hoogte voor de melding.
`OnValidate(Sender, Text, State, MessageText)` bepaalt status en tekst. `Validate`
roept dit event expliciet aan en geeft False bij error; `ValidateOnChange` is
standaard False en `ValidateOnExit` True. Reentrant Validate-aanroepen zijn beschermd.

SearchEdit en SpinEdit erven deze mogelijkheden; hun knoppen blijven naast het
invoerveld, boven de melding. De Inputs-demo toont een password-edit en live
validatie. Windows-smoketests voor selectie, reveal, validatie en DFM zijn toegevoegd,
maar hier niet in Delphi op Windows uitgevoerd.

## Documenttabs: sluiten, herschikken en losmaken

`TERDTabs` bewaart selectie op collection-ID: verplaatsen of het sluiten van een
eerdere tab behoudt het geselecteerde document. `Tabs[Index].Page` koppelt optioneel
een `TWinControl` (bijvoorbeeld een panel) aan de tab. De tabstrip beheert de zichtbaarheid;
de applicatie blijft eigenaar van de Page. Iedere tab heeft een eigen Page en een
Page mag de tabstrip zelf niet bevatten. Zonder Page blijft de bestaande OnChange-aanpak werken.

- In document-stijl sluit het kruisje pas bij mouse-release op dezelfde knop.
  Ctrl+W of `CloseTab(Index)` gebruikt hetzelfde `OnClose`-veto. `OnClosed` volgt
  na verwijderen en levert collection-ID plus Page voor eigen cleanup.
- `AllowReorder=True` laat slepen met invoegmarkering en overflow-autoscroll toe.
  Release voert de verplaatsing uit; Escape/capture-verlies annuleert.
  Ctrl+Shift+Left/Right werkt ook. `MoveTab` is tevens programmatisch beschikbaar.
  `OnReordering` kan weigeren; `OnReordered` meldt de nieuwe positie.
- `AllowDetach=True` laat een tab met Page buiten de strip slepen, of losmaken met
  Ctrl+Shift+D / `DetachTab(Index, ScreenPoint)`. Dezelfde Page verhuist naar een
  themed venster met onze TitleBar. De tab blijft als item bewaard maar is tijdelijk
  verborgen; `Detached` en `DetachedWindow` laten de runtime-status zien.
- Sluiten van het losse venster of Return to tabs brengt de Page terug naar haar
  oorspronkelijke parent, align, anchors en bounds. `AttachTab` doet hetzelfde.
  `OnDetaching` kan weigeren; `OnDetached` en `OnAttached` melden de overgang.
  Detach-state wordt niet opgeslagen in de DFM. Bij opruimen van de tabstrip worden
  losse Pages eerst teruggeplaatst, zodat het losse venster ze niet vernietigt.

Herschikken en losmaken zijn standaard uitgeschakeld. De Layout-pagina van de
DFM-demo bevat echte documentpanels, bewerkbare tekst, een modified-tab-veto en
new-tab/close/reorder/detach/return. Page-verwijzingen zijn via de IDE-keuzelijst te kiezen.

Windows-smoketests zijn toegevoegd voor tab-identiteit, Page-selectie, close/reorder/
detach-veto's, drag-annulering, dezelfde inhoud in een los venster, terugplaatsen,
opruimen en DFM. Deze VCL-tests en visuele drag/DPI-checks zijn nog niet uitgevoerd
in de cloudomgeving; hiervoor is Delphi op Windows nodig.

## Debuggercomponenten

De pagina **Debugger components** toont een gedeeld `TERDDebugModel`, editorbreakpoints
en een uitvoeringsmarkering, een los `TERDBreakpointPanel`, een `TERDCallStack`, een
`TERDLocalsPanel` met scopes, een `TERDWatchPanel` met expressies en lazy loading
via `TERDValueTree`, een `TERDDisassemblyView` voor instructies, een
`TERDFiberList` voor scheduler-samenvattingen en een
`TERDTimeSeriesChart` met begrensde historie. De debug-ribbon en voorbeeldwaarden
zijn expliciet een synthetische demo; er wordt geen TSVM-proces gestart.

- [Component-API, sneltoetsen en Windows-controles](docs/debugger-components-api.md)
- [NDB/DAP-inventaris en verdere componenten](docs/debugger-components-plan.md)
