# Onderzoek: algemene VCL-componentlibrary uit Delphi-OBD

Bron: `main`, commit `e43fd7b` (13 oktober 2026). Onderzocht: alle 90 Pascal-units in `src/UI`, inclusief de `uses`-afhankelijkheden, de basisclasses, de relevante publieke eigenschappen en de design-time registratie. Dit is een brononderzoek; er is geen nieuwe Delphi-build uitgevoerd. De broncomponenten zijn niet verplaatst of gewijzigd.

## Advies

Maak een algemene VCL-library met de bestaande rendering/theme-basis en desktopcontrols. Laat Delphi-OBD deze library gebruiken en behoud de OBD-workflowcomponenten in Delphi-OBD. De eerste selectie bevat **44 units: 7 basisunits en 37 controlunits**. Hun directe en transitieve projectimports blijven binnen die selectie; geen daarvan importeert een OBD-service, adapter, protocol of `ERD.Types`. Dit betekent dat hun afhankelijkheden zelfstandig zijn, niet dat alle namen, voorbeeldteksten en instellingen al productneutraal zijn.

Neem **StatusBar** ook op in de eerste release, nadat de vertaalafhankelijkheid is losgekoppeld. Inspector en Histogram kunnen dezelfde algemene vertaalvoorziening gebruiken. Neem gauges en live-data dashboards pas mee nadat hun databinding is gesplitst.

## Eerste selectie: algemene controls

De unitnamen hieronder zijn relatief aan `ERD.UI.`. De huidige classes hebben nog het prefix `TOBD`; in de nieuwe library is `TERD` een logische naam. Niet alle classes zijn zelfstandige paletcomponenten: collecties, interne popupwindows en baseclasses horen als ondersteuning mee.

| Functie | Units | Belangrijkste componenten |
|---|---|---|
| Invoer | Buttons, Edits, Memo, SpinEdit, Calendar | TOBDButton, TOBDCheckBox, TOBDRadioButton, TOBDEdit, TOBDComboBox, TOBDMemo, TOBDSpinEdit, TOBDCalendar, TOBDDatePicker |
| Applicatievenster en navigatie | Menus, Sidebar, Ribbon, Backstage, TitleBar, ToolBar, Tabs | TOBDMenuBar, TOBDPopupMenu, TOBDSidebar, TOBDRibbon, TOBDBackstage, TOBDTitleBar, TOBDToolBar, TOBDTabs |
| Layout en containers | Card, ScrollBox, Splitter, TileGrid | TOBDCard, TOBDScrollBox, TOBDSplitter, TOBDTileGrid |
| Kleine controls en feedback | Chips, Progress, RangeBar, Segmented, ScrollBar, TrackBar, Hint, Toast | TOBDChip, TOBDBadge, TOBDBanner, TOBDProgressBar, TOBDRangeBar, TOBDSegmented, TOBDScrollBar, TOBDTrackBar, TOBDHintStyle, TOBDToastManager |
| Lijsten en tabellen | ItemList, PopupList, DataGrid, KeyValueList | TOBDListBox, TOBDDataGrid, TOBDKeyValueList; PopupList is ook infrastructuur voor andere controls |
| Dialogen | Dialogs, TaskDialog, FileDialogs, FindDialogs, ColorDialog, FontDialog, PrintDialogs | TOBDDialog, TOBDTaskDialog, TOBDOpenDialog, TOBDSaveDialog, TOBDFolderDialog, TOBDFindDialog, TOBDReplaceDialog, TOBDColorDialog, TOBDFontDialog, TOBDPrintDialog, TOBDPageSetupDialog |
| Bestandsnavigatie | ShellControls | TOBDBreadcrumb, TOBDPlacesPane, TOBDFolderTree, TOBDFileList |
| Tekstconsole | Terminal | TOBDTerminal; de tekstviewer is algemeen, de huidige voorbeeldconversatie is OBD-specifiek |

### Basisunits die mee moeten

| Unit | Waarom nodig |
|---|---|
| Types | Palet, styles, density, enums en gedeelde types |
| Theme | Theme-controller, automatische owner-resolutie en VCL-style fallback |
| Control | Windowed/graphic baseclasses, DPI, theme, surface- en tilehostinterfaces |
| Paint | Gemeenschappelijke painter en glyphs |
| GDIP | GDI+-renderinghelpers |
| IconData | Ingebouwde iconendata die Paint gebruikt |
| Anim | Algemene animatieondersteuning; geen OBD-imports |

Veel controls gebruiken `Control → Theme/Types/Paint → GDIP/IconData`. Alleen de losse controlunit kopiëren is daarom onvoldoende. `IOBDSurface` en `IOBDTileHost` zijn ondanks hun naam algemene UI-contracten: achtergrondkleur en interactie bij het verplaatsen/resizen van tiles. Die kunnen als `IERDSurface` en `IERDTileHost` behouden blijven.

### Productneutrale opschoning van de eerste selectie

- **TitleBar**: fallbackcaption is nu `OBD Studio`. Gebruik de form-/applicatietitel of een algemene fallback.
- **Backstage**: navigatie en paginacompositie zijn algemeen. `TOBDReportPreview` heeft echter een voertuigrapport als ingebouwde voorbeeldinhoud, inclusief Volkswagen/VIN/diagnoseonderdelen. Behoud het algemene A4/zoom/paginamechanisme en `OnPaintPage`; verplaats het voertuigrapport naar Delphi-OBD of vervang het door neutrale voorbeeldinhoud. De hele unit is dus niet zonder opschoning productneutraal.
- **Terminal**: vervang de ELM327-voorbeeldconversatie; gebruik een algemeen tekst-/logvoorbeeld.
- **Types/Theme**: scheid algemene UI-instellingen van dashboard/unit-systemconcepten waar nodig. Geen protocolunit meeverhuizen om alleen een enum te krijgen.
- **Paint/IconData**: algemene en voertuiggerichte glyphs zitten samen. Behoud voor de algemene library de algemene iconen en maak specifieke iconen via image lists of uitbreidingen beschikbaar. Wijzig enumvolgorde of indexen niet zonder migratie van bestaande DFM/layoutdata.
- **Namen en documentatie**: `TOBD`, `IOBD` en OBD Studio-commentaar neutraliseren. Units zijn al `ERD.UI.*`; bepaal eigenaarschap van die namespace voordat dezelfde units in twee packages belanden.

## Algemeen, maar vertalingen loskoppelen

| Unit / component | Huidige koppeling | Voorstel |
|---|---|---|
| StatusBar / TOBDStatusBar | ERD.I18n; preview met ELM327/COM4/CAN | Algemene vertaalservice en neutrale preview. De paneltypes tekst/status/progress/link zijn al algemeen. |
| Inspector / TOBDInspector | ERD.I18n voor onder andere On/Off | Vertaalinterface delen met de algemene library. Property-gridmodel is niet OBD-specifiek. |
| Histogram / TOBDHistogram | ERD.I18n | Algemeen histogram met samples, statistieken en bands; vertalingen loskoppelen. |

`ERD.I18n` staat in Core en importeert `ERD.Types`; bij configuratiefouten wordt `EOBDConfig` gebruikt. Dus de volledige I18n-unit simpelweg kopiëren trekt alsnog OBD-core mee. Extraheer de algemene translator met een algemene configuratie-exception en aparte algemene catalogi, of bied een kleine vertaalprovider/callback. Behoud de taalwijzigingsnotificaties; alleen `Tr` vervangen door vaste Engelse tekst verliest functionaliteit.

## Optionele tweede selectie

| Units | Beoordeling |
|---|---|
| DiffView, HexView, XYChart | Algemeen inzetbare vergelijking, hexviewer en grafiek; apart nalopen op OBD-voorbeelddata en labels. Goede kandidaten als deze app-functies gewenst zijn. |
| RangeProfiles, RangeEditor | Ranges/editor zijn algemeen inzetbaar. De huidige profielbetekenis en voorbeelddata zijn gericht op voertuigmetingen; neutraliseren vóór opname. |
| BitEditor, MapGrid | Technische controls die buiten OBD bruikbaar zijn; voor een uitbreidingspakket, niet noodzakelijk voor de eerste desktopset. |
| Timeline | Datastructuur is bruikbaar, maar kinds/filters zijn nu writes/backups/security/flash/vehicle scan. Eerst een algemeen eventmodel of door de host aangeleverde categorieën maken. |
| GMeter, LapTimer | Geen OBD-imports, maar meet-/motorsportcontrols. Alleen opnemen als die scope voor de algemene library gewenst is. |

Geen van deze tien units heeft een directe OBD-core/service-import. Dat maakt ze technisch relatief makkelijk te verplaatsen; het domeinmodel bepaalt of ze binnen jouw algemene library passen.

## Eerst databinding scheiden

Betrokken units: **Binding, Gauges.Base, Gauges.Bar, Gauges.Dial, Gauges.Types, Units, MatrixDisplay, StatusLamp, TrendChart, ValueTile, Dashboard en LogViewer**.

`TOBDChannelBinding` is gekoppeld aan `TOBDLiveData`, met een channel-id die nu als Mode 01 PID gebruikt wordt. Ook controls waarbij je een Value kunt instellen importeren via de baseclass of binding nog OBD-services. De bindings ondersteunen meer dan een waarde: no-data/live/stale, timestamps, units en updates.

Voor gauges/charts: algemene waarde-/channelbinding in de nieuwe library, OBD-bindingadapter in Delphi-OBD. Behoud `PushValue`, stale-detectie en waarde-/unit-/alertgedrag. `Units` kan een algemene conversiehelper worden; de huidige metric-engineering/imperial-conventie moet daarbij expliciet blijven. `Dashboard` heeft bovendien factories voor live-data tiles en `ERD.JSON`; eerst splitsen in algemene tile-layout/persistentie en OBD-tiles.

`LogViewer` hangt aan `ERD.Logger.Sinks` via `IOBDLogSink`. Verplaats de algemene tekstviewer (Terminal) eerder, en behoud of vervang deze loggeradapter als apart integratiestuk. `Gauges.Types` op zichzelf is ondersteunende typescode; het staat in deze groep omdat het geen eerste-release desktopcontrol is.

## In Delphi-OBD houden

**ActuatorPanel, BusSend, BusTrace, CellMatrix, ConnectionBar, DtcList, DtcPanel, EcuTopology, FreezeFrameView, KeySlots, LiveDataGrid, Pickers, PreconditionList, ProcedureView, ReadinessPanel, ReplayBar, SecurityAccessCard, SignalBrowser, SignalGrid, VehicleCard en WriteConfirm.**

Dit zijn de 21 workflow-/diagnose-/voertuiggerichte units. Sommige importeren alleen UI-units maar hebben alsnog een OBD-model: CellMatrix heeft cellspanningen/moduletemperaturen, EcuTopology heeft ECU-/busstructuren, en WriteConfirm bewaakt ECU-write/backup-bevestiging. Dat is geen reden om ze onderdeel van het algemene desktoppackage te maken. Hergebruik daar liever de nieuwe algemene bases, grids, cards, dialogs en buttons.

## Praktische extractievolgorde

1. Maak de nieuwe VCL-runtime/design-time packages met de 7 basisunits. Geef de library een duidelijke eigen namespace/eigenaarschap; voorkom dubbele unit- en componentclasses in gelijktijdig geladen packages.
2. Voeg de algemene invoer, containers, menus, sidebar, ribbon, titlebar, toolbar en dialogen toe. Neem StatusBar mee nadat I18n is gesplitst. Neutraliseer ingebouwde previewinhoud.
3. Gebruik `TERDButton`, `TERDCheckBox`, `TERDRadioButton`, `TERDMenuBar`, `TERDRibbon`, `TERDTitleBar`, enz. als algemene classes. Plan de migratie van bestaande DFM-classnamen en properties; Pascal typealiases alleen lossen DFM-streaming niet op.
4. Neem alleen de relevante paletregistraties en icoonresources uit de huidige designpackage over. Resource-identifiers zijn nu op TOBD-classnamen gebaseerd; laat ze bij de nieuwe TERD-classnamen passen. Gebruik een unieke resourcenaam. De grote PNG-mockups zijn documentatie/voorbeelden, niet de runtime-afhankelijkheid van de componentunits.
5. Laat Delphi-OBD de algemene library gebruiken, verwijder de gekopieerde eigen versies en behoud uitsluitend zijn workflowcontrols, OBD-adapters en registraties. Houd één eigenaar van theme/painter/baseclasses.
6. Neem de passende bestaande tests mee en splits gemengde tests. Controleer in Delphi beide beoogde package-targets/IDE-architecturen, DFM roundtrips, theme/DPI/density, keyboardfocus, acties en image lists. Een los VCL-demo zonder OBD-units moet menu/ribbon/titlebar/buttons/dialogen kunnen compileren en gebruiken.

Relevante bestaande tests: `Tests.ERD.UI.Foundation`, `Chrome`, `Controls`, `Dialogs`, `DataGrid`, `TileGrid` en `RenderHelpers`; voor Inspector/Histogram en optionele controls relevante delen van ProData/ProCharts/Histogram. Verplaats niet de hele huidige registratieunit: die registreert ook de OBD-workflows.

## Volledige unitinventaris

De bijlage `ui-extraction-inventory-2026-10-13.json` bevat per unit de categorie, gevonden classes/baseclasses en directe ERD-imports. Dit is een statische uses-inventaris; voorwaardelijke imports zijn meegenomen en de semantische selectie is hierboven toegelicht.

| Categorie | Units |
|---|---|
| Basis (7) | Anim, Control, GDIP, IconData, Paint, Theme, Types |
| Algemeen (37) | Backstage, Buttons, Calendar, Card, Chips, ColorDialog, DataGrid, Dialogs, Edits, FileDialogs, FindDialogs, FontDialog, Hint, ItemList, KeyValueList, Memo, Menus, PopupList, PrintDialogs, Progress, RangeBar, Ribbon, ScrollBar, ScrollBox, Segmented, ShellControls, Sidebar, SpinEdit, Splitter, Tabs, TaskDialog, Terminal, TileGrid, TitleBar, Toast, ToolBar, TrackBar |
| Algemeen na I18n-splitsing (3) | Histogram, Inspector, StatusBar |
| Optioneel / model neutraliseren (10) | BitEditor, DiffView, GMeter, HexView, LapTimer, MapGrid, RangeEditor, RangeProfiles, Timeline, XYChart |
| Bindings/adapter splitsen (12) | Binding, Dashboard, Gauges.Bar, Gauges.Base, Gauges.Dial, Gauges.Types, LogViewer, MatrixDisplay, StatusLamp, TrendChart, Units, ValueTile |
| OBD / workflow behouden (21) | ActuatorPanel, BusSend, BusTrace, CellMatrix, ConnectionBar, DtcList, DtcPanel, EcuTopology, FreezeFrameView, KeySlots, LiveDataGrid, Pickers, PreconditionList, ProcedureView, ReadinessPanel, ReplayBar, SecurityAccessCard, SignalBrowser, SignalGrid, VehicleCard, WriteConfirm |
