# Vergelijking met de aangeleverde Office-screenshot

De screenshot toont de recente PowerPoint-, Excel- en Word-interface. Deze
vergelijking betreft algemene bediening en presentatie; documentbewerking,
cloudopslag en samenwerking blijven verantwoordelijkheden van de applicatie.

| Onderdeel | Huidige ERD Components | Wat nog ontbreekt of beter kan |
| --- | --- | --- |
| AutoSave naast het applicatie-icoon | Optionele titlebar-toggle, On/Off, eigen caption/breedte, muisbediening en Space/Enter bij TabStop=True | De applicatie koppelt het event aan haar eigen opslagfunctie |
| Applicatiekleuren | ERDesigns en Office, plus Word/Excel/Access/PowerPoint/Outlook/OneNote in Light/Dark | Visuele afstemming en contrast bevestigen in Delphi |
| Quick access: Save/Undo/Redo | Caption-buttoncollectie; demo toont alle drie | Customize-dropdown, separators, verplaatsing onder de ribbon en eigen keytips |
| Documentnaam en Saved-status | Title, Subtitle en statuschip beschikbaar | Klikbare documenttitel met locatie/naam-dropdown en een afzonderlijke save-state-presentatie |
| Zoekvak midden in de titlebar | Ribbon heeft command search en Alt+Q | Een afzonderlijk optioneel titlebar-zoekvak met eigen event en ruimteverdeling |
| Gebruikersavatar en accountmenu | Extra captionknoppen kunnen eigen glyphs/imagelistafbeeldingen en click-events gebruiken | Een aparte avatar/accountpresentatie met ronde clipping |
| Comments, Share, Record naast de tabs | Algemene ribboncommands en captionknoppen beschikbaar | Een rechter commandocollectie op de tabrij met tekst, dropdown en accentknop |
| File / backstage | File opent echte, algemene backstagepagina's in de demo | Meer voorbeelden van applicatiepagina's; geen vaste reportworkflow |
| Klassieke en vereenvoudigde ribbon | Beide layouts beschikbaar | Visuele vergelijking van spacing, hoogte, afgeronde body en compacte regels |
| Grote/kleine knoppen en splitbuttons | Button/toggle/dropdown/split, dropdownmenu en verschillende itemgroottes | Meer voorbeelden van combinaties en groepsindeling |
| Groeptitel en launcher rechtsonder | ShowLauncher en OnLauncherClick beschikbaar | Duidelijkere demo met zichtbare launchers |
| Contextuele tabs | ContextualTabSets beschikbaar | Duidelijker voorbeeld van tijdelijk activeren en verbergen |
| Font-/alignmentvelden in groepen | Losse ERD-edits/comboboxes bestaan | First-class embedded controls, comboboxes, spin edits en kleurkeuzes als ribbonitems |
| Word-stijlgalerij | Geen ribbon-galleryitem | Scrollbare/selecteerbare galerij met previews en dropdown |
| Collapse/displayopties | Collapsed, pinning, overflow en vereenvoudigde layout bestaan | Office-achtige gecombineerde displaymode-dropdown |
| Toetsenbord en accessibility | Windows focus-cue-status voor bestaande focusranden; AutoSave via Tab/Space/Enter | Ribbon keytips/Alt-navigatie, UI Automation en accessibility voor eigenaar-getekende items |

## Voorgestelde volgorde voor verdere uitbreiding

1. Titlebar-search en documentstatus/dropdown: ontbreken duidelijk in de screenshotvergelijking en zijn breed bruikbaar.
2. Commands rechts op de tabrij en quick-access-customize: maken de commandosurface vollediger.
3. Embedded ribboncontrols en galerijen: nodig voor rijkere applicaties dan een verzameling knoppen.
4. Keytips en accessibility: gezamenlijk ontwerpen voor titlebar, ribbon en hun collecties.

Deze punten zijn een inventaris van vervolgwerk, geen claim dat ze al geïmplementeerd zijn.
De daadwerkelijke VCL-weergave en interactie moeten in Delphi op Windows worden getest.
