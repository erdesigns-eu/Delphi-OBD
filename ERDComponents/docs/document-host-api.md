# Documenthost

`TERDDocumentHost` is een zelfstandige, themed ERD-container met eigen `TERDTabs`
en ERD-contentpaneel. De demo staat op **Docking**. Er is geen bestands-I/O,
compiler-, TSVM- of IDE-afhankelijkheid.

## Documenten openen

```pascal
Doc := Documents.OpenCode('project://main.ts', 'main.ts', Source);
Documents.OpenCode('project://template.json', 'template.json', Json, True);
Documents.ActivateDocument('project://main.ts');
```

Een ID is verplicht, exact en hoofdlettergevoelig. Normaliseer bestands-URI's in
de applicatie. `OpenCode` met een reeds geopend ID activeert hetzelfde document;
de aangeboden tekst, caption en read-only waarde vervangen niets. Een ID van een
niet-code-document gebruiken voor `OpenCode` geeft een argumentexception.

Elke codepagina heeft een eigen `TERDCodeEditor`. Daardoor blijven caret,
selectie, undo, scrollpositie en folds aanwezig wanneer een andere tab actief is.
Gebruik `Documents.ActiveDocument.Editor` om bijvoorbeeld `RevealDebugLine` en
`SelStart` voor navigatie aan te roepen. Stel `DebugModel`/language service per
editor in de applicatie in. Een generiek document kan `Editor = nil` hebben.

`DocumentCount`, `Documents[Index]` (openvolgorde), `FindDocument` en
`ActiveDocument` geven geleende verwijzingen, geldig totdat het document sluit.
Tabherordening verandert geen document-ID. `TabStrip` is bedoeld voor presentatie
zoals breedte en hints; wijzig de collectie, Page-bindingen en interne events
uitsluitend via de documenthost. Documenten en interne controls worden niet als
runtime-inhoud in de DFM opgeslagen.

## Opslaan

```pascal
procedure TMain.Save(Sender: TObject; const Request: TERDDocumentSaveRequest);
begin
  // Schrijf Request.Text naar de bestemming die bij DocumentID hoort.
  // Roep CompleteSave alleen aan als het opslaan geslaagd is.
  Documents.CompleteSave(Request);
end;
```

`SaveDocument(ID)` geeft False voor onbekende/read-only documenten of zonder
`OnSave`. True betekent dat het event is afgegeven, niet dat opslag geslaagd is.
Een request bevat een kopie van de tekst, document-ID, unieke instance en revision.
Bewaar het volledige record voor een asynchrone save en lever het ongewijzigd
terug op de UI-thread. `CompleteSave` accepteert alleen het laatst aangevraagde
snapshot van de nog geopende instance, eenmaal. Een late bevestiging kan dus geen
opnieuw geopend document schoonmaken. Een bevestiging van oudere tekst stelt de
opgeslagen baseline bij, maar latere wijzigingen blijven dirty.

De dirty-status van code volgt de vergelijking met de laatst bevestigde tekst.
Undo/terugkeren naar die tekst maakt het document weer schoon. Gebruik
`SetDocumentReadOnly` om editor en badge samen aan te passen. Directe wijzigingen
van `Editor.ReadOnly` houden de metadata niet bij. Laat `Editor.OnChange` aan de
host gekoppeld; gebruik `OnDocumentChanged` voor applicatienotificaties.

## Sluiten

`CloseDocument(ID)` en de tab-closeknop plaatsen een sluitverzoek in de
Windows-messagequeue. True betekent dat het verzoek is ingepland. De pagina
wordt niet vrijgegeven terwijl de tab-muiscallback nog op de stack staat.

Bij verwerking is `AllowClose` standaard True voor schone documenten en False
voor gewijzigde documenten. `OnClosing` kan dat wijzigen na een eigen save- of
confirmatiedialoog. Zonder event blijven gewijzigde documenten open. Nieuwe
wijzigingen tijdens de callback blokkeren de sluiting opnieuw. Meerdere
verzoeken voor dezelfde instance worden samengevoegd; een oude aanvraag kan geen
heropend document met hetzelfde ID sluiten. Sluiten vernietigt de eigen editor.
Hostdestructie ruimt alle documenten op zonder dialogs of veto; de applicatie
moet vóór formdestructie haar eigen unsaved-work beleid uitvoeren.

## Andere ERD-documenten

```pascal
Documents.AddDocument('project://board', 'Board', PCBDesigner);
Documents.SetDocumentModified('project://board', True);
```

`AddDocument` leent een bestaande `TWinControl` en neemt ownership niet over.
De control wordt tijdelijk in de contentcontainer geplaatst. Bij sluiten of
hostdestructie worden oorspronkelijke parent, bounds, align, anchors, taborder en
zichtbaarheid hersteld. Een verwijderde oorspronkelijke parent wordt nil.
Vroegtijdig vernietigde content wordt via FreeNotification uit de tabs verwijderd.
Voeg dezelfde control niet tegelijk aan meerdere hosts toe.

De applicatie meldt wijzigingen voor deze documenten via
`SetDocumentModified`; elke melding verhoogt de revision. Het generieke save-event
heeft lege Text: de applicatie moet zelf een passend, immutable inhoudssnapshot
bewaren. Een savebevestiging maakt alleen dezelfde revision schoon.
Read-only is hier metadata; de applicatie moet dit op de control toepassen.

Alle API en callbacks draaien op de UI-thread. Bewaar geen documentverwijzing
over een queued close heen. Vernietig de host niet vanuit zijn eigen callbacks.
`OnActiveDocumentChanged` kan nil krijgen wanneer de laatste pagina sluit.

## Verificatie

De Windows-smoketest `TestDocumentHost` controleert retained editors/selectie,
herordening, dirty/read-only gedrag, versiegebonden savebevestiging, late replies,
queued close/veto, expliciet discard, geleende content, vroegtijdige destruction
en DFM zonder runtime-documenten. De showcase-smoketest controleert de koppeling
van projectboom naar documenthost.

Handmatig op Delphi Windows: open main.ts en board.ts; wijzig beide, verplaats de
caret, scroll en vouw code in. Wissel tabs en open hetzelfde bestand opnieuw.
Controleer dat toestand behouden blijft, close op dirty wordt geweigerd, Save
document de stip verwijdert, en een daarna gesloten/heropend document de in-memory
opgeslagen tekst bevat. Navigeer ook vanuit diagnostics en wissel thema/density.

## Gedeeld debugmodel en bronnavigatie

```pascal
Documents.DebugModel := DebugModel;
Documents.OnResolveDocument := ResolveDocument;
Documents.NavigateToSource('project://main.ts', 24, 8);
```

De host bindt `DebugModel` aan alle bestaande en nieuw geopende code-editors.
Gutter-breakpoints, sneltoetsen en execution markers gebruiken daardoor dezelfde
modelinstantie als een los breakpointpaneel. Het model wordt geleend; vervangen,
loskoppelen en vernietigen worden afgehandeld zonder ownership over te nemen.
De individuele editors verwerken het verplaatsen van breakpointankers bij edits.

`NavigateToSource(ID, Line, Column, FocusEditor)` gebruikt één-gebaseerde regels
en UTF-16-kolommen. Kolommen voorbij het regeleinde worden begrensd. Ongeldige
coördinaten of een niet-bestaande regel geven False. Navigatie klapt de doelregel
open, wist de selectie, verplaatst de caret en activeert de juiste tab. Een
bestaand document wordt nooit opnieuw geladen of writable gemaakt. Met
`FocusEditor=False` wordt geen toetsenbordfocus gevraagd.

Is het ID nog niet geopend, dan ontvangt de applicatie synchroon
`OnResolveDocument(Sender, ID)`. De applicatie kan daar `OpenCode` aanroepen met
haar eigen bron en read-only beleid. Daarna zoekt de host het document opnieuw.
Geen beschikbare bron, of een generiek document zonder editor, geeft False.
De host verzint geen broninhoud en leest geen bestanden. Recursieve resolve-
aanvragen worden onderdrukt. Voor asynchrone bronophaling: open het document na
ontvangst op de UI-thread en vraag navigatie opnieuw aan.

`RevealExecutionLocation(FocusEditor)` navigeert naar de huidige pauzelocatie
van het gedeelde model. Running/disconnected of een onoplosbare bron geeft False.
De applicatie roept dit aan wanneer zij een nieuwe stoplocatie verwerkt; algemene
modelwijzigingen laten de documenthost niet automatisch van tab wisselen.

In de dockingdemo delen documenthost en breakpointpaneel één model. De ribbon
heeft **Pause main**, **Pause board**, **Resume fixture**, **Stop fixture** en
**Breakpoints**. Deze acties gebruiken uitsluitend gesimuleerde debugstatus.
Dubbelklikken op een breakpoint en navigatie vanuit diagnostics gebruiken dezelfde
bronresolver. Sluit board.ts en gebruik Pause board om het openen op aanvraag te
controleren. Wijzig main.ts en wissel de pauzelocatie om behoud van edits te testen.

`TestDocumentDebug` voegt Windows-smokegevallen toe voor gedeelde/replacement-
modellen, vernietiging, geïsoleerde breakpointankers, bronresolutie, read-only
navigatie, begrensde kolommen, ontbrekende bron en execution-navigatie na sluiten.


## Documentbeheer als component-API

- `SaveAllDocuments`: vraagt opslag aan voor alle gewijzigde, schrijfbare
  documenten. De returnwaarde telt verzoeken, niet geslaagde saves. Ongewijzigde
  en read-only documenten worden overgeslagen. Documenten die een savecallback
  nieuw opent, vallen buiten deze ronde.
- `CloseAllDocuments`: plant sluiten van de nu geopende documenten in.
- `CloseOtherDocuments(ID)`: behoudt het opgegeven document.
- `CloseDocumentsToRight(ID)`: gebruikt de actuele visuele tabvolgorde, ook na
  herordenen. Ongeldige of lege doel-ID's sluiten niets.

De sluitacties gebruiken één queue met document-instances. Nieuwe documenten
vallen buiten de aanvraag. Annuleren of veto stopt resterende verzoeken; eerder
uitgevoerde sluitingen worden niet teruggedraaid. De queue wordt tijdens een
modale dialoog niet opnieuw verwerkt.

`ConfirmClose=True` schakelt de ingebouwde `TERDTaskDialog` in voor dirty
sluitingen: **Save**, **Don't save**, **Cancel**. De dialoog gebruikt de effectief
geërfde theme en density van de host. Save wordt alleen aangeboden bij een
schrijfbaar document met een `OnSave`-handler. Cancel is de veilige standaard.
De standaardwaarde van ConfirmClose blijft False voor bestaande toepassingen.
Een toegewezen `OnClosing` neemt de beslissing volledig over; dan verschijnt
geen ingebouwde dialoog. Subclasses kunnen `ConfirmDocumentClose` overriden voor
bijvoorbeeld eigen vertalingen of automatisering.

Bij Save blijft het document open zolang de snapshot nog niet bevestigd is.
De overige sluitverzoeken wachten mee. `CompleteSave(Request)` hervat de queue
alleen als het document daarna schoon is. Nieuwe edits tijdens de save stoppen
de resterende sluitingen. Gebruik **`FailSave(Request)`** wanneer opslag mislukt:
het request wordt eenmaal geconsumeerd, het document blijft gewijzigd en de
resterende sluitverzoeken vervallen. Zonder CompleteSave/FailSave blijft een
asynchroon verzoek wachten. Beide antwoorden moeten op de UI-thread komen.

De dockingdemo bevat alleen de API-acties op de bestaande ribbon en een
in-memory savehandler. Navigatiegeschiedenis, workspace-opslag en echte
bestandshandelingen blijven werk voor de toekomstige IDE.

De Windows-smoketest `TestDocumentManagement` dekt save-all, tabvolgorde,
nieuwe documenten tijdens een aanvraag, batch-cancel/discard, een messagepump
binnen de bevestiging, synchrone/asynchrone save en nieuwe edits of opslagfouten.
