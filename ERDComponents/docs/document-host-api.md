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
