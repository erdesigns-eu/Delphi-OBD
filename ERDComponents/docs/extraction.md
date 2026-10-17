# Zelfstandige repository

De inhoud van `ERDComponents` vormt de root van de nieuwe repository. De huidige
Delphi-OBD-repository blijft behouden. De extractie bevat de componentgeschiedenis
van de bronbranch; commit-hashes veranderen doordat de directoryprefix verdwijnt.

## Mee te nemen

Neem de volledige directory mee, inclusief verborgen `.gitattributes` en
`.gitignore`, `LICENSE`, `src`, `packages`, `assets`, `vendor`, `tools`, `tests`,
`docs`, `pascalcheck-projects.json` en de bestaande `demo`.

De eigen Git-attributen bewaren CRLF voor Delphi-bron- en projectbestanden.
Buildmappen, caches en lokale IDE-instellingen worden genegeerd. De bestaande
Windows-compiler onder `demo/compiler/windows-x64` blijft een expliciet gebundelde
optie voor de code-editor-demo. De componentpackages hangen niet van deze exe of
van een TSVM-checkout af. De manifestchecksum en vendor-hashes blijven behouden.
De librarylicentie en meegeleverde vendorlicenties worden ongewijzigd meegenomen.

## Controles vanuit de nieuwe root

Python 3 en Pillow zijn nodig voor de offline checks en het genereren van iconen.
Er is geen Node.js-, TSVM-, GitHub- of Delphi-OBD-checkout nodig voor deze checks.

```sh
python -m pip install Pillow
python tools/validate.py
python -m unittest discover -s tools -p 'test_*.py'
python tools/pascalcheck/run.py -v
```

Verwijder een eventuele `REPO`-environmentoverride voordat de Pascalcheckers
worden gedraaid; anders wijst die override bewust naar een andere checkout.
`pascalcheck-projects.json` markeert deze directory als checker-root.
De checkerregressies kunnen afzonderlijk worden uitgevoerd:

```sh
python -m unittest discover -s tools/pascalcheck -p 'test_checkers.py'
```

## Windows-releasecontrole — nog uitvoeren

De Linux-extractiecontrole kan geen Delphi/VCL-build of visuele tests vervangen.
De nieuwste documentbeheer- en LEDView-wijzigingen moeten op Windows worden
gebouwd en getest. Gebruik een RAD Studio command prompt met de Delphi SDK actief:

```bat
msbuild packages\ERDComponents_RT.dproj /t:Build /p:Config=Release /p:Platform=Win32
msbuild packages\ERDComponents_DT.dproj /t:Build /p:Config=Release /p:Platform=Win32
msbuild tests\ERDComponentsSmoke.dproj /t:Build /p:Config=Release /p:Platform=Win32
tests\Win32\Release\ERDComponentsSmoke.exe
```

Controleer daarnaast de LED-strip/ring visueel met een eigen form; daarvoor is
geen nieuwe demo-integratie toegevoegd. Zie `led-view-api.md` voor de API.
Installeer de standalone DT-package zoals beschreven in de README; installeer
RT en standalone DT niet tegelijk in dezelfde IDE. Extra Win64/Win64x-builds
vereisen de bijbehorende geïnstalleerde Delphi-targets.

## Overdracht

Een Git-bundle bevat de uitgefilterde componentgeschiedenis. Een bron-ZIP bevat
alleen de bestandsinhoud, inclusief correcte Delphi-regelafbrekingen. Vanuit een
bundle kan de repository op een andere machine worden aangemaakt:

```sh
git clone ERDComponents-extraction.bundle ERDComponents
cd ERDComponents
git remote remove origin
```

Koppel daarna de gewenste lege GitHub-repository als `origin` en push `main` en
de extractietag. De lokale extractie kiest geen nieuwe remote of zichtbaarheid.
Er worden geen bestanden uit de oorspronkelijke repo verwijderd.

Na de overdracht blijven herbruikbare controls en modellen in de componentrepo.
IDE-orchestratie, workspace-opslag en echte TSVM/USB-koppelingen horen in de
latere IDE-repository. De bestaande demo blijft een testhulpmiddel.
