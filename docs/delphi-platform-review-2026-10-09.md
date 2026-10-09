# Delphi platform review — 9 oktober 2026

Aanleiding: Delphi 12 Athens Win32 Release stopte achtereenvolgens op
Bluetooth-, BLE-, callback- en socketaanroepen die de Linux/FPC-build niet
kon controleren. De eerdere groene Linux-checks waren onvoldoende bewijs
voor de Delphi-platformcode.

De hele bronboom is opnieuw met de statische suite gecontroleerd. De gerichte
API- en levensduurcontrole omvat Bluetooth/BLE, WiFi/UDP, Serial/FTDI, J2534,
de Windows-socket van de TLS-backend en de gedeelde VCL-control/themacode.

## Hersteld in deze wijziging

| Probleem | Herstel |
| --- | --- |
| WiFi keep-alive/send-timeout en UDP broadcast gebruikten API-namen die Delphi niet aanbiedt. | `ERD.Compat.Socket` biedt nu dezelfde transportopties voor beide compilers; Delphi gebruikt native Winsock `SO_KEEPALIVE`, `SO_SNDTIMEO`, `SO_BROADCAST` en rapporteert fouten. |
| `ConnectTimeout` werd in de Delphi-tak genegeerd. | `ConnectWithTimeout` gebruikt voor Delphi een connect-worker, sluit de socket bij timeout en voegt de worker samen vóór terugkeer. FPC gebruikt zijn bestaande native timeout. DNS-resolutie gebeurt nog vóór deze TCP-connectdeadline. |
| Herhaald rechtstreeks `Open` kon de vorige socket/reader/handle achterlaten. | WiFi, UDP, Bluetooth, Serial en FTDI sluiten na settingsvalidatie de vorige verbinding. BLE deed dit al. |
| Serial gebruikte RTS ENABLE in plaats van HANDSHAKE en zette paritycontrole niet aan. | Hardwareflowcontrol gebruikt de juiste twee RTS-bits; parity volgt de instellingen, DTR/RTS zijn bij normale verbinding ingeschakeld. Sluiten annuleert ook de synchrone I/O van de readerthread. |
| De 20 J2534 exporttypen waren `cdecl`. | De Windows PassThru ABI gebruikt nu `stdcall`; DLL-adressen worden expliciet naar hun functiepointertype gecast. |
| J2534 `GetProcAddress` kreeg een Unicode `PChar`. | Exportnamen worden als `AnsiString`/`PAnsiChar` doorgegeven. |
| J2534 kopieerde driverdata zonder ontvangen buffergrenzen te controleren. | Het bericht wordt geïnitialiseerd; aantal, DataSize en ExtraDataIdx worden vóór allocatie/kopiëren gecontroleerd. |
| VCL textkleuren werden behandeld als stylekleur-enums (`scWindowText`, `scGenericGrayed`). | Gedeelde controls en theme gebruiken `StyleServices.GetSystemColor(clWindowText/clGrayText)`. |
| Een statische checker verwarde de gekwalificeerde native TLS `TSocket` met het projecttype. | TLS noemt `Winapi.Winsock2.TSocket` expliciet; de checker respecteert expliciete kwalificatie via een geïmporteerde unit. |

De eerdere Bluetooth/BLE API-, callback- en UUID-correcties blijven behouden.
De gewijzigde Delphi-bronbestanden zijn hier met CRLF gecontroleerd.

## Regressiecontrole

`check_platformapi.py` is toegevoegd aan de normale suite. Deze controle
herkent de bekende incompatibele Bluetooth/LE/socketaanroepen, const-mismatches
in reader-TProc callbacks, Unicode exportnamen, verkeerde J2534 calling
convention en de genoemde VCL-textkleuren. Fixtures bevatten zowel de foute
als correcte varianten, plus een geldige custom const-callback.

Resultaten:

- 98 statische checkers: nul bevindingen.
- 16 analyzerfixtures en 28 overige toolingtests: geslaagd.
- Drie Delphi IDE-projecten: metadata, configuraties, encoding en bronreferenties geldig.
- FPC 3.3.1: 273 niet-visuele Linux-units gecompileerd.
- 196 runtimechecks, 6 signaturechecks en 7 TLS-scenario's: geslaagd.
- Regeleinden en `git diff --check`: schoon.

## Validatie die Delphi en hardware vereist

Deze omgeving bevat geen Delphi-compiler of VCL-runtime. De native Windows-
opties en connect-worker, Bluetooth/BLE, Serial/FTDI, J2534 en VCL-painting zijn
hier aan de broncode gecontroleerd; hun Windows-compilatie en werking zijn
nog niet bevestigd. De statische API-contracten zijn gerichte checks en vormen
een gedeeltelijke beschrijving van de externe RTL/VCL-API.

Een volledige RT/DT-build en DUnitX-run in Delphi blijven nodig, gevolgd door
verbinden, timeout, verbreken/herverbinden en component-installatie met de
betreffende adapters. ECU-flashing en hardwaretokens blijven aparte hardwaretests.
Een volgende compilerfout kan dus nog een ongedekte Delphi-API of overload
blootleggen; alle mogelijke Delphi-fouten zijn pas uitgesloten voor de
daadwerkelijk gebouwde en geteste configuratie.
