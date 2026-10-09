# KWPReadID — KWP2000 read identifiers

Demonstrates `TOBDKWPReadID` (Services 0x1A / 0x21 / 0x22). Each
helper builds the request, validates the identifier echo in the
response, and returns the payload bytes verbatim.

```sh
dcc32 -B KWPReadID.dpr
KWPReadID
```
