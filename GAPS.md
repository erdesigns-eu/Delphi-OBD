# Post-session gap review

After cleaning out the Python tooling, here's an honest audit of the
Delphi-side state vs. "production-ready" — what's solid, what has
holes, and what was never delivered.

## Critical (must fix before merging)

### G1. ✅ FIXED — `ResolveCatalogPath` now sees vehicle-class subdirs

`OBD.OEM.Catalog.Loader.pas` now probes top-level catalogs/ first,
then each of motorcycle/agricultural/marine/powersports. Backward
compatible: existing car catalogs at the top level still resolve in
the fast path.

### G2. ✅ FIXED — Delphi OEM extension units for all 33 new catalogs

Added 4 new units in `src/Services/`:
- `OBD.OEM.Motorcycles.pas` — 14 extensions (Ducati, Harley-Davidson,
  Triumph, BMW Motorrad, KTM, Yamaha-moto, Honda-moto, Kawasaki,
  Suzuki-moto, Indian Motorcycle, Royal Enfield, MV Agusta, Aprilia,
  Husqvarna-moto)
- `OBD.OEM.Agricultural.pas` — 8 extensions (John Deere, CNH,
  Caterpillar-Agri, Komatsu, Kubota, AGCO, Claas, Volvo CE)
- `OBD.OEM.Marine.pas` — 6 extensions (Mercury Marine, Volvo Penta,
  Yanmar Marine, MTU, Cummins Marine, Yamaha Marine)
- `OBD.OEM.Powersports.pas` — 5 extensions (Polaris, Can-Am/BRP,
  Arctic Cat, Yamaha WaveRunner, Kawasaki Jet Ski)

Each unit has a class-shared abstract base (`TOBDOEMMotorcycleBase`,
etc.) that does all the JSON wiring; concrete classes are 4-line
shells overriding `JsonFilename`/`ManufacturerKey`/`DisplayName`.
All 33 register at unit `initialization` so they self-activate when
the unit is added to a project's uses clause. Wired into RunTime.dpk
+ RunTime.dproj.

### G3. ✅ FIXED — `AllOEMCatalogsLoadFromDirectory` recurses

Now walks vehicle-class subdirs via `TSearchOption.soAllDirectories`
and skips DTC/iso/uds/obd2/test/_schema files. Loads each by absolute
path so the sweep doesn't depend on `ResolveCatalogPath`. Threshold
raised from >=40 to >=70 catalogs to reflect the new total.

### G4. CI Delphi build/test job is `if: false`

`.github/workflows/ci.yml::build-and-test` is gated on a self-hosted
Delphi runner that doesn't exist in this repository. **None of the
tests added this session (UdsClient / DTC.Schema / CatalogIntegrity
/ extended CatalogSmoke) have actually been compiled or run.** The
static-checks job covers syntax-mangling regressions, end-of-file
markers, CRLF, and (now) duplicate primary keys, but it does not
verify the test units compile against the current package.

**Status: cannot fix without a Delphi runner. Author needs to run
the test suite locally before merge.**

### G7. ✅ FIXED — `OBD.Protocol.DoIP.Session.pas` is `{$IFDEF MSWINDOWS}`-guarded

The unit body is wrapped in `{$IFDEF MSWINDOWS}` so non-Windows
targets compile to an empty unit. Cross-platform DoIP would need a
shared socket layer (Indy / Synapse) — out of scope for this
revision; flagged as future work in the unit's docstring.

## Quality issues (should fix)

### G5. ✅ FIXED — UDS client ASCII decoder guarded against empty payloads

`DecodePayloadAs` for `dkAscii` now checks `Length(Payload) > 0`
before taking `@Payload[0]`.

### G6. ✅ FIXED — `WriteAdaptation`/`WriteCodingBlock` always validate bounds

The `<> 0` guard has been removed in both methods. The JSON loader
uses `Low(Int64)`/`High(Int64)` as the "absent" sentinels, so
unbounded fields validate as a no-op while explicit `min=0, max=0`
now correctly enforces that the only legal value is 0. Regression
test `WriteAdaptation_FixedZeroEnforced` added to
`Tests.OEM.UdsClient` with a pinned in-memory catalog.

### G8. `Tests.OEM.UdsClient` mock-transport ARC pattern is fragile

The `NewMock(out Mock, out ITransport)` helper relies on the test's
local `ITransport` keeping the object alive. If a test re-orders the
locals (Catalog last, ITransport first) and the optimizer drops dead
locals, the mock could be freed mid-`Client.OpenSession`. The fix is
to make `TMockTransport` derive from a non-counted base
(`TSingletonImplementation` or override `_AddRef`/`_Release` to no-ops
for tests).

### G9. ✅ FIXED — `ResolveCatalogPath` consolidated

`Tests.OEM.DTC.Schema` and `Tests.OEM.CatalogSmoke` both now call
`OBD.OEM.Catalog.Loader.ResolveCatalogPath`. The duplicate local
helpers were deleted; tests stay in lock-step with production search
logic.

## Documentation / context (lower priority)

### G10. CHANGELOG / ROADMAP not updated for this session's work

I added `Phase E/F/A/C/B` commits but didn't fold them into
`CHANGELOG.md` / `docs/ROADMAP.md` with a v3.78+ tag, the way every
prior catalog-depth pass did (v3.39 → v3.76).

### G11. ✅ FIXED — Per-field type/format validation added

`Tests.OEM.SchemaShape` walks every shipped OEM catalog and asserts
the field-level constraints the JSON Schema would catch:
WMI codes match `^[A-Z0-9]{3}$`, `decoder.kind` / `field.kind` /
`adaptation.kind` use one of the recognised enum tags, DTC codes
match SAE J2012 (P/C/B/U) or J1939 SPN-FMI or one of the 22 OEM
prefixes the CI lint allows, manufacturer keys are non-empty, and
the schema version is 1 or 2. A full third-party JSON-Schema
validator is still out of scope, but the practical gap is closed.

### G12. ✅ FIXED — DoIP TLS + self-loop integration test

`OBD.Protocol.DoIP.Session.TLS` ships a real Indy + OpenSSL TLS
session for ISO 13400-3 §7 (TCP/3496). TLS 1.2 minimum (1.3
allowed), mutual-TLS via the `TDoIPTLSCredentials` record (root
CA, client cert/key/passphrase), peer-verification toggle, and
optional cipher-list override for OEM cert policies. Self-loop
test in `Tests.Protocol.DoIP.TLS.RoutingActivationAndDiagnosticRoundTripOverTLS`
spins a `TIdTCPServer + TIdServerIOHandlerSSLOpenSSL` on a
loopback port using a fixture cert pair, and asserts the round
trip is bit-exact across the encrypted channel.

## Small correctness checks not done

- The 1,282 DTC entries across 47 catalogs were authored against the
  v3.77 schema but never round-tripped through the Delphi loader
  (would require running `Tests.OEM.DTC.Schema`, which can't run
  without G4 fixed).
- `OBD.OEM.UdsClient.ReadCodingBlock` ASCII unpacking iterates
  `Field.BitWidth` characters from `Field.ByteOffset` — but the JSON
  loader stores ASCII length differently from numeric bit_width, and
  the cross-mapping hasn't been verified end-to-end.
- `OBD.Protocol.DoIP.Session.SendReceive` ✅ now caps the
  alive-check / ACK / NACK consumption loop at 16 frames per call;
  raises `EOBDDoIPTransportError` if the diagnostic-message
  response doesn't arrive within that window.
