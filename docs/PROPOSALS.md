# Next-milestone proposals

The original roadmap (v2.1 Foundation → v3.0 FMX & OEM) is complete and
tagged. This document is the menu for what comes next. Each proposal is
sized to ship as a single milestone with a clear exit criterion — pick
one, sequence two, or merge a few items across proposals into a custom
release.

Effort key: **S** ≤1 day · **M** 2–5 days · **L** 1–2 weeks · **XL** >2 weeks.
Priority key: 🔴 must-have for the proposal · 🟠 should-have · 🟢 nice-to-have.

## Status overview

| Proposal | Status | Shipped in |
|---|---|---|
| A — FMX Component Completion | ✅ Done | v3.1 |
| B — Mobile Transports | 🟡 Partial — FMX dashboard runs cross-platform; native Bluetooth/USB transports not yet shipped | (open) |
| C — Production-Grade Crypto + Manufacturer Coverage | ✅ Done | Crypto v3.2; OEM coverage v3.3–v3.69 (79 catalogs / 247k entries) |
| D — Heavy-Duty / Commercial | ✅ Done | v3.16 (J1939 base + 6 HD OEMs), expanded through v3.69–v3.76 |
| E — Performance, Testing & Profiling | 🟡 Partial — coverage harness shipped v3.79; broader perf-testing backlog open |
| F — Localization, Theming & Accessibility | 🔴 Open |
| G — DevEx & Community | 🟡 Partial — CI + changelog shipped; GetIt + community templates open |
| H — Async Polish & Persistence | ✅ Done — async UDS shipped v3.79 (`OBD.OEM.UdsClient.Async`) |

The detailed proposals below are kept verbatim for historical context
and as a reference for the open items. Each section header is annotated
with its current state.

---

## Proposal A — FMX Component Completion ✅ Shipped in v3.1

> Ship the remaining six visual components on FMX so a single codebase
> drives both desktop (VCL) and mobile/macOS (FMX).

**Why now:** The renderer-extract pattern is proven (v3.0 LinearGauge).
Each remaining component is a self-contained chunk roughly the size of
LinearGauge — plumbing is established, no new architecture required.

| Item | Pri | Eff |
|---|---|---|
| Extract `OBD.Render.Tachometer` + ship `TOBDTachometerFMX` | 🔴 | M |
| Extract `OBD.Render.TrendGraph` + ship `TOBDTrendGraphFMX` | 🔴 | M |
| Extract `OBD.Render.DtcList` + ship `TOBDDtcListFMX` (with FMX scroll/wheel handling) | 🔴 | L |
| Extract `OBD.Render.Terminal` + ship `TOBDTerminalFMX` | 🔴 | M |
| Extract `OBD.Render.Knob` + ship `TOBDKnobFMX` (with FMX gesture detector) | 🟠 | M |
| Extract `OBD.Render.SegmentedSwitch` + ship `TOBDSegmentedSwitchFMX` | 🟠 | S |
| Extract `OBD.Render.LED` + ship `TOBDLedFMX` | 🟢 | S |
| `Packages/DesignTime.FMX.dpk` for IDE registration | 🟠 | S |
| `examples/mobile_dashboard/` — FMX form using all the components | 🟠 | M |

**Exit criterion:** Every visual component shipped in v2.1/v2.2 has an
FMX twin with identical visual output. Mobile dashboard example builds
and runs on Win32, Win64, and a non-Windows FMX target (macOS or
emulated Android).

**Risk:** Some components (DtcList scroll, Knob drag) lean on platform
event-loop details. FMX gesture handling differs from VCL `MouseDown`/
`Move`/`Up` enough that a careful re-design is needed for those two.

---

## Proposal B — Mobile Transports 🟡 Partial

> Light up the FMX components on real iOS / Android / macOS hardware.

**Depends on:** Proposal A (or at least the LinearGauge FMX binding to
demo against).

| Item | Pri | Eff |
|---|---|---|
| iOS BLE transport over CoreBluetooth (`OBD.Connection.BLE.iOS.pas`) | 🔴 | L |
| Android BLE transport over the BluetoothGatt JNI bridge (`OBD.Connection.BLE.Android.pas`) | 🔴 | L |
| macOS serial via the IOKit serial gateway | 🟠 | M |
| Conditional compilation in `OBD.Connection.BLE.pas` so a single import works on every target | 🔴 | M |
| Field-test matrix: at least Vgate iCar Pro BLE, Veepeak BLE+, OBDLink CX on each OS | 🔴 | M |
| `examples/mobile_dashboard/` extended with live-mode BLE scan / connect | 🟠 | M |
| Android `INTENT.PERMISSION` + Info.plist Bluetooth keys documented in `docs/MOBILE.md` | 🟠 | S |

**Exit criterion:** A user can install the mobile-dashboard example on
their phone, scan, connect to a real BLE OBD adapter, and see live
data on the gauges.

**Risk:** Apple's Core Bluetooth permission model is finicky; Android
BLE has historic stack quirks per OEM ROM. Budget extra QA time.

---

## Proposal C — Production-Grade Crypto + Manufacturer Coverage ✅ Shipped (v3.2 + v3.3–v3.69)

> Make `TOBDECUFlashing` actually trustworthy in production by adding
> real RSA / ECDSA verifiers and three more OEM extensions (Mercedes,
> Ford, GM/Stellantis).

| Item | Pri | Eff |
|---|---|---|
| `TOBDOpenSSLSignatureVerifier` — RSA-PSS + ECDSA-P256 via OpenSSL bindings | 🔴 | L |
| `TOBDCryptoNGSignatureVerifier` — same, using Windows BCrypt as the no-external-DLL alternative | 🔴 | M |
| HSM hook contract (`IOBDHSMSession`) for caller-supplied hardware-backed verification | 🟠 | M |
| Anti-replay nonce primitive on top of UDS SecurityAccess | 🟠 | M |
| `OBD.OEM.Mercedes` — XENTRY-style session reference | 🟠 | L |
| `OBD.OEM.Ford` — Ford UDS extension reference (FoCCCAN, M2C subnet quirks) | 🟠 | L |
| `OBD.OEM.GM` — Global B / GMLAN extension reference | 🟠 | L |
| `OBD.OEM.Stellantis` — FCA / PSA combined extension | 🟢 | L |
| Update `examples/ecuflashing/` to use `TOBDECUFlashing` end-to-end with a real verifier | 🔴 | M |

**Exit criterion:** A production user can flash a real ECU with
`TOBDECUFlashing`, RSA-signed firmware verifies correctly via OpenSSL
or BCrypt, and four OEMs cover ≥80 % of the European/US market by VIN
WMI.

**Risk:** Real OEM-specific flashing protocols are guarded by NDAs and
closed datasheets; the references will necessarily be documented
starting points rather than complete implementations. Production users
will still need to plug in their own seed-key algorithms.

---

## Proposal D — Heavy-Duty / Commercial ✅ Shipped (v3.16 + v3.69–v3.76)

> Round out coverage for trucks, buses, agricultural equipment, and
> EV-Ethernet diagnostics.

| Item | Pri | Eff |
|---|---|---|
| `OBD.Protocol.J1939` polish — full PGN catalog (~300 PGNs), Address Claim, Transport Protocol BAM/CMDT | 🔴 | XL |
| Tachograph protocol completion (per-card auth flows) | 🟠 | L |
| CAN-FD support (configurable bitrates, 64-byte frames) | 🟠 | L |
| Automotive Ethernet 100BASE-T1 transport (`OBD.Connection.AutoEthernet`) beyond DoIP | 🟢 | XL |
| AUTOSAR Network Management primitives | 🟢 | M |
| `examples/heavy_duty_dashboard/` — gauges scaled for J1939 PGN data | 🟠 | M |

**Exit criterion:** A J1939-equipped vehicle (truck or excavator) can
be fully read out — engine PGNs, transmission PGNs, ABS PGNs — using
the framework with no custom protocol code at the call site.

**Risk:** Heavy-duty hardware to test against is expensive and not
universally accessible. Recommend pairing with a partner shop to
validate.

---

## Proposal E — Performance, Testing & Profiling 🟡 Partial (coverage harness shipped v3.79)

> Bring testing rigor up to "I'd ship this in a hospital" levels and
> publish numbers.

| Item | Pri | Eff |
|---|---|---|
| Image-snapshot test framework — render every visual component to a deterministic Skia surface, hash, compare to golden PNGs | 🔴 | L |
| Capture baseline images for every shipped component, both VCL and FMX | 🔴 | M |
| Adapter-fixture replayer integration — convert representative `.obdlog` files in `tests/fixtures/` into Service / Protocol regression tests | 🔴 | L |
| Real benchmark suite (`benchmarks/` directory, runs in CI nightly) reporting round-trip latency per adapter and per protocol | 🟠 | L |
| 60% coverage target hit on `Services/` / `Protocol/` / `VIN/` / `RadioCode/` (currently the test corpus exists but coverage isn't measured against the bar) | 🔴 | L |
| Custom attributes for service/PID metadata (`[OBDPid($0C, name='RPM', formula='((A*256)+B)/4')]`) — RTTI binds to UI without code | 🟠 | M |
| Performance regression alerts in CI nightly (compare against last green) | 🟢 | M |
| Memory pooling for `IOBDDataMessage` / `IOBDDataFrame` (reduce GC pressure on long-running dashboards) | 🟢 | M |

**Exit criterion:** CI publishes both the test results and the
benchmark numbers on every nightly. Image-snapshot regressions block
merges.

**Risk:** Snapshot tests are sensitive to font availability — bake the
test fonts into the repo or render via a known-stable typeface.

---

## Proposal F — Localization, Theming & Accessibility 🔴 Open

> Make the framework usable internationally and by non-sighted users.

| Item | Pri | Eff |
|---|---|---|
| `resourcestring` sweep — every UI string reachable from a single resources unit | 🔴 | L |
| Localization bundles for DE / FR / ES / NL / ZH / JA | 🔴 | M |
| RTL support — Arabic/Hebrew layout in components, especially DtcList and Terminal | 🟠 | M |
| Screen-reader audit — UIA names + roles on every visual component | 🟠 | L |
| High-contrast theme (`TOBDTheme.HighContrast`) | 🟠 | S |
| Per-component live `Theme` property override (apply by reference, react to mutation) — was deferred from v2.2 | 🟠 | M |
| `docs/LOCALIZATION.md` + `docs/ACCESSIBILITY.md` | 🟢 | S |

**Exit criterion:** A NVDA / JAWS user can drive the dashboard example;
strings render correctly in Arabic; high-contrast theme passes the
WCAG AA contrast tier.

---

## Proposal G — DevEx & Community 🟡 Partial

> Ship the polish that turns "good library" into "go-to library."

| Item | Pri | Eff |
|---|---|---|
| Radio code coverage expansion: Geely, BYD, NIO, Li Auto, Tata, Mahindra, Tesla | 🟠 | L |
| Session save/restore — persist a diagnostic session (selected protocol, recorded PIDs, DTCs) and resume on next launch | 🟠 | M |
| Multi-vehicle example — connect to two adapters at once, compare RPM curves | 🟠 | M |
| Fuel economy calculator + dashboard panel | 🟢 | S |
| Migration helper for existing plain-INI configs → `TOBDSecureSettings` | 🟠 | S |
| GetIt submission flow — actually push the v3.0 manifest, document the resubmission workflow for each tag | 🔴 | M |
| Signed-binary releases on GitHub Releases (Authenticode signed) | 🟠 | M |
| NuGet package (.NETStandard 2.0 wrapper or community-style port?) | 🟢 | L |
| Auto-deploy docs on every tag (already wired in `docs.yml`; verify after first push) | 🟠 | S |
| `docs/MIGRATION.md` — how-to for projects upgrading from v2.0 to v3.0+ | 🔴 | M |

**Exit criterion:** GetIt listing live, signed binaries on Releases,
≥5 new radio code brands, migration guide complete.

---

## Proposal H — Async Polish & Persistence ✅ Async UDS shipped v3.79

> Smaller, opportunistic — bundle as a v3.1 polish release if you don't
> want a big themed milestone.

| Item | Pri | Eff |
|---|---|---|
| Custom attributes for service/PID metadata (RTTI-driven binding) | 🟠 | M |
| FireDAC-backed persistent log + DTC history | 🟠 | M |
| Performance counters emitted as log events (round-trip, bytes/sec, error rate) | 🟠 | S |
| Recorder hooks reused as adapter test fixtures (folds into Proposal E) | 🟠 | S |
| Anti-replay nonce on security-access requests (folds into Proposal C) | 🟠 | S |
| Migration helper for plain-INI → secure storage (folds into Proposal G) | 🟠 | S |

**Exit criterion:** All current backlog items in `ROADMAP.md` ticked.
Functions as a fast-shipping cleanup release.

---

# How to choose

**Most user-visible value:** A + B (mobile reach).
**Most enterprise-credibility:** C (real crypto + OEM coverage).
**Most "this will hold up in production":** E (testing + benchmarks).
**Lowest effort, highest impact for solo maintainers:** H (async polish).

If forced to recommend a single sequence:

1. **v3.1: Proposal A** — FMX component completion (proves the
   pattern, immediately useful).
2. **v3.2: Proposal C** — production crypto + Mercedes/Ford OEM refs
   (turns ECU flashing from "demo-grade" to "shippable").
3. **v3.3: Proposal B** — mobile transports (capitalises on the FMX
   investment from v3.1).
4. **v3.4: Proposal E** — testing + benchmarks (lock in everything).
5. **v3.5: Proposal F or G** — localisation OR community polish,
   depending on which user complaint hurts more by then.
6. **v4.0: Proposal D** — heavy-duty (saves the deepest specialisation
   for last).

But it's your menu — pick whatever excites you.
