# Data Gaps

Tracks features that ship as **framework + stubs** because the
required reference data is not publicly available. Each entry lists
the precise data needed; once the data is supplied the stub is
replaced and the entry moves to a `### Resolved` section with the
release tag that closed it.

The honest path: rather than fabricate algorithms or test vectors that
*look* correct but produce wrong output, every data-pending feature
refuses to compute and raises a typed exception. Call sites can detect
this and surface a clear "data not available" message instead of
shipping a wrong code that could brick a head unit, mis-sign a
firmware image, or freeze-frame the wrong CAN message.

## Open

### v3.80 / 3.2 — Radio code calculator brands

Eight new brands ship registered through `OBD.RadioCode.Registry` but
back the calls with `TOBDRadioCodePending`, which raises
`EOBDRadioCodeDataMissing` on `Calculate`. The framework is tested
against pre-existing brands (Becker4 / Becker5) so the slot is real.

| Brand key | What's needed | Notes |
|---|---|---|
| `pioneer` | Verified serial → code algorithm or lookup table for at least the DEH/AVH/MVH model families. | Commercial DBs cover ~30M units; community-published algorithms are partial and generation-specific. |
| `kenwood` | Verified algorithm or lookup table for KDC/DDX/DNX/KMM model families. | Post-2008 JVC-Kenwood merger means an algorithm covering one may apply to the other. |
| `jvc` | Verified algorithm or lookup table for KD/KW model families. | Same merger note as Kenwood. |
| `sony` | Verified algorithm or lookup table for CDX/WX/MEX after-market head units. | Modern Sony OEM fitments are gateway-tied via VIN and out of scope. |
| `philips` | The licensed serial-to-code database (Philips ships ~14M entries). | EEPROM-extraction route is hardware-side and not implementable in this library. |
| `grundig` | A leaked or published lookup table for WKC/EC pre-2000 head units. | Possibly recoverable from a specific generation via the Becker4/Becker5 approach. |
| `panasonic` | CQ-series algorithm or lookup table; per-region variants common. | Matsushita-era OEM + after-market. |
| `continental_vdo` | Mapping from VDO part number to the underlying VAG variant. | OEM head-unit supplier in VW / Mercedes / Ford; often re-uses VAG variants but the per-PN mapping is undocumented publicly. |

**How to drop in real data:**

1. Replace the entry's factory in `OBD.RadioCode.Pending.pas` (or move
   it to a new `OBD.RadioCode.<Brand>.Advanced.pas` unit) with a real
   `TOBDRadioCode` subclass.
2. Set `DataAvailable := True` when registering with the registry.
3. Add at least one verified `serial → code` fixture in
   `tests/Tests.RadioCode.<Brand>.pas`.
4. Move the row out of "Open" into `### Resolved` with a tag.

## Resolved

*(empty; populated as gaps close)*
