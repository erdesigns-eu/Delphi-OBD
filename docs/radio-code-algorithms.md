# Radio-code algorithms — what's bundled vs what isn't

Delphi-OBD v2 ships **one component per vendor** on the **OBD Radio**
palette tab. Drop the right one onto a form, set `Input` (and
optionally `VIN` / `ModelHint` / `Region`), call `Calculate`, read
`Result_`. Every vendor component validates the input shape (length /
prefix / character set) the same way regardless of whether the
algorithm is bundled.

Production radio-code algorithms range from *trivially documented*
(Hyundai 2002+ is `(serial + 1212) mod 10000`) to *commercially
licensed* (BMW Professional, VW RCD/RNS) to *EEPROM-stored* (Volvo
HU, Opel CD30/CD70 — the code lives on a chip, no calculation).

This document is the honest inventory: which vendor components ship a
real algorithm, which ship as `OnCalculate` stubs, and where to look
for sources if you need to wire your own.

## Status legend

| Marker | Meaning |
|---|---|
| ✅ | **Real algorithm bundled.** `Calculate` returns a code without any host hookup. Verified with pinned-vector tests. |
| 📦 | **Real database bundled.** Lookup table ships under `catalogs/radio-code/`; loaded lazily on first `Calculate`. |
| 🔌 | **OnCalculate stub.** Component validates input + fires `OnCalculate` for the host to supply the algorithm. |
| 💡 | **OnCalculate stub** — but a documented public algorithm exists (linked below). |

## Inventory

### European premium

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeVW`           | 🔌 | VW Gamma / Beta / Alpha / RCD / RNS — licensed. See "VW SAFE extraction" below for an over-the-bus alternative. |
| `TOBDRadioCodeAudiConcert`  | 🔌 | Audi Concert / Symphony — licensed. |
| `TOBDRadioCodeBMW`          | 🔌 | BMW Business CD / Professional / Navigation / Modern — licensed. |
| `TOBDRadioCodeMercedes`     | 🔌 | Mercedes-Benz factory radios (Becker BE2xxx+) — codes live in EEPROM (24C02 at known offset); see [slomkowski tutorial](https://slomkowski.eu/tutorials/extracting-security-codes-from-car-radios/). |
| `TOBDRadioCodeMini`         | 🔌 | BMW group; same as BMW. |
| `TOBDRadioCodePorsche`      | 🔌 | Porsche PCM (Becker-built); private community algorithm exists on RennTech. |
| `TOBDRadioCodeSEAT`         | 🔌 | VW group. |
| `TOBDRadioCodeSkoda`        | 🔌 | VW group. |
| `TOBDRadioCodeSmart`        | 🔌 | Mercedes platform (Grundig MC). |

### French / Italian

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeCitroen`      | 🔌 | PSA-branded; same family as Peugeot. |
| `TOBDRadioCodePeugeot`      | ✅ | Source: [`erdesigns-eu/Peugeot-Calculator`](https://github.com/erdesigns-eu/Peugeot-Calculator). Input: 4-digit C7-barcode suffix (NOT the serial). |
| `TOBDRadioCodeRenault`      | ✅ | Source: [`erdesigns-eu/Renault-Calculator`](https://github.com/erdesigns-eu/Renault-Calculator). Pre-2004 (Tuner List / Update List). |
| `TOBDRadioCodeFiatDaiichi`  | ✅ | Source: [`erdesigns-eu/Fiat-Daiichi-Calculator`](https://github.com/erdesigns-eu/Fiat-Daiichi-Calculator). |
| `TOBDRadioCodeFiatVP`       | ✅ | Source: [`erdesigns-eu/Fiat-VP1-VP2-Calculator`](https://github.com/erdesigns-eu/Fiat-VP1-VP2-Calculator). VP1 + VP2 share one calculation. |
| `TOBDRadioCodeAlfaRomeo`    | 🔌 | Blaupunkt-built; closed source. |
| `TOBDRadioCodeMaserati`     | 🔌 | Becker / Blaupunkt-built; closed source. |

### British

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeJaguar`       | 🔌 | Alpine-supplied; commercial only. |
| `TOBDRadioCodeLandRover`    | 🔌 | Alpine / Visteon-supplied; commercial only. |
| `TOBDRadioCodeSaab`         | 🔌 | YS / PH-series; commercial only. |
| `TOBDRadioCodeOpel`         | 💡 | CD30 / CD70 codes live in 24C32 / 95640 EEPROM at fixed offset; see [vauxhallownersnetwork thread](https://www.vauxhallownersnetwork.co.uk/threads/how-to-find-the-security-code-of-your-cd30-or-cd70-without-a-car-pass.839407/). |

### Asian

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeAcura`        | 💡 | Honda group; database-backed. See [`cdcoyne/HondaRadioCode`](https://github.com/cdcoyne/HondaRadioCode). |
| `TOBDRadioCodeHonda`        | 💡 | Database-backed; same source as Acura. |
| `TOBDRadioCodeHyundai`      | ✅ | 2002+ factory radios. `(pre-code + 1212) mod 10000`; 2009-only variant uses `+1222` (set `ModelHint = '2009-variant'`). Source: [MHH Auto thread](https://mhhauto.com/Thread-HYUNDAI-RADIO-PIN-CODE-CALCULATOR-2002-TO-CURRENT-2015). |
| `TOBDRadioCodeInfiniti`     | 🔌 | Multi-vendor (Clarion / Blaupunkt). |
| `TOBDRadioCodeLexus`        | 💡 | Toyota ERC; commercial-only. See [PELock](https://www.pelock.com/products/toyota-erc-calculator-radio-unlock-code-generator). |
| `TOBDRadioCodeMazda`        | 🔌 | EEPROM-read or paid lookup. |
| `TOBDRadioCodeMitsubishi`   | 🔌 | No public algorithm. |
| `TOBDRadioCodeNissan`       | 🔌 | Multi-vendor. |
| `TOBDRadioCodeSubaru`       | 💡 | Clarion-built; pre-2003 documented as last-4-of-serial → code (see Clarion). |
| `TOBDRadioCodeSuzuki`       | 🔌 | Blaupunkt-built (PACR series). |
| `TOBDRadioCodeToyota`       | 💡 | ERC; commercial-only. See [PELock](https://www.pelock.com/products/toyota-erc-calculator-radio-unlock-code-generator). |

### American

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeChrysler`     | 🔌 | TM9 / Panasonic-built. |
| `TOBDRadioCodeFordM`        | 💡 | Ford "M-prefix" series; full pre-computed serial→code lookup at [`OlegSmelov/ford-radio-codes`](https://github.com/OlegSmelov/ford-radio-codes), Flipper Zero impl at [`DavidB445/fz_fordradiocodes`](https://github.com/DavidB445/fz_fordradiocodes). Will ship as `TOBDRadioCodeFordV` (`TOBDRadioCode.FordVDatabase`) once the database is extracted to JSON. |
| `TOBDRadioCodeGM`           | 💡 | Delco Theftlock — dealer-call procedure, not a calculation. See [Diesel Place thread](https://www.dieselplace.com/threads/here-is-how-to-unlock-delco-theftlock-radio-that-displays-loc.40705/). |
| `TOBDRadioCodeVisteon`      | 🔌 | Fiat Stilo / Bravo Visteon — commercial only. |

### Aftermarket

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeAlpine`       | 🔌 | Commercial only. |
| `TOBDRadioCodeBlaupunkt`    | 🔌 | BPcalc tools circulate but are closed source. |
| `TOBDRadioCodeClarion`      | 💡 | C0 / C7 series; community-documented. See [MHH Auto thread](https://mhhauto.com/Thread-Clarion-Code-generator). |
| `TOBDRadioCodeBecker4`      | 📦 | 10,000-entry database at `catalogs/radio-code/becker4.json`. Imported from [v1 ERDesigns Becker4 calculator](https://github.com/erdesigns-eu/Delphi-OBD/tree/main/src/RadioCode). |
| `TOBDRadioCodeBecker5`      | 📦 | Same shape; `becker5.json`. |

### Outliers

| Component | Status | Notes |
|---|---|---|
| `TOBDRadioCodeVolvo`        | 💡 | Code lives in 24C01 EEPROM. See [HU-601 dump gist](https://gist.github.com/klalle/1ae1bfec5e2506918a3f89492180565e). |

## Wiring `OnCalculate`

Every component (real or stub) accepts a host-supplied
`OnCalculate` handler. For stubs it's the only path to a result;
for real / database calculators the host can override by wiring
the event before calling `Calculate`.

```pascal
procedure TForm1.MyHondaCalc(Sender: TObject;
  const AInput: string; const AContext: TOBDRadioCodeContext;
  var AResult: TOBDRadioCodeResult);
begin
  // Look up via your own database, network call, paid service ...
  AResult.Code    := MyHondaDatabase.LookupCode(AInput);
  AResult.Success := AResult.Code <> '';
  if not AResult.Success then
    AResult.Message := 'Serial not in database';
end;

procedure TForm1.btnUnlockClick(Sender: TObject);
var
  R: TOBDRadioCodeResult;
begin
  HondaCalc.OnCalculate := MyHondaCalc;
  HondaCalc.Input := edtSerial.Text;
  R := HondaCalc.Calculate;
  if R.Success then
    edtCode.Text := R.Code
  else
    ShowMessage(R.Message);
end;
```

## Related: VW Gamma / Premium SAFE extraction over KWP1281

Several VW group radios (Gamma 5, Premium IV / V, Rhapsody) expose
a documented KWP1281 backdoor that lets the host extract the SAFE
code from the radio's EEPROM over the diagnostic bus — no
calculation, no database, no paid service.

`TOBDVWRadioSAFE` (separate component, in
`OBD.Service.VWRadioSAFE.pas` once the KWP1281 codec lands) wraps
the documented sequence. Reference C source:
[`mnaberez/vwradio`](https://github.com/mnaberez/vwradio).

## Adding your own algorithm

If you reverse-engineer or licence an algorithm for a brand
currently shipped as a stub, the cleanest path is to subclass the
component, override `DoCalculate`, and register the subclass at
unit init:

```pascal
type
  TMyBMWCalc = class(TOBDRadioCodeBMW)
  protected
    function DoCalculate(const AInput: string;
      const AContext: TOBDRadioCodeContext): TOBDRadioCodeResult; override;
  end;

initialization
  TOBDRadioCodeRegistry.Default.RegisterClass(TMyBMWCalc);
```

Open a PR if your algorithm is public-domain — a real implementation
beats a stub for every host using the package.
