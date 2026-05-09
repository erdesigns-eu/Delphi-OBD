# Radio Code Calculators

The `src/RadioCode/` units compute the unlock code for OEM head units after
battery loss or theft-protection lockout. Every calculator implements
`IOBDRadioCode` (`src/RadioCode/OBD.RadioCode.pas`):

```pascal
IOBDRadioCode = interface
  function GetDescription: string;
  function Validate(const Input: string; var ErrorMessage: string): Boolean;
  function Calculate(const Input: string; var Output: string;
                     var ErrorMessage: string): Boolean;
end;
```

`TOBDRadioCode` (the base class) supplies the input-sanitisation helpers
(`SanitizeInput`, `ValidateLength`, `ValidateDigits`, …) so each brand
unit only contains the algorithm itself.

## Usage

```pascal
uses
  OBD.RadioCode, OBD.RadioCode.VW.Advanced;

var
  Calc: IOBDRadioCode;
  Code, Err: string;
begin
  Calc := TOBDRadioCodeVWAdvanced.Create;
  if Calc.Calculate('1234567', Code, Err) then
    ShowMessage('Code: ' + Code)
  else
    ShowMessage('Failed: ' + Err);
end;
```

Inputs are always strings (serial numbers, VINs, or pre-codes printed on
the radio chassis). The calculator is responsible for validating format
before computing — a `False` return from `Calculate` means the input was
rejected, not that the algorithm failed.

## Regional and security variants

`OBD.RadioCode.Variants` (`src/RadioCode/OBD.RadioCode.Variants.pas`)
manages algorithm selection across:

- **Region** — `TRadioCodeRegion` (`rcrNorthAmerica`, `rcrEurope`,
  `rcrAsia`, `rcrAustralia`, `rcrMiddleEast`, `rcrSouthAmerica`,
  `rcrAfrica`, `rcrUnknown`).
- **Model-year range** — `TRadioCodeYearRange` (use `EndYear = 9999` for
  open-ended).
- **Security version** — `TRadioCodeSecurityVersion` for OEMs that
  rotated the algorithm (e.g. VAG Concert/Symphony updates).

A single brand unit can register multiple variants; `Variants` resolves
the right one from VIN or model-year metadata.

## Brand coverage

Each brand has a dedicated unit `OBD.RadioCode.<Brand>.Advanced.pas`
unless noted otherwise. All units register on initialisation, so simply
including them via `uses` makes them discoverable through the registry.

### Japanese
Acura, Honda, Hyundai, Infiniti, Lexus, Mazda, Mitsubishi, Nissan,
Subaru, Suzuki, Toyota.

### European — VAG group
Audi (Concert/Symphony variants), SEAT, Skoda, VW.

### European — premium
BMW, Mercedes, Mini, Porsche, Smart, Saab, Volvo, Jaguar, Land Rover,
Maserati.

### European — French
Citroen, Peugeot, Renault.

### European — Italian
Alfa Romeo, Fiat (Daiichi and VP variants).

### European — other
Opel.

### American
Chrysler / Jeep / Dodge, Ford (Advanced + V-series), GM (Chevrolet,
Cadillac, GMC, Buick).

### Universal head-unit OEMs
Becker (Becker4, Becker5, Advanced), Blaupunkt, Alpine, Clarion,
Visteon.

## Adding a new calculator

1. Create `src/RadioCode/OBD.RadioCode.<Brand>.Advanced.pas`.
2. Inherit from `TOBDRadioCode`, override `Validate` and `Calculate`.
3. Use the `SanitizeInput` / `ValidateLength` / `ValidateDigits`
   helpers — don't reimplement input cleaning.
4. Register the unit in `Packages/RunTime.dpk` and `RunTime.dproj`.
5. Add a fixture-driven test in `tests/` with at least one known
   serial → code pair from a public service-manual reference.

## Notes

- These calculators target **legitimate recovery** by the vehicle owner
  or an authorised workshop after a battery disconnect. They are not
  bypass tools — every algorithm derives the code from the radio's own
  serial number, so they only work when you have physical access to the
  unit.
- Newer head units (post-2015 on most OEMs) tie the radio to the VIN
  via the gateway, in which case unlock requires online dealer
  activation rather than a serial-derived code. Those models are
  intentionally out of scope for this library.
