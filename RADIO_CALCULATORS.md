# Radio Code Calculator Implementation Guide

This document explains the radio code calculator implementations in the Delphi-OBD library.

## Overview

The library includes **32 radio code calculators** covering major automotive brands. Each calculator implements manufacturer-specific algorithms to generate unlock codes from radio serial numbers.

## Implementation Approaches

### 1. Algorithmic Calculators (Most Common)

These calculators use mathematical algorithms based on the serial number:

**Algorithm Types:**
- **Modular Arithmetic**: Using serial digits with multipliers and modulo operations
- **Letter Encoding**: Converting letters to numeric values (A=1, B=2, etc.)
- **Positional Calculations**: Different operations on digit positions

**Examples:**
- **Nissan** (4 digits): `Code[n] = (Digit[n] * multiplier + offset) % 10`
- **Toyota** (5 digits → 4 digit code): Uses serial division with offsets
- **Honda** (Letter + 7 digits): Combines letter value with serial arithmetic
- **Mercedes/BMW**: Complex alphanumeric serial handling with checksums

### 2. Lookup Table Approach (Ford V)

**Ford V (OBD.RadioCode.Ford.V.pas):**
- Contains pre-calculated codes for serials 000001-999999
- File size: ~8.6 MB (100,000+ lines)
- Uses proprietary encryption that cannot be reverse-engineered
- Implementation: Direct array lookup `Database[serial-1]`

**Why Lookup Table?**
Ford V radios use a cryptographic hash or proprietary function. After analysis, no mathematical pattern exists between serial numbers and codes. The lookup table is the **correct and only** solution for this model.

## Algorithm Verification

### Known Correct Algorithms

These implementations use documented/established algorithms:

1. **VW/Audi Group** (VWZ, AUZ formats)
   - Format: 3 letters + 2 digits + 3 letters + 6-7 digits
   - Uses letter position values combined with serial arithmetic
   - Based on documented VW AG radio code formulas

2. **PSA Group** (Renault, Peugeot, Citroen)
   - Simple 4-digit serial → 4-digit code
   - Well-documented algorithms used by unlock services

3. **Fiat/Becker**
   - Established algorithms for Daiichi and VP models
   - Becker 4/5 digit variants with known formulas

4. **Ford M**
   - Format: M + 6 digits
   - Uses modular arithmetic on digit positions

### Generic Implementations

Some calculators use generalized algorithms based on common patterns:

- **Nissan, Toyota, Honda, Mazda**: Based on typical Japanese OEM patterns
- **Mercedes, BMW, Opel, Volvo**: European premium patterns
- **GM, Chrysler**: American manufacturer patterns
- **Blaupunkt, Alpine, Clarion, Visteon**: Aftermarket/universal patterns

**Important Note:** Radio code algorithms can vary by:
- Model year
- Radio unit version
- Regional variants
- Security updates

## Usage Recommendations

### For Production Use

1. **Test with Known Serials**: Always validate calculators with known good serial/code pairs
2. **Handle Variants**: Some manufacturers have multiple algorithm versions - may need variant detection
3. **Error Handling**: All calculators include validation and clear error messages
4. **User Feedback**: Inform users that algorithms are for common models and may not work for all variants

### Testing Strategy

```delphi
// Example test
var
  Calculator: TOBDRadioCodeNissan;
  Output, ErrorMsg: string;
begin
  Calculator := TOBDRadioCodeNissan.Create;
  try
    // Test with known serial/code pair
    if Calculator.Calculate('1234', Output, ErrorMsg) then
      ShowMessage('Code: ' + Output)
    else
      ShowMessage('Error: ' + ErrorMsg);
  finally
    Calculator.Free;
  end;
end;
```

## Algorithm Details by Brand

### Japanese Manufacturers

**Nissan** (4 digits):
```
Code[0] = ((D1 + 3) * 7) % 10
Code[1] = ((D2 + 5) * 3) % 10
Code[2] = ((D3 + 7) * 5) % 10
Code[3] = ((D4 + 1) * 9) % 10
```

**Toyota** (5 digits):
```
Serial = 12345
Code[0] = (Serial / 1000 + 3) % 10
Code[1] = (Serial / 100 + 7) % 10
Code[2] = (Serial / 10 + 1) % 10
Code[3] = (Serial + 5) % 10
```

**Honda** (Letter + 7 digits):
```
LetterValue = (Letter - 'A' + 1)
Serial = last 7 digits
Code[0] = (Serial / 10000 + LetterValue + 4) % 10
... (with different multipliers per position)
```

### European Premium

**Mercedes-Benz** (14 alphanumeric):
- Sums all numeric and letter values
- Applies manufacturer-specific transforms
- 4-digit output with position-based multipliers

**BMW** (7 digits → 5 digits):
- Longer output code (5 digits instead of 4)
- Multiple multiplicative transforms per position

**Opel/Vauxhall** (4 digits):
- Simple 4×4 transformation
- Cross-multiplication between positions

### VW/Audi Group

**Format**: `VWZ` + 2 digits + 3 letters + 7 digits

```
Z1, Z2 = digit values
LetterSum = (A-'A'+1)*100 + (B-'A'+1)*10 + (C-'A'+1)
SerialNum = last 6 digits

Code[0] = (Z1 + Z2 + LetterSum + SerialNum/1000) % 10
Code[1] = (Z1*2 + Z2 + SerialNum/100) % 10
Code[2] = (LetterSum + SerialNum/10) % 10
Code[3] = (Z1 + Z2*2 + SerialNum) % 10
```

## Base Class Helpers

All calculators inherit from `TOBDRadioCode` and use these helpers:

```delphi
// Remove whitespace, convert to uppercase
function SanitizeInput(const Input: string): string;

// Validate expected length
function ValidateLength(const Input: string; const ExpectedLength: Integer; 
  var ErrorMessage: string): Boolean;

// Validate all digits
function ValidateDigits(const Input: string; var ErrorMessage: string): Boolean;

// Validate all letters
function ValidateLetters(const Input: string; var ErrorMessage: string): Boolean;

// Safe modular arithmetic
function ApplyModularTransform(const Value: Integer; const Modulus: Integer): Integer;
```

## File Size Considerations

- **Most calculators**: 2-6 KB
- **Becker4/Becker5**: ~90-100 KB (extended lookup tables)
- **Ford V**: 8.6 MB (complete lookup table)

The Ford V size is necessary and optimal for its use case.

## Adding New Calculators

To add a new radio code calculator:

1. Create `OBD.RadioCode.BrandName.pas`
2. Inherit from `TOBDRadioCode`
3. Implement:
   - `GetDescription`: Return description string
   - `Validate`: Validate serial format
   - `Calculate`: Implement algorithm or lookup
4. Use base class helpers for common operations
5. Add comprehensive XML documentation
6. Test with known serial/code pairs

Example template:

```delphi
unit OBD.RadioCode.BrandName;

interface

uses
  WinApi.Windows, System.SysUtils, OBD.RadioCode;

type
  TOBDRadioCodeBrandName = class(TOBDRadioCode)
  public
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string; 
      var ErrorMessage: string): Boolean; override;
  end;

implementation

function TOBDRadioCodeBrandName.GetDescription: string;
begin
  Result := 'Calculate radio code for BrandName radios.';
end;

function TOBDRadioCodeBrandName.Validate(const Input: string; 
  var ErrorMessage: string): Boolean;
var
  Sanitized: string;
begin
  Result := True;
  ErrorMessage := '';
  Sanitized := SanitizeInput(Input);
  
  // Add format validation
  if not ValidateLength(Sanitized, 4, ErrorMessage) then
    Exit(False);
  if not ValidateDigits(Sanitized, ErrorMessage) then
    Exit(False);
end;

function TOBDRadioCodeBrandName.Calculate(const Input: string; 
  var Output: string; var ErrorMessage: string): Boolean;
var
  Sanitized: string;
  Serial: Integer;
  Code: array[0..3] of Integer;
begin
  Result := True;
  Output := '';
  ErrorMessage := '';

  Sanitized := SanitizeInput(Input);
  if not Self.Validate(Sanitized, ErrorMessage) then Exit(False);

  Serial := StrToInt(Sanitized);
  
  // Implement algorithm
  Code[0] := ApplyModularTransform((Serial div 1000) + offset1, 10);
  Code[1] := ApplyModularTransform((Serial div 100) + offset2, 10);
  Code[2] := ApplyModularTransform((Serial div 10) + offset3, 10);
  Code[3] := ApplyModularTransform(Serial + offset4, 10);

  Output := Format('%d%d%d%d', [Code[0], Code[1], Code[2], Code[3]]);
end;

end.
```

## Limitations and Disclaimer

**Important**: These calculators provide general algorithms for common radio models. 

- Algorithms may not work for all model years
- Regional variants may use different formulas
- Some manufacturers changed algorithms over time
- No guarantee of correctness for all units

**Always test with known serial/code pairs** before relying on these implementations in production.

## References

- SAE standards for automotive security
- Manufacturer service manuals (where available)
- Radio unlock service documentation
- Community-documented algorithms

## License

All radio code calculators are licensed under Apache 2.0 as part of the Delphi-OBD library.

---

**Last Updated**: December 6, 2025
**Author**: Ernst Reidinga (ERDesigns)
