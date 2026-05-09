# OEM Demo

Console example for the OEM-specific UDS extension framework. Looks up
the right extension by VIN, prints its DID + RoutineControl catalog, and
optionally decodes a DID payload.

## Usage

```
OEMDemo <vin>                                     # list the catalog
OEMDemo <vin> decode <did-hex> <payload-hex>      # decode one payload
```

## Examples

```
$ OEMDemo WVWZZZ1KZAW123456
Manufacturer: Volkswagen Audi Group (VAG)
Catalogued DIDs:
  0xF186  active_diagnostic_session         Currently active UDS session
  0xF187  spare_part_number                 VAG hardware part number
  ...

$ OEMDemo WVWZZZ1KZAW123456 decode F405 30 39
battery_voltage = 12.345 V

$ OEMDemo WBA3C1C50DF136345 decode D050 0001E240
mileage = 123456 km
```

## Adding a new manufacturer

1. Subclass `TOBDOEMExtensionBase` (`src/Services/OBD.OEM.pas`).
2. Implement `ManufacturerKey`, `DisplayName`, `ApplicableToVIN`, and
   `BuildCatalog`. Specialise `DecodeDID` for any DIDs that need unit
   conversion.
3. Self-register in the unit's `initialization` section:
   ```pascal
   initialization
     TOBDOEMRegistry.RegisterExtension(TOBDOEMExtensionMyOEM.Create);
   ```
4. Drop the unit into your project's uses clause; the registry picks it
   up at startup.

See `src/Services/OBD.OEM.VW.pas` and `src/Services/OBD.OEM.BMW.pas` for
the canonical pattern.
