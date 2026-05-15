# 13-VWRadioSAFE

Demonstrates `TOBDVWRadioSAFE` — VW Group radio SAFE-code
recovery component. Wires the `OnReadEEPROM` event with a
synthetic callback that mimics what a real Premium IV radio
returns for its 4 ASCII SAFE digits at EEPROM offset 0x0083,
then asks the component to decode them.

In production `OnReadEEPROM` is the host's KWP1281 transport
callback (Serial bit-bang, ELM327, TP2.0, ISO-TP, or J2534).
The component owns the per-variant offset map (Gamma 5,
Premium IV/V, Rhapsody) and decode rules; the host owns the
bus-level transport.

## Build & run

```cmd
dcc32 -B VWRadioSAFE.dpr
VWRadioSAFE
```

## Expected output

```
TOBDVWRadioSAFE demo (synthetic OnReadEEPROM)
------------------------------------------------------------
  [OnReadEEPROM addr=$0083 len=4]
Decoded:
  variant : Premium IV
  code    : 1234
  raw     : 31 32 33 34
------------------------------------------------------------
Done.
```

## With a real KWP1281 codec

Set `Codec` (or `Transport`) instead of `OnReadEEPROM`:

```pascal
SAFE.Transport := TKWP1281SerialTransport.Create(MySerialSettings);
SAFE.RadioAddress := $56;
SAFE.RadioVariant := svPremiumIV;
R := SAFE.Extract;     // SAFE drives Connect → ReadEEPROM → Disconnect
```

Or hand it a pre-connected codec to share a session with
other diag operations:

```pascal
SAFE.Codec := MyKWP1281Codec;  // host owns Connect/Disconnect
R := SAFE.Extract;             // just calls Codec.ReadEEPROM
```
