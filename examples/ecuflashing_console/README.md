# ECU Flashing Console

End-to-end demonstration of `TOBDECUFlashing` (v2.5) with the
Windows BCrypt verifier (v3.2) running against a simulated ECU.
Console-only so the pipeline reads top-to-bottom without a form.

## Usage

```
FlashConsole <firmware-bin> <signature-bin> <pubkey-der> [<backup-path>]
```

- `firmware-bin` — raw firmware image to flash.
- `signature-bin` — RSA-PKCS1-SHA256 signature produced with
  `openssl dgst -sha256 -sign priv.pem -out sig.bin firmware.bin`.
- `pubkey-der` — DER-encoded SubjectPublicKeyInfo:
  `openssl rsa -in priv.pem -pubout -outform DER -out pub.der`.
- `backup-path` *(optional)* — where to persist the pre-flash
  snapshot.

ECDSA-P256 signatures and DER public keys work too; the verifier
auto-detects the algorithm.

## What it shows

- Loading firmware + signature + public key from disk
- Constructing `TOBDBCryptVerifier` (no external DLLs)
- Wiring the flashing pipeline:
  - `OnHealthCheck` — pretend battery / ignition / comms are good
  - `OnSnapshot` — read the simulated ECU memory
  - `OnWriteChunk` — append blocks to simulated memory
  - `OnFinalise` — pretend RequestTransferExit + checksum pass
  - `OnVerifyEcu` — pretend post-flash readback matches
- Stage transitions, progress, completion and failure callbacks
- `BackupPath` snapshot persistence

Replace the four simulated callbacks with real OEM-specific UDS
sequences (RequestUpload `35`, TransferData `36`, RoutineControl
`31`) and the same surrounding pipeline drives a live flash.

## Generating test fixtures

```
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out priv.pem
openssl rsa -in priv.pem -pubout -outform DER -out pub.der
echo -n 'firmware contents' > fw.bin
openssl dgst -sha256 -sign priv.pem -out sig.bin fw.bin
FlashConsole fw.bin sig.bin pub.der
```

## See also

- `src/Services/OBD.ECU.Flashing.pas` — pipeline coordinator
- `src/Services/OBD.ECU.Signature.BCrypt.pas` — BCrypt verifier
- `src/Services/OBD.ECU.Signature.OpenSSL.pas` — alternative
  using dynamically-loaded OpenSSL
- `src/Services/OBD.ECU.Signature.HSM.pas` — HSM hook contract
- `examples/ecuflashing/` — original form-based example
