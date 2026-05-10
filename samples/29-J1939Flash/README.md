# 29-J1939Flash

> ⚠️ **HEAVY-DUTY FLASH SAMPLE.** Bricking an HDV ECU costs more
> and is harder to recover than a passenger-car one. Read
> `docs/flashing-safety.md` first.

Walks the SAE J1939-73 memory-access trio (DM14 / DM15 / DM16):

1. Build a DM14 *RequestForMemoryAccess* with a write command +
   target address + length
2. Wait for the matching DM15 *MemoryAccessResponse* (status =
   PROCEED, otherwise abort)
3. Send the binary image as DM16 *BinaryDataTransfer* chunks
   over the J1939 transport
4. Wait for the final DM15 with status = OPERATION_COMPLETE
5. Send DM14 with command = OPERATION_COMPLETED to leave the
   memory-access mode

## Build

```
dcc32 -B J1939Flash.dpr
```

## Run

```
J1939Flash <hd-ecu-source-address> <target-address> <image.bin>
```

Like sample 28, the heavy-duty flash sample is a **template** —
the host wires:

- The J1939 transport (raw CAN driver)
- The session manager (`TOBDJ1939SessionManager`)
- The OEM-specific seed → key for DM14 security
- The pre-flash routine (vendor-specific)

## Notes

- Heavy-duty ECUs commonly require an external charger (truck
  electrical loads dwarf passenger-car ones).
- DM14 length field is 15 bits — split larger images into
  multiple memory-access transactions.
- Some heavy-duty ECUs reject a flash if the engine has been
  off for less than a vendor-specific cool-down time.
