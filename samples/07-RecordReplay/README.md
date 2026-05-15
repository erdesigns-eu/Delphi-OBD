# 07-RecordReplay

Records three synthetic protocol events with `TOBDRecorder` to a
JSONL `.obdlog` file, then replays the file through `TOBDReplayer`.
No bus / hardware needed.

Demonstrates the Recorder/Replayer pair.

## Build

```
dcc32 -B RecordReplay.dpr
```

## Run

```
RecordReplay
```

Expected output (truncated):

```
07-RecordReplay
================
Opened recorder → C:\Users\<you>\AppData\Local\Temp\sample07.obdlog
Wrote 3 entries.

Replaying:
  [12:34:56.789] kind=leInfo     msg=session start
  [12:34:56.790] kind=leResponse sid=0x62 raw=F19031484743 4D
  [12:34:56.791] kind=leNRC      sid=0x22 nrc=0x33 (security access denied)

Done. Log file kept at C:\Users\<you>\AppData\Local\Temp\sample07.obdlog
```

The recorder also subscribes to a bound `TOBDProtocol`'s
`OnFrame` / `OnResponse` / `OnNRC` / `OnError` events when one is
assigned — this sample bypasses the protocol layer to keep the
demo bus-free.
