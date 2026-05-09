# Replay

Console replayer for `.obdlog` files captured by `TOBDRecorder`. Useful for
test fixtures (replay a real adapter session against a parser) and for
debugging without a vehicle attached.

## Usage

```
Replay <path-to-.obdlog> [speed]
```

- `speed` defaults to `1.0` (real-time replay). Pass `0` to fire all
  entries back to back (useful in tests / CI).

## Recording

```pascal
Recorder := TOBDRecorder.Create;
Recorder.Start;
// hook your connection's send/receive paths to RecordSent / RecordReceived
// run the session...
Recorder.SaveToFile('session.obdlog');
```

## File format

Newline-delimited text:

```
# obdlog v1
0       S       AT Z
12      R       ELM327 v1.5
410     S       01 0C
427     R       41 0C 1A F8
```

Each line is `<elapsed_ms>\t<direction>\t<escaped_text>`. Direction is
one of `S` (sent), `R` (received), `I` (info), or `E` (error). Tabs and
newlines inside the text are escaped as `\t` / `\r` / `\n`; backslashes
as `\\`.

## See also

- `src/Services/OBD.Service.Recorder.pas` — the recorder/replayer pair.
- [`docs/ROADMAP.md`](../../docs/ROADMAP.md) — v2.3 Async & Logging
  milestone.
