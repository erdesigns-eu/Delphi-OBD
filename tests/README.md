# Delphi-OBD Tests

DUnitX test harness for the Delphi-OBD library.

## Layout

```
tests/
├── Tests.dpr          # console / TestInsight runner
├── Tests.Smoke.pas    # trivial canary fixture (delete once real tests exist)
└── README.md
```

Test units follow the naming convention `Tests.<Subsystem>.pas`, e.g.
`Tests.VIN.Decoder.pas`, `Tests.RadioCode.Audi.pas`,
`Tests.Protocol.IsoTp.pas`. Fixture classes are `T<Subsystem>Tests`.

## Prerequisites

* Embarcadero Delphi 11 or 12.
* **DUnitX** — bundled with Delphi (`Apps/DUnitX/`) since 10.4. Add the
  DUnitX source path to **Tools → Options → Library → Library path** if it
  isn't already on the path:
  ```
  $(BDS)\source\DUnitX
  ```
* **TestInsight** *(recommended for IDE runs)* —
  <https://bitbucket.org/sglienke/testinsight/>. Install the design-time
  package, then **View → TestInsight Runner**.

## Running tests

### From the IDE (TestInsight)

1. Open `tests/Tests.dpr`.
2. Define the conditional `TESTINSIGHT` for the project (Project Options →
   Building → Delphi Compiler → Conditional defines).
3. Build, then **TestInsight Runner** auto-discovers and runs the suite.

### From the IDE (console)

1. Open `tests/Tests.dpr`.
2. Build and run (F9). A console window prints results.

### From CI / command line

```cmd
Tests.exe
```

NUnit-format XML is written to `TestResults.xml` next to the executable.
The runner exits with code `0` on success and `1` on any test failure
(`2` on harness-level exception).

Useful flags (parsed by `TDUnitX.CheckCommandLine`):

| Flag                              | Purpose                                       |
|-----------------------------------|-----------------------------------------------|
| `/exitbehavior:Pause`             | Pause the console on red so failures readable |
| `/run:Fixture.TestName`           | Run a single test                             |
| `/include:Fixture*` `/exclude:…`  | Filter by fixture / category                  |

## Adding a new fixture

1. Create `tests/Tests.<Subsystem>.pas` with one fixture class.
2. Add it to the `uses` clause in `Tests.dpr`.
3. Register the fixture in the unit's `initialization` section:
   ```pascal
   initialization
     TDUnitX.RegisterTestFixture(TMyFixture);
   ```
4. Run locally before pushing — CI gates merges on the suite.

See [`docs/ROADMAP.md`](../docs/ROADMAP.md) for the v2.1 testing milestone
(VIN goldens, radio code goldens, Service 01–0A, ISO-TP, adapter parsers).
