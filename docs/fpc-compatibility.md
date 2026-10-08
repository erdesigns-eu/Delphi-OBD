# ERD units and compiler targets

All library, UI, design-time and test units now use `ERD.*`. Update application
`uses` clauses from the former `OBD` prefix to `ERD`, update explicit source-file
paths, remove old DCUs/BPLs/DCPs and rebuild both packages. Components retain their
`TOBD*` class names, so their DFM class identities remain compatible. Package names
remain `DelphiOBD_RT` and `DelphiOBD_DT`.

| Target | Supported scope | Validation |
|---|---|---|
| Delphi | Nonvisual library, VCL/FMX UI and IDE integration | Static analysis; actual RAD Studio builds and DUnitX execution remain deferred |
| FPC 3.2.2 | Portable binary codecs, routes, frame parsers and EV request construction | Eleven original units; 185 executable checks |
| FPC 3.3.1+ on Linux x86-64 | Nonvisual library, managed futures, catalogs, coding/flashing services, TCP/UDP/mock transports, recorder/replayer and native crypto integrations | 272 Linux nonvisual source units compile; 38 executable runtime checks |

FPC 3.3.1 is a development compiler. The full runtime needs managed anonymous
functions and the official `vcl-compat` package's **nonvisual** `System.JSON`,
`System.IOUtils`, `System.Diagnostics`, `System.NetEncoding` and regex units.
We pin the official FPCSource revision
`2933f5ca60d86087451a1f8d5c726bc1c7ed5dff` for reproducibility.
The core units conditionally import `SysUtils`, `Classes`, etc. on FPC and their
`System.*` equivalents on Delphi. Every Pascal unit declares Delphi mode on FPC.

FPC does not build the visual controls or IDE package. Built-in Bluetooth/BLE
providers depend on the Delphi Bluetooth framework; serial, FTDI and J2534
backends depend on Windows. These eight platform-specific units are explicitly
outside the Linux build target. Selecting the built-in Bluetooth provider in
FPC raises a descriptive unsupported-backend exception. Hosts can supply their
own `IOBDConnectionTransport`. Windows FPC and Delphi POSIX builds have not been
verified by this Linux environment. Use `cthreads` first and include `cwstring` for Unicode/codepage conversions in Linux FPC programs
that use the threaded library; pump `CheckSynchronize` when using main-thread
callbacks in a console host.

## Reproduce

Install FPC 3.2.2, make, binutils, GCC and OpenSSL 3 runtime libraries, then:

```sh
python3 tools/fpc_smoke.py
bash tools/setup_fpc_runtime.sh /tmp/erd-fpc-source
python3 tools/fpc_runtime.py --source-tree /tmp/erd-fpc-source
python3 -m unittest discover -s tools/pascalcheck -p test_checkers.py
python3 tools/pascalcheck/run.py -v
python3 -m unittest discover -s tools -p test_validate_catalogs.py
python3 tools/validate_catalogs.py --require-coverage
```

For extracted compilers, pass `--compiler` to the Python scripts, or set
`FPC_BOOTSTRAP_COMPILER` for the build script. Installed modern FPC packages can
be selected with `--compiler` and `--rtl units/<target>`. The runtime validation
compiles repository files directly, uses genuine FPC libraries, fails for any
unexpected compilation error and runs linked code. Its local TCP peer keeps a
connection open after a three-byte response, verifying that the actual Wi-Fi
transport delivers short responses without waiting for a full receive buffer.
A local UDP peer checks binary datagrams through the actual UDP transport.
Other regressions cover JSON Unicode/types/defaults, cross-thread futures and
cancellation, queue capacity/timeouts/shutdown, gzip recording and binary replay,
a SHA-256 known vector and OpenSSL loading/default verification policy.
These tests do not establish hardware interoperability or TLS handshakes.

## Runtime repairs

The compiler caught invalid type names, missing direct imports, ambiguous enum
members, invalid event arguments, generic-constructor parsing, inaccessible
same-unit worker fields and captured function-result variables. Those are fixed
in the shared sources. TCP/UDP readers start after their state and callbacks have
been initialized. The portable bounded queue wakes blocked callers on shutdown;
its owner must stop/join users before freeing it.

The J1939 DM23–DM27 and DM31 PGNs were corrected against
[Equipment and Tool Institute's public J1939-84 packets](https://github.com/Equipment-and-Tool-Institute/j1939-84/tree/master/src/org/etools/j1939_84/bus/j1939/packets).
DM27 is `0xFD82`, DM28 is `0xFD80`; both now route separately. The unverified
`J1939_PGN_DM32` alias of DM31 has been removed rather than advertising an
incorrect standard identifier. Only the listed diagnostic messages are claimed.

## Local validation, 2026-10-08

- 107 applicable static checkers: no findings, including hints.
- Nine analyzer regression tests and twelve catalog-auditor regression tests pass.
- All 248 catalogs have schema coverage and zero violations.
- Every unit declaration matches its filename; source, tests, samples and packages
  have no old unit-namespace references.
- FPC compiles and executes original repository sources. Delphi builds, DUnitX,
  hardware sessions and GitHub Actions execution are not established by this run.
