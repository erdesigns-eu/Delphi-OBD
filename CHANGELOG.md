# Changelog

All notable changes are documented per major version. The full history
was previously a single 308 KB file; it has been split for browsability.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Per-version files

- **[CHANGELOG/v3.md](CHANGELOG/v3.md)** — v3.0.0 → v3.79.0 (current).
  All FMX bindings, the OEM extension framework, JSON catalog format,
  79 OEM catalogs, async UDS, cross-platform DoIP, DoIP TLS, coverage
  harness.
- **[CHANGELOG/v2.md](CHANGELOG/v2.md)** — v2.1.0 → v2.5.0. Test +
  CI foundation, component library, async + logging, distribution +
  docs, hardening + ECU flashing.

## Latest release at a glance

**v3.79.0** (2026-05-08) — Async UDS + cross-platform DoIP + TLS +
tooling.

- `OBD.OEM.UdsClient.Async` — future-returning UDS facade with
  cooperative cancellation.
- `OBD.Protocol.DoIP.Session.Cross` — TCP DoIP on `System.Net.Socket`
  for all platforms.
- `OBD.Protocol.DoIP.Session.TLS` — ISO 13400-3 §7 TLS 1.2+ via Indy +
  OpenSSL with mutual TLS support.
- `TCaptureReplayTransport` — deterministic `.obdlog` round-trip.
- `examples/catalogbrowser` — VCL catalog explorer.
- `tools/coverage/` — `delphi-code-coverage` harness.

See [CHANGELOG/v3.md](CHANGELOG/v3.md) for the full v3.79 entry.
