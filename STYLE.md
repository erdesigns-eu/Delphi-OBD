# Delphi-OBD Code Style Guide

This document defines the code style for every Pascal unit in the
package. The principle is **code-as-documentation**: a developer should be
able to navigate the source and understand the system without reading any
external documentation.

External markdown (README, PLAN, CONTRIBUTING, this file, safety docs) covers
disclaimers, quick-starts, the architectural plan, and policy. Everything
about behaviour, units, parameters, ranges, side-effects, lifetimes, and
threading lives in the source.

If a future contributor asks "where is X documented?", the answer should
always be "in the source above the symbol that does X."

---

## 1. File header

Every `.pas` file begins with the header below. The template lives at
[`src/HEADER.template.pas`](src/HEADER.template.pas). Fields in `<…>` are
mandatory; copy the template and fill them in.

```pascal
//------------------------------------------------------------------------------
//  <Unit name>
//
//  <One-line summary of what the unit does.>
//
//  <Optional multi-line description: scope, important behaviours, references
//   to specifications (e.g. "Implements ISO 15765-2 Network Layer Services").>
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Co-authors  : <Name>, <Name>
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - <Spec / standard / source URL or DOI>
//    - <…>
//
//  History     :
//    yyyy-mm-dd  <Initial>  <Short note>
//    yyyy-mm-dd  <Initial>  <Short note>
//
//  Future work :
//    - <Planned addition or known gap>
//------------------------------------------------------------------------------

unit OBD.<Layer>.<Detail>;
```

The `History` block is append-only; do not rewrite or rebase it. Use it to
track behavioural changes that future readers need to know about. Trivial
edits (typos, formatting) do not need a history entry.

The `Future work` block is for things explicitly known to be missing or
planned. Empty is fine; remove the block if you have nothing to add.

---

## 2. XMLDoc on every public symbol

Every public type, method, property, event type, constant, exception, and
record member gets XMLDoc immediately above its declaration in the
`interface` section.

### Rules

The mandatory minimum for every public symbol:

| Symbol | Required tags | Optional tags |
|---|---|---|
| Type (class, record, interface, enum) | `<summary>`, `<remarks>` (when non-trivial) | `<see cref="…">` |
| Enum member | `<summary>` | — |
| Constructor / destructor | `<summary>`, `<param>` for each parameter, `<exception>` for each raise | `<remarks>` |
| Procedure | `<summary>`, `<param>` for each parameter, `<exception>` for each raise | `<remarks>` |
| Function | `<summary>`, `<param>` for each parameter, `<returns>`, `<exception>` for each raise | `<remarks>` |
| Property | `<summary>`. `<remarks>` if it has side effects, threading, default value, units | — |
| Field (record member) | `<summary>` | — |
| Event property | `<summary>` describing **when** it fires and on **which thread** | — |

Specifics:

- **Always** document. There is no "obvious" exception. If it really is
  obvious, the doc takes one line.
- **Use the `///` triple-slash form.** Delphi's IDE and code-completion
  surface these directly.
- **First sentence** is a single line that ends with a period. The IDE
  shows just this in tooltips.
- **`<param>`** for every parameter. Even self-evident ones. The
  CI hygiene job rejects PRs whose public methods are missing parameter
  docs.
- **`<returns>`** for every function (not procedures).
- **`<remarks>`** for behavioural caveats: threading, blocking, side
  effects, ownership, exceptions raised, units of measure.
- **`<exception cref="EOBDxxx">`** for every exception the method can
  raise (programmer / configuration errors only — transient I/O fires
  `OnError`, not raises).
- **`<see cref="…">`** to point at related types/methods.
- **Implementation methods that satisfy an interface** still need
  XMLDoc on the **class**, not just the interface. The IDE shows the
  class doc when navigating from a concrete instance, the interface
  doc only when navigating from the interface reference.

### Example

```pascal
type
  /// <summary>
  ///   Represents a single OBD-II PID value as decoded from an ECU response.
  /// </summary>
  /// <remarks>
  ///   Owned by the component that produces it. Do not free; do not retain
  ///   beyond the scope of the event handler. Copy the fields you need.
  ///
  ///   <c>Scaled</c> uses the unit named in <c>UnitName</c>. Raw bytes are
  ///   preserved in <c>Raw</c> for callers that need to do their own
  ///   decoding.
  /// </remarks>
  TOBDValue = record
    /// <summary>The raw bytes returned by the ECU for this identifier.</summary>
    Raw: TBytes;
    /// <summary>Human-readable scaled value. Variant carries the natural type
    /// (Double for °C / %, Integer for counts, string for VIN, …).</summary>
    Scaled: Variant;
    /// <summary>Unit name in SI form where applicable (e.g. <c>°C</c>,
    /// <c>kPa</c>, <c>%</c>, <c>rpm</c>). Empty for unitless values.</summary>
    UnitName: string;
    /// <summary>Symbol name as defined in the catalogue
    /// (e.g. <c>'Engine RPM'</c>).</summary>
    Name: string;
    /// <summary>Local time the value was decoded.</summary>
    Timestamp: TDateTime;
  end;

  /// <summary>
  ///   Reads OBD-II Mode 01 PIDs from an ECU at a fixed interval.
  /// </summary>
  /// <remarks>
  ///   Bind <c>Adapter</c> to a configured <see cref="TOBDAdapter"/> and add
  ///   one or more PIDs to the <c>PIDs</c> collection before setting
  ///   <c>Active := True</c>. PIDs are batched into the smallest number of
  ///   request frames the wire protocol allows (up to six per frame on
  ///   ISO 15765).
  ///
  ///   All events fire on the main thread.
  /// </remarks>
  TOBDLiveData = class(TComponent)
  public
    /// <summary>Triggers a one-shot read of all configured PIDs without
    /// changing <c>Active</c>.</summary>
    /// <remarks>Blocks the caller until the read completes or the protocol
    /// times out. For UI threads prefer setting <c>Active := True</c>
    /// instead.</remarks>
    /// <exception cref="EOBDNotConnected">Adapter is not connected.</exception>
    /// <exception cref="EOBDConfig">No PIDs in the collection.</exception>
    procedure ReadOnce;
  end;
```

### Implementation comments

Inside the `implementation` section, keep comments to a minimum. The XMLDoc
in the `interface` section is the contract. Inline comments only for the
*why* of non-obvious code: a workaround, a hidden invariant, a reference to
a spec section.

Do not restate the function name or paraphrase what the code obviously
does.

---

## 3. Naming

| Kind | Convention | Example |
|---|---|---|
| Unit | `OBD.<Layer>.<Detail>` | `OBD.UDS.ReadDID` |
| Class | `T<Name>` | `TOBDLiveData` |
| Interface | `I<Name>` | `IOBDProtocolFactory` |
| Record | `T<Name>` | `TOBDValue` |
| Enum type | `TOBD<Name>` | `TOBDTransport` |
| Enum members | `<2-3 char prefix><Name>` | `otSerial`, `pidISO15765_4_CAN_11_500` |
| Field | `F<Name>` | `FAdapter` |
| Property | `<Name>` (no prefix) | `Connection`, `PIDs` |
| Event property | `On<Verb>` | `OnPIDValue`, `OnConfirmExecute` |
| Event type | `TOBD<Name>Event` | `TOBDPIDValueEvent` |
| Exception | `EOBD<Name>` | `EOBDNotConnected` |
| Constant | `OBD_<UPPER_SNAKE>` | `OBD_CAN_TP_MAX_LEN` |
| Method param | `A<Name>` only when shadowing a field; otherwise plain | `procedure Connect(const APort: string);` |
| Local | plain, descriptive | `var Frame: TOBDFrame;` |

Avoid abbreviations except those that are universal in the domain (`PID`,
`DID`, `DTC`, `NRC`, `VIN`, `ECU`, `CAN`, `OBD`, `UDS`, `KWP`, `XCP`, `CCP`,
`PGN`, `SPN`, `FMI`, `IPT`, `MID`, `TID`).

---

## 4. Formatting

- **Indent.** Two spaces. Never tabs.
- **`begin`/`end`** on their own lines.
- **`if … then begin`** on the same line when there is a `begin`. Do not
  put single-statement bodies on the same line as `if`.
- **One statement per line.**
- **Trailing semicolons** before `end` are optional but consistent —
  prefer omitting the final one in a block.
- **Blank line** between top-level routines and between `interface` /
  `implementation` blocks.
- **No trailing whitespace.** No mixed line endings (LF only).
- **Line length:** soft limit 100 columns, hard limit 120. Wrap parameters
  one per line when the call exceeds.

```pascal
// Right
function TOBDAdapter.SendCommand(const ACommand: string;
  ATimeout: Cardinal): TOBDFrame;
begin
  if not Connected then
    raise EOBDNotConnected.Create('Adapter is not connected');
  …
end;

// Wrong
function TOBDAdapter.SendCommand(const ACommand: string; ATimeout: Cardinal): TOBDFrame;
begin
  if not Connected then raise EOBDNotConnected.Create('Adapter is not connected');
  …
end;
```

---

## 5. Language features

### Allowed

- Generics, anonymous methods, parallel library, `TArray<T>`, `TList<T>`,
  attributes, RTTI, `TJSONObject`, `THashSHA2`, `TThread.Queue/Synchronize`.
- `class helper`, `record helper` for clearly bounded utility extensions.
- `for…in` loops.

### Avoided

- **Inline `var`.** Not available in 10.3 Rio. Use a top-of-routine `var`
  block.
- **Custom managed records** (e.g. record operators `Initialize`,
  `Finalize`). Not available in 10.3 Rio.
- **`with`.** Banned. It hides the source of identifiers.
- **Multiple-receiver assignments**, exotic syntax tricks. Code reads
  top-to-bottom.
- **`Application.ProcessMessages`** anywhere in the runtime package. Anywhere.
- **`Sleep`** in any tight loop. Use `TEvent.WaitFor` or signalling.

### Conditional compilation for newer features

Where a newer language feature would be a clear win, gate it:

```pascal
{$IFDEF VER350}  // Delphi 11 Alexandria and later
  // Use the new RTL helper
{$ELSE}
  // 10.3 / 10.4 fallback
{$ENDIF}
```

Do not pepper the codebase with these. The default code path must work on
10.3 Rio.

---

## 6. Threading and the sync + async dual-method rule

- **Every public method that can take more than a few milliseconds
  ships in two forms.** A synchronous `Foo` that blocks until done, and
  a non-blocking `FooAsync` that returns immediately and reports via
  events. See [`PLAN.md` §3.7](PLAN.md) for the full contract,
  cancellation rules, in-flight policy, and the table of methods this
  applies to.
- **All events fire on the main thread.** Use `TThread.Queue` (not
  `Synchronize`) from worker threads. There is no opt-out.
- **No `Application.ProcessMessages`.** Ever.
- **No busy-wait `Sleep` loops.** Use `TEvent.SetEvent` from the
  producer thread and `TEvent.WaitFor(timeout)` on the consumer.
- **Document threading** in `<remarks>` for every non-trivial method:
  whether it blocks, how long, what cancels it. For paired
  sync/async methods the documentation pattern is:

```pascal
/// <summary>Reads the VIN. Blocks until the response arrives or the
/// protocol times out.</summary>
/// <remarks>Use <see cref="ReadVINAsync"/> from GUI code.</remarks>
function ReadVIN: string;

/// <summary>Reads the VIN without blocking. The result is delivered
/// via <c>OnVIN</c> on the main thread; failures via <c>OnError</c>.
/// </summary>
/// <remarks>Only one ReadVINAsync may be in flight at a time. Calling
/// again raises EOBDConfig until the previous one settles.</remarks>
procedure ReadVINAsync;
```

- **One in-flight async op of the same kind per component.** Calling
  `FooAsync` while another is running raises `EOBDConfig`. The sync
  form is always callable (it just blocks).
- **Self-reaping workers.** Async workers must queue their own cleanup
  to the main thread on completion; the host component must cancel +
  join any in-flight worker in its destructor and in any
  lifecycle-cancelling method (`Close`, `Disconnect`, etc.).

---

## 7. Error handling

- **Typed exceptions** for programmer / configuration errors:
  `EOBDConfig`, `EOBDNotConnected`, `EOBDProtocol`, `EOBDUnsupported`,
  `EOBDInternal`. Always include enough context that the user can act on
  it without a debugger.
- **`OnError(Sender; ErrorCode; const Message; var Handled)`** for I/O
  glitches, NRCs, timeouts, NO-DATA. Never raises.
- **Document every raised exception** with `<exception cref="…">`.
- **Catch-and-rethrow** with `raise` (no value) to preserve the call stack.

---

## 8. Memory & ownership

- **Components own their sub-objects.** `TPersistent` for non-collection
  sub-settings. `TOwnedCollection` for lists.
- **`TComponent` ownership** for child components.
- **No reference counting (`IInterface`)** for components. Interfaces are
  reserved for true polymorphism and lightweight value types.
- **Document ownership** in `<remarks>`. Who frees this? Who can retain a
  reference? Is the value valid after the event returns?

---

## 9. UI framework boundary

- `src/Core`, `src/Connection`, `src/Adapter`, `src/Protocol`,
  `src/Services` — **no `Vcl.*`, no `FMX.*`**. CI fails the build if
  these strings appear.
- `src/DesignTime` is the only place VCL is allowed (the IDE itself is
  VCL).
- Demo / sample applications under `samples/` may use VCL or FMX as
  appropriate per sample.

---

## 10. Tests

- One `Tests.<unit>.pas` per source unit where possible.
- DUnitX `[TestFixture]` / `[Test]` attributes.
- Capture-driven for protocol-shaped inputs: store binary fixtures under
  `tests/fixtures/`, load them in `Setup`.
- One assertion per concept. Multiple assertions per test method are fine
  if they verify the same concept.
- Tests are public symbols too — XMLDoc the fixture class and any non-
  obvious `[Test]` method.

---

## 11. Catalogue files

JSON only. Schemas in `catalogs/_schema/`. Validated by `tools/validate-catalogs`
in CI.

- One logical entity per file (one Mode, one OEM, one DM family).
- Stable key order: ID first, then human name, then technical fields, then
  scaling / unit, then notes.
- UTF-8, LF line endings, two-space indent. The same `tools/lint`
  sanity-checks JSON.

---

## 12. Reviews

If a reviewer asks "where is the doc for this?" and the answer is "I'll
add it," the change is incomplete. Doc lives in the same PR as the code.

If reading the source from top to bottom does not tell the next contributor
what they need to know, the source is wrong, not the next contributor.
