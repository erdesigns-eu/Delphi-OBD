# pascalcheck

Static analysis for this repository's Delphi sources, written in Python so it
runs anywhere — a machine without Delphi installed, a CI step, a container.

It is not a compiler and does not pretend to be one. It is a set of small,
single-purpose readers that each know one mistake well enough to find it in
source the compiler has not seen yet: a field declared after a method, a
translation catalog that has drifted, an anonymous method holding a `const`
parameter it will outlive. Where a checker cannot tell — a class whose
ancestors live in the VCL, a `with` block whose target it cannot resolve — it
says nothing rather than guessing. A quiet run means nothing was found, not
that nothing is wrong.

## Running it

    python tools/pascalcheck/run.py             # one line per checker
    python tools/pascalcheck/run.py -v          # and the findings themselves
    python tools/pascalcheck/run.py capture        # only checkers matching a name

`run.py` exits non-zero when anything is found, so it can stand in a build
step. Individual checkers can also be run directly:

    python tools/pascalcheck/check_capture.py

Python 3.8 or newer, no third-party packages. The repository root is worked
out from the file's own location, so the suite runs from any directory; set
`REPO` to point it at a different checkout.

## Delphi-OBD integration

Discovery covers namespaced units under `src/`, `samples/`, `packages/`,
`tests/` and `tools/`. Conditional FPC/Delphi imports are recognized.
No source files are rewritten. The default CI run is strict, including
hint/warning checkers. `--errors-only` is an optional local mode that reports
private/unused/hidden/inlineunit hints without making those hints fail the run.
A crash or unreadable checker result always fails.

Copied media-app translation, branding, HTTP-factory, timezone, teletext,
release-builder and custom-form policy checks have been removed. The retained
platform checker now verifies this repo's three tracked Delphi projects.
There are no missing-input skips or claims of media-app validation.

See [the tool inventory](../README.md) for actual FPC execution and catalogue
validation. Static analysis is heuristic and does not replace Delphi/FPC
compilation or hardware tests.

## What each checker looks for

| checker | finds |
| --- | --- |
| `afterevent` | a cache entry touched again after the event that may have freed it |
| `args` | a call with an argument count no declaration accepts |
| `arraylow` | a loop from nought over an array whose first element is not nought |
| `assign` | an assignment to an identifier with no visible declaration |
| `assigned` | `Assigned(F)` where F is a function, not a variable (E2036) |
| `blocks` | block structure that does not balance inside a routine body |
| `bom` | a byte-order mark an edit added or dropped |
| `caption` | a string helper called on a control's `Text` or `Caption` (E2018) |
| `ctordefault` | a published `default` the constructor does not actually produce |
| `capture` | an anonymous method closing over a `const` parameter |
| `isscode` | a statement left between routines in an installer script's `[Code]`, which Inno Setup refuses with 'BEGIN' expected at the very end of a release build |
| `nestedcapture` | an anonymous method calling a routine nested in the routine around it (E2555) |
| `loopvar` | a for loop inside an anonymous method driven by a variable the closure did not declare itself (E1019) |
| `loopcapture` | an anonymous method made inside a loop and kept, reading a variable the loop moves on: closures share the frame around them, so every one of them ends up on the loop's last value |
| `needsunit` | a Windows API name used without the unit that declares it, and Winsock 2 called the way the C header reads rather than the way Delphi declares it (E2003) |
| `intrinsic` | an intrinsic such as `Default(` called inside a type that has a member of that name, which the member then answers (E2034) |
| `vclmember` | a unit-level const, var or routine whose name a VCL ancestor already declares as a member, read unqualified from a method of that class: the member wins, so a const `Padding` reaches `TWinControl.Padding` (E2010, E2015) |
| `reach` | a name used where nothing in scope declares it: the unit it comes from is not in a uses clause, or the name sits in that unit's implementation section (E2003, and the E2250 cascade around the call) |
| `jsoncast` | a JSON value cast with `as` to a shape nothing checked it has: a server that answers with an array where an object belongs raises EInvalidCast in the middle of the parse |
| `sections` | a type declaration outside a `type` section (after a routine body, or in a uses/var/const section), and a section keyword with nothing under it |
| `platforms` | missing Delphi projects, incorrect default/declared Windows target platforms |
| `searchpath` | a uses name or an `$I` a project's unit search path cannot reach, while the file is in this repository: it compiles from the IDE and fails the first time that project is built (F2613, F1026) |
| `bracecomment` | a `{ }` comment holding another `{`, which ends it at the first `}` and leaves the rest as code |
| `reservedname` | `private`, `public`, `published` and their kin used as a field or parameter name; any reserved word (`is`, `in`, `type`...) as a routine, parameter, variable or field name |
| `declorder` | A class declared before the class it descends from in the same unit (E2003 on the parent) |
| `caselabel` | a case statement listing the same label twice (E2030) |
| `anonevent` | an anonymous method assigned to an event declared `of object` (E2010) |
| `varargtype` | a `var`/`out` argument whose declared type is not the parameter's (E2033) |
| `dupname` | a component named twice in one `.dfm`, or a unit in both uses clauses (E2004) |
| `default` | indexing a type with no default array property (E2149) |
| `dfm` | an object or handler in a .dfm that its form class does not declare |
| `dfmblob` | a binary property in a form file that is unclosed or undecodable |
| `dfmstruct` | a DFM that does not close everything it opened |
| `dispatch` | an action wired to a shared handler that never asks about it |
| `dup` | the same member declared or implemented twice in one place |
| `eol` | mixed line endings, or the wrong ending for the file's kind |
| `eventsig` | a handler whose parameters do not match the event it is assigned to |
| `exceptstore` | an exception object kept past the handler that owns it |
| `exitref` | `Exit(X)` where X is a method reference, which reads as a call (E2035) |
| `fieldorder` | a field declared after a method in the same section (E2169) |
| `formatflag` | a format string carrying a flag Delphi's Format does not have |
| `fields` | an `F`-prefixed name that is not a field of the enclosing class |
| `final` | a `finalization` section with no `initialization` |
| `forin`, `forin2` | a `for-in` variable whose type the container cannot yield |
| `formref` | a form global reached for a member it does not declare (E2003) |
| `forvar` | an assignment to a `for` loop's control variable (E2081) |
| `fwddecl` | a routine used before it is defined, with no forward declaration |
| `hidden` | a method hiding a virtual one it inherits (W1010) |
| `iface` | a class listing an interface it does not fully implement (E2291) |
| `ifaceuses` | a type in a unit's interface its own uses clause cannot reach |
| `ifthen` | `IfThen` on strings with only `System.Math` in scope (E2250) |
| `impl` | a declaration with no body, or a body with no declaration |
| `impluses` | a type named in a unit that no uses clause of it can reach (E2003) |
| `index` | a zero-based `IndexOf` result used as a one-based position |
| `leak` | an interfaced object built inline and never given a reference |
| `lineends` | a source file with a bare LF, a bare CR or a doubled CR where Delphi writes CRLF |
| `logurl` | an address written to the log without `MaskUrl`, so a provider's name and password go in a file people send on |
| `madelater` | a local interface whose first mention is a method called on it, made further down the routine |
| `members` | a member that does not exist on a fully-known project type |
| `private` | a private member never used, and a virtual redeclared without `override` |
| `promote` | a promoted property no ancestor declares (E2147) |
| `queuedfield` | a `TThread.Queue` closure reading a field its caller was handed as a parameter: two calls in one turn of the message pump deliver the second value twice and the first not at all |
| `propfield` | a property reading or writing through a field that is a class |
| `rtl` | a `System.IOUtils.TPath` member the RTL does not have |
| `rtlshadow` | a unit-level routine shadowing an RTL one it can reach |
| `shadow` | a field hiding an enumeration value the same unit reads |
| `gdipshadow` | a variable declared `UINT32`, `UINT16` or `INT16` in a unit that uses `Winapi.GDIPAPI`, whose own distinct types of those names shadow the RTL's, and handed to a var or out parameter of another unit that wants the RTL's (E2033) |
| `typeshadow` | a type two used units both declare, taken unqualified (E2003) |
| `unused` | a local declared but never used (H2164) |
| `usesplace` | a uses clause the compiler does not expect there |
| `gpcolor` | a TColor poured into a GDI+ ARGB value without swapping red and blue |
| `gpclip` | a GDI+ clip set without the clip of the painting it is inside, so paint with alpha in it can take a second coat |
| `methodref` | a method passed where a TFunc/TProc method reference is wanted (E2010) |
| `override` | an override declared below the visibility of the method it overrides (H2269) |
| `package` | a unit a package compiles in that its contains clause does not name (W1033), a contains entry whose file is not there, and one the .dproj has no reference for |
| `varparam` | a bare method name given to `Assigned` or a function result given to `Inc`, `FreeAndNil` and their kin, which want something with an address (E2036) |
| `vcltypes` | a VCL or RTL type no uses clause of the unit provides (E2003) |
| `visibility` | a private or strict-private member reached from another unit (E2361) |
| `shortcut` | a menu item overriding its action's shortcut, and two actions in one category claiming the same keystroke |
| `saferename` | a file deleted before a rename that does not replace it, so a rename that fails leaves neither |
| `waitfree` | waiting on a thread that frees itself |
| `worker` | waiting on the first worker without a count guard |

## How it is put together

The checkers sit on a small shared layer, and a change there is felt by all of
them:

- `paslex.py` — blanks comments, strings and directives so offsets still line
  up, then tokenizes what is left.
- `symbols.py`, `pstruct.py` — units, their types, members and implementations.
- `bodies.py` — routine bodies with their scope: parameters, locals, which of
  those are `const`, and the ranges belonging to nested routines and closures
  rather than to the routine around them.
- `typemap.py`, `resolve.py`, `params.py` — resolving a name to a type, and a
  container to what it yields.
- `common.py` — where the repository is, which files to read, which to skip.

## Adding a checker

Name the file `check_<something>.py`, give it a docstring saying what it finds
and why that is worth finding, print a heading, print the findings, and end
with a line of the form `total: N`. `run.py` picks it up with no registration:
it reads that total to decide whether the run was clean.

Two habits worth keeping:

- **Prove it both ways.** Plant the defect the checker is meant to find,
  confirm it is reported, remove it, confirm the run is clean again. A checker
  that has only ever been seen printing zero has not been tested.
- **Silence beats a guess.** Every false positive costs more attention than
  the finding was worth, and a suite that cries wolf stops being read.
