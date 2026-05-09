# Subsystem: Forms

`src/Forms/` contains the base form scaffolding that every OBD form
inherits from. There's only one unit, but the conventions matter
because the IDE wizards (see [SUBSYSTEM_WIZARDS.md](SUBSYSTEM_WIZARDS.md))
generate forms against this base.

## Files

| Unit | Purpose |
|---|---|
| `OBD.Form.pas` (+ `.dfm`) | `TOBDForm` — base VCL form. Owns application-settings access, fires `OnWindowStateChange`, and triggers a repaint of every touch control on the form when the window state changes (so headers / subheaders / status bars stay clean across minimize/maximize). |

## What `TOBDForm` gives you

- **`OnWindowStateChange: TWindowStateEvent`** — callback whenever
  `WindowState` transitions. The base implementation walks the touch
  controls and forces a redraw, eliminating a class of stale-buffer
  bugs.
- **Application settings access** — convenience proxy onto
  `OBD.Application.Settings` so per-form preferences (window position,
  user choices) persist via the standard settings store.
- **Touch-control repaint** — built-in collection of
  `TOBDTouchHeader` / `TOBDTouchSubheader` / `TOBDTouchStatusbar`
  references discovered at runtime; you don't have to wire repaint
  manually.

## Pattern: writing a new form

Always inherit from `TOBDForm`, not `TForm`:

```pascal
type
  TMyDiagForm = class(TOBDForm)
    // ...
  end;
```

The IDE wizards do this for you (`OBD.Form.Wizard.pas` /
`OBD.Mainform.Wizard.pas`). For ad-hoc forms added by hand, change the
inheritance line and the form will pick up the touch-control repaint
behaviour automatically.

For non-form scaffolding (shared connection / protocol components), use
the data-module wizard instead — see
[SUBSYSTEM_WIZARDS.md](SUBSYSTEM_WIZARDS.md).
