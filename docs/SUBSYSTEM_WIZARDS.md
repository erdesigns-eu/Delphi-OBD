# Subsystem: IDE Wizards

`src/Wizards/` registers four design-time wizards in the Delphi IDE
under **File → New → Other → ERDesigns OBD**. They scaffold projects,
forms, and data modules pre-wired with the non-visual binding
components described in [QuickStart.md](../QuickStart.md).

## Files

| Unit | Wizard menu | What it generates |
|---|---|---|
| `OBD.Project.Wizard.pas` | "ERDesigns OBD Project" | Empty OBD project scaffold (.dpr + main form). |
| `OBD.Mainform.Wizard.pas` | "ERDesigns OBD Mainform" | Form with touch header, subheader, status bar, and one circular gauge — pre-wired to `TOBDConnectionComponent` (serial, COM1 @ 38400), `TOBDProtocolComponent` (auto-binding), `TOBDHeaderComponent`, `TOBDSubheaderComponent`, `TOBDGaugeComponent`. |
| `OBD.Form.Wizard.pas` | "ERDesigns OBD Form" | Secondary form with the same visual + non-visual scaffold as the main form, so it participates in the same binding pattern. |
| `OBD.DataModule.Wizard.pas` | "ERDesigns OBD DataModule" | Data module hosting shared connection + protocol components for reuse across forms. |

## Registration

The wizards register at design time only — they live in
`Packages/DesignTime.dpk` and load via Delphi's IDE-package mechanism.
After installing `DesignTime.bpl` you must restart the IDE for the
"ERDesigns OBD" menu group to appear.

## Pattern: editing a wizard template

Each wizard generates source by string substitution against a template
embedded in the unit. To change what gets emitted:

1. Edit the template literal in the wizard unit (search for the
   `TStringList` build-up of the new file).
2. Recompile and reinstall `DesignTime.dpk`.
3. Restart the IDE; the new template applies to subsequently created
   files only.

When adding a new wizard, register it in
`OBD.CustomControl.Register.pas` alongside the visual component
registrations so a single `Register` call exposes everything.
