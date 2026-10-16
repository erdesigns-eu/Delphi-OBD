# PCB validation

Validation is optional and separate from `TERDPCBDesigner`. It has two layers:

- `TERDPCBValidator` checks snapshots and returns issue records. It imports the
  scene model, not the designer or its window/control implementation.
- `TERDPCBValidationPanel` displays copied issue records, severity groups, search,
  counts and an activation event. It retains no designer, scene or pin references.

The demo host adapts its designer's channels into validation pins, schedules a
check 200 ms after changes, and handles navigation. A content/template key avoids
rerunning rules for selection-only changes. The toolbar's **Validation** button
shows/hides the bottom panel without affecting the designer or inspector.

## Host integration

```pascal
Context := ERDPCBCaptureValidation(Scene, Pins, HasTemplate);
ValidationPanel.Validate(Context);
```

Each `TERDPCBValidationPin` contains a programmable pin ID and a string array of
capabilities. Use the canonical names from the template, such as
`addressable-led-output`, `uart-tx`, `uart-rx`, `i2c-sda`, `spi-clock` and `can-tx`.
The demo adapter gets these names through `ERDPCBPinCapabilityName`. The validation
units themselves do not import `ERD.Components.PCBDesigner`.

Alternatively, run a standalone `TERDPCBValidator.Validate(Context)` and pass its
result to `Panel.SetIssues`. A host may supply issues from another source entirely.
Snapshots copy item records and nested pin-capability arrays. Custom rules receive
an additional copy; they cannot modify the live scene or the caller's snapshot.
Validation does not add history entries, select objects, change pins or invoke the
designer's validation events.

`Panel.OnNavigate` provides an issue containing its stable `ItemID`, property name,
pin and optional related object ID. The host decides whether to select, focus, open
an inspector or do something else. The demo checks that the ID still exists before
selecting/zooming, and ignores global messages and stale IDs. Refreshing reports,
searching and filtering never activate messages automatically. Click or Enter
activates a message; group headings do not navigate. Hover shows the full message.

## Built-in rules

| Code | Severity | Meaning |
| --- | --- | --- |
| `pin.missing` | Warning | An enabled LED output or bus role has no pin |
| `pin.unavailable` | Error | Assigned pin is not programmable in the supplied template |
| `pin.capability` | Error | Pin lacks the role's required capability |
| `pin.role-conflict` | Error | One component assigns multiple roles to the same pin |
| `pin.conflict` | Error | Active driver components claim the same pin |
| `property.invalid` | Error | A registered property's type, required value, bounds, choice or custom validator fails |
| `type.unknown` | Warning | Saved component type is not currently registered |
| `type.kind` | Error | Registered type and stored base kind disagree |
| `name.missing`, `name.conflict` | Error | Missing or duplicate component name |
| `template.unavailable` | Information | A pin-related design has no template to check against |

Missing bus roles are warnings, allowing partial designs such as TX-only UART or
MOSI-only SPI. Disabled drivers do not require missing pins or participate in
ownership conflicts; existing assignments are still checked against the template.
Notes do not require pins. Timers/counters may have optional pins and do not claim
output-driver ownership. A missing template suppresses availability/capability
errors while retaining template-independent checks. Already invalid edits are
normally rejected by the scene/designer; snapshot rules also support imported or
externally assembled contexts.

Schema validation reuses `ERDPCBValidateProperty`, the same value validation used
for normal scene editing. Read-only/locked properties are checked without editing
or temporarily unlocking their component.

## Custom rules

Set `Validator.OnValidate` or `Panel.OnValidate`. The handler receives the snapshot
and a temporary report. Append messages with
`Report.Add(Severity, Code, MessageText, ItemID, ItemName, PropertyName, Pin, RelatedItemID)`;
trailing metadata arguments are optional. Do not retain the temporary report.
The standalone callback sender is the validator; the panel forwards its own sender.
Give each rule a stable code, and use distinct property/pin metadata for multiple
messages from one rule. An issue's key combines code, item ID, property, pin and
related item ID, preserving selection across report refreshes.

Unexpected rule exceptions propagate to the caller. The panel replaces its report
only after successful validation; a failed custom rule leaves the previous report
available. Hosts may choose how to display such failures. Scene edit validation and
these diagnostic rules remain separate APIs.

## Display and persistence

`SeverityFilter` persists through DFM. `SearchText`, issue records and selection are
transient. Search matches all words against message, object name, code, property
and pin. Counts show the complete report; severity headings are ordered errors,
warnings, information, with rows sorted by object name/message. `IssueCount`,
`Issue(Index)`, `SeverityCount` and `VisibleIssue(Row, out Issue)` expose copied data.
`Clear` removes only messages, leaving filter preferences intact.

The panel follows shared theme, density and DPI settings, with solid semantic
icons on tinted badges and themed virtual-list scrollbars. Constructor-owned search,
filter and list controls are excluded from DFM child streaming. There are no
subscriptions or borrowed model lifetimes to detach on close.

Windows smoke tests cover rules, snapshot isolation, custom failures, grouping,
filtering, stable navigation metadata and DFM round-tripping. Linux Python/static
checks cannot execute VCL; run the Windows checklist for visual verification.
