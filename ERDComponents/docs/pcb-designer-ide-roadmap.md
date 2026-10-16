# PCB designer and future IDE

The designer stays a reusable component library. An IDE host owns its documents,
inspector, toolbox, ribbon, simulation controller and hardware connection. It must
be possible to use the designer without TypeScript, firmware or a USB device.

## Current addition

The supplied board template exposes 28 buffered terminal outputs plus one fixed
addressable status LED: LED2 (XL-5050RGBC-WS2812B). The manufacturing source verifies
U6.39 → RGB net → R45 → LED2.4 (DIN); LED2.1 is +5V and LED2.3 is GND. The GPIO
metadata uses the source's net name `RGB`, rather than guessing an ESP32 GPIO
number from a module pad number. LED1 is a separate indicator and is not exposed.
Selecting any LED2 contact or its housing selects the one programmable group.
It accepts an output assignment through the existing inspector; it is fixed
physical hardware, not a movable scene item. No LED is driven by the designer.

The bundled template now has 29 programmable groups. Existing projects enforce
the template policy they were saved with; retain the previous template when
opening projects authored against its original 28-output policy. Do not bypass
that check or silently retarget a saved assignment when revising templates.

## Proposed next component work

1. **Physical device descriptors.** Add a reusable collection to templates for
   fixed devices: stable ID, kind, selectable contour, connection/channel, pixel
   count and physical pixel positions. A ring stores each pixel's position and
   address index, plus first-pixel marker, clockwise/counterclockwise order and
   rotation. A strip, matrix or single onboard RGB LED uses the same address model.
   Preserve imported hardware IDs when replacing artwork or changing board color.
2. **Device overlays and reusable templates.** Render address/index labels,
   single-pixel selection, multi-pixel selection, disabled pixels and assignment
   overlays. Provide 12/16/24-pixel ring fixtures, strip and matrix examples.
   Ring size, start index and direction are template properties. Keep editable
   scene notes/components independent of fixed physical pixels.
3. **Preview-state API.** Supply an optional state model keyed by device ID and
   pixel index, with RGB values, brightness, on/off and diagnostic state. Rendering
   consumes this model without modifying the project or adding undo entries.
   Batch updates should repaint once; light/dark theme applies to designer chrome,
   while LED colors represent the simulated device state.
4. **Simulation host.** A separate controller owns the clock, play/pause/step,
   reset and deterministic stimulus. Start with steady color, blink, chase and
   ring progress. Use a fake device backend so replay is repeatable without USB.
   Add inputs such as a virtual button, digital sensor or UART frame through
   explicit stimuli. TypeScript execution belongs to the later IDE host.

These are proposals for the next phase; pixel-device descriptors, animated
simulation and USB control are not implemented by this change.

## IDE repository boundary

Before extracting the library, keep demo-specific dialogs and commands under
`demo/`; publish the designer/scene/template APIs, standalone inspector binding,
object tree, palette and validation panel with a versioned project format. The
new IDE repository can reference a pinned release or package of this library.
Its document model owns dirty state, save/open, tabs and command routing. Keep
fixture-based import tests and the Delphi Windows smoke project with the library.

## Later USB hardware testing

Use a transport-independent device service, with a simulator backend first and a
USB serial backend later. The IDE sends the same normalized device-state frames
to either backend. Firmware and the host need an agreed protocol before hardware
control can be implemented:

- Handshake: protocol version, board/template identity, firmware version and
  supported capabilities; verify pixel counts and channel identities.
- Commands: set pixel/channel state, apply a complete frame, clear/reset, report
  input state, and return structured errors. Define acknowledgement, sequence
  numbers, timeouts and maximum frame rates.
- Operation: explicit connect/disconnect, live-preview enable, reconnect behavior
  and an explicit stop/clear action. Editing or opening a document alone must not
  start writing to hardware. Firmware should define its own disconnect timeout.
- Diagnostics: distinguish simulation from device acknowledgement, retain a
  bounded transport log and make a mismatched board visible to the IDE user.

A good first vertical slice is **select LED2 → choose a color → preview locally →
explicitly send it to the connected board → clear it**. Follow with a ring chase
using the same device-state model. It needs the actual firmware/USB protocol and
hardware verification; the component library does not assume them.
