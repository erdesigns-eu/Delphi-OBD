# Service Routines

`OBD.OEM.ServiceRoutines` is the canonical home for publicly
documented workshop procedures. Each registered routine carries its
RoutineControl identifier (UDS 0x31), required diagnostic session,
pre-conditions, post-conditions, safety class, and a citation back to
the public spec / TSB / community archive that documents it.

## Usage

```pascal
uses OBD.OEM.ServiceRoutines;

var
  Routine: TOBDServiceRoutine;
  Frame: TBytes;
begin
  if TOBDServiceRoutineRegistry.Instance.Find('oil_reset_bmw', Routine) then
  begin
    // Caller is responsible for opening the right diagnostic session
    // (Routine.RequiredSessionType) and honouring the pre-conditions
    // (Routine.PreConditions); we only build the wire frame.
    Frame := BuildRoutineControlFrame(Routine);
    MyUdsClient.Send(Frame);
  end;
end;
```

`GetByCategory` and `GetByOEM` let UIs filter the registry per panel.

## Safety classes

| Class | Meaning |
|---|---|
| `srsNone` | Safe; no movement / no electrical hazard. |
| `srsEngineMustBeRunning` | Engine running is part of the procedure (e.g. DPF regen). |
| `srsEngineMustBeOff` | Routine writes that require key-on engine-off. |
| `srsVehicleMustBeStationary` | Vehicle on level surface, parking brake set. |
| `srsVehicleMayMove` | EPB unwind, window/sunroof learn — caller MUST clear the area. |
| `srsBatteryMin12V5` | Pair with `OBD.ECU.Flashing.VoltageGate`. |
| `srsRequiresWorkshopLogin` | Workshop card / PIN / SVM portal needed. |

## Coverage at a glance

| Category | Routines |
|---|---|
| Maintenance | Oil reset (VAG, BMW, Mercedes, Ford, Toyota), AdBlue level reset |
| Steering & Brakes | SAS zero, EPB service-mode (open/close), ABS bleed, brake-pad change |
| Powertrain | DPF forced regen, throttle body adapt, idle relearn |
| Comfort | Window pinch learn, sunroof initialisation, seat memory reset, headlight aim |
| Battery & Electrical | BMW IBS, Mercedes IBS, Audi 12V battery write, alternator load test |
| TPMS | Sensor relearn, per-wheel ID write |
| Emissions | Readiness clear, drive-cycle marker |

The full registry sits in `src/Services/OBD.OEM.ServiceRoutines.pas`;
see the `SeedDefault` procedure for the one-line-per-routine entries
with their citations.

## Adding a new routine

1. Confirm the routine is documented in a public spec (ISO/SAE), an
   OEM TSB, or a reputable community archive (Ross-Tech, FORScan,
   BimmerCode, etc.). Don't add proprietary procedures.
2. Add an `Add(...)` line in `SeedDefault` with all fields filled.
3. The Citation field is **mandatory** — `Tests.OEM.ServiceRoutines`
   will fail if any entry has an empty citation.
4. Ship a fixture-driven test if the routine has a non-trivial
   OptionRecord layout.
