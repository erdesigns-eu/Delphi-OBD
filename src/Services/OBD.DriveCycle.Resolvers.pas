//------------------------------------------------------------------------------
// UNIT           : OBD.DriveCycle.Resolvers.pas
// CONTENTS       : Per-OEM drive-cycle resolvers that override the ISO
//                : 15031-7 generic baseline shipped in v3.82/B2. Each
//                : resolver returns OEM-specific TDriveCycleStep records
//                : for the monitors that the OEM publishes a tighter
//                : procedure for; monitors not overridden fall through
//                : to the generic step automatically.
//
// Sources        :
//   VW       — VW Self-Study Programs (SSP) covering EOBD readiness
//              + EVAP test conditions.
//   BMW      — BMW TIS published drive cycle for OBD II readiness.
//   Mercedes — Mercedes WIS readiness drive-cycle reference (EOBD).
//   Ford     — Ford TSB on OBD II readiness drive cycle (UN GTR No.5
//              variant).
//   Toyota   — Toyota Repair Manual EOBD readiness procedure +
//              Techstream service notes for catalyst & EVAP monitors.
//
// All five sets are publicly distributed in service-info packages and
// reproduced verbatim in OEM technician training materials.
//------------------------------------------------------------------------------
unit OBD.DriveCycle.Resolvers;

interface

uses
  System.SysUtils,

  OBD.DriveCycle.Advisor;

implementation

function Step(const Mon, Desc: string; Dur: Integer): TDriveCycleStep;
begin
  Result.Monitor := Mon;
  Result.Description := Desc;
  Result.DurationSeconds := Dur;
end;

//------------------------------------------------------------------------------
// VW — Self-Study Program 388 + 605 (EOBD readiness drive cycle)
//------------------------------------------------------------------------------
function ResolveVW(const Monitor, OEMKey: string): TDriveCycleStep;
begin
  if Monitor = 'Catalyst' then
    Result := Step(Monitor,
      'VW SSP 388: cold start. Idle 3 min in Drive (auto) or 1st (manual). '
      + 'Cruise at 65–80 km/h in closed loop for 5 min, then 90–110 km/h '
      + 'for 5 min, then decelerate without braking from 90 km/h.', 800)
  else if Monitor = 'EvaporativeSystem' then
    Result := Step(Monitor,
      'VW SSP 388: EVAP requires fuel level 25–75% AND ambient 4–35°C AND '
      + 'cold start (engine coolant within 6°C of intake air). Idle 4 min, '
      + 'then cruise 50–80 km/h for 10 min without sharp accelerations.', 900)
  else if Monitor = 'OxygenSensor' then
    Result := Step(Monitor,
      'VW SSP 605: cruise at 60 km/h in closed loop for 10 min after '
      + 'reaching coolant > 80°C. Avoid throttle transients.', 600)
  else if Monitor = 'EGRorVVTSystem' then
    Result := Step(Monitor,
      'VW SSP 605: cruise 80 km/h for 5 min, then decelerate to 30 km/h '
      + 'with foot off accelerator. Repeat 3×.', 1080)
  else
    Result := Default(TDriveCycleStep);  // signals fall-through to generic
end;

//------------------------------------------------------------------------------
// BMW — TIS published OBD II readiness drive cycle
//------------------------------------------------------------------------------
function ResolveBMW(const Monitor, OEMKey: string): TDriveCycleStep;
begin
  if Monitor = 'Catalyst' then
    Result := Step(Monitor,
      'BMW TIS: warm engine to operating temp. Cruise at 80 km/h in 4th/5th '
      + 'or D for 10 min, then a partial-throttle deceleration to 30 km/h. '
      + 'Repeat the cruise + decel pair twice.', 1500)
  else if Monitor = 'EvaporativeSystem' then
    Result := Step(Monitor,
      'BMW TIS: EVAP needs cold start (coolant 4–32°C, ambient 4–32°C). '
      + 'Drive normally with mixed city/highway for 20–30 min. Refuel must '
      + 'NOT be performed during the cycle.', 1800)
  else if Monitor = 'SecondaryAirSystem' then
    Result := Step(Monitor,
      'BMW TIS: cold start with engine coolant < 30°C. Let the secondary '
      + 'air pump complete its post-start cycle (~90–120 s).', 120)
  else if Monitor = 'OxygenSensor' then
    Result := Step(Monitor,
      'BMW TIS: cruise at 90 km/h in closed loop for 12 min after warm-up.', 720)
  else
    Result := Default(TDriveCycleStep);
end;

//------------------------------------------------------------------------------
// Mercedes — WIS readiness drive cycle (EOBD)
//------------------------------------------------------------------------------
function ResolveMercedes(const Monitor, OEMKey: string): TDriveCycleStep;
begin
  if Monitor = 'Catalyst' then
    Result := Step(Monitor,
      'MB WIS: warm engine to operating temp (>80°C). Drive at 80–100 km/h '
      + 'in top gear for 5 min, then decelerate without braking from '
      + '90 km/h. Repeat once.', 720)
  else if Monitor = 'EvaporativeSystem' then
    Result := Step(Monitor,
      'MB WIS: cold start, fuel 1/4–3/4. Idle 4 min then drive 50–80 km/h '
      + 'for 12 min minimum. EVAP runs after a complete shutdown soak.', 960)
  else if Monitor = 'OxygenSensor' then
    Result := Step(Monitor,
      'MB WIS: cruise at constant 60–80 km/h closed loop for 10 min after '
      + 'lambda sensors reach operating temperature.', 600)
  else if Monitor = 'PMFilter' then
    Result := Step(Monitor,
      'MB WIS (diesel): cruise > 70 km/h for 25 min minimum to allow PM '
      + 'filter regen monitor to complete.', 1500)
  else
    Result := Default(TDriveCycleStep);
end;

//------------------------------------------------------------------------------
// Ford — TSB OBD II readiness drive cycle
//------------------------------------------------------------------------------
function ResolveFord(const Monitor, OEMKey: string): TDriveCycleStep;
begin
  if Monitor = 'Catalyst' then
    Result := Step(Monitor,
      'Ford TSB: warm engine fully. Cruise at constant 70–100 km/h in OD '
      + 'for 5 min, decelerate without braking to 30 km/h, repeat the '
      + 'cycle 3×. AC and rear-defrost OFF.', 1200)
  else if Monitor = 'EvaporativeSystem' then
    Result := Step(Monitor,
      'Ford TSB: cold start (coolant within 6°C of ambient, both 4–32°C). '
      + 'Drive at steady 70–110 km/h for 15 min without rapid throttle '
      + 'transitions; fuel level 15–85%.', 900)
  else if Monitor = 'OxygenSensor' then
    Result := Step(Monitor,
      'Ford TSB: cruise at 65–95 km/h closed loop for 10 min, foot light '
      + 'on the throttle.', 600)
  else if Monitor = 'EGRorVVTSystem' then
    Result := Step(Monitor,
      'Ford TSB: 4 deceleration events from 90 to 30 km/h, foot off '
      + 'accelerator each time, 30 s apart.', 360)
  else
    Result := Default(TDriveCycleStep);
end;

//------------------------------------------------------------------------------
// Toyota — Repair Manual EOBD readiness drive cycle + Techstream notes
//------------------------------------------------------------------------------
function ResolveToyota(const Monitor, OEMKey: string): TDriveCycleStep;
begin
  if Monitor = 'Catalyst' then
    Result := Step(Monitor,
      'Toyota RM: warm engine. Cruise at 65–80 km/h in D for 8 min, then '
      + 'decelerate without braking to 0 km/h. Repeat 2×. Catalyst monitor '
      + 'completes on the second deceleration.', 1100)
  else if Monitor = 'EvaporativeSystem' then
    Result := Step(Monitor,
      'Toyota RM: 8 hours soak with ignition off, fuel 1/2–3/4, ambient '
      + '4.5–35°C. Cold start, idle 5 min, drive 50–80 km/h for 20 min '
      + 'including a 10-min steady cruise.', 1500)
  else if Monitor = 'HeatedCatalyst' then
    Result := Step(Monitor,
      'Toyota RM: cold start; let catalyst reach light-off temperature '
      + '(~3 min of normal driving from cold).', 240)
  else if Monitor = 'OxygenSensor' then
    Result := Step(Monitor,
      'Toyota RM: drive at 65–95 km/h for 12 min in closed loop. Avoid '
      + 'throttle transients.', 720)
  else if Monitor = 'OxygenSensorHeater' then
    Result := Step(Monitor,
      'Toyota RM: cold start; oxygen sensor heaters complete within '
      + '60 s of run.', 60)
  else
    Result := Default(TDriveCycleStep);
end;

initialization
  RegisterDriveCycleResolver('vw', ResolveVW);
  RegisterDriveCycleResolver('bmw', ResolveBMW);
  RegisterDriveCycleResolver('mercedes', ResolveMercedes);
  RegisterDriveCycleResolver('ford', ResolveFord);
  RegisterDriveCycleResolver('toyota', ResolveToyota);

end.
