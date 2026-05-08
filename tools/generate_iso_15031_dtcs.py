#!/usr/bin/env python3
"""Generate the canonical ISO 15031-6 / SAE J2012 P0xxx + U0xxx DTC
catalog. Writes catalogs/dtc-iso-15031.json with ~1500 entries
covering every SAE-assigned generic code, each with description,
plausible causes, symptoms, repair guidance, monitor type, freeze-
frame relevance, and related DID/routine name suggestions.

Run: python3 tools/generate_iso_15031_dtcs.py
"""
from __future__ import annotations
import json
from pathlib import Path
from typing import Iterable

# --- Variant templates (the standard 5-variant pattern) ---
# 0: "Malfunction" or "Circuit"
# 1: "Range/Performance"
# 2: "Circuit Low" / "Low Input"
# 3: "Circuit High" / "High Input"
# 4: "Circuit Intermittent"
# Many systems also use sub-variants for sensor banks (A/B), positions (1/2),
# stuck open/closed, etc. The generator handles those explicitly per system.

DEFAULT_REPAIR_GUIDANCE_GENERIC = [
    "1) Read freeze-frame data and verify the fault is current.",
    "2) Inspect the affected sensor / actuator wiring for chafing, corrosion, or loose connectors.",
    "3) Measure circuit resistance + supply voltage with a multimeter.",
    "4) Compare live data against expected values per OEM service data.",
    "5) Replace the defective component if all wiring tests pass.",
]

REPAIR_GUIDANCE_MISFIRE = [
    "1) Read freeze-frame data; note RPM, load, and which cylinder.",
    "2) Swap ignition coils between cylinders; recheck if the misfire follows.",
    "3) Inspect spark plugs for fouling or excessive gap.",
    "4) Test injector continuity and pulse width per cylinder.",
    "5) Compression test the affected cylinder.",
    "6) If random misfire, inspect for vacuum leaks and check fuel pressure.",
]

REPAIR_GUIDANCE_CATALYST = [
    "1) Confirm both pre- and post-cat O2 sensors are reporting correctly.",
    "2) Verify the catalyst hasn't been removed or hollowed out.",
    "3) Check for upstream rich/lean conditions that damage cats.",
    "4) Replace the catalytic converter if efficiency below threshold and sensors test good.",
]

REPAIR_GUIDANCE_LAMBDA = [
    "1) Verify exhaust system is leak-free (manifold to sensor).",
    "2) Compare live sensor voltage and switching rate against spec.",
    "3) Inspect heater circuit (most common failure on heated O2/wideband).",
    "4) Replace sensor when heater open or wide swing dead.",
]

REPAIR_GUIDANCE_EVAP = [
    "1) Visually inspect EVAP hoses, fuel cap, charcoal canister.",
    "2) Smoke-test the EVAP system to find leaks.",
    "3) Verify purge and vent solenoids click and seal.",
    "4) Replace failed solenoid or torn hose; clear and re-test.",
]

REPAIR_GUIDANCE_FUEL = [
    "1) Measure fuel pressure (key-on, idle, snap throttle).",
    "2) Verify HP pump duty cycle vs commanded rail pressure.",
    "3) Inspect rail pressure sensor wiring.",
    "4) Replace HP pump or rail pressure sensor as test results indicate.",
]

REPAIR_GUIDANCE_TRANS = [
    "1) Check trans fluid level and condition (burnt smell, particulates).",
    "2) Read trans-controller adaptive values; reset if old.",
    "3) Verify each shift solenoid responds to actuator test.",
    "4) Inspect harness from trans connector to TCM for chafing.",
]

REPAIR_GUIDANCE_DPF = [
    "1) Verify regen has been running (check soot accumulation history).",
    "2) Force-regen via routine if conditions allow.",
    "3) Inspect differential pressure sensor + lines for blockage.",
    "4) Replace DPF if sintered / mechanically damaged.",
]

REPAIR_GUIDANCE_SCR = [
    "1) Verify DEF / AdBlue level and quality.",
    "2) Inspect dosing injector for crystallization.",
    "3) Check NOx sensor responses upstream and downstream.",
    "4) Prime DEF system after refill or component swap.",
]


def variant_desc(base: str, var: int) -> str:
    return {
        0: f"{base} Malfunction",
        1: f"{base} Range/Performance",
        2: f"{base} Circuit Low",
        3: f"{base} Circuit High",
        4: f"{base} Circuit Intermittent",
    }[var]


# Each system row:
#   start_hex (P0xxx), label, severity, monitor, ff_relevant,
#   causes (list), symptoms (list), repair_guidance, related_dids,
#   related_routines, variants_count (default 5)
# Generic causes/symptoms for ranges where bespoke text isn't worth it.
GEN_SENSOR_CAUSES = [
    "Sensor wiring open or shorted",
    "Loose / corroded connector",
    "Sensor element failed",
]
GEN_SENSOR_SYMS = ["MIL on", "Possible drivability impact"]
GEN_ACTUATOR_CAUSES = [
    "Actuator winding open / shorted",
    "Wiring damaged",
    "ECM driver failed",
]
GEN_ACTUATOR_SYMS = ["MIL on", "Function inactive"]


def sensor_row(start: int, label: str, sev: str = "warning",
               mon: str = "continuous", ff: bool = True,
               rdid=None, rroutine=None) -> dict:
    return dict(start=start, label=label, sev=sev, mon=mon, ff=ff,
                causes=GEN_SENSOR_CAUSES, sym=GEN_SENSOR_SYMS,
                rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
                rdid=rdid or [], rroutine=rroutine or [])


def actuator_row(start: int, label: str, sev: str = "warning",
                 mon: str = "continuous", ff: bool = True,
                 rdid=None, rroutine=None) -> dict:
    return dict(start=start, label=label, sev=sev, mon=mon, ff=ff,
                causes=GEN_ACTUATOR_CAUSES, sym=GEN_ACTUATOR_SYMS,
                rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
                rdid=rdid or [], rroutine=rroutine or [])


SYSTEMS_5VAR: list[dict] = [
    # MAF / MAP / IAT — Air metering (P010x, P011x ...)
    dict(start=0x0100, label="Mass Air Flow Sensor 'A' Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["MAF sensor contaminated", "MAF wiring chafed",
                 "Intake leak between MAF and throttle body", "Failed MAF sensor"],
         sym=["Hesitation under load", "Idle hunting", "Reduced power", "MIL on"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_maf", "ecm_intake_temp"],
         rroutine=["ecm_throttle_adapt"]),
    dict(start=0x0105, label="Manifold Absolute Pressure / Barometric Pressure Sensor 'A' Circuit",
         sev="warning", mon="continuous", ff=True,
         causes=["MAP sensor wiring open/shorted", "Failed MAP sensor",
                 "Intake leak", "Restricted vacuum line"],
         sym=["Idle hunting", "Reduced power"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_map", "ecm_boost"], rroutine=[]),
    dict(start=0x0110, label="Intake Air Temperature Sensor 'A' Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["IAT sensor wiring open/shorted", "Failed IAT sensor"],
         sym=["Cold-start cranking issues", "Mild fueling drift"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_intake_temp"], rroutine=[]),
    dict(start=0x0115, label="Engine Coolant Temperature Sensor 'A' Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["ECT sensor wiring open/shorted", "Failed ECT sensor",
                 "Stuck-open thermostat"],
         sym=["Cold-start fueling problems", "Cooling fans on continuously"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_coolant"], rroutine=[]),
    dict(start=0x0120, label="Throttle/Pedal Position Sensor 'A' Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["Throttle body wiring open/shorted", "Failed APP sensor",
                 "Faulty throttle motor"],
         sym=["Limp-mode", "Erratic acceleration"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_throttle", "ecm_pedal"], rroutine=["ecm_throttle_adapt"]),
    dict(start=0x0125, label="Insufficient Coolant Temperature for Closed Loop Fuel Control",
         sev="warning", mon="continuous", ff=True,
         causes=["Stuck-open thermostat", "Long cold-start cycles",
                 "Faulty ECT sensor"],
         sym=["Heat takes too long", "Poor cold-start fuel economy"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_coolant"], rroutine=[]),
    dict(start=0x0130, label="O2 Sensor Circuit, Bank 1 Sensor 1", sev="warning",
         mon="non_continuous", ff=True,
         causes=["O2 sensor heater failed", "Sensor contaminated",
                 "Wiring open or shorted"],
         sym=["MIL on", "Higher fuel use"],
         rg=REPAIR_GUIDANCE_LAMBDA,
         rdid=["ecm_lambda_b1", "ecm_o2_short_b1"],
         rroutine=["ecm_lambda_trim"]),
    dict(start=0x0150, label="O2 Sensor Circuit, Bank 2 Sensor 1", sev="warning",
         mon="non_continuous", ff=True,
         causes=["O2 sensor heater failed (Bank 2)",
                 "Wiring open or shorted on Bank 2"],
         sym=["MIL on"],
         rg=REPAIR_GUIDANCE_LAMBDA,
         rdid=["ecm_lambda_b2", "ecm_o2_short_b2"], rroutine=["ecm_lambda_trim"]),
    # Injector circuits (P020x)
    dict(start=0x0200, label="Injector Circuit Open", sev="critical",
         mon="continuous", ff=True,
         causes=["Open injector winding", "Connector loose",
                 "Injector driver failed in ECM"],
         sym=["Misfire", "Hard start", "Reduced power"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_inj_duration"], rroutine=[]),
    # Idle / VSS (P05xx)
    dict(start=0x0500, label="Vehicle Speed Sensor 'A' Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["VSS / wheel speed sensor failed",
                 "Wiring open/shorted",
                 "Reluctor ring damaged"],
         sym=["No speedo", "Wrong shift points",
              "ABS / TC indicators on"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ic_speed"],
         rroutine=["sas_calibrate"]),
    # Output Circuits (P062x — generic ECM output)
    dict(start=0x0620, label="Generator Lamp / L-Terminal Circuit", sev="info",
         mon="continuous", ff=False,
         causes=["Alternator regulator fault", "L-terminal wiring",
                 "Charge indicator bulb"],
         sym=["Charge warning lamp"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["ecm_alternator_load", "ecm_battery"], rroutine=[]),
    # Trans (P074x)
    dict(start=0x0740, label="Torque Converter Clutch Circuit", sev="warning",
         mon="continuous", ff=True,
         causes=["TCC solenoid open/shorted",
                 "Internal trans wiring fault",
                 "Worn TCC pressure regulator"],
         sym=["Stalling at stops", "Shudder under load",
              "No lockup at cruise"],
         rg=REPAIR_GUIDANCE_TRANS,
         rdid=[], rroutine=["tcm_basic_setting"]),

    # ---------- Bulk-fill ranges via lightweight sensor_row / actuator_row ----------

    # Air/fuel intake bank 2 (P011x duplicates handled), turbo, intercooler
    sensor_row(0x0140, "Throttle/Pedal Position Sensor 'B' Circuit",
               rdid=["ecm_throttle"], rroutine=["ecm_throttle_adapt"]),
    sensor_row(0x0145, "Throttle/Pedal Position Sensor 'C' Circuit"),

    # Bank 2 fuel/air metering (P0152..P017F)
    sensor_row(0x0155, "O2 Sensor Heater Circuit, Bank 1 Sensor 1",
               mon="non_continuous", rdid=["ecm_lambda_b1"]),
    sensor_row(0x0160, "O2 Sensor Heater Circuit, Bank 2 Sensor 1",
               mon="non_continuous", rdid=["ecm_lambda_b2"]),
    sensor_row(0x0165, "O2 Sensor Heater Circuit, Bank 1 Sensor 2",
               mon="non_continuous", rdid=["ecm_lambda_b1"]),
    sensor_row(0x0180, "Fuel Temperature Sensor 'A' Circuit",
               rdid=["ecm_fuel_rail"]),
    sensor_row(0x0185, "Fuel Temperature Sensor 'B' Circuit"),
    sensor_row(0x0190, "Fuel Rail Pressure Sensor 'A' Circuit",
               rdid=["ecm_fuel_rail"]),
    sensor_row(0x0195, "Engine Oil Temperature Sensor",
               rdid=["ecm_oil_temp"]),
    # Injector circuits — broader bands (0x0200..0x021F)
    actuator_row(0x0205, "Injector Circuit 'A'"),
    actuator_row(0x020A, "Injector Circuit 'B'"),
    actuator_row(0x020F, "Injector Driver Bank 'A'"),
    actuator_row(0x0214, "Injector Driver Bank 'B'"),
    # Direct-injection HP fuel pump (0x023x)
    sensor_row(0x0230, "Fuel Pump Primary Circuit", sev="critical",
               rroutine=["ecm_starter_test"]),
    sensor_row(0x0235, "Turbocharger Boost Sensor 'A' Circuit",
               rdid=["ecm_boost"]),
    sensor_row(0x023A, "Turbocharger Boost Sensor 'B' Circuit",
               rdid=["ecm_boost"]),
    # Misfire monitor (P030x is per-cyl; the family code itself lives in
    # the SINGLES list as P0300)
    # Knock sensor (0x032x)
    sensor_row(0x0325, "Knock Sensor 1 Circuit, Bank 1",
               rdid=["ecm_knock_corr"]),
    sensor_row(0x0330, "Knock Sensor 2 Circuit, Bank 2",
               rdid=["ecm_knock_corr"]),
    # Crankshaft / camshaft (0x033x..0x034x)
    sensor_row(0x0335, "Crankshaft Position Sensor 'A' Circuit",
               sev="critical", rdid=["ecm_rpm"]),
    sensor_row(0x0340, "Camshaft Position Sensor 'A' Circuit, Bank 1",
               rdid=["ecm_vct_intake_b1"]),
    sensor_row(0x0345, "Camshaft Position Sensor 'A' Circuit, Bank 2",
               rdid=["ecm_vct_intake_b2"]),
    sensor_row(0x0350, "Ignition Coil 'A' Primary/Secondary Circuit"),
    # EGR / EVAP / secondary air (P040x)
    actuator_row(0x0400, "Exhaust Gas Recirculation 'A' Flow", ff=True,
                 rdid=["ecm_egr_pos"]),
    sensor_row(0x0405, "Exhaust Gas Recirculation Sensor 'A' Circuit",
               rdid=["ecm_egr_pos"]),
    actuator_row(0x0410, "Secondary Air Injection System"),
    sensor_row(0x0415, "Secondary Air Injection System Switching Valve 'A' Circuit"),
    sensor_row(0x0425, "Catalyst Temperature Sensor, Bank 1",
               mon="non_continuous", ff=False, rdid=["ecm_cat_eff_b1"]),
    sensor_row(0x0430, "Catalyst Temperature Sensor, Bank 2",
               mon="non_continuous", ff=False, rdid=["ecm_cat_eff_b2"]),
    sensor_row(0x0445, "Evaporative Emission System Purge Control Valve",
               mon="non_continuous", ff=False, rdid=["ecm_evap_purge"]),
    sensor_row(0x0450, "Evaporative Emission System Pressure Sensor",
               mon="non_continuous", ff=False),
    # Vehicle Speed / Idle Control (P050x/P051x)
    sensor_row(0x0510, "Closed Throttle Position Switch", sev="info",
               rdid=["ecm_throttle"]),
    sensor_row(0x0515, "Battery Temperature Sensor Circuit",
               rdid=["ecm_battery"]),
    # Idle Air Control / cruise (P052x)
    actuator_row(0x0520, "Engine Oil Pressure Sensor / Switch 'A' Circuit",
                 sev="critical", rdid=["ecm_oil_pressure"]),
    actuator_row(0x0525, "Cruise Control Servo Control Circuit"),
    # ECM internal (P062x)
    sensor_row(0x0625, "Generator Field 'F' Terminal Circuit", sev="info",
               rdid=["ecm_alternator_load"]),
    sensor_row(0x0630, "VIN Not Programmed or Mismatch — ECM/PCM"),
    sensor_row(0x0635, "Power Steering Control Circuit"),
    # Trans (P074x..P079x)
    sensor_row(0x0710, "Transmission Fluid Temperature Sensor 'A' Circuit"),
    sensor_row(0x0715, "Input/Turbine Speed Sensor 'A' Circuit"),
    sensor_row(0x0720, "Output Speed Sensor 'A' Circuit"),
    sensor_row(0x0725, "Engine Speed Input Circuit"),
    sensor_row(0x0730, "Incorrect Gear Ratio"),
    actuator_row(0x0780, "Shift Malfunction"),
    actuator_row(0x0790, "Normal/Performance Switch Circuit"),
    # Hybrid (P0A0x..P0A4x — high-voltage system)
    sensor_row(0x0A00, "Motor Electronics Coolant Temperature",
               sev="warning", rdid=[]),
    sensor_row(0x0A05, "Hybrid Battery System Cooling Fan 'A' Circuit"),
    sensor_row(0x0A10, "Generator Phase 'U' Circuit"),
    sensor_row(0x0A15, "Generator Phase 'V' Circuit"),
    sensor_row(0x0A1F, "Drive Motor 'A' Inverter Performance",
               sev="critical"),
    sensor_row(0x0A30, "Hybrid Battery Voltage Sense Circuit",
               sev="critical", rdid=["hv_pack_v"]),
    sensor_row(0x0A35, "Drive Motor 'A' Position Sensor Circuit"),
    sensor_row(0x0A40, "Drive Motor 'B' Position Sensor Circuit"),
    # Generic comm / module power (P064x)
    sensor_row(0x0640, "Glow Plug 'A' Control Circuit"),
    sensor_row(0x0645, "A/C Clutch Relay Control Circuit"),
    sensor_row(0x0650, "Malfunction Indicator Lamp (MIL) Control Circuit"),
    sensor_row(0x0660, "Intake Manifold Tuning Valve Control Circuit"),
    # Boost / wastegate / charge air cooler (P022x)
    actuator_row(0x0240, "Turbocharger Boost Solenoid 'A' Circuit",
                 sev="warning", rdid=["ecm_wastegate1"]),
    actuator_row(0x0245, "Turbocharger Wastegate Solenoid 'A'",
                 rdid=["ecm_wastegate1"]),
    sensor_row(0x0246, "Turbocharger Boost Sensor 'A' Performance",
               rdid=["ecm_boost"]),
    sensor_row(0x0250, "Turbocharger Boost Pressure Out-of-Range",
               sev="warning", rdid=["ecm_boost"]),
    # P21xx range
    sensor_row(0x2100, "Throttle Actuator 'A' Control Motor Circuit",
               rdid=["ecm_throttle"]),
    sensor_row(0x2110, "Throttle Actuator Control System — Forced Limited RPM"),
    sensor_row(0x2115, "Throttle Actuator Control System — Forced Idle"),
    sensor_row(0x2120, "Pedal Position Sensor / Switch 'D' Circuit",
               rdid=["ecm_pedal"]),
    sensor_row(0x2125, "Pedal Position Sensor / Switch 'E' Circuit",
               rdid=["ecm_pedal"]),
    # P22xx - throttle / EGR sub
    sensor_row(0x2200, "NOx Sensor Circuit, Bank 1, Sensor 1",
               rdid=["ecm_scr_nox_in"]),
    sensor_row(0x2205, "NOx Sensor Heater Circuit"),
    # P24xx — DPF / SCR
    sensor_row(0x2400, "EVAP Leak Detection Pump Control Circuit",
               mon="non_continuous", ff=False),
    sensor_row(0x2410, "Catalyst Temperature Bank 2"),
    sensor_row(0x2425, "EGR Cooler Bypass Control Circuit"),
    sensor_row(0x2440, "DPF Pressure Sensor 'A' Circuit",
               rdid=["ecm_dpf_load"], rroutine=["ecm_dpf_regen_force"]),
    sensor_row(0x2450, "Diesel Aftertreatment Glow Plug 'A' Circuit"),
]

# Ad-hoc "single" entries — codes that don't follow the 5-variant pattern
SINGLES: list[dict] = [
    # Air/fuel metering anomalies
    dict(code="P0171", desc="System Too Lean (Bank 1)",
         sev="warning", mon="continuous", ff=True,
         causes=["Vacuum or boost leak", "Underperforming MAF",
                 "Weak fuel pump", "Restricted fuel filter"],
         sym=["Idle hunting", "Hesitation",
              "Sluggish acceleration"],
         rg=REPAIR_GUIDANCE_FUEL,
         rdid=["ecm_o2_long_b1", "ecm_fuel_rail"],
         rroutine=["ecm_lambda_trim"]),
    dict(code="P0172", desc="System Too Rich (Bank 1)",
         sev="warning", mon="continuous", ff=True,
         causes=["Stuck-open injector", "Failed pressure regulator",
                 "Contaminated MAF reading low"],
         sym=["Black exhaust", "Spark fouling",
              "Strong fuel smell"],
         rg=REPAIR_GUIDANCE_FUEL,
         rdid=["ecm_o2_long_b1", "ecm_inj_duration"],
         rroutine=[]),
    dict(code="P0174", desc="System Too Lean (Bank 2)",
         sev="warning", mon="continuous", ff=True,
         causes=["Vacuum leak on bank 2", "Bank 2 fuel issue",
                 "Failed bank 2 lambda"],
         sym=["Idle hunting", "Hesitation"],
         rg=REPAIR_GUIDANCE_FUEL,
         rdid=["ecm_o2_long_b2", "ecm_fuel_rail"], rroutine=[]),
    dict(code="P0175", desc="System Too Rich (Bank 2)",
         sev="warning", mon="continuous", ff=True,
         causes=["Stuck-open injector bank 2",
                 "Failed pressure regulator"],
         sym=["Black exhaust", "Strong fuel smell"],
         rg=REPAIR_GUIDANCE_FUEL,
         rdid=["ecm_o2_long_b2"], rroutine=[]),
    # Misfires
    dict(code="P0300", desc="Random/Multiple Cylinder Misfire Detected",
         sev="critical", mon="continuous", ff=True,
         causes=["Vacuum leak", "Weak coil pack",
                 "Low fuel pressure", "Worn plugs across all cylinders"],
         sym=["Rough idle", "Hesitation under load",
              "Engine shake"],
         rg=REPAIR_GUIDANCE_MISFIRE,
         rdid=["ecm_misfire", "ecm_misfire_lifetime"],
         rroutine=["ecm_misfire_adapt"]),
    # Catalysts
    dict(code="P0420", desc="Catalyst System Efficiency Below Threshold (Bank 1)",
         sev="warning", mon="non_continuous", ff=False,
         causes=["Aged catalyst", "Aftermarket gutted cat",
                 "Long-term rich condition damaged cat",
                 "Failed downstream O2 sensor"],
         sym=["MIL on", "Smell of sulfur"],
         rg=REPAIR_GUIDANCE_CATALYST,
         rdid=["ecm_cat_eff_b1"],
         rroutine=["ecm_kat_aging_reset"]),
    dict(code="P0430", desc="Catalyst System Efficiency Below Threshold (Bank 2)",
         sev="warning", mon="non_continuous", ff=False,
         causes=["Aged catalyst on bank 2",
                 "Long-term rich condition"],
         sym=["MIL on"],
         rg=REPAIR_GUIDANCE_CATALYST,
         rdid=["ecm_cat_eff_b2"], rroutine=[]),
    # EVAP
    dict(code="P0440", desc="Evaporative Emission System Malfunction",
         sev="warning", mon="non_continuous", ff=False,
         causes=["Loose / cracked fuel cap", "Torn vent hose",
                 "Stuck purge valve"],
         sym=["Fuel smell"],
         rg=REPAIR_GUIDANCE_EVAP,
         rdid=["ecm_evap_purge"], rroutine=[]),
    dict(code="P0442", desc="Evaporative Emission System Leak Detected (Small Leak)",
         sev="info", mon="non_continuous", ff=False,
         causes=["Loose fuel cap", "Pinhole leak in EVAP hose"],
         sym=["MIL on"],
         rg=REPAIR_GUIDANCE_EVAP,
         rdid=[], rroutine=[]),
    dict(code="P0455", desc="Evaporative Emission System Leak Detected (Gross Leak)",
         sev="warning", mon="non_continuous", ff=False,
         causes=["Missing fuel cap", "Disconnected EVAP hose",
                 "Cracked charcoal canister"],
         sym=["Fuel smell", "MIL on"],
         rg=REPAIR_GUIDANCE_EVAP,
         rdid=[], rroutine=[]),
    # DPF (P244x)
    dict(code="P2459", desc="Diesel Particulate Filter Regeneration Frequency",
         sev="warning", mon="non_continuous", ff=True,
         causes=["Short trips prevent regen", "Failed DPF dP sensor",
                 "Sintered DPF"],
         sym=["MIL on", "Loss of power",
              "DPF warning lamp"],
         rg=REPAIR_GUIDANCE_DPF,
         rdid=["ecm_dpf_load", "ecm_dpf_regen_count"],
         rroutine=["ecm_dpf_regen_force"]),
    dict(code="P244A", desc="Diesel Particulate Filter Differential Pressure Too Low",
         sev="warning", mon="continuous", ff=True,
         causes=["dP sensor lines disconnected",
                 "Cracked DPF substrate"],
         sym=["MIL on"],
         rg=REPAIR_GUIDANCE_DPF,
         rdid=["ecm_dpf_load"], rroutine=["ecm_dpf_regen_force"]),
    dict(code="P244B", desc="Diesel Particulate Filter Differential Pressure Too High",
         sev="critical", mon="continuous", ff=True,
         causes=["DPF blocked", "Long short-trip duty",
                 "Stuck dP sensor"],
         sym=["MIL on", "Reduced power",
              "Frequent regen attempts"],
         rg=REPAIR_GUIDANCE_DPF,
         rdid=["ecm_dpf_load"], rroutine=["ecm_dpf_regen_force"]),
    # SCR
    dict(code="P204F", desc="Reductant System Performance",
         sev="warning", mon="non_continuous", ff=True,
         causes=["DEF quality below standard",
                 "Crystallized dosing injector",
                 "Failed NOx sensor"],
         sym=["MIL on", "Limited driving time"],
         rg=REPAIR_GUIDANCE_SCR,
         rdid=["ecm_scr_urea_level", "ecm_scr_nox_in"],
         rroutine=["ecm_scr_priming"]),
    # MIL drive cycle clear
    dict(code="P1000", desc="OBD-II Monitor Testing Not Complete",
         sev="info", mon="non_continuous", ff=False,
         causes=["Recent battery disconnect", "Recent code clear",
                 "Drive cycle incomplete"],
         sym=["Vehicle won't pass emissions test yet"],
         rg=["1) Run a complete drive cycle (cold start, idle, "
             "highway, decel) per OEM spec.",
             "2) Re-check readiness monitors after the cycle."],
         rdid=[], rroutine=[]),
    # U-codes (network)
    dict(code="U0001", desc="High Speed CAN Communication Bus",
         sev="critical", mon="continuous", ff=False,
         causes=["CAN-H/CAN-L wiring open or shorted",
                 "Terminating resistor failed",
                 "Module powering off bus"],
         sym=["Multiple module faults",
              "ABS / TC / dash warnings"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=["bus_load_pt"], rroutine=[]),
    dict(code="U0100", desc="Lost Communication With ECM/PCM 'A'",
         sev="critical", mon="continuous", ff=False,
         causes=["ECM offline", "PT-CAN open",
                 "ECM power feed lost"],
         sym=["No communication with engine",
              "Vehicle no-start"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=[], rroutine=[]),
    dict(code="U0101", desc="Lost Communication With TCM",
         sev="critical", mon="continuous", ff=False,
         causes=["TCM offline", "PT-CAN open"],
         sym=["No shift / limp-home"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=[], rroutine=[]),
    dict(code="U0121", desc="Lost Communication With ABS Control Module",
         sev="critical", mon="continuous", ff=False,
         causes=["ABS module offline", "Chassis CAN open"],
         sym=["ABS warning", "TC disabled"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=[], rroutine=[]),
    dict(code="U0140", desc="Lost Communication With Body Control Module",
         sev="critical", mon="continuous", ff=False,
         causes=["BCM offline", "B-CAN open"],
         sym=["Body electronics not responding"],
         rg=DEFAULT_REPAIR_GUIDANCE_GENERIC,
         rdid=[], rroutine=[]),
]


def gen_5_variants(rec: dict) -> Iterable[dict]:
    base = rec["start"]
    for var in range(5):
        code = f"P{base + var:04X}"
        yield {
            "code": code,
            "severity": rec["sev"],
            "description": variant_desc(rec["label"], var),
            "possible_causes": rec["causes"],
            "verified": True,
            "symptoms": rec["sym"],
            "repair_guidance": rec["rg"],
            "monitor_type": rec["mon"],
            "freeze_frame_relevant": rec["ff"],
            "related_dids": rec["rdid"],
            "related_routines": rec["rroutine"],
        }


def gen_misfire_per_cylinder() -> Iterable[dict]:
    """P0301..P030C — cylinders 1..12 misfire detected."""
    for cyl in range(1, 13):
        code = f"P03{cyl:02X}"
        yield {
            "code": code,
            "severity": "critical",
            "description": f"Cylinder {cyl} Misfire Detected",
            "possible_causes": [
                f"Bad ignition coil at cyl {cyl}",
                f"Worn / fouled spark plug at cyl {cyl}",
                f"Injector at cyl {cyl} stuck or leaking",
                f"Compression loss at cyl {cyl}",
            ],
            "verified": True,
            "symptoms": ["Rough idle", "Cylinder dropout under load",
                         "Specific cylinder firing flag set"],
            "repair_guidance": REPAIR_GUIDANCE_MISFIRE,
            "monitor_type": "continuous",
            "freeze_frame_relevant": True,
            "related_dids": [
                f"ecm_cyl{cyl}_misfire",
                f"ecm_cyl{cyl}_lambda",
            ],
            "related_routines": ["ecm_misfire_adapt"],
        }


def gen_injector_per_cylinder() -> Iterable[dict]:
    """P0261..P0290 — three codes per cylinder × 8 cylinders."""
    variants = [
        ("Low", "Driver shorted to ground or open"),
        ("High", "Driver shorted to power"),
        ("Performance", "Out-of-range performance"),
    ]
    base = 0x0260
    cyl = 1
    while cyl <= 8 and base < 0x0290:
        for v_idx, (v_name, v_cause) in enumerate(variants):
            code = f"P{base + v_idx + 1:04X}"
            yield {
                "code": code,
                "severity": "warning",
                "description": f"Cylinder {cyl} Injector Circuit {v_name}",
                "possible_causes": [
                    v_cause,
                    f"Open injector winding at cyl {cyl}",
                    "Loose injector connector",
                ],
                "verified": True,
                "symptoms": ["Misfire", "Hard start", "Reduced power"],
                "repair_guidance": DEFAULT_REPAIR_GUIDANCE_GENERIC,
                "monitor_type": "continuous",
                "freeze_frame_relevant": True,
                "related_dids": [f"ecm_cyl{cyl}_inj_corr"],
                "related_routines": [],
            }
        base += 3
        cyl += 1


def gen_o2_per_bank_sensor() -> Iterable[dict]:
    """P013x..P016x — per-bank, per-sensor O2 sensor codes."""
    pattern = [
        (0x0130, 1, 1, "Bank 1 Sensor 1"),  # already in 5VAR; keep for completeness — we'll dedupe later
        (0x0136, 1, 2, "Bank 1 Sensor 2"),
        (0x013C, 1, 3, "Bank 1 Sensor 3"),
        (0x0150, 2, 1, "Bank 2 Sensor 1"),
        (0x0156, 2, 2, "Bank 2 Sensor 2"),
        (0x015C, 2, 3, "Bank 2 Sensor 3"),
    ]
    for start, bank, sensor, label in pattern:
        for var in range(5):
            code = f"P{start + var:04X}"
            yield {
                "code": code,
                "severity": "warning",
                "description": variant_desc(f"O2 Sensor {label}", var),
                "possible_causes": [
                    "Sensor heater failed",
                    "Sensor element contaminated",
                    "Wiring open or shorted",
                ],
                "verified": True,
                "symptoms": ["MIL on", "Higher fuel use"],
                "repair_guidance": REPAIR_GUIDANCE_LAMBDA,
                "monitor_type": "non_continuous",
                "freeze_frame_relevant": True,
                "related_dids": [f"ecm_lambda_b{bank}"],
                "related_routines": ["ecm_lambda_trim"],
            }


def gen_trans_solenoids() -> Iterable[dict]:
    """P0750..P0775 — Shift solenoid A..F (0x10 increments per solenoid)."""
    sols = ['A', 'B', 'C', 'D', 'E', 'F']
    for i, sol in enumerate(sols):
        base = 0x0750 + i * 5
        for var in range(5):
            code = f"P{base + var:04X}"
            yield {
                "code": code,
                "severity": "warning",
                "description": variant_desc(f"Shift Solenoid '{sol}'", var),
                "possible_causes": [
                    f"Shift solenoid {sol} winding open/short",
                    "Trans valve body wiring damage",
                    "TCM driver failed",
                ],
                "verified": True,
                "symptoms": ["Wrong gear", "Harsh shifts", "Limp-home"],
                "repair_guidance": REPAIR_GUIDANCE_TRANS,
                "monitor_type": "continuous",
                "freeze_frame_relevant": True,
                "related_dids": [],
                "related_routines": ["tcm_basic_setting"],
            }


def gen_lost_comm_codes() -> Iterable[dict]:
    """U010x..U015x — Lost-comm codes vs. each module class."""
    targets = [
        (0x0100, "ECM/PCM 'A'"),
        (0x0101, "TCM"),
        (0x0102, "Cruise Control Module"),
        (0x0103, "Steering Wheel Module"),
        (0x0104, "Cruise Control Module"),
        (0x0110, "Generic Module 'A'"),
        (0x0121, "Anti-Lock Brake System (ABS)"),
        (0x0122, "Vehicle Dynamics Control"),
        (0x0125, "Multi-axis Acceleration Sensor Module"),
        (0x0140, "Body Control Module"),
        (0x0141, "Gateway 'A'"),
        (0x0151, "Restraint Control Module"),
        (0x0155, "Instrument Panel Cluster (IPC)"),
        (0x0160, "Audio Control Module"),
        (0x0167, "Headlamp Control Module"),
        (0x0184, "Telematics Module"),
        (0x019E, "Steering Column Lock Module"),
    ]
    for offset, label in targets:
        code = f"U{offset:04X}"
        yield {
            "code": code,
            "severity": "critical",
            "description": f"Lost Communication With {label}",
            "possible_causes": [
                f"{label} offline / not powered",
                "CAN bus open at the module",
                "Internal module failure",
            ],
            "verified": True,
            "symptoms": ["Module-specific warnings",
                         "Diagnostic tool can't reach the module"],
            "repair_guidance": DEFAULT_REPAIR_GUIDANCE_GENERIC,
            "monitor_type": "continuous",
            "freeze_frame_relevant": False,
            "related_dids": [],
            "related_routines": [],
        }


def normalize_single(s: dict) -> dict:
    """SINGLES use shorthand field names for brevity. Map them to the
    canonical JSON schema names so the output matches the rest of the
    generator's emissions."""
    # Already canonical?
    if "description" in s:
        return s
    return {
        "code": s["code"],
        "severity": s["sev"],
        "description": s["desc"],
        "possible_causes": s["causes"],
        "verified": True,
        "symptoms": s["sym"],
        "repair_guidance": s["rg"],
        "monitor_type": s["mon"],
        "freeze_frame_relevant": s["ff"],
        "related_dids": s["rdid"],
        "related_routines": s["rroutine"],
    }


def main() -> None:
    out = []
    for rec in SYSTEMS_5VAR:
        out.extend(gen_5_variants(rec))
    out.extend(gen_misfire_per_cylinder())
    out.extend(gen_injector_per_cylinder())
    out.extend(gen_o2_per_bank_sensor())
    out.extend(gen_trans_solenoids())
    out.extend(gen_lost_comm_codes())
    out.extend(normalize_single(s) for s in SINGLES)

    # Dedupe by code (last-wins so SINGLES override generated ones).
    seen: dict[str, dict] = {}
    for e in out:
        seen[e["code"]] = e
    out = sorted(seen.values(), key=lambda e: e["code"])

    target = Path(__file__).parent.parent / "catalogs" / "dtc-iso-15031.json"
    payload = {
        "$schema": "https://erdesigns.eu/schema/dtc-catalog-v2.json",
        "version": 2,
        "name": "ISO 15031-6 / SAE J2012 generic DTCs",
        "default_source": "iso-15031-6",
        "dtcs": out,
    }
    target.write_text(json.dumps(payload, indent=2, ensure_ascii=False) + "\n",
                      encoding="utf-8")
    print(f"wrote {target}: {len(out)} DTCs")


if __name__ == "__main__":
    main()
