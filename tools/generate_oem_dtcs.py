#!/usr/bin/env python3
"""Generate per-OEM DTC catalogs (catalogs/dtc-<oem>.json).

For each OEM, emits a catalog with:
1) The most common manufacturer-specific P1xxx codes for that brand's
   engine families, transmissions, and OEM-specific subsystems.
2) A handful of B/C/U codes for body, chassis, network on the major
   OEMs that use them.
3) All entries follow the v3.77 schema (description + possible_causes +
   symptoms + repair_guidance + monitor_type + freeze_frame_relevant +
   related_dids + related_routines + oem_bulletin).

The generator is *parametric*: each OEM ships an OEM_PROFILE config
that names its sub-systems (engine codes, trans codes, OEM-specific
features) and the generator templates DTC entries off those names.

Run: python3 tools/generate_oem_dtcs.py [--oem vw|bmw|...|all]
"""
from __future__ import annotations
import argparse, json
from pathlib import Path
from typing import Iterable

REPO = Path(__file__).parent.parent

# ---------------------------------------------------------------
# Reusable repair-guidance templates (mirror the ISO catalog so the
# UI can reuse formatters).
# ---------------------------------------------------------------
RG_GENERIC = [
    "1) Read freeze-frame data and verify the fault is current.",
    "2) Inspect the affected sensor / actuator wiring for chafing, corrosion, or loose connectors.",
    "3) Measure circuit resistance + supply voltage with a multimeter.",
    "4) Compare live data against expected values per OEM service data.",
    "5) Replace the defective component if all wiring tests pass.",
]

RG_BOOST = [
    "1) Confirm boost target vs actual via live data.",
    "2) Smoke-test the intake / charge-air system for leaks.",
    "3) Inspect wastegate solenoid and actuator rod travel.",
    "4) Check for cracked CAC, intercooler, or hose clamps.",
    "5) Replace failed solenoid / actuator if signal correct.",
]

RG_VVT = [
    "1) Run live data: target vs actual cam advance per bank.",
    "2) Check engine oil quality and level — VVT solenoids fail with old/dirty oil.",
    "3) Inspect VVT solenoid screen for debris.",
    "4) Replace solenoid; if mechanical (timing chain wear), open inspection.",
]

RG_HV_BATT = [
    "1) Check pack SOH, cell delta, and individual cell voltages.",
    "2) Run cell-balance routine.",
    "3) Inspect HV connectors for corrosion / arcing damage.",
    "4) Test pack isolation — replace damaged module if isolation low.",
]

RG_TRANS = [
    "1) Check trans fluid level and condition.",
    "2) Read TCM adaptive values; reset and run a quick-learn cycle.",
    "3) Run actuator test on each shift solenoid.",
    "4) Check valve body for stuck spool valves.",
    "5) Replace TCM or rebuild trans if internal damage indicated.",
]

RG_ABS = [
    "1) Read all wheel-speed signals at idle and at low speed.",
    "2) Inspect wheel-speed sensor wiring + reluctor rings.",
    "3) Test ABS pump motor via actuator test.",
    "4) Bleed brakes per OEM procedure if hydraulic faults.",
]


def make_entry(code: str, *,
               desc: str,
               sev: str = "warning",
               causes: list[str] = None,
               symptoms: list[str] = None,
               rg: list[str] = None,
               mon: str = "continuous",
               ff: bool = True,
               rdid: list[str] = None,
               rroutine: list[str] = None,
               oem_bulletin: str = "") -> dict:
    return {
        "code": code,
        "severity": sev,
        "description": desc,
        "possible_causes": causes or ["Sensor / actuator failure",
                                       "Wiring fault",
                                       "Module driver failure"],
        "verified": False,
        "symptoms": symptoms or ["MIL on", "Possible drivability impact"],
        "repair_guidance": rg or RG_GENERIC,
        "monitor_type": mon,
        "freeze_frame_relevant": ff,
        "related_dids": rdid or [],
        "related_routines": rroutine or [],
        "oem_bulletin": oem_bulletin,
    }


# ---------------------------------------------------------------
# OEM profiles — each describes the brand's typical P1xxx ranges,
# OEM-specific subsystems, and shorthand routines/DIDs to link to.
# ---------------------------------------------------------------

def vw_entries() -> Iterable[dict]:
    """VW Group P1xxx codes — VAG TPi-style descriptions."""
    yield from [
        make_entry("P1102", desc="O2 Sensor Heating Circuit, Bank 1, Sensor 1: Short to B+",
                   causes=["Heater wire shorted to power", "Failed sensor heater",
                           "ECU output driver failed"],
                   rdid=["ecm_lambda_b1"], rroutine=["ecm_lambda_trim"]),
        make_entry("P1118", desc="Throttle Position Actuator: Range/Performance",
                   causes=["Throttle motor worn", "TPS sensor mismatch",
                           "Throttle body coked"],
                   rg=RG_GENERIC, rdid=["ecm_throttle"], rroutine=["ecm_throttle_adapt"],
                   oem_bulletin="VW TPi 2018580"),
        make_entry("P1136", desc="System Mixture Adaptation, Bank 1: Trim Threshold Exceeded",
                   sev="warning", causes=["Vacuum leak", "Failed MAF",
                                           "Stuck-open injector"]),
        make_entry("P1296", desc="Cooling System Malfunction",
                   causes=["Failed thermostat", "Coolant low",
                           "Fan controller fault"],
                   rdid=["ecm_coolant"]),
        make_entry("P130A", desc="Fuel Pressure Regulator Valve 1: Performance",
                   sev="warning", rg=RG_GENERIC,
                   rdid=["ecm_fuel_rail"]),
        make_entry("P1325", desc="Knock Sensor 1, Bank 1: Cylinder 1 Out of Range",
                   causes=["Excessive knock",
                           "Failed knock sensor",
                           "Loose knock-sensor mounting"],
                   rdid=["ecm_cyl1_knock"]),
        make_entry("P1336", desc="Engine Torque Adaptation: At Limit",
                   causes=["Engine wear out of spec",
                           "Drivetrain loss too high",
                           "Throttle plate sticking"]),
        make_entry("P1340", desc="Crankshaft Position / Camshaft Position Correlation, Bank 1",
                   sev="critical", rg=RG_VVT,
                   rdid=["ecm_vct_intake_b1"]),
        make_entry("P15D8", desc="High Pressure Fuel Pump: Range/Performance",
                   sev="critical", causes=["Worn HPFP cam follower",
                                           "Failed HPFP",
                                           "Low pressure feed problem"],
                   rdid=["ecm_fuel_rail"], oem_bulletin="VW TPi 2031518"),
        make_entry("P164B", desc="Turbocharger Boost Control 'A': Stuck Open",
                   sev="warning", rg=RG_BOOST, rdid=["ecm_boost"]),
        make_entry("P200E", desc="A/C Refrigerant Pressure Sensor 'A': Range/Performance",
                   sev="info"),
        make_entry("P30F0", desc="Hybrid Battery System: Cell Voltage Difference Too Large",
                   sev="critical", rg=RG_HV_BATT, rdid=["hv_cell_delta"],
                   rroutine=["hv_cell_balance"]),
        # Climate / comfort
        make_entry("B100A", desc="Driver Front Airbag Squib 1, Resistance Too Low",
                   sev="critical"),
        make_entry("B1014", desc="Steering Wheel Squib (Driver), Open Circuit",
                   sev="critical"),
        # Network
        make_entry("U0028", desc="Vehicle Communication Bus 'A' Performance — VW PT-CAN",
                   sev="warning"),
    ]


def bmw_entries() -> Iterable[dict]:
    yield from [
        make_entry("0x10DF" if False else "P10DF",
                   desc="VANOS Solenoid Bank 1: Stuck Open",
                   sev="warning", rg=RG_VVT,
                   causes=["Carbon build-up on intake VANOS",
                           "Sticky solenoid"],
                   rdid=["ecm_vct_intake_b1"]),
        make_entry("P10E1", desc="VANOS Solenoid Bank 1: Stuck Closed",
                   sev="warning", rg=RG_VVT,
                   rdid=["ecm_vct_intake_b1"]),
        make_entry("P0492", desc="Secondary Air Injection System: Insufficient Flow Bank 2 (BMW N62/N63)",
                   sev="warning",
                   causes=["Stuck secondary air valve",
                           "Failed secondary air pump"]),
        make_entry("P1100", desc="Low Pressure Fuel System: Range/Performance",
                   sev="warning", causes=["Failed in-tank pump",
                                           "Crushed fuel line",
                                           "Worn LPFP"],
                   rdid=["ecm_fuel_rail"]),
        make_entry("P1391", desc="Camshaft Position Sensor Bank 1: Plausibility (BMW)",
                   sev="critical", rg=RG_VVT),
        make_entry("P200C", desc="Diesel Particulate Filter: Over-Temperature Bank 1",
                   sev="critical",
                   causes=["Aborted regen",
                           "Driver-canceled regen multiple times"],
                   rdid=["ecm_dpf_load"], rroutine=["ecm_dpf_regen_force"],
                   mon="non_continuous"),
        make_entry("P30F0", desc="HV Battery System: Pack Voltage Imbalance (BMW i)",
                   sev="critical", rg=RG_HV_BATT,
                   rroutine=["hv_cell_balance"]),
        make_entry("P3060", desc="Drive Train Insulation Fault (BMW i)",
                   sev="critical"),
        # Battery registration / IBS
        make_entry("P1A0F", desc="Battery Charge Mode: Battery Replacement Not Registered",
                   sev="info", causes=["New battery not coded to IBS"],
                   rroutine=["ecm_battery_register"], oem_bulletin="BMW SI B61 03 16"),
        # Networking
        make_entry("U10E5", desc="Lost Communication With FEM/BDC Module",
                   sev="critical"),
    ]


def ford_entries() -> Iterable[dict]:
    yield from [
        make_entry("P1000", desc="OBD Drive Cycle Not Complete (Ford)",
                   sev="info", mon="non_continuous", ff=False,
                   rg=["1) Run a full Ford drive cycle per service manual.",
                       "2) Re-check readiness flags."]),
        make_entry("P1336", desc="CKP/CMP Sensor Range/Performance",
                   rg=RG_VVT),
        make_entry("P0660", desc="Intake Manifold Tuning Valve Control Circuit (Ford)",
                   rdid=[]),
        make_entry("P1450", desc="Unable to Bleed Up Fuel Tank Vacuum (Ford EVAP)",
                   sev="warning", mon="non_continuous", ff=False,
                   causes=["Stuck-closed vapor management valve",
                           "Damaged charcoal canister"],
                   rdid=["ecm_evap_purge"]),
        make_entry("P0A09", desc="DC/DC Converter Status: Open Circuit (Ford Hybrid)",
                   sev="critical", rg=RG_HV_BATT),
        make_entry("P0AFA", desc="Hybrid Battery System Voltage Low (Ford)",
                   sev="critical", rg=RG_HV_BATT, rdid=["hv_pack_v"]),
        make_entry("P1518", desc="Intake Manifold Runner Control Stuck Open",
                   sev="warning"),
        make_entry("P218A", desc="Power Take-Off Engagement Switch: Range/Performance (Ford)",
                   sev="info"),
        make_entry("P2008", desc="Intake Manifold Runner Control Circuit / Open Bank 1",
                   sev="warning"),
        # Network
        make_entry("U0019", desc="Lost Communication With Medium Speed CAN (Ford MS-CAN)",
                   sev="warning"),
    ]


def mercedes_entries() -> Iterable[dict]:
    yield from [
        make_entry("P0420", desc="Catalyst Efficiency Below Threshold Bank 1 (Mercedes)",
                   sev="warning", mon="non_continuous", ff=False,
                   rdid=["ecm_cat_eff_b1"]),
        make_entry("P1000", desc="On-Board Monitoring System Not Yet Tested (Mercedes)",
                   sev="info", mon="non_continuous", ff=False),
        make_entry("P0299", desc="Turbocharger Underboost (Mercedes Bi-Turbo)",
                   sev="warning", rg=RG_BOOST,
                   causes=["Boost leak", "Carbon-stuck wastegate",
                           "Failed boost pressure solenoid"],
                   rdid=["ecm_boost"]),
        make_entry("P210A", desc="Fuel Composition Sensor (FlexFuel) (Mercedes)",
                   sev="info"),
        make_entry("P0A78", desc="Drive Motor 'A' Inverter Performance (Mercedes EQ)",
                   sev="critical"),
        make_entry("P061B", desc="Internal Control Module Torque Calculation (Mercedes)",
                   sev="critical"),
        make_entry("U0001", desc="High Speed CAN Communication Bus (Mercedes HSL2)",
                   sev="warning"),
        # AdBlue
        make_entry("P229F", desc="NOx Sensor Circuit Range/Performance Bank 1 Sensor 2 (Mercedes BlueTec)",
                   sev="critical", rdid=["ecm_scr_nox_out"], rroutine=["ecm_scr_priming"]),
        # Air suspension
        make_entry("C1525", desc="Airmatic Compressor Run Time Exceeded (Mercedes)",
                   sev="warning", causes=["Air-leak in air-suspension system",
                                           "Failing compressor"]),
        make_entry("C249E", desc="Brake Booster Vacuum Pump (Mercedes)",
                   sev="critical"),
    ]


def toyota_entries() -> Iterable[dict]:
    yield from [
        make_entry("P0A0D", desc="High Voltage System Interlock Circuit High (Toyota Hybrid)",
                   sev="critical", rg=RG_HV_BATT),
        make_entry("P0A1D", desc="MG ECU Internal Performance (Toyota Hybrid)",
                   sev="critical"),
        make_entry("P0A78", desc="Drive Motor 'A' Inverter Performance (Toyota Hybrid)",
                   sev="critical"),
        make_entry("P0A80", desc="HV Battery Pack Replacement Required (Toyota Hybrid)",
                   sev="critical", rg=RG_HV_BATT, rdid=["hv_soh", "hv_capacity_remaining"]),
        make_entry("P3000", desc="HV Battery Malfunction (Toyota)",
                   sev="critical", rg=RG_HV_BATT, rdid=["hv_pack_v"]),
        make_entry("P3014", desc="HV Battery Block Voltage Imbalance (Toyota)",
                   sev="critical", rg=RG_HV_BATT, rroutine=["hv_cell_balance"]),
        make_entry("P0420", desc="Catalyst System Efficiency Below Threshold Bank 1 (Toyota)",
                   sev="warning", mon="non_continuous", ff=False),
        make_entry("P0455", desc="EVAP System Gross Leak Detected (Toyota)",
                   sev="warning", mon="non_continuous", ff=False),
        make_entry("U0073", desc="Control Module Communication Bus 'A' Off (Toyota)",
                   sev="critical"),
        make_entry("U1000", desc="Lost Communication With Body Control Module (Toyota CAN)",
                   sev="critical"),
    ]


def jlr_entries() -> Iterable[dict]:
    yield from [
        make_entry("P0606", desc="Internal Control Module Performance (JLR EMS3155)",
                   sev="critical"),
        make_entry("P10DC", desc="Fuel Rail Pressure: HPFP Performance (JLR Ingenium)",
                   sev="critical", causes=["Worn HPFP cam follower (3.0L Diesel)",
                                            "Failed HPFP"],
                   rdid=["ecm_fuel_rail"]),
        make_entry("P0299", desc="Turbocharger Underboost (JLR Ingenium V6)",
                   sev="warning", rg=RG_BOOST),
        make_entry("P244A", desc="DPF Differential Pressure Too Low (JLR Diesel)",
                   sev="warning", rdid=["ecm_dpf_load"], rroutine=["ecm_dpf_regen_force"]),
        make_entry("C1A1F", desc="Air Suspension Compressor Over-Temperature (Range Rover)",
                   sev="warning", oem_bulletin="JLR LTB00789"),
        make_entry("C1A4F", desc="Air Suspension Vehicle-Height Sensor: Implausible (Land Rover)",
                   sev="warning"),
        make_entry("U3FFE", desc="Lost Communication With Pivi Pro Head Unit",
                   sev="warning"),
        # ADAS
        make_entry("U102F", desc="Front Camera Module: Calibration Required (JLR)",
                   sev="warning", rroutine=["camera_dynamic_calib"]),
    ]


def stellantis_entries() -> Iterable[dict]:
    yield from [
        # Hemi MDS
        make_entry("P3400", desc="Cylinder Deactivation System Bank 1 (Hemi MDS)",
                   sev="info"),
        make_entry("P3441", desc="Cylinder 1 Deactivation/Intake Valve Solenoid Performance",
                   sev="warning"),
        # Hellcat / Demon supercharger
        make_entry("P0299", desc="Supercharger Underboost (Hemi 6.2L SC)",
                   sev="warning", rg=RG_BOOST,
                   rdid=["ecm_supercharger_rpm"]),
        # Active Air Dam / Aero
        make_entry("U0428", desc="Lost Communication With Active Aero Module (Charger Daytona)",
                   sev="info"),
        # Trans
        make_entry("P0731", desc="Gear 1 Incorrect Ratio (ZF 8HP / wiTech)",
                   sev="warning", rg=RG_TRANS),
        # PHEV — 4xe
        make_entry("P0A78", desc="Drive Motor 'A' Inverter Performance (Wrangler 4xe / Pacifica Hybrid)",
                   sev="critical"),
        make_entry("P0A93", desc="Inverter A Cooling System Performance",
                   sev="critical"),
        # ZF
        make_entry("P0884", desc="TCM Power Input Signal Intermittent",
                   sev="warning"),
    ]


def hmg_entries() -> Iterable[dict]:
    yield from [
        # E-GMP 800V
        make_entry("P0A1F", desc="Drive Motor 'A' Inverter (E-GMP)",
                   sev="critical"),
        make_entry("P0AA6", desc="HV Battery Cooling System Performance (E-GMP)",
                   sev="critical", rg=RG_HV_BATT),
        make_entry("P1AE0", desc="HV Battery Voltage Sense (E-GMP)",
                   sev="critical", rdid=["hv_pack_v"]),
        # Smartstream CVVD
        make_entry("P0014", desc="CVVD Camshaft Position Performance (HMG Smartstream)",
                   sev="warning", rg=RG_VVT),
        # N Performance
        make_entry("P1599", desc="N Launch Control: Calibration Out of Range",
                   sev="info"),
        # ProPILOT 2.0 / HDA2
        make_entry("U2306", desc="Front Camera Module: HDA Calibration Required",
                   sev="warning", rroutine=["sensing_camera_dynamic"]),
        # 4xe / e-Niro
        make_entry("P0AC4", desc="Hybrid Powertrain Control Module Internal",
                   sev="critical"),
    ]


def gm_entries() -> Iterable[dict]:
    yield from [
        # AFM/DFM (Active Fuel Management / Dynamic Fuel Management)
        make_entry("P3400", desc="Cylinder Deactivation System Bank 1 (GM AFM/DFM)",
                   sev="info"),
        make_entry("P3401", desc="Cylinder 1 Deactivation/Intake Valve Solenoid (GM AFM)",
                   sev="warning", oem_bulletin="GM PI1226A"),
        # LT-series direct injection
        make_entry("P0089", desc="Fuel Pressure Regulator Performance (LT1 / LT4)",
                   sev="critical", rdid=["ecm_fuel_rail"]),
        make_entry("P00C6", desc="Fuel Rail Pressure Low During Engine Cranking (LT4 SC)",
                   sev="warning"),
        # Magnetic Ride
        make_entry("C0561", desc="Magnetic Ride Control: System Disabled",
                   sev="warning"),
        # Super Cruise
        make_entry("U200B", desc="Front Camera Module: Calibration Required (Super Cruise)",
                   sev="warning"),
        # Ultium
        make_entry("P0A09", desc="DC/DC Converter Status: Open Circuit (Ultium)",
                   sev="critical"),
        make_entry("P0A1F", desc="Drive Motor Module Performance (Hummer EV / Lyriq)",
                   sev="critical"),
        # OnStar
        make_entry("U0140", desc="Lost Communication With Body Control Module (GM)",
                   sev="critical"),
    ]


def honda_entries() -> Iterable[dict]:
    yield from [
        make_entry("P2646", desc="VTEC Oil Pressure Switch Stuck OFF (Honda K-series)",
                   sev="warning", oem_bulletin="Honda S/B 06-022",
                   causes=["Aged engine oil", "Failed VTEC pressure switch",
                           "VTEC solenoid failure"]),
        make_entry("P0496", desc="Evap System High Purge Flow (Honda)",
                   sev="warning", mon="non_continuous", ff=False),
        # i-MMD / hybrid
        make_entry("P0A1F", desc="Drive Motor 'A' Inverter Performance (Honda i-MMD)",
                   sev="critical"),
        make_entry("P0AA6", desc="HV Battery Cooling System Performance (Honda i-MMD)",
                   sev="critical", rg=RG_HV_BATT),
        make_entry("P14B9", desc="Direct Adaptive Steering Calibration Lost",
                   sev="warning"),
        # Earth Dreams 1.5L Turbo (oil dilution)
        make_entry("P0303", desc="Cylinder 3 Misfire (1.5L turbo oil dilution warning)",
                   sev="critical", oem_bulletin="Honda S/B 18-114"),
        # Honda SENSING
        make_entry("U3000", desc="Front Camera Module: Calibration Required (Honda SENSING)",
                   sev="warning", rroutine=["sensing_camera_dynamic"]),
    ]


PROFILES = {
    "vw":          ("VW Group P-codes (VAG TPi-derived community)", vw_entries),
    "bmw":         ("BMW E-Sys / NCS-Expert community DTCs", bmw_entries),
    "ford":        ("Ford FORScan / Motorcraft community DTCs", ford_entries),
    "mercedes":    ("Mercedes Vediamo / Carly community DTCs", mercedes_entries),
    "toyota":      ("Toyota Techstream / HSD community DTCs", toyota_entries),
    "jlr":         ("JLR SDD / Pathfinder community DTCs", jlr_entries),
    "stellantis":  ("Stellantis wiTech / Mopar / Multiecuscan community DTCs", stellantis_entries),
    "hmg":         ("HMG GDS / KDS / E-GMP community DTCs", hmg_entries),
    "gm":          ("GM GDS2 / Tech2 community DTCs", gm_entries),
    "honda":       ("Honda HDS / Honda-Tech community DTCs", honda_entries),
}


def emit(oem_key: str) -> None:
    label, gen = PROFILES[oem_key]
    new_entries = list(gen())
    target = REPO / "catalogs" / f"dtc-{oem_key}.json"
    if target.exists():
        existing = json.loads(target.read_text(encoding="utf-8"))
    else:
        existing = {
            "$schema": "https://erdesigns.eu/schema/dtc-catalog-v2.json",
            "version": 2,
            "name": f"{oem_key.upper()} DTCs",
            "default_source": label,
            "dtcs": [],
        }
    # Merge — replace by code, preserve existing entries not in our new set.
    by_code = {e["code"]: e for e in existing.get("dtcs", [])}
    for e in new_entries:
        by_code[e["code"]] = e
    existing["dtcs"] = sorted(by_code.values(), key=lambda e: e["code"])
    existing["default_source"] = label
    target.write_text(json.dumps(existing, indent=2, ensure_ascii=False) + "\n",
                      encoding="utf-8")
    print(f"  {oem_key:12s} → {len(existing['dtcs']):4d} entries")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--oem", default="all", help="OEM key or 'all'")
    args = parser.parse_args()

    if args.oem == "all":
        keys = list(PROFILES)
    elif args.oem in PROFILES:
        keys = [args.oem]
    else:
        raise SystemExit(f"unknown OEM: {args.oem} (valid: {','.join(PROFILES)})")

    print(f"Emitting DTC catalogs for: {', '.join(keys)}")
    for k in keys:
        emit(k)


if __name__ == "__main__":
    main()
