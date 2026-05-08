#!/usr/bin/env python3
"""Build the 8 agricultural/construction catalogs under
catalogs/agricultural/. Most use J1939 SPN-coded faults; ECU layout
is engine + transmission + hydraulics + implement bus + telematics."""
from __future__ import annotations
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from build_smaller_vehicle import emit, total_entries  # noqa: E402

REPO = Path(__file__).parent.parent


def agri_ecus(extras=None):
    base = [
        ("0x7E0", "ecm", "Engine Control Module (J1939)"),
        ("0x7E1", "tcm", "Transmission Control"),
        ("0x7E2", "hydraulics", "Hydraulic system controller"),
        ("0x7E3", "pto", "Power Take-Off (PTO) control"),
        ("0x7E4", "afs", "AutoTrac / GPS guidance"),
        ("0x7E5", "implement_bus", "ISOBUS implement gateway"),
        ("0x7E6", "load_sensing", "Load-sensing pump control"),
        ("0x7E7", "diff_lock", "Diff lock + 4WD"),
        ("0x7E8", "creeper", "Creeper / range gearbox"),
        ("0x7E9", "axle_suspension", "Axle suspension (cab + front)"),
        ("0x7EA", "headland_management", "Headland management coordinator"),
        ("0x710", "abs", "ABS / steering brakes"),
        ("0x711", "cab_climate", "Cab climate"),
        ("0x712", "cluster", "Instrument cluster"),
        ("0x713", "head_unit", "Cab display / GreenStar / similar"),
        ("0x714", "telematics", "Telematics (JDLink / FieldOps / VisionLink)"),
        ("0x715", "lights", "Cab + work-light controller"),
        ("0x716", "ride_control", "Ride control (front-axle damping)"),
        ("0x717", "ssu_steering", "Steered axle SSU (smart steering unit)"),
        ("0x718", "battery_disconnect", "Battery disconnect / sleep"),
        ("0x719", "fuel_level", "Fuel + DEF level senders"),
        ("0x71A", "egr_dpf", "EGR + DPF + SCR aftertreatment"),
        ("0x71B", "ev_motor_inverter", "Electric drive motor (where fitted)"),
    ]
    return base + (extras or [])


# John Deere
emit("agricultural", oem_key="john-deere",
     brand_label="John Deere (Deere & Company)",
     applicable_wmis=["1JD"],
     source="Service ADVISOR (community) / OEM J1939 docs",
     ecus=agri_ecus(),
     engines=[
         ("powertech_4045", "PowerTech PSS 4.5L diesel"),
         ("powertech_6068", "PowerTech PSS 6.8L diesel"),
         ("powertech_6090", "PowerTech PSS 9.0L diesel"),
         ("powertech_6135", "PowerTech PSS 13.5L diesel"),
         ("xuv_gator_812", "Gator XUV 812cc petrol"),
         ("ev_battery_electric_5e", "5E Series Battery-Electric"),
     ],
     transmissions=[
         ("autoquad_plus", "AutoQuad Plus 24F/24R"),
         ("infinitelyvariable_ivt", "Infinitely Variable Transmission (IVT)"),
         ("autopowr_e23", "AutoPowr / e23 PowerShift"),
         ("creeper_4f4r", "Creeper 4F/4R"),
     ],
     hu_gens=[("greenstar_4_5", "GreenStar 4 / G5 display")],
     act_extra=[
         ("0x0360", "pto_engage", "PTO engage", "0x7E3", 5000,
          "Implement clear of personnel"),
         ("0x0361", "diff_lock_engage", "Diff lock engage",
          "0x7E7", 3000, "Stationary"),
     ],
     adapt_extra=[
         ("0x0150", "headland_default", "Headland mgmt default",
          "0x7EA", "uint8", 0, 8, 0, ""),
         ("0x0151", "load_sensing_pressure", "Load-sense pressure",
          "0x7E6", "uint8", 50, 250, 200, "bar"),
     ],
     dtc_codes=[
         ("SPN3251", "0x01", "occurrence_counter",
          "DPF differential pressure occurrences"),
         ("SPN5246", "0x01", "occurrence_counter",
          "SCR severe inducement occurrences"),
     ],
     highlights="PowerTech PSS diesel + IVT/AutoQuad/AutoPowr + GreenStar 4/G5",
     )

# CNH Industrial (Case IH / New Holland / Steyr)
emit("agricultural", oem_key="cnh",
     brand_label="CNH Industrial (Case IH / New Holland / Steyr)",
     applicable_wmis=["1NH"],
     source="EST / Electronic Service Tool community",
     ecus=agri_ecus(),
     engines=[
         ("fpt_n45", "FPT N45 4.5L diesel"),
         ("fpt_n67", "FPT N67 6.7L diesel"),
         ("fpt_c87", "FPT C87 8.7L diesel"),
         ("fpt_c13", "FPT Cursor 13 12.9L diesel"),
         ("fpt_c16_lng", "FPT Cursor 16 LNG"),
         ("fpt_h2_xc13", "FPT XC13 hydrogen ICE"),
         ("ev_t4", "T4 Electric Power"),
     ],
     transmissions=[
         ("powerdrive", "PowerDrive 21F/15R"),
         ("cvt_cvxdrive", "CVXDrive (Case CVT)"),
         ("auto_command", "Auto Command (NH CVT)"),
     ],
     hu_gens=[("intelliview_xii", "IntelliView XII / Pro 1200 display")],
     highlights="FPT N45/N67/C87/C13 + Cursor 16 LNG + XC13 hydrogen ICE",
     )

# Caterpillar
emit("agricultural", oem_key="caterpillar",
     brand_label="Caterpillar (Cat)",
     applicable_wmis=["CAT"],
     source="Cat ET (Electronic Technician) community",
     ecus=agri_ecus([
         ("0x71C", "telematics_visionlink", "VisionLink telematics"),
         ("0x71D", "auto_grade_control", "Auto Grade Control"),
     ]),
     engines=[
         ("c4_4_acert", "C4.4 ACERT 4.4L"),
         ("c7_1", "C7.1 7.1L"),
         ("c9_3", "C9.3 9.3L"),
         ("c13", "C13 12.5L"),
         ("c15", "C15 15L"),
         ("c18", "C18 18.1L"),
         ("c27_v12", "C27 V12 (mining)"),
         ("c32_v12", "C32 V12 (mining)"),
         ("ev_793F_electric", "793F Electric Drive (mining)"),
     ],
     transmissions=[
         ("ct23_powershift", "CT-23 powershift"),
         ("ct35_powershift", "CT-35 powershift"),
         ("hydrostatic_dual", "Hydrostatic dual-path (track loaders)"),
     ],
     hu_gens=[("cat_grade_2d_3d", "Cat Grade Control 2D/3D"),
               ("touchscreen_10", "Cat 10\" touchscreen")],
     highlights="C4.4 to C32 V12 + Auto Grade Control + VisionLink + 793F Electric",
     )

# Komatsu
emit("agricultural", oem_key="komatsu",
     brand_label="Komatsu",
     applicable_wmis=["KMT"],
     source="KomTrax telematics + service docs",
     ecus=agri_ecus([
         ("0x71C", "komtrax", "KomTrax / Komatsu Care telematics"),
         ("0x71D", "imc_2_0", "Intelligent Machine Control 2.0"),
     ]),
     engines=[
         ("saa6d107", "SAA6D107 6.7L diesel"),
         ("saa6d125", "SAA6D125 11.0L diesel"),
         ("saa6d140", "SAA6D140 15.2L diesel"),
         ("saa12v140", "SAA12V140 30.5L V12 (PC4000)"),
         ("ev_pc01_electric", "PC01 Mini Excavator Electric"),
         ("hb365_hybrid", "HB365 Hybrid excavator"),
     ],
     transmissions=[
         ("powershift_4f3r", "Powershift 4F/3R"),
         ("hst_dual", "HST dual-path (dozers)"),
     ],
     hu_gens=[("komatsu_monitor_7", "Komatsu Monitor 7\" colour")],
     highlights="SAA6D series + IMC 2.0 dozer auto-grade + HB365 Hybrid",
     )

# Kubota
emit("agricultural", oem_key="kubota",
     brand_label="Kubota Corporation",
     applicable_wmis=["KUB"],
     source="Kubota Service Manual community",
     ecus=agri_ecus(),
     engines=[
         ("v3307_di", "V3307-DI 3.3L diesel"),
         ("v3800_di", "V3800-DI 3.8L diesel"),
         ("v6108_di", "V6108-DI 6.1L diesel"),
         ("d1305", "D1305 1.3L diesel"),
         ("ev_lx_electric", "LX e-Series electric utility tractor"),
     ],
     transmissions=[
         ("hst_3", "HST-3 hydrostatic"),
         ("manual_8f8r", "Manual 8F/8R synchro shuttle"),
         ("powershift_24f24r", "Powershift 24F/24R (M8 series)"),
     ],
     hu_gens=[("kubota_now_telematics", "KubotaNOW telematics")],
     highlights="Compact tractors + V3307-DI/V3800-DI/V6108-DI",
     )

# AGCO (Massey Ferguson / Fendt / Valtra / Challenger)
emit("agricultural", oem_key="agco",
     brand_label="AGCO (Massey Ferguson / Fendt / Valtra / Challenger)",
     applicable_wmis=["1AG"],
     source="EDT (Electronic Diagnostic Tool) / community",
     ecus=agri_ecus([
         ("0x71C", "fuse_telematics", "AGCO Fuse / VarioGuide telematics"),
     ]),
     engines=[
         ("agco_power_44", "AGCO Power 44 4.4L diesel"),
         ("agco_power_75", "AGCO Power 75 7.4L diesel"),
         ("agco_power_84", "AGCO Power 84 8.4L diesel"),
         ("agco_power_98", "AGCO Power 98 9.8L diesel"),
         ("man_d3876_fendt", "MAN D3876 16.16L (Fendt 1100 MT)"),
         ("ev_fendt_e100", "Fendt e100 Vario Battery-Electric"),
     ],
     transmissions=[
         ("vario_fendt", "Fendt Vario CVT"),
         ("dyna_vt_mf", "Massey Ferguson Dyna-VT CVT"),
         ("dyna_step_mf", "MF Dyna Step"),
         ("activevibration_mf", "MF ActiveVibration cab suspension"),
     ],
     hu_gens=[("vario_terminal_12", "Fendt Vario Terminal 12\""),
               ("datatronic5_mf", "MF Datatronic 5")],
     highlights="AGCO Power 44/75/84/98 + Fendt Vario CVT + e100 Battery-Electric",
     )

# Claas
emit("agricultural", oem_key="claas",
     brand_label="Claas KGaA",
     applicable_wmis=["DEY"],
     source="Claas Diagnostic System / community",
     ecus=agri_ecus(),
     engines=[
         ("mb_om471_xerion", "MB OM 471 12.8L (Xerion 5000)"),
         ("mb_om473_xerion", "MB OM 473 15.6L (Xerion 12.650)"),
         ("dps_psa_ms104", "DPS / Cummins B6.7 (Arion)"),
         ("man_d2868_jaguar", "MAN D2868 16.2L (Jaguar forage harvester)"),
     ],
     transmissions=[
         ("hexashift", "HexaShift powershift (Arion)"),
         ("cmatic", "CMATIC CVT (Arion / Axion / Xerion)"),
         ("traction_control_xerion", "Xerion 4-wheel-steered chassis"),
     ],
     hu_gens=[("cebis_touch", "CEBIS touch terminal"),
               ("telematics_telematics", "Claas Telematics")],
     highlights="OM 471/473 + CMATIC CVT + Jaguar forage harvester (D2868)",
     )

# Volvo CE (construction equipment)
emit("agricultural", oem_key="volvo-ce",
     brand_label="Volvo Construction Equipment",
     applicable_wmis=["VCE"],
     source="Volvo Tech Tool community",
     ecus=agri_ecus([
         ("0x71C", "care_track", "CareTrack telematics"),
     ]),
     engines=[
         ("d6_volvo_ce", "Volvo D6 6L diesel"),
         ("d8_volvo_ce", "Volvo D8 8L diesel"),
         ("d11_volvo_ce", "Volvo D11 11L diesel"),
         ("d13_volvo_ce", "Volvo D13 13L diesel"),
         ("d16_volvo_ce", "Volvo D16 16L diesel"),
         ("ev_l25_l120h", "L25 / L120H Electric"),
         ("ev_ec230", "EC230 Electric excavator"),
         ("h2_d13h", "D13H hydrogen-ICE prototype"),
     ],
     transmissions=[
         ("powershift_volvo", "Volvo Powershift"),
         ("optishift", "OptiShift (haulers)"),
     ],
     hu_gens=[("co_pilot_volvo", "Co-Pilot 10\" touchscreen")],
     highlights="D6 to D16 + EC230 / L25 Electric + H2 prototype + OptiShift haulers",
     )


if __name__ == "__main__":
    for path in sorted((REPO / "catalogs" / "agricultural").glob("*.json")):
        print(f"  {path.relative_to(REPO)}: {total_entries(path):,} entries")
