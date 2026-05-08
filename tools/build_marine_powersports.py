#!/usr/bin/env python3
"""Build the 6 marine + 5 powersports catalogs.

Marine OEMs use SmartCraft (NMEA 2000-derived) for inboards / sterndrives
and J1939 for commercial diesel; powersports OEMs (Polaris / BRP / Arctic
Cat / Yamaha WaveRunner / Kawasaki Jet Ski) use OEM-specific CAN that
mirrors motorcycle ECUs but with different sensor sets (jet pump,
trim, thrust)."""
from __future__ import annotations
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from build_smaller_vehicle import emit, total_entries  # noqa: E402

REPO = Path(__file__).parent.parent


def marine_ecus():
    return [
        ("0x7E0", "ecm", "Engine Control Module"),
        ("0x7E1", "tcm", "Transmission / drive control"),
        ("0x7E2", "trim", "Trim + tilt actuator"),
        ("0x7E3", "joystick", "Joystick / Skyhook coordinator"),
        ("0x7E4", "battery_lithium", "Lithium house bank monitor"),
        ("0x7E5", "shore_power", "Shore-power converter / inverter"),
        ("0x7E6", "fuel_level", "Fuel + DEF level senders"),
        ("0x7E7", "engine_alarm", "Engine alarm / oil pressure / temp"),
        ("0x7E8", "vesselview", "VesselView / Volvo Glass Cockpit display"),
        ("0x7E9", "telemetry", "Marine telemetry (Mercury Vessel-net / Volvo Connect)"),
        ("0x7EA", "navigation", "Chartplotter integration / NMEA 2000 gateway"),
        ("0x710", "abs_drag_brake", "Drag-brake (jet inboards)"),
        ("0x711", "imu", "Marine IMU + gyro"),
        ("0x712", "cluster", "Helm cluster / SmartCraft display"),
        ("0x713", "auto_pilot", "Autopilot / Mercury Joystick Piloting"),
        ("0x714", "ev_motor_inverter", "Electric drive motor (Torqeedo / etc.)"),
        ("0x715", "active_trim", "Active Trim / Sport Power Trim"),
        ("0x716", "thruster", "Bow / stern thruster"),
        ("0x717", "windlass", "Anchor windlass control"),
        ("0x718", "horn_nav_lights", "Horn + nav-light controller"),
        ("0x719", "blower", "Engine bay blower"),
        ("0x71A", "bilge_pump", "Bilge pump"),
    ]


# Mercury Marine — SmartCraft standard
emit("marine", oem_key="mercury-marine",
     brand_label="Mercury Marine (Brunswick)",
     applicable_wmis=["MAR"],
     source="CDS G3 / SmartCraft community / NMEA 2000",
     ecus=marine_ecus(),
     engines=[
         ("verado_v8_300", "Verado 300 V8 4.6L"),
         ("verado_v10_400", "Verado 400 V10 7.6L (Sea Pro)"),
         ("verado_v12_600", "Verado 600 V12 7.6L Twin-Prop"),
         ("pro_xs_v8_250", "Pro XS V8 4.6L 250 hp"),
         ("seapro_3_4_v6_200", "SeaPro 3.4L V6 200 hp"),
         ("racing_v8_500r", "Racing 500R V8 4.6L"),
         ("avator_7_5e_electric", "Avator 7.5e electric"),
         ("avator_20e_electric", "Avator 20e electric"),
         ("avator_35e_electric", "Avator 35e electric"),
         ("mercruiser_v8_6_2", "MerCruiser 6.2L MPI sterndrive"),
         ("mercruiser_8_2", "MerCruiser 8.2L MAG sterndrive"),
         ("4_5l_diesel_tdi", "MerCruiser 4.5L TDI diesel"),
     ],
     transmissions=[
         ("verado_2_speed", "Verado 2-speed transmission"),
         ("bravo_one_xr", "Bravo One XR sterndrive"),
         ("bravo_three", "Bravo Three counter-rotating sterndrive"),
         ("ev_2_speed_avator", "Avator 2-speed reducer"),
     ],
     hu_gens=[("vesselview_902_1303", "VesselView 902/1303 multi-function display"),
               ("vesselview_link", "VesselView Link mobile gateway")],
     act_extra=[
         ("0x0360", "skyhook_engage", "Skyhook GPS station-keep",
          "0x7E3", 30000, "Engines warmed + clear water"),
         ("0x0361", "active_trim_default", "Active Trim default test",
          "0x715", 5000, ""),
     ],
     adapt_extra=[
         ("0x0150", "skyhook_default", "Skyhook default", "0x7E3",
          "uint8", 0, 1, 0, ""),
         ("0x0151", "joystick_sensitivity", "Joystick sensitivity",
          "0x7E3", "uint8", 0, 5, 3, "step"),
     ],
     highlights="Verado V12 600hp + Avator electric outboard + Skyhook + Active Trim",
     per_cell_count=64, per_module_count=8, max_cyls=12)

# Volvo Penta — VODIA dealer tool
emit("marine", oem_key="volvo-penta",
     brand_label="Volvo Penta",
     applicable_wmis=["VPE"],
     source="VODIA dealer tool community / NMEA 2000",
     ecus=marine_ecus(),
     engines=[
         ("d4_v6_diesel_300", "D4 3.7L V6 diesel"),
         ("d6_v6_diesel_440", "D6 5.5L V6 diesel"),
         ("d8_inline6_550", "D8 7.7L I6 diesel"),
         ("d11_inline6", "D11 10.8L I6 diesel"),
         ("d13_inline6_900", "D13 12.8L I6 900 hp diesel"),
         ("d16_inline6", "D16 16.1L I6 diesel"),
         ("v6_280_petrol", "5.3L V6 petrol gasoline"),
         ("v8_350_petrol", "5.7L V8 petrol gasoline"),
         ("ips_drive_v8_diesel", "IPS pod-drive V8 diesel package"),
         ("ev_volvo_penta_marine", "Volvo Penta marine electric driveline"),
     ],
     transmissions=[
         ("ips_pod", "IPS Pod Drive"),
         ("aquamatic_dpr", "Aquamatic DPR sterndrive"),
         ("forward_drive", "Forward Drive (FWD-rotation propellers)"),
         ("inboard_zf", "ZF inboard reduction (1.5:1 / 2.0:1)"),
     ],
     hu_gens=[("glass_cockpit_volvo", "Volvo Penta Glass Cockpit"),
               ("ips_joystick", "IPS Joystick + Dynamic Positioning")],
     highlights="D4 to D16 inline-6 + IPS Pod + Forward Drive + Volvo Penta marine EV",
     )

# Yanmar Marine
emit("marine", oem_key="yanmar-marine",
     brand_label="Yanmar Marine",
     applicable_wmis=["YAN"],
     source="YDIS community / NMEA 2000",
     ecus=marine_ecus(),
     engines=[
         ("4lv_4_4l", "4LV 4.4L 4-cyl diesel"),
         ("8lv_5_5l_v8", "8LV 5.5L V8 diesel"),
         ("6lf_5_8l_inline6", "6LF 5.8L I6 diesel"),
         ("6ly_5_8l_inline6_high", "6LY 5.8L I6 high-output"),
         ("6ay_8_4l_inline6", "6AY 8.4L I6 diesel"),
         ("6ey_18_inline6_18l", "6EY 18L I6 commercial"),
         ("dtorque_111_legacy", "dtorque 111 turbodiesel outboard (legacy)"),
     ],
     transmissions=[
         ("kanzaki_kmh4a", "Kanzaki KMH4A reduction"),
         ("zf_85_marine", "ZF 85 marine reduction"),
         ("hurth_hsw_630", "ZF Hurth HSW 630 marine gearbox"),
     ],
     hu_gens=[("yanmar_can_display_color", "Yanmar Can-Bus colour display")],
     highlights="4LV/8LV V8 + 6AY commercial diesel + Kanzaki / ZF reductions",
     )

# MTU (Rolls-Royce Power Systems)
emit("marine", oem_key="mtu",
     brand_label="MTU (Rolls-Royce Power Systems)",
     applicable_wmis=["MTU"],
     source="DiaSys dealer tool community",
     ecus=marine_ecus(),
     engines=[
         ("series_2000_v8", "MTU Series 2000 V8 13.5L diesel"),
         ("series_2000_v12", "MTU Series 2000 V12 23.9L diesel"),
         ("series_4000_v12", "MTU Series 4000 V12 48.7L diesel"),
         ("series_4000_v16", "MTU Series 4000 V16 65.4L diesel"),
         ("series_4000_v20", "MTU Series 4000 V20 81.7L diesel"),
         ("series_8000_v20", "MTU Series 8000 V20 211L diesel (mega-yacht / commercial)"),
         ("16v_956_yacht_class", "16V 956 32.4L Yacht-class"),
     ],
     transmissions=[
         ("zf_3000a", "ZF 3000A transmission"),
         ("zf_4600", "ZF 4600 high-power transmission"),
     ],
     hu_gens=[("blue_vision", "MTU Blue Vision multi-function display"),
               ("engine_management_system", "MTU Engine Management System (EMS)")],
     highlights="Series 2000/4000/8000 V8/V12/V16/V20 + 16V 956 yacht-class",
     )

# Cummins Marine — shares engines with Cummins HD
emit("marine", oem_key="cummins-marine",
     brand_label="Cummins Marine",
     applicable_wmis=["1NE"],
     source="INSITE Marine community + NMEA 2000",
     ecus=marine_ecus(),
     engines=[
         ("qsb_5_9", "QSB 5.9L marine diesel"),
         ("qsl_8_9", "QSL 8.9L marine diesel"),
         ("qsm_11", "QSM 11L marine diesel"),
         ("qsk_19", "QSK 19L marine diesel"),
         ("qsk_38", "QSK 38L V12 marine"),
         ("qsk_50", "QSK 50L V16 marine"),
         ("qsk_60", "QSK 60L V16 marine (commercial)"),
         ("x15_hp_marine", "X15 HP marine variant"),
     ],
     transmissions=[
         ("twin_disc_marine", "Twin Disc marine reduction"),
         ("zf_marine_2000", "ZF Marine 2000-series"),
     ],
     hu_gens=[("smartcraft_cummins", "SmartCraft-Cummins gateway"),
               ("vessel_view_cummins", "VesselView (Cummins-branded)")],
     highlights="QSB/QSL/QSM/QSK + X15 HP marine + SmartCraft gateway",
     )

# Yamaha Marine
emit("marine", oem_key="yamaha-marine",
     brand_label="Yamaha Marine",
     applicable_wmis=["YMM"],
     source="YDS Marine community + NMEA 2000",
     ecus=marine_ecus(),
     engines=[
         ("xto_v8_5_6", "XTO V8 5.6L 425/450 hp outboard"),
         ("v6_4_2_offshore", "4.2L V6 offshore"),
         ("inline4_2_8_f200", "F200 2.8L inline-4"),
         ("inline2_2_2_f25", "F25 2.2L parallel-twin"),
         ("siren_3_0_350h", "350 hp 4.3L V8 (Siren legacy)"),
         ("ev_harmo_300", "Harmo electric (concept)"),
     ],
     transmissions=[
         ("xto_2_speed_yamaha", "XTO 2-speed reducer"),
         ("manual_yamaha", "Manual outboard reduction"),
     ],
     hu_gens=[("command_link_plus", "Command Link Plus display"),
               ("helm_master_ex", "Helm Master EX joystick")],
     highlights="XTO V8 425/450hp + Helm Master EX joystick + Harmo electric",
     )


def powersport_ecus():
    return [
        ("0x7E0", "ecm", "Engine Control Module"),
        ("0x7E1", "tcm", "Transmission / DCT"),
        ("0x710", "abs", "ABS / Cornering ABS"),
        ("0x711", "imu", "IMU 6-axis (where fitted)"),
        ("0x712", "tc", "Traction control"),
        ("0x713", "ride_modes", "Ride mode coordinator"),
        ("0x714", "tpms", "TPMS (UTV)"),
        ("0x715", "shock", "Active suspension (Polaris RIDE COMMAND / Maverick X RS)"),
        ("0x720", "cluster", "TFT cluster"),
        ("0x721", "head_unit", "Connected app gateway"),
        ("0x722", "key", "Smart key / RFID"),
        ("0x723", "alarm", "Anti-theft"),
        ("0x724", "trim", "Trim / Bravo (jet ski)"),
        ("0x725", "auto_throttle", "Auto-throttle / cruise"),
        ("0x726", "winch", "Winch control"),
        ("0x727", "horn", "Horn"),
        ("0x728", "fan", "Cooling fan"),
        ("0x729", "fuel_pump", "Fuel pump"),
        ("0x72A", "battery_monitor", "12V battery monitor"),
        ("0x72B", "telematics_polaris_brp", "Polaris RIDE COMMAND / BRP CONNECT"),
    ]


# Polaris (Polaris Industries — RZR, Ranger, Sportsman, Slingshot)
emit("powersports", oem_key="polaris",
     brand_label="Polaris Industries",
     applicable_wmis=["4XA"],
     source="Polaris Digital Wrench community",
     ecus=powersport_ecus(),
     engines=[
         ("prostar_xp_999_turbo", "ProStar XP 1000 (RZR / Ranger XP 1000) Turbo"),
         ("prostar_2_litre_turbo_r", "ProStar 2.0L Turbo R (RZR Turbo R 1995cc)"),
         ("rotax_900_ace", "Rotax 900 ACE (Slingshot / Sportsman)"),
         ("rotax_525", "Rotax 525 single (Sportsman 570)"),
         ("h2o_pwc", "H2O 998cc 4-stroke triple PWC"),
         ("indy_xc_850", "Indy XC 850 snowmobile parallel-twin"),
         ("ev_ranger_kinetic", "Ranger XP Kinetic Battery-Electric"),
     ],
     transmissions=[
         ("p_dct", "PDCT 7-speed DCT (Indy)"),
         ("cvt_polaris", "Polaris CVT (Sportsman / RZR)"),
         ("manual_5_slingshot", "Slingshot 5MT"),
         ("ev_1speed", "Ranger Kinetic single-speed reducer"),
     ],
     hu_gens=[("ride_command_polaris", "RIDE COMMAND 7\" touchscreen")],
     highlights="ProStar XP/Turbo R + Rotax 900 ACE + Ranger XP Kinetic EV",
     )

# Can-Am / BRP (Bombardier Recreational Products)
emit("powersports", oem_key="can-am-brp",
     brand_label="Can-Am / BRP (Bombardier)",
     applicable_wmis=["3JB"],
     source="B.U.D.S. (Bombardier User Diagnostic System) community",
     ecus=powersport_ecus(),
     engines=[
         ("rotax_v_twin_998_can_am", "Rotax 998cc V-twin (Can-Am Spyder F3 / RT)"),
         ("rotax_900_ace_can_am", "Rotax 900 ACE (Can-Am Spyder Ryker)"),
         ("rotax_1330_ace", "Rotax 1330 ACE inline-3 (Can-Am Spyder F3 RT)"),
         ("rotax_976_pwc", "Rotax 1630 ACE 1500cc 3-cyl (Sea-Doo RXP-X 325)"),
         ("rotax_1630_supercharged", "Rotax 1630 ACE supercharged (Sea-Doo RXT-X 325)"),
         ("rotax_e_tec", "Rotax E-TEC G2 800 (Ski-Doo)"),
         ("rotax_900_ace_turbo_r", "Rotax 900 ACE Turbo R (Maverick R)"),
         ("ev_origin_electric", "Can-Am Origin Pulse electric motorcycle"),
     ],
     transmissions=[
         ("semi_auto_can_am", "Semi-automatic 6-speed (Spyder F3)"),
         ("cvt_brp", "BRP CVT (Maverick Sport / X3)"),
         ("manual_5_skidoo", "5MT (Ski-Doo manual variant)"),
         ("ev_2speed", "Origin Pulse 2-speed reducer"),
     ],
     hu_gens=[("brp_connect", "BRP CONNECT app + 7\" touchscreen")],
     highlights="Rotax V-twin / 1330 ACE / 1630 ACE supercharged Sea-Doo + Origin EV",
     )

# Arctic Cat (Textron)
emit("powersports", oem_key="arctic-cat",
     brand_label="Arctic Cat (Textron)",
     applicable_wmis=["4UF"],
     source="Arctic Cat dealer service community",
     ecus=powersport_ecus(),
     engines=[
         ("c_tec_4_1100", "C-TEC4 1100cc inline-4"),
         ("ctec_2_8000", "C-TEC2 8000 800cc 2-stroke twin (snowmobile)"),
         ("ctec_2_6000", "C-TEC2 6000 600cc 2-stroke twin"),
         ("4_stroke_794", "794cc 4-stroke twin (UTV)"),
         ("ev_blast", "ArcticCat Blast EV (concept)"),
     ],
     transmissions=[
         ("rapid_response_2_cvt", "Rapid Response 2 CVT"),
         ("manual_5_arctic_cat", "Arctic Cat 5MT"),
     ],
     hu_gens=[("arctic_cat_app", "Arctic Cat connected app")],
     highlights="C-TEC4 1100 + C-TEC2 800 2-stroke snowmobile",
     )

# Yamaha WaveRunner
emit("powersports", oem_key="yamaha-waverunner",
     brand_label="Yamaha WaveRunner (Yamaha Marine subdivision)",
     applicable_wmis=["YAM"],
     source="YDS Marine community / NMEA 2000",
     ecus=powersport_ecus(),
     engines=[
         ("svho_1898_supercharged", "Supercharged High Output (SVHO) 1898cc 4-cyl (FX SVHO)"),
         ("ho_1898_naturally_aspirated", "High Output (HO) 1898cc NA"),
         ("svho_1812_legacy", "SVHO 1812cc legacy"),
         ("tr_1_1049", "TR-1 1049cc 4-stroke triple (entry-level)"),
         ("ho_genesis_1812", "Genesis 1812 HO (Wave-Runner GP1800R HO)"),
     ],
     transmissions=[
         ("yamaha_jet_pump", "Yamaha jet pump (3-blade / 4-blade)"),
     ],
     hu_gens=[("connext_yamaha", "Connext touchscreen"),
               ("ride_yamaha", "RiDE dual-throttle handlebar")],
     highlights="SVHO 1898 supercharged + RiDE dual-throttle",
     )

# Kawasaki Jet Ski
emit("powersports", oem_key="kawasaki-jet",
     brand_label="Kawasaki Jet Ski",
     applicable_wmis=["KAW"],
     source="KDS-Marine community",
     ecus=powersport_ecus(),
     engines=[
         ("inline4_1498_supercharged_310", "Supercharged 1498cc inline-4 (Ultra 310)"),
         ("inline4_1498_na_160", "1498cc inline-4 NA (Ultra LX)"),
         ("inline4_1498_jet_ski_str_310x", "Supercharged 1498cc Jet Ski STR 310X"),
         ("inline4_1498_jet_ski_jt310", "Supercharged 1498cc JT310"),
     ],
     transmissions=[("kawasaki_jet_pump", "Kawasaki jet pump")],
     hu_gens=[("smart_steering_helm", "Kawasaki Smart Helm 7\" touchscreen")],
     highlights="Supercharged 1498 inline-4 (Ultra 310 / STR 310X / JT310)",
     )


if __name__ == "__main__":
    print("Marine:")
    for path in sorted((REPO / "catalogs" / "marine").glob("*.json")):
        print(f"  {path.relative_to(REPO)}: {total_entries(path):,} entries")
    print("Powersports:")
    for path in sorted((REPO / "catalogs" / "powersports").glob("*.json")):
        print(f"  {path.relative_to(REPO)}: {total_entries(path):,} entries")
