#!/usr/bin/env python3
"""Build the 14 motorcycle catalogs under catalogs/motorcycle/.

Each catalog covers the brand's typical engine families, transmissions,
ECU layout, ride modes, and IMU-driven cornering ABS/TC features —
sized appropriately for motorcycle architectures (single ECU + cluster
+ ABS + ride mode coordinator + IMU + TFT dash).
"""
from __future__ import annotations
import sys, json
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from build_smaller_vehicle import emit, total_entries  # noqa: E402

REPO = Path(__file__).parent.parent

# Common motorcycle ECU template — about 25 ECUs.
def moto_ecus(extras: list[tuple[str, str, str]] = None) -> list[tuple[str, str, str]]:
    base = [
        ("0x7E0", "ecm", "Engine Control"),
        ("0x7E1", "tcm", "Quickshifter / DCT (where fitted)"),
        ("0x710", "abs", "ABS / Cornering ABS"),
        ("0x711", "imu", "IMU 6-axis"),
        ("0x712", "tc", "Traction Control"),
        ("0x713", "wheelie", "Wheelie / Lift control"),
        ("0x714", "tpms", "TPMS"),
        ("0x715", "ride_modes", "Ride mode coordinator"),
        ("0x716", "semi_active", "Semi-active suspension"),
        ("0x717", "launch", "Launch control"),
        ("0x718", "engine_brake", "Engine-brake control"),
        ("0x719", "anti_jerk", "Anti-jerk / slide control"),
        ("0x720", "cluster", "TFT cluster"),
        ("0x721", "head_unit", "Connected app gateway"),
        ("0x722", "key", "Smart key / RFID"),
        ("0x723", "alarm", "Anti-theft"),
        ("0x724", "cruise", "Cruise control"),
        ("0x725", "heated_grips", "Heated grips"),
        ("0x726", "heated_seat", "Heated seat"),
        ("0x727", "led_signature", "LED signature lighting"),
        ("0x728", "horn", "Horn"),
        ("0x729", "fan", "Cooling fan"),
        ("0x72A", "fuel_pump", "Fuel pump"),
        ("0x72B", "battery_monitor", "12V battery monitor"),
        ("0x72C", "tcu", "Telematics"),
    ]
    return base + (extras or [])


# Ducati — DDS / V4 race-DNA bikes
emit("motorcycle", oem_key="ducati", brand_label="Ducati Motor Holding",
     applicable_wmis=["ZDM"],
     source="DDS dealer tool community / Ducatista forums",
     ecus=moto_ecus(),
     engines=[
         ("desmosedici_stradale_v4_1100",
          "Desmosedici Stradale V4 1103cc (Panigale V4 / Streetfighter V4 / Multistrada V4)"),
         ("v2_955_panigale", "Superquadro 955 / V2 Panigale"),
         ("testastretta_evoluzione_1262",
          "Testastretta Evoluzione 1262 (Multistrada / Diavel)"),
         ("testastretta_dvt_937", "Testastretta DVT 937 (Monster / Hypermotard)"),
         ("granturismo_v4_1158", "Granturismo V4 1158 (Diavel V4 / Multistrada V4 RS)"),
     ],
     transmissions=[
         ("dqs_evo_quickshifter", "DQS EVO 2 quickshifter"),
         ("manual_6", "6-speed manual"),
         ("dvt_dual_clutch", "DVT (legacy)"),
     ],
     hu_gens=[
         ("ducati_connect", "Ducati Connect"),
         ("dms_v4_dash", "Multimedia System V4 (TFT)"),
     ],
     act_extra=[
         ("0x0360", "termignoni_db_killer", "Termignoni dB-killer flap",
          "0x7E0", 5000, "Track use only"),
     ],
     adapt_extra=[
         ("0x0150", "wheelie_aggressiveness", "DWC Wheelie Control level",
          "0x713", "uint8", 0, 8, 5, "step"),
         ("0x0151", "engine_brake", "EBC level", "0x718", "uint8", 0, 3, 1, "step"),
         ("0x0152", "ducati_quick_shift", "DQS up/down enable", "0x7E1",
          "uint8", 0, 2, 2, ""),
     ],
     highlights="V4 race + DVT variable timing + DWC + EBC + DQS",
     )

# Harley-Davidson
emit("motorcycle", oem_key="harley-davidson", brand_label="Harley-Davidson",
     applicable_wmis=["1HD", "5HD"],
     source="Digital Tech II / community tuners",
     ecus=moto_ecus(),
     engines=[
         ("milwaukee_eight_107", "Milwaukee-Eight 107 (1746cc)"),
         ("milwaukee_eight_114", "Milwaukee-Eight 114 (1868cc)"),
         ("milwaukee_eight_117", "Milwaukee-Eight 117 (1923cc — CVO)"),
         ("revolution_max_1250", "Revolution Max 1250 (Pan America / Sportster S)"),
         ("revolution_max_975", "Revolution Max 975T (Nightster)"),
         ("livewire_one", "LiveWire One AC permanent-magnet"),
     ],
     transmissions=[("cruise_drive_6", "Cruise Drive 6-speed")],
     hu_gens=[("boom_box_gts", "Boom Box GTS infotainment")],
     adapt_extra=[
         ("0x0150", "rdrs_default", "RDRS default mode", "0x715",
          "uint8", 0, 4, 0, ""),
     ],
     highlights="Milwaukee-Eight + Revolution Max + LiveWire EV",
     )

# Triumph
emit("motorcycle", oem_key="triumph", brand_label="Triumph Motorcycles",
     applicable_wmis=["SMT"],
     source="TuneECU community / Triumph dealer tool",
     ecus=moto_ecus(),
     engines=[
         ("triple_675", "675cc inline-triple"),
         ("triple_765", "765cc Moto2-spec inline-triple (Daytona / Street Triple)"),
         ("triple_1050", "1050cc Speed Triple"),
         ("twin_900", "900cc parallel-twin (Street Twin / Bonneville)"),
         ("twin_1200", "1200cc parallel-twin (T120 / Speedmaster)"),
         ("triple_2500", "2500cc Rocket 3"),
         ("trident_660_triple", "660cc inline-triple (Trident 660)"),
     ],
     transmissions=[("manual_6", "6-speed manual"),
                     ("triumph_shift_assist", "Triumph Shift Assist")],
     hu_gens=[("triumph_connectivity_tft", "Triumph Connectivity TFT")],
     highlights="Inline-triple heritage + Rocket 3 2.5L",
     )

# BMW Motorrad
emit("motorcycle", oem_key="bmw-motorrad", brand_label="BMW Motorrad",
     applicable_wmis=["WB1", "WB2", "ZBM"],
     source="GS-911 / Motoscan community",
     ecus=moto_ecus([
         ("0x72E", "ess", "Engine Smoke Sensor (Telelever)"),
         ("0x72F", "esa", "Electronic Suspension Adjustment (ESA Pro)"),
     ]),
     engines=[
         ("boxer_r1300_1300", "ShiftCam Boxer 1300 (R 1300 GS)"),
         ("boxer_r1250_1254", "ShiftCam Boxer 1254 (R 1250 GS)"),
         ("boxer_r1200_legacy", "Boxer 1170 (R 1200 GS legacy)"),
         ("inline4_s1000_999", "S 1000 RR / S 1000 R (999cc inline-4)"),
         ("inline6_k1600_1649", "K 1600 GT / GTL inline-6"),
         ("single_g310_313", "G 310 R/GS 313cc"),
         ("ce04", "CE 04 e-scooter motor"),
     ],
     transmissions=[("manual_6_msa", "6MT + Shift Assistant Pro"),
                     ("automated_amt", "Automated Shift Assistant (R 1300)"),
                     ("ev_1speed", "CE 04 single-speed reducer")],
     hu_gens=[("connectivity_10_25", "Connectivity 10.25\" TFT"),
               ("bmw_motorrad_app", "BMW Motorrad Connected app")],
     adapt_extra=[
         ("0x0150", "rain_default", "Rain mode default", "0x715",
          "uint8", 0, 1, 0, ""),
         ("0x0151", "esa_default", "ESA Pro default", "0x72F",
          "uint8", 0, 4, 1, ""),
     ],
     highlights="ShiftCam Boxer + S 1000 RR + Inline-6 K 1600 + CE 04 EV",
     )

# KTM
emit("motorcycle", oem_key="ktm", brand_label="KTM AG",
     applicable_wmis=["VBK"],
     source="KTM Tune-ECU community",
     ecus=moto_ecus(),
     engines=[
         ("lc8c_790_799", "LC8c 799cc parallel-twin"),
         ("lc8_v2_1301", "LC8 V2 1301cc (1290 Super Duke / 1290 Super Adventure)"),
         ("rc16_v4_1099", "RC16 V4 1099cc (RC 16 superbike platform)"),
         ("single_690_690", "690 LC4 single"),
         ("single_390_398", "390 Duke / RC 390 — 398cc"),
         ("twin_890_889", "890 Duke / Adventure"),
         ("twin_990_947", "990 Duke parallel-twin"),
     ],
     transmissions=[("manual_6", "6MT"), ("pasc_quickshifter", "PASC Quickshifter+")],
     hu_gens=[("ktm_connect", "KTM Connect TFT")],
     highlights="LC8c parallel-twin + LC8 V2 + 990 Duke",
     )

# Yamaha Motorcycle
emit("motorcycle", oem_key="yamaha-moto", brand_label="Yamaha Motor (motorcycles)",
     applicable_wmis=["JYA", "JYE", "VTL"],
     source="YDS / Yamaha Diagnostic System community",
     ecus=moto_ecus(),
     engines=[
         ("cp4_yzf_999", "CP4 cross-plane 999cc (R1 / R1M)"),
         ("cp3_mt_889", "CP3 cross-plane triple 889cc (MT-09 / Tracer 9 / XSR900)"),
         ("cp2_mt_689", "CP2 parallel-twin 689cc (MT-07 / Ténéré 700)"),
         ("blue_core_155", "Blue Core 155 (Aerox / NMAX 155)"),
         ("vmax_1679_v4", "V-Max 1679 V4"),
         ("xsr_125_124", "XSR 125 single"),
         ("yz450f_450", "YZ 450F single MX (track)"),
     ],
     transmissions=[("manual_6", "6MT"), ("ymsc", "Y-MSC Quickshifter (R1)"),
                     ("yccs", "YCC-S electronic shift")],
     hu_gens=[("y_connect", "Y-Connect TFT")],
     highlights="Cross-plane CP4 R1 + CP3 MT-09 triple + CP2 MT-07",
     )

# Honda Motorcycle
emit("motorcycle", oem_key="honda-moto", brand_label="Honda Motor (motorcycles)",
     applicable_wmis=["JH2", "JH4", "MLH"],
     source="HDS-Moto community / community tuners",
     ecus=moto_ecus([
         ("0x72D", "dct", "Dual-Clutch Transmission ECU"),
     ]),
     engines=[
         ("rc213v_s_v4_1000", "RC213V-S V4 999cc (race-derived)"),
         ("cbr1000rr_r_999", "CBR1000RR-R Fireblade 999cc inline-4"),
         ("cbr650r_649", "CBR 650R inline-4"),
         ("twin_750_745", "NC 750 X parallel-twin"),
         ("crf_450r_450", "CRF 450R single MX"),
         ("rebel_1100_1083", "Rebel 1100 parallel-twin"),
         ("twin_500_471", "CB 500 X parallel-twin"),
         ("v4_africa_twin_1084", "Africa Twin 1084 parallel-twin"),
         ("inline4_goldwing_1833", "Goldwing 1833 flat-six"),
     ],
     transmissions=[
         ("manual_6", "6MT"),
         ("dct_6", "Honda DCT 6-speed"),
         ("hsl_quickshifter", "HSL Quickshifter (CBR1000RR-R SP)"),
     ],
     hu_gens=[("honda_roadsync", "Honda RoadSync TFT")],
     highlights="DCT (Africa Twin/NC) + Goldwing flat-six + CBR-RR-R",
     )

# Kawasaki
emit("motorcycle", oem_key="kawasaki", brand_label="Kawasaki Heavy Industries",
     applicable_wmis=["JKA", "JKB"],
     source="KDS / Kawasaki Diagnostic System community",
     ecus=moto_ecus([
         ("0x72D", "supercharger", "Supercharger control (H2 / H2 SX / H2R)"),
     ]),
     engines=[
         ("h2r_supercharged_998", "H2R Supercharged 998cc inline-4 (310 hp track)"),
         ("h2_supercharged_998", "H2 / H2 SX Supercharged 998cc"),
         ("zx10r_inline4_998", "ZX-10R 998cc inline-4"),
         ("zx6r_inline4_636", "ZX-6R 636cc inline-4"),
         ("z900_948", "Z 900 inline-4"),
         ("twin_650_649", "Z 650 / Ninja 650 parallel-twin"),
         ("ninja_400_399", "Ninja 400 parallel-twin"),
         ("ninja_h2sx_998", "Ninja H2 SX SE supercharged sport-tourer"),
         ("eliminator_451", "Eliminator 451 parallel-twin"),
     ],
     transmissions=[("manual_6", "6MT"), ("quickshifter_kqs", "KQS Quickshifter")],
     hu_gens=[("rideology", "Rideology The App TFT")],
     act_extra=[
         ("0x0360", "supercharger_test", "Supercharger spool test",
          "0x72D", 5000, "Wheels off"),
     ],
     highlights="Supercharged H2 / H2R / H2 SX + ZX-10R",
     )

# Suzuki Motorcycle
emit("motorcycle", oem_key="suzuki-moto", brand_label="Suzuki Motor (motorcycles)",
     applicable_wmis=["JS1", "VTT"],
     source="SDS / Suzuki Diagnostic System community",
     ecus=moto_ecus(),
     engines=[
         ("gsx_r1000_999", "GSX-R1000 999cc inline-4"),
         ("gsx_8s_776_twin", "GSX-8S parallel-twin 776cc"),
         ("v_strom_1050_1037", "V-Strom 1050 V-twin"),
         ("hayabusa_1340", "Hayabusa 1340 inline-4"),
         ("dr_z400_398", "DR-Z 400 single dual-sport"),
         ("burgman_400_400", "Burgman 400 scooter"),
         ("gsx_s_750_749", "GSX-S 750 inline-4"),
         ("inazuma_250_249", "Inazuma 250 parallel-twin"),
     ],
     transmissions=[("manual_6", "6MT"), ("scem_quickshifter", "SCEM Quickshifter")],
     hu_gens=[("suzuki_intelligent_ride", "Suzuki Intelligent Ride System TFT")],
     highlights="Hayabusa 1340 + GSX-R1000 + V-Strom 1050",
     )

# Indian Motorcycle
emit("motorcycle", oem_key="indian-motorcycle", brand_label="Indian Motorcycle (Polaris)",
     applicable_wmis=["56K"],
     source="Polaris Digital Wrench community",
     ecus=moto_ecus(),
     engines=[
         ("powerplus_108_1768", "PowerPlus 108 (1768cc V-twin) — Challenger / Pursuit"),
         ("thunderstroke_111_1811", "Thunderstroke 111 (1811cc V-twin) — Roadmaster / Springfield"),
         ("powerplus_112", "PowerPlus 112 (1834cc V-twin)"),
         ("scout_1133", "Scout (1133cc V-twin)"),
         ("ftr_1203_v2", "FTR 1203 V-twin (FTR 1200)"),
         ("chief_111", "Chief 111 V-twin (legacy)"),
     ],
     transmissions=[("manual_6_indian", "Indian 6MT")],
     hu_gens=[("ride_command", "Ride Command 7\" TFT")],
     highlights="PowerPlus 108/112 + FTR 1203 V-twin + Scout",
     )

# Royal Enfield
emit("motorcycle", oem_key="royal-enfield", brand_label="Royal Enfield (Eicher)",
     applicable_wmis=["MEZ"],
     source="Royal Enfield service / community",
     ecus=moto_ecus(),
     engines=[
         ("interceptor_650_648", "Parallel-twin 648cc (Interceptor / Continental GT)"),
         ("classic_350_349", "Single 349cc (Classic 350 / Meteor / Hunter)"),
         ("himalayan_452_452", "Sherpa 450 single (new Himalayan)"),
         ("super_meteor_650", "Super Meteor 650 parallel-twin"),
         ("guerrilla_452", "Guerrilla 450 single"),
     ],
     transmissions=[("manual_6_re", "Royal Enfield 6MT"),
                     ("manual_5_legacy", "Legacy 5MT")],
     hu_gens=[("tripper_dash", "Tripper navigation dash")],
     highlights="Interceptor 650 + Sherpa 450 single",
     )

# MV Agusta
emit("motorcycle", oem_key="mv-agusta", brand_label="MV Agusta (KTM stake)",
     applicable_wmis=["ZCG"],
     source="MV Agusta dealer tool community",
     ecus=moto_ecus(),
     engines=[
         ("triple_800_798", "MV F3 Triple 798cc"),
         ("inline4_brutale_998", "Brutale 1000 inline-4"),
         ("inline4_f4_998", "F4 RC inline-4 (race)"),
         ("triple_supervedoce_798", "Superveloce 800 triple"),
         ("triple_dragster_798", "Dragster 800 triple"),
     ],
     transmissions=[("manual_6", "6MT"), ("eas_quickshifter", "EAS Quickshifter")],
     hu_gens=[("mv_agusta_app", "MV Agusta connected app")],
     highlights="F3 800 triple + Brutale 1000 + F4 RC",
     )

# Aprilia
emit("motorcycle", oem_key="aprilia", brand_label="Aprilia (Piaggio)",
     applicable_wmis=["ZD4"],
     source="Aprilia dealer tool community",
     ecus=moto_ecus(),
     engines=[
         ("rsv4_v4_1099", "RSV4 V4 1099cc (RSV4 1100)"),
         ("tuono_v4_1077", "Tuono V4 1077cc"),
         ("rs660_twin_659", "Tuono / RS 660 parallel-twin"),
         ("shiver_900_896", "Shiver / Dorsoduro 900 V-twin"),
         ("caponord_1200", "Caponord 1200 V-twin (legacy)"),
     ],
     transmissions=[("manual_6", "6MT"), ("aqs", "AQS Quickshifter")],
     hu_gens=[("aprilia_mia", "Aprilia MIA app TFT")],
     highlights="RSV4 V4 + RS 660 twin + Tuono",
     )

# Husqvarna Motorcycle
emit("motorcycle", oem_key="husqvarna-moto",
     brand_label="Husqvarna Motorcycles (KTM AG)",
     applicable_wmis=["ZBP"],
     source="KTM Tune-ECU shared community",
     ecus=moto_ecus(),
     engines=[
         ("svartpilen_801_799", "Svartpilen 801 parallel-twin (LC8c)"),
         ("vitpilen_401_398", "Vitpilen 401 single"),
         ("701_692", "Husqvarna 701 single (LC4)"),
         ("norden_901_889", "Norden 901 parallel-twin"),
         ("fc450_450", "FC 450 MX single"),
     ],
     transmissions=[("manual_6", "6MT"), ("pasc_qs", "PASC Quickshifter+")],
     hu_gens=[("husqvarna_connect", "Husqvarna Connect")],
     highlights="LC8c shared with KTM + Svartpilen / Vitpilen",
     )


if __name__ == "__main__":
    for path in sorted((REPO / "catalogs" / "motorcycle").glob("*.json")):
        print(f"  {path.relative_to(REPO)}: {total_entries(path):,} entries")
