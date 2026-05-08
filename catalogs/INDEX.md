# Catalog index

Generated 2026-05-08 by `tools/generate_catalog_index.py` (re-run on every release).

## Summary
- **46 OEM catalogs** (`<oem>.json`) — **239,244 total entries**
- **47 DTC catalogs** (`dtc-<oem>.json`) — **1,282 total DTCs**
- **2 universal catalogs** (ISO 15031 / UDS / OBD-II PIDs)

## OEM catalogs

| Brand | Key | WMIs | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Total** |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Volkswagen Audi Group — full diagnostic catalog** <sub>`vw.json`</sub> | `VAG` | WVW, WV1, WV2 +4 | 117 | 3,042 | 536 | 141 | 604 | 340 | 437 | 3,272 | **8,489** |
| **Bayerische Motoren Werke** <sub>`bmw.json`</sub> | `BMW` | WBA, WBS, WBY +3 | 140 | 2,068 | 203 | 43 | 198 | 151 | 69 | 5,060 | **7,932** |
| **Ford Motor Company** <sub>`ford.json`</sub> | `FORD` | 1FA, 1FB, 1FC +12 | 106 | 1,941 | 187 | 37 | 142 | 111 | 40 | 4,820 | **7,384** |
| **Mercedes-Benz Group** <sub>`mercedes.json`</sub> | `MB` | WDB, WDC, WDD +4 | 140 | 2,101 | 164 | 40 | 94 | 130 | 41 | 3,416 | **6,126** |
| **Stellantis (FCA + PSA)** <sub>`stellantis.json`</sub> | `STLA` | 1C3, 1C4, 1C6 +23 | 157 | 2,633 | 159 | 34 | 81 | 168 | 53 | 2,460 | **5,745** |
| **Hyundai Motor Group (Hyundai / Kia / Genesis)** <sub>`hmg.json`</sub> | `HMG` | KMH, KM8, KMF +12 | 143 | 2,481 | 145 | 32 | 76 | 145 | 45 | 2,460 | **5,527** |
| **Jaguar Land Rover Limited (Tata)** <sub>`jlr.json`</sub> | `JLR` | SAJ, SAL, SAD +1 | 144 | 2,396 | 150 | 32 | 75 | 149 | 49 | 2,460 | **5,455** |
| **Dr. Ing. h.c. F. Porsche AG** <sub>`porsche.json`</sub> | `PORSCHE` | WP0, WP1 | 128 | 2,423 | 141 | 31 | 74 | 139 | 40 | 2,460 | **5,436** |
| **General Motors** <sub>`gm.json`</sub> | `GM` | 1G1, 1G2, 1G4 +10 | 132 | 2,242 | 126 | 32 | 78 | 134 | 39 | 2,460 | **5,243** |
| **Toyota Motor Corporation** <sub>`toyota.json`</sub> | `TOYOTA` | JTD, JTE, JTH +13 | 109 | 1,860 | 124 | 31 | 79 | 104 | 38 | 2,804 | **5,149** |
| **Geely Auto / Lynk & Co** <sub>`geely.json`</sub> | `GEELY` | LB3, LFM, LJV +2 | 119 | 2,101 | 109 | 29 | 66 | 126 | 39 | 2,460 | **5,049** |
| **Volvo Cars (Geely)** <sub>`volvo.json`</sub> | `VOLVO` | YV1, YV4, LYV +3 | 117 | 2,078 | 108 | 28 | 68 | 126 | 39 | 2,460 | **5,024** |
| **Renault Group (Renault / Dacia / Alpine)** <sub>`renault.json`</sub> | `RNLT` | VF1, VF2, VS5 +8 | 118 | 2,074 | 110 | 29 | 67 | 126 | 39 | 2,460 | **5,023** |
| **Nissan Motor (incl. Infiniti, Datsun)** <sub>`nissan.json`</sub> | `NISSAN` | JN1, JN6, JN8 +9 | 116 | 2,058 | 109 | 29 | 68 | 127 | 39 | 2,460 | **5,006** |
| **Tata Motors Ltd.** <sub>`tata.json`</sub> | `TATA` | MAT, MAR, KMU | 119 | 2,054 | 107 | 27 | 65 | 124 | 39 | 2,460 | **4,995** |
| **BYD Auto Co. Ltd.** <sub>`byd.json`</sub> | `BYD` | L6T, LGX, 8GA | 122 | 2,030 | 111 | 29 | 68 | 127 | 39 | 2,460 | **4,986** |
| **Mitsubishi Motors Corp.** <sub>`mitsubishi.json`</sub> | `MITSU` | JA3, JA4, JMB +5 | 119 | 2,030 | 109 | 28 | 68 | 126 | 39 | 2,460 | **4,979** |
| **Tesla, Inc.** <sub>`tesla.json`</sub> | `TESLA` | 5YJ, LRW, XP7 +1 | 127 | 2,005 | 114 | 30 | 70 | 132 | 39 | 2,460 | **4,977** |
| **Great Wall Motor (Haval / WEY / ORA / Tank / Poer)** <sub>`gwm.json`</sub> | `GWM` | LGW, LGE, LGT +1 | 119 | 2,020 | 109 | 28 | 65 | 125 | 39 | 2,460 | **4,965** |
| **Cummins Inc. (engine OEM)** <sub>`cummins.json`</sub> | `CUMMINS` |  | 124 | 2,007 | 110 | 27 | 68 | 127 | 39 | 2,460 | **4,962** |
| **Ferrari N.V.** <sub>`ferrari.json`</sub> | `FERRARI` | ZFF | 121 | 2,002 | 110 | 29 | 68 | 127 | 39 | 2,460 | **4,956** |
| **Aston Martin Lagonda Ltd.** <sub>`aston-martin.json`</sub> | `ASTON_MARTIN` | SCF | 118 | 2,009 | 108 | 27 | 66 | 126 | 39 | 2,460 | **4,953** |
| **Mazda Motor Corporation** <sub>`mazda.json`</sub> | `MAZDA` | JM1, JM3, JM7 +3 | 114 | 2,008 | 111 | 28 | 66 | 126 | 39 | 2,460 | **4,952** |
| **Mahindra & Mahindra Ltd.** <sub>`mahindra.json`</sub> | `MAHINDRA` | MAJ, MA6, M3M | 119 | 2,005 | 106 | 28 | 65 | 124 | 39 | 2,460 | **4,946** |
| **Suzuki Motor Corp. (incl. Maruti Suzuki India)** <sub>`suzuki.json`</sub> | `SUZUKI` | JS1, JS2, JSA +6 | 120 | 2,004 | 104 | 28 | 65 | 125 | 39 | 2,460 | **4,945** |
| **MINI (BMW Group sub-brand)** <sub>`mini.json`</sub> | `MINI` | WMW, SAW | 127 | 1,983 | 108 | 28 | 66 | 126 | 39 | 2,460 | **4,937** |
| **PACCAR Inc. (Peterbilt / Kenworth / DAF / Leyland)** <sub>`paccar.json`</sub> | `PACCAR` | 1XP, 1NP, 5KJ +5 | 129 | 1,982 | 110 | 27 | 65 | 125 | 39 | 2,460 | **4,937** |
| **Volvo Group (Volvo Trucks / Mack / Renault Trucks)** <sub>`volvotrucks.json`</sub> | `VOLVOTR` | 4V4, YV2, 4V2 +6 | 133 | 1,969 | 111 | 27 | 65 | 125 | 39 | 2,460 | **4,929** |
| **NIO Inc.** <sub>`nio.json`</sub> | `NIO` | LJN, LBL | 122 | 1,967 | 112 | 29 | 68 | 127 | 39 | 2,460 | **4,924** |
| **Subaru Corporation** <sub>`subaru.json`</sub> | `SUBARU` | JF1, JF2, JF3 +2 | 124 | 1,961 | 111 | 29 | 68 | 126 | 39 | 2,460 | **4,918** |
| **Isuzu Motors Ltd.** <sub>`isuzu.json`</sub> | `ISUZU` | JAA, JAB, JAL +4 | 128 | 1,958 | 109 | 27 | 65 | 125 | 39 | 2,460 | **4,911** |
| **Polestar Performance AB (Geely)** <sub>`polestar.json`</sub> | `POLESTAR` | LPS, LFP | 117 | 1,970 | 107 | 27 | 66 | 125 | 39 | 2,460 | **4,911** |
| **Iveco S.p.A. (Iveco Group)** <sub>`iveco.json`</sub> | `IVECO` | ZCF, VCF | 127 | 1,957 | 110 | 27 | 65 | 125 | 39 | 2,460 | **4,910** |
| **Xpeng Motors (XPeng Inc.)** <sub>`xpeng.json`</sub> | `XPENG` | LJY, LMZ | 121 | 1,950 | 111 | 29 | 68 | 128 | 39 | 2,460 | **4,906** |
| **Automobile Dacia SA (Renault Group)** <sub>`dacia.json`</sub> | `DACIA` | UU1, UU3, LBR +1 | 120 | 1,963 | 106 | 27 | 65 | 125 | 39 | 2,460 | **4,905** |
| **Bentley Motors Ltd.** <sub>`bentley.json`</sub> | `BENTLEY` | SCB | 120 | 1,955 | 110 | 28 | 66 | 125 | 39 | 2,460 | **4,903** |
| **Lucid Group, Inc.** <sub>`lucid.json`</sub> | `LUCID` | 50A | 119 | 1,941 | 107 | 28 | 67 | 126 | 39 | 2,460 | **4,887** |
| **Rivian Automotive, Inc.** <sub>`rivian.json`</sub> | `RIVIAN` | 7PD | 121 | 1,931 | 109 | 29 | 68 | 127 | 39 | 2,460 | **4,884** |
| **AvtoVAZ (Lada)** <sub>`lada.json`</sub> | `LADA` | XTA, XTC, XTV | 120 | 1,938 | 107 | 27 | 65 | 124 | 39 | 2,460 | **4,880** |
| **McLaren Automotive Ltd.** <sub>`mclaren.json`</sub> | `MCLAREN` | SBM | 123 | 1,923 | 111 | 28 | 68 | 127 | 39 | 2,460 | **4,879** |
| **smart Automobile Co. (Mercedes-Geely JV)** <sub>`smart.json`</sub> | `SMART` | WME, L7M | 117 | 1,932 | 104 | 27 | 65 | 125 | 39 | 2,460 | **4,869** |
| **Rolls-Royce Motor Cars Ltd.** <sub>`rolls-royce.json`</sub> | `ROLLS_ROYCE` | SCA | 132 | 1,900 | 111 | 30 | 68 | 128 | 39 | 2,460 | **4,868** |
| **MAN Truck & Bus SE (Traton Group)** <sub>`man.json`</sub> | `MAN` | WMA, 9BW | 130 | 1,911 | 108 | 27 | 65 | 125 | 39 | 2,460 | **4,865** |
| **Scania AB (Traton Group)** <sub>`scania.json`</sub> | `SCANIA` | VLU, YS2, XLE +1 | 130 | 1,912 | 108 | 27 | 65 | 124 | 39 | 2,460 | **4,865** |
| **Detroit Diesel Corp. (Daimler Truck — engine OEM)** <sub>`detroit.json`</sub> | `DDC` |  | 123 | 1,904 | 108 | 27 | 65 | 125 | 39 | 2,460 | **4,851** |
| **Honda Motor Co. (incl. Acura)** <sub>`honda.json`</sub> | `HONDA` | JHM, JHL, JHF +11 | 91 | 1,782 | 110 | 28 | 64 | 93 | 33 | 1,900 | **4,101** |

## DTC catalogs

| File | Name | Default source | Entries |
|---|---|---|---:|
| `dtc-iso-15031.json` | ISO 15031-6 / SAE J2012 generic DTCs | iso-15031-6 | 528 |
| `dtc-lucid.json` | Lucid DTC starter | Lucid community DTCs | 33 |
| `dtc-dacia.json` | Dacia DTC starter | Dacia CLIP shared with Renault DTCs | 29 |
| `dtc-rolls-royce.json` | Rolls-Royce DTC starter | Rolls-Royce BMW ISTA shared DTCs | 28 |
| `dtc-bentley.json` | Bentley DTC starter | Bentley VW Group ODIS shared / VAGCAN DTCs | 27 |
| `dtc-porsche.json` | Porsche DTC starter (PIWIS) | Porsche PIWIS-community / 911uk / Rennlist DTCs | 27 |
| `dtc-tata.json` | Tata Motors DTC starter | Tata service community DTCs | 27 |
| `dtc-lada.json` | Lada / AvtoVAZ DTC starter | Lada OPEN diag / Renolink DTCs | 26 |
| `dtc-jlr.json` | Jaguar Land Rover DTC starter | JLR SDD / Pathfinder community DTCs | 25 |
| `dtc-mahindra.json` | Mahindra DTC starter | Mahindra service community DTCs | 25 |
| `dtc-mclaren.json` | McLaren DTC starter | McLaren MDS community DTCs | 25 |
| `dtc-mini.json` | MINI (BMW Group sub-brand) DTC starter | Mini BMW ISTA / Mini Connected DTCs | 24 |
| `dtc-aston-martin.json` | Aston Martin DTC starter | Aston Martin AMDS community DTCs | 23 |
| `dtc-smart.json` | smart Automobile DTC starter (Mercedes-Geely JV) | smart XENTRY / Geely diag DTCs | 23 |
| `dtc-vw.json` | VW Audi Group DTC starter (P1 / B1 / U1 ranges) | VW Group P-codes (VAG TPi-derived community) | 22 |
| `dtc-ferrari.json` | Ferrari DTC starter (SD3 / Leonardo) | Ferrari SD3 / Leonardo community DTCs | 19 |
| `dtc-isuzu.json` | Isuzu DTC starter (IDSS-II) | Isuzu IDSS community DTCs | 19 |
| `dtc-polestar.json` | Polestar DTC starter | Polestar OC / VIDA shared community DTCs | 17 |
| `dtc-rivian.json` | Rivian DTC starter | Rivian Service Mode community DTCs | 17 |
| `dtc-bmw.json` | BMW DTC starter (DME / DSC / CAS / KOMBI common codes) | BMW E-Sys / NCS-Expert community DTCs | 16 |
| `dtc-ford.json` | Ford DTC starter (FDRS / IDS common P1 / B / U codes) | Ford FORScan / Motorcraft community DTCs | 16 |
| `dtc-gm.json` | GM DTC starter (Tech 2 / GDS-2 common P1 / B / U codes) | GM GDS2 / Tech2 community DTCs | 16 |
| `dtc-iveco.json` | Iveco DTC starter (J1939 SPN-FMI + EASY) | Iveco E.A.S.Y community DTCs | 16 |
| `dtc-mercedes.json` | Mercedes-Benz DTC starter (XENTRY common P1 / B / U codes) | Mercedes Vediamo / Carly community DTCs | 16 |
| `dtc-stellantis.json` | Stellantis DTC starter (DiagBox / wiTech common P1 / B / U codes) | Stellantis wiTech / Mopar / Multiecuscan community DTCs | 15 |
| `dtc-toyota.json` | Toyota / Lexus DTC starter | Toyota Techstream / HSD community DTCs | 15 |
| `dtc-hmg.json` | Hyundai / Kia / Genesis DTC starter | HMG GDS / KDS / E-GMP community DTCs | 14 |
| `dtc-honda.json` | Honda / Acura DTC starter | Honda HDS / Honda-Tech community DTCs | 14 |
| `dtc-cummins.json` | Cummins DTC starter (J1939 SPN-FMI) | Cummins INSITE community SPN/FMI DTCs | 11 |
| `dtc-nissan.json` | Nissan / Infiniti DTC starter | Nissan CONSULT III+ community DTCs | 11 |
| `dtc-renault.json` | Renault / Dacia / Alpine DTC starter | Renault CLIP / Renolink / Pyren / Ddt4all DTCs | 11 |
| `dtc-tesla.json` | Tesla DTC starter | Tesla Service Mode / TeslaScan community DTCs | 11 |
| `dtc-volvo.json` | Volvo Cars DTC starter | Volvo VIDA / DiCE community DTCs | 11 |
| `dtc-detroit.json` | Detroit Diesel DTC starter (J1939 SPN-FMI + GHG17) | Detroit DDDL community SPN/FMI DTCs | 10 |
| `dtc-man.json` | MAN Truck & Bus DTC starter (J1939 SPN-FMI + MAN-cats) | MAN-cats II community SPN/FMI DTCs | 10 |
| `dtc-mazda.json` | Mazda DTC starter | Mazda M-MDS community DTCs | 10 |
| `dtc-volvotrucks.json` | Volvo Trucks / Mack / Renault Trucks DTC starter | Volvo Trucks Tech Tool community SPN/FMI DTCs | 10 |
| `dtc-geely.json` | Geely / Lynk & Co DTC starter | Geely Holding (Lynk & Co / Zeekr / Lotus / Galaxy) DTCs | 9 |
| `dtc-mitsubishi.json` | Mitsubishi Motors DTC starter | Mitsubishi MUT-III community DTCs | 9 |
| `dtc-paccar.json` | PACCAR DTC starter (Peterbilt / Kenworth / DAF) | PACCAR ESA / Davie community SPN/FMI DTCs | 9 |
| `dtc-scania.json` | Scania DTC starter (J1939 SPN-FMI + SDP3) | Scania SDP3 community SPN/FMI DTCs | 9 |
| `dtc-subaru.json` | Subaru DTC starter | Subaru SSM / FreeSSM community DTCs | 9 |
| `dtc-xpeng.json` | Xpeng DTC starter | XPeng XPILOT community DTCs | 9 |
| `dtc-byd.json` | BYD Auto DTC starter | BYD service / e-Platform community DTCs | 8 |
| `dtc-nio.json` | NIO DTC starter | NIO Banyan / NIO House community DTCs | 8 |
| `dtc-suzuki.json` | Suzuki / Maruti DTC starter | Suzuki SDT community DTCs | 8 |
| `dtc-gwm.json` | Great Wall Motor DTC starter | GWM (Haval / Wey / Tank / ORA / Poer) DTCs | 7 |

## Universal / standards catalogs

| File | Name | DIDs | Routines | Total |
|---|---|---:|---:|---:|
| `obd2-pids.json` |  | 84 | 0 | 84 |
| `uds-standard.json` |  | 31 | 4 | 35 |
