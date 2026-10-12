# Catalog index

Inventory of the OEM, DTC, universal and range-profile catalogues. This file is
generated from the catalogue files by `python3 tools/catalog_index.py --write`;
do not edit it by hand. CI runs `python3 tools/catalog_index.py` and fails when
this file is out of date. `python3 tools/validate_catalogs.py --require-coverage`
validates the catalogues themselves. All other catalogue files (standard OBD-II /
UDS / J1939 / KWP data, OEM overlays, VIN, EV battery, radio code, adapter,
tachograph, key platforms, service routines and drive cycles) are described in
[`docs/catalogs.md`](../docs/catalogs.md).

How the counts are made:

- An OEM catalogue is a file with the `oem-catalog-v2` schema at the top level of
  `catalogs/` (cars and trucks) or in `agricultural/`, `marine/`, `motorcycle/` or
  `powersports/`. `obd2-pids.json` and `uds-standard.json` are listed as universal
  catalogues, and `test-schema-v2.json` (a schema test fixture) is not counted.
- **Own** is the sum of the catalogue's own `ecus`, `dids`, `routines`,
  `coding_blocks`, `adaptations`, `actuator_tests`, `live_pids` and
  `dtc_extended_data` entries: the brand-specific data. Generic template entries
  live once in a shared base under `_shared/` that the catalogue names in
  `extends`; **Inherited** counts the entries it gets from that base and **Base**
  names it. **Verified** counts own entries marked `"verified": true` (confirmed
  on a vehicle or against a specification).
- DTC catalogues are the top-level `dtc-*.json` files; **Entries** counts `dtcs`.

## Summary
- **79 OEM catalogs** — **64,812 brand-specific entries** (plus 186,545 inherited from 6 shared bases, 0 verified) across 5 vehicle classes:
  - **agricultural**: 8 catalogs / 728 brand-specific entries
  - **cars-and-trucks**: 46 catalogs / 61,834 brand-specific entries
  - **marine**: 6 catalogs / 766 brand-specific entries
  - **motorcycle**: 14 catalogs / 1,108 brand-specific entries
  - **powersports**: 5 catalogs / 376 brand-specific entries
- **47 DTC catalogs** (`dtc-<oem>.json`) — **1,282 total DTCs**
- **2 universal catalogs** (`obd2-pids.json` and `uds-standard.json`)
- **2 range profiles** (`range-profiles/*.json`) — editable normal bands for OBD Studio values
- **6 shared base catalogs** (`_shared/*.json`) — **7,428 template entries** stored once instead of per brand

## Shared base catalogs

| File | Name | Extends | Used by | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Total** |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `_shared/dtc-extended-generic.json` | Shared generic DTC extended-data records (occurrence / aging / mileage / status / freeze-frame per generic DTC) |  | 10 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 2,460 | **2,460** |
| `_shared/oem-agricultural.json` | Shared generic template (Agricultural and construction) |  | 8 | 23 | 74 | 14 | 0 | 12 | 15 | 12 | 0 | **150** |
| `_shared/oem-cars-and-trucks.json` | Shared generic template (Cars and trucks) |  | 36 | 74 | 1,475 | 98 | 26 | 63 | 123 | 39 | 2,460 | **4,358** |
| `_shared/oem-marine.json` | Shared generic template (Marine) |  | 6 | 22 | 74 | 14 | 0 | 12 | 15 | 12 | 4 | **153** |
| `_shared/oem-motorcycle.json` | Shared generic template (Motorcycles) |  | 14 | 25 | 74 | 14 | 0 | 12 | 15 | 12 | 4 | **156** |
| `_shared/oem-powersports.json` | Shared generic template (Powersports) |  | 5 | 20 | 74 | 14 | 0 | 12 | 15 | 12 | 4 | **151** |

## Range profiles

| Profile | File | Vehicle | Source | Ranges |
|---|---|---|---|---:|
| `generic` | `range-profiles/generic.json` | Generic OBD-II vehicle | generic workshop defaults | 15 |
| `vag-16-tdi` | `range-profiles/vag-16-tdi.json` | VW 1.6 TDI (EA189/EA288 era) | generic workshop defaults adjusted for VW/Audi/Skoda/Seat 1.6 TDI | 15 |

## Agricultural

| Brand | Key | WMIs | Base | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Own** | Inherited | Verified |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Caterpillar (Cat)** <sub>`agricultural/caterpillar.json`</sub> | `CATERPILLAR` | CAT | `oem-agricultural.json` | 2 | 112 | 0 | 0 | 0 | 0 | 0 | 4 | **118** | 150 | 0 |
| **AGCO (Massey Ferguson / Fendt / Valtra / Challenger)** <sub>`agricultural/agco.json`</sub> | `AGCO` | 1AG | `oem-agricultural.json` | 1 | 96 | 0 | 0 | 0 | 0 | 0 | 4 | **101** | 150 | 0 |
| **John Deere (Deere & Company)** <sub>`agricultural/john-deere.json`</sub> | `JOHN-DEERE` | 1JD | `oem-agricultural.json` | 0 | 88 | 0 | 0 | 2 | 2 | 0 | 2 | **94** | 150 | 0 |
| **Volvo Construction Equipment** <sub>`agricultural/volvo-ce.json`</sub> | `VOLVO-CE` | VCE | `oem-agricultural.json` | 1 | 88 | 0 | 0 | 0 | 0 | 0 | 4 | **93** | 150 | 0 |
| **CNH Industrial (Case IH / New Holland / Steyr)** <sub>`agricultural/cnh.json`</sub> | `CNH` | 1NH | `oem-agricultural.json` | 0 | 88 | 0 | 0 | 0 | 0 | 0 | 4 | **92** | 150 | 0 |
| **Komatsu** <sub>`agricultural/komatsu.json`</sub> | `KOMATSU` | KMT | `oem-agricultural.json` | 2 | 72 | 0 | 0 | 0 | 0 | 0 | 4 | **78** | 150 | 0 |
| **Claas KGaA** <sub>`agricultural/claas.json`</sub> | `CLAAS` | DEY | `oem-agricultural.json` | 0 | 72 | 0 | 0 | 0 | 0 | 0 | 4 | **76** | 150 | 0 |
| **Kubota Corporation** <sub>`agricultural/kubota.json`</sub> | `KUBOTA` | KUB | `oem-agricultural.json` | 0 | 72 | 0 | 0 | 0 | 0 | 0 | 4 | **76** | 150 | 0 |

## Cars And Trucks

| Brand | Key | WMIs | Base | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Own** | Inherited | Verified |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Volkswagen Audi Group — full diagnostic catalog** <sub>`vw.json`</sub> | `VAG` | WVW, WV1, WV2 +4 | `dtc-extended-generic.json` | 117 | 3,042 | 536 | 141 | 604 | 340 | 437 | 2,310 | **7,527** | 2,460 | 0 |
| **Bayerische Motoren Werke** <sub>`bmw.json`</sub> | `BMW` | WBA, WBS, WBY +3 | `dtc-extended-generic.json` | 140 | 2,068 | 203 | 43 | 198 | 151 | 69 | 3,856 | **6,728** | 2,460 | 0 |
| **Ford Motor Company** <sub>`ford.json`</sub> | `FORD` | 1FA, 1FB, 1FC +12 | `dtc-extended-generic.json` | 106 | 1,941 | 187 | 37 | 142 | 111 | 40 | 2,728 | **5,292** | 2,460 | 0 |
| **Mercedes-Benz Group** <sub>`mercedes.json`</sub> | `MB` | WDB, WDC, WDD +4 | `dtc-extended-generic.json` | 140 | 2,101 | 164 | 40 | 94 | 130 | 41 | 1,124 | **3,834** | 2,460 | 0 |
| **Stellantis (FCA + PSA)** <sub>`stellantis.json`</sub> | `STLA` | 1C3, 1C4, 1C6 +23 | `dtc-extended-generic.json` | 157 | 2,633 | 159 | 34 | 81 | 168 | 53 | 0 | **3,285** | 2,460 | 0 |
| **Hyundai Motor Group (Hyundai / Kia / Genesis)** <sub>`hmg.json`</sub> | `HMG` | KMH, KM8, KMF +12 | `dtc-extended-generic.json` | 143 | 2,481 | 145 | 32 | 76 | 145 | 45 | 0 | **3,067** | 2,460 | 0 |
| **Jaguar Land Rover Limited (Tata)** <sub>`jlr.json`</sub> | `JLR` | SAJ, SAL, SAD +1 | `dtc-extended-generic.json` | 144 | 2,396 | 150 | 32 | 75 | 149 | 49 | 0 | **2,995** | 2,460 | 0 |
| **Dr. Ing. h.c. F. Porsche AG** <sub>`porsche.json`</sub> | `PORSCHE` | WP0, WP1 | `dtc-extended-generic.json` | 128 | 2,423 | 141 | 31 | 74 | 139 | 40 | 0 | **2,976** | 2,460 | 0 |
| **Toyota Motor Corporation** <sub>`toyota.json`</sub> | `TOYOTA` | JTD, JTE, JTH +13 | `dtc-extended-generic.json` | 109 | 1,860 | 124 | 31 | 79 | 104 | 38 | 572 | **2,917** | 2,460 | 0 |
| **Honda Motor Co. (incl. Acura)** <sub>`honda.json`</sub> | `HONDA` | JHM, JHL, JHF +11 | `dtc-extended-generic.json` | 91 | 1,782 | 110 | 28 | 64 | 93 | 33 | 0 | **2,201** | 2,460 | 0 |
| **General Motors** <sub>`gm.json`</sub> | `GM` | 1G1, 1G2, 1G4 +10 | `oem-cars-and-trucks.json` | 58 | 767 | 28 | 6 | 15 | 11 | 0 | 0 | **885** | 4,358 | 0 |
| **Geely Auto / Lynk & Co** <sub>`geely.json`</sub> | `GEELY` | LB3, LFM, LJV +2 | `oem-cars-and-trucks.json` | 45 | 626 | 11 | 3 | 3 | 3 | 0 | 0 | **691** | 4,358 | 0 |
| **Volvo Cars (Geely)** <sub>`volvo.json`</sub> | `VOLVO` | YV1, YV4, LYV +3 | `oem-cars-and-trucks.json` | 43 | 603 | 10 | 2 | 5 | 3 | 0 | 0 | **666** | 4,358 | 0 |
| **Renault Group (Renault / Dacia / Alpine)** <sub>`renault.json`</sub> | `RNLT` | VF1, VF2, VS5 +8 | `oem-cars-and-trucks.json` | 44 | 599 | 12 | 3 | 4 | 3 | 0 | 0 | **665** | 4,358 | 0 |
| **Nissan Motor (incl. Infiniti, Datsun)** <sub>`nissan.json`</sub> | `NISSAN` | JN1, JN6, JN8 +9 | `oem-cars-and-trucks.json` | 42 | 583 | 11 | 3 | 5 | 4 | 0 | 0 | **648** | 4,358 | 0 |
| **Tata Motors Ltd.** <sub>`tata.json`</sub> | `TATA` | MAT, MAR, KMU | `oem-cars-and-trucks.json` | 45 | 579 | 9 | 1 | 2 | 1 | 0 | 0 | **637** | 4,358 | 0 |
| **BYD Auto Co. Ltd.** <sub>`byd.json`</sub> | `BYD` | L6T, LGX, 8GA | `oem-cars-and-trucks.json` | 48 | 555 | 13 | 3 | 5 | 4 | 0 | 0 | **628** | 4,358 | 0 |
| **Mitsubishi Motors Corp.** <sub>`mitsubishi.json`</sub> | `MITSU` | JA3, JA4, JMB +5 | `oem-cars-and-trucks.json` | 45 | 555 | 11 | 2 | 5 | 3 | 0 | 0 | **621** | 4,358 | 0 |
| **Tesla, Inc.** <sub>`tesla.json`</sub> | `TESLA` | 5YJ, LRW, XP7 +1 | `oem-cars-and-trucks.json` | 53 | 530 | 16 | 4 | 7 | 9 | 0 | 0 | **619** | 4,358 | 0 |
| **Great Wall Motor (Haval / WEY / ORA / Tank / Poer)** <sub>`gwm.json`</sub> | `GWM` | LGW, LGE, LGT +1 | `oem-cars-and-trucks.json` | 45 | 545 | 11 | 2 | 2 | 2 | 0 | 0 | **607** | 4,358 | 0 |
| **Cummins Inc. (engine OEM)** <sub>`cummins.json`</sub> | `CUMMINS` |  | `oem-cars-and-trucks.json` | 50 | 532 | 12 | 1 | 5 | 4 | 0 | 0 | **604** | 4,358 | 0 |
| **Ferrari N.V.** <sub>`ferrari.json`</sub> | `FERRARI` | ZFF | `oem-cars-and-trucks.json` | 47 | 527 | 12 | 3 | 5 | 4 | 0 | 0 | **598** | 4,358 | 0 |
| **Aston Martin Lagonda Ltd.** <sub>`aston-martin.json`</sub> | `ASTON_MARTIN` | SCF | `oem-cars-and-trucks.json` | 44 | 534 | 10 | 1 | 3 | 3 | 0 | 0 | **595** | 4,358 | 0 |
| **Mazda Motor Corporation** <sub>`mazda.json`</sub> | `MAZDA` | JM1, JM3, JM7 +3 | `oem-cars-and-trucks.json` | 40 | 533 | 13 | 2 | 3 | 3 | 0 | 0 | **594** | 4,358 | 0 |
| **Mahindra & Mahindra Ltd.** <sub>`mahindra.json`</sub> | `MAHINDRA` | MAJ, MA6, M3M | `oem-cars-and-trucks.json` | 45 | 530 | 8 | 2 | 2 | 1 | 0 | 0 | **588** | 4,358 | 0 |
| **Suzuki Motor Corp. (incl. Maruti Suzuki India)** <sub>`suzuki.json`</sub> | `SUZUKI` | JS1, JS2, JSA +6 | `oem-cars-and-trucks.json` | 46 | 529 | 6 | 2 | 2 | 2 | 0 | 0 | **587** | 4,358 | 0 |
| **MINI (BMW Group sub-brand)** <sub>`mini.json`</sub> | `MINI` | WMW, SAW | `oem-cars-and-trucks.json` | 53 | 508 | 10 | 2 | 3 | 3 | 0 | 0 | **579** | 4,358 | 0 |
| **PACCAR Inc. (Peterbilt / Kenworth / DAF / Leyland)** <sub>`paccar.json`</sub> | `PACCAR` | 1XP, 1NP, 5KJ +5 | `oem-cars-and-trucks.json` | 55 | 507 | 12 | 1 | 2 | 2 | 0 | 0 | **579** | 4,358 | 0 |
| **Volvo Group (Volvo Trucks / Mack / Renault Trucks)** <sub>`volvotrucks.json`</sub> | `VOLVOTR` | 4V4, YV2, 4V2 +6 | `oem-cars-and-trucks.json` | 59 | 494 | 13 | 1 | 2 | 2 | 0 | 0 | **571** | 4,358 | 0 |
| **NIO Inc.** <sub>`nio.json`</sub> | `NIO` | LJN, LBL | `oem-cars-and-trucks.json` | 48 | 492 | 14 | 3 | 5 | 4 | 0 | 0 | **566** | 4,358 | 0 |
| **Subaru Corporation** <sub>`subaru.json`</sub> | `SUBARU` | JF1, JF2, JF3 +2 | `oem-cars-and-trucks.json` | 50 | 486 | 13 | 3 | 5 | 3 | 0 | 0 | **560** | 4,358 | 0 |
| **Isuzu Motors Ltd.** <sub>`isuzu.json`</sub> | `ISUZU` | JAA, JAB, JAL +4 | `oem-cars-and-trucks.json` | 54 | 483 | 11 | 1 | 2 | 2 | 0 | 0 | **553** | 4,358 | 0 |
| **Polestar Performance AB (Geely)** <sub>`polestar.json`</sub> | `POLESTAR` | LPS, LFP | `oem-cars-and-trucks.json` | 43 | 495 | 9 | 1 | 3 | 2 | 0 | 0 | **553** | 4,358 | 0 |
| **Iveco S.p.A. (Iveco Group)** <sub>`iveco.json`</sub> | `IVECO` | ZCF, VCF | `oem-cars-and-trucks.json` | 53 | 482 | 12 | 1 | 2 | 2 | 0 | 0 | **552** | 4,358 | 0 |
| **Xpeng Motors (XPeng Inc.)** <sub>`xpeng.json`</sub> | `XPENG` | LJY, LMZ | `oem-cars-and-trucks.json` | 47 | 475 | 13 | 3 | 5 | 5 | 0 | 0 | **548** | 4,358 | 0 |
| **Automobile Dacia SA (Renault Group)** <sub>`dacia.json`</sub> | `DACIA` | UU1, UU3, LBR +1 | `oem-cars-and-trucks.json` | 46 | 488 | 8 | 1 | 2 | 2 | 0 | 0 | **547** | 4,358 | 0 |
| **Bentley Motors Ltd.** <sub>`bentley.json`</sub> | `BENTLEY` | SCB | `oem-cars-and-trucks.json` | 46 | 480 | 12 | 2 | 3 | 2 | 0 | 0 | **545** | 4,358 | 0 |
| **Lucid Group, Inc.** <sub>`lucid.json`</sub> | `LUCID` | 50A | `oem-cars-and-trucks.json` | 45 | 466 | 9 | 2 | 4 | 3 | 0 | 0 | **529** | 4,358 | 0 |
| **Rivian Automotive, Inc.** <sub>`rivian.json`</sub> | `RIVIAN` | 7PD | `oem-cars-and-trucks.json` | 47 | 456 | 11 | 3 | 5 | 4 | 0 | 0 | **526** | 4,358 | 0 |
| **AvtoVAZ (Lada)** <sub>`lada.json`</sub> | `LADA` | XTA, XTC, XTV | `oem-cars-and-trucks.json` | 46 | 463 | 9 | 1 | 2 | 1 | 0 | 0 | **522** | 4,358 | 0 |
| **McLaren Automotive Ltd.** <sub>`mclaren.json`</sub> | `MCLAREN` | SBM | `oem-cars-and-trucks.json` | 49 | 448 | 13 | 2 | 5 | 4 | 0 | 0 | **521** | 4,358 | 0 |
| **smart Automobile Co. (Mercedes-Geely JV)** <sub>`smart.json`</sub> | `SMART` | WME, L7M | `oem-cars-and-trucks.json` | 43 | 457 | 6 | 1 | 2 | 2 | 0 | 0 | **511** | 4,358 | 0 |
| **Rolls-Royce Motor Cars Ltd.** <sub>`rolls-royce.json`</sub> | `ROLLS_ROYCE` | SCA | `oem-cars-and-trucks.json` | 58 | 425 | 13 | 4 | 5 | 5 | 0 | 0 | **510** | 4,358 | 0 |
| **MAN Truck & Bus SE (Traton Group)** <sub>`man.json`</sub> | `MAN` | WMA, 9BW | `oem-cars-and-trucks.json` | 56 | 436 | 10 | 1 | 2 | 2 | 0 | 0 | **507** | 4,358 | 0 |
| **Scania AB (Traton Group)** <sub>`scania.json`</sub> | `SCANIA` | VLU, YS2, XLE +1 | `oem-cars-and-trucks.json` | 56 | 437 | 10 | 1 | 2 | 1 | 0 | 0 | **507** | 4,358 | 0 |
| **Detroit Diesel Corp. (Daimler Truck — engine OEM)** <sub>`detroit.json`</sub> | `DDC` |  | `oem-cars-and-trucks.json` | 49 | 429 | 10 | 1 | 2 | 2 | 0 | 0 | **493** | 4,358 | 0 |

## Marine

| Brand | Key | WMIs | Base | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Own** | Inherited | Verified |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Mercury Marine (Brunswick)** <sub>`marine/mercury-marine.json`</sub> | `MERCURY-MARINE` | MAR | `oem-marine.json` | 0 | 282 | 0 | 0 | 2 | 2 | 0 | 0 | **286** | 153 | 0 |
| **Volvo Penta** <sub>`marine/volvo-penta.json`</sub> | `VOLVO-PENTA` | VPE | `oem-marine.json` | 0 | 128 | 0 | 0 | 0 | 0 | 0 | 0 | **128** | 153 | 0 |
| **Cummins Marine** <sub>`marine/cummins-marine.json`</sub> | `CUMMINS-MARINE` | 1NE | `oem-marine.json` | 0 | 96 | 0 | 0 | 0 | 0 | 0 | 0 | **96** | 153 | 0 |
| **MTU (Rolls-Royce Power Systems)** <sub>`marine/mtu.json`</sub> | `MTU` | MTU | `oem-marine.json` | 0 | 88 | 0 | 0 | 0 | 0 | 0 | 0 | **88** | 153 | 0 |
| **Yanmar Marine** <sub>`marine/yanmar-marine.json`</sub> | `YANMAR-MARINE` | YAN | `oem-marine.json` | 0 | 88 | 0 | 0 | 0 | 0 | 0 | 0 | **88** | 153 | 0 |
| **Yamaha Marine** <sub>`marine/yamaha-marine.json`</sub> | `YAMAHA-MARINE` | YMM | `oem-marine.json` | 0 | 80 | 0 | 0 | 0 | 0 | 0 | 0 | **80** | 153 | 0 |

## Motorcycle

| Brand | Key | WMIs | Base | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Own** | Inherited | Verified |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Honda Motor (motorcycles)** <sub>`motorcycle/honda-moto.json`</sub> | `HONDA-MOTO` | JH2, JH4, MLH | `oem-motorcycle.json` | 1 | 104 | 0 | 0 | 0 | 0 | 0 | 0 | **105** | 156 | 0 |
| **BMW Motorrad** <sub>`motorcycle/bmw-motorrad.json`</sub> | `BMW-MOTORRAD` | WB1, WB2, ZBM | `oem-motorcycle.json` | 2 | 96 | 0 | 0 | 2 | 0 | 0 | 0 | **100** | 156 | 0 |
| **Kawasaki Heavy Industries** <sub>`motorcycle/kawasaki.json`</sub> | `KAWASAKI` | JKA, JKB | `oem-motorcycle.json` | 1 | 96 | 0 | 0 | 0 | 1 | 0 | 0 | **98** | 156 | 0 |
| **Suzuki Motor (motorcycles)** <sub>`motorcycle/suzuki-moto.json`</sub> | `SUZUKI-MOTO` | JS1, VTT | `oem-motorcycle.json` | 0 | 88 | 0 | 0 | 0 | 0 | 0 | 0 | **88** | 156 | 0 |
| **Yamaha Motor (motorcycles)** <sub>`motorcycle/yamaha-moto.json`</sub> | `YAMAHA-MOTO` | JYA, JYE, VTL | `oem-motorcycle.json` | 0 | 88 | 0 | 0 | 0 | 0 | 0 | 0 | **88** | 156 | 0 |
| **Ducati Motor Holding** <sub>`motorcycle/ducati.json`</sub> | `DUCATI` | ZDM | `oem-motorcycle.json` | 0 | 80 | 0 | 0 | 3 | 1 | 0 | 0 | **84** | 156 | 0 |
| **KTM AG** <sub>`motorcycle/ktm.json`</sub> | `KTM` | VBK | `oem-motorcycle.json` | 0 | 80 | 0 | 0 | 0 | 0 | 0 | 0 | **80** | 156 | 0 |
| **Triumph Motorcycles** <sub>`motorcycle/triumph.json`</sub> | `TRIUMPH` | SMT | `oem-motorcycle.json` | 0 | 80 | 0 | 0 | 0 | 0 | 0 | 0 | **80** | 156 | 0 |
| **Harley-Davidson** <sub>`motorcycle/harley-davidson.json`</sub> | `HARLEY-DAVIDSON` | 1HD, 5HD | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 1 | 0 | 0 | 0 | **65** | 156 | 0 |
| **Aprilia (Piaggio)** <sub>`motorcycle/aprilia.json`</sub> | `APRILIA` | ZD4 | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 156 | 0 |
| **Husqvarna Motorcycles (KTM AG)** <sub>`motorcycle/husqvarna-moto.json`</sub> | `HUSQVARNA-MOTO` | ZBP | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 156 | 0 |
| **Indian Motorcycle (Polaris)** <sub>`motorcycle/indian-motorcycle.json`</sub> | `INDIAN-MOTORCYCLE` | 56K | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 156 | 0 |
| **MV Agusta (KTM stake)** <sub>`motorcycle/mv-agusta.json`</sub> | `MV-AGUSTA` | ZCG | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 156 | 0 |
| **Royal Enfield (Eicher)** <sub>`motorcycle/royal-enfield.json`</sub> | `ROYAL-ENFIELD` | MEZ | `oem-motorcycle.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 156 | 0 |

## Powersports

| Brand | Key | WMIs | Base | ECUs | DIDs | Routines | Coding | Adapt | Act | Live | DTC ext | **Own** | Inherited | Verified |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **Can-Am / BRP (Bombardier)** <sub>`powersports/can-am-brp.json`</sub> | `CAN-AM-BRP` | 3JB | `oem-powersports.json` | 0 | 104 | 0 | 0 | 0 | 0 | 0 | 0 | **104** | 151 | 0 |
| **Polaris Industries** <sub>`powersports/polaris.json`</sub> | `POLARIS` | 4XA | `oem-powersports.json` | 0 | 96 | 0 | 0 | 0 | 0 | 0 | 0 | **96** | 151 | 0 |
| **Arctic Cat (Textron)** <sub>`powersports/arctic-cat.json`</sub> | `ARCTIC-CAT` | 4UF | `oem-powersports.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 151 | 0 |
| **Yamaha WaveRunner (Yamaha Marine subdivision)** <sub>`powersports/yamaha-waverunner.json`</sub> | `YAMAHA-WAVERUNNER` | YAM | `oem-powersports.json` | 0 | 64 | 0 | 0 | 0 | 0 | 0 | 0 | **64** | 151 | 0 |
| **Kawasaki Jet Ski** <sub>`powersports/kawasaki-jet.json`</sub> | `KAWASAKI-JET` | KAW | `oem-powersports.json` | 0 | 48 | 0 | 0 | 0 | 0 | 0 | 0 | **48** | 151 | 0 |

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
| `obd2-pids.json` | ISO 15031-6 / SAE J1979 OBD-II Service 01 PIDs | 84 | 0 | 84 |
| `uds-standard.json` | ISO 14229-1 Universal UDS DIDs | 31 | 4 | 35 |
