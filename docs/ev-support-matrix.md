# EV capability matrix

Generated from `catalogs/ev-battery/` with `python3 tools/ev_support_matrix.py --write`.

A field rule is a decoder definition, not a confirmed supported vehicle.
Models and ECU addressing below reproduce catalogue declarations. Model years
are specified only where the catalogue supplies them. No vehicle has been bench
validated in this branch. “Full” in older manifest labels does not mean complete
brand or model support. Only `fields` are executable; `passive_can_fields` and
`phase1_fields` are supplementary source notes, not implemented polling rules.

All 15 vendor files load in the FPC runtime regression. BMW DD69 has signed-current
golden vectors; declared array lengths also have regressions. Other field maps
still require ECU-specific captured responses or bench confirmation.

| Vendor | Declared models | ECU request / response | Executable rules | Available fields | Source |
|---|---|---|---:|---|---|
| bmw | BMW i3 60Ah (2014-2016); BMW i3 94Ah (2017-2018); BMW i3 / i3s 120Ah (2019-2022) | 0x6F1 / 0x607 (ISOTP_EXTADR) | 9 | battery_temp_max, battery_temp_min, capacity_remaining_ah, cell_voltages_min_max, odometer_km, pack_current, pack_voltage, range_km, soc_displayed | [primary source](https://github.com/openvehicles/Open-Vehicle-Monitoring-System-3/tree/master/vehicle/OVMS.V3/components/vehicle_bmwi3) |
| ford | Ford Mustang Mach-E; Ford F-150 Lightning; Ford E-Transit | 0x7E4 / 0x7EC | 2 | hvb_soc, hvb_voltage_variation | [primary source](https://www.macheforum.com/) |
| gm | Chevrolet Bolt EV (2017-2023); Chevrolet Bolt EUV (2022-2023) | 0x7E4 / 0x7EC | 8 | max_cell_voltage, min_cell_voltage, pack_current, pack_max_temp, pack_min_temp, pack_voltage, soc_display, soc_raw_bms | [primary source](https://allev.info/boltpids/) |
| hmg | Hyundai Kona EV; Hyundai Ioniq Electric (28/38 kWh); Hyundai Ioniq 5; Hyundai Ioniq 6; Kia Niro EV; Kia Soul EV; Kia EV6; Genesis GV60 | 0x7E4 / 0x7EC | 31 | aux_battery_voltage, available_charge_power, available_discharge_power, battery_inlet_temp, battery_max_temp, battery_min_temp, cell_voltages_1_32, cell_voltages_33_64, cell_voltages_65_96, cell_voltages_97_98, charging_state_bits, cumulative_charge_ah, cumulative_discharge_ah, cumulative_energy_charged_kwh, cumulative_energy_discharged_kwh, isolation_resistance, max_cell_no, max_cell_voltage, min_cell_no, min_cell_voltage, module_temp_1, module_temp_2, module_temp_3, module_temp_4, odometer_km, operating_time_s, pack_current, pack_voltage, soc_bms, soc_display, soh | [primary source](https://github.com/JejuSoul/OBD-PIDs-for-HKMC-EVs) |
| honda | Honda e; Honda e:Ny1; Honda Prologue | undocumented / undocumented | 0 | **Not supported** | No decode source |
| mazda | Mazda MX-30 EV | 0x7E4 / 0x7EC | 0 | **Not supported** | [primary source](https://www.mx30forum.com/threads/obd2-and-our-mx-30.321/) |
| mercedes | Mercedes-Benz EQC; Mercedes-Benz EQA; Mercedes-Benz EQB; Mercedes-Benz EQE; Mercedes-Benz EQS | undocumented / undocumented | 0 | **Not supported** | No decode source |
| nissan | Nissan Leaf ZE0 (2011-2012, 24 kWh); Nissan Leaf AZE0 (2013-2017, 24/30 kWh); Nissan Leaf ZE1 (2018+, 40/62 kWh) | 0x79B / 0x7BB | 12 | ahr_remaining, bus_voltage, cell_voltages, pack_temp_1, pack_temp_2, pack_temp_3, pack_temp_4, pack_voltage, shunt_balancing, soc, soh_factory, soh_pct | [primary source](https://github.com/openvehicles/Open-Vehicle-Monitoring-System-3/blob/master/vehicle/OVMS.V3/components/vehicle_nissanleaf/src/vehicle_nissanleaf.cpp) |
| polestar | Polestar 2; Volvo XC40 Recharge; Volvo C40 Recharge; Volvo EX40 / EC40; Volvo EX30 (note: EX30 is on Geely SEA platform - decode may differ) | 0x18DA52F1 / 0x18DAF152 | 1 | soh | [primary source](https://www.polestar-forum.com/threads/howto-read-bms-soh-with-car-scanner-elm327.19509/) |
| porsche | Porsche Taycan; Audi e-tron GT; Audi RS e-tron GT | undocumented / undocumented | 0 | **Not supported** | No decode source |
| renault | Renault Zoe Q210 (2013-2016); Renault Zoe R240 (2015-2017); Renault Zoe Q90 / R90 (2017-2019, 41 kWh); Renault Zoe ZE50 R110 / R135 (2019+, 52 kWh) - Phase 2 | 0x792 / 0x793 | 8 | max_cell_voltage, max_pack_temp, min_cell_voltage, min_pack_temp, pack_current, pack_voltage, soc, soh | [primary source](https://canze.fisch.lu/) |
| stellantis | Peugeot e-208; Peugeot e-2008; Opel Corsa-e; Opel Mokka-e; Citroen e-C4; DS3 Crossback E-Tense; Jeep Avenger EV; Fiat 500e | undocumented / undocumented | 0 | **Not supported** | [primary source](https://github.com/openvehicles/Open-Vehicle-Monitoring-System-3/issues/719) |
| tesla | Model S; Model 3; Model X; Model Y | undocumented / undocumented | 0 | **Not supported** | [primary source](https://github.com/joshwardell/model3dbc) |
| toyota | Toyota bZ4X; Subaru Solterra; Lexus RZ450e | 0x7E2 / 0x7EA | 0 | **Not supported** | [primary source](https://www.solterraforum.com/threads/pids-obd-commands.1172/) |
| vw | VW e-Up (2013-2019); VW e-Up / Skoda Citigo-e iV / SEAT Mii electric (2020+) | 0x7E5 / 0x7ED (ISOTP_NORMAL) | 229 | battery_temperature, capacity_remaining_ah, cell_voltages, cumulative_energy_charged_kwh, cumulative_energy_discharged_kwh, max_cell_voltage, min_cell_voltage, module_temps, pack_current, pack_voltage, soc, soc_motor_normalized, soh | [primary source](https://github.com/openvehicles/Open-Vehicle-Monitoring-System-3/blob/master/vehicle/OVMS.V3/components/vehicle_vweup/src/vweup_obd.cpp) |

## Integration limits

- The seven empty catalogues expose no executable UDS measurements. Tesla needs a
  separate passive CAN integration; an empty UDS catalogue is not Tesla support.
- VW and other maps can contain multiple ECU/model variants of a field. Select the
  exact matching rule and route before polling; rule count is not measurement count.
- Unsupported or missing measurements must be shown as unavailable, never as zero.
- Check firmware, addressing, session/security prerequisites, units and sign against
  the cited source before using any decoder with a vehicle.
- `_stub-test.json` is a regression fixture and is excluded from this support matrix.
