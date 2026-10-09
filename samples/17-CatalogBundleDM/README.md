# 17-CatalogBundleDM

Demonstrates the three catalogue manager components on the
**OBD Catalogs** palette tab. Each one wraps a static
catalogue class so a host can configure `CatalogDir` /
`AutoLoad` in the Object Inspector instead of in code.

| Component | Wraps | Catalog dir |
|---|---|---|
| `TOBDVINCatalog` | `TOBDVINDecoder` | `catalogs/vin/` |
| `TOBDDriveCycleCatalogComp` | `TOBDDriveCycleCatalog` | `catalogs/drive-cycle-*.json` |
| `TOBDEVBatteryCatalogComp` | `TOBDEVBatteryCatalog` | `catalogs/ev-battery/` |

The wizard `Delphi-OBD: Catalogue manager DataModule`
generates a `TDataModule` with all three pre-wired
(`AutoLoad = True`).

## Build & run

```cmd
dcc32 -B CatalogBundleDM.dpr
CatalogBundleDM
```

## Drop-on-form pattern

```pascal
TDM_OBDCatalogs = class(TDataModule)
  VINCatalog:        TOBDVINCatalog;          // AutoLoad=True
  DriveCycleCatalog: TOBDDriveCycleCatalogComp;
  EVCatalog:         TOBDEVBatteryCatalogComp;
end;
```

Once the DM streams in, every form that uses the catalogue
classes (VINInspector, EVBattery, DriveCycleAdvisor) sees a
loaded catalogue without any host code.
