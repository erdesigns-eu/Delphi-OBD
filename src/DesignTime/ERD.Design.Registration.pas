// ------------------------------------------------------------------------------
// ERD.Design.Registration
//
// Design-time registration entry point for the Delphi-OBD package.
//
// This unit is linked into DelphiOBD_DT.bpl only; it must never be
// referenced from runtime code. The IDE calls <c>Register</c> when the
// design-time package is installed.
//
// Component categories used:
// - "OBD" — non-visual diagnostic and protocol components
// - "OBD OEM" — vendor-specific coding helpers
// - "OBD Visual" — Delphi VCL controls and dashboards
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-09  ERD  Initial empty registration.
// 2026-05-09  ERD  Register the Connection / Adapter / Protocol
// / DoIP / SecOC components.
// 2026-05-10  ERD  Add splash + About-box registration via Tools API.
//
// 2026-10-08  ERD  Components/editors only; remove wizards and IDE branding.
// ------------------------------------------------------------------------------

unit ERD.Design.Registration;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

/// <summary>
/// Called by the IDE when DelphiOBD_DT.bpl is installed. Registers
/// every palette component shipped by Delphi-OBD on the "OBD" tab.
/// </summary>
procedure Register;

implementation

{$R ERD.Design.Icons.res}

uses
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  ERD.Connection,
  ERD.Adapter,
  ERD.Protocol,
  ERD.Protocol.DoIP.Client,
  ERD.Protocol.SecOC,
  ERD.Service.LiveData,
  ERD.Service.DTCs,
  ERD.Service.VIN,
  ERD.Service.VINInspector,
  ERD.Service.FreezeFrame,
  ERD.Service.OnBoardMonitor,
  ERD.Service.VehicleHealth,
  ERD.Service.DriveCycle,
  ERD.Service.EVBattery,
  ERD.Service.Actuator,
  ERD.ClearDTC,
  ERD.OxygenMonitor,
  ERD.DataSource,
  ERD.WWHOBD,
  ERD.WWHOBD.Readiness,
  ERD.Diagnostics.UDS,
  ERD.Diagnostics.UDS.Reset,
  ERD.Diagnostics.UDS.ReadMemory,
  ERD.Diagnostics.UDS.IOControl,
  ERD.Diagnostics.UDS.ReadDID,
  ERD.Diagnostics.UDS.ReadDTC,
  ERD.Diagnostics.UDS.Periodic,
  ERD.Diagnostics.UDS.DynamicDID,
  ERD.Diagnostics.KWP,
  ERD.Diagnostics.KWP.ReadID,
  ERD.Diagnostics.KWP.ReadDTC,
  ERD.Diagnostics.KWP.IOControl,
  ERD.Diagnostics.KWP.Routine,
  ERD.Diagnostics.J1939,
  ERD.Diagnostics.J1939.DM,
  ERD.OEM.Registry,
  ERD.OEM.Catalog,
  ERD.Service.VWRadioSAFE,
  ERD.Coding.SecurityAccess,
  ERD.Coding.DataIdentifierIO,
  ERD.Coding.RoutineControl,
  ERD.Coding.Flasher,
  ERD.Coding.Uploader,
  ERD.Coding.FlashSession,
  ERD.Calibration.XCP,
  ERD.Calibration.CCP,
  ERD.Speciality.IsoBus,
  ERD.UDS.WriteMemory,
  ERD.UDS.WriteDID,
  ERD.KWP.WriteID,
  ERD.Coding.AuditLog,
  ERD.Coding.Session,
  ERD.OEM.ComponentProtection.VAG,
  ERD.OEM.ComponentProtection.BMW,
  ERD.OEM.ComponentProtection.Mercedes,
  ERD.OEM.ComponentProtection.Stellantis,
  ERD.OEM.KeyAdaptation.Ford,
  ERD.OEM.KeyAdaptation.HMG,
  ERD.OEM.KeyAdaptation.BMW,
  ERD.OEM.KeyAdaptation.Toyota,
  ERD.Protocol.KWP1281.Session,
  ERD.Protocol.TP20.Session,
  ERD.J2534.Components,
  ERD.Service.VINDecoder.Catalog.Component,
  ERD.Service.DriveCycle.Catalog.Component,
  ERD.Service.EVBattery.Catalog.Component,
  ERD.UI.Theme,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Linear,
  ERD.UI.Gauges.Variants,
  ERD.UI.Gauges.Sparkline,
  ERD.UI.Gauges.DialExtended,
  ERD.UI.Indicators,
  ERD.UI.Telltales,
  ERD.UI.Shift,
  ERD.UI.Timing,
  ERD.UI.Gauges.Specialised,
  ERD.UI.LivePanels,
  ERD.UI.LiveGrids,
  ERD.UI.Terminal,
  ERD.UI.LogViewer,
  ERD.UI.DtcList,
  ERD.UI.Knob,
  ERD.UI.TrendGraph,
  ERD.UI.Info,
  ERD.UI.MonitorEV,
  ERD.UI.Session,
  ERD.UI.Connection,
  ERD.UI.Pickers,
  ERD.UI.FlashDashboards,
  ERD.UI.Dyno,
  ERD.UI.Charts,
  ERD.UI.Tuning,
  ERD.UI.Motorsport,
  ERD.UI.Commercial,
  ERD.UI.Replay,
  ERD.UI.Branding,
  ERD.UI.CodingEditors,
  ERD.UI.Diag,
  ERD.UI.SessionInspect,
  ERD.UI.Insights,
  ERD.UI.Logger,
  ERD.UDS.Transfer,
  ERD.Flash.VoltageGate,
  ERD.Flash.Pipeline,
  ERD.Recorder,
  ERD.Replayer,
  ERD.RadioCode,
  ERD.RadioCode.EuropeanPremium,
  ERD.RadioCode.FrenchItalian,
  ERD.RadioCode.British,
  ERD.RadioCode.Asian,
  ERD.RadioCode.American,
  ERD.RadioCode.Aftermarket,
  ERD.RadioCode.Volvo,
  ERD.RadioCode.FordV,
  ERD.RadioCode.EEPROM,
  ERD.Design.Editors;

procedure Register;
begin
  // Lower-level building blocks: connection, adapter, protocol,
  // network and security.
  RegisterComponents('OBD', [TOBDConnection, TOBDAdapter, TOBDProtocol,
    TOBDDoIPClient, TOBDSecOCCodec]);

  // Service-mode: higher-level diagnostics that sit on top of
  // TOBDProtocol.
  RegisterComponents('OBD Services', [TOBDLiveData, TOBDDTCs, TOBDVIN,
    TOBDVINInspector, TOBDFreezeFrame, TOBDOnBoardMonitor, TOBDActuator,
    TOBDVehicleHealth, TOBDDriveCycleAdvisor, TOBDEVBattery, TOBDClearDTC,
    TOBDOxygenMonitor, TOBDDataSource, TOBDWWHOBD, TOBDWWHReadiness]);

  // Advanced UDS / KWP / J1939 / OEM-overlay diagnostic components.
  RegisterComponents('OBD Diagnostics', [TOBDUDS, TOBDUDSReset,
    TOBDUDSReadMemory, TOBDUDSIOControl, TOBDUDSReadDID, TOBDUDSReadDTC,
    TOBDUDSReadByPeriodic, TOBDUDSDynamicDID, TOBDKWP, TOBDKWPReadID,
    TOBDKWPReadDTC, TOBDKWPIOControl, TOBDKWPRoutine, TOBDJ1939, TOBDJ1939DM,
    TOBDOEMCatalog]);

  // Coding & flashing: write-side UDS components. On their own
  // tab so a host can keep them visually separated from the
  // read-only service-mode components.
  RegisterComponents('OBD Coding', [TOBDSecurityAccess, TOBDDataIdentifierIO,
    TOBDRoutineControl, TOBDFlasher, TOBDUploader, TOBDFlashSession]);

  // Calibration + speciality buses. XCP and CCP need a transport
  // injected at runtime; IsoBus is component-friendly out of the
  // box.
  RegisterComponents('OBD Calibration', [TOBDXCP, TOBDCCP, TOBDIsoBus]);

  // Extra write-side components and the session orchestrator.
  RegisterComponents('OBD Coding', [TOBDUDSWriteMemory, TOBDUDSWriteDID,
    TOBDKWPWriteID, TOBDCodingAuditLog, TOBDCodingSession,
    TOBDComponentProtectionVAG, TOBDComponentProtectionBMW,
    TOBDComponentProtectionMercedes, TOBDComponentProtectionStellantis,
    TOBDKeyAdaptationFord, TOBDKeyAdaptationHMG, TOBDKeyAdaptationBMW,
    TOBDKeyAdaptationToyota]);

  // Flashing components. WARNING — drop on a form, wire
  // OnConfirmExecute, leave AutoExecute = False until the host
  // really means it. Read docs/flashing-safety.md.
  RegisterComponents('OBD Flashing', [TOBDUDSTransfer, TOBDVoltageGate,
    TOBDFlashPipeline]);

  // Recorder / replayer pair. Drop on a form, point at a
  // TOBDProtocol, capture the entire session for forensic /
  // offline analysis or replay for tests.
  RegisterComponents('OBD', [TOBDRecorder, TOBDReplayer]);

  // Session / transport components. Foundation for KWP1281,
  // TP2.0 and J2534 work without going through a higher-level
  // facade like TOBDVWRadioSAFE. Drop, wire, call.
  RegisterComponents('OBD', [TOBDKWP1281Session, TOBDTP20Session,
    TOBDJ2534Device, TOBDJ2534Channel]);

  // Catalogue manager components. Wrap the static catalogs so
  // hosts can configure CatalogDir / AutoLoad in the Object
  // Inspector instead of doing it in code.
  RegisterComponents('OBD Catalogs', [TOBDVINCatalog, TOBDDriveCycleCatalogComp,
    TOBDEVBatteryCatalogComp]);

  // Visual UI foundation. The TOBDTheme controller is the
  // anchor: drop one on a form / data-module and every
  // Delphi-OBD visual on the form auto-binds to it (the
  // visuals walk Owner ancestry at runtime). Sub-phases A2.2+
  // add the gauges / telltales / lists that consume the theme.
  RegisterComponents('OBD Visual', [TOBDTheme, TOBDTerminal, TOBDLogViewer,
    TOBDDtcList, TOBDKnob, TOBDTrendGraph, TOBDCircularGauge, TOBDLinearGauge,
    TOBDTachometer, TOBDArcGauge, TOBDComboGauge, TOBDDigitalGauge,
    TOBDBarSegmentGauge, TOBDDeltaGauge, TOBDSparkline, TOBDDualNeedleGauge,
    TOBDMinMaxGauge, TOBDLED, TOBDMatrixDisplay, TOBDMILLamp, TOBDDTCBadge,
    TOBDReadinessLamp, TOBDDashLamp, TOBDShiftLight, TOBDShiftLightBar,
    TOBDGearIndicator, TOBDDragTimer, TOBDLapTimer, TOBDAccelGraph,
    TOBDBoostGauge, TOBDAFRGauge, TOBDStateOfChargeBar, TOBDRegenIndicator,
    TOBDPidPanel, TOBDFuelTrimDisplay, TOBDMultiPidGrid, TOBDFreezeFrameTable,
    TOBDVINCard, TOBDAdapterPanel, TOBDOdometer, TOBDClock, TOBDReadinessGrid,
    TOBDDriveCycleProgress, TOBDCellVoltageHeatmap, TOBDChargingFlow,
    TOBDFlashProgress, TOBDCodingSessionPanel, TOBDXCPProgressBar,
    TOBDRecorderToolbar, TOBDConnectionStateLamp, TOBDDoIPStatusPanel,
    TOBDSecurityAccessLamp, TOBDSecOCStatusLamp, TOBDVINEdit, TOBDPidPicker,
    TOBDOEMPicker, TOBDCANIdEdit, TOBDFlashSafetyDashboard,
    TOBDFlashCheckpointTimeline, TOBDFlashAuditTail, TOBDStripChart,
    TOBDLiveGridChart, TOBDDynoChart, TOBDPowerCurveGraph, TOBDXYHeatmap,
    TOBDTorqueRPMMap, TOBDRunRecorder, TOBDLapTrackMap, TOBDPredictiveLap,
    TOBDGForceVisualiser, TOBDMarineTach, TOBDPTOMeter, TOBDDPFStatus,
    TOBDAdBlueLevel, TOBDChargePortIndicator, TOBDMaintenanceCard,
    TOBDServiceHistoryTimeline, TOBDPlaybackScrubber, TOBDPlaybackTimeline,
    TOBDFrameInspector, TOBDOEMBadge, TOBDDigitalCluster, TOBDBluetoothSignal,
    TOBDWiFiSignal, TOBDGPSAccuracy, TOBDCodingDiffViewer, TOBDLabelFileEditor,
    TOBDAdaptationEditor, TOBDLongCodingEditor, TOBDSeedKeyDebugger,
    TOBDMode06Viewer, TOBDMode07Viewer, TOBDMode0AViewer, TOBDMode04Confirm,
    TOBDRoutineControlLauncher, TOBDActuatorTestPanel,
    TOBDKWP1281SessionInspector, TOBDJ2534DeviceList, TOBDTP20ChannelPanel,
    TOBDDoIPNodePicker, TOBDDriverScoreWidget, TOBDEcoScoreWidget,
    TOBDTripSummaryCard, TOBDLoggerControl, TOBDLoggerExplorer]);
  // Non-visual dyno math (own palette tab).
  RegisterComponents('OBD Dyno', [TOBDDynoCalculator, TOBDPowerCurve,
    TOBDDragRun, TOBDDynoConditions, TOBDFuelEconomyMeter,
    TOBDEmissionsEstimator, TOBDInertialBrake, TOBDTorqueAtWheels]);

  // Radio-code calculators (one component per vendor). Each
  // validates the input shape; production algorithms are
  // proprietary / licensed and supplied by the host via the
  // OnCalculate event.
  RegisterComponents('OBD Radio', [TOBDRadioCodeVW, TOBDRadioCodeAudiConcert,
    TOBDRadioCodeBMW, TOBDRadioCodeMercedes, TOBDRadioCodeMini,
    TOBDRadioCodePorsche, TOBDRadioCodeSEAT, TOBDRadioCodeSkoda,
    TOBDRadioCodeSmart, TOBDRadioCodeCitroen, TOBDRadioCodePeugeot,
    TOBDRadioCodeRenault, TOBDRadioCodeFiatDaiichi, TOBDRadioCodeFiatVP,
    TOBDRadioCodeAlfaRomeo, TOBDRadioCodeMaserati, TOBDRadioCodeJaguar,
    TOBDRadioCodeLandRover, TOBDRadioCodeSaab, TOBDRadioCodeOpel,
    TOBDRadioCodeAcura, TOBDRadioCodeHonda, TOBDRadioCodeHyundai,
    TOBDRadioCodeInfiniti, TOBDRadioCodeLexus, TOBDRadioCodeMazda,
    TOBDRadioCodeMitsubishi, TOBDRadioCodeNissan, TOBDRadioCodeSubaru,
    TOBDRadioCodeSuzuki, TOBDRadioCodeToyota, TOBDRadioCodeChrysler,
    TOBDRadioCodeFordM, TOBDRadioCodeGM, TOBDRadioCodeVisteon,
    TOBDRadioCodeAlpine, TOBDRadioCodeBlaupunkt, TOBDRadioCodeClarion,
    TOBDRadioCodeBecker4, TOBDRadioCodeBecker5, TOBDRadioCodeVolvo,
    TOBDRadioCodeFordV]);

  // EEPROM-dump extractors. Different beast from the calculator
  // family above: the host pulls the radio's serial-EEPROM with
  // a chip programmer, hands the .bin to the matching component,
  // and the component reads the code out at a fixed offset. No
  // algorithm, no licensed service.
  RegisterComponents('OBD EEPROM', [TOBDRadioCodeEEPROM_VolvoHU,
    TOBDRadioCodeEEPROM_OpelCD30, TOBDRadioCodeEEPROM_MercedesBecker]);

  // VW group SAFE-code recovery over the diagnostic bus
  // (KWP1281). Goes on the OBD Radio tab because the host-facing
  // shape is "give me the unlock code" - even though under the
  // hood it talks to the radio rather than running an algorithm
  // or reading a chip dump.
  RegisterComponents('OBD Radio', [TOBDVWRadioSAFE]);

  // Register only the property and component editors for palette components.
  RegisterDelphiOBDEditors;
end;

end.
