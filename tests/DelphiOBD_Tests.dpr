//------------------------------------------------------------------------------
//  DelphiOBD_Tests
//
//  DUnitX runner program for the Delphi-OBD test suite.
//
//  Run from the IDE or from the command line. CI uses the console runner
//  via the {$IFDEF CI} branch below.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial skeleton with one trivial test.
//    2026-05-09  ERD  Add types / errors / decoders / catalog tests.
//    2026-05-09  ERD  Add connection mock / retry / lifecycle tests.
//------------------------------------------------------------------------------

program DelphiOBD_Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  OBD.Version in '..\src\Core\OBD.Version.pas',
  OBD.Types in '..\src\Core\OBD.Types.pas',
  OBD.Errors in '..\src\Core\OBD.Errors.pas',
  OBD.Decoders in '..\src\Core\OBD.Decoders.pas',
  OBD.Catalog in '..\src\Core\OBD.Catalog.pas',
  OBD.Connection.Types in '..\src\Connection\OBD.Connection.Types.pas',
  OBD.Connection.Settings in '..\src\Connection\OBD.Connection.Settings.pas',
  OBD.Connection.Retry in '..\src\Connection\OBD.Connection.Retry.pas',
  OBD.Connection.Transport.Base in '..\src\Connection\OBD.Connection.Transport.Base.pas',
  OBD.Connection.Mock in '..\src\Connection\OBD.Connection.Mock.pas',
  OBD.Connection.Bluetooth in '..\src\Connection\OBD.Connection.Bluetooth.pas',
  OBD.Connection.BLE in '..\src\Connection\OBD.Connection.BLE.pas',
  OBD.Connection.WiFi in '..\src\Connection\OBD.Connection.WiFi.pas',
  OBD.Connection.UDP in '..\src\Connection\OBD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  OBD.Connection.Serial in '..\src\Connection\OBD.Connection.Serial.pas',
  OBD.Connection.FTDI in '..\src\Connection\OBD.Connection.FTDI.pas',
  {$ENDIF}
  OBD.Connection in '..\src\Connection\OBD.Connection.pas',
  OBD.Adapter.Types in '..\src\Adapter\OBD.Adapter.Types.pas',
  OBD.Adapter.Capabilities in '..\src\Adapter\OBD.Adapter.Capabilities.pas',
  OBD.Adapter.Commands in '..\src\Adapter\OBD.Adapter.Commands.pas',
  OBD.Adapter.Detection in '..\src\Adapter\OBD.Adapter.Detection.pas',
  OBD.Adapter.Init in '..\src\Adapter\OBD.Adapter.Init.pas',
  OBD.Adapter in '..\src\Adapter\OBD.Adapter.pas',
  OBD.Protocol.Types in '..\src\Protocol\OBD.Protocol.Types.pas',
  OBD.Protocol.ISO15765 in '..\src\Protocol\OBD.Protocol.ISO15765.pas',
  OBD.Protocol.UDS in '..\src\Protocol\OBD.Protocol.UDS.pas',
  OBD.Protocol.KWP2000 in '..\src\Protocol\OBD.Protocol.KWP2000.pas',
  OBD.Protocol.KWP1281 in '..\src\Protocol\OBD.Protocol.KWP1281.pas',
  {$IFDEF MSWINDOWS}
  OBD.Protocol.KWP1281.Transport.Serial in '..\src\Protocol\OBD.Protocol.KWP1281.Transport.Serial.pas',
  {$ENDIF}
  OBD.Protocol.KWP1281.Transport.ELM in '..\src\Protocol\OBD.Protocol.KWP1281.Transport.ELM.pas',
  OBD.Protocol.CAN in '..\src\Protocol\OBD.Protocol.CAN.pas',
  OBD.Protocol.TP20 in '..\src\Protocol\OBD.Protocol.TP20.pas',
  OBD.Protocol.KWP1281.Transport.TP20 in '..\src\Protocol\OBD.Protocol.KWP1281.Transport.TP20.pas',
  OBD.Protocol.KWP1281.Transport.ISOTP in '..\src\Protocol\OBD.Protocol.KWP1281.Transport.ISOTP.pas',
  {$IFDEF MSWINDOWS}
  OBD.J2534 in '..\src\Protocol\OBD.J2534.pas',
  OBD.Protocol.KWP1281.Transport.J2534 in '..\src\Protocol\OBD.Protocol.KWP1281.Transport.J2534.pas',
  OBD.J2534.Components in '..\src\Protocol\OBD.J2534.Components.pas',
  {$ENDIF}
  OBD.Protocol.KWP1281.Session in '..\src\Protocol\OBD.Protocol.KWP1281.Session.pas',
  OBD.Protocol.TP20.Session in '..\src\Protocol\OBD.Protocol.TP20.Session.pas',
  OBD.Protocol.ISO9141 in '..\src\Protocol\OBD.Protocol.ISO9141.pas',
  OBD.Protocol.J1850 in '..\src\Protocol\OBD.Protocol.J1850.pas',
  OBD.Protocol.J1939 in '..\src\Protocol\OBD.Protocol.J1939.pas',
  OBD.Protocol.J1939.TP in '..\src\Protocol\OBD.Protocol.J1939.TP.pas',
  OBD.Protocol.VIN in '..\src\Protocol\OBD.Protocol.VIN.pas',
  OBD.Protocol.DoIP.Header in '..\src\Protocol\OBD.Protocol.DoIP.Header.pas',
  OBD.Protocol.DoIP.Messages in '..\src\Protocol\OBD.Protocol.DoIP.Messages.pas',
  OBD.Protocol.DoIP.Transport in '..\src\Protocol\OBD.Protocol.DoIP.Transport.pas',
  OBD.Protocol.DoIP.Client in '..\src\Protocol\OBD.Protocol.DoIP.Client.pas',
  OBD.Protocol.DoIP in '..\src\Protocol\OBD.Protocol.DoIP.pas',
  OBD.Protocol.SecOC.AES in '..\src\Protocol\OBD.Protocol.SecOC.AES.pas',
  OBD.Protocol.SecOC.CMAC in '..\src\Protocol\OBD.Protocol.SecOC.CMAC.pas',
  OBD.Protocol.SecOC.Keys in '..\src\Protocol\OBD.Protocol.SecOC.Keys.pas',
  OBD.Protocol.SecOC.Freshness in '..\src\Protocol\OBD.Protocol.SecOC.Freshness.pas',
  OBD.Protocol.SecOC in '..\src\Protocol\OBD.Protocol.SecOC.pas',
  OBD.Protocol.LIN.Frame in '..\src\Protocol\OBD.Protocol.LIN.Frame.pas',
  OBD.Protocol.LIN.LDF in '..\src\Protocol\OBD.Protocol.LIN.LDF.pas',
  OBD.Protocol.FlexRay.Frame in '..\src\Protocol\OBD.Protocol.FlexRay.Frame.pas',
  OBD.Protocol.MOST.Control in '..\src\Protocol\OBD.Protocol.MOST.Control.pas',
  OBD.Protocol in '..\src\Protocol\OBD.Protocol.pas',
  OBD.Service.Catalog in '..\src\Service\OBD.Service.Catalog.pas',
  OBD.Service.LiveData in '..\src\Service\OBD.Service.LiveData.pas',
  OBD.Service.DTCs in '..\src\Service\OBD.Service.DTCs.pas',
  OBD.Service.VIN in '..\src\Service\OBD.Service.VIN.pas',
  OBD.Service.VINDecoder.Types in '..\src\Service\OBD.Service.VINDecoder.Types.pas',
  OBD.Service.VINDecoder in '..\src\Service\OBD.Service.VINDecoder.pas',
  OBD.Service.VINInspector in '..\src\Service\OBD.Service.VINInspector.pas',
  OBD.Service.VINDecoder.Catalog.Component in '..\src\Service\OBD.Service.VINDecoder.Catalog.Component.pas',
  OBD.Service.FreezeFrame in '..\src\Service\OBD.Service.FreezeFrame.pas',
  OBD.Service.OnBoardMonitor in '..\src\Service\OBD.Service.OnBoardMonitor.pas',
  OBD.Service.VehicleHealth in '..\src\Service\OBD.Service.VehicleHealth.pas',
  OBD.Service.DriveCycle.Types in '..\src\Service\OBD.Service.DriveCycle.Types.pas',
  OBD.Service.DriveCycle.Catalog in '..\src\Service\OBD.Service.DriveCycle.Catalog.pas',
  OBD.Service.DriveCycle in '..\src\Service\OBD.Service.DriveCycle.pas',
  OBD.Service.DriveCycle.Catalog.Component in '..\src\Service\OBD.Service.DriveCycle.Catalog.Component.pas',
  OBD.Service.EVBattery.Types in '..\src\Service\OBD.Service.EVBattery.Types.pas',
  OBD.Service.EVBattery.Catalog in '..\src\Service\OBD.Service.EVBattery.Catalog.pas',
  OBD.Service.EVBattery in '..\src\Service\OBD.Service.EVBattery.pas',
  OBD.Service.EVBattery.Catalog.Component in '..\src\Service\OBD.Service.EVBattery.Catalog.Component.pas',
  OBD.Service.Actuator in '..\src\Service\OBD.Service.Actuator.pas',
  OBD.Service.VWRadioSAFE in '..\src\Service\OBD.Service.VWRadioSAFE.pas',
  OBD.Coding.SecurityAccess in '..\src\Coding\OBD.Coding.SecurityAccess.pas',
  OBD.Coding.DataIdentifierIO in '..\src\Coding\OBD.Coding.DataIdentifierIO.pas',
  OBD.Coding.RoutineControl in '..\src\Coding\OBD.Coding.RoutineControl.pas',
  OBD.Coding.Flasher in '..\src\Coding\OBD.Coding.Flasher.pas',
  OBD.Coding.Uploader in '..\src\Coding\OBD.Coding.Uploader.pas',
  OBD.Coding.FlashSession in '..\src\Coding\OBD.Coding.FlashSession.pas',
  OBD.UDS.WriteMemory in '..\src\Coding\OBD.UDS.WriteMemory.pas',
  OBD.KWP.WriteID in '..\src\Coding\OBD.KWP.WriteID.pas',
  OBD.Coding.Diff in '..\src\Coding\OBD.Coding.Diff.pas',
  OBD.Coding.AuditLog in '..\src\Coding\OBD.Coding.AuditLog.pas',
  OBD.Coding.Session in '..\src\Coding\OBD.Coding.Session.pas',
  OBD.Coding.VAG in '..\src\Coding\OBD.Coding.VAG.pas',
  OBD.Coding.BMW in '..\src\Coding\OBD.Coding.BMW.pas',
  OBD.Coding.Ford in '..\src\Coding\OBD.Coding.Ford.pas',
  OBD.Coding.HMG in '..\src\Coding\OBD.Coding.HMG.pas',
  OBD.Coding.Honda in '..\src\Coding\OBD.Coding.Honda.pas',
  OBD.Coding.Mercedes in '..\src\Coding\OBD.Coding.Mercedes.pas',
  OBD.Coding.Stellantis in '..\src\Coding\OBD.Coding.Stellantis.pas',
  OBD.Coding.Toyota in '..\src\Coding\OBD.Coding.Toyota.pas',
  OBD.OEM.ComponentProtection.VAG in '..\src\Coding\OBD.OEM.ComponentProtection.VAG.pas',
  OBD.OEM.ComponentProtection.BMW in '..\src\Coding\OBD.OEM.ComponentProtection.BMW.pas',
  OBD.OEM.ComponentProtection.Mercedes in '..\src\Coding\OBD.OEM.ComponentProtection.Mercedes.pas',
  OBD.OEM.ComponentProtection.Stellantis in '..\src\Coding\OBD.OEM.ComponentProtection.Stellantis.pas',
  OBD.OEM.KeyAdaptation.Types in '..\src\OEM\OBD.OEM.KeyAdaptation.Types.pas',
  OBD.OEM.KeyAdaptation.Base in '..\src\OEM\OBD.OEM.KeyAdaptation.Base.pas',
  OBD.OEM.KeyAdaptation.Ford in '..\src\OEM\OBD.OEM.KeyAdaptation.Ford.pas',
  OBD.OEM.KeyAdaptation.HMG in '..\src\OEM\OBD.OEM.KeyAdaptation.HMG.pas',
  OBD.OEM.KeyAdaptation.BMW in '..\src\OEM\OBD.OEM.KeyAdaptation.BMW.pas',
  OBD.OEM.KeyAdaptation.Toyota in '..\src\OEM\OBD.OEM.KeyAdaptation.Toyota.pas',
  OBD.Coding.OptionCatalog in '..\src\Coding\OBD.Coding.OptionCatalog.pas',
  OBD.Coding.DiffRLE in '..\src\Coding\OBD.Coding.DiffRLE.pas',
  OBD.Coding.LabelFile.VAG in '..\src\Coding\OBD.Coding.LabelFile.VAG.pas',
  OBD.UDS.Transfer in '..\src\Flashing\OBD.UDS.Transfer.pas',
  OBD.J1939.MemoryAccess in '..\src\Flashing\OBD.J1939.MemoryAccess.pas',
  OBD.Flash.VoltageGate in '..\src\Flashing\OBD.Flash.VoltageGate.pas',
  OBD.Flash.Checkpoint in '..\src\Flashing\OBD.Flash.Checkpoint.pas',
  OBD.Flash.Phases in '..\src\Flashing\OBD.Flash.Phases.pas',
  OBD.Flash.Pipeline in '..\src\Flashing\OBD.Flash.Pipeline.pas',
  OBD.Signature in '..\src\Flashing\OBD.Signature.pas',
  OBD.Signature.BCrypt in '..\src\Flashing\OBD.Signature.BCrypt.pas',
  OBD.Signature.OpenSSL in '..\src\Flashing\OBD.Signature.OpenSSL.pas',
  OBD.Signature.HSM in '..\src\Flashing\OBD.Signature.HSM.pas',
  OBD.Signature.PQC in '..\src\Flashing\OBD.Signature.PQC.pas',
  OBD.Flash.OEM.Common in '..\src\Flashing\OBD.Flash.OEM.Common.pas',
  OBD.Flash.OEM.VAG in '..\src\Flashing\OBD.Flash.OEM.VAG.pas',
  OBD.Flash.OEM.BMW in '..\src\Flashing\OBD.Flash.OEM.BMW.pas',
  OBD.Flash.OEM.Ford in '..\src\Flashing\OBD.Flash.OEM.Ford.pas',
  OBD.Flash.OEM.HMG in '..\src\Flashing\OBD.Flash.OEM.HMG.pas',
  OBD.Flash.OEM.Mercedes in '..\src\Flashing\OBD.Flash.OEM.Mercedes.pas',
  OBD.Flash.OEM.Stellantis in '..\src\Flashing\OBD.Flash.OEM.Stellantis.pas',
  OBD.Flash.OEM.Toyota in '..\src\Flashing\OBD.Flash.OEM.Toyota.pas',
  OBD.Flash.ImageApplicability in '..\src\Flashing\OBD.Flash.ImageApplicability.pas',
  OBD.Signature.HSM.PKCS11 in '..\src\Flashing\OBD.Signature.HSM.PKCS11.pas',
  OBD.Flash.OEM.Catalog in '..\src\Flashing\OBD.Flash.OEM.Catalog.pas',
  OBD.Calibration.A2L in '..\src\Calibration\OBD.Calibration.A2L.pas',
  OBD.Calibration.XCP.Transport in '..\src\Calibration\OBD.Calibration.XCP.Transport.pas',
  OBD.Calibration.XCP.Loopback in '..\src\Calibration\OBD.Calibration.XCP.Loopback.pas',
  OBD.Calibration.XCP in '..\src\Calibration\OBD.Calibration.XCP.pas',
  OBD.Calibration.CCP in '..\src\Calibration\OBD.Calibration.CCP.pas',
  OBD.Speciality.IsoBus in '..\src\Speciality\OBD.Speciality.IsoBus.pas',
  OBD.Speciality.IsoBus.VT in '..\src\Speciality\OBD.Speciality.IsoBus.VT.pas',
  OBD.Speciality.IsoBus.TC in '..\src\Speciality\OBD.Speciality.IsoBus.TC.pas',
  OBD.Speciality.IsoBus.FS in '..\src\Speciality\OBD.Speciality.IsoBus.FS.pas',
  OBD.Speciality.IsoBus.GNSS in '..\src\Speciality\OBD.Speciality.IsoBus.GNSS.pas',
  OBD.Speciality.Tachograph in '..\src\Speciality\OBD.Speciality.Tachograph.pas',
  OBD.Speciality.Tachograph.PCSC in '..\src\Speciality\OBD.Speciality.Tachograph.PCSC.pas',
  OBD.Recorder in '..\src\Recorder\OBD.Recorder.pas',
  OBD.Replayer in '..\src\Recorder\OBD.Replayer.pas',
  OBD.Recorder.ProtocolMock in '..\src\Recorder\OBD.Recorder.ProtocolMock.pas',
  OBD.Recorder.Redactor in '..\src\Recorder\OBD.Recorder.Redactor.pas',
  OBD.RadioCode.Types in '..\src\RadioCode\OBD.RadioCode.Types.pas',
  OBD.RadioCode in '..\src\RadioCode\OBD.RadioCode.pas',
  OBD.RadioCode.EuropeanPremium in '..\src\RadioCode\OBD.RadioCode.EuropeanPremium.pas',
  OBD.RadioCode.FrenchItalian in '..\src\RadioCode\OBD.RadioCode.FrenchItalian.pas',
  OBD.RadioCode.British in '..\src\RadioCode\OBD.RadioCode.British.pas',
  OBD.RadioCode.Asian in '..\src\RadioCode\OBD.RadioCode.Asian.pas',
  OBD.RadioCode.American in '..\src\RadioCode\OBD.RadioCode.American.pas',
  OBD.RadioCode.Aftermarket in '..\src\RadioCode\OBD.RadioCode.Aftermarket.pas',
  OBD.RadioCode.Volvo in '..\src\RadioCode\OBD.RadioCode.Volvo.pas',
  OBD.RadioCode.FordV in '..\src\RadioCode\OBD.RadioCode.FordV.pas',
  OBD.RadioCode.EEPROM in '..\src\RadioCode\OBD.RadioCode.EEPROM.pas',
  OBD.UI.Types         in '..\src\UI\OBD.UI.Types.pas',
  OBD.UI.Theme         in '..\src\UI\OBD.UI.Theme.pas',
  OBD.UI.Control       in '..\src\UI\OBD.UI.Control.pas',
  OBD.UI.Anim          in '..\src\UI\OBD.UI.Anim.pas',
  OBD.UI.Gauges.Types  in '..\src\UI\OBD.UI.Gauges.Types.pas',
  OBD.UI.Gauges.Base   in '..\src\UI\OBD.UI.Gauges.Base.pas',
  OBD.UI.Gauges.Dial   in '..\src\UI\OBD.UI.Gauges.Dial.pas',
  OBD.UI.Gauges.Linear in '..\src\UI\OBD.UI.Gauges.Linear.pas',
  OBD.UI.Gauges.Variants     in '..\src\UI\OBD.UI.Gauges.Variants.pas',
  OBD.UI.Gauges.Sparkline    in '..\src\UI\OBD.UI.Gauges.Sparkline.pas',
  OBD.UI.Gauges.DialExtended in '..\src\UI\OBD.UI.Gauges.DialExtended.pas',
  OBD.UI.Indicators          in '..\src\UI\OBD.UI.Indicators.pas',
  OBD.UI.Telltales           in '..\src\UI\OBD.UI.Telltales.pas',
  OBD.UI.Shift               in '..\src\UI\OBD.UI.Shift.pas',
  OBD.UI.Timing              in '..\src\UI\OBD.UI.Timing.pas',
  OBD.UI.Gauges.Specialised  in '..\src\UI\OBD.UI.Gauges.Specialised.pas',
  OBD.UI.LivePanels          in '..\src\UI\OBD.UI.LivePanels.pas',
  OBD.UI.LiveGrids           in '..\src\UI\OBD.UI.LiveGrids.pas',
  OBD.UI.Info                in '..\src\UI\OBD.UI.Info.pas',
  OBD.UI.MonitorEV           in '..\src\UI\OBD.UI.MonitorEV.pas',
  OBD.UI.Session             in '..\src\UI\OBD.UI.Session.pas',
  OBD.UI.Connection          in '..\src\UI\OBD.UI.Connection.pas',
  OBD.UI.Pickers             in '..\src\UI\OBD.UI.Pickers.pas',
  OBD.UI.FlashDashboards     in '..\src\UI\OBD.UI.FlashDashboards.pas',
  OBD.UI.Dyno                in '..\src\UI\OBD.UI.Dyno.pas',
  OBD.UI.Charts              in '..\src\UI\OBD.UI.Charts.pas',
  OBD.UI.Tuning              in '..\src\UI\OBD.UI.Tuning.pas',
  OBD.UI.Motorsport          in '..\src\UI\OBD.UI.Motorsport.pas',
  OBD.UI.Commercial          in '..\src\UI\OBD.UI.Commercial.pas',
  OBD.UI.Replay              in '..\src\UI\OBD.UI.Replay.pas',
  OBD.UI.Branding            in '..\src\UI\OBD.UI.Branding.pas',
  OBD.UI.CodingEditors       in '..\src\UI\OBD.UI.CodingEditors.pas',
  OBD.UI.Diag                in '..\src\UI\OBD.UI.Diag.pas',
  OBD.UI.SessionInspect      in '..\src\UI\OBD.UI.SessionInspect.pas',
  OBD.UI.Insights            in '..\src\UI\OBD.UI.Insights.pas',
  OBD.UI.Logger              in '..\src\UI\OBD.UI.Logger.pas',
  Tests.OBD.Version in 'Tests.OBD.Version.pas',
  Tests.OBD.Types in 'Tests.OBD.Types.pas',
  Tests.OBD.Errors in 'Tests.OBD.Errors.pas',
  Tests.OBD.Decoders in 'Tests.OBD.Decoders.pas',
  Tests.OBD.Catalog in 'Tests.OBD.Catalog.pas',
  Tests.OBD.Catalog.Inventory in 'Tests.OBD.Catalog.Inventory.pas',
  Tests.OBD.Connection.Mock in 'Tests.OBD.Connection.Mock.pas',
  Tests.OBD.Connection.Retry in 'Tests.OBD.Connection.Retry.pas',
  Tests.OBD.Connection in 'Tests.OBD.Connection.pas',
  Tests.OBD.Connection.Async in 'Tests.OBD.Connection.Async.pas',
  Tests.OBD.Connection.Progress in 'Tests.OBD.Connection.Progress.pas',
  Tests.OBD.Adapter.Commands in 'Tests.OBD.Adapter.Commands.pas',
  Tests.OBD.Adapter.Capabilities in 'Tests.OBD.Adapter.Capabilities.pas',
  Tests.OBD.Adapter.Detection in 'Tests.OBD.Adapter.Detection.pas',
  Tests.OBD.Adapter in 'Tests.OBD.Adapter.pas',
  Tests.OBD.Adapter.Followups in 'Tests.OBD.Adapter.Followups.pas',
  Tests.OBD.Protocol.Types in 'Tests.OBD.Protocol.Types.pas',
  Tests.OBD.Protocol.ISO15765 in 'Tests.OBD.Protocol.ISO15765.pas',
  Tests.OBD.Protocol.UDS in 'Tests.OBD.Protocol.UDS.pas',
  Tests.OBD.Protocol.J1939 in 'Tests.OBD.Protocol.J1939.pas',
  Tests.OBD.Protocol.Legacy in 'Tests.OBD.Protocol.Legacy.pas',
  Tests.OBD.Protocol in 'Tests.OBD.Protocol.pas',
  Tests.OBD.Protocol.VIN in 'Tests.OBD.Protocol.VIN.pas',
  Tests.OBD.Protocol.J1939.TP in 'Tests.OBD.Protocol.J1939.TP.pas',
  Tests.OBD.Protocol.DoIP in 'Tests.OBD.Protocol.DoIP.pas',
  Tests.OBD.Protocol.SecOC in 'Tests.OBD.Protocol.SecOC.pas',
  Tests.OBD.Protocol.LIN in 'Tests.OBD.Protocol.LIN.pas',
  Tests.OBD.Protocol.FlexRay in 'Tests.OBD.Protocol.FlexRay.pas',
  Tests.OBD.Protocol.MOST in 'Tests.OBD.Protocol.MOST.pas',
  Tests.OBD.Protocol.Integration in 'Tests.OBD.Protocol.Integration.pas',
  Tests.OBD.Service in 'Tests.OBD.Service.pas',
  Tests.OBD.Service.ComponentLifecycle in 'Tests.OBD.Service.ComponentLifecycle.pas',
  Tests.OBD.Service.Phase5 in 'Tests.OBD.Service.Phase5.pas',
  Tests.OBD.Diagnostics.UDS in 'Tests.OBD.Diagnostics.UDS.pas',
  Tests.OBD.Diagnostics.KWP in 'Tests.OBD.Diagnostics.KWP.pas',
  Tests.OBD.Diagnostics.J1939 in 'Tests.OBD.Diagnostics.J1939.pas',
  Tests.OBD.OEM in 'Tests.OBD.OEM.pas',
  Tests.OBD.Speciality in 'Tests.OBD.Speciality.pas',
  Tests.OBD.Service.Catalog in 'Tests.OBD.Service.Catalog.pas',
  Tests.OBD.Service.VINDecoder in 'Tests.OBD.Service.VINDecoder.pas',
  Tests.OBD.Service.VINInspector in 'Tests.OBD.Service.VINInspector.pas',
  Tests.OBD.Service.VehicleHealth in 'Tests.OBD.Service.VehicleHealth.pas',
  Tests.OBD.Service.DriveCycle in 'Tests.OBD.Service.DriveCycle.pas',
  Tests.OBD.Service.EVBattery in 'Tests.OBD.Service.EVBattery.pas',
  Tests.OBD.OEM.KeyAdaptation in 'Tests.OBD.OEM.KeyAdaptation.pas',
  Tests.OBD.RadioCode in 'Tests.OBD.RadioCode.pas',
  Tests.OBD.RadioCode.FrenchItalian in 'Tests.OBD.RadioCode.FrenchItalian.pas',
  Tests.OBD.RadioCode.Asian in 'Tests.OBD.RadioCode.Asian.pas',
  Tests.OBD.RadioCode.DBBacked in 'Tests.OBD.RadioCode.DBBacked.pas',
  Tests.OBD.RadioCode.StubVendors in 'Tests.OBD.RadioCode.StubVendors.pas',
  Tests.OBD.RadioCode.American in 'Tests.OBD.RadioCode.American.pas',
  Tests.OBD.RadioCode.EEPROM in 'Tests.OBD.RadioCode.EEPROM.pas',
  Tests.OBD.UI.Foundation in 'Tests.OBD.UI.Foundation.pas',
  Tests.OBD.UI.Gauges in 'Tests.OBD.UI.Gauges.pas',
  Tests.OBD.UI.Gauges.Variants in 'Tests.OBD.UI.Gauges.Variants.pas',
  Tests.OBD.UI.Indicators      in 'Tests.OBD.UI.Indicators.pas',
  Tests.OBD.UI.Telltales       in 'Tests.OBD.UI.Telltales.pas',
  Tests.OBD.UI.Shift           in 'Tests.OBD.UI.Shift.pas',
  Tests.OBD.UI.Timing          in 'Tests.OBD.UI.Timing.pas',
  Tests.OBD.UI.Gauges.Specialised in 'Tests.OBD.UI.Gauges.Specialised.pas',
  Tests.OBD.UI.LivePanels      in 'Tests.OBD.UI.LivePanels.pas',
  Tests.OBD.UI.LiveGrids       in 'Tests.OBD.UI.LiveGrids.pas',
  Tests.OBD.UI.Info            in 'Tests.OBD.UI.Info.pas',
  Tests.OBD.UI.MonitorEV       in 'Tests.OBD.UI.MonitorEV.pas',
  Tests.OBD.UI.Session         in 'Tests.OBD.UI.Session.pas',
  Tests.OBD.UI.Connection      in 'Tests.OBD.UI.Connection.pas',
  Tests.OBD.UI.Pickers         in 'Tests.OBD.UI.Pickers.pas',
  Tests.OBD.UI.FlashDashboards in 'Tests.OBD.UI.FlashDashboards.pas',
  Tests.OBD.UI.Dyno            in 'Tests.OBD.UI.Dyno.pas',
  Tests.OBD.UI.A2_12           in 'Tests.OBD.UI.A2_12.pas',
  Tests.OBD.Service.VWRadioSAFE in 'Tests.OBD.Service.VWRadioSAFE.pas',
  Tests.OBD.Protocol.KWP1281 in 'Tests.OBD.Protocol.KWP1281.pas',
  Tests.OBD.Protocol.KWP1281.TransportELM in 'Tests.OBD.Protocol.KWP1281.TransportELM.pas',
  Tests.OBD.Protocol.TP20 in 'Tests.OBD.Protocol.TP20.pas',
  Tests.OBD.Coding in 'Tests.OBD.Coding.pas',
  Tests.OBD.Calibration in 'Tests.OBD.Calibration.pas',
  Tests.OBD.Calibration.Followups in 'Tests.OBD.Calibration.Followups.pas',
  Tests.OBD.Coding.Advanced in 'Tests.OBD.Coding.Advanced.pas',
  Tests.OBD.Coding.Catalog in 'Tests.OBD.Coding.Catalog.pas',
  Tests.OBD.Flashing.Transfer in 'Tests.OBD.Flashing.Transfer.pas',
  Tests.OBD.Flashing.SafetyGates in 'Tests.OBD.Flashing.SafetyGates.pas',
  Tests.OBD.Flashing.Pipeline in 'Tests.OBD.Flashing.Pipeline.pas',
  Tests.OBD.Flashing.Signature in 'Tests.OBD.Flashing.Signature.pas',
  Tests.OBD.Flashing.OEMHandshakes in 'Tests.OBD.Flashing.OEMHandshakes.pas',
  Tests.OBD.Flashing.Followups in 'Tests.OBD.Flashing.Followups.pas',
  Tests.OBD.Recorder in 'Tests.OBD.Recorder.pas';

{$IFDEF CI}
var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  Exit;
{$ENDIF}
{$IFDEF CI}
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;
    Runner.FailsOnNoAsserts := False;

    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);

    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(
      TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);

    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
{$ELSE}
  TDUnitX.CheckCommandLine;
  TDUnitX.CreateRunner.Execute;
  Write('Press <Enter> to quit.');
  Readln;
{$ENDIF}
end.
