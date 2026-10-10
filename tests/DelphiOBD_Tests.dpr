//------------------------------------------------------------------------------
//  DelphiOBD_Tests
//
//  DUnitX runner program for the Delphi-OBD test suite.
//
//  Run from the IDE or from the command line. CI uses the console runner
//  via the {$IFDEF CI} branch below.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
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
  ERD.Version in '..\src\Core\ERD.Version.pas',
  ERD.Types in '..\src\Core\ERD.Types.pas',
  ERD.Errors in '..\src\Core\ERD.Errors.pas',
  ERD.Decoders in '..\src\Core\ERD.Decoders.pas',
  ERD.Catalog in '..\src\Core\ERD.Catalog.pas',
  ERD.JSON in '..\src\Core\ERD.JSON.pas',
  ERD.CAN.Route in '..\src\Core\ERD.CAN.Route.pas',
  ERD.Binary.Value in '..\src\Core\ERD.Binary.Value.pas',
  ERD.Service.EVBattery.Request in '..\src\Service\ERD.Service.EVBattery.Request.pas',
  Tests.ERD.JSON in 'Tests.ERD.JSON.pas',
  Tests.ERD.OEM.UdsValues in 'Tests.ERD.OEM.UdsValues.pas',
  Tests.ERD.Adapter.Routing in 'Tests.ERD.Adapter.Routing.pas',
  ERD.Connection.Types in '..\src\Connection\ERD.Connection.Types.pas',
  ERD.Connection.Settings in '..\src\Connection\ERD.Connection.Settings.pas',
  ERD.Connection.Retry in '..\src\Connection\ERD.Connection.Retry.pas',
  ERD.Connection.Transport.Base in '..\src\Connection\ERD.Connection.Transport.Base.pas',
  ERD.Connection.Mock in '..\src\Connection\ERD.Connection.Mock.pas',
  ERD.Connection.Bluetooth in '..\src\Connection\ERD.Connection.Bluetooth.pas',
  ERD.Connection.BLE in '..\src\Connection\ERD.Connection.BLE.pas',
  ERD.Connection.WiFi in '..\src\Connection\ERD.Connection.WiFi.pas',
  ERD.Connection.UDP in '..\src\Connection\ERD.Connection.UDP.pas',
  {$IFDEF MSWINDOWS}
  ERD.Connection.Serial in '..\src\Connection\ERD.Connection.Serial.pas',
  ERD.Connection.FTDI in '..\src\Connection\ERD.Connection.FTDI.pas',
  {$ENDIF}
  ERD.Connection in '..\src\Connection\ERD.Connection.pas',
  ERD.Adapter.Types in '..\src\Adapter\ERD.Adapter.Types.pas',
  ERD.Adapter.Capabilities in '..\src\Adapter\ERD.Adapter.Capabilities.pas',
  ERD.Adapter.Commands in '..\src\Adapter\ERD.Adapter.Commands.pas',
  ERD.Adapter.Detection in '..\src\Adapter\ERD.Adapter.Detection.pas',
  ERD.Adapter.Init in '..\src\Adapter\ERD.Adapter.Init.pas',
  ERD.Adapter in '..\src\Adapter\ERD.Adapter.pas',
  ERD.Protocol.Types in '..\src\Protocol\ERD.Protocol.Types.pas',
  ERD.Protocol.ISO15765 in '..\src\Protocol\ERD.Protocol.ISO15765.pas',
  ERD.Protocol.UDS in '..\src\Protocol\ERD.Protocol.UDS.pas',
  ERD.Protocol.KWP2000 in '..\src\Protocol\ERD.Protocol.KWP2000.pas',
  ERD.Protocol.KWP1281 in '..\src\Protocol\ERD.Protocol.KWP1281.pas',
  {$IFDEF MSWINDOWS}
  ERD.Protocol.KWP1281.Transport.Serial in '..\src\Protocol\ERD.Protocol.KWP1281.Transport.Serial.pas',
  {$ENDIF}
  ERD.Protocol.KWP1281.Transport.ELM in '..\src\Protocol\ERD.Protocol.KWP1281.Transport.ELM.pas',
  ERD.Protocol.CAN in '..\src\Protocol\ERD.Protocol.CAN.pas',
  ERD.Protocol.TP20 in '..\src\Protocol\ERD.Protocol.TP20.pas',
  ERD.Protocol.KWP1281.Transport.TP20 in '..\src\Protocol\ERD.Protocol.KWP1281.Transport.TP20.pas',
  ERD.Protocol.KWP1281.Transport.ISOTP in '..\src\Protocol\ERD.Protocol.KWP1281.Transport.ISOTP.pas',
  {$IFDEF MSWINDOWS}
  ERD.J2534 in '..\src\Protocol\ERD.J2534.pas',
  ERD.Protocol.KWP1281.Transport.J2534 in '..\src\Protocol\ERD.Protocol.KWP1281.Transport.J2534.pas',
  ERD.J2534.Components in '..\src\Protocol\ERD.J2534.Components.pas',
  {$ENDIF}
  ERD.Protocol.KWP1281.Session in '..\src\Protocol\ERD.Protocol.KWP1281.Session.pas',
  ERD.Protocol.TP20.Session in '..\src\Protocol\ERD.Protocol.TP20.Session.pas',
  ERD.Protocol.ISO9141 in '..\src\Protocol\ERD.Protocol.ISO9141.pas',
  ERD.Protocol.J1850 in '..\src\Protocol\ERD.Protocol.J1850.pas',
  ERD.Protocol.J1939 in '..\src\Protocol\ERD.Protocol.J1939.pas',
  ERD.Protocol.J1939.TP in '..\src\Protocol\ERD.Protocol.J1939.TP.pas',
  ERD.Protocol.VIN in '..\src\Protocol\ERD.Protocol.VIN.pas',
  ERD.Protocol.DoIP.Header in '..\src\Protocol\ERD.Protocol.DoIP.Header.pas',
  ERD.Protocol.DoIP.Messages in '..\src\Protocol\ERD.Protocol.DoIP.Messages.pas',
  ERD.Protocol.DoIP.Transport in '..\src\Protocol\ERD.Protocol.DoIP.Transport.pas',
  ERD.Protocol.DoIP.Client in '..\src\Protocol\ERD.Protocol.DoIP.Client.pas',
  ERD.Protocol.DoIP in '..\src\Protocol\ERD.Protocol.DoIP.pas',
  ERD.Protocol.SecOC.AES in '..\src\Protocol\ERD.Protocol.SecOC.AES.pas',
  ERD.Protocol.SecOC.CMAC in '..\src\Protocol\ERD.Protocol.SecOC.CMAC.pas',
  ERD.Protocol.SecOC.Keys in '..\src\Protocol\ERD.Protocol.SecOC.Keys.pas',
  ERD.Protocol.SecOC.Freshness in '..\src\Protocol\ERD.Protocol.SecOC.Freshness.pas',
  ERD.Protocol.SecOC in '..\src\Protocol\ERD.Protocol.SecOC.pas',
  ERD.Protocol.LIN.Frame in '..\src\Protocol\ERD.Protocol.LIN.Frame.pas',
  ERD.Protocol.LIN.LDF in '..\src\Protocol\ERD.Protocol.LIN.LDF.pas',
  ERD.Protocol.FlexRay.Frame in '..\src\Protocol\ERD.Protocol.FlexRay.Frame.pas',
  ERD.Protocol.MOST.Control in '..\src\Protocol\ERD.Protocol.MOST.Control.pas',
  ERD.Protocol in '..\src\Protocol\ERD.Protocol.pas',
  ERD.Service.Catalog in '..\src\Service\ERD.Service.Catalog.pas',
  ERD.Service.LiveData in '..\src\Service\ERD.Service.LiveData.pas',
  ERD.Service.Dyno in '..\src\Service\ERD.Service.Dyno.pas',
  ERD.Service.DTCs in '..\src\Service\ERD.Service.DTCs.pas',
  ERD.Service.VIN in '..\src\Service\ERD.Service.VIN.pas',
  ERD.Service.VINDecoder.Types in '..\src\Service\ERD.Service.VINDecoder.Types.pas',
  ERD.Service.VINDecoder in '..\src\Service\ERD.Service.VINDecoder.pas',
  ERD.Service.VINInspector in '..\src\Service\ERD.Service.VINInspector.pas',
  ERD.Service.VINDecoder.Catalog.Component in '..\src\Service\ERD.Service.VINDecoder.Catalog.Component.pas',
  ERD.Service.FreezeFrame in '..\src\Service\ERD.Service.FreezeFrame.pas',
  ERD.Service.OnBoardMonitor in '..\src\Service\ERD.Service.OnBoardMonitor.pas',
  ERD.Service.VehicleHealth in '..\src\Service\ERD.Service.VehicleHealth.pas',
  ERD.Service.DriveCycle.Types in '..\src\Service\ERD.Service.DriveCycle.Types.pas',
  ERD.Service.DriveCycle.Catalog in '..\src\Service\ERD.Service.DriveCycle.Catalog.pas',
  ERD.Service.DriveCycle in '..\src\Service\ERD.Service.DriveCycle.pas',
  ERD.Service.DriveCycle.Catalog.Component in '..\src\Service\ERD.Service.DriveCycle.Catalog.Component.pas',
  ERD.Service.EVBattery.Types in '..\src\Service\ERD.Service.EVBattery.Types.pas',
  ERD.Service.EVBattery.Catalog in '..\src\Service\ERD.Service.EVBattery.Catalog.pas',
  ERD.Service.EVBattery in '..\src\Service\ERD.Service.EVBattery.pas',
  ERD.Service.EVBattery.Catalog.Component in '..\src\Service\ERD.Service.EVBattery.Catalog.Component.pas',
  ERD.Service.Actuator in '..\src\Service\ERD.Service.Actuator.pas',
  ERD.Service.VWRadioSAFE in '..\src\Service\ERD.Service.VWRadioSAFE.pas',
  ERD.Coding.SecurityAccess in '..\src\Coding\ERD.Coding.SecurityAccess.pas',
  ERD.Coding.DataIdentifierIO in '..\src\Coding\ERD.Coding.DataIdentifierIO.pas',
  ERD.Coding.RoutineControl in '..\src\Coding\ERD.Coding.RoutineControl.pas',
  ERD.Coding.Flasher in '..\src\Coding\ERD.Coding.Flasher.pas',
  ERD.Coding.Uploader in '..\src\Coding\ERD.Coding.Uploader.pas',
  ERD.Coding.FlashSession in '..\src\Coding\ERD.Coding.FlashSession.pas',
  ERD.UDS.WriteMemory in '..\src\Coding\ERD.UDS.WriteMemory.pas',
  ERD.KWP.WriteID in '..\src\Coding\ERD.KWP.WriteID.pas',
  ERD.Coding.Diff in '..\src\Coding\ERD.Coding.Diff.pas',
  ERD.Coding.AuditLog in '..\src\Coding\ERD.Coding.AuditLog.pas',
  ERD.Coding.Session in '..\src\Coding\ERD.Coding.Session.pas',
  ERD.Coding.VAG in '..\src\Coding\ERD.Coding.VAG.pas',
  ERD.Coding.BMW in '..\src\Coding\ERD.Coding.BMW.pas',
  ERD.Coding.Ford in '..\src\Coding\ERD.Coding.Ford.pas',
  ERD.Coding.HMG in '..\src\Coding\ERD.Coding.HMG.pas',
  ERD.Coding.Honda in '..\src\Coding\ERD.Coding.Honda.pas',
  ERD.Coding.Mercedes in '..\src\Coding\ERD.Coding.Mercedes.pas',
  ERD.Coding.Stellantis in '..\src\Coding\ERD.Coding.Stellantis.pas',
  ERD.Coding.Toyota in '..\src\Coding\ERD.Coding.Toyota.pas',
  ERD.OEM.ComponentProtection.VAG in '..\src\Coding\ERD.OEM.ComponentProtection.VAG.pas',
  ERD.OEM.ComponentProtection.BMW in '..\src\Coding\ERD.OEM.ComponentProtection.BMW.pas',
  ERD.OEM.ComponentProtection.Mercedes in '..\src\Coding\ERD.OEM.ComponentProtection.Mercedes.pas',
  ERD.OEM.ComponentProtection.Stellantis in '..\src\Coding\ERD.OEM.ComponentProtection.Stellantis.pas',
  ERD.OEM.KeyAdaptation.Types in '..\src\OEM\ERD.OEM.KeyAdaptation.Types.pas',
  ERD.OEM.KeyAdaptation.Base in '..\src\OEM\ERD.OEM.KeyAdaptation.Base.pas',
  ERD.OEM.KeyAdaptation.Ford in '..\src\OEM\ERD.OEM.KeyAdaptation.Ford.pas',
  ERD.OEM.KeyAdaptation.HMG in '..\src\OEM\ERD.OEM.KeyAdaptation.HMG.pas',
  ERD.OEM.KeyAdaptation.BMW in '..\src\OEM\ERD.OEM.KeyAdaptation.BMW.pas',
  ERD.OEM.KeyAdaptation.Toyota in '..\src\OEM\ERD.OEM.KeyAdaptation.Toyota.pas',
  ERD.Coding.OptionCatalog in '..\src\Coding\ERD.Coding.OptionCatalog.pas',
  ERD.Coding.DiffRLE in '..\src\Coding\ERD.Coding.DiffRLE.pas',
  ERD.Coding.LabelFile.VAG in '..\src\Coding\ERD.Coding.LabelFile.VAG.pas',
  ERD.UDS.Transfer in '..\src\Flashing\ERD.UDS.Transfer.pas',
  ERD.J1939.MemoryAccess in '..\src\Flashing\ERD.J1939.MemoryAccess.pas',
  ERD.Flash.VoltageGate in '..\src\Flashing\ERD.Flash.VoltageGate.pas',
  ERD.Flash.Checkpoint in '..\src\Flashing\ERD.Flash.Checkpoint.pas',
  ERD.Flash.Phases in '..\src\Flashing\ERD.Flash.Phases.pas',
  ERD.Flash.Pipeline in '..\src\Flashing\ERD.Flash.Pipeline.pas',
  ERD.Signature in '..\src\Flashing\ERD.Signature.pas',
  ERD.Signature.BCrypt in '..\src\Flashing\ERD.Signature.BCrypt.pas',
  ERD.Signature.OpenSSL in '..\src\Flashing\ERD.Signature.OpenSSL.pas',
  ERD.Signature.HSM in '..\src\Flashing\ERD.Signature.HSM.pas',
  ERD.Signature.PQC in '..\src\Flashing\ERD.Signature.PQC.pas',
  ERD.Flash.OEM.Common in '..\src\Flashing\ERD.Flash.OEM.Common.pas',
  ERD.Flash.OEM.VAG in '..\src\Flashing\ERD.Flash.OEM.VAG.pas',
  ERD.Flash.OEM.BMW in '..\src\Flashing\ERD.Flash.OEM.BMW.pas',
  ERD.Flash.OEM.Ford in '..\src\Flashing\ERD.Flash.OEM.Ford.pas',
  ERD.Flash.OEM.HMG in '..\src\Flashing\ERD.Flash.OEM.HMG.pas',
  ERD.Flash.OEM.Mercedes in '..\src\Flashing\ERD.Flash.OEM.Mercedes.pas',
  ERD.Flash.OEM.Stellantis in '..\src\Flashing\ERD.Flash.OEM.Stellantis.pas',
  ERD.Flash.OEM.Toyota in '..\src\Flashing\ERD.Flash.OEM.Toyota.pas',
  ERD.Flash.ImageApplicability in '..\src\Flashing\ERD.Flash.ImageApplicability.pas',
  ERD.Signature.HSM.PKCS11 in '..\src\Flashing\ERD.Signature.HSM.PKCS11.pas',
  ERD.Flash.OEM.Catalog in '..\src\Flashing\ERD.Flash.OEM.Catalog.pas',
  ERD.Calibration.A2L in '..\src\Calibration\ERD.Calibration.A2L.pas',
  ERD.Calibration.XCP.Transport in '..\src\Calibration\ERD.Calibration.XCP.Transport.pas',
  ERD.Calibration.XCP.Loopback in '..\src\Calibration\ERD.Calibration.XCP.Loopback.pas',
  ERD.Calibration.XCP in '..\src\Calibration\ERD.Calibration.XCP.pas',
  ERD.Calibration.CCP in '..\src\Calibration\ERD.Calibration.CCP.pas',
  ERD.Speciality.IsoBus in '..\src\Speciality\ERD.Speciality.IsoBus.pas',
  ERD.Speciality.IsoBus.VT in '..\src\Speciality\ERD.Speciality.IsoBus.VT.pas',
  ERD.Speciality.IsoBus.TC in '..\src\Speciality\ERD.Speciality.IsoBus.TC.pas',
  ERD.Speciality.IsoBus.FS in '..\src\Speciality\ERD.Speciality.IsoBus.FS.pas',
  ERD.Speciality.IsoBus.GNSS in '..\src\Speciality\ERD.Speciality.IsoBus.GNSS.pas',
  ERD.Speciality.Tachograph in '..\src\Speciality\ERD.Speciality.Tachograph.pas',
  ERD.Speciality.Tachograph.PCSC in '..\src\Speciality\ERD.Speciality.Tachograph.PCSC.pas',
  ERD.Recorder in '..\src\Recorder\ERD.Recorder.pas',
  ERD.Replayer in '..\src\Recorder\ERD.Replayer.pas',
  ERD.Recorder.ProtocolMock in '..\src\Recorder\ERD.Recorder.ProtocolMock.pas',
  ERD.Recorder.Redactor in '..\src\Recorder\ERD.Recorder.Redactor.pas',
  ERD.RadioCode.Types in '..\src\RadioCode\ERD.RadioCode.Types.pas',
  ERD.RadioCode in '..\src\RadioCode\ERD.RadioCode.pas',
  ERD.RadioCode.EuropeanPremium in '..\src\RadioCode\ERD.RadioCode.EuropeanPremium.pas',
  ERD.RadioCode.FrenchItalian in '..\src\RadioCode\ERD.RadioCode.FrenchItalian.pas',
  ERD.RadioCode.British in '..\src\RadioCode\ERD.RadioCode.British.pas',
  ERD.RadioCode.Asian in '..\src\RadioCode\ERD.RadioCode.Asian.pas',
  ERD.RadioCode.American in '..\src\RadioCode\ERD.RadioCode.American.pas',
  ERD.RadioCode.Aftermarket in '..\src\RadioCode\ERD.RadioCode.Aftermarket.pas',
  ERD.RadioCode.Volvo in '..\src\RadioCode\ERD.RadioCode.Volvo.pas',
  ERD.RadioCode.FordV in '..\src\RadioCode\ERD.RadioCode.FordV.pas',
  ERD.RadioCode.EEPROM in '..\src\RadioCode\ERD.RadioCode.EEPROM.pas',
  ERD.UI.Types               in '..\src\UI\ERD.UI.Types.pas',
  ERD.UI.GDIP                in '..\src\UI\ERD.UI.GDIP.pas',
  ERD.UI.Theme               in '..\src\UI\ERD.UI.Theme.pas',
  ERD.UI.Control             in '..\src\UI\ERD.UI.Control.pas',
  ERD.UI.Anim                in '..\src\UI\ERD.UI.Anim.pas',
  ERD.UI.Units               in '..\src\UI\ERD.UI.Units.pas',
  ERD.UI.Binding             in '..\src\UI\ERD.UI.Binding.pas',
  ERD.UI.Gauges.Types        in '..\src\UI\ERD.UI.Gauges.Types.pas',
  ERD.UI.Gauges.Base         in '..\src\UI\ERD.UI.Gauges.Base.pas',
  ERD.UI.Gauges.Dial         in '..\src\UI\ERD.UI.Gauges.Dial.pas',
  ERD.UI.Gauges.Bar          in '..\src\UI\ERD.UI.Gauges.Bar.pas',
  ERD.UI.ValueTile           in '..\src\UI\ERD.UI.ValueTile.pas',
  ERD.UI.TrendChart          in '..\src\UI\ERD.UI.TrendChart.pas',
  ERD.UI.LiveDataGrid        in '..\src\UI\ERD.UI.LiveDataGrid.pas',
  ERD.UI.StatusLamp          in '..\src\UI\ERD.UI.StatusLamp.pas',
  ERD.UI.ConnectionBar       in '..\src\UI\ERD.UI.ConnectionBar.pas',
  ERD.UI.MatrixDisplay       in '..\src\UI\ERD.UI.MatrixDisplay.pas',
  ERD.UI.Dashboard           in '..\src\UI\ERD.UI.Dashboard.pas',
  ERD.UI.Terminal            in '..\src\UI\ERD.UI.Terminal.pas',
  ERD.UI.LogViewer           in '..\src\UI\ERD.UI.LogViewer.pas',
  ERD.UI.DtcList             in '..\src\UI\ERD.UI.DtcList.pas',
  ERD.UI.Pickers             in '..\src\UI\ERD.UI.Pickers.pas',
  ERD.UI.Paint               in '..\src\UI\ERD.UI.Paint.pas',
  ERD.UI.PopupList           in '..\src\UI\ERD.UI.PopupList.pas',
  ERD.UI.Card                in '..\src\UI\ERD.UI.Card.pas',
  ERD.UI.Buttons             in '..\src\UI\ERD.UI.Buttons.pas',
  ERD.UI.Chips               in '..\src\UI\ERD.UI.Chips.pas',
  ERD.UI.Edits               in '..\src\UI\ERD.UI.Edits.pas',
  ERD.UI.Segmented           in '..\src\UI\ERD.UI.Segmented.pas',
  ERD.UI.RangeBar            in '..\src\UI\ERD.UI.RangeBar.pas',
  ERD.UI.RangeProfiles       in '..\src\UI\ERD.UI.RangeProfiles.pas',
  ERD.UI.Inspector           in '..\src\UI\ERD.UI.Inspector.pas',
  ERD.UI.Sidebar             in '..\src\UI\ERD.UI.Sidebar.pas',
  ERD.UI.VehicleCard         in '..\src\UI\ERD.UI.VehicleCard.pas',
  ERD.UI.DtcPanel            in '..\src\UI\ERD.UI.DtcPanel.pas',
  ERD.UI.ReadinessPanel      in '..\src\UI\ERD.UI.ReadinessPanel.pas',
  ERD.UI.FreezeFrameView     in '..\src\UI\ERD.UI.FreezeFrameView.pas',
  ERD.UI.RangeEditor         in '..\src\UI\ERD.UI.RangeEditor.pas',
  ERD.UI.Menus               in '..\src\UI\ERD.UI.Menus.pas',
  ERD.UI.TitleBar            in '..\src\UI\ERD.UI.TitleBar.pas',
  ERD.UI.Ribbon              in '..\src\UI\ERD.UI.Ribbon.pas',
  ERD.UI.Backstage           in '..\src\UI\ERD.UI.Backstage.pas',
  ERD.UI.Tabs                in '..\src\UI\ERD.UI.Tabs.pas',
  ERD.UI.ToolBar             in '..\src\UI\ERD.UI.ToolBar.pas',
  ERD.UI.StatusBar           in '..\src\UI\ERD.UI.StatusBar.pas',
  ERD.UI.Progress            in '..\src\UI\ERD.UI.Progress.pas',
  ERD.UI.Dialogs             in '..\src\UI\ERD.UI.Dialogs.pas',
  ERD.UI.Toast               in '..\src\UI\ERD.UI.Toast.pas',
  ERD.UI.Hint                in '..\src\UI\ERD.UI.Hint.pas',
  ERD.UI.ScrollBar           in '..\src\UI\ERD.UI.ScrollBar.pas',
  Tests.ERD.Version in 'Tests.ERD.Version.pas',
  Tests.ERD.Types in 'Tests.ERD.Types.pas',
  Tests.ERD.Errors in 'Tests.ERD.Errors.pas',
  Tests.ERD.Decoders in 'Tests.ERD.Decoders.pas',
  Tests.ERD.Catalog in 'Tests.ERD.Catalog.pas',
  Tests.ERD.Catalog.Inventory in 'Tests.ERD.Catalog.Inventory.pas',
  Tests.ERD.Connection.Mock in 'Tests.ERD.Connection.Mock.pas',
  Tests.ERD.Connection.Retry in 'Tests.ERD.Connection.Retry.pas',
  Tests.ERD.Connection in 'Tests.ERD.Connection.pas',
  Tests.ERD.Connection.Async in 'Tests.ERD.Connection.Async.pas',
  Tests.ERD.Connection.Progress in 'Tests.ERD.Connection.Progress.pas',
  Tests.ERD.Adapter.Commands in 'Tests.ERD.Adapter.Commands.pas',
  Tests.ERD.Adapter.Capabilities in 'Tests.ERD.Adapter.Capabilities.pas',
  Tests.ERD.Adapter.Detection in 'Tests.ERD.Adapter.Detection.pas',
  Tests.ERD.Adapter in 'Tests.ERD.Adapter.pas',
  Tests.ERD.Adapter.Followups in 'Tests.ERD.Adapter.Followups.pas',
  Tests.ERD.Protocol.Types in 'Tests.ERD.Protocol.Types.pas',
  Tests.ERD.Protocol.ISO15765 in 'Tests.ERD.Protocol.ISO15765.pas',
  Tests.ERD.Protocol.UDS in 'Tests.ERD.Protocol.UDS.pas',
  Tests.ERD.Protocol.J1939 in 'Tests.ERD.Protocol.J1939.pas',
  Tests.ERD.Protocol.Legacy in 'Tests.ERD.Protocol.Legacy.pas',
  Tests.ERD.Protocol in 'Tests.ERD.Protocol.pas',
  Tests.ERD.Protocol.VIN in 'Tests.ERD.Protocol.VIN.pas',
  Tests.ERD.Protocol.J1939.TP in 'Tests.ERD.Protocol.J1939.TP.pas',
  Tests.ERD.Protocol.DoIP in 'Tests.ERD.Protocol.DoIP.pas',
  Tests.ERD.Protocol.SecOC in 'Tests.ERD.Protocol.SecOC.pas',
  Tests.ERD.Protocol.LIN in 'Tests.ERD.Protocol.LIN.pas',
  Tests.ERD.Protocol.FlexRay in 'Tests.ERD.Protocol.FlexRay.pas',
  Tests.ERD.Protocol.MOST in 'Tests.ERD.Protocol.MOST.pas',
  Tests.ERD.Protocol.Integration in 'Tests.ERD.Protocol.Integration.pas',
  Tests.ERD.Service in 'Tests.ERD.Service.pas',
  Tests.ERD.Service.ComponentLifecycle in 'Tests.ERD.Service.ComponentLifecycle.pas',
  Tests.ERD.Service.Extras in 'Tests.ERD.Service.Extras.pas',
  Tests.ERD.Diagnostics.UDS in 'Tests.ERD.Diagnostics.UDS.pas',
  Tests.ERD.Diagnostics.KWP in 'Tests.ERD.Diagnostics.KWP.pas',
  Tests.ERD.Diagnostics.J1939 in 'Tests.ERD.Diagnostics.J1939.pas',
  Tests.ERD.OEM in 'Tests.ERD.OEM.pas',
  Tests.ERD.OEM.Extensions in 'Tests.ERD.OEM.Extensions.pas',
  Tests.ERD.OEM.CatalogLoader in 'Tests.ERD.OEM.CatalogLoader.pas',
  Tests.ERD.OEM.CatalogCSV in 'Tests.ERD.OEM.CatalogCSV.pas',
  Tests.ERD.OEM.Support in 'Tests.ERD.OEM.Support.pas',
  Tests.ERD.OEM.Routines in 'Tests.ERD.OEM.Routines.pas',
  Tests.ERD.OEM.ServiceRoutines in 'Tests.ERD.OEM.ServiceRoutines.pas',
  Tests.ERD.OEM.SessionHelper in 'Tests.ERD.OEM.SessionHelper.pas',
  Tests.ERD.OEM.Captures in 'Tests.ERD.OEM.Captures.pas',
  Tests.ERD.UDS.NRC in 'Tests.ERD.UDS.NRC.pas',
  Tests.ERD.Async in 'Tests.ERD.Async.pas',
  Tests.ERD.Speciality in 'Tests.ERD.Speciality.pas',
  Tests.ERD.Service.Catalog in 'Tests.ERD.Service.Catalog.pas',
  Tests.ERD.Service.VINDecoder in 'Tests.ERD.Service.VINDecoder.pas',
  Tests.ERD.Service.VINInspector in 'Tests.ERD.Service.VINInspector.pas',
  Tests.ERD.Service.VehicleHealth in 'Tests.ERD.Service.VehicleHealth.pas',
  Tests.ERD.Service.DriveCycle in 'Tests.ERD.Service.DriveCycle.pas',
  Tests.ERD.Service.EVBattery in 'Tests.ERD.Service.EVBattery.pas',
  Tests.ERD.OEM.KeyAdaptation in 'Tests.ERD.OEM.KeyAdaptation.pas',
  Tests.ERD.RadioCode in 'Tests.ERD.RadioCode.pas',
  Tests.ERD.RadioCode.FrenchItalian in 'Tests.ERD.RadioCode.FrenchItalian.pas',
  Tests.ERD.RadioCode.Asian in 'Tests.ERD.RadioCode.Asian.pas',
  Tests.ERD.RadioCode.DBBacked in 'Tests.ERD.RadioCode.DBBacked.pas',
  Tests.ERD.RadioCode.StubVendors in 'Tests.ERD.RadioCode.StubVendors.pas',
  Tests.ERD.RadioCode.American in 'Tests.ERD.RadioCode.American.pas',
  Tests.ERD.RadioCode.EEPROM in 'Tests.ERD.RadioCode.EEPROM.pas',
  Tests.ERD.UI.Foundation      in 'Tests.ERD.UI.Foundation.pas',
  Tests.ERD.UI.LegacyPorts     in 'Tests.ERD.UI.LegacyPorts.pas',
  Tests.ERD.UI.Pickers         in 'Tests.ERD.UI.Pickers.pas',
  Tests.ERD.UI.RenderHelpers   in 'Tests.ERD.UI.RenderHelpers.pas',
  Tests.ERD.UI.Binding         in 'Tests.ERD.UI.Binding.pas',
  Tests.ERD.UI.Gauges          in 'Tests.ERD.UI.Gauges.pas',
  Tests.ERD.UI.Panels          in 'Tests.ERD.UI.Panels.pas',
  Tests.ERD.UI.Dashboard       in 'Tests.ERD.UI.Dashboard.pas',
  Tests.ERD.UI.Studio          in 'Tests.ERD.UI.Studio.pas',
  Tests.ERD.Service.Dyno       in 'Tests.ERD.Service.Dyno.pas',
  Tests.ERD.Tachograph         in 'Tests.ERD.Tachograph.pas',
  Tests.ERD.Utilities          in 'Tests.ERD.Utilities.pas',
  Tests.ERD.Service.VWRadioSAFE in 'Tests.ERD.Service.VWRadioSAFE.pas',
  Tests.ERD.Protocol.KWP1281 in 'Tests.ERD.Protocol.KWP1281.pas',
  Tests.ERD.Protocol.KWP1281.TransportELM in 'Tests.ERD.Protocol.KWP1281.TransportELM.pas',
  Tests.ERD.Protocol.TP20 in 'Tests.ERD.Protocol.TP20.pas',
  Tests.ERD.Coding in 'Tests.ERD.Coding.pas',
  Tests.ERD.Calibration in 'Tests.ERD.Calibration.pas',
  Tests.ERD.Calibration.Followups in 'Tests.ERD.Calibration.Followups.pas',
  Tests.ERD.Coding.Advanced in 'Tests.ERD.Coding.Advanced.pas',
  Tests.ERD.Coding.Catalog in 'Tests.ERD.Coding.Catalog.pas',
  Tests.ERD.Flashing.Transfer in 'Tests.ERD.Flashing.Transfer.pas',
  Tests.ERD.Flashing.SafetyGates in 'Tests.ERD.Flashing.SafetyGates.pas',
  Tests.ERD.Flashing.Pipeline in 'Tests.ERD.Flashing.Pipeline.pas',
  Tests.ERD.Flashing.Signature in 'Tests.ERD.Flashing.Signature.pas',
  Tests.ERD.Flashing.OEMHandshakes in 'Tests.ERD.Flashing.OEMHandshakes.pas',
  Tests.ERD.Flashing.Followups in 'Tests.ERD.Flashing.Followups.pas',
  Tests.ERD.Recorder in 'Tests.ERD.Recorder.pas';

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
