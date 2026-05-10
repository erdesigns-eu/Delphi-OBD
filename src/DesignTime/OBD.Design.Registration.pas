//------------------------------------------------------------------------------
//  OBD.Design.Registration
//
//  Design-time registration entry point for the Delphi-OBD package.
//
//  This unit is linked into DelphiOBD_DT.bpl only; it must never be
//  referenced from runtime code. The IDE calls <c>Register</c> when the
//  design-time package is installed.
//
//  Component categories used:
//    - "OBD"        — non-visual diagnostic and protocol components
//    - "OBD OEM"    — vendor-specific coding helpers
//    - "OBD Visual" — reserved for future visual companions (currently
//                     empty by design — v1 is headless)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial empty registration.
//    2026-05-09  ERD  Register the Connection / Adapter / Protocol
//                     / DoIP / SecOC components.
//    2026-05-10  ERD  Add splash + About-box registration via Tools API.
//
//  Future work :
//    - Property editors for adapter init scripts and SecOC keys.
//------------------------------------------------------------------------------

unit OBD.Design.Registration;

interface

/// <summary>
///   Called by the IDE when DelphiOBD_DT.bpl is installed. Registers
///   every palette component shipped by Delphi-OBD on the "OBD" tab.
/// </summary>
procedure Register;

implementation

{$R DelphiOBD_DT.res}

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Imaging.PngImage,
  ToolsAPI,
  OBD.Connection,
  OBD.Adapter,
  OBD.Protocol,
  OBD.Protocol.DoIP.Client,
  OBD.Protocol.SecOC,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.Service.VIN,
  OBD.Service.VINInspector,
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor,
  OBD.Service.VehicleHealth,
  OBD.Service.DriveCycle,
  OBD.Service.EVBattery,
  OBD.Service.Actuator,
  OBD.Service.VWRadioSAFE,
  OBD.Coding.SecurityAccess,
  OBD.Coding.DataIdentifierIO,
  OBD.Coding.RoutineControl,
  OBD.Coding.Flasher,
  OBD.Coding.Uploader,
  OBD.Coding.FlashSession,
  OBD.Calibration.XCP,
  OBD.Calibration.CCP,
  OBD.Speciality.IsoBus,
  OBD.UDS.WriteMemory,
  OBD.KWP.WriteID,
  OBD.Coding.AuditLog,
  OBD.Coding.Session,
  OBD.OEM.ComponentProtection.VAG,
  OBD.OEM.ComponentProtection.BMW,
  OBD.OEM.ComponentProtection.Mercedes,
  OBD.OEM.ComponentProtection.Stellantis,
  OBD.OEM.KeyAdaptation.Ford,
  OBD.OEM.KeyAdaptation.HMG,
  OBD.OEM.KeyAdaptation.BMW,
  OBD.OEM.KeyAdaptation.Toyota,
  OBD.UDS.Transfer,
  OBD.Flash.VoltageGate,
  OBD.Flash.Pipeline,
  OBD.Recorder,
  OBD.Replayer,
  OBD.RadioCode,
  OBD.RadioCode.EuropeanPremium,
  OBD.RadioCode.FrenchItalian,
  OBD.RadioCode.British,
  OBD.RadioCode.Asian,
  OBD.RadioCode.American,
  OBD.RadioCode.Aftermarket,
  OBD.RadioCode.Volvo,
  OBD.RadioCode.FordVDatabase,
  OBD.RadioCode.EEPROM,
  OBD.Design.Editors,
  OBD.Design.Wizards.Starters;

/// <summary>Loads a PNG (kept transparent) from an RCDATA
/// resource into a fresh <c>TPngImage</c>. Returns nil when the
/// resource is missing.</summary>
function LoadPngResource(const AResName: string): TPngImage;
var
  Stream: TResourceStream;
begin
  Result := nil;
  if FindResource(HInstance, PChar(AResName), RT_RCDATA) = 0 then
    Exit;
  Stream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    Result := TPngImage.Create;
    Result.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

/// <summary>The IDE's splash and About-box services accept an
/// <c>HBITMAP</c>, not a PNG. The splash and About PNGs ship
/// with their own opaque charcoal backgrounds so we just blit
/// them onto a fresh <c>TBitmap</c> — no fill required.</summary>
function PngToBitmap(APng: TPngImage): Vcl.Graphics.TBitmap;
begin
  Result := Vcl.Graphics.TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(APng.Width, APng.Height);
  Result.Canvas.Draw(0, 0, APng);
end;

/// <summary>Registers the Delphi-OBD splash entry on the RAD
/// Studio splash screen. No-op when the IDE doesn't expose
/// <c>SplashScreenServices</c>.</summary>
procedure RegisterSplash;
var
  Png: TPngImage;
  Bmp: Vcl.Graphics.TBitmap;
begin
  if SplashScreenServices = nil then Exit;
  Png := LoadPngResource('SPLASH');
  if Png = nil then Exit;
  try
    Bmp := PngToBitmap(Png);
    try
      SplashScreenServices.AddPluginBitmap(
        'Delphi-OBD',
        Bmp.Handle,
        False,
        'MIT licensed — diagnostics, coding, flashing.',
        'v2 by ERDesigns');
    finally
      Bmp.Free;
    end;
  finally
    Png.Free;
  end;
end;

/// <summary>Registers the Delphi-OBD About-box entry. No-op
/// when <c>IOTAAboutBoxServices</c> is unavailable (older IDE
/// builds or stripped Tools API).</summary>
procedure RegisterAbout;
var
  Png: TPngImage;
  Bmp: Vcl.Graphics.TBitmap;
  Svc: IOTAAboutBoxServices;
begin
  if not Supports(BorlandIDEServices, IOTAAboutBoxServices, Svc) then Exit;
  Png := LoadPngResource('ABOUT');
  if Png = nil then Exit;
  try
    Bmp := PngToBitmap(Png);
    try
      Svc.AddPluginInfo(
        'Delphi-OBD',
        'Vehicle diagnostics, coding, and ECU flashing — MIT licensed.' +
          sLineBreak +
          '(c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors.',
        Bmp.Handle,
        False,
        '');
    finally
      Bmp.Free;
    end;
  finally
    Png.Free;
  end;
end;

procedure Register;
begin
  // Lower-level building blocks: connection, adapter, protocol,
  // network and security.
  RegisterComponents('OBD', [
    TOBDConnection,
    TOBDAdapter,
    TOBDProtocol,
    TOBDDoIPClient,
    TOBDSecOCCodec
  ]);

  // Service-mode: higher-level diagnostics that sit on top of
  // TOBDProtocol.
  RegisterComponents('OBD Services', [
    TOBDLiveData,
    TOBDDTCs,
    TOBDVIN,
    TOBDVINInspector,
    TOBDFreezeFrame,
    TOBDOnBoardMonitor,
    TOBDActuator,
    TOBDVehicleHealth,
    TOBDDriveCycleAdvisor,
    TOBDEVBattery
  ]);

  // Coding & flashing: write-side UDS components. On their own
  // tab so a host can keep them visually separated from the
  // read-only service-mode components.
  RegisterComponents('OBD Coding', [
    TOBDSecurityAccess,
    TOBDDataIdentifierIO,
    TOBDRoutineControl,
    TOBDFlasher,
    TOBDUploader,
    TOBDFlashSession
  ]);

  // Calibration + speciality buses. XCP and CCP need a transport
  // injected at runtime; IsoBus is component-friendly out of the
  // box.
  RegisterComponents('OBD Calibration', [
    TOBDXCP,
    TOBDCCP,
    TOBDIsoBus
  ]);

  // Extra write-side components and the session orchestrator.
  RegisterComponents('OBD Coding', [
    TOBDUDSWriteMemory,
    TOBDKWPWriteID,
    TOBDCodingAuditLog,
    TOBDCodingSession,
    TOBDComponentProtectionVAG,
    TOBDComponentProtectionBMW,
    TOBDComponentProtectionMercedes,
    TOBDComponentProtectionStellantis,
    TOBDKeyAdaptationFord,
    TOBDKeyAdaptationHMG,
    TOBDKeyAdaptationBMW,
    TOBDKeyAdaptationToyota
  ]);

  // Flashing components. WARNING — drop on a form, wire
  // OnConfirmExecute, leave AutoExecute = False until the host
  // really means it. Read docs/flashing-safety.md.
  RegisterComponents('OBD Flashing', [
    TOBDUDSTransfer,
    TOBDVoltageGate,
    TOBDFlashPipeline
  ]);

  // Recorder / replayer pair. Drop on a form, point at a
  // TOBDProtocol, capture the entire session for forensic /
  // offline analysis or replay for tests.
  RegisterComponents('OBD', [
    TOBDRecorder,
    TOBDReplayer
  ]);

  // Radio-code calculators (one component per vendor). Each
  // validates the input shape; production algorithms are
  // proprietary / licensed and supplied by the host via the
  // OnCalculate event. See docs/v2-port-backlog.md (P-A1).
  RegisterComponents('OBD Radio', [
    TOBDRadioCodeVW,
    TOBDRadioCodeAudiConcert,
    TOBDRadioCodeBMW,
    TOBDRadioCodeMercedes,
    TOBDRadioCodeMini,
    TOBDRadioCodePorsche,
    TOBDRadioCodeSEAT,
    TOBDRadioCodeSkoda,
    TOBDRadioCodeSmart,
    TOBDRadioCodeCitroen,
    TOBDRadioCodePeugeot,
    TOBDRadioCodeRenault,
    TOBDRadioCodeFiatDaiichi,
    TOBDRadioCodeFiatVP,
    TOBDRadioCodeAlfaRomeo,
    TOBDRadioCodeMaserati,
    TOBDRadioCodeJaguar,
    TOBDRadioCodeLandRover,
    TOBDRadioCodeSaab,
    TOBDRadioCodeOpel,
    TOBDRadioCodeAcura,
    TOBDRadioCodeHonda,
    TOBDRadioCodeHyundai,
    TOBDRadioCodeInfiniti,
    TOBDRadioCodeLexus,
    TOBDRadioCodeMazda,
    TOBDRadioCodeMitsubishi,
    TOBDRadioCodeNissan,
    TOBDRadioCodeSubaru,
    TOBDRadioCodeSuzuki,
    TOBDRadioCodeToyota,
    TOBDRadioCodeChrysler,
    TOBDRadioCodeFordM,
    TOBDRadioCodeGM,
    TOBDRadioCodeVisteon,
    TOBDRadioCodeAlpine,
    TOBDRadioCodeBlaupunkt,
    TOBDRadioCodeClarion,
    TOBDRadioCodeBecker4,
    TOBDRadioCodeBecker5,
    TOBDRadioCodeVolvo,
    TOBDRadioCodeFordV
  ]);

  // EEPROM-dump extractors. Different beast from the calculator
  // family above: the host pulls the radio's serial-EEPROM with
  // a chip programmer, hands the .bin to the matching component,
  // and the component reads the code out at a fixed offset. No
  // algorithm, no licensed service.
  RegisterComponents('OBD EEPROM', [
    TOBDRadioCodeEEPROM_VolvoHU,
    TOBDRadioCodeEEPROM_OpelCD30,
    TOBDRadioCodeEEPROM_MercedesBecker
  ]);

  // VW group SAFE-code recovery over the diagnostic bus
  // (KWP1281). Goes on the OBD Radio tab because the host-facing
  // shape is "give me the unlock code" - even though under the
  // hood it talks to the radio rather than running an algorithm
  // or reading a chip dump.
  RegisterComponents('OBD Radio', [
    TOBDVWRadioSAFE
  ]);

  // Register the splash-screen plugin entry and the About-box
  // plugin info. Both are best-effort — wrapped so a missing IDE
  // service or a missing resource never breaks the package
  // install.
  try
    RegisterSplash;
  except
    // Swallow — splash registration is decoration only.
  end;
  try
    RegisterAbout;
  except
    // Swallow — About-box registration is decoration only.
  end;

  // Property + component editors. Best-effort for the same
  // reason — if a host's RAD strips DesignEditors / DesignIntf
  // unexpectedly, the package install still succeeds.
  try
    RegisterDelphiOBDEditors;
  except
    // Swallow — editors are ergonomic only.
  end;

  // Project starter wizard (File > New > Other > Delphi-OBD).
  // Best-effort — the IDE service might be missing in custom
  // builds.
  try
    RegisterDelphiOBDStarterWizard;
  except
    // Swallow — wizard is ergonomic only.
  end;
end;

end.
