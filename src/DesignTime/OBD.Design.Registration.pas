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
//    2026-05-09  ERD  Initial empty registration, Phase 0 skeleton.
//    2026-05-09  ERD  Phase 4g — register TOBDConnection, TOBDAdapter,
//                     TOBDProtocol, TOBDDoIPClient, TOBDSecOCCodec.
//
//  Future work :
//    - Component icons (16 / 24 / 32 px) once design assets land.
//    - Property editors for adapter init scripts and SecOC keys.
//    - Splash bitmap + About box.
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
  OBD.Service.FreezeFrame,
  OBD.Service.OnBoardMonitor,
  OBD.Service.Actuator,
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
  OBD.UDS.Transfer,
  OBD.Flash.VoltageGate,
  OBD.Flash.Pipeline,
  OBD.Recorder,
  OBD.Replayer;

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

  // Service-mode (Phase 5): higher-level diagnostics that sit on
  // top of TOBDProtocol.
  RegisterComponents('OBD Services', [
    TOBDLiveData,
    TOBDDTCs,
    TOBDVIN,
    TOBDFreezeFrame,
    TOBDOnBoardMonitor,
    TOBDActuator
  ]);

  // Coding & flashing (Phase 6): write-side UDS components. Live
  // on their own tab so a host can keep them visually separated
  // from the read-only service-mode components.
  RegisterComponents('OBD Coding', [
    TOBDSecurityAccess,
    TOBDDataIdentifierIO,
    TOBDRoutineControl,
    TOBDFlasher,
    TOBDUploader,
    TOBDFlashSession
  ]);

  // Phase 7: calibration + speciality buses. XCP and CCP need a
  // transport injected at runtime; IsoBus is component-friendly
  // out of the box.
  RegisterComponents('OBD Calibration', [
    TOBDXCP,
    TOBDCCP,
    TOBDIsoBus
  ]);

  // Phase 8 additions: extra write-side components and the
  // session orchestrator.
  RegisterComponents('OBD Coding', [
    TOBDUDSWriteMemory,
    TOBDKWPWriteID,
    TOBDCodingAuditLog,
    TOBDCodingSession,
    TOBDComponentProtectionVAG,
    TOBDComponentProtectionBMW,
    TOBDComponentProtectionMercedes,
    TOBDComponentProtectionStellantis
  ]);

  // Phase 9: flashing components. WARNING — drop on a form, wire
  // OnConfirmExecute, leave AutoExecute = False until the host
  // really means it. Read docs/flashing-safety.md.
  RegisterComponents('OBD Flashing', [
    TOBDUDSTransfer,
    TOBDVoltageGate,
    TOBDFlashPipeline
  ]);

  // Phase 10: recorder / replayer pair. Drop on a form, point at
  // a TOBDProtocol, capture the entire session for forensic /
  // offline analysis or replay for tests.
  RegisterComponents('OBD', [
    TOBDRecorder,
    TOBDReplayer
  ]);

  // Phase 11: register the splash-screen plugin entry and the
  // About-box plugin info. Both are best-effort — wrapped so a
  // missing IDE service or a missing resource never breaks the
  // package install.
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
end;

end.
