//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Register.pas
// CONTENTS       : Register custom components
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 22/03/2024
//------------------------------------------------------------------------------
unit OBD.CustomControl.Register;

interface

uses WinApi.Windows, System.Classes, System.SysUtils, DesignIntf, DesignEditors, ToolsAPI;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  ComponentPage      = 'ERDesigns OBD';
  PackageCopyright   = 'ERDesigns - Ernst Reidinga © 2024';
  PackageName        = 'ERDesigns OBD Framework';
  PackageDescription = 'ERDesigns OBD is a Framework including components to create OBD (On Board Diagnostics) applications for vehicles.' + #13#10#13#10 + PackageCopyright;
  PackageLicense     = 'Open source under Apache 2.0 library';

procedure Register;

implementation

uses
  OBD.CircularGauge, OBD.MatrixDisplay, OBD.LED, OBD.Gauge.Component,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.Form, OBD.DataModule,
  OBD.Connection.Component, OBD.Protocol.Component, OBD.Header.Component, OBD.Subheader.Component,
  OBD.Project.Wizard, OBD.Mainform.Wizard, OBD.Form.Wizard, OBD.DataModule.Wizard;

//------------------------------------------------------------------------------
// ADD ICON AND INFO TO ABOUT BOX
//------------------------------------------------------------------------------
procedure AddAboutBox;
var
  AboutBitmap: HBITMAP;
  AboutBoxServices: IOTAAboutBoxServices;
begin
  // Load the bitmap for the about box
  AboutBitmap := LoadBitmap(hInstance, 'PACKAGE');
  // Check if aboutbox services is supported
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
  begin
    // Add to the about box
    AboutBoxServices.AddPluginInfo(PackageName, PackageDescription, AboutBitmap, False, PackageLicense);
  end;
end;

//------------------------------------------------------------------------------
// ADD ICON AND INFO TO SPLASH SCREEN
//------------------------------------------------------------------------------
procedure AddSplashScreen;
var
  SplashBitmap: HBITMAP;
begin
  // Load the bitmap for the Splash Screen
  SplashBitmap := LoadBitmap(hInstance, 'PACKAGE');
  if SplashBitmap <> 0 then
  begin
    // Add to the splash screen
    SplashScreenServices.AddPluginBitmap(PackageName, SplashBitmap, False, PackageLicense);
  end
end;

//------------------------------------------------------------------------------
// REGISTER THE COMPONENTS AND WIZARDS
//------------------------------------------------------------------------------
procedure Register;
begin
  // Register our own components
  RegisterComponents(ComponentPage, [
    TOBDCircularGauge,  // Register the Circular Gauge
    TOBDMatrixDisplay,  // Register the Matrix Display
    TOBDLed,            // Register the LED

    TOBDTouchHeader,    // Register the Touch Header
    TOBDTouchSubheader, // Register the Touch Subheader
    TOBDTouchStatusbar, // Register the Touch Statusbar

    TOBDConnectionComponent, // Register the non-visual connection wrapper
    TOBDProtocolComponent,   // Register the non-visual protocol wrapper
    TOBDGaugeComponent,      // Register the non-visual gauge controller
    TOBDHeaderComponent,     // Register the non-visual header controller
    TOBDSubheaderComponent   // Register the non-visual subheader controller
  ]);

  // Register our custom form
  RegisterCustomModule(TOBDForm, TCustomModule);
  // Register our custom datamodule
  RegisterCustomModule(TOBDDataModule, TCustomModule);

  // Register the Project wizard
  RegisterPackageWizard(TOBDProjectModuleCreatorWizard.Create);
  // Register the MainForm wizard
  RegisterPackageWizard(TOBDMainFormModuleCreatorWizard.Create);
  // Register the Form wizard
  RegisterPackageWizard(TOBFormModuleCreatorWizard.Create);
  // Register the DataModule wizard
  RegisterPackageWizard(TOBDataModuleCreatorWizard.Create);
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization
  // Add the about box information
  AddAboutBox;
  // Add the splash screen information
  AddSplashScreen;

end.
