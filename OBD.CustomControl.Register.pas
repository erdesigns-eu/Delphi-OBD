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

uses System.Classes, DesignIntf, DesignEditors, ToolsAPI;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  ComponentPage = 'ERDesigns OBD';

procedure Register;

implementation

uses
  OBD.CircularGauge, OBD.MatrixDisplay, OBD.LED,
  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar, OBD.Form,
  OBD.Project.Wizard, OBD.Mainform.Wizard, OBD.Form.Wizard;

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
    TOBDTouchStatusbar  // Register the Touch Statusbar
  ]);

  // Register our custom form
  RegisterCustomModule(TOBDForm, TCustomModule);

  // Register the Project wizard
  RegisterPackageWizard(TOBDProjectModuleCreatorWizard.Create);
  // Register the MainForm wizard
  RegisterPackageWizard(TOBDMainFormModuleCreatorWizard.Create);
  // Register the Form wizard
  RegisterPackageWizard(TOBFormModuleCreatorWizard.Create);
end;

end.
