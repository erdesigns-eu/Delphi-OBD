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

uses System.Classes;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  ComponentPage = 'ERDesigns OBD';

procedure Register;

implementation

uses
  OBD.CircularGauge, OBD.MatrixDisplay, OBD.LED,
  OBD.Touch.Header, OBD.Touch.Subheader;

//------------------------------------------------------------------------------
// REGISTER THE COMPONENT
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents(ComponentPage, [
    TOBDCircularGauge,  // Register the Circular Gauge
    TOBDMatrixDisplay,  // Register the Matrix Display
    TOBDLed,            // Register the LED

    TOBDTouchHeader,    // Register the Touch Header
    TOBDTouchSubheader  // Register the Touch Subheader
  ]);
end;

end.
