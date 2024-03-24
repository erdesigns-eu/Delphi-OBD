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

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  ComponentPage = 'ERDesigns OBD';

procedure Register;

implementation

uses System.Classes, OBD.CircularGauge, OBD.MatrixDisplay, OBD.LED;

//------------------------------------------------------------------------------
// REGISTER THE COMPONENT
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents(ComponentPage, [
    TOBDCircularGauge, // Register the Circular Gauge
    TOBDMatrixDisplay, // Register the Matrix Display
    TOBDLed            // Register the LED
  ]);
end;

end.
