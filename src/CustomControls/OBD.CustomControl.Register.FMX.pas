//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Register.FMX.pas
// CONTENTS       : IDE registration for the FMX components
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lives in `Packages/DesignTime.FMX.dpk`. Drops every
//                  v3.1 FMX component onto the same "ERDesigns OBD"
//                  palette page the VCL components use, so a developer
//                  switching between VCL and FMX projects sees the same
//                  set of components.
//------------------------------------------------------------------------------
unit OBD.CustomControl.Register.FMX;

interface

uses System.Classes;

const
  ComponentPage = 'ERDesigns OBD';

procedure Register;

implementation

uses
  OBD.LinearGauge.FMX, OBD.Tachometer.FMX, OBD.TrendGraph.FMX,
  OBD.DtcList.FMX, OBD.Terminal.FMX, OBD.Knob.FMX,
  OBD.SegmentedSwitch.FMX, OBD.LED.FMX;

//------------------------------------------------------------------------------
// REGISTER
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents(ComponentPage, [
    TOBDLinearGaugeFMX,
    TOBDTachometerFMX,
    TOBDTrendGraphFMX,
    TOBDDtcListFMX,
    TOBDTerminalFMX,
    TOBDKnobFMX,
    TOBDSegmentedSwitchFMX,
    TOBDLedFMX
  ]);
end;

end.
