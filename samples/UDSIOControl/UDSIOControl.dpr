//------------------------------------------------------------------------------
//  UDSIOControl
//
//  Demonstrates TOBDUDSIOControl — Service 0x2F
//  InputOutputControlByIdentifier. Drives an ECU I/O DID into a
//  host-driven state (returnControl, freezeCurrentState,
//  shortTermAdjustment). Destructive — ships with
//  AutoExecute = False and an OnBeforeSend cancellation hook.
//
//  Build & run:
//    dcc32 -B UDSIOControl.dpr
//    UDSIOControl
//------------------------------------------------------------------------------

program UDSIOControl;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types                     in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors                    in '..\..\src\Core\OBD.Errors.pas',
  OBD.Diagnostics.UDS.IOControl in '..\..\src\Diagnostics\OBD.Diagnostics.UDS.IOControl.pas';

var
  C: TOBDUDSIOControl;
begin
  Writeln('Delphi-OBD UDS InputOutputControl demo');
  Writeln(StringOfChar('-', 60));

  C := TOBDUDSIOControl.Create(nil);
  try
    Writeln('  AutoExecute defaults to False — Send raises EOBDConfig:');
    try
      C.Send($1234, UDS_IOCTL_RETURN_CONTROL_TO_ECU);
    except
      on E: EOBDConfig do
        Writeln('    EOBDConfig — OK');
    end;
  finally
    C.Free;
  end;

  Writeln;
  Writeln('  Control-parameter constants:');
  Writeln(Format('    UDS_IOCTL_RETURN_CONTROL_TO_ECU = 0x%.2x',
    [UDS_IOCTL_RETURN_CONTROL_TO_ECU]));
  Writeln(Format('    UDS_IOCTL_RESET_TO_DEFAULT      = 0x%.2x',
    [UDS_IOCTL_RESET_TO_DEFAULT]));
  Writeln(Format('    UDS_IOCTL_FREEZE_CURRENT_STATE  = 0x%.2x',
    [UDS_IOCTL_FREEZE_CURRENT_STATE]));
  Writeln(Format('    UDS_IOCTL_SHORT_TERM_ADJUSTMENT = 0x%.2x',
    [UDS_IOCTL_SHORT_TERM_ADJUSTMENT]));
  Writeln;
  Writeln('Done.');
end.
