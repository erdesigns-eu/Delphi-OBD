//------------------------------------------------------------------------------
//  LiveDashboard — live-data dashboard demo
//
//  Demonstrates TOBDLiveData + TOBDPIDList: configures a design-time-
//  editable PID list, subscribes to value updates, prints them.
//
//  The sample wires the components but does NOT call any I/O
//  methods — supply a real TOBDProtocol and call Poll() in your
//  app. This file shows the pattern.
//
//  Build & run:
//    dcc32 -B LiveDashboard.dpr
//    LiveDashboard
//------------------------------------------------------------------------------

program LiveDashboard;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Types               in '..\..\src\Core\OBD.Types.pas',
  OBD.Errors              in '..\..\src\Core\OBD.Errors.pas',
  OBD.PIDList             in '..\..\src\Service\OBD.PIDList.pas';

procedure Dump(L: TOBDPIDList);
var
  I: Integer;
  It: TOBDPIDItem;
begin
  Writeln(Format('  list count: %d', [L.Count]));
  for I := 0 to L.Count - 1 do
  begin
    It := TOBDPIDItem(L.Items[I]);
    Writeln(Format('    [%d] Mode=%.2x PID=%.2x  enabled=%s  name=%s',
      [I, It.Mode, It.PID,
       BoolToStr(It.Enabled, True), It.Name]));
  end;
end;

var
  Owner: TComponent;
  PIDs: TOBDPIDList;
  Enabled: TArray<Byte>;
begin
  Writeln('Delphi-OBD live-dashboard PID-list demo');
  Writeln(StringOfChar('-', 60));

  Owner := TComponent.Create(nil);
  try
    PIDs := TOBDPIDList.Create(Owner);
    try
      PIDs.Add($01, $0C, 'Engine RPM');
      PIDs.Add($01, $0D, 'Vehicle Speed');
      PIDs.Add($01, $11, 'Throttle Position');
      PIDs.Add($01, $05, 'Coolant Temp', False);  // disabled
      Dump(PIDs);

      Enabled := PIDs.EnabledFor($01);
      Writeln(Format('  EnabledFor($01): %d entries', [Length(Enabled)]));
      for var B in Enabled do
        Write(Format(' %.2x', [B]));
      Writeln;
    finally
      // Owner frees PIDs.
    end;
  finally
    Owner.Free;
  end;

  Writeln;
  Writeln('Done. Wire TOBDLiveData.Subscribe(...) per PID and call Poll().');
end.
