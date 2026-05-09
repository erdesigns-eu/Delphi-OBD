//------------------------------------------------------------------------------
// UNIT           : DiagTool.MainForm
// CONTENTS       : Programmatic VCL diagnostic-tool main form.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : All controls are created in code (no .dfm) so
//                  the example is editable in any text editor and
//                  doesn't need a Delphi-IDE-specific layout file.
//                  Layout: TStatusBar (bottom) + connection panel
//                  (top) + OEM/session info (top-2) + TPageControl
//                  with four tab sheets (live data, DTCs, DIDs,
//                  routines). The tool wires every API the v3.x
//                  framework ships:
//                    - OBD.Connection.Serial → adapter
//                    - OBD.Connection.Async → request/response
//                    - OBD.OEM.Registry → VIN-based routing
//                    - TOBDDiagSession → session lifecycle
//                    - OEM extension's DescribeDTC + DecodeDID
//------------------------------------------------------------------------------
unit DiagTool.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  OBD.Connection, OBD.Connection.Types, OBD.Connection.Serial,
  OBD.Connection.Async,
  OBD.OEM, OBD.OEM.Session, OBD.OEM.DiagSession, OBD.OEM.DTC;

type
  TDiagToolMainForm = class(TForm)
  strict private
    // Connection panel
    FPnlConnection: TPanel;
    FCmbPort: TComboBox;
    FCmbBaud: TComboBox;
    FBtnConnect: TButton;
    FBtnDisconnect: TButton;
    FLblStatus: TLabel;
    // OEM / session info
    FPnlInfo: TPanel;
    FLblVin: TLabel;
    FLblOEM: TLabel;
    FLblSession: TLabel;
    FBtnReadVin: TButton;
    FBtnEnterExtended: TButton;
    FBtnExitSession: TButton;
    // Tab control
    FPageControl: TPageControl;
    FTabLiveData: TTabSheet;
    FTabDTCs: TTabSheet;
    FTabDIDs: TTabSheet;
    FTabRoutines: TTabSheet;
    // Live data tab
    FLblBattery, FLblRPM, FLblSpeed, FLblCoolant: TLabel;
    FBtnRefreshLive: TButton;
    // DTCs tab
    FLstDTCs: TListBox;
    FMemoDtcDetail: TMemo;
    FBtnReadDTCs: TButton;
    FBtnClearDTCs: TButton;
    // DIDs tab
    FCmbDID: TComboBox;
    FBtnReadDID: TButton;
    FMemoDidResult: TMemo;
    // Routines tab
    FCmbRoutine: TComboBox;
    FBtnRunRoutine: TButton;
    FMemoRoutineResult: TMemo;
    // Status bar
    FStatusBar: TStatusBar;
    // Wiring
    FConnection: IOBDConnection;
    FAsync: TOBDConnectionAsync;
    FOEM: IOBDOEMExtension;
    FSession: TOBDDiagSession;
    procedure BuildUI;
    procedure BuildConnectionPanel;
    procedure BuildInfoPanel;
    procedure BuildLiveDataTab;
    procedure BuildDTCsTab;
    procedure BuildDIDsTab;
    procedure BuildRoutinesTab;
    procedure SetConnected(Value: Boolean);
    procedure RefreshOEMComboboxes;
    procedure SetStatus(const S: string);
    function ReadOBDPid(const Pid: Word; out Bytes: TBytes): Boolean;
    // Event handlers
    procedure OnConnectClick(Sender: TObject);
    procedure OnDisconnectClick(Sender: TObject);
    procedure OnReadVinClick(Sender: TObject);
    procedure OnEnterExtendedClick(Sender: TObject);
    procedure OnExitSessionClick(Sender: TObject);
    procedure OnRefreshLiveClick(Sender: TObject);
    procedure OnReadDTCsClick(Sender: TObject);
    procedure OnClearDTCsClick(Sender: TObject);
    procedure OnDTCSelected(Sender: TObject);
    procedure OnReadDIDClick(Sender: TObject);
    procedure OnRunRoutineClick(Sender: TObject);
    procedure OnFormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DiagToolMainForm: TDiagToolMainForm;

implementation

uses
  OBD.OEM.Coding, OBD.Async;

const
  // Standard OBD-II PID constants used by the live-data tab.
  PID_RPM             = $0C;
  PID_VEHICLE_SPEED   = $0D;
  PID_COOLANT_TEMP    = $05;
  PID_BATTERY_VOLTAGE = $42;  // SAE J1979: control module voltage

constructor TDiagToolMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Delphi-OBD Diagnostic Tool — v3.20';
  ClientWidth := 880;
  ClientHeight := 620;
  Position := poScreenCenter;
  Font.Name := 'Segoe UI';
  Font.Size := 9;
  OnClose := OnFormClose;
  BuildUI;
end;

destructor TDiagToolMainForm.Destroy;
begin
  // Best-effort teardown; SetConnected handles the heavy lifting.
  if Assigned(FSession) then FSession.Free;
  if Assigned(FAsync) then FAsync.Free;
  // FConnection is an interface — released automatically.
  inherited;
end;

procedure TDiagToolMainForm.OnFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SetConnected(False);
end;

procedure TDiagToolMainForm.BuildUI;
begin
  FStatusBar := TStatusBar.Create(Self);
  FStatusBar.Parent := Self;
  FStatusBar.SimplePanel := True;
  FStatusBar.SimpleText := 'Ready. Pick a COM port and click Connect.';

  BuildConnectionPanel;
  BuildInfoPanel;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;

  FTabLiveData := TTabSheet.Create(FPageControl);
  FTabLiveData.PageControl := FPageControl;
  FTabLiveData.Caption := 'Live Data';

  FTabDTCs := TTabSheet.Create(FPageControl);
  FTabDTCs.PageControl := FPageControl;
  FTabDTCs.Caption := 'DTCs';

  FTabDIDs := TTabSheet.Create(FPageControl);
  FTabDIDs.PageControl := FPageControl;
  FTabDIDs.Caption := 'DIDs';

  FTabRoutines := TTabSheet.Create(FPageControl);
  FTabRoutines.PageControl := FPageControl;
  FTabRoutines.Caption := 'Routines';

  BuildLiveDataTab;
  BuildDTCsTab;
  BuildDIDsTab;
  BuildRoutinesTab;
end;

procedure TDiagToolMainForm.BuildConnectionPanel;
var
  L: TLabel;
begin
  FPnlConnection := TPanel.Create(Self);
  FPnlConnection.Parent := Self;
  FPnlConnection.Align := alTop;
  FPnlConnection.Height := 50;
  FPnlConnection.BevelOuter := bvNone;
  FPnlConnection.Padding.SetBounds(8, 6, 8, 6);

  L := TLabel.Create(Self);
  L.Parent := FPnlConnection;
  L.SetBounds(8, 14, 30, 17);
  L.Caption := 'Port:';

  FCmbPort := TComboBox.Create(Self);
  FCmbPort.Parent := FPnlConnection;
  FCmbPort.SetBounds(40, 11, 100, 23);
  FCmbPort.Style := csDropDown;
  FCmbPort.Items.CommaText := 'COM1,COM3,COM4,COM5,COM7';
  FCmbPort.ItemIndex := 1;

  L := TLabel.Create(Self);
  L.Parent := FPnlConnection;
  L.SetBounds(155, 14, 35, 17);
  L.Caption := 'Baud:';

  FCmbBaud := TComboBox.Create(Self);
  FCmbBaud.Parent := FPnlConnection;
  FCmbBaud.SetBounds(192, 11, 90, 23);
  FCmbBaud.Style := csDropDownList;
  FCmbBaud.Items.CommaText := '9600,19200,38400,57600,115200';
  FCmbBaud.ItemIndex := 2;

  FBtnConnect := TButton.Create(Self);
  FBtnConnect.Parent := FPnlConnection;
  FBtnConnect.SetBounds(298, 10, 90, 27);
  FBtnConnect.Caption := 'Connect';
  FBtnConnect.OnClick := OnConnectClick;

  FBtnDisconnect := TButton.Create(Self);
  FBtnDisconnect.Parent := FPnlConnection;
  FBtnDisconnect.SetBounds(396, 10, 90, 27);
  FBtnDisconnect.Caption := 'Disconnect';
  FBtnDisconnect.Enabled := False;
  FBtnDisconnect.OnClick := OnDisconnectClick;

  FLblStatus := TLabel.Create(Self);
  FLblStatus.Parent := FPnlConnection;
  FLblStatus.SetBounds(498, 14, 350, 17);
  FLblStatus.Caption := 'Disconnected';
end;

procedure TDiagToolMainForm.BuildInfoPanel;
var
  Y: Integer;
begin
  FPnlInfo := TPanel.Create(Self);
  FPnlInfo.Parent := Self;
  FPnlInfo.Align := alTop;
  FPnlInfo.Height := 100;
  FPnlInfo.BevelOuter := bvLowered;
  FPnlInfo.Padding.SetBounds(8, 6, 8, 6);

  Y := 8;
  FLblVin := TLabel.Create(Self);
  FLblVin.Parent := FPnlInfo;
  FLblVin.SetBounds(10, Y, 700, 17);
  FLblVin.Caption := 'VIN: —';
  FLblVin.Font.Style := [fsBold];

  Inc(Y, 22);
  FLblOEM := TLabel.Create(Self);
  FLblOEM.Parent := FPnlInfo;
  FLblOEM.SetBounds(10, Y, 700, 17);
  FLblOEM.Caption := 'OEM: — (waiting for VIN)';

  Inc(Y, 22);
  FLblSession := TLabel.Create(Self);
  FLblSession.Parent := FPnlInfo;
  FLblSession.SetBounds(10, Y, 400, 17);
  FLblSession.Caption := 'Session: idle';

  FBtnReadVin := TButton.Create(Self);
  FBtnReadVin.Parent := FPnlInfo;
  FBtnReadVin.SetBounds(630, 8, 110, 27);
  FBtnReadVin.Caption := 'Read VIN';
  FBtnReadVin.Enabled := False;
  FBtnReadVin.OnClick := OnReadVinClick;

  FBtnEnterExtended := TButton.Create(Self);
  FBtnEnterExtended.Parent := FPnlInfo;
  FBtnEnterExtended.SetBounds(630, 38, 110, 27);
  FBtnEnterExtended.Caption := 'Extended →';
  FBtnEnterExtended.Enabled := False;
  FBtnEnterExtended.OnClick := OnEnterExtendedClick;

  FBtnExitSession := TButton.Create(Self);
  FBtnExitSession.Parent := FPnlInfo;
  FBtnExitSession.SetBounds(746, 38, 110, 27);
  FBtnExitSession.Caption := 'End Session';
  FBtnExitSession.Enabled := False;
  FBtnExitSession.OnClick := OnExitSessionClick;
end;

procedure TDiagToolMainForm.BuildLiveDataTab;
var
  Y: Integer;
begin
  Y := 16;
  FLblBattery := TLabel.Create(Self);
  FLblBattery.Parent := FTabLiveData;
  FLblBattery.SetBounds(20, Y, 500, 22);
  FLblBattery.Caption := 'Battery voltage: —';
  FLblBattery.Font.Size := 11;

  Inc(Y, 28);
  FLblRPM := TLabel.Create(Self);
  FLblRPM.Parent := FTabLiveData;
  FLblRPM.SetBounds(20, Y, 500, 22);
  FLblRPM.Caption := 'Engine RPM: —';
  FLblRPM.Font.Size := 11;

  Inc(Y, 28);
  FLblSpeed := TLabel.Create(Self);
  FLblSpeed.Parent := FTabLiveData;
  FLblSpeed.SetBounds(20, Y, 500, 22);
  FLblSpeed.Caption := 'Vehicle speed: —';
  FLblSpeed.Font.Size := 11;

  Inc(Y, 28);
  FLblCoolant := TLabel.Create(Self);
  FLblCoolant.Parent := FTabLiveData;
  FLblCoolant.SetBounds(20, Y, 500, 22);
  FLblCoolant.Caption := 'Coolant temperature: —';
  FLblCoolant.Font.Size := 11;

  FBtnRefreshLive := TButton.Create(Self);
  FBtnRefreshLive.Parent := FTabLiveData;
  FBtnRefreshLive.SetBounds(20, Y + 50, 130, 30);
  FBtnRefreshLive.Caption := 'Refresh';
  FBtnRefreshLive.OnClick := OnRefreshLiveClick;
end;

procedure TDiagToolMainForm.BuildDTCsTab;
begin
  FBtnReadDTCs := TButton.Create(Self);
  FBtnReadDTCs.Parent := FTabDTCs;
  FBtnReadDTCs.SetBounds(10, 10, 130, 27);
  FBtnReadDTCs.Caption := 'Read DTCs';
  FBtnReadDTCs.OnClick := OnReadDTCsClick;

  FBtnClearDTCs := TButton.Create(Self);
  FBtnClearDTCs.Parent := FTabDTCs;
  FBtnClearDTCs.SetBounds(150, 10, 130, 27);
  FBtnClearDTCs.Caption := 'Clear DTCs (04)';
  FBtnClearDTCs.OnClick := OnClearDTCsClick;

  FLstDTCs := TListBox.Create(Self);
  FLstDTCs.Parent := FTabDTCs;
  FLstDTCs.SetBounds(10, 47, 250, 320);
  FLstDTCs.OnClick := OnDTCSelected;

  FMemoDtcDetail := TMemo.Create(Self);
  FMemoDtcDetail.Parent := FTabDTCs;
  FMemoDtcDetail.SetBounds(270, 47, 580, 320);
  FMemoDtcDetail.ReadOnly := True;
  FMemoDtcDetail.ScrollBars := ssVertical;
  FMemoDtcDetail.Lines.Add('Select a DTC to view details from the OEM catalog.');
end;

procedure TDiagToolMainForm.BuildDIDsTab;
var
  L: TLabel;
begin
  L := TLabel.Create(Self);
  L.Parent := FTabDIDs;
  L.SetBounds(10, 14, 50, 17);
  L.Caption := 'DID:';

  FCmbDID := TComboBox.Create(Self);
  FCmbDID.Parent := FTabDIDs;
  FCmbDID.SetBounds(60, 11, 600, 23);
  FCmbDID.Style := csDropDown;

  FBtnReadDID := TButton.Create(Self);
  FBtnReadDID.Parent := FTabDIDs;
  FBtnReadDID.SetBounds(670, 10, 130, 27);
  FBtnReadDID.Caption := 'Read DID';
  FBtnReadDID.OnClick := OnReadDIDClick;

  FMemoDidResult := TMemo.Create(Self);
  FMemoDidResult.Parent := FTabDIDs;
  FMemoDidResult.SetBounds(10, 47, 840, 320);
  FMemoDidResult.ReadOnly := True;
  FMemoDidResult.ScrollBars := ssVertical;
  FMemoDidResult.Font.Name := 'Consolas';
end;

procedure TDiagToolMainForm.BuildRoutinesTab;
var
  L: TLabel;
begin
  L := TLabel.Create(Self);
  L.Parent := FTabRoutines;
  L.SetBounds(10, 14, 60, 17);
  L.Caption := 'Routine:';

  FCmbRoutine := TComboBox.Create(Self);
  FCmbRoutine.Parent := FTabRoutines;
  FCmbRoutine.SetBounds(70, 11, 590, 23);
  FCmbRoutine.Style := csDropDown;

  FBtnRunRoutine := TButton.Create(Self);
  FBtnRunRoutine.Parent := FTabRoutines;
  FBtnRunRoutine.SetBounds(670, 10, 130, 27);
  FBtnRunRoutine.Caption := 'Start (31 01)';
  FBtnRunRoutine.OnClick := OnRunRoutineClick;

  FMemoRoutineResult := TMemo.Create(Self);
  FMemoRoutineResult.Parent := FTabRoutines;
  FMemoRoutineResult.SetBounds(10, 47, 840, 320);
  FMemoRoutineResult.ReadOnly := True;
  FMemoRoutineResult.ScrollBars := ssVertical;
  FMemoRoutineResult.Font.Name := 'Consolas';
end;

//==============================================================================
//  Connection lifecycle
//==============================================================================
procedure TDiagToolMainForm.SetConnected(Value: Boolean);
begin
  if Value then
  begin
    FBtnConnect.Enabled := False;
    FBtnDisconnect.Enabled := True;
    FBtnReadVin.Enabled := True;
    FBtnEnterExtended.Enabled := True;
    FCmbPort.Enabled := False;
    FCmbBaud.Enabled := False;
  end
  else
  begin
    if Assigned(FSession) then FreeAndNil(FSession);
    if Assigned(FAsync) then FreeAndNil(FAsync);
    if Assigned(FConnection) then
    begin
      try FConnection.Disconnect; except end;
      FConnection := nil;
    end;
    FOEM := nil;
    FBtnConnect.Enabled := True;
    FBtnDisconnect.Enabled := False;
    FBtnReadVin.Enabled := False;
    FBtnEnterExtended.Enabled := False;
    FBtnExitSession.Enabled := False;
    FCmbPort.Enabled := True;
    FCmbBaud.Enabled := True;
    FLblStatus.Caption := 'Disconnected';
    FLblVin.Caption := 'VIN: —';
    FLblOEM.Caption := 'OEM: — (waiting for VIN)';
    FLblSession.Caption := 'Session: idle';
  end;
end;

procedure TDiagToolMainForm.SetStatus(const S: string);
begin
  FStatusBar.SimpleText := S;
end;

procedure TDiagToolMainForm.OnConnectClick(Sender: TObject);
var
  Params: TOBDConnectionParams;
begin
  try
    FConnection := TOBDConnectionSerial.Create;
    Params := Default(TOBDConnectionParams);
    Params.ConnectionType := ctSerial;
    Params.SerialPort := FCmbPort.Text;
    Params.SerialBaudRate := StrToIntDef(FCmbBaud.Text, 38400);
    if not FConnection.Connect(Params) then
    begin
      ShowMessage('Failed to open ' + FCmbPort.Text);
      FConnection := nil;
      Exit;
    end;
    FAsync := TOBDConnectionAsync.Create(FConnection);
    SetConnected(True);
    FLblStatus.Caption := Format('Connected on %s @ %d baud',
      [FCmbPort.Text, Params.SerialBaudRate]);
    SetStatus('Connection open. Read VIN to auto-detect the OEM.');
  except
    on E: Exception do
    begin
      ShowMessage('Connect failed: ' + E.Message);
      SetConnected(False);
    end;
  end;
end;

procedure TDiagToolMainForm.OnDisconnectClick(Sender: TObject);
begin
  SetConnected(False);
  SetStatus('Disconnected.');
end;

//==============================================================================
//  VIN read + OEM auto-detect
//==============================================================================
function TDiagToolMainForm.ReadOBDPid(const Pid: Word; out Bytes: TBytes): Boolean;
var
  Future: IOBDFuture<string>;
  Reply: string;
begin
  Bytes := nil;
  if FAsync = nil then Exit(False);
  try
    Future := FAsync.OBDAsync(Format('01 %.2X', [Pid]), 2000);
    Reply := Future.Await(2500);
    Bytes := HexStringToBytes(Reply);
    Result := Length(Bytes) >= 2;
  except
    on E: Exception do
    begin
      SetStatus('OBD-II PID read failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TDiagToolMainForm.OnReadVinClick(Sender: TObject);
var
  Future: IOBDFuture<string>;
  Reply: string;
  Bytes: TBytes;
  Vin: string;
  I: Integer;
begin
  if FAsync = nil then Exit;
  SetStatus('Reading VIN (Service 09 PID 02)…');
  try
    Future := FAsync.OBDAsync('09 02', 3000);
    Reply := Future.Await(3500);
    Bytes := HexStringToBytes(Reply);
    Vin := '';
    // Service 09 PID 02 reply layout: 49 02 01 + 17 ASCII bytes,
    // delivered across multiple frames the adapter reassembles.
    for I := 3 to High(Bytes) do
      if (Bytes[I] >= 32) and (Bytes[I] < 127) then
        Vin := Vin + Char(Bytes[I]);
    Vin := Vin.Trim;
    if Length(Vin) <> 17 then
    begin
      SetStatus('VIN response malformed; raw: ' + Reply);
      FLblVin.Caption := 'VIN: (failed)';
      Exit;
    end;

    FLblVin.Caption := 'VIN: ' + Vin;
    FOEM := TOBDOEMRegistry.FindByVIN(Vin);
    if FOEM <> nil then
    begin
      FLblOEM.Caption := Format('OEM: %s (key %s, %s)',
        [FOEM.DisplayName, FOEM.ManufacturerKey,
         FOEM.SessionNegotiator.DisplayName]);
      RefreshOEMComboboxes;
      SetStatus('OEM auto-detected from VIN.');
    end
    else
    begin
      FLblOEM.Caption := 'OEM: (no extension claims this VIN)';
      SetStatus('No OEM extension matched the VIN prefix.');
    end;
  except
    on E: Exception do
      SetStatus('VIN read failed: ' + E.Message);
  end;
end;

procedure TDiagToolMainForm.RefreshOEMComboboxes;
var
  D: TOBDOEMDataIdentifier;
  R: TOBDOEMRoutine;
begin
  FCmbDID.Items.BeginUpdate;
  try
    FCmbDID.Items.Clear;
    if FOEM <> nil then
      for D in FOEM.DataIdentifiers do
        FCmbDID.Items.AddObject(
          Format('0x%.4X — %s — %s', [D.DID, D.Name, D.Description]),
          TObject(NativeUInt(D.DID)));
    if FCmbDID.Items.Count > 0 then FCmbDID.ItemIndex := 0;
  finally
    FCmbDID.Items.EndUpdate;
  end;

  FCmbRoutine.Items.BeginUpdate;
  try
    FCmbRoutine.Items.Clear;
    if FOEM <> nil then
      for R in FOEM.Routines do
        FCmbRoutine.Items.AddObject(
          Format('0x%.4X — %s — %s',
            [R.Identifier, R.Name, R.Description]),
          TObject(NativeUInt(R.Identifier)));
    if FCmbRoutine.Items.Count > 0 then FCmbRoutine.ItemIndex := 0;
  finally
    FCmbRoutine.Items.EndUpdate;
  end;
end;

//==============================================================================
//  Session control
//==============================================================================
procedure TDiagToolMainForm.OnEnterExtendedClick(Sender: TObject);
begin
  if (FOEM = nil) or (FAsync = nil) then
  begin
    ShowMessage('Read the VIN first so the OEM extension is bound.');
    Exit;
  end;
  if FSession = nil then
    FSession := TOBDDiagSession.Create(FAsync, FOEM);
  if FSession.BeginSession(sstExtendedDiagnostic, $7E0) then
  begin
    FLblSession.Caption := 'Session: extended (0x7E0) — heartbeat running';
    FBtnExitSession.Enabled := True;
    SetStatus('Extended session entered.');
  end
  else
  begin
    FLblSession.Caption := 'Session: BeginSession FAILED';
    SetStatus('BeginSession failed: ' + FSession.LastError);
  end;
end;

procedure TDiagToolMainForm.OnExitSessionClick(Sender: TObject);
begin
  if FSession = nil then Exit;
  FSession.EndSession;
  FLblSession.Caption := 'Session: idle';
  FBtnExitSession.Enabled := False;
  SetStatus('Session closed.');
end;

//==============================================================================
//  Live data tab
//==============================================================================
procedure TDiagToolMainForm.OnRefreshLiveClick(Sender: TObject);
var
  Bytes: TBytes;
  Voltage: Double;
  RPM: Word;
  Speed: Byte;
  Coolant: Integer;
begin
  if FAsync = nil then
  begin
    SetStatus('Connect first.');
    Exit;
  end;

  // PID 0x42: A*256 + B then / 1000 = volts (per SAE J1979).
  if ReadOBDPid(PID_BATTERY_VOLTAGE, Bytes) and (Length(Bytes) >= 4) then
  begin
    Voltage := ((Bytes[2] shl 8) or Bytes[3]) / 1000.0;
    FLblBattery.Caption := Format('Battery voltage: %.2f V', [Voltage]);
  end;

  // PID 0x0C: ((A*256)+B) / 4 = rpm.
  if ReadOBDPid(PID_RPM, Bytes) and (Length(Bytes) >= 4) then
  begin
    RPM := (Bytes[2] shl 8) or Bytes[3];
    FLblRPM.Caption := Format('Engine RPM: %d', [RPM div 4]);
  end;

  // PID 0x0D: A = km/h.
  if ReadOBDPid(PID_VEHICLE_SPEED, Bytes) and (Length(Bytes) >= 3) then
  begin
    Speed := Bytes[2];
    FLblSpeed.Caption := Format('Vehicle speed: %d km/h', [Speed]);
  end;

  // PID 0x05: A - 40 = °C.
  if ReadOBDPid(PID_COOLANT_TEMP, Bytes) and (Length(Bytes) >= 3) then
  begin
    Coolant := Integer(Bytes[2]) - 40;
    FLblCoolant.Caption := Format('Coolant temperature: %d °C', [Coolant]);
  end;

  SetStatus('Live data refreshed.');
end;

//==============================================================================
//  DTCs tab
//==============================================================================
procedure TDiagToolMainForm.OnReadDTCsClick(Sender: TObject);
var
  Future: IOBDFuture<string>;
  Reply: string;
  Bytes: TBytes;
  I: Integer;
  Hi, Lo: Byte;
  Code: string;
const
  SYSTEMS: array[0..3] of Char = ('P', 'C', 'B', 'U');
begin
  if FAsync = nil then Exit;
  FLstDTCs.Items.Clear;
  FMemoDtcDetail.Lines.Clear;
  SetStatus('Reading DTCs (Service 03)…');
  try
    Future := FAsync.OBDAsync('03', 3000);
    Reply := Future.Await(3500);
    Bytes := HexStringToBytes(Reply);
    // Service 03 reply layout: 43 NN AA BB CC DD …  where NN is the
    // count and each pair (AA,BB) decodes to one P/C/B/U code.
    if (Length(Bytes) < 2) or (Bytes[0] <> $43) then
    begin
      SetStatus('Service 03 reply malformed: ' + Reply);
      Exit;
    end;
    I := 2;
    while I + 1 < Length(Bytes) do
    begin
      Hi := Bytes[I]; Lo := Bytes[I + 1];
      if (Hi = 0) and (Lo = 0) then Break;
      Code := Format('%s%d%d%.2X',
        [SYSTEMS[(Hi shr 6) and 3],
         ((Hi shr 5) and 1) * 2 + ((Hi shr 4) and 1),
         Hi and $0F, Lo]);
      FLstDTCs.Items.Add(Code);
      Inc(I, 2);
    end;
    SetStatus(Format('%d DTC(s) read.', [FLstDTCs.Items.Count]));
  except
    on E: Exception do
      SetStatus('Service 03 failed: ' + E.Message);
  end;
end;

procedure TDiagToolMainForm.OnDTCSelected(Sender: TObject);
var
  Code: string;
  Entry: TOBDDtcCatalogEntry;
  Cause: string;
begin
  FMemoDtcDetail.Lines.Clear;
  if FLstDTCs.ItemIndex < 0 then Exit;
  Code := FLstDTCs.Items[FLstDTCs.ItemIndex];
  FMemoDtcDetail.Lines.Add('Code: ' + Code);
  FMemoDtcDetail.Lines.Add('');
  if FOEM = nil then
  begin
    FMemoDtcDetail.Lines.Add('(no OEM extension bound — read VIN first)');
    Exit;
  end;
  if FOEM.DescribeDTC(Code, Entry) then
  begin
    FMemoDtcDetail.Lines.Add('Description: ' + Entry.Description);
    FMemoDtcDetail.Lines.Add('Severity: ' +
      string(case Entry.Severity of
               dtcSeverityCritical: 'critical';
               dtcSeverityWarning:  'warning';
               dtcSeverityInfo:     'info';
             else  'unknown'; end));
    if Length(Entry.PossibleCauses) > 0 then
    begin
      FMemoDtcDetail.Lines.Add('');
      FMemoDtcDetail.Lines.Add('Possible causes:');
      for Cause in Entry.PossibleCauses do
        FMemoDtcDetail.Lines.Add('  - ' + Cause);
    end;
    if Entry.RepairHints <> '' then
    begin
      FMemoDtcDetail.Lines.Add('');
      FMemoDtcDetail.Lines.Add('Repair hints: ' + Entry.RepairHints);
    end;
    FMemoDtcDetail.Lines.Add('');
    FMemoDtcDetail.Lines.Add('Source: ' + Entry.Source);
    FMemoDtcDetail.Lines.Add('Verified: ' + BoolToStr(Entry.Verified, True));
  end
  else
    FMemoDtcDetail.Lines.Add('Not in the OEM catalog.');
end;

procedure TDiagToolMainForm.OnClearDTCsClick(Sender: TObject);
var
  Future: IOBDFuture<string>;
begin
  if FAsync = nil then Exit;
  if MessageDlg('Clear stored DTCs (Service 04)?',
    mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  try
    Future := FAsync.OBDAsync('04', 3000);
    Future.Await(3500);
    FLstDTCs.Items.Clear;
    FMemoDtcDetail.Lines.Clear;
    SetStatus('DTCs cleared.');
  except
    on E: Exception do
      SetStatus('Clear DTCs failed: ' + E.Message);
  end;
end;

//==============================================================================
//  DIDs tab
//==============================================================================
procedure TDiagToolMainForm.OnReadDIDClick(Sender: TObject);
var
  DID: Word;
  Idx: Integer;
  Payload: TBytes;
  Decoded: string;
begin
  Idx := FCmbDID.ItemIndex;
  if (Idx < 0) or (FOEM = nil) or (FSession = nil) then
  begin
    FMemoDidResult.Lines.Add('(connect, read VIN, and enter extended session first)');
    Exit;
  end;
  DID := Word(NativeUInt(FCmbDID.Items.Objects[Idx]));
  if FSession.ReadDID(DID, Payload) then
  begin
    Decoded := FOEM.DecodeDID(DID, Payload);
    FMemoDidResult.Lines.Add(Format('0x%.4X → %s', [DID, Decoded]));
    FMemoDidResult.Lines.Add('  bytes: ' + BytesToHexString(Payload, ' '));
    SetStatus('DID read OK.');
  end
  else
  begin
    FMemoDidResult.Lines.Add(Format('0x%.4X — failed: %s',
      [DID, FSession.LastError]));
    SetStatus('DID read failed: ' + FSession.LastError);
  end;
end;

//==============================================================================
//  Routines tab
//==============================================================================
procedure TDiagToolMainForm.OnRunRoutineClick(Sender: TObject);
var
  RID: Word;
  Idx: Integer;
  Status: TBytes;
begin
  Idx := FCmbRoutine.ItemIndex;
  if (Idx < 0) or (FSession = nil) then
  begin
    FMemoRoutineResult.Lines.Add(
      '(connect, read VIN, and enter extended session first)');
    Exit;
  end;
  if MessageDlg(Format('Start routine 0x%.4X?',
       [Word(NativeUInt(FCmbRoutine.Items.Objects[Idx]))]),
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  RID := Word(NativeUInt(FCmbRoutine.Items.Objects[Idx]));
  if FSession.StartRoutine(RID, nil, Status) then
  begin
    FMemoRoutineResult.Lines.Add(Format('0x%.4X → started; status %s',
      [RID, BytesToHexString(Status, ' ')]));
    SetStatus('Routine started.');
  end
  else
  begin
    FMemoRoutineResult.Lines.Add(Format('0x%.4X — failed: %s',
      [RID, FSession.LastError]));
    SetStatus('StartRoutine failed: ' + FSession.LastError);
  end;
end;

end.
