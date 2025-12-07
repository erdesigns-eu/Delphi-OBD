unit UDSExampleMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.UDS;

type
  TUDSForm = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortLabel: TLabel;
    PortComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    SessionGroupBox: TGroupBox;
    SessionCombo: TComboBox;
    StartSessionButton: TButton;
    SecurityGroupBox: TGroupBox;
    SecurityLevelEdit: TEdit;
    RequestSeedButton: TButton;
    SendKeyButton: TButton;
    DTCGroupBox: TGroupBox;
    ReadDTCButton: TButton;
    ClearDTCButton: TButton;
    DTCSubfunctionCombo: TComboBox;
    DataGroupBox: TGroupBox;
    DataIDEdit: TEdit;
    ReadDataButton: TButton;
    WriteDataButton: TButton;
    DataValueEdit: TEdit;
    RoutineGroupBox: TGroupBox;
    RoutineIDEdit: TEdit;
    StartRoutineButton: TButton;
    StopRoutineButton: TButton;
    GetRoutineResultButton: TButton;
    ResultMemo: TMemo;
    StatusBar: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure StartSessionButtonClick(Sender: TObject);
    procedure RequestSeedButtonClick(Sender: TObject);
    procedure SendKeyButtonClick(Sender: TObject);
    procedure ReadDTCButtonClick(Sender: TObject);
    procedure ClearDTCButtonClick(Sender: TObject);
    procedure ReadDataButtonClick(Sender: TObject);
    procedure WriteDataButtonClick(Sender: TObject);
    procedure StartRoutineButtonClick(Sender: TObject);
    procedure StopRoutineButtonClick(Sender: TObject);
    procedure GetRoutineResultButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FUDS: TUDSProtocol;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
    function BytesToHex(const Data: TBytes): string;
  end;

var
  UDSForm: TUDSForm;

implementation

{$R *.dfm}

procedure TUDSForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  PortComboBox.Clear;
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  SessionCombo.Items.AddStrings(['Default', 'Programming', 'Extended Diagnostic', 'Safety System']);
  SessionCombo.ItemIndex := 0;
  
  DTCSubfunctionCombo.Items.AddStrings(['Report Number', 'Report by Status Mask', 'Report Snapshot ID', 'Report Extended Data']);
  DTCSubfunctionCombo.ItemIndex := 1;
  
  SecurityLevelEdit.Text := '1';
  DataIDEdit.Text := '$F190';
  DataValueEdit.Text := '00 01 02 03';
  RoutineIDEdit.Text := '$0203';
  
  UpdateConnectionState(False);
  Log('UDS (ISO 14229) - Unified Diagnostic Services Example');
  Log('Ready to connect');
end;

procedure TUDSForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FUDS) then FUDS.Free;
  if Assigned(FAdapter) then FAdapter.Free;
  if Assigned(FConnection) then FConnection.Free;
end;

procedure TUDSForm.Log(const Msg: string);
begin
  ResultMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
  Application.ProcessMessages;
end;

function TUDSForm.BytesToHex(const Data: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Data) do
    Result := Result + IntToHex(Data[I], 2) + ' ';
  Result := Trim(Result);
end;

procedure TUDSForm.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  PortComboBox.Enabled := not Connected;
  StartSessionButton.Enabled := Connected;
  RequestSeedButton.Enabled := Connected;
  SendKeyButton.Enabled := Connected;
  ReadDTCButton.Enabled := Connected;
  ClearDTCButton.Enabled := Connected;
  ReadDataButton.Enabled := Connected;
  WriteDataButton.Enabled := Connected;
  StartRoutineButton.Enabled := Connected;
  StopRoutineButton.Enabled := Connected;
  GetRoutineResultButton.Enabled := Connected;
end;

procedure TUDSForm.ConnectButtonClick(Sender: TObject);
begin
  try
    Log('Connecting to ' + PortComboBox.Text + '...');
    FConnection := TOBDConnection.Create(PortComboBox.Text, 115200);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      Log('Connected: ' + FAdapter.Version);
      FUDS := TUDSProtocol.Create;
      UpdateConnectionState(True);
      Log('UDS protocol ready');
    end
    else
    begin
      Log('Connection failed');
      FreeAndNil(FAdapter);
      FreeAndNil(FConnection);
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.DisconnectButtonClick(Sender: TObject);
begin
  try
    if Assigned(FUDS) then FreeAndNil(FUDS);
    if Assigned(FAdapter) then
    begin
      FAdapter.Disconnect;
      FreeAndNil(FAdapter);
    end;
    if Assigned(FConnection) then FreeAndNil(FConnection);
    Log('Disconnected');
    UpdateConnectionState(False);
  except
    on E: Exception do
      Log('Disconnect error: ' + E.Message);
  end;
end;

procedure TUDSForm.StartSessionButtonClick(Sender: TObject);
var
  Request: TBytes;
  SessionType: TUDSSessionType;
begin
  if not Assigned(FUDS) then Exit;
  try
    case SessionCombo.ItemIndex of
      0: SessionType := udsDefaultSession;
      1: SessionType := udsProgrammingSession;
      2: SessionType := udsExtendedDiagnostic;
      3: SessionType := udsSafetySystemDiagnostic;
    else
      SessionType := udsDefaultSession;
    end;
    
    Log('Starting ' + SessionCombo.Text + ' session...');
    Request := FUDS.DiagnosticSessionControl(SessionType);
    Log('TX: ' + BytesToHex(Request));
    Log('Session started successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.RequestSeedButtonClick(Sender: TObject);
var
  Request: TBytes;
  Level: Byte;
begin
  if not Assigned(FUDS) then Exit;
  try
    Level := StrToIntDef(SecurityLevelEdit.Text, 1);
    Log('Requesting security seed (Level ' + IntToStr(Level) + ')...');
    Request := FUDS.SecurityAccessRequestSeed(Level);
    Log('TX: ' + BytesToHex(Request));
    Log('Seed requested - calculate key and send');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.SendKeyButtonClick(Sender: TObject);
var
  Request: TBytes;
  Key: TBytes;
  Level: Byte;
begin
  if not Assigned(FUDS) then Exit;
  try
    Level := StrToIntDef(SecurityLevelEdit.Text, 1);
    SetLength(Key, 4);
    Key[0] := $12; Key[1] := $34; Key[2] := $56; Key[3] := $78;
    
    Log('Sending security key...');
    Request := FUDS.SecurityAccessSendKey(Level + 1, Key);
    Log('TX: ' + BytesToHex(Request));
    Log('Security access granted');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.ReadDTCButtonClick(Sender: TObject);
var
  Request: TBytes;
  Subfunction: Byte;
begin
  if not Assigned(FUDS) then Exit;
  try
    case DTCSubfunctionCombo.ItemIndex of
      0: Subfunction := $01;
      1: Subfunction := $02;
      2: Subfunction := $03;
      3: Subfunction := $06;
    else
      Subfunction := $02;
    end;
    
    Log('Reading DTCs (subfunction ' + IntToHex(Subfunction, 2) + ')...');
    Request := FUDS.ReadDTCInformation(Subfunction);
    Log('TX: ' + BytesToHex(Request));
    Log('Waiting for DTC data from ECU...');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.ClearDTCButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FUDS) then Exit;
  try
    if MessageDlg('Clear all diagnostic trouble codes?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    
    Log('Clearing all DTCs...');
    Request := FUDS.ClearDiagnosticInformation($FFFFFF);
    Log('TX: ' + BytesToHex(Request));
    Log('DTCs cleared');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.ReadDataButtonClick(Sender: TObject);
var
  Request: TBytes;
  DataID: Word;
begin
  if not Assigned(FUDS) then Exit;
  try
    DataID := StrToInt(DataIDEdit.Text);
    Log('Reading data by identifier ' + IntToHex(DataID, 4) + '...');
    Request := FUDS.ReadDataByIdentifier(DataID);
    Log('TX: ' + BytesToHex(Request));
    Log('Waiting for data from ECU...');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.WriteDataButtonClick(Sender: TObject);
var
  Request: TBytes;
  DataID: Word;
  Data: TBytes;
  Values: TArray<string>;
  I: Integer;
begin
  if not Assigned(FUDS) then Exit;
  try
    DataID := StrToInt(DataIDEdit.Text);
    Values := DataValueEdit.Text.Split([' ']);
    SetLength(Data, Length(Values));
    for I := 0 to High(Values) do
      Data[I] := StrToInt('$' + Values[I]);
    
    Log('Writing data by identifier ' + IntToHex(DataID, 4) + '...');
    Request := FUDS.WriteDataByIdentifier(DataID, Data);
    Log('TX: ' + BytesToHex(Request));
    Log('Data written successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.StartRoutineButtonClick(Sender: TObject);
var
  Request: TBytes;
  RoutineID: Word;
begin
  if not Assigned(FUDS) then Exit;
  try
    RoutineID := StrToInt(RoutineIDEdit.Text);
    Log('Starting routine ' + IntToHex(RoutineID, 4) + '...');
    Request := FUDS.RoutineControl($01, RoutineID, []);
    Log('TX: ' + BytesToHex(Request));
    Log('Routine started');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.StopRoutineButtonClick(Sender: TObject);
var
  Request: TBytes;
  RoutineID: Word;
begin
  if not Assigned(FUDS) then Exit;
  try
    RoutineID := StrToInt(RoutineIDEdit.Text);
    Log('Stopping routine ' + IntToHex(RoutineID, 4) + '...');
    Request := FUDS.RoutineControl($02, RoutineID, []);
    Log('TX: ' + BytesToHex(Request));
    Log('Routine stopped');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TUDSForm.GetRoutineResultButtonClick(Sender: TObject);
var
  Request: TBytes;
  RoutineID: Word;
begin
  if not Assigned(FUDS) then Exit;
  try
    RoutineID := StrToInt(RoutineIDEdit.Text);
    Log('Getting routine ' + IntToHex(RoutineID, 4) + ' results...');
    Request := FUDS.RoutineControl($03, RoutineID, []);
    Log('TX: ' + BytesToHex(Request));
    Log('Waiting for routine results...');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.
