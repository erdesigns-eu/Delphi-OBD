unit KWP2000ExampleMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.KWP2000;

type
  TKWP2000Form = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortLabel: TLabel;
    PortComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    SessionGroupBox: TGroupBox;
    SessionLabel: TLabel;
    SessionCombo: TComboBox;
    StartSessionButton: TButton;
    SecurityGroupBox: TGroupBox;
    SecurityLevelLabel: TLabel;
    SecurityLevelEdit: TEdit;
    RequestSeedButton: TButton;
    SendKeyButton: TButton;
    KeyEdit: TEdit;
    KeyLabel: TLabel;
    DiagnosticsGroupBox: TGroupBox;
    ReadDTCButton: TButton;
    ClearDTCButton: TButton;
    ReadECUIDButton: TButton;
    TesterPresentButton: TButton;
    ECUProgrammingGroupBox: TGroupBox;
    RequestDownloadButton: TButton;
    TransferDataButton: TButton;
    RequestTransferExitButton: TButton;
    AddressEdit: TEdit;
    AddressLabel: TLabel;
    SizeEdit: TEdit;
    SizeLabel: TLabel;
    ResultMemo: TMemo;
    ResultLabel: TLabel;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure StartSessionButtonClick(Sender: TObject);
    procedure RequestSeedButtonClick(Sender: TObject);
    procedure SendKeyButtonClick(Sender: TObject);
    procedure ReadDTCButtonClick(Sender: TObject);
    procedure ClearDTCButtonClick(Sender: TObject);
    procedure ReadECUIDButtonClick(Sender: TObject);
    procedure TesterPresentButtonClick(Sender: TObject);
    procedure RequestDownloadButtonClick(Sender: TObject);
    procedure TransferDataButtonClick(Sender: TObject);
    procedure RequestTransferExitButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FKWP2000: TKWP2000Protocol;
    FSeedData: TBytes;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
    function BytesToHex(const Data: TBytes): string;
    function HexToBytes(const Hex: string): TBytes;
    function CalculateKeyFromSeed(const Seed: TBytes): TBytes;
  end;

var
  KWP2000Form: TKWP2000Form;

implementation

{$R *.dfm}

procedure TKWP2000Form.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // Initialize COM ports
  PortComboBox.Clear;
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  // Initialize sessions
  SessionCombo.Items.Add('Default Session');
  SessionCombo.Items.Add('Programming Session');
  SessionCombo.Items.Add('Extended Diagnostic Session');
  SessionCombo.ItemIndex := 0;
  
  // Default values
  SecurityLevelEdit.Text := '1';
  KeyEdit.Text := '12345678';
  AddressEdit.Text := '$00100000';
  SizeEdit.Text := '$00010000';
  
  UpdateConnectionState(False);
  Log('KWP2000 (ISO 14230) Protocol Example');
  Log('-----------------------------------');
  Log('Ready to connect');
end;

procedure TKWP2000Form.FormDestroy(Sender: TObject);
begin
  if Assigned(FKWP2000) then
    FKWP2000.Free;
  if Assigned(FAdapter) then
    FAdapter.Free;
  if Assigned(FConnection) then
    FConnection.Free;
end;

procedure TKWP2000Form.Log(const Msg: string);
begin
  ResultMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
  Application.ProcessMessages;
end;

function TKWP2000Form.BytesToHex(const Data: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Data) do
    Result := Result + IntToHex(Data[I], 2) + ' ';
  Result := Trim(Result);
end;

function TKWP2000Form.HexToBytes(const Hex: string): TBytes;
var
  S: string;
  I, Len: Integer;
begin
  S := StringReplace(Hex, ' ', '', [rfReplaceAll]);
  S := StringReplace(S, '$', '', [rfReplaceAll]);
  Len := Length(S) div 2;
  SetLength(Result, Len);
  
  for I := 0 to Len - 1 do
    Result[I] := StrToInt('$' + Copy(S, I * 2 + 1, 2));
end;

function TKWP2000Form.CalculateKeyFromSeed(const Seed: TBytes): TBytes;
var
  I: Integer;
begin
  // Simple key calculation (in production, use manufacturer's algorithm)
  SetLength(Result, Length(Seed));
  for I := 0 to High(Seed) do
    Result[I] := Seed[I] xor $AA;
end;

procedure TKWP2000Form.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  PortComboBox.Enabled := not Connected;
  
  StartSessionButton.Enabled := Connected;
  RequestSeedButton.Enabled := Connected;
  SendKeyButton.Enabled := Connected;
  ReadDTCButton.Enabled := Connected;
  ClearDTCButton.Enabled := Connected;
  ReadECUIDButton.Enabled := Connected;
  TesterPresentButton.Enabled := Connected;
  RequestDownloadButton.Enabled := Connected;
  TransferDataButton.Enabled := Connected;
  RequestTransferExitButton.Enabled := Connected;
end;

procedure TKWP2000Form.ConnectButtonClick(Sender: TObject);
var
  PortName: string;
begin
  try
    PortName := PortComboBox.Text;
    Log('Connecting to ' + PortName + '...');
    
    FConnection := TOBDConnection.Create(PortName, 115200);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      Log('Connected: ' + FAdapter.Version);
      
      FKWP2000 := TKWP2000Protocol.Create;
      
      UpdateConnectionState(True);
      Log('KWP2000 protocol ready');
    end
    else
    begin
      Log('Connection failed');
      FAdapter.Free;
      FAdapter := nil;
      FConnection.Free;
      FConnection := nil;
    end;
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Connection error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.DisconnectButtonClick(Sender: TObject);
begin
  try
    if Assigned(FKWP2000) then
      FreeAndNil(FKWP2000);
    
    if Assigned(FAdapter) then
    begin
      FAdapter.Disconnect;
      FreeAndNil(FAdapter);
    end;
    
    if Assigned(FConnection) then
      FreeAndNil(FConnection);
    
    Log('Disconnected');
    UpdateConnectionState(False);
  except
    on E: Exception do
      Log('Disconnect error: ' + E.Message);
  end;
end;

procedure TKWP2000Form.StartSessionButtonClick(Sender: TObject);
var
  Request: TBytes;
  SessionType: TKWP2000DiagnosticSession;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    case SessionCombo.ItemIndex of
      0: SessionType := kwpDefaultSession;
      1: SessionType := kwpProgrammingSession;
      2: SessionType := kwpExtendedDiagnosticSession;
    else
      SessionType := kwpDefaultSession;
    end;
    
    Log('Starting ' + SessionCombo.Text + '...');
    Request := FKWP2000.StartDiagnosticSession(SessionType);
    Log('TX: ' + BytesToHex(Request));
    Log('Session started successfully');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Session start error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.RequestSeedButtonClick(Sender: TObject);
var
  Request: TBytes;
  Level: Byte;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Level := StrToIntDef(SecurityLevelEdit.Text, 1);
    Log('Requesting security seed (Level ' + IntToStr(Level) + ')...');
    
    Request := FKWP2000.RequestSecuritySeed(Level);
    Log('TX: ' + BytesToHex(Request));
    
    // Simulate receiving seed (in real app, parse ECU response)
    SetLength(FSeedData, 4);
    FSeedData[0] := $12; FSeedData[1] := $34;
    FSeedData[2] := $56; FSeedData[3] := $78;
    
    Log('RX (simulated): Seed = ' + BytesToHex(FSeedData));
    Log('Calculate key and click "Send Key"');
    
    // Calculate and display key
    KeyEdit.Text := BytesToHex(CalculateKeyFromSeed(FSeedData));
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Seed request error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.SendKeyButtonClick(Sender: TObject);
var
  Request: TBytes;
  Key: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Key := HexToBytes(KeyEdit.Text);
    
    Log('Sending security key...');
    Log('Key: ' + BytesToHex(Key));
    
    Request := FKWP2000.SendSecurityKey(Key);
    Log('TX: ' + BytesToHex(Request));
    Log('Security access granted');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Send key error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.ReadDTCButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Log('Reading diagnostic trouble codes...');
    Request := FKWP2000.ReadDTCByStatus($FF);
    Log('TX: ' + BytesToHex(Request));
    Log('Waiting for DTCs from ECU...');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('DTC read error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.ClearDTCButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    if MessageDlg('Clear all diagnostic trouble codes?', 
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    
    Log('Clearing diagnostic trouble codes...');
    Request := FKWP2000.ClearDTC($FFFFFF);
    Log('TX: ' + BytesToHex(Request));
    Log('DTCs cleared');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('DTC clear error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.ReadECUIDButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Log('Reading ECU identification...');
    Request := FKWP2000.ReadECUIdentification($87);
    Log('TX: ' + BytesToHex(Request));
    Log('Waiting for ECU ID...');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('ECU ID read error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.TesterPresentButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Log('Sending Tester Present...');
    Request := FKWP2000.TesterPresent;
    Log('TX: ' + BytesToHex(Request));
    Log('Keep-alive sent');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Tester Present error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.RequestDownloadButtonClick(Sender: TObject);
var
  Request: TBytes;
  Address, Size: Cardinal;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Address := StrToInt(AddressEdit.Text);
    Size := StrToInt(SizeEdit.Text);
    
    Log('Requesting download...');
    Log('Address: ' + IntToHex(Address, 8));
    Log('Size: ' + IntToHex(Size, 8));
    
    Request := FKWP2000.RequestDownload(Address, Size);
    Log('TX: ' + BytesToHex(Request));
    Log('Download request sent');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Download request error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.TransferDataButtonClick(Sender: TObject);
var
  Request: TBytes;
  Data: TBytes;
  I: Integer;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    // Create sample data block
    SetLength(Data, 128);
    for I := 0 to High(Data) do
      Data[I] := I and $FF;
    
    Log('Transferring data block (128 bytes)...');
    Request := FKWP2000.TransferData(1, Data);
    Log('TX: ' + BytesToHex(Request));
    Log('Data transfer sent');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Data transfer error: ' + E.Message);
    end;
  end;
end;

procedure TKWP2000Form.RequestTransferExitButtonClick(Sender: TObject);
var
  Request: TBytes;
begin
  if not Assigned(FKWP2000) then Exit;
  
  try
    Log('Requesting transfer exit...');
    Request := FKWP2000.RequestTransferExit;
    Log('TX: ' + BytesToHex(Request));
    Log('Transfer exit requested');
    Log('---');
  except
    on E: Exception do
    begin
      Log('Error: ' + E.Message);
      ShowMessage('Transfer exit error: ' + E.Message);
    end;
  end;
end;

end.
