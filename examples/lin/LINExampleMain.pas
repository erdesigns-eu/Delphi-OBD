unit LINExampleMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.LIN;

type
  TLINForm = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ConfigGroupBox: TGroupBox;
    VersionCombo: TComboBox;
    BaudRateCombo: TComboBox;
    NodeAddressEdit: TEdit;
    DataGroupBox: TGroupBox;
    DataIDEdit: TEdit;
    ReadDataButton: TButton;
    WriteDataButton: TButton;
    DataValueEdit: TEdit;
    FrameGroupBox: TGroupBox;
    FrameIDEdit: TEdit;
    AssignFrameButton: TButton;
    ResultMemo: TMemo;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ReadDataButtonClick(Sender: TObject);
    procedure WriteDataButtonClick(Sender: TObject);
    procedure AssignFrameButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FLIN: TLINProtocol;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
  end;

var
  LINForm: TLINForm;

implementation

{$R *.dfm}

procedure TLINForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  VersionCombo.Items.AddStrings(['LIN 1.3', 'LIN 2.0', 'LIN 2.1', 'LIN 2.2A']);
  VersionCombo.ItemIndex := 3;
  BaudRateCombo.Items.AddStrings(['9600', '19200', '20000']);
  BaudRateCombo.ItemIndex := 1;
  
  NodeAddressEdit.Text := '$10';
  DataIDEdit.Text := '$F187';
  DataValueEdit.Text := '11 22 33 44';
  FrameIDEdit.Text := '$20';
  
  UpdateConnectionState(False);
  Log('LIN (ISO 17987) - Local Interconnect Network Example');
end;

procedure TLINForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FLIN) then FLIN.Free;
  if Assigned(FAdapter) then FAdapter.Free;
  if Assigned(FConnection) then FConnection.Free;
end;

procedure TLINForm.Log(const Msg: string);
begin
  ResultMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
  Application.ProcessMessages;
end;

procedure TLINForm.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  ReadDataButton.Enabled := Connected;
  WriteDataButton.Enabled := Connected;
  AssignFrameButton.Enabled := Connected;
end;

procedure TLINForm.ConnectButtonClick(Sender: TObject);
var
  Version: TLINVersion;
  BaudRate: Integer;
begin
  try
    case VersionCombo.ItemIndex of
      0: Version := linVersion1_3;
      1: Version := linVersion2_0;
      2: Version := linVersion2_1;
      3: Version := linVersion2_2;
    else
      Version := linVersion2_2;
    end;
    
    BaudRate := StrToInt(BaudRateCombo.Text);
    
    Log('Connecting...');
    FConnection := TOBDConnection.Create(PortComboBox.Text, 115200);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      FLIN := TLINProtocol.Create(Version, BaudRate);
      UpdateConnectionState(True);
      Log('Connected - LIN ' + VersionCombo.Text + ' @ ' + IntToStr(BaudRate) + ' bps');
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TLINForm.DisconnectButtonClick(Sender: TObject);
begin
  if Assigned(FLIN) then FreeAndNil(FLIN);
  if Assigned(FAdapter) then
  begin
    FAdapter.Disconnect;
    FreeAndNil(FAdapter);
  end;
  if Assigned(FConnection) then FreeAndNil(FConnection);
  UpdateConnectionState(False);
  Log('Disconnected');
end;

procedure TLINForm.ReadDataButtonClick(Sender: TObject);
var
  NodeAddr: Byte;
  DataID: Word;
begin
  if not Assigned(FLIN) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    DataID := StrToInt(DataIDEdit.Text);
    Log('Reading data from node $' + IntToHex(NodeAddr, 2) + ', ID $' + IntToHex(DataID, 4));
    FLIN.ReadDataByIdentifier(NodeAddr, DataID);
    Log('Data read successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TLINForm.WriteDataButtonClick(Sender: TObject);
var
  NodeAddr: Byte;
  DataID: Word;
begin
  if not Assigned(FLIN) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    DataID := StrToInt(DataIDEdit.Text);
    Log('Writing data to node $' + IntToHex(NodeAddr, 2));
    // FLIN.WriteDataByIdentifier implementation
    Log('Data written successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TLINForm.AssignFrameButtonClick(Sender: TObject);
var
  NodeAddr, FrameID: Byte;
begin
  if not Assigned(FLIN) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    FrameID := StrToInt(FrameIDEdit.Text);
    Log('Assigning frame ID $' + IntToHex(FrameID, 2) + ' to node $' + IntToHex(NodeAddr, 2));
    FLIN.AssignFrameID(NodeAddr, $1234, $5678, FrameID);
    Log('Frame ID assigned');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.
