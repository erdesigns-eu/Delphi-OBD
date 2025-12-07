unit MOSTExampleMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.MOST;

type
  TMOSTForm = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ConfigGroupBox: TGroupBox;
    VersionCombo: TComboBox;
    NodeAddressEdit: TEdit;
    FunctionBlockCombo: TComboBox;
    PropertyGroupBox: TGroupBox;
    PropertyIDEdit: TEdit;
    GetPropertyButton: TButton;
    SetPropertyButton: TButton;
    PropertyValueEdit: TEdit;
    StreamingGroupBox: TGroupBox;
    ChannelIDEdit: TEdit;
    RequestChannelButton: TButton;
    ResultMemo: TMemo;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure GetPropertyButtonClick(Sender: TObject);
    procedure SetPropertyButtonClick(Sender: TObject);
    procedure RequestChannelButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FMOST: TMOSTProtocol;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
  end;

var
  MOSTForm: TMOSTForm;

implementation

{$R *.dfm}

procedure TMOSTForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  VersionCombo.Items.AddStrings(['MOST25 (25 Mbps)', 'MOST50 (50 Mbps)', 'MOST150 (150 Mbps)']);
  VersionCombo.ItemIndex := 2;
  
  FunctionBlockCombo.Items.AddStrings(['Audio Disk Player', 'Audio Amplifier', 'Video Display', 'Phone', 'Navigation', 'Diagnostics']);
  FunctionBlockCombo.ItemIndex := 0;
  
  NodeAddressEdit.Text := '$200';
  PropertyIDEdit.Text := '$0100';
  PropertyValueEdit.Text := '00 50';
  ChannelIDEdit.Text := '5';
  
  UpdateConnectionState(False);
  Log('MOST - Media Oriented Systems Transport Example');
end;

procedure TMOSTForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FMOST) then FMOST.Free;
  if Assigned(FAdapter) then FAdapter.Free;
  if Assigned(FConnection) then FConnection.Free;
end;

procedure TMOSTForm.Log(const Msg: string);
begin
  ResultMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
  Application.ProcessMessages;
end;

procedure TMOSTForm.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  GetPropertyButton.Enabled := Connected;
  SetPropertyButton.Enabled := Connected;
  RequestChannelButton.Enabled := Connected;
end;

procedure TMOSTForm.ConnectButtonClick(Sender: TObject);
var
  Version: TMOSTVersion;
begin
  try
    case VersionCombo.ItemIndex of
      0: Version := most25;
      1: Version := most50;
      2: Version := most150;
    else
      Version := most150;
    end;
    
    Log('Connecting...');
    FConnection := TOBDConnection.Create(PortComboBox.Text, 115200);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      FMOST := TMOSTProtocol.Create(Version);
      UpdateConnectionState(True);
      Log('Connected - MOST ' + VersionCombo.Text + ' ready');
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TMOSTForm.DisconnectButtonClick(Sender: TObject);
begin
  if Assigned(FMOST) then FreeAndNil(FMOST);
  if Assigned(FAdapter) then
  begin
    FAdapter.Disconnect;
    FreeAndNil(FAdapter);
  end;
  if Assigned(FConnection) then FreeAndNil(FConnection);
  UpdateConnectionState(False);
  Log('Disconnected');
end;

procedure TMOSTForm.GetPropertyButtonClick(Sender: TObject);
var
  NodeAddr: Word;
  FuncBlock: TMOSTFunctionBlock;
  PropertyID: Word;
begin
  if not Assigned(FMOST) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    PropertyID := StrToInt(PropertyIDEdit.Text);
    
    case FunctionBlockCombo.ItemIndex of
      0: FuncBlock := fbAudioDiskPlayer;
      1: FuncBlock := fbAudioAmplifier;
      2: FuncBlock := fbVideoDisplay;
      3: FuncBlock := fbPhone;
      4: FuncBlock := fbNavigation;
      5: FuncBlock := fbDiagnostics;
    else
      FuncBlock := fbAudioDiskPlayer;
    end;
    
    Log('Getting property from node $' + IntToHex(NodeAddr, 3) + '...');
    FMOST.GetProperty(NodeAddr, Byte(FuncBlock), PropertyID);
    Log('Property read successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TMOSTForm.SetPropertyButtonClick(Sender: TObject);
var
  NodeAddr: Word;
  FuncBlock: TMOSTFunctionBlock;
  PropertyID: Word;
  Data: TBytes;
begin
  if not Assigned(FMOST) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    PropertyID := StrToInt(PropertyIDEdit.Text);
    
    case FunctionBlockCombo.ItemIndex of
      0: FuncBlock := fbAudioDiskPlayer;
      1: FuncBlock := fbAudioAmplifier;
      2: FuncBlock := fbVideoDisplay;
      3: FuncBlock := fbPhone;
      4: FuncBlock := fbNavigation;
      5: FuncBlock := fbDiagnostics;
    else
      FuncBlock := fbAudioDiskPlayer;
    end;
    
    SetLength(Data, 2);
    Data[0] := $00;
    Data[1] := $50;
    
    Log('Setting property on node $' + IntToHex(NodeAddr, 3) + '...');
    FMOST.SetProperty(NodeAddr, Byte(FuncBlock), PropertyID, Data);
    Log('Property set successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TMOSTForm.RequestChannelButtonClick(Sender: TObject);
var
  NodeAddr: Word;
  ChannelID: Byte;
begin
  if not Assigned(FMOST) then Exit;
  try
    NodeAddr := StrToInt(NodeAddressEdit.Text);
    ChannelID := StrToInt(ChannelIDEdit.Text);
    
    Log('Requesting streaming channel ' + IntToStr(ChannelID) + ' from node $' + IntToHex(NodeAddr, 3) + '...');
    FMOST.RequestStreamingChannel(NodeAddr, ChannelID, 1024);
    Log('Streaming channel requested');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.
