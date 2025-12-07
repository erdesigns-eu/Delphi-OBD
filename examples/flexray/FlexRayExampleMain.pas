unit FlexRayExampleMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  OBD.Adapter.ELM327, OBD.Connection, OBD.Protocol.FlexRay;

type
  TFlexRayForm = class(TForm)
    MainPanel: TPanel;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    ConnectionGroupBox: TGroupBox;
    PortComboBox: TComboBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ConfigGroupBox: TGroupBox;
    SpeedCombo: TComboBox;
    ChannelCombo: TComboBox;
    ConfigureButton: TButton;
    StaticGroupBox: TGroupBox;
    StaticSlotEdit: TEdit;
    SendStaticButton: TButton;
    DynamicGroupBox: TGroupBox;
    DynamicSlotEdit: TEdit;
    SendDynamicButton: TButton;
    ResultMemo: TMemo;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ConfigureButtonClick(Sender: TObject);
    procedure SendStaticButtonClick(Sender: TObject);
    procedure SendDynamicButtonClick(Sender: TObject);
  private
    FConnection: TOBDConnection;
    FAdapter: TOBDELM327Adapter;
    FFlexRay: TFlexRayProtocol;
    procedure Log(const Msg: string);
    procedure UpdateConnectionState(Connected: Boolean);
  end;

var
  FlexRayForm: TFlexRayForm;

implementation

{$R *.dfm}

procedure TFlexRayForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 20 do
    PortComboBox.Items.Add('COM' + IntToStr(I));
  PortComboBox.ItemIndex := 0;
  
  SpeedCombo.Items.AddStrings(['2.5 Mbps', '5.0 Mbps', '10.0 Mbps']);
  SpeedCombo.ItemIndex := 1;
  ChannelCombo.Items.AddStrings(['Channel A', 'Channel B', 'Channel A+B']);
  ChannelCombo.ItemIndex := 0;
  
  StaticSlotEdit.Text := '10';
  DynamicSlotEdit.Text := '100';
  
  UpdateConnectionState(False);
  Log('FlexRay (ISO 17458) - High-Speed Deterministic Network Example');
end;

procedure TFlexRayForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FFlexRay) then FFlexRay.Free;
  if Assigned(FAdapter) then FAdapter.Free;
  if Assigned(FConnection) then FConnection.Free;
end;

procedure TFlexRayForm.Log(const Msg: string);
begin
  ResultMemo.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  StatusBar.SimpleText := Msg;
  Application.ProcessMessages;
end;

procedure TFlexRayForm.UpdateConnectionState(Connected: Boolean);
begin
  ConnectButton.Enabled := not Connected;
  DisconnectButton.Enabled := Connected;
  ConfigureButton.Enabled := Connected;
  SendStaticButton.Enabled := Connected;
  SendDynamicButton.Enabled := Connected;
end;

procedure TFlexRayForm.ConnectButtonClick(Sender: TObject);
begin
  try
    Log('Connecting...');
    FConnection := TOBDConnection.Create(PortComboBox.Text, 115200);
    FAdapter := TOBDELM327Adapter.Create(FConnection);
    
    if FAdapter.Connect then
    begin
      FFlexRay := TFlexRayProtocol.Create;
      UpdateConnectionState(True);
      Log('Connected - FlexRay protocol ready');
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TFlexRayForm.DisconnectButtonClick(Sender: TObject);
begin
  if Assigned(FFlexRay) then FreeAndNil(FFlexRay);
  if Assigned(FAdapter) then
  begin
    FAdapter.Disconnect;
    FreeAndNil(FAdapter);
  end;
  if Assigned(FConnection) then FreeAndNil(FConnection);
  UpdateConnectionState(False);
  Log('Disconnected');
end;

procedure TFlexRayForm.ConfigureButtonClick(Sender: TObject);
var
  Speed: Cardinal;
begin
  if not Assigned(FFlexRay) then Exit;
  try
    case SpeedCombo.ItemIndex of
      0: Speed := 2500;
      1: Speed := 5000;
      2: Speed := 10000;
    else
      Speed := 5000;
    end;
    
    Log('Configuring FlexRay cluster (' + IntToStr(Speed) + ' kbps)...');
    FFlexRay.ConfigureCluster(Speed, 60, 50, 10, 10.0);
    Log('Cluster configured successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TFlexRayForm.SendStaticButtonClick(Sender: TObject);
var
  Slot: Word;
  Data: TBytes;
  Channel: TFlexRayChannel;
begin
  if not Assigned(FFlexRay) then Exit;
  try
    Slot := StrToInt(StaticSlotEdit.Text);
    SetLength(Data, 16);
    FillChar(Data[0], Length(Data), $AA);
    
    case ChannelCombo.ItemIndex of
      0: Channel := frChannelA;
      1: Channel := frChannelB;
      2: Channel := frChannelAB;
    else
      Channel := frChannelA;
    end;
    
    Log('Sending static frame (slot ' + IntToStr(Slot) + ')...');
    FFlexRay.SendStaticFrame(Slot, Data, Channel);
    Log('Static frame sent successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

procedure TFlexRayForm.SendDynamicButtonClick(Sender: TObject);
var
  Slot: Word;
  Data: TBytes;
  Channel: TFlexRayChannel;
begin
  if not Assigned(FFlexRay) then Exit;
  try
    Slot := StrToInt(DynamicSlotEdit.Text);
    SetLength(Data, 16);
    FillChar(Data[0], Length(Data), $55);
    
    case ChannelCombo.ItemIndex of
      0: Channel := frChannelA;
      1: Channel := frChannelB;
      2: Channel := frChannelAB;
    else
      Channel := frChannelA;
    end;
    
    Log('Sending dynamic frame (slot ' + IntToStr(Slot) + ')...');
    FFlexRay.SendDynamicFrame(Slot, Data, Channel);
    Log('Dynamic frame sent successfully');
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

end.
