//------------------------------------------------------------------------------
//  untPageCoding
//
//  OBD Studio Pro page for long coding, adaptation changes, security access
//  and guarded writes on the sample Golf VII.
//
//  Components used: TOBDVehicleInfoCard, TOBDTabs, TOBDComboBox,
//  TOBDBitEditor, TOBDDiffView, TOBDWriteConfirm, TOBDSecurityAccessCard,
//  TOBDCard and TOBDInspector.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageCoding;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.VehicleCard, ERD.UI.Tabs, ERD.UI.Edits, ERD.UI.ScrollBox,
  ERD.UI.BitEditor, ERD.UI.DiffView, ERD.UI.WriteConfirm,
  ERD.UI.SecurityAccessCard, ERD.UI.Card, ERD.UI.Inspector,
  ERD.UI.PreconditionList,
  untProPage, untSimulator;

type
  TfrmCoding = class(TFrame, IProPage)
    vcVehicle: TOBDVehicleInfoCard;
    sbPage: TOBDScrollBox;
    pnlTabs: TPanel;
    cbEcu: TOBDComboBox;
    tabsCoding: TOBDTabs;
    pnlCoding: TPanel;
    bitsCoding: TOBDBitEditor;
    diffCoding: TOBDDiffView;
    wrConfirm: TOBDWriteConfirm;
    pnlBottom: TPanel;
    cdAdaptations: TOBDCard;
    insAdaptations: TOBDInspector;
    pnlBottomRight: TPanel;
    secAccess: TOBDSecurityAccessCard;
    cdOnline: TOBDCard;
    insOnline: TOBDInspector;
    procedure bitsCodingChange(Sender: TObject; AChangedCount: Integer);
    procedure wrConfirmConfirm(Sender: TObject);
    procedure wrConfirmBackup(Sender: TObject);
    procedure wrConfirmCancel(Sender: TObject);
  private
    FHost: IProHost;
    FOriginalCoding: TBytes;
    FWriteActive: Boolean;
    FWriteTick: Integer;
    FBackupDone: Boolean;
    procedure FillVehicle;
    procedure FillTabs;
    procedure FillCoding;
    procedure FillDiff(AWritten: Boolean = False);
    procedure FillAdaptations;
    procedure FillOnline;
    procedure UpdateWriteStrip;
    procedure StartWrite;
    procedure FinishWrite;
    function ChangedBits: Integer;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

procedure AddInspectorRow(AInspector: TOBDInspector; const ACategory, AName,
  AValue: string; AKind: TOBDInspectorValueKind; ALevel: TOBDAlertLevel = alvNormal);
var
  Category: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
  I: Integer;
begin
  Category := nil;
  for I := 0 to AInspector.Categories.Count - 1 do
    if SameText(AInspector.Categories[I].Caption, ACategory) then
    begin
      Category := AInspector.Categories[I];
      Break;
    end;
  if Category = nil then
    Category := AInspector.AddCategory(ACategory);
  Prop := Category.AddProperty(AName, AValue, AKind);
  Prop.Level := ALevel;
end;

{ TfrmCoding }

procedure TfrmCoding.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FBackupDone := True;
  FillVehicle;
  FillTabs;
  FillCoding;
  FillDiff;
  FillAdaptations;
  FillOnline;
  UpdateWriteStrip;
  ConnectionChanged;
end;

procedure TfrmCoding.Tick;
var
  Progress: Integer;
begin
  if secAccess.State = sacUnlocked then
  begin
    secAccess.KeepAliveProgress := 1 - Frac((GetTickCount mod 2000) / 2000);
    secAccess.KeepAliveText := 'Tester present '#183' 2 s';
  end;
  if not FWriteActive then
    Exit;
  Inc(FWriteTick);
  Progress := EnsureRange(FWriteTick * 8, 0, 100);
  if FHost <> nil then
    FHost.SetProgress('Writing 09 Central electrics coding', Progress);
  if FWriteTick >= 13 then
    FinishWrite;
end;

procedure TfrmCoding.ConnectionChanged;
begin
  FillVehicle;
  if (FHost <> nil) and FHost.Connected then
  begin
    secAccess.State := sacUnlocked;
    secAccess.ControlUnit := '09 Central electrics';
    secAccess.SessionText := 'Extended diagnostic (0x03)';
    secAccess.LevelText := '0x03 / 0x04 seed-key';
    secAccess.KeepAliveText := 'Tester present '#183' 2 s';
  end
  else
  begin
    secAccess.State := sacLocked;
    secAccess.ControlUnit := '09 Central electrics';
    secAccess.SessionText := 'Default session';
    secAccess.LevelText := 'Locked';
    secAccess.KeepAliveText := 'Offline';
  end;
end;

procedure TfrmCoding.bitsCodingChange(Sender: TObject; AChangedCount: Integer);
begin
  FillDiff;
  UpdateWriteStrip;
end;

procedure TfrmCoding.wrConfirmConfirm(Sender: TObject);
begin
  StartWrite;
end;

procedure TfrmCoding.wrConfirmBackup(Sender: TObject);
begin
  FBackupDone := True;
  wrConfirm.BackupDone := True;
  wrConfirm.BackupText := 'Backup saved 07:05 '#183' CODING_09_2026-10-12.json';
  if FHost <> nil then
  begin
    FHost.Log('Saved coding backup for 09 Central electrics.');
    FHost.Notify(tkSuccess, 'Backup saved',
      'CODING_09_2026-10-12.json is ready for the write.');
  end;
  UpdateWriteStrip;
end;

procedure TfrmCoding.wrConfirmCancel(Sender: TObject);
begin
  bitsCoding.OriginalCoding := FOriginalCoding;
  bitsCoding.Coding := FOriginalCoding;
  FillDiff;
  UpdateWriteStrip;
  if FHost <> nil then
    FHost.Log('Pending coding changes were cancelled.');
end;

procedure TfrmCoding.FillVehicle;
begin
  vcVehicle.Layout := vlCompact;
  vcVehicle.VIN := 'WVWZZZAUZGW123456';
  vcVehicle.Make := 'Volkswagen';
  vcVehicle.Model := 'Golf VII';
  vcVehicle.ModelYear := 2016;
  vcVehicle.Engine := '1.6 TDI (CR)';
  vcVehicle.BodyStyle := 'Hatchback';
  vcVehicle.FuelType := 'Diesel';
  vcVehicle.EngineCode := 'CRKB';
  vcVehicle.EnginePower := '81 kW';
  vcVehicle.ProtocolCompact := 'CAN 11 bit 500 kbaud';
  vcVehicle.EcuCount := 14;
  vcVehicle.Odometer := '184352 km';
  vcVehicle.MilOn := True;
  vcVehicle.ScanTimeText := 'Garage Peeters '#183' coding backup saved';
  vcVehicle.ActionCaption := '09 Central electrics';
  if FHost <> nil then
    vcVehicle.Connected := FHost.Connected
  else
    vcVehicle.Connected := False;
end;

procedure TfrmCoding.FillTabs;
var
  Tab: TOBDTabItem;
begin
  cbEcu.Items.BeginUpdate;
  try
    cbEcu.Items.Clear;
    cbEcu.Items.Add('09 Central electrics');
    cbEcu.Items.Add('01 Engine electronics');
    cbEcu.Items.Add('17 Instruments');
    cbEcu.Items.Add('19 Gateway');
    cbEcu.ItemIndex := 0;
  finally
    cbEcu.Items.EndUpdate;
  end;
  tabsCoding.Tabs.Clear;
  Tab := tabsCoding.Tabs.Add;
  Tab.Caption := 'Long coding';
  Tab.Glyph := glCoding;
  Tab := tabsCoding.Tabs.Add;
  Tab.Caption := 'Adaptations';
  Tab.Glyph := glAdaptation;
  Tab.Badge := '1';
  Tab.BadgeKind := skAccent;
  Tab := tabsCoding.Tabs.Add;
  Tab.Caption := 'Online coding';
  Tab.Glyph := glGlobe;
  Tab := tabsCoding.Tabs.Add;
  Tab.Caption := 'History';
  Tab.Glyph := glClock;
  tabsCoding.TabIndex := 0;
end;

procedure TfrmCoding.FillCoding;
var
  Coding: TBytes;
begin
  SetLength(FOriginalCoding, 16);
  FOriginalCoding[0] := $2D;
  FOriginalCoding[1] := $0B;
  FOriginalCoding[2] := $0C;
  FOriginalCoding[3] := $81;
  FOriginalCoding[4] := $40;
  FOriginalCoding[5] := $1A;
  FOriginalCoding[6] := $00;
  FOriginalCoding[7] := $0C;
  FOriginalCoding[8] := $80;
  FOriginalCoding[9] := $6C;
  FOriginalCoding[10] := $03;
  FOriginalCoding[11] := $12;
  FOriginalCoding[12] := $08;
  FOriginalCoding[13] := $00;
  FOriginalCoding[14] := $51;
  FOriginalCoding[15] := $C0;
  Coding := Copy(FOriginalCoding, 0, Length(FOriginalCoding));
  Coding[2] := $3C;
  Coding[3] := $84;
  bitsCoding.Title := 'Long coding';
  bitsCoding.Subtitle := '09 Central electrics '#183' label file 5Q0-937-08X.clb';
  bitsCoding.Clear;
  bitsCoding.OriginalCoding := FOriginalCoding;
  bitsCoding.Coding := Coding;
  bitsCoding.AddBitLabel(0, 0, 'Fog lights installed');
  bitsCoding.AddBitLabel(0, 2, 'Cornering lights');
  bitsCoding.AddBitLabel(2, 4, 'Coming home');
  bitsCoding.AddBitLabel(2, 5, 'Leaving home');
  bitsCoding.AddOptionLabel(2, 0, 2, 'Coming home source',
    'Manual switch|Light switch|Rain-light sensor|Body controller');
  bitsCoding.AddOptionLabel(3, 0, 3, 'Daytime running lights',
    'Off|Via fog lights|Via low beam|Via LED module|Scandinavian');
  bitsCoding.AddBitLabel(3, 7, 'DRL switchable in menu');
  bitsCoding.SelectedByte := 2;
end;

procedure TfrmCoding.FillDiff(AWritten: Boolean);
var
  B2, B3: Byte;
  ComingHome, LeavingHome, DrlMode: string;
begin
  B2 := bitsCoding.ByteValue(2);
  B3 := bitsCoding.ByteValue(3);
  if (B2 and $10) <> 0 then
    ComingHome := 'On'
  else
    ComingHome := 'Off';
  if (B2 and $20) <> 0 then
    LeavingHome := 'On'
  else
    LeavingHome := 'Off';
  if B3 = $84 then
    DrlMode := 'Via low beam'
  else
    DrlMode := 'Via fog lights';
  diffCoding.BeginUpdate;
  try
    diffCoding.Clear;
    diffCoding.Title := 'Changes';
    diffCoding.OnlyChanges := False;
    if AWritten then
      diffCoding.FooterText := 'Written to 09 Central electrics '#183' backup retained.'
    else
      diffCoding.FooterText := 'Backup CODING_09_2026-10-12.json '#183' write requires confirmation.';
    diffCoding.AddRow('Byte 0', 'Fog lights installed', 'Yes', 'Yes', False);
    diffCoding.AddRow('Byte 2 '#183' bit 4', 'Coming home', 'Off',
        ComingHome, (B2 and $10) <> 0);
    diffCoding.AddRow('Byte 2 '#183' bit 5', 'Leaving home', 'Off',
        LeavingHome, (B2 and $20) <> 0);
    diffCoding.AddRow('Byte 3', 'Daytime running lights', 'Via fog lights',
        DrlMode, B3 <> $81);
    diffCoding.AddRow('Byte 5', 'Rear fog light', 'Left', 'Left', False);
    diffCoding.AddRow('Byte 7', 'Comfort turn signal', '3 flashes',
      '3 flashes', False);
  finally
    diffCoding.EndUpdate;
  end;
end;

procedure TfrmCoding.FillAdaptations;
begin
  insAdaptations.BeginUpdate;
  try
    insAdaptations.Clear;
    AddInspectorRow(insAdaptations, 'Lighting', 'Coming home duration',
      '45 s (was 30)', ivText, alvWarning);
    AddInspectorRow(insAdaptations, 'Lighting', 'Leaving home duration',
      '10 s', ivReadOnly);
    AddInspectorRow(insAdaptations, 'Lighting', 'Comfort turn signal flashes',
      '3', ivReadOnly);
    AddInspectorRow(insAdaptations, 'Lighting', 'Daytime running light dimming',
      '100 %', ivReadOnly);
    AddInspectorRow(insAdaptations, 'Lighting', 'Footwell light brightness',
      '60 %', ivReadOnly);
    AddInspectorRow(insAdaptations, 'Convenience', 'Mirror dip on reverse',
      'Passenger', ivPickList);
    AddInspectorRow(insAdaptations, 'Convenience', 'Auto-lock speed',
      '15 km/h', ivReadOnly);
  finally
    insAdaptations.EndUpdate;
  end;
end;

procedure TfrmCoding.FillOnline;
begin
  insOnline.BeginUpdate;
  try
    insOnline.Clear;
    AddInspectorRow(insOnline, 'VAG SVM', 'Account signed in',
      'garage@example.eu', ivReadOnly);
    AddInspectorRow(insOnline, 'VAG SVM', 'Workshop certificate',
      'Valid to 2027-03-01', ivReadOnly);
    AddInspectorRow(insOnline, 'VAG SVM', 'Vehicle data sent',
      'Golf VII '#183' 14 control units', ivReadOnly);
    AddInspectorRow(insOnline, 'VAG SVM', 'SVM code received',
      'Waiting for OEM response', ivReadOnly, alvWarning);
    AddInspectorRow(insOnline, 'VAG SVM', 'Write SVM coding',
      'After download', ivReadOnly);
  finally
    insOnline.EndUpdate;
  end;
end;

procedure TfrmCoding.UpdateWriteStrip;
var
  Count: Integer;
begin
  Count := ChangedBits;
  wrConfirm.BackupDone := FBackupDone;
  wrConfirm.Enabled := Count > 0;
  if Count > 0 then
    wrConfirm.Title := Format('Write %d coding changes to 09 Central electrics?',
      [Count])
  else
    wrConfirm.Title := 'No pending coding changes';
  wrConfirm.BackupRequiredTitle := 'Make a backup before writing';
  wrConfirm.BackupText := 'Backup saved 07:05 '#183' CODING_09_2026-10-12.json';
  wrConfirm.BackupRequiredText := 'Writing is enabled once current coding is saved.';
  wrConfirm.ConfirmCaption := 'Write coding';
  wrConfirm.BackupCaption := 'Make backup';
  wrConfirm.CancelCaption := 'Cancel';
end;

procedure TfrmCoding.StartWrite;
begin
  if FWriteActive or (ChangedBits = 0) then
    Exit;
  if (FHost <> nil) and not FHost.Connected then
  begin
    FHost.Notify(tkWarning, 'Adapter offline',
      'Connect to the Golf VII before writing long coding.');
    Exit;
  end;
  if (FHost <> nil) and not FHost.Confirm(dkWarning, 'Write coding',
    'Write long-coding changes to 09 Central electrics? A backup has been saved.',
    'Write coding') then
    Exit;
  FWriteActive := True;
  FWriteTick := 0;
  wrConfirm.Enabled := False;
  if FHost <> nil then
  begin
    FHost.Log('Writing 09 Central electrics long coding.');
    FHost.SetProgress('Writing 09 Central electrics coding', 0);
  end;
end;

procedure TfrmCoding.FinishWrite;
begin
  FWriteActive := False;
  FOriginalCoding := Copy(bitsCoding.Coding, 0, Length(bitsCoding.Coding));
  bitsCoding.OriginalCoding := FOriginalCoding;
  FillDiff(True);
  UpdateWriteStrip;
  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.Log('Coding write completed for 09 Central electrics.');
    FHost.Notify(tkSuccess, 'Coding written',
      '09 Central electrics accepted the long-coding changes.');
  end;
end;

function TfrmCoding.ChangedBits: Integer;
begin
  Result := bitsCoding.ChangedCount;
end;

end.
