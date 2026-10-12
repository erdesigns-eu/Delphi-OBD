//------------------------------------------------------------------------------
//  untPageSecurity
//
//  Immobilizer and component-protection page for OBD Studio Pro: security
//  access, key learning slots, guided procedure and protected ECU list.
//
//    - TOBDSecurityAccessCard, TOBDProcedureView, TOBDKeySlots and
//      TOBDPreconditionList drive the immobilizer workflow.
//    - TOBDCard, TOBDDataGrid and TOBDButton show component protection
//      status for the Golf VII sample job.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageSecurity;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.PreconditionList,
  ERD.UI.ProcedureView, ERD.UI.SecurityAccessCard, ERD.UI.KeySlots,
  ERD.UI.DataGrid,
  untProPage, untSimulator;

type
  TfrmSecurity = class(TFrame, IProPage)
    pnlSecurityLeft: TPanel;
    procKeyLearn: TOBDProcedureView;
    cdKeySlots: TOBDCard;
    slotsKeys: TOBDKeySlots;
    btnStartLearning: TOBDButton;
    btnCancelLearning: TOBDButton;
    pnlSecurityRight: TPanel;
    secAccess: TOBDSecurityAccessCard;
    preKeyLearning: TOBDPreconditionList;
    cdProtection: TOBDCard;
    grdProtection: TOBDDataGrid;
    btnRefreshProtection: TOBDButton;
    procedure btnStartLearningClick(Sender: TObject);
    procedure btnCancelLearningClick(Sender: TObject);
    procedure btnRefreshProtectionClick(Sender: TObject);
    procedure procKeyLearnAbort(Sender: TObject);
    procedure procKeyLearnAction(Sender: TObject);
    procedure procKeyLearnNext(Sender: TObject);
    procedure slotsKeysSlotClick(Sender: TObject; AIndex: Integer);
  private
    FHost: IProHost;
    FLearning: Boolean;
    FLearnTicks: Integer;
    procedure BeginLearning;
    procedure EndLearning(ASuccess: Boolean);
    procedure FillProtectionList;
    procedure UpdateAccessCard;
    procedure UpdateLearningState;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

const
  LEARN_TOTAL_TICKS = 110;

procedure TfrmSecurity.BeginLearning;
begin
  if FLearning then
    Exit;

  if (FHost <> nil) and not FHost.Confirm(dkDanger, 'Learn immobilizer key',
    'Start key learning for key 3? Keys not learned in this session stop working on a real vehicle.',
    'Learn key') then
    Exit;

  FLearning := True;
  FLearnTicks := 0;
  procKeyLearn.CurrentStep := 0;
  btnStartLearning.Enabled := False;
  btnCancelLearning.Enabled := True;
  slotsKeys.Slots.Items[2].State := kssLearning;
  slotsKeys.Slots.Items[2].KeyId := 'Hold to marker';
  if FHost <> nil then
  begin
    FHost.Log('Key learning started for 17 Instruments / immobilizer 5.');
    FHost.SetProgress('Learning key 3', 0);
  end;
end;

procedure TfrmSecurity.btnCancelLearningClick(Sender: TObject);
begin
  if FLearning then
    EndLearning(False);
end;

procedure TfrmSecurity.btnRefreshProtectionClick(Sender: TObject);
begin
  FillProtectionList;
  if FHost <> nil then
    FHost.Notify(tkInfo, 'Component protection refreshed',
      'One protected infotainment unit remains in the sample vehicle.');
end;

procedure TfrmSecurity.btnStartLearningClick(Sender: TObject);
begin
  BeginLearning;
end;

procedure TfrmSecurity.ConnectionChanged;
begin
  UpdateAccessCard;
  btnStartLearning.Enabled := not FLearning;
end;

procedure TfrmSecurity.EndLearning(ASuccess: Boolean);
begin
  FLearning := False;
  btnStartLearning.Enabled := True;
  btnCancelLearning.Enabled := False;
  if ASuccess then
  begin
    procKeyLearn.CurrentStep := 4;
    slotsKeys.Slots.Items[2].State := kssThisKey;
    slotsKeys.Slots.Items[2].KeyId := 'Learned ID48';
    procKeyLearn.LiveValues.Items[0].ValueText := '3 of 3';
    procKeyLearn.LiveValues.Items[0].State := opvSuccess;
    procKeyLearn.LiveValues.Items[2].ValueText := 'Complete';
    procKeyLearn.LiveValues.Items[2].State := opvSuccess;
    if FHost <> nil then
    begin
      FHost.SetProgress('', -1);
      FHost.Log('Key 3 learned successfully; three keys are now adapted.');
      FHost.Notify(tkSuccess, 'Key learned',
        'Key 3 was added to the immobilizer key table.');
    end;
  end
  else
  begin
    slotsKeys.Slots.Items[2].State := kssEmpty;
    slotsKeys.Slots.Items[2].KeyId := 'Empty';
    procKeyLearn.LiveValues.Items[2].ValueText := 'Cancelled';
    procKeyLearn.LiveValues.Items[2].State := opvWarning;
    if FHost <> nil then
    begin
      FHost.SetProgress('', -1);
      FHost.LogWarning('Key learning cancelled by the operator.');
      FHost.Notify(tkWarning, 'Learning cancelled',
        'The simulated procedure stopped before changing the key table.');
    end;
  end;
end;

procedure TfrmSecurity.FillProtectionList;
var
  Row: Integer;
begin
  grdProtection.BeginUpdate;
  try
    grdProtection.Clear;
    Row := grdProtection.AddRow(['19', 'Gateway', '5Q0 907 530 AQ',
      'Not active']);
    grdProtection.CellTone[3, Row] := skSuccess;
    Row := grdProtection.AddRow(['17', 'Instruments', '5G1 920 640 B',
      'Not active']);
    grdProtection.CellTone[3, Row] := skSuccess;
    Row := grdProtection.AddRow(['5F', 'Infotainment', '5G0 035 020 E',
      'Protected']);
    grdProtection.CellTone[3, Row] := skDanger;
    grdProtection.RowAccent[Row] := True;
    Row := grdProtection.AddRow(['46', 'Comfort system', '5Q0 959 435 C',
      'Not active']);
    grdProtection.CellTone[3, Row] := skSuccess;
    Row := grdProtection.AddRow(['15', 'Airbag', '5Q0 959 655 BJ',
      'Not active']);
    grdProtection.CellTone[3, Row] := skSuccess;
    Row := grdProtection.AddRow(['44', 'Steering assistance',
      '5Q1 909 144 R', 'Not active']);
    grdProtection.CellTone[3, Row] := skSuccess;
  finally
    grdProtection.EndUpdate;
  end;
end;

procedure TfrmSecurity.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FLearning := False;
  FLearnTicks := 0;
  slotsKeys.ItemIndex := 2;
  btnCancelLearning.Enabled := False;
  FillProtectionList;
  UpdateAccessCard;
  UpdateLearningState;
end;

procedure TfrmSecurity.procKeyLearnAbort(Sender: TObject);
begin
  if FLearning then
    EndLearning(False);
end;

procedure TfrmSecurity.procKeyLearnAction(Sender: TObject);
begin
  BeginLearning;
end;

procedure TfrmSecurity.procKeyLearnNext(Sender: TObject);
begin
  if procKeyLearn.CurrentStep < 4 then
    procKeyLearn.CurrentStep := procKeyLearn.CurrentStep + 1;
end;

procedure TfrmSecurity.slotsKeysSlotClick(Sender: TObject; AIndex: Integer);
begin
  if FHost <> nil then
    FHost.Log(Format('Key slot %d selected.', [AIndex + 1]));
end;

procedure TfrmSecurity.Tick;
var
  Percent: Integer;
begin
  UpdateAccessCard;
  if not FLearning then
    Exit;

  Inc(FLearnTicks);
  Percent := EnsureRange(Round(FLearnTicks * 100 / LEARN_TOTAL_TICKS), 0, 100);
  if FHost <> nil then
    FHost.SetProgress('Learning key 3', Percent);
  UpdateLearningState;

  if FLearnTicks >= LEARN_TOTAL_TICKS then
    EndLearning(True);
end;

procedure TfrmSecurity.UpdateAccessCard;
var
  KeepAlive: Double;
begin
  if (FHost <> nil) and FHost.Connected then
  begin
    secAccess.State := sacUnlocked;
    secAccess.ControlUnit := '17 Instruments';
    secAccess.SessionText := 'Extended (0x03)';
    secAccess.LevelText := 'Immobilizer login / PIN';
    KeepAlive := 0.5 + 0.5 * Sin(FHost.Simulator.Time * 2);
    secAccess.KeepAliveText := 'Tester present / 2 s';
    secAccess.KeepAliveProgress := KeepAlive;
    if preKeyLearning.Items.Count > 2 then
      preKeyLearning.Items[2].ValueText := Format('%.1f V',
        [FHost.Simulator.Value(PID_MODULE_VOLTAGE)]);
  end
  else
  begin
    secAccess.State := sacLocked;
    secAccess.ControlUnit := '17 Instruments';
    secAccess.SessionText := 'Offline';
    secAccess.LevelText := 'Locked';
    secAccess.KeepAliveText := 'No tester present';
    secAccess.KeepAliveProgress := 0;
  end;
end;

procedure TfrmSecurity.UpdateLearningState;
var
  Remaining: Integer;
begin
  Remaining := Max(0, (LEARN_TOTAL_TICKS - FLearnTicks) div 10);
  procKeyLearn.LiveValues.Items[0].ValueText := '2 of 3';
  procKeyLearn.LiveValues.Items[1].ValueText := 'Detected / ID48';
  procKeyLearn.LiveValues.Items[2].ValueText := Format('00:%2.2d', [Remaining]);
  procKeyLearn.LiveValues.Items[2].State := opvAccent;
  procKeyLearn.LiveValues.Items[2].Fraction := Remaining / 11;

  if not FLearning then
    Exit;

  if FLearnTicks < 20 then
    procKeyLearn.CurrentStep := 1
  else if FLearnTicks < 42 then
    procKeyLearn.CurrentStep := 2
  else if FLearnTicks < 88 then
    procKeyLearn.CurrentStep := 3
  else
    procKeyLearn.CurrentStep := 4;
end;

end.
