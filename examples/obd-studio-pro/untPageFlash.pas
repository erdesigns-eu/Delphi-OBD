//------------------------------------------------------------------------------
//  untPageFlash
//
//  Flash programming page for OBD Studio Pro: file applicability,
//  voltage gate, guarded progress, session log and audit timeline.
//
//    - TOBDCard, TOBDPreconditionList, TOBDProgressBar, TOBDRangeBar,
//      TOBDValueTile, TOBDBanner, TOBDButton, TOBDLogViewer and
//      TOBDTimeline compose a realistic ECU flash workflow.
//    - Tick advances a simulated 01 Engine electronics flash using the
//      shared Golf VII 1.6 TDI simulator and keeps host progress alive.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageFlash;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.Chips, ERD.UI.Progress,
  ERD.UI.RangeBar, ERD.UI.ValueTile, ERD.UI.PreconditionList,
  ERD.UI.Timeline, ERD.UI.LogViewer,
  untProPage, untSimulator;

type
  TfrmFlash = class(TFrame, IProPage)
    pnlLeft: TPanel;
    cdFlashImage: TOBDCard;
    lstFlashChecks: TOBDPreconditionList;
    preFlashReady: TOBDPreconditionList;
    tlFlash: TOBDTimeline;
    pnlMain: TPanel;
    cdFlash: TOBDCard;
    pbFlashSteps: TOBDProgressBar;
    pbFlashTransfer: TOBDProgressBar;
    pnlMetrics: TPanel;
    tileBlock: TOBDValueTile;
    tileSpeed: TOBDValueTile;
    tileRemaining: TOBDValueTile;
    tileRetries: TOBDValueTile;
    rbrVoltage: TOBDRangeBar;
    bnrIgnition: TOBDBanner;
    lvFlash: TOBDLogViewer;
    pnlFlashButtons: TPanel;
    btnAbortFlash: TOBDButton;
    btnStartFlash: TOBDButton;
    procedure btnStartFlashClick(Sender: TObject);
    procedure btnAbortFlashClick(Sender: TObject);
  private
    FHost: IProHost;
    FFlashActive: Boolean;
    FFlashTicks: Integer;
    FLastPhase: Integer;
    procedure AddFlashLog(const AText: string);
    procedure FinishFlash;
    procedure ResetFlash;
    procedure SetStepStates(APercent: Integer);
    procedure UpdateLiveVoltage;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

const
  FLASH_TOTAL_TICKS = 200;
  FLASH_BLOCK_COUNT = 664;

procedure TfrmFlash.AddFlashLog(const AText: string);
begin
  lvFlash.Info(AText);
  if FHost <> nil then
    FHost.Log(AText);
end;

procedure TfrmFlash.btnAbortFlashClick(Sender: TObject);
begin
  if not FFlashActive then
    Exit;

  if (FHost <> nil) and not FHost.Confirm(dkWarning, 'Abort flash',
    'Abort the simulated programming session? The real ECU would need a resume from the last checkpoint.',
    'Abort') then
    Exit;

  FFlashActive := False;
  btnStartFlash.Enabled := True;
  btnAbortFlash.Enabled := False;
  pbFlashTransfer.State := pbsWarning;
  AddFlashLog('Flash aborted by the operator at the last checkpoint.');
  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.Notify(tkWarning, 'Flash aborted',
      'The simulated session stopped at the saved checkpoint.');
  end;
end;

procedure TfrmFlash.btnStartFlashClick(Sender: TObject);
begin
  if FFlashActive then
    Exit;

  if (FHost <> nil) and not FHost.Connected then
  begin
    FHost.Notify(tkWarning, 'Adapter offline',
      'Connect to the simulated Golf before starting the flash.');
    Exit;
  end;

  if (FHost <> nil) and not FHost.Confirm(dkDanger, 'Start ECU flash',
    'Program 04L906056KJ_0012.frf to 01 Engine electronics? Keep a charger connected and do not switch off ignition.',
    'Start flash') then
    Exit;

  ResetFlash;
  FFlashActive := True;
  btnStartFlash.Enabled := False;
  btnAbortFlash.Enabled := True;
  AddFlashLog('Programming session requested for 01 Engine electronics.');
  AddFlashLog('Security access level 0x11 granted for UDS on CAN 0x7E0.');
  if FHost <> nil then
    FHost.SetProgress('Flashing 01 Engine electronics', 0);
end;

procedure TfrmFlash.ConnectionChanged;
begin
  UpdateLiveVoltage;
  btnStartFlash.Enabled := (not FFlashActive) and (FHost <> nil) and
    FHost.Connected;
end;

procedure TfrmFlash.FinishFlash;
var
  Entry: TOBDTimelineEntry;
begin
  FFlashActive := False;
  pbFlashTransfer.Position := 100;
  pbFlashTransfer.Caption := 'Transfer complete';
  pbFlashTransfer.State := pbsSuccess;
  SetStepStates(100);
  btnStartFlash.Enabled := (FHost <> nil) and FHost.Connected;
  btnAbortFlash.Enabled := False;
  tileBlock.Value := FLASH_BLOCK_COUNT;
  tileSpeed.Value := 0;
  tileRemaining.Value := 0;
  AddFlashLog('Verification finished, ECU reset accepted.');

  Entry := tlFlash.AddEntry(FormatDateTime('hh:nn', Now), otlFlash,
    'Flash 04L906056KJ completed',
    '01 Engine electronics updated from 9978 to 0012 for WVWZZZAUZGW123456.',
    'Garage Peeters', skSuccess);
  Entry.HasDetails := True;

  if FHost <> nil then
  begin
    FHost.SetProgress('', -1);
    FHost.Notify(tkSuccess, 'Flash complete',
      '01 Engine electronics accepted the new software and passed verification.');
  end;
end;

procedure TfrmFlash.Init(const AHost: IProHost);
begin
  FHost := AHost;
  ResetFlash;
  lvFlash.ClearLog;
  AddFlashLog('Loaded 04L906056KJ_0012.frf from the VAG flash catalogue.');
  AddFlashLog('Target: 2016 VW Golf VII 1.6 TDI CR, VIN WVWZZZAUZGW123456.');
  ConnectionChanged;
end;

procedure TfrmFlash.ResetFlash;
begin
  FFlashActive := False;
  FFlashTicks := 0;
  FLastPhase := -1;
  pbFlashTransfer.Position := 0;
  pbFlashTransfer.Caption := 'Ready to transfer';
  pbFlashTransfer.State := pbsNormal;
  tileBlock.Value := 0;
  tileSpeed.Value := 0;
  tileRemaining.Value := 0;
  tileRetries.Value := 0;
  SetStepStates(0);
  btnAbortFlash.Enabled := False;
  UpdateLiveVoltage;
end;

procedure TfrmFlash.SetStepStates(APercent: Integer);
var
  I: Integer;
  Phase: Integer;
begin
  if APercent >= 96 then
    Phase := 5
  else if APercent >= 80 then
    Phase := 4
  else if APercent >= 55 then
    Phase := 3
  else if APercent >= 20 then
    Phase := 2
  else if APercent > 0 then
    Phase := 1
  else
    Phase := 0;

  for I := 0 to pbFlashSteps.Steps.Count - 1 do
  begin
    if I < Phase then
      pbFlashSteps.Steps.Items[I].State := stDone
    else if I = Phase then
      pbFlashSteps.Steps.Items[I].State := stActive
    else
      pbFlashSteps.Steps.Items[I].State := stPending;
  end;

  if Phase <> FLastPhase then
  begin
    FLastPhase := Phase;
    case Phase of
      1: AddFlashLog('Fingerprint written: WSC 01234, importer 790.');
      2: AddFlashLog('Erase memory 0x00080000 completed in 4.1 s.');
      3: AddFlashLog('Checkpoint saved at block 400.');
      4: AddFlashLog('Transfer finished, verifying block checksums.');
      5: AddFlashLog('Resetting ECU and leaving programming session.');
    end;
  end;
end;

procedure TfrmFlash.Tick;
var
  Blocks: Integer;
  Percent: Integer;
  Remaining: Integer;
  Speed: Double;
begin
  UpdateLiveVoltage;
  if not FFlashActive then
    Exit;

  Inc(FFlashTicks);
  Percent := EnsureRange(Round(FFlashTicks * 100 / FLASH_TOTAL_TICKS), 0, 100);
  Blocks := EnsureRange(Round(FLASH_BLOCK_COUNT * Percent / 100), 0,
    FLASH_BLOCK_COUNT);
  Remaining := Max(0, (FLASH_TOTAL_TICKS - FFlashTicks) div 10);
  Speed := 36.0 + 3.5 * Sin(FFlashTicks / 8);

  pbFlashTransfer.Position := Percent;
  pbFlashTransfer.Caption := Format('Block %d of %d', [Blocks,
    FLASH_BLOCK_COUNT]);
  tileBlock.Value := Blocks;
  tileSpeed.Value := Speed;
  tileRemaining.Value := Remaining;
  SetStepStates(Percent);

  if FHost <> nil then
    FHost.SetProgress('Flashing 01 Engine electronics', Percent);

  if FFlashTicks >= FLASH_TOTAL_TICKS then
    FinishFlash;
end;

procedure TfrmFlash.UpdateLiveVoltage;
var
  Voltage: Double;
begin
  Voltage := 12.4;
  if (FHost <> nil) and FHost.Connected then
    Voltage := FHost.Simulator.Value(PID_MODULE_VOLTAGE);
  rbrVoltage.Value := Voltage;
  if preFlashReady.Items.Count > 1 then
  begin
    preFlashReady.Items[1].ValueText := Format('%.1f V', [Voltage]);
    if Voltage >= 12.5 then
      preFlashReady.Items[1].State := pcnMet
    else
      preFlashReady.Items[1].State := pcnFailed;
  end;
end;

end.
