//------------------------------------------------------------------------------
//  untPageRadio
//
//  Radio-code page for OBD Studio Pro: Volkswagen serial entry, EEPROM
//  extraction evidence and a host-supplied licensed calculator hook.
//
//    - TOBDComboBox, TOBDSegmented, TOBDEdit, TOBDButton and TOBDBanner
//      make the serial-number workflow.
//    - TOBDHexView shows the supporting 24C32 bytes. TOBDRadioCodeVW is
//      used for validation while the example host supplies the algorithm.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageRadio;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.Edits, ERD.UI.Segmented,
  ERD.UI.Chips, ERD.UI.HexView, ERD.UI.DataGrid,
  ERD.RadioCode.Types, ERD.RadioCode, ERD.RadioCode.EuropeanPremium,
  untProPage, untSimulator;

type
  TfrmRadio = class(TFrame, IProPage)
    pnlRadio: TPanel;
    cdRadio: TOBDCard;
    segRadioMethod: TOBDSegmented;
    cbRadioBrand: TOBDComboBox;
    cbRadioUnit: TOBDComboBox;
    edRadioSerial: TOBDEdit;
    chpRadioStatus: TOBDChip;
    edCode1: TOBDEdit;
    edCode2: TOBDEdit;
    edCode3: TOBDEdit;
    edCode4: TOBDEdit;
    btnCalcRadio: TOBDButton;
    btnCopyRadio: TOBDButton;
    btnSaveRadio: TOBDButton;
    bnrRadio: TOBDBanner;
    pnlRadioRight: TPanel;
    hexRadio: TOBDHexView;
    cdRadioEvidence: TOBDCard;
    grdRadioEvidence: TOBDDataGrid;
    procedure btnCalcRadioClick(Sender: TObject);
    procedure btnCopyRadioClick(Sender: TObject);
    procedure btnSaveRadioClick(Sender: TObject);
    procedure edRadioSerialChange(Sender: TObject);
  private
    FHost: IProHost;
    FCalculator: TOBDRadioCodeVW;
    FLastCode: string;
    function BuildRadioDump: TBytes;
    procedure HostCalculate(Sender: TObject; const AInput: string;
      const AContext: TOBDRadioCodeContext; var AResult: TOBDRadioCodeResult);
    procedure SetCode(const ACode: string);
    procedure SetEvidence(const AText: string);
    procedure UpdateValidation;
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

function TfrmRadio.BuildRadioDump: TBytes;
var
  I: Integer;
  V: Int64;
  Serial: AnsiString;
begin
  SetLength(Result, 256);
  V := 7;
  for I := 0 to High(Result) do
  begin
    V := (V * 1103515245 + 12345) and $7FFFFFFF;
    Result[I] := Byte((V shr 16) and $FF);
  end;
  Serial := 'VWZ1Z2N1234567';
  for I := 1 to Length(Serial) do
    Result[$0010 + I - 1] := Ord(Serial[I]);
  Result[$0038] := $04;
  Result[$0039] := $01;
  Result[$003A] := $07;
  Result[$003B] := $02;
end;

procedure TfrmRadio.btnCalcRadioClick(Sender: TObject);
var
  CalcResult: TOBDRadioCodeResult;
begin
  CalcResult := FCalculator.Calculate(edRadioSerial.Text);
  if CalcResult.Success then
  begin
    SetCode(CalcResult.Code);
    SetEvidence('Licensed host algorithm returned code ' + CalcResult.Code +
      ' for ' + edRadioSerial.Text + '.');
    if FHost <> nil then
    begin
      FHost.Log('Radio SAFE code calculated for serial ' + edRadioSerial.Text +
        ' using host-supplied VW algorithm.');
      FHost.Notify(tkSuccess, 'Radio code found',
        'Code ' + CalcResult.Code + ' is ready for the RCD 330 unit.');
    end;
  end
  else
  begin
    SetCode('');
    SetEvidence('Validation failed: ' + CalcResult.Message);
    if FHost <> nil then
      FHost.Notify(tkWarning, 'Radio code unavailable', CalcResult.Message);
  end;
end;

procedure TfrmRadio.btnCopyRadioClick(Sender: TObject);
begin
  if FLastCode = '' then
    btnCalcRadioClick(Sender)
  else if FHost <> nil then
    FHost.Notify(tkInfo, 'Code copied',
      'Code ' + FLastCode + ' would be copied to the clipboard by the host.');
end;

procedure TfrmRadio.btnSaveRadioClick(Sender: TObject);
begin
  if FLastCode = '' then
    btnCalcRadioClick(Sender)
  else if FHost <> nil then
  begin
    FHost.Log('Radio code ' + FLastCode + ' saved to job Garage Peeters.');
    FHost.Notify(tkSuccess, 'Saved to job',
      'The code and EEPROM evidence were added to the customer job.');
  end;
end;

procedure TfrmRadio.ConnectionChanged;
begin
end;

procedure TfrmRadio.edRadioSerialChange(Sender: TObject);
begin
  UpdateValidation;
end;

procedure TfrmRadio.HostCalculate(Sender: TObject; const AInput: string;
  const AContext: TOBDRadioCodeContext; var AResult: TOBDRadioCodeResult);
begin
  AResult.Success := True;
  AResult.Code := '4172';
  AResult.Variant := 'Garage Peeters licensed VW RCD 330 provider';
  AResult.Message := '';
end;

procedure TfrmRadio.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FCalculator := TOBDRadioCodeVW.Create(Self);
  FCalculator.VIN := 'WVWZZZAUZGW123456';
  FCalculator.ModelHint := 'RCD 330 Delphi';
  FCalculator.OnCalculate := HostCalculate;
  hexRadio.SetData(BuildRadioDump);
  hexRadio.AddRegion('VWZ serial', $0010, 14, hvrNeutral);
  hexRadio.AddRegion('SAFE code bytes', $0038, 4, hvrAccent);
  SetCode('4172');
  SetEvidence('Code bytes 0x0038..0x003B: 04 01 07 02 -> 4172.');
  UpdateValidation;
end;

procedure TfrmRadio.SetCode(const ACode: string);
begin
  FLastCode := ACode;
  if Length(ACode) = 4 then
  begin
    edCode1.Text := Copy(ACode, 1, 1);
    edCode2.Text := Copy(ACode, 2, 1);
    edCode3.Text := Copy(ACode, 3, 1);
    edCode4.Text := Copy(ACode, 4, 1);
    chpRadioStatus.Caption := 'CODE FOUND';
    chpRadioStatus.Kind := skSuccess;
  end
  else
  begin
    edCode1.Text := '-';
    edCode2.Text := '-';
    edCode3.Text := '-';
    edCode4.Text := '-';
    chpRadioStatus.Caption := 'WAITING';
    chpRadioStatus.Kind := skWarning;
  end;
end;

procedure TfrmRadio.SetEvidence(const AText: string);
var
  Row: Integer;
  ResultTone: TOBDStatusKind;
begin
  if Pos('Validation failed:', AText) = 1 then
    ResultTone := skWarning
  else
    ResultTone := skSuccess;
  grdRadioEvidence.BeginUpdate;
  try
    grdRadioEvidence.Clear;
    Row := grdRadioEvidence.AddRow(['Brand', 'Volkswagen', 'OK']);
    grdRadioEvidence.CellTone[2, Row] := skSuccess;
    Row := grdRadioEvidence.AddRow(['Unit', 'RCD 330 (Delphi)', 'OK']);
    grdRadioEvidence.CellTone[2, Row] := skSuccess;
    Row := grdRadioEvidence.AddRow(['Vehicle VIN', 'WVWZZZAUZGW123456', 'OK']);
    grdRadioEvidence.CellTone[2, Row] := skSuccess;
    Row := grdRadioEvidence.AddRow(['Serial', edRadioSerial.Text, 'OK']);
    grdRadioEvidence.CellTone[2, Row] := skSuccess;
    Row := grdRadioEvidence.AddRow(['EEPROM',
      '24C32, bytes 0x0038..0x003B', 'OK']);
    grdRadioEvidence.CellTone[2, Row] := skSuccess;
    Row := grdRadioEvidence.AddRow(['Note',
      'Algorithms are licensed and supplied by the host.', 'Info']);
    grdRadioEvidence.CellTone[2, Row] := skNeutral;
    Row := grdRadioEvidence.AddRow(['Result', AText, 'State']);
    grdRadioEvidence.CellTone[2, Row] := ResultTone;
  finally
    grdRadioEvidence.EndUpdate;
  end;
end;

procedure TfrmRadio.Tick;
begin
end;

procedure TfrmRadio.UpdateValidation;
var
  Reason: string;
begin
  if FCalculator = nil then
    Exit;

  if FCalculator.Validate(edRadioSerial.Text, Reason) then
  begin
    chpRadioStatus.Caption := 'FORMAT OK';
    chpRadioStatus.Kind := skSuccess;
    bnrRadio.Kind := bnInfo;
    bnrRadio.Title := 'Entering the code';
    bnrRadio.Text := 'Preset keys 1-4 select each digit; hold OK for 2 s. Algorithms are supplied by the host.';
  end
  else
  begin
    chpRadioStatus.Caption := 'CHECK SERIAL';
    chpRadioStatus.Kind := skWarning;
    bnrRadio.Kind := bnWarning;
    bnrRadio.Title := 'Serial number required';
    bnrRadio.Text := Reason;
  end;
end;

end.
