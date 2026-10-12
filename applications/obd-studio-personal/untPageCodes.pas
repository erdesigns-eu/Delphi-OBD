//------------------------------------------------------------------------------
//  untPageCodes
//
//  Trouble-code page of OBD-Studio Personal. Reads classic OBD-II stored,
//  pending and permanent DTCs, the related freeze frame and the optional
//  UDS / WWH-OBD DTC surfaces, then renders a local report.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untPageCodes;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.IOUtils,
  System.Math,
  System.NetEncoding,
  System.StrUtils,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  ERD.I18n,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Card,
  ERD.UI.Buttons,
  ERD.UI.Chips,
  ERD.UI.Segmented,
  ERD.UI.ScrollBox,
  ERD.UI.DtcPanel,
  ERD.UI.DataGrid,
  ERD.UI.KeyValueList,
  ERD.UI.FreezeFrameView,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.Service.DTCs,
  ERD.Service.LiveData,
  ERD.Service.Catalog,
  ERD.Service.DriveCycle.Types,
  ERD.WWHOBD,
  ERD.OEM,
  ERD.OEM.Types,
  ERD.OEM.DTC,
  untPersonalPage;

type
  TFreezeValue = record
    PID: Byte;
    Caption: string;
    Value: Double;
    UnitText: string;
    RawText: string;
    HasNumber: Boolean;
  end;

  TCodeRow = record
    Code: string;
    Description: string;
    Severity: string;
    Source: string;
    Ecu: string;
    SystemName: string;
    Status: TOBDDtcStatus;
    FreezeValues: TArray<string>;
  end;

  TUDSRow = record
    Code: string;
    PlainCode: string;
    Description: string;
    Source: string;
    Ecu: string;
    StatusText: string;
    FailureType: string;
    ReadinessGroup: string;
    WWHClass: string;
    Permanent: Boolean;
  end;

  TfrmPageCodes = class(TFrame, IPersonalPage)
    scMain: TOBDScrollBox;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    lblSubtitle: TLabel;
    segView: TOBDSegmented;
    btnSave: TOBDButton;
    btnClear: TOBDButton;
    btnRead: TOBDButton;
    banOffline: TOBDBanner;
    pnlClassic: TPanel;
    pnlClassicLeft: TPanel;
    dtcPanel: TOBDDtcPanel;
    cardDetails: TOBDCard;
    btnWeb: TOBDButton;
    kvDetails: TOBDKeyValueList;
    pnlFreeze: TPanel;
    freezeView: TOBDFreezeFrameView;
    pnlUDS: TPanel;
    gridUDS: TOBDDataGrid;
    banUDS: TOBDBanner;
    cardUDSDetails: TOBDCard;
    kvUDS: TOBDKeyValueList;
    procedure btnReadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnWebClick(Sender: TObject);
    procedure segViewChange(Sender: TObject);
    procedure dtcPanelReadCodes(Sender: TObject);
    procedure dtcPanelClearCodes(Sender: TObject);
    procedure dtcPanelSelect(Sender: TObject; AIndex: Integer);
    procedure gridUDSChange(Sender: TObject);
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FReading: Boolean;
    FClassicRows: TArray<TCodeRow>;
    FUDSRows: TArray<TUDSRow>;
    FFreezeValues: TArray<TFreezeValue>;
    FFreezeDtc: string;
    FSelectedClassic: Integer;
    FSelectedUDS: Integer;
    FLastRead: TDateTime;
    FLastMIL: Boolean;
    FLastStoredCount: Integer;
    procedure ResetPage;
    procedure UpdateConnectedState;
    procedure SetView(AIndex: Integer);
    procedure ReadClassicCodes;
    procedure ReadUDSCodes;
    procedure ClearCodes;
    procedure ApplyClassicRows;
    procedure ApplyUDSRows(const AMessage: string; AKind: TOBDBannerKind);
    procedure AttachFreezeToRows;
    procedure UpdateClassicDetails;
    procedure UpdateUDSDetails;
    procedure RenderFreezeView;
    procedure ReadFreezeFrameValues(out AValues: TArray<TFreezeValue>;
      out ADtc: string);
    procedure AppendClassicRow(var ARows: TArray<TCodeRow>;
      const AEntry: TOBDDtcEntry; AStatus: TOBDDtcStatus);
    procedure AppendUDSRow(var ARows: TArray<TUDSRow>;
      const AEntry: TOBDDtcEntry);
    procedure AppendWWHRow(var ARows: TArray<TUDSRow>;
      const AEntry: TOBDWWHDtcEntry; APermanent: Boolean);
    procedure LookupDtcText(const ACode, AFallback: string;
      out ADescription, ASeverity, ASource: string);
    function CurrentFreezeLines: TArray<string>;
    function FormatFreezeValue(const AValue: TFreezeValue): string;
    function ConvertedValue(AValue: Double; const AUnit: string;
      out AConvertedUnit: string): Double;
    function SelectedCode: string;
    function BuildTextReport: string;
    function BuildHtmlReport: string;
    function ReportFileBase: string;
  public
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
  end;

implementation

{$R *.dfm}

const
  FREEZE_PIDS: array [0 .. 8] of Byte = ($02, $04, $05, $0B, $0C, $0D, $0F,
    $10, $11);

function HtmlEscape(const AText: string): string;
begin
  Result := StringReplace(AText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;

function BytesToText(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ABytes) do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(ABytes[I], 2);
  end;
end;

function DtcStatusText(AStatus: TOBDDtcStatus): string;
begin
  case AStatus of
    dsPending:
      Result := Tr('personal.codes.statusPending', 'Pending');
    dsPermanent:
      Result := Tr('personal.codes.statusPermanent', 'Permanent');
  else
    Result := Tr('personal.codes.statusStored', 'Stored');
  end;
end;

function DtcStatusTone(AStatus: TOBDDtcStatus): TOBDStatusKind;
begin
  case AStatus of
    dsPending:
      Result := skWarning;
    dsPermanent:
      Result := skAccent;
  else
    Result := skDanger;
  end;
end;

function WWHClassText(AClass: TOBDWWHDtcClass): string;
begin
  case AClass of
    wcMaintenance:
      Result := 'maintenance';
    wcCheckHalt:
      Result := 'check at next halt';
    wcCheckNow:
      Result := 'check immediately';
    wcCheckBoth:
      Result := 'check soon / now';
    wcClass0:
      Result := '0';
    wcClassA:
      Result := 'A';
    wcClassB1:
      Result := 'B1';
    wcClassB2:
      Result := 'B2';
    wcClassC:
      Result := 'C';
  else
    Result := '';
  end;
end;

function UDSStatusText(AStatus: Byte): string;

  procedure Add(const AText: string);
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + AText;
  end;

begin
  Result := '';
  if (AStatus and $01) <> 0 then
    Add(Tr('personal.codes.udsTestFailed', 'test failed'));
  if (AStatus and $02) <> 0 then
    Add(Tr('personal.codes.udsThisCycle', 'this cycle'));
  if (AStatus and $04) <> 0 then
    Add(Tr('personal.codes.udsPending', 'pending'));
  if (AStatus and $08) <> 0 then
    Add(Tr('personal.codes.udsConfirmed', 'confirmed'));
  if (AStatus and $10) <> 0 then
    Add(Tr('personal.codes.udsSinceClear', 'failed since clear'));
  if (AStatus and $20) <> 0 then
    Add(Tr('personal.codes.udsNotComplete', 'test not complete'));
  if (AStatus and $40) <> 0 then
    Add(Tr('personal.codes.udsWarning', 'warning indicator requested'));
  if (AStatus and $80) <> 0 then
    Add(Tr('personal.codes.udsIncompleteCycle', 'incomplete this cycle'));
  if Result = '' then
    Result := Tr('personal.codes.udsNoStatus', 'no status bits');
end;

function DisplayUDSCode(const ACode: string): string;
begin
  Result := ACode;
  if (Length(ACode) = 7) and (Pos('-', ACode) = 0) then
    Result := Copy(ACode, 1, 5) + '-' + Copy(ACode, 6, 2);
end;

function PlainDtcCode(const ACode: string): string;
begin
  Result := ACode;
  if Length(Result) > 5 then
    Result := Copy(Result, 1, 5);
end;

{ TfrmPageCodes }

function TfrmPageCodes.PageKey: string;
begin
  Result := PAGE_CODES;
end;

procedure TfrmPageCodes.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  FSelectedClassic := -1;
  FSelectedUDS := -1;
  segView.ItemIndex := 0;
  Translate;
  ResetPage;
  UpdateConnectedState;
end;

procedure TfrmPageCodes.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive and Assigned(FHost) and FHost.Connected and
    (Length(FClassicRows) = 0) and not FReading then
    ReadClassicCodes;
end;

procedure TfrmPageCodes.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
    ResetPage
  else if FActive and not FReading then
    ReadClassicCodes;
  UpdateConnectedState;
end;

procedure TfrmPageCodes.UnitsChanged;
begin
  AttachFreezeToRows;
  ApplyClassicRows;
  RenderFreezeView;
end;

procedure TfrmPageCodes.Translate;
begin
  lblTitle.Caption := Tr('personal.codes.title', 'Trouble codes');
  lblSubtitle.Caption := Tr('personal.codes.subtitle',
    'Stored, pending and permanent faults, freeze frame and UDS details.');
  btnRead.Caption := Tr('personal.codes.read', 'Read codes');
  btnClear.Caption := Tr('personal.codes.clear', 'Clear codes...');
  btnSave.Caption := Tr('personal.codes.save', 'Save report');
  btnWeb.Caption := Tr('personal.codes.searchWeb', 'Search the web');

  segView.Items.BeginUpdate;
  try
    segView.Items.Clear;
    segView.Items.Add(Tr('personal.codes.classicView', 'Classic OBD'));
    segView.Items.Add(Tr('personal.codes.udsView', 'All modules (UDS)'));
  finally
    segView.Items.EndUpdate;
  end;

  banOffline.Title := Tr('personal.common.offlineTitle', 'Connect to your car');
  banOffline.Text := Tr('personal.codes.offlineText',
    'Open the Adapter page and connect before reading trouble codes.');
  banUDS.Title := Tr('personal.codes.udsNotReadTitle', 'UDS view not read yet');
  banUDS.Text := Tr('personal.codes.udsNotReadText',
    'Use Read codes in the All modules view to try SAE J1979-2 and WWH-OBD.');

  dtcPanel.TitleCaption := Tr('personal.codes.codesCaption', 'Codes');
  dtcPanel.ReadCaption := btnRead.Caption;
  dtcPanel.ClearCaption := Tr('personal.codes.clearShort', 'Clear');
  dtcPanel.ConfirmClearCaption := Tr('personal.codes.clearConfirmShort',
    'Clear now');
  dtcPanel.CancelCaption := Tr('personal.common.cancel', 'Cancel');

  cardDetails.Title := Tr('personal.codes.detailsTitle', 'Code details');
  cardDetails.FooterText := Tr('personal.codes.detailsHint',
    'Causes are typical; confirm the diagnosis before replacing parts.');
  freezeView.Title := Tr('personal.codes.freezeTitle', 'Freeze frame');
  freezeView.Subtitle := Tr('personal.codes.freezeSubtitle',
    'Snapshot recorded when the MIL-setting code was stored.');
  freezeView.HeaderInfo := Tr('personal.codes.freezeEmpty',
    'Read codes to load the freeze frame.');
  cardUDSDetails.Title := Tr('personal.codes.udsDetailsTitle',
    'UDS / WWH-OBD details');
  cardUDSDetails.FooterText := Tr('personal.codes.udsFooter',
    'UDS status bits and WWH classes are reported only by newer cars.');

  if gridUDS.DataColumns.Count >= 6 then
  begin
    gridUDS.DataColumns[0].Caption := Tr('personal.codes.udsColStatus',
      'Status');
    gridUDS.DataColumns[1].Caption := Tr('personal.codes.udsColCode', 'Code');
    gridUDS.DataColumns[2].Caption := Tr('personal.codes.udsColText',
      'Description / failure type');
    gridUDS.DataColumns[3].Caption := Tr('personal.codes.udsColGroup',
      'Readiness group');
    gridUDS.DataColumns[4].Caption := Tr('personal.codes.udsColClass',
      'WWH class');
    gridUDS.DataColumns[5].Caption := Tr('personal.codes.udsColEcu', 'ECU');
  end;
  gridUDS.EmptyTitle := Tr('personal.codes.udsEmptyTitle', 'No UDS codes');
  gridUDS.EmptyHint := Tr('personal.codes.udsEmptyHint',
    'Read the UDS view, or this vehicle may not support it.');

  UpdateClassicDetails;
  UpdateUDSDetails;
  UpdateConnectedState;
end;

procedure TfrmPageCodes.ResetPage;
begin
  FClassicRows := nil;
  FUDSRows := nil;
  FFreezeValues := nil;
  FFreezeDtc := '';
  FSelectedClassic := -1;
  FSelectedUDS := -1;
  FLastRead := 0;
  FLastMIL := False;
  FLastStoredCount := 0;
  dtcPanel.Clear;
  gridUDS.Clear;
  kvDetails.Clear;
  kvUDS.Clear;
  freezeView.Clear;
  if Assigned(FHost) then
  begin
    FHost.SetBadge(PAGE_CODES, 0);
    FHost.SetCodeSummary(0, False);
  end;
end;

procedure TfrmPageCodes.UpdateConnectedState;
begin
  if not Assigned(FHost) then
    Exit;
  banOffline.Visible := not FHost.Connected;
  btnRead.Enabled := FHost.Connected and not FReading;
  btnClear.Enabled := FHost.Connected and not FReading and
    ((Length(FClassicRows) > 0) or (Length(FUDSRows) > 0));
  btnSave.Enabled := (Length(FClassicRows) + Length(FUDSRows)) > 0;
  btnWeb.Enabled := SelectedCode <> '';
  dtcPanel.Enabled := FHost.Connected and not FReading;
  gridUDS.Enabled := FHost.Connected and not FReading;
end;

procedure TfrmPageCodes.SetView(AIndex: Integer);
begin
  pnlClassic.Visible := AIndex = 0;
  pnlUDS.Visible := AIndex = 1;
  if AIndex = 1 then
    UpdateUDSDetails
  else
    UpdateClassicDetails;
  UpdateConnectedState;
end;

procedure TfrmPageCodes.segViewChange(Sender: TObject);
begin
  SetView(segView.ItemIndex);
end;

procedure TfrmPageCodes.btnReadClick(Sender: TObject);
begin
  if segView.ItemIndex = 1 then
    ReadUDSCodes
  else
    ReadClassicCodes;
end;

procedure TfrmPageCodes.dtcPanelReadCodes(Sender: TObject);
begin
  ReadClassicCodes;
end;

procedure TfrmPageCodes.dtcPanelClearCodes(Sender: TObject);
begin
  ClearCodes;
end;

procedure TfrmPageCodes.btnClearClick(Sender: TObject);
begin
  ClearCodes;
end;

procedure TfrmPageCodes.btnSaveClick(Sender: TObject);
var
  BaseName, TextName, HtmlName: string;
begin
  if not Assigned(FHost) then
    Exit;
  BaseName := ReportFileBase;
  TextName := BaseName + '.txt';
  HtmlName := BaseName + '.html';
  TFile.WriteAllText(TextName, BuildTextReport, TEncoding.UTF8);
  TFile.WriteAllText(HtmlName, BuildHtmlReport, TEncoding.UTF8);
  FHost.Notify(tkSuccess, Tr('personal.codes.reportSavedTitle', 'Report saved'),
    TextName);
  FHost.Log(TrFmt('personal.codes.reportSavedLog', 'Trouble-code report saved: %s',
    [TextName]));
end;

procedure TfrmPageCodes.btnWebClick(Sender: TObject);
var
  Code, Url: string;
begin
  Code := SelectedCode;
  if Code = '' then
    Exit;
  Url := 'https://www.google.com/search?q=' +
    TNetEncoding.URL.Encode(Code + ' OBD-II trouble code');
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPageCodes.dtcPanelSelect(Sender: TObject; AIndex: Integer);
begin
  FSelectedClassic := AIndex;
  UpdateClassicDetails;
end;

procedure TfrmPageCodes.gridUDSChange(Sender: TObject);
begin
  FSelectedUDS := gridUDS.SelectedRow;
  UpdateUDSDetails;
end;

procedure TfrmPageCodes.ReadClassicCodes;
var
  Confirmed, Pending, Permanent: TArray<TOBDDtcEntry>;
  Freeze: TArray<TFreezeValue>;
  FreezeDtc: string;
  Status: TOBDMonitorStatus;
  StatusOK: Boolean;
  StatusError: string;
  Errors: TStringList;
begin
  if not (Assigned(FHost) and FHost.Connected) or FReading then
    Exit;
  FReading := True;
  UpdateConnectedState;
  FHost.SetProgress(Tr('personal.codes.reading', 'Reading trouble codes...'), 5);
  Errors := TStringList.Create;
  Confirmed := nil;
  Pending := nil;
  Permanent := nil;
  Freeze := nil;
  FreezeDtc := '';
  Status := Default(TOBDMonitorStatus);
  StatusOK := False;
  StatusError := '';

  FHost.Session.RunJob(
    procedure
    begin
      try
        Confirmed := FHost.Session.DTCs.ReadConfirmed;
      except
        on E: Exception do
          Errors.Add('stored: ' + E.Message);
      end;
      try
        Pending := FHost.Session.DTCs.ReadPending;
      except
        on E: Exception do
          Errors.Add('pending: ' + E.Message);
      end;
      try
        Permanent := FHost.Session.DTCs.ReadPermanent;
      except
        on E: Exception do
          Errors.Add('permanent: ' + E.Message);
      end;
      StatusOK := FHost.Session.DriveCycle.ReadMonitorStatus($01, Status,
        StatusError);
      if not StatusOK and (StatusError <> '') then
        Errors.Add('MIL: ' + StatusError);
      ReadFreezeFrameValues(Freeze, FreezeDtc);
    end,
    procedure(const AError: string)
    var
      Rows: TArray<TCodeRow>;
      I: Integer;
      ErrorText: string;
    begin
      try
        FReading := False;
        FHost.SetProgress('', -1);
        if AError <> '' then
        begin
          FHost.Notify(tkDanger, Tr('personal.codes.readFailedTitle',
            'Codes not read'), AError);
          Exit;
        end;

        Rows := nil;
        for I := 0 to High(Confirmed) do
          AppendClassicRow(Rows, Confirmed[I], dsStored);
        for I := 0 to High(Pending) do
          AppendClassicRow(Rows, Pending[I], dsPending);
        for I := 0 to High(Permanent) do
          AppendClassicRow(Rows, Permanent[I], dsPermanent);

        FClassicRows := Rows;
        FFreezeValues := Freeze;
        FFreezeDtc := FreezeDtc;
        if StatusOK then
        begin
          FLastMIL := Status.MILOn;
          FLastStoredCount := Status.DTCCount;
        end
        else
        begin
          FLastMIL := Length(Confirmed) > 0;
          FLastStoredCount := Length(Confirmed);
        end;
        FLastRead := Now;
        AttachFreezeToRows;
        ApplyClassicRows;
        RenderFreezeView;
        FHost.SetCodeSummary(FLastStoredCount, FLastMIL);
        FHost.SetBadge(PAGE_CODES, Length(FClassicRows));
        if Errors.Count > 0 then
        begin
          ErrorText := Errors.Text.Trim;
          FHost.Notify(tkWarning, Tr('personal.codes.partialTitle',
            'Some code groups were not supported'), ErrorText);
          FHost.LogWarning('Trouble-code read warnings: ' + ErrorText);
        end
        else
          FHost.Log(Format('Read %d trouble codes.', [Length(FClassicRows)]));
      finally
        Errors.Free;
        UpdateConnectedState;
      end;
    end);
end;

procedure TfrmPageCodes.ReadUDSCodes;
var
  UDS: TArray<TOBDDtcEntry>;
  WWH, WWHPermanent: TArray<TOBDWWHDtcEntry>;
  Rows: TArray<TUDSRow>;
  Status: TOBDMonitorStatus;
  StatusOK: Boolean;
  StatusError: string;
  MILDistance, MILTime: UInt32;
  Errors: TStringList;
begin
  if not (Assigned(FHost) and FHost.Connected) or FReading then
    Exit;
  FReading := True;
  UpdateConnectedState;
  FHost.SetProgress(Tr('personal.codes.readingUds', 'Reading UDS codes...'), 5);
  Errors := TStringList.Create;
  UDS := nil;
  WWH := nil;
  WWHPermanent := nil;
  Rows := nil;
  Status := Default(TOBDMonitorStatus);
  StatusOK := False;
  StatusError := '';
  MILDistance := 0;
  MILTime := 0;

  FHost.Session.RunJob(
    procedure
    begin
      try
        UDS := FHost.Session.DTCs.ReadUDS($FF);
      except
        on E: Exception do
          Errors.Add('SAE J1979-2: ' + E.Message);
      end;
      try
        WWH := FHost.Session.WWHOBD.ReadBySeverity;
      except
        on E: Exception do
          Errors.Add('WWH-OBD severity: ' + E.Message);
      end;
      try
        WWHPermanent := FHost.Session.WWHOBD.ReadPermanent;
      except
        on E: Exception do
          Errors.Add('WWH-OBD permanent: ' + E.Message);
      end;
      try
        FHost.Session.WWHOBD.ReadMILUsage(MILDistance, MILTime);
      except
        on E: Exception do
          Errors.Add('WWH-OBD MIL usage: ' + E.Message);
      end;
      StatusOK := FHost.Session.DriveCycle.ReadMonitorStatus($01, Status,
        StatusError);
    end,
    procedure(const AError: string)
    var
      I: Integer;
      Msg: string;
      Kind: TOBDBannerKind;
    begin
      try
        FReading := False;
        FHost.SetProgress('', -1);
        if AError <> '' then
        begin
          FHost.Notify(tkDanger, Tr('personal.codes.udsFailedTitle',
            'UDS codes not read'), AError);
          Exit;
        end;
        for I := 0 to High(UDS) do
          AppendUDSRow(Rows, UDS[I]);
        for I := 0 to High(WWH) do
          AppendWWHRow(Rows, WWH[I], False);
        for I := 0 to High(WWHPermanent) do
          AppendWWHRow(Rows, WWHPermanent[I], True);
        FUDSRows := Rows;
        FSelectedUDS := -1;
        if StatusOK then
        begin
          FLastMIL := Status.MILOn;
          FLastStoredCount := Length(FUDSRows);
        end
        else
        begin
          FLastMIL := Length(FUDSRows) > 0;
          FLastStoredCount := Length(FUDSRows);
        end;
        FLastRead := Now;
        if Length(FUDSRows) > 0 then
        begin
          Kind := bnSuccess;
          Msg := TrFmt('personal.codes.udsReadOk',
            'Read %d UDS / WWH-OBD code rows.', [Length(FUDSRows)]);
          if (MILDistance > 0) or (MILTime > 0) then
            Msg := Msg + ' ' + TrFmt('personal.codes.milUsage',
              'MIL usage: %d km, %d min.', [MILDistance, MILTime]);
        end
        else
        begin
          Kind := bnWarning;
          if Errors.Count > 0 then
            Msg := Tr('personal.codes.udsNotSupported',
              'This car did not answer the UDS / WWH-OBD code requests.')
          else
            Msg := Tr('personal.codes.udsNoCodes',
              'No UDS / WWH-OBD codes were reported.');
        end;
        ApplyUDSRows(Msg, Kind);
        FHost.SetCodeSummary(FLastStoredCount, FLastMIL);
        FHost.SetBadge(PAGE_CODES, Length(FUDSRows));
        if Errors.Count > 0 then
          FHost.LogWarning('UDS/WWH read warnings: ' + Errors.Text.Trim)
        else
          FHost.Log(Msg);
      finally
        Errors.Free;
        UpdateConnectedState;
      end;
    end);
end;

procedure TfrmPageCodes.ClearCodes;
begin
  if not (Assigned(FHost) and FHost.Connected) or FReading then
    Exit;
  if not FHost.Confirm(dkDanger,
    Tr('personal.codes.clearTitle', 'Clear trouble codes?'),
    Tr('personal.codes.clearText',
    'Clearing asks the ECU to erase stored and pending emission faults. The MIL can turn off, readiness monitors reset to not complete and freeze-frame data is lost. Only clear codes after recording the report and fixing the cause.'),
    Tr('personal.codes.clearConfirm', 'Clear codes')) then
    Exit;

  FReading := True;
  UpdateConnectedState;
  FHost.SetProgress(Tr('personal.codes.clearing', 'Clearing trouble codes...'),
    30);
  FHost.Session.RunJob(
    procedure
    begin
      FHost.Session.ClearDTC.AutoExecute := True;
      try
        FHost.Session.ClearDTC.Clear;
      finally
        FHost.Session.ClearDTC.AutoExecute := False;
      end;
    end,
    procedure(const AError: string)
    begin
      FReading := False;
      FHost.SetProgress('', -1);
      if AError <> '' then
        FHost.Notify(tkDanger, Tr('personal.codes.clearFailedTitle',
          'Codes not cleared'), AError)
      else
      begin
        FHost.Notify(tkSuccess, Tr('personal.codes.clearedTitle',
          'Codes cleared'), Tr('personal.codes.clearedText',
          'The ECU acknowledged the clear request. Reading the codes again.'));
        FHost.Log('Trouble codes cleared by user request.');
        ReadClassicCodes;
      end;
      UpdateConnectedState;
    end);
end;

procedure TfrmPageCodes.ReadFreezeFrameValues(out AValues: TArray<TFreezeValue>;
  out ADtc: string);
var
  I, N: Integer;
  PID: Byte;
  V: TOBDPIDValue;
  FV: TFreezeValue;
begin
  AValues := nil;
  ADtc := '';
  for I := Low(FREEZE_PIDS) to High(FREEZE_PIDS) do
  begin
    PID := FREEZE_PIDS[I];
    try
      V := FHost.Session.FreezeFrame.Read(PID, 0);
    except
      Continue;
    end;
    FV := Default(TFreezeValue);
    FV.PID := PID;
    FV.Caption := V.Description;
    if FV.Caption = '' then
      FV.Caption := Format('PID $%.2x', [PID]);
    FV.UnitText := V.Unit_;
    FV.HasNumber := not IsNan(V.Value);
    FV.Value := V.Value;
    FV.RawText := BytesToText(V.Raw);
    if (PID = $02) and (Length(V.Raw) >= 2) then
    begin
      ADtc := TOBDDTCs.DecodeJ2012(V.Raw[0], V.Raw[1]);
      FV.Caption := Tr('personal.codes.freezeDtc', 'DTC that set the MIL');
      FV.RawText := ADtc;
      FV.HasNumber := False;
    end;
    N := Length(AValues);
    SetLength(AValues, N + 1);
    AValues[N] := FV;
  end;
end;

procedure TfrmPageCodes.LookupDtcText(const ACode, AFallback: string;
  out ADescription, ASeverity, ASource: string);
var
  Info: TOBDDtcInfo;
  Ext: IOBDOEMExtension;
  OEMEntry: TOBDDtcCatalogEntry;
  Plain: string;
begin
  Plain := PlainDtcCode(ACode);
  ADescription := AFallback;
  ASeverity := '';
  ASource := Tr('personal.codes.sourceGeneric', 'Generic');
  if TOBDServiceCatalog.Default.TryGetDTC(Plain, Info) then
  begin
    if Info.Description <> '' then
      ADescription := Info.Description;
    ASeverity := Info.Severity;
  end;
  if IsManufacturerDtc(Plain) then
  begin
    ASource := Tr('personal.codes.sourceManufacturer', 'Manufacturer');
    if Assigned(FHost) and (FHost.Session.VehicleVIN <> '') then
    begin
      Ext := TOBDOEMRegistry.FindByVIN(FHost.Session.VehicleVIN);
      if (Ext <> nil) and Ext.DescribeDTC(Plain, OEMEntry) then
      begin
        if OEMEntry.Description <> '' then
          ADescription := OEMEntry.Description;
        ASeverity := FormatSeverity(OEMEntry.Severity);
        ASource := Ext.DisplayName;
      end;
    end;
  end;
  if ADescription = '' then
    ADescription := Tr('personal.codes.unknownDtc', 'Diagnostic trouble code');
  if ASeverity = '' then
    ASeverity := Tr('personal.codes.severityUnknown', 'unknown');
end;

procedure TfrmPageCodes.AppendClassicRow(var ARows: TArray<TCodeRow>;
  const AEntry: TOBDDtcEntry; AStatus: TOBDDtcStatus);
var
  Row: TCodeRow;
  N: Integer;
begin
  Row := Default(TCodeRow);
  Row.Code := AEntry.Code;
  Row.Ecu := AEntry.Ecu;
  Row.SystemName := SystemNameFromCode(AEntry.Code);
  Row.Status := AStatus;
  LookupDtcText(AEntry.Code, AEntry.Description, Row.Description, Row.Severity,
    Row.Source);
  N := Length(ARows);
  SetLength(ARows, N + 1);
  ARows[N] := Row;
end;

procedure TfrmPageCodes.AppendUDSRow(var ARows: TArray<TUDSRow>;
  const AEntry: TOBDDtcEntry);
var
  Row: TUDSRow;
  Severity, Source: string;
  N: Integer;
begin
  Row := Default(TUDSRow);
  Row.Code := DisplayUDSCode(AEntry.Code);
  Row.PlainCode := PlainDtcCode(AEntry.Code);
  Row.Ecu := AEntry.Ecu;
  Row.Source := Tr('personal.codes.sourceUds', 'SAE J1979-2');
  Row.FailureType := '';
  if Length(AEntry.Raw) >= 3 then
    Row.FailureType := IntToHex(AEntry.Raw[2], 2);
  if AEntry.HasStatus then
    Row.StatusText := UDSStatusText(AEntry.Status)
  else
    Row.StatusText := Tr('personal.codes.udsNoStatus', 'no status bits');
  LookupDtcText(Row.PlainCode, AEntry.Description, Row.Description, Severity,
    Source);
  if Source <> '' then
    Row.Source := Row.Source + ' / ' + Source;
  Row.ReadinessGroup := Tr('personal.codes.groupUnknown', 'Not reported');
  Row.WWHClass := '';
  N := Length(ARows);
  SetLength(ARows, N + 1);
  ARows[N] := Row;
end;

procedure TfrmPageCodes.AppendWWHRow(var ARows: TArray<TUDSRow>;
  const AEntry: TOBDWWHDtcEntry; APermanent: Boolean);
var
  Row: TUDSRow;
  Severity, Source: string;
  N: Integer;
begin
  Row := Default(TUDSRow);
  Row.Code := AEntry.Code;
  Row.PlainCode := AEntry.PlainCode;
  Row.Ecu := AEntry.Ecu;
  Row.Source := Tr('personal.codes.sourceWwh', 'WWH-OBD');
  Row.Permanent := APermanent;
  Row.FailureType := IntToHex(AEntry.FailureTypeByte, 2);
  Row.StatusText := UDSStatusText(AEntry.StatusByte);
  if APermanent then
    Row.StatusText := Row.StatusText + ', ' +
      Tr('personal.codes.statusPermanent', 'Permanent');
  LookupDtcText(Row.PlainCode, '', Row.Description, Severity, Source);
  Row.ReadinessGroup := Format('$%.2x', [AEntry.ReadinessGroupIdentifier]);
  Row.WWHClass := WWHClassText(AEntry.WWHClass);
  if Row.WWHClass = '' then
    Row.WWHClass := WWHClassText(AEntry.DtcClass);
  N := Length(ARows);
  SetLength(ARows, N + 1);
  ARows[N] := Row;
end;

procedure TfrmPageCodes.AttachFreezeToRows;
var
  I, Target: Integer;
  Lines: TArray<string>;
begin
  Target := -1;
  if Length(FClassicRows) = 0 then
    Exit;
  if FFreezeDtc <> '' then
    for I := 0 to High(FClassicRows) do
      if SameText(FClassicRows[I].Code, FFreezeDtc) then
      begin
        Target := I;
        Break;
      end;
  if Target < 0 then
    for I := 0 to High(FClassicRows) do
      if FClassicRows[I].Status = dsStored then
      begin
        Target := I;
        Break;
      end;
  Lines := CurrentFreezeLines;
  for I := 0 to High(FClassicRows) do
    if I = Target then
      FClassicRows[I].FreezeValues := Lines
    else
      FClassicRows[I].FreezeValues := nil;
end;

procedure TfrmPageCodes.ApplyClassicRows;
var
  I: Integer;
begin
  dtcPanel.BeginUpdate;
  try
    dtcPanel.Clear;
    for I := 0 to High(FClassicRows) do
      dtcPanel.AddCode(FClassicRows[I].Code, FClassicRows[I].Description,
        FClassicRows[I].SystemName, FClassicRows[I].Ecu, FClassicRows[I].Status,
        FClassicRows[I].FreezeValues);
    dtcPanel.LastRead := FLastRead;
  finally
    dtcPanel.EndUpdate;
  end;
  if Length(FClassicRows) = 0 then
    FSelectedClassic := -1
  else if (FSelectedClassic < 0) or (FSelectedClassic > High(FClassicRows)) then
    FSelectedClassic := 0;
  if FSelectedClassic >= 0 then
    dtcPanel.SelectedIndex := FSelectedClassic;
  UpdateClassicDetails;
end;

procedure TfrmPageCodes.ApplyUDSRows(const AMessage: string;
  AKind: TOBDBannerKind);
var
  I, RowIndex: Integer;
  Group, LastGroup: string;
begin
  banUDS.Kind := AKind;
  banUDS.Title := Tr('personal.codes.udsStatusTitle', 'UDS / WWH-OBD status');
  banUDS.Text := AMessage;
  gridUDS.BeginUpdate;
  try
    gridUDS.Clear;
    LastGroup := '';
    for I := 0 to High(FUDSRows) do
    begin
      Group := FUDSRows[I].Source;
      if FUDSRows[I].Ecu <> '' then
        Group := Group + '  ·  ' + FUDSRows[I].Ecu;
      if not SameText(Group, LastGroup) then
      begin
        gridUDS.AddGroup(Group);
        LastGroup := Group;
      end;
      RowIndex := gridUDS.AddRow([FUDSRows[I].StatusText, FUDSRows[I].Code,
        FUDSRows[I].Description + IfThen(FUDSRows[I].FailureType <> '',
        ' (' + FUDSRows[I].FailureType + ')', ''), FUDSRows[I].ReadinessGroup,
        FUDSRows[I].WWHClass, FUDSRows[I].Ecu]);
      gridUDS.RowData[RowIndex] := I;
      if FUDSRows[I].Permanent then
        gridUDS.CellTone[0, RowIndex] := skAccent
      else if Pos('pending', LowerCase(FUDSRows[I].StatusText)) > 0 then
        gridUDS.CellTone[0, RowIndex] := skWarning
      else if Pos('failed', LowerCase(FUDSRows[I].StatusText)) > 0 then
        gridUDS.CellTone[0, RowIndex] := skDanger;
    end;
  finally
    gridUDS.EndUpdate;
  end;
  UpdateUDSDetails;
end;

procedure TfrmPageCodes.UpdateClassicDetails;
var
  Row: TCodeRow;
  Item: TOBDKeyValueItem;
begin
  kvDetails.BeginUpdate;
  try
    kvDetails.Clear;
    if (FSelectedClassic < 0) or (FSelectedClassic > High(FClassicRows)) then
    begin
      cardDetails.Title := Tr('personal.codes.detailsTitle', 'Code details');
      btnWeb.Enabled := False;
      kvDetails.Add('', Tr('personal.codes.emptySelection', 'Selection'),
        Tr('personal.codes.emptySelectionText', 'Read codes and select a row.'));
      Exit;
    end;
    Row := FClassicRows[FSelectedClassic];
    cardDetails.Title := Row.Code;
    cardDetails.FooterText := TrFmt('personal.codes.severityFooter',
      'Severity: %s. Source: %s.', [Row.Severity, Row.Source]);
    Item := kvDetails.Add(Tr('personal.codes.detailGroupCode', 'Code'),
      Tr('personal.codes.detailStatus', 'Status'), DtcStatusText(Row.Status),
      kvkChip);
    Item.Tone := DtcStatusTone(Row.Status);
    kvDetails.Add(Tr('personal.codes.detailGroupCode', 'Code'),
      Tr('personal.codes.detailSystem', 'System'), Row.SystemName);
    kvDetails.Add(Tr('personal.codes.detailGroupCode', 'Code'),
      Tr('personal.codes.detailEcu', 'Control unit'), IfThen(Row.Ecu <> '',
      Row.Ecu, Tr('personal.codes.ecuUnknown', 'not reported')), kvkMono);
    kvDetails.Add(Tr('personal.codes.detailGroupMeaning', 'Meaning'),
      Tr('personal.codes.detailDescription', 'Description'), Row.Description);
    kvDetails.Add(Tr('personal.codes.detailGroupMeaning', 'Meaning'),
      Tr('personal.codes.detailSeverity', 'Severity'), Row.Severity, kvkTone);
    kvDetails.Add(Tr('personal.codes.detailGroupMeaning', 'Meaning'),
      Tr('personal.codes.detailCatalog', 'Catalog'), Row.Source);
    if Length(Row.FreezeValues) > 0 then
      kvDetails.Add(Tr('personal.codes.detailGroupFreeze', 'Freeze frame'),
        Tr('personal.codes.detailFreeze', 'Captured values'),
        Format(TrPlural('personal.codes.freezeCount', Length(Row.FreezeValues),
        '%d value', '%d values'), [Length(Row.FreezeValues)]))
    else
      kvDetails.Add(Tr('personal.codes.detailGroupFreeze', 'Freeze frame'),
        Tr('personal.codes.detailFreeze', 'Captured values'),
        Tr('personal.codes.noFreezeForCode', 'No freeze frame attached'));
    btnWeb.Enabled := True;
  finally
    kvDetails.EndUpdate;
  end;
end;

procedure TfrmPageCodes.UpdateUDSDetails;
var
  Row: TUDSRow;
  DataIndex: Integer;
begin
  kvUDS.BeginUpdate;
  try
    kvUDS.Clear;
    DataIndex := FSelectedUDS;
    if (DataIndex < 0) or (DataIndex > High(FUDSRows)) then
    begin
      cardUDSDetails.Title := Tr('personal.codes.udsDetailsTitle',
        'UDS / WWH-OBD details');
      kvUDS.Add('', Tr('personal.codes.emptySelection', 'Selection'),
        Tr('personal.codes.udsSelectHint', 'Read the UDS view and select a row.'));
      Exit;
    end;
    Row := FUDSRows[DataIndex];
    cardUDSDetails.Title := Row.Code;
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.detailStatus', 'Status'), Row.StatusText);
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.detailDescription', 'Description'), Row.Description);
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.failureType', 'Failure type'), IfThen(Row.FailureType <> '',
      Row.FailureType, Tr('personal.codes.notReported', 'not reported')),
      kvkMono);
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.readinessGroup', 'Readiness group'),
      Row.ReadinessGroup);
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.wwhClass', 'WWH class'), IfThen(Row.WWHClass <> '',
      Row.WWHClass, Tr('personal.codes.notReported', 'not reported')));
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.detailEcu', 'Control unit'), IfThen(Row.Ecu <> '',
      Row.Ecu, Tr('personal.codes.ecuUnknown', 'not reported')), kvkMono);
    kvUDS.Add(Tr('personal.codes.udsGroup', 'UDS data'),
      Tr('personal.codes.detailCatalog', 'Catalog'), Row.Source);
  finally
    kvUDS.EndUpdate;
  end;
end;

procedure TfrmPageCodes.RenderFreezeView;
var
  I: Integer;
  UnitText: string;
  V: Double;
begin
  freezeView.BeginUpdate;
  try
    freezeView.Clear;
    freezeView.DtcCode := FFreezeDtc;
    if FFreezeDtc <> '' then
      freezeView.HeaderInfo := TrFmt('personal.codes.freezeHeaderFor',
        'Frame 0 for %s', [FFreezeDtc])
    else
      freezeView.HeaderInfo := Tr('personal.codes.freezeHeader',
        'Frame 0, most recent freeze frame');
    for I := 0 to High(FFreezeValues) do
    begin
      if FFreezeValues[I].HasNumber then
      begin
        V := ConvertedValue(FFreezeValues[I].Value, FFreezeValues[I].UnitText,
          UnitText);
        freezeView.AddValue(FFreezeValues[I].PID,
          'pid_' + IntToHex(FFreezeValues[I].PID, 2), FFreezeValues[I].Caption,
          V, UnitText, 1);
      end
      else
        freezeView.AddText(FFreezeValues[I].PID,
          'pid_' + IntToHex(FFreezeValues[I].PID, 2), FFreezeValues[I].Caption,
          FFreezeValues[I].RawText);
    end;
  finally
    freezeView.EndUpdate;
  end;
end;

function TfrmPageCodes.ConvertedValue(AValue: Double; const AUnit: string;
  out AConvertedUnit: string): Double;
begin
  Result := AValue;
  AConvertedUnit := AUnit;
  if not Assigned(FHost) or (FHost.Settings.UnitSystem <> usImperial) then
    Exit;
  if SameText(AUnit, '°C') then
  begin
    Result := AValue * 9 / 5 + 32;
    AConvertedUnit := '°F';
  end
  else if SameText(AUnit, 'km/h') then
  begin
    Result := AValue * 0.621371;
    AConvertedUnit := 'mph';
  end
  else if SameText(AUnit, 'km') then
  begin
    Result := AValue * 0.621371;
    AConvertedUnit := 'mi';
  end
  else if SameText(AUnit, 'kPa') then
  begin
    Result := AValue * 0.145038;
    AConvertedUnit := 'psi';
  end;
end;

function TfrmPageCodes.FormatFreezeValue(const AValue: TFreezeValue): string;
var
  UnitText: string;
  V: Double;
begin
  if AValue.HasNumber then
  begin
    V := ConvertedValue(AValue.Value, AValue.UnitText, UnitText);
    Result := Format('%s: %s %s', [AValue.Caption, FormatFloat('0.###', V),
      UnitText]).Trim;
  end
  else
    Result := AValue.Caption + ': ' + AValue.RawText;
end;

function TfrmPageCodes.CurrentFreezeLines: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Length(FFreezeValues));
  for I := 0 to High(FFreezeValues) do
    Result[I] := FormatFreezeValue(FFreezeValues[I]);
end;

function TfrmPageCodes.SelectedCode: string;
begin
  Result := '';
  if pnlUDS.Visible then
  begin
    if (FSelectedUDS >= 0) and (FSelectedUDS <= High(FUDSRows)) then
      Result := FUDSRows[FSelectedUDS].PlainCode;
  end
  else if (FSelectedClassic >= 0) and (FSelectedClassic <= High(FClassicRows)) then
    Result := FClassicRows[FSelectedClassic].Code;
end;

function TfrmPageCodes.ReportFileBase: string;
var
  Folder: string;
begin
  Folder := FHost.Settings.DataSubFolder('Reports');
  Result := TPath.Combine(Folder, 'trouble-codes-' +
    FormatDateTime('yyyymmdd-hhnnss', Now));
end;

function TfrmPageCodes.BuildTextReport: string;
var
  Lines: TStringList;
  I, J: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('OBD-Studio Personal trouble-code report');
    Lines.Add('Created: ' + DateTimeToStr(Now));
    Lines.Add('VIN: ' + IfThen(Assigned(FHost), FHost.Session.VehicleVIN, ''));
    Lines.Add('');
    Lines.Add('Classic OBD-II');
    for I := 0 to High(FClassicRows) do
    begin
      Lines.Add(Format('%s  %s  %s  %s', [FClassicRows[I].Code,
        DtcStatusText(FClassicRows[I].Status), FClassicRows[I].Ecu,
        FClassicRows[I].Description]));
      Lines.Add('  Severity: ' + FClassicRows[I].Severity + ' / ' +
        FClassicRows[I].Source);
      for J := 0 to High(FClassicRows[I].FreezeValues) do
        Lines.Add('  Freeze: ' + FClassicRows[I].FreezeValues[J]);
    end;
    Lines.Add('');
    Lines.Add('UDS / WWH-OBD');
    for I := 0 to High(FUDSRows) do
      Lines.Add(Format('%s  %s  %s  %s  %s', [FUDSRows[I].Code,
        FUDSRows[I].StatusText, FUDSRows[I].Ecu, FUDSRows[I].WWHClass,
        FUDSRows[I].Description]));
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TfrmPageCodes.BuildHtmlReport: string;
var
  S: TStringBuilder;
  I, J: Integer;
begin
  S := TStringBuilder.Create;
  try
    S.AppendLine('<!doctype html><html><head><meta charset="utf-8"><title>OBD-Studio Personal trouble-code report</title>');
    S.AppendLine('<style>body{font-family:Segoe UI,Arial,sans-serif;margin:24px}table{border-collapse:collapse;width:100%}td,th{border:1px solid #ddd;padding:6px;text-align:left}th{background:#f3f3f3}</style></head><body>');
    S.AppendLine('<h1>OBD-Studio Personal trouble-code report</h1>');
    S.AppendLine('<p>Created: ' + HtmlEscape(DateTimeToStr(Now)) + '</p>');
    if Assigned(FHost) and (FHost.Session.VehicleVIN <> '') then
      S.AppendLine('<p>VIN: ' + HtmlEscape(FHost.Session.VehicleVIN) + '</p>');
    S.AppendLine('<h2>Classic OBD-II</h2><table><tr><th>Code</th><th>Status</th><th>ECU</th><th>Description</th><th>Severity</th></tr>');
    for I := 0 to High(FClassicRows) do
    begin
      S.AppendLine('<tr><td>' + HtmlEscape(FClassicRows[I].Code) + '</td><td>' +
        HtmlEscape(DtcStatusText(FClassicRows[I].Status)) + '</td><td>' +
        HtmlEscape(FClassicRows[I].Ecu) + '</td><td>' +
        HtmlEscape(FClassicRows[I].Description) + '</td><td>' +
        HtmlEscape(FClassicRows[I].Severity) + '</td></tr>');
      for J := 0 to High(FClassicRows[I].FreezeValues) do
        S.AppendLine('<tr><td></td><td colspan="4">Freeze: ' +
          HtmlEscape(FClassicRows[I].FreezeValues[J]) + '</td></tr>');
    end;
    S.AppendLine('</table><h2>UDS / WWH-OBD</h2><table><tr><th>Code</th><th>Status</th><th>ECU</th><th>Class</th><th>Description</th></tr>');
    for I := 0 to High(FUDSRows) do
      S.AppendLine('<tr><td>' + HtmlEscape(FUDSRows[I].Code) + '</td><td>' +
        HtmlEscape(FUDSRows[I].StatusText) + '</td><td>' +
        HtmlEscape(FUDSRows[I].Ecu) + '</td><td>' +
        HtmlEscape(FUDSRows[I].WWHClass) + '</td><td>' +
        HtmlEscape(FUDSRows[I].Description) + '</td></tr>');
    S.AppendLine('</table></body></html>');
    Result := S.ToString;
  finally
    S.Free;
  end;
end;

end.
