//------------------------------------------------------------------------------
//  untPageRecordings
//
//  Recording browser and CSV replay page for OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageRecordings;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.DateUtils,
  System.IOUtils,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.Graphics,
  ERD.I18n,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.ReplayBar,
  ERD.UI.TrendChart,
  ERD.UI.Units,
  ERD.Recorder,
  untSession,
  untPersonalPage;

type
  TRecordingInfo = record
    FileName: string;
    DisplayName: string;
    Modified: TDateTime;
    DurationSec: Double;
    SizeBytes: Int64;
    Channels: string;
    ChannelCount: Integer;
  end;

  TCSVSeries = record
    Name: string;
    UnitText: string;
    Times: TArray<Int64>;
    Values: TArray<Double>;
    MinValue: Double;
    MaxValue: Double;
  end;

  TfrmPageRecordings = class(TFrame, IPersonalPage)
  strict private
    FHost: IPersonalHost;
    FRecordings: TList<TRecordingInfo>;
    FFullSessionRecording: Boolean;
    FFullSessionStarted: TDateTime;
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblSummary: TLabel;
    edtSearch: TEdit;
    btnRefresh: TButton;
    btnFolder: TButton;
    btnRecordSession: TButton;
    replayBar: TOBDReplayBar;
    pnlLeft: TPanel;
    lvRecordings: TListView;
    pnlActions: TPanel;
    btnRename: TButton;
    btnDelete: TButton;
    btnExport: TButton;
    btnOpenFolder: TButton;
    splMain: TSplitter;
    pnlRight: TPanel;
    memDetails: TMemo;
    chartReplay: TOBDTrendChart;
    tmrReplay: TTimer;
    tmrSession: TTimer;
    procedure BuildUI;
    procedure TopResize(Sender: TObject);
    procedure ActionsResize(Sender: TObject);
    procedure RefreshRecordings;
    procedure ApplyFilter;
    procedure AddListItem(const AInfo: TRecordingInfo);
    procedure AnalyzeCSV(const AFileName: string; out AInfo: TRecordingInfo);
    procedure LoadRecording(AIndex: Integer);
    procedure LoadCSVIntoChart(const AFileName: string);
    procedure UpdateDetails(const AInfo: TRecordingInfo);
    procedure SearchChanged(Sender: TObject);
    procedure RefreshClicked(Sender: TObject);
    procedure FolderClicked(Sender: TObject);
    procedure RecordSessionClicked(Sender: TObject);
    procedure RenameClicked(Sender: TObject);
    procedure DeleteClicked(Sender: TObject);
    procedure ExportClicked(Sender: TObject);
    procedure OpenFolderClicked(Sender: TObject);
    procedure RecordingSelected(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ReplayPlay(Sender: TObject);
    procedure ReplayPause(Sender: TObject);
    procedure ReplaySeek(Sender: TObject; APositionMs: Int64);
    procedure ReplayTimer(Sender: TObject);
    procedure SessionTimer(Sender: TObject);
    procedure StartFullSessionRecording;
    procedure StopFullSessionRecording;
    procedure UpdateSessionRecordingStatus;
    function Connected: Boolean;
    function RecordingsFolder: string;
    function SelectedIndex: Integer;
    function SelectedFileName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
  end;

implementation

{$R *.dfm}

function CSVFields(const ALine: string): TArray<string>;
var
  I: Integer;
  C: Char;
  Field: string;
  Quoted: Boolean;
begin
  Result := nil;
  Field := '';
  Quoted := False;
  I := 1;
  while I <= Length(ALine) do
  begin
    C := ALine[I];
    if C = '"' then
    begin
      if Quoted and (I < Length(ALine)) and (ALine[I + 1] = '"') then
      begin
        Field := Field + '"';
        Inc(I);
      end
      else
        Quoted := not Quoted;
    end
    else if (C = ',') and not Quoted then
    begin
      Result := Result + [Field];
      Field := '';
    end
    else
      Field := Field + C;
    Inc(I);
  end;
  Result := Result + [Field];
end;

function CleanHeader(const AHeader: string; out AUnit: string): string;
var
  L, R: Integer;
begin
  Result := AHeader;
  AUnit := '';
  L := LastDelimiter('[', Result);
  R := LastDelimiter(']', Result);
  if (L > 0) and (R > L) then
  begin
    AUnit := Copy(Result, L + 1, R - L - 1);
    Result := Trim(Copy(Result, 1, L - 1));
  end;
  if Result <> '' then
    Result := Result.Trim([#$FEFF, ' ']);
end;

function AppendDouble(const AValues: TArray<Double>; AValue: Double): TArray<Double>;
begin
  Result := AValues + [AValue];
end;

function AppendInt64(const AValues: TArray<Int64>; AValue: Int64): TArray<Int64>;
begin
  Result := AValues + [AValue];
end;

function FormatDuration(ADurationSec: Double): string;
var
  Seconds: Integer;
begin
  Seconds := Round(ADurationSec);
  if Seconds < 0 then
    Seconds := 0;
  Result := Format('%.2d:%.2d', [Seconds div 60, Seconds mod 60]);
end;

function FormatSize(ASize: Int64): string;
begin
  if ASize > 1024 * 1024 then
    Result := FormatFloat('0.0 MB', ASize / (1024 * 1024))
  else if ASize > 1024 then
    Result := FormatFloat('0.0 KB', ASize / 1024)
  else
    Result := IntToStr(ASize) + ' B';
end;

function LocalFileSize(const AFileName: string): Int64;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(AFileName, faAnyFile, SR) = 0 then
  try
    Result := SR.Size;
  finally
    FindClose(SR);
  end;
end;

{ TfrmPageRecordings }

constructor TfrmPageRecordings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordings := TList<TRecordingInfo>.Create;
  BuildUI;
end;

destructor TfrmPageRecordings.Destroy;
begin
  StopFullSessionRecording;
  FRecordings.Free;
  inherited Destroy;
end;

procedure TfrmPageRecordings.BuildUI;
begin
  Align := alClient;
  ParentBackground := False;

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.Height := 76;
  pnlTop.BevelOuter := bvNone;
  pnlTop.ParentBackground := False;
  pnlTop.OnResize := TopResize;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := pnlTop;
  lblTitle.Left := 16;
  lblTitle.Top := 10;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Font.Height := -18;

  lblSummary := TLabel.Create(Self);
  lblSummary.Parent := pnlTop;
  lblSummary.Left := 16;
  lblSummary.Top := 38;
  lblSummary.Font.Color := clGrayText;

  edtSearch := TEdit.Create(Self);
  edtSearch.Parent := pnlTop;
  edtSearch.Top := 22;
  edtSearch.Width := 220;
  edtSearch.TextHint := 'Search recordings';
  edtSearch.OnChange := SearchChanged;

  btnRefresh := TButton.Create(Self);
  btnRefresh.Parent := pnlTop;
  btnRefresh.Top := 22;
  btnRefresh.Width := 78;
  btnRefresh.OnClick := RefreshClicked;

  btnFolder := TButton.Create(Self);
  btnFolder.Parent := pnlTop;
  btnFolder.Top := 22;
  btnFolder.Width := 94;
  btnFolder.OnClick := FolderClicked;

  btnRecordSession := TButton.Create(Self);
  btnRecordSession.Parent := pnlTop;
  btnRecordSession.Top := 22;
  btnRecordSession.Width := 128;
  btnRecordSession.OnClick := RecordSessionClicked;

  replayBar := TOBDReplayBar.Create(Self);
  replayBar.Parent := Self;
  replayBar.Align := alTop;
  replayBar.Height := 82;
  replayBar.OnPlay := ReplayPlay;
  replayBar.OnPause := ReplayPause;
  replayBar.OnSeek := ReplaySeek;

  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := Self;
  pnlLeft.Align := alLeft;
  pnlLeft.Width := 560;
  pnlLeft.BevelOuter := bvNone;

  pnlActions := TPanel.Create(Self);
  pnlActions.Parent := pnlLeft;
  pnlActions.Align := alBottom;
  pnlActions.Height := 46;
  pnlActions.BevelOuter := bvNone;
  pnlActions.OnResize := ActionsResize;

  btnRename := TButton.Create(Self);
  btnRename.Parent := pnlActions;
  btnRename.Top := 8;
  btnRename.Width := 80;
  btnRename.OnClick := RenameClicked;

  btnExport := TButton.Create(Self);
  btnExport.Parent := pnlActions;
  btnExport.Top := 8;
  btnExport.Width := 94;
  btnExport.OnClick := ExportClicked;

  btnOpenFolder := TButton.Create(Self);
  btnOpenFolder.Parent := pnlActions;
  btnOpenFolder.Top := 8;
  btnOpenFolder.Width := 96;
  btnOpenFolder.OnClick := OpenFolderClicked;

  btnDelete := TButton.Create(Self);
  btnDelete.Parent := pnlActions;
  btnDelete.Top := 8;
  btnDelete.Width := 78;
  btnDelete.OnClick := DeleteClicked;

  lvRecordings := TListView.Create(Self);
  lvRecordings.Parent := pnlLeft;
  lvRecordings.Align := alClient;
  lvRecordings.ViewStyle := vsReport;
  lvRecordings.RowSelect := True;
  lvRecordings.HideSelection := False;
  lvRecordings.ReadOnly := True;
  lvRecordings.OnSelectItem := RecordingSelected;
  lvRecordings.Columns.Add.Caption := 'Name';
  lvRecordings.Columns.Add.Caption := 'Date';
  lvRecordings.Columns.Add.Caption := 'Duration';
  lvRecordings.Columns.Add.Caption := 'Size';
  lvRecordings.Columns.Add.Caption := 'Channels';
  lvRecordings.Columns[0].Width := 170;
  lvRecordings.Columns[1].Width := 130;
  lvRecordings.Columns[2].Width := 70;
  lvRecordings.Columns[3].Width := 70;
  lvRecordings.Columns[4].Width := 240;

  splMain := TSplitter.Create(Self);
  splMain.Parent := Self;
  splMain.Align := alLeft;
  splMain.Width := 6;

  pnlRight := TPanel.Create(Self);
  pnlRight.Parent := Self;
  pnlRight.Align := alClient;
  pnlRight.BevelOuter := bvNone;

  memDetails := TMemo.Create(Self);
  memDetails.Parent := pnlRight;
  memDetails.Align := alTop;
  memDetails.Height := 148;
  memDetails.ReadOnly := True;
  memDetails.ScrollBars := ssVertical;

  chartReplay := TOBDTrendChart.Create(Self);
  chartReplay.Parent := pnlRight;
  chartReplay.Align := alClient;
  chartReplay.TimeWindowSec := 600;
  chartReplay.HistorySec := 36000;
  chartReplay.ShowLegend := True;

  tmrReplay := TTimer.Create(Self);
  tmrReplay.Enabled := False;
  tmrReplay.Interval := 200;
  tmrReplay.OnTimer := ReplayTimer;

  tmrSession := TTimer.Create(Self);
  tmrSession.Enabled := False;
  tmrSession.Interval := 1000;
  tmrSession.OnTimer := SessionTimer;

  Translate;
  TopResize(pnlTop);
  ActionsResize(pnlActions);
end;

procedure TfrmPageRecordings.TopResize(Sender: TObject);
var
  X: Integer;
begin
  X := pnlTop.ClientWidth - 16;
  btnRecordSession.Left := X - btnRecordSession.Width;
  X := btnRecordSession.Left - 8;
  btnFolder.Left := X - btnFolder.Width;
  X := btnFolder.Left - 8;
  btnRefresh.Left := X - btnRefresh.Width;
  X := btnRefresh.Left - 8;
  edtSearch.Left := X - edtSearch.Width;
end;

procedure TfrmPageRecordings.ActionsResize(Sender: TObject);
var
  X: Integer;
begin
  X := 8;
  btnRename.Left := X;
  Inc(X, btnRename.Width + 8);
  btnExport.Left := X;
  Inc(X, btnExport.Width + 8);
  btnOpenFolder.Left := X;
  btnDelete.Left := pnlActions.ClientWidth - btnDelete.Width - 8;
end;

function TfrmPageRecordings.Connected: Boolean;
begin
  Result := (FHost <> nil) and FHost.Connected;
end;

function TfrmPageRecordings.PageKey: string;
begin
  Result := PAGE_RECORDINGS;
end;

procedure TfrmPageRecordings.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  RefreshRecordings;
end;

procedure TfrmPageRecordings.Activated(AActive: Boolean);
begin
  if AActive then
    RefreshRecordings
  else
    tmrReplay.Enabled := False;
end;

procedure TfrmPageRecordings.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
    StopFullSessionRecording;
end;

procedure TfrmPageRecordings.UnitsChanged;
begin
  chartReplay.Invalidate;
end;

procedure TfrmPageRecordings.Translate;
begin
  lblTitle.Caption := Tr('personal.recordings.title', 'Recordings');
  btnRefresh.Caption := Tr('personal.recordings.refresh', 'Refresh');
  btnFolder.Caption := Tr('personal.recordings.folder', 'Open folder');
  btnRename.Caption := Tr('personal.recordings.rename', 'Rename');
  btnExport.Caption := Tr('personal.recordings.export', 'Export copy');
  btnOpenFolder.Caption := Tr('personal.recordings.show', 'Show file');
  btnDelete.Caption := Tr('personal.recordings.delete', 'Delete');
  if FFullSessionRecording then
    btnRecordSession.Caption := Tr('personal.recordings.stopRecord', 'Stop recording')
  else
    btnRecordSession.Caption := Tr('personal.recordings.recordNow', 'Record session');
end;

function TfrmPageRecordings.RecordingsFolder: string;
begin
  Result := FHost.Settings.DataSubFolder('Recordings');
end;

procedure TfrmPageRecordings.RefreshRecordings;
var
  Files: TArray<string>;
  FileName: string;
  Info: TRecordingInfo;
begin
  if FHost = nil then
    Exit;
  FRecordings.Clear;
  lvRecordings.Items.BeginUpdate;
  try
    lvRecordings.Items.Clear;
    Files := TDirectory.GetFiles(RecordingsFolder, '*.csv');
    for FileName in Files do
    begin
      AnalyzeCSV(FileName, Info);
      FRecordings.Add(Info);
    end;
    ApplyFilter;
  finally
    lvRecordings.Items.EndUpdate;
  end;
  lblSummary.Caption := Format(Tr('personal.recordings.summary',
    '%d CSV recordings in %s.'), [FRecordings.Count, RecordingsFolder]);
end;

procedure TfrmPageRecordings.AnalyzeCSV(const AFileName: string;
  out AInfo: TRecordingInfo);
var
  Lines: TArray<string>;
  Header, Fields: TArray<string>;
  I, LastData: Integer;
  UnitText, ChannelNames: string;
  Duration: Double;
  FS: TFormatSettings;
begin
  AInfo.FileName := AFileName;
  AInfo.DisplayName := TPath.GetFileNameWithoutExtension(AFileName);
  AInfo.Modified := TFile.GetLastWriteTime(AFileName);
  AInfo.SizeBytes := LocalFileSize(AFileName);
  AInfo.DurationSec := 0;
  AInfo.Channels := '';
  AInfo.ChannelCount := 0;
  FS := TFormatSettings.Invariant;
  Lines := TFile.ReadAllLines(AFileName, TEncoding.UTF8);
  if Length(Lines) = 0 then
    Exit;
  Header := CSVFields(Lines[0]);
  if Length(Header) > 2 then
  begin
    AInfo.ChannelCount := Length(Header) - 2;
    ChannelNames := '';
    for I := 2 to High(Header) do
    begin
      if ChannelNames <> '' then
        ChannelNames := ChannelNames + ', ';
      ChannelNames := ChannelNames + CleanHeader(Header[I], UnitText);
    end;
    AInfo.Channels := ChannelNames;
  end;
  LastData := High(Lines);
  while (LastData > 0) and (Trim(Lines[LastData]) = '') do
    Dec(LastData);
  if LastData > 0 then
  begin
    Fields := CSVFields(Lines[LastData]);
    if Length(Fields) > 1 then
      if TryStrToFloat(Fields[1], Duration, FS) then
        AInfo.DurationSec := Duration;
  end;
end;

procedure TfrmPageRecordings.ApplyFilter;
var
  I: Integer;
  Filter, Haystack: string;
begin
  Filter := LowerCase(Trim(edtSearch.Text));
  lvRecordings.Items.BeginUpdate;
  try
    lvRecordings.Items.Clear;
    for I := 0 to FRecordings.Count - 1 do
    begin
      Haystack := LowerCase(FRecordings[I].DisplayName + ' ' + FRecordings[I].Channels);
      if (Filter = '') or (Pos(Filter, Haystack) > 0) then
        AddListItem(FRecordings[I]);
    end;
  finally
    lvRecordings.Items.EndUpdate;
  end;
end;

procedure TfrmPageRecordings.AddListItem(const AInfo: TRecordingInfo);
var
  Item: TListItem;
begin
  Item := lvRecordings.Items.Add;
  Item.Caption := AInfo.DisplayName;
  Item.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn', AInfo.Modified));
  Item.SubItems.Add(FormatDuration(AInfo.DurationSec));
  Item.SubItems.Add(FormatSize(AInfo.SizeBytes));
  Item.SubItems.Add(Format('%d: %s', [AInfo.ChannelCount, AInfo.Channels]));
end;

function TfrmPageRecordings.SelectedIndex: Integer;
var
  I: Integer;
  FileName: string;
begin
  Result := -1;
  if lvRecordings.Selected = nil then
    Exit;
  FileName := lvRecordings.Selected.Caption;
  for I := 0 to FRecordings.Count - 1 do
    if SameText(FRecordings[I].DisplayName, FileName) then
      Exit(I);
end;

function TfrmPageRecordings.SelectedFileName: string;
var
  Index: Integer;
begin
  Result := '';
  Index := SelectedIndex;
  if Index >= 0 then
    Result := FRecordings[Index].FileName;
end;

procedure TfrmPageRecordings.LoadRecording(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FRecordings.Count) then
    Exit;
  replayBar.FileName := FRecordings[AIndex].DisplayName;
  replayBar.InfoText := Format('%d channels · %s · %s',
    [FRecordings[AIndex].ChannelCount, FormatDuration(FRecordings[AIndex].DurationSec),
    FormatSize(FRecordings[AIndex].SizeBytes)]);
  replayBar.DurationMs := Round(FRecordings[AIndex].DurationSec * 1000);
  replayBar.PositionMs := 0;
  replayBar.ClearMarkers;
  if replayBar.DurationMs > 0 then
  begin
    replayBar.AddMarker(replayBar.DurationMs div 4, rpbBookmark, '25%');
    replayBar.AddMarker(replayBar.DurationMs div 2, rpbNote, '50%');
    replayBar.AddMarker((replayBar.DurationMs * 3) div 4, rpbDtc, '75%');
  end;
  LoadCSVIntoChart(FRecordings[AIndex].FileName);
  UpdateDetails(FRecordings[AIndex]);
end;

procedure TfrmPageRecordings.LoadCSVIntoChart(const AFileName: string);
var
  Lines, Fields, Header: TArray<string>;
  Series: TArray<TCSVSeries>;
  FS: TFormatSettings;
  Row, Col, Count: Integer;
  Elapsed, Value, Span: Double;
  TimeMs: Int64;
  UnitText: string;
  Channel: TOBDTrendChannel;
begin
  FS := TFormatSettings.Invariant;
  Lines := TFile.ReadAllLines(AFileName, TEncoding.UTF8);
  chartReplay.Source := nil;
  chartReplay.Channels.Clear;
  chartReplay.ClearSamples;
  if Length(Lines) = 0 then
    Exit;
  Header := CSVFields(Lines[0]);
  if Length(Header) <= 2 then
    Exit;
  Count := Min(Length(Header) - 2, 4);
  SetLength(Series, Count);
  for Col := 0 to Count - 1 do
  begin
    Series[Col].Name := CleanHeader(Header[Col + 2], UnitText);
    Series[Col].UnitText := UnitText;
    Series[Col].MinValue := Infinity;
    Series[Col].MaxValue := NegInfinity;
  end;
  for Row := 1 to High(Lines) do
  begin
    if Trim(Lines[Row]) = '' then
      Continue;
    Fields := CSVFields(Lines[Row]);
    if Length(Fields) < 2 then
      Continue;
    if not TryStrToFloat(Fields[1], Elapsed, FS) then
      Continue;
    TimeMs := Round(Elapsed * 1000);
    for Col := 0 to Count - 1 do
      if Length(Fields) > Col + 2 then
        if TryStrToFloat(Fields[Col + 2], Value, FS) then
        begin
          Series[Col].Times := AppendInt64(Series[Col].Times, TimeMs);
          Series[Col].Values := AppendDouble(Series[Col].Values, Value);
          Series[Col].MinValue := Min(Series[Col].MinValue, Value);
          Series[Col].MaxValue := Max(Series[Col].MaxValue, Value);
        end;
  end;
  for Col := 0 to Count - 1 do
  begin
    if Length(Series[Col].Values) = 0 then
    begin
      Series[Col].MinValue := 0;
      Series[Col].MaxValue := 100;
    end
    else if SameValue(Series[Col].MinValue, Series[Col].MaxValue) then
    begin
      Series[Col].MinValue := Series[Col].MinValue - 1;
      Series[Col].MaxValue := Series[Col].MaxValue + 1;
    end
    else
    begin
      Span := Series[Col].MaxValue - Series[Col].MinValue;
      Series[Col].MinValue := Series[Col].MinValue - Span * 0.1;
      Series[Col].MaxValue := Series[Col].MaxValue + Span * 0.1;
    end;
    Channel := chartReplay.AddChannel(Series[Col].Name, 0, Series[Col].MinValue,
      Series[Col].MaxValue, Series[Col].UnitText);
    Channel.Decimals := 1;
    for Row := 0 to High(Series[Col].Values) do
      chartReplay.AddSampleAt(Col, Series[Col].Times[Row], Series[Col].Values[Row]);
  end;
end;

procedure TfrmPageRecordings.UpdateDetails(const AInfo: TRecordingInfo);
begin
  memDetails.Lines.BeginUpdate;
  try
    memDetails.Clear;
    memDetails.Lines.Add('File: ' + AInfo.FileName);
    memDetails.Lines.Add('Started/modified: ' + FormatDateTime('yyyy-mm-dd hh:nn', AInfo.Modified));
    memDetails.Lines.Add('Duration: ' + FormatDuration(AInfo.DurationSec));
    memDetails.Lines.Add('Size: ' + FormatSize(AInfo.SizeBytes));
    memDetails.Lines.Add('Channels: ' + IntToStr(AInfo.ChannelCount));
    memDetails.Lines.Add(AInfo.Channels);
  finally
    memDetails.Lines.EndUpdate;
  end;
end;

procedure TfrmPageRecordings.SearchChanged(Sender: TObject);
begin
  ApplyFilter;
end;

procedure TfrmPageRecordings.RefreshClicked(Sender: TObject);
begin
  RefreshRecordings;
end;

procedure TfrmPageRecordings.FolderClicked(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(RecordingsFolder), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPageRecordings.RecordSessionClicked(Sender: TObject);
begin
  if FFullSessionRecording then
    StopFullSessionRecording
  else
    StartFullSessionRecording;
end;

procedure TfrmPageRecordings.RenameClicked(Sender: TObject);
var
  OldName, NewName, NewFile: string;
begin
  OldName := SelectedFileName;
  if OldName = '' then
    Exit;
  NewName := TPath.GetFileNameWithoutExtension(OldName);
  if not InputQuery(Tr('personal.recordings.renameTitle', 'Rename recording'),
    Tr('personal.recordings.renamePrompt', 'New name'), NewName) then
    Exit;
  NewName := Trim(NewName);
  if NewName = '' then
    Exit;
  if not SameText(ExtractFileExt(NewName), '.csv') then
    NewName := NewName + '.csv';
  NewFile := TPath.Combine(ExtractFilePath(OldName), NewName);
  if TFile.Exists(NewFile) then
  begin
    FHost.Notify(tkWarning, Tr('personal.recordings.renameExists', 'Rename'),
      Tr('personal.recordings.renameExistsText', 'A recording with that name already exists.'));
    Exit;
  end;
  TFile.Move(OldName, NewFile);
  RefreshRecordings;
end;

procedure TfrmPageRecordings.DeleteClicked(Sender: TObject);
var
  FileName: string;
begin
  FileName := SelectedFileName;
  if FileName = '' then
    Exit;
  if not FHost.Confirm(dkDanger, Tr('personal.recordings.deleteTitle', 'Delete recording'),
    Format(Tr('personal.recordings.deleteText', 'Delete %s? This cannot be undone.'),
      [ExtractFileName(FileName)]), Tr('personal.recordings.deleteConfirm', 'Delete')) then
    Exit;
  TFile.Delete(FileName);
  RefreshRecordings;
end;

procedure TfrmPageRecordings.ExportClicked(Sender: TObject);
var
  Dialog: TSaveDialog;
  FileName: string;
begin
  FileName := SelectedFileName;
  if FileName = '' then
    Exit;
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    Dialog.FileName := ExtractFileName(FileName);
    if Dialog.Execute then
      TFile.Copy(FileName, Dialog.FileName, True);
  finally
    Dialog.Free;
  end;
end;

procedure TfrmPageRecordings.OpenFolderClicked(Sender: TObject);
var
  FileName, Params: string;
begin
  FileName := SelectedFileName;
  if FileName = '' then
  begin
    FolderClicked(Sender);
    Exit;
  end;
  Params := '/select,"' + FileName + '"';
  ShellExecute(Handle, 'open', 'explorer.exe', PChar(Params), nil, SW_SHOWNORMAL);
end;

procedure TfrmPageRecordings.RecordingSelected(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    LoadRecording(SelectedIndex);
end;

procedure TfrmPageRecordings.ReplayPlay(Sender: TObject);
begin
  replayBar.Playing := True;
  tmrReplay.Enabled := True;
end;

procedure TfrmPageRecordings.ReplayPause(Sender: TObject);
begin
  replayBar.Playing := False;
  tmrReplay.Enabled := False;
end;

procedure TfrmPageRecordings.ReplaySeek(Sender: TObject; APositionMs: Int64);
begin
  replayBar.PositionMs := APositionMs;
end;

procedure TfrmPageRecordings.ReplayTimer(Sender: TObject);
var
  StepMs: Integer;
begin
  if not replayBar.Playing then
  begin
    tmrReplay.Enabled := False;
    Exit;
  end;
  StepMs := Round(tmrReplay.Interval * replayBar.Speed);
  replayBar.PositionMs := replayBar.PositionMs + StepMs;
  if replayBar.PositionMs >= replayBar.DurationMs then
  begin
    replayBar.PositionMs := replayBar.DurationMs;
    replayBar.Pause;
    tmrReplay.Enabled := False;
  end;
end;

procedure TfrmPageRecordings.SessionTimer(Sender: TObject);
begin
  UpdateSessionRecordingStatus;
end;

procedure TfrmPageRecordings.StartFullSessionRecording;
var
  FileName: string;
begin
  if not Connected then
  begin
    FHost.Notify(tkInfo, Tr('personal.recordings.connectTitle', 'Record session'),
      Tr('personal.recordings.connectText', 'Connect to a vehicle before recording a full session.'));
    Exit;
  end;
  FileName := TPath.Combine(RecordingsFolder,
    'session-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.obdlog');
  FHost.Session.Recorder.Open(FileName);
  FFullSessionRecording := True;
  FFullSessionStarted := Now;
  tmrSession.Enabled := True;
  Translate;
  UpdateSessionRecordingStatus;
end;

procedure TfrmPageRecordings.StopFullSessionRecording;
begin
  if (FHost <> nil) and (FHost.Session <> nil) and FHost.Session.Recorder.Active then
    FHost.Session.Recorder.Close;
  FFullSessionRecording := False;
  tmrSession.Enabled := False;
  if FHost <> nil then
    FHost.SetRecording('');
  Translate;
end;

procedure TfrmPageRecordings.UpdateSessionRecordingStatus;
var
  Seconds: Integer;
begin
  if not FFullSessionRecording then
    Exit;
  Seconds := Round((Now - FFullSessionStarted) * SecsPerDay);
  if Seconds < 0 then
    Seconds := 0;
  FHost.SetRecording(Format('LOG %.2d:%.2d', [Seconds div 60, Seconds mod 60]));
end;

end.
