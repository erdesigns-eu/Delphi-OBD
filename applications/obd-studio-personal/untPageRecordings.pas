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
  Vcl.ExtCtrls,
  ERD.I18n,
  ERD.UI.Toast,
  ERD.UI.Paint,
  ERD.UI.Dialogs,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.DataGrid,
  ERD.UI.KeyValueList,
  ERD.UI.Card,
  ERD.UI.FileDialogs,
  ERD.UI.ReplayBar,
  ERD.UI.TrendChart,
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
    SampleCount: Integer;
  end;

  TCSVSeries = record
    Name: string;
    UnitText: string;
    Times: TArray<Int64>;
    Values: TArray<Double>;
    MinValue: Double;
    MaxValue: Double;
  end;

  TfrmPageRecordings = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FRecordings: TList<TRecordingInfo>;
    FFullSessionRecording: Boolean;
    FFullSessionStarted: TDateTime;
    FUpdatingGrid: Boolean;
    FUpdatingRecordSwitch: Boolean;
    FTitle: TOBDDialogText;
    FSummary: TOBDDialogText;
    FSearchEdit: TOBDEdit;
    FRefreshButton: TOBDButton;
    FFolderButton: TOBDButton;
    FRecordSessionSwitch: TOBDCheckBox;
    FReplayCard: TOBDCard;
    replayBar: TOBDReplayBar;
    FRecordingsCard: TOBDCard;
    FRecordingGrid: TOBDDataGrid;
    FRenameButton: TOBDButton;
    FDeleteButton: TOBDButton;
    FExportButton: TOBDButton;
    FShowButton: TOBDButton;
    FDetailsCard: TOBDCard;
    FDetailsList: TOBDKeyValueList;
    FChartCard: TOBDCard;
    chartReplay: TOBDTrendChart;
    tmrReplay: TTimer;
    tmrSession: TTimer;
    procedure BuildUI;
    function NewText(AStyle: TOBDDialogTextStyle): TOBDDialogText;
    function NewButton(AParent: TWinControl; AKind: TOBDButtonKind;
      AGlyph: TOBDGlyph): TOBDButton;
    procedure LayoutControls;
    procedure ConfigureGridColumns;
    procedure RefreshRecordings;
    procedure ApplyFilter;
    procedure AddGridRow(AIndex: Integer; const AInfo: TRecordingInfo);
    procedure AnalyzeCSV(const AFileName: string; out AInfo: TRecordingInfo);
    procedure LoadRecording(AIndex: Integer);
    procedure LoadCSVIntoChart(const AFileName: string);
    procedure UpdateDetails(const AInfo: TRecordingInfo);
    procedure ClearDetails;
    procedure ClearReplay;
    procedure UpdateActionButtons;
    procedure SearchChanged(Sender: TObject);
    procedure RefreshClicked(Sender: TObject);
    procedure FolderClicked(Sender: TObject);
    procedure RecordSessionChanged(Sender: TObject);
    procedure RenameClicked(Sender: TObject);
    procedure DeleteClicked(Sender: TObject);
    procedure ExportClicked(Sender: TObject);
    procedure OpenFolderClicked(Sender: TObject);
    procedure RecordingGridChanged(Sender: TObject);
    procedure RecordingGridActivated(Sender: TObject);
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
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PageKey: string;
    procedure Init(const AHost: IPersonalHost);
    procedure Activated(AActive: Boolean);
    procedure ConnectionChanged(AConnected: Boolean);
    procedure UnitsChanged;
    procedure Translate;
    // IPersonalCommands
    function Commands: TPersonalCommands;
    function CommandEnabled(ACommand: TPersonalCommand): Boolean;
    function CommandChecked(ACommand: TPersonalCommand): Boolean;
    procedure ExecuteCommand(ACommand: TPersonalCommand);
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
    Result := TrFmt('personal.recordings.sizeMB', '%.1f MB',
      [ASize / (1024 * 1024)])
  else if ASize > 1024 then
    Result := TrFmt('personal.recordings.sizeKB', '%.1f KB', [ASize / 1024])
  else
    Result := TrFmt('personal.recordings.sizeB', '%d B', [ASize]);
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

function ChannelSummary(const AInfo: TRecordingInfo): string;
begin
  Result := TrFmt('personal.recordings.channelSummary', '%d channels',
    [AInfo.ChannelCount]);
  if AInfo.Channels <> '' then
    Result := Result + ': ' + AInfo.Channels;
end;

{ TfrmPageRecordings }

constructor TfrmPageRecordings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;
  FRecordings := TList<TRecordingInfo>.Create;
  BuildUI;
  Translate;
  ClearDetails;
  ClearReplay;
end;

destructor TfrmPageRecordings.Destroy;
begin
  StopFullSessionRecording;
  FRecordings.Free;
  inherited Destroy;
end;

function TfrmPageRecordings.NewText(AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := Self;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

function TfrmPageRecordings.NewButton(AParent: TWinControl;
  AKind: TOBDButtonKind; AGlyph: TOBDGlyph): TOBDButton;
begin
  Result := TOBDButton.Create(Self);
  Result.Parent := AParent;
  Result.Kind := AKind;
  Result.Glyph := AGlyph;
  Result.AutoSize := False;
end;

procedure TfrmPageRecordings.BuildUI;
begin
  Align := alClient;

  FTitle := NewText(dtsHeading);
  FSummary := NewText(dtsSubtle);
  FSearchEdit := TOBDEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.OnChange := SearchChanged;
  FRefreshButton := NewButton(Self, bkSecondary, glRefresh);
  FRefreshButton.OnClick := RefreshClicked;
  FFolderButton := NewButton(Self, bkSecondary, glOpen);
  FFolderButton.OnClick := FolderClicked;
  FRecordSessionSwitch := TOBDCheckBox.Create(Self);
  FRecordSessionSwitch.Parent := Self;
  FRecordSessionSwitch.Style := csSwitch;
  FRecordSessionSwitch.OnChange := RecordSessionChanged;

  FReplayCard := TOBDCard.Create(Self);
  FReplayCard.Parent := Self;
  FReplayCard.StatusEdge := ceAccent;
  replayBar := TOBDReplayBar.Create(Self);
  replayBar.Parent := FReplayCard;
  replayBar.OnPlay := ReplayPlay;
  replayBar.OnPause := ReplayPause;
  replayBar.OnSeek := ReplaySeek;

  FRecordingsCard := TOBDCard.Create(Self);
  FRecordingsCard.Parent := Self;
  FRecordingsCard.StatusEdge := ceNeutral;
  FRecordingGrid := TOBDDataGrid.Create(Self);
  FRecordingGrid.Parent := FRecordingsCard;
  FRecordingGrid.EmptyGlyph := glRecord;
  FRecordingGrid.BorderVisible := True;
  FRecordingGrid.OnChange := RecordingGridChanged;
  FRecordingGrid.OnItemActivate := RecordingGridActivated;
  FRecordingGrid.OnEmptyAction := RefreshClicked;

  FRenameButton := NewButton(FRecordingsCard, bkSecondary, glEdit);
  FRenameButton.OnClick := RenameClicked;
  FExportButton := NewButton(FRecordingsCard, bkSecondary, glExport);
  FExportButton.OnClick := ExportClicked;
  FShowButton := NewButton(FRecordingsCard, bkSecondary, glOpen);
  FShowButton.OnClick := OpenFolderClicked;
  FDeleteButton := NewButton(FRecordingsCard, bkDangerOutline, glClear);
  FDeleteButton.OnClick := DeleteClicked;

  FDetailsCard := TOBDCard.Create(Self);
  FDetailsCard.Parent := Self;
  FDetailsCard.StatusEdge := ceNeutral;
  FDetailsList := TOBDKeyValueList.Create(Self);
  FDetailsList.Parent := FDetailsCard;
  FDetailsList.KeyWidth := 118;
  FDetailsList.WordWrap := True;

  FChartCard := TOBDCard.Create(Self);
  FChartCard.Parent := Self;
  FChartCard.StatusEdge := ceNeutral;
  chartReplay := TOBDTrendChart.Create(Self);
  chartReplay.Parent := FChartCard;
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

  ConfigureGridColumns;
  LayoutControls;
end;

procedure TfrmPageRecordings.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmPageRecordings.ConfigureGridColumns;
var
  Column: TOBDDataGridColumn;
begin
  FRecordingGrid.DataColumns.BeginUpdate;
  try
    FRecordingGrid.DataColumns.Clear;
    Column := FRecordingGrid.DataColumns.Add;
    Column.Caption := Tr('personal.recordings.columnDate', 'Date');
    Column.Width := 126;
    Column.Kind := dckMono;
    Column := FRecordingGrid.DataColumns.Add;
    Column.Caption := Tr('personal.recordings.columnVehicle', 'Vehicle');
    Column.Width := 170;
    Column := FRecordingGrid.DataColumns.Add;
    Column.Caption := Tr('personal.recordings.columnDuration', 'Duration');
    Column.Width := 78;
    Column.Alignment := taRightJustify;
    Column.Kind := dckMono;
    Column := FRecordingGrid.DataColumns.Add;
    Column.Caption := Tr('personal.recordings.columnSizeSamples',
      'Size / samples');
    Column.Width := 120;
    Column.Alignment := taRightJustify;
    Column := FRecordingGrid.DataColumns.Add;
    Column.Caption := Tr('personal.recordings.columnChannels', 'Channels');
    Column.Width := 0;
  finally
    FRecordingGrid.DataColumns.EndUpdate;
  end;
  FRecordingGrid.SortColumn := 0;
  FRecordingGrid.SortDescending := True;
end;

procedure TfrmPageRecordings.LayoutControls;
var
  Gap, HeaderH, ReplayH, ContentTop, ContentH, LeftW, RightX, RightW,
  DetailsH, ButtonTop, ButtonW, X, RightMin: Integer;
begin
  if FTitle = nil then
    Exit;
  Gap := 16;
  HeaderH := 88;
  ReplayH := 88;
  RightMin := 380;

  FTitle.SetBounds(24, 18, ClientWidth - 48, 28);
  X := ClientWidth - 24;
  FRecordSessionSwitch.SetBounds(X - 188, 22, 188, 34);
  X := FRecordSessionSwitch.Left - 8;
  FFolderButton.SetBounds(X - 112, 22, 112, 34);
  X := FFolderButton.Left - 8;
  FRefreshButton.SetBounds(X - 96, 22, 96, 34);
  X := FRefreshButton.Left - 8;
  FSearchEdit.SetBounds(Max(280, X - 240), 22, Min(240, Max(120, X - 280)), 34);
  FSummary.SetBounds(24, 48, Max(240, FSearchEdit.Left - 40), 34);

  FReplayCard.SetBounds(24, HeaderH, ClientWidth - 48, ReplayH);
  replayBar.SetBounds(16, 18, FReplayCard.Width - 32, FReplayCard.Height - 36);

  ContentTop := HeaderH + ReplayH + Gap;
  ContentH := Max(240, ClientHeight - ContentTop - 24);
  if ClientWidth >= 1040 then
  begin
    LeftW := Max(500, Min(640, (ClientWidth - 64) div 2));
    RightX := 24 + LeftW + Gap;
    RightW := ClientWidth - RightX - 24;
  end
  else
  begin
    LeftW := Max(360, ClientWidth - RightMin - 72);
    if LeftW > 560 then
      LeftW := 560;
    RightX := 24 + LeftW + Gap;
    RightW := ClientWidth - RightX - 24;
    if RightW < RightMin then
    begin
      LeftW := Max(320, ClientWidth - RightMin - 72);
      RightX := 24 + LeftW + Gap;
      RightW := ClientWidth - RightX - 24;
    end;
  end;

  FRecordingsCard.SetBounds(24, ContentTop, LeftW, ContentH);
  ButtonTop := FRecordingsCard.Height - 48;
  ButtonW := (FRecordingsCard.Width - 56) div 4;
  FRecordingGrid.SetBounds(16, 62, FRecordingsCard.Width - 32,
    Max(120, ButtonTop - 70));
  FRenameButton.SetBounds(16, ButtonTop, ButtonW, 32);
  FExportButton.SetBounds(FRenameButton.Left + ButtonW + 8, ButtonTop,
    ButtonW, 32);
  FShowButton.SetBounds(FExportButton.Left + ButtonW + 8, ButtonTop,
    ButtonW, 32);
  FDeleteButton.SetBounds(FShowButton.Left + ButtonW + 8, ButtonTop,
    ButtonW, 32);

  DetailsH := 176;
  FDetailsCard.SetBounds(RightX, ContentTop, RightW, DetailsH);
  FDetailsList.SetBounds(16, 62, FDetailsCard.Width - 32,
    FDetailsCard.Height - 78);
  FChartCard.SetBounds(RightX, ContentTop + DetailsH + Gap, RightW,
    Max(160, ContentH - DetailsH - Gap));
  chartReplay.SetBounds(16, 62, FChartCard.Width - 32,
    FChartCard.Height - 78);
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
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.Activated(AActive: Boolean);
begin
  if AActive then
    RefreshRecordings
  else
    tmrReplay.Enabled := False;
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
    StopFullSessionRecording;
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.UnitsChanged;
begin
  if chartReplay <> nil then
    chartReplay.Invalidate;
end;

procedure TfrmPageRecordings.Translate;
begin
  FTitle.Caption := Tr('personal.recordings.title', 'Recordings');
  FSearchEdit.TextHint := Tr('personal.recordings.searchHint',
    'Search recordings');
  FRefreshButton.Caption := Tr('personal.recordings.refresh', 'Refresh');
  FFolderButton.Caption := Tr('personal.recordings.folder', 'Open folder');
  FRenameButton.Caption := Tr('personal.recordings.rename', 'Rename');
  FExportButton.Caption := Tr('personal.recordings.export', 'Export copy');
  FShowButton.Caption := Tr('personal.recordings.show', 'Show file');
  FDeleteButton.Caption := Tr('personal.recordings.delete', 'Delete');
  if FFullSessionRecording then
    FRecordSessionSwitch.Caption := Tr('personal.recordings.stopRecord',
      'Stop recording')
  else
    FRecordSessionSwitch.Caption := Tr('personal.recordings.recordNow',
      'Record session');
  FReplayCard.Title := Tr('personal.recordings.replayTitle', 'Replay');
  FRecordingsCard.Title := Tr('personal.recordings.browserTitle',
    'Recorded drives');
  FDetailsCard.Title := Tr('personal.recordings.detailsTitle',
    'Recording details');
  FChartCard.Title := Tr('personal.recordings.chartTitle', 'Signals');
  FRecordingGrid.EmptyTitle := Tr('personal.recordings.emptyTitle',
    'No recordings yet');
  FRecordingGrid.EmptyHint := Tr('personal.recordings.emptyHint',
    'Record a drive or export live data as CSV to replay it here.');
  FRecordingGrid.EmptyAction := Tr('personal.recordings.refresh', 'Refresh');
  ConfigureGridColumns;
  UpdateActionButtons;
  LayoutControls;
end;

function TfrmPageRecordings.RecordingsFolder: string;
begin
  if FHost <> nil then
    Result := FHost.Settings.DataSubFolder('Recordings')
  else
    Result := '';
end;

procedure TfrmPageRecordings.RefreshRecordings;
var
  Files: TArray<string>;
  FileName: string;
  Info: TRecordingInfo;
  Folder: string;
begin
  Folder := RecordingsFolder;
  if Folder = '' then
  begin
    FSummary.Caption := Tr('personal.recordings.summaryUnavailable',
      'Recording folder is not available yet.');
    Exit;
  end;
  FRecordings.Clear;
  Files := TDirectory.GetFiles(Folder, '*.csv');
  for FileName in Files do
  begin
    AnalyzeCSV(FileName, Info);
    FRecordings.Add(Info);
  end;
  ApplyFilter;
  FSummary.Caption := TrFmt('personal.recordings.summary',
    '%d CSV recordings in %s.', [FRecordings.Count, Folder]);
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
  AInfo.SampleCount := 0;
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
    AInfo.SampleCount := LastData;
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
  if FRecordingGrid = nil then
    Exit;
  Filter := LowerCase(Trim(FSearchEdit.Text));
  FUpdatingGrid := True;
  FRecordingGrid.BeginUpdate;
  try
    FRecordingGrid.Clear;
    for I := 0 to FRecordings.Count - 1 do
    begin
      Haystack := LowerCase(FRecordings[I].DisplayName + ' ' +
        FRecordings[I].Channels);
      if (Filter = '') or (Pos(Filter, Haystack) > 0) then
        AddGridRow(I, FRecordings[I]);
    end;
  finally
    FRecordingGrid.EndUpdate;
    FUpdatingGrid := False;
  end;
  if SelectedIndex < 0 then
  begin
    ClearDetails;
    ClearReplay;
  end;
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.AddGridRow(AIndex: Integer;
  const AInfo: TRecordingInfo);
var
  Row: Integer;
  SizeSamples: string;
begin
  SizeSamples := TrFmt('personal.recordings.sizeSamples', '%s · %d samples',
    [FormatSize(AInfo.SizeBytes), AInfo.SampleCount]);
  Row := FRecordingGrid.AddRow([
    FormatDateTime(Tr('personal.recordings.dateFormat', 'yyyy-mm-dd hh:nn'),
      AInfo.Modified),
    AInfo.DisplayName,
    FormatDuration(AInfo.DurationSec),
    SizeSamples,
    ChannelSummary(AInfo)]);
  FRecordingGrid.RowData[Row] := AIndex;
end;

function TfrmPageRecordings.SelectedIndex: Integer;
var
  Row: Integer;
begin
  Result := -1;
  if FRecordingGrid = nil then
    Exit;
  Row := FRecordingGrid.SelectedRow;
  if Row < 0 then
    Exit;
  Result := FRecordingGrid.RowData[Row];
  if (Result < 0) or (Result >= FRecordings.Count) then
    Result := -1;
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

procedure TfrmPageRecordings.ClearReplay;
begin
  if replayBar <> nil then
  begin
    replayBar.Playing := False;
    replayBar.FileName := Tr('personal.recordings.noSelectionTitle',
      'No recording selected');
    replayBar.InfoText := Tr('personal.recordings.noSelectionText',
      'Select a CSV recording to inspect and replay it.');
    replayBar.DurationMs := 0;
    replayBar.PositionMs := 0;
    replayBar.ClearMarkers;
  end;
  if tmrReplay <> nil then
    tmrReplay.Enabled := False;
  if chartReplay <> nil then
  begin
    chartReplay.Source := nil;
    chartReplay.Channels.Clear;
    chartReplay.ClearSamples;
  end;
end;

procedure TfrmPageRecordings.LoadRecording(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FRecordings.Count) then
    Exit;
  replayBar.FileName := FRecordings[AIndex].DisplayName;
  replayBar.InfoText := TrFmt('personal.recordings.replayInfo',
    '%d channels · %s · %s', [FRecordings[AIndex].ChannelCount,
    FormatDuration(FRecordings[AIndex].DurationSec),
    FormatSize(FRecordings[AIndex].SizeBytes)]);
  replayBar.DurationMs := Round(FRecordings[AIndex].DurationSec * 1000);
  replayBar.PositionMs := 0;
  replayBar.ClearMarkers;
  if replayBar.DurationMs > 0 then
  begin
    replayBar.AddMarker(replayBar.DurationMs div 4, rpbBookmark,
      Tr('personal.recordings.marker25', '25%'));
    replayBar.AddMarker(replayBar.DurationMs div 2, rpbNote,
      Tr('personal.recordings.marker50', '50%'));
    replayBar.AddMarker((replayBar.DurationMs * 3) div 4, rpbDtc,
      Tr('personal.recordings.marker75', '75%'));
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
      chartReplay.AddSampleAt(Col, Series[Col].Times[Row],
        Series[Col].Values[Row]);
  end;
end;

procedure TfrmPageRecordings.ClearDetails;
var
  Category: string;
begin
  if FDetailsList = nil then
    Exit;
  Category := Tr('personal.recordings.detailsCategory', 'Details');
  FDetailsList.BeginUpdate;
  try
    FDetailsList.Clear;
    FDetailsList.Add(Category, Tr('personal.recordings.selectionLabel',
      'Selection'), Tr('personal.recordings.selectionEmpty',
      'Select a recording to show its metadata.'));
  finally
    FDetailsList.EndUpdate;
  end;
  FDetailsCard.StatusEdge := ceNeutral;
end;

procedure TfrmPageRecordings.UpdateDetails(const AInfo: TRecordingInfo);
var
  Category: string;
begin
  Category := Tr('personal.recordings.detailsCategory', 'Details');
  FDetailsList.BeginUpdate;
  try
    FDetailsList.Clear;
    FDetailsList.Add(Category, Tr('personal.recordings.fileLabel', 'File'),
      AInfo.FileName, kvkMono);
    FDetailsList.Add(Category, Tr('personal.recordings.modifiedLabel',
      'Started / modified'), FormatDateTime(Tr('personal.recordings.dateFormat',
      'yyyy-mm-dd hh:nn'), AInfo.Modified), kvkMono);
    FDetailsList.Add(Category, Tr('personal.recordings.durationLabel',
      'Duration'), FormatDuration(AInfo.DurationSec), kvkMono);
    FDetailsList.Add(Category, Tr('personal.recordings.sizeLabel', 'Size'),
      FormatSize(AInfo.SizeBytes), kvkMono);
    FDetailsList.Add(Category, Tr('personal.recordings.samplesLabel',
      'Samples'), IntToStr(AInfo.SampleCount), kvkMono);
    FDetailsList.Add(Category, Tr('personal.recordings.channelsLabel',
      'Channels'), ChannelSummary(AInfo));
  finally
    FDetailsList.EndUpdate;
  end;
  FDetailsCard.StatusEdge := ceAccent;
end;

procedure TfrmPageRecordings.UpdateActionButtons;
var
  HasSelection: Boolean;
begin
  HasSelection := SelectedFileName <> '';
  FRenameButton.Enabled := HasSelection;
  FDeleteButton.Enabled := HasSelection;
  FExportButton.Enabled := HasSelection;
  FShowButton.Enabled := FHost <> nil;
  FRefreshButton.Enabled := FHost <> nil;
  FFolderButton.Enabled := FHost <> nil;
  FUpdatingRecordSwitch := True;
  try
    FRecordSessionSwitch.Enabled := Connected or FFullSessionRecording;
    FRecordSessionSwitch.Checked := FFullSessionRecording;
  finally
    FUpdatingRecordSwitch := False;
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
var
  Folder: string;
begin
  Folder := RecordingsFolder;
  if Folder <> '' then
    ShellExecute(Handle, 'open', PChar(Folder), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmPageRecordings.RecordSessionChanged(Sender: TObject);
begin
  if FUpdatingRecordSwitch then
    Exit;
  if FFullSessionRecording then
    StopFullSessionRecording
  else
    StartFullSessionRecording;
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.RenameClicked(Sender: TObject);
var
  OldName, NewName, NewFile: string;
begin
  OldName := SelectedFileName;
  if OldName = '' then
    Exit;
  NewName := TPath.GetFileNameWithoutExtension(OldName);
  if not OBDInputQuery(Tr('personal.recordings.renameTitle',
    'Rename recording'), Tr('personal.recordings.renamePrompt', 'New name'),
    NewName) then
    Exit;
  NewName := Trim(NewName);
  if NewName = '' then
    Exit;
  if not SameText(ExtractFileExt(NewName), '.csv') then
    NewName := NewName + '.csv';
  NewFile := TPath.Combine(ExtractFilePath(OldName), NewName);
  if TFile.Exists(NewFile) then
  begin
    if FHost <> nil then
      FHost.Notify(tkWarning, Tr('personal.recordings.renameExists', 'Rename'),
        Tr('personal.recordings.renameExistsText',
        'A recording with that name already exists.'));
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
  if (FileName = '') or (FHost = nil) then
    Exit;
  if not FHost.Confirm(dkDanger, Tr('personal.recordings.deleteTitle',
    'Delete recording'), TrFmt('personal.recordings.deleteText',
    'Delete %s? This cannot be undone.', [ExtractFileName(FileName)]),
    Tr('personal.recordings.deleteConfirm', 'Delete')) then
    Exit;
  TFile.Delete(FileName);
  RefreshRecordings;
end;

procedure TfrmPageRecordings.ExportClicked(Sender: TObject);
var
  Dialog: TOBDSaveDialog;
  FileName: string;
begin
  FileName := SelectedFileName;
  if FileName = '' then
    Exit;
  Dialog := TOBDSaveDialog.Create(Self);
  try
    Dialog.Title := Tr('personal.recordings.exportTitle', 'Export recording');
    Dialog.Filter := Tr('personal.recordings.csvFilter',
      'CSV files (*.csv)|*.csv|All files (*.*)|*.*');
    Dialog.DefaultExt := 'csv';
    Dialog.FileName := ExtractFileName(FileName);
    Dialog.InitialDir := ExtractFilePath(FileName);
    Dialog.Options := [foOverwritePrompt, foPathMustExist];
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
  ShellExecute(Handle, 'open', 'explorer.exe', PChar(Params), nil,
    SW_SHOWNORMAL);
end;

procedure TfrmPageRecordings.RecordingGridChanged(Sender: TObject);
begin
  if FUpdatingGrid then
    Exit;
  LoadRecording(SelectedIndex);
  UpdateActionButtons;
end;

procedure TfrmPageRecordings.RecordingGridActivated(Sender: TObject);
begin
  LoadRecording(SelectedIndex);
end;

procedure TfrmPageRecordings.ReplayPlay(Sender: TObject);
begin
  replayBar.Playing := True;
  tmrReplay.Enabled := replayBar.DurationMs > 0;
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
  if FHost = nil then
    Exit;
  if not Connected then
  begin
    FHost.Notify(tkInfo, Tr('personal.recordings.connectTitle',
      'Record session'), Tr('personal.recordings.connectText',
      'Connect to a vehicle before recording a full session.'));
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
  if (FHost <> nil) and (FHost.Session <> nil) and
    FHost.Session.Recorder.Active then
    FHost.Session.Recorder.Close;
  FFullSessionRecording := False;
  if tmrSession <> nil then
    tmrSession.Enabled := False;
  if FHost <> nil then
    FHost.SetRecording('');
  Translate;
end;

procedure TfrmPageRecordings.UpdateSessionRecordingStatus;
var
  Seconds: Integer;
begin
  if (not FFullSessionRecording) or (FHost = nil) then
    Exit;
  Seconds := Round((Now - FFullSessionStarted) * SecsPerDay);
  if Seconds < 0 then
    Seconds := 0;
  FHost.SetRecording(TrFmt('personal.recordings.recordingStatus',
    'LOG %.2d:%.2d', [Seconds div 60, Seconds mod 60]));
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageRecordings.Commands: TPersonalCommands;
begin
  Result := [pcRecordSession, pcRefreshRecordings, pcRenameRecording,
    pcDeleteRecording, pcExportRecording, pcShowRecording];
end;

function TfrmPageRecordings.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  case ACommand of
    pcRecordSession:
      Result := Connected or FFullSessionRecording;
    pcRefreshRecordings, pcShowRecording:
      Result := FHost <> nil;
    pcRenameRecording, pcDeleteRecording, pcExportRecording:
      Result := SelectedFileName <> '';
  else
    Result := False;
  end;
end;

function TfrmPageRecordings.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := (ACommand = pcRecordSession) and FFullSessionRecording;
end;

procedure TfrmPageRecordings.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcRecordSession:
      RecordSessionChanged(Self);
    pcRefreshRecordings:
      RefreshClicked(Self);
    pcRenameRecording:
      RenameClicked(Self);
    pcDeleteRecording:
      DeleteClicked(Self);
    pcExportRecording:
      ExportClicked(Self);
    pcShowRecording:
      OpenFolderClicked(Self);
  end;
end;

end.
