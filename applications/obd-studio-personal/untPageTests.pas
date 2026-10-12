//------------------------------------------------------------------------------
//  untPageTests
//
//  Mode 06 monitor-test and in-use-performance page for OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageTests;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  ERD.I18n,
  ERD.Protocol,
  ERD.Protocol.Types,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  ERD.UI.Buttons,
  ERD.UI.ScrollBox,
  ERD.UI.Card,
  ERD.UI.Chips,
  ERD.UI.DataGrid,
  ERD.UI.KeyValueList,
  ERD.UI.Paint,
  ERD.Service.OnBoardMonitor,
  ERD.Service.VehicleInfo,
  untPersonalPage;

type
  TMode06UnitInfo = record
    UnitName: string;
    Scale: Double;
    Offset: Double;
  end;

  TMode06DisplayRow = record
    MID: Byte;
    TID: Byte;
    UASID: Byte;
    MIDName: string;
    TestName: string;
    UnitName: string;
    ValueText: string;
    MinText: string;
    MaxText: string;
    Passed: Boolean;
  end;

  TIPTDisplayRow = record
    ECU: string;
    Name: string;
    Completions: Word;
    Conditions: Word;
    Ratio: Double;
  end;

  TTestsSnapshot = record
    ModeRows: TArray<TMode06DisplayRow>;
    IPTRows: TArray<TIPTDisplayRow>;
    Notes: TArray<string>;
    FailedCount: Integer;
  end;

  TfrmPageTests = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FActive: Boolean;
    FReading: Boolean;
    FLastSnapshot: TTestsSnapshot;
    pnlTop: TPanel;
    lblTitle: TOBDDialogText;
    lblSummary: TOBDDialogText;
    btnRefresh: TOBDButton;
    btnExport: TOBDButton;
    Scroll: TOBDScrollBox;
    ModeCard: TOBDCard;
    ModeGrid: TOBDDataGrid;
    IPTCard: TOBDCard;
    IPTGrid: TOBDDataGrid;
    NotesBanner: TOBDBanner;
    procedure BuildUI;
    procedure ConfigureGrids;
    procedure RefreshClicked(Sender: TObject);
    procedure ExportClicked(Sender: TObject);
    procedure RefreshData;
    procedure ApplySnapshot(const ASnapshot: TTestsSnapshot);
    procedure ExportCSV;
    function Connected: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
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

const
  MODE06_SUPPORTED_SERVICE = OBD_MODE_ON_BOARD_MONITORING;

function HexByte(AValue: Byte): string;
begin
  Result := '0x' + IntToHex(AValue, 2);
end;

function CatalogFile(const ARelative: string): string;
var
  Base: string;
begin
  Base := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  Result := TPath.Combine(Base, ARelative);
  if TFile.Exists(Result) then
    Exit;
  Result := TPath.Combine(TDirectory.GetCurrentDirectory, ARelative);
end;

function ParseHexByte(const AText: string; out AValue: Byte): Boolean;
var
  N: Integer;
  S: string;
begin
  S := Trim(AText);
  if StartsText('0x', S) then
    S := '$' + Copy(S, 3, MaxInt);
  Result := TryStrToInt(S, N) and (N >= 0) and (N <= 255);
  if Result then
    AValue := Byte(N)
  else
    AValue := 0;
end;

procedure LoadNameCatalog(const AFileName: string;
  AMap: TDictionary<Byte, string>);
var
  Root: TJSONValue;
  Obj: TJSONObject;
  Entries: TJSONArray;
  Entry: TJSONValue;
  Id: Byte;
  IdText, NameText: string;
begin
  if not TFile.Exists(AFileName) then
    Exit;
  Root := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName,
    TEncoding.UTF8));
  try
    if not(Root is TJSONObject) then
      Exit;
    Entries := TJSONObject(Root).GetValue<TJSONArray>('entries');
    if Entries = nil then
      Exit;
    for Entry in Entries do
      if Entry is TJSONObject then
      begin
        Obj := TJSONObject(Entry);
        IdText := Obj.GetValue<string>('id', '');
        NameText := Obj.GetValue<string>('name', '');
        if (NameText <> '') and ParseHexByte(IdText, Id) then
          AMap.AddOrSetValue(Id, NameText);
      end;
  finally
    Root.Free;
  end;
end;

procedure LoadUnitCatalog(const AFileName: string;
  AMap: TDictionary<Byte, TMode06UnitInfo>);
var
  Root, Value: TJSONValue;
  Obj: TJSONObject;
  Entries: TJSONArray;
  Entry: TJSONValue;
  Id: Byte;
  Info: TMode06UnitInfo;
  FS: TFormatSettings;
begin
  if not TFile.Exists(AFileName) then
    Exit;
  FS := TFormatSettings.Invariant;
  Root := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName,
    TEncoding.UTF8));
  try
    if not(Root is TJSONObject) then
      Exit;
    Entries := TJSONObject(Root).GetValue<TJSONArray>('entries');
    if Entries = nil then
      Exit;
    for Entry in Entries do
      if Entry is TJSONObject then
      begin
        Obj := TJSONObject(Entry);
        if not ParseHexByte(Obj.GetValue<string>('ucsid', ''), Id) then
          Continue;
        Info := Default(TMode06UnitInfo);
        Info.UnitName := Obj.GetValue<string>('unit', 'raw');
        Info.Scale := 1.0;
        Info.Offset := 0.0;
        Value := Obj.GetValue('scale');
        if Value <> nil then
          Info.Scale := StrToFloatDef(Value.Value, 1.0, FS);
        Value := Obj.GetValue('offset');
        if Value <> nil then
          Info.Offset := StrToFloatDef(Value.Value, 0.0, FS);
        AMap.AddOrSetValue(Id, Info);
      end;
  finally
    Root.Free;
  end;
end;

function LookupName(AMap: TDictionary<Byte, string>; AID: Byte;
  const APrefix: string): string;
begin
  if not AMap.TryGetValue(AID, Result) then
    Result := APrefix + ' ' + HexByte(AID);
end;

function FormatScaled(AValue: SmallInt; const AInfo: TMode06UnitInfo): string;
var
  Scaled: Double;
begin
  Scaled := AValue * AInfo.Scale + AInfo.Offset;
  Result := FormatFloat('0.###', Scaled, TFormatSettings.Invariant);
end;

procedure AddNote(var ANotes: TArray<string>; const ANote: string);
var
  N: Integer;
begin
  if ANote = '' then
    Exit;
  N := Length(ANotes);
  SetLength(ANotes, N + 1);
  ANotes[N] := ANote;
end;

procedure AddByte(var ABytes: TBytes; AValue: Byte);
var
  N: Integer;
begin
  N := Length(ABytes);
  SetLength(ABytes, N + 1);
  ABytes[N] := AValue;
end;

function ReadSupportedMIDs(AProtocol: TOBDProtocol): TBytes;
var
  Base, MID: Integer;
  Resp: TOBDResponse;
  Payload: TBytes;
  I, ByteIndex: Integer;
  Mask: Byte;
  More: Boolean;
begin
  Result := nil;
  Base := 0;
  repeat
    More := False;
    try
      Resp := AProtocol.Request(MODE06_SUPPORTED_SERVICE, TBytes.Create(Byte(Base)));
      if Resp.IsNegative then
        Break;
      Payload := Resp.Data;
      if (Length(Payload) > 4) and (Payload[0] = Byte(Base)) then
        Payload := Copy(Payload, 1, 4)
      else
        Payload := Copy(Payload, 0, 4);
      if Length(Payload) < 4 then
        Break;
      for I := 0 to 31 do
      begin
        ByteIndex := I div 8;
        Mask := Byte($80 shr (I mod 8));
        if (Payload[ByteIndex] and Mask) <> 0 then
        begin
          MID := Base + I + 1;
          if (MID mod $20) = 0 then
            More := True
          else if MID <= $FF then
            AddByte(Result, Byte(MID));
        end;
      end;
    except
      Break;
    end;
    Inc(Base, $20);
  until (not More) or (Base > $E0);
end;

procedure AddModeRow(var ARows: TArray<TMode06DisplayRow>;
  const ARow: TMode06DisplayRow);
var
  N: Integer;
begin
  N := Length(ARows);
  SetLength(ARows, N + 1);
  ARows[N] := ARow;
end;

procedure AddIPTRow(var ARows: TArray<TIPTDisplayRow>;
  const ARow: TIPTDisplayRow);
var
  N: Integer;
begin
  N := Length(ARows);
  SetLength(ARows, N + 1);
  ARows[N] := ARow;
end;

function CsvEscape(const S: string): string;
begin
  Result := StringReplace(S, '"', '""', [rfReplaceAll]);
  if (Pos(',', Result) > 0) or (Pos('"', Result) > 0) or
    (Pos(#13, Result) > 0) or (Pos(#10, Result) > 0) then
    Result := '"' + Result + '"';
end;

{ TfrmPageTests }

constructor TfrmPageTests.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildUI;
end;

procedure TfrmPageTests.BuildUI;
begin
  Align := alClient;
  ParentBackground := False;

  pnlTop := TPanel.Create(Self);
  pnlTop.Parent := Self;
  pnlTop.Align := alTop;
  pnlTop.Height := 86;
  pnlTop.BevelOuter := bvNone;
  pnlTop.ParentBackground := False;
  pnlTop.ParentColor := True;
  pnlTop.ShowCaption := False;

  lblTitle := TOBDDialogText.Create(Self);
  lblTitle.Parent := pnlTop;
  lblTitle.Left := 16;
  lblTitle.Top := 8;
  lblTitle.Width := 360;
  lblTitle.Height := 28;
  lblTitle.Style := dtsHeading;

  lblSummary := TOBDDialogText.Create(Self);
  lblSummary.Parent := pnlTop;
  lblSummary.Left := 16;
  lblSummary.Top := 42;
  lblSummary.Width := 700;
  lblSummary.Height := 34;
  lblSummary.Style := dtsSubtle;
  lblSummary.WordWrap := True;

  btnRefresh := TOBDButton.Create(Self);
  btnRefresh.Parent := pnlTop;
  btnRefresh.Left := 780;
  btnRefresh.Top := 30;
  btnRefresh.Width := 120;
  btnRefresh.AutoSize := False;
  btnRefresh.Kind := bkPrimary;
  btnRefresh.OnClick := RefreshClicked;

  btnExport := TOBDButton.Create(Self);
  btnExport.Parent := pnlTop;
  btnExport.Left := 914;
  btnExport.Top := 30;
  btnExport.Width := 120;
  btnExport.AutoSize := False;
  btnExport.Kind := bkSecondary;
  btnExport.OnClick := ExportClicked;

  Scroll := TOBDScrollBox.Create(Self);
  Scroll.Parent := Self;
  Scroll.Align := alClient;

  NotesBanner := TOBDBanner.Create(Self);
  NotesBanner.Parent := Scroll;
  NotesBanner.Align := alTop;
  NotesBanner.Top := 0;
  NotesBanner.Height := 68;
  NotesBanner.Margins.SetBounds(16, 12, 16, 8);
  NotesBanner.AlignWithMargins := True;
  NotesBanner.Kind := bnInfo;

  ModeCard := TOBDCard.Create(Self);
  ModeCard.Parent := Scroll;
  ModeCard.Align := alTop;
  ModeCard.Top := 84;
  ModeCard.Height := 470;
  ModeCard.Margins.SetBounds(16, 8, 16, 8);
  ModeCard.AlignWithMargins := True;
  ModeCard.Padding.SetBounds(10, 10, 10, 10);

  ModeGrid := TOBDDataGrid.Create(Self);
  ModeGrid.Parent := ModeCard;
  ModeGrid.Align := alClient;
  ModeGrid.BorderVisible := False;

  IPTCard := TOBDCard.Create(Self);
  IPTCard.Parent := Scroll;
  IPTCard.Align := alTop;
  IPTCard.Top := 570;
  IPTCard.Height := 330;
  IPTCard.Margins.SetBounds(16, 8, 16, 16);
  IPTCard.AlignWithMargins := True;
  IPTCard.Padding.SetBounds(10, 10, 10, 10);

  IPTGrid := TOBDDataGrid.Create(Self);
  IPTGrid.Parent := IPTCard;
  IPTGrid.Align := alClient;
  IPTGrid.BorderVisible := False;

  ConfigureGrids;
end;

procedure TfrmPageTests.ConfigureGrids;
var
  Col: TOBDDataGridColumn;
begin
  ModeGrid.DataColumns.Clear;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'MID';
  Col.Width := 76;
  Col.Kind := dckMono;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Monitor';
  Col.Width := 180;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Test';
  Col.Width := 0;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Value';
  Col.Width := 96;
  Col.Kind := dckNumber;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Min';
  Col.Width := 86;
  Col.Kind := dckNumber;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Max';
  Col.Width := 86;
  Col.Kind := dckNumber;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Unit';
  Col.Width := 74;
  Col := ModeGrid.DataColumns.Add;
  Col.Caption := 'Result';
  Col.Width := 88;
  Col.Kind := dckChip;
  ModeGrid.EmptyGlyph := glInfo;

  IPTGrid.DataColumns.Clear;
  Col := IPTGrid.DataColumns.Add;
  Col.Caption := 'ECU';
  Col.Width := 110;
  Col.Kind := dckMono;
  Col := IPTGrid.DataColumns.Add;
  Col.Caption := 'Monitor';
  Col.Width := 0;
  Col := IPTGrid.DataColumns.Add;
  Col.Caption := 'Completed';
  Col.Width := 108;
  Col.Kind := dckNumber;
  Col := IPTGrid.DataColumns.Add;
  Col.Caption := 'Conditions';
  Col.Width := 108;
  Col.Kind := dckNumber;
  Col := IPTGrid.DataColumns.Add;
  Col.Caption := 'Ratio';
  Col.Width := 84;
  Col.Kind := dckNumber;
  IPTGrid.EmptyGlyph := glInfo;
end;

function TfrmPageTests.PageKey: string;
begin
  Result := PAGE_TESTS;
end;

procedure TfrmPageTests.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  Translate;
end;

procedure TfrmPageTests.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive then
    RefreshData;
end;

procedure TfrmPageTests.ConnectionChanged(AConnected: Boolean);
begin
  if not AConnected then
  begin
    ModeGrid.Clear;
    IPTGrid.Clear;
    if FHost <> nil then
      FHost.SetBadge(PAGE_TESTS, 0);
  end
  else if FActive then
    RefreshData;
end;

procedure TfrmPageTests.UnitsChanged;
begin
end;

procedure TfrmPageTests.Translate;
begin
  lblTitle.Caption := Tr('personal.tests.title', 'Monitor tests');
  lblSummary.Caption := Tr('personal.tests.summary',
    'Mode 06 on-board monitor results and in-use performance tracking.');
  btnRefresh.Caption := Tr('personal.tests.refresh', 'Read tests');
  btnExport.Caption := Tr('personal.tests.export', 'Export CSV');
  NotesBanner.Title := Tr('personal.tests.bannerTitle', 'On-board monitor tests');
  NotesBanner.Text := Tr('personal.tests.bannerText',
    'Failed Mode 06 rows are highlighted. Some vehicles do not publish every monitor or IPT counter.');
  ModeCard.Title := Tr('personal.tests.mode06Card',
    'Mode 06 monitor results');
  IPTCard.Title := Tr('personal.tests.iptCard',
    'In-use performance tracking (Mode 09)');
  ModeGrid.EmptyTitle := Tr('personal.tests.noMode06',
    'Read monitor tests to see Mode 06 results');
  ModeGrid.EmptyHint := Tr('personal.tests.noMode06Hint',
    'Unsupported monitors are skipped automatically.');
  IPTGrid.EmptyTitle := Tr('personal.tests.noIpt',
    'No in-use performance counters reported');
  IPTGrid.EmptyHint := Tr('personal.tests.noIptHint',
    'IPT is available only on vehicles that support Mode 09 InfoType 08 or 0B.');
  ConfigureGrids;
end;

function TfrmPageTests.Connected: Boolean;
begin
  Result := (FHost <> nil) and FHost.Connected;
end;

procedure TfrmPageTests.RefreshClicked(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmPageTests.ExportClicked(Sender: TObject);
begin
  ExportCSV;
end;

procedure TfrmPageTests.RefreshData;
var
  Snapshot: TTestsSnapshot;
begin
  if FReading or (FHost = nil) then
    Exit;
  btnRefresh.Enabled := False;
  Snapshot := Default(TTestsSnapshot);
  if not Connected then
  begin
    btnRefresh.Enabled := True;
    Exit;
  end;
  FReading := True;
  FHost.Session.RunJob(
    procedure
    var
      MidNames, TidNames: TDictionary<Byte, string>;
      Units: TDictionary<Byte, TMode06UnitInfo>;
      MIDs: TBytes;
      MID: Byte;
      Results: TArray<TOBDMonitorResult>;
      ResultItem: TOBDMonitorResult;
      ModeRow: TMode06DisplayRow;
      UnitInfo: TMode06UnitInfo;
      IPT: TArray<TOBDInUsePerformance>;
      IPTItem: TOBDInUsePerformance;
      IPTMonitor: TOBDIptMonitor;
      IPTRow: TIPTDisplayRow;
    begin
      MidNames := TDictionary<Byte, string>.Create;
      TidNames := TDictionary<Byte, string>.Create;
      Units := TDictionary<Byte, TMode06UnitInfo>.Create;
      try
        LoadNameCatalog(CatalogFile('catalogs/obd2/mids-mode06.json'), MidNames);
        LoadNameCatalog(CatalogFile('catalogs/obd2/tids-mode06.json'), TidNames);
        LoadUnitCatalog(CatalogFile('catalogs/mode06-units.json'), Units);
        MIDs := ReadSupportedMIDs(FHost.Session.Protocol);
        if Length(MIDs) = 0 then
          AddNote(Snapshot.Notes,
            Tr('personal.tests.mode06NoSupported',
            'The vehicle did not report a supported Mode 06 MID bitmap.'));
        for MID in MIDs do
        begin
          try
            Results := FHost.Session.OnBoardMonitor.Read(MID);
          except
            on E: Exception do
            begin
              AddNote(Snapshot.Notes, Format('MID %s: %s', [HexByte(MID),
                E.Message]));
              Continue;
            end;
          end;
          for ResultItem in Results do
          begin
            UnitInfo := Default(TMode06UnitInfo);
            UnitInfo.UnitName := 'raw';
            UnitInfo.Scale := 1.0;
            UnitInfo.Offset := 0.0;
            Units.TryGetValue(ResultItem.UnitAndScale, UnitInfo);
            ModeRow := Default(TMode06DisplayRow);
            ModeRow.MID := ResultItem.MID;
            ModeRow.TID := ResultItem.TID;
            ModeRow.UASID := ResultItem.UnitAndScale;
            ModeRow.MIDName := LookupName(MidNames, ResultItem.MID, 'MID');
            ModeRow.TestName := LookupName(TidNames, ResultItem.TID, 'TID');
            ModeRow.UnitName := UnitInfo.UnitName;
            ModeRow.ValueText := FormatScaled(ResultItem.TestValue, UnitInfo);
            ModeRow.MinText := FormatScaled(ResultItem.MinLimit, UnitInfo);
            ModeRow.MaxText := FormatScaled(ResultItem.MaxLimit, UnitInfo);
            ModeRow.Passed := (ResultItem.TestValue >= ResultItem.MinLimit) and
              (ResultItem.TestValue <= ResultItem.MaxLimit);
            if not ModeRow.Passed then
              Inc(Snapshot.FailedCount);
            AddModeRow(Snapshot.ModeRows, ModeRow);
          end;
        end;
        try
          IPT := FHost.Session.VehicleInfo.ReadInUsePerformance;
          for IPTItem in IPT do
            for IPTMonitor in IPTItem.Monitors do
            begin
              IPTRow := Default(TIPTDisplayRow);
              IPTRow.ECU := IPTItem.Ecu;
              if IPTRow.ECU = '' then
                IPTRow.ECU := '0x' + IntToHex(IPTItem.EcuId, 8);
              IPTRow.Name := IPTMonitor.Name;
              IPTRow.Completions := IPTMonitor.Completions;
              IPTRow.Conditions := IPTMonitor.Conditions;
              IPTRow.Ratio := IPTMonitor.Ratio;
              AddIPTRow(Snapshot.IPTRows, IPTRow);
            end;
        except
          on E: Exception do
            AddNote(Snapshot.Notes, TrFmt('personal.tests.iptFailed',
              'IPT not available: %s', [E.Message]));
        end;
      finally
        Units.Free;
        TidNames.Free;
        MidNames.Free;
      end;
    end,
    procedure(const AError: string)
    begin
      FReading := False;
      btnRefresh.Enabled := True;
      if AError <> '' then
      begin
        FHost.Notify(tkDanger, Tr('personal.tests.readFailed',
          'Monitor tests failed'), AError);
        Exit;
      end;
      ApplySnapshot(Snapshot);
    end);
end;

procedure TfrmPageTests.ApplySnapshot(const ASnapshot: TTestsSnapshot);
var
  RowIndex: Integer;
  ModeRow: TMode06DisplayRow;
  IPTRow: TIPTDisplayRow;
  Note, Text: string;
begin
  FLastSnapshot := ASnapshot;
  ModeGrid.BeginUpdate;
  try
    ModeGrid.Clear;
    for ModeRow in ASnapshot.ModeRows do
    begin
      RowIndex := ModeGrid.AddRow([HexByte(ModeRow.MID), ModeRow.MIDName,
        ModeRow.TestName, ModeRow.ValueText, ModeRow.MinText, ModeRow.MaxText,
        ModeRow.UnitName, IfThen(ModeRow.Passed,
        Tr('personal.tests.pass', 'Pass'), Tr('personal.tests.fail', 'Fail'))]);
      if ModeRow.Passed then
        ModeGrid.CellTone[7, RowIndex] := skSuccess
      else
      begin
        ModeGrid.CellTone[7, RowIndex] := skDanger;
        ModeGrid.RowAccent[RowIndex] := True;
      end;
    end;
  finally
    ModeGrid.EndUpdate;
  end;

  IPTGrid.BeginUpdate;
  try
    IPTGrid.Clear;
    for IPTRow in ASnapshot.IPTRows do
      IPTGrid.AddRow([IPTRow.ECU, IPTRow.Name, IntToStr(IPTRow.Completions),
        IntToStr(IPTRow.Conditions), FormatFloat('0.###', IPTRow.Ratio,
        TFormatSettings.Invariant)]);
  finally
    IPTGrid.EndUpdate;
  end;

  Text := '';
  for Note in ASnapshot.Notes do
  begin
    if Text <> '' then
      Text := Text + '  ';
    Text := Text + Note;
  end;
  if Text = '' then
    Text := Tr('personal.tests.bannerTextOk',
      'Mode 06 and IPT data were read from the vehicle.');
  NotesBanner.Text := Text;
  if ASnapshot.FailedCount > 0 then
    NotesBanner.Kind := bnDanger
  else if Length(ASnapshot.Notes) > 0 then
    NotesBanner.Kind := bnWarning
  else
    NotesBanner.Kind := bnSuccess;

  lblSummary.Caption := TrFmt('personal.tests.summaryCounts',
    '%d Mode 06 result(s), %d failed, %d IPT row(s).',
    [Length(ASnapshot.ModeRows), ASnapshot.FailedCount,
    Length(ASnapshot.IPTRows)]);
  if FHost <> nil then
    FHost.SetBadge(PAGE_TESTS, ASnapshot.FailedCount);
end;

procedure TfrmPageTests.ExportCSV;
var
  Lines: TStringList;
  Row: TMode06DisplayRow;
  IPTRow: TIPTDisplayRow;
  FileName, Dir: string;
begin
  if FHost = nil then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Add('Section,MID,TID,Monitor,Test,Value,Min,Max,Unit,Result,ECU,Completed,Conditions,Ratio');
    for Row in FLastSnapshot.ModeRows do
      Lines.Add(String.Join(',', [CsvEscape('Mode 06'), CsvEscape(HexByte(Row.MID)),
        CsvEscape(HexByte(Row.TID)), CsvEscape(Row.MIDName),
        CsvEscape(Row.TestName), CsvEscape(Row.ValueText), CsvEscape(Row.MinText),
        CsvEscape(Row.MaxText), CsvEscape(Row.UnitName),
        CsvEscape(IfThen(Row.Passed, 'Pass', 'Fail')), '', '', '', '']));
    for IPTRow in FLastSnapshot.IPTRows do
      Lines.Add(String.Join(',', [CsvEscape('IPT'), '', '', '', CsvEscape(IPTRow.Name),
        '', '', '', '', '', CsvEscape(IPTRow.ECU), IntToStr(IPTRow.Completions),
        IntToStr(IPTRow.Conditions), FormatFloat('0.###', IPTRow.Ratio,
        TFormatSettings.Invariant)]));
    Dir := FHost.Settings.DataSubFolder('Reports');
    FileName := TPath.Combine(Dir, FormatDateTime('yyyymmdd-hhnnss', Now) +
      '-tests.csv');
    Lines.SaveToFile(FileName, TEncoding.UTF8);
    FHost.Notify(tkSuccess, Tr('personal.tests.exported', 'CSV exported'),
      FileName);
  finally
    Lines.Free;
  end;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageTests.Commands: TPersonalCommands;
begin
  Result := [pcReadTests, pcExportTests];
end;

function TfrmPageTests.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  case ACommand of
    pcReadTests:
      Result := Connected and not FReading;
    pcExportTests:
      Result := (Length(FLastSnapshot.ModeRows) > 0) or
        (Length(FLastSnapshot.IPTRows) > 0);
  else
    Result := False;
  end;
end;

function TfrmPageTests.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageTests.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcReadTests:
      RefreshClicked(Self);
    pcExportTests:
      ExportClicked(Self);
  end;
end;

end.
