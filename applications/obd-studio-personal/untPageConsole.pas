//------------------------------------------------------------------------------
//  untPageConsole
//
//  Adapter console page of OBD-Studio Personal.
//------------------------------------------------------------------------------

unit untPageConsole;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  ERD.I18n,
  ERD.UI.Paint,
  ERD.UI.Dialogs,
  ERD.UI.Card,
  ERD.UI.Chips,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Tabs,
  ERD.UI.Terminal,
  ERD.UI.Memo,
  untPersonalPage;

type
  TConsoleCommandKeyEvent = procedure(Sender: TObject; var Key: Word;
    Shift: TShiftState) of object;

  TConsoleCommandEdit = class(TOBDEdit)
  strict private
    FOnCommandKey: TConsoleCommandKeyEvent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property OnCommandKey: TConsoleCommandKeyEvent read FOnCommandKey
      write FOnCommandKey;
  end;

  TfrmPageConsole = class(TFrame, IPersonalPage, IPersonalCommands)
  strict private
    FHost: IPersonalHost;
    FTitle: TOBDDialogText;
    FSubtitle: TOBDDialogText;
    FSaveButton: TOBDButton;
    FClearButton: TOBDButton;
    FTabs: TOBDTabs;
    FTerminalCard: TOBDCard;
    FTerminal: TOBDTerminal;
    FCommandEdit: TConsoleCommandEdit;
    FSendButton: TOBDButton;
    FQuickCard: TOBDCard;
    FQuickChips: array of TOBDChip;
    FQuickLabels: array of TOBDDialogText;
    FActivityMemo: TOBDMemo;
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FActive: Boolean;
    procedure BuildControls;
    function NewText(AParent: TWinControl; const ACaption: string;
      AStyle: TOBDDialogTextStyle): TOBDDialogText;
    procedure LayoutControls;
    procedure TabsChange(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure QuickCommandClick(Sender: TObject);
    procedure CommandKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SendCommand(const ACommand: string);
    procedure AddHistory(const ACommand: string);
    procedure HistoryPrevious;
    procedure HistoryNext;
    procedure RefreshActivity;
    procedure ShowTerminalTab;
    function TerminalLineText(const ALine: TOBDTerminalLine): string;
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

const
  QUICK_COUNT = 8;
  QUICK_COMMANDS: array[0..QUICK_COUNT - 1] of string = (
    'ATZ', 'ATI', 'ATRV', 'ATDP', '0100', '03', '0902', 'ATSP0');

{ TConsoleCommandEdit }

procedure TConsoleCommandEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnCommandKey) then
    FOnCommandKey(Self, Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

{ TfrmPageConsole }

constructor TfrmPageConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;
  FHistory := TStringList.Create;
  FHistoryIndex := -1;
  BuildControls;
  Translate;
end;

destructor TfrmPageConsole.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

function TfrmPageConsole.NewText(AParent: TWinControl; const ACaption: string;
  AStyle: TOBDDialogTextStyle): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Style := AStyle;
  Result.WordWrap := True;
end;

procedure TfrmPageConsole.BuildControls;
var
  I: Integer;
begin
  FTitle := NewText(Self, '', dtsHeading);
  FSubtitle := NewText(Self, '', dtsSubtle);

  FSaveButton := TOBDButton.Create(Self);
  FSaveButton.Parent := Self;
  FSaveButton.Kind := bkSecondary;
  FSaveButton.OnClick := SaveButtonClick;

  FClearButton := TOBDButton.Create(Self);
  FClearButton.Parent := Self;
  FClearButton.Kind := bkSecondary;
  FClearButton.OnClick := ClearButtonClick;

  FTabs := TOBDTabs.Create(Self);
  FTabs.Parent := Self;
  FTabs.Tabs.Add.Caption := 'Terminal';
  FTabs.Tabs.Add.Caption := 'Activity';
  FTabs.TabIndex := 0;
  FTabs.OnChange := TabsChange;

  FTerminalCard := TOBDCard.Create(Self);
  FTerminalCard.Parent := Self;
  FTerminalCard.ShowHeader := False;
  FTerminalCard.Padding.SetBounds(0, 0, 0, 0);

  FTerminal := TOBDTerminal.Create(Self);
  FTerminal.Parent := FTerminalCard;
  FTerminal.MaxLines := 5000;
  FTerminal.ShowTimestamps := True;

  FCommandEdit := TConsoleCommandEdit.Create(Self);
  FCommandEdit.Parent := FTerminalCard;
  FCommandEdit.Mono := True;
  FCommandEdit.CharCase := ecUpperCase;
  FCommandEdit.OnCommandKey := CommandKey;

  FSendButton := TOBDButton.Create(Self);
  FSendButton.Parent := FTerminalCard;
  FSendButton.Kind := bkPrimary;
  FSendButton.Glyph := glPlay;
  FSendButton.OnClick := SendButtonClick;

  FQuickCard := TOBDCard.Create(Self);
  FQuickCard.Parent := Self;
  FQuickCard.StatusEdge := ceAccent;

  SetLength(FQuickChips, QUICK_COUNT);
  SetLength(FQuickLabels, QUICK_COUNT);
  for I := 0 to QUICK_COUNT - 1 do
  begin
    FQuickChips[I] := TOBDChip.Create(Self);
    FQuickChips[I].Parent := FQuickCard;
    FQuickChips[I].Caption := QUICK_COMMANDS[I];
    FQuickChips[I].Hint := QUICK_COMMANDS[I];
    FQuickChips[I].Mono := True;
    FQuickChips[I].Kind := skAccent;
    FQuickChips[I].OnClick := QuickCommandClick;
    FQuickLabels[I] := NewText(FQuickCard, '', dtsSmall);
  end;

  FActivityMemo := TOBDMemo.Create(Self);
  FActivityMemo.Parent := Self;
  FActivityMemo.ReadOnly := True;
  FActivityMemo.Mono := True;
  FActivityMemo.WordWrap := False;
  FActivityMemo.Visible := False;
end;

procedure TfrmPageConsole.Resize;
begin
  inherited Resize;
  LayoutControls;
end;

procedure TfrmPageConsole.LayoutControls;
var
  W, H, TopY, RightW, Gap, BtnW, X, Y, RowH, InputH: Integer;
  I: Integer;
begin
  W := ClientWidth;
  H := ClientHeight;
  if W <= 0 then
    Exit;

  FTitle.SetBounds(24, 18, W div 2, 28);
  FSubtitle.SetBounds(24, 46, W div 2, 36);

  FClearButton.SetBounds(W - 24 - 96, 24, 96, 30);
  FSaveButton.SetBounds(FClearButton.Left - 116, 24, 108, 30);

  FTabs.SetBounds(24, 86, W - 48, 36);
  TopY := FTabs.Top + FTabs.Height + 12;
  Gap := 16;
  RightW := 320;
  if W < 880 then
    RightW := 260;

  if FTabs.TabIndex = 1 then
  begin
    FTerminalCard.Visible := False;
    FQuickCard.Visible := False;
    FActivityMemo.Visible := True;
    FActivityMemo.SetBounds(24, TopY, W - 48, H - TopY - 24);
    Exit;
  end;

  FActivityMemo.Visible := False;
  FTerminalCard.Visible := True;
  FQuickCard.Visible := True;
  FTerminalCard.SetBounds(24, TopY, W - 48 - RightW - Gap, H - TopY - 24);
  FQuickCard.SetBounds(W - 24 - RightW, TopY, RightW, H - TopY - 24);

  InputH := 54;
  BtnW := 92;
  FTerminal.SetBounds(14, 14, FTerminalCard.Width - 28,
    FTerminalCard.Height - InputH - 28);
  Y := FTerminalCard.Height - InputH + 12;
  FCommandEdit.SetBounds(16, Y, FTerminalCard.Width - 40 - BtnW, 30);
  FSendButton.SetBounds(FTerminalCard.Width - 16 - BtnW, Y, BtnW, 30);

  X := 16;
  Y := 66;
  RowH := 42;
  for I := 0 to QUICK_COUNT - 1 do
  begin
    FQuickChips[I].SetBounds(X, Y + 7, 58, 24);
    FQuickLabels[I].SetBounds(X + 70, Y + 2, FQuickCard.Width - X - 86,
      RowH - 4);
    Inc(Y, RowH);
  end;
end;

function TfrmPageConsole.PageKey: string;
begin
  Result := PAGE_CONSOLE;
end;

procedure TfrmPageConsole.Init(const AHost: IPersonalHost);
begin
  FHost := AHost;
  ConnectionChanged((FHost <> nil) and FHost.Connected);
end;

procedure TfrmPageConsole.Activated(AActive: Boolean);
begin
  FActive := AActive;
  if AActive and (FTabs.TabIndex = 1) then
    RefreshActivity
  else if AActive and FCommandEdit.Enabled then
    FCommandEdit.SetFocus;
end;

procedure TfrmPageConsole.ConnectionChanged(AConnected: Boolean);
var
  I: Integer;
begin
  FCommandEdit.Enabled := AConnected;
  FSendButton.Enabled := AConnected;
  for I := 0 to High(FQuickChips) do
    FQuickChips[I].Enabled := AConnected;
  if not AConnected then
    FCommandEdit.Text := '';
end;

procedure TfrmPageConsole.UnitsChanged;
begin
end;

procedure TfrmPageConsole.Translate;
const
  DESCRIPTIONS: array[0..QUICK_COUNT - 1] of string = (
    'Reset the adapter', 'Adapter version', 'Battery voltage',
    'Current protocol', 'Supported PIDs', 'Stored trouble codes',
    'Vehicle identification number', 'Automatic protocol');
  KEYS: array[0..QUICK_COUNT - 1] of string = (
    'reset', 'version', 'voltage', 'protocol', 'pids', 'codes', 'vin',
    'automatic');
var
  I: Integer;
begin
  FTitle.Caption := Tr('personal.console.title', 'Adapter console');
  FSubtitle.Caption := Tr('personal.console.subtitle',
    'Type AT, ST and OBD commands; the adapter answers as it is.');
  FSaveButton.Caption := Tr('personal.console.saveLog', 'Save log...');
  FClearButton.Caption := Tr('personal.console.clear', 'Clear');
  FTabs.Tabs[0].Caption := Tr('personal.console.tabTerminal', 'Terminal');
  FTabs.Tabs[1].Caption := Tr('personal.console.tabActivity', 'Activity');
  FQuickCard.Title := Tr('personal.console.quick', 'Quick commands');
  FCommandEdit.TextHint := Tr('personal.console.commandHint',
    'ATZ, 0100, 03...');
  FSendButton.Caption := Tr('personal.console.send', 'Send');
  for I := 0 to QUICK_COUNT - 1 do
    FQuickLabels[I].Caption := Tr('personal.console.quick.' + KEYS[I],
      DESCRIPTIONS[I]);
  LayoutControls;
end;

procedure TfrmPageConsole.TabsChange(Sender: TObject);
begin
  ShowTerminalTab;
  if (FTabs.TabIndex = 1) and FActive then
    RefreshActivity;
end;

procedure TfrmPageConsole.ShowTerminalTab;
begin
  LayoutControls;
end;

procedure TfrmPageConsole.SendButtonClick(Sender: TObject);
begin
  SendCommand(FCommandEdit.Text);
end;

procedure TfrmPageConsole.ClearButtonClick(Sender: TObject);
begin
  if FTabs.TabIndex = 1 then
    RefreshActivity
  else
    FTerminal.ClearLog;
end;

procedure TfrmPageConsole.SaveButtonClick(Sender: TObject);
var
  Dialog: TSaveDialog;
  Lines: TStringList;
  I: Integer;
begin
  if FHost = nil then
    Exit;
  Dialog := TSaveDialog.Create(Self);
  try
    Dialog.Title := Tr('personal.console.saveTitle', 'Save adapter log');
    Dialog.InitialDir := FHost.Settings.DataSubFolder('Logs');
    Dialog.DefaultExt := 'log';
    Dialog.Filter := Tr('personal.console.logFilter',
      'Log files (*.log)|*.log|Text files (*.txt)|*.txt|All files (*.*)|*.*');
    Dialog.FileName := 'adapter-console-' + FormatDateTime('yyyymmdd-hhnnss',
      Now) + '.log';
    if Dialog.Execute then
    begin
      Lines := TStringList.Create;
      try
        if FTabs.TabIndex = 1 then
          Lines.Assign(FActivityMemo.Lines)
        else
          for I := 0 to FTerminal.LineCount - 1 do
            Lines.Add(TerminalLineText(FTerminal.Line(I)));
        TFile.WriteAllText(Dialog.FileName, Lines.Text, TEncoding.UTF8);
      finally
        Lines.Free;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrmPageConsole.QuickCommandClick(Sender: TObject);
begin
  if Sender is TControl then
    SendCommand(TControl(Sender).Hint);
end;

procedure TfrmPageConsole.CommandKey(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        SendCommand(FCommandEdit.Text);
        Key := 0;
      end;
    VK_UP:
      begin
        HistoryPrevious;
        Key := 0;
      end;
    VK_DOWN:
      begin
        HistoryNext;
        Key := 0;
      end;
  end;
end;

procedure TfrmPageConsole.SendCommand(const ACommand: string);
var
  Command: string;
begin
  Command := Trim(ACommand);
  if Command = '' then
    Exit;
  if (FHost = nil) or not FHost.Connected then
  begin
    FTerminal.LogWarning(Tr('personal.console.offline',
      'Connect to an adapter before sending commands.'));
    Exit;
  end;

  AddHistory(Command);
  FCommandEdit.Text := '';
  FTerminal.LogSent(Command);
  FHost.Log('Console TX: ' + Command);
  FHost.Session.SendRaw(Command,
    procedure(AReply: string)
    var
      Lines: TStringList;
      I: Integer;
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := AReply;
        if Lines.Count = 0 then
          FTerminal.LogInfo(Tr('personal.console.noReply', '(no reply)'))
        else if AReply.StartsWith('? ') then
          for I := 0 to Lines.Count - 1 do
            FTerminal.LogError(Lines[I])
        else
          for I := 0 to Lines.Count - 1 do
            if Lines[I] <> '' then
              FTerminal.LogReceived(Lines[I]);
      finally
        Lines.Free;
      end;
    end);
end;

procedure TfrmPageConsole.AddHistory(const ACommand: string);
begin
  if (FHistory.Count = 0) or
    not SameText(FHistory[FHistory.Count - 1], ACommand) then
    FHistory.Add(ACommand);
  while FHistory.Count > 100 do
    FHistory.Delete(0);
  FHistoryIndex := -1;
end;

procedure TfrmPageConsole.HistoryPrevious;
begin
  if FHistory.Count = 0 then
    Exit;
  if FHistoryIndex < 0 then
    FHistoryIndex := FHistory.Count - 1
  else if FHistoryIndex > 0 then
    Dec(FHistoryIndex);
  FCommandEdit.Text := FHistory[FHistoryIndex];
  FCommandEdit.SelectAll;
end;

procedure TfrmPageConsole.HistoryNext;
begin
  if FHistoryIndex < 0 then
    Exit;
  if FHistoryIndex < FHistory.Count - 1 then
  begin
    Inc(FHistoryIndex);
    FCommandEdit.Text := FHistory[FHistoryIndex];
    FCommandEdit.SelectAll;
  end
  else
  begin
    FHistoryIndex := -1;
    FCommandEdit.Text := '';
  end;
end;

procedure TfrmPageConsole.RefreshActivity;
begin
  if FHost <> nil then
    FActivityMemo.Lines.Assign(FHost.ActivityLog);
end;

function TfrmPageConsole.TerminalLineText(
  const ALine: TOBDTerminalLine): string;
var
  Dir: string;
begin
  case ALine.Direction of
    tdSent:
      Dir := 'TX';
    tdReceived:
      Dir := 'RX';
    tdWarning:
      Dir := 'WARN';
    tdError:
      Dir := 'ERROR';
  else
    Dir := 'INFO';
  end;
  Result := FormatDateTime('hh:nn:ss.zzz', ALine.Timestamp) + '  ' + Dir +
    '  ' + ALine.Text;
end;

{ Commands ------------------------------------------------------------------ }

function TfrmPageConsole.Commands: TPersonalCommands;
begin
  Result := [pcClearConsole, pcSaveConsole];
end;

function TfrmPageConsole.CommandEnabled(ACommand: TPersonalCommand): Boolean;
begin
  Result := (FHost <> nil) and (ACommand in Commands);
end;

function TfrmPageConsole.CommandChecked(ACommand: TPersonalCommand): Boolean;
begin
  Result := False;
end;

procedure TfrmPageConsole.ExecuteCommand(ACommand: TPersonalCommand);
begin
  case ACommand of
    pcClearConsole:
      ClearButtonClick(Self);
    pcSaveConsole:
      SaveButtonClick(Self);
  end;
end;

end.
