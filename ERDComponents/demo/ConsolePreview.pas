unit ConsolePreview;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  ERD.Components.Control, ERD.Components.Panel, ERD.Components.DebugConsole, ERD.Components.DebugModel,
  ERD.Components.Memo, ERD.Components.Ribbon;
type
  TConsolePreview = class(TERDPanel)
  private
    FConsole: TERDDebugConsole;
    FModel: TERDDebugModel;
    FStatus: TLabel;
    FDetails: TERDMemo;
    FTimer: TTimer;
    FTick: Integer;
    FRibbon: TERDRibbon;
    FTab: TERDRibbonTab;
    procedure Tick(Sender: TObject);
    procedure Command(Sender: TObject; Session: Int64; const Text: string);
    procedure Inspect(Sender: TObject; const Entry: TERDConsoleEntry);
    procedure NewSession;
    procedure RibbonClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachRibbon(Value: TERDRibbon);
    procedure SetRibbonActive(Value: Boolean);
    procedure ThemeChanged; override;
  end;
implementation
constructor TConsolePreview.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TERDDebugModel.Create(Self);
  FStatus := TLabel.Create(Self); FStatus.Parent := Self; FStatus.Align := alTop; FStatus.AutoSize := False; FStatus.Height := ScaleValue(28);
  FStatus.Caption := 'DEMO DATA - commands are handled by this demo only; no TSVM or shell execution.';
  FDetails := TERDMemo.Create(Self); FDetails.Parent := Self; FDetails.Align := alBottom; FDetails.Height := ScaleValue(140);
  FDetails.Mono := True; FDetails.ReadOnly := True; FDetails.Visible := False;
  FConsole := TERDDebugConsole.Create(Self); FConsole.Parent := Self; FConsole.Align := alClient;
  FConsole.DebugModel := FModel; FConsole.MaxEntries := 1000; FConsole.MaxTextChars := 262144; FConsole.ShowInput := True;
  FConsole.OnCommand := Command; FConsole.OnInspectEntry := Inspect;
  FTimer := TTimer.Create(Self); FTimer.Interval := 1000; FTimer.OnTimer := Tick;
  NewSession; ThemeChanged;
end;
destructor TConsolePreview.Destroy;
begin
  if FTimer <> nil then begin FTimer.Enabled := False; FTimer.OnTimer := nil; end;
  AttachRibbon(nil); if FConsole <> nil then begin FConsole.OnCommand := nil; FConsole.OnInspectEntry := nil; end;
  inherited;
end;
procedure TConsolePreview.NewSession;
begin
  FModel.BeginSession; FModel.SetExecution(dsRunning); FConsole.JumpToTail;
  FConsole.AddEntry(FModel.Session,Now,dlsInfo,'host','Synthetic session connected. Enter help for demo commands.');
end;
procedure TConsolePreview.ThemeChanged;
begin
  inherited; if FStatus <> nil then FStatus.Font.Color := Palette.ForegroundText;
  if FConsole <> nil then begin FConsole.Theme := Theme; FConsole.ThemeChanged; end;
  if FDetails <> nil then begin FDetails.Theme := Theme; FDetails.ThemeChanged; end;
end;
procedure TConsolePreview.Tick(Sender: TObject);
const Channels: array[0..3] of string = ('stdout','scheduler','host','stderr');
      Messages: array[0..4] of string = ('Trace observation','Worker scheduled','Status updated','Delayed response (demo)','Synthetic error'+#10+'  at updateStatus (demo.ts:4)'+#10+'  at <module> (demo.ts:7)');
begin
  if not Showing or (FModel.State = dsDisconnected) then Exit;
  Inc(FTick); FConsole.AddEntry(FModel.Session,Now,TERDDebugLogLevel(FTick mod 5),Channels[FTick mod 4],Messages[FTick mod 5]);
end;
procedure TConsolePreview.Command(Sender: TObject; Session: Int64; const Text: string);
begin
  FConsole.AddEntry(Session,Now,dlsInfo,'input',Text);
  if SameText(Text,'help') then FConsole.AddEntry(Session,Now,dlsInfo,'host','Demo commands: help, echo <text>, clear, session, disconnect')
  else if SameText(Text,'clear') then FConsole.Clear
  else if SameText(Text,'session') then NewSession
  else if SameText(Text,'disconnect') then begin FConsole.AddEntry(Session,Now,dlsInfo,'host','Synthetic disconnect'); FModel.SetExecution(dsDisconnected); end
  else if SameText(Copy(Text,1,5),'echo ') then FConsole.AddEntry(Session,Now,dlsInfo,'stdout',Copy(Text,6,MaxInt))
  else FConsole.AddEntry(Session,Now,dlsWarning,'host','Unknown demo command. Enter help. No real code was executed.');
end;
procedure TConsolePreview.Inspect(Sender: TObject; const Entry: TERDConsoleEntry);
begin
  FDetails.Text := 'Entry '+IntToStr(Entry.ID)+' | session '+IntToStr(Entry.Session)+' | '+Entry.Channel+sLineBreak+Entry.Text;
  if Entry.Truncated then FDetails.Text := FDetails.Text+sLineBreak+'[truncated by console retention limit]';
  FDetails.Visible := True;
end;
procedure TConsolePreview.RibbonClick(Sender: TObject);
var I: Integer;
begin
  case TERDRibbonItem(Sender).Tag of
    0: begin
      FConsole.BeginUpdate;
      try for I := 1 to 1000 do FConsole.AddEntry(FModel.Session,Now,TERDDebugLogLevel(I mod 5),'burst','Synthetic batch line '+IntToStr(I)); finally FConsole.EndUpdate; end;
    end;
    1: FConsole.Paused := not FConsole.Paused;
    2: FConsole.JumpToTail;
    3: FConsole.Clear;
    4: FConsole.ShowInput := not FConsole.ShowInput;
    5: NewSession;
    6: begin FConsole.AddEntry(FModel.Session,Now,dlsInfo,'host','Synthetic disconnect'); FModel.SetExecution(dsDisconnected); end;
    7: FTimer.Enabled := not FTimer.Enabled;
    8: FConsole.ClearCommandHistory;
    9: FDetails.Visible := False;
  end;
end;
procedure TConsolePreview.AttachRibbon(Value: TERDRibbon);
var Group: TERDRibbonGroup; Item: TERDRibbonItem; I: Integer;
const Captions: array[0..9] of string = ('Emit batch','Pause / resume','Follow latest','Clear log','Input on/off','New session','Disconnect','Sampling on/off','Clear commands','Hide details');
begin
  if FRibbon = Value then Exit;
  if FRibbon <> nil then begin FRibbon.RemoveFreeNotification(Self); FreeAndNil(FTab); end;
  FRibbon := Value; if Value = nil then Exit;
  Value.FreeNotification(Self); FTab := Value.Tabs.Add; FTab.Caption := 'Console (demo)'; FTab.Visible := False;
  Group := FTab.Groups.Add; Group.Caption := 'Synthetic log';
  for I := 0 to High(Captions) do
  begin
    if I = 4 then begin Group := FTab.Groups.Add; Group.Caption := 'Demo host'; end;
    Item := Group.Items.Add; Item.Caption := Captions[I]; Item.Size := risSmall; Item.Tag := I; Item.OnClick := RibbonClick;
  end;
end;
procedure TConsolePreview.SetRibbonActive(Value: Boolean);
var I, Index: Integer;
begin
  if (FRibbon = nil) or (FTab = nil) then Exit;
  Index := 0; for I := 0 to FTab.Index-1 do if FRibbon.Tabs[I].Visible then Inc(Index);
  if not Value and FTab.Visible and (FRibbon.ActiveTab = Index) then FRibbon.ActiveTab := 0;
  FTab.Visible := Value; if Value then FRibbon.ActiveTab := Index;
end;
procedure TConsolePreview.Notification(AComponent: TComponent; Operation: TOperation);
begin inherited; if (Operation = opRemove) and (AComponent = FRibbon) then begin FRibbon := nil; FTab := nil; end; end;
end.
