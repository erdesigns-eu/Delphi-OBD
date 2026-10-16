unit ERD.Components.DockHost;
{$B-}
interface
uses System.Classes, System.SysUtils, System.Types, System.Math, System.JSON,
  System.Generics.Collections, Winapi.Windows, Winapi.Messages, Vcl.Controls,
  Vcl.Forms, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Menus,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint,
  ERD.Components.Panel, ERD.Components.Tabs, ERD.Components.TitleBar,
  ERD.Components.Buttons, ERD.Components.Menus;
type
  TERDDockZone = (dzLeft, dzRight, dzTop, dzBottom, dzCenter);
  TERDDockMode = (dpmDocked, dpmFloating, dpmAutoHide, dpmHidden);
  TERDDockHost = class;
  TERDDockGroup = class;
  TERDDockFloatWindow = class;
  TERDDockPanel = class
  private
    FHost: TERDDockHost;
    FID, FCaption: string;
    FControl, FOriginalParent: TWinControl;
    FOriginalAlign: TAlign;
    FOriginalAnchors: TAnchors;
    FOriginalBounds, FFloatBounds: TRect;
    FOriginalVisible: Boolean;
    FOriginalTabOrder, FSerial: Integer;
    FZone: TERDDockZone;
    FMode, FRestoreMode: TERDDockMode;
    FClosable: Boolean;
    FQueuedPoint: TPoint;
    procedure SetCaption(const Value: string);
  public
    property ID: string read FID;
    property Caption: string read FCaption write SetCaption;
    property Control: TWinControl read FControl;
    property Zone: TERDDockZone read FZone;
    property Mode: TERDDockMode read FMode;
    property Closable: Boolean read FClosable write FClosable;
  end;
  TERDDockPanelEvent = procedure(Sender: TObject; const PanelID: string) of object;
  TERDDockTabs = class(TERDTabs)
  private
    FHost: TERDDockHost;
    FZone: TERDDockZone;
    FDownSerial: Integer;
    FStart: TPoint;
    FDragging: Boolean;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;
  TERDDockGroup = class(TERDPanel)
  private
    FHost: TERDDockHost;
    FZone: TERDDockZone;
    FTabs: TERDDockTabs;
    FBody: TERDPanel;
    procedure TabChanged(Sender: TObject);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  TERDDockRail = class(TERDCustomControl)
  private
    FHost: TERDDockHost;
    FZone: TERDDockZone;
    FIDs: TArray<string>;
    FOffset: Integer;
    function SlotRect(Index: Integer): TRect;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    function PanelAt(const Point: TPoint): string;
  end;
  TERDDockGrip = class(TERDCustomControl)
  private
    FHost: TERDDockHost;
    FZone: TERDDockZone;
    FDown: Boolean;
    FStart: TPoint;
    FSize: Integer;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;
  TERDDockPreview = class(TERDCustomControl)
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  end;
  TERDDockFloatWindow = class(TForm)
  private
    FHost: TERDDockHost;
    FSerial: Integer;
    FTitle: TERDTitleBar;
    FTabs: TERDDockTabs;
    FBody: TERDPanel;
    FReturn: TERDButton;
    procedure WindowClose(Sender: TObject; var Action: TCloseAction);
    procedure ReturnClick(Sender: TObject);
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  public
    constructor CreatePanel(Host: TERDDockHost; Panel: TERDDockPanel);
    procedure UpdateAppearance;
    destructor Destroy; override;
  end;
  /// Five tabbed dock groups with borrowed content, floating windows and auto-hide rails.
  TERDDockHost = class(TERDCustomControl)
  private
    FPanels: TObjectList<TERDDockPanel>;
    FFloats: TObjectDictionary<string,TERDDockFloatWindow>;
    FGroups: array[TERDDockZone] of TERDDockGroup;
    FRails: array[dzLeft..dzBottom] of TERDDockRail;
    FGrips: array[dzLeft..dzBottom] of TERDDockGrip;
    FSizes: array[dzLeft..dzBottom] of Integer;
    FActive: array[TERDDockZone] of string;
    FParking: TERDPanel;
    FPeek: TERDDockGroup;
    FPeekID, FMenuID, FHoverID: string;
    FPreview: TERDDockPreview;
    FTimer: TTimer;
    FMenu: TERDPopupMenu;
    FNextSerial, FUpdateDepth: Integer;
    FUpdating, FShuttingDown, FDirty: Boolean;
    FHoverTick, FLeaveTick: UInt64;
    FWork: TRect;
    FOnLayoutChanged: TNotifyEvent;
    FOnPanelActivated: TERDDockPanelEvent;
    procedure Changed;
    procedure Rebuild;
    procedure SyncContent;
    procedure RestoreControl(Panel: TERDDockPanel);
    procedure SetLeftSize(Value: Integer);
    procedure SetRightSize(Value: Integer);
    procedure SetTopSize(Value: Integer);
    procedure SetBottomSize(Value: Integer);
    procedure SetSideSize(Zone: TERDDockZone; Value: Integer);
    function GetLeftSize: Integer;
    function GetRightSize: Integer;
    function GetTopSize: Integer;
    function GetBottomSize: Integer;
    function FindSerial(Serial: Integer): TERDDockPanel;
    procedure QueueDrop(Serial: Integer; const ScreenPoint: TPoint);
    procedure DockMessage(var Message: TMessage); message WM_APP+$4D1;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure Tick(Sender: TObject);
    procedure MenuOpening(Sender: TObject);
    procedure MenuAction(Sender: TObject);
    procedure UpdateFloatVisibility;
    function ClampFloat(const Bounds: TRect): TRect;
    function GetPanel(Index: Integer): TERDDockPanel;
    function GetPanelCount: Integer;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    function AddPanel(const ID, Caption: string; Control: TWinControl; Zone: TERDDockZone = dzCenter): TERDDockPanel;
    function FindPanel(const ID: string): TERDDockPanel;
    function RemovePanel(const ID: string): Boolean;
    function DockPanel(const ID: string; Zone: TERDDockZone; TabIndex: Integer = -1): Boolean;
    function FloatPanel(const ID: string; const ScreenBounds: TRect): Boolean;
    function AutoHidePanel(const ID: string; Zone: TERDDockZone): Boolean;
    function HidePanel(const ID: string): Boolean;
    function ShowPanel(const ID: string): Boolean;
    function ActivatePanel(const ID: string): Boolean;
    function PeekPanel(const ID: string): Boolean;
    procedure ClosePeek;
    function ActivePanel(Zone: TERDDockZone): string;
    function GroupBounds(Zone: TERDDockZone): TRect;
    function FloatingWindow(const ID: string): TCustomForm;
    function DropZoneAt(const ScreenPoint: TPoint; out Zone: TERDDockZone): Boolean;
    function DropPanel(const ID: string; const ScreenPoint: TPoint): Boolean;
    procedure PreviewDrop(const ScreenPoint: TPoint);
    procedure CancelDrag;
    procedure BeginUpdate;
    procedure EndUpdate;
    function SaveLayout: string;
    function LoadLayout(const JSON: string; out ErrorText: string; IgnoreUnknown: Boolean = True): Boolean;
    property PanelCount: Integer read GetPanelCount;
    property Panels[Index: Integer]: TERDDockPanel read GetPanel;
    property PeekPanelID: string read FPeekID;
  published
    property LeftSize: Integer read GetLeftSize write SetLeftSize default 220;
    property RightSize: Integer read GetRightSize write SetRightSize default 220;
    property TopSize: Integer read GetTopSize write SetTopSize default 160;
    property BottomSize: Integer read GetBottomSize write SetBottomSize default 180;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnPanelActivated: TERDDockPanelEvent read FOnPanelActivated write FOnPanelActivated;
  end;
implementation
const WM_ERDDOCK = WM_APP+$4D1;
procedure TERDDockPanel.SetCaption(const Value: string);
begin if FCaption = Value then Exit; FCaption := Value; FHost.Changed; end;
constructor TERDDockGroup.Create(AOwner: TComponent);
begin
  inherited; FHost := AOwner as TERDDockHost;
  FTabs := TERDDockTabs.Create(Self); FTabs.Parent := Self; FTabs.FHost := FHost;
  FTabs.TabStyle := tsDocument; FTabs.OnChange := TabChanged;
  FBody := TERDPanel.Create(Self); FBody.Parent := Self; FBody.BorderWidth := 0;
  Visible := False;
end;
procedure TERDDockGroup.Resize;
begin
  inherited; if (FTabs = nil) or (FBody = nil) then Exit;
  FTabs.SetBounds(ScaleValue(1),ScaleValue(1),Max(0,ClientWidth-ScaleValue(2)),FTabs.Height);
  FBody.SetBounds(ScaleValue(1),FTabs.Top+FTabs.Height,Max(0,ClientWidth-ScaleValue(2)),Max(0,ClientHeight-FTabs.Height-ScaleValue(2)));
end;
procedure TERDDockGroup.TabChanged(Sender: TObject);
var Panel: TERDDockPanel;
begin
  if (FHost = nil) or FHost.FUpdating or FHost.FShuttingDown then Exit;
  if (FTabs.TabIndex < 0) or (FTabs.TabIndex >= FTabs.Tabs.Count) then Exit;
  Panel := FHost.FindSerial(FTabs.Tabs[FTabs.TabIndex].Tag); if Panel <> nil then FHost.ActivatePanel(Panel.ID);
end;
procedure TERDDockTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var I: Integer;
begin
  inherited; if (FHost = nil) or FHost.FShuttingDown then Exit;
  if (TabIndex >= 0) and (TabIndex < Tabs.Count) then
  begin
    FHost.FMenuID := ''; if FHost.FindSerial(Tabs[TabIndex].Tag) <> nil then FHost.FMenuID := FHost.FindSerial(Tabs[TabIndex].Tag).ID;
  end;
  if Button <> mbLeft then Exit;
  for I := 0 to Tabs.Count-1 do if PtInRect(TabBounds(I),Point(X,Y)) then
  begin FDownSerial := Tabs[I].Tag; FStart := ClientToScreen(Point(X,Y)); FDragging := False; MouseCapture := True; Break; end;
end;
procedure TERDDockTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  inherited; if (FDownSerial = 0) or (FHost = nil) or FHost.FShuttingDown then Exit;
  P := ClientToScreen(Point(X,Y));
  if (Abs(P.X-FStart.X) >= ScaleValue(5)) or (Abs(P.Y-FStart.Y) >= ScaleValue(5)) then FDragging := True;
  if FDragging then begin Cursor := crSizeAll; FHost.PreviewDrop(P); end;
end;
procedure TERDDockTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Serial: Integer; WasDragging: Boolean; P: TPoint;
begin
  Serial := FDownSerial; WasDragging := FDragging; P := ClientToScreen(Point(X,Y));
  FDownSerial := 0; FDragging := False; MouseCapture := False; Cursor := crDefault;
  inherited;
  if (Button = mbLeft) and WasDragging and (FHost <> nil) and not FHost.FShuttingDown then FHost.QueueDrop(Serial,P);
end;
procedure TERDDockTabs.WMCaptureChanged(var Message: TMessage);
begin
  inherited; FDownSerial := 0; FDragging := False; Cursor := crDefault;
  if (FHost <> nil) and not FHost.FShuttingDown then FHost.CancelDrag;
end;
procedure TERDDockTabs.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (FDownSerial <> 0) then
  begin FDownSerial := 0; FDragging := False; MouseCapture := False; FHost.CancelDrag; Key := 0; Exit; end;
  inherited;
end;
function TERDDockRail.SlotRect(Index: Integer): TRect;
var N: Integer;
begin
  N := Index-FOffset;
  if FZone in [dzLeft,dzRight] then Result := Rect(0,N*ScaleValue(120),ClientWidth,(N+1)*ScaleValue(120))
  else Result := Rect(N*ScaleValue(120),0,(N+1)*ScaleValue(120),ClientHeight);
end;
function TERDDockRail.PanelAt(const Point: TPoint): string;
var I: Integer;
begin
  Result := ''; if not PtInRect(ClientRect,Point) then Exit;
  for I := FOffset to High(FIDs) do if PtInRect(SlotRect(I),Point) then Exit(FIDs[I]);
end;
procedure TERDDockRail.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; I: Integer; R: TRect; Panel: TERDDockPanel;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    Painter.FillRect(ClientRect,Palette.Background);
    for I := FOffset to High(FIDs) do
    begin
      R := SlotRect(I); if (R.Top >= ClientHeight) or (R.Left >= ClientWidth) then Break;
      Panel := FHost.FindPanel(FIDs[I]); if Panel = nil then Continue;
      Painter.FillRect(R,Palette.GaugeFace); Painter.FrameRect(R,Palette.NeutralLight);
      if FZone in [dzLeft,dzRight] then
      begin
        ACanvas.Font.Name := 'Segoe UI'; ACanvas.Font.Height := -ScaleValue(11); ACanvas.Font.Orientation := 900;
        ACanvas.Font.Color := Palette.ForegroundText; ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(R.Left+ScaleValue(6),R.Bottom-ScaleValue(8),Copy(Panel.Caption,1,16)); ACanvas.Font.Orientation := 0;
      end
      else Painter.TextF((R.Left+R.Right)/2,ClientHeight/2,Panel.Caption,11,Palette.ForegroundText,twRegular,taCenter,R.Width-ScaleValue(10));
    end;
  finally Painter.Free; end;
end;
procedure TERDDockRail.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ID: string;
begin inherited; if Button <> mbLeft then Exit; ID := PanelAt(Point(X,Y)); if ID <> '' then if FHost.PeekPanelID = ID then FHost.ClosePeek else FHost.PeekPanel(ID); end;
function TERDDockRail.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var Slots: Integer;
begin
  if FZone in [dzLeft,dzRight] then Slots := Max(1,ClientHeight div ScaleValue(120)) else Slots := Max(1,ClientWidth div ScaleValue(120));
  if WheelDelta > 0 then FOffset := Max(0,FOffset-1) else FOffset := Min(Max(0,Length(FIDs)-Slots),FOffset+1);
  Invalidate; Result := True;
end;
procedure TERDDockGrip.WMCaptureChanged(var Message: TMessage);
begin inherited; FDown := False; Invalidate; end;
procedure TERDDockGrip.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try Painter.FillRect(ClientRect,Palette.Background); if FDown then Painter.FillRect(ClientRect,Palette.Accent) else
    if FZone in [dzLeft,dzRight] then Painter.LineF(ClientWidth/2,0,ClientWidth/2,ClientHeight,Palette.NeutralLight)
    else Painter.LineF(0,ClientHeight/2,ClientWidth,ClientHeight/2,Palette.NeutralLight);
  finally Painter.Free; end;
end;
procedure TERDDockGrip.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin inherited; if Button <> mbLeft then Exit; FDown := True; FStart := ClientToScreen(Point(X,Y)); FSize := FHost.FSizes[FZone]; MouseCapture := True; Invalidate; end;
procedure TERDDockGrip.MouseMove(Shift: TShiftState; X, Y: Integer);
var P: TPoint; Delta: Integer;
begin
  inherited; if not FDown then Exit; P := ClientToScreen(Point(X,Y));
  if FZone in [dzLeft,dzRight] then Delta := P.X-FStart.X else Delta := P.Y-FStart.Y;
  if FZone in [dzRight,dzBottom] then Delta := -Delta;
  FHost.SetSideSize(FZone,EnsureRange(FSize+MulDiv(Delta,96,FHost.CurrentPPI),48,8192));
end;
procedure TERDDockGrip.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin inherited; FDown := False; MouseCapture := False; Invalidate; end;
procedure TERDDockPreview.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try Painter.FillRect(ClientRect,Painter.Tint(Palette.Accent)); Painter.FrameRect(ClientRect,Painter.AccentText);
    Painter.TextF(ClientWidth/2,ClientHeight/2,Caption,12,Palette.ForegroundText,twSemibold,taCenter,ClientWidth-ScaleValue(12));
  finally Painter.Free; end;
end;
constructor TERDDockFloatWindow.CreatePanel(Host: TERDDockHost; Panel: TERDDockPanel);
begin
  inherited CreateNew(Host); FHost := Host; FSerial := Panel.FSerial; Position := poDesigned;
  BorderStyle := bsSizeable; PopupMode := pmExplicit; PopupParent := GetParentForm(Host);
  FTitle := TERDTitleBar.Create(Self); FTitle.Parent := Self; FTitle.Align := alTop;
  FTabs := TERDDockTabs.Create(Self); FTabs.Parent := Self; FTabs.Align := alTop; FTabs.FHost := Host; FTabs.PopupMenu := Host.FMenu;
  with FTabs.Tabs.Add do begin Caption := Panel.Caption; Tag := Panel.FSerial; Closable := False; end; FTabs.TabIndex := 0;
  FReturn := TERDButton.Create(Self); FReturn.Parent := Self; FReturn.Align := alBottom; FReturn.Caption := 'Dock panel'; FReturn.OnClick := ReturnClick;
  FBody := TERDPanel.Create(Self); FBody.Parent := Self; FBody.Align := alClient; FBody.BorderWidth := 0;
  OnClose := WindowClose; BoundsRect := Host.ClampFloat(Panel.FFloatBounds); UpdateAppearance;
end;
destructor TERDDockFloatWindow.Destroy;
var Panel: TERDDockPanel;
begin
  if (FHost <> nil) and not FHost.FShuttingDown then
  begin
    Panel := FHost.FindSerial(FSerial);
    if (Panel <> nil) and (Panel.Control <> nil) and (Panel.Control.Parent = FBody) then
    begin Panel.Control.Visible := False; Panel.Control.Parent := FHost.FParking; end;
  end;
  inherited;
end;
procedure TERDDockFloatWindow.WindowClose(Sender: TObject; var Action: TCloseAction);
var Panel: TERDDockPanel; Command: Integer;
begin
  Action := caNone; if (FHost = nil) or FHost.FShuttingDown then Exit;
  Panel := FHost.FindSerial(FSerial); if Panel = nil then Exit; Command := 0; if not Panel.Closable then Command := 1;
  PostMessage(FHost.Handle,WM_ERDDOCK,FSerial,Command);
end;
procedure TERDDockFloatWindow.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var Panel: TERDDockPanel;
begin
  inherited;
  if (FHost = nil) or FHost.FShuttingDown or FHost.FUpdating or (FHost.FUpdateDepth > 0) or (csDestroying in ComponentState) then Exit;
  Panel := FHost.FindSerial(FSerial);
  if (Panel <> nil) and (Panel.Mode = dpmFloating) and (Panel.FFloatBounds <> BoundsRect) then
  begin Panel.FFloatBounds := BoundsRect; if Assigned(FHost.FOnLayoutChanged) then FHost.FOnLayoutChanged(FHost); end;
end;
procedure TERDDockFloatWindow.ReturnClick(Sender: TObject);
begin if (FHost <> nil) and not FHost.FShuttingDown then PostMessage(FHost.Handle,WM_ERDDOCK,FSerial,1); end;
procedure TERDDockFloatWindow.UpdateAppearance;
var Panel: TERDDockPanel;
begin
  if FHost = nil then Exit; Panel := FHost.FindSerial(FSerial); if Panel = nil then Exit;
  Caption := Panel.Caption; FTitle.Title := Caption; FTabs.Tabs[0].Caption := Caption;
  FTitle.Theme := FHost.Theme; FTabs.Theme := FHost.Theme; FBody.Theme := FHost.Theme; FReturn.Theme := FHost.Theme;
  FTitle.ParentDensity := False; FTitle.Density := FHost.Density; FTabs.ParentDensity := False; FTabs.Density := FHost.Density;
  FBody.ParentDensity := False; FBody.Density := FHost.Density; FReturn.ParentDensity := False; FReturn.Density := FHost.Density;
  Color := FHost.Palette.GaugeFace; Invalidate;
end;
constructor TERDDockHost.Create(AOwner: TComponent);
var Zone: TERDDockZone; I: Integer; Item: TMenuItem;
const Actions: array[0..7] of string = ('Dock left','Dock right','Dock top','Dock bottom','Dock center','Float','Auto hide / pin','Hide panel');
begin
  inherited; Width := 900; Height := 600; ControlStyle := ControlStyle+[csAcceptsControls]; TabStop := False;
  FSizes[dzLeft] := 220;
  FSizes[dzRight] := 220;
  FSizes[dzTop] := 160;
  FSizes[dzBottom] := 180;
  FPanels := TObjectList<TERDDockPanel>.Create(True); FFloats := TObjectDictionary<string,TERDDockFloatWindow>.Create([doOwnsValues]);
  FMenu := TERDPopupMenu.Create(Self); FMenu.OnPopup := MenuOpening;
  for I := 0 to High(Actions) do begin Item := TMenuItem.Create(FMenu); Item.Caption := Actions[I]; Item.Tag := I; Item.OnClick := MenuAction; FMenu.Items.Add(Item); end;
  FParking := TERDPanel.Create(Self); FParking.Parent := Self; FParking.Visible := False;
  for Zone := Low(TERDDockZone) to High(TERDDockZone) do
  begin
    FGroups[Zone] := TERDDockGroup.Create(Self); FGroups[Zone].Parent := Self; FGroups[Zone].FZone := Zone;
    FGroups[Zone].FTabs.FZone := Zone; FGroups[Zone].FTabs.PopupMenu := FMenu;
    if Zone = dzCenter then Continue;
    FRails[Zone] := TERDDockRail.Create(Self); FRails[Zone].Parent := Self; FRails[Zone].FHost := Self; FRails[Zone].FZone := Zone; FRails[Zone].Visible := False;
    FGrips[Zone] := TERDDockGrip.Create(Self); FGrips[Zone].Parent := Self; FGrips[Zone].FHost := Self; FGrips[Zone].FZone := Zone; FGrips[Zone].Visible := False;
    if Zone in [dzLeft,dzRight] then FGrips[Zone].Cursor := crHSplit else FGrips[Zone].Cursor := crVSplit;
  end;
  FPeek := TERDDockGroup.Create(Self); FPeek.Parent := Self; FPeek.FTabs.PopupMenu := FMenu;
  FPreview := TERDDockPreview.Create(Self); FPreview.Parent := Self; FPreview.Visible := False;
  FTimer := TTimer.Create(Self); FTimer.Interval := 100; FTimer.OnTimer := Tick;
  ThemeChanged; Resize;
end;
destructor TERDDockHost.Destroy;
var Panel: TERDDockPanel; Zone: TERDDockZone;
begin
  FShuttingDown := True; FUpdating := True;
  if FTimer <> nil then begin FTimer.Enabled := False; FTimer.OnTimer := nil; end;
  if FMenu <> nil then FMenu.OnPopup := nil;
  if FPanels <> nil then for Panel in FPanels do RestoreControl(Panel);
  if FFloats <> nil then for Zone := Low(TERDDockZone) to High(TERDDockZone) do if FGroups[Zone] <> nil then FGroups[Zone].FTabs.OnChange := nil;
  FreeAndNil(FFloats); FreeAndNil(FPanels); inherited;
end;
procedure TERDDockHost.GetChildren(Proc: TGetChildProc; Root: TComponent);
var I: Integer; Panel: TERDDockPanel;
begin
  if Root = Self then Exit;
  for I := 0 to ControlCount-1 do if Controls[I].Owner = Root then Proc(Controls[I]);
  if FPanels <> nil then for Panel in FPanels do if (Panel.Control <> nil) and (Panel.Control.Owner = Root) and (Panel.Control.Parent <> Self) then Proc(Panel.Control);
end;
function TERDDockHost.GetPanelCount: Integer;
begin Result := FPanels.Count; end;
function TERDDockHost.GetPanel(Index: Integer): TERDDockPanel;
begin Result := FPanels[Index]; end;
function TERDDockHost.FindPanel(const ID: string): TERDDockPanel;
var Panel: TERDDockPanel;
begin Result := nil; if FPanels = nil then Exit; for Panel in FPanels do if Panel.ID = ID then Exit(Panel); end;
function TERDDockHost.FindSerial(Serial: Integer): TERDDockPanel;
var Panel: TERDDockPanel;
begin Result := nil; if FPanels = nil then Exit; for Panel in FPanels do if Panel.FSerial = Serial then Exit(Panel); end;
function TERDDockHost.AddPanel(const ID, Caption: string; Control: TWinControl; Zone: TERDDockZone): TERDDockPanel;
var Panel: TERDDockPanel; ParentControl: TWinControl;
begin
  if (Trim(ID) = '') or (Length(ID) > 128) or (Control = nil) or (Ord(Zone) > Ord(High(TERDDockZone))) then raise EArgumentException.Create('Panel needs a stable ID, content control and valid zone');
  if (FindPanel(ID) <> nil) or (FPanels.Count >= 4096) then raise EArgumentException.Create('Duplicate panel ID or panel limit exceeded');
  ParentControl := Self; while ParentControl <> nil do begin if ParentControl = Control then raise EArgumentException.Create('Dock content cannot contain its host'); ParentControl := ParentControl.Parent; end;
  ParentControl := Control.Parent;
  while ParentControl <> nil do
  begin
    if ((ParentControl is TERDDockHost) and (ParentControl <> Self)) or (ParentControl is TERDDockFloatWindow) then raise EArgumentException.Create('Content already belongs to another dock host');
    ParentControl := ParentControl.Parent;
  end;
  for Panel in FPanels do if Panel.Control = Control then raise EArgumentException.Create('Control is already registered');
  if FNextSerial = High(Integer) then raise EInvalidOperation.Create('Dock serial space exhausted');
  Result := TERDDockPanel.Create; Result.FHost := Self; Result.FID := ID; Result.FCaption := Caption; Result.FControl := Control;
  Result.FOriginalParent := Control.Parent; Result.FOriginalAlign := Control.Align; Result.FOriginalAnchors := Control.Anchors; Result.FOriginalBounds := Control.BoundsRect;
  Result.FOriginalVisible := Control.Visible; Result.FOriginalTabOrder := Control.TabOrder; Result.FZone := Zone; Result.FClosable := True;
  Result.FFloatBounds := Rect(100,100,740,520); Inc(FNextSerial); Result.FSerial := FNextSerial;
  FPanels.Add(Result); Control.FreeNotification(Self); if (Control.Parent <> nil) and (Control.Parent <> Self) then Control.Parent.FreeNotification(Self);
  FActive[Zone] := ID; Changed;
end;
procedure TERDDockHost.RestoreControl(Panel: TERDDockPanel);
var Control, Target, Ancestor: TWinControl;
begin
  Control := Panel.Control; if Control = nil then Exit; Control.RemoveFreeNotification(Self);
  Target := Panel.FOriginalParent;
  if FShuttingDown then
  begin
    Ancestor := Target;
    while Ancestor <> nil do
    begin
      if (Ancestor = Self) or ((Ancestor is TERDDockFloatWindow) and (TERDDockFloatWindow(Ancestor).FHost = Self)) then begin Target := nil; Break; end;
      Ancestor := Ancestor.Parent;
    end;
  end;
  if (Target <> nil) and (csDestroying in Target.ComponentState) then Target := nil;
  Control.Align := alNone; Control.Visible := False; Control.Parent := Target;
  Control.BoundsRect := Panel.FOriginalBounds; Control.Anchors := Panel.FOriginalAnchors; Control.Align := Panel.FOriginalAlign;
  if Target <> nil then begin Control.TabOrder := Panel.FOriginalTabOrder; Control.Visible := Panel.FOriginalVisible; end;
  Panel.FControl := nil;
end;
function TERDDockHost.RemovePanel(const ID: string): Boolean;
var Panel: TERDDockPanel;
begin
  Panel := FindPanel(ID); Result := Panel <> nil; if not Result then Exit;
  if FPeekID = ID then FPeekID := ''; RestoreControl(Panel); FFloats.Remove(ID); FPanels.Remove(Panel); Changed;
end;
procedure TERDDockHost.Notification(AComponent: TComponent; Operation: TOperation);
var Panel: TERDDockPanel; Pair: TPair<string,TERDDockFloatWindow>; Affected: Boolean;
begin
  inherited; if (Operation <> opRemove) or FShuttingDown or (FPanels = nil) then Exit;
  Affected := False;
  if (AComponent is TERDDockFloatWindow) and (FFloats <> nil) then
  begin
    Panel := FindSerial(TERDDockFloatWindow(AComponent).FSerial);
    if (Panel <> nil) and (Panel.Mode = dpmFloating) and (Panel.Control <> nil) and not FUpdating and FFloats.TryGetValue(Panel.ID,Pair.Value) and (Pair.Value = AComponent) then
    begin Pair := FFloats.ExtractPair(Panel.ID); Panel.FMode := dpmDocked; Affected := True; end;
  end;
  for Panel in FPanels do
  begin
    if AComponent = Panel.FOriginalParent then Panel.FOriginalParent := nil;
    if AComponent = Panel.FControl then begin Panel.FControl := nil; if FPeekID = Panel.ID then FPeekID := ''; Panel.FMode := dpmHidden; Affected := True; end;
  end;
  // No reparenting from a control's destructor; rebuild after the notification unwinds.
  if Affected and not (csDestroying in ComponentState) and HandleAllocated then PostMessage(Handle,WM_ERDDOCK,0,9);
end;
procedure TERDDockHost.BeginUpdate;
begin Inc(FUpdateDepth); end;
procedure TERDDockHost.EndUpdate;
begin if FUpdateDepth = 0 then raise EInvalidOperation.Create('Unbalanced dock EndUpdate'); Dec(FUpdateDepth); if (FUpdateDepth = 0) and FDirty then Changed; end;
procedure TERDDockHost.Changed;
begin
  if FShuttingDown then Exit; FDirty := True; if (FUpdateDepth > 0) or FUpdating then Exit;
  FDirty := False; Rebuild; if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;
procedure TERDDockHost.Rebuild;
var Panel: TERDDockPanel; Zone: TERDDockZone; Window: TERDDockFloatWindow;
    Keys: TArray<string>; Key: string; I, Selected: Integer; Target: TWinControl;
begin
  if FUpdating or FShuttingDown or (FPanels = nil) or (FPeek = nil) then Exit;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then Exit;
  FUpdating := True;
  try
    for Panel in FPanels do if Panel.Control <> nil then
    begin
      if FFloats.TryGetValue(Panel.ID,Window) then Panel.FFloatBounds := Window.BoundsRect;
      if Panel.Mode <> dpmFloating then
      begin
        Target := FParking;
        if Panel.Mode = dpmDocked then Target := FGroups[Panel.Zone].FBody
        else if (Panel.Mode = dpmAutoHide) and (FPeekID = Panel.ID) then Target := FPeek.FBody;
        if Panel.Control.Parent <> Target then begin Panel.Control.Align := alNone; Panel.Control.Parent := Target; end;
        if Target = FParking then Panel.Control.Visible := False;
      end;
    end;
    Keys := FFloats.Keys.ToArray;
    for Key in Keys do begin Panel := FindPanel(Key); if (Panel = nil) or (Panel.Mode <> dpmFloating) or (Panel.Control = nil) then FFloats.Remove(Key); end;
    for Zone := Low(TERDDockZone) to High(TERDDockZone) do
    begin
      FGroups[Zone].FTabs.Tabs.BeginUpdate;
      try
        FGroups[Zone].FTabs.Tabs.Clear; Selected := -1;
        if Zone <> dzCenter then FRails[Zone].FIDs := nil;
        for Panel in FPanels do if (Panel.Control <> nil) and (Panel.Zone = Zone) then
        begin
          if Panel.Mode = dpmDocked then with FGroups[Zone].FTabs.Tabs.Add do
          begin Caption := Panel.Caption; Tag := Panel.FSerial; Closable := False; if FActive[Zone] = Panel.ID then Selected := Index; end;
          if (Panel.Mode = dpmAutoHide) and (Zone <> dzCenter) then
          begin I := Length(FRails[Zone].FIDs); SetLength(FRails[Zone].FIDs,I+1); FRails[Zone].FIDs[I] := Panel.ID; end;
        end;
      finally FGroups[Zone].FTabs.Tabs.EndUpdate; end;
      if (Selected < 0) and (FGroups[Zone].FTabs.Tabs.Count > 0) then Selected := 0;
      FGroups[Zone].FTabs.TabIndex := Selected;
      if Selected >= 0 then FActive[Zone] := FindSerial(FGroups[Zone].FTabs.Tabs[Selected].Tag).ID else FActive[Zone] := '';
      FGroups[Zone].Visible := Selected >= 0;
      if Zone <> dzCenter then begin FRails[Zone].Visible := Length(FRails[Zone].FIDs) > 0; FRails[Zone].FOffset := EnsureRange(FRails[Zone].FOffset,0,Max(0,Length(FRails[Zone].FIDs)-1)); FRails[Zone].Invalidate; FGrips[Zone].Visible := Selected >= 0; end;
    end;
    for Panel in FPanels do if (Panel.Control <> nil) and (Panel.Mode = dpmFloating) then
    begin
      if not FFloats.TryGetValue(Panel.ID,Window) then begin Window := TERDDockFloatWindow.CreatePanel(Self,Panel); FFloats.Add(Panel.ID,Window); Window.FreeNotification(Self); end;
      Window.UpdateAppearance; Panel.Control.Parent := Window.FBody; Panel.Control.Align := alClient; Panel.Control.Visible := True;
    end;
    Panel := FindPanel(FPeekID); FPeek.Visible := (Panel <> nil) and (Panel.Mode = dpmAutoHide) and (Panel.Control <> nil);
    FPeek.FTabs.Tabs.Clear;
    if FPeek.Visible then
    begin
      with FPeek.FTabs.Tabs.Add do begin Caption := Panel.Caption; Tag := Panel.FSerial; Closable := False; end;
      FPeek.FTabs.TabIndex := 0; Panel.Control.Parent := FPeek.FBody; Panel.Control.Align := alClient; Panel.Control.Visible := True;
    end else FPeekID := '';
    SyncContent; Resize; UpdateFloatVisibility;
  finally FUpdating := False; end;
  Invalidate;
end;
procedure TERDDockHost.SyncContent;
var Panel: TERDDockPanel;
begin
  for Panel in FPanels do if (Panel.Control <> nil) and (Panel.Mode = dpmDocked) then
  begin
    Panel.Control.Parent := FGroups[Panel.Zone].FBody; Panel.Control.Align := alClient; Panel.Control.Visible := FActive[Panel.Zone] = Panel.ID;
  end;
end;
function TERDDockHost.DockPanel(const ID: string; Zone: TERDDockZone; TabIndex: Integer): Boolean;
var Panel, Other: TERDDockPanel; Source, Destination, N, I: Integer;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Control <> nil) and (Ord(Zone) <= Ord(High(TERDDockZone))); if not Result then Exit;
  Source := FPanels.IndexOf(Panel); Destination := FPanels.Count; N := 0;
  if TabIndex >= 0 then for I := 0 to FPanels.Count-1 do
  begin Other := FPanels[I]; if (Other <> Panel) and (Other.Mode = dpmDocked) and (Other.Zone = Zone) then begin if N = TabIndex then begin Destination := I; Break; end; Inc(N); end; end;
  if Source < Destination then Dec(Destination); if Source <> Destination then FPanels.Move(Source,Destination);
  Panel.FZone := Zone; Panel.FMode := dpmDocked; Panel.FRestoreMode := dpmDocked; FActive[Zone] := ID;
  if FPeekID = ID then FPeekID := ''; Changed;
end;
function TERDDockHost.ClampFloat(const Bounds: TRect): TRect;
var Work: TRect; W, H: Integer;
begin
  Work := Screen.MonitorFromPoint(Bounds.TopLeft).WorkareaRect;
  W := EnsureRange(Bounds.Width,Min(ScaleValue(160),Work.Width),Work.Width); H := EnsureRange(Bounds.Height,Min(ScaleValue(120),Work.Height),Work.Height);
  Result.Left := EnsureRange(Bounds.Left,Work.Left,Work.Right-W); Result.Top := EnsureRange(Bounds.Top,Work.Top,Work.Bottom-H);
  Result.Right := Result.Left+W; Result.Bottom := Result.Top+H;
end;
function TERDDockHost.FloatPanel(const ID: string; const ScreenBounds: TRect): Boolean;
var Panel: TERDDockPanel; Window: TERDDockFloatWindow;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Control <> nil); if not Result then Exit;
  Panel.FFloatBounds := ClampFloat(ScreenBounds); Panel.FMode := dpmFloating; Panel.FRestoreMode := dpmFloating;
  if FFloats.TryGetValue(ID,Window) then Window.BoundsRect := Panel.FFloatBounds;
  if FPeekID = ID then FPeekID := ''; Changed;
end;
function TERDDockHost.AutoHidePanel(const ID: string; Zone: TERDDockZone): Boolean;
var Panel: TERDDockPanel;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Control <> nil) and (Ord(Zone) < Ord(dzCenter)); if not Result then Exit;
  Panel.FZone := Zone; Panel.FMode := dpmAutoHide; Panel.FRestoreMode := dpmAutoHide; if FPeekID = ID then FPeekID := ''; Changed;
end;
function TERDDockHost.HidePanel(const ID: string): Boolean;
var Panel: TERDDockPanel;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and Panel.Closable; if not Result then Exit;
  if Panel.Mode <> dpmHidden then Panel.FRestoreMode := Panel.Mode;
  Panel.FMode := dpmHidden; if FPeekID = ID then FPeekID := ''; Changed;
end;
function TERDDockHost.ShowPanel(const ID: string): Boolean;
var Panel: TERDDockPanel;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Control <> nil); if not Result then Exit;
  if Panel.Mode = dpmHidden then begin Panel.FMode := Panel.FRestoreMode; Changed; end;
  ActivatePanel(ID);
end;
function TERDDockHost.ActivatePanel(const ID: string): Boolean;
var Panel: TERDDockPanel; Window: TERDDockFloatWindow; I: Integer; PreviousActive: string;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Control <> nil) and (Panel.Mode <> dpmHidden); if not Result then Exit;
  PreviousActive := FActive[Panel.Zone];
  case Panel.Mode of
    dpmDocked: begin
      FActive[Panel.Zone] := ID; FUpdating := True;
      try for I := 0 to FGroups[Panel.Zone].FTabs.Tabs.Count-1 do if FGroups[Panel.Zone].FTabs.Tabs[I].Tag = Panel.FSerial then FGroups[Panel.Zone].FTabs.TabIndex := I; SyncContent; finally FUpdating := False; end;
    end;
    dpmFloating: if FFloats.TryGetValue(ID,Window) and Showing then begin Window.Show; Window.BringToFront; end;
    dpmAutoHide: PeekPanel(ID);
  end;
  if (Panel.Mode = dpmDocked) and (PreviousActive <> ID) and Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
  if Assigned(FOnPanelActivated) then FOnPanelActivated(Self,ID);
end;
function TERDDockHost.PeekPanel(const ID: string): Boolean;
var Panel: TERDDockPanel;
begin
  Panel := FindPanel(ID); Result := (Panel <> nil) and (Panel.Mode = dpmAutoHide) and (Panel.Control <> nil); if not Result then Exit;
  if FPeekID = ID then Exit; FPeekID := ID; FLeaveTick := 0; Rebuild;
end;
procedure TERDDockHost.ClosePeek;
begin if FPeekID = '' then Exit; FPeekID := ''; Rebuild; end;
function TERDDockHost.ActivePanel(Zone: TERDDockZone): string;
begin Result := ''; if Ord(Zone) <= Ord(High(TERDDockZone)) then Result := FActive[Zone]; end;
function TERDDockHost.GroupBounds(Zone: TERDDockZone): TRect;
begin Result := Rect(0,0,0,0); if (Ord(Zone) <= Ord(High(TERDDockZone))) and FGroups[Zone].Visible then Result := FGroups[Zone].BoundsRect; end;
function TERDDockHost.FloatingWindow(const ID: string): TCustomForm;
var Window: TERDDockFloatWindow;
begin Result := nil; if FFloats.TryGetValue(ID,Window) then Result := Window; end;
procedure TERDDockHost.UpdateFloatVisibility;
var Pair: TPair<string,TERDDockFloatWindow>;
begin if FFloats = nil then Exit; for Pair in FFloats do Pair.Value.Visible := Showing and not FShuttingDown; end;
procedure TERDDockHost.CMShowingChanged(var Message: TMessage);
begin inherited; if not FShuttingDown then begin UpdateFloatVisibility; if not Showing then ClosePeek; end; end;
function TERDDockHost.DropZoneAt(const ScreenPoint: TPoint; out Zone: TERDDockZone): Boolean;
var P: TPoint; Z: TERDDockZone; EdgeX, EdgeY: Integer;
begin
  P := ScreenToClient(ScreenPoint); Zone := dzCenter; Result := PtInRect(ClientRect,P); if not Result then Exit;
  EdgeX := Min(ScaleValue(72),ClientWidth div 4); EdgeY := Min(ScaleValue(72),ClientHeight div 4);
  if P.X < EdgeX then Zone := dzLeft else if P.X >= ClientWidth-EdgeX then Zone := dzRight
  else if P.Y < EdgeY then Zone := dzTop else if P.Y >= ClientHeight-EdgeY then Zone := dzBottom
  else for Z := dzLeft to dzBottom do if FGroups[Z].Visible and PtInRect(FGroups[Z].BoundsRect,P) then begin Zone := Z; Break; end;
end;
function TERDDockHost.DropPanel(const ID: string; const ScreenPoint: TPoint): Boolean;
var Zone: TERDDockZone; Panel: TERDDockPanel; P: TPoint; I, Target: Integer;
begin
  CancelDrag; Panel := FindPanel(ID); Result := Panel <> nil; if not Result then Exit;
  if DropZoneAt(ScreenPoint,Zone) then
  begin
    Target := -1; P := FGroups[Zone].FTabs.ScreenToClient(ScreenPoint);
    for I := 0 to FGroups[Zone].FTabs.Tabs.Count-1 do if PtInRect(FGroups[Zone].FTabs.TabBounds(I),P) then begin Target := I; Break; end;
    Result := DockPanel(ID,Zone,Target);
  end
  else Result := FloatPanel(ID,Rect(ScreenPoint.X-ScaleValue(80),ScreenPoint.Y-ScaleValue(16),ScreenPoint.X+ScaleValue(560),ScreenPoint.Y+ScaleValue(404)));
end;
procedure TERDDockHost.PreviewDrop(const ScreenPoint: TPoint);
var Zone: TERDDockZone; P: TPoint;
const Names: array[TERDDockZone] of string = ('Dock left','Dock right','Dock top','Dock bottom','Join center tabs');
begin
  if FShuttingDown then Exit;
  if not DropZoneAt(ScreenPoint,Zone) then begin CancelDrag; Exit; end;
  P := ScreenToClient(ScreenPoint); FPreview.Caption := Names[Zone];
  FPreview.SetBounds(EnsureRange(P.X-ScaleValue(90),0,Max(0,ClientWidth-ScaleValue(180))),EnsureRange(P.Y+ScaleValue(18),0,Max(0,ClientHeight-ScaleValue(36))),Min(ClientWidth,ScaleValue(180)),Min(ClientHeight,ScaleValue(36)));
  FPreview.Visible := True; FPreview.BringToFront; FPreview.Invalidate;
end;
procedure TERDDockHost.CancelDrag;
begin if (FPreview <> nil) and not FShuttingDown then FPreview.Visible := False; end;
procedure TERDDockHost.QueueDrop(Serial: Integer; const ScreenPoint: TPoint);
var Panel: TERDDockPanel;
begin
  CancelDrag; Panel := FindSerial(Serial); if Panel = nil then Exit; Panel.FQueuedPoint := ScreenPoint;
  PostMessage(Handle,WM_ERDDOCK,Serial,8);
end;
procedure TERDDockHost.DockMessage(var Message: TMessage);
var Panel: TERDDockPanel;
begin
  if FShuttingDown then Exit; if Message.LParam = 9 then begin Changed; Exit; end;
  Panel := FindSerial(Integer(Message.WParam)); if Panel = nil then Exit;
  case Message.LParam of 0: HidePanel(Panel.ID); 1: DockPanel(Panel.ID,Panel.Zone); 8: DropPanel(Panel.ID,Panel.FQueuedPoint); end;
end;
procedure TERDDockHost.Tick(Sender: TObject);
var P, Local: TPoint; Zone: TERDDockZone; ID: string; Focus: HWND; OverPeek: Boolean; NowTick: UInt64;
begin
  if FShuttingDown or not Showing or (FUpdateDepth > 0) then Exit;
  P := Mouse.CursorPos; ID := ''; NowTick := GetTickCount64;
  for Zone := dzLeft to dzBottom do if FRails[Zone].Visible then
  begin Local := FRails[Zone].ScreenToClient(P); ID := FRails[Zone].PanelAt(Local); if ID <> '' then Break; end;
  if ID <> FHoverID then begin FHoverID := ID; FHoverTick := NowTick; end;
  if (ID <> '') and (NowTick-FHoverTick >= 400) then PeekPanel(ID);
  if FPeekID = '' then Exit;
  Local := FPeek.ScreenToClient(P); Focus := GetFocus;
  OverPeek := (ID = FPeekID) or PtInRect(FPeek.ClientRect,Local) or ((Focus <> 0) and ((Focus = FPeek.Handle) or IsChild(FPeek.Handle,Focus)));
  if OverPeek then FLeaveTick := 0 else if FLeaveTick = 0 then FLeaveTick := NowTick else if NowTick-FLeaveTick >= 600 then ClosePeek;
end;
procedure TERDDockHost.MenuOpening(Sender: TObject);
var Panel: TERDDockPanel;
begin
  FMenu.Theme := Theme; Panel := FindPanel(FMenuID);
  FMenu.Items[6].Enabled := (Panel <> nil) and (Panel.Zone <> dzCenter); FMenu.Items[7].Enabled := (Panel <> nil) and Panel.Closable;
end;
procedure TERDDockHost.MenuAction(Sender: TObject);
var Panel: TERDDockPanel; Tag: Integer; P: TPoint;
begin
  Panel := FindPanel(FMenuID); if Panel = nil then Exit; Tag := TMenuItem(Sender).Tag;
  case Tag of
    0..4: DockPanel(Panel.ID,TERDDockZone(Tag));
    5: begin P := ClientToScreen(Point(ScaleValue(40),ScaleValue(40))); FloatPanel(Panel.ID,Rect(P.X,P.Y,P.X+ScaleValue(640),P.Y+ScaleValue(420))); end;
    6: if Panel.Mode = dpmAutoHide then DockPanel(Panel.ID,Panel.Zone) else AutoHidePanel(Panel.ID,Panel.Zone);
    7: HidePanel(Panel.ID);
  end;
end;
function TERDDockHost.GetLeftSize: Integer;
begin Result := FSizes[dzLeft]; end;
function TERDDockHost.GetRightSize: Integer;
begin Result := FSizes[dzRight]; end;
function TERDDockHost.GetTopSize: Integer;
begin Result := FSizes[dzTop]; end;
function TERDDockHost.GetBottomSize: Integer;
begin Result := FSizes[dzBottom]; end;
procedure TERDDockHost.SetLeftSize(Value: Integer);
begin SetSideSize(dzLeft,Value); end;
procedure TERDDockHost.SetRightSize(Value: Integer);
begin SetSideSize(dzRight,Value); end;
procedure TERDDockHost.SetTopSize(Value: Integer);
begin SetSideSize(dzTop,Value); end;
procedure TERDDockHost.SetBottomSize(Value: Integer);
begin SetSideSize(dzBottom,Value); end;
procedure TERDDockHost.SetSideSize(Zone: TERDDockZone; Value: Integer);
begin
  if (Value < 48) or (Value > 8192) then raise EArgumentOutOfRangeException.Create('Dock sizes must be 48..8192');
  if FSizes[Zone] = Value then Exit; FSizes[Zone] := Value; Resize;
  if FUpdateDepth > 0 then FDirty := True else if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self);
end;
procedure TERDDockHost.Resize;
var R: TRect; Zone: TERDDockZone; Rail, Grip, Size, Limit: Integer; Panel: TERDDockPanel;
  procedure Bounds(Control: TControl; const Rect: TRect);
  begin Control.SetBounds(Rect.Left,Rect.Top,Max(0,Rect.Width),Max(0,Rect.Height)); end;
begin
  inherited; if (FPeek = nil) or (FPreview = nil) then Exit;
  R := ClientRect; Rail := ScaleValue(28); Grip := ScaleValue(6);
  for Zone := dzLeft to dzBottom do if FRails[Zone].Visible then
  begin
    case Zone of
      dzLeft: begin Size := Min(Rail,Max(0,R.Width)); Bounds(FRails[Zone],Rect(R.Left,R.Top,R.Left+Size,R.Bottom)); Inc(R.Left,Size); end;
      dzRight: begin Size := Min(Rail,Max(0,R.Width)); Bounds(FRails[Zone],Rect(R.Right-Size,R.Top,R.Right,R.Bottom)); Dec(R.Right,Size); end;
      dzTop: begin Size := Min(Rail,Max(0,R.Height)); Bounds(FRails[Zone],Rect(R.Left,R.Top,R.Right,R.Top+Size)); Inc(R.Top,Size); end;
      dzBottom: begin Size := Min(Rail,Max(0,R.Height)); Bounds(FRails[Zone],Rect(R.Left,R.Bottom-Size,R.Right,R.Bottom)); Dec(R.Bottom,Size); end;
    end;
  end;
  FWork := R;
  // Preserve a usable center and a minimum for the opposing visible edge.
  for Zone := dzTop to dzBottom do if FGroups[Zone].Visible then
  begin
    Limit := Max(0,R.Height-ScaleValue(120)-Grip);
    if (Zone = dzTop) and FGroups[dzBottom].Visible then Dec(Limit,Min(Limit,ScaleValue(48)+Grip));
    Size := Min(ScaleValue(FSizes[Zone]),Limit);
    if Zone = dzTop then begin Bounds(FGroups[Zone],Rect(R.Left,R.Top,R.Right,R.Top+Size)); Inc(R.Top,Size); Bounds(FGrips[Zone],Rect(R.Left,R.Top,R.Right,Min(R.Bottom,R.Top+Grip))); Inc(R.Top,Min(Grip,Max(0,R.Height))); end
    else begin Bounds(FGroups[Zone],Rect(R.Left,R.Bottom-Size,R.Right,R.Bottom)); Dec(R.Bottom,Size); Bounds(FGrips[Zone],Rect(R.Left,Max(R.Top,R.Bottom-Grip),R.Right,R.Bottom)); Dec(R.Bottom,Min(Grip,Max(0,R.Height))); end;
  end;
  for Zone := dzLeft to dzRight do if FGroups[Zone].Visible then
  begin
    Limit := Max(0,R.Width-ScaleValue(120)-Grip);
    if (Zone = dzLeft) and FGroups[dzRight].Visible then Dec(Limit,Min(Limit,ScaleValue(48)+Grip));
    Size := Min(ScaleValue(FSizes[Zone]),Limit);
    if Zone = dzLeft then begin Bounds(FGroups[Zone],Rect(R.Left,R.Top,R.Left+Size,R.Bottom)); Inc(R.Left,Size); Bounds(FGrips[Zone],Rect(R.Left,R.Top,Min(R.Right,R.Left+Grip),R.Bottom)); Inc(R.Left,Min(Grip,Max(0,R.Width))); end
    else begin Bounds(FGroups[Zone],Rect(R.Right-Size,R.Top,R.Right,R.Bottom)); Dec(R.Right,Size); Bounds(FGrips[Zone],Rect(Max(R.Left,R.Right-Grip),R.Top,R.Right,R.Bottom)); Dec(R.Right,Min(Grip,Max(0,R.Width))); end;
  end;
  Bounds(FGroups[dzCenter],R);
  Panel := FindPanel(FPeekID);
  if (Panel <> nil) and FPeek.Visible then
  begin
    R := FWork;
    case Panel.Zone of
      dzLeft: R.Right := R.Left+Min(ScaleValue(FSizes[dzLeft]),Max(0,R.Width));
      dzRight: R.Left := R.Right-Min(ScaleValue(FSizes[dzRight]),Max(0,R.Width));
      dzTop: R.Bottom := R.Top+Min(ScaleValue(FSizes[dzTop]),Max(0,R.Height));
      dzBottom: R.Top := R.Bottom-Min(ScaleValue(FSizes[dzBottom]),Max(0,R.Height));
    end;
    Bounds(FPeek,R); FPeek.BringToFront;
  end;
  if FPreview.Visible then FPreview.BringToFront;
end;
procedure TERDDockHost.ThemeChanged;
var Zone: TERDDockZone; Pair: TPair<string,TERDDockFloatWindow>;
  procedure Apply(Control: TERDCustomControl);
  begin if Control <> nil then begin Control.Theme := Theme; Control.ThemeChanged; end; end;
begin
  inherited;
  for Zone := Low(TERDDockZone) to High(TERDDockZone) do if FGroups[Zone] <> nil then
  begin
    Apply(FGroups[Zone]); Apply(FGroups[Zone].FTabs); Apply(FGroups[Zone].FBody);
    if Zone <> dzCenter then begin Apply(FRails[Zone]); Apply(FGrips[Zone]); end;
  end;
  Apply(FParking); Apply(FPeek); if FPeek <> nil then begin Apply(FPeek.FTabs); Apply(FPeek.FBody); end; Apply(FPreview);
  if FFloats <> nil then for Pair in FFloats do Pair.Value.UpdateAppearance;
end;
procedure TERDDockHost.DensityChanged;
var Zone: TERDDockZone; Pair: TPair<string,TERDDockFloatWindow>;
  procedure Apply(Control: TERDCustomControl);
  begin if Control <> nil then begin Control.ParentDensity := False; Control.Density := Density; end; end;
begin
  inherited;
  for Zone := Low(TERDDockZone) to High(TERDDockZone) do if FGroups[Zone] <> nil then
  begin Apply(FGroups[Zone]); Apply(FGroups[Zone].FTabs); Apply(FGroups[Zone].FBody); if Zone <> dzCenter then begin Apply(FRails[Zone]); Apply(FGrips[Zone]); end; end;
  Apply(FPeek); if FPeek <> nil then begin Apply(FPeek.FTabs); Apply(FPeek.FBody); end; Apply(FPreview);
  if FFloats <> nil then for Pair in FFloats do Pair.Value.UpdateAppearance; Resize;
end;
procedure TERDDockHost.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try Painter.FillRect(ClientRect,Palette.Background);
    if (FPanels = nil) or (FPanels.Count = 0) then Painter.TextF(ClientWidth/2,ClientHeight/2,'Dock host - register panels with stable IDs',12,Palette.Subtle,twRegular,taCenter,ClientWidth);
  finally Painter.Free; end;
end;

function TERDDockHost.SaveLayout: string;
var Root, Row: TJSONObject; Sizes, Items, Active, Bounds: TJSONArray;
    Panel: TERDDockPanel; Zone: TERDDockZone; Window: TERDDockFloatWindow; R: TRect; PPI: Integer;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('version',TJSONNumber.Create(1)); Sizes := TJSONArray.Create; Root.AddPair('sizes',Sizes);
    for Zone := dzLeft to dzBottom do Sizes.Add(FSizes[Zone]);
    Active := TJSONArray.Create; Root.AddPair('active',Active);
    for Zone := Low(TERDDockZone) to High(TERDDockZone) do Active.Add(FActive[Zone]);
    Items := TJSONArray.Create; Root.AddPair('panels',Items);
    for Panel in FPanels do
    begin
      Row := TJSONObject.Create; Items.AddElement(Row); Row.AddPair('id',Panel.ID);
      Row.AddPair('zone',TJSONNumber.Create(Ord(Panel.Zone))); Row.AddPair('mode',TJSONNumber.Create(Ord(Panel.Mode)));
      Row.AddPair('restore',TJSONNumber.Create(Ord(Panel.FRestoreMode)));
      R := Panel.FFloatBounds; PPI := CurrentPPI;
      if FFloats.TryGetValue(Panel.ID,Window) then begin R := Window.BoundsRect; PPI := Window.CurrentPPI; end;
      Bounds := TJSONArray.Create; Row.AddPair('float',Bounds); Bounds.Add(R.Left); Bounds.Add(R.Top);
      Bounds.Add(Max(160,MulDiv(R.Width,96,PPI))); Bounds.Add(Max(120,MulDiv(R.Height,96,PPI)));
    end;
    Result := Root.ToJSON;
  finally Root.Free; end;
end;
function TERDDockHost.LoadLayout(const JSON: string; out ErrorText: string; IgnoreUnknown: Boolean): Boolean;
type TPlan = record Panel: TERDDockPanel; Zone: TERDDockZone; Mode, Restore: TERDDockMode; Bounds: TRect; end;
var Value: TJSONValue; Root, Row: TJSONObject; Rows, Sizes, Active, Bounds: TJSONArray;
    Plans: TArray<TPlan>; Seen: TDictionary<string,Boolean>; ProposedSizes: array[dzLeft..dzBottom] of Integer;
    ProposedActive: array[TERDDockZone] of string; Zone: TERDDockZone;
    I, N, X, Y, W, H: Integer; ID: string; Window: TERDDockFloatWindow;
  function Number(Value: TJSONValue; Low, High: Integer): Integer;
  begin
    if not (Value is TJSONNumber) or not TryStrToInt(Value.Value,Result) or (Result < Low) or (Result > High) then raise EArgumentException.Create('Invalid layout integer');
  end;
  function Text(Value: TJSONValue): string;
  begin
    if not (Value is TJSONString) then raise EArgumentException.Create('Invalid layout string'); Result := Value.Value;
    if Length(Result) > 128 then raise EArgumentException.Create('Layout ID is too long');
  end;
  function ArrayValue(Value: TJSONValue; Count: Integer): TJSONArray;
  begin
    if not (Value is TJSONArray) then raise EArgumentException.Create('Invalid layout array'); Result := TJSONArray(Value);
    if (Count >= 0) and (Result.Count <> Count) then raise EArgumentException.Create('Invalid layout array length');
  end;
begin
  Result := False; ErrorText := ''; Value := nil; Seen := TDictionary<string,Boolean>.Create;
  try
    try
      if (JSON = '') or (Length(JSON) > 4194304) then raise EArgumentException.Create('Layout must be 1..4194304 characters');
      Value := TJSONObject.ParseJSONValue(JSON);
      if not (Value is TJSONObject) then raise EArgumentException.Create('Layout must be an object'); Root := TJSONObject(Value);
      Number(Root.Values['version'],1,1); Sizes := ArrayValue(Root.Values['sizes'],4); Active := ArrayValue(Root.Values['active'],5);
      Rows := ArrayValue(Root.Values['panels'],-1); if Rows.Count > 4096 then raise EArgumentException.Create('Too many layout panels');
      for Zone := dzLeft to dzBottom do ProposedSizes[Zone] := Number(Sizes.Items[Ord(Zone)],48,8192);
      for Zone := Low(TERDDockZone) to High(TERDDockZone) do ProposedActive[Zone] := Text(Active.Items[Ord(Zone)]);
      SetLength(Plans,Rows.Count);
      for I := 0 to Rows.Count-1 do
      begin
        if not (Rows.Items[I] is TJSONObject) then raise EArgumentException.Create('Invalid panel layout'); Row := TJSONObject(Rows.Items[I]);
        ID := Text(Row.Values['id']); if (Trim(ID) = '') or Seen.ContainsKey(ID) then raise EArgumentException.Create('Duplicate or empty panel ID'); Seen.Add(ID,True);
        Plans[I].Panel := FindPanel(ID); if (Plans[I].Panel = nil) and not IgnoreUnknown then raise EArgumentException.Create('Unknown panel: '+ID);
        Plans[I].Zone := TERDDockZone(Number(Row.Values['zone'],0,Ord(High(TERDDockZone))));
        Plans[I].Mode := TERDDockMode(Number(Row.Values['mode'],0,Ord(High(TERDDockMode))));
        Plans[I].Restore := TERDDockMode(Number(Row.Values['restore'],0,Ord(dpmAutoHide)));
        if ((Plans[I].Mode = dpmAutoHide) or ((Plans[I].Mode = dpmHidden) and (Plans[I].Restore = dpmAutoHide))) and (Plans[I].Zone = dzCenter) then raise EArgumentException.Create('Auto hide requires an edge zone');
        if (Plans[I].Panel <> nil) and not Plans[I].Panel.Closable and (Plans[I].Mode = dpmHidden) then raise EArgumentException.Create('Non-closable panel cannot be hidden');
        Bounds := ArrayValue(Row.Values['float'],4); X := Number(Bounds.Items[0],-1000000,1000000); Y := Number(Bounds.Items[1],-1000000,1000000);
        W := Number(Bounds.Items[2],160,8192); H := Number(Bounds.Items[3],120,8192);
        Plans[I].Bounds := ClampFloat(Rect(X,Y,X+ScaleValue(W),Y+ScaleValue(H)));
      end;
      // Only the fully validated plan can change live content or create windows.
      BeginUpdate;
      try
        FPeekID := ''; CancelDrag;
        for Zone := dzLeft to dzBottom do FSizes[Zone] := ProposedSizes[Zone];
        for Zone := Low(TERDDockZone) to High(TERDDockZone) do FActive[Zone] := ProposedActive[Zone];
        N := 0;
        for I := 0 to High(Plans) do if Plans[I].Panel <> nil then
        begin
          Plans[I].Panel.FZone := Plans[I].Zone; Plans[I].Panel.FMode := Plans[I].Mode; Plans[I].Panel.FRestoreMode := Plans[I].Restore; Plans[I].Panel.FFloatBounds := Plans[I].Bounds;
          if FFloats.TryGetValue(Plans[I].Panel.ID,Window) then Window.BoundsRect := Plans[I].Bounds;
          FPanels.Move(FPanels.IndexOf(Plans[I].Panel),N); Inc(N);
        end;
        FDirty := True;
      finally EndUpdate; end;
      Result := True;
    except on E: Exception do ErrorText := E.Message; end;
  finally Seen.Free; Value.Free; end;
end;
end.
