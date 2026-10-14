unit ERD.Components.CommandPalette;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics, Vcl.ActnList, Vcl.Menus, Vcl.Forms,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint,
  ERD.Components.Icons, ERD.Components.Dialogs, ERD.Components.CommonControls,
  ERD.Components.ItemList, ERD.Components.Buttons;
type
  TERDCommandPalette = class;
  TERDCommandItem = class(TCollectionItem)
  private
    FKey, FCaption, FCategory, FKeywords, FShortcutText: string;
    FEnabled, FVisible: Boolean;
    FAction: TCustomAction;
    FIcon: TERDIcon;
    procedure SetKey(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetKeywords(const Value: string);
    procedure SetShortcutText(const Value: string);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetAction(Value: TCustomAction);
    procedure SetIcon(Value: TERDIcon);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DisplayCaption: string;
    function DisplayShortcut: string;
    function CanExecute: Boolean;
  published
    property Key: string read FKey write SetKey;
    property Caption: string read FCaption write SetCaption;
    property Category: string read FCategory write SetCategory;
    property Keywords: string read FKeywords write SetKeywords;
    property ShortcutText: string read FShortcutText write SetShortcutText;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Action: TCustomAction read FAction write SetAction;
    property Icon: TERDIcon read FIcon write SetIcon;
  end;
  TERDCommands = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TERDCommandItem;
    procedure SetItem(Index: Integer; Value: TERDCommandItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TERDCommandPalette);
    function Add: TERDCommandItem;
    property Items[Index: Integer]: TERDCommandItem read GetItem write SetItem; default;
  end;
  TERDCommandExecuteEvent = procedure(Sender: TObject; Command: TERDCommandItem) of object;
  /// <summary>Searchable command popup anchored near the top of the active window.</summary>
  TERDCommandPalette = class(TERDCommonDialog)
  private
    FCommands: TERDCommands;
    FPaletteWidth, FMaxVisibleResults: Integer;
    FSession: TObject;
    FExecuting: Boolean;
    FLastCommand: string;
    FOnExecute: TERDCommandExecuteEvent;
    procedure SetPaletteWidth(Value: Integer);
    procedure SetMaxVisibleResults(Value: Integer);
    procedure SetCommands(Value: TERDCommands);
  protected
    function DefaultTitle: string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    /// <summary>Returns stable collection IDs matching every query word, in collection order.</summary>
    function Search(const Query: string): TArray<Integer>;
    function ExecuteCommand(ID: Integer): Boolean;
    procedure Refresh;
    property LastCommand: string read FLastCommand;
  published
    property PaletteWidth: Integer read FPaletteWidth write SetPaletteWidth default 620;
    property MaxVisibleResults: Integer read FMaxVisibleResults write SetMaxVisibleResults default 8;
    property Commands: TERDCommands read FCommands write SetCommands;
    /// <summary>Used when a command has no Action. Action-backed commands execute the Action.</summary>
    property OnExecute: TERDCommandExecuteEvent read FOnExecute write FOnExecute;
  end;
implementation
type
  TERDCommandWindow = class(TERDDialogWindow)
  private
    FAnchor: TRect;
    FWorkArea: TRect;
    procedure HitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure LostActivation(Sender: TObject);
  protected
    procedure Paint; override;
    procedure DoShow; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure AnchorTo(Form: TCustomForm);
    function RunPalette: TModalResult;
  end;
  TERDPaletteSession = class;
  TERDCommandResults = class(TERDCustomItemList)
  private
    FSession: TERDPaletteSession;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
  public
    constructor CreateResults(Owner: TComponent; Session: TERDPaletteSession);
    procedure RefreshItems;
  end;
  TERDPaletteSession = class
  private
    FPalette: TERDCommandPalette;
    FWindow: TERDCommandWindow;
    FSearch: TERDSearchEdit;
    FList: TERDCommandResults;
    FMatches: TArray<Integer>;
    FRefreshing: Boolean;
    FSelectedID: Integer;
    procedure SearchChanged(Sender: TObject);
    procedure SearchKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Accept(Sender: TObject);
    function ItemAt(Index: Integer): TERDCommandItem;
  public
    constructor Create(Palette: TERDCommandPalette);
    destructor Destroy; override;
    procedure Refresh;
  end;
procedure TERDCommandWindow.AnchorTo(Form: TCustomForm);
var Monitor: TMonitor;
begin
  if Form <> nil then
  begin
    PopupMode := pmExplicit;
    PopupParent := Form;
    FAnchor.TopLeft := Form.ClientToScreen(Point(0, 0));
    FAnchor.BottomRight := Form.ClientToScreen(Point(Form.ClientWidth, Form.ClientHeight));
    Monitor := Screen.MonitorFromWindow(Form.Handle, mdNearest);
  end
  else Monitor := Screen.MonitorFromWindow(Application.Handle, mdNearest);
  if Monitor <> nil then FWorkArea := Monitor.WorkareaRect else FWorkArea := Screen.DesktopRect;
  if Form = nil then FAnchor := FWorkArea;
  OnDeactivate := LostActivation;
end;

procedure TERDCommandWindow.Paint;
var P: TERDPainter;
begin
  P := TERDPainter.Create(Canvas, Palette, PPI);
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    P.HLine(Dp(1), Dp(47), ClientWidth - Dp(2), Palette.NeutralLight);
  finally P.Free end;
end;

procedure TERDCommandWindow.DoShow;
begin
  inherited;
  Left := EnsureRange(FAnchor.Left + (FAnchor.Width - Width) div 2,
    FWorkArea.Left, Max(FWorkArea.Left, FWorkArea.Right - Width));
  Top := EnsureRange(FAnchor.Top + Dp(16), FWorkArea.Top,
    Max(FWorkArea.Top, FWorkArea.Bottom - Height));
end;

procedure TERDCommandWindow.HitTest(var Message: TWMNCHitTest);
begin Message.Result := HTCLIENT end;
procedure TERDCommandWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin end;
procedure TERDCommandWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin end;
procedure TERDCommandWindow.LostActivation(Sender: TObject);
begin ModalResult := mrCancel end;

function TERDCommandWindow.RunPalette: TModalResult;
begin
  ModalResult := mrNone;
  Show;
  try
    // The owner stays enabled: clicking outside dismisses the popup.
    while Visible and (ModalResult = mrNone) and not Application.Terminated do
      Application.HandleMessage;
    Result := ModalResult;
  finally Hide end;
end;

procedure TERDCommandResults.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  inherited;
  if Button <> mbLeft then Exit;
  Index := ItemAt(X, Y);
  if (Index >= 0) and (Index = ItemIndex) then FSession.Accept(Self);
end;

procedure TERDCommandPalette.SetPaletteWidth(Value: Integer);
begin FPaletteWidth := EnsureRange(Value, 280, 1200) end;
procedure TERDCommandPalette.SetMaxVisibleResults(Value: Integer);
begin FMaxVisibleResults := EnsureRange(Value, 1, 20); Refresh end;

constructor TERDCommandItem.Create(Collection: TCollection);
begin
  inherited;
  FEnabled := True;
  FVisible := True;
  FIcon := TERDIcon.Create(Self);
end;
destructor TERDCommandItem.Destroy;
begin FIcon.Free; inherited end;
procedure TERDCommandItem.Assign(Source: TPersistent);
var Item: TERDCommandItem;
begin
  if Source is TERDCommandItem then
  begin
    Item := TERDCommandItem(Source);
    FKey := Item.Key; FCaption := Item.Caption; FCategory := Item.Category;
    FKeywords := Item.Keywords; FShortcutText := Item.ShortcutText;
    FEnabled := Item.Enabled; FVisible := Item.Visible;
    Action := Item.Action;
    FIcon.Assign(Item.Icon);
    Changed(False);
  end else inherited;
end;
function TERDCommandItem.DisplayCaption: string;
begin
  Result := FCaption;
  if (Result = '') and (FAction <> nil) then Result := StripHotKey(FAction.Caption);
  if Result = '' then Result := FKey;
end;
function TERDCommandItem.DisplayShortcut: string;
begin
  Result := FShortcutText;
  if (Result = '') and (FAction <> nil) then Result := ShortCutToText(FAction.ShortCut);
end;
function TERDCommandItem.GetDisplayName: string;
begin Result := DisplayCaption; if Result = '' then Result := inherited GetDisplayName end;
function TERDCommandItem.CanExecute: Boolean;
begin
  Result := FEnabled and FVisible and ((FAction = nil) or (FAction.Enabled and FAction.Visible));
end;
procedure TERDCommandItem.SetKey(const Value: string);
begin FKey := Value; Changed(False) end;
procedure TERDCommandItem.SetCaption(const Value: string);
begin FCaption := Value; Changed(False) end;
procedure TERDCommandItem.SetCategory(const Value: string);
begin FCategory := Value; Changed(False) end;
procedure TERDCommandItem.SetKeywords(const Value: string);
begin FKeywords := Value; Changed(False) end;
procedure TERDCommandItem.SetShortcutText(const Value: string);
begin FShortcutText := Value; Changed(False) end;
procedure TERDCommandItem.SetEnabled(Value: Boolean);
begin FEnabled := Value; Changed(False) end;
procedure TERDCommandItem.SetVisible(Value: Boolean);
begin FVisible := Value; Changed(False) end;
procedure TERDCommandItem.SetAction(Value: TCustomAction);
begin
  if FAction = Value then Exit;
  FAction := Value;
  if (Value <> nil) and (Collection <> nil) and (Collection.Owner is TComponent) then
    Value.FreeNotification(TComponent(Collection.Owner));
  Changed(False);
end;
procedure TERDCommandItem.SetIcon(Value: TERDIcon);
begin FIcon.Assign(Value); Changed(False) end;
constructor TERDCommands.Create(Owner: TERDCommandPalette);
begin inherited Create(Owner, TERDCommandItem) end;
function TERDCommands.Add: TERDCommandItem;
begin Result := TERDCommandItem(inherited Add) end;
function TERDCommands.GetItem(Index: Integer): TERDCommandItem;
begin Result := TERDCommandItem(inherited Items[Index]) end;
procedure TERDCommands.SetItem(Index: Integer; Value: TERDCommandItem);
begin Items[Index].Assign(Value) end;
procedure TERDCommands.Update(Item: TCollectionItem);
begin inherited; if GetOwner is TERDCommandPalette then TERDCommandPalette(GetOwner).Refresh end;
constructor TERDCommandPalette.Create(AOwner: TComponent);
begin
  inherited;
  FPaletteWidth := 620;
  FMaxVisibleResults := 8;
  FCommands := TERDCommands.Create(Self);
end;
destructor TERDCommandPalette.Destroy;
begin FreeAndNil(FCommands); inherited end;
function TERDCommandPalette.DefaultTitle: string;
begin Result := 'Commands' end;
procedure TERDCommandPalette.SetCommands(Value: TERDCommands);
begin FCommands.Assign(Value) end;
procedure TERDCommandPalette.Notification(AComponent: TComponent; Operation: TOperation);
var I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (FCommands <> nil) then
    for I := 0 to FCommands.Count - 1 do if FCommands[I].Action = AComponent then FCommands[I].Action := nil;
end;
procedure TERDCommandPalette.Refresh;
begin if FSession <> nil then TERDPaletteSession(FSession).Refresh end;
function TERDCommandPalette.Search(const Query: string): TArray<Integer>;
var Item: TERDCommandItem; Words: TArray<string>; Word, Haystack: string;
  I: Integer; Matches: Boolean; IDs: TList<Integer>;
begin
  IDs := TList<Integer>.Create;
  try
    Words := LowerCase(Trim(Query)).Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    for I := 0 to FCommands.Count - 1 do
    begin
      Item := FCommands[I];
      if not Item.CanExecute then Continue;
      Haystack := LowerCase(Item.DisplayCaption + ' ' + Item.Category + ' ' + Item.Keywords + ' ' + Item.Key);
      Matches := True;
      for Word in Words do if Pos(Word, Haystack) = 0 then begin Matches := False; Break end;
      if Matches then IDs.Add(Item.ID);
    end;
    Result := IDs.ToArray;
  finally IDs.Free end;
end;
function TERDCommandPalette.ExecuteCommand(ID: Integer): Boolean;
var Item: TERDCommandItem; Action: TCustomAction;
begin
  Result := False;
  Item := TERDCommandItem(FCommands.FindItemID(ID));
  if Item = nil then Exit;
  Action := Item.Action;
  if Action <> nil then
  begin
    Action.Update;
    Item := TERDCommandItem(FCommands.FindItemID(ID));
    if Item = nil then Exit;
  end;
  if not Item.CanExecute then Exit;
  Action := Item.Action;
  if (Action = nil) and not Assigned(FOnExecute) then Exit;
  FLastCommand := Item.Key;
  if Action <> nil then Result := Action.Execute
  else if Assigned(FOnExecute) then begin Result := True; FOnExecute(Self, Item) end;
end;
function TERDCommandPalette.Execute: Boolean;
var Session: TERDPaletteSession; Selected: Integer; I: Integer;
begin
  Result := False;
  if FExecuting or (csDesigning in ComponentState) then Exit;
  FExecuting := True;
  FLastCommand := '';
  Session := nil;
  Selected := -1;
  try
    for I := FCommands.Count - 1 downto 0 do
      if (I < FCommands.Count) and (FCommands[I].Action <> nil) then FCommands[I].Action.Update;
    Session := TERDPaletteSession.Create(Self);
    FSession := Session;
    DoShow;
    try
      if Session.FWindow.RunPalette = mrOk then Selected := Session.FSelectedID;
    finally DoClose end;
  finally FSession := nil; Session.Free; FExecuting := False end;
  if Selected >= 0 then Result := ExecuteCommand(Selected);
end;
constructor TERDCommandResults.CreateResults(Owner: TComponent; Session: TERDPaletteSession);
begin inherited Create(Owner); FSession := Session end;
function TERDCommandResults.GetItemCount: Integer;
begin if FSession = nil then Result := 0 else Result := Length(FSession.FMatches) end;
function TERDCommandResults.GetItemText(AIndex: Integer): string;
var Item: TERDCommandItem;
begin
  Result := '';
  Item := FSession.ItemAt(AIndex);
  if Item <> nil then Result := Item.DisplayCaption;
end;
function TERDCommandResults.ItemHeight: Integer;
begin Result := ScaleValue(44) end;
procedure TERDCommandResults.RefreshItems;
begin ItemsChanged end;
procedure TERDCommandResults.DrawItem(APainter: TERDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var Item: TERDCommandItem; X, CY, Reserved: Integer; Ink: TColor;
begin
  Item := FSession.ItemAt(AIndex);
  if Item = nil then Exit;
  X := ARect.Left + APainter.S(12);
  CY := (ARect.Top + ARect.Bottom) div 2;
  Ink := Palette.ForegroundText;
  if not Item.CanExecute then Ink := APainter.DisabledText;
  if Item.Icon.Available then
  begin
    Item.Icon.Draw(APainter.Canvas, Palette, ScaleValue(96), X + APainter.S(8), CY, Palette.Accent, 1, Item.CanExecute);
    Inc(X, APainter.S(28));
  end;
  Reserved := APainter.S(16);
  if Item.DisplayShortcut <> '' then Inc(Reserved, APainter.S(110));
  if Item.Category = '' then
    APainter.Text(X, CY, Item.DisplayCaption, ERD_TEXT_BODY, Ink, twRegular, taLeftJustify, ARect.Right - X - Reserved)
  else
  begin
    APainter.Text(X, CY - APainter.S(9), Item.DisplayCaption, ERD_TEXT_BODY, Ink, twRegular, taLeftJustify, ARect.Right - X - Reserved);
    APainter.Text(X, CY + APainter.S(12), Item.Category, ERD_TEXT_CAPS, Palette.Subtle, twRegular, taLeftJustify, ARect.Right - X - Reserved);
  end;
  APainter.Text(ARect.Right - APainter.S(12), CY, Item.DisplayShortcut, ERD_TEXT_CAPS,
    Palette.Subtle, twRegular, taRightJustify, APainter.S(110));
end;
constructor TERDPaletteSession.Create(Palette: TERDCommandPalette);
begin
  inherited Create;
  FPalette := Palette;
  FSelectedID := -1;
  FWindow := TERDCommandWindow.CreateWindow(Application, Palette.Theme, Palette.Density, Palette.Title);
  FWindow.AnchorTo(Screen.ActiveCustomForm);
  FWindow.ClientWidth := Min(FWindow.Dp(Palette.PaletteWidth), FWindow.FWorkArea.Width - FWindow.Dp(24));
  FSearch := TERDSearchEdit.Create(FWindow);
  FWindow.Adopt(FSearch);
  FSearch.SetBounds(FWindow.Dp(8), FWindow.Dp(8), FWindow.ClientWidth - FWindow.Dp(16), FWindow.Dp(32));
  FSearch.TextHint := 'Search commands...';
  FSearch.SearchDelay := 0;
  FSearch.OnSearch := SearchChanged;
  FSearch.OnKeyDown := SearchKey;
  FList := TERDCommandResults.CreateResults(FWindow, Self);
  FWindow.Adopt(FList);
  FList.SetBounds(FWindow.Dp(1), FWindow.Dp(48), FWindow.ClientWidth - FWindow.Dp(2), FWindow.Dp(44));
  FList.EmptyText := 'No matching commands';
  FList.OnItemActivate := Accept;
  FWindow.InitialFocus := FSearch;
  Refresh;
end;
destructor TERDPaletteSession.Destroy;
begin FWindow.Free; inherited end;
function TERDPaletteSession.ItemAt(Index: Integer): TERDCommandItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < Length(FMatches)) then Result := TERDCommandItem(FPalette.Commands.FindItemID(FMatches[Index]));
end;
procedure TERDPaletteSession.Refresh;
begin
  if FRefreshing then Exit;
  FRefreshing := True;
  try
    FMatches := FPalette.Search(FSearch.Text);
    FList.RefreshItems;
    if Length(FMatches) > 0 then FList.ItemIndex := 0 else FList.ItemIndex := -1;
    FList.Height := Min(Max(1, Length(FMatches)), FPalette.MaxVisibleResults) * FWindow.Dp(44);
    FList.Height := Min(FList.Height, Max(FWindow.Dp(44), FWindow.FWorkArea.Height - FWindow.Dp(80)));
    FWindow.ClientHeight := FList.Top + FList.Height + FWindow.Dp(8);
  finally FRefreshing := False end;
end;
procedure TERDPaletteSession.SearchChanged(Sender: TObject);
begin Refresh end;
procedure TERDPaletteSession.SearchKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  case Key of
    VK_DOWN: if Length(FMatches) > 0 then FList.ItemIndex := Min(Length(FMatches) - 1, FList.ItemIndex + 1);
    VK_UP: if Length(FMatches) > 0 then FList.ItemIndex := Max(0, FList.ItemIndex - 1);
    VK_NEXT: if Length(FMatches) > 0 then FList.ItemIndex := Min(Length(FMatches) - 1, FList.ItemIndex + FPalette.MaxVisibleResults);
    VK_PRIOR: if Length(FMatches) > 0 then FList.ItemIndex := Max(0, FList.ItemIndex - FPalette.MaxVisibleResults);
    VK_RETURN: Accept(Self);
    VK_ESCAPE: FWindow.ModalResult := mrCancel;
  else Exit;
  end;
  FList.EnsureVisible(FList.ItemIndex);
  Key := 0;
end;
procedure TERDPaletteSession.Accept(Sender: TObject);
var Item: TERDCommandItem;
begin
  Item := ItemAt(FList.ItemIndex);
  if (Item = nil) or not Item.CanExecute then Exit;
  FSelectedID := Item.ID;
  FWindow.ModalResult := mrOk;
end;
end.
