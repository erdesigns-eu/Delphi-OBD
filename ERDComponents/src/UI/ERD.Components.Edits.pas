//------------------------------------------------------------------------------
//  ERD.Components.Edits
//
//  Themed text entry and pick list for the Application controls.
//
//    TERDEdit      single-line edit: a borderless VCL edit inside a
//                  painted frame (1 px border, 2 px orange when
//                  focused, background fill when disabled). Mono
//                  switches to Consolas for VINs, CAN IDs and files.
//    TERDComboBox  drop-down list (no free text): the current item and
//                  a chevron; click, Alt+Down or F4 opens a themed
//                  TERDPopupList. While closed the arrow keys step
//                  through the items and typing selects the first item
//                  that starts with the typed text.
//
//  Both take their height from the density (26 px desktop, 44 px
//  tablet).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.Edits;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.PopupList;

type
  /// <summary>Themed single-line edit.</summary>
  TERDEdit = class(TERDCustomControl)
  strict private
    FWantReturnKey: Boolean;
    FEdit: TEdit;
    FMono: Boolean;
    FOnChange: TNotifyEvent;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(AValue: Integer);
    function GetNumbersOnly: Boolean;
    procedure SetNumbersOnly(AValue: Boolean);
    function GetEditTextHint: string;
    procedure SetEditTextHint(const AValue: string);
    function GetSelStart: Integer;
    procedure SetSelStart(AValue: Integer);
    function GetSelLength: Integer;
    procedure SetSelLength(AValue: Integer);
    procedure SetMono(AValue: Boolean);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(AValue: TEditCharCase);
    procedure InnerChange(Sender: TObject);
    procedure InnerFocusChange(Sender: TObject);
    procedure InnerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InnerKeyPress(Sender: TObject; var Key: Char);
    procedure SyncInnerLook;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
  protected
    /// <summary>Places the inner edit: one text line high, centred
    /// vertically, inset by the padding and <see cref="TrailingSpace"/>.</summary>
    procedure LayoutInner;
    /// <summary>Width reserved at the right edge for an adornment that a
    /// descendant paints (for example a validity lamp).</summary>
    /// <returns>Reserved width in device pixels; 0 by default.</returns>
    function TrailingSpace: Integer; virtual;
    /// <summary>Called when the text changes; fires
    /// <see cref="OnChange"/>. Descendants override it to react to edits
    /// without taking over the event.</summary>
    procedure Change; virtual;
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates the edit with its inner VCL edit.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-sizes and re-places the inner edit.</summary>
    procedure DensityChanged; override;
    /// <summary>Focuses the inner edit.</summary>
    procedure SetFocus; override;
    /// <summary>Selects all text.</summary>
    procedure SelectAll;
    /// <summary>True while the inner edit has the focus.</summary>
    /// <returns>Focus flag.</returns>
    function Focused: Boolean; override;
    /// <summary>Start of the selection.</summary>
    property SelStart: Integer read GetSelStart write SetSelStart;
    /// <summary>Length of the selection.</summary>
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    /// <summary>Height follows the density.</summary>
    property AutoSize default True;
    /// <summary>Edit text.</summary>
    property WantReturnKey: Boolean read FWantReturnKey write FWantReturnKey default False;
    property Text: string read GetText write SetText;
    /// <summary>Text alignment.</summary>
    property Alignment: TAlignment read GetAlignment write SetAlignment
      default taLeftJustify;
    /// <summary>Text can be selected and copied, not changed.</summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly
      default False;
    /// <summary>Maximum number of characters; 0 = no limit.</summary>
    property MaxLength: Integer read GetMaxLength write SetMaxLength
      default 0;
    /// <summary>Accepts digits only.</summary>
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly
      default False;
    /// <summary>Grey hint shown while the edit is empty.</summary>
    property TextHint: string read GetEditTextHint write SetEditTextHint;
    /// <summary>Consolas instead of Segoe UI.</summary>
    property Mono: Boolean read FMono write SetMono default False;
    /// <summary>Forces typed text to upper or lower case.</summary>
    property CharCase: TEditCharCase read GetCharCase write SetCharCase
      default ecNormal;
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the text changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed drop-down list.</summary>
  TERDComboBox = class(TERDCustomControl)
  strict private
    FItems: TStrings;
    FItemIndex: Integer;
    FPopup: TERDPopupList;
    FMono: Boolean;
    FDropDownCount: Integer;
    FSearch: string;
    FSearchTick: Cardinal;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure SetItems(AValue: TStrings);
    procedure SetItemIndex(AValue: Integer);
    function GetText: string;
    procedure SetMono(AValue: Boolean);
    procedure SetDropDownCount(AValue: Integer);
    procedure ItemsChanged(Sender: TObject);
    procedure PopupCloseUp(Sender: TObject; AAccepted: Boolean;
      AIndex: Integer);
    procedure ChangeIndex(AIndex: Integer);
    procedure Search(AChar: Char);
    function IsOpen: Boolean;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    /// <summary>Called when the user picks another item; fires
    /// <see cref="OnChange"/>. Descendants override it to react without
    /// taking over the event.</summary>
    procedure Change; virtual;
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    /// <summary>Creates an empty combo box.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Closes the list and frees the items.</summary>
    destructor Destroy; override;
    /// <summary>Re-sizes for the new density.</summary>
    procedure DensityChanged; override;
    /// <summary>Opens the list under the box.</summary>
    procedure DropDown;
    /// <summary>Closes the list.</summary>
    /// <param name="AAccept">Take the highlighted item.</param>
    procedure CloseUp(AAccept: Boolean);
    /// <summary>The selected item, or '' when ItemIndex is -1.
    /// </summary>
    property Text: string read GetText;
    /// <summary>True while the list is open.</summary>
    property DroppedDown: Boolean read IsOpen;
  published
    /// <summary>Height follows the density.</summary>
    property AutoSize default True;
    /// <summary>Items to choose from.</summary>
    property Items: TStrings read FItems write SetItems;
    /// <summary>Selected item; -1 = none.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex
      default -1;
    /// <summary>Consolas instead of Segoe UI.</summary>
    property Mono: Boolean read FMono write SetMono default False;
    /// <summary>Most rows the open list shows before it scrolls.</summary>
    property DropDownCount: Integer read FDropDownCount
      write SetDropDownCount default 8;
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Focusable with Tab.</summary>
    property TabStop default True;
    /// <summary>Fires when the user picks another item.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// <summary>Fires before the list opens.</summary>
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    /// <summary>Fires after the list closed.</summary>
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

uses ERD.Components.NativeSelection;

const
  EDIT_TEXT_SIZE = ERD_TEXT_BODY;
  SEARCH_RESET_MS = 1000;

function TextFontHeight(ASize: Single; APPI: Integer): Integer;
begin
  Result := -Round(ASize * APPI / 96);
end;

type
  // Native edits need end padding so a right-aligned final character
  // does not share the last drawable pixel with the caret.
  TERDInnerEdit = class(TERDSelectionEdit)
  private
    procedure PositionEndCaret;
  protected
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
  end;

procedure TERDInnerEdit.CreateWnd;
var Margin: Integer;
begin
  inherited;
  Margin := Max(2, MulDiv(2, Font.PixelsPerInch, 96));
  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN,
    LPARAM(Margin or (Margin shl 16)));
end;

procedure TERDInnerEdit.PositionEndCaret;
var Caret: TPoint; Margins: LRESULT; RightMargin, X: Integer;
begin
  if not HandleAllocated or not Focused or (Alignment <> taRightJustify) or
    (BiDiMode <> bdLeftToRight) or (SelLength <> 0) or
    (SelStart <> Length(Text)) or not GetCaretPos(Caret) then Exit;
  Margins := SendMessage(Handle, EM_GETMARGINS, 0, 0);
  RightMargin := Integer((Margins shr 16) and $FFFF);
  // The logical end of a right-aligned single-line edit is its right
  // text edge, not the native caret's sometimes rounded glyph position.
  X := Max(0, Min(ClientWidth - 1, ClientWidth - RightMargin));
  if Caret.X <> X then SetCaretPos(X, Caret.Y);
end;

procedure TERDInnerEdit.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = WM_GETDLGCODE) and (Message.WParam = VK_RETURN) and
    (Parent is TERDEdit) and TERDEdit(Parent).WantReturnKey then
    Message.Result := Message.Result or DLGC_WANTMESSAGE;
  case Message.Msg of
    WM_SETFOCUS, WM_PAINT, WM_SETTEXT, WM_SETFONT, WM_SIZE,
    WM_KEYUP, WM_CHAR, WM_LBUTTONDOWN, WM_LBUTTONUP, EM_SETSEL, EM_SETMARGINS:
      PositionEndCaret;
  end;
end;

{ TERDEdit }

constructor TERDEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 180;
  Height := 26;
  TabStop := False;
  FEdit := TERDInnerEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.BorderStyle := bsNone;
  FEdit.AutoSize := False;
  FEdit.ParentFont := False;
  FEdit.ParentColor := False;
  FEdit.TabStop := True;
  FEdit.OnChange := InnerChange;
  FEdit.OnEnter := InnerFocusChange;
  FEdit.OnExit := InnerFocusChange;
  FEdit.OnKeyDown := InnerKeyDown;
  FEdit.OnKeyPress := InnerKeyPress;
  AutoSize := True;
  LayoutInner;
end;

function TERDEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TERDEdit.SetText(const AValue: string);
begin
  FEdit.Text := AValue;
end;

function TERDEdit.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;

procedure TERDEdit.SetAlignment(AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
  LayoutInner;
end;

function TERDEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TERDEdit.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

function TERDEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

procedure TERDEdit.SetMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

function TERDEdit.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

procedure TERDEdit.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

function TERDEdit.GetEditTextHint: string;
begin
  Result := FEdit.TextHint;
end;

procedure TERDEdit.SetEditTextHint(const AValue: string);
begin
  FEdit.TextHint := AValue;
end;

function TERDEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

procedure TERDEdit.SetSelStart(AValue: Integer);
begin
  FEdit.SelStart := AValue;
end;

function TERDEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

procedure TERDEdit.SetSelLength(AValue: Integer);
begin
  FEdit.SelLength := AValue;
end;

procedure TERDEdit.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  LayoutInner;
end;

function TERDEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

procedure TERDEdit.SetCharCase(AValue: TEditCharCase);
begin
  FEdit.CharCase := AValue;
end;

function TERDEdit.TrailingSpace: Integer;
begin
  Result := 0;
end;

procedure TERDEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TERDEdit.SetFocus;
begin
  if FEdit.CanFocus then
    FEdit.SetFocus
  else
    inherited SetFocus;
end;

function TERDEdit.Focused: Boolean;
begin
  Result := FEdit.Focused;
end;

procedure TERDEdit.InnerChange(Sender: TObject);
begin
  Change;
end;

procedure TERDEdit.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDEdit.InnerFocusChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TERDEdit.InnerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TERDEdit.InnerKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;

procedure TERDEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FEdit.Enabled := Enabled;
  SyncInnerLook;
  Invalidate;
end;

procedure TERDEdit.SyncInnerLook;
var
  Back, Ink: TColor;
begin
  if Enabled then
  begin
    Back := Palette.GaugeFace;
    Ink := Palette.ForegroundText;
  end
  else
  begin
    Back := Palette.Background;
    Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
  end;
  if FEdit.Color <> Back then
    FEdit.Color := Back;
  if FEdit.Font.Color <> Ink then
    FEdit.Font.Color := Ink;
end;

procedure TERDEdit.LayoutInner;
var
  FontH, Pad, EH, Margin: Integer;
begin
  if FEdit = nil then
    Exit;
  if FMono and not SameText(FEdit.Font.Name, 'Consolas') then
    FEdit.Font.Name := 'Consolas'
  else if not FMono and not SameText(FEdit.Font.Name, 'Segoe UI') then
    FEdit.Font.Name := 'Segoe UI';
  FontH := TextFontHeight(EDIT_TEXT_SIZE, ScaleValue(96));
  if FEdit.Font.Height <> FontH then
    FEdit.Font.Height := FontH;
  Pad := ScaleValue(10);
  // A borderless edit draws its text at the top of its window, so the
  // window is exactly one text line high and centred in the frame; that
  // centres the text itself.
  EH := Max(1, Min(ERDFontLineHeight(FEdit.Font), Height - ScaleValue(4)));
  FEdit.SetBounds(Pad, (Height - EH) div 2,
    Max(0, Width - 2 * Pad - TrailingSpace), EH);
  if FEdit.HandleAllocated then
  begin
    Margin := Max(2, ScaleValue(2));
    SendMessage(FEdit.Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN,
      LPARAM(Margin or (Margin shl 16)));
  end;
  SyncInnerLook;
end;

procedure TERDEdit.Resize;
begin
  inherited Resize;
  LayoutInner;
  // Repaint the whole frame and the inner edit after a size change, not
  // only the newly exposed strip, so a shrinking edit keeps its border.
  Invalidate;
  if (FEdit <> nil) and FEdit.HandleAllocated then
    FEdit.Invalidate;
end;

procedure TERDEdit.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  LayoutInner;
  inherited DensityChanged;
end;

function TERDEdit.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TERDEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and FEdit.CanFocus then
    FEdit.SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TERDEdit.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  SyncInnerLook;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height), FEdit.Focused and not IsPreview,
      Enabled);
  finally
    P.Free;
  end;
end;

{ TERDComboBox }

constructor TERDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FItemIndex := -1;
  FDropDownCount := 8;
  Width := 180;
  Height := 26;
  TabStop := True;
  AutoSize := True;
end;

destructor TERDComboBox.Destroy;
begin
  if FPopup <> nil then
    FPopup.OnCloseUp := nil;
  FItems.Free;
  inherited Destroy;
end;

procedure TERDComboBox.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

procedure TERDComboBox.ItemsChanged(Sender: TObject);
begin
  if FItemIndex >= FItems.Count then
    FItemIndex := FItems.Count - 1;
  Invalidate;
end;

procedure TERDComboBox.SetItemIndex(AValue: Integer);
begin
  if not (csLoading in ComponentState) then
    AValue := EnsureRange(AValue, -1, FItems.Count - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
end;

function TERDComboBox.GetText: string;
begin
  if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
    Result := FItems[FItemIndex]
  else
    Result := '';
end;

procedure TERDComboBox.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  Invalidate;
end;

procedure TERDComboBox.SetDropDownCount(AValue: Integer);
begin
  FDropDownCount := Max(1, AValue);
end;

procedure TERDComboBox.ChangeIndex(AIndex: Integer);
begin
  AIndex := EnsureRange(AIndex, -1, FItems.Count - 1);
  if AIndex = FItemIndex then
    Exit;
  FItemIndex := AIndex;
  Invalidate;
  Change;
end;

procedure TERDComboBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TERDComboBox.IsOpen: Boolean;
begin
  Result := (FPopup <> nil) and FPopup.IsOpen;
end;

procedure TERDComboBox.DropDown;
var
  TopLeft, BottomRight: TPoint;
begin
  if IsOpen or not Enabled or (FItems.Count = 0) then
    Exit;
  if FPopup = nil then
  begin
    FPopup := TERDPopupList.Create(Self);
    FPopup.OnCloseUp := PopupCloseUp;
  end;
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
  TopLeft := ClientToScreen(Point(0, 0));
  BottomRight := ClientToScreen(Point(Width, Height));
  FPopup.Popup(Self, Rect(TopLeft.X, TopLeft.Y, BottomRight.X, BottomRight.Y),
    FItems, FItemIndex, Palette, ScaleValue(96),
    ScaleValue(Metrics.CompactRow), FDropDownCount);
  Invalidate;
end;

procedure TERDComboBox.CloseUp(AAccept: Boolean);
begin
  if IsOpen then
    FPopup.CloseUp(AAccept);
end;

procedure TERDComboBox.PopupCloseUp(Sender: TObject; AAccepted: Boolean;
  AIndex: Integer);
begin
  if AAccepted and (AIndex >= 0) then
    ChangeIndex(AIndex);
  Invalidate;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TERDComboBox.Search(AChar: Char);
var
  Tick: Cardinal;
  I, Start: Integer;
begin
  Tick := GetTickCount;
  if Tick - FSearchTick > SEARCH_RESET_MS then
    FSearch := '';
  FSearchTick := Tick;
  FSearch := FSearch + AChar;
  Start := Max(FItemIndex, 0);
  if Length(FSearch) = 1 then
    Inc(Start);
  for I := 0 to FItems.Count - 1 do
    if StartsText(FSearch, FItems[(Start + I) mod FItems.Count]) then
    begin
      ChangeIndex((Start + I) mod FItems.Count);
      Exit;
    end;
end;

procedure TERDComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
    CloseUp(False);
  Invalidate;
end;

procedure TERDComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPopup) then
    CloseUp(False);
end;

procedure TERDComboBox.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if IsOpen and ((Message.CharCode = VK_RETURN) or
    (Message.CharCode = VK_ESCAPE)) then
    Message.Result := 1
  else
    inherited;
end;

procedure TERDComboBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TERDComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;

procedure TERDComboBox.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TERDComboBox.DoExit;
begin
  CloseUp(False);
  inherited DoExit;
  Invalidate;
end;

procedure TERDComboBox.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TERDComboBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TERDComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    if CanFocus and not Focused then
      SetFocus;
    if IsOpen then
      CloseUp(False)
    else
      DropDown;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TERDComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  if IsOpen then
    Handled := FPopup.HandleKey(Key, Shift)
  else
  begin
    Handled := True;
    case Key of
      VK_DOWN:
        if ssAlt in Shift then
          DropDown
        else
          ChangeIndex(FItemIndex + 1);
      VK_UP:
        if not (ssAlt in Shift) then
          ChangeIndex(Max(FItemIndex - 1, 0));
      VK_F4:
        DropDown;
      VK_HOME:
        ChangeIndex(0);
      VK_END:
        ChangeIndex(FItems.Count - 1);
      VK_PRIOR:
        ChangeIndex(Max(FItemIndex - 8, 0));
      VK_NEXT:
        ChangeIndex(FItemIndex + 8);
    else
      Handled := False;
    end;
  end;
  if Handled then
    Key := 0
  else
    inherited KeyDown(Key, Shift);
end;

procedure TERDComboBox.KeyPress(var Key: Char);
begin
  if (Key >= ' ') and (FItems.Count > 0) and not IsOpen then
  begin
    Search(Key);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

procedure TERDComboBox.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Ink: TColor;
  Weight: TERDTextWeight;
  CY: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height),
      (Focused or IsOpen) and not IsPreview, Enabled);
    if Enabled then
      Ink := Palette.ForegroundText
    else
      Ink := ERDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
    if FMono then
      Weight := twMono
    else
      Weight := twRegular;
    CY := Height div 2;
    P.Text(P.S(10), CY, GetText, EDIT_TEXT_SIZE, Ink, Weight, taLeftJustify,
      Width - P.S(36));
    P.GlyphChevron(Width - P.SF(14), CY, Palette.Subtle, True);
  finally
    P.Free;
  end;
end;

end.
