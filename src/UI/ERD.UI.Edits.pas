//------------------------------------------------------------------------------
//  ERD.UI.Edits
//
//  Themed text entry and pick list for the OBD Studio controls.
//
//    TOBDEdit      single-line edit: a borderless VCL edit inside a
//                  painted frame (1 px border, 2 px orange when
//                  focused, background fill when disabled). Mono
//                  switches to Consolas for VINs, CAN IDs and codes.
//    TOBDComboBox  drop-down list (no free text): the current item and
//                  a chevron; click, Alt+Down or F4 opens a themed
//                  TOBDPopupList. While closed the arrow keys step
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
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Edits;

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
  Vcl.StdCtrls,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.PopupList;

type
  /// <summary>Themed single-line edit.</summary>
  TOBDEdit = class(TOBDCustomControl)
  strict private
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
    procedure InnerChange(Sender: TObject);
    procedure InnerFocusChange(Sender: TObject);
    procedure InnerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InnerKeyPress(Sender: TObject; var Key: Char);
    procedure LayoutInner;
    procedure SyncInnerLook;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
  protected
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
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the text changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed drop-down list.</summary>
  TOBDComboBox = class(TOBDCustomControl)
  strict private
    FItems: TStrings;
    FItemIndex: Integer;
    FPopup: TOBDPopupList;
    FHover: Boolean;
    FMono: Boolean;
    FSearch: string;
    FSearchTick: Cardinal;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure SetItems(AValue: TStrings);
    procedure SetItemIndex(AValue: Integer);
    function GetText: string;
    procedure SetMono(AValue: Boolean);
    procedure ItemsChanged(Sender: TObject);
    procedure PopupCloseUp(Sender: TObject; AAccepted: Boolean;
      AIndex: Integer);
    procedure ChangeIndex(AIndex: Integer);
    procedure Search(AChar: Char);
    function IsOpen: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
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

const
  EDIT_TEXT_SIZE = 12.5;
  SEARCH_RESET_MS = 1000;

function TextFontHeight(ASize: Single; APPI: Integer): Integer;
begin
  Result := -Round(ASize * APPI / 96);
end;

{ TOBDEdit }

constructor TOBDEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 180;
  Height := 26;
  TabStop := False;
  FEdit := TEdit.Create(Self);
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

function TOBDEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TOBDEdit.SetText(const AValue: string);
begin
  FEdit.Text := AValue;
end;

function TOBDEdit.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;

procedure TOBDEdit.SetAlignment(AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
end;

function TOBDEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TOBDEdit.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

function TOBDEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

procedure TOBDEdit.SetMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

function TOBDEdit.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

procedure TOBDEdit.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

function TOBDEdit.GetEditTextHint: string;
begin
  Result := FEdit.TextHint;
end;

procedure TOBDEdit.SetEditTextHint(const AValue: string);
begin
  FEdit.TextHint := AValue;
end;

function TOBDEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

procedure TOBDEdit.SetSelStart(AValue: Integer);
begin
  FEdit.SelStart := AValue;
end;

function TOBDEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

procedure TOBDEdit.SetSelLength(AValue: Integer);
begin
  FEdit.SelLength := AValue;
end;

procedure TOBDEdit.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  LayoutInner;
end;

procedure TOBDEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TOBDEdit.SetFocus;
begin
  if FEdit.CanFocus then
    FEdit.SetFocus
  else
    inherited SetFocus;
end;

function TOBDEdit.Focused: Boolean;
begin
  Result := FEdit.Focused;
end;

procedure TOBDEdit.InnerChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDEdit.InnerFocusChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDEdit.InnerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TOBDEdit.InnerKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;

procedure TOBDEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FEdit.Enabled := Enabled;
  SyncInnerLook;
  Invalidate;
end;

procedure TOBDEdit.SyncInnerLook;
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
    Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
  end;
  if FEdit.Color <> Back then
    FEdit.Color := Back;
  if FEdit.Font.Color <> Ink then
    FEdit.Font.Color := Ink;
end;

procedure TOBDEdit.LayoutInner;
var
  FontH, Pad, EH: Integer;
begin
  if FEdit = nil then
    Exit;
  if FMono then
    FEdit.Font.Name := 'Consolas'
  else
    FEdit.Font.Name := 'Segoe UI';
  FontH := TextFontHeight(EDIT_TEXT_SIZE, ScaleValue(96));
  if FEdit.Font.Height <> FontH then
    FEdit.Font.Height := FontH;
  Pad := ScaleValue(10);
  EH := Abs(FontH) + ScaleValue(6);
  FEdit.SetBounds(Pad, Max(ScaleValue(2), (Height - EH) div 2),
    Max(0, Width - 2 * Pad), Min(EH, Height - ScaleValue(4)));
  SyncInnerLook;
end;

procedure TOBDEdit.Resize;
begin
  inherited Resize;
  LayoutInner;
end;

procedure TOBDEdit.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  LayoutInner;
  inherited DensityChanged;
end;

function TOBDEdit.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TOBDEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and FEdit.CanFocus then
    FEdit.SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TOBDEdit.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  SyncInnerLook;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height), FEdit.Focused and not IsPreview,
      Enabled);
  finally
    P.Free;
  end;
end;

{ TOBDComboBox }

constructor TOBDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FItemIndex := -1;
  Width := 180;
  Height := 26;
  TabStop := True;
  AutoSize := True;
end;

destructor TOBDComboBox.Destroy;
begin
  if FPopup <> nil then
    FPopup.OnCloseUp := nil;
  FItems.Free;
  inherited Destroy;
end;

procedure TOBDComboBox.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

procedure TOBDComboBox.ItemsChanged(Sender: TObject);
begin
  if FItemIndex >= FItems.Count then
    FItemIndex := FItems.Count - 1;
  Invalidate;
end;

procedure TOBDComboBox.SetItemIndex(AValue: Integer);
begin
  if not (csLoading in ComponentState) then
    AValue := EnsureRange(AValue, -1, FItems.Count - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
end;

function TOBDComboBox.GetText: string;
begin
  if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
    Result := FItems[FItemIndex]
  else
    Result := '';
end;

procedure TOBDComboBox.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  Invalidate;
end;

procedure TOBDComboBox.ChangeIndex(AIndex: Integer);
begin
  AIndex := EnsureRange(AIndex, -1, FItems.Count - 1);
  if AIndex = FItemIndex then
    Exit;
  FItemIndex := AIndex;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TOBDComboBox.IsOpen: Boolean;
begin
  Result := (FPopup <> nil) and FPopup.IsOpen;
end;

procedure TOBDComboBox.DropDown;
var
  TopLeft, BottomRight: TPoint;
begin
  if IsOpen or not Enabled or (FItems.Count = 0) then
    Exit;
  if FPopup = nil then
  begin
    FPopup := TOBDPopupList.Create(Self);
    FPopup.OnCloseUp := PopupCloseUp;
  end;
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
  TopLeft := ClientToScreen(Point(0, 0));
  BottomRight := ClientToScreen(Point(Width, Height));
  FPopup.Popup(Self, Rect(TopLeft.X, TopLeft.Y, BottomRight.X, BottomRight.Y),
    FItems, FItemIndex, Palette, ScaleValue(96),
    ScaleValue(Metrics.CompactRow));
  Invalidate;
end;

procedure TOBDComboBox.CloseUp(AAccept: Boolean);
begin
  if IsOpen then
    FPopup.CloseUp(AAccept);
end;

procedure TOBDComboBox.PopupCloseUp(Sender: TObject; AAccepted: Boolean;
  AIndex: Integer);
begin
  if AAccepted and (AIndex >= 0) then
    ChangeIndex(AIndex);
  Invalidate;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TOBDComboBox.Search(AChar: Char);
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

procedure TOBDComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TOBDComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TOBDComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
    CloseUp(False);
  Invalidate;
end;

procedure TOBDComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPopup) then
    CloseUp(False);
end;

procedure TOBDComboBox.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if IsOpen and ((Message.CharCode = VK_RETURN) or
    (Message.CharCode = VK_ESCAPE)) then
    Message.Result := 1
  else
    inherited;
end;

procedure TOBDComboBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TOBDComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;

procedure TOBDComboBox.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TOBDComboBox.DoExit;
begin
  CloseUp(False);
  inherited DoExit;
  Invalidate;
end;

procedure TOBDComboBox.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

function TOBDComboBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewHeight := ScaleValue(Metrics.Edit);
end;

procedure TOBDComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
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

procedure TOBDComboBox.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TOBDComboBox.KeyPress(var Key: Char);
begin
  if (Key >= ' ') and (FItems.Count > 0) and not IsOpen then
  begin
    Search(Key);
    Key := #0;
  end;
  inherited KeyPress(Key);
end;

procedure TOBDComboBox.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Ink: TColor;
  Weight: TOBDTextWeight;
  CY: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.EditFrame(Rect(0, 0, Width, Height),
      (Focused or IsOpen) and not IsPreview, Enabled);
    if FHover and Enabled and not Focused and not IsOpen then
      P.FrameRect(Rect(0, 0, Width, Height), Palette.NeutralDark);
    if Enabled then
      Ink := Palette.ForegroundText
    else
      Ink := OBDMixColor(Palette.Subtle, Palette.GaugeFace, 0.6);
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
