//------------------------------------------------------------------------------
//  OBD.UI.Pickers
//
//  Input pickers descending from the stock VCL edit / combo
//  controls so they inherit Windows visual styles + VCL Styles
//  for free:
//
//    TOBDVINEdit     17-char VIN editor with shape + check-
//                    digit validation + a small valid lamp on
//                    the right edge.
//    TOBDPidPicker   PID combo-box pulling names from the
//                    service catalog. Drop-down hosts PID hex
//                    + display name; the underlying byte
//                    surfaces via the SelectedPID property.
//    TOBDOEMPicker   Combo of known OEMs (VAG / BMW / Ford /
//                    HMG / Honda / Mercedes / Stellantis /
//                    Toyota). Drives the matching coding /
//                    flashing component class selection.
//    TOBDCANIdEdit   Hex CAN-ID editor with 11 / 29-bit toggle
//                    and clamped validation.
//
//  All four are theme- / HiDPI- / VCL-Style-aware through the
//  stock VCL bases they descend from, route every state
//  change through TBindings.Notify, and guard the catalogue
//  lookup paths with csDesigning.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Pickers;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  OBD.Service.VINDecoder,
  OBD.Service.Catalog;

type
  /// <summary>VIN validity tri-state. The lamp on
  /// <see cref="TOBDVINEdit"/> reflects this.</summary>
  TOBDVINValidity = (
    /// <summary>Empty input — neither valid nor invalid.</summary>
    vvEmpty,
    /// <summary>17 characters, valid shape, valid check digit.
    /// </summary>
    vvValid,
    /// <summary>17 characters, valid shape, but the check
    /// digit doesn't match (typical for non-North-American
    /// VINs where the check digit is a filler).</summary>
    vvShapeOnly,
    /// <summary>Less than 17 characters or contains illegal
    /// characters.</summary>
    vvInvalid
  );

  /// <summary>17-character VIN input. Filters illegal
  /// characters at <c>KeyPress</c>, caps the input at 17,
  /// recomputes validity on every <c>Change</c>, and paints
  /// a small status lamp on the right edge of the client
  /// area.</summary>
  TOBDVINEdit = class(TCustomEdit)
  strict private
    FValidity:     TOBDVINValidity;
    FShowLamp:     Boolean;
    FAutoUppercase: Boolean;
    procedure RecomputeValidity;
    procedure NotifyBindings;
    procedure PaintLamp;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>True when the input is a 17-character VIN
    /// (shape valid) — regardless of check-digit match.
    /// </summary>
    function IsShapeValid: Boolean;
    /// <summary>True only when the check digit matches.
    /// </summary>
    function IsCheckDigitValid: Boolean;
    /// <summary>Current validity classification.</summary>
    property Validity: TOBDVINValidity read FValidity;
  published
    /// <summary>Show the small valid-state lamp on the right
    /// edge of the editor. Default True.</summary>
    property ShowLamp: Boolean
      read FShowLamp write FShowLamp default True;
    /// <summary>Force-uppercase user input (VINs are always
    /// upper-case). Default True.</summary>
    property AutoUppercase: Boolean
      read FAutoUppercase write FAutoUppercase default True;

    property Align;
    property Anchors;
    property AutoSelect;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

  /// <summary>PID combo-box. Pulls every catalogued OBD-II
  /// PID from the service catalog and lists them as
  /// <c>"0x0C — Engine RPM"</c>; the byte surfaces via
  /// <see cref="SelectedPID"/>. Host can also seed the list
  /// from a custom byte set via <see cref="SetPIDs"/>.
  /// </summary>
  TOBDPidPicker = class(TCustomComboBox)
  strict private
    FSelected: Byte;
    FHaveSel:  Boolean;
    procedure HandleChange(Sender: TObject);
    procedure NotifyBindings;
    procedure RebuildFromCatalog;
    procedure RememberSelection;
  protected
    procedure Loaded; override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replaces the drop-down items with the supplied
    /// PID set. Each is rendered as <c>"0x.. — name"</c>.
    /// </summary>
    procedure SetPIDs(const APIDs: TArray<Byte>);
    /// <summary>Refreshes the items from the service catalog.
    /// Use after the host loads new catalog files.</summary>
    procedure RefreshFromCatalog;
    /// <summary>Currently-selected PID byte. Read 0 when
    /// nothing is selected; check
    /// <see cref="HasSelection"/> first.</summary>
    property SelectedPID: Byte read FSelected;
    /// <summary>True when an item is selected (not "-1").
    /// </summary>
    property HasSelection: Boolean read FHaveSel;
  published
    property Align;
    property Anchors;
    property AutoComplete default True;
    property AutoDropDown default True;
    property Color;
    property DropDownCount default 16;
    property Enabled;
    property Font;
    property Hint;
    property ItemIndex;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Style default csDropDownList;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDropDown;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  /// <summary>Known OEM identifier. Matches the keys used by
  /// the coding / flashing helpers.</summary>
  TOBDOEM = (
    oemNone,
    oemVAG,
    oemBMW,
    oemFord,
    oemHMG,
    oemHonda,
    oemMercedes,
    oemStellantis,
    oemToyota
  );

  /// <summary>OEM combo-box. Lists every entry in
  /// <see cref="TOBDOEM"/>; host reads
  /// <see cref="SelectedOEM"/> to drive the matching coding
  /// / flashing component class.</summary>
  TOBDOEMPicker = class(TCustomComboBox)
  strict private
    FSelected: TOBDOEM;
    procedure HandleChange(Sender: TObject);
    procedure NotifyBindings;
    procedure RebuildItems;
    function  TextFor(AOEM: TOBDOEM): string;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Programmatic selection. Updates
    /// <see cref="ItemIndex"/> + fires the binding notify.
    /// </summary>
    procedure Select(AOEM: TOBDOEM);
    property SelectedOEM: TOBDOEM read FSelected;
  published
    property Align;
    property Anchors;
    property AutoComplete default True;
    property AutoDropDown default True;
    property Color;
    property DropDownCount default 16;
    property Enabled;
    property Font;
    property Hint;
    property ItemIndex;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Style default csDropDownList;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDropDown;
    property OnEnter;
    property OnExit;
  end;

  /// <summary>Hex CAN-ID editor with 11 / 29-bit validation.
  /// Accepts <c>0x</c> / <c>$</c> prefix or bare hex. Input is
  /// filtered to <c>[0-9A-F]</c> + the prefix tokens; commits
  /// clamp to the active bit width.</summary>
  TOBDCANIdEdit = class(TCustomEdit)
  strict private
    FExtendedId: Boolean;
    FCanId:      Cardinal;
    FInUpdate:   Boolean;
    procedure SetExtendedId(AValue: Boolean);
    procedure SetCanId(AValue: Cardinal);
    procedure RecomputeFromText;
    function  MaxIdValue: Cardinal;
    procedure NotifyBindings;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>True when the current input parses to a valid
    /// CAN ID at the active bit width.</summary>
    function IsValid: Boolean;
  published
    /// <summary>11-bit (False, default) or 29-bit (True) CAN
    /// ID space.</summary>
    property ExtendedId: Boolean
      read FExtendedId write SetExtendedId default False;
    /// <summary>Current parsed CAN ID. Setting this updates
    /// the Text in canonical <c>0xNNN</c> / <c>0xNNNNNNNN</c>
    /// form.</summary>
    property CanId: Cardinal read FCanId write SetCanId;

    property Align;
    property Anchors;
    property AutoSelect;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Cursor;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

implementation

{ ---- TOBDVINEdit -------------------------------------------------------- }

constructor TOBDVINEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  CharCase  := ecUpperCase;
  MaxLength := 17;
  FShowLamp     := True;
  FAutoUppercase := True;
  FValidity := vvEmpty;
end;

procedure TOBDVINEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Reserve a small inset on the right so the lamp doesn't
  // overlap text. Cheap approach: rely on the lamp paint to
  // not clobber the caret since the editor isn't expected to
  // fill all 17 chars worth of space, but the standard
  // single-line edit doesn't let us push the client rect.
  // For production-grade look the lamp paints a translucent
  // pip at the very right edge and is small enough to avoid
  // most overlap.
end;

procedure TOBDVINEdit.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDVINEdit.RecomputeValidity;
var
  T: string;
  ShapeOK: Boolean;
begin
  T := Trim(Text);
  if T = '' then
  begin
    FValidity := vvEmpty;
    Exit;
  end;
  ShapeOK := TOBDVINDecoder.IsValidShape(T);
  if not ShapeOK then
  begin
    FValidity := vvInvalid;
    Exit;
  end;
  if TOBDVINDecoder.IsCheckDigitValid(T) then
    FValidity := vvValid
  else
    FValidity := vvShapeOnly;
end;

procedure TOBDVINEdit.KeyPress(var Key: Char);
begin
  // Allow control codes (backspace, tab, etc.) untouched.
  if Key >= #32 then
  begin
    if FAutoUppercase then Key := UpCase(Key);
    if not (TOBDVINDecoder.IsVINChar(Key) or (Key = ' ')) then
    begin
      Key := #0;
      Exit;
    end;
  end;
  inherited;
end;

procedure TOBDVINEdit.Change;
begin
  RecomputeValidity;
  NotifyBindings;
  inherited;
  Invalidate;
end;

function TOBDVINEdit.IsShapeValid: Boolean;
begin
  Result := FValidity in [vvValid, vvShapeOnly];
end;

function TOBDVINEdit.IsCheckDigitValid: Boolean;
begin
  Result := FValidity = vvValid;
end;

procedure TOBDVINEdit.PaintLamp;
var
  DC:        HDC;
  R:         TRect;
  Brush:     HBRUSH;
  OldBrush:  HBRUSH;
  LampColor: TColor;
  D, X, Y:   Integer;
begin
  if not FShowLamp then Exit;
  if not HandleAllocated then Exit;
  DC := GetDC(Handle);
  try
    R := ClientRect;
    case FValidity of
      vvValid:     LampColor := clGreen;
      vvShapeOnly: LampColor := $0000A8FF;     // amber (BGR)
      vvInvalid:   LampColor := clRed;
    else           LampColor := clGray;
    end;
    D := R.Height - 6;
    if D < 6  then D := 6;
    if D > 14 then D := 14;
    X := R.Right - D - 2;
    Y := R.Top + (R.Height - D) div 2;
    Brush := CreateSolidBrush(ColorToRGB(LampColor));
    try
      OldBrush := SelectObject(DC, Brush);
      Winapi.Windows.Ellipse(DC, X, Y, X + D, Y + D);
      SelectObject(DC, OldBrush);
    finally
      DeleteObject(Brush);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TOBDVINEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  PaintLamp;
end;

{ ---- TOBDPidPicker ------------------------------------------------------ }

constructor TOBDPidPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 220;
  Style := csDropDownList;
  DropDownCount := 16;
  AutoComplete  := True;
  AutoDropDown  := True;
  OnChange      := HandleChange;
end;

procedure TOBDPidPicker.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    RebuildFromCatalog;
end;

procedure TOBDPidPicker.CreateWnd;
begin
  inherited;
  if (Items.Count = 0) and not (csDesigning in ComponentState) then
    RebuildFromCatalog;
end;

procedure TOBDPidPicker.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPidPicker.RememberSelection;
begin
  if ItemIndex >= 0 then
  begin
    FSelected := Byte(NativeInt(Items.Objects[ItemIndex]));
    FHaveSel  := True;
  end
  else
  begin
    FSelected := 0;
    FHaveSel  := False;
  end;
end;

procedure TOBDPidPicker.HandleChange(Sender: TObject);
begin
  RememberSelection;
  NotifyBindings;
end;

procedure TOBDPidPicker.RebuildFromCatalog;
var
  I:    Integer;
  Info: TOBDPIDInfo;
  Line: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    // OBD-II mode-01 PID range is 0x00..0xFF; we walk the
    // whole space and pick out everything the catalog knows
    // about.
    for I := 0 to 255 do
      if TOBDServiceCatalog.Default.TryGetPID(Word(I), Info) then
      begin
        if Info.Name <> '' then
          Line := Format('0x%2.2X — %s', [I, Info.Name])
        else
          Line := Format('0x%2.2X', [I]);
        Items.AddObject(Line, TObject(NativeInt(I)));
      end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDPidPicker.RefreshFromCatalog;
begin
  RebuildFromCatalog;
end;

procedure TOBDPidPicker.SetPIDs(const APIDs: TArray<Byte>);
var
  B:    Byte;
  Info: TOBDPIDInfo;
  Line: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for B in APIDs do
    begin
      if TOBDServiceCatalog.Default.TryGetPID(B, Info) and
         (Info.Name <> '') then
        Line := Format('0x%2.2X — %s', [B, Info.Name])
      else
        Line := Format('0x%2.2X', [B]);
      Items.AddObject(Line, TObject(NativeInt(B)));
    end;
  finally
    Items.EndUpdate;
  end;
end;

{ ---- TOBDOEMPicker ------------------------------------------------------ }

constructor TOBDOEMPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 180;
  Style := csDropDownList;
  DropDownCount := 16;
  AutoComplete  := True;
  AutoDropDown  := True;
  OnChange      := HandleChange;
  FSelected := oemNone;
end;

procedure TOBDOEMPicker.CreateWnd;
begin
  inherited;
  if Items.Count = 0 then RebuildItems;
end;

procedure TOBDOEMPicker.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

function TOBDOEMPicker.TextFor(AOEM: TOBDOEM): string;
begin
  case AOEM of
    oemNone:       Result := '(none)';
    oemVAG:        Result := 'VAG';
    oemBMW:        Result := 'BMW';
    oemFord:       Result := 'Ford';
    oemHMG:        Result := 'Hyundai-Kia';
    oemHonda:      Result := 'Honda';
    oemMercedes:   Result := 'Mercedes-Benz';
    oemStellantis: Result := 'Stellantis';
    oemToyota:     Result := 'Toyota';
  else
    Result := '';
  end;
end;

procedure TOBDOEMPicker.RebuildItems;
var
  O: TOBDOEM;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for O := Low(TOBDOEM) to High(TOBDOEM) do
      Items.AddObject(TextFor(O), TObject(NativeInt(Ord(O))));
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDOEMPicker.HandleChange(Sender: TObject);
begin
  if ItemIndex >= 0 then
    FSelected := TOBDOEM(NativeInt(Items.Objects[ItemIndex]))
  else
    FSelected := oemNone;
  NotifyBindings;
end;

procedure TOBDOEMPicker.Select(AOEM: TOBDOEM);
var
  I: Integer;
begin
  if Items.Count = 0 then RebuildItems;
  for I := 0 to Items.Count - 1 do
    if TOBDOEM(NativeInt(Items.Objects[I])) = AOEM then
    begin
      ItemIndex := I;
      FSelected := AOEM;
      NotifyBindings;
      Exit;
    end;
end;

{ ---- TOBDCANIdEdit ------------------------------------------------------ }

constructor TOBDCANIdEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 160;
  FExtendedId := False;
  FCanId      := 0;
  MaxLength   := 10;     // "0x" + 8 hex digits
  Text        := '0x000';
end;

procedure TOBDCANIdEdit.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

function TOBDCANIdEdit.MaxIdValue: Cardinal;
begin
  if FExtendedId then Result := $1FFFFFFF
  else                Result := $7FF;
end;

procedure TOBDCANIdEdit.SetExtendedId(AValue: Boolean);
begin
  if FExtendedId = AValue then Exit;
  FExtendedId := AValue;
  if FExtendedId then MaxLength := 10
  else                MaxLength := 5;
  // Re-clamp + re-format in the active bit width.
  SetCanId(System.Math.Min(FCanId, MaxIdValue));
  NotifyBindings;
end;

procedure TOBDCANIdEdit.SetCanId(AValue: Cardinal);
begin
  if AValue > MaxIdValue then AValue := MaxIdValue;
  FCanId := AValue;
  FInUpdate := True;
  try
    if FExtendedId then
      Text := Format('0x%8.8X', [FCanId])
    else
      Text := Format('0x%3.3X', [FCanId]);
  finally
    FInUpdate := False;
  end;
  NotifyBindings;
end;

procedure TOBDCANIdEdit.KeyPress(var Key: Char);
begin
  // Control codes pass through.
  if Key < #32 then
  begin
    inherited;
    Exit;
  end;
  Key := UpCase(Key);
  // Accept hex digits, the prefix characters ($ / x / X), and
  // the leading zero of "0x".
  if not (CharInSet(Key, ['0'..'9', 'A'..'F', '$', 'X'])) then
  begin
    Key := #0;
    Exit;
  end;
  inherited;
end;

procedure TOBDCANIdEdit.RecomputeFromText;
var
  T:  string;
  V:  Integer;
begin
  T := Trim(Text);
  if T = '' then
  begin
    FCanId := 0;
    Exit;
  end;
  if (Length(T) > 2) and (Copy(T, 1, 2).ToLower = '0x') then
    T := '$' + Copy(T, 3, MaxInt);
  if TryStrToInt(T, V) and (V >= 0) then
  begin
    if Cardinal(V) <= MaxIdValue then
      FCanId := Cardinal(V);
  end;
end;

procedure TOBDCANIdEdit.Change;
begin
  inherited;
  if FInUpdate then Exit;
  RecomputeFromText;
  NotifyBindings;
end;

procedure TOBDCANIdEdit.DoExit;
begin
  inherited;
  // Canonicalise the display on focus loss — "12" reformats
  // to "0x012" / "0x00000012" depending on the bit width.
  SetCanId(FCanId);
end;

function TOBDCANIdEdit.IsValid: Boolean;
var
  T: string;
  V: Integer;
begin
  T := Trim(Text);
  if T = '' then Exit(False);
  if (Length(T) > 2) and (Copy(T, 1, 2).ToLower = '0x') then
    T := '$' + Copy(T, 3, MaxInt);
  Result := TryStrToInt(T, V) and (V >= 0) and
            (Cardinal(V) <= MaxIdValue);
end;

end.
