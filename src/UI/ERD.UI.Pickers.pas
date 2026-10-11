// ------------------------------------------------------------------------------
// ERD.UI.Pickers
//
// Input pickers built on the themed OBD Studio edit and combo box
// (ERD.UI.Edits), so they share the theme, density, focus frame,
// drop-down list and scroll bars of every other control - no native
// Windows edit borders, combo boxes or scroll bars:
//
// TOBDVINEdit     17-char VIN editor (TOBDEdit) with shape + check-
//                 digit validation + a themed validity lamp on the
//                 right edge.
// TOBDPidPicker   PID combo box (TOBDComboBox) pulling names from the
//                 service catalog. The list shows PID hex + display
//                 name; the byte surfaces via SelectedPID.
// TOBDOEMPicker   Combo of known OEMs (VAG / BMW / Ford / HMG / Honda /
//                 Mercedes / Stellantis / Toyota). Drives the matching
//                 coding / flashing component class selection.
// TOBDCANIdEdit   Hex CAN-ID editor (TOBDEdit) with 11 / 29-bit toggle
//                 and clamped validation.
//
// All four route every state change through TBindings.Notify and
// guard the catalogue lookup paths with csDesigning.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.UI.Pickers;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  System.Math,
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  System.Bindings.Helper, Data.Bind.Components,
  Vcl.Graphics,
  Vcl.StdCtrls,
  ERD.UI.Control,
  ERD.UI.Edits,
  ERD.Service.VINDecoder,
  ERD.Service.Catalog;

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
    vvInvalid);

  /// <summary>17-character VIN input on the themed edit. Filters
  /// illegal characters at <c>KeyPress</c>, caps the input at 17,
  /// recomputes validity on every <c>Change</c>, and paints a
  /// themed status lamp on the right edge of the field.</summary>
  TOBDVINEdit = class(TOBDEdit)
  strict private
    FValidity: TOBDVINValidity;
    FShowLamp: Boolean;
    FAutoUppercase: Boolean;
    procedure SetShowLamp(AValue: Boolean);
    procedure RecomputeValidity;
    procedure NotifyBindings;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    function TrailingSpace: Integer; override;
    procedure PaintControl(ACanvas: TCanvas); override;
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
    property ShowLamp: Boolean read FShowLamp write SetShowLamp default True;
    /// <summary>Force-uppercase user input (VINs are always
    /// upper-case). Default True.</summary>
    property AutoUppercase: Boolean read FAutoUppercase write FAutoUppercase
      default True;
    /// <summary>A VIN is 17 characters.</summary>
    property MaxLength default 17;
    /// <summary>VINs are upper-case.</summary>
    property CharCase default ecUpperCase;
    /// <summary>VINs read best in Consolas.</summary>
    property Mono default True;
  end;

  /// <summary>PID combo box on the themed combo. Pulls every
  /// catalogued OBD-II PID from the service catalog and lists them
  /// as <c>"0x0C — Engine RPM"</c>; the byte surfaces via
  /// <see cref="SelectedPID"/>. Host can also seed the list from a
  /// custom byte set via <see cref="SetPIDs"/>.</summary>
  TOBDPidPicker = class(TOBDComboBox)
  strict private
    procedure NotifyBindings;
    procedure RebuildFromCatalog;
    function GetSelectedPID: Byte;
    function GetHasSelection: Boolean;
  protected
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure Change; override;
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
    property SelectedPID: Byte read GetSelectedPID;
    /// <summary>True when an item is selected (not "-1").
    /// </summary>
    property HasSelection: Boolean read GetHasSelection;
  published
    /// <summary>Items come from the catalog or
    /// <see cref="SetPIDs"/>, so they are not streamed.</summary>
    property Items stored False;
    /// <summary>The PID list is long: show 16 rows.</summary>
    property DropDownCount default 16;
    /// <summary>PID codes read best in Consolas.</summary>
    property Mono default True;
  end;

  /// <summary>Known OEM identifier. Matches the keys used by
  /// the coding / flashing helpers.</summary>
  TOBDOEM = (oemNone, oemVAG, oemBMW, oemFord, oemHMG, oemHonda, oemMercedes,
    oemStellantis, oemToyota);

  /// <summary>OEM combo box on the themed combo. Lists every entry
  /// in <see cref="TOBDOEM"/>; host reads
  /// <see cref="SelectedOEM"/> to drive the matching coding
  /// / flashing component class.</summary>
  TOBDOEMPicker = class(TOBDComboBox)
  strict private
    procedure NotifyBindings;
    procedure RebuildItems;
    function TextFor(AOEM: TOBDOEM): string;
    function GetSelectedOEM: TOBDOEM;
  protected
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Programmatic selection. Updates
    /// <see cref="ItemIndex"/> + fires the binding notify.
    /// </summary>
    procedure Select(AOEM: TOBDOEM);
    /// <summary>OEM of the selected item; oemNone when nothing is
    /// selected.</summary>
    property SelectedOEM: TOBDOEM read GetSelectedOEM;
  published
    /// <summary>The OEM list is fixed, so it is not streamed.</summary>
    property Items stored False;
    /// <summary>Shows every OEM without scrolling.</summary>
    property DropDownCount default 16;
  end;

  /// <summary>Hex CAN-ID editor on the themed edit, with 11 / 29-bit
  /// validation. Accepts <c>0x</c> / <c>$</c> prefix or bare hex.
  /// Input is filtered to <c>[0-9A-F]</c> + the prefix tokens;
  /// commits clamp to the active bit width.</summary>
  TOBDCANIdEdit = class(TOBDEdit)
  strict private
    FExtendedId: Boolean;
    FCanId: Cardinal;
    FInUpdate: Boolean;
    procedure SetExtendedId(AValue: Boolean);
    procedure SetCanId(AValue: Cardinal);
    procedure RecomputeFromText;
    function MaxIdValue: Cardinal;
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
    property ExtendedId: Boolean read FExtendedId write SetExtendedId
      default False;
    /// <summary>Current parsed CAN ID. Setting this updates
    /// the Text in canonical <c>0xNNN</c> / <c>0xNNNNNNNN</c>
    /// form.</summary>
    property CanId: Cardinal read FCanId write SetCanId default 0;
    /// <summary>"0x" + 3 hex digits (11-bit) or 8 (29-bit); follows
    /// <see cref="ExtendedId"/>.</summary>
    property MaxLength stored False;
    /// <summary>CAN IDs read best in Consolas.</summary>
    property Mono default True;
  end;

implementation

uses
  ERD.UI.Paint;

{ ---- TOBDVINEdit -------------------------------------------------------- }

constructor TOBDVINEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Mono := True;
  CharCase := ecUpperCase;
  MaxLength := 17;
  FShowLamp := True;
  FAutoUppercase := True;
  FValidity := vvEmpty;
  LayoutInner;
end;

procedure TOBDVINEdit.SetShowLamp(AValue: Boolean);
begin
  if FShowLamp = AValue then
    Exit;
  FShowLamp := AValue;
  LayoutInner;
  Invalidate;
end;

procedure TOBDVINEdit.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then
    Exit;
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
    if FAutoUppercase then
      Key := UpCase(Key);
    if not(TOBDVINDecoder.IsVINChar(Key) or (Key = ' ')) then
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

function TOBDVINEdit.TrailingSpace: Integer;
begin
  // Lamp diameter plus a gap to the text.
  if FShowLamp then
    Result := ScaleValue(16)
  else
    Result := 0;
end;

procedure TOBDVINEdit.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  LampColor: TColor;
  D, X, Y: Single;
begin
  inherited PaintControl(ACanvas);
  if not FShowLamp then
    Exit;
  case FValidity of
    vvValid:
      LampColor := Palette.Success;
    vvShapeOnly:
      LampColor := Palette.Warning;
    vvInvalid:
      LampColor := Palette.Danger;
  else
    LampColor := Palette.NeutralLight;
  end;
  if not Enabled then
    LampColor := OBDMixColor(LampColor, Palette.Background, 0.5);
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    D := ScaleValue(8);
    X := Width - ScaleValue(10) - D;
    Y := (Height - D) / 2;
    P.Ellipse(X, Y, D, D, LampColor, clNone);
  finally
    P.Free;
  end;
end;

{ ---- TOBDPidPicker ------------------------------------------------------ }

constructor TOBDPidPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 220;
  Mono := True;
  DropDownCount := 16;
end;

procedure TOBDPidPicker.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RebuildFromCatalog;
end;

procedure TOBDPidPicker.CreateWnd;
begin
  inherited;
  if (Items.Count = 0) and not([csDesigning, csLoading] * ComponentState <> [])
  then
    RebuildFromCatalog;
end;

procedure TOBDPidPicker.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then
    Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

function TOBDPidPicker.GetHasSelection: Boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Items.Count);
end;

function TOBDPidPicker.GetSelectedPID: Byte;
begin
  if GetHasSelection then
    Result := Byte(NativeInt(Items.Objects[ItemIndex]))
  else
    Result := 0;
end;

procedure TOBDPidPicker.Change;
begin
  NotifyBindings;
  inherited;
end;

procedure TOBDPidPicker.RebuildFromCatalog;
var
  I: Integer;
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
  B: Byte;
  Info: TOBDPIDInfo;
  Line: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for B in APIDs do
    begin
      if TOBDServiceCatalog.Default.TryGetPID(B, Info) and (Info.Name <> '')
      then
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
  DropDownCount := 16;
  RebuildItems;
end;

procedure TOBDOEMPicker.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then
    Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

function TOBDOEMPicker.TextFor(AOEM: TOBDOEM): string;
begin
  case AOEM of
    oemNone:
      Result := '(none)';
    oemVAG:
      Result := 'VAG';
    oemBMW:
      Result := 'BMW';
    oemFord:
      Result := 'Ford';
    oemHMG:
      Result := 'Hyundai-Kia';
    oemHonda:
      Result := 'Honda';
    oemMercedes:
      Result := 'Mercedes-Benz';
    oemStellantis:
      Result := 'Stellantis';
    oemToyota:
      Result := 'Toyota';
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

function TOBDOEMPicker.GetSelectedOEM: TOBDOEM;
begin
  if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
    Result := TOBDOEM(NativeInt(Items.Objects[ItemIndex]))
  else
    Result := oemNone;
end;

procedure TOBDOEMPicker.Change;
begin
  NotifyBindings;
  inherited;
end;

procedure TOBDOEMPicker.Select(AOEM: TOBDOEM);
var
  I: Integer;
begin
  if Items.Count = 0 then
    RebuildItems;
  for I := 0 to Items.Count - 1 do
    if TOBDOEM(NativeInt(Items.Objects[I])) = AOEM then
    begin
      ItemIndex := I;
      NotifyBindings;
      Exit;
    end;
end;

{ ---- TOBDCANIdEdit ------------------------------------------------------ }

constructor TOBDCANIdEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 160;
  Mono := True;
  FExtendedId := False;
  FCanId := 0;
  MaxLength := 5; // "0x" + 3 hex digits (11-bit)
  Text := '0x000';
end;

procedure TOBDCANIdEdit.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then
    Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

function TOBDCANIdEdit.MaxIdValue: Cardinal;
begin
  if FExtendedId then
    Result := $1FFFFFFF
  else
    Result := $7FF;
end;

procedure TOBDCANIdEdit.SetExtendedId(AValue: Boolean);
begin
  if FExtendedId = AValue then
    Exit;
  FExtendedId := AValue;
  if FExtendedId then
    MaxLength := 10
  else
    MaxLength := 5;
  // Re-clamp + re-format in the active bit width.
  SetCanId(System.Math.Min(FCanId, MaxIdValue));
  NotifyBindings;
end;

procedure TOBDCANIdEdit.SetCanId(AValue: Cardinal);
begin
  if AValue > MaxIdValue then
    AValue := MaxIdValue;
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
  if not(CharInSet(Key, ['0' .. '9', 'A' .. 'F', '$', 'X'])) then
  begin
    Key := #0;
    Exit;
  end;
  inherited;
end;

procedure TOBDCANIdEdit.RecomputeFromText;
var
  T: string;
  V: Integer;
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
  // Parse first so an OnChange handler reads the new CanId.
  if not FInUpdate then
  begin
    RecomputeFromText;
    NotifyBindings;
  end;
  inherited;
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
  if T = '' then
    Exit(False);
  if (Length(T) > 2) and (Copy(T, 1, 2).ToLower = '0x') then
    T := '$' + Copy(T, 3, MaxInt);
  Result := TryStrToInt(T, V) and (V >= 0) and (Cardinal(V) <= MaxIdValue);
end;

end.
