//------------------------------------------------------------------------------
//  ERD.UI.FontDialog
//
//  Themed font picker dialog for OBD Studio.
//
//    TOBDFontDialog  themed TFontDialog replacement built on
//                    TOBDCommonDialog. It exposes a TFont in/out value,
//                    useful font options, font family search, style and size
//                    lists, optional effects and a live sample.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation of the themed font dialog.
//------------------------------------------------------------------------------

unit ERD.UI.FontDialog;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Dialogs,
  ERD.UI.ItemList;

type
  /// <summary>Options supported by <see cref="TOBDFontDialog"/>.</summary>
  TOBDFontDialogOption = (
    /// <summary>Shows underline, strikeout and colour controls.</summary>
    fdoEffects,
    /// <summary>Only lists fixed-pitch fonts.</summary>
    fdoFixedPitchOnly,
    /// <summary>Only lists scalable TrueType or vector fonts.</summary>
    fdoScalableOnly);

  /// <summary>Set of <see cref="TOBDFontDialogOption"/> values.</summary>
  TOBDFontDialogOptions = set of TOBDFontDialogOption;

  /// <summary>Themed replacement for <c>TFontDialog</c>.</summary>
  TOBDFontDialog = class(TOBDCommonDialog)
  strict private
    FFont: TFont;
    FOptions: TOBDFontDialogOptions;
    FMinFontSize: Integer;
    FMaxFontSize: Integer;
    FOnApply: TNotifyEvent;
    procedure SetFont(AValue: TFont);
    procedure SetMinFontSize(AValue: Integer);
    procedure SetMaxFontSize(AValue: Integer);
  protected
    /// <summary>Title used when <see cref="Title"/> is empty.</summary>
    /// <returns>Default title.</returns>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates the dialog and its private font value.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the private font value.</summary>
    destructor Destroy; override;
    /// <summary>Shows the themed font dialog.</summary>
    /// <returns>True when the user accepts the dialog.</returns>
    function Execute: Boolean; override;
  published
    /// <summary>Font value passed in and out of the dialog.</summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>Options that affect the visible font list and effects.</summary>
    property Options: TOBDFontDialogOptions read FOptions write FOptions;
    /// <summary>Smallest font size offered or accepted; 0 = no minimum.</summary>
    property MinFontSize: Integer read FMinFontSize write SetMinFontSize
      default 0;
    /// <summary>Largest font size offered or accepted; 0 = no maximum.</summary>
    property MaxFontSize: Integer read FMaxFontSize write SetMaxFontSize
      default 0;
    /// <summary>Fires when the Apply button is pressed.</summary>
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
    /// <summary>Lets the user resize the dialog; the lists grow with it.
    /// </summary>
    property Resizable default True;
  end;

implementation

const
  FONT_BODY_W = 620;
  FONT_ROWS = 7;
  SAMPLE_TEXT = 'P0401  EGR flow insufficient';

type
  TFontFamilyList = class(TOBDCustomItemList)
  strict private
    FAll: TStringList;
    FItems: TStringList;
    FOptions: TOBDFontDialogOptions;
    FFilter: string;
    procedure SetFilter(const AValue: string);
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(AOptions: TOBDFontDialogOptions);
    function IndexOfFamily(const AName: string): Integer;
    procedure SelectFamily(const AName: string);
    property Filter: string read FFilter write SetFilter;
  end;

  TFontStyleList = class(TOBDCustomItemList)
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
  public
    function SelectedStyle: TFontStyles;
    procedure SelectStyle(AStyle: TFontStyles);
  end;

  TFontSizeList = class(TOBDCustomItemList)
  strict private
    FItems: TStringList;
    FMinSize: Integer;
    FMaxSize: Integer;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(AMinSize, AMaxSize: Integer);
    procedure SelectSize(ASize: Integer);
    function SelectedSize: Integer;
  end;

  TFontColorRow = class(TOBDCustomControl)
  strict private
    FSelected: Integer;
    FOnChange: TNotifyEvent;
    procedure SetSelected(AValue: Integer);
    function SwatchRect(AIndex: Integer): TRect;
    function ColorAt(AIndex: Integer): TColor;
    function NameAt(AIndex: Integer): string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner: TComponent); override;
    function SelectedColor: TColor;
    procedure SelectColor(AColor: TColor);
    property Selected: Integer read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFontPreview = class(TOBDCustomControl)
  strict private
    FSampleFont: TFont;
    FSampleText: string;
    procedure SetSampleText(const AValue: string);
    procedure SetSampleFont(AValue: TFont);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SampleFont: TFont read FSampleFont write SetSampleFont;
    property SampleText: string read FSampleText write SetSampleText;
  end;

  TFontDialogController = class(TComponent)
  strict private
    FDialog: TOBDFontDialog;
    FFamily: TFontFamilyList;
    FSearch: TOBDEdit;
    FStyle: TFontStyleList;
    FStyleEdit: TOBDEdit;
    FSizeList: TFontSizeList;
    FSizeEdit: TOBDEdit;
    FUnderline: TOBDCheckBox;
    FStrikeout: TOBDCheckBox;
    FColorRow: TFontColorRow;
    FPreview: TFontPreview;
    FUpdating: Boolean;
    procedure FamilyChange(Sender: TObject);
    procedure FamilyActivate(Sender: TObject);
    procedure SearchChange(Sender: TObject);
    procedure StyleChange(Sender: TObject);
    procedure StyleEditChange(Sender: TObject);
    procedure SizeChange(Sender: TObject);
    procedure SizeEditChange(Sender: TObject);
    procedure EffectsChange(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    function CurrentSize: Integer;
  public
    constructor CreateController(AOwner: TComponent; ADialog: TOBDFontDialog);
    procedure Build(AWindow: TOBDDialogWindow);
    procedure LoadFromFont(AFont: TFont);
    procedure ApplyToFont(AFont: TFont);
    procedure UpdatePreview;
  end;

const
  STYLE_NAMES: array[0..3] of string = (
    'Regular', 'Italic', 'Bold', 'Bold Italic');
  COMMON_SIZES: array[0..15] of Integer = (
    8, 9, 10, 11, 12, 14, 16, 18, 20, 24, 28, 32, 36, 48, 72, 96);

function SameColor(A, B: TColor): Boolean;
begin
  Result := ColorToRGB(A) = ColorToRGB(B);
end;

function TextFontHeight(ASize: Single; APPI: Integer): Integer;
begin
  Result := -Round(ASize * APPI / 96);
end;

function FontMatchesOptions(const AName: string;
  AOptions: TOBDFontDialogOptions): Boolean;
var
  Bitmap: TBitmap;
  TextMetric: TTextMetric;
  Pitch: Byte;
begin
  Result := True;
  if not ((fdoFixedPitchOnly in AOptions) or
    (fdoScalableOnly in AOptions)) then
    Exit;

  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Name := AName;
    Bitmap.Canvas.Font.Size := 10;
    if not GetTextMetrics(Bitmap.Canvas.Handle, TextMetric) then
      Exit;
    Pitch := TextMetric.tmPitchAndFamily;
    if fdoFixedPitchOnly in AOptions then
      Result := Result and ((Pitch and TMPF_FIXED_PITCH) = 0);
    if fdoScalableOnly in AOptions then
      Result := Result and
        ((Pitch and (TMPF_TRUETYPE or TMPF_VECTOR)) <> 0);
  finally
    Bitmap.Free;
  end;
end;

function ClampFontSize(AValue, AMinSize, AMaxSize: Integer): Integer;
begin
  Result := AValue;
  if (AMinSize > 0) and (Result < AMinSize) then
    Result := AMinSize;
  if (AMaxSize > 0) and (Result > AMaxSize) then
    Result := AMaxSize;
  if Result <= 0 then
    Result := 1;
end;

{ TFontFamilyList }

constructor TFontFamilyList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAll := TStringList.Create;
  FItems := TStringList.Create;
  FAll.Sorted := True;
  FAll.Duplicates := dupIgnore;
  FItems.Sorted := True;
  FItems.Duplicates := dupIgnore;
  EmptyText := 'No fonts';
end;

destructor TFontFamilyList.Destroy;
begin
  FItems.Free;
  FAll.Free;
  inherited;
end;

procedure TFontFamilyList.Populate(AOptions: TOBDFontDialogOptions);
var
  I: Integer;
  S: string;
begin
  FOptions := AOptions;
  FAll.Clear;
  for I := 0 to Screen.Fonts.Count - 1 do
  begin
    S := Screen.Fonts[I];
    if (S <> '') and (S[1] <> '@') and FontMatchesOptions(S, FOptions) then
      FAll.Add(S);
  end;
  SetFilter(FFilter);
end;

procedure TFontFamilyList.SetFilter(const AValue: string);
var
  I: Integer;
begin
  FFilter := AValue;
  FItems.BeginUpdate;
  try
    FItems.Clear;
    for I := 0 to FAll.Count - 1 do
      if (FFilter = '') or ContainsText(FAll[I], FFilter) then
        FItems.Add(FAll[I]);
  finally
    FItems.EndUpdate;
  end;
  ItemIndex := -1;
  ItemsChanged;
end;

function TFontFamilyList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TFontFamilyList.GetItemText(AIndex: Integer): string;
begin
  Result := FItems[AIndex];
end;

function TFontFamilyList.ItemHeight: Integer;
begin
  Result := ScaleValue(Metrics.Edit);
end;

procedure TFontFamilyList.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  R: TRect;
  Flags: Cardinal;
begin
  R := ARect;
  InflateRect(R, -ScaleValue(10), 0);
  APainter.Canvas.Brush.Style := bsClear;
  APainter.Canvas.Font.Name := GetItemText(AIndex);
  APainter.Canvas.Font.Height := TextFontHeight(OBD_TEXT_BODY,
    ScaleValue(96));
  APainter.Canvas.Font.Style := [];
  APainter.Canvas.Font.Color := Palette.ForegroundText;
  Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
  DrawText(APainter.Canvas.Handle, PChar(GetItemText(AIndex)), -1, R, Flags);
end;

function TFontFamilyList.IndexOfFamily(const AName: string): Integer;
begin
  Result := FItems.IndexOf(AName);
end;

procedure TFontFamilyList.SelectFamily(const AName: string);
var
  Index: Integer;
begin
  Index := IndexOfFamily(AName);
  if (Index < 0) and (FAll.IndexOf(AName) >= 0) then
  begin
    Filter := '';
    Index := IndexOfFamily(AName);
  end;
  if (Index < 0) and (GetItemCount > 0) then
    Index := 0;
  ItemIndex := Index;
end;

{ TFontStyleList }

function TFontStyleList.GetItemCount: Integer;
begin
  Result := Length(STYLE_NAMES);
end;

function TFontStyleList.GetItemText(AIndex: Integer): string;
begin
  Result := STYLE_NAMES[AIndex];
end;

function TFontStyleList.ItemHeight: Integer;
begin
  Result := ScaleValue(Metrics.Edit);
end;

procedure TFontStyleList.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  Weight: TOBDTextWeight;
  TextStyle: TFontStyles;
  R: TRect;
  Flags: Cardinal;
begin
  Weight := twRegular;
  TextStyle := [];
  if AIndex in [1, 3] then
    Include(TextStyle, fsItalic);
  if AIndex in [2, 3] then
  begin
    Include(TextStyle, fsBold);
    Weight := twBold;
  end;
  R := ARect;
  InflateRect(R, -ScaleValue(10), 0);
  APainter.Canvas.Brush.Style := bsClear;
  APainter.Canvas.Font.Name := 'Segoe UI';
  APainter.Canvas.Font.Height := TextFontHeight(OBD_TEXT_BODY,
    ScaleValue(96));
  APainter.Canvas.Font.Style := TextStyle;
  APainter.Canvas.Font.Color := Palette.ForegroundText;
  Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
  DrawText(APainter.Canvas.Handle, PChar(GetItemText(AIndex)), -1, R, Flags);
  if Weight = twBold then
    APainter.Canvas.Font.Style := [];
end;

function TFontStyleList.SelectedStyle: TFontStyles;
begin
  case ItemIndex of
    1: Result := [fsItalic];
    2: Result := [fsBold];
    3: Result := [fsBold, fsItalic];
  else
    Result := [];
  end;
end;

procedure TFontStyleList.SelectStyle(AStyle: TFontStyles);
begin
  if (fsBold in AStyle) and (fsItalic in AStyle) then
    ItemIndex := 3
  else if fsBold in AStyle then
    ItemIndex := 2
  else if fsItalic in AStyle then
    ItemIndex := 1
  else
    ItemIndex := 0;
end;

{ TFontSizeList }

constructor TFontSizeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
end;

destructor TFontSizeList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TFontSizeList.Populate(AMinSize, AMaxSize: Integer);
var
  I, N: Integer;
begin
  FMinSize := AMinSize;
  FMaxSize := AMaxSize;
  FItems.Clear;
  for I := Low(COMMON_SIZES) to High(COMMON_SIZES) do
  begin
    N := COMMON_SIZES[I];
    if ((FMinSize <= 0) or (N >= FMinSize)) and
      ((FMaxSize <= 0) or (N <= FMaxSize)) then
      FItems.Add(IntToStr(N));
  end;
  ItemsChanged;
end;

function TFontSizeList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TFontSizeList.GetItemText(AIndex: Integer): string;
begin
  Result := FItems[AIndex];
end;

function TFontSizeList.ItemHeight: Integer;
begin
  Result := ScaleValue(Metrics.Edit);
end;

procedure TFontSizeList.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
begin
  APainter.Text(ARect.Right - ScaleValue(18),
    (ARect.Top + ARect.Bottom) div 2, GetItemText(AIndex), OBD_TEXT_BODY,
    Palette.ForegroundText, twRegular, taRightJustify,
    System.Math.Max(1, ARect.Width - ScaleValue(20)));
end;

procedure TFontSizeList.SelectSize(ASize: Integer);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(IntToStr(ASize));
  if (Index < 0) and (FItems.Count > 0) then
    Index := 0;
  ItemIndex := Index;
end;

function TFontSizeList.SelectedSize: Integer;
begin
  if (ItemIndex >= 0) and (ItemIndex < FItems.Count) then
    Result := StrToIntDef(FItems[ItemIndex], 10)
  else
    Result := 10;
end;

{ TFontColorRow }

constructor TFontColorRow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelected := 0;
  TabStop := True;
  Width := 340;
  Height := 30;
end;

function TFontColorRow.SwatchRect(AIndex: Integer): TRect;
var
  S, G: Integer;
begin
  S := ScaleValue(22);
  G := ScaleValue(6);
  Result := Rect(AIndex * (S + G), (Height - S) div 2,
    AIndex * (S + G) + S, (Height - S) div 2 + S);
end;

function TFontColorRow.ColorAt(AIndex: Integer): TColor;
var
  P: TOBDThemePalette;
begin
  P := Palette;
  case AIndex of
    1: Result := P.Accent;
    2: Result := P.Success;
    3: Result := P.Warning;
    4: Result := P.Danger;
    5: Result := P.GaugeNeedle;
    6: Result := P.GaugeLabel;
  else
    Result := P.ForegroundText;
  end;
end;

function TFontColorRow.NameAt(AIndex: Integer): string;
begin
  case AIndex of
    1: Result := 'Accent';
    2: Result := 'Success';
    3: Result := 'Warning';
    4: Result := 'Danger';
    5: Result := 'Needle';
    6: Result := 'Label';
  else
    Result := 'Text (theme)';
  end;
end;

procedure TFontColorRow.SetSelected(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 6);
  if FSelected = AValue then
    Exit;
  FSelected := AValue;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TFontColorRow.SelectedColor: TColor;
begin
  Result := ColorAt(FSelected);
end;

procedure TFontColorRow.SelectColor(AColor: TColor);
var
  I: Integer;
begin
  for I := 0 to 6 do
    if SameColor(AColor, ColorAt(I)) then
    begin
      FSelected := I;
      Invalidate;
      Exit;
    end;
  FSelected := 0;
  Invalidate;
end;

procedure TFontColorRow.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
  I: Integer;
  R: TRect;
  C: TColor;
begin
  Painter := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    for I := 0 to 6 do
    begin
      R := SwatchRect(I);
      C := ColorAt(I);
      Painter.FillRect(R, C);
      Painter.FrameRect(R, OBDMixColor(Palette.ForegroundText, C, 0.25));
      if I = FSelected then
        Painter.FrameRect(Rect(R.Left - ScaleValue(3), R.Top - ScaleValue(3),
          R.Right + ScaleValue(3), R.Bottom + ScaleValue(3)),
          Painter.AccentText, ScaleValue(2));
    end;
    R := SwatchRect(6);
    Painter.Text(R.Right + ScaleValue(10), Height div 2, NameAt(FSelected),
      OBD_TEXT_BODY, Palette.ForegroundText, twRegular, taLeftJustify,
      System.Math.Max(1, Width - R.Right - ScaleValue(12)));
    if Focused then
      Painter.FocusRing(0, 0, Width, Height, 0);
  finally
    Painter.Free;
  end;
end;

procedure TFontColorRow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if CanFocus and not Focused then
    SetFocus;
  if Button <> mbLeft then
    Exit;
  for I := 0 to 6 do
    if PtInRect(SwatchRect(I), Point(X, Y)) then
    begin
      Selected := I;
      Break;
    end;
end;

procedure TFontColorRow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT:
      Selected := FSelected - 1;
    VK_RIGHT:
      Selected := FSelected + 1;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TFontColorRow.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

{ TFontPreview }

constructor TFontPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSampleFont := TFont.Create;
  FSampleText := SAMPLE_TEXT;
  TabStop := False;
end;

destructor TFontPreview.Destroy;
begin
  FSampleFont.Free;
  inherited;
end;

procedure TFontPreview.SetSampleText(const AValue: string);
begin
  if FSampleText = AValue then
    Exit;
  FSampleText := AValue;
  Invalidate;
end;

procedure TFontPreview.SetSampleFont(AValue: TFont);
begin
  FSampleFont.Assign(AValue);
  Invalidate;
end;

procedure TFontPreview.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
  R: TRect;
  Flags: Cardinal;
begin
  Painter := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    Painter.Card(ClientRect, clNone, Palette.Background);
  finally
    Painter.Free;
  end;

  R := ClientRect;
  InflateRect(R, -ScaleValue(12), -ScaleValue(8));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Assign(FSampleFont);
  if ACanvas.Font.Name = '' then
    ACanvas.Font.Name := 'Segoe UI';
  if ACanvas.Font.Size <= 0 then
    ACanvas.Font.Size := 10;
  // The sample deliberately draws user-selected text in the selected font and
  // point size; this is the font dialog's only OBD_TEXT_* exception.
  Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or
    DT_NOPREFIX;
  DrawText(ACanvas.Handle, PChar(FSampleText), -1, R, Flags);
end;

{ TFontDialogController }

constructor TFontDialogController.CreateController(AOwner: TComponent;
  ADialog: TOBDFontDialog);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
end;

procedure TFontDialogController.Build(AWindow: TOBDDialogWindow);
var
  ListH, EditH, RowY: Integer;
  OKButton, CancelButton, ApplyButton: TOBDButton;
  StyleLabel, SizeLabel, EffectsLabel, ColorLabel,
    SampleLabel: TOBDDialogText;
begin
  EditH := DensityMetrics(AWindow.Density).Edit;
  ListH := EditH + 4 + FONT_ROWS * System.Math.Min(EditH, 40) + 4;
  RowY := 34 + ListH + 34;
  AWindow.SetLayout(FONT_BODY_W, RowY + EditH + 34 + 64 + 16);

  AWindow.AddText(16, 16, 220, 'Font');
  StyleLabel := AWindow.AddText(248, 16, 150, 'Style');
  SizeLabel := AWindow.AddText(422, 16, 182, 'Size');

  FSearch := TOBDEdit.Create(AWindow);
  AWindow.Adopt(FSearch);
  FSearch.TextHint := 'Search fonts';
  FSearch.OnChange := SearchChange;
  AWindow.Place(FSearch, 16, 34, 220, EditH);

  FFamily := TFontFamilyList.Create(AWindow);
  AWindow.Adopt(FFamily);
  FFamily.OnChange := FamilyChange;
  FFamily.OnItemActivate := FamilyActivate;
  FFamily.Populate(FDialog.Options);
  AWindow.Place(FFamily, 16, 34 + EditH + 4, 220,
    ListH - EditH - 4);

  FStyleEdit := TOBDEdit.Create(AWindow);
  AWindow.Adopt(FStyleEdit);
  FStyleEdit.OnChange := StyleEditChange;
  AWindow.Place(FStyleEdit, 248, 34, 150, EditH);

  FStyle := TFontStyleList.Create(AWindow);
  AWindow.Adopt(FStyle);
  FStyle.OnChange := StyleChange;
  AWindow.Place(FStyle, 248, 34 + EditH + 4, 150,
    ListH - EditH - 4);

  FSizeEdit := TOBDEdit.Create(AWindow);
  AWindow.Adopt(FSizeEdit);
  FSizeEdit.Alignment := taRightJustify;
  FSizeEdit.NumbersOnly := True;
  FSizeEdit.OnChange := SizeEditChange;
  AWindow.Place(FSizeEdit, 422, 34, 182, EditH);

  FSizeList := TFontSizeList.Create(AWindow);
  AWindow.Adopt(FSizeList);
  FSizeList.OnChange := SizeChange;
  FSizeList.Populate(FDialog.MinFontSize, FDialog.MaxFontSize);
  AWindow.Place(FSizeList, 422, 34 + EditH + 4, 182,
    ListH - EditH - 4);

  EffectsLabel := AWindow.AddText(16, 34 + ListH + 16, 220, 'Effects');
  ColorLabel := AWindow.AddText(248, 34 + ListH + 16, 356, 'Colour');

  FUnderline := TOBDCheckBox.Create(AWindow);
  AWindow.Adopt(FUnderline);
  FUnderline.Caption := 'Underline';
  FUnderline.Enabled := fdoEffects in FDialog.Options;
  FUnderline.OnChange := EffectsChange;
  AWindow.Place(FUnderline, 16, RowY, 116, EditH);

  FStrikeout := TOBDCheckBox.Create(AWindow);
  AWindow.Adopt(FStrikeout);
  FStrikeout.Caption := 'Strikeout';
  FStrikeout.Enabled := fdoEffects in FDialog.Options;
  FStrikeout.OnChange := EffectsChange;
  AWindow.Place(FStrikeout, 138, RowY, 104, EditH);

  FColorRow := TFontColorRow.Create(AWindow);
  AWindow.Adopt(FColorRow);
  FColorRow.Enabled := fdoEffects in FDialog.Options;
  FColorRow.OnChange := EffectsChange;
  AWindow.Place(FColorRow, 248, RowY, 356, EditH);

  SampleLabel := AWindow.AddText(16, RowY + EditH + 16, 588, 'Sample');
  FPreview := TFontPreview.Create(AWindow);
  AWindow.Adopt(FPreview);
  AWindow.Place(FPreview, 16, RowY + EditH + 34, 588, 64);

  // The font list takes any extra width and every list any extra height;
  // the style and size columns and the rows below follow the edges.
  FSearch.Anchors := [akLeft, akTop, akRight];
  FFamily.Anchors := [akLeft, akTop, akRight, akBottom];
  StyleLabel.Anchors := [akTop, akRight];
  FStyleEdit.Anchors := [akTop, akRight];
  FStyle.Anchors := [akTop, akRight, akBottom];
  SizeLabel.Anchors := [akTop, akRight];
  FSizeEdit.Anchors := [akTop, akRight];
  FSizeList.Anchors := [akTop, akRight, akBottom];
  EffectsLabel.Anchors := [akLeft, akBottom];
  FUnderline.Anchors := [akLeft, akBottom];
  FStrikeout.Anchors := [akLeft, akBottom];
  ColorLabel.Anchors := [akRight, akBottom];
  FColorRow.Anchors := [akRight, akBottom];
  SampleLabel.Anchors := [akLeft, akRight, akBottom];
  FPreview.Anchors := [akLeft, akRight, akBottom];

  CancelButton := AWindow.AddButton('Cancel', bkSecondary, mrCancel);
  CancelButton.Cancel := True;
  OKButton := AWindow.AddButton('OK', bkPrimary, mrOk);
  OKButton.Default := True;
  AWindow.CancelResult := mrCancel;
  AWindow.InitialFocus := FSearch;

  ApplyButton := AWindow.AddLeftButton('Apply');
  ApplyButton.Enabled := Assigned(FDialog.OnApply);
  ApplyButton.OnClick := ApplyClick;
end;

procedure TFontDialogController.LoadFromFont(AFont: TFont);
var
  Size: Integer;
begin
  FUpdating := True;
  try
    FSearch.Text := AFont.Name;
    FFamily.SelectFamily(AFont.Name);
    FStyle.SelectStyle(AFont.Style);
    FStyleEdit.Text := STYLE_NAMES[FStyle.ItemIndex];
    Size := ClampFontSize(Abs(AFont.Size), FDialog.MinFontSize,
      FDialog.MaxFontSize);
    FSizeList.SelectSize(Size);
    FSizeEdit.Text := IntToStr(Size);
    FUnderline.Checked := fsUnderline in AFont.Style;
    FStrikeout.Checked := fsStrikeOut in AFont.Style;
    FColorRow.SelectColor(AFont.Color);
  finally
    FUpdating := False;
  end;
  UpdatePreview;
end;

function TFontDialogController.CurrentSize: Integer;
begin
  Result := StrToIntDef(Trim(FSizeEdit.Text), FSizeList.SelectedSize);
  Result := ClampFontSize(Result, FDialog.MinFontSize, FDialog.MaxFontSize);
end;

procedure TFontDialogController.ApplyToFont(AFont: TFont);
var
  Styles: TFontStyles;
begin
  if (FFamily <> nil) and (FFamily.ItemIndex >= 0) then
    AFont.Name := FFamily.GetItemText(FFamily.ItemIndex);
  AFont.Size := CurrentSize;
  Styles := FStyle.SelectedStyle;
  if fdoEffects in FDialog.Options then
  begin
    if FUnderline.Checked then
      Include(Styles, fsUnderline);
    if FStrikeout.Checked then
      Include(Styles, fsStrikeOut);
    AFont.Color := FColorRow.SelectedColor;
  end
  else
  begin
    if fsUnderline in AFont.Style then
      Include(Styles, fsUnderline);
    if fsStrikeOut in AFont.Style then
      Include(Styles, fsStrikeOut);
  end;
  AFont.Style := Styles;
end;

procedure TFontDialogController.UpdatePreview;
var
  PreviewFont: TFont;
begin
  if FPreview = nil then
    Exit;
  PreviewFont := TFont.Create;
  try
    PreviewFont.Assign(FDialog.Font);
    ApplyToFont(PreviewFont);
    FPreview.SampleFont := PreviewFont;
  finally
    PreviewFont.Free;
  end;
end;

procedure TFontDialogController.FamilyChange(Sender: TObject);
begin
  if not FUpdating then
    UpdatePreview;
end;

procedure TFontDialogController.FamilyActivate(Sender: TObject);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(FFamily);
  if Form <> nil then
    Form.ModalResult := mrOk;
end;

procedure TFontDialogController.SearchChange(Sender: TObject);
var
  Name: string;
begin
  if FFamily = nil then
    Exit;
  Name := FSearch.Text;
  FFamily.Filter := Name;
  if FFamily.ItemCount > 0 then
    FFamily.ItemIndex := 0;
  if not FUpdating then
    UpdatePreview;
end;

procedure TFontDialogController.StyleChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    if FStyle.ItemIndex >= 0 then
      FStyleEdit.Text := STYLE_NAMES[FStyle.ItemIndex];
  finally
    FUpdating := False;
  end;
  UpdatePreview;
end;

procedure TFontDialogController.StyleEditChange(Sender: TObject);
var
  Typed: string;
  I: Integer;
begin
  if FUpdating then
    Exit;
  Typed := Trim(FStyleEdit.Text);
  if Typed = '' then
    Exit;
  for I := Low(STYLE_NAMES) to High(STYLE_NAMES) do
    if SameText(Copy(STYLE_NAMES[I], 1, Length(Typed)), Typed) then
    begin
      FUpdating := True;
      try
        FStyle.ItemIndex := I;
      finally
        FUpdating := False;
      end;
      UpdatePreview;
      Exit;
    end;
end;

procedure TFontDialogController.SizeChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    FSizeEdit.Text := IntToStr(FSizeList.SelectedSize);
  finally
    FUpdating := False;
  end;
  UpdatePreview;
end;

procedure TFontDialogController.SizeEditChange(Sender: TObject);
var
  Size: Integer;
begin
  if FUpdating then
    Exit;
  Size := CurrentSize;
  FUpdating := True;
  try
    FSizeList.SelectSize(Size);
  finally
    FUpdating := False;
  end;
  UpdatePreview;
end;

procedure TFontDialogController.EffectsChange(Sender: TObject);
begin
  if not FUpdating then
    UpdatePreview;
end;

procedure TFontDialogController.ApplyClick(Sender: TObject);
begin
  ApplyToFont(FDialog.Font);
  if Assigned(FDialog.OnApply) then
    FDialog.OnApply(FDialog);
end;

{ TOBDFontDialog }

constructor TOBDFontDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FFont.Color := clWindowText;
  FOptions := [fdoEffects];
  Resizable := True;
end;

destructor TOBDFontDialog.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDFontDialog.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDFontDialog.SetMinFontSize(AValue: Integer);
begin
  FMinFontSize := System.Math.Max(0, AValue);
  if (FMaxFontSize > 0) and (FMinFontSize > FMaxFontSize) then
    FMaxFontSize := FMinFontSize;
end;

procedure TOBDFontDialog.SetMaxFontSize(AValue: Integer);
begin
  FMaxFontSize := System.Math.Max(0, AValue);
  if (FMaxFontSize > 0) and (FMinFontSize > FMaxFontSize) then
    FMinFontSize := FMaxFontSize;
end;

function TOBDFontDialog.DefaultTitle: string;
begin
  Result := 'Font';
end;

function TOBDFontDialog.Execute: Boolean;
var
  Window: TOBDDialogWindow;
  Controller: TFontDialogController;
begin
  Window := NewWindow;
  try
    Controller := TFontDialogController.CreateController(Window, Self);
    Controller.Build(Window);
    Controller.LoadFromFont(FFont);
    Result := RunWindow(Window) = mrOk;
    if Result then
      Controller.ApplyToFont(FFont);
  finally
    Window.Free;
  end;
end;

end.
