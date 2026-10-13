//------------------------------------------------------------------------------
//  ERD.Components.ColorDialog
//
//  Themed colour picker dialog for Application.
//
//    TERDColorDialog    themed TColorDialog replacement with theme,
//                       standard and custom colours, HSV picker, hex and RGB
//                       entry, old/new chips and a screen pipette.
//    TERDColorSwatches  reusable sectioned swatch grid.
//    TERDColorSpectrum  reusable saturation/value square and hue bar.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation of the themed colour dialog.
//------------------------------------------------------------------------------

unit ERD.Components.ColorDialog;

interface

uses
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.Components.Theme,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.Buttons,
  ERD.Components.Edits,
  ERD.Components.SpinEdit,
  ERD.Components.Dialogs;

type
  /// <summary>Event fired when a colour value changes.</summary>
  TERDColorChangeEvent = procedure(Sender: TObject; AColor: TColor) of object;

  /// <summary>Sectioned grid of selectable colour swatches.</summary>
  TERDColorSwatches = class(TERDCustomControl)
  strict private type
    TSection = record
      Caption: string;
      Colors: array of TColor;
    end;
  strict private
    FSections: array of TSection;
    FSelectedColor: TColor;
    FOnChange: TNotifyEvent;
    function SwatchSize: Integer;
    function SwatchGap: Integer;
    function SwatchInset: Integer;
    function Columns: Integer;
    function SectionHeight(const ASection: TSection): Integer;
    function SwatchRect(ASection, AIndex: Integer): TRect;
    function HitTestSwatch(X, Y: Integer; out ASection,
      AIndex: Integer): Boolean;
    procedure SetSelectedColor(AValue: TColor);
  protected
    /// <summary>Paints the section captions, swatches and selection ring.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Selects the clicked swatch.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Moves the selection with the keyboard.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Requests arrow keys.</summary>
    /// <param name="Message">Window message.</param>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    /// <summary>Creates an empty swatch grid.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Removes all sections.</summary>
    procedure Clear;
    /// <summary>Adds a captioned section of colours.</summary>
    /// <param name="ACaption">Section caption.</param>
    /// <param name="AColors">Colours; clNone draws an empty slot.</param>
    procedure AddSection(const ACaption: string; const AColors: array of TColor);
    /// <summary>Selects the next selectable colour.</summary>
    /// <param name="ADelta">Positive or negative step count.</param>
    procedure MoveSelection(ADelta: Integer);
  published
    /// <summary>Currently selected colour.</summary>
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor
      default clBlack;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity;
    /// <summary>Fires when the selected colour changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Saturation/value square plus hue bar for selecting colours.</summary>
  TERDColorSpectrum = class(TERDCustomControl)
  strict private
    FSelectedColor: TColor;
    FHue: Double;
    FSaturation: Double;
    FValue: Double;
    FDragPart: Integer;
    FOnChange: TNotifyEvent;
    function SVRect: TRect;
    function HueRect: TRect;
    procedure SetSelectedColor(AValue: TColor);
    procedure SetHue(AValue: Double);
    procedure SetSaturation(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure SetHSV(AHue, ASaturation, AValue: Double);
    procedure UpdateFromPoint(X, Y: Integer);
  protected
    /// <summary>Paints the HSV spectrum.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Starts dragging the saturation/value square or hue bar.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Updates the colour during a drag.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Changes saturation, value or hue with the keyboard.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Requests arrow keys.</summary>
    /// <param name="Message">Window message.</param>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    /// <summary>Creates the spectrum with black selected.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Selected RGB colour.</summary>
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor
      default clBlack;
    /// <summary>Hue in degrees, 0..360.</summary>
    property Hue: Double read FHue write SetHue;
    /// <summary>Saturation, 0..1.</summary>
    property Saturation: Double read FSaturation write SetSaturation;
    /// <summary>Value, 0..1.</summary>
    property Value: Double read FValue write SetValue;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity;
    /// <summary>Fires when the selected colour changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed replacement for <c>TColorDialog</c>.</summary>
  TERDColorDialog = class(TERDCommonDialog)
  strict private
    FColor: TColor;
    FCustomColors: TStrings;
    procedure SetCustomColors(AValue: TStrings);
  protected
    /// <summary>Title used when <see cref="Title"/> is empty.</summary>
    /// <returns>Default title.</returns>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates the dialog and its custom-colour string list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the custom-colour string list.</summary>
    destructor Destroy; override;
    /// <summary>Shows the themed colour dialog.</summary>
    /// <returns>True when the user accepts the dialog.</returns>
    function Execute: Boolean; override;
  published
    /// <summary>Colour passed in and out of the dialog.</summary>
    property Color: TColor read FColor write FColor default clBlack;
    /// <summary>VCL-format custom colours, ColorA=FFFFFF through ColorP.</summary>
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
  end;

/// <summary>Converts an RGB colour to HSV.</summary>
/// <param name="AColor">Input colour.</param>
/// <param name="AHue">Hue in degrees, 0..360.</param>
/// <param name="ASaturation">Saturation, 0..1.</param>
/// <param name="AValue">Value, 0..1.</param>
procedure ERDRGBToHSV(AColor: TColor; out AHue, ASaturation,
  AValue: Double);

/// <summary>Converts HSV to an RGB colour.</summary>
/// <param name="AHue">Hue in degrees, 0..360.</param>
/// <param name="ASaturation">Saturation, 0..1.</param>
/// <param name="AValue">Value, 0..1.</param>
/// <returns>RGB colour.</returns>
function ERDHSVToRGB(AHue, ASaturation, AValue: Double): TColor;

implementation

const
  COLOR_BODY_W = 620;
  COLOR_BODY_H = 326;
  DRAG_NONE = 0;
  DRAG_SV = 1;
  DRAG_HUE = 2;

  STANDARD_COLORS: array[0..15] of TColor = (
    $000000, $595959, $8C8C8C, $BFBFBF,
    $D9D9D9, $FFFFFF, $A03070, $C07000,
    $0000C0, $0C59E8, $00C0FF, $50D092,
    $50B000, $F0B000, $794E1F, $3C4C7C);

type
  TColorPreview = class(TERDCustomControl)
  strict private
    FOldColor: TColor;
    FNewColor: TColor;
    procedure SetOldColor(AValue: TColor);
    procedure SetNewColor(AValue: TColor);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    property OldColor: TColor read FOldColor write SetOldColor;
    property NewColor: TColor read FNewColor write SetNewColor;
  end;

  TPipetteControl = class(TERDCustomControl)
  strict private
    FActive: Boolean;
    FOnPick: TERDColorChangeEvent;
    FOnFinish: TNotifyEvent;
    procedure SetActive(AValue: Boolean);
    procedure PickAtScreenPoint(const APoint: TPoint; ACommit: Boolean);
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property Active: Boolean read FActive write SetActive;
    property OnPick: TERDColorChangeEvent read FOnPick write FOnPick;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  TColorDialogController = class(TComponent)
  strict private
    FDialog: TERDColorDialog;
    FWindow: TERDDialogWindow;
    FSwatches: TERDColorSwatches;
    FSpectrum: TERDColorSpectrum;
    FHex: TERDEdit;
    FRed: TERDSpinEdit;
    FGreen: TERDSpinEdit;
    FBlue: TERDSpinEdit;
    FPreview: TColorPreview;
    FPipette: TPipetteControl;
    FPickButton: TERDButton;
    FOldColor: TColor;
    FNewColor: TColor;
    FUpdating: Boolean;
    procedure BuildSwatches;
    procedure SetColor(AColor: TColor);
    procedure UpdateControls;
    procedure SwatchesChange(Sender: TObject);
    procedure SpectrumChange(Sender: TObject);
    procedure HexChange(Sender: TObject);
    procedure RGBChange(Sender: TObject);
    procedure AddCustomClick(Sender: TObject);
    procedure PipetteClick(Sender: TObject);
    procedure PipettePick(Sender: TObject; AColor: TColor);
    procedure PipetteFinish(Sender: TObject);
  public
    constructor CreateController(AOwner: TComponent; ADialog: TERDColorDialog);
    procedure Build(AWindow: TERDDialogWindow);
    procedure Accept;
  end;

function RGBColor(ARed, AGreen, ABlue: Integer): TColor;
begin
  Result := RGB(EnsureRange(ARed, 0, 255), EnsureRange(AGreen, 0, 255),
    EnsureRange(ABlue, 0, 255));
end;

function SameColor(A, B: TColor): Boolean;
begin
  Result := ColorToRGB(A) = ColorToRGB(B);
end;

function ColorToHex(AColor: TColor): string;
var
  C: COLORREF;
begin
  C := ColorToRGB(AColor);
  Result := '#' + IntToHex(GetRValue(C), 2) + IntToHex(GetGValue(C), 2) +
    IntToHex(GetBValue(C), 2);
end;

function TryHexToColor(const AText: string; out AColor: TColor): Boolean;
var
  S: string;
  R, G, B: Integer;
begin
  S := Trim(AText);
  if (S <> '') and (S[1] = '#') then
    Delete(S, 1, 1);
  Result := Length(S) = 6;
  if not Result then
    Exit;
  Result := TryStrToInt('$' + Copy(S, 1, 2), R) and
    TryStrToInt('$' + Copy(S, 3, 2), G) and
    TryStrToInt('$' + Copy(S, 5, 2), B);
  if Result then
    AColor := RGBColor(R, G, B);
end;

function CustomName(AIndex: Integer): string;
begin
  Result := 'Color' + Chr(Ord('A') + AIndex);
end;

function CustomHex(AColor: TColor): string;
var
  C: COLORREF;
begin
  C := ColorToRGB(AColor);
  Result := IntToHex(GetRValue(C), 2) + IntToHex(GetGValue(C), 2) +
    IntToHex(GetBValue(C), 2);
end;

function TryCustomColor(const ALine: string; out AColor: TColor): Boolean;
var
  P: Integer;
  S: string;
begin
  P := Pos('=', ALine);
  if P <= 0 then
    S := ''
  else
    S := Trim(Copy(ALine, P + 1, MaxInt));
  Result := TryHexToColor(S, AColor);
end;

procedure EnsureCustomSlots(AStrings: TStrings);
var
  I: Integer;
  Name: string;
begin
  for I := 0 to 15 do
  begin
    Name := CustomName(I);
    if AStrings.IndexOfName(Name) < 0 then
      AStrings.Add(Name + '=');
  end;
end;

procedure ERDRGBToHSV(AColor: TColor; out AHue, ASaturation,
  AValue: Double);
var
  C: COLORREF;
  R, G, B, MaxC, MinC, Delta: Double;
begin
  C := ColorToRGB(AColor);
  R := GetRValue(C) / 255;
  G := GetGValue(C) / 255;
  B := GetBValue(C) / 255;
  MaxC := System.Math.Max(R, System.Math.Max(G, B));
  MinC := System.Math.Min(R, System.Math.Min(G, B));
  Delta := MaxC - MinC;
  AValue := MaxC;
  if MaxC = 0 then
    ASaturation := 0
  else
    ASaturation := Delta / MaxC;
  if Delta = 0 then
    AHue := 0
  else if MaxC = R then
    AHue := 60 * ((G - B) / Delta)
  else if MaxC = G then
    AHue := 60 * (2 + (B - R) / Delta)
  else
    AHue := 60 * (4 + (R - G) / Delta);
  if AHue < 0 then
    AHue := AHue + 360;
end;

function ERDHSVToRGB(AHue, ASaturation, AValue: Double): TColor;
var
  C, X, M, R1, G1, B1: Double;
  H: Double;
  R, G, B: Integer;
begin
  H := AHue;
  while H < 0 do
    H := H + 360;
  while H >= 360 do
    H := H - 360;
  ASaturation := EnsureRange(ASaturation, 0, 1);
  AValue := EnsureRange(AValue, 0, 1);
  C := AValue * ASaturation;
  X := C * (1 - Abs((H / 60) - 2 * Floor(H / 120) - 1));
  M := AValue - C;
  if H < 60 then
  begin
    R1 := C;
    G1 := X;
    B1 := 0;
  end
  else if H < 120 then
  begin
    R1 := X;
    G1 := C;
    B1 := 0;
  end
  else if H < 180 then
  begin
    R1 := 0;
    G1 := C;
    B1 := X;
  end
  else if H < 240 then
  begin
    R1 := 0;
    G1 := X;
    B1 := C;
  end
  else if H < 300 then
  begin
    R1 := X;
    G1 := 0;
    B1 := C;
  end
  else
  begin
    R1 := C;
    G1 := 0;
    B1 := X;
  end;
  R := Round((R1 + M) * 255);
  G := Round((G1 + M) * 255);
  B := Round((B1 + M) * 255);
  Result := RGBColor(R, G, B);
end;

{ TERDColorSwatches }

constructor TERDColorSwatches.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FSelectedColor := clBlack;
  Width := 244;
  Height := 250;
end;

function TERDColorSwatches.SwatchSize: Integer;
begin
  Result := ScaleValue(24);
end;

function TERDColorSwatches.SwatchGap: Integer;
begin
  Result := ScaleValue(6);
end;

function TERDColorSwatches.SwatchInset: Integer;
begin
  Result := ScaleValue(4);
end;

function TERDColorSwatches.Columns: Integer;
begin
  Result := 8;
end;

function TERDColorSwatches.SectionHeight(const ASection: TSection): Integer;
var
  Rows: Integer;
begin
  if Length(ASection.Colors) = 0 then
    Rows := 0
  else
    Rows := (Length(ASection.Colors) + Columns - 1) div Columns;
  Result := ScaleValue(18) + Rows * SwatchSize +
    System.Math.Max(0, Rows - 1) * SwatchGap + ScaleValue(16);
end;

function TERDColorSwatches.SwatchRect(ASection, AIndex: Integer): TRect;
var
  I, TopY, Row, Col: Integer;
begin
  TopY := 0;
  for I := 0 to ASection - 1 do
    Inc(TopY, SectionHeight(FSections[I]));
  Inc(TopY, ScaleValue(18));
  Row := AIndex div Columns;
  Col := AIndex mod Columns;
  // SwatchInset keeps the selection ring (3 px outside) inside the control.
  Result := Rect(SwatchInset + Col * (SwatchSize + SwatchGap),
    TopY + Row * (SwatchSize + SwatchGap),
    SwatchInset + Col * (SwatchSize + SwatchGap) + SwatchSize,
    TopY + Row * (SwatchSize + SwatchGap) + SwatchSize);
end;

function TERDColorSwatches.HitTestSwatch(X, Y: Integer; out ASection,
  AIndex: Integer): Boolean;
var
  S, I: Integer;
begin
  Result := False;
  ASection := -1;
  AIndex := -1;
  for S := 0 to High(FSections) do
    for I := 0 to High(FSections[S].Colors) do
      if (FSections[S].Colors[I] <> clNone) and
        PtInRect(SwatchRect(S, I), Point(X, Y)) then
      begin
        ASection := S;
        AIndex := I;
        Exit(True);
      end;
end;

procedure TERDColorSwatches.Clear;
begin
  SetLength(FSections, 0);
  Invalidate;
end;

procedure TERDColorSwatches.AddSection(const ACaption: string;
  const AColors: array of TColor);
var
  S, I: Integer;
begin
  S := Length(FSections);
  SetLength(FSections, S + 1);
  FSections[S].Caption := ACaption;
  SetLength(FSections[S].Colors, Length(AColors));
  for I := 0 to High(AColors) do
    FSections[S].Colors[I] := AColors[I];
  Invalidate;
end;

procedure TERDColorSwatches.SetSelectedColor(AValue: TColor);
begin
  if SameColor(FSelectedColor, AValue) then
    Exit;
  FSelectedColor := ColorToRGB(AValue);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDColorSwatches.MoveSelection(ADelta: Integer);
var
  Flat: array of TColor;
  S, I, Cur, Next: Integer;
begin
  SetLength(Flat, 0);
  Cur := -1;
  for S := 0 to High(FSections) do
    for I := 0 to High(FSections[S].Colors) do
      if FSections[S].Colors[I] <> clNone then
      begin
        if SameColor(FSections[S].Colors[I], FSelectedColor) then
          Cur := Length(Flat);
        SetLength(Flat, Length(Flat) + 1);
        Flat[High(Flat)] := FSections[S].Colors[I];
      end;
  if Length(Flat) = 0 then
    Exit;
  if Cur < 0 then
    Cur := 0;
  Next := EnsureRange(Cur + ADelta, 0, High(Flat));
  SelectedColor := Flat[Next];
end;

procedure TERDColorSwatches.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  S, I, TopY: Integer;
  R: TRect;
  C: TColor;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    TopY := 0;
    for S := 0 to High(FSections) do
    begin
      Painter.Caps(SwatchInset, TopY + ScaleValue(7), FSections[S].Caption);
      for I := 0 to High(FSections[S].Colors) do
      begin
        R := SwatchRect(S, I);
        C := FSections[S].Colors[I];
        if C = clNone then
        begin
          Painter.FrameRect(R, Palette.NeutralLight);
          if I = 0 then
          begin
            Painter.HLine(R.Left + ScaleValue(7), (R.Top + R.Bottom) div 2,
              R.Width - ScaleValue(14), Palette.Subtle);
            Painter.VLine((R.Left + R.Right) div 2, R.Top + ScaleValue(7),
              R.Height - ScaleValue(14), Palette.Subtle);
          end;
        end
        else
        begin
          Painter.FillRect(R, C);
          Painter.FrameRect(R, ERDMixColor(Palette.ForegroundText, C, 0.25));
          if SameColor(C, FSelectedColor) then
            Painter.FrameRect(Rect(R.Left - ScaleValue(3),
              R.Top - ScaleValue(3), R.Right + ScaleValue(3),
              R.Bottom + ScaleValue(3)), Painter.AccentText, ScaleValue(2));
        end;
      end;
      Inc(TopY, SectionHeight(FSections[S]));
    end;
    if Focused then
      Painter.FocusRing(0, 0, Width, Height, 0);
  finally
    Painter.Free;
  end;
end;

procedure TERDColorSwatches.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  S, I: Integer;
begin
  inherited;
  if CanFocus and not Focused then
    SetFocus;
  if (Button = mbLeft) and HitTestSwatch(X, Y, S, I) then
    SelectedColor := FSections[S].Colors[I];
end;

procedure TERDColorSwatches.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT, VK_UP:
      MoveSelection(-1);
    VK_RIGHT, VK_DOWN:
      MoveSelection(1);
    VK_HOME:
      MoveSelection(-MaxInt);
    VK_END:
      MoveSelection(MaxInt);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TERDColorSwatches.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

{ TERDColorSpectrum }

constructor TERDColorSpectrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  Width := 320;
  Height := 150;
  FSelectedColor := clBlack;
  FHue := 0;
  FSaturation := 0;
  FValue := 0;
end;

function TERDColorSpectrum.SVRect: TRect;
begin
  Result := Rect(0, 0, System.Math.Max(1, Width - ScaleValue(24)), Height);
end;

function TERDColorSpectrum.HueRect: TRect;
begin
  Result := Rect(Width - ScaleValue(14), 0, Width, Height);
end;

procedure TERDColorSpectrum.SetHSV(AHue, ASaturation, AValue: Double);
begin
  AHue := EnsureRange(AHue, 0, 360);
  ASaturation := EnsureRange(ASaturation, 0, 1);
  AValue := EnsureRange(AValue, 0, 1);
  if SameValue(AHue, FHue) and SameValue(ASaturation, FSaturation) and
    SameValue(AValue, FValue) then
    Exit;
  FHue := AHue;
  FSaturation := ASaturation;
  FValue := AValue;
  FSelectedColor := ERDHSVToRGB(FHue, FSaturation, FValue);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDColorSpectrum.SetSelectedColor(AValue: TColor);
begin
  AValue := ColorToRGB(AValue);
  if SameColor(FSelectedColor, AValue) then
    Exit;
  FSelectedColor := AValue;
  ERDRGBToHSV(FSelectedColor, FHue, FSaturation, FValue);
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TERDColorSpectrum.SetHue(AValue: Double);
begin
  SetHSV(AValue, FSaturation, FValue);
end;

procedure TERDColorSpectrum.SetSaturation(AValue: Double);
begin
  SetHSV(FHue, AValue, FValue);
end;

procedure TERDColorSpectrum.SetValue(AValue: Double);
begin
  SetHSV(FHue, FSaturation, AValue);
end;

procedure TERDColorSpectrum.UpdateFromPoint(X, Y: Integer);
var
  R: TRect;
begin
  if FDragPart = DRAG_SV then
  begin
    R := SVRect;
    SetHSV(FHue, EnsureRange((X - R.Left) / System.Math.Max(1, R.Width),
      0, 1), EnsureRange(1 - (Y - R.Top) / System.Math.Max(1, R.Height),
      0, 1));
  end
  else if FDragPart = DRAG_HUE then
  begin
    R := HueRect;
    SetHSV(EnsureRange((Y - R.Top) / System.Math.Max(1, R.Height), 0, 1) *
      360, FSaturation, FValue);
  end;
end;

procedure TERDColorSpectrum.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  R, HR: TRect;
  X, Y, Step: Integer;
  C: TColor;
  MX, MY, HY: Integer;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    R := SVRect;
    Step := System.Math.Max(1, ScaleValue(2));
    Y := R.Top;
    while Y < R.Bottom do
    begin
      X := R.Left;
      while X < R.Right do
      begin
        C := ERDHSVToRGB(FHue, (X - R.Left + Step / 2) /
          System.Math.Max(1, R.Width), 1 - (Y - R.Top + Step / 2) /
          System.Math.Max(1, R.Height));
        Painter.FillRect(Rect(X, Y, System.Math.Min(X + Step, R.Right),
          System.Math.Min(Y + Step, R.Bottom)), C);
        Inc(X, Step);
      end;
      Inc(Y, Step);
    end;
    Painter.FrameRect(R, Palette.NeutralLight);

    MX := R.Left + Round(FSaturation * R.Width);
    MY := R.Top + Round((1 - FValue) * R.Height);
    Painter.Ellipse(MX - ScaleValue(6), MY - ScaleValue(6), ScaleValue(12),
      ScaleValue(12), clNone, clWhite, ScaleValue(2));
    Painter.Ellipse(MX - ScaleValue(7), MY - ScaleValue(7), ScaleValue(14),
      ScaleValue(14), clNone, clBlack, ScaleValue(1));

    HR := HueRect;
    Y := HR.Top;
    while Y < HR.Bottom do
    begin
      C := ERDHSVToRGB((Y - HR.Top + Step / 2) /
        System.Math.Max(1, HR.Height) * 360, 1, 1);
      Painter.FillRect(Rect(HR.Left, Y, HR.Right,
        System.Math.Min(Y + Step, HR.Bottom)), C);
      Inc(Y, Step);
    end;
    Painter.FrameRect(HR, Palette.NeutralLight);
    HY := HR.Top + Round(FHue / 360 * HR.Height);
    Painter.FrameRect(Rect(HR.Left - ScaleValue(3), HY - ScaleValue(2),
      HR.Right + ScaleValue(3), HY + ScaleValue(2)), Palette.ForegroundText);
    if Focused then
      Painter.FocusRing(0, 0, Width, Height, 0);
  finally
    Painter.Free;
  end;
end;

procedure TERDColorSpectrum.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if CanFocus and not Focused then
    SetFocus;
  if Button <> mbLeft then
    Exit;
  if PtInRect(SVRect, Point(X, Y)) then
    FDragPart := DRAG_SV
  else if PtInRect(HueRect, Point(X, Y)) then
    FDragPart := DRAG_HUE
  else
    FDragPart := DRAG_NONE;
  if FDragPart <> DRAG_NONE then
  begin
    MouseCapture := True;
    UpdateFromPoint(X, Y);
  end;
end;

procedure TERDColorSpectrum.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragPart <> DRAG_NONE then
    UpdateFromPoint(X, Y);
end;

procedure TERDColorSpectrum.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragPart <> DRAG_NONE then
  begin
    UpdateFromPoint(X, Y);
    FDragPart := DRAG_NONE;
    MouseCapture := False;
  end;
end;

procedure TERDColorSpectrum.KeyDown(var Key: Word; Shift: TShiftState);
var
  Step: Double;
begin
  inherited;
  if ssShift in Shift then
    Step := 0.05
  else
    Step := 0.01;
  case Key of
    VK_LEFT:
      Saturation := FSaturation - Step;
    VK_RIGHT:
      Saturation := FSaturation + Step;
    VK_UP:
      Value := FValue + Step;
    VK_DOWN:
      Value := FValue - Step;
    VK_PRIOR:
      Hue := FHue - 5;
    VK_NEXT:
      Hue := FHue + 5;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TERDColorSpectrum.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

{ TColorPreview }

constructor TColorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  FOldColor := clBlack;
  FNewColor := clBlack;
end;

procedure TColorPreview.SetOldColor(AValue: TColor);
begin
  FOldColor := ColorToRGB(AValue);
  Invalidate;
end;

procedure TColorPreview.SetNewColor(AValue: TColor);
begin
  FNewColor := ColorToRGB(AValue);
  Invalidate;
end;

procedure TColorPreview.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  R: TRect;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    R := ClientRect;
    Painter.FillRect(Rect(R.Left, R.Top, R.Left + R.Width div 2, R.Bottom),
      FNewColor);
    Painter.FillRect(Rect(R.Left + R.Width div 2, R.Top, R.Right, R.Bottom),
      FOldColor);
    Painter.FrameRect(R, Palette.NeutralLight);
  finally
    Painter.Free;
  end;
end;

{ TPipetteControl }

procedure TPipetteControl.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if FActive then
  begin
    Cursor := crCross;
    SetCapture(Handle);
    if CanFocus then
      SetFocus;
  end
  else
  begin
    if GetCapture = Handle then
      ReleaseCapture;
    Cursor := crDefault;
    if Assigned(FOnFinish) then
      FOnFinish(Self);
  end;
end;

procedure TPipetteControl.PickAtScreenPoint(const APoint: TPoint;
  ACommit: Boolean);
var
  DC: HDC;
  Pixel: COLORREF;
  Picked: TColor;
begin
  DC := GetDC(0);
  try
    Pixel := GetPixel(DC, APoint.X, APoint.Y);
  finally
    ReleaseDC(0, DC);
  end;
  if Pixel = CLR_INVALID then
    Exit;
  Picked := RGBColor(GetRValue(Pixel), GetGValue(Pixel), GetBValue(Pixel));
  if Assigned(FOnPick) then
    FOnPick(Self, Picked);
  if ACommit then
    Active := False;
end;

procedure TPipetteControl.PaintControl(ACanvas: TCanvas);
begin
end;

procedure TPipetteControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FActive then
    PickAtScreenPoint(ClientToScreen(Point(X, Y)), False);
end;

procedure TPipetteControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FActive and (Button = mbLeft) then
    PickAtScreenPoint(ClientToScreen(Point(X, Y)), True);
end;

procedure TPipetteControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FActive and (Key = VK_ESCAPE) then
  begin
    Key := 0;
    Active := False;
  end;
end;

procedure TPipetteControl.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if FActive and (HWND(Message.LParam) <> Handle) then
    Active := False;
end;

{ TColorDialogController }

constructor TColorDialogController.CreateController(AOwner: TComponent;
  ADialog: TERDColorDialog);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
end;

procedure TColorDialogController.BuildSwatches;
var
  P: TERDThemePalette;
  ThemeColors: array[0..7] of TColor;
  Custom: array[0..15] of TColor;
  I, Index: Integer;
  C: TColor;
begin
  P := FWindow.Palette;
  ThemeColors[0] := P.Accent;
  ThemeColors[1] := P.ForegroundText;
  ThemeColors[2] := P.Success;
  ThemeColors[3] := P.Warning;
  ThemeColors[4] := P.Danger;
  ThemeColors[5] := P.Subtle;
  ThemeColors[6] := P.GaugeFace;
  ThemeColors[7] := P.GaugeNeedle;

  for I := 0 to High(Custom) do
    Custom[I] := clNone;
  EnsureCustomSlots(FDialog.CustomColors);
  for I := 0 to 15 do
  begin
    Index := FDialog.CustomColors.IndexOfName(CustomName(I));
    if (Index >= 0) and TryCustomColor(FDialog.CustomColors[Index], C) then
      Custom[I] := C;
  end;

  FSwatches.Clear;
  FSwatches.AddSection('Theme colours', ThemeColors);
  FSwatches.AddSection('Standard colours', STANDARD_COLORS);
  FSwatches.AddSection('Custom colours', Custom);
  FSwatches.SelectedColor := FNewColor;
end;

procedure TColorDialogController.Build(AWindow: TERDDialogWindow);
var
  EditH: Integer;
  OKButton, CancelButton, AddButton: TERDButton;
begin
  FWindow := AWindow;
  FOldColor := ColorToRGB(FDialog.Color);
  FNewColor := FOldColor;
  AWindow.SetLayout(COLOR_BODY_W, COLOR_BODY_H);
  EditH := DensityMetrics(AWindow.Density).Edit;

  FSwatches := TERDColorSwatches.Create(AWindow);
  AWindow.Adopt(FSwatches);
  FSwatches.OnChange := SwatchesChange;
  AWindow.Place(FSwatches, 12, 16, 244, 244);

  FSpectrum := TERDColorSpectrum.Create(AWindow);
  AWindow.Adopt(FSpectrum);
  FSpectrum.OnChange := SpectrumChange;
  AWindow.Place(FSpectrum, 280, 16, 324, 150);

  AWindow.AddText(280, 180, 156, 'Hex');
  FHex := TERDEdit.Create(AWindow);
  AWindow.Adopt(FHex);
  FHex.CharCase := ecUpperCase;
  FHex.MaxLength := 7;
  FHex.Mono := True;
  FHex.OnChange := HexChange;
  AWindow.Place(FHex, 280, 198, 156, EditH);

  AWindow.AddText(448, 180, 156, 'New / current');
  FPreview := TColorPreview.Create(AWindow);
  AWindow.Adopt(FPreview);
  AWindow.Place(FPreview, 448, 198, 156, EditH);

  AWindow.AddText(280, 198 + EditH + 12, 96, 'Red');
  FRed := TERDSpinEdit.Create(AWindow);
  AWindow.Adopt(FRed);
  FRed.MinValue := 0;
  FRed.MaxValue := 255;
  FRed.OnValueChange := RGBChange;
  AWindow.Place(FRed, 280, 198 + EditH + 30, 98, EditH);

  AWindow.AddText(390, 198 + EditH + 12, 96, 'Green');
  FGreen := TERDSpinEdit.Create(AWindow);
  AWindow.Adopt(FGreen);
  FGreen.MinValue := 0;
  FGreen.MaxValue := 255;
  FGreen.OnValueChange := RGBChange;
  AWindow.Place(FGreen, 390, 198 + EditH + 30, 98, EditH);

  AWindow.AddText(500, 198 + EditH + 12, 96, 'Blue');
  FBlue := TERDSpinEdit.Create(AWindow);
  AWindow.Adopt(FBlue);
  FBlue.MinValue := 0;
  FBlue.MaxValue := 255;
  FBlue.OnValueChange := RGBChange;
  AWindow.Place(FBlue, 500, 198 + EditH + 30, 104, EditH);

  AddButton := TERDButton.Create(AWindow);
  AWindow.Adopt(AddButton);
  AddButton.Caption := 'Add to custom colours';
  AddButton.Kind := bkSecondary;
  AddButton.OnClick := AddCustomClick;
  AWindow.Place(AddButton, 280, 198 + 2 * EditH + 64, 196,
    DensityMetrics(AWindow.Density).Button + 8);

  FPipette := TPipetteControl.Create(AWindow);
  AWindow.Adopt(FPipette);
  FPipette.SetBounds(0, 0, 1, 1);
  FPipette.Visible := False;
  FPipette.OnPick := PipettePick;
  FPipette.OnFinish := PipetteFinish;

  CancelButton := AWindow.AddButton('Cancel', bkSecondary, mrCancel);
  CancelButton.Cancel := True;
  OKButton := AWindow.AddButton('OK', bkPrimary, mrOk);
  OKButton.Default := True;
  FPickButton := AWindow.AddLeftButton('Pick from screen', glPipette);
  FPickButton.OnClick := PipetteClick;
  AWindow.CancelResult := mrCancel;
  AWindow.FitBody;
  AWindow.InitialFocus := FHex;

  BuildSwatches;
  UpdateControls;
end;

procedure TColorDialogController.SetColor(AColor: TColor);
begin
  AColor := ColorToRGB(AColor);
  if SameColor(FNewColor, AColor) then
    Exit;
  FNewColor := AColor;
  UpdateControls;
end;

procedure TColorDialogController.UpdateControls;
var
  C: COLORREF;
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    C := ColorToRGB(FNewColor);
    FSwatches.SelectedColor := FNewColor;
    FSpectrum.SelectedColor := FNewColor;
    FHex.Text := ColorToHex(FNewColor);
    FRed.IntValue := GetRValue(C);
    FGreen.IntValue := GetGValue(C);
    FBlue.IntValue := GetBValue(C);
    FPreview.OldColor := FOldColor;
    FPreview.NewColor := FNewColor;
  finally
    FUpdating := False;
  end;
end;

procedure TColorDialogController.SwatchesChange(Sender: TObject);
begin
  if not FUpdating then
    SetColor(FSwatches.SelectedColor);
end;

procedure TColorDialogController.SpectrumChange(Sender: TObject);
begin
  if not FUpdating then
    SetColor(FSpectrum.SelectedColor);
end;

procedure TColorDialogController.HexChange(Sender: TObject);
var
  C: TColor;
begin
  if FUpdating then
    Exit;
  if TryHexToColor(FHex.Text, C) then
    SetColor(C);
end;

procedure TColorDialogController.RGBChange(Sender: TObject);
begin
  if not FUpdating then
    SetColor(RGBColor(FRed.IntValue, FGreen.IntValue, FBlue.IntValue));
end;

procedure TColorDialogController.AddCustomClick(Sender: TObject);
var
  I, Index, Target: Integer;
  C: TColor;
  Line: string;
begin
  EnsureCustomSlots(FDialog.CustomColors);
  Target := 15;
  for I := 0 to 15 do
  begin
    Index := FDialog.CustomColors.IndexOfName(CustomName(I));
    if (Index < 0) or not TryCustomColor(FDialog.CustomColors[Index], C) then
    begin
      Target := I;
      Break;
    end;
  end;
  Line := CustomName(Target) + '=' + CustomHex(FNewColor);
  Index := FDialog.CustomColors.IndexOfName(CustomName(Target));
  if Index >= 0 then
    FDialog.CustomColors[Index] := Line
  else
    FDialog.CustomColors.Add(Line);
  BuildSwatches;
end;

procedure TColorDialogController.PipetteClick(Sender: TObject);
begin
  FPipette.Visible := True;
  FPipette.Active := True;
  FPickButton.Enabled := False;
end;

procedure TColorDialogController.PipettePick(Sender: TObject; AColor: TColor);
begin
  SetColor(AColor);
end;

procedure TColorDialogController.PipetteFinish(Sender: TObject);
begin
  FPipette.Visible := False;
  FPickButton.Enabled := True;
end;

procedure TColorDialogController.Accept;
begin
  FDialog.Color := FNewColor;
end;

{ TERDColorDialog }

constructor TERDColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomColors := TStringList.Create;
  FColor := clBlack;
end;

destructor TERDColorDialog.Destroy;
begin
  FCustomColors.Free;
  inherited;
end;

procedure TERDColorDialog.SetCustomColors(AValue: TStrings);
begin
  FCustomColors.Assign(AValue);
end;

function TERDColorDialog.DefaultTitle: string;
begin
  Result := 'Colour';
end;

function TERDColorDialog.Execute: Boolean;
var
  Window: TERDDialogWindow;
  Controller: TColorDialogController;
begin
  Window := NewWindow;
  try
    Controller := TColorDialogController.CreateController(Window, Self);
    Controller.Build(Window);
    Result := RunWindow(Window) = mrOk;
    if Result then
      Controller.Accept;
  finally
    Window.Free;
  end;
end;

end.
