//------------------------------------------------------------------------------
//  OBD.UI.Indicators
//
//  TOBDLED            small round status indicator. Off / On /
//                     Flashing. Theme-coloured glow.
//  TOBDMatrixDisplay  dot-matrix LCD-style readout. Drives a
//                     scrolling marquee when the text is wider
//                     than the visible columns.
//
//  Both auto-bind a TOBDTheme via inheritance from
//  TOBDGraphicControl / TOBDCustomControl, honour VCL Styles
//  when no Theme is found, and call TBindings.Notify on every
//  state change so a LiveBinding refreshes automatically.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Indicators;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>LED state.</summary>
  TOBDLEDState = (lsOff, lsOn, lsFlashing);

  /// <summary>Small round status LED. Configurable colour,
  /// diameter, and flash period. Use for boolean "live /
  /// stale" / "ready / waiting" markers next to text labels.
  /// </summary>
  TOBDLED = class(TOBDGraphicControl)
  strict private
    FState:         TOBDLEDState;
    FOnColor:       TColor;
    FOffColor:      TColor;
    FDiameter:      Integer;
    FFlashPeriodMs: Cardinal;
    FFlashOn:       Boolean;
    FTimer:         TTimer;
    procedure SetState(AValue: TOBDLEDState);
    procedure SetOnColor(AValue: TColor);
    procedure SetOffColor(AValue: TColor);
    procedure SetDiameter(AValue: Integer);
    procedure SetFlashPeriodMs(AValue: Cardinal);
    procedure HandleTimer(Sender: TObject);
    procedure NotifyBindings;
    function  EffectiveLitColor: TColor;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Off / On / Flashing. Default <c>lsOff</c>.</summary>
    property State: TOBDLEDState read FState write SetState
      default lsOff;
    /// <summary>Colour when lit. Default green (theme success).
    /// </summary>
    property OnColor: TColor read FOnColor write SetOnColor
      default clDefault;
    /// <summary>Colour when dark. Default subtle theme neutral.
    /// </summary>
    property OffColor: TColor read FOffColor write SetOffColor
      default clDefault;
    /// <summary>Diameter in design pixels. Default 16.</summary>
    property Diameter: Integer read FDiameter write SetDiameter
      default 16;
    /// <summary>Flash period (full on+off cycle). Default 700 ms.
    /// </summary>
    property FlashPeriodMs: Cardinal
      read FFlashPeriodMs write SetFlashPeriodMs default 700;
  end;

  /// <summary>Dot-matrix LCD readout. Renders ASCII text as 5×7
  /// dot glyphs. When the text width exceeds the visible
  /// column count the display scrolls horizontally at
  /// <see cref="ScrollSpeedMs"/>.</summary>
  TOBDMatrixDisplay = class(TOBDCustomControl)
  strict private
    FText:          string;
    FDotColor:      TColor;
    FDotOffColor:   TColor;
    FDotSize:       Integer;
    FDotGap:        Integer;
    FScrollSpeedMs: Cardinal;
    FScrollOffset:  Integer;
    FTimer:         TTimer;
    procedure SetText(const AValue: string);
    procedure SetDotColor(AValue: TColor);
    procedure SetDotOffColor(AValue: TColor);
    procedure SetDotSize(AValue: Integer);
    procedure SetDotGap(AValue: Integer);
    procedure SetScrollSpeedMs(AValue: Cardinal);
    procedure HandleTimer(Sender: TObject);
    procedure NotifyBindings;
    function  GlyphColumns(AChar: Char): TArray<Byte>;
    function  TextPixelWidth: Integer;
    function  VisibleColumns: Integer;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Text to display. ASCII subset (32..126). Unknown
    /// codepoints render as a blank glyph.</summary>
    property Text: string read FText write SetText;
    /// <summary>Lit dot colour. Default amber / theme accent.
    /// </summary>
    property DotColor: TColor read FDotColor write SetDotColor
      default clDefault;
    /// <summary>Unlit dot colour. Default subtle theme tint.
    /// </summary>
    property DotOffColor: TColor read FDotOffColor
      write SetDotOffColor default clDefault;
    /// <summary>Dot diameter in design pixels. Default 3.</summary>
    property DotSize: Integer read FDotSize write SetDotSize
      default 3;
    /// <summary>Gap between dots in design pixels. Default 1.
    /// </summary>
    property DotGap: Integer read FDotGap write SetDotGap
      default 1;
    /// <summary>Scroll-step interval. Zero disables scrolling
    /// (text is clipped). Default 90 ms.</summary>
    property ScrollSpeedMs: Cardinal
      read FScrollSpeedMs write SetScrollSpeedMs default 90;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

{ ---- TOBDLED -------------------------------------------------------------- }

constructor TOBDLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 18;
  Height := 18;
  FState         := lsOff;
  FOnColor       := clDefault;
  FOffColor      := clDefault;
  FDiameter      := 16;
  FFlashPeriodMs := 700;
  FFlashOn       := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
end;

destructor TOBDLED.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDLED.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLED.SetState(AValue: TOBDLEDState);
begin
  if FState = AValue then Exit;
  FState := AValue;
  // Timer only runs in flashing mode AND not at design-time.
  FTimer.Enabled := (FState = lsFlashing) and
                    not (csDesigning in ComponentState);
  if FTimer.Enabled then
    FTimer.Interval := System.Math.Max(50, FFlashPeriodMs div 2);
  FFlashOn := True;
  NotifyBindings;
  Repaint;
end;

procedure TOBDLED.SetOnColor(AValue: TColor);
begin
  if FOnColor = AValue then Exit;
  FOnColor := AValue; Repaint;
end;

procedure TOBDLED.SetOffColor(AValue: TColor);
begin
  if FOffColor = AValue then Exit;
  FOffColor := AValue; Repaint;
end;

procedure TOBDLED.SetDiameter(AValue: Integer);
begin
  if AValue < 4 then AValue := 4;
  if AValue > 512 then AValue := 512;
  if FDiameter = AValue then Exit;
  FDiameter := AValue; Repaint;
end;

procedure TOBDLED.SetFlashPeriodMs(AValue: Cardinal);
begin
  if AValue < 100 then AValue := 100;
  if FFlashPeriodMs = AValue then Exit;
  FFlashPeriodMs := AValue;
  if FTimer.Enabled then
    FTimer.Interval := FFlashPeriodMs div 2;
end;

procedure TOBDLED.HandleTimer(Sender: TObject);
begin
  FFlashOn := not FFlashOn;
  Repaint;
end;

function TOBDLED.EffectiveLitColor: TColor;
begin
  if FOnColor <> clDefault then Result := FOnColor
  else                         Result := Palette.Success;
end;

procedure TOBDLED.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  D, X, Y: Integer;
  R: TGPRectF;
  Lit: Boolean;
  Color, BorderCol, OffCol: TColor;
begin
  D := ScaleValue(FDiameter);
  if D > Width  then D := Width;
  if D > Height then D := Height;
  X := (Width  - D) div 2;
  Y := (Height - D) div 2;

  Lit := (FState = lsOn) or
         ((FState = lsFlashing) and FFlashOn);

  if FOffColor <> clDefault then OffCol := FOffColor
  else                           OffCol := Palette.NeutralLight;
  BorderCol := EffectiveBorder;
  if Lit then Color := EffectiveLitColor else Color := OffCol;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    R.X := X;
    R.Y := Y;
    R.Width  := D;
    R.Height := D;

    // Soft "glow" halo when lit. Three concentric translucent
    // rings keep the look modern without needing a real blur.
    if Lit then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(Color, 48));
      try
        var Halo := R;
        Halo.X := Halo.X - ScaleValue(3);
        Halo.Y := Halo.Y - ScaleValue(3);
        Halo.Width  := Halo.Width  + ScaleValue(6);
        Halo.Height := Halo.Height + ScaleValue(6);
        Graphics.FillEllipse(Brush, Halo);
      finally
        Brush.Free;
      end;
    end;

    // Body.
    Brush := TGPSolidBrush.Create(ColorToARGB(Color));
    try
      Graphics.FillEllipse(Brush, R);
    finally
      Brush.Free;
    end;

    // Specular highlight.
    if Lit then
    begin
      Brush := TGPSolidBrush.Create(ColorToARGB(clWhite, 110));
      try
        var Hi := R;
        Hi.X := Hi.X + Hi.Width * 0.18;
        Hi.Y := Hi.Y + Hi.Height * 0.12;
        Hi.Width  := Hi.Width  * 0.40;
        Hi.Height := Hi.Height * 0.28;
        Graphics.FillEllipse(Brush, Hi);
      finally
        Brush.Free;
      end;
    end;

    // Bezel.
    Pen := TGPPen.Create(ColorToARGB(BorderCol), ScaleValue(1));
    try
      Graphics.DrawEllipse(Pen, R);
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDMatrixDisplay 5x7 font ------------------------------------------ }

const
  // Compact 5x7 glyph table for ASCII 32..126. Each entry is
  // five bytes; each byte is a column (bit 0 = top row,
  // bit 6 = bottom row).
  GLYPH_BLANK: array[0..4] of Byte = ($00, $00, $00, $00, $00);

function GlyphFor(C: Char): TArray<Byte>;
begin
  // Hand-rolled subset for the characters a host typically
  // routes through the matrix display: digits, common letters,
  // unit symbols. Anything else falls back to a blank cell.
  SetLength(Result, 5);
  case C of
    ' ': Result := [$00, $00, $00, $00, $00];
    '!': Result := [$00, $00, $5F, $00, $00];
    '"': Result := [$00, $07, $00, $07, $00];
    '#': Result := [$14, $7F, $14, $7F, $14];
    '$': Result := [$24, $2A, $7F, $2A, $12];
    '%': Result := [$23, $13, $08, $64, $62];
    '&': Result := [$36, $49, $55, $22, $50];
    '''':Result := [$00, $05, $03, $00, $00];
    '(': Result := [$00, $1C, $22, $41, $00];
    ')': Result := [$00, $41, $22, $1C, $00];
    '*': Result := [$14, $08, $3E, $08, $14];
    '+': Result := [$08, $08, $3E, $08, $08];
    ',': Result := [$00, $50, $30, $00, $00];
    '-': Result := [$08, $08, $08, $08, $08];
    '.': Result := [$00, $60, $60, $00, $00];
    '/': Result := [$20, $10, $08, $04, $02];
    '0': Result := [$3E, $51, $49, $45, $3E];
    '1': Result := [$00, $42, $7F, $40, $00];
    '2': Result := [$42, $61, $51, $49, $46];
    '3': Result := [$21, $41, $45, $4B, $31];
    '4': Result := [$18, $14, $12, $7F, $10];
    '5': Result := [$27, $45, $45, $45, $39];
    '6': Result := [$3C, $4A, $49, $49, $30];
    '7': Result := [$01, $71, $09, $05, $03];
    '8': Result := [$36, $49, $49, $49, $36];
    '9': Result := [$06, $49, $49, $29, $1E];
    ':': Result := [$00, $36, $36, $00, $00];
    ';': Result := [$00, $56, $36, $00, $00];
    '<': Result := [$00, $08, $14, $22, $41];
    '=': Result := [$14, $14, $14, $14, $14];
    '>': Result := [$41, $22, $14, $08, $00];
    '?': Result := [$02, $01, $51, $09, $06];
    '@': Result := [$32, $49, $79, $41, $3E];
    'A': Result := [$7E, $11, $11, $11, $7E];
    'B': Result := [$7F, $49, $49, $49, $36];
    'C': Result := [$3E, $41, $41, $41, $22];
    'D': Result := [$7F, $41, $41, $22, $1C];
    'E': Result := [$7F, $49, $49, $49, $41];
    'F': Result := [$7F, $09, $09, $01, $01];
    'G': Result := [$3E, $41, $41, $51, $32];
    'H': Result := [$7F, $08, $08, $08, $7F];
    'I': Result := [$00, $41, $7F, $41, $00];
    'J': Result := [$20, $40, $41, $3F, $01];
    'K': Result := [$7F, $08, $14, $22, $41];
    'L': Result := [$7F, $40, $40, $40, $40];
    'M': Result := [$7F, $02, $0C, $02, $7F];
    'N': Result := [$7F, $04, $08, $10, $7F];
    'O': Result := [$3E, $41, $41, $41, $3E];
    'P': Result := [$7F, $09, $09, $09, $06];
    'Q': Result := [$3E, $41, $51, $21, $5E];
    'R': Result := [$7F, $09, $19, $29, $46];
    'S': Result := [$46, $49, $49, $49, $31];
    'T': Result := [$01, $01, $7F, $01, $01];
    'U': Result := [$3F, $40, $40, $40, $3F];
    'V': Result := [$1F, $20, $40, $20, $1F];
    'W': Result := [$7F, $20, $18, $20, $7F];
    'X': Result := [$63, $14, $08, $14, $63];
    'Y': Result := [$03, $04, $78, $04, $03];
    'Z': Result := [$61, $51, $49, $45, $43];
    '[': Result := [$00, $00, $7F, $41, $41];
    '\': Result := [$02, $04, $08, $10, $20];
    ']': Result := [$41, $41, $7F, $00, $00];
    '^': Result := [$04, $02, $01, $02, $04];
    '_': Result := [$40, $40, $40, $40, $40];
    '`': Result := [$00, $01, $02, $04, $00];
    'a': Result := [$20, $54, $54, $54, $78];
    'b': Result := [$7F, $48, $44, $44, $38];
    'c': Result := [$38, $44, $44, $44, $20];
    'd': Result := [$38, $44, $44, $48, $7F];
    'e': Result := [$38, $54, $54, $54, $18];
    'f': Result := [$08, $7E, $09, $01, $02];
    'g': Result := [$08, $54, $54, $54, $3C];
    'h': Result := [$7F, $08, $04, $04, $78];
    'i': Result := [$00, $44, $7D, $40, $00];
    'j': Result := [$20, $40, $44, $3D, $00];
    'k': Result := [$00, $7F, $10, $28, $44];
    'l': Result := [$00, $41, $7F, $40, $00];
    'm': Result := [$7C, $04, $18, $04, $78];
    'n': Result := [$7C, $08, $04, $04, $78];
    'o': Result := [$38, $44, $44, $44, $38];
    'p': Result := [$7C, $14, $14, $14, $08];
    'q': Result := [$08, $14, $14, $18, $7C];
    'r': Result := [$7C, $08, $04, $04, $08];
    's': Result := [$48, $54, $54, $54, $20];
    't': Result := [$04, $3F, $44, $40, $20];
    'u': Result := [$3C, $40, $40, $20, $7C];
    'v': Result := [$1C, $20, $40, $20, $1C];
    'w': Result := [$3C, $40, $30, $40, $3C];
    'x': Result := [$44, $28, $10, $28, $44];
    'y': Result := [$0C, $50, $50, $50, $3C];
    'z': Result := [$44, $64, $54, $4C, $44];
    '{': Result := [$00, $08, $36, $41, $00];
    '|': Result := [$00, $00, $7F, $00, $00];
    '}': Result := [$00, $41, $36, $08, $00];
    '~': Result := [$08, $04, $08, $10, $08];
  else
    Result[0] := GLYPH_BLANK[0];
    Result[1] := GLYPH_BLANK[1];
    Result[2] := GLYPH_BLANK[2];
    Result[3] := GLYPH_BLANK[3];
    Result[4] := GLYPH_BLANK[4];
  end;
end;

{ ---- TOBDMatrixDisplay ---------------------------------------------------- }

constructor TOBDMatrixDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 200;
  Height := 36;
  FText          := '';
  FDotColor      := clDefault;
  FDotOffColor   := clDefault;
  FDotSize       := 3;
  FDotGap        := 1;
  FScrollSpeedMs := 90;
  FScrollOffset  := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
end;

destructor TOBDMatrixDisplay.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDMatrixDisplay.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDMatrixDisplay.SetText(const AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
  FScrollOffset := 0;
  NotifyBindings;
  // Auto-scroll when the text is wider than the visible columns.
  FTimer.Interval := System.Math.Max(20, FScrollSpeedMs);
  FTimer.Enabled  := (FScrollSpeedMs > 0) and
                      not (csDesigning in ComponentState) and
                      (TextPixelWidth > VisibleColumns);
  Repaint;
end;

procedure TOBDMatrixDisplay.SetDotColor(AValue: TColor);
begin
  if FDotColor = AValue then Exit;
  FDotColor := AValue; Repaint;
end;

procedure TOBDMatrixDisplay.SetDotOffColor(AValue: TColor);
begin
  if FDotOffColor = AValue then Exit;
  FDotOffColor := AValue; Repaint;
end;

procedure TOBDMatrixDisplay.SetDotSize(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 32 then AValue := 32;
  if FDotSize = AValue then Exit;
  FDotSize := AValue; Repaint;
end;

procedure TOBDMatrixDisplay.SetDotGap(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if AValue > 16 then AValue := 16;
  if FDotGap = AValue then Exit;
  FDotGap := AValue; Repaint;
end;

procedure TOBDMatrixDisplay.SetScrollSpeedMs(AValue: Cardinal);
begin
  if FScrollSpeedMs = AValue then Exit;
  FScrollSpeedMs := AValue;
  if AValue = 0 then
    FTimer.Enabled := False
  else
  begin
    FTimer.Interval := System.Math.Max(20, AValue);
    FTimer.Enabled  := not (csDesigning in ComponentState) and
                       (TextPixelWidth > VisibleColumns);
  end;
end;

procedure TOBDMatrixDisplay.HandleTimer(Sender: TObject);
var
  Total: Integer;
begin
  Total := TextPixelWidth + VisibleColumns;  //  off-screen tail
  if Total <= 0 then Exit;
  Inc(FScrollOffset);
  if FScrollOffset >= Total then FScrollOffset := 0;
  Repaint;
end;

function TOBDMatrixDisplay.GlyphColumns(AChar: Char): TArray<Byte>;
begin
  Result := GlyphFor(AChar);
end;

function TOBDMatrixDisplay.TextPixelWidth: Integer;
const
  GLYPH_COLS = 5;
  INTER_GLYPH_COLS = 1;
begin
  // Each glyph is 5 columns, plus a 1-column gap. Final glyph
  // has no trailing gap.
  if FText = '' then Exit(0);
  Result := Length(FText) * (GLYPH_COLS + INTER_GLYPH_COLS) -
            INTER_GLYPH_COLS;
end;

function TOBDMatrixDisplay.VisibleColumns: Integer;
var
  Step: Integer;
begin
  Step := ScaleValue(FDotSize + FDotGap);
  if Step <= 0 then Exit(0);
  Result := Width div Step;
end;

procedure TOBDMatrixDisplay.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  DotBrushOn, DotBrushOff: TGPSolidBrush;
  Step: Integer;
  Cols, Rows: Integer;
  C, R, GlyphCol, GlyphIdx, CharIdx, ColInChar: Integer;
  Cell: TGPRectF;
  OnCol, OffCol: TColor;
  Cols7: TArray<Byte>;
  Lit: Boolean;
begin
  Step := ScaleValue(FDotSize + FDotGap);
  if Step <= 0 then Exit;
  Cols := Width  div Step;
  Rows := Height div Step;
  if Rows > 7 then Rows := 7;          // glyph is 7 rows
  if (Cols <= 0) or (Rows <= 0) then Exit;

  if FDotColor    <> clDefault then OnCol  := FDotColor
  else                              OnCol  := Palette.Accent;
  if FDotOffColor <> clDefault then OffCol := FDotOffColor
  else                              OffCol := Palette.NeutralLight;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  DotBrushOn  := TGPSolidBrush.Create(ColorToARGB(OnCol));
  DotBrushOff := TGPSolidBrush.Create(ColorToARGB(OffCol, 96));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for C := 0 to Cols - 1 do
    begin
      // Map screen column → text column considering scroll.
      GlyphCol := C + FScrollOffset;
      CharIdx    := GlyphCol div 6;            // 5 glyph + 1 gap
      ColInChar  := GlyphCol mod 6;
      if (CharIdx >= 0) and
         (CharIdx < Length(FText)) and
         (ColInChar < 5) then
      begin
        Cols7 := GlyphColumns(FText[CharIdx + 1]);
        GlyphIdx := ColInChar;
      end
      else
      begin
        Cols7 := nil;
        GlyphIdx := -1;
      end;

      for R := 0 to Rows - 1 do
      begin
        Lit := (Cols7 <> nil) and (GlyphIdx >= 0) and
               ((Cols7[GlyphIdx] and (1 shl R)) <> 0);
        Cell.X := C * Step;
        Cell.Y := R * Step;
        Cell.Width  := ScaleValue(FDotSize);
        Cell.Height := ScaleValue(FDotSize);
        if Lit then
          Graphics.FillEllipse(DotBrushOn,  Cell)
        else
          Graphics.FillEllipse(DotBrushOff, Cell);
      end;
    end;
  finally
    DotBrushOff.Free;
    DotBrushOn.Free;
    Graphics.Free;
  end;
end;

end.
