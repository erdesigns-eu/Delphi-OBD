//------------------------------------------------------------------------------
//  ERD.UI.MatrixDisplay
//
//  TOBDMatrixDisplay - LED dot-matrix board for messages, warnings,
//  icons, images and live values.
//
//  - Text mode: a single text or a list of messages that rotate; text
//    can stand still (aligned) or scroll left / right. Every message
//    starts readable at the left edge and then scrolls.
//  - Value mode: shows a bound channel as "CAPTION VALUE UNIT" with
//    the same channel binding, unit conversion, no-data / stale
//    states and warning / alarm thresholds as the gauges. An alarm
//    makes the board blink red.
//  - Image mode: any TPicture is fitted to the matrix height; dots
//    take the image colour (or the dot colour) where the image is
//    bright enough.
//  - A built-in icon (warning, engine, battery, temperature, oil,
//    info, check) can sit on the left while the text scrolls next to
//    it.
//  - Presets (ticker, warning, alarm, status, value, LCD) set colours,
//    size, scrolling and icon in one go, so the board is useful
//    straight from the palette.
//
//  Glyphs are 5x7 dots (ASCII plus the degree sign), drawn at
//  TextScale 1..4.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.MatrixDisplay;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.UI.Binding,
  ERD.UI.Gauges.Base,
  ERD.Service.LiveData;

type
  /// <summary>What the board shows.</summary>
  TOBDMatrixMode = (
    /// <summary><c>Text</c> or the rotating <c>Lines</c>.</summary>
    mxmText,
    /// <summary>The bound channel as caption, value and unit.</summary>
    mxmValue,
    /// <summary>The <c>Picture</c>.</summary>
    mxmImage);

  /// <summary>Scroll direction.</summary>
  TOBDMatrixScroll = (
    /// <summary>Content stands still and is aligned.</summary>
    mxsNone,
    /// <summary>Content moves to the left (ticker).</summary>
    mxsLeft,
    /// <summary>Content moves to the right.</summary>
    mxsRight);

  /// <summary>Dot shape.</summary>
  TOBDMatrixDotShape = (
    /// <summary>Round LEDs.</summary>
    mxdRound,
    /// <summary>Square pixels (LCD look).</summary>
    mxdSquare);

  /// <summary>Built-in 7x7 icon shown left of the content.</summary>
  TOBDMatrixIcon = (
    /// <summary>No icon.</summary>
    mxiNone,
    /// <summary>Warning triangle.</summary>
    mxiWarning,
    /// <summary>Engine (check engine).</summary>
    mxiEngine,
    /// <summary>Battery.</summary>
    mxiBattery,
    /// <summary>Thermometer.</summary>
    mxiTemperature,
    /// <summary>Oil can.</summary>
    mxiOil,
    /// <summary>Information.</summary>
    mxiInfo,
    /// <summary>Check mark.</summary>
    mxiCheck);

  /// <summary>Ready-made configurations.</summary>
  TOBDMatrixPreset = (
    /// <summary>Properties as set by the developer.</summary>
    mxpCustom,
    /// <summary>Amber scrolling message board.</summary>
    mxpTicker,
    /// <summary>Amber warning with triangle icon.</summary>
    mxpWarning,
    /// <summary>Red blinking alarm with triangle icon.</summary>
    mxpAlarm,
    /// <summary>Green centred status with check icon.</summary>
    mxpStatus,
    /// <summary>Large live value of the bound channel.</summary>
    mxpValue,
    /// <summary>Dark pixels on a green LCD background.</summary>
    mxpLCD);

  /// <summary>LED dot-matrix board.</summary>
  /// <remarks>Animation (scrolling, blinking, rotating messages) only
  /// runs at run time; the designer shows the first frame.</remarks>
  TOBDMatrixDisplay = class(TOBDCustomControl)
  strict private
    FMode: TOBDMatrixMode;
    FPreset: TOBDMatrixPreset;
    FText: string;
    FLines: TStrings;
    FLineIndex: Integer;
    FColumns: Integer;
    FRows: Integer;
    FTextScale: Integer;
    FAlignment: TAlignment;
    FScroll: TOBDMatrixScroll;
    FScrollIntervalMs: Cardinal;
    FScrollX: Integer;
    FMessageHoldMs: Cardinal;
    FMessageStart: UInt64;
    FBlink: Boolean;
    FBlinkIntervalMs: Cardinal;
    FBlinkOff: Boolean;
    FIcon: TOBDMatrixIcon;
    FDotShape: TOBDMatrixDotShape;
    FDotGap: Integer;
    FDotColor: TColor;
    FDotOffColor: TColor;
    FBoardColor: TColor;
    FPicture: TPicture;
    FImageThreshold: Byte;
    FImageColors: Boolean;
    FImageCache: TArray<TColor>;
    FImageCacheW: Integer;
    FImageCacheH: Integer;
    FImageCacheValid: Boolean;
    FChannel: TOBDChannelBinding;
    FAlerts: TOBDGaugeAlerts;
    FCaption: string;
    FUnit: string;
    FDecimals: Byte;
    FTimer: TTimer;
    FOnPassComplete: TNotifyEvent;
    procedure SetMode(AValue: TOBDMatrixMode);
    procedure SetPreset(AValue: TOBDMatrixPreset);
    procedure SetText(const AValue: string);
    procedure SetLines(AValue: TStrings);
    procedure SetColumns(AValue: Integer);
    procedure SetRows(AValue: Integer);
    procedure SetTextScale(AValue: Integer);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetScroll(AValue: TOBDMatrixScroll);
    procedure SetScrollIntervalMs(AValue: Cardinal);
    procedure SetMessageHoldMs(AValue: Cardinal);
    procedure SetBlink(AValue: Boolean);
    procedure SetBlinkIntervalMs(AValue: Cardinal);
    procedure SetIcon(AValue: TOBDMatrixIcon);
    procedure SetDotShape(AValue: TOBDMatrixDotShape);
    procedure SetDotGap(AValue: Integer);
    procedure SetDotColor(AValue: TColor);
    procedure SetDotOffColor(AValue: TColor);
    procedure SetBoardColor(AValue: TColor);
    procedure SetPicture(AValue: TPicture);
    procedure SetImageThreshold(AValue: Byte);
    procedure SetImageColors(AValue: Boolean);
    procedure SetChannel(AValue: TOBDChannelBinding);
    procedure SetAlerts(AValue: TOBDGaugeAlerts);
    procedure SetCaption(const AValue: string);
    procedure SetUnit(const AValue: string);
    procedure SetDecimals(AValue: Byte);
    procedure LinesChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure ChannelChanged(Sender: TObject);
    procedure AlertsChanged(Sender: TObject);
    procedure HandleTimer(Sender: TObject);
    procedure UpdateTimer;
    function NeedsTimer: Boolean;
    procedure ResetMessage;
    procedure NextMessage;
    function ContentLeft: Integer;
    function ContentText: string;
    function ContentColor: TColor;
    function ContentWidth: Integer;
    function IconWidth: Integer;
    function ResolvedOffColor: TColor;
    function ResolvedBoardColor: TColor;
    procedure BuildImageCache;
    procedure PlotGlyphColumn(var AFrame: TArray<TColor>; AX, ATop: Integer;
      ABits: Byte; AColor: TColor);
    procedure PlotText(var AFrame: TArray<TColor>; const AText: string;
      AStartX, AClipLeft: Integer; AColor: TColor);
    procedure PlotImage(var AFrame: TArray<TColor>; AStartX,
      AClipLeft: Integer);
    procedure PlotIcon(var AFrame: TArray<TColor>; AColor: TColor);
    function StartX: Integer;
  protected
    /// <summary>Re-subscribes the channel and starts animation after
    /// streaming.</summary>
    procedure Loaded; override;
    /// <summary>Drops the data source when it is freed.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the board.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates an amber 64x9 board.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops animation and releases owned objects.</summary>
    destructor Destroy; override;
    /// <summary>Applies a preset (also available as the
    /// <see cref="Preset"/> property).</summary>
    /// <param name="APreset">Preset to apply.</param>
    procedure ApplyPreset(APreset: TOBDMatrixPreset);
    /// <summary>Shows a message with an icon and colour matching an
    /// alert level: normal = green check, warning = amber triangle,
    /// alarm = red blinking triangle.</summary>
    /// <param name="AText">Message.</param>
    /// <param name="ALevel">Alert level.</param>
    procedure ShowAlert(const AText: string; ALevel: TOBDAlertLevel);
    /// <summary>Computes the current frame.</summary>
    /// <returns><c>Columns * Rows</c> colours, row by row;
    /// <c>clNone</c> for a dot that is off.</returns>
    function BuildFrame: TArray<TColor>;
    /// <summary>Whether a dot is lit in the current frame.</summary>
    /// <param name="ACol">Column, 0 = left.</param>
    /// <param name="ARow">Row, 0 = top.</param>
    /// <returns>True when lit.</returns>
    function IsDotOn(ACol, ARow: Integer): Boolean;
    /// <summary>Advances the animation by one step (what the timer
    /// does); useful for tests and for hosts driving their own clock.
    /// </summary>
    procedure Step;
    /// <summary>The message currently shown.</summary>
    /// <returns>Text, current line, or the formatted value.</returns>
    function CurrentText: string;
    /// <summary>Data state of the bound channel (value mode).
    /// </summary>
    /// <returns>No data, live or stale.</returns>
    function DataState: TOBDDataState;
    /// <summary>Alert level of the bound channel (value mode).
    /// </summary>
    /// <returns>Normal, warning or alarm.</returns>
    function AlertLevel: TOBDAlertLevel;
    /// <summary>Binds the channel to a data source.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
    /// <summary>Writes the configuration to JSON.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Reads the configuration from JSON.</summary>
    /// <param name="AObject">Source object; nil is ignored.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
    /// <summary>Horizontal scroll position in dots, relative to the
    /// content area.</summary>
    property ScrollX: Integer read FScrollX;
  published
    /// <summary>Applies a ready-made configuration. Individual
    /// properties can still be changed afterwards.</summary>
    property Preset: TOBDMatrixPreset read FPreset write SetPreset
      default mxpCustom;
    /// <summary>What the board shows.</summary>
    property Mode: TOBDMatrixMode read FMode write SetMode default mxmText;
    /// <summary>Message in text mode (when <see cref="Lines"/> is
    /// empty).</summary>
    property Text: string read FText write SetText;
    /// <summary>Messages shown one after another in text mode. With
    /// scrolling, the next message starts after a full pass; without,
    /// after <see cref="MessageHoldMs"/>.</summary>
    property Lines: TStrings read FLines write SetLines;
    /// <summary>Number of dot columns.</summary>
    property Columns: Integer read FColumns write SetColumns default 64;
    /// <summary>Number of dot rows.</summary>
    property Rows: Integer read FRows write SetRows default 9;
    /// <summary>Glyph and icon magnification (1..4).</summary>
    property TextScale: Integer read FTextScale write SetTextScale default 1;
    /// <summary>Alignment of content that does not scroll.</summary>
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    /// <summary>Scroll direction.</summary>
    property Scroll: TOBDMatrixScroll read FScroll write SetScroll
      default mxsNone;
    /// <summary>Milliseconds per one-dot scroll step.</summary>
    property ScrollIntervalMs: Cardinal read FScrollIntervalMs
      write SetScrollIntervalMs default 50;
    /// <summary>Milliseconds a non-scrolling message stays before
    /// the next one in <see cref="Lines"/>.</summary>
    property MessageHoldMs: Cardinal read FMessageHoldMs
      write SetMessageHoldMs default 3000;
    /// <summary>Blink the content.</summary>
    property Blink: Boolean read FBlink write SetBlink default False;
    /// <summary>Blink half-period in milliseconds.</summary>
    property BlinkIntervalMs: Cardinal read FBlinkIntervalMs
      write SetBlinkIntervalMs default 500;
    /// <summary>Icon left of the content.</summary>
    property Icon: TOBDMatrixIcon read FIcon write SetIcon default mxiNone;
    /// <summary>Dot shape.</summary>
    property DotShape: TOBDMatrixDotShape read FDotShape write SetDotShape
      default mxdRound;
    /// <summary>Gap between dots as a percentage of the dot pitch
    /// (0..60).</summary>
    property DotGap: Integer read FDotGap write SetDotGap default 20;
    /// <summary>Colour of a lit dot.</summary>
    property DotColor: TColor read FDotColor write SetDotColor
      default $0000B0FF;
    /// <summary>Colour of an unlit dot; <c>clNone</c> = a dim shade
    /// of <see cref="DotColor"/>.</summary>
    property DotOffColor: TColor read FDotOffColor write SetDotOffColor
      default clNone;
    /// <summary>Board colour; <c>clNone</c> = near black.</summary>
    property BoardColor: TColor read FBoardColor write SetBoardColor
      default clNone;
    /// <summary>Image for image mode.</summary>
    property Picture: TPicture read FPicture write SetPicture;
    /// <summary>Minimum brightness (0..255) for an image pixel to
    /// light its dot.</summary>
    property ImageThreshold: Byte read FImageThreshold
      write SetImageThreshold default 96;
    /// <summary>Lit dots take the image colour instead of
    /// <see cref="DotColor"/>.</summary>
    property ImageColors: Boolean read FImageColors write SetImageColors
      default True;
    /// <summary>Channel shown in value mode.</summary>
    property Channel: TOBDChannelBinding read FChannel write SetChannel;
    /// <summary>Thresholds for the value mode colour and blink.
    /// </summary>
    property Alerts: TOBDGaugeAlerts read FAlerts write SetAlerts;
    /// <summary>Label in front of the value in value mode.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Metric unit of the channel; empty = unit delivered
    /// by the source.</summary>
    property &Unit: string read FUnit write SetUnit;
    /// <summary>Decimals of the value in value mode.</summary>
    property Decimals: Byte read FDecimals write SetDecimals default 0;
    /// <summary>Fires when a scrolling message has fully passed.
    /// </summary>
    property OnPassComplete: TNotifyEvent read FOnPassComplete
      write FOnPassComplete;
  end;

implementation

const
  GlyphRows = 7;
  GlyphCols = 5;

  ColorAmber = TColor($0000B0FF);
  ColorRed = TColor($002838FF);
  ColorGreen = TColor($005ADC28);
  ColorCyan = TColor($00FFC800);
  ColorLcdBoard = TColor($000FBC9B);
  ColorLcdDot = TColor($000F380F);
  ColorLcdOff = TColor($0013AE8D);
  ColorBoard = TColor($00141414);

  // 7x7 icons, one byte per row, bit 6 = leftmost column.
  IconBits: array [TOBDMatrixIcon, 0 .. 6] of Byte = (
    ($00, $00, $00, $00, $00, $00, $00),
    ($08, $14, $14, $2A, $22, $49, $7F),
    ($38, $7D, $7F, $7E, $7F, $7D, $28),
    ($22, $7F, $51, $77, $51, $41, $7F),
    ($08, $0C, $08, $0C, $1C, $3E, $1C),
    ($18, $09, $3E, $7E, $3C, $00, $01),
    ($08, $00, $18, $08, $08, $08, $1C),
    ($00, $01, $02, $44, $28, $10, $00));

/// <summary>5x7 glyph: five column bytes, bit 0 = top row.</summary>
function GlyphFor(C: Char): TArray<Byte>;
begin
  case C of
    '!': Result := [$00, $00, $5F, $00, $00];
    '"': Result := [$00, $07, $00, $07, $00];
    '#': Result := [$14, $7F, $14, $7F, $14];
    '$': Result := [$24, $2A, $7F, $2A, $12];
    '%': Result := [$23, $13, $08, $64, $62];
    '&': Result := [$36, $49, $55, $22, $50];
    '''': Result := [$00, $05, $03, $00, $00];
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
    #$00B0: Result := [$00, $06, $09, $09, $06];
  else
    Result := [$00, $00, $00, $00, $00];
  end;
end;

/// <summary>Mixes two colours.</summary>
function BlendColor(AFrom, ATo: TColor; AAmount: Double): TColor;
var
  A, B: Cardinal;
  R, G, Bl: Integer;
begin
  A := Cardinal(ColorToRGB(AFrom));
  B := Cardinal(ColorToRGB(ATo));
  R := Round((A and $FF) * (1 - AAmount) + (B and $FF) * AAmount);
  G := Round(((A shr 8) and $FF) * (1 - AAmount) +
    ((B shr 8) and $FF) * AAmount);
  Bl := Round(((A shr 16) and $FF) * (1 - AAmount) +
    ((B shr 16) and $FF) * AAmount);
  Result := TColor(R or (G shl 8) or (Bl shl 16));
end;

{ ---- TOBDMatrixDisplay ------------------------------------------------------- }

constructor TOBDMatrixDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := mxmText;
  FPreset := mxpCustom;
  FColumns := 64;
  FRows := 9;
  FTextScale := 1;
  FAlignment := taLeftJustify;
  FScroll := mxsNone;
  FScrollIntervalMs := 50;
  FMessageHoldMs := 3000;
  FBlinkIntervalMs := 500;
  FIcon := mxiNone;
  FDotShape := mxdRound;
  FDotGap := 20;
  FDotColor := $0000B0FF;
  FDotOffColor := clNone;
  FBoardColor := clNone;
  FImageThreshold := 96;
  FImageColors := True;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FChannel := TOBDChannelBinding.Create(Self);
  FChannel.PID := $0C;
  FChannel.OnValue := ChannelChanged;
  FChannel.OnStateChange := ChannelChanged;
  FAlerts := TOBDGaugeAlerts.Create;
  FAlerts.OnChange := AlertsChanged;
  FCaption := 'RPM';
  Width := 384;
  Height := 60;
end;

destructor TOBDMatrixDisplay.Destroy;
begin
  FTimer.Free;
  FAlerts.Free;
  FChannel.Free;
  FPicture.Free;
  FLines.Free;
  inherited;
end;

procedure TOBDMatrixDisplay.Loaded;
begin
  inherited;
  FChannel.Rebind;
  ResetMessage;
  UpdateTimer;
end;

procedure TOBDMatrixDisplay.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FChannel <> nil) then
    FChannel.SourceRemoved(AComponent);
end;

procedure TOBDMatrixDisplay.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    FChannel.Source := TOBDLiveData(ASource)
  else
    FChannel.Source := nil;
end;

{ ---- presets ----------------------------------------------------------------- }

procedure TOBDMatrixDisplay.SetPreset(AValue: TOBDMatrixPreset);
begin
  if csLoading in ComponentState then
  begin
    // The streamed individual properties already describe the
    // result of the preset; only remember which one it was.
    FPreset := AValue;
    Exit;
  end;
  ApplyPreset(AValue);
end;

procedure TOBDMatrixDisplay.ApplyPreset(APreset: TOBDMatrixPreset);
begin
  FPreset := APreset;
  if APreset = mxpCustom then
    Exit;
  FDotOffColor := clNone;
  FBoardColor := clNone;
  FDotShape := mxdRound;
  FBlink := False;
  FTextScale := 1;
  FRows := 9;
  FAlignment := taLeftJustify;
  case APreset of
    mxpTicker:
      begin
        FMode := mxmText;
        FDotColor := ColorAmber;
        FIcon := mxiNone;
        FScroll := mxsLeft;
        FColumns := 96;
      end;
    mxpWarning:
      begin
        FMode := mxmText;
        FDotColor := ColorAmber;
        FIcon := mxiWarning;
        FScroll := mxsLeft;
        FColumns := 64;
      end;
    mxpAlarm:
      begin
        FMode := mxmText;
        FDotColor := ColorRed;
        FIcon := mxiWarning;
        FScroll := mxsLeft;
        FBlink := True;
        FColumns := 64;
      end;
    mxpStatus:
      begin
        FMode := mxmText;
        FDotColor := ColorGreen;
        FIcon := mxiCheck;
        FScroll := mxsNone;
        FAlignment := taCenter;
        FColumns := 64;
      end;
    mxpValue:
      begin
        FMode := mxmValue;
        FDotColor := ColorCyan;
        FIcon := mxiNone;
        FScroll := mxsNone;
        FAlignment := taCenter;
        FTextScale := 2;
        FRows := 16;
        FColumns := 64;
      end;
    mxpLCD:
      begin
        FMode := mxmText;
        FDotColor := ColorLcdDot;
        FDotOffColor := ColorLcdOff;
        FBoardColor := ColorLcdBoard;
        FDotShape := mxdSquare;
        FIcon := mxiNone;
        FScroll := mxsNone;
        FColumns := 64;
      end;
  end;
  FImageCacheValid := False;
  ResetMessage;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDMatrixDisplay.ShowAlert(const AText: string;
  ALevel: TOBDAlertLevel);
begin
  case ALevel of
    alvWarning:
      ApplyPreset(mxpWarning);
    alvAlarm:
      ApplyPreset(mxpAlarm);
  else
    ApplyPreset(mxpStatus);
  end;
  FLines.Clear;
  Text := AText;
end;

{ ---- setters ----------------------------------------------------------------- }

procedure TOBDMatrixDisplay.SetMode(AValue: TOBDMatrixMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
  ResetMessage;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  ResetMessage;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetLines(AValue: TStrings);
begin
  FLines.Assign(AValue);
end;

procedure TOBDMatrixDisplay.LinesChanged(Sender: TObject);
begin
  if FLineIndex >= FLines.Count then
    FLineIndex := 0;
  ResetMessage;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetColumns(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 8, 512);
  if FColumns = AValue then
    Exit;
  FColumns := AValue;
  FImageCacheValid := False;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetRows(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 7, 128);
  if FRows = AValue then
    Exit;
  FRows := AValue;
  FImageCacheValid := False;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetTextScale(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 4);
  if FTextScale = AValue then
    Exit;
  FTextScale := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetScroll(AValue: TOBDMatrixScroll);
begin
  if FScroll = AValue then
    Exit;
  FScroll := AValue;
  ResetMessage;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetScrollIntervalMs(AValue: Cardinal);
begin
  if AValue < 10 then
    AValue := 10;
  if AValue > 2000 then
    AValue := 2000;
  FScrollIntervalMs := AValue;
  UpdateTimer;
end;

procedure TOBDMatrixDisplay.SetMessageHoldMs(AValue: Cardinal);
begin
  if AValue < 250 then
    AValue := 250;
  FMessageHoldMs := AValue;
end;

procedure TOBDMatrixDisplay.SetBlink(AValue: Boolean);
begin
  if FBlink = AValue then
    Exit;
  FBlink := AValue;
  FBlinkOff := False;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetBlinkIntervalMs(AValue: Cardinal);
begin
  if AValue < 100 then
    AValue := 100;
  FBlinkIntervalMs := AValue;
end;

procedure TOBDMatrixDisplay.SetIcon(AValue: TOBDMatrixIcon);
begin
  if FIcon = AValue then
    Exit;
  FIcon := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetDotShape(AValue: TOBDMatrixDotShape);
begin
  if FDotShape = AValue then
    Exit;
  FDotShape := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetDotGap(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 60);
  if FDotGap = AValue then
    Exit;
  FDotGap := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetDotColor(AValue: TColor);
begin
  if FDotColor = AValue then
    Exit;
  FDotColor := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetDotOffColor(AValue: TColor);
begin
  if FDotOffColor = AValue then
    Exit;
  FDotOffColor := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetBoardColor(AValue: TColor);
begin
  if FBoardColor = AValue then
    Exit;
  FBoardColor := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetPicture(AValue: TPicture);
begin
  FPicture.Assign(AValue);
end;

procedure TOBDMatrixDisplay.PictureChanged(Sender: TObject);
begin
  FImageCacheValid := False;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetImageThreshold(AValue: Byte);
begin
  if FImageThreshold = AValue then
    Exit;
  FImageThreshold := AValue;
  FImageCacheValid := False;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetImageColors(AValue: Boolean);
begin
  if FImageColors = AValue then
    Exit;
  FImageColors := AValue;
  FImageCacheValid := False;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetChannel(AValue: TOBDChannelBinding);
begin
  FChannel.Assign(AValue);
end;

procedure TOBDMatrixDisplay.SetAlerts(AValue: TOBDGaugeAlerts);
begin
  FAlerts.Assign(AValue);
end;

procedure TOBDMatrixDisplay.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetUnit(const AValue: string);
begin
  if FUnit = AValue then
    Exit;
  FUnit := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.SetDecimals(AValue: Byte);
begin
  if AValue > 6 then
    AValue := 6;
  if FDecimals = AValue then
    Exit;
  FDecimals := AValue;
  Invalidate;
end;

procedure TOBDMatrixDisplay.ChannelChanged(Sender: TObject);
begin
  if FMode = mxmValue then
    Invalidate;
end;

procedure TOBDMatrixDisplay.AlertsChanged(Sender: TObject);
begin
  if FMode = mxmValue then
    Invalidate;
end;

{ ---- animation --------------------------------------------------------------- }

function TOBDMatrixDisplay.NeedsTimer: Boolean;
begin
  Result := (FScroll <> mxsNone) or FBlink or (FMode = mxmValue) or
    ((FMode = mxmText) and (FLines.Count > 1));
end;

procedure TOBDMatrixDisplay.UpdateTimer;
var
  Want: Boolean;
begin
  Want := NeedsTimer and not(csDesigning in ComponentState) and
    not(csLoading in ComponentState);
  if Want and (FTimer = nil) then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.OnTimer := HandleTimer;
  end;
  if FTimer = nil then
    Exit;
  if FScroll <> mxsNone then
    FTimer.Interval := FScrollIntervalMs
  else
    FTimer.Interval := 100;
  FTimer.Enabled := Want;
end;

procedure TOBDMatrixDisplay.HandleTimer(Sender: TObject);
begin
  Step;
end;

procedure TOBDMatrixDisplay.ResetMessage;
begin
  FScrollX := 0;
  FMessageStart := TThread.GetTickCount64;
end;

procedure TOBDMatrixDisplay.NextMessage;
begin
  if (FMode = mxmText) and (FLines.Count > 1) then
    FLineIndex := (FLineIndex + 1) mod FLines.Count;
end;

procedure TOBDMatrixDisplay.Step;
var
  NowTick: UInt64;
  Area, W: Integer;
  BlinkOff: Boolean;
begin
  NowTick := TThread.GetTickCount64;
  if FScroll <> mxsNone then
  begin
    Area := FColumns - ContentLeft;
    W := ContentWidth;
    if FScroll = mxsLeft then
    begin
      Dec(FScrollX);
      if FScrollX < -W then
      begin
        NextMessage;
        FScrollX := Area;
        if Assigned(FOnPassComplete) then
          FOnPassComplete(Self);
      end;
    end
    else
    begin
      Inc(FScrollX);
      if FScrollX > Area then
      begin
        NextMessage;
        FScrollX := -ContentWidth;
        if Assigned(FOnPassComplete) then
          FOnPassComplete(Self);
      end;
    end;
  end
  else if (FMode = mxmText) and (FLines.Count > 1) and
    (NowTick - FMessageStart >= FMessageHoldMs) then
  begin
    NextMessage;
    FMessageStart := NowTick;
  end;
  BlinkOff := (FBlink or ((FMode = mxmValue) and (AlertLevel = alvAlarm)))
    and ((NowTick div FBlinkIntervalMs) mod 2 = 1);
  FBlinkOff := BlinkOff;
  Invalidate;
end;

{ ---- content ----------------------------------------------------------------- }

function TOBDMatrixDisplay.DataState: TOBDDataState;
begin
  if not FChannel.HasValue or IsNan(FChannel.Value) then
    Result := dstNoData
  else if FChannel.IsStale then
    Result := dstStale
  else
    Result := dstLive;
end;

function TOBDMatrixDisplay.AlertLevel: TOBDAlertLevel;
begin
  if FChannel.HasValue and not IsNan(FChannel.Value) then
    Result := FAlerts.Level(FChannel.Value)
  else
    Result := alvNormal;
end;

function TOBDMatrixDisplay.CurrentText: string;
var
  Conv: TOBDUnitConversion;
  U, V: string;
begin
  case FMode of
    mxmValue:
      begin
        U := FUnit;
        if U = '' then
          U := FChannel.LastUnit;
        Conv := OBDUnitConversion(U, UnitSystem);
        if FChannel.HasValue and not IsNan(FChannel.Value) then
          V := OBDFormatNumber(Conv.ToDisplay(FChannel.Value), FDecimals)
        else if IsPreview then
          V := '3250'
        else
          V := '--';
        Result := V;
        if FCaption <> '' then
          Result := FCaption + ' ' + Result;
        if Conv.DisplayUnit <> '' then
          Result := Result + ' ' + Conv.DisplayUnit;
      end;
    mxmText:
      begin
        if FLines.Count > 0 then
          Result := FLines[EnsureRange(FLineIndex, 0, FLines.Count - 1)]
        else
          Result := FText;
        if (Result = '') and IsPreview then
          Result := 'OBD STUDIO';
      end;
  else
    Result := '';
  end;
end;

function TOBDMatrixDisplay.ContentText: string;
begin
  Result := CurrentText;
end;

function TOBDMatrixDisplay.ContentColor: TColor;
begin
  Result := FDotColor;
  if FMode <> mxmValue then
    Exit;
  case DataState of
    dstNoData, dstStale:
      begin
        if not IsPreview then
          Result := BlendColor(FDotColor, ResolvedBoardColor, 0.55);
      end;
  else
    case AlertLevel of
      alvWarning:
        Result := ColorAmber;
      alvAlarm:
        Result := ColorRed;
    end;
  end;
end;

function TOBDMatrixDisplay.IconWidth: Integer;
begin
  if FIcon = mxiNone then
    Result := 0
  else
    Result := (GlyphRows + 1) * FTextScale;
end;

function TOBDMatrixDisplay.ContentLeft: Integer;
begin
  Result := System.Math.Min(IconWidth, FColumns - 1);
end;

function TOBDMatrixDisplay.ContentWidth: Integer;
var
  N: Integer;
begin
  if FMode = mxmImage then
  begin
    BuildImageCache;
    Exit(FImageCacheW);
  end;
  N := Length(ContentText);
  if N = 0 then
    Exit(0);
  Result := (N * (GlyphCols + 1) - 1) * FTextScale;
end;

function TOBDMatrixDisplay.StartX: Integer;
var
  Area, W: Integer;
begin
  Area := FColumns - ContentLeft;
  W := ContentWidth;
  if FScroll <> mxsNone then
    Exit(ContentLeft + FScrollX);
  if W >= Area then
    Exit(ContentLeft);
  case FAlignment of
    taCenter:
      Result := ContentLeft + (Area - W) div 2;
    taRightJustify:
      Result := ContentLeft + Area - W;
  else
    Result := ContentLeft;
  end;
end;

function TOBDMatrixDisplay.ResolvedOffColor: TColor;
begin
  if FDotOffColor <> clNone then
    Result := FDotOffColor
  else
    Result := BlendColor(ResolvedBoardColor, FDotColor, 0.14);
end;

function TOBDMatrixDisplay.ResolvedBoardColor: TColor;
begin
  if FBoardColor <> clNone then
    Result := FBoardColor
  else
    Result := ColorBoard;
end;

procedure TOBDMatrixDisplay.BuildImageCache;
var
  Bmp: TBitmap;
  W, H, X, Y: Integer;
  C: TColor;
  Rgb24: Cardinal;
  Lum: Integer;
begin
  if FImageCacheValid then
    Exit;
  FImageCacheValid := True;
  FImageCache := nil;
  FImageCacheW := 0;
  FImageCacheH := 0;
  if (FPicture.Graphic = nil) or FPicture.Graphic.Empty or
    (FPicture.Width <= 0) or (FPicture.Height <= 0) then
    Exit;
  H := FRows;
  W := System.Math.Max(1, Round(FPicture.Width * H / FPicture.Height));
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.SetSize(W, H);
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Canvas.FillRect(Rect(0, 0, W, H));
    Bmp.Canvas.StretchDraw(Rect(0, 0, W, H), FPicture.Graphic);
    SetLength(FImageCache, W * H);
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        C := Bmp.Canvas.Pixels[X, Y];
        Rgb24 := Cardinal(ColorToRGB(C));
        Lum := ((Rgb24 and $FF) * 30 + ((Rgb24 shr 8) and $FF) * 59 +
          ((Rgb24 shr 16) and $FF) * 11) div 100;
        if Lum >= FImageThreshold then
        begin
          if FImageColors then
            FImageCache[Y * W + X] := TColor(Rgb24)
          else
            FImageCache[Y * W + X] := FDotColor;
        end
        else
          FImageCache[Y * W + X] := clNone;
      end;
  finally
    Bmp.Free;
  end;
  FImageCacheW := W;
  FImageCacheH := H;
end;

procedure TOBDMatrixDisplay.PlotGlyphColumn(var AFrame: TArray<TColor>;
  AX, ATop: Integer; ABits: Byte; AColor: TColor);
var
  R, S, Y: Integer;
begin
  if (AX < 0) or (AX >= FColumns) then
    Exit;
  for R := 0 to GlyphRows - 1 do
    if (ABits and (1 shl R)) <> 0 then
      for S := 0 to FTextScale - 1 do
      begin
        Y := ATop + R * FTextScale + S;
        if (Y >= 0) and (Y < FRows) then
          AFrame[Y * FColumns + AX] := AColor;
      end;
end;

procedure TOBDMatrixDisplay.PlotText(var AFrame: TArray<TColor>;
  const AText: string; AStartX, AClipLeft: Integer; AColor: TColor);
var
  I, G, S, X, TopRow: Integer;
  Glyph: TArray<Byte>;
begin
  TopRow := (FRows - GlyphRows * FTextScale) div 2;
  X := AStartX;
  for I := 1 to Length(AText) do
  begin
    if X >= FColumns then
      Break;
    if X + (GlyphCols + 1) * FTextScale > AClipLeft then
    begin
      Glyph := GlyphFor(AText[I]);
      for G := 0 to GlyphCols - 1 do
        for S := 0 to FTextScale - 1 do
          if X + G * FTextScale + S >= AClipLeft then
            PlotGlyphColumn(AFrame, X + G * FTextScale + S, TopRow,
              Glyph[G], AColor);
    end;
    Inc(X, (GlyphCols + 1) * FTextScale);
  end;
end;

procedure TOBDMatrixDisplay.PlotImage(var AFrame: TArray<TColor>;
  AStartX, AClipLeft: Integer);
var
  X, Y, FX, TopRow: Integer;
  C: TColor;
begin
  BuildImageCache;
  if FImageCacheW = 0 then
    Exit;
  TopRow := (FRows - FImageCacheH) div 2;
  for X := 0 to FImageCacheW - 1 do
  begin
    FX := AStartX + X;
    if (FX < AClipLeft) or (FX >= FColumns) then
      Continue;
    for Y := 0 to FImageCacheH - 1 do
    begin
      C := FImageCache[Y * FImageCacheW + X];
      if (C <> clNone) and (TopRow + Y >= 0) and (TopRow + Y < FRows) then
        AFrame[(TopRow + Y) * FColumns + FX] := C;
    end;
  end;
end;

procedure TOBDMatrixDisplay.PlotIcon(var AFrame: TArray<TColor>;
  AColor: TColor);
var
  R, C, SX, SY, X, Y, TopRow: Integer;
begin
  if FIcon = mxiNone then
    Exit;
  TopRow := (FRows - GlyphRows * FTextScale) div 2;
  for R := 0 to GlyphRows - 1 do
    for C := 0 to GlyphRows - 1 do
      if (IconBits[FIcon, R] and (1 shl (GlyphRows - 1 - C))) <> 0 then
        for SY := 0 to FTextScale - 1 do
          for SX := 0 to FTextScale - 1 do
          begin
            X := C * FTextScale + SX;
            Y := TopRow + R * FTextScale + SY;
            if (X < FColumns) and (Y >= 0) and (Y < FRows) then
              AFrame[Y * FColumns + X] := AColor;
          end;
end;

function TOBDMatrixDisplay.BuildFrame: TArray<TColor>;
var
  I: Integer;
  DotCol: TColor;
  S: string;
begin
  SetLength(Result, FColumns * FRows);
  for I := 0 to High(Result) do
    Result[I] := clNone;
  if FBlinkOff then
    Exit;
  DotCol := ContentColor;
  PlotIcon(Result, DotCol);
  if FMode = mxmImage then
  begin
    BuildImageCache;
    if FImageCacheW > 0 then
      PlotImage(Result, StartX, ContentLeft)
    else if IsPreview then
    begin
      S := 'IMAGE';
      PlotText(Result, S, ContentLeft, ContentLeft, DotCol);
    end;
  end
  else
    PlotText(Result, ContentText, StartX, ContentLeft, DotCol);
end;

function TOBDMatrixDisplay.IsDotOn(ACol, ARow: Integer): Boolean;
var
  F: TArray<TColor>;
begin
  if (ACol < 0) or (ACol >= FColumns) or (ARow < 0) or (ARow >= FRows) then
    Exit(False);
  F := BuildFrame;
  Result := F[ARow * FColumns + ACol] <> clNone;
end;

{ ---- painting ---------------------------------------------------------------- }

procedure TOBDMatrixDisplay.PaintControl(ACanvas: TCanvas);
var
  Frame: TArray<TColor>;
  Pitch, Gap, Dot, OX, OY, X, Y, L, T: Integer;
  OffColor: TColor;
  C: TColor;
begin
  Pitch := System.Math.Min(Width div FColumns, Height div FRows);
  if Pitch < 1 then
    Pitch := 1;
  OX := (Width - Pitch * FColumns) div 2;
  OY := (Height - Pitch * FRows) div 2;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ResolvedBoardColor;
  ACanvas.FillRect(Rect(0, 0, Width, Height));

  Gap := Pitch * FDotGap div 100;
  Dot := System.Math.Max(1, Pitch - Gap);
  OffColor := ResolvedOffColor;
  Frame := BuildFrame;
  ACanvas.Pen.Style := psClear;
  for Y := 0 to FRows - 1 do
    for X := 0 to FColumns - 1 do
    begin
      C := Frame[Y * FColumns + X];
      if C = clNone then
        C := OffColor;
      L := OX + X * Pitch + (Pitch - Dot) div 2;
      T := OY + Y * Pitch + (Pitch - Dot) div 2;
      ACanvas.Brush.Color := C;
      if (FDotShape = mxdRound) and (Dot >= 3) then
        ACanvas.Ellipse(L, T, L + Dot + 1, T + Dot + 1)
      else
        ACanvas.FillRect(Rect(L, T, L + Dot, T + Dot));
    end;
  ACanvas.Pen.Style := psSolid;
end;

{ ---- settings ---------------------------------------------------------------- }

procedure TOBDMatrixDisplay.SaveSettings(AObject: TJSONObject);
var
  Arr: TJSONArray;
  I: Integer;
begin
  AObject.AddPair('preset', TJSONNumber.Create(Ord(FPreset)));
  AObject.AddPair('mode', TJSONNumber.Create(Ord(FMode)));
  AObject.AddPair('text', FText);
  Arr := TJSONArray.Create;
  AObject.AddPair('lines', Arr);
  for I := 0 to FLines.Count - 1 do
    Arr.Add(FLines[I]);
  AObject.AddPair('columns', TJSONNumber.Create(FColumns));
  AObject.AddPair('rows', TJSONNumber.Create(FRows));
  AObject.AddPair('textScale', TJSONNumber.Create(FTextScale));
  AObject.AddPair('alignment', TJSONNumber.Create(Ord(FAlignment)));
  AObject.AddPair('scroll', TJSONNumber.Create(Ord(FScroll)));
  AObject.AddPair('scrollIntervalMs', TJSONNumber.Create(FScrollIntervalMs));
  OBDJsonWriteBool(AObject, 'blink', FBlink);
  AObject.AddPair('icon', TJSONNumber.Create(Ord(FIcon)));
  AObject.AddPair('dotShape', TJSONNumber.Create(Ord(FDotShape)));
  AObject.AddPair('dotColor', TJSONNumber.Create(Integer(FDotColor)));
  AObject.AddPair('dotOffColor', TJSONNumber.Create(Integer(FDotOffColor)));
  AObject.AddPair('boardColor', TJSONNumber.Create(Integer(FBoardColor)));
  AObject.AddPair('caption', FCaption);
  AObject.AddPair('unit', FUnit);
  AObject.AddPair('decimals', TJSONNumber.Create(FDecimals));
  AObject.AddPair('pid', TJSONNumber.Create(FChannel.PID));
end;

procedure TOBDMatrixDisplay.LoadSettings(AObject: TJSONObject);
var
  N: Integer;
  S: string;
  B: Boolean;
  AV: TJSONValue;
  Arr: TJSONArray;
  I: Integer;
begin
  if AObject = nil then
    Exit;
  N := Ord(FPreset);
  if OBDJsonReadInt(AObject, 'preset', N) then
    ApplyPreset(TOBDMatrixPreset(EnsureRange(N, Ord(Low(TOBDMatrixPreset)),
      Ord(High(TOBDMatrixPreset)))));
  N := Ord(FMode);
  if OBDJsonReadInt(AObject, 'mode', N) then
    Mode := TOBDMatrixMode(EnsureRange(N, Ord(Low(TOBDMatrixMode)),
      Ord(High(TOBDMatrixMode))));
  S := FText;
  if OBDJsonReadStr(AObject, 'text', S) then
    Text := S;
  AV := AObject.Values['lines'];
  if AV is TJSONArray then
  begin
    Arr := TJSONArray(AV);
    FLines.BeginUpdate;
    try
      FLines.Clear;
      for I := 0 to Arr.Count - 1 do
        FLines.Add(Arr.Items[I].Value);
    finally
      FLines.EndUpdate;
    end;
  end;
  N := FColumns;
  if OBDJsonReadInt(AObject, 'columns', N) then
    Columns := N;
  N := FRows;
  if OBDJsonReadInt(AObject, 'rows', N) then
    Rows := N;
  N := FTextScale;
  if OBDJsonReadInt(AObject, 'textScale', N) then
    TextScale := N;
  N := Ord(FAlignment);
  if OBDJsonReadInt(AObject, 'alignment', N) then
    Alignment := TAlignment(EnsureRange(N, Ord(Low(TAlignment)),
      Ord(High(TAlignment))));
  N := Ord(FScroll);
  if OBDJsonReadInt(AObject, 'scroll', N) then
    Scroll := TOBDMatrixScroll(EnsureRange(N, Ord(Low(TOBDMatrixScroll)),
      Ord(High(TOBDMatrixScroll))));
  N := Integer(FScrollIntervalMs);
  if OBDJsonReadInt(AObject, 'scrollIntervalMs', N) then
    ScrollIntervalMs := Cardinal(System.Math.Max(0, N));
  B := FBlink;
  if OBDJsonReadBool(AObject, 'blink', B) then
    Blink := B;
  N := Ord(FIcon);
  if OBDJsonReadInt(AObject, 'icon', N) then
    Icon := TOBDMatrixIcon(EnsureRange(N, Ord(Low(TOBDMatrixIcon)),
      Ord(High(TOBDMatrixIcon))));
  N := Ord(FDotShape);
  if OBDJsonReadInt(AObject, 'dotShape', N) then
    DotShape := TOBDMatrixDotShape(EnsureRange(N,
      Ord(Low(TOBDMatrixDotShape)), Ord(High(TOBDMatrixDotShape))));
  N := Integer(FDotColor);
  if OBDJsonReadInt(AObject, 'dotColor', N) then
    DotColor := TColor(N);
  N := Integer(FDotOffColor);
  if OBDJsonReadInt(AObject, 'dotOffColor', N) then
    DotOffColor := TColor(N);
  N := Integer(FBoardColor);
  if OBDJsonReadInt(AObject, 'boardColor', N) then
    BoardColor := TColor(N);
  S := FCaption;
  if OBDJsonReadStr(AObject, 'caption', S) then
    Caption := S;
  S := FUnit;
  if OBDJsonReadStr(AObject, 'unit', S) then
    &Unit := S;
  N := FDecimals;
  if OBDJsonReadInt(AObject, 'decimals', N) then
    Decimals := Byte(EnsureRange(N, 0, 6));
  N := FChannel.PID;
  if OBDJsonReadInt(AObject, 'pid', N) then
    FChannel.PID := Byte(EnsureRange(N, 0, 255));
end;

end.
