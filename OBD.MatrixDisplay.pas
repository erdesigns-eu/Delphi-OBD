//------------------------------------------------------------------------------
// UNIT           : OBD.MatrixDisplay.pas
// CONTENTS       : Matrix display component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 23/03/2024
//------------------------------------------------------------------------------
unit OBD.MatrixDisplay;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes,

  OBD.CustomControl, OBD.CustomControl.Common;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Margin from the border (Otherwise the display border is flattened on the side)
  /// </summary>
  MARGIN_FROM_BORDER = 2;

  /// <summary>
  ///   Default cell size
  /// </summary>
  DEFAULT_CELL_SIZE = 2;
  /// <summary>
  ///   Default cell spacing
  /// </summary>
  DEFAULT_CELL_SPACING = 1;

  /// <summary>
  ///   Default rows
  /// </summary>
  DEFAULT_ROWS = 30;
  /// <summary>
  ///   Default columns
  /// </summary>
  DEFAULT_COLS = 125;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00FBF5F7;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00BBAEAC;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00918888;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00776F6F;
  /// <summary>
  ///   Default border width
  /// </summary>
  DEFAULT_BORDER_WIDTH = 3;
  /// <summary>
  ///   Default border width
  /// </summary>
  DEFAULT_BORDER_CORNER = 25;

  /// <summary>
  ///   Default cell on color
  /// </summary>
  DEFAULT_CELL_ON_COLOR = $00DF7000;
  /// <summary>
  ///   Default cell off color
  /// </summary>
  DEFAULT_CELL_OFF_COLOR = $00F9F9F9;

  /// <summary>
  ///   Default animation enabled
  /// </summary>
  DEFAULT_ANIMATION_ENABLED = False;
  /// <summary>
  ///   Default animation duration
  /// </summary>
  DEFAULT_ANIMATION_DURATION = 1000;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Matrix Display cell (Is a simple boolean: On/Off)
  /// </summary>
  TOBDMatrixDisplayCell = Boolean;
  /// <summary>
  ///   Matrix Display row (Array of cells)
  /// </summary>
  TOBDMatrixDisplayRow = TArray<TOBDMatrixDisplayCell>;
  /// <summary>
  ///   Matrix Display cells (Array of rows)
  /// </summary>
  TOBDMatrixDisplayCells = TArray<TOBDMatrixDisplayRow>;
  /// <summary>
  ///   Matrix Display Animation types
  /// </summary>
  TOBDMatrixDisplayAnimationType = (atScrollLeft, atScrollRight, atScrollUp, atScrollDown, atInvert);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Matrix Display background properties
  /// </summary>
  TOBDMatrixDisplayBackground = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Matrix Display border properties
  /// </summary>
  TOBDMatrixDisplayBorder = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FWidth: Integer;
    /// <summary>
    ///   Border corner size
    /// </summary>
    FCorner: Single;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetWidth(Value: Integer);
    /// <summary>
    ///   Set corner size
    /// </summary>
    procedure SetCorner(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Border width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default DEFAULT_BORDER_WIDTH;
    /// <summary>
    ///   Border corner size
    /// </summary>
    property Corner: Single read FCorner write SetCorner;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Matrix Display Animation
  /// </summary>
  TOBDMatrixDisplayAnimation = class(TPersistent)
  private
    /// <summary>
    ///   Is animation enabled
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Duration of the animation
    /// </summary>
    FDuration: Cardinal;
    /// <summary>
    ///   Animation type
    /// </summary>
    FType: TOBDMatrixDisplayAnimationType;

    /// <summary>
    ///   Set enabled
    /// </summary>
    procedure SetEnabled(Value: Boolean);
    /// <summary>
    ///   Set duration
    /// </summary>
    procedure SetDuration(Value: Cardinal);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Is animation enabled
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default DEFAULT_ANIMATION_ENABLED;
    /// <summary>
    ///   Animation duration
    /// </summary>
    property Duration: Cardinal read FDuration write SetDuration default DEFAULT_ANIMATION_DURATION;
    /// <summary>
    ///   Type of animation
    /// </summary>
    property &Type: TOBDMatrixDisplayAnimationType read FType write FType default atScrollLeft;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Circular Gauge Component
  /// </summary>
  TOBDMatrixDisplay = class(TOBDCustomControl)
  private
    /// <summary>
    ///   Background Buffer
    /// </summary>
    FBackgroundBuffer: TBitmap;
    /// <summary>
    ///   Window handle needed for the timer
    /// </summary>
    FWindowHandle: THandle;
    /// <summary>
    ///   Handle of the animation timer
    /// </summary>
    FTimerHandle: THandle;
  private
    /// <summary>
    ///   Cells
    /// </summary>
    FCells: TOBDMatrixDisplayCells;
    /// <summary>
    ///   Cell size
    /// </summary>
    FCellSize: Integer;
    /// <summary>
    ///   Cell spacing (Space between cells)
    /// </summary>
    FCellSpacing: Integer;
    /// <summary>
    ///   Rows
    /// </summary>
    FRows: Integer;
    /// <summary>
    ///   Columns
    /// </summary>
    FCols: Integer;
    /// <summary>
    ///   Display background
    /// </summary>
    FBackground: TOBDMatrixDisplayBackground;
    /// <summary>
    ///   Display border
    /// </summary>
    FBorder: TOBDMatrixDisplayBorder;
    /// <summary>
    ///   Cell on color
    /// </summary>
    FOnColor: TColor;
    /// <summary>
    ///   Cell off color
    /// </summary>
    FOffColor: TColor;
    /// <summary>
    ///   Animation
    /// </summary>
    FAnimation: TOBDMatrixDisplayAnimation;

    /// <summary>
    ///   Set cell size
    /// </summary>
    procedure SetCellSize(Value: Integer);
    /// <summary>
    ///   Set cell spacing
    /// </summary>
    procedure SetCellSpacing(Value: Integer);
    /// <summary>
    ///   Set rows
    /// </summary>
    procedure SetRows(Value: Integer);
    /// <summary>
    ///   Set cols
    /// </summary>
    procedure SetCols(Value: Integer);
    /// <summary>
    ///   Set display background
    /// </summary>
    procedure SetBackground(Value: TOBDMatrixDisplayBackground);
    /// <summary>
    ///   Set display border
    /// </summary>
    procedure SetBorder(Value: TOBDMatrixDisplayBorder);
    /// <summary>
    ///   Set cell on color
    /// </summary>
    procedure SetOnColor(Value: TColor);
    /// <summary>
    ///   Set cell off color
    /// </summary>
    procedure SetOffColor(Value: TColor);
    /// <summary>
    ///   Set animation
    /// </summary>
    procedure SetAnimation(Value: TOBDMatrixDisplayAnimation);

    /// <summary>
    ///   Initialize cells
    /// </summary>
    procedure InitializeCells;
  protected
    /// <summary>
    ///   Invalidate background (Repaint background buffer)
    /// </summary>
    procedure InvalidateBackground; virtual;
    /// <summary>
    ///   Paint matrix cells
    /// </summary>
    procedure PaintMatrix; virtual;
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; override;
    /// <summary>
    ///   On change handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   On animation changed handler
    /// </summary>
    procedure AnimationChanged(Sender: TObject);
    /// <summary>
    ///   Timer proc handler
    /// </summary>
    procedure AnimationTimerProc(var Msg: TMessage);
    /// <summary>
    ///   Get cell at specified row/col
    /// </summary>
    function GetCell(Row, Col: Integer): TOBDMatrixDisplayCell;
    /// <summary>
    ///   Set cell at specified row/col
    /// </summary>
    procedure SetCell(Row, Col: Integer; Value: TOBDMatrixDisplayCell);
  protected
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
    /// <summary>
    ///   Override WndProc method
    /// </summary>
    procedure WndProc(var Message: TMessage); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Clear display (Set all cells to false)
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Invert display (Set all on cells off, and off cells on)
    /// </summary>
    procedure Invert;
    /// <summary>
    ///   Scroll up
    /// </summary>
    procedure ScrollUp(const Loop: Boolean = True; const Step: Integer = 1);
    /// <summary>
    ///   Scroll down
    /// </summary>
    procedure ScrollDown(const Loop: Boolean = True; const Step: Integer = 1);
    /// <summary>
    ///   Scroll left
    /// </summary>
    procedure ScrollLeft(const Loop: Boolean = True; const Step: Integer = 1);
    /// <summary>
    ///   Scroll right
    /// </summary>
    procedure ScrollRight(const Loop: Boolean = True; const Step: Integer = 1);
    /// <summary>
    ///   Load a bitmap mask in the display
    /// </summary>
    procedure LoadMask(const Value: TBitmap; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = false);
    /// <summary>
    ///   Load a bitmap mask centered in the display
    /// </summary>
    procedure LoadMaskCenterd(const Value: TBitmap; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
    /// <summary>
    ///   Load text in the display
    /// </summary>
    procedure LoadText(const Value: string; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = False);
    /// <summary>
    ///   Load text centered in the display
    /// </summary>
    procedure LoadTextCentered(const Value: string; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
  published
    /// <summary>
    ///   Cell size
    /// </summary>
    property CellSize: Integer read FCellSize write SetCellSize default DEFAULT_CELL_SIZE;
    /// <summary>
    ///   Cell spacing (Space between cells)
    /// </summary>
    property CellSpacing: Integer read FCellSpacing write SetCellSpacing default DEFAULT_CELL_SPACING;
    /// <summary>
    ///   Rows
    /// </summary>
    property Rows: Integer read FRows write SetRows default DEFAULT_ROWS;
    /// <summary>
    ///   Columns
    /// </summary>
    property Cols: Integer read FCols write SetCols default DEFAULT_COLS;
    /// <summary>
    ///   Display background
    /// </summary>
    property Background: TOBDMatrixDisplayBackground read FBackground write SetBackground;
    /// <summary>
    ///   Display border
    /// </summary>
    property Border: TOBDMatrixDisplayBorder read FBorder write SetBorder;
    /// <summary>
    ///   Cell on color
    /// </summary>
    property CellOnColor: TColor read FOnColor write SetOnColor default DEFAULT_CELL_ON_COLOR;
    /// <summary>
    ///   Cell off color
    /// </summary>
    property CellOffColor: TColor read FOffColor write SetOffColor default DEFAULT_CELL_OFF_COLOR;
    /// <summary>
    ///   Animation
    /// </summary>
    property Animation: TOBDMatrixDisplayAnimation read FAnimation write SetAnimation;
  published
    /// <summary>
    ///   Inherited Font
    /// </summary>
    property Font;
    /// <summary>
    ///   Inherited Text
    /// </summary>
    property Text;
  end;

implementation

uses
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBackground.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBackground.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMatrixDisplayBackground.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BACKGROUND_FROM;
  FToColor := DEFAULT_BACKGROUND_TO;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDMatrixDisplayBackground) then
  begin
    FFromColor := (Source as TOBDMatrixDisplayBackground).FromColor;
    FToColor := (Source as TOBDMatrixDisplayBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBorder.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBorder.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBorder.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    // Set the new width
    FWidth := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CORNER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBorder.SetCorner(Value: Single);
begin
  if (FCorner <> Value) and (Value >= 0) then
  begin
    // Set the nex corner size
    FCorner := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMatrixDisplayBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FWidth := DEFAULT_BORDER_WIDTH;
  FCorner := DEFAULT_BORDER_CORNER;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDMatrixDisplayBorder) then
  begin
    FFromColor := (Source as TOBDMatrixDisplayBorder).FromColor;
    FToColor := (Source as TOBDMatrixDisplayBorder).ToColor;
    FWidth := (Source as TOBDMatrixDisplayBorder).Width;
    FCorner := (Source as TOBDMatrixDisplayBorder).Corner;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayAnimation.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    // Set enabled
    FEnabled := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DURATION
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayAnimation.SetDuration(Value: Cardinal);
begin
  if (FDuration <> Value) and (Value >= 10) then
  begin
    // Set duration
    FDuration := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMatrixDisplayAnimation.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FEnabled := DEFAULT_ANIMATION_ENABLED;
  FDuration := DEFAULT_ANIMATION_DURATION;
  FType := atScrollLeft;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplayAnimation.Assign(Source: TPersistent);
begin
  if (Source is TOBDMatrixDisplayAnimation) then
  begin
    FEnabled := (Source as TOBDMatrixDisplayAnimation).Enabled;
    FDuration := (Source as TOBDMatrixDisplayAnimation).Duration;
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET CELL SIZE
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCellSize(Value: Integer);
begin
  if (FCellSize <> Value) then
  begin
    // Set new cell size
    FCellSize := Value;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET CELL SPACING
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCellSpacing(Value: Integer);
begin
  if (FCellSpacing <> Value) then
  begin
    // Set new cell spacing
    FCellSpacing := Value;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET ROWS
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetRows(Value: Integer);
begin
  if (FRows <> Value) and (Value >= 0) then
  begin
    // Set new row count
    FRows := Value;
    // Initialize cells
    InitializeCells;
    // Invalidate the buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET COLUMNS
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCols(Value: Integer);
begin
  if (FCols <> Value) and (Value >= 0) then
  begin
    // Set new column count
    FCols := Value;
    // Initialize cells
    InitializeCells;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetBackground(Value: TOBDMatrixDisplayBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetBorder(Value: TOBDMatrixDisplayBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ON COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetOnColor(Value: TColor);
begin
  if (FOnColor <> Value) and (Value <> clNone) then
  begin
    // Set new on color
    FOnColor := Value;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET OFF COLOR
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetOffColor(Value: TColor);
begin
  if (FOffColor <> Value) and (Value <> clNone) then
  begin
    // Set new off color
    FOffColor := Value;
    // Invalidate buffer
    InvalidateBuffer;
  end;
end;

//------------------------------------------------------------------------------
// SET ANIMATION
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetAnimation(Value: TOBDMatrixDisplayAnimation);
begin
  FAnimation.Assign(Value);
end;

//------------------------------------------------------------------------------
// INITIALIZE CELLS
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.InitializeCells;
var
  R, C: Integer;
begin
  // Set the length of the cells
  SetLength(FCells, FRows, FCols);
  // Initialize cells
  for R := 0 to FRows -1 do
  for C := 0 to FCols -1 do
  FCells[R, C] := False;
end;

//------------------------------------------------------------------------------
// INVALIDATE BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.InvalidateBackground;
var
  SS: TCustomStyleServices;
  Graphics: TGPGraphics;
  BackgroundRect, BorderRect, GlareRect: TGPRect;
  Brush, GlareBrush: TGPBrush;
  Pen: TGPPen;
  Path, GlarePath: TGPGraphicsPath;
begin
  // Update the size of the background buffer
  FBackgroundBuffer.SetSize(Width, Height);

  // If VCL styles is available and enabled, then draw the VCL Style background
  // so it matches the active style background like on the Form or a Panel.
  if TStyleManager.IsCustomStyleActive then
  begin
    SS := StyleServices;
    // Draw the styled background
    SS.DrawElement(FBackgroundBuffer.Canvas.Handle, SS.GetElementDetails(twWindowRoot), Rect(0, 0, Width, Height));
  end else
  // Otherwise fill the background with the color.
  with FBackgroundBuffer.Canvas do
  begin
    // Use the component color
    Brush.Color := Self.Color;
    // Use a solid brush
    Brush.Style := bsSolid;
    // Fill the background with the component color
    FillRect(Rect(0, 0, Width, Height));
  end;

  // Initialize GDI+ Graphics object
  Graphics := TGPGraphics.Create(FBackgroundBuffer.Canvas.Handle);
  try
    // Set smoothing mode to high-quality
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    // Set compositing quality to high-quality
    Graphics.SetCompositingQuality(CompositingQualityHighQuality);

    // Draw the backround
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
    begin
      // Get the rectangle for the background
      BackgroundRect := MakeRect(Border.Width, Border.Width, Width - (Border.Width * 2), Height - (Border.Width * 2));
      // Create the background brush
      Brush := TGPLinearGradientBrush.Create(BackgroundRect, SafeColorRefToARGB(Background.FromColor), SafeColorRefToARGB(Background.ToColor), LinearGradientModeVertical);
      // Get the background path
      Path := CreateRoundRectPath(BackgroundRect, Border.Corner);
      try
        // Fill the background
        Graphics.FillPath(Brush, Path);
      finally
        // Free the background brush object
        Brush.Free;
        // Free the background path
        Path.Free;
      end;
    end;

    // Draw the glare
    GlareRect := MakeRect(Border.Width, Border.Width, Width - (Border.Width * 2), Height - (Border.Width * 2));
    GlarePath := CreateGlareRoundRectPath(GlareRect, Border.Corner);
    GlareBrush := TGPLinearGradientBrush.Create(GlareRect, MakeColor(75, 255, 255, 255), MakeColor(30, 255, 255, 255), LinearGradientModeVertical);
    try
      Graphics.FillPath(GlareBrush, GlarePath);
    finally
      GlarePath.Free;
      GlareBrush.Free;
    end;

    // Draw the border
    if(Border.FromColor <> clNone) and (Border.ToColor <> clNone) and (Border.Width > 0) then
    begin
      // Get the rectangle for the border
      BorderRect := MakeRect(MARGIN_FROM_BORDER, MARGIN_FROM_BORDER, Width - (MARGIN_FROM_BORDER * 2), Height - (MARGIN_FROM_BORDER * 2));
      // Create the border brush
      Brush := TGPLinearGradientBrush.Create(BorderRect, SafeColorRefToARGB(Border.FromColor), SafeColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
      // Create the border pen
      Pen := TGPPen.Create(Brush, Border.Width);
      Pen.SetAlignment(PenAlignmentInset);
      // Get the border path
      Path := CreateRoundRectPath(BorderRect, Border.Corner);
      try
        // Draw the border
        Graphics.DrawPath(Pen, Path);
      finally
        // Free the background brush object
        Brush.Free;
        // Free the background pen object
        Pen.Free;
        // Free the border path
        Path.Free;
      end;
    end;

  finally
    Graphics.Free;
  end;
end;

//------------------------------------------------------------------------------
// PAINT MATRIX
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.PaintMatrix;

  procedure PaintCell(CellRect: TRect; Value: Boolean);
  begin
    with Buffer.Canvas do
    begin
      // Set brush color
      if Value then
        Brush.Color := GetAppropriateColor(FOnColor)
      else
        Brush.Color := GetAppropriateColor(FOffColor);
      // Draw cell
      FillRect(CellRect);
    end;
  end;

var
  CenterX, CenterY, StartX, X, Y, R, C: Integer;
begin
  // Get horizontal center
  CenterX := Width div 2;
  // Get vertical center
  CenterY := Height div 2;

  // Calculate the horizontal start position
  X := CenterX - (((Cols * CellSize) + ((Cols - 2) * CellSpacing)) div 2);
  // Calculate the vertical start position
  Y := CenterY - (((Rows * CellSize) + ((Rows - 2) * CellSpacing)) div 2);

  // Remember the start position
  StartX := X;

  // Loop over rows
  for R := 0 to Rows -1 do
  begin
    // Loop over cols
    for C := 0 to Cols -1 do
    begin
      // Make sure we dont paint over the border
      if (X > (Border.Width + (CellSpacing * 2))) and (X < (Width - ((Border.Width * 2)) + (CellSpacing * 2))) and
         (Y > (Border.Width + (CellSpacing * 2))) and (Y < (Height - ((Border.Width * 2)) + (CellSpacing * 2))) then
      // Paint the cell
      PaintCell(TRect.Create(X, Y, X + CellSize, Y + CellSize), FCells[R][C]);
      // Increase the X position
      Inc(X, CellSize + CellSpacing);
    end;
    // Reset the X position
    X := StartX;
    // Increase the Y position
    Inc(Y, CellSize + CellSpacing);
  end;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.PaintBuffer;
begin
  // Call inherited PaintBuffer
  inherited;
  // Copy the background buffer to the main buffer, by buffering the background
  // and only updating the background buffer when the background is changed
  // allows us to just copy the background buffer, which speeds up our PaintBuffer
  // resulting in less CPU consumption and allowing higher framerates.
  BitBlt(Buffer.Canvas.Handle, 0, 0, Width, Height, FBackgroundBuffer.Canvas.Handle, 0,  0, SRCCOPY);
  // Paint matrix cells on the buffer.
  PaintMatrix;
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SettingsChanged(Sender: TObject);
begin
  // Invalidate the background
  InvalidateBackground;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// ON ANIMATION CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(Handle, FTimerHandle);
    // Create new timer
    if Animation.Enabled then FTimerHandle := SetTimer(FWindowHandle, 1, Animation.Duration, nil);
  end;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// ANIMATION TIMER MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.AnimationTimerProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
  begin
    case Animation.&Type of
      atScrollLeft  : ScrollLeft;
      atScrollRight : ScrollRight;
      atScrollUp    : ScrollUp;
      atScrollDown  : ScrollDown;
      atInvert      : Invert;
    end;

    // Trigger a repaint to display the updated needle position
    InvalidateBuffer;
  end else
    // Pass message to default message handler
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

//------------------------------------------------------------------------------
// GET CELL VALUE
//------------------------------------------------------------------------------
function TOBDMatrixDisplay.GetCell(Row: Integer; Col: Integer): Boolean;
begin
  // Initialize result
  Result := False;

  if Row > Rows then Exit;
  if Col > Cols then Exit;

  // If we make it here, the row and column exist so lets return its value.
  Result := FCells[Row][Col];
end;

//------------------------------------------------------------------------------
// SET CELL VALUE
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCell(Row: Integer; Col: Integer; Value: Boolean);
begin
  if Row > Rows then Exit;
  if Col > Cols then Exit;

  // If we make it here, the row and column exist so lets set its value.
  FCells[Row][Col] := Value;
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Resize;
begin
  // Call inherited Resize
  inherited;
  // Invalidate background
  InvalidateBackground;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Loaded;
begin
  // Call inherited Loaded
  inherited;
  // Invalidate the background
  InvalidateBackground;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// WND PROC
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.WndProc(var Message: TMessage);
begin
  // Call inherited WndProc
  inherited;
  // Handle message
  case Message.Msg of
    // Color changed
    CM_COLORCHANGED: InvalidateBackground;
    // Font changed
    CM_FONTCHANGED:
    begin
      // If there is text, reload the mask so it reflects the current font.
      if (Text <> '') then LoadTextCentered(Text);
      // Invalidate the buffer
      InvalidateBuffer;
    end;
    // Text changed
    CM_TEXTCHANGED:
    begin
      // If there is text, reload the mask so it reflects the current font.
      if (Text <> '') then LoadTextCentered(Text);
    end;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMatrixDisplay.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Create background buffer
  FBackgroundBuffer := TBitmap.Create;
  // Set the background buffer pixel format
  FBackgroundBuffer.PixelFormat := pf32bit;
  // Set defaults
  FCellSize := DEFAULT_CELL_SIZE;
  FCellSpacing := DEFAULT_CELL_SPACING;
  // Create background
  FBackground := TOBDMatrixDisplayBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDMatrixDisplayBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create animation
  FAnimation := TOBDMatrixDisplayAnimation.Create;
  FAnimation.OnChange := AnimationChanged;
  // Set defaults
  FRows := DEFAULT_ROWS;
  FCols := DEFAULT_COLS;
  FOnColor := DEFAULT_CELL_ON_COLOR;
  FOffColor := DEFAULT_CELL_OFF_COLOR;
  // Set default dimensions
  Width := 409;
  Height := 121;
  // Initialize cells
  InitializeCells;
  //
  if not (csDesigning in ComponentState) then
  begin
    // Allocate window handle for the timer
    FWindowHandle := AllocateHWnd(AnimationTimerProc);
    // Create new timer
    if Animation.Enabled then FTimerHandle := SetTimer(FWindowHandle, 1, 1000 div FramesPerSecond, nil);
  end;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDMatrixDisplay.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Kill the timer
    if (FTimerHandle <> 0) then KillTimer(FWindowHandle, FTimerHandle);
    // Deallocate window handle
    DeallocateHWnd(FWindowHandle);
  end;
  // Free background buffer
  FBackgroundBuffer.Free;
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free animation
  FAnimation.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDMatrixDisplay) then
  begin
    FCellSize := (Source as TOBDMatrixDisplay).CellSize;
    FCellSpacing := (Source as TOBDMatrixDisplay).CellSpacing;
    FOnColor := (Source as TOBDMatrixDisplay).CellOnColor;
    FOffColor := (Source as TOBDMatrixDisplay).CellOffColor;
    FBackground.Assign((Source as TOBDMatrixDisplay).Background);
    FBorder.Assign((Source as TOBDMatrixDisplay).Border);
    FAnimation.Assign((Source as TOBDMatrixDisplay).Animation);
  end;
  // Invalidate background
  InvalidateBackground;
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// CLEAR DISPLAY
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Clear;
var
  R, C: Integer;
begin
  // Clear all cells
  for R := 0 to FRows -1 do for C := 0 to FCols -1 do FCells[R, C] := False;
end;

//------------------------------------------------------------------------------
// INVERT DISPLAY
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Invert;
var
  R, C: Integer;
begin
  // Invert all cells
  for R := 0 to FRows -1 do for C := 0 to FCols -1 do FCells[R, C] := not FCells[R, C];
end;

//------------------------------------------------------------------------------
// SCROLL UP
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollUp(const Loop: Boolean = True; const Step: Integer = 1);
var
  R, C: Integer;
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  if Loop then
  begin
    // Shift the rows up by Step
    for R := 0 to FRows - Step - 1 do FCells[R] := FCells[R + Step];
    // Wrap the rows from the bottom to the top
    for R := FRows - Step to FRows - 1 do FCells[R] := FCells[R - FRows + Step];
  end else
  begin
    // Shift the rows up by Step
    for R := 0 to FRows - Step - 1 do FCells[R] := FCells[R + Step];
    // Fill the new rows with False
    for R := FRows - Step to FRows - 1 do
    for C := 0 to FCols - 1 do
    FCells[R][C] := False;
  end;
end;

//------------------------------------------------------------------------------
// SCROLL DOWN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollDown(const Loop: Boolean = True; const Step: Integer = 1);
var
  R, C: Integer;
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  if Loop then
  begin
    // Shift the rows down by Step
    for R := FRows - 1 downto Step do FCells[R] := FCells[R - Step];
    // Wrap the rows from the top to the bottom
    for R := 0 to Step - 1 do FCells[R] := FCells[R + FRows - Step];
  end else
  begin
    // Shift the rows down by Step
    for R := FRows - 1 downto Step do FCells[R] := FCells[R - Step];
    // Fill the new rows with False
    for R := 0 to Step - 1 do
    for C := 0 to FCols - 1 do
    FCells[R][C] := False;
  end;
end;

//------------------------------------------------------------------------------
// SCROLL LEFT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollLeft(const Loop: Boolean = True; const Step: Integer = 1);
var
  R, C: Integer;
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  if Loop then
  begin
    // Shift the columns left by Step
    for R := 0 to FRows - 1 do
    for C := 0 to FCols - Step - 1 do
    FCells[R][C] := FCells[R][C + Step];
    // Wrap the columns from the right to the left
    for R := 0 to FRows - 1 do
    for C := FCols - Step to FCols - 1 do
    FCells[R][C] := FCells[R][C - FCols + Step];
  end else
  begin
    // Shift the columns left by Step
    for R := 0 to FRows - 1 do
    for C := 0 to FCols - Step - 1 do
    FCells[R][C] := FCells[R][C + Step];
    // Fill the new columns with False
    for R := 0 to FRows - 1 do
    for C := FCols - Step to FCols - 1 do
    FCells[R][C] := False;
  end;
end;

//------------------------------------------------------------------------------
// SCROLL RIGHT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollRight(const Loop: Boolean = True; const Step: Integer = 1);
var
  R, C: Integer;
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  if Loop then
  begin
    // Shift the columns right by Step
    for R := 0 to FRows - 1 do
    for C := FCols - 1 downto Step do
    FCells[R][C] := FCells[R][C - Step];
    // Wrap the columns from the left to the right
    for R := 0 to FRows - 1 do
    for C := 0 to Step - 1 do
    FCells[R][C] := FCells[R][C + FCols - Step];
  end else
  begin
    // Shift the columns right by Step
    for R := 0 to FRows - 1 do
    for C := FCols - 1 downto Step do
    FCells[R][C] := FCells[R][C - Step];
    // Fill the new columns with False
    for R := 0 to FRows - 1 do
    for C := 0 to Step - 1 do
    FCells[R][C] := False;
  end;
end;

//------------------------------------------------------------------------------
// LOAD BITMAP MASK
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadMask(const Value: TBitmap; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = False);

  function IsCellOn(const Row, Col: Integer) : Boolean;
  begin
    if Inversed then
      Result := Value.Canvas.Pixels[Col, Row] <> clBlack
    else
      Result := Value.Canvas.Pixels[Col, Row] = clBlack;
  end;

var
  R, C: Integer;
begin
  // First clear the display
  Clear;
  // Loop over the bitmap pixels
  for R := 0 to Value.Height do
  for C := 0 to Value.Width do
  begin
    // Make sure the pixel is in the range of our rows and cols
    if (Row >= 0) and ((Row + R) < Rows) and (Col >= 0) and ((Col + C) < Cols) then
    // Set the cell on/off depending on the pixel
    FCells[Row + R, Col + C] := IsCellOn(R, C);
  end;
end;

//------------------------------------------------------------------------------
// LOAD BITMAP MASK CENTERED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadMaskCenterd(const Value: TBitmap; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
var
  W, H, R, C: Integer;
begin
  // Get bitmap width
  W := Value.Width;
  // Get bitmap height
  H := Value.Height;
  // Calculate the vertical position (Row)
  if Vertical then R := (Rows div 2) - (H div 2) else R := 0;
  // Calculate the horizontal position (Col)
  if Horizontal then C := (Cols div 2) - (W div 2) else C := 0;
  // Make sure the row starts at zero
  if (R < 0) then R := 0;
  // Make sure the col starts at zero
  if (C < 0) then C := 0;
  // Load the mask
  LoadMask(Value, R, C, Inversed);
  // Invalidate the buffer
  InvalidateBuffer;
end;

//------------------------------------------------------------------------------
// LOAD TEXT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadText(const Value: string; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = False);
var
  W, H: Integer;
  B: TBitmap;
begin
  // Create temporary bitmap
  B := TBitmap.Create;
  try
    // Set bitmap to 1 bit - black/white
    B.PixelFormat := pf1bit;
    B.Monochrome  := True;
    // Draw the text on the bitmap
    with B.Canvas do
    begin
      Font.Assign(Self.Font);
      Font.Color := clBlack;
      // Calculate the text width
      W := TextWidth(Value);
      // Calculate the text height
      H := TextHeight(Value);
      // Set the size of the bitmap
      B.SetSize(W, H);
      // Fill the background white
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, W, H));
      // Draw the text
      TextOut(0, 0, Value);
    end;
    // Load the mask
    LoadMask(B, Row, Col, Inversed);
  finally
    B.Free;
  end;
end;

//------------------------------------------------------------------------------
// LOAD TEXT CENTERED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadTextCentered(const Value: string; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
var
  W, H: Integer;
  B: TBitmap;
begin
  // Create temporary bitmap
  B := TBitmap.Create;
  try
    // Set bitmap to 1 bit - black/white
    B.PixelFormat := pf1bit;
    B.Monochrome  := True;
    // Draw the text on the bitmap
    with B.Canvas do
    begin
      Font.Assign(Self.Font);
      Font.Color := clBlack;
      // Calculate the text width
      W := TextWidth(Value);
      // Calculate the text height
      H := TextHeight(Value);
      // Set the size of the bitmap
      B.SetSize(W, H);
      // Fill the background white
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, W, H));
      // Draw the text
      TextOut(0, 0, Value);
    end;
    // Load the mask
    Self.LoadMaskCenterd(B, Horizontal, Vertical, Inversed);
  finally
    B.Free;
  end;
end;

end.
