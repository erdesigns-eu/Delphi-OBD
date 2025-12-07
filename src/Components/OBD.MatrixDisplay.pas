//------------------------------------------------------------------------------
// UNIT           : OBD.MatrixDisplay.pas
// CONTENTS       : Matrix display component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 23/03/2024
// UPDATED        : 06/12/2025 - Refactored for direct Skia rendering
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.MatrixDisplay;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Diagnostics, Vcl.Controls, 
  WinApi.Windows, Winapi.Messages, System.Math, Vcl.Graphics, System.Skia, Vcl.Skia,
  System.UITypes,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.AnimationManager;

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
  ///   Matrix Display Component
  /// </summary>
  TOBDMatrixDisplay = class(TOBDCustomControl, IOBDAnimatable)
  private
    /// <summary>
    ///   Cached Skia background image to reuse across paint cycles
    /// </summary>
    FBackgroundImage: ISkImage;
    /// <summary>
    ///   Lightweight lock to coordinate multithreaded cell access
    /// </summary>
    FCellsLock: TObject;
    /// <summary>
    ///   Stopwatch for high-resolution timing
    /// </summary>
    FStopwatch: TStopwatch;
    /// <summary>
    ///   Last animation tick time in milliseconds
    /// </summary>
    FLastAnimationMs: Int64;
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
    /// <summary>
    ///   Executes a routine while holding the cell monitor for thread safety
    /// </summary>
    procedure ExecuteWithCellsLocked(const Action: TProc);
  protected
    /// <summary>
    ///   Invalidate background (Repaint background buffer)
    /// </summary>
    procedure InvalidateBackground; virtual;
    /// <summary>
    ///   Paint matrix cells
    /// </summary>
    procedure PaintMatrix(const ACanvas: ISkCanvas); virtual;
    /// <summary>
    ///   Paint buffer
    /// </summary>
    procedure PaintSkia(Canvas: ISkCanvas); override;
    /// <summary>
    ///   On change handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   On animation changed handler
    /// </summary>
    procedure AnimationChanged(Sender: TObject);
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
    procedure LoadMaskCentered(const Value: TBitmap; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
    /// <summary>
    ///   Load text in the display
    /// </summary>
    procedure LoadText(const Value: string; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = False);
    /// <summary>
    ///   Load text centered in the display
    /// </summary>
    procedure LoadTextCentered(const Value: string; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);

    // IOBDAnimatable interface methods
    /// <summary>
    ///   Called on each animation tick
    /// </summary>
    procedure AnimationTick(ElapsedMs: Int64);
    /// <summary>
    ///   Returns true if the control has active animations
    /// </summary>
    function IsAnimating: Boolean;
    /// <summary>
    ///   Get the desired frames per second for this control
    /// </summary>
    function GetFramesPerSecond: Integer;
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
  // Validate cell size (minimum 1 pixel)
  if Value < 1 then Value := 1;
  
  if (FCellSize <> Value) then
  begin
    // Set new cell size
    FCellSize := Value;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET CELL SPACING
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCellSpacing(Value: Integer);
begin
  // Validate cell spacing (minimum 0 pixels)
  if Value < 0 then Value := 0;
  
  if (FCellSpacing <> Value) then
  begin
    // Set new cell spacing
    FCellSpacing := Value;
    // Invalidate buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ROWS
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetRows(Value: Integer);
begin
  // Validate rows (minimum 1, maximum 1000 for reasonable memory usage)
  if Value < 1 then Value := 1;
  if Value > 1000 then Value := 1000;
  
  if (FRows <> Value) then
  begin
    // Set new row count
    FRows := Value;
    // Initialize cells
    InitializeCells;
    // Invalidate the buffer
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET COLUMNS
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCols(Value: Integer);
begin
  // Validate columns (minimum 1, maximum 1000 for reasonable memory usage)
  if Value < 1 then Value := 1;
  if Value > 1000 then Value := 1000;
  
  if (FCols <> Value) then
  begin
    // Set new column count
    FCols := Value;
    // Initialize cells
    InitializeCells;
    // Invalidate buffer
    Invalidate;
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
    Invalidate;
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
    Invalidate;
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
  // Initialize the cell matrix within a monitor to allow safe concurrent access
  ExecuteWithCellsLocked(
    procedure
    begin
      // Set the length of the cells
      SetLength(FCells, FRows, FCols);
      // Initialize cells
      for R := 0 to FRows - 1 do
        for C := 0 to FCols - 1 do
          FCells[R, C] := False;
    end);
end;

//------------------------------------------------------------------------------
// EXECUTE WITH CELLS LOCKED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ExecuteWithCellsLocked(const Action: TProc);
begin
  // If there is no work to perform, simply exit without acquiring the monitor
  if not Assigned(Action) then
    Exit;

  // Acquire the monitor so parallel timers or worker threads cannot mutate the buffer mid-frame
  TMonitor.Enter(FCellsLock);
  try
    Action;
  finally
    // Always release the monitor to prevent deadlocks
    TMonitor.Exit(FCellsLock);
  end;
end;

//------------------------------------------------------------------------------
// INVALIDATE BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.InvalidateBackground;
var
  BackgroundRect, BorderRect: TRectF;
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Path: ISkPath;
  Paint: ISkPaint;
begin
  // Allocate a Skia surface for fully hardware-accelerated drawing
  Surface := TSkSurface.MakeRaster(Width, Height);
  Canvas := Surface.Canvas;

  // Clear the canvas using the resolved style color so the base matches styled controls
  Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Draw the background gradient if both colors are provided
  if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    // Calculate the background rectangle taking the border thickness into account
    BackgroundRect := TRectF.Create(Border.Width + 0.0, Border.Width, Width - (Border.Width * 2), Height - (Border.Width * 2));
    // Build a Skia path for the rounded rectangle once
    Path := CreateSkRoundRectPath(RectF(BackgroundRect.Left, BackgroundRect.Top, BackgroundRect.Right, BackgroundRect.Bottom), Border.Corner);
    // Configure gradient paint
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(BackgroundRect.Left, BackgroundRect.Top),
      TPointF.Create(BackgroundRect.Left, BackgroundRect.Bottom),
      [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
      nil,
      TSkTileMode.Clamp);
    try
      // Fill the rounded rectangle with the gradient
      Canvas.DrawPath(Path, Paint);
    finally
      Paint := nil;
      Path := nil;
    end;
  end;

  // Draw a subtle glare overlay to mimic the previous glossy effect
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Shader := TSkShader.MakeGradientLinear(
    TPointF.Create(0, Border.Width),
    TPointF.Create(0, (Height - Border.Width) / 2),
    [$4BFFFFFF, $1EFFFFFF],
    nil,
    TSkTileMode.Clamp);
  Path := CreateSkRoundRectPath(RectF(Border.Width + 0.0, Border.Width, Width - (Border.Width * 2), Height - (Border.Width * 2)), Border.Corner);
  try
    Canvas.DrawPath(Path, Paint);
  finally
    Paint := nil;
    Path := nil;
  end;

  // Draw the border using a vertical gradient stroke when configured
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) and (Border.Width > 0) then
  begin
    BorderRect := TRectF.Create(MARGIN_FROM_BORDER, MARGIN_FROM_BORDER, Width - (MARGIN_FROM_BORDER * 2), Height - (MARGIN_FROM_BORDER * 2) + 0.0);
    Path := CreateSkRoundRectPath(RectF(BorderRect.Left, BorderRect.Top, BorderRect.Right, BorderRect.Bottom), Border.Corner);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := Border.Width;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(BorderRect.Left, BorderRect.Top),
      TPointF.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
      nil,
      TSkTileMode.Clamp);
    try
      Canvas.DrawPath(Path, Paint);
    finally
      Paint := nil;
      Path := nil;
    end;
  end;

  // Persist the Skia-rendered background so it can be reused across paint cycles
  FBackgroundImage := Surface.MakeImageSnapshot;
end;

//------------------------------------------------------------------------------
// PAINT MATRIX
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.PaintMatrix(const ACanvas: ISkCanvas);
var
  CenterX, CenterY, StartX, X, Y: Integer;
  PaintOn, PaintOff: ISkPaint;
  CellRect: TRectF;
begin
  // Prepare paint objects upfront to avoid allocations inside the nested loops
  PaintOn := TSkPaint.Create;
  PaintOn.Style := TSkPaintStyle.Fill;
  PaintOn.Color := SafeColorRefToSkColor(FOnColor);
  PaintOn.AntiAlias := False;

  PaintOff := TSkPaint.Create;
  PaintOff.Style := TSkPaintStyle.Fill;
  PaintOff.Color := SafeColorRefToSkColor(FOffColor);
  PaintOff.AntiAlias := False;

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

  // Lock cells so drawing remains thread-safe when animation updates come from timers
  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
      // Loop over rows
      for R := 0 to Rows - 1 do
      begin
        // Loop over cols
        for C := 0 to Cols - 1 do
        begin
          // Make sure we dont paint over the border
          if (X > (Border.Width + (CellSpacing * 2))) and (X < (Width - ((Border.Width * 2)) + (CellSpacing * 2))) and
             (Y > (Border.Width + (CellSpacing * 2))) and (Y < (Height - ((Border.Width * 2)) + (CellSpacing * 2))) then
          begin
            // Convert the integer rect into a Skia rectangle and draw using the correct paint
            CellRect := TRectF.Create(X, Y, X + CellSize, Y + CellSize);
            if FCells[R][C] then
              ACanvas.DrawRect(CellRect, PaintOn)
            else
              ACanvas.DrawRect(CellRect, PaintOff);
          end;
          // Increase the X position
          Inc(X, CellSize + CellSpacing);
        end;
        // Reset the X position
        X := StartX;
        // Increase the Y position
        Inc(Y, CellSize + CellSpacing);
      end;
    end);
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.PaintSkia(Canvas: ISkCanvas);
begin
  try
    // Draw the cached background image first for optimal overdraw behavior
    if FBackgroundImage <> nil then
      Canvas.DrawImage(FBackgroundImage, 0, 0)
    else
      Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

    // Paint matrix cells on the Skia canvas (direct rendering, zero-copy)
    PaintMatrix(Canvas);
  except
    on E: Exception do
    begin
      // On error, clear canvas with background color
      Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
    end;
  end;
end;

//------------------------------------------------------------------------------
// ON CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SettingsChanged(Sender: TObject);
begin
  // Invalidate the background
  InvalidateBackground;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// ON ANIMATION CHANGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.AnimationChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    // Reset the animation timer when animation settings change
    FLastAnimationMs := FStopwatch.ElapsedMilliseconds;
    // Notify the animation manager to check animation state
    AnimationManager.CheckAnimationState;
  end;
  // Invalidate the buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// ANIMATION TICK (IOBDAnimatable interface)
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.AnimationTick(ElapsedMs: Int64);
var
  CurrentMs, TimeSinceLastTick: Int64;
begin
  if not Animation.Enabled then
    Exit;

  // Get current time from stopwatch
  CurrentMs := FStopwatch.ElapsedMilliseconds;
  // Calculate time since last animation tick
  TimeSinceLastTick := CurrentMs - FLastAnimationMs;

  // Only perform animation action if duration has elapsed
  if TimeSinceLastTick >= Animation.Duration then
  begin
    case Animation.&Type of
      atScrollLeft  : ScrollLeft;
      atScrollRight : ScrollRight;
      atScrollUp    : ScrollUp;
      atScrollDown  : ScrollDown;
      atInvert      : Invert;
    end;

    // Update last tick time
    FLastAnimationMs := CurrentMs;
    
    // Trigger a repaint to display the updated display
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// IS ANIMATING (IOBDAnimatable interface)
//------------------------------------------------------------------------------
function TOBDMatrixDisplay.IsAnimating: Boolean;
begin
  Result := Animation.Enabled;
end;

//------------------------------------------------------------------------------
// GET FRAMES PER SECOND (IOBDAnimatable interface)
//------------------------------------------------------------------------------
function TOBDMatrixDisplay.GetFramesPerSecond: Integer;
begin
  // MatrixDisplay uses custom duration-based timing, so use default FPS
  Result := FramesPerSecond;
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
  ExecuteWithCellsLocked(
    procedure
    begin
      Result := FCells[Row][Col];
    end);
end;

//------------------------------------------------------------------------------
// SET CELL VALUE
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.SetCell(Row: Integer; Col: Integer; Value: Boolean);
begin
  if Row > Rows then Exit;
  if Col > Cols then Exit;

  // If we make it here, the row and column exist so lets set its value.
  ExecuteWithCellsLocked(
    procedure
    begin
      FCells[Row][Col] := Value;
    end);
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
  Invalidate;
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
  Invalidate;
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
      Invalidate;
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
  // Create a lightweight lock object for multithreaded access
  FCellsLock := TObject.Create;
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
  // Initialize stopwatch for high-resolution timing
  FStopwatch := TStopwatch.StartNew;
  FLastAnimationMs := 0;

  if not (csDesigning in ComponentState) then
  begin
    // Register with the animation manager
    AnimationManager.RegisterControl(Self);
  end;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDMatrixDisplay.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    // Unregister from the animation manager
    AnimationManager.UnregisterControl(Self);
  end;
  // Release the cell lock
  FCellsLock.Free;
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
  Invalidate;
end;

//------------------------------------------------------------------------------
// CLEAR DISPLAY
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Clear;
begin
  // Clear all cells inside a monitored block for safe concurrent access
  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
      for R := 0 to FRows - 1 do
        for C := 0 to FCols - 1 do
          FCells[R, C] := False;
    end);
end;

//------------------------------------------------------------------------------
// INVERT DISPLAY
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.Invert;
begin
  // Invert all cells while holding the monitor so timer callbacks cannot interleave
  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
      for R := 0 to FRows - 1 do
        for C := 0 to FCols - 1 do
          FCells[R, C] := not FCells[R, C];
    end);
end;

//------------------------------------------------------------------------------
// SCROLL UP
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollUp(const Loop: Boolean = True; const Step: Integer = 1);
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
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
    end);
end;

//------------------------------------------------------------------------------
// SCROLL DOWN
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollDown(const Loop: Boolean = True; const Step: Integer = 1);
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
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
    end);
end;

//------------------------------------------------------------------------------
// SCROLL LEFT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollLeft(const Loop: Boolean = True; const Step: Integer = 1);
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
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
    end);
end;

//------------------------------------------------------------------------------
// SCROLL RIGHT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.ScrollRight(const Loop: Boolean = True; const Step: Integer = 1);
begin
  // Exit when the step is negative
  if Step <= 0 then Exit;

  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
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
    end);
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

begin
  // First clear the display
  Clear;
  // Loop over the bitmap pixels while holding the monitor to keep state coherent
  ExecuteWithCellsLocked(
    procedure
    var
      R, C: Integer;
    begin
      for R := 0 to Value.Height - 1 do
        for C := 0 to Value.Width - 1 do
        begin
          // Make sure the pixel is in the range of our rows and cols
          if (Row >= 0) and ((Row + R) < Rows) and (Col >= 0) and ((Col + C) < Cols) then
            // Set the cell on/off depending on the pixel
            FCells[Row + R, Col + C] := IsCellOn(R, C);
        end;
    end);
end;

//------------------------------------------------------------------------------
// LOAD BITMAP MASK CENTERED
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadMaskCentered(const Value: TBitmap; const Horizontal: Boolean = True; const Vertical: Boolean = True; const Inversed: Boolean = False);
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
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOAD TEXT
//------------------------------------------------------------------------------
procedure TOBDMatrixDisplay.LoadText(const Value: string; const Row: Integer = 0; const Col: Integer = 0; const Inversed: Boolean = False);
var
  W, H: Integer;
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Typeface: ISkTypeface;
  SkFont: ISkFont;
  Bounds: TRectF;
  B: TBitmap;
begin
  // Create temporary bitmap
  B := TBitmap.Create;
  try
    Typeface := CreateSkTypeface(Font);
    SkFont := TSkFont.Create(Typeface, Font.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := TAlphaColors.Black;

    // Measure the text using Skia so the mask bitmap fits snugly
    SkFont.MeasureText(Value, Bounds, Paint);
    W := Ceil(Bounds.Width);
    H := Ceil(Bounds.Height);

    // Create a Skia surface that renders the monochrome mask
    Surface := TSkSurface.MakeRaster(W, H);
    Canvas := Surface.Canvas;
    Canvas.Clear(TAlphaColors.White);
    Canvas.DrawSimpleText(Value, -Bounds.Left, -Bounds.Top, SkFont, Paint);

    // Extract the Skia-rendered text into a bitmap for cell loading
    SkImageToBitmap(Surface.MakeImageSnapshot, B);
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
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  Typeface: ISkTypeface;
  SkFont: ISkFont;
  Bounds: TRectF;
  B: TBitmap;
begin
  // Create temporary bitmap
  B := TBitmap.Create;
  try
    Typeface := CreateSkTypeface(Font);
    SkFont := TSkFont.Create(Typeface, Font.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := TAlphaColors.Black;

    // Measure the text using Skia so the mask bitmap fits snugly
    SkFont.MeasureText(Value, Bounds, Paint);
    W := Ceil(Bounds.Width);
    H := Ceil(Bounds.Height);

    // Create a Skia surface that renders the monochrome mask
    Surface := TSkSurface.MakeRaster(W, H);
    Canvas := Surface.Canvas;
    Canvas.Clear(TAlphaColors.White);
    Canvas.DrawSimpleText(Value, -Bounds.Left, -Bounds.Top, SkFont, Paint);

    // Extract the Skia-rendered text into a bitmap for cell loading
    SkImageToBitmap(Surface.MakeImageSnapshot, B);
    // Load the mask
    Self.LoadMaskCentered(B, Horizontal, Vertical, Inversed);
  finally
    B.Free;
  end;
end;

end.
