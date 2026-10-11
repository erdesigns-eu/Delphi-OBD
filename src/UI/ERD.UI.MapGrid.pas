//------------------------------------------------------------------------------
//  ERD.UI.MapGrid
//
//  TOBDMapGrid - OBD Studio Pro card that shows a calibration map (an ECU
//  lookup table) as a coloured grid. X and Y axis breakpoints label the
//  columns and rows, every cell value is coloured on a scale, the live
//  operating point is drawn as an interpolated marker over highlighted
//  cells, edited cells are marked and a selected cell can be moved and
//  adjusted with the keyboard.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.MapGrid;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Fired when the selected cell changes.</summary>
  /// <param name="Sender">The map grid.</param>
  /// <param name="ARow">Selected row index.</param>
  /// <param name="ACol">Selected column index.</param>
  TOBDMapSelectEvent = procedure(Sender: TObject; ARow, ACol: Integer)
    of object;

  /// <summary>Fired when a cell value is edited.</summary>
  /// <param name="Sender">The map grid.</param>
  /// <param name="ARow">Edited row index.</param>
  /// <param name="ACol">Edited column index.</param>
  /// <param name="AValue">New cell value.</param>
  TOBDMapCellEditEvent = procedure(Sender: TObject; ARow, ACol: Integer;
    AValue: Double) of object;

  /// <summary>Calibration map grid with a live operating point.</summary>
  TOBDMapGrid = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FSubtitle: string;
    FAxisHint: string;
    FUnitText: string;
    FRows: Integer;
    FCols: Integer;
    FXAxis: array of Double;
    FYAxis: array of Double;
    FValues: array of array of Double;
    FEdited: array of array of Boolean;
    FSelRow: Integer;
    FSelCol: Integer;
    FOpX: Double;
    FOpY: Double;
    FHasOperatingPoint: Boolean;
    FEditStep: Double;
    FOnSelect: TOBDMapSelectEvent;
    FOnCellEdit: TOBDMapCellEditEvent;
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetAxisHint(const AValue: string);
    procedure SetUnitText(const AValue: string);
    procedure SetEditStep(AValue: Double);
    function GetValue(ARow, ACol: Integer): Double;
    procedure SetValue(ARow, ACol: Integer; AValue: Double);
    function GetEdited(ARow, ACol: Integer): Boolean;
    function UsePreviewData: Boolean;
    function HasData: Boolean;
    function EnsureGrid: Boolean;
    function HeaderHeight: Integer;
    function GridRect: TRect;
    function CellRect(const AGrid: TRect; ARow, ACol: Integer): TRect;
    function ValueMin: Double;
    function ValueMax: Double;
    function CellColor(var P: TOBDPainter; AValue: Double): TColor;
    function AxisFraction(const AAxis: array of Double; AValue: Double): Double;
    procedure MoveSelection(ADRow, ADCol: Integer);
    procedure AdjustSelected(ADelta: Double);
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintGrid(var P: TOBDPainter; const AGrid: TRect);
    procedure PaintOperatingPoint(var P: TOBDPainter; const AGrid: TRect);
    procedure PaintScale(var P: TOBDPainter);
  protected
    /// <summary>Paints the map grid card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Selects the cell under the mouse.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Moves the selection and edits with the keyboard.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates the map grid with preview defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Defines the map size and axis breakpoints; clears
    /// values.</summary>
    /// <param name="AXAxis">Column breakpoints (engine speed).</param>
    /// <param name="AYAxis">Row breakpoints (load).</param>
    procedure SetAxes(const AXAxis, AYAxis: array of Double);
    /// <summary>Sets the live operating point, interpolated over the
    /// axes.</summary>
    /// <param name="AX">X value (column axis units).</param>
    /// <param name="AY">Y value (row axis units).</param>
    procedure SetOperatingPoint(AX, AY: Double);
    /// <summary>Clears the live operating point.</summary>
    procedure ClearOperatingPoint;
    /// <summary>Number of columns.</summary>
    /// <returns>Column count.</returns>
    function ColCount: Integer;
    /// <summary>Number of rows.</summary>
    /// <returns>Row count.</returns>
    function RowCount: Integer;
    /// <summary>Selected row index, or -1.</summary>
    property SelectedRow: Integer read FSelRow;
    /// <summary>Selected column index, or -1.</summary>
    property SelectedCol: Integer read FSelCol;
    /// <summary>Cell value indexer.</summary>
    property Values[ARow, ACol: Integer]: Double read GetValue write SetValue;
    /// <summary>True when a cell has been edited.</summary>
    property Edited[ARow, ACol: Integer]: Boolean read GetEdited;
  published
    /// <summary>Card title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Caption above the grid.</summary>
    property AxisHint: string read FAxisHint write SetAxisHint;
    /// <summary>Cell value unit.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Amount added or removed by the +/- keys.</summary>
    property EditStep: Double read FEditStep write SetEditStep;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the selected cell changes.</summary>
    property OnSelect: TOBDMapSelectEvent read FOnSelect write FOnSelect;
    /// <summary>Fires when a cell value is edited.</summary>
    property OnCellEdit: TOBDMapCellEditEvent read FOnCellEdit
      write FOnCellEdit;
  end;

implementation

const
  PREVIEW_ROWS = 8;
  PREVIEW_COLS = 8;

function PreviewMapValue(ARow, ACol: Integer): Double;
begin
  Result := 4 + ARow * 6.2 + ACol * 1.3 + (ARow * ACol) * 0.35;
end;

{ TOBDMapGrid }

constructor TOBDMapGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Injection quantity';
  FSubtitle := 'mg/stroke ' + #$00B7 + ' A2L EDC17C64 ' + #$00B7 + ' 8 ' +
    #$00D7 + ' 8';
  FAxisHint := 'Pedal % (rows) ' + #$00B7 + ' engine speed rpm (columns)';
  FUnitText := 'mg';
  FEditStep := 0.1;
  FSelRow := -1;
  FSelCol := -1;
  TabStop := True;
  Width := 560;
  Height := 420;
end;

procedure TOBDMapGrid.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDMapGrid.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDMapGrid.SetAxes(const AXAxis, AYAxis: array of Double);
var
  I, J: Integer;
begin
  FCols := Length(AXAxis);
  FRows := Length(AYAxis);
  SetLength(FXAxis, FCols);
  SetLength(FYAxis, FRows);
  for I := 0 to FCols - 1 do
    FXAxis[I] := AXAxis[I];
  for I := 0 to FRows - 1 do
    FYAxis[I] := AYAxis[I];
  SetLength(FValues, FRows, FCols);
  SetLength(FEdited, FRows, FCols);
  for I := 0 to FRows - 1 do
    for J := 0 to FCols - 1 do
    begin
      FValues[I][J] := 0;
      FEdited[I][J] := False;
    end;
  FSelRow := -1;
  FSelCol := -1;
  Invalidate;
end;

procedure TOBDMapGrid.SetOperatingPoint(AX, AY: Double);
begin
  FOpX := AX;
  FOpY := AY;
  FHasOperatingPoint := True;
  Invalidate;
end;

procedure TOBDMapGrid.ClearOperatingPoint;
begin
  if not FHasOperatingPoint then
    Exit;
  FHasOperatingPoint := False;
  Invalidate;
end;

function TOBDMapGrid.ColCount: Integer;
begin
  Result := FCols;
end;

function TOBDMapGrid.RowCount: Integer;
begin
  Result := FRows;
end;

function TOBDMapGrid.GetValue(ARow, ACol: Integer): Double;
begin
  if (ARow >= 0) and (ARow < FRows) and (ACol >= 0) and (ACol < FCols) then
    Result := FValues[ARow][ACol]
  else
    Result := 0;
end;

procedure TOBDMapGrid.SetValue(ARow, ACol: Integer; AValue: Double);
begin
  if (ARow < 0) or (ARow >= FRows) or (ACol < 0) or (ACol >= FCols) then
    Exit;
  if SameValue(FValues[ARow][ACol], AValue) then
    Exit;
  FValues[ARow][ACol] := AValue;
  FEdited[ARow][ACol] := True;
  Invalidate;
end;

function TOBDMapGrid.GetEdited(ARow, ACol: Integer): Boolean;
begin
  if (ARow >= 0) and (ARow < FRows) and (ACol >= 0) and (ACol < FCols) then
    Result := FEdited[ARow][ACol]
  else
    Result := False;
end;

procedure TOBDMapGrid.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDMapGrid.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDMapGrid.SetAxisHint(const AValue: string);
begin
  if FAxisHint = AValue then
    Exit;
  FAxisHint := AValue;
  Invalidate;
end;

procedure TOBDMapGrid.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Invalidate;
end;

procedure TOBDMapGrid.SetEditStep(AValue: Double);
begin
  if SameValue(FEditStep, AValue) then
    Exit;
  FEditStep := AValue;
end;

function TOBDMapGrid.HasData: Boolean;
begin
  Result := (FRows > 0) and (FCols > 0);
end;

function TOBDMapGrid.UsePreviewData: Boolean;
begin
  Result := IsPreview and not HasData;
end;

function TOBDMapGrid.EnsureGrid: Boolean;
var
  R, C: Integer;
  Rpms: array [0 .. PREVIEW_COLS - 1] of Double;
  Loads: array [0 .. PREVIEW_ROWS - 1] of Double;
begin
  Result := HasData;
  if Result or not IsPreview then
    Exit;
  Rpms[0] := 800;
  Rpms[1] := 1250;
  Rpms[2] := 1750;
  Rpms[3] := 2250;
  Rpms[4] := 2750;
  Rpms[5] := 3250;
  Rpms[6] := 3750;
  Rpms[7] := 4500;
  Loads[0] := 0;
  Loads[1] := 10;
  Loads[2] := 20;
  Loads[3] := 35;
  Loads[4] := 50;
  Loads[5] := 65;
  Loads[6] := 80;
  Loads[7] := 100;
  SetAxes(Rpms, Loads);
  for R := 0 to FRows - 1 do
    for C := 0 to FCols - 1 do
    begin
      FValues[R][C] := PreviewMapValue(R, C);
      FEdited[R][C] := ((R = 5) and (C = 6)) or ((R = 6) and (C = 6));
    end;
  FOpX := 2250;
  FOpY := 50;
  FHasOperatingPoint := True;
  Result := True;
end;

function TOBDMapGrid.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDMapGrid.GridRect: TRect;
begin
  Result := Rect(ScaleValue(74), HeaderHeight + ScaleValue(48),
    Width - ScaleValue(20), Height - ScaleValue(58));
end;

function TOBDMapGrid.CellRect(const AGrid: TRect; ARow, ACol: Integer): TRect;
var
  CW, CH: Double;
begin
  if (FCols = 0) or (FRows = 0) then
    Exit(Rect(0, 0, 0, 0));
  CW := AGrid.Width / FCols;
  CH := AGrid.Height / FRows;
  Result := Rect(AGrid.Left + Round(ACol * CW), AGrid.Top + Round(ARow * CH),
    AGrid.Left + Round((ACol + 1) * CW), AGrid.Top + Round((ARow + 1) * CH));
end;

function TOBDMapGrid.ValueMin: Double;
var
  R, C: Integer;
begin
  Result := 0;
  if (FRows = 0) or (FCols = 0) then
    Exit;
  Result := FValues[0][0];
  for R := 0 to FRows - 1 do
    for C := 0 to FCols - 1 do
      Result := System.Math.Min(Result, FValues[R][C]);
end;

function TOBDMapGrid.ValueMax: Double;
var
  R, C: Integer;
begin
  Result := 1;
  if (FRows = 0) or (FCols = 0) then
    Exit;
  Result := FValues[0][0];
  for R := 0 to FRows - 1 do
    for C := 0 to FCols - 1 do
      Result := System.Math.Max(Result, FValues[R][C]);
end;

function TOBDMapGrid.CellColor(var P: TOBDPainter; AValue: Double): TColor;
var
  T, VMax: Double;
begin
  VMax := ValueMax;
  if VMax <= 0 then
    T := 0
  else
    T := EnsureRange(AValue / VMax, 0, 1);
  Result := P.Tint(Palette.Accent, 0.08 + T * 0.8);
end;

function TOBDMapGrid.AxisFraction(const AAxis: array of Double;
  AValue: Double): Double;
var
  I, N: Integer;
begin
  N := Length(AAxis);
  if N <= 1 then
    Exit(0);
  if AValue <= AAxis[0] then
    Exit(0);
  if AValue >= AAxis[N - 1] then
    Exit(N - 1);
  for I := 1 to N - 1 do
    if AValue <= AAxis[I] then
    begin
      if SameValue(AAxis[I], AAxis[I - 1]) then
        Exit(I - 1);
      Exit((I - 1) + (AValue - AAxis[I - 1]) / (AAxis[I] - AAxis[I - 1]));
    end;
  Result := N - 1;
end;

procedure TOBDMapGrid.MoveSelection(ADRow, ADCol: Integer);
begin
  if not HasData then
    Exit;
  if (FSelRow < 0) or (FSelCol < 0) then
  begin
    FSelRow := 0;
    FSelCol := 0;
  end
  else
  begin
    FSelRow := EnsureRange(FSelRow + ADRow, 0, FRows - 1);
    FSelCol := EnsureRange(FSelCol + ADCol, 0, FCols - 1);
  end;
  Invalidate;
  if Assigned(FOnSelect) then
    FOnSelect(Self, FSelRow, FSelCol);
end;

procedure TOBDMapGrid.AdjustSelected(ADelta: Double);
begin
  if (FSelRow < 0) or (FSelCol < 0) then
    Exit;
  FValues[FSelRow][FSelCol] := FValues[FSelRow][FSelCol] + ADelta;
  FEdited[FSelRow][FSelCol] := True;
  Invalidate;
  if Assigned(FOnCellEdit) then
    FOnCellEdit(Self, FSelRow, FSelCol, FValues[FSelRow][FSelCol]);
end;

procedure TOBDMapGrid.PaintHeader(var P: TOBDPainter; const R: TRect);
var
  X, Y, ChipW: Integer;
  ChipText: string;
begin
  Y := R.Top + R.Height div 2;
  P.Glyph(glCalibrationMap, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  if FSubtitle <> '' then
  begin
    P.Text(X, Y - P.S(8), FTitle, OBD_TEXT_TITLE, Palette.ForegroundText,
      twBold, taLeftJustify, R.Width - P.S(180));
    P.Text(X, Y + P.S(11), FSubtitle, OBD_TEXT_SMALL, Palette.GaugeLabel,
      twRegular, taLeftJustify, R.Width - P.S(180));
  end
  else
    P.Text(X, Y, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText, twBold,
      taLeftJustify, R.Width - P.S(180));
  if FHasOperatingPoint then
  begin
    ChipText := 'LIVE';
    ChipW := P.ChipWidth(ChipText);
    P.Chip(R.Right - P.S(16) - ChipW, Y - P.S(10), ChipText, Palette.Success);
  end;
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDMapGrid.PaintGrid(var P: TOBDPainter; const AGrid: TRect);
var
  R, C: Integer;
  CR: TRect;
  Fill, Ink: TColor;
  T, VMax: Double;
  Txt: string;
  W: TOBDTextWeight;
begin
  P.Caps(AGrid.Left - P.S(58), AGrid.Top - P.S(32), FAxisHint);
  VMax := ValueMax;
  // Column axis labels.
  for C := 0 to FCols - 1 do
  begin
    CR := CellRect(AGrid, 0, C);
    P.Text((CR.Left + CR.Right) div 2, AGrid.Top - P.S(10),
      FormatFloat('0', FXAxis[C]), OBD_TEXT_SMALL, Palette.GaugeLabel,
      twSemibold, taCenter);
  end;
  for R := 0 to FRows - 1 do
  begin
    CR := CellRect(AGrid, R, 0);
    P.Text(AGrid.Left - P.S(10), (CR.Top + CR.Bottom) div 2,
      FormatFloat('0', FYAxis[R]), OBD_TEXT_SMALL, Palette.GaugeLabel,
      twSemibold, taRightJustify);
    for C := 0 to FCols - 1 do
    begin
      CR := CellRect(AGrid, R, C);
      if VMax <= 0 then
        T := 0
      else
        T := EnsureRange(FValues[R][C] / VMax, 0, 1);
      Fill := CellColor(P, FValues[R][C]);
      P.FillRect(Rect(CR.Left, CR.Top, CR.Right - P.S(1), CR.Bottom - P.S(1)),
        Fill);
      if T > 0.55 then
        Ink := P.OnAccent
      else
        Ink := Palette.ForegroundText;
      if FEdited[R][C] then
        W := twMonoBold
      else
        W := twMono;
      Txt := FormatFloat('0.0', FValues[R][C]);
      P.Text((CR.Left + CR.Right) div 2, (CR.Top + CR.Bottom) div 2, Txt,
        OBD_TEXT_BODY, Ink, W, taCenter);
      if FEdited[R][C] then
        P.Polygon([OBDPointF(CR.Right - P.S(9), CR.Top),
          OBDPointF(CR.Right - P.S(1), CR.Top),
          OBDPointF(CR.Right - P.S(1), CR.Top + P.S(8))],
          Palette.ForegroundText);
    end;
  end;
  if (FSelRow >= 0) and (FSelCol >= 0) then
  begin
    CR := CellRect(AGrid, FSelRow, FSelCol);
    P.FrameRect(Rect(CR.Left, CR.Top, CR.Right, CR.Bottom), P.AccentText,
      P.S(2));
  end;
end;

procedure TOBDMapGrid.PaintOperatingPoint(var P: TOBDPainter;
  const AGrid: TRect);
var
  Fx, Fy: Double;
  CR: TRect;
  PX, PY, Lr, Lc: Integer;
begin
  if not FHasOperatingPoint then
    Exit;
  Fx := AxisFraction(FXAxis, FOpX);
  Fy := AxisFraction(FYAxis, FOpY);
  Lc := EnsureRange(Round(Fx), 0, FCols - 1);
  Lr := EnsureRange(Round(Fy), 0, FRows - 1);
  CR := CellRect(AGrid, Lr, Lc);
  P.FrameRect(Rect(CR.Left, CR.Top, CR.Right, CR.Bottom),
    Palette.ForegroundText, P.S(2));
  PX := AGrid.Left + Round((Fx + 0.5) / FCols * AGrid.Width);
  PY := AGrid.Top + Round((Fy + 0.5) / FRows * AGrid.Height);
  P.Ellipse(PX - P.SF(5), PY - P.SF(5), P.SF(10), P.SF(10),
    Palette.ForegroundText, Palette.GaugeFace, P.SF(2));
end;

procedure TOBDMapGrid.PaintScale(var P: TOBDPainter);
var
  SY, SX0, SX1, I, Steps, EditCount, R, C: Integer;
  SegW: Double;
  Txt: string;
begin
  SY := Height - ScaleValue(22);
  P.Text(ScaleValue(16), SY, FormatFloat('0.0', ValueMin), OBD_TEXT_SMALL,
    Palette.GaugeLabel, twRegular);
  SX0 := ScaleValue(60);
  SX1 := Width - ScaleValue(220);
  Steps := 40;
  if SX1 > SX0 then
  begin
    SegW := (SX1 - SX0) / Steps;
    for I := 0 to Steps - 1 do
      P.FillRect(Rect(SX0 + Round(I * SegW), SY - P.S(4),
        SX0 + Round((I + 1) * SegW) + 1, SY + P.S(4)),
        P.Tint(Palette.Accent, 0.08 + I / Steps * 0.8));
  end;
  P.Text(SX1 + P.S(8), SY, FormatFloat('0.0', ValueMax), OBD_TEXT_SMALL,
    Palette.GaugeLabel);
  EditCount := 0;
  for R := 0 to FRows - 1 do
    for C := 0 to FCols - 1 do
      if FEdited[R][C] then
        Inc(EditCount);
  if EditCount > 0 then
  begin
    if EditCount = 1 then
      Txt := '1 cell edited ' + #$00B7 + ' not written'
    else
      Txt := Format('%d cells edited ' + #$00B7 + ' not written', [EditCount]);
    P.Text(Width - ScaleValue(16), SY, Txt, OBD_TEXT_SMALL, P.AccentText,
      twSemibold, taRightJustify);
  end;
end;

procedure TOBDMapGrid.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Grid: TRect;
begin
  EnsureGrid;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    PaintHeader(P, Rect(0, 0, Width, HeaderHeight));
    if not HasData then
    begin
      P.Text(Width div 2, HeaderHeight + (Height - HeaderHeight) div 2,
        'No map loaded', OBD_TEXT_BODY, Palette.GaugeLabel, twRegular,
        taCenter);
      Exit;
    end;
    Grid := GridRect;
    if (Grid.Width < P.S(40)) or (Grid.Height < P.S(40)) then
      Exit;
    PaintGrid(P, Grid);
    PaintOperatingPoint(P, Grid);
    PaintScale(P);
  finally
    P.Free;
  end;
end;

procedure TOBDMapGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Grid, CR: TRect;
  R, C: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if CanFocus then
    SetFocus;
  if not HasData then
    Exit;
  Grid := GridRect;
  if not PtInRect(Grid, Point(X, Y)) then
    Exit;
  for R := 0 to FRows - 1 do
    for C := 0 to FCols - 1 do
    begin
      CR := CellRect(Grid, R, C);
      if PtInRect(CR, Point(X, Y)) then
      begin
        FSelRow := R;
        FSelCol := C;
        Invalidate;
        if Assigned(FOnSelect) then
          FOnSelect(Self, R, C);
        Exit;
      end;
    end;
end;

procedure TOBDMapGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT:
      MoveSelection(0, -1);
    VK_RIGHT:
      MoveSelection(0, 1);
    VK_UP:
      MoveSelection(-1, 0);
    VK_DOWN:
      MoveSelection(1, 0);
    VK_ADD, VK_OEM_PLUS:
      AdjustSelected(FEditStep);
    VK_SUBTRACT, VK_OEM_MINUS:
      AdjustSelected(-FEditStep);
  end;
end;

end.
