//------------------------------------------------------------------------------
//  ERD.UI.CellMatrix
//
//  TOBDCellMatrix - OBD Studio Pro card for high-voltage battery packs. It
//  draws the voltage of every cell as a bar grouped per module, coloured
//  relative to the pack minimum, maximum and average so outliers stand out.
//  Each module shows its label and temperature; a summary and legend give
//  the minimum, maximum and spread. Hovering a cell shows its number and
//  voltage.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.CellMatrix;

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
  Vcl.Forms,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Fired when a cell is clicked.</summary>
  /// <param name="Sender">The matrix.</param>
  /// <param name="AModule">Zero-based module index.</param>
  /// <param name="ACell">Zero-based cell index within the module.</param>
  TOBDCellClickEvent = procedure(Sender: TObject; AModule, ACell: Integer)
    of object;

  /// <summary>High-voltage battery cell-voltage matrix card.</summary>
  TOBDCellMatrix = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FModuleCount: Integer;
    FCellsPerModule: Integer;
    FVoltages: TArray<TArray<Double>>;
    FTemps: TArray<Double>;
    FWarmTemp: Double;
    FHotModule: Integer;
    FHotCell: Integer;
    FHotRect: TRect;
    FOnCellClick: TOBDCellClickEvent;
    procedure SetTitle(const AValue: string);
    procedure SetModuleCount(AValue: Integer);
    procedure SetCellsPerModule(AValue: Integer);
    procedure SetWarmTemp(AValue: Double);
    function GetCellVoltage(AModule, ACell: Integer): Double;
    procedure SetCellVoltage(AModule, ACell: Integer; AValue: Double);
    function GetModuleTemperature(AModule: Integer): Double;
    procedure SetModuleTemperature(AModule: Integer; AValue: Double);
    procedure EnsureGrid;
    function UsePreviewData: Boolean;
    function HeaderHeight: Integer;
    procedure Measure(out ALo, AHi, AAvg: Double);
    function CellColor(var P: TOBDPainter; AValue, ALo, AHi, AAvg: Double;
      out AOutlier: Boolean): TColor;
    function GridRect: TRect;
    function CellRectAt(AModule, ACell: Integer; const AGrid: TRect;
      ALo, AHi: Double): TRect;
    procedure PaintHeader(var P: TOBDPainter; const R: TRect; ADelta: Double);
    procedure PaintGrid(var P: TOBDPainter);
  protected
    /// <summary>Paints the matrix card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks the hovered cell for the hint.</summary>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Fires <c>OnCellClick</c> for the cell under the mouse.
    /// </summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Clears the hovered cell when the mouse leaves.</summary>
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    /// <summary>Supplies a per-cell hint anchored to the cell.</summary>
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  public
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Creates the matrix with preview defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Replaces the voltages and temperature of one module.
    /// </summary>
    /// <param name="AModule">Zero-based module index.</param>
    /// <param name="AValues">Cell voltages for the module.</param>
    /// <param name="ATemperature">Module temperature in degrees C.
    /// </param>
    procedure SetModule(AModule: Integer; const AValues: array of Double;
      ATemperature: Double);
    /// <summary>Voltage of a single cell in volts.</summary>
    property CellVoltage[AModule, ACell: Integer]: Double
      read GetCellVoltage write SetCellVoltage;
    /// <summary>Temperature of a module in degrees C.</summary>
    property ModuleTemperature[AModule: Integer]: Double
      read GetModuleTemperature write SetModuleTemperature;
  published
    /// <summary>Card title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Number of modules.</summary>
    property ModuleCount: Integer read FModuleCount write SetModuleCount;
    /// <summary>Cells per module.</summary>
    property CellsPerModule: Integer read FCellsPerModule
      write SetCellsPerModule;
    /// <summary>Temperature in degrees C at which a module reads warm.
    /// </summary>
    property WarmTemp: Double read FWarmTemp write SetWarmTemp;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a cell is clicked.</summary>
    property OnCellClick: TOBDCellClickEvent read FOnCellClick
      write FOnCellClick;
  end;

implementation

const
  /// <summary>Preview module temperatures in degrees C from the mockup.
  /// </summary>
  PREVIEW_TEMPS: array[0..7] of Double =
    (23, 24, 24, 25, 26, 25, 24, 31);

function PreviewTemp(AModule: Integer): Double;
begin
  if (AModule >= 0) and (AModule <= High(PREVIEW_TEMPS)) then
    Result := PREVIEW_TEMPS[AModule]
  else
    Result := 25;
end;

{ TOBDCellMatrix }

constructor TOBDCellMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Cell voltages';
  FModuleCount := 8;
  FCellsPerModule := 12;
  FWarmTemp := 30;
  FHotModule := -1;
  FHotCell := -1;
  Width := 620;
  Height := 352;
end;

procedure TOBDCellMatrix.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDCellMatrix.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDCellMatrix.EnsureGrid;
var
  M, I, Idx: Integer;
begin
  if (FModuleCount > 0) and (FCellsPerModule > 0) and
    (Length(FVoltages) = FModuleCount) then
    if Length(FVoltages[0]) = FCellsPerModule then
      Exit;
  SetLength(FVoltages, FModuleCount);
  SetLength(FTemps, FModuleCount);
  for M := 0 to FModuleCount - 1 do
    SetLength(FVoltages[M], FCellsPerModule);
  if UsePreviewData then
  begin
    for M := 0 to FModuleCount - 1 do
    begin
      for I := 0 to FCellsPerModule - 1 do
      begin
        Idx := M * FCellsPerModule + I;
        FVoltages[M][I] := 3.752 + 0.012 * Sin(Idx * 0.7) +
          0.006 * Cos(Idx * 1.9);
      end;
      FTemps[M] := PreviewTemp(M);
    end;
    if (FModuleCount * FCellsPerModule) > 76 then
      FVoltages[76 div FCellsPerModule][76 mod FCellsPerModule] := 3.712;
    if (FModuleCount * FCellsPerModule) > 40 then
      FVoltages[40 div FCellsPerModule][40 mod FCellsPerModule] := 3.791;
  end;
end;

function TOBDCellMatrix.UsePreviewData: Boolean;
begin
  Result := IsPreview;
end;

function TOBDCellMatrix.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

procedure TOBDCellMatrix.Measure(out ALo, AHi, AAvg: Double);
var
  M, I, Count: Integer;
  V, Sum: Double;
begin
  ALo := 1.0E9;
  AHi := -1.0E9;
  Sum := 0;
  Count := 0;
  for M := 0 to High(FVoltages) do
    for I := 0 to High(FVoltages[M]) do
    begin
      V := FVoltages[M][I];
      if V < ALo then
        ALo := V;
      if V > AHi then
        AHi := V;
      Sum := Sum + V;
      Inc(Count);
    end;
  if Count = 0 then
  begin
    ALo := 3.70;
    AHi := 3.80;
    AAvg := 3.75;
    Exit;
  end;
  AAvg := Sum / Count;
  if AHi - ALo < 0.001 then
  begin
    ALo := ALo - 0.05;
    AHi := AHi + 0.05;
  end;
end;

function TOBDCellMatrix.CellColor(var P: TOBDPainter;
  AValue, ALo, AHi, AAvg: Double; out AOutlier: Boolean): TColor;
var
  Band: Double;
begin
  Band := System.Math.Max((AHi - ALo) * 0.35, 0.005);
  if AValue <= AAvg - Band then
  begin
    AOutlier := True;
    Result := Palette.Warning;
  end
  else if AValue >= AAvg + Band then
  begin
    AOutlier := True;
    Result := P.AccentText;
  end
  else
  begin
    AOutlier := False;
    Result := OBDMixColor(Palette.Success, Palette.GaugeFace, 0.6);
  end;
end;

function TOBDCellMatrix.GridRect: TRect;
begin
  Result := Rect(ScaleValue(52), HeaderHeight + ScaleValue(22),
    Width - ScaleValue(16), Height - ScaleValue(64));
end;

function TOBDCellMatrix.CellRectAt(AModule, ACell: Integer; const AGrid: TRect;
  ALo, AHi: Double): TRect;
var
  MW, BW: Double;
  V, Top: Double;
  L: Double;
begin
  MW := AGrid.Width / FModuleCount;
  BW := (MW - ScaleValue(8)) / FCellsPerModule;
  L := AGrid.Left + AModule * MW + ScaleValue(4) + ACell * BW;
  V := FVoltages[AModule][ACell];
  Top := AGrid.Bottom - (V - ALo) / (AHi - ALo) * AGrid.Height;
  Result := Rect(Round(L), Round(Top), Round(L + BW - 1.5), AGrid.Bottom);
end;

procedure TOBDCellMatrix.PaintHeader(var P: TOBDPainter; const R: TRect;
  ADelta: Double);
var
  X, Y, ChipW: Integer;
  Sub, ChipText: string;
begin
  Y := R.Top + R.Height div 2;
  P.Glyph(glEvBattery, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  P.Text(X, Y - P.S(9), FTitle, OBD_TEXT_TITLE, Palette.ForegroundText,
    twBold, taLeftJustify, R.Width - P.S(180));
  Sub := Format('%d cells ' + #$00B7 + ' %d modules ' + #$00D7 + ' %d',
    [FModuleCount * FCellsPerModule, FModuleCount, FCellsPerModule]);
  P.Text(X, Y + P.S(10), Sub, OBD_TEXT_SMALL, Palette.GaugeLabel);
  ChipText := Format(#$0394 + ' %.0f mV', [ADelta * 1000]);
  ChipW := P.ChipWidth(ChipText);
  P.Chip(R.Right - P.S(16) - ChipW, Y - P.S(10), ChipText, Palette.Warning,
    True);
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDCellMatrix.PaintGrid(var P: TOBDPainter);
var
  G: TRect;
  Lo, Hi, Avg, V: Double;
  M, I: Integer;
  MW: Double;
  MX, YY, Mid: Integer;
  CR: TRect;
  Col: TColor;
  Outlier, Warm: Boolean;
  Temp: Double;
  TempText: string;
  Steps: array[0..2] of Double;
  K: Integer;
begin
  G := GridRect;
  Measure(Lo, Hi, Avg);
  MW := G.Width / FModuleCount;

  // Horizontal reference lines with voltage labels.
  Steps[0] := Lo;
  Steps[1] := (Lo + Hi) / 2;
  Steps[2] := Hi;
  for K := 0 to 2 do
  begin
    YY := Round(G.Bottom - (Steps[K] - Lo) / (Hi - Lo) * G.Height);
    P.HLine(G.Left, YY, G.Width, Palette.NeutralLight);
    P.Text(G.Left - P.S(6), YY, Format('%.2f', [Steps[K]]), OBD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taRightJustify);
  end;

  for M := 0 to FModuleCount - 1 do
  begin
    MX := Round(G.Left + M * MW);
    if M > 0 then
      P.VLine(MX, G.Top - P.S(4), G.Height + P.S(8), Palette.NeutralDark);
    for I := 0 to FCellsPerModule - 1 do
    begin
      V := FVoltages[M][I];
      Col := CellColor(P, V, Lo, Hi, Avg, Outlier);
      CR := CellRectAt(M, I, G, Lo, Hi);
      P.FillRect(CR, Col);
      if Outlier then
        P.HLine(CR.Left, CR.Top - P.S(2), CR.Width, Col);
    end;
    Mid := Round(G.Left + M * MW + MW / 2);
    P.Text(Mid, G.Bottom + P.S(14), Format('M%d', [M + 1]), OBD_TEXT_SMALL,
      Palette.GaugeLabel, twSemibold, taCenter);
    Temp := FTemps[M];
    Warm := Temp >= FWarmTemp;
    TempText := Format('%.0f ' + #$00B0 + 'C', [Temp]);
    if Warm then
      Col := Palette.Warning
    else
      Col := Palette.Subtle;
    P.Chip(Mid - P.ChipWidth(TempText, False, OBD_TEXT_CAPS) div 2,
      G.Bottom + P.S(26), TempText, Col, False, False, OBD_TEXT_CAPS);
  end;

  // Summary and legend.
  P.Text(ScaleValue(16), G.Bottom + P.S(37),
    Format('min %.3f ' + #$00B7 + ' max %.3f ' + #$00B7 + ' ' + #$0394 +
    ' %.0f mV', [Lo, Hi, (Hi - Lo) * 1000]), OBD_TEXT_SMALL,
    Palette.GaugeLabel);
end;

procedure TOBDCellMatrix.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Lo, Hi, Avg: Double;
begin
  EnsureGrid;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    Measure(Lo, Hi, Avg);
    PaintHeader(P, Rect(0, 0, Width, HeaderHeight), Hi - Lo);
    PaintGrid(P);
  finally
    P.Free;
  end;
end;

procedure TOBDCellMatrix.SetModule(AModule: Integer;
  const AValues: array of Double; ATemperature: Double);
var
  I: Integer;
begin
  EnsureGrid;
  if (AModule < 0) or (AModule >= FModuleCount) then
    Exit;
  for I := 0 to System.Math.Min(High(AValues), FCellsPerModule - 1) do
    FVoltages[AModule][I] := AValues[I];
  FTemps[AModule] := ATemperature;
  Invalidate;
end;

function TOBDCellMatrix.GetCellVoltage(AModule, ACell: Integer): Double;
begin
  EnsureGrid;
  if (AModule >= 0) and (AModule < FModuleCount) and (ACell >= 0) and
    (ACell < FCellsPerModule) then
    Result := FVoltages[AModule][ACell]
  else
    Result := 0;
end;

procedure TOBDCellMatrix.SetCellVoltage(AModule, ACell: Integer;
  AValue: Double);
begin
  EnsureGrid;
  if (AModule >= 0) and (AModule < FModuleCount) and (ACell >= 0) and
    (ACell < FCellsPerModule) then
  begin
    FVoltages[AModule][ACell] := AValue;
    Invalidate;
  end;
end;

function TOBDCellMatrix.GetModuleTemperature(AModule: Integer): Double;
begin
  EnsureGrid;
  if (AModule >= 0) and (AModule < FModuleCount) then
    Result := FTemps[AModule]
  else
    Result := 0;
end;

procedure TOBDCellMatrix.SetModuleTemperature(AModule: Integer;
  AValue: Double);
begin
  EnsureGrid;
  if (AModule >= 0) and (AModule < FModuleCount) then
  begin
    FTemps[AModule] := AValue;
    Invalidate;
  end;
end;

procedure TOBDCellMatrix.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDCellMatrix.SetModuleCount(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FModuleCount = AValue then
    Exit;
  FModuleCount := AValue;
  SetLength(FVoltages, 0);
  SetLength(FTemps, 0);
  FHotModule := -1;
  FHotCell := -1;
  FHotRect := Rect(0, 0, 0, 0);
  Invalidate;
end;

procedure TOBDCellMatrix.SetCellsPerModule(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FCellsPerModule = AValue then
    Exit;
  FCellsPerModule := AValue;
  SetLength(FVoltages, 0);
  SetLength(FTemps, 0);
  FHotModule := -1;
  FHotCell := -1;
  FHotRect := Rect(0, 0, 0, 0);
  Invalidate;
end;

procedure TOBDCellMatrix.SetWarmTemp(AValue: Double);
begin
  if SameValue(FWarmTemp, AValue) then
    Exit;
  FWarmTemp := AValue;
  Invalidate;
end;

procedure TOBDCellMatrix.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  G: TRect;
  Lo, Hi, Avg: Double;
  M, I: Integer;
  MW, BW, L: Double;
  OldM, OldC: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  EnsureGrid;
  OldM := FHotModule;
  OldC := FHotCell;
  FHotModule := -1;
  FHotCell := -1;
  G := GridRect;
  if (G.Width > 0) and (G.Height > 0) and
    (Y >= G.Top - ScaleValue(6)) and (Y <= G.Bottom) and
    (X >= G.Left) and (X <= G.Right) and (FModuleCount > 0) and
    (FCellsPerModule > 0) then
  begin
    MW := G.Width / FModuleCount;
    M := Trunc((X - G.Left) / MW);
    if (M >= 0) and (M < FModuleCount) then
    begin
      BW := (MW - ScaleValue(8)) / FCellsPerModule;
      L := X - (G.Left + M * MW + ScaleValue(4));
      if (L >= 0) and (BW > 0) then
      begin
        I := Trunc(L / BW);
        if (I >= 0) and (I < FCellsPerModule) then
        begin
          FHotModule := M;
          FHotCell := I;
          Measure(Lo, Hi, Avg);
          FHotRect := CellRectAt(M, I, G, Lo, Hi);
          FHotRect.Top := G.Top;
        end;
      end;
    end;
  end;
  if (FHotModule <> OldM) or (FHotCell <> OldC) then
  begin
    Hint := '';
    Application.CancelHint;
  end;
end;

procedure TOBDCellMatrix.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (FHotModule >= 0) and (FHotCell >= 0) and
    Assigned(FOnCellClick) then
    FOnCellClick(Self, FHotModule, FHotCell);
end;

procedure TOBDCellMatrix.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHotModule := -1;
  FHotCell := -1;
end;

procedure TOBDCellMatrix.CMHintShow(var Message: TCMHintShow);
var
  Idx: Integer;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  if (FHotModule < 0) or (FHotCell < 0) then
    Exit;
  Idx := FHotModule * FCellsPerModule + FHotCell + 1;
  Message.HintInfo^.HintStr := Format('Cell %d ' + #$00B7 + ' %.3f V',
    [Idx, FVoltages[FHotModule][FHotCell]]);
  Message.HintInfo^.CursorRect := FHotRect;
end;

end.
