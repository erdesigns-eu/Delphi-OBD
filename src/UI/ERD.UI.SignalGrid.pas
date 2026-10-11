//------------------------------------------------------------------------------
//  ERD.UI.SignalGrid
//
//  TOBDSignalGrid - OBD Studio Pro live measurement grid with value, unit,
//  min/max and per-signal sparkline ring buffers.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.SignalGrid;

interface

uses
  Winapi.Windows,
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
  TOBDSignalGrid = class;

  /// <summary>One live signal row with a ring buffer of recent values.</summary>
  TOBDSignalGridItem = class(TCollectionItem)
  strict private
    FName: string;
    FUnitText: string;
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FCapacity: Integer;
    FSamples: array of Double;
    FSampleStart: Integer;
    FSampleCount: Integer;
    procedure SetName(const AValue: string);
    procedure SetUnitText(const AValue: string);
    procedure SetCapacity(AValue: Integer);
    procedure SetValueInternal(AValue: Double);
  protected
    /// <summary>Returns the signal name for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a signal row with empty numeric values.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another signal-grid item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Pushes a live value, updating min, max and the sparkline.</summary>
    /// <param name="AValue">New value.</param>
    procedure PushValue(AValue: Double);
    /// <summary>Clears min and max while keeping the latest value.</summary>
    procedure ResetMinMax;
    /// <summary>Clears sparkline samples.</summary>
    procedure ClearSamples;
    /// <summary>Returns one sample in chronological order.</summary>
    /// <param name="AIndex">Sample index.</param>
    /// <returns>Sample value.</returns>
    function Sample(AIndex: Integer): Double;
    /// <summary>Number of sparkline samples.</summary>
    property SampleCount: Integer read FSampleCount;
  published
    /// <summary>Signal display name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Physical unit text.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Latest value.</summary>
    property Value: Double read FValue;
    /// <summary>Minimum value seen since reset.</summary>
    property MinValue: Double read FMinValue;
    /// <summary>Maximum value seen since reset.</summary>
    property MaxValue: Double read FMaxValue;
    /// <summary>Sparkline ring-buffer capacity.</summary>
    property Capacity: Integer read FCapacity write SetCapacity default 40;
  end;

  /// <summary>Streamable live-signal collection.</summary>
  TOBDSignalGridCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDSignalGridItem;
    procedure SetItem(Index: Integer; AValue: TOBDSignalGridItem);
  protected
    /// <summary>Notifies the owner when a row changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a signal-grid collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a signal row.</summary>
    /// <returns>The new row.</returns>
    function Add: TOBDSignalGridItem;
    /// <summary>Copies another signal-grid collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Finds a signal row by name.</summary>
    /// <param name="AName">Signal name.</param>
    /// <returns>Row index, or -1.</returns>
    function IndexOfName(const AName: string): Integer;
    /// <summary>Typed signal row indexer.</summary>
    property Items[Index: Integer]: TOBDSignalGridItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Live values with min/max columns and sparklines.</summary>
  TOBDSignalGrid = class(TOBDCustomControl, IOBDSurface)
  strict private
    FSignals: TOBDSignalGridCollection;
    procedure SetSignals(AValue: TOBDSignalGridCollection);
    function UsePreviewData: Boolean;
    function VisualCount: Integer;
    function VisualName(AIndex: Integer): string;
    function VisualUnit(AIndex: Integer): string;
    function VisualValue(AIndex: Integer): Double;
    function VisualMin(AIndex: Integer): Double;
    function VisualMax(AIndex: Integer): Double;
    function HeaderHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function RowHeight: Integer;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawColumnHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter);
    procedure DrawSpark(APainter: TOBDPainter; const R: TRect;
      AIndex: Integer);
    procedure DrawEmpty(APainter: TOBDPainter);
    function FormatValue(AValue: Double): string;
  protected
    /// <summary>Paints the live signal grid.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Notifies the grid that its signal collection changed.</summary>
    procedure SignalsChanged;
    /// <summary>Creates an empty signal grid.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the signal collection.</summary>
    destructor Destroy; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes every signal row.</summary>
    procedure Clear;
    /// <summary>Adds a signal unless a row of the same name already exists.</summary>
    /// <param name="AName">Signal name.</param>
    /// <param name="AUnit">Unit text.</param>
    /// <returns>Row index.</returns>
    function AddSignal(const AName, AUnit: string): Integer;
    /// <summary>Pushes a value by row index.</summary>
    /// <param name="AIndex">Row index.</param>
    /// <param name="AValue">New value.</param>
    procedure PushValue(AIndex: Integer; AValue: Double); overload;
    /// <summary>Pushes a value by signal name, adding the row if needed.</summary>
    /// <param name="AName">Signal name.</param>
    /// <param name="AValue">New value.</param>
    /// <param name="AUnit">Unit text for new rows.</param>
    procedure PushValue(const AName: string; AValue: Double;
      const AUnit: string = ''); overload;
    /// <summary>Resets minimum and maximum values of every row.</summary>
    procedure ResetMinMax;
    /// <summary>Number of signal rows.</summary>
    /// <returns>Signal count.</returns>
    function Count: Integer;
  published
    /// <summary>Streamed live signal rows.</summary>
    property Signals: TOBDSignalGridCollection read FSignals write SetSignals;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
  end;

implementation

uses
  Winapi.GDIPAPI;

const
  PREVIEW_GRID_COUNT = 6;

function PreviewGridName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Boost pressure actual';
    1: Result := 'Boost pressure setpoint';
    2: Result := 'Air mass actual';
    3: Result := 'Main injection quantity';
    4: Result := 'Rail pressure actual';
  else
    Result := 'Start of injection';
  end;
end;

function PreviewGridUnit(AIndex: Integer): string;
begin
  case AIndex of
    0, 1: Result := 'hPa';
    2, 3: Result := 'mg/stroke';
    4: Result := 'bar';
  else
    Result := #$00B0 + 'CA';
  end;
end;

function PreviewGridValue(AIndex: Integer): Double;
begin
  case AIndex of
    0: Result := 1918;
    1: Result := 1950;
    2: Result := 742;
    3: Result := 38.6;
    4: Result := 1482;
  else
    Result := -2.4;
  end;
end;

{ TOBDSignalGridItem }

constructor TOBDSignalGridItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCapacity := 40;
  SetLength(FSamples, FCapacity);
  FValue := NaN;
  FMinValue := NaN;
  FMaxValue := NaN;
end;

procedure TOBDSignalGridItem.Assign(Source: TPersistent);
var
  Item: TOBDSignalGridItem;
  I: Integer;
begin
  if Source is TOBDSignalGridItem then
  begin
    Item := TOBDSignalGridItem(Source);
    FName := Item.Name;
    FUnitText := Item.UnitText;
    FValue := Item.Value;
    FMinValue := Item.MinValue;
    FMaxValue := Item.MaxValue;
    FCapacity := Item.Capacity;
    SetLength(FSamples, FCapacity);
    FSampleStart := 0;
    FSampleCount := 0;
    for I := 0 to Item.SampleCount - 1 do
      PushValue(Item.Sample(I));
    Changed(False);
  end
  else
    inherited;
end;

function TOBDSignalGridItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDSignalGridItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDSignalGridItem.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Changed(False);
end;

procedure TOBDSignalGridItem.SetCapacity(AValue: Integer);
var
  OldValues: array of Double;
  OldCount, I: Integer;
begin
  AValue := EnsureRange(AValue, 8, 240);
  if FCapacity = AValue then
    Exit;
  OldCount := FSampleCount;
  SetLength(OldValues, OldCount);
  for I := 0 to OldCount - 1 do
    OldValues[I] := Sample(I);
  FCapacity := AValue;
  SetLength(FSamples, FCapacity);
  FSampleStart := 0;
  FSampleCount := 0;
  for I := System.Math.Max(0, OldCount - FCapacity) to OldCount - 1 do
    PushValue(OldValues[I]);
  Changed(False);
end;

procedure TOBDSignalGridItem.SetValueInternal(AValue: Double);
begin
  FValue := AValue;
  if IsNan(FMinValue) or (AValue < FMinValue) then
    FMinValue := AValue;
  if IsNan(FMaxValue) or (AValue > FMaxValue) then
    FMaxValue := AValue;
end;

procedure TOBDSignalGridItem.PushValue(AValue: Double);
var
  P: Integer;
begin
  SetValueInternal(AValue);
  if FCapacity <= 0 then
    Exit;
  if FSampleCount < FCapacity then
  begin
    P := (FSampleStart + FSampleCount) mod FCapacity;
    Inc(FSampleCount);
  end
  else
  begin
    P := FSampleStart;
    FSampleStart := (FSampleStart + 1) mod FCapacity;
  end;
  FSamples[P] := AValue;
  Changed(False);
end;

procedure TOBDSignalGridItem.ResetMinMax;
begin
  FMinValue := NaN;
  FMaxValue := NaN;
  if not IsNan(FValue) then
  begin
    FMinValue := FValue;
    FMaxValue := FValue;
  end;
  Changed(False);
end;

procedure TOBDSignalGridItem.ClearSamples;
begin
  FSampleStart := 0;
  FSampleCount := 0;
  Changed(False);
end;

function TOBDSignalGridItem.Sample(AIndex: Integer): Double;
begin
  if (AIndex < 0) or (AIndex >= FSampleCount) or (FCapacity <= 0) then
    Exit(NaN);
  Result := FSamples[(FSampleStart + AIndex) mod FCapacity];
end;

{ TOBDSignalGridCollection }

constructor TOBDSignalGridCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDSignalGridItem);
end;

function TOBDSignalGridCollection.Add: TOBDSignalGridItem;
begin
  Result := TOBDSignalGridItem(inherited Add);
end;

procedure TOBDSignalGridCollection.Assign(Source: TPersistent);
var
  Src: TOBDSignalGridCollection;
  I: Integer;
begin
  if Source is TOBDSignalGridCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDSignalGridCollection(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    Update(nil);
  end
  else
    inherited;
end;

function TOBDSignalGridCollection.GetItem(Index: Integer): TOBDSignalGridItem;
begin
  Result := TOBDSignalGridItem(inherited GetItem(Index));
end;

procedure TOBDSignalGridCollection.SetItem(Index: Integer;
  AValue: TOBDSignalGridItem);
begin
  inherited SetItem(Index, AValue);
end;

function TOBDSignalGridCollection.IndexOfName(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SameText(Items[I].Name, AName) then
      Exit(I);
end;

procedure TOBDSignalGridCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDSignalGrid then
    TOBDSignalGrid(GetOwner).SignalsChanged;
end;

{ TOBDSignalGrid }

constructor TOBDSignalGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 640;
  Height := 360;
  FSignals := TOBDSignalGridCollection.Create(Self);
end;

destructor TOBDSignalGrid.Destroy;
begin
  FSignals.Free;
  inherited;
end;

function TOBDSignalGrid.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDSignalGrid.Clear;
begin
  FSignals.Clear;
  Invalidate;
end;

function TOBDSignalGrid.AddSignal(const AName, AUnit: string): Integer;
begin
  Result := FSignals.IndexOfName(AName);
  if Result >= 0 then
    Exit;
  Result := FSignals.Add.Index;
  FSignals[Result].Name := AName;
  FSignals[Result].UnitText := AUnit;
  Invalidate;
end;

procedure TOBDSignalGrid.PushValue(AIndex: Integer; AValue: Double);
begin
  if (AIndex < 0) or (AIndex >= FSignals.Count) then
    Exit;
  FSignals[AIndex].PushValue(AValue);
  Invalidate;
end;

procedure TOBDSignalGrid.PushValue(const AName: string; AValue: Double;
  const AUnit: string);
var
  I: Integer;
begin
  I := AddSignal(AName, AUnit);
  if (AUnit <> '') and (FSignals[I].UnitText = '') then
    FSignals[I].UnitText := AUnit;
  PushValue(I, AValue);
end;

procedure TOBDSignalGrid.ResetMinMax;
var
  I: Integer;
begin
  for I := 0 to FSignals.Count - 1 do
    FSignals[I].ResetMinMax;
  Invalidate;
end;

function TOBDSignalGrid.Count: Integer;
begin
  Result := FSignals.Count;
end;

procedure TOBDSignalGrid.SetSignals(AValue: TOBDSignalGridCollection);
begin
  FSignals.Assign(AValue);
end;

procedure TOBDSignalGrid.SignalsChanged;
begin
  Invalidate;
end;

function TOBDSignalGrid.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FSignals.Count = 0);
end;

function TOBDSignalGrid.VisualCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_GRID_COUNT
  else
    Result := FSignals.Count;
end;

function TOBDSignalGrid.VisualName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewGridName(AIndex)
  else
    Result := FSignals[AIndex].Name;
end;

function TOBDSignalGrid.VisualUnit(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewGridUnit(AIndex)
  else
    Result := FSignals[AIndex].UnitText;
end;

function TOBDSignalGrid.VisualValue(AIndex: Integer): Double;
begin
  if UsePreviewData then
    Result := PreviewGridValue(AIndex)
  else
    Result := FSignals[AIndex].Value;
end;

function TOBDSignalGrid.VisualMin(AIndex: Integer): Double;
begin
  if UsePreviewData then
    Result := PreviewGridValue(AIndex) * 0.75
  else
    Result := FSignals[AIndex].MinValue;
end;

function TOBDSignalGrid.VisualMax(AIndex: Integer): Double;
begin
  if UsePreviewData then
    Result := PreviewGridValue(AIndex) * 1.12
  else
    Result := FSignals[AIndex].MaxValue;
end;

function TOBDSignalGrid.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDSignalGrid.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDSignalGrid.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row - 8);
end;

function TOBDSignalGrid.FormatValue(AValue: Double): string;
begin
  if IsNan(AValue) then
    Result := '--'
  else if Abs(AValue) >= 100 then
    Result := FormatFloat('#,##0', AValue)
  else
    Result := FormatFloat('0.0#', AValue);
end;

procedure TOBDSignalGrid.DrawHeader(APainter: TOBDPainter);
var
  H, X, C: Integer;
begin
  H := HeaderHeight;
  APainter.Glyph(glScope, ScaleValue(26), H div 2, APainter.AccentText, 1);
  X := ScaleValue(48);
  APainter.Text(X, H div 2, 'Live measurement', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold);
  C := X + APainter.TextWidth('Live measurement', OBD_TEXT_TITLE, twBold) +
    ScaleValue(14);
  C := C + APainter.Chip(C, H div 2 - ScaleValue(10), 'DAQ 10 MS',
    Palette.Success) + ScaleValue(6);
  APainter.Chip(C, H div 2 - ScaleValue(10), 'DAQ 100 MS', Palette.Subtle);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDSignalGrid.DrawColumnHeader(APainter: TOBDPainter);
var
  Y, H: Integer;
begin
  Y := HeaderHeight;
  H := ColumnHeaderHeight;
  APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + H), APainter.HeaderFill);
  APainter.HLine(1, Y + H - 1, ClientWidth - 2, Palette.NeutralLight);
  APainter.Caps(ScaleValue(16), Y + H div 2, 'Signal');
  APainter.Caps(ScaleValue(320), Y + H div 2, 'Value', taRightJustify);
  APainter.Caps(ScaleValue(330), Y + H div 2, 'Unit');
  APainter.Caps(ScaleValue(440), Y + H div 2, 'Min / max');
  APainter.Caps(ScaleValue(580), Y + H div 2, 'Last 10 s');
end;

procedure TOBDSignalGrid.DrawSpark(APainter: TOBDPainter; const R: TRect;
  AIndex: Integer);
var
  Values: array of Double;
  Points: array of TGPPointF;
  I, N: Integer;
  MinV, MaxV, V, X, Y: Double;
begin
  APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(4),
    OBDMixColor(Palette.NeutralLight, Palette.GaugeFace, 0.35),
    Palette.NeutralLight);
  if UsePreviewData then
  begin
    N := 36;
    SetLength(Values, N);
    for I := 0 to N - 1 do
      Values[I] := PreviewGridValue(AIndex) + Sin((I + AIndex) / 4) *
        System.Math.Max(1, Abs(PreviewGridValue(AIndex)) * 0.06);
  end
  else
  begin
    N := FSignals[AIndex].SampleCount;
    SetLength(Values, N);
    for I := 0 to N - 1 do
      Values[I] := FSignals[AIndex].Sample(I);
  end;
  if N < 2 then
    Exit;
  MinV := Values[0];
  MaxV := Values[0];
  for I := 1 to N - 1 do
  begin
    MinV := System.Math.Min(MinV, Values[I]);
    MaxV := System.Math.Max(MaxV, Values[I]);
  end;
  if SameValue(MinV, MaxV) then
  begin
    MinV := MinV - 1;
    MaxV := MaxV + 1;
  end;
  SetLength(Points, N);
  for I := 0 to N - 1 do
  begin
    V := (Values[I] - MinV) / (MaxV - MinV);
    X := R.Left + (R.Width - 1) * I / (N - 1);
    Y := R.Bottom - ScaleValue(4) - V * (R.Height - ScaleValue(8));
    Points[I] := OBDPointF(X, Y);
  end;
  APainter.Lines(Points, APainter.AccentText, 2);
end;

procedure TOBDSignalGrid.DrawRows(APainter: TOBDPainter);
var
  I, Y, Mid, SparkLeft: Integer;
  S: string;
  MM: string;
  V: Double;
begin
  if VisualCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  Y := HeaderHeight + ColumnHeaderHeight;
  SparkLeft := ScaleValue(580);
  for I := 0 to VisualCount - 1 do
  begin
    if Y + RowHeight > ClientHeight - ScaleValue(8) then
      Break;
    Mid := Y + RowHeight div 2;
    APainter.Text(ScaleValue(16), Mid, VisualName(I), OBD_TEXT_BODY,
      Palette.ForegroundText, twRegular, taLeftJustify, ScaleValue(260));
    V := VisualValue(I);
    APainter.Text(ScaleValue(320), Mid, FormatValue(V), OBD_TEXT_HEADING,
      Palette.ForegroundText, twBold, taRightJustify, ScaleValue(100));
    APainter.Text(ScaleValue(330), Mid, VisualUnit(I), OBD_TEXT_SMALL,
      Palette.GaugeLabel);
    S := FormatValue(VisualMin(I));
    MM := S + ' / ' + FormatValue(VisualMax(I));
    APainter.Text(ScaleValue(440), Mid, MM, OBD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(120));
    DrawSpark(APainter, Rect(SparkLeft, Y + ScaleValue(8),
      ClientWidth - ScaleValue(16), Y + RowHeight - ScaleValue(8)), I);
    Y := Y + RowHeight;
    APainter.HLine(1, Y - 1, ClientWidth - 2, Palette.NeutralLight);
  end;
end;

procedure TOBDSignalGrid.DrawEmpty(APainter: TOBDPainter);
begin
  APainter.Text(ClientWidth div 2,
    HeaderHeight + ColumnHeaderHeight + (ClientHeight - HeaderHeight -
    ColumnHeaderHeight) div 2, 'No live signals', OBD_TEXT_HEADING,
    Palette.GaugeLabel, twSemibold, taCenter);
end;

procedure TOBDSignalGrid.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    P.Card(ClientRect);
    DrawHeader(P);
    DrawColumnHeader(P);
    DrawRows(P);
  finally
    P.Free;
  end;
end;

end.
