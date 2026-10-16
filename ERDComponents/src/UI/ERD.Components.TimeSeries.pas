unit ERD.Components.TimeSeries;
{$B-}
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Types, System.Math, System.Generics.Collections, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types;
type
  TERDTimeSample = record
    TimeSeconds, Value: Double;
    Available: Boolean;
  end;
  TERDTimeChartStyle = (tcsLines, tcsBars, tcsStackedBars);
  TERDTimeBar = record
    SeriesIndex, SampleIndex: Integer;
    Sample: TERDTimeSample;
    BaseValue: Double;
    Bounds: TRectF;
  end;
  TERDTimeSeriesChart = class;
  TERDTimeSeries = class(TCollectionItem)
  private
    FCaption: string;
    FColor: TColor;
    FBuffer: TArray<TERDTimeSample>;
    FHead, FCount: Integer;
    procedure SetCaption(const Value: string);
    procedure SetColor(Value: TColor);
    function Capacity: Integer;
    procedure ResizeBuffer;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure AddSample(TimeSeconds, Value: Double; Available: Boolean = True);
    procedure Clear;
    function Sample(Index: Integer): TERDTimeSample;
    property Count: Integer read FCount;
  published
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clDefault;
  end;
  TERDTimeSeriesCollection = class(TOwnedCollection)
  private
    FChart: TERDTimeSeriesChart;
    function GetItem(Index: Integer): TERDTimeSeries;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TERDTimeSeriesChart);
    function Add: TERDTimeSeries;
    property Items[Index: Integer]: TERDTimeSeries read GetItem; default;
  end;
  TERDTimeSampleEvent = procedure(Sender: TObject; SeriesIndex, SampleIndex: Integer; const Sample: TERDTimeSample) of object;
  /// Timestamped scalar series sharing one unit and Y-axis. No polling or transport.
  TERDTimeSeriesChart = class(TERDCustomControl)
  private
    FSeries: TERDTimeSeriesCollection;
    FMaxSamples: Integer;
    FWindowSeconds, FPausedEnd: Double;
    FPaused: Boolean;
    FUnitText: string;
    FChartStyle: TERDTimeChartStyle;
    FOnSampleHover: TERDTimeSampleEvent;
    procedure SetChartStyle(Value: TERDTimeChartStyle);
    procedure PaintBars(ACanvas: TCanvas);
    function BuildBars(out MinValue, MaxValue: Double): TArray<TERDTimeBar>;
    function SeriesColor(Index: Integer): TColor;
    procedure SetSeries(Value: TERDTimeSeriesCollection);
    procedure SetMaxSamples(Value: Integer);
    procedure SetWindowSeconds(Value: Double);
    procedure SetPaused(Value: Boolean);
    procedure SetUnitText(const Value: string);
    function PlotBounds: TRect;
    function LatestTime: Double;
    function VisibleEnd: Double;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function BarGeometry: TArray<TERDTimeBar>;
    function BarAt(X, Y: Integer; out Bar: TERDTimeBar): Boolean;
    function SampleNear(X: Integer; out SeriesIndex, SampleIndex: Integer; out Sample: TERDTimeSample): Boolean;
  published
    property ChartStyle: TERDTimeChartStyle read FChartStyle write SetChartStyle default tcsLines;
    property Series: TERDTimeSeriesCollection read FSeries write SetSeries;
    property MaxSamples: Integer read FMaxSamples write SetMaxSamples default 2048;
    property WindowSeconds: Double read FWindowSeconds write SetWindowSeconds;
    property Paused: Boolean read FPaused write SetPaused default False;
    property UnitText: string read FUnitText write SetUnitText;
    property OnSampleHover: TERDTimeSampleEvent read FOnSampleHover write FOnSampleHover;
  end;
implementation
uses ERD.Components.Paint;
constructor TERDTimeSeries.Create(Collection: TCollection);
begin
  inherited;
  FColor := clDefault;
end;
function TERDTimeSeries.Capacity: Integer;
begin Result := TERDTimeSeriesCollection(Collection).FChart.MaxSamples; end;
procedure TERDTimeSeries.ResizeBuffer;
var Values: TArray<TERDTimeSample>; I, Keep: Integer;
begin
  if Length(FBuffer) = Capacity then Exit;
  Keep := Min(FCount,Capacity); SetLength(Values,Capacity);
  for I := 0 to Keep-1 do Values[I] := Sample(FCount-Keep+I);
  FBuffer := Values; FCount := Keep; FHead := Keep mod Length(FBuffer);
end;
procedure TERDTimeSeries.AddSample(TimeSeconds, Value: Double; Available: Boolean);
begin
  if IsNan(TimeSeconds) or IsInfinite(TimeSeconds) or (Available and (IsNan(Value) or IsInfinite(Value))) then
    raise EArgumentException.Create('Samples need finite timestamps and available values');
  if (FCount > 0) and (TimeSeconds <= Sample(FCount-1).TimeSeconds) then raise EArgumentException.Create('Sample timestamps must increase');
  ResizeBuffer; FBuffer[FHead].TimeSeconds := TimeSeconds; FBuffer[FHead].Available := Available;
  if Available then FBuffer[FHead].Value := Value else FBuffer[FHead].Value := 0;
  FHead := (FHead+1) mod Length(FBuffer); FCount := Min(FCount+1,Length(FBuffer)); Changed(False);
end;
function TERDTimeSeries.Sample(Index: Integer): TERDTimeSample;
begin
  if (Index < 0) or (Index >= FCount) then raise EArgumentOutOfRangeException.Create('Unknown sample');
  Result := FBuffer[(FHead-FCount+Length(FBuffer)+Index) mod Length(FBuffer)];
end;
procedure TERDTimeSeries.Clear;
begin FCount := 0; FHead := 0; Changed(False); end;
procedure TERDTimeSeries.SetCaption(const Value: string);
begin if FCaption = Value then Exit; FCaption := Value; Changed(False); end;
procedure TERDTimeSeries.SetColor(Value: TColor);
begin if FColor = Value then Exit; FColor := Value; Changed(False); end;
procedure TERDTimeSeries.Assign(Source: TPersistent);
var Series: TERDTimeSeries;
begin
  if Source is TERDTimeSeries then begin Series := TERDTimeSeries(Source); FCaption := Series.Caption; FColor := Series.Color; Clear; end
  else inherited;
end;
constructor TERDTimeSeriesCollection.Create(Owner: TERDTimeSeriesChart);
begin FChart := Owner; inherited Create(Owner,TERDTimeSeries); end;
function TERDTimeSeriesCollection.Add: TERDTimeSeries;
begin Result := TERDTimeSeries(inherited Add); end;
function TERDTimeSeriesCollection.GetItem(Index: Integer): TERDTimeSeries;
begin Result := TERDTimeSeries(inherited Items[Index]); end;
procedure TERDTimeSeriesCollection.Update(Item: TCollectionItem);
begin inherited; if GetOwner is TERDTimeSeriesChart then TERDTimeSeriesChart(GetOwner).Invalidate; end;
constructor TERDTimeSeriesChart.Create(AOwner: TComponent);
begin
  inherited;
  FMaxSamples := 2048;
  FWindowSeconds := 60;
  FSeries := TERDTimeSeriesCollection.Create(Self); Width := 500; Height := 220; ShowHint := True;
end;
destructor TERDTimeSeriesChart.Destroy;
begin FSeries.Free; inherited; end;
procedure TERDTimeSeriesChart.SetChartStyle(Value: TERDTimeChartStyle);
begin
  if (Ord(Value) < 0) or (Ord(Value) > Ord(High(TERDTimeChartStyle))) then raise EArgumentException.Create('Invalid chart style');
  if FChartStyle = Value then Exit; FChartStyle := Value; Invalidate;
end;
function TERDTimeSeriesChart.SeriesColor(Index: Integer): TColor;
begin
  Result := FSeries[Index].Color;
  if Result = clDefault then case Index mod 4 of 0: Result := Palette.Accent; 1: Result := Palette.Success; 2: Result := Palette.Warning; else Result := Palette.Danger; end;
end;
function TERDTimeSeriesChart.BuildBars(out MinValue, MaxValue: Double): TArray<TERDTimeBar>;
var Bars: TList<TERDTimeBar>; R: TRect; I, J, K, Start: Integer; Bar: TERDTimeBar;
    Value, Other: TERDTimeSample; Finish, BeginTime, Gap, Width, X, Positive, Negative, TopValue, Scale: Double;
    Valid: Boolean;
  function FindAt(SeriesIndex: Integer; Time: Double; out Index: Integer; out Sample: TERDTimeSample): Boolean;
  var L, H, M: Integer;
  begin
    L := 0; H := FSeries[SeriesIndex].Count-1; Index := -1; Sample := Default(TERDTimeSample);
    while L <= H do
    begin
      M := L+(H-L) div 2; Sample := FSeries[SeriesIndex].Sample(M);
      if Sample.TimeSeconds = Time then begin Index := M; Exit(Sample.Available); end;
      if Sample.TimeSeconds < Time then L := M+1 else H := M-1;
    end;
    Result := False;
  end;
  function BarWidth(SeriesIndex, SampleIndex: Integer): Double;
  begin
    Gap := FWindowSeconds/10;
    if (SampleIndex > 0) and (FSeries[SeriesIndex].Sample(SampleIndex-1).TimeSeconds > Value.TimeSeconds-Gap) then Gap := Value.TimeSeconds-FSeries[SeriesIndex].Sample(SampleIndex-1).TimeSeconds;
    if (SampleIndex+1 < FSeries[SeriesIndex].Count) and (FSeries[SeriesIndex].Sample(SampleIndex+1).TimeSeconds < Value.TimeSeconds+Gap) then Gap := FSeries[SeriesIndex].Sample(SampleIndex+1).TimeSeconds-Value.TimeSeconds;
    Result := Max(1,Min(ScaleValue(32),Gap/FWindowSeconds*R.Width*0.75));
  end;
  function PixelY(Number: Double): Single;
  begin Result := R.Bottom-((Number/Scale-MinValue/Scale)/(MaxValue/Scale-MinValue/Scale))*R.Height; end;
begin
  Result := nil; MinValue := 0; MaxValue := 0;
  if (FChartStyle = tcsLines) or (FSeries.Count = 0) then Exit;
  R := PlotBounds; Finish := VisibleEnd; BeginTime := Finish-FWindowSeconds; Bars := TList<TERDTimeBar>.Create;
  try
    for I := 0 to FSeries.Count-1 do
    begin
      if (FChartStyle = tcsStackedBars) and (I > 0) then Break;
      for J := 0 to FSeries[I].Count-1 do
      begin
        Value := FSeries[I].Sample(J);
        if not Value.Available or (Value.TimeSeconds < BeginTime) or (Value.TimeSeconds > Finish) then Continue;
        X := R.Left+(Value.TimeSeconds-BeginTime)/FWindowSeconds*R.Width; Width := Min(R.Width,BarWidth(I,J));
        X := EnsureRange(X,R.Left+Width/2,R.Right-Width/2);
        if FChartStyle = tcsStackedBars then
        begin
          Start := Bars.Count; Positive := 0; Negative := 0; Valid := True;
          for K := 0 to FSeries.Count-1 do
          begin
            if not FindAt(K,Value.TimeSeconds,Bar.SampleIndex,Other) then begin Valid := False; Break; end;
            Bar.Sample := Other; Bar.SeriesIndex := K;
            if Other.Value >= 0 then
            begin
              if Positive > MaxDouble-Other.Value then begin Valid := False; Break; end;
              Bar.BaseValue := Positive; Positive := Positive+Other.Value;
            end
            else
            begin
              if Negative < -MaxDouble-Other.Value then begin Valid := False; Break; end;
              Bar.BaseValue := Negative; Negative := Negative+Other.Value;
            end;
            if IsInfinite(Positive) or IsInfinite(Negative) then begin Valid := False; Break; end;
            Bar.Bounds := System.Types.RectF(X-Width/2,0,X+Width/2,0); Bars.Add(Bar);
          end;
          // A partial stack must not imply a known total. A missing segment leaves a gap.
          if not Valid then while Bars.Count > Start do Bars.Delete(Bars.Count-1);
        end
        else
        begin
          Bar.SeriesIndex := I; Bar.SampleIndex := J; Bar.Sample := Value; Bar.BaseValue := 0;
          Bar.Bounds := System.Types.RectF(X-Width/2+Width*I/FSeries.Count,0,X-Width/2+Width*(I+1)/FSeries.Count,0); Bars.Add(Bar);
        end;
      end;
    end;
    Result := Bars.ToArray;
  finally Bars.Free; end;
  for I := 0 to High(Result) do
  begin
    TopValue := Result[I].BaseValue+Result[I].Sample.Value;
    MinValue := Min(MinValue,Min(Result[I].BaseValue,TopValue)); MaxValue := Max(MaxValue,Max(Result[I].BaseValue,TopValue));
  end;
  if MinValue = MaxValue then MaxValue := MinValue+1;
  // Normalize before subtraction so finite extremes cannot overflow the axis range.
  Scale := Max(1,Max(Abs(MinValue),Abs(MaxValue)));
  for I := 0 to High(Result) do
  begin
    TopValue := Result[I].BaseValue+Result[I].Sample.Value;
    Result[I].Bounds.Top := PixelY(Max(Result[I].BaseValue,TopValue)); Result[I].Bounds.Bottom := PixelY(Min(Result[I].BaseValue,TopValue));
    if Result[I].Bounds.Height < 1 then
    begin
      Result[I].Bounds.Top := EnsureRange(Result[I].Bounds.Bottom-1,Single(R.Top),Single(R.Bottom-1));
      Result[I].Bounds.Bottom := Result[I].Bounds.Top+1;
    end;
    Result[I].Bounds.Left := Max(R.Left,Result[I].Bounds.Left); Result[I].Bounds.Right := Min(R.Right,Result[I].Bounds.Right);
    Result[I].Bounds.Top := Max(R.Top,Result[I].Bounds.Top); Result[I].Bounds.Bottom := Min(R.Bottom,Result[I].Bounds.Bottom);
  end;
end;
function TERDTimeSeriesChart.BarGeometry: TArray<TERDTimeBar>;
var AxisMin, AxisMax: Double;
begin Result := BuildBars(AxisMin,AxisMax); end;
function TERDTimeSeriesChart.BarAt(X, Y: Integer; out Bar: TERDTimeBar): Boolean;
var Bars: TArray<TERDTimeBar>; I: Integer;
begin
  Bar := Default(TERDTimeBar); Result := False; if not PtInRect(PlotBounds,Point(X,Y)) then Exit;
  Bars := BarGeometry;
  for I := High(Bars) downto 0 do if (X >= Bars[I].Bounds.Left) and (X <= Bars[I].Bounds.Right) and
    (Y >= Bars[I].Bounds.Top) and (Y <= Bars[I].Bounds.Bottom) then begin Bar := Bars[I]; Exit(True); end;
end;
procedure TERDTimeSeriesChart.PaintBars(ACanvas: TCanvas);
var Painter: TERDPainter; Bars: TArray<TERDTimeBar>; Bar: TERDTimeBar; R: TRect;
    AxisMin, AxisMax, Ratio: Double; I, LegendX: Integer; Y: Single;
begin
  Bars := BuildBars(AxisMin,AxisMax); R := PlotBounds; Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    for I := 0 to 4 do
    begin
      Ratio := I/4; Y := R.Top+R.Height*Ratio;
      Painter.LineF(R.Left,Y,R.Right,Y,ERDMixColor(Palette.ForegroundText,Palette.Background,0.12));
      Painter.TextF(R.Left-ScaleValue(7),Y,FloatToStrF(AxisMax*(1-Ratio)+AxisMin*Ratio,ffGeneral,5,2),10,Palette.Subtle,twRegular,taRightJustify);
    end;
    Painter.TextF(R.Left,R.Bottom+ScaleValue(14),'-'+FloatToStrF(FWindowSeconds,ffGeneral,5,2)+' s',10,Palette.Subtle);
    Painter.TextF(R.Right,R.Bottom+ScaleValue(14),'0 s',10,Palette.Subtle,twRegular,taRightJustify);
    LegendX := R.Left;
    for I := 0 to FSeries.Count-1 do begin Painter.TextF(LegendX,ScaleValue(14),FSeries[I].Caption,11,SeriesColor(I)); Inc(LegendX,ScaleValue(130)); end;
    Painter.ClipRectF(R.Left,R.Top,R.Width,R.Height);
    try for Bar in Bars do if (Bar.Bounds.Width > 0) and (Bar.Bounds.Height > 0) then Painter.RectangleF(Bar.Bounds.Left,Bar.Bounds.Top,Bar.Bounds.Width,Bar.Bounds.Height,SeriesColor(Bar.SeriesIndex),clNone);
    finally Painter.ResetClip; end;
    if Length(Bars) = 0 then Painter.TextF((R.Left+R.Right)/2,(R.Top+R.Bottom)/2,'No measurements',12,Palette.Subtle,twRegular,taCenter);
    Painter.TextF(Width-ScaleValue(12),ScaleValue(14),FUnitText,10,Palette.Subtle,twRegular,taRightJustify);
  finally Painter.Free; end;
end;
procedure TERDTimeSeriesChart.SetSeries(Value: TERDTimeSeriesCollection);
begin FSeries.Assign(Value); end;
procedure TERDTimeSeriesChart.SetMaxSamples(Value: Integer);
var I: Integer;
begin
  if (Value < 16) or (Value > 100000) then raise EArgumentException.Create('MaxSamples must be 16..100000');
  FMaxSamples := Value; for I := 0 to FSeries.Count-1 do FSeries[I].ResizeBuffer; Invalidate;
end;
procedure TERDTimeSeriesChart.SetWindowSeconds(Value: Double);
begin
  if IsNan(Value) or IsInfinite(Value) or (Value < 0.01) or (Value > 86400) then raise EArgumentException.Create('WindowSeconds must be 0.01..86400');
  FWindowSeconds := Value; Invalidate;
end;
procedure TERDTimeSeriesChart.SetUnitText(const Value: string);
begin FUnitText := Value; Invalidate; end;
procedure TERDTimeSeriesChart.SetPaused(Value: Boolean);
begin if FPaused = Value then Exit; if Value then FPausedEnd := LatestTime; FPaused := Value; Invalidate; end;
function TERDTimeSeriesChart.LatestTime: Double;
var I: Integer; Found: Boolean;
begin
  Result := 0; Found := False;
  for I := 0 to FSeries.Count-1 do if FSeries[I].Count > 0 then
    if not Found or (FSeries[I].Sample(FSeries[I].Count-1).TimeSeconds > Result) then
    begin Result := FSeries[I].Sample(FSeries[I].Count-1).TimeSeconds; Found := True; end;
end;
function TERDTimeSeriesChart.VisibleEnd: Double;
begin if FPaused then Result := FPausedEnd else Result := LatestTime; end;
function TERDTimeSeriesChart.PlotBounds: TRect;
begin Result := Rect(ScaleValue(64),ScaleValue(32),Max(ScaleValue(65),Width-ScaleValue(12)),Max(ScaleValue(33),Height-ScaleValue(28))); end;
procedure TERDTimeSeriesChart.Clear;
var I: Integer;
begin FSeries.BeginUpdate; try for I := 0 to FSeries.Count-1 do FSeries[I].Clear; finally FSeries.EndUpdate; end; FPausedEnd := 0; end;
function TERDTimeSeriesChart.SampleNear(X: Integer; out SeriesIndex, SampleIndex: Integer; out Sample: TERDTimeSample): Boolean;
var I, J: Integer; R: TRect; Wanted, Best, Distance, Finish: Double; Value: TERDTimeSample;
begin
  Result := False; Sample := Default(TERDTimeSample); SeriesIndex := -1; SampleIndex := -1;
  R := PlotBounds; if (X < R.Left) or (X > R.Right) then Exit;
  Finish := VisibleEnd; Wanted := Finish-FWindowSeconds+(X-R.Left)/Max(1,R.Width)*FWindowSeconds; Best := MaxDouble;
  for I := 0 to FSeries.Count-1 do for J := 0 to FSeries[I].Count-1 do
  begin
    Value := FSeries[I].Sample(J); if (Value.TimeSeconds < Finish-FWindowSeconds) or (Value.TimeSeconds > Finish) then Continue;
    Distance := Abs(Value.TimeSeconds-Wanted);
    if Distance < Best then begin Best := Distance; Sample := Value; SeriesIndex := I; SampleIndex := J; Result := True; end;
  end;
end;
procedure TERDTimeSeriesChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var I, J: Integer; Value: TERDTimeSample; Bar: TERDTimeBar; Found: Boolean;
begin
  inherited;
  if FChartStyle = tcsLines then Found := PtInRect(PlotBounds,Point(X,Y)) and SampleNear(X,I,J,Value)
  else begin Found := BarAt(X,Y,Bar); if Found then begin I := Bar.SeriesIndex; J := Bar.SampleIndex; Value := Bar.Sample; end; end;
  if Found then
  begin
    if Value.Available then Hint := FSeries[I].Caption+': '+FloatToStrF(Value.Value,ffGeneral,15,2)+' '+FUnitText+' | t='+FloatToStrF(Value.TimeSeconds,ffGeneral,15,3)+' s'
    else Hint := FSeries[I].Caption+': unavailable';
    if Assigned(FOnSampleHover) then FOnSampleHover(Self,I,J,Value);
  end else Hint := '';
end;
function TERDTimeSeriesChart.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if ssCtrl in Shift then begin if WheelDelta > 0 then WindowSeconds := Max(0.01,FWindowSeconds*0.8) else WindowSeconds := Min(86400,FWindowSeconds/0.8); Exit(True); end;
  Result := inherited DoMouseWheel(Shift,WheelDelta,MousePos);
end;
procedure TERDTimeSeriesChart.PaintControl(ACanvas: TCanvas);
type TBucket = record Used, Gap: Boolean; LowY, HighY, FirstY, LastY: Single; end;
var P: TERDPainter; R: TRect; I, J, X, LastX, LegendX: Integer; MinValue, MaxValue, Finish, BeginTime, Margin: Double;
    Value: TERDTimeSample; Ink: TColor; Y, LastY: Single; Buckets: TArray<TBucket>; Previous, Any: Boolean;
begin
  if FChartStyle <> tcsLines then begin PaintBars(ACanvas); Exit; end;
  P := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try
    R := PlotBounds; Finish := VisibleEnd; BeginTime := Finish-FWindowSeconds; MinValue := MaxDouble; MaxValue := -MaxDouble; Any := False;
    for I := 0 to FSeries.Count-1 do for J := 0 to FSeries[I].Count-1 do
    begin
      Value := FSeries[I].Sample(J);
      if Value.Available and (Value.TimeSeconds >= BeginTime) and (Value.TimeSeconds <= Finish) then
      begin MinValue := Min(MinValue,Value.Value); MaxValue := Max(MaxValue,Value.Value); Any := True; end;
    end;
    if not Any then begin MinValue := 0; MaxValue := 1; end;
    Margin := Max(0.001,Abs(MaxValue-MinValue)*0.08); if MaxValue = MinValue then Margin := Max(1,Abs(MaxValue)*0.1);
    MinValue := MinValue-Margin; MaxValue := MaxValue+Margin;
    for I := 0 to 4 do
    begin
      Y := R.Top+R.Height*I/4; P.LineF(R.Left,Y,R.Right,Y,ERDMixColor(Palette.ForegroundText,Palette.Background,0.12));
      P.TextF(R.Left-ScaleValue(7),Y,FloatToStrF(MaxValue-(MaxValue-MinValue)*I/4,ffGeneral,5,2),10,Palette.Subtle,twRegular,taRightJustify);
    end;
    P.TextF(R.Left,R.Bottom+ScaleValue(14),'-'+FloatToStrF(FWindowSeconds,ffGeneral,5,2)+' s',10,Palette.Subtle);
    P.TextF(R.Right,R.Bottom+ScaleValue(14),'0 s',10,Palette.Subtle,twRegular,taRightJustify);
    LegendX := R.Left;
    for I := 0 to FSeries.Count-1 do
    begin
      Ink := FSeries[I].Color;
      if Ink = clDefault then case I mod 4 of 0: Ink := Palette.Accent; 1: Ink := Palette.Success; 2: Ink := Palette.Warning; else Ink := Palette.Danger; end;
      P.TextF(LegendX,ScaleValue(14),FSeries[I].Caption,11,Ink); Inc(LegendX,ScaleValue(130));
      Buckets := nil; SetLength(Buckets,R.Width+1);
      for J := 0 to FSeries[I].Count-1 do
      begin
        Value := FSeries[I].Sample(J); if (Value.TimeSeconds < BeginTime) or (Value.TimeSeconds > Finish) then Continue;
        X := EnsureRange(Round((Value.TimeSeconds-BeginTime)/FWindowSeconds*R.Width),0,R.Width);
        if not Value.Available then begin Buckets[X].Gap := True; Continue; end;
        Y := R.Bottom-(Value.Value-MinValue)/(MaxValue-MinValue)*R.Height;
        if not Buckets[X].Used then begin Buckets[X].LowY := Y; Buckets[X].HighY := Y; Buckets[X].FirstY := Y; end;
        Buckets[X].Used := True; Buckets[X].LowY := Min(Buckets[X].LowY,Y); Buckets[X].HighY := Max(Buckets[X].HighY,Y); Buckets[X].LastY := Y;
      end;
      P.ClipRectF(R.Left,R.Top,R.Width+1,R.Height+1);
      try
        Previous := False; LastX := 0; LastY := 0;
        for X := 0 to High(Buckets) do
        begin
          if Buckets[X].Gap then begin Previous := False; Continue; end;
          if not Buckets[X].Used then Continue;
          if Previous then P.LineF(R.Left+LastX,LastY,R.Left+X,Buckets[X].FirstY,Ink,ScaleValue(2));
          if Buckets[X].LowY = Buckets[X].HighY then
            P.Ellipse(R.Left+X-ScaleValue(1),Buckets[X].LowY-ScaleValue(1),ScaleValue(2),ScaleValue(2),Ink,clNone)
          else P.LineF(R.Left+X,Buckets[X].LowY,R.Left+X,Buckets[X].HighY,Ink,ScaleValue(2));
          LastX := X; LastY := Buckets[X].LastY; Previous := not Buckets[X].Gap;
        end;
      finally P.ResetClip; end;
    end;
    if not Any then P.TextF((R.Left+R.Right)/2,(R.Top+R.Bottom)/2,'No measurements',12,Palette.Subtle,twRegular,taCenter);
    P.TextF(Width-ScaleValue(12),ScaleValue(14),FUnitText,10,Palette.Subtle,twRegular,taRightJustify);
  finally P.Free; end;
end;
end.
