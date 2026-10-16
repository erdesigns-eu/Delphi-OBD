unit ERD.Components.TimeSeries;
interface
uses Winapi.Windows, System.Classes, System.SysUtils, System.Types, System.Math, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types;
type
  TERDTimeSample = record
    TimeSeconds, Value: Double;
    Available: Boolean;
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
    FOnSampleHover: TERDTimeSampleEvent;
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
    function SampleNear(X: Integer; out SeriesIndex, SampleIndex: Integer; out Sample: TERDTimeSample): Boolean;
  published
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
var I, J: Integer; Value: TERDTimeSample;
begin
  inherited;
  if PtInRect(PlotBounds,Point(X,Y)) and SampleNear(X,I,J,Value) then
  begin
    if Value.Available then Hint := FSeries[I].Caption+': '+FloatToStrF(Value.Value,ffGeneral,8,2)+' '+FUnitText
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
