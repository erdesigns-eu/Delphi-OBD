//------------------------------------------------------------------------------
//  OBD.UI.Charts
//
//  Chart visuals for the A2 inventory:
//
//    TOBDStripChart       Single-trace rolling chart. Lightweight.
//    TOBDLiveGridChart    N-trace rolling chart with per-trace
//                         colour / scale.
//    TOBDDynoChart        Dual-axis rolling chart (HP + torque)
//                         with optional redline marker.
//    TOBDPowerCurveGraph  Static XY plot of HP/torque vs RPM —
//                         the end-of-run shape from a
//                         TOBDPowerCurve sweep.
//
//  Every chart inherits theme / HiDPI / VCL-Style awareness
//  from TOBDCustomControl and routes state mutations through
//  TBindings.Notify.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Charts;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.UI.Dyno;

type
  /// <summary>Single-trace rolling chart. Host pushes one
  /// sample per update; the chart auto-fits its Y axis.
  /// </summary>
  TOBDStripChart = class(TOBDCustomControl)
  strict private
    FBuffer:   TArray<Double>;
    FCapacity: Integer;
    FCount:    Integer;
    FNext:     Integer;
    FAutoFit:  Boolean;
    FMinY:     Double;
    FMaxY:     Double;
    FLineColor: TColor;
    procedure SetCapacity(AValue: Integer);
    procedure SetAutoFit(AValue: Boolean);
    procedure SetMinY(AValue: Double);
    procedure SetMaxY(AValue: Double);
    procedure SetLineColor(AValue: TColor);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PushSample(AValue: Double);
    procedure Reset;
    function  SampleCount: Integer;
  published
    property Capacity: Integer
      read FCapacity write SetCapacity default 256;
    property AutoFit: Boolean
      read FAutoFit write SetAutoFit default True;
    property MinY: Double read FMinY write SetMinY;
    property MaxY: Double read FMaxY write SetMaxY;
    property LineColor: TColor
      read FLineColor write SetLineColor default clDefault;
  end;

  /// <summary>One trace definition for
  /// <see cref="TOBDLiveGridChart"/>.</summary>
  TOBDLiveTrace = class
  strict private
    FName:     string;
    FColor:    TColor;
    FUnits:    string;
    FMin:      Double;
    FMax:      Double;
    FSamples:  TArray<Double>;
    FCount:    Integer;
    FNext:     Integer;
    FCapacity: Integer;
  public
    constructor Create(const AName: string; AColor: TColor;
      ACapacity: Integer);
    procedure Push(AValue: Double);
    procedure Reset;
    function  Sample(AIndex: Integer): Double;
    property Name:    string  read FName  write FName;
    property Color:   TColor  read FColor write FColor;
    property Units:   string  read FUnits write FUnits;
    property MinY:    Double  read FMin   write FMin;
    property MaxY:    Double  read FMax   write FMax;
    property Count:   Integer read FCount;
    property Capacity: Integer read FCapacity;
  end;

  /// <summary>Multi-trace rolling chart. Host adds traces by
  /// name + colour, pushes samples per trace, the chart
  /// renders each as a polyline using its own scale.</summary>
  TOBDLiveGridChart = class(TOBDCustomControl)
  strict private
    FTraces:   TObjectList<TOBDLiveTrace>;
    FCapacity: Integer;
    FShowLegend: Boolean;
    FCaptionFont: TFont;
    procedure SetCapacity(AValue: Integer);
    procedure SetShowLegend(AValue: Boolean);
    procedure SetCaptionFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function AddTrace(const AName: string; AColor: TColor;
      const AUnits: string = ''; AMinY: Double = 0;
      AMaxY: Double = 100): TOBDLiveTrace;
    procedure PushSample(const ATraceName: string; AValue: Double);
    procedure ResetAll;
    function  TraceCount: Integer;
    function  Trace(AIndex: Integer): TOBDLiveTrace;
  published
    property Capacity: Integer
      read FCapacity write SetCapacity default 256;
    property ShowLegend: Boolean
      read FShowLegend write SetShowLegend default True;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>Dual-axis rolling HP + torque chart. Host
  /// pushes (RPM, HP, torque) tuples; the chart maintains a
  /// ring of the last <c>Capacity</c> samples and renders two
  /// polylines with independent Y axes.</summary>
  TOBDDynoChart = class(TOBDCustomControl)
  strict private
    FCapacity:    Integer;
    FCount:       Integer;
    FRPM:         TArray<Double>;
    FHP:          TArray<Double>;
    FTorque:      TArray<Double>;
    FMaxHP:       Double;
    FMaxTorque:   Double;
    FRedlineRPM:  Double;
    FHPColor:     TColor;
    FTorqueColor: TColor;
    procedure SetCapacity(AValue: Integer);
    procedure SetMaxHP(AValue: Double);
    procedure SetMaxTorque(AValue: Double);
    procedure SetRedlineRPM(AValue: Double);
    procedure SetHPColor(AValue: TColor);
    procedure SetTorqueColor(AValue: TColor);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PushSample(ARPM, AHP, ATorqueNm: Double);
    procedure Reset;
    function  SampleCount: Integer;
  published
    property Capacity: Integer
      read FCapacity write SetCapacity default 360;
    property MaxHP: Double read FMaxHP write SetMaxHP;
    property MaxTorqueNm: Double
      read FMaxTorque write SetMaxTorque;
    property RedlineRPM: Double
      read FRedlineRPM write SetRedlineRPM;
    property HPColor: TColor
      read FHPColor write SetHPColor default clDefault;
    property TorqueColor: TColor
      read FTorqueColor write SetTorqueColor default clDefault;
  end;

  /// <summary>Static XY plot of HP/torque vs RPM from a
  /// captured <see cref="TOBDPowerCurve"/>. Host calls
  /// <see cref="LoadCurve"/> after a sweep completes.
  /// </summary>
  TOBDPowerCurveGraph = class(TOBDCustomControl)
  strict private
    FCurve:       TArray<TOBDDynoPoint>;
    FHPColor:     TColor;
    FTorqueColor: TColor;
    procedure SetHPColor(AValue: TColor);
    procedure SetTorqueColor(AValue: TColor);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadCurve(const APoints: TArray<TOBDDynoPoint>);
    procedure Clear;
    function  PointCount: Integer;
  published
    property HPColor: TColor
      read FHPColor write SetHPColor default clDefault;
    property TorqueColor: TColor
      read FTorqueColor write SetTorqueColor default clDefault;
  end;

implementation

{ ---- TOBDStripChart ----------------------------------------------------- }

constructor TOBDStripChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 240;
  Height := 80;
  FCapacity := 256;
  SetLength(FBuffer, FCapacity);
  FAutoFit := True;
  FMinY := 0;
  FMaxY := 1;
  FLineColor := clDefault;
end;

procedure TOBDStripChart.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDStripChart.SetCapacity(AValue: Integer);
begin
  if AValue < 8 then AValue := 8;
  if AValue > 16384 then AValue := 16384;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  SetLength(FBuffer, FCapacity);
  FCount := 0;
  FNext := 0;
  Repaint;
end;

procedure TOBDStripChart.SetAutoFit(AValue: Boolean);
begin
  if FAutoFit = AValue then Exit;
  FAutoFit := AValue; Repaint;
end;

procedure TOBDStripChart.SetMinY(AValue: Double);
begin
  if SameValue(FMinY, AValue) then Exit;
  FMinY := AValue; Repaint;
end;

procedure TOBDStripChart.SetMaxY(AValue: Double);
begin
  if SameValue(FMaxY, AValue) then Exit;
  FMaxY := AValue; Repaint;
end;

procedure TOBDStripChart.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then Exit;
  FLineColor := AValue; Repaint;
end;

procedure TOBDStripChart.PushSample(AValue: Double);
begin
  FBuffer[FNext] := AValue;
  FNext := (FNext + 1) mod FCapacity;
  if FCount < FCapacity then Inc(FCount);
  NotifyBindings;
  Repaint;
end;

procedure TOBDStripChart.Reset;
begin
  FCount := 0;
  FNext  := 0;
  NotifyBindings;
  Repaint;
end;

function TOBDStripChart.SampleCount: Integer;
begin
  Result := FCount;
end;

procedure TOBDStripChart.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen:   TGPPen;
  Pad:   Integer;
  PlotX, PlotY, PlotW, PlotH: Integer;
  Pts: array of TGPPointF;
  Start, I, Idx: Integer;
  Lo, Hi, Span, V: Double;
  Col: TColor;
begin
  Pad   := ScaleValue(4);
  PlotX := Pad;
  PlotY := Pad;
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if (PlotW <= 0) or (PlotH <= 0) or (FCount < 2) then Exit;

  if FCount < FCapacity then Start := 0 else Start := FNext;

  if FAutoFit then
  begin
    Lo := FBuffer[Start mod FCapacity];
    Hi := Lo;
    for I := 1 to FCount - 1 do
    begin
      Idx := (Start + I) mod FCapacity;
      V := FBuffer[Idx];
      if V < Lo then Lo := V;
      if V > Hi then Hi := V;
    end;
    if SameValue(Lo, Hi) then
    begin
      Lo := Lo - 0.5;
      Hi := Hi + 0.5;
    end;
  end
  else
  begin
    Lo := FMinY;
    Hi := FMaxY;
    if Hi <= Lo then Hi := Lo + 1;
  end;
  Span := Hi - Lo;

  SetLength(Pts, FCount);
  for I := 0 to FCount - 1 do
  begin
    Idx := (Start + I) mod FCapacity;
    Pts[I].X := PlotX + I / (FCount - 1) * PlotW;
    Pts[I].Y := PlotY + PlotH -
      Single((FBuffer[Idx] - Lo) / Span) * PlotH;
  end;

  Col := ResolveColor(FLineColor, EffectiveAccent);
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Pen := TGPPen.Create(ColorToARGB(Col), ScaleValue(2));
  Pen.SetStartCap(LineCapRound);
  Pen.SetEndCap(LineCapRound);
  Pen.SetLineJoin(LineJoinRound);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.DrawLines(Pen, PGPPointF(@Pts[0]), FCount);
  finally
    Pen.Free;
    Graphics.Free;
  end;
end;

{ ---- TOBDLiveTrace ------------------------------------------------------ }

constructor TOBDLiveTrace.Create(const AName: string;
  AColor: TColor; ACapacity: Integer);
begin
  inherited Create;
  FName := AName;
  FColor := AColor;
  FCapacity := ACapacity;
  SetLength(FSamples, FCapacity);
  FMin := 0;
  FMax := 100;
end;

procedure TOBDLiveTrace.Push(AValue: Double);
begin
  FSamples[FNext] := AValue;
  FNext := (FNext + 1) mod FCapacity;
  if FCount < FCapacity then Inc(FCount);
end;

procedure TOBDLiveTrace.Reset;
begin
  FCount := 0;
  FNext  := 0;
end;

function TOBDLiveTrace.Sample(AIndex: Integer): Double;
var Start: Integer;
begin
  if FCount < FCapacity then Start := 0 else Start := FNext;
  Result := FSamples[(Start + AIndex) mod FCapacity];
end;

{ ---- TOBDLiveGridChart -------------------------------------------------- }

constructor TOBDLiveGridChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 200;
  FTraces := TObjectList<TOBDLiveTrace>.Create(True);
  FCapacity := 256;
  FShowLegend := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 8;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDLiveGridChart.Destroy;
begin
  FTraces.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDLiveGridChart.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLiveGridChart.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDLiveGridChart.SetCapacity(AValue: Integer);
begin
  if AValue < 8 then AValue := 8;
  if AValue > 16384 then AValue := 16384;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  // Note: existing traces keep their original capacity; new
  // traces honour the new default.
  Repaint;
end;

procedure TOBDLiveGridChart.SetShowLegend(AValue: Boolean);
begin
  if FShowLegend = AValue then Exit;
  FShowLegend := AValue; Repaint;
end;

procedure TOBDLiveGridChart.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

function TOBDLiveGridChart.AddTrace(const AName: string;
  AColor: TColor; const AUnits: string;
  AMinY, AMaxY: Double): TOBDLiveTrace;
begin
  Result := TOBDLiveTrace.Create(AName, AColor, FCapacity);
  Result.Units := AUnits;
  Result.MinY  := AMinY;
  Result.MaxY  := AMaxY;
  FTraces.Add(Result);
  NotifyBindings;
  Repaint;
end;

procedure TOBDLiveGridChart.PushSample(const ATraceName: string;
  AValue: Double);
var
  I: Integer;
begin
  for I := 0 to FTraces.Count - 1 do
    if SameText(FTraces[I].Name, ATraceName) then
    begin
      FTraces[I].Push(AValue);
      NotifyBindings;
      Repaint;
      Exit;
    end;
end;

procedure TOBDLiveGridChart.ResetAll;
var I: Integer;
begin
  for I := 0 to FTraces.Count - 1 do FTraces[I].Reset;
  NotifyBindings;
  Repaint;
end;

function TOBDLiveGridChart.TraceCount: Integer;
begin
  Result := FTraces.Count;
end;

function TOBDLiveGridChart.Trace(AIndex: Integer): TOBDLiveTrace;
begin
  Result := FTraces[AIndex];
end;

procedure TOBDLiveGridChart.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Pad: Integer;
  PlotX, PlotY, PlotW, PlotH: Integer;
  T: TOBDLiveTrace;
  I, S: Integer;
  Pts: array of TGPPointF;
  LegendY: Integer;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  PlotX := Pad;
  PlotY := Pad;
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if FShowLegend then PlotH := PlotH - ScaleValue(18);
  if (PlotW <= 0) or (PlotH <= 0) then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen,
        Single(PlotX), Single(PlotY),
        Single(PlotW), Single(PlotH));
    finally
      Pen.Free;
    end;

    for I := 0 to FTraces.Count - 1 do
    begin
      T := FTraces[I];
      if T.Count < 2 then Continue;
      SetLength(Pts, T.Count);
      for S := 0 to T.Count - 1 do
      begin
        Pts[S].X := PlotX + S / (T.Count - 1) * PlotW;
        Pts[S].Y := PlotY + PlotH -
          Single((T.Sample(S) - T.MinY) /
                 System.Math.Max(T.MaxY - T.MinY, 1e-9)) * PlotH;
      end;
      Pen := TGPPen.Create(ColorToARGB(T.Color), ScaleValue(2));
      Pen.SetLineJoin(LineJoinRound);
      try
        Graphics.DrawLines(Pen, PGPPointF(@Pts[0]), T.Count);
      finally
        Pen.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;

  if FShowLegend then
  begin
    ACanvas.Font := FCaptionFont;
    LegendY := PlotY + PlotH + ScaleValue(2);
    for I := 0 to FTraces.Count - 1 do
    begin
      T := FTraces[I];
      ACanvas.Font.Color := T.Color;
      ACanvas.TextOut(PlotX + I * ScaleValue(90), LegendY,
        Format('● %s', [T.Name]));
    end;
  end;
end;

{ ---- TOBDDynoChart ------------------------------------------------------ }

constructor TOBDDynoChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 480;
  Height := 240;
  FCapacity := 360;
  SetLength(FRPM,    FCapacity);
  SetLength(FHP,     FCapacity);
  SetLength(FTorque, FCapacity);
  FMaxHP     := 300;
  FMaxTorque := 500;
  FRedlineRPM := 7000;
  FHPColor     := clDefault;
  FTorqueColor := clDefault;
end;

procedure TOBDDynoChart.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDDynoChart.SetCapacity(AValue: Integer);
begin
  if AValue < 16 then AValue := 16;
  if AValue > 16384 then AValue := 16384;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  SetLength(FRPM,    FCapacity);
  SetLength(FHP,     FCapacity);
  SetLength(FTorque, FCapacity);
  FCount := 0;
  Repaint;
end;

procedure TOBDDynoChart.SetMaxHP(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FMaxHP, AValue) then Exit;
  FMaxHP := AValue; Repaint;
end;

procedure TOBDDynoChart.SetMaxTorque(AValue: Double);
begin
  if AValue <= 0 then Exit;
  if SameValue(FMaxTorque, AValue) then Exit;
  FMaxTorque := AValue; Repaint;
end;

procedure TOBDDynoChart.SetRedlineRPM(AValue: Double);
begin
  if AValue < 0 then AValue := 0;
  if SameValue(FRedlineRPM, AValue) then Exit;
  FRedlineRPM := AValue; Repaint;
end;

procedure TOBDDynoChart.SetHPColor(AValue: TColor);
begin
  if FHPColor = AValue then Exit;
  FHPColor := AValue; Repaint;
end;

procedure TOBDDynoChart.SetTorqueColor(AValue: TColor);
begin
  if FTorqueColor = AValue then Exit;
  FTorqueColor := AValue; Repaint;
end;

procedure TOBDDynoChart.PushSample(ARPM, AHP, ATorqueNm: Double);
begin
  if FCount < FCapacity then
  begin
    FRPM[FCount] := ARPM;
    FHP[FCount] := AHP;
    FTorque[FCount] := ATorqueNm;
    Inc(FCount);
  end
  else
  begin
    Move(FRPM[1],    FRPM[0],    (FCapacity - 1) * SizeOf(Double));
    Move(FHP[1],     FHP[0],     (FCapacity - 1) * SizeOf(Double));
    Move(FTorque[1], FTorque[0], (FCapacity - 1) * SizeOf(Double));
    FRPM[FCapacity - 1]    := ARPM;
    FHP[FCapacity - 1]     := AHP;
    FTorque[FCapacity - 1] := ATorqueNm;
  end;
  NotifyBindings;
  Repaint;
end;

procedure TOBDDynoChart.Reset;
begin
  FCount := 0;
  NotifyBindings;
  Repaint;
end;

function TOBDDynoChart.SampleCount: Integer;
begin
  Result := FCount;
end;

procedure TOBDDynoChart.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Pad: Integer;
  PlotX, PlotY, PlotW, PlotH: Integer;
  HPPts, TqPts: array of TGPPointF;
  I: Integer;
  HpCol, TqCol: TColor;
begin
  Pad := ScaleValue(8);
  PlotX := Pad;
  PlotY := Pad;
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if (PlotW <= 0) or (PlotH <= 0) or (FCount < 2) then Exit;

  HpCol := ResolveColor(FHPColor, Palette.Accent);
  TqCol := ResolveColor(FTorqueColor, Palette.Success);

  SetLength(HPPts, FCount);
  SetLength(TqPts, FCount);
  for I := 0 to FCount - 1 do
  begin
    HPPts[I].X := PlotX + I / (FCount - 1) * PlotW;
    HPPts[I].Y := PlotY + PlotH -
      Single(FHP[I] / FMaxHP) * PlotH;
    TqPts[I].X := HPPts[I].X;
    TqPts[I].Y := PlotY + PlotH -
      Single(FTorque[I] / FMaxTorque) * PlotH;
  end;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen,
        Single(PlotX), Single(PlotY),
        Single(PlotW), Single(PlotH));
    finally
      Pen.Free;
    end;

    Pen := TGPPen.Create(ColorToARGB(TqCol), ScaleValue(2));
    try
      Graphics.DrawLines(Pen, PGPPointF(@TqPts[0]), FCount);
    finally
      Pen.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(HpCol), ScaleValue(2));
    try
      Graphics.DrawLines(Pen, PGPPointF(@HPPts[0]), FCount);
    finally
      Pen.Free;
    end;

    // Redline marker — vertical dashed line at the RPM
    // closest to FRedlineRPM in the latest sample.
    if (FRedlineRPM > 0) and
       (FRPM[FCount - 1] >= FRedlineRPM) then
    begin
      Pen := TGPPen.Create(ColorToARGB(Palette.Danger),
        ScaleValue(1));
      Pen.SetDashStyle(DashStyleDash);
      try
        // Mark the last sample.
        Graphics.DrawLine(Pen,
          HPPts[FCount - 1].X, Single(PlotY),
          HPPts[FCount - 1].X, Single(PlotY + PlotH));
      finally
        Pen.Free;
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDPowerCurveGraph ----------------------------------------------- }

constructor TOBDPowerCurveGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 480;
  Height := 240;
  FHPColor := clDefault;
  FTorqueColor := clDefault;
end;

procedure TOBDPowerCurveGraph.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDPowerCurveGraph.SetHPColor(AValue: TColor);
begin
  if FHPColor = AValue then Exit;
  FHPColor := AValue; Repaint;
end;

procedure TOBDPowerCurveGraph.SetTorqueColor(AValue: TColor);
begin
  if FTorqueColor = AValue then Exit;
  FTorqueColor := AValue; Repaint;
end;

procedure TOBDPowerCurveGraph.LoadCurve(
  const APoints: TArray<TOBDDynoPoint>);
begin
  FCurve := Copy(APoints);
  NotifyBindings;
  Repaint;
end;

procedure TOBDPowerCurveGraph.Clear;
begin
  SetLength(FCurve, 0);
  NotifyBindings;
  Repaint;
end;

function TOBDPowerCurveGraph.PointCount: Integer;
begin
  Result := Length(FCurve);
end;

procedure TOBDPowerCurveGraph.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen: TGPPen;
  Pad: Integer;
  PlotX, PlotY, PlotW, PlotH: Integer;
  HpCol, TqCol: TColor;
  HPPts, TqPts: array of TGPPointF;
  I: Integer;
  MinRPM, MaxRPM, MaxHP, MaxTq: Double;
begin
  Pad := ScaleValue(8);
  PlotX := Pad;
  PlotY := Pad;
  PlotW := Width  - 2 * Pad;
  PlotH := Height - 2 * Pad;
  if (PlotW <= 0) or (PlotH <= 0) or (Length(FCurve) < 2) then Exit;

  MinRPM := FCurve[0].RPM;
  MaxRPM := MinRPM;
  MaxHP  := FCurve[0].HP;
  MaxTq  := FCurve[0].TorqueNm;
  for I := 1 to High(FCurve) do
  begin
    if FCurve[I].RPM      < MinRPM then MinRPM := FCurve[I].RPM;
    if FCurve[I].RPM      > MaxRPM then MaxRPM := FCurve[I].RPM;
    if FCurve[I].HP       > MaxHP  then MaxHP  := FCurve[I].HP;
    if FCurve[I].TorqueNm > MaxTq  then MaxTq  := FCurve[I].TorqueNm;
  end;
  if MaxRPM <= MinRPM then MaxRPM := MinRPM + 1;
  if MaxHP  <= 0      then MaxHP  := 1;
  if MaxTq  <= 0      then MaxTq  := 1;

  HpCol := ResolveColor(FHPColor, Palette.Accent);
  TqCol := ResolveColor(FTorqueColor, Palette.Success);

  SetLength(HPPts, Length(FCurve));
  SetLength(TqPts, Length(FCurve));
  for I := 0 to High(FCurve) do
  begin
    HPPts[I].X := PlotX + (FCurve[I].RPM - MinRPM) /
                   (MaxRPM - MinRPM) * PlotW;
    HPPts[I].Y := PlotY + PlotH -
      Single(FCurve[I].HP / MaxHP) * PlotH;
    TqPts[I].X := HPPts[I].X;
    TqPts[I].Y := PlotY + PlotH -
      Single(FCurve[I].TorqueNm / MaxTq) * PlotH;
  end;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.DrawRectangle(Pen,
        Single(PlotX), Single(PlotY),
        Single(PlotW), Single(PlotH));
    finally
      Pen.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(TqCol), ScaleValue(2));
    try
      Graphics.DrawLines(Pen, PGPPointF(@TqPts[0]),
        Length(FCurve));
    finally
      Pen.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(HpCol), ScaleValue(2));
    try
      Graphics.DrawLines(Pen, PGPPointF(@HPPts[0]),
        Length(FCurve));
    finally
      Pen.Free;
    end;
  finally
    Graphics.Free;
  end;
end;

end.
