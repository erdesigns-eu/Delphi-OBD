//------------------------------------------------------------------------------
//  ERD.UI.LapTimer
//
//  TOBDLapTimer - OBD Studio Pro card for drag and acceleration runs. It
//  shows a large elapsed-time read-out, the run state (staged, armed,
//  running, finished), a target line and a list of recorded splits with
//  their time, speed and difference against the best run. While running an
//  internal timer repaints the read-out.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.LapTimer;

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
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Run state of a <see cref="TOBDLapTimer"/>.</summary>
  TOBDLapState = (
    /// <summary>Lined up at the start, not armed.</summary>
    ltsStaged,
    /// <summary>Armed and waiting for launch.</summary>
    ltsArmed,
    /// <summary>The run is in progress; the timer counts up.</summary>
    ltsRunning,
    /// <summary>The run has finished; the final time is held.</summary>
    ltsFinished);

  TOBDLapTimer = class;

  /// <summary>One recorded split of a run.</summary>
  TOBDLapSplit = class(TCollectionItem)
  strict private
    FName: string;
    FTimeText: string;
    FSpeedText: string;
    FDelta: Double;
    procedure SetName(const AValue: string);
    procedure SetTimeText(const AValue: string);
    procedure SetSpeedText(const AValue: string);
    procedure SetDelta(AValue: Double);
  protected
    /// <summary>Row name for the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a split row.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies the split fields.</summary>
    /// <param name="ASource">Another split.</param>
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>Split label (e.g. "0-100 km/h").</summary>
    property Name: string read FName write SetName;
    /// <summary>Formatted split time (e.g. "4.21 s").</summary>
    property TimeText: string read FTimeText write SetTimeText;
    /// <summary>Formatted speed at the split, or empty.</summary>
    property SpeedText: string read FSpeedText write SetSpeedText;
    /// <summary>Difference against the best run in seconds; negative
    /// is faster.</summary>
    property Delta: Double read FDelta write SetDelta;
  end;

  /// <summary>Collection of <see cref="TOBDLapSplit"/>.</summary>
  TOBDLapSplitCollection = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDLapSplit;
  protected
    /// <summary>Repaints the timer when a split changes.</summary>
    /// <param name="Item">Changed item or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a split.</summary>
    /// <returns>The new split.</returns>
    function Add: TOBDLapSplit;
    /// <summary>Split by index.</summary>
    property Items[AIndex: Integer]: TOBDLapSplit read GetItem; default;
  end;

  /// <summary>Fired when a split is recorded through <c>AddSplit</c>.
  /// </summary>
  /// <param name="Sender">The timer.</param>
  /// <param name="ASplit">The recorded split.</param>
  TOBDLapSplitEvent = procedure(Sender: TObject; ASplit: TOBDLapSplit)
    of object;

  /// <summary>Drag / acceleration run timer card.</summary>
  TOBDLapTimer = class(TOBDCustomControl, IOBDSurface)
  strict private
    FSplits: TOBDLapSplitCollection;
    FTitle: string;
    FTargetText: string;
    FState: TOBDLapState;
    FStartTick: UInt64;
    FFinalMs: Int64;
    FTimer: TTimer;
    FOnSplit: TOBDLapSplitEvent;
    FOnStateChange: TNotifyEvent;
    procedure SetSplits(AValue: TOBDLapSplitCollection);
    procedure SetTitle(const AValue: string);
    procedure SetTargetText(const AValue: string);
    procedure SetState(AValue: TOBDLapState);
    procedure TimerTick(Sender: TObject);
    function UsePreviewData: Boolean;
    function HeaderHeight: Integer;
    function RowHeight: Integer;
    function StateText(AState: TOBDLapState): string;
    function StateColorFor(AState: TOBDLapState; AActive: Boolean): TColor;
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintReadout(var P: TOBDPainter; ATop: Integer);
    procedure PaintStateChips(var P: TOBDPainter; ATop: Integer);
    procedure PaintSplits(var P: TOBDPainter; ATop: Integer);
  protected
    /// <summary>Paints the timer card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Repaints after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Creates the timer with preview defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Stops the timer and frees the splits.</summary>
    destructor Destroy; override;
    /// <summary>Card face colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Arms and starts the run; the read-out counts up.
    /// </summary>
    procedure Start;
    /// <summary>Stops the run and holds the final time.</summary>
    procedure Stop;
    /// <summary>Clears the run, splits and time.</summary>
    procedure Reset;
    /// <summary>Milliseconds elapsed in the current or last run.
    /// </summary>
    /// <returns>Elapsed milliseconds.</returns>
    function ElapsedMs: Int64;
    /// <summary>Records a split row and fires <c>OnSplit</c>.</summary>
    /// <param name="AName">Split label.</param>
    /// <param name="ATimeText">Formatted time.</param>
    /// <param name="ASpeedText">Formatted speed.</param>
    /// <param name="ADelta">Difference against the best run.</param>
    /// <returns>The new split.</returns>
    function AddSplit(const AName, ATimeText, ASpeedText: string;
      ADelta: Double): TOBDLapSplit;
    /// <summary>Removes every split.</summary>
    procedure ClearSplits;
    /// <summary>Current run state.</summary>
    property State: TOBDLapState read FState write SetState;
  published
    /// <summary>Recorded splits.</summary>
    property Splits: TOBDLapSplitCollection read FSplits write SetSplits;
    /// <summary>Card title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Target / summary line under the read-out.</summary>
    property TargetText: string read FTargetText write SetTargetText;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a split is recorded.</summary>
    property OnSplit: TOBDLapSplitEvent read FOnSplit write FOnSplit;
    /// <summary>Fires when the run state changes.</summary>
    property OnStateChange: TNotifyEvent read FOnStateChange
      write FOnStateChange;
  end;

implementation

const
  /// <summary>Em height of the big elapsed-time digits, 96-DPI pixels.
  /// </summary>
  OBD_LAPTIMER_DIGITS = 44;

type
  TPreviewSplit = record
    Name: string;
    TimeText: string;
    SpeedText: string;
    Delta: Double;
  end;

function PreviewSplits: TArray<TPreviewSplit>;
  function MakeSplit(const AName, ATime, ASpeed: string;
    ADelta: Double): TPreviewSplit;
  begin
    Result.Name := AName;
    Result.TimeText := ATime;
    Result.SpeedText := ASpeed;
    Result.Delta := ADelta;
  end;
begin
  SetLength(Result, 5);
  Result[0] := MakeSplit('60 ft', '2.41 s', '', -0.02);
  Result[1] := MakeSplit('0' + #$2013 + '60 km/h', '4.21 s', '', 0.03);
  Result[2] := MakeSplit('0' + #$2013 + '100 km/h', '10.96 s', '', 0.08);
  Result[3] := MakeSplit(#$215B + ' mile', '11.32 s', '106 km/h', 0.06);
  Result[4] := MakeSplit(#$00BC + ' mile', '17.62 s', '129 km/h', 0.14);
end;

{ TOBDLapSplit }

constructor TOBDLapSplit.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TOBDLapSplit.Assign(ASource: TPersistent);
var
  S: TOBDLapSplit;
begin
  if ASource is TOBDLapSplit then
  begin
    S := TOBDLapSplit(ASource);
    FName := S.Name;
    FTimeText := S.TimeText;
    FSpeedText := S.SpeedText;
    FDelta := S.Delta;
    Changed(False);
  end
  else
    inherited Assign(ASource);
end;

function TOBDLapSplit.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDLapSplit.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDLapSplit.SetTimeText(const AValue: string);
begin
  if FTimeText = AValue then
    Exit;
  FTimeText := AValue;
  Changed(False);
end;

procedure TOBDLapSplit.SetSpeedText(const AValue: string);
begin
  if FSpeedText = AValue then
    Exit;
  FSpeedText := AValue;
  Changed(False);
end;

procedure TOBDLapSplit.SetDelta(AValue: Double);
begin
  if SameValue(FDelta, AValue) then
    Exit;
  FDelta := AValue;
  Changed(False);
end;

{ TOBDLapSplitCollection }

function TOBDLapSplitCollection.Add: TOBDLapSplit;
begin
  Result := TOBDLapSplit(inherited Add);
end;

function TOBDLapSplitCollection.GetItem(AIndex: Integer): TOBDLapSplit;
begin
  Result := TOBDLapSplit(inherited Items[AIndex]);
end;

procedure TOBDLapSplitCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner is TOBDLapTimer then
    TOBDLapTimer(Owner).Invalidate;
end;

{ TOBDLapTimer }

constructor TOBDLapTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSplits := TOBDLapSplitCollection.Create(Self, TOBDLapSplit);
  FTitle := 'Drag run';
  FTargetText := #$00BC + ' mile ' + #$00B7 + ' 129 km/h ' + #$00B7 +
    ' best 17.48 s (+0.14)';
  FState := ltsFinished;
  FFinalMs := 17620;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTick;
  Width := 420;
  Height := 420;
end;

destructor TOBDLapTimer.Destroy;
begin
  FTimer.Enabled := False;
  FSplits.Free;
  inherited;
end;

procedure TOBDLapTimer.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDLapTimer.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDLapTimer.TimerTick(Sender: TObject);
begin
  if FState = ltsRunning then
    Invalidate;
end;

procedure TOBDLapTimer.Start;
begin
  FStartTick := TThread.GetTickCount64;
  FFinalMs := 0;
  SetState(ltsRunning);
  FTimer.Enabled := True;
end;

procedure TOBDLapTimer.Stop;
begin
  if FState = ltsRunning then
    FFinalMs := Int64(TThread.GetTickCount64 - FStartTick);
  FTimer.Enabled := False;
  SetState(ltsFinished);
end;

procedure TOBDLapTimer.Reset;
begin
  FTimer.Enabled := False;
  FFinalMs := 0;
  FStartTick := 0;
  FSplits.Clear;
  SetState(ltsStaged);
  Invalidate;
end;

function TOBDLapTimer.ElapsedMs: Int64;
begin
  if FState = ltsRunning then
    Result := Int64(TThread.GetTickCount64 - FStartTick)
  else
    Result := FFinalMs;
end;

function TOBDLapTimer.AddSplit(const AName, ATimeText, ASpeedText: string;
  ADelta: Double): TOBDLapSplit;
begin
  Result := FSplits.Add;
  Result.Name := AName;
  Result.TimeText := ATimeText;
  Result.SpeedText := ASpeedText;
  Result.Delta := ADelta;
  if Assigned(FOnSplit) then
    FOnSplit(Self, Result);
end;

procedure TOBDLapTimer.ClearSplits;
begin
  FSplits.Clear;
end;

procedure TOBDLapTimer.SetSplits(AValue: TOBDLapSplitCollection);
begin
  FSplits.Assign(AValue);
end;

procedure TOBDLapTimer.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDLapTimer.SetTargetText(const AValue: string);
begin
  if FTargetText = AValue then
    Exit;
  FTargetText := AValue;
  Invalidate;
end;

procedure TOBDLapTimer.SetState(AValue: TOBDLapState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  if FState <> ltsRunning then
    FTimer.Enabled := False;
  Invalidate;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

function TOBDLapTimer.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FSplits.Count = 0);
end;

function TOBDLapTimer.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDLapTimer.RowHeight: Integer;
begin
  Result := ScaleValue(System.Math.Min(30, Metrics.Row - 12));
end;

function TOBDLapTimer.StateText(AState: TOBDLapState): string;
begin
  case AState of
    ltsStaged:
      Result := 'STAGED';
    ltsArmed:
      Result := 'ARMED';
    ltsRunning:
      Result := 'RUNNING';
  else
    Result := 'FINISHED';
  end;
end;

procedure TOBDLapTimer.PaintHeader(var P: TOBDPainter; const R: TRect);
var
  X, Y, ChipW: Integer;
  ChipText: string;
  ChipColor: TColor;
begin
  Y := R.Top + R.Height div 2;
  P.Glyph(glFinishFlag, R.Left + P.S(26), Y, P.AccentText, 1);
  X := R.Left + P.S(46);
  P.Text(X, Y, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText, twBold,
    taLeftJustify, R.Width - P.S(160));
  ChipText := StateText(FState);
  if FState = ltsFinished then
    ChipColor := Palette.Success
  else if FState = ltsRunning then
    ChipColor := P.AccentText
  else
    ChipColor := Palette.Subtle;
  ChipW := P.ChipWidth(ChipText);
  P.Chip(R.Right - P.S(16) - ChipW, Y - P.S(10), ChipText, ChipColor,
    FState in [ltsRunning, ltsFinished]);
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDLapTimer.PaintStateChips(var P: TOBDPainter; ATop: Integer);
var
  I, SX: Integer;
  St: TOBDLapState;
  Txt: string;
  Active: Boolean;
begin
  SX := ScaleValue(20);
  for I := 0 to 3 do
  begin
    St := TOBDLapState(I);
    Txt := StateText(St);
    Active := St = FState;
    P.Chip(SX, ATop, Txt, StateColorFor(St, Active), Active, False,
      OBD_TEXT_CAPS);
    Inc(SX, P.ChipWidth(Txt, False, OBD_TEXT_CAPS) + P.S(6));
  end;
end;

procedure TOBDLapTimer.PaintReadout(var P: TOBDPainter; ATop: Integer);
var
  Secs: Double;
  Big, X: Integer;
  Txt: string;
begin
  Secs := ElapsedMs / 1000;
  Txt := FormatFloat('0.00', Secs);
  X := ScaleValue(20);
  Big := P.Text(X, ATop, Txt, OBD_LAPTIMER_DIGITS, Palette.ForegroundText,
    twBold);
  P.Text(X + Big + P.S(6), ATop + P.S(10), 's', OBD_TEXT_TITLE,
    Palette.GaugeLabel);
  P.Text(X, ATop + P.S(36), FTargetText, OBD_TEXT_BODY, Palette.GaugeLabel,
    twRegular, taLeftJustify, Width - ScaleValue(40));
end;

procedure TOBDLapTimer.PaintSplits(var P: TOBDPainter; ATop: Integer);
var
  Y, RH, Mid, I, Count: Integer;
  Name, TimeText, SpeedText: string;
  Delta: Double;
  Preview: TArray<TPreviewSplit>;
  Col: TColor;
begin
  // Column header.
  P.FillRect(Rect(ScaleValue(1), ATop, Width - ScaleValue(1),
    ATop + ScaleValue(Metrics.ColHead)), P.HeaderFill);
  P.HLine(ScaleValue(1), ATop + ScaleValue(Metrics.ColHead) - ScaleValue(1),
    Width - ScaleValue(2), Palette.NeutralLight);
  Mid := ATop + ScaleValue(Metrics.ColHead) div 2;
  P.Caps(ScaleValue(20), Mid, 'Split');
  P.Caps(Round(Width * 0.45), Mid, 'Time');
  P.Caps(Round(Width * 0.65), Mid, 'Speed');
  P.Caps(Width - ScaleValue(16), Mid, 'vs best', taRightJustify);
  Y := ATop + ScaleValue(Metrics.ColHead);
  RH := RowHeight;

  if UsePreviewData then
  begin
    Preview := PreviewSplits;
    Count := Length(Preview);
  end
  else
  begin
    Preview := nil;
    Count := FSplits.Count;
  end;

  if Count = 0 then
  begin
    P.Text(Width div 2, Y + RH div 2, 'No splits', OBD_TEXT_BODY,
      Palette.GaugeLabel, twRegular, taCenter);
    Exit;
  end;

  for I := 0 to Count - 1 do
  begin
    if Y >= Height - ScaleValue(2) then
      Break;
    if UsePreviewData then
    begin
      Name := Preview[I].Name;
      TimeText := Preview[I].TimeText;
      SpeedText := Preview[I].SpeedText;
      Delta := Preview[I].Delta;
    end
    else
    begin
      Name := FSplits[I].Name;
      TimeText := FSplits[I].TimeText;
      SpeedText := FSplits[I].SpeedText;
      Delta := FSplits[I].Delta;
    end;
    Mid := Y + RH div 2;
    P.Text(ScaleValue(20), Mid, Name, OBD_TEXT_BODY, Palette.ForegroundText);
    P.Text(Round(Width * 0.45), Mid, TimeText, OBD_TEXT_BODY,
      Palette.ForegroundText, twSemibold);
    P.Text(Round(Width * 0.65), Mid, SpeedText, OBD_TEXT_BODY,
      Palette.GaugeLabel);
    if Delta < 0 then
      Col := Palette.Success
    else
      Col := P.DangerInk;
    P.Text(Width - ScaleValue(16), Mid, FormatFloat('+0.00;-0.00', Delta),
      OBD_TEXT_BODY, Col, twSemibold, taRightJustify);
    Inc(Y, RH);
    P.HLine(ScaleValue(1), Y - ScaleValue(1), Width - ScaleValue(2),
      Palette.NeutralLight);
  end;
end;

procedure TOBDLapTimer.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Y: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    PaintHeader(P, Rect(0, 0, Width, HeaderHeight));
    Y := HeaderHeight + ScaleValue(12);
    PaintStateChips(P, Y);
    PaintReadout(P, Y + ScaleValue(28));
    PaintSplits(P, Y + ScaleValue(116));
  finally
    P.Free;
  end;
end;

function TOBDLapTimer.StateColorFor(AState: TOBDLapState;
  AActive: Boolean): TColor;
begin
  if not AActive then
    Exit(Palette.Subtle);
  case AState of
    ltsStaged:
      Result := Palette.Subtle;
    ltsArmed:
      Result := Palette.Warning;
    ltsRunning:
      Result := EffectiveAccent;
  else
    Result := Palette.Success;
  end;
end;

end.
