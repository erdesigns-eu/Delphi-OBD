//------------------------------------------------------------------------------
//  ERD.UI.Progress
//
//  Themed progress indicators for the OBD Studio application chrome.
//
//    TOBDProgressBar  determinate, indeterminate and step progress with
//                     status colouring and design-time preview content.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.Progress;

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
  TOBDProgressBar = class;

  /// <summary>Colour state of a progress bar.</summary>
  TOBDProgressBarState = (
    /// <summary>Accent progress.</summary>
    pbsNormal,
    /// <summary>Success progress.</summary>
    pbsSuccess,
    /// <summary>Warning progress.</summary>
    pbsWarning,
    /// <summary>Danger progress.</summary>
    pbsError);

  /// <summary>Progress drawing style.</summary>
  TOBDProgressBarStyle = (
    /// <summary>One horizontal bar with optional label and value.</summary>
    pbsBar,
    /// <summary>Segmented ECU step row.</summary>
    pbsSteps);

  /// <summary>State of one step in a step progress row.</summary>
  TOBDProgressStepState = (
    /// <summary>Completed step.</summary>
    stDone,
    /// <summary>Current active step.</summary>
    stActive,
    /// <summary>Waiting step.</summary>
    stPending,
    /// <summary>Failed step.</summary>
    stFailed);

  /// <summary>One streamable progress step.</summary>
  TOBDProgressStep = class(TCollectionItem)
  strict private
    FCaption: string;
    FState: TOBDProgressStepState;
    procedure SetCaption(const AValue: string);
    procedure SetState(AValue: TOBDProgressStepState);
  protected
    /// <summary>Returns the caption for collection editors.</summary>
    /// <returns>Caption or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a pending step.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies all streamable fields from another step.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Step label shown to the right of the icon.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Visual state of the step.</summary>
    property State: TOBDProgressStepState read FState write SetState
      default stPending;
  end;

  /// <summary>Owned collection of progress steps.</summary>
  TOBDProgressSteps = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDProgressStep;
    procedure SetItem(AIndex: Integer; AValue: TOBDProgressStep);
  protected
    /// <summary>Invalidates the owner when contents change.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates the collection for a progress control.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a step item.</summary>
    /// <returns>New step item.</returns>
    function Add: TOBDProgressStep;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDProgressStep read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed progress bar and step progress control.</summary>
  TOBDProgressBar = class(TOBDCustomControl)
  strict private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FState: TOBDProgressBarState;
    FIndeterminate: Boolean;
    FCaption: string;
    FShowValue: Boolean;
    FStyle: TOBDProgressBarStyle;
    FSteps: TOBDProgressSteps;
    FTimer: TTimer;
    FMarquee: Integer;
    procedure SetMin(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetState(AValue: TOBDProgressBarState);
    procedure SetIndeterminate(AValue: Boolean);
    procedure SetCaption(const AValue: string);
    procedure SetShowValue(AValue: Boolean);
    procedure SetStyle(AValue: TOBDProgressBarStyle);
    procedure SetSteps(AValue: TOBDProgressSteps);
    procedure TimerTick(Sender: TObject);
    procedure UpdateTimer;
    procedure StepsChanged;
    function ProgressColor(APainter: TOBDPainter): TColor;
    function PreviewMode: Boolean;
    function PreviewStepCount: Integer;
    function EffectiveStepCount: Integer;
    function EffectiveStepCaption(AIndex: Integer): string;
    function EffectiveStepState(AIndex: Integer): TOBDProgressStepState;
    function Percent: Double;
    function PercentText: string;
    procedure DrawBar(APainter: TOBDPainter);
    procedure DrawSteps(APainter: TOBDPainter);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    /// <summary>Paints the progress control.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Starts or stops animation when the control receives a handle.</summary>
    procedure CreateWnd; override;
    /// <summary>Stops animation before the handle is destroyed.</summary>
    procedure DestroyWnd; override;
  public
    /// <summary>Creates a determinate progress bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the step collection.</summary>
    destructor Destroy; override;
    /// <summary>Updates animation after density changes.</summary>
    procedure DensityChanged; override;
  published
    /// <summary>Minimum progress value.</summary>
    property Min: Integer read FMin write SetMin default 0;
    /// <summary>Maximum progress value.</summary>
    property Max: Integer read FMax write SetMax default 100;
    /// <summary>Current progress value.</summary>
    property Position: Integer read FPosition write SetPosition default 0;
    /// <summary>Colour state of the bar.</summary>
    property State: TOBDProgressBarState read FState write SetState
      default pbsNormal;
    /// <summary>Shows an animated sliding segment instead of a fixed value.</summary>
    property Indeterminate: Boolean read FIndeterminate
      write SetIndeterminate default False;
    /// <summary>Label drawn above the track.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Shows percentage or status text at the right.</summary>
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
    /// <summary>Bar or step-row presentation.</summary>
    property Style: TOBDProgressBarStyle read FStyle write SetStyle
      default pbsBar;
    /// <summary>Steps drawn when <see cref="Style"/> is pbsSteps.</summary>
    property Steps: TOBDProgressSteps read FSteps write SetSteps;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Whether the control follows the parent theme density.</summary>
    property ParentDensity;
  end;

implementation

{ TOBDProgressStep ----------------------------------------------------------- }

constructor TOBDProgressStep.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FState := stPending;
end;

procedure TOBDProgressStep.Assign(Source: TPersistent);
var
  Step: TOBDProgressStep;
begin
  if Source is TOBDProgressStep then
  begin
    Step := TOBDProgressStep(Source);
    FCaption := Step.FCaption;
    FState := Step.FState;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDProgressStep.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDProgressStep.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDProgressStep.SetState(AValue: TOBDProgressStepState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

{ TOBDProgressSteps ---------------------------------------------------------- }

constructor TOBDProgressSteps.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDProgressStep);
end;

function TOBDProgressSteps.Add: TOBDProgressStep;
begin
  Result := TOBDProgressStep(inherited Add);
end;

function TOBDProgressSteps.GetItem(AIndex: Integer): TOBDProgressStep;
begin
  Result := TOBDProgressStep(inherited GetItem(AIndex));
end;

procedure TOBDProgressSteps.SetItem(AIndex: Integer; AValue: TOBDProgressStep);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDProgressSteps.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDProgressBar then
    TOBDProgressBar(GetOwner).StepsChanged;
end;

{ TOBDProgressBar ------------------------------------------------------------ }

constructor TOBDProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax := 100;
  FShowValue := True;
  FSteps := TOBDProgressSteps.Create(Self);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 45;
  FTimer.OnTimer := TimerTick;
  Width := ScaleValue(320);
  Height := ScaleValue(40);
end;

destructor TOBDProgressBar.Destroy;
begin
  FSteps.Free;
  inherited Destroy;
end;

procedure TOBDProgressBar.CreateWnd;
begin
  inherited;
  UpdateTimer;
end;

procedure TOBDProgressBar.DestroyWnd;
begin
  FTimer.Enabled := False;
  inherited;
end;

procedure TOBDProgressBar.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
  UpdateTimer;
end;

procedure TOBDProgressBar.SetMin(AValue: Integer);
begin
  if FMin = AValue then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TOBDProgressBar.SetMax(AValue: Integer);
begin
  if FMax = AValue then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  SetPosition(FPosition);
  Invalidate;
end;

procedure TOBDProgressBar.SetPosition(AValue: Integer);
begin
  AValue := EnsureRange(AValue, FMin, FMax);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Invalidate;
end;

procedure TOBDProgressBar.SetState(AValue: TOBDProgressBarState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Invalidate;
end;

procedure TOBDProgressBar.SetIndeterminate(AValue: Boolean);
begin
  if FIndeterminate = AValue then
    Exit;
  FIndeterminate := AValue;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDProgressBar.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Invalidate;
end;

procedure TOBDProgressBar.SetShowValue(AValue: Boolean);
begin
  if FShowValue = AValue then
    Exit;
  FShowValue := AValue;
  Invalidate;
end;

procedure TOBDProgressBar.SetStyle(AValue: TOBDProgressBarStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  UpdateTimer;
  Invalidate;
end;

procedure TOBDProgressBar.SetSteps(AValue: TOBDProgressSteps);
begin
  FSteps.Assign(AValue);
end;

procedure TOBDProgressBar.TimerTick(Sender: TObject);
begin
  Inc(FMarquee, ScaleValue(5));
  if FMarquee > Width then
    FMarquee := 0;
  Invalidate;
end;

procedure TOBDProgressBar.UpdateTimer;
begin
  if FTimer = nil then
    Exit;
  FTimer.Enabled := FIndeterminate and (FStyle = pbsBar) and Visible and
    HandleAllocated and not (csDesigning in ComponentState);
end;

procedure TOBDProgressBar.StepsChanged;
begin
  Invalidate;
end;

function TOBDProgressBar.ProgressColor(APainter: TOBDPainter): TColor;
begin
  case FState of
    pbsSuccess:
      Result := Palette.Success;
    pbsWarning:
      Result := Palette.Warning;
    pbsError:
      Result := Palette.Danger;
  else
    Result := Palette.Accent;
  end;
end;

function TOBDProgressBar.PreviewMode: Boolean;
begin
  Result := IsPreview and (FSteps.Count = 0) and (FCaption = '') and
    (FPosition = FMin) and not FIndeterminate;
end;

function TOBDProgressBar.PreviewStepCount: Integer;
begin
  Result := 4;
end;

function TOBDProgressBar.EffectiveStepCount: Integer;
begin
  if PreviewMode then
    Result := PreviewStepCount
  else
    Result := FSteps.Count;
end;

function TOBDProgressBar.EffectiveStepCaption(AIndex: Integer): string;
begin
  if not PreviewMode then
    Result := FSteps[AIndex].Caption
  else
    case AIndex of
      0: Result := 'Engine 7E8';
      1: Result := 'Gearbox 7E9';
      2: Result := 'ABS 7E2';
    else
      Result := 'Airbag 7E3';
    end;
end;

function TOBDProgressBar.EffectiveStepState(AIndex: Integer): TOBDProgressStepState;
begin
  if not PreviewMode then
    Result := FSteps[AIndex].State
  else
    case AIndex of
      0, 1: Result := stDone;
      2: Result := stActive;
    else
      Result := stPending;
    end;
end;

function TOBDProgressBar.Percent: Double;
begin
  if FMax = FMin then
    Result := 0
  else
    Result := (FPosition - FMin) / (FMax - FMin);
  if Result < 0 then
    Result := 0
  else if Result > 1 then
    Result := 1;
end;

function TOBDProgressBar.PercentText: string;
begin
  if FIndeterminate then
    Result := 'indeterminate'
  else if FState = pbsError then
    Result := 'Failed'
  else
    Result := Format('%d %%', [Round(Percent * 100)]);
end;

procedure TOBDProgressBar.DrawBar(APainter: TOBDPainter);
var
  TopY, TrackY, TrackH, LabelSize, FillW, SegW, SegX: Integer;
  C, TextColor: TColor;
  LabelText, RightText: string;
begin
  if PreviewMode then
  begin
    LabelText := 'Reading codes ' + WideChar($00B7) + ' 2 of 3 ECUs';
    RightText := '62 %';
  end
  else
  begin
    LabelText := FCaption;
    RightText := PercentText;
  end;

  LabelSize := ScaleValue(20);
  TopY := ScaleValue(2);
  TrackY := TopY + LabelSize;
  TrackH := ScaleValue(6);
  C := ProgressColor(APainter);
  TextColor := Palette.ForegroundText;
  if FState = pbsError then
    TextColor := Palette.Danger;

  if LabelText <> '' then
    APainter.Text(0, TopY + ScaleValue(8), LabelText, 12.5,
      Palette.ForegroundText, twRegular, taLeftJustify,
      Width - ScaleValue(90));
  if FShowValue or FIndeterminate or PreviewMode then
    APainter.Text(Width, TopY + ScaleValue(8), RightText, 12,
      IfThen(FState = pbsError, TextColor, Palette.GaugeLabel), twSemibold,
      taRightJustify);

  APainter.FillRect(Rect(0, TrackY, Width, TrackY + TrackH),
    Palette.NeutralLight);
  if FIndeterminate and not PreviewMode then
  begin
    SegW := System.Math.Max(ScaleValue(48), Width div 4);
    SegX := (FMarquee mod (Width + SegW)) - SegW;
    APainter.FillRect(Rect(SegX, TrackY, SegX + SegW, TrackY + TrackH), C);
  end
  else
  begin
    if PreviewMode then
      FillW := Round(Width * 0.62)
    else
      FillW := Round(Width * Percent);
    APainter.FillRect(Rect(0, TrackY, FillW, TrackY + TrackH), C);
  end;
end;

procedure TOBDProgressBar.DrawSteps(APainter: TOBDPainter);
var
  Count, I, SX, SW, BarW, YY, CY: Integer;
  St: TOBDProgressStepState;
  C, Ink: TColor;
begin
  Count := EffectiveStepCount;
  if Count <= 0 then
    Exit;
  YY := ScaleValue(6);
  CY := YY + ScaleValue(18);
  SW := Width div Count;
  for I := 0 to Count - 1 do
  begin
    SX := I * SW;
    BarW := SW - ScaleValue(6);
    if I = Count - 1 then
      BarW := Width - SX;
    St := EffectiveStepState(I);
    case St of
      stDone:
        C := Palette.Success;
      stActive:
        C := APainter.AccentText;
      stFailed:
        C := Palette.Danger;
    else
      C := Palette.NeutralLight;
    end;
    APainter.FillRect(Rect(SX, YY, SX + BarW, YY + ScaleValue(4)), C);
    case St of
      stDone:
        APainter.Glyph(glCheck, SX + ScaleValue(7), CY, Palette.Success, 0.7);
      stActive:
        APainter.Glyph(glPending, SX + ScaleValue(7), CY, APainter.AccentText,
          0.75);
      stFailed:
        APainter.Glyph(glAlert, SX + ScaleValue(7), CY, Palette.Danger, 0.55);
    else
      APainter.Glyph(glDash, SX + ScaleValue(7), CY, Palette.Subtle, 0.7);
    end;
    if St = stPending then
      Ink := Palette.GaugeLabel
    else
      Ink := Palette.ForegroundText;
    APainter.Text(SX + ScaleValue(18), CY, EffectiveStepCaption(I), 12, Ink,
      twRegular, taLeftJustify, SW - ScaleValue(22));
  end;
end;

procedure TOBDProgressBar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    if FStyle = pbsSteps then
      DrawSteps(P)
    else
      DrawBar(P);
  finally
    P.Free;
  end;
end;

procedure TOBDProgressBar.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  UpdateTimer;
end;

end.
