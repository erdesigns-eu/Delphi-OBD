//------------------------------------------------------------------------------
//  ERD.UI.ActuatorPanel
//
//  TOBDActuatorPanel - OBD Studio Pro actuator-test list with state,
//  progress, feedback values and painted Start / Stop actions.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.ActuatorPanel;

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
  ERD.UI.Paint,
  ERD.UI.ScrollBar;

type
  /// <summary>Runtime state of an actuator test row.</summary>
  TOBDActuatorTestState = (
    /// <summary>The test is ready to start.</summary>
    apsIdle,
    /// <summary>The test is running and can be stopped.</summary>
    apsRunning,
    /// <summary>The test completed successfully.</summary>
    apsDone,
    /// <summary>The test failed.</summary>
    apsFailed);

  /// <summary>Fires for a row action button.</summary>
  /// <param name="Sender">The actuator panel.</param>
  /// <param name="AIndex">Actuator row index.</param>
  TOBDActuatorEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  TOBDActuatorPanel = class;

  /// <summary>One actuator-test row.</summary>
  TOBDActuatorItem = class(TCollectionItem)
  strict private
    FName: string;
    FState: TOBDActuatorTestState;
    FProgress: Integer;
    FFeedback: string;
    procedure SetName(const AValue: string);
    procedure SetState(AValue: TOBDActuatorTestState);
    procedure SetProgress(AValue: Integer);
    procedure SetFeedback(const AValue: string);
  protected
    /// <summary>Returns the actuator name for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an idle actuator row.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another actuator row.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Actuator-test name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Current test state.</summary>
    property State: TOBDActuatorTestState read FState write SetState
      default apsIdle;
    /// <summary>Running progress from 0 to 100.</summary>
    property Progress: Integer read FProgress write SetProgress default 0;
    /// <summary>Live feedback value text.</summary>
    property Feedback: string read FFeedback write SetFeedback;
  end;

  /// <summary>Streamable actuator-test row collection.</summary>
  TOBDActuatorCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDActuatorItem;
    procedure SetItem(Index: Integer; AValue: TOBDActuatorItem);
  protected
    /// <summary>Notifies the panel when an item changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates an actuator row collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds an actuator row.</summary>
    /// <returns>The new row.</returns>
    function Add: TOBDActuatorItem;
    /// <summary>Copies another actuator row collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed actuator row indexer.</summary>
    property Items[Index: Integer]: TOBDActuatorItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Actuator-test card with Start, Stop and Stop all actions.</summary>
  TOBDActuatorPanel = class(TOBDCustomControl, IOBDSurface)
  strict private
    FItems: TOBDActuatorCollection;
    FHoverIndex: Integer;
    FHoverButton: Integer;
    FDownIndex: Integer;
    FDownButton: Integer;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FOnStart: TOBDActuatorEvent;
    FOnStop: TOBDActuatorEvent;
    FOnStopAll: TNotifyEvent;
    procedure SetItems(AValue: TOBDActuatorCollection);
    procedure SetScrollPos(AValue: Integer);
    procedure ItemsChanged;
    function UsePreviewData: Boolean;
    function VisualCount: Integer;
    function VisualName(AIndex: Integer): string;
    function VisualFeedback(AIndex: Integer): string;
    function VisualState(AIndex: Integer): TOBDActuatorTestState;
    function VisualProgress(AIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollBarRect: TRect;
    function MaxScroll: Integer;
    function ScrollThumbRect: TRect;
    function StopAllRect: TRect;
    function RowButtonRect(AIndex, AButton: Integer): TRect;
    function HitButton(X, Y: Integer; out AIndex: Integer;
      out AButton: Integer): Boolean;
    function ActiveCount: Integer;
    function StateColor(APainter: TOBDPainter;
      AState: TOBDActuatorTestState): TColor;
    function StateText(AState: TOBDActuatorTestState): string;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the actuator-test card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks action-button hover and scroll dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Presses action buttons or starts scroll dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Fires a clicked action button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Scrolls down.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    /// <summary>Scrolls up.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
  public
    /// <summary>Creates an empty actuator panel.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row collection.</summary>
    destructor Destroy; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes every actuator row.</summary>
    procedure Clear;
    /// <summary>Adds one actuator test row.</summary>
    /// <param name="AName">Actuator name.</param>
    /// <param name="AFeedback">Initial feedback text.</param>
    /// <returns>The new row.</returns>
    function AddTest(const AName, AFeedback: string): TOBDActuatorItem;
    /// <summary>Number of actuator rows.</summary>
    /// <returns>Row count.</returns>
    function Count: Integer;
  published
    /// <summary>Streamed actuator-test rows.</summary>
    property Items: TOBDActuatorCollection read FItems write SetItems;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a row Start button is clicked.</summary>
    property OnStart: TOBDActuatorEvent read FOnStart write FOnStart;
    /// <summary>Fires when a row Stop button is clicked.</summary>
    property OnStop: TOBDActuatorEvent read FOnStop write FOnStop;
    /// <summary>Fires when the header Stop all button is clicked.</summary>
    property OnStopAll: TNotifyEvent read FOnStopAll write FOnStopAll;
  end;

implementation

const
  PREVIEW_ACTUATOR_COUNT = 6;

function PreviewActuatorName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Radiator fan';
    1: Result := 'EGR valve';
    2: Result := 'Fuel pump relay';
    3: Result := 'Glow plugs';
    4: Result := 'Boost pressure actuator';
  else
    Result := 'Throttle valve';
  end;
end;

function PreviewActuatorFeedback(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Fan speed 2 140 rpm';
    1: Result := 'Position 34 %';
    2: Result := 'Off';
    3: Result := 'Current 0.0 A';
    4: Result := 'Position 2 %';
  else
    Result := 'Angle 87 %';
  end;
end;

function PreviewActuatorState(AIndex: Integer): TOBDActuatorTestState;
begin
  case AIndex of
    0, 1: Result := apsRunning;
    3: Result := apsDone;
  else
    Result := apsIdle;
  end;
end;

function PreviewActuatorProgress(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := 60;
    1: Result := 35;
  else
    Result := 0;
  end;
end;

{ TOBDActuatorItem }

constructor TOBDActuatorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := apsIdle;
end;

procedure TOBDActuatorItem.Assign(Source: TPersistent);
var
  Item: TOBDActuatorItem;
begin
  if Source is TOBDActuatorItem then
  begin
    Item := TOBDActuatorItem(Source);
    FName := Item.Name;
    FState := Item.State;
    FProgress := Item.Progress;
    FFeedback := Item.Feedback;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDActuatorItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDActuatorItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetState(AValue: TOBDActuatorTestState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetProgress(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 100);
  if FProgress = AValue then
    Exit;
  FProgress := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetFeedback(const AValue: string);
begin
  if FFeedback = AValue then
    Exit;
  FFeedback := AValue;
  Changed(False);
end;

{ TOBDActuatorCollection }

constructor TOBDActuatorCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDActuatorItem);
end;

function TOBDActuatorCollection.Add: TOBDActuatorItem;
begin
  Result := TOBDActuatorItem(inherited Add);
end;

procedure TOBDActuatorCollection.Assign(Source: TPersistent);
var
  Src: TOBDActuatorCollection;
  I: Integer;
begin
  if Source is TOBDActuatorCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDActuatorCollection(Source);
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

function TOBDActuatorCollection.GetItem(Index: Integer): TOBDActuatorItem;
begin
  Result := TOBDActuatorItem(inherited GetItem(Index));
end;

procedure TOBDActuatorCollection.SetItem(Index: Integer;
  AValue: TOBDActuatorItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDActuatorCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDActuatorPanel then
    TOBDActuatorPanel(GetOwner).ItemsChanged;
end;

{ TOBDActuatorPanel }

constructor TOBDActuatorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 560;
  Height := 430;
  FHoverIndex := -1;
  FHoverButton := -1;
  FDownIndex := -1;
  FDownButton := -1;
  FItems := TOBDActuatorCollection.Create(Self);
end;

destructor TOBDActuatorPanel.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TOBDActuatorPanel.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDActuatorPanel.Clear;
begin
  FItems.Clear;
  FScrollPos := 0;
  Invalidate;
end;

function TOBDActuatorPanel.AddTest(const AName,
  AFeedback: string): TOBDActuatorItem;
begin
  Result := FItems.Add;
  Result.Name := AName;
  Result.Feedback := AFeedback;
  Invalidate;
end;

function TOBDActuatorPanel.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TOBDActuatorPanel.SetItems(AValue: TOBDActuatorCollection);
begin
  FItems.Assign(AValue);
end;

procedure TOBDActuatorPanel.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDActuatorPanel.ItemsChanged;
begin
  SetScrollPos(FScrollPos);
  Invalidate;
end;

function TOBDActuatorPanel.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FItems.Count = 0);
end;

function TOBDActuatorPanel.VisualCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_ACTUATOR_COUNT
  else
    Result := FItems.Count;
end;

function TOBDActuatorPanel.VisualName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewActuatorName(AIndex)
  else
    Result := FItems[AIndex].Name;
end;

function TOBDActuatorPanel.VisualFeedback(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewActuatorFeedback(AIndex)
  else
    Result := FItems[AIndex].Feedback;
end;

function TOBDActuatorPanel.VisualState(AIndex: Integer): TOBDActuatorTestState;
begin
  if UsePreviewData then
    Result := PreviewActuatorState(AIndex)
  else
    Result := FItems[AIndex].State;
end;

function TOBDActuatorPanel.VisualProgress(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewActuatorProgress(AIndex)
  else
    Result := FItems[AIndex].Progress;
end;

function TOBDActuatorPanel.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDActuatorPanel.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDActuatorPanel.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row + 12);
end;

function TOBDActuatorPanel.BodyRect: TRect;
begin
  Result := Rect(1, HeaderHeight, ClientWidth - 1, ClientHeight - FooterHeight);
end;

function TOBDActuatorPanel.ScrollBarRect: TRect;
begin
  Result := Rect(ClientWidth - ScaleValue(14), BodyRect.Top,
    ClientWidth - 1, BodyRect.Bottom);
end;

function TOBDActuatorPanel.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, VisualCount * RowHeight - BodyRect.Height);
end;

function TOBDActuatorPanel.ScrollThumbRect: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0, MaxScroll,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDActuatorPanel.StopAllRect: TRect;
var
  W, H: Integer;
begin
  W := OBDMeasureText('Stop all', OBD_TEXT_BODY, twSemibold,
    ScaleValue(96)) + ScaleValue(46);
  H := ScaleValue(Metrics.Button);
  Result := Rect(ClientWidth - ScaleValue(16) - W, (HeaderHeight - H) div 2,
    ClientWidth - ScaleValue(16), (HeaderHeight - H) div 2 + H);
end;

function TOBDActuatorPanel.RowButtonRect(AIndex, AButton: Integer): TRect;
var
  Y, W, H, RightX: Integer;
begin
  H := ScaleValue(Metrics.Button);
  W := ScaleValue(72);
  Y := BodyRect.Top + AIndex * RowHeight - FScrollPos + (RowHeight - H) div 2;
  RightX := ClientWidth - ScaleValue(24);
  if AButton = 0 then
    Result := Rect(RightX - 2 * W - ScaleValue(8), Y,
      RightX - W - ScaleValue(8), Y + H)
  else
    Result := Rect(RightX - W, Y, RightX, Y + H);
end;

function TOBDActuatorPanel.HitButton(X, Y: Integer; out AIndex: Integer;
  out AButton: Integer): Boolean;
var
  I, B: Integer;
  R: TRect;
begin
  Result := False;
  AIndex := -1;
  AButton := -1;
  for I := 0 to VisualCount - 1 do
    for B := 0 to 1 do
    begin
      R := RowButtonRect(I, B);
      if PtInRect(R, Point(X, Y)) then
      begin
        AIndex := I;
        AButton := B;
        Exit(True);
      end;
    end;
end;

function TOBDActuatorPanel.ActiveCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisualCount - 1 do
    if VisualState(I) = apsRunning then
      Inc(Result);
end;

function TOBDActuatorPanel.StateColor(APainter: TOBDPainter;
  AState: TOBDActuatorTestState): TColor;
begin
  case AState of
    apsRunning: Result := APainter.AccentText;
    apsDone: Result := Palette.Success;
    apsFailed: Result := Palette.Danger;
  else
    Result := Palette.Subtle;
  end;
end;

function TOBDActuatorPanel.StateText(AState: TOBDActuatorTestState): string;
begin
  case AState of
    apsRunning: Result := 'RUNNING';
    apsDone: Result := 'DONE';
    apsFailed: Result := 'FAILED';
  else
    Result := 'IDLE';
  end;
end;

procedure TOBDActuatorPanel.DrawHeader(APainter: TOBDPainter);
var
  H, X, C: Integer;
  R: TRect;
  State: TOBDControlState;
begin
  H := HeaderHeight;
  APainter.Glyph(glRoutine, ScaleValue(26), H div 2, APainter.AccentText, 1);
  X := ScaleValue(48);
  APainter.Text(X, H div 2 - ScaleValue(8), 'Actuator tests', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold);
  APainter.Text(X, H div 2 + ScaleValue(12),
    '01 Engine electronics ' + #$00B7 + ' engine off', OBD_TEXT_SMALL,
    Palette.GaugeLabel);
  C := X + APainter.TextWidth('Actuator tests', OBD_TEXT_TITLE, twBold) +
    ScaleValue(14);
  APainter.Chip(C, H div 2 - ScaleValue(10), Format('%d ACTIVE',
    [ActiveCount]), APainter.AccentText);
  R := StopAllRect;
  State := cstNormal;
  if (FHoverIndex = -2) and (FHoverButton = 2) then
    State := cstHover;
  if (FDownIndex = -2) and (FDownButton = 2) then
    State := cstPressed;
  APainter.Button(R, 'Stop all', bkDanger, State, False, glStop);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDActuatorPanel.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, Y, Mid, ProgX, ProgW, ProgH: Integer;
  RStart, RStop: TRect;
  StateStart, StateStop: TOBDControlState;
  S: string;
  C: TColor;
begin
  if VisualCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, BodyRect.Left, BodyRect.Top,
      BodyRect.Right, BodyRect.Bottom);
    for I := 0 to VisualCount - 1 do
    begin
      Y := BodyRect.Top + I * RowHeight - FScrollPos;
      if (Y + RowHeight < BodyRect.Top) or (Y > BodyRect.Bottom) then
        Continue;
      Mid := Y + RowHeight div 2;
      if VisualState(I) = apsRunning then
        APainter.FillRect(Rect(1, Y + ScaleValue(8), ScaleValue(5),
          Y + RowHeight - ScaleValue(8)), Palette.Accent);
      APainter.Text(ScaleValue(20), Mid - ScaleValue(9), VisualName(I),
        OBD_TEXT_HEADING, Palette.ForegroundText, twSemibold,
        taLeftJustify, ScaleValue(240));
      C := StateColor(APainter, VisualState(I));
      APainter.Text(ScaleValue(20), Mid + ScaleValue(10), VisualFeedback(I),
        OBD_TEXT_SMALL, C, twRegular, taLeftJustify, ScaleValue(260));
      ProgX := Round(ClientWidth * 0.46);
      ProgW := Round(ClientWidth * 0.24);
      ProgH := ScaleValue(6);
      APainter.FillRect(Rect(ProgX, Mid - ProgH div 2, ProgX + ProgW,
        Mid + ProgH div 2), Palette.NeutralLight);
      if VisualState(I) = apsRunning then
        APainter.FillRect(Rect(ProgX, Mid - ProgH div 2,
          ProgX + Round(ProgW * VisualProgress(I) / 100), Mid + ProgH div 2),
          Palette.Accent);
      S := StateText(VisualState(I));
      APainter.Chip(ProgX, Mid + ScaleValue(12), S, C, False, False,
        OBD_TEXT_CAPS);
      RStart := RowButtonRect(I, 0);
      RStop := RowButtonRect(I, 1);
      StateStart := cstNormal;
      StateStop := cstNormal;
      if (FHoverIndex = I) and (FHoverButton = 0) then
        StateStart := cstHover;
      if (FHoverIndex = I) and (FHoverButton = 1) then
        StateStop := cstHover;
      if (FDownIndex = I) and (FDownButton = 0) then
        StateStart := cstPressed;
      if (FDownIndex = I) and (FDownButton = 1) then
        StateStop := cstPressed;
      if VisualState(I) = apsRunning then
        StateStart := cstDisabled;
      if VisualState(I) <> apsRunning then
        StateStop := cstDisabled;
      APainter.Button(RStart, 'Start', bkSecondary, StateStart, False, glPlay);
      APainter.Button(RStop, 'Stop', bkDangerOutline, StateStop, False, glStop);
      APainter.HLine(1, Y + RowHeight - 1, ClientWidth - 2,
        Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDActuatorPanel.DrawFooter(APainter: TOBDPainter);
var
  FY: Integer;
begin
  FY := ClientHeight - FooterHeight;
  APainter.HLine(1, FY, ClientWidth - 2, Palette.NeutralLight);
  APainter.Text(ScaleValue(16), FY + FooterHeight div 2,
    'Tests stop after 30 s, when the engine starts or when the connection drops.',
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
    ClientWidth - ScaleValue(32));
end;

procedure TOBDActuatorPanel.DrawEmpty(APainter: TOBDPainter);
begin
  APainter.Text(ClientWidth div 2, BodyRect.Top + BodyRect.Height div 2,
    'No actuator tests', OBD_TEXT_HEADING, Palette.GaugeLabel, twSemibold,
    taCenter);
end;

procedure TOBDActuatorPanel.DrawScrollBar(APainter: TOBDPainter);
begin
  if MaxScroll > 0 then
    OBDPaintScrollBar(APainter, ScrollBarRect, True, 0, MaxScroll,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDActuatorPanel.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    P.Card(ClientRect);
    DrawHeader(P);
    DrawRows(P, ACanvas);
    DrawScrollBar(P);
    DrawFooter(P);
  finally
    P.Free;
  end;
end;

procedure TOBDActuatorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I, B: Integer;
  R, Thumb: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    R := ScrollBarRect;
    Thumb := ScrollThumbRect;
    if R.Height > Thumb.Height then
      SetScrollPos(Round((Y - R.Top - FScrollDragOffset) * MaxScroll /
        (R.Height - Thumb.Height)));
    Exit;
  end;
  if HitButton(X, Y, I, B) then
  begin
    if (FHoverIndex <> I) or (FHoverButton <> B) then
    begin
      FHoverIndex := I;
      FHoverButton := B;
      Invalidate;
    end;
  end
  else
  begin
    R := StopAllRect;
    if PtInRect(R, Point(X, Y)) then
    begin
      FHoverIndex := -2;
      FHoverButton := 2;
      Invalidate;
    end
    else if (FHoverIndex <> -1) or (FHoverButton <> -1) then
    begin
      FHoverIndex := -1;
      FHoverButton := -1;
      Invalidate;
    end;
  end;
  R := ScrollBarRect;
  if FScrollHover <> PtInRect(R, Point(X, Y)) then
  begin
    FScrollHover := not FScrollHover;
    Invalidate;
  end;
end;

procedure TOBDActuatorPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, B: Integer;
  Thumb: TRect;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  Thumb := ScrollThumbRect;
  if PtInRect(Thumb, Point(X, Y)) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    Invalidate;
    Exit;
  end;
  if HitButton(X, Y, I, B) then
  begin
    FDownIndex := I;
    FDownButton := B;
    Invalidate;
  end
  else if PtInRect(StopAllRect, Point(X, Y)) then
  begin
    FDownIndex := -2;
    FDownButton := 2;
    Invalidate;
  end;
end;

procedure TOBDActuatorPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, B: Integer;
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    Invalidate;
    Exit;
  end;
  if (FDownIndex = -2) and (FDownButton = 2) and
    PtInRect(StopAllRect, Point(X, Y)) then
  begin
    if Assigned(FOnStopAll) then
      FOnStopAll(Self);
  end
  else if HitButton(X, Y, I, B) and (I = FDownIndex) and (B = FDownButton) then
  begin
    if (B = 0) and (VisualState(I) <> apsRunning) and Assigned(FOnStart) then
      FOnStart(Self, I)
    else if (B = 1) and (VisualState(I) = apsRunning) and Assigned(FOnStop) then
      FOnStop(Self, I);
  end;
  FDownIndex := -1;
  FDownButton := -1;
  Invalidate;
end;

function TOBDActuatorPanel.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos + RowHeight);
  Result := True;
end;

function TOBDActuatorPanel.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos - RowHeight);
  Result := True;
end;

procedure TOBDActuatorPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or (FHoverButton <> -1) or FScrollHover then
  begin
    FHoverIndex := -1;
    FHoverButton := -1;
    FScrollHover := False;
    Invalidate;
  end;
end;

end.
