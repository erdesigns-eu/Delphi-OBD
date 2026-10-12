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
//    2026-10-12  ERD  Added row control kinds, locked rows and value events.
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

  /// <summary>Interactive control painted in an actuator row.</summary>
  TOBDActuatorKind = (
    /// <summary>Start button with progress and a timed run.</summary>
    akButton,
    /// <summary>On / off switch. Value is 0 or 1.</summary>
    akSwitch,
    /// <summary>Set-point slider. Value is 0..100.</summary>
    akSlider,
    /// <summary>Segmented step selector. Value is the selected step index.</summary>
    akSteps);

  /// <summary>Fires for a row action button.</summary>
  /// <param name="Sender">The actuator panel.</param>
  /// <param name="AIndex">Actuator row index.</param>
  TOBDActuatorEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>Fires when a row value control changes.</summary>
  /// <param name="Sender">The actuator panel.</param>
  /// <param name="AIndex">Actuator row index.</param>
  /// <param name="AValue">New clamped value.</param>
  TOBDActuatorValueEvent = procedure(Sender: TObject; AIndex, AValue: Integer)
    of object;

  TOBDActuatorPanel = class;

  /// <summary>One actuator-test row.</summary>
  TOBDActuatorItem = class(TCollectionItem)
  strict private
    FName: string;
    FState: TOBDActuatorTestState;
    FProgress: Integer;
    FFeedback: string;
    FKind: TOBDActuatorKind;
    FButtonCaption: string;
    FValue: Integer;
    FSteps: string;
    FLocked: Boolean;
    FLockReason: string;
    FRemainingSec: Integer;
    procedure SetName(const AValue: string);
    procedure SetState(AValue: TOBDActuatorTestState);
    procedure SetProgress(AValue: Integer);
    procedure SetFeedback(const AValue: string);
    procedure SetKind(AValue: TOBDActuatorKind);
    procedure SetButtonCaption(const AValue: string);
    procedure SetValue(AValue: Integer);
    procedure SetSteps(const AValue: string);
    procedure SetLocked(AValue: Boolean);
    procedure SetLockReason(const AValue: string);
    procedure SetRemainingSec(AValue: Integer);
    function ClampValue(AValue: Integer): Integer;
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
    /// <summary>Number of captions in <see cref="Steps"/>.</summary>
    /// <returns>Step count.</returns>
    function StepCount: Integer;
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
    /// <summary>Interactive control kind painted in the row.</summary>
    property Kind: TOBDActuatorKind read FKind write SetKind default akButton;
    /// <summary>Caption for <see cref="akButton"/> rows; empty shows Start.</summary>
    property ButtonCaption: string read FButtonCaption write SetButtonCaption;
    /// <summary>Current switch, slider or step value, clamped by <see cref="Kind"/>.</summary>
    property Value: Integer read FValue write SetValue default 0;
    /// <summary>Comma-separated captions for <see cref="akSteps"/> rows.</summary>
    property Steps: string read FSteps write SetSteps;
    /// <summary>Disables input, dims the row and shows the locked state.</summary>
    property Locked: Boolean read FLocked write SetLocked default False;
    /// <summary>Reason shown on the feedback line while <see cref="Locked"/> is True.</summary>
    property LockReason: string read FLockReason write SetLockReason;
    /// <summary>Seconds remaining for the running timer chip.</summary>
    property RemainingSec: Integer read FRemainingSec write SetRemainingSec
      default 0;
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

  /// <summary>Actuator-test card with value controls, Start, Stop and Stop all actions.</summary>
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
    FDragControlIndex: Integer;
    FOnStart: TOBDActuatorEvent;
    FOnStop: TOBDActuatorEvent;
    FOnStopAll: TNotifyEvent;
    FOnValueChange: TOBDActuatorValueEvent;
    procedure SetItems(AValue: TOBDActuatorCollection);
    procedure SetScrollPos(AValue: Integer);
    function UsePreviewData: Boolean;
    function VisualCount: Integer;
    function VisualName(AIndex: Integer): string;
    function VisualFeedback(AIndex: Integer): string;
    function VisualState(AIndex: Integer): TOBDActuatorTestState;
    function VisualProgress(AIndex: Integer): Integer;
    function VisualKind(AIndex: Integer): TOBDActuatorKind;
    function VisualButtonCaption(AIndex: Integer): string;
    function VisualValue(AIndex: Integer): Integer;
    function VisualSteps(AIndex: Integer): string;
    function VisualLocked(AIndex: Integer): Boolean;
    function VisualLockReason(AIndex: Integer): string;
    function VisualRemainingSec(AIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollBarRect: TRect;
    function MaxScroll: Integer;
    function ScrollThumbRect: TRect;
    function StopAllRect: TRect;
    function RowButtonRect(AIndex, AButton: Integer): TRect;
    function ControlRect(AIndex: Integer): TRect;
    function HitButton(X, Y: Integer; out AIndex: Integer;
      out AButton: Integer): Boolean;
    function HitControl(X, Y: Integer; out AIndex: Integer): Boolean;
    function ActiveCount: Integer;
    function StateColor(APainter: TOBDPainter;
      AState: TOBDActuatorTestState): TColor;
    function StateText(AState: TOBDActuatorTestState): string;
    function TimerText(ARemainingSec: Integer): string;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure DrawValueControl(APainter: TOBDPainter; AIndex, AMid: Integer;
      const R: TRect; AEnabled, AHover: Boolean);
    procedure DrawSlider(APainter: TOBDPainter; const R: TRect; AMid,
      AValue: Integer; AEnabled, AHover: Boolean);
    procedure DrawSteps(APainter: TOBDPainter; const R: TRect; AMid: Integer;
      const ASteps: string; AValue: Integer; AEnabled, AHover: Boolean);
    procedure UpdateSliderFromPoint(AIndex, X: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the actuator-test card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks action-button hover, value dragging and scroll dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Presses action buttons, value controls or starts scroll dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Fires a clicked action button or value control.</summary>
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
    /// <summary>Notifies the panel that its actuator collection changed.</summary>
    procedure ItemsChanged;
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
    /// <summary>Adds one button-style actuator test row.</summary>
    /// <param name="AName">Actuator name.</param>
    /// <param name="AFeedback">Initial feedback text.</param>
    /// <returns>The new row.</returns>
    function AddTest(const AName, AFeedback: string): TOBDActuatorItem;
    /// <summary>Adds one actuator row with the requested control kind.</summary>
    /// <param name="AName">Actuator name.</param>
    /// <param name="AFeedback">Initial feedback text.</param>
    /// <param name="AKind">Control kind.</param>
    /// <returns>The new row.</returns>
    function AddControl(const AName, AFeedback: string;
      AKind: TOBDActuatorKind): TOBDActuatorItem;
    /// <summary>Number of actuator rows.</summary>
    /// <returns>Row count.</returns>
    function Count: Integer;
    /// <summary>Sets a row control value, fires OnValueChange and starts an idle row when non-zero.</summary>
    /// <param name="AIndex">Row index.</param>
    /// <param name="AValue">Requested value before clamping.</param>
    procedure SetControlValue(AIndex, AValue: Integer);
    /// <summary>Activates the row control as if clicked; locked rows ignore the call.</summary>
    /// <param name="AIndex">Row index.</param>
    procedure ActivateControl(AIndex: Integer);
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
    /// <summary>Fires when a switch, slider or step selector changes value.</summary>
    property OnValueChange: TOBDActuatorValueEvent read FOnValueChange
      write FOnValueChange;
  end;

implementation

const
  PREVIEW_ACTUATOR_COUNT = 6;

function ActuatorStepCount(const AText: string): Integer;
var
  I: Integer;
begin
  if Trim(AText) = '' then
    Exit(0);
  Result := 1;
  for I := 1 to Length(AText) do
    if AText[I] = ',' then
      Inc(Result);
end;

function ActuatorStepCaption(const AText: string; AIndex: Integer): string;
var
  I, N, Start: Integer;
begin
  Result := '';
  Start := 1;
  N := 0;
  for I := 1 to Length(AText) + 1 do
    if (I > Length(AText)) or (AText[I] = ',') then
    begin
      if N = AIndex then
      begin
        Result := Trim(Copy(AText, Start, I - Start));
        Break;
      end;
      Inc(N);
      Start := I + 1;
    end;
  if Result = '' then
    Result := 'Step ' + IntToStr(AIndex + 1);
end;

function PreviewActuatorName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Radiator fan';
    1: Result := 'Coolant pump';
    2: Result := 'Fuel pump relay';
    3: Result := 'Glow plugs';
    4: Result := 'EGR valve';
  else
    Result := 'Starter motor';
  end;
end;

function PreviewActuatorFeedback(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Fan speed 2 140 rpm';
    1: Result := 'Flow 12 l/min';
    2: Result := 'Relay on, 4.8 A';
    3: Result := 'No current on cylinder 3';
    4: Result := 'Position 2 %';
  else
    Result := 'Not available while the engine runs';
  end;
end;

function PreviewActuatorState(AIndex: Integer): TOBDActuatorTestState;
begin
  case AIndex of
    0, 1: Result := apsRunning;
    2: Result := apsDone;
    3: Result := apsFailed;
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

function PreviewActuatorKind(AIndex: Integer): TOBDActuatorKind;
begin
  case AIndex of
    0, 4: Result := akSlider;
    1: Result := akSteps;
    2: Result := akSwitch;
  else
    Result := akButton;
  end;
end;

function PreviewActuatorValue(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := 60;
    1: Result := 1;
    2: Result := 1;
  else
    Result := 0;
  end;
end;

function PreviewActuatorButtonCaption(AIndex: Integer): string;
begin
  case AIndex of
    3: Result := 'Run 10 s';
    5: Result := 'Crank 3 s';
  else
    Result := '';
  end;
end;

function PreviewActuatorRemainingSec(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := 24;
    1: Result := 8;
  else
    Result := 0;
  end;
end;

{ TOBDActuatorItem }

constructor TOBDActuatorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := apsIdle;
  FKind := akButton;
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
    FKind := Item.Kind;
    FButtonCaption := Item.ButtonCaption;
    FValue := Item.Value;
    FSteps := Item.Steps;
    FLocked := Item.Locked;
    FLockReason := Item.LockReason;
    FRemainingSec := Item.RemainingSec;
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

function TOBDActuatorItem.ClampValue(AValue: Integer): Integer;
begin
  case FKind of
    akSwitch:
      Result := EnsureRange(AValue, 0, 1);
    akSlider:
      Result := EnsureRange(AValue, 0, 100);
    akSteps:
      begin
        if StepCount > 0 then
          Result := EnsureRange(AValue, 0, StepCount - 1)
        else
          Result := 0;
      end;
  else
    Result := EnsureRange(AValue, 0, 100);
  end;
end;

function TOBDActuatorItem.StepCount: Integer;
begin
  Result := ActuatorStepCount(FSteps);
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

procedure TOBDActuatorItem.SetKind(AValue: TOBDActuatorKind);
var
  NewValue: Integer;
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  NewValue := ClampValue(FValue);
  if FValue <> NewValue then
    FValue := NewValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetButtonCaption(const AValue: string);
begin
  if FButtonCaption = AValue then
    Exit;
  FButtonCaption := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetValue(AValue: Integer);
begin
  AValue := ClampValue(AValue);
  if FValue = AValue then
    Exit;
  FValue := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetSteps(const AValue: string);
var
  NewValue: Integer;
begin
  if FSteps = AValue then
    Exit;
  FSteps := AValue;
  NewValue := ClampValue(FValue);
  if FValue <> NewValue then
    FValue := NewValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetLocked(AValue: Boolean);
begin
  if FLocked = AValue then
    Exit;
  FLocked := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetLockReason(const AValue: string);
begin
  if FLockReason = AValue then
    Exit;
  FLockReason := AValue;
  Changed(False);
end;

procedure TOBDActuatorItem.SetRemainingSec(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FRemainingSec = AValue then
    Exit;
  FRemainingSec := AValue;
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
  FDragControlIndex := -1;
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
  Result := AddControl(AName, AFeedback, akButton);
end;

function TOBDActuatorPanel.AddControl(const AName, AFeedback: string;
  AKind: TOBDActuatorKind): TOBDActuatorItem;
begin
  Result := FItems.Add;
  Result.Name := AName;
  Result.Feedback := AFeedback;
  Result.Kind := AKind;
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
  else if FItems[AIndex].Locked and (FItems[AIndex].LockReason <> '') then
    Result := FItems[AIndex].LockReason
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

function TOBDActuatorPanel.VisualKind(AIndex: Integer): TOBDActuatorKind;
begin
  if UsePreviewData then
    Result := PreviewActuatorKind(AIndex)
  else
    Result := FItems[AIndex].Kind;
end;

function TOBDActuatorPanel.VisualButtonCaption(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewActuatorButtonCaption(AIndex)
  else
    Result := FItems[AIndex].ButtonCaption;
  if Result = '' then
    Result := 'Start';
end;

function TOBDActuatorPanel.VisualValue(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewActuatorValue(AIndex)
  else
    Result := FItems[AIndex].Value;
end;

function TOBDActuatorPanel.VisualSteps(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Off,Low,High'
  else
    Result := FItems[AIndex].Steps;
end;

function TOBDActuatorPanel.VisualLocked(AIndex: Integer): Boolean;
begin
  if UsePreviewData then
    Result := AIndex = 5
  else
    Result := FItems[AIndex].Locked;
end;

function TOBDActuatorPanel.VisualLockReason(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Not available while the engine runs'
  else
    Result := FItems[AIndex].LockReason;
end;

function TOBDActuatorPanel.VisualRemainingSec(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewActuatorRemainingSec(AIndex)
  else
    Result := FItems[AIndex].RemainingSec;
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
  Result := System.Math.Max(ScaleValue(44), ScaleValue(Metrics.Row + 12));
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

function TOBDActuatorPanel.ControlRect(AIndex: Integer): TRect;
var
  Y, H, L, W: Integer;
begin
  H := ScaleValue(Metrics.Button);
  L := Round(ClientWidth * 0.38);
  W := Round(ClientWidth * 0.26);
  if W < ScaleValue(120) then
    W := ScaleValue(120);
  Y := BodyRect.Top + AIndex * RowHeight - FScrollPos + (RowHeight - H) div 2;
  Result := Rect(L, Y, L + W, Y + H);
end;

function TOBDActuatorPanel.RowButtonRect(AIndex, AButton: Integer): TRect;
var
  Y, W, H, RightX, TimerW: Integer;
begin
  H := ScaleValue(Metrics.Button);
  Y := BodyRect.Top + AIndex * RowHeight - FScrollPos + (RowHeight - H) div 2;
  if AButton = 0 then
    Exit(ControlRect(AIndex));
  W := ScaleValue(72);
  RightX := ClientWidth - ScaleValue(24);
  TimerW := 0;
  if VisualState(AIndex) = apsRunning then
    TimerW := ScaleValue(64);
  Result := Rect(RightX - W, Y, RightX, Y + H);
  if (AButton = 1) and (TimerW > 0) then
    OffsetRect(Result, 0, 0);
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
      if (B = 0) and (VisualKind(I) <> akButton) then
        Continue;
      R := RowButtonRect(I, B);
      if PtInRect(R, Point(X, Y)) then
      begin
        AIndex := I;
        AButton := B;
        Exit(True);
      end;
    end;
end;

function TOBDActuatorPanel.HitControl(X, Y: Integer; out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  AIndex := -1;
  if UsePreviewData then
    Exit;
  for I := 0 to VisualCount - 1 do
  begin
    if VisualKind(I) = akButton then
      Continue;
    if VisualLocked(I) then
      Continue;
    if PtInRect(ControlRect(I), Point(X, Y)) then
    begin
      AIndex := I;
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
    apsFailed: Result := APainter.DangerInk;
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

function TOBDActuatorPanel.TimerText(ARemainingSec: Integer): string;
begin
  ARemainingSec := System.Math.Max(0, ARemainingSec);
  Result := Format('%.2d:%.2d', [ARemainingSec div 60, ARemainingSec mod 60]);
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
    '01 Engine electronics ' + #$00B7 + ' ignition on', OBD_TEXT_SMALL,
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

procedure TOBDActuatorPanel.DrawSlider(APainter: TOBDPainter; const R: TRect;
  AMid, AValue: Integer; AEnabled, AHover: Boolean);
var
  Track, Fill: TRect;
  ThumbX, LabelX, TextW: Integer;
  TrackColor, FillColor, ThumbColor: TColor;
  S: string;
begin
  Track := Rect(R.Left, AMid - ScaleValue(3), R.Right - ScaleValue(48),
    AMid + ScaleValue(3));
  TrackColor := Palette.NeutralLight;
  FillColor := APainter.AccentText;
  ThumbColor := Palette.GaugeFace;
  if not AEnabled then
  begin
    FillColor := Palette.Subtle;
    ThumbColor := Palette.NeutralLight;
  end;
  APainter.RoundRect(Track.Left, Track.Top, Track.Width, Track.Height,
    ScaleValue(3), TrackColor, clNone);
  Fill := Track;
  Fill.Right := Track.Left + Round(Track.Width * EnsureRange(AValue, 0, 100) / 100);
  APainter.RoundRect(Fill.Left, Fill.Top, Fill.Width, Fill.Height,
    ScaleValue(3), FillColor, clNone);
  ThumbX := Fill.Right;
  APainter.Ellipse(ThumbX - ScaleValue(7), AMid - ScaleValue(7),
    ScaleValue(14), ScaleValue(14), ThumbColor,
    IfThen(AHover and AEnabled, APainter.AccentText, FillColor), ScaleValue(1));
  S := Format('%d %%', [EnsureRange(AValue, 0, 100)]);
  TextW := APainter.TextWidth(S, OBD_TEXT_BODY, twSemibold);
  LabelX := R.Right - TextW;
  APainter.Text(LabelX, AMid, S, OBD_TEXT_BODY,
    IfThen(AEnabled, Palette.ForegroundText, APainter.DisabledText),
    twSemibold);
end;

procedure TOBDActuatorPanel.DrawSteps(APainter: TOBDPainter; const R: TRect;
  AMid: Integer; const ASteps: string; AValue: Integer; AEnabled,
  AHover: Boolean);
var
  I, Count, SegW, H, X: Integer;
  Fill, Outline, Ink: TColor;
  S: string;
  SR: TRect;
begin
  Count := ActuatorStepCount(ASteps);
  if Count <= 0 then
    Count := 1;
  H := ScaleValue(Metrics.Segment);
  APainter.RoundRect(R.Left, AMid - H div 2, R.Width, H, ScaleValue(6),
    Palette.GaugeFace, Palette.NeutralLight);
  SegW := System.Math.Max(1, R.Width div Count);
  for I := 0 to Count - 1 do
  begin
    X := R.Left + I * SegW;
    if I = Count - 1 then
      SR := Rect(X, AMid - H div 2, R.Right, AMid + H div 2)
    else
      SR := Rect(X, AMid - H div 2, X + SegW, AMid + H div 2);
    if I = EnsureRange(AValue, 0, Count - 1) then
    begin
      Fill := IfThen(AEnabled, APainter.AccentText, Palette.NeutralLight);
      Outline := Fill;
      Ink := IfThen(AEnabled, APainter.OnAccent, Palette.Subtle);
      APainter.RoundRect(SR.Left + ScaleValue(2), SR.Top + ScaleValue(2),
        SR.Width - ScaleValue(4), SR.Height - ScaleValue(4), ScaleValue(4),
        Fill, Outline);
    end
    else
      Ink := IfThen(AEnabled, Palette.GaugeLabel, APainter.DisabledText);
    if I > 0 then
      APainter.VLine(SR.Left, SR.Top + ScaleValue(5), SR.Height - ScaleValue(10),
        Palette.NeutralLight);
    S := ActuatorStepCaption(ASteps, I);
    APainter.Text(SR.Left + SR.Width div 2, AMid, S, OBD_TEXT_SMALL, Ink,
      twSemibold, taCenter, SR.Width - ScaleValue(6));
  end;
end;

procedure TOBDActuatorPanel.DrawValueControl(APainter: TOBDPainter; AIndex,
  AMid: Integer; const R: TRect; AEnabled, AHover: Boolean);
var
  State: TOBDControlState;
  Caption: string;
  TrackW, ProgW: Integer;
  Prog: TRect;
begin
  case VisualKind(AIndex) of
    akSlider:
      DrawSlider(APainter, R, AMid, VisualValue(AIndex), AEnabled, AHover);
    akSwitch:
      APainter.Switch(R.Left, AMid, Metrics.Segment, VisualValue(AIndex) <> 0,
        AEnabled, False);
    akSteps:
      DrawSteps(APainter, R, AMid, VisualSteps(AIndex), VisualValue(AIndex),
        AEnabled, AHover);
  else
    begin
      State := cstNormal;
      if not AEnabled or (VisualState(AIndex) = apsRunning) then
        State := cstDisabled
      else if AHover then
        State := cstHover;
      if (FDownIndex = AIndex) and (FDownButton = 0) then
        State := cstPressed;
      Caption := VisualButtonCaption(AIndex);
      APainter.Button(R, Caption, bkSecondary, State, False, glPlay);
      if VisualState(AIndex) = apsRunning then
      begin
        TrackW := R.Width;
        Prog := Rect(R.Left, R.Bottom + ScaleValue(5), R.Left + TrackW,
          R.Bottom + ScaleValue(10));
        APainter.FillRect(Prog, Palette.NeutralLight);
        ProgW := Round(TrackW * VisualProgress(AIndex) / 100);
        APainter.FillRect(Rect(Prog.Left, Prog.Top, Prog.Left + ProgW,
          Prog.Bottom), Palette.Accent);
      end;
    end;
  end;
end;

procedure TOBDActuatorPanel.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, Y, Mid, NameW, RightX, ChipW: Integer;
  RControl, RStop: TRect;
  StateStop: TOBDControlState;
  S: string;
  C, Ink: TColor;
  EnabledRow, HoverControl: Boolean;
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
      EnabledRow := not VisualLocked(I);
      if VisualState(I) = apsRunning then
        APainter.FillRect(Rect(1, Y + ScaleValue(8), ScaleValue(5),
          Y + RowHeight - ScaleValue(8)), Palette.Accent);
      NameW := Round(ClientWidth * 0.34) - ScaleValue(28);
      Ink := IfThen(EnabledRow, Palette.ForegroundText, APainter.DisabledText);
      APainter.Text(ScaleValue(20), Mid - ScaleValue(9), VisualName(I),
        OBD_TEXT_HEADING, Ink, twSemibold, taLeftJustify, NameW);
      if VisualLocked(I) then
      begin
        APainter.Glyph(glLock, ScaleValue(28), Mid + ScaleValue(10),
          Palette.Subtle, 0.8);
        APainter.Text(ScaleValue(40), Mid + ScaleValue(10),
          VisualLockReason(I), OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular,
          taLeftJustify, NameW - ScaleValue(20));
      end
      else
      begin
        C := StateColor(APainter, VisualState(I));
        APainter.Text(ScaleValue(20), Mid + ScaleValue(10), VisualFeedback(I),
          OBD_TEXT_SMALL, C, twRegular, taLeftJustify, NameW);
      end;
      RControl := ControlRect(I);
      HoverControl := (FHoverIndex = I) and (FHoverButton in [0, 3]);
      DrawValueControl(APainter, I, Mid, RControl, EnabledRow, HoverControl);
      RightX := ClientWidth - ScaleValue(24);
      if VisualLocked(I) then
      begin
        ChipW := APainter.ChipWidth('LOCKED', False, OBD_TEXT_CAPS);
        APainter.Chip(RightX - ChipW, Mid - ScaleValue(10), 'LOCKED',
          Palette.Subtle, False, False, OBD_TEXT_CAPS);
      end
      else if VisualState(I) = apsRunning then
      begin
        RStop := RowButtonRect(I, 1);
        StateStop := cstNormal;
        if (FHoverIndex = I) and (FHoverButton = 1) then
          StateStop := cstHover;
        if (FDownIndex = I) and (FDownButton = 1) then
          StateStop := cstPressed;
        APainter.Button(RStop, 'Stop', bkDangerOutline, StateStop, False, glStop);
        S := TimerText(VisualRemainingSec(I));
        ChipW := APainter.ChipWidth(S, True, OBD_TEXT_CAPS);
        APainter.Chip(RStop.Left - ScaleValue(8) - ChipW, Mid - ScaleValue(10),
          S, APainter.AccentText, False, True, OBD_TEXT_CAPS);
      end
      else if VisualState(I) in [apsDone, apsFailed] then
      begin
        S := StateText(VisualState(I));
        C := StateColor(APainter, VisualState(I));
        ChipW := APainter.ChipWidth(S, False, OBD_TEXT_CAPS);
        APainter.Chip(RightX - ChipW, Mid - ScaleValue(10), S, C, False,
          False, OBD_TEXT_CAPS);
      end;
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
    'Moving a slider, a step or a switch starts an idle test; tests stop after 30 s or when the connection drops.',
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

procedure TOBDActuatorPanel.SetControlValue(AIndex, AValue: Integer);
var
  Item: TOBDActuatorItem;
  OldValue: Integer;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  Item := FItems[AIndex];
  if Item.Locked then
    Exit;
  OldValue := Item.Value;
  Item.Value := AValue;
  if OldValue <> Item.Value then
  begin
    if Assigned(FOnValueChange) then
      FOnValueChange(Self, AIndex, Item.Value);
    if (Item.State = apsIdle) and (Item.Value <> 0) and Assigned(FOnStart) then
      FOnStart(Self, AIndex);
  end;
end;

procedure TOBDActuatorPanel.ActivateControl(AIndex: Integer);
var
  Item: TOBDActuatorItem;
  StepTotal: Integer;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  Item := FItems[AIndex];
  if Item.Locked then
    Exit;
  case Item.Kind of
    akButton:
      if (Item.State <> apsRunning) and Assigned(FOnStart) then
        FOnStart(Self, AIndex);
    akSwitch:
      SetControlValue(AIndex, 1 - EnsureRange(Item.Value, 0, 1));
    akSteps:
      begin
        StepTotal := Item.StepCount;
        if StepTotal > 0 then
          SetControlValue(AIndex, (Item.Value + 1) mod StepTotal);
      end;
    akSlider:
      if Item.Value = 0 then
        SetControlValue(AIndex, 100);
  end;
end;

procedure TOBDActuatorPanel.UpdateSliderFromPoint(AIndex, X: Integer);
var
  R: TRect;
  W, Value: Integer;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  R := ControlRect(AIndex);
  R.Right := R.Right - ScaleValue(48);
  W := System.Math.Max(1, R.Width);
  Value := EnsureRange(Round((X - R.Left) * 100 / W), 0, 100);
  SetControlValue(AIndex, Value);
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
  if FDragControlIndex >= 0 then
  begin
    UpdateSliderFromPoint(FDragControlIndex, X);
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
  else if HitControl(X, Y, I) then
  begin
    if (FHoverIndex <> I) or (FHoverButton <> 3) then
    begin
      FHoverIndex := I;
      FHoverButton := 3;
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
  else if HitControl(X, Y, I) then
  begin
    FDownIndex := I;
    FDownButton := 3;
    if VisualKind(I) = akSlider then
    begin
      FDragControlIndex := I;
      UpdateSliderFromPoint(I, X);
    end;
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
  I, B, StepTotal, Value: Integer;
  R: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    Invalidate;
    Exit;
  end;
  if FDragControlIndex >= 0 then
  begin
    FDragControlIndex := -1;
    FDownIndex := -1;
    FDownButton := -1;
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
    if (B = 0) and (VisualKind(I) = akButton) and not VisualLocked(I) and
      (VisualState(I) <> apsRunning) and Assigned(FOnStart) then
      FOnStart(Self, I)
    else if (B = 1) and not VisualLocked(I) and (VisualState(I) = apsRunning) and
      Assigned(FOnStop) then
      FOnStop(Self, I);
  end
  else if (FDownButton = 3) and HitControl(X, Y, I) and (I = FDownIndex) then
  begin
    if VisualKind(I) = akSteps then
    begin
      R := ControlRect(I);
      StepTotal := ActuatorStepCount(VisualSteps(I));
      if StepTotal > 0 then
      begin
        Value := EnsureRange((X - R.Left) * StepTotal div System.Math.Max(1,
          R.Width), 0, StepTotal - 1);
        SetControlValue(I, Value);
      end;
    end
    else
      ActivateControl(I);
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
