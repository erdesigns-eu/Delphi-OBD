//------------------------------------------------------------------------------
//  OBD.UI.FlashDashboards
//
//  Flash-control dashboards for the A2 inventory:
//
//    TOBDFlashSafetyDashboard  Pre-flight checklist. Each row
//                              is a named gate with Pass /
//                              Warn / Fail state. The "Arm"
//                              button only enables when every
//                              required gate is green.
//    TOBDFlashCheckpointTimeline Horizontal timeline of
//                              checkpoint events: created /
//                              restored / rolled-back.
//                              Markers along a baseline; host
//                              pushes events via PushEvent.
//    TOBDFlashAuditTail        TListView tail of the most
//                              recent audit entries (kind +
//                              target + message + timestamp).
//                              Capped capacity; overflow drops
//                              the oldest row.
//
//  Theme- / HiDPI- / VCL-Style-aware via TOBDCustomControl
//  and TListView. Every state mutation routes through
//  TBindings.Notify; csDesigning guards the event-driven
//  paths.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.FlashDashboards;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  System.DateUtils,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control,
  OBD.Coding.AuditLog;

type
  /// <summary>Pre-flight gate state.</summary>
  TOBDFlashGateState = (
    /// <summary>Not yet evaluated.</summary>
    fgsUnknown,
    /// <summary>Green — gate met.</summary>
    fgsPass,
    /// <summary>Amber — soft warning, gate still met.</summary>
    fgsWarning,
    /// <summary>Red — gate failed; arming blocked.</summary>
    fgsFail
  );

  /// <summary>One pre-flight gate.</summary>
  TOBDFlashGate = record
    /// <summary>Short name shown on the dashboard row.</summary>
    Name:     string;
    /// <summary>Free-text description / value (e.g.
    /// <c>"12.6 V"</c> for the voltage gate).</summary>
    Detail:   string;
    /// <summary>Current state. Default <c>fgsUnknown</c>.</summary>
    State:    TOBDFlashGateState;
    /// <summary>True when the gate must pass for arming. False
    /// turns the gate into an info-only row.</summary>
    Required: Boolean;
  end;

  /// <summary>Fires when the user clicks the Arm button while
  /// every required gate is green.</summary>
  TOBDFlashArmEvent = procedure(Sender: TObject) of object;

  /// <summary>Pre-flight safety dashboard. Drop on a form,
  /// seed gates with <see cref="AddGate"/> (or accept the
  /// built-in voltage / temp / battery / image / signature
  /// defaults), then call <see cref="UpdateGate"/> from your
  /// preflight handlers. The Arm button only fires
  /// <see cref="OnArm"/> when every required gate is in
  /// <c>fgsPass</c>.</summary>
  TOBDFlashSafetyDashboard = class(TOBDCustomControl)
  strict private
    FGates:        TList<TOBDFlashGate>;
    FArmRect:      TRect;
    FHoverArm:     Boolean;
    FCaptionFont:  TFont;
    FValueFont:    TFont;
    FOnArm:        TOBDFlashArmEvent;
    procedure SetCaptionFont(AValue: TFont);
    procedure SetValueFont(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure SeedDefaults;
    function  ArmEnabled: Boolean;
    function  IndexOfGate(const AName: string): Integer;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseLeave(var Message: TMessage);
      message CM_MOUSELEAVE;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Adds (or replaces by name) a gate.</summary>
    procedure AddGate(const AName, ADetail: string;
      AState: TOBDFlashGateState; ARequired: Boolean = True);
    /// <summary>Updates a gate's state and detail. Adds it if
    /// no gate with that name exists yet.</summary>
    procedure UpdateGate(const AName, ADetail: string;
      AState: TOBDFlashGateState);
    /// <summary>Removes every gate.</summary>
    procedure ClearGates;
    /// <summary>Snapshot of the current gate list.</summary>
    function  Gates: TArray<TOBDFlashGate>;
    /// <summary>True when every required gate is in
    /// <c>fgsPass</c>.</summary>
    property Armed: Boolean read ArmEnabled;
  published
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
    property ValueFont: TFont
      read FValueFont write SetValueFont;
    /// <summary>Fires on Arm-button click when
    /// <see cref="Armed"/> is True.</summary>
    property OnArm: TOBDFlashArmEvent read FOnArm write FOnArm;
  end;

  /// <summary>Checkpoint event kind.</summary>
  TOBDCheckpointKind = (
    ckCreated,
    ckRestored,
    ckRolledBack
  );

  /// <summary>One checkpoint event for the timeline.</summary>
  TOBDCheckpointEvent = record
    Timestamp: TDateTime;
    Kind:      TOBDCheckpointKind;
    Label_:    string;
  end;

  /// <summary>Horizontal timeline of checkpoint events.
  /// Markers slide along a baseline from left (oldest) to
  /// right (newest); shape + colour reflect the kind.</summary>
  TOBDFlashCheckpointTimeline = class(TOBDCustomControl)
  strict private
    FEvents:      TList<TOBDCheckpointEvent>;
    FCapacity:    Integer;
    FCaptionFont: TFont;
    procedure SetCaptionFont(AValue: TFont);
    procedure SetCapacity(AValue: Integer);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  KindColor(AKind: TOBDCheckpointKind): TColor;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Appends one event. Old events drop when the
    /// ring fills.</summary>
    procedure PushEvent(AKind: TOBDCheckpointKind;
      const ALabel: string);
    /// <summary>Clears every event.</summary>
    procedure ClearEvents;
    /// <summary>Snapshot of the stored events.</summary>
    function  Events: TArray<TOBDCheckpointEvent>;
    /// <summary>Number of events currently stored.</summary>
    function  EventCount: Integer;
  published
    /// <summary>Maximum events kept on the timeline. Default
    /// 64.</summary>
    property Capacity: Integer
      read FCapacity write SetCapacity default 64;
    property CaptionFont: TFont
      read FCaptionFont write SetCaptionFont;
  end;

  /// <summary>TListView tail of the most recent audit
  /// entries. Host calls <see cref="PushEntry"/> from a
  /// TOBDCodingAuditLog handler; the tail caps at
  /// <see cref="MaxRows"/> (oldest row drops on overflow).
  /// </summary>
  TOBDFlashAuditTail = class(TListView)
  strict private
    FMaxRows: Integer;
    procedure SetMaxRows(AValue: Integer);
    procedure NotifyBindings;
    function  KindText(AKind: TOBDAuditKind): string;
    function  KindColor(AKind: TOBDAuditKind): TColor;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Appends one entry. Drops the oldest row when
    /// the row count exceeds <see cref="MaxRows"/>.</summary>
    procedure PushEntry(const AEntry: TOBDAuditEntry);
    /// <summary>Empties the tail.</summary>
    procedure ClearTail;
  published
    /// <summary>Row cap. Default 200.</summary>
    property MaxRows: Integer
      read FMaxRows write SetMaxRows default 200;

    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

implementation

procedure ConfigureColumns(AListView: TListView;
  const ANames: array of string;
  const AWidths: array of Integer);
var
  I: Integer;
  Col: TListColumn;
begin
  AListView.Columns.BeginUpdate;
  try
    AListView.Columns.Clear;
    for I := 0 to High(ANames) do
    begin
      Col := AListView.Columns.Add;
      Col.Caption := ANames[I];
      Col.Width   := AWidths[I];
    end;
  finally
    AListView.Columns.EndUpdate;
  end;
end;

{ ---- TOBDFlashSafetyDashboard ------------------------------------------- }

constructor TOBDFlashSafetyDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 220;
  FGates := TList<TOBDFlashGate>.Create;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 9;
  FCaptionFont.OnChange := HandleFontChange;
  FValueFont := TFont.Create;
  FValueFont.Name := 'Segoe UI';
  FValueFont.Size := 10;
  FValueFont.Style := [fsBold];
  FValueFont.OnChange := HandleFontChange;
  SeedDefaults;
end;

destructor TOBDFlashSafetyDashboard.Destroy;
begin
  FGates.Free;
  FCaptionFont.Free;
  FValueFont.Free;
  inherited;
end;

procedure TOBDFlashSafetyDashboard.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFlashSafetyDashboard.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDFlashSafetyDashboard.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDFlashSafetyDashboard.SetValueFont(AValue: TFont);
begin
  FValueFont.Assign(AValue);
end;

procedure TOBDFlashSafetyDashboard.SeedDefaults;

  procedure G(const AName: string);
  var
    Gate: TOBDFlashGate;
  begin
    Gate.Name     := AName;
    Gate.Detail   := '';
    Gate.State    := fgsUnknown;
    Gate.Required := True;
    FGates.Add(Gate);
  end;

begin
  G('Voltage');
  G('Temperature');
  G('Battery');
  G('Image');
  G('Signature');
end;

function TOBDFlashSafetyDashboard.IndexOfGate(
  const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FGates.Count - 1 do
    if SameText(FGates[I].Name, AName) then Exit(I);
  Result := -1;
end;

procedure TOBDFlashSafetyDashboard.AddGate(const AName, ADetail: string;
  AState: TOBDFlashGateState; ARequired: Boolean);
var
  Idx: Integer;
  Gate: TOBDFlashGate;
begin
  Gate.Name := AName;
  Gate.Detail := ADetail;
  Gate.State := AState;
  Gate.Required := ARequired;
  Idx := IndexOfGate(AName);
  if Idx >= 0 then FGates[Idx] := Gate
  else             FGates.Add(Gate);
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashSafetyDashboard.UpdateGate(const AName, ADetail: string;
  AState: TOBDFlashGateState);
var
  Idx: Integer;
  Gate: TOBDFlashGate;
begin
  Idx := IndexOfGate(AName);
  if Idx < 0 then
  begin
    AddGate(AName, ADetail, AState, True);
    Exit;
  end;
  Gate := FGates[Idx];
  Gate.Detail := ADetail;
  Gate.State := AState;
  FGates[Idx] := Gate;
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashSafetyDashboard.ClearGates;
begin
  FGates.Clear;
  NotifyBindings;
  Repaint;
end;

function TOBDFlashSafetyDashboard.Gates: TArray<TOBDFlashGate>;
begin
  Result := FGates.ToArray;
end;

function TOBDFlashSafetyDashboard.ArmEnabled: Boolean;
var
  I: Integer;
begin
  if FGates.Count = 0 then Exit(False);
  for I := 0 to FGates.Count - 1 do
    if FGates[I].Required and (FGates[I].State <> fgsPass) then
      Exit(False);
  Result := True;
end;

procedure TOBDFlashSafetyDashboard.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  WasHover: Boolean;
begin
  inherited;
  WasHover := FHoverArm;
  FHoverArm := PtInRect(FArmRect, Point(X, Y));
  if WasHover <> FHoverArm then Repaint;
end;

procedure TOBDFlashSafetyDashboard.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if not ArmEnabled then Exit;
  if not PtInRect(FArmRect, Point(X, Y)) then Exit;
  if Assigned(FOnArm) then
    try
      FOnArm(Self);
    except
    end;
end;

procedure TOBDFlashSafetyDashboard.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverArm then
  begin
    FHoverArm := False;
    Repaint;
  end;
end;

procedure TOBDFlashSafetyDashboard.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, RowH, Y, I: Integer;
  Gate: TOBDFlashGate;
  LampColor, BodyCol: TColor;
  LampSize: Integer;
  Lamp:   TGPRectF;
  ButtonRect: TGPRectF;
  ArmCaption: string;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(10);
  RowH := ScaleValue(22);
  LampSize := ScaleValue(12);
  Y := Pad;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to FGates.Count - 1 do
    begin
      Gate := FGates[I];
      case Gate.State of
        fgsPass:    LampColor := Palette.Success;
        fgsWarning: LampColor := Palette.Warning;
        fgsFail:    LampColor := Palette.Danger;
      else
        LampColor := Palette.NeutralLight;
      end;
      Lamp.X := Pad;
      Lamp.Y := Y + (RowH - LampSize) div 2;
      Lamp.Width  := LampSize;
      Lamp.Height := LampSize;
      Brush := TGPSolidBrush.Create(ColorToARGB(LampColor));
      try
        Graphics.FillEllipse(Brush, Lamp);
      finally
        Brush.Free;
      end;
      Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
      try
        Graphics.DrawEllipse(Pen, Lamp);
      finally
        Pen.Free;
      end;

      ACanvas.Font := FValueFont;
      ACanvas.Font.Color := EffectiveForeground;
      ACanvas.TextOut(Pad + LampSize + ScaleValue(8),
        Y + (RowH - ACanvas.TextHeight('Mg')) div 2,
        Gate.Name);

      ACanvas.Font := FCaptionFont;
      ACanvas.Font.Color := EffectiveForeground;
      if Gate.Detail <> '' then
        ACanvas.TextOut(Width - Pad -
          ACanvas.TextWidth(Gate.Detail),
          Y + (RowH - ACanvas.TextHeight(Gate.Detail)) div 2,
          Gate.Detail);

      Y := Y + RowH;
    end;

    // Arm button.
    ButtonRect.X := Pad;
    ButtonRect.Y := Height - Pad - ScaleValue(36);
    ButtonRect.Width  := Width - 2 * Pad;
    ButtonRect.Height := ScaleValue(32);
    FArmRect := Rect(
      Round(ButtonRect.X), Round(ButtonRect.Y),
      Round(ButtonRect.X + ButtonRect.Width),
      Round(ButtonRect.Y + ButtonRect.Height));

    if ArmEnabled then
      BodyCol := Palette.Success
    else
      BodyCol := Palette.NeutralLight;
    if FHoverArm and ArmEnabled then
      BodyCol := Palette.Accent;

    Brush := TGPSolidBrush.Create(ColorToARGB(BodyCol));
    try
      Graphics.FillRectangle(Brush, ButtonRect);
    finally
      Brush.Free;
    end;
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawRectangle(Pen, ButtonRect);
    finally
      Pen.Free;
    end;

    if ArmEnabled then ArmCaption := 'ARM'
    else                ArmCaption := 'arm (gates pending)';
    ACanvas.Font := FValueFont;
    if ArmEnabled then ACanvas.Font.Color := clWhite
    else                ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(
      Round(ButtonRect.X + (ButtonRect.Width  -
        ACanvas.TextWidth(ArmCaption)) / 2),
      Round(ButtonRect.Y + (ButtonRect.Height -
        ACanvas.TextHeight(ArmCaption)) / 2),
      ArmCaption);
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDFlashCheckpointTimeline ---------------------------------------- }

constructor TOBDFlashCheckpointTimeline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 420;
  Height := 80;
  FEvents := TList<TOBDCheckpointEvent>.Create;
  FCapacity := 64;
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 8;
  FCaptionFont.OnChange := HandleFontChange;
end;

destructor TOBDFlashCheckpointTimeline.Destroy;
begin
  FEvents.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TOBDFlashCheckpointTimeline.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFlashCheckpointTimeline.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDFlashCheckpointTimeline.SetCaptionFont(AValue: TFont);
begin
  FCaptionFont.Assign(AValue);
end;

procedure TOBDFlashCheckpointTimeline.SetCapacity(AValue: Integer);
begin
  if AValue < 4 then AValue := 4;
  if AValue > 4096 then AValue := 4096;
  if FCapacity = AValue then Exit;
  FCapacity := AValue;
  while FEvents.Count > FCapacity do FEvents.Delete(0);
  Repaint;
end;

procedure TOBDFlashCheckpointTimeline.PushEvent(
  AKind: TOBDCheckpointKind; const ALabel: string);
var
  Ev: TOBDCheckpointEvent;
begin
  Ev.Timestamp := Now;
  Ev.Kind      := AKind;
  Ev.Label_    := ALabel;
  FEvents.Add(Ev);
  while FEvents.Count > FCapacity do FEvents.Delete(0);
  NotifyBindings;
  Repaint;
end;

procedure TOBDFlashCheckpointTimeline.ClearEvents;
begin
  FEvents.Clear;
  NotifyBindings;
  Repaint;
end;

function TOBDFlashCheckpointTimeline.Events: TArray<TOBDCheckpointEvent>;
begin
  Result := FEvents.ToArray;
end;

function TOBDFlashCheckpointTimeline.EventCount: Integer;
begin
  Result := FEvents.Count;
end;

function TOBDFlashCheckpointTimeline.KindColor(
  AKind: TOBDCheckpointKind): TColor;
begin
  case AKind of
    ckCreated:    Result := Palette.Success;
    ckRestored:   Result := EffectiveAccent;
    ckRolledBack: Result := Palette.Warning;
  else
    Result := EffectiveForeground;
  end;
end;

procedure TOBDFlashCheckpointTimeline.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pen:   TGPPen;
  Brush: TGPSolidBrush;
  Pad: Integer;
  BaselineY: Integer;
  PlotW, PlotX: Integer;
  I: Integer;
  X: Single;
  Marker: TGPRectF;
  Lab: string;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  BaselineY := Height div 2;
  PlotX := Pad;
  PlotW := Width - 2 * Pad;
  if PlotW < 1 then Exit;

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    // Baseline.
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), ScaleValue(1));
    try
      Graphics.DrawLine(Pen,
        Single(PlotX), Single(BaselineY),
        Single(PlotX + PlotW), Single(BaselineY));
    finally
      Pen.Free;
    end;

    if FEvents.Count = 0 then Exit;

    for I := 0 to FEvents.Count - 1 do
    begin
      if FEvents.Count > 1 then
        X := PlotX + I / (FEvents.Count - 1) * PlotW
      else
        X := PlotX + PlotW / 2;
      Marker.X := X - ScaleValue(5);
      Marker.Y := BaselineY - ScaleValue(5);
      Marker.Width  := ScaleValue(10);
      Marker.Height := ScaleValue(10);
      Brush := TGPSolidBrush.Create(
        ColorToARGB(KindColor(FEvents[I].Kind)));
      try
        Graphics.FillEllipse(Brush, Marker);
      finally
        Brush.Free;
      end;

      // Print the latest few labels alternately above / below.
      if (I = FEvents.Count - 1) or
         (I = FEvents.Count - 2) or
         (I = FEvents.Count - 3) then
      begin
        Lab := FEvents[I].Label_;
        if Lab = '' then
          Lab := FormatDateTime('hh:nn:ss', FEvents[I].Timestamp);
        ACanvas.Font := FCaptionFont;
        ACanvas.Font.Color := KindColor(FEvents[I].Kind);
        if I mod 2 = 0 then
          ACanvas.TextOut(Round(X) -
            ACanvas.TextWidth(Lab) div 2,
            BaselineY - ScaleValue(20), Lab)
        else
          ACanvas.TextOut(Round(X) -
            ACanvas.TextWidth(Lab) div 2,
            BaselineY + ScaleValue(8), Lab);
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDFlashAuditTail ------------------------------------------------- }

constructor TOBDFlashAuditTail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
  FMaxRows := 200;
end;

procedure TOBDFlashAuditTail.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureColumns(Self,
      ['Time', 'Kind', 'Target', 'Message'],
      [80, 70, 140, 240]);
end;

procedure TOBDFlashAuditTail.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFlashAuditTail.SetMaxRows(AValue: Integer);
begin
  if AValue < 10 then AValue := 10;
  if AValue > 100000 then AValue := 100000;
  if FMaxRows = AValue then Exit;
  FMaxRows := AValue;
  while Items.Count > FMaxRows do
    Items.Delete(0);
end;

function TOBDFlashAuditTail.KindText(AKind: TOBDAuditKind): string;
begin
  case AKind of
    akSessionStart: Result := 'start';
    akWrite:        Result := 'write';
    akVerify:       Result := 'verify';
    akRollback:     Result := 'rollback';
    akError:        Result := 'error';
  else
    Result := 'event';
  end;
end;

function TOBDFlashAuditTail.KindColor(AKind: TOBDAuditKind): TColor;
begin
  case AKind of
    akError:    Result := clRed;
    akRollback: Result := $0000A8FF;     // amber (BGR)
    akWrite:    Result := clBlack;
  else
    Result := clBlack;
  end;
end;

procedure TOBDFlashAuditTail.PushEntry(const AEntry: TOBDAuditEntry);
var
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Item := Items.Add;
    Item.Caption := FormatDateTime('hh:nn:ss', AEntry.Timestamp);
    Item.SubItems.Add(KindText(AEntry.Kind));
    Item.SubItems.Add(AEntry.Target);
    Item.SubItems.Add(AEntry.Message);
    while Items.Count > FMaxRows do
      Items.Delete(0);
    if Items.Count > 0 then
      Items[Items.Count - 1].MakeVisible(False);
  finally
    Items.EndUpdate;
  end;
  NotifyBindings;
end;

procedure TOBDFlashAuditTail.ClearTail;
begin
  Items.Clear;
  NotifyBindings;
end;

end.
