//------------------------------------------------------------------------------
//  OBD.UI.Diag
//
//  Advanced diagnostics panels:
//
//    TOBDMode06Viewer            On-board monitor results
//                                table.
//    TOBDMode07Viewer            Pending DTC viewer.
//    TOBDMode0AViewer            Permanent DTC viewer.
//    TOBDMode04Confirm           Clear DTCs button + confirm.
//    TOBDRoutineControlLauncher  UDS routine launcher (0x31).
//    TOBDActuatorTestPanel       Sequenced actuator test
//                                runner.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Diag;

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
  Vcl.ComCtrls,
  Vcl.Dialogs,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>One result row for the Mode-06 viewer.</summary>
  TOBDMode06Row = record
    MID:     Byte;
    TID:     Byte;
    Name:    string;
    Value:   Double;
    Min:     Double;
    Max:     Double;
    Passed:  Boolean;
  end;

  /// <summary>Mode-06 on-board-monitor results table.</summary>
  TOBDMode06Viewer = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Idempotently set up the columns. Safe to call
    /// before or after the handle exists.</summary>
    procedure   ConfigureColumnsIfNeeded;
    /// <summary>Replace the displayed rows with the supplied
    /// array.</summary>
    procedure   Refresh(const ARows: TArray<TOBDMode06Row>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearRows;
  protected
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>One DTC row for Mode-07 / Mode-0A viewers.
  /// </summary>
  TOBDDTCRow = record
    Code:        string;
    Description: string;
    Status:      string;
  end;

  /// <summary>Pending DTC viewer (Mode 07).</summary>
  TOBDMode07Viewer = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replace the displayed rows with the supplied
    /// array.</summary>
    procedure   Refresh(const ARows: TArray<TOBDDTCRow>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearRows;
  protected
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>Permanent DTC viewer (Mode 0A).</summary>
  TOBDMode0AViewer = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replace the displayed rows with the supplied
    /// array.</summary>
    procedure   Refresh(const ARows: TArray<TOBDDTCRow>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearRows;
  protected
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>Fires when the user confirms a DTC clear.
  /// </summary>
  TOBDClearDTCsEvent = procedure(Sender: TObject) of object;

  /// <summary>"Clear DTCs" button + confirm dialog. Painted
  /// danger-coloured to flag the destructive nature.
  /// </summary>
  TOBDMode04Confirm = class(TOBDCustomControl)
  strict private
    FConfirmText: string;
    FOnClear:     TOBDClearDTCsEvent;
    FFont:        TFont;
    procedure SetFontA(AValue: TFont);
    procedure SetConfirmText(const AValue: string);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Confirm dialog body. Default warns about the
    /// emissions readiness loss.</summary>
    property ConfirmText: string
      read FConfirmText write SetConfirmText;
    /// <summary>Font used for the button caption.</summary>
    property LabelFont: TFont read FFont write SetFontA;
    /// <summary>Fires once the user has confirmed the clear.
    /// </summary>
    property OnClear: TOBDClearDTCsEvent
      read FOnClear write FOnClear;
  end;

  /// <summary>Fires when the user clicks the launch button.
  /// </summary>
  TOBDRoutineLaunchEvent = procedure(Sender: TObject;
    ARoutineId: Word; const AArgs: TBytes;
    out AResultText: string) of object;

  /// <summary>UDS routine launcher (0x31). Host wires the
  /// routine-id + argument hex; click fires
  /// <see cref="OnLaunch"/>.</summary>
  TOBDRoutineControlLauncher = class(TOBDCustomControl)
  strict private
    FRoutineId: Word;
    FArgsHex:   string;
    FResult:    string;
    FButtonRect: TRect;
    FFont:      TFont;
    FOnLaunch:  TOBDRoutineLaunchEvent;
    procedure SetRoutineId(AValue: Word);
    procedure SetArgsHex(const AValue: string);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure DoLaunch;
    function  ParseArgs: TBytes;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>UDS routine identifier (16-bit). Default 0.
    /// </summary>
    property RoutineId: Word
      read FRoutineId write SetRoutineId default 0;
    /// <summary>Routine arguments as a hex string (whitespace
    /// permitted), e.g. <c>"01 0A FF"</c>.</summary>
    property ArgsHex: string read FArgsHex write SetArgsHex;
    /// <summary>Monospaced font used for the args and result
    /// readout.</summary>
    property MonoFont: TFont read FFont write SetFontA;
    /// <summary>Fires when the host should execute the routine.
    /// </summary>
    property OnLaunch: TOBDRoutineLaunchEvent
      read FOnLaunch write FOnLaunch;
  end;

  /// <summary>One step in an actuator-test sequence.</summary>
  TOBDActuatorStep = record
    Name:    string;
    PIDByte: Byte;
    Status:  string;     // populated as the runner progresses
  end;

  /// <summary>Fires for each step the host should execute.
  /// </summary>
  TOBDActuatorStepEvent = procedure(Sender: TObject;
    AStepIndex: Integer; const AStep: TOBDActuatorStep;
    out AStatus: string; out AAbort: Boolean) of object;

  /// <summary>Sequenced actuator-test runner. Host loads a
  /// step list and clicks Run; the panel fires
  /// <see cref="OnStep"/> for each in order, recording the
  /// status. Abort fires after any step that sets AAbort=True.
  /// </summary>
  TOBDActuatorTestPanel = class(TOBDCustomControl)
  strict private
    FSteps:    TList<TOBDActuatorStep>;
    FRunning:  Boolean;
    FCurrent:  Integer;
    FRunRect:  TRect;
    FAbortRect: TRect;
    FFont:     TFont;
    FOnStep:   TOBDActuatorStepEvent;
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure RunSequence;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Replace the step list with the supplied array.
    /// </summary>
    procedure LoadSteps(const ASteps: array of TOBDActuatorStep);
    /// <summary>Remove all steps.</summary>
    procedure ClearSteps;
    /// <summary>Snapshot of the current step list including
    /// per-step status text.</summary>
    function  Steps: TArray<TOBDActuatorStep>;
    /// <summary>True while a sequence is executing.</summary>
    property Running: Boolean read FRunning;
  published
    /// <summary>Font used for the step labels and buttons.
    /// </summary>
    property LabelFont: TFont read FFont write SetFontA;
    /// <summary>Fires for each step the host must execute.
    /// </summary>
    property OnStep: TOBDActuatorStepEvent
      read FOnStep write FOnStep;
  end;

implementation

procedure ConfigureCols(AListView: TListView;
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

{ ---- TOBDMode06Viewer --------------------------------------------- }

constructor TOBDMode06Viewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDMode06Viewer.CreateWnd;
begin
  inherited;
  ConfigureColumnsIfNeeded;
end;

procedure TOBDMode06Viewer.ConfigureColumnsIfNeeded;
begin
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['MID', 'TID', 'Name', 'Value', 'Min', 'Max', 'Pass'],
      [50, 50, 200, 80, 80, 80, 50]);
end;

procedure TOBDMode06Viewer.Refresh(
  const ARows: TArray<TOBDMode06Row>);
var
  R: TOBDMode06Row;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for R in ARows do
    begin
      Item := Items.Add;
      Item.Caption := Format('0x%2.2X', [R.MID]);
      Item.SubItems.Add(Format('0x%2.2X', [R.TID]));
      Item.SubItems.Add(R.Name);
      Item.SubItems.Add(Format('%g', [R.Value]));
      Item.SubItems.Add(Format('%g', [R.Min]));
      Item.SubItems.Add(Format('%g', [R.Max]));
      if R.Passed then Item.SubItems.Add('yes')
      else             Item.SubItems.Add('no');
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDMode06Viewer.ClearRows;
begin
  Items.Clear;
end;

{ ---- TOBDMode07Viewer / TOBDMode0AViewer -------------------------- }

constructor TOBDMode07Viewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDMode07Viewer.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['Code', 'Description', 'Status'],
      [80, 280, 100]);
end;

procedure TOBDMode07Viewer.Refresh(const ARows: TArray<TOBDDTCRow>);
var
  R: TOBDDTCRow;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for R in ARows do
    begin
      Item := Items.Add;
      Item.Caption := R.Code;
      Item.SubItems.Add(R.Description);
      Item.SubItems.Add(R.Status);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDMode07Viewer.ClearRows;
begin
  Items.Clear;
end;

constructor TOBDMode0AViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDMode0AViewer.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['Code', 'Description', 'Status'],
      [80, 280, 100]);
end;

procedure TOBDMode0AViewer.Refresh(const ARows: TArray<TOBDDTCRow>);
var
  R: TOBDDTCRow;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for R in ARows do
    begin
      Item := Items.Add;
      Item.Caption := R.Code;
      Item.SubItems.Add(R.Description);
      Item.SubItems.Add(R.Status);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDMode0AViewer.ClearRows;
begin
  Items.Clear;
end;

{ ---- TOBDMode04Confirm ------------------------------------------- }

constructor TOBDMode04Confirm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 220;
  Height := 48;
  FConfirmText :=
    'Clearing DTCs also clears Mode-01 readiness flags. ' +
    'Continue?';
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 11;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDMode04Confirm.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDMode04Confirm.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDMode04Confirm.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDMode04Confirm.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDMode04Confirm.SetConfirmText(const AValue: string);
begin
  if FConfirmText = AValue then Exit;
  FConfirmText := AValue;
end;

procedure TOBDMode04Confirm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if csDesigning in ComponentState then Exit;
  if MessageDlg(FConfirmText, mtWarning, [mbYes, mbNo], 0) <> mrYes
  then Exit;
  if Assigned(FOnClear) then
    try
      FOnClear(Self);
    except
    end;
  NotifyBindings;
end;

procedure TOBDMode04Confirm.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  R: TGPRectF;
  Pad: Integer;
  S: string;
  Col: TColor;
begin
  Pad := ScaleValue(2);
  R.X := Pad;
  R.Y := Pad;
  R.Width := Width - 2 * Pad;
  R.Height := Height - 2 * Pad;
  Col := Palette.Danger;
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillRectangle(Brush, R);
  finally
    Brush.Free;
    Graphics.Free;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  S := 'Clear DTCs';
  ACanvas.TextOut(
    Round(R.X + (R.Width  - ACanvas.TextWidth(S)) / 2),
    Round(R.Y + (R.Height - ACanvas.TextHeight(S)) / 2),
    S);
end;

{ ---- TOBDRoutineControlLauncher ---------------------------------- }

constructor TOBDRoutineControlLauncher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 110;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDRoutineControlLauncher.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDRoutineControlLauncher.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDRoutineControlLauncher.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDRoutineControlLauncher.SetRoutineId(AValue: Word);
begin
  if FRoutineId = AValue then Exit;
  FRoutineId := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRoutineControlLauncher.SetArgsHex(const AValue: string);
begin
  if FArgsHex = AValue then Exit;
  FArgsHex := AValue; NotifyBindings; Repaint;
end;

procedure TOBDRoutineControlLauncher.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TOBDRoutineControlLauncher.ParseArgs: TBytes;
var
  Trimmed: string;
  I: Integer;
  V: Integer;
begin
  Trimmed := StringReplace(FArgsHex, ' ', '',
    [rfReplaceAll]);
  if (Length(Trimmed) mod 2) <> 0 then Exit(nil);
  SetLength(Result, Length(Trimmed) div 2);
  for I := 0 to High(Result) do
    if TryStrToInt('$' + Copy(Trimmed, I * 2 + 1, 2), V) then
      Result[I] := Byte(V)
    else
    begin
      SetLength(Result, 0);
      Exit;
    end;
end;

procedure TOBDRoutineControlLauncher.DoLaunch;
var
  Args: TBytes;
  Res: string;
begin
  Args := ParseArgs;
  Res := '';
  if Assigned(FOnLaunch) then
    try
      FOnLaunch(Self, FRoutineId, Args, Res);
      FResult := Res;
    except
      on E: Exception do
        FResult := 'raised: ' + E.Message;
    end
  else
    FResult := 'no OnLaunch handler';
  NotifyBindings;
  Repaint;
end;

procedure TOBDRoutineControlLauncher.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and PtInRect(FButtonRect, Point(X, Y)) then
    DoLaunch;
end;

procedure TOBDRoutineControlLauncher.PaintControl(ACanvas: TCanvas);
var
  Pad, Y, ButW, ButH: Integer;
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  R: TGPRectF;
  S: string;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  Pad := ScaleValue(8);
  Y := Pad;
  ACanvas.TextOut(Pad, Y,
    Format('routine 0x%4.4X  args [%s]', [FRoutineId, FArgsHex]));
  Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(8));

  ButW := ScaleValue(120);
  ButH := ScaleValue(28);
  FButtonRect := Rect(Pad, Y, Pad + ButW, Y + ButH);
  R.X := FButtonRect.Left;
  R.Y := FButtonRect.Top;
  R.Width := ButW;
  R.Height := ButH;
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(EffectiveAccent));
  Pen   := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillRectangle(Brush, R);
    Graphics.DrawRectangle(Pen, R);
  finally
    Pen.Free;
    Brush.Free;
    Graphics.Free;
  end;
  ACanvas.Font.Color := clWhite;
  S := 'Launch (0x31)';
  ACanvas.TextOut(
    FButtonRect.Left + (ButW - ACanvas.TextWidth(S)) div 2,
    FButtonRect.Top  + (ButH - ACanvas.TextHeight(S)) div 2,
    S);

  if FResult <> '' then
  begin
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y + ButH + ScaleValue(6),
      'result: ' + FResult);
  end;
end;

{ ---- TOBDActuatorTestPanel -------------------------------------- }

constructor TOBDActuatorTestPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 200;
  FSteps := TList<TOBDActuatorStep>.Create;
  FCurrent := -1;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 9;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDActuatorTestPanel.Destroy;
begin
  FSteps.Free;
  FFont.Free;
  inherited;
end;

procedure TOBDActuatorTestPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDActuatorTestPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDActuatorTestPanel.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDActuatorTestPanel.LoadSteps(
  const ASteps: array of TOBDActuatorStep);
var I: Integer;
begin
  FSteps.Clear;
  for I := 0 to High(ASteps) do FSteps.Add(ASteps[I]);
  FCurrent := -1;
  NotifyBindings;
  Repaint;
end;

procedure TOBDActuatorTestPanel.ClearSteps;
begin
  FSteps.Clear;
  FCurrent := -1;
  NotifyBindings;
  Repaint;
end;

function TOBDActuatorTestPanel.Steps: TArray<TOBDActuatorStep>;
begin
  Result := FSteps.ToArray;
end;

procedure TOBDActuatorTestPanel.RunSequence;
var
  I: Integer;
  Step: TOBDActuatorStep;
  Status: string;
  AbortReq: Boolean;
begin
  if FRunning then Exit;
  FRunning := True;
  try
    for I := 0 to FSteps.Count - 1 do
    begin
      FCurrent := I;
      Repaint;
      Step := FSteps[I];
      Status := '';
      AbortReq := False;
      if Assigned(FOnStep) then
        try
          FOnStep(Self, I, Step, Status, AbortReq);
          Step.Status := Status;
        except
          on E: Exception do
          begin
            Step.Status := 'raised: ' + E.Message;
            AbortReq := True;
          end;
        end
      else
        Step.Status := 'no OnStep handler';
      FSteps[I] := Step;
      if AbortReq then Break;
    end;
  finally
    FRunning := False;
    FCurrent := -1;
    NotifyBindings;
    Repaint;
  end;
end;

procedure TOBDActuatorTestPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if PtInRect(FRunRect, Point(X, Y)) and not FRunning then
    RunSequence;
end;

procedure TOBDActuatorTestPanel.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, ButW, ButH, RowH, Y, I: Integer;
  R: TGPRectF;
  S: string;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  Pad := ScaleValue(8);

  ButW := ScaleValue(80);
  ButH := ScaleValue(26);
  FRunRect := Rect(Pad, Pad, Pad + ButW, Pad + ButH);
  R.X := FRunRect.Left;
  R.Y := FRunRect.Top;
  R.Width := ButW;
  R.Height := ButH;
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  Brush := TGPSolidBrush.Create(ColorToARGB(Palette.Success));
  Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.FillRectangle(Brush, R);
    Graphics.DrawRectangle(Pen, R);
  finally
    Pen.Free;
    Brush.Free;
    Graphics.Free;
  end;
  ACanvas.Font.Color := clWhite;
  S := 'Run';
  ACanvas.TextOut(
    FRunRect.Left + (ButW - ACanvas.TextWidth(S)) div 2,
    FRunRect.Top  + (ButH - ACanvas.TextHeight(S)) div 2,
    S);

  RowH := ScaleValue(18);
  Y := Pad + ButH + ScaleValue(8);
  for I := 0 to FSteps.Count - 1 do
  begin
    if Y + RowH > Height then Break;
    if I = FCurrent then ACanvas.Font.Color := Palette.Warning
    else                 ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y,
      Format('%2.2d. %s — %s',
        [I + 1, FSteps[I].Name, FSteps[I].Status]));
    Inc(Y, RowH);
  end;
end;

end.
