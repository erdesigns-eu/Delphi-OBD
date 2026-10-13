//------------------------------------------------------------------------------
//  ERD.Components.Buttons
//
//  Themed push button, check box and radio button for the Application
//  controls.
//
//    TERDButton       primary (orange), secondary, danger, danger
//                     outline and ghost kinds, an optional line glyph,
//                     ModalResult / Default / Cancel like a VCL button.
//    TERDCheckBox     check box (Style = csCheck) or on / off switch
//                     (Style = csSwitch); AllowGrayed gives the third
//                     state.
//    TERDRadioButton  checking one clears the others with the same
//                     Parent and GroupIndex; arrow keys move the choice
//                     through the group.
//
//  All three size themselves from the density (AutoSize, default
//  True) and keep a 4 px margin around the visible shape for the
//  focus ring, which is drawn 3 px outside the shape.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//------------------------------------------------------------------------------

unit ERD.Components.Buttons;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  ERD.Components.Control,
  ERD.Components.Paint;

type
  /// <summary>Look of a <c>TERDCheckBox</c>.</summary>
  TERDCheckStyle = (
    /// <summary>Square check box.</summary>
    csCheck,
    /// <summary>On / off switch.</summary>
    csSwitch);

  /// <summary>Base of the clickable Application controls: tracks hover,
  /// pressed and focus, repaints on caption and enabled changes and
  /// sizes itself from the density.</summary>
  TERDInteractiveControl = class(TERDCustomControl)
  strict private
    FHover: Boolean;
    FPressed: Boolean;
    FKeyPressed: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    /// <summary>Margin kept free around the shape for the focus ring,
    /// in device pixels.</summary>
    /// <returns>4 px, scaled.</returns>
    function RingMargin: Integer;
    /// <summary>Width of a text in device pixels, measured with the
    /// painter fonts.</summary>
    /// <param name="AText">Text.</param>
    /// <param name="ASize">Em height in 96-DPI pixels.</param>
    /// <param name="AWeight">Weight.</param>
    /// <returns>Width.</returns>
    function MeasureText(const AText: string; ASize: Single;
      AWeight: TERDTextWeight): Integer;
    /// <summary>Interaction state for the painter.</summary>
    /// <returns>Disabled, pressed, hover or normal.</returns>
    function InteractionState: TERDControlState;
    /// <summary>True while the control has the keyboard focus.</summary>
    /// <returns>Focus flag.</returns>
    function HasFocus: Boolean;
    /// <summary>Performs the action of the control: called on a mouse
    /// click and on Space. Default fires OnClick.</summary>
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    /// <summary>True while the pointer is over the control.</summary>
    property Hover: Boolean read FHover;
    /// <summary>True while the mouse button or Space is held down.
    /// </summary>
    property Pressed: Boolean read FPressed;
  public
    /// <summary>Creates the control with AutoSize on and TabStop on.
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Re-sizes for the new density.</summary>
    procedure DensityChanged; override;
  published
    /// <summary>Sizes the control to its content and the density.
    /// </summary>
    property AutoSize default True;
    /// <summary>Text of the control.</summary>
    property Caption;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Focusable with Tab.</summary>
    property TabStop default True;
  end;

  /// <summary>Themed push button.</summary>
  TERDButton = class(TERDInteractiveControl)
  strict private
    FKind: TERDButtonKind;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FModalResult: TModalResult;
    FDefault: Boolean;
    FCancel: Boolean;
    procedure SetKind(AValue: TERDButtonKind);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a primary button.</summary>
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>Sets the parent form's ModalResult when ModalResult is
    /// not mrNone, then fires OnClick.</summary>
    procedure Click; override;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Visual weight of the button.</summary>
    property Kind: TERDButtonKind read FKind write SetKind default bkPrimary;
    /// <summary>Line glyph left of the caption.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Modal result handed to the parent form on a click.
    /// </summary>
    property ModalResult: TModalResult read FModalResult write FModalResult
      default mrNone;
    /// <summary>Enter anywhere on the form clicks this button.</summary>
    property Default: Boolean read FDefault write FDefault default False;
    /// <summary>Escape anywhere on the form clicks this button.
    /// </summary>
    property Cancel: Boolean read FCancel write FCancel default False;
  end;

  /// <summary>Themed check box or on / off switch.</summary>
  TERDCheckBox = class(TERDInteractiveControl)
  strict private
    FState: TCheckBoxState;
    FAllowGrayed: Boolean;
    FStyle: TERDCheckStyle;
    FOnChange: TNotifyEvent;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    procedure SetState(AValue: TCheckBoxState);
    procedure SetStyle(AValue: TERDCheckStyle);
    function IndicatorWidth: Integer;
    function IndicatorHeight: Integer;
    function LabelSize: Single;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    /// <summary>Creates an unchecked check box.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Moves to the next state (unchecked, checked, grayed
    /// when AllowGrayed) and fires OnClick.</summary>
    procedure Click; override;
  published
    /// <summary>Checked or not; Grayed reads as checked.</summary>
    property Checked: Boolean read GetChecked write SetChecked
      stored False;
    /// <summary>Unchecked, checked or grayed.</summary>
    property State: TCheckBoxState read FState write SetState
      default cbUnchecked;
    /// <summary>A click cycles through the grayed state too.</summary>
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed
      default False;
    /// <summary>Check box or switch.</summary>
    property Style: TERDCheckStyle read FStyle write SetStyle
      default csCheck;
    /// <summary>Fires whenever State changes, from code or by the user.
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed radio button.</summary>
  TERDRadioButton = class(TERDInteractiveControl)
  strict private
    FChecked: Boolean;
    FGroupIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetChecked(AValue: Boolean);
    procedure SetGroupIndex(AValue: Integer);
    procedure ClearSiblings;
    procedure MoveInGroup(ADelta: Integer);
    function LabelSize: Single;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Checks the button and fires OnClick.</summary>
    procedure Click; override;
  published
    /// <summary>Checking clears the other buttons of the group.
    /// </summary>
    property Checked: Boolean read FChecked write SetChecked default False;
    /// <summary>Buttons with the same Parent and GroupIndex form one
    /// group.</summary>
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex
      default 0;
    /// <summary>Fires whenever Checked changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  BUTTON_TEXT_SIZE = ERD_TEXT_BODY;

{ TERDInteractiveControl }

constructor TERDInteractiveControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  TabStop := True;
  AutoSize := True;
end;

function TERDInteractiveControl.RingMargin: Integer;
begin
  Result := ScaleValue(4);
end;

function TERDInteractiveControl.MeasureText(const AText: string;
  ASize: Single; AWeight: TERDTextWeight): Integer;
begin
  Result := ERDMeasureText(AText, ASize, AWeight, ScaleValue(96));
end;

function TERDInteractiveControl.InteractionState: TERDControlState;
begin
  if not Enabled then
    Result := cstDisabled
  else if FPressed and (FHover or FKeyPressed) then
    Result := cstPressed
  else if FHover then
    Result := cstHover
  else
    Result := cstNormal;
end;

function TERDInteractiveControl.HasFocus: Boolean;
begin
  Result := Focused and not IsPreview;
end;

procedure TERDInteractiveControl.Click;
begin
  inherited Click;
end;

procedure TERDInteractiveControl.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

procedure TERDInteractiveControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TERDInteractiveControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHover := False;
  Invalidate;
end;

procedure TERDInteractiveControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not Enabled then
  begin
    FPressed := False;
    FKeyPressed := False;
  end;
  Invalidate;
end;

procedure TERDInteractiveControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TERDInteractiveControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    if TabStop and CanFocus and not Focused then
      SetFocus;
    FPressed := True;
    Invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TERDInteractiveControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FPressed then
  begin
    FPressed := False;
    Invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TERDInteractiveControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (Shift = []) and Enabled then
  begin
    FPressed := True;
    FKeyPressed := True;
    Invalidate;
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TERDInteractiveControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and FKeyPressed then
  begin
    FPressed := False;
    FKeyPressed := False;
    Invalidate;
    Key := 0;
    Click;
    Exit;
  end;
  inherited KeyUp(Key, Shift);
end;

procedure TERDInteractiveControl.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TERDInteractiveControl.DoExit;
begin
  FPressed := False;
  FKeyPressed := False;
  inherited DoExit;
  Invalidate;
end;

{ TERDButton }

destructor TERDButton.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TERDButton.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := TERDIcon.Create(Self);
  FKind := bkPrimary;
  FGlyph := glNone;
  FModalResult := mrNone;
  Width := 96;
  Height := 38;
end;

procedure TERDButton.SetKind(AValue: TERDButtonKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TERDButton.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TERDButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W: Integer;
begin
  Result := True;
  W := MeasureText(Caption, BUTTON_TEXT_SIZE, twSemibold) + ScaleValue(28);
  if (FIcon <> nil) and FIcon.Available(FGlyph) then
    Inc(W, ScaleValue(18));
  NewWidth := W + 2 * RingMargin;
  NewHeight := ScaleValue(Metrics.Button) + 2 * RingMargin;
end;

procedure TERDButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) and Enabled then
  begin
    Key := 0;
    Click;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TERDButton.CMDialogKey(var Message: TCMDialogKey);
begin
  if Enabled and Visible and
    (KeyDataToShiftState(Message.KeyData) = []) and
    (((Message.CharCode = VK_RETURN) and (FDefault or Focused)) or
    ((Message.CharCode = VK_ESCAPE) and FCancel)) and
    (Parent <> nil) and Parent.Showing then
  begin
    Click;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TERDButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and (FModalResult <> mrNone) then
    Form.ModalResult := FModalResult;
  inherited Click;
end;

procedure TERDButton.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  M: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    M := RingMargin;
    P.Button(Rect(M, M, Width - M, Height - M), Caption, FKind,
      InteractionState, HasFocus, FGlyph, FIcon);
  finally
    P.Free;
  end;
end;

{ TERDCheckBox }

constructor TERDCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := cbUnchecked;
  FStyle := csCheck;
  Width := 96;
  Height := 38;
end;

function TERDCheckBox.GetChecked: Boolean;
begin
  Result := FState <> cbUnchecked;
end;

procedure TERDCheckBox.SetChecked(AValue: Boolean);
begin
  if AValue then
    SetState(cbChecked)
  else
    SetState(cbUnchecked);
end;

procedure TERDCheckBox.SetState(AValue: TCheckBoxState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Invalidate;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then
    FOnChange(Self);
end;

procedure TERDCheckBox.SetStyle(AValue: TERDCheckStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

function TERDCheckBox.IndicatorWidth: Integer;
begin
  if FStyle = csSwitch then
    Result := Round(ScaleValue(Metrics.Switch) * 1.9)
  else
    Result := ScaleValue(Metrics.Check);
end;

function TERDCheckBox.IndicatorHeight: Integer;
begin
  if FStyle = csSwitch then
    Result := ScaleValue(Metrics.Switch)
  else
    Result := ScaleValue(Metrics.Check);
end;

function TERDCheckBox.LabelSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

function TERDCheckBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W: Integer;
begin
  Result := True;
  W := RingMargin + IndicatorWidth;
  if Caption <> '' then
    Inc(W, ScaleValue(8) + MeasureText(Caption, LabelSize, twRegular));
  NewWidth := W + RingMargin;
  NewHeight := Max(ScaleValue(Metrics.Button),
    IndicatorHeight + 2 * RingMargin);
end;

procedure TERDCheckBox.Click;
begin
  case FState of
    cbUnchecked:
      SetState(cbChecked);
    cbChecked:
      if FAllowGrayed and (FStyle = csCheck) then
        SetState(cbGrayed)
      else
        SetState(cbUnchecked);
  else
    SetState(cbUnchecked);
  end;
  inherited Click;
end;

procedure TERDCheckBox.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  X, CY: Integer;
  Ink: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    X := RingMargin;
    CY := Height div 2;
    if FStyle = csSwitch then
      P.Switch(X, CY, Metrics.Switch, FState = cbChecked, Enabled, HasFocus)
    else
      P.CheckBox(X, CY, Metrics.Check, FState, Enabled, Hover, HasFocus);
    if Caption <> '' then
    begin
      if Enabled then
        Ink := Palette.ForegroundText
      else
        Ink := P.DisabledText;
      Inc(X, IndicatorWidth + P.S(8));
      P.Text(X, CY, Caption, LabelSize, Ink, twRegular, taLeftJustify,
        Width - X - RingMargin);
    end;
  finally
    P.Free;
  end;
end;

{ TERDRadioButton }

procedure TERDRadioButton.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked then
    ClearSiblings;
  Invalidate;
  if Assigned(FOnChange) and not (csLoading in ComponentState) then
    FOnChange(Self);
end;

procedure TERDRadioButton.SetGroupIndex(AValue: Integer);
begin
  if FGroupIndex = AValue then
    Exit;
  FGroupIndex := AValue;
  if FChecked then
    ClearSiblings;
end;

procedure TERDRadioButton.ClearSiblings;
var
  I: Integer;
  Sibling: TControl;
begin
  if (Parent = nil) or (csLoading in ComponentState) then
    Exit;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Sibling := Parent.Controls[I];
    if (Sibling <> Self) and (Sibling is TERDRadioButton) and
      (TERDRadioButton(Sibling).GroupIndex = FGroupIndex) then
      TERDRadioButton(Sibling).Checked := False;
  end;
end;

procedure TERDRadioButton.MoveInGroup(ADelta: Integer);
var
  Group: TList;
  I, Index: Integer;
  Sibling: TControl;
  Next: TERDRadioButton;
begin
  if Parent = nil then
    Exit;
  Group := TList.Create;
  try
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Sibling := Parent.Controls[I];
      if (Sibling is TERDRadioButton) and
        (TERDRadioButton(Sibling).GroupIndex = FGroupIndex) and
        Sibling.Visible and Sibling.Enabled then
        Group.Add(Sibling);
    end;
    Index := Group.IndexOf(Self);
    if (Index < 0) or (Group.Count < 2) then
      Exit;
    Index := (Index + ADelta + Group.Count) mod Group.Count;
    Next := TERDRadioButton(Group[Index]);
    if Next.CanFocus then
      Next.SetFocus;
    Next.Click;
  finally
    Group.Free;
  end;
end;

function TERDRadioButton.LabelSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

procedure TERDRadioButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

function TERDRadioButton.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  W: Integer;
begin
  Result := True;
  W := RingMargin + ScaleValue(Metrics.Check);
  if Caption <> '' then
    Inc(W, ScaleValue(8) + MeasureText(Caption, LabelSize, twRegular));
  NewWidth := W + RingMargin;
  NewHeight := Max(ScaleValue(Metrics.Button),
    ScaleValue(Metrics.Check) + 2 * RingMargin);
end;

procedure TERDRadioButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_UP:
      begin
        Key := 0;
        MoveInGroup(-1);
        Exit;
      end;
    VK_RIGHT, VK_DOWN:
      begin
        Key := 0;
        MoveInGroup(1);
        Exit;
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TERDRadioButton.Click;
begin
  SetChecked(True);
  inherited Click;
end;

procedure TERDRadioButton.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  X, CY: Integer;
  Ink: TColor;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    X := RingMargin;
    CY := Height div 2;
    P.RadioButton(X, CY, Metrics.Check, FChecked, Enabled, Hover, HasFocus);
    if Caption <> '' then
    begin
      if Enabled then
        Ink := Palette.ForegroundText
      else
        Ink := P.DisabledText;
      Inc(X, P.S(Metrics.Check) + P.S(8));
      P.Text(X, CY, Caption, LabelSize, Ink, twRegular, taLeftJustify,
        Width - X - RingMargin);
    end;
  finally
    P.Free;
  end;
end;

end.
