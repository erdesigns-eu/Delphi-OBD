//------------------------------------------------------------------------------
//  ERD.UI.Dialogs
//
//  Themed modal dialog support for OBD Studio.
//
//    TOBDDialog       non-visual MessageDlg replacement that builds a
//                     borderless themed modal form at run time.
//    OBDMessageDlg    convenience routine for one-off themed dialogs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.Dialogs;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Themes,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons;

type
  /// <summary>Dialog icon and semantic colour.</summary>
  TOBDDialogKind = (
    /// <summary>Information dialog.</summary>
    dkInfo,
    /// <summary>Successful result dialog.</summary>
    dkSuccess,
    /// <summary>Warning or pending confirmation dialog.</summary>
    dkWarning,
    /// <summary>Danger or destructive action dialog.</summary>
    dkDanger,
    /// <summary>Confirmation dialog.</summary>
    dkConfirm);

  /// <summary>Themed replacement for MessageDlg.</summary>
  TOBDDialog = class(TComponent, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FDensity: TOBDDensity;
    FTitle: string;
    FText: string;
    FKind: TOBDDialogKind;
    FButtons: TMsgDlgButtons;
    FDefaultButton: TMsgDlgBtn;
    FButtonCaptions: TStrings;
    FDangerButton: TMsgDlgBtn;
    FCheckBoxCaption: string;
    FCheckBoxChecked: Boolean;
    procedure SetTheme(AValue: TOBDTheme);
    procedure SetDensity(AValue: TOBDDensity);
    procedure SetTitle(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetKind(AValue: TOBDDialogKind);
    procedure SetButtons(AValue: TMsgDlgButtons);
    procedure SetDefaultButton(AValue: TMsgDlgBtn);
    procedure SetButtonCaptions(AValue: TStrings);
    procedure SetDangerButton(AValue: TMsgDlgBtn);
    procedure SetCheckBoxCaption(const AValue: string);
    procedure SetCheckBoxChecked(AValue: Boolean);
    function GetPalette: TOBDThemePalette;
    function CaptionForButton(AButton: TMsgDlgBtn): string;
  protected
    /// <summary>Clears theme references when they are removed.</summary>
    /// <param name="AComponent">Removed component.</param>
    /// <param name="Operation">Notification operation.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates a warning OK dialog.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases owned strings and theme references.</summary>
    destructor Destroy; override;
    /// <summary>Receives theme changes.</summary>
    procedure ThemeChanged;
    /// <summary>Builds and shows the themed modal dialog.</summary>
    /// <returns>Selected modal result.</returns>
    function Execute: TModalResult;
    /// <summary>Palette currently used by the dialog.</summary>
    /// <returns>Resolved palette.</returns>
    property Palette: TOBDThemePalette read GetPalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TOBDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Header title and bold body title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Wrapped body text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Icon and semantic colour.</summary>
    property Kind: TOBDDialogKind read FKind write SetKind default dkWarning;
    /// <summary>Buttons shown in the footer.</summary>
    property Buttons: TMsgDlgButtons read FButtons write SetButtons;
    /// <summary>Button clicked by Enter.</summary>
    property DefaultButton: TMsgDlgBtn read FDefaultButton
      write SetDefaultButton default mbOK;
    /// <summary>Optional button caption overrides, for example mbYes=Clear codes.</summary>
    property ButtonCaptions: TStrings read FButtonCaptions
      write SetButtonCaptions;
    /// <summary>Button rendered as a destructive primary action.</summary>
    property DangerButton: TMsgDlgBtn read FDangerButton write SetDangerButton
      default mbNo;
    /// <summary>Optional check box caption.</summary>
    property CheckBoxCaption: string read FCheckBoxCaption
      write SetCheckBoxCaption;
    /// <summary>Current check box state.</summary>
    property CheckBoxChecked: Boolean read FCheckBoxChecked
      write SetCheckBoxChecked default False;
  end;

/// <summary>Shows a themed message dialog.</summary>
/// <param name="ATitle">Header and bold body title.</param>
/// <param name="AText">Wrapped body text.</param>
/// <param name="AKind">Dialog kind.</param>
/// <param name="AButtons">Buttons to show.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>Selected modal result.</returns>
function OBDMessageDlg(const ATitle, AText: string; AKind: TOBDDialogKind;
  AButtons: TMsgDlgButtons; ATheme: TOBDTheme = nil): TModalResult;

implementation

type
  TDialogContent = class(TOBDCustomControl)
  strict private
    FDialog: TOBDDialog;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreateContent(AOwner: TComponent; ADialog: TOBDDialog);
  end;

  TDialogForm = class(TForm)
  strict private
    FDialog: TOBDDialog;
    FPalette: TOBDThemePalette;
    FDensity: TOBDDensity;
    FHeaderHeight: Integer;
    FDefaultResult: TModalResult;
    FCancelResult: TModalResult;
    function CloseRect: TRect;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor CreateDialog(AOwner: TComponent; ADialog: TOBDDialog);
    property DefaultResult: TModalResult read FDefaultResult
      write FDefaultResult;
    property CancelResult: TModalResult read FCancelResult
      write FCancelResult;
  end;

function FallbackPalette: TOBDThemePalette;
begin
  if VCLStyleIsDark then
    Result := BRAND_PALETTE_DARK
  else
    Result := BRAND_PALETTE_LIGHT;
  if TStyleManager.IsCustomStyleActive then
  begin
    Result.Background := StyleColor(scWindow, Result.Background);
    Result.ForegroundText := StyleServices.GetSystemColor(clWindowText);
  end;
end;

function ButtonName(AButton: TMsgDlgBtn): string;
begin
  case AButton of
    mbYes: Result := 'mbYes';
    mbNo: Result := 'mbNo';
    mbOK: Result := 'mbOK';
    mbCancel: Result := 'mbCancel';
    mbAbort: Result := 'mbAbort';
    mbRetry: Result := 'mbRetry';
    mbIgnore: Result := 'mbIgnore';
    mbAll: Result := 'mbAll';
    mbNoToAll: Result := 'mbNoToAll';
    mbYesToAll: Result := 'mbYesToAll';
    mbHelp: Result := 'mbHelp';
  else
    Result := 'mbClose';
  end;
end;

function DefaultCaption(AButton: TMsgDlgBtn): string;
begin
  case AButton of
    mbYes: Result := 'Yes';
    mbNo: Result := 'No';
    mbOK: Result := 'OK';
    mbCancel: Result := 'Cancel';
    mbAbort: Result := 'Abort';
    mbRetry: Result := 'Retry';
    mbIgnore: Result := 'Ignore';
    mbAll: Result := 'All';
    mbNoToAll: Result := 'No to all';
    mbYesToAll: Result := 'Yes to all';
    mbHelp: Result := 'Help';
  else
    Result := 'Close';
  end;
end;

function ModalResultOf(AButton: TMsgDlgBtn): TModalResult;
begin
  case AButton of
    mbYes: Result := mrYes;
    mbNo: Result := mrNo;
    mbOK: Result := mrOk;
    mbCancel: Result := mrCancel;
    mbAbort: Result := mrAbort;
    mbRetry: Result := mrRetry;
    mbIgnore: Result := mrIgnore;
    mbAll: Result := mrAll;
    mbNoToAll: Result := mrNoToAll;
    mbYesToAll: Result := mrYesToAll;
    mbHelp: Result := mrNone;
  else
    Result := mrClose;
  end;
end;

function KindColor(AKind: TOBDDialogKind; const P: TOBDThemePalette): TColor;
begin
  case AKind of
    dkSuccess:
      Result := P.Success;
    dkWarning, dkConfirm:
      Result := P.Warning;
    dkDanger:
      Result := P.Danger;
  else
    Result := P.Accent;
  end;
end;

{ TDialogContent }

constructor TDialogContent.CreateContent(AOwner: TComponent;
  ADialog: TOBDDialog);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  Theme := ADialog.Theme;
  Density := ADialog.Density;
  ParentDensity := False;
  TabStop := False;
end;

procedure TDialogContent.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
  P: TOBDThemePalette;
  C, Ink: TColor;
  TextRect: TRect;
  X, Y: Integer;
begin
  P := Palette;
  Painter := TOBDPainter.Create(ACanvas, P, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, P.Background);
    C := KindColor(FDialog.Kind, P);
    X := ScaleValue(32);
    Y := ScaleValue(32);
    Painter.Ellipse(X - ScaleValue(18), Y - ScaleValue(18), ScaleValue(36),
      ScaleValue(36), Painter.Tint(C), C, ScaleValue(1));
    Ink := C;
    case FDialog.Kind of
      dkSuccess:
        Painter.GlyphCheck(X, Y, Ink, 1.3);
      dkWarning, dkConfirm:
        Painter.GlyphPending(X, Y, Ink, 1.35);
      dkDanger:
        Painter.Text(X, Y + ScaleValue(1), '!', 16, Ink, twBold, taCenter);
    else
      Painter.Text(X, Y + ScaleValue(1), 'i', 16, Ink, twBold, taCenter);
    end;

    Painter.Text(ScaleValue(56), ScaleValue(22), FDialog.Title, 15,
      P.ForegroundText, twBold, taLeftJustify, Width - ScaleValue(72));
    TextRect := Rect(ScaleValue(56), ScaleValue(40), Width - ScaleValue(20),
      Height - ScaleValue(8));
    Painter.WrapText(TextRect, FDialog.Text, 12.5, P.ForegroundText,
      twRegular);
  finally
    Painter.Free;
  end;
end;

{ TDialogForm }

constructor TDialogForm.CreateDialog(AOwner: TComponent; ADialog: TOBDDialog);
var
  Metrics: TOBDDensityMetrics;
begin
  inherited CreateNew(AOwner);
  FDialog := ADialog;
  FPalette := ADialog.Palette;
  FDensity := ADialog.Density;
  Metrics := DensityMetrics(FDensity);
  FHeaderHeight := System.Math.Max(24, Metrics.TitleBar - 6);
  BorderStyle := bsNone;
  BorderIcons := [];
  Position := poDesigned;
  KeyPreview := True;
  Color := FPalette.Background;
  Font.Name := 'Segoe UI';
end;

procedure TDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

function TDialogForm.CloseRect: TRect;
begin
  Result := Rect(ClientWidth - FHeaderHeight, 1, ClientWidth - 1,
    FHeaderHeight + 1);
end;

procedure TDialogForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  P := ScreenToClient(Point(Message.XPos, Message.YPos));
  if PtInRect(CloseRect, P) then
    Message.Result := HTCLIENT
  else if (P.Y >= 0) and (P.Y < FHeaderHeight) then
    Message.Result := HTCAPTION;
end;

procedure TDialogForm.Paint;
var
  Painter: TOBDPainter;
  R: TRect;
  Metrics: TOBDDensityMetrics;
  FooterH: Integer;
begin
  inherited;
  Metrics := DensityMetrics(FDensity);
  FooterH := Metrics.Button + 28;
  Painter := TOBDPainter.Create(Canvas, FPalette, Screen.PixelsPerInch);
  try
    R := ClientRect;
    Painter.FillRect(R, FPalette.Background);
    Painter.FrameRect(R, FPalette.Accent);
    Painter.FillRect(Rect(1, 1, ClientWidth - 1, FHeaderHeight + 1),
      FPalette.GaugeFace);
    Painter.HLine(1, FHeaderHeight + 1, ClientWidth - 2,
      FPalette.NeutralLight);
    Painter.FillRect(Rect(1, ClientHeight - FooterH, ClientWidth - 1,
      ClientHeight - 1), FPalette.GaugeFace);
    Painter.HLine(1, ClientHeight - FooterH, ClientWidth - 2,
      FPalette.NeutralLight);
    Painter.Text(14, 1 + FHeaderHeight div 2, FDialog.Title, 12.5,
      FPalette.GaugeLabel, twRegular, taLeftJustify,
      ClientWidth - FHeaderHeight - 24);
    Painter.Lines([MakePoint(ClientWidth - FHeaderHeight div 2 - 4,
      1 + FHeaderHeight div 2 - 4), MakePoint(ClientWidth - FHeaderHeight div 2 + 4,
      1 + FHeaderHeight div 2 + 4)], FPalette.Subtle, 1.6);
    Painter.Lines([MakePoint(ClientWidth - FHeaderHeight div 2 + 4,
      1 + FHeaderHeight div 2 - 4), MakePoint(ClientWidth - FHeaderHeight div 2 - 4,
      1 + FHeaderHeight div 2 + 4)], FPalette.Subtle, 1.6);
  finally
    Painter.Free;
  end;
end;

procedure TDialogForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and PtInRect(CloseRect, Point(X, Y)) then
    ModalResult := FCancelResult;
end;

procedure TDialogForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
  begin
    ModalResult := FCancelResult;
    Key := 0;
  end
  else if Key = VK_RETURN then
  begin
    if FDefaultResult <> mrNone then
      ModalResult := FDefaultResult;
    Key := 0;
  end;
end;

{ TOBDDialog }

constructor TOBDDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FTitle := 'OBD Studio';
  FKind := dkWarning;
  FButtons := [mbOK];
  FDefaultButton := mbOK;
  FDangerButton := mbNo;
  FButtonCaptions := TStringList.Create;
end;

destructor TOBDDialog.Destroy;
begin
  FButtonCaptions.Free;
  SetTheme(nil);
  inherited;
end;

procedure TOBDDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TOBDDialog.ThemeChanged;
begin
end;

function TOBDDialog.GetPalette: TOBDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else if TOBDTheme.GetDefault <> nil then
    Result := TOBDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
end;

procedure TOBDDialog.SetTheme(AValue: TOBDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
  end;
  FTheme := AValue;
  if FTheme <> nil then
  begin
    FTheme.FreeNotification(Self);
    FTheme.Attach(Self);
  end;
end;

procedure TOBDDialog.SetDensity(AValue: TOBDDensity);
begin
  FDensity := AValue;
end;

procedure TOBDDialog.SetTitle(const AValue: string);
begin
  FTitle := AValue;
end;

procedure TOBDDialog.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TOBDDialog.SetKind(AValue: TOBDDialogKind);
begin
  FKind := AValue;
end;

procedure TOBDDialog.SetButtons(AValue: TMsgDlgButtons);
begin
  if AValue = [] then
    AValue := [mbOK];
  FButtons := AValue;
end;

procedure TOBDDialog.SetDefaultButton(AValue: TMsgDlgBtn);
begin
  FDefaultButton := AValue;
end;

procedure TOBDDialog.SetButtonCaptions(AValue: TStrings);
begin
  FButtonCaptions.Assign(AValue);
end;

procedure TOBDDialog.SetDangerButton(AValue: TMsgDlgBtn);
begin
  FDangerButton := AValue;
end;

procedure TOBDDialog.SetCheckBoxCaption(const AValue: string);
begin
  FCheckBoxCaption := AValue;
end;

procedure TOBDDialog.SetCheckBoxChecked(AValue: Boolean);
begin
  FCheckBoxChecked := AValue;
end;

function TOBDDialog.CaptionForButton(AButton: TMsgDlgBtn): string;
var
  I, P: Integer;
  Name, Line: string;
begin
  Name := ButtonName(AButton);
  for I := 0 to FButtonCaptions.Count - 1 do
  begin
    Line := FButtonCaptions[I];
    P := Pos('=', Line);
    if P > 0 then
      if SameText(Trim(Copy(Line, 1, P - 1)), Name) then
        Exit(Trim(Copy(Line, P + 1, MaxInt)));
  end;
  Result := DefaultCaption(AButton);
end;

function TOBDDialog.Execute: TModalResult;
const
  ORDER: array[0..11] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort,
    mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbClose, mbHelp);
var
  Form: TDialogForm;
  Content: TDialogContent;
  Check: TOBDCheckBox;
  Btn: TOBDButton;
  Metrics: TOBDDensityMetrics;
  HeaderH, BodyH, FooterH, W, H, I, BX, BW, FooterY: Integer;
  Button: TMsgDlgBtn;
  Caption: string;
  MR: TModalResult;
  Area: TRect;
  OwnerForm: TCustomForm;
  Mon: TMonitor;
begin
  Metrics := DensityMetrics(FDensity);
  HeaderH := System.Math.Max(24, Metrics.TitleBar - 6);
  BodyH := 128;
  if Pos(sLineBreak, FText) > 0 then
    Inc(BodyH, 24);
  if FCheckBoxCaption <> '' then
    BodyH := System.Math.Max(BodyH, 126);
  FooterH := Metrics.Button + 28;
  W := 470;
  H := HeaderH + BodyH + FooterH + 1;

  Form := TDialogForm.CreateDialog(Application, Self);
  try
    Form.SetBounds(0, 0, W, H);
    Content := TDialogContent.CreateContent(Form, Self);
    Content.Parent := Form;
    Content.SetBounds(1, HeaderH + 2, W - 2, BodyH - 2);

    Check := nil;
    if FCheckBoxCaption <> '' then
    begin
      Check := TOBDCheckBox.Create(Form);
      Check.Parent := Form;
      Check.Theme := FTheme;
      Check.Density := FDensity;
      Check.ParentDensity := False;
      Check.Caption := FCheckBoxCaption;
      Check.Checked := FCheckBoxChecked;
      Check.SetBounds(56, HeaderH + 84, W - 80, Metrics.Check + 8);
    end;

    FooterY := HeaderH + BodyH;
    BX := W - 16;
    Form.DefaultResult := ModalResultOf(FDefaultButton);
    Form.CancelResult := mrCancel;
    for I := Low(ORDER) to High(ORDER) do
    begin
      Button := ORDER[I];
      if not (Button in FButtons) then
        Continue;
      Caption := CaptionForButton(Button);
      BW := System.Math.Max(76, OBDMeasureText(Caption, 12.5, twSemibold,
        Screen.PixelsPerInch) + 28);
      Dec(BX, BW);
      Btn := TOBDButton.Create(Form);
      Btn.Parent := Form;
      Btn.Theme := FTheme;
      Btn.Density := FDensity;
      Btn.ParentDensity := False;
      Btn.Caption := Caption;
      if Button = FDangerButton then
        Btn.Kind := bkDanger
      else if Button = FDefaultButton then
        Btn.Kind := bkPrimary
      else if Button in [mbCancel, mbNo, mbClose] then
        Btn.Kind := bkSecondary
      else
        Btn.Kind := bkGhost;
      MR := ModalResultOf(Button);
      Btn.ModalResult := MR;
      Btn.Default := Button = FDefaultButton;
      Btn.Cancel := Button in [mbCancel, mbNo, mbClose];
      if Btn.Cancel then
        Form.CancelResult := MR;
      Btn.SetBounds(BX, FooterY + 14, BW, Metrics.Button);
      Dec(BX, 10);
    end;

    OwnerForm := Screen.ActiveCustomForm;
    if OwnerForm <> nil then
      Area := OwnerForm.BoundsRect
    else
    begin
      Mon := Screen.MonitorFromWindow(Application.Handle, mdNearest);
      if Mon <> nil then
        Area := Mon.WorkareaRect
      else
        Area := Screen.DesktopRect;
    end;
    Form.Left := Area.Left + (Area.Width - W) div 2;
    Form.Top := Area.Top + (Area.Height - H) div 2;

    Result := Form.ShowModal;
    if Check <> nil then
      FCheckBoxChecked := Check.Checked;
  finally
    Form.Free;
  end;
end;

function OBDMessageDlg(const ATitle, AText: string; AKind: TOBDDialogKind;
  AButtons: TMsgDlgButtons; ATheme: TOBDTheme): TModalResult;
var
  Dialog: TOBDDialog;
begin
  Dialog := TOBDDialog.Create(nil);
  try
    Dialog.Title := ATitle;
    Dialog.Text := AText;
    Dialog.Kind := AKind;
    Dialog.Buttons := AButtons;
    Dialog.Theme := ATheme;
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

end.
