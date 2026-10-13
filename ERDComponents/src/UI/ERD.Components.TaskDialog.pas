//------------------------------------------------------------------------------
//  ERD.Components.TaskDialog
//
//  Themed modal task dialog for Application.
//
//    TERDTaskDialog  replacement for VCL TTaskDialog with common buttons,
//                    command links, radio buttons, details, verification
//                    and progress.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed text dialogs.
//------------------------------------------------------------------------------

unit ERD.Components.TaskDialog;

interface

uses
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  ERD.Components.Paint,
  ERD.Components.Control,
  ERD.Components.Buttons,
  ERD.Components.Progress,
  ERD.Components.Dialogs;

type
  TERDTaskDialog = class;

  /// <summary>Icon shown next to the task instruction.</summary>
  TERDTaskIcon = (
    /// <summary>No icon.</summary>
    tdiNone,
    /// <summary>Information icon.</summary>
    tdiInfo,
    /// <summary>Warning icon.</summary>
    tdiWarning,
    /// <summary>Error icon.</summary>
    tdiError,
    /// <summary>Success icon.</summary>
    tdiSuccess);

  /// <summary>Common task dialog button.</summary>
  TERDTaskCommonButton = (
    /// <summary>OK button.</summary>
    otcbOk,
    /// <summary>Yes button.</summary>
    otcbYes,
    /// <summary>No button.</summary>
    otcbNo,
    /// <summary>Cancel button.</summary>
    otcbCancel,
    /// <summary>Retry button.</summary>
    otcbRetry,
    /// <summary>Close button.</summary>
    otcbClose);

  /// <summary>Set of common task dialog buttons.</summary>
  TERDTaskCommonButtons = set of TERDTaskCommonButton;

  /// <summary>Visual kind for custom task buttons.</summary>
  TERDTaskButtonKind = (
    /// <summary>Secondary button.</summary>
    otbkNormal,
    /// <summary>Primary accent button.</summary>
    otbkPrimary,
    /// <summary>Danger button or command link.</summary>
    otbkDanger);

  /// <summary>Custom streamable task button or command link.</summary>
  TERDTaskButtonItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FHint: string;
    FModalResult: TModalResult;
    FDefault: Boolean;
    FKind: TERDTaskButtonKind;
    procedure SetCaption(const AValue: string);
    procedure SetHint(const AValue: string);
    procedure SetModalResult(AValue: TModalResult);
    procedure SetDefault(AValue: Boolean);
    procedure SetKind(AValue: TERDTaskButtonKind);
  protected
    /// <summary>Display name used by collection editors.</summary>
    /// <returns>Caption or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a secondary custom button.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies every streamable property from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Button caption or command link title.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Secondary text used by command links.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Modal result returned when this item is chosen.</summary>
    property ModalResult: TModalResult read FModalResult write SetModalResult
      default mrOk;
    /// <summary>True when Enter should choose this custom item.</summary>
    property Default: Boolean read FDefault write SetDefault default False;
    /// <summary>Visual kind of this item.</summary>
    property Kind: TERDTaskButtonKind read FKind write SetKind
      default otbkNormal;
  end;

  /// <summary>Owned collection of custom task buttons.</summary>
  TERDTaskButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDTaskButtonItem;
    procedure SetItem(AIndex: Integer; AValue: TERDTaskButtonItem);
  public
    /// <summary>Creates the collection.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a custom button item.</summary>
    /// <returns>New item.</returns>
    function Add: TERDTaskButtonItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TERDTaskButtonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Streamable task dialog radio choice.</summary>
  TERDTaskRadioButtonItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FValue: Integer;
    procedure SetCaption(const AValue: string);
    procedure SetValue(AValue: Integer);
  protected
    /// <summary>Display name used by collection editors.</summary>
    /// <returns>Caption or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a radio choice.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies every streamable property from another item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Text shown next to the radio button.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Value copied to TERDTaskDialog.RadioButton when checked.</summary>
    property Value: Integer read FValue write SetValue default 0;
  end;

  /// <summary>Owned collection of task radio choices.</summary>
  TERDTaskRadioButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDTaskRadioButtonItem;
    procedure SetItem(AIndex: Integer; AValue: TERDTaskRadioButtonItem);
  public
    /// <summary>Creates the collection.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a radio choice.</summary>
    /// <returns>New item.</returns>
    function Add: TERDTaskRadioButtonItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TERDTaskRadioButtonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Streamable progress settings for a task dialog.</summary>
  TERDTaskProgress = class(TPersistent)
  strict private
    FOwner: TERDTaskDialog;
    FVisible: Boolean;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FMarquee: Boolean;
    procedure Changed;
    procedure SetVisible(AValue: Boolean);
    procedure SetMin(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetMarquee(AValue: Boolean);
  public
    /// <summary>Creates the progress settings object.</summary>
    /// <param name="AOwner">Owning dialog.</param>
    constructor Create(AOwner: TERDTaskDialog);
    /// <summary>Copies every streamable property from another progress object.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Shows the progress bar.</summary>
    property Visible: Boolean read FVisible write SetVisible default False;
    /// <summary>Minimum value.</summary>
    property Min: Integer read FMin write SetMin default 0;
    /// <summary>Maximum value.</summary>
    property Max: Integer read FMax write SetMax default 100;
    /// <summary>Current value.</summary>
    property Position: Integer read FPosition write SetPosition default 0;
    /// <summary>Shows an indeterminate marquee.</summary>
    property Marquee: Boolean read FMarquee write SetMarquee default False;
  end;

  /// <summary>Raised before a task dialog button closes the dialog.</summary>
  /// <param name="Sender">Dialog component.</param>
  /// <param name="AModalResult">Modal result requested by the button.</param>
  /// <param name="ACanClose">Set to False to keep the dialog open.</param>
  TERDTaskButtonClickEvent = procedure(Sender: TObject;
    AModalResult: TModalResult; var ACanClose: Boolean) of object;

  /// <summary>Themed replacement for VCL TTaskDialog.</summary>
  TERDTaskDialog = class(TERDCommonDialog)
  strict private
    FMainInstruction: string;
    FText: string;
    FExpandedText: string;
    FFooterText: string;
    FMainIcon: TERDTaskIcon;
    FCommonButtons: TERDTaskCommonButtons;
    FDefaultButton: TERDTaskCommonButton;
    FButtons: TERDTaskButtons;
    FCommandLinks: Boolean;
    FRadioButtons: TERDTaskRadioButtons;
    FRadioButton: Integer;
    FVerificationText: string;
    FVerificationChecked: Boolean;
    FProgressBar: TERDTaskProgress;
    FModalResult: TModalResult;
    FTimerInterval: Integer;
    FOnButtonClicked: TERDTaskButtonClickEvent;
    FOnTimer: TNotifyEvent;
    procedure SetMainInstruction(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetExpandedText(const AValue: string);
    procedure SetFooterText(const AValue: string);
    procedure SetMainIcon(AValue: TERDTaskIcon);
    procedure SetCommonButtons(AValue: TERDTaskCommonButtons);
    procedure SetDefaultButton(AValue: TERDTaskCommonButton);
    procedure SetButtons(AValue: TERDTaskButtons);
    procedure SetCommandLinks(AValue: Boolean);
    procedure SetRadioButtons(AValue: TERDTaskRadioButtons);
    procedure SetRadioButton(AValue: Integer);
    procedure SetVerificationText(const AValue: string);
    procedure SetVerificationChecked(AValue: Boolean);
    procedure SetProgressBar(AValue: TERDTaskProgress);
    procedure SetTimerInterval(AValue: Integer);
    procedure RadioChanged(Sender: TObject);
    procedure ControlButtonClick(Sender: TObject);
    procedure TimerTick(Sender: TObject);
    function BodyHeight: Integer;
  protected
    /// <summary>Default window title used while Title is empty.</summary>
    /// <returns>Dialog title.</returns>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates a task dialog.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees streamable child objects.</summary>
    destructor Destroy; override;
    /// <summary>Shows the task dialog modally.</summary>
    /// <returns>True unless the user cancelled or closed the dialog.</returns>
    function Execute: Boolean; override;
  published
    /// <summary>Main heading. Caption is an alias for MainInstruction.</summary>
    property Caption: string read FMainInstruction write SetMainInstruction;
    /// <summary>Main heading shown in a heading style.</summary>
    property MainInstruction: string read FMainInstruction
      write SetMainInstruction;
    /// <summary>Wrapped body text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Text shown by the Show details / Hide details toggle.</summary>
    property ExpandedText: string read FExpandedText write SetExpandedText;
    /// <summary>Small footer text above the footer strip.</summary>
    property FooterText: string read FFooterText write SetFooterText;
    /// <summary>Icon shown beside the main instruction.</summary>
    property MainIcon: TERDTaskIcon read FMainIcon write SetMainIcon
      default tdiNone;
    /// <summary>Common footer buttons.</summary>
    property CommonButtons: TERDTaskCommonButtons read FCommonButtons
      write SetCommonButtons default [otcbOk];
    /// <summary>Common button selected by Enter.</summary>
    property DefaultButton: TERDTaskCommonButton read FDefaultButton
      write SetDefaultButton default otcbOk;
    /// <summary>Custom buttons or command links.</summary>
    property Buttons: TERDTaskButtons read FButtons write SetButtons;
    /// <summary>Renders custom Buttons as command-link rows.</summary>
    property CommandLinks: Boolean read FCommandLinks write SetCommandLinks
      default False;
    /// <summary>Radio button choices.</summary>
    property RadioButtons: TERDTaskRadioButtons read FRadioButtons
      write SetRadioButtons;
    /// <summary>Selected radio button value.</summary>
    property RadioButton: Integer read FRadioButton write SetRadioButton
      default 0;
    /// <summary>Verification check box text in the footer.</summary>
    property VerificationText: string read FVerificationText
      write SetVerificationText;
    /// <summary>Verification check box state.</summary>
    property VerificationChecked: Boolean read FVerificationChecked
      write SetVerificationChecked default False;
    /// <summary>Progress bar settings.</summary>
    property ProgressBar: TERDTaskProgress read FProgressBar
      write SetProgressBar;
    /// <summary>Modal result selected by the user.</summary>
    property ModalResult: TModalResult read FModalResult write FModalResult
      default mrNone;
    /// <summary>Timer interval in milliseconds; 0 disables the timer.</summary>
    property TimerInterval: Integer read FTimerInterval
      write SetTimerInterval default 0;
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme;
    /// <summary>Desktop or tablet sizing; follows the theme until set.</summary>
    property Density;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity;
    /// <summary>Text in the title strip; empty = the default title.</summary>
    property Title;
    /// <summary>Fires just before the window is shown.</summary>
    property OnShow;
    /// <summary>Fires after the window closed.</summary>
    property OnClose;
    /// <summary>Fires before a button closes the dialog.</summary>
    property OnButtonClicked: TERDTaskButtonClickEvent read FOnButtonClicked
      write FOnButtonClicked;
    /// <summary>Fires while the modal dialog is visible when TimerInterval is set.</summary>
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

type
  TTaskIconControl = class(TERDCustomControl)
  strict private
    FIcon: TERDTaskIcon;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreateIcon(AOwner: TComponent; AIcon: TERDTaskIcon);
  end;

  TTaskCommandLink = class(TERDCustomControl)
  strict private
    FCaption: string;
    FNote: string;
    FKind: TERDTaskButtonKind;
    FModalResult: TModalResult;
    FHover: Boolean;
    FPressed: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor CreateLink(AOwner: TComponent; const ACaption, ANote: string;
      AKind: TERDTaskButtonKind; AModalResult: TModalResult);
    property ModalResult: TModalResult read FModalResult;
  end;

  TDetailsController = class(TComponent)
  strict private
    FWindow: TERDDialogWindow;
    FButton: TERDButton;
    FText: TERDDialogText;
    FDetailH: Integer;
    FExpanded: Boolean;
  public
    procedure Setup(AWindow: TERDDialogWindow; AButton: TERDButton;
      AText: TERDDialogText; ADetailH: Integer);
    procedure Toggle(Sender: TObject);
  end;

function CommonButtonCaption(AButton: TERDTaskCommonButton): string;
begin
  case AButton of
    otcbYes:
      Result := 'Yes';
    otcbNo:
      Result := 'No';
    otcbCancel:
      Result := 'Cancel';
    otcbRetry:
      Result := 'Retry';
    otcbClose:
      Result := 'Close';
  else
    Result := 'OK';
  end;
end;

function CommonButtonResult(AButton: TERDTaskCommonButton): TModalResult;
begin
  case AButton of
    otcbYes:
      Result := mrYes;
    otcbNo:
      Result := mrNo;
    otcbCancel:
      Result := mrCancel;
    otcbRetry:
      Result := mrRetry;
    otcbClose:
      Result := mrClose;
  else
    Result := mrOk;
  end;
end;

function ButtonKind(AKind: TERDTaskButtonKind; ADefault: Boolean): TERDButtonKind;
begin
  if AKind = otbkDanger then
    Result := bkDanger
  else if ADefault or (AKind = otbkPrimary) then
    Result := bkPrimary
  else
    Result := bkSecondary;
end;

function IconColor(AIcon: TERDTaskIcon; const APalette: TERDThemePalette): TColor;
begin
  case AIcon of
    tdiWarning:
      Result := APalette.Warning;
    tdiError:
      Result := APalette.Danger;
    tdiSuccess:
      Result := APalette.Success;
  else
    Result := APalette.Accent;
  end;
end;

{ TERDTaskButtonItem }

constructor TERDTaskButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FModalResult := mrOk;
  FKind := otbkNormal;
end;

procedure TERDTaskButtonItem.Assign(Source: TPersistent);
var
  Item: TERDTaskButtonItem;
begin
  if Source is TERDTaskButtonItem then
  begin
    Item := TERDTaskButtonItem(Source);
    FCaption := Item.FCaption;
    FHint := Item.FHint;
    FModalResult := Item.FModalResult;
    FDefault := Item.FDefault;
    FKind := Item.FKind;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDTaskButtonItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDTaskButtonItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDTaskButtonItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDTaskButtonItem.SetModalResult(AValue: TModalResult);
begin
  if FModalResult = AValue then
    Exit;
  FModalResult := AValue;
  Changed(False);
end;

procedure TERDTaskButtonItem.SetDefault(AValue: Boolean);
begin
  if FDefault = AValue then
    Exit;
  FDefault := AValue;
  Changed(False);
end;

procedure TERDTaskButtonItem.SetKind(AValue: TERDTaskButtonKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

{ TERDTaskButtons }

constructor TERDTaskButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDTaskButtonItem);
end;

function TERDTaskButtons.Add: TERDTaskButtonItem;
begin
  Result := TERDTaskButtonItem(inherited Add);
end;

function TERDTaskButtons.GetItem(AIndex: Integer): TERDTaskButtonItem;
begin
  Result := TERDTaskButtonItem(inherited GetItem(AIndex));
end;

procedure TERDTaskButtons.SetItem(AIndex: Integer; AValue: TERDTaskButtonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TERDTaskRadioButtonItem }

constructor TERDTaskRadioButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TERDTaskRadioButtonItem.Assign(Source: TPersistent);
var
  Item: TERDTaskRadioButtonItem;
begin
  if Source is TERDTaskRadioButtonItem then
  begin
    Item := TERDTaskRadioButtonItem(Source);
    FCaption := Item.FCaption;
    FValue := Item.FValue;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDTaskRadioButtonItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDTaskRadioButtonItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDTaskRadioButtonItem.SetValue(AValue: Integer);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  Changed(False);
end;

{ TERDTaskRadioButtons }

constructor TERDTaskRadioButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDTaskRadioButtonItem);
end;

function TERDTaskRadioButtons.Add: TERDTaskRadioButtonItem;
begin
  Result := TERDTaskRadioButtonItem(inherited Add);
end;

function TERDTaskRadioButtons.GetItem(AIndex: Integer): TERDTaskRadioButtonItem;
begin
  Result := TERDTaskRadioButtonItem(inherited GetItem(AIndex));
end;

procedure TERDTaskRadioButtons.SetItem(AIndex: Integer;
  AValue: TERDTaskRadioButtonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TERDTaskProgress }

constructor TERDTaskProgress.Create(AOwner: TERDTaskDialog);
begin
  inherited Create;
  FOwner := AOwner;
  FMax := 100;
end;

procedure TERDTaskProgress.Assign(Source: TPersistent);
var
  Progress: TERDTaskProgress;
begin
  if Source is TERDTaskProgress then
  begin
    Progress := TERDTaskProgress(Source);
    FVisible := Progress.FVisible;
    FMin := Progress.FMin;
    FMax := Progress.FMax;
    FPosition := Progress.FPosition;
    FMarquee := Progress.FMarquee;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TERDTaskProgress.Changed;
begin
  if FOwner <> nil then
    FOwner.ThemeChanged;
end;

procedure TERDTaskProgress.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed;
end;

procedure TERDTaskProgress.SetMin(AValue: Integer);
begin
  if FMin = AValue then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  FPosition := EnsureRange(FPosition, FMin, FMax);
  Changed;
end;

procedure TERDTaskProgress.SetMax(AValue: Integer);
begin
  if FMax = AValue then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  FPosition := EnsureRange(FPosition, FMin, FMax);
  Changed;
end;

procedure TERDTaskProgress.SetPosition(AValue: Integer);
begin
  AValue := EnsureRange(AValue, FMin, FMax);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Changed;
end;

procedure TERDTaskProgress.SetMarquee(AValue: Boolean);
begin
  if FMarquee = AValue then
    Exit;
  FMarquee := AValue;
  Changed;
end;

{ TTaskIconControl }

constructor TTaskIconControl.CreateIcon(AOwner: TComponent; AIcon: TERDTaskIcon);
begin
  inherited Create(AOwner);
  FIcon := AIcon;
  TabStop := False;
end;

procedure TTaskIconControl.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  C: TColor;
  CX, CY, R: Integer;
begin
  if FIcon = tdiNone then
    Exit;
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    C := IconColor(FIcon, Palette);
    CX := Width div 2;
    CY := Height div 2;
    R := System.Math.Min(Width, Height) div 2 - ScaleValue(2);
    Painter.Ellipse(CX - R, CY - R, 2 * R, 2 * R, Painter.Tint(C), C,
      Painter.SF(1));
    case FIcon of
      tdiWarning:
        Painter.GlyphPending(CX, CY, C, 1.35);
      tdiError:
        Painter.InkText(CX, CY, '!', 16, C, twBold);
      tdiSuccess:
        Painter.GlyphCheck(CX, CY, C, 1.35);
    else
      Painter.InkText(CX, CY, 'i', 16, C, twBold);
    end;
  finally
    Painter.Free;
  end;
end;

{ TTaskCommandLink }

constructor TTaskCommandLink.CreateLink(AOwner: TComponent;
  const ACaption, ANote: string; AKind: TERDTaskButtonKind;
  AModalResult: TModalResult);
begin
  inherited Create(AOwner);
  FCaption := ACaption;
  FNote := ANote;
  FKind := AKind;
  FModalResult := AModalResult;
  TabStop := True;
  Width := 400;
  Height := 56;
end;

procedure TTaskCommandLink.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHover := True;
  Invalidate;
end;

procedure TTaskCommandLink.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHover := False;
  FPressed := False;
  Invalidate;
end;

procedure TTaskCommandLink.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TTaskCommandLink.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    FPressed := True;
    if CanFocus and not Focused then
      SetFocus;
    Invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TTaskCommandLink.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and FPressed then
  begin
    FPressed := False;
    Invalidate;
    if PtInRect(ClientRect, Point(X, Y)) then
      Click;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TTaskCommandLink.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_RETURN) or (Key = VK_SPACE)) and (Shift = []) then
  begin
    Key := 0;
    Click;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TTaskCommandLink.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TTaskCommandLink.DoExit;
begin
  inherited DoExit;
  FPressed := False;
  Invalidate;
end;

procedure TTaskCommandLink.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  R: TRect;
  Ink, Fill, Outline: TColor;
  Mid: Integer;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, Palette.Background);
    R := Rect(0, 0, Width, Height);
    if FKind = otbkDanger then
      Ink := Painter.DangerInk
    else
      Ink := Painter.AccentText;
    Fill := clNone;
    Outline := clNone;
    if FHover or FPressed or Focused then
    begin
      Fill := Painter.Tint(Palette.Accent, 0.14);
      Outline := Painter.AccentText;
    end;
    if FKind = otbkDanger then
      Fill := Painter.Tint(Palette.Danger, 0.12);
    Painter.FillRect(R, Fill);
    Painter.FrameRect(R, Outline);
    Mid := Height div 2;
    Painter.Lines([ERDPointF(ScaleValue(8), Mid - ScaleValue(9)),
      ERDPointF(ScaleValue(18), Mid - ScaleValue(9))], Ink, Painter.SF(1.8));
    Painter.Lines([ERDPointF(ScaleValue(14), Mid - ScaleValue(13)),
      ERDPointF(ScaleValue(18), Mid - ScaleValue(9)),
      ERDPointF(ScaleValue(14), Mid - ScaleValue(5))], Ink, Painter.SF(1.8));
    Painter.Text(ScaleValue(30), Mid - ScaleValue(9), FCaption,
      ERD_TEXT_HEADING, Ink, twSemibold, taLeftJustify,
      Width - ScaleValue(44));
    Painter.Text(ScaleValue(30), Mid + ScaleValue(10), FNote,
      ERD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
      Width - ScaleValue(44));
    if Focused and not IsPreview then
      Painter.FocusRing(0, 0, Width, Height);
  finally
    Painter.Free;
  end;
end;

{ TDetailsController }

procedure TDetailsController.Setup(AWindow: TERDDialogWindow;
  AButton: TERDButton; AText: TERDDialogText; ADetailH: Integer);
begin
  FWindow := AWindow;
  FButton := AButton;
  FText := AText;
  FDetailH := ADetailH;
  FExpanded := False;
end;

procedure TDetailsController.Toggle(Sender: TObject);
var
  Delta: Integer;
begin
  if (FWindow = nil) or (FButton = nil) or (FText = nil) then
    Exit;
  Delta := FWindow.Dp(FDetailH);
  FExpanded := not FExpanded;
  FText.Visible := FExpanded;
  if FExpanded then
  begin
    FButton.Caption := 'Hide details';
    FWindow.ClientHeight := FWindow.ClientHeight + Delta;
  end
  else
  begin
    FButton.Caption := 'Show details';
    FWindow.ClientHeight := FWindow.ClientHeight - Delta;
  end;
  // The footer is anchored to the bottom; pin it explicitly as well so the
  // result does not depend on when the anchors are applied.
  if FWindow.Footer <> nil then
    FWindow.Footer.Top := FWindow.ClientHeight - 1 - FWindow.Footer.Height;
end;

{ TERDTaskDialog }

constructor TERDTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonButtons := [otcbOk];
  FDefaultButton := otcbOk;
  FModalResult := mrNone;
  FButtons := TERDTaskButtons.Create(Self);
  FRadioButtons := TERDTaskRadioButtons.Create(Self);
  FProgressBar := TERDTaskProgress.Create(Self);
end;

destructor TERDTaskDialog.Destroy;
begin
  FProgressBar.Free;
  FRadioButtons.Free;
  FButtons.Free;
  inherited Destroy;
end;

function TERDTaskDialog.DefaultTitle: string;
begin
  Result := 'Application';
end;

procedure TERDTaskDialog.SetMainInstruction(const AValue: string);
begin
  FMainInstruction := AValue;
end;

procedure TERDTaskDialog.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TERDTaskDialog.SetExpandedText(const AValue: string);
begin
  FExpandedText := AValue;
end;

procedure TERDTaskDialog.SetFooterText(const AValue: string);
begin
  FFooterText := AValue;
end;

procedure TERDTaskDialog.SetMainIcon(AValue: TERDTaskIcon);
begin
  FMainIcon := AValue;
end;

procedure TERDTaskDialog.SetCommonButtons(AValue: TERDTaskCommonButtons);
begin
  FCommonButtons := AValue;
end;

procedure TERDTaskDialog.SetDefaultButton(AValue: TERDTaskCommonButton);
begin
  FDefaultButton := AValue;
end;

procedure TERDTaskDialog.SetButtons(AValue: TERDTaskButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TERDTaskDialog.SetCommandLinks(AValue: Boolean);
begin
  FCommandLinks := AValue;
end;

procedure TERDTaskDialog.SetRadioButtons(AValue: TERDTaskRadioButtons);
begin
  FRadioButtons.Assign(AValue);
end;

procedure TERDTaskDialog.SetRadioButton(AValue: Integer);
begin
  FRadioButton := AValue;
end;

procedure TERDTaskDialog.SetVerificationText(const AValue: string);
begin
  FVerificationText := AValue;
end;

procedure TERDTaskDialog.SetVerificationChecked(AValue: Boolean);
begin
  FVerificationChecked := AValue;
end;

procedure TERDTaskDialog.SetProgressBar(AValue: TERDTaskProgress);
begin
  FProgressBar.Assign(AValue);
end;

procedure TERDTaskDialog.SetTimerInterval(AValue: Integer);
begin
  FTimerInterval := System.Math.Max(0, AValue);
end;

function TERDTaskDialog.BodyHeight: Integer;
var
  LinkH, I: Integer;
begin
  LinkH := System.Math.Max(52, DensityMetrics(Density).Button + 18);
  Result := 22 + 70;
  if FCommandLinks and (FButtons.Count > 0) then
    Inc(Result, FButtons.Count * (LinkH + 6) + 10);
  if FRadioButtons.Count > 0 then
    Inc(Result, 18 + FRadioButtons.Count * 30 + 10);
  if FProgressBar.Visible then
    Inc(Result, 36);
  if FFooterText <> '' then
    Inc(Result, 30);
  if FExpandedText <> '' then
    Inc(Result, DensityMetrics(Density).Button + 8);
  for I := 1 to Length(FText) div 80 do
    Inc(Result, 18);
  Inc(Result, 16);
end;

procedure TERDTaskDialog.RadioChanged(Sender: TObject);
begin
  if (Sender is TERDRadioButton) and TERDRadioButton(Sender).Checked then
    FRadioButton := TERDRadioButton(Sender).Tag;
end;

procedure TERDTaskDialog.ControlButtonClick(Sender: TObject);
var
  ResultValue: TModalResult;
  CanClose: Boolean;
  Form: TCustomForm;
begin
  ResultValue := mrNone;
  if Sender is TERDButton then
    ResultValue := TERDButton(Sender).ModalResult
  else if Sender is TTaskCommandLink then
    ResultValue := TTaskCommandLink(Sender).ModalResult;
  CanClose := True;
  if Assigned(FOnButtonClicked) then
    FOnButtonClicked(Self, ResultValue, CanClose);
  Form := nil;
  if Sender is TControl then
    Form := GetParentForm(TControl(Sender));
  if CanClose then
  begin
    FModalResult := ResultValue;
    if Form <> nil then
      Form.ModalResult := ResultValue;
  end
  else if Form <> nil then
    Form.ModalResult := mrNone;
end;

procedure TERDTaskDialog.TimerTick(Sender: TObject);
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

function TERDTaskDialog.Execute: Boolean;
const
  COMMON_ORDER: array[0..5] of TERDTaskCommonButton = (otcbOk, otcbYes,
    otcbNo, otcbCancel, otcbRetry, otcbClose);
var
  Window: TERDDialogWindow;
  Icon: TTaskIconControl;
  LabelText, DetailText: TERDDialogText;
  Details: TDetailsController;
  DetailButton: TERDButton;
  Progress: TERDProgressBar;
  Verify: TERDCheckBox;
  Timer: TTimer;
  Button: TERDButton;
  Link: TTaskCommandLink;
  Radio: TERDRadioButton;
  Item: TERDTaskButtonItem;
  RItem: TERDTaskRadioButtonItem;
  MR: TModalResult;
  I, Y, L, W, LinkH, DetailH, DetailButtonH: Integer;
  CB: TERDTaskCommonButton;
  Default: Boolean;
begin
  Window := NewWindow;
  try
    Window.SetLayout(600, BodyHeight, True);
    Window.CancelResult := mrCancel;
    L := 56;
    W := 600 - L - 16;
    Y := 22;

    Icon := TTaskIconControl.CreateIcon(Window, FMainIcon);
    Window.Place(Icon, 16, Y - 4, 32, 36);
    if FMainInstruction <> '' then
      Window.AddText(L, Y, W, FMainInstruction, dtsHeading, 24);
    if FText <> '' then
    begin
      LabelText := Window.AddText(L, Y + 24, W, FText, dtsBody, 40);
      LabelText.WordWrap := True;
    end;
    Inc(Y, 70);

    LinkH := System.Math.Max(52, DensityMetrics(Density).Button + 18);
    if FCommandLinks and (FButtons.Count > 0) then
    begin
      for I := 0 to FButtons.Count - 1 do
      begin
        Item := FButtons[I];
        Link := TTaskCommandLink.CreateLink(Window, Item.Caption, Item.Hint,
          Item.Kind, Item.ModalResult);
        Link.OnClick := ControlButtonClick;
        Window.Place(Link, L - 8, Y, W + 8, LinkH);
        if Item.Default then
          Window.InitialFocus := Link;
        Inc(Y, LinkH + 6);
      end;
      Inc(Y, 10);
    end;

    if FRadioButtons.Count > 0 then
    begin
      Window.AddText(L, Y, W, 'Choice', dtsCaps);
      Inc(Y, 18);
      for I := 0 to FRadioButtons.Count - 1 do
      begin
        RItem := FRadioButtons[I];
        Radio := TERDRadioButton.Create(Window);
        Radio.Caption := RItem.Caption;
        Radio.GroupIndex := 4211;
        Radio.Tag := RItem.Value;
        Radio.Checked := RItem.Value = FRadioButton;
        Radio.OnChange := RadioChanged;
        Window.Place(Radio, L, Y, W, 30);
        Inc(Y, 30);
      end;
      Inc(Y, 10);
    end;

    if FProgressBar.Visible then
    begin
      Progress := TERDProgressBar.Create(Window);
      Progress.Min := FProgressBar.Min;
      Progress.Max := FProgressBar.Max;
      Progress.Position := FProgressBar.Position;
      Progress.Indeterminate := FProgressBar.Marquee;
      Progress.Caption := 'Progress';
      Window.Place(Progress, L, Y, W, 36);
      Inc(Y, 36);
    end;

    if FFooterText <> '' then
    begin
      LabelText := Window.AddText(L, Y, W, FFooterText, dtsSmall, 24);
      LabelText.WordWrap := True;
      Inc(Y, 30);
    end;

    if FExpandedText <> '' then
    begin
      DetailButton := TERDButton.Create(Window);
      DetailButton.Kind := bkGhost;
      DetailButton.Caption := 'Show details';
      DetailButton.Glyph := glChevronRight;
      DetailButtonH := DensityMetrics(Density).Button + 8;
      Window.Place(DetailButton, L - 10, Y, MulDiv(Window.ButtonWidth(
        'Hide details', glChevronRight), 96, Window.PPI), DetailButtonH);
      DetailH := 54;
      DetailText := Window.AddText(L, Y + DetailButtonH, W, FExpandedText,
        dtsSmall, DetailH);
      DetailText.WordWrap := True;
      DetailText.Visible := False;
      Details := TDetailsController.Create(Window);
      Details.Setup(Window, DetailButton, DetailText, DetailH);
      DetailButton.OnClick := Details.Toggle;
    end;

    Verify := nil;
    if FVerificationText <> '' then
      Verify := Window.AddLeftCheck(FVerificationText, FVerificationChecked);

    for I := 0 to FButtons.Count - 1 do
      if not FCommandLinks then
      begin
        Item := FButtons[I];
        Button := Window.AddButton(Item.Caption, ButtonKind(Item.Kind,
          Item.Default), Item.ModalResult);
        Button.Default := Item.Default;
        Button.Cancel := Item.ModalResult in [mrCancel, mrClose];
        Button.OnClick := ControlButtonClick;
        if Item.Default then
          Window.InitialFocus := Button;
      end;

    for I := Low(COMMON_ORDER) to High(COMMON_ORDER) do
    begin
      CB := COMMON_ORDER[I];
      if not (CB in FCommonButtons) then
        Continue;
      MR := CommonButtonResult(CB);
      Default := CB = FDefaultButton;
      Button := Window.AddButton(CommonButtonCaption(CB), ButtonKind(otbkNormal,
        Default), MR);
      Button.Default := Default;
      Button.Cancel := MR in [mrCancel, mrClose, mrNo];
      Button.OnClick := ControlButtonClick;
      if Button.Cancel then
        Window.CancelResult := MR;
      if Default and (Window.InitialFocus = nil) then
        Window.InitialFocus := Button;
    end;

    Timer := nil;
    if Assigned(FOnTimer) and (FTimerInterval > 0) then
    begin
      Timer := TTimer.Create(Window);
      Timer.Interval := FTimerInterval;
      Timer.OnTimer := TimerTick;
      Timer.Enabled := True;
    end;

    MR := RunWindow(Window);
    if Timer <> nil then
      Timer.Enabled := False;
    if MR <> mrNone then
      FModalResult := MR;
    if Verify <> nil then
      FVerificationChecked := Verify.Checked;
    Result := not (FModalResult in [mrCancel, mrClose]);
  finally
    Window.Free;
  end;
end;

end.
