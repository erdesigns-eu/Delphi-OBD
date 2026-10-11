//------------------------------------------------------------------------------
//  ERD.UI.TaskDialog
//
//  Themed modal task dialog for OBD Studio.
//
//    TOBDTaskDialog  replacement for VCL TTaskDialog with common buttons,
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

unit ERD.UI.TaskDialog;

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
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Control,
  ERD.UI.Buttons,
  ERD.UI.Progress,
  ERD.UI.Dialogs;

type
  TOBDTaskDialog = class;

  /// <summary>Icon shown next to the task instruction.</summary>
  TOBDTaskIcon = (
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
  TOBDTaskCommonButton = (
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
  TOBDTaskCommonButtons = set of TOBDTaskCommonButton;

  /// <summary>Visual kind for custom task buttons.</summary>
  TOBDTaskButtonKind = (
    /// <summary>Secondary button.</summary>
    otbkNormal,
    /// <summary>Primary accent button.</summary>
    otbkPrimary,
    /// <summary>Danger button or command link.</summary>
    otbkDanger);

  /// <summary>Custom streamable task button or command link.</summary>
  TOBDTaskButtonItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FHint: string;
    FModalResult: TModalResult;
    FDefault: Boolean;
    FKind: TOBDTaskButtonKind;
    procedure SetCaption(const AValue: string);
    procedure SetHint(const AValue: string);
    procedure SetModalResult(AValue: TModalResult);
    procedure SetDefault(AValue: Boolean);
    procedure SetKind(AValue: TOBDTaskButtonKind);
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
    property Kind: TOBDTaskButtonKind read FKind write SetKind
      default otbkNormal;
  end;

  /// <summary>Owned collection of custom task buttons.</summary>
  TOBDTaskButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDTaskButtonItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDTaskButtonItem);
  public
    /// <summary>Creates the collection.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a custom button item.</summary>
    /// <returns>New item.</returns>
    function Add: TOBDTaskButtonItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDTaskButtonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Streamable task dialog radio choice.</summary>
  TOBDTaskRadioButtonItem = class(TCollectionItem)
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
    /// <summary>Value copied to TOBDTaskDialog.RadioButton when checked.</summary>
    property Value: Integer read FValue write SetValue default 0;
  end;

  /// <summary>Owned collection of task radio choices.</summary>
  TOBDTaskRadioButtons = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDTaskRadioButtonItem;
    procedure SetItem(AIndex: Integer; AValue: TOBDTaskRadioButtonItem);
  public
    /// <summary>Creates the collection.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a radio choice.</summary>
    /// <returns>New item.</returns>
    function Add: TOBDTaskRadioButtonItem;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDTaskRadioButtonItem read GetItem
      write SetItem; default;
  end;

  /// <summary>Streamable progress settings for a task dialog.</summary>
  TOBDTaskProgress = class(TPersistent)
  strict private
    FOwner: TOBDTaskDialog;
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
    constructor Create(AOwner: TOBDTaskDialog);
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
  TOBDTaskButtonClickEvent = procedure(Sender: TObject;
    AModalResult: TModalResult; var ACanClose: Boolean) of object;

  /// <summary>Themed replacement for VCL TTaskDialog.</summary>
  TOBDTaskDialog = class(TOBDCommonDialog)
  strict private
    FMainInstruction: string;
    FText: string;
    FExpandedText: string;
    FFooterText: string;
    FMainIcon: TOBDTaskIcon;
    FCommonButtons: TOBDTaskCommonButtons;
    FDefaultButton: TOBDTaskCommonButton;
    FButtons: TOBDTaskButtons;
    FCommandLinks: Boolean;
    FRadioButtons: TOBDTaskRadioButtons;
    FRadioButton: Integer;
    FVerificationText: string;
    FVerificationChecked: Boolean;
    FProgressBar: TOBDTaskProgress;
    FModalResult: TModalResult;
    FTimerInterval: Integer;
    FOnButtonClicked: TOBDTaskButtonClickEvent;
    FOnTimer: TNotifyEvent;
    procedure SetMainInstruction(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetExpandedText(const AValue: string);
    procedure SetFooterText(const AValue: string);
    procedure SetMainIcon(AValue: TOBDTaskIcon);
    procedure SetCommonButtons(AValue: TOBDTaskCommonButtons);
    procedure SetDefaultButton(AValue: TOBDTaskCommonButton);
    procedure SetButtons(AValue: TOBDTaskButtons);
    procedure SetCommandLinks(AValue: Boolean);
    procedure SetRadioButtons(AValue: TOBDTaskRadioButtons);
    procedure SetRadioButton(AValue: Integer);
    procedure SetVerificationText(const AValue: string);
    procedure SetVerificationChecked(AValue: Boolean);
    procedure SetProgressBar(AValue: TOBDTaskProgress);
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
    property MainIcon: TOBDTaskIcon read FMainIcon write SetMainIcon
      default tdiNone;
    /// <summary>Common footer buttons.</summary>
    property CommonButtons: TOBDTaskCommonButtons read FCommonButtons
      write SetCommonButtons default [otcbOk];
    /// <summary>Common button selected by Enter.</summary>
    property DefaultButton: TOBDTaskCommonButton read FDefaultButton
      write SetDefaultButton default otcbOk;
    /// <summary>Custom buttons or command links.</summary>
    property Buttons: TOBDTaskButtons read FButtons write SetButtons;
    /// <summary>Renders custom Buttons as command-link rows.</summary>
    property CommandLinks: Boolean read FCommandLinks write SetCommandLinks
      default False;
    /// <summary>Radio button choices.</summary>
    property RadioButtons: TOBDTaskRadioButtons read FRadioButtons
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
    property ProgressBar: TOBDTaskProgress read FProgressBar
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
    property OnButtonClicked: TOBDTaskButtonClickEvent read FOnButtonClicked
      write FOnButtonClicked;
    /// <summary>Fires while the modal dialog is visible when TimerInterval is set.</summary>
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

type
  TTaskIconControl = class(TOBDCustomControl)
  strict private
    FIcon: TOBDTaskIcon;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreateIcon(AOwner: TComponent; AIcon: TOBDTaskIcon);
  end;

  TTaskCommandLink = class(TOBDCustomControl)
  strict private
    FCaption: string;
    FNote: string;
    FKind: TOBDTaskButtonKind;
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
      AKind: TOBDTaskButtonKind; AModalResult: TModalResult);
    property ModalResult: TModalResult read FModalResult;
  end;

  TDetailsController = class(TComponent)
  strict private
    FWindow: TOBDDialogWindow;
    FButton: TOBDButton;
    FText: TOBDDialogText;
    FDetailH: Integer;
    FExpanded: Boolean;
  public
    procedure Setup(AWindow: TOBDDialogWindow; AButton: TOBDButton;
      AText: TOBDDialogText; ADetailH: Integer);
    procedure Toggle(Sender: TObject);
  end;

function CommonButtonCaption(AButton: TOBDTaskCommonButton): string;
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

function CommonButtonResult(AButton: TOBDTaskCommonButton): TModalResult;
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

function ButtonKind(AKind: TOBDTaskButtonKind; ADefault: Boolean): TOBDButtonKind;
begin
  if AKind = otbkDanger then
    Result := bkDanger
  else if ADefault or (AKind = otbkPrimary) then
    Result := bkPrimary
  else
    Result := bkSecondary;
end;

function IconColor(AIcon: TOBDTaskIcon; const APalette: TOBDThemePalette): TColor;
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

{ TOBDTaskButtonItem }

constructor TOBDTaskButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FModalResult := mrOk;
  FKind := otbkNormal;
end;

procedure TOBDTaskButtonItem.Assign(Source: TPersistent);
var
  Item: TOBDTaskButtonItem;
begin
  if Source is TOBDTaskButtonItem then
  begin
    Item := TOBDTaskButtonItem(Source);
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

function TOBDTaskButtonItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDTaskButtonItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDTaskButtonItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDTaskButtonItem.SetModalResult(AValue: TModalResult);
begin
  if FModalResult = AValue then
    Exit;
  FModalResult := AValue;
  Changed(False);
end;

procedure TOBDTaskButtonItem.SetDefault(AValue: Boolean);
begin
  if FDefault = AValue then
    Exit;
  FDefault := AValue;
  Changed(False);
end;

procedure TOBDTaskButtonItem.SetKind(AValue: TOBDTaskButtonKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

{ TOBDTaskButtons }

constructor TOBDTaskButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDTaskButtonItem);
end;

function TOBDTaskButtons.Add: TOBDTaskButtonItem;
begin
  Result := TOBDTaskButtonItem(inherited Add);
end;

function TOBDTaskButtons.GetItem(AIndex: Integer): TOBDTaskButtonItem;
begin
  Result := TOBDTaskButtonItem(inherited GetItem(AIndex));
end;

procedure TOBDTaskButtons.SetItem(AIndex: Integer; AValue: TOBDTaskButtonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TOBDTaskRadioButtonItem }

constructor TOBDTaskRadioButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TOBDTaskRadioButtonItem.Assign(Source: TPersistent);
var
  Item: TOBDTaskRadioButtonItem;
begin
  if Source is TOBDTaskRadioButtonItem then
  begin
    Item := TOBDTaskRadioButtonItem(Source);
    FCaption := Item.FCaption;
    FValue := Item.FValue;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDTaskRadioButtonItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDTaskRadioButtonItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDTaskRadioButtonItem.SetValue(AValue: Integer);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  Changed(False);
end;

{ TOBDTaskRadioButtons }

constructor TOBDTaskRadioButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDTaskRadioButtonItem);
end;

function TOBDTaskRadioButtons.Add: TOBDTaskRadioButtonItem;
begin
  Result := TOBDTaskRadioButtonItem(inherited Add);
end;

function TOBDTaskRadioButtons.GetItem(AIndex: Integer): TOBDTaskRadioButtonItem;
begin
  Result := TOBDTaskRadioButtonItem(inherited GetItem(AIndex));
end;

procedure TOBDTaskRadioButtons.SetItem(AIndex: Integer;
  AValue: TOBDTaskRadioButtonItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TOBDTaskProgress }

constructor TOBDTaskProgress.Create(AOwner: TOBDTaskDialog);
begin
  inherited Create;
  FOwner := AOwner;
  FMax := 100;
end;

procedure TOBDTaskProgress.Assign(Source: TPersistent);
var
  Progress: TOBDTaskProgress;
begin
  if Source is TOBDTaskProgress then
  begin
    Progress := TOBDTaskProgress(Source);
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

procedure TOBDTaskProgress.Changed;
begin
  if FOwner <> nil then
    FOwner.ThemeChanged;
end;

procedure TOBDTaskProgress.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed;
end;

procedure TOBDTaskProgress.SetMin(AValue: Integer);
begin
  if FMin = AValue then
    Exit;
  FMin := AValue;
  if FMax < FMin then
    FMax := FMin;
  FPosition := EnsureRange(FPosition, FMin, FMax);
  Changed;
end;

procedure TOBDTaskProgress.SetMax(AValue: Integer);
begin
  if FMax = AValue then
    Exit;
  FMax := AValue;
  if FMin > FMax then
    FMin := FMax;
  FPosition := EnsureRange(FPosition, FMin, FMax);
  Changed;
end;

procedure TOBDTaskProgress.SetPosition(AValue: Integer);
begin
  AValue := EnsureRange(AValue, FMin, FMax);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Changed;
end;

procedure TOBDTaskProgress.SetMarquee(AValue: Boolean);
begin
  if FMarquee = AValue then
    Exit;
  FMarquee := AValue;
  Changed;
end;

{ TTaskIconControl }

constructor TTaskIconControl.CreateIcon(AOwner: TComponent; AIcon: TOBDTaskIcon);
begin
  inherited Create(AOwner);
  FIcon := AIcon;
  TabStop := False;
end;

procedure TTaskIconControl.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
  C: TColor;
  CX, CY, R: Integer;
begin
  if FIcon = tdiNone then
    Exit;
  Painter := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
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
  const ACaption, ANote: string; AKind: TOBDTaskButtonKind;
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
  Painter: TOBDPainter;
  R: TRect;
  Ink, Fill, Outline: TColor;
  Mid: Integer;
begin
  Painter := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
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
    Painter.Lines([OBDPointF(ScaleValue(8), Mid - ScaleValue(9)),
      OBDPointF(ScaleValue(18), Mid - ScaleValue(9))], Ink, Painter.SF(1.8));
    Painter.Lines([OBDPointF(ScaleValue(14), Mid - ScaleValue(13)),
      OBDPointF(ScaleValue(18), Mid - ScaleValue(9)),
      OBDPointF(ScaleValue(14), Mid - ScaleValue(5))], Ink, Painter.SF(1.8));
    Painter.Text(ScaleValue(30), Mid - ScaleValue(9), FCaption,
      OBD_TEXT_HEADING, Ink, twSemibold, taLeftJustify,
      Width - ScaleValue(44));
    Painter.Text(ScaleValue(30), Mid + ScaleValue(10), FNote,
      OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
      Width - ScaleValue(44));
    if Focused and not IsPreview then
      Painter.FocusRing(0, 0, Width, Height);
  finally
    Painter.Free;
  end;
end;

{ TDetailsController }

procedure TDetailsController.Setup(AWindow: TOBDDialogWindow;
  AButton: TOBDButton; AText: TOBDDialogText; ADetailH: Integer);
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

{ TOBDTaskDialog }

constructor TOBDTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonButtons := [otcbOk];
  FDefaultButton := otcbOk;
  FModalResult := mrNone;
  FButtons := TOBDTaskButtons.Create(Self);
  FRadioButtons := TOBDTaskRadioButtons.Create(Self);
  FProgressBar := TOBDTaskProgress.Create(Self);
end;

destructor TOBDTaskDialog.Destroy;
begin
  FProgressBar.Free;
  FRadioButtons.Free;
  FButtons.Free;
  inherited Destroy;
end;

function TOBDTaskDialog.DefaultTitle: string;
begin
  Result := 'OBD Studio';
end;

procedure TOBDTaskDialog.SetMainInstruction(const AValue: string);
begin
  FMainInstruction := AValue;
end;

procedure TOBDTaskDialog.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TOBDTaskDialog.SetExpandedText(const AValue: string);
begin
  FExpandedText := AValue;
end;

procedure TOBDTaskDialog.SetFooterText(const AValue: string);
begin
  FFooterText := AValue;
end;

procedure TOBDTaskDialog.SetMainIcon(AValue: TOBDTaskIcon);
begin
  FMainIcon := AValue;
end;

procedure TOBDTaskDialog.SetCommonButtons(AValue: TOBDTaskCommonButtons);
begin
  FCommonButtons := AValue;
end;

procedure TOBDTaskDialog.SetDefaultButton(AValue: TOBDTaskCommonButton);
begin
  FDefaultButton := AValue;
end;

procedure TOBDTaskDialog.SetButtons(AValue: TOBDTaskButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TOBDTaskDialog.SetCommandLinks(AValue: Boolean);
begin
  FCommandLinks := AValue;
end;

procedure TOBDTaskDialog.SetRadioButtons(AValue: TOBDTaskRadioButtons);
begin
  FRadioButtons.Assign(AValue);
end;

procedure TOBDTaskDialog.SetRadioButton(AValue: Integer);
begin
  FRadioButton := AValue;
end;

procedure TOBDTaskDialog.SetVerificationText(const AValue: string);
begin
  FVerificationText := AValue;
end;

procedure TOBDTaskDialog.SetVerificationChecked(AValue: Boolean);
begin
  FVerificationChecked := AValue;
end;

procedure TOBDTaskDialog.SetProgressBar(AValue: TOBDTaskProgress);
begin
  FProgressBar.Assign(AValue);
end;

procedure TOBDTaskDialog.SetTimerInterval(AValue: Integer);
begin
  FTimerInterval := System.Math.Max(0, AValue);
end;

function TOBDTaskDialog.BodyHeight: Integer;
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

procedure TOBDTaskDialog.RadioChanged(Sender: TObject);
begin
  if (Sender is TOBDRadioButton) and TOBDRadioButton(Sender).Checked then
    FRadioButton := TOBDRadioButton(Sender).Tag;
end;

procedure TOBDTaskDialog.ControlButtonClick(Sender: TObject);
var
  ResultValue: TModalResult;
  CanClose: Boolean;
  Form: TCustomForm;
begin
  ResultValue := mrNone;
  if Sender is TOBDButton then
    ResultValue := TOBDButton(Sender).ModalResult
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

procedure TOBDTaskDialog.TimerTick(Sender: TObject);
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

function TOBDTaskDialog.Execute: Boolean;
const
  COMMON_ORDER: array[0..5] of TOBDTaskCommonButton = (otcbOk, otcbYes,
    otcbNo, otcbCancel, otcbRetry, otcbClose);
var
  Window: TOBDDialogWindow;
  Icon: TTaskIconControl;
  LabelText, DetailText: TOBDDialogText;
  Details: TDetailsController;
  DetailButton: TOBDButton;
  Progress: TOBDProgressBar;
  Verify: TOBDCheckBox;
  Timer: TTimer;
  Button: TOBDButton;
  Link: TTaskCommandLink;
  Radio: TOBDRadioButton;
  Item: TOBDTaskButtonItem;
  RItem: TOBDTaskRadioButtonItem;
  MR: TModalResult;
  I, Y, L, W, LinkH, DetailH, DetailButtonH: Integer;
  CB: TOBDTaskCommonButton;
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
        Radio := TOBDRadioButton.Create(Window);
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
      Progress := TOBDProgressBar.Create(Window);
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
      DetailButton := TOBDButton.Create(Window);
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
      Inc(Y, DetailButtonH);
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
