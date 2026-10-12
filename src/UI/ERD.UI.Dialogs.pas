//------------------------------------------------------------------------------
//  ERD.UI.Dialogs
//
//  Themed modal dialog support for OBD Studio.
//
//    TOBDDialogWindow   borderless themed form with a title strip, a close
//                       button and a footer; the window every OBD Studio
//                       dialog is built on.
//    TOBDDialogText     caption, body, error and upper-case label text
//                       for dialog layouts.
//    TOBDDialogFooter   footer strip; its buttons sit on the footer fill.
//    TOBDCommonDialog   base of the non-visual dialog components (Theme,
//                       Density, Title, Execute, OnShow, OnClose).
//    TOBDDialog         non-visual MessageDlg replacement.
//    OBDMessageDlg      convenience routine for one-off themed dialogs.
//    OBDInputQuery,     InputQuery, InputBox and ShowMessage
//    OBDInputBox,       replacements with optional validation.
//    OBDShowMessage
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//    2026-10-11  ERD  Shared dialog window, common dialog base and the
//                     input routines for the themed standard dialogs.
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
  ERD.UI.Buttons,
  ERD.UI.Edits;

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

  /// <summary>Look of a <see cref="TOBDDialogText"/>.</summary>
  TOBDDialogTextStyle = (
    /// <summary>Body text in the foreground colour.</summary>
    dtsBody,
    /// <summary>Secondary text in the label colour.</summary>
    dtsSubtle,
    /// <summary>Small upper-case caption above a field.</summary>
    dtsCaps,
    /// <summary>Bold heading.</summary>
    dtsHeading,
    /// <summary>Small validation message in the danger colour.</summary>
    dtsError,
    /// <summary>Small secondary text.</summary>
    dtsSmall);

  /// <summary>Label for dialog layouts: captions above fields, body text
  /// and validation messages, in the type scale of the other controls.
  /// </summary>
  TOBDDialogText = class(TOBDCustomControl)
  strict private
    FStyle: TOBDDialogTextStyle;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    procedure SetStyle(AValue: TOBDDialogTextStyle);
    procedure SetWordWrap(AValue: Boolean);
    procedure SetAlignment(AValue: TAlignment);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    /// <summary>Paints the text on the surface colour.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates a body text label.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>The text.</summary>
    property Caption;
    /// <summary>Look of the text.</summary>
    property Style: TOBDDialogTextStyle read FStyle write SetStyle
      default dtsBody;
    /// <summary>Wraps the text over several lines.</summary>
    property WordWrap: Boolean read FWordWrap write SetWordWrap
      default False;
    /// <summary>Horizontal alignment of single-line text.</summary>
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity;
  end;

  /// <summary>Footer strip of a dialog window. Child controls paint on its
  /// fill.</summary>
  TOBDDialogFooter = class(TOBDCustomControl, IOBDSurface)
  strict private
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    /// <summary>Paints the fill and the divider line.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates the footer.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>IOBDSurface: the footer fill.</summary>
    /// <returns>Footer colour.</returns>
    function SurfaceColor: TColor;
  end;

  /// <summary>Borderless themed form with a title strip, close button and
  /// optional footer. All OBD Studio dialogs are built on it.</summary>
  TOBDDialogWindow = class(TForm, IOBDSurface)
  strict private
    FTheme: TOBDTheme;
    FPalette: TOBDThemePalette;
    FDensity: TOBDDensity;
    FTitle: string;
    FPPI: Integer;
    FHeaderHeight: Integer;
    FFooter: TOBDDialogFooter;
    FButtonRight: Integer;
    FLeftRight: Integer;
    FCancelResult: TModalResult;
    FCloseHot: Boolean;
    FInitialFocus: TWinControl;
    FOnCloseButton: TNotifyEvent;
    FSizable: Boolean;
    FMinSize: TPoint;
    FSizeHit: Integer;
    FSizeOrigin: TPoint;
    FSizeBounds: TRect;
    procedure SetTitle(const AValue: string);
    procedure SetCloseHot(AValue: Boolean);
    function CloseRect: TRect;
    procedure TrackSize;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown);
      message WM_NCLBUTTONDOWN;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Adds the drop shadow and clipping styles.</summary>
    /// <param name="Params">Window parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>Paints the frame, the title strip and the close button.
    /// </summary>
    procedure Paint; override;
    /// <summary>Tracks the close button hover.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Closes on a click on the close button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Escape cancels the dialog.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Moves the focus to InitialFocus the first time the window
    /// activates.</summary>
    procedure Activate; override;
    /// <summary>Records the shown size as the minimum size.</summary>
    procedure DoShow; override;
    /// <summary>Repaints the frame and the footer after a size change.
    /// </summary>
    procedure Resize; override;
  public
    /// <summary>Creates an empty dialog window.</summary>
    /// <param name="AOwner">Component owner.</param>
    /// <param name="ATheme">Optional explicit theme.</param>
    /// <param name="ADensity">Desktop or tablet sizing.</param>
    /// <param name="ATitle">Text in the title strip.</param>
    constructor CreateWindow(AOwner: TComponent; ATheme: TOBDTheme;
      ADensity: TOBDDensity; const ATitle: string);
    /// <summary>Turns a 96 dpi size into pixels of this window.</summary>
    /// <param name="AValue">Size at 96 dpi.</param>
    /// <returns>Size in device pixels.</returns>
    function Dp(AValue: Integer): Integer;
    /// <summary>Sizes the window for a body of the given size.</summary>
    /// <param name="AWidth">Window width at 96 dpi.</param>
    /// <param name="ABodyHeight">Body height at 96 dpi.</param>
    /// <param name="AFooter">Adds the footer strip.</param>
    procedure SetLayout(AWidth, ABodyHeight: Integer; AFooter: Boolean = True);
    /// <summary>Top of the body in device pixels.</summary>
    /// <returns>Body top.</returns>
    function BodyTop: Integer;
    /// <summary>Sizes the body to the controls placed on it, with a 16 px
    /// bottom margin, and moves the footer below them. Call it after the
    /// layout is built and before anchoring controls to the bottom.
    /// </summary>
    procedure FitBody;
    /// <summary>Hit-test code of the resize border at a screen point.
    /// </summary>
    /// <param name="AScreen">Point in screen coordinates.</param>
    /// <returns>HTLEFT..HTBOTTOMRIGHT, or HTNOWHERE when the window is not
    /// sizable or the point is not on its border.</returns>
    function SizingHit(const AScreen: TPoint): Integer;
    /// <summary>Parents a control on the window, or on AParent, and gives
    /// an OBD control the dialog theme and density.</summary>
    /// <param name="AControl">Control to add.</param>
    /// <param name="AParent">Parent; nil = the window.</param>
    procedure Adopt(AControl: TControl; AParent: TWinControl = nil);
    /// <summary>Places a control in body coordinates at 96 dpi.</summary>
    /// <param name="AControl">Control to place.</param>
    /// <param name="X">Left at 96 dpi.</param>
    /// <param name="Y">Top below the title strip at 96 dpi.</param>
    /// <param name="W">Width at 96 dpi.</param>
    /// <param name="H">Height at 96 dpi.</param>
    procedure Place(AControl: TControl; X, Y, W, H: Integer);
    /// <summary>Adds a caption label at body coordinates.</summary>
    /// <param name="X">Left at 96 dpi.</param>
    /// <param name="Y">Top at 96 dpi.</param>
    /// <param name="W">Width at 96 dpi.</param>
    /// <param name="AText">Text.</param>
    /// <param name="AStyle">Look of the text.</param>
    /// <param name="AHeight">Height at 96 dpi; 0 = one line.</param>
    /// <returns>The label.</returns>
    function AddText(X, Y, W: Integer; const AText: string;
      AStyle: TOBDDialogTextStyle = dtsCaps;
      AHeight: Integer = 0): TOBDDialogText;
    /// <summary>Adds a footer button right of the previous one, from the
    /// right edge inwards.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AKind">Visual weight.</param>
    /// <param name="AResult">Modal result; mrNone to handle OnClick.</param>
    /// <returns>The button.</returns>
    function AddButton(const ACaption: string; AKind: TOBDButtonKind;
      AResult: TModalResult): TOBDButton;
    /// <summary>Adds a ghost button at the left of the footer.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AGlyph">Optional glyph.</param>
    /// <returns>The button.</returns>
    function AddLeftButton(const ACaption: string;
      AGlyph: TOBDGlyph = glNone): TOBDButton;
    /// <summary>Adds a check box at the left of the footer.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AChecked">Initial state.</param>
    /// <returns>The check box.</returns>
    function AddLeftCheck(const ACaption: string;
      AChecked: Boolean): TOBDCheckBox;
    /// <summary>Width a footer button needs, in device pixels.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AGlyph">Optional glyph.</param>
    /// <returns>Control width including the focus ring margin.</returns>
    function ButtonWidth(const ACaption: string;
      AGlyph: TOBDGlyph = glNone): Integer;
    /// <summary>Centres the window over the active form or the work area.
    /// </summary>
    procedure CenterOnActiveForm;
    /// <summary>Centres and shows the window modally.</summary>
    /// <returns>Modal result.</returns>
    function Run: TModalResult;
    /// <summary>IOBDSurface: the window background.</summary>
    /// <returns>Background colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Resolved palette.</summary>
    property Palette: TOBDThemePalette read FPalette;
    /// <summary>Theme handed to the child controls.</summary>
    property Theme: TOBDTheme read FTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TOBDDensity read FDensity;
    /// <summary>Pixels per inch the layout is computed for.</summary>
    property PPI: Integer read FPPI;
    /// <summary>Text in the title strip.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Footer strip; nil without a footer.</summary>
    property Footer: TOBDDialogFooter read FFooter;
    /// <summary>Control focused when the window first activates.
    /// </summary>
    property InitialFocus: TWinControl read FInitialFocus
      write FInitialFocus;
    /// <summary>Result of Escape and the close button.</summary>
    property CancelResult: TModalResult read FCancelResult
      write FCancelResult;
    /// <summary>Fires instead of closing when the close button or Escape
    /// is used on a modeless window.</summary>
    property OnCloseButton: TNotifyEvent read FOnCloseButton
      write FOnCloseButton;
    /// <summary>Lets the user resize the window by its border. The size
    /// it is shown at is the minimum; anchor the controls that follow.
    /// </summary>
    property Sizable: Boolean read FSizable write FSizable;
  end;

  /// <summary>Base of the non-visual OBD Studio dialog components.
  /// </summary>
  TOBDCommonDialog = class(TComponent, IOBDThemeAware)
  strict private
    FTheme: TOBDTheme;
    FDensity: TOBDDensity;
    FParentDensity: Boolean;
    FTitle: string;
    FResizable: Boolean;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    procedure SetTheme(AValue: TOBDTheme);
    function GetDensity: TOBDDensity;
    procedure SetDensity(AValue: TOBDDensity);
    function IsDensityStored: Boolean;
    function IsTitleStored: Boolean;
    function GetTitle: string;
  protected
    /// <summary>Clears theme references when they are removed.</summary>
    /// <param name="AComponent">Removed component.</param>
    /// <param name="Operation">Notification operation.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Title used while Title is empty.</summary>
    /// <returns>Default title.</returns>
    function DefaultTitle: string; virtual;
    /// <summary>Creates an empty window with the dialog theme, density
    /// and title.</summary>
    /// <returns>New window; the caller frees it.</returns>
    function NewWindow: TOBDDialogWindow;
    /// <summary>Shows a window modally between OnShow and OnClose.
    /// </summary>
    /// <param name="AWindow">Window to show.</param>
    /// <returns>Modal result.</returns>
    function RunWindow(AWindow: TOBDDialogWindow): TModalResult;
    /// <summary>Fires OnShow.</summary>
    procedure DoShow; virtual;
    /// <summary>Fires OnClose.</summary>
    procedure DoClose; virtual;
    /// <summary>Lets the user resize the window. Descendants with a
    /// layout that follows the size publish it.</summary>
    property Resizable: Boolean read FResizable write FResizable
      default False;
  public
    /// <summary>Creates the dialog.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases the theme reference.</summary>
    destructor Destroy; override;
    /// <summary>Receives theme changes.</summary>
    procedure ThemeChanged; virtual;
    /// <summary>Shows the dialog.</summary>
    /// <returns>True when the user accepted the dialog.</returns>
    function Execute: Boolean; virtual; abstract;
    /// <summary>Palette currently used by the dialog.</summary>
    /// <returns>Resolved palette.</returns>
    function Palette: TOBDThemePalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.
    /// </summary>
    property Theme: TOBDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing; follows the theme until set.
    /// </summary>
    property Density: TOBDDensity read GetDensity write SetDensity
      stored IsDensityStored;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity: Boolean read FParentDensity write FParentDensity
      default True;
    /// <summary>Text in the title strip; empty = the default title.
    /// </summary>
    property Title: string read GetTitle write FTitle stored IsTitleStored;
    /// <summary>Fires just before the window shows.</summary>
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    /// <summary>Fires after the window closed.</summary>
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

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

  /// <summary>Checks one value of <see cref="OBDInputQuery"/>.</summary>
  /// <param name="AIndex">Index of the prompt.</param>
  /// <param name="AValue">Current text.</param>
  /// <param name="AMessage">Message shown below the field when invalid.
  /// </param>
  /// <returns>True when the value is valid.</returns>
  TOBDInputValidator = reference to function(AIndex: Integer;
    const AValue: string; out AMessage: string): Boolean;

/// <summary>Palette a dialog uses for an optional theme.</summary>
/// <param name="ATheme">Theme; nil = default theme or VCL style.</param>
/// <returns>Resolved palette.</returns>
function OBDDialogPalette(ATheme: TOBDTheme): TOBDThemePalette;

/// <summary>Shows a themed message dialog.</summary>
/// <param name="ATitle">Header and bold body title.</param>
/// <param name="AText">Wrapped body text.</param>
/// <param name="AKind">Dialog kind.</param>
/// <param name="AButtons">Buttons to show.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>Selected modal result.</returns>
function OBDMessageDlg(const ATitle, AText: string; AKind: TOBDDialogKind;
  AButtons: TMsgDlgButtons; ATheme: TOBDTheme = nil): TModalResult;

/// <summary>Themed InputQuery with several prompts.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="AText">Optional text above the fields.</param>
/// <param name="APrompts">Field captions.</param>
/// <param name="AValues">Field values in and out.</param>
/// <param name="AValidator">Optional check; OK stays disabled while a
/// value is invalid.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>True when OK was chosen.</returns>
function OBDInputQuery(const ACaption, AText: string;
  const APrompts: array of string; var AValues: array of string;
  const AValidator: TOBDInputValidator = nil;
  ATheme: TOBDTheme = nil): Boolean; overload;

/// <summary>Themed InputQuery with one prompt.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="APrompt">Field caption.</param>
/// <param name="AValue">Field value in and out.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>True when OK was chosen.</returns>
function OBDInputQuery(const ACaption, APrompt: string;
  var AValue: string; ATheme: TOBDTheme = nil): Boolean; overload;

/// <summary>Themed InputBox.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="APrompt">Field caption.</param>
/// <param name="ADefault">Initial value, returned on Cancel.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>Entered value.</returns>
function OBDInputBox(const ACaption, APrompt, ADefault: string;
  ATheme: TOBDTheme = nil): string;

/// <summary>Themed ShowMessage.</summary>
/// <param name="AMessage">Message text.</param>
/// <param name="ATheme">Optional explicit theme.</param>
procedure OBDShowMessage(const AMessage: string; ATheme: TOBDTheme = nil);

implementation

const
  // Width of the resize border of a sizable dialog window, at 96 dpi.
  SIZE_GRIP = 6;

type
  // Gives the dialog window access to the protected Density of the OBD
  // controls it lays out.
  TOBDControlAccess = class(TOBDCustomControl);
  // Place gives explicit bounds, so AutoSize must not change them.
  TControlSizeAccess = class(TControl);

  TDialogContent = class(TOBDCustomControl)
  strict private
    FDialog: TOBDDialog;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreateContent(AOwner: TComponent; ADialog: TOBDDialog);
  end;

  TInputController = class(TComponent)
  strict private
    FEdits: array of TOBDEdit;
    FMessages: array of TOBDDialogText;
    FValidator: TOBDInputValidator;
    FOK: TOBDButton;
  public
    procedure Setup(ACount: Integer; AOK: TOBDButton;
      const AValidator: TOBDInputValidator);
    procedure SetField(AIndex: Integer; AEdit: TOBDEdit;
      AMessage: TOBDDialogText);
    procedure Validate(Sender: TObject);
    function Value(AIndex: Integer): string;
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

function OBDDialogPalette(ATheme: TOBDTheme): TOBDThemePalette;
begin
  if ATheme <> nil then
    Result := ATheme.Palette
  else if TOBDTheme.GetDefault <> nil then
    Result := TOBDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
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

{ TOBDDialogText }

constructor TOBDDialogText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := dtsBody;
  FAlignment := taLeftJustify;
  TabStop := False;
  Width := 200;
  Height := 18;
end;

procedure TOBDDialogText.SetStyle(AValue: TOBDDialogTextStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TOBDDialogText.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap = AValue then
    Exit;
  FWordWrap := AValue;
  Invalidate;
end;

procedure TOBDDialogText.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TOBDDialogText.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOBDDialogText.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
  P: TOBDThemePalette;
  Size: Single;
  Ink: TColor;
  Weight: TOBDTextWeight;
  X: Integer;
begin
  P := Palette;
  Painter := TOBDPainter.Create(ACanvas, P, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    if Caption = '' then
      Exit;
    Size := OBD_TEXT_BODY;
    Weight := twRegular;
    Ink := P.ForegroundText;
    case FStyle of
      dtsSubtle:
        Ink := P.GaugeLabel;
      dtsHeading:
        begin
          Size := OBD_TEXT_HEADING;
          Weight := twBold;
        end;
      dtsError:
        begin
          Size := OBD_TEXT_SMALL;
          Ink := Painter.DangerInk;
        end;
      dtsSmall:
        begin
          Size := OBD_TEXT_SMALL;
          Ink := P.GaugeLabel;
        end;
    end;
    if not Enabled then
      Ink := Painter.DisabledText;
    if FStyle = dtsCaps then
    begin
      Painter.Caps(0, Height div 2, Caption, taLeftJustify, clDefault, Width);
      Exit;
    end;
    if FWordWrap then
    begin
      Painter.WrapText(ClientRect, Caption, Size, Ink, Weight);
      Exit;
    end;
    case FAlignment of
      taRightJustify:
        X := Width;
      taCenter:
        X := Width div 2;
    else
      X := 0;
    end;
    Painter.Text(X, Height div 2, Caption, Size, Ink, Weight, FAlignment,
      Width);
  finally
    Painter.Free;
  end;
end;

{ TOBDDialogFooter }

constructor TOBDDialogFooter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  Width := 400;
  Height := 58;
end;

procedure TOBDDialogFooter.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  // The footer covers the bottom border: let the window resize there.
  if (Parent is TOBDDialogWindow) and (TOBDDialogWindow(Parent).SizingHit(
    Point(Message.XPos, Message.YPos)) <> HTNOWHERE) then
    Message.Result := HTTRANSPARENT;
end;

function TOBDDialogFooter.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDDialogFooter.PaintControl(ACanvas: TCanvas);
var
  Painter: TOBDPainter;
begin
  Painter := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, Palette.GaugeFace);
    Painter.HLine(0, 0, Width, Palette.NeutralLight);
  finally
    Painter.Free;
  end;
end;

{ TOBDDialogWindow }

constructor TOBDDialogWindow.CreateWindow(AOwner: TComponent;
  ATheme: TOBDTheme; ADensity: TOBDDensity; const ATitle: string);
begin
  inherited CreateNew(AOwner);
  FTheme := ATheme;
  FPalette := OBDDialogPalette(ATheme);
  FDensity := ADensity;
  FTitle := ATitle;
  FCancelResult := mrCancel;
{$IF CompilerVersion >= 35}
  FPPI := CurrentPPI;
{$ELSE}
  FPPI := Screen.PixelsPerInch;
{$ENDIF}
  if FPPI <= 0 then
    FPPI := 96;
  FHeaderHeight := Dp(System.Math.Max(24, DensityMetrics(FDensity).TitleBar - 6));
  BorderStyle := bsNone;
  BorderIcons := [];
  Position := poDesigned;
  KeyPreview := True;
  Color := FPalette.Background;
  Font.Name := 'Segoe UI';
  Caption := ATitle;
end;

function TOBDDialogWindow.Dp(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, FPPI, 96);
end;

function TOBDDialogWindow.SurfaceColor: TColor;
begin
  Result := FPalette.Background;
end;

procedure TOBDDialogWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TOBDDialogWindow.SetLayout(AWidth, ABodyHeight: Integer;
  AFooter: Boolean);
var
  FooterH: Integer;
begin
  FooterH := 0;
  if AFooter then
    FooterH := Dp(DensityMetrics(FDensity).Button + 28);
  ClientWidth := Dp(AWidth);
  ClientHeight := 1 + FHeaderHeight + 1 + Dp(ABodyHeight) + FooterH + 1;
  if AFooter then
  begin
    if FFooter = nil then
    begin
      FFooter := TOBDDialogFooter.Create(Self);
      Adopt(FFooter);
    end;
    FFooter.SetBounds(1, ClientHeight - 1 - FooterH, ClientWidth - 2,
      FooterH);
    FFooter.Anchors := [akLeft, akRight, akBottom];
    FButtonRight := FFooter.Width - Dp(16);
    FLeftRight := Dp(16);
  end;
end;

function TOBDDialogWindow.BodyTop: Integer;
begin
  Result := 1 + FHeaderHeight + 1;
end;

procedure TOBDDialogWindow.FitBody;
var
  I, Bottom, FooterH: Integer;
  C: TControl;
begin
  Bottom := BodyTop;
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    if (C <> FFooter) and C.Visible then
      Bottom := System.Math.Max(Bottom, C.Top + C.Height);
  end;
  FooterH := 0;
  if FFooter <> nil then
    FooterH := FFooter.Height;
  ClientHeight := Bottom + Dp(16) + FooterH + 1;
  if FFooter <> nil then
    FFooter.SetBounds(1, ClientHeight - 1 - FooterH, ClientWidth - 2,
      FooterH);
end;

function TOBDDialogWindow.SizingHit(const AScreen: TPoint): Integer;
var
  P: TPoint;
  G: Integer;
  L, R, T, B: Boolean;
begin
  Result := HTNOWHERE;
  if not FSizable or not HandleAllocated then
    Exit;
  P := ScreenToClient(AScreen);
  G := Dp(SIZE_GRIP);
  if (P.X < 0) or (P.Y < 0) or (P.X >= ClientWidth) or
    (P.Y >= ClientHeight) then
    Exit;
  L := P.X < G;
  R := P.X >= ClientWidth - G;
  T := P.Y < G;
  B := P.Y >= ClientHeight - G;
  if T and L then
    Result := HTTOPLEFT
  else if T and R then
    Result := HTTOPRIGHT
  else if B and L then
    Result := HTBOTTOMLEFT
  else if B and R then
    Result := HTBOTTOMRIGHT
  else if L then
    Result := HTLEFT
  else if R then
    Result := HTRIGHT
  else if T then
    Result := HTTOP
  else if B then
    Result := HTBOTTOM;
end;

procedure TOBDDialogWindow.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  // A borderless window has no sizing frame, so Windows does not size it:
  // track the border drag here.
  if FSizable and (Message.HitTest >= HTLEFT) and
    (Message.HitTest <= HTBOTTOMRIGHT) then
  begin
    FSizeHit := Message.HitTest;
    FSizeOrigin := Point(Message.XCursor, Message.YCursor);
    FSizeBounds := BoundsRect;
    SetCapture(Handle);
    Message.Result := 0;
    Exit;
  end;
  inherited;
end;

procedure TOBDDialogWindow.TrackSize;
var
  P: TPoint;
  R: TRect;
  DX, DY: Integer;
begin
  GetCursorPos(P);
  DX := P.X - FSizeOrigin.X;
  DY := P.Y - FSizeOrigin.Y;
  R := FSizeBounds;
  if FSizeHit in [HTLEFT, HTTOPLEFT, HTBOTTOMLEFT] then
    R.Left := System.Math.Min(R.Left + DX, R.Right - FMinSize.X);
  if FSizeHit in [HTRIGHT, HTTOPRIGHT, HTBOTTOMRIGHT] then
    R.Right := System.Math.Max(R.Right + DX, R.Left + FMinSize.X);
  if FSizeHit in [HTTOP, HTTOPLEFT, HTTOPRIGHT] then
    R.Top := System.Math.Min(R.Top + DY, R.Bottom - FMinSize.Y);
  if FSizeHit in [HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT] then
    R.Bottom := System.Math.Max(R.Bottom + DY, R.Top + FMinSize.Y);
  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
end;

procedure TOBDDialogWindow.DoShow;
begin
  FMinSize := Point(Width, Height);
  inherited;
end;

procedure TOBDDialogWindow.Resize;
begin
  inherited;
  Invalidate;
  if FFooter <> nil then
    FFooter.Invalidate;
end;

procedure TOBDDialogWindow.Adopt(AControl: TControl; AParent: TWinControl);
begin
  if AParent = nil then
    AParent := Self;
  AControl.Parent := AParent;
  if AControl is TOBDCustomControl then
  begin
    TOBDCustomControl(AControl).Theme := FTheme;
    TOBDControlAccess(AControl).Density := FDensity;
  end;
end;

procedure TOBDDialogWindow.Place(AControl: TControl; X, Y, W, H: Integer);
begin
  if AControl.Parent = nil then
    Adopt(AControl);
  TControlSizeAccess(AControl).AutoSize := False;
  AControl.SetBounds(Dp(X), BodyTop + Dp(Y), Dp(W), Dp(H));
end;

function TOBDDialogWindow.AddText(X, Y, W: Integer; const AText: string;
  AStyle: TOBDDialogTextStyle; AHeight: Integer): TOBDDialogText;
begin
  Result := TOBDDialogText.Create(Self);
  Adopt(Result);
  Result.Style := AStyle;
  Result.Caption := AText;
  if AHeight <= 0 then
  begin
    if AStyle = dtsCaps then
      AHeight := 14
    else
      AHeight := 20;
  end
  else
    Result.WordWrap := True;
  Place(Result, X, Y, W, AHeight);
end;

function TOBDDialogWindow.ButtonWidth(const ACaption: string;
  AGlyph: TOBDGlyph): Integer;
begin
  Result := OBDMeasureText(ACaption, OBD_TEXT_BODY, twSemibold, FPPI) +
    Dp(28);
  if AGlyph <> glNone then
    Inc(Result, Dp(18));
  Result := System.Math.Max(Result, Dp(84)) + 2 * Dp(4);
end;

function TOBDDialogWindow.AddButton(const ACaption: string;
  AKind: TOBDButtonKind; AResult: TModalResult): TOBDButton;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TOBDButton.Create(Self);
  Adopt(Result, FFooter);
  Result.Caption := ACaption;
  Result.Kind := AKind;
  Result.ModalResult := AResult;
  Result.AutoSize := False;
  Result.Anchors := [akTop, akRight];
  W := ButtonWidth(ACaption);
  H := Dp(DensityMetrics(FDensity).Button) + 2 * Dp(4);
  Result.SetBounds(FButtonRight - W + Dp(4), (FFooter.Height - H) div 2,
    W, H);
  FButtonRight := FButtonRight - W + 2 * Dp(4) - Dp(8);
end;

function TOBDDialogWindow.AddLeftButton(const ACaption: string;
  AGlyph: TOBDGlyph): TOBDButton;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TOBDButton.Create(Self);
  Adopt(Result, FFooter);
  Result.Caption := ACaption;
  Result.Kind := bkGhost;
  Result.Glyph := AGlyph;
  Result.AutoSize := False;
  W := ButtonWidth(ACaption, AGlyph);
  H := Dp(DensityMetrics(FDensity).Button) + 2 * Dp(4);
  Result.SetBounds(FLeftRight - Dp(10), (FFooter.Height - H) div 2, W, H);
  FLeftRight := FLeftRight + W;
end;

function TOBDDialogWindow.AddLeftCheck(const ACaption: string;
  AChecked: Boolean): TOBDCheckBox;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TOBDCheckBox.Create(Self);
  Adopt(Result, FFooter);
  Result.Caption := ACaption;
  Result.Checked := AChecked;
  Result.AutoSize := False;
  W := OBDMeasureText(ACaption, OBD_TEXT_BODY, twRegular, FPPI) +
    Dp(DensityMetrics(FDensity).Check + 24);
  H := Dp(DensityMetrics(FDensity).Button) + 2 * Dp(4);
  Result.SetBounds(FLeftRight - Dp(4), (FFooter.Height - H) div 2, W, H);
  FLeftRight := FLeftRight + W;
end;

procedure TOBDDialogWindow.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Caption := AValue;
  Invalidate;
end;

function TOBDDialogWindow.CloseRect: TRect;
begin
  Result := Rect(ClientWidth - 1 - FHeaderHeight - Dp(6), 1,
    ClientWidth - 1, FHeaderHeight + 1);
end;

procedure TOBDDialogWindow.SetCloseHot(AValue: Boolean);
var
  R: TRect;
begin
  if FCloseHot = AValue then
    Exit;
  FCloseHot := AValue;
  R := CloseRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TOBDDialogWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  Hit: Integer;
begin
  inherited;
  Hit := SizingHit(Point(Message.XPos, Message.YPos));
  if Hit <> HTNOWHERE then
  begin
    Message.Result := Hit;
    Exit;
  end;
  P := ScreenToClient(Point(Message.XPos, Message.YPos));
  if PtInRect(CloseRect, P) then
    Message.Result := HTCLIENT
  else if (P.Y >= 0) and (P.Y < FHeaderHeight + 1) then
    Message.Result := HTCAPTION;
end;

procedure TOBDDialogWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetCloseHot(False);
end;

procedure TOBDDialogWindow.Paint;
var
  Painter: TOBDPainter;
  R: TRect;
  CX, CY, D: Integer;
  Ink: TColor;
begin
  inherited;
  Painter := TOBDPainter.Create(Canvas, FPalette, FPPI);
  try
    R := ClientRect;
    Painter.FillRect(R, FPalette.Background);
    Painter.FrameRect(R, FPalette.Accent);
    Painter.FillRect(Rect(1, 1, ClientWidth - 1, FHeaderHeight + 1),
      FPalette.GaugeFace);
    Painter.HLine(1, FHeaderHeight + 1, ClientWidth - 2,
      FPalette.NeutralLight);
    Painter.Text(Dp(14), 1 + FHeaderHeight div 2, FTitle, OBD_TEXT_BODY,
      FPalette.ForegroundText, twSemibold, taLeftJustify,
      ClientWidth - FHeaderHeight - Dp(32));
    R := CloseRect;
    Ink := FPalette.Subtle;
    if FCloseHot then
    begin
      Painter.FillRect(R, FPalette.Danger);
      Ink := Painter.ContrastInk(FPalette.Danger);
    end;
    CX := (R.Left + R.Right) div 2;
    CY := (R.Top + R.Bottom) div 2;
    D := Dp(4);
    Painter.Lines([OBDPointF(CX - D, CY - D), OBDPointF(CX + D, CY + D)],
      Ink, Painter.SF(1.4));
    Painter.Lines([OBDPointF(CX + D, CY - D), OBDPointF(CX - D, CY + D)],
      Ink, Painter.SF(1.4));
  finally
    Painter.Free;
  end;
end;

procedure TOBDDialogWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FSizeHit <> 0 then
  begin
    if ssLeft in Shift then
    begin
      TrackSize;
      Exit;
    end;
    FSizeHit := 0;
  end;
  SetCloseHot(PtInRect(CloseRect, Point(X, Y)));
end;

procedure TOBDDialogWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FSizeHit <> 0 then
  begin
    FSizeHit := 0;
    if GetCapture = Handle then
      ReleaseCapture;
    Exit;
  end;
  if (Button <> mbLeft) or not PtInRect(CloseRect, Point(X, Y)) then
    Exit;
  if fsModal in FormState then
    ModalResult := FCancelResult
  else if Assigned(FOnCloseButton) then
    FOnCloseButton(Self)
  else
    Close;
end;

procedure TOBDDialogWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key <> VK_ESCAPE) or (Shift <> []) then
    Exit;
  // Escape first closes an open drop-down list.
  if (ActiveControl is TOBDComboBox) and
    TOBDComboBox(ActiveControl).DroppedDown then
    Exit;
  Key := 0;
  if fsModal in FormState then
    ModalResult := FCancelResult
  else if Assigned(FOnCloseButton) then
    FOnCloseButton(Self)
  else
    Close;
end;

procedure TOBDDialogWindow.Activate;
var
  Target: TWinControl;
begin
  inherited;
  Target := FInitialFocus;
  FInitialFocus := nil;
  if (Target <> nil) and Target.CanFocus then
    Target.SetFocus;
end;

procedure TOBDDialogWindow.CenterOnActiveForm;
var
  OwnerForm: TCustomForm;
  Area: TRect;
  Mon: TMonitor;
begin
  OwnerForm := Screen.ActiveCustomForm;
  if (OwnerForm <> nil) and (OwnerForm <> Self) and OwnerForm.Visible then
    Area := OwnerForm.BoundsRect
  else
  begin
    Mon := Screen.MonitorFromWindow(Application.Handle, mdNearest);
    if Mon <> nil then
      Area := Mon.WorkareaRect
    else
      Area := Screen.DesktopRect;
  end;
  Left := Area.Left + (Area.Width - Width) div 2;
  Top := Area.Top + (Area.Height - Height) div 2;
end;

function TOBDDialogWindow.Run: TModalResult;
begin
  CenterOnActiveForm;
  Result := ShowModal;
end;

{ TOBDCommonDialog }

constructor TOBDCommonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FParentDensity := True;
end;

destructor TOBDCommonDialog.Destroy;
begin
  SetTheme(nil);
  inherited;
end;

procedure TOBDCommonDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TOBDCommonDialog.ThemeChanged;
begin
end;

procedure TOBDCommonDialog.SetTheme(AValue: TOBDTheme);
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

function TOBDCommonDialog.GetDensity: TOBDDensity;
var
  T: TOBDTheme;
begin
  T := FTheme;
  if T = nil then
    T := TOBDTheme.GetDefault;
  if FParentDensity and (T <> nil) then
    Result := T.Density
  else
    Result := FDensity;
end;

procedure TOBDCommonDialog.SetDensity(AValue: TOBDDensity);
begin
  FDensity := AValue;
  FParentDensity := False;
end;

function TOBDCommonDialog.IsDensityStored: Boolean;
begin
  Result := not FParentDensity;
end;

function TOBDCommonDialog.GetTitle: string;
begin
  Result := FTitle;
end;

function TOBDCommonDialog.IsTitleStored: Boolean;
begin
  Result := FTitle <> '';
end;

function TOBDCommonDialog.DefaultTitle: string;
begin
  Result := 'OBD Studio';
end;

function TOBDCommonDialog.Palette: TOBDThemePalette;
begin
  Result := OBDDialogPalette(FTheme);
end;

function TOBDCommonDialog.NewWindow: TOBDDialogWindow;
var
  S: string;
begin
  S := FTitle;
  if S = '' then
    S := DefaultTitle;
  Result := TOBDDialogWindow.CreateWindow(Application, FTheme, GetDensity, S);
  Result.Sizable := FResizable;
end;

function TOBDCommonDialog.RunWindow(AWindow: TOBDDialogWindow): TModalResult;
begin
  DoShow;
  try
    Result := AWindow.Run;
  finally
    DoClose;
  end;
end;

procedure TOBDCommonDialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TOBDCommonDialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
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

    Painter.Text(ScaleValue(56), ScaleValue(22), FDialog.Title, OBD_TEXT_TITLE,
      P.ForegroundText, twBold, taLeftJustify, Width - ScaleValue(72));
    TextRect := Rect(ScaleValue(56), ScaleValue(40), Width - ScaleValue(20),
      Height - ScaleValue(8));
    Painter.WrapText(TextRect, FDialog.Text, 12.5, P.ForegroundText,
      twRegular);
  finally
    Painter.Free;
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
  Result := OBDDialogPalette(FTheme);
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
  Form: TOBDDialogWindow;
  Content: TDialogContent;
  Check: TOBDCheckBox;
  Btn: TOBDButton;
  BodyH, I: Integer;
  Button: TMsgDlgBtn;
  Kind: TOBDButtonKind;
  Focus: TOBDButton;
begin
  BodyH := 128;
  if Pos(sLineBreak, FText) > 0 then
    Inc(BodyH, 24);
  Form := TOBDDialogWindow.CreateWindow(Application, FTheme, FDensity,
    FTitle);
  try
    Form.SetLayout(470, BodyH);
    Content := TDialogContent.CreateContent(Form, Self);
    Content.Parent := Form;
    Content.SetBounds(1, Form.BodyTop, Form.ClientWidth - 2, Form.Dp(BodyH));

    Check := nil;
    if FCheckBoxCaption <> '' then
      Check := Form.AddLeftCheck(FCheckBoxCaption, FCheckBoxChecked);

    Focus := nil;
    for I := Low(ORDER) to High(ORDER) do
    begin
      Button := ORDER[I];
      if not (Button in FButtons) then
        Continue;
      if Button = FDangerButton then
        Kind := bkDanger
      else if Button = FDefaultButton then
        Kind := bkPrimary
      else if Button in [mbCancel, mbNo, mbClose] then
        Kind := bkSecondary
      else
        Kind := bkGhost;
      Btn := Form.AddButton(CaptionForButton(Button), Kind,
        ModalResultOf(Button));
      Btn.Default := Button = FDefaultButton;
      Btn.Cancel := Button in [mbCancel, mbNo, mbClose];
      if Btn.Cancel then
        Form.CancelResult := Btn.ModalResult;
      if Btn.Default then
        Focus := Btn;
    end;
    if Focus <> nil then
      Form.ActiveControl := Focus;

    Result := Form.Run;
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
    if ATheme <> nil then
      Dialog.Density := ATheme.Density
    else if TOBDTheme.GetDefault <> nil then
      Dialog.Density := TOBDTheme.GetDefault.Density;
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

{ TInputController }

procedure TInputController.Setup(ACount: Integer; AOK: TOBDButton;
  const AValidator: TOBDInputValidator);
begin
  SetLength(FEdits, ACount);
  SetLength(FMessages, ACount);
  FOK := AOK;
  FValidator := AValidator;
end;

procedure TInputController.SetField(AIndex: Integer; AEdit: TOBDEdit;
  AMessage: TOBDDialogText);
begin
  FEdits[AIndex] := AEdit;
  FMessages[AIndex] := AMessage;
  AEdit.OnChange := Validate;
end;

procedure TInputController.Validate(Sender: TObject);
var
  I: Integer;
  AllValid, Valid: Boolean;
  Msg: string;
begin
  AllValid := True;
  for I := 0 to High(FEdits) do
  begin
    Msg := '';
    Valid := True;
    if Assigned(FValidator) then
      Valid := FValidator(I, FEdits[I].Text, Msg);
    if Valid then
      Msg := '';
    FMessages[I].Caption := Msg;
    AllValid := AllValid and Valid;
  end;
  if FOK <> nil then
    FOK.Enabled := AllValid;
end;

function TInputController.Value(AIndex: Integer): string;
begin
  Result := FEdits[AIndex].Text;
end;

function OBDInputQuery(const ACaption, AText: string;
  const APrompts: array of string; var AValues: array of string;
  const AValidator: TOBDInputValidator; ATheme: TOBDTheme): Boolean;
var
  Form: TOBDDialogWindow;
  Controller: TInputController;
  Edit: TOBDEdit;
  Msg: TOBDDialogText;
  OK, CancelBtn: TOBDButton;
  Density: TOBDDensity;
  I, Count, EditH, Y, BodyH: Integer;
  First: TOBDEdit;
begin
  Count := System.Math.Min(Length(APrompts), Length(AValues));
  Density := dnDesktop;
  if ATheme <> nil then
    Density := ATheme.Density
  else if TOBDTheme.GetDefault <> nil then
    Density := TOBDTheme.GetDefault.Density;
  EditH := DensityMetrics(Density).Edit;
  BodyH := 16 + Count * (18 + EditH + 20) + 4;
  if AText <> '' then
    Inc(BodyH, 32);
  Form := TOBDDialogWindow.CreateWindow(Application, ATheme, Density,
    ACaption);
  try
    Form.SetLayout(436, BodyH);
    Y := 16;
    if AText <> '' then
    begin
      Form.AddText(16, Y, 404, AText, dtsBody);
      Inc(Y, 32);
    end;
    OK := Form.AddButton('OK', bkPrimary, mrOk);
    OK.Default := True;
    CancelBtn := Form.AddButton('Cancel', bkSecondary, mrCancel);
    CancelBtn.Cancel := True;
    Controller := TInputController.Create(Form);
    Controller.Setup(Count, OK, AValidator);
    First := nil;
    for I := 0 to Count - 1 do
    begin
      Form.AddText(16, Y, 404, APrompts[I], dtsCaps);
      Edit := TOBDEdit.Create(Form);
      Form.Place(Edit, 16, Y + 18, 404, EditH);
      Edit.Text := AValues[I];
      Msg := Form.AddText(16, Y + 18 + EditH + 2, 404, '', dtsError, 0);
      Controller.SetField(I, Edit, Msg);
      if First = nil then
        First := Edit;
      Inc(Y, 18 + EditH + 20);
    end;
    Controller.Validate(nil);
    Form.InitialFocus := First;
    Result := Form.Run = mrOk;
    if Result then
      for I := 0 to Count - 1 do
        AValues[I] := Controller.Value(I);
  finally
    Form.Free;
  end;
end;

function OBDInputQuery(const ACaption, APrompt: string;
  var AValue: string; ATheme: TOBDTheme): Boolean;
var
  Values: array of string;
begin
  SetLength(Values, 1);
  Values[0] := AValue;
  Result := OBDInputQuery(ACaption, '', [APrompt], Values, nil, ATheme);
  if Result then
    AValue := Values[0];
end;

function OBDInputBox(const ACaption, APrompt, ADefault: string;
  ATheme: TOBDTheme): string;
begin
  Result := ADefault;
  if not OBDInputQuery(ACaption, APrompt, Result, ATheme) then
    Result := ADefault;
end;

procedure OBDShowMessage(const AMessage: string; ATheme: TOBDTheme);
var
  Title: string;
begin
  Title := Application.Title;
  if Title = '' then
    Title := 'OBD Studio';
  OBDMessageDlg(Title, AMessage, dkInfo, [mbOK], ATheme);
end;

end.
