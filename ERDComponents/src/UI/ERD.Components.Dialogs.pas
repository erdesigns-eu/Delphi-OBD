//------------------------------------------------------------------------------
//  ERD.Components.Dialogs
//
//  Themed modal dialog support for Application.
//
//    TERDDialogWindow   borderless themed form with a title strip, a close
//                       button and a footer; the window every Application
//                       dialog is built on.
//    TERDDialogText     caption, body, error and upper-case label text
//                       for dialog layouts.
//    TERDDialogFooter   footer strip; its buttons sit on the footer fill.
//    TERDCommonDialog   base of the non-visual dialog components (Theme,
//                       Density, Title, Execute, OnShow, OnClose).
//    TERDDialog         non-visual MessageDlg replacement.
//    ERDMessageDlg      convenience routine for one-off themed dialogs.
//    ERDInputQuery,     InputQuery, InputBox and ShowMessage
//    ERDInputBox,       replacements with optional validation.
//    ERDShowMessage
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

unit ERD.Components.Dialogs;

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
  Vcl.Dialogs,
  Vcl.Themes,
  ERD.Components.Theme,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.Buttons,
  ERD.Components.Edits;

type
  /// <summary>Dialog icon and semantic colour.</summary>
  TERDDialogKind = (
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

  /// <summary>Look of a <see cref="TERDDialogText"/>.</summary>
  TERDDialogTextStyle = (
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
  TERDDialogText = class(TERDCustomControl)
  strict private
    FStyle: TERDDialogTextStyle;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    procedure SetStyle(AValue: TERDDialogTextStyle);
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
    property Style: TERDDialogTextStyle read FStyle write SetStyle
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
  TERDDialogFooter = class(TERDCustomControl, IERDSurface)
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
    /// <summary>IERDSurface: the footer fill.</summary>
    /// <returns>Footer colour.</returns>
    function SurfaceColor: TColor;
  end;

  /// <summary>Borderless themed form with a title strip, close button and
  /// optional footer. All Application dialogs are built on it.</summary>
  TERDDialogWindow = class(TForm, IERDSurface)
  strict private
    FTheme: TERDTheme;
    FPalette: TERDThemePalette;
    FDensity: TERDDensity;
    FTitle: string;
    FPPI: Integer;
    FHeaderHeight: Integer;
    FFooter: TERDDialogFooter;
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
    constructor CreateWindow(AOwner: TComponent; ATheme: TERDTheme;
      ADensity: TERDDensity; const ATitle: string);
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
    /// an ERD control the dialog theme and density.</summary>
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
      AStyle: TERDDialogTextStyle = dtsCaps;
      AHeight: Integer = 0): TERDDialogText;
    /// <summary>Adds a footer button right of the previous one, from the
    /// right edge inwards.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AKind">Visual weight.</param>
    /// <param name="AResult">Modal result; mrNone to handle OnClick.</param>
    /// <returns>The button.</returns>
    function AddButton(const ACaption: string; AKind: TERDButtonKind;
      AResult: TModalResult): TERDButton;
    /// <summary>Adds a ghost button at the left of the footer.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AGlyph">Optional glyph.</param>
    /// <returns>The button.</returns>
    function AddLeftButton(const ACaption: string;
      AGlyph: TERDGlyph = glNone): TERDButton;
    /// <summary>Adds a check box at the left of the footer.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AChecked">Initial state.</param>
    /// <returns>The check box.</returns>
    function AddLeftCheck(const ACaption: string;
      AChecked: Boolean): TERDCheckBox;
    /// <summary>Width a footer button needs, in device pixels.</summary>
    /// <param name="ACaption">Caption.</param>
    /// <param name="AGlyph">Optional glyph.</param>
    /// <returns>Control width including the focus ring margin.</returns>
    function ButtonWidth(const ACaption: string;
      AGlyph: TERDGlyph = glNone): Integer;
    /// <summary>Centres the window over the active form or the work area.
    /// </summary>
    procedure CenterOnActiveForm;
    /// <summary>Centres and shows the window modally.</summary>
    /// <returns>Modal result.</returns>
    function Run: TModalResult;
    /// <summary>IERDSurface: the window background.</summary>
    /// <returns>Background colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Resolved palette.</summary>
    property Palette: TERDThemePalette read FPalette;
    /// <summary>Theme handed to the child controls.</summary>
    property Theme: TERDTheme read FTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TERDDensity read FDensity;
    /// <summary>Pixels per inch the layout is computed for.</summary>
    property PPI: Integer read FPPI;
    /// <summary>Text in the title strip.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Footer strip; nil without a footer.</summary>
    property Footer: TERDDialogFooter read FFooter;
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

  /// <summary>Base of the non-visual Application dialog components.
  /// </summary>
  TERDCommonDialog = class(TComponent, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FParentDensity: Boolean;
    FTitle: string;
    FResizable: Boolean;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    procedure SetTheme(AValue: TERDTheme);
    function GetDensity: TERDDensity;
    procedure SetDensity(AValue: TERDDensity);
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
    function NewWindow: TERDDialogWindow;
    /// <summary>Shows a window modally between OnShow and OnClose.
    /// </summary>
    /// <param name="AWindow">Window to show.</param>
    /// <returns>Modal result.</returns>
    function RunWindow(AWindow: TERDDialogWindow): TModalResult;
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
    function Palette: TERDThemePalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.
    /// </summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing; follows the theme until set.
    /// </summary>
    property Density: TERDDensity read GetDensity write SetDensity
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
  TERDDialog = class(TComponent, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FTitle: string;
    FText: string;
    FKind: TERDDialogKind;
    FButtons: TMsgDlgButtons;
    FDefaultButton: TMsgDlgBtn;
    FButtonCaptions: TStrings;
    FDangerButton: TMsgDlgBtn;
    FCheckBoxCaption: string;
    FCheckBoxChecked: Boolean;
    procedure SetTheme(AValue: TERDTheme);
    procedure SetDensity(AValue: TERDDensity);
    procedure SetTitle(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetKind(AValue: TERDDialogKind);
    procedure SetButtons(AValue: TMsgDlgButtons);
    procedure SetDefaultButton(AValue: TMsgDlgBtn);
    procedure SetButtonCaptions(AValue: TStrings);
    procedure SetDangerButton(AValue: TMsgDlgBtn);
    procedure SetCheckBoxCaption(const AValue: string);
    procedure SetCheckBoxChecked(AValue: Boolean);
    function GetPalette: TERDThemePalette;
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
    property Palette: TERDThemePalette read GetPalette;
  published
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TERDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Header title and bold body title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Wrapped body text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Icon and semantic colour.</summary>
    property Kind: TERDDialogKind read FKind write SetKind default dkWarning;
    /// <summary>Buttons shown in the footer.</summary>
    property Buttons: TMsgDlgButtons read FButtons write SetButtons;
    /// <summary>Button clicked by Enter.</summary>
    property DefaultButton: TMsgDlgBtn read FDefaultButton
      write SetDefaultButton default mbOK;
    /// <summary>Optional button caption overrides, for example mbYes=Clear list.</summary>
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

  /// <summary>Checks one value of <see cref="ERDInputQuery"/>.</summary>
  /// <param name="AIndex">Index of the prompt.</param>
  /// <param name="AValue">Current text.</param>
  /// <param name="AMessage">Message shown below the field when invalid.
  /// </param>
  /// <returns>True when the value is valid.</returns>
  TERDInputValidator = reference to function(AIndex: Integer;
    const AValue: string; out AMessage: string): Boolean;

/// <summary>Palette a dialog uses for an optional theme.</summary>
/// <param name="ATheme">Theme; nil = default theme or VCL style.</param>
/// <returns>Resolved palette.</returns>
function ERDDialogPalette(ATheme: TERDTheme): TERDThemePalette;

/// <summary>Shows a themed message dialog.</summary>
/// <param name="ATitle">Header and bold body title.</param>
/// <param name="AText">Wrapped body text.</param>
/// <param name="AKind">Dialog kind.</param>
/// <param name="AButtons">Buttons to show.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>Selected modal result.</returns>
function ERDMessageDlg(const ATitle, AText: string; AKind: TERDDialogKind;
  AButtons: TMsgDlgButtons; ATheme: TERDTheme = nil): TModalResult;

/// <summary>Themed InputQuery with several prompts.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="AText">Optional text above the fields.</param>
/// <param name="APrompts">Field captions.</param>
/// <param name="AValues">Field values in and out.</param>
/// <param name="AValidator">Optional check; OK stays disabled while a
/// value is invalid.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>True when OK was chosen.</returns>
function ERDInputQuery(const ACaption, AText: string;
  const APrompts: array of string; var AValues: array of string;
  const AValidator: TERDInputValidator = nil;
  ATheme: TERDTheme = nil): Boolean; overload;

/// <summary>Themed InputQuery with one prompt.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="APrompt">Field caption.</param>
/// <param name="AValue">Field value in and out.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>True when OK was chosen.</returns>
function ERDInputQuery(const ACaption, APrompt: string;
  var AValue: string; ATheme: TERDTheme = nil): Boolean; overload;

/// <summary>Themed InputBox.</summary>
/// <param name="ACaption">Title of the window.</param>
/// <param name="APrompt">Field caption.</param>
/// <param name="ADefault">Initial value, returned on Cancel.</param>
/// <param name="ATheme">Optional explicit theme.</param>
/// <returns>Entered value.</returns>
function ERDInputBox(const ACaption, APrompt, ADefault: string;
  ATheme: TERDTheme = nil): string;

/// <summary>Themed ShowMessage.</summary>
/// <param name="AMessage">Message text.</param>
/// <param name="ATheme">Optional explicit theme.</param>
procedure ERDShowMessage(const AMessage: string; ATheme: TERDTheme = nil);

implementation

const
  // Width of the resize border of a sizable dialog window, at 96 dpi.
  SIZE_GRIP = 6;

type
  // Gives the dialog window access to the protected Density of the ERD
  // controls it lays out.
  TERDControlAccess = class(TERDCustomControl);
  // Place gives explicit bounds, so AutoSize must not change them.
  TControlSizeAccess = class(TControl);

  TDialogContent = class(TERDCustomControl)
  strict private
    FDialog: TERDDialog;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreateContent(AOwner: TComponent; ADialog: TERDDialog);
  end;

  TInputController = class(TComponent)
  strict private
    FEdits: array of TERDEdit;
    FMessages: array of TERDDialogText;
    FValidator: TERDInputValidator;
    FOK: TERDButton;
  public
    procedure Setup(ACount: Integer; AOK: TERDButton;
      const AValidator: TERDInputValidator);
    procedure SetField(AIndex: Integer; AEdit: TERDEdit;
      AMessage: TERDDialogText);
    procedure Validate(Sender: TObject);
    function Value(AIndex: Integer): string;
  end;

function FallbackPalette: TERDThemePalette;
begin
  Result := BRAND_PALETTE_LIGHT;
end;

function ERDDialogPalette(ATheme: TERDTheme): TERDThemePalette;
begin
  if ATheme <> nil then
    Result := ATheme.Palette
  else if TERDTheme.GetDefault <> nil then
    Result := TERDTheme.GetDefault.Palette
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

function KindColor(AKind: TERDDialogKind; const P: TERDThemePalette): TColor;
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

{ TERDDialogText }

constructor TERDDialogText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := dtsBody;
  FAlignment := taLeftJustify;
  TabStop := False;
  Width := 200;
  Height := 18;
end;

procedure TERDDialogText.SetStyle(AValue: TERDDialogTextStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TERDDialogText.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap = AValue then
    Exit;
  FWordWrap := AValue;
  Invalidate;
end;

procedure TERDDialogText.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TERDDialogText.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TERDDialogText.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
  P: TERDThemePalette;
  Size: Single;
  Ink: TColor;
  Weight: TERDTextWeight;
  X: Integer;
begin
  P := Palette;
  Painter := TERDPainter.Create(ACanvas, P, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, EffectiveBackground);
    if Caption = '' then
      Exit;
    Size := ERD_TEXT_BODY;
    Weight := twRegular;
    Ink := P.ForegroundText;
    case FStyle of
      dtsSubtle:
        Ink := P.GaugeLabel;
      dtsHeading:
        begin
          Size := ERD_TEXT_HEADING;
          Weight := twBold;
        end;
      dtsError:
        begin
          Size := ERD_TEXT_SMALL;
          Ink := Painter.DangerInk;
        end;
      dtsSmall:
        begin
          Size := ERD_TEXT_SMALL;
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

{ TERDDialogFooter }

constructor TERDDialogFooter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  Width := 400;
  Height := 58;
end;

procedure TERDDialogFooter.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  // The footer covers the bottom border: let the window resize there.
  if (Parent is TERDDialogWindow) and (TERDDialogWindow(Parent).SizingHit(
    Point(Message.XPos, Message.YPos)) <> HTNOWHERE) then
    Message.Result := HTTRANSPARENT;
end;

function TERDDialogFooter.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TERDDialogFooter.PaintControl(ACanvas: TCanvas);
var
  Painter: TERDPainter;
begin
  Painter := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Painter.FillRect(ClientRect, Palette.GaugeFace);
    Painter.HLine(0, 0, Width, Palette.NeutralLight);
  finally
    Painter.Free;
  end;
end;

{ TERDDialogWindow }

constructor TERDDialogWindow.CreateWindow(AOwner: TComponent;
  ATheme: TERDTheme; ADensity: TERDDensity; const ATitle: string);
begin
  inherited CreateNew(AOwner);
  FTheme := ATheme;
  FPalette := ERDDialogPalette(ATheme);
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

function TERDDialogWindow.Dp(AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, FPPI, 96);
end;

function TERDDialogWindow.SurfaceColor: TColor;
begin
  Result := FPalette.Background;
end;

procedure TERDDialogWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TERDDialogWindow.SetLayout(AWidth, ABodyHeight: Integer;
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
      FFooter := TERDDialogFooter.Create(Self);
      Adopt(FFooter);
    end;
    FFooter.SetBounds(1, ClientHeight - 1 - FooterH, ClientWidth - 2,
      FooterH);
    FFooter.Anchors := [akLeft, akRight, akBottom];
    FButtonRight := FFooter.Width - Dp(16);
    FLeftRight := Dp(16);
  end;
end;

function TERDDialogWindow.BodyTop: Integer;
begin
  Result := 1 + FHeaderHeight + 1;
end;

procedure TERDDialogWindow.FitBody;
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

function TERDDialogWindow.SizingHit(const AScreen: TPoint): Integer;
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

procedure TERDDialogWindow.WMNCLButtonDown(var Message: TWMNCLButtonDown);
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

procedure TERDDialogWindow.TrackSize;
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

procedure TERDDialogWindow.DoShow;
begin
  FMinSize := Point(Width, Height);
  inherited;
end;

procedure TERDDialogWindow.Resize;
begin
  inherited;
  Invalidate;
  if FFooter <> nil then
    FFooter.Invalidate;
end;

procedure TERDDialogWindow.Adopt(AControl: TControl; AParent: TWinControl);
begin
  if AParent = nil then
    AParent := Self;
  AControl.Parent := AParent;
  if AControl is TERDCustomControl then
  begin
    TERDCustomControl(AControl).Theme := FTheme;
    TERDControlAccess(AControl).Density := FDensity;
  end;
end;

procedure TERDDialogWindow.Place(AControl: TControl; X, Y, W, H: Integer);
begin
  if AControl.Parent = nil then
    Adopt(AControl);
  TControlSizeAccess(AControl).AutoSize := False;
  AControl.SetBounds(Dp(X), BodyTop + Dp(Y), Dp(W), Dp(H));
end;

function TERDDialogWindow.AddText(X, Y, W: Integer; const AText: string;
  AStyle: TERDDialogTextStyle; AHeight: Integer): TERDDialogText;
begin
  Result := TERDDialogText.Create(Self);
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

function TERDDialogWindow.ButtonWidth(const ACaption: string;
  AGlyph: TERDGlyph): Integer;
begin
  Result := ERDMeasureText(ACaption, ERD_TEXT_BODY, twSemibold, FPPI) +
    Dp(28);
  if AGlyph <> glNone then
    Inc(Result, Dp(18));
  Result := System.Math.Max(Result, Dp(84)) + 2 * Dp(4);
end;

function TERDDialogWindow.AddButton(const ACaption: string;
  AKind: TERDButtonKind; AResult: TModalResult): TERDButton;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TERDButton.Create(Self);
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

function TERDDialogWindow.AddLeftButton(const ACaption: string;
  AGlyph: TERDGlyph): TERDButton;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TERDButton.Create(Self);
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

function TERDDialogWindow.AddLeftCheck(const ACaption: string;
  AChecked: Boolean): TERDCheckBox;
var
  W, H: Integer;
begin
  if FFooter = nil then
    SetLayout(MulDiv(ClientWidth, 96, FPPI), 0, True);
  Result := TERDCheckBox.Create(Self);
  Adopt(Result, FFooter);
  Result.Caption := ACaption;
  Result.Checked := AChecked;
  Result.AutoSize := False;
  W := ERDMeasureText(ACaption, ERD_TEXT_BODY, twRegular, FPPI) +
    Dp(DensityMetrics(FDensity).Check + 24);
  H := Dp(DensityMetrics(FDensity).Button) + 2 * Dp(4);
  Result.SetBounds(FLeftRight - Dp(4), (FFooter.Height - H) div 2, W, H);
  FLeftRight := FLeftRight + W;
end;

procedure TERDDialogWindow.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Caption := AValue;
  Invalidate;
end;

function TERDDialogWindow.CloseRect: TRect;
begin
  Result := Rect(ClientWidth - 1 - FHeaderHeight - Dp(6), 1,
    ClientWidth - 1, FHeaderHeight + 1);
end;

procedure TERDDialogWindow.SetCloseHot(AValue: Boolean);
var
  R: TRect;
begin
  if FCloseHot = AValue then
    Exit;
  FCloseHot := AValue;
  R := CloseRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TERDDialogWindow.WMNCHitTest(var Message: TWMNCHitTest);
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

procedure TERDDialogWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetCloseHot(False);
end;

procedure TERDDialogWindow.Paint;
var
  Painter: TERDPainter;
  R: TRect;
  CX, CY, D: Integer;
  Ink: TColor;
begin
  inherited;
  Painter := TERDPainter.Create(Canvas, FPalette, FPPI);
  try
    R := ClientRect;
    Painter.FillRect(R, FPalette.Background);
    Painter.FrameRect(R, FPalette.Accent);
    Painter.FillRect(Rect(1, 1, ClientWidth - 1, FHeaderHeight + 1),
      FPalette.GaugeFace);
    Painter.HLine(1, FHeaderHeight + 1, ClientWidth - 2,
      FPalette.NeutralLight);
    Painter.Text(Dp(14), 1 + FHeaderHeight div 2, FTitle, ERD_TEXT_BODY,
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
    Painter.Lines([ERDPointF(CX - D, CY - D), ERDPointF(CX + D, CY + D)],
      Ink, Painter.SF(1.4));
    Painter.Lines([ERDPointF(CX + D, CY - D), ERDPointF(CX - D, CY + D)],
      Ink, Painter.SF(1.4));
  finally
    Painter.Free;
  end;
end;

procedure TERDDialogWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TERDDialogWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

procedure TERDDialogWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key <> VK_ESCAPE) or (Shift <> []) then
    Exit;
  // Escape first closes an open drop-down list.
  if (ActiveControl is TERDComboBox) and
    TERDComboBox(ActiveControl).DroppedDown then
    Exit;
  Key := 0;
  if fsModal in FormState then
    ModalResult := FCancelResult
  else if Assigned(FOnCloseButton) then
    FOnCloseButton(Self)
  else
    Close;
end;

procedure TERDDialogWindow.Activate;
var
  Target: TWinControl;
begin
  inherited;
  Target := FInitialFocus;
  FInitialFocus := nil;
  if (Target <> nil) and Target.CanFocus then
    Target.SetFocus;
end;

procedure TERDDialogWindow.CenterOnActiveForm;
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

function TERDDialogWindow.Run: TModalResult;
begin
  CenterOnActiveForm;
  Result := ShowModal;
end;

{ TERDCommonDialog }

constructor TERDCommonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FParentDensity := True;
end;

destructor TERDCommonDialog.Destroy;
begin
  SetTheme(nil);
  inherited;
end;

procedure TERDCommonDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDCommonDialog.ThemeChanged;
begin
end;

procedure TERDCommonDialog.SetTheme(AValue: TERDTheme);
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

function TERDCommonDialog.GetDensity: TERDDensity;
var
  T: TERDTheme;
begin
  T := FTheme;
  if T = nil then
    T := TERDTheme.GetDefault;
  if FParentDensity and (T <> nil) then
    Result := T.Density
  else
    Result := FDensity;
end;

procedure TERDCommonDialog.SetDensity(AValue: TERDDensity);
begin
  FDensity := AValue;
  FParentDensity := False;
end;

function TERDCommonDialog.IsDensityStored: Boolean;
begin
  Result := not FParentDensity;
end;

function TERDCommonDialog.GetTitle: string;
begin
  Result := FTitle;
end;

function TERDCommonDialog.IsTitleStored: Boolean;
begin
  Result := FTitle <> '';
end;

function TERDCommonDialog.DefaultTitle: string;
begin
  Result := 'Application';
end;

function TERDCommonDialog.Palette: TERDThemePalette;
begin
  Result := ERDDialogPalette(FTheme);
end;

function TERDCommonDialog.NewWindow: TERDDialogWindow;
var
  S: string;
begin
  S := FTitle;
  if S = '' then
    S := DefaultTitle;
  Result := TERDDialogWindow.CreateWindow(Application, FTheme, GetDensity, S);
  Result.Sizable := FResizable;
end;

function TERDCommonDialog.RunWindow(AWindow: TERDDialogWindow): TModalResult;
begin
  DoShow;
  try
    Result := AWindow.Run;
  finally
    DoClose;
  end;
end;

procedure TERDCommonDialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TERDCommonDialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

{ TDialogContent }

constructor TDialogContent.CreateContent(AOwner: TComponent;
  ADialog: TERDDialog);
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
  Painter: TERDPainter;
  P: TERDThemePalette;
  C, Ink: TColor;
  TextRect: TRect;
  X, Y: Integer;
begin
  P := Palette;
  Painter := TERDPainter.Create(ACanvas, P, ScaleValue(96));
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

    Painter.Text(ScaleValue(56), ScaleValue(22), FDialog.Title, ERD_TEXT_TITLE,
      P.ForegroundText, twBold, taLeftJustify, Width - ScaleValue(72));
    TextRect := Rect(ScaleValue(56), ScaleValue(40), Width - ScaleValue(20),
      Height - ScaleValue(8));
    Painter.WrapText(TextRect, FDialog.Text, 12.5, P.ForegroundText,
      twRegular);
  finally
    Painter.Free;
  end;
end;

{ TERDDialog }

constructor TERDDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FTitle := 'Application';
  FKind := dkWarning;
  FButtons := [mbOK];
  FDefaultButton := mbOK;
  FDangerButton := mbNo;
  FButtonCaptions := TStringList.Create;
end;

destructor TERDDialog.Destroy;
begin
  FButtonCaptions.Free;
  SetTheme(nil);
  inherited;
end;

procedure TERDDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDDialog.ThemeChanged;
begin
end;

function TERDDialog.GetPalette: TERDThemePalette;
begin
  Result := ERDDialogPalette(FTheme);
end;

procedure TERDDialog.SetTheme(AValue: TERDTheme);
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

procedure TERDDialog.SetDensity(AValue: TERDDensity);
begin
  FDensity := AValue;
end;

procedure TERDDialog.SetTitle(const AValue: string);
begin
  FTitle := AValue;
end;

procedure TERDDialog.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TERDDialog.SetKind(AValue: TERDDialogKind);
begin
  FKind := AValue;
end;

procedure TERDDialog.SetButtons(AValue: TMsgDlgButtons);
begin
  if AValue = [] then
    AValue := [mbOK];
  FButtons := AValue;
end;

procedure TERDDialog.SetDefaultButton(AValue: TMsgDlgBtn);
begin
  FDefaultButton := AValue;
end;

procedure TERDDialog.SetButtonCaptions(AValue: TStrings);
begin
  FButtonCaptions.Assign(AValue);
end;

procedure TERDDialog.SetDangerButton(AValue: TMsgDlgBtn);
begin
  FDangerButton := AValue;
end;

procedure TERDDialog.SetCheckBoxCaption(const AValue: string);
begin
  FCheckBoxCaption := AValue;
end;

procedure TERDDialog.SetCheckBoxChecked(AValue: Boolean);
begin
  FCheckBoxChecked := AValue;
end;

function TERDDialog.CaptionForButton(AButton: TMsgDlgBtn): string;
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

function TERDDialog.Execute: TModalResult;
const
  ORDER: array[0..11] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort,
    mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbClose, mbHelp);
var
  Form: TERDDialogWindow;
  Content: TDialogContent;
  Check: TERDCheckBox;
  Btn: TERDButton;
  BodyH, I: Integer;
  Button: TMsgDlgBtn;
  Kind: TERDButtonKind;
  Focus: TERDButton;
begin
  BodyH := 128;
  if Pos(sLineBreak, FText) > 0 then
    Inc(BodyH, 24);
  Form := TERDDialogWindow.CreateWindow(Application, FTheme, FDensity,
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

function ERDMessageDlg(const ATitle, AText: string; AKind: TERDDialogKind;
  AButtons: TMsgDlgButtons; ATheme: TERDTheme): TModalResult;
var
  Dialog: TERDDialog;
begin
  Dialog := TERDDialog.Create(nil);
  try
    Dialog.Title := ATitle;
    Dialog.Text := AText;
    Dialog.Kind := AKind;
    Dialog.Buttons := AButtons;
    Dialog.Theme := ATheme;
    if ATheme <> nil then
      Dialog.Density := ATheme.Density
    else if TERDTheme.GetDefault <> nil then
      Dialog.Density := TERDTheme.GetDefault.Density;
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

{ TInputController }

procedure TInputController.Setup(ACount: Integer; AOK: TERDButton;
  const AValidator: TERDInputValidator);
begin
  SetLength(FEdits, ACount);
  SetLength(FMessages, ACount);
  FOK := AOK;
  FValidator := AValidator;
end;

procedure TInputController.SetField(AIndex: Integer; AEdit: TERDEdit;
  AMessage: TERDDialogText);
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

function ERDInputQuery(const ACaption, AText: string;
  const APrompts: array of string; var AValues: array of string;
  const AValidator: TERDInputValidator; ATheme: TERDTheme): Boolean;
var
  Form: TERDDialogWindow;
  Controller: TInputController;
  Edit: TERDEdit;
  Msg: TERDDialogText;
  OK, CancelBtn: TERDButton;
  Density: TERDDensity;
  I, Count, EditH, Y, BodyH: Integer;
  First: TERDEdit;
begin
  Count := System.Math.Min(Length(APrompts), Length(AValues));
  Density := dnDesktop;
  if ATheme <> nil then
    Density := ATheme.Density
  else if TERDTheme.GetDefault <> nil then
    Density := TERDTheme.GetDefault.Density;
  EditH := DensityMetrics(Density).Edit;
  BodyH := 16 + Count * (18 + EditH + 20) + 4;
  if AText <> '' then
    Inc(BodyH, 32);
  Form := TERDDialogWindow.CreateWindow(Application, ATheme, Density,
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
      Edit := TERDEdit.Create(Form);
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

function ERDInputQuery(const ACaption, APrompt: string;
  var AValue: string; ATheme: TERDTheme): Boolean;
var
  Values: array of string;
begin
  SetLength(Values, 1);
  Values[0] := AValue;
  Result := ERDInputQuery(ACaption, '', [APrompt], Values, nil, ATheme);
  if Result then
    AValue := Values[0];
end;

function ERDInputBox(const ACaption, APrompt, ADefault: string;
  ATheme: TERDTheme): string;
begin
  Result := ADefault;
  if not ERDInputQuery(ACaption, APrompt, Result, ATheme) then
    Result := ADefault;
end;

procedure ERDShowMessage(const AMessage: string; ATheme: TERDTheme);
var
  Title: string;
begin
  Title := Application.Title;
  if Title = '' then
    Title := 'Application';
  ERDMessageDlg(Title, AMessage, dkInfo, [mbOK], ATheme);
end;

end.
