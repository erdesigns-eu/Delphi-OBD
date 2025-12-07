//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Statusbar.pas
// CONTENTS       : Statusbar component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
// UPDATED        : 06/12/2025 - Refactored to inherit from TOBDCustomControl
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Touch.Statusbar;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls,
  Vcl.Forms, System.Skia, Skia.Vcl,

  OBD.CustomControl, OBD.LED, OBD.CustomControl.Helpers, OBD.CustomControl.Constants;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 21;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Size grip dot shape type
  /// </summary>
  TOBDTouchStatusbarSizeGripDotShape = (dsSquare, dsCircle);
  /// <summary>
  ///   Statusbar panel style
  /// </summary>
  TOBDTouchStatusbarPanelStyle = (psSimpleText, psAdvancedText);

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Statusbar background properties
  /// </summary>
  TOBDTouchStatusbarBackground = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar border properties
  /// </summary>
  TOBDTouchStatusbarBorder = class(TPersistent)
  private
    /// <summary>
    ///   Gradient from color
    /// </summary>
    FFromColor: TColor;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    FToColor: TColor;
    /// <summary>
    ///   Border height
    /// </summary>
    FHeight: Integer;

    /// <summary>
    ///   Set gradient from color
    /// </summary
    procedure SetFromColor(Value: TColor);
    /// <summary>
    ///   Set gradient to color
    /// </summary
    procedure SetToColor(Value: TColor);
    /// <summary>
    ///   Set border height
    /// </summary>
    procedure SetHeight(Value: Integer);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Gradient from color
    /// </summary>
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Border height
    /// </summary>
    property Height: Integer read FHeight write SetHeight default DEFAULT_BORDER_HEIGHT;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Sizegrip properties
  /// </summary>
  TOBDTouchStatusbarSizeGrip = class(TPersistent)
  private
    /// <summary>
    ///   Size Grip rect
    /// </summary>
    FSizeGripRect: TRect;
  private
    /// <summary>
    ///   Dot Color
    /// </summary>
    FDotColor: TColor;
    /// <summary>
    ///   Dot size
    /// </summary>
    FDotSize: Single;
    /// <summary>
    ///   Dot spacing
    /// </summary>
    FDotSpacing: Single;
    /// <summary>
    ///   Dot shape
    /// </summary>
    FDotShape: TOBDTouchStatusbarSizeGripDotShape;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;

    /// <summary>
    ///   Set dot color
    /// </summary
    procedure SetDotColor(Value: TColor);
    /// <summary>
    ///   Set dot size
    /// </summary>
    procedure SetDotSize(Value: Single);
    /// <summary>
    ///   Set dot spacing
    /// </summary>
    procedure SetDotSpacing(Value: Single);
    /// <summary>
    ///   Set dot shape
    /// </summary>
    procedure SetDotShape(Value: TOBDTouchStatusbarSizeGripDotShape);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Size Grip rect
    /// </summary>
    property SizeGripRect: TRect read FSizeGripRect write FSizeGripRect;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Dot Color
    /// </summary>
    property DotColor: TColor read FDotColor write SetDotColor default DEFAULT_SIZEGRIP_COLOR;
    /// <summary>
    ///   Dot size
    /// </summary>
    property DotSize: Single read FDotSize write SetDotSize;
    /// <summary>
    ///   Dot spacing
    /// </summary>
    property DotSpacing: Single read FDotSpacing write SetDotSpacing;
    /// <summary>
    ///   Dot shape
    /// </summary>
    property DotShape: TOBDTouchStatusbarSizeGripDotShape read FDotShape write SetDotShape default dsCircle;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Panel
  /// </summary>
  TOBDTouchStatusbarPanel = class(TCollectionItem)
  private
    /// <summary>
    ///   Panel Rect
    /// </summary>
    FPanelRect: TRect;
  private
    /// <summary>
    ///   Width
    /// </summary>
    FWidth: Integer;
    /// <summary>
    ///   Status Panel style
    /// </summary>
    FStyle: TOBDTouchStatusbarPanelStyle;
    /// <summary>
    ///   Text
    /// </summary>
    FText: TCaption;
    /// <summary>
    ///   Primary Text
    /// </summary>
    FPrimaryText: TCaption;
    /// <summary>
    ///   Secondary Text
    /// </summary>
    FSecondaryText: TCaption;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Primary Font
    /// </summary>
    FPrimaryFont: TFont;
    /// <summary>
    ///   Secondary Font
    /// </summary>
    FSecondaryFont: TFont;
    /// <summary>
    ///   Show LED
    /// </summary>
    FShowLed: Boolean;
    /// <summary>
    ///   LED state
    /// </summary>
    FLedState: TOBDLedState;
    /// <summary>
    ///   Auto size
    /// </summary>
    FAutoSize: Boolean;
    /// <summary>
    ///   Led border color
    /// </summary>
    FLedBorder: TOBDLedBorder;
    /// <summary>
    ///   Led grayed color
    /// </summary>
    FLedGrayedColor: TOBDLedGrayedColor;
    /// <summary>
    ///   Led off color
    /// </summary>
    FLedOffColor: TOBDLedOffColor;
    /// <summary>
    ///   Led on color
    /// </summary>
    FLedOnColor: TOBDLedOnColor;

    /// <summary>
    ///   Set width
    /// </summary>
    procedure SetWidth(Value: Integer);
    /// <summary>
    ///   Set status Panel style
    /// </summary>
    procedure SetStyle(Value: TOBDTouchStatusbarPanelStyle);
    /// <summary>
    ///   Set Text
    /// </summary>
    procedure SetText(Value: TCaption);
    /// <summary>
    ///   Set Primary Text
    /// </summary>
    procedure SetPrimaryText(Value: TCaption);
    /// <summary>
    ///   Set Secondary Text
    /// </summary>
    procedure SetSecondaryText(Value: TCaption);
    /// <summary>
    ///   Set Font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set Primary Font
    /// </summary>
    procedure SetPrimaryFont(Value: TFont);
    /// <summary>
    ///   Set Secondary Font
    /// </summary>
    procedure SetSecondaryFont(Value: TFont);
    /// <summary>
    ///   Set show LED
    /// </summary>
    procedure SetShowLed(Value: Boolean);
    /// <summary>
    ///   Set LED state
    /// </summary>
    procedure SetLedState(Value: TOBDLedState);
    /// <summary>
    ///   Set auto size
    /// </summary>
    procedure SetAutoSize(Value: Boolean);
    /// <summary>
    ///   Set Led border color
    /// </summary>
    procedure SetLedBorder(Value: TOBDLedBorder);
    /// <summary>
    ///   Set Led grayed color
    /// </summary>
    procedure SetLedGrayedColor(Value: TOBDLedGrayedColor);
    /// <summary>
    ///   Set Led off color
    /// </summary>
    procedure SetLedOffColor(Value: TOBDLedOffColor);
    /// <summary>
    ///   Set Led on color
    /// </summary>
    procedure SetLedOnColor(Value: TOBDLedOnColor);
  protected
    /// <summary>
    ///   Settings changed handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   Override get displayname function
    /// </summary>
    function GetDisplayName : String; override;
  protected
    /// <summary>
    ///   Panel Rect
    /// </summary>
    property PanelRect: TRect read FPanelRect write FPanelRect;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default 100;
    /// <summary>
    ///   Status Panel style
    /// </summary>
    property Style: TOBDTouchStatusbarPanelStyle read FStyle write SetStyle default psSimpleText;
    /// <summary>
    ///   Text
    /// </summary>
    property Text: TCaption read FText write SetText;
    /// <summary>
    ///   Primary Text
    /// </summary>
    property PrimaryText: TCaption read FPrimaryText write SetPrimaryText;
    /// <summary>
    ///   Secondary Text
    /// </summary>
    property SecondaryText: TCaption read FSecondaryText write SetSecondaryText;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Primary Font
    /// </summary>
    property PrimaryFont: TFont read FPrimaryFont write SetPrimaryFont;
    /// <summary>
    ///   Secondary Font
    /// </summary>
    property SecondaryFont: TFont read FSecondaryFont write SetSecondaryFont;
    /// <summary>
    ///   Show LED
    /// </summary>
    property ShowLed: Boolean read FShowLed write SetShowLed default False;
    /// <summary>
    ///   LED state
    /// </summary>
    property LedState: TOBDLedState read FLedState write SetLedState default lsOff;
    /// <summary>
    ///   Auto size
    /// </summary>
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    /// <summary>
    ///   Led border color
    /// </summary>
    property LedBorder: TOBDLedBorder read FLedBorder write SetLedBorder;
    /// <summary>
    ///   Led grayed color
    /// </summary>
    property LedGrayedColor: TOBDLedGrayedColor read FLedGrayedColor write SetLedGrayedColor;
    /// <summary>
    ///   Led off color
    /// </summary>
    property LedOffColor: TOBDLedOffColor read FLedOffColor write SetLedOffColor;
    /// <summary>
    ///   Led on color
    /// </summary>
    property LedOnColor: TOBDLedOnColor read FLedOnColor write SetLedOnColor;
  end;

  /// <summary>
  ///   Touch Statusbar Panel Collection
  /// </summary>
  TOBDTouchStatusbarPanelCollection = class(TOwnedCollection)
  private
    /// <summary>
    ///   On Change event
    /// </summary>
    FOnChange: TNotifyEvent;

    /// <summary>
    ///   Get status panel
    /// </summary>
    function GetItem(AIndex: Integer): TOBDTouchStatusbarPanel;
    /// <summary>
    ///   Set status panel
    /// </summary>
    procedure SetItem(AIndex: Integer; const Value: TOBDTouchStatusbarPanel);
  protected
    /// <summary>
    ///   Override Item update handler
    /// </summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TPersistent); virtual;

    /// <summary>
    ///   Add status panel
    /// </summary>
    function Add: TOBDTouchStatusbarPanel;
    /// <summary>
    ///   Assign
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Panels
    /// </summary>
    property Panels[AIndex: Integer]: TOBDTouchStatusbarPanel read GetItem write SetItem; default;
    /// <summary>
    ///   On Change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Statusbar Component
  /// </summary>
  TOBDTouchStatusbar = class(TOBDCustomControl)
  private
    /// <summary>
    ///   Class constructor
    /// </summary>
    class constructor Create;
    /// <summary>
    ///   Class destructor
    /// </summary>
    class destructor Destroy;
  private
    /// <summary>
    ///   Background
    /// </summary>
    FBackground: TOBDTouchStatusbarBackground;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDTouchStatusbarBorder;
    /// <summary>
    ///   Size Grip
    /// </summary>
    FSizeGrip: TOBDTouchStatusbarSizeGrip;
    /// <summary>
    ///   Panels
    /// </summary>
    FPanels: TOBDTouchStatusbarPanelCollection;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchStatusbarBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchStatusbarBorder);
    /// <summary>
    ///   Set size grip
    /// </summary>
    procedure SetSizeGrip(Value: TOBDTouchStatusbarSizeGrip);
    /// <summary>
    ///   Set panels
    /// </summary>
    procedure SetPanels(Value: TOBDTouchStatusbarPanelCollection);
  private
    /// <summary>
    ///   WM_KILLFOCUS message handler
    /// </summary>
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    /// <summary>
    ///   CM_ENABLEDCHANGED message handler
    /// </summary>
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    /// <summary>
    ///   CM_MOUSELEAVE message handler
    /// </summary>
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>
    ///   Override UpdateStyleElements method
    /// </summary>
    procedure UpdateStyleElements; override;
    /// <summary>
    ///   Override MouseDown method
    /// </summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>
    ///   Override MouseUp method
    /// </summary>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>
    ///   Override MouseMove method
    /// </summary>
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    /// <summary>
    ///   Override WndProc method
    /// </summary>
    procedure WndProc(var Message: TMessage); override;
  protected
    /// <summary>
    ///   Settings changed handler
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   Paint with Skia canvas
    /// </summary>
    procedure PaintSkia(Canvas: ISkCanvas); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override Repaint method
    /// </summary>
    procedure Repaint; override;
    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Background
    /// </summary>
    property Background: TOBDTouchStatusbarBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchStatusbarBorder read FBorder write SetBorder;
    /// <summary>
    ///   Size Grip
    /// </summary>
    property SizeGrip: TOBDTouchStatusbarSizeGrip read FSizeGrip write SetSizeGrip;
    /// <summary>
    ///   Panels
    /// </summary>
    property Panels: TOBDTouchStatusbarPanelCollection read FPanels write SetPanels;
  published
    /// <summary>
    ///   Component alignment (inherited)
    /// </summary>
    property Align default alBottom;
    /// <summary>
    ///   Component anchors (inherited)
    /// </summary>
    property Anchors;
    /// <summary>
    ///   Component color (inherited)
    /// </summary>
    property Color;
  end;

implementation

uses
  System.Math, System.Skia, Skia.Vcl;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBackground.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBackground.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarBackground.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BACKGROUND_FROM;
  FToColor := DEFAULT_BACKGROUND_TO;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarBackground) then
  begin
    FFromColor := (Source as TOBDTouchStatusbarBackground).FromColor;
    FToColor := (Source as TOBDTouchStatusbarBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBorder.SetFromColor(Value: TColor);
begin
  if (FFromColor <> Value) then
  begin
    // Set the new from color
    FFromColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET TO COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBorder.SetToColor(Value: TColor);
begin
  if (FToColor <> Value) then
  begin
    // Set the new to color
    FToColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBorder.SetHeight(Value: Integer);
begin
  if (FHeight <> Value) and (Value >= 0) then
  begin
    // Set the new height
    FHeight := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_TO;
  FToColor := DEFAULT_BORDER_FROM;
  FHeight := DEFAULT_BORDER_HEIGHT;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarBorder) then
  begin
    FFromColor := (Source as TOBDTouchStatusbarBorder).FromColor;
    FToColor := (Source as TOBDTouchStatusbarBorder).ToColor;
    FHeight := (Source as TOBDTouchStatusbarBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET DOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotColor(Value: TColor);
begin
  if (FDotColor <> Value) then
  begin
    // Set the new dot color
    FDotColor := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotSize(Value: Single);
begin
  if (FDotSize <> Value) and (Value >= 1) then
  begin
    // Set the new dot size
    FDotSize := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SPACING
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotSpacing(Value: Single);
begin
  if (FDotSpacing <> Value) and (Value >= 1) then
  begin
    // Set the new dot spacing
    FDotSpacing := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET DOT SHAPE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetDotShape(Value: TOBDTouchStatusbarSizeGripDotShape);
begin
  if (FDotShape <> Value) then
  begin
    // Set the new dot shape
    FDotShape := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify the change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarSizeGrip.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FDotColor := DEFAULT_SIZEGRIP_COLOR;
  FDotSize := 2;
  FDotSpacing := 1;
  FDotShape := dsCircle;
  FVisible := True;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarSizeGrip.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarSizeGrip) then
  begin
    FDotColor := (Source as TOBDTouchStatusbarSizeGrip).DotColor;
    FDotSize := (Source as TOBDTouchStatusbarSizeGrip).DotSize;
    FDotSpacing := (Source as TOBDTouchStatusbarSizeGrip).DotSpacing;
    FDotShape := (Source as TOBDTouchStatusbarSizeGrip).DotShape;
    FVisible := (Source as TOBDTouchStatusbarSizeGrip).Visible;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET STYLE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetStyle(Value: TOBDTouchStatusbarPanelStyle);
begin
  if (FStyle <> Value) then
  begin
    // Set new style
    FStyle := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET TEXT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetText(Value: TCaption);
begin
  if (FText <> Value) then
  begin
    // Set new text
    FText := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET PRIMARY TEXT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetPrimaryText(Value: TCaption);
begin
  if (FPrimaryText <> Value) then
  begin
    // Set new primary text
    FPrimaryText := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET SECONDARY TEXT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetSecondaryText(Value: TCaption);
begin
  if (FSecondaryText <> Value) then
  begin
    // Set new secondary text
    FSecondaryText := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetFont(Value: TFont);
begin
  // Assign font
  FFont.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET PRIMARY FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetPrimaryFont(Value: TFont);
begin
  // Assign primary font
  FPrimaryFont.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET SECONDARY FONT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetSecondaryFont(Value: TFont);
begin
  // Assign secondary font
  FSecondaryFont.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET SHOW LED
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetShowLed(Value: Boolean);
begin
  if (FShowLed <> Value) then
  begin
    // Set show LED
    FShowLed := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET LED STATE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetLedState(Value: TOBDLedState);
begin
  if (FLedState <> Value) then
  begin
    // Set new LED state
    FLedState := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET AUTO SIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetAutoSize(Value: Boolean);
begin
  if (FAutoSize <> Value) then
  begin
    // Set new autosize
    FAutoSize := Value;
    // Notify change
    Changed(False);
  end;
end;

//------------------------------------------------------------------------------
// SET LED BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetLedBorder(Value: TOBDLedBorder);
begin
  // Assign color
  FLedBorder.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET LED GRAYED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetLedGrayedColor(Value: TOBDLedGrayedColor);
begin
  FLedGrayedColor.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET LED OFF COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetLedOffColor(Value: TOBDLedOffColor);
begin
  // Assign color
  FLedOffColor.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SET LED ON COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SetLedOnColor(Value: TOBDLedOnColor);
begin
  // Assign color
  FLedOnColor.Assign(Value);
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.SettingsChanged(Sender: TObject);
begin
  // Notify change
  Changed(False);
end;

//------------------------------------------------------------------------------
// GET DISPLAY NAME
//------------------------------------------------------------------------------
function TOBDTouchStatusbarPanel.GetDisplayName: string;
begin
  // Return text
  if (FText <> '') then
    Result := FText
  else
  // Return primary and secondary text
  if (FPrimaryText <> '') and (FSecondaryText <> '') then
    Result := FPrimaryText + ' - ' + FSecondaryText
  else
  // Return primary text
  if (FPrimaryText <> '') then
    Result := FPrimaryText
  else
  // Return secondary text
  if (FSecondaryText <> '') then
    Result := FSecondaryText
  else
  // Return default (inherited) text
    Result := inherited GetDisplayName;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarPanel.Create(Collection: TCollection);
begin
  // Call inherited constructor
  inherited Create(Collection);
  // Create fonts
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FPrimaryFont := TFont.Create;
  FPrimaryFont.OnChange := SettingsChanged;
  FSecondaryFont := TFont.Create;
  FSecondaryFont.OnChange := SettingsChanged;
  // Set defaults
  FWidth := 100;
  FStyle := psSimpleText;
  FText := Format('Panel %d', [Index + 1]);
  FPrimaryText := '';
  FSecondaryText := '';
  FShowLed := False;
  FLedState := lsOff;
  FAutoSize := True;
  // Create LED colors
  FLedBorder := TOBDLedBorder.Create;
  FLedBorder.OnChange := SettingsChanged;
  FLedGrayedColor := TOBDLedGrayedColor.Create;
  FLedGrayedColor.OnChange := SettingsChanged;
  FLedOffColor := TOBDLedOffColor.Create;
  FLedOffColor.OnChange := SettingsChanged;
  FLedOnColor := TOBDLedOnColor.Create;
  FLedOnColor.OnChange := SettingsChanged;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbarPanel.Destroy;
begin
  // Free fonts
  FFont.Free;
  FPrimaryFont.Free;
  FSecondaryFont.Free;
  // Free LED colors
  FLedBorder.Free;
  FLedGrayedColor.Free;
  FLedOffColor.Free;
  FLedOnColor.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanel.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchStatusbarPanel) then
  begin
    FWidth := (Source as TOBDTouchStatusbarPanel).Width;
    FStyle := (Source as TOBDTouchStatusbarPanel).Style;
    FText := (Source as TOBDTouchStatusbarPanel).Text;
    FPrimaryText := (Source as TOBDTouchStatusbarPanel).PrimaryText;
    FSecondaryText := (Source as TOBDTouchStatusbarPanel).SecondaryText;
    FShowLed := (Source as TOBDTouchStatusbarPanel).ShowLed;
    FLedState := (Source as TOBDTouchStatusbarPanel).LedState;
    FAutoSize := (Source as TOBDTouchStatusbarPanel).AutoSize;
    FFont.Assign((Source as TOBDTouchStatusbarPanel).Font);
    FPrimaryFont.Assign((Source as TOBDTouchStatusbarPanel).PrimaryFont);
    FSecondaryFont.Assign((Source as TOBDTouchStatusbarPanel).SecondaryFont);
    // Notify change
    Changed(False);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// GET PANEL ITEM
//------------------------------------------------------------------------------
function TOBDTouchStatusbarPanelCollection.GetItem(AIndex: Integer): TOBDTouchStatusbarPanel;
begin
  Result := TOBDTouchStatusbarPanel(inherited Items[AIndex]);
end;

//------------------------------------------------------------------------------
// SET PANEL ITEM
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanelCollection.SetItem(AIndex: Integer; const Value: TOBDTouchStatusbarPanel);
begin
  // Call inherited set item
  inherited SetItem(AIndex, Value);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// ITEM UPDATE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanelCollection.Update(Item: TCollectionItem);
begin
  // Call inherited update
  inherited Update(Item);
  // Notify change
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbarPanelCollection.Create(AOwner: TPersistent);
begin
  // Call inherited constructor
  inherited Create(AOwner, TOBDTouchStatusbarPanel);
end;

//------------------------------------------------------------------------------
// ADD PANEL ITEM
//------------------------------------------------------------------------------
function TOBDTouchStatusbarPanelCollection.Add: TOBDTouchStatusbarPanel;
begin
  // Create new panel item
  Result := TOBDTouchStatusbarPanel(inherited Add);
  // Set new panel item text
  Result.Text := Format('Panel %d', [NextID]);
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbarPanelCollection.Assign(Source: TPersistent);
var
  L: TOBDTouchStatusbarPanelCollection;
  I: Integer;
begin
  if (Source is TOBDTouchStatusbarPanelCollection) then
  begin
    // Cast the list as TOBDTouchStatusbarPanelCollection
    L := TOBDTouchStatusbarPanelCollection(Source);
    // Clear the items
    Clear;
    // Add the items
    for I := 0 to L.Count - 1 do Add.Assign(L.Items[I]);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchStatusbar.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchStatusbar, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchStatusbar.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchStatusbar, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetBackground(Value: TOBDTouchStatusbarBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetBorder(Value: TOBDTouchStatusbarBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET SIZE GRIP
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetSizeGrip(Value: TOBDTouchStatusbarSizeGrip);
begin
  FSizeGrip.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PANELS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SetPanels(Value: TOBDTouchStatusbarPanelCollection);
begin
  FPanels.Assign(Value);
end;

//------------------------------------------------------------------------------
// WM_KILLFOCUS MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// CM_ENABLEDCHANGED MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// CM_MOUSELEAVE MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.UpdateStyleElements;
begin
  // Call inherited
  inherited;
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// MOUSE DOWN HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousedown
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE UP HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mouseup
  inherited;
end;

//------------------------------------------------------------------------------
// MOUSE MOVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  // Call inherited mousemove
  inherited;
end;

//------------------------------------------------------------------------------
// WND PROC HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WndProc(var Message: TMessage);
var
  P: TPoint;
  Form: TCustomForm;
begin
  // Call inherited WndProc
  inherited WndProc(Message);

  // Get the parent form
  Form := GetParentForm(Self);
  // If there is a parentform and the window state is normal
  if Assigned(Form) and (Form is TForm) and (Form.WindowState = wsNormal) then
  begin
    // Check if we are in the sizegrip rect
    if (Message.Msg = WM_NCHITTEST) and FSizeGrip.Visible then
    begin
      P := ScreenToClient(Mouse.CursorPos);
      if PtInRect(FSizeGrip.SizeGripRect, P) then
      begin
        Message.Result := HTBOTTOMRIGHT;
        Exit;
      end;
    end;

    // Handle the WM_NCLBUTTONDOWN message to start resizing
    if (Message.Msg = WM_NCLBUTTONDOWN) and (Message.WParam = HTBOTTOMRIGHT) then
    begin
      ReleaseCapture;
      Form.Perform(WM_SYSCOMMAND, SC_SIZE + 9, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.SettingsChanged(Sender: TObject);
begin
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.PaintSkia(Canvas: ISkCanvas);
const
  PanelPadding = 4;
  LedSize = 14;
var
  Paint: ISkPaint;
  Typeface: ISkTypeface;
  SkFont: ISkFont;
  Metrics: TSkFontMetrics;
  BackgroundRect, BorderRect, LedRect, LedBorderRect, SizeGripRect: TRectF;
  SizeGripWidth, I, W, S, PanelX: Integer;
  X, Y: Single;

  function MeasureTextWidth(const Text: string; const AFont: TFont): Integer;
  var
    TextPaint: ISkPaint;
    TextFont: ISkFont;
    Bounds: TRectF;
  begin
    TextPaint := TSkPaint.Create;
    TextPaint.AntiAlias := True;
    TextPaint.Style := TSkPaintStyle.Fill;

    TextFont := TSkFont.Create(CreateSkTypeface(AFont), AFont.Size);
    TextFont.MeasureText(Text, Bounds, TextPaint);
    Result := Ceil(Bounds.Width);
  end;

  procedure PaintLed(Panel: TOBDTouchStatusbarPanel);
  begin
    X := Panel.PanelRect.Left;
    Y := (Border.Height + ((ClientRect.Height - Border.Height) / 2)) - (LedSize / 2);

    LedBorderRect := TRectF.Create(X, Y, X + LedSize, Y + LedSize);
    LedRect := TRectF.Create(
      LedBorderRect.Left + Panel.LedBorder.Width + 1,
      LedBorderRect.Top + Panel.LedBorder.Width + 1,
      LedBorderRect.Right - Panel.LedBorder.Width - 1,
      LedBorderRect.Bottom - Panel.LedBorder.Width - 1
    );

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    case Panel.LedState of
      lsGrayed : Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(LedRect.Left, LedRect.Top),
        TPointF.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedGrayedColor.FromColor), SafeColorRefToSkColor(Panel.LedGrayedColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
      lsOff    : Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(LedRect.Left, LedRect.Top),
        TPointF.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedOffColor.FromColor), SafeColorRefToSkColor(Panel.LedOffColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
      lsOn     : Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(LedRect.Left, LedRect.Top),
        TPointF.Create(LedRect.Left, LedRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedOnColor.FromColor), SafeColorRefToSkColor(Panel.LedOnColor.ToColor)],
        nil,
        TSkTileMode.Clamp);
    end;
    Canvas.DrawOval(LedRect, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := 1.5;
    case Panel.LedState of
      lsGrayed : Paint.Color := SafeColorRefToSkColor(Panel.LedGrayedColor.FromColor);
      lsOff    : Paint.Color := SafeColorRefToSkColor(Panel.LedOffColor.FromColor);
      lsOn     : Paint.Color := SafeColorRefToSkColor(Panel.LedOnColor.FromColor);
    end;
    Canvas.DrawOval(LedRect, Paint);

    if (Panel.LedBorder.FromColor <> clNone) and (Panel.LedBorder.ToColor <> clNone) and (Panel.LedBorder.Width > 0) then
    begin
      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.StrokeWidth := Panel.LedBorder.Width;
      Paint.Shader := TSkShader.MakeGradientLinear(
        TPointF.Create(LedBorderRect.Left, LedBorderRect.Top),
        TPointF.Create(LedBorderRect.Left, LedBorderRect.Bottom),
        [SafeColorRefToSkColor(Panel.LedBorder.FromColor), SafeColorRefToSkColor(Panel.LedBorder.ToColor)],
        nil,
        TSkTileMode.Clamp);
      Canvas.DrawOval(LedBorderRect, Paint);
    end;
  end;
begin
  try
    // Clear canvas with resolved background color
    Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

    // Paint the background gradient when both colors are provided
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
  begin
    BackgroundRect := TRectF.Create(0.0, 0.0, Width + 0.0, Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(BackgroundRect.Left, BackgroundRect.Top),
      TPointF.Create(BackgroundRect.Left, BackgroundRect.Bottom),
      [SafeColorRefToSkColor(Background.FromColor), SafeColorRefToSkColor(Background.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BackgroundRect, Paint);
  end;

  // Draw the top border strip when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TRectF.Create(0.0, 0.0, Width + 0.0, Border.Height + 0.0);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(BorderRect.Left, BorderRect.Top),
      TPointF.Create(BorderRect.Left, BorderRect.Bottom),
      [SafeColorRefToSkColor(Border.FromColor), SafeColorRefToSkColor(Border.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawRect(BorderRect, Paint);
  end;

  // Calculate panel rectangles based on auto-size/text/LED requirements
  PanelX := ClientRect.Left;
  for I := 0 to Panels.Count -1 do
  begin
    W := 0;
    if Panels[I].AutoSize then
    begin
      if Panels[I].ShowLed then W := W + LedSize + PanelPadding + 2;
      if Panels[I].Style = psSimpleText then
        W := W + MeasureTextWidth(Panels[I].Text, Panels[I].Font)
      else
      begin
        W := W + MeasureTextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);
        W := W + MeasureTextWidth(Panels[I].SecondaryText, Panels[I].SecondaryFont);
        W := W + PanelPadding;
      end;
    end else
    begin
      if Panels[I].ShowLed then W := W + LedSize;
      W := W + Panels[I].Width;
    end;
    Panels[I].PanelRect := TRect.Create(
      PanelX + PanelPadding,
      ClientRect.Top,
      PanelX + PanelPadding + W + PanelPadding,
      ClientRect.Bottom
    );
    PanelX := PanelX + Panels[I].PanelRect.Width;
  end;

  // Render LEDs for all panels that need them
  for I := 0 to Panels.Count -1 do
  begin
    if Panels[I].ShowLed then
      PaintLed(Panels[I]);
  end;

  // Draw the resize size grip as a 3x3 matrix of dots
  if SizeGrip.Visible then
  begin
    SizeGripRect := TRectF.Create(0.0, Border.Height + 0.0, Width - 3.0, Height - Border.Height + 0.0);
    SizeGripWidth := Ceil((SizeGrip.DotSize + SizeGrip.DotSpacing) * 3);
    FSizeGrip.SizeGripRect := Rect((Width - 3) - SizeGripWidth, Border.Height, Width - 3, Height);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := SafeColorRefToSkColor(SizeGrip.DotColor);
    for W := 0 to 2 do
      for S := 0 to 2 do
      begin
        X := SizeGripRect.Right - SizeGripWidth + (W * (SizeGrip.DotSize + SizeGrip.DotSpacing));
        Y := SizeGripRect.Bottom - ((S + 1) * (SizeGrip.DotSize + SizeGrip.DotSpacing));
        if SizeGrip.DotShape = dsSquare then
          Canvas.DrawRect(TRectF.Create(X, Y, X + SizeGrip.DotSize, Y + SizeGrip.DotSize), Paint)
        else
          Canvas.DrawCircle(TPointF.Create(X + (SizeGrip.DotSize / 2), Y + (SizeGrip.DotSize / 2)), SizeGrip.DotSize / 2, Paint);
      end;
  end;

  // Draw captions for each panel, honoring LED offsets and style differences
  for I := 0 to Panels.Count -1 do
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;

    if Panels[I].ShowLed then
      X := LedSize + 2
    else
      X := 0;

    if Panels[I].Style = psSimpleText then
    begin
      Typeface := CreateSkTypeface(Panels[I].Font);
      SkFont := TSkFont.Create(Typeface, Panels[I].Font.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].Font.Color);
      SkFont.GetMetrics(Metrics);
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].Text, Panels[I].PanelRect.Left + X, Y, SkFont, Paint);
    end else
    begin
      Typeface := CreateSkTypeface(Panels[I].PrimaryFont);
      SkFont := TSkFont.Create(Typeface, Panels[I].PrimaryFont.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].PrimaryFont.Color);
      SkFont.GetMetrics(Metrics);
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].PrimaryText, Panels[I].PanelRect.Left + X, Y, SkFont, Paint);

      X := X + MeasureTextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);

      Typeface := CreateSkTypeface(Panels[I].SecondaryFont);
      SkFont := TSkFont.Create(Typeface, Panels[I].SecondaryFont.Size);
      Paint.Color := SafeColorRefToSkColor(Panels[I].SecondaryFont.Color);
      SkFont.GetMetrics(Metrics);
      Y := Panels[I].PanelRect.Top + Border.Height + ((Panels[I].PanelRect.Height - Border.Height) / 2) - ((Metrics.Ascent + Metrics.Descent) / 2);
      Canvas.DrawSimpleText(Panels[I].SecondaryText, Panels[I].PanelRect.Left + X + PanelPadding, Y, SkFont, Paint);
    end;
  end;

    // Direct rendering to canvas - no conversion needed!
  except
    on E: Exception do
    begin
      // On error, clear canvas with background color
      Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));
    end;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbar.Create(AOwner: TComponent);
begin
  // Call inherited constructor (handles rendering setup)
  inherited Create(AOwner);
  // Create background
  FBackground := TOBDTouchStatusbarBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchStatusbarBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create size grip
  FSizegrip := TOBDTouchStatusbarSizeGrip.Create;
  FSizeGrip.OnChange := SettingsChanged;
  // Create panels
  FPanels := TOBDTouchStatusbarPanelCollection.Create(Self);
  FPanels.OnChange := SettingsChanged;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alBottom;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchStatusbar.Destroy;
begin
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free size grip
  FSizegrip.Free;
  // Free panels
  FPanels.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// REPAINT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Repaint;
begin
  // Call inherited repaint
  inherited;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchStatusbar) then
  begin
    FBackground.Assign((Source as TOBDTouchStatusbar).Background);
    FBorder.Assign((Source as TOBDTouchStatusbar).Border);
    FSizeGrip.Assign((Source as TOBDTouchStatusbar).SizeGrip);
    FPanels.Assign((Source as TOBDTouchStatusbar).Panels);
  end;
end;

end.
