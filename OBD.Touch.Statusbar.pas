﻿//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Statusbar.pas
// CONTENTS       : Statusbar component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Statusbar;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes, Vcl.ExtCtrls,
  Vcl.Forms,

  OBD.LED, OBD.CustomControl.Common, OBD.CustomControl.Constants;

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
  TOBDTouchStatusbar = class(TCustomControl)
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
    ///   Buffer (This is the canvas we draw on)
    /// </summary>
    FBuffer: TBitmap;
    /// <summary>
    ///   Update rect (Invalidated rectangle)
    /// </summary>
    FUpdateRect: TRect;
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
    ///   WM_PAINT message handler
    /// </summary>
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    /// <summary>
    ///   WM_ERASEBKGND message handler
    /// </summary>
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
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
    ///   Buffer (This is the canvas we draw on)
    /// </summary>
    property Buffer: TBitmap read FBuffer;

    /// <summary>
    ///   Override CreateParams method
    /// </summary>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>
    ///   Override Paint method
    /// </summary>
    procedure Paint; override;
    /// <summary>
    ///   Override Resize method
    /// </summary>
    procedure Resize; override;
    /// <summary>
    ///   Override Loaded method
    /// </summary>
    procedure Loaded; override;
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
    ///   Paint buffer
    /// </summary>
    procedure PaintBuffer; virtual;
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
  Winapi.GDIPOBJ, Winapi.GDIPAPI, System.Math;

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
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  // Retrieve the invalidated rectangle
  if not GetUpdateRect(Handle, FUpdateRect, False) then
  // If no update region, default to the entire client area
  FUpdateRect := Rect(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------
// WM_ERASEBKGND MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
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
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Paint;
var
  X, Y, W, H: Integer;
begin
  // Call inherited Paint
  inherited;

  // Draw the buffer to the component canvas
  X := FUpdateRect.Left;
  Y := FUpdateRect.Top;
  W := FUpdateRect.Right - FUpdateRect.Left;
  H := FUpdateRect.Bottom - FUpdateRect.Top;

  if (W <> 0) and (H <> 0) then
    // Only update invalidated part
    BitBlt(Canvas.Handle, X, Y, W, H, FBuffer.Canvas.Handle, X,  Y, SRCCOPY)
  else
    // Repaint the whole buffer to the surface
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FBuffer.Canvas.Handle, X,  Y, SRCCOPY);
end;

//------------------------------------------------------------------------------
// RESIZE
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Resize;
begin
  // Call inherited Resize
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// LOADED
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.Loaded;
begin
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
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
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchStatusbar.PaintBuffer;
const
  PanelPadding = 4;
  LedSize = 14;
var
  Graphics: TGPGraphics;

  //----------------------------------------------------------------------------
  // MEASURE THE TEXT WIDTH
  //----------------------------------------------------------------------------
  function TextWidth(const Text: string; Font: TFont): Integer;
  var
    F: TGPFont;
    FF: TGPFontFamily;
    SF: TGPStringFormat;
    CR: TGPRectF;
    RR: TGPRectF;
  begin
    FF := TGPFontFamily.Create(Font.Name);
    F := TGPFont.Create(FF, Font.Size, OBD.CustomControl.Common.FontStyle(Font), UnitPoint);
    SF := TGPStringFormat.Create;
    SF.SetAlignment(StringAlignmentNear);
    SF.SetLineAlignment(StringAlignmentCenter);
    try
      CR := MakeRect(0.0, 0, 10000, 10000);
      if Graphics.MeasureString(Text, Length(Text), F, CR, SF, RR) = Ok then
        Result := Ceil(RR.Width)
      else
        Result := 0;
    finally
      F.Free;
      FF.Free;
      SF.Free;
    end;
  end;

  //----------------------------------------------------------------------------
  // MEASURE THE TEXT WIDTH
  //----------------------------------------------------------------------------
  procedure PaintLed(Panel: TOBDTouchStatusbarPanel);
  var
    SS: TCustomStyleServices;
    X, Y: Single;
    Graphics: TGPGraphics;
    BorderRect, LedRect: TGPRectF;
    Brush: TGPBrush;
    Pen: TGPPen;
  begin
    // Initialize GDI+ Graphics object
    Graphics := TGPGraphics.Create(Buffer.Canvas.Handle);
    try
      // Set smoothing mode to high-quality
      Graphics.SetSmoothingMode(SmoothingModeHighQuality);
      // Set compositing quality to high-quality
      Graphics.SetCompositingQuality(CompositingQualityHighQuality);

      X := Panel.PanelRect.Left;
      Y := (Border.Height + ((ClientRect.Height - Border.Height) / 2)) - (LedSize / 2);

      // Get the rectangle for the border
      BorderRect := MakeRect(X, Y, LedSize, LedSize);

      // Draw the background - we use a clWindow color for the background,
      // this shines a bit through to offset the color and the border.
      Brush := TGPSolidBrush.Create(SafeColorRefToARGB(clWindow));
      try
        Graphics.FillEllipse(Brush, BorderRect);
      finally
        // Free brush object
        Brush.Free;
      end;

      // Get the rectangle for the led color
      LedRect := MakeRect(X + Panel.LedBorder.Width + 1, Y + Panel.LedBorder.Width + 1, LedSize - (Panel.LedBorder.Width * 2) - 2,  LedSize - (Panel.LedBorder.Width * 2) - 2);

      // Create the led brush
      case Panel.LedState of
        lsGrayed : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(Panel.LedGrayedColor.FromColor), SafeColorRefToARGB(Panel.LedGrayedColor.ToColor), LinearGradientModeVertical);
        lsOff    : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(Panel.LedOffColor.FromColor), SafeColorRefToARGB(Panel.LedOffColor.ToColor), LinearGradientModeVertical);
        lsOn     : Brush := TGPLinearGradientBrush.Create(LedRect, SafeColorRefToARGB(Panel.LedOnColor.FromColor), SafeColorRefToARGB(Panel.LedOnColor.ToColor), LinearGradientModeVertical);
      end;
      // Create the led pen
      case Panel.LedState of
        lsGrayed : Pen := TGPPen.Create(SafeColorRefToARGB(Panel.LedGrayedColor.FromColor), 1.5);
        lsOff    : Pen := TGPPen.Create(SafeColorRefToARGB(Panel.LedOffColor.FromColor), 1.5);
        lsOn     : Pen := TGPPen.Create(SafeColorRefToARGB(Panel.LedOnColor.FromColor), 1.5);
      end;
      Pen.SetAlignment(PenAlignmentInset);
      // Draw the led
      try
        Graphics.FillEllipse(Brush, LedRect);
        Graphics.DrawEllipse(Pen, LedRect);
      finally
        Brush.Free;
        Pen.Free;
      end;

      // Draw the border
      if (Panel.LedBorder.FromColor <> clNone) and (Panel.LedBorder.ToColor <> clNone) and (Panel.LedBorder.Width > 0) then
      begin
        // Create the border brush
        Brush := TGPLinearGradientBrush.Create(BorderRect, SafeColorRefToARGB(Panel.LedBorder.FromColor), SafeColorRefToARGB(Panel.LedBorder.ToColor), LinearGradientModeVertical);
        // Create the border pen
        Pen := TGPPen.Create(Brush, Panel.LedBorder.Width);
        Pen.SetAlignment(PenAlignmentInset);
        try
          // Draw the gauge border
          Graphics.DrawEllipse(Pen, BorderRect);
        finally
          // Free the background brush object
          Brush.Free;
          // Free the background pen object
          Pen.Free;
        end;
      end;
    finally
      Graphics.Free;
    end;
  end;

var
  SS: TCustomStyleServices;
  BackgroundRect, BorderRect, SizeGripRect: TGPRectF;
  Brush: TGPBrush;
  SizeGripPath: TGPGraphicsPath;
  SizeGripWidth, I, W, S, PanelX: Integer;
  X, Y: Single;

  Font: TGPFont;
  FontBrush: TGPSolidBrush;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
  CaptionRect: TGPRectF;
begin
  // Update the size of the buffer
  Buffer.SetSize(Width, Height);

  // If VCL styles is available and enabled, then draw the VCL Style background
  // so it matches the active style background like on the Form or a Panel.
  if TStyleManager.IsCustomStyleActive then
  begin
    SS := StyleServices;
    // Draw the styled background
    SS.DrawElement(Buffer.Canvas.Handle, SS.GetElementDetails(twWindowRoot), Rect(0, 0, Width, Height));
  end else
  // Otherwise fill the background with the color.
  with Buffer.Canvas do
  begin
    // Use the component color
    Brush.Color := Self.Color;
    // Use a solid brush
    Brush.Style := bsSolid;
    // Fill the background with the component color
    FillRect(Rect(0, 0, Width, Height));
  end;

  // Initialize GDI+ Graphics object
  Graphics := TGPGraphics.Create(Buffer.Canvas.Handle);
  try
    // Set smoothing mode to high-quality
    Graphics.SetSmoothingMode(SmoothingModeHighQuality);
    // Set compositing quality to high-quality
    Graphics.SetCompositingQuality(CompositingQualityHighQuality);

    // Draw the backround
    if (Background.FromColor <> clNone) and (Background.ToColor <> clNone) then
    begin
      // Get the rectangle for the background
      BackgroundRect := MakeRect(0.0, 0, Width, Height);
      // Create the background brush
      Brush := TGPLinearGradientBrush.Create(BackgroundRect, SafeColorRefToARGB(Background.FromColor), SafeColorRefToARGB(Background.ToColor), LinearGradientModeVertical);
      try
        // Fill the background
        Graphics.FillRectangle(Brush, BackgroundRect);
      finally
        // Free the background brush object
        Brush.Free;
      end;
    end;

    // Draw the border
    if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
    begin
      // Get the rectangle for the border
      BorderRect := MakeRect(0.0, 0, Width, Border.Height);
      // Create the border brush
      Brush := TGPLinearGradientBrush.Create(BackgroundRect, SafeColorRefToARGB(Border.FromColor), SafeColorRefToARGB(Border.ToColor), LinearGradientModeVertical);
      try
        // Fill the border
        Graphics.FillRectangle(Brush, BorderRect);
      finally
        // Free the border brush object
        Brush.Free;
      end;
    end;

    // Set initial Panel X position
    PanelX := ClientRect.Left;

    // Calculate panel rects
    for I := 0 to Panels.Count -1 do
    begin
      W := 0;
      if Panels[I].AutoSize then
      begin
        // Include led width
        if Panels[I].ShowLed then W := W + LedSize + PanelPadding + 2;
        // Calculate simple panel text width
        if Panels[I].Style = psSimpleText then
          W := W + TextWidth(Panels[I].Text, Panels[I].Font)
        else
        begin
          // Calculate adanced panel primary text width
          W := W + TextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);
          // Calculate adanced panel secondary text width
          W := W + TextWidth(Panels[I].SecondaryText, Panels[I].SecondaryFont);
          // Add padding between primary and secondary text
          W := W + PanelPadding;
        end;
      end else
      begin
        // Include led width
        if Panels[I].ShowLed then W := W + LedSize;
        // Panel width
        W := W + Panels[I].Width;
      end;
      // Set panel rect
      Panels[I].PanelRect := TRect.Create(
        PanelX + PanelPadding,
        ClientRect.Top,
        PanelX + PanelPadding + W + PanelPadding,
        ClientRect.Bottom
      );
      // Inc Panel X position
      PanelX := PanelX + Panels[I].PanelRect.Width;
    end;

    // Draw the panels
    for I := 0 to Panels.Count -1 do
    begin
      if Panels[I].ShowLed then
      begin
        PaintLed(Panels[I]);
        X := LedSize + 2;
      end else X := 0;
      if Panels[I].Style = psSimpleText then
      begin
        FontFamily := TGPFontFamily.Create(Panels[I].Font.Name);
        Font := TGPFont.Create(FontFamily, Panels[I].Font.Size, OBD.CustomControl.Common.FontStyle(Panels[I].Font), UnitPoint);
        FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(Panels[I].Font.Color));
        StringFormat := TGPStringFormat.Create;
        StringFormat.SetAlignment(StringAlignmentNear);
        StringFormat.SetLineAlignment(StringAlignmentCenter);
        try
          CaptionRect := MakeRect(
            Panels[I].PanelRect.Left + X,
            Panels[I].PanelRect.Top + Border.Height,
            Panels[I].PanelRect.Width - X,
            Panels[I].PanelRect.Height
          );
          Graphics.DrawString(Panels[I].Text, Length(Panels[I].Text), Font, CaptionRect, StringFormat, FontBrush);
        finally
          FontFamily.Free;
          Font.Free;
          FontBrush.Free;
          StringFormat.Free;
        end;
      end else
      begin
        // Primary text
        FontFamily := TGPFontFamily.Create(Panels[I].PrimaryFont.Name);
        Font := TGPFont.Create(FontFamily, Panels[I].PrimaryFont.Size, OBD.CustomControl.Common.FontStyle(Panels[I].PrimaryFont), UnitPoint);
        FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(Panels[I].PrimaryFont.Color));
        StringFormat := TGPStringFormat.Create;
        StringFormat.SetAlignment(StringAlignmentNear);
        StringFormat.SetLineAlignment(StringAlignmentCenter);
        try
          CaptionRect := MakeRect(
            Panels[I].PanelRect.Left + X,
            Panels[I].PanelRect.Top + Border.Height,
            Panels[I].PanelRect.Width - X,
            Panels[I].PanelRect.Height
          );
          Graphics.DrawString(Panels[I].PrimaryText, Length(Panels[I].PrimaryText), Font, CaptionRect, StringFormat, FontBrush);
        finally
          FontFamily.Free;
          Font.Free;
          FontBrush.Free;
          StringFormat.Free;
        end;
        X := X + TextWidth(Panels[I].PrimaryText, Panels[I].PrimaryFont);
        // Secondary text
        FontFamily := TGPFontFamily.Create(Panels[I].SecondaryFont.Name);
        Font := TGPFont.Create(FontFamily, Panels[I].SecondaryFont.Size, OBD.CustomControl.Common.FontStyle(Panels[I].SecondaryFont), UnitPoint);
        FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(Panels[I].SecondaryFont.Color));
        StringFormat := TGPStringFormat.Create;
        StringFormat.SetAlignment(StringAlignmentNear);
        StringFormat.SetLineAlignment(StringAlignmentCenter);
        try
          CaptionRect := MakeRect(
            Panels[I].PanelRect.Left + X,
            Panels[I].PanelRect.Top + Border.Height,
            Panels[I].PanelRect.Width - X,
            Panels[I].PanelRect.Height
          );
          Graphics.DrawString(Panels[I].SecondaryText, Length(Panels[I].SecondaryText), Font, CaptionRect, StringFormat, FontBrush);
        finally
          FontFamily.Free;
          Font.Free;
          FontBrush.Free;
          StringFormat.Free;
        end;
      end;
    end;

    // Draw the Size Grip
    if SizeGrip.Visible then
    begin
      SizeGripRect := MakeRect(0.0, Border.Height, Width - 3, Height - Border.Height);
      SizeGripWidth := Ceil((SizeGrip.DotSize + SizeGrip.DotSpacing) * 3);
      FSizeGrip.SizeGripRect := Rect((Width - 3) - SizeGripWidth, Border.Height, Width - 3, Height);
      if SizeGrip.DotShape = dsSquare then
        SizeGripPath := CreateSizeGripPathSquare(SizeGripRect, SizeGrip.DotSize, SizeGrip.DotSpacing)
      else
        SizeGripPath := CreateSizeGripPathCircle(SizeGripRect, SizeGrip.DotSize, SizeGrip.DotSpacing);
      Brush := TGPSolidBrush.Create(SafeColorRefToARGB(SizeGrip.DotColor));
      try
        Graphics.FillPath(Brush, SizeGripPath);
      finally
        SizeGripPath.Free;
        Brush.Free;
      end;
    end;

  finally
    // Free GDI+ graphics object
    Graphics.Free;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchStatusbar.Create(AOwner: TComponent);
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Prevent background erasure for smoother rendering and reduced flickering.
  ControlStyle := ControlStyle + [csOpaque];
  // Create Buffer
  FBuffer := TBitmap.Create;
  // Set the buffer pixel format
  FBuffer.PixelFormat := pf32bit;
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
  // Free buffer
  FBuffer.Free;
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
  // Paint buffer
  PaintBuffer;
  // Invalidate
  Invalidate;
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
