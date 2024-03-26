//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Header.pas
// CONTENTS       : LED component
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 25/03/2024
//------------------------------------------------------------------------------
unit OBD.Touch.Header;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, Vcl.ExtCtrls,

  OBD.CustomControl.Common;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 50;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = $00FBF5F7;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00BBAEAC;

  /// <summary>
  ///   Default border from color
  /// </summary>
  DEFAULT_BORDER_FROM = $00918888;
  /// <summary>
  ///   Default border to color
  /// </summary>
  DEFAULT_BORDER_TO = $00776F6F;
  /// <summary>
  ///   Default border height
  /// </summary>
  DEFAULT_BORDER_HEIGHT = 2;

  /// <summary>
  ///   Default back button caption
  /// </summary>
  DEFAULT_BACK_BUTTON_CAPTION = 'Back';
  /// <summary>
  ///   Default back button width
  /// </summary>
  DEFAULT_BACK_BUTTON_WIDTH = 60;

  /// <summary>
  ///   Default caption text
  /// </summary>
  DEFAULT_CAPTION_TEXT = '';
  /// <summary>
  ///   Default caption font size
  /// </summary>
  DEFAULT_CAPTION_FONT_SIZE = 12;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Header background properties
  /// </summary>
  TOBDTouchHeaderBackground = class(TPersistent)
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
  ///   Touch Header border properties
  /// </summary>
  TOBDTouchHeaderBorder = class(TPersistent)
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
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;
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
  ///   Touch Header button color
  /// </summary>
  TOBDTouchHeaderButtonColor = class(TPersistent)
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
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    /// <summary>
    ///   Gradient to color
    /// </summary>
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header button
  /// </summary>
  TOBDTouchHeaderButton = class(TPersistent)
  private
    /// <summary>
    ///   Button Rect
    /// </summary>
    FButtonRect: TRect;
  private
    /// <summary>
    ///   Normal button color
    /// </summary>
    FNormalColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Hot button color
    /// </summary>
    FHotColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Pressed button color
    /// </summary>
    FPressedColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Disabled button color
    /// </summary>
    FDisabledColor: TOBDTouchHeaderButtonColor;
    /// <summary>
    ///   Enabled
    /// </summary>
    FEnabled: Boolean;
    /// <summary>
    ///   Visible
    /// </summary>
    FVisible: Boolean;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;
    /// <summary>
    ///   Width
    /// </summary>
    FWidth: Integer;
    /// <summary>
    ///   Border color
    /// </summary>
    FBorderColor: TColor;
    /// <summary>
    ///   Border width
    /// </summary>
    FBorderWidth: Single;

    /// <summary>
    ///   Set normal button color
    /// </summary>
    procedure SetNormalColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set hot button color
    /// </summary>
    procedure SetHotColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set pressed button color
    /// </summary>
    procedure SetPressedColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set disabled button color
    /// </summary>
    procedure SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
    /// <summary>
    ///   Set enabled
    /// </summary>
    procedure SetEnabled(Value: Boolean);
    /// <summary>
    ///   Set visible
    /// </summary>
    procedure SetVisible(Value: Boolean);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
    /// <summary>
    ///   Set width
    /// </summary>
    procedure SetWidth(Value: Integer);
    /// <summary>
    ///   Set border color
    /// </summary>
    procedure SetBorderColor(Value: TColor);
    /// <summary>
    ///   Set border width
    /// </summary>
    procedure SetBorderWidth(Value: Single);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
    /// <summary>
    ///   Button Rect
    /// </summary>
    property ButtonRect: TRect read FButtonRect write FButtonRect;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Normal button color
    /// </summary>
    property NormalColor: TOBDTouchHeaderButtonColor read FNormalColor write SetNormalColor;
    /// <summary>
    ///   Hot button color
    /// </summary>
    property HotColor: TOBDTouchHeaderButtonColor read FHotColor write SetHotColor;
    /// <summary>
    ///   Pressed button color
    /// </summary>
    property PressedColor: TOBDTouchHeaderButtonColor read FPressedColor write SetPressedColor;
    /// <summary>
    ///   Disabled button color
    /// </summary>
    property DisabledColor: TOBDTouchHeaderButtonColor read FDisabledColor write SetDisabledColor;
    /// <summary>
    ///   Enabled
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// <summary>
    ///   Visible
    /// </summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;
    /// <summary>
    ///   Width
    /// </summary>
    property Width: Integer read FWidth write SetWidth default DEFAULT_BACK_BUTTON_WIDTH;
    /// <summary>
    ///   Border color
    /// </summary>
    property BorderColor: TColor read FBorderColor write SetBorderColor default DEFAULT_BORDER_TO;
    /// <summary>
    ///   Border width
    /// </summary>
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header button
  /// </summary>
  TOBDTouchHeaderCaption = class(TPersistent)
  private
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TCaption;
    /// <summary>
    ///   Font
    /// </summary>
    FFont: TFont;

    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TCaption);
    /// <summary>
    ///   Set font
    /// </summary>
    procedure SetFont(Value: TFont);
  private
    /// <summary>
    ///   On change event
    /// </summary>
    FOnChange: TNotifyEvent;
  protected
    /// <summary>
    ///   Settings changed hanlder
    /// </summary>
    procedure SettingsChanged(Sender: TObject);
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create; virtual;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>
    ///   Font
    /// </summary>
    property Font: TFont read FFont write SetFont;

    /// <summary>
    ///   On change event
    /// </summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Header Component
  /// </summary>
  TOBDTouchHeader = class(TCustomControl)
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
    FBackground: TOBDTouchHeaderBackground;
    /// <summary>
    ///   Border
    /// </summary>
    FBorder: TOBDTouchHeaderBorder;
    /// <summary>
    ///   Back button
    /// </summary>
    FBackButton: TOBDTouchHeaderButton;
    /// <summary>
    ///   Caption
    /// </summary>
    FCaption: TOBDTouchHeaderCaption;

    /// <summary>
    ///   Set background
    /// </summary>
    procedure SetBackground(Value: TOBDTouchHeaderBackground);
    /// <summary>
    ///   Set border
    /// </summary>
    procedure SetBorder(Value: TOBDTouchHeaderBorder);
    /// <summary>
    ///   Set back button
    /// </summary>
    procedure SetBackButton(Value: TOBDTouchHeaderButton);
    /// <summary>
    ///   Set caption
    /// </summary>
    procedure SetCaption(Value: TOBDTouchHeaderCaption);
  private
    /// <summary>
    ///   WM_PAINT message handler
    /// </summary>
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    /// <summary>
    ///   WM_ERASEBKGND message handler
    /// </summary>
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
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
    ///   Override assign method
    /// </summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>
    ///   Background
    /// </summary>
    property Background: TOBDTouchHeaderBackground read FBackground write SetBackground;
    /// <summary>
    ///   Border
    /// </summary>
    property Border: TOBDTouchHeaderBorder read FBorder write SetBorder;
    /// <summary>
    ///   Back button
    /// </summary>
    property BackButton: TOBDTouchHeaderButton read FBackButton write SetBackButton;
    /// <summary>
    ///   Caption
    /// </summary>
    property Caption: TOBDTouchHeaderCaption read FCaption write SetCaption;
  published
    /// <summary>
    ///   Component alignment (inherited)
    /// </summary>
    property Align default alTop;
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
procedure TOBDTouchHeaderBackground.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderBackground.SetToColor(Value: TColor);
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
constructor TOBDTouchHeaderBackground.Create;
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
procedure TOBDTouchHeaderBackground.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBackground) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBackground).FromColor;
    FToColor := (Source as TOBDTouchHeaderBackground).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetToColor(Value: TColor);
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
procedure TOBDTouchHeaderBorder.SetHeight(Value: Integer);
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
constructor TOBDTouchHeaderBorder.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Set defaults
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FHeight := DEFAULT_BORDER_HEIGHT;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderBorder.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderBorder) then
  begin
    FFromColor := (Source as TOBDTouchHeaderBorder).FromColor;
    FToColor := (Source as TOBDTouchHeaderBorder).ToColor;
    FHeight := (Source as TOBDTouchHeaderBorder).Height;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET FROM COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.SetFromColor(Value: TColor);
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
procedure TOBDTouchHeaderButtonColor.SetToColor(Value: TColor);
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
constructor TOBDTouchHeaderButtonColor.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButtonColor.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButtonColor) then
  begin
    FFromColor := (Source as TOBDTouchHeaderButtonColor).FromColor;
    FToColor := (Source as TOBDTouchHeaderButtonColor).ToColor;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET NORMAL COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetNormalColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assig nnormal color
  FNormalColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET HOT COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetHotColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign hot color
  FHotColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET PRESSED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetPressedColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign pressed color
  FPressedColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET DISABLED COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetDisabledColor(Value: TOBDTouchHeaderButtonColor);
begin
  // Assign disabled color
  FDisabledColor.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET ENABLED
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetEnabled(Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    // Set enabled status
    FEnabled := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET VISIBLE
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    // Set visible
    FVisible := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    // Set new width
    FWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetBorderColor(Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    // Set new border color
    FBorderColor := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER WIDTH
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SetBorderWidth(Value: Single);
begin
  if (FBorderWidth <> Value) and (Value >= 1) then
  begin
    // Set new border width
    FBorderWidth := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderButton.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create Normal button color
  FNormalColor := TOBDTouchHeaderButtonColor.Create;
  FNormalColor.OnChange := SettingsChanged;
  FNormalColor.FromColor := $00EFEFEE;
  FNormalColor.ToColor := $00ACA2A1;
  // Create Hot button color
  FHotColor := TOBDTouchHeaderButtonColor.Create;
  FHotColor.OnChange := SettingsChanged;
  FHotColor.FromColor := $00F0F0F0;
  FHotColor.ToColor := $00CCC7C6;
  // Create Pressed button color
  FPressedColor := TOBDTouchHeaderButtonColor.Create;
  FPressedColor.OnChange := SettingsChanged;
  FPressedColor.FromColor := $00EFEFEE;
  FPressedColor.ToColor := $00ACA2A1;
  // Create Disabled button color
  FDisabledColor := TOBDTouchHeaderButtonColor.Create;
  FDisabledColor.OnChange := SettingsChanged;
  FDisabledColor.FromColor := $00CCC7C6;
  FDisabledColor.ToColor := $00A89F9D;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  // Set defaults
  FEnabled := True;
  FVisible := True;
  FWidth   := DEFAULT_BACK_BUTTON_WIDTH;
  FCaption := DEFAULT_BACK_BUTTON_CAPTION;
  FBorderColor := DEFAULT_BORDER_TO;
  FBorderWidth := 1;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderButton.Destroy;
begin
  // Free normal button color
  FNormalColor.Free;
  // Free hot button color
  FHotColor.Free;
  // Free pressed button color
  FPressedColor.Free;
  // Free disabled button color
  FDisabledColor.Free;
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderButton.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderButton) then
  begin
    FNormalColor.Assign((Source as TOBDTouchHeaderButton).NormalColor);
    FHotColor.Assign((Source as TOBDTouchHeaderButton).HotColor);
    FPressedColor.Assign((Source as TOBDTouchHeaderButton).PressedColor);
    FDisabledColor.Assign((Source as TOBDTouchHeaderButton).DisabledColor);
    FEnabled := (Source as TOBDTouchHeaderButton).Enabled;
    FVisible := (Source as TOBDTouchHeaderButton).Visible;
    FCaption := (Source as TOBDTouchHeaderButton).Caption;
    FFont.Assign((Source as TOBDTouchHeaderButton).Font);
    FWidth := (Source as TOBDTouchHeaderButton).Width;
    FBorderColor := (Source as TOBDTouchHeaderButton).BorderColor;
    FBorderWidth := (Source as TOBDTouchHeaderButton).BorderWidth;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SetCaption(Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    // Set new caption
    FCaption := Value;
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// SET FONT
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.SettingsChanged(Sender: TObject);
begin
  // Notify change
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchHeaderCaption.Create;
begin
  // Call inherited constructor
  inherited Create;
  // Create font
  FFont := TFont.Create;
  FFont.OnChange := SettingsChanged;
  FFont.Size := DEFAULT_CAPTION_FONT_SIZE;
  FCaption := DEFAULT_CAPTION_TEXT;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeaderCaption.Destroy;
begin
  // Free font
  FFont.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeaderCaption.Assign(Source: TPersistent);
begin
  // Assign properties
  if (Source is TOBDTouchHeaderCaption) then
  begin
    FCaption := (Source as TOBDTouchHeaderCaption).Caption;
    FFont.Assign((Source as TOBDTouchHeaderCaption).Font);
    // Notify change
    if Assigned(OnChange) then OnChange(Self);
  end else
    // Call inherited assign
    inherited;
end;

//------------------------------------------------------------------------------
// CLASS CONSTRUCTOR
//------------------------------------------------------------------------------
class constructor TOBDTouchHeader.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// CLASS DESTRUCTOR
//------------------------------------------------------------------------------
class destructor TOBDTouchHeader.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TOBDTouchHeader, TPanelStyleHook);
end;

//------------------------------------------------------------------------------
// SET BACKGROUND
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackground(Value: TOBDTouchHeaderBackground);
begin
  FBackground.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BORDER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBorder(Value: TOBDTouchHeaderBorder);
begin
  FBorder.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET BACK BUTTON
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetBackButton(Value: TOBDTouchHeaderButton);
begin
  FBackButton.Assign(Value);
end;

//------------------------------------------------------------------------------
// SET CAPTION
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SetCaption(Value: TOBDTouchHeaderCaption);
begin
  FCaption.Assign(Value);
end;

//------------------------------------------------------------------------------
// WM_PAINT MESSAGE HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.WMPaint(var Msg: TWMPaint);
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
procedure TOBDTouchHeader.WMEraseBkGnd(var Msg: TWMEraseBkgnd);
begin
  // Set the handled flag
  Msg.Result := 1;
end;

//------------------------------------------------------------------------------
// CREATE PARAMS
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Adjust window style to avoid unnecessary redraws on size changes,
  // optimizing performance for custom drawing.
  with Params do Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

//------------------------------------------------------------------------------
// PAINT
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Paint;
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
procedure TOBDTouchHeader.Resize;
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
procedure TOBDTouchHeader.Loaded;
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
procedure TOBDTouchHeader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.SettingsChanged(Sender: TObject);
begin
  // Paint buffer
  PaintBuffer;
  // Invalidate buffer
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.PaintBuffer;

  function FontSize(Font: TFont; Text: string): TSize;
  begin
    FBuffer.Canvas.Font.Assign(Font);
    Result.cx := FBuffer.Canvas.TextWidth(Text);
    Result.cy := FBuffer.Canvas.TextHeight(Text);
  end;

var
  SS: TCustomStyleServices;
  Graphics: TGPGraphics;
  BackgroundRect, BorderRect: TGPRect;
  BackButtonRect, CaptionRect: TGPRectF;
  Brush: TGPBrush;
  Pen: TGPPen;
  BackButtonPath: TGPGraphicsPath;
  X, Y, W, H: Single;

  Font: TGPFont;
  FontBrush: TGPSolidBrush;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
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
      BackgroundRect := MakeRect(0, 0, Width, Height);
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
      BorderRect := MakeRect(0, Height - Border.Height, Width, Border.Height);
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

    // Draw the back button
    if BackButton.Visible then
    begin
      // Offset from the left
      X := 8;
      // Height of the button
      H := Height - 16;
      // Vertical position
      Y := ((Height / 2) - (H / 2)) - (Border.Height / 2);
      // Create font objects
      FontFamily := TGPFontFamily.Create(BackButton.Font.Name);
      Font := TGPFont.Create(FontFamily, BackButton.Font.Size, OBD.CustomControl.Common.FontStyle(BackButton.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(BackButton.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      // Calculate the button rect and caption rect
      BackButtonRect := MakeRect(X, Y, BackButton.Width, H);
      BackButton.ButtonRect := TRect.Create(Round(X), Round(Y), Round(X) + BackButton.Width, Round(Y + H));
      CaptionRect := MakeRect(X + 6, Y, BackButton.Width - 6, H);
      // Create the button path
      BackButtonPath := CreateBackButtonPath(BackButtonRect, 4);
      // Create the brush and pen
      if not BackButton.Enabled then
      begin
        Brush := TGPLinearGradientBrush.Create(BackButtonRect, SafeColorRefToARGB(BackButton.DisabledColor.FromColor), SafeColorRefToARGB(BackButton.DisabledColor.ToColor), LinearGradientModeVertical);
      end else
      begin
        Brush := TGPLinearGradientBrush.Create(BackButtonRect, SafeColorRefToARGB(BackButton.NormalColor.FromColor), SafeColorRefToARGB(BackButton.NormalColor.ToColor), LinearGradientModeVertical);
      end;
      Pen := TGPPen.Create(SafeColorRefToARGB(BackButton.BorderColor), BackButton.BorderWidth);
      try
        // Draw the button
        Graphics.FillPath(Brush, BackButtonPath);
        // Draw the border
        if BackButton.BorderColor <> clNone then Graphics.DrawPath(Pen, BackButtonPath);
        // Draw the caption
        Graphics.DrawString(BackButton.Caption, Length(BackButton.Caption), Font, CaptionRect, StringFormat, FontBrush);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
        Brush.Free;
        Pen.Free;
      end;
    end;

    // Draw the caption
    if (Caption.Caption <> '') then
    begin
      // Create font objects
      FontFamily := TGPFontFamily.Create(Caption.Font.Name);
      Font := TGPFont.Create(FontFamily, Caption.Font.Size, OBD.CustomControl.Common.FontStyle(Caption.Font), UnitPoint);
      FontBrush := TGPSolidBrush.Create(SafeColorRefToARGB(Caption.Font.Color));
      StringFormat := TGPStringFormat.Create;
      StringFormat.SetAlignment(StringAlignmentCenter);
      StringFormat.SetLineAlignment(StringAlignmentCenter);
      CaptionRect := MakeRect(0.0, 0.0, Width, Height);
      try
        // Draw the caption
        Graphics.DrawString(Caption.Caption, Length(Caption.Caption), Font, CaptionRect, StringFormat, FontBrush);
      finally
        FontFamily.Free;
        Font.Free;
        FontBrush.Free;
        StringFormat.Free;
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
constructor TOBDTouchHeader.Create(AOwner: TComponent);
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
  FBackground := TOBDTouchHeaderBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchHeaderBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create back button
  FBackButton := TOBDTouchHeaderButton.Create;
  FBackButton.OnChange := SettingsChanged;
  // Create caption
  FCaption := TOBDTouchHeaderCaption.Create;
  FCaption.OnChange := SettingsChanged;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchHeader.Destroy;
begin
  // Free buffer
  FBuffer.Free;
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free back button
  FBackButton.Free;
  // Free caption
  FCaption.Free;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchHeader.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchHeader) then
  begin
    FBackground.Assign((Source as TOBDTouchHeader).Background);
    FBorder.Assign((Source as TOBDTouchHeader).Border);
    FBackButton.Assign((Source as TOBDTouchHeader).BackButton);
    FCaption.Assign((Source as TOBDTouchHeader).Caption);
  end;
end;

end.
