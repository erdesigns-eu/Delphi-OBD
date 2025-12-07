//------------------------------------------------------------------------------
// UNIT           : OBD.Touch.Subheader.pas
// CONTENTS       : Subheader component with Skia rendering
// VERSION        : 2.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 02/04/2024
// UPDATED        : 06/12/2025 - Refactored to inherit from TOBDCustomControl
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Touch.Subheader;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, Vcl.ExtCtrls, Vcl.Forms, System.Skia, Vcl.Skia, System.Math,

  OBD.CustomControl, OBD.CustomControl.Helpers, OBD.CustomControl.Constants,
  OBD.Connection.Types, OBD.Connection, OBD.Connection.Component;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  /// <summary>
  ///   Default height
  /// </summary>
  DEFAULT_HEIGHT = 40;

  /// <summary>
  ///   Default background from color
  /// </summary>
  DEFAULT_BACKGROUND_FROM = clWhite;
  /// <summary>
  ///   Default background to color
  /// </summary>
  DEFAULT_BACKGROUND_TO = $00D7D7D7;

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

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Touch Subheader background properties
  /// </summary>
  TOBDTouchSubheaderBackground = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FOnChange: TNotifyEvent;

    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BACKGROUND_FROM;
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BACKGROUND_TO;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader border properties
  /// </summary>
  TOBDTouchSubheaderBorder = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FHeight: Integer;
    FOnChange: TNotifyEvent;

    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetHeight(Value: Integer);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TColor read FFromColor write SetFromColor default DEFAULT_BORDER_FROM;
    property ToColor: TColor read FToColor write SetToColor default DEFAULT_BORDER_TO;
    property Height: Integer read FHeight write SetHeight default DEFAULT_BORDER_HEIGHT;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader battery indicator properties
  /// </summary>
  TOBDTouchSubheaderBatteryIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FFormat: string;
    FVoltage: Single;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetFormat(const Value: string);
    procedure SetVoltage(Value: Single);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clLime;
    property ToColor: TColor read FToColor write SetToColor default clGreen;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Format: string read FFormat write SetFormat;
    property Voltage: Single read FVoltage write SetVoltage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader VCI indicator properties
  /// </summary>
  TOBDTouchSubheaderVciIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clSkyBlue;
    property ToColor: TColor read FToColor write SetToColor default clBlue;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader internet connection indicator properties
  /// </summary>
  TOBDTouchSubheaderInternetConnectionIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clYellow;
    property ToColor: TColor read FToColor write SetToColor default clOlive;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader protocol indicator properties
  /// </summary>
  TOBDTouchSubheaderProtocolIndicator = class(TPersistent)
  private
    FVisible: Boolean;
    FSize: Integer;
    FFromColor: TColor;
    FToColor: TColor;
    FBorderWidth: Single;
    FBorderColor: TColor;
    FFont: TFont;
    FCaption: string;
    FOnChange: TNotifyEvent;

    procedure SetVisible(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetBorderWidth(Value: Single);
    procedure SetBorderColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: Integer read FSize write SetSize default 24;
    property FromColor: TColor read FFromColor write SetFromColor default clAqua;
    property ToColor: TColor read FToColor write SetToColor default clTeal;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Font: TFont read FFont write SetFont;
    property Caption: string read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>
  ///   Touch Subheader Component
  /// </summary>
  TOBDTouchSubheader = class(TOBDCustomControl)
  private
    FBackground: TOBDTouchSubheaderBackground;
    FBorder: TOBDTouchSubheaderBorder;
    FBatteryIndicator: TOBDTouchSubheaderBatteryIndicator;
    FVciIndicator: TOBDTouchSubheaderVciIndicator;
    FInternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator;
    FProtocolIndicator: TOBDTouchSubheaderProtocolIndicator;
    FAutoApplyConnectionDetails: Boolean;
    FConnectionComponent: TOBDConnectionComponent;

    procedure SetBackground(Value: TOBDTouchSubheaderBackground);
    procedure SetBorder(Value: TOBDTouchSubheaderBorder);
    procedure SetBatteryIndicator(Value: TOBDTouchSubheaderBatteryIndicator);
    procedure SetVciIndicator(Value: TOBDTouchSubheaderVciIndicator);
    procedure SetInternetConnectionIndicator(Value: TOBDTouchSubheaderInternetConnectionIndicator);
    procedure SetProtocolIndicator(Value: TOBDTouchSubheaderProtocolIndicator);
    procedure SetAutoApplyConnectionDetails(Value: Boolean);
    procedure SetConnectionComponent(Value: TOBDConnectionComponent);
  protected
    procedure UpdateStyleElements; override;
    procedure SettingsChanged(Sender: TObject);
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TOBDTouchSubheaderBackground read FBackground write SetBackground;
    property Border: TOBDTouchSubheaderBorder read FBorder write SetBorder;
    property BatteryIndicator: TOBDTouchSubheaderBatteryIndicator read FBatteryIndicator write SetBatteryIndicator;
    property VciIndicator: TOBDTouchSubheaderVciIndicator read FVciIndicator write SetVciIndicator;
    property InternetConnectionIndicator: TOBDTouchSubheaderInternetConnectionIndicator read FInternetConnectionIndicator write SetInternetConnectionIndicator;
    property ProtocolIndicator: TOBDTouchSubheaderProtocolIndicator read FProtocolIndicator write SetProtocolIndicator;
    property AutoApplyConnectionDetails: Boolean read FAutoApplyConnectionDetails write SetAutoApplyConnectionDetails default True;
    property ConnectionComponent: TOBDConnectionComponent read FConnectionComponent write SetConnectionComponent;
    property Align;
    property Anchors;
    property Color;
  end;

implementation

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBackground - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBackground.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBackground.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBackground - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderBackground.Create;
begin
  inherited Create;
  FFromColor := DEFAULT_BACKGROUND_FROM;
  FToColor := DEFAULT_BACKGROUND_TO;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBackground - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBackground.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderBackground then
  begin
    FFromColor := (Source as TOBDTouchSubheaderBackground).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBackground).ToColor;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBorder - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBorder.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBorder.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBorder.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBorder - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderBorder.Create;
begin
  inherited Create;
  FFromColor := DEFAULT_BORDER_FROM;
  FToColor := DEFAULT_BORDER_TO;
  FHeight := DEFAULT_BORDER_HEIGHT;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBorder - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBorder.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderBorder then
  begin
    FFromColor := (Source as TOBDTouchSubheaderBorder).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBorder).ToColor;
    FHeight := (Source as TOBDTouchSubheaderBorder).Height;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBatteryIndicator - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBatteryIndicator.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetBorderWidth(Value: Single);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.SetVoltage(Value: Single);
begin
  if FVoltage <> Value then
  begin
    FVoltage := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderBatteryIndicator.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBatteryIndicator - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderBatteryIndicator.Create;
begin
  inherited Create;
  FVisible := True;
  FSize := 24;
  FFromColor := clLime;
  FToColor := clGreen;
  FBorderWidth := 1.0;
  FBorderColor := clBlack;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFormat := '%.1fV';
  FVoltage := 0.0;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBatteryIndicator - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheaderBatteryIndicator.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderBatteryIndicator - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderBatteryIndicator.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderBatteryIndicator then
  begin
    FVisible := (Source as TOBDTouchSubheaderBatteryIndicator).Visible;
    FSize := (Source as TOBDTouchSubheaderBatteryIndicator).Size;
    FFromColor := (Source as TOBDTouchSubheaderBatteryIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderBatteryIndicator).ToColor;
    FBorderWidth := (Source as TOBDTouchSubheaderBatteryIndicator).BorderWidth;
    FBorderColor := (Source as TOBDTouchSubheaderBatteryIndicator).BorderColor;
    FFont.Assign((Source as TOBDTouchSubheaderBatteryIndicator).Font);
    FFormat := (Source as TOBDTouchSubheaderBatteryIndicator).Format;
    FVoltage := (Source as TOBDTouchSubheaderBatteryIndicator).Voltage;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderVciIndicator - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderVciIndicator.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetBorderWidth(Value: Single);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TOBDTouchSubheaderVciIndicator.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderVciIndicator.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderVciIndicator - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderVciIndicator.Create;
begin
  inherited Create;
  FVisible := True;
  FSize := 24;
  FFromColor := clSkyBlue;
  FToColor := clBlue;
  FBorderWidth := 1.0;
  FBorderColor := clBlack;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FCaption := 'VCI';
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderVciIndicator - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheaderVciIndicator.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderVciIndicator - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderVciIndicator.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderVciIndicator then
  begin
    FVisible := (Source as TOBDTouchSubheaderVciIndicator).Visible;
    FSize := (Source as TOBDTouchSubheaderVciIndicator).Size;
    FFromColor := (Source as TOBDTouchSubheaderVciIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderVciIndicator).ToColor;
    FBorderWidth := (Source as TOBDTouchSubheaderVciIndicator).BorderWidth;
    FBorderColor := (Source as TOBDTouchSubheaderVciIndicator).BorderColor;
    FFont.Assign((Source as TOBDTouchSubheaderVciIndicator).Font);
    FCaption := (Source as TOBDTouchSubheaderVciIndicator).Caption;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderInternetConnectionIndicator - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderInternetConnectionIndicator.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetBorderWidth(Value: Single);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderInternetConnectionIndicator.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderInternetConnectionIndicator - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderInternetConnectionIndicator.Create;
begin
  inherited Create;
  FVisible := False;
  FSize := 24;
  FFromColor := clYellow;
  FToColor := clOlive;
  FBorderWidth := 1.0;
  FBorderColor := clBlack;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FCaption := 'NET';
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderInternetConnectionIndicator - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheaderInternetConnectionIndicator.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderInternetConnectionIndicator - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderInternetConnectionIndicator.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderInternetConnectionIndicator then
  begin
    FVisible := (Source as TOBDTouchSubheaderInternetConnectionIndicator).Visible;
    FSize := (Source as TOBDTouchSubheaderInternetConnectionIndicator).Size;
    FFromColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).ToColor;
    FBorderWidth := (Source as TOBDTouchSubheaderInternetConnectionIndicator).BorderWidth;
    FBorderColor := (Source as TOBDTouchSubheaderInternetConnectionIndicator).BorderColor;
    FFont.Assign((Source as TOBDTouchSubheaderInternetConnectionIndicator).Font);
    FCaption := (Source as TOBDTouchSubheaderInternetConnectionIndicator).Caption;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderProtocolIndicator - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderProtocolIndicator.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetSize(Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetBorderWidth(Value: Single);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TOBDTouchSubheaderProtocolIndicator.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TOBDTouchSubheaderProtocolIndicator.FontChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderProtocolIndicator - CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDTouchSubheaderProtocolIndicator.Create;
begin
  inherited Create;
  FVisible := True;
  FSize := 24;
  FFromColor := clAqua;
  FToColor := clTeal;
  FBorderWidth := 1.0;
  FBorderColor := clBlack;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FCaption := 'AUTO';
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderProtocolIndicator - DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheaderProtocolIndicator.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheaderProtocolIndicator - ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheaderProtocolIndicator.Assign(Source: TPersistent);
begin
  if Source is TOBDTouchSubheaderProtocolIndicator then
  begin
    FVisible := (Source as TOBDTouchSubheaderProtocolIndicator).Visible;
    FSize := (Source as TOBDTouchSubheaderProtocolIndicator).Size;
    FFromColor := (Source as TOBDTouchSubheaderProtocolIndicator).FromColor;
    FToColor := (Source as TOBDTouchSubheaderProtocolIndicator).ToColor;
    FBorderWidth := (Source as TOBDTouchSubheaderProtocolIndicator).BorderWidth;
    FBorderColor := (Source as TOBDTouchSubheaderProtocolIndicator).BorderColor;
    FFont.Assign((Source as TOBDTouchSubheaderProtocolIndicator).Font);
    FCaption := (Source as TOBDTouchSubheaderProtocolIndicator).Caption;
    if Assigned(FOnChange) then FOnChange(Self);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TOBDTouchSubheader - SETTERS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SetBackground(Value: TOBDTouchSubheaderBackground);
begin
  FBackground.Assign(Value);
end;

procedure TOBDTouchSubheader.SetBorder(Value: TOBDTouchSubheaderBorder);
begin
  FBorder.Assign(Value);
end;

procedure TOBDTouchSubheader.SetBatteryIndicator(Value: TOBDTouchSubheaderBatteryIndicator);
begin
  FBatteryIndicator.Assign(Value);
end;

procedure TOBDTouchSubheader.SetVciIndicator(Value: TOBDTouchSubheaderVciIndicator);
begin
  FVciIndicator.Assign(Value);
end;

procedure TOBDTouchSubheader.SetInternetConnectionIndicator(Value: TOBDTouchSubheaderInternetConnectionIndicator);
begin
  FInternetConnectionIndicator.Assign(Value);
end;

procedure TOBDTouchSubheader.SetProtocolIndicator(Value: TOBDTouchSubheaderProtocolIndicator);
begin
  FProtocolIndicator.Assign(Value);
end;

procedure TOBDTouchSubheader.SetAutoApplyConnectionDetails(Value: Boolean);
begin
  if FAutoApplyConnectionDetails <> Value then
  begin
    FAutoApplyConnectionDetails := Value;
    Invalidate;
  end;
end;

procedure TOBDTouchSubheader.SetConnectionComponent(Value: TOBDConnectionComponent);
begin
  if FConnectionComponent <> Value then
  begin
    // Remove old event handler
    if Assigned(FConnectionComponent) then
      FConnectionComponent.OnConnectionStateChanged := nil;
    
    FConnectionComponent := Value;
    
    // Add new event handler
    if Assigned(FConnectionComponent) and FAutoApplyConnectionDetails then
      FConnectionComponent.OnConnectionStateChanged := HandleConnectionStateChanged;
    
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// HANDLE CONNECTION STATE CHANGED
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.HandleConnectionStateChanged(Sender: TObject; const Connected: Boolean; const ConnectionType: TOBDConnectionType);
begin
  // Update indicators based on connection state
  if FAutoApplyConnectionDetails then
  begin
    // Update VCI indicator
    FVciIndicator.Visible := Connected;
    
    // Update protocol indicator based on connection type
    case ConnectionType of
      ctSerial: FProtocolIndicator.Caption := 'SERIAL';
      ctBluetooth: FProtocolIndicator.Caption := 'BT';
      ctWiFi: FProtocolIndicator.Caption := 'WIFI';
      ctFTDI: FProtocolIndicator.Caption := 'FTDI';
    else
      FProtocolIndicator.Caption := 'AUTO';
    end;
    
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// UPDATE STYLE ELEMENTS
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.UpdateStyleElements;
begin
  // Call inherited Loaded
  inherited;
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// SETTINGS CHANGED HANDLER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.SettingsChanged(Sender: TObject);
begin
  // Trigger repaint
  Invalidate;
end;

//------------------------------------------------------------------------------
// PAINT BUFFER
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.PaintSkia(Canvas: ISkCanvas);
var
  Paint: ISkPaint;
  BackgroundRect, BorderRect: TRectF;
  MeasureRect: TRect;
  BatteryRect, VciRect, InternetRect, ProtocolRect: TRectF;
  BatteryCaptionRect, VciCaptionRect, InternetCaptionRect, ProtocolCaptionRect: TRect;
  BatteryLabelText, VciLabelText, InternetLabelText, ProtocolLabelText: string;
  HasBattery, HasVci, HasInternet, HasProtocol: Boolean;
  X, Y, Z: Single;
  TextSize: TSizeF;
begin
  try
    // Clear canvas with background color
    Canvas.Clear(ResolveStyledBackgroundColor(Self.Color));

  // Draw the backround gradient when configured
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

  // Draw the border stripe when enabled
  if (Border.FromColor <> clNone) and (Border.ToColor <> clNone) then
  begin
    BorderRect := TRectF.Create(0.0, Height - Border.Height + 0.0, Width + 0.0, Height + 0.0);
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

  // Prepare layout state
  X := Width - 8;
  HasBattery := False;
  HasVci := False;
  HasInternet := False;
  HasProtocol := False;

  // Draw the battery indicator
  if BatteryIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (BatteryIndicator.Size / 2);
    BatteryLabelText := Format(BatteryIndicator.Format, [BatteryIndicator.Voltage]);

    TextSize := MeasureSkText(BatteryLabelText, BatteryIndicator.Font);
    MeasureRect := Rect(0, 0, Width - 8, Height - Border.Height);
    BatteryCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 4);

    BatteryRect := TRectF.Create(X - BatteryIndicator.Size, Y, X, Y + BatteryIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(BatteryRect.Left, BatteryRect.Top),
      TPointF.Create(BatteryRect.Left, BatteryRect.Bottom),
      [SafeColorRefToSkColor(BatteryIndicator.FromColor), SafeColorRefToSkColor(BatteryIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := BatteryIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(BatteryIndicator.BorderColor);
    Canvas.DrawPath(CreateVehicleBatteryPath(BatteryRect), Paint);

    X := X - (BatteryIndicator.Size + 4);
    HasBattery := True;
  end;

  // Draw the VCI indicator
  if VciIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (VciIndicator.Size / 2);
    VciLabelText := VciIndicator.Caption;

    TextSize := MeasureSkText(VciLabelText, VciIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    VciCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - (TextSize.cx + 2);

    VciRect := TRectF.Create(X - VciIndicator.Size, Y, X, Y + VciIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(VciRect.Left, VciRect.Top),
      TPointF.Create(VciRect.Left, VciRect.Bottom),
      [SafeColorRefToSkColor(VciIndicator.FromColor), SafeColorRefToSkColor(VciIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := VciIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(VciIndicator.BorderColor);
    Canvas.DrawPath(CreateJ1962Path(VciRect), Paint);

    X := X - (VciIndicator.Size + 4);
    HasVci := True;
  end;

  // Draw the internet connection indicator
  if InternetConnectionIndicator.Visible then
  begin
    Y := ((Height - Border.Height) / 2) - (InternetConnectionIndicator.Size / 2);
    InternetLabelText := InternetConnectionIndicator.Caption;

    TextSize := MeasureSkText(InternetLabelText, InternetConnectionIndicator.Font);
    MeasureRect := Rect(0, 0, Round(X), Height - Border.Height);
    InternetCaptionRect := Rect(
      MeasureRect.Right - Ceil(TextSize.cx),
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Right,
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    X := X - TextSize.cx;

    InternetRect := TRectF.Create(X - InternetConnectionIndicator.Size, Y, X, Y + InternetConnectionIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(InternetRect.Left, InternetRect.Top),
      TPointF.Create(InternetRect.Left, InternetRect.Bottom),
      [SafeColorRefToSkColor(InternetConnectionIndicator.FromColor), SafeColorRefToSkColor(InternetConnectionIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawOval(InternetRect, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := InternetConnectionIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(InternetConnectionIndicator.BorderColor);
    Canvas.DrawPath(CreateInternetGlobePath(InternetRect), Paint);

    X := X - (InternetConnectionIndicator.Size + 4);
    HasInternet := True;
  end;

  // Draw the protocol indicator
  if ProtocolIndicator.Visible then
  begin
    Z := X;
    Y := ((Height - Border.Height) / 2) - (ProtocolIndicator.Size / 2);

    ProtocolRect := TRectF.Create(8, Y, 8 + ProtocolIndicator.Size, Y + ProtocolIndicator.Size);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Shader := TSkShader.MakeGradientLinear(
      TPointF.Create(ProtocolRect.Left, ProtocolRect.Top),
      TPointF.Create(ProtocolRect.Left, ProtocolRect.Bottom),
      [SafeColorRefToSkColor(ProtocolIndicator.FromColor), SafeColorRefToSkColor(ProtocolIndicator.ToColor)],
      nil,
      TSkTileMode.Clamp);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeWidth := ProtocolIndicator.BorderWidth;
    Paint.Color := SafeColorRefToSkColor(ProtocolIndicator.BorderColor);
    Canvas.DrawPath(CreateProtocolPath(ProtocolRect), Paint);

    ProtocolLabelText := FitTextToWidth(ProtocolIndicator.Caption, ProtocolIndicator.Font, Round(Z - (12 + ProtocolIndicator.Size)));
    TextSize := MeasureSkText(ProtocolLabelText, ProtocolIndicator.Font);
    MeasureRect := Rect(12 + ProtocolIndicator.Size, 0, Round(Z - (12 + ProtocolIndicator.Size)), Height - Border.Height);
    ProtocolCaptionRect := Rect(
      MeasureRect.Left,
      ((Height - Border.Height) div 2) - Ceil(TextSize.cy / 2),
      MeasureRect.Left + Min(Ceil(TextSize.cx), MeasureRect.Right - MeasureRect.Left),
      ((Height - Border.Height) div 2) + Ceil(TextSize.cy / 2));
    HasProtocol := True;
  end;

  // Render captions with Skia to keep text drawing inside the Skia pipeline
  if HasBattery then
  begin
    DrawSkTextCentered(Canvas, BatteryLabelText, BatteryIndicator.Font, TRectF.Create(BatteryCaptionRect), BatteryIndicator.Font.Color, TSkTextAlign.Right);
    DrawSkTextCentered(Canvas, '- +', BatteryIndicator.Font, BatteryRect, BatteryIndicator.Font.Color);
  end;

  if HasVci then
    DrawSkTextCentered(Canvas, VciLabelText, VciIndicator.Font, TRectF.Create(VciCaptionRect), VciIndicator.Font.Color, TSkTextAlign.Right);

  if HasInternet then
    DrawSkTextCentered(Canvas, InternetLabelText, InternetConnectionIndicator.Font, TRectF.Create(InternetCaptionRect), InternetConnectionIndicator.Font.Color, TSkTextAlign.Right);

  if HasProtocol then
    DrawSkTextCentered(Canvas, ProtocolLabelText, ProtocolIndicator.Font, TRectF.Create(ProtocolCaptionRect), ProtocolIndicator.Font.Color, TSkTextAlign.Left);

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
constructor TOBDTouchSubheader.Create(AOwner: TComponent);
begin
  // Call inherited constructor (handles rendering setup)
  inherited Create(AOwner);
  // Create background
  FBackground := TOBDTouchSubheaderBackground.Create;
  FBackground.OnChange := SettingsChanged;
  // Create border
  FBorder := TOBDTouchSubheaderBorder.Create;
  FBorder.OnChange := SettingsChanged;
  // Create battery indicator
  FBatteryIndicator := TOBDTouchSubheaderBatteryIndicator.Create;
  FBatteryIndicator.OnChange := SettingsChanged;
  // Create vci indicator
  FVciIndicator := TOBDTouchSubheaderVciIndicator.Create;
  FVciIndicator.OnChange := SettingsChanged;
  // Create internet connection indicator
  FInternetConnectionIndicator := TOBDTouchSubheaderInternetConnectionIndicator.Create;
  FInternetConnectionIndicator.OnChange := SettingsChanged;
  // Create protocol indicator
  FProtocolIndicator := TOBDTouchSubheaderProtocolIndicator.Create;
  FProtocolIndicator.OnChange := SettingsChanged;
  // Enable automatic connection application by default
  FAutoApplyConnectionDetails := True;
  // Set defaults
  Height := DEFAULT_HEIGHT;
  Align := alTop;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDTouchSubheader.Destroy;
begin
  // Free background
  FBackground.Free;
  // Free border
  FBorder.Free;
  // Free battery indicator
  FBatteryIndicator.Free;
  // Free vci indicator
  FVciIndicator.Free;
  // Free internet connection indicator
  FInternetConnectionIndicator.Free;
  // Free protocol indicator
  FProtocolIndicator.Free;
  // Clear connection component reference
  if Assigned(FConnectionComponent) then
    FConnectionComponent.OnConnectionStateChanged := nil;
  // Call inherited destructor
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// ASSIGN
//------------------------------------------------------------------------------
procedure TOBDTouchSubheader.Assign(Source: TPersistent);
begin
  // Call inherited assign
  inherited;
  // Assign custom properties
  if (Source is TOBDTouchSubheader) then
  begin
    FBackground.Assign((Source as TOBDTouchSubheader).Background);
    FBorder.Assign((Source as TOBDTouchSubheader).Border);
    FBatteryIndicator.Assign((Source as TOBDTouchSubheader).BatteryIndicator);
    FVciIndicator.Assign((Source as TOBDTouchSubheader).VciIndicator);
    FInternetConnectionIndicator.Assign((Source as TOBDTouchSubheader).InternetConnectionIndicator);
    FProtocolIndicator.Assign((Source as TOBDTouchSubheader).ProtocolIndicator);
  end;
end;

end.
