//------------------------------------------------------------------------------
// UNIT           : OBD.SegmentedSwitch.pas
// CONTENTS       : Touch-style multi-state segmented toggle
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : iOS-style segmented control. Click a segment to make it
//                  active. Suitable for "AT command set | OBD command set"
//                  toggles, mode pickers and small enumerated state UIs.
//------------------------------------------------------------------------------
unit OBD.SegmentedSwitch;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  Vcl.Controls, Vcl.Graphics, WinApi.Windows, Winapi.Messages,
  System.Skia, Vcl.Skia,

  OBD.CustomControl, OBD.CustomControl.Helpers,
  OBD.Render.SegmentedSwitch;

const
  SS_DEFAULT_BACKGROUND   = $00282828;
  SS_DEFAULT_BORDER       = $00404040;
  SS_DEFAULT_ACTIVE_BG    = TColor($001F8FE6);
  SS_DEFAULT_ACTIVE_TEXT  = clWhite;
  SS_DEFAULT_INACTIVE_TEXT = clWhite;
  SS_DEFAULT_RADIUS       = 6;

type
  TOBDSegmentedChangeEvent = procedure(Sender: TObject; const Index: Integer) of object;

  /// <summary>
  ///   Segmented toggle. Set <c>Segments</c> to a comma-/newline-separated
  ///   list, set <c>SelectedIndex</c> to choose, hook <c>OnChange</c> to
  ///   react to user clicks.
  /// </summary>
  TOBDSegmentedSwitch = class(TOBDCustomControl)
  private
    FSegments: TStringList;
    FSelectedIndex: Integer;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FActiveColor: TColor;
    FActiveTextColor: TColor;
    FInactiveTextColor: TColor;
    FCornerRadius: Integer;
    FOnChange: TOBDSegmentedChangeEvent;

    procedure SegmentsChanged(Sender: TObject);
    procedure SetSegments(const AValue: TStringList);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetBorderColor(const AValue: TColor);
    procedure SetActiveColor(const AValue: TColor);
    procedure SetActiveTextColor(const AValue: TColor);
    procedure SetInactiveTextColor(const AValue: TColor);
    procedure SetCornerRadius(const AValue: Integer);

    function SegmentWidth: Single;
    function IndexAt(X: Integer): Integer;

  protected
    procedure PaintSkia(Canvas: ISkCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Segments: TStringList read FSegments write SetSegments;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default 0;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default SS_DEFAULT_BACKGROUND;
    property BorderColor: TColor read FBorderColor write SetBorderColor default SS_DEFAULT_BORDER;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default SS_DEFAULT_ACTIVE_BG;
    property ActiveTextColor: TColor read FActiveTextColor write SetActiveTextColor default SS_DEFAULT_ACTIVE_TEXT;
    property InactiveTextColor: TColor read FInactiveTextColor write SetInactiveTextColor default SS_DEFAULT_INACTIVE_TEXT;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default SS_DEFAULT_RADIUS;

    property OnChange: TOBDSegmentedChangeEvent read FOnChange write FOnChange;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSegmentedSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSegments := TStringList.Create;
  FSegments.OnChange := SegmentsChanged;
  FSelectedIndex := 0;
  FBackgroundColor := SS_DEFAULT_BACKGROUND;
  FBorderColor := SS_DEFAULT_BORDER;
  FActiveColor := SS_DEFAULT_ACTIVE_BG;
  FActiveTextColor := SS_DEFAULT_ACTIVE_TEXT;
  FInactiveTextColor := SS_DEFAULT_INACTIVE_TEXT;
  FCornerRadius := SS_DEFAULT_RADIUS;

  TabStop := True;
  Width := 240;
  Height := 32;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDSegmentedSwitch.Destroy;
begin
  FSegments.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SEGMENTS CHANGED
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SegmentsChanged(Sender: TObject);
begin
  if FSelectedIndex >= FSegments.Count then
    FSelectedIndex := FSegments.Count - 1;
  if FSelectedIndex < 0 then FSelectedIndex := 0;
  Invalidate;
end;

//------------------------------------------------------------------------------
// SET SEGMENTS
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetSegments(const AValue: TStringList);
begin
  FSegments.Assign(AValue);
end;

//------------------------------------------------------------------------------
// SET SELECTED INDEX
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetSelectedIndex(const AValue: Integer);
var
  Clamped: Integer;
begin
  Clamped := AValue;
  if Clamped < 0 then Clamped := 0;
  if Clamped >= FSegments.Count then Clamped := FSegments.Count - 1;
  if FSelectedIndex <> Clamped then
  begin
    FSelectedIndex := Clamped;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self, FSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetBorderColor(const AValue: TColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ACTIVE COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetActiveColor(const AValue: TColor);
begin
  if FActiveColor <> AValue then begin FActiveColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET ACTIVE TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetActiveTextColor(const AValue: TColor);
begin
  if FActiveTextColor <> AValue then begin FActiveTextColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET INACTIVE TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetInactiveTextColor(const AValue: TColor);
begin
  if FInactiveTextColor <> AValue then begin FInactiveTextColor := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SET CORNER RADIUS
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.SetCornerRadius(const AValue: Integer);
begin
  if (AValue >= 0) and (FCornerRadius <> AValue) then begin FCornerRadius := AValue;
  Invalidate;
  end;
end;

//------------------------------------------------------------------------------
// SEGMENT WIDTH
//------------------------------------------------------------------------------
function TOBDSegmentedSwitch.SegmentWidth: Single;
begin
  if FSegments.Count = 0 then Exit(0);
  Result := Width / FSegments.Count;
end;

//------------------------------------------------------------------------------
// INDEX AT
//------------------------------------------------------------------------------
function TOBDSegmentedSwitch.IndexAt(X: Integer): Integer;
var
  W: Single;
begin
  Result := -1;
  W := SegmentWidth;
  if W <= 0 then Exit;
  Result := Trunc(X / W);
  if Result < 0 then Result := 0;
  if Result >= FSegments.Count then Result := FSegments.Count - 1;
end;

//------------------------------------------------------------------------------
// MOUSE DOWN
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  if not Focused then SetFocus;
  if Button <> mbLeft then Exit;
  Idx := IndexAt(X);
  if Idx >= 0 then SetSelectedIndex(Idx);
end;

//------------------------------------------------------------------------------
// PAINT (delegates to the framework-neutral renderer)
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitch.PaintSkia(Canvas: ISkCanvas);
var
  State: TOBDSegmentedSwitchRenderState;
  I: Integer;
  Items: TArray<string>;
begin
  SetLength(Items, FSegments.Count);
  for I := 0 to FSegments.Count - 1 do Items[I] := FSegments[I];

  State.Width := Width;
  State.Height := Height;
  State.Segments := Items;
  State.SelectedIndex := FSelectedIndex;
  State.BackgroundColor := SafeColorRefToSkColor(FBackgroundColor);
  State.BorderColor := SafeColorRefToSkColor(FBorderColor);
  State.ActiveColor := SafeColorRefToSkColor(FActiveColor);
  State.ActiveTextColor := SafeColorRefToSkColor(FActiveTextColor);
  State.InactiveTextColor := SafeColorRefToSkColor(FInactiveTextColor);
  State.CornerRadius := FCornerRadius;

  RenderSegmentedSwitch(Canvas, State);
end;

end.
