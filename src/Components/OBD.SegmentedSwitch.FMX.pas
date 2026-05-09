//------------------------------------------------------------------------------
// UNIT           : OBD.SegmentedSwitch.FMX.pas
// CONTENTS       : FMX binding for the segmented switch
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.SegmentedSwitch.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.SegmentedSwitch;

const
  SSFMX_DEFAULT_BACKGROUND   = $FF282828;
  SSFMX_DEFAULT_BORDER       = $FF404040;
  SSFMX_DEFAULT_ACTIVE_BG    = $FFE68F1F;
  SSFMX_DEFAULT_ACTIVE_TEXT  = TAlphaColors.White;
  SSFMX_DEFAULT_INACTIVE     = TAlphaColors.White;
  SSFMX_DEFAULT_RADIUS       = 6;

type
  TOBDSegmentedChangeEventFMX = procedure(Sender: TObject; const Index: Integer) of object;

  TOBDSegmentedSwitchFMX = class(TSkPaintBox)
  private
    FSegments: TStringList;
    FSelectedIndex: Integer;
    FBackgroundColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FActiveColor: TAlphaColor;
    FActiveTextColor: TAlphaColor;
    FInactiveTextColor: TAlphaColor;
    FCornerRadius: Integer;
    FOnChange: TOBDSegmentedChangeEventFMX;

    procedure SegmentsChanged(Sender: TObject);
    procedure SetSegments(const AValue: TStringList);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetActiveColor(const AValue: TAlphaColor);
    procedure SetActiveTextColor(const AValue: TAlphaColor);
    procedure SetInactiveTextColor(const AValue: TAlphaColor);
    procedure SetCornerRadius(const AValue: Integer);

    function SegmentWidth: Single;
    function IndexAt(X: Single): Integer;

    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Segments: TStringList read FSegments write SetSegments;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default 0;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default SSFMX_DEFAULT_BACKGROUND;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default SSFMX_DEFAULT_BORDER;
    property ActiveColor: TAlphaColor read FActiveColor write SetActiveColor default SSFMX_DEFAULT_ACTIVE_BG;
    property ActiveTextColor: TAlphaColor read FActiveTextColor write SetActiveTextColor default SSFMX_DEFAULT_ACTIVE_TEXT;
    property InactiveTextColor: TAlphaColor read FInactiveTextColor write SetInactiveTextColor default SSFMX_DEFAULT_INACTIVE;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default SSFMX_DEFAULT_RADIUS;
    property OnChange: TOBDSegmentedChangeEventFMX read FOnChange write FOnChange;
  end;

implementation

//------------------------------------------------------------------------------
// CREATE
//------------------------------------------------------------------------------
constructor TOBDSegmentedSwitchFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSegments := TStringList.Create;
  FSegments.OnChange := SegmentsChanged;
  FSelectedIndex := 0;
  FBackgroundColor := SSFMX_DEFAULT_BACKGROUND;
  FBorderColor := SSFMX_DEFAULT_BORDER;
  FActiveColor := SSFMX_DEFAULT_ACTIVE_BG;
  FActiveTextColor := SSFMX_DEFAULT_ACTIVE_TEXT;
  FInactiveTextColor := SSFMX_DEFAULT_INACTIVE;
  FCornerRadius := SSFMX_DEFAULT_RADIUS;

  CanFocus := True;
  Width := 240;
  Height := 32;
  OnDraw := HandleDraw;
end;

//------------------------------------------------------------------------------
// DESTROY
//------------------------------------------------------------------------------
destructor TOBDSegmentedSwitchFMX.Destroy;
begin
  FSegments.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// SEGMENTS CHANGED
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SegmentsChanged(Sender: TObject);
begin
  if FSelectedIndex >= FSegments.Count then FSelectedIndex := FSegments.Count - 1;
  if FSelectedIndex < 0 then FSelectedIndex := 0;
  Redraw;
end;

//------------------------------------------------------------------------------
// SET SEGMENTS
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetSegments(const AValue: TStringList);
begin
  FSegments.Assign(AValue);
end;

//------------------------------------------------------------------------------
// SET SELECTED INDEX
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetSelectedIndex(const AValue: Integer);
var
  Clamped: Integer;
begin
  Clamped := AValue;
  if Clamped < 0 then Clamped := 0;
  if Clamped >= FSegments.Count then Clamped := FSegments.Count - 1;
  if FSelectedIndex <> Clamped then
  begin
    FSelectedIndex := Clamped;
    Redraw;
    if Assigned(FOnChange) then FOnChange(Self, FSelectedIndex);
  end;
end;

//------------------------------------------------------------------------------
// SET BACKGROUND COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin
  if FBackgroundColor <> AValue then begin FBackgroundColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET BORDER COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetBorderColor(const AValue: TAlphaColor);
begin
  if FBorderColor <> AValue then begin FBorderColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET ACTIVE COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetActiveColor(const AValue: TAlphaColor);
begin
  if FActiveColor <> AValue then begin FActiveColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET ACTIVE TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetActiveTextColor(const AValue: TAlphaColor);
begin
  if FActiveTextColor <> AValue then begin FActiveTextColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET INACTIVE TEXT COLOR
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetInactiveTextColor(const AValue: TAlphaColor);
begin
  if FInactiveTextColor <> AValue then begin FInactiveTextColor := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SET CORNER RADIUS
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.SetCornerRadius(const AValue: Integer);
begin
  if (AValue >= 0) and (FCornerRadius <> AValue) then begin FCornerRadius := AValue;
  Redraw;
  end;
end;

//------------------------------------------------------------------------------
// SEGMENT WIDTH
//------------------------------------------------------------------------------
function TOBDSegmentedSwitchFMX.SegmentWidth: Single;
begin
  if FSegments.Count = 0 then Exit(0);
  Result := Width / FSegments.Count;
end;

//------------------------------------------------------------------------------
// INDEX AT
//------------------------------------------------------------------------------
function TOBDSegmentedSwitchFMX.IndexAt(X: Single): Integer;
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
procedure TOBDSegmentedSwitchFMX.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Idx: Integer;
begin
  inherited;
  if not IsFocused then SetFocus;
  if Button <> TMouseButton.mbLeft then Exit;
  Idx := IndexAt(X);
  if Idx >= 0 then SetSelectedIndex(Idx);
end;

//------------------------------------------------------------------------------
// HANDLE DRAW
//------------------------------------------------------------------------------
procedure TOBDSegmentedSwitchFMX.HandleDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDSegmentedSwitchRenderState;
  Items: TArray<string>;
  I: Integer;
begin
  SetLength(Items, FSegments.Count);
  for I := 0 to FSegments.Count - 1 do Items[I] := FSegments[I];

  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Segments := Items;
  State.SelectedIndex := FSelectedIndex;
  State.BackgroundColor := FBackgroundColor;
  State.BorderColor := FBorderColor;
  State.ActiveColor := FActiveColor;
  State.ActiveTextColor := FActiveTextColor;
  State.InactiveTextColor := FInactiveTextColor;
  State.CornerRadius := FCornerRadius;

  RenderSegmentedSwitch(ACanvas, State);
end;

end.
