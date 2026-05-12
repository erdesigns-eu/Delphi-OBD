//------------------------------------------------------------------------------
//  OBD.UI.SegmentedSwitch
//
//  TOBDSegmentedSwitch — iOS-style segmented multi-state toggle.
//  Set <see cref="Segments"/> to a label list (one segment per
//  line), set <see cref="SelectedIndex"/> to choose, hook
//  <see cref="OnChange"/> to react to user clicks.
//
//  Built on <c>TCustomControl</c> with native VCL
//  <c>TCanvas</c> painting (no third-party renderer).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.SegmentedSwitch.
//------------------------------------------------------------------------------

unit OBD.UI.SegmentedSwitch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Winapi.Windows,
  Winapi.Messages;

const
  /// <summary>Background colour of the unselected segments.</summary>
  SS_DEFAULT_BACKGROUND   = TColor($00282828);
  /// <summary>Border / divider colour.</summary>
  SS_DEFAULT_BORDER       = TColor($00404040);
  /// <summary>Active segment background colour.</summary>
  SS_DEFAULT_ACTIVE_BG    = TColor($001F8FE6);
  /// <summary>Active segment text colour.</summary>
  SS_DEFAULT_ACTIVE_TEXT  = clWhite;
  /// <summary>Inactive segment text colour.</summary>
  SS_DEFAULT_INACTIVE_TEXT = clWhite;
  /// <summary>Rounded-corner radius in pixels.</summary>
  SS_DEFAULT_RADIUS       = 6;

type
  /// <summary>Fires on every segment change. Main thread.</summary>
  TOBDSegmentedChangeEvent = procedure(Sender: TObject;
    AIndex: Integer) of object;

  /// <summary>
  ///   Multi-state segmented toggle control.
  /// </summary>
  TOBDSegmentedSwitch = class(TCustomControl)
  strict private
    FSegments: TStringList;
    FSelectedIndex: Integer;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FActiveColor: TColor;
    FActiveTextColor: TColor;
    FInactiveTextColor: TColor;
    FCornerRadius: Integer;
    FOnChange: TOBDSegmentedChangeEvent;
    procedure SegmentsChanged(ASender: TObject);
    procedure SetSegments(AValue: TStringList);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetColor(AIndex: Integer; AValue: TColor);
    procedure SetCornerRadius(AValue: Integer);
    function SegmentRect(AIndex: Integer): TRect;
    function HitTest(const APoint: TPoint): Integer;
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Constructs the control with two empty segments
    /// so a fresh drop on a form is immediately visible.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the underlying segment list.</summary>
    destructor Destroy; override;
  published
    /// <summary>Segment labels, one per line.</summary>
    property Segments: TStringList read FSegments write SetSegments;
    /// <summary>Index of the currently-selected segment.
    /// <c>-1</c> when nothing is selected.</summary>
    property SelectedIndex: Integer read FSelectedIndex
      write SetSelectedIndex default 0;
    /// <summary>Unselected-segment background colour.</summary>
    property BackgroundColor: TColor index 0 read FBackgroundColor
      write SetColor default SS_DEFAULT_BACKGROUND;
    /// <summary>Border / divider colour.</summary>
    property BorderColor: TColor index 1 read FBorderColor
      write SetColor default SS_DEFAULT_BORDER;
    /// <summary>Selected-segment background colour.</summary>
    property ActiveColor: TColor index 2 read FActiveColor
      write SetColor default SS_DEFAULT_ACTIVE_BG;
    /// <summary>Selected-segment text colour.</summary>
    property ActiveTextColor: TColor index 3 read FActiveTextColor
      write SetColor default SS_DEFAULT_ACTIVE_TEXT;
    /// <summary>Unselected-segment text colour.</summary>
    property InactiveTextColor: TColor index 4 read FInactiveTextColor
      write SetColor default SS_DEFAULT_INACTIVE_TEXT;
    /// <summary>Rounded-corner radius.</summary>
    property CornerRadius: Integer read FCornerRadius
      write SetCornerRadius default SS_DEFAULT_RADIUS;
    /// <summary>Fires when the user picks a different
    /// segment.</summary>
    property OnChange: TOBDSegmentedChangeEvent
      read FOnChange write FOnChange;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

constructor TOBDSegmentedSwitch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  Width := 180;
  Height := 28;
  TabStop := True;
  FSegments := TStringList.Create;
  FSegments.OnChange := SegmentsChanged;
  FSegments.Add('Option 1');
  FSegments.Add('Option 2');
  FSelectedIndex := 0;
  FBackgroundColor := SS_DEFAULT_BACKGROUND;
  FBorderColor := SS_DEFAULT_BORDER;
  FActiveColor := SS_DEFAULT_ACTIVE_BG;
  FActiveTextColor := SS_DEFAULT_ACTIVE_TEXT;
  FInactiveTextColor := SS_DEFAULT_INACTIVE_TEXT;
  FCornerRadius := SS_DEFAULT_RADIUS;
end;

destructor TOBDSegmentedSwitch.Destroy;
begin
  FSegments.Free;
  inherited;
end;

procedure TOBDSegmentedSwitch.SegmentsChanged(ASender: TObject);
begin
  if FSelectedIndex >= FSegments.Count then
    FSelectedIndex := FSegments.Count - 1;
  if FSelectedIndex < 0 then
    FSelectedIndex := 0;
  Invalidate;
end;

procedure TOBDSegmentedSwitch.SetSegments(AValue: TStringList);
begin
  FSegments.Assign(AValue);
end;

procedure TOBDSegmentedSwitch.SetSelectedIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := -1
  else if AValue >= FSegments.Count then
    AValue := FSegments.Count - 1;
  if FSelectedIndex = AValue then
    Exit;
  FSelectedIndex := AValue;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self, FSelectedIndex);
end;

procedure TOBDSegmentedSwitch.SetColor(AIndex: Integer; AValue: TColor);
begin
  case AIndex of
    0: FBackgroundColor := AValue;
    1: FBorderColor := AValue;
    2: FActiveColor := AValue;
    3: FActiveTextColor := AValue;
    4: FInactiveTextColor := AValue;
  end;
  Invalidate;
end;

procedure TOBDSegmentedSwitch.SetCornerRadius(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FCornerRadius = AValue then
    Exit;
  FCornerRadius := AValue;
  Invalidate;
end;

function TOBDSegmentedSwitch.SegmentRect(AIndex: Integer): TRect;
var
  Count: Integer;
  SegWidth: Integer;
begin
  Count := FSegments.Count;
  if Count = 0 then
    Exit(Rect(0, 0, 0, 0));
  SegWidth := Width div Count;
  Result.Left := AIndex * SegWidth;
  Result.Top := 0;
  Result.Right := Result.Left + SegWidth;
  Result.Bottom := Height;
  if AIndex = Count - 1 then
    Result.Right := Width;
end;

function TOBDSegmentedSwitch.HitTest(const APoint: TPoint): Integer;
var
  I: Integer;
begin
  for I := 0 to FSegments.Count - 1 do
    if PtInRect(SegmentRect(I), APoint) then
      Exit(I);
  Result := -1;
end;

procedure TOBDSegmentedSwitch.Paint;
var
  Outer: TRect;
  Seg: TRect;
  I: Integer;
  Caption: string;
begin
  Outer := ClientRect;
  // Background.
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.Pen.Color := FBorderColor;
  Canvas.RoundRect(Outer.Left, Outer.Top, Outer.Right, Outer.Bottom,
    FCornerRadius * 2, FCornerRadius * 2);

  Canvas.Font.Assign(Self.Font);
  for I := 0 to FSegments.Count - 1 do
  begin
    Seg := SegmentRect(I);
    if I = FSelectedIndex then
    begin
      Canvas.Brush.Color := FActiveColor;
      Canvas.Pen.Color := FActiveColor;
      Canvas.RoundRect(Seg.Left + 2, Seg.Top + 2,
                       Seg.Right - 2, Seg.Bottom - 2,
                       FCornerRadius * 2, FCornerRadius * 2);
      Canvas.Font.Color := FActiveTextColor;
    end
    else
      Canvas.Font.Color := FInactiveTextColor;

    Canvas.Brush.Style := bsClear;
    Caption := FSegments[I];
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Seg,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);

    // Inter-segment divider (skip after the last one).
    if (I < FSegments.Count - 1) and (I <> FSelectedIndex) and
       (I + 1 <> FSelectedIndex) then
    begin
      Canvas.Pen.Color := FBorderColor;
      Canvas.MoveTo(Seg.Right, Seg.Top + 4);
      Canvas.LineTo(Seg.Right, Seg.Bottom - 4);
    end;
  end;
end;

procedure TOBDSegmentedSwitch.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
var
  Hit: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  if CanFocus then
    SetFocus;
  Hit := HitTest(Point(X, Y));
  if Hit >= 0 then
    SetSelectedIndex(Hit);
end;

procedure TOBDSegmentedSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT:  SetSelectedIndex(System.Math.Max(0, FSelectedIndex - 1));
    VK_RIGHT: SetSelectedIndex(System.Math.Min(FSegments.Count - 1,
                                               FSelectedIndex + 1));
    VK_HOME:  SetSelectedIndex(0);
    VK_END:   SetSelectedIndex(FSegments.Count - 1);
  end;
end;

end.
