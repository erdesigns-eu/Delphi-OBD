//------------------------------------------------------------------------------
//  ERD.UI.Segmented
//
//  TOBDSegmented - a filter strip of adjacent segments ("All 5",
//  "Stored 3", "Pending 1", ...) of which exactly one is selected. The
//  selected segment has an orange outline over a light orange fill;
//  the others are separated by short divider lines.
//
//  Click or Left / Right / Home / End change the selection. AutoSize
//  sizes the strip to its labels; the height follows the density
//  (24 px desktop, 44 px tablet).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Segmented;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>Themed filter strip with one selected segment.</summary>
  TOBDSegmented = class(TOBDCustomControl)
  strict private
    FItems: TStrings;
    FItemIndex: Integer;
    FHotIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure SetItems(AValue: TStrings);
    procedure SetItemIndex(AValue: Integer);
    procedure ItemsChanged(Sender: TObject);
    procedure ChangeIndex(AIndex: Integer);
    function SegmentWidth(AIndex: Integer): Integer;
    function RingMargin: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    /// <summary>Creates an empty strip.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the items.</summary>
    destructor Destroy; override;
    /// <summary>Re-sizes for the new density.</summary>
    procedure DensityChanged; override;
    /// <summary>Segment under a client point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Segment index, or -1.</returns>
    function SegmentAt(X, Y: Integer): Integer;
    /// <summary>Bounds of a segment in client coordinates.</summary>
    /// <param name="AIndex">Segment index.</param>
    /// <returns>Segment rectangle.</returns>
    function SegmentRect(AIndex: Integer): TRect;
  published
    /// <summary>Sizes the strip to its labels.</summary>
    property AutoSize default True;
    /// <summary>Segment labels.</summary>
    property Items: TStrings read FItems write SetItems;
    /// <summary>Selected segment; -1 = none.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex
      default -1;
    /// <summary>Desktop or tablet height.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Focusable with Tab.</summary>
    property TabStop default True;
    /// <summary>Fires when the user selects another segment.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  SEGMENT_TEXT_SIZE = 11.5;
  SEGMENT_PAD = 20;

{ TOBDSegmented }

constructor TOBDSegmented.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChanged;
  FItemIndex := -1;
  FHotIndex := -1;
  Width := 240;
  Height := 32;
  TabStop := True;
  AutoSize := True;
end;

destructor TOBDSegmented.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TOBDSegmented.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

procedure TOBDSegmented.ItemsChanged(Sender: TObject);
begin
  if FItemIndex >= FItems.Count then
    FItemIndex := FItems.Count - 1;
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TOBDSegmented.SetItemIndex(AValue: Integer);
begin
  if not (csLoading in ComponentState) then
    AValue := EnsureRange(AValue, -1, FItems.Count - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
end;

procedure TOBDSegmented.ChangeIndex(AIndex: Integer);
begin
  AIndex := EnsureRange(AIndex, 0, FItems.Count - 1);
  if AIndex = FItemIndex then
    Exit;
  FItemIndex := AIndex;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TOBDSegmented.RingMargin: Integer;
begin
  Result := ScaleValue(4);
end;

function TOBDSegmented.SegmentWidth(AIndex: Integer): Integer;
begin
  Result := OBDMeasureText(FItems[AIndex], SEGMENT_TEXT_SIZE, twSemibold,
    ScaleValue(96)) + ScaleValue(SEGMENT_PAD);
end;

function TOBDSegmented.SegmentRect(AIndex: Integer): TRect;
var
  I, X: Integer;
begin
  X := RingMargin;
  for I := 0 to AIndex - 1 do
    Inc(X, SegmentWidth(I));
  Result := Rect(X, RingMargin, X, Height - RingMargin);
  if (AIndex >= 0) and (AIndex < FItems.Count) then
    Result.Right := X + SegmentWidth(AIndex);
end;

function TOBDSegmented.SegmentAt(X, Y: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if SegmentRect(I).Contains(Point(X, Y)) then
      Exit(I);
  Result := -1;
end;

function TOBDSegmented.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  I, W: Integer;
begin
  Result := True;
  W := 0;
  for I := 0 to FItems.Count - 1 do
    Inc(W, SegmentWidth(I));
  NewWidth := Max(W, ScaleValue(40)) + 2 * RingMargin;
  NewHeight := ScaleValue(Metrics.Segment) + 2 * RingMargin;
end;

procedure TOBDSegmented.DensityChanged;
begin
  if AutoSize and not (csLoading in ComponentState) then
    AdjustSize;
  inherited DensityChanged;
end;

procedure TOBDSegmented.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotIndex <> -1 then
  begin
    FHotIndex := -1;
    Invalidate;
  end;
end;

procedure TOBDSegmented.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOBDSegmented.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TOBDSegmented.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  if (Button = mbLeft) and Enabled then
  begin
    if TabStop and CanFocus and not Focused then
      SetFocus;
    Index := SegmentAt(X, Y);
    if Index >= 0 then
      ChangeIndex(Index);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TOBDSegmented.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := SegmentAt(X, Y);
  if Index <> FHotIndex then
  begin
    FHotIndex := Index;
    Invalidate;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TOBDSegmented.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FItems.Count > 0 then
    case Key of
      VK_LEFT, VK_UP:
        begin
          ChangeIndex(Max(FItemIndex - 1, 0));
          Key := 0;
        end;
      VK_RIGHT, VK_DOWN:
        begin
          ChangeIndex(FItemIndex + 1);
          Key := 0;
        end;
      VK_HOME:
        begin
          ChangeIndex(0);
          Key := 0;
        end;
      VK_END:
        begin
          ChangeIndex(FItems.Count - 1);
          Key := 0;
        end;
    end;
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TOBDSegmented.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TOBDSegmented.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TOBDSegmented.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  I, Total: Integer;
  R: TRect;
  Ink: TColor;
  Strength: Single;
begin
  if FItems.Count = 0 then
    Exit;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    R := SegmentRect(FItems.Count - 1);
    Total := R.Right - RingMargin;
    R := Rect(RingMargin, RingMargin, RingMargin + Total, Height - RingMargin);
    P.FillRect(R, Palette.GaugeFace);
    P.FrameRect(R, Palette.NeutralLight);
    if P.Dark then
      Strength := 0.28
    else
      Strength := 0.18;
    for I := 0 to FItems.Count - 1 do
    begin
      R := SegmentRect(I);
      if not Enabled then
      begin
        Ink := P.DisabledText;
        if I = FItemIndex then
          P.FrameRect(R, Palette.NeutralDark);
      end
      else if I = FItemIndex then
      begin
        P.FillRect(R, P.Tint(Palette.Accent, Strength));
        P.FrameRect(R, P.AccentText);
        Ink := P.AccentText;
      end
      else
      begin
        if I = FHotIndex then
          P.FillRect(Rect(R.Left + 1, R.Top + 1, R.Right, R.Bottom - 1),
            OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.06));
        Ink := Palette.Subtle;
      end;
      if (I > 0) and (I <> FItemIndex) then
        P.VLine(R.Left, R.Top + P.S(5), R.Height - P.S(10),
          Palette.NeutralLight);
      P.Text(R.Left + R.Width div 2, R.Top + R.Height div 2, FItems[I],
        SEGMENT_TEXT_SIZE, Ink, twSemibold, taCenter);
      if (I = FItemIndex) and Focused and not IsPreview then
        P.FocusRing(R.Left, R.Top, R.Width, R.Height);
    end;
  finally
    P.Free;
  end;
end;

end.
