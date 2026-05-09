//------------------------------------------------------------------------------
// UNIT           : OBD.DtcList.FMX.pas
// CONTENTS       : FMX binding for the virtualised DTC list
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Lives in `Packages/RunTime.FMX.dpk`. Reuses the
//                  framework-neutral renderer; mouse/touch handling
//                  uses FMX's gesture / wheel callbacks.
//------------------------------------------------------------------------------
unit OBD.DtcList.FMX;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Skia, System.Skia,

  OBD.Render.DtcList;

const
  DTCFMX_DEFAULT_ROW_HEIGHT     = 28;
  DTCFMX_DEFAULT_HEADER_HEIGHT  = 24;
  DTCFMX_DEFAULT_SCROLLBAR      = 8;
  DTCFMX_DEFAULT_BACKGROUND     = $FF181818;
  DTCFMX_DEFAULT_HEADER_BG      = $FF282828;
  DTCFMX_DEFAULT_ROW_ALT        = $FF1E1E1E;
  DTCFMX_DEFAULT_BORDER         = $FF404040;
  DTCFMX_DEFAULT_TEXT           = TAlphaColors.White;
  DTCFMX_DEFAULT_SELECTION      = $FF505050;
  DTCFMX_DEFAULT_INFO           = $FFFF9933;
  DTCFMX_DEFAULT_WARNING        = $FFFFA500;
  DTCFMX_DEFAULT_CRITICAL       = $FFE63333;

type
  TOBDDtcItemFMX = record
    Code: string;
    Description: string;
    Severity: TOBDDtcSeverity;
    Status: TOBDDtcStatus;
    Tag: NativeInt;
  end;

  TOBDDtcEventFMX = procedure(Sender: TObject; Index: Integer) of object;

  TOBDDtcListFMX = class(TSkPaintBox)
  private
    FItems: TList<TOBDDtcItemFMX>;
    FRowHeight: Integer;
    FHeaderHeight: Integer;
    FScrollY: Integer;
    FSelectedIndex: Integer;
    FBackgroundColor: TAlphaColor;
    FHeaderBackgroundColor: TAlphaColor;
    FRowAltColor: TAlphaColor;
    FBorderColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FSelectionColor: TAlphaColor;
    FSeverityInfoColor: TAlphaColor;
    FSeverityWarningColor: TAlphaColor;
    FSeverityCriticalColor: TAlphaColor;
    FShowHeader: Boolean;
    FShowAlternateRows: Boolean;
    FOnDtcClick: TOBDDtcEventFMX;
    FOnDtcDoubleClick: TOBDDtcEventFMX;

    procedure SetRowHeight(const AValue: Integer);
    procedure SetHeaderHeight(const AValue: Integer);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetBackgroundColor(const AValue: TAlphaColor);
    procedure SetHeaderBackgroundColor(const AValue: TAlphaColor);
    procedure SetRowAltColor(const AValue: TAlphaColor);
    procedure SetBorderColor(const AValue: TAlphaColor);
    procedure SetTextColor(const AValue: TAlphaColor);
    procedure SetSelectionColor(const AValue: TAlphaColor);
    procedure SetSeverityInfoColor(const AValue: TAlphaColor);
    procedure SetSeverityWarningColor(const AValue: TAlphaColor);
    procedure SetSeverityCriticalColor(const AValue: TAlphaColor);
    procedure SetShowHeader(const AValue: Boolean);
    procedure SetShowAlternateRows(const AValue: Boolean);
    function GetItem(Index: Integer): TOBDDtcItemFMX;
    function GetItemCount: Integer;
    function ListAreaTop: Integer;
    function ListAreaHeight: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function IndexAtY(Y: Single): Integer;
    procedure ClampScroll;
    procedure HandleDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DblClick; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddItem(const Code, Description: string;
      Severity: TOBDDtcSeverity = dsWarning;
      Status: TOBDDtcStatus = dsActive;
      Tag: NativeInt = 0): Integer;
    procedure UpdateItem(Index: Integer; const Item: TOBDDtcItemFMX);
    procedure RemoveItem(Index: Integer);
    procedure ClearItems;
    procedure EnsureVisible(Index: Integer);

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TOBDDtcItemFMX read GetItem;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
  published
    property RowHeight: Integer read FRowHeight write SetRowHeight default DTCFMX_DEFAULT_ROW_HEIGHT;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default DTCFMX_DEFAULT_HEADER_HEIGHT;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default DTCFMX_DEFAULT_BACKGROUND;
    property HeaderBackgroundColor: TAlphaColor read FHeaderBackgroundColor write SetHeaderBackgroundColor default DTCFMX_DEFAULT_HEADER_BG;
    property RowAlternateColor: TAlphaColor read FRowAltColor write SetRowAltColor default DTCFMX_DEFAULT_ROW_ALT;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default DTCFMX_DEFAULT_BORDER;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default DTCFMX_DEFAULT_TEXT;
    property SelectionColor: TAlphaColor read FSelectionColor write SetSelectionColor default DTCFMX_DEFAULT_SELECTION;
    property SeverityInfoColor: TAlphaColor read FSeverityInfoColor write SetSeverityInfoColor default DTCFMX_DEFAULT_INFO;
    property SeverityWarningColor: TAlphaColor read FSeverityWarningColor write SetSeverityWarningColor default DTCFMX_DEFAULT_WARNING;
    property SeverityCriticalColor: TAlphaColor read FSeverityCriticalColor write SetSeverityCriticalColor default DTCFMX_DEFAULT_CRITICAL;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property ShowAlternateRows: Boolean read FShowAlternateRows write SetShowAlternateRows default True;
    property OnDtcClick: TOBDDtcEventFMX read FOnDtcClick write FOnDtcClick;
    property OnDtcDoubleClick: TOBDDtcEventFMX read FOnDtcDoubleClick write FOnDtcDoubleClick;
  end;

implementation

constructor TOBDDtcListFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList<TOBDDtcItemFMX>.Create;
  FRowHeight := DTCFMX_DEFAULT_ROW_HEIGHT;
  FHeaderHeight := DTCFMX_DEFAULT_HEADER_HEIGHT;
  FSelectedIndex := -1;
  FBackgroundColor := DTCFMX_DEFAULT_BACKGROUND;
  FHeaderBackgroundColor := DTCFMX_DEFAULT_HEADER_BG;
  FRowAltColor := DTCFMX_DEFAULT_ROW_ALT;
  FBorderColor := DTCFMX_DEFAULT_BORDER;
  FTextColor := DTCFMX_DEFAULT_TEXT;
  FSelectionColor := DTCFMX_DEFAULT_SELECTION;
  FSeverityInfoColor := DTCFMX_DEFAULT_INFO;
  FSeverityWarningColor := DTCFMX_DEFAULT_WARNING;
  FSeverityCriticalColor := DTCFMX_DEFAULT_CRITICAL;
  FShowHeader := True;
  FShowAlternateRows := True;

  CanFocus := True;
  Width := 360;
  Height := 240;
  OnDraw := HandleDraw;
end;

destructor TOBDDtcListFMX.Destroy;
begin FItems.Free; inherited; end;

function TOBDDtcListFMX.AddItem(const Code, Description: string;
  Severity: TOBDDtcSeverity; Status: TOBDDtcStatus; Tag: NativeInt): Integer;
var Item: TOBDDtcItemFMX;
begin
  Item.Code := Code; Item.Description := Description;
  Item.Severity := Severity; Item.Status := Status; Item.Tag := Tag;
  Result := FItems.Add(Item);
  Redraw;
end;

procedure TOBDDtcListFMX.UpdateItem(Index: Integer; const Item: TOBDDtcItemFMX);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems[Index] := Item;
  Redraw;
end;

procedure TOBDDtcListFMX.RemoveItem(Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  FItems.Delete(Index);
  if FSelectedIndex >= FItems.Count then FSelectedIndex := FItems.Count - 1;
  ClampScroll;
  Redraw;
end;

procedure TOBDDtcListFMX.ClearItems;
begin
  FItems.Clear; FSelectedIndex := -1; FScrollY := 0; Redraw;
end;

function TOBDDtcListFMX.GetItem(Index: Integer): TOBDDtcItemFMX;
begin Result := FItems[Index]; end;

function TOBDDtcListFMX.GetItemCount: Integer;
begin Result := FItems.Count; end;

function TOBDDtcListFMX.ListAreaTop: Integer;
begin if FShowHeader then Result := FHeaderHeight else Result := 0; end;

function TOBDDtcListFMX.ListAreaHeight: Integer;
begin
  Result := Trunc(Height) - ListAreaTop;
  if Result < 0 then Result := 0;
end;

function TOBDDtcListFMX.ContentHeight: Integer;
begin Result := FItems.Count * FRowHeight; end;

function TOBDDtcListFMX.MaxScroll: Integer;
begin
  Result := ContentHeight - ListAreaHeight;
  if Result < 0 then Result := 0;
end;

procedure TOBDDtcListFMX.ClampScroll;
begin
  if FScrollY < 0 then FScrollY := 0;
  if FScrollY > MaxScroll then FScrollY := MaxScroll;
end;

function TOBDDtcListFMX.IndexAtY(Y: Single): Integer;
var RelY, Idx: Integer;
begin
  Result := -1;
  if Y < ListAreaTop then Exit;
  RelY := Trunc(Y) - ListAreaTop + FScrollY;
  Idx := RelY div FRowHeight;
  if (Idx >= 0) and (Idx < FItems.Count) then Result := Idx;
end;

procedure TOBDDtcListFMX.EnsureVisible(Index: Integer);
var RowTop, RowBottom: Integer;
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  RowTop := Index * FRowHeight;
  RowBottom := RowTop + FRowHeight;
  if RowTop < FScrollY then FScrollY := RowTop
  else if RowBottom > FScrollY + ListAreaHeight then
    FScrollY := RowBottom - ListAreaHeight;
  ClampScroll;
  Redraw;
end;

procedure TOBDDtcListFMX.SetRowHeight(const AValue: Integer);
begin
  if (AValue >= 12) and (FRowHeight <> AValue) then
  begin FRowHeight := AValue; ClampScroll; Redraw; end;
end;
procedure TOBDDtcListFMX.SetHeaderHeight(const AValue: Integer);
begin
  if (AValue >= 0) and (FHeaderHeight <> AValue) then
  begin FHeaderHeight := AValue; ClampScroll; Redraw; end;
end;
procedure TOBDDtcListFMX.SetSelectedIndex(const AValue: Integer);
begin if FSelectedIndex <> AValue then begin FSelectedIndex := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetBackgroundColor(const AValue: TAlphaColor);
begin if FBackgroundColor <> AValue then begin FBackgroundColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetHeaderBackgroundColor(const AValue: TAlphaColor);
begin if FHeaderBackgroundColor <> AValue then begin FHeaderBackgroundColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetRowAltColor(const AValue: TAlphaColor);
begin if FRowAltColor <> AValue then begin FRowAltColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetBorderColor(const AValue: TAlphaColor);
begin if FBorderColor <> AValue then begin FBorderColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetTextColor(const AValue: TAlphaColor);
begin if FTextColor <> AValue then begin FTextColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetSelectionColor(const AValue: TAlphaColor);
begin if FSelectionColor <> AValue then begin FSelectionColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetSeverityInfoColor(const AValue: TAlphaColor);
begin if FSeverityInfoColor <> AValue then begin FSeverityInfoColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetSeverityWarningColor(const AValue: TAlphaColor);
begin if FSeverityWarningColor <> AValue then begin FSeverityWarningColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetSeverityCriticalColor(const AValue: TAlphaColor);
begin if FSeverityCriticalColor <> AValue then begin FSeverityCriticalColor := AValue; Redraw; end; end;
procedure TOBDDtcListFMX.SetShowHeader(const AValue: Boolean);
begin if FShowHeader <> AValue then begin FShowHeader := AValue; ClampScroll; Redraw; end; end;
procedure TOBDDtcListFMX.SetShowAlternateRows(const AValue: Boolean);
begin if FShowAlternateRows <> AValue then begin FShowAlternateRows := AValue; Redraw; end; end;

procedure TOBDDtcListFMX.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var Idx: Integer;
begin
  inherited;
  if not IsFocused then SetFocus;
  if Button <> TMouseButton.mbLeft then Exit;
  Idx := IndexAtY(Y);
  if Idx <> FSelectedIndex then begin FSelectedIndex := Idx; Redraw; end;
  if (Idx >= 0) and Assigned(FOnDtcClick) then FOnDtcClick(Self, Idx);
end;

procedure TOBDDtcListFMX.DblClick;
begin
  inherited;
  if (FSelectedIndex >= 0) and Assigned(FOnDtcDoubleClick) then
    FOnDtcDoubleClick(Self, FSelectedIndex);
end;

procedure TOBDDtcListFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
const
  WHEEL_LINE_DELTA = 120;
begin
  inherited;
  // FMX delivers WheelDelta the same way VCL does — multiples of 120
  // per notch. Three rows per notch matches the VCL feel.
  FScrollY := FScrollY - (WheelDelta div WHEEL_LINE_DELTA) * FRowHeight * 3;
  ClampScroll;
  Redraw;
  Handled := True;
end;

procedure TOBDDtcListFMX.HandleDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  State: TOBDDtcListRenderState;
  Views: TArray<TOBDDtcRowView>;
  I: Integer;
  V: TOBDDtcRowView;
begin
  SetLength(Views, FItems.Count);
  for I := 0 to FItems.Count - 1 do
  begin
    V.Code := FItems[I].Code;
    V.Description := FItems[I].Description;
    V.Severity := FItems[I].Severity;
    V.Status := FItems[I].Status;
    Views[I] := V;
  end;

  State.Width := ADest.Width;
  State.Height := ADest.Height;
  State.Rows := Views;
  State.SelectedIndex := FSelectedIndex;
  State.ScrollY := FScrollY;
  State.RowHeight := FRowHeight;
  State.HeaderHeight := FHeaderHeight;
  State.ScrollbarWidth := DTCFMX_DEFAULT_SCROLLBAR;
  State.BackgroundColor := FBackgroundColor;
  State.HeaderBackgroundColor := FHeaderBackgroundColor;
  State.RowAlternateColor := FRowAltColor;
  State.BorderColor := FBorderColor;
  State.TextColor := FTextColor;
  State.SelectionColor := FSelectionColor;
  State.SeverityInfoColor := FSeverityInfoColor;
  State.SeverityWarningColor := FSeverityWarningColor;
  State.SeverityCriticalColor := FSeverityCriticalColor;
  State.ShowHeader := FShowHeader;
  State.ShowAlternateRows := FShowAlternateRows;

  RenderDtcList(ACanvas, State);
end;

end.
