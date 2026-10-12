//------------------------------------------------------------------------------
//  ERD.UI.DiffView
//
//  TOBDDiffView - scrollable before/after setting comparison card with change
//  highlighting, only-changes filtering and revert actions for OBD Studio Pro.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.DiffView;

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
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.ScrollBar;

type
  TOBDDiffView = class;

  /// <summary>One before/after setting row shown by <see cref="TOBDDiffView"/>.</summary>
  TOBDDiffRow = class(TCollectionItem)
  strict private
    FName: string;
    FLocation: string;
    FBeforeValue: string;
    FAfterValue: string;
    FIsChanged: Boolean;
    procedure SetName(const AValue: string);
    procedure SetLocation(const AValue: string);
    procedure SetBeforeValue(const AValue: string);
    procedure SetAfterValue(const AValue: string);
    procedure SetIsChanged(AValue: Boolean);
    procedure DoChanged;
  protected
    /// <summary>Returns the row name in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an unchanged diff row.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another diff row.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Human-readable setting name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Monospace setting location, such as Byte 2 . bit 4.</summary>
    property Location: string read FLocation write SetLocation;
    /// <summary>Value read before the edit.</summary>
    property BeforeValue: string read FBeforeValue write SetBeforeValue;
    /// <summary>Value that will be written or was written after the edit.</summary>
    property AfterValue: string read FAfterValue write SetAfterValue;
    /// <summary>Whether the row changed and receives accent styling.</summary>
    property IsChanged: Boolean read FIsChanged write SetIsChanged default False;
  end;

  /// <summary>Owned collection of diff rows.</summary>
  TOBDDiffRowCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDDiffRow;
    procedure SetItem(Index: Integer; AValue: TOBDDiffRow);
  protected
    /// <summary>Notifies the owner when a row changes.</summary>
    /// <param name="Item">Changed row.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a diff-row collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one diff row.</summary>
    /// <returns>New row.</returns>
    function Add: TOBDDiffRow;
    /// <summary>Indexed diff rows.</summary>
    property Items[Index: Integer]: TOBDDiffRow read GetItem write SetItem; default;
    /// <summary>Fires whenever the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Revert-row event.</summary>
  TOBDDiffRevertEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>Scrollable before/after diff card.</summary>
  TOBDDiffView = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FRows: TOBDDiffRowCollection;
    FOnlyChanges: Boolean;
    FFooterText: string;
    FScrollPos: Integer;
    FHoverIndex: Integer;
    FHoverRevertAll: Boolean;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FUpdateCount: Integer;
    FOnRevert: TOBDDiffRevertEvent;
    FOnRevertAll: TNotifyEvent;
    procedure SetTitle(const AValue: string);
    procedure SetRows(AValue: TOBDDiffRowCollection);
    procedure SetOnlyChanges(AValue: Boolean);
    procedure SetFooterText(const AValue: string);
    procedure RowsChanged(Sender: TObject);
    procedure DoChanged;
    function UsePreviewData: Boolean;
    function SourceCount: Integer;
    function SourceName(AIndex: Integer): string;
    function SourceLocation(AIndex: Integer): string;
    function SourceBefore(AIndex: Integer): string;
    function SourceAfter(AIndex: Integer): string;
    function SourceChanged(AIndex: Integer): Boolean;
    function VisibleByFilter(AIndex: Integer): Boolean;
    function VisibleCount: Integer;
    function VisibleToSource(AVisibleIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function FooterRect: TRect;
    function SwitchRect: TRect;
    function RevertAllRect(APainter: TOBDPainter): TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function ScrollBarWidth: Integer;
    function ScrollTrackRect: TRect;
    function ScrollThumbRect: TRect;
    function HitRow(X, Y: Integer; out ASourceIndex: Integer): Boolean;
    function HitRevert(X, Y: Integer; out ASourceIndex: Integer): Boolean;
    procedure SetScrollPos(AValue: Integer);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawColumnHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the diff card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles switch, revert and scroll clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Tracks hover state and scrollbar dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends scrollbar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Handles keyboard scrolling.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Scrolls down with the mouse wheel.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    /// <summary>Scrolls up with the mouse wheel.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    /// <summary>Creates the diff view and row collection.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Suspends repaint/change notifications.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes repaint/change notifications.</summary>
    procedure EndUpdate;
    /// <summary>Clears all diff rows.</summary>
    procedure Clear;
    /// <summary>Adds one diff row.</summary>
    /// <param name="ALocation">Monospace location.</param>
    /// <param name="AName">Setting name.</param>
    /// <param name="ABefore">Before value.</param>
    /// <param name="AAfter">After value.</param>
    /// <param name="AChanged">Changed flag.</param>
    /// <returns>New row.</returns>
    function AddRow(const ALocation, AName, ABefore, AAfter: string; AChanged: Boolean): TOBDDiffRow;
    /// <summary>Number of rows marked as changed.</summary>
    /// <returns>Changed row count.</returns>
    function ChangedCount: Integer;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Before/after rows.</summary>
    property Rows: TOBDDiffRowCollection read FRows write SetRows;
    /// <summary>When True, unchanged rows are hidden.</summary>
    property OnlyChanges: Boolean read FOnlyChanges write SetOnlyChanges default False;
    /// <summary>Footer provenance text.</summary>
    property FooterText: string read FFooterText write SetFooterText;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a row revert glyph is clicked.</summary>
    property OnRevert: TOBDDiffRevertEvent read FOnRevert write FOnRevert;
    /// <summary>Fires when the Revert all footer link is clicked.</summary>
    property OnRevertAll: TNotifyEvent read FOnRevertAll write FOnRevertAll;
  end;

implementation

const
  PREVIEW_COUNT = 6;

{ TOBDDiffRow --------------------------------------------------------------- }

constructor TOBDDiffRow.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TOBDDiffRow.Assign(Source: TPersistent);
var
  Row: TOBDDiffRow;
begin
  if Source is TOBDDiffRow then
  begin
    Row := TOBDDiffRow(Source);
    FName := Row.Name;
    FLocation := Row.Location;
    FBeforeValue := Row.BeforeValue;
    FAfterValue := Row.AfterValue;
    FIsChanged := Row.IsChanged;
    DoChanged;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDDiffRow.DoChanged;
begin
  Changed(False);
end;

function TOBDDiffRow.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDDiffRow.SetAfterValue(const AValue: string);
begin
  if FAfterValue = AValue then
    Exit;
  FAfterValue := AValue;
  DoChanged;
end;

procedure TOBDDiffRow.SetBeforeValue(const AValue: string);
begin
  if FBeforeValue = AValue then
    Exit;
  FBeforeValue := AValue;
  DoChanged;
end;

procedure TOBDDiffRow.SetIsChanged(AValue: Boolean);
begin
  if FIsChanged = AValue then
    Exit;
  FIsChanged := AValue;
  DoChanged;
end;

procedure TOBDDiffRow.SetLocation(const AValue: string);
begin
  if FLocation = AValue then
    Exit;
  FLocation := AValue;
  DoChanged;
end;

procedure TOBDDiffRow.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  DoChanged;
end;

{ TOBDDiffRowCollection ----------------------------------------------------- }

constructor TOBDDiffRowCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDDiffRow);
end;

function TOBDDiffRowCollection.Add: TOBDDiffRow;
begin
  Result := TOBDDiffRow(inherited Add);
end;

function TOBDDiffRowCollection.GetItem(Index: Integer): TOBDDiffRow;
begin
  Result := TOBDDiffRow(inherited GetItem(Index));
end;

procedure TOBDDiffRowCollection.SetItem(Index: Integer; AValue: TOBDDiffRow);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDDiffRowCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDDiffView -------------------------------------------------------------- }

constructor TOBDDiffView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := 420;
  Height := 400;
  FTitle := 'Changes';
  FHoverIndex := -1;
  FRows := TOBDDiffRowCollection.Create(Self);
  FRows.OnChange := RowsChanged;
end;

destructor TOBDDiffView.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TOBDDiffView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDDiffView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChanged;
end;

procedure TOBDDiffView.Clear;
begin
  FRows.Clear;
  FScrollPos := 0;
  DoChanged;
end;

function TOBDDiffView.AddRow(const ALocation, AName, ABefore, AAfter: string; AChanged: Boolean): TOBDDiffRow;
begin
  Result := FRows.Add;
  Result.Location := ALocation;
  Result.Name := AName;
  Result.BeforeValue := ABefore;
  Result.AfterValue := AAfter;
  Result.IsChanged := AChanged;
  DoChanged;
end;

function TOBDDiffView.ChangedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if SourceChanged(I) then
      Inc(Result);
end;

function TOBDDiffView.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDDiffView.DensityChanged;
begin
  inherited;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

procedure TOBDDiffView.DoChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

procedure TOBDDiffView.RowsChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TOBDDiffView.SetFooterText(const AValue: string);
begin
  if FFooterText = AValue then
    Exit;
  FFooterText := AValue;
  Invalidate;
end;

procedure TOBDDiffView.SetOnlyChanges(AValue: Boolean);
begin
  if FOnlyChanges = AValue then
    Exit;
  FOnlyChanges := AValue;
  FScrollPos := 0;
  Invalidate;
end;

procedure TOBDDiffView.SetRows(AValue: TOBDDiffRowCollection);
begin
  FRows.Assign(AValue);
  DoChanged;
end;

procedure TOBDDiffView.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

function TOBDDiffView.UsePreviewData: Boolean;
begin
  Result := (FRows.Count = 0) and IsPreview;
end;

function TOBDDiffView.SourceCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_COUNT
  else
    Result := FRows.Count;
end;

function TOBDDiffView.SourceName(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Fog lights installed';
      1: Result := 'Coming home';
      2: Result := 'Leaving home';
      3: Result := 'Daytime running lights';
      4: Result := 'Rear fog light';
    else
      Result := 'Comfort turn signal';
    end
  else
    Result := FRows[AIndex].Name;
end;

function TOBDDiffView.SourceLocation(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Byte 0';
      1: Result := 'Byte 2 ' + #$00B7 + ' bit 4';
      2: Result := 'Byte 2 ' + #$00B7 + ' bit 5';
      3: Result := 'Byte 3';
      4: Result := 'Byte 5';
    else
      Result := 'Byte 7';
    end
  else
    Result := FRows[AIndex].Location;
end;

function TOBDDiffView.SourceBefore(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Yes';
      1, 2: Result := 'Off';
      3: Result := 'Via fog lights';
      4: Result := 'Left';
    else
      Result := '3 flashes';
    end
  else
    Result := FRows[AIndex].BeforeValue;
end;

function TOBDDiffView.SourceAfter(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Yes';
      1, 2: Result := 'On';
      3: Result := 'Via low beam';
      4: Result := 'Left';
    else
      Result := '3 flashes';
    end
  else
    Result := FRows[AIndex].AfterValue;
end;

function TOBDDiffView.SourceChanged(AIndex: Integer): Boolean;
begin
  if UsePreviewData then
    Result := AIndex in [1, 2, 3]
  else
    Result := FRows[AIndex].IsChanged;
end;

function TOBDDiffView.VisibleByFilter(AIndex: Integer): Boolean;
begin
  Result := (not FOnlyChanges) or SourceChanged(AIndex);
end;

function TOBDDiffView.VisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(I) then
      Inc(Result);
end;

function TOBDDiffView.VisibleToSource(AVisibleIndex: Integer): Integer;
var
  I, N: Integer;
begin
  Result := -1;
  N := -1;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(I) then
    begin
      Inc(N);
      if N = AVisibleIndex then
        Exit(I);
    end;
end;

function TOBDDiffView.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDDiffView.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDDiffView.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDDiffView.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row - 6);
end;

function TOBDDiffView.BodyRect: TRect;
begin
  Result := Rect(1, HeaderHeight + ColumnHeaderHeight, ClientWidth - 1,
    ClientHeight - FooterHeight - 1);
end;

function TOBDDiffView.FooterRect: TRect;
begin
  Result := Rect(0, ClientHeight - FooterHeight, ClientWidth, ClientHeight);
end;

function TOBDDiffView.SwitchRect: TRect;
var
  H, W, LabelW: Integer;
begin
  H := ScaleValue(Metrics.Switch);
  W := Round(H * 1.9);
  LabelW := OBDMeasureText('Only changes', OBD_TEXT_BODY, twRegular, ScaleValue(96));
  Result := Rect(ClientWidth - ScaleValue(16) - W - ScaleValue(8) - LabelW,
    (HeaderHeight - H) div 2, ClientWidth - ScaleValue(16) - ScaleValue(8) - LabelW,
    (HeaderHeight + H) div 2);
end;

function TOBDDiffView.RevertAllRect(APainter: TOBDPainter): TRect;
var
  W, CY: Integer;
begin
  W := APainter.TextWidth('Revert all', OBD_TEXT_SMALL, twSemibold);
  CY := FooterRect.Top + FooterRect.Height div 2;
  Result := Rect(ClientWidth - ScaleValue(16) - W, CY - ScaleValue(12),
    ClientWidth - ScaleValue(16), CY + ScaleValue(12));
end;

function TOBDDiffView.ContentHeight: Integer;
begin
  Result := VisibleCount * RowHeight;
end;

function TOBDDiffView.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDDiffView.ScrollBarWidth: Integer;
begin
  Result := ScaleValue(10);
end;

function TOBDDiffView.ScrollTrackRect: TRect;
begin
  Result := Rect(ClientWidth - ScrollBarWidth - ScaleValue(3), BodyRect.Top + ScaleValue(2),
    ClientWidth - ScaleValue(3), BodyRect.Bottom - ScaleValue(2));
end;

function TOBDDiffView.ScrollThumbRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if ContentHeight <= BodyRect.Height then
    Exit;
  Result := OBDScrollThumbRect(ScrollTrackRect, True, 0, ContentHeight - 1,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDDiffView.HitRow(X, Y: Integer; out ASourceIndex: Integer): Boolean;
var
  Body: TRect;
  Visible: Integer;
begin
  Result := False;
  ASourceIndex := -1;
  Body := BodyRect;
  if not PtInRect(Body, Point(X, Y)) then
    Exit;
  Visible := (Y - Body.Top + FScrollPos) div RowHeight;
  ASourceIndex := VisibleToSource(Visible);
  Result := ASourceIndex >= 0;
end;

function TOBDDiffView.HitRevert(X, Y: Integer; out ASourceIndex: Integer): Boolean;
begin
  Result := HitRow(X, Y, ASourceIndex) and SourceChanged(ASourceIndex) and
    (X >= ClientWidth - ScaleValue(42));
end;

procedure TOBDDiffView.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDDiffView.DrawHeader(APainter: TOBDPainter);
var
  ChipText: string;
  ChipW, X: Integer;
  SR: TRect;
begin
  APainter.Text(ScaleValue(16), HeaderHeight div 2, FTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - ScaleValue(220));
  ChipText := Format('%d CHANGED', [ChangedCount]);
  ChipW := APainter.ChipWidth(ChipText);
  X := ScaleValue(16) + APainter.TextWidth(FTitle, OBD_TEXT_TITLE, twBold) + ScaleValue(14);
  if X + ChipW < ClientWidth - ScaleValue(190) then
    APainter.Chip(X, (HeaderHeight - ScaleValue(20)) div 2, ChipText, APainter.AccentText);
  SR := SwitchRect;
  APainter.Switch(SR.Left, (SR.Top + SR.Bottom) div 2, Metrics.Switch, FOnlyChanges, Enabled, Focused);
  APainter.Text(SR.Right + ScaleValue(8), (SR.Top + SR.Bottom) div 2, 'Only changes',
    OBD_TEXT_BODY, Palette.ForegroundText, twRegular);
  APainter.HLine(1, HeaderHeight - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDDiffView.DrawColumnHeader(APainter: TOBDPainter);
var
  Y, OldX, NewX: Integer;
begin
  Y := HeaderHeight;
  APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + ColumnHeaderHeight), APainter.HeaderFill);
  APainter.HLine(1, Y + ColumnHeaderHeight - 1, ClientWidth - 2, Palette.NeutralLight);
  OldX := Round(ClientWidth * 0.48);
  NewX := Round(ClientWidth * 0.72);
  APainter.Caps(ScaleValue(16), Y + ColumnHeaderHeight div 2, 'Setting');
  APainter.Caps(OldX, Y + ColumnHeaderHeight div 2, 'Before');
  APainter.Caps(NewX, Y + ColumnHeaderHeight div 2, 'After');
end;

procedure TOBDDiffView.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Body: TRect;
  Save: Integer;
  I, V, Y, Mid, OldX, NewX: Integer;
  Changed: Boolean;
  EdgeR: TRect;
begin
  Body := BodyRect;
  if VisibleCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  OldX := Round(ClientWidth * 0.48);
  NewX := Round(ClientWidth * 0.72);
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, Body.Left, Body.Top, Body.Right, Body.Bottom);
    V := -1;
    for I := 0 to SourceCount - 1 do
    begin
      if not VisibleByFilter(I) then
        Continue;
      Inc(V);
      Y := Body.Top + V * RowHeight - FScrollPos;
      if (Y > Body.Bottom) or (Y + RowHeight < Body.Top) then
        Continue;
      Mid := Y + RowHeight div 2;
      Changed := SourceChanged(I);
      if Changed then
      begin
        APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + RowHeight), APainter.Tint(Palette.Accent, 0.10));
        EdgeR := Rect(1, Y + ScaleValue(4), ScaleValue(5), Y + RowHeight - ScaleValue(8));
        APainter.FillRect(EdgeR, Palette.Accent);
      end
      else if I = FHoverIndex then
        APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + RowHeight), APainter.Tint(Palette.Accent, 0.06));
      APainter.Text(ScaleValue(16), Mid - ScaleValue(8), SourceName(I), OBD_TEXT_BODY,
        Palette.ForegroundText, twRegular, taLeftJustify, OldX - ScaleValue(28));
      APainter.Text(ScaleValue(16), Mid + ScaleValue(9), SourceLocation(I), OBD_TEXT_SMALL,
        Palette.GaugeLabel, twMono, taLeftJustify, OldX - ScaleValue(28));
      if Changed then
        APainter.Text(OldX, Mid, SourceBefore(I), OBD_TEXT_BODY, Palette.GaugeLabel,
          twRegular, taLeftJustify, NewX - OldX - ScaleValue(28))
      else
        APainter.Text(OldX, Mid, SourceBefore(I), OBD_TEXT_BODY, Palette.ForegroundText,
          twRegular, taLeftJustify, NewX - OldX - ScaleValue(28));
      if Changed then
        APainter.Text(NewX - ScaleValue(14), Mid, #$2192, OBD_TEXT_BODY, Palette.Subtle, twRegular, taCenter);
      if Changed then
        APainter.Text(NewX, Mid, SourceAfter(I), OBD_TEXT_BODY, APainter.AccentText,
          twSemibold, taLeftJustify, ClientWidth - NewX - ScaleValue(42))
      else
        APainter.Text(NewX, Mid, SourceAfter(I), OBD_TEXT_BODY, Palette.ForegroundText,
          twRegular, taLeftJustify, ClientWidth - NewX - ScaleValue(42));
      if Changed then
        APainter.Glyph(glUndo, ClientWidth - ScaleValue(20), Mid, Palette.Subtle, 0.75);
      APainter.HLine(1, Y + RowHeight - 1, ClientWidth - 2, Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDDiffView.DrawFooter(APainter: TOBDPainter);
var
  F: TRect;
  Text: string;
begin
  F := FooterRect;
  APainter.HLine(1, F.Top, ClientWidth - 2, Palette.NeutralLight);
  Text := FFooterText;
  if UsePreviewData and (Text = '') then
    Text := 'Read from the ECU 18:47 ' + #$00B7 + ' label file 5Q0-937-08X.clb';
  APainter.Text(ScaleValue(16), F.Top + F.Height div 2, Text, OBD_TEXT_BODY,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - ScaleValue(120));
  APainter.Text(ClientWidth - ScaleValue(16), F.Top + F.Height div 2, 'Revert all',
    OBD_TEXT_SMALL, APainter.AccentText, twSemibold, taRightJustify);
end;

procedure TOBDDiffView.DrawEmpty(APainter: TOBDPainter);
var
  Body: TRect;
  CX, CY: Integer;
begin
  Body := BodyRect;
  CX := (Body.Left + Body.Right) div 2;
  CY := (Body.Top + Body.Bottom) div 2;
  APainter.Glyph(glCompare, CX, CY - ScaleValue(24), Palette.Subtle, 1.7);
  APainter.Text(CX, CY + ScaleValue(4), 'No changes to show', OBD_TEXT_HEADING,
    Palette.ForegroundText, twSemibold, taCenter);
  APainter.Text(CX, CY + ScaleValue(26), 'Add rows or turn off Only changes.',
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDDiffView.DrawScrollBar(APainter: TOBDPainter);
begin
  if ContentHeight <= BodyRect.Height then
    Exit;
  OBDPaintScrollBar(APainter, ScrollTrackRect, True, 0, ContentHeight - 1,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDDiffView.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(ClientRect);
    DrawHeader(P);
    DrawColumnHeader(P);
    DrawRows(P, ACanvas);
    DrawFooter(P);
    DrawScrollBar(P);
  finally
    P.Free;
  end;
end;

procedure TOBDDiffView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Source: Integer;
  Thumb: TRect;
  P: TOBDPainter;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  if PtInRect(SwitchRect, Point(X, Y)) then
  begin
    OnlyChanges := not FOnlyChanges;
    Exit;
  end;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    if PtInRect(RevertAllRect(P), Point(X, Y)) then
    begin
      if Assigned(FOnRevertAll) then
        FOnRevertAll(Self);
      Exit;
    end;
  finally
    P.Free;
  end;
  if HitRevert(X, Y, Source) then
  begin
    if Assigned(FOnRevert) then
      FOnRevert(Self, Source);
    Exit;
  end;
  Thumb := ScrollThumbRect;
  if not Thumb.IsEmpty and (X >= ScrollTrackRect.Left - ScaleValue(2)) and (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    MouseCapture := True;
    Invalidate;
    Exit;
  end;
  if (ContentHeight > BodyRect.Height) and (X >= ScrollTrackRect.Left - ScaleValue(2)) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - BodyRect.Height)
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + BodyRect.Height);
  end;
end;

procedure TOBDDiffView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Source: Integer;
  Track, Thumb: TRect;
  P: TOBDPainter;
begin
  inherited;
  if FDraggingScroll then
  begin
    Track := ScrollTrackRect;
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll, Track.Height - Thumb.Height));
    Exit;
  end;
  FScrollHover := (ContentHeight > BodyRect.Height) and (X >= ScrollTrackRect.Left - ScaleValue(2)) and
    (Y >= BodyRect.Top) and (Y <= BodyRect.Bottom);
  if HitRow(X, Y, Source) then
    FHoverIndex := Source
  else
    FHoverIndex := -1;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    FHoverRevertAll := PtInRect(RevertAllRect(P), Point(X, Y));
  finally
    P.Free;
  end;
  if PtInRect(SwitchRect, Point(X, Y)) or FHoverRevertAll or FScrollHover or HitRevert(X, Y, Source) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDDiffView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TOBDDiffView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverIndex := -1;
  FHoverRevertAll := False;
  FScrollHover := False;
  if not FDraggingScroll then
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDDiffView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      SetScrollPos(FScrollPos - RowHeight);
    VK_DOWN:
      SetScrollPos(FScrollPos + RowHeight);
    VK_PRIOR:
      SetScrollPos(FScrollPos - BodyRect.Height);
    VK_NEXT:
      SetScrollPos(FScrollPos + BodyRect.Height);
    VK_HOME:
      SetScrollPos(0);
    VK_END:
      SetScrollPos(MaxScroll);
    VK_SPACE:
      OnlyChanges := not FOnlyChanges;
  else
    Exit;
  end;
  Key := 0;
end;

function TOBDDiffView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos + RowHeight * 3);
end;

function TOBDDiffView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDDiffView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
