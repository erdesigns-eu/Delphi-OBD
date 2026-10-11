//------------------------------------------------------------------------------
//  ERD.UI.Timeline
//
//  TOBDTimeline - scrollable job-history timeline with status glyphs,
//  segmented filtering and click events for OBD Studio Pro audit trails.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.Timeline;

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
  /// <summary>Timeline filter selected by the segmented control.</summary>
  TOBDTimelineFilter = (
    /// <summary>Show all entries.</summary>
    otlAll,
    /// <summary>Show write/programming entries.</summary>
    otlWrites,
    /// <summary>Show backup entries.</summary>
    otlBackups,
    /// <summary>Show failed/error entries.</summary>
    otlErrors);

  /// <summary>Timeline entry category used for filtering and default glyphs.</summary>
  TOBDTimelineEntryKind = (
    /// <summary>Generic audit entry.</summary>
    otlOther,
    /// <summary>ECU write or coding entry.</summary>
    otlWrite,
    /// <summary>Backup entry.</summary>
    otlBackup,
    /// <summary>Security unlock entry.</summary>
    otlSecurity,
    /// <summary>Flash or programming entry.</summary>
    otlFlash,
    /// <summary>Vehicle scan entry.</summary>
    otlScan);

  TOBDTimeline = class;

  /// <summary>One row in <see cref="TOBDTimeline"/>.</summary>
  TOBDTimelineEntry = class(TCollectionItem)
  strict private
    FTimeText: string;
    FKind: TOBDTimelineEntryKind;
    FGlyph: TOBDGlyph;
    FStatus: TOBDStatusKind;
    FTitle: string;
    FDetail: string;
    FUserName: string;
    FHasDetails: Boolean;
    procedure SetTimeText(const AValue: string);
    procedure SetKind(AValue: TOBDTimelineEntryKind);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetStatus(AValue: TOBDStatusKind);
    procedure SetTitle(const AValue: string);
    procedure SetDetail(const AValue: string);
    procedure SetUserName(const AValue: string);
    procedure SetHasDetails(AValue: Boolean);
    procedure DoChanged;
  protected
    /// <summary>Returns the entry title in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a neutral timeline row.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another timeline entry.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Monospace time text shown on the left.</summary>
    property TimeText: string read FTimeText write SetTimeText;
    /// <summary>Category used by the filter strip.</summary>
    property Kind: TOBDTimelineEntryKind read FKind write SetKind default otlOther;
    /// <summary>Row glyph.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glAudit;
    /// <summary>Status colour of the circular icon.</summary>
    property Status: TOBDStatusKind read FStatus write SetStatus default skNeutral;
    /// <summary>Primary row title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Secondary detail text.</summary>
    property Detail: string read FDetail write SetDetail;
    /// <summary>User or system actor shown right-aligned.</summary>
    property UserName: string read FUserName write SetUserName;
    /// <summary>Whether a chevron is painted to indicate drill-down details.</summary>
    property HasDetails: Boolean read FHasDetails write SetHasDetails default False;
  end;

  /// <summary>Owned collection of timeline entries.</summary>
  TOBDTimelineEntryCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDTimelineEntry;
    procedure SetItem(Index: Integer; AValue: TOBDTimelineEntry);
  protected
    /// <summary>Notifies the owner when an entry changes.</summary>
    /// <param name="Item">Changed entry.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a timeline-entry collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one entry.</summary>
    /// <returns>New entry.</returns>
    function Add: TOBDTimelineEntry;
    /// <summary>Indexed entries.</summary>
    property Items[Index: Integer]: TOBDTimelineEntry read GetItem write SetItem; default;
    /// <summary>Fires whenever entries change.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Timeline row click event.</summary>
  TOBDTimelineEntryClickEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>Scrollable job-history timeline card.</summary>
  TOBDTimeline = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FSubtitle: string;
    FGlyph: TOBDGlyph;
    FEntries: TOBDTimelineEntryCollection;
    FFilter: TOBDTimelineFilter;
    FScrollPos: Integer;
    FHoverIndex: Integer;
    FDraggingScroll: Boolean;
    FScrollHover: Boolean;
    FScrollDragOffset: Integer;
    FUpdateCount: Integer;
    FOnEntryClick: TOBDTimelineEntryClickEvent;
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetEntries(AValue: TOBDTimelineEntryCollection);
    procedure SetFilter(AValue: TOBDTimelineFilter);
    procedure EntriesChanged(Sender: TObject);
    procedure DoChanged;
    function UsePreviewData: Boolean;
    function SourceCount: Integer;
    function SourceTime(AIndex: Integer): string;
    function SourceKind(AIndex: Integer): TOBDTimelineEntryKind;
    function SourceGlyph(AIndex: Integer): TOBDGlyph;
    function SourceStatus(AIndex: Integer): TOBDStatusKind;
    function SourceTitle(AIndex: Integer): string;
    function SourceDetail(AIndex: Integer): string;
    function SourceUser(AIndex: Integer): string;
    function SourceHasDetails(AIndex: Integer): Boolean;
    function VisibleByFilter(AIndex: Integer): Boolean;
    function VisibleCount: Integer;
    function VisibleToSource(AVisibleIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function SegmentRect(AFilter: TOBDTimelineFilter): TRect;
    function BodyRect: TRect;
    function RowHeight: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function ScrollBarWidth: Integer;
    function ScrollTrackRect: TRect;
    function ScrollThumbRect: TRect;
    function HitSegment(X, Y: Integer): TOBDTimelineFilter;
    function HitRow(X, Y: Integer; out ASourceIndex: Integer): Boolean;
    procedure SetScrollPos(AValue: Integer);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the timeline card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles filter, row and scrollbar clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Tracks row and scrollbar hover state.</summary>
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
    /// <summary>Creates the timeline and entry collection.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the entry collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour for children.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Suspends repaint/change notifications.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes repaint/change notifications.</summary>
    procedure EndUpdate;
    /// <summary>Clears all runtime entries.</summary>
    procedure Clear;
    /// <summary>Adds a runtime entry.</summary>
    /// <param name="ATimeText">Time text.</param>
    /// <param name="AKind">Entry category.</param>
    /// <param name="ATitle">Title.</param>
    /// <param name="ADetail">Detail text.</param>
    /// <param name="AUserName">User or system actor.</param>
    /// <param name="AStatus">Status colour.</param>
    /// <returns>New entry.</returns>
    function AddEntry(const ATimeText: string; AKind: TOBDTimelineEntryKind;
      const ATitle, ADetail, AUserName: string; AStatus: TOBDStatusKind = skSuccess): TOBDTimelineEntry;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Subtitle shown next to the filter strip.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Header glyph.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glAudit;
    /// <summary>Timeline entry collection.</summary>
    property Entries: TOBDTimelineEntryCollection read FEntries write SetEntries;
    /// <summary>Selected segmented filter.</summary>
    property Filter: TOBDTimelineFilter read FFilter write SetFilter default otlAll;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a visible row is clicked.</summary>
    property OnEntryClick: TOBDTimelineEntryClickEvent read FOnEntryClick write FOnEntryClick;
  end;

implementation

const
  PREVIEW_COUNT = 5;

{ TOBDTimelineEntry --------------------------------------------------------- }

constructor TOBDTimelineEntry.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FGlyph := glAudit;
  FStatus := skNeutral;
end;

procedure TOBDTimelineEntry.Assign(Source: TPersistent);
var
  Entry: TOBDTimelineEntry;
begin
  if Source is TOBDTimelineEntry then
  begin
    Entry := TOBDTimelineEntry(Source);
    FTimeText := Entry.TimeText;
    FKind := Entry.Kind;
    FGlyph := Entry.Glyph;
    FStatus := Entry.Status;
    FTitle := Entry.Title;
    FDetail := Entry.Detail;
    FUserName := Entry.UserName;
    FHasDetails := Entry.HasDetails;
    DoChanged;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDTimelineEntry.DoChanged;
begin
  Changed(False);
end;

function TOBDTimelineEntry.GetDisplayName: string;
begin
  Result := FTitle;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDTimelineEntry.SetDetail(const AValue: string);
begin
  if FDetail = AValue then
    Exit;
  FDetail := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetHasDetails(AValue: Boolean);
begin
  if FHasDetails = AValue then
    Exit;
  FHasDetails := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetKind(AValue: TOBDTimelineEntryKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetStatus(AValue: TOBDStatusKind);
begin
  if FStatus = AValue then
    Exit;
  FStatus := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetTimeText(const AValue: string);
begin
  if FTimeText = AValue then
    Exit;
  FTimeText := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  DoChanged;
end;

procedure TOBDTimelineEntry.SetUserName(const AValue: string);
begin
  if FUserName = AValue then
    Exit;
  FUserName := AValue;
  DoChanged;
end;

{ TOBDTimelineEntryCollection ---------------------------------------------- }

constructor TOBDTimelineEntryCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDTimelineEntry);
end;

function TOBDTimelineEntryCollection.Add: TOBDTimelineEntry;
begin
  Result := TOBDTimelineEntry(inherited Add);
end;

function TOBDTimelineEntryCollection.GetItem(Index: Integer): TOBDTimelineEntry;
begin
  Result := TOBDTimelineEntry(inherited GetItem(Index));
end;

procedure TOBDTimelineEntryCollection.SetItem(Index: Integer; AValue: TOBDTimelineEntry);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDTimelineEntryCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDTimeline -------------------------------------------------------------- }

constructor TOBDTimeline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := 420;
  Height := 400;
  FTitle := 'Job history';
  FSubtitle := '';
  FGlyph := glAudit;
  FHoverIndex := -1;
  FEntries := TOBDTimelineEntryCollection.Create(Self);
  FEntries.OnChange := EntriesChanged;
end;

destructor TOBDTimeline.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TOBDTimeline.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDTimeline.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChanged;
end;

procedure TOBDTimeline.Clear;
begin
  FEntries.Clear;
  FScrollPos := 0;
  DoChanged;
end;

function TOBDTimeline.AddEntry(const ATimeText: string; AKind: TOBDTimelineEntryKind;
  const ATitle, ADetail, AUserName: string; AStatus: TOBDStatusKind): TOBDTimelineEntry;
begin
  Result := FEntries.Add;
  Result.TimeText := ATimeText;
  Result.Kind := AKind;
  Result.Title := ATitle;
  Result.Detail := ADetail;
  Result.UserName := AUserName;
  Result.Status := AStatus;
  case AKind of
    otlWrite: Result.Glyph := glWrite;
    otlBackup: Result.Glyph := glDatabase;
    otlSecurity: Result.Glyph := glUnlock;
    otlFlash: Result.Glyph := glFlash;
    otlScan: Result.Glyph := glEcu;
  else
    Result.Glyph := glAudit;
  end;
  DoChanged;
end;

function TOBDTimeline.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDTimeline.DensityChanged;
begin
  inherited;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

procedure TOBDTimeline.DoChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

procedure TOBDTimeline.EntriesChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TOBDTimeline.SetEntries(AValue: TOBDTimelineEntryCollection);
begin
  FEntries.Assign(AValue);
  DoChanged;
end;

procedure TOBDTimeline.SetFilter(AValue: TOBDTimelineFilter);
begin
  if FFilter = AValue then
    Exit;
  FFilter := AValue;
  FScrollPos := 0;
  Invalidate;
end;

procedure TOBDTimeline.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Invalidate;
end;

procedure TOBDTimeline.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDTimeline.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

function TOBDTimeline.UsePreviewData: Boolean;
begin
  Result := (FEntries.Count = 0) and IsPreview;
end;

function TOBDTimeline.SourceCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_COUNT
  else
    Result := FEntries.Count;
end;

function TOBDTimeline.SourceTime(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := '18:52';
      1: Result := '18:51';
      2: Result := '18:49';
      3: Result := '18:47';
    else
      Result := '18:40';
    end
  else
    Result := FEntries[AIndex].TimeText;
end;

function TOBDTimeline.SourceKind(AIndex: Integer): TOBDTimelineEntryKind;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := otlWrite;
      1: Result := otlBackup;
      2: Result := otlSecurity;
      3: Result := otlFlash;
    else
      Result := otlScan;
    end
  else
    Result := FEntries[AIndex].Kind;
end;

function TOBDTimeline.SourceGlyph(AIndex: Integer): TOBDGlyph;
begin
  if UsePreviewData then
    case SourceKind(AIndex) of
      otlWrite: Result := glWrite;
      otlBackup: Result := glDatabase;
      otlSecurity: Result := glUnlock;
      otlFlash: Result := glFlash;
      otlScan: Result := glEcu;
    else
      Result := glAudit;
    end
  else
    Result := FEntries[AIndex].Glyph;
end;

function TOBDTimeline.SourceStatus(AIndex: Integer): TOBDStatusKind;
begin
  if UsePreviewData then
  begin
    if AIndex = 3 then
      Result := skDanger
    else
      Result := skSuccess;
  end
  else
    Result := FEntries[AIndex].Status;
end;

function TOBDTimeline.SourceTitle(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Long coding written';
      1: Result := 'Backup saved';
      2: Result := 'Security access granted';
      3: Result := 'Flash aborted';
    else
      Result := 'Vehicle scanned';
    end
  else
    Result := FEntries[AIndex].Title;
end;

function TOBDTimeline.SourceDetail(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := '09 Central electrics ' + #$00B7 + ' 3 bytes changed';
      1: Result := 'CODING_09_2026-10-11.json';
      2: Result := '09 Central electrics ' + #$00B7 + ' level 0x03';
      3: Result := '01 Engine ' + #$00B7 + ' voltage dropped to 11.9 V ' + #$00B7 + ' checkpoint block 400';
    else
      Result := '14 control units ' + #$00B7 + ' 6 with faults';
    end
  else
    Result := FEntries[AIndex].Detail;
end;

function TOBDTimeline.SourceUser(AIndex: Integer): string;
begin
  if UsePreviewData then
  begin
    if AIndex in [2, 3] then
      Result := 'System'
    else
      Result := 'J. de Vries';
  end
  else
    Result := FEntries[AIndex].UserName;
end;

function TOBDTimeline.SourceHasDetails(AIndex: Integer): Boolean;
begin
  if UsePreviewData then
    Result := AIndex = 3
  else
    Result := FEntries[AIndex].HasDetails;
end;

function TOBDTimeline.VisibleByFilter(AIndex: Integer): Boolean;
begin
  case FFilter of
    otlWrites:
      Result := SourceKind(AIndex) in [otlWrite, otlFlash];
    otlBackups:
      Result := SourceKind(AIndex) = otlBackup;
    otlErrors:
      Result := SourceStatus(AIndex) = skDanger;
  else
    Result := True;
  end;
end;

function TOBDTimeline.VisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(I) then
      Inc(Result);
end;

function TOBDTimeline.VisibleToSource(AVisibleIndex: Integer): Integer;
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

function TOBDTimeline.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head + Metrics.Segment + 22);
end;

function TOBDTimeline.RowHeight: Integer;
begin
  Result := ScaleValue(System.Math.Max(56, Metrics.Row + 8));
end;

function TOBDTimeline.BodyRect: TRect;
begin
  Result := Rect(1, HeaderHeight, ClientWidth - 1, ClientHeight - 1);
end;

function TOBDTimeline.ContentHeight: Integer;
begin
  Result := VisibleCount * RowHeight;
end;

function TOBDTimeline.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDTimeline.ScrollBarWidth: Integer;
begin
  Result := ScaleValue(10);
end;

function TOBDTimeline.ScrollTrackRect: TRect;
begin
  Result := Rect(ClientWidth - ScrollBarWidth - ScaleValue(3), BodyRect.Top + ScaleValue(2),
    ClientWidth - ScaleValue(3), BodyRect.Bottom - ScaleValue(2));
end;

function TOBDTimeline.ScrollThumbRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if ContentHeight <= BodyRect.Height then
    Exit;
  Result := OBDScrollThumbRect(ScrollTrackRect, True, 0, ContentHeight - 1,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDTimeline.SegmentRect(AFilter: TOBDTimelineFilter): TRect;
var
  W, X, Y, I: Integer;
begin
  W := ScaleValue(72);
  Y := ScaleValue(Metrics.Head + 10);
  X := ClientWidth - ScaleValue(16) - W * 4;
  I := Ord(AFilter);
  Result := Rect(X + I * W, Y, X + (I + 1) * W, Y + ScaleValue(Metrics.Segment));
end;

function TOBDTimeline.HitSegment(X, Y: Integer): TOBDTimelineFilter;
var
  F: TOBDTimelineFilter;
begin
  Result := FFilter;
  for F := Low(TOBDTimelineFilter) to High(TOBDTimelineFilter) do
    if PtInRect(SegmentRect(F), Point(X, Y)) then
      Exit(F);
end;

function TOBDTimeline.HitRow(X, Y: Integer; out ASourceIndex: Integer): Boolean;
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

procedure TOBDTimeline.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDTimeline.DrawHeader(APainter: TOBDPainter);
var
  H, X, Y, I: Integer;
  Texts: array[0..3] of string;
  R: TRect;
  F: TOBDTimelineFilter;
  Fill, Ink: TColor;
  Sub: string;
begin
  H := ScaleValue(Metrics.Head);
  X := ScaleValue(16);
  APainter.Glyph(FGlyph, X + ScaleValue(8), H div 2, APainter.AccentText, 1);
  Inc(X, ScaleValue(26));
  APainter.Text(X, H div 2, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText, twBold,
    taLeftJustify, ClientWidth - X - ScaleValue(32));
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);

  Sub := FSubtitle;
  if UsePreviewData and (Sub = '') then
    Sub := 'Golf VII ' + #$00B7 + ' job 2026-0142';
  Y := H + ScaleValue(10);
  Texts[0] := 'All';
  Texts[1] := 'Writes';
  Texts[2] := 'Backups';
  Texts[3] := 'Errors';
  APainter.Text(ScaleValue(16), Y + ScaleValue(Metrics.Segment) div 2, Sub,
    OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taLeftJustify,
    ClientWidth - ScaleValue(340));
  for I := 0 to 3 do
  begin
    F := TOBDTimelineFilter(I);
    R := SegmentRect(F);
    if F = FFilter then
    begin
      Fill := Palette.Accent;
      Ink := APainter.OnAccent;
    end
    else
    begin
      Fill := APainter.HeaderFill;
      Ink := Palette.GaugeLabel;
    end;
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(8), Fill, Palette.NeutralLight);
    APainter.Text((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2,
      Texts[I], OBD_TEXT_BODY, Ink, twSemibold, taCenter, R.Width - ScaleValue(8));
  end;
end;

procedure TOBDTimeline.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Body: TRect;
  Save: Integer;
  I, V, Y, CY, LineX: Integer;
  C: TColor;
  DetailColor: TColor;
begin
  Body := BodyRect;
  if VisibleCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, Body.Left, Body.Top, Body.Right, Body.Bottom);
    LineX := ScaleValue(92);
    if VisibleCount > 1 then
      APainter.VLine(LineX, Body.Top + ScaleValue(12) - FScrollPos,
        RowHeight * (VisibleCount - 1), Palette.NeutralLight);
    V := -1;
    for I := 0 to SourceCount - 1 do
    begin
      if not VisibleByFilter(I) then
        Continue;
      Inc(V);
      Y := Body.Top + V * RowHeight - FScrollPos;
      CY := Y + ScaleValue(12);
      if (Y > Body.Bottom) or (Y + RowHeight < Body.Top) then
        Continue;
      if I = FHoverIndex then
        APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + RowHeight), APainter.Tint(Palette.Accent, 0.06));
      APainter.Text(ScaleValue(16), CY, SourceTime(I), OBD_TEXT_BODY, Palette.GaugeLabel, twMono);
      C := APainter.StatusColor(SourceStatus(I));
      APainter.Ellipse(LineX - ScaleValue(13), CY - ScaleValue(13), ScaleValue(26), ScaleValue(26), Palette.GaugeFace, C, 1.5);
      APainter.Glyph(SourceGlyph(I), LineX, CY, C, 0.75);
      APainter.Text(LineX + ScaleValue(24), CY, SourceTitle(I), OBD_TEXT_HEADING,
        Palette.ForegroundText, twSemibold, taLeftJustify, ClientWidth - LineX - ScaleValue(150));
      APainter.Text(ClientWidth - ScaleValue(16), CY, SourceUser(I), OBD_TEXT_SMALL,
        Palette.GaugeLabel, twRegular, taRightJustify, ScaleValue(120));
      if SourceStatus(I) = skDanger then
        DetailColor := APainter.DangerInk
      else
        DetailColor := Palette.GaugeLabel;
      APainter.Text(LineX + ScaleValue(24), CY + ScaleValue(20), SourceDetail(I),
        OBD_TEXT_BODY, DetailColor, twRegular, taLeftJustify,
        ClientWidth - LineX - ScaleValue(64));
      if SourceHasDetails(I) then
        APainter.Glyph(glChevronRight, ClientWidth - ScaleValue(20), CY + ScaleValue(20), Palette.Subtle, 0.75);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDTimeline.DrawEmpty(APainter: TOBDPainter);
var
  Body: TRect;
  CX, CY: Integer;
begin
  Body := BodyRect;
  CX := (Body.Left + Body.Right) div 2;
  CY := (Body.Top + Body.Bottom) div 2;
  APainter.Glyph(glAudit, CX, CY - ScaleValue(24), Palette.Subtle, 1.7);
  APainter.Text(CX, CY + ScaleValue(4), 'No history entries', OBD_TEXT_HEADING,
    Palette.ForegroundText, twSemibold, taCenter);
  APainter.Text(CX, CY + ScaleValue(26), 'Entries added at run time appear here.',
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDTimeline.DrawScrollBar(APainter: TOBDPainter);
begin
  if ContentHeight <= BodyRect.Height then
    Exit;
  OBDPaintScrollBar(APainter, ScrollTrackRect, True, 0, ContentHeight - 1,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDTimeline.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(ClientRect);
    DrawHeader(P);
    DrawRows(P, ACanvas);
    DrawScrollBar(P);
  finally
    P.Free;
  end;
end;

procedure TOBDTimeline.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Seg: TOBDTimelineFilter;
  Source: Integer;
  Thumb: TRect;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  Seg := HitSegment(X, Y);
  if (Seg <> FFilter) and PtInRect(SegmentRect(Seg), Point(X, Y)) then
  begin
    Filter := Seg;
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
    Exit;
  end;
  if HitRow(X, Y, Source) and Assigned(FOnEntryClick) then
    FOnEntryClick(Self, Source);
end;

procedure TOBDTimeline.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Source: Integer;
  Track, Thumb: TRect;
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
  if (FHoverIndex >= 0) or FScrollHover or PtInRect(SegmentRect(HitSegment(X, Y)), Point(X, Y)) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDTimeline.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TOBDTimeline.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverIndex := -1;
  FScrollHover := False;
  if not FDraggingScroll then
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDTimeline.KeyDown(var Key: Word; Shift: TShiftState);
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
  else
    Exit;
  end;
  Key := 0;
end;

function TOBDTimeline.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos + RowHeight * 3);
end;

function TOBDTimeline.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDTimeline.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
