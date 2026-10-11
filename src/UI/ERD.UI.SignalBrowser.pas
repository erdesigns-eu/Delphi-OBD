//------------------------------------------------------------------------------
//  ERD.UI.SignalBrowser
//
//  TOBDSignalBrowser - OBD Studio Pro A2L measurement-object browser with
//  search text, grouped rows, checks and an overlay scroll bar.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.SignalBrowser;

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
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.ScrollBar;

type
  /// <summary>Fires when a signal checkbox changes.</summary>
  /// <param name="Sender">The signal browser.</param>
  /// <param name="AIndex">Signal index in Signals.</param>
  /// <param name="AChecked">New checked state.</param>
  TOBDSignalCheckEvent = procedure(Sender: TObject; AIndex: Integer;
    AChecked: Boolean) of object;

  TOBDSignalBrowser = class;

  /// <summary>A2L measurement or calibration object row.</summary>
  TOBDSignalItem = class(TCollectionItem)
  strict private
    FGroup: string;
    FName: string;
    FUnitText: string;
    FAddress: string;
    FDataType: string;
    FChecked: Boolean;
    procedure SetGroup(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetUnitText(const AValue: string);
    procedure SetAddress(const AValue: string);
    procedure SetDataType(const AValue: string);
    procedure SetChecked(AValue: Boolean);
  protected
    /// <summary>Returns the signal name for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an unchecked signal row.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another signal item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>A2L group path or heading.</summary>
    property Group: string read FGroup write SetGroup;
    /// <summary>Measurement object name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Physical unit text.</summary>
    property UnitText: string read FUnitText write SetUnitText;
    /// <summary>Address text shown in the mono column.</summary>
    property Address: string read FAddress write SetAddress;
    /// <summary>Data type text shown in the mono column.</summary>
    property DataType: string read FDataType write SetDataType;
    /// <summary>Whether the signal is selected for measuring.</summary>
    property Checked: Boolean read FChecked write SetChecked default False;
  end;

  /// <summary>Streamable A2L signal collection.</summary>
  TOBDSignalCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDSignalItem;
    procedure SetItem(Index: Integer; AValue: TOBDSignalItem);
  protected
    /// <summary>Notifies the browser when an item changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a signal collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a signal item.</summary>
    /// <returns>The new signal.</returns>
    function Add: TOBDSignalItem;
    /// <summary>Copies another signal collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed signal indexer.</summary>
    property Items[Index: Integer]: TOBDSignalItem read GetItem write SetItem;
      default;
  end;

  /// <summary>A2L measurement-object browser card.</summary>
  TOBDSignalBrowser = class(TOBDCustomControl, IOBDSurface)
  strict private
    FSignals: TOBDSignalCollection;
    FFilterText: string;
    FItemIndex: Integer;
    FHoverIndex: Integer;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FOnCheckChange: TOBDSignalCheckEvent;
    procedure SetSignals(AValue: TOBDSignalCollection);
    procedure SetFilterText(const AValue: string);
    procedure SetItemIndex(AValue: Integer);
    procedure SetScrollPos(AValue: Integer);
    procedure SignalsChanged;
    function UsePreviewData: Boolean;
    function SourceCount: Integer;
    function SourceGroup(AIndex: Integer): string;
    function SourceName(AIndex: Integer): string;
    function SourceUnit(AIndex: Integer): string;
    function SourceAddress(AIndex: Integer): string;
    function SourceDataType(AIndex: Integer): string;
    function SourceChecked(AIndex: Integer): Boolean;
    function MatchesFilter(AIndex: Integer): Boolean;
    function VisibleCount: Integer;
    function VisibleToSource(AVisibleIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function EditHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollBarRect: TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function ScrollThumbRect: TRect;
    function HitRow(X, Y: Integer; out ASourceIndex: Integer;
      out AOnCheck: Boolean): Boolean;
    function CheckedCount: Integer;
    procedure ToggleChecked(ASourceIndex: Integer);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawSearch(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the browser card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks row and scroll-bar hover.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Toggles checks or starts scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Ends scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Handles navigation and space-to-toggle.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Scrolls down.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    /// <summary>Scrolls up.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
  public
    /// <summary>Creates an empty signal browser.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the signal collection.</summary>
    destructor Destroy; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes every signal.</summary>
    procedure Clear;
    /// <summary>Adds one signal row.</summary>
    /// <param name="AGroup">Group heading.</param>
    /// <param name="AName">Signal name.</param>
    /// <param name="AUnit">Unit text.</param>
    /// <param name="AAddress">Address text.</param>
    /// <param name="ADataType">Data type text.</param>
    /// <param name="AChecked">Initial checked state.</param>
    /// <returns>The new signal item.</returns>
    function AddSignal(const AGroup, AName, AUnit, AAddress,
      ADataType: string; AChecked: Boolean = False): TOBDSignalItem;
    /// <summary>Number of stored signals.</summary>
    /// <returns>Signal count.</returns>
    function Count: Integer;
  published
    /// <summary>Streamed A2L measurement objects.</summary>
    property Signals: TOBDSignalCollection read FSignals write SetSignals;
    /// <summary>Case-insensitive text filter.</summary>
    property FilterText: string read FFilterText write SetFilterText;
    /// <summary>Selected source signal index, or -1.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires after a signal check box changes.</summary>
    property OnCheckChange: TOBDSignalCheckEvent read FOnCheckChange
      write FOnCheckChange;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

const
  PREVIEW_SIGNAL_COUNT = 12;

function PreviewGroup(AIndex: Integer): string;
begin
  case AIndex of
    0 .. 3: Result := 'Air system';
    4 .. 7: Result := 'Injection';
    8, 9: Result := 'Exhaust';
  else
    Result := 'Maps';
  end;
end;

function PreviewName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Boost pressure actual';
    1: Result := 'Boost pressure setpoint';
    2: Result := 'Air mass actual';
    3: Result := 'Air mass setpoint';
    4: Result := 'Main injection quantity';
    5: Result := 'Rail pressure actual';
    6: Result := 'Rail pressure setpoint';
    7: Result := 'Start of injection';
    8: Result := 'Exhaust gas temperature';
    9: Result := 'DPF differential pressure';
    10: Result := 'Injection quantity';
  else
    Result := 'Boost setpoint';
  end;
end;

function PreviewUnit(AIndex: Integer): string;
begin
  case AIndex of
    0, 1: Result := 'hPa';
    2, 3, 4: Result := 'mg/stroke';
    5, 6: Result := 'bar';
    7: Result := #$00B0 + 'CA';
    8: Result := #$00B0 + 'C';
    9: Result := 'hPa';
  else
    Result := 'map';
  end;
end;

function PreviewAddress(AIndex: Integer): string;
begin
  Result := Format('0x%6.6x', [$180000 + AIndex * $24]);
end;

function PreviewType(AIndex: Integer): string;
begin
  if AIndex >= 10 then
    Result := 'MAP16'
  else
    Result := 'UWORD';
end;

{ TOBDSignalItem }

constructor TOBDSignalItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDataType := 'UWORD';
end;

procedure TOBDSignalItem.Assign(Source: TPersistent);
var
  Item: TOBDSignalItem;
begin
  if Source is TOBDSignalItem then
  begin
    Item := TOBDSignalItem(Source);
    FGroup := Item.Group;
    FName := Item.Name;
    FUnitText := Item.UnitText;
    FAddress := Item.Address;
    FDataType := Item.DataType;
    FChecked := Item.Checked;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDSignalItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDSignalItem.SetGroup(const AValue: string);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  Changed(False);
end;

procedure TOBDSignalItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDSignalItem.SetUnitText(const AValue: string);
begin
  if FUnitText = AValue then
    Exit;
  FUnitText := AValue;
  Changed(False);
end;

procedure TOBDSignalItem.SetAddress(const AValue: string);
begin
  if FAddress = AValue then
    Exit;
  FAddress := AValue;
  Changed(False);
end;

procedure TOBDSignalItem.SetDataType(const AValue: string);
begin
  if FDataType = AValue then
    Exit;
  FDataType := AValue;
  Changed(False);
end;

procedure TOBDSignalItem.SetChecked(AValue: Boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  Changed(False);
end;

{ TOBDSignalCollection }

constructor TOBDSignalCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDSignalItem);
end;

function TOBDSignalCollection.Add: TOBDSignalItem;
begin
  Result := TOBDSignalItem(inherited Add);
end;

procedure TOBDSignalCollection.Assign(Source: TPersistent);
var
  Src: TOBDSignalCollection;
  I: Integer;
begin
  if Source is TOBDSignalCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDSignalCollection(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    Update(nil);
  end
  else
    inherited;
end;

function TOBDSignalCollection.GetItem(Index: Integer): TOBDSignalItem;
begin
  Result := TOBDSignalItem(inherited GetItem(Index));
end;

procedure TOBDSignalCollection.SetItem(Index: Integer; AValue: TOBDSignalItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDSignalCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDSignalBrowser then
    TOBDSignalBrowser(GetOwner).SignalsChanged;
end;

{ TOBDSignalBrowser }

constructor TOBDSignalBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 380;
  Height := 560;
  TabStop := True;
  FItemIndex := -1;
  FHoverIndex := -1;
  FSignals := TOBDSignalCollection.Create(Self);
end;

destructor TOBDSignalBrowser.Destroy;
begin
  FSignals.Free;
  inherited;
end;

function TOBDSignalBrowser.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDSignalBrowser.Clear;
begin
  FSignals.Clear;
  FItemIndex := -1;
  FScrollPos := 0;
  Invalidate;
end;

function TOBDSignalBrowser.AddSignal(const AGroup, AName, AUnit, AAddress,
  ADataType: string; AChecked: Boolean): TOBDSignalItem;
begin
  Result := FSignals.Add;
  Result.Group := AGroup;
  Result.Name := AName;
  Result.UnitText := AUnit;
  Result.Address := AAddress;
  Result.DataType := ADataType;
  Result.Checked := AChecked;
  if FItemIndex < 0 then
    FItemIndex := Result.Index;
  Invalidate;
end;

function TOBDSignalBrowser.Count: Integer;
begin
  Result := FSignals.Count;
end;

procedure TOBDSignalBrowser.SetSignals(AValue: TOBDSignalCollection);
begin
  FSignals.Assign(AValue);
end;

procedure TOBDSignalBrowser.SetFilterText(const AValue: string);
begin
  if FFilterText = AValue then
    Exit;
  FFilterText := AValue;
  FScrollPos := 0;
  Invalidate;
end;

procedure TOBDSignalBrowser.SetItemIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, SourceCount - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
end;

procedure TOBDSignalBrowser.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDSignalBrowser.SignalsChanged;
begin
  if FItemIndex >= SourceCount then
    FItemIndex := SourceCount - 1;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

function TOBDSignalBrowser.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FSignals.Count = 0);
end;

function TOBDSignalBrowser.SourceCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_SIGNAL_COUNT
  else
    Result := FSignals.Count;
end;

function TOBDSignalBrowser.SourceGroup(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewGroup(AIndex)
  else
    Result := FSignals[AIndex].Group;
end;

function TOBDSignalBrowser.SourceName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewName(AIndex)
  else
    Result := FSignals[AIndex].Name;
end;

function TOBDSignalBrowser.SourceUnit(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewUnit(AIndex)
  else
    Result := FSignals[AIndex].UnitText;
end;

function TOBDSignalBrowser.SourceAddress(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewAddress(AIndex)
  else
    Result := FSignals[AIndex].Address;
end;

function TOBDSignalBrowser.SourceDataType(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewType(AIndex)
  else
    Result := FSignals[AIndex].DataType;
end;

function TOBDSignalBrowser.SourceChecked(AIndex: Integer): Boolean;
begin
  if UsePreviewData then
    Result := AIndex in [0, 1, 2, 4, 5, 7]
  else
    Result := FSignals[AIndex].Checked;
end;

function TOBDSignalBrowser.MatchesFilter(AIndex: Integer): Boolean;
var
  F, S: string;
begin
  if FFilterText = '' then
    Exit(True);
  F := AnsiUpperCase(FFilterText);
  S := AnsiUpperCase(SourceGroup(AIndex) + ' ' + SourceName(AIndex) + ' ' +
    SourceUnit(AIndex) + ' ' + SourceAddress(AIndex) + ' ' +
    SourceDataType(AIndex));
  Result := Pos(F, S) > 0;
end;

function TOBDSignalBrowser.VisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if MatchesFilter(I) then
      Inc(Result);
end;

function TOBDSignalBrowser.VisibleToSource(AVisibleIndex: Integer): Integer;
var
  I, N: Integer;
begin
  Result := -1;
  N := 0;
  for I := 0 to SourceCount - 1 do
    if MatchesFilter(I) then
    begin
      if N = AVisibleIndex then
        Exit(I);
      Inc(N);
    end;
end;

function TOBDSignalBrowser.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDSignalBrowser.EditHeight: Integer;
begin
  Result := ScaleValue(Metrics.Edit);
end;

function TOBDSignalBrowser.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDSignalBrowser.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row - 12);
  if Result < ScaleValue(32) then
    Result := ScaleValue(32);
end;

function TOBDSignalBrowser.BodyRect: TRect;
begin
  Result := Rect(1, HeaderHeight + EditHeight + ScaleValue(24),
    ClientWidth - 1, ClientHeight - FooterHeight);
end;

function TOBDSignalBrowser.ScrollBarRect: TRect;
begin
  Result := Rect(ClientWidth - ScaleValue(14), BodyRect.Top,
    ClientWidth - 1, BodyRect.Bottom);
end;

function TOBDSignalBrowser.ContentHeight: Integer;
var
  I, Last: Integer;
  G: string;
begin
  Result := 0;
  Last := -1;
  G := '';
  for I := 0 to VisibleCount - 1 do
  begin
    if SourceGroup(VisibleToSource(I)) <> G then
    begin
      Inc(Result, RowHeight);
      G := SourceGroup(VisibleToSource(I));
    end;
    Inc(Result, RowHeight);
    Last := I;
  end;
  if Last < 0 then
    Result := 0;
end;

function TOBDSignalBrowser.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDSignalBrowser.ScrollThumbRect: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0, MaxScroll,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDSignalBrowser.HitRow(X, Y: Integer; out ASourceIndex: Integer;
  out AOnCheck: Boolean): Boolean;
var
  R: TRect;
  I, Src, RowTop, CheckX: Integer;
  G: string;
begin
  Result := False;
  ASourceIndex := -1;
  AOnCheck := False;
  R := BodyRect;
  if not PtInRect(R, Point(X, Y)) then
    Exit;
  RowTop := R.Top - FScrollPos;
  G := '';
  for I := 0 to VisibleCount - 1 do
  begin
    Src := VisibleToSource(I);
    if SourceGroup(Src) <> G then
    begin
      if (Y >= RowTop) and (Y < RowTop + RowHeight) then
        Exit(False);
      Inc(RowTop, RowHeight);
      G := SourceGroup(Src);
    end;
    if (Y >= RowTop) and (Y < RowTop + RowHeight) then
    begin
      ASourceIndex := Src;
      CheckX := ScaleValue(16);
      AOnCheck := (X >= CheckX) and (X <= CheckX + ScaleValue(Metrics.Check));
      Exit(True);
    end;
    Inc(RowTop, RowHeight);
  end;
end;

function TOBDSignalBrowser.CheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if SourceChecked(I) then
      Inc(Result);
end;

procedure TOBDSignalBrowser.ToggleChecked(ASourceIndex: Integer);
var
  NewValue: Boolean;
begin
  if (ASourceIndex < 0) or (ASourceIndex >= SourceCount) or UsePreviewData then
    Exit;
  NewValue := not FSignals[ASourceIndex].Checked;
  FSignals[ASourceIndex].Checked := NewValue;
  if Assigned(FOnCheckChange) then
    FOnCheckChange(Self, ASourceIndex, NewValue);
end;

procedure TOBDSignalBrowser.DrawHeader(APainter: TOBDPainter);
var
  H, X: Integer;
begin
  H := HeaderHeight;
  APainter.Glyph(glDatabase, ScaleValue(26), H div 2, APainter.AccentText, 1);
  X := ScaleValue(48);
  APainter.Text(X, H div 2 - ScaleValue(8), 'Signals', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold);
  APainter.Text(X, H div 2 + ScaleValue(12),
    '03L906023xx.a2l ' + #$00B7 + ' EDC17C64', OBD_TEXT_SMALL,
    Palette.GaugeLabel);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDSignalBrowser.DrawSearch(APainter: TOBDPainter);
var
  R: TRect;
  S: string;
begin
  R := Rect(ScaleValue(16), HeaderHeight + ScaleValue(12),
    ClientWidth - ScaleValue(16), HeaderHeight + ScaleValue(12) + EditHeight);
  APainter.EditFrame(R, False, Enabled);
  APainter.Glyph(glSearch, R.Left + ScaleValue(18), R.Top + R.Height div 2,
    Palette.Subtle, 0.8);
  if FFilterText = '' then
    S := Format('Search %d objects', [SourceCount])
  else
    S := FFilterText;
  APainter.Text(R.Left + ScaleValue(38), R.Top + R.Height div 2, S,
    OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taLeftJustify,
    R.Width - ScaleValue(48));
end;

procedure TOBDSignalBrowser.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, Src, TopY, Mid, UnitX, InfoX: Integer;
  R: TRect;
  G, Info: string;
begin
  if VisibleCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  R := BodyRect;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    TopY := R.Top - FScrollPos;
    G := '';
    for I := 0 to VisibleCount - 1 do
    begin
      Src := VisibleToSource(I);
      if SourceGroup(Src) <> G then
      begin
        Mid := TopY + RowHeight div 2;
        APainter.FillRect(Rect(1, TopY, ClientWidth - 1, TopY + RowHeight),
          APainter.HeaderFill);
        APainter.Glyph(glChevronDown, ScaleValue(22), Mid, Palette.Subtle,
          0.65);
        APainter.Text(ScaleValue(36), Mid, SourceGroup(Src), OBD_TEXT_BODY,
          Palette.ForegroundText, twSemibold);
        TopY := TopY + RowHeight;
        G := SourceGroup(Src);
      end;
      Mid := TopY + RowHeight div 2;
      if Src = FItemIndex then
        APainter.FillRect(Rect(1, TopY, ClientWidth - 1, TopY + RowHeight),
          APainter.Tint(Palette.Accent, 0.10))
      else if Src = FHoverIndex then
        APainter.FillRect(Rect(1, TopY, ClientWidth - 1, TopY + RowHeight),
          APainter.HeaderFill);
      if SourceChecked(Src) then
        APainter.CheckBox(ScaleValue(16), Mid, Metrics.Check, cbChecked,
          True, Src = FHoverIndex, Src = FItemIndex)
      else
        APainter.CheckBox(ScaleValue(16), Mid, Metrics.Check, cbUnchecked,
          True, Src = FHoverIndex, Src = FItemIndex);
      UnitX := ClientWidth - ScaleValue(112);
      InfoX := ClientWidth - ScaleValue(16);
      APainter.Text(ScaleValue(16) + ScaleValue(Metrics.Check) + ScaleValue(8),
        Mid, SourceName(Src), OBD_TEXT_BODY, Palette.ForegroundText,
        twRegular, taLeftJustify, UnitX - ScaleValue(64));
      APainter.Text(UnitX, Mid, SourceUnit(Src), OBD_TEXT_SMALL,
        Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(64));
      Info := SourceAddress(Src) + ' ' + SourceDataType(Src);
      APainter.Text(InfoX, Mid, Info, OBD_TEXT_SMALL, Palette.GaugeLabel,
        twMono, taRightJustify, ScaleValue(104));
      TopY := TopY + RowHeight;
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDSignalBrowser.DrawFooter(APainter: TOBDPainter);
var
  FY: Integer;
begin
  FY := ClientHeight - FooterHeight;
  APainter.HLine(1, FY, ClientWidth - 2, Palette.NeutralLight);
  APainter.Text(ScaleValue(16), FY + FooterHeight div 2,
    Format('%d measurements selected', [CheckedCount]), OBD_TEXT_SMALL,
    Palette.GaugeLabel);
end;

procedure TOBDSignalBrowser.DrawEmpty(APainter: TOBDPainter);
begin
  APainter.Text(ClientWidth div 2, BodyRect.Top + BodyRect.Height div 2,
    'No signals', OBD_TEXT_HEADING, Palette.GaugeLabel, twSemibold, taCenter);
end;

procedure TOBDSignalBrowser.DrawScrollBar(APainter: TOBDPainter);
begin
  if MaxScroll > 0 then
    OBDPaintScrollBar(APainter, ScrollBarRect, True, 0, MaxScroll,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDSignalBrowser.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    P.Card(ClientRect);
    DrawHeader(P);
    DrawSearch(P);
    DrawRows(P, ACanvas);
    DrawScrollBar(P);
    DrawFooter(P);
  finally
    P.Free;
  end;
end;

procedure TOBDSignalBrowser.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Src: Integer;
  OnCheck: Boolean;
  R, Thumb: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    R := ScrollBarRect;
    Thumb := ScrollThumbRect;
    if R.Height > Thumb.Height then
      SetScrollPos(Round((Y - R.Top - FScrollDragOffset) * MaxScroll /
        (R.Height - Thumb.Height)));
    Exit;
  end;
  if not HitRow(X, Y, Src, OnCheck) then
    Src := -1;
  if FHoverIndex <> Src then
  begin
    FHoverIndex := Src;
    Invalidate;
  end;
  R := ScrollBarRect;
  if FScrollHover <> PtInRect(R, Point(X, Y)) then
  begin
    FScrollHover := not FScrollHover;
    Invalidate;
  end;
end;

procedure TOBDSignalBrowser.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Src: Integer;
  OnCheck: Boolean;
  Thumb: TRect;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  SetFocus;
  Thumb := ScrollThumbRect;
  if PtInRect(Thumb, Point(X, Y)) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    Invalidate;
    Exit;
  end;
  if HitRow(X, Y, Src, OnCheck) then
  begin
    ItemIndex := Src;
    if OnCheck then
      ToggleChecked(Src);
  end;
end;

procedure TOBDSignalBrowser.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    Invalidate;
  end;
end;

procedure TOBDSignalBrowser.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      begin
        ItemIndex := System.Math.Max(0, FItemIndex - 1);
        Key := 0;
      end;
    VK_DOWN:
      begin
        ItemIndex := System.Math.Min(SourceCount - 1, FItemIndex + 1);
        Key := 0;
      end;
    VK_SPACE:
      begin
        ToggleChecked(FItemIndex);
        Key := 0;
      end;
  end;
end;

function TOBDSignalBrowser.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos + RowHeight);
  Result := True;
end;

function TOBDSignalBrowser.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos - RowHeight);
  Result := True;
end;

procedure TOBDSignalBrowser.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or FScrollHover then
  begin
    FHoverIndex := -1;
    FScrollHover := False;
    Invalidate;
  end;
end;

procedure TOBDSignalBrowser.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

end.
