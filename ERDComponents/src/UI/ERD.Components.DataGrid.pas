//------------------------------------------------------------------------------
//  ERD.Components.DataGrid
//
//  TERDDataGrid - common themed data table built on TERDCustomItemList.
//
//  - Streamable column definitions with sortable, resizable headers.
//  - Data rows with chips, check boxes, glyph cells and group headers.
//  - Empty state and design-time preview for data-view panels.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.DataGrid;

interface

uses
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.ItemList;

type
  TERDDataGrid = class;

  /// <summary>Kind of content a data-grid cell paints.</summary>
  TERDDataCellKind = (
    /// <summary>Regular proportional text.</summary>
    dckText,
    /// <summary>Monospace text.</summary>
    dckMono,
    /// <summary>Monospace numeric value, right-aligned by default.</summary>
    dckNumber,
    /// <summary>Status chip using the cell tone.</summary>
    dckChip,
    /// <summary>Centered row check box.</summary>
    dckCheck,
    /// <summary>Centered row glyph.</summary>
    dckIcon);

  /// <summary>Fires after a check cell is toggled.</summary>
  /// <param name="Sender">The data grid.</param>
  /// <param name="ARow">Data-row index.</param>
  /// <param name="ACol">Column index.</param>
  TERDDataGridCheckEvent = procedure(Sender: TObject; ARow, ACol: Integer) of object;

  /// <summary>Fires after the grid changes sort order.</summary>
  /// <param name="Sender">The data grid.</param>
  /// <param name="AColumn">Sorted column, or -1.</param>
  /// <param name="ADescending">True when descending.</param>
  TERDDataGridSortEvent = procedure(Sender: TObject; AColumn: Integer;
    ADescending: Boolean) of object;

  /// <summary>One streamable data-grid column definition.</summary>
  TERDDataGridColumn = class(TCollectionItem)
  strict private
    FCaption: string;
    FWidth: Integer;
    FMinWidth: Integer;
    FAlignment: TAlignment;
    FKind: TERDDataCellKind;
    FSortable: Boolean;
    FVisible: Boolean;
    procedure SetCaption(const AValue: string);
    procedure SetWidth(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetKind(AValue: TERDDataCellKind);
    procedure SetSortable(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
  protected
    /// <summary>Returns the caption for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a default visible text column.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another data-grid column.</summary>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Header caption.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Column width in 96-DPI pixels; 0 lets the column fill.</summary>
    property Width: Integer read FWidth write SetWidth default 96;
    /// <summary>Minimum width in 96-DPI pixels.</summary>
    property MinWidth: Integer read FMinWidth write SetMinWidth default 24;
    /// <summary>Horizontal alignment for text cells.</summary>
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    /// <summary>Cell painter used for this column.</summary>
    property Kind: TERDDataCellKind read FKind write SetKind default dckText;
    /// <summary>Whether clicking this header sorts the grid.</summary>
    property Sortable: Boolean read FSortable write SetSortable default True;
    /// <summary>Whether the column is visible.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  /// <summary>Streamable collection of data-grid columns.</summary>
  TERDDataGridColumns = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TERDDataGridColumn;
    procedure SetItem(Index: Integer; AValue: TERDDataGridColumn);
  protected
    /// <summary>Notifies the owning grid that a column changed.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a data-grid column collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a column.</summary>
    function Add: TERDDataGridColumn;
    /// <summary>Copies another column collection.</summary>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed column indexer.</summary>
    property Items[Index: Integer]: TERDDataGridColumn read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed, self-painted common data grid.</summary>
  TERDDataGrid = class(TERDCustomItemList)
  strict private type
    TGridEntry = record
      IsGroup: Boolean;
      RowID: Integer;
      Caption: string;
      Expanded: Boolean;
      Values: TArray<string>;
      Tones: TArray<TERDStatusKind>;
      Data: NativeInt;
      Checked: Boolean;
      Glyph: TERDGlyph;
      Enabled: Boolean;
      Accent: Boolean;
    end;
  strict private
    FDataColumns: TERDDataGridColumns;
    FRows: TArray<TGridEntry>;
    FVisible: TArray<Integer>;
    FNextRowID: Integer;
    FUpdateCount: Integer;
    FSortColumn: Integer;
    FSortDescending: Boolean;
    FResizeColumn: Integer;
    FResizeStartX: Integer;
    FResizeStartWidth: Integer;
    FEmptyTitle: string;
    FEmptyHint: string;
    FEmptyGlyph: TERDGlyph;
    FEmptyAction: string;
    FEmptyActionRect: TRect;
    FOnCheckClick: TERDDataGridCheckEvent;
    FOnSort: TERDDataGridSortEvent;
    FOnEmptyAction: TNotifyEvent;
    procedure SetDataColumns(AValue: TERDDataGridColumns);
    procedure SetSortColumn(AValue: Integer);
    procedure SetSortDescending(AValue: Boolean);
    procedure SetEmptyTitle(const AValue: string);
    procedure SetEmptyHint(const AValue: string);
    procedure SetEmptyGlyph(AValue: TERDGlyph);
    procedure SetEmptyAction(const AValue: string);
    function GetCell(ACol, ARow: Integer): string;
    procedure SetCell(ACol, ARow: Integer; const AValue: string);
    function GetRowData(ARow: Integer): NativeInt;
    procedure SetRowData(ARow: Integer; AValue: NativeInt);
    function GetRowChecked(ARow: Integer): Boolean;
    procedure SetRowChecked(ARow: Integer; AValue: Boolean);
    function GetRowGlyph(ARow: Integer): TERDGlyph;
    procedure SetRowGlyph(ARow: Integer; AValue: TERDGlyph);
    function GetRowEnabled(ARow: Integer): Boolean;
    procedure SetRowEnabled(ARow: Integer; AValue: Boolean);
    function GetRowAccent(ARow: Integer): Boolean;
    procedure SetRowAccent(ARow: Integer; AValue: Boolean);
    function GetCellTone(ACol, ARow: Integer): TERDStatusKind;
    procedure SetCellTone(ACol, ARow: Integer; AValue: TERDStatusKind);
    function GetRowCount: Integer;
    function GetGroupExpanded(AGroup: Integer): Boolean;
    procedure SetGroupExpanded(AGroup: Integer; AValue: Boolean);
    function UsePreviewData: Boolean;
    function PreviewIsGroup(AIndex: Integer): Boolean;
    function PreviewCell(ACol, ARow: Integer): string;
    function DataEntryIndex(ARow: Integer): Integer;
    function DataIndexOfEntry(AEntry: Integer): Integer;
    function GroupEntryIndex(AGroup: Integer): Integer;
    function GroupIndexOfEntry(AEntry: Integer): Integer;
    function GroupRowCount(AGroupEntry: Integer): Integer;
    function VisibleEntry(AIndex: Integer): Integer;
    function VisibleIsGroup(AIndex: Integer): Boolean;
    function VisibleDataRow(AIndex: Integer): Integer;
    function VisibleIndexOfRowID(ARowID: Integer): Integer;
    function SelectedRowID: Integer;
    function GetSelectedRow: Integer;
    function FirstCheckColumn: Integer;
    procedure EnsureCellArrays(var AEntry: TGridEntry; ACol: Integer);
    procedure RebuildVisible;
    procedure GridChanged;
    procedure RefreshRows(AResort: Boolean);
    procedure SortRows;
    function CompareEntries(const A, B: TGridEntry): Integer;
    function HeaderBounds: TRect;
    function FillColumnIndex: Integer;
    function ColumnWidthPx(AColumn: Integer; const ABounds: TRect): Integer;
    function ColumnRect(AColumn: Integer; const ABounds: TRect): TRect;
    function ColumnAtX(X: Integer; const ABounds: TRect): Integer;
    function ColumnDividerAtX(X: Integer; const ABounds: TRect): Integer;
    function CurrentColumnWidth96(AColumn: Integer): Integer;
    function EffectiveAlignment(AColumn: TERDDataGridColumn): TAlignment;
    function EmptyActionHit(const P: TPoint): Boolean;
    procedure DrawGroup(APainter: TERDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean);
    procedure DrawDataRow(APainter: TERDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean);
    procedure DrawEmptyState(APainter: TERDPainter);
  protected
    /// <summary>Number of visible rows, including group headers.</summary>
    function GetItemCount: Integer; override;
    /// <summary>Text used by type-ahead navigation.</summary>
    function GetItemText(AIndex: Integer): string; override;
    /// <summary>Data-grid row height in device pixels.</summary>
    function ItemHeight: Integer; override;
    /// <summary>Column header strip height.</summary>
    function HeaderHeight: Integer; override;
    /// <summary>Paints column headers.</summary>
    procedure DrawHeader(APainter: TERDPainter; const ARect: TRect); override;
    /// <summary>Handles sorting or starts column resizing.</summary>
    procedure HeaderClick(X, Y: Integer); override;
    /// <summary>Updates the resize cursor over header dividers.</summary>
    procedure HeaderMouseMove(X, Y: Integer); override;
    /// <summary>Paints group and data rows.</summary>
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
    /// <summary>Paints the grid and overlays its custom empty state.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles row selection, group toggles, checks and empty action.</summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Resizes columns and tracks header hover.</summary>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends column resizing.</summary>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keyboard navigation skips group headers.</summary>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates an empty data grid.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the column collection.</summary>
    destructor Destroy; override;
    /// <summary>Called by the column collection after a change.</summary>
    procedure DataColumnsChanged;
    /// <summary>Begins a batched row update.</summary>
    procedure BeginUpdate;
    /// <summary>Ends a batched row update and refreshes the grid.</summary>
    procedure EndUpdate;
    /// <summary>Adds a data row.</summary>
    function AddRow(const AValues: array of string): Integer;
    /// <summary>Adds a group header row.</summary>
    function AddGroup(const ACaption: string): Integer;
    /// <summary>Removes every group and data row.</summary>
    procedure Clear;
    /// <summary>Toggles a row check box and fires OnCheckClick.</summary>
    procedure ToggleCheck(ARow, ACol: Integer);
    /// <summary>Sets a column width, clamped to its minimum width.</summary>
    procedure SetColumnWidth(ACol, AWidth: Integer);
    /// <summary>Tests whether a point is over the empty-state action button.</summary>
    function EmptyActionHitTest(X, Y: Integer): Boolean;
    /// <summary>Number of data rows, excluding group headers.</summary>
    property RowCount: Integer read GetRowCount;
    /// <summary>Data-row index of the selected row; -1 when nothing or a
    /// group header is selected. Indexes follow the current sort order;
    /// use RowData as a stable handle.</summary>
    property SelectedRow: Integer read GetSelectedRow;
    /// <summary>Cell text by column and data-row index.</summary>
    property Cells[ACol, ARow: Integer]: string read GetCell write SetCell;
    /// <summary>Caller-defined data tag by data-row index.</summary>
    property RowData[ARow: Integer]: NativeInt read GetRowData write SetRowData;
    /// <summary>Check state by data-row index.</summary>
    property RowChecked[ARow: Integer]: Boolean read GetRowChecked
      write SetRowChecked;
    /// <summary>Glyph drawn by dckIcon cells.</summary>
    property RowGlyph[ARow: Integer]: TERDGlyph read GetRowGlyph write SetRowGlyph;
    /// <summary>Whether the data row is enabled.</summary>
    property RowEnabled[ARow: Integer]: Boolean read GetRowEnabled
      write SetRowEnabled;
    /// <summary>Whether the data row paints an accent bar.</summary>
    property RowAccent[ARow: Integer]: Boolean read GetRowAccent
      write SetRowAccent;
    /// <summary>Status tone used by chip cells.</summary>
    property CellTone[ACol, ARow: Integer]: TERDStatusKind read GetCellTone
      write SetCellTone;
    /// <summary>Expanded state of group headers by group index.</summary>
    property GroupExpanded[AGroup: Integer]: Boolean read GetGroupExpanded
      write SetGroupExpanded;
  published
    /// <summary>Column definitions. Named DataColumns to avoid hiding Columns.</summary>
    property DataColumns: TERDDataGridColumns read FDataColumns
      write SetDataColumns;
    /// <summary>Sorted column, or -1 for no sort.</summary>
    property SortColumn: Integer read FSortColumn write SetSortColumn default -1;
    /// <summary>True when sort order is descending.</summary>
    property SortDescending: Boolean read FSortDescending write SetSortDescending
      default False;
    /// <summary>Centered empty-state title. Inherited EmptyText is unused.</summary>
    property EmptyTitle: string read FEmptyTitle write SetEmptyTitle;
    /// <summary>Optional secondary empty-state line.</summary>
    property EmptyHint: string read FEmptyHint write SetEmptyHint;
    /// <summary>Optional empty-state glyph.</summary>
    property EmptyGlyph: TERDGlyph read FEmptyGlyph write SetEmptyGlyph
      default glNone;
    /// <summary>Optional empty-state action caption.</summary>
    property EmptyAction: string read FEmptyAction write SetEmptyAction;
    /// <summary>Allows selecting more than one data row.</summary>
    property MultiSelect;
    /// <summary>Draws the 1 px frame.</summary>
    property BorderVisible;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the selection changes.</summary>
    property OnChange;
    /// <summary>Fires on a double-click or Enter on a data row.</summary>
    property OnItemActivate;
    /// <summary>Fires after a check cell toggles.</summary>
    property OnCheckClick: TERDDataGridCheckEvent read FOnCheckClick
      write FOnCheckClick;
    /// <summary>Fires after sorting changes.</summary>
    property OnSort: TERDDataGridSortEvent read FOnSort write FOnSort;
    /// <summary>Fires when the empty-state action is clicked.</summary>
    property OnEmptyAction: TNotifyEvent read FOnEmptyAction write FOnEmptyAction;
  end;

implementation

const
  DEFAULT_MIN_WIDTH = 24;
  DEFAULT_COLUMN_WIDTH = 96;
  RESIZE_HIT_ZONE = 4;

constructor TERDDataGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWidth := 96;
  FMinWidth := 24;
  FAlignment := taLeftJustify;
  FKind := dckText;
  FSortable := True;
  FVisible := True;
end;

procedure TERDDataGridColumn.Assign(Source: TPersistent);
var
  C: TERDDataGridColumn;
begin
  if Source is TERDDataGridColumn then
  begin
    C := TERDDataGridColumn(Source);
    FCaption := C.Caption;
    FWidth := C.Width;
    FMinWidth := C.MinWidth;
    FAlignment := C.Alignment;
    FKind := C.Kind;
    FSortable := C.Sortable;
    FVisible := C.Visible;
    Changed(False);
  end
  else
    inherited;
end;

function TERDDataGridColumn.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TERDDataGridColumn.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if (AValue > 0) and (AValue < FMinWidth) then
    AValue := FMinWidth;
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetMinWidth(AValue: Integer);
begin
  AValue := System.Math.Max(8, AValue);
  if FMinWidth <> AValue then
  begin
    FMinWidth := AValue;
    if (FWidth > 0) and (FWidth < FMinWidth) then
      FWidth := FMinWidth;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetKind(AValue: TERDDataCellKind);
begin
  if FKind <> AValue then
  begin
    FKind := AValue;
    if (FKind = dckNumber) and (FAlignment = taLeftJustify) then
      FAlignment := taRightJustify;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetSortable(AValue: Boolean);
begin
  if FSortable <> AValue then
  begin
    FSortable := AValue;
    Changed(False);
  end;
end;

procedure TERDDataGridColumn.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(False);
  end;
end;

constructor TERDDataGridColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDDataGridColumn);
end;

function TERDDataGridColumns.Add: TERDDataGridColumn;
begin
  Result := TERDDataGridColumn(inherited Add);
end;

procedure TERDDataGridColumns.Assign(Source: TPersistent);
var
  Src: TERDDataGridColumns;
  I: Integer;
begin
  if Source is TERDDataGridColumns then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TERDDataGridColumns(Source);
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

function TERDDataGridColumns.GetItem(Index: Integer): TERDDataGridColumn;
begin
  Result := TERDDataGridColumn(inherited GetItem(Index));
end;

procedure TERDDataGridColumns.SetItem(Index: Integer; AValue: TERDDataGridColumn);
begin
  inherited SetItem(Index, AValue);
end;

procedure TERDDataGridColumns.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDDataGrid then
    TERDDataGrid(GetOwner).DataColumnsChanged;
end;

constructor TERDDataGrid.Create(AOwner: TComponent);
var
  C: TERDDataGridColumn;
begin
  inherited Create(AOwner);
  FDataColumns := TERDDataGridColumns.Create(Self);
  FSortColumn := -1;
  FResizeColumn := -1;
  FEmptyGlyph := glNone;
  EmptyText := '';
  Width := 720;
  Height := 360;
  FDataColumns.BeginUpdate;
  try
    C := FDataColumns.Add; C.Caption := 'Run'; C.Width := 150;
    C.Kind := dckIcon;
    C := FDataColumns.Add; C.Caption := 'Date'; C.Width := 130;
    C.Kind := dckMono;
    C := FDataColumns.Add; C.Caption := 'Status'; C.Width := 104;
    C.Kind := dckChip;
    C := FDataColumns.Add; C.Caption := 'Power'; C.Width := 92;
    C.Kind := dckNumber;
    C := FDataColumns.Add; C.Caption := 'Notes'; C.Width := 0;
  finally
    FDataColumns.EndUpdate;
  end;
  FEmptyTitle := 'No data';
end;

destructor TERDDataGrid.Destroy;
begin
  FDataColumns.Free;
  inherited;
end;

procedure TERDDataGrid.DataColumnsChanged;
begin
  Invalidate;
end;

procedure TERDDataGrid.SetDataColumns(AValue: TERDDataGridColumns);
begin
  FDataColumns.Assign(AValue);
end;

procedure TERDDataGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TERDDataGrid.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      GridChanged;
  end;
end;

procedure TERDDataGrid.GridChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  RefreshRows(True);
end;

procedure TERDDataGrid.RefreshRows(AResort: Boolean);
var
  ID, GroupEntry, I, NewIndex: Integer;
begin
  // Keep the selection on the same row (or group header) while rows move
  // because of sorting, new rows or a group opening or closing.
  ID := SelectedRowID;
  GroupEntry := -1;
  if (ID < 0) and VisibleIsGroup(ItemIndex) then
    GroupEntry := VisibleEntry(ItemIndex);
  if AResort then
    SortRows;
  RebuildVisible;
  ItemsChanged;
  NewIndex := -1;
  if ID >= 0 then
    NewIndex := VisibleIndexOfRowID(ID)
  else if GroupEntry >= 0 then
    for I := 0 to High(FVisible) do
      if FVisible[I] = GroupEntry then
      begin
        NewIndex := I;
        Break;
      end;
  if ((ID >= 0) or (GroupEntry >= 0)) and (NewIndex <> ItemIndex) then
    ItemIndex := NewIndex;
end;

procedure TERDDataGrid.SetSortColumn(AValue: Integer);
begin
  if AValue < -1 then
    AValue := -1;
  if AValue >= FDataColumns.Count then
    AValue := FDataColumns.Count - 1;
  if FSortColumn = AValue then
    Exit;
  FSortColumn := AValue;
  RefreshRows(True);
  if Assigned(FOnSort) then
    FOnSort(Self, FSortColumn, FSortDescending);
end;

procedure TERDDataGrid.SetSortDescending(AValue: Boolean);
begin
  if FSortDescending = AValue then
    Exit;
  FSortDescending := AValue;
  RefreshRows(True);
  if Assigned(FOnSort) then
    FOnSort(Self, FSortColumn, FSortDescending);
end;

procedure TERDDataGrid.SetEmptyTitle(const AValue: string);
begin
  if FEmptyTitle <> AValue then
  begin
    FEmptyTitle := AValue;
    Invalidate;
  end;
end;

procedure TERDDataGrid.SetEmptyHint(const AValue: string);
begin
  if FEmptyHint <> AValue then
  begin
    FEmptyHint := AValue;
    Invalidate;
  end;
end;

procedure TERDDataGrid.SetEmptyGlyph(AValue: TERDGlyph);
begin
  if FEmptyGlyph <> AValue then
  begin
    FEmptyGlyph := AValue;
    Invalidate;
  end;
end;

procedure TERDDataGrid.SetEmptyAction(const AValue: string);
begin
  if FEmptyAction <> AValue then
  begin
    FEmptyAction := AValue;
    Invalidate;
  end;
end;

procedure TERDDataGrid.EnsureCellArrays(var AEntry: TGridEntry; ACol: Integer);
var
  I, OldLen, NewLen: Integer;
begin
  if ACol < 0 then
    Exit;
  NewLen := System.Math.Max(ACol + 1, FDataColumns.Count);
  if Length(AEntry.Values) < NewLen then
    SetLength(AEntry.Values, NewLen);
  OldLen := Length(AEntry.Tones);
  if OldLen < NewLen then
  begin
    SetLength(AEntry.Tones, NewLen);
    for I := OldLen to NewLen - 1 do
      AEntry.Tones[I] := skNeutral;
  end;
end;

function TERDDataGrid.AddRow(const AValues: array of string): Integer;
var
  E: TGridEntry;
  I, N, ID: Integer;
begin
  E := Default(TGridEntry);
  E.RowID := FNextRowID;
  ID := E.RowID;
  Inc(FNextRowID);
  E.Enabled := True;
  E.Glyph := glNone;
  SetLength(E.Values, System.Math.Max(Length(AValues), FDataColumns.Count));
  SetLength(E.Tones, Length(E.Values));
  for I := 0 to High(E.Tones) do
    E.Tones[I] := skNeutral;
  for I := 0 to High(AValues) do
    E.Values[I] := AValues[I];
  N := Length(FRows);
  SetLength(FRows, N + 1);
  FRows[N] := E;
  GridChanged;
  // Sorting may have moved the new row: return where it is now.
  Result := -1;
  for I := 0 to High(FRows) do
    if not FRows[I].IsGroup and (FRows[I].RowID = ID) then
    begin
      Result := DataIndexOfEntry(I);
      Break;
    end;
end;

function TERDDataGrid.AddGroup(const ACaption: string): Integer;
var
  E: TGridEntry;
  N: Integer;
begin
  E := Default(TGridEntry);
  E.IsGroup := True;
  E.Caption := ACaption;
  E.Expanded := True;
  N := Length(FRows);
  SetLength(FRows, N + 1);
  FRows[N] := E;
  Result := GroupIndexOfEntry(N);
  GridChanged;
end;

procedure TERDDataGrid.Clear;
begin
  SetLength(FRows, 0);
  SetLength(FVisible, 0);
  ClearSelection;
  GridChanged;
end;

function TERDDataGrid.GetRowCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FRows) do
    if not FRows[I].IsGroup then
      Inc(Result);
end;

function TERDDataGrid.DataEntryIndex(ARow: Integer): Integer;
var
  I, N: Integer;
begin
  Result := -1;
  N := 0;
  for I := 0 to High(FRows) do
    if not FRows[I].IsGroup then
    begin
      if N = ARow then
        Exit(I);
      Inc(N);
    end;
end;

function TERDDataGrid.DataIndexOfEntry(AEntry: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AEntry < 0) or (AEntry > High(FRows)) or FRows[AEntry].IsGroup then
    Exit;
  Result := 0;
  for I := 0 to AEntry - 1 do
    if not FRows[I].IsGroup then
      Inc(Result);
end;

function TERDDataGrid.GroupEntryIndex(AGroup: Integer): Integer;
var
  I, N: Integer;
begin
  Result := -1;
  N := 0;
  for I := 0 to High(FRows) do
    if FRows[I].IsGroup then
    begin
      if N = AGroup then
        Exit(I);
      Inc(N);
    end;
end;

function TERDDataGrid.GroupIndexOfEntry(AEntry: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AEntry < 0) or (AEntry > High(FRows)) or not FRows[AEntry].IsGroup then
    Exit;
  Result := 0;
  for I := 0 to AEntry - 1 do
    if FRows[I].IsGroup then
      Inc(Result);
end;

function TERDDataGrid.GroupRowCount(AGroupEntry: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (AGroupEntry < 0) or (AGroupEntry > High(FRows)) then
    Exit;
  for I := AGroupEntry + 1 to High(FRows) do
  begin
    if FRows[I].IsGroup then
      Break;
    Inc(Result);
  end;
end;

function TERDDataGrid.GetGroupExpanded(AGroup: Integer): Boolean;
var
  I: Integer;
begin
  I := GroupEntryIndex(AGroup);
  Result := (I >= 0) and FRows[I].Expanded;
end;

procedure TERDDataGrid.SetGroupExpanded(AGroup: Integer; AValue: Boolean);
var
  I: Integer;
begin
  I := GroupEntryIndex(AGroup);
  if (I >= 0) and (FRows[I].Expanded <> AValue) then
  begin
    FRows[I].Expanded := AValue;
    RefreshRows(False);
  end;
end;

function TERDDataGrid.GetCell(ACol, ARow: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := DataEntryIndex(ARow);
  if (I >= 0) and (ACol >= 0) and (ACol < Length(FRows[I].Values)) then
    Result := FRows[I].Values[ACol];
end;

procedure TERDDataGrid.SetCell(ACol, ARow: Integer; const AValue: string);
var
  I: Integer;
  E: TGridEntry;
begin
  I := DataEntryIndex(ARow);
  if (I < 0) or (ACol < 0) then
    Exit;
  E := FRows[I];
  EnsureCellArrays(E, ACol);
  if E.Values[ACol] <> AValue then
  begin
    E.Values[ACol] := AValue;
    FRows[I] := E;
    GridChanged;
  end;
end;

function TERDDataGrid.GetRowData(ARow: Integer): NativeInt;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    Result := FRows[I].Data
  else
    Result := 0;
end;

procedure TERDDataGrid.SetRowData(ARow: Integer; AValue: NativeInt);
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    FRows[I].Data := AValue;
  Invalidate;
end;

function TERDDataGrid.GetRowChecked(ARow: Integer): Boolean;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  Result := (I >= 0) and FRows[I].Checked;
end;

procedure TERDDataGrid.SetRowChecked(ARow: Integer; AValue: Boolean);
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if (I >= 0) and (FRows[I].Checked <> AValue) then
  begin
    FRows[I].Checked := AValue;
    Invalidate;
  end;
end;

function TERDDataGrid.GetRowGlyph(ARow: Integer): TERDGlyph;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    Result := FRows[I].Glyph
  else
    Result := glNone;
end;

procedure TERDDataGrid.SetRowGlyph(ARow: Integer; AValue: TERDGlyph);
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    FRows[I].Glyph := AValue;
  Invalidate;
end;

function TERDDataGrid.GetRowEnabled(ARow: Integer): Boolean;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  Result := (I >= 0) and FRows[I].Enabled;
end;

procedure TERDDataGrid.SetRowEnabled(ARow: Integer; AValue: Boolean);
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    FRows[I].Enabled := AValue;
  Invalidate;
end;

function TERDDataGrid.GetRowAccent(ARow: Integer): Boolean;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  Result := (I >= 0) and FRows[I].Accent;
end;

procedure TERDDataGrid.SetRowAccent(ARow: Integer; AValue: Boolean);
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if I >= 0 then
    FRows[I].Accent := AValue;
  Invalidate;
end;

function TERDDataGrid.GetCellTone(ACol, ARow: Integer): TERDStatusKind;
var
  I: Integer;
begin
  I := DataEntryIndex(ARow);
  if (I >= 0) and (ACol >= 0) and (ACol < Length(FRows[I].Tones)) then
    Result := FRows[I].Tones[ACol]
  else
    Result := skNeutral;
end;

procedure TERDDataGrid.SetCellTone(ACol, ARow: Integer; AValue: TERDStatusKind);
var
  I: Integer;
  E: TGridEntry;
begin
  I := DataEntryIndex(ARow);
  if (I < 0) or (ACol < 0) then
    Exit;
  E := FRows[I];
  EnsureCellArrays(E, ACol);
  E.Tones[ACol] := AValue;
  FRows[I] := E;
  Invalidate;
end;

procedure TERDDataGrid.ToggleCheck(ARow, ACol: Integer);
begin
  RowChecked[ARow] := not RowChecked[ARow];
  if Assigned(FOnCheckClick) then
    FOnCheckClick(Self, ARow, ACol);
end;

procedure TERDDataGrid.SetColumnWidth(ACol, AWidth: Integer);
begin
  if (ACol >= 0) and (ACol < FDataColumns.Count) then
    FDataColumns[ACol].Width := AWidth;
end;

function TERDDataGrid.EmptyActionHitTest(X, Y: Integer): Boolean;
begin
  Result := EmptyActionHit(Point(X, Y));
end;

function TERDDataGrid.EmptyActionHit(const P: TPoint): Boolean;
begin
  Result := (FEmptyAction <> '') and (RowCount = 0) and not UsePreviewData and
    not FEmptyActionRect.IsEmpty and PtInRect(FEmptyActionRect, P);
end;

function TERDDataGrid.UsePreviewData: Boolean;
begin
  Result := IsPreview and (RowCount = 0);
end;

function TERDDataGrid.PreviewIsGroup(AIndex: Integer): Boolean;
begin
  Result := AIndex in [0, 4];
end;

function TERDDataGrid.PreviewCell(ACol, ARow: Integer): string;
begin
  Result := '';
  case ARow of
    1: case ACol of 0: Result := 'Dyno baseline'; 1: Result := '2026-10-12';
      2: Result := 'PASS'; 3: Result := '173.4'; else Result := 'Stock map'; end;
    2: case ACol of 0: Result := 'Stage 1 pull'; 1: Result := '2026-10-12';
      2: Result := 'PASS'; 3: Result := '205.8'; else Result := 'Corrected'; end;
    3: case ACol of 0: Result := 'Aborted run'; 1: Result := '2026-10-12';
      2: Result := 'ABORT'; 3: Result := '80.3'; else Result := 'Wheel slip'; end;
    5: case ACol of 0: Result := '0-100 km/h'; 1: Result := '2026-10-11';
      2: Result := 'BEST'; 3: Result := '6.2'; else Result := 'Dry road'; end;
    6: case ACol of 0: Result := 'Quarter mile'; 1: Result := '2026-10-11';
      2: Result := 'PASS'; 3: Result := '14.1'; else Result := '102 mph'; end;
  end;
end;

function TERDDataGrid.VisibleEntry(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FVisible)) then
    Result := FVisible[AIndex]
  else
    Result := -1;
end;

function TERDDataGrid.VisibleIsGroup(AIndex: Integer): Boolean;
var
  I: Integer;
begin
  if UsePreviewData then
    Exit(PreviewIsGroup(AIndex));
  I := VisibleEntry(AIndex);
  Result := (I >= 0) and FRows[I].IsGroup;
end;

function TERDDataGrid.VisibleDataRow(AIndex: Integer): Integer;
begin
  if UsePreviewData then
  begin
    if PreviewIsGroup(AIndex) then
      Result := -1
    else
      Result := AIndex;
  end
  else
    Result := DataIndexOfEntry(VisibleEntry(AIndex));
end;

function TERDDataGrid.VisibleIndexOfRowID(ARowID: Integer): Integer;
var
  I, E: Integer;
begin
  Result := -1;
  for I := 0 to High(FVisible) do
  begin
    E := FVisible[I];
    if (E >= 0) and (E <= High(FRows)) and not FRows[E].IsGroup and
      (FRows[E].RowID = ARowID) then
      Exit(I);
  end;
end;

function TERDDataGrid.SelectedRowID: Integer;
var
  E: Integer;
begin
  Result := -1;
  E := VisibleEntry(ItemIndex);
  if (E >= 0) and (E <= High(FRows)) and not FRows[E].IsGroup then
    Result := FRows[E].RowID;
end;

function TERDDataGrid.GetSelectedRow: Integer;
begin
  if ItemIndex < 0 then
    Result := -1
  else
    Result := VisibleDataRow(ItemIndex);
end;

function TERDDataGrid.FirstCheckColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible and (FDataColumns[I].Kind = dckCheck) then
      Exit(I);
end;

procedure TERDDataGrid.RebuildVisible;
var
  I, N: Integer;
  Expanded: Boolean;
begin
  SetLength(FVisible, 0);
  Expanded := True;
  for I := 0 to High(FRows) do
  begin
    if FRows[I].IsGroup then
    begin
      N := Length(FVisible);
      SetLength(FVisible, N + 1);
      FVisible[N] := I;
      Expanded := FRows[I].Expanded;
    end
    else if Expanded then
    begin
      N := Length(FVisible);
      SetLength(FVisible, N + 1);
      FVisible[N] := I;
    end;
  end;
end;

function TERDDataGrid.GetItemCount: Integer;
begin
  if UsePreviewData then
    Result := 7
  else
    Result := Length(FVisible);
end;

function TERDDataGrid.GetItemText(AIndex: Integer): string;
var
  E, C: Integer;
begin
  Result := '';
  if UsePreviewData then
  begin
    if PreviewIsGroup(AIndex) then
      Result := 'Runs'
    else
      Result := PreviewCell(0, AIndex);
    Exit;
  end;
  E := VisibleEntry(AIndex);
  if E < 0 then
    Exit;
  if FRows[E].IsGroup then
    Result := FRows[E].Caption
  else
    for C := 0 to FDataColumns.Count - 1 do
      if (C < Length(FRows[E].Values)) and (FRows[E].Values[C] <> '') then
        Exit(FRows[E].Values[C]);
end;

function TERDDataGrid.ItemHeight: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(System.Math.Max(44, Metrics.CompactRow))
  else
    Result := ScaleValue(Metrics.CompactRow + 4);
end;

function TERDDataGrid.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TERDDataGrid.FillColumnIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible and (FDataColumns[I].Width = 0) then
      Exit(I);
  for I := FDataColumns.Count - 1 downto 0 do
    if FDataColumns[I].Visible then
      Exit(I);
end;

function TERDDataGrid.ColumnWidthPx(AColumn: Integer; const ABounds: TRect): Integer;
var
  I, FillIndex, FixedTotal: Integer;
begin
  Result := 0;
  if (AColumn < 0) or (AColumn >= FDataColumns.Count) or
    not FDataColumns[AColumn].Visible then
    Exit;
  FillIndex := FillColumnIndex;
  if AColumn <> FillIndex then
    Exit(ScaleValue(FDataColumns[AColumn].Width));
  FixedTotal := 0;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible and (I <> FillIndex) then
      Inc(FixedTotal, ScaleValue(FDataColumns[I].Width));
  Result := System.Math.Max(ScaleValue(FDataColumns[AColumn].MinWidth),
    ABounds.Width - FixedTotal);
end;

function TERDDataGrid.ColumnRect(AColumn: Integer; const ABounds: TRect): TRect;
var
  I, X: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  X := ABounds.Left;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible then
    begin
      if I = AColumn then
        Exit(Rect(X, ABounds.Top, X + ColumnWidthPx(I, ABounds), ABounds.Bottom));
      Inc(X, ColumnWidthPx(I, ABounds));
    end;
end;

function TERDDataGrid.ColumnAtX(X: Integer; const ABounds: TRect): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible then
    begin
      R := ColumnRect(I, ABounds);
      if (X >= R.Left) and (X < R.Right) then
        Exit(I);
    end;
end;

function TERDDataGrid.ColumnDividerAtX(X: Integer; const ABounds: TRect): Integer;
var
  I, LastVisible, Slop: Integer;
  R: TRect;
begin
  Result := -1;
  LastVisible := -1;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible then
      LastVisible := I;
  Slop := ScaleValue(RESIZE_HIT_ZONE);
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible and (I <> LastVisible) then
    begin
      R := ColumnRect(I, ABounds);
      if Abs(X - R.Right) <= Slop then
        Exit(I);
    end;
end;

function TERDDataGrid.CurrentColumnWidth96(AColumn: Integer): Integer;
var
  R: TRect;
begin
  R := ColumnRect(AColumn, HeaderBounds);
  if R.Width <= 0 then
    Result := FDataColumns[AColumn].MinWidth
  else
    Result := System.Math.Max(FDataColumns[AColumn].MinWidth,
      MulDiv(R.Width, 96, ScaleValue(96)));
end;

function TERDDataGrid.HeaderBounds: TRect;
var
  F: Integer;
begin
  if BorderVisible then
    F := 1
  else
    F := 0;
  Result := Rect(F, F, Width - F, F + HeaderHeight);
end;

function TERDDataGrid.EffectiveAlignment(AColumn: TERDDataGridColumn): TAlignment;
begin
  Result := AColumn.Alignment;
  if (AColumn.Kind = dckNumber) and (Result = taLeftJustify) then
    Result := taRightJustify;
end;

procedure TERDDataGrid.DrawHeader(APainter: TERDPainter; const ARect: TRect);
var
  I, CY, X, MaxW: Integer;
  R: TRect;
  Align: TAlignment;
  Color: TColor;
begin
  CY := (ARect.Top + ARect.Bottom) div 2;
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible then
    begin
      R := ColumnRect(I, ARect);
      Align := EffectiveAlignment(FDataColumns[I]);
      if Align = taRightJustify then
        X := R.Right - ScaleValue(8)
      else if Align = taCenter then
        X := (R.Left + R.Right) div 2
      else
        X := R.Left + ScaleValue(8);
      MaxW := System.Math.Max(1, R.Width - ScaleValue(18));
      if I = FSortColumn then
        Color := APainter.AccentText
      else
        Color := clDefault;
      APainter.Caps(X, CY, FDataColumns[I].Caption, Align, Color, MaxW);
      if I = FSortColumn then
        if FSortDescending then
          APainter.Glyph(glChevronDown, R.Right - ScaleValue(9), CY,
            APainter.AccentText, 0.65)
        else
          APainter.Glyph(glChevronUp, R.Right - ScaleValue(9), CY,
            APainter.AccentText, 0.65);
      if I <> FillColumnIndex then
        APainter.VLine(R.Right, ARect.Top + ScaleValue(6),
          ARect.Height - ScaleValue(12), Palette.NeutralLight);
    end;
end;

procedure TERDDataGrid.HeaderClick(X, Y: Integer);
var
  C: Integer;
begin
  C := ColumnDividerAtX(X, HeaderBounds);
  if C >= 0 then
  begin
    FResizeColumn := C;
    FResizeStartX := X;
    FResizeStartWidth := CurrentColumnWidth96(C);
    MouseCapture := True;
    Cursor := crSizeWE;
    Exit;
  end;
  C := ColumnAtX(X, HeaderBounds);
  if (C < 0) or not FDataColumns[C].Sortable then
    Exit;
  if FSortColumn = C then
    SortDescending := not FSortDescending
  else
  begin
    FSortDescending := False;
    SetSortColumn(C);
  end;
end;

procedure TERDDataGrid.HeaderMouseMove(X, Y: Integer);
begin
  if FResizeColumn >= 0 then
    Exit;
  if (X >= 0) and (ColumnDividerAtX(X, HeaderBounds) >= 0) then
    Cursor := crSizeWE
  else
    Cursor := crDefault;
end;

procedure TERDDataGrid.DrawGroup(APainter: TERDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  Entry, Count, CY, X: Integer;
  S: string;
  Down: Boolean;
begin
  CY := (ARect.Top + ARect.Bottom) div 2;
  APainter.FillRect(ARect, APainter.HeaderFill);
  APainter.HLine(ARect.Left, ARect.Bottom - 1, ARect.Width, Palette.NeutralLight);
  if UsePreviewData then
  begin
    Down := True;
    if AIndex = 0 then
      S := 'Dyno runs (3)'
    else
      S := 'Drag runs (2)';
  end
  else
  begin
    Entry := VisibleEntry(AIndex);
    Count := GroupRowCount(Entry);
    Down := FRows[Entry].Expanded;
    S := FRows[Entry].Caption + Format(' (%d)', [Count]);
  end;
  X := ARect.Left + ScaleValue(12);
  APainter.GlyphChevron(X, CY, Palette.Subtle, Down);
  APainter.Text(X + ScaleValue(18), CY, S, ERD_TEXT_BODY,
    Palette.ForegroundText, twSemibold, taLeftJustify,
    System.Math.Max(1, ARect.Width - ScaleValue(36)));
end;

procedure TERDDataGrid.DrawDataRow(APainter: TERDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  EntryIndex, I, CY, Pad, Box, ChipW, TextX, MaxW: Integer;
  E: TGridEntry;
  R: TRect;
  Column: TERDDataGridColumn;
  TextColor, ToneColor: TColor;
  State: TCheckBoxState;
  Align: TAlignment;
  Value: string;
begin
  CY := (ARect.Top + ARect.Bottom) div 2;
  Pad := ScaleValue(8);
  if UsePreviewData then
  begin
    E := Default(TGridEntry);
    E.Enabled := AIndex <> 3;
    E.Glyph := glInfo;
    E.Accent := AIndex = 2;
    E.Checked := AIndex in [1, 2, 5];
    SetLength(E.Tones, FDataColumns.Count);
    for I := 0 to High(E.Tones) do
      E.Tones[I] := skNeutral;
    if Length(E.Tones) > 2 then
    begin
      if AIndex = 3 then
        E.Tones[2] := skDanger
      else if AIndex = 5 then
        E.Tones[2] := skAccent
      else
        E.Tones[2] := skSuccess;
    end;
  end
  else
  begin
    EntryIndex := VisibleEntry(AIndex);
    if EntryIndex < 0 then
      Exit;
    E := FRows[EntryIndex];
  end;
  if E.Enabled and Enabled then
    TextColor := Palette.ForegroundText
  else
    TextColor := APainter.DisabledText;
  if E.Accent or ASelected then
    APainter.FillRect(Rect(ARect.Left, ARect.Top + ScaleValue(5),
      ARect.Left + ScaleValue(3), ARect.Bottom - ScaleValue(5)), Palette.Accent);
  for I := 0 to FDataColumns.Count - 1 do
    if FDataColumns[I].Visible then
    begin
      Column := FDataColumns[I];
      R := ColumnRect(I, ARect);
      R.Left := R.Left + Pad;
      R.Right := R.Right - Pad;
      if R.Right <= R.Left then
        Continue;
      if UsePreviewData then
        Value := PreviewCell(I, AIndex)
      else if I < Length(E.Values) then
        Value := E.Values[I]
      else
        Value := '';
      case Column.Kind of
        dckChip:
          begin
            if I < Length(E.Tones) then
              ToneColor := APainter.StatusColor(E.Tones[I])
            else
              ToneColor := APainter.StatusColor(skNeutral);
            ChipW := APainter.ChipWidth(Value);
            if EffectiveAlignment(Column) = taRightJustify then
              TextX := R.Right - ChipW
            else if EffectiveAlignment(Column) = taCenter then
              TextX := R.Left + (R.Width - ChipW) div 2
            else
              TextX := R.Left;
            APainter.Chip(TextX, CY - ScaleValue(10), Value, ToneColor);
          end;
        dckCheck:
          begin
            Box := Metrics.Check;
            if E.Checked then
              State := cbChecked
            else
              State := cbUnchecked;
            APainter.CheckBox((R.Left + R.Right - ScaleValue(Box)) div 2,
              CY, Box, State, E.Enabled and Enabled, AHot, False);
          end;
        dckIcon:
          if E.Glyph <> glNone then
            APainter.Glyph(E.Glyph, (R.Left + R.Right) / 2, CY, TextColor, 0.9);
      else
        Align := EffectiveAlignment(Column);
        MaxW := System.Math.Max(1, R.Width);
        if Align = taRightJustify then
          TextX := R.Right
        else if Align = taCenter then
          TextX := (R.Left + R.Right) div 2
        else
          TextX := R.Left;
        if Column.Kind = dckNumber then
          APainter.Text(TextX, CY, Value, ERD_TEXT_BODY, TextColor,
            twMonoBold, Align, MaxW)
        else if Column.Kind = dckMono then
          APainter.Text(TextX, CY, Value, ERD_TEXT_BODY, TextColor,
            twMono, Align, MaxW)
        else
          APainter.Text(TextX, CY, Value, ERD_TEXT_BODY, TextColor,
            twRegular, Align, MaxW);
      end;
    end;
end;

procedure TERDDataGrid.DrawItem(APainter: TERDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
begin
  if VisibleIsGroup(AIndex) then
    DrawGroup(APainter, AIndex, ARect, ASelected, AHot)
  else
    DrawDataRow(APainter, AIndex, ARect, ASelected, AHot);
end;

procedure TERDDataGrid.DrawEmptyState(APainter: TERDPainter);
var
  CR: TRect;
  Y, ButtonW, ButtonH: Integer;
begin
  FEmptyActionRect := Rect(0, 0, 0, 0);
  if RowCount <> 0 then
    Exit;
  CR := ContentRect;
  if CR.IsEmpty then
    Exit;
  Y := CR.Top + CR.Height div 2 - ScaleValue(18);
  if FEmptyGlyph <> glNone then
  begin
    APainter.Glyph(FEmptyGlyph, (CR.Left + CR.Right) / 2, Y - ScaleValue(26),
      APainter.AccentText, 1.35);
    Inc(Y, ScaleValue(6));
  end;
  APainter.Text((CR.Left + CR.Right) div 2, Y, FEmptyTitle,
    ERD_TEXT_HEADING, Palette.ForegroundText, twSemibold, taCenter,
    System.Math.Max(1, CR.Width - ScaleValue(32)));
  if FEmptyHint <> '' then
    APainter.Text((CR.Left + CR.Right) div 2, Y + ScaleValue(24), FEmptyHint,
      ERD_TEXT_BODY, Palette.GaugeLabel, twRegular, taCenter,
      System.Math.Max(1, CR.Width - ScaleValue(32)));
  if FEmptyAction <> '' then
  begin
    ButtonW := APainter.ButtonWidth(FEmptyAction, glNone);
    ButtonH := ScaleValue(Metrics.Button);
    FEmptyActionRect := Rect((CR.Left + CR.Right - ButtonW) div 2,
      Y + ScaleValue(54), (CR.Left + CR.Right + ButtonW) div 2,
      Y + ScaleValue(54) + ButtonH);
    APainter.Button(FEmptyActionRect, FEmptyAction, bkSecondary, cstNormal,
      False, glNone);
  end;
end;

procedure TERDDataGrid.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
begin
  inherited PaintControl(ACanvas);
  if (RowCount = 0) and not UsePreviewData then
  begin
    P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
    try
      DrawEmptyState(P);
    finally
      P.Free;
    end;
  end;
end;

procedure TERDDataGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index, Entry, Row, Col, Group: Integer;
begin
  if (Button = mbLeft) and EmptyActionHit(Point(X, Y)) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    if Assigned(FOnEmptyAction) then
      FOnEmptyAction(Self);
    Exit;
  end;
  if (HeaderHeight > 0) and (Y < HeaderHeight + Ord(BorderVisible)) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  Index := ItemAt(X, Y);
  if (Button = mbLeft) and (Index >= 0) then
  begin
    if VisibleIsGroup(Index) then
    begin
      if not UsePreviewData then
      begin
        Entry := VisibleEntry(Index);
        Group := GroupIndexOfEntry(Entry);
        GroupExpanded[Group] := not GroupExpanded[Group];
      end;
      Exit;
    end;
    Row := VisibleDataRow(Index);
    Col := ColumnAtX(X, ItemRect(Index));
    if (Row >= 0) and (Col >= 0) and (Col < FDataColumns.Count) and
      (FDataColumns[Col].Kind = dckCheck) then
    begin
      SelectByUser(Index, Shift);
      ToggleCheck(Row, Col);
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TERDDataGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta, NewWidth: Integer;
begin
  if FResizeColumn >= 0 then
  begin
    Delta := MulDiv(X - FResizeStartX, 96, ScaleValue(96));
    NewWidth := System.Math.Max(FDataColumns[FResizeColumn].MinWidth,
      FResizeStartWidth + Delta);
    FDataColumns[FResizeColumn].Width := NewWidth;
    Exit;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TERDDataGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FResizeColumn >= 0 then
  begin
    FResizeColumn := -1;
    MouseCapture := False;
    HeaderMouseMove(X, Y);
  end;
end;

procedure TERDDataGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cur, N, Target, Page, CheckCol, I: Integer;

  function IsSelectable(AIndex: Integer): Boolean;
  begin
    Result := (AIndex >= 0) and (AIndex < N) and not VisibleIsGroup(AIndex);
  end;

  function FindFrom(AStart, ADirection: Integer): Integer;
  begin
    Result := EnsureRange(AStart, 0, N - 1);
    while (Result >= 0) and (Result < N) and not IsSelectable(Result) do
      Inc(Result, ADirection);
    if (Result < 0) or (Result >= N) then
      Result := -1;
  end;

begin
  N := GetItemCount;
  if N = 0 then
  begin
    inherited;
    Exit;
  end;
  Cur := ItemIndex;
  Page := System.Math.Max(1, ContentRect.Height div System.Math.Max(1, ItemHeight));
  case Key of
    VK_UP: if Cur < 0 then Target := FindFrom(0, 1)
      else Target := FindFrom(Cur - 1, -1);
    VK_DOWN: if Cur < 0 then Target := FindFrom(0, 1)
      else Target := FindFrom(Cur + 1, 1);
    VK_PRIOR: Target := FindFrom(System.Math.Max(0, Cur - Page), -1);
    VK_NEXT: Target := FindFrom(System.Math.Min(N - 1,
      System.Math.Max(0, Cur) + Page), 1);
    VK_HOME: Target := FindFrom(0, 1);
    VK_END: Target := FindFrom(N - 1, -1);
    VK_SPACE:
      begin
        CheckCol := FirstCheckColumn;
        if (Cur >= 0) and (CheckCol >= 0) and IsSelectable(Cur) then
        begin
          ToggleCheck(VisibleDataRow(Cur), CheckCol);
          Key := 0;
        end;
        Exit;
      end;
    VK_RETURN:
      begin
        if (Cur >= 0) and IsSelectable(Cur) then
        begin
          DoItemActivate;
          Key := 0;
        end;
        Exit;
      end;
    Ord('A'):
      begin
        if MultiSelect and (ssCtrl in Shift) then
        begin
          for I := 0 to N - 1 do
            if IsSelectable(I) then
              Selected[I] := True;
          Key := 0;
        end;
        Exit;
      end;
  else
    inherited;
    Exit;
  end;
  if Target >= 0 then
  begin
    SelectByUser(Target, Shift * [ssShift]);
    Key := 0;
  end;
end;

function CleanNumberText(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if not CharInSet(S[I], [' ', ',', #39]) then
      Result := Result + S[I];
end;

function TERDDataGrid.CompareEntries(const A, B: TGridEntry): Integer;
var
  SA, SB: string;
  DA, DB: Double;
  Fmt: TFormatSettings;
begin
  Result := 0;
  if (FSortColumn < 0) or (FSortColumn >= FDataColumns.Count) then
    Exit;
  if FSortColumn < Length(A.Values) then SA := A.Values[FSortColumn] else SA := '';
  if FSortColumn < Length(B.Values) then SB := B.Values[FSortColumn] else SB := '';
  if FDataColumns[FSortColumn].Kind = dckNumber then
  begin
    Fmt := TFormatSettings.Invariant;
    if TryStrToFloat(CleanNumberText(SA), DA, Fmt) and
      TryStrToFloat(CleanNumberText(SB), DB, Fmt) then
    begin
      if DA < DB then Result := -1
      else if DA > DB then Result := 1
      else Result := 0;
    end
    else
      Result := CompareText(SA, SB);
  end
  else
    Result := CompareText(SA, SB);
  if FSortDescending then
    Result := -Result;
end;

procedure TERDDataGrid.SortRows;
var
  Start, Stop: Integer;

  procedure SortRange(ALo, AHi: Integer);
  var
    I, J: Integer;
    T: TGridEntry;
  begin
    for I := ALo to AHi - 1 do
      for J := I + 1 to AHi do
        if CompareEntries(FRows[I], FRows[J]) > 0 then
        begin
          T := FRows[I];
          FRows[I] := FRows[J];
          FRows[J] := T;
        end;
  end;

begin
  if (FSortColumn < 0) or (FSortColumn >= FDataColumns.Count) then
    Exit;
  Start := 0;
  while Start <= High(FRows) do
  begin
    while (Start <= High(FRows)) and FRows[Start].IsGroup do
      Inc(Start);
    Stop := Start;
    while (Stop <= High(FRows)) and not FRows[Stop].IsGroup do
      Inc(Stop);
    if Stop > Start + 1 then
      SortRange(Start, Stop - 1);
    Start := Stop;
  end;
end;

end.
