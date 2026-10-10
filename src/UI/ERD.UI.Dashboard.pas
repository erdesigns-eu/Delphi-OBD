//------------------------------------------------------------------------------
//  ERD.UI.Dashboard
//
//  TOBDDashboard - a grid of tiles that hosts the dashboard controls.
//
//  - The grid has Columns x Rows cells with a Gap between them; each
//    tile covers ColSpan x RowSpan cells and resizes with the
//    dashboard.
//  - At design time controls dropped on the dashboard become tiles
//    and snap to the cell under them.
//  - At run time EditMode lets the mechanic drag tiles to move them,
//    drag the bottom-right grip to resize and click the red cross to
//    remove; clicking an empty cell fires OnEmptyCellClick so the
//    host can offer "add gauge here".
//  - AddTile creates a control by kind name ('dial', 'bar', 'value',
//    'trend', 'lamp', 'matrix', 'grid'); RegisterTileKind adds more.
//  - SaveLayout / LoadLayout store the grid, the tiles and each
//    tile's settings as JSON, so layouts can be kept per vehicle or
//    per job type.
//  - LiveData is handed to every tile, so one assignment wires the
//    whole dashboard.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.Dashboard;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.Service.LiveData;

const
  /// <summary>Version written to saved layouts.</summary>
  OBD_DASHBOARD_LAYOUT_VERSION = 1;

  /// <summary>Posted to the dashboard to free tiles removed in edit
  /// mode once the tile's own message handling has finished.</summary>
  CM_OBD_REMOVE_PENDING = CM_BASE + 412;

type
  /// <summary>Class of a control that can be a tile.</summary>
  TOBDTileClass = class of TOBDCustomControl;

  /// <summary>Raised for invalid layouts and unknown tile kinds.
  /// </summary>
  EOBDDashboard = class(Exception);

  /// <summary>Fires when an empty cell is clicked in edit mode.
  /// </summary>
  /// <param name="Sender">The dashboard.</param>
  /// <param name="ACol">Cell column.</param>
  /// <param name="ARow">Cell row.</param>
  TOBDDashboardCellEvent = procedure(Sender: TObject;
    ACol, ARow: Integer) of object;

  TOBDDashboard = class;

  /// <summary>Placement of one control in the grid.</summary>
  TOBDDashboardTile = class(TCollectionItem)
  strict private
    FControl: TOBDCustomControl;
    FCol: Integer;
    FRow: Integer;
    FColSpan: Integer;
    FRowSpan: Integer;
    procedure SetControl(AValue: TOBDCustomControl);
    procedure SetCol(AValue: Integer);
    procedure SetRow(AValue: Integer);
    procedure SetColSpan(AValue: Integer);
    procedure SetRowSpan(AValue: Integer);
    function Dashboard: TOBDDashboard;
  protected
    /// <summary>Name of the control in the collection editor.
    /// </summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a 1x1 tile at cell (0, 0).</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies the placement and control reference.</summary>
    /// <param name="ASource">Another tile.</param>
    procedure Assign(ASource: TPersistent); override;
    /// <summary>Sets position and size in one step.</summary>
    /// <param name="ACol">Column.</param>
    /// <param name="ARow">Row.</param>
    /// <param name="AColSpan">Columns covered.</param>
    /// <param name="ARowSpan">Rows covered.</param>
    procedure SetCells(ACol, ARow, AColSpan, ARowSpan: Integer);
  published
    /// <summary>Control shown in this tile.</summary>
    property Control: TOBDCustomControl read FControl write SetControl;
    /// <summary>Left cell (0-based).</summary>
    property Col: Integer read FCol write SetCol default 0;
    /// <summary>Top cell (0-based).</summary>
    property Row: Integer read FRow write SetRow default 0;
    /// <summary>Columns covered.</summary>
    property ColSpan: Integer read FColSpan write SetColSpan default 1;
    /// <summary>Rows covered.</summary>
    property RowSpan: Integer read FRowSpan write SetRowSpan default 1;
  end;

  /// <summary>Collection of <see cref="TOBDDashboardTile"/>.</summary>
  TOBDDashboardTiles = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDDashboardTile;
  protected
    /// <summary>Re-lays out the dashboard after a change.</summary>
    /// <param name="Item">Changed item or nil.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a tile.</summary>
    /// <returns>The new tile.</returns>
    function Add: TOBDDashboardTile;
    /// <summary>Tile showing a control.</summary>
    /// <param name="AControl">Control to look for.</param>
    /// <returns>The tile or nil.</returns>
    function FindControl(AControl: TControl): TOBDDashboardTile;
    /// <summary>Tile by index.</summary>
    property Items[AIndex: Integer]: TOBDDashboardTile read GetItem; default;
  end;

  /// <summary>Grid container for dashboard controls.</summary>
  TOBDDashboard = class(TOBDCustomControl, IOBDTileHost)
  strict private
    class var FKinds: TDictionary<string, TOBDTileClass>;
  strict private
    FTiles: TOBDDashboardTiles;
    FColumns: Integer;
    FRows: Integer;
    FGap: Integer;
    FEditMode: Boolean;
    FLiveData: TOBDLiveData;
    FArranging: Integer;
    FDragTile: TOBDDashboardTile;
    FDragHit: TOBDTileHit;
    FDragStart: TPoint;
    FDragOrigin: TRect;
    FPendingRemove: TList<TControl>;
    FOnLayoutChanged: TNotifyEvent;
    FOnEmptyCellClick: TOBDDashboardCellEvent;
    FOnEditModeChanged: TNotifyEvent;
    procedure SetTiles(AValue: TOBDDashboardTiles);
    procedure SetColumns(AValue: Integer);
    procedure SetRows(AValue: Integer);
    procedure SetGap(AValue: Integer);
    procedure SetEditMode(AValue: Boolean);
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure AdoptControl(AControl: TOBDCustomControl);
    procedure SnapFromBounds(ATile: TOBDDashboardTile);
    procedure RepaintTiles;
    procedure CMControlListChange(var Message: TCMControlListChange);
      message CM_CONTROLLISTCHANGE;
    procedure CMRemovePending(var Message: TMessage); message CM_OBD_REMOVE_PENDING;
    class procedure EnsureKinds;
  protected
    /// <summary>Lets controls be dropped on the dashboard.</summary>
    /// <param name="Params">Window parameters.</param>
    procedure CreateParams(var Params: TCreateParams); override;
    /// <summary>Adopts streamed children and lays out.</summary>
    procedure Loaded; override;
    /// <summary>Drops references to freed components.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Snaps tiles to their cells whenever a child moves or
    /// the dashboard resizes.</summary>
    /// <param name="AControl">Child that changed, nil for all.</param>
    /// <param name="Rect">Client area.</param>
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    /// <summary>Paints the background and, in edit mode or when
    /// empty, the cell grid.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Empty-cell clicks in edit mode.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>IOBDTileHost: edit-mode flag.</summary>
    /// <returns>True in edit mode.</returns>
    function IsEditingTiles: Boolean;
    /// <summary>IOBDTileHost: drag / resize / remove handling.</summary>
    /// <param name="ATile">Tile control.</param>
    /// <param name="AMessage">Mouse message.</param>
    procedure TileMouseMessage(ATile: TControl; var AMessage: TMessage);
  public
    /// <summary>Creates a 4x3 dashboard.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Releases the tile list.</summary>
    destructor Destroy; override;
    /// <summary>Releases the kind registry.</summary>
    class destructor DestroyKinds;
    /// <summary>Registers a tile kind for <see cref="AddTile"/> and
    /// layouts.</summary>
    /// <param name="AKind">Kind name, e.g. <c>'dial'</c>.</param>
    /// <param name="AClass">Control class.</param>
    class procedure RegisterTileKind(const AKind: string;
      AClass: TOBDTileClass);
    /// <summary>Kind name of a control class.</summary>
    /// <param name="AClass">Control class.</param>
    /// <returns>Registered kind or the class name.</returns>
    class function KindOf(AClass: TClass): string;
    /// <summary>Registered kind names.</summary>
    /// <returns>Sorted names.</returns>
    class function TileKinds: TArray<string>;
    /// <summary>Pixel rectangle of a cell range.</summary>
    /// <param name="ACol">Left column.</param>
    /// <param name="ARow">Top row.</param>
    /// <param name="AColSpan">Columns covered.</param>
    /// <param name="ARowSpan">Rows covered.</param>
    /// <returns>Rectangle in client coordinates.</returns>
    function CellRect(ACol, ARow, AColSpan, ARowSpan: Integer): TRect;
    /// <summary>Cell under a client point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <param name="ACol">Column, -1 outside.</param>
    /// <param name="ARow">Row, -1 outside.</param>
    procedure CellAt(X, Y: Integer; out ACol, ARow: Integer);
    /// <summary>Whether a cell range is inside the grid and not used
    /// by another tile.</summary>
    /// <param name="ACol">Left column.</param>
    /// <param name="ARow">Top row.</param>
    /// <param name="AColSpan">Columns covered.</param>
    /// <param name="ARowSpan">Rows covered.</param>
    /// <param name="AIgnore">Tile to ignore (the one being moved).
    /// </param>
    /// <returns>True when free.</returns>
    function IsFree(ACol, ARow, AColSpan, ARowSpan: Integer;
      AIgnore: TOBDDashboardTile = nil): Boolean;
    /// <summary>First free cell range, scanning row by row.</summary>
    /// <param name="AColSpan">Columns needed.</param>
    /// <param name="ARowSpan">Rows needed.</param>
    /// <param name="ACol">Found column.</param>
    /// <param name="ARow">Found row.</param>
    /// <returns>False when the grid is full.</returns>
    function FindFree(AColSpan, ARowSpan: Integer;
      out ACol, ARow: Integer): Boolean;
    /// <summary>Creates a control of a registered kind as a tile.
    /// </summary>
    /// <param name="AKind">Kind name.</param>
    /// <param name="ACol">Column; -1 = first free.</param>
    /// <param name="ARow">Row; -1 = first free.</param>
    /// <param name="AColSpan">Columns covered.</param>
    /// <param name="ARowSpan">Rows covered.</param>
    /// <returns>The new control (owned by the dashboard).</returns>
    /// <exception cref="EOBDDashboard">Unknown kind.</exception>
    function AddTile(const AKind: string; ACol: Integer = -1;
      ARow: Integer = -1; AColSpan: Integer = 1;
      ARowSpan: Integer = 1): TOBDCustomControl;
    /// <summary>Places an existing control as a tile.</summary>
    /// <param name="AControl">Control; its parent becomes the
    /// dashboard.</param>
    /// <param name="ACol">Column; -1 = first free.</param>
    /// <param name="ARow">Row; -1 = first free.</param>
    /// <param name="AColSpan">Columns covered.</param>
    /// <param name="ARowSpan">Rows covered.</param>
    /// <returns>The tile.</returns>
    function PlaceControl(AControl: TOBDCustomControl; ACol: Integer = -1;
      ARow: Integer = -1; AColSpan: Integer = 1;
      ARowSpan: Integer = 1): TOBDDashboardTile;
    /// <summary>Removes a tile and frees its control.</summary>
    /// <param name="AControl">Tile control.</param>
    procedure RemoveTile(AControl: TControl);
    /// <summary>Removes every tile and frees the controls.</summary>
    procedure ClearTiles;
    /// <summary>Positions every tile control in its cells.</summary>
    procedure ArrangeTiles;
    /// <summary>Layout as JSON text.</summary>
    /// <returns>JSON document.</returns>
    function SaveLayout: string;
    /// <summary>Replaces all tiles with a JSON layout. Tiles of an
    /// unknown kind are skipped.</summary>
    /// <param name="AJson">JSON document from
    /// <see cref="SaveLayout"/>.</param>
    /// <exception cref="EOBDDashboard">Not a dashboard layout.
    /// </exception>
    procedure LoadLayout(const AJson: string);
    /// <summary>Writes the layout to a UTF-8 file.</summary>
    /// <param name="AFileName">Target file.</param>
    procedure SaveLayoutToFile(const AFileName: string);
    /// <summary>Reads a layout from a UTF-8 file.</summary>
    /// <param name="AFileName">Source file.</param>
    procedure LoadLayoutFromFile(const AFileName: string);
    /// <summary>Raises <see cref="OnLayoutChanged"/>.</summary>
    procedure LayoutChanged;
  published
    /// <summary>Tiles in the grid.</summary>
    property Tiles: TOBDDashboardTiles read FTiles write SetTiles;
    /// <summary>Number of grid columns.</summary>
    property Columns: Integer read FColumns write SetColumns default 4;
    /// <summary>Number of grid rows.</summary>
    property Rows: Integer read FRows write SetRows default 3;
    /// <summary>Gap between tiles in pixels at 96 DPI.</summary>
    property Gap: Integer read FGap write SetGap default 8;
    /// <summary>Lets the user move, resize and remove tiles.</summary>
    property EditMode: Boolean read FEditMode write SetEditMode
      default False;
    /// <summary>Data source handed to every tile.</summary>
    property LiveData: TOBDLiveData read FLiveData write SetLiveData;
    /// <summary>Fires after tiles were added, moved, resized or
    /// removed, or a layout was loaded.</summary>
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged
      write FOnLayoutChanged;
    /// <summary>Fires when an empty cell is clicked in edit mode.
    /// </summary>
    property OnEmptyCellClick: TOBDDashboardCellEvent read FOnEmptyCellClick
      write FOnEmptyCellClick;
    /// <summary>Fires when <see cref="EditMode"/> changes.</summary>
    property OnEditModeChanged: TNotifyEvent read FOnEditModeChanged
      write FOnEditModeChanged;
  end;

implementation

uses
  ERD.JSON,
  ERD.UI.Gauges.Dial,
  ERD.UI.Gauges.Bar,
  ERD.UI.ValueTile,
  ERD.UI.TrendChart,
  ERD.UI.StatusLamp,
  ERD.UI.MatrixDisplay,
  ERD.UI.LiveDataGrid;

{ ---- TOBDDashboardTile ------------------------------------------------------- }

constructor TOBDDashboardTile.Create(ACollection: TCollection);
begin
  FColSpan := 1;
  FRowSpan := 1;
  inherited Create(ACollection);
end;

function TOBDDashboardTile.Dashboard: TOBDDashboard;
begin
  Result := nil;
  if (Collection is TOwnedCollection) and
    (TOwnedCollection(Collection).Owner is TOBDDashboard) then
    Result := TOBDDashboard(TOwnedCollection(Collection).Owner);
end;

function TOBDDashboardTile.GetDisplayName: string;
begin
  if (FControl <> nil) and (FControl.Name <> '') then
    Result := FControl.Name
  else if FControl <> nil then
    Result := FControl.ClassName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDDashboardTile.Assign(ASource: TPersistent);
var
  S: TOBDDashboardTile;
begin
  if ASource is TOBDDashboardTile then
  begin
    S := TOBDDashboardTile(ASource);
    Control := S.Control;
    SetCells(S.Col, S.Row, S.ColSpan, S.RowSpan);
  end
  else
    inherited Assign(ASource);
end;

procedure TOBDDashboardTile.SetControl(AValue: TOBDCustomControl);
var
  D: TOBDDashboard;
begin
  if FControl = AValue then
    Exit;
  D := Dashboard;
  if (FControl <> nil) and (D <> nil) then
    FControl.RemoveFreeNotification(D);
  FControl := AValue;
  if (FControl <> nil) and (D <> nil) then
    FControl.FreeNotification(D);
  Changed(False);
end;

procedure TOBDDashboardTile.SetCells(ACol, ARow, AColSpan, ARowSpan: Integer);
begin
  FCol := System.Math.Max(0, ACol);
  FRow := System.Math.Max(0, ARow);
  FColSpan := System.Math.Max(1, AColSpan);
  FRowSpan := System.Math.Max(1, ARowSpan);
  Changed(False);
end;

procedure TOBDDashboardTile.SetCol(AValue: Integer);
begin
  FCol := System.Math.Max(0, AValue);
  Changed(False);
end;

procedure TOBDDashboardTile.SetRow(AValue: Integer);
begin
  FRow := System.Math.Max(0, AValue);
  Changed(False);
end;

procedure TOBDDashboardTile.SetColSpan(AValue: Integer);
begin
  FColSpan := System.Math.Max(1, AValue);
  Changed(False);
end;

procedure TOBDDashboardTile.SetRowSpan(AValue: Integer);
begin
  FRowSpan := System.Math.Max(1, AValue);
  Changed(False);
end;

{ ---- TOBDDashboardTiles ------------------------------------------------------ }

function TOBDDashboardTiles.Add: TOBDDashboardTile;
begin
  Result := TOBDDashboardTile(inherited Add);
end;

function TOBDDashboardTiles.GetItem(AIndex: Integer): TOBDDashboardTile;
begin
  Result := TOBDDashboardTile(inherited Items[AIndex]);
end;

function TOBDDashboardTiles.FindControl(AControl: TControl): TOBDDashboardTile;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Control = AControl then
      Exit(Items[I]);
  Result := nil;
end;

procedure TOBDDashboardTiles.Update(Item: TCollectionItem);
begin
  inherited;
  if Owner is TOBDDashboard then
    TOBDDashboard(Owner).ArrangeTiles;
end;

{ ---- TOBDDashboard: kinds ---------------------------------------------------- }

class procedure TOBDDashboard.EnsureKinds;
begin
  if FKinds <> nil then
    Exit;
  FKinds := TDictionary<string, TOBDTileClass>.Create;
  FKinds.Add('dial', TOBDDialGauge);
  FKinds.Add('bar', TOBDBarGauge);
  FKinds.Add('value', TOBDValueTile);
  FKinds.Add('trend', TOBDTrendChart);
  FKinds.Add('lamp', TOBDStatusLamp);
  FKinds.Add('matrix', TOBDMatrixDisplay);
  FKinds.Add('grid', TOBDLiveDataGrid);
end;

class destructor TOBDDashboard.DestroyKinds;
begin
  FreeAndNil(FKinds);
end;

class procedure TOBDDashboard.RegisterTileKind(const AKind: string;
  AClass: TOBDTileClass);
begin
  EnsureKinds;
  FKinds.AddOrSetValue(LowerCase(AKind), AClass);
end;

class function TOBDDashboard.KindOf(AClass: TClass): string;
var
  Pair: TPair<string, TOBDTileClass>;
begin
  EnsureKinds;
  for Pair in FKinds do
    if Pair.Value = AClass then
      Exit(Pair.Key);
  Result := AClass.ClassName;
end;

class function TOBDDashboard.TileKinds: TArray<string>;
begin
  EnsureKinds;
  Result := FKinds.Keys.ToArray;
  TArray.Sort<string>(Result);
end;

{ ---- TOBDDashboard ----------------------------------------------------------- }

constructor TOBDDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FTiles := TOBDDashboardTiles.Create(Self, TOBDDashboardTile);
  FPendingRemove := TList<TControl>.Create;
  FColumns := 4;
  FRows := 3;
  FGap := 8;
  Width := 800;
  Height := 480;
end;

destructor TOBDDashboard.Destroy;
begin
  FEditMode := False;
  FreeAndNil(FTiles);
  FreeAndNil(FPendingRemove);
  inherited;
end;

procedure TOBDDashboard.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TOBDDashboard.Loaded;
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if (Controls[I] is TOBDCustomControl) and
      (FTiles.FindControl(Controls[I]) = nil) then
      AdoptControl(TOBDCustomControl(Controls[I]));
  if FLiveData <> nil then
    for I := 0 to FTiles.Count - 1 do
      if FTiles[I].Control <> nil then
        FTiles[I].Control.AssignDataSource(FLiveData);
  ArrangeTiles;
end;

procedure TOBDDashboard.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  T: TOBDDashboardTile;
begin
  inherited;
  if Operation <> opRemove then
    Exit;
  if AComponent = FLiveData then
    FLiveData := nil;
  if (FTiles <> nil) and (AComponent is TControl) then
  begin
    T := FTiles.FindControl(TControl(AComponent));
    if T <> nil then
    begin
      if FDragTile = T then
        FDragTile := nil;
      T.Free;
    end;
  end;
  if (FPendingRemove <> nil) and (AComponent is TControl) then
    FPendingRemove.Remove(TControl(AComponent));
end;

procedure TOBDDashboard.SetTiles(AValue: TOBDDashboardTiles);
begin
  FTiles.Assign(AValue);
end;

procedure TOBDDashboard.SetColumns(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 24);
  if FColumns = AValue then
    Exit;
  FColumns := AValue;
  ArrangeTiles;
  Invalidate;
end;

procedure TOBDDashboard.SetRows(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 24);
  if FRows = AValue then
    Exit;
  FRows := AValue;
  ArrangeTiles;
  Invalidate;
end;

procedure TOBDDashboard.SetGap(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 64);
  if FGap = AValue then
    Exit;
  FGap := AValue;
  ArrangeTiles;
  Invalidate;
end;

procedure TOBDDashboard.SetEditMode(AValue: Boolean);
begin
  if FEditMode = AValue then
    Exit;
  FEditMode := AValue;
  FDragTile := nil;
  Invalidate;
  RepaintTiles;
  if Assigned(FOnEditModeChanged) then
    FOnEditModeChanged(Self);
end;

procedure TOBDDashboard.SetLiveData(AValue: TOBDLiveData);
var
  I: Integer;
begin
  if FLiveData = AValue then
    Exit;
  if FLiveData <> nil then
    FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then
    FLiveData.FreeNotification(Self);
  if csLoading in ComponentState then
    Exit;
  for I := 0 to FTiles.Count - 1 do
    if FTiles[I].Control <> nil then
      FTiles[I].Control.AssignDataSource(FLiveData);
end;

procedure TOBDDashboard.RepaintTiles;
var
  I: Integer;
begin
  for I := 0 to FTiles.Count - 1 do
    if FTiles[I].Control <> nil then
      FTiles[I].Control.Invalidate;
end;

procedure TOBDDashboard.LayoutChanged;
begin
  if Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
end;

{ ---- geometry ---------------------------------------------------------------- }

function TOBDDashboard.CellRect(ACol, ARow, AColSpan,
  ARowSpan: Integer): TRect;
var
  G: Integer;
  CW, CH: Double;
begin
  G := ScaleValue(FGap);
  CW := (ClientWidth - G * (FColumns + 1)) / FColumns;
  CH := (ClientHeight - G * (FRows + 1)) / FRows;
  Result.Left := G + Round(ACol * (CW + G));
  Result.Top := G + Round(ARow * (CH + G));
  Result.Right := G + Round((ACol + AColSpan) * (CW + G)) - G;
  Result.Bottom := G + Round((ARow + ARowSpan) * (CH + G)) - G;
end;

procedure TOBDDashboard.CellAt(X, Y: Integer; out ACol, ARow: Integer);
var
  G: Integer;
  CW, CH: Double;
begin
  G := ScaleValue(FGap);
  CW := (ClientWidth - G) / FColumns;
  CH := (ClientHeight - G) / FRows;
  ACol := -1;
  ARow := -1;
  if (CW <= 0) or (CH <= 0) or (X < 0) or (Y < 0) then
    Exit;
  ACol := Trunc((X - G / 2) / CW);
  ARow := Trunc((Y - G / 2) / CH);
  if (ACol < 0) or (ACol >= FColumns) or (ARow < 0) or (ARow >= FRows) then
  begin
    ACol := -1;
    ARow := -1;
  end;
end;

function TOBDDashboard.IsFree(ACol, ARow, AColSpan, ARowSpan: Integer;
  AIgnore: TOBDDashboardTile): Boolean;
var
  I: Integer;
  T: TOBDDashboardTile;
begin
  Result := (ACol >= 0) and (ARow >= 0) and (AColSpan >= 1) and
    (ARowSpan >= 1) and (ACol + AColSpan <= FColumns) and
    (ARow + ARowSpan <= FRows);
  if not Result then
    Exit;
  for I := 0 to FTiles.Count - 1 do
  begin
    T := FTiles[I];
    if T = AIgnore then
      Continue;
    if (ACol < T.Col + T.ColSpan) and (T.Col < ACol + AColSpan) and
      (ARow < T.Row + T.RowSpan) and (T.Row < ARow + ARowSpan) then
      Exit(False);
  end;
end;

function TOBDDashboard.FindFree(AColSpan, ARowSpan: Integer;
  out ACol, ARow: Integer): Boolean;
var
  C, R: Integer;
begin
  for R := 0 to FRows - ARowSpan do
    for C := 0 to FColumns - AColSpan do
      if IsFree(C, R, AColSpan, ARowSpan) then
      begin
        ACol := C;
        ARow := R;
        Exit(True);
      end;
  ACol := -1;
  ARow := -1;
  Result := False;
end;

procedure TOBDDashboard.ArrangeTiles;
var
  I: Integer;
  T: TOBDDashboardTile;
  R: TRect;
begin
  if (FTiles = nil) or (csLoading in ComponentState) or
    (csDestroying in ComponentState) or (FArranging > 0) then
    Exit;
  Inc(FArranging);
  try
    for I := 0 to FTiles.Count - 1 do
    begin
      T := FTiles[I];
      if (T.Control = nil) or (T.Control.Parent <> Self) then
        Continue;
      R := CellRect(T.Col, T.Row, T.ColSpan, T.RowSpan);
      if T.Control.Align <> alNone then
        T.Control.Align := alNone;
      T.Control.SetBounds(R.Left, R.Top, System.Math.Max(1, R.Width),
        System.Math.Max(1, R.Height));
    end;
  finally
    Dec(FArranging);
  end;
end;

procedure TOBDDashboard.SnapFromBounds(ATile: TOBDDashboardTile);
var
  C, R, CS, RS, C2, R2: Integer;
  B: TRect;
begin
  B := ATile.Control.BoundsRect;
  CellAt(B.Left + ScaleValue(4), B.Top + ScaleValue(4), C, R);
  CellAt(B.Right - ScaleValue(4), B.Bottom - ScaleValue(4), C2, R2);
  if (C < 0) or (R < 0) then
    Exit;
  if (C2 < C) or (R2 < R) then
  begin
    CS := ATile.ColSpan;
    RS := ATile.RowSpan;
  end
  else
  begin
    CS := C2 - C + 1;
    RS := R2 - R + 1;
  end;
  if (C = ATile.Col) and (R = ATile.Row) and (CS = ATile.ColSpan) and
    (RS = ATile.RowSpan) then
    Exit;
  if IsFree(C, R, CS, RS, ATile) then
    ATile.SetCells(C, R, CS, RS)
  else if IsFree(C, R, ATile.ColSpan, ATile.RowSpan, ATile) then
    ATile.SetCells(C, R, ATile.ColSpan, ATile.RowSpan);
end;

procedure TOBDDashboard.AlignControls(AControl: TControl; var Rect: TRect);
var
  T: TOBDDashboardTile;
begin
  inherited AlignControls(AControl, Rect);
  if (FTiles = nil) or (FArranging > 0) then
    Exit;
  // A child moved by the form designer lands in the cell under it.
  if (AControl <> nil) and (csDesigning in ComponentState) then
  begin
    T := FTiles.FindControl(AControl);
    if T <> nil then
    begin
      Inc(FArranging);
      try
        SnapFromBounds(T);
      finally
        Dec(FArranging);
      end;
    end;
  end;
  ArrangeTiles;
end;

{ ---- adding / removing ------------------------------------------------------- }

procedure TOBDDashboard.AdoptControl(AControl: TOBDCustomControl);
var
  T: TOBDDashboardTile;
  C, R: Integer;
begin
  Inc(FArranging);
  try
    T := FTiles.Add;
    T.Control := AControl;
    CellAt(AControl.Left + ScaleValue(4), AControl.Top + ScaleValue(4), C, R);
    if (C < 0) or not IsFree(C, R, 1, 1, T) then
      if not FindFree(1, 1, C, R) then
      begin
        C := 0;
        R := FRows;
        FRows := FRows + 1;
      end;
    T.SetCells(C, R, 1, 1);
    if FLiveData <> nil then
      AControl.AssignDataSource(FLiveData);
  finally
    Dec(FArranging);
  end;
end;

procedure TOBDDashboard.CMControlListChange(var Message: TCMControlListChange);
var
  T: TOBDDashboardTile;
begin
  inherited;
  if (FTiles = nil) or (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  if Message.Inserting then
  begin
    if (Message.Control is TOBDCustomControl) and
      (FTiles.FindControl(Message.Control) = nil) then
      AdoptControl(TOBDCustomControl(Message.Control));
  end
  else
  begin
    T := FTiles.FindControl(Message.Control);
    if T <> nil then
    begin
      if FDragTile = T then
        FDragTile := nil;
      T.Free;
    end;
  end;
end;

function TOBDDashboard.PlaceControl(AControl: TOBDCustomControl;
  ACol, ARow, AColSpan, ARowSpan: Integer): TOBDDashboardTile;
var
  C, R: Integer;
begin
  AColSpan := EnsureRange(AColSpan, 1, FColumns);
  ARowSpan := System.Math.Max(1, ARowSpan);
  C := ACol;
  R := ARow;
  Inc(FArranging);
  try
    Result := FTiles.FindControl(AControl);
    if Result = nil then
    begin
      Result := FTiles.Add;
      Result.Control := AControl;
    end;
    if (C < 0) or (R < 0) or not IsFree(C, R, AColSpan, ARowSpan, Result) then
      if not FindFree(AColSpan, ARowSpan, C, R) then
      begin
        // Grid full: grow it downwards.
        C := 0;
        R := FRows;
        FRows := FRows + ARowSpan;
      end;
    if R + ARowSpan > FRows then
      FRows := R + ARowSpan;
    Result.SetCells(C, R, AColSpan, ARowSpan);
    if AControl.Parent <> Self then
      AControl.Parent := Self;
  finally
    Dec(FArranging);
  end;
  if FLiveData <> nil then
    AControl.AssignDataSource(FLiveData);
  ArrangeTiles;
  Invalidate;
  LayoutChanged;
end;

function TOBDDashboard.AddTile(const AKind: string; ACol, ARow, AColSpan,
  ARowSpan: Integer): TOBDCustomControl;
var
  Cls: TOBDTileClass;
begin
  EnsureKinds;
  if not FKinds.TryGetValue(LowerCase(AKind), Cls) then
    raise EOBDDashboard.CreateFmt('Unknown dashboard tile kind "%s"',
      [AKind]);
  Result := Cls.Create(Self);
  try
    if Theme <> nil then
      Result.Theme := Theme;
    PlaceControl(Result, ACol, ARow, AColSpan, ARowSpan);
  except
    Result.Free;
    raise;
  end;
end;

procedure TOBDDashboard.RemoveTile(AControl: TControl);
var
  T: TOBDDashboardTile;
begin
  T := FTiles.FindControl(AControl);
  if T = nil then
    Exit;
  if FDragTile = T then
    FDragTile := nil;
  T.Free;
  AControl.Free;
  Invalidate;
  LayoutChanged;
end;

procedure TOBDDashboard.ClearTiles;
var
  C: TControl;
begin
  FDragTile := nil;
  while FTiles.Count > 0 do
  begin
    C := FTiles[FTiles.Count - 1].Control;
    FTiles[FTiles.Count - 1].Free;
    C.Free;
  end;
  Invalidate;
end;

procedure TOBDDashboard.CMRemovePending(var Message: TMessage);
var
  C: TControl;
begin
  while FPendingRemove.Count > 0 do
  begin
    C := FPendingRemove[0];
    FPendingRemove.Delete(0);
    RemoveTile(C);
  end;
end;

{ ---- edit mode --------------------------------------------------------------- }

function TOBDDashboard.IsEditingTiles: Boolean;
begin
  Result := FEditMode and not(csDesigning in ComponentState);
end;

procedure TOBDDashboard.TileMouseMessage(ATile: TControl;
  var AMessage: TMessage);
var
  T: TOBDDashboardTile;
  P: TPoint;
  Hit: TOBDTileHit;
  DC, DR, NC, NR, NCS, NRS: Integer;
  Moved: Boolean;
  G: Integer;
  CW, CH: Double;
begin
  AMessage.Result := 0;
  T := FTiles.FindControl(ATile);
  if (T = nil) or not(ATile is TOBDCustomControl) then
    Exit;
  P := Point(TWMMouse(AMessage).XPos, TWMMouse(AMessage).YPos);
  case AMessage.Msg of
    WM_LBUTTONDOWN:
      begin
        Hit := TOBDCustomControl(ATile).EditHitTest(P.X, P.Y);
        if Hit = thClose then
        begin
          // The tile is inside its own window procedure; free it once
          // the message has unwound.
          if FPendingRemove.IndexOf(ATile) < 0 then
            FPendingRemove.Add(ATile);
          PostMessage(Handle, CM_OBD_REMOVE_PENDING, 0, 0);
          Exit;
        end;
        FDragTile := T;
        FDragHit := Hit;
        FDragStart := ATile.ClientToParent(P, Self);
        FDragOrigin := Rect(T.Col, T.Row, T.ColSpan, T.RowSpan);
        Winapi.Windows.SetCapture(TWinControl(ATile).Handle);
      end;
    WM_MOUSEMOVE:
      begin
        Hit := TOBDCustomControl(ATile).EditHitTest(P.X, P.Y);
        if FDragTile = T then
          Hit := FDragHit;
        case Hit of
          thResize:
            Winapi.Windows.SetCursor(Screen.Cursors[crSizeNWSE]);
          thClose:
            Winapi.Windows.SetCursor(Screen.Cursors[crHandPoint]);
        else
          Winapi.Windows.SetCursor(Screen.Cursors[crSizeAll]);
        end;
        if FDragTile <> T then
          Exit;
        P := ATile.ClientToParent(P, Self);
        G := ScaleValue(FGap);
        CW := (ClientWidth - G) / FColumns;
        CH := (ClientHeight - G) / FRows;
        if (CW <= 0) or (CH <= 0) then
          Exit;
        DC := Round((P.X - FDragStart.X) / CW);
        DR := Round((P.Y - FDragStart.Y) / CH);
        // FDragOrigin holds Col, Row, ColSpan, RowSpan.
        if FDragHit = thResize then
        begin
          NC := FDragOrigin.Left;
          NR := FDragOrigin.Top;
          NCS := EnsureRange(FDragOrigin.Right + DC, 1, FColumns - NC);
          NRS := EnsureRange(FDragOrigin.Bottom + DR, 1, FRows - NR);
        end
        else
        begin
          NCS := FDragOrigin.Right;
          NRS := FDragOrigin.Bottom;
          NC := EnsureRange(FDragOrigin.Left + DC, 0, FColumns - NCS);
          NR := EnsureRange(FDragOrigin.Top + DR, 0, FRows - NRS);
        end;
        if ((NC <> T.Col) or (NR <> T.Row) or (NCS <> T.ColSpan) or
          (NRS <> T.RowSpan)) and IsFree(NC, NR, NCS, NRS, T) then
        begin
          T.SetCells(NC, NR, NCS, NRS);
          Invalidate;
        end;
      end;
    WM_LBUTTONUP:
      if FDragTile = T then
      begin
        Winapi.Windows.ReleaseCapture;
        Moved := (T.Col <> FDragOrigin.Left) or
          (T.Row <> FDragOrigin.Top) or (T.ColSpan <> FDragOrigin.Right) or
          (T.RowSpan <> FDragOrigin.Bottom);
        FDragTile := nil;
        if Moved then
          LayoutChanged;
      end;
  end;
end;

procedure TOBDDashboard.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  C, R: Integer;
begin
  inherited;
  if not IsEditingTiles or (Button <> mbLeft) then
    Exit;
  CellAt(X, Y, C, R);
  if (C >= 0) and IsFree(C, R, 1, 1) and Assigned(FOnEmptyCellClick) then
    FOnEmptyCellClick(Self, C, R);
end;

{ ---- painting ---------------------------------------------------------------- }

procedure TOBDDashboard.PaintControl(ACanvas: TCanvas);
var
  C, R: Integer;
  Cell: TRect;
  S: string;
  ShowCells: Boolean;
begin
  ShowCells := IsEditingTiles or (FTiles.Count = 0) or
    (csDesigning in ComponentState);
  if not ShowCells then
    Exit;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := Palette.Subtle;
  ACanvas.Pen.Style := psDot;
  for R := 0 to FRows - 1 do
    for C := 0 to FColumns - 1 do
      if IsFree(C, R, 1, 1) then
      begin
        Cell := CellRect(C, R, 1, 1);
        ACanvas.Rectangle(Cell.Left, Cell.Top, Cell.Right, Cell.Bottom);
        if IsEditingTiles then
        begin
          S := '+';
          ACanvas.Font.Height := -ScaleValue(20);
          ACanvas.Font.Color := Palette.Subtle;
          ACanvas.TextOut(Cell.Left + (Cell.Width - ACanvas.TextWidth(S)) div 2,
            Cell.Top + (Cell.Height - ACanvas.TextHeight(S)) div 2, S);
        end;
      end;
  ACanvas.Pen.Style := psSolid;
  if FTiles.Count = 0 then
  begin
    if csDesigning in ComponentState then
      S := 'Drop dashboard controls here'
    else
      S := 'No tiles';
    ACanvas.Font.Height := -ScaleValue(14);
    ACanvas.Font.Style := [fsBold];
    ACanvas.Font.Color := Palette.Subtle;
    ACanvas.TextOut((Width - ACanvas.TextWidth(S)) div 2,
      (Height - ACanvas.TextHeight(S)) div 2, S);
  end;
end;

{ ---- layouts ----------------------------------------------------------------- }

function TOBDDashboard.SaveLayout: string;
var
  Root, O, Settings: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  T: TOBDDashboardTile;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('version', TJSONNumber.Create(OBD_DASHBOARD_LAYOUT_VERSION));
    Root.AddPair('columns', TJSONNumber.Create(FColumns));
    Root.AddPair('rows', TJSONNumber.Create(FRows));
    Root.AddPair('gap', TJSONNumber.Create(FGap));
    Arr := TJSONArray.Create;
    Root.AddPair('tiles', Arr);
    for I := 0 to FTiles.Count - 1 do
    begin
      T := FTiles[I];
      if T.Control = nil then
        Continue;
      O := TJSONObject.Create;
      Arr.AddElement(O);
      O.AddPair('kind', KindOf(T.Control.ClassType));
      O.AddPair('col', TJSONNumber.Create(T.Col));
      O.AddPair('row', TJSONNumber.Create(T.Row));
      O.AddPair('colSpan', TJSONNumber.Create(T.ColSpan));
      O.AddPair('rowSpan', TJSONNumber.Create(T.RowSpan));
      Settings := TJSONObject.Create;
      O.AddPair('settings', Settings);
      T.Control.SaveSettings(Settings);
    end;
    Result := Root.Format(2);
  finally
    Root.Free;
  end;
end;

procedure TOBDDashboard.LoadLayout(const AJson: string);
var
  Root, O: TJSONObject;
  AV, EV: TJSONValue;
  Arr: TJSONArray;
  I, N, C, R, CS, RS: Integer;
  Kind: string;
  Cls: TOBDTileClass;
  Ctl: TOBDCustomControl;
begin
  Root := ParseOBDJSONObject(AJson);
  try
    AV := Root.Values['tiles'];
    if not(AV is TJSONArray) then
      raise EOBDDashboard.Create('Not a dashboard layout: "tiles" missing');
    EnsureKinds;
    ClearTiles;
    N := FColumns;
    if OBDJsonReadInt(Root, 'columns', N) then
      FColumns := EnsureRange(N, 1, 24);
    N := FRows;
    if OBDJsonReadInt(Root, 'rows', N) then
      FRows := EnsureRange(N, 1, 24);
    N := FGap;
    if OBDJsonReadInt(Root, 'gap', N) then
      FGap := EnsureRange(N, 0, 64);
    Arr := TJSONArray(AV);
    for I := 0 to Arr.Count - 1 do
    begin
      EV := Arr.Items[I];
      if not(EV is TJSONObject) then
        Continue;
      O := TJSONObject(EV);
      Kind := '';
      if not OBDJsonReadStr(O, 'kind', Kind) then
        Continue;
      if not FKinds.TryGetValue(LowerCase(Kind), Cls) then
        Continue;
      C := -1;
      R := -1;
      CS := 1;
      RS := 1;
      OBDJsonReadInt(O, 'col', C);
      OBDJsonReadInt(O, 'row', R);
      OBDJsonReadInt(O, 'colSpan', CS);
      OBDJsonReadInt(O, 'rowSpan', RS);
      Ctl := Cls.Create(Self);
      try
        if Theme <> nil then
          Ctl.Theme := Theme;
        EV := O.Values['settings'];
        if EV is TJSONObject then
          Ctl.LoadSettings(TJSONObject(EV));
        PlaceControl(Ctl, C, R, CS, RS);
      except
        Ctl.Free;
        raise;
      end;
    end;
  finally
    Root.Free;
  end;
  ArrangeTiles;
  Invalidate;
  LayoutChanged;
end;

procedure TOBDDashboard.SaveLayoutToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, SaveLayout, TEncoding.UTF8);
end;

procedure TOBDDashboard.LoadLayoutFromFile(const AFileName: string);
begin
  LoadLayout(TFile.ReadAllText(AFileName, TEncoding.UTF8));
end;

end.
