//------------------------------------------------------------------------------
//  ERD.UI.LiveDataGrid
//
//  TOBDLiveDataGrid - every PID of interest in one table: name,
//  value, unit, session minimum and maximum. A checkbox per row lets
//  the mechanic pick PIDs for the dashboard or the trend chart
//  (OnCheckChanged / CheckedPIDs).
//
//  - Rows come from SetPIDs / AddPID or from the vehicle via
//    PopulateFromSource (Mode 01 PID $00/$20/... support bitmaps).
//  - Values arrive through Source subscriptions or PushValue.
//  - Rows without an update for StaleAfterMs are drawn greyed out;
//    rows that never received a value show "--".
//  - Values, minimum and maximum follow the theme's unit system.
//  - Mouse wheel / arrow keys scroll, space or a click on the box
//    toggles the check.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : MIT - see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the dashboard set.
//------------------------------------------------------------------------------

unit ERD.UI.LiveDataGrid;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.JSON,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Units,
  ERD.Service.Catalog,
  ERD.Service.LiveData;

type
  /// <summary>Fires when a row's checkbox changes.</summary>
  /// <param name="Sender">The grid.</param>
  /// <param name="APID">PID of the row.</param>
  /// <param name="AChecked">New check state.</param>
  TOBDGridCheckEvent = procedure(Sender: TObject; APID: Byte;
    AChecked: Boolean) of object;

  /// <summary>One row of the grid.</summary>
  TOBDLiveDataRow = record
    /// <summary>Mode 01 PID.</summary>
    PID: Byte;
    /// <summary>Display name.</summary>
    Name: string;
    /// <summary>Metric unit.</summary>
    UnitText: string;
    /// <summary>Last value (metric), NaN when none.</summary>
    Value: Double;
    /// <summary>Lowest value this session.</summary>
    MinValue: Double;
    /// <summary>Highest value this session.</summary>
    MaxValue: Double;
    /// <summary>Tick of the last update, 0 = never.</summary>
    LastTick: UInt64;
    /// <summary>Checkbox state.</summary>
    Checked: Boolean;
  end;

  /// <summary>Table of live PID values with min/max and checkboxes.
  /// </summary>
  TOBDLiveDataGrid = class(TOBDCustomControl)
  strict private
    FRows: TList<TOBDLiveDataRow>;
    FSource: TOBDLiveData;
    FSubscribed: TBytes;
    FStaleAfterMs: Cardinal;
    FTopRow: Integer;
    FSelected: Integer;
    FShowCheckboxes: Boolean;
    FTimer: TTimer;
    FOnCheckChanged: TOBDGridCheckEvent;
    procedure SetSource(AValue: TOBDLiveData);
    procedure SetStaleAfterMs(AValue: Cardinal);
    procedure SetShowCheckboxes(AValue: Boolean);
    procedure SetSelected(AValue: Integer);
    procedure Subscribe;
    procedure Unsubscribe;
    procedure HandleValue(Sender: TObject; const AValue: TOBDPIDValue);
    procedure HandleTimer(Sender: TObject);
    procedure EnsureTimer;
    function RowHeight: Integer;
    function HeaderHeight: Integer;
    function VisibleRows: Integer;
    function IndexOfPID(APID: Byte): Integer;
    function ResolveName(APID: Byte): string;
    function ResolveUnit(APID: Byte): string;
    function IsRowStale(const ARow: TOBDLiveDataRow): Boolean;
    function ColumnX(AColumn: Integer): Integer;
    procedure ToggleRow(AIndex: Integer);
    procedure ScrollTo(ATop: Integer);
    procedure DrawRow(ACanvas: TCanvas; AIndex, ATop: Integer;
      const ARow: TOBDLiveDataRow; APreview: Boolean);
    function PreviewRow(AIndex: Integer): TOBDLiveDataRow;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Subscribes after streaming.</summary>
    procedure Loaded; override;
    /// <summary>Drops the source when it is freed.</summary>
    /// <param name="AComponent">Component inserted / removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints header and visible rows.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Selects a row; toggles the check when the box is hit.
    /// </summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Arrow keys, page keys and space.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates an empty grid.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Unsubscribes and releases rows.</summary>
    destructor Destroy; override;
    /// <summary>Replaces the rows.</summary>
    /// <param name="APIDs">PIDs in display order.</param>
    procedure SetPIDs(const APIDs: TBytes);
    /// <summary>Adds a row unless the PID is already present.</summary>
    /// <param name="APID">Mode 01 PID.</param>
    /// <param name="AName">Display name; empty = from the catalogue.
    /// </param>
    /// <returns>Row index.</returns>
    function AddPID(APID: Byte; const AName: string = ''): Integer;
    /// <summary>Removes every row.</summary>
    procedure Clear;
    /// <summary>Asks the vehicle which PIDs it supports and shows
    /// them (blocking; call after connecting).</summary>
    /// <returns>Number of rows.</returns>
    function PopulateFromSource: Integer;
    /// <summary>Records a value for a PID (adds the row when
    /// missing).</summary>
    /// <param name="APID">Mode 01 PID.</param>
    /// <param name="AValue">Value in the metric unit.</param>
    /// <param name="AUnit">Metric unit; empty keeps the known unit.
    /// </param>
    procedure PushValue(APID: Byte; AValue: Double;
      const AUnit: string = ''); overload;
    /// <summary>Records a decoded value.</summary>
    /// <param name="AValue">Value as delivered by TOBDLiveData.</param>
    procedure PushValue(const AValue: TOBDPIDValue); overload;
    /// <summary>Resets minimum and maximum of every row.</summary>
    procedure ResetMinMax;
    /// <summary>Number of rows.</summary>
    /// <returns>Row count.</returns>
    function RowCount: Integer;
    /// <summary>Row by index.</summary>
    /// <param name="AIndex">Row index.</param>
    /// <returns>Copy of the row.</returns>
    function Row(AIndex: Integer): TOBDLiveDataRow;
    /// <summary>Whether a PID's row is checked.</summary>
    /// <param name="APID">Mode 01 PID.</param>
    /// <returns>True when checked.</returns>
    function IsChecked(APID: Byte): Boolean;
    /// <summary>Checks or unchecks a PID's row.</summary>
    /// <param name="APID">Mode 01 PID.</param>
    /// <param name="AChecked">New state.</param>
    procedure SetChecked(APID: Byte; AChecked: Boolean);
    /// <summary>PIDs of all checked rows.</summary>
    /// <returns>PIDs in display order.</returns>
    function CheckedPIDs: TBytes;
    /// <summary>All PIDs in display order.</summary>
    /// <returns>PIDs.</returns>
    function PIDs: TBytes;
    /// <summary>Binds the grid to a data source.</summary>
    /// <param name="ASource">A <c>TOBDLiveData</c> or nil.</param>
    procedure AssignDataSource(ASource: TComponent); override;
    /// <summary>Writes rows and checks to JSON.</summary>
    /// <param name="AObject">Target object.</param>
    procedure SaveSettings(AObject: TJSONObject); override;
    /// <summary>Reads rows and checks from JSON.</summary>
    /// <param name="AObject">Source object; nil is ignored.</param>
    procedure LoadSettings(AObject: TJSONObject); override;
    /// <summary>Selected row, -1 when none.</summary>
    property Selected: Integer read FSelected write SetSelected;
    /// <summary>First visible row.</summary>
    property TopRow: Integer read FTopRow;
  published
    /// <summary>Data source; every row is subscribed.</summary>
    property Source: TOBDLiveData read FSource write SetSource;
    /// <summary>Milliseconds without an update before a row is
    /// greyed out; 0 disables.</summary>
    property StaleAfterMs: Cardinal read FStaleAfterMs write SetStaleAfterMs
      default 3000;
    /// <summary>Show the checkbox column.</summary>
    property ShowCheckboxes: Boolean read FShowCheckboxes
      write SetShowCheckboxes default True;
    /// <summary>Fires when a row's checkbox changes.</summary>
    property OnCheckChanged: TOBDGridCheckEvent read FOnCheckChanged
      write FOnCheckChanged;
  end;

implementation

const
  ColCheck = 0;
  ColPID = 1;
  ColName = 2;
  ColValue = 3;
  ColUnit = 4;
  ColMin = 5;
  ColMax = 6;

{ ---- TOBDLiveDataGrid -------------------------------------------------------- }

constructor TOBDLiveDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRows := TList<TOBDLiveDataRow>.Create;
  FStaleAfterMs := 3000;
  FShowCheckboxes := True;
  FSelected := -1;
  TabStop := True;
  Width := 520;
  Height := 260;
end;

destructor TOBDLiveDataGrid.Destroy;
begin
  Unsubscribe;
  FTimer.Free;
  FRows.Free;
  inherited;
end;

procedure TOBDLiveDataGrid.Loaded;
begin
  inherited;
  Subscribe;
end;

procedure TOBDLiveDataGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSource) then
  begin
    FSubscribed := nil;
    FSource := nil;
  end;
end;

procedure TOBDLiveDataGrid.AssignDataSource(ASource: TComponent);
begin
  if ASource is TOBDLiveData then
    Source := TOBDLiveData(ASource)
  else
    Source := nil;
end;

procedure TOBDLiveDataGrid.SetSource(AValue: TOBDLiveData);
begin
  if FSource = AValue then
    Exit;
  Unsubscribe;
  if FSource <> nil then
    FSource.RemoveFreeNotification(Self);
  FSource := AValue;
  if FSource <> nil then
    FSource.FreeNotification(Self);
  Subscribe;
end;

procedure TOBDLiveDataGrid.Subscribe;
var
  I: Integer;
begin
  Unsubscribe;
  if (FSource = nil) or (csDesigning in ComponentState) or
    (csLoading in ComponentState) then
    Exit;
  SetLength(FSubscribed, FRows.Count);
  for I := 0 to FRows.Count - 1 do
  begin
    FSubscribed[I] := FRows[I].PID;
    FSource.Subscribe(FRows[I].PID, HandleValue);
  end;
end;

procedure TOBDLiveDataGrid.Unsubscribe;
var
  I: Integer;
begin
  if FSource <> nil then
    for I := 0 to High(FSubscribed) do
      FSource.Unsubscribe(FSubscribed[I], HandleValue);
  FSubscribed := nil;
end;

procedure TOBDLiveDataGrid.HandleValue(Sender: TObject;
  const AValue: TOBDPIDValue);
begin
  PushValue(AValue);
end;

procedure TOBDLiveDataGrid.SetStaleAfterMs(AValue: Cardinal);
begin
  if FStaleAfterMs = AValue then
    Exit;
  FStaleAfterMs := AValue;
  Invalidate;
end;

procedure TOBDLiveDataGrid.SetShowCheckboxes(AValue: Boolean);
begin
  if FShowCheckboxes = AValue then
    Exit;
  FShowCheckboxes := AValue;
  Invalidate;
end;

procedure TOBDLiveDataGrid.SetSelected(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, FRows.Count - 1);
  if FSelected = AValue then
    Exit;
  FSelected := AValue;
  if FSelected >= 0 then
  begin
    if FSelected < FTopRow then
      ScrollTo(FSelected)
    else if FSelected >= FTopRow + VisibleRows then
      ScrollTo(FSelected - VisibleRows + 1);
  end;
  Invalidate;
end;

procedure TOBDLiveDataGrid.EnsureTimer;
begin
  if (FStaleAfterMs = 0) or (csDesigning in ComponentState) then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Interval := 1000;
    FTimer.OnTimer := HandleTimer;
  end;
  FTimer.Enabled := True;
end;

procedure TOBDLiveDataGrid.HandleTimer(Sender: TObject);
begin
  // Repaint so rows that stopped updating turn grey.
  Invalidate;
end;

function TOBDLiveDataGrid.IndexOfPID(APID: Byte): Integer;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    if FRows[I].PID = APID then
      Exit(I);
  Result := -1;
end;

function TOBDLiveDataGrid.ResolveName(APID: Byte): string;
var
  Info: TOBDPIDInfo;
begin
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) then
  begin
    if Info.Name <> '' then
      Exit(Info.Name);
    if Info.Description <> '' then
      Exit(Info.Description);
  end;
  Result := Format('PID %.2X', [APID]);
end;

function TOBDLiveDataGrid.ResolveUnit(APID: Byte): string;
var
  Info: TOBDPIDInfo;
begin
  Result := '';
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) then
    Result := Info.Decoder.Unit_;
end;

function TOBDLiveDataGrid.AddPID(APID: Byte; const AName: string): Integer;
var
  R: TOBDLiveDataRow;
begin
  Result := IndexOfPID(APID);
  if Result >= 0 then
    Exit;
  R := Default(TOBDLiveDataRow);
  R.PID := APID;
  if AName <> '' then
    R.Name := AName
  else
    R.Name := ResolveName(APID);
  R.UnitText := ResolveUnit(APID);
  R.Value := NaN;
  R.MinValue := NaN;
  R.MaxValue := NaN;
  Result := FRows.Add(R);
  if (FSource <> nil) and not(csDesigning in ComponentState) and
    not(csLoading in ComponentState) then
  begin
    FSource.Subscribe(APID, HandleValue);
    FSubscribed := FSubscribed + [APID];
  end;
  Invalidate;
end;

procedure TOBDLiveDataGrid.SetPIDs(const APIDs: TBytes);
var
  I: Integer;
begin
  Unsubscribe;
  FRows.Clear;
  FTopRow := 0;
  FSelected := -1;
  for I := 0 to High(APIDs) do
    AddPID(APIDs[I]);
  Subscribe;
  Invalidate;
end;

procedure TOBDLiveDataGrid.Clear;
begin
  SetPIDs(nil);
end;

function TOBDLiveDataGrid.PopulateFromSource: Integer;
var
  Supported, Filtered: TBytes;
  I: Integer;
begin
  Result := 0;
  if FSource = nil then
    Exit;
  Supported := FSource.SupportedPIDs;
  Filtered := nil;
  // Skip the support-bitmap PIDs themselves ($00, $20, $40, ...).
  for I := 0 to High(Supported) do
    if (Supported[I] mod $20) <> 0 then
      Filtered := Filtered + [Supported[I]];
  SetPIDs(Filtered);
  Result := FRows.Count;
end;

procedure TOBDLiveDataGrid.PushValue(APID: Byte; AValue: Double;
  const AUnit: string);
var
  I: Integer;
  R: TOBDLiveDataRow;
begin
  I := IndexOfPID(APID);
  if I < 0 then
    I := AddPID(APID);
  R := FRows[I];
  if AUnit <> '' then
    R.UnitText := AUnit;
  R.LastTick := TThread.GetTickCount64;
  if not IsNan(AValue) then
  begin
    R.Value := AValue;
    if IsNan(R.MinValue) or (AValue < R.MinValue) then
      R.MinValue := AValue;
    if IsNan(R.MaxValue) or (AValue > R.MaxValue) then
      R.MaxValue := AValue;
  end;
  FRows[I] := R;
  EnsureTimer;
  Invalidate;
end;

procedure TOBDLiveDataGrid.PushValue(const AValue: TOBDPIDValue);
var
  I: Integer;
  R: TOBDLiveDataRow;
begin
  I := IndexOfPID(AValue.PID);
  if (I >= 0) and (AValue.Description <> '') and
    (FRows[I].Name = Format('PID %.2X', [AValue.PID])) then
  begin
    R := FRows[I];
    R.Name := AValue.Description;
    FRows[I] := R;
  end;
  PushValue(AValue.PID, AValue.Value, AValue.Unit_);
end;

procedure TOBDLiveDataGrid.ResetMinMax;
var
  I: Integer;
  R: TOBDLiveDataRow;
begin
  for I := 0 to FRows.Count - 1 do
  begin
    R := FRows[I];
    R.MinValue := R.Value;
    R.MaxValue := R.Value;
    FRows[I] := R;
  end;
  Invalidate;
end;

function TOBDLiveDataGrid.RowCount: Integer;
begin
  Result := FRows.Count;
end;

function TOBDLiveDataGrid.Row(AIndex: Integer): TOBDLiveDataRow;
begin
  Result := FRows[AIndex];
end;

function TOBDLiveDataGrid.IsChecked(APID: Byte): Boolean;
var
  I: Integer;
begin
  I := IndexOfPID(APID);
  Result := (I >= 0) and FRows[I].Checked;
end;

procedure TOBDLiveDataGrid.SetChecked(APID: Byte; AChecked: Boolean);
var
  I: Integer;
  R: TOBDLiveDataRow;
begin
  I := IndexOfPID(APID);
  if (I < 0) or (FRows[I].Checked = AChecked) then
    Exit;
  R := FRows[I];
  R.Checked := AChecked;
  FRows[I] := R;
  Invalidate;
  if Assigned(FOnCheckChanged) then
    FOnCheckChanged(Self, APID, AChecked);
end;

procedure TOBDLiveDataGrid.ToggleRow(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FRows.Count) then
    Exit;
  SetChecked(FRows[AIndex].PID, not FRows[AIndex].Checked);
end;

function TOBDLiveDataGrid.CheckedPIDs: TBytes;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FRows.Count - 1 do
    if FRows[I].Checked then
      Result := Result + [FRows[I].PID];
end;

function TOBDLiveDataGrid.PIDs: TBytes;
var
  I: Integer;
begin
  SetLength(Result, FRows.Count);
  for I := 0 to FRows.Count - 1 do
    Result[I] := FRows[I].PID;
end;

function TOBDLiveDataGrid.IsRowStale(const ARow: TOBDLiveDataRow): Boolean;
begin
  Result := (FStaleAfterMs > 0) and (ARow.LastTick <> 0) and
    (TThread.GetTickCount64 - ARow.LastTick > FStaleAfterMs);
end;

{ ---- layout ------------------------------------------------------------------ }

function TOBDLiveDataGrid.RowHeight: Integer;
begin
  Result := System.Math.Max(ScaleValue(18),
    Round(Abs(Font.Height) * 1.9));
end;

function TOBDLiveDataGrid.HeaderHeight: Integer;
begin
  Result := RowHeight;
end;

function TOBDLiveDataGrid.VisibleRows: Integer;
begin
  Result := System.Math.Max(1, (Height - HeaderHeight) div RowHeight);
end;

function TOBDLiveDataGrid.ColumnX(AColumn: Integer): Integer;
var
  CheckW, PidW, Rest: Integer;
begin
  if FShowCheckboxes then
    CheckW := ScaleValue(28)
  else
    CheckW := 0;
  PidW := ScaleValue(40);
  Rest := System.Math.Max(0, Width - CheckW - PidW);
  // Name 40 %, value 18 %, unit 12 %, min 15 %, max 15 %.
  case AColumn of
    ColCheck:
      Result := 0;
    ColPID:
      Result := CheckW;
    ColName:
      Result := CheckW + PidW;
    ColValue:
      Result := CheckW + PidW + Rest * 40 div 100;
    ColUnit:
      Result := CheckW + PidW + Rest * 58 div 100;
    ColMin:
      Result := CheckW + PidW + Rest * 70 div 100;
    ColMax:
      Result := CheckW + PidW + Rest * 85 div 100;
  else
    Result := Width;
  end;
end;

procedure TOBDLiveDataGrid.ScrollTo(ATop: Integer);
begin
  ATop := EnsureRange(ATop, 0, System.Math.Max(0, FRows.Count - VisibleRows));
  if FTopRow = ATop then
    Exit;
  FTopRow := ATop;
  Invalidate;
end;

{ ---- painting ---------------------------------------------------------------- }

function TOBDLiveDataGrid.PreviewRow(AIndex: Integer): TOBDLiveDataRow;
begin
  Result := Default(TOBDLiveDataRow);
  Result.LastTick := 0;
  case AIndex of
    0:
      begin
        Result.PID := $0C;
        Result.Name := 'Engine RPM';
        Result.UnitText := 'rpm';
        Result.Value := 812;
        Result.MinValue := 780;
        Result.MaxValue := 3420;
        Result.Checked := True;
      end;
    1:
      begin
        Result.PID := $0D;
        Result.Name := 'Vehicle speed';
        Result.UnitText := 'km/h';
        Result.Value := 0;
        Result.MinValue := 0;
        Result.MaxValue := 48;
      end;
    2:
      begin
        Result.PID := $05;
        Result.Name := 'Engine coolant temperature';
        Result.UnitText := OBD_DEGREE_SIGN + 'C';
        Result.Value := 88;
        Result.MinValue := 21;
        Result.MaxValue := 91;
        Result.Checked := True;
      end;
    3:
      begin
        Result.PID := $06;
        Result.Name := 'Short term fuel trim B1';
        Result.UnitText := '%';
        Result.Value := 2.3;
        Result.MinValue := -4.7;
        Result.MaxValue := 6.2;
      end;
  else
    begin
      Result.PID := $42;
      Result.Name := 'Control module voltage';
      Result.UnitText := 'V';
      Result.Value := 14.1;
      Result.MinValue := 12.4;
      Result.MaxValue := 14.3;
    end;
  end;
end;

procedure TOBDLiveDataGrid.DrawRow(ACanvas: TCanvas; AIndex, ATop: Integer;
  const ARow: TOBDLiveDataRow; APreview: Boolean);
var
  H, Box, BX, BY: Integer;
  Conv: TOBDUnitConversion;
  Decimals: Integer;
  TextColor: TColor;

  procedure Cell(AColumn: Integer; const AText: string; ARight: Boolean);
  var
    L, R, TX: Integer;
  begin
    L := ColumnX(AColumn) + ScaleValue(6);
    R := ColumnX(AColumn + 1) - ScaleValue(6);
    if ARight then
      TX := R - ACanvas.TextWidth(AText)
    else
      TX := L;
    ACanvas.TextRect(Rect(L, ATop, R, ATop + H), TX,
      ATop + (H - ACanvas.TextHeight(AText)) div 2, AText);
  end;

  function Fmt(AValue: Double): string;
  begin
    if IsNan(AValue) then
      Result := '--'
    else
      Result := OBDFormatNumber(Conv.ToDisplay(AValue), Decimals);
  end;

begin
  H := RowHeight;
  ACanvas.Brush.Style := bsSolid;
  if AIndex = FSelected then
    ACanvas.Brush.Color := EffectiveAccent
  else if Odd(AIndex) then
    ACanvas.Brush.Color := Palette.GaugeFace
  else
    ACanvas.Brush.Color := EffectiveBackground;
  ACanvas.FillRect(Rect(0, ATop, Width, ATop + H));

  Conv := OBDUnitConversion(ARow.UnitText, UnitSystem);
  Decimals := 0;
  if not IsNan(ARow.Value) and (Frac(ARow.Value) <> 0) then
  begin
    if Abs(Conv.ToDisplay(ARow.Value)) < 10 then
      Decimals := 2
    else
      Decimals := 1;
  end;

  if AIndex = FSelected then
    TextColor := Palette.Background
  else if (not APreview) and ((ARow.LastTick = 0) or IsRowStale(ARow)) then
    TextColor := Palette.Subtle
  else
    TextColor := EffectiveForeground;

  if FShowCheckboxes then
  begin
    Box := System.Math.Min(ScaleValue(14), H - ScaleValue(4));
    BX := (ColumnX(ColPID) - Box) div 2;
    BY := ATop + (H - Box) div 2;
    ACanvas.Pen.Color := TextColor;
    if ARow.Checked then
      ACanvas.Brush.Color := Palette.Accent
    else
      ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(BX, BY, BX + Box, BY + Box);
    if ARow.Checked then
    begin
      ACanvas.Pen.Color := Palette.Background;
      ACanvas.Pen.Width := System.Math.Max(1, ScaleValue(2));
      ACanvas.MoveTo(BX + Box * 2 div 10, BY + Box div 2);
      ACanvas.LineTo(BX + Box * 4 div 10, BY + Box * 75 div 100);
      ACanvas.LineTo(BX + Box * 8 div 10, BY + Box * 25 div 100);
      ACanvas.Pen.Width := 1;
    end;
  end;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := TextColor;
  ACanvas.Font.Style := [];
  Cell(ColPID, Format('%.2X', [ARow.PID]), False);
  Cell(ColName, ARow.Name, False);
  ACanvas.Font.Style := [fsBold];
  Cell(ColValue, Fmt(ARow.Value), True);
  ACanvas.Font.Style := [];
  Cell(ColUnit, Conv.DisplayUnit, False);
  Cell(ColMin, Fmt(ARow.MinValue), True);
  Cell(ColMax, Fmt(ARow.MaxValue), True);
end;

procedure TOBDLiveDataGrid.PaintControl(ACanvas: TCanvas);
const
  Headers: array [ColPID .. ColMax] of string = (
    'PID', 'Name', 'Value', 'Unit', 'Min', 'Max');
var
  I, Y, HH, N: Integer;
  Preview: Boolean;
  S: string;
begin
  ACanvas.Font.Assign(Font);
  HH := HeaderHeight;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Palette.NeutralDark;
  ACanvas.FillRect(Rect(0, 0, Width, HH));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := Palette.NeutralLight;
  for I := ColPID to ColMax do
  begin
    S := Headers[I];
    if I in [ColValue, ColMin, ColMax] then
      ACanvas.TextOut(ColumnX(I + 1) - ScaleValue(6) - ACanvas.TextWidth(S),
        (HH - ACanvas.TextHeight(S)) div 2, S)
    else
      ACanvas.TextOut(ColumnX(I) + ScaleValue(6),
        (HH - ACanvas.TextHeight(S)) div 2, S);
  end;

  Preview := (FRows.Count = 0) and IsPreview;
  if Preview then
    N := 5
  else
    N := FRows.Count;
  if N = 0 then
  begin
    S := 'NO PIDS';
    ACanvas.Font.Color := Palette.Subtle;
    ACanvas.TextOut((Width - ACanvas.TextWidth(S)) div 2,
      HH + (Height - HH - ACanvas.TextHeight(S)) div 2, S);
    Exit;
  end;

  Y := HH;
  I := FTopRow;
  if Preview then
    I := 0;
  while (I < N) and (Y < Height) do
  begin
    if Preview then
      DrawRow(ACanvas, I, Y, PreviewRow(I), True)
    else
      DrawRow(ACanvas, I, Y, FRows[I], False);
    Inc(Y, RowHeight);
    Inc(I);
  end;

  ACanvas.Pen.Color := EffectiveBorder;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(0, 0, Width, Height);
end;

{ ---- input ------------------------------------------------------------------- }

procedure TOBDLiveDataGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  inherited;
  if CanFocus then
    SetFocus;
  if Y < HeaderHeight then
    Exit;
  Index := FTopRow + (Y - HeaderHeight) div RowHeight;
  if Index >= FRows.Count then
    Exit;
  Selected := Index;
  if FShowCheckboxes and (X < ColumnX(ColPID)) then
    ToggleRow(Index);
end;

procedure TOBDLiveDataGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      Selected := System.Math.Max(0, FSelected - 1);
    VK_DOWN:
      Selected := FSelected + 1;
    VK_PRIOR:
      Selected := System.Math.Max(0, FSelected - VisibleRows);
    VK_NEXT:
      Selected := FSelected + VisibleRows;
    VK_HOME:
      Selected := 0;
    VK_END:
      Selected := FRows.Count - 1;
    VK_SPACE:
      ToggleRow(FSelected);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDLiveDataGrid.CMMouseWheel(var Message: TCMMouseWheel);
begin
  ScrollTo(FTopRow - Sign(Message.WheelDelta) * 3);
  Message.Result := 1;
end;

procedure TOBDLiveDataGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

{ ---- settings ---------------------------------------------------------------- }

procedure TOBDLiveDataGrid.SaveSettings(AObject: TJSONObject);
var
  P, C: TJSONArray;
  I: Integer;
begin
  P := TJSONArray.Create;
  AObject.AddPair('pids', P);
  C := TJSONArray.Create;
  AObject.AddPair('checked', C);
  for I := 0 to FRows.Count - 1 do
  begin
    P.Add(Integer(FRows[I].PID));
    if FRows[I].Checked then
      C.Add(Integer(FRows[I].PID));
  end;
  AObject.AddPair('staleAfterMs', TJSONNumber.Create(FStaleAfterMs));
end;

procedure TOBDLiveDataGrid.LoadSettings(AObject: TJSONObject);
var
  AV: TJSONValue;
  Arr: TJSONArray;
  List: TBytes;
  I, N: Integer;
begin
  if AObject = nil then
    Exit;
  AV := AObject.Values['pids'];
  if AV is TJSONArray then
  begin
    Arr := TJSONArray(AV);
    List := nil;
    for I := 0 to Arr.Count - 1 do
      if Arr.Items[I] is TJSONNumber then
        List := List + [Byte(EnsureRange(TJSONNumber(Arr.Items[I]).AsInt,
          0, 255))];
    SetPIDs(List);
  end;
  AV := AObject.Values['checked'];
  if AV is TJSONArray then
  begin
    Arr := TJSONArray(AV);
    for I := 0 to Arr.Count - 1 do
      if Arr.Items[I] is TJSONNumber then
        SetChecked(Byte(EnsureRange(TJSONNumber(Arr.Items[I]).AsInt, 0, 255)),
          True);
  end;
  N := Integer(FStaleAfterMs);
  if OBDJsonReadInt(AObject, 'staleAfterMs', N) then
    StaleAfterMs := Cardinal(System.Math.Max(0, N));
end;

end.
