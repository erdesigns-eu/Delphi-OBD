//------------------------------------------------------------------------------
//  OBD.UI.DtcList
//
//  TOBDDtcList — diagnostic-trouble-code grid built on the OS-
//  native VCL TListView face (<c>vsReport</c> style, 4 columns:
//  Code, Description, Severity, Status). Severity colour is
//  applied via <c>OnCustomDrawItem</c> so rows stay theme-aware
//  while keeping the per-severity foreground that v1 shipped.
//
//  Drop on a form, set <see cref="Items"/> from any DTC-producing
//  component (TOBDDTCs, TOBDUDSReadDTC, TOBDKWPReadDTC, …) and
//  wire <c>OnDtcSelected</c> + <c>OnDtcDoubleClick</c> to drive a
//  host freeze-frame / extended-data viewer.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.DtcList.pas, redrawn
//                     on top of the VCL TListView face per the v2
//                     "OS-native control faces" rule.
//------------------------------------------------------------------------------

unit OBD.UI.DtcList;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Graphics,
  Winapi.CommCtrl;

type
  /// <summary>DTC severity — drives the row foreground colour.</summary>
  TOBDDtcSeverity = (
    /// <summary>Informational — light blue foreground.</summary>
    dsInfo,
    /// <summary>Warning — amber foreground.</summary>
    dsWarning,
    /// <summary>Critical — red foreground.</summary>
    dsCritical
  );

  /// <summary>DTC status — surfaced in the Status column.</summary>
  TOBDDtcStatus = (
    /// <summary>Currently active.</summary>
    dtActive,
    /// <summary>Pending (one-trip).</summary>
    dtPending,
    /// <summary>Permanent (cleared only by the OEM tool / driving
    /// cycle proof).</summary>
    dtPermanent,
    /// <summary>History (previously stored).</summary>
    dtHistory
  );

  /// <summary>One DTC row.</summary>
  TOBDDtcItem = record
    /// <summary>5-character J2012 code (e.g. <c>'P0301'</c>).</summary>
    Code: string;
    /// <summary>Human-readable description (resolved via
    /// <c>TOBDOEMRegistry</c> when the host runs through it).</summary>
    Description: string;
    /// <summary>Severity tag — drives row colour.</summary>
    Severity: TOBDDtcSeverity;
    /// <summary>Status — surfaced in the Status column.</summary>
    Status: TOBDDtcStatus;
    /// <summary>Free-form tag (e.g. ECU module id) — opaque to
    /// the list.</summary>
    Tag: NativeInt;
  end;

  /// <summary>Selection / activation event.</summary>
  TOBDDtcEvent = procedure(Sender: TObject;
    AIndex: Integer) of object;

  /// <summary>
  ///   Diagnostic-Trouble-Code list component.
  /// </summary>
  /// <remarks>
  ///   Theme-aware via TListView's normal IDE-theming path. The
  ///   per-row severity colour is painted from
  ///   <c>OnCustomDrawItem</c>; hosts that want a different
  ///   palette tweak the <c>InfoColor</c> / <c>WarningColor</c> /
  ///   <c>CriticalColor</c> published properties.
  /// </remarks>
  TOBDDtcList = class(TListView)
  strict private
    FDtcItems: TList<TOBDDtcItem>;
    FInfoColor: TColor;
    FWarningColor: TColor;
    FCriticalColor: TColor;
    FOnDtcSelected: TOBDDtcEvent;
    FOnDtcDoubleClick: TOBDDtcEvent;
    function SeverityColor(ASeverity: TOBDDtcSeverity): TColor;
    function StatusText(AStatus: TOBDDtcStatus): string;
    function SeverityText(ASeverity: TOBDDtcSeverity): string;
    procedure RebuildView;
    procedure HandleCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure HandleSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure HandleDblClick(Sender: TObject);
  protected
    procedure CreateWnd; override;
  public
    /// <summary>Constructs the list with the four standard
    /// columns + custom-draw + theme-aware defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row buffer.</summary>
    destructor Destroy; override;

    /// <summary>Replaces the row set.</summary>
    /// <param name="AItems">Source rows.</param>
    procedure SetDtcs(const AItems: array of TOBDDtcItem);

    /// <summary>Appends one row to the list.</summary>
    /// <param name="AItem">Row to add.</param>
    procedure AddDtc(const AItem: TOBDDtcItem);

    /// <summary>Convenience: build a row from primitive fields and
    /// append it.</summary>
    /// <param name="ACode">5-character J2012 code.</param>
    /// <param name="ADescription">Human-readable description.</param>
    /// <param name="ASeverity">Severity tag (default
    /// <c>dsInfo</c>).</param>
    /// <param name="AStatus">Status (default <c>dtActive</c>).</param>
    procedure AddDtcEx(const ACode: string;
      const ADescription: string;
      ASeverity: TOBDDtcSeverity = dsInfo;
      AStatus: TOBDDtcStatus = dtActive);

    /// <summary>Drops every row.</summary>
    procedure ClearDtcs;

    /// <summary>Read-only access to the source row at
    /// <c>AIndex</c>.</summary>
    /// <param name="AIndex">0-based row index.</param>
    /// <returns>The source row.</returns>
    function Dtc(AIndex: Integer): TOBDDtcItem;

    /// <summary>Number of rows.</summary>
    function DtcCount: Integer;
  published
    /// <summary>Foreground colour for <c>dsInfo</c> rows.</summary>
    property InfoColor: TColor read FInfoColor write FInfoColor
      default TColor($00FF9933);                  // BGR — light blue
    /// <summary>Foreground colour for <c>dsWarning</c> rows.</summary>
    property WarningColor: TColor read FWarningColor write FWarningColor
      default TColor($0000A5FF);                  // BGR — amber
    /// <summary>Foreground colour for <c>dsCritical</c> rows.</summary>
    property CriticalColor: TColor read FCriticalColor write FCriticalColor
      default TColor($003333E6);                  // BGR — red

    /// <summary>Fires when the user selects a row. Main thread.</summary>
    property OnDtcSelected: TOBDDtcEvent
      read FOnDtcSelected write FOnDtcSelected;
    /// <summary>Fires on row double-click. Main thread.</summary>
    property OnDtcDoubleClick: TOBDDtcEvent
      read FOnDtcDoubleClick write FOnDtcDoubleClick;
  end;

implementation

constructor TOBDDtcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDtcItems := TList<TOBDDtcItem>.Create;
  FInfoColor := TColor($00FF9933);
  FWarningColor := TColor($0000A5FF);
  FCriticalColor := TColor($003333E6);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly := True;
  HideSelection := False;
  GridLines := True;
  OwnerData := False;
  OnCustomDrawItem := HandleCustomDrawItem;
  OnSelectItem := HandleSelectItem;
  OnDblClick := HandleDblClick;
end;

destructor TOBDDtcList.Destroy;
begin
  FDtcItems.Free;
  inherited;
end;

procedure TOBDDtcList.CreateWnd;
var
  Col: TListColumn;
begin
  inherited;
  if Columns.Count = 0 then
  begin
    Col := Columns.Add;
    Col.Caption := 'Code';
    Col.Width := 80;
    Col := Columns.Add;
    Col.Caption := 'Description';
    Col.Width := 320;
    Col := Columns.Add;
    Col.Caption := 'Severity';
    Col.Width := 80;
    Col := Columns.Add;
    Col.Caption := 'Status';
    Col.Width := 100;
  end;
  RebuildView;
end;

function TOBDDtcList.SeverityColor(ASeverity: TOBDDtcSeverity): TColor;
begin
  case ASeverity of
    dsInfo:     Result := FInfoColor;
    dsWarning:  Result := FWarningColor;
    dsCritical: Result := FCriticalColor;
  else
    Result := Font.Color;
  end;
end;

function TOBDDtcList.StatusText(AStatus: TOBDDtcStatus): string;
begin
  case AStatus of
    dtActive:    Result := 'Active';
    dtPending:   Result := 'Pending';
    dtPermanent: Result := 'Permanent';
    dtHistory:   Result := 'History';
  else
    Result := '';
  end;
end;

function TOBDDtcList.SeverityText(ASeverity: TOBDDtcSeverity): string;
begin
  case ASeverity of
    dsInfo:     Result := 'Info';
    dsWarning:  Result := 'Warning';
    dsCritical: Result := 'Critical';
  else
    Result := '';
  end;
end;

procedure TOBDDtcList.RebuildView;
var
  I: Integer;
  LI: TListItem;
  Src: TOBDDtcItem;
begin
  if not HandleAllocated then
    Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    for I := 0 to FDtcItems.Count - 1 do
    begin
      Src := FDtcItems[I];
      LI := Items.Add;
      LI.Caption := Src.Code;
      LI.SubItems.Add(Src.Description);
      LI.SubItems.Add(SeverityText(Src.Severity));
      LI.SubItems.Add(StatusText(Src.Status));
      LI.Data := Pointer(NativeInt(I));
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDDtcList.SetDtcs(const AItems: array of TOBDDtcItem);
var
  I: Integer;
begin
  FDtcItems.Clear;
  for I := Low(AItems) to High(AItems) do
    FDtcItems.Add(AItems[I]);
  RebuildView;
end;

procedure TOBDDtcList.AddDtc(const AItem: TOBDDtcItem);
begin
  FDtcItems.Add(AItem);
  RebuildView;
end;

procedure TOBDDtcList.AddDtcEx(const ACode: string;
  const ADescription: string;
  ASeverity: TOBDDtcSeverity; AStatus: TOBDDtcStatus);
var
  Row: TOBDDtcItem;
begin
  Row.Code := ACode;
  Row.Description := ADescription;
  Row.Severity := ASeverity;
  Row.Status := AStatus;
  Row.Tag := 0;
  AddDtc(Row);
end;

procedure TOBDDtcList.ClearDtcs;
begin
  FDtcItems.Clear;
  if HandleAllocated then
    Items.Clear;
end;

function TOBDDtcList.Dtc(AIndex: Integer): TOBDDtcItem;
begin
  Result := FDtcItems[AIndex];
end;

function TOBDDtcList.DtcCount: Integer;
begin
  Result := FDtcItems.Count;
end;

procedure TOBDDtcList.HandleCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  Idx: Integer;
begin
  if not Assigned(Item) then
    Exit;
  Idx := Item.Index;
  if (Idx < 0) or (Idx >= FDtcItems.Count) then
    Exit;
  Canvas.Font.Color := SeverityColor(FDtcItems[Idx].Severity);
  DefaultDraw := True;
end;

procedure TOBDDtcList.HandleSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then
    Exit;
  if not Assigned(FOnDtcSelected) then
    Exit;
  if (Item <> nil) and (Item.Index >= 0) then
    FOnDtcSelected(Self, Item.Index);
end;

procedure TOBDDtcList.HandleDblClick(Sender: TObject);
begin
  if not Assigned(FOnDtcDoubleClick) then
    Exit;
  if Selected <> nil then
    FOnDtcDoubleClick(Self, Selected.Index);
end;

end.
