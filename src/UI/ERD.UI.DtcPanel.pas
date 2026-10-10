//------------------------------------------------------------------------------
//  ERD.UI.DtcPanel
//
//  TOBDDtcPanel - OBD Studio trouble-code panel with status chips,
//  filter strip, inline freeze-frame details and guarded clear-codes
//  confirmation.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.DtcPanel;

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
  ERD.UI.Buttons,
  ERD.UI.Segmented,
  ERD.Service.DTCs;

type
  TOBDDtcPanel = class;
  TOBDDtcCollection = class;

  /// <summary>Code bucket shown in the status column.</summary>
  TOBDDtcStatus = (
    /// <summary>Confirmed or stored in the ECU memory.</summary>
    dsStored,
    /// <summary>Seen on the current or previous trip but not confirmed.</summary>
    dsPending,
    /// <summary>Permanent code that clears only after the ECU verifies the fix.</summary>
    dsPermanent);

  /// <summary>Rows included by the footer filter strip.</summary>
  TOBDDtcFilter = (
    /// <summary>Show every trouble code.</summary>
    dfAll,
    /// <summary>Show stored codes only.</summary>
    dfStored,
    /// <summary>Show pending codes only.</summary>
    dfPending,
    /// <summary>Show permanent codes only.</summary>
    dfPermanent);

  /// <summary>Fires when the selected trouble-code row changes.</summary>
  /// <param name="Sender">The panel.</param>
  /// <param name="AIndex">Index in <see cref="Items"/>, or -1.</param>
  TOBDDtcSelectEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>One trouble-code row with optional freeze-frame values.</summary>
  TOBDDtcItem = class(TCollectionItem)
  strict private
    FCode: string;
    FDescription: string;
    FSystemName: string;
    FEcu: string;
    FStatus: TOBDDtcStatus;
    FFreezeValues: TStrings;
    procedure SetCode(const AValue: string);
    procedure SetDescription(const AValue: string);
    procedure SetSystemName(const AValue: string);
    procedure SetEcu(const AValue: string);
    procedure SetStatus(AValue: TOBDDtcStatus);
    procedure SetFreezeValues(AValue: TStrings);
    procedure FreezeValuesChanged(Sender: TObject);
  protected
    /// <summary>Returns the code for the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates the row and its freeze-frame string list.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the freeze-frame string list.</summary>
    destructor Destroy; override;
    /// <summary>Copies code, text, status and freeze-frame values.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>SAE J2012 code text, for example P0401.</summary>
    property Code: string read FCode write SetCode;
    /// <summary>Human-readable fault description.</summary>
    property Description: string read FDescription write SetDescription;
    /// <summary>Vehicle system group shown in the System column.</summary>
    property SystemName: string read FSystemName write SetSystemName;
    /// <summary>Control-unit text shown in the ECU column.</summary>
    property Ecu: string read FEcu write SetEcu;
    /// <summary>Status bucket that drives the chip colour.</summary>
    property Status: TOBDDtcStatus read FStatus write SetStatus default dsStored;
    /// <summary>Freeze-frame values as Name=Value strings.</summary>
    property FreezeValues: TStrings read FFreezeValues write SetFreezeValues;
  end;

  /// <summary>Streamable trouble-code row collection.</summary>
  TOBDDtcCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDDtcItem;
    procedure SetItem(Index: Integer; AValue: TOBDDtcItem);
  protected
    /// <summary>Notifies the panel when a row changes.</summary>
    /// <param name="Item">Changed collection item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a trouble-code row.</summary>
    /// <returns>The new row.</returns>
    function Add: TOBDDtcItem;
    /// <summary>Copies another DTC collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed row indexer.</summary>
    property Items[Index: Integer]: TOBDDtcItem read GetItem write SetItem; default;
  end;

  /// <summary>OBD Studio trouble-code panel.</summary>
  TOBDDtcPanel = class(TOBDCustomControl, IOBDSurface)
  strict private
    FItems: TOBDDtcCollection;
    FReadButton: TOBDButton;
    FClearButton: TOBDButton;
    FCancelButton: TOBDButton;
    FConfirmButton: TOBDButton;
    FPrecheckIgnition: TOBDCheckBox;
    FPrecheckReport: TOBDCheckBox;
    FFilterStrip: TOBDSegmented;
    FTitleCaption: string;
    FReadCaption: string;
    FClearCaption: string;
    FConfirmClearCaption: string;
    FCancelCaption: string;
    FShowEcu: Boolean;
    FConfirmClear: Boolean;
    FConfirmVisible: Boolean;
    FLastRead: TDateTime;
    FSelectedIndex: Integer;
    FExpandedIndex: Integer;
    FFilter: TOBDDtcFilter;
    FScrollPos: Integer;
    FHoverIndex: Integer;
    FDraggingScroll: Boolean;
    FScrollHover: Boolean;
    FScrollDragOffset: Integer;
    FUpdateCount: Integer;
    FOnReadCodes: TNotifyEvent;
    FOnClearCodes: TNotifyEvent;
    FOnSelect: TOBDDtcSelectEvent;
    procedure SetItems(AValue: TOBDDtcCollection);
    procedure SetTitleCaption(const AValue: string);
    procedure SetReadCaption(const AValue: string);
    procedure SetClearCaption(const AValue: string);
    procedure SetConfirmClearCaption(const AValue: string);
    procedure SetCancelCaption(const AValue: string);
    procedure SetShowEcu(AValue: Boolean);
    procedure SetConfirmClear(AValue: Boolean);
    procedure SetLastRead(AValue: TDateTime);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetExpandedIndex(AValue: Integer);
    procedure SetFilter(AValue: TOBDDtcFilter);
    procedure ItemsChanged;
    procedure ReadButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ConfirmButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure PrecheckChanged(Sender: TObject);
    procedure FilterStripChange(Sender: TObject);
    procedure UpdateFilterStrip;
    procedure LayoutChildren;
    function ConfirmationFillColor: TColor;
    function HeaderHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function FreezeHeight: Integer;
    function ConfirmHeight: Integer;
    function BodyRect: TRect;
    function FooterRect: TRect;
    function ConfirmRect: TRect;
    function ScrollBarWidth: Integer;
    function UsePreviewRows: Boolean;
    function SourceCount: Integer;
    function SourceStatus(AIndex: Integer): TOBDDtcStatus;
    function SourceCode(AIndex: Integer): string;
    function SourceDescription(AIndex: Integer): string;
    function SourceSystem(AIndex: Integer): string;
    function SourceEcu(AIndex: Integer): string;
    function SourceFreezeCount(AIndex: Integer): Integer;
    function SourceFreezeValue(AIndex, AFreezeIndex: Integer): string;
    function EffectiveExpandedIndex: Integer;
    function VisibleByFilter(AStatus: TOBDDtcStatus): Boolean;
    function VisibleCount: Integer;
    function VisibleToSource(AVisibleIndex: Integer): Integer;
    function SourceToVisible(ASourceIndex: Integer): Integer;
    function DtcCount(AStatus: TOBDDtcStatus): Integer; overload;
    function DtcCount: Integer; overload;
    function ClearableCount: Integer;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function ScrollThumbRect: TRect;
    function HitRow(X, Y: Integer; out ASourceIndex: Integer;
      out AOnExpander: Boolean): Boolean;
    procedure SetScrollPos(AValue: Integer);
    procedure EnsureSelectedVisible;
    procedure SelectVisible(AVisibleIndex: Integer);
    procedure ToggleExpanded(ASourceIndex: Integer);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawColumnHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawRow(APainter: TOBDPainter; ASourceIndex, ATop: Integer);
    procedure DrawInlineFreeze(APainter: TOBDPainter; ASourceIndex, ATop: Integer);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawClearConfirmation(APainter: TOBDPainter);
    procedure DrawEmptyState(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    function StatusText(AStatus: TOBDDtcStatus): string;
    function StatusColor(APainter: TOBDPainter; AStatus: TOBDDtcStatus): TColor;
    function FooterText: string;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the card, header, grid, inline details and footer.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Repositions the child buttons and filter strip.</summary>
    procedure Resize; override;
    /// <summary>Handles row selection, expanders and scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Tracks row hover and scroll-bar dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Finishes scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Handles row navigation and Enter-to-expand.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Scrolls the row area downward.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    /// <summary>Scrolls the row area upward.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    /// <summary>Creates the panel and the interactive child controls.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the item collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies rows and settings from another DTC panel.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Re-lays out child controls after a density change.</summary>
    procedure DensityChanged; override;
    /// <summary>Suspends filter/count refresh while adding many rows.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes refresh and repaint after <see cref="BeginUpdate"/>.</summary>
    procedure EndUpdate;
    /// <summary>Removes every trouble-code row.</summary>
    procedure Clear;
    /// <summary>Adds one trouble-code row.</summary>
    /// <param name="ACode">SAE J2012 code.</param>
    /// <param name="ADescription">Human-readable description.</param>
    /// <param name="ASystem">Vehicle system group.</param>
    /// <param name="AEcu">Control-unit label.</param>
    /// <param name="AStatus">Status bucket.</param>
    /// <returns>The new row.</returns>
    function AddCode(const ACode, ADescription, ASystem, AEcu: string;
      AStatus: TOBDDtcStatus): TOBDDtcItem; overload;
    /// <summary>Adds one trouble-code row with freeze-frame values.</summary>
    /// <param name="ACode">SAE J2012 code.</param>
    /// <param name="ADescription">Human-readable description.</param>
    /// <param name="ASystem">Vehicle system group.</param>
    /// <param name="AEcu">Control-unit label.</param>
    /// <param name="AStatus">Status bucket.</param>
    /// <param name="AFreezeValues">Name=Value freeze-frame values.</param>
    /// <returns>The new row.</returns>
    function AddCode(const ACode, ADescription, ASystem, AEcu: string;
      AStatus: TOBDDtcStatus; const AFreezeValues: array of string): TOBDDtcItem; overload;
    /// <summary>Loads rows from TOBDDTCs service results.</summary>
    /// <param name="AKind">Service result bucket.</param>
    /// <param name="AEntries">Decoded service entries.</param>
    procedure LoadFromService(AKind: TOBDDtcKind;
      const AEntries: TArray<TOBDDtcEntry>);
    /// <summary>Number of stored rows, before filtering.</summary>
    /// <returns>Row count.</returns>
    function Count: Integer;
    /// <summary>Card face used by child OBD controls.</summary>
    /// <returns>The palette gauge face.</returns>
    function SurfaceColor: TColor;
    /// <summary>Selected source row index, or -1.</summary>
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
  published
    /// <summary>Streamed trouble-code rows.</summary>
    property Items: TOBDDtcCollection read FItems write SetItems;
    /// <summary>Header title.</summary>
    property TitleCaption: string read FTitleCaption write SetTitleCaption;
    /// <summary>Caption of the primary read button.</summary>
    property ReadCaption: string read FReadCaption write SetReadCaption;
    /// <summary>Caption of the header clear button.</summary>
    property ClearCaption: string read FClearCaption write SetClearCaption;
    /// <summary>Caption of the inline destructive confirmation button.</summary>
    property ConfirmClearCaption: string read FConfirmClearCaption
      write SetConfirmClearCaption;
    /// <summary>Caption of the inline cancel button.</summary>
    property CancelCaption: string read FCancelCaption write SetCancelCaption;
    /// <summary>Shows the ECU column.</summary>
    property ShowEcu: Boolean read FShowEcu write SetShowEcu default True;
    /// <summary>When True, clear uses inline confirmation before firing OnClearCodes.</summary>
    property ConfirmClear: Boolean read FConfirmClear write SetConfirmClear
      default True;
    /// <summary>Timestamp shown in the footer.</summary>
    property LastRead: TDateTime read FLastRead write SetLastRead;
    /// <summary>Active footer filter.</summary>
    property Filter: TOBDDtcFilter read FFilter write SetFilter default dfAll;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the user asks the host to read trouble codes.</summary>
    property OnReadCodes: TNotifyEvent read FOnReadCodes write FOnReadCodes;
    /// <summary>Fires after clear-codes confirmation, or immediately when ConfirmClear is False.</summary>
    property OnClearCodes: TNotifyEvent read FOnClearCodes write FOnClearCodes;
    /// <summary>Fires when the selected row changes.</summary>
    property OnSelect: TOBDDtcSelectEvent read FOnSelect write FOnSelect;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

const
  DTC_TITLE_SIZE = 16;
  DTC_CODE_SIZE = 14;
  DTC_TEXT_SIZE = 13;
  DTC_META_SIZE = 12.5;
  DTC_FOOTER_SIZE = 12;
  DTC_CONFIRM_HEIGHT = 124;
  DTC_PREVIEW_COUNT = 5;

  PREVIEW_FREEZE: array [0 .. 7] of string = (
    'Engine speed=2 140 rpm',
    'Load=62.4 %',
    'Coolant=84 °C',
    'Speed=78 km/h',
    'Commanded EGR=38.0 %',
    'EGR error=-31.5 %',
    'Intake MAP=142 kPa',
    'DPF Δp=18.6 kPa');

function SystemNameFromCode(const ACode: string): string;
begin
  if ACode = '' then
    Exit('Powertrain');
  case UpCase(ACode[1]) of
    'C':
      Result := 'Chassis';
    'B':
      Result := 'Body';
    'U':
      Result := 'Network';
  else
    Result := 'Powertrain';
  end;
end;

{ TOBDDtcItem }

constructor TOBDDtcItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStatus := dsStored;
  FFreezeValues := TStringList.Create;
  TStringList(FFreezeValues).OnChange := FreezeValuesChanged;
end;

destructor TOBDDtcItem.Destroy;
begin
  FFreezeValues.Free;
  inherited;
end;

procedure TOBDDtcItem.Assign(Source: TPersistent);
var
  Item: TOBDDtcItem;
begin
  if Source is TOBDDtcItem then
  begin
    Item := TOBDDtcItem(Source);
    FCode := Item.Code;
    FDescription := Item.Description;
    FSystemName := Item.SystemName;
    FEcu := Item.Ecu;
    FStatus := Item.Status;
    FFreezeValues.Assign(Item.FreezeValues);
    Changed(False);
  end
  else
    inherited;
end;

function TOBDDtcItem.GetDisplayName: string;
begin
  if FCode <> '' then
    Result := FCode
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDDtcItem.FreezeValuesChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TOBDDtcItem.SetCode(const AValue: string);
begin
  if FCode = AValue then
    Exit;
  FCode := AValue;
  Changed(False);
end;

procedure TOBDDtcItem.SetDescription(const AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
  Changed(False);
end;

procedure TOBDDtcItem.SetSystemName(const AValue: string);
begin
  if FSystemName = AValue then
    Exit;
  FSystemName := AValue;
  Changed(False);
end;

procedure TOBDDtcItem.SetEcu(const AValue: string);
begin
  if FEcu = AValue then
    Exit;
  FEcu := AValue;
  Changed(False);
end;

procedure TOBDDtcItem.SetStatus(AValue: TOBDDtcStatus);
begin
  if FStatus = AValue then
    Exit;
  FStatus := AValue;
  Changed(False);
end;

procedure TOBDDtcItem.SetFreezeValues(AValue: TStrings);
begin
  FFreezeValues.Assign(AValue);
end;

{ TOBDDtcCollection }

function TOBDDtcCollection.Add: TOBDDtcItem;
begin
  Result := TOBDDtcItem(inherited Add);
end;

procedure TOBDDtcCollection.Assign(Source: TPersistent);
var
  Src: TOBDDtcCollection;
  I: Integer;
begin
  if Source is TOBDDtcCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDDtcCollection(Source);
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

function TOBDDtcCollection.GetItem(Index: Integer): TOBDDtcItem;
begin
  Result := TOBDDtcItem(inherited GetItem(Index));
end;

procedure TOBDDtcCollection.SetItem(Index: Integer; AValue: TOBDDtcItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDDtcCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDDtcPanel then
    TOBDDtcPanel(GetOwner).ItemsChanged;
end;

{ TOBDDtcPanel }

constructor TOBDDtcPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csDoubleClicks,
    csCaptureMouse, csAcceptsControls];
  Width := 760;
  Height := 376;
  TabStop := True;

  FItems := TOBDDtcCollection.Create(Self, TOBDDtcItem);
  FTitleCaption := 'Diagnostic trouble codes';
  FReadCaption := 'Read codes';
  FClearCaption := 'Clear codes…';
  FConfirmClearCaption := 'Clear codes';
  FCancelCaption := 'Cancel';
  FShowEcu := True;
  FConfirmClear := True;
  FSelectedIndex := -1;
  FExpandedIndex := -1;
  FHoverIndex := -1;
  FFilter := dfAll;

  FReadButton := TOBDButton.Create(Self);
  FReadButton.Parent := Self;
  FReadButton.Caption := FReadCaption;
  FReadButton.Kind := bkPrimary;
  FReadButton.Glyph := glRead;
  FReadButton.OnClick := ReadButtonClick;

  FClearButton := TOBDButton.Create(Self);
  FClearButton.Parent := Self;
  FClearButton.Caption := FClearCaption;
  FClearButton.Kind := bkDangerOutline;
  FClearButton.Glyph := glClear;
  FClearButton.OnClick := ClearButtonClick;

  FCancelButton := TOBDButton.Create(Self);
  FCancelButton.Parent := Self;
  FCancelButton.Caption := FCancelCaption;
  FCancelButton.Kind := bkSecondary;
  FCancelButton.OnClick := CancelButtonClick;
  FCancelButton.Visible := False;

  FConfirmButton := TOBDButton.Create(Self);
  FConfirmButton.Parent := Self;
  FConfirmButton.Caption := FConfirmClearCaption;
  FConfirmButton.Kind := bkDanger;
  FConfirmButton.Glyph := glClear;
  FConfirmButton.OnClick := ConfirmButtonClick;
  FConfirmButton.Visible := False;

  FPrecheckIgnition := TOBDCheckBox.Create(Self);
  FPrecheckIgnition.Parent := Self;
  FPrecheckIgnition.Caption := 'Ignition on, engine off';
  FPrecheckIgnition.OnChange := PrecheckChanged;
  FPrecheckIgnition.Visible := False;

  FPrecheckReport := TOBDCheckBox.Create(Self);
  FPrecheckReport.Parent := Self;
  FPrecheckReport.Caption := 'Codes saved to the job report';
  FPrecheckReport.OnChange := PrecheckChanged;
  FPrecheckReport.Visible := False;

  FFilterStrip := TOBDSegmented.Create(Self);
  FFilterStrip.Parent := Self;
  FFilterStrip.OnChange := FilterStripChange;

  UpdateFilterStrip;
  LayoutChildren;
end;

destructor TOBDDtcPanel.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TOBDDtcPanel.Assign(Source: TPersistent);
var
  Panel: TOBDDtcPanel;
begin
  if Source is TOBDDtcPanel then
  begin
    Panel := TOBDDtcPanel(Source);
    FItems.Assign(Panel.Items);
    FTitleCaption := Panel.TitleCaption;
    FReadCaption := Panel.ReadCaption;
    FClearCaption := Panel.ClearCaption;
    FConfirmClearCaption := Panel.ConfirmClearCaption;
    FCancelCaption := Panel.CancelCaption;
    FShowEcu := Panel.ShowEcu;
    FConfirmClear := Panel.ConfirmClear;
    FLastRead := Panel.LastRead;
    FFilter := Panel.Filter;
    FSelectedIndex := Panel.SelectedIndex;
    FExpandedIndex := Panel.FExpandedIndex;
    UpdateFilterStrip;
    LayoutChildren;
    Invalidate;
  end
  else
    inherited;
end;

procedure TOBDDtcPanel.DensityChanged;
begin
  inherited;
  LayoutChildren;
end;

procedure TOBDDtcPanel.Resize;
begin
  inherited;
  SetScrollPos(FScrollPos);
  LayoutChildren;
end;

procedure TOBDDtcPanel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDDtcPanel.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    ItemsChanged;
end;

procedure TOBDDtcPanel.Clear;
begin
  BeginUpdate;
  try
    FItems.Clear;
    FSelectedIndex := -1;
    FExpandedIndex := -1;
    FScrollPos := 0;
    FConfirmVisible := False;
  finally
    EndUpdate;
  end;
end;

function TOBDDtcPanel.AddCode(const ACode, ADescription, ASystem, AEcu: string;
  AStatus: TOBDDtcStatus): TOBDDtcItem;
begin
  Result := FItems.Add;
  Result.Code := ACode;
  Result.Description := ADescription;
  Result.SystemName := ASystem;
  Result.Ecu := AEcu;
  Result.Status := AStatus;
  if FSelectedIndex < 0 then
    FSelectedIndex := Result.Index;
  ItemsChanged;
end;

function TOBDDtcPanel.AddCode(const ACode, ADescription, ASystem, AEcu: string;
  AStatus: TOBDDtcStatus; const AFreezeValues: array of string): TOBDDtcItem;
var
  I: Integer;
begin
  Result := AddCode(ACode, ADescription, ASystem, AEcu, AStatus);
  for I := Low(AFreezeValues) to High(AFreezeValues) do
    Result.FreezeValues.Add(AFreezeValues[I]);
end;

procedure TOBDDtcPanel.LoadFromService(AKind: TOBDDtcKind;
  const AEntries: TArray<TOBDDtcEntry>);
var
  Entry: TOBDDtcEntry;
  Status: TOBDDtcStatus;
  Desc: string;
begin
  case AKind of
    dkPending:
      Status := dsPending;
    dkPermanent:
      Status := dsPermanent;
  else
    Status := dsStored;
  end;

  BeginUpdate;
  try
    for Entry in AEntries do
    begin
      Desc := Entry.Description;
      if Desc = '' then
        Desc := 'Diagnostic trouble code';
      AddCode(Entry.Code, Desc, SystemNameFromCode(Entry.Code), '', Status);
    end;
    FLastRead := Now;
  finally
    EndUpdate;
  end;
end;

function TOBDDtcPanel.Count: Integer;
begin
  Result := FItems.Count;
end;

function TOBDDtcPanel.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDDtcPanel.SetItems(AValue: TOBDDtcCollection);
begin
  FItems.Assign(AValue);
end;

procedure TOBDDtcPanel.SetTitleCaption(const AValue: string);
begin
  if FTitleCaption = AValue then
    Exit;
  FTitleCaption := AValue;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetReadCaption(const AValue: string);
begin
  if FReadCaption = AValue then
    Exit;
  FReadCaption := AValue;
  FReadButton.Caption := AValue;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetClearCaption(const AValue: string);
begin
  if FClearCaption = AValue then
    Exit;
  FClearCaption := AValue;
  FClearButton.Caption := AValue;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetConfirmClearCaption(const AValue: string);
begin
  if FConfirmClearCaption = AValue then
    Exit;
  FConfirmClearCaption := AValue;
  FConfirmButton.Caption := AValue;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetCancelCaption(const AValue: string);
begin
  if FCancelCaption = AValue then
    Exit;
  FCancelCaption := AValue;
  FCancelButton.Caption := AValue;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetShowEcu(AValue: Boolean);
begin
  if FShowEcu = AValue then
    Exit;
  FShowEcu := AValue;
  Invalidate;
end;

procedure TOBDDtcPanel.SetConfirmClear(AValue: Boolean);
begin
  if FConfirmClear = AValue then
    Exit;
  FConfirmClear := AValue;
  if not FConfirmClear then
    FConfirmVisible := False;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.SetLastRead(AValue: TDateTime);
begin
  if FLastRead = AValue then
    Exit;
  FLastRead := AValue;
  Invalidate;
end;

procedure TOBDDtcPanel.SetSelectedIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, SourceCount - 1);
  if FSelectedIndex = AValue then
    Exit;
  FSelectedIndex := AValue;
  EnsureSelectedVisible;
  Invalidate;
  if Assigned(FOnSelect) then
    FOnSelect(Self, FSelectedIndex);
end;

procedure TOBDDtcPanel.SetExpandedIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, SourceCount - 1);
  if FExpandedIndex = AValue then
    Exit;
  FExpandedIndex := AValue;
  SetScrollPos(FScrollPos);
  Invalidate;
end;

procedure TOBDDtcPanel.SetFilter(AValue: TOBDDtcFilter);
begin
  if FFilter = AValue then
    Exit;
  FFilter := AValue;
  FScrollPos := 0;
  if SourceToVisible(FSelectedIndex) < 0 then
    SelectVisible(0);
  UpdateFilterStrip;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.ItemsChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  if FSelectedIndex >= SourceCount then
    FSelectedIndex := SourceCount - 1;
  if FExpandedIndex >= SourceCount then
    FExpandedIndex := -1;
  if DtcCount = 0 then
  begin
    FConfirmVisible := False;
    FPrecheckIgnition.Checked := False;
    FPrecheckReport.Checked := False;
  end;
  SetScrollPos(FScrollPos);
  UpdateFilterStrip;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.ReadButtonClick(Sender: TObject);
begin
  FConfirmVisible := False;
  LayoutChildren;
  Invalidate;
  if Assigned(FOnReadCodes) then
    FOnReadCodes(Self);
end;

procedure TOBDDtcPanel.ClearButtonClick(Sender: TObject);
begin
  if not Enabled then
    Exit;
  if FConfirmClear then
  begin
    FPrecheckIgnition.Checked := False;
    FPrecheckReport.Checked := False;
    FConfirmVisible := True;
    LayoutChildren;
    Invalidate;
  end
  else if Assigned(FOnClearCodes) then
    FOnClearCodes(Self);
end;

procedure TOBDDtcPanel.ConfirmButtonClick(Sender: TObject);
begin
  if FConfirmVisible and
    (not (FPrecheckIgnition.Checked and FPrecheckReport.Checked)) then
    Exit;
  FConfirmVisible := False;
  LayoutChildren;
  Invalidate;
  if Assigned(FOnClearCodes) then
    FOnClearCodes(Self);
end;

procedure TOBDDtcPanel.CancelButtonClick(Sender: TObject);
begin
  FConfirmVisible := False;
  FPrecheckIgnition.Checked := False;
  FPrecheckReport.Checked := False;
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.PrecheckChanged(Sender: TObject);
begin
  LayoutChildren;
  Invalidate;
end;

procedure TOBDDtcPanel.FilterStripChange(Sender: TObject);
begin
  case FFilterStrip.ItemIndex of
    1:
      Filter := dfStored;
    2:
      Filter := dfPending;
    3:
      Filter := dfPermanent;
  else
    Filter := dfAll;
  end;
end;

procedure TOBDDtcPanel.UpdateFilterStrip;
var
  Wanted: Integer;
begin
  if FFilterStrip = nil then
    Exit;
  FFilterStrip.Items.BeginUpdate;
  try
    FFilterStrip.Items.Clear;
    FFilterStrip.Items.Add(Format('All %d', [DtcCount]));
    FFilterStrip.Items.Add(Format('Stored %d', [DtcCount(dsStored)]));
    FFilterStrip.Items.Add(Format('Pending %d', [DtcCount(dsPending)]));
    FFilterStrip.Items.Add(Format('Permanent %d', [DtcCount(dsPermanent)]));
  finally
    FFilterStrip.Items.EndUpdate;
  end;
  case FFilter of
    dfStored:
      Wanted := 1;
    dfPending:
      Wanted := 2;
    dfPermanent:
      Wanted := 3;
  else
    Wanted := 0;
  end;
  FFilterStrip.ItemIndex := Wanted;
end;

procedure TOBDDtcPanel.LayoutChildren;
var
  Ring, ButtonH, ButtonY, RightX, VisibleW, VisibleH, X, Y, CheckY: Integer;
  Footer, Confirm: TRect;
  ConfirmFill: TColor;
begin
  if FReadButton = nil then
    Exit;

  FReadButton.Caption := FReadCaption;
  FClearButton.Caption := FClearCaption;
  FConfirmButton.Caption := FConfirmClearCaption;
  FCancelButton.Caption := FCancelCaption;
  FPrecheckIgnition.Caption := 'Ignition on, engine off';
  FPrecheckReport.Caption := 'Codes saved to the job report';

  FReadButton.AdjustSize;
  FClearButton.AdjustSize;
  FConfirmButton.AdjustSize;
  FCancelButton.AdjustSize;
  FPrecheckIgnition.AdjustSize;
  FPrecheckReport.AdjustSize;
  FFilterStrip.AdjustSize;

  Ring := ScaleValue(4);
  ButtonH := ScaleValue(Metrics.Button);
  ButtonY := (HeaderHeight - ButtonH) div 2;
  RightX := ClientWidth - ScaleValue(16);

  VisibleW := FClearButton.Width - 2 * Ring;
  FClearButton.SetBounds(RightX - VisibleW - Ring, ButtonY - Ring,
    FClearButton.Width, FClearButton.Height);
  RightX := RightX - VisibleW - ScaleValue(8);

  VisibleW := FReadButton.Width - 2 * Ring;
  FReadButton.SetBounds(RightX - VisibleW - Ring, ButtonY - Ring,
    FReadButton.Width, FReadButton.Height);

  FClearButton.Enabled := DtcCount > 0;

  Footer := FooterRect;
  VisibleW := FFilterStrip.Width - 2 * Ring;
  VisibleH := FFilterStrip.Height - 2 * Ring;
  X := ClientWidth - ScaleValue(16) - VisibleW - Ring;
  Y := Footer.Top + (Footer.Height - VisibleH) div 2 - Ring;
  FFilterStrip.SetBounds(X, Y, FFilterStrip.Width, FFilterStrip.Height);

  Confirm := ConfirmRect;
  FCancelButton.Visible := FConfirmVisible;
  FConfirmButton.Visible := FConfirmVisible;
  FPrecheckIgnition.Visible := FConfirmVisible;
  FPrecheckReport.Visible := FConfirmVisible;
  if FConfirmVisible then
    ConfirmFill := ConfirmationFillColor
  else
    ConfirmFill := clDefault;
  FCancelButton.StyleBackground := ConfirmFill;
  FConfirmButton.StyleBackground := ConfirmFill;
  FPrecheckIgnition.StyleBackground := ConfirmFill;
  FPrecheckReport.StyleBackground := ConfirmFill;
  FConfirmButton.Enabled := (not FConfirmVisible) or
    ((DtcCount > 0) and FPrecheckIgnition.Checked and FPrecheckReport.Checked);
  if FConfirmVisible then
  begin
    VisibleW := FConfirmButton.Width - 2 * Ring;
    X := Confirm.Right - ScaleValue(16) - VisibleW - Ring;
    Y := Confirm.Bottom - ScaleValue(42) - Ring;
    FConfirmButton.SetBounds(X, Y, FConfirmButton.Width, FConfirmButton.Height);
    X := X - ScaleValue(8) - (FCancelButton.Width - 2 * Ring);
    FCancelButton.SetBounds(X, Y, FCancelButton.Width, FCancelButton.Height);
    CheckY := Confirm.Bottom - ScaleValue(26) - FPrecheckIgnition.Height div 2;
    X := Confirm.Left + ScaleValue(56) - Ring;
    FPrecheckIgnition.SetBounds(X, CheckY, FPrecheckIgnition.Width,
      FPrecheckIgnition.Height);
    X := X + ScaleValue(230);
    FPrecheckReport.SetBounds(X, CheckY, FPrecheckReport.Width,
      FPrecheckReport.Height);
  end;
end;

function TOBDDtcPanel.ConfirmationFillColor: TColor;
var
  Strength: Single;
begin
  if OBDIsDarkPalette(Palette) then
    Strength := 0.14
  else
    Strength := 0.16;
  Result := OBDMixColor(Palette.Warning, Palette.GaugeFace, Strength);
end;

function TOBDDtcPanel.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDDtcPanel.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDDtcPanel.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDDtcPanel.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row);
end;

function TOBDDtcPanel.FreezeHeight: Integer;
begin
  Result := ScaleValue(32 + 2 * Metrics.Cell);
end;

function TOBDDtcPanel.ConfirmHeight: Integer;
begin
  if FConfirmVisible then
    Result := ScaleValue(DTC_CONFIRM_HEIGHT)
  else
    Result := 0;
end;

function TOBDDtcPanel.BodyRect: TRect;
var
  TopY, BottomY: Integer;
begin
  TopY := HeaderHeight + ColumnHeaderHeight;
  BottomY := ClientHeight - FooterHeight - ConfirmHeight;
  if BottomY < TopY then
    BottomY := TopY;
  Result := Rect(ScaleValue(1), TopY, ClientWidth - ScaleValue(1), BottomY);
end;

function TOBDDtcPanel.FooterRect: TRect;
begin
  Result := Rect(ScaleValue(1), ClientHeight - FooterHeight,
    ClientWidth - ScaleValue(1), ClientHeight - ScaleValue(1));
end;

function TOBDDtcPanel.ConfirmRect: TRect;
var
  H: Integer;
begin
  H := ConfirmHeight;
  if H = 0 then
    Exit(Rect(0, 0, 0, 0));
  Result := Rect(ScaleValue(12), ClientHeight - FooterHeight - H + ScaleValue(8),
    ClientWidth - ScaleValue(12), ClientHeight - FooterHeight - ScaleValue(8));
end;

function TOBDDtcPanel.ScrollBarWidth: Integer;
begin
  Result := ScaleValue(6);
end;

function TOBDDtcPanel.UsePreviewRows: Boolean;
begin
  Result := (FItems.Count = 0) and IsPreview;
end;

function TOBDDtcPanel.SourceCount: Integer;
begin
  if UsePreviewRows then
    Result := DTC_PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TOBDDtcPanel.SourceStatus(AIndex: Integer): TOBDDtcStatus;
begin
  if UsePreviewRows then
    case AIndex of
      2:
        Result := dsPending;
      4:
        Result := dsPermanent;
    else
      Result := dsStored;
    end
  else
    Result := FItems[AIndex].Status;
end;

function TOBDDtcPanel.SourceCode(AIndex: Integer): string;
begin
  if UsePreviewRows then
    case AIndex of
      0:
        Result := 'P0401';
      1:
        Result := 'P2002';
      2:
        Result := 'P0299';
      3:
        Result := 'U0121';
    else
      Result := 'P20EE';
    end
  else
    Result := FItems[AIndex].Code;
end;

function TOBDDtcPanel.SourceDescription(AIndex: Integer): string;
begin
  if UsePreviewRows then
    case AIndex of
      0:
        Result := 'Exhaust gas recirculation flow insufficient';
      1:
        Result := 'Diesel particulate filter efficiency below threshold (bank 1)';
      2:
        Result := 'Turbocharger / supercharger underboost';
      3:
        Result := 'Lost communication with ABS control module';
    else
      Result := 'SCR NOx catalyst efficiency below threshold (bank 1)';
    end
  else
    Result := FItems[AIndex].Description;
end;

function TOBDDtcPanel.SourceSystem(AIndex: Integer): string;
begin
  if UsePreviewRows then
    case AIndex of
      2:
        Result := 'Air induction';
      3:
        Result := 'Network';
    else
      Result := 'Emissions';
    end
  else
    Result := FItems[AIndex].SystemName;
end;

function TOBDDtcPanel.SourceEcu(AIndex: Integer): string;
begin
  if UsePreviewRows then
    Result := 'Engine · 7E8'
  else
    Result := FItems[AIndex].Ecu;
end;

function TOBDDtcPanel.SourceFreezeCount(AIndex: Integer): Integer;
begin
  if UsePreviewRows then
  begin
    if AIndex in [0, 1, 2] then
      Result := Length(PREVIEW_FREEZE)
    else
      Result := 0;
  end
  else
    Result := FItems[AIndex].FreezeValues.Count;
end;

function TOBDDtcPanel.SourceFreezeValue(AIndex, AFreezeIndex: Integer): string;
begin
  if UsePreviewRows then
    Result := PREVIEW_FREEZE[AFreezeIndex]
  else
    Result := FItems[AIndex].FreezeValues[AFreezeIndex];
end;

function TOBDDtcPanel.EffectiveExpandedIndex: Integer;
begin
  if UsePreviewRows and (FExpandedIndex < 0) then
    Result := 0
  else
    Result := FExpandedIndex;
end;

function TOBDDtcPanel.VisibleByFilter(AStatus: TOBDDtcStatus): Boolean;
begin
  case FFilter of
    dfStored:
      Result := AStatus = dsStored;
    dfPending:
      Result := AStatus = dsPending;
    dfPermanent:
      Result := AStatus = dsPermanent;
  else
    Result := True;
  end;
end;

function TOBDDtcPanel.VisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(SourceStatus(I)) then
      Inc(Result);
end;

function TOBDDtcPanel.VisibleToSource(AVisibleIndex: Integer): Integer;
var
  I, N: Integer;
begin
  N := -1;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(SourceStatus(I)) then
    begin
      Inc(N);
      if N = AVisibleIndex then
        Exit(I);
    end;
  Result := -1;
end;

function TOBDDtcPanel.SourceToVisible(ASourceIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (ASourceIndex < 0) or (ASourceIndex >= SourceCount) or
    not VisibleByFilter(SourceStatus(ASourceIndex)) then
    Exit;
  for I := 0 to ASourceIndex do
    if VisibleByFilter(SourceStatus(I)) then
      Inc(Result);
end;

function TOBDDtcPanel.DtcCount(AStatus: TOBDDtcStatus): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if SourceStatus(I) = AStatus then
      Inc(Result);
end;

function TOBDDtcPanel.DtcCount: Integer;
begin
  Result := SourceCount;
end;

function TOBDDtcPanel.ClearableCount: Integer;
begin
  Result := DtcCount - DtcCount(dsPermanent);
end;

function TOBDDtcPanel.ContentHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(SourceStatus(I)) then
    begin
      Inc(Result, RowHeight);
      if I = EffectiveExpandedIndex then
        Inc(Result, FreezeHeight);
    end;
end;

function TOBDDtcPanel.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDDtcPanel.ScrollThumbRect: TRect;
var
  Track: TRect;
  ThumbH, ThumbY: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if ContentHeight <= BodyRect.Height then
    Exit;
  Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(3), BodyRect.Top + ScaleValue(2),
    ClientWidth - ScaleValue(3), BodyRect.Bottom - ScaleValue(2));
  ThumbH := System.Math.Max(ScaleValue(24), MulDiv(Track.Height,
    BodyRect.Height, ContentHeight));
  ThumbY := Track.Top;
  if MaxScroll > 0 then
    Inc(ThumbY, MulDiv(Track.Height - ThumbH, FScrollPos, MaxScroll));
  Result := Rect(Track.Left, ThumbY, Track.Right, ThumbY + ThumbH);
end;

function TOBDDtcPanel.HitRow(X, Y: Integer; out ASourceIndex: Integer;
  out AOnExpander: Boolean): Boolean;
var
  I, RowTop, RowBottom: Integer;
  Body: TRect;
begin
  Result := False;
  ASourceIndex := -1;
  AOnExpander := False;
  Body := BodyRect;
  if not PtInRect(Body, Point(X, Y)) then
    Exit;
  RowTop := Body.Top - FScrollPos;
  for I := 0 to SourceCount - 1 do
  begin
    if not VisibleByFilter(SourceStatus(I)) then
      Continue;
    RowBottom := RowTop + RowHeight;
    if (Y >= RowTop) and (Y < RowBottom) then
    begin
      ASourceIndex := I;
      AOnExpander := X >= ClientWidth - ScaleValue(48);
      Exit(True);
    end;
    RowTop := RowBottom;
    if I = EffectiveExpandedIndex then
      Inc(RowTop, FreezeHeight);
  end;
end;

procedure TOBDDtcPanel.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDDtcPanel.EnsureSelectedVisible;
var
  Body: TRect;
  I, Y, Visible: Integer;
begin
  Visible := SourceToVisible(FSelectedIndex);
  if Visible < 0 then
    Exit;
  Body := BodyRect;
  Y := 0;
  for I := 0 to SourceCount - 1 do
    if VisibleByFilter(SourceStatus(I)) then
    begin
      if I = FSelectedIndex then
        Break;
      Inc(Y, RowHeight);
      if I = EffectiveExpandedIndex then
        Inc(Y, FreezeHeight);
    end;
  if Y < FScrollPos then
    SetScrollPos(Y)
  else if Y + RowHeight > FScrollPos + Body.Height then
    SetScrollPos(Y + RowHeight - Body.Height);
end;

procedure TOBDDtcPanel.SelectVisible(AVisibleIndex: Integer);
var
  Source: Integer;
begin
  Source := VisibleToSource(AVisibleIndex);
  if Source >= 0 then
    SelectedIndex := Source
  else
    SelectedIndex := -1;
end;

procedure TOBDDtcPanel.ToggleExpanded(ASourceIndex: Integer);
begin
  if ASourceIndex < 0 then
    Exit;
  if FExpandedIndex = ASourceIndex then
    SetExpandedIndex(-1)
  else
    SetExpandedIndex(ASourceIndex);
  EnsureSelectedVisible;
end;

function TOBDDtcPanel.StatusText(AStatus: TOBDDtcStatus): string;
begin
  case AStatus of
    dsPending:
      Result := 'PENDING';
    dsPermanent:
      Result := 'PERMANENT';
  else
    Result := 'STORED';
  end;
end;

function TOBDDtcPanel.StatusColor(APainter: TOBDPainter;
  AStatus: TOBDDtcStatus): TColor;
begin
  case AStatus of
    dsPending:
      Result := Palette.Warning;
    dsPermanent:
      Result := APainter.AccentText;
  else
    Result := Palette.Danger;
  end;
end;

function TOBDDtcPanel.FooterText: string;
begin
  if FLastRead > 0 then
    Result := 'Last read ' + FormatDateTime('hh:nn:ss', FLastRead)
  else if UsePreviewRows then
    Result := 'Last read 18:42:07'
  else
    Result := 'Last read never';
  Result := Result + Format('  ·  %d codes  ·  %d stored, %d pending, %d permanent',
    [DtcCount, DtcCount(dsStored), DtcCount(dsPending), DtcCount(dsPermanent)]);
end;

procedure TOBDDtcPanel.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  LayoutChildren;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawHeader(P);
    DrawColumnHeader(P);
    if (SourceCount = 0) and not UsePreviewRows then
      DrawEmptyState(P)
    else
      DrawRows(P, ACanvas);
    if FConfirmVisible then
      DrawClearConfirmation(P);
    DrawFooter(P);
    DrawScrollBar(P);
  finally
    P.Free;
  end;
end;

procedure TOBDDtcPanel.DrawHeader(APainter: TOBDPainter);
var
  X, Y, Limit, ChipW: Integer;
  Text: string;
begin
  Y := HeaderHeight div 2;
  APainter.Text(ScaleValue(16), Y, FTitleCaption, DTC_TITLE_SIZE,
    Palette.ForegroundText, twBold, taLeftJustify);
  X := ScaleValue(16) + APainter.TextWidth(FTitleCaption, DTC_TITLE_SIZE, twBold) +
    ScaleValue(14);
  Limit := FReadButton.Left - ScaleValue(8);

  Text := Format('%d STORED', [DtcCount(dsStored)]);
  ChipW := APainter.ChipWidth(Text);
  if X + ChipW <= Limit then
    Inc(X, APainter.Chip(X, Y - ScaleValue(10), Text, Palette.Danger) + ScaleValue(6));

  Text := Format('%d PENDING', [DtcCount(dsPending)]);
  ChipW := APainter.ChipWidth(Text);
  if X + ChipW <= Limit then
    Inc(X, APainter.Chip(X, Y - ScaleValue(10), Text, Palette.Warning) + ScaleValue(6));

  Text := Format('%d PERMANENT', [DtcCount(dsPermanent)]);
  ChipW := APainter.ChipWidth(Text);
  if X + ChipW <= Limit then
    APainter.Chip(X, Y - ScaleValue(10), Text, APainter.AccentText);
end;

procedure TOBDDtcPanel.DrawColumnHeader(APainter: TOBDPainter);
var
  R, ColStatus, ColCode, ColDesc, ColSys, ColEcu: Integer;
  Y, H: Integer;
begin
  R := ClientWidth;
  Y := HeaderHeight;
  H := ColumnHeaderHeight;
  APainter.FillRect(Rect(ScaleValue(1), Y, ClientWidth - ScaleValue(1), Y + H),
    APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), Y + H - ScaleValue(1), ClientWidth - ScaleValue(2),
    Palette.NeutralLight);
  ColStatus := ScaleValue(16);
  ColCode := ScaleValue(118);
  ColDesc := ScaleValue(190);
  if FShowEcu then
    ColEcu := R - ScaleValue(132)
  else
    ColEcu := R;
  ColSys := ColEcu - ScaleValue(118);
  APainter.Caps(ColStatus, Y + H div 2, 'Status');
  APainter.Caps(ColCode, Y + H div 2, 'Code');
  APainter.Caps(ColDesc, Y + H div 2, 'Description');
  APainter.Caps(ColSys, Y + H div 2, 'System');
  if FShowEcu then
    APainter.Caps(ColEcu, Y + H div 2, 'Control unit');
end;

procedure TOBDDtcPanel.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, Y: Integer;
  Body: TRect;
begin
  Body := BodyRect;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, Body.Left, Body.Top, Body.Right, Body.Bottom);
    Y := Body.Top - FScrollPos;
    for I := 0 to SourceCount - 1 do
    begin
      if not VisibleByFilter(SourceStatus(I)) then
        Continue;
      if (Y + RowHeight >= Body.Top) and (Y <= Body.Bottom) then
        DrawRow(APainter, I, Y);
      Inc(Y, RowHeight);
      if I = EffectiveExpandedIndex then
      begin
        if (Y + FreezeHeight >= Body.Top) and (Y <= Body.Bottom) then
          DrawInlineFreeze(APainter, I, Y);
        Inc(Y, FreezeHeight);
      end;
      APainter.HLine(ScaleValue(1), Y - ScaleValue(1),
        ClientWidth - ScaleValue(2), Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDDtcPanel.DrawRow(APainter: TOBDPainter; ASourceIndex, ATop: Integer);
var
  R, Mid, ColStatus, ColCode, ColDesc, ColSys, ColEcu, MaxDesc: Integer;
  Color, Fill: TColor;
  IsSelected, IsHover: Boolean;
begin
  R := ClientWidth;
  Mid := ATop + RowHeight div 2;
  ColStatus := ScaleValue(16);
  ColCode := ScaleValue(118);
  ColDesc := ScaleValue(190);
  if FShowEcu then
    ColEcu := R - ScaleValue(132)
  else
    ColEcu := R;
  ColSys := ColEcu - ScaleValue(118);
  MaxDesc := System.Math.Max(0, ColSys - ColDesc - ScaleValue(16));
  Color := StatusColor(APainter, SourceStatus(ASourceIndex));
  IsSelected := (ASourceIndex = FSelectedIndex) or (UsePreviewRows and (ASourceIndex = 0));
  IsHover := ASourceIndex = FHoverIndex;

  if IsSelected then
  begin
    if APainter.Dark then
      Fill := APainter.Tint(Palette.Accent, 0.14)
    else
      Fill := APainter.Tint(Palette.Accent, 0.10);
    APainter.FillRect(Rect(ScaleValue(1), ATop, ClientWidth - ScaleValue(1),
      ATop + RowHeight), Fill);
  end
  else if IsHover then
    APainter.FillRect(Rect(ScaleValue(1), ATop, ClientWidth - ScaleValue(1),
      ATop + RowHeight), OBDMixColor(Palette.ForegroundText, Palette.GaugeFace, 0.04));

  APainter.FillRect(Rect(ScaleValue(1), ATop + ScaleValue(6), ScaleValue(5),
    ATop + RowHeight - ScaleValue(6)), Color);
  APainter.Chip(ColStatus, Mid - ScaleValue(10), StatusText(SourceStatus(ASourceIndex)),
    Color, False, False, 9.5);
  APainter.Text(ColCode, Mid, SourceCode(ASourceIndex), DTC_CODE_SIZE,
    Palette.ForegroundText, twMonoBold, taLeftJustify);
  APainter.Text(ColDesc, Mid, SourceDescription(ASourceIndex), DTC_TEXT_SIZE,
    Palette.ForegroundText, twRegular, taLeftJustify, MaxDesc);
  APainter.Text(ColSys, Mid, SourceSystem(ASourceIndex), DTC_META_SIZE,
    Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(110));
  if FShowEcu then
    APainter.Text(ColEcu, Mid, SourceEcu(ASourceIndex), DTC_META_SIZE,
      Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(96));
  if SourceFreezeCount(ASourceIndex) > 0 then
    APainter.GlyphSnapshot(R - ScaleValue(38), Mid, Palette.Subtle);
  APainter.GlyphChevron(R - ScaleValue(16), Mid, Palette.Subtle,
    ASourceIndex = EffectiveExpandedIndex);
end;

procedure TOBDDtcPanel.DrawInlineFreeze(APainter: TOBDPainter; ASourceIndex,
  ATop: Integer);
var
  H, Cell, I, PerRow, CX, CY, CW, TY, Eq: Integer;
  PairText, NameText, ValueText: string;
  Color: TColor;
begin
  H := FreezeHeight;
  Cell := ScaleValue(Metrics.Cell);
  Color := StatusColor(APainter, SourceStatus(ASourceIndex));
  APainter.FillRect(Rect(ScaleValue(1), ATop, ClientWidth - ScaleValue(1), ATop + H),
    Palette.Background);
  APainter.FillRect(Rect(ScaleValue(1), ATop, ScaleValue(5), ATop + H), Color);
  APainter.Caps(ScaleValue(118), ATop + ScaleValue(16),
    'Freeze frame · when ' + SourceCode(ASourceIndex) + ' was stored');
  APainter.Text(ClientWidth - ScaleValue(16), ATop + ScaleValue(16),
    'Open freeze frame', 11.5, APainter.AccentText, twSemibold, taRightJustify);

  if SourceFreezeCount(ASourceIndex) = 0 then
  begin
    APainter.Text(ScaleValue(118), ATop + H div 2 + ScaleValue(10),
      'No freeze-frame values are linked to this code.', DTC_META_SIZE,
      Palette.GaugeLabel, twRegular, taLeftJustify);
    Exit;
  end;

  PerRow := 4;
  CW := System.Math.Max(ScaleValue(80), (ClientWidth - ScaleValue(118) -
    ScaleValue(16)) div PerRow);
  for I := 0 to System.Math.Min(7, SourceFreezeCount(ASourceIndex) - 1) do
  begin
    PairText := SourceFreezeValue(ASourceIndex, I);
    Eq := Pos('=', PairText);
    if Eq > 0 then
    begin
      NameText := Copy(PairText, 1, Eq - 1);
      ValueText := Copy(PairText, Eq + 1, MaxInt);
    end
    else
    begin
      NameText := PairText;
      ValueText := '';
    end;
    CX := ScaleValue(118) + (I mod PerRow) * CW;
    CY := ATop + ScaleValue(30) + (I div PerRow) * Cell;
    TY := CY + (Cell - ScaleValue(16)) div 2;
    APainter.Text(CX, TY, NameText, 11, Palette.GaugeLabel, twRegular,
      taLeftJustify, System.Math.Max(0, CW - ScaleValue(80)));
    Color := Palette.ForegroundText;
    if SameText(NameText, 'EGR error') then
      Color := Palette.Danger
    else if SameText(NameText, 'Intake MAP') or SameText(NameText, 'DPF Δp') then
      Color := Palette.Warning;
    APainter.Text(CX + CW - ScaleValue(14), TY, ValueText, DTC_META_SIZE,
      Color, twSemibold, taRightJustify, System.Math.Max(0, CW - ScaleValue(8)));
    APainter.HLine(CX, CY + Cell - ScaleValue(10), CW - ScaleValue(14),
      Palette.NeutralLight);
  end;
end;

procedure TOBDDtcPanel.DrawFooter(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := FooterRect;
  APainter.HLine(ScaleValue(1), R.Top, ClientWidth - ScaleValue(2),
    Palette.NeutralLight);
  APainter.Text(ScaleValue(16), R.Top + R.Height div 2, FooterText,
    DTC_FOOTER_SIZE, Palette.GaugeLabel, twRegular, taLeftJustify,
    System.Math.Max(0, FFilterStrip.Left - ScaleValue(24)));
end;

procedure TOBDDtcPanel.DrawClearConfirmation(APainter: TOBDPainter);
var
  R: TRect;
  L, ClearN: Integer;
  Outline: TColor;
begin
  R := ConfirmRect;
  if R.IsEmpty then
    Exit;
  Outline := OBDMixColor(Palette.Warning, Palette.GaugeFace, 0.5);
  APainter.FillRect(R, ConfirmationFillColor);
  APainter.FrameRect(R, Outline);
  APainter.FillRect(Rect(R.Left, R.Top, R.Left + ScaleValue(4), R.Bottom),
    Palette.Warning);
  APainter.GlyphAlert(R.Left + ScaleValue(30), R.Top + ScaleValue(30),
    Palette.Warning);
  L := R.Left + ScaleValue(56);
  ClearN := ClearableCount;
  if ClearN <= 0 then
    ClearN := DtcCount;
  APainter.Text(L, R.Top + ScaleValue(24),
    Format('Clear %d diagnostic trouble codes?', [ClearN]), 15,
    Palette.ForegroundText, twBold, taLeftJustify, R.Width - ScaleValue(72));
  APainter.Text(L, R.Top + ScaleValue(48),
    'Clearing also erases the freeze frames and resets all readiness monitors.',
    DTC_META_SIZE, Palette.ForegroundText, twRegular, taLeftJustify,
    R.Width - ScaleValue(72));
  APainter.Text(L, R.Top + ScaleValue(66),
    'The car will show "not ready" for the emissions test until a full drive cycle is done.',
    DTC_META_SIZE, Palette.ForegroundText, twRegular, taLeftJustify,
    R.Width - ScaleValue(72));
  APainter.Text(L, R.Top + ScaleValue(84),
    'Permanent codes stay until the ECU has seen the fault cleared on its own.',
    DTC_META_SIZE, Palette.GaugeLabel, twRegular, taLeftJustify,
    R.Width - ScaleValue(72));
end;

procedure TOBDDtcPanel.DrawEmptyState(APainter: TOBDPainter);
var
  R: TRect;
  CX, CY: Integer;
begin
  R := BodyRect;
  CX := R.Left + R.Width div 2;
  CY := R.Top + R.Height div 2;
  APainter.GlyphCheck(CX, CY - ScaleValue(22), Palette.Success, 1.4);
  APainter.Text(CX, CY + ScaleValue(10), 'No trouble codes stored', 15,
    Palette.ForegroundText, twSemibold, taCenter);
  APainter.Text(CX, CY + ScaleValue(32), 'Read codes to refresh the vehicle status.',
    DTC_FOOTER_SIZE, Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDDtcPanel.DrawScrollBar(APainter: TOBDPainter);
var
  Track, Thumb: TRect;
  ThumbColor: TColor;
begin
  if ContentHeight <= BodyRect.Height then
    Exit;
  Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(3), BodyRect.Top + ScaleValue(2),
    ClientWidth - ScaleValue(3), BodyRect.Bottom - ScaleValue(2));
  Thumb := ScrollThumbRect;
  APainter.FillRect(Track, APainter.Tint(Palette.NeutralLight, 0.35));
  ThumbColor := APainter.Tint(Palette.NeutralDark, 0.65);
  if FScrollHover or FDraggingScroll then
  begin
    InflateRect(Thumb, ScaleValue(1), 0);
    ThumbColor := APainter.Tint(Palette.NeutralDark, 0.85);
  end;
  APainter.RoundRect(Thumb.Left, Thumb.Top, Thumb.Width, Thumb.Height,
    ScrollBarWidth / 2, ThumbColor, clNone);
end;

procedure TOBDDtcPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Source: Integer;
  Expander: Boolean;
  Thumb: TRect;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;

  Thumb := ScrollThumbRect;
  if not Thumb.IsEmpty and PtInRect(Rect(Thumb.Left - ScaleValue(2), Thumb.Top,
    Thumb.Right + ScaleValue(2), Thumb.Bottom), Point(X, Y)) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    Exit;
  end;
  if (ContentHeight > BodyRect.Height) and
    (X >= ClientWidth - ScrollBarWidth - ScaleValue(5)) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - BodyRect.Height)
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + BodyRect.Height);
    Exit;
  end;

  if HitRow(X, Y, Source, Expander) then
  begin
    SelectedIndex := Source;
    if Expander then
      ToggleExpanded(Source);
  end;
end;

procedure TOBDDtcPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Source: Integer;
  Expander: Boolean;
  Track, Thumb: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(3), BodyRect.Top + ScaleValue(2),
      ClientWidth - ScaleValue(3), BodyRect.Bottom - ScaleValue(2));
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll,
        Track.Height - Thumb.Height));
    Exit;
  end;

  FScrollHover := (ContentHeight > BodyRect.Height) and
    (X >= ClientWidth - ScrollBarWidth - ScaleValue(5)) and
    (Y >= BodyRect.Top) and (Y <= BodyRect.Bottom);
  if HitRow(X, Y, Source, Expander) then
    FHoverIndex := Source
  else
    FHoverIndex := -1;
  if Expander or FScrollHover then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDDtcPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDraggingScroll := False;
  Cursor := crDefault;
end;

procedure TOBDDtcPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHoverIndex := -1;
  FScrollHover := False;
  if not FDraggingScroll then
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDDtcPanel.KeyDown(var Key: Word; Shift: TShiftState);
var
  Visible: Integer;
begin
  inherited;
  if not Enabled then
    Exit;
  Visible := SourceToVisible(FSelectedIndex);
  if Visible < 0 then
    Visible := 0;
  case Key of
    VK_UP:
      SelectVisible(System.Math.Max(0, Visible - 1));
    VK_DOWN:
      SelectVisible(System.Math.Min(VisibleCount - 1, Visible + 1));
    VK_HOME:
      SelectVisible(0);
    VK_END:
      SelectVisible(VisibleCount - 1);
    VK_PRIOR:
      SelectVisible(System.Math.Max(0, Visible -
        System.Math.Max(1, BodyRect.Height div RowHeight)));
    VK_NEXT:
      SelectVisible(System.Math.Min(VisibleCount - 1, Visible +
        System.Math.Max(1, BodyRect.Height div RowHeight)));
    VK_RETURN:
      ToggleExpanded(FSelectedIndex);
  else
    Exit;
  end;
  Key := 0;
end;

function TOBDDtcPanel.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos + RowHeight * 3);
end;

function TOBDDtcPanel.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDDtcPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
end;

end.
