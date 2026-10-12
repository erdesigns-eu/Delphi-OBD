//------------------------------------------------------------------------------
//  ERD.UI.EcuTopology
//
//  TOBDEcuTopology and TOBDEcuList - OBD Studio Pro control-unit network
//  views sharing one streamable bus and ECU model.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.EcuTopology;

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
  /// <summary>Visual communication state of a control unit.</summary>
  TOBDEcuNodeState = (
    /// <summary>The control unit replied and has no faults.</summary>
    ensOk,
    /// <summary>The control unit replied and reported faults.</summary>
    ensFaults,
    /// <summary>The control unit replied but protected access is locked.</summary>
    ensLocked,
    /// <summary>The scan tried the address and received no reply.</summary>
    ensNoResponse,
    /// <summary>The address is known but has not been scanned yet.</summary>
    ensNotScanned);

  TOBDEcuTopology = class;
  TOBDEcuList = class;

  /// <summary>Fires when an ECU address is selected.</summary>
  /// <param name="Sender">The topology or list control.</param>
  /// <param name="AAddress">Selected ECU address text.</param>
  /// <param name="AIndex">Selected item index, or -1.</param>
  TOBDEcuSelectEvent = procedure(Sender: TObject; const AAddress: string;
    AIndex: Integer) of object;

  /// <summary>One bus lane in the topology view.</summary>
  TOBDEcuBusItem = class(TCollectionItem)
  strict private
    FName: string;
    FSpeedText: string;
    procedure SetName(const AValue: string);
    procedure SetSpeedText(const AValue: string);
  protected
    /// <summary>Returns the bus name for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a bus with the sample CAN speed.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another bus item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Bus display name, for example Powertrain CAN.</summary>
    property Name: string read FName write SetName;
    /// <summary>Bus speed text, for example 500 kbit/s.</summary>
    property SpeedText: string read FSpeedText write SetSpeedText;
  end;

  /// <summary>Streamable collection of bus lanes.</summary>
  TOBDEcuBusCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDEcuBusItem;
    procedure SetItem(Index: Integer; AValue: TOBDEcuBusItem);
  protected
    /// <summary>Notifies the owner when a bus changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a collection of ECU bus items.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a bus item.</summary>
    /// <returns>The new bus.</returns>
    function Add: TOBDEcuBusItem;
    /// <summary>Copies another bus collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed bus item indexer.</summary>
    property Items[Index: Integer]: TOBDEcuBusItem read GetItem write SetItem;
      default;
  end;

  /// <summary>One control-unit row used by both the topology and list.</summary>
  TOBDEcuItem = class(TCollectionItem)
  strict private
    FAddress: string;
    FName: string;
    FPartNumber: string;
    FSystemText: string;
    FBusIndex: Integer;
    FState: TOBDEcuNodeState;
    FFaultCount: Integer;
    procedure SetAddress(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetPartNumber(const AValue: string);
    procedure SetSystemText(const AValue: string);
    procedure SetBusIndex(AValue: Integer);
    procedure SetState(AValue: TOBDEcuNodeState);
    procedure SetFaultCount(AValue: Integer);
  protected
    /// <summary>Returns address and name for the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an unscanned ECU item.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another ECU item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Diagnostic address, for example 09.</summary>
    property Address: string read FAddress write SetAddress;
    /// <summary>Control-unit name.</summary>
    property Name: string read FName write SetName;
    /// <summary>Part number or hardware identifier.</summary>
    property PartNumber: string read FPartNumber write SetPartNumber;
    /// <summary>Short system family, label or software text.</summary>
    property SystemText: string read FSystemText write SetSystemText;
    /// <summary>Index of the bus in Buses.</summary>
    property BusIndex: Integer read FBusIndex write SetBusIndex default 0;
    /// <summary>Reply, fault or protection state.</summary>
    property State: TOBDEcuNodeState read FState write SetState default ensOk;
    /// <summary>Number of faults reported by this ECU.</summary>
    property FaultCount: Integer read FFaultCount write SetFaultCount default 0;
  end;

  /// <summary>Streamable collection of control units.</summary>
  TOBDEcuCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDEcuItem;
    procedure SetItem(Index: Integer; AValue: TOBDEcuItem);
  protected
    /// <summary>Notifies the owner when an ECU changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a collection of ECU items.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds an ECU item.</summary>
    /// <returns>The new ECU.</returns>
    function Add: TOBDEcuItem;
    /// <summary>Copies another ECU collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Finds an ECU by diagnostic address.</summary>
    /// <param name="AAddress">Address text.</param>
    /// <returns>Item index, or -1.</returns>
    function IndexOfAddress(const AAddress: string): Integer;
    /// <summary>Typed ECU item indexer.</summary>
    property Items[Index: Integer]: TOBDEcuItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Topology card showing the tester, gateway, bus lanes and ECUs.</summary>
  TOBDEcuTopology = class(TOBDCustomControl, IOBDSurface)
  strict private
    FBuses: TOBDEcuBusCollection;
    FEcus: TOBDEcuCollection;
    FSelectedAddress: string;
    FItemIndex: Integer;
    FHoverIndex: Integer;
    FNodeRects: array of TRect;
    FOnSelect: TOBDEcuSelectEvent;
    procedure SetBuses(AValue: TOBDEcuBusCollection);
    procedure SetEcus(AValue: TOBDEcuCollection);
    procedure SetSelectedAddress(const AValue: string);
    procedure SetItemIndex(AValue: Integer);
    function UsePreviewData: Boolean;
    function VisualBusCount: Integer;
    function VisualEcuCount: Integer;
    function VisualBusName(AIndex: Integer): string;
    function VisualBusSpeed(AIndex: Integer): string;
    function VisualEcuAddress(AIndex: Integer): string;
    function VisualEcuName(AIndex: Integer): string;
    function VisualEcuPart(AIndex: Integer): string;
    function VisualEcuSystem(AIndex: Integer): string;
    function VisualEcuBus(AIndex: Integer): Integer;
    function VisualEcuState(AIndex: Integer): TOBDEcuNodeState;
    function VisualEcuFaults(AIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function FooterHeight: Integer;
    function StateColor(APainter: TOBDPainter; AState: TOBDEcuNodeState): TColor;
    function BodyTop: Integer;
    procedure LayoutNodeRects;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawTesterGateway(APainter: TOBDPainter; ATop, ABodyHeight,
      ANodeHeight: Integer; out APortX, ALaneX, ANodesX, ATermX: Integer);
    procedure DrawNode(APainter: TOBDPainter; AIndex: Integer;
      const R: TRect; ASelected, AHover: Boolean);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DoSelect(AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the topology card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks node hover.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Selects a node under the pointer.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Keyboard navigation between topology nodes.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Enables arrow-key input.</summary>
    /// <param name="Message">Dialog-code message.</param>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    /// <summary>Notifies the control that the bus or ECU model changed.</summary>
    procedure DataChanged;
    /// <summary>Creates an empty topology view.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the shared model collections.</summary>
    destructor Destroy; override;
    /// <summary>Relayouts nodes for the new density.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes all buses and ECUs.</summary>
    procedure Clear;
    /// <summary>Adds a bus lane.</summary>
    /// <param name="AName">Bus name.</param>
    /// <param name="ASpeedText">Bus speed text.</param>
    /// <returns>The new bus item.</returns>
    function AddBus(const AName, ASpeedText: string): TOBDEcuBusItem;
    /// <summary>Adds an ECU to the shared model.</summary>
    /// <param name="AAddress">Diagnostic address.</param>
    /// <param name="AName">Control-unit name.</param>
    /// <param name="APartNumber">Part number.</param>
    /// <param name="ABusIndex">Bus index.</param>
    /// <param name="AState">Node state.</param>
    /// <param name="AFaultCount">Fault count.</param>
    /// <returns>The new ECU item.</returns>
    function AddEcu(const AAddress, AName, APartNumber: string;
      ABusIndex: Integer; AState: TOBDEcuNodeState;
      AFaultCount: Integer = 0): TOBDEcuItem;
    /// <summary>Returns the ECU index at a client coordinate.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>ECU index, or -1.</returns>
    function NodeAt(X, Y: Integer): Integer;
    /// <summary>Selected ECU item index, or -1.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  published
    /// <summary>Bus lanes in display order.</summary>
    property Buses: TOBDEcuBusCollection read FBuses write SetBuses;
    /// <summary>Control units placed on bus lanes.</summary>
    property Ecus: TOBDEcuCollection read FEcus write SetEcus;
    /// <summary>Selected diagnostic address.</summary>
    property SelectedAddress: string read FSelectedAddress
      write SetSelectedAddress;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the selected ECU changes.</summary>
    property OnSelect: TOBDEcuSelectEvent read FOnSelect write FOnSelect;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

  /// <summary>Scrollable list card for the same ECU model.</summary>
  TOBDEcuList = class(TOBDCustomControl, IOBDSurface)
  strict private
    FBuses: TOBDEcuBusCollection;
    FEcus: TOBDEcuCollection;
    FSelectedAddress: string;
    FItemIndex: Integer;
    FHoverIndex: Integer;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FOnSelect: TOBDEcuSelectEvent;
    procedure SetBuses(AValue: TOBDEcuBusCollection);
    procedure SetEcus(AValue: TOBDEcuCollection);
    procedure SetSelectedAddress(const AValue: string);
    procedure SetItemIndex(AValue: Integer);
    procedure SetScrollPos(AValue: Integer);
    function UsePreviewData: Boolean;
    function VisualEcuCount: Integer;
    function VisualFaultCount: Integer;
    function VisualEcuAddress(AIndex: Integer): string;
    function VisualEcuName(AIndex: Integer): string;
    function VisualEcuPart(AIndex: Integer): string;
    function VisualEcuSystem(AIndex: Integer): string;
    function VisualEcuState(AIndex: Integer): TOBDEcuNodeState;
    function VisualEcuFaults(AIndex: Integer): Integer;
    function HeaderHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollBarRect: TRect;
    function MaxScroll: Integer;
    function ScrollThumbRect: TRect;
    function HitRow(X, Y: Integer): Integer;
    function StatusText(AState: TOBDEcuNodeState; AFaults: Integer): string;
    function StateColor(APainter: TOBDPainter; AState: TOBDEcuNodeState): TColor;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawColumnHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure DoSelect(AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the ECU list card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks row and scroll-bar hover.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Selects a row or starts scroll-bar dragging.</summary>
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
    /// <summary>Handles arrow and page keys.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Scrolls the list down.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    /// <summary>Scrolls the list up.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
  public
    /// <summary>Notifies the control that the bus or ECU model changed.</summary>
    procedure DataChanged;
    /// <summary>Creates an empty ECU list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the shared model collections.</summary>
    destructor Destroy; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes all buses and ECUs.</summary>
    procedure Clear;
    /// <summary>Adds a bus lane.</summary>
    /// <param name="AName">Bus name.</param>
    /// <param name="ASpeedText">Bus speed text.</param>
    /// <returns>The new bus item.</returns>
    function AddBus(const AName, ASpeedText: string): TOBDEcuBusItem;
    /// <summary>Adds one ECU row.</summary>
    /// <param name="AAddress">Diagnostic address.</param>
    /// <param name="AName">Control-unit name.</param>
    /// <param name="APartNumber">Part number.</param>
    /// <param name="ABusIndex">Bus index.</param>
    /// <param name="AState">Node state.</param>
    /// <param name="AFaultCount">Fault count.</param>
    /// <returns>The new ECU item.</returns>
    function AddEcu(const AAddress, AName, APartNumber: string;
      ABusIndex: Integer; AState: TOBDEcuNodeState;
      AFaultCount: Integer = 0): TOBDEcuItem;
    /// <summary>Selected ECU item index, or -1.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  published
    /// <summary>Bus lanes in display order.</summary>
    property Buses: TOBDEcuBusCollection read FBuses write SetBuses;
    /// <summary>Control units in display order.</summary>
    property Ecus: TOBDEcuCollection read FEcus write SetEcus;
    /// <summary>Selected diagnostic address.</summary>
    property SelectedAddress: string read FSelectedAddress
      write SetSelectedAddress;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the selected ECU changes.</summary>
    property OnSelect: TOBDEcuSelectEvent read FOnSelect write FOnSelect;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

const
  ECU_PREVIEW_COUNT = 14;
  ECU_PREVIEW_BUS_COUNT = 5;

function PreviewBusName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Powertrain CAN';
    1: Result := 'Chassis CAN';
    2: Result := 'Comfort CAN';
    3: Result := 'Infotainment CAN';
  else Result := 'Extended CAN';
  end;
end;

function PreviewBusSpeed(AIndex: Integer): string;
begin
  Result := '500 kbit/s';
end;

function PreviewEcuAddress(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := '01'; 1: Result := '02'; 2: Result := '03';
    3: Result := '08'; 4: Result := '09'; 5: Result := '13';
    6: Result := '15'; 7: Result := '16'; 8: Result := '17';
    9: Result := '19'; 10: Result := '44'; 11: Result := '53';
    12: Result := '5F';
  else Result := 'A5';
  end;
end;

function PreviewEcuName(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'Engine electronics'; 1: Result := 'Transmission (DSG)';
    2: Result := 'Brakes (ABS/ESC)'; 3: Result := 'Climatronic';
    4: Result := 'Central electrics'; 5: Result := 'Distance regulation';
    6: Result := 'Airbag'; 7: Result := 'Steering column';
    8: Result := 'Instruments'; 9: Result := 'Gateway';
    10: Result := 'Steering assist'; 11: Result := 'Parking brake';
    12: Result := 'Infotainment';
  else Result := 'Front camera';
  end;
end;

function PreviewEcuPart(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := '04L906056HT'; 1: Result := '0AM300050R';
    2: Result := '5Q0907379AA'; 3: Result := '5G0907044EM';
    4: Result := '5Q0937084CK'; 5: Result := '5Q0907572H';
    6: Result := '5Q0959655BJ'; 7: Result := '5Q0953507DH';
    8: Result := '5G1920741A'; 9: Result := '5Q0907530AH';
    10: Result := '5Q0909144R'; 11: Result := '5Q0907801B';
    12: Result := '5G0035867A';
  else Result := '3Q0980653';
  end;
end;

function PreviewEcuSystem(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := 'EDC17C64'; 1: Result := 'DQ200'; 2: Result := 'MK100';
    4: Result := 'BCM MQB'; 5: Result := 'ACC'; 9: Result := 'J533';
    12: Result := 'MIB2';
  else Result := '';
  end;
end;

function PreviewEcuBus(AIndex: Integer): Integer;
begin
  case AIndex of
    0, 1: Result := 0;
    2, 6, 10, 11: Result := 1;
    3, 4, 7, 8, 9: Result := 2;
    12: Result := 3;
  else Result := 4;
  end;
end;

function PreviewEcuState(AIndex: Integer): TOBDEcuNodeState;
begin
  case AIndex of
    0, 2, 4, 6, 10: Result := ensFaults;
    12: Result := ensLocked;
    13: Result := ensNoResponse;
  else Result := ensOk;
  end;
end;

function PreviewEcuFaults(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := 3;
    2, 4, 6, 10, 12: Result := 1;
  else Result := 0;
  end;
end;

{ TOBDEcuBusItem }

constructor TOBDEcuBusItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := 'CAN';
  FSpeedText := '500 kbit/s';
end;

procedure TOBDEcuBusItem.Assign(Source: TPersistent);
var
  Item: TOBDEcuBusItem;
begin
  if Source is TOBDEcuBusItem then
  begin
    Item := TOBDEcuBusItem(Source);
    FName := Item.Name;
    FSpeedText := Item.SpeedText;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDEcuBusItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDEcuBusItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDEcuBusItem.SetSpeedText(const AValue: string);
begin
  if FSpeedText = AValue then
    Exit;
  FSpeedText := AValue;
  Changed(False);
end;

{ TOBDEcuBusCollection }

constructor TOBDEcuBusCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDEcuBusItem);
end;

function TOBDEcuBusCollection.Add: TOBDEcuBusItem;
begin
  Result := TOBDEcuBusItem(inherited Add);
end;

procedure TOBDEcuBusCollection.Assign(Source: TPersistent);
var
  Src: TOBDEcuBusCollection;
  I: Integer;
begin
  if Source is TOBDEcuBusCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDEcuBusCollection(Source);
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

function TOBDEcuBusCollection.GetItem(Index: Integer): TOBDEcuBusItem;
begin
  Result := TOBDEcuBusItem(inherited GetItem(Index));
end;

procedure TOBDEcuBusCollection.SetItem(Index: Integer; AValue: TOBDEcuBusItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDEcuBusCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDEcuTopology then
    TOBDEcuTopology(GetOwner).DataChanged
  else if GetOwner is TOBDEcuList then
    TOBDEcuList(GetOwner).DataChanged;
end;

{ TOBDEcuItem }

constructor TOBDEcuItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := ensOk;
end;

procedure TOBDEcuItem.Assign(Source: TPersistent);
var
  Item: TOBDEcuItem;
begin
  if Source is TOBDEcuItem then
  begin
    Item := TOBDEcuItem(Source);
    FAddress := Item.Address;
    FName := Item.Name;
    FPartNumber := Item.PartNumber;
    FSystemText := Item.SystemText;
    FBusIndex := Item.BusIndex;
    FState := Item.State;
    FFaultCount := Item.FaultCount;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDEcuItem.GetDisplayName: string;
begin
  if FAddress <> '' then
    Result := FAddress + ' ' + FName
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDEcuItem.SetAddress(const AValue: string);
begin
  if FAddress = AValue then
    Exit;
  FAddress := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetPartNumber(const AValue: string);
begin
  if FPartNumber = AValue then
    Exit;
  FPartNumber := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetSystemText(const AValue: string);
begin
  if FSystemText = AValue then
    Exit;
  FSystemText := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetBusIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FBusIndex = AValue then
    Exit;
  FBusIndex := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetState(AValue: TOBDEcuNodeState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

procedure TOBDEcuItem.SetFaultCount(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FFaultCount = AValue then
    Exit;
  FFaultCount := AValue;
  Changed(False);
end;

{ TOBDEcuCollection }

constructor TOBDEcuCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDEcuItem);
end;

function TOBDEcuCollection.Add: TOBDEcuItem;
begin
  Result := TOBDEcuItem(inherited Add);
end;

procedure TOBDEcuCollection.Assign(Source: TPersistent);
var
  Src: TOBDEcuCollection;
  I: Integer;
begin
  if Source is TOBDEcuCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDEcuCollection(Source);
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

function TOBDEcuCollection.GetItem(Index: Integer): TOBDEcuItem;
begin
  Result := TOBDEcuItem(inherited GetItem(Index));
end;

procedure TOBDEcuCollection.SetItem(Index: Integer; AValue: TOBDEcuItem);
begin
  inherited SetItem(Index, AValue);
end;

function TOBDEcuCollection.IndexOfAddress(const AAddress: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SameText(Items[I].Address, AAddress) then
      Exit(I);
end;

procedure TOBDEcuCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDEcuTopology then
    TOBDEcuTopology(GetOwner).DataChanged
  else if GetOwner is TOBDEcuList then
    TOBDEcuList(GetOwner).DataChanged;
end;

{ TOBDEcuTopology }

constructor TOBDEcuTopology.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 900;
  Height := 520;
  TabStop := True;
  FItemIndex := -1;
  FHoverIndex := -1;
  FBuses := TOBDEcuBusCollection.Create(Self);
  FEcus := TOBDEcuCollection.Create(Self);
end;

destructor TOBDEcuTopology.Destroy;
begin
  FEcus.Free;
  FBuses.Free;
  inherited;
end;

procedure TOBDEcuTopology.DensityChanged;
begin
  inherited;
  LayoutNodeRects;
end;

function TOBDEcuTopology.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDEcuTopology.Clear;
begin
  FBuses.Clear;
  FEcus.Clear;
  FItemIndex := -1;
  FSelectedAddress := '';
  DataChanged;
end;

function TOBDEcuTopology.AddBus(const AName, ASpeedText: string): TOBDEcuBusItem;
begin
  Result := FBuses.Add;
  Result.Name := AName;
  Result.SpeedText := ASpeedText;
end;

function TOBDEcuTopology.AddEcu(const AAddress, AName, APartNumber: string;
  ABusIndex: Integer; AState: TOBDEcuNodeState;
  AFaultCount: Integer): TOBDEcuItem;
begin
  Result := FEcus.Add;
  Result.Address := AAddress;
  Result.Name := AName;
  Result.PartNumber := APartNumber;
  Result.BusIndex := ABusIndex;
  Result.State := AState;
  Result.FaultCount := AFaultCount;
  if FItemIndex < 0 then
    SetItemIndex(Result.Index);
end;

procedure TOBDEcuTopology.SetBuses(AValue: TOBDEcuBusCollection);
begin
  FBuses.Assign(AValue);
end;

procedure TOBDEcuTopology.SetEcus(AValue: TOBDEcuCollection);
begin
  FEcus.Assign(AValue);
end;

procedure TOBDEcuTopology.SetSelectedAddress(const AValue: string);
var
  I, J: Integer;
begin
  if FSelectedAddress = AValue then
    Exit;
  if UsePreviewData then
  begin
    I := -1;
    for J := 0 to ECU_PREVIEW_COUNT - 1 do
      if SameText(PreviewEcuAddress(J), AValue) then
      begin
        I := J;
        Break;
      end;
  end
  else
    I := FEcus.IndexOfAddress(AValue);
  if (not UsePreviewData) and (I < 0) and (AValue <> '') then
  begin
    FSelectedAddress := AValue;
    FItemIndex := -1;
    Invalidate;
    Exit;
  end;
  SetItemIndex(I);
end;

procedure TOBDEcuTopology.SetItemIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, VisualEcuCount - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  if FItemIndex >= 0 then
    FSelectedAddress := VisualEcuAddress(FItemIndex)
  else
    FSelectedAddress := '';
  Invalidate;
  if Assigned(FOnSelect) then
    FOnSelect(Self, FSelectedAddress, FItemIndex);
end;

procedure TOBDEcuTopology.DataChanged;
begin
  if FItemIndex >= VisualEcuCount then
    FItemIndex := VisualEcuCount - 1;
  if FItemIndex < 0 then
    FSelectedAddress := ''
  else
    FSelectedAddress := VisualEcuAddress(FItemIndex);
  LayoutNodeRects;
  Invalidate;
end;

function TOBDEcuTopology.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FEcus.Count = 0) and (FBuses.Count = 0);
end;

function TOBDEcuTopology.VisualBusCount: Integer;
begin
  if UsePreviewData then
    Result := ECU_PREVIEW_BUS_COUNT
  else
    Result := FBuses.Count;
end;

function TOBDEcuTopology.VisualEcuCount: Integer;
begin
  if UsePreviewData then
    Result := ECU_PREVIEW_COUNT
  else
    Result := FEcus.Count;
end;

function TOBDEcuTopology.VisualBusName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewBusName(AIndex)
  else
    Result := FBuses[AIndex].Name;
end;

function TOBDEcuTopology.VisualBusSpeed(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewBusSpeed(AIndex)
  else
    Result := FBuses[AIndex].SpeedText;
end;

function TOBDEcuTopology.VisualEcuAddress(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuAddress(AIndex)
  else
    Result := FEcus[AIndex].Address;
end;

function TOBDEcuTopology.VisualEcuName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuName(AIndex)
  else
    Result := FEcus[AIndex].Name;
end;

function TOBDEcuTopology.VisualEcuPart(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuPart(AIndex)
  else
    Result := FEcus[AIndex].PartNumber;
end;

function TOBDEcuTopology.VisualEcuSystem(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuSystem(AIndex)
  else
    Result := FEcus[AIndex].SystemText;
end;

function TOBDEcuTopology.VisualEcuBus(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewEcuBus(AIndex)
  else
    Result := FEcus[AIndex].BusIndex;
end;

function TOBDEcuTopology.VisualEcuState(AIndex: Integer): TOBDEcuNodeState;
begin
  if UsePreviewData then
    Result := PreviewEcuState(AIndex)
  else
    Result := FEcus[AIndex].State;
end;

function TOBDEcuTopology.VisualEcuFaults(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewEcuFaults(AIndex)
  else
    Result := FEcus[AIndex].FaultCount;
end;

function TOBDEcuTopology.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDEcuTopology.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDEcuTopology.BodyTop: Integer;
begin
  Result := HeaderHeight + ScaleValue(20);
end;

function TOBDEcuTopology.StateColor(APainter: TOBDPainter;
  AState: TOBDEcuNodeState): TColor;
begin
  case AState of
    ensFaults: Result := APainter.Palette.Danger;
    ensLocked: Result := APainter.AccentText;
    ensNoResponse, ensNotScanned: Result := APainter.Palette.Subtle;
  else
    Result := APainter.Palette.Success;
  end;
end;

procedure TOBDEcuTopology.LayoutNodeRects;
var
  BusN, EcuN, I, J, K, MaxOnBus, NodeW, NodeH, LaneH, TopY, BusY,
    PortX, LabelX, NodesX, TermX, CountOnBus: Integer;
begin
  EcuN := VisualEcuCount;
  SetLength(FNodeRects, EcuN);
  for I := 0 to EcuN - 1 do
    FNodeRects[I] := Rect(0, 0, 0, 0);
  BusN := VisualBusCount;
  if (BusN <= 0) or (EcuN <= 0) or (ClientWidth <= 0) then
    Exit;

  NodeH := ScaleValue(52);
  if Density = dnTablet then
    NodeH := ScaleValue(64);
  LaneH := NodeH + ScaleValue(28);
  TopY := BodyTop;
  PortX := ScaleValue(220);
  LabelX := PortX + ScaleValue(24);
  NodesX := PortX + ScaleValue(190);
  TermX := ClientWidth - ScaleValue(28);
  MaxOnBus := 1;
  for I := 0 to BusN - 1 do
  begin
    CountOnBus := 0;
    for J := 0 to EcuN - 1 do
      if VisualEcuBus(J) = I then
        Inc(CountOnBus);
    MaxOnBus := System.Math.Max(MaxOnBus, CountOnBus);
  end;
  NodeW := (TermX - ScaleValue(20) - NodesX - (MaxOnBus - 1) *
    ScaleValue(16)) div MaxOnBus;
  NodeW := System.Math.Max(ScaleValue(120), NodeW);
  for I := 0 to BusN - 1 do
  begin
    BusY := TopY + I * LaneH;
    K := 0;
    for J := 0 to EcuN - 1 do
      if VisualEcuBus(J) = I then
      begin
        FNodeRects[J] := Rect(NodesX + K * (NodeW + ScaleValue(16)), BusY,
          NodesX + K * (NodeW + ScaleValue(16)) + NodeW, BusY + NodeH);
        Inc(K);
      end;
  end;
end;

procedure TOBDEcuTopology.DrawHeader(APainter: TOBDPainter);
var
  H, X, ChipX: Integer;
begin
  H := HeaderHeight;
  APainter.Glyph(glNetwork, ScaleValue(26), H div 2, APainter.AccentText, 1);
  X := ScaleValue(48);
  APainter.Text(X, H div 2 - ScaleValue(8), 'Topology', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold);
  APainter.Text(X, H div 2 + ScaleValue(12),
    Format('%d control units on %d buses behind the gateway',
    [VisualEcuCount, VisualBusCount]), OBD_TEXT_SMALL, Palette.GaugeLabel);
  ChipX := ClientWidth - ScaleValue(16) - APainter.ChipWidth('TOPOLOGY');
  APainter.Chip(ChipX, (H - ScaleValue(20)) div 2, 'TOPOLOGY',
    APainter.AccentText);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDEcuTopology.DrawTesterGateway(APainter: TOBDPainter; ATop,
  ABodyHeight, ANodeHeight: Integer; out APortX, ALaneX, ANodesX,
  ATermX: Integer);
var
  GX, GW, TH, GY, GH, CY, KY: Integer;
begin
  GX := ScaleValue(20);
  GW := ScaleValue(200);
  TH := ANodeHeight;
  APainter.RoundRect(GX, ATop, GW, TH, ScaleValue(0), APainter.HeaderFill,
    Palette.NeutralLight);
  APainter.Glyph(glVci, GX + ScaleValue(22), ATop + TH div 2,
    APainter.AccentText, 1);
  APainter.Text(GX + ScaleValue(42), ATop + TH div 2 - ScaleValue(9),
    'Tester', OBD_TEXT_BODY, Palette.ForegroundText, twSemibold);
  APainter.Text(GX + ScaleValue(42), ATop + TH div 2 + ScaleValue(10),
    'OBDLink EX ' + #$00B7 + ' OBD port', OBD_TEXT_SMALL, Palette.GaugeLabel,
    twRegular, taLeftJustify, GW - ScaleValue(52));

  GY := ATop + TH + ScaleValue(28);
  GH := ABodyHeight - TH - ScaleValue(40);
  CY := GX + GW div 2;
  APainter.VLine(CY, ATop + TH, GY - ATop - TH, Palette.NeutralDark);
  APainter.Caps(CY + ScaleValue(8), ATop + TH + ScaleValue(14),
    'Diagnostic CAN');
  APainter.Card(Rect(GX, GY, GX + GW, GY + GH), Palette.Accent);
  APainter.Glyph(glNetwork, GX + ScaleValue(26), GY + ScaleValue(26),
    APainter.AccentText, 1.5);
  APainter.Text(GX + ScaleValue(48), GY + ScaleValue(18), '19 Gateway',
    OBD_TEXT_HEADING, Palette.ForegroundText, twBold);
  APainter.Text(GX + ScaleValue(48), GY + ScaleValue(36),
    'J533 ' + #$00B7 + ' 5Q0907530AH', OBD_TEXT_SMALL, Palette.GaugeLabel,
    twRegular, taLeftJustify, GW - ScaleValue(56));
  KY := GY + ScaleValue(70);
  APainter.Text(GX + ScaleValue(18), KY, 'Buses', OBD_TEXT_BODY,
    Palette.GaugeLabel);
  APainter.Text(GX + GW - ScaleValue(18), KY, IntToStr(VisualBusCount),
    OBD_TEXT_BODY, Palette.ForegroundText, twSemibold, taRightJustify);
  Inc(KY, ScaleValue(24));
  APainter.Text(GX + ScaleValue(18), KY, 'Units', OBD_TEXT_BODY,
    Palette.GaugeLabel);
  APainter.Text(GX + GW - ScaleValue(18), KY, IntToStr(VisualEcuCount),
    OBD_TEXT_BODY, Palette.ForegroundText, twSemibold, taRightJustify);
  Inc(KY, ScaleValue(24));
  APainter.Text(GX + ScaleValue(18), KY, 'Wake-up', OBD_TEXT_BODY,
    Palette.GaugeLabel);
  APainter.Text(GX + GW - ScaleValue(18), KY, 'On', OBD_TEXT_BODY,
    Palette.ForegroundText, twSemibold, taRightJustify);

  APortX := GX + GW;
  ALaneX := APortX + ScaleValue(24);
  ANodesX := APortX + ScaleValue(190);
  ATermX := ClientWidth - ScaleValue(28);
end;

procedure TOBDEcuTopology.DrawNode(APainter: TOBDPainter; AIndex: Integer;
  const R: TRect; ASelected, AHover: Boolean);
var
  Edge, Fill, Ink: TColor;
  Mid, AddrW, RX: Integer;
  S: string;
begin
  Edge := StateColor(APainter, VisualEcuState(AIndex));
  if ASelected then
    Fill := APainter.Tint(Palette.Accent, 0.12)
  else if AHover then
    Fill := APainter.HeaderFill
  else
    Fill := Palette.GaugeFace;
  APainter.Card(R, Edge, Fill);
  if ASelected then
    APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(8),
      clNone, APainter.AccentText, 2);
  Mid := (R.Top + R.Bottom) div 2;
  if VisualEcuState(AIndex) = ensNoResponse then
    Ink := Palette.GaugeLabel
  else
    Ink := Palette.ForegroundText;
  AddrW := APainter.Text(R.Left + ScaleValue(16), Mid - ScaleValue(9),
    VisualEcuAddress(AIndex), OBD_TEXT_BODY, Ink, twMonoBold);
  APainter.Text(R.Left + ScaleValue(24) + AddrW, Mid - ScaleValue(9),
    VisualEcuName(AIndex), OBD_TEXT_BODY, Ink, twSemibold, taLeftJustify,
    R.Width - AddrW - ScaleValue(72));
  S := VisualEcuSystem(AIndex);
  if S = '' then
    S := VisualEcuPart(AIndex);
  APainter.Text(R.Left + ScaleValue(16), Mid + ScaleValue(10), S,
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
    R.Width - ScaleValue(56));
  RX := R.Right - ScaleValue(14);
  case VisualEcuState(AIndex) of
    ensFaults:
      APainter.Badge(RX, Mid - ScaleValue(9),
        IntToStr(System.Math.Max(1, VisualEcuFaults(AIndex))), Palette.Danger);
    ensLocked:
      APainter.Glyph(glLock, RX - ScaleValue(8), Mid, APainter.AccentText, 1);
    ensNoResponse, ensNotScanned:
      APainter.GlyphDash(RX - ScaleValue(8), Mid, Palette.Subtle, 0.8);
  else
    APainter.GlyphCheck(RX - ScaleValue(8), Mid, Palette.Success, 0.7);
  end;
end;

procedure TOBDEcuTopology.DrawFooter(APainter: TOBDPainter);
var
  FY, CY, X, I, TW: Integer;
  C: TColor;
  Labels: array [0 .. 3] of string;
  Colors: array [0 .. 3] of TColor;
begin
  FY := ClientHeight - FooterHeight;
  CY := FY + FooterHeight div 2;
  APainter.HLine(1, FY, ClientWidth - 2, Palette.NeutralLight);
  APainter.Text(ScaleValue(16), CY, 'Select a unit to open it ' + #$00B7 +
    ' scanned 18:40', OBD_TEXT_SMALL, Palette.GaugeLabel);
  Labels[0] := 'OK'; Labels[1] := 'Faults'; Labels[2] := 'Protected';
  Labels[3] := 'No reply';
  Colors[0] := Palette.Success; Colors[1] := Palette.Danger;
  Colors[2] := APainter.AccentText; Colors[3] := Palette.Subtle;
  X := ClientWidth - ScaleValue(16);
  for I := 3 downto 0 do
  begin
    TW := APainter.TextWidth(Labels[I], OBD_TEXT_SMALL);
    APainter.Text(X - TW, CY, Labels[I], OBD_TEXT_SMALL, Palette.GaugeLabel);
    C := Colors[I];
    APainter.FillRect(Rect(X - TW - ScaleValue(14), CY - ScaleValue(6),
      X - TW - ScaleValue(10), CY + ScaleValue(6)), C);
    Dec(X, TW + ScaleValue(32));
  end;
end;

procedure TOBDEcuTopology.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  BusN, EcuN, I, J, NodeH, LaneH, BodyH, TopY, PortX, LabX, NodesX,
    TermX, BusY, LineY, EndX, Faults, Save: Integer;
  BusHasNode: Boolean;
  S: string;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    P.Card(ClientRect);
    DrawHeader(P);
    BusN := VisualBusCount;
    EcuN := VisualEcuCount;
    if (BusN = 0) or (EcuN = 0) then
    begin
      P.Text(ClientWidth div 2, HeaderHeight + (ClientHeight - HeaderHeight -
        FooterHeight) div 2, 'No ECU topology data', OBD_TEXT_HEADING,
        Palette.GaugeLabel, twSemibold, taCenter);
      DrawFooter(P);
      Exit;
    end;
    LayoutNodeRects;
    NodeH := ScaleValue(52);
    if Density = dnTablet then
      NodeH := ScaleValue(64);
    LaneH := NodeH + ScaleValue(28);
    TopY := BodyTop;
    BodyH := BusN * LaneH;
    DrawTesterGateway(P, TopY, BodyH, NodeH, PortX, LabX, NodesX, TermX);
    Save := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, PortX, HeaderHeight + 1,
        ClientWidth - 1, ClientHeight - FooterHeight);
      for I := 0 to BusN - 1 do
      begin
        BusY := TopY + I * LaneH;
        LineY := BusY + NodeH div 2;
        if Odd(I) then
          P.FillRect(Rect(PortX + 1, BusY - ScaleValue(10), ClientWidth - 1,
            BusY - ScaleValue(10) + LaneH), OBDMixColor(P.HeaderFill,
            Palette.GaugeFace, 0.5));
        EndX := PortX + ScaleValue(80);
        Faults := 0;
        BusHasNode := False;
        for J := 0 to EcuN - 1 do
          if VisualEcuBus(J) = I then
          begin
            BusHasNode := True;
            EndX := System.Math.Max(EndX, FNodeRects[J].Right + ScaleValue(20));
            if VisualEcuState(J) = ensFaults then
              Inc(Faults, System.Math.Max(1, VisualEcuFaults(J)));
          end;
        if BusHasNode then
        begin
          P.FillRect(Rect(PortX, LineY - ScaleValue(1), EndX, LineY +
            ScaleValue(2)), Palette.NeutralDark);
          P.FillRect(Rect(EndX, LineY - ScaleValue(8), EndX + ScaleValue(3),
            LineY + ScaleValue(8)), Palette.NeutralDark);
        end;
        P.RoundRect(PortX - ScaleValue(4), LineY - ScaleValue(5),
          ScaleValue(8), ScaleValue(10), ScaleValue(2), Palette.GaugeFace,
          Palette.NeutralDark, 1.5);
        P.Text(LabX, LineY - ScaleValue(13), VisualBusName(I), OBD_TEXT_BODY,
          Palette.ForegroundText, twSemibold);
        S := VisualBusSpeed(I);
        if Faults > 0 then
          S := S + ' ' + #$00B7 + ' ' + IntToStr(Faults) + ' fault';
        if Faults > 1 then
          S := S + 's';
        if Faults > 0 then
          P.Text(LabX, LineY + ScaleValue(13), S, OBD_TEXT_SMALL,
            P.DangerInk)
        else
          P.Text(LabX, LineY + ScaleValue(13), S, OBD_TEXT_SMALL,
            Palette.GaugeLabel);
      end;
      for I := 0 to EcuN - 1 do
        DrawNode(P, I, FNodeRects[I], I = FItemIndex, I = FHoverIndex);
    finally
      RestoreDC(ACanvas.Handle, Save);
    end;
    DrawFooter(P);
  finally
    P.Free;
  end;
end;

function TOBDEcuTopology.NodeAt(X, Y: Integer): Integer;
var
  I: Integer;
begin
  LayoutNodeRects;
  Result := -1;
  for I := 0 to Length(FNodeRects) - 1 do
    if PtInRect(FNodeRects[I], Point(X, Y)) then
      Exit(I);
end;

procedure TOBDEcuTopology.DoSelect(AIndex: Integer);
begin
  SetItemIndex(AIndex);
end;

procedure TOBDEcuTopology.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  H: Integer;
begin
  inherited;
  H := NodeAt(X, Y);
  if FHoverIndex <> H then
  begin
    FHoverIndex := H;
    Invalidate;
  end;
end;

procedure TOBDEcuTopology.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  H: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  SetFocus;
  H := NodeAt(X, Y);
  if H >= 0 then
    DoSelect(H);
end;

procedure TOBDEcuTopology.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT, VK_UP:
      begin
        SetItemIndex(System.Math.Max(0, FItemIndex - 1));
        Key := 0;
      end;
    VK_RIGHT, VK_DOWN:
      begin
        SetItemIndex(System.Math.Min(VisualEcuCount - 1, FItemIndex + 1));
        Key := 0;
      end;
  end;
end;

procedure TOBDEcuTopology.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TOBDEcuTopology.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

{ TOBDEcuList }

constructor TOBDEcuList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 680;
  Height := 460;
  TabStop := True;
  FItemIndex := -1;
  FHoverIndex := -1;
  FBuses := TOBDEcuBusCollection.Create(Self);
  FEcus := TOBDEcuCollection.Create(Self);
end;

destructor TOBDEcuList.Destroy;
begin
  FEcus.Free;
  FBuses.Free;
  inherited;
end;

function TOBDEcuList.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDEcuList.Clear;
begin
  FBuses.Clear;
  FEcus.Clear;
  FItemIndex := -1;
  FSelectedAddress := '';
  FScrollPos := 0;
  DataChanged;
end;

function TOBDEcuList.AddBus(const AName, ASpeedText: string): TOBDEcuBusItem;
begin
  Result := FBuses.Add;
  Result.Name := AName;
  Result.SpeedText := ASpeedText;
end;

function TOBDEcuList.AddEcu(const AAddress, AName, APartNumber: string;
  ABusIndex: Integer; AState: TOBDEcuNodeState;
  AFaultCount: Integer): TOBDEcuItem;
begin
  Result := FEcus.Add;
  Result.Address := AAddress;
  Result.Name := AName;
  Result.PartNumber := APartNumber;
  Result.BusIndex := ABusIndex;
  Result.State := AState;
  Result.FaultCount := AFaultCount;
  if FItemIndex < 0 then
    SetItemIndex(Result.Index);
end;

procedure TOBDEcuList.SetBuses(AValue: TOBDEcuBusCollection);
begin
  FBuses.Assign(AValue);
end;

procedure TOBDEcuList.SetEcus(AValue: TOBDEcuCollection);
begin
  FEcus.Assign(AValue);
end;

procedure TOBDEcuList.SetSelectedAddress(const AValue: string);
var
  I, J: Integer;
begin
  if FSelectedAddress = AValue then
    Exit;
  if UsePreviewData then
  begin
    I := -1;
    for J := 0 to ECU_PREVIEW_COUNT - 1 do
      if SameText(PreviewEcuAddress(J), AValue) then
      begin
        I := J;
        Break;
      end;
  end
  else
    I := FEcus.IndexOfAddress(AValue);
  SetItemIndex(I);
end;

procedure TOBDEcuList.SetItemIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, VisualEcuCount - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  if FItemIndex >= 0 then
    FSelectedAddress := VisualEcuAddress(FItemIndex)
  else
    FSelectedAddress := '';
  if FItemIndex >= 0 then
  begin
    if FItemIndex * RowHeight < FScrollPos then
      SetScrollPos(FItemIndex * RowHeight)
    else if (FItemIndex + 1) * RowHeight > FScrollPos + BodyRect.Height then
      SetScrollPos((FItemIndex + 1) * RowHeight - BodyRect.Height);
  end;
  Invalidate;
  if Assigned(FOnSelect) then
    FOnSelect(Self, FSelectedAddress, FItemIndex);
end;

procedure TOBDEcuList.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDEcuList.DataChanged;
begin
  if FItemIndex >= VisualEcuCount then
    FItemIndex := VisualEcuCount - 1;
  if FItemIndex < 0 then
    FSelectedAddress := ''
  else
    FSelectedAddress := VisualEcuAddress(FItemIndex);
  SetScrollPos(FScrollPos);
  Invalidate;
end;

function TOBDEcuList.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FEcus.Count = 0);
end;

function TOBDEcuList.VisualEcuCount: Integer;
begin
  if UsePreviewData then
    Result := ECU_PREVIEW_COUNT
  else
    Result := FEcus.Count;
end;

function TOBDEcuList.VisualFaultCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisualEcuCount - 1 do
    if VisualEcuState(I) = ensFaults then
      Inc(Result);
end;

function TOBDEcuList.VisualEcuAddress(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuAddress(AIndex)
  else
    Result := FEcus[AIndex].Address;
end;

function TOBDEcuList.VisualEcuName(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuName(AIndex)
  else
    Result := FEcus[AIndex].Name;
end;

function TOBDEcuList.VisualEcuPart(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuPart(AIndex)
  else
    Result := FEcus[AIndex].PartNumber;
end;

function TOBDEcuList.VisualEcuSystem(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewEcuSystem(AIndex)
  else
    Result := FEcus[AIndex].SystemText;
end;

function TOBDEcuList.VisualEcuState(AIndex: Integer): TOBDEcuNodeState;
begin
  if UsePreviewData then
    Result := PreviewEcuState(AIndex)
  else
    Result := FEcus[AIndex].State;
end;

function TOBDEcuList.VisualEcuFaults(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := PreviewEcuFaults(AIndex)
  else
    Result := FEcus[AIndex].FaultCount;
end;

function TOBDEcuList.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDEcuList.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDEcuList.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDEcuList.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row - 4);
end;

function TOBDEcuList.BodyRect: TRect;
begin
  Result := Rect(1, HeaderHeight + ColumnHeaderHeight, ClientWidth - 1,
    ClientHeight - FooterHeight);
end;

function TOBDEcuList.ScrollBarRect: TRect;
begin
  Result := Rect(ClientWidth - ScaleValue(14), BodyRect.Top,
    ClientWidth - 1, BodyRect.Bottom);
end;

function TOBDEcuList.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, VisualEcuCount * RowHeight - BodyRect.Height);
end;

function TOBDEcuList.ScrollThumbRect: TRect;
begin
  Result := OBDScrollThumbRect(ScrollBarRect, True, 0, MaxScroll,
    BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDEcuList.HitRow(X, Y: Integer): Integer;
var
  R: TRect;
begin
  Result := -1;
  R := BodyRect;
  if not PtInRect(R, Point(X, Y)) then
    Exit;
  Result := (Y - R.Top + FScrollPos) div RowHeight;
  if Result >= VisualEcuCount then
    Result := -1;
end;

function TOBDEcuList.StatusText(AState: TOBDEcuNodeState;
  AFaults: Integer): string;
begin
  case AState of
    ensFaults:
      if AFaults = 1 then
        Result := '1 FAULT'
      else
        Result := Format('%d FAULTS', [AFaults]);
    ensLocked: Result := 'PROTECTED';
    ensNoResponse: Result := 'NO REPLY';
    ensNotScanned: Result := 'NOT SCANNED';
  else
    Result := 'OK';
  end;
end;

function TOBDEcuList.StateColor(APainter: TOBDPainter;
  AState: TOBDEcuNodeState): TColor;
begin
  case AState of
    ensFaults: Result := Palette.Danger;
    ensLocked: Result := APainter.AccentText;
    ensNoResponse, ensNotScanned: Result := Palette.Subtle;
  else
    Result := Palette.Success;
  end;
end;

procedure TOBDEcuList.DrawHeader(APainter: TOBDPainter);
var
  H, X, C1, C2, ButtonW, ButtonH: Integer;
begin
  H := HeaderHeight;
  APainter.Glyph(glNetwork, ScaleValue(26), H div 2, APainter.AccentText, 1);
  X := ScaleValue(48);
  APainter.Text(X, H div 2, 'Control units', OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold);
  C1 := X + APainter.TextWidth('Control units', OBD_TEXT_TITLE, twBold) +
    ScaleValue(14);
  C2 := C1 + APainter.Chip(C1, H div 2 - ScaleValue(10),
    Format('%d FOUND', [VisualEcuCount]), Palette.Subtle) + ScaleValue(6);
  APainter.Chip(C2, H div 2 - ScaleValue(10),
    Format('%d WITH FAULTS', [VisualFaultCount]), Palette.Danger);
  ButtonW := APainter.ButtonWidth('Scan all', glNone);
  ButtonH := ScaleValue(Metrics.Button);
  APainter.Button(Rect(ClientWidth - ScaleValue(16) - ButtonW,
    (H - ButtonH) div 2, ClientWidth - ScaleValue(16),
    (H - ButtonH) div 2 + ButtonH), 'Scan all', bkPrimary, cstNormal,
    False, glNone);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDEcuList.DrawColumnHeader(APainter: TOBDPainter);
var
  Y, H: Integer;
begin
  Y := HeaderHeight;
  H := ColumnHeaderHeight;
  APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + H), APainter.HeaderFill);
  APainter.HLine(1, Y + H - 1, ClientWidth - 2, Palette.NeutralLight);
  APainter.Caps(ScaleValue(16), Y + H div 2, 'Addr');
  APainter.Caps(ScaleValue(64), Y + H div 2, 'Control unit');
  APainter.Caps(ScaleValue(270), Y + H div 2, 'Part number');
  APainter.Caps(ScaleValue(400), Y + H div 2, 'System');
  APainter.Caps(ClientWidth - ScaleValue(130), Y + H div 2, 'Status');
end;

procedure TOBDEcuList.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, I, Y, Mid, ChipX, ChipW: Integer;
  R: TRect;
  C: TColor;
  S: string;
begin
  if VisualEcuCount = 0 then
  begin
    DrawEmpty(APainter);
    Exit;
  end;
  R := BodyRect;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    for I := 0 to VisualEcuCount - 1 do
    begin
      Y := R.Top + I * RowHeight - FScrollPos;
      if (Y + RowHeight < R.Top) or (Y > R.Bottom) then
        Continue;
      Mid := Y + RowHeight div 2;
      if I = FItemIndex then
        APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + RowHeight),
          APainter.Tint(Palette.Accent, 0.10))
      else if I = FHoverIndex then
        APainter.FillRect(Rect(1, Y, ClientWidth - 1, Y + RowHeight),
          APainter.HeaderFill);
      if VisualEcuState(I) <> ensOk then
      begin
        C := StateColor(APainter, VisualEcuState(I));
        APainter.FillRect(Rect(1, Y + ScaleValue(6), ScaleValue(5),
          Y + RowHeight - ScaleValue(6)), C);
      end;
      APainter.Text(ScaleValue(16), Mid, VisualEcuAddress(I), OBD_TEXT_HEADING,
        Palette.ForegroundText, twMonoBold);
      APainter.Text(ScaleValue(64), Mid, VisualEcuName(I), OBD_TEXT_BODY,
        Palette.ForegroundText, twRegular, taLeftJustify, ScaleValue(196));
      APainter.Text(ScaleValue(270), Mid, VisualEcuPart(I), OBD_TEXT_BODY,
        Palette.GaugeLabel, twMono, taLeftJustify, ScaleValue(118));
      S := VisualEcuSystem(I);
      if S = '' then
        S := '-';
      APainter.Text(ScaleValue(400), Mid, S, OBD_TEXT_BODY,
        Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(110));
      S := StatusText(VisualEcuState(I), VisualEcuFaults(I));
      ChipW := APainter.ChipWidth(S, False, OBD_TEXT_CAPS);
      ChipX := ClientWidth - ScaleValue(130);
      if ChipX + ChipW > ClientWidth - ScaleValue(30) then
        ChipX := ClientWidth - ScaleValue(30) - ChipW;
      APainter.Chip(ChipX, Mid - ScaleValue(10), S,
        StateColor(APainter, VisualEcuState(I)), False, False, OBD_TEXT_CAPS);
      APainter.Glyph(glChevronRight, ClientWidth - ScaleValue(16), Mid,
        Palette.Subtle, 0.75);
      APainter.HLine(1, Y + RowHeight - 1, ClientWidth - 2,
        Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDEcuList.DrawFooter(APainter: TOBDPainter);
var
  FY: Integer;
begin
  FY := ClientHeight - FooterHeight;
  APainter.HLine(1, FY, ClientWidth - 2, Palette.NeutralLight);
  APainter.Text(ScaleValue(16), FY + FooterHeight div 2,
    'Scanned 18:40 ' + #$00B7 + ' 1 min 12 s ' + #$00B7 +
    ' gateway list + address probe', OBD_TEXT_SMALL, Palette.GaugeLabel,
    twRegular, taLeftJustify, ClientWidth - ScaleValue(32));
end;

procedure TOBDEcuList.DrawEmpty(APainter: TOBDPainter);
begin
  APainter.Text(ClientWidth div 2, BodyRect.Top + BodyRect.Height div 2,
    'No control units', OBD_TEXT_HEADING, Palette.GaugeLabel, twSemibold,
    taCenter);
end;

procedure TOBDEcuList.DrawScrollBar(APainter: TOBDPainter);
begin
  if MaxScroll > 0 then
    OBDPaintScrollBar(APainter, ScrollBarRect, True, 0, MaxScroll,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDEcuList.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    P.Card(ClientRect);
    DrawHeader(P);
    DrawColumnHeader(P);
    DrawRows(P, ACanvas);
    DrawScrollBar(P);
    DrawFooter(P);
  finally
    P.Free;
  end;
end;

procedure TOBDEcuList.DoSelect(AIndex: Integer);
begin
  SetItemIndex(AIndex);
end;

procedure TOBDEcuList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  H: Integer;
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
  H := HitRow(X, Y);
  if FHoverIndex <> H then
  begin
    FHoverIndex := H;
    Invalidate;
  end;
  R := ScrollBarRect;
  if FScrollHover <> PtInRect(R, Point(X, Y)) then
  begin
    FScrollHover := not FScrollHover;
    Invalidate;
  end;
end;

procedure TOBDEcuList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  H: Integer;
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
  H := HitRow(X, Y);
  if H >= 0 then
    DoSelect(H);
end;

procedure TOBDEcuList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    Invalidate;
  end;
end;

procedure TOBDEcuList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      begin
        SetItemIndex(System.Math.Max(0, FItemIndex - 1));
        Key := 0;
      end;
    VK_DOWN:
      begin
        SetItemIndex(System.Math.Min(VisualEcuCount - 1, FItemIndex + 1));
        Key := 0;
      end;
    VK_PRIOR:
      begin
        SetItemIndex(System.Math.Max(0, FItemIndex - BodyRect.Height div
          RowHeight));
        Key := 0;
      end;
    VK_NEXT:
      begin
        SetItemIndex(System.Math.Min(VisualEcuCount - 1, FItemIndex +
          BodyRect.Height div RowHeight));
        Key := 0;
      end;
  end;
end;

function TOBDEcuList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos + RowHeight);
  Result := True;
end;

function TOBDEcuList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  SetScrollPos(FScrollPos - RowHeight);
  Result := True;
end;

procedure TOBDEcuList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) or FScrollHover then
  begin
    FHoverIndex := -1;
    FScrollHover := False;
    Invalidate;
  end;
end;

procedure TOBDEcuList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
