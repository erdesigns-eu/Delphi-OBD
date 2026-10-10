//------------------------------------------------------------------------------
//  ERD.UI.Inspector
//
//  TOBDInspector - a themed Object-Inspector style property grid with
//  categories, inline editors, pick lists, check values and ellipsis buttons.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.Inspector;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Paint,
  ERD.UI.Control,
  ERD.UI.PopupList;

type
  TOBDInspectorProperty = class;
  TOBDInspectorCategory = class;
  TOBDInspectorPropertyCollection = class;
  TOBDInspectorCategoryCollection = class;

  /// <summary>Kind of value editor a property row uses.</summary>
  TOBDInspectorValueKind = (
    /// <summary>Editable text.</summary>
    ivText,
    /// <summary>Read-only text with selectable value.</summary>
    ivReadOnly,
    /// <summary>Value chosen from a fixed list.</summary>
    ivPickList,
    /// <summary>Boolean value rendered as a check box and caption.</summary>
    ivCheck,
    /// <summary>Text value with a trailing ellipsis action button.</summary>
    ivEllipsis);

  /// <summary>Property-row notification.</summary>
  /// <param name="Sender">Inspector.</param>
  /// <param name="AProperty">Property row.</param>
  TOBDInspectorPropertyEvent = procedure(Sender: TObject;
    AProperty: TOBDInspectorProperty) of object;

  /// <summary>Category-row notification.</summary>
  /// <param name="Sender">Inspector.</param>
  /// <param name="ACategory">Category row.</param>
  TOBDInspectorCategoryEvent = procedure(Sender: TObject;
    ACategory: TOBDInspectorCategory) of object;

  /// <summary>Validation hook fired before an edited text value is committed.</summary>
  /// <param name="Sender">Inspector.</param>
  /// <param name="AProperty">Property being changed.</param>
  /// <param name="NewValue">Proposed value. Handlers may replace it.</param>
  /// <param name="Accept">Set False to keep the editor open.</param>
  TOBDInspectorPropertyChangingEvent = procedure(Sender: TObject;
    AProperty: TOBDInspectorProperty; var NewValue: string;
    var Accept: Boolean) of object;

  /// <summary>One streamable property row in a category.</summary>
  TOBDInspectorProperty = class(TCollectionItem)
  strict private
    FName: string;
    FValue: string;
    FDefaultValue: string;
    FHasDefault: Boolean;
    FKind: TOBDInspectorValueKind;
    FPickList: TStrings;
    FChecked: Boolean;
    FCheckedCaption: string;
    FUncheckedCaption: string;
    FReadOnly: Boolean;
    FLevel: TOBDAlertLevel;
    FHint: string;
    FTag: NativeInt;
    FVisible: Boolean;
    FRect: TRect;
    procedure SetName(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetDefaultValue(const AValue: string);
    procedure SetHasDefault(AValue: Boolean);
    procedure SetKind(AValue: TOBDInspectorValueKind);
    procedure SetPickList(AValue: TStrings);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckedCaption(const AValue: string);
    procedure SetUncheckedCaption(const AValue: string);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetLevel(AValue: TOBDAlertLevel);
    procedure SetHint(const AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure PickListChanged(Sender: TObject);
    function GetModified: Boolean;
    function GetCategory: TOBDInspectorCategory;
  protected
    /// <summary>Returns the display name used in the collection editor.</summary>
    /// <returns>Property name or inherited fallback.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates the property row and its pick-list collection.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the pick-list collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies all streamable values from another property row.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Restores <see cref="Value"/> from <see cref="DefaultValue"/>.</summary>
    procedure ResetToDefault;
    /// <summary>Owning category, or nil while detached.</summary>
    property Category: TOBDInspectorCategory read GetCategory;
    /// <summary>Layout rectangle in content coordinates.</summary>
    property Rect: TRect read FRect write FRect;
    /// <summary>True when <see cref="HasDefault"/> and Value differs from DefaultValue.</summary>
    property Modified: Boolean read GetModified;
  published
    /// <summary>Property name shown in the left column.</summary>
    property Name: string read FName write SetName;
    /// <summary>String value shown in the value column.</summary>
    property Value: string read FValue write SetValue;
    /// <summary>Default value used by <see cref="Modified"/> and reset.</summary>
    property DefaultValue: string read FDefaultValue write SetDefaultValue;
    /// <summary>True when DefaultValue is meaningful.</summary>
    property HasDefault: Boolean read FHasDefault write SetHasDefault default False;
    /// <summary>Value editor kind.</summary>
    property Kind: TOBDInspectorValueKind read FKind write SetKind default ivText;
    /// <summary>Fixed choices for pick-list rows.</summary>
    property PickList: TStrings read FPickList write SetPickList;
    /// <summary>Boolean state for check rows. Value mirrors True or False.</summary>
    property Checked: Boolean read FChecked write SetChecked default False;
    /// <summary>Caption drawn when Checked is True.</summary>
    property CheckedCaption: string read FCheckedCaption write SetCheckedCaption;
    /// <summary>Caption drawn when Checked is False.</summary>
    property UncheckedCaption: string read FUncheckedCaption write SetUncheckedCaption;
    /// <summary>True when the text part of an ellipsis row is read-only.</summary>
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    /// <summary>Alert level used to colour read-only values and row edge.</summary>
    property Level: TOBDAlertLevel read FLevel write SetLevel default alvNormal;
    /// <summary>Optional row hint text.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>True when the row is included in layout and painting.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  /// <summary>Streamable property collection owned by a category.</summary>
  TOBDInspectorPropertyCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDInspectorProperty;
    procedure SetItem(Index: Integer; AValue: TOBDInspectorProperty);
  protected
    /// <summary>Notifies the owner when collection contents change.</summary>
    /// <param name="Item">Changed item, or nil for a structural change.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a property row.</summary>
    /// <returns>The new property row.</returns>
    function Add: TOBDInspectorProperty;
    /// <summary>Returns the category that owns this collection.</summary>
    /// <returns>Owner category, or nil.</returns>
    function OwnerCategory: TOBDInspectorCategory;
    /// <summary>Copies another property collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Indexed property rows.</summary>
    property Items[Index: Integer]: TOBDInspectorProperty read GetItem
      write SetItem; default;
    /// <summary>Fires whenever a row or the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>One collapsible category row with owned properties.</summary>
  TOBDInspectorCategory = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FCollapsed: Boolean;
    FProperties: TOBDInspectorPropertyCollection;
    FVisible: Boolean;
    FTag: NativeInt;
    FRect: TRect;
    procedure SetCaption(const AValue: TCaption);
    procedure SetCollapsed(AValue: Boolean);
    procedure SetProperties(AValue: TOBDInspectorPropertyCollection);
    procedure SetVisible(AValue: Boolean);
    procedure PropertiesChanged(Sender: TObject);
  protected
    /// <summary>Returns the category caption in the collection editor.</summary>
    /// <returns>Category caption or inherited fallback.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates the category and its property collection.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees the property collection.</summary>
    destructor Destroy; override;
    /// <summary>Copies all streamable values from another category.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Adds and initializes a property row.</summary>
    /// <param name="AName">Property name.</param>
    /// <param name="AValue">Initial value.</param>
    /// <param name="AKind">Editor kind.</param>
    /// <returns>The new property row.</returns>
    function AddProperty(const AName, AValue: string;
      AKind: TOBDInspectorValueKind = ivText): TOBDInspectorProperty;
    /// <summary>Layout rectangle in content coordinates.</summary>
    property Rect: TRect read FRect write FRect;
  published
    /// <summary>Category header text.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>True when child properties are hidden.</summary>
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    /// <summary>Properties owned by this category.</summary>
    property Properties: TOBDInspectorPropertyCollection read FProperties
      write SetProperties;
    /// <summary>True when the category participates in layout and painting.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>Streamable category collection owned by the inspector.</summary>
  TOBDInspectorCategoryCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDInspectorCategory;
    procedure SetItem(Index: Integer; AValue: TOBDInspectorCategory);
  protected
    /// <summary>Notifies the owner when collection contents change.</summary>
    /// <param name="Item">Changed item, or nil for a structural change.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a category row.</summary>
    /// <returns>The new category row.</returns>
    function Add: TOBDInspectorCategory;
    /// <summary>Copies another category collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Indexed category rows.</summary>
    property Items[Index: Integer]: TOBDInspectorCategory read GetItem
      write SetItem; default;
    /// <summary>Fires whenever a row or the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed property grid with inline editing and object-inspector navigation.</summary>
  TOBDInspector = class(TOBDCustomControl)
  strict private
    FCategories: TOBDInspectorCategoryCollection;
    FSelected: TCollectionItem;
    FReadOnly: Boolean;
    FSplitterPosition: Integer;
    FScrollPos: Integer;
    FContentHeight: Integer;
    FUpdateCount: Integer;
    FEditor: TCustomEdit;
    FEditProperty: TOBDInspectorProperty;
    FEditOriginalValue: string;
    FInternalEditorUpdate: Boolean;
    FCommitting: Boolean;
    FPopup: TOBDPopupList;
    FDraggingSplitter: Boolean;
    FDraggingScroll: Boolean;
    FScrollHover: Boolean;
    FScrollDragOffset: Integer;
    FLastMouse: TPoint;
    FIncrementalText: string;
    FIncrementalTick: Cardinal;
    FOnPropertySelect: TOBDInspectorPropertyEvent;
    FOnPropertyChanging: TOBDInspectorPropertyChangingEvent;
    FOnPropertyChanged: TOBDInspectorPropertyEvent;
    FOnPropertyButtonClick: TOBDInspectorPropertyEvent;
    FOnCategorySelect: TOBDInspectorCategoryEvent;
    FOnCategoryCollapse: TOBDInspectorCategoryEvent;
    FOnCategoryExpand: TOBDInspectorCategoryEvent;
    procedure SetCategories(AValue: TOBDInspectorCategoryCollection);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSplitterPosition(AValue: Integer);
    procedure CategoriesChanged(Sender: TObject);
    procedure EditorExit(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PopupCloseUp(Sender: TObject; AAccepted: Boolean; AIndex: Integer);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    function RowHeight: Integer;
    function GutterWidth: Integer;
    function SplitterX: Integer;
    function ScrollBarWidth: Integer;
    function MaxScroll: Integer;
    function EffectiveRight: Integer;
    function VisibleValueRect(AProperty: TOBDInspectorProperty): TRect;
    function EllipsisButtonRect(AProperty: TOBDInspectorProperty): TRect;
    function CheckBoxRect(AProperty: TOBDInspectorProperty): TRect;
    function DropDownRect(AProperty: TOBDInspectorProperty): TRect;
    function ScrollThumbRect: TRect;
    function PropertyCategory(AProperty: TOBDInspectorProperty): TOBDInspectorCategory;
    function CanEditText(AProperty: TOBDInspectorProperty): Boolean;
    function IsReadOnlyText(AProperty: TOBDInspectorProperty): Boolean;
    function CommitEditor(AKeepFocus: Boolean): Boolean;
    procedure RevertEditor;
    procedure HideEditor;
    procedure ActivateEditor(ASelectAll: Boolean);
    procedure UpdateEditorBounds;
    procedure ApplyEditorTheme;
    procedure EnsureLayout;
    procedure EnsureSelectedVisible;
    procedure SetScrollPos(AValue: Integer);
    procedure SelectItem(AItem: TCollectionItem; ASelectAll: Boolean);
    procedure SelectRelative(ADelta: Integer);
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectPage(ADirection: Integer);
    procedure ToggleCategory(ACategory: TOBDInspectorCategory);
    procedure ExpandAll(ACollapsed: Boolean);
    procedure ToggleCheck(AProperty: TOBDInspectorProperty);
    procedure CyclePickList(AProperty: TOBDInspectorProperty);
    procedure OpenPickList(AProperty: TOBDInspectorProperty);
    procedure SelectPickListPrefix(AProperty: TOBDInspectorProperty; Ch: Char);
    procedure FirePropertyChanged(AProperty: TOBDInspectorProperty);
    procedure DrawCategory(APainter: TOBDPainter; ACategory: TOBDInspectorCategory;
      const R: TRect);
    procedure DrawProperty(APainter: TOBDPainter; AProperty: TOBDInspectorProperty;
      const R: TRect);
    procedure DrawScrollBar(APainter: TOBDPainter);
    function ItemAt(const P: TPoint): TCollectionItem;
    function CategoryAt(const P: TPoint): TOBDInspectorCategory;
    function PropertyAt(const P: TPoint): TOBDInspectorProperty;
    function ValueTruncated(AProperty: TOBDInspectorProperty): Boolean;
  protected
    /// <summary>Paints the inspector surface.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Repositions child controls after resizing.</summary>
    procedure Resize; override;
    /// <summary>Handles splitter, row, scroll-bar and editor mouse clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Finishes mouse drags.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Tracks splitter and scroll-bar dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Clears scroll hover state.</summary>
    procedure MouseLeave; override;
    /// <summary>Handles double-click toggles and splitter reset.</summary>
    procedure DblClick; override;
    /// <summary>Handles object-inspector keyboard navigation.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Starts text editing or pick-list incremental search.</summary>
    /// <param name="Key">Character.</param>
    procedure KeyPress(var Key: Char); override;
    /// <summary>Scrolls or moves the selection upward.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    /// <summary>Scrolls or moves the selection downward.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="MousePos">Mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    /// <summary>Reapplies density-sensitive layout.</summary>
    procedure DensityChanged; override;
    /// <summary>Creates the inspector, editor and popup list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned collections and child controls.</summary>
    destructor Destroy; override;
    /// <summary>Copies categories and editor settings from another inspector.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Suspends expensive layout and repaint work.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes layout and repaint work.</summary>
    procedure EndUpdate;
    /// <summary>Removes all categories and properties.</summary>
    procedure Clear;
    /// <summary>Finds the first visible or hidden property with a matching name.</summary>
    /// <param name="AName">Name to search for.</param>
    /// <returns>The property row, or nil.</returns>
    function FindProperty(const AName: string): TOBDInspectorProperty;
    /// <summary>Adds a category row.</summary>
    /// <param name="ACaption">Category caption.</param>
    /// <returns>The new category.</returns>
    function AddCategory(const ACaption: string): TOBDInspectorCategory;
    /// <summary>Currently selected category or property row.</summary>
    property Selected: TCollectionItem read FSelected;
  published
    /// <summary>Streamed category collection.</summary>
    property Categories: TOBDInspectorCategoryCollection read FCategories
      write SetCategories;
    /// <summary>Name-column width in 96-DPI design pixels.</summary>
    property SplitterPosition: Integer read FSplitterPosition
      write SetSplitterPosition default 150;
    /// <summary>True when no property value can be edited.</summary>
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    /// <summary>Density inherited from the theme or stored on the control.</summary>
    property Density;
    /// <summary>True when density follows the resolved theme.</summary>
    property ParentDensity;
    /// <summary>Fires when a property row is selected.</summary>
    property OnPropertySelect: TOBDInspectorPropertyEvent read FOnPropertySelect
      write FOnPropertySelect;
    /// <summary>Fires before an edited value is committed.</summary>
    property OnPropertyChanging: TOBDInspectorPropertyChangingEvent
      read FOnPropertyChanging write FOnPropertyChanging;
    /// <summary>Fires after a property value is committed.</summary>
    property OnPropertyChanged: TOBDInspectorPropertyEvent read FOnPropertyChanged
      write FOnPropertyChanged;
    /// <summary>Fires when an ellipsis property button is clicked.</summary>
    property OnPropertyButtonClick: TOBDInspectorPropertyEvent
      read FOnPropertyButtonClick write FOnPropertyButtonClick;
    /// <summary>Fires when a category row is selected.</summary>
    property OnCategorySelect: TOBDInspectorCategoryEvent read FOnCategorySelect
      write FOnCategorySelect;
    /// <summary>Fires after a category is collapsed.</summary>
    property OnCategoryCollapse: TOBDInspectorCategoryEvent read FOnCategoryCollapse
      write FOnCategoryCollapse;
    /// <summary>Fires after a category is expanded.</summary>
    property OnCategoryExpand: TOBDInspectorCategoryEvent read FOnCategoryExpand
      write FOnCategoryExpand;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

type
  TOBDInspectorEdit = class(TCustomEdit)
  protected
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TOBDInspectorEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  TabStop := False;
end;

procedure TOBDInspectorEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  Message.Result := Message.Result and not DLGC_WANTTAB;
end;

{ TOBDInspectorProperty }

constructor TOBDInspectorProperty.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPickList := TStringList.Create;
  TStringList(FPickList).OnChange := PickListChanged;
  FKind := ivText;
  FCheckedCaption := 'On';
  FUncheckedCaption := 'Off';
  FLevel := alvNormal;
  FVisible := True;
end;

destructor TOBDInspectorProperty.Destroy;
begin
  FPickList.Free;
  inherited;
end;

procedure TOBDInspectorProperty.Assign(Source: TPersistent);
var
  P: TOBDInspectorProperty;
begin
  if Source is TOBDInspectorProperty then
  begin
    P := TOBDInspectorProperty(Source);
    FName := P.Name;
    FValue := P.Value;
    FDefaultValue := P.DefaultValue;
    FHasDefault := P.HasDefault;
    FKind := P.Kind;
    FPickList.Assign(P.PickList);
    FChecked := P.Checked;
    FCheckedCaption := P.CheckedCaption;
    FUncheckedCaption := P.UncheckedCaption;
    FReadOnly := P.ReadOnly;
    FLevel := P.Level;
    FHint := P.Hint;
    FTag := P.Tag;
    FVisible := P.Visible;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDInspectorProperty.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

function TOBDInspectorProperty.GetCategory: TOBDInspectorCategory;
begin
  Result := nil;
  if Collection is TOBDInspectorPropertyCollection then
    Result := TOBDInspectorPropertyCollection(Collection).OwnerCategory;
end;

function TOBDInspectorProperty.GetModified: Boolean;
begin
  Result := FHasDefault and (FValue <> FDefaultValue);
end;

procedure TOBDInspectorProperty.PickListChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TOBDInspectorProperty.ResetToDefault;
begin
  if FHasDefault then
    Value := FDefaultValue;
end;

procedure TOBDInspectorProperty.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetValue(const AValue: string);
begin
  if FKind = ivCheck then
  begin
    FChecked := SameText(AValue, 'True') or SameText(AValue, FCheckedCaption) or
      SameText(AValue, 'On') or (AValue = '1');
    if FChecked then
      FValue := 'True'
    else
      FValue := 'False';
    Changed(False);
    Exit;
  end;
  if FValue = AValue then
    Exit;
  FValue := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetDefaultValue(const AValue: string);
begin
  if FDefaultValue = AValue then
    Exit;
  FDefaultValue := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetHasDefault(AValue: Boolean);
begin
  if FHasDefault = AValue then
    Exit;
  FHasDefault := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetKind(AValue: TOBDInspectorValueKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  if FKind = ivCheck then
    SetChecked(FChecked)
  else
    Changed(False);
end;

procedure TOBDInspectorProperty.SetPickList(AValue: TStrings);
begin
  FPickList.Assign(AValue);
  Changed(False);
end;

procedure TOBDInspectorProperty.SetChecked(AValue: Boolean);
begin
  if (FChecked = AValue) and ((FValue = 'True') or (FValue = 'False')) then
    Exit;
  FChecked := AValue;
  if FChecked then
    FValue := 'True'
  else
    FValue := 'False';
  Changed(False);
end;

procedure TOBDInspectorProperty.SetCheckedCaption(const AValue: string);
begin
  if FCheckedCaption = AValue then
    Exit;
  FCheckedCaption := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetUncheckedCaption(const AValue: string);
begin
  if FUncheckedCaption = AValue then
    Exit;
  FUncheckedCaption := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetLevel(AValue: TOBDAlertLevel);
begin
  if FLevel = AValue then
    Exit;
  FLevel := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDInspectorProperty.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

{ TOBDInspectorPropertyCollection }

function TOBDInspectorPropertyCollection.Add: TOBDInspectorProperty;
begin
  Result := TOBDInspectorProperty(inherited Add);
end;

function TOBDInspectorPropertyCollection.OwnerCategory: TOBDInspectorCategory;
var
  O: TPersistent;
begin
  Result := nil;
  O := GetOwner;
  if O is TOBDInspectorCategory then
    Result := TOBDInspectorCategory(O);
end;

procedure TOBDInspectorPropertyCollection.Assign(Source: TPersistent);
var
  Src: TOBDInspectorPropertyCollection;
  I: Integer;
begin
  if Source is TOBDInspectorPropertyCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDInspectorPropertyCollection(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited;
end;

function TOBDInspectorPropertyCollection.GetItem(Index: Integer): TOBDInspectorProperty;
begin
  Result := TOBDInspectorProperty(inherited GetItem(Index));
end;

procedure TOBDInspectorPropertyCollection.SetItem(Index: Integer;
  AValue: TOBDInspectorProperty);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDInspectorPropertyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDInspectorCategory }

constructor TOBDInspectorCategory.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCaption := Format('Category %d', [Index + 1]);
  FProperties := TOBDInspectorPropertyCollection.Create(Self,
    TOBDInspectorProperty);
  FProperties.OnChange := PropertiesChanged;
  FVisible := True;
end;

destructor TOBDInspectorCategory.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TOBDInspectorCategory.Assign(Source: TPersistent);
var
  C: TOBDInspectorCategory;
begin
  if Source is TOBDInspectorCategory then
  begin
    C := TOBDInspectorCategory(Source);
    FCaption := C.Caption;
    FCollapsed := C.Collapsed;
    FProperties.Assign(C.Properties);
    FVisible := C.Visible;
    FTag := C.Tag;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDInspectorCategory.AddProperty(const AName, AValue: string;
  AKind: TOBDInspectorValueKind): TOBDInspectorProperty;
begin
  Result := FProperties.Add;
  Result.Name := AName;
  Result.Kind := AKind;
  if AKind = ivCheck then
    Result.Checked := SameText(AValue, 'True') or SameText(AValue, 'On') or
      (AValue = '1')
  else
    Result.Value := AValue;
end;

function TOBDInspectorCategory.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDInspectorCategory.PropertiesChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TOBDInspectorCategory.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDInspectorCategory.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  Changed(False);
end;

procedure TOBDInspectorCategory.SetProperties(AValue: TOBDInspectorPropertyCollection);
begin
  FProperties.Assign(AValue);
end;

procedure TOBDInspectorCategory.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

{ TOBDInspectorCategoryCollection }

function TOBDInspectorCategoryCollection.Add: TOBDInspectorCategory;
begin
  Result := TOBDInspectorCategory(inherited Add);
end;

procedure TOBDInspectorCategoryCollection.Assign(Source: TPersistent);
var
  Src: TOBDInspectorCategoryCollection;
  I: Integer;
begin
  if Source is TOBDInspectorCategoryCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDInspectorCategoryCollection(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited;
end;

function TOBDInspectorCategoryCollection.GetItem(Index: Integer): TOBDInspectorCategory;
begin
  Result := TOBDInspectorCategory(inherited GetItem(Index));
end;

procedure TOBDInspectorCategoryCollection.SetItem(Index: Integer;
  AValue: TOBDInspectorCategory);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDInspectorCategoryCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDInspector }

constructor TOBDInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csDoubleClicks,
    csCaptureMouse];
  Width := 360;
  Height := 240;
  TabStop := True;
  FSplitterPosition := 150;
  FUpdateCount := 0;
  FCategories := TOBDInspectorCategoryCollection.Create(Self,
    TOBDInspectorCategory);
  FCategories.OnChange := CategoriesChanged;

  FEditor := TOBDInspectorEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Visible := False;
  FEditor.OnExit := EditorExit;
  FEditor.OnKeyDown := EditorKeyDown;

  FPopup := TOBDPopupList.Create(Self);
  FPopup.OnCloseUp := PopupCloseUp;
  ApplyEditorTheme;
end;

destructor TOBDInspector.Destroy;
begin
  FPopup.Free;
  FEditor.Free;
  FCategories.Free;
  inherited;
end;

procedure TOBDInspector.Assign(Source: TPersistent);
var
  I: TOBDInspector;
begin
  inherited;
  if Source is TOBDInspector then
  begin
    I := TOBDInspector(Source);
    FCategories.Assign(I.Categories);
    FSplitterPosition := I.SplitterPosition;
    FReadOnly := I.ReadOnly;
    FScrollPos := 0;
    EnsureLayout;
    Invalidate;
  end;
end;

procedure TOBDInspector.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDInspector.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    EnsureLayout;
    EnsureSelectedVisible;
    UpdateEditorBounds;
    Invalidate;
  end;
end;

procedure TOBDInspector.Clear;
begin
  HideEditor;
  if FPopup.IsOpen then
    FPopup.CloseUp(False);
  FCategories.Clear;
  FSelected := nil;
  FScrollPos := 0;
  EnsureLayout;
  Invalidate;
end;

function TOBDInspector.AddCategory(const ACaption: string): TOBDInspectorCategory;
begin
  Result := FCategories.Add;
  Result.Caption := ACaption;
end;

function TOBDInspector.FindProperty(const AName: string): TOBDInspectorProperty;
var
  C, P: Integer;
begin
  Result := nil;
  for C := 0 to FCategories.Count - 1 do
    for P := 0 to FCategories[C].Properties.Count - 1 do
      if SameText(FCategories[C].Properties[P].Name, AName) then
        Exit(FCategories[C].Properties[P]);
end;

procedure TOBDInspector.SetCategories(AValue: TOBDInspectorCategoryCollection);
begin
  FCategories.Assign(AValue);
end;

procedure TOBDInspector.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  ActivateEditor(True);
  Invalidate;
end;

procedure TOBDInspector.SetSplitterPosition(AValue: Integer);
begin
  AValue := System.Math.Max(40, AValue);
  if FSplitterPosition = AValue then
    Exit;
  FSplitterPosition := AValue;
  UpdateEditorBounds;
  Invalidate;
end;

procedure TOBDInspector.CategoriesChanged(Sender: TObject);
begin
  if FUpdateCount > 0 then
    Exit;
  EnsureLayout;
  SetScrollPos(FScrollPos);
  UpdateEditorBounds;
  Invalidate;
end;

procedure TOBDInspector.ApplyEditorTheme;
var
  P: TOBDThemePalette;
begin
  P := Palette;
  FEditor.Color := P.GaugeFace;
  FEditor.Font.Name := 'Segoe UI';
  FEditor.Font.Color := P.ForegroundText;
  FEditor.Font.Height := -System.Math.Max(1, Round(12.5 * ScaleValue(96) / 96));
end;

function TOBDInspector.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.CompactRow);
end;

function TOBDInspector.GutterWidth: Integer;
begin
  Result := ScaleValue(20);
end;

function TOBDInspector.ScrollBarWidth: Integer;
begin
  Result := ScaleValue(6);
end;

function TOBDInspector.EffectiveRight: Integer;
begin
  Result := ClientWidth - 1;
  if FContentHeight > ClientHeight - 2 then
    Dec(Result, ScrollBarWidth + ScaleValue(2));
end;

function TOBDInspector.SplitterX: Integer;
var
  MinX, MaxX, X: Integer;
begin
  X := ScaleValue(FSplitterPosition);
  MinX := ScaleValue(40);
  MaxX := ClientWidth - ScaleValue(40);
  if MaxX < MinX then
    MaxX := MinX;
  Result := EnsureRange(X, MinX, MaxX);
end;

function TOBDInspector.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, FContentHeight - System.Math.Max(0,
    ClientHeight - 2));
end;

procedure TOBDInspector.EnsureLayout;
var
  Y, C, P, RH: Integer;
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
begin
  RH := RowHeight;
  Y := 1;
  for C := 0 to FCategories.Count - 1 do
  begin
    Cat := FCategories[C];
    if not Cat.Visible then
    begin
      Cat.Rect := Rect(0, 0, 0, 0);
      Continue;
    end;
    Cat.Rect := Rect(1, Y, ClientWidth - 1, Y + RH);
    Inc(Y, RH);
    for P := 0 to Cat.Properties.Count - 1 do
    begin
      Prop := Cat.Properties[P];
      if Cat.Collapsed or not Prop.Visible then
        Prop.Rect := Rect(0, 0, 0, 0)
      else
      begin
        Prop.Rect := Rect(1, Y, ClientWidth - 1, Y + RH);
        Inc(Y, RH);
      end;
    end;
  end;
  FContentHeight := Y + 1;
end;

procedure TOBDInspector.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  UpdateEditorBounds;
  Invalidate;
end;

function TOBDInspector.PropertyCategory(
  AProperty: TOBDInspectorProperty): TOBDInspectorCategory;
begin
  if AProperty <> nil then
    Result := AProperty.Category
  else
    Result := nil;
end;

function TOBDInspector.CanEditText(AProperty: TOBDInspectorProperty): Boolean;
begin
  Result := (AProperty <> nil) and Enabled and not FReadOnly and
    (AProperty.Kind in [ivText, ivEllipsis]) and
    not ((AProperty.Kind = ivEllipsis) and AProperty.ReadOnly);
end;

function TOBDInspector.IsReadOnlyText(AProperty: TOBDInspectorProperty): Boolean;
begin
  Result := (AProperty <> nil) and
    ((AProperty.Kind = ivReadOnly) or FReadOnly or not Enabled or
    ((AProperty.Kind = ivEllipsis) and AProperty.ReadOnly));
end;

function TOBDInspector.VisibleValueRect(AProperty: TOBDInspectorProperty): TRect;
var
  R: TRect;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Result := Rect(SplitterX + ScaleValue(8), R.Top + ScaleValue(4),
    EffectiveRight - ScaleValue(4), R.Bottom - ScaleValue(4));
  if AProperty.Kind = ivEllipsis then
    Result.Right := EllipsisButtonRect(AProperty).Left - ScaleValue(4);
end;

function TOBDInspector.EllipsisButtonRect(AProperty: TOBDInspectorProperty): TRect;
var
  R: TRect;
  S: Integer;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  S := RowHeight - ScaleValue(8);
  Result := Rect(EffectiveRight - ScaleValue(2) - S, R.Top + ScaleValue(4),
    EffectiveRight - ScaleValue(2), R.Top + ScaleValue(4) + S);
end;

function TOBDInspector.CheckBoxRect(AProperty: TOBDInspectorProperty): TRect;
var
  R: TRect;
  S: Integer;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  S := ScaleValue(Metrics.Check);
  Result := Rect(SplitterX + ScaleValue(8), R.Top + (R.Height - S) div 2,
    SplitterX + ScaleValue(8) + S, R.Top + (R.Height - S) div 2 + S);
end;

function TOBDInspector.DropDownRect(AProperty: TOBDInspectorProperty): TRect;
var
  R: TRect;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Result := Rect(EffectiveRight - ScaleValue(28), R.Top, EffectiveRight,
    R.Bottom);
end;

function TOBDInspector.ScrollThumbRect: TRect;
var
  Track: TRect;
  ThumbH, ThumbY: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if FContentHeight <= ClientHeight - 2 then
    Exit;
  Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(2), ScaleValue(2),
    ClientWidth - ScaleValue(2), ClientHeight - ScaleValue(2));
  ThumbH := System.Math.Max(ScaleValue(24), MulDiv(Track.Height,
    ClientHeight - 2, FContentHeight));
  ThumbY := Track.Top;
  if MaxScroll > 0 then
    Inc(ThumbY, MulDiv(Track.Height - ThumbH, FScrollPos, MaxScroll));
  Result := Rect(Track.Left, ThumbY, Track.Right, ThumbY + ThumbH);
end;

procedure TOBDInspector.UpdateEditorBounds;
var
  R, Row: TRect;
begin
  if (FEditProperty = nil) or not FEditor.Visible then
    Exit;
  Row := FEditProperty.Rect;
  OffsetRect(Row, 0, -FScrollPos);
  if (Row.Bottom <= 1) or (Row.Top >= ClientHeight - 1) then
  begin
    FEditor.Visible := False;
    Exit;
  end;
  R := VisibleValueRect(FEditProperty);
  InflateRect(R, 0, -ScaleValue(1));
  if R.Right < R.Left then
    R.Right := R.Left;
  FEditor.SetBounds(R.Left, R.Top + ScaleValue(1), R.Width,
    System.Math.Max(1, R.Height - ScaleValue(2)));
end;

procedure TOBDInspector.HideEditor;
begin
  FEditProperty := nil;
  FEditor.Visible := False;
end;

procedure TOBDInspector.ActivateEditor(ASelectAll: Boolean);
var
  Prop: TOBDInspectorProperty;
begin
  HideEditor;
  if not (FSelected is TOBDInspectorProperty) then
    Exit;
  Prop := TOBDInspectorProperty(FSelected);
  if not (Prop.Kind in [ivText, ivReadOnly, ivEllipsis]) then
    Exit;
  FEditProperty := Prop;
  FEditOriginalValue := Prop.Value;
  FInternalEditorUpdate := True;
  try
    FEditor.Text := Prop.Value;
    FEditor.ReadOnly := IsReadOnlyText(Prop) or not CanEditText(Prop);
    if Prop.Kind = ivEllipsis then
      FEditor.Font.Name := 'Consolas'
    else
      FEditor.Font.Name := 'Segoe UI';
  finally
    FInternalEditorUpdate := False;
  end;
  FEditor.Visible := True;
  UpdateEditorBounds;
  if ASelectAll then
    FEditor.SelectAll;
  if CanFocus and FEditor.CanFocus and ASelectAll then
    FEditor.SetFocus;
end;

function TOBDInspector.CommitEditor(AKeepFocus: Boolean): Boolean;
var
  NewValue: string;
  Accept: Boolean;
  Prop: TOBDInspectorProperty;
begin
  Result := True;
  if FCommitting or (FEditProperty = nil) or not FEditor.Visible then
    Exit;
  Prop := FEditProperty;
  if not CanEditText(Prop) then
    Exit;
  if FEditor.Text = Prop.Value then
    Exit;

  FCommitting := True;
  try
    NewValue := FEditor.Text;
    Accept := True;
    if Assigned(FOnPropertyChanging) then
      FOnPropertyChanging(Self, Prop, NewValue, Accept);
    if not Accept then
    begin
      Result := False;
      MessageBeep(MB_ICONEXCLAMATION);
      FInternalEditorUpdate := True;
      try
        FEditor.Text := NewValue;
        FEditor.SelectAll;
      finally
        FInternalEditorUpdate := False;
      end;
      if AKeepFocus and FEditor.CanFocus then
        FEditor.SetFocus;
      Exit;
    end;
    Prop.Value := NewValue;
    FirePropertyChanged(Prop);
  finally
    FCommitting := False;
  end;
end;

procedure TOBDInspector.RevertEditor;
begin
  if (FEditProperty = nil) or not FEditor.Visible then
    Exit;
  FInternalEditorUpdate := True;
  try
    FEditor.Text := FEditOriginalValue;
    FEditProperty.Value := FEditOriginalValue;
    FEditor.SelectAll;
  finally
    FInternalEditorUpdate := False;
  end;
  Invalidate;
end;

procedure TOBDInspector.EditorExit(Sender: TObject);
begin
  if FPopup.IsOpen or FInternalEditorUpdate then
    Exit;
  CommitEditor(False);
end;

procedure TOBDInspector.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FPopup.IsOpen and FPopup.HandleKey(Key, Shift) then
    Exit;

  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          if (FSelected is TOBDInspectorProperty) and
            (TOBDInspectorProperty(FSelected).Kind = ivEllipsis) and
            Assigned(FOnPropertyButtonClick) then
            FOnPropertyButtonClick(Self, TOBDInspectorProperty(FSelected));
        end
        else if CommitEditor(True) then
          FEditor.SelectAll;
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        RevertEditor;
        Key := 0;
      end;
    VK_UP:
      begin
        if CommitEditor(True) then
          SelectRelative(-1);
        Key := 0;
      end;
    VK_DOWN:
      begin
        if ssAlt in Shift then
        begin
          if FSelected is TOBDInspectorProperty then
            OpenPickList(TOBDInspectorProperty(FSelected));
        end
        else if CommitEditor(True) then
          SelectRelative(1);
        Key := 0;
      end;
    VK_PRIOR:
      begin
        if CommitEditor(True) then
          SelectPage(-1);
        Key := 0;
      end;
    VK_NEXT:
      begin
        if CommitEditor(True) then
          SelectPage(1);
        Key := 0;
      end;
    VK_F4:
      begin
        if FSelected is TOBDInspectorProperty then
          OpenPickList(TOBDInspectorProperty(FSelected));
        Key := 0;
      end;
  end;
end;

procedure TOBDInspector.SelectItem(AItem: TCollectionItem; ASelectAll: Boolean);
begin
  if AItem = FSelected then
  begin
    ActivateEditor(ASelectAll);
    Exit;
  end;
  if not CommitEditor(True) then
    Exit;
  FSelected := AItem;
  if FSelected is TOBDInspectorProperty then
  begin
    if Assigned(FOnPropertySelect) then
      FOnPropertySelect(Self, TOBDInspectorProperty(FSelected));
    ActivateEditor(ASelectAll);
  end
  else
  begin
    HideEditor;
    if (FSelected is TOBDInspectorCategory) and Assigned(FOnCategorySelect) then
      FOnCategorySelect(Self, TOBDInspectorCategory(FSelected));
    if CanFocus then
      SetFocus;
  end;
  EnsureSelectedVisible;
  Invalidate;
end;

function TOBDInspector.ItemAt(const P: TPoint): TCollectionItem;
var
  C, I, Y: Integer;
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
begin
  Result := nil;
  Y := P.Y + FScrollPos;
  for C := 0 to FCategories.Count - 1 do
  begin
    Cat := FCategories[C];
    if Cat.Visible and PtInRect(Cat.Rect, Point(P.X, Y)) then
      Exit(Cat);
    if not Cat.Visible or Cat.Collapsed then
      Continue;
    for I := 0 to Cat.Properties.Count - 1 do
    begin
      Prop := Cat.Properties[I];
      if Prop.Visible and PtInRect(Prop.Rect, Point(P.X, Y)) then
        Exit(Prop);
    end;
  end;
end;

function TOBDInspector.CategoryAt(const P: TPoint): TOBDInspectorCategory;
var
  Item: TCollectionItem;
begin
  Item := ItemAt(P);
  if Item is TOBDInspectorCategory then
    Result := TOBDInspectorCategory(Item)
  else
    Result := nil;
end;

function TOBDInspector.PropertyAt(const P: TPoint): TOBDInspectorProperty;
var
  Item: TCollectionItem;
begin
  Item := ItemAt(P);
  if Item is TOBDInspectorProperty then
    Result := TOBDInspectorProperty(Item)
  else
    Result := nil;
end;

procedure TOBDInspector.EnsureSelectedVisible;
var
  R: TRect;
begin
  if FSelected is TOBDInspectorCategory then
    R := TOBDInspectorCategory(FSelected).Rect
  else if FSelected is TOBDInspectorProperty then
    R := TOBDInspectorProperty(FSelected).Rect
  else
    Exit;
  if R.Top < FScrollPos + 1 then
    SetScrollPos(R.Top - 1)
  else if R.Bottom > FScrollPos + ClientHeight - 1 then
    SetScrollPos(R.Bottom - ClientHeight + 1);
end;

procedure TOBDInspector.SelectRelative(ADelta: Integer);
var
  List: TList;
  C, P, I, Cur: Integer;
  Item: TCollectionItem;
begin
  List := TList.Create;
  try
    for C := 0 to FCategories.Count - 1 do
      if FCategories[C].Visible then
      begin
        List.Add(FCategories[C]);
        if not FCategories[C].Collapsed then
          for P := 0 to FCategories[C].Properties.Count - 1 do
            if FCategories[C].Properties[P].Visible then
              List.Add(FCategories[C].Properties[P]);
      end;
    if List.Count = 0 then
      Exit;
    Cur := List.IndexOf(FSelected);
    if Cur < 0 then
      I := 0
    else
      I := EnsureRange(Cur + ADelta, 0, List.Count - 1);
    Item := TCollectionItem(List[I]);
    SelectItem(Item, True);
  finally
    List.Free;
  end;
end;

procedure TOBDInspector.SelectFirst;
begin
  SelectRelative(-1000000);
end;

procedure TOBDInspector.SelectLast;
begin
  SelectRelative(1000000);
end;

procedure TOBDInspector.SelectPage(ADirection: Integer);
var
  Delta: Integer;
begin
  Delta := System.Math.Max(1, (ClientHeight - 2) div RowHeight);
  SelectRelative(ADirection * Delta);
end;

procedure TOBDInspector.ToggleCategory(ACategory: TOBDInspectorCategory);
begin
  if ACategory = nil then
    Exit;
  ACategory.Collapsed := not ACategory.Collapsed;
  if ACategory.Collapsed then
  begin
    if Assigned(FOnCategoryCollapse) then
      FOnCategoryCollapse(Self, ACategory);
  end
  else if Assigned(FOnCategoryExpand) then
    FOnCategoryExpand(Self, ACategory);
  EnsureLayout;
  EnsureSelectedVisible;
  Invalidate;
end;

procedure TOBDInspector.ExpandAll(ACollapsed: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to FCategories.Count - 1 do
      FCategories[I].Collapsed := ACollapsed;
  finally
    EndUpdate;
  end;
end;

procedure TOBDInspector.FirePropertyChanged(AProperty: TOBDInspectorProperty);
begin
  Invalidate;
  if Assigned(FOnPropertyChanged) then
    FOnPropertyChanged(Self, AProperty);
end;

procedure TOBDInspector.ToggleCheck(AProperty: TOBDInspectorProperty);
begin
  if (AProperty = nil) or (AProperty.Kind <> ivCheck) or FReadOnly or not Enabled then
    Exit;
  AProperty.Checked := not AProperty.Checked;
  FirePropertyChanged(AProperty);
end;

procedure TOBDInspector.CyclePickList(AProperty: TOBDInspectorProperty);
var
  I: Integer;
begin
  if (AProperty = nil) or (AProperty.Kind <> ivPickList) or
    (AProperty.PickList.Count = 0) or FReadOnly or not Enabled then
    Exit;
  I := AProperty.PickList.IndexOf(AProperty.Value);
  I := (I + 1) mod AProperty.PickList.Count;
  AProperty.Value := AProperty.PickList[I];
  FirePropertyChanged(AProperty);
end;

procedure TOBDInspector.OpenPickList(AProperty: TOBDInspectorProperty);
var
  R: TRect;
  Field: TRect;
begin
  if (AProperty = nil) or (AProperty.Kind <> ivPickList) or
    (AProperty.PickList.Count = 0) or FReadOnly or not Enabled then
    Exit;
  SelectItem(AProperty, False);
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Field := Rect(SplitterX + ScaleValue(2), R.Top + ScaleValue(2),
    EffectiveRight, R.Bottom - ScaleValue(2));
  Field.TopLeft := ClientToScreen(Field.TopLeft);
  Field.BottomRight := ClientToScreen(Field.BottomRight);
  FPopup.Popup(Self, Field, AProperty.PickList,
    AProperty.PickList.IndexOf(AProperty.Value), Palette, ScaleValue(96),
    RowHeight);
end;

procedure TOBDInspector.PopupCloseUp(Sender: TObject; AAccepted: Boolean;
  AIndex: Integer);
var
  Prop: TOBDInspectorProperty;
begin
  if not (FSelected is TOBDInspectorProperty) then
    Exit;
  Prop := TOBDInspectorProperty(FSelected);
  if AAccepted and (Prop.Kind = ivPickList) and (AIndex >= 0) and
    (AIndex < Prop.PickList.Count) and not FReadOnly then
  begin
    Prop.Value := Prop.PickList[AIndex];
    FirePropertyChanged(Prop);
  end;
  if CanFocus then
    SetFocus;
end;

procedure TOBDInspector.SelectPickListPrefix(AProperty: TOBDInspectorProperty;
  Ch: Char);
var
  NowTick: Cardinal;
  I: Integer;
  Prefix: string;
begin
  if (AProperty = nil) or (AProperty.Kind <> ivPickList) or FReadOnly then
    Exit;
  NowTick := GetTickCount;
  if NowTick - FIncrementalTick > 1000 then
    FIncrementalText := '';
  FIncrementalTick := NowTick;
  FIncrementalText := FIncrementalText + Ch;
  Prefix := AnsiLowerCase(FIncrementalText);
  for I := 0 to AProperty.PickList.Count - 1 do
    if Pos(Prefix, AnsiLowerCase(AProperty.PickList[I])) = 1 then
    begin
      AProperty.Value := AProperty.PickList[I];
      FirePropertyChanged(AProperty);
      Break;
    end;
end;

procedure TOBDInspector.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Save: Integer;
  C, I: Integer;
  R: TRect;
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
begin
  EnsureLayout;
  ApplyEditorTheme;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.FrameRect(ClientRect, Palette.NeutralLight, ScaleValue(1));
    Save := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, 1, 1, ClientWidth - 1,
        ClientHeight - 1);
      for C := 0 to FCategories.Count - 1 do
      begin
        Cat := FCategories[C];
        if not Cat.Visible then
          Continue;
        R := Cat.Rect;
        OffsetRect(R, 0, -FScrollPos);
        if (R.Bottom >= 1) and (R.Top <= ClientHeight - 1) then
          DrawCategory(P, Cat, R);
        if Cat.Collapsed then
          Continue;
        for I := 0 to Cat.Properties.Count - 1 do
        begin
          Prop := Cat.Properties[I];
          if not Prop.Visible then
            Continue;
          R := Prop.Rect;
          OffsetRect(R, 0, -FScrollPos);
          if (R.Bottom >= 1) and (R.Top <= ClientHeight - 1) then
            DrawProperty(P, Prop, R);
        end;
      end;
      P.VLine(SplitterX, 1, System.Math.Max(0, ClientHeight - 2),
        Palette.NeutralLight);
      DrawScrollBar(P);
    finally
      RestoreDC(ACanvas.Handle, Save);
    end;
  finally
    P.Free;
  end;
  UpdateEditorBounds;
end;

procedure TOBDInspector.DrawCategory(APainter: TOBDPainter;
  ACategory: TOBDInspectorCategory; const R: TRect);
var
  CountText: string;
  Fill, CaptionInk, CountInk: TColor;
begin
  Fill := APainter.HeaderFill;
  if FSelected = ACategory then
  begin
    if APainter.Dark then
      Fill := APainter.Tint(Palette.Accent, 0.18)
    else
      Fill := APainter.Tint(Palette.Accent, 0.12);
  end;
  APainter.FillRect(R, Fill);
  if Enabled then
  begin
    CaptionInk := Palette.ForegroundText;
    CountInk := Palette.GaugeLabel;
  end
  else
  begin
    CaptionInk := APainter.DisabledText;
    CountInk := APainter.DisabledText;
  end;
  APainter.GlyphChevron(GutterWidth div 2 + ScaleValue(2),
    R.Top + R.Height div 2, Palette.Subtle, not ACategory.Collapsed);
  APainter.Text(GutterWidth + ScaleValue(4), R.Top + R.Height div 2,
    ACategory.Caption, 12.5, CaptionInk, twSemibold, taLeftJustify,
    System.Math.Max(0, SplitterX - GutterWidth - ScaleValue(12)));
  if ACategory.Collapsed then
  begin
    CountText := IntToStr(ACategory.Properties.Count);
    APainter.Text(EffectiveRight - ScaleValue(12), R.Top + R.Height div 2,
      CountText, 11.5, CountInk, twRegular, taRightJustify);
  end;
  APainter.HLine(1, R.Bottom - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDInspector.DrawProperty(APainter: TOBDPainter;
  AProperty: TOBDInspectorProperty; const R: TRect);
var
  SelectedRow: Boolean;
  Ink, ValueInk: TColor;
  NameWeight, ValueWeight, MonoWeight: TOBDTextWeight;
  VX, VW, ButtonSize: Integer;
  Btn, Box: TRect;
  Caption: string;
  State: TCheckBoxState;
begin
  SelectedRow := FSelected = AProperty;
  if SelectedRow then
  begin
    if APainter.Dark then
      APainter.FillRect(R, APainter.Tint(Palette.Accent, 0.18))
    else
      APainter.FillRect(R, APainter.Tint(Palette.Accent, 0.12));
  end;

  if AProperty.Level <> alvNormal then
    APainter.FillRect(Rect(R.Left, R.Top + ScaleValue(4), R.Left + ScaleValue(3),
      R.Bottom - ScaleValue(4)), APainter.LevelColor(AProperty.Level));

  if not Enabled then
    Ink := APainter.DisabledText
  else if SelectedRow then
    Ink := APainter.AccentText
  else
    Ink := Palette.ForegroundText;
  if SelectedRow then
    NameWeight := twSemibold
  else
    NameWeight := twRegular;
  APainter.Text(GutterWidth + ScaleValue(4), R.Top + R.Height div 2,
    AProperty.Name, 12.5, Ink, NameWeight, taLeftJustify,
    System.Math.Max(0, SplitterX - GutterWidth - ScaleValue(12)));

  VX := SplitterX + ScaleValue(8);
  VW := System.Math.Max(0, EffectiveRight - VX - ScaleValue(6));
  ValueInk := Palette.ForegroundText;
  if not Enabled then
    ValueInk := APainter.DisabledText
  else if AProperty.Kind = ivReadOnly then
    ValueInk := APainter.LevelColor(AProperty.Level);
  if AProperty.Modified or (AProperty.Level <> alvNormal) then
    ValueWeight := twSemibold
  else
    ValueWeight := twRegular;

  if (FEditProperty = AProperty) and FEditor.Visible then
  begin
    APainter.FrameRect(Rect(SplitterX + ScaleValue(2), R.Top + ScaleValue(3),
      EffectiveRight - ScaleValue(4), R.Bottom - ScaleValue(3)),
      APainter.AccentText, ScaleValue(2));
    if AProperty.Kind = ivEllipsis then
    begin
      Btn := EllipsisButtonRect(AProperty);
      APainter.FillRect(Btn, Palette.GaugeFace);
      APainter.FrameRect(Btn, Palette.NeutralLight, ScaleValue(1));
      APainter.Text(Btn.Left + Btn.Width div 2, Btn.Top + Btn.Height div 2 -
        ScaleValue(2), '…', 12.5, ValueInk, twBold, taCenter);
    end;
    APainter.HLine(1, R.Bottom - 1, ClientWidth - 2, Palette.NeutralLight);
    Exit;
  end;

  case AProperty.Kind of
    ivPickList:
      begin
        APainter.Text(VX, R.Top + R.Height div 2, AProperty.Value, 12.5,
          ValueInk, ValueWeight, taLeftJustify, System.Math.Max(0,
          VW - ScaleValue(24)));
        APainter.GlyphChevron(EffectiveRight - ScaleValue(14),
          R.Top + R.Height div 2, Palette.Subtle, True);
      end;
    ivCheck:
      begin
        if AProperty.Checked then
        begin
          State := cbChecked;
          Caption := AProperty.CheckedCaption;
        end
        else
        begin
          State := cbUnchecked;
          Caption := AProperty.UncheckedCaption;
        end;
        APainter.CheckBox(VX, R.Top + R.Height div 2, Metrics.Check, State,
          Enabled and not FReadOnly, False, SelectedRow);
        Box := CheckBoxRect(AProperty);
        APainter.Text(Box.Right + ScaleValue(8), R.Top + R.Height div 2,
          Caption, 12.5, ValueInk, ValueWeight, taLeftJustify,
          System.Math.Max(0, VW - Box.Width - ScaleValue(12)));
      end;
    ivEllipsis:
      begin
        Btn := EllipsisButtonRect(AProperty);
        ButtonSize := Btn.Width;
        if AProperty.Modified then
          MonoWeight := twMonoBold
        else
          MonoWeight := twMono;
        APainter.Text(VX, R.Top + R.Height div 2, AProperty.Value, 12.5,
          ValueInk, MonoWeight, taLeftJustify, System.Math.Max(0,
          VW - ButtonSize - ScaleValue(8)));
        APainter.FillRect(Btn, Palette.GaugeFace);
        APainter.FrameRect(Btn, Palette.NeutralLight, ScaleValue(1));
        APainter.Text(Btn.Left + Btn.Width div 2, Btn.Top + Btn.Height div 2 -
          ScaleValue(2), '…', 12.5, ValueInk, twBold, taCenter);
      end;
  else
    APainter.Text(VX, R.Top + R.Height div 2, AProperty.Value, 12.5,
      ValueInk, ValueWeight, taLeftJustify, VW);
  end;
  APainter.HLine(1, R.Bottom - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDInspector.DrawScrollBar(APainter: TOBDPainter);
var
  Track, Thumb: TRect;
  ThumbColor: TColor;
begin
  if FContentHeight <= ClientHeight - 2 then
    Exit;
  Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(2), ScaleValue(2),
    ClientWidth - ScaleValue(2), ClientHeight - ScaleValue(2));
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

procedure TOBDInspector.Resize;
begin
  inherited;
  EnsureLayout;
  SetScrollPos(FScrollPos);
  UpdateEditorBounds;
end;

procedure TOBDInspector.DensityChanged;
begin
  inherited;
  EnsureLayout;
  ApplyEditorTheme;
  UpdateEditorBounds;
end;

procedure TOBDInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
  Item: TCollectionItem;
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
  SplitBand: TRect;
  Thumb: TRect;
begin
  inherited;
  if not Enabled or (Button <> mbLeft) then
    Exit;
  P := Point(X, Y);
  FLastMouse := P;
  if CanFocus and not Focused then
    SetFocus;

  Thumb := ScrollThumbRect;
  if not Thumb.IsEmpty and PtInRect(Rect(Thumb.Left - ScaleValue(2), Thumb.Top,
    Thumb.Right + ScaleValue(2), Thumb.Bottom), P) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    Exit;
  end;
  if (FContentHeight > ClientHeight - 2) and
    (X >= ClientWidth - ScrollBarWidth - ScaleValue(3)) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - (ClientHeight - RowHeight))
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + (ClientHeight - RowHeight));
    Exit;
  end;

  SplitBand := Rect(SplitterX - ScaleValue(3), 0, SplitterX + ScaleValue(4),
    ClientHeight);
  if PtInRect(SplitBand, P) then
  begin
    FDraggingSplitter := True;
    Cursor := crSizeWE;
    Exit;
  end;

  Cat := CategoryAt(P);
  if (Cat <> nil) and (X <= GutterWidth) then
  begin
    SelectItem(Cat, False);
    ToggleCategory(Cat);
    Exit;
  end;

  Prop := PropertyAt(P);
  if Prop <> nil then
  begin
    SelectItem(Prop, True);
    if (Prop.Kind = ivCheck) and PtInRect(CheckBoxRect(Prop), P) then
      ToggleCheck(Prop)
    else if (Prop.Kind = ivPickList) and PtInRect(DropDownRect(Prop), P) then
      OpenPickList(Prop)
    else if (Prop.Kind = ivEllipsis) and PtInRect(EllipsisButtonRect(Prop), P) and
      Assigned(FOnPropertyButtonClick) then
      FOnPropertyButtonClick(Self, Prop);
    Exit;
  end;

  Item := ItemAt(P);
  if Item <> nil then
    SelectItem(Item, False);
end;

procedure TOBDInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPos: Integer;
  Track: TRect;
  Thumb: TRect;
begin
  inherited;
  FLastMouse := Point(X, Y);
  if FDraggingSplitter then
  begin
    NewPos := EnsureRange(X, ScaleValue(40), ClientWidth - ScaleValue(40));
    FSplitterPosition := MulDiv(NewPos, 96, ScaleValue(96));
    UpdateEditorBounds;
    Invalidate;
    Exit;
  end;
  if FDraggingScroll then
  begin
    Track := Rect(ClientWidth - ScrollBarWidth - ScaleValue(2), ScaleValue(2),
      ClientWidth - ScaleValue(2), ClientHeight - ScaleValue(2));
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll,
        Track.Height - Thumb.Height));
    Exit;
  end;
  FScrollHover := (FContentHeight > ClientHeight - 2) and
    (X >= ClientWidth - ScrollBarWidth - ScaleValue(4));
  if Abs(X - SplitterX) <= ScaleValue(3) then
    Cursor := crSizeWE
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDraggingSplitter := False;
  FDraggingScroll := False;
  Cursor := crDefault;
end;

procedure TOBDInspector.MouseLeave;
begin
  inherited;
  FScrollHover := False;
  if not FDraggingSplitter then
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDInspector.DblClick;
var
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
begin
  inherited;
  if Abs(FLastMouse.X - SplitterX) <= ScaleValue(3) then
  begin
    SplitterPosition := 150;
    Exit;
  end;
  Cat := CategoryAt(FLastMouse);
  if Cat <> nil then
  begin
    ToggleCategory(Cat);
    Exit;
  end;
  Prop := PropertyAt(FLastMouse);
  if Prop <> nil then
  begin
    case Prop.Kind of
      ivPickList:
        CyclePickList(Prop);
      ivCheck:
        ToggleCheck(Prop);
    end;
  end;
end;

procedure TOBDInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cat: TOBDInspectorCategory;
  Prop: TOBDInspectorProperty;
begin
  inherited;
  if FPopup.IsOpen and FPopup.HandleKey(Key, Shift) then
    Exit;
  if not Enabled then
    Exit;

  if FSelected is TOBDInspectorCategory then
    Cat := TOBDInspectorCategory(FSelected)
  else
    Cat := nil;
  if FSelected is TOBDInspectorProperty then
    Prop := TOBDInspectorProperty(FSelected)
  else
    Prop := nil;

  case Key of
    VK_UP:
      begin
        SelectRelative(-1);
        Key := 0;
      end;
    VK_DOWN:
      begin
        if ssAlt in Shift then
          OpenPickList(Prop)
        else
          SelectRelative(1);
        Key := 0;
      end;
    VK_PRIOR:
      begin
        SelectPage(-1);
        Key := 0;
      end;
    VK_NEXT:
      begin
        SelectPage(1);
        Key := 0;
      end;
    VK_HOME:
      begin
        SelectFirst;
        Key := 0;
      end;
    VK_END:
      begin
        SelectLast;
        Key := 0;
      end;
    VK_LEFT:
      begin
        if Cat <> nil then
          Cat.Collapsed := True
        else if Prop <> nil then
          SelectItem(PropertyCategory(Prop), False);
        EnsureLayout;
        Invalidate;
        Key := 0;
      end;
    VK_RIGHT:
      begin
        if Cat <> nil then
          Cat.Collapsed := False;
        EnsureLayout;
        Invalidate;
        Key := 0;
      end;
    VK_ADD:
      begin
        if Cat <> nil then
          Cat.Collapsed := False;
        EnsureLayout;
        Invalidate;
        Key := 0;
      end;
    VK_SUBTRACT:
      begin
        if Cat <> nil then
          Cat.Collapsed := True;
        EnsureLayout;
        Invalidate;
        Key := 0;
      end;
    VK_MULTIPLY:
      begin
        ExpandAll(False);
        Key := 0;
      end;
    VK_SPACE:
      begin
        ToggleCheck(Prop);
        Key := 0;
      end;
    VK_RETURN:
      begin
        if (ssCtrl in Shift) and (Prop <> nil) and (Prop.Kind = ivEllipsis) and
          Assigned(FOnPropertyButtonClick) then
          FOnPropertyButtonClick(Self, Prop)
        else if (Prop <> nil) and (Prop.Kind = ivPickList) then
          CyclePickList(Prop)
        else
          ActivateEditor(True);
        Key := 0;
      end;
    VK_F4:
      begin
        OpenPickList(Prop);
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        RevertEditor;
        Key := 0;
      end;
  end;
end;

procedure TOBDInspector.KeyPress(var Key: Char);
var
  Prop: TOBDInspectorProperty;
begin
  inherited;
  if not (FSelected is TOBDInspectorProperty) or (Key < #32) then
    Exit;
  Prop := TOBDInspectorProperty(FSelected);
  if Prop.Kind = ivPickList then
  begin
    SelectPickListPrefix(Prop, Key);
    Key := #0;
  end
  else if CanEditText(Prop) and not FEditor.Focused then
  begin
    ActivateEditor(False);
    FInternalEditorUpdate := True;
    try
      FEditor.Text := Key;
      FEditor.SelStart := Length(FEditor.Text);
    finally
      FInternalEditorUpdate := False;
    end;
    if FEditor.CanFocus then
      FEditor.SetFocus;
    Key := #0;
  end;
end;

function TOBDInspector.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if ssCtrl in Shift then
    SelectRelative(1)
  else
    SetScrollPos(FScrollPos + RowHeight * 3);
end;

function TOBDInspector.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if ssCtrl in Shift then
    SelectRelative(-1)
  else
    SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDInspector.CMHintShow(var Message: TCMHintShow);
var
  Prop: TOBDInspectorProperty;
  P: TPoint;
  R: TRect;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  P := Message.HintInfo^.CursorPos;
  Prop := PropertyAt(P);
  if (Prop = nil) or not ValueTruncated(Prop) then
    Exit;
  Message.HintInfo^.HintStr := Prop.Value;
  R := Prop.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Message.HintInfo^.CursorRect := R;
end;

function TOBDInspector.ValueTruncated(AProperty: TOBDInspectorProperty): Boolean;
var
  P: TOBDPainter;
  R: TRect;
  W: Integer;
begin
  Result := False;
  if (AProperty = nil) or (AProperty.Value = '') then
    Exit;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    R := VisibleValueRect(AProperty);
    W := R.Width;
    if AProperty.Kind = ivPickList then
      Dec(W, ScaleValue(24));
    Result := P.TextWidth(AProperty.Value, 12.5) > W;
  finally
    P.Free;
  end;
end;

procedure TOBDInspector.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  Message.Result := Message.Result and not DLGC_WANTTAB;
end;

end.
