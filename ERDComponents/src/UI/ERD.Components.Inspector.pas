//------------------------------------------------------------------------------
//  ERD.Components.Inspector
//
//  TERDInspector - a themed Object-Inspector style property grid with
//  categories, inline editors, pick lists, check values and ellipsis buttons.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the Application controls.
//    2026-10-12  ERD  Translated On/Off captions; empty captions show the
//                     translated default and follow the active language.
//------------------------------------------------------------------------------

unit ERD.Components.Inspector;

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
  Vcl.Forms,
  Vcl.StdCtrls,
  ERD.Components.Types,
  ERD.Components.Paint,
  ERD.Components.Control,
  ERD.Components.PopupList,
  ERD.Components.ScrollBar,
  ERD.Components.I18n;

type
  TERDInspectorProperty = class;
  TERDInspectorCategory = class;
  TERDInspectorPropertyCollection = class;
  TERDInspectorCategoryCollection = class;

  /// <summary>Kind of value editor a property row uses.</summary>
  TERDInspectorValueKind = (
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
  TERDInspectorPropertyEvent = procedure(Sender: TObject;
    AProperty: TERDInspectorProperty) of object;

  /// <summary>Category-row notification.</summary>
  /// <param name="Sender">Inspector.</param>
  /// <param name="ACategory">Category row.</param>
  TERDInspectorCategoryEvent = procedure(Sender: TObject;
    ACategory: TERDInspectorCategory) of object;

  /// <summary>Validation hook fired before an edited text value is committed.</summary>
  /// <param name="Sender">Inspector.</param>
  /// <param name="AProperty">Property being changed.</param>
  /// <param name="NewValue">Proposed value. Handlers may replace it.</param>
  /// <param name="Accept">Set False to keep the editor open.</param>
  TERDInspectorPropertyChangingEvent = procedure(Sender: TObject;
    AProperty: TERDInspectorProperty; var NewValue: string;
    var Accept: Boolean) of object;

  /// <summary>One streamable property row in a category.</summary>
  TERDInspectorProperty = class(TCollectionItem)
  strict private
    FName: string;
    FValue: string;
    FDefaultValue: string;
    FHasDefault: Boolean;
    FKind: TERDInspectorValueKind;
    FPickList: TStrings;
    FChecked: Boolean;
    FCheckedCaption: string;
    FUncheckedCaption: string;
    FReadOnly: Boolean;
    FLevel: TERDAlertLevel;
    FHint: string;
    FTag: NativeInt;
    FVisible: Boolean;
    FRect: TRect;
    procedure SetName(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetDefaultValue(const AValue: string);
    procedure SetHasDefault(AValue: Boolean);
    procedure SetKind(AValue: TERDInspectorValueKind);
    procedure SetPickList(AValue: TStrings);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckedCaption(const AValue: string);
    procedure SetUncheckedCaption(const AValue: string);
    function GetCheckedCaption: string;
    function GetUncheckedCaption: string;
    function IsCheckedCaptionStored: Boolean;
    function IsUncheckedCaptionStored: Boolean;
    procedure SetReadOnly(AValue: Boolean);
    procedure SetLevel(AValue: TERDAlertLevel);
    procedure SetHint(const AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure PickListChanged(Sender: TObject);
    function GetModified: Boolean;
    function GetCategory: TERDInspectorCategory;
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
    property Category: TERDInspectorCategory read GetCategory;
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
    property Kind: TERDInspectorValueKind read FKind write SetKind default ivText;
    /// <summary>Fixed choices for pick-list rows.</summary>
    property PickList: TStrings read FPickList write SetPickList;
    /// <summary>Boolean state for check rows. Value mirrors True or False.</summary>
    property Checked: Boolean read FChecked write SetChecked default False;
    /// <summary>Caption drawn when Checked is True. Empty (the default)
    /// shows the translated "On".</summary>
    property CheckedCaption: string read GetCheckedCaption
      write SetCheckedCaption stored IsCheckedCaptionStored;
    /// <summary>Caption drawn when Checked is False. Empty (the default)
    /// shows the translated "Off".</summary>
    property UncheckedCaption: string read GetUncheckedCaption
      write SetUncheckedCaption stored IsUncheckedCaptionStored;
    /// <summary>True when the text part of an ellipsis row is read-only.</summary>
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    /// <summary>Alert level used to colour read-only values and row edge.</summary>
    property Level: TERDAlertLevel read FLevel write SetLevel default alvNormal;
    /// <summary>Optional row hint text.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
    /// <summary>True when the row is included in layout and painting.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  /// <summary>Streamable property collection owned by a category.</summary>
  TERDInspectorPropertyCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TERDInspectorProperty;
    procedure SetItem(Index: Integer; AValue: TERDInspectorProperty);
  protected
    /// <summary>Notifies the owner when collection contents change.</summary>
    /// <param name="Item">Changed item, or nil for a structural change.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a property row.</summary>
    /// <returns>The new property row.</returns>
    function Add: TERDInspectorProperty;
    /// <summary>Returns the category that owns this collection.</summary>
    /// <returns>Owner category, or nil.</returns>
    function OwnerCategory: TERDInspectorCategory;
    /// <summary>Copies another property collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Indexed property rows.</summary>
    property Items[Index: Integer]: TERDInspectorProperty read GetItem
      write SetItem; default;
    /// <summary>Fires whenever a row or the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>One collapsible category row with owned properties.</summary>
  TERDInspectorCategory = class(TCollectionItem)
  strict private
    FCaption: TCaption;
    FCollapsed: Boolean;
    FProperties: TERDInspectorPropertyCollection;
    FVisible: Boolean;
    FTag: NativeInt;
    FRect: TRect;
    procedure SetCaption(const AValue: TCaption);
    procedure SetCollapsed(AValue: Boolean);
    procedure SetProperties(AValue: TERDInspectorPropertyCollection);
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
      AKind: TERDInspectorValueKind = ivText): TERDInspectorProperty;
    /// <summary>Layout rectangle in content coordinates.</summary>
    property Rect: TRect read FRect write FRect;
  published
    /// <summary>Category header text.</summary>
    property Caption: TCaption read FCaption write SetCaption;
    /// <summary>True when child properties are hidden.</summary>
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    /// <summary>Properties owned by this category.</summary>
    property Properties: TERDInspectorPropertyCollection read FProperties
      write SetProperties;
    /// <summary>True when the category participates in layout and painting.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Application-defined value.</summary>
    property Tag: NativeInt read FTag write FTag default 0;
  end;

  /// <summary>Streamable category collection owned by the inspector.</summary>
  TERDInspectorCategoryCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TERDInspectorCategory;
    procedure SetItem(Index: Integer; AValue: TERDInspectorCategory);
  protected
    /// <summary>Notifies the owner when collection contents change.</summary>
    /// <param name="Item">Changed item, or nil for a structural change.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Adds a category row.</summary>
    /// <returns>The new category row.</returns>
    function Add: TERDInspectorCategory;
    /// <summary>Copies another category collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Indexed category rows.</summary>
    property Items[Index: Integer]: TERDInspectorCategory read GetItem
      write SetItem; default;
    /// <summary>Fires whenever a row or the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Themed property grid with inline editing and object-inspector navigation.</summary>
  TERDInspector = class(TERDCustomControl)
  strict private
    FCategories: TERDInspectorCategoryCollection;
    FSelected: TCollectionItem;
    FReadOnly: Boolean;
    FSplitterPosition: Integer;
    FScrollPos: Integer;
    FContentHeight: Integer;
    FUpdateCount: Integer;
    FEditor: TEdit;
    FEditProperty: TERDInspectorProperty;
    FEditOriginalValue: string;
    FInternalEditorUpdate: Boolean;
    FCommitting: Boolean;
    FPopup: TERDPopupList;
    FDraggingSplitter: Boolean;
    FDraggingScroll: Boolean;
    FDraggingSelect: Boolean;
    FScrollHover: Boolean;
    FScrollDragOffset: Integer;
    FLastMouse: TPoint;
    FIncrementalText: string;
    FIncrementalTick: Cardinal;
    FOnPropertySelect: TERDInspectorPropertyEvent;
    FOnPropertyChanging: TERDInspectorPropertyChangingEvent;
    FOnPropertyChanged: TERDInspectorPropertyEvent;
    FOnPropertyButtonClick: TERDInspectorPropertyEvent;
    FOnCategorySelect: TERDInspectorCategoryEvent;
    FOnCategoryCollapse: TERDInspectorCategoryEvent;
    FOnCategoryExpand: TERDInspectorCategoryEvent;
    procedure SetCategories(AValue: TERDInspectorCategoryCollection);
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
    function OverSplitter(const P: TPoint): Boolean;
    function ScrollBarWidth: Integer;
    function MaxScroll: Integer;
    function EffectiveRight: Integer;
    function VisibleValueRect(AProperty: TERDInspectorProperty): TRect;
    function EditFrameRect(AProperty: TERDInspectorProperty): TRect;
    function EllipsisButtonRect(AProperty: TERDInspectorProperty): TRect;
    function CheckBoxRect(AProperty: TERDInspectorProperty): TRect;
    function ScrollThumbRect: TRect;
    function ScrollTrackRect: TRect;
    function PropertyCategory(AProperty: TERDInspectorProperty): TERDInspectorCategory;
    function CanEditText(AProperty: TERDInspectorProperty): Boolean;
    function IsReadOnlyText(AProperty: TERDInspectorProperty): Boolean;
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
    procedure ToggleCategory(ACategory: TERDInspectorCategory);
    procedure ExpandAll(ACollapsed: Boolean);
    procedure ToggleCheck(AProperty: TERDInspectorProperty);
    procedure CyclePickList(AProperty: TERDInspectorProperty);
    procedure OpenPickList(AProperty: TERDInspectorProperty);
    procedure SelectPickListPrefix(AProperty: TERDInspectorProperty; Ch: Char);
    procedure FirePropertyChanged(AProperty: TERDInspectorProperty);
    procedure LanguageChanged(Sender: TObject);
    procedure DrawCategory(APainter: TERDPainter; ACategory: TERDInspectorCategory;
      const R: TRect);
    procedure DrawProperty(APainter: TERDPainter; AProperty: TERDInspectorProperty;
      const R: TRect);
    procedure DrawScrollBar(APainter: TERDPainter);
    function ItemAt(const P: TPoint): TCollectionItem;
    function CategoryAt(const P: TPoint): TERDInspectorCategory;
    function PropertyAt(const P: TPoint): TERDInspectorProperty;
    function ValueTruncated(AProperty: TERDInspectorProperty): Boolean;
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
    /// <param name="Message">Mouse-leave notification.</param>
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
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
    function FindProperty(const AName: string): TERDInspectorProperty;
    /// <summary>Adds a category row.</summary>
    /// <param name="ACaption">Category caption.</param>
    /// <returns>The new category.</returns>
    function AddCategory(const ACaption: string): TERDInspectorCategory;
    /// <summary>Currently selected category or property row.</summary>
    property Selected: TCollectionItem read FSelected;
  published
    /// <summary>Streamed category collection.</summary>
    property Categories: TERDInspectorCategoryCollection read FCategories
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
    property OnPropertySelect: TERDInspectorPropertyEvent read FOnPropertySelect
      write FOnPropertySelect;
    /// <summary>Fires before an edited value is committed.</summary>
    property OnPropertyChanging: TERDInspectorPropertyChangingEvent
      read FOnPropertyChanging write FOnPropertyChanging;
    /// <summary>Fires after a property value is committed.</summary>
    property OnPropertyChanged: TERDInspectorPropertyEvent read FOnPropertyChanged
      write FOnPropertyChanged;
    /// <summary>Fires when an ellipsis property button is clicked.</summary>
    property OnPropertyButtonClick: TERDInspectorPropertyEvent
      read FOnPropertyButtonClick write FOnPropertyButtonClick;
    /// <summary>Fires when a category row is selected.</summary>
    property OnCategorySelect: TERDInspectorCategoryEvent read FOnCategorySelect
      write FOnCategorySelect;
    /// <summary>Fires after a category is collapsed.</summary>
    property OnCategoryCollapse: TERDInspectorCategoryEvent read FOnCategoryCollapse
      write FOnCategoryCollapse;
    /// <summary>Fires after a category is expanded.</summary>
    property OnCategoryExpand: TERDInspectorCategoryEvent read FOnCategoryExpand
      write FOnCategoryExpand;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

type
  TERDInspectorEdit = class(TEdit)
  protected
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TERDInspectorEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  AutoSize := False;
  TabStop := False;
end;

procedure TERDInspectorEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  Message.Result := Message.Result and not DLGC_WANTTAB;
end;

{ TERDInspectorProperty }

constructor TERDInspectorProperty.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPickList := TStringList.Create;
  TStringList(FPickList).OnChange := PickListChanged;
  FKind := ivText;
  FLevel := alvNormal;
  FVisible := True;
end;

destructor TERDInspectorProperty.Destroy;
begin
  FPickList.Free;
  inherited;
end;

procedure TERDInspectorProperty.Assign(Source: TPersistent);
var
  P: TERDInspectorProperty;
begin
  if Source is TERDInspectorProperty then
  begin
    P := TERDInspectorProperty(Source);
    FName := P.Name;
    FValue := P.Value;
    FDefaultValue := P.DefaultValue;
    FHasDefault := P.HasDefault;
    FKind := P.Kind;
    FPickList.Assign(P.PickList);
    FChecked := P.Checked;
    FCheckedCaption := P.FCheckedCaption;
    FUncheckedCaption := P.FUncheckedCaption;
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

function TERDInspectorProperty.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

function TERDInspectorProperty.GetCategory: TERDInspectorCategory;
begin
  Result := nil;
  if Collection is TERDInspectorPropertyCollection then
    Result := TERDInspectorPropertyCollection(Collection).OwnerCategory;
end;

function TERDInspectorProperty.GetModified: Boolean;
begin
  Result := FHasDefault and (FValue <> FDefaultValue);
end;

procedure TERDInspectorProperty.PickListChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TERDInspectorProperty.ResetToDefault;
begin
  if FHasDefault then
    Value := FDefaultValue;
end;

procedure TERDInspectorProperty.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetValue(const AValue: string);
begin
  if FKind = ivCheck then
  begin
    FChecked := SameText(AValue, 'True') or SameText(AValue, CheckedCaption) or
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

procedure TERDInspectorProperty.SetDefaultValue(const AValue: string);
begin
  if FDefaultValue = AValue then
    Exit;
  FDefaultValue := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetHasDefault(AValue: Boolean);
begin
  if FHasDefault = AValue then
    Exit;
  FHasDefault := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetKind(AValue: TERDInspectorValueKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  if FKind = ivCheck then
    SetChecked(FChecked)
  else
    Changed(False);
end;

procedure TERDInspectorProperty.SetPickList(AValue: TStrings);
begin
  FPickList.Assign(AValue);
  Changed(False);
end;

procedure TERDInspectorProperty.SetChecked(AValue: Boolean);
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

procedure TERDInspectorProperty.SetCheckedCaption(const AValue: string);
begin
  if FCheckedCaption = AValue then
    Exit;
  FCheckedCaption := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetUncheckedCaption(const AValue: string);
begin
  if FUncheckedCaption = AValue then
    Exit;
  FUncheckedCaption := AValue;
  Changed(False);
end;

function TERDInspectorProperty.GetCheckedCaption: string;
begin
  if FCheckedCaption <> '' then
    Result := FCheckedCaption
  else
    Result := Tr('ui.inspector.boolean.on', 'On');
end;

function TERDInspectorProperty.GetUncheckedCaption: string;
begin
  if FUncheckedCaption <> '' then
    Result := FUncheckedCaption
  else
    Result := Tr('ui.inspector.boolean.off', 'Off');
end;

function TERDInspectorProperty.IsCheckedCaptionStored: Boolean;
begin
  Result := FCheckedCaption <> '';
end;

function TERDInspectorProperty.IsUncheckedCaptionStored: Boolean;
begin
  Result := FUncheckedCaption <> '';
end;

procedure TERDInspectorProperty.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetLevel(AValue: TERDAlertLevel);
begin
  if FLevel = AValue then
    Exit;
  FLevel := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDInspectorProperty.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

{ TERDInspectorPropertyCollection }

function TERDInspectorPropertyCollection.Add: TERDInspectorProperty;
begin
  Result := TERDInspectorProperty(inherited Add);
end;

function TERDInspectorPropertyCollection.OwnerCategory: TERDInspectorCategory;
var
  O: TPersistent;
begin
  Result := nil;
  O := GetOwner;
  if O is TERDInspectorCategory then
    Result := TERDInspectorCategory(O);
end;

procedure TERDInspectorPropertyCollection.Assign(Source: TPersistent);
var
  Src: TERDInspectorPropertyCollection;
  I: Integer;
begin
  if Source is TERDInspectorPropertyCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TERDInspectorPropertyCollection(Source);
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

function TERDInspectorPropertyCollection.GetItem(Index: Integer): TERDInspectorProperty;
begin
  Result := TERDInspectorProperty(inherited GetItem(Index));
end;

procedure TERDInspectorPropertyCollection.SetItem(Index: Integer;
  AValue: TERDInspectorProperty);
begin
  inherited SetItem(Index, AValue);
end;

procedure TERDInspectorPropertyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TERDInspectorCategory }

constructor TERDInspectorCategory.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCaption := Format('Category %d', [Index + 1]);
  FProperties := TERDInspectorPropertyCollection.Create(Self,
    TERDInspectorProperty);
  FProperties.OnChange := PropertiesChanged;
  FVisible := True;
end;

destructor TERDInspectorCategory.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TERDInspectorCategory.Assign(Source: TPersistent);
var
  C: TERDInspectorCategory;
begin
  if Source is TERDInspectorCategory then
  begin
    C := TERDInspectorCategory(Source);
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

function TERDInspectorCategory.AddProperty(const AName, AValue: string;
  AKind: TERDInspectorValueKind): TERDInspectorProperty;
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

function TERDInspectorCategory.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TERDInspectorCategory.PropertiesChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TERDInspectorCategory.SetCaption(const AValue: TCaption);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TERDInspectorCategory.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed = AValue then
    Exit;
  FCollapsed := AValue;
  Changed(False);
end;

procedure TERDInspectorCategory.SetProperties(AValue: TERDInspectorPropertyCollection);
begin
  FProperties.Assign(AValue);
end;

procedure TERDInspectorCategory.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

{ TERDInspectorCategoryCollection }

function TERDInspectorCategoryCollection.Add: TERDInspectorCategory;
begin
  Result := TERDInspectorCategory(inherited Add);
end;

procedure TERDInspectorCategoryCollection.Assign(Source: TPersistent);
var
  Src: TERDInspectorCategoryCollection;
  I: Integer;
begin
  if Source is TERDInspectorCategoryCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TERDInspectorCategoryCollection(Source);
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

function TERDInspectorCategoryCollection.GetItem(Index: Integer): TERDInspectorCategory;
begin
  Result := TERDInspectorCategory(inherited GetItem(Index));
end;

procedure TERDInspectorCategoryCollection.SetItem(Index: Integer;
  AValue: TERDInspectorCategory);
begin
  inherited SetItem(Index, AValue);
end;

procedure TERDInspectorCategoryCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TERDInspector }

constructor TERDInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csDoubleClicks,
    csCaptureMouse];
  Width := 360;
  Height := 240;
  TabStop := True;
  FSplitterPosition := 150;
  FUpdateCount := 0;
  FCategories := TERDInspectorCategoryCollection.Create(Self,
    TERDInspectorCategory);
  FCategories.OnChange := CategoriesChanged;

  FEditor := TERDInspectorEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Visible := False;
  FEditor.OnExit := EditorExit;
  FEditor.OnKeyDown := EditorKeyDown;

  FPopup := TERDPopupList.Create(Self);
  FPopup.OnCloseUp := PopupCloseUp;
  ApplyEditorTheme;
  ERDI18n.AddChangeListener(LanguageChanged);
end;

destructor TERDInspector.Destroy;
begin
  ERDI18n.RemoveChangeListener(LanguageChanged);
  FPopup.Free;
  FEditor.Free;
  FCategories.Free;
  inherited;
end;

procedure TERDInspector.LanguageChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TERDInspector.Assign(Source: TPersistent);
var
  I: TERDInspector;
begin
  inherited;
  if Source is TERDInspector then
  begin
    I := TERDInspector(Source);
    FCategories.Assign(I.Categories);
    FSplitterPosition := I.SplitterPosition;
    FReadOnly := I.ReadOnly;
    FScrollPos := 0;
    EnsureLayout;
    Invalidate;
  end;
end;

procedure TERDInspector.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TERDInspector.EndUpdate;
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

procedure TERDInspector.Clear;
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

function TERDInspector.AddCategory(const ACaption: string): TERDInspectorCategory;
begin
  Result := FCategories.Add;
  Result.Caption := ACaption;
end;

function TERDInspector.FindProperty(const AName: string): TERDInspectorProperty;
var
  C, P: Integer;
begin
  Result := nil;
  for C := 0 to FCategories.Count - 1 do
    for P := 0 to FCategories[C].Properties.Count - 1 do
      if SameText(FCategories[C].Properties[P].Name, AName) then
        Exit(FCategories[C].Properties[P]);
end;

procedure TERDInspector.SetCategories(AValue: TERDInspectorCategoryCollection);
begin
  FCategories.Assign(AValue);
end;

procedure TERDInspector.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  ActivateEditor(True);
  Invalidate;
end;

procedure TERDInspector.SetSplitterPosition(AValue: Integer);
begin
  AValue := System.Math.Max(40, AValue);
  if FSplitterPosition = AValue then
    Exit;
  FSplitterPosition := AValue;
  UpdateEditorBounds;
  Invalidate;
end;

procedure TERDInspector.CategoriesChanged(Sender: TObject);
begin
  if FUpdateCount > 0 then
    Exit;
  EnsureLayout;
  SetScrollPos(FScrollPos);
  UpdateEditorBounds;
  Invalidate;
end;

procedure TERDInspector.ApplyEditorTheme;
var
  P: TERDThemePalette;
  FontName: string;
  FontHeight: Integer;
begin
  P := Palette;
  if (FEditProperty <> nil) and (FEditProperty.Kind = ivEllipsis) then
    FontName := 'Consolas'
  else
    FontName := 'Segoe UI';
  FontHeight := -System.Math.Max(1, Round(12.5 * ScaleValue(96) / 96));
  // Only assign changed values: every font assignment makes the edit repaint,
  // and this runs on each paint of the inspector.
  if FEditor.Color <> P.GaugeFace then
    FEditor.Color := P.GaugeFace;
  if not SameText(FEditor.Font.Name, FontName) then
    FEditor.Font.Name := FontName;
  if FEditor.Font.Color <> P.ForegroundText then
    FEditor.Font.Color := P.ForegroundText;
  if FEditor.Font.Height <> FontHeight then
    FEditor.Font.Height := FontHeight;
end;

function TERDInspector.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.CompactRow);
end;

function TERDInspector.GutterWidth: Integer;
begin
  Result := ScaleValue(20);
end;

function TERDInspector.ScrollBarWidth: Integer;
begin
  Result := ScaleValue(10);
end;

function TERDInspector.EffectiveRight: Integer;
begin
  Result := ClientWidth - 1;
  if FContentHeight > ClientHeight - 2 then
    Dec(Result, ScrollBarWidth + ScaleValue(2));
end;

function TERDInspector.OverSplitter(const P: TPoint): Boolean;
begin
  // The splitter only exists on property rows; category headers span the
  // full width.
  Result := (P.X >= SplitterX - ScaleValue(3)) and
    (P.X <= SplitterX + ScaleValue(3)) and (PropertyAt(P) <> nil);
end;

function TERDInspector.SplitterX: Integer;
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

function TERDInspector.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, FContentHeight - System.Math.Max(0,
    ClientHeight - 2));
end;

procedure TERDInspector.EnsureLayout;
var
  Y, C, P, RH: Integer;
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
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

procedure TERDInspector.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  UpdateEditorBounds;
  Invalidate;
end;

function TERDInspector.PropertyCategory(
  AProperty: TERDInspectorProperty): TERDInspectorCategory;
begin
  if AProperty <> nil then
    Result := AProperty.Category
  else
    Result := nil;
end;

function TERDInspector.CanEditText(AProperty: TERDInspectorProperty): Boolean;
begin
  Result := (AProperty <> nil) and Enabled and not FReadOnly and
    (AProperty.Kind in [ivText, ivEllipsis]) and
    not ((AProperty.Kind = ivEllipsis) and AProperty.ReadOnly);
end;

function TERDInspector.IsReadOnlyText(AProperty: TERDInspectorProperty): Boolean;
begin
  Result := (AProperty <> nil) and
    ((AProperty.Kind = ivReadOnly) or FReadOnly or not Enabled or
    ((AProperty.Kind = ivEllipsis) and AProperty.ReadOnly));
end;

function TERDInspector.VisibleValueRect(AProperty: TERDInspectorProperty): TRect;
var
  R: TRect;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Result := Rect(SplitterX + ScaleValue(8), R.Top + ScaleValue(4),
    EditFrameRect(AProperty).Right - ScaleValue(6), R.Bottom - ScaleValue(4));
  if AProperty.Kind = ivEllipsis then
    Result.Right := EllipsisButtonRect(AProperty).Left - ScaleValue(4);
end;

function TERDInspector.EditFrameRect(AProperty: TERDInspectorProperty): TRect;
var
  R: TRect;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  Result := Rect(SplitterX + ScaleValue(2), R.Top + ScaleValue(3),
    EffectiveRight - ScaleValue(4), R.Bottom - ScaleValue(3));
end;

function TERDInspector.EllipsisButtonRect(AProperty: TERDInspectorProperty): TRect;
var
  R: TRect;
  S: Integer;
begin
  R := AProperty.Rect;
  OffsetRect(R, 0, -FScrollPos);
  S := RowHeight - ScaleValue(10);
  Result := Rect(EffectiveRight - ScaleValue(6) - S, R.Top + ScaleValue(5),
    EffectiveRight - ScaleValue(6), R.Top + ScaleValue(5) + S);
end;

function TERDInspector.CheckBoxRect(AProperty: TERDInspectorProperty): TRect;
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

function TERDInspector.ScrollTrackRect: TRect;
begin
  Result := Rect(ClientWidth - 1 - ScrollBarWidth, 1, ClientWidth - 1,
    ClientHeight - 1);
end;

function TERDInspector.ScrollThumbRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if FContentHeight <= ClientHeight - 2 then
    Exit;
  // Same themed overlay bar as the other ERD controls (ERD.Components.ScrollBar);
  // positions are in pixels.
  Result := ERDScrollThumbRect(ScrollTrackRect, True, 0, FContentHeight - 1,
    ClientHeight - 2, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TERDInspector.UpdateEditorBounds;
var
  R, Row: TRect;
  H: Integer;
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
  if R.Right < R.Left then
    R.Right := R.Left;
  // A borderless TEdit draws its text at the top of its client area, so
  // size it to one text line and centre that line in the row (rows are much
  // taller in tablet density).
  H := System.Math.Max(1, System.Math.Min(Row.Height - ScaleValue(2),
    ERDFontLineHeight(FEditor.Font)));
  FEditor.SetBounds(R.Left, Row.Top + (Row.Height - H) div 2, R.Width, H);
end;

procedure TERDInspector.HideEditor;
begin
  FEditProperty := nil;
  FEditor.Visible := False;
end;

procedure TERDInspector.ActivateEditor(ASelectAll: Boolean);
var
  Prop: TERDInspectorProperty;
  EditorHadFocus: Boolean;
begin
  // Hiding the focused editor hands the focus to the form, not to the
  // inspector, so the arrow keys would stop working on a pick-list or
  // check row reached from a text row: take the focus back.
  EditorHadFocus := FEditor.Visible and FEditor.Focused;
  HideEditor;
  if not (FSelected is TERDInspectorProperty) or
    not (TERDInspectorProperty(FSelected).Kind in [ivText, ivReadOnly,
    ivEllipsis]) then
  begin
    if EditorHadFocus and CanFocus then
      SetFocus;
    Exit;
  end;
  Prop := TERDInspectorProperty(FSelected);
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

function TERDInspector.CommitEditor(AKeepFocus: Boolean): Boolean;
var
  NewValue: string;
  Accept: Boolean;
  Prop: TERDInspectorProperty;
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

procedure TERDInspector.RevertEditor;
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

procedure TERDInspector.EditorExit(Sender: TObject);
begin
  if FPopup.IsOpen or FInternalEditorUpdate then
    Exit;
  CommitEditor(False);
end;

procedure TERDInspector.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FPopup.IsOpen and FPopup.HandleKey(Key, Shift) then
    Exit;

  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          if (FSelected is TERDInspectorProperty) and
            (TERDInspectorProperty(FSelected).Kind = ivEllipsis) and
            Assigned(FOnPropertyButtonClick) then
            FOnPropertyButtonClick(Self, TERDInspectorProperty(FSelected));
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
          if FSelected is TERDInspectorProperty then
            OpenPickList(TERDInspectorProperty(FSelected));
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
        if FSelected is TERDInspectorProperty then
          OpenPickList(TERDInspectorProperty(FSelected));
        Key := 0;
      end;
  end;
end;

procedure TERDInspector.SelectItem(AItem: TCollectionItem; ASelectAll: Boolean);
begin
  if AItem = FSelected then
  begin
    ActivateEditor(ASelectAll);
    Exit;
  end;
  if not CommitEditor(True) then
    Exit;
  FSelected := AItem;
  if FSelected is TERDInspectorProperty then
  begin
    if Assigned(FOnPropertySelect) then
      FOnPropertySelect(Self, TERDInspectorProperty(FSelected));
    ActivateEditor(ASelectAll);
  end
  else
  begin
    HideEditor;
    if (FSelected is TERDInspectorCategory) and Assigned(FOnCategorySelect) then
      FOnCategorySelect(Self, TERDInspectorCategory(FSelected));
    if CanFocus then
      SetFocus;
  end;
  EnsureSelectedVisible;
  Invalidate;
end;

function TERDInspector.ItemAt(const P: TPoint): TCollectionItem;
var
  C, I, Y: Integer;
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
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

function TERDInspector.CategoryAt(const P: TPoint): TERDInspectorCategory;
var
  Item: TCollectionItem;
begin
  Item := ItemAt(P);
  if Item is TERDInspectorCategory then
    Result := TERDInspectorCategory(Item)
  else
    Result := nil;
end;

function TERDInspector.PropertyAt(const P: TPoint): TERDInspectorProperty;
var
  Item: TCollectionItem;
begin
  Item := ItemAt(P);
  if Item is TERDInspectorProperty then
    Result := TERDInspectorProperty(Item)
  else
    Result := nil;
end;

procedure TERDInspector.EnsureSelectedVisible;
var
  R: TRect;
begin
  if FSelected is TERDInspectorCategory then
    R := TERDInspectorCategory(FSelected).Rect
  else if FSelected is TERDInspectorProperty then
    R := TERDInspectorProperty(FSelected).Rect
  else
    Exit;
  if R.Top < FScrollPos + 1 then
    SetScrollPos(R.Top - 1)
  else if R.Bottom > FScrollPos + ClientHeight - 1 then
    SetScrollPos(R.Bottom - ClientHeight + 1);
end;

procedure TERDInspector.SelectRelative(ADelta: Integer);
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

procedure TERDInspector.SelectFirst;
begin
  SelectRelative(-1000000);
end;

procedure TERDInspector.SelectLast;
begin
  SelectRelative(1000000);
end;

procedure TERDInspector.SelectPage(ADirection: Integer);
var
  Delta: Integer;
begin
  Delta := System.Math.Max(1, (ClientHeight - 2) div RowHeight);
  SelectRelative(ADirection * Delta);
end;

procedure TERDInspector.ToggleCategory(ACategory: TERDInspectorCategory);
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

procedure TERDInspector.ExpandAll(ACollapsed: Boolean);
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

procedure TERDInspector.FirePropertyChanged(AProperty: TERDInspectorProperty);
begin
  Invalidate;
  if Assigned(FOnPropertyChanged) then
    FOnPropertyChanged(Self, AProperty);
end;

procedure TERDInspector.ToggleCheck(AProperty: TERDInspectorProperty);
begin
  if (AProperty = nil) or (AProperty.Kind <> ivCheck) or FReadOnly or not Enabled then
    Exit;
  AProperty.Checked := not AProperty.Checked;
  FirePropertyChanged(AProperty);
end;

procedure TERDInspector.CyclePickList(AProperty: TERDInspectorProperty);
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

procedure TERDInspector.OpenPickList(AProperty: TERDInspectorProperty);
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

procedure TERDInspector.PopupCloseUp(Sender: TObject; AAccepted: Boolean;
  AIndex: Integer);
var
  Prop: TERDInspectorProperty;
begin
  if not (FSelected is TERDInspectorProperty) then
    Exit;
  Prop := TERDInspectorProperty(FSelected);
  if AAccepted and (Prop.Kind = ivPickList) and (AIndex >= 0) and
    (AIndex < Prop.PickList.Count) and not FReadOnly then
  begin
    Prop.Value := Prop.PickList[AIndex];
    FirePropertyChanged(Prop);
  end;
  if CanFocus then
    SetFocus;
end;

procedure TERDInspector.SelectPickListPrefix(AProperty: TERDInspectorProperty;
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

procedure TERDInspector.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Save: Integer;
  C, I: Integer;
  R: TRect;
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
begin
  EnsureLayout;
  ApplyEditorTheme;
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
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
      DrawScrollBar(P);
    finally
      RestoreDC(ACanvas.Handle, Save);
    end;
  finally
    P.Free;
  end;
  UpdateEditorBounds;
end;

procedure TERDInspector.DrawCategory(APainter: TERDPainter;
  ACategory: TERDInspectorCategory; const R: TRect);
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

procedure TERDInspector.DrawProperty(APainter: TERDPainter;
  AProperty: TERDInspectorProperty; const R: TRect);
var
  SelectedRow: Boolean;
  Ink, ValueInk: TColor;
  NameWeight, ValueWeight, MonoWeight: TERDTextWeight;
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

  APainter.VLine(SplitterX, R.Top, R.Height, Palette.NeutralLight);

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
    // Fill the field with the editor's own colour: the editor is one text
    // line high, so on taller (tablet) rows the field shows above and below
    // it and must not show the selected-row tint.
    APainter.EditFrame(EditFrameRect(AProperty), True, True);
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

procedure TERDInspector.DrawScrollBar(APainter: TERDPainter);
begin
  if FContentHeight <= ClientHeight - 2 then
    Exit;
  ERDPaintScrollBar(APainter, ScrollTrackRect, True, 0, FContentHeight - 1,
    ClientHeight - 2, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TERDInspector.Resize;
begin
  inherited;
  EnsureLayout;
  SetScrollPos(FScrollPos);
  UpdateEditorBounds;
end;

procedure TERDInspector.DensityChanged;
begin
  inherited;
  EnsureLayout;
  ApplyEditorTheme;
  UpdateEditorBounds;
end;

procedure TERDInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
  Item: TCollectionItem;
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
  Thumb: TRect;
begin
  inherited;
  if FPopup.IsOpen then
  begin
    // Pressing the open pick-list field again closes it, like a combo box.
    FPopup.CloseUp(False);
    Exit;
  end;
  if not Enabled or (Button <> mbLeft) then
    Exit;
  P := Point(X, Y);
  FLastMouse := P;
  if CanFocus and not Focused then
    SetFocus;

  Thumb := ScrollThumbRect;
  if not Thumb.IsEmpty and (X >= ScrollTrackRect.Left) and (Y >= Thumb.Top) and
    (Y < Thumb.Bottom) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    MouseCapture := True;
    Invalidate;
    Exit;
  end;
  if (FContentHeight > ClientHeight - 2) and
    (X >= ScrollTrackRect.Left) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - (ClientHeight - RowHeight))
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + (ClientHeight - RowHeight));
    Exit;
  end;

  if OverSplitter(P) then
  begin
    FDraggingSplitter := True;
    Cursor := crSizeWE;
    Exit;
  end;

  // The chevron gutter toggles a category; in tablet density the whole
  // header does, as the gutter is too small a target for a finger. The
  // second press of a double-click is left to DblClick.
  Cat := CategoryAt(P);
  if (Cat <> nil) and ((X <= GutterWidth) or (Density = dnTablet)) then
  begin
    SelectItem(Cat, False);
    if not (ssDouble in Shift) then
      ToggleCategory(Cat);
    Exit;
  end;

  Prop := PropertyAt(P);
  if Prop <> nil then
  begin
    SelectItem(Prop, True);
    if (Prop.Kind = ivCheck) and PtInRect(CheckBoxRect(Prop), P) then
      ToggleCheck(Prop)
    else if (Prop.Kind = ivPickList) and (X > SplitterX + ScaleValue(3)) then
      // Like a csDropDownList combo box: the whole value field opens the
      // list, not only the arrow button.
      OpenPickList(Prop)
    else if (Prop.Kind = ivEllipsis) and PtInRect(EllipsisButtonRect(Prop), P) and
      Assigned(FOnPropertyButtonClick) then
      FOnPropertyButtonClick(Self, Prop)
    else
      FDraggingSelect := FSelected = Prop;
    Exit;
  end;

  Item := ItemAt(P);
  if Item <> nil then
  begin
    SelectItem(Item, False);
    FDraggingSelect := FSelected = Item;
  end;
end;

procedure TERDInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPos: Integer;
  Track: TRect;
  Thumb: TRect;
  Item: TCollectionItem;
  NewHover: Boolean;
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
    Track := ScrollTrackRect;
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll,
        Track.Height - Thumb.Height));
    Exit;
  end;
  if FDraggingSelect then
  begin
    if not (ssLeft in Shift) then
      FDraggingSelect := False
    else
    begin
      // Holding the button and sweeping over rows moves the selection, like
      // the Delphi Object Inspector; dragging past an edge scrolls.
      if Y < 0 then
        SetScrollPos(FScrollPos - RowHeight)
      else if Y >= ClientHeight then
        SetScrollPos(FScrollPos + RowHeight);
      Item := ItemAt(Point(X, EnsureRange(Y, 1, ClientHeight - 2)));
      if (Item <> nil) and (Item <> FSelected) then
      begin
        SelectItem(Item, True);
        if FSelected <> Item then
          FDraggingSelect := False;
      end;
      Exit;
    end;
  end;
  NewHover := (FContentHeight > ClientHeight - 2) and
    (X >= ScrollTrackRect.Left);
  if OverSplitter(Point(X, Y)) then
    Cursor := crSizeWE
  else if (Density = dnTablet) and not NewHover and
    (CategoryAt(Point(X, Y)) <> nil) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  if NewHover <> FScrollHover then
  begin
    FScrollHover := NewHover;
    Invalidate;
  end;
end;

procedure TERDInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDraggingSplitter := False;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    MouseCapture := False;
    Invalidate;
  end;
  FDraggingSelect := False;
  Cursor := crDefault;
end;

procedure TERDInspector.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FDraggingSplitter then
    Cursor := crDefault;
  if FScrollHover then
  begin
    FScrollHover := False;
    Invalidate;
  end;
end;

procedure TERDInspector.DblClick;
var
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
begin
  inherited;
  if OverSplitter(FLastMouse) then
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

procedure TERDInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cat: TERDInspectorCategory;
  Prop: TERDInspectorProperty;
begin
  inherited;
  if FPopup.IsOpen and FPopup.HandleKey(Key, Shift) then
    Exit;
  if not Enabled then
    Exit;

  if FSelected is TERDInspectorCategory then
    Cat := TERDInspectorCategory(FSelected)
  else
    Cat := nil;
  if FSelected is TERDInspectorProperty then
    Prop := TERDInspectorProperty(FSelected)
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

procedure TERDInspector.KeyPress(var Key: Char);
var
  Prop: TERDInspectorProperty;
begin
  inherited;
  if not (FSelected is TERDInspectorProperty) or (Key < #32) then
    Exit;
  Prop := TERDInspectorProperty(FSelected);
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

function TERDInspector.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if ssCtrl in Shift then
    SelectRelative(1)
  else
    SetScrollPos(FScrollPos + RowHeight * 3);
end;

function TERDInspector.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if ssCtrl in Shift then
    SelectRelative(-1)
  else
    SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TERDInspector.CMHintShow(var Message: TCMHintShow);
var
  Prop: TERDInspectorProperty;
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

function TERDInspector.ValueTruncated(AProperty: TERDInspectorProperty): Boolean;
var
  P: TERDPainter;
  R: TRect;
  W: Integer;
begin
  Result := False;
  if (AProperty = nil) or (AProperty.Value = '') then
    Exit;
  P := TERDPainter.Create(Canvas, Palette, ScaleValue(96));
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

procedure TERDInspector.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  Message.Result := Message.Result and not DLGC_WANTTAB;
end;

end.
