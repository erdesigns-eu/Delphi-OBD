//------------------------------------------------------------------------------
//  ERD.Components.KeyValueList
//
//  TERDKeyValueList - read-only, self-painted details list for Application
//  cards and inspector surfaces.
//
//  - Category captions with hairlines divide related key/value rows.
//  - Values can be regular text, monospace data, status chips, links or tone
//    coloured text.
//  - One or two columns, hover copy affordance and themed overlay scrolling
//    are implemented without native scroll bars.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.KeyValueList;

interface

uses
  ERD.Components.Types,
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Clipbrd,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.ScrollBar;

type
  /// <summary>Value presentation used by <see cref="TERDKeyValueList"/>.</summary>
  TERDKeyValueKind = (
    /// <summary>Semibold proportional value text.</summary>
    kvkText,
    /// <summary>Monospace value text.</summary>
    kvkMono,
    /// <summary>Status chip value.</summary>
    kvkChip,
    /// <summary>Accent-coloured clickable link.</summary>
    kvkLink,
    /// <summary>Semibold text coloured by <see cref="TERDKeyValueItem.Tone"/>.</summary>
    kvkTone);

  /// <summary>Fires for a key/value row index.</summary>
  /// <param name="Sender">The key/value list.</param>
  /// <param name="AIndex">Item index.</param>
  TERDKeyValueEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  TERDKeyValueList = class;

  /// <summary>One key/value row.</summary>
  TERDKeyValueItem = class(TCollectionItem)
  strict private
    FCategory: string;
    FKey: string;
    FValue: string;
    FKind: TERDKeyValueKind;
    FTone: TERDStatusKind;
    FHint: string;
    procedure SetCategory(const AValue: string);
    procedure SetKey(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetKind(AValue: TERDKeyValueKind);
    procedure SetTone(AValue: TERDStatusKind);
    procedure SetHint(const AValue: string);
  protected
    /// <summary>Returns the key in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a text key/value row.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another key/value row.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Category caption that groups this row.</summary>
    property Category: string read FCategory write SetCategory;
    /// <summary>Key caption shown on the left.</summary>
    property Key: string read FKey write SetKey;
    /// <summary>Value text shown on the right.</summary>
    property Value: string read FValue write SetValue;
    /// <summary>How the value is drawn.</summary>
    property Kind: TERDKeyValueKind read FKind write SetKind default kvkText;
    /// <summary>Status colour used by chip and tone values.</summary>
    property Tone: TERDStatusKind read FTone write SetTone default skNeutral;
    /// <summary>Optional row hint.</summary>
    property Hint: string read FHint write SetHint;
  end;

  /// <summary>Streamable key/value row collection.</summary>
  TERDKeyValueItems = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TERDKeyValueItem;
    procedure SetItem(Index: Integer; AValue: TERDKeyValueItem);
  protected
    /// <summary>Notifies the owning list when an item changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a key/value collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one key/value row.</summary>
    /// <returns>The new row.</returns>
    function Add: TERDKeyValueItem;
    /// <summary>Copies another key/value collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed key/value indexer.</summary>
    property Items[Index: Integer]: TERDKeyValueItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Read-only themed list of category key/value rows.</summary>
  TERDKeyValueList = class(TERDCustomControl)
  strict private
    type
      TVisualKind = (kvvCategory, kvvRow);
      TVisualEntry = record
        Kind: TVisualKind;
        Index: Integer;
        Rect: TRect;
        ValueRect: TRect;
        CopyRect: TRect;
        Text: string;
      end;
      TVisualEntries = array of TVisualEntry;
      TStringArray = array of string;
  strict private
    FItems: TERDKeyValueItems;
    FColumnCount: Integer;
    FKeyWidth: Integer;
    FShowCategories: Boolean;
    FCopyOnHover: Boolean;
    FWordWrap: Boolean;
    FHotIndex: Integer;
    FCopyHot: Boolean;
    FScrollY: Integer;
    FBarHot: Boolean;
    FBarDragging: Boolean;
    FBarDragOffset: Integer;
    FOnCopy: TERDKeyValueEvent;
    FOnLinkClick: TERDKeyValueEvent;
    procedure SetItems(AValue: TERDKeyValueItems);
    procedure SetColumnCount(AValue: Integer);
    procedure SetKeyWidth(AValue: Integer);
    procedure SetShowCategories(AValue: Boolean);
    procedure SetCopyOnHover(AValue: Boolean);
    procedure SetWordWrap(AValue: Boolean);
    procedure SetHotIndex(AValue: Integer);
    procedure SetCopyHot(AValue: Boolean);
    procedure SetScrollY(AValue: Integer);
    procedure SetBarHot(AValue: Boolean);
    function BarWidth: Integer;
    function RowHeight: Integer;
    function LineHeight: Integer;
    function CategoryHeight: Integer;
    function ContentRectNoBar: TRect;
    function ContentRectWithBar(AWithBar: Boolean): TRect;
    function NeedsScrollBar: Boolean;
    function ScrollBarRect: TRect;
    function ScrollThumbRect: TRect;
    function MaxScroll: Integer;
    function ValueLineCount(const AValue: string; AValueWidth: Integer): Integer;
    function RowHeightFor(AIndex, AValueWidth: Integer): Integer;
    procedure AddCategoryName(var ACategories: TStringArray;
      const ACategory: string);
    function CategoryRowCount(const ACategory: string): Integer;
    procedure BuildLayout(AWithBar: Boolean; out AEntries: TVisualEntries;
      out AHeight: Integer);
    procedure DrawValue(APainter: TERDPainter; AItem: TERDKeyValueItem;
      const ARect: TRect; ACopySpace: Integer);
    procedure DrawPreview(APainter: TERDPainter);
    procedure CopyRow(AIndex: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  protected
    /// <summary>Paints the key/value list.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Keeps the scroll position valid after a resize.</summary>
    procedure Resize; override;
    /// <summary>Starts scroll dragging, copies a value or opens a link.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Tracks row hover, copy affordance and scroll dragging.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    /// <summary>Creates an empty key/value list.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row collection.</summary>
    destructor Destroy; override;
    /// <summary>Notifies the list that its collection changed.</summary>
    procedure ItemsChanged;
    /// <summary>Adds one key/value row.</summary>
    /// <param name="ACategory">Category caption.</param>
    /// <param name="AKey">Key caption.</param>
    /// <param name="AValue">Value text.</param>
    /// <param name="AKind">Value presentation.</param>
    /// <returns>The new row.</returns>
    function Add(const ACategory, AKey, AValue: string;
      AKind: TERDKeyValueKind = kvkText): TERDKeyValueItem;
    /// <summary>Updates the first matching key, or adds it when missing.</summary>
    /// <param name="AKey">Key caption.</param>
    /// <param name="AValue">New value text.</param>
    procedure SetValue(const AKey, AValue: string);
    /// <summary>Removes every row.</summary>
    procedure Clear;
    /// <summary>Starts a batched item update.</summary>
    procedure BeginUpdate;
    /// <summary>Ends a batched item update.</summary>
    procedure EndUpdate;
    /// <summary>Row item at a client point.</summary>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    /// <returns>Item index, or -1.</returns>
    function RowAt(X, Y: Integer): Integer;
    /// <summary>Total content height in pixels for the current width.</summary>
    /// <returns>Content height.</returns>
    function ContentHeight: Integer;
    /// <summary>Row under the mouse, or -1.</summary>
    property HotIndex: Integer read FHotIndex;
  published
    /// <summary>Streamed key/value row collection.</summary>
    property Items: TERDKeyValueItems read FItems write SetItems;
    /// <summary>Number of key/value columns: 1 or 2.</summary>
    property ColumnCount: Integer read FColumnCount write SetColumnCount
      default 1;
    /// <summary>Key caption width in 96-DPI pixels, clamped per column.</summary>
    property KeyWidth: Integer read FKeyWidth write SetKeyWidth default 150;
    /// <summary>Shows category captions and hairlines.</summary>
    property ShowCategories: Boolean read FShowCategories write SetShowCategories
      default True;
    /// <summary>Shows a copy glyph on the hovered row.</summary>
    property CopyOnHover: Boolean read FCopyOnHover write SetCopyOnHover
      default True;
    /// <summary>Wraps long text values instead of ellipsising one line.</summary>
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density;
    /// <summary>Takes density from the owning theme.</summary>
    property ParentDensity;
    /// <summary>Fires after a value is copied.</summary>
    property OnCopy: TERDKeyValueEvent read FOnCopy write FOnCopy;
    /// <summary>Fires when a link value is clicked.</summary>
    property OnLinkClick: TERDKeyValueEvent read FOnLinkClick write FOnLinkClick;
  end;

implementation

{ TERDKeyValueItem }

constructor TERDKeyValueItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FKind := kvkText;
  FTone := skNeutral;
end;

procedure TERDKeyValueItem.Assign(Source: TPersistent);
var
  Item: TERDKeyValueItem;
begin
  if Source is TERDKeyValueItem then
  begin
    Item := TERDKeyValueItem(Source);
    FCategory := Item.Category;
    FKey := Item.Key;
    FValue := Item.Value;
    FKind := Item.Kind;
    FTone := Item.Tone;
    FHint := Item.Hint;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TERDKeyValueItem.GetDisplayName: string;
begin
  Result := FKey;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDKeyValueItem.SetCategory(const AValue: string);
begin
  if FCategory = AValue then
    Exit;
  FCategory := AValue;
  Changed(False);
end;

procedure TERDKeyValueItem.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDKeyValueItem.SetKey(const AValue: string);
begin
  if FKey = AValue then
    Exit;
  FKey := AValue;
  Changed(False);
end;

procedure TERDKeyValueItem.SetKind(AValue: TERDKeyValueKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TERDKeyValueItem.SetTone(AValue: TERDStatusKind);
begin
  if FTone = AValue then
    Exit;
  FTone := AValue;
  Changed(False);
end;

procedure TERDKeyValueItem.SetValue(const AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  Changed(False);
end;

{ TERDKeyValueItems }

constructor TERDKeyValueItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDKeyValueItem);
end;

function TERDKeyValueItems.Add: TERDKeyValueItem;
begin
  Result := TERDKeyValueItem(inherited Add);
end;

procedure TERDKeyValueItems.Assign(Source: TPersistent);
var
  Src: TERDKeyValueItems;
  I: Integer;
begin
  if Source is TERDKeyValueItems then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TERDKeyValueItems(Source);
      for I := 0 to Src.Count - 1 do
        Add.Assign(Src[I]);
    finally
      EndUpdate;
    end;
    Update(nil);
  end
  else
    inherited Assign(Source);
end;

function TERDKeyValueItems.GetItem(Index: Integer): TERDKeyValueItem;
begin
  Result := TERDKeyValueItem(inherited GetItem(Index));
end;

procedure TERDKeyValueItems.SetItem(Index: Integer; AValue: TERDKeyValueItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TERDKeyValueItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TERDKeyValueList then
    TERDKeyValueList(GetOwner).ItemsChanged;
end;

{ TERDKeyValueList }

constructor TERDKeyValueList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := False;
  Width := 460;
  Height := 280;
  FItems := TERDKeyValueItems.Create(Self);
  FColumnCount := 1;
  FKeyWidth := 150;
  FShowCategories := True;
  FCopyOnHover := True;
  FHotIndex := -1;
end;

destructor TERDKeyValueList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TERDKeyValueList.BeginUpdate;
begin
  FItems.BeginUpdate;
end;

procedure TERDKeyValueList.EndUpdate;
begin
  FItems.EndUpdate;
  ItemsChanged;
end;

function TERDKeyValueList.Add(const ACategory, AKey, AValue: string;
  AKind: TERDKeyValueKind): TERDKeyValueItem;
begin
  Result := FItems.Add;
  Result.Category := ACategory;
  Result.Key := AKey;
  Result.Value := AValue;
  Result.Kind := AKind;
  ItemsChanged;
end;

procedure TERDKeyValueList.SetValue(const AKey, AValue: string);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].Key, AKey) then
    begin
      FItems[I].Value := AValue;
      Exit;
    end;
  Add('', AKey, AValue);
end;

procedure TERDKeyValueList.Clear;
begin
  FItems.Clear;
  FHotIndex := -1;
  FScrollY := 0;
  Invalidate;
end;

procedure TERDKeyValueList.ItemsChanged;
begin
  if FHotIndex >= FItems.Count then
    FHotIndex := -1;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TERDKeyValueList.SetItems(AValue: TERDKeyValueItems);
begin
  FItems.Assign(AValue);
end;

procedure TERDKeyValueList.SetColumnCount(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 2);
  if FColumnCount = AValue then
    Exit;
  FColumnCount := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TERDKeyValueList.SetKeyWidth(AValue: Integer);
begin
  AValue := System.Math.Max(48, AValue);
  if FKeyWidth = AValue then
    Exit;
  FKeyWidth := AValue;
  Invalidate;
end;

procedure TERDKeyValueList.SetShowCategories(AValue: Boolean);
begin
  if FShowCategories = AValue then
    Exit;
  FShowCategories := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TERDKeyValueList.SetCopyOnHover(AValue: Boolean);
begin
  if FCopyOnHover = AValue then
    Exit;
  FCopyOnHover := AValue;
  Invalidate;
end;

procedure TERDKeyValueList.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap = AValue then
    Exit;
  FWordWrap := AValue;
  SetScrollY(FScrollY);
  Invalidate;
end;

procedure TERDKeyValueList.SetHotIndex(AValue: Integer);
begin
  if (AValue < -1) or (AValue >= FItems.Count) then
    AValue := -1;
  if FHotIndex = AValue then
    Exit;
  FHotIndex := AValue;
  Invalidate;
end;

procedure TERDKeyValueList.SetCopyHot(AValue: Boolean);
begin
  if FCopyHot = AValue then
    Exit;
  FCopyHot := AValue;
  Invalidate;
end;

procedure TERDKeyValueList.SetScrollY(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollY = AValue then
    Exit;
  FScrollY := AValue;
  Invalidate;
end;

procedure TERDKeyValueList.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

function TERDKeyValueList.BarWidth: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(16)
  else
    Result := ScaleValue(10);
end;

function TERDKeyValueList.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.CompactRow);
end;

function TERDKeyValueList.LineHeight: Integer;
begin
  Result := ScaleValue(18);
end;

function TERDKeyValueList.CategoryHeight: Integer;
begin
  Result := ScaleValue(30);
end;

function TERDKeyValueList.ContentRectNoBar: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TERDKeyValueList.ContentRectWithBar(AWithBar: Boolean): TRect;
begin
  Result := ContentRectNoBar;
  if AWithBar then
    Result.Right := System.Math.Max(Result.Left, Result.Right - BarWidth);
end;

function TERDKeyValueList.NeedsScrollBar: Boolean;
var
  H: Integer;
  Entries: TVisualEntries;
begin
  BuildLayout(False, Entries, H);
  Result := H > ContentRectNoBar.Height;
end;

function TERDKeyValueList.ScrollBarRect: TRect;
var
  R: TRect;
begin
  if not NeedsScrollBar then
    Exit(Rect(0, 0, 0, 0));
  R := ContentRectNoBar;
  Result := Rect(R.Right - BarWidth, R.Top, R.Right, R.Bottom);
end;

function TERDKeyValueList.ScrollThumbRect: TRect;
begin
  Result := ERDScrollThumbRect(ScrollBarRect, True, 0,
    System.Math.Max(0, ContentHeight - 1), ContentRectWithBar(True).Height,
    FScrollY, FBarHot, FBarDragging);
end;

function TERDKeyValueList.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - ContentRectWithBar(
    NeedsScrollBar).Height);
end;

function TERDKeyValueList.ValueLineCount(const AValue: string;
  AValueWidth: Integer): Integer;
var
  S: string;
  I: Integer;
begin
  S := StringReplace(AValue, #13#10, #10, [rfReplaceAll]);
  S := StringReplace(S, #13, #10, [rfReplaceAll]);
  Result := 1;
  for I := 1 to Length(S) do
    if S[I] = #10 then
      Inc(Result);
  if FWordWrap and (AValueWidth > 0) then
    Result := System.Math.Max(Result, (Length(S) * ScaleValue(7) +
      AValueWidth - 1) div AValueWidth);
end;

function TERDKeyValueList.RowHeightFor(AIndex, AValueWidth: Integer): Integer;
begin
  Result := RowHeight + (ValueLineCount(FItems[AIndex].Value, AValueWidth) - 1) *
    LineHeight;
end;

procedure TERDKeyValueList.AddCategoryName(var ACategories: TStringArray;
  const ACategory: string);
var
  I, N: Integer;
begin
  for I := 0 to High(ACategories) do
    if SameText(ACategories[I], ACategory) then
      Exit;
  N := Length(ACategories);
  SetLength(ACategories, N + 1);
  ACategories[N] := ACategory;
end;

function TERDKeyValueList.CategoryRowCount(const ACategory: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].Category, ACategory) then
      Inc(Result);
end;

procedure TERDKeyValueList.BuildLayout(AWithBar: Boolean;
  out AEntries: TVisualEntries; out AHeight: Integer);
var
  Categories: TStringArray;
  CR, R, VR, CopyR: TRect;
  C, I, N, Pad, Gap, Cols, ColW, KeyW, ValueW, Y, X, RowY, RowMax, RH: Integer;
  Category: string;

  procedure AddEntry(AKind: TVisualKind; AIndex: Integer; const ARect,
    AValueRect, ACopyRect: TRect; const AText: string);
  var
    L: Integer;
  begin
    L := Length(AEntries);
    SetLength(AEntries, L + 1);
    AEntries[L].Kind := AKind;
    AEntries[L].Index := AIndex;
    AEntries[L].Rect := ARect;
    AEntries[L].ValueRect := AValueRect;
    AEntries[L].CopyRect := ACopyRect;
    AEntries[L].Text := AText;
  end;

begin
  SetLength(AEntries, 0);
  SetLength(Categories, 0);
  AddCategoryName(Categories, '');
  for I := 0 to FItems.Count - 1 do
    if FItems[I].Category <> '' then
      AddCategoryName(Categories, FItems[I].Category);

  CR := ContentRectWithBar(AWithBar);
  Pad := ScaleValue(8);
  Gap := ScaleValue(24);
  Cols := FColumnCount;
  ColW := System.Math.Max(1, (CR.Width - 2 * Pad - (Cols - 1) * Gap) div Cols);
  KeyW := System.Math.Min(ScaleValue(FKeyWidth), Round(ColW * 0.42));
  Y := CR.Top + Pad;
  for C := 0 to High(Categories) do
  begin
    Category := Categories[C];
    if CategoryRowCount(Category) = 0 then
      Continue;
    if FShowCategories and (Category <> '') then
    begin
      R := Rect(CR.Left + Pad, Y, CR.Right - Pad, Y + CategoryHeight);
      AddEntry(kvvCategory, -1, R, Rect(0, 0, 0, 0), Rect(0, 0, 0, 0),
        AnsiUpperCase(Category));
      Inc(Y, CategoryHeight);
    end;
    N := 0;
    RowY := Y;
    RowMax := 0;
    for I := 0 to FItems.Count - 1 do
      if SameText(FItems[I].Category, Category) then
      begin
        if (Cols = 1) or (N mod Cols = 0) then
        begin
          if (Cols > 1) and (N > 0) then
          begin
            Inc(Y, RowMax);
            RowY := Y;
            RowMax := 0;
          end;
        end;
        X := CR.Left + Pad + (N mod Cols) * (ColW + Gap);
        ValueW := ColW - KeyW - ScaleValue(4);
        if FCopyOnHover then
          Dec(ValueW, ScaleValue(24));
        RH := RowHeightFor(I, ValueW);
        R := Rect(X, RowY, X + ColW, RowY + RH);
        VR := Rect(X + KeyW, RowY, X + ColW, RowY + RH);
        CopyR := Rect(X + ColW - ScaleValue(24), RowY,
          X + ColW, RowY + RowHeight);
        AddEntry(kvvRow, I, R, VR, CopyR, '');
        if Cols = 1 then
          Inc(Y, RH)
        else
          RowMax := System.Math.Max(RowMax, RH);
        Inc(N);
      end;
    if Cols > 1 then
      Inc(Y, RowMax);
  end;
  AHeight := System.Math.Max(0, Y - CR.Top + Pad);
end;

function TERDKeyValueList.ContentHeight: Integer;
var
  Entries: TVisualEntries;
begin
  BuildLayout(NeedsScrollBar, Entries, Result);
end;

function TERDKeyValueList.RowAt(X, Y: Integer): Integer;
var
  Entries: TVisualEntries;
  I, H: Integer;
  P: TPoint;
begin
  Result := -1;
  BuildLayout(NeedsScrollBar, Entries, H);
  P := Point(X, Y);
  for I := 0 to High(Entries) do
    if Entries[I].Kind = kvvRow then
    begin
      OffsetRect(Entries[I].Rect, 0, -FScrollY);
      if PtInRect(Entries[I].Rect, P) then
        Exit(Entries[I].Index);
    end;
end;

procedure TERDKeyValueList.CopyRow(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    Exit;
  Clipboard.AsText := FItems[AIndex].Value;
  if Assigned(FOnCopy) then
    FOnCopy(Self, AIndex);
end;

procedure TERDKeyValueList.DrawValue(APainter: TERDPainter;
  AItem: TERDKeyValueItem; const ARect: TRect; ACopySpace: Integer);
var
  Lines: TStringList;
  S: string;
  I, Y, MaxW, ChipW: Integer;
  Ink: TColor;
  Weight: TERDTextWeight;
begin
  MaxW := System.Math.Max(1, ARect.Width - ACopySpace);
  if AItem.Kind = kvkChip then
  begin
    ChipW := APainter.ChipWidth(AItem.Value, False, ERD_TEXT_CAPS);
    APainter.Chip(ARect.Left, ARect.Top + (RowHeight - ScaleValue(20)) div 2,
      AItem.Value, APainter.StatusColor(AItem.Tone), False, False,
      ERD_TEXT_CAPS);
    if ChipW > MaxW then
      APainter.Text(ARect.Left, ARect.Top + RowHeight div 2, AItem.Value,
        ERD_TEXT_SMALL, APainter.StatusColor(AItem.Tone), twSemibold,
        taLeftJustify, MaxW);
    Exit;
  end;

  case AItem.Kind of
    kvkMono:
      begin
        Ink := Palette.ForegroundText;
        Weight := twMono;
      end;
    kvkLink:
      begin
        Ink := APainter.AccentText;
        Weight := twSemibold;
      end;
    kvkTone:
      begin
        Ink := APainter.StatusColor(AItem.Tone);
        Weight := twSemibold;
      end;
  else
    Ink := Palette.ForegroundText;
    Weight := twSemibold;
  end;

  if FWordWrap and (Pos(#10, AItem.Value) = 0) and
    (Pos(#13, AItem.Value) = 0) then
  begin
    APainter.WrapText(Rect(ARect.Left, ARect.Top + ScaleValue(5),
      ARect.Left + MaxW, ARect.Bottom), AItem.Value, ERD_TEXT_BODY, Ink,
      Weight);
    Exit;
  end;

  Lines := TStringList.Create;
  try
    S := StringReplace(AItem.Value, #13#10, #10, [rfReplaceAll]);
    S := StringReplace(S, #13, #10, [rfReplaceAll]);
    ExtractStrings([#10], [], PChar(S), Lines);
    if Lines.Count = 0 then
      Lines.Add('');
    for I := 0 to Lines.Count - 1 do
    begin
      Y := ARect.Top + RowHeight div 2 + I * LineHeight;
      APainter.Text(ARect.Left, Y, Lines[I], ERD_TEXT_BODY, Ink, Weight,
        taLeftJustify, MaxW);
    end;
  finally
    Lines.Free;
  end;
end;

procedure TERDKeyValueList.DrawPreview(APainter: TERDPainter);
var
  R: TRect;
  X, Y, KeyW, RowH: Integer;
begin
  X := ScaleValue(16);
  Y := ScaleValue(12);
  KeyW := ScaleValue(FKeyWidth);
  RowH := RowHeight;
  APainter.Caps(X, Y + CategoryHeight div 2, 'IDENTIFICATION');
  APainter.HLine(X + ScaleValue(120), Y + CategoryHeight div 2,
    Width - X - ScaleValue(136), Palette.NeutralLight);
  Inc(Y, CategoryHeight);
  R := Rect(X, Y, Width - X, Y + RowH);
  APainter.Text(R.Left, R.Top + RowH div 2, 'Part number', ERD_TEXT_BODY,
    Palette.GaugeLabel, twRegular, taLeftJustify, KeyW - ScaleValue(8));
  APainter.Text(R.Left + KeyW, R.Top + RowH div 2, '5Q0937084CK',
    ERD_TEXT_BODY, Palette.ForegroundText, twMono, taLeftJustify,
    R.Width - KeyW - ScaleValue(24));
  APainter.Glyph(glCopy, R.Right - ScaleValue(18), R.Top + RowH div 2,
    Palette.Subtle, 1);
  OffsetRect(R, 0, RowH);
  APainter.Text(R.Left, R.Top + RowH div 2, 'Software', ERD_TEXT_BODY,
    Palette.GaugeLabel, twRegular, taLeftJustify, KeyW - ScaleValue(8));
  APainter.Text(R.Left + KeyW, R.Top + RowH div 2, '0254 - BCM MQB',
    ERD_TEXT_BODY, Palette.ForegroundText, twSemibold, taLeftJustify,
    R.Width - KeyW - ScaleValue(8));
  OffsetRect(R, 0, RowH);
  APainter.Text(R.Left, R.Top + RowH div 2, 'Fault memory', ERD_TEXT_BODY,
    Palette.GaugeLabel, twRegular, taLeftJustify, KeyW - ScaleValue(8));
  APainter.Chip(R.Left + KeyW, R.Top + (RowH - ScaleValue(20)) div 2,
    '1 FAULT', Palette.Danger, False, False, ERD_TEXT_CAPS);
end;

procedure TERDKeyValueList.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  Entries: TVisualEntries;
  I, H, Save, CopySpace, LineX: Integer;
  R, CR: TRect;
  Item: TERDKeyValueItem;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(Rect(0, 0, Width, Height), Palette.GaugeFace);
    if (FItems.Count = 0) and IsPreview then
      DrawPreview(P)
    else
    begin
      CR := ContentRectWithBar(NeedsScrollBar);
      BuildLayout(NeedsScrollBar, Entries, H);
      Save := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, CR.Left, CR.Top, CR.Right,
          CR.Bottom);
        for I := 0 to High(Entries) do
        begin
          R := Entries[I].Rect;
          OffsetRect(R, 0, -FScrollY);
          if (R.Bottom < CR.Top) or (R.Top > CR.Bottom) then
            Continue;
          if Entries[I].Kind = kvvCategory then
          begin
            P.Caps(R.Left, (R.Top + R.Bottom) div 2, Entries[I].Text);
            LineX := R.Left + P.TextWidth(Entries[I].Text, ERD_TEXT_CAPS,
              twSemibold) + ScaleValue(10);
            P.HLine(LineX, (R.Top + R.Bottom) div 2,
              System.Math.Max(0, R.Right - LineX), Palette.NeutralLight);
          end
          else
          begin
            Item := FItems[Entries[I].Index];
            if Entries[I].Index = FHotIndex then
              P.FillRect(R, ERDMixColor(Palette.ForegroundText,
                Palette.GaugeFace, 0.05));
            P.Text(R.Left, R.Top + RowHeight div 2, Item.Key, ERD_TEXT_BODY,
              Palette.GaugeLabel, twRegular, taLeftJustify,
              Entries[I].ValueRect.Left - R.Left - ScaleValue(8));
            CopySpace := 0;
            if FCopyOnHover and (Entries[I].Index = FHotIndex) then
              CopySpace := ScaleValue(24);
            Entries[I].ValueRect.Offset(0, -FScrollY);
            DrawValue(P, Item, Entries[I].ValueRect, CopySpace);
            if FCopyOnHover and (Entries[I].Index = FHotIndex) then
            begin
              Entries[I].CopyRect.Offset(0, -FScrollY);
              P.Glyph(glCopy, (Entries[I].CopyRect.Left + Entries[I].CopyRect.Right) / 2,
                (Entries[I].CopyRect.Top + Entries[I].CopyRect.Bottom) / 2,
                Palette.Subtle, 1);
            end;
          end;
        end;
      finally
        RestoreDC(ACanvas.Handle, Save);
      end;
    end;
    if NeedsScrollBar then
      ERDPaintScrollBar(P, ScrollBarRect, True, 0,
        System.Math.Max(0, ContentHeight - 1), ContentRectWithBar(True).Height,
        FScrollY, FBarHot, FBarDragging);
  finally
    P.Free;
  end;
end;

procedure TERDKeyValueList.Resize;
begin
  inherited;
  SetScrollY(FScrollY);
end;

procedure TERDKeyValueList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Entries: TVisualEntries;
  I, H, Index: Integer;
  Thumb: TRect;
  Pnt: TPoint;
begin
  inherited;
  if (Button = mbLeft) and NeedsScrollBar and PtInRect(ScrollBarRect,
    Point(X, Y)) then
  begin
    Thumb := ScrollThumbRect;
    if (Y >= Thumb.Top) and (Y < Thumb.Bottom) then
    begin
      FBarDragging := True;
      FBarDragOffset := Y - Thumb.Top;
      MouseCapture := True;
      Invalidate;
    end
    else if Y < Thumb.Top then
      SetScrollY(FScrollY - ContentRectWithBar(True).Height)
    else
      SetScrollY(FScrollY + ContentRectWithBar(True).Height);
    Exit;
  end;
  Index := RowAt(X, Y);
  if (Button <> mbLeft) or (Index < 0) then
    Exit;
  BuildLayout(NeedsScrollBar, Entries, H);
  Pnt := Point(X, Y);
  for I := 0 to High(Entries) do
    if (Entries[I].Kind = kvvRow) and (Entries[I].Index = Index) then
    begin
      Entries[I].CopyRect.Offset(0, -FScrollY);
      Entries[I].ValueRect.Offset(0, -FScrollY);
      if FCopyOnHover and PtInRect(Entries[I].CopyRect, Pnt) then
        CopyRow(Index)
      else if (FItems[Index].Kind = kvkLink) and PtInRect(Entries[I].ValueRect,
        Pnt) and Assigned(FOnLinkClick) then
        FOnLinkClick(Self, Index);
      Break;
    end;
end;

procedure TERDKeyValueList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Entries: TVisualEntries;
  I, H, Index: Integer;
  Bar, Thumb: TRect;
  Travel: Integer;
  Pnt: TPoint;
  CopyNow, LinkNow: Boolean;
begin
  inherited;
  if FBarDragging then
  begin
    Bar := ScrollBarRect;
    Thumb := ScrollThumbRect;
    Travel := Bar.Height - Thumb.Height;
    if Travel > 0 then
      SetScrollY(Round((Y - FBarDragOffset - Bar.Top) / Travel * MaxScroll));
    Exit;
  end;
  SetBarHot(NeedsScrollBar and PtInRect(ScrollBarRect, Point(X, Y)));
  Index := RowAt(X, Y);
  SetHotIndex(Index);
  CopyNow := False;
  LinkNow := False;
  if Index >= 0 then
  begin
    BuildLayout(NeedsScrollBar, Entries, H);
    Pnt := Point(X, Y);
    for I := 0 to High(Entries) do
      if (Entries[I].Kind = kvvRow) and (Entries[I].Index = Index) then
      begin
        Entries[I].CopyRect.Offset(0, -FScrollY);
        Entries[I].ValueRect.Offset(0, -FScrollY);
        CopyNow := FCopyOnHover and PtInRect(Entries[I].CopyRect, Pnt);
        LinkNow := (FItems[Index].Kind = kvkLink) and PtInRect(Entries[I].ValueRect,
          Pnt);
        Break;
      end;
  end;
  SetCopyHot(CopyNow);
  if LinkNow or CopyNow then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TERDKeyValueList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FBarDragging then
  begin
    FBarDragging := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TERDKeyValueList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetHotIndex(-1);
  SetCopyHot(False);
  SetBarHot(False);
  Cursor := crDefault;
end;

procedure TERDKeyValueList.CMMouseWheel(var Message: TCMMouseWheel);
begin
  SetScrollY(FScrollY - Sign(Message.WheelDelta) * RowHeight * 3);
  Message.Result := 1;
end;

end.
