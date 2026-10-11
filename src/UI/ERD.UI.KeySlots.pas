//------------------------------------------------------------------------------
//  ERD.UI.KeySlots
//
//  TOBDKeySlots - OBD Studio Pro immobilizer key-slot grid with slot state,
//  selection and click notifications.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.KeySlots;

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
  ERD.UI.Paint;

type
  /// <summary>State of one immobilizer key slot.</summary>
  TOBDKeySlotState = (
    /// <summary>The slot contains a learned key.</summary>
    kssLearned,
    /// <summary>The slot contains the key currently being learned.</summary>
    kssThisKey,
    /// <summary>The slot is available and empty.</summary>
    kssEmpty,
    /// <summary>The slot is waiting for the transponder during learning.</summary>
    kssLearning,
    /// <summary>The slot cannot be used in this procedure.</summary>
    kssDisabled);

  /// <summary>Fires when a key slot is clicked.</summary>
  /// <param name="Sender">The key-slot grid.</param>
  /// <param name="AIndex">Clicked slot index.</param>
  TOBDKeySlotClickEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  TOBDKeySlots = class;

  /// <summary>One key slot in the grid.</summary>
  TOBDKeySlotItem = class(TCollectionItem)
  strict private
    FSlotLabel: string;
    FKeyId: string;
    FState: TOBDKeySlotState;
    procedure SetSlotLabel(const AValue: string);
    procedure SetKeyId(const AValue: string);
    procedure SetState(AValue: TOBDKeySlotState);
  protected
    /// <summary>Returns the slot label in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an empty key slot.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another slot item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Slot label, for example Key 1.</summary>
    property SlotLabel: string read FSlotLabel write SetSlotLabel;
    /// <summary>Key identifier or mechanic-facing status text.</summary>
    property KeyId: string read FKeyId write SetKeyId;
    /// <summary>Slot state that drives colour and glyphs.</summary>
    property State: TOBDKeySlotState read FState write SetState default kssEmpty;
  end;

  /// <summary>Streamable key-slot collection.</summary>
  TOBDKeySlotCollection = class(TOwnedCollection)
  strict private
    function GetItem(Index: Integer): TOBDKeySlotItem;
    procedure SetItem(Index: Integer; AValue: TOBDKeySlotItem);
  protected
    /// <summary>Notifies the owning grid when an item changes.</summary>
    /// <param name="Item">Changed item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a collection of key-slot items.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a slot item.</summary>
    /// <returns>The new slot.</returns>
    function Add: TOBDKeySlotItem;
    /// <summary>Copies another key-slot collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed slot indexer.</summary>
    property Items[Index: Integer]: TOBDKeySlotItem read GetItem write SetItem;
      default;
  end;

  /// <summary>Grid of immobilizer key slots.</summary>
  TOBDKeySlots = class(TOBDCustomControl, IOBDSurface)
  strict private
    FSlots: TOBDKeySlotCollection;
    FCols: Integer;
    FItemIndex: Integer;
    FHoverIndex: Integer;
    FOnSlotClick: TOBDKeySlotClickEvent;
    procedure SetSlots(AValue: TOBDKeySlotCollection);
    procedure SetCols(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    function UsePreviewData: Boolean;
    function VisualCount: Integer;
    function VisualLabel(AIndex: Integer): string;
    function VisualKeyId(AIndex: Integer): string;
    function VisualState(AIndex: Integer): TOBDKeySlotState;
    function SlotRect(AIndex: Integer): TRect;
    function HitSlot(X, Y: Integer): Integer;
    function StateColor(APainter: TOBDPainter; AState: TOBDKeySlotState): TColor;
    procedure SlotsChanged;
    procedure DrawSlot(APainter: TOBDPainter; AIndex: Integer; const R: TRect);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the key-slot grid.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks hovered slot.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Selects and clicks a slot.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Handles arrow-key navigation.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Enables arrow-key input.</summary>
    /// <param name="Message">Dialog-code message.</param>
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    /// <summary>Creates a key-slot grid.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the slot collection.</summary>
    destructor Destroy; override;
    /// <summary>Returns the card face colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Removes every slot.</summary>
    procedure Clear;
    /// <summary>Adds a slot.</summary>
    /// <param name="ALabel">Slot label.</param>
    /// <param name="AKeyId">Key id or status text.</param>
    /// <param name="AState">Slot state.</param>
    /// <returns>The new slot.</returns>
    function AddSlot(const ALabel, AKeyId: string;
      AState: TOBDKeySlotState): TOBDKeySlotItem;
    /// <summary>Number of stored slots.</summary>
    /// <returns>Slot count.</returns>
    function Count: Integer;
  published
    /// <summary>Streamed key slots.</summary>
    property Slots: TOBDKeySlotCollection read FSlots write SetSlots;
    /// <summary>Number of columns in the grid.</summary>
    property Cols: Integer read FCols write SetCols default 4;
    /// <summary>Selected slot index, or -1.</summary>
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    /// <summary>Desktop or tablet row metrics.</summary>
    property Density;
    /// <summary>Takes density from the assigned or inherited theme.</summary>
    property ParentDensity;
    /// <summary>Fires when the user clicks a slot.</summary>
    property OnSlotClick: TOBDKeySlotClickEvent read FOnSlotClick
      write FOnSlotClick;
    /// <summary>Keyboard focus is enabled by default.</summary>
    property TabStop default True;
  end;

implementation

const
  PREVIEW_KEY_COUNT = 8;

function PreviewKeyState(AIndex: Integer): TOBDKeySlotState;
begin
  case AIndex of
    0, 1: Result := kssLearned;
    2: Result := kssLearning;
  else
    Result := kssEmpty;
  end;
end;

function PreviewKeyId(AIndex: Integer): string;
begin
  case AIndex of
    0, 1: Result := 'Learned';
    2: Result := 'Hold to the marker';
  else
    Result := 'Empty';
  end;
end;

{ TOBDKeySlotItem }

constructor TOBDKeySlotItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := kssEmpty;
  FSlotLabel := 'Key';
  FKeyId := 'Empty';
end;

procedure TOBDKeySlotItem.Assign(Source: TPersistent);
var
  Item: TOBDKeySlotItem;
begin
  if Source is TOBDKeySlotItem then
  begin
    Item := TOBDKeySlotItem(Source);
    FSlotLabel := Item.SlotLabel;
    FKeyId := Item.KeyId;
    FState := Item.State;
    Changed(False);
  end
  else
    inherited;
end;

function TOBDKeySlotItem.GetDisplayName: string;
begin
  if FSlotLabel <> '' then
    Result := FSlotLabel
  else
    Result := inherited GetDisplayName;
end;

procedure TOBDKeySlotItem.SetSlotLabel(const AValue: string);
begin
  if FSlotLabel = AValue then
    Exit;
  FSlotLabel := AValue;
  Changed(False);
end;

procedure TOBDKeySlotItem.SetKeyId(const AValue: string);
begin
  if FKeyId = AValue then
    Exit;
  FKeyId := AValue;
  Changed(False);
end;

procedure TOBDKeySlotItem.SetState(AValue: TOBDKeySlotState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

{ TOBDKeySlotCollection }

constructor TOBDKeySlotCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDKeySlotItem);
end;

function TOBDKeySlotCollection.Add: TOBDKeySlotItem;
begin
  Result := TOBDKeySlotItem(inherited Add);
end;

procedure TOBDKeySlotCollection.Assign(Source: TPersistent);
var
  Src: TOBDKeySlotCollection;
  I: Integer;
begin
  if Source is TOBDKeySlotCollection then
  begin
    BeginUpdate;
    try
      Clear;
      Src := TOBDKeySlotCollection(Source);
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

function TOBDKeySlotCollection.GetItem(Index: Integer): TOBDKeySlotItem;
begin
  Result := TOBDKeySlotItem(inherited GetItem(Index));
end;

procedure TOBDKeySlotCollection.SetItem(Index: Integer;
  AValue: TOBDKeySlotItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDKeySlotCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDKeySlots then
    TOBDKeySlots(GetOwner).SlotsChanged;
end;

{ TOBDKeySlots }

constructor TOBDKeySlots.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 520;
  Height := 152;
  TabStop := True;
  FCols := 4;
  FItemIndex := -1;
  FHoverIndex := -1;
  FSlots := TOBDKeySlotCollection.Create(Self);
end;

destructor TOBDKeySlots.Destroy;
begin
  FSlots.Free;
  inherited;
end;

function TOBDKeySlots.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDKeySlots.Clear;
begin
  FSlots.Clear;
  FItemIndex := -1;
  Invalidate;
end;

function TOBDKeySlots.AddSlot(const ALabel, AKeyId: string;
  AState: TOBDKeySlotState): TOBDKeySlotItem;
begin
  Result := FSlots.Add;
  Result.SlotLabel := ALabel;
  Result.KeyId := AKeyId;
  Result.State := AState;
  if FItemIndex < 0 then
    FItemIndex := Result.Index;
  Invalidate;
end;

function TOBDKeySlots.Count: Integer;
begin
  Result := FSlots.Count;
end;

procedure TOBDKeySlots.SetSlots(AValue: TOBDKeySlotCollection);
begin
  FSlots.Assign(AValue);
end;

procedure TOBDKeySlots.SetCols(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 12);
  if FCols = AValue then
    Exit;
  FCols := AValue;
  Invalidate;
end;

procedure TOBDKeySlots.SetItemIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, VisualCount - 1);
  if FItemIndex = AValue then
    Exit;
  FItemIndex := AValue;
  Invalidate;
end;

procedure TOBDKeySlots.SlotsChanged;
begin
  if FItemIndex >= VisualCount then
    FItemIndex := VisualCount - 1;
  Invalidate;
end;

function TOBDKeySlots.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FSlots.Count = 0);
end;

function TOBDKeySlots.VisualCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_KEY_COUNT
  else
    Result := FSlots.Count;
end;

function TOBDKeySlots.VisualLabel(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Key ' + IntToStr(AIndex + 1)
  else
    Result := FSlots[AIndex].SlotLabel;
end;

function TOBDKeySlots.VisualKeyId(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewKeyId(AIndex)
  else
    Result := FSlots[AIndex].KeyId;
end;

function TOBDKeySlots.VisualState(AIndex: Integer): TOBDKeySlotState;
begin
  if UsePreviewData then
    Result := PreviewKeyState(AIndex)
  else
    Result := FSlots[AIndex].State;
end;

function TOBDKeySlots.SlotRect(AIndex: Integer): TRect;
var
  Gap, Cols, W, H, X, Y: Integer;
begin
  Gap := ScaleValue(12);
  Cols := System.Math.Max(1, FCols);
  W := (ClientWidth - (Cols - 1) * Gap) div Cols;
  H := ScaleValue(64);
  if Density = dnTablet then
    H := ScaleValue(76);
  X := (AIndex mod Cols) * (W + Gap);
  Y := (AIndex div Cols) * (H + Gap);
  Result := Rect(X, Y, X + W, Y + H);
end;

function TOBDKeySlots.HitSlot(X, Y: Integer): Integer;
var
  I: Integer;
  R: TRect;
begin
  Result := -1;
  for I := 0 to VisualCount - 1 do
  begin
    R := SlotRect(I);
    if PtInRect(R, Point(X, Y)) then
      Exit(I);
  end;
end;

function TOBDKeySlots.StateColor(APainter: TOBDPainter;
  AState: TOBDKeySlotState): TColor;
begin
  case AState of
    kssLearned: Result := Palette.Success;
    kssThisKey, kssLearning: Result := APainter.AccentText;
    kssDisabled: Result := APainter.DisabledText;
  else
    Result := Palette.Subtle;
  end;
end;

procedure TOBDKeySlots.DrawSlot(APainter: TOBDPainter; AIndex: Integer;
  const R: TRect);
var
  Fill, Outline, Ink, LabelInk: TColor;
  State: TOBDKeySlotState;
  Mid: Integer;
begin
  State := VisualState(AIndex);
  if (State = kssLearning) or (State = kssThisKey) or (AIndex = FItemIndex) then
  begin
    Fill := APainter.Tint(Palette.Accent, 0.10);
    Outline := APainter.AccentText;
  end
  else if AIndex = FHoverIndex then
  begin
    Fill := APainter.HeaderFill;
    Outline := Palette.NeutralLight;
  end
  else
  begin
    Fill := Palette.GaugeFace;
    Outline := Palette.NeutralLight;
  end;
  APainter.RoundRect(R.Left, R.Top, R.Width, R.Height, ScaleValue(8), Fill,
    Outline, 1 + Ord((State = kssLearning) or (AIndex = FItemIndex)));
  Ink := StateColor(APainter, State);
  if State in [kssEmpty, kssDisabled] then
    LabelInk := Palette.GaugeLabel
  else
    LabelInk := Palette.ForegroundText;
  Mid := (R.Top + R.Bottom) div 2;
  APainter.Glyph(glKey, R.Left + ScaleValue(22), Mid, Ink, 1);
  APainter.Text(R.Left + ScaleValue(42), Mid - ScaleValue(9),
    VisualLabel(AIndex), OBD_TEXT_HEADING, LabelInk, twSemibold,
    taLeftJustify, R.Width - ScaleValue(64));
  APainter.Text(R.Left + ScaleValue(42), Mid + ScaleValue(10),
    VisualKeyId(AIndex), OBD_TEXT_SMALL, Ink, twSemibold, taLeftJustify,
    R.Width - ScaleValue(54));
  if State = kssLearned then
    APainter.GlyphCheck(R.Right - ScaleValue(16), R.Top + ScaleValue(14),
      Palette.Success, 0.6)
  else if (State = kssLearning) or (State = kssThisKey) then
    APainter.GlyphPending(R.Right - ScaleValue(16), R.Top + ScaleValue(14),
      APainter.AccentText, 0.65);
end;

procedure TOBDKeySlots.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  I: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.Background);
    if VisualCount = 0 then
    begin
      P.Card(ClientRect);
      P.Text(ClientWidth div 2, ClientHeight div 2, 'No key slots',
        OBD_TEXT_HEADING, Palette.GaugeLabel, twSemibold, taCenter);
      Exit;
    end;
    for I := 0 to VisualCount - 1 do
      DrawSlot(P, I, SlotRect(I));
  finally
    P.Free;
  end;
end;

procedure TOBDKeySlots.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  H: Integer;
begin
  inherited;
  H := HitSlot(X, Y);
  if FHoverIndex <> H then
  begin
    FHoverIndex := H;
    Invalidate;
  end;
end;

procedure TOBDKeySlots.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  H: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  SetFocus;
  H := HitSlot(X, Y);
  if H >= 0 then
  begin
    ItemIndex := H;
    if Assigned(FOnSlotClick) then
      FOnSlotClick(Self, H);
  end;
end;

procedure TOBDKeySlots.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT:
      begin
        ItemIndex := FItemIndex - 1;
        Key := 0;
      end;
    VK_RIGHT:
      begin
        ItemIndex := FItemIndex + 1;
        Key := 0;
      end;
    VK_UP:
      begin
        ItemIndex := FItemIndex - FCols;
        Key := 0;
      end;
    VK_DOWN:
      begin
        ItemIndex := FItemIndex + FCols;
        Key := 0;
      end;
  end;
end;

procedure TOBDKeySlots.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TOBDKeySlots.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
