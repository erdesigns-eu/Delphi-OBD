//------------------------------------------------------------------------------
//  ERD.UI.HexView
//
//  TOBDHexView - card-style hexadecimal memory viewer for dumps, backups and
//  ECU data blocks with regions, compare highlights, selection and scrolling.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit ERD.UI.HexView;

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
  /// <summary>Colour category used to tint a named byte range.</summary>
  TOBDHexRegionKind = (
    /// <summary>Neutral range such as padding or unused EEPROM.</summary>
    hvrNeutral,
    /// <summary>Accent range such as coding or a radio-code block.</summary>
    hvrAccent,
    /// <summary>Successful range such as a verified checksum.</summary>
    hvrSuccess,
    /// <summary>Warning range that deserves attention.</summary>
    hvrWarning,
    /// <summary>Danger range such as a corrupt or protected block.</summary>
    hvrDanger);

  /// <summary>Byte selection in the hex view.</summary>
  TOBDHexSelection = record
    /// <summary>First selected byte offset.</summary>
    Offset: Integer;
    /// <summary>Number of selected bytes.</summary>
    Length: Integer;
  end;

  /// <summary>Event fired when the selection changes.</summary>
  TOBDHexSelectionEvent = procedure(Sender: TObject;
    const ASelection: TOBDHexSelection) of object;

  /// <summary>Named range painted over the memory dump.</summary>
  TOBDHexRegionItem = class(TCollectionItem)
  strict private
    FName: string;
    FOffset: Integer;
    FLength: Integer;
    FKind: TOBDHexRegionKind;
    procedure SetName(const AValue: string);
    procedure SetOffset(AValue: Integer);
    procedure SetLength(AValue: Integer);
    procedure SetKind(AValue: TOBDHexRegionKind);
  protected
    /// <summary>Returns the range name in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an accent region.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>True when this region contains the byte offset.</summary>
    /// <param name="AOffset">Byte offset.</param>
    /// <returns>Containment flag.</returns>
    function Contains(AOffset: Integer): Boolean;
  published
    /// <summary>Name shown in the legend and row annotation.</summary>
    property Name: string read FName write SetName;
    /// <summary>First byte offset of the region.</summary>
    property Offset: Integer read FOffset write SetOffset default 0;
    /// <summary>Number of bytes in the region.</summary>
    property Length: Integer read FLength write SetLength default 0;
    /// <summary>Theme colour category for this range.</summary>
    property Kind: TOBDHexRegionKind read FKind write SetKind default hvrAccent;
  end;

  /// <summary>Collection of named byte ranges.</summary>
  TOBDHexRegionCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDHexRegionItem;
    procedure SetItem(Index: Integer; AValue: TOBDHexRegionItem);
  protected
    /// <summary>Notifies the owner after an item changes.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a region collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a region item.</summary>
    /// <returns>New region item.</returns>
    function Add: TOBDHexRegionItem;
    /// <summary>Finds the first region that contains a byte offset.</summary>
    /// <param name="AOffset">Byte offset.</param>
    /// <returns>Region item or nil.</returns>
    function RegionAt(AOffset: Integer): TOBDHexRegionItem;
    /// <summary>Indexed region items.</summary>
    property Items[Index: Integer]: TOBDHexRegionItem read GetItem
      write SetItem; default;
    /// <summary>Fires when a region changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Painted hexadecimal memory viewer.</summary>
  TOBDHexView = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FData: TBytes;
    FCompareData: TBytes;
    FRegions: TOBDHexRegionCollection;
    FBaseAddress: Int64;
    FBytesPerRow: Integer;
    FSelection: TOBDHexSelection;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FOnSelectionChange: TOBDHexSelectionEvent;
    procedure SetTitle(const AValue: string);
    procedure SetCompareData(const AValue: TBytes);
    procedure SetBaseAddress(AValue: Int64);
    procedure SetBytesPerRow(AValue: Integer);
    procedure SetSelection(const AValue: TOBDHexSelection);
    procedure SetRegions(AValue: TOBDHexRegionCollection);
    procedure RegionsChanged(Sender: TObject);
    procedure SetScrollPos(AValue: Integer);
    procedure DoSelectionChange;
    function HeaderHeight: Integer;
    function ToolHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function BodyRect: TRect;
    function ScrollTrackRect: TRect;
    function ContentHeight: Integer;
    function MaxScroll: Integer;
    function VisibleRowCount: Integer;
    function RowCount: Integer;
    function DataLength: Integer;
    function UsePreviewData: Boolean;
    function ByteValue(AOffset: Integer): Byte;
    function CompareByteValue(AOffset: Integer; out AValue: Byte): Boolean;
    function IsChanged(AOffset: Integer): Boolean;
    function IsSelected(AOffset: Integer): Boolean;
    function RegionColor(APainter: TOBDPainter; AKind: TOBDHexRegionKind)
      : TColor;
    function ScrollThumbRect: TRect;
    function HitByte(X, Y: Integer; out AOffset: Integer): Boolean;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawToolbar(APainter: TOBDPainter);
    procedure DrawColumns(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScroll(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the hex view card.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles byte selection and scroll-bar paging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Updates scroll-bar hover and thumb dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends a scroll-bar drag.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Scrolls the byte rows.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Screen mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Moves the caret and selection with the keyboard.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a 720 x 360 hex view.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the region collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints when density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Copies bytes into the view.</summary>
    /// <param name="AData">Bytes to display.</param>
    procedure SetData(const AData: TBytes);
    /// <summary>Loads all bytes from a stream.</summary>
    /// <param name="AStream">Stream positioned at the first byte.</param>
    procedure LoadFromStream(AStream: TStream);
    /// <summary>Clears data, compare data, regions and selection.</summary>
    procedure Clear;
    /// <summary>Adds a named coloured region.</summary>
    /// <param name="AName">Legend name.</param>
    /// <param name="AOffset">First byte offset.</param>
    /// <param name="ALength">Byte count.</param>
    /// <param name="AKind">Colour kind.</param>
    /// <returns>Created region item.</returns>
    function AddRegion(const AName: string; AOffset, ALength: Integer;
      AKind: TOBDHexRegionKind): TOBDHexRegionItem;
    /// <summary>Compares one byte with the compare buffer.</summary>
    /// <param name="AOffset">Byte offset.</param>
    /// <returns>True when the byte differs.</returns>
    function ByteChanged(AOffset: Integer): Boolean;
    /// <summary>Number of bytes that differ from the compare buffer.</summary>
    /// <returns>Changed byte count.</returns>
    function ChangedCount: Integer;
    /// <summary>Displayed byte data.</summary>
    property Data: TBytes read FData write SetData;
    /// <summary>Optional compare buffer.</summary>
    property CompareData: TBytes read FCompareData write SetCompareData;
    /// <summary>Current byte selection.</summary>
    property Selection: TOBDHexSelection read FSelection write SetSelection;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Address added to the offset column.</summary>
    property BaseAddress: Int64 read FBaseAddress write SetBaseAddress;
    /// <summary>Number of bytes shown per row.</summary>
    property BytesPerRow: Integer read FBytesPerRow write SetBytesPerRow
      default 16;
    /// <summary>Named coloured byte ranges.</summary>
    property Regions: TOBDHexRegionCollection read FRegions write SetRegions;
    /// <summary>Desktop or tablet row sizes.</summary>
    property Density;
    /// <summary>Takes density from the current theme.</summary>
    property ParentDensity;
    /// <summary>Fires after the byte selection changes.</summary>
    property OnSelectionChange: TOBDHexSelectionEvent read FOnSelectionChange
      write FOnSelectionChange;
  end;

implementation

const
  HEX_TEXT = OBD_TEXT_BODY;
  HEX_META = OBD_TEXT_SMALL;

function SameBytes(const A, B: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(A) = Length(B);
  if not Result then
    Exit;
  for I := 0 to Length(A) - 1 do
    if A[I] <> B[I] then
      Exit(False);
end;

function HexPreviewByte(AOffset: Integer): Byte;
var
  V: Integer;
begin
  V := (AOffset * 73 + 41) and $FF;
  Result := Byte(V);
  if AOffset < 14 then
    Result := Byte(Ord('0') + (AOffset mod 10));
end;

{ TOBDHexRegionItem }

constructor TOBDHexRegionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FKind := hvrAccent;
end;

function TOBDHexRegionItem.Contains(AOffset: Integer): Boolean;
begin
  Result := (FLength > 0) and (AOffset >= FOffset) and
    (Int64(AOffset) < Int64(FOffset) + FLength);
end;

function TOBDHexRegionItem.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDHexRegionItem.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  Changed(False);
end;

procedure TOBDHexRegionItem.SetOffset(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FOffset = AValue then
    Exit;
  FOffset := AValue;
  Changed(False);
end;

procedure TOBDHexRegionItem.SetLength(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FLength = AValue then
    Exit;
  FLength := AValue;
  Changed(False);
end;

procedure TOBDHexRegionItem.SetKind(AValue: TOBDHexRegionKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

{ TOBDHexRegionCollection }

constructor TOBDHexRegionCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDHexRegionItem);
end;

function TOBDHexRegionCollection.Add: TOBDHexRegionItem;
begin
  Result := TOBDHexRegionItem(inherited Add);
end;

function TOBDHexRegionCollection.GetItem(Index: Integer): TOBDHexRegionItem;
begin
  Result := TOBDHexRegionItem(inherited GetItem(Index));
end;

function TOBDHexRegionCollection.RegionAt(AOffset: Integer): TOBDHexRegionItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Contains(AOffset) then
      Exit(Items[I]);
end;

procedure TOBDHexRegionCollection.SetItem(Index: Integer;
  AValue: TOBDHexRegionItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDHexRegionCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDHexView }

constructor TOBDHexView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'EEPROM dump';
  FBytesPerRow := 16;
  FSelection.Offset := -1;
  FSelection.Length := 0;
  FRegions := TOBDHexRegionCollection.Create(Self);
  FRegions.OnChange := RegionsChanged;
  Width := 720;
  Height := 360;
  TabStop := True;
end;

destructor TOBDHexView.Destroy;
begin
  FRegions.Free;
  inherited;
end;

procedure TOBDHexView.DensityChanged;
begin
  inherited DensityChanged;
  SetScrollPos(FScrollPos);
end;

function TOBDHexView.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
  if StyleBackground <> clDefault then
    Result := StyleBackground;
end;

procedure TOBDHexView.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDHexView.SetData(const AData: TBytes);
begin
  if SameBytes(FData, AData) then
    Exit;
  FData := Copy(AData, 0, Length(AData));
  SetScrollPos(FScrollPos);
  if FSelection.Offset >= Length(FData) then
  begin
    FSelection.Offset := -1;
    FSelection.Length := 0;
    DoSelectionChange;
  end;
  Invalidate;
end;

procedure TOBDHexView.SetCompareData(const AValue: TBytes);
begin
  if SameBytes(FCompareData, AValue) then
    Exit;
  FCompareData := Copy(AValue, 0, Length(AValue));
  Invalidate;
end;

procedure TOBDHexView.SetBaseAddress(AValue: Int64);
begin
  if AValue < 0 then
    AValue := 0;
  if FBaseAddress = AValue then
    Exit;
  FBaseAddress := AValue;
  Invalidate;
end;

procedure TOBDHexView.SetBytesPerRow(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 32);
  if FBytesPerRow = AValue then
    Exit;
  FBytesPerRow := AValue;
  SetScrollPos(0);
  Invalidate;
end;

procedure TOBDHexView.SetSelection(const AValue: TOBDHexSelection);
var
  NewSel: TOBDHexSelection;
  MaxLen: Integer;
begin
  NewSel := AValue;
  if NewSel.Offset < 0 then
  begin
    NewSel.Offset := -1;
    NewSel.Length := 0;
  end
  else
  begin
    MaxLen := DataLength;
    if MaxLen > 0 then
      NewSel.Offset := EnsureRange(NewSel.Offset, 0, MaxLen - 1)
    else
      NewSel.Offset := -1;
    if NewSel.Offset >= 0 then
      NewSel.Length := EnsureRange(NewSel.Length, 1, MaxLen - NewSel.Offset)
    else
      NewSel.Length := 0;
  end;
  if (FSelection.Offset = NewSel.Offset) and (FSelection.Length = NewSel.Length) then
    Exit;
  FSelection := NewSel;
  DoSelectionChange;
  Invalidate;
end;

procedure TOBDHexView.SetRegions(AValue: TOBDHexRegionCollection);
begin
  FRegions.Assign(AValue);
end;

procedure TOBDHexView.RegionsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDHexView.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDHexView.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self, FSelection);
end;

procedure TOBDHexView.LoadFromStream(AStream: TStream);
var
  Remaining: Int64;
begin
  if AStream = nil then
    SetLength(FData, 0)
  else
  begin
    Remaining := AStream.Size - AStream.Position;
    if Remaining < 0 then
      Remaining := 0;
    if Remaining > MaxInt then
      raise EStreamError.Create('Stream is too large for TOBDHexView');
    SetLength(FData, Integer(Remaining));
    if Length(FData) > 0 then
      AStream.ReadBuffer(FData[0], Length(FData));
  end;
  SetScrollPos(0);
  Invalidate;
end;

procedure TOBDHexView.Clear;
begin
  SetLength(FData, 0);
  SetLength(FCompareData, 0);
  FRegions.Clear;
  FSelection.Offset := -1;
  FSelection.Length := 0;
  SetScrollPos(0);
  DoSelectionChange;
  Invalidate;
end;

function TOBDHexView.AddRegion(const AName: string; AOffset,
  ALength: Integer; AKind: TOBDHexRegionKind): TOBDHexRegionItem;
begin
  Result := FRegions.Add;
  Result.Name := AName;
  Result.Offset := AOffset;
  Result.Length := ALength;
  Result.Kind := AKind;
end;

function TOBDHexView.ByteChanged(AOffset: Integer): Boolean;
begin
  Result := IsChanged(AOffset);
end;

function TOBDHexView.ChangedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DataLength - 1 do
    if IsChanged(I) then
      Inc(Result);
end;

function TOBDHexView.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDHexView.ToolHeight: Integer;
begin
  Result := ScaleValue(Metrics.Edit + 20);
end;

function TOBDHexView.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDHexView.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDHexView.RowHeight: Integer;
begin
  if Density = dnTablet then
    Result := ScaleValue(30)
  else
    Result := ScaleValue(22);
end;

function TOBDHexView.BodyRect: TRect;
begin
  Result := Rect(ScaleValue(1), HeaderHeight + ToolHeight + ColumnHeaderHeight,
    ClientWidth - ScaleValue(1), ClientHeight - FooterHeight);
end;

function TOBDHexView.ScrollTrackRect: TRect;
var
  R: TRect;
begin
  R := BodyRect;
  Result := Rect(ClientWidth - ScaleValue(12), R.Top + ScaleValue(4),
    ClientWidth - ScaleValue(2), R.Bottom - ScaleValue(4));
end;

function TOBDHexView.ContentHeight: Integer;
begin
  Result := RowCount * RowHeight;
end;

function TOBDHexView.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDHexView.VisibleRowCount: Integer;
begin
  Result := BodyRect.Height div System.Math.Max(1, RowHeight);
end;

function TOBDHexView.RowCount: Integer;
begin
  Result := (DataLength + FBytesPerRow - 1) div FBytesPerRow;
end;

function TOBDHexView.DataLength: Integer;
begin
  if UsePreviewData then
    Result := 256
  else
    Result := Length(FData);
end;

function TOBDHexView.UsePreviewData: Boolean;
begin
  Result := (Length(FData) = 0) and IsPreview;
end;

function TOBDHexView.ByteValue(AOffset: Integer): Byte;
begin
  if UsePreviewData then
    Result := HexPreviewByte(AOffset)
  else if (AOffset >= 0) and (AOffset < Length(FData)) then
    Result := FData[AOffset]
  else
    Result := 0;
end;

function TOBDHexView.CompareByteValue(AOffset: Integer; out AValue: Byte): Boolean;
begin
  Result := (AOffset >= 0) and (AOffset < Length(FCompareData));
  if Result then
    AValue := FCompareData[AOffset]
  else
    AValue := 0;
end;

function TOBDHexView.IsChanged(AOffset: Integer): Boolean;
var
  B: Byte;
begin
  Result := CompareByteValue(AOffset, B) and (ByteValue(AOffset) <> B);
  if UsePreviewData then
    Result := (AOffset = $52) or (AOffset = $53) or (AOffset = $7A) or
      (AOffset = $9C);
end;

function TOBDHexView.IsSelected(AOffset: Integer): Boolean;
begin
  Result := (FSelection.Offset >= 0) and (AOffset >= FSelection.Offset) and
    (AOffset < FSelection.Offset + FSelection.Length);
  if UsePreviewData and (FSelection.Offset < 0) then
    Result := (AOffset >= $3A) and (AOffset < $3C);
end;

function TOBDHexView.RegionColor(APainter: TOBDPainter;
  AKind: TOBDHexRegionKind): TColor;
begin
  case AKind of
    hvrSuccess:
      Result := Palette.Success;
    hvrWarning:
      Result := Palette.Warning;
    hvrDanger:
      Result := Palette.Danger;
    hvrNeutral:
      Result := Palette.Subtle;
  else
    Result := APainter.AccentText;
  end;
end;

function TOBDHexView.ScrollThumbRect: TRect;
begin
  if ContentHeight <= BodyRect.Height then
    Result := Rect(0, 0, 0, 0)
  else
    Result := OBDScrollThumbRect(ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDHexView.HitByte(X, Y: Integer; out AOffset: Integer): Boolean;
var
  Body: TRect;
  Row, Col, CellW, Bx0: Integer;
begin
  Result := False;
  AOffset := -1;
  Body := BodyRect;
  if not PtInRect(Body, Point(X, Y)) then
    Exit;
  CellW := System.Math.Max(ScaleValue(20), System.Math.Min(ScaleValue(26),
    (ClientWidth - ScaleValue(160)) div System.Math.Max(1, FBytesPerRow)));
  Bx0 := ScaleValue(82);
  if (X < Bx0) or (X >= Bx0 + CellW * FBytesPerRow) then
    Exit;
  Row := (Y - Body.Top + FScrollPos) div RowHeight;
  Col := (X - Bx0) div CellW;
  AOffset := Row * FBytesPerRow + Col;
  Result := (AOffset >= 0) and (AOffset < DataLength);
end;

procedure TOBDHexView.DrawHeader(APainter: TOBDPainter);
var
  Y, X, ChipW, BtnW: Integer;
  Text: string;
begin
  Y := HeaderHeight div 2;
  APainter.Glyph(glEeprom, ScaleValue(26), Y, APainter.AccentText, 1);
  APainter.Text(ScaleValue(46), Y - ScaleValue(8), FTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - ScaleValue(220));
  APainter.Text(ScaleValue(46), Y + ScaleValue(12), Format('%d bytes at 0x%.4x',
    [DataLength, FBaseAddress]), HEX_META, Palette.GaugeLabel, twRegular,
    taLeftJustify, ClientWidth - ScaleValue(220));
  X := ScaleValue(46) + APainter.TextWidth(FTitle, OBD_TEXT_TITLE, twBold) +
    ScaleValue(12);
  if (Length(FCompareData) > 0) or UsePreviewData then
  begin
    Text := Format('%d DIFFER', [ChangedCount]);
    ChipW := APainter.ChipWidth(Text);
    if X + ChipW < ClientWidth - ScaleValue(120) then
      APainter.Chip(X, Y - ScaleValue(10), Text, Palette.Danger);
  end;
  BtnW := APainter.ButtonWidth('Save as...', glSave);
  APainter.Button(Rect(ClientWidth - ScaleValue(16) - BtnW,
    Y - ScaleValue(Metrics.Button) div 2, ClientWidth - ScaleValue(16),
    Y + ScaleValue(Metrics.Button) div 2), 'Save as...', bkSecondary,
    cstNormal, False, glSave);
end;

procedure TOBDHexView.DrawToolbar(APainter: TOBDPainter);
var
  Y, H, R: Integer;
begin
  Y := HeaderHeight + ScaleValue(10);
  H := ScaleValue(Metrics.Edit);
  R := ClientWidth - ScaleValue(16);
  APainter.Caps(ScaleValue(16), Y + H div 2, 'Offset');
  APainter.EditFrame(Rect(ScaleValue(70), Y, ScaleValue(160), Y + H), False,
    Enabled);
  APainter.Text(ScaleValue(80), Y + H div 2, Format('0x%.4x',
    [System.Math.Max(0, FSelection.Offset)]), HEX_TEXT, Palette.ForegroundText,
    twMono);
  APainter.Caps(ScaleValue(180), Y + H div 2, 'Compare');
  APainter.EditFrame(Rect(ScaleValue(248), Y, ScaleValue(458), Y + H), False,
    Enabled);
  if (Length(FCompareData) > 0) or UsePreviewData then
    APainter.Text(ScaleValue(258), Y + H div 2, 'Backup buffer', HEX_TEXT,
      Palette.ForegroundText, twRegular, taLeftJustify, ScaleValue(170))
  else
    APainter.Text(ScaleValue(258), Y + H div 2, 'None', HEX_TEXT,
      Palette.GaugeLabel);
  APainter.GlyphChevron(ScaleValue(444), Y + H div 2, Palette.Subtle, True);
  if R - ScaleValue(220) > ScaleValue(470) then
  begin
    APainter.EditFrame(Rect(R - ScaleValue(220), Y, R, Y + H), False, Enabled);
    APainter.Glyph(glSearch, R - ScaleValue(204), Y + H div 2,
      Palette.Subtle, 0.85);
    APainter.Text(R - ScaleValue(184), Y + H div 2, 'Find hex or text',
      HEX_TEXT, Palette.GaugeLabel);
  end;
end;

procedure TOBDHexView.DrawColumns(APainter: TOBDPainter);
var
  Y, H, I, CellW, Bx0, AscX: Integer;
begin
  Y := HeaderHeight + ToolHeight;
  H := ColumnHeaderHeight;
  APainter.FillRect(Rect(ScaleValue(1), Y, ClientWidth - ScaleValue(1), Y + H),
    APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), Y + H - ScaleValue(1),
    ClientWidth - ScaleValue(2), Palette.NeutralLight);
  APainter.Caps(ScaleValue(16), Y + H div 2, 'Offset');
  CellW := System.Math.Max(ScaleValue(20), System.Math.Min(ScaleValue(26),
    (ClientWidth - ScaleValue(160)) div System.Math.Max(1, FBytesPerRow)));
  Bx0 := ScaleValue(82);
  for I := 0 to FBytesPerRow - 1 do
    APainter.Text(Bx0 + I * CellW + CellW div 2, Y + H div 2,
      Format('%.2x', [I]), OBD_TEXT_CAPS, Palette.GaugeLabel, twMonoBold,
      taCenter);
  AscX := Bx0 + FBytesPerRow * CellW + ScaleValue(18);
  APainter.Caps(AscX, Y + H div 2, 'ASCII');
end;

procedure TOBDHexView.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, Row, Col, Offset, Top, Mid, CellW, Bx0, AscX, I: Integer;
  Body: TRect;
  Text, Ch: string;
  Reg: TOBDHexRegionItem;
  Color, Fill: TColor;
begin
  Body := BodyRect;
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, Body.Left, Body.Top, Body.Right,
      Body.Bottom);
    CellW := System.Math.Max(ScaleValue(20), System.Math.Min(ScaleValue(26),
      (ClientWidth - ScaleValue(160)) div System.Math.Max(1, FBytesPerRow)));
    Bx0 := ScaleValue(82);
    AscX := Bx0 + FBytesPerRow * CellW + ScaleValue(18);
    Top := Body.Top - FScrollPos;
    for Row := 0 to RowCount - 1 do
    begin
      if (Top + RowHeight >= Body.Top) and (Top <= Body.Bottom) then
      begin
        Mid := Top + RowHeight div 2;
        APainter.Text(ScaleValue(16), Mid, Format('%.4x',
          [FBaseAddress + Row * FBytesPerRow]), HEX_TEXT, Palette.GaugeLabel,
          twMono);
        Text := '';
        for Col := 0 to FBytesPerRow - 1 do
        begin
          Offset := Row * FBytesPerRow + Col;
          if Offset >= DataLength then
            Break;
          Reg := FRegions.RegionAt(Offset);
          if (Reg = nil) and UsePreviewData and (Offset >= $38) and
            (Offset < $3C) then
          begin
            Fill := APainter.Tint(APainter.AccentText, 0.16);
            Color := APainter.AccentText;
          end
          else if Reg <> nil then
          begin
            Color := RegionColor(APainter, Reg.Kind);
            Fill := APainter.Tint(Color, 0.16);
          end
          else
          begin
            Color := Palette.ForegroundText;
            Fill := clNone;
          end;
          if Fill <> clNone then
            APainter.FillRect(Rect(Bx0 + Col * CellW, Top + ScaleValue(1),
              Bx0 + (Col + 1) * CellW, Top + RowHeight - ScaleValue(1)), Fill);
          if IsSelected(Offset) then
            APainter.FrameRect(Rect(Bx0 + Col * CellW, Top + ScaleValue(1),
              Bx0 + (Col + 1) * CellW, Top + RowHeight - ScaleValue(1)),
              APainter.AccentText);
          if IsChanged(Offset) then
            Color := APainter.DangerInk;
          APainter.Text(Bx0 + Col * CellW + CellW div 2, Mid,
            Format('%.2x', [ByteValue(Offset)]), HEX_TEXT, Color,
            twMonoBold, taCenter);
          if IsChanged(Offset) then
            APainter.HLine(Bx0 + Col * CellW + ScaleValue(5),
              Mid + ScaleValue(8), CellW - ScaleValue(10), APainter.DangerInk);
          if ByteValue(Offset) in [32..126] then
            Ch := Chr(ByteValue(Offset))
          else
            Ch := #$00B7;
          Text := Text + Ch;
        end;
        APainter.Text(AscX, Mid, Text, HEX_TEXT, Palette.GaugeLabel, twMono,
          taLeftJustify, ClientWidth - AscX - ScaleValue(28));
        for I := 0 to FRegions.Count - 1 do
          if FRegions[I].Offset div FBytesPerRow = Row then
          begin
            APainter.Chip(ClientWidth - ScaleValue(30) -
              APainter.ChipWidth(FRegions[I].Name, False, OBD_TEXT_CAPS),
              Mid - ScaleValue(10), FRegions[I].Name,
              RegionColor(APainter, FRegions[I].Kind), False, False,
              OBD_TEXT_CAPS);
            Break;
          end;
      end;
      Inc(Top, RowHeight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDHexView.DrawFooter(APainter: TOBDPainter);
var
  R: TRect;
  Text: string;
begin
  R := Rect(0, ClientHeight - FooterHeight, ClientWidth, ClientHeight);
  APainter.HLine(ScaleValue(1), R.Top, ClientWidth - ScaleValue(2),
    Palette.NeutralLight);
  if FSelection.Offset >= 0 then
    Text := Format('Offset 0x%.4x %s %d bytes selected %s %d bytes differ',
      [FSelection.Offset, #$00B7, FSelection.Length, #$00B7, ChangedCount])
  else
    Text := Format('%d bytes %s %d regions %s %d bytes differ',
      [DataLength, #$00B7, FRegions.Count, #$00B7, ChangedCount]);
  APainter.Text(ScaleValue(16), R.Top + R.Height div 2, Text, HEX_META,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - ScaleValue(32));
end;

procedure TOBDHexView.DrawEmpty(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := BodyRect;
  APainter.Glyph(glEeprom, R.Left + R.Width div 2, R.Top + R.Height div 2 -
    ScaleValue(22), Palette.Subtle, 1.5);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(10),
    'No memory dump loaded', OBD_TEXT_HEADING, Palette.ForegroundText,
    twSemibold, taCenter);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(32),
    'Call SetData or LoadFromStream to display bytes.', HEX_META,
    Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDHexView.DrawScroll(APainter: TOBDPainter);
begin
  if ContentHeight > BodyRect.Height then
    OBDPaintScrollBar(APainter, ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDHexView.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawHeader(P);
    DrawToolbar(P);
    DrawColumns(P);
    if DataLength = 0 then
      DrawEmpty(P)
    else
      DrawRows(P, ACanvas);
    DrawFooter(P);
    DrawScroll(P);
  finally
    P.Free;
  end;
end;

procedure TOBDHexView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Offset: Integer;
  Thumb, Track: TRect;
  Sel: TOBDHexSelection;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  Thumb := ScrollThumbRect;
  Track := ScrollTrackRect;
  if not Thumb.IsEmpty and PtInRect(Thumb, Point(X, Y)) then
  begin
    FDraggingScroll := True;
    FScrollDragOffset := Y - Thumb.Top;
    MouseCapture := True;
    Invalidate;
    Exit;
  end;
  if (ContentHeight > BodyRect.Height) and PtInRect(Track, Point(X, Y)) then
  begin
    if Y < Thumb.Top then
      SetScrollPos(FScrollPos - BodyRect.Height)
    else if Y > Thumb.Bottom then
      SetScrollPos(FScrollPos + BodyRect.Height);
    Exit;
  end;
  if HitByte(X, Y, Offset) then
  begin
    Sel.Offset := Offset;
    Sel.Length := 1;
    SetSelection(Sel);
  end;
end;

procedure TOBDHexView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Track, Thumb: TRect;
begin
  inherited;
  if FDraggingScroll then
  begin
    Track := ScrollTrackRect;
    Thumb := ScrollThumbRect;
    if (Track.Height - Thumb.Height) > 0 then
      SetScrollPos(MulDiv(Y - FScrollDragOffset - Track.Top, MaxScroll,
        Track.Height - Thumb.Height));
    Exit;
  end;
  FScrollHover := (ContentHeight > BodyRect.Height) and
    PtInRect(ScrollTrackRect, Point(X, Y));
  if FScrollHover then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Invalidate;
end;

procedure TOBDHexView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

function TOBDHexView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if WheelDelta < 0 then
    SetScrollPos(FScrollPos + RowHeight * 3)
  else
    SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDHexView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Sel: TOBDHexSelection;
  Delta: Integer;
begin
  inherited;
  Sel := FSelection;
  if Sel.Offset < 0 then
  begin
    Sel.Offset := 0;
    Sel.Length := 1;
  end;
  Delta := 0;
  case Key of
    VK_LEFT:
      Delta := -1;
    VK_RIGHT:
      Delta := 1;
    VK_UP:
      Delta := -FBytesPerRow;
    VK_DOWN:
      Delta := FBytesPerRow;
    VK_PRIOR:
      Delta := -FBytesPerRow * VisibleRowCount;
    VK_NEXT:
      Delta := FBytesPerRow * VisibleRowCount;
    VK_HOME:
      Sel.Offset := 0;
    VK_END:
      Sel.Offset := DataLength - 1;
  else
    Exit;
  end;
  if Delta <> 0 then
    Inc(Sel.Offset, Delta);
  Sel.Length := 1;
  SetSelection(Sel);
  if Sel.Offset >= 0 then
    SetScrollPos((Sel.Offset div FBytesPerRow) * RowHeight - BodyRect.Height div 2);
  Key := 0;
end;

procedure TOBDHexView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FScrollHover := False;
  Cursor := crDefault;
  Invalidate;
end;

procedure TOBDHexView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
