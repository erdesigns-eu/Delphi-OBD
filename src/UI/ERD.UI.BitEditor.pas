//------------------------------------------------------------------------------
//  ERD.UI.BitEditor
//
//  TOBDBitEditor - long-coding byte and bit editor with byte list, labelled
//  bits, option definitions and changed-bit highlighting.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit ERD.UI.BitEditor;

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
  ERD.UI.ScrollBar;

type
  /// <summary>Kind of coding label.</summary>
  TOBDBitLabelKind = (
    /// <summary>Single-bit check box label.</summary>
    bleCheck,
    /// <summary>Multi-bit option label using pipe-separated options.</summary>
    bleOptions);

  /// <summary>Event fired when coding bytes change.</summary>
  TOBDBitEditorChangeEvent = procedure(Sender: TObject; AChangedCount: Integer)
    of object;

  /// <summary>One label definition for a byte or bit field.</summary>
  TOBDBitLabelItem = class(TCollectionItem)
  strict private
    FByteIndex: Integer;
    FBitIndex: Integer;
    FBitCount: Integer;
    FCaption: string;
    FKind: TOBDBitLabelKind;
    FOptions: string;
    procedure SetByteIndex(AValue: Integer);
    procedure SetBitIndex(AValue: Integer);
    procedure SetBitCount(AValue: Integer);
    procedure SetCaption(const AValue: string);
    procedure SetKind(AValue: TOBDBitLabelKind);
    procedure SetOptions(const AValue: string);
  protected
    /// <summary>Returns the label caption in the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a single-bit check label.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Mask for this label inside its byte.</summary>
    /// <returns>Byte mask.</returns>
    function Mask: Byte;
    /// <summary>Current value extracted from a byte.</summary>
    /// <param name="AByte">Byte value.</param>
    /// <returns>Field value.</returns>
    function Extract(AByte: Byte): Integer;
    /// <summary>Applies a field value to a byte.</summary>
    /// <param name="AByte">Byte to modify.</param>
    /// <param name="AValue">New field value.</param>
    /// <returns>Modified byte.</returns>
    function Apply(AByte: Byte; AValue: Integer): Byte;
    /// <summary>Returns one option caption by value.</summary>
    /// <param name="AValue">Option index.</param>
    /// <returns>Option caption or an empty string.</returns>
    function OptionCaption(AValue: Integer): string;
  published
    /// <summary>Byte index this label belongs to.</summary>
    property ByteIndex: Integer read FByteIndex write SetByteIndex default 0;
    /// <summary>Least significant bit of the field.</summary>
    property BitIndex: Integer read FBitIndex write SetBitIndex default 0;
    /// <summary>Number of bits in the field.</summary>
    property BitCount: Integer read FBitCount write SetBitCount default 1;
    /// <summary>Mechanic-facing label text.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Single-bit check or multi-bit options.</summary>
    property Kind: TOBDBitLabelKind read FKind write SetKind default bleCheck;
    /// <summary>Pipe-separated option captions for option labels.</summary>
    property Options: string read FOptions write SetOptions;
  end;

  /// <summary>Collection of coding label definitions.</summary>
  TOBDBitLabelCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDBitLabelItem;
    procedure SetItem(Index: Integer; AValue: TOBDBitLabelItem);
  protected
    /// <summary>Notifies the owner after a label changes.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a label collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a label item.</summary>
    /// <returns>New label item.</returns>
    function Add: TOBDBitLabelItem;
    /// <summary>Finds the first label for a byte.</summary>
    /// <param name="AByteIndex">Byte index.</param>
    /// <returns>Label item or nil.</returns>
    function FirstForByte(AByteIndex: Integer): TOBDBitLabelItem;
    /// <summary>Indexed label items.</summary>
    property Items[Index: Integer]: TOBDBitLabelItem read GetItem write SetItem;
      default;
    /// <summary>Fires after a label changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Painted long-coding bit editor.</summary>
  TOBDBitEditor = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FSubtitle: string;
    FCoding: TBytes;
    FOriginalCoding: TBytes;
    FLabels: TOBDBitLabelCollection;
    FSelectedByte: Integer;
    FScrollPos: Integer;
    FScrollHover: Boolean;
    FDraggingScroll: Boolean;
    FScrollDragOffset: Integer;
    FOnChange: TOBDBitEditorChangeEvent;
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetCoding(const AValue: TBytes);
    procedure SetOriginalCoding(const AValue: TBytes);
    procedure SetLabels(AValue: TOBDBitLabelCollection);
    procedure SetSelectedByte(AValue: Integer);
    procedure LabelsChanged(Sender: TObject);
    procedure SetScrollPos(AValue: Integer);
    procedure DoChange;
    function HeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowHeight: Integer;
    function SummaryHeight: Integer;
    function BodyRect: TRect;
    function ScrollTrackRect: TRect;
    function ByteCount: Integer;
    function UsePreviewData: Boolean;
    function PreviewByte(AIndex: Integer): Byte;
    function CodingByte(AIndex: Integer): Byte;
    function OriginalByte(AIndex: Integer): Byte;
    function ByteChanged(AIndex: Integer): Boolean;
    function BitChanged(AByte, ABit: Integer): Boolean;
    function MaxScroll: Integer;
    function ContentHeight: Integer;
    function ScrollThumbRect: TRect;
    function HitByteRow(X, Y: Integer; out AByteIndex: Integer): Boolean;
    function HitBit(X, Y: Integer; out AByteIndex, ABit: Integer): Boolean;
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawSummary(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure DrawLabelRows(APainter: TOBDPainter; AByteIndex: Integer;
      var ATop: Integer; ALeft: Integer);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawEmpty(APainter: TOBDPainter);
    procedure DrawScroll(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the bit editor card.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Selects rows and toggles painted bits.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Updates scroll-bar hover and dragging.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Ends scroll-bar dragging.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Shift state.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    /// <summary>Scrolls rows.</summary>
    /// <param name="Shift">Shift state.</param>
    /// <param name="WheelDelta">Wheel delta.</param>
    /// <param name="MousePos">Screen mouse position.</param>
    /// <returns>True when handled.</returns>
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    /// <summary>Moves the selected byte and toggles bits from the keyboard.</summary>
    /// <param name="Key">Virtual key code.</param>
    /// <param name="Shift">Shift state.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates a 720 x 420 bit editor.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the label collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints when density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Clears coding and labels.</summary>
    procedure Clear;
    /// <summary>Adds a single-bit label definition.</summary>
    /// <param name="AByteIndex">Byte index.</param>
    /// <param name="ABitIndex">Bit index.</param>
    /// <param name="ACaption">Label caption.</param>
    /// <returns>Created label item.</returns>
    function AddBitLabel(AByteIndex, ABitIndex: Integer;
      const ACaption: string): TOBDBitLabelItem;
    /// <summary>Adds a multi-bit option label definition.</summary>
    /// <param name="AByteIndex">Byte index.</param>
    /// <param name="ABitIndex">Least significant bit.</param>
    /// <param name="ABitCount">Number of bits.</param>
    /// <param name="ACaption">Label caption.</param>
    /// <param name="AOptions">Pipe-separated option captions.</param>
    /// <returns>Created label item.</returns>
    function AddOptionLabel(AByteIndex, ABitIndex, ABitCount: Integer;
      const ACaption, AOptions: string): TOBDBitLabelItem;
    /// <summary>Returns a byte value.</summary>
    /// <param name="AIndex">Byte index.</param>
    /// <returns>Byte value or zero.</returns>
    function ByteValue(AIndex: Integer): Byte;
    /// <summary>Sets or clears a bit.</summary>
    /// <param name="AByteIndex">Byte index.</param>
    /// <param name="ABitIndex">Bit index.</param>
    /// <param name="AOn">New check state.</param>
    procedure SetBit(AByteIndex, ABitIndex: Integer; AOn: Boolean);
    /// <summary>Toggles a bit and fires OnChange.</summary>
    /// <param name="AByteIndex">Byte index.</param>
    /// <param name="ABitIndex">Bit index.</param>
    procedure ToggleBit(AByteIndex, ABitIndex: Integer);
    /// <summary>Applies a multi-bit option value.</summary>
    /// <param name="ALabel">Label definition.</param>
    /// <param name="AValue">New field value.</param>
    procedure SetLabelValue(ALabel: TOBDBitLabelItem; AValue: Integer);
    /// <summary>Number of bytes or bits changed against OriginalCoding.</summary>
    /// <returns>Changed bit count.</returns>
    function ChangedCount: Integer;
    /// <summary>Current coding bytes.</summary>
    property Coding: TBytes read FCoding write SetCoding;
    /// <summary>Original bytes used for change highlighting.</summary>
    property OriginalCoding: TBytes read FOriginalCoding write SetOriginalCoding;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle with ECU and label-file information.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Label definitions shown under byte rows.</summary>
    property Labels: TOBDBitLabelCollection read FLabels write SetLabels;
    /// <summary>Selected byte index.</summary>
    property SelectedByte: Integer read FSelectedByte write SetSelectedByte
      default 0;
    /// <summary>Desktop or tablet row sizes.</summary>
    property Density;
    /// <summary>Takes density from the current theme.</summary>
    property ParentDensity;
    /// <summary>Fires after coding changes.</summary>
    property OnChange: TOBDBitEditorChangeEvent read FOnChange write FOnChange;
  end;

implementation

const
  BIT_TEXT = OBD_TEXT_BODY;
  BIT_META = OBD_TEXT_SMALL;

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

{ TOBDBitLabelItem }

constructor TOBDBitLabelItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBitCount := 1;
  FKind := bleCheck;
end;

function TOBDBitLabelItem.Mask: Byte;
var
  I, M: Integer;
begin
  M := 0;
  for I := 0 to FBitCount - 1 do
    M := M or (1 shl (FBitIndex + I));
  Result := Byte(M and $FF);
end;

function TOBDBitLabelItem.Extract(AByte: Byte): Integer;
begin
  Result := (AByte and Mask) shr FBitIndex;
end;

function TOBDBitLabelItem.Apply(AByte: Byte; AValue: Integer): Byte;
var
  M, V: Integer;
begin
  M := Mask;
  V := (AValue shl FBitIndex) and M;
  Result := Byte((AByte and not M) or V);
end;

function TOBDBitLabelItem.OptionCaption(AValue: Integer): string;
var
  S: string;
  P, I: Integer;
begin
  Result := '';
  S := FOptions + '|';
  I := 0;
  while S <> '' do
  begin
    P := Pos('|', S);
    if P <= 0 then
      Break;
    if I = AValue then
      Exit(Copy(S, 1, P - 1));
    Delete(S, 1, P);
    Inc(I);
  end;
end;

function TOBDBitLabelItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDBitLabelItem.SetByteIndex(AValue: Integer);
begin
  AValue := System.Math.Max(0, AValue);
  if FByteIndex = AValue then
    Exit;
  FByteIndex := AValue;
  Changed(False);
end;

procedure TOBDBitLabelItem.SetBitIndex(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 7);
  if FBitIndex = AValue then
    Exit;
  FBitIndex := AValue;
  if FBitIndex + FBitCount > 8 then
    FBitCount := 8 - FBitIndex;
  Changed(False);
end;

procedure TOBDBitLabelItem.SetBitCount(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 1, 8 - FBitIndex);
  if FBitCount = AValue then
    Exit;
  FBitCount := AValue;
  Changed(False);
end;

procedure TOBDBitLabelItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDBitLabelItem.SetKind(AValue: TOBDBitLabelKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TOBDBitLabelItem.SetOptions(const AValue: string);
begin
  if FOptions = AValue then
    Exit;
  FOptions := AValue;
  Changed(False);
end;

{ TOBDBitLabelCollection }

constructor TOBDBitLabelCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDBitLabelItem);
end;

function TOBDBitLabelCollection.Add: TOBDBitLabelItem;
begin
  Result := TOBDBitLabelItem(inherited Add);
end;

function TOBDBitLabelCollection.FirstForByte(AByteIndex: Integer): TOBDBitLabelItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].ByteIndex = AByteIndex then
      Exit(Items[I]);
end;

function TOBDBitLabelCollection.GetItem(Index: Integer): TOBDBitLabelItem;
begin
  Result := TOBDBitLabelItem(inherited GetItem(Index));
end;

procedure TOBDBitLabelCollection.SetItem(Index: Integer;
  AValue: TOBDBitLabelItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDBitLabelCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDBitEditor }

constructor TOBDBitEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Long coding';
  FSubtitle := 'Control unit coding';
  FSelectedByte := 0;
  FLabels := TOBDBitLabelCollection.Create(Self);
  FLabels.OnChange := LabelsChanged;
  Width := 720;
  Height := 420;
  TabStop := True;
end;

destructor TOBDBitEditor.Destroy;
begin
  FLabels.Free;
  inherited;
end;

procedure TOBDBitEditor.DensityChanged;
begin
  inherited DensityChanged;
  SetScrollPos(FScrollPos);
end;

function TOBDBitEditor.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
  if StyleBackground <> clDefault then
    Result := StyleBackground;
end;

procedure TOBDBitEditor.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDBitEditor.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDBitEditor.SetCoding(const AValue: TBytes);
begin
  if SameBytes(FCoding, AValue) then
    Exit;
  FCoding := Copy(AValue, 0, Length(AValue));
  if Length(FOriginalCoding) = 0 then
    FOriginalCoding := Copy(AValue, 0, Length(AValue));
  SetSelectedByte(FSelectedByte);
  SetScrollPos(FScrollPos);
  DoChange;
end;

procedure TOBDBitEditor.SetOriginalCoding(const AValue: TBytes);
begin
  if SameBytes(FOriginalCoding, AValue) then
    Exit;
  FOriginalCoding := Copy(AValue, 0, Length(AValue));
  Invalidate;
end;

procedure TOBDBitEditor.SetLabels(AValue: TOBDBitLabelCollection);
begin
  FLabels.Assign(AValue);
end;

procedure TOBDBitEditor.SetSelectedByte(AValue: Integer);
begin
  if ByteCount > 0 then
    AValue := EnsureRange(AValue, 0, ByteCount - 1)
  else
    AValue := 0;
  if FSelectedByte = AValue then
    Exit;
  FSelectedByte := AValue;
  Invalidate;
end;

procedure TOBDBitEditor.LabelsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TOBDBitEditor.SetScrollPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, MaxScroll);
  if FScrollPos = AValue then
    Exit;
  FScrollPos := AValue;
  Invalidate;
end;

procedure TOBDBitEditor.DoChange;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self, ChangedCount);
end;

procedure TOBDBitEditor.Clear;
begin
  SetLength(FCoding, 0);
  SetLength(FOriginalCoding, 0);
  FLabels.Clear;
  FSelectedByte := 0;
  FScrollPos := 0;
  DoChange;
end;

function TOBDBitEditor.AddBitLabel(AByteIndex, ABitIndex: Integer;
  const ACaption: string): TOBDBitLabelItem;
begin
  Result := FLabels.Add;
  Result.ByteIndex := AByteIndex;
  Result.BitIndex := ABitIndex;
  Result.BitCount := 1;
  Result.Kind := bleCheck;
  Result.Caption := ACaption;
end;

function TOBDBitEditor.AddOptionLabel(AByteIndex, ABitIndex,
  ABitCount: Integer; const ACaption, AOptions: string): TOBDBitLabelItem;
begin
  Result := FLabels.Add;
  Result.ByteIndex := AByteIndex;
  Result.BitIndex := ABitIndex;
  Result.BitCount := ABitCount;
  Result.Kind := bleOptions;
  Result.Caption := ACaption;
  Result.Options := AOptions;
end;

function TOBDBitEditor.ByteValue(AIndex: Integer): Byte;
begin
  Result := CodingByte(AIndex);
end;

procedure TOBDBitEditor.SetBit(AByteIndex, ABitIndex: Integer; AOn: Boolean);
var
  B: Byte;
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(FCoding)) or
    (ABitIndex < 0) or (ABitIndex > 7) then
    Exit;
  B := FCoding[AByteIndex];
  if AOn then
    B := B or Byte(1 shl ABitIndex)
  else
    B := B and not Byte(1 shl ABitIndex);
  if FCoding[AByteIndex] = B then
    Exit;
  FCoding[AByteIndex] := B;
  DoChange;
end;

procedure TOBDBitEditor.ToggleBit(AByteIndex, ABitIndex: Integer);
begin
  if (AByteIndex < 0) or (AByteIndex >= Length(FCoding)) or
    (ABitIndex < 0) or (ABitIndex > 7) then
    Exit;
  FCoding[AByteIndex] := FCoding[AByteIndex] xor Byte(1 shl ABitIndex);
  DoChange;
end;

procedure TOBDBitEditor.SetLabelValue(ALabel: TOBDBitLabelItem; AValue: Integer);
begin
  if (ALabel = nil) or (ALabel.ByteIndex < 0) or
    (ALabel.ByteIndex >= Length(FCoding)) then
    Exit;
  FCoding[ALabel.ByteIndex] := ALabel.Apply(FCoding[ALabel.ByteIndex], AValue);
  DoChange;
end;

function TOBDBitEditor.ChangedCount: Integer;
var
  I, B: Integer;
begin
  Result := 0;
  for I := 0 to ByteCount - 1 do
    for B := 0 to 7 do
      if BitChanged(I, B) then
        Inc(Result);
end;

function TOBDBitEditor.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDBitEditor.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDBitEditor.RowHeight: Integer;
begin
  Result := System.Math.Max(ScaleValue(24), ScaleValue(Metrics.Row - 10));
end;

function TOBDBitEditor.SummaryHeight: Integer;
begin
  Result := ScaleValue(58);
end;

function TOBDBitEditor.BodyRect: TRect;
begin
  Result := Rect(ScaleValue(1), HeaderHeight + SummaryHeight,
    ClientWidth - ScaleValue(1), ClientHeight - FooterHeight);
end;

function TOBDBitEditor.ScrollTrackRect: TRect;
var
  R: TRect;
begin
  R := BodyRect;
  Result := Rect(ClientWidth - ScaleValue(12), R.Top + ScaleValue(4),
    ClientWidth - ScaleValue(2), R.Bottom - ScaleValue(4));
end;

function TOBDBitEditor.ByteCount: Integer;
begin
  if UsePreviewData then
    Result := 16
  else
    Result := Length(FCoding);
end;

function TOBDBitEditor.UsePreviewData: Boolean;
begin
  Result := (Length(FCoding) = 0) and IsPreview;
end;

function TOBDBitEditor.PreviewByte(AIndex: Integer): Byte;
const
  PREVIEW: array[0..4] of Byte = ($2D, $0B, $3C, $84, $40);
begin
  if AIndex <= High(PREVIEW) then
    Result := PREVIEW[AIndex]
  else
    Result := Byte((AIndex * 37) and $FF);
end;

function TOBDBitEditor.CodingByte(AIndex: Integer): Byte;
begin
  if UsePreviewData then
    Result := PreviewByte(AIndex)
  else if (AIndex >= 0) and (AIndex < Length(FCoding)) then
    Result := FCoding[AIndex]
  else
    Result := 0;
end;

function TOBDBitEditor.OriginalByte(AIndex: Integer): Byte;
const
  PREVIEW_ORIGINAL: array[0..4] of Byte = ($2D, $0B, $0C, $81, $40);
begin
  if UsePreviewData then
  begin
    if AIndex <= High(PREVIEW_ORIGINAL) then
      Result := PREVIEW_ORIGINAL[AIndex]
    else
      Result := PreviewByte(AIndex);
  end
  else if (AIndex >= 0) and (AIndex < Length(FOriginalCoding)) then
    Result := FOriginalCoding[AIndex]
  else
    Result := CodingByte(AIndex);
end;

function TOBDBitEditor.ByteChanged(AIndex: Integer): Boolean;
begin
  Result := CodingByte(AIndex) <> OriginalByte(AIndex);
end;

function TOBDBitEditor.BitChanged(AByte, ABit: Integer): Boolean;
begin
  Result := (((CodingByte(AByte) xor OriginalByte(AByte)) shr ABit) and 1) <> 0;
end;

function TOBDBitEditor.MaxScroll: Integer;
begin
  Result := System.Math.Max(0, ContentHeight - BodyRect.Height);
end;

function TOBDBitEditor.ContentHeight: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to ByteCount - 1 do
  begin
    Inc(Result, RowHeight);
    if (I = FSelectedByte) or (FLabels.FirstForByte(I) <> nil) then
      for J := 0 to FLabels.Count - 1 do
        if FLabels[J].ByteIndex = I then
          Inc(Result, RowHeight);
  end;
end;

function TOBDBitEditor.ScrollThumbRect: TRect;
begin
  if ContentHeight <= BodyRect.Height then
    Result := Rect(0, 0, 0, 0)
  else
    Result := OBDScrollThumbRect(ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

function TOBDBitEditor.HitByteRow(X, Y: Integer; out AByteIndex: Integer): Boolean;
var
  Top, I, J: Integer;
begin
  Result := False;
  AByteIndex := -1;
  if not PtInRect(BodyRect, Point(X, Y)) then
    Exit;
  Top := BodyRect.Top - FScrollPos;
  for I := 0 to ByteCount - 1 do
  begin
    if (Y >= Top) and (Y < Top + RowHeight) then
    begin
      AByteIndex := I;
      Exit(True);
    end;
    Inc(Top, RowHeight);
    if (I = FSelectedByte) or (FLabels.FirstForByte(I) <> nil) then
      for J := 0 to FLabels.Count - 1 do
        if FLabels[J].ByteIndex = I then
          Inc(Top, RowHeight);
  end;
end;

function TOBDBitEditor.HitBit(X, Y: Integer; out AByteIndex, ABit: Integer): Boolean;
var
  Top, I, J, Bx, BitW, B: Integer;
begin
  Result := False;
  AByteIndex := -1;
  ABit := -1;
  if not PtInRect(BodyRect, Point(X, Y)) then
    Exit;
  BitW := ScaleValue(22);
  if Density = dnTablet then
    BitW := ScaleValue(30);
  Bx := ScaleValue(160);
  Top := BodyRect.Top - FScrollPos;
  for I := 0 to ByteCount - 1 do
  begin
    if (Y >= Top) and (Y < Top + RowHeight) and (X >= Bx) and
      (X < Bx + 8 * (BitW + ScaleValue(2))) then
    begin
      B := (X - Bx) div (BitW + ScaleValue(2));
      AByteIndex := I;
      ABit := 7 - B;
      Exit((ABit >= 0) and (ABit <= 7));
    end;
    Inc(Top, RowHeight);
    if (I = FSelectedByte) or (FLabels.FirstForByte(I) <> nil) then
      for J := 0 to FLabels.Count - 1 do
        if FLabels[J].ByteIndex = I then
          Inc(Top, RowHeight);
  end;
end;

procedure TOBDBitEditor.DrawHeader(APainter: TOBDPainter);
var
  Y, W, X: Integer;
begin
  Y := HeaderHeight div 2;
  APainter.Glyph(glCoding, ScaleValue(26), Y, APainter.AccentText, 1);
  APainter.Text(ScaleValue(46), Y - ScaleValue(8), FTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - ScaleValue(240));
  APainter.Text(ScaleValue(46), Y + ScaleValue(12), FSubtitle, BIT_META,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - ScaleValue(240));
  W := ScaleValue(44);
  X := ClientWidth - ScaleValue(16) - W * 3;
  APainter.Button(Rect(X, Y - ScaleValue(Metrics.Segment) div 2, X + W,
    Y + ScaleValue(Metrics.Segment) div 2), 'Bits', bkPrimary, cstNormal,
    False, glNone);
  APainter.Button(Rect(X + W, Y - ScaleValue(Metrics.Segment) div 2, X + W * 2,
    Y + ScaleValue(Metrics.Segment) div 2), 'Text', bkSecondary, cstNormal,
    False, glNone);
  APainter.Button(Rect(X + W * 2, Y - ScaleValue(Metrics.Segment) div 2,
    X + W * 3, Y + ScaleValue(Metrics.Segment) div 2), 'Hex', bkSecondary,
    cstNormal, False, glNone);
end;

procedure TOBDBitEditor.DrawSummary(APainter: TOBDPainter);
var
  Y, X, I: Integer;
  Text: string;
  Color: TColor;
begin
  Y := HeaderHeight + ScaleValue(22);
  APainter.Caps(ScaleValue(16), Y, 'Coding');
  X := ScaleValue(90);
  for I := 0 to System.Math.Min(ByteCount - 1, 23) do
  begin
    Text := Format('%.2x', [CodingByte(I)]);
    if ByteChanged(I) then
    begin
      APainter.RoundRect(X - ScaleValue(3), Y - ScaleValue(10),
        APainter.TextWidth(Text, BIT_TEXT, twMonoBold) + ScaleValue(6),
        ScaleValue(20), ScaleValue(4), APainter.Tint(Palette.Accent, 0.18),
        clNone);
      Color := APainter.AccentText;
    end
    else
      Color := Palette.ForegroundText;
    APainter.Text(X, Y, Text, BIT_TEXT, Color, twMonoBold);
    Inc(X, APainter.TextWidth('00 ', BIT_TEXT, twMono));
  end;
  APainter.Text(ClientWidth - ScaleValue(16), Y, 'Copy', BIT_META,
    APainter.AccentText, twSemibold, taRightJustify);
  APainter.HLine(ScaleValue(1), HeaderHeight + SummaryHeight - ScaleValue(1),
    ClientWidth - ScaleValue(2), Palette.NeutralLight);
end;

procedure TOBDBitEditor.DrawRows(APainter: TOBDPainter; ACanvas: TCanvas);
var
  Save, Top, Mid, I, B, BitW, Bx, Cx, LabelLeft: Integer;
  Color, Fill: TColor;
begin
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, BodyRect.Left, BodyRect.Top,
      BodyRect.Right, BodyRect.Bottom);
    BitW := ScaleValue(22);
    if Density = dnTablet then
      BitW := ScaleValue(30);
    Bx := ScaleValue(160);
    LabelLeft := Bx + 8 * (BitW + ScaleValue(2)) + ScaleValue(24);
    Top := BodyRect.Top - FScrollPos;
    for I := 0 to ByteCount - 1 do
    begin
      if (Top + RowHeight >= BodyRect.Top) and (Top <= BodyRect.Bottom) then
      begin
        Mid := Top + RowHeight div 2;
        if I = FSelectedByte then
          APainter.FillRect(Rect(ScaleValue(1), Top, ClientWidth - ScaleValue(1),
            Top + RowHeight), APainter.Tint(Palette.Accent, 0.10));
        if ByteChanged(I) then
          APainter.FillRect(Rect(ScaleValue(1), Top + ScaleValue(4),
            ScaleValue(5), Top + RowHeight - ScaleValue(4)), Palette.Accent);
        APainter.Text(ScaleValue(16), Mid, Format('Byte %.2d', [I]), BIT_TEXT,
          Palette.ForegroundText, twSemibold);
        APainter.Text(ScaleValue(80), Mid, Format('%.2x', [CodingByte(I)]),
          BIT_TEXT, IfThen(ByteChanged(I), APainter.AccentText, Palette.ForegroundText),
          twMonoBold);
        if ByteChanged(I) then
          APainter.Text(ScaleValue(104), Mid, Format('was %.2x',
            [OriginalByte(I)]), BIT_META, Palette.GaugeLabel);
        for B := 7 downto 0 do
        begin
          Cx := Bx + (7 - B) * (BitW + ScaleValue(2));
          if BitChanged(I, B) then
          begin
            Fill := APainter.Tint(Palette.Accent, 0.18);
            Color := APainter.AccentText;
          end
          else if ((CodingByte(I) shr B) and 1) <> 0 then
          begin
            Fill := OBDMixColor(Palette.NeutralLight, Palette.GaugeFace, 0.5);
            Color := Palette.ForegroundText;
          end
          else
          begin
            Fill := Palette.GaugeFace;
            Color := Palette.GaugeLabel;
          end;
          APainter.RoundRect(Cx, Mid - BitW div 2, BitW, BitW,
            ScaleValue(4), Fill, IfThen(BitChanged(I, B), APainter.AccentText,
            Palette.NeutralLight));
          APainter.Text(Cx + BitW div 2, Mid, IntToStr((CodingByte(I) shr B)
            and 1), BIT_TEXT, Color, twMonoBold, taCenter);
        end;
        if FLabels.FirstForByte(I) = nil then
          APainter.Text(LabelLeft, Mid, 'No labels for this byte ' + #$00B7 +
            ' edit bits directly', BIT_META, Palette.GaugeLabel, twRegular,
            taLeftJustify, ClientWidth - LabelLeft - ScaleValue(44));
        APainter.GlyphChevron(ClientWidth - ScaleValue(18), Mid, Palette.Subtle,
          FLabels.FirstForByte(I) <> nil);
      end;
      Inc(Top, RowHeight);
      DrawLabelRows(APainter, I, Top, LabelLeft);
      APainter.HLine(ScaleValue(1), Top - ScaleValue(1),
        ClientWidth - ScaleValue(2), Palette.NeutralLight);
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDBitEditor.DrawLabelRows(APainter: TOBDPainter; AByteIndex: Integer;
  var ATop: Integer; ALeft: Integer);
var
  I, Mid, EditW, Value: Integer;
  Item: TOBDBitLabelItem;
  WhereText, ValueText: string;
begin
  if not ((AByteIndex = FSelectedByte) or (FLabels.FirstForByte(AByteIndex) <> nil)) then
    Exit;
  for I := 0 to FLabels.Count - 1 do
  begin
    Item := FLabels[I];
    if Item.ByteIndex <> AByteIndex then
      Continue;
    Mid := ATop + RowHeight div 2;
    if Item.BitCount = 1 then
      WhereText := Format('bit %d', [Item.BitIndex])
    else
      WhereText := Format('bits %d-%d', [Item.BitIndex,
        Item.BitIndex + Item.BitCount - 1]);
    APainter.Text(ALeft, Mid, WhereText, BIT_META, Palette.GaugeLabel, twMono);
    if Item.Kind = bleCheck then
    begin
      APainter.CheckBox(ALeft + ScaleValue(70), Mid, Metrics.Check,
        TCheckBoxState(Item.Extract(CodingByte(AByteIndex))), True, False,
        False);
      APainter.Text(ALeft + ScaleValue(70 + Metrics.Check + 8), Mid,
        Item.Caption, BIT_TEXT, Palette.ForegroundText, twRegular,
        taLeftJustify, ClientWidth - ALeft - ScaleValue(150));
    end
    else
    begin
      Value := Item.Extract(CodingByte(AByteIndex));
      ValueText := Item.OptionCaption(Value);
      if ValueText = '' then
        ValueText := IntToStr(Value);
      EditW := System.Math.Min(ScaleValue(170),
        System.Math.Max(ScaleValue(120), ClientWidth - ALeft - ScaleValue(220)));
      APainter.Text(ALeft + ScaleValue(70), Mid, Item.Caption, BIT_TEXT,
        Palette.ForegroundText, twRegular, taLeftJustify,
        ClientWidth - ALeft - EditW - ScaleValue(130));
      APainter.EditFrame(Rect(ClientWidth - ScaleValue(44) - EditW,
        Mid - ScaleValue(Metrics.Edit) div 2, ClientWidth - ScaleValue(44),
        Mid + ScaleValue(Metrics.Edit) div 2), False, Enabled);
      APainter.Text(ClientWidth - ScaleValue(34) - EditW, Mid, ValueText,
        BIT_TEXT, Palette.ForegroundText, twRegular, taLeftJustify,
        EditW - ScaleValue(28));
      APainter.GlyphChevron(ClientWidth - ScaleValue(56), Mid, Palette.Subtle,
        True);
    end;
    Inc(ATop, RowHeight);
  end;
end;

procedure TOBDBitEditor.DrawFooter(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := Rect(0, ClientHeight - FooterHeight, ClientWidth, ClientHeight);
  APainter.HLine(ScaleValue(1), R.Top, ClientWidth - ScaleValue(2),
    Palette.NeutralLight);
  APainter.Text(ScaleValue(16), R.Top + R.Height div 2,
    Format('%d changed bits %s %d bytes %s %d labels', [ChangedCount,
    #$00B7, ByteCount, #$00B7, FLabels.Count]), BIT_META,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - ScaleValue(32));
end;

procedure TOBDBitEditor.DrawEmpty(APainter: TOBDPainter);
var
  R: TRect;
begin
  R := BodyRect;
  APainter.Glyph(glCoding, R.Left + R.Width div 2, R.Top + R.Height div 2 -
    ScaleValue(22), Palette.Subtle, 1.5);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(10),
    'No coding loaded', OBD_TEXT_HEADING, Palette.ForegroundText,
    twSemibold, taCenter);
  APainter.Text(R.Left + R.Width div 2, R.Top + R.Height div 2 + ScaleValue(32),
    'Assign Coding and OriginalCoding to edit long coding.', BIT_META,
    Palette.GaugeLabel, twRegular, taCenter);
end;

procedure TOBDBitEditor.DrawScroll(APainter: TOBDPainter);
begin
  if ContentHeight > BodyRect.Height then
    OBDPaintScrollBar(APainter, ScrollTrackRect, True, 0, ContentHeight - 1,
      BodyRect.Height, FScrollPos, FScrollHover, FDraggingScroll);
end;

procedure TOBDBitEditor.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, ClientWidth, ClientHeight), clNone, SurfaceColor);
    DrawHeader(P);
    DrawSummary(P);
    if ByteCount = 0 then
      DrawEmpty(P)
    else
      DrawRows(P, ACanvas);
    DrawFooter(P);
    DrawScroll(P);
  finally
    P.Free;
  end;
end;

procedure TOBDBitEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ByteIndex, BitIndex: Integer;
  Thumb, Track: TRect;
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
  if HitBit(X, Y, ByteIndex, BitIndex) then
  begin
    SetSelectedByte(ByteIndex);
    ToggleBit(ByteIndex, BitIndex);
  end
  else if HitByteRow(X, Y, ByteIndex) then
    SetSelectedByte(ByteIndex);
end;

procedure TOBDBitEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TOBDBitEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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

function TOBDBitEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if WheelDelta < 0 then
    SetScrollPos(FScrollPos + RowHeight * 3)
  else
    SetScrollPos(FScrollPos - RowHeight * 3);
end;

procedure TOBDBitEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP:
      SetSelectedByte(FSelectedByte - 1);
    VK_DOWN:
      SetSelectedByte(FSelectedByte + 1);
    VK_SPACE:
      ToggleBit(FSelectedByte, 0);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDBitEditor.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FScrollHover := False;
  Cursor := crDefault;
  Invalidate;
end;

procedure TOBDBitEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
