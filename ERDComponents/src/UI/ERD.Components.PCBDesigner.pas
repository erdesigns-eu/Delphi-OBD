unit ERD.Components.PCBDesigner;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Math, System.JSON,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types;

type
  TERDPCBPad = record
    ID, Net, Reference, Number: string;
    Position: TPointF;
    Polygon, Drill: TArray<TPointF>;
  end;
  TERDPCBChannel = record
    Pin, GPIO, Route, Name, Action: string;
    LEDCount: Integer;
  end;
  TERDPCBComponent = record
    Reference, Device: string;
    Position: TPointF;
  end;
  /// Imported geometry is read-only; project assignments have a separate file.
  TERDPCBDesigner = class(TERDCustomControl)
  private
    FPads: TArray<TERDPCBPad>;
    FChannels: TArray<TERDPCBChannel>;
    FComponents: TArray<TERDPCBComponent>;
    FPaths, FOutline: TArray<TArray<TPointF>>;
    FBoardHash, FSelected, FHot: string;
    FZoom, FPanX, FPanY, FMaxX, FMaxY: Double;
    FScreenScale, FScreenX, FScreenY: Double;
    FDragging: Boolean;
    FLast: TPoint;
    FOnSelectionChange: TNotifyEvent;
    function ViewScale: Double;
    procedure UpdateTransform;
    function ScreenPoint(const Point: TPointF): TPoint;
    function PadAt(X, Y: Integer): string;
    procedure SetSelected(const Value: string);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadBoard(const FileName: string);
    procedure FitBoard;
    function SelectedPad: TERDPCBPad;
    function ChannelIndex(const Pin: string): Integer;
    function Channel(Index: Integer): TERDPCBChannel;
    procedure SetBinding(Index: Integer; const Name, Action: string; LEDCount: Integer);
    procedure SaveProject(const FileName: string);
    procedure LoadProject(const FileName: string);
    property SelectedPin: string read FSelected write SetSelected;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses System.IOUtils;

function PointValue(Value: TJSONValue): TPointF;
var A: TJSONArray;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected coordinate pair');
  A := TJSONArray(Value);
  if (A.Count <> 2) or not (A.Items[0] is TJSONNumber) or not (A.Items[1] is TJSONNumber) then
    raise EConvertError.Create('Expected two numeric coordinates');
  Result := PointF((A.Items[0] as TJSONNumber).AsDouble, (A.Items[1] as TJSONNumber).AsDouble);
end;
function PolygonValue(Value: TJSONValue): TArray<TPointF>;
var A: TJSONArray; I: Integer;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected geometry array');
  A := TJSONArray(Value); SetLength(Result, A.Count);
  for I := 0 to A.Count - 1 do Result[I] := PointValue(A.Items[I]);
end;
function PathsValue(Value: TJSONValue): TArray<TArray<TPointF>>;
var A: TJSONArray; I: Integer;
begin
  if not (Value is TJSONArray) then raise EConvertError.Create('Expected geometry array');
  A := TJSONArray(Value); SetLength(Result, A.Count);
  for I := 0 to A.Count - 1 do Result[I] := PolygonValue(A.Items[I]);
end;
constructor TERDPCBDesigner.Create(AOwner: TComponent);
begin inherited; FZoom := 1; Width := 700; Height := 400; TabStop := True; end;
procedure TERDPCBDesigner.LoadBoard(const FileName: string);
var Root: TJSONValue; Obj: TJSONObject; A: TJSONArray; I, J: Integer;
    Pads: TArray<TERDPCBPad>; Channels: TArray<TERDPCBChannel>;
    Components: TArray<TERDPCBComponent>; Paths, Outline: TArray<TArray<TPointF>>;
    Hash: string; MaxX, MaxY: Double;
begin
  Root := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName, TEncoding.UTF8));
  try
    if not (Root is TJSONObject) then raise EConvertError.Create('Invalid board package');
    if Root.GetValue<Integer>('schema') <> 1 then raise EConvertError.Create('Unsupported board schema');
    if (Root.GetValue<string>('units') <> 'mm') or (Root.GetValue<string>('y_axis') <> 'up') then
      raise EConvertError.Create('Unsupported board coordinate system');
    Hash := Root.GetValue<string>('source_sha256');
    A := Root.GetValue<TJSONArray>('pads'); SetLength(Pads, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Pads[I].ID := Obj.GetValue<string>('id'); Pads[I].Net := Obj.GetValue<string>('net');
      Pads[I].Reference := Obj.GetValue<string>('reference'); Pads[I].Number := Obj.GetValue<string>('number');
      Pads[I].Position := PointValue(Obj.GetValue('position'));
      Pads[I].Polygon := PolygonValue(Obj.GetValue('polygon'));
      Pads[I].Drill := PolygonValue(Obj.GetValue('drill'));
    end;
    A := Root.GetValue<TJSONArray>('channels'); SetLength(Channels, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Channels[I].Pin := Obj.GetValue<string>('pin'); Channels[I].GPIO := Obj.GetValue<string>('gpio');
      Channels[I].Route := Obj.GetValue<string>('route'); Channels[I].LEDCount := 1;
    end;
    A := Root.GetValue<TJSONArray>('components'); SetLength(Components, A.Count);
    for I := 0 to A.Count - 1 do
    begin
      if not (A.Items[I] is TJSONObject) then raise EConvertError.Create('Expected board object');
      Obj := TJSONObject(A.Items[I]);
      Components[I].Reference := Obj.GetValue<string>('reference');
      Components[I].Device := Obj.GetValue<string>('device');
      Components[I].Position := PointValue(Obj.GetValue('position'));
    end;
    Paths := PathsValue(Root.GetValue('silkscreen')); Outline := PathsValue(Root.GetValue('outline'));
    MaxX := 0; MaxY := 0;
    for I := 0 to High(Outline) do for J := 0 to High(Outline[I]) do
    begin MaxX := Max(MaxX, Outline[I][J].X); MaxY := Max(MaxY, Outline[I][J].Y); end;
    if (MaxX <= 0) or (MaxY <= 0) then raise EConvertError.Create('Board outline is empty');
    FPads := Pads; FChannels := Channels; FComponents := Components;
    FPaths := Paths; FOutline := Outline; FBoardHash := Hash; FMaxX := MaxX; FMaxY := MaxY;
    FSelected := ''; FHot := ''; FitBoard;
    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  finally Root.Free; end;
end;
procedure TERDPCBDesigner.FitBoard;
begin FZoom := 1; FPanX := 0; FPanY := 0; Invalidate; end;
function TERDPCBDesigner.ViewScale: Double;
begin Result := FZoom * Max(0.01, Min((ClientWidth - ScaleValue(40)) / Max(1, FMaxX),
  (ClientHeight - ScaleValue(40)) / Max(1, FMaxY))); end;
procedure TERDPCBDesigner.UpdateTransform;
begin
  // Compute once per paint/hit test, rather than querying window bounds per vertex.
  FScreenScale := ViewScale;
  FScreenX := (ClientWidth - FMaxX * FScreenScale) / 2 + FPanX;
  FScreenY := (ClientHeight + FMaxY * FScreenScale) / 2 + FPanY;
end;
function TERDPCBDesigner.ScreenPoint(const Point: TPointF): TPoint;
begin
  Result := System.Types.Point(Round(FScreenX + Point.X * FScreenScale),
    Round(FScreenY - Point.Y * FScreenScale));
end;
procedure TERDPCBDesigner.PaintControl(ACanvas: TCanvas);
var I, J: Integer; P: TPoint; Poly: TArray<TPoint>; R: TRect; S: Double;
  procedure DrawPaths(const Paths: TArray<TArray<TPointF>>; Ink: TColor);
  var A, B: Integer;
  begin
    ACanvas.Pen.Color := Ink; ACanvas.Pen.Width := 1;
    for A := 0 to High(Paths) do
    begin
      SetLength(Poly, Length(Paths[A]));
      for B := 0 to High(Poly) do Poly[B] := ScreenPoint(Paths[A][B]);
      if Length(Poly) > 1 then ACanvas.Polyline(Poly);
    end;
  end;
begin
  ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(ClientRect);
  if Length(FPads) = 0 then Exit;
  UpdateTransform; S := FScreenScale;
  P := ScreenPoint(PointF(0, FMaxY)); R := Rect(P.X, P.Y, P.X + Round(FMaxX * S), P.Y + Round(FMaxY * S));
  ACanvas.Brush.Color := RGB(25, 74, 57); ACanvas.Pen.Color := RGB(90, 130, 105);
  ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Round(4 * S), Round(4 * S));
  DrawPaths(FPaths, RGB(175, 196, 178)); DrawPaths(FOutline, RGB(210, 220, 190));
  for I := 0 to High(FPads) do
  begin
    SetLength(Poly, Length(FPads[I].Polygon));
    for J := 0 to High(Poly) do Poly[J] := ScreenPoint(FPads[I].Polygon[J]);
    ACanvas.Pen.Width := 1; ACanvas.Pen.Color := RGB(155, 131, 65);
    ACanvas.Brush.Color := RGB(211, 188, 100);
    if FPads[I].ID = FHot then ACanvas.Brush.Color := Palette.Accent;
    if FPads[I].ID = FSelected then
    begin ACanvas.Brush.Color := Palette.Accent; ACanvas.Pen.Color := clWhite; ACanvas.Pen.Width := ScaleValue(2); end;
    if Length(Poly) > 2 then ACanvas.Polygon(Poly);
    SetLength(Poly, Length(FPads[I].Drill));
    for J := 0 to High(Poly) do Poly[J] := ScreenPoint(FPads[I].Drill[J]);
    ACanvas.Brush.Color := RGB(17, 46, 35); ACanvas.Pen.Color := RGB(17, 46, 35); ACanvas.Pen.Width := 1;
    if Length(Poly) > 2 then ACanvas.Polygon(Poly);
  end;
  ACanvas.Font.Name := 'Segoe UI'; ACanvas.Font.Height := -ScaleValue(10);
  ACanvas.Font.Color := RGB(230, 240, 230); ACanvas.Brush.Style := bsClear;
  for I := 0 to High(FComponents) do
    if (S >= ScaleValue(8)) or (ChannelIndex(FComponents[I].Reference + '.2') >= 0) or (FComponents[I].Reference = 'U6') then
    begin
      P := ScreenPoint(FComponents[I].Position);
      ACanvas.TextOut(P.X + ScaleValue(3), P.Y + ScaleValue(5), FComponents[I].Reference);
    end;
end;
function TERDPCBDesigner.PadAt(X, Y: Integer): string;
var I, J, K: Integer; P, Q, Center: TPoint; Inside: Boolean; Distance, Best, DX, DY, T: Double;
begin
  UpdateTransform;
  Result := ''; Best := Sqr(ScaleValue(7));
  for I := 0 to High(FPads) do
  begin
    Inside := False; K := High(FPads[I].Polygon); Distance := MaxDouble;
    for J := 0 to High(FPads[I].Polygon) do
    begin
      P := ScreenPoint(FPads[I].Polygon[J]); Q := ScreenPoint(FPads[I].Polygon[K]);
      if ((P.Y > Y) <> (Q.Y > Y)) then
        if X < (Q.X - P.X) * (Y - P.Y) / (Q.Y - P.Y) + P.X then Inside := not Inside;
      DX := Q.X - P.X; DY := Q.Y - P.Y;
      T := EnsureRange(((X - P.X) * DX + (Y - P.Y) * DY) / Max(1, DX * DX + DY * DY), 0.0, 1.0);
      Distance := Min(Distance, Sqr(X - P.X - T * DX) + Sqr(Y - P.Y - T * DY)); K := J;
    end;
    if Inside then Distance := 0;
    Center := ScreenPoint(FPads[I].Position);
    // A tiny centre-distance tie break prefers the closest overlapping pad.
    Distance := Distance + 0.000001 * (Sqr(X - Center.X) + Sqr(Y - Center.Y));
    if Distance < Best then begin Best := Distance; Result := FPads[I].ID; end;
  end;
end;
procedure TERDPCBDesigner.SetSelected(const Value: string);
var P: TERDPCBPad; Found: Boolean;
begin
  if FSelected = Value then Exit;
  Found := Value = '';
  for P in FPads do if P.ID = Value then begin Found := True; Break; end;
  if not Found then raise EArgumentException.Create('Unknown PCB pin');
  FSelected := Value; Invalidate;
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
function TERDPCBDesigner.SelectedPad: TERDPCBPad;
var P: TERDPCBPad;
begin Result := Default(TERDPCBPad); for P in FPads do if P.ID = FSelected then Exit(P); end;
function TERDPCBDesigner.ChannelIndex(const Pin: string): Integer;
var I: Integer;
begin Result := -1; for I := 0 to High(FChannels) do if FChannels[I].Pin = Pin then Exit(I); end;
function TERDPCBDesigner.Channel(Index: Integer): TERDPCBChannel;
begin Result := FChannels[Index]; end;
procedure TERDPCBDesigner.SetBinding(Index: Integer; const Name, Action: string; LEDCount: Integer);
var I: Integer;
begin
  if (Index < 0) or (Index >= Length(FChannels)) then raise EArgumentOutOfRangeException.Create('Unknown channel');
  if (LEDCount < 1) or (LEDCount > 65535) then raise EArgumentException.Create('LED count must be 1..65535');
  if (Action <> '') and (Action <> 'showPick') then raise EArgumentException.Create('Unsupported action');
  for I := 0 to High(FChannels) do
    if (I <> Index) and (Name <> '') and SameText(Name, FChannels[I].Name) then
      raise EArgumentException.Create('Channel names must be unique');
  FChannels[Index].Name := Name; FChannels[Index].Action := Action; FChannels[Index].LEDCount := LEDCount;
  Invalidate;
end;
procedure TERDPCBDesigner.KeyDown(var Key: Word; Shift: TShiftState);
var Index: Integer;
begin
  inherited;
  if (Key = VK_HOME) then begin FitBoard; Key := 0; Exit; end;
  if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) and (Length(FChannels) > 0) then
  begin
    Index := ChannelIndex(FSelected);
    if Key in [VK_LEFT, VK_UP] then
    begin if Index < 0 then Index := 0; Index := (Index + Length(FChannels) - 1) mod Length(FChannels); end
    else Index := (Index + 1) mod Length(FChannels);
    SelectedPin := FChannels[Index].Pin; Key := 0;
  end;
end;
procedure TERDPCBDesigner.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited; SetFocus;
  if Button = mbLeft then SelectedPin := PadAt(X, Y);
  if Button = mbMiddle then begin FDragging := True; FLast := Point(X, Y); MouseCapture := True; end;
end;
procedure TERDPCBDesigner.MouseMove(Shift: TShiftState; X, Y: Integer);
var Hot: string;
begin
  inherited;
  if FDragging and (ssMiddle in Shift) then
  begin FPanX := FPanX + X - FLast.X; FPanY := FPanY + Y - FLast.Y; FLast := Point(X, Y); Invalidate; Exit; end;
  Hot := PadAt(X, Y);
  if Hot <> FHot then begin FHot := Hot; Invalidate; end;
  if Hot <> '' then Cursor := crHand else Cursor := crDefault;
end;
procedure TERDPCBDesigner.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin inherited; if Button = mbMiddle then begin FDragging := False; MouseCapture := False; end; end;
function TERDPCBDesigner.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var P: TPoint; Ratio: Double;
begin
  P := ScreenToClient(MousePos); Ratio := FZoom;
  FZoom := EnsureRange(FZoom * Power(1.2, WheelDelta / 120), 0.3, 20.0); Ratio := FZoom / Ratio;
  FPanX := P.X - ClientWidth / 2 - (P.X - ClientWidth / 2 - FPanX) * Ratio;
  FPanY := P.Y - ClientHeight / 2 - (P.Y - ClientHeight / 2 - FPanY) * Ratio;
  Invalidate; Result := True;
end;
procedure TERDPCBDesigner.SaveProject(const FileName: string);
var Root, Item: TJSONObject; Items: TJSONArray; C: TERDPCBChannel;
begin
  if FBoardHash = '' then raise EConvertError.Create('Load a board before saving a project');
  Root := TJSONObject.Create;
  try
    Root.AddPair('schema', TJSONNumber.Create(1)); Root.AddPair('board', FBoardHash);
    Root.AddPair('selected', FSelected); Items := TJSONArray.Create; Root.AddPair('bindings', Items);
    for C in FChannels do
    begin
      Item := TJSONObject.Create; Items.AddElement(Item);
      Item.AddPair('pin', C.Pin); Item.AddPair('name', C.Name); Item.AddPair('action', C.Action);
      Item.AddPair('led_count', TJSONNumber.Create(C.LEDCount));
    end;
    TFile.WriteAllText(FileName, Root.ToJSON, TEncoding.UTF8);
  finally Root.Free; end;
end;
procedure TERDPCBDesigner.LoadProject(const FileName: string);
var Root: TJSONValue; Items: TJSONArray; Item: TJSONObject; Backup: TArray<TERDPCBChannel>;
    I, Index: Integer; Selection: string; Seen: TStringList;
begin
  Root := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName, TEncoding.UTF8));
  Backup := Copy(FChannels); Seen := TStringList.Create;
  try
    try
      if not (Root is TJSONObject) then raise EConvertError.Create('Invalid PCB project');
      if (Root.GetValue<Integer>('schema') <> 1) or (Root.GetValue<string>('board') <> FBoardHash) then
        raise EConvertError.Create('Project belongs to a different board or schema');
      Selection := Root.GetValue<string>('selected');
      Items := Root.GetValue<TJSONArray>('bindings');
      if Items.Count <> Length(FChannels) then raise EConvertError.Create('Incomplete channel assignments');
      for I := 0 to High(FChannels) do begin FChannels[I].Name := ''; FChannels[I].Action := ''; FChannels[I].LEDCount := 1; end;
      for I := 0 to Items.Count - 1 do
      begin
        if not (Items.Items[I] is TJSONObject) then raise EConvertError.Create('Expected binding object');
        Item := TJSONObject(Items.Items[I]); Index := ChannelIndex(Item.GetValue<string>('pin'));
        if (Index < 0) or (Seen.IndexOf(IntToStr(Index)) >= 0) then raise EConvertError.Create('Unknown or repeated output');
        Seen.Add(IntToStr(Index));
        SetBinding(Index, Item.GetValue<string>('name'), Item.GetValue<string>('action'), Item.GetValue<Integer>('led_count'));
      end;
      if Selection = FSelected then
      begin if Assigned(FOnSelectionChange) then FOnSelectionChange(Self); end
      else SelectedPin := Selection;
    except
      FChannels := Backup;
      raise;
    end;
  finally
    Seen.Free; Root.Free;
  end;
end;
end.
