unit ERD.Components.LEDView;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Math,
  Winapi.Windows, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Buttons, ERD.Components.Types, ERD.Components.Paint, ERD.Components.Hint;

type
  TERDLEDLayout = (ldlHorizontalStrip, ldlVerticalStrip, ldlRing);
  TERDLEDPixelEvent = procedure(Sender: TObject; Index: Integer; Shift: TShiftState) of object;

  /// <summary>Independent RGB strip/ring view; colors and timing are supplied by the host.</summary>
  TERDLEDView = class(TERDInteractiveControl, IERDHintProvider)
  private
    FColors: TArray<TColor>;
    FSelected: TArray<Boolean>;
    FLEDCount, FPixelSize, FSpacing, FCurrent, FAnchor, FUpdateCount: Integer;
    FLayout: TERDLEDLayout;
    FStartAngle: Single;
    FClockwise, FShowIndices, FSelectionEnabled: Boolean;
    FColorChanged, FSelectionChanged: Boolean;
    FBrightness: Byte;
    FOnChange, FOnSelectionChange: TNotifyEvent;
    FOnPixelClick: TERDLEDPixelEvent;
    procedure SetLEDCount(Value: Integer);
    procedure SetPixelSize(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetLayout(Value: TERDLEDLayout);
    procedure SetStartAngle(Value: Single);
    procedure SetClockwise(Value: Boolean);
    procedure SetShowIndices(Value: Boolean);
    procedure SetSelectionEnabled(Value: Boolean);
    procedure SetBrightness(Value: Byte);
    function GetColor(Index: Integer): TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    function GetSelected(Index: Integer): Boolean;
    function GetSelectionCount: Integer;
    procedure CheckIndex(Index: Integer);
    procedure Changed(Selection: Boolean);
    procedure FlushChanges;
  protected
    procedure Loaded; override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetColors(const Values: array of TColor);
    function Colors: TArray<TColor>;
    procedure FillColor(Value: TColor; SelectedOnly: Boolean = False);
    procedure SetSelection(const Indices: array of Integer);
    procedure SelectPixel(Index: Integer; Extend: Boolean = False; Toggle: Boolean = False);
    procedure ClearSelection;
    function SelectedIndices: TArray<Integer>;
    function PixelBounds(Index: Integer): TRectF;
    function PixelAt(X, Y: Integer): Integer;
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
    property PixelColors[Index: Integer]: TColor read GetColor write SetColor;
    property Selected[Index: Integer]: Boolean read GetSelected;
    property CurrentIndex: Integer read FCurrent;
    property SelectionCount: Integer read GetSelectionCount;
  published
    property LEDCount: Integer read FLEDCount write SetLEDCount default 16;
    property Layout: TERDLEDLayout read FLayout write SetLayout default ldlHorizontalStrip;
    property PixelSize: Integer read FPixelSize write SetPixelSize default 24;
    property Spacing: Integer read FSpacing write SetSpacing default 8;
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property Clockwise: Boolean read FClockwise write SetClockwise default True;
    property ShowIndices: Boolean read FShowIndices write SetShowIndices default False;
    property SelectionEnabled: Boolean read FSelectionEnabled write SetSelectionEnabled default True;
    property Brightness: Byte read FBrightness write SetBrightness default 255;
    property Density;
    property ParentDensity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPixelClick: TERDLEDPixelEvent read FOnPixelClick write FOnPixelClick;
  end;

implementation

constructor TERDLEDView.Create(AOwner: TComponent);
begin
  inherited;
  FLEDCount := 16;
  FPixelSize := 24;
  FSpacing := 8;
  FStartAngle := -90;
  FClockwise := True;
  FSelectionEnabled := True;
  FBrightness := 255;
  FCurrent := -1; FAnchor := -1;
  SetLength(FColors,FLEDCount); SetLength(FSelected,FLEDCount);
  Width := 520; Height := 100; TabStop := True;
  Hint := 'LED pixels|Select pixels to inspect or assign colors.';
end;
procedure TERDLEDView.Loaded;
begin
  inherited; FColorChanged := False; FSelectionChanged := False; Invalidate;
end;
procedure TERDLEDView.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FLEDCount) then
    raise EArgumentOutOfRangeException.Create('Pixel index is outside the LED view');
end;
procedure TERDLEDView.BeginUpdate;
begin Inc(FUpdateCount); end;
procedure TERDLEDView.EndUpdate;
begin
  if FUpdateCount = 0 then raise EInvalidOperation.Create('EndUpdate requires BeginUpdate');
  Dec(FUpdateCount); FlushChanges;
end;
procedure TERDLEDView.FlushChanges;
var ColorsChanged, SelectionChanged: Boolean;
begin
  if (FUpdateCount <> 0) or (csLoading in ComponentState) then Exit;
  ColorsChanged := FColorChanged; SelectionChanged := FSelectionChanged;
  FColorChanged := False; FSelectionChanged := False;
  if ColorsChanged or SelectionChanged then Invalidate;
  if ColorsChanged and Assigned(FOnChange) then FOnChange(Self);
  if SelectionChanged and Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;
procedure TERDLEDView.Changed(Selection: Boolean);
begin
  if Selection then FSelectionChanged := True else FColorChanged := True;
  FlushChanges;
end;
procedure TERDLEDView.SetLEDCount(Value: Integer);
var HadSelection: Boolean; I: Integer;
begin
  if (Value < 1) or (Value > 4096) then raise EArgumentOutOfRangeException.Create('LEDCount must be between 1 and 4096');
  if Value = FLEDCount then Exit;
  HadSelection := False;
  for I := Value to FLEDCount-1 do if FSelected[I] then HadSelection := True;
  FLEDCount := Value; SetLength(FColors,Value); SetLength(FSelected,Value);
  if FCurrent >= Value then begin FCurrent := -1; HadSelection := True; end;
  if FAnchor >= Value then FAnchor := -1;
  BeginUpdate;
  try Changed(False); if HadSelection then Changed(True); finally EndUpdate; end;
end;
procedure TERDLEDView.SetPixelSize(Value: Integer);
begin
  if (Value < 4) or (Value > 128) then raise EArgumentOutOfRangeException.Create('PixelSize must be between 4 and 128');
  if FPixelSize = Value then Exit; FPixelSize := Value; Invalidate;
end;
procedure TERDLEDView.SetSpacing(Value: Integer);
begin
  if (Value < 0) or (Value > 128) then raise EArgumentOutOfRangeException.Create('Spacing must be between 0 and 128');
  if FSpacing = Value then Exit; FSpacing := Value; Invalidate;
end;
procedure TERDLEDView.SetLayout(Value: TERDLEDLayout);
begin
  if FLayout = Value then Exit; FLayout := Value; Invalidate;
end;
procedure TERDLEDView.SetStartAngle(Value: Single);
begin
  if IsNan(Value) or IsInfinite(Value) or (Abs(Value) > 1000000) then
    raise EArgumentException.Create('StartAngle must be finite and within one million degrees');
  if FStartAngle = Value then Exit; FStartAngle := Value; Invalidate;
end;
procedure TERDLEDView.SetClockwise(Value: Boolean);
begin if FClockwise <> Value then begin FClockwise := Value; Invalidate; end; end;
procedure TERDLEDView.SetShowIndices(Value: Boolean);
begin if FShowIndices <> Value then begin FShowIndices := Value; Invalidate; end; end;
procedure TERDLEDView.SetSelectionEnabled(Value: Boolean);
begin FSelectionEnabled := Value; Invalidate; end;
procedure TERDLEDView.SetBrightness(Value: Byte);
begin if FBrightness <> Value then begin FBrightness := Value; Invalidate; end; end;
function TERDLEDView.GetColor(Index: Integer): TColor;
begin CheckIndex(Index); Result := FColors[Index]; end;
procedure TERDLEDView.SetColor(Index: Integer; Value: TColor);
begin
  CheckIndex(Index); if FColors[Index] = Value then Exit;
  FColors[Index] := Value; Changed(False);
end;
procedure TERDLEDView.SetColors(const Values: array of TColor);
var I: Integer; Any: Boolean;
begin
  if Length(Values) <> FLEDCount then raise EArgumentException.Create('Color frame must match LEDCount');
  Any := False;
  for I := 0 to FLEDCount-1 do
    if FColors[I] <> Values[I] then begin FColors[I] := Values[I]; Any := True; end;
  if Any then Changed(False);
end;
function TERDLEDView.Colors: TArray<TColor>;
begin Result := Copy(FColors); end;
procedure TERDLEDView.FillColor(Value: TColor; SelectedOnly: Boolean);
var I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to FLEDCount-1 do if not SelectedOnly or FSelected[I] then SetColor(I,Value);
  finally EndUpdate; end;
end;
function TERDLEDView.GetSelected(Index: Integer): Boolean;
begin CheckIndex(Index); Result := FSelected[Index]; end;
function TERDLEDView.GetSelectionCount: Integer;
var Value: Boolean;
begin Result := 0; for Value in FSelected do if Value then Inc(Result); end;
function TERDLEDView.SelectedIndices: TArray<Integer>;
var I, Count: Integer;
begin
  SetLength(Result,SelectionCount); Count := 0;
  for I := 0 to FLEDCount-1 do if FSelected[I] then begin Result[Count] := I; Inc(Count); end;
end;
procedure TERDLEDView.SetSelection(const Indices: array of Integer);
var Values: TArray<Boolean>; Index, I: Integer; Any: Boolean;
begin
  for Index in Indices do CheckIndex(Index);
  SetLength(Values,FLEDCount);
  for Index in Indices do Values[Index] := True;
  Any := False;
  for I := 0 to FLEDCount-1 do if Values[I] <> FSelected[I] then Any := True;
  FSelected := Values;
  if Length(Indices) > 0 then Index := Indices[High(Indices)] else Index := -1;
  Any := Any or (FCurrent <> Index); FCurrent := Index; FAnchor := Index;
  if Any then Changed(True);
end;
procedure TERDLEDView.ClearSelection;
begin SetSelection([]); end;
procedure TERDLEDView.SelectPixel(Index: Integer; Extend, Toggle: Boolean);
var I, Start: Integer; Any, Value: Boolean;
begin
  CheckIndex(Index); Any := FCurrent <> Index;
  Start := Index; if Extend and (FAnchor >= 0) then Start := FAnchor;
  for I := 0 to FLEDCount-1 do
  begin
    Value := FSelected[I];
    if Extend then
    begin
      if (I >= Min(Start,Index)) and (I <= Max(Start,Index)) then Value := True
      else if not Toggle then Value := False;
    end
    else if Toggle then begin if I = Index then Value := not Value; end
    else Value := I = Index;
    if Value <> FSelected[I] then begin FSelected[I] := Value; Any := True; end;
  end;
  FCurrent := Index; if not Extend then FAnchor := Index;
  if Any then Changed(True);
end;
function TERDLEDView.PixelBounds(Index: Integer): TRectF;
var Pad, W, H, Diameter, Gap, Ratio, TotalLength, CX, CY, Radius, Angle, Direction: Single;
begin
  CheckIndex(Index); Result := System.Types.RectF(0,0,0,0);
  Pad := ScaleValue(10); W := Max(0,ClientWidth-2*Pad); H := Max(0,ClientHeight-2*Pad);
  if (W <= 0) or (H <= 0) then Exit;
  Diameter := ScaleValue(FPixelSize); Gap := ScaleValue(FSpacing);
  CX := ClientWidth/2; CY := ClientHeight/2;
  if FLayout = ldlRing then
  begin
    if FLEDCount = 1 then Diameter := Min(Diameter,Min(W,H))
    else
    begin
      Ratio := Sin(Pi/FLEDCount);
      Diameter := Min(Diameter,Min(W,H)*Ratio/(1+Ratio+FSpacing/FPixelSize));
      Radius := (Min(W,H)-Diameter)/2;
      if FClockwise then Direction := 1 else Direction := -1;
      Angle := DegToRad(FStartAngle+Direction*Index*360/FLEDCount);
      CX := CX+Cos(Angle)*Radius; CY := CY+Sin(Angle)*Radius;
    end;
  end
  else
  begin
    TotalLength := FLEDCount*Diameter+(FLEDCount-1)*Gap;
    if FLayout = ldlHorizontalStrip then Ratio := Min(1,Min(W/TotalLength,H/Diameter))
    else Ratio := Min(1,Min(H/TotalLength,W/Diameter));
    Diameter := Diameter*Ratio; Gap := Gap*Ratio; TotalLength := TotalLength*Ratio;
    if FLayout = ldlHorizontalStrip then CX := CX-TotalLength/2+Diameter/2+Index*(Diameter+Gap)
    else CY := CY-TotalLength/2+Diameter/2+Index*(Diameter+Gap);
  end;
  Result := System.Types.RectF(CX-Diameter/2,CY-Diameter/2,CX+Diameter/2,CY+Diameter/2);
end;
function TERDLEDView.PixelAt(X, Y: Integer): Integer;
var I: Integer; R: TRectF; DX, DY: Single;
begin
  Result := -1;
  if not PtInRect(ClientRect,Point(X,Y)) then Exit;
  for I := 0 to FLEDCount-1 do
  begin
    R := PixelBounds(I); if R.IsEmpty then Continue;
    DX := X-R.CenterPoint.X; DY := Y-R.CenterPoint.Y;
    if Sqr(DX)+Sqr(DY) <= Sqr(R.Width/2) then Exit(I);
  end;
end;
procedure TERDLEDView.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter; I: Integer; R: TRectF; Ink, Fill, LabelInk: TColor;
    Raw: COLORREF; Inset, Stroke: Single;
begin
  Painter := TERDPainter.Create(ACanvas,Palette,CurrentPPI);
  try
    Painter.FillRect(ClientRect,Palette.Background);
    Stroke := CurrentPPI/96;
    for I := 0 to FLEDCount-1 do
    begin
      R := PixelBounds(I); if R.IsEmpty then Continue;
      Raw := ColorToRGB(FColors[I]);
      if FColors[I] = clNone then Fill := Palette.GaugeFace
      else Fill := RGB(GetRValue(Raw)*FBrightness div 255,GetGValue(Raw)*FBrightness div 255,GetBValue(Raw)*FBrightness div 255);
      Ink := Palette.Subtle;
      Painter.Ellipse(R.Left,R.Top,R.Width,R.Height,Palette.GaugeFace,Ink,Stroke);
      Inset := Min(3*Stroke,R.Width/5);
      Painter.Ellipse(R.Left+Inset,R.Top+Inset,R.Width-2*Inset,R.Height-2*Inset,Fill,clNone);
      if FSelected[I] then
      begin
        Painter.Ellipse(R.Left-2*Stroke,R.Top-2*Stroke,R.Width+4*Stroke,R.Height+4*Stroke,clNone,Palette.Background,3*Stroke);
        Painter.Ellipse(R.Left-2*Stroke,R.Top-2*Stroke,R.Width+4*Stroke,R.Height+4*Stroke,clNone,Palette.Accent,Stroke);
      end;
      if FShowIndices and (R.Width >= ScaleValue(18)) then
      begin
        Raw := ColorToRGB(Fill);
        if GetRValue(Raw)*299+GetGValue(Raw)*587+GetBValue(Raw)*114 > 128000 then LabelInk := clBlack else LabelInk := clWhite;
        Painter.TextF(R.CenterPoint.X,R.CenterPoint.Y,IntToStr(I),9,LabelInk,twRegular,taCenter,R.Width);
      end;
      if Focused and FSelectionEnabled and (I = FCurrent) then
        Painter.FocusRing(R.Left-4*Stroke,R.Top-4*Stroke,R.Width+8*Stroke,R.Height+8*Stroke,R.Width/2);
    end;
  finally Painter.Free; end;
end;
procedure TERDLEDView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled or not FSelectionEnabled then Exit;
  Index := PixelAt(X,Y);
  if Index < 0 then begin if not (ssCtrl in Shift) then ClearSelection; Exit; end;
  if CanFocus then SetFocus;
  SelectPixel(Index,ssShift in Shift,ssCtrl in Shift);
  if Assigned(FOnPixelClick) then FOnPixelClick(Self,Index,Shift);
end;
procedure TERDLEDView.KeyDown(var Key: Word; Shift: TShiftState);
var Index, I: Integer; Indices: TArray<Integer>;
begin
  if Enabled and FSelectionEnabled then
  begin
    if (Key = Ord('A')) and (Shift = [ssCtrl]) then
    begin
      SetLength(Indices,FLEDCount); for I := 0 to FLEDCount-1 do Indices[I] := I;
      SetSelection(Indices); Key := 0;
    end
    else if (Shift- [ssShift,ssCtrl] = []) then
    begin
      Index := FCurrent;
      case Key of
        VK_LEFT, VK_UP: if Index < 0 then Index := 0 else Index := Max(0,Index-1);
        VK_RIGHT, VK_DOWN: Index := Min(FLEDCount-1,Index+1);
        VK_HOME: Index := 0;
        VK_END: Index := FLEDCount-1;
        VK_ESCAPE: begin ClearSelection; Key := 0; end;
        VK_SPACE: if Index >= 0 then begin SelectPixel(Index,False,True); Key := 0; end;
      else Index := -1;
      end;
      if (Index >= 0) and (Key <> 0) then begin SelectPixel(Index,ssShift in Shift,False); Key := 0; end;
    end;
  end;
  inherited;
end;
function TERDLEDView.HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
var P: TPoint; Index: Integer; Raw: COLORREF;
begin
  Content := Default(TERDHintContent); P := ScreenToClient(ScreenPoint); Index := PixelAt(P.X,P.Y);
  Result := Index >= 0; if not Result then Exit;
  Raw := ColorToRGB(FColors[Index]); Content.Title := 'LED '+IntToStr(Index);
  if FColors[Index] = clNone then Content.Description := 'No color assigned'
  else Content.Description := Format('RGB %d, %d, %d',[GetRValue(Raw),GetGValue(Raw),GetBValue(Raw)]);
  Content.Description := Content.Description+sLineBreak+'Brightness '+IntToStr(FBrightness)+'/255';
  Content.Palette := Palette; Content.HasPalette := True; Content.Density := Density; Content.HasDensity := True;
  Content.Glyph := glInfo;
end;

end.
