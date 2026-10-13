unit ERD.Components.Icons;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.ImgList,
  ERD.Components.Types, ERD.Components.IconCatalog;

type
  TERDIconSource = (eisNone, eisBuiltIn, eisImageList, eisGlyph);
  TERDIcon = class;
  TERDIconObserver = class(TComponent)
  private
    FIcon: TERDIcon;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  /// <summary>One explicitly selected icon source, shared by controls and items.</summary>
  TERDIcon = class(TPersistent)
  private
    FOwner: TPersistent;
    FSource: TERDIconSource;
    FBuiltIn: TERDIconId;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FGlyph: TPicture;
    FObserver: TERDIconObserver;
    FChangeLink: TChangeLink;
    procedure Changed(Sender: TObject);
    procedure SetSource(Value: TERDIconSource);
    procedure SetBuiltIn(Value: TERDIconId);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure SetGlyph(Value: TPicture);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Available(AFallback: TERDGlyph = glNone;
      AImages: TCustomImageList = nil; AIndex: Integer = -1): Boolean;
    procedure Draw(ACanvas: TCanvas; const APalette: TERDThemePalette;
      APPI, CX, CY: Integer; AColor: TColor; AScale: Single = 1;
      AEnabled: Boolean = True; AFallback: TERDGlyph = glNone;
      AImages: TCustomImageList = nil; AIndex: Integer = -1);
  published
    property Source: TERDIconSource read FSource write SetSource default eisBuiltIn;
    property BuiltIn: TERDIconId read FBuiltIn write SetBuiltIn default eiNone;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Glyph: TPicture read FGlyph write SetGlyph;
  end;

implementation

uses System.Math, ERD.Components.Paint, ERD.Components.IconData;

type
  TCollectionItemAccess = class(TCollectionItem) end;

procedure TERDIconObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FIcon <> nil) and (AComponent = FIcon.FImages) then
  begin
    FIcon.FImages := nil;
    FIcon.Changed(Self);
  end;
end;

constructor TERDIcon.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FSource := eisBuiltIn;
  FImageIndex := -1;
  FGlyph := TPicture.Create;
  FGlyph.OnChange := Changed;
  FObserver := TERDIconObserver.Create(nil);
  FObserver.FIcon := Self;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := Changed;
end;

destructor TERDIcon.Destroy;
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(FObserver);
  end;
  if FObserver <> nil then FObserver.FIcon := nil;
  FObserver.Free;
  FChangeLink.Free;
  FGlyph.Free;
  inherited;
end;

function TERDIcon.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TERDIcon.Changed(Sender: TObject);
begin
  if FOwner is TControl then
  begin
    TControl(FOwner).Perform(CM_TEXTCHANGED, 0, 0);
    TControl(FOwner).Invalidate;
  end
  else if FOwner is TCollectionItem then TCollectionItemAccess(FOwner).Changed(False);
end;

procedure TERDIcon.SetSource(Value: TERDIconSource);
begin
  if FSource = Value then Exit;
  FSource := Value; Changed(Self);
end;
procedure TERDIcon.SetBuiltIn(Value: TERDIconId);
begin
  if FBuiltIn = Value then Exit;
  FBuiltIn := Value; Changed(Self);
end;
procedure TERDIcon.SetImages(Value: TCustomImageList);
begin
  if FImages = Value then Exit;
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(FObserver);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.FreeNotification(FObserver);
    FImages.RegisterChanges(FChangeLink);
  end;
  Changed(Self);
end;
procedure TERDIcon.SetImageIndex(Value: Integer);
begin
  if FImageIndex = Value then Exit;
  FImageIndex := Value; Changed(Self);
end;
procedure TERDIcon.SetGlyph(Value: TPicture);
begin
  FGlyph.Assign(Value);
end;
procedure TERDIcon.Assign(Source: TPersistent);
var Other: TERDIcon;
begin
  if Source is TERDIcon then
  begin
    Other := TERDIcon(Source);
    FSource := Other.FSource;
    FBuiltIn := Other.FBuiltIn;
    SetImages(Other.Images);
    FImageIndex := Other.FImageIndex;
    FGlyph.Assign(Other.Glyph);
    Changed(Self);
  end
  else inherited;
end;

function TERDIcon.Available(AFallback: TERDGlyph; AImages: TCustomImageList;
  AIndex: Integer): Boolean;
begin
  Result := False;
  case FSource of
    eisBuiltIn: Result := (FBuiltIn <> eiNone) or (AFallback <> glNone);
    eisImageList:
      begin
        if FImages <> nil then begin AImages := FImages; AIndex := FImageIndex end
        else if FImageIndex >= 0 then AIndex := FImageIndex;
        Result := (AImages <> nil) and (AIndex >= 0) and (AIndex < AImages.Count);
      end;
    eisGlyph: Result := (FGlyph.Graphic <> nil) and not FGlyph.Graphic.Empty;
  end;
end;

procedure TERDIcon.Draw(ACanvas: TCanvas; const APalette: TERDThemePalette;
  APPI, CX, CY: Integer; AColor: TColor; AScale: Single; AEnabled: Boolean;
  AFallback: TERDGlyph; AImages: TCustomImageList; AIndex: Integer);
var
  Painter: TERDPainter;
  G: TERDGlyph;
  Bitmap: TBitmap;
  R: TRect;
  Size, W, H: Integer;
  Factor: Double;
begin
  if not Available(AFallback, AImages, AIndex) then Exit;
  if FSource = eisBuiltIn then
  begin
    G := AFallback;
    Painter := TERDPainter.Create(ACanvas, APalette, APPI);
    try
      if not AEnabled then AColor := Painter.DisabledText;
      if FBuiltIn <> eiNone then
        Painter.DrawIcon(ERD_ICON_CATALOG_FIRST + Ord(FBuiltIn) - 1, CX, CY, AColor, AScale)
      else
        Painter.Glyph(G, CX, CY, AColor, AScale);
    finally Painter.Free end;
    Exit;
  end;
  if APPI <= 0 then APPI := 96;
  Size := Max(1, Round(16 * APPI / 96 * AScale));
  Bitmap := TBitmap.Create;
  try
    if FSource = eisImageList then
    begin
      if FImages <> nil then begin AImages := FImages; AIndex := FImageIndex end
      else if FImageIndex >= 0 then AIndex := FImageIndex;
      Bitmap.SetSize(AImages.Width, AImages.Height);
      Bitmap.Canvas.Brush.Color := clFuchsia;
      Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
      AImages.Draw(Bitmap.Canvas, 0, 0, AIndex, AEnabled);
      Bitmap.Transparent := True;
      Bitmap.TransparentColor := clFuchsia;
      W := Bitmap.Width; H := Bitmap.Height;
    end
    else begin W := FGlyph.Width; H := FGlyph.Height end;
    if (W <= 0) or (H <= 0) then Exit;
    Factor := Size / Max(W, H);
    W := Max(1, Round(W * Factor)); H := Max(1, Round(H * Factor));
    R := Rect(CX - W div 2, CY - H div 2, CX - W div 2 + W, CY - H div 2 + H);
    if FSource = eisImageList then ACanvas.StretchDraw(R, Bitmap)
    else if AEnabled then ACanvas.StretchDraw(R, FGlyph.Graphic)
    else
    begin
      Bitmap.SetSize(W, H);
      Bitmap.Canvas.Brush.Color := clFuchsia;
      Bitmap.Canvas.FillRect(Rect(0,0,W,H));
      Bitmap.Canvas.StretchDraw(Rect(0,0,W,H), FGlyph.Graphic);
      DrawState(ACanvas.Handle, 0, nil, LPARAM(Bitmap.Handle), 0,
        R.Left, R.Top, W, H, DST_BITMAP or DSS_DISABLED);
    end;
  finally Bitmap.Free end;
end;

end.
