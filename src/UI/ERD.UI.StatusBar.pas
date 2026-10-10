//------------------------------------------------------------------------------
//  ERD.UI.StatusBar
//
//  Themed status bar for the OBD Studio application chrome.
//
//    TOBDStatusBar   left and right aligned text, status, progress and link
//                    panels with OBD Studio palette and density metrics.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.UI.StatusBar;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ImgList,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  TOBDStatusBar = class;
  TOBDStatusPanel = class;

  /// <summary>Kind of content a status panel draws.</summary>
  TOBDStatusPanelKind = (
    /// <summary>Text and optional glyph.</summary>
    spkText,
    /// <summary>Coloured dot and text.</summary>
    spkStatus,
    /// <summary>Small progress bar and optional text.</summary>
    spkProgress,
    /// <summary>Accent-coloured clickable text.</summary>
    spkLink);

  /// <summary>Panel layout side.</summary>
  TOBDStatusPanelAlignment = (
    /// <summary>Panel is laid out from the left edge.</summary>
    paLeft,
    /// <summary>Panel is laid out from the right edge.</summary>
    paRight);

  /// <summary>Panel click notification.</summary>
  /// <param name="Sender">Status bar.</param>
  /// <param name="Panel">Clicked panel.</param>
  TOBDStatusPanelEvent = procedure(Sender: TObject;
    Panel: TOBDStatusPanel) of object;

  /// <summary>One streamable status-bar panel.</summary>
  TOBDStatusPanel = class(TCollectionItem)
  strict private
    FKind: TOBDStatusPanelKind;
    FText: string;
    FGlyph: TOBDGlyph;
    FImageIndex: Integer;
    FWidth: Integer;
    FAlignment: TOBDStatusPanelAlignment;
    FStatusKind: TOBDStatusKind;
    FPosition: Integer;
    FVisible: Boolean;
    FHint: string;
    FMono: Boolean;
    FOnClick: TNotifyEvent;
    FRect: TRect;
    procedure SetKind(AValue: TOBDStatusPanelKind);
    procedure SetText(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetAlignment(AValue: TOBDStatusPanelAlignment);
    procedure SetStatusKind(AValue: TOBDStatusKind);
    procedure SetPosition(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetMono(AValue: Boolean);
  protected
    /// <summary>Returns the text for collection editors.</summary>
    /// <returns>Text or inherited display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a visible text panel.</summary>
    /// <param name="ACollection">Owning collection.</param>
    constructor Create(ACollection: TCollection); override;
    /// <summary>Copies all streamable fields from another panel.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Current client rectangle from the latest layout pass.</summary>
    property Rect: TRect read FRect write FRect;
  published
    /// <summary>Type of content drawn by the panel.</summary>
    property Kind: TOBDStatusPanelKind read FKind write SetKind default spkText;
    /// <summary>Panel text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Built-in glyph used when no image is assigned.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image-list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Fixed width in pixels; 0 sizes to content.</summary>
    property Width: Integer read FWidth write SetWidth default 0;
    /// <summary>Left or right aligned layout group.</summary>
    property Alignment: TOBDStatusPanelAlignment read FAlignment
      write SetAlignment default paLeft;
    /// <summary>Status colour for status dots and progress bars.</summary>
    property StatusKind: TOBDStatusKind read FStatusKind write SetStatusKind
      default skNeutral;
    /// <summary>Progress position 0..100, or -1 for indeterminate.</summary>
    property Position: Integer read FPosition write SetPosition default 0;
    /// <summary>Whether the panel participates in layout and hit testing.</summary>
    property Visible: Boolean read FVisible write SetVisible default True;
    /// <summary>Per-panel hint text.</summary>
    property Hint: string read FHint write SetHint;
    /// <summary>Draws text with the mono font.</summary>
    property Mono: Boolean read FMono write SetMono default False;
    /// <summary>Fires when the panel is clicked.</summary>
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  /// <summary>Owned collection of status panels.</summary>
  TOBDStatusPanels = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TOBDStatusPanel;
    procedure SetItem(AIndex: Integer; AValue: TOBDStatusPanel);
  protected
    /// <summary>Invalidates the owner when contents change.</summary>
    /// <param name="Item">Changed item, or nil for bulk changes.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates the panel collection.</summary>
    /// <param name="AOwner">Owning persistent.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a panel.</summary>
    /// <returns>New panel.</returns>
    function Add: TOBDStatusPanel;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TOBDStatusPanel read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed OBD Studio status bar.</summary>
  TOBDStatusBar = class(TOBDCustomControl)
  strict private
    FPanels: TOBDStatusPanels;
    FImages: TCustomImageList;
    FHoverIndex: Integer;
    FOnPanelClick: TOBDStatusPanelEvent;
    FPreviewPaint: Boolean;
    procedure SetPanels(AValue: TOBDStatusPanels);
    procedure SetImages(AValue: TCustomImageList);
    function BarHeight: Integer;
    function PreviewMode: Boolean;
    function EffectiveCount: Integer;
    function EffectiveKind(AIndex: Integer): TOBDStatusPanelKind;
    function EffectiveText(AIndex: Integer): string;
    function EffectiveGlyph(AIndex: Integer): TOBDGlyph;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveWidth(AIndex: Integer): Integer;
    function EffectiveAlignment(AIndex: Integer): TOBDStatusPanelAlignment;
    function EffectiveStatusKind(AIndex: Integer): TOBDStatusKind;
    function EffectivePosition(AIndex: Integer): Integer;
    function EffectiveVisible(AIndex: Integer): Boolean;
    function EffectiveHint(AIndex: Integer): string;
    function EffectiveMono(AIndex: Integer): Boolean;
    function TextSize: Single;
    function PanelNaturalWidth(APainter: TOBDPainter; AIndex: Integer): Integer;
    procedure PanelsChanged;
    procedure LayoutPanels(APainter: TOBDPainter);
    procedure DrawPanel(APainter: TOBDPainter; ACanvas: TCanvas;
      AIndex: Integer);
    procedure DrawSizeGrip(APainter: TOBDPainter);
    function PanelAt(X, Y: Integer): Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    /// <summary>Clears the image list reference when it is freed.</summary>
    /// <param name="AComponent">Component inserted or removed.</param>
    /// <param name="Operation">Insert or remove.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    /// <summary>Paints the status bar.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Updates hover state.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Fires panel click events.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    /// <summary>Creates a bottom-aligned status bar.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the panel collection.</summary>
    destructor Destroy; override;
    /// <summary>Applies the status-bar height from the current density.</summary>
    procedure DensityChanged; override;
  published
    /// <summary>Panels in display order.</summary>
    property Panels: TOBDStatusPanels read FPanels write SetPanels;
    /// <summary>Optional images used before built-in glyphs.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Desktop or tablet status-bar height.</summary>
    property Density;
    /// <summary>Whether the control follows the parent theme density.</summary>
    property ParentDensity;
    /// <summary>Fires after any panel is clicked.</summary>
    property OnPanelClick: TOBDStatusPanelEvent read FOnPanelClick
      write FOnPanelClick;
    /// <summary>Status bars are normally aligned to the bottom.</summary>
    property Align default alBottom;
    /// <summary>Height follows <see cref="Metrics.StatusBar"/>.</summary>
    property Height default 26;
  end;

implementation

const
  PREVIEW_COUNT = 6;

{ TOBDStatusPanel ------------------------------------------------------------ }

constructor TOBDStatusPanel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
  FVisible := True;
  FStatusKind := skNeutral;
end;

procedure TOBDStatusPanel.Assign(Source: TPersistent);
var
  Panel: TOBDStatusPanel;
begin
  if Source is TOBDStatusPanel then
  begin
    Panel := TOBDStatusPanel(Source);
    FKind := Panel.FKind;
    FText := Panel.FText;
    FGlyph := Panel.FGlyph;
    FImageIndex := Panel.FImageIndex;
    FWidth := Panel.FWidth;
    FAlignment := Panel.FAlignment;
    FStatusKind := Panel.FStatusKind;
    FPosition := Panel.FPosition;
    FVisible := Panel.FVisible;
    FHint := Panel.FHint;
    FMono := Panel.FMono;
    FOnClick := Panel.FOnClick;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDStatusPanel.GetDisplayName: string;
begin
  Result := FText;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDStatusPanel.SetKind(AValue: TOBDStatusPanelKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetAlignment(AValue: TOBDStatusPanelAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetStatusKind(AValue: TOBDStatusKind);
begin
  if FStatusKind = AValue then
    Exit;
  FStatusKind := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetPosition(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, 100);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TOBDStatusPanel.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  Changed(False);
end;

{ TOBDStatusPanels ----------------------------------------------------------- }

constructor TOBDStatusPanels.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDStatusPanel);
end;

function TOBDStatusPanels.Add: TOBDStatusPanel;
begin
  Result := TOBDStatusPanel(inherited Add);
end;

function TOBDStatusPanels.GetItem(AIndex: Integer): TOBDStatusPanel;
begin
  Result := TOBDStatusPanel(inherited GetItem(AIndex));
end;

procedure TOBDStatusPanels.SetItem(AIndex: Integer; AValue: TOBDStatusPanel);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TOBDStatusPanels.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TOBDStatusBar then
    TOBDStatusBar(GetOwner).PanelsChanged;
end;

{ TOBDStatusBar -------------------------------------------------------------- }

constructor TOBDStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanels := TOBDStatusPanels.Create(Self);
  FHoverIndex := -1;
  Align := alBottom;
  ShowHint := True;
  Height := BarHeight;
  Width := ScaleValue(600);
end;

destructor TOBDStatusBar.Destroy;
begin
  FPanels.Free;
  inherited Destroy;
end;

procedure TOBDStatusBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

procedure TOBDStatusBar.DensityChanged;
begin
  Height := BarHeight;
  inherited DensityChanged;
end;

procedure TOBDStatusBar.SetPanels(AValue: TOBDStatusPanels);
begin
  FPanels.Assign(AValue);
end;

procedure TOBDStatusBar.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if FImages <> nil then
    FImages.RemoveFreeNotification(Self);
  FImages := AValue;
  if FImages <> nil then
    FImages.FreeNotification(Self);
  Invalidate;
end;

function TOBDStatusBar.BarHeight: Integer;
begin
  Result := ScaleValue(Metrics.StatusBar);
end;

function TOBDStatusBar.PreviewMode: Boolean;
begin
  Result := FPreviewPaint or ((FPanels.Count = 0) and IsPreview);
end;

function TOBDStatusBar.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FPanels.Count;
end;

function TOBDStatusBar.EffectiveKind(AIndex: Integer): TOBDStatusPanelKind;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Kind
  else
    case AIndex of
      0: Result := spkStatus;
      4: Result := spkProgress;
    else
      Result := spkText;
    end;
end;

function TOBDStatusBar.EffectiveText(AIndex: Integer): string;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Text
  else
    case AIndex of
      0: Result := 'Connected';
      1: Result := 'ELM327 v2.2 ' + WideChar($00B7) + ' COM4';
      2: Result := 'CAN 11/500';
      3: Result := '12.6 V';
      4: Result := 'Reading 7E9 (gearbox)' + WideChar($2026);
    else
      Result := '5 codes  ' + WideChar($00B7) + '  MIL on';
    end;
end;

function TOBDStatusBar.EffectiveGlyph(AIndex: Integer): TOBDGlyph;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Glyph
  else if AIndex = 3 then
    Result := glBattery
  else
    Result := glNone;
end;

function TOBDStatusBar.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FPanels[AIndex].ImageIndex;
end;

function TOBDStatusBar.EffectiveWidth(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := 0
  else
    Result := FPanels[AIndex].Width;
end;

function TOBDStatusBar.EffectiveAlignment(AIndex: Integer): TOBDStatusPanelAlignment;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Alignment
  else if AIndex >= 4 then
    Result := paRight
  else
    Result := paLeft;
end;

function TOBDStatusBar.EffectiveStatusKind(AIndex: Integer): TOBDStatusKind;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].StatusKind
  else if AIndex = 0 then
    Result := skSuccess
  else
    Result := skAccent;
end;

function TOBDStatusBar.EffectivePosition(AIndex: Integer): Integer;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Position
  else if AIndex = 4 then
    Result := 60
  else
    Result := 0;
end;

function TOBDStatusBar.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FPanels[AIndex].Visible;
end;

function TOBDStatusBar.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := EffectiveText(AIndex)
  else
    Result := FPanels[AIndex].Hint;
end;

function TOBDStatusBar.EffectiveMono(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := AIndex = 3
  else
    Result := FPanels[AIndex].Mono;
end;

function TOBDStatusBar.TextSize: Single;
begin
  if Density = dnTablet then
    Result := 13
  else
    Result := 12;
end;

function TOBDStatusBar.PanelNaturalWidth(APainter: TOBDPainter;
  AIndex: Integer): Integer;
var
  Weight: TOBDTextWeight;
begin
  if EffectiveWidth(AIndex) > 0 then
  begin
    Result := ScaleValue(EffectiveWidth(AIndex));
    Exit;
  end;
  if EffectiveMono(AIndex) then
    Weight := twMono
  else if EffectiveKind(AIndex) in [spkStatus, spkLink] then
    Weight := twSemibold
  else
    Weight := twRegular;
  Result := APainter.TextWidth(EffectiveText(AIndex), TextSize, Weight) +
    ScaleValue(24);
  if EffectiveGlyph(AIndex) <> glNone then
    Inc(Result, ScaleValue(18));
  if EffectiveKind(AIndex) = spkStatus then
    Inc(Result, ScaleValue(14));
  if EffectiveKind(AIndex) = spkProgress then
    Inc(Result, ScaleValue(132));
end;

procedure TOBDStatusBar.PanelsChanged;
begin
  FHoverIndex := -1;
  Invalidate;
end;

procedure TOBDStatusBar.LayoutPanels(APainter: TOBDPainter);
var
  I, X, RX, W: Integer;
begin
  X := ScaleValue(12);
  RX := Width - ScaleValue(12);
  for I := 0 to EffectiveCount - 1 do
    if (I < FPanels.Count) then
      FPanels[I].Rect := Rect(0, 0, 0, 0);
  for I := 0 to EffectiveCount - 1 do
    if EffectiveVisible(I) and (EffectiveAlignment(I) = paLeft) then
    begin
      W := PanelNaturalWidth(APainter, I);
      if not PreviewMode then
        FPanels[I].Rect := Rect(X, 0, X + W, Height);
      Inc(X, W);
    end;
  for I := EffectiveCount - 1 downto 0 do
    if EffectiveVisible(I) and (EffectiveAlignment(I) = paRight) then
    begin
      W := PanelNaturalWidth(APainter, I);
      if not PreviewMode then
        FPanels[I].Rect := Rect(RX - W, 0, RX, Height);
      Dec(RX, W);
    end;
end;

procedure TOBDStatusBar.DrawPanel(APainter: TOBDPainter; ACanvas: TCanvas;
  AIndex: Integer);
var
  R: TRect;
  X, CY, Img, BarW, BarX, Pos, TextW: Integer;
  Ink, C: TColor;
  Weight: TOBDTextWeight;
  Kind: TOBDStatusPanelKind;
  Txt: string;
begin
  R := FPanels[AIndex].Rect;
  Kind := EffectiveKind(AIndex);
  Txt := EffectiveText(AIndex);
  CY := Height div 2;
  X := R.Left + ScaleValue(12);
  if R.Left > ScaleValue(12) then
    APainter.VLine(R.Left, ScaleValue(6), Height - ScaleValue(12),
      Palette.NeutralLight);

  if Kind = spkStatus then
  begin
    C := APainter.StatusColor(EffectiveStatusKind(AIndex));
    APainter.Ellipse(X, CY - ScaleValue(4), ScaleValue(8), ScaleValue(8), C,
      clNone);
    Inc(X, ScaleValue(14));
    Ink := C;
    if EffectiveStatusKind(AIndex) = skSuccess then
      Ink := Palette.ForegroundText;
    APainter.Text(X, CY, Txt, TextSize, Ink, twSemibold, taLeftJustify,
      R.Right - X - ScaleValue(8));
    Exit;
  end;

  Img := EffectiveImageIndex(AIndex);
  if (FImages <> nil) and (Img >= 0) and (Img < FImages.Count) then
  begin
    FImages.Draw(ACanvas, X, CY - FImages.Height div 2, Img, True);
    Inc(X, FImages.Width + ScaleValue(6));
  end
  else if EffectiveGlyph(AIndex) <> glNone then
  begin
    APainter.Glyph(EffectiveGlyph(AIndex), X + ScaleValue(7), CY,
      Palette.Subtle, 0.75);
    Inc(X, ScaleValue(18));
  end;

  if Kind = spkProgress then
  begin
    BarW := ScaleValue(120);
    BarX := R.Right - ScaleValue(12) - BarW;
    TextW := BarX - X - ScaleValue(10);
    APainter.Text(BarX - ScaleValue(10), CY, Txt, TextSize,
      Palette.ForegroundText, twRegular, taRightJustify, TextW);
    APainter.FillRect(Rect(BarX, CY - ScaleValue(3), BarX + BarW,
      CY + ScaleValue(3)), Palette.NeutralLight);
    Pos := EffectivePosition(AIndex);
    if Pos < 0 then
      APainter.FillRect(Rect(BarX + BarW div 3, CY - ScaleValue(3),
        BarX + BarW div 3 + BarW div 4, CY + ScaleValue(3)),
        APainter.StatusColor(EffectiveStatusKind(AIndex)))
    else
      APainter.FillRect(Rect(BarX, CY - ScaleValue(3),
        BarX + MulDiv(BarW, Pos, 100), CY + ScaleValue(3)),
        APainter.StatusColor(EffectiveStatusKind(AIndex)));
    Exit;
  end;

  if Kind = spkLink then
  begin
    Ink := APainter.AccentText;
    Weight := twSemibold;
  end
  else
  begin
    Ink := Palette.GaugeLabel;
    if EffectiveMono(AIndex) then
      Weight := twMono
    else
      Weight := twRegular;
  end;
  APainter.Text(X, CY, Txt, TextSize, Ink, Weight, taLeftJustify,
    R.Right - X - ScaleValue(8));
end;

procedure TOBDStatusBar.DrawSizeGrip(APainter: TOBDPainter);
var
  Form: TCustomForm;
  X, Y, I: Integer;
begin
  Form := GetParentForm(Self);
  if (Form = nil) or (Form.BorderStyle in [bsNone, bsDialog]) or
    (Form.WindowState = wsMaximized) then
    Exit;
  X := Width - ScaleValue(13);
  Y := Height - ScaleValue(6);
  for I := 0 to 2 do
  begin
    APainter.Ellipse(X + I * ScaleValue(4), Y, ScaleValue(2), ScaleValue(2),
      Palette.Subtle, clNone);
    APainter.Ellipse(X + I * ScaleValue(4), Y - ScaleValue(4),
      ScaleValue(2), ScaleValue(2), Palette.Subtle, clNone);
  end;
end;

function TOBDStatusBar.PanelAt(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FPanels.Count - 1 do
    if FPanels[I].Visible and PtInRect(FPanels[I].Rect, Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TOBDStatusBar.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  I: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.HLine(0, 0, Width, Palette.NeutralLight);
    if PreviewMode then
    begin
      FPreviewPaint := True;
      try
        FPanels.Clear;
        for I := 0 to PREVIEW_COUNT - 1 do
          FPanels.Add;
        LayoutPanels(P);
        for I := 0 to PREVIEW_COUNT - 1 do
          DrawPanel(P, ACanvas, I);
        FPanels.Clear;
      finally
        FPreviewPaint := False;
      end;
    end
    else
    begin
      LayoutPanels(P);
      for I := 0 to FPanels.Count - 1 do
        if FPanels[I].Visible then
          DrawPanel(P, ACanvas, I);
    end;
    DrawSizeGrip(P);
  finally
    P.Free;
  end;
end;

procedure TOBDStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Old: Integer;
begin
  inherited;
  Old := FHoverIndex;
  FHoverIndex := PanelAt(X, Y);
  if Old <> FHoverIndex then
    Invalidate;
end;

procedure TOBDStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  I := PanelAt(X, Y);
  if I < 0 then
    Exit;
  if Assigned(FOnPanelClick) then
    FOnPanelClick(Self, FPanels[I]);
  if Assigned(FPanels[I].OnClick) then
    FPanels[I].OnClick(FPanels[I]);
end;

procedure TOBDStatusBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TOBDStatusBar.CMHintShow(var Message: TCMHintShow);
var
  I: Integer;
  S: string;
  P: TPoint;
begin
  inherited;
  if (Message.HintInfo = nil) or not ShowHint then
    Exit;
  P := Message.HintInfo^.CursorPos;
  I := PanelAt(P.X, P.Y);
  if I < 0 then
    Exit;
  S := EffectiveHint(I);
  if S = '' then
    S := EffectiveText(I);
  Message.HintInfo^.HintStr := S;
end;

end.
