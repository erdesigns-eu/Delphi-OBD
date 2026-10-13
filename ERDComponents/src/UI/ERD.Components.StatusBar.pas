//------------------------------------------------------------------------------
//  ERD.Components.StatusBar
//
//  Themed status bar for the Application application chrome.
//
//    TERDStatusBar   left and right aligned text, status, progress and link
//                    panels with Application palette and density metrics.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.StatusBar;

interface

uses
  ERD.Components.Icons,
  ERD.Components.Types,
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
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.I18n;

type
  TERDStatusBar = class;
  TERDStatusPanel = class;

  /// <summary>Kind of content a status panel draws.</summary>
  TERDStatusPanelKind = (
    /// <summary>Text and optional glyph.</summary>
    spkText,
    /// <summary>Coloured dot and text.</summary>
    spkStatus,
    /// <summary>Small progress bar and optional text.</summary>
    spkProgress,
    /// <summary>Accent-coloured clickable text.</summary>
    spkLink,
    spkImage,
    spkCustom,
    spkControl);

  /// <summary>Panel layout side.</summary>
  TERDStatusPanelAlignment = (
    /// <summary>Panel is laid out from the left edge.</summary>
    paLeft,
    /// <summary>Panel is laid out from the right edge.</summary>
    paRight);

  /// <summary>Panel click notification.</summary>
  /// <param name="Sender">Status bar.</param>
  /// <param name="Panel">Clicked panel.</param>
  TERDStatusPanelEvent = procedure(Sender: TObject;
    Panel: TERDStatusPanel) of object;

  /// <summary>One streamable status-bar panel.</summary>
  TERDStatusPanel = class(TCollectionItem)
  private
    FControl: TControl;
    FOriginalParent: TWinControl;
    FOriginalBounds: TRect;
    FOriginalAlign: TAlign;
    FOriginalVisible: Boolean;
    FKind: TERDStatusPanelKind;
    FText: string;
    FIcon: TERDIcon;
    FGlyph: TERDGlyph;
    FImageIndex: Integer;
    FWidth: Integer;
    FAlignment: TERDStatusPanelAlignment;
    FStatusKind: TERDStatusKind;
    FPosition: Integer;
    FVisible: Boolean;
    FHint: string;
    FMono: Boolean;
    FOnClick: TNotifyEvent;
    FRect: TRect;
    procedure SetControl(Value: TControl);
    procedure RestoreControl;
    procedure SetKind(AValue: TERDStatusPanelKind);
    procedure SetText(const AValue: string);
    procedure SetIcon(Value: TERDIcon);
    procedure SetGlyph(AValue: TERDGlyph);
    procedure SetImageIndex(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetAlignment(AValue: TERDStatusPanelAlignment);
    procedure SetStatusKind(AValue: TERDStatusKind);
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
    destructor Destroy; override;
    /// <summary>Copies all streamable fields from another panel.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Current client rectangle from the latest layout pass.</summary>
    property Rect: TRect read FRect write FRect;
  published
    /// <summary>Explicit built-in, ImageList or custom glyph source.</summary>
    /// <summary>Optional hosted control; use TERDPanel for multiple children.</summary>
    property Control: TControl read FControl write SetControl;
    property Icon: TERDIcon read FIcon write SetIcon;
    /// <summary>Type of content drawn by the panel.</summary>
    property Kind: TERDStatusPanelKind read FKind write SetKind default spkText;
    /// <summary>Panel text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Built-in glyph used when no image is assigned.</summary>
    property Glyph: TERDGlyph read FGlyph write SetGlyph default glNone;
    /// <summary>Image-list index; -1 uses <see cref="Glyph"/>.</summary>
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    /// <summary>Fixed width in pixels; 0 sizes to content.</summary>
    property Width: Integer read FWidth write SetWidth default 0;
    /// <summary>Left or right aligned layout group.</summary>
    property Alignment: TERDStatusPanelAlignment read FAlignment
      write SetAlignment default paLeft;
    /// <summary>Status colour for status dots and progress bars.</summary>
    property StatusKind: TERDStatusKind read FStatusKind write SetStatusKind
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
  TERDStatusPanelDrawEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Panel: TERDStatusPanel; const Bounds: TRect) of object;

  TERDStatusPanels = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TERDStatusPanel;
    procedure SetItem(AIndex: Integer; AValue: TERDStatusPanel);
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
    function OwnerBar: TERDStatusBar;
    function Add: TERDStatusPanel;
    /// <summary>Typed indexed access.</summary>
    property Items[AIndex: Integer]: TERDStatusPanel read GetItem
      write SetItem; default;
  end;

  /// <summary>Themed Application status bar.</summary>
  TERDStatusBar = class(TERDCustomControl)
  strict private
    FAutoHeight: Boolean;
    FPanels: TERDStatusPanels;
    FImages: TCustomImageList;
    FHoverIndex: Integer;
    FOnPanelClick: TERDStatusPanelEvent;
    FOnDrawPanel: TERDStatusPanelDrawEvent;
    FPreviewPaint: Boolean;
    procedure SetPanels(AValue: TERDStatusPanels);
    procedure SetImages(AValue: TCustomImageList);
    function BarHeight: Integer;
    function PreviewMode: Boolean;
    function EffectiveCount: Integer;
    function EffectiveKind(AIndex: Integer): TERDStatusPanelKind;
    function EffectiveText(AIndex: Integer): string;
    function EffectiveGlyph(AIndex: Integer): TERDGlyph;
    function EffectiveImageIndex(AIndex: Integer): Integer;
    function EffectiveWidth(AIndex: Integer): Integer;
    function EffectiveAlignment(AIndex: Integer): TERDStatusPanelAlignment;
    function EffectiveStatusKind(AIndex: Integer): TERDStatusKind;
    function EffectivePosition(AIndex: Integer): Integer;
    function EffectiveVisible(AIndex: Integer): Boolean;
    function EffectiveHint(AIndex: Integer): string;
    function EffectiveMono(AIndex: Integer): Boolean;
    function TextSize: Single;
    function PanelNaturalWidth(APainter: TERDPainter; AIndex: Integer): Integer;
    procedure LayoutPanels(APainter: TERDPainter);
    procedure DrawPanel(APainter: TERDPainter; ACanvas: TCanvas;
      AIndex: Integer);
    procedure DrawSizeGrip(APainter: TERDPainter);
    function SizeGripVisible: Boolean;
    function SizeGripRect: TRect;
    function PanelAt(X, Y: Integer): Integer;
    procedure LanguageChanged(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  private
    procedure SetAutoHeight(Value: Boolean);
    procedure UpdateHostedControls;
    procedure PanelsChanged;
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
    /// <summary>Starts sizing the form from the size grip.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Mouse X.</param>
    /// <param name="Y">Mouse Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
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
    /// <summary>Draws a custom panel inside its allocated bounds.</summary>
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property OnDrawPanel: TERDStatusPanelDrawEvent read FOnDrawPanel write FOnDrawPanel;
    /// <summary>Panels in display order.</summary>
    property Panels: TERDStatusPanels read FPanels write SetPanels;
    /// <summary>Optional images used before built-in glyphs.</summary>
    property Images: TCustomImageList read FImages write SetImages;
    /// <summary>Desktop or tablet status-bar height.</summary>
    property Density;
    /// <summary>Whether the control follows the parent theme density.</summary>
    property ParentDensity;
    /// <summary>Fires after any panel is clicked.</summary>
    property OnPanelClick: TERDStatusPanelEvent read FOnPanelClick
      write FOnPanelClick;
    /// <summary>Status bars are normally aligned to the bottom.</summary>
    property Align default alBottom;
    /// <summary>Height follows <see cref="Metrics.StatusBar"/>.</summary>
    property Height default 26;
  end;

implementation

const
  PREVIEW_COUNT = 6;

{ TERDStatusPanel ------------------------------------------------------------ }

destructor TERDStatusPanel.Destroy;
begin
  RestoreControl;
  FIcon.Free;
  inherited;
end;

procedure TERDStatusPanel.RestoreControl;
var Bar: TERDStatusBar; I: Integer; SharedParent: Boolean;
begin
  if FControl = nil then Exit;
  Bar := TERDStatusPanels(Collection).OwnerBar;
  FControl.RemoveFreeNotification(Bar);
  SharedParent := False;
  if Bar.Panels <> nil then
    for I := 0 to Bar.Panels.Count - 1 do
      if (Bar.Panels[I] <> Self) and (Bar.Panels[I].FOriginalParent = FOriginalParent) then
        SharedParent := True;
  if (FOriginalParent <> nil) and not SharedParent then
    FOriginalParent.RemoveFreeNotification(Bar);
  FControl.Parent := FOriginalParent;
  FControl.Align := FOriginalAlign;
  FControl.BoundsRect := FOriginalBounds;
  FControl.Visible := FOriginalVisible;
  FControl := nil;
  FOriginalParent := nil;
end;

procedure TERDStatusPanel.SetControl(Value: TControl);
var Bar: TERDStatusBar; Cursor: TWinControl; I: Integer;
begin
  if FControl = Value then Exit;
  Bar := TERDStatusPanels(Collection).OwnerBar;
  if Value <> nil then
  begin
    Cursor := Bar;
    while Cursor <> nil do
    begin
      if Cursor = Value then raise EInvalidOperation.Create('A status panel cannot host its ancestor');
      Cursor := Cursor.Parent;
    end;
    for I := 0 to Bar.Panels.Count - 1 do
      if (Bar.Panels[I] <> Self) and (Bar.Panels[I].Control = Value) then
        raise EInvalidOperation.Create('Control already belongs to another status panel');
    if Value.Parent is TERDStatusBar then
      for I := 0 to TERDStatusBar(Value.Parent).Panels.Count - 1 do
        if TERDStatusBar(Value.Parent).Panels[I].Control = Value then
          raise EInvalidOperation.Create('Control already belongs to another status bar');
  end;
  RestoreControl;
  FControl := Value;
  if Value <> nil then
  begin
    FOriginalParent := Value.Parent;
    FOriginalBounds := Value.BoundsRect;
    FOriginalAlign := Value.Align;
    FOriginalVisible := Value.Visible;
    Value.FreeNotification(Bar);
    if FOriginalParent <> nil then FOriginalParent.FreeNotification(Bar);
    Value.Align := alNone;
    Value.Parent := Bar;
    FKind := spkControl;
  end;
  Changed(False);
end;

function TERDStatusPanels.OwnerBar: TERDStatusBar;
begin
  Result := TERDStatusBar(GetOwner);
end;

procedure TERDStatusPanel.SetIcon(Value: TERDIcon);
begin
  FIcon.Assign(Value);
end;

constructor TERDStatusPanel.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIcon := TERDIcon.Create(Self);
  FImageIndex := -1;
  FVisible := True;
  FStatusKind := skNeutral;
end;

procedure TERDStatusPanel.Assign(Source: TPersistent);
var
  Panel: TERDStatusPanel;
begin
  if Source is TERDStatusPanel then
  begin
    Panel := TERDStatusPanel(Source);
    if Panel = Self then Exit;
    RestoreControl;
    FKind := Panel.FKind;
    FText := Panel.FText;
    FGlyph := Panel.FGlyph;
    FIcon.Assign(Panel.Icon);
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

function TERDStatusPanel.GetDisplayName: string;
begin
  Result := FText;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TERDStatusPanel.SetKind(AValue: TERDStatusPanelKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetGlyph(AValue: TERDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetAlignment(AValue: TERDStatusPanelAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetStatusKind(AValue: TERDStatusKind);
begin
  if FStatusKind = AValue then
    Exit;
  FStatusKind := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetPosition(AValue: Integer);
begin
  AValue := EnsureRange(AValue, -1, 100);
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetHint(const AValue: string);
begin
  if FHint = AValue then
    Exit;
  FHint := AValue;
  Changed(False);
end;

procedure TERDStatusPanel.SetMono(AValue: Boolean);
begin
  if FMono = AValue then
    Exit;
  FMono := AValue;
  Changed(False);
end;

{ TERDStatusPanels ----------------------------------------------------------- }

constructor TERDStatusPanels.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TERDStatusPanel);
end;

function TERDStatusPanels.Add: TERDStatusPanel;
begin
  Result := TERDStatusPanel(inherited Add);
end;

function TERDStatusPanels.GetItem(AIndex: Integer): TERDStatusPanel;
begin
  Result := TERDStatusPanel(inherited GetItem(AIndex));
end;

procedure TERDStatusPanels.SetItem(AIndex: Integer; AValue: TERDStatusPanel);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TERDStatusPanels.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TERDStatusBar then
    TERDStatusBar(GetOwner).PanelsChanged;
end;

{ TERDStatusBar -------------------------------------------------------------- }

constructor TERDStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FAutoHeight := True;
  FPanels := TERDStatusPanels.Create(Self);
  FHoverIndex := -1;
  Align := alBottom;
  ShowHint := True;
  Height := BarHeight;
  Width := ScaleValue(600);
  ERDI18n.AddChangeListener(LanguageChanged);
end;

destructor TERDStatusBar.Destroy;
begin
  ERDI18n.RemoveChangeListener(LanguageChanged);
  FreeAndNil(FPanels);
  inherited Destroy;
end;

procedure TERDStatusBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var I: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
  if (Operation = opRemove) and (FPanels <> nil) then
    for I := 0 to FPanels.Count - 1 do
    begin
      if AComponent = FPanels[I].FControl then FPanels[I].FControl := nil;
      if AComponent = FPanels[I].FOriginalParent then FPanels[I].FOriginalParent := nil;
    end;
end;

procedure TERDStatusBar.DensityChanged;
begin
  Height := BarHeight;
  inherited DensityChanged;
end;

procedure TERDStatusBar.SetPanels(AValue: TERDStatusPanels);
begin
  FPanels.Assign(AValue);
end;

procedure TERDStatusBar.SetImages(AValue: TCustomImageList);
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

function TERDStatusBar.BarHeight: Integer;
var I: Integer;
begin
  Result := ScaleValue(Metrics.StatusBar);
  if FPanels <> nil then
    for I := 0 to FPanels.Count - 1 do
      if FPanels[I].Visible and (FPanels[I].Kind = spkControl) and
        (FPanels[I].Control <> nil) then
        Result := System.Math.Max(Result, FPanels[I].FOriginalBounds.Height + ScaleValue(6));
end;

procedure TERDStatusBar.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight = Value then Exit;
  FAutoHeight := Value;
  if Value then Height := BarHeight;
  Invalidate;
end;

procedure TERDStatusBar.LanguageChanged(Sender: TObject);
begin
  Invalidate;
end;

function TERDStatusBar.PreviewMode: Boolean;
begin
  Result := FPreviewPaint or ((FPanels.Count = 0) and IsPreview);
end;

function TERDStatusBar.EffectiveCount: Integer;
begin
  if PreviewMode then
    Result := PREVIEW_COUNT
  else
    Result := FPanels.Count;
end;

function TERDStatusBar.EffectiveKind(AIndex: Integer): TERDStatusPanelKind;
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

function TERDStatusBar.EffectiveText(AIndex: Integer): string;
begin
  if not PreviewMode then Result := FPanels[AIndex].Text
  else case AIndex of
    0: Result := Tr('ui.status.preview.ready', 'Ready');
    1: Result := Tr('ui.status.preview.document', 'Untitled document');
    2: Result := Tr('ui.status.preview.selection', 'Selection: 0 items');
    3: Result := '100%';
    4: Result := Tr('ui.status.preview.processing', 'Processing');
  else Result := Tr('ui.status.preview.help', 'Help');
  end;
end;

function TERDStatusBar.EffectiveGlyph(AIndex: Integer): TERDGlyph;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Glyph
  else if AIndex = 3 then
    Result := glInfo
  else
    Result := glNone;
end;

function TERDStatusBar.EffectiveImageIndex(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := -1
  else
    Result := FPanels[AIndex].ImageIndex;
end;

function TERDStatusBar.EffectiveWidth(AIndex: Integer): Integer;
begin
  if PreviewMode then
    Result := 0
  else
    Result := FPanels[AIndex].Width;
end;

function TERDStatusBar.EffectiveAlignment(AIndex: Integer): TERDStatusPanelAlignment;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Alignment
  else if AIndex >= 4 then
    Result := paRight
  else
    Result := paLeft;
end;

function TERDStatusBar.EffectiveStatusKind(AIndex: Integer): TERDStatusKind;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].StatusKind
  else if AIndex = 0 then
    Result := skSuccess
  else
    Result := skAccent;
end;

function TERDStatusBar.EffectivePosition(AIndex: Integer): Integer;
begin
  if not PreviewMode then
    Result := FPanels[AIndex].Position
  else if AIndex = 4 then
    Result := 60
  else
    Result := 0;
end;

function TERDStatusBar.EffectiveVisible(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := True
  else
    Result := FPanels[AIndex].Visible;
end;

function TERDStatusBar.EffectiveHint(AIndex: Integer): string;
begin
  if PreviewMode then
    Result := EffectiveText(AIndex)
  else
    Result := FPanels[AIndex].Hint;
end;

function TERDStatusBar.EffectiveMono(AIndex: Integer): Boolean;
begin
  if PreviewMode then
    Result := AIndex = 3
  else
    Result := FPanels[AIndex].Mono;
end;

function TERDStatusBar.TextSize: Single;
begin
  Result := ERD_TEXT_BODY;
end;

function TERDStatusBar.PanelNaturalWidth(APainter: TERDPainter;
  AIndex: Integer): Integer;
var
  Weight: TERDTextWeight;
begin
  if not PreviewMode and (FPanels[AIndex].Control <> nil) and
    (FPanels[AIndex].Width = 0) then
    Exit(System.Math.Max(ScaleValue(24), FPanels[AIndex].FOriginalBounds.Width + ScaleValue(6)));

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
  if not PreviewMode then
  begin
    if FPanels[AIndex].Icon.Available(EffectiveGlyph(AIndex), FImages,
      EffectiveImageIndex(AIndex)) then Inc(Result, ScaleValue(22));
  end
  else if EffectiveGlyph(AIndex) <> glNone then Inc(Result, ScaleValue(18));
  if EffectiveKind(AIndex) = spkStatus then
    Inc(Result, ScaleValue(14));
  if EffectiveKind(AIndex) = spkProgress then
    Inc(Result, ScaleValue(132));
end;

procedure TERDStatusBar.UpdateHostedControls;
var I: Integer; Panel: TERDStatusPanel; R: TRect;
begin
  for I := 0 to FPanels.Count - 1 do
  begin
    Panel := FPanels[I];
    if Panel.Control = nil then Continue;
    Panel.Control.Visible := Panel.Visible and (Panel.Kind = spkControl) and
      not PreviewMode;
    if Panel.Control.Visible then
    begin
      R := Panel.Rect;
      InflateRect(R, -ScaleValue(3), -ScaleValue(3));
      Panel.Control.SetBounds(R.Left, R.Top, System.Math.Max(0, R.Width),
        System.Math.Max(0, R.Height));
    end;
  end;
end;

procedure TERDStatusBar.PanelsChanged;
begin
  if FPanels = nil then Exit;
  if FAutoHeight then Height := BarHeight;
  FHoverIndex := -1;
  UpdateHostedControls;
  Invalidate;
end;

procedure TERDStatusBar.LayoutPanels(APainter: TERDPainter);
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
  if not PreviewMode then UpdateHostedControls;
end;

procedure TERDStatusBar.DrawPanel(APainter: TERDPainter; ACanvas: TCanvas;
  AIndex: Integer);
var
  R: TRect;
  X, CY, Img, BarW, BarX, Pos, TextW, SavedDC: Integer;
  Ink, C: TColor;
  Weight: TERDTextWeight;
  Kind: TERDStatusPanelKind;
  Txt: string;
begin
  R := FPanels[AIndex].Rect;
  Kind := EffectiveKind(AIndex);
  Txt := EffectiveText(AIndex);
  if Kind = spkControl then Exit;
  if not PreviewMode and (Kind = spkCustom) then
  begin
    if Assigned(FOnDrawPanel) then
    begin
      SavedDC := SaveDC(ACanvas.Handle);
      try
        IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
        FOnDrawPanel(Self, ACanvas, FPanels[AIndex], R);
      finally RestoreDC(ACanvas.Handle, SavedDC) end;
    end;
    Exit;
  end;
  if not PreviewMode and (Kind = spkImage) then
  begin
    FPanels[AIndex].Icon.Draw(ACanvas, Palette, APainter.PPI,
      R.Left + R.Width div 2, R.Top + R.Height div 2, Palette.ForegroundText,
      1, True, FPanels[AIndex].Glyph, FImages, FPanels[AIndex].ImageIndex);
    Exit;
  end;
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
  if not PreviewMode then
  begin
    if FPanels[AIndex].Icon.Available(EffectiveGlyph(AIndex), FImages, Img) then
    begin
      FPanels[AIndex].Icon.Draw(ACanvas, Palette, APainter.PPI,
        X + ScaleValue(8), CY, Palette.Subtle, 1, True,
        EffectiveGlyph(AIndex), FImages, Img);
      Inc(X, ScaleValue(22));
    end;
  end
  else if EffectiveGlyph(AIndex) <> glNone then
  begin
    APainter.Glyph(EffectiveGlyph(AIndex), X + ScaleValue(7), CY, Palette.Subtle, 0.75);
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

function TERDStatusBar.SizeGripVisible: Boolean;
var
  Form: TCustomForm;
  Corner: TPoint;
begin
  Result := False;
  if csDesigning in ComponentState then
    Exit;
  Form := GetParentForm(Self);
  if (Form = nil) or not (Form.BorderStyle in [bsSizeable, bsSizeToolWin]) or
    (Form.WindowState = wsMaximized) or not Form.HandleAllocated then
    Exit;
  // Only when this bar sits in the form's bottom-right corner.
  Corner := Form.ScreenToClient(ClientToScreen(Point(Width, Height)));
  Result := (Abs(Corner.X - Form.ClientWidth) <= ScaleValue(2)) and
    (Abs(Corner.Y - Form.ClientHeight) <= ScaleValue(2));
end;

function TERDStatusBar.SizeGripRect: TRect;
var
  S: Integer;
begin
  S := ScaleValue(16);
  Result := Rect(Width - S, Height - S, Width, Height);
end;

procedure TERDStatusBar.DrawSizeGrip(APainter: TERDPainter);
var
  Row, Col, Step, Dot, Right, Bottom: Integer;
begin
  if not SizeGripVisible then
    Exit;
  // Classic triangle of dots hugging the corner: 3 / 2 / 1 from the bottom.
  Step := ScaleValue(4);
  Dot := ScaleValue(2);
  Right := Width - ScaleValue(3);
  Bottom := Height - ScaleValue(3);
  for Row := 0 to 2 do
    for Col := 0 to 2 - Row do
      APainter.FillRect(Rect(Right - Col * Step - Dot, Bottom - Row * Step - Dot,
        Right - Col * Step, Bottom - Row * Step), Palette.Subtle);
end;

procedure TERDStatusBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
const
  WMSZ_BOTTOMRIGHT_SIZE = 8;
var
  Form: TCustomForm;
begin
  inherited;
  if (Button <> mbLeft) or not SizeGripVisible or
    not PtInRect(SizeGripRect, Point(X, Y)) then
    Exit;
  Form := GetParentForm(Self);
  // Hand the drag to Windows' own sizing loop, bottom-right edge.
  ReleaseCapture;
  SendMessage(Form.Handle, WM_SYSCOMMAND, SC_SIZE or WMSZ_BOTTOMRIGHT_SIZE, 0);
end;

function TERDStatusBar.PanelAt(X, Y: Integer): Integer;
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

procedure TERDStatusBar.PaintControl(ACanvas: TCanvas);
var
  P: TERDPainter;
  I: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
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

procedure TERDStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Old: Integer;
begin
  inherited;
  if SizeGripVisible and PtInRect(SizeGripRect, Point(X, Y)) then
    Cursor := crSizeNWSE
  else if Cursor = crSizeNWSE then
    Cursor := crDefault;
  Old := FHoverIndex;
  FHoverIndex := PanelAt(X, Y);
  if Old <> FHoverIndex then
    Invalidate;
end;

procedure TERDStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

procedure TERDStatusBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TERDStatusBar.CMHintShow(var Message: TCMHintShow);
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
