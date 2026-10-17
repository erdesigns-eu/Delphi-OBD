// ------------------------------------------------------------------------------
// ERD.Components.Theme
//
// TERDTheme — non-visual controller component that owns the
// palette, dark-mode and metric/imperial setting for every ERD Components visual on
// the form / data-module. Drop one on a form, and every visual
// on the same Owner auto-binds at runtime — no host code
// required.
//
// The resolution chain a visual control walks (highest priority
// to lowest):
//
// 1. The control's <c>Style</c> per-component overrides.
// 2. The control's explicit <c>Theme</c> property.
// 3. The first <c>TERDTheme</c> found on the Owner / its
// ancestors at runtime (auto-bind).
// 4. The process-wide default theme — set via
// <c>TERDTheme.RegisterDefault</c>.
// 5. The active VCL Style (TStyleManager.ActiveStyle).
// 6. The built-in palette for the theme's Mode: the ERDesigns
// light / dark palette, or the Windows system colours for
// tmWindows.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
// ------------------------------------------------------------------------------

unit ERD.Components.Theme;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF},
  Vcl.Graphics,
  ERD.Components.Types;

type
  TERDTheme = class;

  /// <summary>Fired whenever a visible palette slot changes
  /// (Mode flipped, custom palette overlay set, VCL Style
  /// changed). Bound visuals invalidate themselves.</summary>
  TERDThemeChangedEvent = procedure(Sender: TObject) of object;

  /// <summary>Visuals that want to receive Theme change
  /// notifications implement this interface and call
  /// <c>TERDTheme.Attach(Self)</c>. The control receives
  /// <c>ThemeChanged</c> on every palette change.</summary>
  IERDThemeAware = interface
    ['{0A6C2A65-FF4B-5926-8A30-6C39AB1BE314}']
    procedure ThemeChanged;
  end;

  // Shared contract for ERD controls and adapters with a different VCL ancestor.
  IERDDensitySource = interface
    ['{FA73EF7D-4B5D-4EBC-84AB-901207496849}']
    function GetDensity: TERDDensity;
    function GetParentDensity: Boolean;
  end;

  /// <summary>Controls whose layout depends on
  /// <see cref="TERDTheme.Density"/> implement this next to
  /// <see cref="IERDThemeAware"/>; the theme calls
  /// <c>DensityChanged</c> before it repaints them.</summary>
  IERDDensityAware = interface
    ['{B306C9D8-BA99-5CE7-A43E-AE2F74BC8357}']
    procedure DensityChanged;
  end;

  /// <summary>Non-visual theme controller. Drop on a form /
  /// data-module. Mode = tmAuto follows the active VCL Style;
  /// tmERDesignsLight / tmERDesignsDark force the ERDesigns palette; tmWindows uses
  /// the Windows / VCL system colours.</summary>
  TERDTheme = class(TComponent)
  strict private
    FMode: TERDThemeMode;
    FUnitSystem: TERDUnitSystem;
    FDensity: TERDDensity;
    FOnChange: TERDThemeChangedEvent;
    FOverrideUsed: Boolean;
    FOverride: TERDThemePalette;
    FAttached: TList<TComponent>;

    class var FDefault: TERDTheme;

    procedure SetMode(AValue: TERDThemeMode);
    procedure SetUnitSystem(AValue: TERDUnitSystem);
    procedure SetDensity(AValue: TERDDensity);
    function ResolveBuiltIn: TERDThemePalette;
    procedure NotifyAttached;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>Currently-resolved palette. Honours
    /// <see cref="OverridePalette"/> first, then VCL Style
    /// (for the few slots the style can colour), then the
    /// brand built-in for Mode.</summary>
    function Palette: TERDThemePalette;

    /// <summary>Attach a Theme-aware visual so it receives
    /// <c>ThemeChanged</c> on every palette change. Visual
    /// implements <see cref="IERDThemeAware"/>. Idempotent.</summary>
    procedure Attach(AComponent: TComponent);

    /// <summary>Detach. Auto-called on
    /// <c>Notification(opRemove)</c> for attached components.</summary>
    procedure Detach(AComponent: TComponent);

    /// <summary>Replace the resolved palette with an explicit
    /// custom palette. Setting <c>AHasOverride = False</c>
    /// clears the override and returns to mode-based
    /// resolution.</summary>
    procedure SetCustomPalette(const APalette: TERDThemePalette;
      AHasOverride: Boolean = True);

    /// <summary>Convenience: read-only access to the currently-
    /// set custom-palette override (<c>HasOverride = True</c>
    /// makes this the live palette).</summary>
    function CustomPalette: TERDThemePalette;
    function HasOverride: Boolean;

    /// <summary>Force every attached visual to repaint. Called
    /// internally on Mode / palette changes; hosts can call it
    /// after changing a VCL Style at runtime.</summary>
    procedure Refresh;

    /// <summary>Process-wide fallback theme. Visual controls
    /// use this when their Owner has no <c>TERDTheme</c>. Hosts
    /// call <c>TERDTheme.RegisterDefault(MyTheme)</c> in their
    /// <c>initialization</c> to opt in.</summary>
    class procedure RegisterDefault(ATheme: TERDTheme); static;
    class function GetDefault: TERDTheme; static;

    /// <summary>Walks <c>AControl.Owner</c> ancestry looking for
    /// a <c>TERDTheme</c> sibling. Returns nil when none is
    /// registered. Used by the visual base classes for
    /// auto-bind.</summary>
    class function FindOnOwner(AControl: TComponent): TERDTheme; static;
  published
    /// <summary>Auto / Light / Dark / Windows. Auto = ERDesigns
    /// light or dark palette following the active VCL Style's luma;
    /// Windows = system colours.</summary>
    property Mode: TERDThemeMode read FMode write SetMode default tmERDesignsLight;

    /// <summary>Metric or imperial display units for every bound
    /// dashboard control. Values stay metric internally; changing this
    /// only repaints.</summary>
    property UnitSystem: TERDUnitSystem read FUnitSystem write SetUnitSystem
      default usMetric;

    /// <summary>Row height and hit-target size for every bound ERD
    /// Studio control whose <c>ParentDensity</c> is True. Switch to
    /// <c>dnTablet</c> for touch screens.</summary>
    property Density: TERDDensity read FDensity write SetDensity
      default dnDesktop;

    /// <summary>Fires on every palette change.</summary>
    property OnChange: TERDThemeChangedEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Vcl.Themes;

{ TERDTheme ----------------------------------------------------------------- }

constructor TERDTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := tmERDesignsLight;
  FUnitSystem := usMetric;
  FDensity := dnDesktop;
  FAttached := TList<TComponent>.Create;
end;

destructor TERDTheme.Destroy;
begin
  // If we were the process-wide default, clear that.
  if FDefault = Self then
    FDefault := nil;
  // Keep the free-notification links: inherited Destroy sends
  // opRemove to every attached component, which clears its
  // reference to this theme.
  FAttached.Clear;
  inherited;
  FreeAndNil(FAttached);
end;

procedure TERDTheme.SetMode(AValue: TERDThemeMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
  Refresh;
end;

procedure TERDTheme.SetUnitSystem(AValue: TERDUnitSystem);
begin
  if FUnitSystem = AValue then
    Exit;
  FUnitSystem := AValue;
  Refresh;
end;

procedure TERDTheme.SetDensity(AValue: TERDDensity);
var
  C: TComponent;
  Aware: IERDDensityAware;
begin
  if FDensity = AValue then
    Exit;
  FDensity := AValue;
  // Controls re-layout on a density change; tell them before the
  // repaint.
  for C in FAttached do
    if Supports(C, IERDDensityAware, Aware) then
      try
        Aware.DensityChanged;
      except
        // Don't let one bad subscriber take the rest down.
      end;
  Refresh;
end;

function TERDTheme.ResolveBuiltIn: TERDThemePalette;
var Mode: TERDThemeMode;
begin
  Mode := FMode;
  if Mode = tmAuto then
  begin
    if VCLStyleIsDark then Mode := tmERDesignsDark else Mode := tmERDesignsLight;
  end;
  if Mode in [tmERDesignsDark, tmOfficeDark, tmOfficeWordDark, tmOfficeExcelDark, tmOfficeAccessDark, tmOfficePowerPointDark, tmOfficeOutlookDark, tmOfficeOneNoteDark] then
    Result := BRAND_PALETTE_DARK
  else Result := BRAND_PALETTE_LIGHT;
  case Mode of
    tmOfficeLight:
      begin Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.Accent := $00BD6C0F; Result.NeutralLight := $00D8D8D8 end;
    tmOfficeDark:
      begin Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.Accent := $00F6A13B; Result.NeutralLight := $00484848 end;
    tmOfficeWordLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $00BD5A18;
      end;
    tmOfficeWordDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $00E88D4F;
      end;
    tmOfficeExcelLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $00417C10;
      end;
    tmOfficeExcelDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $007FBB3E;
      end;
    tmOfficeAccessLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $003A37A4;
      end;
    tmOfficeAccessDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $007573DD;
      end;
    tmOfficePowerPointLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $002647D2;
      end;
    tmOfficePowerPointDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $00688BF2;
      end;
    tmOfficeOutlookLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $00D47800;
      end;
    tmOfficeOutlookDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $00F1A755;
      end;
    tmOfficeOneNoteLight:
      begin
        Result.Background := $00F5F5F5; Result.GaugeFace := clWhite;
        Result.NeutralLight := $00D8D8D8;
        Result.Accent := $00AA1977;
      end;
    tmOfficeOneNoteDark:
      begin
        Result.Background := $00202020; Result.GaugeFace := $002A2A2A;
        Result.NeutralLight := $00484848;
        Result.Accent := $00EA7ABA;
      end;
  end;
  Result.GaugeNeedle := Result.Accent;
  Result.GaugeLabel := Result.Subtle;
  Result.GaugeTick := Result.ForegroundText;
end;

function TERDTheme.Palette: TERDThemePalette;
var
  Built: TERDThemePalette;
begin
  if FOverrideUsed then
    Exit(FOverride);

  // The Windows palette is built from the (styled) system colours
  // already, so it needs no further overlay.
  if FMode = tmWindows then
    Exit(WindowsPalette);

  // Start from the brand built-in for the resolved mode.
  Built := ResolveBuiltIn;

  // If an active VCL Style is present, overlay the styled slots
  // that map cleanly onto the palette. (We deliberately don't
  // overlay gauge-face / needle / tick — those are brand
  // identifiers, not OS chrome.)
  if (FMode = tmAuto) and TStyleManager.IsCustomStyleActive then
  begin
    Built.Background := StyleColor(scWindow, Built.Background);
    Built.ForegroundText := StyleServices.GetSystemColor(clWindowText);
    Built.NeutralLight := StyleColor(scPanel, Built.NeutralLight);
    Built.NeutralDark := StyleServices.GetSystemColor(clWindowText);
    Built.Subtle := StyleServices.GetSystemColor(clGrayText);
  end;

  Result := Built;
end;

procedure TERDTheme.Attach(AComponent: TComponent);
begin
  if AComponent = nil then
    Exit;
  if not Supports(AComponent, IERDThemeAware) then
    Exit;
  if FAttached.IndexOf(AComponent) >= 0 then
    Exit;
  FAttached.Add(AComponent);
  AComponent.FreeNotification(Self);
end;

procedure TERDTheme.Detach(AComponent: TComponent);
begin
  if AComponent = nil then
    Exit;
  FAttached.Remove(AComponent);
  AComponent.RemoveFreeNotification(Self);
end;

procedure TERDTheme.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    FAttached.Remove(AComponent);
end;

procedure TERDTheme.SetCustomPalette(const APalette: TERDThemePalette;
  AHasOverride: Boolean);
begin
  FOverride := APalette;
  FOverrideUsed := AHasOverride;
  Refresh;
end;

function TERDTheme.CustomPalette: TERDThemePalette;
begin
  Result := FOverride;
end;

function TERDTheme.HasOverride: Boolean;
begin
  Result := FOverrideUsed;
end;

procedure TERDTheme.NotifyAttached;
var
  C: TComponent;
  Aware: IERDThemeAware;
begin
  for C in FAttached do
    if Supports(C, IERDThemeAware, Aware) then
      try
        Aware.ThemeChanged;
      except
        // Don't let one bad subscriber take the rest down.
      end;
end;

procedure TERDTheme.Refresh;
begin
  NotifyAttached;
  if Assigned(FOnChange) then
    try
      FOnChange(Self);
    except
    end;
end;

class procedure TERDTheme.RegisterDefault(ATheme: TERDTheme);
begin
  FDefault := ATheme;
end;

class function TERDTheme.GetDefault: TERDTheme;
begin
  Result := FDefault;
end;

class function TERDTheme.FindOnOwner(AControl: TComponent): TERDTheme;
var
  Owner: TComponent;
  I: Integer;
begin
  Result := nil;
  if AControl = nil then
    Exit;
  Owner := AControl.Owner;
  while Owner <> nil do
  begin
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TERDTheme then
        Exit(TERDTheme(Owner.Components[I]));
    Owner := Owner.Owner;
  end;
end;

end.
