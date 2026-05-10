//------------------------------------------------------------------------------
//  OBD.UI.Theme
//
//  TOBDTheme — non-visual controller component that owns the
//  palette + dark-mode setting for every Delphi-OBD visual on
//  the form / data-module. Drop one on a form, and every visual
//  on the same Owner auto-binds at runtime — no host code
//  required.
//
//  The resolution chain a visual control walks (highest priority
//  to lowest):
//
//    1. The control's <c>Style</c> per-component overrides.
//    2. The control's explicit <c>Theme</c> property.
//    3. The first <c>TOBDTheme</c> found on the Owner / its
//       ancestors at runtime (auto-bind).
//    4. The process-wide default theme — set via
//       <c>TOBDTheme.RegisterDefault</c>.
//    5. The active VCL Style (TStyleManager.ActiveStyle).
//    6. The built-in brand palette (light / dark per mode).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Theme;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  OBD.UI.Types;

type
  TOBDTheme = class;

  /// <summary>Fired whenever a visible palette slot changes
  /// (Mode flipped, custom palette overlay set, VCL Style
  /// changed). Bound visuals invalidate themselves.</summary>
  TOBDThemeChangedEvent = procedure(Sender: TObject) of object;

  /// <summary>Visuals that want to receive Theme change
  /// notifications implement this interface and call
  /// <c>TOBDTheme.Attach(Self)</c>. The control receives
  /// <c>ThemeChanged</c> on every palette change.</summary>
  IOBDThemeAware = interface
    ['{B0F4E6F0-7C8A-4A5D-8E3F-1F2B6E0F2F1A}']
    procedure ThemeChanged;
  end;

  /// <summary>Non-visual theme controller. Drop on a form /
  /// data-module. Mode = tmAuto follows the active VCL Style;
  /// tmLight / tmDark force a palette.</summary>
  TOBDTheme = class(TComponent)
  strict private
    FMode:        TOBDThemeMode;
    FOnChange:    TOBDThemeChangedEvent;
    FOverrideUsed: Boolean;
    FOverride:    TOBDThemePalette;
    FAttached:    TList<TComponent>;

    class var FDefault: TOBDTheme;

    procedure SetMode(AValue: TOBDThemeMode);
    function  ResolveBuiltIn: TOBDThemePalette;
    procedure NotifyAttached;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>Currently-resolved palette. Honours
    /// <see cref="OverridePalette"/> first, then VCL Style
    /// (for the few slots the style can colour), then the
    /// brand built-in for Mode.</summary>
    function Palette: TOBDThemePalette;

    /// <summary>Attach a Theme-aware visual so it receives
    /// <c>ThemeChanged</c> on every palette change. Visual
    /// implements <see cref="IOBDThemeAware"/>. Idempotent.</summary>
    procedure Attach(AComponent: TComponent);

    /// <summary>Detach. Auto-called on
    /// <c>Notification(opRemove)</c> for attached components.</summary>
    procedure Detach(AComponent: TComponent);

    /// <summary>Replace the resolved palette with an explicit
    /// custom palette. Setting <c>AHasOverride = False</c>
    /// clears the override and returns to mode-based
    /// resolution.</summary>
    procedure SetCustomPalette(const APalette: TOBDThemePalette;
                               AHasOverride: Boolean = True);

    /// <summary>Convenience: read-only access to the currently-
    /// set custom-palette override (<c>HasOverride = True</c>
    /// makes this the live palette).</summary>
    function CustomPalette: TOBDThemePalette;
    function HasOverride: Boolean;

    /// <summary>Force every attached visual to repaint. Called
    /// internally on Mode / palette changes; hosts can call it
    /// after changing a VCL Style at runtime.</summary>
    procedure Refresh;

    /// <summary>Process-wide fallback theme. Visual controls
    /// use this when their Owner has no <c>TOBDTheme</c>. Hosts
    /// call <c>TOBDTheme.RegisterDefault(MyTheme)</c> in their
    /// <c>initialization</c> to opt in.</summary>
    class procedure RegisterDefault(ATheme: TOBDTheme); static;
    class function  GetDefault: TOBDTheme; static;

    /// <summary>Walks <c>AControl.Owner</c> ancestry looking for
    /// a <c>TOBDTheme</c> sibling. Returns nil when none is
    /// registered. Used by the visual base classes for
    /// auto-bind.</summary>
    class function FindOnOwner(AControl: TComponent): TOBDTheme; static;
  published
    /// <summary>Light / Dark / Auto. Auto = follow the active
    /// VCL Style's luma.</summary>
    property Mode: TOBDThemeMode read FMode write SetMode default tmAuto;

    /// <summary>Fires on every palette change.</summary>
    property OnChange: TOBDThemeChangedEvent
      read FOnChange write FOnChange;
  end;

implementation

uses
  Vcl.Themes;

{ TOBDTheme ----------------------------------------------------------------- }

constructor TOBDTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := tmAuto;
  FAttached := TList<TComponent>.Create;
end;

destructor TOBDTheme.Destroy;
begin
  // Detach every attached component first so they don't dangle.
  while FAttached.Count > 0 do
    Detach(FAttached[FAttached.Count - 1]);
  FAttached.Free;
  // If we were the process-wide default, clear that.
  if FDefault = Self then
    FDefault := nil;
  inherited;
end;

procedure TOBDTheme.SetMode(AValue: TOBDThemeMode);
begin
  if FMode = AValue then Exit;
  FMode := AValue;
  Refresh;
end;

function TOBDTheme.ResolveBuiltIn: TOBDThemePalette;
var
  Mode: TOBDThemeMode;
begin
  Mode := FMode;
  if Mode = tmAuto then
  begin
    if VCLStyleIsDark then Mode := tmDark
    else                   Mode := tmLight;
  end;
  case Mode of
    tmDark: Result := BRAND_PALETTE_DARK;
  else      Result := BRAND_PALETTE_LIGHT;
  end;
end;

function TOBDTheme.Palette: TOBDThemePalette;
var
  Built: TOBDThemePalette;
begin
  if FOverrideUsed then Exit(FOverride);

  // Start from the brand built-in for the resolved mode.
  Built := ResolveBuiltIn;

  // If an active VCL Style is present, overlay the styled slots
  // that map cleanly onto the palette. (We deliberately don't
  // overlay gauge-face / needle / tick — those are brand
  // identifiers, not OS chrome.)
  if TStyleManager.IsCustomStyleActive then
  begin
    Built.Background     := StyleColor(scWindow,        Built.Background);
    Built.ForegroundText := StyleColor(scWindowText,    Built.ForegroundText);
    Built.NeutralLight   := StyleColor(scPanel,         Built.NeutralLight);
    Built.NeutralDark    := StyleColor(scWindowText,    Built.NeutralDark);
    Built.Subtle         := StyleColor(scGenericGrayed, Built.Subtle);
  end;

  Result := Built;
end;

procedure TOBDTheme.Attach(AComponent: TComponent);
begin
  if AComponent = nil then Exit;
  if not Supports(AComponent, IOBDThemeAware) then Exit;
  if FAttached.IndexOf(AComponent) >= 0 then Exit;
  FAttached.Add(AComponent);
  AComponent.FreeNotification(Self);
end;

procedure TOBDTheme.Detach(AComponent: TComponent);
begin
  if AComponent = nil then Exit;
  FAttached.Remove(AComponent);
  AComponent.RemoveFreeNotification(Self);
end;

procedure TOBDTheme.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    FAttached.Remove(AComponent);
end;

procedure TOBDTheme.SetCustomPalette(const APalette: TOBDThemePalette;
  AHasOverride: Boolean);
begin
  FOverride := APalette;
  FOverrideUsed := AHasOverride;
  Refresh;
end;

function TOBDTheme.CustomPalette: TOBDThemePalette;
begin
  Result := FOverride;
end;

function TOBDTheme.HasOverride: Boolean;
begin
  Result := FOverrideUsed;
end;

procedure TOBDTheme.NotifyAttached;
var
  C: TComponent;
  Aware: IOBDThemeAware;
begin
  for C in FAttached do
    if Supports(C, IOBDThemeAware, Aware) then
      try
        Aware.ThemeChanged;
      except
        // Don't let one bad subscriber take the rest down.
      end;
end;

procedure TOBDTheme.Refresh;
begin
  NotifyAttached;
  if Assigned(FOnChange) then
    try FOnChange(Self); except end;
end;

class procedure TOBDTheme.RegisterDefault(ATheme: TOBDTheme);
begin
  FDefault := ATheme;
end;

class function TOBDTheme.GetDefault: TOBDTheme;
begin
  Result := FDefault;
end;

class function TOBDTheme.FindOnOwner(AControl: TComponent): TOBDTheme;
var
  Owner: TComponent;
  I: Integer;
begin
  Result := nil;
  if AControl = nil then Exit;
  Owner := AControl.Owner;
  while Owner <> nil do
  begin
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TOBDTheme then
        Exit(TOBDTheme(Owner.Components[I]));
    Owner := Owner.Owner;
  end;
end;

end.
