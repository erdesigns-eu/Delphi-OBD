//------------------------------------------------------------------------------
//  ERD.Components.Hint
//
//  Themed hint windows for Application controls.
//
//    TERDHintWindow   THintWindow descendant that draws a card with a bold
//                     title, optional body text and an optional shortcut chip.
//    TERDHintStyle    non-visual component that installs TERDHintWindow for
//                     the application while active.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit ERD.Components.Hint;
{$B-}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.TypInfo,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Themes,
  Vcl.ImgList,
  ERD.Components.Icons,
  ERD.Components.Types,
  ERD.Components.Theme,
  ERD.Components.Paint;

type
  TERDHintContent = record
    Title, Description, Shortcut: string;
    Glyph: TERDGlyph;
    Icon: TERDIcon;
    Images: TCustomImageList;
    ImageIndex: Integer;
    Palette: TERDThemePalette;
    Density: TERDDensity;
    HasPalette, HasDensity: Boolean;
  end;
  IERDHintProvider = interface
    ['{D5C553E7-2868-4638-AF24-E7C2688A2B4D}']
    function HintContentAt(const ScreenPoint: TPoint; out Content: TERDHintContent): Boolean;
  end;
  TERDHintLayout = (hlCompact, hlOffice);
  TERDResolveHintEvent = procedure(Sender: TObject; HintControl: TControl;
    var Title, Description, Shortcut: string; var Glyph: TERDGlyph) of object;
  /// <summary>Themed hint window with title, body and shortcut text.</summary>
  TERDHintWindow = class(THintWindow)
  strict private
    FTitle: string;
    FBody: string;
    FShortcut: string;
    FGlyph: TERDGlyph;
    FHintIcon: TERDIcon;
    FSourcePalette: TERDThemePalette;
    FSourceDensity: TERDDensity;
    FHasSourcePalette, FHasSourceDensity: Boolean;
    FTitleHeight, FBodyHeight, FTextLeft, FPadding: Integer;
    function HasIcon: Boolean;
    function OfficeLayout: Boolean;
    function OfficeRect(MaxWidth: Integer): TRect;
    procedure PaintOffice;
    function HintScale(Value: Integer): Integer;
    procedure ParseHint(const AHint: string; AData: Pointer);
    function ActivePalette: TERDThemePalette;
    function ActiveDensity: TERDDensity;
    function TextWidth(const AText: string; ASize: Single;
      AWeight: TERDTextWeight): Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    /// <summary>Draws the themed card.</summary>
    procedure Paint; override;
    /// <summary>Draws no native border.</summary>
    /// <param name="Message">Non-client paint message.</param>
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  public
    /// <summary>Calculates the themed hint rectangle.</summary>
    /// <param name="MaxWidth">Maximum width.</param>
    /// <param name="AHint">Hint text in Title|Body form.</param>
    /// <param name="AData">Hint information pointer.</param>
    /// <returns>Required rectangle.</returns>
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    /// <summary>Reads hint data before the window is shown.</summary>
    /// <param name="Rect">Window rectangle.</param>
    /// <param name="AHint">Hint text.</param>
    /// <param name="AData">Hint information pointer.</param>
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
    /// <summary>Creates the themed hint window.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  /// <summary>Application-level themed hint installer.</summary>
  TERDHintStyle = class(TComponent, IERDThemeAware)
  strict private
    FTheme: TERDTheme;
    FDensity: TERDDensity;
    FActive: Boolean;
    FLayout: TERDHintLayout;
    FGlyph: TERDGlyph;
    FOnResolveHint: TERDResolveHintEvent;
    procedure SetTheme(AValue: TERDTheme);
    procedure SetDensity(AValue: TERDDensity);
    procedure SetActive(AValue: Boolean);
    function GetPalette: TERDThemePalette;
    procedure Install;
    procedure Uninstall;
  protected
    /// <summary>Clears theme references when they are removed.</summary>
    /// <param name="AComponent">Removed component.</param>
    /// <param name="Operation">Notification operation.</param>
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    /// <summary>Creates an active hint style.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Restores the previous hint window class when needed.</summary>
    destructor Destroy; override;
    /// <summary>Receives theme changes.</summary>
    procedure ThemeChanged;
    /// <summary>Palette currently used by hint windows.</summary>
    /// <returns>Resolved palette.</returns>
    property Palette: TERDThemePalette read GetPalette;
  published
    property Layout: TERDHintLayout read FLayout write FLayout default hlOffice;
    property Glyph: TERDGlyph read FGlyph write FGlyph default glInfo;
    property OnResolveHint: TERDResolveHintEvent read FOnResolveHint write FOnResolveHint;
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme: TERDTheme read FTheme write SetTheme;
    /// <summary>Desktop or tablet sizing.</summary>
    property Density: TERDDensity read FDensity write SetDensity
      default dnDesktop;
    /// <summary>Installs the themed hint window at run time.</summary>
    property Active: Boolean read FActive write SetActive default True;
  end;

function ERDResolveHint(Control: TControl; const HintText: string; const ScreenPoint: TPoint): TERDHintContent;

implementation

var
  GActiveHintStyle: TERDHintStyle = nil;
  GPreviousHintWindowClass: THintWindowClass = nil;

function ERDResolveHint(Control: TControl; const HintText: string; const ScreenPoint: TPoint): TERDHintContent;
var Provider: IERDHintProvider; Info: PPropInfo; ObjectValue: TObject; Theme: TERDTheme;
    Action: TCustomAction; Raw, Caption: string; P: Integer; ParentControl: TControl;
  function StringProperty(const Name: string): string;
  var Prop: PPropInfo;
  begin
    Result := ''; if Control = nil then Exit; Prop := GetPropInfo(Control.ClassInfo,Name);
    if (Prop <> nil) and (Prop.PropType^.Kind in [tkString,tkLString,tkWString,tkUString]) then Result := GetStrProp(Control,Prop);
  end;
begin
  Result := Default(TERDHintContent); Result.ImageIndex := -1; Raw := HintText;
  if Control <> nil then
  begin
    if Supports(Control,IERDHintProvider,Provider) and Provider.HintContentAt(ScreenPoint,Result) then
    begin if Result.Description <> '' then Raw := Result.Description; end
    else
    begin
      Result := Default(TERDHintContent); Result.ImageIndex := -1;
      Result.Title := StringProperty('Caption'); if Result.Title = '' then Result.Title := StringProperty('TextHint');
      Info := GetPropInfo(Control.ClassInfo,'Glyph');
      if (Info <> nil) and (Info.PropType^ = TypeInfo(TERDGlyph)) then Result.Glyph := TERDGlyph(GetOrdProp(Control,Info));
      Info := GetPropInfo(Control.ClassInfo,'Icon');
      if (Info <> nil) and (Info.PropType^.Kind = tkClass) then begin ObjectValue := GetObjectProp(Control,Info); if ObjectValue is TERDIcon then Result.Icon := TERDIcon(ObjectValue); end;
      Info := GetPropInfo(Control.ClassInfo,'Images');
      if (Info <> nil) and (Info.PropType^.Kind = tkClass) then begin ObjectValue := GetObjectProp(Control,Info); if ObjectValue is TCustomImageList then Result.Images := TCustomImageList(ObjectValue); end;
      Info := GetPropInfo(Control.ClassInfo,'ImageIndex');
      if (Info <> nil) and (Info.PropType^.Kind = tkInteger) then Result.ImageIndex := GetOrdProp(Control,Info);
    end;
    if Control.Action is TCustomAction then
    begin
      Action := TCustomAction(Control.Action); if Result.Title = '' then Result.Title := Action.Caption;
      if Result.Shortcut = '' then Result.Shortcut := ShortCutToText(Action.ShortCut);
    end;
    // Native VCL may hand us the long portion after '|'. Recover the original
    // control hint only when it is the same hint, not unrelated custom item text.
    if (Pos('|',Raw) = 0) and (Pos('|',Control.Hint) > 0) and
      ((Raw = GetLongHint(Control.Hint)) or (Raw = GetShortHint(Control.Hint))) then Raw := Control.Hint;
    Theme := nil; ParentControl := Control;
    while ParentControl <> nil do
    begin
      Info := GetPropInfo(ParentControl.ClassInfo,'Theme');
      if (Info <> nil) and (Info.PropType^.Kind = tkClass) then
      begin ObjectValue := GetObjectProp(ParentControl,Info); if ObjectValue is TERDTheme then begin Theme := TERDTheme(ObjectValue); Break; end; end;
      ParentControl := ParentControl.Parent;
    end;
    if Theme = nil then Theme := TERDTheme.FindOnOwner(Control); if Theme = nil then Theme := TERDTheme.GetDefault;
    if (Theme <> nil) and not Result.HasPalette then begin Result.Palette := Theme.Palette; Result.HasPalette := True; Result.Density := Theme.Density; Result.HasDensity := True; end;
    Info := GetPropInfo(Control.ClassInfo,'Density');
    if not Result.HasDensity and (Info <> nil) and (Info.PropType^ = TypeInfo(TERDDensity)) then begin Result.Density := TERDDensity(GetOrdProp(Control,Info)); Result.HasDensity := True; end;
  end;
  Caption := StringReplace(Result.Title,'&','',[rfReplaceAll]); P := Pos('|',Raw);
  if P > 0 then begin Result.Title := Copy(Raw,1,P-1); Result.Description := Copy(Raw,P+1,MaxInt); end
  else if (Caption <> '') and (Raw <> Caption) then begin Result.Title := Caption; Result.Description := Raw; end
  else begin Result.Title := Raw; Result.Description := ''; end;
  P := Pos(#9,Result.Title); if P > 0 then begin Result.Shortcut := Copy(Result.Title,P+1,MaxInt); Result.Title := Copy(Result.Title,1,P-1); end;
  Result.Title := Trim(Result.Title); Result.Description := Trim(Result.Description); Result.Shortcut := Trim(Result.Shortcut);
end;

function FallbackPalette: TERDThemePalette;
begin
  Result := BRAND_PALETTE_LIGHT;
end;

{ TERDHintWindow }

constructor TERDHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clNone;
  StyleElements := [];
  FHintIcon := TERDIcon.Create(Self);
end;

destructor TERDHintWindow.Destroy;
begin FreeAndNil(FHintIcon); inherited; end;
function TERDHintWindow.HasIcon: Boolean;
begin Result := (FGlyph <> glNone) or ((FHintIcon <> nil) and FHintIcon.Available); end;
procedure TERDHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Keep the inherited popup shadow; replace only the native raised frame.
  Params.Style := Params.Style and not (WS_BORDER or WS_DLGFRAME or WS_THICKFRAME);
  Params.ExStyle := Params.ExStyle and not (WS_EX_CLIENTEDGE or
    WS_EX_STATICEDGE or WS_EX_WINDOWEDGE or WS_EX_DLGMODALFRAME);
end;

procedure TERDHintWindow.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  // Paint the single flat border in the client area, without a native inset.
  Message.Result := 0;
end;

function TERDHintWindow.ActivePalette: TERDThemePalette;
begin
  if (GActiveHintStyle <> nil) and (GActiveHintStyle.Theme <> nil) then Result := GActiveHintStyle.Palette
  else if FHasSourcePalette then Result := FSourcePalette
  else if GActiveHintStyle <> nil then Result := GActiveHintStyle.Palette
  else if TERDTheme.GetDefault <> nil then Result := TERDTheme.GetDefault.Palette else Result := FallbackPalette;
end;
function TERDHintWindow.ActiveDensity: TERDDensity;
begin
  if FHasSourceDensity then Result := FSourceDensity else if GActiveHintStyle <> nil then Result := GActiveHintStyle.Density else Result := dnDesktop;
end;

function TERDHintWindow.TextWidth(const AText: string; ASize: Single;
  AWeight: TERDTextWeight): Integer;
begin
  Result := ERDMeasureText(AText, ASize, AWeight, Screen.PixelsPerInch);
end;

procedure TERDHintWindow.ParseHint(const AHint: string; AData: Pointer);
var Control: TControl; Content: TERDHintContent;
begin
  Control := FindDragTarget(Mouse.CursorPos,True); Content := ERDResolveHint(Control,AHint,Mouse.CursorPos);
  FTitle := Content.Title; FBody := Content.Description; FShortcut := Content.Shortcut;
  FGlyph := Content.Glyph; if FGlyph = glNone then if GActiveHintStyle <> nil then FGlyph := GActiveHintStyle.Glyph else FGlyph := glInfo;
  FSourcePalette := Content.Palette; FHasSourcePalette := Content.HasPalette; FSourceDensity := Content.Density; FHasSourceDensity := Content.HasDensity;
  FHintIcon.Source := eisNone;
  if Content.Icon <> nil then FHintIcon.Assign(Content.Icon);
  if not FHintIcon.Available and (Content.Images <> nil) and (Content.ImageIndex >= 0) and (Content.ImageIndex < Content.Images.Count) then
  begin FHintIcon.Images := Content.Images; FHintIcon.ImageIndex := Content.ImageIndex; FHintIcon.Source := eisImageList; end;
  if (GActiveHintStyle <> nil) and Assigned(GActiveHintStyle.OnResolveHint) then
  begin
    // An explicit resolver takes final control over the glyph, including glNone.
    FHintIcon.Source := eisNone;
    GActiveHintStyle.OnResolveHint(GActiveHintStyle,Control,FTitle,FBody,FShortcut,FGlyph);
  end;
  // AData is arbitrary caller HintData, never an assumed object/record pointer.
end;

function TERDHintWindow.HintScale(Value: Integer): Integer;
begin Result := MulDiv(Value,Screen.PixelsPerInch,96); end;
function TERDHintWindow.OfficeLayout: Boolean;
begin Result := (GActiveHintStyle = nil) or (GActiveHintStyle.Layout = hlOffice); end;
function TERDHintWindow.OfficeRect(MaxWidth: Integer): TRect;
var W, Limit, TextW, H: Integer; Box: TRect;
  function Measure(const Text: string; Bold: Boolean): Integer;
  begin
    if Text = '' then Exit(0);
    Canvas.Font.Name := 'Segoe UI'; Canvas.Font.Height := -HintScale(12);
    if Bold then Canvas.Font.Style := [fsBold] else Canvas.Font.Style := [];
    Box := Rect(0,0,TextW,0); DrawTextW(Canvas.Handle,PWideChar(Text),Length(Text),Box,DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
    Result := Box.Height;
  end;
begin
  FPadding := HintScale(12); if ActiveDensity = dnTablet then FPadding := HintScale(16);
  FTextLeft := FPadding; if HasIcon then Inc(FTextLeft,HintScale(32));
  Limit := HintScale(360); if MaxWidth > 0 then Limit := Min(Limit,MaxWidth); Limit := Max(1,Limit);
  W := Min(Limit,Max(HintScale(160),Max(TextWidth(FTitle,12,twBold),TextWidth(FBody,12,twRegular))+FTextLeft+FPadding));
  if FShortcut <> '' then W := Min(Limit,Max(W,TextWidth(FShortcut,11,twRegular)+2*FPadding));
  TextW := Max(1,W-FTextLeft-FPadding);
  FTitleHeight := Min(HintScale(80),Measure(FTitle,True)); FBodyHeight := Min(HintScale(220),Measure(FBody,False));
  H := 2*FPadding+Max(HintScale(22),FTitleHeight);
  if FBody <> '' then Inc(H,HintScale(8)+FBodyHeight);
  if FShortcut <> '' then Inc(H,HintScale(30));
  Result := Rect(0,0,W,Min(H,Screen.WorkAreaRect.Height));
end;
procedure TERDHintWindow.PaintOffice;
var Painter: TERDPainter; Palette: TERDThemePalette; R: TRect; Top, Bottom: Integer;
begin
  Palette := ActivePalette; Canvas.Brush.Color := Palette.GaugeFace; Canvas.FillRect(ClientRect);
  Painter := TERDPainter.Create(Canvas,Palette,Screen.PixelsPerInch);
  try
    Painter.FillRect(ClientRect,Palette.GaugeFace); Painter.FrameRect(ClientRect,Palette.NeutralLight);
    Bottom := ClientHeight-FPadding;
    if FShortcut <> '' then
    begin
      Dec(Bottom,HintScale(30)); Painter.LineF(FPadding,Bottom+HintScale(8),ClientWidth-FPadding,Bottom+HintScale(8),Palette.NeutralLight);
      Painter.TextF(FPadding,Bottom+HintScale(22),FShortcut,11,Palette.Subtle,twRegular,taLeftJustify,Max(0,ClientWidth-2*FPadding));
    end;
    Top := FPadding;
    if (FHintIcon <> nil) and FHintIcon.Available then FHintIcon.Draw(Canvas,Palette,Screen.PixelsPerInch,FPadding+HintScale(11),Top+HintScale(11),Painter.AccentText,1.4)
    else if FGlyph <> glNone then Painter.Glyph(FGlyph,FPadding+HintScale(11),Top+HintScale(11),Painter.AccentText,1.4);
    R := Rect(FTextLeft,Top,Max(FTextLeft,ClientWidth-FPadding),Min(Bottom,Top+FTitleHeight));
    Painter.WrapText(R,FTitle,12,Palette.ForegroundText,twBold); Inc(Top,Max(HintScale(22),FTitleHeight));
    if FBody <> '' then
    begin Inc(Top,HintScale(8)); R := Rect(FTextLeft,Top,Max(FTextLeft,ClientWidth-FPadding),Max(Top,Bottom)); Painter.WrapText(R,FBody,12,Palette.GaugeLabel,twRegular); end;
  finally Painter.Free; end;
end;

function TERDHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  W, H, TitleW, BodyW, ShortcutW, Limit: Integer;
  Metrics: TERDDensityMetrics;
begin
  ParseHint(AHint, AData);
  if OfficeLayout then Exit(OfficeRect(MaxWidth));
  Metrics := DensityMetrics(ActiveDensity);
  TitleW := TextWidth(FTitle, 12.5, twBold);
  BodyW := TextWidth(FBody, 12, twRegular);
  ShortcutW := TextWidth(FShortcut, 11.5, twRegular);
  W := System.Math.Max(TitleW + ShortcutW + 24, BodyW) + 24;
  Limit := MaxWidth;
  if Limit <= 0 then
    Limit := 360;
  W := Min(Max(W,96),Max(1,Limit));
  if FBody <> '' then
    H := 52
  else
    H := 34;
  if Metrics.Button > 30 then
    H := H + (Metrics.Button - 30) div 2;
  Result := Rect(0, 0, W, H);
end;

procedure TERDHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
begin
  ParseHint(AHint, AData);
  if OfficeLayout then OfficeRect(Rect.Width);
  inherited ActivateHintData(Rect, AHint, AData);
end;

procedure TERDHintWindow.WMNCPaint(var Message: TWMNCPaint);
begin
  Message.Result := 0;
end;

procedure TERDHintWindow.Paint;
var
  Painter: TERDPainter;
  P: TERDThemePalette;
  R: TRect;
  YTitle, YBody, ShortcutW: Integer;
  MaxTitleW: Integer;
begin
  if OfficeLayout then begin PaintOffice; Exit; end;
  P := ActivePalette;
  Canvas.Brush.Color := P.GaugeFace;
  Canvas.FillRect(ClientRect);
  Painter := TERDPainter.Create(Canvas, P, Screen.PixelsPerInch);
  try
    R := ClientRect;
    Painter.FillRect(R, P.GaugeFace);
    Painter.FrameRect(R, P.NeutralLight);
    YTitle := 16;
    if FBody = '' then
      YTitle := R.Height div 2;
    ShortcutW := 0;
    if FShortcut <> '' then
      ShortcutW := Painter.TextWidth(FShortcut, 11.5, twRegular) + 8;
    MaxTitleW := R.Width - 24 - ShortcutW;
    Painter.Text(12, YTitle, FTitle, 12.5, P.ForegroundText, twBold,
      taLeftJustify, MaxTitleW);
    if FShortcut <> '' then
      Painter.Text(R.Right - 12, YTitle, FShortcut, 11.5, P.GaugeLabel,
        twRegular, taRightJustify);
    if FBody <> '' then
    begin
      YBody := 36;
      Painter.Text(12, YBody, FBody, ERD_TEXT_BODY, P.GaugeLabel, twRegular,
        taLeftJustify, R.Width - 24);
    end;
  finally
    Painter.Free;
  end;
end;

{ TERDHintStyle }

constructor TERDHintStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDensity := dnDesktop;
  FLayout := hlOffice;
  FGlyph := glInfo;
  FActive := True;
  if not (csDesigning in ComponentState) then
    Install;
end;

destructor TERDHintStyle.Destroy;
begin
  Uninstall;
  SetTheme(nil);
  inherited;
end;

procedure TERDHintStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTheme) then
    FTheme := nil;
end;

procedure TERDHintStyle.ThemeChanged;
begin
end;

function TERDHintStyle.GetPalette: TERDThemePalette;
begin
  if FTheme <> nil then
    Result := FTheme.Palette
  else if TERDTheme.GetDefault <> nil then
    Result := TERDTheme.GetDefault.Palette
  else
    Result := FallbackPalette;
end;

procedure TERDHintStyle.SetTheme(AValue: TERDTheme);
begin
  if FTheme = AValue then
    Exit;
  if FTheme <> nil then
  begin
    FTheme.Detach(Self);
    FTheme.RemoveFreeNotification(Self);
  end;
  FTheme := AValue;
  if FTheme <> nil then
  begin
    FTheme.FreeNotification(Self);
    FTheme.Attach(Self);
  end;
end;

procedure TERDHintStyle.SetDensity(AValue: TERDDensity);
begin
  FDensity := AValue;
end;

procedure TERDHintStyle.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if FActive then
    Install
  else
    Uninstall;
end;

procedure RecreateHintWindow;
begin
  // Toggling ShowHint frees the application's hint window and creates one of
  // the current HintWindowClass. Never do that while the application shuts
  // down: the main form (and this component) is freed inside
  // Application.DestroyComponents, after DoneApplication already switched
  // ShowHint off. Switching it back on there would create a hint window that
  // DestroyComponents frees while Application still references it, which
  // ends in "Invalid pointer operation" on exit.
  if (Application = nil) or (csDestroying in Application.ComponentState) or
    Application.Terminated or not Application.ShowHint then
    Exit;
  Application.ShowHint := False;
  Application.ShowHint := True;
end;

procedure TERDHintStyle.Install;
begin
  if csDesigning in ComponentState then
    Exit;
  if GPreviousHintWindowClass = nil then
    GPreviousHintWindowClass := HintWindowClass;
  GActiveHintStyle := Self;
  HintWindowClass := TERDHintWindow;
  RecreateHintWindow;
end;

procedure TERDHintStyle.Uninstall;
begin
  if GActiveHintStyle <> Self then
    Exit;
  GActiveHintStyle := nil;
  if GPreviousHintWindowClass <> nil then
    HintWindowClass := GPreviousHintWindowClass;
  GPreviousHintWindowClass := nil;
  RecreateHintWindow;
end;

end.
