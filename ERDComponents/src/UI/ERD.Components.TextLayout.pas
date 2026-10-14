unit ERD.Components.TextLayout;
// DirectWrite layout and COLR colour layers over a GDI-compatible bitmap target.
// Internal COM projections preserve Windows SDK vtable order. Reserved slots are
// intentionally not callable; declarations are independent of Delphi SDK versions.
{$B-}
{$ALIGN 8}
interface
uses Winapi.Windows, System.SysUtils, System.Types, System.Math, Vcl.Controls, Vcl.Graphics;
type
  TERDDirectWriteLine = class
  private
    FLayout, FFactory, FInterop, FRenderingParams: IInterface;
    FCanvas: TCanvas;
    FText: UnicodeString;
    FLineHeight: Integer;
  public
    constructor Create(ACanvas: TCanvas; const Text: UnicodeString);
    function X(Position: Integer): Integer;
    function Width: Integer;
    procedure Range(Position, Count: Integer; out Left, Right: Integer);
    procedure Draw(X, Y: Integer);
  end;
function ERDTextLayoutABIValid: Boolean;
implementation
type
  TERDWriteMatrix = record M11, M12, M21, M22, DX, DY: Single; end;
  TERDWriteMetrics = record
    Left, Top, Width, WidthTrailing, Height, LayoutWidth, LayoutHeight: Single;
    MaxBidiDepth, LineCount: Cardinal;
  end;
  TERDWriteHit = record
    Position, Count: Cardinal;
    Left, Top, Width, Height: Single;
    BidiLevel: Cardinal;
    IsText, IsTrimmed: BOOL;
  end;
  TERDWriteRange = record Start, Count: Cardinal; end;
  TERDWriteGlyphRun = record
    FontFace: Pointer;
    FontSize: Single;
    GlyphCount: Cardinal;
    Indices, Advances, Offsets: Pointer;
    Sideways: BOOL;
    BidiLevel: Cardinal;
  end;
  PERDWriteGlyphRun = ^TERDWriteGlyphRun;
  TERDWriteColorRun = record
    GlyphRun: TERDWriteGlyphRun;
    Description: Pointer;
    X, Y: Single;
    Red, Green, Blue, Alpha: Single;
    PaletteIndex: Word;
  end;
  PERDWriteColorRun = ^TERDWriteColorRun;
  IERDWriteFormat = interface;
  IERDWriteLayout = interface;
  IERDWriteInterop = interface;
  IERDWriteTarget = interface;
  IERDWriteRenderer = interface;
  IERDWriteColors = interface;
  IERDWriteFormat = interface(IInterface)
    ['{9C906818-31D7-4FD3-A151-7C5E225DB55A}']
    procedure ReservedIERDWriteFormat_SetTextAlignment; stdcall;
    procedure ReservedIERDWriteFormat_SetParagraphAlignment; stdcall;
    function SetWordWrapping(Value: Integer): HRESULT; stdcall;
    procedure ReservedIERDWriteFormat_SetReadingDirection; stdcall;
    procedure ReservedIERDWriteFormat_SetFlowDirection; stdcall;
    procedure ReservedIERDWriteFormat_SetIncrementalTabStop; stdcall;
    procedure ReservedIERDWriteFormat_SetTrimming; stdcall;
    function SetLineSpacing(Method: Integer; Height, Baseline: Single): HRESULT; stdcall;
    procedure ReservedIERDWriteFormat_GetTextAlignment; stdcall;
    procedure ReservedIERDWriteFormat_GetParagraphAlignment; stdcall;
    procedure ReservedIERDWriteFormat_GetWordWrapping; stdcall;
    procedure ReservedIERDWriteFormat_GetReadingDirection; stdcall;
    procedure ReservedIERDWriteFormat_GetFlowDirection; stdcall;
    procedure ReservedIERDWriteFormat_GetIncrementalTabStop; stdcall;
    procedure ReservedIERDWriteFormat_GetTrimming; stdcall;
    procedure ReservedIERDWriteFormat_GetLineSpacing; stdcall;
    procedure ReservedIERDWriteFormat_GetFontCollection; stdcall;
    procedure ReservedIERDWriteFormat_GetFontFamilyNameLength; stdcall;
    procedure ReservedIERDWriteFormat_GetFontFamilyName; stdcall;
    procedure ReservedIERDWriteFormat_GetFontWeight; stdcall;
    procedure ReservedIERDWriteFormat_GetFontStyle; stdcall;
    procedure ReservedIERDWriteFormat_GetFontStretch; stdcall;
    procedure ReservedIERDWriteFormat_GetFontSize; stdcall;
    procedure ReservedIERDWriteFormat_GetLocaleNameLength; stdcall;
    procedure ReservedIERDWriteFormat_GetLocaleName; stdcall;
  end;
  IERDWriteLayout = interface(IERDWriteFormat)
    ['{53737037-6D14-410B-9BFE-0B18B7B70961}']
    procedure ReservedIERDWriteLayout_SetMaxWidth; stdcall;
    procedure ReservedIERDWriteLayout_SetMaxHeight; stdcall;
    procedure ReservedIERDWriteLayout_SetFontCollection; stdcall;
    procedure ReservedIERDWriteLayout_SetFontFamilyName; stdcall;
    procedure ReservedIERDWriteLayout_SetFontWeight; stdcall;
    procedure ReservedIERDWriteLayout_SetFontStyle; stdcall;
    procedure ReservedIERDWriteLayout_SetFontStretch; stdcall;
    procedure ReservedIERDWriteLayout_SetFontSize; stdcall;
    function SetUnderline(Value: BOOL; Range: TERDWriteRange): HRESULT; stdcall;
    function SetStrikethrough(Value: BOOL; Range: TERDWriteRange): HRESULT; stdcall;
    procedure ReservedIERDWriteLayout_SetDrawingEffect; stdcall;
    procedure ReservedIERDWriteLayout_SetInlineObject; stdcall;
    procedure ReservedIERDWriteLayout_SetTypography; stdcall;
    procedure ReservedIERDWriteLayout_SetLocaleName; stdcall;
    procedure ReservedIERDWriteLayout_GetMaxWidth; stdcall;
    procedure ReservedIERDWriteLayout_GetMaxHeight; stdcall;
    procedure ReservedIERDWriteLayout_GetFontCollection; stdcall;
    procedure ReservedIERDWriteLayout_GetFontFamilyNameLength; stdcall;
    procedure ReservedIERDWriteLayout_GetFontFamilyName; stdcall;
    procedure ReservedIERDWriteLayout_GetFontWeight; stdcall;
    procedure ReservedIERDWriteLayout_GetFontStyle; stdcall;
    procedure ReservedIERDWriteLayout_GetFontStretch; stdcall;
    procedure ReservedIERDWriteLayout_GetFontSize; stdcall;
    procedure ReservedIERDWriteLayout_GetUnderline; stdcall;
    procedure ReservedIERDWriteLayout_GetStrikethrough; stdcall;
    procedure ReservedIERDWriteLayout_GetDrawingEffect; stdcall;
    procedure ReservedIERDWriteLayout_GetInlineObject; stdcall;
    procedure ReservedIERDWriteLayout_GetTypography; stdcall;
    procedure ReservedIERDWriteLayout_GetLocaleNameLength; stdcall;
    procedure ReservedIERDWriteLayout_GetLocaleName; stdcall;
    function Draw(Context: Pointer; const Renderer: IERDWriteRenderer; X, Y: Single): HRESULT; stdcall;
    procedure ReservedIERDWriteLayout_GetLineMetrics; stdcall;
    function GetMetrics(out Metrics: TERDWriteMetrics): HRESULT; stdcall;
    procedure ReservedIERDWriteLayout_GetOverhangMetrics; stdcall;
    procedure ReservedIERDWriteLayout_GetClusterMetrics; stdcall;
    procedure ReservedIERDWriteLayout_DetermineMinWidth; stdcall;
    procedure ReservedIERDWriteLayout_HitTestPoint; stdcall;
    function HitTestTextPosition(Position: Cardinal; Trailing: BOOL; out X, Y: Single; out Hit: TERDWriteHit): HRESULT; stdcall;
    function HitTestTextRange(Position, Count: Cardinal; X, Y: Single; Hits: Pointer; Capacity: Cardinal; out Actual: Cardinal): HRESULT; stdcall;
  end;
  IERDWriteTarget = interface(IInterface)
    ['{5E5A32A3-8DFF-4773-9FF6-0696EAB77267}']
    function DrawGlyphRun(X, Y: Single; Mode: Integer; Run: PERDWriteGlyphRun; const Params: IInterface; Ink: COLORREF; Bounds: Pointer): HRESULT; stdcall;
    function GetMemoryDC: HDC; stdcall;
    procedure ReservedIERDWriteTarget_GetPixelsPerDip; stdcall;
    function SetPixelsPerDip(Value: Single): HRESULT; stdcall;
    procedure ReservedIERDWriteTarget_GetCurrentTransform; stdcall;
    procedure ReservedIERDWriteTarget_SetCurrentTransform; stdcall;
    procedure ReservedIERDWriteTarget_GetSize; stdcall;
    procedure ReservedIERDWriteTarget_Resize; stdcall;
  end;
  IERDWriteInterop = interface(IInterface)
    ['{1EDD9491-9853-4299-898F-6432983B6F3A}']
    procedure ReservedIERDWriteInterop_CreateFontFromLOGFONT; stdcall;
    procedure ReservedIERDWriteInterop_ConvertFontToLOGFONT; stdcall;
    procedure ReservedIERDWriteInterop_ConvertFontFaceToLOGFONT; stdcall;
    procedure ReservedIERDWriteInterop_CreateFontFaceFromHdc; stdcall;
    function CreateBitmapRenderTarget(DC: HDC; Width, Height: Cardinal; out Target: IERDWriteTarget): HRESULT; stdcall;
  end;
  IERDWriteFactory = interface(IInterface)
    ['{B859EE5A-D838-4B5B-A2E8-1ADC7D93DB48}']
    procedure ReservedIERDWriteFactory_GetSystemFontCollection; stdcall;
    procedure ReservedIERDWriteFactory_CreateCustomFontCollection; stdcall;
    procedure ReservedIERDWriteFactory_RegisterFontCollectionLoader; stdcall;
    procedure ReservedIERDWriteFactory_UnregisterFontCollectionLoader; stdcall;
    procedure ReservedIERDWriteFactory_CreateFontFileReference; stdcall;
    procedure ReservedIERDWriteFactory_CreateCustomFontFileReference; stdcall;
    procedure ReservedIERDWriteFactory_CreateFontFace; stdcall;
    function CreateRenderingParams(out Params: IInterface): HRESULT; stdcall;
    procedure ReservedIERDWriteFactory_CreateMonitorRenderingParams; stdcall;
    procedure ReservedIERDWriteFactory_CreateCustomRenderingParams; stdcall;
    procedure ReservedIERDWriteFactory_RegisterFontFileLoader; stdcall;
    procedure ReservedIERDWriteFactory_UnregisterFontFileLoader; stdcall;
    function CreateTextFormat(Name: PWideChar; const Collection: IInterface; Weight, Style, Stretch: Integer; Size: Single; Locale: PWideChar; out Format: IERDWriteFormat): HRESULT; stdcall;
    procedure ReservedIERDWriteFactory_CreateTypography; stdcall;
    function GetGdiInterop(out Interop: IERDWriteInterop): HRESULT; stdcall;
    function CreateTextLayout(Text: PWideChar; Count: Cardinal; const Format: IERDWriteFormat; Width, Height: Single; out Layout: IERDWriteLayout): HRESULT; stdcall;
    procedure ReservedIERDWriteFactory_CreateGdiCompatibleTextLayout; stdcall;
    procedure ReservedIERDWriteFactory_CreateEllipsisTrimmingSign; stdcall;
    procedure ReservedIERDWriteFactory_CreateTextAnalyzer; stdcall;
    procedure ReservedIERDWriteFactory_CreateNumberSubstitution; stdcall;
    procedure ReservedIERDWriteFactory_CreateGlyphRunAnalysis; stdcall;
  end;
  IERDWriteFactory1 = interface(IERDWriteFactory)
    ['{30572F99-DAC6-41DB-A16E-0486307E606A}']
    procedure ReservedIERDWriteFactory1_GetEudcFontCollection; stdcall;
    procedure ReservedIERDWriteFactory1_CreateCustomRenderingParams; stdcall;
  end;
  IERDWriteFactory2 = interface(IERDWriteFactory1)
    ['{0439FC60-CA44-4994-8DEE-3A9AF7B732EC}']
    procedure ReservedIERDWriteFactory2_GetSystemFontFallback; stdcall;
    procedure ReservedIERDWriteFactory2_CreateFontFallbackBuilder; stdcall;
    function TranslateColorGlyphRun(X, Y: Single; Run: PERDWriteGlyphRun; Description: Pointer; Mode: Integer; Transform: Pointer; Palette: Cardinal; out Colors: IERDWriteColors): HRESULT; stdcall;
    procedure ReservedIERDWriteFactory2_CreateCustomRenderingParams; stdcall;
    procedure ReservedIERDWriteFactory2_CreateGlyphRunAnalysis; stdcall;
  end;
  IERDWriteColors = interface(IInterface)
    ['{D31FBE17-F157-41A2-8D24-CB779E0560E8}']
    function MoveNext(out HasRun: BOOL): HRESULT; stdcall;
    function GetCurrentRun(out Run: PERDWriteColorRun): HRESULT; stdcall;
  end;
  IERDWriteSnapping = interface(IInterface)
    ['{EAF3A2DA-ECF4-4D24-B644-B34F6842024B}']
    function IsPixelSnappingDisabled(Context: Pointer; out Disabled: BOOL): HRESULT; stdcall;
    function GetCurrentTransform(Context: Pointer; out Transform: TERDWriteMatrix): HRESULT; stdcall;
    function GetPixelsPerDip(Context: Pointer; out Value: Single): HRESULT; stdcall;
  end;
  IERDWriteRenderer = interface(IERDWriteSnapping)
    ['{EF8A8135-5CC6-45FE-8825-C5A0724EB819}']
    function DrawGlyphRun(Context: Pointer; X, Y: Single; Mode: Integer;
      Run: PERDWriteGlyphRun; Description: Pointer; const Effect: IInterface): HRESULT; stdcall;
    function DrawUnderline(Context: Pointer; X, Y: Single; Underline: Pointer;
      const Effect: IInterface): HRESULT; stdcall;
    function DrawStrikethrough(Context: Pointer; X, Y: Single; Strike: Pointer;
      const Effect: IInterface): HRESULT; stdcall;
    function DrawInlineObject(Context: Pointer; X, Y: Single; const Obj: IInterface;
      Sideways, RTL: BOOL; const Effect: IInterface): HRESULT; stdcall;
  end;
  TERDWriteDecoration = record Width, Thickness, Offset: Single; end;
  PERDWriteDecoration = ^TERDWriteDecoration;
  TERDWriteRenderer = class(TInterfacedObject, IERDWriteSnapping, IERDWriteRenderer)
  private
    FTarget: IERDWriteTarget;
    FColors: IERDWriteFactory2;
    FParams: IInterface;
    FInk: COLORREF;
    function Decoration(X, Y: Single; Data: Pointer): HRESULT;
  public
    constructor Create(const Target: IERDWriteTarget; const Factory: IInterface;
      const Params: IInterface; Ink: COLORREF);
    function IsPixelSnappingDisabled(Context: Pointer; out Disabled: BOOL): HRESULT; stdcall;
    function GetCurrentTransform(Context: Pointer; out Transform: TERDWriteMatrix): HRESULT; stdcall;
    function GetPixelsPerDip(Context: Pointer; out Value: Single): HRESULT; stdcall;
    function DrawGlyphRun(Context: Pointer; X, Y: Single; Mode: Integer;
      Run: PERDWriteGlyphRun; Description: Pointer; const Effect: IInterface): HRESULT; stdcall;
    function DrawUnderline(Context: Pointer; X, Y: Single; Underline: Pointer;
      const Effect: IInterface): HRESULT; stdcall;
    function DrawStrikethrough(Context: Pointer; X, Y: Single; Strike: Pointer;
      const Effect: IInterface): HRESULT; stdcall;
    function DrawInlineObject(Context: Pointer; X, Y: Single; const Obj: IInterface;
      Sideways, RTL: BOOL; const Effect: IInterface): HRESULT; stdcall;
  end;
  TERD2DPoint = record X, Y: Single; end;
  TERD2DSize = record Width, Height: Cardinal; end;
  TERD2DBitmapProps = record Format, AlphaMode: Integer; DpiX, DpiY: Single; end;
  TERD2DTargetProps = record
    Kind, Format, AlphaMode: Integer;
    DpiX, DpiY: Single;
    Usage, MinLevel: Integer;
  end;
  TERD2DColor = record Red, Green, Blue, Alpha: Single; end;
  IERD2DDCTarget = interface;
  IERD2DResource = interface(IInterface)
    ['{2CD90691-12E2-11DC-9FED-001143A055F9}']
    procedure ReservedIERD2DResource_GetFactory; stdcall;
  end;
  IERD2DTarget = interface(IERD2DResource)
    ['{2CD90694-12E2-11DC-9FED-001143A055F9}']
    function CreateBitmap(Size: TERD2DSize; Data: Pointer; Pitch: Cardinal; const Props: TERD2DBitmapProps; out Bitmap: IInterface): HRESULT; stdcall;
    procedure ReservedIERD2DTarget_CreateBitmapFromWicBitmap; stdcall;
    procedure ReservedIERD2DTarget_CreateSharedBitmap; stdcall;
    procedure ReservedIERD2DTarget_CreateBitmapBrush; stdcall;
    function CreateSolidColorBrush(const Color: TERD2DColor; Props: Pointer; out Brush: IInterface): HRESULT; stdcall;
    procedure ReservedIERD2DTarget_CreateGradientStopCollection; stdcall;
    procedure ReservedIERD2DTarget_CreateLinearGradientBrush; stdcall;
    procedure ReservedIERD2DTarget_CreateRadialGradientBrush; stdcall;
    procedure ReservedIERD2DTarget_CreateCompatibleRenderTarget; stdcall;
    procedure ReservedIERD2DTarget_CreateLayer; stdcall;
    procedure ReservedIERD2DTarget_CreateMesh; stdcall;
    procedure ReservedIERD2DTarget_DrawLine; stdcall;
    procedure ReservedIERD2DTarget_DrawRectangle; stdcall;
    procedure ReservedIERD2DTarget_FillRectangle; stdcall;
    procedure ReservedIERD2DTarget_DrawRoundedRectangle; stdcall;
    procedure ReservedIERD2DTarget_FillRoundedRectangle; stdcall;
    procedure ReservedIERD2DTarget_DrawEllipse; stdcall;
    procedure ReservedIERD2DTarget_FillEllipse; stdcall;
    procedure ReservedIERD2DTarget_DrawGeometry; stdcall;
    procedure ReservedIERD2DTarget_FillGeometry; stdcall;
    procedure ReservedIERD2DTarget_FillMesh; stdcall;
    procedure ReservedIERD2DTarget_FillOpacityMask; stdcall;
    procedure DrawBitmap(const Bitmap: IInterface; Destination: Pointer; Opacity: Single; Interpolation: Integer; Source: Pointer); stdcall;
    procedure ReservedIERD2DTarget_DrawText; stdcall;
    procedure DrawTextLayout(Origin: TERD2DPoint; const Layout, Brush: IInterface; Options: Cardinal); stdcall;
    procedure ReservedIERD2DTarget_DrawGlyphRun; stdcall;
    procedure ReservedIERD2DTarget_SetTransform; stdcall;
    procedure ReservedIERD2DTarget_GetTransform; stdcall;
    procedure ReservedIERD2DTarget_SetAntialiasMode; stdcall;
    procedure ReservedIERD2DTarget_GetAntialiasMode; stdcall;
    procedure SetTextAntialiasMode(Value: Integer); stdcall;
    procedure ReservedIERD2DTarget_GetTextAntialiasMode; stdcall;
    procedure ReservedIERD2DTarget_SetTextRenderingParams; stdcall;
    procedure ReservedIERD2DTarget_GetTextRenderingParams; stdcall;
    procedure ReservedIERD2DTarget_SetTags; stdcall;
    procedure ReservedIERD2DTarget_GetTags; stdcall;
    procedure ReservedIERD2DTarget_PushLayer; stdcall;
    procedure ReservedIERD2DTarget_PopLayer; stdcall;
    procedure ReservedIERD2DTarget_Flush; stdcall;
    procedure ReservedIERD2DTarget_SaveDrawingState; stdcall;
    procedure ReservedIERD2DTarget_RestoreDrawingState; stdcall;
    procedure ReservedIERD2DTarget_PushAxisAlignedClip; stdcall;
    procedure ReservedIERD2DTarget_PopAxisAlignedClip; stdcall;
    procedure ReservedIERD2DTarget_Clear; stdcall;
    procedure BeginDraw; stdcall;
    function EndDraw(Tag1, Tag2: Pointer): HRESULT; stdcall;
    procedure ReservedIERD2DTarget_GetPixelFormat; stdcall;
    procedure ReservedIERD2DTarget_SetDpi; stdcall;
    procedure ReservedIERD2DTarget_GetDpi; stdcall;
    procedure ReservedIERD2DTarget_GetSize; stdcall;
    procedure ReservedIERD2DTarget_GetPixelSize; stdcall;
    procedure ReservedIERD2DTarget_GetMaximumBitmapSize; stdcall;
    procedure ReservedIERD2DTarget_IsSupported; stdcall;
  end;
  IERD2DDCTarget = interface(IERD2DTarget)
    ['{1C51BC64-DE61-46FD-9899-63A5D8F03950}']
    function BindDC(DC: HDC; const Bounds: TRect): HRESULT; stdcall;
  end;
  IERD2DFactory = interface(IInterface)
    ['{06152247-6F50-465A-9245-118BFD3B6007}']
    procedure ReservedIERD2DFactory_ReloadSystemMetrics; stdcall;
    procedure ReservedIERD2DFactory_GetDesktopDpi; stdcall;
    procedure ReservedIERD2DFactory_CreateRectangleGeometry; stdcall;
    procedure ReservedIERD2DFactory_CreateRoundedRectangleGeometry; stdcall;
    procedure ReservedIERD2DFactory_CreateEllipseGeometry; stdcall;
    procedure ReservedIERD2DFactory_CreateGeometryGroup; stdcall;
    procedure ReservedIERD2DFactory_CreateTransformedGeometry; stdcall;
    procedure ReservedIERD2DFactory_CreatePathGeometry; stdcall;
    procedure ReservedIERD2DFactory_CreateStrokeStyle; stdcall;
    procedure ReservedIERD2DFactory_CreateDrawingStateBlock; stdcall;
    procedure ReservedIERD2DFactory_CreateWicBitmapRenderTarget; stdcall;
    procedure ReservedIERD2DFactory_CreateHwndRenderTarget; stdcall;
    procedure ReservedIERD2DFactory_CreateDxgiSurfaceRenderTarget; stdcall;
    function CreateDCRenderTarget(const Props: TERD2DTargetProps; out Target: IERD2DDCTarget): HRESULT; stdcall;
  end;
var Shared2DFactory: IERD2DFactory;
function DWriteCreateFactory(Kind: Integer; const IID: TGUID; out Factory: IInterface): HRESULT;
  stdcall; external 'dwrite.dll';
function D2D1CreateFactory(Kind: Integer; const IID: TGUID; Options: Pointer; out Factory: IInterface): HRESULT;
  stdcall; external 'd2d1.dll';
procedure Checked(Status: HRESULT);
begin
  if Status < 0 then raise EInvalidOperation.CreateFmt('DirectWrite failed ($%.8x)', [Cardinal(Status)]);
end;
function TryNativeColorDraw(const Layout: IInterface; DC: HDC; const Clip: TRect;
  X, Y: Integer): Boolean;
var FactoryObject, Bitmap, Brush: IInterface; Factory: IERD2DFactory; Target: IERD2DDCTarget;
    Props: TERD2DTargetProps; BitmapProps: TERD2DBitmapProps; Size: TERD2DSize;
    Origin: TERD2DPoint; Color: TERD2DColor; Ink: COLORREF;
    Info: TBitmapInfo; Pixels: Pointer; Memory: HDC; Image, Previous: HGDIOBJ;
begin
  Result := False; Memory := 0; Image := 0; Previous := 0;
  try
    try
      Memory := CreateCompatibleDC(DC); if Memory = 0 then Exit;
      Info := Default(TBitmapInfo); Info.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
      Info.bmiHeader.biWidth := Clip.Width; Info.bmiHeader.biHeight := -Clip.Height;
      Info.bmiHeader.biPlanes := 1; Info.bmiHeader.biBitCount := 32;
      Info.bmiHeader.biCompression := BI_RGB;
      Image := CreateDIBSection(DC, Info, DIB_RGB_COLORS, Pixels, 0, 0); if Image = 0 then Exit;
      Previous := SelectObject(Memory, Image);
      if (Previous = 0) or (Previous = HGDI_ERROR) then Exit;
      if not BitBlt(Memory, 0, 0, Clip.Width, Clip.Height, DC, Clip.Left, Clip.Top, SRCCOPY) then Exit;
      // Flush GDI before handing its DIB pixels to Direct2D.
      GdiFlush;
      if Shared2DFactory = nil then
      begin
        Checked(D2D1CreateFactory(0, IERD2DFactory, nil, FactoryObject));
        Shared2DFactory := FactoryObject as IERD2DFactory;
      end;
      Factory := Shared2DFactory;
      Props := Default(TERD2DTargetProps);
      Props.Kind := 1; Props.Format := 87; Props.AlphaMode := 3;
      Props.DpiX := 96; Props.DpiY := 96;
      Checked(Factory.CreateDCRenderTarget(Props, Target));
      Checked(Target.BindDC(Memory, Rect(0, 0, Clip.Width, Clip.Height)));
      BitmapProps.Format := 87; BitmapProps.AlphaMode := 3;
      BitmapProps.DpiX := 96; BitmapProps.DpiY := 96;
      Size.Width := Clip.Width; Size.Height := Clip.Height;
      Checked(Target.CreateBitmap(Size, Pixels, Clip.Width * 4, BitmapProps, Bitmap));
      Ink := GetTextColor(DC);
      Color.Red := GetRValue(Ink) / 255; Color.Green := GetGValue(Ink) / 255;
      Color.Blue := GetBValue(Ink) / 255; Color.Alpha := 1;
      Checked(Target.CreateSolidColorBrush(Color, nil, Brush));
      Origin.X := X - Clip.Left; Origin.Y := Y - Clip.Top;
      Target.SetTextAntialiasMode(2); // Grayscale: stable on both light and dark backgrounds.
      Target.BeginDraw;
      Target.DrawBitmap(Bitmap, nil, 1, 0, nil);
      // CLIP plus ENABLE_COLOR_FONT; native Windows handles COLR, bitmap and SVG
      // formats supported by the installed OS, including newer colour-font formats.
      Target.DrawTextLayout(Origin, Layout, Brush, 2 or 4);
      Checked(Target.EndDraw(nil, nil));
      Result := BitBlt(DC, Clip.Left, Clip.Top, Clip.Width, Clip.Height, Memory, 0, 0, SRCCOPY);
    except Result := False; end;
  finally
    Brush := nil; Bitmap := nil; Target := nil; Factory := nil; FactoryObject := nil;
    if (Previous <> 0) and (Previous <> HGDI_ERROR) then SelectObject(Memory, Previous);
    if Image <> 0 then DeleteObject(Image);
    if Memory <> 0 then DeleteDC(Memory);
  end;
end;
function ERDTextLayoutABIValid: Boolean;
begin
  Result := (SizeOf(TERDWriteMetrics) = 36) and (SizeOf(TERDWriteHit) = 36) and
    (SizeOf(TERDWriteRange) = 8) and (SizeOf(TERDWriteMatrix) = 24) and
    (SizeOf(TERD2DPoint) = 8) and (SizeOf(TERD2DSize) = 8) and
    (SizeOf(TERD2DBitmapProps) = 16) and (SizeOf(TERD2DTargetProps) = 28) and
    (SizeOf(TERD2DColor) = 16);
  {$IFDEF WIN64}
  Result := Result and (SizeOf(TERDWriteGlyphRun) = 48) and (SizeOf(TERDWriteColorRun) = 88);
  {$ELSE}
  Result := Result and (SizeOf(TERDWriteGlyphRun) = 32) and (SizeOf(TERDWriteColorRun) = 64);
  {$ENDIF}
end;
constructor TERDDirectWriteLine.Create(ACanvas: TCanvas; const Text: UnicodeString);
var Factory: IERDWriteFactory; Format: IERDWriteFormat; Layout: IERDWriteLayout;
    Interop: IERDWriteInterop; Weight, Style: Integer; Metrics: TTextMetric;
    AllText: TERDWriteRange;
begin
  inherited Create;
  if not ERDTextLayoutABIValid then raise EInvalidOperation.Create('Invalid DirectWrite record layout');
  FCanvas := ACanvas; FText := Text;
  Checked(DWriteCreateFactory(0, IERDWriteFactory, FFactory));
  Factory := FFactory as IERDWriteFactory;
  Weight := 400; if fsBold in ACanvas.Font.Style then Weight := 700;
  Style := 0; if fsItalic in ACanvas.Font.Style then Style := 2;
  Checked(Factory.CreateTextFormat(PWideChar(ACanvas.Font.Name), nil, Weight,
    Style, 5, Max(1, Abs(ACanvas.Font.Height)), 'en-us', Format));
  Checked(Format.SetWordWrapping(1));
  if not GetTextMetrics(ACanvas.Handle, Metrics) then RaiseLastOSError;
  FLineHeight := Metrics.tmHeight;
  Checked(Format.SetLineSpacing(1, Metrics.tmHeight, Metrics.tmAscent));
  Checked(Factory.CreateTextLayout(PWideChar(FText), Length(FText), Format, 1000000, 1000000, Layout));
  AllText.Start := 0; AllText.Count := Length(FText);
  if fsUnderline in ACanvas.Font.Style then Checked(Layout.SetUnderline(True, AllText));
  if fsStrikeOut in ACanvas.Font.Style then Checked(Layout.SetStrikethrough(True, AllText));
  FLayout := Layout;
  Checked(Factory.GetGdiInterop(Interop)); FInterop := Interop;
  Checked(Factory.CreateRenderingParams(FRenderingParams));
end;
function TERDDirectWriteLine.X(Position: Integer): Integer;
var PX, PY: Single; Hit: TERDWriteHit; CP: Integer; Trailing: BOOL;
begin
  Result := 0; if FText = '' then Exit;
  CP := EnsureRange(Position, 0, Length(FText)); Trailing := CP = Length(FText);
  if Trailing then Dec(CP);
  Checked((FLayout as IERDWriteLayout).HitTestTextPosition(CP, Trailing, PX, PY, Hit));
  Result := Round(PX);
end;
function TERDDirectWriteLine.Width: Integer;
var Metrics: TERDWriteMetrics;
begin Checked((FLayout as IERDWriteLayout).GetMetrics(Metrics)); Result := Ceil(Metrics.WidthTrailing); end;
procedure TERDDirectWriteLine.Range(Position, Count: Integer; out Left, Right: Integer);
var Hits: TArray<TERDWriteHit>; Actual, I: Cardinal; Status: HRESULT;
    L, R: Single;
begin
  Left := X(Position); Right := Left; if Count <= 0 then Exit;
  Position := EnsureRange(Position, 0, Length(FText)); Count := Min(Count, Length(FText) - Position);
  if Count = 0 then Exit;
  Actual := 0;
  Status := (FLayout as IERDWriteLayout).HitTestTextRange(Position, Count, 0, 0, nil, 0, Actual);
  if Actual = 0 then begin Checked(Status); Exit; end;
  SetLength(Hits, Actual);
  Status := (FLayout as IERDWriteLayout).HitTestTextRange(Position, Count, 0, 0, @Hits[0], Length(Hits), Actual);
  Checked(Status); L := Hits[0].Left; R := L + Hits[0].Width;
  if Actual > 1 then for I := 1 to Actual - 1 do
  begin L := Min(L, Hits[I].Left); R := Max(R, Hits[I].Left + Hits[I].Width); end;
  Left := Floor(L); Right := Ceil(R);
end;
procedure TERDDirectWriteLine.Draw(X, Y: Integer);
var Clip: TRect; Target: IERDWriteTarget; Renderer: IERDWriteRenderer; DC: HDC;
begin
  if FText = '' then Exit;
  if GetClipBox(FCanvas.Handle, Clip) <= NULLREGION then Exit;
  Clip.Top := Max(Clip.Top, Y); Clip.Bottom := Min(Clip.Bottom, Y + FLineHeight);
  if (Clip.Width <= 0) or (Clip.Height <= 0) then Exit;
  if TryNativeColorDraw(FLayout, FCanvas.Handle, Clip, X, Y) then Exit;
  Checked((FInterop as IERDWriteInterop).CreateBitmapRenderTarget(FCanvas.Handle, Clip.Width, Clip.Height, Target));
  Checked(Target.SetPixelsPerDip(1)); DC := Target.GetMemoryDC;
  if not BitBlt(DC, 0, 0, Clip.Width, Clip.Height, FCanvas.Handle, Clip.Left, Clip.Top, SRCCOPY) then
    RaiseLastOSError;
  Renderer := TERDWriteRenderer.Create(Target, FFactory, FRenderingParams, GetTextColor(FCanvas.Handle));
  Checked((FLayout as IERDWriteLayout).Draw(nil, Renderer, X - Clip.Left, Y - Clip.Top));
  if not BitBlt(FCanvas.Handle, Clip.Left, Clip.Top, Clip.Width, Clip.Height, DC, 0, 0, SRCCOPY) then
    RaiseLastOSError;
end;
constructor TERDWriteRenderer.Create(const Target: IERDWriteTarget; const Factory: IInterface;
  const Params: IInterface; Ink: COLORREF);
begin
  inherited Create; FTarget := Target; FParams := Params; FInk := Ink;
  Supports(Factory, IERDWriteFactory2, FColors);
end;
function TERDWriteRenderer.IsPixelSnappingDisabled(Context: Pointer; out Disabled: BOOL): HRESULT;
begin Disabled := False; Result := S_OK; end;
function TERDWriteRenderer.GetCurrentTransform(Context: Pointer; out Transform: TERDWriteMatrix): HRESULT;
begin Transform := Default(TERDWriteMatrix); Transform.M11 := 1; Transform.M22 := 1; Result := S_OK; end;
function TERDWriteRenderer.GetPixelsPerDip(Context: Pointer; out Value: Single): HRESULT;
begin Value := 1; Result := S_OK; end;
function TERDWriteRenderer.DrawGlyphRun(Context: Pointer; X, Y: Single; Mode: Integer;
  Run: PERDWriteGlyphRun; Description: Pointer; const Effect: IInterface): HRESULT;
var Colors: IERDWriteColors; HasRun: BOOL; Layer: PERDWriteColorRun; Ink: COLORREF;
begin
  // No Pascal exceptions may cross a native COM callback boundary.
  try
    Colors := nil;
    if FColors <> nil then
      FColors.TranslateColorGlyphRun(X, Y, Run, Description, Mode, nil, 0, Colors);
    if Colors = nil then Exit(FTarget.DrawGlyphRun(X, Y, Mode, Run, FParams, FInk, nil));
    while True do
    begin
      Result := Colors.MoveNext(HasRun); if (Result < 0) or not HasRun then Exit;
      Result := Colors.GetCurrentRun(Layer); if Result < 0 then Exit;
      if (Layer.PaletteIndex <> $FFFF) and (Layer.Alpha <= 0) then Continue;
      Ink := FInk;
      if Layer.PaletteIndex <> $FFFF then
        Ink := RGB(Round(EnsureRange(Layer.Red, 0.0, 1.0) * 255),
          Round(EnsureRange(Layer.Green, 0.0, 1.0) * 255),
          Round(EnsureRange(Layer.Blue, 0.0, 1.0) * 255));
      Result := FTarget.DrawGlyphRun(Layer.X, Layer.Y, Mode, @Layer.GlyphRun, FParams, Ink, nil);
      if Result < 0 then Exit;
    end;
  except Result := E_FAIL; end;
end;
function TERDWriteRenderer.Decoration(X, Y: Single; Data: Pointer): HRESULT;
var Item: PERDWriteDecoration; R: TRect; Brush: HBRUSH;
begin
  try
    Item := Data; R := Rect(Floor(X), Floor(Y + Item.Offset),
      Ceil(X + Item.Width), Ceil(Y + Item.Offset + Max(1.0, Item.Thickness)));
    Brush := CreateSolidBrush(FInk);
    try FillRect(FTarget.GetMemoryDC, R, Brush); finally DeleteObject(Brush); end;
    Result := S_OK;
  except Result := E_FAIL; end;
end;
function TERDWriteRenderer.DrawUnderline(Context: Pointer; X, Y: Single; Underline: Pointer;
  const Effect: IInterface): HRESULT;
begin Result := Decoration(X, Y, Underline); end;
function TERDWriteRenderer.DrawStrikethrough(Context: Pointer; X, Y: Single; Strike: Pointer;
  const Effect: IInterface): HRESULT;
begin Result := Decoration(X, Y, Strike); end;
function TERDWriteRenderer.DrawInlineObject(Context: Pointer; X, Y: Single; const Obj: IInterface;
  Sideways, RTL: BOOL; const Effect: IInterface): HRESULT;
begin Result := E_NOTIMPL; end;
end.
