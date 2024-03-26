//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Common.pas
// CONTENTS       : Custom Component Common Types and Functions
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 22/03/2024
//------------------------------------------------------------------------------
unit OBD.CustomControl.Common;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, WinApi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Themes, Winapi.GDIPOBJ, Winapi.GDIPAPI;

//------------------------------------------------------------------------------
// FUNCTIONS
//------------------------------------------------------------------------------
function GetAppropriateColor(Color: TColor): TColor;
function SafeColorRefToARGB(Color: TColor): DWORD;
function FontStyle(Font: TFont): TFontStyle;
function CreateRoundRectPath(Rect: TGPRectF; Corner: Single) : TGPGraphicsPath; overload;
function CreateRoundRectPath(Rect: TGPRect; Corner: Single) : TGPGraphicsPath; overload;
function CreateBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;

implementation

//------------------------------------------------------------------------------
// GET APPROPRIATE COLOR (THEMED COLOR IF THEMES ENABLED)
//------------------------------------------------------------------------------
function GetAppropriateColor(Color: TColor): TColor;
begin
  // Check if VCL styles are enabled
  if StyleServices.Enabled then
    Result := StyleServices.GetSystemColor(Color)
  else
    Result := Color;
end;

//------------------------------------------------------------------------------
// CONVERT COLOR TO GDI+ COLOR
//------------------------------------------------------------------------------
function SafeColorRefToARGB(Color: TColor): DWORD;
var
  RGBColor: COLORREF;
begin
  // Convert TColor to a RGB color if it's a system color
  RGBColor := ColorToRGB(GetAppropriateColor(Color));
  // Now convert the RGB color to ARGB format expected by GDI+
  Result := (GetRValue(RGBColor) shl RedShift) or
            (GetGValue(RGBColor) shl GreenShift) or
            (GetBValue(RGBColor) shl BlueShift) or
            (AlphaMask);
end;

//------------------------------------------------------------------------------
// GET GDI+ FONTSTYLE FROM FONT
//------------------------------------------------------------------------------
function FontStyle(Font: TFont): TFontStyle;
begin
  if (fsBold in Font.Style) and (fsItalic in Font.Style) then
    Result := FontStyleBoldItalic
  else
  if (fsBold in Font.Style) then
    Result := FontStyleBold
  else
  if (fsItalic in Font.Style) then
    Result := FontStyleItalic
  else
  if (fsUnderline in Font.Style) then
    Result := FontStyleUnderline
  else
  if (fsStrikeOut in Font.Style) then
    Result := FontStyleStrikeout
  else
    Result := FontStyleRegular;
end;

//------------------------------------------------------------------------------
// GET GDI+ ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateRoundRectPath(Rect: TGPRectF; Corner: Single) : TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateRoundRectPath(Rect: TGPRect; Corner: Single) : TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ BACK BUTTON PATH
//------------------------------------------------------------------------------
function CreateBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
var
  MiddleY, Radius: Single;
begin
  Result := TGPGraphicsPath.Create;

  // Calculate the vertical middle point for the triangle peak
  MiddleY := Rect.Y + (Rect.Height / 2);
  // The radius for the arcs on the left side, half the width of the arrow part
  Radius := Rect.Height / 4;

  // Start the figure
  Result.StartFigure;

  // Add the arrow point arc
  Result.AddArc(Rect.X, MiddleY - (Corner / 2), Corner, Corner, 135, 45);
  // Top left arc
  Result.AddArc(Rect.X + (Rect.Height / 4), Rect.Y, Radius * 2, Radius * 2, 225, 45);
  // Top-right corner arc
  Result.AddArc(Rect.X + Rect.Width - Corner * 2, Rect.Y, Corner * 2, Corner * 2, 270, 90);
  // Bottom-right corner arc
  Result.AddArc(Rect.X + Rect.Width - Corner * 2, Rect.Y + Rect.Height - Corner * 2, Corner * 2, Corner * 2, 0, 90);
  // Bottom left arc
  Result.AddArc(Rect.X + (Rect.Height / 4), Rect.Y + Rect.Height - Radius * 2, Radius * 2, Radius * 2, 90, 45);

  // Flatten
  Result.Flatten;
  // Close the figure
  Result.CloseFigure;
end;


end.
