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
  Vcl.Graphics, Vcl.Themes, Winapi.GDIPAPI;

//------------------------------------------------------------------------------
// FUNCTIONS
//------------------------------------------------------------------------------
function GetAppropriateColor(Color: TColor): TColor;
function SafeColorRefToARGB(Color: TColor): DWORD;
function FontStyle(Font: TFont): TFontStyle;

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

end.
