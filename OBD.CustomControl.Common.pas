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
  WinApi.Windows, System.SysUtils, System.Types, System.UITypes,
  Vcl.Graphics, Vcl.Themes, Winapi.GDIPOBJ, Winapi.GDIPAPI;

//------------------------------------------------------------------------------
// FUNCTIONS
//------------------------------------------------------------------------------
function GetAppropriateColor(Color: TColor): TColor;
function SafeColorRefToARGB(Color: TColor): DWORD;
function FontStyle(Font: TFont): TFontStyle;
function CreateRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateRoundRectPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareRoundRectPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateBackButtonPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareBackButtonPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateTabLeftPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareTabLeftPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateTabCenterPath(Rect: TGPRectF): TGPGraphicsPath; overload;
function CreateTabCenterPath(Rect: TGPRect): TGPGraphicsPath; overload;
function CreateGlareTabCenterPath(Rect: TGPRectF): TGPGraphicsPath; overload;
function CreateGlareTabCenterPath(Rect: TGPRect): TGPGraphicsPath; overload;
function CreateTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateTabRightPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateGlareTabRightPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateBatteryPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath; overload;
function CreateBatteryPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath; overload;
function CreateBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath; overload;
function CreateBatteryPercentagePath(Rect: TGPRect; Percentage: Single; Distance: Single = 2): TGPGraphicsPath; overload;
function CreateGlareBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath; overload;
function CreateGlareBatteryPercentagePath(Rect: TGPRect; Percentage: Single; Distance: Single = 2): TGPGraphicsPath; overload;
function CreateGlareBatteryPercentageRect(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPRectF;

function CreateVehicleBatteryPath(Rect: TGPRectF): TGPGraphicsPath; overload;
function CreateVehicleBatteryPath(Rect: TGPRect): TGPGraphicsPath; overload;

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
function CreateRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  // Add arc bottom left corner
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateRoundRectPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  // Add arc bottom left corner
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ GLARE ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateGlareRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add line from top right corner to center right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height / 2, Rect.X, Rect.Y + Rect.Height / 2);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ GLARE ROUNDRECT PATH
//------------------------------------------------------------------------------
function CreateGlareRoundRectPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add line from top right corner to center right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height / 2, Rect.X, Rect.Y + Rect.Height / 2);
  // Close the figure
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

//------------------------------------------------------------------------------
// GET GDI+ BACK BUTTON PATH
//------------------------------------------------------------------------------
function CreateBackButtonPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
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

//------------------------------------------------------------------------------
// GET GDI+ BACK BUTTON PATH
//------------------------------------------------------------------------------
function CreateGlareBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
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
  //
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Corner, Rect.X + Rect.Width, Rect.Y + Rect.Height / 2);

  // Flatten
  Result.Flatten;
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ BACK BUTTON PATH
//------------------------------------------------------------------------------
function CreateGlareBackButtonPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
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
  //
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Corner, Rect.X + Rect.Width, Rect.Y + Rect.Height / 2);

  // Flatten
  Result.Flatten;
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.Height);
  // Add arc bottom left corner
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateTabLeftPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.Height);
  // Add arc bottom left corner
  Result.AddArc(Rect.X, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateGlareTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2));
  // Add straight line at the bottom
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB LEFT PATH
//------------------------------------------------------------------------------
function CreateGlareTabLeftPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(Rect.X, Rect.Y, Corner, Corner, 180, 90);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2));
  // Add straight line at the bottom
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateTabCenterPath(Rect: TGPRectF): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line on the left
  Result.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.Height);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateTabCenterPath(Rect: TGPRect): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line on the left
  Result.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + Rect.Height);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateGlareTabCenterPath(Rect: TGPRectF): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line on the left
  Result.AddLine(Rect.X, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB CENTER PATH
//------------------------------------------------------------------------------
function CreateGlareTabCenterPath(Rect: TGPRect): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line on the left
  Result.AddLine(Rect.X, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y);
  // Add straight line on the right
  Result.AddLine(Rect.X + Rect.Width, Rect.Y, Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line to the left
  Result.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateTabRightPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line to the left
  Result.AddLine(Rect.X, Rect.Y + Rect.Height, Rect.X, Rect.Y);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, (Rect.Y + Rect.Height) - Corner, Corner, Corner, 0, 90);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateGlareTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line to the left
  Result.AddLine(Rect.X, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add straight line at the bottom
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ TAB RIGHT PATH
//------------------------------------------------------------------------------
function CreateGlareTabRightPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add straight line to the left
  Result.AddLine(Rect.X, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y);
  // Add arc top right corner
  Result.AddArc((Rect.X + Rect.Width) - Corner, Rect.Y, Corner, Corner, 270, 90);
  // Add straight line at the bottom
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + (Rect.Height / 2), Rect.X, Rect.Y + (Rect.Height / 2));
  // Close the figure
  Result.CloseFigure;
end;


//------------------------------------------------------------------------------
// GET GDI+ BATTERY PATH
//------------------------------------------------------------------------------
function CreateBatteryPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
var
  H, W, X, Y, BW, BH: Single;
  BatteryTopRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery "top" dimensions
  BW := Rect.Width * 0.1;
  BH := H * 0.5;

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(X, Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((X + W) - Corner, Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((X + W) - Corner, (Y + H) - Corner, Corner, Corner, 0, 90);
  // Add arc bottom left corner
  Result.AddArc(X, (Y + H) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;

  // Calculate the battery "top" rect
  BatteryTopRect := MakeRect(X + W, Y + (BH / 2), BW, BH);

  // Start the figure
  Result.StartFigure;
  // Add battery top
  Result.AddRectangle(BatteryTopRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ BATTERY PATH
//------------------------------------------------------------------------------
function CreateBatteryPath(Rect: TGPRect; Corner: Single): TGPGraphicsPath;
var
  H, W, X, Y, BW, BH: Single;
  BatteryTopRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery "top" dimensions
  BW := Rect.Width * 0.1;
  BH := H * 0.5;

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Add arc left top corner
  Result.AddArc(X, Y, Corner, Corner, 180, 90);
  // Add arc top right corner
  Result.AddArc((X + W) - Corner, Y, Corner, Corner, 270, 90);
  // Add arc bottom right corner
  Result.AddArc((X + W) - Corner, (Y + H) - Corner, Corner, Corner, 0, 90);
  // Add arc bottom left corner
  Result.AddArc(X, (Y + H) - Corner, Corner, Corner, 90, 90);
  // Close the figure
  Result.CloseFigure;

  // Calculate the battery "top" rect
  BatteryTopRect := MakeRect(X + W, Y + (BH / 2), BW, BH);

  // Start the figure
  Result.StartFigure;
  // Add battery top
  Result.AddRectangle(BatteryTopRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
var
  H, W, X, Y: Single;
  BatterypercentageRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := MakeRect(X + Distance, Y + Distance, ((W - (Distance * 2)) / 100) * Percentage, H - (Distance * 2));

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Create the battery percentage path
  Result.AddRectangle(BatteryPercentageRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateBatteryPercentagePath(Rect: TGPRect; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
var
  H, W, X, Y: Single;
  BatterypercentageRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := MakeRect(X + Distance, Y + Distance, ((W - (Distance * 2)) / 100) * Percentage, H - (Distance * 2));

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Create the battery percentage path
  Result.AddRectangle(BatteryPercentageRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ GLARE BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateGlareBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
var
  H, W, X, Y: Single;
  BatterypercentageRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := MakeRect(X + Distance, Y + Distance, ((W - (Distance * 2)) / 100) * Percentage, (H - (Distance * 2)) / 2);

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Create the battery percentage path
  Result.AddRectangle(BatteryPercentageRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ GLARE BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateGlareBatteryPercentagePath(Rect: TGPRect; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
var
  H, W, X, Y: Single;
  BatterypercentageRect: TGPRectF;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  BatterypercentageRect := MakeRect(X + Distance, Y + Distance, ((W - (Distance * 2)) / 100) * Percentage, (H - (Distance * 2)) / 2);

  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;
  // Create the battery percentage path
  Result.AddRectangle(BatteryPercentageRect);
  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ GLARE BATTERY PERCENTAGE PATH
//------------------------------------------------------------------------------
function CreateGlareBatteryPercentageRect(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPRectF;
var
  H, W, X, Y: Single;
begin
  // Calculate the dimensions of the battery housing
  W := Rect.Width * 0.9;
  H := Rect.Height * 0.55;
  X := Rect.X;
  Y := (Rect.Y + (Rect.Height / 2)) - (H / 2);

  // Calculate the battery percentage rect
  Result := MakeRect(X + Distance, Y + Distance, ((W - (Distance * 2)) / 100) * Percentage, (H - (Distance * 2) / 2));
end;

//------------------------------------------------------------------------------
// GET GDI+ VEHICLE BATTERY PATH
//------------------------------------------------------------------------------
function CreateVehicleBatteryPath(Rect: TGPRectF): TGPGraphicsPath;
const
  S: string = '- +';
var
  X, Y, W, H, PW, PH, PS: Single;
  BoxRect, Polerect: TGPRectF;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;

  X := Rect.X;
  Y := Rect.Y + (Rect.Height / 2);

  // Battery box height
  H := Rect.Height * 0.6;

  // Pole width
  PW := Rect.Width / 4;
  // Pole height
  PH := Rect.Height / 8;
  // Space next and between poles
  PS := (Rect.Width / 2) / 3;

  // Start with the left line
  Result.AddLine(X, Y + (H / 2), X, Y - (H / 2));

  // Then the top line with the two poles
  // 1.
  Result.AddLine(X, Y - (H / 2), X + PS, Y - (H / 2));
  // 2.
  Result.AddLine(X + PS,  Y - (H / 2), X + PS, (Y - (H / 2)) - PH);
  // 3.
  Result.AddLine(X + PS, (Y - (H / 2)) - PH, X + PS + PW, (Y - (H / 2)) - PH);
  // 4.
  Result.AddLine(X + PS + PW, (Y - (H / 2)) - PH, X + PS + PW, Y - (H / 2));
  // 5.
  Result.AddLine(X + PS + PW, Y - (H / 2), X + PS + PW + PS, Y - (H / 2));
  // 6.
  Result.AddLine(X + PS + PW + PS, Y - (H / 2),  X + PS + PW + PS, (Y - (H / 2)) - PH);
  // 7.
  Result.AddLine(X + PS + PW + PS, (Y - (H / 2)) - PH, X + PS + PW + PS + PW, (Y - (H / 2)) - PH);
  // 8.
  Result.AddLine(X + PS + PW + PS + PW, (Y - (H / 2)) - PH, X + PS + PW + PS + PW, Y - (H / 2));
  // 9.
  Result.AddLine(X + PS + PW + PS + PW, Y - (H / 2), X + PS + PW + PS + PW + PS, Y - (H / 2));

  // And then the right line
  Result.AddLine(X + PS + PW + PS + PW + PS,  Y - (H / 2), X + PS + PW + PS + PW + PS, Y + (H / 2));

  // Close the figure
  Result.CloseFigure;

  // Add the minus and plus signs
  FontFamily := TGPFontFamily.Create('Arial');
  StringFormat := TGPStringFormat.Create;
  StringFormat.SetAlignment(StringAlignmentCenter);
  StringFormat.SetLineAlignment(StringAlignmentCenter);
  try
    Result.AddString(S, Length(S), FontFamily, FontStyleRegular, Rect.Height / 2, Rect, StringFormat);
  finally
    FontFamily.Free;
    StringFormat.Free;
  end;
end;

//------------------------------------------------------------------------------
// GET GDI+ VEHICLE BATTERY PATH
//------------------------------------------------------------------------------
function CreateVehicleBatteryPath(Rect: TGPRect): TGPGraphicsPath; overload;
const
  S: string = '- +';
var
  X, Y, W, H, PW, PH, PS: Single;
  BoxRect, Polerect: TGPRectF;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;

  X := Rect.X;
  Y := Rect.Y + (Rect.Height / 2);

  // Battery box height
  H := Rect.Height * 0.6;

  // Pole width
  PW := Rect.Width / 4;
  // Pole height
  PH := Rect.Height / 8;
  // Space next and between poles
  PS := (Rect.Width / 2) / 3;

  // Start with the left line
  Result.AddLine(X, Y + (H / 2), X, Y - (H / 2));

  // Then the top line with the two poles
  // 1.
  Result.AddLine(X, Y - (H / 2), X + PS, Y - (H / 2));
  // 2.
  Result.AddLine(X + PS,  Y - (H / 2), X + PS, (Y - (H / 2)) - PH);
  // 3.
  Result.AddLine(X + PS, (Y - (H / 2)) - PH, X + PS + PW, (Y - (H / 2)) - PH);
  // 4.
  Result.AddLine(X + PS + PW, (Y - (H / 2)) - PH, X + PS + PW, Y - (H / 2));
  // 5.
  Result.AddLine(X + PS + PW, Y - (H / 2), X + PS + PW + PS, Y - (H / 2));
  // 6.
  Result.AddLine(X + PS + PW + PS, Y - (H / 2),  X + PS + PW + PS, (Y - (H / 2)) - PH);
  // 7.
  Result.AddLine(X + PS + PW + PS, (Y - (H / 2)) - PH, X + PS + PW + PS + PW, (Y - (H / 2)) - PH);
  // 8.
  Result.AddLine(X + PS + PW + PS + PW, (Y - (H / 2)) - PH, X + PS + PW + PS + PW, Y - (H / 2));
  // 9.
  Result.AddLine(X + PS + PW + PS + PW, Y - (H / 2), X + PS + PW + PS + PW + PS, Y - (H / 2));

  // And then the right line
  Result.AddLine(X + PS + PW + PS + PW + PS,  Y - (H / 2), X + PS + PW + PS + PW + PS, Y + (H / 2));

  // Close the figure
  Result.CloseFigure;

  // Add the minus and plus signs
  FontFamily := TGPFontFamily.Create('Arial');
  StringFormat := TGPStringFormat.Create;
  StringFormat.SetAlignment(StringAlignmentCenter);
  StringFormat.SetLineAlignment(StringAlignmentCenter);
  try
    Result.AddString(S, Length(S), FontFamily, FontStyleRegular, Rect.Height / 2, Rect, StringFormat);
  finally
    FontFamily.Free;
    StringFormat.Free;
  end;
end;

end.
