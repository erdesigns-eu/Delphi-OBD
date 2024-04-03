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
function CreateRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateGlareRoundRectPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateGlareBackButtonPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateGlareTabLeftPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateTabCenterPath(Rect: TGPRectF): TGPGraphicsPath;
function CreateGlareTabCenterPath(Rect: TGPRectF): TGPGraphicsPath;
function CreateTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateGlareTabRightPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateBatteryPath(Rect: TGPRectF; Corner: Single): TGPGraphicsPath;
function CreateBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
function CreateGlareBatteryPercentagePath(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPGraphicsPath;
function CreateGlareBatteryPercentageRect(Rect: TGPRectF; Percentage: Single; Distance: Single = 2): TGPRectF;
function CreateVehicleBatteryPath(Rect: TGPRectF): TGPGraphicsPath;
function CreateJ1962Path(Rect: TGPRectF): TGPGraphicsPath;
function CreateInternetGlobePath(Rect: TGPRectF): TGPGraphicsPath;
function CreateProtocolPath(Rect: TGPRectF): TGPGraphicsPath;
function CreateSizeGripPathSquare(Rect: TGPRectF; DotSize: Single; DotSpacing: Single): TGPGraphicsPath;
function CreateSizeGripPathCircle(Rect: TGPRectF; DotSize: Single; DotSpacing: Single): TGPGraphicsPath;

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
  X, Y, H, PW, PH, PS: Single;
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
// GET GDI+ J1962 PATH
//------------------------------------------------------------------------------
function CreateJ1962Path(Rect: TGPRectF): TGPGraphicsPath;
var
  W, H, X, Y: Single;
  Corner: Single;
  PlugHeight: Single;
  CenterRect: TGPRectF;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;

  Corner := Rect.Width / 8;
  PlugHeight := Rect.Height / 2;
  Y := (Rect.Y + (Rect.Height / 2)) - (PlugHeight / 2);

  // Start with the top left arc
  Result.AddArc(Rect.X, Y, Corner, Corner, 150, 120);
  // Then the top right arc
  Result.AddArc((Rect.X + Rect.Width) - Corner, Y, Corner, Corner, 270, 120);

  // Then the bottom line
  W := Rect.Width / 6;
  H := Rect.Height / 20;
  Result.AddLine((Rect.X + Rect.Width) - Corner, Y + PlugHeight, ((Rect.X + Rect.Width) - Corner) - W, Y + PlugHeight);
  Result.AddLine(((Rect.X + Rect.Width) - Corner) - W, Y + PlugHeight, ((Rect.X + Rect.Width) - Corner) - W, Y + PlugHeight + H);
  X := ((Rect.X + Rect.Width) - Corner) - W;
  Result.AddLine(X, Y + PlugHeight + H, X - (W * 2.5), Y + PlugHeight + H);
  Result.AddLine(X - (W * 2.5), Y + PlugHeight + H, X - (W * 2.5), Y + PlugHeight);
  Result.AddLine(X - (W * 2.5), Y + PlugHeight, X - (W * 3.5), Y + PlugHeight);

  // Close the figure
  Result.CloseFigure;

  // Add center rectangle
  H := Rect.Height / 16;
  W := Rect.Width / 16;
  CenterRect := MakeRect(Rect.X + Corner + W, (Y + (PlugHeight / 2)) - (H / 2), Rect.Width - ((Corner * 2) + (W * 2)), H);
  Result.AddRectangle(CenterRect);
end;

//------------------------------------------------------------------------------
// GET GDI+ INTERNET GLOBE PATH
//------------------------------------------------------------------------------
function CreateInternetGlobePath(Rect: TGPRectF): TGPGraphicsPath;
var
  W, H, X, Y, C: Single;
  EllipseRect: TGPRectF;
begin
  Result := TGPGraphicsPath.Create;
  // Start the figure
  Result.StartFigure;

  // Outer ellipse
  EllipseRect := MakeRect(Rect.X, Rect.Y, Rect.Width, Rect.Height);
  Result.AddEllipse(EllipseRect);

  // Inner ellipse
  W := Rect.Width / 2;
  X := (Rect.X + (Rect.Width / 2)) - (W / 2);
  EllipseRect := MakeRect(X, Rect.Y, W, Rect.Height);
  Result.AddEllipse(EllipseRect);

  // Close the figure
  Result.CloseFigure;

  // Calculate the height for horizontal lines
  H := Rect.Height / 3;
  C := Rect.Height / 32.75;

  // First horizontal line
  Y := Rect.Y + Rect.Height / 2 - H / 2;
  Result.StartFigure;
  Result.AddLine(Rect.X + C, Y, (Rect.X + Rect.Width) - C, Y);
  Result.CloseFigure;

  // Second horizontal line
  Y := Rect.Y + Rect.Height / 2 + H / 2;
  Result.StartFigure;
  Result.AddLine(Rect.X + C, Y, (Rect.X + Rect.Width) - C, Y);
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ PROTOCOL PATH
//------------------------------------------------------------------------------
function CreateProtocolPath(Rect: TGPRectF): TGPGraphicsPath;
const
  S: string = '00';
var
  W, H, X, Y, T: Single;
  R: TGPRectF;
  FontFamily: TGPFontFamily;
  StringFormat: TGPStringFormat;
begin
  Result := TGPGraphicsPath.Create;
  Result.StartFigure;

  // Center
  X := Rect.X + (Rect.Width / 2);
  Y := Rect.Y + (Rect.Height / 2);

  W := Rect.Width;
  H := Rect.Height / 2;

  // Add top rectandle
  R := MakeRect(X - (W / 2), (Y - H) + (Rect.Height / 8), W, H);
  Result.AddRectangle(R);
  Result.CloseFigure;
  Result.StartFigure;

  // Add the code sign
  FontFamily := TGPFontFamily.Create('Arial');
  StringFormat := TGPStringFormat.Create;
  StringFormat.SetAlignment(StringAlignmentCenter);
  StringFormat.SetLineAlignment(StringAlignmentCenter);
  try
    R := MakeRect(X - (W / 2), (Y - H) + (Rect.Height / 6), W, H);
    Result.AddString(S, Length(S), FontFamily, FontStyleRegular, Rect.Height / 3, R, StringFormat);
  finally
    FontFamily.Free;
    StringFormat.Free;
  end;

  // Add line down
  Result.AddLine(X, Y + (Rect.Height / 8), X, Y + (Rect.Height / 4));

  // Add bottom rectangle
  W := Rect.Width / 6;
  H := Rect.Height / 6;
  R := MakeRect(X - (W / 2), Y + (Rect.Height / 4), W, H);
  Result.AddRectangle(R);

  // Add line to the left
  Result.CloseFigure;
  Result.StartFigure;
  Result.AddLine(X - (W / 2), Y + (Rect.Height / 4) + (H / 2), Rect.X + (Rect.Width / 4), Y + (Rect.Height / 4) + (H / 2));

  // Add ellipse to the left
  Result.CloseFigure;
  Result.StartFigure;
  T := Y + (Rect.Height / 4) + (H / 2);
  R := MakeRect((Rect.X + (Rect.Width / 4)) - Rect.Width / 8, T - (Rect.Height / 16), Rect.Width / 8, Rect.Height / 8);
  Result.AddEllipse(R);

  // Add line to the right
  Result.CloseFigure;
  Result.StartFigure;
  Result.AddLine(X + (W / 2), Y + (Rect.Height / 4) + (H / 2), (Rect.X + Rect.Width) - (Rect.Width / 4), Y + (Rect.Height / 4) + (H / 2));

  // Add ellipse to the right
  Result.CloseFigure;
  Result.StartFigure;
  T := Y + (Rect.Height / 4) + (H / 2);
  R := MakeRect((Rect.X + Rect.Width) - (Rect.Width / 4), T - (Rect.Height / 16), Rect.Width / 8, Rect.Height / 8);
  Result.AddEllipse(R);

  // Close the figure
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------
// GET GDI+ SIZE GRIP PATH (SQUARE DOTS)
//------------------------------------------------------------------------------
function CreateSizeGripPathSquare(Rect: TGPRectF; DotSize: Single; DotSpacing: Single): TGPGraphicsPath;
var
  I, J: Integer;
  StartX, StartY, PosX, PosY: Single;
  TotalWidth, TotalHeight: Single;
  DotRect: TGPRectF;
begin
  Result := TGPGraphicsPath.Create;

  // Calculate the total width and height so we can position
  // the X and Y positions.
  TotalWidth := ((DotSize + DotSpacing) * 3);
  TotalHeight := ((DotSize + DotSpacing) * 3);

  // Calculate the starting X and Y positions for the grip drawing
  // This will still place the grip in the bottom right
  StartX := (Rect.X + Rect.Width) - TotalWidth;
  StartY := (Rect.Y + (Rect.Height / 2)) - (TotalHeight / 2);

  for I := 0 to 2 do // Rows
  for J := 0 to 2 do // Columns
  if I >= J then     // Ensures the grip pattern (diagonal)
  begin
    PosX := StartX + (2 - J) * (DotSize + DotSpacing);
    PosY := StartY + I * (DotSize + DotSpacing);

    Result.StartFigure;
    DotRect := MakeRect(PosX, PosY, DotSize, DotSize);
    Result.AddRectangle(DotRect);
    Result.CloseFigure;
  end;
end;

//------------------------------------------------------------------------------
// GET GDI+ SIZE GRIP PATH (CIRCLE DOTS)
//------------------------------------------------------------------------------
function CreateSizeGripPathCircle(Rect: TGPRectF; DotSize: Single; DotSpacing: Single): TGPGraphicsPath;
var
  I, J: Integer;
  StartX, StartY, PosX, PosY: Single;
  TotalWidth, TotalHeight: Single;
  DotRect: TGPRectF;
begin
  Result := TGPGraphicsPath.Create;

  // Calculate the total width and height so we can position
  // the X and Y positions.
  TotalWidth := ((DotSize + DotSpacing) * 3);
  TotalHeight := ((DotSize + DotSpacing) * 3);

  // Calculate the starting X and Y positions for the grip drawing
  // This will still place the grip in the bottom right
  StartX := (Rect.X + Rect.Width) - TotalWidth;
  StartY := (Rect.Y + (Rect.Height / 2)) - (TotalHeight / 2);

  for I := 0 to 2 do // Rows
  for J := 0 to 2 do // Columns
  if I >= J then     // Ensures the grip pattern (diagonal)
  begin
    PosX := StartX + (2 - J) * (DotSize + DotSpacing);
    PosY := StartY + I * (DotSize + DotSpacing);

    Result.StartFigure;
    DotRect := MakeRect(PosX, PosY, DotSize, DotSize);
    Result.AddEllipse(DotRect);
    Result.CloseFigure;
  end;
end;

end.
