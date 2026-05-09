//------------------------------------------------------------------------------
// UNIT           : OBD.Theme.pas
// CONTENTS       : Central colour palette and theme application
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 06/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : One central palette type plus explicit per-component
//                  Apply methods. No RTTI — every colour assignment is
//                  visible and reviewable. Two factory themes ship out of
//                  the box (Dark, Light); users can also build their own
//                  by populating the fields directly.
//------------------------------------------------------------------------------
unit OBD.Theme;

interface

uses
  System.Classes, System.UITypes, Vcl.Controls, Vcl.Graphics,

  OBD.LED,
  OBD.LinearGauge,
  OBD.Tachometer,
  OBD.TrendGraph,
  OBD.DtcList,
  OBD.Terminal,
  OBD.Knob,
  OBD.SegmentedSwitch;

type
  /// <summary>
  ///   Central palette. The fields are deliberately named after their role
  ///   (chrome / accent / severity) rather than after specific components,
  ///   so a single theme drives every shipped component.
  /// </summary>
  TOBDTheme = class
  public
    // ---- Chrome (panels, frames, headers) ----
    ChromeBackground: TColor;       // dominant control fill
    ChromeBorder: TColor;            // 1px outline
    ChromeText: TColor;              // primary text on chrome
    ChromeMutedText: TColor;         // timestamp / hint text
    ChromeAlternate: TColor;         // alternate-row fill

    // ---- Plot / display (bars, arcs, graph plots) ----
    PlotBackground: TColor;          // unfilled bar / arc background
    PlotBorder: TColor;
    GridLine: TColor;
    PlotText: TColor;

    // ---- Accents (active state, primary action colour) ----
    AccentPrimary: TColor;           // main fill / active arc
    AccentSecondary: TColor;         // gradient endpoint
    AccentText: TColor;              // text on accent fill

    // ---- Severity (info / warning / critical) ----
    SeverityInfo: TColor;
    SeverityWarning: TColor;
    SeverityCritical: TColor;

    // ---- Selection / hover ----
    Selection: TColor;

    /// <summary>
    ///   Built-in dark theme.
    /// </summary>
    class function Dark: TOBDTheme;
    /// <summary>
    ///   Built-in light theme.
    /// </summary>
    class function Light: TOBDTheme;

    procedure Apply(C: TOBDLinearGauge); overload;
    procedure Apply(C: TOBDTachometer); overload;
    procedure Apply(C: TOBDTrendGraph); overload;
    procedure Apply(C: TOBDDtcList); overload;
    procedure Apply(C: TOBDTerminal); overload;
    procedure Apply(C: TOBDKnob); overload;
    procedure Apply(C: TOBDSegmentedSwitch); overload;

    /// <summary>
    ///   Walk the entire control tree below <c>Root</c> and apply this
    ///   theme to every component the palette knows how to colour.
    /// </summary>
    procedure ApplyToTree(Root: TWinControl);
  end;

implementation

//------------------------------------------------------------------------------
// FACTORY: DARK
//------------------------------------------------------------------------------
class function TOBDTheme.Dark: TOBDTheme;
begin
  Result := TOBDTheme.Create;
  Result.ChromeBackground := $00181818;
  Result.ChromeBorder     := $00404040;
  Result.ChromeText       := clWhite;
  Result.ChromeMutedText  := $00808080;
  Result.ChromeAlternate  := $001E1E1E;

  Result.PlotBackground   := $00282828;
  Result.PlotBorder       := $00404040;
  Result.GridLine         := $002A2A2A;
  Result.PlotText         := clWhite;

  Result.AccentPrimary    := TColor($001F8FE6);  // BGR: e68f1f → vivid blue
  Result.AccentSecondary  := TColor($0033C033);  // green
  Result.AccentText       := clWhite;

  Result.SeverityInfo     := TColor($003399FF);
  Result.SeverityWarning  := TColor($0000A5FF);
  Result.SeverityCritical := TColor($003333E6);

  Result.Selection        := $00505050;
end;

//------------------------------------------------------------------------------
// FACTORY: LIGHT
//------------------------------------------------------------------------------
class function TOBDTheme.Light: TOBDTheme;
begin
  Result := TOBDTheme.Create;
  Result.ChromeBackground := $00FAFAFA;
  Result.ChromeBorder     := $00C8C8C8;
  Result.ChromeText       := clBlack;
  Result.ChromeMutedText  := $00606060;
  Result.ChromeAlternate  := $00F0F0F0;

  Result.PlotBackground   := $00E8E8E8;
  Result.PlotBorder       := $00C0C0C0;
  Result.GridLine         := $00D8D8D8;
  Result.PlotText         := clBlack;

  Result.AccentPrimary    := TColor($00CC6600);
  Result.AccentSecondary  := TColor($0066AA00);
  Result.AccentText       := clWhite;

  Result.SeverityInfo     := TColor($00CC6600);
  Result.SeverityWarning  := TColor($000099CC);
  Result.SeverityCritical := TColor($000033CC);

  Result.Selection        := $00D8D8D8;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDLinearGauge
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDLinearGauge);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := PlotBackground;
  C.BorderColor := PlotBorder;
  C.BarColorFrom := AccentSecondary;
  C.BarColorTo := AccentPrimary;
  C.TextColor := ChromeText;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDTachometer
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDTachometer);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := ChromeBackground;
  C.RingColor := PlotBackground;
  C.BorderColor := ChromeBorder;
  C.TickColor := ChromeMutedText;
  C.RedlineColor := SeverityCritical;
  C.NeedleColor := AccentPrimary;
  C.TextColor := ChromeText;
  C.ShiftLightColorOff := PlotBackground;
  C.ShiftLightColorOn := SeverityWarning;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDTrendGraph
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDTrendGraph);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := ChromeBackground;
  C.GridColor := GridLine;
  C.BorderColor := ChromeBorder;
  C.TextColor := ChromeText;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDDtcList
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDDtcList);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := ChromeBackground;
  C.HeaderBackgroundColor := PlotBackground;
  C.RowAlternateColor := ChromeAlternate;
  C.BorderColor := ChromeBorder;
  C.TextColor := ChromeText;
  C.SelectionColor := Selection;
  C.SeverityInfoColor := SeverityInfo;
  C.SeverityWarningColor := SeverityWarning;
  C.SeverityCriticalColor := SeverityCritical;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDTerminal
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDTerminal);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := ChromeBackground;
  C.BorderColor := ChromeBorder;
  C.TextColor := ChromeText;
  C.TimestampColor := ChromeMutedText;
  C.ReceivedColor := AccentSecondary;
  C.SentColor := AccentPrimary;
  C.InfoColor := ChromeMutedText;
  C.ErrorColor := SeverityCritical;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDKnob
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDKnob);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := ChromeBackground;
  C.BodyColor := PlotBackground;
  C.RingColor := ChromeAlternate;
  C.ActiveRingColor := AccentPrimary;
  C.IndicatorColor := ChromeText;
  C.TextColor := ChromeText;
end;

//------------------------------------------------------------------------------
// APPLY: TOBDSegmentedSwitch
//------------------------------------------------------------------------------
procedure TOBDTheme.Apply(C: TOBDSegmentedSwitch);
begin
  if not Assigned(C) then Exit;
  C.BackgroundColor := PlotBackground;
  C.BorderColor := ChromeBorder;
  C.ActiveColor := AccentPrimary;
  C.ActiveTextColor := AccentText;
  C.InactiveTextColor := ChromeText;
end;

//------------------------------------------------------------------------------
// APPLY-TO-TREE
//------------------------------------------------------------------------------
procedure TOBDTheme.ApplyToTree(Root: TWinControl);
var
  I: Integer;
  Child: TControl;
begin
  if not Assigned(Root) then Exit;
  for I := 0 to Root.ControlCount - 1 do
  begin
    Child := Root.Controls[I];
    // Specific-type apply for everything we know about. The TWinControl
    // descent recurses so nested layouts (panels, group boxes) are covered.
    if Child is TOBDLinearGauge     then Apply(TOBDLinearGauge(Child))
    else if Child is TOBDTachometer then Apply(TOBDTachometer(Child))
    else if Child is TOBDTrendGraph then Apply(TOBDTrendGraph(Child))
    else if Child is TOBDDtcList    then Apply(TOBDDtcList(Child))
    else if Child is TOBDTerminal   then Apply(TOBDTerminal(Child))
    else if Child is TOBDKnob       then Apply(TOBDKnob(Child))
    else if Child is TOBDSegmentedSwitch then Apply(TOBDSegmentedSwitch(Child));

    if Child is TWinControl then
      ApplyToTree(TWinControl(Child));
  end;
end;

end.
