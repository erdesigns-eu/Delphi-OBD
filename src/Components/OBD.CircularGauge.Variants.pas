//------------------------------------------------------------------------------
// UNIT           : OBD.CircularGauge.Variants.pas
// CONTENTS       : Specialized circular gauge variants (180°, 270°)
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 07/12/2024
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.CircularGauge.Variants;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls,
  OBD.CircularGauge;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Arc preset configurations for circular gauges
  /// </summary>
  TArcPreset = (
    apCustom,           // User-defined StartAngle and EndAngle
    apFullCircle,       // 360° (0-360)
    apThreeQuarter,     // 270° (135-45)
    apHalfBottom,       // 180° bottom half (180-0)
    apHalfTop,          // 180° top half (0-180)
    apHalfLeft,         // 180° left half (90-270)
    apHalfRight         // 180° right half (270-90)
  );

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Base circular gauge with arc preset support
  /// </summary>
  TOBDCircularGaugeEx = class(TOBDCircularGauge)
  private
    FArcPreset: TArcPreset;
    procedure SetArcPreset(Value: TArcPreset);
  protected
    procedure ApplyArcPreset(Preset: TArcPreset); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ArcPreset: TArcPreset read FArcPreset write SetArcPreset default apThreeQuarter;
  end;

  /// <summary>
  ///   Three-quarter circular gauge (270°) - Popular for automotive clusters
  /// </summary>
  TOBDThreeQuarterGauge = class(TOBDCircularGaugeEx)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>
  ///   Half circular gauge - Bottom semicircle (180°)
  /// </summary>
  TOBDSemiCircularGauge = class(TOBDCircularGaugeEx)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>
  ///   Half circular gauge - Top semicircle (180°)
  /// </summary>
  TOBDSemiCircularTopGauge = class(TOBDCircularGaugeEx)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>
  ///   Half circular gauge - Left semicircle (180°)
  /// </summary>
  TOBDSemiCircularLeftGauge = class(TOBDCircularGaugeEx)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>
  ///   Half circular gauge - Right semicircle (180°)
  /// </summary>
  TOBDSemiCircularRightGauge = class(TOBDCircularGaugeEx)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

//------------------------------------------------------------------------------
// TOBDCircularGaugeEx Implementation
//------------------------------------------------------------------------------

constructor TOBDCircularGaugeEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apThreeQuarter;
  ApplyArcPreset(FArcPreset);
end;

procedure TOBDCircularGaugeEx.SetArcPreset(Value: TArcPreset);
begin
  if (FArcPreset <> Value) then
  begin
    FArcPreset := Value;
    if FArcPreset <> apCustom then
      ApplyArcPreset(FArcPreset);
  end;
end;

procedure TOBDCircularGaugeEx.ApplyArcPreset(Preset: TArcPreset);
begin
  case Preset of
    apFullCircle:
      begin
        StartAngle := 0;
        EndAngle := 360;
      end;
    apThreeQuarter:
      begin
        StartAngle := 135;
        EndAngle := 225;  // 135 + 270 = 405, wraps to 45 (225 shown as ending position)
      end;
    apHalfBottom:
      begin
        StartAngle := 180;
        EndAngle := 0;
      end;
    apHalfTop:
      begin
        StartAngle := 0;
        EndAngle := 180;
      end;
    apHalfLeft:
      begin
        StartAngle := 90;
        EndAngle := 270;
      end;
    apHalfRight:
      begin
        StartAngle := 270;
        EndAngle := 90;
      end;
  end;
end;

//------------------------------------------------------------------------------
// TOBDThreeQuarterGauge Implementation
//------------------------------------------------------------------------------

constructor TOBDThreeQuarterGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apThreeQuarter;
  ApplyArcPreset(apThreeQuarter);
  Width := 200;
  Height := 200;
end;

//------------------------------------------------------------------------------
// TOBDSemiCircularGauge Implementation (Bottom)
//------------------------------------------------------------------------------

constructor TOBDSemiCircularGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apHalfBottom;
  ApplyArcPreset(apHalfBottom);
  Width := 200;
  Height := 120;  // Shorter for semicircle
end;

//------------------------------------------------------------------------------
// TOBDSemiCircularTopGauge Implementation
//------------------------------------------------------------------------------

constructor TOBDSemiCircularTopGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apHalfTop;
  ApplyArcPreset(apHalfTop);
  Width := 200;
  Height := 120;
end;

//------------------------------------------------------------------------------
// TOBDSemiCircularLeftGauge Implementation
//------------------------------------------------------------------------------

constructor TOBDSemiCircularLeftGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apHalfLeft;
  ApplyArcPreset(apHalfLeft);
  Width := 120;
  Height := 200;
end;

//------------------------------------------------------------------------------
// TOBDSemiCircularRightGauge Implementation
//------------------------------------------------------------------------------

constructor TOBDSemiCircularRightGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArcPreset := apHalfRight;
  ApplyArcPreset(apHalfRight);
  Width := 120;
  Height := 200;
end;

end.
