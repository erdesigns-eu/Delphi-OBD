//------------------------------------------------------------------------------
// UNIT           : OBD.CustomControl.Animation.pas
// CONTENTS       : Custom Component Animations
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 22/03/2024
//------------------------------------------------------------------------------
unit OBD.CustomControl.Animation;

interface

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Animation type
  /// </summary>
  TOBDCustomControlAnimationType = (
    anLinear,
    anBackEaseIn,    anBackEaseOut,    anBackEaseInOut,
    anBounceEaseIn,  anBounceEaseOut,  anBounceEaseInOut,
    anCircEaseIn,    anCircEaseOut,    anCircEaseInOut,
    anCubicEaseIn,   anCubicEaseOut,   anCubicEaseInOut,
    anElasticEaseIn, anElasticEaseOut, anElasticEaseInOut,
    anExpoEaseIn,    anExpoEaseOut,    anExpoEaseInOut,
    anQuadEaseIn,    anQuadEaseOut,    anQuadEaseInOut,
    anQuartEaseIn,   anQuartEaseOut,   anQuartEaseInOut,
    anQuintEaseIn,   anQuintEaseOut,   anQuintEaseInOut,
    anSineEaseIn,    anSineEaseOut,    anSineEaseInOut
  );

  /// <summary>
  ///   Animation function
  /// </summary>
  TOBDCustomControlAnimationEasingFunction = function(T: Single): Single;

//------------------------------------------------------------------------------
// ANIMATION FUNCTIONS
//------------------------------------------------------------------------------
function BackEaseIn(t: Single): Single;
function BackEaseOut(t: Single): Single;
function BackEaseInOut(t: Single): Single;
function BounceEaseOut(t: Single): Single;
function BounceEaseIn(t: Single): Single;
function BounceEaseInOut(t: Single): Single;
function CircEaseIn(t: Single): Single;
function CircEaseOut(t: Single): Single;
function CircEaseInOut(t: Single): Single;
function CubicEaseIn(t: Single): Single;
function CubicEaseOut(t: Single): Single;
function CubicEaseInOut(t: Single): Single;
function ElasticEaseIn(t: Single): Single;
function ElasticEaseOut(t: Single): Single;
function ElasticEaseInOut(t: Single): Single;
function ExpoEaseIn(t: Single): Single;
function ExpoEaseOut(t: Single): Single;
function ExpoEaseInOut(t: Single): Single;
function QuadEaseIn(t: Single): Single;
function QuadEaseOut(t: Single): Single;
function QuadEaseInOut(t: Single): Single;
function QuartEaseIn(t: Single): Single;
function QuartEaseOut(t: Single): Single;
function QuartEaseInOut(t: Single): Single;
function QuintEaseIn(t: Single): Single;
function QuintEaseOut(t: Single): Single;
function QuintEaseInOut(t: Single): Single;
function SineEaseIn(t: Single): Single;
function SineEaseOut(t: Single): Single;
function SineEaseInOut(t: Single): Single;

//------------------------------------------------------------------------------
// OTHER FUNCTIONS
//------------------------------------------------------------------------------
function GetEasingFunction(AnimationType: TOBDCustomControlAnimationType): TOBDCustomControlAnimationEasingFunction;

implementation

uses System.Math;

//------------------------------------------------------------------------------
// LINEAR
//------------------------------------------------------------------------------
function Linear(T: Single): Single;
begin
  Result := T;
end;

//------------------------------------------------------------------------------
// BACK EASE IN
//------------------------------------------------------------------------------
function BackEaseIn(T: Single): Single;
const
  S = 1.70158;
begin
  Result := T * T * ((S + 1) * T - S);
end;

//------------------------------------------------------------------------------
// BACK EASE OUT
//------------------------------------------------------------------------------
function BackEaseOut(T: Single): Single;
const
  S = 1.70158;
begin
  T := T - 1;
  Result := T * T * ((S + 1) * T + S) + 1;
end;

//------------------------------------------------------------------------------
// BACK EASE IN-OUT
//------------------------------------------------------------------------------
function BackEaseInOut(T: Single): Single;
const
  S = 1.70158 * 1.525;
begin
  T := T * 2;
  if (T < 1) then Result := 0.5 * T * T * ((S + 1) * T - S) else
  begin
    T := T - 2;
    Result := 0.5 * (T * T * ((S + 1) * T + S) + 2);
  end;
end;

//------------------------------------------------------------------------------
// BOUNCE EASE OUT
//------------------------------------------------------------------------------
function BounceEaseOut(T: Single): Single;
begin
  if (T < (1 / 2.75)) then
    Result := 7.5625 * T * T
  else if (T < (2 / 2.75)) then
  begin
    T := T - (1.5 / 2.75);
    Result := 7.5625 * T * T + 0.75;
  enD else
  if (T < (2.5 / 2.75)) then
  begin
    T := T - (2.25 / 2.75);
    Result := 7.5625 * T * T + 0.9375;
  end else
  begin
    T := T - (2.625 / 2.75);
    Result := 7.5625 * T * T + 0.984375;
  end;
end;

//------------------------------------------------------------------------------
// BOUNCE EASE IN
//------------------------------------------------------------------------------
function BounceEaseIn(T: Single): Single;
begin
  Result := 1 - BounceEaseOut(1 - T);
end;

//------------------------------------------------------------------------------
// BOUNCE EASE IN-OUT
//------------------------------------------------------------------------------
function BounceEaseInOut(t: Single): Single;
begin
  if (T < 0.5) then
    Result := (1 - BounceEaseOut(1 - 2 * T)) * 0.5
  else
    Result := (1 + BounceEaseOut(2 * T - 1)) * 0.5;
end;

//------------------------------------------------------------------------------
// CIRC EASE IN
//------------------------------------------------------------------------------
function CircEaseIn(T: Single): Single;
begin
  Result := 1 - Sqrt(1 - T * T);
end;

//------------------------------------------------------------------------------
// CIRC EASE OUT
//------------------------------------------------------------------------------
function CircEaseOut(T: Single): Single;
begin
  T := T - 1;
  Result := Sqrt(1 - T * T);
end;

//------------------------------------------------------------------------------
// CIRC EASE IN-OUT
//------------------------------------------------------------------------------
function CircEaseInOut(T: Single): Single;
begin
  T := T * 2;
  if (T < 1) then Result := -0.5 * (Sqrt(1 - T * T) - 1) else
  begin
    T := T - 2;
    Result := 0.5 * (Sqrt(1 - T * T) + 1);
  end;
end;

//------------------------------------------------------------------------------
// CUBIC EASE IN
//------------------------------------------------------------------------------
function CubicEaseIn(T: Single): Single;
begin
  Result := T * T * T;
end;

//------------------------------------------------------------------------------
// CUBIC EASE OUT
//------------------------------------------------------------------------------
function CubicEaseOut(T: Single): Single;
begin
  T := T - 1;
  Result := T * T * T + 1;
end;

//------------------------------------------------------------------------------
// CUBIC EASE IN-OUT
//------------------------------------------------------------------------------
function CubicEaseInOut(t: Single): Single;
begin
  T := T * 2;
  if (T < 1) then Result := 0.5 * T * T * T else
  begin
    T := T - 2;
    Result := 0.5 * (T * T * T + 2);
  end;
end;

//------------------------------------------------------------------------------
// ELASTIC EASE IN
//------------------------------------------------------------------------------
function ElasticEaseIn(T: Single): Single;
var
  S, Amplitude, Period: Single;
begin
  if T = 0 then Exit(0);
  if T = 1 then Exit(1);
  Period := 0.3;
  Amplitude := 1;
  S := Period / 4;
  T := T - 1;
  Result := -(Amplitude * Power(2, 10 * T) * Sin((T - S) * (2 * PI) / Period));
end;

//------------------------------------------------------------------------------
// ELASTIC EASE OUT
//------------------------------------------------------------------------------
function ElasticEaseOut(T: Single): Single;
var
  S, Amplitude, Period: Single;
begin
  if T = 0 then Exit(0);
  if T = 1 then Exit(1);
  Period := 0.3;
  Amplitude := 1;
  S := Period / 4;
  Result := Amplitude * Power(2, -10 * T) * Sin((T - S) * (2 * PI) / Period) + 1;
end;

//------------------------------------------------------------------------------
// ELASTIC EASE IN-OUT
//------------------------------------------------------------------------------
function ElasticEaseInOut(T: Single): Single;
var
  S, Amplitude, Period: Single;
begin
  if T = 0 then Exit(0);
  if T = 1 then Exit(1);
  Period := 0.3 * 1.5;
  Amplitude := 1;
  S := Period / 4;
  T := T * 2;
  if T < 1 then
  begin
    T := T - 1;
    Result := -0.5 * (Amplitude * Power(2, 10 * T) * Sin((T - S) * (2 * PI) / Period));
  end
  else
  begin
    T := T - 1;
    Result := Amplitude * Power(2, -10 * t) * Sin((T - S) * (2 * PI) / Period) * 0.5 + 1;
  end;
end;

//------------------------------------------------------------------------------
// EXPO EASE IN
//------------------------------------------------------------------------------
function ExpoEaseIn(T: Single): Single;
begin
  if T = 0 then
    Result := 0
  else
    Result := Power(2, 10 * (T - 1));
end;

//------------------------------------------------------------------------------
// EXPO EASE OUT
//------------------------------------------------------------------------------
function ExpoEaseOut(T: Single): Single;
begin
  if T = 1 then
    Result := 1
  else
    Result := -Power(2, -10 * T) + 1;
end;

//------------------------------------------------------------------------------
// EXPO EASE IN-OUT
//------------------------------------------------------------------------------
function ExpoEaseInOut(T: Single): Single;
begin
  if T = 0 then Exit(0);
  if T = 1 then Exit(1);
  T := T * 2;
  if T < 1 then Result := 0.5 * Power(2, 10 * (T - 1)) else
  begin
    T := T - 1;
    Result := 0.5 * (-Power(2, -10 * T) + 2);
  end;
end;

//------------------------------------------------------------------------------
// QUAD EASE IN
//------------------------------------------------------------------------------
function QuadEaseIn(T: Single): Single;
begin
  Result := T * T;
end;

//------------------------------------------------------------------------------
// QUAD EASE OUT
//------------------------------------------------------------------------------
function QuadEaseOut(T: Single): Single;
begin
  Result := -T * (T - 2);
end;

//------------------------------------------------------------------------------
// QUAD EASE IN-OUT
//------------------------------------------------------------------------------
function QuadEaseInOut(T: Single): Single;
begin
  T := T * 2;
  if T < 1 then Result := 0.5 * T * T else
  begin
    T := T - 1;
    Result := -0.5 * (T * (T - 2) - 1);
  end;
end;

//------------------------------------------------------------------------------
// QUART EASE IN
//------------------------------------------------------------------------------
function QuartEaseIn(T: Single): Single;
begin
  Result := T * T * T * T;
end;

//------------------------------------------------------------------------------
// QUART EASE OUT
//------------------------------------------------------------------------------
function QuartEaseOut(T: Single): Single;
begin
  T := T - 1;
  Result := -(T * T * T * T - 1);
end;

//------------------------------------------------------------------------------
// QUART EASE IN-OUT
//------------------------------------------------------------------------------
function QuartEaseInOut(T: Single): Single;
begin
  T := T * 2;
  if T < 1 then Result := 0.5 * T * T * T * T else
  begin
    T := T - 2;
    Result := -0.5 * (T * T * T * T - 2);
  end;
end;

//------------------------------------------------------------------------------
// QUINT EASE IN
//------------------------------------------------------------------------------
function QuintEaseIn(T: Single): Single;
begin
  Result := T * T * T * T * T;
end;

//------------------------------------------------------------------------------
// QUINT EASE OUT
//------------------------------------------------------------------------------
function QuintEaseOut(T: Single): Single;
begin
  T := T - 1;
  Result := T * T * T * T * T + 1;
end;

//------------------------------------------------------------------------------
// QUINT EASE IN-OUT
//------------------------------------------------------------------------------
function QuintEaseInOut(T: Single): Single;
begin
  T := T * 2;
  if T < 1 then Result := 0.5 * T * T * T * T * T else
  begin
    T := T - 2;
    Result := 0.5 * (T * T * T * T * T + 2);
  end;
end;

//------------------------------------------------------------------------------
// SINE EASE IN
//------------------------------------------------------------------------------
function SineEaseIn(T: Single): Single;
begin
  Result := -Cos(T * (PI / 2)) + 1;
end;

//------------------------------------------------------------------------------
// SINE EASE OUT
//------------------------------------------------------------------------------
function SineEaseOut(T: Single): Single;
begin
  Result := Sin(T * (PI / 2));
end;

//------------------------------------------------------------------------------
// SINE EASE IN-OUT
//------------------------------------------------------------------------------
function SineEaseInOut(T: Single): Single;
begin
  Result := -0.5 * (Cos(PI * T) - 1);
end;

//------------------------------------------------------------------------------
// GET ANIMATION FUNCTION FROM ANIMATION TYPE
//------------------------------------------------------------------------------
function GetEasingFunction(AnimationType: TOBDCustomControlAnimationType): TOBDCustomControlAnimationEasingFunction;
begin
  case AnimationType of
    anBackEaseIn      : Result := BackEaseIn;
    anBackEaseOut     : Result := BackEaseOut;
    anBackEaseInOut   : Result := BackEaseInOut;
    anBounceEaseIn    : Result := BounceEaseOut;
    anBounceEaseOut   : Result := BounceEaseIn;
    anBounceEaseInOut : Result := BounceEaseInOut;
    anCircEaseIn      : Result := CircEaseIn;
    anCircEaseOut     : Result := CircEaseOut;
    anCircEaseInOut   : Result := CircEaseInOut;
    anCubicEaseIn     : Result := CubicEaseIn;
    anCubicEaseOut    : Result := CubicEaseOut;
    anCubicEaseInOut  : Result := CubicEaseInOut;
    anElasticEaseIn   : Result := ElasticEaseIn;
    anElasticEaseOut  : Result := ElasticEaseOut;
    anElasticEaseInOut: Result := ElasticEaseInOut;
    anExpoEaseIn      : Result := ExpoEaseIn;
    anExpoEaseOut     : Result := ExpoEaseOut;
    anExpoEaseInOut   : Result := ExpoEaseInOut;
    anQuadEaseIn      : Result := QuadEaseIn;
    anQuadEaseOut     : Result := QuadEaseOut;
    anQuadEaseInOut   : Result := QuadEaseInOut;
    anQuartEaseIn     : Result := QuartEaseIn;
    anQuartEaseOut    : Result := QuartEaseOut;
    anQuartEaseInOut  : Result := QuartEaseInOut;
    anQuintEaseIn     : Result := QuintEaseIn;
    anQuintEaseOut    : Result := QuintEaseOut;
    anQuintEaseInOut  : Result := QuintEaseInOut;
    anSineEaseIn      : Result := SineEaseIn;
    anSineEaseOut     : Result := SineEaseOut;
    anSineEaseInOut   : Result := SineEaseInOut;

    // All other cases use Linear as default
    else Result := Linear;
  end;
end;

end.
