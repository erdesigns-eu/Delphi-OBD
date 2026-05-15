//------------------------------------------------------------------------------
//  OBD.UI.Anim
//
//  Lightweight animation helpers used by every visual that
//  needs smoothed value transitions (gauge needles sweeping
//  to a new value, sparkline scrolls, charging-flow arrows).
//
//  All zero-allocation in steady state and TTimer-based - no
//  external dependencies, no SDK requirements beyond stock VCL.
//
//  Use:
//
//    FAnim := TOBDValueAnim.Create;
//    FAnim.Easing := emSpring;
//    FAnim.DurationMs := 300;
//    FAnim.OnFrame := HandleFrame;
//    FAnim.Animate(FromValue, ToValue);
//
//    procedure THost.HandleFrame(Sender: TObject; AValue: Double);
//    begin
//      FNeedleAngle := AValue;
//      Invalidate;
//    end;
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Anim;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Diagnostics,
  Vcl.ExtCtrls;

type
  /// <summary>Easing function family. Spring (slight overshoot
  /// then settle) is the default for gauge needles; linear /
  /// ease-in-out for everything else.</summary>
  TOBDEasingMode = (
    emLinear,
    emEaseIn,         // x^2 - slow start, fast finish
    emEaseOut,        // 1 - (1-x)^2 - fast start, slow finish
    emEaseInOut,      // cubic in-out
    emSpring          // mild overshoot then settle
  );

  /// <summary>Fired on each animation step. <c>AValue</c> is
  /// the interpolated value at the current frame.</summary>
  TOBDAnimFrameEvent = procedure(Sender: TObject;
    AValue: Double) of object;

  /// <summary>Fired once when the animation completes (or is
  /// stopped). <c>AFinal</c> is the value that was being
  /// animated towards.</summary>
  TOBDAnimDoneEvent = procedure(Sender: TObject;
    AFinal: Double) of object;

  /// <summary>One scalar value animation. Drive it with
  /// <c>Animate(From, To)</c>; the timer ticks
  /// <see cref="OnFrame"/> at ~60 fps until the duration
  /// elapses or <see cref="Stop"/> is called.</summary>
  TOBDValueAnim = class
  strict private
    FTimer:       TTimer;
    FStopwatch:   TStopwatch;
    FFrom:        Double;
    FTo:          Double;
    FCurrent:     Double;
    FDurationMs:  Cardinal;
    FEasing:      TOBDEasingMode;
    FFPS:         Byte;
    FOnFrame:     TOBDAnimFrameEvent;
    FOnDone:      TOBDAnimDoneEvent;
    FActive:      Boolean;
    procedure TimerTick(Sender: TObject);
    procedure SetFPS(AValue: Byte);
    function  Ease(T: Double): Double;
  public
    constructor Create;
    destructor  Destroy; override;

    /// <summary>Start animating from <c>AFrom</c> to <c>ATo</c>
    /// over <see cref="DurationMs"/>. If an animation is in
    /// flight it's replaced; <c>OnDone</c> fires once for the
    /// new run.</summary>
    procedure Animate(AFrom, ATo: Double);

    /// <summary>Stop the animation. Final value is snapped to
    /// the target; <c>OnDone</c> fires once.</summary>
    procedure Stop;

    /// <summary>Snap to a target without animating.</summary>
    procedure SnapTo(AValue: Double);

    property Active:     Boolean             read FActive;
    property Current:    Double              read FCurrent;
    property DurationMs: Cardinal            read FDurationMs write FDurationMs;
    property Easing:     TOBDEasingMode      read FEasing     write FEasing;
    property FPS:        Byte                read FFPS        write SetFPS;
    property OnFrame:    TOBDAnimFrameEvent  read FOnFrame    write FOnFrame;
    property OnDone:     TOBDAnimDoneEvent   read FOnDone     write FOnDone;
  end;

implementation

{ TOBDValueAnim ------------------------------------------------------------- }

constructor TOBDValueAnim.Create;
begin
  inherited;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTick;
  FFPS := 60;
  FTimer.Interval := 1000 div FFPS;
  FDurationMs := 250;
  FEasing := emSpring;
end;

destructor TOBDValueAnim.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOBDValueAnim.SetFPS(AValue: Byte);
begin
  if AValue < 5 then AValue := 5;
  if AValue > 120 then AValue := 120;
  FFPS := AValue;
  FTimer.Interval := 1000 div FFPS;
end;

procedure TOBDValueAnim.Animate(AFrom, ATo: Double);
begin
  FFrom := AFrom;
  FTo := ATo;
  FCurrent := AFrom;
  FStopwatch := TStopwatch.StartNew;
  FActive := True;
  FTimer.Enabled := True;
end;

procedure TOBDValueAnim.Stop;
var
  FinalVal: Double;
begin
  if not FActive then Exit;
  FTimer.Enabled := False;
  FActive := False;
  FCurrent := FTo;
  FinalVal := FTo;
  if Assigned(FOnFrame) then FOnFrame(Self, FCurrent);
  if Assigned(FOnDone)  then FOnDone(Self, FinalVal);
end;

procedure TOBDValueAnim.SnapTo(AValue: Double);
begin
  FTimer.Enabled := False;
  FActive := False;
  FCurrent := AValue;
  if Assigned(FOnFrame) then FOnFrame(Self, FCurrent);
end;

function TOBDValueAnim.Ease(T: Double): Double;
begin
  // T expected in [0, 1].
  if T < 0 then T := 0;
  if T > 1 then T := 1;
  case FEasing of
    emLinear:    Result := T;
    emEaseIn:    Result := T * T;
    emEaseOut:   Result := 1 - Sqr(1 - T);
    emEaseInOut:
      if T < 0.5 then Result := 4 * T * T * T
      else            Result := 1 - Power(-2 * T + 2, 3) / 2;
    emSpring:
      // Lightly damped spring - overshoots by ~10% then settles.
      // Closed-form approximation: 1 - e^(-6t) * cos(8t).
      Result := 1 - Exp(-6 * T) * Cos(8 * T);
  else
    Result := T;
  end;
end;

procedure TOBDValueAnim.TimerTick(Sender: TObject);
var
  Elapsed: Cardinal;
  T, EasedT: Double;
  V: Double;
begin
  Elapsed := Cardinal(FStopwatch.ElapsedMilliseconds);
  if (FDurationMs = 0) or (Elapsed >= FDurationMs) then
  begin
    Stop;
    Exit;
  end;
  T := Elapsed / FDurationMs;
  EasedT := Ease(T);
  V := FFrom + (FTo - FFrom) * EasedT;
  FCurrent := V;
  if Assigned(FOnFrame) then
    try
      FOnFrame(Self, V);
    except
    end;
end;

end.
