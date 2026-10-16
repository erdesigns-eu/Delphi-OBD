unit ERD.Components.PCBRoute;
interface
uses System.Types, System.Math, System.Generics.Collections;
type
  TERDPCBPathSegment = record
    Cubic: Boolean;
    Points: array[0..3] of TPointF;
  end;
  TERDPCBConnectionGeometry = record
    Segments: TArray<TERDPCBPathSegment>;
    Samples: TArray<TPointF>;
  end;
function ERDBuildPCBConnection(const FromPoint, ToPoint: TPoint; const Obstacles: TArray<TRect>;
  Scale: Single): TERDPCBConnectionGeometry;
/// Route outside the physical board, with short normal entry/exit corridors.
function ERDBuildPCBBoardConnection(const FromPoint, ToPoint: TPoint; const Obstacles: TArray<TRect>;
  const Board: TRect; Scale: Single): TERDPCBConnectionGeometry;
function ERDPCBConnectionDistance(const Geometry: TERDPCBConnectionGeometry; const P: TPointF): Double;
// Screen-coordinate orthogonal routing around expanded scene rectangles.
function ERDPCBRoute(const StartPoint, EndPoint: TPoint;
  const Obstacles: TArray<TRect>; Clearance: Integer): TArray<TPoint>;
implementation
function ERDPCBRoute(const StartPoint, EndPoint: TPoint;
  const Obstacles: TArray<TRect>; Clearance: Integer): TArray<TPoint>;
var Xs, Ys: TList<Integer>; Open: TList<Integer>; Boxes: TArray<TRect>;
    Cost: TArray<Double>; Parent: TArray<Integer>; Closed: TArray<Boolean>;
    I, N, Source, Target, Current, Next, Best, X, Y, W, H, Step: Integer;
    BestScore, Score, Distance: Double; P, Q: TPoint; Reverse: TList<TPoint>;
  procedure AddCoordinate(List: TList<Integer>; Value: Integer);
  begin if List.IndexOf(Value) < 0 then List.Add(Value); end;
  function NodePoint(Index: Integer): TPoint;
  begin Result := Point(Xs[Index mod W], Ys[Index div W]); end;
  function ClearSegment(const A, B: TPoint): Boolean;
  var Box: TRect;
  begin
    Result := False;
    for Box in Boxes do
      if A.X = B.X then
      begin
        if (A.X > Box.Left) and (A.X < Box.Right) and
          (Max(A.Y, B.Y) > Box.Top) and (Min(A.Y, B.Y) < Box.Bottom) then Exit;
      end
      else if (A.Y > Box.Top) and (A.Y < Box.Bottom) and
        (Max(A.X, B.X) > Box.Left) and (Min(A.X, B.X) < Box.Right) then Exit;
    Result := True;
  end;
begin
  Result := nil;
  if StartPoint = EndPoint then begin SetLength(Result, 1); Result[0] := StartPoint; Exit; end;
  Xs := TList<Integer>.Create; Ys := TList<Integer>.Create;
  Open := TList<Integer>.Create; Reverse := TList<TPoint>.Create;
  try
    AddCoordinate(Xs, StartPoint.X); AddCoordinate(Xs, EndPoint.X);
    AddCoordinate(Ys, StartPoint.Y); AddCoordinate(Ys, EndPoint.Y);
    SetLength(Boxes, Length(Obstacles));
    for I := 0 to High(Obstacles) do
    begin
      Boxes[I] := Obstacles[I]; Boxes[I].Inflate(Clearance, Clearance);
      AddCoordinate(Xs, Boxes[I].Left); AddCoordinate(Xs, Boxes[I].Right);
      AddCoordinate(Ys, Boxes[I].Top); AddCoordinate(Ys, Boxes[I].Bottom);
    end;
    Xs.Sort; Ys.Sort; W := Xs.Count; H := Ys.Count; N := W * H;
    SetLength(Cost, N); SetLength(Parent, N); SetLength(Closed, N);
    for I := 0 to N - 1 do begin Cost[I] := MaxDouble; Parent[I] := -1; end;
    Source := Ys.IndexOf(StartPoint.Y) * W + Xs.IndexOf(StartPoint.X);
    Target := Ys.IndexOf(EndPoint.Y) * W + Xs.IndexOf(EndPoint.X);
    Cost[Source] := 0; Open.Add(Source);
    while Open.Count > 0 do
    begin
      Best := 0; BestScore := MaxDouble;
      for I := 0 to Open.Count - 1 do
      begin
        P := NodePoint(Open[I]);
        Score := Cost[Open[I]] + Abs(P.X - EndPoint.X) + Abs(P.Y - EndPoint.Y);
        if Score < BestScore then begin Best := I; BestScore := Score; end;
      end;
      Current := Open[Best]; Open.Delete(Best);
      if Current = Target then Break;
      Closed[Current] := True; P := NodePoint(Current); X := Current mod W; Y := Current div W;
      for Step := 0 to 3 do
      begin
        case Step of
          0: begin if X = 0 then Continue; Next := Current - 1; end;
          1: begin if X = W - 1 then Continue; Next := Current + 1; end;
          2: begin if Y = 0 then Continue; Next := Current - W; end;
        else begin if Y = H - 1 then Continue; Next := Current + W; end;
        end;
        if Closed[Next] then Continue;
        Q := NodePoint(Next); if not ClearSegment(P, Q) then Continue;
        Distance := Cost[Current] + Abs(P.X - Q.X) + Abs(P.Y - Q.Y);
        if Distance >= Cost[Next] then Continue;
        Cost[Next] := Distance; Parent[Next] := Current;
        if Open.IndexOf(Next) < 0 then Open.Add(Next);
      end;
    end;
    if Parent[Target] < 0 then Exit;
    Current := Target;
    while Current >= 0 do begin Reverse.Add(NodePoint(Current)); Current := Parent[Current]; end;
    // Collapse grid nodes on straight runs before the painter rounds the corners.
    SetLength(Result, Reverse.Count); N := 0;
    for I := Reverse.Count - 1 downto 0 do
    begin
      P := Reverse[I];
      if N >= 2 then
        if ((Result[N-2].X = Result[N-1].X) and (Result[N-1].X = P.X)) or
           ((Result[N-2].Y = Result[N-1].Y) and (Result[N-1].Y = P.Y)) then Dec(N);
      Result[N] := P; Inc(N);
    end;
    SetLength(Result, N);
  finally Reverse.Free; Open.Free; Ys.Free; Xs.Free; end;
end;
procedure AddPCBPathSegment(var Geometry: TERDPCBConnectionGeometry;
  const P0, P1, P2, P3: TPointF; Cubic: Boolean; Scale: Single);
  var S, Steps, K, Count: Integer; T, U, LengthEstimate: Double; Sample: TPointF;
  begin
    S := Length(Geometry.Segments); SetLength(Geometry.Segments, S + 1);
    Geometry.Segments[S].Cubic := Cubic; Geometry.Segments[S].Points[0] := P0;
    Geometry.Segments[S].Points[1] := P1; Geometry.Segments[S].Points[2] := P2; Geometry.Segments[S].Points[3] := P3;
    Count := Length(Geometry.Samples);
    if Count = 0 then begin SetLength(Geometry.Samples, 1); Geometry.Samples[0] := P0; Count := 1; end;
    Steps := 1;
    if Cubic then
    begin
      LengthEstimate := Hypot(P1.X-P0.X, P1.Y-P0.Y) + Hypot(P2.X-P1.X, P2.Y-P1.Y) + Hypot(P3.X-P2.X, P3.Y-P2.Y);
      Steps := Max(16, Min(256, Ceil(LengthEstimate / (4 * Max(0.1, Scale)))));
    end;
    SetLength(Geometry.Samples, Count + Steps);
    for K := 1 to Steps do
    begin
      T := K / Steps; U := 1 - T;
      if Cubic then
      begin
        Sample.X := U*U*U*P0.X + 3*U*U*T*P1.X + 3*U*T*T*P2.X + T*T*T*P3.X;
        Sample.Y := U*U*U*P0.Y + 3*U*U*T*P1.Y + 3*U*T*T*P2.Y + T*T*T*P3.Y;
      end
      else Sample := P3;
      Geometry.Samples[Count + K - 1] := Sample;
    end;
  end;
function ERDBuildPCBConnection(const FromPoint, ToPoint: TPoint; const Obstacles: TArray<TRect>;
  Scale: Single): TERDPCBConnectionGeometry;
var Direction, Gap, Stub, Bend, StartY, EndY, Radius, LengthIn, LengthOut: Single;
    Route, Points: TArray<TPoint>; I, N: Integer; Blocked: Boolean;
    A, B, C, Previous, BeforeCorner, AfterCorner: TPointF; Segment, Expanded: TRect; Box: TRect;
  procedure AddSegment(const P0, P1, P2, P3: TPointF; Cubic: Boolean);
  begin AddPCBPathSegment(Result,P0,P1,P2,P3,Cubic,Scale); end;
  procedure Line(const P0, P1: TPointF);
  begin AddSegment(P0, P0, P1, P1, False); end;
begin
  Result := Default(TERDPCBConnectionGeometry); Scale := Max(0.1, Scale);
  Direction := 1; if ToPoint.Y < FromPoint.Y then Direction := -1;
  Gap := Abs(ToPoint.Y - FromPoint.Y); Stub := Min(8 * Scale, Gap / 4);
  Bend := Min(Gap / 3, Max(24 * Scale, Min(160 * Scale, (Gap + Abs(ToPoint.X - FromPoint.X)) * 0.35)));
  StartY := FromPoint.Y + Direction * Stub; EndY := ToPoint.Y - Direction * Stub;
  Line(PointF(FromPoint.X, FromPoint.Y), PointF(FromPoint.X, StartY));
  AddSegment(PointF(FromPoint.X, StartY), PointF(FromPoint.X, StartY + Direction * Bend),
    PointF(ToPoint.X, EndY - Direction * Bend), PointF(ToPoint.X, EndY), True);
  Line(PointF(ToPoint.X, EndY), PointF(ToPoint.X, ToPoint.Y));
  Blocked := False;
  for I := 1 to High(Result.Samples) do
  begin
    A := Result.Samples[I-1]; B := Result.Samples[I];
    Segment := Rect(Floor(Min(A.X,B.X)), Floor(Min(A.Y,B.Y)), Ceil(Max(A.X,B.X)) + 1, Ceil(Max(A.Y,B.Y)) + 1);
    for Box in Obstacles do
    begin
      Expanded := Box; Expanded.Inflate(Round(4 * Scale), Round(4 * Scale));
      if (Segment.Left < Expanded.Right) and (Segment.Right > Expanded.Left) and
         (Segment.Top < Expanded.Bottom) and (Segment.Bottom > Expanded.Top) then begin Blocked := True; Break; end;
    end;
    if Blocked then Break;
  end;
  if not Blocked then Exit;
  Route := ERDPCBRoute(Point(FromPoint.X, Round(StartY)), Point(ToPoint.X, Round(EndY)), Obstacles, Round(8 * Scale));
  if Length(Route) = 0 then Exit;
  Result := Default(TERDPCBConnectionGeometry);
  N := Length(Route); SetLength(Points, N + 2); Points[0] := FromPoint;
  for I := 0 to N - 1 do Points[I+1] := Route[I]; Points[N+1] := ToPoint;
  Previous := PointF(FromPoint.X, FromPoint.Y);
  for I := 1 to High(Points) - 1 do
  begin
    A := PointF(Points[I-1].X, Points[I-1].Y); B := PointF(Points[I].X, Points[I].Y); C := PointF(Points[I+1].X, Points[I+1].Y);
    LengthIn := Hypot(B.X-A.X, B.Y-A.Y); LengthOut := Hypot(C.X-B.X, C.Y-B.Y);
    if (LengthIn = 0) or (LengthOut = 0) then Continue;
    Radius := Min(4 * Scale, Min(LengthIn, LengthOut) / 2);
    BeforeCorner := PointF(B.X + (A.X-B.X)*Radius/LengthIn, B.Y + (A.Y-B.Y)*Radius/LengthIn);
    AfterCorner := PointF(B.X + (C.X-B.X)*Radius/LengthOut, B.Y + (C.Y-B.Y)*Radius/LengthOut);
    Line(Previous, BeforeCorner); AddSegment(BeforeCorner, B, B, AfterCorner, True); Previous := AfterCorner;
  end;
  Line(Previous, PointF(ToPoint.X, ToPoint.Y));
end;
function ERDBuildPCBBoardConnection(const FromPoint, ToPoint: TPoint; const Obstacles: TArray<TRect>;
  const Board: TRect; Scale: Single): TERDPCBConnectionGeometry;
var StartGate, EndGate: TPoint; Boxes: TArray<TRect>; Route, Points: TArray<TPoint>;
    Margin, I, N: Integer; Distances: array[0..3] of Integer;
    A, B, C, Previous, BeforeCorner, AfterCorner: TPointF; Radius, LengthIn, LengthOut: Single;
  function Gate(const P: TPoint): TPoint;
  var Expanded: TRect; Edge, J: Integer;
  begin
    Result := P; Expanded := Board; Expanded.Inflate(Margin,Margin);
    if not Expanded.Contains(P) then Exit;
    if P.X < Board.Left then Edge := 0
    else if P.X > Board.Right then Edge := 1
    else if P.Y < Board.Top then Edge := 2
    else if P.Y > Board.Bottom then Edge := 3
    else
    begin
      Distances[0] := P.X-Board.Left; Distances[1] := Board.Right-P.X;
      Distances[2] := P.Y-Board.Top; Distances[3] := Board.Bottom-P.Y; Edge := 0;
      for J := 1 to 3 do if Distances[J] < Distances[Edge] then Edge := J;
    end;
    case Edge of 0: Result.X := Board.Left-Margin; 1: Result.X := Board.Right+Margin;
      2: Result.Y := Board.Top-Margin; 3: Result.Y := Board.Bottom+Margin; end;
  end;
  procedure Append(const P: TPoint);
  var Count: Integer;
  begin
    Count := Length(Points); if (Count > 0) and (Points[Count-1] = P) then Exit;
    if Count >= 2 then
      if ((Points[Count-2].X = Points[Count-1].X) and (Points[Count-1].X = P.X)) or
        ((Points[Count-2].Y = Points[Count-1].Y) and (Points[Count-1].Y = P.Y)) then
      begin Points[Count-1] := P; Exit; end;
    SetLength(Points,Count+1); Points[Count] := P;
  end;
  procedure Line(const P0, P1: TPointF);
  begin if P0 <> P1 then AddPCBPathSegment(Result,P0,P0,P1,P1,False,Scale); end;
begin
  if Board.IsEmpty then Exit(ERDBuildPCBConnection(FromPoint,ToPoint,Obstacles,Scale));
  Result := Default(TERDPCBConnectionGeometry); Scale := Max(0.1,Scale);
  Margin := Max(1,Ceil(24*Scale)); StartGate := Gate(FromPoint); EndGate := Gate(ToPoint);
  Boxes := Copy(Obstacles); N := Length(Boxes); SetLength(Boxes,N+1); Boxes[N] := Board;
  Route := ERDPCBRoute(StartGate,EndGate,Boxes,Max(1,Round(8*Scale)));
  // A failed route must never fall back to a diagonal curve through the physical PCB.
  if Length(Route) = 0 then Exit;
  Append(FromPoint); for I := 0 to High(Route) do Append(Route[I]); Append(ToPoint);
  Previous := PointF(FromPoint.X,FromPoint.Y);
  for I := 1 to High(Points)-1 do
  begin
    A := PointF(Points[I-1].X,Points[I-1].Y); B := PointF(Points[I].X,Points[I].Y); C := PointF(Points[I+1].X,Points[I+1].Y);
    LengthIn := Hypot(B.X-A.X,B.Y-A.Y); LengthOut := Hypot(C.X-B.X,C.Y-B.Y);
    if (LengthIn = 0) or (LengthOut = 0) then Continue;
    Radius := Min(4*Scale,Min(LengthIn,LengthOut)/2);
    BeforeCorner := PointF(B.X+(A.X-B.X)*Radius/LengthIn,B.Y+(A.Y-B.Y)*Radius/LengthIn);
    AfterCorner := PointF(B.X+(C.X-B.X)*Radius/LengthOut,B.Y+(C.Y-B.Y)*Radius/LengthOut);
    Line(Previous,BeforeCorner); AddPCBPathSegment(Result,BeforeCorner,B,B,AfterCorner,True,Scale); Previous := AfterCorner;
  end;
  Line(Previous,PointF(ToPoint.X,ToPoint.Y));
end;
function ERDPCBConnectionDistance(const Geometry: TERDPCBConnectionGeometry; const P: TPointF): Double;
var I: Integer; A, B: TPointF; DX, DY, T, Denominator: Double;
begin
  Result := MaxDouble;
  for I := 1 to High(Geometry.Samples) do
  begin
    A := Geometry.Samples[I-1]; B := Geometry.Samples[I]; DX := B.X - A.X; DY := B.Y - A.Y;
    Denominator := DX*DX + DY*DY;
    if Denominator = 0 then T := 0 else T := EnsureRange(((P.X-A.X)*DX + (P.Y-A.Y)*DY) / Denominator, 0.0, 1.0);
    Result := Min(Result, Hypot(P.X - A.X - T*DX, P.Y - A.Y - T*DY));
  end;
end;
end.
