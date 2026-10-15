unit ERD.Components.PCBRoute;
interface
uses System.Types, System.Math, System.Generics.Collections;
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
end.
