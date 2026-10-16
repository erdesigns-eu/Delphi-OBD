unit ERD.Components.PCBLayout;
interface
uses System.Types, System.Math;
type
  TERDPCBLayoutGuideKind = (lgNone, lgAlignment, lgSpacing);
  TERDPCBLayoutGuide = record
    Kind: TERDPCBLayoutGuideKind;
    Coordinate, SpanStart, SpanEnd: Double;
    Gaps: array[0..1] of TPointF;
  end;
/// Screen-space solver. Tolerance stays independent of PCB zoom.
procedure ERDPCBSnapLayout(const Moving: TRectF; const Targets: TArray<TRectF>;
  const Board: TRectF; HasBoard, Align, Spacing: Boolean; Tolerance: Double;
  out Correction: TPointF; out XGuide, YGuide: TERDPCBLayoutGuide);
implementation
procedure ERDPCBSnapLayout(const Moving: TRectF; const Targets: TArray<TRectF>;
  const Board: TRectF; HasBoard, Align, Spacing: Boolean; Tolerance: Double;
  out Correction: TPointF; out XGuide, YGuide: TERDPCBLayoutGuide);
  procedure SolveAxis(Horizontal: Boolean; out Offset: Single; out Guide: TERDPCBLayoutGuide);
  var Best, Start, Finish, Size, CrossStart, CrossEnd, Delta, Gap, Candidate, Marker: Double;
      A, B, I, J, K, N: Integer; Other, First, Second: TRectF; Indices: TArray<Integer>;
      Own, Reference: array[0..2] of Double;
    function AxisStart(const R: TRectF): Double;
    begin if Horizontal then Result := R.Left else Result := R.Top; end;
    function AxisEnd(const R: TRectF): Double;
    begin if Horizontal then Result := R.Right else Result := R.Bottom; end;
    function PerpendicularStart(const R: TRectF): Double;
    begin if Horizontal then Result := R.Top else Result := R.Left; end;
    function PerpendicularEnd(const R: TRectF): Double;
    begin if Horizontal then Result := R.Bottom else Result := R.Right; end;
    procedure ConsiderSpacing(Position, A1, A2, B1, B2: Double);
    var Distance: Double;
    begin
      Distance := Abs(Position-Start);
      if (Distance > Tolerance) or (Distance >= Best) then Exit;
      Best := Distance; Offset := Position-Start; Guide.Kind := lgSpacing;
      Guide.Coordinate := Marker;
      Guide.Gaps[0] := PointF(A1,A2); Guide.Gaps[1] := PointF(B1,B2);
    end;
  begin
    Guide := Default(TERDPCBLayoutGuide); Offset := 0; Best := Tolerance+0.001;
    Start := AxisStart(Moving); Finish := AxisEnd(Moving); Size := Finish-Start;
    CrossStart := PerpendicularStart(Moving); CrossEnd := PerpendicularEnd(Moving);
    Own[0] := Start; Own[1] := (Start+Finish)/2; Own[2] := Finish;
    if Align then for I := -1 to High(Targets) do
    begin
      if I < 0 then begin if not HasBoard then Continue; Other := Board; end else Other := Targets[I];
      Reference[0] := AxisStart(Other); Reference[1] := (AxisStart(Other)+AxisEnd(Other))/2; Reference[2] := AxisEnd(Other);
      for A := 0 to 2 do for B := 0 to 2 do
      begin
        Delta := Reference[B]-Own[A];
        if (Abs(Delta) <= Tolerance) and (Abs(Delta) < Best) then
        begin
          Best := Abs(Delta); Offset := Delta; Guide.Kind := lgAlignment;
          Guide.Coordinate := Reference[B];
          Guide.SpanStart := Min(CrossStart,PerpendicularStart(Other));
          Guide.SpanEnd := Max(CrossEnd,PerpendicularEnd(Other));
        end;
      end;
    end;
    if not Spacing then Exit;
    // Only compare consecutive objects in the same row/column, never overlapping pairs.
    for I := 0 to High(Targets) do
    begin
      Other := Targets[I];
      if Min(CrossEnd,PerpendicularEnd(Other)) <= Max(CrossStart,PerpendicularStart(Other)) then Continue;
      N := Length(Indices); SetLength(Indices,N+1); J := N;
      while J > 0 do
      begin
        if AxisStart(Targets[Indices[J-1]]) <= AxisStart(Other) then Break;
        Indices[J] := Indices[J-1]; Dec(J);
      end;
      Indices[J] := I;
    end;
    for K := 0 to High(Indices)-1 do
    begin
      First := Targets[Indices[K]]; Second := Targets[Indices[K+1]];
      if Min(PerpendicularEnd(First),PerpendicularEnd(Second)) <= Max(PerpendicularStart(First),PerpendicularStart(Second)) then Continue;
      Gap := AxisStart(Second)-AxisEnd(First); if Gap < 0 then Continue;
      Marker := Max(CrossEnd,Max(PerpendicularEnd(First),PerpendicularEnd(Second)));
      if Gap >= Size then
      begin
        Candidate := (AxisEnd(First)+AxisStart(Second)-Size)/2;
        ConsiderSpacing(Candidate,AxisEnd(First),Candidate,Candidate+Size,AxisStart(Second));
      end;
      Candidate := AxisEnd(Second)+Gap;
      ConsiderSpacing(Candidate,AxisEnd(First),AxisStart(Second),AxisEnd(Second),Candidate);
      Candidate := AxisStart(First)-Gap-Size;
      ConsiderSpacing(Candidate,Candidate+Size,AxisStart(First),AxisEnd(First),AxisStart(Second));
    end;
  end;
begin
  SolveAxis(True,Correction.X,XGuide); SolveAxis(False,Correction.Y,YGuide);
end;
end.
