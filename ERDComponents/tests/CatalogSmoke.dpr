program CatalogSmoke;
{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}
uses SysUtils, ERD.Components.IconCatalog, ERD.Components.IconData;
var Id: TERDIconId; Index, Position, Limit, Count: Integer;
procedure Require(Value: Boolean; const Text: string);
begin
  if not Value then raise Exception.Create(Text);
end;
function NextPrimitive(At: Integer; Boundary: Integer): Integer;
var Code, N: Integer;
begin
  Require(At < Boundary, 'Primitive exceeds icon');
  Code := Round(ERD_ICON_DATA[At]);
  case Code of
    ERD_ICON_LINE, ERD_ICON_LOOP, ERD_ICON_FILL:
      begin
        Require(At + 1 < Boundary, 'Missing point count');
        N := Round(ERD_ICON_DATA[At+1]); Require(N >= 2, 'Invalid point count');
        Result := At + 2 + N*2;
      end;
    ERD_ICON_ARC, ERD_ICON_PIE: Result := At + 6;
    ERD_ICON_RING, ERD_ICON_DISC: Result := At + 4;
    ERD_ICON_ELLIPSE: Result := At + 5;
    ERD_ICON_KNOCK, ERD_ICON_INK: Result := NextPrimitive(At+1, Boundary);
  else raise Exception.Create('Unknown vector operation');
  end;
  Require(Result <= Boundary, 'Primitive crosses icon boundary');
end;
begin
  Require(Ord(High(TERDIconId)) = ERD_CATALOG_COUNT, 'Catalog enum/count mismatch');
  Count := 0;
  for Id := Succ(eiNone) to High(TERDIconId) do
  begin
    Require(ERD_ICON_NAMES[Id] <> '', 'Missing name');
    Index := ERD_ICON_CATALOG_FIRST + Ord(Id) - 1;
    Require(Index < ERD_ICON_COUNT, 'Missing catalog vector');
    Position := ERD_ICON_START[Index]; Limit := ERD_ICON_START[Index+1];
    Require(Position < Limit, 'Empty catalog vector');
    while Position < Limit do Position := NextPrimitive(Position, Limit);
    Inc(Count);
  end;
  Writeln(Count, ' compiled catalog icons decoded without crossing vector boundaries.');
end.
