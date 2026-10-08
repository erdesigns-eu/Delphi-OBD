//------------------------------------------------------------------------------
//  ERD.JSON
//
//  Checked JSON shape conversions for catalog readers.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-10-08  Add checked conversions with explicit ownership.
//------------------------------------------------------------------------------
unit ERD.JSON;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IF FPC_FULLVERSION >= 30301}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

uses
  System.JSON,
  ERD.Types;

/// <summary>Requires an object without taking ownership of its value.</summary>
/// <param name="AValue">Borrowed value, including nil.</param>
/// <returns>The same value as a JSON object.</returns>
/// <exception cref="EOBDConfig">The value is not an object.</exception>
function RequireOBDJSONObject(AValue: TJSONValue): TJSONObject;

/// <summary>Reads a string without taking ownership of its value.</summary>
/// <param name="AValue">Borrowed value, including nil.</param>
/// <returns>The JSON string's content.</returns>
/// <exception cref="EOBDConfig">The value is not a string.</exception>
function RequireOBDJSONString(AValue: TJSONValue): string;

/// <summary>Parses an object, releasing an invalid root on failure.</summary>
/// <param name="AText">JSON document text.</param>
/// <returns>An object owned by the caller, who must free it.</returns>
/// <exception cref="EOBDConfig">Invalid JSON or a non-object root.</exception>
function ParseOBDJSONObject(const AText: string): TJSONObject;

implementation

function RequireOBDJSONObject(AValue: TJSONValue): TJSONObject;
begin
  if not (AValue is TJSONObject) then
    raise EOBDConfig.Create('Catalogue JSON: expected an object');
  Result := TJSONObject(AValue);
end;

function RequireOBDJSONString(AValue: TJSONValue): string;
begin
  if not (AValue is TJSONString) then
    raise EOBDConfig.Create('Catalogue JSON: expected a string');
  Result := TJSONString(AValue).Value;
end;

function ParseOBDJSONObject(const AText: string): TJSONObject;
var
  Root: TJSONValue;
begin
  Root := TJSONObject.ParseJSONValue(AText);
  try
    Result := RequireOBDJSONObject(Root);
  except
    Root.Free;
    raise;
  end;
end;

end.
