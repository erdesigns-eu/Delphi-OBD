// ------------------------------------------------------------------------------
// ERD.CAN.Route
//
// Validated ELM CAN routing command plans shared by protocol and adapter.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-10-08  ERD  Atomic request routing and extended-address plans.
// ------------------------------------------------------------------------------
unit ERD.CAN.Route;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, ERD.Types;

/// <summary>Format an 11-bit or 29-bit CAN identifier for ELM commands.</summary>
/// <param name="AID">CAN identifier, at most 29 bits.</param>
/// <returns>Three or eight uppercase hexadecimal digits.</returns>
/// <exception cref="EOBDConfig">Identifier exceeds 29 bits.</exception>
function CANHeader(AID: Cardinal): string;

/// <summary>Build routing commands. Empty header and filter with no extended
/// addressing means use the adapter's current configuration.</summary>
/// <param name="AHeader">Optional transmit identifier, three or eight hex digits.</param>
/// <param name="AFilter">Optional receive identifier; empty clears filtering.</param>
/// <param name="AExtended">Use ISO-TP extended addressing.</param>
/// <param name="ATxAddress">Destination extended-address byte.</param>
/// <param name="ARxAddress">Tester extended-address byte expected in replies.</param>
/// <returns>Commands to execute under the same lock as the diagnostic request.</returns>
/// <exception cref="EOBDConfig">Malformed or out-of-range CAN header.</exception>
function CANRouteCommands(const AHeader, AFilter: string; AExtended: Boolean;
  ATxAddress, ARxAddress: Byte): TArray<string>;

implementation

function CANHeader(AID: Cardinal): string;
begin
  if AID > $1FFFFFFF then
    raise EOBDConfig.Create('CAN identifier exceeds 29 bits');
  if AID <= $7FF then
    Result := IntToHex(AID, 3)
  else
    Result := IntToHex(AID, 8);
end;

function NormaliseHeader(const AValue: string): string;
var
  Value: Int64;
  C: Char;
begin
  Result := UpperCase(Trim(AValue));
  if Result = '' then
    Exit;
  if (Length(Result) <> 3) and (Length(Result) <> 8) then
    raise EOBDConfig.Create('CAN header requires three or eight hex digits');
  for C in Result do
    if not CharInSet(C, ['0' .. '9', 'A' .. 'F']) then
      raise EOBDConfig.Create('Invalid hexadecimal CAN header');
  if not TryStrToInt64('$' + Result, Value) then
    raise EOBDConfig.Create('Invalid CAN identifier');
  if ((Length(Result) = 3) and (Value > $7FF)) or (Value > $1FFFFFFF) then
    raise EOBDConfig.Create('CAN identifier out of range');
end;

function CANRouteCommands(const AHeader, AFilter: string; AExtended: Boolean;
  ATxAddress, ARxAddress: Byte): TArray<string>;
var
  Header, Filter: string;
  Count: Integer;
  procedure Add(const ACommand: string);
  begin
    Result[Count] := ACommand;
    Inc(Count);
  end;

begin
  Header := NormaliseHeader(AHeader);
  Filter := NormaliseHeader(AFilter);
  Result := nil;
  if (Header = '') and (Filter = '') and not AExtended then
    Exit;
  if AExtended and ((Header = '') or (Filter = '')) then
    raise EOBDConfig.Create
      ('Extended CAN addressing requires transmit and receive IDs');
  SetLength(Result, 4);
  Count := 0;
  if Header <> '' then
    Add('ATSH' + Header);
  Add('ATCRA' + Filter);
  if AExtended then
  begin
    Add('ATCEA' + IntToHex(ATxAddress, 2));
    Add('ATCER' + IntToHex(ARxAddress, 2));
  end
  else
    Add('ATCEA');
  SetLength(Result, Count);
end;

end.
