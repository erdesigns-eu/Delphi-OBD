program OEMDemo;

//------------------------------------------------------------------------------
// PROGRAM        : OEMDemo
// CONTENTS       : Demonstrates OEM-specific UDS extension lookup
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// USAGE          : OEMDemo <vin>
//                  OEMDemo <vin> decode <did-hex> <payload-hex>
//------------------------------------------------------------------------------

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.OEM in '..\..\src\Services\OBD.OEM.pas',
  OBD.OEM.Helpers in '..\..\src\Services\OBD.OEM.Helpers.pas',
  OBD.OEM.VW in '..\..\src\Services\OBD.OEM.VW.pas',
  OBD.OEM.BMW in '..\..\src\Services\OBD.OEM.BMW.pas',
  OBD.OEM.Mercedes in '..\..\src\Services\OBD.OEM.Mercedes.pas',
  OBD.OEM.Ford in '..\..\src\Services\OBD.OEM.Ford.pas',
  OBD.OEM.GM in '..\..\src\Services\OBD.OEM.GM.pas',
  OBD.OEM.Stellantis in '..\..\src\Services\OBD.OEM.Stellantis.pas';

procedure ListCatalog(const Ext: IOBDOEMExtension);
var
  D: TOBDOEMDataIdentifier;
  R: TOBDOEMRoutine;
begin
  Writeln(Format('Manufacturer: %s (%s)',
    [Ext.DisplayName, Ext.ManufacturerKey]));
  Writeln('Catalogued DIDs:');
  for D in Ext.DataIdentifiers do
    Writeln(Format('  0x%.4X  %-32s  %s', [D.DID, D.Name, D.Description]));
  Writeln('Catalogued routines:');
  for R in Ext.Routines do
    Writeln(Format('  0x%.4X  %-32s  %s', [R.Identifier, R.Name, R.Description]));
end;

function ParseHexBytes(const S: string): TBytes;
var
  I: Integer;
  Pair: string;
  Idx: Integer;
begin
  // Tolerate "DE AD BE EF" or "DEADBEEF".
  Pair := StringReplace(S, ' ', '', [rfReplaceAll]);
  if (Length(Pair) mod 2) <> 0 then
    raise Exception.Create('Hex string must have an even length');
  SetLength(Result, Length(Pair) div 2);
  Idx := 0;
  I := 1;
  while I <= Length(Pair) do
  begin
    Result[Idx] := StrToInt('$' + Copy(Pair, I, 2));
    Inc(Idx);
    Inc(I, 2);
  end;
end;

var
  VIN: string;
  Ext: IOBDOEMExtension;
  DID: Word;
  Payload: TBytes;
begin
  if ParamCount < 1 then
  begin
    Writeln('Usage:');
    Writeln('  OEMDemo <vin>');
    Writeln('  OEMDemo <vin> decode <did-hex> <payload-hex>');
    ExitCode := 1;
    Exit;
  end;

  try
    VIN := ParamStr(1);
    Ext := TOBDOEMRegistry.FindByVIN(VIN);
    if Ext = nil then
    begin
      Writeln(Format('No OEM extension matches VIN %s. Registered: %d',
        [VIN, TOBDOEMRegistry.Count]));
      ExitCode := 2;
      Exit;
    end;

    if (ParamCount >= 4) and SameText(ParamStr(2), 'decode') then
    begin
      DID := StrToInt('$' + ParamStr(3));
      Payload := ParseHexBytes(ParamStr(4));
      Writeln(Ext.DecodeDID(DID, Payload));
    end
    else
      ListCatalog(Ext);
  except
    on E: Exception do
    begin
      Writeln('Error: ', E.Message);
      ExitCode := 3;
    end;
  end;
end.
