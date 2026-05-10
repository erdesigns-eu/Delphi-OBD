//------------------------------------------------------------------------------
//  VWRadioSAFE — sample 13
//
//  Demonstrates TOBDVWRadioSAFE — the VW SAFE-code recovery
//  component. Wires the OnReadEEPROM event with a synthetic
//  callback that mimics the bytes a real Premium IV radio
//  would return for its 4 ASCII SAFE digits, then asks the
//  component to decode them.
//
//  In production OnReadEEPROM is the host's KWP1281 transport
//  callback (Serial / ELM / TP2.0 / J2534). This sample fakes
//  it so the example runs without hardware.
//------------------------------------------------------------------------------

program VWRadioSAFE;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  OBD.Service.VWRadioSAFE in '..\..\src\Service\OBD.Service.VWRadioSAFE.pas';

procedure FakePremiumIVRead(Sender: TObject;
  AAddress: Word; ALength: Byte;
  out AData: TBytes; out ASuccess: Boolean;
  out AError: string);
begin
  // Premium IV stores the SAFE code as 4 ASCII digits at
  // EEPROM offset 0x0083. Component asks for that range; we
  // hand back the bytes "1234" so the decode has something
  // to work on.
  Writeln(Format('  [OnReadEEPROM addr=$%.4x len=%d]',
    [AAddress, ALength]));
  AData    := TBytes.Create(Ord('1'), Ord('2'), Ord('3'), Ord('4'));
  ASuccess := True;
  AError   := '';
end;

var
  SAFE: TOBDVWRadioSAFE;
  R:    TVWRadioSAFEResult;
begin
  Writeln('TOBDVWRadioSAFE demo (synthetic OnReadEEPROM)');
  Writeln(StringOfChar('-', 60));

  SAFE := TOBDVWRadioSAFE.Create(nil);
  try
    SAFE.RadioVariant := svPremiumIV;
    SAFE.OnReadEEPROM := FakePremiumIVRead;
    R := SAFE.Extract;
    if R.Success then
    begin
      Writeln('Decoded:');
      Writeln('  variant : ', R.VariantUsed);
      Writeln('  code    : ', R.Code);
      Writeln('  raw     : ', R.RawHex);
    end
    else
      Writeln('Extract failed: ', R.Message);
  finally
    SAFE.Free;
  end;
  Writeln(StringOfChar('-', 60));
  Writeln('Done.');
end.
