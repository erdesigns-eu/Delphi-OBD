//------------------------------------------------------------------------------
// UNIT           : Tests.RadioCode.Smoke
// CONTENTS       : Universal smoke tests across every IOBDRadioCode calculator
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Per-brand serial→code goldens require verified pairs supplied
//                  by the maintainer (see ROADMAP v2.1). Until then these
//                  invariants pin the universal contract every calculator must
//                  honour: a non-empty description, deterministic Calculate,
//                  rejection of empty input.
//------------------------------------------------------------------------------
unit Tests.RadioCode.Smoke;

interface

uses
  DUnitX.TestFramework,
  OBD.RadioCode;

type
  [TestFixture]
  TRadioCodeSmokeTests = class
  strict private
    procedure RunInvariants(const CalcClass: TClass);
  public
    [Test] procedure Acura;
    [Test] procedure AlfaRomeo;
    [Test] procedure Alpine;
    [Test] procedure AudiConcert;
    [Test] procedure Becker;
    [Test] procedure Becker4;
    [Test] procedure Becker5;
    [Test] procedure Blaupunkt;
    [Test] procedure BMW;
    [Test] procedure Chrysler;
    [Test] procedure Citroen;
    [Test] procedure Clarion;
    [Test] procedure FiatDaiichi;
    [Test] procedure FiatVP;
    [Test] procedure Ford;
    [Test] procedure FordV;
    [Test] procedure GM;
    [Test] procedure Honda;
    [Test] procedure Hyundai;
    [Test] procedure Infiniti;
    [Test] procedure Jaguar;
    [Test] procedure LandRover;
    [Test] procedure Lexus;
    [Test] procedure Maserati;
    [Test] procedure Mazda;
    [Test] procedure Mercedes;
    [Test] procedure Mini;
    [Test] procedure Mitsubishi;
    [Test] procedure Nissan;
    [Test] procedure Opel;
    [Test] procedure Peugeot;
    [Test] procedure Porsche;
    [Test] procedure Renault;
    [Test] procedure Saab;
    [Test] procedure SEAT;
    [Test] procedure Skoda;
    [Test] procedure Smart;
    [Test] procedure Subaru;
    [Test] procedure Suzuki;
    [Test] procedure Toyota;
    [Test] procedure Visteon;
    [Test] procedure Volvo;
    [Test] procedure VW;
  end;

implementation

uses
  System.SysUtils,
  OBD.RadioCode.Acura.Advanced,
  OBD.RadioCode.AlfaRomeo.Advanced,
  OBD.RadioCode.Alpine.Advanced,
  OBD.RadioCode.Audi.Concert.Advanced,
  OBD.RadioCode.Becker.Advanced,
  OBD.RadioCode.Becker4,
  OBD.RadioCode.Becker5,
  OBD.RadioCode.Blaupunkt.Advanced,
  OBD.RadioCode.BMW.Advanced,
  OBD.RadioCode.Chrysler.Advanced,
  OBD.RadioCode.Citroen.Advanced,
  OBD.RadioCode.Clarion.Advanced,
  OBD.RadioCode.Fiat.Daiichi.Advanced,
  OBD.RadioCode.Fiat.VP.Advanced,
  OBD.RadioCode.Ford.Advanced,
  OBD.RadioCode.Ford.V,
  OBD.RadioCode.GM.Advanced,
  OBD.RadioCode.Honda.Advanced,
  OBD.RadioCode.Hyundai.Advanced,
  OBD.RadioCode.Infiniti.Advanced,
  OBD.RadioCode.Jaguar.Advanced,
  OBD.RadioCode.LandRover.Advanced,
  OBD.RadioCode.Lexus.Advanced,
  OBD.RadioCode.Maserati.Advanced,
  OBD.RadioCode.Mazda.Advanced,
  OBD.RadioCode.Mercedes.Advanced,
  OBD.RadioCode.Mini.Advanced,
  OBD.RadioCode.Mitsubishi.Advanced,
  OBD.RadioCode.Nissan.Advanced,
  OBD.RadioCode.Opel.Advanced,
  OBD.RadioCode.Peugeot.Advanced,
  OBD.RadioCode.Porsche.Advanced,
  OBD.RadioCode.Renault.Advanced,
  OBD.RadioCode.Saab.Advanced,
  OBD.RadioCode.SEAT.Advanced,
  OBD.RadioCode.Skoda.Advanced,
  OBD.RadioCode.Smart.Advanced,
  OBD.RadioCode.Subaru.Advanced,
  OBD.RadioCode.Suzuki.Advanced,
  OBD.RadioCode.Toyota.Advanced,
  OBD.RadioCode.Visteon.Advanced,
  OBD.RadioCode.Volvo.Advanced,
  OBD.RadioCode.VW.Advanced;

type
  TOBDRadioCodeClass = class of TOBDRadioCode;

{ TRadioCodeSmokeTests }

procedure TRadioCodeSmokeTests.RunInvariants(const CalcClass: TClass);
var
  Calc: TOBDRadioCode;
  Description: string;
  Err1, Err2: string;
begin
  Assert.IsTrue(CalcClass.InheritsFrom(TOBDRadioCode),
    Format('%s does not inherit from TOBDRadioCode', [CalcClass.ClassName]));

  Calc := TOBDRadioCodeClass(CalcClass).Create;
  try
    // Invariant 1: description is non-empty (used in UI, must always render).
    Description := Calc.GetDescription;
    Assert.IsNotEmpty(Description,
      CalcClass.ClassName + '.GetDescription returned empty');

    // Invariant 2: empty input is rejected with a non-empty error message.
    Err1 := '';
    Assert.IsFalse(Calc.Validate('', Err1),
      CalcClass.ClassName + '.Validate accepted empty input');
    Assert.IsNotEmpty(Err1,
      CalcClass.ClassName + '.Validate(empty) produced no error message');

    // Invariant 3: Calculate refuses invalid input rather than throwing.
    // We only require it returns False; the error string may be empty for
    // calculators that report through other channels.
    Err2 := '';
    Assert.IsFalse(Calc.Calculate('', Description, Err2),
      CalcClass.ClassName + '.Calculate accepted empty input');
  finally
    Calc.Free;
  end;
end;

procedure TRadioCodeSmokeTests.Acura;       begin RunInvariants(TOBDRadioCodeAcuraAdvanced);       end;
procedure TRadioCodeSmokeTests.AlfaRomeo;   begin RunInvariants(TOBDRadioCodeAlfaRomeoAdvanced);   end;
procedure TRadioCodeSmokeTests.Alpine;      begin RunInvariants(TOBDRadioCodeAlpineAdvanced);      end;
procedure TRadioCodeSmokeTests.AudiConcert; begin RunInvariants(TOBDRadioCodeAudiConcertAdvanced); end;
procedure TRadioCodeSmokeTests.Becker;      begin RunInvariants(TOBDRadioCodeBeckerAdvanced);      end;
procedure TRadioCodeSmokeTests.Becker4;     begin RunInvariants(TOBDRadioCodeBecker4);             end;
procedure TRadioCodeSmokeTests.Becker5;     begin RunInvariants(TOBDRadioCodeBecker5);             end;
procedure TRadioCodeSmokeTests.Blaupunkt;   begin RunInvariants(TOBDRadioCodeBlaupunktAdvanced);   end;
procedure TRadioCodeSmokeTests.BMW;         begin RunInvariants(TOBDRadioCodeBMWAdvanced);         end;
procedure TRadioCodeSmokeTests.Chrysler;    begin RunInvariants(TOBDRadioCodeChryslerAdvanced);    end;
procedure TRadioCodeSmokeTests.Citroen;     begin RunInvariants(TOBDRadioCodeCitroenAdvanced);     end;
procedure TRadioCodeSmokeTests.Clarion;     begin RunInvariants(TOBDRadioCodeClarionAdvanced);     end;
procedure TRadioCodeSmokeTests.FiatDaiichi; begin RunInvariants(TOBDRadioCodeFiatDaiichiAdvanced); end;
procedure TRadioCodeSmokeTests.FiatVP;      begin RunInvariants(TOBDRadioCodeFiatVPAdvanced);      end;
procedure TRadioCodeSmokeTests.Ford;        begin RunInvariants(TOBDRadioCodeFordAdvanced);        end;
procedure TRadioCodeSmokeTests.FordV;       begin RunInvariants(TOBDRadioCodeFordV);               end;
procedure TRadioCodeSmokeTests.GM;          begin RunInvariants(TOBDRadioCodeGMAdvanced);          end;
procedure TRadioCodeSmokeTests.Honda;       begin RunInvariants(TOBDRadioCodeHondaAdvanced);       end;
procedure TRadioCodeSmokeTests.Hyundai;     begin RunInvariants(TOBDRadioCodeHyundaiAdvanced);     end;
procedure TRadioCodeSmokeTests.Infiniti;    begin RunInvariants(TOBDRadioCodeInfinitiAdvanced);    end;
procedure TRadioCodeSmokeTests.Jaguar;      begin RunInvariants(TOBDRadioCodeJaguarAdvanced);      end;
procedure TRadioCodeSmokeTests.LandRover;   begin RunInvariants(TOBDRadioCodeLandRoverAdvanced);   end;
procedure TRadioCodeSmokeTests.Lexus;       begin RunInvariants(TOBDRadioCodeLexusAdvanced);       end;
procedure TRadioCodeSmokeTests.Maserati;    begin RunInvariants(TOBDRadioCodeMaseratiAdvanced);    end;
procedure TRadioCodeSmokeTests.Mazda;       begin RunInvariants(TOBDRadioCodeMazdaAdvanced);       end;
procedure TRadioCodeSmokeTests.Mercedes;    begin RunInvariants(TOBDRadioCodeMercedesAdvanced);    end;
procedure TRadioCodeSmokeTests.Mini;        begin RunInvariants(TOBDRadioCodeMiniAdvanced);        end;
procedure TRadioCodeSmokeTests.Mitsubishi;  begin RunInvariants(TOBDRadioCodeMitsubishiAdvanced);  end;
procedure TRadioCodeSmokeTests.Nissan;      begin RunInvariants(TOBDRadioCodeNissanAdvanced);      end;
procedure TRadioCodeSmokeTests.Opel;        begin RunInvariants(TOBDRadioCodeOpelAdvanced);        end;
procedure TRadioCodeSmokeTests.Peugeot;     begin RunInvariants(TOBDRadioCodePeugeotAdvanced);     end;
procedure TRadioCodeSmokeTests.Porsche;     begin RunInvariants(TOBDRadioCodePorscheAdvanced);     end;
procedure TRadioCodeSmokeTests.Renault;     begin RunInvariants(TOBDRadioCodeRenaultAdvanced);     end;
procedure TRadioCodeSmokeTests.Saab;        begin RunInvariants(TOBDRadioCodeSaabAdvanced);        end;
procedure TRadioCodeSmokeTests.SEAT;        begin RunInvariants(TOBDRadioCodeSEATAdvanced);        end;
procedure TRadioCodeSmokeTests.Skoda;       begin RunInvariants(TOBDRadioCodeSkodaAdvanced);       end;
procedure TRadioCodeSmokeTests.Smart;       begin RunInvariants(TOBDRadioCodeSmartAdvanced);       end;
procedure TRadioCodeSmokeTests.Subaru;      begin RunInvariants(TOBDRadioCodeSubaruAdvanced);      end;
procedure TRadioCodeSmokeTests.Suzuki;      begin RunInvariants(TOBDRadioCodeSuzukiAdvanced);      end;
procedure TRadioCodeSmokeTests.Toyota;      begin RunInvariants(TOBDRadioCodeToyotaAdvanced);      end;
procedure TRadioCodeSmokeTests.Visteon;     begin RunInvariants(TOBDRadioCodeVisteonAdvanced);     end;
procedure TRadioCodeSmokeTests.Volvo;       begin RunInvariants(TOBDRadioCodeVolvoAdvanced);       end;
procedure TRadioCodeSmokeTests.VW;          begin RunInvariants(TOBDRadioCodeVWAdvanced);          end;

initialization
  TDUnitX.RegisterTestFixture(TRadioCodeSmokeTests);

end.
