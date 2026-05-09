//------------------------------------------------------------------------------
// UNIT           : OBD.RadioCode.Pending.pas
// CONTENTS       : Eight brand stubs that satisfy IOBDRadioCode but raise
//                : EOBDRadioCodeDataMissing on Calculate. Each brand has a
//                : registry entry with a precise data-gap description so a
//                : maintainer with reference data can replace the stub
//                : without touching call sites.
//
// AFFECTED BRANDS:
//   Pioneer, Kenwood, JVC, Sony, Philips, Grundig, Panasonic, Continental/VDO
//
// WHY STUBS     : Public web research conducted 2026-05-09 confirmed that no
//                : freely available algorithm or lookup table exists for any
//                : of these brands. Commercial unlock services rely on
//                : licensed databases (Philips: ~14M-entry DB) or EEPROM
//                : extraction. Existing Becker4/Becker5 lookup tables
//                : (10,000 entries each) are exceptions for a specific
//                : older product line. See docs/DATA_GAPS.md for the precise
//                : reference data each brand needs.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
//------------------------------------------------------------------------------
unit OBD.RadioCode.Pending;

interface

uses
  System.SysUtils,

  OBD.RadioCode, OBD.RadioCode.Registry, OBD.RadioCode.Variants;

type
  /// <summary>Common base for data-pending calculator stubs. Validate
  /// returns False with a clear message; Calculate raises
  /// EOBDRadioCodeDataMissing.</summary>
  TOBDRadioCodePending = class(TOBDRadioCode)
  private
    FBrandKey: string;
    FDisplayName: string;
    FDataNotes: string;
  public
    constructor Create(const BrandKey, DisplayName, DataNotes: string);
    function GetDescription: string; override;
    function Validate(const Input: string; var ErrorMessage: string): Boolean; override;
    function Calculate(const Input: string; var Output: string;
      var ErrorMessage: string): Boolean; override;
  end;

implementation

{ TOBDRadioCodePending }

constructor TOBDRadioCodePending.Create(const BrandKey, DisplayName,
  DataNotes: string);
begin
  inherited Create;
  FBrandKey := BrandKey;
  FDisplayName := DisplayName;
  FDataNotes := DataNotes;
end;

function TOBDRadioCodePending.GetDescription: string;
begin
  Result := Format(
    '%s radio-code calculator (DATA-PENDING — algorithm or database not available in this build). %s',
    [FDisplayName, FDataNotes]);
end;

function TOBDRadioCodePending.Validate(const Input: string;
  var ErrorMessage: string): Boolean;
begin
  Result := False;
  ErrorMessage := Format(
    '%s calculator is not yet operational. %s',
    [FDisplayName, FDataNotes]);
end;

function TOBDRadioCodePending.Calculate(const Input: string;
  var Output: string; var ErrorMessage: string): Boolean;
begin
  Output := '';
  ErrorMessage := Format(
    '%s calculator is data-pending: %s',
    [FDisplayName, FDataNotes]);
  raise EOBDRadioCodeDataMissing.Create(ErrorMessage);
end;

//------------------------------------------------------------------------------
// REGISTRATION
//------------------------------------------------------------------------------
type
  TPendingFactory = record
    Key, Name, Notes: string;
  end;

const
  PendingFactories: array[0..7] of TPendingFactory = (
    (Key: 'pioneer';
     Name: 'Pioneer';
     Notes: 'Needs verified serial-to-code algorithm or lookup table for at least DEH/AVH/MVH model families. Commercial DBs cover ~30M units; community-published algorithms are partial and generation-specific.'),
    (Key: 'kenwood';
     Name: 'Kenwood';
     Notes: 'Needs verified algorithm or lookup table for KDC/DDX/DNX/KMM model families. After the 2008 JVC-Kenwood merger some platforms share supply chain with JVC; an algorithm covering one may apply to the other.'),
    (Key: 'jvc';
     Name: 'JVC';
     Notes: 'Needs verified algorithm or lookup table for KD/KW model families. Post-2008 platforms may share with Kenwood.'),
    (Key: 'sony';
     Name: 'Sony';
     Notes: 'Needs verified algorithm or lookup table for CDX/WX/MEX after-market head units. Modern Sony OEM fitments are tied to VIN via the gateway and out of scope.'),
    (Key: 'philips';
     Name: 'Philips';
     Notes: 'Needs the licensed serial-to-code database (Philips ships ~14M entries). EEPROM-extraction route is hardware-side and not implementable here.'),
    (Key: 'grundig';
     Name: 'Grundig';
     Notes: 'Pre-2000 European OEM head units (WKC/EC series). Possibly recoverable from a specific generation via the same approach used for Becker4/Becker5; needs a leaked/published table.'),
    (Key: 'panasonic';
     Name: 'Panasonic (Matsushita)';
     Notes: 'Needs CQ-series algorithm or lookup table; per-region variants common.'),
    (Key: 'continental_vdo';
     Name: 'Continental / VDO';
     Notes: 'OEM head-unit supplier in VW / Mercedes / Ford. Often re-uses VAG variants but the specific mapping per part number is undocumented publicly.')
  );

function MakePendingFactory(const Key, Name, Notes: string): TOBDRadioCodeFactory;
begin
  // Wrapping in a separate function captures parameters per-call rather
  // than per-loop-iteration; necessary because Delphi anonymous methods
  // capture enclosing variables by reference.
  Result := function: IOBDRadioCode
    begin
      Result := TOBDRadioCodePending.Create(Key, Name, Notes);
    end;
end;

procedure RegisterPendingBrands;
var
  P: TPendingFactory;
begin
  for P in PendingFactories do
    TOBDRadioCodeRegistry.Instance.Register(
      TOBDRadioCodeBrand.Create(
        P.Key, P.Name, False, P.Notes,
        MakePendingFactory(P.Key, P.Name, P.Notes)));
end;

initialization
  RegisterPendingBrands;

end.
