//------------------------------------------------------------------------------
// UNIT           : OBD.OEM.Helpers.pas
// CONTENTS       : Constructor helpers for OEM catalog records
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Tiny convenience wrappers so OEM extensions can build
//                  their catalogs as `[DID($1234, 'name', 'desc'), …]`
//                  rather than two-line record literals.
//------------------------------------------------------------------------------
unit OBD.OEM.Helpers;

interface

uses
  OBD.OEM;

function DID(const ADID: Word;
  const AName, ADescription: string): TOBDOEMDataIdentifier;

function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine;

implementation

function DID(const ADID: Word;
  const AName, ADescription: string): TOBDOEMDataIdentifier;
begin
  Result.DID := ADID;
  Result.Name := AName;
  Result.Description := ADescription;
end;

function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine;
begin
  Result.Identifier := AIdentifier;
  Result.Name := AName;
  Result.Description := ADescription;
end;

end.
