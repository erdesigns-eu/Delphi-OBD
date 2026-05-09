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
  const AName, ADescription: string): TOBDOEMDataIdentifier; overload;
function DID(const ADID: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMDataIdentifier; overload;

function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine; overload;
function Routine(const AIdentifier: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMRoutine; overload;

function ECU(const AAddress: Word;
  const AName, ACommonName: string): TOBDOEMECU;

implementation

//------------------------------------------------------------------------------
// DID
//------------------------------------------------------------------------------
function DID(const ADID: Word;
  const AName, ADescription: string): TOBDOEMDataIdentifier;
begin
  Result := Default(TOBDOEMDataIdentifier);
  Result.DID := ADID;
  Result.Name := AName;
  Result.Description := ADescription;
end;

//------------------------------------------------------------------------------
// DID
//------------------------------------------------------------------------------
function DID(const ADID: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMDataIdentifier;
begin
  Result := DID(ADID, AName, ADescription);
  Result.EcuAddress := AEcuAddress;
end;

//------------------------------------------------------------------------------
// ROUTINE
//------------------------------------------------------------------------------
function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine;
begin
  Result := Default(TOBDOEMRoutine);
  Result.Identifier := AIdentifier;
  Result.Name := AName;
  Result.Description := ADescription;
end;

//------------------------------------------------------------------------------
// ROUTINE
//------------------------------------------------------------------------------
function Routine(const AIdentifier: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMRoutine;
begin
  Result := Routine(AIdentifier, AName, ADescription);
  Result.EcuAddress := AEcuAddress;
end;

//------------------------------------------------------------------------------
// ECU
//------------------------------------------------------------------------------
function ECU(const AAddress: Word;
  const AName, ACommonName: string): TOBDOEMECU;
begin
  Result.Address := AAddress;
  Result.Name := AName;
  Result.CommonName := ACommonName;
end;

end.
