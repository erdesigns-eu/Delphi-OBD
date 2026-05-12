//------------------------------------------------------------------------------
//  OBD.OEM.Helpers
//
//  Constructor helpers for OEM catalogue records. Tiny convenience
//  wrappers so vendor extensions can build their catalogues as
//  array literals — e.g.
//    DIDs := [DID($1234, 'name', 'desc'),
//             DID($5678, 'name2', 'desc2', $7E0)];
//  rather than two-line record-literal blocks.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM.Helpers;

interface

uses
  OBD.OEM.Types;

/// <summary>Builds a global DID catalogue entry.</summary>
/// <param name="ADID">DID value.</param>
/// <param name="AName">Short snake_case key.</param>
/// <param name="ADescription">Human-readable description.</param>
function DID(const ADID: Word;
  const AName, ADescription: string): TOBDOEMDataIdentifier; overload;
/// <summary>Builds an ECU-scoped DID catalogue entry.</summary>
/// <param name="ADID">DID value.</param>
/// <param name="AName">Short snake_case key.</param>
/// <param name="ADescription">Human-readable description.</param>
/// <param name="AEcuAddress">Owning ECU CAN-ID.</param>
function DID(const ADID: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMDataIdentifier; overload;

/// <summary>Builds a global routine catalogue entry.</summary>
/// <param name="AIdentifier">Routine ID.</param>
/// <param name="AName">Short snake_case key.</param>
/// <param name="ADescription">Human-readable description.</param>
function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine; overload;
/// <summary>Builds an ECU-scoped routine catalogue entry.</summary>
/// <param name="AIdentifier">Routine ID.</param>
/// <param name="AName">Short snake_case key.</param>
/// <param name="ADescription">Human-readable description.</param>
/// <param name="AEcuAddress">Owning ECU CAN-ID.</param>
function Routine(const AIdentifier: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMRoutine; overload;

/// <summary>Builds an ECU bus-map entry.</summary>
/// <param name="AAddress">CAN-ID.</param>
/// <param name="AName">Short snake_case key.</param>
/// <param name="ACommonName">Display label.</param>
function ECU(const AAddress: Word;
  const AName, ACommonName: string): TOBDOEMECU;

implementation

function DID(const ADID: Word;
  const AName, ADescription: string): TOBDOEMDataIdentifier;
begin
  Result := Default(TOBDOEMDataIdentifier);
  Result.DID := ADID;
  Result.Name := AName;
  Result.Description := ADescription;
end;

function DID(const ADID: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMDataIdentifier;
begin
  Result := DID(ADID, AName, ADescription);
  Result.EcuAddress := AEcuAddress;
end;

function Routine(const AIdentifier: Word;
  const AName, ADescription: string): TOBDOEMRoutine;
begin
  Result := Default(TOBDOEMRoutine);
  Result.Identifier := AIdentifier;
  Result.Name := AName;
  Result.Description := ADescription;
end;

function Routine(const AIdentifier: Word;
  const AName, ADescription: string;
  const AEcuAddress: Word): TOBDOEMRoutine;
begin
  Result := Routine(AIdentifier, AName, ADescription);
  Result.EcuAddress := AEcuAddress;
end;

function ECU(const AAddress: Word;
  const AName, ACommonName: string): TOBDOEMECU;
begin
  Result.Address := AAddress;
  Result.Name := AName;
  Result.CommonName := ACommonName;
end;

end.
