// ------------------------------------------------------------------------------
// ERD.OEM
//
// Umbrella unit that publishes the full OEM-extension surface
// (records, enums, the <see cref="IOBDOEMExtension"/> contract,
// the convenience base class and the vendor registry) under a
// single import. A vendor unit can keep its imports compact:
//
// uses {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, ERD.OEM, ERD.OEM.Session,
// ERD.OEM.SeedKey, ERD.OEM.DTC;
//
// The detailed declarations live in <see cref="ERD.OEM.Types"/>
// (records / enums) and <see cref="ERD.OEM.Extensions"/> (the
// contract, base class and registry). This unit re-exports them
// by aliasing.
//
// Note on the registry name. <see cref="TOBDOEMRegistry"/> is
// the vendor registry that <c>RegisterExtension</c> targets. The
// unrelated runtime overlay resolver of the same nominal role
// lives in <c>ERD.OEM.Registry</c> as
// <c>TOBDOEMOverlayRegistry</c>; do not confuse the two.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-05-12  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.OEM;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
  ERD.OEM.Types,
  ERD.OEM.Extensions;

type
  /// <summary>Extension contract — see
  /// <see cref="ERD.OEM.Extensions"/>.</summary>
  IOBDOEMExtension = ERD.OEM.Extensions.IOBDOEMExtension;
  /// <summary>Convenience base class — see
  /// <see cref="ERD.OEM.Extensions"/>.</summary>
  TOBDOEMExtensionBase = ERD.OEM.Extensions.TOBDOEMExtensionBase;
  /// <summary>Vendor registry. Hosts call
  /// <c>TOBDOEMRegistry.RegisterExtension</c> from each
  /// extension unit's <c>initialization</c> section.</summary>
  TOBDOEMRegistry = ERD.OEM.Extensions.TOBDOEMExtensionRegistry;

  /// <summary>DID record — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMDataIdentifier = ERD.OEM.Types.TOBDOEMDataIdentifier;
  /// <summary>Routine record — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMRoutine = ERD.OEM.Types.TOBDOEMRoutine;
  /// <summary>ECU descriptor — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMECU = ERD.OEM.Types.TOBDOEMECU;
  /// <summary>Coding block — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMCodingBlock = ERD.OEM.Types.TOBDOEMCodingBlock;
  /// <summary>Adaptation channel — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMAdaptation = ERD.OEM.Types.TOBDOEMAdaptation;
  /// <summary>Actuator test descriptor — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMActuatorTest = ERD.OEM.Types.TOBDOEMActuatorTest;
  /// <summary>Live PID descriptor — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMLivePID = ERD.OEM.Types.TOBDOEMLivePID;
  /// <summary>DTC extended-data record — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDDtcExtendedDataRecord = ERD.OEM.Types.TOBDDtcExtendedDataRecord;
  /// <summary>Decoder kind — see
  /// <see cref="ERD.OEM.Types"/>.</summary>
  TOBDOEMDecoderKind = ERD.OEM.Types.TOBDOEMDecoderKind;

implementation

end.
