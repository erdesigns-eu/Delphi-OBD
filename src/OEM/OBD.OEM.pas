//------------------------------------------------------------------------------
//  OBD.OEM
//
//  Umbrella unit that publishes the full OEM-extension surface
//  (records, enums, the <see cref="IOBDOEMExtension"/> contract,
//  the convenience base class and the vendor registry) under a
//  single import. A vendor unit can keep its imports compact:
//
//    uses System.SysUtils, OBD.OEM, OBD.OEM.Session,
//      OBD.OEM.SeedKey, OBD.OEM.DTC;
//
//  The detailed declarations live in <see cref="OBD.OEM.Types"/>
//  (records / enums) and <see cref="OBD.OEM.Extensions"/> (the
//  contract, base class and registry). This unit re-exports them
//  by aliasing.
//
//  Note on the registry name. <see cref="TOBDOEMRegistry"/> is
//  the vendor registry that <c>RegisterExtension</c> targets. The
//  unrelated runtime overlay resolver of the same nominal role
//  lives in <c>OBD.OEM.Registry</c> as
//  <c>TOBDOEMOverlayRegistry</c>; do not confuse the two.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.OEM;

interface

uses
  OBD.OEM.Types,
  OBD.OEM.Extensions;

type
  /// <summary>Extension contract — see
  /// <see cref="OBD.OEM.Extensions"/>.</summary>
  IOBDOEMExtension = OBD.OEM.Extensions.IOBDOEMExtension;
  /// <summary>Convenience base class — see
  /// <see cref="OBD.OEM.Extensions"/>.</summary>
  TOBDOEMExtensionBase = OBD.OEM.Extensions.TOBDOEMExtensionBase;
  /// <summary>Vendor registry. Hosts call
  /// <c>TOBDOEMRegistry.RegisterExtension</c> from each
  /// extension unit's <c>initialization</c> section.</summary>
  TOBDOEMRegistry = OBD.OEM.Extensions.TOBDOEMExtensionRegistry;

  /// <summary>DID record — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMDataIdentifier = OBD.OEM.Types.TOBDOEMDataIdentifier;
  /// <summary>Routine record — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMRoutine = OBD.OEM.Types.TOBDOEMRoutine;
  /// <summary>ECU descriptor — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMECU = OBD.OEM.Types.TOBDOEMECU;
  /// <summary>Coding block — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMCodingBlock = OBD.OEM.Types.TOBDOEMCodingBlock;
  /// <summary>Adaptation channel — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMAdaptation = OBD.OEM.Types.TOBDOEMAdaptation;
  /// <summary>Actuator test descriptor — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMActuatorTest = OBD.OEM.Types.TOBDOEMActuatorTest;
  /// <summary>Live PID descriptor — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMLivePID = OBD.OEM.Types.TOBDOEMLivePID;
  /// <summary>DTC extended-data record — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDDtcExtendedDataRecord = OBD.OEM.Types.TOBDDtcExtendedDataRecord;
  /// <summary>Decoder kind — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMDecoderKind = OBD.OEM.Types.TOBDOEMDecoderKind;

implementation

end.
