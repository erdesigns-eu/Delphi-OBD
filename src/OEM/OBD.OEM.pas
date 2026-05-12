//------------------------------------------------------------------------------
//  OBD.OEM
//
//  Compatibility umbrella unit. Re-exports the v2 OEM types and
//  the extension registry under the names v1 vendor extensions
//  expect, so a vendor unit can keep its single
//  <c>uses ..., OBD.OEM, OBD.OEM.Session, OBD.OEM.SeedKey,
//  OBD.OEM.DTC;</c> import line unchanged.
//
//  v2 split the v1 monolithic <c>OBD.OEM</c> into
//  <see cref="OBD.OEM.Types"/> (records / enums) and
//  <see cref="OBD.OEM.Extensions"/> (interface, base class,
//  registry). The split is the right architecture for v2 but
//  would force every vendor file to fan its uses clause out;
//  this alias unit is a one-line shim that avoids that.
//
//  Renamed in v2:
//    v1 <c>TOBDOEMRegistry</c> (vendor registry) →
//    v2 <c>TOBDOEMExtensionRegistry</c> (the v2
//    <c>TOBDOEMRegistry</c> in OBD.OEM.Registry is a different
//    concept — the runtime overlay resolver). The alias here
//    points at the vendor registry so v1 vendor code that calls
//    <c>TOBDOEMRegistry.RegisterExtension(...)</c> continues
//    to compile.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-12  ERD  Compatibility unit added for vendor port.
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
  /// <summary>Vendor registry — see
  /// <see cref="OBD.OEM.Extensions.TOBDOEMExtensionRegistry"/>.
  /// The unrelated v2 <c>TOBDOEMRegistry</c> (overlay resolver)
  /// lives in <c>OBD.OEM.Registry</c>.</summary>
  TOBDOEMRegistry = OBD.OEM.Extensions.TOBDOEMExtensionRegistry;

  /// <summary>DID record — see
  /// <see cref="OBD.OEM.Types"/>.</summary>
  TOBDOEMDataIdentifier = OBD.OEM.Types.TOBDOEMDataIdentifier;
  TOBDOEMRoutine = OBD.OEM.Types.TOBDOEMRoutine;
  TOBDOEMECU = OBD.OEM.Types.TOBDOEMECU;
  TOBDOEMCodingBlock = OBD.OEM.Types.TOBDOEMCodingBlock;
  TOBDOEMAdaptation = OBD.OEM.Types.TOBDOEMAdaptation;
  TOBDOEMActuatorTest = OBD.OEM.Types.TOBDOEMActuatorTest;
  TOBDOEMLivePID = OBD.OEM.Types.TOBDOEMLivePID;
  TOBDDtcExtendedDataRecord = OBD.OEM.Types.TOBDDtcExtendedDataRecord;
  TOBDOEMDecoderKind = OBD.OEM.Types.TOBDOEMDecoderKind;

implementation

end.
