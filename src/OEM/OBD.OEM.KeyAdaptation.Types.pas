//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.Types
//
//  Value types shared across the per-vendor key-adaptation
//  components (P-A6).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.Types;

interface

uses
  System.SysUtils;

type
  /// <summary>One destructive operation a key-adaptation
  /// component can perform. Every op except <c>kaoListSlots</c>
  /// changes ECU state; <c>OnConfirmExecute</c> must return
  /// True before any of them run.</summary>
  TOBDKeyAdaptOp = (
    kaoListSlots,           // read which key slots are filled
    kaoAddKey,              // program a new transponder
    kaoClearOneSlot,        // clear a single named slot
    kaoClearAllKeys,        // wipe every slot
    kaoCheckPin             // validate the supplied PIN/CS
  );

  /// <summary>One key-slot summary returned by ListSlots.</summary>
  TOBDKeySlot = record
    Index:    Byte;
    Filled:   Boolean;
    /// <summary>Free-text label / fingerprint reported by the
    /// immobiliser (vendor-specific). Empty when the ECU
    /// doesn't expose a per-slot ID.</summary>
    Label_:   string;
  end;

  /// <summary>Outcome record returned by every public method
  /// on the per-vendor components.</summary>
  TOBDKeyAdaptResult = record
    Op:        TOBDKeyAdaptOp;
    Success:   Boolean;
    /// <summary>Slot index touched by an Add / Clear op.
    /// Meaningless for ListSlots / CheckPin.</summary>
    SlotIndex: Byte;
    /// <summary>Free-text reason (NRC text, validation
    /// failure, host-rejected). Empty on success.</summary>
    Message:   string;
    /// <summary>Populated by ListSlots; empty otherwise.</summary>
    Slots:     TArray<TOBDKeySlot>;
  end;

  /// <summary>Confirm-execute callback: the host MUST return
  /// True for the destructive op to proceed. Implementations
  /// that go ahead without an installed handler raise
  /// <c>EOBDConfig</c>.</summary>
  TOBDKeyAdaptConfirmEvent = procedure(Sender: TObject;
    AOp: TOBDKeyAdaptOp; ASlotIndex: Byte;
    var ACanProceed: Boolean) of object;

  /// <summary>Fired once an op completes (success or failure).</summary>
  TOBDKeyAdaptResultEvent = procedure(Sender: TObject;
    const AResult: TOBDKeyAdaptResult) of object;

function OpName(AOp: TOBDKeyAdaptOp): string;

implementation

function OpName(AOp: TOBDKeyAdaptOp): string;
begin
  case AOp of
    kaoListSlots:     Result := 'ListSlots';
    kaoAddKey:        Result := 'AddKey';
    kaoClearOneSlot:  Result := 'ClearOneSlot';
    kaoClearAllKeys:  Result := 'ClearAllKeys';
    kaoCheckPin:      Result := 'CheckPin';
  else                Result := 'Unknown';
  end;
end;

end.
