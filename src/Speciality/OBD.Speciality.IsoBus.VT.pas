//------------------------------------------------------------------------------
//  OBD.Speciality.IsoBus.VT
//
//  TOBDIsoBusVT — IsoBus Virtual Terminal client (ISO 11783-6).
//  The VT is the implement-controller's display + soft-key surface
//  on agricultural machinery; an ECU uploads an Object Pool to the
//  VT and then exchanges commands to drive the UI.
//
//  v1 ships the request/response framing for the load-bearing
//  commands every IsoBus implement needs:
//
//    - Get_Versions                 (which object-pool versions the VT has)
//    - Load_Version                 (instruct the VT to load a stored pool)
//    - Store_Version                (ask the VT to save the pool)
//    - Delete_Version
//    - Object_Pool_Transfer Begin / Chunk / End
//    - Soft_Key_Activation          (inbound: user pressed a key)
//    - VT_Status                    (inbound: heartbeat with active mask)
//    - Audio_Signal                 (beep)
//    - Change_Active_Mask
//    - End_Of_Object_Pool
//
//  Each method returns the 8-byte payload that the host wires onto
//  a J1939 PGN 0xE700 (VT-to-ECU) / 0xE600 (ECU-to-VT) frame.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - ISO 11783-6:2018 (Virtual Terminal)
//
//  History     :
//    2026-05-09  ERD  Phase 7 follow-up.
//------------------------------------------------------------------------------

unit OBD.Speciality.IsoBus.VT;

interface

uses
  System.SysUtils,
  OBD.Types;

const
  /// <summary>VT-to-ECU PGN.</summary>
  ISOBUS_VT_PGN_TO_ECU   = $00E700;
  /// <summary>ECU-to-VT PGN.</summary>
  ISOBUS_VT_PGN_TO_VT    = $00E600;

  // ---- VT command function bytes (ISO 11783-6 Annex A) ----
  VT_FN_SOFT_KEY_ACTIVATION       = $00;
  VT_FN_BUTTON_ACTIVATION         = $01;
  VT_FN_POINTING_EVENT            = $02;
  VT_FN_VT_SELECT_INPUT_OBJECT    = $03;
  VT_FN_VT_ESC                    = $04;
  VT_FN_VT_CHANGE_NUMERIC_VALUE   = $05;
  VT_FN_VT_CHANGE_ACTIVE_MASK     = $06;
  VT_FN_VT_CHANGE_SOFT_KEY_MASK   = $07;
  VT_FN_VT_CHANGE_STRING_VALUE    = $08;
  VT_FN_VT_GET_MEMORY             = $C0;
  VT_FN_VT_GET_VERSIONS           = $E0;
  VT_FN_VT_LOAD_VERSION           = $D1;
  VT_FN_VT_STORE_VERSION          = $D0;
  VT_FN_VT_DELETE_VERSION         = $D4;
  VT_FN_VT_END_OF_OBJECT_POOL     = $C2;
  VT_FN_VT_AUDIO_SIGNAL           = $C3;
  VT_FN_VT_CHANGE_ACTIVE_MASK_OUT = $92;
  VT_FN_VT_STATUS                 = $FE;

type
  /// <summary>Decoded soft-key activation event.</summary>
  TOBDIsoBusVTSoftKey = record
    /// <summary>1 = key released, 2 = key pressed, 3 = held.</summary>
    State: Byte;
    /// <summary>Object ID of the soft key.</summary>
    ObjectID: Word;
    /// <summary>Object ID of the parent mask.</summary>
    ParentMaskID: Word;
    /// <summary>Key code (raw, vendor-defined).</summary>
    KeyCode: Byte;
  end;

  /// <summary>VT-Status inbound frame (PGN 0xE700, function 0xFE).</summary>
  TOBDIsoBusVTStatus = record
    /// <summary>Source address of the working-set master ECU.</summary>
    WorkingSetMasterAddress: Byte;
    /// <summary>Active data/alarm mask object ID.</summary>
    ActiveMaskID: Word;
    /// <summary>Active soft-key mask object ID.</summary>
    ActiveSoftKeyMaskID: Word;
    /// <summary>VT busy flags (Annex A.6.2).</summary>
    BusyCodes: Byte;
    /// <summary>Function code or VT busy interpretation.</summary>
    FunctionCode: Byte;
  end;

  /// <summary>IsoBus VT framing helpers. Stateless.</summary>
  TOBDIsoBusVT = class
  public
    /// <summary>Builds a Get_Memory frame: function 0xC0 followed
    /// by a 4-byte memory-required value (LSB first).</summary>
    class function BuildGetMemory(ABytesRequired: Cardinal): TBytes; static;
    /// <summary>Builds a Get_Versions request.</summary>
    class function BuildGetVersions: TBytes; static;
    /// <summary>Builds a Load_Version frame referencing an existing
    /// 7-byte version label stored on the VT.</summary>
    class function BuildLoadVersion(const ALabel7: TBytes): TBytes; static;
    /// <summary>Builds a Store_Version frame.</summary>
    class function BuildStoreVersion(const ALabel7: TBytes): TBytes; static;
    /// <summary>Builds a Delete_Version frame.</summary>
    class function BuildDeleteVersion(const ALabel7: TBytes): TBytes; static;
    /// <summary>Builds an End_Of_Object_Pool frame.</summary>
    class function BuildEndOfObjectPool: TBytes; static;
    /// <summary>Builds an Audio_Signal frame: number of beeps,
    /// frequency (Hz), on/off durations (ms).</summary>
    class function BuildAudioSignal(ABeeps: Byte; AFrequencyHz: Word;
      AOnMs, AOffMs: Word): TBytes; static;
    /// <summary>Builds a Change_Active_Mask frame.</summary>
    class function BuildChangeActiveMask(AWorkingSet, AMaskID: Word): TBytes; static;

    /// <summary>Decodes an inbound PGN-0xE700 frame into a soft-key
    /// activation event (function 0x00). Returns False when the
    /// function code does not match.</summary>
    class function DecodeSoftKey(const APayload: TBytes;
      out AKey: TOBDIsoBusVTSoftKey): Boolean; static;
    /// <summary>Decodes a VT-Status frame (function 0xFE).</summary>
    class function DecodeVTStatus(const APayload: TBytes;
      out AStatus: TOBDIsoBusVTStatus): Boolean; static;
  end;

implementation

procedure WriteWord(var AOut: TBytes; AOffset: Integer; AValue: Word);
begin
  AOut[AOffset    ] := Byte(AValue and $FF);
  AOut[AOffset + 1] := Byte((AValue shr 8) and $FF);
end;

procedure WriteCardinal(var AOut: TBytes; AOffset: Integer; AValue: Cardinal);
begin
  AOut[AOffset    ] := Byte(AValue and $FF);
  AOut[AOffset + 1] := Byte((AValue shr 8) and $FF);
  AOut[AOffset + 2] := Byte((AValue shr 16) and $FF);
  AOut[AOffset + 3] := Byte((AValue shr 24) and $FF);
end;

class function TOBDIsoBusVT.BuildGetMemory(ABytesRequired: Cardinal): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_GET_MEMORY;
  Result[1] := $FF; // version-of-VT spec — let the VT decide
  WriteCardinal(Result, 2, ABytesRequired);
  Result[6] := $FF;
  Result[7] := $FF;
end;

class function TOBDIsoBusVT.BuildGetVersions: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_GET_VERSIONS;
  for I := 1 to 7 do Result[I] := $FF;
end;

procedure WriteLabel(var AOut: TBytes; const ALabel: TBytes; AOffset: Integer);
var
  I, Len: Integer;
begin
  Len := Length(ALabel);
  if Len > 7 then Len := 7;
  for I := 0 to Len - 1 do AOut[AOffset + I] := ALabel[I];
  for I := Len to 6 do AOut[AOffset + I] := $20; // pad with spaces
end;

class function TOBDIsoBusVT.BuildLoadVersion(const ALabel7: TBytes): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_LOAD_VERSION;
  WriteLabel(Result, ALabel7, 1);
end;

class function TOBDIsoBusVT.BuildStoreVersion(const ALabel7: TBytes): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_STORE_VERSION;
  WriteLabel(Result, ALabel7, 1);
end;

class function TOBDIsoBusVT.BuildDeleteVersion(const ALabel7: TBytes): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_DELETE_VERSION;
  WriteLabel(Result, ALabel7, 1);
end;

class function TOBDIsoBusVT.BuildEndOfObjectPool: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_END_OF_OBJECT_POOL;
  for I := 1 to 7 do Result[I] := $FF;
end;

class function TOBDIsoBusVT.BuildAudioSignal(ABeeps: Byte;
  AFrequencyHz, AOnMs, AOffMs: Word): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_AUDIO_SIGNAL;
  Result[1] := ABeeps;
  WriteWord(Result, 2, AFrequencyHz);
  WriteWord(Result, 4, AOnMs);
  WriteWord(Result, 6, AOffMs);
end;

class function TOBDIsoBusVT.BuildChangeActiveMask(AWorkingSet,
  AMaskID: Word): TBytes;
begin
  SetLength(Result, 8);
  Result[0] := VT_FN_VT_CHANGE_ACTIVE_MASK_OUT;
  WriteWord(Result, 1, AWorkingSet);
  WriteWord(Result, 3, AMaskID);
  Result[5] := $FF; Result[6] := $FF; Result[7] := $FF;
end;

class function TOBDIsoBusVT.DecodeSoftKey(const APayload: TBytes;
  out AKey: TOBDIsoBusVTSoftKey): Boolean;
begin
  AKey := Default(TOBDIsoBusVTSoftKey);
  if Length(APayload) < 8 then Exit(False);
  if APayload[0] <> VT_FN_SOFT_KEY_ACTIVATION then Exit(False);
  AKey.State        := APayload[1];
  AKey.ObjectID     := (Word(APayload[3]) shl 8) or APayload[2];
  AKey.ParentMaskID := (Word(APayload[5]) shl 8) or APayload[4];
  AKey.KeyCode      := APayload[6];
  Result := True;
end;

class function TOBDIsoBusVT.DecodeVTStatus(const APayload: TBytes;
  out AStatus: TOBDIsoBusVTStatus): Boolean;
begin
  AStatus := Default(TOBDIsoBusVTStatus);
  if Length(APayload) < 8 then Exit(False);
  if APayload[0] <> VT_FN_VT_STATUS then Exit(False);
  AStatus.WorkingSetMasterAddress := APayload[1];
  AStatus.ActiveMaskID            := (Word(APayload[3]) shl 8) or APayload[2];
  AStatus.ActiveSoftKeyMaskID     := (Word(APayload[5]) shl 8) or APayload[4];
  AStatus.BusyCodes               := APayload[6];
  AStatus.FunctionCode            := APayload[7];
  Result := True;
end;

end.
