//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.Base
//
//  Abstract base TComponent for every per-vendor key-adaptation
//  unit. Owns the safety scaffold (AutoExecute / OnConfirm
//  Execute / standard event firing); subclasses implement
//  DoListSlots / DoAddKey / DoClearOneSlot / DoClearAllKeys /
//  DoCheckPin against their vendor's UDS routines + DIDs.
//
//  Every public destructive op:
//    1. raises EOBDConfig if AutoExecute is False
//    2. fires OnConfirmExecute - must return ACanProceed=True
//       or the op aborts with Result.Message="Host rejected
//       confirmation"
//    3. delegates to the abstract DoXxx method
//    4. fires OnResult with the outcome
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.Base;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Errors,
  OBD.Connection.Types,
  OBD.Protocol,
  OBD.OEM.KeyAdaptation.Types;

type
  TOBDKeyAdaptationBase = class abstract(TComponent)
  strict private
    FProtocol:        TOBDProtocol;
    FAutoExecute:     Boolean;
    FChassisCode:     string;
    FPin:             string;
    FOnConfirm:       TOBDKeyAdaptConfirmEvent;
    FOnResult:        TOBDKeyAdaptResultEvent;
    FOnError:         TOBDConnectionErrorEvent;
    procedure SetProtocol(AValue: TOBDProtocol);
    function  Confirm(AOp: TOBDKeyAdaptOp;
      ASlotIndex: Byte): Boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GuardExecute;
    procedure FireResult(const AResult: TOBDKeyAdaptResult);
    procedure FireError(ACode: TOBDErrorCode; const AMessage: string);

    // Vendor hooks - subclasses override these with their
    // routine IDs / DID writes.
    function DoListSlots: TOBDKeyAdaptResult; virtual; abstract;
    function DoAddKey: TOBDKeyAdaptResult; virtual; abstract;
    function DoClearOneSlot(ASlotIndex: Byte):
      TOBDKeyAdaptResult; virtual; abstract;
    function DoClearAllKeys: TOBDKeyAdaptResult; virtual; abstract;
    function DoCheckPin: TOBDKeyAdaptResult; virtual; abstract;

    // Convenience for subclasses that always need a Protocol.
    function RequireProtocol: TOBDProtocol;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>Vendor-specific identifier for the platform
    /// the host claims this VIN is. Subclasses may use it to
    /// dispatch between routine flavours; the base validates
    /// non-empty before destructive ops if
    /// <see cref="RequiresChassisCode"/> returns True.</summary>
    function RequiresChassisCode: Boolean; virtual;

    /// <summary>Read-only key-slot inventory. Safe to call
    /// without confirmation (non-destructive).</summary>
    function ListSlots: TOBDKeyAdaptResult;

    /// <summary>Programs a new transponder. Destructive -
    /// requires AutoExecute + OnConfirmExecute.</summary>
    function AddKey: TOBDKeyAdaptResult;

    /// <summary>Clear a single slot (typically used to remove
    /// a lost key without wiping all of them).</summary>
    function ClearOneSlot(ASlotIndex: Byte): TOBDKeyAdaptResult;

    /// <summary>Wipe every slot. Destructive.</summary>
    function ClearAllKeys: TOBDKeyAdaptResult;

    /// <summary>Validate the supplied PIN / CS without
    /// triggering any other op. Non-destructive.</summary>
    function CheckPin: TOBDKeyAdaptResult;
  published
    /// <summary>Required - source of the bus reads / writes.</summary>
    property Protocol: TOBDProtocol
      read FProtocol write SetProtocol;

    /// <summary>Master safety gate. Must be True for any
    /// destructive op (AddKey / ClearOne / ClearAll). Off by
    /// default - hosts have to opt in explicitly.</summary>
    property AutoExecute: Boolean
      read FAutoExecute write FAutoExecute default False;

    /// <summary>Vendor chassis / platform key (e.g. "p552"
    /// for Ford F-150, "rb" for Hyundai i20). Used by the
    /// vendor unit to pick the right routine variant. Look up
    /// the keys in catalogs/key-platforms-<vendor>.json.</summary>
    property ChassisCode: string
      read FChassisCode write FChassisCode;

    /// <summary>4 / 5 / 6 / 8-digit dealer PIN or CS code.
    /// Some platforms accept it as decimal text, others as
    /// hex; the per-vendor unit handles the conversion.</summary>
    property PIN: string read FPin write FPin;

    /// <summary>Required for every destructive op. Host
    /// returns ACanProceed=True (typically after a modal
    /// confirmation dialog).</summary>
    property OnConfirmExecute: TOBDKeyAdaptConfirmEvent
      read FOnConfirm write FOnConfirm;

    property OnResult: TOBDKeyAdaptResultEvent
      read FOnResult write FOnResult;
    property OnError: TOBDConnectionErrorEvent
      read FOnError write FOnError;
  end;

implementation

{ TOBDKeyAdaptationBase -------------------------------------------------------}

constructor TOBDKeyAdaptationBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoExecute := False;
end;

procedure TOBDKeyAdaptationBase.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDKeyAdaptationBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FProtocol) then
    FProtocol := nil;
end;

procedure TOBDKeyAdaptationBase.GuardExecute;
begin
  if not FAutoExecute then
    raise EOBDConfig.Create(
      ClassName + ': AutoExecute is False - destructive key ' +
      'adaptation refused. Set AutoExecute=True only after the ' +
      'host has confirmed the operation.');
  if not Assigned(FOnConfirm) then
    raise EOBDConfig.Create(
      ClassName + ': OnConfirmExecute is not assigned - the ' +
      'host must wire a confirmation handler before any ' +
      'destructive op.');
end;

function TOBDKeyAdaptationBase.Confirm(AOp: TOBDKeyAdaptOp;
  ASlotIndex: Byte): Boolean;
begin
  Result := False;
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, AOp, ASlotIndex, Result);
end;

function TOBDKeyAdaptationBase.RequireProtocol: TOBDProtocol;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create(
      ClassName + ': Protocol not assigned');
  Result := FProtocol;
end;

procedure TOBDKeyAdaptationBase.FireResult(
  const AResult: TOBDKeyAdaptResult);
begin
  if Assigned(FOnResult) then FOnResult(Self, AResult);
end;

procedure TOBDKeyAdaptationBase.FireError(ACode: TOBDErrorCode;
  const AMessage: string);
begin
  if Assigned(FOnError) then FOnError(Self, ACode, AMessage);
end;

function TOBDKeyAdaptationBase.RequiresChassisCode: Boolean;
begin
  Result := False;
end;

function TOBDKeyAdaptationBase.ListSlots: TOBDKeyAdaptResult;
begin
  // Non-destructive: no confirmation gate.
  Result := DoListSlots;
  Result.Op := kaoListSlots;
  FireResult(Result);
end;

function TOBDKeyAdaptationBase.AddKey: TOBDKeyAdaptResult;
begin
  GuardExecute;
  if RequiresChassisCode and (FChassisCode = '') then
  begin
    Result := Default(TOBDKeyAdaptResult);
    Result.Op := kaoAddKey;
    Result.Message := ClassName + ': ChassisCode is required';
    FireResult(Result);
    Exit;
  end;
  if not Confirm(kaoAddKey, 0) then
  begin
    Result := Default(TOBDKeyAdaptResult);
    Result.Op := kaoAddKey;
    Result.Message := 'Host rejected confirmation';
    FireResult(Result);
    Exit;
  end;
  Result := DoAddKey;
  Result.Op := kaoAddKey;
  FireResult(Result);
end;

function TOBDKeyAdaptationBase.ClearOneSlot(
  ASlotIndex: Byte): TOBDKeyAdaptResult;
begin
  GuardExecute;
  if not Confirm(kaoClearOneSlot, ASlotIndex) then
  begin
    Result := Default(TOBDKeyAdaptResult);
    Result.Op := kaoClearOneSlot;
    Result.SlotIndex := ASlotIndex;
    Result.Message := 'Host rejected confirmation';
    FireResult(Result);
    Exit;
  end;
  Result := DoClearOneSlot(ASlotIndex);
  Result.Op := kaoClearOneSlot;
  Result.SlotIndex := ASlotIndex;
  FireResult(Result);
end;

function TOBDKeyAdaptationBase.ClearAllKeys: TOBDKeyAdaptResult;
begin
  GuardExecute;
  if not Confirm(kaoClearAllKeys, 0) then
  begin
    Result := Default(TOBDKeyAdaptResult);
    Result.Op := kaoClearAllKeys;
    Result.Message := 'Host rejected confirmation';
    FireResult(Result);
    Exit;
  end;
  Result := DoClearAllKeys;
  Result.Op := kaoClearAllKeys;
  FireResult(Result);
end;

function TOBDKeyAdaptationBase.CheckPin: TOBDKeyAdaptResult;
begin
  Result := DoCheckPin;
  Result.Op := kaoCheckPin;
  FireResult(Result);
end;

end.
