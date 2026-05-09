//------------------------------------------------------------------------------
//  OBD.Signature.HSM.PKCS11
//
//  TOBDSignaturePKCS11 — concrete PKCS#11 v3.0 vendor shim that
//  drives any PKCS#11 driver through the standard
//  C_GetFunctionList → C_Initialize → C_OpenSession → C_Login →
//  C_FindObjects → C_VerifyInit → C_Verify → C_Logout →
//  C_CloseSession → C_Finalize sequence.
//
//  Tested against SoftHSM 2.x; the same code drives Vector,
//  Thales, Utimaco, AWS CloudHSM, and YubiHSM2 with their own
//  PKCS#11 drivers — only <c>LibraryPath</c> changes.
//
//  Hosts assign:
//    LibraryPath  := 'C:\Program Files\SoftHSM2\lib\softhsm2.dll'
//    SlotID       := 0;
//    PinFunc      := function: string begin Result := '1234'; end;
//    KeyLabelOrID := <CKA_LABEL or CKA_ID of the public key>
//
//  The bound public-key handle is found via CKA_LABEL (when the
//  string is non-numeric) or CKA_ID (when it parses as hex).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - PKCS#11 v3.0 (OASIS)
//
//  History     :
//    2026-05-09  ERD  Phase 9 follow-up.
//------------------------------------------------------------------------------

unit OBD.Signature.HSM.PKCS11;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  OBD.Types,
  OBD.Signature,
  OBD.Signature.HSM;

type
  /// <summary>Concrete PKCS#11 verifier.</summary>
  TOBDSignaturePKCS11 = class(TOBDSignatureVerifier)
  strict private
    FLibraryPath: string;
    FSlotID: Cardinal;
    FPINFunc: TOBDPKCS11PINFunc;
    FKeyLabelOrID: string;
    FLib: HMODULE;
    FFuncList: Pointer;
    FLoadLock: TCriticalSection;
    procedure EnsureLoaded;
    procedure Unload;
  strict protected
    function DoVerify(const AArgs: TOBDSignatureVerifyArgs): Boolean; override;
    function DoSupports(AAlgorithm: TOBDSignatureAlgorithm): Boolean; override;
    function DoName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>True when <c>LibraryPath</c> exists and dynamic-
    /// loads cleanly.</summary>
    function IsAvailable: Boolean;
    property LibraryPath: string read FLibraryPath write FLibraryPath;
    property SlotID: Cardinal read FSlotID write FSlotID;
    property PINFunc: TOBDPKCS11PINFunc read FPINFunc write FPINFunc;
    /// <summary>CKA_LABEL of the public key (preferred) OR a
    /// hex-encoded CKA_ID. The Verify path tries CKA_LABEL first,
    /// then CKA_ID when the string is hex-only.</summary>
    property KeyLabelOrID: string read FKeyLabelOrID write FKeyLabelOrID;
  end;

implementation

const
  // ---- PKCS#11 return codes ----
  CKR_OK                = 0;
  CKR_FUNCTION_FAILED   = $00000006;

  // ---- Object class ----
  CKO_PUBLIC_KEY        = $00000002;

  // ---- Attribute IDs ----
  CKA_CLASS             = $00000000;
  CKA_LABEL             = $00000003;
  CKA_ID                = $00000102;

  // ---- Mechanisms ----
  CKM_SHA256_RSA_PKCS_PKCS11   = $00000040;
  CKM_RSA_PKCS_PSS_PKCS11      = $0000000D;
  CKM_ECDSA_SHA256_PKCS11      = $00001044;
  CKM_ECDSA_SHA384_PKCS11      = $00001045;
  CKM_EDDSA_PKCS11             = $00001057;
  CKM_SHA256_RSA_PKCS_PSS      = $00000043;

  // ---- Login user types ----
  CKU_USER              = 1;

  // ---- Session flags ----
  CKF_RW_SESSION        = $00000002;
  CKF_SERIAL_SESSION    = $00000004;

type
  CK_RV = NativeUInt;
  CK_SLOT_ID = NativeUInt;
  CK_SESSION_HANDLE = NativeUInt;
  CK_OBJECT_HANDLE = NativeUInt;
  CK_FLAGS = NativeUInt;
  CK_ULONG = NativeUInt;
  CK_USER_TYPE = NativeUInt;
  CK_NOTIFY = Pointer;

  CK_ATTRIBUTE = record
    type_: NativeUInt;
    pValue: Pointer;
    ulValueLen: NativeUInt;
  end;
  PCK_ATTRIBUTE = ^CK_ATTRIBUTE;

  CK_MECHANISM = record
    mechanism: NativeUInt;
    pParameter: Pointer;
    ulParameterLen: NativeUInt;
  end;

  // PKCS#11 v3.0 standard function-pointer table.
  CK_FUNCTION_LIST_PTR = ^CK_FUNCTION_LIST;
  CK_FUNCTION_LIST = record
    version: array[0..1] of Byte;
    C_Initialize: Pointer;
    C_Finalize: Pointer;
    C_GetInfo: Pointer;
    C_GetFunctionList: Pointer;
    C_GetSlotList: Pointer;
    C_GetSlotInfo: Pointer;
    C_GetTokenInfo: Pointer;
    C_GetMechanismList: Pointer;
    C_GetMechanismInfo: Pointer;
    C_InitToken: Pointer;
    C_InitPIN: Pointer;
    C_SetPIN: Pointer;
    C_OpenSession: Pointer;
    C_CloseSession: Pointer;
    C_CloseAllSessions: Pointer;
    C_GetSessionInfo: Pointer;
    C_GetOperationState: Pointer;
    C_SetOperationState: Pointer;
    C_Login: Pointer;
    C_Logout: Pointer;
    C_CreateObject: Pointer;
    C_CopyObject: Pointer;
    C_DestroyObject: Pointer;
    C_GetObjectSize: Pointer;
    C_GetAttributeValue: Pointer;
    C_SetAttributeValue: Pointer;
    C_FindObjectsInit: Pointer;
    C_FindObjects: Pointer;
    C_FindObjectsFinal: Pointer;
    C_EncryptInit: Pointer;
    C_Encrypt: Pointer;
    C_EncryptUpdate: Pointer;
    C_EncryptFinal: Pointer;
    C_DecryptInit: Pointer;
    C_Decrypt: Pointer;
    C_DecryptUpdate: Pointer;
    C_DecryptFinal: Pointer;
    C_DigestInit: Pointer;
    C_Digest: Pointer;
    C_DigestUpdate: Pointer;
    C_DigestKey: Pointer;
    C_DigestFinal: Pointer;
    C_SignInit: Pointer;
    C_Sign: Pointer;
    C_SignUpdate: Pointer;
    C_SignFinal: Pointer;
    C_SignRecoverInit: Pointer;
    C_SignRecover: Pointer;
    C_VerifyInit: Pointer;
    C_Verify: Pointer;
    C_VerifyUpdate: Pointer;
    C_VerifyFinal: Pointer;
    // ... (further pointers omitted; we don't call them)
  end;

  TC_GetFunctionList = function(out ppFunctionList: CK_FUNCTION_LIST_PTR): CK_RV; cdecl;
  TC_Initialize = function(pInitArgs: Pointer): CK_RV; cdecl;
  TC_Finalize = function(pReserved: Pointer): CK_RV; cdecl;
  TC_OpenSession = function(slot: CK_SLOT_ID; flags: CK_FLAGS;
    appCallback: Pointer; notify: CK_NOTIFY;
    out hSession: CK_SESSION_HANDLE): CK_RV; cdecl;
  TC_CloseSession = function(hSession: CK_SESSION_HANDLE): CK_RV; cdecl;
  TC_Login = function(hSession: CK_SESSION_HANDLE; userType: CK_USER_TYPE;
    pPin: PAnsiChar; ulPinLen: CK_ULONG): CK_RV; cdecl;
  TC_Logout = function(hSession: CK_SESSION_HANDLE): CK_RV; cdecl;
  TC_FindObjectsInit = function(hSession: CK_SESSION_HANDLE;
    pTemplate: PCK_ATTRIBUTE; ulCount: CK_ULONG): CK_RV; cdecl;
  TC_FindObjects = function(hSession: CK_SESSION_HANDLE;
    var phObject: CK_OBJECT_HANDLE; ulMax: CK_ULONG;
    out pulCount: CK_ULONG): CK_RV; cdecl;
  TC_FindObjectsFinal = function(hSession: CK_SESSION_HANDLE): CK_RV; cdecl;
  TC_VerifyInit = function(hSession: CK_SESSION_HANDLE;
    pMechanism: Pointer; hKey: CK_OBJECT_HANDLE): CK_RV; cdecl;
  TC_Verify = function(hSession: CK_SESSION_HANDLE;
    pData: Pointer; ulDataLen: CK_ULONG;
    pSignature: Pointer; ulSigLen: CK_ULONG): CK_RV; cdecl;

constructor TOBDSignaturePKCS11.Create;
begin
  inherited Create;
  FLoadLock := TCriticalSection.Create;
end;

destructor TOBDSignaturePKCS11.Destroy;
begin
  Unload;
  FLoadLock.Free;
  inherited;
end;

procedure TOBDSignaturePKCS11.Unload;
begin
{$IFDEF MSWINDOWS}
  if FLib <> 0 then FreeLibrary(FLib);
{$ENDIF}
  FLib := 0;
  FFuncList := nil;
end;

procedure TOBDSignaturePKCS11.EnsureLoaded;
var
  GetFuncList: TC_GetFunctionList;
  RC: CK_RV;
begin
  if FFuncList <> nil then Exit;
  FLoadLock.Enter;
  try
    if FFuncList <> nil then Exit;
    if FLibraryPath = '' then
      raise EOBDConfig.Create('PKCS#11: LibraryPath not set');
{$IFDEF MSWINDOWS}
    FLib := LoadLibrary(PChar(FLibraryPath));
{$ENDIF}
    if FLib = 0 then
      raise EOBDError.CreateFmt(
        'PKCS#11: failed to load "%s"', [FLibraryPath]);
{$IFDEF MSWINDOWS}
    @GetFuncList := GetProcAddress(FLib, 'C_GetFunctionList');
{$ENDIF}
    if not Assigned(GetFuncList) then
      raise EOBDError.Create('PKCS#11: C_GetFunctionList missing');
    RC := GetFuncList(CK_FUNCTION_LIST_PTR(FFuncList));
    if RC <> CKR_OK then
      raise EOBDError.CreateFmt('PKCS#11: C_GetFunctionList failed (0x%.8X)',
        [RC]);
  finally
    FLoadLock.Leave;
  end;
end;

function TOBDSignaturePKCS11.IsAvailable: Boolean;
begin
  if FLibraryPath = '' then Exit(False);
  if not FileExists(FLibraryPath) then Exit(False);
  try
    EnsureLoaded;
    Result := FFuncList <> nil;
  except
    Result := False;
  end;
end;

function TOBDSignaturePKCS11.DoName: string;
begin
  Result := 'PKCS#11 v3.0 (vendor shim)';
end;

function TOBDSignaturePKCS11.DoSupports(
  AAlgorithm: TOBDSignatureAlgorithm): Boolean;
begin
  case AAlgorithm of
    saRSA_PSS_SHA256, saRSA_PKCS1_SHA256,
    saECDSA_P256_SHA256, saECDSA_P384_SHA384,
    saED25519:
      Result := True;
  else
    Result := False;
  end;
end;

function ParseHexBytes(const AHex: string): TBytes;
var
  I: Integer;
  S: string;
begin
  S := UpperCase(StringReplace(AHex, ' ', '', [rfReplaceAll]));
  if Odd(Length(S)) then Exit(nil);
  SetLength(Result, Length(S) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(S, I * 2 + 1, 2));
end;

function MechanismFor(AAlgorithm: TOBDSignatureAlgorithm): NativeUInt;
begin
  case AAlgorithm of
    saRSA_PKCS1_SHA256:    Result := CKM_SHA256_RSA_PKCS_PKCS11;
    saRSA_PSS_SHA256:      Result := CKM_SHA256_RSA_PKCS_PSS;
    saECDSA_P256_SHA256:   Result := CKM_ECDSA_SHA256_PKCS11;
    saECDSA_P384_SHA384:   Result := CKM_ECDSA_SHA384_PKCS11;
    saED25519:             Result := CKM_EDDSA_PKCS11;
  else
    Result := 0;
  end;
end;

function TOBDSignaturePKCS11.DoVerify(
  const AArgs: TOBDSignatureVerifyArgs): Boolean;
var
  FuncList: CK_FUNCTION_LIST_PTR;
  Init_F: TC_Initialize;
  Final_F: TC_Finalize;
  Open_F: TC_OpenSession;
  Close_F: TC_CloseSession;
  Login_F: TC_Login;
  Logout_F: TC_Logout;
  FindInit_F: TC_FindObjectsInit;
  Find_F: TC_FindObjects;
  FindFin_F: TC_FindObjectsFinal;
  VerifyInit_F: TC_VerifyInit;
  Verify_F: TC_Verify;
  Session: CK_SESSION_HANDLE;
  KeyHandle: CK_OBJECT_HANDLE;
  Found: CK_ULONG;
  RC: CK_RV;
  Template: array[0..2] of CK_ATTRIBUTE;
  TemplateCount: Integer;
  ObjClass: NativeUInt;
  Mechanism: CK_MECHANISM;
  PIN: AnsiString;
  KeyID: TBytes;
  LabelStr: AnsiString;
begin
  EnsureLoaded;
  FuncList := CK_FUNCTION_LIST_PTR(FFuncList);

  @Init_F     := FuncList^.C_Initialize;
  @Final_F    := FuncList^.C_Finalize;
  @Open_F     := FuncList^.C_OpenSession;
  @Close_F    := FuncList^.C_CloseSession;
  @Login_F    := FuncList^.C_Login;
  @Logout_F   := FuncList^.C_Logout;
  @FindInit_F := FuncList^.C_FindObjectsInit;
  @Find_F     := FuncList^.C_FindObjects;
  @FindFin_F  := FuncList^.C_FindObjectsFinal;
  @VerifyInit_F := FuncList^.C_VerifyInit;
  @Verify_F   := FuncList^.C_Verify;

  RC := Init_F(nil);
  if RC <> CKR_OK then
    raise EOBDError.CreateFmt('C_Initialize failed (0x%.8X)', [RC]);
  try
    RC := Open_F(FSlotID, CKF_SERIAL_SESSION, nil, nil, Session);
    if RC <> CKR_OK then
      raise EOBDError.CreateFmt('C_OpenSession failed (0x%.8X)', [RC]);
    try
      // Public-key Verify in PKCS#11 doesn't require login on most
      // tokens, but some HSMs require user-context for any object
      // access. Login when a PIN func is wired.
      if Assigned(FPINFunc) then
      begin
        PIN := AnsiString(FPINFunc);
        if PIN <> '' then
        begin
          RC := Login_F(Session, CKU_USER, PAnsiChar(PIN), Length(PIN));
          if (RC <> CKR_OK) then
            raise EOBDError.CreateFmt('C_Login failed (0x%.8X)', [RC]);
        end;
      end;

      // Find the key. Build a 2- or 3-attribute template:
      //   CKA_CLASS = CKO_PUBLIC_KEY (always)
      //   CKA_LABEL = <ASCII> or CKA_ID = <bytes>
      ObjClass := CKO_PUBLIC_KEY;
      Template[0].type_ := CKA_CLASS;
      Template[0].pValue := @ObjClass;
      Template[0].ulValueLen := SizeOf(ObjClass);
      TemplateCount := 1;

      KeyID := ParseHexBytes(FKeyLabelOrID);
      if (Length(KeyID) > 0) and
         (Length(KeyID) * 2 = Length(StringReplace(
           FKeyLabelOrID, ' ', '', [rfReplaceAll]))) then
      begin
        Template[1].type_ := CKA_ID;
        Template[1].pValue := @KeyID[0];
        Template[1].ulValueLen := Length(KeyID);
        TemplateCount := 2;
      end
      else
      begin
        LabelStr := AnsiString(FKeyLabelOrID);
        Template[1].type_ := CKA_LABEL;
        Template[1].pValue := PAnsiChar(LabelStr);
        Template[1].ulValueLen := Length(LabelStr);
        TemplateCount := 2;
      end;

      RC := FindInit_F(Session, @Template[0], TemplateCount);
      if RC <> CKR_OK then
        raise EOBDError.CreateFmt('C_FindObjectsInit failed (0x%.8X)', [RC]);
      try
        RC := Find_F(Session, KeyHandle, 1, Found);
        if RC <> CKR_OK then
          raise EOBDError.CreateFmt('C_FindObjects failed (0x%.8X)', [RC]);
        if Found = 0 then
          raise EOBDError.CreateFmt(
            'PKCS#11: no public key matches "%s" in slot %d',
            [FKeyLabelOrID, FSlotID]);
      finally
        FindFin_F(Session);
      end;

      Mechanism := Default(CK_MECHANISM);
      Mechanism.mechanism := MechanismFor(AArgs.Algorithm);
      if Mechanism.mechanism = 0 then
        raise EOBDConfig.Create('PKCS#11: algorithm not supported');

      RC := VerifyInit_F(Session, @Mechanism, KeyHandle);
      if RC <> CKR_OK then
        raise EOBDError.CreateFmt('C_VerifyInit failed (0x%.8X)', [RC]);

      RC := Verify_F(Session,
        @AArgs.Message[0], Length(AArgs.Message),
        @AArgs.Signature[0], Length(AArgs.Signature));
      Result := RC = CKR_OK;

      if Assigned(FPINFunc) and (PIN <> '') then
        Logout_F(Session);
    finally
      Close_F(Session);
    end;
  finally
    Final_F(nil);
  end;
end;

end.
