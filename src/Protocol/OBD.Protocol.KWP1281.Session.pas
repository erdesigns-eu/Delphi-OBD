//------------------------------------------------------------------------------
//  OBD.Protocol.KWP1281.Session
//
//  TOBDKWP1281Session - non-visual component wrapping
//  TKWP1281Codec. Drop one on a form / data module, wire the
//  Transport in code (the K-line / TP2.0 / ISO-TP / J2534
//  transports are interfaces - not design-time-wirable - so
//  the host hands them in via SetTransport before Connect).
//
//  Forwards every codec method as a published method on the
//  component so the IDE Object Inspector shows the API.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.KWP1281.Session;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.KWP1281;

type
  TOBDKWP1281SessionLogEvent = procedure(Sender: TObject;
    const ALine: string) of object;

  TOBDKWP1281Session = class(TComponent)
  strict private
    FCodec:        TKWP1281Codec;
    FTransport:    IKWP1281Transport;
    FByteTimeoutMs: Integer;
    FAddress:      Byte;
    FStrictCounter: Boolean;
    FOnLog:        TOBDKWP1281SessionLogEvent;
    procedure EnsureCodec;
    procedure HandleCodecLog(Sender: TObject; const ALine: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>Hand over the K-line / CAN transport before
    /// calling <see cref="Connect"/>. Setting this releases
    /// any previously-built codec.</summary>
    procedure SetTransport(const ATransport: IKWP1281Transport);

    // ---- session ------------------------------------------
    procedure Connect;
    procedure Disconnect;
    function  Connected: Boolean;
    function  KW1: Byte;
    function  KW2: Byte;
    function  Counter: Byte;

    // ---- generic exchange + ACK pump ---------------------
    function Exchange(const ABlock: TKWP1281Block): TKWP1281Block;
    function SendAck: TKWP1281Block;
    function PumpUntil(AStopTitle: Byte): TArray<TKWP1281Block>;
    function InitialBlocks: TArray<TKWP1281Block>;

    // ---- standard convenience blocks ----------------------
    function ReadDTCs: TArray<TKWP1281Block>;
    function ClearDTCs: TKWP1281Block;
    function ReadRAM(AAddress: Word; ALength: Byte): TBytes;
    function ReadROM(AAddress: Word; ALength: Byte): TBytes;
    function ReadEEPROM(AAddress: Word; ALength: Byte;
      ATitle: Byte = $03): TBytes;
    function ReadGroup(AGroup: Byte): TBytes;
    function ReadSingleValue(AChannel: Byte): TBytes;
    function ReadAdaptation(AChannel: Byte): TBytes;
    function WriteAdaptation(AChannel: Byte; AValue: Word): TBytes;
    function SaveAdaptation: TKWP1281Block;
    function Login(const ASecret: TBytes): TKWP1281Block;
    function LoginLong(const ASecret: TBytes): TKWP1281Block;
    function RequestSeed(ASeedTitle: Byte = $2C): TBytes;
    function SendKey(const AKey: TBytes;
      AKeyTitle: Byte = $2D): TKWP1281Block;
    function SecurityAccess(AAlgo: TFunc<TBytes, TBytes>;
      ASeedTitle: Byte = $2C; AKeyTitle: Byte = $2D): TKWP1281Block;
    function Recode(const ACoding: TBytes): TKWP1281Block;
    function ActuatorTest: TKWP1281Block;

    /// <summary>Direct access to the underlying codec for hosts
    /// that need a method we haven't wrapped.</summary>
    property Codec: TKWP1281Codec read FCodec;
  published
    /// <summary>Diagnostic address for the 5-baud init.
    /// Default 0x56 (VW radio). Common: 0x01 engine, 0x17
    /// instruments, 0x56 radio, 0x7C nav.</summary>
    property Address: Byte read FAddress write FAddress default $56;

    /// <summary>Per-byte timeout in ms. Default 1000. Affects
    /// every TX / RX call on the transport.</summary>
    property ByteTimeoutMs: Integer
      read FByteTimeoutMs write FByteTimeoutMs default 1000;

    /// <summary>True (default) raises on counter drift; False
    /// swallows drift on buggy radio firmware.</summary>
    property StrictCounter: Boolean
      read FStrictCounter write FStrictCounter default True;

    /// <summary>Per-line on-wire trace. Wire to capture for
    /// offline analysis.</summary>
    property OnLog: TOBDKWP1281SessionLogEvent
      read FOnLog write FOnLog;
  end;

implementation

constructor TOBDKWP1281Session.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddress       := $56;
  FByteTimeoutMs := 1000;
  FStrictCounter := True;
end;

destructor TOBDKWP1281Session.Destroy;
begin
  FreeAndNil(FCodec);
  FTransport := nil;
  inherited;
end;

procedure TOBDKWP1281Session.EnsureCodec;
begin
  if FCodec = nil then
  begin
    if FTransport = nil then
      raise EKWP1281Error.Create(
        'TOBDKWP1281Session: SetTransport must be called before ' +
        'Connect / any block call.');
    FCodec := TKWP1281Codec.Create(FTransport, FByteTimeoutMs);
    FCodec.StrictCounter := FStrictCounter;
    FCodec.OnLog := HandleCodecLog;
  end;
end;

procedure TOBDKWP1281Session.HandleCodecLog(Sender: TObject;
  const ALine: string);
begin
  if Assigned(FOnLog) then FOnLog(Self, ALine);
end;

procedure TOBDKWP1281Session.SetTransport(const ATransport: IKWP1281Transport);
begin
  if FCodec <> nil then FreeAndNil(FCodec);
  FTransport := ATransport;
end;

procedure TOBDKWP1281Session.Connect;
begin
  EnsureCodec;
  FCodec.Connect(FAddress);
end;

procedure TOBDKWP1281Session.Disconnect;
begin
  if FCodec <> nil then FCodec.Disconnect;
end;

function TOBDKWP1281Session.Connected:     Boolean; begin if FCodec <> nil then Result := FCodec.Connected else Result := False; end;
function TOBDKWP1281Session.KW1:           Byte;    begin if FCodec <> nil then Result := FCodec.KW1 else Result := 0; end;
function TOBDKWP1281Session.KW2:           Byte;    begin if FCodec <> nil then Result := FCodec.KW2 else Result := 0; end;
function TOBDKWP1281Session.Counter:       Byte;    begin if FCodec <> nil then Result := FCodec.Counter else Result := 0; end;

function TOBDKWP1281Session.Exchange(const ABlock: TKWP1281Block): TKWP1281Block;
begin EnsureCodec; Result := FCodec.Exchange(ABlock); end;

function TOBDKWP1281Session.SendAck: TKWP1281Block;
begin EnsureCodec; Result := FCodec.SendAck; end;

function TOBDKWP1281Session.PumpUntil(AStopTitle: Byte): TArray<TKWP1281Block>;
begin EnsureCodec; Result := FCodec.PumpUntil(AStopTitle); end;

function TOBDKWP1281Session.InitialBlocks: TArray<TKWP1281Block>;
begin EnsureCodec; Result := FCodec.InitialBlocks; end;

function TOBDKWP1281Session.ReadDTCs: TArray<TKWP1281Block>;
begin EnsureCodec; Result := FCodec.ReadDTCs; end;

function TOBDKWP1281Session.ClearDTCs: TKWP1281Block;
begin EnsureCodec; Result := FCodec.ClearDTCs; end;

function TOBDKWP1281Session.ReadRAM(AAddress: Word; ALength: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadRAM(AAddress, ALength); end;

function TOBDKWP1281Session.ReadROM(AAddress: Word; ALength: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadROM(AAddress, ALength); end;

function TOBDKWP1281Session.ReadEEPROM(AAddress: Word; ALength: Byte;
  ATitle: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadEEPROM(AAddress, ALength, ATitle); end;

function TOBDKWP1281Session.ReadGroup(AGroup: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadGroup(AGroup); end;

function TOBDKWP1281Session.ReadSingleValue(AChannel: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadSingleValue(AChannel); end;

function TOBDKWP1281Session.ReadAdaptation(AChannel: Byte): TBytes;
begin EnsureCodec; Result := FCodec.ReadAdaptation(AChannel); end;

function TOBDKWP1281Session.WriteAdaptation(AChannel: Byte; AValue: Word): TBytes;
begin EnsureCodec; Result := FCodec.WriteAdaptation(AChannel, AValue); end;

function TOBDKWP1281Session.SaveAdaptation: TKWP1281Block;
begin EnsureCodec; Result := FCodec.SaveAdaptation; end;

function TOBDKWP1281Session.Login(const ASecret: TBytes): TKWP1281Block;
begin EnsureCodec; Result := FCodec.Login(ASecret); end;

function TOBDKWP1281Session.LoginLong(const ASecret: TBytes): TKWP1281Block;
begin EnsureCodec; Result := FCodec.LoginLong(ASecret); end;

function TOBDKWP1281Session.RequestSeed(ASeedTitle: Byte): TBytes;
begin EnsureCodec; Result := FCodec.RequestSeed(ASeedTitle); end;

function TOBDKWP1281Session.SendKey(const AKey: TBytes;
  AKeyTitle: Byte): TKWP1281Block;
begin EnsureCodec; Result := FCodec.SendKey(AKey, AKeyTitle); end;

function TOBDKWP1281Session.SecurityAccess(AAlgo: TFunc<TBytes, TBytes>;
  ASeedTitle: Byte; AKeyTitle: Byte): TKWP1281Block;
begin EnsureCodec; Result := FCodec.SecurityAccess(AAlgo, ASeedTitle, AKeyTitle); end;

function TOBDKWP1281Session.Recode(const ACoding: TBytes): TKWP1281Block;
begin EnsureCodec; Result := FCodec.Recode(ACoding); end;

function TOBDKWP1281Session.ActuatorTest: TKWP1281Block;
begin EnsureCodec; Result := FCodec.ActuatorTest; end;

end.
