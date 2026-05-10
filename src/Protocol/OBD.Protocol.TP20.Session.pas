//------------------------------------------------------------------------------
//  OBD.Protocol.TP20.Session
//
//  TOBDTP20Session - non-visual component wrapping a TTP20Codec.
//  Drop on a form, wire the CAN transport in code via
//  SetCANTransport, then Connect / SendBlock / ReceiveBlock /
//  KeepAlive / Disconnect.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Protocol.TP20.Session;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.CAN,
  OBD.Protocol.TP20;

type
  TOBDTP20Session = class(TComponent)
  strict private
    FCodec:        TTP20Codec;
    FTransport:    ICANTransport;
    FAppId:        TTP20AppId;
    FByteTimeoutMs: Integer;
    procedure EnsureCodec;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    /// <summary>Hand over the CAN transport before
    /// <see cref="Connect"/>. Setting this releases any
    /// previously-built codec.</summary>
    procedure SetCANTransport(const ATransport: ICANTransport);

    procedure Connect;
    procedure Disconnect;
    procedure KeepAlive;
    procedure SendBlock(const ABlock: TTP20Block);
    function  ReceiveBlock(ATimeoutMs: Integer): TTP20Block;

    function Connected: Boolean;
    function RxId:      Cardinal;
    function TxId:      Cardinal;
    function BlockSize: Byte;

    property Codec: TTP20Codec read FCodec;
  published
    /// <summary>Application id (= ECU dest id) for the channel
    /// setup. Default <c>apRadio</c> ($56).</summary>
    property AppId: TTP20AppId
      read FAppId write FAppId default apRadio;

    property ByteTimeoutMs: Integer
      read FByteTimeoutMs write FByteTimeoutMs default 1000;
  end;

implementation

constructor TOBDTP20Session.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppId         := apRadio;
  FByteTimeoutMs := 1000;
end;

destructor TOBDTP20Session.Destroy;
begin
  FreeAndNil(FCodec);
  FTransport := nil;
  inherited;
end;

procedure TOBDTP20Session.EnsureCodec;
begin
  if FCodec = nil then
  begin
    if FTransport = nil then
      raise ETP20Error.Create(
        'TOBDTP20Session: SetCANTransport must be called ' +
        'before Connect / any block call.');
    FCodec := TTP20Codec.Create(FTransport, FByteTimeoutMs);
  end;
end;

procedure TOBDTP20Session.SetCANTransport(const ATransport: ICANTransport);
begin
  if FCodec <> nil then FreeAndNil(FCodec);
  FTransport := ATransport;
end;

procedure TOBDTP20Session.Connect;        begin EnsureCodec; FCodec.Connect(FAppId); end;
procedure TOBDTP20Session.Disconnect;     begin if FCodec <> nil then FCodec.Disconnect; end;
procedure TOBDTP20Session.KeepAlive;      begin EnsureCodec; FCodec.KeepAlive; end;
procedure TOBDTP20Session.SendBlock(const ABlock: TTP20Block); begin EnsureCodec; FCodec.SendBlock(ABlock); end;
function  TOBDTP20Session.ReceiveBlock(ATimeoutMs: Integer): TTP20Block;
begin EnsureCodec; Result := FCodec.ReceiveBlock(ATimeoutMs); end;

function TOBDTP20Session.Connected: Boolean;  begin if FCodec <> nil then Result := FCodec.Connected else Result := False; end;
function TOBDTP20Session.RxId:      Cardinal; begin if FCodec <> nil then Result := FCodec.RxId else Result := 0; end;
function TOBDTP20Session.TxId:      Cardinal; begin if FCodec <> nil then Result := FCodec.TxId else Result := 0; end;
function TOBDTP20Session.BlockSize: Byte;     begin if FCodec <> nil then Result := FCodec.BlockSize else Result := 0; end;

end.
