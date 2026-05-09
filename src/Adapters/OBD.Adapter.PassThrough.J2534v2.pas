//------------------------------------------------------------------------------
// UNIT           : OBD.Adapter.PassThrough.J2534v2.pas
// CONTENTS       : J2534-2 (2018) IOCTL constants and SCONFIG_LIST builder
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Adapter.PassThrough.J2534v2;

interface

uses
  System.SysUtils;

//------------------------------------------------------------------------------
// CONSTANTS
//------------------------------------------------------------------------------
const
  // J2534-1 IOCTL ids retained for reference; J2534-2 adds many more.
  IOCTL_GET_CONFIG          = $00000001;
  IOCTL_SET_CONFIG          = $00000002;
  IOCTL_READ_VBATT          = $00000003;
  IOCTL_FIVE_BAUD_INIT      = $00000004;
  IOCTL_FAST_INIT           = $00000005;
  IOCTL_CLEAR_TX_BUFFER     = $00000007;
  IOCTL_CLEAR_RX_BUFFER     = $00000008;
  IOCTL_CLEAR_PERIODIC_MSGS = $00000009;
  IOCTL_CLEAR_MSG_FILTERS   = $0000000A;
  IOCTL_CLEAR_FUNCT_MSG_LOOKUP_TABLE = $0000000B;

  // J2534-2 SET_CONFIG parameter IDs (selection — see spec table 4.1).
  CFG_DATA_RATE             = $00000001;
  CFG_LOOPBACK              = $00000003;
  CFG_NODE_ADDRESS          = $00000004;
  CFG_NETWORK_LINE          = $00000005;
  CFG_P1_MIN                = $00000006;
  CFG_P1_MAX                = $00000007;
  CFG_P2_MIN                = $00000008;
  CFG_P2_MAX                = $00000009;
  CFG_P3_MIN                = $0000000A;
  CFG_P3_MAX                = $0000000B;
  CFG_P4_MIN                = $0000000C;
  CFG_P4_MAX                = $0000000D;
  CFG_W0                    = $0000000E;
  CFG_W1                    = $0000000F;
  CFG_W2                    = $00000010;
  CFG_W3                    = $00000011;
  CFG_W4                    = $00000012;
  CFG_W5                    = $00000013;
  CFG_TIDLE                 = $00000014;
  CFG_TINIL                 = $00000015;
  CFG_TWUP                  = $00000016;
  CFG_PARITY                = $00000017;
  CFG_BIT_SAMPLE_POINT      = $00000018;
  CFG_SYNC_JUMP_WIDTH       = $00000019;
  CFG_T1_MAX                = $0000001C;
  CFG_T2_MAX                = $0000001D;
  CFG_T3_MAX                = $0000001E;
  CFG_T4_MAX                = $0000001F;
  CFG_T5_MAX                = $00000020;
  CFG_ISO15765_BS           = $00000021;
  CFG_ISO15765_STMIN        = $00000022;
  CFG_DATA_BITS             = $00000023;
  CFG_FIVE_BAUD_MOD         = $00000024;
  CFG_BS_TX                 = $00000025;
  CFG_STMIN_TX              = $00000026;
  CFG_T3_TIME_OUT           = $00000027;
  CFG_ISO15765_WFT_MAX      = $00000028;

  // J2534-2 (2018) additions
  CFG_CAN_MIXED_FORMAT      = $0000800B;  // 0=disable, 1=CAN-classic+CAN-FD on the same channel, 2=CAN-FD only
  CFG_J1962_PINS            = $0000800C;  // pin assignment overrides
  CFG_CAN_FD_DATA_RATE      = $00008011;  // bps for the CAN-FD data phase
  CFG_BIT_SAMPLE_POINT_FD   = $00008012;
  CFG_SYNC_JUMP_WIDTH_FD    = $00008013;
  CFG_TX_DELAY_COMP         = $00008014;
  CFG_ISO15765_FD_BS        = $00008021;
  CFG_ISO15765_FD_STMIN     = $00008022;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  EOBDPassThroughJ2534v2 = class(Exception);

  /// <summary>One (parameter, value) entry as understood by SET_CONFIG.</summary>
  TJ2534ConfigEntry = record
    /// <summary>Parameter.</summary>
    Parameter: Cardinal;
    /// <summary>Value.</summary>
    Value: Cardinal;
  end;

  /// <summary>Builder for the SCONFIG_LIST struct passed into
  /// IOCTL_SET_CONFIG. Use Add(...) for each parameter; ToBytes
  /// renders the buffer in the layout the J2534 spec defines:
  ///   uint32 NumOfParams
  ///   for each: uint32 Parameter, uint32 Value
  /// </summary>
  TJ2534ConfigList = class
  private
    FEntries: TArray<TJ2534ConfigEntry>;
  public
    /// <summary>Add.</summary>
    procedure Add(Parameter, Value: Cardinal);
    /// <summary>Count.</summary>
    function Count: Integer;
    /// <summary>To bytes.</summary>
    function ToBytes: TBytes;
  end;

//------------------------------------------------------------------------------
// IMPLEMENTATION
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
// ADD
//------------------------------------------------------------------------------
procedure TJ2534ConfigList.Add(Parameter, Value: Cardinal);
var
  E: TJ2534ConfigEntry;
begin
  E.Parameter := Parameter;
  E.Value := Value;
  FEntries := FEntries + [E];
end;

//------------------------------------------------------------------------------
// COUNT
//------------------------------------------------------------------------------
function TJ2534ConfigList.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TJ2534ConfigList.ToBytes: TBytes;
var
  Buf: TBytes;
  Cursor, I: Integer;
  N: Cardinal;
begin
  N := Cardinal(Length(FEntries));
  SetLength(Buf, 4 + Length(FEntries) * 8);
  // Little-endian everywhere — matches Windows DLL layout the
  // J2534 vendor binaries use.
  Buf[0] := Byte(N);
  Buf[1] := Byte(N shr 8);
  Buf[2] := Byte(N shr 16);
  Buf[3] := Byte(N shr 24);
  Cursor := 4;
  for I := 0 to High(FEntries) do
  begin
    Buf[Cursor]     := Byte(FEntries[I].Parameter);
    Buf[Cursor + 1] := Byte(FEntries[I].Parameter shr 8);
    Buf[Cursor + 2] := Byte(FEntries[I].Parameter shr 16);
    Buf[Cursor + 3] := Byte(FEntries[I].Parameter shr 24);
    Buf[Cursor + 4] := Byte(FEntries[I].Value);
    Buf[Cursor + 5] := Byte(FEntries[I].Value shr 8);
    Buf[Cursor + 6] := Byte(FEntries[I].Value shr 16);
    Buf[Cursor + 7] := Byte(FEntries[I].Value shr 24);
    Inc(Cursor, 8);
  end;
  Result := Buf;
end;

end.
