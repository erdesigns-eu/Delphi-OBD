//------------------------------------------------------------------------------
//  OBD.Connection.FTDI
//
//  FTDI USB-serial transport via the D2XX direct driver. Bypasses the
//  Windows COM stack — useful for FT232 / FT2232 / FT4232 chips when
//  COM-port enumeration is unreliable, or when the application wants
//  to address an adapter by serial number.
//
//  D2XX is dynamically loaded; the package does not link against
//  ftd2xx.dll at build time. If the DLL is missing, Open raises with a
//  clear message instead of failing at module-load time.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - FTDI D2XX Programmer's Guide (FT_000071)
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//    2026-05-09  ERD  Phase 2 follow-up: rebased onto TOBDBaseTransport
//                     and instrumented with step-progress events.
//
//  Future work :
//    - 64-bit FTDI EEPROM reading helper (Phase 3 detection)
//    - FT_SetEventNotification to replace the 2 ms idle in the read
//      loop.
//------------------------------------------------------------------------------

unit OBD.Connection.FTDI;

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'OBD.Connection.FTDI is Windows-only.'}
{$ENDIF}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection.Transport.Base;

type
  TOBDFTDITransport = class;

  /// <summary>FTDI D2XX read loop.</summary>
  TOBDFTDIReadThread = class(TThread)
  strict private
    FOwner: TOBDFTDITransport;
    FOnBytes: TProc<TBytes>;
    FOnError: TProc<TOBDErrorCode, string>;
  protected
    /// <summary>Read loop. Exits when the FTDI handle is cleared.</summary>
    procedure Execute; override;
  public
    /// <summary>Spawns the thread bound to its owning transport.</summary>
    /// <param name="AOwner">Transport owning the FT_HANDLE.</param>
    /// <param name="AOnBytes">Inbound-bytes callback. Required.</param>
    /// <param name="AOnError">Error callback. Optional.</param>
    constructor Create(AOwner: TOBDFTDITransport;
      const AOnBytes: TProc<TBytes>;
      const AOnError: TProc<TOBDErrorCode, string>);
  end;

  /// <summary>FTDI D2XX direct transport.</summary>
  TOBDFTDITransport = class(TOBDBaseTransport)
  strict private
    FHandle: Pointer;
    FReader: TOBDFTDIReadThread;
    function GetHandle: Pointer;
  public
    /// <summary>Constructs an idle FTDI transport.</summary>
    constructor Create;
    /// <summary>Closes the device if open.</summary>
    destructor Destroy; override;

    /// <summary>
    ///   Opens the configured FTDI device. Selection precedence:
    ///   <c>SerialNumber</c> > <c>Description</c> > <c>DeviceIndex</c>.
    /// </summary>
    /// <param name="ASettings">FTDI matcher and tuneables.</param>
    /// <remarks>
    ///   Synchronous. Fires four step-progress events:
    ///   <c>1/4 Loading D2XX</c>, <c>2/4 Opening device</c>,
    ///   <c>3/4 Configuring</c>, <c>4/4 Ready</c>. <c>ftd2xx.dll</c>
    ///   is loaded lazily on first use; missing DLL raises with a
    ///   clear message.
    /// </remarks>
    /// <exception cref="EOBDConfig"><c>ASettings</c> is <c>nil</c>.</exception>
    /// <exception cref="EOBDError">D2XX DLL missing, device not
    /// found, or any FT_* call returned non-zero.</exception>
    procedure Open(const ASettings: TOBDFTDISettings);

    /// <summary>Closes the device and joins the read thread.</summary>
    procedure Close; override;
    /// <summary>Writes bytes via <c>FT_Write</c>.</summary>
    /// <param name="ABytes">Bytes to send.</param>
    /// <returns>Number of bytes the driver accepted.</returns>
    /// <exception cref="EOBDNotConnected">Device not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;

    /// <summary>Internal: <c>FT_HANDLE</c> the read thread reads
    /// from.</summary>
    property Handle: Pointer read GetHandle;
  end;

implementation

{ ---- D2XX dynamic binding ---------------------------------------------------- }

const
  FT_OK                  = 0;
  FT_OPEN_BY_SERIAL_NUMBER = 1;
  FT_OPEN_BY_DESCRIPTION   = 2;
  FT_OPEN_BY_LOCATION      = 4;
  FT_PURGE_RX = 1;
  FT_PURGE_TX = 2;

type
  FT_STATUS = Cardinal;
  FT_HANDLE = Pointer;

  TFT_OpenEx       = function(Arg1: PAnsiChar; Flags: DWORD; var FT: FT_HANDLE): FT_STATUS; stdcall;
  TFT_Open         = function(Index: Integer; var FT: FT_HANDLE): FT_STATUS; stdcall;
  TFT_Close        = function(FT: FT_HANDLE): FT_STATUS; stdcall;
  TFT_Read         = function(FT: FT_HANDLE; Buffer: Pointer; BytesToRead: DWORD; var BytesReturned: DWORD): FT_STATUS; stdcall;
  TFT_Write        = function(FT: FT_HANDLE; Buffer: Pointer; BytesToWrite: DWORD; var BytesWritten: DWORD): FT_STATUS; stdcall;
  TFT_SetBaudRate  = function(FT: FT_HANDLE; BaudRate: DWORD): FT_STATUS; stdcall;
  TFT_SetDataChars = function(FT: FT_HANDLE; WordLength, StopBits, Parity: Byte): FT_STATUS; stdcall;
  TFT_SetTimeouts  = function(FT: FT_HANDLE; ReadTO, WriteTO: DWORD): FT_STATUS; stdcall;
  TFT_SetLatency   = function(FT: FT_HANDLE; LatencyTimer: Byte): FT_STATUS; stdcall;
  TFT_Purge        = function(FT: FT_HANDLE; Mask: DWORD): FT_STATUS; stdcall;
  TFT_GetQueueStatus = function(FT: FT_HANDLE; var BytesInRx: DWORD): FT_STATUS; stdcall;

var
  FD2XX: HMODULE = 0;
  FT_OpenEx_F: TFT_OpenEx;
  FT_Open_F: TFT_Open;
  FT_Close_F: TFT_Close;
  FT_Read_F: TFT_Read;
  FT_Write_F: TFT_Write;
  FT_SetBaudRate_F: TFT_SetBaudRate;
  FT_SetDataChars_F: TFT_SetDataChars;
  FT_SetTimeouts_F: TFT_SetTimeouts;
  FT_SetLatency_F: TFT_SetLatency;
  FT_Purge_F: TFT_Purge;
  FT_GetQueueStatus_F: TFT_GetQueueStatus;

procedure EnsureD2XX;
  procedure NeedProc(var P; const Name: AnsiString);
  begin
    Pointer(P) := GetProcAddress(FD2XX, PAnsiChar(Name));
    if Pointer(P) = nil then
      raise EOBDError.CreateFmt(
        'ftd2xx.dll missing symbol "%s"', [string(Name)]);
  end;
begin
  if FD2XX <> 0 then Exit;
  FD2XX := LoadLibrary('ftd2xx.dll');
  if FD2XX = 0 then
    raise EOBDError.Create(
      'ftd2xx.dll could not be loaded — install the FTDI D2XX driver');
  NeedProc(FT_OpenEx_F,         'FT_OpenEx');
  NeedProc(FT_Open_F,           'FT_Open');
  NeedProc(FT_Close_F,          'FT_Close');
  NeedProc(FT_Read_F,           'FT_Read');
  NeedProc(FT_Write_F,          'FT_Write');
  NeedProc(FT_SetBaudRate_F,    'FT_SetBaudRate');
  NeedProc(FT_SetDataChars_F,   'FT_SetDataCharacteristics');
  NeedProc(FT_SetTimeouts_F,    'FT_SetTimeouts');
  NeedProc(FT_SetLatency_F,     'FT_SetLatencyTimer');
  NeedProc(FT_Purge_F,          'FT_Purge');
  NeedProc(FT_GetQueueStatus_F, 'FT_GetQueueStatus');
end;

function StopBitsToFTDI(AValue: TOBDStopBits): Byte;
begin
  case AValue of
    sb1:  Result := 0;
    sb1_5: Result := 1;
    sb2:  Result := 2;
  else
    Result := 0;
  end;
end;

function ParityToFTDI(AValue: TOBDParity): Byte;
begin
  case AValue of
    paNone:  Result := 0;
    paOdd:   Result := 1;
    paEven:  Result := 2;
    paMark:  Result := 3;
    paSpace: Result := 4;
  else
    Result := 0;
  end;
end;

{ ---- TOBDFTDIReadThread ------------------------------------------------------ }

constructor TOBDFTDIReadThread.Create(AOwner: TOBDFTDITransport;
  const AOnBytes: TProc<TBytes>;
  const AOnError: TProc<TOBDErrorCode, string>);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FOnBytes := AOnBytes;
  FOnError := AOnError;
end;

procedure TOBDFTDIReadThread.Execute;
const
  ChunkSize = 256;
var
  Buf: array[0..ChunkSize - 1] of Byte;
  Status: FT_STATUS;
  Got: DWORD;
  H: FT_HANDLE;
  Slice: TBytes;
begin
  while not Terminated do
  begin
    H := FOwner.Handle;
    if H = nil then Break;
    Got := 0;
    Status := FT_Read_F(H, @Buf, ChunkSize, Got);
    if Status <> FT_OK then
    begin
      if Assigned(FOnError) then
        FOnError(oeIO, Format('FT_Read failed (status %d)', [Status]));
      Break;
    end;
    if Got > 0 then
    begin
      SetLength(Slice, Got);
      Move(Buf, Slice[0], Got);
      if Assigned(FOnBytes) then
        FOnBytes(Slice);
    end
    else
      Sleep(2); // brief idle to avoid pegging CPU on FTDI's read timeout
  end;
end;

{ ---- TOBDFTDITransport ------------------------------------------------------- }

constructor TOBDFTDITransport.Create;
begin
  inherited;
end;

destructor TOBDFTDITransport.Destroy;
begin
  Close;
  inherited;
end;

function TOBDFTDITransport.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TOBDFTDITransport.Open(const ASettings: TOBDFTDISettings);
var
  Status: FT_STATUS;
  Tmp: AnsiString;
  Detail: string;
begin
  if ASettings = nil then
    raise EOBDConfig.Create('FTDI settings are nil');

  SetState(csOpening);
  try
    FireProgress(1, 4, 'Loading D2XX', '');
    EnsureD2XX;

    if Trim(ASettings.SerialNumber) <> '' then
      Detail := 'serial=' + ASettings.SerialNumber
    else if Trim(ASettings.Description) <> '' then
      Detail := 'description=' + ASettings.Description
    else
      Detail := Format('index=%d', [ASettings.DeviceIndex]);
    FireProgress(2, 4, 'Opening device', Detail);

    if Trim(ASettings.SerialNumber) <> '' then
    begin
      Tmp := AnsiString(ASettings.SerialNumber);
      Status := FT_OpenEx_F(PAnsiChar(Tmp), FT_OPEN_BY_SERIAL_NUMBER, FHandle);
    end
    else if Trim(ASettings.Description) <> '' then
    begin
      Tmp := AnsiString(ASettings.Description);
      Status := FT_OpenEx_F(PAnsiChar(Tmp), FT_OPEN_BY_DESCRIPTION, FHandle);
    end
    else
    begin
      if ASettings.DeviceIndex < 0 then
        Status := FT_Open_F(0, FHandle)
      else
        Status := FT_Open_F(ASettings.DeviceIndex, FHandle);
    end;
    if Status <> FT_OK then
      raise EOBDError.CreateFmt('FT_Open failed (status %d)', [Status]);

    FireProgress(3, 4, 'Configuring',
      Format('%d baud', [OBDBaudRateValue(ASettings.BaudRate)]));
    Status := FT_SetBaudRate_F(FHandle, OBDBaudRateValue(ASettings.BaudRate));
    if Status <> FT_OK then
      raise EOBDError.CreateFmt('FT_SetBaudRate failed (status %d)', [Status]);
    Status := FT_SetDataChars_F(FHandle, 8,
      StopBitsToFTDI(sb1), ParityToFTDI(paNone));
    if Status <> FT_OK then
      raise EOBDError.CreateFmt('FT_SetDataCharacteristics failed (status %d)', [Status]);
    Status := FT_SetTimeouts_F(FHandle,
      ASettings.ReadTimeout, ASettings.WriteTimeout);
    if Status <> FT_OK then
      raise EOBDError.CreateFmt('FT_SetTimeouts failed (status %d)', [Status]);
    Status := FT_SetLatency_F(FHandle, ASettings.LatencyTimer);
    if Status <> FT_OK then
      raise EOBDError.CreateFmt('FT_SetLatencyTimer failed (status %d)', [Status]);

    FT_Purge_F(FHandle, FT_PURGE_RX or FT_PURGE_TX);
  except
    on E: Exception do
    begin
      if Assigned(FHandle) then
      begin
        FT_Close_F(FHandle);
        FHandle := nil;
      end;
      SetState(csError);
      raise;
    end;
  end;

  FReader := TOBDFTDIReadThread.Create(Self,
    procedure(const Bytes: TBytes) begin FireBytes(Bytes); end,
    procedure(Code: TOBDErrorCode; const Msg: string) begin FireError(Code, Msg); end);

  FireProgress(4, 4, 'Ready', '');
  SetState(csOpen);
end;

procedure TOBDFTDITransport.Close;
var
  H: FT_HANDLE;
begin
  FLock.Enter;
  try
    if FState in [csClosed, csClosing] then Exit;
    SetState(csClosing);
    H := FHandle;
    FHandle := nil;
  finally
    FLock.Leave;
  end;

  if Assigned(FReader) then
  begin
    FReader.Terminate;
    FReader.WaitFor;
    FreeAndNil(FReader);
  end;

  if Assigned(H) and Assigned(FT_Close_F) then
    FT_Close_F(H);

  SetState(csClosed);
end;

function TOBDFTDITransport.WriteBytes(const ABytes: TBytes): Integer;
var
  Status: FT_STATUS;
  Written: DWORD;
begin
  if not IsOpen then
    raise EOBDNotConnected.Create('FTDI transport is not open');
  if Length(ABytes) = 0 then
    Exit(0);
  Written := 0;
  Status := FT_Write_F(FHandle, @ABytes[0], Length(ABytes), Written);
  if Status <> FT_OK then
  begin
    FireError(oeIO, Format('FT_Write failed (status %d)', [Status]));
    Exit(0);
  end;
  Result := Integer(Written);
end;

initialization

finalization
  if FD2XX <> 0 then
  begin
    FreeLibrary(FD2XX);
    FD2XX := 0;
  end;

end.
