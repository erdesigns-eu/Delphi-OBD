// ------------------------------------------------------------------------------
// ERD.Simulator.ELM327
//
// ELM327/STN text-mode emulator and connection transport for simulator tests.
//
// Converts AT/ST and OBD command text into prompt-terminated responses with
// configurable echo, spacing, line endings, headers, CAN auto-formatting and
// adapter persona behaviour.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// References  :
// - ELM327 datasheet v2.x (AT command set and text formatting)
// - ISO 15765-2 / ISO 15765-4 (CAN OBD-II framing)
// - SAE J1979 / J1979-2 (OBD-II and WWH/J1979-2 services)
//
// History     :
// 2026-10-12  ERD  Initial implementation.
// 2026-10-12  ERD  Use invariant ATRV formatting and serialize responder teardown.
// ------------------------------------------------------------------------------

unit ERD.Simulator.ELM327;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
{$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
{$IFDEF FPC}SyncObjs{$ELSE}System.SyncObjs{$ENDIF},
  ERD.Types,
  ERD.Connection.Types,
  ERD.Connection.Transport.Base,
  ERD.Connection,
  ERD.Simulator.Vehicle;

type
  /// <summary>Adapter identity persona exposed by the emulator.</summary>
  TOBDELM327Persona = (
    /// <summary>Genuine ELM327 v2.3-like behaviour.</summary>
    epELM327v23,
    /// <summary>Common v1.5 clone banner and stricter command set.</summary>
    epELM327v15Clone,
    /// <summary>OBDLink/STN-like persona with STI/STDI support.</summary>
    epOBDLinkSTN);

  /// <summary>ELM327/STN command interpreter backed by a simulated vehicle.</summary>
  TOBDELM327Emulator = class
  strict private
    FVehicle: TOBDSimVehicle;
    FOwnsVehicle: Boolean;
    FPersona: TOBDELM327Persona;
    FEcho: Boolean;
    FLineFeeds: Boolean;
    FSpaces: Boolean;
    FHeaders: Boolean;
    FCanAutoFormatting: Boolean;
    FProtocol: string;
    FAutoProtocol: Boolean;
    FSearchPending: Boolean;
    FHeader: Cardinal;
    FReceiveFilter: string;
    FExtendedAddress: string;
    FAdaptiveTiming: Byte;
    FTimeoutByte: Byte;
    procedure SetVehicle(AValue: TOBDSimVehicle);
    function Banner: string;
    function DeviceDescription: string;
    function EOL: string;
    function NormalizeCommand(const ACommand: string): string;
    function ParseHexCommand(const ACommand: string; out ABytes: TBytes): Boolean;
    function ParseHexId(const AText: string; out AId: Cardinal): Boolean;
    function FormatId(AId: Cardinal): string;
    function FilterAllows(AResponseId: Cardinal): Boolean;
    function ApplySpaceSetting(const ALine: string): string;
    function BuildOutput(const ACommand: string; ADoEcho: Boolean;
      ALines: TStrings): string;
    procedure ExecuteAT(const ANormalized: string; ALines: TStrings);
    procedure ExecuteST(const ANormalized: string; ALines: TStrings);
    procedure ExecuteOBD(const ANormalized: string; ALines: TStrings);
    procedure AddFormattedResponses(const AResponses: TOBDSimECUResponses;
      ALines: TStrings);
    procedure AddCAF1PayloadBlock(const APayload: TBytes; ALines: TStrings);
    procedure AddCANFrames(AResponseId: Cardinal; const APayload: TBytes;
      ALines: TStrings);
  public
    /// <summary>Creates an ELM327 v2.3 emulator with a Golf demo vehicle.</summary>
    constructor Create;
    /// <summary>Destroys an owned vehicle if one was created by the emulator.</summary>
    destructor Destroy; override;

    /// <summary>Restores adapter settings to ELM power-on defaults.</summary>
    procedure ResetAdapter;

    /// <summary>Executes one command and returns the complete text including prompt.</summary>
    function Execute(const ACommand: string): string;

    /// <summary>Vehicle model used to answer OBD requests.</summary>
    property Vehicle: TOBDSimVehicle read FVehicle write SetVehicle;
    /// <summary>Adapter persona and banner style.</summary>
    property Persona: TOBDELM327Persona read FPersona write FPersona;
    /// <summary>Whether input commands are echoed before the response.</summary>
    property Echo: Boolean read FEcho write FEcho;
    /// <summary>Whether responses use CRLF instead of CR line endings.</summary>
    property LineFeeds: Boolean read FLineFeeds write FLineFeeds;
    /// <summary>Whether formatted hex bytes contain spaces.</summary>
    property Spaces: Boolean read FSpaces write FSpaces;
    /// <summary>Whether CAN identifiers are printed before each CAN frame.</summary>
    property Headers: Boolean read FHeaders write FHeaders;
    /// <summary>Whether ELM CAN auto-formatting is enabled.</summary>
    property CanAutoFormatting: Boolean read FCanAutoFormatting
      write FCanAutoFormatting;
  end;

  /// <summary>Asynchronous transport that feeds TOBDConnection from the emulator.</summary>
  TOBDELM327EmulatorTransport = class(TOBDBaseTransport)
  strict private
    FEmulator: TOBDELM327Emulator;
    FOwnsEmulator: Boolean;
    FBuffer: string;
    FResponseDelayMs: Cardinal;
    FWorker: TThread;
    FWorkerLock: TCriticalSection;
    FClosing: Boolean;
    FKeepOpenOnClose: Boolean;
    procedure WaitForWorker;
    procedure StartResponder(const AText: string);
    procedure SetEmulator(AValue: TOBDELM327Emulator);
    procedure DeliverBytes(const ABytes: TBytes);
  public
    /// <summary>Creates a closed transport with an owned emulator.</summary>
    constructor Create;
    /// <summary>Closes the transport and releases an owned emulator.</summary>
    destructor Destroy; override;

    /// <summary>Marks the in-memory transport open and ready for writes.</summary>
    procedure Open;
    /// <summary>Closes the in-memory transport and waits for pending responses.</summary>
    procedure Close; override;
    /// <summary>Accepts bytes, splits commands at CR and schedules responses.</summary>
    function WriteBytes(const ABytes: TBytes): Integer; override;

    /// <summary>Owned or assigned emulator instance.</summary>
    property Emulator: TOBDELM327Emulator read FEmulator write SetEmulator;
    /// <summary>Delay inserted before each response is delivered.</summary>
    property ResponseDelayMs: Cardinal read FResponseDelayMs write FResponseDelayMs;
    /// <summary>Leaves the transport open when TOBDConnection closes it.</summary>
    property KeepOpenOnClose: Boolean read FKeepOpenOnClose write FKeepOpenOnClose;
  end;

  /// <summary>TOBDConnection subclass pre-wired to an ELM327 emulator transport.</summary>
  TOBDEmulatorConnection = class(TOBDConnection)
  strict private
    FTransportObject: TOBDELM327EmulatorTransport;
    FTransportIntf: IOBDConnectionTransport;
    function GetPersona: TOBDELM327Persona;
    procedure SetPersona(AValue: TOBDELM327Persona);
    function GetResponseDelayMs: Cardinal;
    procedure SetResponseDelayMs(AValue: Cardinal);
    function GetEnableFaults: Boolean;
    procedure SetEnableFaults(AValue: Boolean);
    function GetEnableJ1979DIDs: Boolean;
    procedure SetEnableJ1979DIDs(AValue: Boolean);
    function GetVehicleProfile: TOBDSimVehicleProfile;
    procedure SetVehicleProfile(AValue: TOBDSimVehicleProfile);
  public
    /// <summary>Creates the emulator transport and installs it as CustomTransport.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Closes the emulator transport and releases references.</summary>
    destructor Destroy; override;

    /// <summary>Ensures the embedded transport is open before activating.</summary>
    procedure Open; reintroduce;

    /// <summary>Embedded transport for low-level tests.</summary>
    property EmulatorTransport: TOBDELM327EmulatorTransport read FTransportObject;
  published
    /// <summary>Vehicle profile. Only the Golf VII diesel is implemented.</summary>
    property VehicleProfile: TOBDSimVehicleProfile read GetVehicleProfile
      write SetVehicleProfile default svpGolf7TDI;
    /// <summary>Adapter persona.</summary>
    property Persona: TOBDELM327Persona read GetPersona write SetPersona
      default epELM327v23;
    /// <summary>Asynchronous response delay in milliseconds.</summary>
    property ResponseDelayMs: Cardinal read GetResponseDelayMs
      write SetResponseDelayMs default 15;
    /// <summary>Enables the EGR fault and DTCs in the vehicle model.</summary>
    property EnableFaults: Boolean read GetEnableFaults write SetEnableFaults
      default True;
    /// <summary>Enables optional J1979-2 UDS DID responses.</summary>
    property EnableJ1979DIDs: Boolean read GetEnableJ1979DIDs
      write SetEnableJ1979DIDs default False;
  end;

implementation

type
  TELMFrame = record
    Id: Cardinal;
    Data: TBytes;
  end;

  TELMFrames = array of TELMFrame;

function IsHex(C: Char): Boolean;
begin
  Result := CharInSet(C, ['0' .. '9', 'A' .. 'F', 'a' .. 'f']);
end;

function HexNibble(C: Char): Byte;
begin
  case C of
    '0' .. '9':
      Result := Ord(C) - Ord('0');
    'A' .. 'F':
      Result := Ord(C) - Ord('A') + 10;
    'a' .. 'f':
      Result := Ord(C) - Ord('a') + 10;
  else
    Result := 0;
  end;
end;

function InvariantFloatFormatSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
end;

function BytesNoSpaces(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    if AText[I] <> ' ' then
      Result := Result + AText[I];
end;

function IsFrameLikeLine(const ALine: string): Boolean;
var
  I: Integer;
  HasHex: Boolean;
begin
  HasHex := False;
  Result := ALine <> '';
  for I := 1 to Length(ALine) do
  begin
    if IsHex(ALine[I]) then
      HasHex := True
    else if not CharInSet(ALine[I], [' ', ':']) then
      Exit(False);
  end;
  Result := Result and HasHex;
end;

procedure AppendFrame(var AFrames: TELMFrames; AId: Cardinal;
  const AData: TBytes);
var
  L: Integer;
begin
  L := Length(AFrames);
  SetLength(AFrames, L + 1);
  AFrames[L].Id := AId;
  AFrames[L].Data := Copy(AData);
end;

procedure PadToEight(var AData: TBytes);
begin
  while Length(AData) < 8 do
  begin
    SetLength(AData, Length(AData) + 1);
    AData[High(AData)] := 0;
  end;
end;

function BuildIsoTpFrames(AId: Cardinal; const APayload: TBytes;
  APadSingle: Boolean): TELMFrames;
var
  Frame: TBytes;
  Offset, Take, Seq, I: Integer;
begin
  SetLength(Result, 0);
  if Length(APayload) <= 7 then
  begin
    SetLength(Frame, Length(APayload) + 1);
    Frame[0] := Byte(Length(APayload));
    for I := 0 to High(APayload) do
      Frame[I + 1] := APayload[I];
    if APadSingle then
      PadToEight(Frame);
    AppendFrame(Result, AId, Frame);
    Exit;
  end;

  SetLength(Frame, 8);
  Frame[0] := $10 or Byte((Length(APayload) shr 8) and $0F);
  Frame[1] := Byte(Length(APayload) and $FF);
  for I := 0 to 5 do
    Frame[I + 2] := APayload[I];
  AppendFrame(Result, AId, Frame);

  Offset := 6;
  Seq := 1;
  while Offset < Length(APayload) do
  begin
    SetLength(Frame, 8);
    Frame[0] := $20 or Byte(Seq and $0F);
    Take := Length(APayload) - Offset;
    if Take > 7 then
      Take := 7;
    for I := 0 to 6 do
      if I < Take then
        Frame[I + 1] := APayload[Offset + I]
      else
        Frame[I + 1] := 0;
    AppendFrame(Result, AId, Frame);
    Inc(Offset, Take);
    Seq := (Seq + 1) and $0F;
  end;
end;

function HexBytesToBytes(const AText: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, 0);
  I := 1;
  while I <= Length(AText) do
  begin
    while (I <= Length(AText)) and not IsHex(AText[I]) do
      Inc(I);
    if I > Length(AText) then
      Break;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := HexNibble(AText[I]) shl 4;
    Inc(I);
    if (I > Length(AText)) or not IsHex(AText[I]) then
    begin
      SetLength(Result, Length(Result) - 1);
      Break;
    end;
    Result[High(Result)] := Result[High(Result)] or HexNibble(AText[I]);
    Inc(I);
  end;
end;

{ TOBDELM327Emulator }

constructor TOBDELM327Emulator.Create;
begin
  inherited Create;
  FVehicle := TOBDSimVehicle.Create;
  FOwnsVehicle := True;
  FPersona := epELM327v23;
  ResetAdapter;
end;

destructor TOBDELM327Emulator.Destroy;
begin
  if FOwnsVehicle then
    FVehicle.Free;
  inherited;
end;

procedure TOBDELM327Emulator.SetVehicle(AValue: TOBDSimVehicle);
begin
  if FVehicle = AValue then
    Exit;
  if FOwnsVehicle then
    FVehicle.Free;
  FVehicle := AValue;
  FOwnsVehicle := False;
  if FVehicle = nil then
  begin
    FVehicle := TOBDSimVehicle.Create;
    FOwnsVehicle := True;
  end;
end;

procedure TOBDELM327Emulator.ResetAdapter;
begin
  FEcho := True;
  FLineFeeds := False;
  FSpaces := True;
  FHeaders := False;
  FCanAutoFormatting := True;
  FProtocol := '0';
  FAutoProtocol := True;
  FSearchPending := False;
  FHeader := OBD_SIM_FUNCTIONAL_REQUEST_ID;
  FReceiveFilter := '';
  FExtendedAddress := '';
  FAdaptiveTiming := 1;
  FTimeoutByte := $32;
end;

function TOBDELM327Emulator.Banner: string;
begin
  case FPersona of
    epELM327v15Clone:
      Result := 'ELM327 v1.5';
    epOBDLinkSTN:
      Result := 'OBDLink SX';
  else
    Result := 'ELM327 v2.3';
  end;
end;

function TOBDELM327Emulator.DeviceDescription: string;
begin
  case FPersona of
    epOBDLinkSTN:
      Result := 'OBDLink STN2230';
    epELM327v15Clone:
      Result := 'OBDII to RS232 Interpreter';
  else
    Result := 'OBDII to RS232 Interpreter';
  end;
end;

function TOBDELM327Emulator.EOL: string;
begin
  if FLineFeeds then
    Result := #13#10
  else
    Result := #13;
end;

function TOBDELM327Emulator.NormalizeCommand(const ACommand: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(ACommand) do
    if not CharInSet(ACommand[I], [#0 .. #32]) then
      Result := Result + UpCase(ACommand[I]);
end;

function TOBDELM327Emulator.ParseHexCommand(const ACommand: string;
  out ABytes: TBytes): Boolean;
var
  I: Integer;
begin
  Result := False;
  SetLength(ABytes, 0);
  if (ACommand = '') or ((Length(ACommand) mod 2) <> 0) then
    Exit;
  for I := 1 to Length(ACommand) do
    if not IsHex(ACommand[I]) then
      Exit;
  ABytes := HexBytesToBytes(ACommand);
  Result := Length(ABytes) > 0;
end;

function TOBDELM327Emulator.ParseHexId(const AText: string;
  out AId: Cardinal): Boolean;
var
  V: Int64;
begin
  Result := (AText <> '') and TryStrToInt64('$' + AText, V);
  if Result then
    AId := Cardinal(V)
  else
    AId := 0;
end;

function TOBDELM327Emulator.FormatId(AId: Cardinal): string;
begin
  if AId > $7FF then
    Result := IntToHex(AId, 8)
  else
    Result := IntToHex(AId, 3);
end;

function TOBDELM327Emulator.FilterAllows(AResponseId: Cardinal): Boolean;
var
  FilterId: Cardinal;
begin
  Result := True;
  if FReceiveFilter = '' then
    Exit;
  if ParseHexId(FReceiveFilter, FilterId) then
    Result := FilterId = AResponseId
  else
    Result := False;
end;

function TOBDELM327Emulator.ApplySpaceSetting(const ALine: string): string;
begin
  if FSpaces or not IsFrameLikeLine(ALine) then
    Result := ALine
  else
    Result := BytesNoSpaces(ALine);
end;

function TOBDELM327Emulator.BuildOutput(const ACommand: string; ADoEcho: Boolean;
  ALines: TStrings): string;
var
  I: Integer;
  Sep: string;
begin
  Sep := EOL;
  Result := '';
  if ADoEcho then
    Result := Trim(ACommand) + Sep;
  for I := 0 to ALines.Count - 1 do
    Result := Result + ApplySpaceSetting(ALines[I]) + Sep;
  Result := Result + '>';
end;

function TOBDELM327Emulator.Execute(const ACommand: string): string;
var
  Lines: TStringList;
  Normalized: string;
  DoEcho: Boolean;
begin
  Lines := TStringList.Create;
  try
    DoEcho := FEcho;
    Normalized := NormalizeCommand(ACommand);
    if Normalized = '' then
      ;
    if Copy(Normalized, 1, 2) = 'AT' then
      ExecuteAT(Normalized, Lines)
    else if Copy(Normalized, 1, 2) = 'ST' then
      ExecuteST(Normalized, Lines)
    else
      ExecuteOBD(Normalized, Lines);
    Result := BuildOutput(ACommand, DoEcho, Lines);
  finally
    Lines.Free;
  end;
end;

procedure TOBDELM327Emulator.ExecuteAT(const ANormalized: string;
  ALines: TStrings);
var
  Arg: string;
  Id: Cardinal;
  FS: TFormatSettings;
begin
  if ANormalized = 'ATZ' then
  begin
    ResetAdapter;
    ALines.Add(Banner);
  end
  else if ANormalized = 'ATWS' then
  begin
    ResetAdapter;
    ALines.Add(Banner);
  end
  else if ANormalized = 'ATI' then
    ALines.Add(Banner)
  else if ANormalized = 'AT@1' then
    ALines.Add(DeviceDescription)
  else if ANormalized = 'ATRV' then
  begin
    FS := InvariantFloatFormatSettings;
    ALines.Add(Format('%.1fV', [FVehicle.Value(OBD_SIM_PID_MODULE_VOLTAGE)], FS));
  end
  else if ANormalized = 'ATD' then
  begin
    ResetAdapter;
    ALines.Add('OK');
  end
  else if (ANormalized = 'ATE0') or (ANormalized = 'ATE1') then
  begin
    FEcho := ANormalized = 'ATE1';
    ALines.Add('OK');
  end
  else if (ANormalized = 'ATL0') or (ANormalized = 'ATL1') then
  begin
    FLineFeeds := ANormalized = 'ATL1';
    ALines.Add('OK');
  end
  else if (ANormalized = 'ATS0') or (ANormalized = 'ATS1') then
  begin
    FSpaces := ANormalized = 'ATS1';
    ALines.Add('OK');
  end
  else if (ANormalized = 'ATH0') or (ANormalized = 'ATH1') then
  begin
    FHeaders := ANormalized = 'ATH1';
    ALines.Add('OK');
  end
  else if (ANormalized = 'ATCAF0') or (ANormalized = 'ATCAF1') then
  begin
    FCanAutoFormatting := ANormalized = 'ATCAF1';
    ALines.Add('OK');
  end
  else if Copy(ANormalized, 1, 4) = 'ATSP' then
  begin
    Arg := Copy(ANormalized, 5, MaxInt);
    if (Arg = '0') or (Arg = '6') then
    begin
      FProtocol := Arg;
      FAutoProtocol := Arg = '0';
      FSearchPending := FAutoProtocol;
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if Copy(ANormalized, 1, 4) = 'ATTP' then
  begin
    Arg := Copy(ANormalized, 5, MaxInt);
    if (Arg = '0') or (Arg = '6') then
    begin
      FProtocol := Arg;
      FAutoProtocol := Arg = '0';
      FSearchPending := FAutoProtocol;
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if ANormalized = 'ATDP' then
  begin
    if FAutoProtocol then
      ALines.Add('AUTO, ISO 15765-4 (CAN 11/500)')
    else
      ALines.Add('ISO 15765-4 (CAN 11/500)');
  end
  else if ANormalized = 'ATDPN' then
  begin
    if FAutoProtocol then
      ALines.Add('A6')
    else
      ALines.Add('6');
  end
  else if Copy(ANormalized, 1, 4) = 'ATSH' then
  begin
    Arg := Copy(ANormalized, 5, MaxInt);
    if ParseHexId(Arg, Id) then
    begin
      FHeader := Id;
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if Copy(ANormalized, 1, 5) = 'ATCRA' then
  begin
    Arg := Copy(ANormalized, 6, MaxInt);
    if Arg = '' then
    begin
      FReceiveFilter := '';
      ALines.Add('OK');
    end
    else if ParseHexId(Arg, Id) then
    begin
      FReceiveFilter := Arg;
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if Copy(ANormalized, 1, 5) = 'ATCEA' then
  begin
    FExtendedAddress := Copy(ANormalized, 6, MaxInt);
    ALines.Add('OK');
  end
  else if Copy(ANormalized, 1, 4) = 'ATST' then
  begin
    Arg := Copy(ANormalized, 5, MaxInt);
    if (Length(Arg) = 2) and ParseHexId(Arg, Id) then
    begin
      FTimeoutByte := Byte(Id and $FF);
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if Copy(ANormalized, 1, 4) = 'ATAT' then
  begin
    Arg := Copy(ANormalized, 5, MaxInt);
    if (Arg = '0') or (Arg = '1') or (Arg = '2') then
    begin
      FAdaptiveTiming := Byte(Ord(Arg[1]) - Ord('0'));
      ALines.Add('OK');
    end
    else
      ALines.Add('?');
  end
  else if (ANormalized = 'ATAL') or (ANormalized = 'ATPC') or
    (ANormalized = 'ATIB10') or (ANormalized = 'ATIB96') then
    ALines.Add('OK')
  else
    ALines.Add('?');
end;

procedure TOBDELM327Emulator.ExecuteST(const ANormalized: string;
  ALines: TStrings);
begin
  if FPersona <> epOBDLinkSTN then
  begin
    ALines.Add('?');
    Exit;
  end;
  if ANormalized = 'STI' then
    ALines.Add('STN2230 v5.6.19')
  else if ANormalized = 'STDI' then
    ALines.Add('OBDLink SX USB')
  else if ANormalized = 'STSLCS' then
    ALines.Add('OK')
  else
    ALines.Add('?');
end;

procedure TOBDELM327Emulator.ExecuteOBD(const ANormalized: string;
  ALines: TStrings);
var
  Request: TBytes;
  Responses: TOBDSimECUResponses;
begin
  if not ParseHexCommand(ANormalized, Request) then
  begin
    ALines.Add('?');
    Exit;
  end;
  if FSearchPending then
  begin
    ALines.Add('SEARCHING...');
    FSearchPending := False;
    FProtocol := '6';
  end;
  Responses := FVehicle.HandleRequest(FHeader, Request);
  AddFormattedResponses(Responses, ALines);
end;

procedure TOBDELM327Emulator.AddFormattedResponses(
  const AResponses: TOBDSimECUResponses; ALines: TStrings);
var
  I, Shown: Integer;
begin
  Shown := 0;
  for I := 0 to High(AResponses) do
  begin
    if not FilterAllows(AResponses[I].ResponseId) then
      Continue;
    Inc(Shown);
    if FCanAutoFormatting and (not FHeaders) and (Length(AResponses[I].Payload) > 7) then
      AddCAF1PayloadBlock(AResponses[I].Payload, ALines)
    else if FCanAutoFormatting and (not FHeaders) then
      ALines.Add(OBDBytesToHex(AResponses[I].Payload))
    else
      AddCANFrames(AResponses[I].ResponseId, AResponses[I].Payload, ALines);
  end;
  if Shown = 0 then
    ALines.Add('NO DATA');
end;

procedure TOBDELM327Emulator.AddCAF1PayloadBlock(const APayload: TBytes;
  ALines: TStrings);
var
  Offset, Take, I, Index: Integer;
  Chunk: TBytes;
begin
  ALines.Add(IntToHex(Length(APayload), 3));
  Offset := 0;
  Index := 0;
  while Offset < Length(APayload) do
  begin
    if Offset = 0 then
      Take := 6
    else
      Take := 7;
    if Take > Length(APayload) - Offset then
      Take := Length(APayload) - Offset;
    SetLength(Chunk, Take);
    for I := 0 to Take - 1 do
      Chunk[I] := APayload[Offset + I];
    if Offset > 0 then
      while Length(Chunk) < 7 do
      begin
        SetLength(Chunk, Length(Chunk) + 1);
        Chunk[High(Chunk)] := 0;
      end;
    ALines.Add(IntToHex(Index and $0F, 1) + ': ' + OBDBytesToHex(Chunk));
    Inc(Offset, Take);
    Inc(Index);
  end;
end;

procedure TOBDELM327Emulator.AddCANFrames(AResponseId: Cardinal;
  const APayload: TBytes; ALines: TStrings);
var
  Frames: TELMFrames;
  I: Integer;
  Line: string;
begin
  Frames := BuildIsoTpFrames(AResponseId, APayload, not FCanAutoFormatting);
  for I := 0 to High(Frames) do
  begin
    Line := '';
    if FHeaders then
      Line := FormatId(Frames[I].Id) + ' ';
    Line := Line + OBDBytesToHex(Frames[I].Data);
    ALines.Add(Line);
  end;
end;

{ TOBDELM327EmulatorTransport }

constructor TOBDELM327EmulatorTransport.Create;
begin
  inherited Create;
  FEmulator := TOBDELM327Emulator.Create;
  FOwnsEmulator := True;
  FResponseDelayMs := 15;
  FWorkerLock := TCriticalSection.Create;
end;

destructor TOBDELM327EmulatorTransport.Destroy;
begin
  FKeepOpenOnClose := False;
  Close;
  WaitForWorker;
  if FOwnsEmulator then
    FEmulator.Free;
  FWorkerLock.Free;
  inherited;
end;

procedure TOBDELM327EmulatorTransport.SetEmulator(AValue: TOBDELM327Emulator);
begin
  if FEmulator = AValue then
    Exit;
  if FOwnsEmulator then
    FEmulator.Free;
  FEmulator := AValue;
  FOwnsEmulator := False;
  if FEmulator = nil then
  begin
    FEmulator := TOBDELM327Emulator.Create;
    FOwnsEmulator := True;
  end;
end;

procedure TOBDELM327EmulatorTransport.Open;
begin
  FWorkerLock.Enter;
  try
    FClosing := False;
  finally
    FWorkerLock.Leave;
  end;
  if State = csOpen then
    Exit;
  SetState(csOpening);
  SetState(csOpen);
end;

procedure TOBDELM327EmulatorTransport.WaitForWorker;
var
  Worker: TThread;
begin
  Worker := nil;
  FWorkerLock.Enter;
  try
    Worker := FWorker;
    FWorker := nil;
  finally
    FWorkerLock.Leave;
  end;
  if Worker = nil then
    Exit;
  Worker.WaitFor;
  Worker.Free;
end;

procedure TOBDELM327EmulatorTransport.DeliverBytes(const ABytes: TBytes);
begin
  FireBytes(ABytes);
end;

procedure TOBDELM327EmulatorTransport.Close;
begin
  FWorkerLock.Enter;
  try
    FClosing := True;
  finally
    FWorkerLock.Leave;
  end;
  if FKeepOpenOnClose then
  begin
    WaitForWorker;
    Exit;
  end;
  if State = csClosed then
  begin
    WaitForWorker;
    Exit;
  end;
  SetState(csClosing);
  WaitForWorker;
  SetState(csClosed);
end;

procedure TOBDELM327EmulatorTransport.StartResponder(const AText: string);
var
  Self_: TOBDELM327EmulatorTransport;
  TextCopy: string;
  DelayCopy: Cardinal;
begin
  WaitForWorker;
  Self_ := Self;
  TextCopy := AText;
  DelayCopy := FResponseDelayMs;
  FWorkerLock.Enter;
  try
    if FClosing then
      Exit;
    FWorker := TThread.CreateAnonymousThread(
      procedure
      begin
        if DelayCopy > 0 then
          TThread.Sleep(DelayCopy);
        if Self_.State = csOpen then
          Self_.DeliverBytes(TEncoding.ASCII.GetBytes(TextCopy));
      end);
    FWorker.FreeOnTerminate := False;
    FWorker.Start;
  finally
    FWorkerLock.Leave;
  end;
end;

function TOBDELM327EmulatorTransport.WriteBytes(const ABytes: TBytes): Integer;
var
  Text: string;
  P: Integer;
  Command: string;
begin
  if State <> csOpen then
    raise EOBDNotConnected.Create('ELM327 emulator transport is not open');
  Result := Length(ABytes);
  if Result = 0 then
    Exit;
  Text := TEncoding.ASCII.GetString(ABytes);
  FBuffer := FBuffer + Text;
  P := Pos(#13, FBuffer);
  while P > 0 do
  begin
    Command := Copy(FBuffer, 1, P - 1);
    Delete(FBuffer, 1, P);
    StartResponder(FEmulator.Execute(Command));
    P := Pos(#13, FBuffer);
  end;
end;

{ TOBDEmulatorConnection }

constructor TOBDEmulatorConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransportObject := TOBDELM327EmulatorTransport.Create;
  FTransportObject.KeepOpenOnClose := True;
  FTransportObject.Open;
  FTransportIntf := FTransportObject as IOBDConnectionTransport;
  CustomTransport := FTransportIntf;
end;

destructor TOBDEmulatorConnection.Destroy;
begin
  if FTransportObject <> nil then
  begin
    FTransportObject.KeepOpenOnClose := False;
    FTransportObject.Close;
  end;
  inherited;
  FTransportIntf := nil;
  FTransportObject := nil;
end;

procedure TOBDEmulatorConnection.Open;
begin
  if FTransportObject <> nil then
    FTransportObject.Open;
  inherited Open;
end;

function TOBDEmulatorConnection.GetPersona: TOBDELM327Persona;
begin
  Result := FTransportObject.Emulator.Persona;
end;

procedure TOBDEmulatorConnection.SetPersona(AValue: TOBDELM327Persona);
begin
  FTransportObject.Emulator.Persona := AValue;
end;

function TOBDEmulatorConnection.GetResponseDelayMs: Cardinal;
begin
  Result := FTransportObject.ResponseDelayMs;
end;

procedure TOBDEmulatorConnection.SetResponseDelayMs(AValue: Cardinal);
begin
  FTransportObject.ResponseDelayMs := AValue;
end;

function TOBDEmulatorConnection.GetEnableFaults: Boolean;
begin
  Result := FTransportObject.Emulator.Vehicle.EgrFault;
end;

procedure TOBDEmulatorConnection.SetEnableFaults(AValue: Boolean);
begin
  FTransportObject.Emulator.Vehicle.EgrFault := AValue;
end;

function TOBDEmulatorConnection.GetEnableJ1979DIDs: Boolean;
begin
  Result := FTransportObject.Emulator.Vehicle.EnableJ1979DIDs;
end;

procedure TOBDEmulatorConnection.SetEnableJ1979DIDs(AValue: Boolean);
begin
  FTransportObject.Emulator.Vehicle.EnableJ1979DIDs := AValue;
end;

function TOBDEmulatorConnection.GetVehicleProfile: TOBDSimVehicleProfile;
begin
  Result := FTransportObject.Emulator.Vehicle.Profile;
end;

procedure TOBDEmulatorConnection.SetVehicleProfile(AValue: TOBDSimVehicleProfile);
begin
  if AValue <> svpGolf7TDI then
    raise EOBDConfig.Create('Only the Golf VII TDI simulator profile is implemented');
  FTransportObject.Emulator.Vehicle.Reset;
end;

end.
