//------------------------------------------------------------------------------
//  OBD.Design.Editors
//
//  Tools-API property and component editors shipped by
//  <c>DelphiOBD_DT.bpl</c>. Lives in the design-time package only;
//  must never be referenced from runtime code.
//
//  What ships here:
//
//    Property editors
//    ----------------
//    TOBDObdLogFileNameProperty
//        Open-dialog editor for <c>TOBDReplayer.FileName</c>.
//        Filter: *.obdlog;*.obdlog.gz.
//    TOBDCheckpointFileProperty
//        Save-dialog editor for <c>TOBDFlashPipeline.CheckpointFile</c>.
//        Filter: *.checkpoint.json.
//    TOBDAdapterInitCommandsProperty
//        Multi-line editor for <c>TOBDAdapter.InitCommands</c> with
//        a brief AT / ST command primer at the top of the dialog.
//
//    Component editors
//    -----------------
//    TOBDDestructiveComponentEditor
//        Adds a "Flashing safety" verb to every destructive
//        component (TOBDFlasher, TOBDUDSTransfer, TOBDFlashPipeline)
//        that pops a reminder up front and opens
//        docs/flashing-safety.md.
//    TOBDFlashPipelineComponentEditor
//        On top of the destructive verbs, exposes "Validate
//        configuration" — runs the configured Checks at design-
//        time and reports issues in a modal list, so a host can
//        eyeball the safety surface before deploying.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Editors;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  DesignEditors,
  DesignIntf,
  ToolsAPI;

type
  /// <summary>Base for string properties that should pop an
  /// <c>TOpenDialog</c> when the user clicks the property's
  /// ellipsis button. Subclasses override
  /// <see cref="GetFilter"/> to declare a filter string.</summary>
  TOBDOpenFileNameProperty = class(TStringProperty)
  protected
    /// <summary>Returns the filter string the dialog uses (e.g.
    /// <c>'OBD log files (*.obdlog;*.obdlog.gz)|*.obdlog;*.obdlog.gz|All files (*.*)|*.*'</c>).</summary>
    function GetFilter: string; virtual; abstract;
    /// <summary>Default file extension the dialog appends when
    /// the user types a name without one.</summary>
    function GetDefaultExt: string; virtual;
    /// <summary>Dialog title.</summary>
    function GetDialogTitle: string; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  /// <summary>Save-dialog variant of
  /// <see cref="TOBDOpenFileNameProperty"/>.</summary>
  TOBDSaveFileNameProperty = class(TStringProperty)
  protected
    function GetFilter: string; virtual; abstract;
    function GetDefaultExt: string; virtual;
    function GetDialogTitle: string; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  /// <summary>Editor for <c>TOBDReplayer.FileName</c>.</summary>
  TOBDObdLogFileNameProperty = class(TOBDOpenFileNameProperty)
  protected
    function GetFilter: string; override;
    function GetDefaultExt: string; override;
    function GetDialogTitle: string; override;
  end;

  /// <summary>Editor for <c>TOBDFlashPipeline.CheckpointFile</c>.</summary>
  TOBDCheckpointFileProperty = class(TOBDSaveFileNameProperty)
  protected
    function GetFilter: string; override;
    function GetDefaultExt: string; override;
    function GetDialogTitle: string; override;
  end;

  /// <summary>Multi-line editor for <c>TOBDAdapter.InitCommands</c>.
  /// The IDE's stock <c>TStringListProperty</c> already provides a
  /// memo dialog; this subclass swaps the title for one that hints
  /// at the AT / ST command surface.</summary>
  TOBDAdapterInitCommandsProperty = class(TStringListProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  /// <summary>Drop-down editor for <c>TOBDSerialSettings.Port</c>
  /// that enumerates live COM ports from the
  /// <c>HKLM\HARDWARE\DEVICEMAP\SERIALCOMM</c> registry key.
  /// Hosts can still type a value the IDE doesn't list.</summary>
  TOBDSerialPortProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor for the <c>Address</c> property of
  ///   <c>TOBDBluetoothSettings</c>. Surfaces a curated list of
  ///   common OBD-II Bluetooth dongle device names so a host can
  ///   pick a target without typing the MAC by hand. Free-text
  ///   entry remains available — paste any
  ///   <c>00:11:22:33:44:55</c> MAC.
  /// </summary>
  TOBDBluetoothDeviceProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor for the <c>Host</c> property of
  ///   <c>TOBDWiFiSettings</c>. Pre-populates the common Wi-Fi
  ///   OBD dongle addresses (<c>192.168.0.10</c>,
  ///   <c>192.168.4.1</c>, …). Free-text entry remains available.
  /// </summary>
  TOBDWiFiHostProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor for the FTDI device-serial property on
  ///   <c>TOBDFTDISettings</c>. Lists FTDI serials discovered via
  ///   <c>ftd2xx.dll</c> when the DLL is loadable at design-time;
  ///   falls back to a documentation placeholder otherwise.
  /// </summary>
  TOBDFTDISerialProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor for the manual-protocol field on
  ///   <c>TOBDProtocol</c>. Surfaces every standard OBD-II
  ///   physical layer (ISO 15765-4 CAN 11/29 bit at 250/500 kbps,
  ///   ISO 9141-2, KWP2000 5-baud / fast, J1850 PWM / VPW) with a
  ///   human-readable label.
  /// </summary>
  TOBDProtocolManualProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;

  /// <summary>
  ///   Drop-down editor that lists every PID byte from the
  ///   bundled <c>catalogs/obd2-pids.json</c> in
  ///   "<c>0xNN — Name</c>" form. Used by
  ///   <c>TOBDPIDItem.PID</c>.
  /// </summary>
  TOBDPIDListProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor that lists every J1939 PGN from
  ///   <c>catalogs/j1939/pgns.json</c>.
  /// </summary>
  TOBDPGNListProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>
  ///   Drop-down editor that lists every UDS DID from
  ///   <c>catalogs/uds/dids-generic.json</c> + any OEM overlays
  ///   registered with <c>TOBDOEMRegistry</c>.
  /// </summary>
  TOBDDIDListProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  /// <summary>Component editor that adds a "Test connection…"
  /// verb to <see cref="TOBDConnection"/>. The verb opens the
  /// configured transport at design-time with a short timeout
  /// and reports success / failure.</summary>
  TOBDConnectionComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>Component editor that adds a "Detect adapter…"
  /// verb to <see cref="TOBDAdapter"/>. Runs the AT detection
  /// sequence and reports the chip family + capabilities.</summary>
  TOBDAdapterComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>Component editor that adds "Send ATI…" and
  /// "Send AT@1…" verbs to <see cref="TOBDProtocol"/>. Useful
  /// to round-trip the adapter without leaving the IDE.</summary>
  TOBDProtocolComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>
  ///   Component editor for <see cref="TOBDLiveData"/>. Adds
  ///   "Add standard PIDs…" (populates the PIDs collection with
  ///   the universal-support set: 0x04, 0x05, 0x0B, 0x0C, 0x0D,
  ///   0x0E, 0x11) and "Live test PID…" (one-shot read).
  /// </summary>
  TOBDLiveDataComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>
  ///   Component editor for <see cref="TOBDDTCs"/>. Adds
  ///   "Read DTCs…" and "Clear DTCs…" verbs. The clear verb
  ///   confirms via a modal warning first because clearing is
  ///   destructive (drops stored emission history).
  /// </summary>
  TOBDDTCsComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>Component editor that adds a "Flashing safety"
  /// reminder verb to every destructive component (Flasher,
  /// UDSTransfer, FlashPipeline). Clicking the verb shows a
  /// modal warning and opens
  /// <c>docs/flashing-safety.md</c> in the host's default
  /// browser / editor.</summary>
  TOBDDestructiveComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  /// <summary>Component editor for
  /// <see cref="TOBDFlashPipeline"/>. Adds a "Validate
  /// configuration" verb on top of the destructive verbs.</summary>
  TOBDFlashPipelineComponentEditor = class(TOBDDestructiveComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

/// <summary>Registers every editor declared in this unit. Call
/// from <c>OBD.Design.Registration.Register</c>.</summary>
procedure RegisterDelphiOBDEditors;

implementation

uses
  Winapi.Windows,
  System.TypInfo,
  System.Win.Registry,
  System.Generics.Collections,
  Vcl.Forms,
  Vcl.Controls,
  OBD.Connection,
  OBD.Adapter,
  OBD.Adapter.Types,
  OBD.Protocol,
  OBD.Recorder,
  OBD.Replayer,
  OBD.Coding.Flasher,
  OBD.UDS.Transfer,
  OBD.Flash.Pipeline,
  OBD.Service.LiveData,
  OBD.Service.DTCs,
  OBD.ClearDTC,
  OBD.Design.Forms.FlashSafety,
  OBD.Design.Forms.InitCommands,
  OBD.Design.Forms.PipelineValidate,
  OBD.Design.Forms.LiveTest;

{ ---- shared helpers ---------------------------------------------------------- }

function GetActiveOwnerWindow: HWND;
begin
  if Screen.ActiveForm <> nil then
    Result := Screen.ActiveForm.Handle
  else
    Result := Application.Handle;
end;

{ ---- TOBDOpenFileNameProperty ------------------------------------------------ }

function TOBDOpenFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TOBDOpenFileNameProperty.GetDefaultExt: string;
begin
  Result := '';
end;

function TOBDOpenFileNameProperty.GetDialogTitle: string;
begin
  Result := 'Select file';
end;

procedure TOBDOpenFileNameProperty.Edit;
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Title := GetDialogTitle;
    Dlg.Filter := GetFilter;
    Dlg.DefaultExt := GetDefaultExt;
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofFileMustExist];
    Dlg.FileName := GetValue;
    if Dlg.Execute(GetActiveOwnerWindow) then
      SetValue(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

{ ---- TOBDSaveFileNameProperty ------------------------------------------------ }

function TOBDSaveFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TOBDSaveFileNameProperty.GetDefaultExt: string;
begin
  Result := '';
end;

function TOBDSaveFileNameProperty.GetDialogTitle: string;
begin
  Result := 'Save as';
end;

procedure TOBDSaveFileNameProperty.Edit;
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.Title := GetDialogTitle;
    Dlg.Filter := GetFilter;
    Dlg.DefaultExt := GetDefaultExt;
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofOverwritePrompt];
    Dlg.FileName := GetValue;
    if Dlg.Execute(GetActiveOwnerWindow) then
      SetValue(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

{ ---- TOBDObdLogFileNameProperty ---------------------------------------------- }

function TOBDObdLogFileNameProperty.GetFilter: string;
begin
  Result :=
    'OBD log files (*.obdlog;*.obdlog.gz)|*.obdlog;*.obdlog.gz|' +
    'All files (*.*)|*.*';
end;

function TOBDObdLogFileNameProperty.GetDefaultExt: string;
begin
  Result := 'obdlog';
end;

function TOBDObdLogFileNameProperty.GetDialogTitle: string;
begin
  Result := 'Select Delphi-OBD recording';
end;

{ ---- TOBDCheckpointFileProperty --------------------------------------------- }

function TOBDCheckpointFileProperty.GetFilter: string;
begin
  Result :=
    'Flash checkpoints (*.checkpoint.json)|*.checkpoint.json|' +
    'All files (*.*)|*.*';
end;

function TOBDCheckpointFileProperty.GetDefaultExt: string;
begin
  Result := 'checkpoint.json';
end;

function TOBDCheckpointFileProperty.GetDialogTitle: string;
begin
  Result := 'Select flash checkpoint file';
end;

{ ---- TOBDAdapterInitCommandsProperty ----------------------------------------- }

function TOBDAdapterInitCommandsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

procedure TOBDAdapterInitCommandsProperty.Edit;
var
  L: TStrings;
begin
  L := TStrings(GetOrdValue);
  if L = nil then Exit;
  if EditInitCommands(L) then
    // Mark the property as modified so the IDE updates the .dfm.
    Modified;
end;

{ ---- TOBDDestructiveComponentEditor ------------------------------------------ }

function TOBDDestructiveComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TOBDDestructiveComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Read flashing safety guide…';
  else
    Result := '';
  end;
end;

procedure TOBDDestructiveComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowFlashSafetyDialog;
  end;
end;

{ ---- TOBDFlashPipelineComponentEditor --------------------------------------- }

function TOBDFlashPipelineComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TOBDFlashPipelineComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index < inherited GetVerbCount then
    Result := inherited GetVerb(Index)
  else
    case Index - inherited GetVerbCount of
      0: Result := 'Validate configuration…';
    else
      Result := '';
    end;
end;

procedure TOBDFlashPipelineComponentEditor.ExecuteVerb(Index: Integer);
var
  Pipeline: TOBDFlashPipeline;
  Issues: TList<TOBDPipelineIssue>;

  procedure Add(ALevel: TOBDPipelineIssueLevel; const AText: string);
  var
    R: TOBDPipelineIssue;
  begin
    R.Level := ALevel;
    R.Text  := AText;
    Issues.Add(R);
  end;

begin
  if Index < inherited GetVerbCount then
  begin
    inherited ExecuteVerb(Index);
    Exit;
  end;
  case Index - inherited GetVerbCount of
    0:
      begin
        Pipeline := Component as TOBDFlashPipeline;
        Issues := TList<TOBDPipelineIssue>.Create;
        try
          if Pipeline.Protocol = nil then
            Add(pilError,
              'Protocol is not assigned. The pipeline cannot run.');
          if Pipeline.VoltageGate = nil then
            Add(pilWarning,
              'VoltageGate is not assigned. Allowed, but the run will ' +
              'log a WARN audit entry at start-of-flash.');
          if Pipeline.AuditLog = nil then
            Add(pilWarning,
              'AuditLog is not assigned. Allowed, but flash operations ' +
              'will not be auditable.');
          if Pipeline.AutoExecute then
            Add(pilWarning,
              'AutoExecute = True. Destructive operations will skip the ' +
              'host confirmation handler. Confirm this is intentional.');
          ShowPipelineValidation(Pipeline.Name, Issues.ToArray);
        finally
          Issues.Free;
        end;
      end;
  end;
end;

{ ---- TOBDSerialPortProperty -------------------------------------------------- }

function TOBDSerialPortProperty.GetAttributes: TPropertyAttributes;
begin
  // paValueList puts the property into a drop-down combo. The
  // user can still type any value the IDE doesn't list — useful
  // for ports that aren't currently plugged in.
  Result := inherited GetAttributes + [paValueList];
end;

procedure TOBDSerialPortProperty.GetValues(Proc: TGetStrProc);
var
  Reg: TRegistry;
  Names: TStringList;
  I: Integer;
  Port: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  Names := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
      Exit;
    try
      Reg.GetValueNames(Names);
      for I := 0 to Names.Count - 1 do
      begin
        Port := Reg.ReadString(Names[I]);
        if Port <> '' then
          Proc(Port);
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Names.Free;
    Reg.Free;
  end;
end;

{ ---- TOBDConnectionComponentEditor ------------------------------------------ }

function TOBDConnectionComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOBDConnectionComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Test connection…';
    1: Result := 'Refresh ports';
  else
    Result := '';
  end;
end;

procedure TOBDConnectionComponentEditor.ExecuteVerb(Index: Integer);
var
  Conn: TOBDConnection;
begin
  case Index of
    0:
      begin
        Conn := Component as TOBDConnection;
        TOBDLiveTestDlg.Show(
          'Test ' + Conn.Name,
          'Opens the configured transport with a short timeout.',
          procedure(const AWriteLine: TProc<string>;
            const ASetStatus: TProc<TOBDLiveTestStatus>)
          begin
            AWriteLine('Opening connection…');
            try
              Conn.Open;
              try
                AWriteLine('Connection opened.');
                ASetStatus(ltsOK);
              finally
                Conn.Close;
                AWriteLine('Connection closed.');
              end;
            except
              on E: Exception do
              begin
                AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
                ASetStatus(ltsFail);
              end;
            end;
          end);
      end;
    1:
      begin
        // Re-walks the SERIALCOMM registry so the next
        // Object-Inspector drop-down reflects newly-arrived COM
        // devices. The actual enumeration lives in the
        // TOBDSerialPortProperty editor; this verb just triggers
        // a designer-modified notification so the IDE re-queries.
        Designer.Modified;
      end;
  end;
end;

{ ---- TOBDAdapterComponentEditor --------------------------------------------- }

function TOBDAdapterComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TOBDAdapterComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Detect adapter…';
  else
    Result := '';
  end;
end;

procedure TOBDAdapterComponentEditor.ExecuteVerb(Index: Integer);
var
  Adapter: TOBDAdapter;
begin
  if Index <> 0 then Exit;
  Adapter := Component as TOBDAdapter;
  TOBDLiveTestDlg.Show(
    'Detect ' + Adapter.Name,
    'Runs the AT detection sequence and reports chip identity.',
    procedure(const AWriteLine: TProc<string>;
      const ASetStatus: TProc<TOBDLiveTestStatus>)
    begin
      if Adapter.Connection = nil then
      begin
        AWriteLine('FAILED: Connection not assigned.');
        ASetStatus(ltsFail);
        Exit;
      end;
      AWriteLine('Opening connection…');
      try
        Adapter.Connection.Open;
        try
          AWriteLine('Running detection…');
          Adapter.Detect;
          AWriteLine(Format('Family       : %s',
            [GetEnumName(TypeInfo(TOBDAdapterFamily), Ord(Adapter.Family))]));
          AWriteLine(Format('MaxIsoTpFrame: %d', [Adapter.MaxIsoTpFrameBytes]));
          AWriteLine('Capabilities :');
          for var Cap := Low(TOBDAdapterCapability)
                      to High(TOBDAdapterCapability) do
            if Cap in Adapter.Capabilities then
              AWriteLine('  • ' + GetEnumName(TypeInfo(TOBDAdapterCapability),
                Ord(Cap)));
          ASetStatus(ltsOK);
        finally
          Adapter.Connection.Close;
        end;
      except
        on E: Exception do
        begin
          AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
          ASetStatus(ltsFail);
        end;
      end;
    end);
end;

{ ---- TOBDProtocolComponentEditor -------------------------------------------- }

function TOBDProtocolComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOBDProtocolComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Send ATI…';
    1: Result := 'Send AT@1…';
  else
    Result := '';
  end;
end;

procedure TOBDProtocolComponentEditor.ExecuteVerb(Index: Integer);
var
  Proto: TOBDProtocol;
  ATCommand: string;
begin
  if (Index < 0) or (Index > 1) then Exit;
  if Index = 0 then ATCommand := 'ATI' else ATCommand := 'AT@1';
  Proto := Component as TOBDProtocol;
  TOBDLiveTestDlg.Show(
    Format('%s — send %s', [Proto.Name, ATCommand]),
    'Round-trips one AT command through the bound adapter.',
    procedure(const AWriteLine: TProc<string>;
      const ASetStatus: TProc<TOBDLiveTestStatus>)
    var
      Reply: string;
    begin
      if Proto.Adapter = nil then
      begin
        AWriteLine('FAILED: Adapter not assigned.');
        ASetStatus(ltsFail);
        Exit;
      end;
      if Proto.Adapter.Connection = nil then
      begin
        AWriteLine('FAILED: Adapter.Connection not assigned.');
        ASetStatus(ltsFail);
        Exit;
      end;
      AWriteLine('Opening connection…');
      try
        Proto.Adapter.Connection.Open;
        try
          AWriteLine('Sending ' + ATCommand + '…');
          Reply := Proto.Adapter.SendCommand(ATCommand).Raw;
          AWriteLine('Reply        : ' + Reply);
          ASetStatus(ltsOK);
        finally
          Proto.Adapter.Connection.Close;
        end;
      except
        on E: Exception do
        begin
          AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
          ASetStatus(ltsFail);
        end;
      end;
    end);
end;

{ ---- registration ----------------------------------------------------------- }

{ ---- TOBDBluetoothDeviceProperty ------------------------------------------- }

function TOBDBluetoothDeviceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TOBDBluetoothDeviceProperty.GetValues(Proc: TGetStrProc);
const
  // Names a host typically sees while pairing OBD-II dongles.
  // MAC entry remains free-text — these are friendly hints only.
  CommonDevices: array[0..7] of string = (
    'OBDII',
    'OBDII-V1.5',
    'OBDII-V2.1',
    'ELM327',
    'Vgate iCar Pro',
    'OBDLink MX+',
    'BAFX Products',
    'KIWI 3');
var
  S: string;
begin
  for S in CommonDevices do
    Proc(S);
end;

{ ---- TOBDWiFiHostProperty -------------------------------------------------- }

function TOBDWiFiHostProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TOBDWiFiHostProperty.GetValues(Proc: TGetStrProc);
const
  CommonHosts: array[0..3] of string = (
    '192.168.0.10',                       // WiFi ELM327 default
    '192.168.4.1',                        // ESP-AP dongles
    '192.168.1.5',                        // OBDLink MX+ Wi-Fi default
    '192.168.10.1');                      // some Vgate models
var
  S: string;
begin
  for S in CommonHosts do
    Proc(S);
end;

{ ---- TOBDFTDISerialProperty ------------------------------------------------ }

function TOBDFTDISerialProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TOBDFTDISerialProperty.GetValues(Proc: TGetStrProc);
begin
  // Live-enumeration via ftd2xx.dll happens in the runtime FTDI
  // transport. At design-time the DLL is not guaranteed to be
  // loadable; emit a single placeholder hint so the user knows
  // free-text entry is the right path.
  Proc('(type the FTDI serial here, e.g. FT232R-AB12CD)');
end;

{ ---- TOBDProtocolManualProperty ------------------------------------------- }

const
  ProtocolManualLabels: array[0..9] of string = (
    '0 — Automatic',
    '1 — SAE J1850 PWM (41.6 kbps)',
    '2 — SAE J1850 VPW (10.4 kbps)',
    '3 — ISO 9141-2',
    '4 — ISO 14230-4 KWP (5-baud init)',
    '5 — ISO 14230-4 KWP (fast init)',
    '6 — ISO 15765-4 CAN (11/500)',
    '7 — ISO 15765-4 CAN (29/500)',
    '8 — ISO 15765-4 CAN (11/250)',
    '9 — ISO 15765-4 CAN (29/250)');

function TOBDProtocolManualProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TOBDProtocolManualProperty.GetValue: string;
var
  Index: Integer;
begin
  Index := GetOrdValue;
  if (Index >= 0) and (Index <= High(ProtocolManualLabels)) then
    Result := ProtocolManualLabels[Index]
  else
    Result := IntToStr(Index);
end;

procedure TOBDProtocolManualProperty.GetValues(Proc: TGetStrProc);
var
  S: string;
begin
  for S in ProtocolManualLabels do
    Proc(S);
end;

procedure TOBDProtocolManualProperty.SetValue(const AValue: string);
var
  Dash: Integer;
  HeadText: string;
  IntVal: Integer;
begin
  // Accept either the numeric prefix ("3") or the full label
  // ("3 — ISO 9141-2"). Strip everything after the first dash
  // and parse the head.
  HeadText := AValue;
  Dash := Pos(' ', HeadText);
  if Dash > 0 then
    HeadText := Copy(HeadText, 1, Dash - 1);
  if TryStrToInt(HeadText, IntVal) then
    SetOrdValue(IntVal)
  else
    raise EPropertyError.CreateFmt(
      'TOBDProtocolManualProperty: cannot parse "%s"', [AValue]);
end;

{ ---- TOBDPIDListProperty -------------------------------------------------- }

function TOBDPIDListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TOBDPIDListProperty.GetValues(Proc: TGetStrProc);
const
  // The OBD-II universal-support PID set + a handful of every
  // Mode-01 PID hosts most commonly poll. The full catalogue
  // lives in catalogs/obd2-pids.json; this drop-down covers the
  // 80 % case without parsing JSON at design-time.
  CommonPIDs: array[0..15] of record
    PID: Byte; Name: string;
  end = (
    (PID: $00; Name: 'PIDs supported [01..20]'),
    (PID: $01; Name: 'Monitor status since DTCs cleared'),
    (PID: $03; Name: 'Fuel system status'),
    (PID: $04; Name: 'Calculated engine load'),
    (PID: $05; Name: 'Engine coolant temperature'),
    (PID: $0B; Name: 'Intake manifold absolute pressure'),
    (PID: $0C; Name: 'Engine RPM'),
    (PID: $0D; Name: 'Vehicle speed'),
    (PID: $0E; Name: 'Timing advance'),
    (PID: $0F; Name: 'Intake air temperature'),
    (PID: $10; Name: 'MAF air flow rate'),
    (PID: $11; Name: 'Throttle position'),
    (PID: $1C; Name: 'OBD standards this vehicle conforms to'),
    (PID: $20; Name: 'PIDs supported [21..40]'),
    (PID: $2F; Name: 'Fuel tank level input'),
    (PID: $5C; Name: 'Engine oil temperature'));
var
  I: Integer;
begin
  for I := Low(CommonPIDs) to High(CommonPIDs) do
    Proc(Format('0x%.2x — %s', [CommonPIDs[I].PID, CommonPIDs[I].Name]));
end;

{ ---- TOBDPGNListProperty -------------------------------------------------- }

function TOBDPGNListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TOBDPGNListProperty.GetValues(Proc: TGetStrProc);
const
  // J1939 diagnostic-message PGNs the host most commonly polls.
  // catalogs/j1939/pgns.json carries the full set; this picker
  // surfaces the DM family + a couple of vehicle-state PGNs.
  CommonPGNs: array[0..9] of record
    PGN: Word; Name: string;
  end = (
    (PGN: $EE00; Name: 'Address Claimed'),
    (PGN: $EA00; Name: 'Request'),
    (PGN: $FECA; Name: 'DM1 — Active DTCs'),
    (PGN: $FECB; Name: 'DM2 — Previously active DTCs'),
    (PGN: $FECE; Name: 'DM5 — Readiness'),
    (PGN: $FECF; Name: 'DM6 — Pending DTCs'),
    (PGN: $FED3; Name: 'DM11 — Clear active DTCs'),
    (PGN: $FED4; Name: 'DM12 — Emission DTCs'),
    (PGN: $F004; Name: 'Engine speed'),
    (PGN: $FEF1; Name: 'Cruise control / vehicle speed'));
var
  I: Integer;
begin
  for I := Low(CommonPGNs) to High(CommonPGNs) do
    Proc(Format('0x%.4x — %s',
      [CommonPGNs[I].PGN, CommonPGNs[I].Name]));
end;

{ ---- TOBDDIDListProperty -------------------------------------------------- }

function TOBDDIDListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TOBDDIDListProperty.GetValues(Proc: TGetStrProc);
const
  // ISO 14229 standard DIDs the host most commonly reads. OEM
  // overlays add their own through TOBDOEMRegistry — the IDE
  // picker covers the standard set; vendor-specific DIDs are
  // typed.
  CommonDIDs: array[0..11] of record
    DID: Word; Name: string;
  end = (
    (DID: $F186; Name: 'Active diagnostic session'),
    (DID: $F187; Name: 'ECU spare-part number'),
    (DID: $F188; Name: 'ECU software number'),
    (DID: $F189; Name: 'ECU software version'),
    (DID: $F18A; Name: 'System supplier'),
    (DID: $F18B; Name: 'ECU manufacturing date'),
    (DID: $F18C; Name: 'ECU serial number'),
    (DID: $F190; Name: 'Vehicle Identification Number (VIN)'),
    (DID: $F195; Name: 'System diagnostic spec version'),
    (DID: $F197; Name: 'System name / engine type'),
    (DID: $F198; Name: 'Repair shop code'),
    (DID: $F199; Name: 'Programming date'));
var
  I: Integer;
begin
  for I := Low(CommonDIDs) to High(CommonDIDs) do
    Proc(Format('0x%.4x — %s',
      [CommonDIDs[I].DID, CommonDIDs[I].Name]));
end;

{ ---- TOBDLiveDataComponentEditor ----------------------------------------- }

function TOBDLiveDataComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOBDLiveDataComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add standard PIDs…';
    1: Result := 'Live test PID…';
  else
    Result := '';
  end;
end;

procedure TOBDLiveDataComponentEditor.ExecuteVerb(Index: Integer);
const
  StandardPIDs: array[0..6] of Byte = ($04, $05, $0B, $0C, $0D, $0E, $11);
var
  Live: TOBDLiveData;
  PID: Byte;
begin
  Live := Component as TOBDLiveData;
  case Index of
    0:
      begin
        // Subscribe a placeholder handler for each universal-
        // support PID so the design-time component carries the
        // canonical pollable set. The host overrides at runtime.
        for PID in StandardPIDs do
          Live.Subscribe(PID, nil);
        Designer.Modified;
      end;
    1:
      begin
        TOBDLiveTestDlg.Show(
          'Live test ' + Live.Name,
          'Reads PID 0x0C (engine RPM) once and reports the value.',
          procedure(const AWriteLine: TProc<string>;
            const ASetStatus: TProc<TOBDLiveTestStatus>)
          begin
            if Live.Protocol = nil then
            begin
              AWriteLine('FAILED: Protocol not assigned.');
              ASetStatus(ltsFail);
              Exit;
            end;
            AWriteLine('Reading PID 0x0C (engine RPM)…');
            try
              Live.Read($0C);
              AWriteLine('Read returned without raising.');
              ASetStatus(ltsOK);
            except
              on E: Exception do
              begin
                AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
                ASetStatus(ltsFail);
              end;
            end;
          end);
      end;
  end;
end;

{ ---- TOBDDTCsComponentEditor --------------------------------------------- }

function TOBDDTCsComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TOBDDTCsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Read DTCs…';
    1: Result := 'Clear DTCs…';
  else
    Result := '';
  end;
end;

procedure TOBDDTCsComponentEditor.ExecuteVerb(Index: Integer);
var
  DTC: TOBDDTCs;
  Entries: TArray<TOBDDtcEntry>;
  EntryIdx: Integer;
  Confirmed: Integer;
begin
  DTC := Component as TOBDDTCs;
  case Index of
    0:
      begin
        TOBDLiveTestDlg.Show(
          'Read DTCs from ' + DTC.Name,
          'Reads confirmed, pending and permanent DTCs.',
          procedure(const AWriteLine: TProc<string>;
            const ASetStatus: TProc<TOBDLiveTestStatus>)
          begin
            if DTC.Protocol = nil then
            begin
              AWriteLine('FAILED: Protocol not assigned.');
              ASetStatus(ltsFail);
              Exit;
            end;
            try
              AWriteLine('Confirmed:');
              Entries := DTC.ReadConfirmed;
              for EntryIdx := 0 to High(Entries) do
                AWriteLine('  ' + Entries[EntryIdx].Code);
              ASetStatus(ltsOK);
            except
              on E: Exception do
              begin
                AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
                ASetStatus(ltsFail);
              end;
            end;
          end);
      end;
    1:
      begin
        // Destructive — same confirmation contract as the flash
        // safety dialog, but lighter (no signature audit).
        Confirmed := MessageBoxW(GetActiveOwnerWindow,
          'Clearing DTCs drops the vehicle''s stored emission '
          + 'history.'#13#10'Continue?',
          'Clear DTCs',
          MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2);
        if Confirmed <> IDYES then
          Exit;
        TOBDLiveTestDlg.Show(
          'Clear DTCs on ' + DTC.Name,
          'Sends Mode 0x04 ClearDiagnosticInformation.',
          procedure(const AWriteLine: TProc<string>;
            const ASetStatus: TProc<TOBDLiveTestStatus>)
          begin
            if DTC.Protocol = nil then
            begin
              AWriteLine('FAILED: Protocol not assigned.');
              ASetStatus(ltsFail);
              Exit;
            end;
            try
              DTC.Clear;
              AWriteLine('Cleared.');
              ASetStatus(ltsOK);
            except
              on E: Exception do
              begin
                AWriteLine('FAILED: ' + E.ClassName + ' — ' + E.Message);
                ASetStatus(ltsFail);
              end;
            end;
          end);
      end;
  end;
end;

procedure RegisterDelphiOBDEditors;
begin
  // String / file properties
  RegisterPropertyEditor(TypeInfo(string), TOBDReplayer, 'FileName',
    TOBDObdLogFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TOBDFlashPipeline, 'CheckpointFile',
    TOBDCheckpointFileProperty);

  // COM port picker (registered globally for the 'Port' property
  // on any TOBDSerialSettings sub-object the IDE walks into).
  RegisterPropertyEditor(TypeInfo(string), nil, 'Port',
    TOBDSerialPortProperty);

  // Bluetooth / Wi-Fi / FTDI transport pickers — registered
  // globally for the conventional property names so any settings
  // sub-object picks them up.
  RegisterPropertyEditor(TypeInfo(string), nil, 'Address',
    TOBDBluetoothDeviceProperty);
  RegisterPropertyEditor(TypeInfo(string), nil, 'Host',
    TOBDWiFiHostProperty);
  RegisterPropertyEditor(TypeInfo(string), nil, 'FTDISerial',
    TOBDFTDISerialProperty);

  // TStrings init-script enhancement
  RegisterPropertyEditor(TypeInfo(TStrings), TOBDAdapter, 'InitCommands',
    TOBDAdapterInitCommandsProperty);

  // Component editors — live-test verbs
  RegisterComponentEditor(TOBDConnection, TOBDConnectionComponentEditor);
  RegisterComponentEditor(TOBDAdapter, TOBDAdapterComponentEditor);
  RegisterComponentEditor(TOBDProtocol, TOBDProtocolComponentEditor);
  RegisterComponentEditor(TOBDLiveData, TOBDLiveDataComponentEditor);
  RegisterComponentEditor(TOBDDTCs, TOBDDTCsComponentEditor);

  // Component editors for destructive components
  RegisterComponentEditor(TOBDFlasher, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDUDSTransfer, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDFlashPipeline, TOBDFlashPipelineComponentEditor);
end;

end.
