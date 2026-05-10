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
  Result := 1;
end;

function TOBDConnectionComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Test connection…';
  else
    Result := '';
  end;
end;

procedure TOBDConnectionComponentEditor.ExecuteVerb(Index: Integer);
var
  Conn: TOBDConnection;
begin
  if Index <> 0 then Exit;
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

  // TStrings init-script enhancement
  RegisterPropertyEditor(TypeInfo(TStrings), TOBDAdapter, 'InitCommands',
    TOBDAdapterInitCommandsProperty);

  // Component editors — live-test verbs
  RegisterComponentEditor(TOBDConnection, TOBDConnectionComponentEditor);
  RegisterComponentEditor(TOBDAdapter, TOBDAdapterComponentEditor);
  RegisterComponentEditor(TOBDProtocol, TOBDProtocolComponentEditor);

  // Component editors for destructive components
  RegisterComponentEditor(TOBDFlasher, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDUDSTransfer, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDFlashPipeline, TOBDFlashPipelineComponentEditor);
end;

end.
