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
  System.Generics.Collections,
  Vcl.Forms,
  Vcl.Controls,
  OBD.Adapter,
  OBD.Recorder,
  OBD.Replayer,
  OBD.Coding.Flasher,
  OBD.UDS.Transfer,
  OBD.Flash.Pipeline,
  OBD.Design.Forms.FlashSafety,
  OBD.Design.Forms.InitCommands,
  OBD.Design.Forms.PipelineValidate;

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

{ ---- registration ----------------------------------------------------------- }

procedure RegisterDelphiOBDEditors;
begin
  // String / file properties
  RegisterPropertyEditor(TypeInfo(string), TOBDReplayer, 'FileName',
    TOBDObdLogFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TOBDFlashPipeline, 'CheckpointFile',
    TOBDCheckpointFileProperty);

  // TStrings init-script enhancement
  RegisterPropertyEditor(TypeInfo(TStrings), TOBDAdapter, 'InitCommands',
    TOBDAdapterInitCommandsProperty);

  // Component editors for destructive components
  RegisterComponentEditor(TOBDFlasher, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDUDSTransfer, TOBDDestructiveComponentEditor);
  RegisterComponentEditor(TOBDFlashPipeline, TOBDFlashPipelineComponentEditor);
end;

end.
