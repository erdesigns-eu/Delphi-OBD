//------------------------------------------------------------------------------
// UNIT           : OBD.MainForm.Wizard.pas
// CONTENTS       : OBD MainForm wizard
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 04/04/2024
//------------------------------------------------------------------------------
unit OBD.Mainform.Wizard;

interface

uses
  System.SysUtils, WinApi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, PlatformAPI;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD MainForm Module Creator Wizard
  /// </summary>
  TOBDMainFormModuleCreatorWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTARepositoryWizard160)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard80
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    // IOTARepositoryWizard160
    function GetFrameworkTypes: TArray<string>;
    function GetPlatforms: TArray<string>;
  end;

  /// <summary>
  ///   OBD MainForm Module Creator
  /// </summary>
  TOBDMainFormModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FProject: IOTAProject;
  public
    constructor Create; overload; virtual;
    constructor Create(AProject: IOTAProject); overload; virtual;

    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  /// <summary>
  ///   OBD Source File
  /// </summary>
  TOBDSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

implementation

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET ID STRING
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetIDString: string;
begin
  Result := 'ERDesigns.OBDMainFormWizard';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET NAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetName: string;
begin
  Result := 'Windows VCL Mainform';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET STATE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: EXECUTE
//------------------------------------------------------------------------------
procedure TOBDMainFormModuleCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TOBDMainFormModuleCreator.Create);
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET AUTHOR
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET COMMENT
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetComment: string;
begin
  Result := 'Creates a new ERDesigns OBD Mainform including a header, subheader and statusbar.';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET PAGE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetPage: string;
begin
  Result := 'ERDesigns OBD';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET GLYPH
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET DESIGNER
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET PERSONALITY
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET GALLERY CATEGORY
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  Category: IOTAGalleryCategory;
  CatManager: IOTAGalleryCategoryManager;
begin
  CatManager := (BorlandIDEServices as IOTAGalleryCategoryManager);
  Assert(Assigned(CatManager));
  Category := CatManager.FindCategory(sCategoryDelphiNewFiles);
  if Assigned(Category) then
    Result := Category
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET FRAMEWORK TYPES
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetFrameworkTypes: TArray<String>;
begin
  Setlength(Result, 1);
  Result[0] := sFrameworkTypeVCL;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR WIZARD: GET PLATFORMS
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreatorWizard.GetPlatforms: TArray<String>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: FORM CREATED
//------------------------------------------------------------------------------
procedure TOBDMainFormModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetAncestorName: string;
begin
  Result := 'OBDForm';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMainFormModuleCreator.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDMainFormModuleCreator.Create(AProject: IOTAProject);
begin
  // Call inherited constructor
  inherited Create;
  // Store project
  FProject := AProject;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET EXCISTING
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET FILE SYSTEM
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET FORM NAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetFormName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET IMPLEMENTATION FILENAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET INTERFACE FILENAME
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetIntfFileName: string;
begin
  // Blank for forms
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET MAIN FORM
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetMainForm: Boolean;
begin
  // Since this is the mainform, return true.
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET OWNER
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  if Assigned(FProject) then
  begin
    Result := FProject;
  end else
  begin
    Result := nil;
    ModuleServices := (BorlandIDEServices as IOTAModuleServices);
    Module := ModuleServices.CurrentModule;

    if Module <> nil then
    begin
      if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
        Result := NewModule

      else if Module.OwnerModuleCount > 0 then
      begin
        NewModule := Module.OwnerModules[0];
        if NewModule <> nil then
          if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
            Result := nil;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET SHOW FORM
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetShowForm: Boolean;
begin
  // Show the form by default
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET SHOW SOURCE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetShowSource: Boolean;
begin
  // Show the source code by default
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: GET UNNAMED
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.GetUnnamed: Boolean;
begin
  // Typically True for new files to prompt for a name
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: NEW FORM FILE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
const
  FormDFMTemplate =
    'object %s: T%s'                                  + #13#10 +
    'Left = 0'                                        + #13#10 +
    'Top = 0'                                         + #13#10 +
    'Caption = ''%s'''                                + #13#10 +
    'ClientHeight = 442'                              + #13#10 +
    'ClientWidth = 628'                               + #13#10 +
    'Color = clBtnFace'                               + #13#10 +
    'Font.Charset = DEFAULT_CHARSET'                  + #13#10 +
    'Font.Color = clWindowText'                       + #13#10 +
    'Font.Height = -12'                               + #13#10 +
    'Font.Name = ''Segoe UI'''                        + #13#10 +
    'Font.Style = []'                                 + #13#10 +
    'TextHeight = 15'                                 + #13#10 +

    // Our OBD Touch controls
    'object OBDTouchHeader1: TOBDTouchHeader'         + #13#10 +
    'Left = 0'                                        + #13#10 +
    'Top = 0'                                         + #13#10 +
    'Width = 628'                                     + #13#10 +
    'Height = 50'                                     + #13#10 +
    'end'                                             + #13#10 +
    'object OBDTouchSubheader1: TOBDTouchSubheader'   + #13#10 +
    'Left = 0'                                        + #13#10 +
    'Top = 50'                                        + #13#10 +
    'Width = 628'                                     + #13#10 +
    'Height = 25'                                     + #13#10 +
    'end'                                             + #13#10 +
    'object OBDTouchStatusbar1: TOBDTouchStatusbar'   + #13#10 +
    'Left = 0'                                        + #13#10 +
    'Top = 421'                                       + #13#10 +
    'Width = 628'                                     + #13#10 +
    'Height = 21'                                     + #13#10 +
    'end'                                             + #13#10 +
    // Our OBD Touch controls

    'end';
begin
  Result := TOBDSourceFile.Create(Format(FormDFMTemplate, [FormIdent, FormIdent, FormIdent]))
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: NEW IMPLEMENATION SOURCE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
const
  SourceTemplate =
    '//------------------------------------------------------------------------------'                   + #13#10 +
    '// UNIT           : %s.pas'                                                                         + #13#10 +
    '// VERSION        : 1.0'                                                                            + #13#10 +
    '// TARGET         : Embarcadero Delphi 11 or higher'                                                + #13#10 +
    '// AUTHOR         : Ernst Reidinga (ERDesigns)'                                                     + #13#10 +
    '// STATUS         : Open source under Apache 2.0 library'                                           + #13#10 +
    '// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11'                                                       + #13#10 +
    '// CREATED DATE   : %s'                                                                             + #13#10 +
    '//------------------------------------------------------------------------------'                   + #13#10 +
    ''                                                                                                   + #13#10 +
    'unit %s;'                                                                                           + #13#10 +
    ''                                                                                                   + #13#10 +
    'interface'                                                                                          + #13#10 +
    ''                                                                                                   + #13#10 +
    'uses'                                                                                               + #13#10 +
    '  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,' + #13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,'                                                            + #13#10 +
    ''                                                                                                   + #13#10 +
    '  OBD.Form, OBD.Touch.Subheader, OBD.Touch.Header, OBD.Touch.Statusbar;'                            + #13#10 +
    ''                                                                                                   + #13#10 +
    'type'                                                                                               + #13#10 +
    '  T%s = class(T%s)'                                                                                 + #13#10 +

    // Our OBD Touch controls
    '    OBDTouchHeader1: TOBDTouchHeader;'                                                              + #13#10 +
    '    OBDTouchSubheader1: TOBDTouchSubheader;'                                                        + #13#10 +
    '    OBDTouchStatusbar1: TOBDTouchStatusbar;'                                                        + #13#10 +
    // Our OBD Touch controls

    '  private'                                                                                          + #13#10 +
    '    { Private declarations }'                                                                       + #13#10 +
    '  public'                                                                                           + #13#10 +
    '    { Public declarations }'                                                                        + #13#10 +
    '  end;'                                                                                             + #13#10 +
    ''                                                                                                   + #13#10 +
    'var'                                                                                                + #13#10 +
    '  %s: T%s;'                                                                                         + #13#10 +
    ''                                                                                                   + #13#10 +
    'implementation'                                                                                     + #13#10 +
    ''                                                                                                   + #13#10 +
    '{$R *.dfm}'                                                                                         + #13#10 +
    ''                                                                                                   + #13#10 +
    'end.';
begin
  Result := TOBDSourceFile.Create(Format(SourceTemplate, [ModuleIdent, FormatDateTime('dd/mm/yyyy', Now), ModuleIdent, FormIdent, AncestorIdent, FormIdent, FormIdent]));
end;

//------------------------------------------------------------------------------
// TOBDMAINFORM MODULE CREATOR: NEW INTERFACE SOURCE
//------------------------------------------------------------------------------
function TOBDMainFormModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
// TOBSOURCEFILE: CREATE
//------------------------------------------------------------------------------
constructor TOBDSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

//------------------------------------------------------------------------------
// TOBSOURCEFILE: GET AGE
//------------------------------------------------------------------------------
function TOBDSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------
// TOBSOURCEFILE: GET SOURCE
//------------------------------------------------------------------------------
function TOBDSourceFile.GetSource: string;
begin
  Result := FSource;
end;

end.

