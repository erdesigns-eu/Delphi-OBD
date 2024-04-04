//------------------------------------------------------------------------------
// UNIT           : OBD.Form.Wizard.pas
// CONTENTS       : OBD Form wizard
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 04/04/2024
//------------------------------------------------------------------------------
unit OBD.Form.Wizard;

interface

uses
  System.SysUtils, WinApi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, PlatformAPI,

  OBD.Mainform.Wizard;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Form Module Creator Wizard
  /// </summary>
  TOBFormModuleCreatorWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTARepositoryWizard160)
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
  ///   OBD Form Module Creator
  /// </summary>
  TOBDFormModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

implementation

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET ID STRING
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetIDString: string;
begin
  Result := 'ERDesigns.OBDFormWizard';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET NAME
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetName: string;
begin
  Result := 'ERDesigns OBD Form';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET STATE
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: EXECUTE
//------------------------------------------------------------------------------
procedure TOBFormModuleCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TOBDMainFormModuleCreator.Create);
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET AUTHOR
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET COMMENT
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetComment: string;
begin
  Result := 'Creates a new empty ERDesigns OBD Form.';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET PAGE
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetPage: string;
begin
  Result := 'ERDesigns OBD';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET GLYPH
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET DESIGNER
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET PERSONALITY
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET GALLERY CATEGORY
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetGalleryCategory: IOTAGalleryCategory;
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
// TOBDFORM MODULE CREATOR WIZARD: GET FRAMEWORK TYPES
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetFrameworkTypes: TArray<String>;
begin
  Setlength(Result, 1);
  Result[0] := sFrameworkTypeVCL;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR WIZARD: GET PLATFORMS
//------------------------------------------------------------------------------
function TOBFormModuleCreatorWizard.GetPlatforms: TArray<String>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: FORM CREATED
//------------------------------------------------------------------------------
procedure TOBDFormModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetAncestorName: string;
begin
  Result := 'OBDForm';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDFormModuleCreator.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDFormModuleCreator.Create(AProject: IOTAProject);
begin
  // Call inherited constructor
  inherited Create;
  // Store project
  FProject := AProject;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET EXCISTING
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET FILE SYSTEM
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET FORM NAME
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetFormName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET IMPLEMENTATION FILENAME
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET INTERFACE FILENAME
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetIntfFileName: string;
begin
  // Blank for forms
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET MAIN FORM
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetMainForm: Boolean;
begin
  // Since this is not a mainform, return false.
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET OWNER
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetOwner: IOTAModule;
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
// TOBDFORM MODULE CREATOR: GET SHOW FORM
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetShowForm: Boolean;
begin
  // Show the form by default
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET SHOW SOURCE
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetShowSource: Boolean;
begin
  // Show the source code by default
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: GET UNNAMED
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.GetUnnamed: Boolean;
begin
  // Typically True for new files to prompt for a name
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: NEW FORM FILE
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
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
    'end';
begin
  Result := TOBDSourceFile.Create(Format(FormDFMTemplate, [FormIdent, FormIdent, FormIdent]))
end;

//------------------------------------------------------------------------------
// TOBDFORM MODULE CREATOR: NEW IMPLEMENATION SOURCE
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
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
    '  OBD.Form;'                                                                                        + #13#10 +
    ''                                                                                                   + #13#10 +
    'type'                                                                                               + #13#10 +
    '  T%s = class(T%s)'                                                                                 + #13#10 +
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
// TOBDFORM MODULE CREATOR: NEW INTERFACE SOURCE
//------------------------------------------------------------------------------
function TOBDFormModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
