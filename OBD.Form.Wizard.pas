//------------------------------------------------------------------------------
// UNIT           : OBD.Form.Wizard.pas
// CONTENTS       : OBD Form wizard
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 03/04/2024
//------------------------------------------------------------------------------
unit OBD.Form.Wizard;

interface

uses
  ToolsAPI, System.SysUtils, Dialogs;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Base OBD Form Wizard
  /// </summary>
  TOBDFormWizard = class(TInterfacedObject, IOTARepositoryWizard, IOTAWizard, IOTAFormWizard, IOTARepositoryWizard80)
  public
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    // IOTARepositoryWizard methods
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

    // Additional methods for IOTAFormWizard
    function GetDesigner: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;

    // IOTANotifier methods
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

  /// <summary>
  ///   OTA File
  /// </summary>
  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FContent: string;
  public
    constructor Create(const Content: string);

    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  /// <summary>
  ///   Form File Creator
  /// </summary>
  type
  TOTAFileCreator = class(TInterfacedObject, IOTAModuleCreator)
  private
    FUnitName: string;
    FFormName: string;
    FClassName: string;
  public
    constructor Create(const UnitName, FormName, ClassName: string);

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
    function NewProjectSource(const ProjectName: string): IOTAFile;
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean; virtual;
    function GetFileSystem: string; virtual;
    function GetOwner: IOTAModule; virtual;
    function GetUnnamed: Boolean; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;


implementation

//------------------------------------------------------------------------------
// GET ID STRING
//------------------------------------------------------------------------------
function TOBDFormWizard.GetIDString: string;
begin
  Result := 'ERDesigns.OBDFormWizard';
end;

//------------------------------------------------------------------------------
// GET NAME
//------------------------------------------------------------------------------
function TOBDFormWizard.GetName: string;
begin
  Result := 'OBD Form';
end;

//------------------------------------------------------------------------------
// GET STATE
//------------------------------------------------------------------------------
function TOBDFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------
// EXECUTE
//------------------------------------------------------------------------------
procedure TOBDFormWizard.Execute;
const
  Name: string = 'OBDMainForm';
var
  Project: IOTAProject;
  UnitName, ClassName, FileName, FormName: string;
begin
  if Assigned(BorlandIDEServices) then
  begin
    // Query new default unit, class and filename
    (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', UnitName, ClassName, FileName);
    ClassName := Format('T%s%s', [Name, Copy(UnitName, 5, Length(UnitName))]);
    FormName  := Format('%s%s', [Name, Copy(UnitName, 5, Length(UnitName))]);
    Project := GetActiveProject;
    if Assigned(Project) then
    begin
      (BorlandIDEServices as IOTAModuleServices).CreateModule(TOTAFileCreator.Create(UnitName, FormName, ClassName) as IOTAModuleCreator);
    end;
  end;
end;


//------------------------------------------------------------------------------
// GET AUTHOR
//------------------------------------------------------------------------------
function TOBDFormWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

//------------------------------------------------------------------------------
// GET COMMENT
//------------------------------------------------------------------------------
function TOBDFormWizard.GetComment: string;
begin
  Result := 'Create a new ERDesigns OBD Mainform including a header, subheader and statusbar.';
end;

//------------------------------------------------------------------------------
// GET PAGE
//------------------------------------------------------------------------------
function TOBDFormWizard.GetPage: string;
begin
  Result := 'ERDesigns OBD';
end;

//------------------------------------------------------------------------------
// GET GLYPH
//------------------------------------------------------------------------------
function TOBDFormWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// GET DESIGNER
//------------------------------------------------------------------------------
function TOBDFormWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

//------------------------------------------------------------------------------
// GET GALLERY CATEGORY
//------------------------------------------------------------------------------
function TOBDFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
// GET PERSONALITY
//------------------------------------------------------------------------------
function TOBDFormWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// AFTER SAVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDFormWizard.AfterSave;
begin
  //
end;

//------------------------------------------------------------------------------
// BEFORE SAVE HANDLER
//------------------------------------------------------------------------------
procedure TOBDFormWizard.BeforeSave;
begin
  //
end;

//------------------------------------------------------------------------------
// DESTROYED HANDLER
//------------------------------------------------------------------------------
procedure TOBDFormWizard.Destroyed;
begin
  //
end;

//------------------------------------------------------------------------------
// MODIFIED HANDLER
//------------------------------------------------------------------------------
procedure TOBDFormWizard.Modified;
begin
  //
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOTAFile.Create(const Content: string);
begin
  // Call inherited constructor
  inherited Create;
  // Set content
  FContent := Content;
end;

//------------------------------------------------------------------------------
// GET SOURCE
//------------------------------------------------------------------------------
function TOTAFile.GetSource: string;
begin
  Result := FContent;
end;

//------------------------------------------------------------------------------
// GET AGE
//------------------------------------------------------------------------------
function TOTAFile.GetAge: TDateTime;
begin
  // Indicates a new file
  Result := -1;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOTAFileCreator.Create(const UnitName, FormName, ClassName: string);
begin
  inherited Create;
  FUnitName := UnitName;
  FFormName := FormName;
  FClassName := ClassName;
end;

//------------------------------------------------------------------------------
// GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOTAFileCreator.GetAncestorName: string;
begin
  Result := 'TOBDForm';
end;

//------------------------------------------------------------------------------
// GET IMPLEMENTATION FILENAME
//------------------------------------------------------------------------------
function TOTAFileCreator.GetImplFileName: string;
begin
  Result := FUnitName + '.pas';
end;

//------------------------------------------------------------------------------
// GET INTERFACE FILENAME
//------------------------------------------------------------------------------
function TOTAFileCreator.GetIntfFileName: string;
begin
  // Blank for forms
  Result := '';
end;

//------------------------------------------------------------------------------
// GET NAME OF THE FORM
//------------------------------------------------------------------------------
function TOTAFileCreator.GetFormName: string;
begin
  Result := FFormName;
end;

//------------------------------------------------------------------------------
// GET MAINFORM
//------------------------------------------------------------------------------
function TOTAFileCreator.GetMainForm: Boolean;
begin
  // True if this is the main form of the application
  Result := False;
end;

//------------------------------------------------------------------------------
// GET SHOW FORM
//------------------------------------------------------------------------------
function TOTAFileCreator.GetShowForm: Boolean;
begin
  // Show the form by default
  Result := True;
end;

//------------------------------------------------------------------------------
// GET SHOW SOURCE
//------------------------------------------------------------------------------
function TOTAFileCreator.GetShowSource: Boolean;
begin
  // Show the source code by default
  Result := True;
end;

//------------------------------------------------------------------------------
// GET NEW FORM FILE
//------------------------------------------------------------------------------
function TOTAFileCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
const
  FormDFMTemplate =
    'object %s: %s'                                   + #13#10 +
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
  Result := TOTAFile.Create(Format(FormDFMTemplate, [FFormName, FClassName, FFormName]));
end;

//------------------------------------------------------------------------------
// GET IMPLEMENATION SOURCE
//------------------------------------------------------------------------------
function TOTAFileCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
const
  SourceTemplate =
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
    '  %s = class(%s)'                                                                                   + #13#10 +

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
    '  %s: %s;'                                                                                          + #13#10 +
    ''                                                                                                   + #13#10 +
    'implementation'                                                                                     + #13#10 +
    ''                                                                                                   + #13#10 +
    '{$R *.dfm}'                                                                                         + #13#10 +
    ''                                                                                                   + #13#10 +
    'end.';
begin
  Result := TOTAFile.Create(Format(SourceTemplate, [ModuleIdent, FClassName, AncestorIdent, FormIdent, FClassName, FormIdent, FormIdent]));
end;

//------------------------------------------------------------------------------
// GET INTERFACE SOURCE
//------------------------------------------------------------------------------
function TOTAFileCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
// GET NEW PROJECT SOURCE
//------------------------------------------------------------------------------
function TOTAFileCreator.NewProjectSource(const ProjectName: string): IOTAFile;
const
  ProjectTemplate =
    'program %s'                                + #13#10 +
    ''                                          + #13#10 +
    'uses'                                      + #13#10 +
    '  Vcl.Forms,'                              + #13#10 +
    '  %s in ''%s'' {%s};'                      + #13#10 +
    ''                                          + #13#10 +
    '{$R *.res}'                                + #13#10 +
    ''                                          + #13#10 +
    'begin'                                     + #13#10 +
    '  Application.Initialize;'                 + #13#10 +
    '  Application.MainFormOnTaskbar := True;'  + #13#10 +
    '  Application.CreateForm(%s, %s);'         + #13#10 +
    '  Application.Run;'                        + #13#10 +
    'end.';
begin
  Result := TOTAFile.Create(Format(ProjectTemplate, [ProjectName, FUnitName, FUnitName, FClassName, FFormName]));
end;

//------------------------------------------------------------------------------
// GET CREATOR TYPE
//------------------------------------------------------------------------------
function TOTAFileCreator.GetCreatorType: string;
begin
  // Indicates creating a form
  Result := sForm;
end;

//------------------------------------------------------------------------------
// GET EXISTING
//------------------------------------------------------------------------------
function TOTAFileCreator.GetExisting: Boolean;
begin
  // Typically False for new files
  Result := False;
end;

//------------------------------------------------------------------------------
// GET FILE SYSTEM
//------------------------------------------------------------------------------
function TOTAFileCreator.GetFileSystem: string;
begin
  // Use the default file system
  Result := '';
end;

//------------------------------------------------------------------------------
// GET OWNER
//------------------------------------------------------------------------------
function TOTAFileCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
// GET UNNAMED
//------------------------------------------------------------------------------
function TOTAFileCreator.GetUnnamed: Boolean;
begin
  // Typically True for new files to prompt for a name
  Result := True;
end;

//------------------------------------------------------------------------------
// FORM CREATED HANDLER
//------------------------------------------------------------------------------
procedure TOTAFileCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // This method is called after the form has been created by the IDE.
  // If you need to perform any actions with the form editor, you can do it here.
  // For many simple use cases, this method can remain empty.
end;

end.
