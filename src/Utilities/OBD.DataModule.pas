//------------------------------------------------------------------------------
// UNIT           : OBD.DataModule.pas
// CONTENTS       : OBD DataModule
// COMMENTS       : This DataModule is used for OBD Projects, and include access
//                : to the Application Settings.
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/04/2024
//------------------------------------------------------------------------------
unit OBD.DataModule;

interface

uses
  System.SysUtils, System.Classes,

  OBD.Application.Settings;

type
  TOBDDataModule = class(TDataModule)
  private
    /// <summary>
    ///   Get application settings singleton instance
    /// </summary>
    function GetApplicationSettings: TOBDApplicationSettings;
  published
    /// <summary>
    ///   Application settings instance
    /// </summary>
    property ApplicationSettings: TOBDApplicationSettings read GetApplicationSettings;
  end;

var
  OBDDataModule: TOBDDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

//------------------------------------------------------------------------------
// GET APPLICATION SETTINGS INSTANCE
//------------------------------------------------------------------------------
function TOBDDataModule.GetApplicationSettings: TOBDApplicationSettings;
begin
  // Get the singleton instance
  Result := TOBDApplicationSettings.Instance;
end;

end.
