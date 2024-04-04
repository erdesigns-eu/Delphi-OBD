//------------------------------------------------------------------------------
// UNIT           : OBD.Form.pas
// CONTENTS       : OBD Form
// COMMENTS       : This form is used for OBD Projects, and include access to
//                : the Application Settings, repainting of OBD Touch controls
//                : when the window state changes, and other
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 03/04/2024
//------------------------------------------------------------------------------
unit OBD.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections,

  OBD.Application.Settings, OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar;

//------------------------------------------------------------------------------
// TYPES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Window state change event
  /// </summary>
  TWindowStateEvent = procedure(Sender: TObject; OldState, NewState: TWindowState) of object;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   Base OBD Form
  /// </summary>
  TOBDForm = class(TForm)
  private
    /// <summary>
    ///   "Old" window state used in WndProc
    /// </summary>
    FOldWindowState: TWindowState;
    /// <summary>
    ///   Windows State event
    /// </summary>
    FOnWindowState: TWindowStateEvent;

    /// <summary>
    ///   List for OBDTouchHeader
    /// </summary>
    FHeaders: TList<TOBDTouchHeader>;
    /// <summary>
    ///   List for OBDTouchSubHeader
    /// </summary>
    FSubHeaders: TList<TOBDTouchSubHeader>;
    /// <summary>
    ///   List for OBDTouchStatusbar
    /// </summary>
    FStatusBars: TList<TOBDTouchStatusbar>;

    /// <summary>
    ///   Get application settings singleton instance
    /// </summary>
    function GetApplicationSettings: TOBDApplicationSettings;
    /// <summary>
    ///   Repaint OBD controls
    /// </summary>
    procedure RepaintOBDControls; virtual;
  protected
    /// <summary>
    ///   Override WndProc method
    /// </summary>
    procedure WndProc(var Msg: TMessage); override;
  public
    /// <summary>
    ///   Constructor
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///   Destructor
    /// </summary>
    destructor Destroy; override;
  published
    /// <summary>
    ///   Application settings instance
    /// </summary>
    property ApplicationSettings: TOBDApplicationSettings read GetApplicationSettings;
    /// <summary>
    ///   Event that is fired when the Window State changed
    /// </summary>
    property OnWindowState: TWindowStateEvent read FOnWindowState write FOnWindowState;
  end;

var
  OBDForm: TOBDForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// GET APPLICATION SETTINGS INSTANCE
//------------------------------------------------------------------------------
function TOBDForm.GetApplicationSettings: TOBDApplicationSettings;
begin
  // Get the singleton instance
  Result := TOBDApplicationSettings.Instance;
end;

//------------------------------------------------------------------------------
// REPAINT OBD CONTROLS
//------------------------------------------------------------------------------
procedure TOBDForm.RepaintOBDControls;
var
  Header: TOBDTouchHeader;
  SubHeader: TOBDTouchSubHeader;
  StatusBar: TOBDTouchStatusbar;
begin
  // Repaint TOBTouchDHeaders
  for Header in FHeaders do Header.Repaint;
  // Repaint TOBDTouchSubHeaders
  for SubHeader in FSubHeaders do SubHeader.Repaint;
  // Repaint TOBDTouchStatusbars
  for StatusBar in FStatusBars do StatusBar.Repaint;
end;

//------------------------------------------------------------------------------
// WNDPROC HANDLER
//------------------------------------------------------------------------------
procedure TOBDForm.WndProc(var Msg: TMessage);
begin
  // Call inherited WndProc
  inherited WndProc(Msg);

  // Handle window messages that we need to repaint OBD controls
  // like the header, subheader and statusbar. This is because when the
  // window is Maximized or Restored there is no resize handler called
  // and the buffers of our custom components are not updated.
  if Msg.Msg = WM_WINDOWPOSCHANGED then
  begin
    // Check if the window state changed
    if (FOldWindowState <> WindowState) then
    begin
      // Fire the event
      if Assigned(OnWindowState) then OnWindowState(Self, FOldWindowState, WindowState);
      // Update window state for next event, as we only need to repaint the
      // controls when the window state changed.
      FOldWindowState := WindowState;
      // Repaint our controls
      RepaintOBDControls;
    end;
  end;
end;

//------------------------------------------------------------------------------
// CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDForm.Create(AOwner: TComponent);
var
  I: Integer;
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Set initial window state
  FOldWindowState := WindowState;
  // Create lists for storing our OBD components that need repainting
  // when the window state changed. By populating the list once on
  // form creation we prevent the need to loop over all components to
  // find our OBD components when these need to be repainted.
  FHeaders := TList<TOBDTouchHeader>.Create;
  FSubHeaders := TList<TOBDTouchSubHeader>.Create;
  FStatusBars := TList<TOBDTouchStatusbar>.Create;
  // Loop over the components and fill the lists
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TOBDTouchHeader then
      FHeaders.Add(TOBDTouchHeader(Components[I]))
    else if Components[I] is TOBDTouchSubHeader then
      FSubHeaders.Add(TOBDTouchSubHeader(Components[I]))
    else if Components[I] is TOBDTouchStatusbar then
      FStatusBars.Add(TOBDTouchStatusbar(Components[I]));
  end;
end;

//------------------------------------------------------------------------------
// DESTRUCTOR
//------------------------------------------------------------------------------
destructor TOBDForm.Destroy;
begin
  // Free the lists with our OBD components.
  FreeAndNil(FHeaders);
  FreeAndNil(FSubHeaders);
  FreeAndNil(FStatusBars);
  // Call inherited destructor
  inherited Destroy;
end;

end.

