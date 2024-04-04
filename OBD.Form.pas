//------------------------------------------------------------------------------
// UNIT           : OBD.Form.pas
// CONTENTS       : OBD Form
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  OBD.Touch.Header, OBD.Touch.Subheader, OBD.Touch.Statusbar;

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
  end;

var
  OBDForm: TOBDForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// REPAINT OBD CONTROLS
//------------------------------------------------------------------------------
procedure TOBDForm.RepaintOBDControls;
var
  I: Integer;
  Header: TOBDTouchHeader;
  SubHeader: TOBDTouchSubHeader;
  StatusBar: TOBDTouchStatusbar;
begin
  for I := 0 to ControlCount - 1 do
  begin
    // Repaint Header
    if Controls[I] is TOBDTouchHeader then
    begin
      Header := TOBDTouchHeader(Controls[I]);
      Header.Repaint;
    end;

    // Repaint Subheader
    if Controls[I] is TOBDTouchSubHeader then
    begin
      SubHeader := TOBDTouchSubHeader(Controls[I]);
      SubHeader.Repaint;
    end;

    // Repaint Statusbar
    if Controls[I] is TOBDTouchStatusbar then
    begin
      StatusBar := TOBDTouchStatusbar(Controls[I]);
      StatusBar.Repaint;
    end;
  end;
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
begin
  // Call inherited constructor
  inherited Create(AOwner);
  // Set initial window state
  FOldWindowState := WindowState;
end;

end.

