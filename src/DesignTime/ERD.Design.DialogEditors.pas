// ------------------------------------------------------------------------------
// ERD.Design.DialogEditors
//
// Component editor for the themed OBD Studio dialogs, shipped by
// <c>DelphiOBD_DT.bpl</c>. Lives in the design-time package only;
// must never be referenced from runtime code.
//
// TOBDDialogTestEditor
// Adds a "Test Dialog" verb to the form designer's right-click menu
// of TOBDDialog and of every TOBDCommonDialog descendant (open /
// save / picture / folder, print / page setup, font, colour, find /
// replace and task dialogs), like the VCL dialogs have.
// Double-clicking the component runs it too.
//
// The preview runs on a temporary copy of the component: the
// published properties are copied over, the event handlers are not
// (at design time they point at designer stubs), so testing never
// changes the form. A modeless preview (find / replace) stays open
// until it is closed, the next preview starts or the package unloads.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : see LICENSE
//
// History     :
// 2026-10-11  ERD  Initial implementation.
// ------------------------------------------------------------------------------

unit ERD.Design.DialogEditors;

interface

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  DesignEditors,
  DesignIntf;

type
  /// <summary>Component editor that previews a themed dialog at
  /// design time ("Test Dialog").</summary>
  TOBDDialogTestEditor = class(TComponentEditor)
  public
    /// <summary>Runs the preview on a double-click.</summary>
    procedure Edit; override;
    /// <summary>Runs the verb.</summary>
    /// <param name="Index">Verb index.</param>
    procedure ExecuteVerb(Index: Integer); override;
    /// <summary>Caption of a verb.</summary>
    /// <param name="Index">Verb index.</param>
    /// <returns>Verb caption.</returns>
    function GetVerb(Index: Integer): string; override;
    /// <summary>Number of verbs.</summary>
    /// <returns>1.</returns>
    function GetVerbCount: Integer; override;
  end;

/// <summary>Registers the dialog component editor. Call from
/// <c>ERD.Design.Registration.Register</c>.</summary>
procedure RegisterDelphiOBDDialogEditors;

implementation

uses
  ERD.UI.Dialogs;

var
  // The last preview copy. Kept alive so a modeless preview stays
  // open; freed when the next preview starts or the package unloads.
  PreviewCopy: TComponent = nil;

procedure ReleasePreview;
begin
  FreeAndNil(PreviewCopy);
end;

/// <summary>Copies the published, writable properties of ASource to
/// ATarget (same class). Event handlers and the Name are skipped;
/// persistent sub-objects (strings, fonts, collections) are
/// assigned; component references are shared.</summary>
procedure CopyPublishedProperties(ASource, ATarget: TComponent);
var
  Props: PPropList;
  Count, I: Integer;
  Info: PPropInfo;
  Value, Target: TObject;
begin
  Count := GetPropList(ASource.ClassInfo, Props);
  if Count <= 0 then
    Exit;
  try
    for I := 0 to Count - 1 do
    begin
      Info := Props^[I];
      if SameText(string(Info^.Name), 'Name') then
        Continue;
      try
        case Info^.PropType^.Kind of
          tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,
          tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkUString:
            if (Info^.GetProc <> nil) and (Info^.SetProc <> nil) then
              SetPropValue(ATarget, Info, GetPropValue(ASource, Info));
          tkClass:
            begin
              Value := GetObjectProp(ASource, Info);
              if Value is TComponent then
              begin
                if Info^.SetProc <> nil then
                  SetObjectProp(ATarget, Info, Value);
              end
              else if Value is TPersistent then
              begin
                Target := GetObjectProp(ATarget, Info);
                if Target is TPersistent then
                  TPersistent(Target).Assign(TPersistent(Value));
              end;
            end;
        else
          // Events (designer stubs at design time), interfaces and
          // records are not copied.
        end;
      except
        // A property that cannot be copied keeps its default in the
        // preview; the preview itself must still open.
        on Exception do
          ;
      end;
    end;
  finally
    FreeMem(Props);
  end;
end;

procedure RunPreview(AComponent: TComponent);
var
  Preview: TComponent;
begin
  ReleasePreview;
  Preview := TComponentClass(AComponent.ClassType).Create(nil);
  PreviewCopy := Preview;
  CopyPublishedProperties(AComponent, Preview);
  if Preview is TOBDCommonDialog then
    TOBDCommonDialog(Preview).Execute
  else if Preview is TOBDDialog then
    TOBDDialog(Preview).Execute;
end;

{ TOBDDialogTestEditor }

procedure TOBDDialogTestEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TOBDDialogTestEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    RunPreview(Component);
end;

function TOBDDialogTestEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Test Dialog'
  else
    Result := '';
end;

function TOBDDialogTestEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure RegisterDelphiOBDDialogEditors;
begin
  // Registered on the bases; every dialog descending from
  // TOBDCommonDialog picks the editor up automatically.
  RegisterComponentEditor(TOBDCommonDialog, TOBDDialogTestEditor);
  RegisterComponentEditor(TOBDDialog, TOBDDialogTestEditor);
end;

initialization

finalization
  ReleasePreview;

end.
