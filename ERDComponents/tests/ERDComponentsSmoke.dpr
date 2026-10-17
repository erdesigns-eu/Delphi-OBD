program ERDComponentsSmoke;
{$APPTYPE CONSOLE}
uses
  ERD.Components.DocumentHost, ERD.Components.ProjectTree, ERD.Components.DiagnosticList, ERD.Components.DockHost, ERD.Components.Hint, ERD.Components.ShapeInspector, ERD.Components.DebugConsole, ERD.Components.RuntimeMonitor, ERD.Components.FiberList, ERD.Components.DisassemblyView, ERD.Components.WatchPanel, ERD.Components.LocalsPanel, ERD.Components.CallStack, ERD.Components.BreakpointPanel, ERD.Components.DebugModel, ERD.Components.ValueTree, ERD.Components.TimeSeries,
  ERD.Components.LSP, ERD.Components.Paint, ERD.Components.Control, ERD.Components.LanguageServices, ERD.Components.TextLayout, ERD.Components.CodeEditor, ERD.Components.Syntax,
  ERD.Components.PCBValidation, ERD.Components.PCBValidationPanel, ERD.Components.PCBLayout, ERD.Components.PCBObjectTree, ERD.Components.PCBDesigner, ERD.Components.PCBComponentPalette, ERD.Components.PCBInspector, ERD.Components.Inspector, ERD.Components.PCBScene, ERD.Components.PCBRoute, ERD.Components.PCBLibrary,
  ERD.Components.TextModel, ERD.Components.TextInput,
  System.IOUtils, System.JSON, System.UITypes,
  System.SysUtils, System.Classes, System.Types, System.Math, Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls,
  Winapi.Windows, Vcl.Clipbrd, Vcl.ActnList, Vcl.StdCtrls, Vcl.Graphics, Vcl.ImgList,
  ERD.Components.Sidebar, ERD.Components.Tabs, ERD.Components.Memo, ERD.Components.Menus,
  ERD.Components.Rating,
  ERD.Components.CommandPalette,
  ERD.Components.DropZone,
  ERD.Components.LoadingIndicator,
  ERD.Components.LinkLabel,
  ERD.Components.Calendar, ERD.Components.DateTimePicker,
  ERD.Components.TimePicker,
  ERD.Components.CheckListBox,
  ERD.Components.ScrollBar, ERD.Components.KeyValueList,
  ERD.Components.CommonControls, ERD.Components.MediaControls, ERD.Components.TrackBar,
  ERD.Components.ShellControls,
  ERD.Components.Types, ERD.Components.Icons, ERD.Components.IconCatalog,
  ERD.Components.Buttons, ERD.Components.Panel, ERD.Components.Chips,
  ERD.Components.VirtualTree, ERD.Components.VT.BaseTree, ERD.Components.VT.Types,
  PCBPreview in '../demo/PCBPreview.pas',
  Showcase in '../demo/Showcase.pas',
  ERD.Components.SpinEdit, ERD.Components.Edits, ERD.Components.PopupList, ERD.Components.TitleBar, ERD.Components.StatusBar, ERD.Components.Theme, ERD.Components.Ribbon;
type
  TLanguageServiceProbe = class(TInterfacedObject, IERDCodeLanguageService)
  public
    Snapshot: TERDLanguageRequest;
    Responses: IERDLanguageResponses;
    Requests, Cancels: Integer;
    procedure Request(const Value: TERDLanguageRequest; const Reply: IERDLanguageResponses);
    procedure Cancel(RequestID: Int64);
  end;
  TPCBPaletteSink = class
    Choices, Changes, Cancels, Starts, Ends: Integer;
    LastID: string;
    Designer: TERDPCBDesigner;
    procedure Chosen(Sender: TObject);
    procedure Selection(Sender: TObject);
    procedure Cancelled(Sender: TObject);
    procedure Started(Sender: TObject; var DragObject: TDragObject);
    procedure Ended(Sender, Target: TObject; X, Y: Integer);
  end;
  TPCBValidationProbe = class
    Reject, Mutate: Boolean;
    Rules, Navigations: Integer;
    LastSender: TObject;
    LastID: string;
    procedure Validate(Sender: TObject; const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
    procedure Navigate(Sender: TObject; const Issue: TERDPCBValidationIssue);
  end;
  TPCBDragProbe = class(TERDPCBDesigner)
    procedure Render(Bitmap: TBitmap);
    procedure StartAt(const P: TPoint);
    procedure MoveTo(const P: TPoint; Alt: Boolean = False);
    procedure StopAt(const P: TPoint);
    procedure Escape;
  end;
  TPCBContrastProbe = class(TERDPCBDesigner)
    procedure Render(Bitmap: TBitmap);
  end;
  TPCBHistoryProbe = class
    Scene: TERDPCBScene;
    RejectCaption: string;
    Changes: Integer;
    SawUndo, SawRedo: Boolean;
    procedure Validate(Sender: TObject; const Item: TERDPCBDesignItem);
    procedure Changed(Sender: TObject);
  end;
  TPCBGroupProbe = class
    Changes: Integer;
    Binding: TERDPCBInspectorBinding;
    RejectID: string;
    procedure ChangingProperty(Sender: TObject; Row: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
    procedure ChangedProperty(Sender: TObject; Row: TERDInspectorProperty);
    procedure Changed(Sender: TObject);
    procedure Validate(Sender: TObject; const Item: TERDPCBDesignItem);
  end;
  TPCBHostProbe = class
    Validations: Integer;
    procedure ValidateItem(Sender: TObject; const Item: TERDPCBDesignItem);
    procedure RejectCheck(Sender: TObject; Row: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
  end;
  TCodeEditorProbe = class(TERDCodeEditor)
  protected
    procedure DestroyWnd; override;
  public
    ShutdownModelAlive: PBoolean;
    function RowCount: Integer;
    procedure DebugClick(Row: Integer; ToggleEnabled: Boolean = False);
    procedure DebugKey(Key: Word; Shift: TShiftState);
    function DebugPixel(Row: Integer; Marker: Boolean): TColor;
    procedure FoldMouse(Action: Integer; Outside: Boolean = False);
    procedure GutterMouse(Action, Row: Integer; Shift: Boolean = False);
    function FoldPixel(Guide: Boolean): TColor;
    function TopFoldGuidePixel: TColor;
    function IndentGuidePixel(Row: Integer): TColor;
    function GutterBackground: TColor;
    function EditorBackground: TColor;
    function MinimapBackground: TColor;
    procedure GetNavigationRects(out Vertical, Horizontal, Map: TRect);
    function HorizontalOffset: Integer;
    function ViewWidth: Integer;
  end;
  TBreakpointPanelProbe = class(TERDBreakpointPanel)
  public
    NavigatedUri: string;
    NavigatedLine: Integer;
    procedure Navigate(Sender: TObject; const DocumentUri: string; Line: Integer);
    procedure CommitCell(Column: TColumnIndex; const Value: string);
  end;
  TStackProbe = class
  public
    Calls, Navigations, Line: Integer;
    LastFrame: TERDStackFrame;
    LastContext: TERDStackRequest;
    procedure Selected(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame);
    procedure Navigate(Sender: TObject; const DocumentUri: string; SourceLine, Column: Integer);
  end;
  TLocalsProbe = class
  public
    Panel: TERDLocalsPanel;
    ScopeCalls, VariableCalls: Integer;
    LastContext: TERDLocalsRequest;
    LastRequest: TERDValueRequest;
    LastParent: TERDInspectValue;
    Synchronous: Boolean;
    procedure Scopes(Sender: TObject; const Context: TERDLocalsRequest);
    procedure Variables(Sender: TObject; const Context: TERDLocalsRequest; const Request: TERDValueRequest; const Parent: TERDInspectValue);
  end;
  TWatchProbe = class
  public
    Panel: TERDWatchPanel;
    Calls, VariableCalls: Integer;
    LastContext: TERDWatchContext;
    LastRequest: TERDValueRequest;
    LastExpressions: TArray<TERDWatchExpression>;
    Synchronous: Boolean;
    procedure Evaluate(Sender: TObject; const Context: TERDWatchContext; const Expressions: TArray<TERDWatchExpression>);
    procedure Variables(Sender: TObject; const Context: TERDWatchContext; const Request: TERDValueRequest; const Parent: TERDInspectValue);
  end;
  TDisassemblyProbe = class
  public
    View: TERDDisassemblyView;
    Last: TERDDisassemblyRequest;
    Calls, Navigations, Line: Integer;
    procedure Request(Sender: TObject; const Value: TERDDisassemblyRequest);
    procedure Navigate(Sender: TObject; const DocumentUri: string; SourceLine, Column: Integer);
  end;
  TFiberProbe = class
  public
    Calls, Selections, Clears, Inspections, Navigations: Integer;
    Last: TERDFiberRequest;
    Fiber: TERDFiberInfo;
    procedure Request(Sender: TObject; const Context: TERDFiberRequest);
    procedure Selected(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
    procedure Cleared(Sender: TObject);
    procedure Inspect(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
    procedure Navigate(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
  end;
  TDocumentEvents = class
  public
    Request: TERDDocumentSaveRequest;
    AllowDirtyClose, DestroyContentOnClose: Boolean;
    Saves, Closes: Integer;
    procedure Save(Sender: TObject; const Value: TERDDocumentSaveRequest);
    procedure Closing(Sender: TObject; Document: TERDDocument; var AllowClose: Boolean);
  end;
  TProjectTreeProbe = class(TERDProjectTree)
  public
    procedure StartMove;
    function DropAt(const Point: TPoint; Execute: Boolean; Source: TObject): Boolean;
    procedure EditName(const Text: string);
  end;
  TProjectEvents = class
  public
    Tree: TERDProjectTree;
    Items: TArray<TERDProjectItem>;
    Opens, Renames, Moves: Integer;
    LastID, LastArgument: string;
    Apply: Boolean;
    procedure Open(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem);
    procedure Rename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const Name: string);
    procedure Move(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
  end;
  TDiagnosticProbe = class
  public
    Navigations, Changes, Selections: Integer;
    ID: string;
    Context: TERDDiagnosticRequest;
    procedure Navigate(Sender: TObject; const Request: TERDDiagnosticRequest; const Value: TERDDiagnostic);
    procedure Changed(Sender: TObject);
    procedure Selected(Sender: TObject);
  end;
  TDiagnosticKeyProbe = class(TERDDiagnosticList)
  public
    procedure Press(var Key: Word; Shift: TShiftState);
  end;
  TDockProbe = class
  public
    Layouts, Activations: Integer;
    ID: string;
    procedure Layout(Sender: TObject);
    procedure Activate(Sender: TObject; const PanelID: string);
    procedure Resolve(Sender: TObject; HintControl: TControl; var Title, Description, Shortcut: string; var Glyph: TERDGlyph);
  end;
  TOfficeHintProbe = class(TERDHintWindow)
  public
    procedure Params(out Value: TCreateParams);
  end;
  TShapeProbe = class
  public
    Requests, Navigations, Inspections: Integer;
    Last: TERDShapeRequest;
    Target: Cardinal;
    PropertyName: string;
    procedure Request(Sender: TObject; const Context: TERDShapeRequest);
    procedure Navigate(Sender: TObject; const Context: TERDShapeRequest; TargetShapeID: Cardinal);
    procedure Inspect(Sender: TObject; const Context: TERDShapeRequest; const Value: TERDShapeProperty);
  end;
  TConsoleProbe = class
  public
    Calls, Inspections: Integer;
    Session: Int64;
    Text: string;
    procedure Command(Sender: TObject; Value: Int64; const CommandText: string);
    procedure Inspect(Sender: TObject; const Entry: TERDConsoleEntry);
  end;
  TRuntimeProbe = class
  public
    Calls: Integer;
    Last: TERDRuntimeRequest;
    procedure Request(Sender: TObject; const Context: TERDRuntimeRequest);
  end;
  TDebugCommandProbe = class
  public
    Calls: Integer;
    Command: TERDDebugCommand;
    procedure Execute(Sender: TObject; Value: TERDDebugCommand);
  end;
  TValueChildrenProbe = class
  public
    Calls: Integer;
    Last: TERDValueRequest;
    Synchronous: Boolean;
    procedure Request(Sender: TObject; const Value: TERDValueRequest);
  end;
  TEditKeyFilter = class
  public
    Calls: Integer;
    procedure Filter(Sender: TObject; var Key: Char);
  end;
  TTextInputProbe = class(TERDTextInput)
  public
    function EditMenu: TERDPopupMenu;
    procedure RebuildWindow;
  end;
  TPalettePopupProbe = class
    OwnerForm: TForm;
    Timer: TTimer;
    SawPopup, OwnerEnabled, NoButtons, TopAnchored: Boolean;
    Attempts: Integer;
    procedure InspectPopup(Sender: TObject);
  end;
  TTabProbe = class(TERDTabs)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure DragTo(X, Y: Integer);
    procedure PressKey(Key: Word; Shift: TShiftState);
  end;
  TTabCounter = class
    Changes, ClosedCount, Reorders: Integer;
    VetoClose, VetoMove, VetoDetach: Boolean;
    procedure Changed(Sender: TObject);
    procedure Closing(Sender: TObject; Index: Integer; var CanClose: Boolean);
    procedure Reordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
    procedure Reordered(Sender: TObject; OldIndex, NewIndex: Integer);
    procedure Detaching(Sender: TObject; Index: Integer; var Allow: Boolean);
    procedure Closed(Sender: TObject; TabID: Integer; Page: TWinControl);
  end;
  TEditProbe = class(TERDEdit)
  public
    procedure RevealByKeyboard;
  end;
  TValidationProbe = class
    Calls: Integer;
    procedure ValidateText(Sender: TObject; const Text: string;
      var State: TERDEditValidationState; var MessageText: string);
  end;
  TRatingProbe = class(TERDRating)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure PressKey(Key: Word);
    procedure PreviewAt(X, Y: Integer);
  end;
  TCommandCounter = class
    Count: Integer;
    LastKey: string;
    procedure Executed(Sender: TObject; Command: TERDCommandItem);
  end;
  TDropCounter = class
    Count, FileCount: Integer;
    FirstFile: string;
    procedure Dropped(Sender: TObject; Files: TStrings);
  end;
  TLoadingProbe = class(TERDLoadingIndicator)
  public
    procedure NextFrame;
  end;
  TLinkProbe = class(TERDLinkLabel)
  public
    procedure Activate;
    procedure PressEnter;
  end;
  TDateProbe = class(TERDDatePicker)
  public
    procedure TypeText(const Value: string);
    procedure PressKey(Value: Word);
  end;
  TTimeProbe = class(TERDTimePicker)
  public
    procedure TypeText(const Value: string);
    procedure PressKey(Value: Word);
    procedure ClickAt(X, Y: Integer);
  end;
  TCheckListProbe = class(TERDCheckListBox)
  public
    procedure PressAt(X, Y: Integer);
    procedure ReleaseAt(X, Y: Integer);
    procedure Space;
  end;
  TCheckListCounter = class
    Count, LastIndex: Integer;
    procedure Checked(Sender: TObject; Index: Integer);
  end;
  TSearchProbe = class(TERDSearchEdit)
  public
    procedure EnterSearch;
  end;
  TTrackProbe = class(TERDTrackBar)
  public
    procedure ClickAt(X, Y: Integer);
  end;
  TRangeProbe = class(TERDRangeSlider)
  public
    procedure Step(Key: Word);
  end;
  TFocusProbe = class(TERDButton)
  public
    function CuesVisible: Boolean;
  end;
  TChangeProbe = class
    Changes: Integer;
    procedure Changed(Sender: TObject);
  end;
var Checks: Integer;
procedure TTabProbe.PressAt(X, Y: Integer);
begin MouseDown(mbLeft, [], X, Y) end;
procedure TTabProbe.ReleaseAt(X, Y: Integer);
begin MouseUp(mbLeft, [], X, Y) end;
procedure TTabProbe.DragTo(X, Y: Integer);
begin MouseMove([ssLeft], X, Y) end;
procedure TTabProbe.PressKey(Key: Word; Shift: TShiftState);
begin KeyDown(Key, Shift) end;
procedure TTabCounter.Changed(Sender: TObject);
begin Inc(Changes) end;
procedure TTabCounter.Closing(Sender: TObject; Index: Integer; var CanClose: Boolean);
begin CanClose := not VetoClose end;
procedure TTabCounter.Reordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
begin Allow := not VetoMove end;
procedure TTabCounter.Reordered(Sender: TObject; OldIndex, NewIndex: Integer);
begin Inc(Reorders) end;
procedure TTabCounter.Detaching(Sender: TObject; Index: Integer; var Allow: Boolean);
begin Allow := not VetoDetach end;
procedure TTabCounter.Closed(Sender: TObject; TabID: Integer; Page: TWinControl);
begin Inc(ClosedCount) end;

procedure TEditProbe.RevealByKeyboard;
var Key: Word;
begin Key := Ord('V'); KeyDown(Key, [ssAlt]) end;
procedure TValidationProbe.ValidateText(Sender: TObject; const Text: string;
  var State: TERDEditValidationState; var MessageText: string);
begin
  Inc(Calls);
  if Text = '' then begin State := evsError; MessageText := 'Required' end
  else if Length(Text) < 4 then begin State := evsWarning; MessageText := 'Use four characters' end
  else begin State := evsSuccess; MessageText := '' end;
  TERDEdit(Sender).Validate; // Reentrant request must not call the validator again.
end;

procedure TRatingProbe.PressAt(X, Y: Integer);
begin MouseDown(mbLeft, [], X, Y) end;
procedure TRatingProbe.ReleaseAt(X, Y: Integer);
begin MouseUp(mbLeft, [], X, Y) end;
procedure TRatingProbe.PressKey(Key: Word);
begin KeyDown(Key, []) end;
procedure TRatingProbe.PreviewAt(X, Y: Integer);
begin MouseMove([], X, Y) end;

procedure TCommandCounter.Executed(Sender: TObject; Command: TERDCommandItem);
begin Inc(Count); LastKey := Command.Key end;

procedure TDropCounter.Dropped(Sender: TObject; Files: TStrings);
begin Inc(Count); FileCount := Files.Count; FirstFile := Files[0] end;

procedure TLoadingProbe.NextFrame;
begin Advance end;

procedure TLinkProbe.Activate;
begin Click end;
procedure TLinkProbe.PressEnter;
var Key: Word;
begin Key := VK_RETURN; KeyDown(Key, []) end;

procedure TDateProbe.TypeText(const Value: string);
var I: Integer; Key: Char;
begin
  for I := 1 to Length(Value) do begin Key := Value[I]; KeyPress(Key) end;
end;
procedure TDateProbe.PressKey(Value: Word);
begin KeyDown(Value, []) end;

procedure TTimeProbe.TypeText(const Value: string);
var I: Integer; Key: Char;
begin
  for I := 1 to Length(Value) do
  begin Key := Value[I]; KeyPress(Key) end;
end;
procedure TTimeProbe.PressKey(Value: Word);
begin
  KeyDown(Value, []);
end;
procedure TTimeProbe.ClickAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
  MouseUp(mbLeft, [], X, Y);
end;

procedure TCheckListProbe.PressAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
end;
procedure TCheckListProbe.ReleaseAt(X, Y: Integer);
begin
  MouseUp(mbLeft, [], X, Y);
end;
procedure TCheckListProbe.Space;
var Key: Word;
begin
  Key := VK_SPACE;
  KeyDown(Key, []);
end;
procedure TCheckListCounter.Checked(Sender: TObject; Index: Integer);
begin
  Inc(Count);
  LastIndex := Index;
end;

procedure TSearchProbe.EnterSearch;
var Key: Word;
begin
  Key := VK_RETURN;
  KeyDown(Key, []);
end;
procedure TTrackProbe.ClickAt(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
  MouseUp(mbLeft, [], X, Y);
end;
procedure TRangeProbe.Step(Key: Word);
begin
  KeyDown(Key, []);
end;

function TFocusProbe.CuesVisible: Boolean;
begin
  Result := FocusCuesVisible;
end;
procedure TChangeProbe.Changed(Sender: TObject);
begin
  Inc(Changes);
end;

procedure TLanguageServiceProbe.Request(const Value: TERDLanguageRequest; const Reply: IERDLanguageResponses);
begin Snapshot := Value; Responses := Reply; Inc(Requests); end;
procedure TLanguageServiceProbe.Cancel(RequestID: Int64);
begin Inc(Cancels); end;

procedure TCodeEditorProbe.DebugClick(Row: Integer; ToggleEnabled: Boolean);
var Shift: TShiftState; X,Y: Integer;
begin
  Shift := []; if ToggleEnabled then Include(Shift,ssShift);
  X := GutterBounds.Left+ScaleValue(10); Y := TextViewport.Top+Row*RowHeight+RowHeight div 2;
  MouseDown(mbLeft,Shift,X,Y); MouseUp(mbLeft,Shift,X,Y);
end;
procedure TCodeEditorProbe.DebugKey(Key: Word; Shift: TShiftState);
begin KeyDown(Key,Shift); end;
function TCodeEditorProbe.DebugPixel(Row: Integer; Marker: Boolean): TColor;
var Bitmap: TBitmap; X,Y: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    RenderTo(Bitmap); X := TextViewport.Right-ScaleValue(5);
    if Marker then X := GutterBounds.Left+ScaleValue(10);
    Y := TextViewport.Top+Row*RowHeight+RowHeight div 2;
    Result := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]);
  finally Bitmap.Free; end;
end;
procedure TBreakpointPanelProbe.Navigate(Sender: TObject; const DocumentUri: string; Line: Integer);
begin NavigatedUri := DocumentUri; NavigatedLine := Line; end;
procedure TBreakpointPanelProbe.CommitCell(Column: TColumnIndex; const Value: string);
begin DoNewText(FocusedNode,Column,Value); end;
procedure TStackProbe.Selected(Sender: TObject; const Context: TERDStackRequest; const Frame: TERDStackFrame);
begin Inc(Calls); LastFrame := Frame; LastContext := Context; end;
procedure TStackProbe.Navigate(Sender: TObject; const DocumentUri: string; SourceLine, Column: Integer);
begin Inc(Navigations); Line := SourceLine; end;
procedure TLocalsProbe.Scopes(Sender: TObject; const Context: TERDLocalsRequest);
var Scopes: TArray<TERDDebugScope>;
begin
  Inc(ScopeCalls); LastContext := Context;
  if Synchronous then
  begin SetLength(Scopes,1); Scopes[0].ID := 'scope.local'; Scopes[0].Name := 'Local'; Scopes[0].Reference := 100; Panel.PublishScopes(Context,Scopes); end;
end;
procedure TLocalsProbe.Variables(Sender: TObject; const Context: TERDLocalsRequest; const Request: TERDValueRequest; const Parent: TERDInspectValue);
var Values: TArray<TERDInspectValue>;
begin
  Inc(VariableCalls); LastContext := Context; LastRequest := Request; LastParent := Parent;
  if Synchronous then
  begin SetLength(Values,1); Values[0].ID := Parent.ID+'.value'; Values[0].Name := 'value'; Values[0].ValueText := '42'; Panel.PublishVariables(Context,Request,Values,False); end;
end;
procedure TWatchProbe.Evaluate(Sender: TObject; const Context: TERDWatchContext; const Expressions: TArray<TERDWatchExpression>);
var Results: TArray<TERDWatchResult>; I: Integer;
begin
  Inc(Calls); LastContext := Context; LastExpressions := Copy(Expressions);
  if Synchronous then
  begin
    SetLength(Results,Length(Expressions));
    for I := 0 to High(Expressions) do begin Results[I].ID := Expressions[I].ID; Results[I].Value.ValueText := '42'; end;
    Panel.PublishResults(Context,Results);
  end;
end;
procedure TWatchProbe.Variables(Sender: TObject; const Context: TERDWatchContext; const Request: TERDValueRequest; const Parent: TERDInspectValue);
begin Inc(VariableCalls); LastContext := Context; LastRequest := Request; end;
procedure TDisassemblyProbe.Request(Sender: TObject; const Value: TERDDisassemblyRequest);
begin Inc(Calls); Last := Value; end;
procedure TDisassemblyProbe.Navigate(Sender: TObject; const DocumentUri: string; SourceLine, Column: Integer);
begin Inc(Navigations); Line := SourceLine; end;
procedure TFiberProbe.Request(Sender: TObject; const Context: TERDFiberRequest);
begin Inc(Calls); Last := Context; end;
procedure TFiberProbe.Selected(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
begin Inc(Selections); Last := Context; Fiber := Value; end;
procedure TFiberProbe.Cleared(Sender: TObject);
begin Inc(Clears); end;
procedure TFiberProbe.Inspect(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
begin Inc(Inspections); end;
procedure TFiberProbe.Navigate(Sender: TObject; const Context: TERDFiberRequest; const Value: TERDFiberInfo);
begin Inc(Navigations); end;
procedure TProjectTreeProbe.StartMove;
var DragObject: TDragObject;
begin DragObject := nil; DoStartDrag(DragObject); end;
function TProjectTreeProbe.DropAt(const Point: TPoint; Execute: Boolean; Source: TObject): Boolean;
var Effect: Integer; Formats: TFormatArray;
begin
  Effect := 0; Result := DoDragOver(Source,[],dsDragMove,Point,dmOnNode,Effect);
  if Result and Execute then begin Formats := nil; DoDragDrop(Source,nil,Formats,[],Point,Effect,dmOnNode); end;
end;
procedure TProjectTreeProbe.EditName(const Text: string);
begin DoNewText(FocusedNode,0,Text); end;
procedure TProjectEvents.Open(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem);
begin Inc(Opens); LastID := Item.ID; end;
procedure TProjectEvents.Rename(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const Name: string);
var I: Integer;
begin
  Inc(Renames); LastID := Item.ID; LastArgument := Name;
  if Apply then begin for I := 0 to High(Items) do if Items[I].ID = Item.ID then Items[I].Name := Name;
    Tree.SetItems(Context.ProjectID,Context.Revision+1,Items); end;
end;
procedure TProjectEvents.Move(Sender: TObject; const Context: TERDProjectContext; const Item: TERDProjectItem; const ParentID: string);
var I: Integer;
begin
  Inc(Moves); LastID := Item.ID; LastArgument := ParentID;
  if Apply then begin for I := 0 to High(Items) do if Items[I].ID = Item.ID then Items[I].ParentID := ParentID;
    Tree.SetItems(Context.ProjectID,Context.Revision+1,Items); end;
end;
procedure TDiagnosticProbe.Navigate(Sender: TObject; const Request: TERDDiagnosticRequest; const Value: TERDDiagnostic);
begin Inc(Navigations); ID := Value.ID; Context := Request; end;
procedure TDiagnosticProbe.Changed(Sender: TObject);
begin Inc(Changes); end;
procedure TDiagnosticProbe.Selected(Sender: TObject);
begin Inc(Selections); end;
procedure TDiagnosticKeyProbe.Press(var Key: Word; Shift: TShiftState);
begin KeyDown(Key,Shift); end;
procedure TDockProbe.Layout(Sender: TObject);
begin Inc(Layouts); end;
procedure TDockProbe.Activate(Sender: TObject; const PanelID: string);
begin Inc(Activations); ID := PanelID; end;
procedure TDockProbe.Resolve(Sender: TObject; HintControl: TControl; var Title, Description, Shortcut: string; var Glyph: TERDGlyph);
begin Title := 'Save layout'; Description := 'Saves the current panel arrangement.'; Shortcut := 'Ctrl+S'; Glyph := glSave; end;
procedure TOfficeHintProbe.Params(out Value: TCreateParams);
begin Value := Default(TCreateParams); CreateParams(Value); end;

procedure TShapeProbe.Request(Sender: TObject; const Context: TERDShapeRequest);
begin Inc(Requests); Last := Context; end;
procedure TShapeProbe.Navigate(Sender: TObject; const Context: TERDShapeRequest; TargetShapeID: Cardinal);
begin Inc(Navigations); Last := Context; Target := TargetShapeID; end;
procedure TShapeProbe.Inspect(Sender: TObject; const Context: TERDShapeRequest; const Value: TERDShapeProperty);
begin Inc(Inspections); Last := Context; PropertyName := Value.Name; end;

procedure TConsoleProbe.Command(Sender: TObject; Value: Int64; const CommandText: string);
begin Inc(Calls); Session := Value; Text := CommandText; end;
procedure TConsoleProbe.Inspect(Sender: TObject; const Entry: TERDConsoleEntry);
begin Inc(Inspections); Text := Entry.Text; end;

procedure TRuntimeProbe.Request(Sender: TObject; const Context: TERDRuntimeRequest);
begin Inc(Calls); Last := Context; end;
procedure TDebugCommandProbe.Execute(Sender: TObject; Value: TERDDebugCommand);
begin Inc(Calls); Command := Value; end;
procedure TValueChildrenProbe.Request(Sender: TObject; const Value: TERDValueRequest);
var Children: TArray<TERDInspectValue>;
begin
  Inc(Calls); Last := Value;
  if Synchronous then
  begin
    SetLength(Children,1); Children[0].ID := Value.ParentID+'.sync'; Children[0].Name := 'sync';
    TERDValueTree(Sender).PublishChildren(Value,Children,False);
  end;
end;
procedure TCodeEditorProbe.FoldMouse(Action: Integer; Outside: Boolean);
var View: TRect; X, Y: Integer;
begin
  View := TextViewport; X := View.Left - ScaleValue(13); Y := View.Top + RowHeight div 2;
  if Outside then X := View.Left + ScaleValue(30);
  case Action of
    0: Perform(WM_MOUSEMOVE, 0, MakeLParam(X, Y));
    1: Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(X, Y));
    2: Perform(WM_LBUTTONUP, 0, MakeLParam(X, Y));
  end;
end;
procedure TCodeEditorProbe.GutterMouse(Action, Row: Integer; Shift: Boolean);
var View: TRect; X, Y: Integer; Flags: WPARAM;
begin
  View := TextViewport; X := View.Left - ScaleValue(30);
  Y := View.Top + Row * RowHeight + RowHeight div 2;
  Flags := 0; if Shift then Flags := MK_SHIFT;
  case Action of
    0: Perform(WM_LBUTTONDOWN, Flags or MK_LBUTTON, MakeLParam(X, Y));
    1: Perform(WM_MOUSEMOVE, Flags or MK_LBUTTON, MakeLParam(X, Y));
    2: Perform(WM_LBUTTONUP, Flags, MakeLParam(X, Y));
    3: Perform(WM_LBUTTONDBLCLK, Flags or MK_LBUTTON, MakeLParam(X, Y));
  end;
end;
procedure TCodeEditorProbe.DestroyWnd;
begin
  if HandleAllocated and (ShutdownModelAlive <> nil) then
  begin
    ShutdownModelAlive^ := TextModel <> nil;
    if ShutdownModelAlive^ then ShutdownModelAlive^ := Text = 'shutdown';
  end;
  inherited;
end;
procedure TCodeEditorProbe.GetNavigationRects(out Vertical, Horizontal, Map: TRect);
begin NavigationRects(Vertical, Horizontal, Map); end;
function TCodeEditorProbe.HorizontalOffset: Integer;
begin Result := HorizontalScrollPosition; end;
function TCodeEditorProbe.ViewWidth: Integer;
begin Result := TextViewport.Width; end;
function TCodeEditorProbe.GutterBackground: TColor;
begin Result := ColorToRGB(GutterBackgroundColor); end;
function TCodeEditorProbe.EditorBackground: TColor;
begin Result := ColorToRGB(Palette.GaugeFace); end;
function TCodeEditorProbe.MinimapBackground: TColor;
begin Result := ColorToRGB(MinimapBackgroundColor); end;

function TCodeEditorProbe.FoldPixel(Guide: Boolean): TColor;
var Bitmap: TBitmap; View: TRect; X, Y: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    RenderTo(Bitmap); View := TextViewport;
    X := View.Left - ScaleValue(13); Y := View.Top + RowHeight + 1;
    if not Guide then
    begin Dec(X, ScaleValue(3)); Y := View.Top + RowHeight div 2 - ScaleValue(3); end;
    Result := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
  finally Bitmap.Free; end;
end;
function TCodeEditorProbe.TopFoldGuidePixel: TColor;
var Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    RenderTo(Bitmap);
    Result := ColorToRGB(Bitmap.Canvas.Pixels[TextViewport.Left - ScaleValue(13), GutterBounds.Top + 1]);
  finally Bitmap.Free; end;
end;
function TCodeEditorProbe.IndentGuidePixel(Row: Integer): TColor;
var Bitmap: TBitmap; Line: TERDTextLayoutLine; X, Y: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    RenderTo(Bitmap);
    Bitmap.Canvas.Font.Assign(Font);
    Line := TERDTextLayoutLine.Create(Bitmap.Canvas, StringOfChar(' ', TabWidth + 1));
    try X := TextViewport.Left + Line.X(TabWidth) - HorizontalScrollPosition;
    finally Line.Free; end;
    Y := TextViewport.Top + Row * RowHeight + 1;
    Result := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
  finally Bitmap.Free; end;
end;
function TCodeEditorProbe.RowCount: Integer;
begin Result := Length(VisualRows); end;

procedure Check(Value: Boolean; const Text: string);
begin
  Inc(Checks);
  if not Value then raise Exception.Create(Text);
end;
procedure TestIconLifetimeAndStreaming;
var Button, CopyButton: TERDButton; Images: TImageList; Bitmap: TBitmap; Stream: TMemoryStream;
begin
  RegisterClass(TERDButton);
  Button := TERDButton.Create(nil);
  try
    Button.Icon.BuiltIn := ei249Teletext;
    Check(Button.Icon.Available, 'Catalog icon available');
    Button.Icon.Source := eisNone;
    Check(not Button.Icon.Available(glCheck), 'None does not fall back');
    Images := TImageList.Create(nil);
    try
      Bitmap := TBitmap.Create;
      try
        Bitmap.SetSize(16,16); Bitmap.Canvas.Brush.Color := clRed;
        Bitmap.Canvas.FillRect(Rect(0,0,16,16));
        Images.Add(Bitmap,nil);
        Button.Icon.Glyph.Assign(Bitmap);
      finally Bitmap.Free end;
      Button.Icon.Source := eisImageList; Button.Icon.Images := Images; Button.Icon.ImageIndex := 0;
      Check(Button.Icon.Available, 'ImageList source available');
      Button.Icon.ImageIndex := 50;
      Check(not Button.Icon.Available(glCheck), 'Invalid index does not change sources');
    finally Images.Free end;
    Check(Button.Icon.Images = nil, 'Freed ImageList clears reference');
    Button.Icon.Source := eisGlyph;
    Check(Button.Icon.Available, 'Own glyph survives source bitmap destruction');
    Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Button); Stream.Position := 0;
      CopyButton := TERDButton(Stream.ReadComponent(nil));
      try
        Check(CopyButton.Icon.Source = eisGlyph, 'Icon source streams');
        Check(CopyButton.Icon.Available, 'Picture streams');
        Check(CopyButton.Icon.Glyph.Width = 16, 'Picture dimensions preserved');
      finally CopyButton.Free end;
    finally Stream.Free end;
    Button.Icon.Source := eisBuiltIn;
    Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Button); Stream.Position := 0;
      CopyButton := TERDButton(Stream.ReadComponent(nil));
      try Check(CopyButton.Icon.BuiltIn = ei249Teletext, 'Catalog ID streams');
      finally CopyButton.Free end;
    finally Stream.Free end;
  finally Button.Free end;
end;
procedure TestRibbonHostedControls;
var Form: TForm; Ribbon: TERDRibbon; Host: TERDPanel; Edit: TERDEdit;
  Group, Other: TERDRibbonGroup; Bitmap: TBitmap; Original: TRect; Rejected: Boolean;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  try
    Form.ClientWidth := 900; Form.ClientHeight := 500;
    Host := TERDPanel.Create(Form); Host.Parent := Form; Host.Top := 180;
    Edit := TERDEdit.Create(Form); Edit.Parent := Host;
    Edit.SetBounds(10, 10, 180, 26); Edit.Text := 'Retained input';
    Original := Edit.BoundsRect;
    Ribbon := TERDRibbon.Create(Form); Ribbon.Parent := Form;
    Group := Ribbon.Tabs.Add.Groups.Add; Group.Caption := 'Input';
    Group.ControlWidth := 220; Group.Control := Edit;
    Other := Ribbon.Tabs[0].Groups.Add;
    Ribbon.ActiveTab := 0;
    Form.Show; Application.ProcessMessages;
    Bitmap.SetSize(Ribbon.Width, Ribbon.Height);
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check((Edit.Parent = Ribbon) and Edit.Visible and (Edit.Text = 'Retained input'),
      'Classic ribbon hosts the original input instance');
    Rejected := False;
    try Other.Control := Edit except on E: EArgumentException do Rejected := True end;
    Check(Rejected and (Other.Control = nil) and (Group.Control = Edit),
      'A control cannot bind to two ribbon groups');
    Group.Visible := False;
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(not Edit.Visible, 'Hidden ribbon group hides its hosted input');
    Group.Visible := True;
    Ribbon.RibbonStyle := rsSimplified;
    Bitmap.SetSize(Ribbon.Width, Ribbon.Height);
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(Edit.Visible and (Edit.Parent = Ribbon), 'Simplified ribbon hosts fitting controls');
    Ribbon.Collapsed := True;
    Ribbon.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Check(not Edit.Visible, 'Collapsed ribbon hides inline controls');
    Ribbon.Collapsed := False;
    Group.Control := nil;
    Check((Edit.Parent = Host) and Edit.Visible and EqualRect(Edit.BoundsRect, Original) and
      (Edit.Owner = Form) and (Edit.Text = 'Retained input'), 'Unbinding restores layout without destroying input');
    Group.Control := Edit;
    Edit.Free;
    Check(Group.Control = nil, 'FreeNotification clears freed hosted input');
  finally Bitmap.Free; Form.Free end;
end;

procedure TestHostedStatusPanels;
var
  Form: TForm; Status: TERDStatusBar; Host, OriginalParent: TERDPanel;
  First, Second: TERDStatusPanel; Badge: TERDBadge; Button: TERDButton;
  OriginalBounds: TRect;
begin
  Form := TForm.CreateNew(nil);
  try
    OriginalParent := TERDPanel.Create(Form); OriginalParent.Parent := Form;
    Host := TERDPanel.Create(Form); Host.Parent := OriginalParent; Host.SetBounds(10,12,250,44);
    OriginalBounds := Host.BoundsRect;
    Badge := TERDBadge.Create(Form); Badge.Parent := Host; Badge.Count := 4;
    Badge := TERDBadge.Create(Form); Badge.Parent := Host; Badge.Count := 12;
    Button := TERDButton.Create(Form); Button.Parent := Host; Button.Caption := 'Action';
    Status := TERDStatusBar.Create(Form); Status.Parent := Form;
    First := Status.Panels.Add; First.Width := 280; First.Control := Host;
    Check(First.Kind = spkControl, 'Hosting selects control panel');
    Check(Host.Parent = Status, 'Container is hosted by status bar');
    Check(Host.ControlCount = 3, 'One status panel contains two badges and a button');
    Second := Status.Panels.Add;
    try
      Second.Control := Host;
      Check(False, 'Duplicate host assignment must fail');
    except on E: EInvalidOperation do Check(True, 'Duplicate host rejected') end;
    try
      Second.Control := Form;
      Check(False, 'Ancestor host must fail');
    except on E: EInvalidOperation do Check(True, 'Ancestor hosting rejected') end;
    First.Control := nil;
    Check(Host.Parent = OriginalParent, 'Detach restores parent');
    Check((Host.Left = OriginalBounds.Left) and (Host.Top = OriginalBounds.Top) and
      (Host.Width = OriginalBounds.Width) and (Host.Height = OriginalBounds.Height), 'Detach restores bounds');
    First.Control := Host;
    OriginalParent.Free;
    First.Control := nil;
    Check(Host.Parent = nil, 'Destroyed original parent does not leave dangling reference');
    First.Control := Host;
    Host.Free;
    Check(First.Control = nil, 'Destroyed hosted control clears reference');
    // Status destruction with a live host must detach safely.
    Host := TERDPanel.Create(Form); Host.Parent := Form; First.Control := Host;
    Status.Free;
    Check(Host.Parent = Form, 'Status bar destruction restores child parent');
  finally Form.Free end;
end;
procedure TestThemesAndRibbon;
var Theme: TERDTheme; Ribbon: TERDRibbon; Mode: TERDThemeMode;
  Appearance: TERDRibbonAppearance; Layout: TERDRibbonStyle;
  Bitmap: TBitmap; P: TERDThemePalette;
begin
  Theme := TERDTheme.Create(nil); Ribbon := TERDRibbon.Create(nil); Bitmap := TBitmap.Create;
  try
    Check(Theme.Mode = tmERDesignsLight, 'Default is ERDesigns Light');
    Ribbon.Theme := Theme;
    Ribbon.Width := 900;
    with Ribbon.Tabs.Add do
    begin
      Caption := 'Test';
      with Groups.Add do
      begin
        Caption := 'Actions';
        with Items.Add do begin Caption := 'Play'; Icon.BuiltIn := ei161Afspelen end;
      end;
    end;
    for Mode := tmERDesignsLight to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode; P := Theme.Palette;
      Check(P.Background <> P.ForegroundText, ERDThemeName(Mode) + ' readable foreground');
      for Appearance := Low(TERDRibbonAppearance) to High(TERDRibbonAppearance) do
        for Layout := Low(TERDRibbonStyle) to High(TERDRibbonStyle) do
        begin
          Ribbon.Appearance := Appearance; Ribbon.RibbonStyle := Layout;
          Ribbon.RenderTo(Bitmap);
          Check((Bitmap.Width > 0) and (Bitmap.Height > 0), 'Ribbon renders for theme/shape/layout');
          Check(Theme.Mode = Mode, 'Ribbon appearance leaves theme selection intact');
        end;
    end;
  finally Bitmap.Free; Ribbon.Free; Theme.Free end;
end;

procedure TestPopupRowsAndCommandSwitch;
var
  Form: TForm;
  Popup: TERDPopupList;
  TitleBar: TERDTitleBar;
  Ribbon: TERDRibbon;
  Items: TStringList;
  Bitmap: TBitmap;
  R: TRect;
  X, Y, InkPixels, I, Mismatches: Integer;
  LiveDC: HDC;
begin
  Form := TForm.CreateNew(nil);
  Items := TStringList.Create;
  Bitmap := TBitmap.Create;
  try
    TitleBar := TERDTitleBar.Create(Form);
    TitleBar.Parent := Form;
    Ribbon := TERDRibbon.Create(Form);
    Ribbon.Parent := Form;
    TitleBar.Ribbon := Ribbon;
    TitleBar.CommandStyle := csRibbon;
    Check(Ribbon.Visible, 'Ribbon shown in ribbon command mode');
    TitleBar.CommandStyle := csMenu;
    Check(not Ribbon.Visible, 'Ribbon hidden in menu command mode');
    TitleBar.CommandStyle := csRibbon;
    Check(Ribbon.Visible, 'Ribbon restored after switching back');
    Popup := TERDPopupList.Create(Form);
    Items.Add('First item');
    Items.Add('Visible second item');
    Popup.Popup(Form, Rect(100, 100, 320, 130), Items, 0,
      BRAND_PALETTE_LIGHT, 96, 28, 8);
    Bitmap.SetSize(Popup.ClientWidth, Popup.ClientHeight);
    SendMessage(Popup.Handle, WM_PRINTCLIENT, Bitmap.Canvas.Handle, PRF_CLIENT);
    SendMessage(Popup.Handle, LB_GETITEMRECT, 1, LPARAM(@R));
    InkPixels := 0;
    for Y := R.Top + 4 to R.Bottom - 5 do
      for X := R.Left + 8 to R.Right - 20 do
        if ColorToRGB(Bitmap.Canvas.Pixels[X, Y]) <>
          ColorToRGB(BRAND_PALETTE_LIGHT.GaugeFace) then Inc(InkPixels);
    Check(InkPixels > 10, 'Unselected popup row paints visible text');
    SendMessage(Popup.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    SendMessage(Popup.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    Check(not Popup.IsOpen, 'Mouse selection closes the popup');
    Check(Popup.ItemIndex = 1, 'Mouse selection accepts the clicked row');
    Items.Clear;
    for I := 0 to 79 do Items.Add('Completion item ' + IntToStr(I));
    Popup.Popup(Form, Rect(100, 100, 420, 130), Items, 0, BRAND_PALETTE_LIGHT, 96, 24, 8);
    RedrawWindow(Popup.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    Popup.TopIndex := 12;
    Bitmap.SetSize(Popup.ClientWidth, Popup.ClientHeight);
    SendMessage(Popup.Handle, WM_PRINTCLIENT, Bitmap.Canvas.Handle, PRF_CLIENT);
    LiveDC := GetDC(Popup.Handle);
    try
      Mismatches := 0;
      for Y := 0 to Popup.ClientHeight - 1 do
        for X := Popup.ClientWidth - 10 to Popup.ClientWidth - 1 do
          if GetPixel(LiveDC, X, Y) <> ColorToRGB(Bitmap.Canvas.Pixels[X, Y]) then Inc(Mismatches);
      Check(Mismatches = 0, 'Scrolling presents final scrollbar without an intermediate shifted thumb');
    finally ReleaseDC(Popup.Handle, LiveDC); end;

  finally
    Bitmap.Free;
    Items.Free;
    Form.Free;
  end;
end;

procedure TestComboMouseChange;
var
  Form: TForm;
  Combo: TERDComboBox;
  Popup: TERDPopupList;
  Probe: TChangeProbe;
  I: Integer;
  R: TRect;
begin
  Form := TForm.CreateNew(nil);
  Probe := TChangeProbe.Create;
  try
    Combo := TERDComboBox.Create(Form);
    Combo.Parent := Form;
    Combo.Items.Add('First');
    Combo.Items.Add('Second');
    Combo.ItemIndex := 0;
    Combo.OnChange := Probe.Changed;
    Combo.DropDown;
    Popup := nil;
    for I := 0 to Combo.ComponentCount - 1 do
      if Combo.Components[I] is TERDPopupList then
        Popup := TERDPopupList(Combo.Components[I]);
    Check(Popup <> nil, 'Combo creates its dropdown');
    SendMessage(Popup.Handle, LB_GETITEMRECT, 1, LPARAM(@R));
    SendMessage(Popup.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    SendMessage(Popup.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + 20, R.Top + 12)));
    Check(Combo.ItemIndex = 1, 'Combo accepts a mouse-selected item');
    Check(Probe.Changes = 1, 'Mouse selection fires OnChange exactly once');
    Check(not Combo.DroppedDown, 'Combo closes after mouse selection');
  finally
    Form.Free;
    Probe.Free;
  end;
end;

procedure TestDemoLayoutAndSpinMargins;
var
  Form: TShowcaseForm;
  Spin: TERDSpinEdit;
  NativeEdit: TERDTextInput;
  Caret: TPoint;
  I: Integer;
begin
  Form := TShowcaseForm.Create(nil);
  try
    for I := 1 to 3 do
    begin
      Form.CommandChoice.ItemIndex := 1;
      Form.CommandSelected(Form.CommandChoice);
      Check(Form.Choices.Top = Form.TitleBar.Height, 'Choices follow titlebar in menu mode');
      Form.CommandChoice.ItemIndex := 0;
      Form.CommandSelected(Form.CommandChoice);
      Check(Form.Ribbon.Top = Form.TitleBar.Height, 'Ribbon follows titlebar');
      Check(Form.Choices.Top = Form.Ribbon.Top + Form.Ribbon.Height,
        'Choices remain below ribbon after command switching');
    end;
    Check(Form.Ribbon.Backstage = Form.DemoBackstage, 'Ribbon File is linked to the real backstage');
    Form.ShowBackstage(nil);
    Check(Form.DemoBackstage.Visible, 'Backstage opens');
    Check(not Form.Sidebar.Visible and not Form.Choices.Visible, 'Backstage replaces the demo navigation');
    Form.DemoBackstage.Close;
    Check(Form.Sidebar.Visible and Form.Choices.Visible, 'Closing backstage restores navigation');
    Check(Form.Choices.Top = Form.Ribbon.Top + Form.Ribbon.Height, 'Closing backstage restores chrome order');
    Form.Theme.Mode := tmERDesignsDark;
    Check(Form.ThemeChoice.Items.Count = Ord(High(TERDThemeMode)) - Ord(tmERDesignsLight) + 1,
      'Demo lists every explicit theme');
    Check(Form.DemoDialog.Palette.Background = Form.Theme.Palette.Background,
      'General dialog follows the selected theme');
    Check(Form.DemoOpenDialog.Palette.Background = Form.Theme.Palette.Background,
      'File dialog follows the selected theme');
    Check(Form.DemoToastManager.Palette.Background = Form.Theme.Palette.Background,
      'Toast follows the selected theme');
    Check(Form.DemoHintStyle.Palette.Background = Form.Theme.Palette.Background,
      'Hint follows the selected theme');
    Form.Theme.Mode := tmOfficeLight;
    Check(Form.DemoToastManager.Palette.Background = Form.Theme.Palette.Background,
      'Toast follows a later theme change');
    Form.PageTabs.TabIndex := 1;
    Check(Form.InputsPage.Visible and not Form.IconsPage.Visible, 'ERD tabs select the input page');
    Spin := Form.DemoSpinEdit;
    Spin.Alignment := taLeftJustify;
    Spin.Alignment := taRightJustify;
    Spin.Mono := True;
    NativeEdit := nil;
    for I := 0 to Spin.ControlCount - 1 do
      if Spin.Controls[I] is TERDTextInput then
        NativeEdit := TERDTextInput(Spin.Controls[I]);
    Check(NativeEdit <> nil, 'SpinEdit hosts the self-painted ERD text engine');
    Form.Show;
    Spin.Value := 18;
    Spin.SetFocus;
    Spin.SelStart := Length(Spin.Text);
    Spin.SelLength := 0;
    Check(GetCaretPos(Caret), 'Native spin caret is available');
    Check(Caret.X = NativeEdit.ClientWidth - MulDiv(2, NativeEdit.CurrentPPI, 96),
      'Right-aligned caret is at the text edge after the final digit');
  finally
    Form.Free;
  end;
end;

procedure TestWindowsFocusCuesAndAutoSave;
var
  Form: TForm;
  Button: TFocusProbe;
  Title: TERDTitleBar;
  Probe: TChangeProbe;
  AlwaysShow: BOOL;
  R, Wider: TRect;
  P: TPoint;
  Panel: TERDPanel;
  I: Integer;
  Mode: TERDThemeMode;
  SavedState: Boolean;
  Stream: TMemoryStream;
  Restored: TERDTitleBar;
begin
  Form := TForm.CreateNew(nil);
  Probe := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  try
    Form.ClientWidth := 1000;
    Button := TFocusProbe.Create(Form);
    Button.Parent := Form;
    AlwaysShow := False;
    SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @AlwaysShow, 0);
    SendMessage(Button.Handle, WM_QUERYUISTATE, 0, 0);
    SendMessage(Form.Handle, WM_CHANGEUISTATE, UIS_SET or (UISF_HIDEFOCUS shl 16), 0);
    Check(Button.CuesVisible = Boolean(AlwaysShow), 'Hidden focus cues respect Windows accessibility setting');
    SendMessage(Form.Handle, WM_CHANGEUISTATE, UIS_CLEAR or (UISF_HIDEFOCUS shl 16), 0);
    Check(Button.CuesVisible, 'Cleared Windows hide-focus state reveals cues');
    Title := TERDTitleBar.Create(Form);
    Title.Parent := Form;
    Title.Width := 900;
    Check(not Title.AutoSaveVisible, 'AutoSave starts hidden');
    Title.AutoSaveVisible := True;
    Title.OnAutoSaveChange := Probe.Changed;
    R := Title.AutoSaveBounds;
    Check(not R.IsEmpty, 'Visible AutoSave has a hit target');
    Check(Title.AutoSavePadding = 8, 'AutoSave defaults to 8 logical pixels between caption and toggle');
    Title.AutoSavePadding := 24;
    Wider := Title.AutoSaveBounds;
    Check(Wider.Width = R.Width + MulDiv(16, Title.CurrentPPI, 96), 'Padding adds the requested space without stretching the caption');
    Title.AutoSavePadding := 8;
    Title.AutoSaveCaption := 'Automatically save changes';
    Wider := Title.AutoSaveBounds;
    Check(Wider.Width > R.Width, 'Longer AutoSave captions reserve more space');
    Title.AutoSaveCaption := 'AutoSave';
    Panel := TERDPanel.Create(Form);
    Panel.Parent := Form;
    Panel.Align := alClient;
    Title.Parent := Panel;
    P := Title.ClientToScreen(Point(Title.Width div 2, Title.Height div 2));
    Check(SendMessage(Title.Handle, WM_NCHITTEST, 0,
      LPARAM(MakeLParam(P.X, P.Y))) = HTCAPTION, 'Nested titlebar exposes a native caption drag hit');
    SendMessage(Title.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
      LPARAM(MakeLParam(R.Left + R.Width div 2, R.Top + R.Height div 2)));
    SendMessage(Title.Handle, WM_LBUTTONUP, 0,
      LPARAM(MakeLParam(R.Left + R.Width div 2, R.Top + R.Height div 2)));
    Check(Title.AutoSave and (Probe.Changes = 1), 'Mouse toggle changes AutoSave exactly once');
    SendMessage(Title.Handle, WM_KEYDOWN, VK_SPACE, 0);
    SendMessage(Title.Handle, WM_KEYDOWN, VK_SPACE, $40000000);
    SendMessage(Title.Handle, WM_KEYUP, VK_SPACE, 0);
    Check(not Title.AutoSave and (Probe.Changes = 2), 'Space toggles AutoSave exactly once');
    Title.AutoSave := True;
    Title.AutoSaveVisible := False;
    Check(Title.AutoSaveBounds.IsEmpty, 'Hidden AutoSave releases caption space');
    Title.AutoSaveVisible := True;
    SavedState := Title.AutoSave;
    for I := 1 to 2 do
    begin
      Title.CommandStyle := csMenu;
      Title.CommandStyle := csRibbon;
    end;
    Check(Title.AutoSave = SavedState, 'Command switching preserves AutoSave state');
    Title.OnAutoSaveChange := nil;
    RegisterClass(TERDTitleBar);
    Stream.WriteComponent(Title);
    Stream.Position := 0;
    Restored := TERDTitleBar(Stream.ReadComponent(nil));
    try
      Check(Restored.AutoSaveVisible and Restored.AutoSave, 'AutoSave survives DFM streaming');
      Check(Restored.AutoSavePadding = 8, 'AutoSave padding survives DFM streaming');
    finally
      Restored.Free;
    end;
    for Mode := tmOfficeWordLight to High(TERDThemeMode) do
      Check(ERDThemeName(Mode) <> '', 'Every Office profile has a display name');
  finally
    Stream.Free;
    Form.Free;
    Probe.Free;
  end;
end;

procedure TestShowcaseDockingNavigation;
var Form: TShowcaseForm; I, DockIndex: Integer; Item: TERDSidebarItem;
    Host: TERDDockHost; Project: TERDProjectTree; Documents: TERDDocumentHost; Editor: TERDCodeEditor; R: TRect;
begin
  Form := TShowcaseForm.Create(nil);
  try
    DockIndex := -1; Item := nil; Host := nil;
    Check(Form.PageTabs.Tabs.Count = 13, 'Demo loads thirteen tabs without runtime duplicates');
    for I := 0 to Form.PageTabs.Tabs.Count-1 do
      if Form.PageTabs.Tabs[I].Caption = 'Docking' then DockIndex := I;
    Check(DockIndex = Form.PageTabs.Tabs.Count-1, 'Docking is the final demo page');
    for I := 0 to Form.Sidebar.Items.Count-1 do
      if Form.Sidebar.Items[I].Caption = 'Docking' then Item := Form.Sidebar.Items[I];
    Check((Item <> nil) and Item.Visible and Item.Enabled and (Item.Index = 1) and
      (Item.Tag = -DockIndex), 'Visible docking sidebar entry targets the actual docking tab');
    Check((Form.DockPage <> nil) and (Form.PageTabs.Tabs[DockIndex].Page = Form.DockPage), 'Docking tab is bound to a live demo page');
    Form.Show; Application.ProcessMessages;
    Form.SidebarSelected(Form.Sidebar,Item);
    Check((Form.PageTabs.TabIndex = DockIndex) and Form.DockPage.Visible and
      not Form.IconsPage.Visible, 'Sidebar navigation opens the docking demo');
    R := Form.PageTabs.TabBounds(DockIndex);
    Check((R.Left >= 0) and (R.Right <= Form.PageTabs.ClientWidth), 'Selecting docking reveals the overflowed final tab');
    for I := 0 to Form.DockPage.ComponentCount-1 do
      if Form.DockPage.Components[I] is TERDDockHost then Host := TERDDockHost(Form.DockPage.Components[I]);
    Check((Host <> nil) and (Host.PanelCount = 6) and (Host.ClientWidth > 0) and
      (Host.ClientHeight > 0), 'Docking demo contains its six panels in a visible viewport');
    Project := nil;
    for I := 0 to Form.DockPage.ComponentCount-1 do
      if Form.DockPage.Components[I] is TERDProjectTree then Project := TERDProjectTree(Form.DockPage.Components[I]);
    Check((Project <> nil) and (Project.ItemCount = 9), 'Docking demo hosts the reusable project tree instead of project text');
    Documents := Host.FindPanel('editor').Control as TERDDocumentHost;
    Project.SelectItem('template'); Project.OpenSelected; Application.ProcessMessages;
    Editor := Documents.ActiveDocument.Editor;
    Check(Editor.ReadOnly and (Editor.DocumentUri = 'demo://board.template.json'), 'Project demo opens protected templates read-only');
    Project.SelectItem('main'); Project.OpenSelected; Application.ProcessMessages;
    Editor := Documents.ActiveDocument.Editor;
    Check(not Editor.ReadOnly and (Editor.DocumentUri = 'demo://main.ts'), 'Opening an editable project resource restores editor typing');
    Form.PageTabs.TabIndex := 0; Check(not Form.DockPage.Visible, 'Leaving docking hides its preview');
    Form.PageTabs.TabIndex := DockIndex; Check(Form.DockPage.Visible, 'Selecting the last tab directly opens docking');
  finally Form.Free; end;
end;

procedure TestVirtualTreeDensity;
var Form: TForm; Theme, OtherTheme: TERDTheme; Host: TERDPanel; Tree, Direct, CopyTree: TERDVirtualStringTree;
    Input: TERDEdit; Source: IERDDensitySource; Aware: IERDDensityAware;
    Node, Custom: PVirtualNode; Stream: TMemoryStream; Delta: Integer;
begin
  Form := TForm.Create(nil);
  try
    Theme := TERDTheme.Create(Form); Theme.Density := dnDesktop;
    Host := TERDPanel.Create(Form); Host.Parent := Form; Host.Theme := Theme;
    Tree := TERDVirtualStringTree.Create(Host); Tree.Parent := Host;
    Direct := TERDVirtualStringTree.Create(Form); Direct.Parent := Form;
    Input := TERDEdit.Create(Tree); Input.Parent := Tree;
    Check(Supports(Tree,IERDDensitySource,Source) and Supports(Tree,IERDDensityAware,Aware), 'ERD tree adapter implements the shared density contract');
    Source := nil; Aware := nil;
    Check(Tree.ParentDensity and (Tree.Density = dnDesktop), 'ERD trees inherit density by default');
    Node := Tree.AddChild(nil); Custom := Tree.AddChild(nil); Tree.NodeHeight[Custom] := MulDiv(80,Tree.CurrentPPI,96);
    Delta := DensityMetrics(dnTablet).Edit-DensityMetrics(dnDesktop).Edit;
    Host.Density := dnTablet;
    Check((Tree.Density = dnTablet) and (Input.Density = dnTablet), 'Owner density propagates across the tree adapter to ERD editor controls');
    Check((Tree.VerticalScrollBar.Density = dnTablet) and (Tree.HorizontalScrollBar.Density = dnTablet), 'Owned ERD tree scrollbars inherit density');
    Check((Tree.DefaultNodeHeight = MulDiv(32+Delta,Tree.CurrentPPI,96)) and
      (Tree.Header.Height = MulDiv(30+Delta,Tree.CurrentPPI,96)), 'Tree density adds touch spacing before DPI scaling');
    Check((Node.NodeHeight = Tree.DefaultNodeHeight) and (Custom.NodeHeight = MulDiv(80,Tree.CurrentPPI,96)), 'Density remeasures default rows while retaining custom node heights');
    Tree.Density := dnDesktop;
    Check(not Tree.ParentDensity and (Tree.DefaultNodeHeight = MulDiv(32,Tree.CurrentPPI,96)) and
      (Input.Density = dnDesktop), 'A local tree density overrides its composite owner and reaches editor controls');
    Host.Density := dnDesktop; Host.Density := dnTablet;
    Check(Tree.Density = dnDesktop, 'Owner changes respect explicit tree overrides');
    Tree.ParentDensity := True; Check(Tree.Density = dnTablet, 'ParentDensity restores owner inheritance');
    Theme.Density := dnTablet;
    Check((Direct.Density = dnTablet) and (Direct.DefaultNodeHeight = MulDiv(32+Delta,Direct.CurrentPPI,96)), 'Standalone trees follow theme density notifications');
    Host.Density := dnDesktop;
    Check(Tree.Density = dnDesktop, 'Composite owner override takes precedence over theme density');
    Direct.RowHeight := 40; Direct.HeaderHeight := 36; Theme.Density := dnDesktop; Theme.Density := dnTablet;
    Check((Direct.RowHeight = 40) and (Direct.DefaultNodeHeight = MulDiv(40+Delta,Direct.CurrentPPI,96)) and
      (Direct.Header.Height = MulDiv(36+Delta,Direct.CurrentPPI,96)), 'Custom logical tree dimensions are retained across density changes');
    Direct.Density := dnDesktop; Theme.Density := dnDesktop; Theme.Density := dnTablet;
    Check((Direct.Density = dnDesktop) and (Direct.DefaultNodeHeight = MulDiv(40,Direct.CurrentPPI,96)), 'Theme notifications respect local tree density');
    RegisterClass(TERDVirtualStringTree); CopyTree := TERDVirtualStringTree.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Direct); Stream.Position := 0; Stream.ReadComponent(CopyTree);
      Check(not CopyTree.ParentDensity and (CopyTree.Density = dnDesktop) and (CopyTree.RowHeight = 40), 'DFM preserves explicit density and desktop logical sizes');
    finally Stream.Free; CopyTree.Free; end;
    OtherTheme := TERDTheme.Create(Form); OtherTheme.Density := dnDesktop;
    Input := TERDEdit.Create(Direct); Input.Parent := Direct;
    Direct.ParentDensity := True; Direct.Theme := OtherTheme;
    Check((Direct.Density = dnDesktop) and (Input.Density = dnDesktop) and
      (Direct.DefaultNodeHeight = MulDiv(40,Direct.CurrentPPI,96)), 'Replacing the tree theme also refreshes inherited density and editor parts');
  finally Form.Free; end;
end;

procedure TestVirtualTreeThemeAndLazyNodes;
var
  Form: TShowcaseForm;
  Tree: TERDVirtualStringTree;
  Node, Child: PVirtualNode;
  Mode: TERDThemeMode;
  TextBefore, TextAfter: string;
  Rect: TRect;
  Snapshot: TBitmap;
  Input: TControl;
  function CellInput: TControl;
  var I: Integer; Host: TWinControl;
  begin
    Result := nil;
    for I := 0 to Tree.ControlCount - 1 do
      if Tree.Controls[I] is TERDPanel then
      begin
        Host := TWinControl(Tree.Controls[I]);
        if Host.Visible and (Host.ControlCount > 0) then Exit(Host.Controls[0]);
      end;
  end;
begin
  Form := TShowcaseForm.Create(nil);
  Snapshot := TBitmap.Create;
  try
    Tree := Form.DemoVirtualStringTree;
    Check(Tree.RootNodeCount = 20, 'Virtual tree has twenty lazy root groups');
    Node := Tree.GetFirst;
    Check(Node <> nil, 'First virtual node exists');
    Tree.ValidateNode(Node, False);
    Check(vsHasChildren in Node.States, 'Root advertises lazy children');
    Check(Node.ChildCount = 0, 'Collapsed group has no initialized channel nodes');
    Tree.Expanded[Node] := True;
    Check(Tree.Handle <> 0, 'Tree window is created');
    Check(Tree.VerticalScrollBar.Visible, 'Expanded tree uses its ERD vertical scrollbar');
    Check((GetWindowLong(Tree.Handle, GWL_STYLE) and WS_VSCROLL) = 0, 'Native vertical scrollbar is absent');
    Tree.OffsetY := 0;
    Tree.Perform(WM_MOUSEWHEEL, WPARAM($FF880000), 0);
    Check(Tree.OffsetY < 0, 'Mouse wheel scrolls expanded virtual tree');
    Check(Tree.VerticalScrollBar.Position = -Tree.OffsetY, 'Tree and scrollbar positions agree after wheel');
    Tree.VerticalScrollBar.Position := 500;
    Check(Tree.OffsetY = -Tree.VerticalScrollBar.Position, 'ERD scrollbar scrolls the engine');
    Tree.OffsetY := 0;
    Check(Node.ChildCount = 1000, 'Expanding initializes the requested thousand child nodes');
    Child := Tree.GetFirstChild(Node);
    Check(Child <> nil, 'Expanded group exposes a channel');
    Form.TreeGetText(Tree, Child, 0, ttNormal, TextBefore);
    Tree.SortTree(0, sdDescending, False);
    Form.TreeGetText(Tree, Child, 0, ttNormal, TextAfter);
    Check(TextBefore = TextAfter, 'Sorting preserves node model identity');
    for Mode := tmERDesignsLight to High(TERDThemeMode) do
    begin
      Form.Theme.Mode := Mode;
      Check(Tree.Color = Form.Theme.Palette.GaugeFace, 'Tree background follows theme');
      Check(Tree.Header.Font.Color = Form.Theme.Palette.ForegroundText, 'Header text follows theme');
      Check(Tree.Colors.SelectionTextColor = Form.Theme.Palette.ForegroundText, 'Selection text follows theme');
      Snapshot.SetSize(Tree.Width, Tree.Height);
      Tree.PaintTo(Snapshot.Canvas.Handle, 0, 0);
    end;
    Tree.Selected[Child] := True;
    Check(Tree.Selected[Child], 'Native selection API remains available');
    Rect := Tree.GetDisplayRect(Child, 0, False);
    Check(Rect.Bottom > Rect.Top, 'Expanded child has layout geometry');
    Form.PageTabs.TabIndex := 3;
    Form.Show;
    Application.ProcessMessages;
    Tree.FocusedNode := Child;
    Check(Tree.EditNode(Child, 0), 'Tree starts its ERD text editor');
    Input := CellInput;
    Check(Input is TERDEdit, 'Name cell uses ERD Edit');
    TERDEdit(Input).Text := 'Renamed channel';
    Check(Tree.EndEditNode and (Tree.Text[Child, 0] = 'Renamed channel'),
      'Accepting editor persists text through OnNewText');
    Check(Tree.EditNode(Child, 0), 'Renamed cell can be edited again');
    TERDEdit(CellInput).Text := 'Discard this';
    Tree.CancelEditNode;
    Check(Tree.Text[Child, 0] = 'Renamed channel', 'Escape/cancel preserves model text');
    Check(Tree.EditNode(Child, 2), 'Quality cell starts a combo editor');
    Input := CellInput;
    Check(Input is TERDComboBox, 'Quality uses ERD searchable combo');
    Check(TERDComboBox(Input).Items.Count = 3, 'Column supplies independent combo choices');
    TERDComboBox(Input).Text := '4K';
    Check(Tree.EndEditNode and (Tree.Text[Child, 2] = '4K'), 'Combo writes accepted value');
    Application.ProcessMessages;
    Tree.Clear;
    Check(Tree.RootNodeCount = 0, 'Clear releases virtual nodes');
  finally
    Snapshot.Free;
    Form.Free;
  end;
end;

procedure TestCommonControls;
var
  Form: TForm;
  Search: TSearchProbe;
  Tags, TagsCopy: TERDTagEdit;
  Split: TERDSplitButton;
  Expander, ExpanderCopy: TERDExpander;
  Content, IntentionallyHidden: TERDButton;
  Track: TTrackProbe;
  Range: TRangeProbe;
  Counter: TChangeProbe;
  Stream: TMemoryStream;
  Bitmap: TBitmap;
  OriginalHeight: Integer;
begin
  RegisterClasses([TERDTagEdit, TERDExpander, TERDButton]);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Form.SetBounds(20, 20, 800, 600);
    Search := TSearchProbe.Create(Form);
    Search.Parent := Form;
    Search.OnSearch := Counter.Changed;
    Search.SearchDelay := 0;
    Search.Text := 'live';
    Check(Counter.Changes = 1, 'SearchDelay=0 dispatches a changed query');
    Search.EnterSearch;
    Check(Counter.Changes = 2, 'Enter executes search immediately');
    Search.SearchDelay := 40;
    Search.Text := 'live t';
    Search.Text := 'live tv';
    Check(Counter.Changes = 2, 'Debounce does not dispatch intermediate queries');
    Sleep(80);
    Application.ProcessMessages;
    Check(Counter.Changes = 3, 'Debounce dispatches one final query');

    Tags := TERDTagEdit.Create(Form);
    Tags.Parent := Form;
    Tags.Top := 50;
    Check(Tags.AddTag(' HD '), 'Tag accepts nonempty trimmed text');
    Check(Tags.Tags[0] = 'HD', 'Stored tag is trimmed');
    Check(not Tags.AddTag('hd'), 'Duplicate tags are case-insensitively rejected');
    Tags.Editor.Text := 'Live TV';
    Tags.CommitText;
    Check((Tags.Tags.Count = 2) and (Tags.Editor.Text = ''), 'Commit adds tag and clears input');
    Tags.ReadOnly := True;
    Tags.RemoveTag(0);
    Check((Tags.Tags.Count = 2) and not Tags.AddTag('Blocked'), 'ReadOnly blocks user/API mutations');
    Tags.ReadOnly := False;
    Tags.CloseIconSize := 10;
    Tags.CloseSpacing := 6;
    Tags.TagColor := clRed;
    Tags.TagColorStrength := 40;
    Stream.WriteComponent(Tags);
    Stream.Position := 0;
    TagsCopy := TERDTagEdit(Stream.ReadComponent(nil));
    try
      Check(TagsCopy.Tags.Text = Tags.Tags.Text, 'Tags survive component streaming');
      Check((TagsCopy.CloseIconSize = 10) and (TagsCopy.CloseSpacing = 6) and
        (TagsCopy.TagColor = clRed) and (TagsCopy.TagColorStrength = 40), 'Tag appearance properties survive streaming')
    finally TagsCopy.Free end;
    Stream.Clear;

    Split := TERDSplitButton.Create(Form);
    Split.Parent := Form;
    Split.SetBounds(300, 10, 180, 40);
    Split.AutoSize := False;
    Split.OnClick := Counter.Changed;
    Split.OnDropDown := Counter.Changed;
    Counter.Changes := 0;
    Split.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(20, 20));
    Split.Perform(WM_LBUTTONUP, 0, MakeLParam(20, 20));
    Check(Counter.Changes = 1, 'Split primary region triggers one action');
    Split.OnClick := nil;
    Split.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Split.Width - 12, 20));
    Split.Perform(WM_LBUTTONUP, 0, MakeLParam(Split.Width - 12, 20));
    Check(Counter.Changes = 2, 'Split arrow dispatches dropdown without primary action');

    Expander := TERDExpander.Create(Form);
    Expander.Parent := Form;
    Expander.Top := 200;
    Content := TERDButton.Create(Expander);
    Content.Name := 'ContentSample';
    Content.Parent := Expander;
    Content.Top := 60;
    IntentionallyHidden := TERDButton.Create(Expander);
    IntentionallyHidden.Name := 'IntentionallyHidden';
    IntentionallyHidden.Parent := Expander;
    IntentionallyHidden.Visible := False;
    OriginalHeight := Expander.Height;
    Expander.Expanded := False;
    Check(not Content.Visible and (Expander.Height < OriginalHeight), 'Collapse hides content and reduces height');
    Expander.Expanded := True;
    Check(Content.Visible and not IntentionallyHidden.Visible, 'Expand preserves intentional child visibility');
    Check(Expander.Height = OriginalHeight, 'Expanded height is restored');
    Expander.Expanded := False;
    Stream.WriteComponent(Expander);
    Stream.Position := 0;
    ExpanderCopy := TERDExpander(Stream.ReadComponent(nil));
    try
      Check(not ExpanderCopy.Expanded, 'Collapsed state survives streaming');
      ExpanderCopy.Expanded := True;
      Check(ExpanderCopy.Height = OriginalHeight, 'Collapsed streaming preserves expanded height');
      Check(ExpanderCopy.FindComponent('ContentSample') <> nil, 'Expander streams editable children');
      Check(TControl(ExpanderCopy.FindComponent('ContentSample')).Visible,
        'Expanded copy restores child visibility after collapsed streaming');
      Check(not TControl(ExpanderCopy.FindComponent('IntentionallyHidden')).Visible,
        'Collapsed streaming preserves intentionally hidden children');
    finally ExpanderCopy.Free end;

    Track := TTrackProbe.Create(Form);
    Track.Parent := Form;
    Track.AutoSize := False;
    Track.Orientation := toVertical;
    Track.SetBounds(500, 10, 40, 220);
    Track.ClickAt(20, 10);
    Check(Track.Position > 90, 'Vertical track increases towards top');
    Track.ClickAt(20, 210);
    Check(Track.Position < 10, 'Vertical track decreases towards bottom');
    Track.RenderTo(Bitmap);
    Check((Bitmap.Width = Track.Width) and (Bitmap.Height = Track.Height), 'Vertical track renders at control dimensions');

    Range := TRangeProbe.Create(Form);
    Range.Parent := Form;
    Range.Position := 20;
    Range.UpperValue := 80;
    Range.ActiveThumb := rtUpper;
    Range.Step(VK_RIGHT);
    Check((Range.Position = 20) and (Range.UpperValue = 81), 'Upper-thumb keys preserve lower value');
    Range.Position := 99;
    Check(Range.Position = 81, 'Lower thumb cannot cross upper thumb');
    Range.UpperValue := 0;
    Check(Range.UpperValue = 81, 'Upper thumb cannot cross lower thumb');
    Range.Max := 50;
    Check((Range.Position = 50) and (Range.UpperValue = 50), 'Range bounds clamp both handles');
    Range.RenderTo(Bitmap);
  finally
    Bitmap.Free;
    Stream.Free;
    Form.Free;
    Counter.Free;
  end;
end;

procedure TestFloatingPainter;
var Bitmap: TBitmap; Painter: TERDPainter; Palette: TERDThemePalette; C: Cardinal;
    X, Y, Ink, Intermediate: Integer; Width, Narrow, Wide: Single;
  procedure Clear;
  begin Bitmap.Canvas.Brush.Color := clWhite; Bitmap.Canvas.FillRect(Rect(0,0,160,100)); end;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit; Bitmap.SetSize(160,100); Palette := BRAND_PALETTE_LIGHT;
    Painter := TERDPainter.Create(Bitmap.Canvas,Palette,96);
    try
      Clear; Painter.RectangleF(10.25,10.25,40,40,clRed,clBlue,2,64);
      C := ColorToRGB(Bitmap.Canvas.Pixels[30,30]);
      Check((GetRValue(C) >= 252) and (Abs(Integer(GetGValue(C))-191) <= 3) and (Abs(Integer(GetBValue(C))-191) <= 3), 'Shared GDI+ rectangle blends translucent fill over the destination');
      C := ColorToRGB(Bitmap.Canvas.Pixels[11,30]);
      Check((GetBValue(C) > 240) and (GetRValue(C) < 20), 'Transparent shape fill retains an opaque selection border');
      Clear; Painter.ClipRectF(20,20,20,20); Painter.RectangleF(0,0,100,100,clRed,clNone);
      Check((ColorToRGB(Bitmap.Canvas.Pixels[25,25]) = ColorToRGB(clRed)) and
        (ColorToRGB(Bitmap.Canvas.Pixels[10,10]) = ColorToRGB(clWhite)), 'Shared clip restricts subsequent GDI+ primitives');
      Painter.ResetClip; Painter.RectangleF(2,2,6,6,clBlue,clNone);
      Check(ColorToRGB(Bitmap.Canvas.Pixels[4,4]) = ColorToRGB(clBlue), 'Resetting painter clip restores drawing outside it');
      Clear; Painter.LineF(5.25,5.75,90.5,65.25,clBlack,1); Intermediate := 0;
      for Y := 2 to 70 do for X := 2 to 100 do
      begin C := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]); if (GetRValue(C) > 0) and (GetRValue(C) < 255) then Inc(Intermediate); end;
      Check(Intermediate > 10, 'Shared diagonal strokes produce anti-aliased edge pixels');
      Clear; Width := Painter.TextF(10.5,30.25,'Floating PCB text',14,clBlack,twRegular,taLeftJustify,120.5);
      Check((Width > 0) and (Width <= 120.5), 'Floating text honors device-pixel maximum width');
      Ink := 0; for Y := 5 to 55 do for X := 5 to 145 do if ColorToRGB(Bitmap.Canvas.Pixels[X,Y]) <> ColorToRGB(clWhite) then Inc(Ink);
      Check(Ink > 20, 'GDI+ floating text produces visible glyphs');
      Narrow := Painter.TextF(0,80,'iiii',12,clBlack,twMono); Wide := Painter.TextF(0,80,'MMMM',12,clBlack,twMono);
      Check(Abs(Narrow-Wide) < 1, 'Floating text retains monospaced font weights');
      Clear; Painter.ClipRectF(20,20,80,40); Painter.TextBlockF(0,0,150,90,'Line one'+sLineBreak+'Line two',12,clBlack);
      Ink := 0; Intermediate := 0;
      for Y := 0 to Bitmap.Height-1 do for X := 0 to Bitmap.Width-1 do
        if ColorToRGB(Bitmap.Canvas.Pixels[X,Y]) <> ColorToRGB(clWhite) then
          if (X >= 20) and (X < 100) and (Y >= 20) and (Y < 60) then Inc(Ink) else Inc(Intermediate);
      Check((Ink > 5) and (Intermediate = 0), 'Wrapped GDI+ text is visible inside the clip and cannot spill outside');
      Painter.ResetClip;
    finally Painter.Free; end;
  finally Bitmap.Free; end;
end;

procedure TestTextPainting;
var Form: TForm; Theme: TERDTheme; Edit: TERDEdit; Memo: TERDMemo;
  Other: TButton; Native: TWinControl; I, J, BeforeInk, AfterInk: Integer;
  Ch: Char; Back: TColor;
  procedure AssignValue(const S: string);
  begin
    if J = 0 then Edit.Text := S else Memo.Lines.Text := S;
  end;
  function CurrentValue: string;
  begin if J = 0 then Result := Edit.Text else Result := TrimRight(Memo.Lines.Text); end;
  function PixelCount(Window: TWinControl; Color: TColor; Match, Repaint: Boolean): Integer;
  var DC: HDC; X, Y: Integer; RGB: COLORREF;
  begin
    Result := 0;
    if Repaint then RedrawWindow(Window.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
    DC := GetDC(Window.Handle);
    try
      RGB := ColorToRGB(Color);
      for Y := 0 to Window.ClientHeight - 1 do
        for X := 0 to Window.ClientWidth - 1 do
          if (GetPixel(DC, X, Y) = RGB) = Match then Inc(Result);
    finally ReleaseDC(Window.Handle, DC) end;
  end;
begin
  Form := TForm.CreateNew(nil);
  try
    Form.SetBounds(100, 100, 520, 320);
    Theme := TERDTheme.Create(Form);
    Edit := TERDEdit.Create(Form); Edit.Parent := Form; Edit.Theme := Theme;
    Edit.SetBounds(12, 12, 400, 28);
    Memo := TERDMemo.Create(Form); Memo.Parent := Form; Memo.Theme := Theme;
    Memo.SetBounds(12, 56, 400, 100);
    Other := TButton.Create(Form); Other.Parent := Form; Other.Top := 180;
    Form.Show; Other.SetFocus; Application.ProcessMessages;
    for J := 0 to 1 do
    begin
      Native := nil;
      if J = 0 then
      begin
        for I := 0 to Edit.ControlCount - 1 do
          if Edit.Controls[I] is TERDTextInput then Native := TWinControl(Edit.Controls[I]);
      end
      else
        for I := 0 to Memo.ControlCount - 1 do
          if Memo.Controls[I] is TERDTextArea then Native := TWinControl(Memo.Controls[I]);
      Check(Native <> nil, 'Edit/Memo have a text window');
      Back := TERDTextInput(Native).Color;
      AssignValue('');
      BeforeInk := PixelCount(Native, Back, False, True);
      AssignValue('Visible before focus');
      AfterInk := PixelCount(Native, Back, False, True);
      Check(AfterInk > BeforeInk + 10, 'Unfocused native text is painted after invalidation');
      Native.SetFocus;
      AssignValue('');
      BeforeInk := PixelCount(Native, Back, False, True);
      for Ch in 'Typed text' do Native.Perform(WM_CHAR, Ord(Ch), 0);
      AfterInk := PixelCount(Native, Back, False, True);
      Check((CurrentValue = 'Typed text') and (AfterInk > BeforeInk + 10),
        'Typing updates both native text and visible glyphs for Edit and Memo');
      Native.Perform(EM_SETSEL, 0, 7);
      Native.Repaint;
      Check(PixelCount(Native, Theme.Palette.Accent, True, False) > 10,
        'Selection uses ERD accent in text engine paint');
      Other.SetFocus;
    end;
  finally Form.Free end;
end;

procedure TestDocumentTabs;
var Form: TForm; Tabs: TTabProbe; Host: TERDPanel;
  Pages: array[0..2] of TERDPanel; Items: array[0..2] of TERDTabItem;
  Counter: TTabCounter; Edit: TERDEdit; R, Target: TRect; I, SelectedID, Changes: Integer;
  Window: TCustomForm; Stored, Copy: TERDTabs; Stream: TMemoryStream;
begin
  RegisterClass(TERDTabs);
  Form := TForm.CreateNew(nil);
  Counter := TTabCounter.Create;
  Stream := TMemoryStream.Create;
  try
    Form.ClientWidth := 800; Form.ClientHeight := 420;
    Host := TERDPanel.Create(Form);
    Host.Parent := Form; Host.SetBounds(0, 44, 640, 300);
    Tabs := TTabProbe.Create(Form);
    Tabs.Parent := Form; Tabs.SetBounds(0, 0, 640, 36); Tabs.TabStyle := tsDocument;
    Check(not Tabs.AllowReorder and not Tabs.AllowDetach, 'Tab drag features are opt-in');
    Tabs.AllowReorder := True; Tabs.AllowDetach := True;
    for I := 0 to 2 do
    begin
      Pages[I] := TERDPanel.Create(Form); Pages[I].Parent := Host; Pages[I].Align := alClient;
      Items[I] := Tabs.Tabs.Add; Items[I].Caption := 'Document ' + IntToStr(I);
      Items[I].Page := Pages[I];
    end;
    Edit := TERDEdit.Create(Pages[1]); Edit.Parent := Pages[1]; Edit.Text := 'Retain this content';
    Tabs.TabIndex := 1;
    Tabs.OnChange := Counter.Changed; Tabs.OnClose := Counter.Closing;
    Tabs.OnClosed := Counter.Closed; Tabs.OnReordering := Counter.Reordering;
    Tabs.OnReordered := Counter.Reordered; Tabs.OnDetaching := Counter.Detaching;
    Form.Show; Application.ProcessMessages;
    SelectedID := Items[1].ID;
    Check(Pages[1].Visible and not Pages[0].Visible and not Pages[2].Visible, 'Only selected attached Page is visible');
    Check(Tabs.MoveTab(0, 2) and (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and
      Pages[1].Visible and (Counter.Reorders = 1), 'Reordering retains selected item and Page');
    Counter.VetoMove := True;
    Check(not Tabs.MoveTab(0, 1) and (Counter.Reorders = 1), 'Reorder can be vetoed');
    Counter.VetoMove := False;
    R := Tabs.TabBounds(0); Target := Tabs.TabBounds(1);
    Tabs.Perform(WM_LBUTTONDOWN, MK_LBUTTON,
      MakeLParam((R.Left + R.Right) div 2, Tabs.Height div 2));
    Tabs.Perform(WM_MOUSEMOVE, MK_LBUTTON,
      MakeLParam((Target.Left + Target.Right) div 2, Tabs.Height div 2));
    Check(Tabs.DragInProgress, 'Native mouse movement starts tab drag');
    Tabs.Perform(WM_LBUTTONUP, 0,
      MakeLParam((Target.Left + Target.Right) div 2, Tabs.Height div 2));
    Check(Tabs.Tabs[1].ID = SelectedID, 'Native release over next tab reorders it');
    Check(Tabs.MoveTab(1, 0), 'Restore order after native drag regression');
    R := Tabs.TabBounds(0); Target := Tabs.TabBounds(1);
    Tabs.PressAt((R.Left + R.Right) div 2, Tabs.Height div 2);
    Tabs.DragTo(Target.Right - 24, Tabs.Height div 2);
    Check(Tabs.DragInProgress and (Tabs.Tabs[0].ID = SelectedID), 'Drag shows marker without moving data immediately');
    Tabs.PressKey(VK_ESCAPE, []);
    Tabs.ReleaseAt(Target.Right - 24, Tabs.Height div 2);
    Check((Tabs.Tabs[0].ID = SelectedID) and not Tabs.DragInProgress, 'Escape cancels pending drag');
    Counter.VetoDetach := True;
    Check(not Tabs.DetachTab(Items[1].Index, Point(200, 200)) and (Pages[1].Parent = Host), 'Detach can be vetoed');
    Counter.VetoDetach := False;
    Check(Tabs.DetachTab(Items[1].Index, Point(200, 200)), 'Page-backed tab detaches');
    Window := Tabs.DetachedWindow(Items[1].Index);
    Check((Window <> nil) and Items[1].Detached and (Pages[1].Parent = Window) and
      (Edit.Owner = Pages[1]) and (Edit.Text = 'Retain this content'), 'Detached host uses same Page and content objects');
    Window.Close;
    Application.ProcessMessages;
    Check(not Items[1].Detached and (Pages[1].Parent = Host) and (Pages[1].Align = alClient) and
      (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and (Edit.Text = 'Retain this content'), 'Closing detached window reattaches and preserves content');
    R := Tabs.CloseButtonBounds(Items[0].Index);
    Tabs.PressAt((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
    Check(Tabs.Tabs.Count = 3, 'Close waits for release');
    Tabs.ReleaseAt(Tabs.Width + 5, Tabs.Height div 2);
    Check(Tabs.Tabs.Count = 3, 'Release away cancels close');
    Counter.VetoClose := True;
    Tabs.CloseTab(Items[0].Index);
    Check((Tabs.Tabs.Count = 3) and (Counter.ClosedCount = 0), 'Close veto preserves tab');
    Counter.VetoClose := False;
    R := Tabs.CloseButtonBounds(Items[0].Index);
    Tabs.Perform(WM_LBUTTONDOWN, MK_LBUTTON,
      MakeLParam((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2));
    Check(Tabs.Tabs.Count = 3, 'Native close press waits for release');
    Tabs.Perform(WM_LBUTTONUP, 0,
      MakeLParam((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2));
    Check((Tabs.Tabs.Count = 2) and (Tabs.Tabs[Tabs.TabIndex].ID = SelectedID) and not Pages[0].Visible,
      'Closing another tab keeps selected document');
    Changes := Counter.Changes;
    Tabs.CloseTab(Items[1].Index);
    Check((Tabs.Tabs.Count = 1) and (Counter.Changes = Changes + 1) and Pages[2].Visible,
      'Closing selected tab notifies when replacement has same index');
    // Destroying a tabstrip must not destroy a Page owned by the host form.
    Check(Tabs.DetachTab(0, Point(220, 220)), 'Last tab can detach');
    Tabs.OnChange := nil; Tabs.OnClosed := nil;
    Tabs.Free;
    Application.ProcessMessages;
    Check((Pages[2].Parent = Host) and (Pages[2].Owner = Form), 'Destroying source restores detached content before freeing host');
    Stored := TERDTabs.Create(nil);
    try
      Stored.TabStyle := tsDocument; Stored.AllowReorder := True; Stored.AllowDetach := True;
      Stored.Tabs.Add.Caption := 'Temporary'; Stored.TabIndex := 0;
      Stored.CloseTab(0);
      Check((Stored.Tabs.Count = 0) and (Stored.TabIndex = -1), 'Closing last tab leaves a valid empty selection');
      Stored.Tabs.Add.Caption := 'Stored'; Stored.TabIndex := 0;
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDTabs(Stream.ReadComponent(nil));
    try Check(Copy.AllowReorder and Copy.AllowDetach and (Copy.TabIndex = 0) and
      (Copy.Tabs.Count = 1) and not Copy.Tabs[0].Detached, 'Tab DFM persists config and starts attached')
    finally Copy.Free end;
  finally Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestEditPasswordAndValidation;
var Form: TForm; Edit: TEditProbe; Native: TERDTextInput; Validator: TValidationProbe;
  Copy, Stored: TERDEdit; Counter: TChangeProbe; Stream: TMemoryStream;
  Bitmap: TBitmap; BaseHeight, Calls: Integer;
begin
  RegisterClass(TERDEdit);
  Form := TForm.CreateNew(nil);
  Validator := TValidationProbe.Create;
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Edit := TEditProbe.Create(Form);
    Edit.Parent := Form;
    Edit.Text := 'Example';
    Edit.PasswordChar := #8226;
    Edit.ShowPasswordButton := True;
    Edit.OnChange := Counter.Changed;
    Form.Show; Application.ProcessMessages; Edit.SetFocus;
    Edit.SelStart := 1; Edit.SelLength := 3;
    Native := Edit.Editor;
    Check(Native.PasswordChar = #8226, 'Configured password character is applied to native editor');
    Edit.RevealByKeyboard;
    Check(Edit.PasswordVisible and Native.PasswordVisible and
      (Edit.SelStart = 1) and (Edit.SelLength = 3) and (Edit.Text = 'Example') and
      (Counter.Changes = 0), 'Password reveal preserves text and selection without OnChange');
    Edit.RevealByKeyboard;
    Check(not Edit.PasswordVisible and (Native.PasswordChar = #8226), 'Keyboard hides password again');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(not Edit.PasswordVisible, 'Eye waits for matching release');
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(Edit.PasswordVisible and Native.PasswordVisible, 'Native mouse release reveals password');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(4, Edit.Height div 2));
    Check(Edit.PasswordVisible, 'Release away cancels eye toggle');
    Edit.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Edit.Perform(WM_LBUTTONUP, 0, MakeLParam(Edit.Width - 16, Edit.Height div 2));
    Check(not Edit.PasswordVisible and (Native.PasswordChar = #8226), 'Eye hides password on next click');

    Check(Native.Left + Native.Width <= Edit.Width - 32, 'Native edit reserves room for reveal button');
    Edit.PasswordChar := #0;
    Edit.OnValidate := Validator.ValidateText;
    Edit.ValidateOnChange := True;
    BaseHeight := Edit.Height;
    Edit.Text := '';
    Check((Edit.ValidationState = evsError) and (Edit.ValidationText = 'Required') and
      (Edit.Height > BaseHeight), 'Validation error grows message below field');
    Calls := Validator.Calls;
    Check(not Edit.Validate and (Validator.Calls = Calls + 1), 'Validation reports failure and guards recursion');
    Edit.Text := 'abc';
    Check((Edit.ValidationState = evsWarning) and Edit.Validate, 'Warning is displayed but not invalid');
    Edit.Text := 'valid';
    Check((Edit.ValidationState = evsSuccess) and (Edit.ValidationText = '') and
      (Edit.Height = BaseHeight), 'Successful validation clears message and restores field height');
    Stored := TERDEdit.Create(nil);
    try
      Stored.PasswordChar := #8226; Stored.ShowPasswordButton := True;
      Stored.PasswordVisible := True; Stored.Text := 'Example';
      Stored.ValidationState := evsError; Stored.ValidationText := 'Example message';
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Password/validation preview works without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDEdit(Stream.ReadComponent(nil));
    try
      Check((Copy.PasswordChar = #8226) and Copy.ShowPasswordButton and not Copy.PasswordVisible and
        (Copy.ValidationState = evsError) and (Copy.ValidationText = 'Example message') and
        (Copy.Text = 'Example') and (Copy.ControlCount = 1), 'Edit DFM preserves config and masks visibility on reload');
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Form.Free; Validator.Free; Counter.Free end;
end;

procedure TestEditableCombo;
var Form: TForm; Combo, Copy: TERDComboBox; Counter: TChangeProbe;
  Popup: TERDPopupList; I: Integer; Stream: TMemoryStream;
begin
  RegisterClass(TERDComboBox);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  try
    Combo := TERDComboBox.Create(Form);
    Combo.Parent := Form;
    Combo.Items.Add('Belgium'); Combo.Items.Add('Netherlands');
    Combo.Items.Add('Belgium HD'); Combo.Items.Add('France');
    Check((Combo.Style = ecsDropDownList) and Combo.AutoComplete, 'Legacy combo defaults preserved');
    Combo.Style := ecsDropDown;
    Combo.FilterMode := cfsContains;
    Combo.FilterText := 'BEL';
    Check((Combo.FilteredCount = 2) and (Combo.FilteredIndex(0) = 0) and
      (Combo.FilteredIndex(1) = 2), 'Filtering retains original item indices');
    Combo.FilterText := 'land';
    Check((Combo.FilteredCount = 1) and (Combo.FilteredIndex(0) = 1), 'Contains filter can match middle of text');
    Combo.FilterMode := cfsPrefix;
    Check(Combo.FilteredCount = 0, 'Prefix filter does not match middle');
    Combo.FilterText := 'bel';
    Combo.OnChange := Counter.Changed;
    Form.Show; Application.ProcessMessages;
    Combo.SetFocus;
    Combo.DropDown;
    Popup := nil;
    for I := 0 to Combo.ComponentCount - 1 do if Combo.Components[I] is TERDPopupList then
      Popup := TERDPopupList(Combo.Components[I]);
    Check((Popup <> nil) and Popup.IsOpen and (Popup.Items.Count = 2), 'Editable combo opens filtered popup');
    Popup.ItemIndex := 1;
    Popup.CloseUp(True);
    Check((Combo.ItemIndex = 2) and (Combo.Text = 'Belgium HD') and (Counter.Changes = 1),
      'Filtered popup commits original index exactly once');
    Combo.FilterMode := cfsNone;
    Combo.Text := '';
    Combo.Editor.Perform(WM_CHAR, Ord('B'), 0);
    Check((Combo.Text = 'Belgium') and (Combo.Editor.SelLength = 6), 'Native typing completes and selects suffix');
    Combo.Editor.Perform(WM_CHAR, Ord('e'), 0);
    Check((Combo.Text = 'Belgium') and (Combo.Editor.SelLength = 5), 'Next character replaces completion suffix');
    Combo.Editor.Perform(WM_KEYDOWN, VK_BACK, 0);
    Combo.Editor.Perform(WM_CHAR, VK_BACK, 0);
    Check(Combo.Text = 'Be', 'Backspace removes suffix without immediately restoring it');
    Combo.Editor.SelStart := 1; Combo.Editor.SelLength := 1;
    Combo.Editor.Perform(WM_CLEAR, 0, 0);
    Check(Combo.Text = 'B', 'Native delete command is not undone by completion');
    Combo.Editor.Perform(WM_UNDO, 0, 0);
    Check(Combo.Text = 'Be', 'Native undo restores edit without completion');
    Combo.AutoComplete := False;
    Combo.Text := 'custom text';
    Check(Combo.ItemIndex = -1, 'Editable combo permits free text outside Items');
    Combo.AutoComplete := True; Combo.Text := ''; Combo.SetFocus;
    Combo.Editor.Perform(WM_CHAR, Ord('f'), 0);
    Combo.Editor.Perform(WM_CHAR, Ord('R'), 0);
    Check((Combo.Text = 'fRance') and (Combo.ItemIndex = 3), 'Autocomplete preserves casing until explicit selection');
    Combo.DropDown;
    Popup.ItemIndex := 3; Counter.Changes := 0; Popup.CloseUp(True);
    Check((Combo.Text = 'France') and (Combo.ItemIndex = 3) and (Counter.Changes = 1),
      'Selecting an autocompleted item commits exact casing even when index is unchanged');
    Combo.Text := 'france'; Combo.DropDown; Popup.ItemIndex := 3; Popup.CloseUp(True);
    Check(Combo.Text = 'France', 'Selecting an exact case-insensitive match restores item casing');
    Combo.AutoComplete := False; Combo.Text := 'custom text';
    Combo.OnChange := nil;
    Stream.WriteComponent(Combo);
    Stream.Position := 0;
    Copy := TERDComboBox(Stream.ReadComponent(nil));
    try
      Check((Copy.Style = ecsDropDown) and (Copy.Text = 'custom text') and (Copy.Items.Count = 4), 'Combo DFM preserves style and free text');
      Check(Copy.ControlCount = 1, 'Combo stream does not duplicate native editor');
    finally Copy.Free end;
  finally Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestRating;
var Form: TForm; Rating: TRatingProbe; Copy, Stored: TERDRating;
  Counter: TChangeProbe; Bitmap: TBitmap; Stream: TMemoryStream; I: Integer;
begin
  RegisterClass(TERDRating);
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Rating := TRatingProbe.Create(Form);
    Rating.Parent := Form;
    Rating.OnChange := Counter.Changed;
    Check((Rating.MaxStars = 5) and not Rating.AllowHalf and Rating.AllowClear and (Rating.Value = 0), 'Rating defaults');
    Rating.Value := 3.7;
    Check(Rating.Value = 4, 'Whole rating rounds to nearest integer');
    Rating.AllowHalf := True;
    Rating.Value := 3.7;
    Check(Rating.Value = 3.5, 'Half rating rounds to nearest half');
    Rating.Value := 20;
    Check(Rating.Value = 5, 'Rating clamps maximum');
    Rating.Value := -2;
    Check(Rating.Value = 0, 'Rating clamps minimum');
    Rating.PressKey(VK_RIGHT);
    Check(Rating.Value = 0.5, 'Keyboard advances a half step');
    Rating.PressKey(VK_END);
    Check(Rating.Value = 5, 'End chooses maximum rating');
    Rating.PressKey(VK_HOME);
    Check(Rating.Value = 0, 'Home clears when allowed');
    Rating.AllowClear := False;
    Rating.PressKey(VK_HOME);
    Check(Rating.Value = 0.5, 'Home respects minimum nonzero value when clearing disabled');
    Rating.Value := 0;
    I := Counter.Changes;
    Rating.PressKey(VK_DELETE);
    Check((Rating.Value = 0) and (Counter.Changes = I), 'Delete has no effect when clearing disabled');
    Rating.AllowClear := True;
    Form.Show; Application.ProcessMessages;
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Check(Rating.Value = 0.5, 'Left half of first star selects half value');
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Check(Rating.Value = 0, 'Clicking selected value clears rating');
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(Rating.Width + 2, Rating.Height div 2);
    Check(Rating.Value = 0, 'Releasing outside cancels rating selection');
    Rating.PreviewAt(32, Rating.Height div 2);
    Check((Rating.PreviewValue > 0) and (Rating.Value = 0), 'Hover previews without changing stored rating');
    Rating.ReadOnly := True;
    I := Counter.Changes;
    Rating.PressAt(8, Rating.Height div 2);
    Rating.ReleaseAt(8, Rating.Height div 2);
    Rating.PressKey(VK_END);
    Check(Counter.Changes = I, 'Readonly blocks mouse and keyboard changes');
    Rating.Value := 4.5;
    Rating.MaxStars := 3;
    Check(Rating.Value = 3, 'Changing star count clamps existing value');
    Stored := TERDRating.Create(nil);
    try
      Stored.AllowHalf := True; Stored.Value := 4.5; Stored.ReadOnly := True;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Filled and half-filled stars render without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDRating(Stream.ReadComponent(nil));
    try Check(Copy.AllowHalf and Copy.ReadOnly and (Copy.Value = 4.5), 'Rating DFM roundtrip')
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Form.Free; Counter.Free end;
end;

procedure TPalettePopupProbe.InspectPopup(Sender: TObject);
var Window: TCustomForm; I: Integer; Search: TERDSearchEdit;
begin
  Window := Screen.ActiveCustomForm;
  Inc(Attempts);
  if (Window = nil) or (Window = OwnerForm) then
  begin
    if Attempts >= 40 then begin Timer.Enabled := False; PostQuitMessage(1) end;
    Exit;
  end;
  Timer.Enabled := False;
  SawPopup := TForm(Window).BorderStyle = bsNone;
  OwnerEnabled := IsWindowEnabled(OwnerForm.Handle);
  TopAnchored := Window.Top < OwnerForm.Top + OwnerForm.Height div 3;
  NoButtons := True;
  Search := nil;
  for I := 0 to Window.ControlCount - 1 do
  begin
    if Window.Controls[I] is TERDButton then NoButtons := False;
    if Window.Controls[I] is TERDSearchEdit then Search := TERDSearchEdit(Window.Controls[I]);
  end;
  if Search <> nil then
  begin
    Search.Text := 'playlist';
    SendMessage(GetFocus, WM_KEYDOWN, VK_RETURN, 0);
    if TForm(Window).ModalResult = mrNone then TForm(Window).ModalResult := mrCancel;
  end
  else TForm(Window).ModalResult := mrCancel;
end;

procedure TestCommandPalettePopup;
var Form: TForm; Palette: TERDCommandPalette; Probe: TPalettePopupProbe;
  Counter: TCommandCounter;
begin
  Form := TForm.CreateNew(nil);
  Probe := TPalettePopupProbe.Create;
  Counter := TCommandCounter.Create;
  try
    Form.SetBounds(100, 100, 800, 600);
    Palette := TERDCommandPalette.Create(Form);
    Palette.OnExecute := Counter.Executed;
    Palette.Commands.Add.Caption := 'Open playlist';
    Palette.Commands[0].Key := 'open';
    Probe.OwnerForm := Form;
    Probe.Timer := TTimer.Create(Form);
    Probe.Timer.Enabled := False;
    Probe.Timer.Interval := 50;
    Probe.Timer.OnTimer := Probe.InspectPopup;
    Form.Show;
    Probe.Timer.Enabled := True;
    Check(Palette.Execute and (Counter.Count = 1), 'Palette search Enter executes after popup closes');
    Check(Probe.SawPopup and Probe.OwnerEnabled and Probe.NoButtons and Probe.TopAnchored,
      'Command palette is a top-anchored popup without dialog buttons or disabled owner');
  finally Form.Free; Probe.Free; Counter.Free end;
end;

procedure TestCommandPalette;
var Palette, Copy: TERDCommandPalette; First, Other: TERDCommandItem;
  Counter: TCommandCounter; ActionCounter: TChangeProbe; Action: TAction;
  IDs: TArray<Integer>; SavedID: Integer; Stream: TMemoryStream;
begin
  RegisterClass(TERDCommandPalette);
  Palette := TERDCommandPalette.Create(nil);
  Counter := TCommandCounter.Create;
  ActionCounter := TChangeProbe.Create;
  Action := TAction.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Palette.OnExecute := Counter.Executed;
    First := Palette.Commands.Add;
    First.Key := 'open'; First.Caption := 'Open playlist'; First.Category := 'Files';
    First.Keywords := 'import m3u'; First.ShortcutText := 'Ctrl+O';
    Other := Palette.Commands.Add;
    Other.Key := 'tv'; Other.Caption := 'Play channel'; Other.Category := 'Live TV'; Other.Keywords := 'Belgium stream';
    IDs := Palette.Search('M3U files');
    Check((Length(IDs) = 1) and (IDs[0] = First.ID), 'Command search matches all case-insensitive words across fields');
    IDs := Palette.Search('Belgium live');
    Check((Length(IDs) = 1) and (IDs[0] = Other.ID), 'Command search covers category and keywords');
    Check(Length(Palette.Search('missing')) = 0, 'Unknown search gives no results');
    Other.Enabled := False;
    Check(Length(Palette.Search('')) = 1, 'Disabled commands omitted from palette');
    Check(not Palette.ExecuteCommand(Other.ID), 'Disabled commands cannot execute');
    First.Visible := False;
    Check(Length(Palette.Search('')) = 0, 'Hidden commands omitted');
    First.Visible := True;
    Check(Palette.ExecuteCommand(First.ID) and (Counter.Count = 1) and (Counter.LastKey = 'open') and
      (Palette.LastCommand = 'open'), 'Command without Action invokes OnExecute');
    Action.Caption := '&Save file'; Action.ShortCut := ShortCut(Ord('S'), [ssCtrl]);
    Action.OnExecute := ActionCounter.Changed;
    Other.Enabled := True; Other.Caption := ''; Other.Action := Action;
    Check((Other.DisplayCaption = 'Save file') and (Other.DisplayShortcut <> ''), 'Action supplies missing caption and shortcut');
    Check(Palette.ExecuteCommand(Other.ID) and (ActionCounter.Changes = 1) and (Counter.Count = 1),
      'Action-backed commands execute Action without duplicate OnExecute');
    Action.Enabled := False;
    Check(not Palette.ExecuteCommand(Other.ID), 'Disabled Action cannot execute');
    Action.Free; Action := nil;
    Check(Other.Action = nil, 'FreeNotification clears action reference');
    SavedID := First.ID;
    First.Index := 1;
    Check(Palette.ExecuteCommand(SavedID), 'Collection ID survives reordering');
    First.Free;
    Check(not Palette.ExecuteCommand(SavedID), 'Deleted command ID cannot execute another item');
    Palette.OnExecute := nil;
    Palette.Commands[0].Caption := 'Stored command';
    Palette.Commands[0].Icon.BuiltIn := ei028Info;
    Stream.WriteComponent(Palette);
    Stream.Position := 0;
    Copy := TERDCommandPalette(Stream.ReadComponent(nil));
    try
      Check((Copy.Commands.Count = 1) and (Copy.Commands[0].Key = 'tv') and
        (Copy.Commands[0].Icon.BuiltIn = ei028Info), 'Command collection and icon survive DFM streaming');
    finally Copy.Free end;
  finally Stream.Free; Action.Free; Palette.Free; Counter.Free; ActionCounter.Free end;
end;

procedure TestDropZone;
var Zone: TERDDropZone; Counter: TDropCounter; Files, Accepted: TStringList;
  Root, First, Second, Other: string; Id: TGUID; Bitmap: TBitmap;
begin
  CreateGUID(Id);
  Root := TPath.Combine(TPath.GetTempPath, 'ERD-drop-' + GUIDToString(Id));
  CreateDir(Root);
  First := TPath.Combine(Root, 'één.m3u');
  Second := TPath.Combine(Root, 'UPPER.M3U8');
  Other := TPath.Combine(Root, 'notes.txt');
  Zone := TERDDropZone.Create(nil);
  Counter := TDropCounter.Create;
  Files := TStringList.Create;
  Accepted := TStringList.Create;
  Bitmap := TBitmap.Create;
  try
    TFile.WriteAllText(First, '#EXTM3U');
    TFile.WriteAllText(Second, '#EXTM3U');
    TFile.WriteAllText(Other, 'notes');
    Zone.FileMasks := '*.m3u;*.m3u8';
    Zone.OnFilesDropped := Counter.Dropped;
    Check(Zone.AcceptsFile(First) and Zone.AcceptsFile(Second), 'Unicode paths and case-insensitive masks are accepted');
    Check(not Zone.AcceptsFile(Other) and not Zone.AcceptsFile(Root) and not Zone.AcceptsFile(Other + '.missing'),
      'Wrong mask, directories and missing files are rejected');
    Files.Add(Other); Files.Add(First); Files.Add(Second);
    Check(Zone.DropFiles(Files) and (Counter.FileCount = 2) and (Counter.FirstFile = First),
      'Drop delivers only matching files in source order');
    Check(Files.Count = 3, 'Source list remains owned and unchanged by caller');
    Zone.AllowMultiple := False;
    Check(not Zone.DropFiles(Files) and (Counter.Count = 1), 'Multiple matching files rejected in single mode');
    Files.Delete(2);
    Check(Zone.DropFiles(Files) and (Counter.FileCount = 1), 'Single mode accepts one remaining filtered file');
    Zone.AcceptDrops := False;
    Check(not Zone.DropFiles(Files), 'AcceptDrops false blocks delivery');
    Zone.AcceptDrops := True; Zone.Enabled := False;
    Check(not Zone.DropFiles(Files), 'Disabled drop zone blocks delivery');
    Zone.Enabled := True; Zone.AllowDirectories := True;
    Files.Clear; Files.Add(Root);
    Check(Zone.FilterFiles(Files, Accepted) and (Accepted[0] = Root), 'Opt-in directory drops are supported');
    Zone.RenderTo(Bitmap);
    Check(not Zone.HandleAllocated, 'DropZone preview does not initialize an OLE window');
  finally
    Bitmap.Free; Zone.Free; Files.Free; Accepted.Free; Counter.Free;
    TDirectory.Delete(Root, True);
  end;
end;

procedure TestLoadingIndicator;
var Form: TForm; Loading: TLoadingProbe; Stored, Copy: TERDLoadingIndicator;
  Bitmap: TBitmap; Stream: TMemoryStream; I, Frame: Integer; Style: TERDLoadingIndicatorStyle;
begin
  RegisterClass(TERDLoadingIndicator);
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Stored := TERDLoadingIndicator.Create(nil);
    try
      Check(Stored.Active and Stored.RespectAnimations and not Stored.IsAnimating,
        'Loading defaults: active, respects OS animation, no timer before showing');
      Stored.Caption := 'Loading';
      Stored.Style := lisBars;
      Stored.IndicatorSize := 48;
      Stored.AnimationInterval := 120;
      Stored.Active := False;
      Stored.RespectAnimations := False;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'Loading indicator renders without a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDLoadingIndicator(Stream.ReadComponent(nil));
    try
      Check(not Copy.Active and not Copy.RespectAnimations and (Copy.Caption = 'Loading') and
        (Copy.Style = lisBars) and (Copy.IndicatorSize = 48) and (Copy.AnimationInterval = 120),
        'Loading indicator DFM stores active, animation option and caption');
    finally Copy.Free end;
    Loading := TLoadingProbe.Create(Form);
    Loading.Parent := Form;
    Loading.RespectAnimations := False;
    Loading.Caption := '';
    for Style := Low(TERDLoadingIndicatorStyle) to High(TERDLoadingIndicatorStyle) do
    begin
      Loading.Style := Style;
      for I := 1 to 4 do
      begin
        Loading.IndicatorSize := I * 16;
        Loading.RenderTo(Bitmap);
        Check((Loading.Width = Loading.Height) and (Bitmap.Width = Loading.Width),
          'Every loader style supports autosized 16/32/48/64 pixel boxes');
      end;
    end;
    Loading.Style := lisSpinner;
    Loading.IndicatorSize := 32;
    for I := 1 to 12 do Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Spinner wraps after twelve frames');
    Loading.Active := False;
    Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Inactive spinner does not advance');
    Loading.Active := True;
    Loading.Enabled := False;
    Loading.NextFrame;
    Check(Loading.FrameIndex = 0, 'Disabled spinner does not advance');
    Loading.Enabled := True;
    Form.Show;
    Application.ProcessMessages;
    Check(Loading.IsAnimating, 'Visible active spinner starts its timer');
    Loading.Visible := False;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating, 'Hiding spinner stops timer');
    Loading.Visible := True;
    Application.ProcessMessages;
    Check(Loading.IsAnimating, 'Showing spinner resumes timer');
    Frame := Loading.FrameIndex;
    Loading.Active := False;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating and (Loading.FrameIndex = Frame), 'Stopping preserves frame and disables timer');
    Loading.Active := True;
    Form.Hide;
    Application.ProcessMessages;
    Check(not Loading.IsAnimating, 'Hiding parent form stops spinner timer');
  finally Bitmap.Free; Stream.Free; Form.Free end;
end;

procedure TestLinkLabel;
var Link: TLinkProbe; Counter: TChangeProbe; Bitmap: TBitmap;
begin
  Link := TLinkProbe.Create(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  try
    Link.OnClick := Counter.Changed;
    Check(not Link.AutoOpen and not Link.Visited, 'Link does not auto-open by default');
    Link.URL := 'https://erdesigns.be';
    Link.Activate;
    Check(Link.Visited and (Counter.Changes = 1), 'Link click marks visited and notifies');
    Link.URL := 'https://erdesigns.be/products';
    Check(not Link.Visited, 'New URL resets visited state');
    Link.PressEnter;
    Check(Link.Visited and (Counter.Changes = 2), 'Enter activates link');
    Link.Enabled := False;
    Link.Activate;
    Link.PressEnter;
    Check((Counter.Changes = 2) and not Link.OpenLink, 'Disabled link cannot activate or launch URL');
    Link.Enabled := True;
    Link.URL := '';
    Check(not Link.OpenLink, 'Empty URL is not launched');
    Link.Icon.BuiltIn := ei028Info;
    Link.RenderTo(Bitmap);
    Check(not Link.HandleAllocated and (Bitmap.Width = Link.Width), 'Link with icon renders without a window');
  finally Bitmap.Free; Link.Free; Counter.Free end;
end;

procedure TestDateSegmentsAndDateTime;
var Picker: TDateProbe; Combined, Copy: TERDDateTimePicker; Counter: TChangeProbe;
  Stream: TMemoryStream; Bitmap: TBitmap; I: Integer; R: TRect;
begin
  RegisterClass(TERDDateTimePicker);
  Picker := TDateProbe.Create(nil);
  Combined := TERDDateTimePicker.Create(nil);
  Counter := TChangeProbe.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    Picker.DisplayFormat := 'dd/mm/yyyy';
    Picker.Date := EncodeDate(2024, 1, 31);
    Picker.ActiveSegment := dsMonth;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2024, 2, 29), 'Changing month clamps to leap-day');
    Picker.ActiveSegment := dsYear;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2025, 2, 28), 'Changing leap year clamps to valid day');
    Picker.ActiveSegment := dsDay;
    Picker.Step(1);
    Check(Picker.Date = EncodeDate(2025, 2, 1), 'Day wraps within selected month');
    Picker.TypeText('27');
    Picker.TypeText('10');
    Picker.TypeText('2026');
    Check(Picker.Date = EncodeDate(2026, 10, 27), 'Date digits follow day/month/year segments');
    Picker.DisplayFormat := 'yyyy-mm-dd';
    Picker.ActiveSegment := dsYear;
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsMonth, 'Segment navigation follows custom format order');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsDay, 'ISO date navigation reaches day after month');
    Picker.DisplayFormat := 'dddd, dd mmmm yyyy';
    R := Picker.SegmentRect(dsDay);
    Check(R.Left > 10, 'Weekday text stays outside editable day segment');
    Picker.DisplayFormat := 'mmmm yyyy';
    Picker.ActiveSegment := dsDay;
    Check(Picker.ActiveSegment = dsMonth, 'Missing date field cannot remain active');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = dsYear, 'Month/year format skips missing day');
    Picker.ReadOnly := True;
    Picker.Step(1);
    Picker.TypeText('2030');
    Check(Picker.Date = EncodeDate(2026, 10, 27), 'Readonly date blocks keyboard and stepping');
    Picker.RenderTo(Bitmap);
    Check(not Picker.HandleAllocated, 'Segmented DatePicker renders without a window');
    Combined.DateFormat := 'dd/mm/yyyy';
    Combined.ShowSeconds := True;
    Combined.TimeFormat := tf12Hour;
    Combined.OnChange := Counter.Changed;
    Combined.DateTime := EncodeDate(2024, 2, 29) + EncodeTime(13, 14, 15, 321);
    Check(Counter.Changes = 1, 'DateTime assignment raises one event for both fields');
    Combined.DateEditor.ActiveSegment := dsYear;
    Combined.DateEditor.Step(1);
    Check(SameValue(Combined.DateTime, EncodeDate(2025, 2, 28) + EncodeTime(13, 14, 15, 321)),
      'Date editing preserves time and milliseconds');
    Combined.TimeEditor.ActiveSegment := tsMinute;
    Combined.TimeEditor.Step(1);
    Check(SameValue(Combined.DateTime, EncodeDate(2025, 2, 28) + EncodeTime(13, 15, 15, 321)),
      'Time editing preserves date');
    Combined.DateTime := -1.75;
    Check(SameValue(Combined.DateTime, -1.75), 'DateTime retains pre-epoch negative-date convention');
    I := Counter.Changes;
    Combined.DateTime := Combined.DateTime;
    Check(Counter.Changes = I, 'Unchanged DateTime does not notify');
    Combined.ReadOnly := True;
    Combined.DateEditor.Step(1);
    Combined.TimeEditor.Step(1);
    Check((Counter.Changes = I) and Combined.DateEditor.ReadOnly and Combined.TimeEditor.ReadOnly,
      'Combined readonly applies to both editors');
    Combined.Enabled := False;
    Check(not Combined.DateEditor.Enabled and not Combined.TimeEditor.Enabled, 'Combined disable reaches both fields');
    Combined.Enabled := True;
    Combined.RenderTo(Bitmap);
    Check(not Combined.HandleAllocated, 'DateTime preview renders without a window');
    Combined.OnChange := nil;
    Stream.WriteComponent(Combined);
    Stream.Position := 0;
    Copy := TERDDateTimePicker(Stream.ReadComponent(nil));
    try
      Check(SameValue(Copy.DateTime, Combined.DateTime) and Copy.ReadOnly and Copy.ShowSeconds and
        (Copy.DateFormat = Combined.DateFormat) and (Copy.TimeFormat = tf12Hour), 'DateTime DFM roundtrip');
      Check(Copy.ControlCount = 2, 'Streaming does not duplicate internal date/time editors');
    finally Copy.Free end;
  finally Bitmap.Free; Stream.Free; Picker.Free; Combined.Free; Counter.Free end;
end;

procedure TestTimePicker;
var Form: TForm; Picker: TTimeProbe; Stored, Copy: TERDTimePicker;
  Counter: TChangeProbe; Bitmap: TBitmap; Stream: TMemoryStream;
  Theme: TERDTheme; Mode: TERDThemeMode; R: TRect;
  SavedFormat: TFormatSettings; I: Integer; Rejected: Boolean;
  procedure CheckTime(Hour, Minute, Second, Millisecond: Word; const MessageText: string);
  var H, M, S, MS: Word;
  begin
    DecodeTime(Picker.Time, H, M, S, MS);
    Check((H = Hour) and (M = Minute) and (S = Second) and (MS = Millisecond), MessageText);
  end;
begin
  RegisterClass(TERDTimePicker);
  SavedFormat := FormatSettings;
  Form := TForm.CreateNew(nil);
  Counter := TChangeProbe.Create;
  Bitmap := TBitmap.Create;
  Stream := TMemoryStream.Create;
  try
    Stored := TERDTimePicker.Create(nil);
    try
      Stored.Time := EncodeTime(13, 30, 45, 321);
      Stored.TimeFormat := tf12Hour;
      Stored.ShowSeconds := True;
      Stored.ShowSpinButtons := False;
      Stored.ReadOnly := True;
      Stored.RenderTo(Bitmap);
      Check(not Stored.HandleAllocated, 'TimePicker renders before parenting without creating a window');
      Stream.WriteComponent(Stored);
    finally Stored.Free end;
    Stream.Position := 0;
    Copy := TERDTimePicker(Stream.ReadComponent(nil));
    try
      Check(SameValue(Copy.Time, EncodeTime(13, 30, 45, 321)) and
        (Copy.TimeFormat = tf12Hour) and Copy.ShowSeconds and not Copy.ShowSpinButtons and
        Copy.ReadOnly, 'TimePicker DFM preserves time, format, seconds, spin buttons and readonly');
    finally Copy.Free end;
    Picker := TTimeProbe.Create(Form);
    Picker.Parent := Form;
    Picker.OnChange := Counter.Changed;
    Check((Picker.Time = 0) and (Picker.TimeFormat = tf24Hour) and not Picker.ShowSeconds and
      Picker.ShowSpinButtons and Picker.TabStop, 'TimePicker defaults');
    Picker.Time := EncodeTime(23, 59, 45, 321);
    I := Counter.Changes;
    Picker.Time := Picker.Time;
    Check(Counter.Changes = I, 'Unchanged time does not raise OnChange');
    Picker.ActiveSegment := tsHour;
    Picker.Step(1);
    CheckTime(0, 59, 45, 321, 'Hour wraps without carrying into minutes');
    Picker.Step(-1);
    CheckTime(23, 59, 45, 321, 'Hour wraps backwards');
    Picker.ActiveSegment := tsMinute;
    Picker.Step(1);
    CheckTime(23, 0, 45, 321, 'Minute wraps without changing hour or seconds');
    Picker.ShowSeconds := True;
    Picker.ActiveSegment := tsSecond;
    Picker.Step(15);
    CheckTime(23, 0, 0, 321, 'Second wraps and retains milliseconds');
    Picker.ActiveSegment := tsHour;
    Picker.TypeText('13');
    Picker.TypeText('47');
    Picker.TypeText('52');
    CheckTime(13, 47, 52, 321, 'Digit input enters complete time through segments');
    Picker.ActiveSegment := tsMinute;
    Picker.PressKey(VK_HOME);
    CheckTime(13, 0, 52, 321, 'Home selects minimum for active field');
    Picker.PressKey(VK_END);
    CheckTime(13, 59, 52, 321, 'End selects maximum for active field');
    Picker.ActiveSegment := tsHour;
    Picker.TypeText('29');
    CheckTime(2, 59, 52, 321, 'Invalid second digit is rejected rather than creating hour 29');
    Picker.TimeFormat := tf12Hour;
    Picker.Time := EncodeTime(0, 15, 30, 0);
    Picker.ActiveSegment := tsHour;
    Picker.Step(1);
    CheckTime(1, 15, 30, 0, '12-hour field wraps from 12 to 1 within AM');
    Picker.ActiveSegment := tsPeriod;
    Picker.TypeText('p');
    CheckTime(13, 15, 30, 0, 'PM typing preserves minute and seconds');
    Picker.Step(1);
    CheckTime(1, 15, 30, 0, 'AM/PM stepping toggles by twelve hours');
    FormatSettings.TimeSeparator := '.';
    FormatSettings.TimeAMString := 'vorm';
    FormatSettings.TimePMString := 'nam';
    Check(Pos('01.15.30 vorm', Picker.DisplayText) = 1, 'Display respects locale separator and period labels');
    Picker.TypeText('n');
    CheckTime(13, 15, 30, 0, 'Locale period initial selects PM');
    Picker.ShowSeconds := False;
    Picker.ActiveSegment := tsSecond;
    Check((Picker.ActiveSegment = tsMinute) and Picker.SegmentRect(tsSecond).IsEmpty,
      'Hidden seconds cannot remain active or receive mouse hits');
    Picker.ActiveSegment := tsPeriod;
    Picker.TimeFormat := tf24Hour;
    Check(Picker.ActiveSegment = tsMinute, 'Hidden period cannot remain active');
    Picker.ReadOnly := True;
    I := Counter.Changes;
    Picker.TypeText('12');
    Picker.Step(1);
    Picker.PressKey(VK_END);
    Check(Counter.Changes = I, 'Readonly blocks digits, stepping and End');
    Picker.ReadOnly := False;
    Picker.Enabled := False;
    Picker.Step(1);
    Picker.TypeText('12');
    Check(Counter.Changes = I, 'Disabled picker blocks user edits');
    Picker.Enabled := True;
    Picker.Time := EncodeDate(2026, 10, 13) + 0.5;
    CheckTime(12, 0, 0, 0, 'Date part is normalized out of a time value');
    Picker.Time := -0.5;
    CheckTime(12, 0, 0, 0, 'Negative TDateTime follows absolute fractional time convention');
    Rejected := False;
    try Picker.Time := NaN except on E: EArgumentException do Rejected := True end;
    Check(Rejected, 'NaN time is rejected');
    Rejected := False;
    try Picker.Time := Infinity except on E: EArgumentException do Rejected := True end;
    Check(Rejected, 'Infinite time is rejected');
    Form.Show;
    Application.ProcessMessages;
    R := Picker.SegmentRect(tsHour);
    Picker.ClickAt((R.Left + R.Right) div 2, Picker.Height div 2);
    Check(Picker.Focused and (Picker.ActiveSegment = tsHour), 'Mouse selects and focuses hour segment');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = tsMinute, 'Right skips hidden seconds and navigates fields');
    Picker.PressKey(VK_RIGHT);
    Check(Picker.ActiveSegment = tsHour, 'Right wraps through visible fields');
    Picker.ClickAt(Picker.Width - 10, Picker.Height div 4);
    CheckTime(13, 0, 0, 0, 'Spin button changes the active field once on click');
    Theme := TERDTheme.Create(Form);
    Picker.Theme := Theme;
    for Mode := Low(TERDThemeMode) to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode;
      Picker.RenderTo(Bitmap);
      Check((Bitmap.Width = Picker.Width) and (Bitmap.Height = Picker.Height), 'TimePicker renders for each theme');
    end;
  finally
    FormatSettings := SavedFormat;
    Bitmap.Free;
    Stream.Free;
    Form.Free;
    Counter.Free;
  end;
end;

procedure TestCheckListBox;
var Form: TForm; List: TCheckListProbe; Copy: TERDCheckListBox;
  Counter: TCheckListCounter; Stream: TMemoryStream; Bitmap: TBitmap;
  Row: TRect; Item: TERDCheckListItem; I: Integer; Mode: TERDThemeMode;
  Theme: TERDTheme;
begin
  RegisterClass(TERDCheckListBox);
  Form := TForm.CreateNew(nil);
  Counter := TCheckListCounter.Create;
  Stream := TMemoryStream.Create;
  Bitmap := TBitmap.Create;
  try
    List := TCheckListProbe.Create(Form);
    List.Parent := Form;
    List.SetBounds(0, 0, 400, 160);
    List.OnClickCheck := Counter.Checked;
    Item := List.Items.Add;
    Item.Caption := 'Live TV';
    List.Items.Add.Caption := 'Movies';
    List.Items.Add.Caption := 'Unavailable';
    List.Items[2].Enabled := False;
    Check((List.ItemCount = 3) and (List.CheckedCount = 0), 'Checklist defaults and item count');
    Row := List.ItemRect(0);
    List.PressAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check((List.ItemIndex = 0) and not List.Checked[0], 'Caption selects without checking');
    List.PressAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0] and (Counter.Count = 1) and (Counter.LastIndex = 0),
      'Checkbox click checks once and reports index');
    List.PressAt(Row.Left + 14, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0] and (Counter.Count = 1), 'Release away from checkbox cancels toggle');
    List.AllowGrayed := True;
    List.Space;
    Check(List.State[0] = cbGrayed, 'Space cycles checked to grayed');
    List.Space;
    Check(List.State[0] = cbUnchecked, 'Space cycles grayed to unchecked');
    List.AllowGrayed := False;
    List.Space;
    List.Space;
    Check(List.State[0] = cbUnchecked, 'Without AllowGrayed Space uses two states');
    I := Counter.Count;
    List.Toggle(2);
    List.Enabled := False;
    List.Toggle(0);
    List.Enabled := True;
    Check((Counter.Count = I) and not List.Checked[2] and not List.Checked[0],
      'Disabled control and item block user checks');
    List.CheckOnClick := True;
    List.PressAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    List.ReleaseAt(Row.Left + 80, (Row.Top + Row.Bottom) div 2);
    Check(List.Checked[0], 'CheckOnClick enables caption toggle');
    I := Counter.Count;
    List.CheckAll(cbChecked);
    Check((List.CheckedCount = 2) and not List.Checked[2], 'CheckAll skips disabled items');
    List.CheckAll(cbChecked, True);
    Check((List.CheckedCount = 3) and (Counter.Count = I), 'Programmatic batch has no user event');
    List.Items[0].State := cbGrayed;
    Item.Index := 1;
    Check((List.Items[1] = Item) and (List.State[1] = cbGrayed), 'State follows item when reordered');
    List.Items.Delete(0);
    Check((List.Items[0] = Item) and (List.State[0] = cbGrayed), 'Removing earlier row preserves remaining state');
    // Stream the palette class rather than the test-only descendant.
    Copy := TERDCheckListBox.Create(nil);
    try
      Copy.Items.Assign(List.Items);
      Copy.AllowGrayed := True;
      Stream.WriteComponent(Copy);
    finally Copy.Free end;
    Stream.Position := 0;
    Copy := TERDCheckListBox(Stream.ReadComponent(nil));
    try
      Check((Copy.ItemCount = 2) and (Copy.State[0] = cbGrayed) and
        not Copy.Items[1].Enabled and Copy.Items[1].Checked, 'DFM stream preserves caption/state/enabled');
    finally Copy.Free end;
    Theme := TERDTheme.Create(Form);
    List.Theme := Theme;
    for Mode := Low(TERDThemeMode) to High(TERDThemeMode) do
    begin
      Theme.Mode := Mode;
      List.RenderTo(Bitmap);
      Check((Bitmap.Width = List.Width) and (Bitmap.Height = List.Height), 'Checklist renders in each theme');
    end;
    List.Items.BeginUpdate;
    try
      for I := 1 to 40 do List.Items.Add.Caption := 'Extra ' + IntToStr(I);
    finally List.Items.EndUpdate end;
    List.ItemIndex := List.ItemCount - 1;
    List.EnsureVisible(List.ItemIndex);
    Check(List.ScrollPos > 0, 'Checklist keyboard selection can reach rows below viewport');
    List.Items.Clear;
    Check((List.ItemCount = 0) and (List.CheckedCount = 0) and (List.ItemIndex = -1), 'Clear resets item selection');
  finally Bitmap.Free; Stream.Free; Form.Free; Counter.Free end;
end;

procedure TestSplitMenuGeometry;
var Form: TForm; Theme: TERDTheme; Split: TERDSplitButton; Menu: TERDPopupMenu;
  Popup: TWinControl; Item: TERDMenuItem; Bounds: TRect; Anchor: TPoint;
  Bitmap: TBitmap; M, Y, FirstHot, LastHot: Integer; Fill: TColor;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  try
    Form.SetBounds(100, 100, 500, 300);
    Theme := TERDTheme.Create(Form);
    Split := TERDSplitButton.Create(Form); Split.Parent := Form; Split.Theme := Theme;
    Split.AutoSize := False; Split.SetBounds(40, 40, 120, 40);
    Menu := TERDPopupMenu.Create(Form); Menu.Theme := Theme;
    Item := TERDMenuItem.Create(Menu); Item.Caption := 'Show dialog'; Menu.Items.Add(Item);
    Split.DropDownMenu := Menu;
    Form.Show; Application.ProcessMessages;
    Split.DropDown;
    Check(Split.ControlCount = 1, 'Split button owns one visible themed popup window');
    Popup := TWinControl(Split.Controls[0]);
    M := MulDiv(4, Split.CurrentPPI, 96);
    Anchor := Split.ClientToScreen(Point(M, Split.Height - M));
    GetWindowRect(Popup.Handle, Bounds);
    Check((Bounds.Left = Anchor.X) and (Bounds.Top = Anchor.Y),
      'Popup aligns with painted button left and bottom edges');
    Popup.Perform(WM_MOUSEMOVE, 0, MakeLParam(M + 2, M + 6));
    Bitmap.SetSize(Popup.Width, Popup.Height);
    Popup.PaintTo(Bitmap.Canvas.Handle, 0, 0);
    Fill := Bitmap.Canvas.Pixels[M + 2, Popup.Height div 2];
    FirstHot := -1; LastHot := -1;
    for Y := 1 to Popup.Height - 2 do
      if Bitmap.Canvas.Pixels[M + 2, Y] = Fill then
      begin
        if FirstHot < 0 then FirstHot := Y;
        LastHot := Y;
      end;
    Check((FirstHot > 0) and (FirstHot = Popup.Height - 1 - LastHot),
      'Single-item popup has balanced top and bottom row padding');
  finally ERDCloseThemedMenu; Bitmap.Free; Form.Free end;
end;

procedure TestFeedbackRegressions;
var Form: TForm; Values: TERDKeyValueList; Tags: TERDTagEdit;
  Token: TERDTagToken; Ribbon: TERDRibbon; Group: TERDRibbonGroup;
  First, Second, Independent: TERDRibbonItem; Y, I: Integer;
  FoundFirst, FoundSecond: Boolean;
  Bitmap: TBitmap; Normal, Hot, Down: TColor; TokenWidth: Integer;
begin
  Form := TForm.CreateNew(nil);
  Bitmap := TBitmap.Create;
  try
    Values := TERDKeyValueList.Create(Form);
    Values.Parent := Form;
    Values.SetBounds(0, 0, 400, 160);
    Values.SetValue('Application', 'Demo');
    Values.SetValue('Theme', 'ERDesigns Light');
    FoundFirst := False;
    FoundSecond := False;
    for Y := 0 to Values.Height - 1 do
    begin
      I := Values.RowAt(50, Y);
      FoundFirst := FoundFirst or (I = 0);
      FoundSecond := FoundSecond or (I = 1);
    end;
    Check(FoundFirst and FoundSecond, 'Key/value rows occupy distinct reachable rows');
    Tags := TERDTagEdit.Create(Form);
    Tags.Parent := Form;
    Tags.SetBounds(0, 180, 400, 80);
    Tags.AddTag('Live TV');
    Token := nil;
    for I := 0 to Tags.ControlCount - 1 do
      if Tags.Controls[I] is TERDTagToken then Token := TERDTagToken(Tags.Controls[I]);
    Check(Token <> nil, 'Tag creates a themed token');
    TokenWidth := Token.Width;
    Tags.CloseSpacing := 10;
    Check((Token.Width = TokenWidth + 6) and (Token.CloseSpacing = 10),
      'Close spacing relayouts existing tokens without recreating them');
    Tags.CloseSpacing := 4;
    Tags.TagColorStrength := 40;
    Check(Token.TagColorStrength = 40, 'Appearance changes propagate to existing tags');
    Token.RenderTo(Bitmap);
    Normal := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Token.Perform(WM_MOUSEMOVE, 0, MakeLParam(Token.Width - 12, Token.Height div 2));
    Token.RenderTo(Bitmap);
    Hot := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Token.Width - 12, Token.Height div 2));
    Token.RenderTo(Bitmap);
    Down := Bitmap.Canvas.Pixels[Token.Width - 8, Token.Height div 2 + 5];
    Check((Normal <> Hot) and (Hot <> Down), 'Tag close button paints distinct hover and pressed states');
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(4, Token.Height div 2));
    Check(Tags.Tags.Count = 1, 'Releasing away from close icon does not delete tag');

    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(8, Token.Height div 2));
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(8, Token.Height div 2));
    Check(Tags.Tags.Count = 1, 'Clicking tag caption does not remove tag');
    Token.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Token.Width - 14, Token.Height div 2));
    Token.Perform(WM_LBUTTONUP, 0, MakeLParam(Token.Width - 14, Token.Height div 2));
    Application.ProcessMessages;
    Check(Tags.Tags.Count = 0, 'Clicking tag close glyph removes tag');
    Ribbon := TERDRibbon.Create(Form);
    Group := Ribbon.Tabs.Add.Groups.Add;
    First := Group.Items.Add;
    Second := Group.Items.Add;
    Independent := Group.Items.Add;
    First.Kind := rikToggle;
    Second.Kind := rikToggle;
    Independent.Kind := rikToggle;
    First.GroupIndex := 1;
    Second.GroupIndex := 1;
    First.Execute;
    Second.Execute;
    Check(Second.Down and not First.Down, 'Radio ribbon items exclude peers');
    Second.Execute;
    Check(Second.Down, 'Radio item remains selected without AllowAllUp');
    Independent.Execute;
    Check(Independent.Down and Second.Down, 'Independent toggle preserves radio choice');
    Independent.Execute;
    Check(not Independent.Down, 'Independent ribbon toggle can be unchecked');
  finally Bitmap.Free; Form.Free end;
end;


procedure TEditKeyFilter.Filter(Sender: TObject; var Key: Char);
begin
  if Key = #8 then begin Inc(Calls); Key := #0; end;
end;

function TTextInputProbe.EditMenu: TERDPopupMenu;
begin Result := DefaultEditMenu; end;
procedure TestTextInputEditMenu;
var Form: TForm; Input: TTextInputProbe; Menu: TERDPopupMenu;
begin
  Form := TForm.Create(nil);
  try
    Input := TTextInputProbe.Create(Form); Input.Parent := Form;
    Input.Text := 'Unicode ' + #$D83D#$DD12; Input.SelectAll;
    Menu := Input.EditMenu;
    Check(Menu.Items.Count = 9, 'Themed input menu contains editing commands and separators');
    Check(Menu.Items[3].Enabled and Menu.Items[4].Enabled, 'Selection enables Cut and Copy');
    Menu.Items[4].Click; Check(Clipboard.AsText = Input.Text, 'Menu copies Unicode text');
    Input.ReadOnly := True; Menu := Input.EditMenu;
    Check(Menu.Items[4].Enabled and not Menu.Items[3].Enabled and not Menu.Items[6].Enabled,
      'ReadOnly menu permits Copy but prevents modification');
    Input.ReadOnly := False; Input.PasswordChar := #$2022; Menu := Input.EditMenu;
    Check(not Menu.Items[3].Enabled and not Menu.Items[4].Enabled, 'Password menu disables Cut and Copy');
    Input.PasswordChar := #0; Input.SelectAll; Menu := Input.EditMenu;
    Menu.Items[6].Click; Check(Input.Text = '', 'Menu Delete removes selection');
    Menu := Input.EditMenu; Check(Menu.Items[0].Enabled, 'Menu Undo enabled after deletion');
    Menu.Items[0].Click; Check(Input.Text <> '', 'Menu Undo restores Unicode text');
    Menu := Input.EditMenu; Check(Menu.Items[1].Enabled, 'Menu Redo enabled after undo');
  finally Form.Free; end;
end;

procedure CheckGraphemes(const S: UnicodeString; const Expected: array of Integer);
var Actual: TArray<Integer>; I: Integer;
begin
  Actual := ERDGraphemeBoundaries(S);
  Check(Length(Actual) = Length(Expected), 'Unicode grapheme boundary count');
  for I := 0 to High(Expected) do
    Check(Actual[I] = Expected[I], 'Unicode grapheme UTF-16 boundary');
end;
procedure TestUnicodeTextModel;
var M: TERDTextModel; Family, Flag, Tone, Accent: UnicodeString;
begin
  {$I GraphemeConformance.inc}
  Family := #$D83D#$DC69#$200D#$D83D#$DC69#$200D#$D83D#$DC67;
  Flag := #$D83C#$DDE7#$D83C#$DDEA;
  Tone := #$D83D#$DC4D#$D83C#$DFFD;
  Accent := 'e' + #$0301;
  M := TERDTextModel.Create;
  try
    M.SetText('A' + Accent + Flag + Tone + Family);
    Check(M.Previous(Length(M.Text)) = 1 + Length(Accent + Flag + Tone), 'Family moves as one grapheme');
    M.DeleteBackward;
    Check(M.Text = 'A' + Accent + Flag + Tone, 'Backspace removes whole family');
    M.Undo; Check(M.Text = 'A' + Accent + Flag + Tone + Family, 'Unicode undo restores family');
    M.Redo; M.DeleteBackward;
    Check(M.Text = 'A' + Accent + Flag, 'Backspace removes skin tone with emoji');
    M.Select(1 + Length(Accent), 1 + Length(Accent)); M.DeleteForward;
    Check(M.Text = 'A' + Accent, 'Delete removes entire flag');
    M.Select(2, 2); Check(M.Caret = 1, 'Selection cannot split combining sequence');
    M.Select(Length(M.Text), Length(M.Text)); M.ReplaceSelection(Family, Length(M.Text) + 2);
    Check(M.Text = 'A' + Accent, 'MaxLength cannot insert partial emoji family');
    M.SetText('A' + #$D800 + 'B');
    Check(M.Text = 'A' + #$FFFD + 'B', 'Invalid surrogate replaced without corrupting UTF-16');
  finally M.Free; end;
end;
procedure TestWindowsEditInteractionParity;
var Form: TForm; Edit: TERDEdit; Reference: TEdit; P: TPoint; Filter: TEditKeyFilter;
    X, Y, ReferenceStart, ReferenceLength: Integer;
  procedure CheckState(const Action: string);
  begin
    Check((Edit.Text = Reference.Text) and (Edit.SelStart = Reference.SelStart) and
      (Edit.SelLength = Reference.SelLength), 'Windows edit parity: ' + Action);
  end;
  procedure Reset;
  begin
    Edit.Text := 'one two three'; Reference.Text := Edit.Text;
    Edit.SelStart := 6; Reference.SelStart := 6;
    Edit.SelLength := 0; Reference.SelLength := 0;
  end;
  procedure Key(KeyCode: Word);
  begin Edit.Editor.Perform(WM_KEYDOWN, KeyCode, 0); Reference.Perform(WM_KEYDOWN, KeyCode, 0); end;
  procedure DoubleClick(Control: TWinControl; AtX, AtY: Integer);
  begin
    Control.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONUP, 0, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONDBLCLK, MK_LBUTTON, MakeLParam(AtX, AtY));
    Control.Perform(WM_LBUTTONUP, 0, MakeLParam(AtX, AtY));
  end;
begin
  Form := TForm.Create(nil); Filter := TEditKeyFilter.Create;
  try
    Edit := TERDEdit.Create(Form); Edit.Parent := Form; Edit.Width := 300;
    // Native edit exists only as the Windows behavioral reference in this test.
    Reference := TEdit.Create(Form); Reference.Parent := Form;
    Reference.SetBounds(0, 50, 300, 28); Reference.BorderStyle := bsNone;
    Reference.AutoSize := False; Reference.Font.Assign(Edit.Editor.Font);
    Form.Show; Reset;
    Key(VK_LEFT); CheckState('Left'); Key(VK_RIGHT); CheckState('Right');
    Key(VK_HOME); CheckState('Home'); Key(VK_END); CheckState('End');
    Reset; Edit.Editor.Perform(EM_SETSEL, 1, 4); Reference.Perform(EM_SETSEL, 1, 4);
    Edit.Editor.Perform(WM_CHAR, Ord('Z'), 0); Reference.Perform(WM_CHAR, Ord('Z'), 0);
    CheckState('typing replaces selection');
    Check(Edit.Editor.Modified = Reference.Modified, 'Modified flag matches native typing');
    Check(Edit.Editor.CanUndo, 'Edit exposes CanUndo after replacement');
    Key(VK_BACK); Edit.Editor.Perform(WM_CHAR, 8, 0); Reference.Perform(WM_CHAR, 8, 0);
    CheckState('Backspace'); Key(VK_DELETE); CheckState('Delete');
    Reset; Reference.SetFocus; Reference.SelStart := 6; Reference.SelLength := 0;
    Check(GetCaretPos(P), 'Native reference caret available');
    DoubleClick(Reference, P.X + 1, P.Y + 4);
    ReferenceStart := Reference.SelStart; ReferenceLength := Reference.SelLength;
    Edit.SetFocus; Edit.SelStart := 6; Edit.SelLength := 0;
    Check(GetCaretPos(P), 'ERD caret available for double-click test');
    X := P.X + 1; Y := P.Y + 4;
    DoubleClick(Edit.Editor, X, Y);
    Check((Edit.SelStart = ReferenceStart) and (Edit.SelLength = ReferenceLength),
      'Double-click selects same word range as native Windows edit');
    Check(Edit.SelLength > 0, 'Second mouse-down does not collapse double-click selection');
    Check(Edit.Editor.SelText = 'two', 'Double-click excludes trailing spaces and adjacent words');
    Edit.ReadOnly := True;
    Edit.Editor.Perform(WM_CHAR, Ord('X'), 0);
    Check(Edit.Text = 'one two three', 'ReadOnly edit remains selectable but blocks typing');
    Edit.ReadOnly := False; Edit.Editor.ClearUndo;
    Check(not Edit.Editor.CanUndo and not Edit.Editor.CanRedo, 'ClearUndo clears both histories');
    Edit.Text := 'reset'; Reference.Text := 'reset';
    Check(not Edit.Editor.Modified and not Reference.Modified, 'Programmatic Text resets Modified');
    Reset; Edit.OnKeyPress := Filter.Filter; Reference.OnKeyPress := Filter.Filter;
    Key(VK_BACK); Edit.Editor.Perform(WM_CHAR, 8, 0); Reference.Perform(WM_CHAR, 8, 0);
    CheckState('OnKeyPress can reject Backspace');
    Check((Filter.Calls = 2) and (Edit.Text = 'one two three'), 'Both controls dispatch cancellable Backspace KeyPress');
    Edit.OnKeyPress := nil; Reference.OnKeyPress := nil;
    Edit.NumbersOnly := True; Edit.Text := '123'; Edit.SelectAll;
    Edit.Editor.Perform(WM_CHAR, Ord('A'), 0);
    Check(Edit.Text = '123', 'Rejected numeric input does not delete selection');
  finally Form.Free; Filter.Free; end;
end;

procedure TestCodeLanguageServices;
var Form: TForm; Editor, Temporary: TERDCodeEditor; Service: TLanguageServiceProbe;
    ServiceReference: IERDCodeLanguageService; Sink: IERDLanguageResponses;
    Old, Current: TERDLanguageRequest; Items: TArray<TERDCompletionItem>;
    Target: TERDDefinitionTarget; Version: Int64; Worker: TThread; OffThreadAccepted: Boolean;
    Keyboard, SavedKeyboard: TKeyboardState; Popup: TERDPopupList; I, X, Y, InkCount, FooterTop: Integer;
    FooterDC: HDC; FooterFill: COLORREF; Corner: TPoint; WindowRect: TRect;
begin
  Form := TForm.Create(nil); Service := TLanguageServiceProbe.Create; ServiceReference := Service;
  try
    Editor := TERDCodeEditor.Create(Form); Editor.Parent := Form;
    Editor.LanguageService := ServiceReference; Editor.DocumentUri := 'memory:///test.ts';
    Editor.Text := 'con'; Editor.SelStart := 3;
    Check(Editor.RequestCompletion, 'Language provider receives completion request');
    Old := Service.Snapshot; Sink := Service.Responses;
    Check((Old.Text = 'con') and (Old.Caret = 3) and (Old.DocumentUri = Editor.DocumentUri), 'Language request contains full source snapshot and URI');
    Editor.RequestCompletion; Current := Service.Snapshot;
    SetLength(Items, 1); Items[0].Start := 0; Items[0].Count := 3;
    Items[0].LabelText := 'console'; Items[0].InsertText := 'console';
    Check(not Sink.Completion(Old, Items) and (Service.Cancels > 0), 'Superseded request is cancelled and rejected');
    Check(Sink.Completion(Current, Items), 'Newest completion result accepted');
    Items[0].InsertText := 'mutated';
    Check(Editor.Completions[0].InsertText = 'console', 'Completion results are copied');
    Check(Editor.ApplyCompletion(0) and (Editor.Text = 'console'), 'Completion replaces provider range');
    Editor.Undo; Check((Editor.Text = 'con') and (Editor.SelStart = 3), 'Completion undo restores source and original caret');
    Editor.RequestHover; Old := Service.Snapshot; Version := Editor.DocumentVersion;
    Editor.ReplaceSelection('x'); Editor.Undo;
    Check((Editor.Text = Old.Text) and (Editor.DocumentVersion > Version) and not Sink.Hover(Old, 'stale'),
      'Edit then Undo still rejects old result for identical source');
    Editor.RequestHover; Old := Service.Snapshot; Editor.SelStart := 0; Editor.SelStart := 3;
    Check(not Sink.Hover(Old, 'stale caret'), 'Caret moving away and back invalidates old request');
    Editor.RequestHover; Current := Service.Snapshot;
    OffThreadAccepted := True;
    Worker := TThread.CreateAnonymousThread(procedure begin OffThreadAccepted := Sink.Hover(Current, 'wrong thread'); end);
    Worker.FreeOnTerminate := False;
    try Worker.Start; Worker.WaitFor; finally Worker.Free; end;
    Check(not OffThreadAccepted, 'Provider response on worker thread rejected without touching VCL');
    Check(Sink.Hover(Current, 'console documentation') and (Editor.HoverText = 'console documentation'), 'Hover result is available to host');
    Editor.RequestHover; Old := Service.Snapshot; Editor.CancelLanguageRequests;
    Check(not Sink.Hover(Old, 'cancelled'), 'Explicit cancellation rejects delayed result');
    Editor.RequestCompletion; Current := Service.Snapshot; Items[0].InsertText := 'console';
    Items[0].Kind := 3; Items[0].Detail := '(value: string): void';
    Items[0].Documentation := 'Writes a value to the example console.';
    Sink.Completion(Current, Items); Editor.ReadOnly := True;
    Check(not Editor.ApplyCompletion(0) and (Editor.Text = 'con'), 'ReadOnly blocks completion application');
    Editor.ReadOnly := False; Editor.MaxLength := 3;
    Check(not Editor.ApplyCompletion(0), 'MaxLength rejects oversized completion'); Editor.MaxLength := 0;
    Editor.RequestDefinition; Current := Service.Snapshot; Target := Default(TERDDefinitionTarget);
    Target.Found := True; Target.Start := 0; Target.Count := 3;
    Check(Sink.Definition(Current, Target) and (Editor.SelStart = 0) and (Editor.SelLength = 3), 'Same-document definition selects target');
    Editor.RequestDefinition; Current := Service.Snapshot; Target := Default(TERDDefinitionTarget);
    Check(Sink.Definition(Current, Target), 'No-definition result completes request without navigation');
    Editor.Text := 'con'; Editor.SelStart := 3; Form.Show; Editor.SetFocus;
    Check((Editor.Perform(WM_GETDLGCODE, VK_SPACE, 0) and DLGC_WANTMESSAGE) <> 0,
      'Editor claims completion space from form dialog routing');
    GetKeyboardState(SavedKeyboard); Keyboard := SavedKeyboard; Keyboard[VK_CONTROL] := $80;
    try
      SetKeyboardState(Keyboard); Version := Service.Requests;
      Check(Editor.Perform(CN_KEYDOWN, VK_SPACE, 0) = 1, 'Editor claims Ctrl+Space before form shortcut processing');
      Check(Service.Requests = Version + 1, 'Ctrl+Space early key route requests completion');
      Version := Service.Requests;
      Editor.Perform(CM_DIALOGKEY, VK_SPACE, 0);
      Check((Service.Requests = Version + 1) and (Service.Snapshot.Kind = lrCompletion),
        'Ctrl+Space dialog route requests completion');
      Version := Service.Requests; Editor.Perform(WM_KEYDOWN, VK_SPACE, 0);
      Check(Service.Requests = Version + 1, 'Ctrl+Space key route requests completion');
      Version := Service.Requests; Editor.Perform(WM_KEYDOWN, Ord('J'), 0);
      Editor.Perform(WM_CHAR, 10, 0);
      Check((Service.Requests = Version + 1) and (Editor.Text = 'con'),
        'Ctrl+J requests completion without inserting a newline');
    finally SetKeyboardState(SavedKeyboard); end;
    Editor.RequestCompletion; Current := Service.Snapshot; Items[0].InsertText := 'console';
    Items[0].Kind := 3; Items[0].Detail := '(value: string): void';
    Items[0].Documentation := 'Writes a value to the example console.';
    Check(Sink.Completion(Current, Items), 'Focused completion result opens popup');
    Popup := nil;
    for I := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[I] is TERDPopupList then Popup := TERDPopupList(Editor.Components[I]);
    Check((Popup <> nil) and Popup.IsOpen and (Popup.Items[0] = 'console'),
      'Completion keeps label separate from kind and signature');
    Check(Popup.Height - Popup.ClientHeight > 60, 'Completion reserves a documentation and keyboard footer');
    RedrawWindow(Popup.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_UPDATENOW);
    GetWindowRect(Popup.Handle, WindowRect);
    Corner := Popup.ClientToScreen(Point(0, Popup.ClientHeight));
    FooterTop := Corner.Y - WindowRect.Top;
    FooterDC := GetWindowDC(Popup.Handle);
    try
      FooterFill := GetPixel(FooterDC, 2, FooterTop + 8); InkCount := 0;
      for Y := FooterTop + 5 to FooterTop + 23 do
        for X := 10 to Min(Popup.Width - 12, 280) do
          if GetPixel(FooterDC, X, Y) <> FooterFill then Inc(InkCount);
      Check(InkCount > 15, 'Buffered non-client footer contains visible signature text');
    finally ReleaseDC(Popup.Handle, FooterDC); end;

    Check((Editor.Completions[0].Kind = 3) and (Editor.Completions[0].Documentation = Items[0].Documentation),
      'Completion retains symbol kind and documentation');
    Editor.Perform(WM_KEYDOWN, VK_RETURN, 0); Editor.Perform(WM_CHAR, 13, 0);
    Check(Editor.Text = 'console', 'Enter accepts completion without inserting newline');
    Editor.Undo; Check(Editor.Text = 'con', 'Keyboard completion remains one undo transaction');
    Editor.RequestCompletion; Current := Service.Snapshot;
    Check(Sink.Completion(Current, nil), 'Manual empty completion result is accepted');
    Editor.Perform(WM_KEYDOWN, VK_RETURN, 0); Editor.Perform(WM_CHAR, 13, 0);
    Check(Editor.Text = 'con', 'No-suggestions popup cannot insert a completion or newline');
    Editor.Text := 'console'; Editor.SelStart := 7; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.TriggerCharacter = '.') and
      (Service.Snapshot.Text = 'console.') and (Service.Snapshot.Caret = 8), 'Dot requests member completion after insertion');
    Old := Service.Snapshot; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('l'), 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.Text = 'console.l'),
      'Typing a member prefix refreshes pending completion');
    Check(not Sink.Completion(Old, nil), 'Prefix refresh rejects the old dot response');
    Version := Service.Requests; Editor.Perform(WM_KEYDOWN, VK_BACK, 0); Editor.Perform(WM_CHAR, 8, 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.Text = 'console.'),
      'Backspace refreshes completion within its prefix');
    Version := Service.Requests; Editor.Perform(WM_KEYDOWN, VK_BACK, 0); Editor.Perform(WM_CHAR, 8, 0);
    Check((Service.Requests = Version) and (Editor.Text = 'console'), 'Removing the member dot ends prefix completion');
    Editor.Text := 'console?'; Editor.SelStart := 8; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check((Service.Requests = Version + 1) and (Service.Snapshot.Text = 'console?.'), 'Optional-chain dot requests member completion');
    Editor.Text := '// console'; Editor.SelStart := Length(Editor.Text); Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Dot in line comment does not trigger completion');
    Editor.Text := '"console"'; Editor.SelStart := 8; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Dot in string does not trigger completion');
    Editor.AutoMemberCompletion := False; Editor.Text := 'console'; Editor.SelStart := 7; Version := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Service.Requests = Version, 'Automatic member completion can be disabled');
    Editor.AutoMemberCompletion := True; Editor.RequestCompletion;
    Check(Service.Snapshot.TriggerCharacter = #0, 'Manual completion has no trigger character');
    Temporary := TERDCodeEditor.Create(nil);
    try Temporary.LanguageService := ServiceReference; Temporary.RequestHover; Old := Service.Snapshot; Sink := Service.Responses;
    finally Temporary.Free; end;
    Check(not Sink.Hover(Old, 'late'), 'Retained response sink is safe after editor destruction');
    Editor.LanguageService := nil;
    Check(not Editor.RequestCompletion and (Editor.LanguageError <> ''),
      'Manual completion explains a missing provider');
  finally Form.Free; end;
end;

procedure TPCBHistoryProbe.Validate(Sender: TObject; const Item: TERDPCBDesignItem);
begin
  if (RejectCaption <> '') and (Item.Caption = RejectCaption) then raise EArgumentException.Create('History snapshot rejected by the host');
end;
procedure TPCBHistoryProbe.Changed(Sender: TObject);
begin Inc(Changes); SawUndo := Scene.CanUndo; SawRedo := Scene.CanRedo; end;
procedure TestPCBHistoryValidation;
var Scene: TERDPCBScene; Probe: TPCBHistoryProbe; Index: Integer; Item: TERDPCBDesignItem; Before: string; Failed: Boolean;
begin
  Scene := TERDPCBScene.Create(nil); Probe := TPCBHistoryProbe.Create;
  try
    Probe.Scene := Scene; Index := Scene.Add(piNote, PointF(0,0)); Item := Scene.Item(Index);
    Item.Caption := 'Old'; Scene.UpdateItem(Index, Item); Scene.ResetHistory;
    Item.Caption := 'New'; Scene.UpdateItem(Index, Item);
    Scene.OnValidateItem := Probe.Validate; Scene.OnChange := Probe.Changed;
    Before := Scene.ToJSON; Probe.RejectCaption := 'Old'; Failed := False;
    try Scene.Undo; except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ToJSON = Before) and Scene.CanUndo and not Scene.CanRedo and (Probe.Changes = 0),
      'Rejected undo retains content, selection, history and change-event state');
    Probe.RejectCaption := ''; Scene.Undo;
    Check((Scene.Item(Index).Caption = 'Old') and not Scene.CanUndo and Scene.CanRedo and
      (Probe.Changes = 1) and not Probe.SawUndo and Probe.SawRedo, 'Retried undo succeeds and listeners see completed history');
    Before := Scene.ToJSON; Probe.RejectCaption := 'New'; Failed := False;
    try Scene.Redo; except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ToJSON = Before) and not Scene.CanUndo and Scene.CanRedo and (Probe.Changes = 1),
      'Rejected redo preserves both history stacks and emits no content change');
    Probe.RejectCaption := ''; Scene.Redo;
    Check((Scene.Item(Index).Caption = 'New') and Scene.CanUndo and not Scene.CanRedo and
      (Probe.Changes = 2) and Probe.SawUndo and not Probe.SawRedo, 'Retried redo restores the original pending state');
  finally Scene.Free; Probe.Free; end;
end;

procedure TPCBContrastProbe.Render(Bitmap: TBitmap);
begin Bitmap.SetSize(ClientWidth, ClientHeight); PaintControl(Bitmap.Canvas); end;
procedure TestPCBThemeContrast;
var Form: TForm; Designer: TPCBContrastProbe; Theme: TERDTheme; Mode: TERDThemeMode;
    OffGrid, OnGrid: TBitmap; X, Y, MaxDifference, Count, Index, Selected: Integer;
    NoteType: TERDPCBNoteType; Item: TERDPCBDesignItem; P, Q: TPoint; Palette: TERDThemePalette; StatusColor: TColor;
  function Difference(A, B: TColor): Integer;
  var CA, CB: Cardinal;
  begin
    CA := ColorToRGB(A); CB := ColorToRGB(B);
    Result := Max(Abs(Integer(GetRValue(CA))-Integer(GetRValue(CB))),
      Max(Abs(Integer(GetGValue(CA))-Integer(GetGValue(CB))), Abs(Integer(GetBValue(CA))-Integer(GetBValue(CB)))));
  end;
begin
  Form := TForm.Create(nil); OffGrid := TBitmap.Create; OnGrid := TBitmap.Create;
  try
    Theme := TERDTheme.Create(Form); Designer := TPCBContrastProbe.Create(Form); Designer.Parent := Form;
    Designer.Theme := Theme; Designer.SetBounds(0,0,500,400); Designer.AntiAlias := False;
    for Mode := tmERDesignsLight to tmERDesignsDark do
    begin
      Theme.Mode := Mode; Designer.ShowGrid := False; Designer.Render(OffGrid);
      Designer.ShowGrid := True; Designer.Render(OnGrid); Count := 0; MaxDifference := 0;
      for Y := MulDiv(64,Designer.CurrentPPI,96) to OnGrid.Height-1 do for X := 0 to OnGrid.Width-1 do
        if OnGrid.Canvas.Pixels[X,Y] <> OffGrid.Canvas.Pixels[X,Y] then
        begin Inc(Count); MaxDifference := Max(MaxDifference, Difference(OnGrid.Canvas.Pixels[X,Y], OffGrid.Canvas.Pixels[X,Y])); end;
      Check((Count > 20) and (MaxDifference >= 38), 'Grid dots visibly contrast with light and dark canvases');
    end;
    Index := Designer.AddDesignItem(piNote, PointF(0,0)); Item := Designer.Scene.Item(Index);
    Item.Caption := ''; Item.Width := 80; Item.Height := 50; Designer.Scene.UpdateItem(Index, Item); Designer.FitBoard;
    for Mode := tmERDesignsLight to tmERDesignsDark do
    begin
      Theme.Mode := Mode; Palette := Theme.Palette;
      for NoteType := ntNone to ntQuestion do
      begin
        Item := Designer.Scene.Item(Index); Item.NoteType := NoteType; Designer.Scene.UpdateItem(Index, Item);
        StatusColor := Palette.Subtle;
        case NoteType of ntInfo, ntQuestion: StatusColor := Palette.Accent; ntWarning: StatusColor := Palette.Warning; ntError: StatusColor := Palette.Danger; end;
        for Selected := 0 to 1 do
        begin
          if Selected = 0 then Designer.Scene.ClearSelection else Designer.Scene.Select(Index);
          Designer.Render(OnGrid); P := Designer.BoardToClient(PointF(Item.X, Item.Y+Item.Height));
          Q := Designer.BoardToClient(PointF(Item.X+Item.Width, Item.Y));
          Check(Difference(OnGrid.Canvas.Pixels[(P.X+Q.X) div 2,(P.Y+Q.Y) div 2], Palette.Background) <= 34,
            'Notes retain a subtle tint when selected and unselected in both themes');
          if NoteType = ntNone then Continue;
          Count := 0;
          for Y := P.Y+MulDiv(11,Designer.CurrentPPI,96) to P.Y+MulDiv(25,Designer.CurrentPPI,96) do
            for X := P.X+MulDiv(11,Designer.CurrentPPI,96) to P.X+MulDiv(25,Designer.CurrentPPI,96) do
              if Difference(OnGrid.Canvas.Pixels[X,Y], StatusColor) >= 80 then Inc(Count);
          Check(Count >= 4, 'Every note status icon has visible contrasting ink on its badge');
        end;
      end;
    end;
  finally OnGrid.Free; OffGrid.Free; Form.Free; end;
end;

procedure TPCBPaletteSink.Chosen(Sender: TObject);
begin
  Inc(Choices); LastID := TERDPCBComponentPalette(Sender).SelectedTypeID;
  if Designer <> nil then Designer.BeginTypePlacement(LastID);
end;
procedure TPCBPaletteSink.Selection(Sender: TObject);
begin Inc(Changes); end;
procedure TPCBPaletteSink.Cancelled(Sender: TObject);
begin Inc(Cancels); if Designer <> nil then Designer.CancelPlacement; end;
procedure TPCBPaletteSink.Started(Sender: TObject; var DragObject: TDragObject);
begin Inc(Starts); LastID := TERDPCBComponentPalette(Sender).SelectedTypeID; end;
procedure TPCBPaletteSink.Ended(Sender, Target: TObject; X, Y: Integer);
begin Inc(Ends); LastID := TERDPCBComponentPalette(Sender).SelectedTypeID; end;
procedure TPCBValidationProbe.Validate(Sender: TObject; const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
var Items: TArray<TERDPCBDesignItem>; Pins: TArray<TERDPCBValidationPin>;
begin
  Inc(Rules); LastSender := Sender;
  if Reject then raise EArgumentException.Create('Custom rule failure');
  if Mutate then
  begin
    Items := Context.Items; if Length(Items) > 0 then Items[0].Name := 'Changed snapshot';
    Pins := Context.Pins; if (Length(Pins) > 0) and (Length(Pins[0].Capabilities) > 0) then Pins[0].Capabilities[0] := 'changed';
  end;
  Report.Add(vsInfo,'custom.check','Custom validation executed');
end;
procedure TPCBValidationProbe.Navigate(Sender: TObject; const Issue: TERDPCBValidationIssue);
begin Inc(Navigations); LastSender := Sender; LastID := Issue.ItemID; end;
procedure TestPCBValidation;
var Scene: TERDPCBScene; Validator: TERDPCBValidator; Context: TERDPCBValidationContext;
    Pins: TArray<TERDPCBValidationPin>; Issues: TArray<TERDPCBValidationIssue>; Probe: TPCBValidationProbe;
    Index, Other, I: Integer; Item: TERDPCBDesignItem; Before: string; Definition: TERDPCBComponentType;
  function Has(const Code: string; const Field: string = ''): Boolean;
  var Issue: TERDPCBValidationIssue;
  begin Result := False; for Issue in Issues do if (Issue.Code = Code) and ((Field = '') or (Issue.PropertyName = Field)) then Exit(True); end;
begin
  Scene := TERDPCBScene.Create(nil); Validator := TERDPCBValidator.Create; Probe := TPCBValidationProbe.Create;
  try
    Index := Scene.Add(piUART,PointF(0,0)); Other := Scene.Add(piNote,PointF(20,20));
    Before := Scene.ToJSON; Scene.ResetHistory;
    Context := ERDPCBCaptureValidation(Scene,nil,False); Issues := Validator.Validate(Context);
    Check(Has('pin.missing','pin') and Has('pin.missing','secondary_pin') and Has('template.unavailable'), 'Validation explains missing bus roles and unavailable template');
    Check((Scene.ToJSON = Before) and not Scene.CanUndo, 'Validation leaves content, selection and history untouched');
    for I := 0 to High(Issues) do Check(Issues[I].ItemID <> Scene.Item(Other).ID, 'Notes do not require pin assignments');
    SetLength(Pins,2); Pins[0].ID := 'TX'; Pins[0].Capabilities := ['uart-tx']; Pins[1].ID := 'RX'; Pins[1].Capabilities := ['uart-rx'];
    Item := Scene.Item(Index); Item.Pin := 'TX'; Item.SecondaryPin := 'RX'; Scene.UpdateItem(Index,Item);
    Context := ERDPCBCaptureValidation(Scene,Pins,True); Issues := Validator.Validate(Context);
    Check(Length(Issues) = 0, 'Complete compatible bus has no validation messages');
    Pins[0].Capabilities[0] := 'digital-output';
    Issues := Validator.Validate(Context); Check(Length(Issues) = 0, 'Captured pin capabilities are independent of caller arrays');
    Context.Pins[0].Capabilities[0] := 'digital-output'; Issues := Validator.Validate(Context);
    Check(Has('pin.capability','pin'), 'Validation reports incompatible assigned pin capability');
    Context.Items[Index].Pin := 'missing'; Issues := Validator.Validate(Context);
    Check(Has('pin.unavailable','pin'), 'Validation identifies pins absent from the template');
    Context := ERDPCBCaptureValidation(Scene,nil,False); Context.Items[Index].SecondaryPin := 'TX'; Issues := Validator.Validate(Context);
    Check(Has('pin.role-conflict'), 'Bus roles cannot share one pin');
    Other := Scene.Add(piUART,PointF(40,0)); Item := Scene.Item(Other); Item.Pin := 'TX'; Item.SecondaryPin := 'RX'; Scene.UpdateItem(Other,Item);
    Context := ERDPCBCaptureValidation(Scene,nil,False); Issues := Validator.Validate(Context);
    Check(Has('pin.conflict'), 'Validation detects active drivers sharing a pin');
    Context.Items[Other].Enabled := False; Context.Items[Other].Pin := ''; Context.Items[Other].SecondaryPin := ''; Issues := Validator.Validate(Context);
    Check(not Has('pin.conflict') and not Has('pin.missing'), 'Disabled drivers do not claim pins or require missing assignments');
    Context.Items[Index].TypeID := 'unknown.saved.type'; Issues := Validator.Validate(Context);
    Check(Has('type.unknown'), 'Unregistered saved component types are reported');
    ERDPCBFindComponentType('erd.timer',Definition); Definition.TypeID := 'smoke.validation.required'; Definition.NamePrefix := 'RequiredTimer';
    SetLength(Definition.Properties,1); Definition.Properties[0].Name := 'label'; Definition.Properties[0].Caption := 'Label';
    Definition.Properties[0].Required := True; Definition.Properties[0].DefaultValue := 'Default'; ERDPCBRegisterComponentType(Definition);
    Other := Scene.AddType(Definition.TypeID,PointF(0,20)); Context := ERDPCBCaptureValidation(Scene,nil,False);
    Context.Items[Other].PropertiesJSON := '{"label":""}'; Issues := Validator.Validate(Context);
    Check(Has('property.invalid','label'), 'Required custom schema properties participate in validation');
    Context := ERDPCBCaptureValidation(Scene,Pins,True); Before := Scene.ToJSON;
    Validator.OnValidate := Probe.Validate; Probe.Mutate := True; Issues := Validator.Validate(Context);
    Check(Has('custom.check') and (Probe.Rules = 1), 'Host rules append to built-in messages');
    Check((Scene.ToJSON = Before) and (Context.Items[0].Name <> 'Changed snapshot') and (Context.Pins[0].Capabilities[0] <> 'changed'), 'Custom rules only receive isolated snapshots');
  finally Probe.Free; Validator.Free; Scene.Free; end;
end;
procedure TestPCBValidationPanel;
var Form: TForm; Panel, CopyPanel: TERDPCBValidationPanel; Probe: TPCBValidationProbe;
    Report: TERDPCBValidationReport; Stream: TMemoryStream; Issues: TArray<TERDPCBValidationIssue>;
    Context: TERDPCBValidationContext; Value: TERDPCBValidationIssue; I, Row: Integer; Failed: Boolean; SavedKey: string;
begin
  Form := TForm.Create(nil); Probe := TPCBValidationProbe.Create; Report := TERDPCBValidationReport.Create;
  try
    Panel := TERDPCBValidationPanel.Create(Form); Panel.Parent := Form; Panel.SetBounds(0,0,600,300);
    Panel.OnNavigate := Probe.Navigate;
    Report.Add(vsWarning,'pin.missing','Assign TX pin','id-uart','UART1','pin');
    Report.Add(vsError,'pin.conflict','Pin already assigned','id-led','LED1','pin','U7.2');
    Report.Add(vsInfo,'design.info','Design information'); Issues := Report.Issues; Panel.SetIssues(Issues);
    Check((Panel.IssueCount = 3) and (Panel.SeverityCount(vsError) = 1) and (Panel.ItemList.ItemCount = 6) and (Probe.Navigations = 0), 'Validation panel groups messages without automatic navigation');
    Issues[0].Code := ''; Failed := False;
    try Panel.SetIssues(Issues); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Panel.IssueCount = 3) and (Panel.Issue(0).Code = 'pin.missing'), 'Invalid replacement report preserves independent issue copies');
    Issues := Report.Issues;
    Row := -1; for I := 0 to Panel.ItemList.ItemCount-1 do if Panel.VisibleIssue(I,Value) and (Value.ItemID = 'id-led') then Row := I;
    Check(Row >= 0, 'Validation rows keep immutable scene identities');
    Panel.ItemList.ItemIndex := Row; SavedKey := Panel.Issue(1).Key; Panel.NavigateSelected;
    Check((Probe.Navigations = 1) and (Probe.LastID = 'id-led') and (Probe.LastSender = Panel), 'Validation navigation emits host event with the object identity');
    Panel.SetIssues(Issues); Check(Panel.VisibleIssue(Panel.ItemList.ItemIndex,Value) and (Value.Key = SavedKey), 'Refresh preserves selected issue by stable key');
    Panel.SeverityFilter := vfWarnings; Check(Panel.ItemList.ItemCount = 2, 'Severity filter includes its heading and matching issue');
    Panel.SearchText := 'uart tx'; Check(Panel.ItemList.ItemCount = 2, 'Validation search matches multiple words across object and message');
    Panel.SearchText := 'no match'; Check(Panel.ItemList.ItemCount = 0, 'Empty validation filter hides obsolete rows');
    Panel.NavigateSelected; Check(Probe.Navigations = 1, 'Empty results cannot navigate a stale row');
    Panel.SearchText := ''; Panel.SeverityFilter := vfAll;
    Panel.OnValidate := Probe.Validate; Probe.Reject := True; Context := Default(TERDPCBValidationContext); Failed := False;
    try Panel.Validate(Context); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Panel.IssueCount = 3), 'Failed custom validation preserves the previous report');
    Probe.Reject := False; Panel.Validate(Context);
    Check((Panel.IssueCount = 1) and (Probe.LastSender = Panel), 'Panel forwards custom rules with its own sender');
    Panel.OnValidate := nil; Panel.OnNavigate := nil; Panel.SeverityFilter := vfWarnings; Panel.SearchText := 'transient';
    CopyPanel := TERDPCBValidationPanel.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Panel); Stream.Position := 0; Stream.ReadComponent(CopyPanel);
      Check((CopyPanel.SearchText = '') and (CopyPanel.IssueCount = 0) and (CopyPanel.ControlCount = 3) and
        (CopyPanel.SeverityFilter = vfWarnings), 'Validation DFM keeps display settings without messages, references or duplicate children');
    finally Stream.Free; CopyPanel.Free; end;
    Panel.Clear; Check(Panel.IssueCount = 0, 'Validation report can be cleared independently of any designer');
  finally Report.Free; Probe.Free; Form.Free; end;
end;
procedure TPCBDragProbe.Render(Bitmap: TBitmap);
begin Bitmap.SetSize(ClientWidth,ClientHeight); PaintControl(Bitmap.Canvas); end;
procedure TestPCBSelectionHandles;
var Form: TForm; Designer: TPCBDragProbe; Bitmap, Unselected: TBitmap; I, A, B, X, Y, Count: Integer;
    P, Q: TPoint; First, Second: TERDPCBDesignItem; Before: string;
begin
  Form := TForm.Create(nil); Bitmap := TBitmap.Create; Unselected := TBitmap.Create;
  try
    Designer := TPCBDragProbe.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,700,450);
    Designer.ScrollBars := System.UITypes.TScrollStyle.ssNone;
    Designer.SnapToGrid := False; Designer.SnapToObjects := False; Designer.SnapToSpacing := False;
    A := Designer.AddDesignItem(piNote,PointF(0,0)); B := Designer.AddDesignItem(piNote,PointF(60,0));
    Designer.FitBoard; Form.Show; Designer.Scene.ClearSelection; Designer.Render(Unselected);
    Designer.Scene.SelectAll; Designer.Scene.ResetHistory; Designer.Render(Bitmap);
    for I := A to B do
    begin
      First := Designer.Scene.Item(I);
      if I = A then P := Designer.BoardToClient(PointF(First.X,First.Y+First.Height))
      else P := Designer.BoardToClient(PointF(First.X+First.Width,First.Y+First.Height));
      Count := 0;
      for Y := P.Y-MulDiv(4,Designer.CurrentPPI,96) to P.Y-1 do
        for X := P.X-MulDiv(4,Designer.CurrentPPI,96) to P.X+MulDiv(4,Designer.CurrentPPI,96) do
          if Bitmap.Canvas.Pixels[X,Y] <> Unselected.Canvas.Pixels[X,Y] then Inc(Count);
      Check(Count > 0, 'Multiselection has handles on its outer group corners');
    end;
    First := Designer.Scene.Item(B); P := Designer.BoardToClient(PointF(First.X,First.Y+First.Height));
    Count := 0;
    for Y := P.Y-MulDiv(4,Designer.CurrentPPI,96) to P.Y-MulDiv(2,Designer.CurrentPPI,96) do
      for X := P.X-MulDiv(3,Designer.CurrentPPI,96) to P.X+MulDiv(3,Designer.CurrentPPI,96) do
        if Bitmap.Canvas.Pixels[X,Y] <> Unselected.Canvas.Pixels[X,Y] then Inc(Count);
    Check(Count = 0, 'Multiselection does not draw handles on individual interior component corners');
    First := Designer.Scene.Item(A); Second := Designer.Scene.Item(B); Before := Designer.Scene.ContentJSON;
    P := Designer.BoardToClient(PointF((First.X+Second.X+Second.Width)/2,First.Y+First.Height));
    Dec(P.Y,1); Q := Point(P.X+15,P.Y+10);
    Designer.StartAt(P); Designer.MoveTo(Q,True); Designer.StopAt(Q);
    Check((Designer.Scene.SelectionCount = 2) and (Designer.Scene.Item(A).Width = First.Width) and
      (Designer.Scene.Item(B).Width = Second.Width) and (Designer.Scene.Item(A).X <> First.X) and
      (Abs((Designer.Scene.Item(A).X-First.X)-(Designer.Scene.Item(B).X-Second.X)) < 0.001), 'Dragging an exterior multi-note handle moves the group without resizing');
    Designer.Undo; Check(Designer.Scene.ContentJSON = Before, 'Moving selection by its handles is one undo step');
    Designer.Scene.ClearSelection;
    A := Designer.AddDesignItem(piTimer,PointF(0,-50)); Designer.FitBoard; First := Designer.Scene.Item(A); Designer.Scene.ResetHistory;
    P := Designer.BoardToClient(PointF(First.X,First.Y)); Dec(P.X,1); Inc(P.Y,1); Q := Point(P.X+20,P.Y+12);
    Designer.StartAt(P); Designer.MoveTo(Q,True); Designer.StopAt(Q);
    Check((Designer.Scene.Item(A).Width = First.Width) and (Designer.Scene.Item(A).Height = First.Height) and
      (Designer.Scene.Item(A).X <> First.X), 'Exterior nonvisual handles move the fixed-size component and cannot resize it');
    Designer.Undo; Designer.Scene.Select(B); First := Designer.Scene.Item(B);
    P := Designer.BoardToClient(PointF(First.X+First.Width,First.Y+First.Height)); Q := Point(P.X+20,P.Y-10);
    Designer.StartAt(P); Designer.MoveTo(Q,True); Designer.StopAt(Q);
    Check(Designer.Scene.Item(B).Width > First.Width, 'Single unlocked note retains its resize handles');
    First := Designer.Scene.Item(B); First.Locked := True; Designer.Scene.UpdateItem(B,First);
    Designer.Scene.ClearSelection; Designer.Render(Unselected);
    Designer.Scene.SelectAll; Designer.Render(Bitmap);
    P := Designer.BoardToClient(PointF(First.X,First.Y+First.Height)); Count := 0;
    for Y := P.Y-MulDiv(4,Designer.CurrentPPI,96) to P.Y-MulDiv(2,Designer.CurrentPPI,96) do
      for X := P.X-MulDiv(4,Designer.CurrentPPI,96) to P.X+MulDiv(4,Designer.CurrentPPI,96) do
        if Bitmap.Canvas.Pixels[X,Y] <> Unselected.Canvas.Pixels[X,Y] then Inc(Count);
    Check(Count = 0, 'Locked objects have no exterior handles even in mixed multiselection');
    Before := Designer.Scene.ContentJSON;
    Dec(P.X,1); Dec(P.Y,1); Q := Point(P.X+20,P.Y+10);
    Designer.StartAt(P); Designer.MoveTo(Q,True); Designer.StopAt(Q);
    Check(Designer.Scene.ContentJSON = Before, 'Hidden locked handles cannot initiate a group move');
  finally Unselected.Free; Bitmap.Free; Form.Free; end;
end;
procedure TPCBDragProbe.StartAt(const P: TPoint);
begin MouseDown(mbLeft,[],P.X,P.Y); end;
procedure TPCBDragProbe.MoveTo(const P: TPoint; Alt: Boolean);
var Shift: TShiftState;
begin Shift := [ssLeft]; if Alt then Include(Shift,ssAlt); MouseMove(Shift,P.X,P.Y); end;
procedure TPCBDragProbe.StopAt(const P: TPoint);
begin MouseUp(mbLeft,[],P.X,P.Y); end;
procedure TPCBDragProbe.Escape;
var Key: Word;
begin Key := VK_ESCAPE; KeyDown(Key,[]); end;
procedure TestPCBBoardRouting;
var Geometry: TERDPCBConnectionGeometry; Board: TRect; P: TPointF; I, CaseIndex: Integer; Source: TPoint;
begin
  Board := Rect(100,100,300,200);
  for CaseIndex := 0 to 3 do
  begin
    case CaseIndex of 0: Source := Point(200,190); 1: Source := Point(110,150);
      2: Source := Point(290,150); else Source := Point(200,110); end;
    Geometry := ERDBuildPCBBoardConnection(Source,Point(50,50),nil,Board,1);
    Check(Length(Geometry.Samples) > 2, 'Connection routes around board from every terminal side');
    Check((Geometry.Samples[0].X = Source.X) and (Geometry.Samples[0].Y = Source.Y), 'Board routing preserves the real terminal endpoint');
    Check((Geometry.Samples[1].X = Source.X) or (Geometry.Samples[1].Y = Source.Y), 'Terminal leaves the PCB normally instead of diagonally crossing it');
    for I := 1 to High(Geometry.Samples)-1 do
    begin
      P := Geometry.Samples[I];
      Check(not ((P.X > Board.Left) and (P.X < Board.Right) and (P.Y > Board.Top) and (P.Y < Board.Bottom)), 'External route samples stay outside the physical board');
    end;
  end;
end;
procedure TestPCBLayoutToolbar;
var Form: TForm; Preview: TPCBPreview; Designer: TERDPCBDesigner; Ribbon: TERDRibbon; ItemButton: TERDRibbonItem; Tab: TERDRibbonTab; Group: TERDRibbonGroup;
    I, J, A, B, C, Tag: Integer; Before: string; Item: TERDPCBDesignItem; P, Q: TPoint;
begin
  Form := TForm.Create(nil);
  try
    Ribbon := TERDRibbon.Create(Form);
    Preview := TPCBPreview.Create(Form); Preview.Parent := Form; Preview.SetBounds(0,0,1200,800);
    Preview.AttachRibbon(Ribbon); Preview.SetRibbonActive(True);
    Tab := Ribbon.Tabs[Ribbon.Tabs.Count-1];
    Designer := nil;
    for I := 0 to Preview.ComponentCount-1 do if Preview.Components[I] is TERDPCBDesigner then Designer := TERDPCBDesigner(Preview.Components[I]);
    Check(Designer <> nil, 'PCB demo exposes an independently hosted designer');
    for Tag := 100 to 106 do
    begin
      ItemButton := nil;
      for I := 0 to Tab.Groups.Count-1 do for J := 0 to Tab.Groups[I].Items.Count-1 do
        if Tab.Groups[I].Items[J].Tag = Tag then ItemButton := Tab.Groups[I].Items[J];
      Check(ItemButton <> nil, 'Every PCB view option is available on the application ribbon');
      ItemButton.Execute;
      case Tag of
        100: Check(Designer.ShowBodies = ItemButton.Down, 'Ribbon synchronizes bodies visibility');
        101: Check(Designer.ShowPads = ItemButton.Down, 'Ribbon synchronizes pads visibility');
        102: Check(Designer.ShowLabels = ItemButton.Down, 'Ribbon synchronizes labels visibility');
        103: Check(Designer.ShowComponentOutlines = ItemButton.Down, 'Ribbon synchronizes outlines visibility');
        104: Check(Designer.ShowGrid = ItemButton.Down, 'Ribbon synchronizes grid visibility');
        105: Check(Designer.TemplateEditMode = ItemButton.Down, 'Ribbon synchronizes template editing');
        106: Check(Designer.PanMode = ItemButton.Down, 'Ribbon synchronizes hand mode');
      end;
      ItemButton.Execute;
    end;
    Designer.ClearBoard; A := Designer.AddDesignItem(piNote,PointF(0,0));
    B := Designer.AddDesignItem(piNote,PointF(60,30)); C := Designer.AddDesignItem(piNote,PointF(150,80)); Designer.FitBoard;
    for Tag := 20 to 27 do
    begin
      Designer.Scene.SelectAll; Designer.Scene.ResetHistory; Before := Designer.Scene.ContentJSON;
      ItemButton := nil;
      for I := 0 to Tab.Groups.Count-1 do
      begin Group := Tab.Groups[I]; for J := 0 to Group.Items.Count-1 do if Group.Items[J].Tag = Tag then ItemButton := Group.Items[J]; end;
      Check((ItemButton <> nil) and ItemButton.Enabled, 'PCB ribbon layout action is enabled for sufficient selected objects');
      ItemButton.Execute;
      Check(Designer.Scene.ContentJSON <> Before, 'Actual PCB ribbon command changes layout');
      Check(Designer.Scene.SelectionCount = 3, 'Ribbon layout preserves multiselection');
      if Tag = 20 then
      begin
        P := Designer.BoardToClient(PointF(Designer.Scene.Item(A).X,Designer.Scene.Item(A).Y));
        Q := Designer.BoardToClient(PointF(Designer.Scene.Item(C).X,Designer.Scene.Item(C).Y));
        Check(Abs(P.X-Q.X) <= 1, 'Left ribbon command aligns selected visual edges');
      end;
      Designer.Undo; Check((Designer.Scene.ContentJSON = Before) and not Designer.Scene.CanUndo, 'Ribbon layout is one atomic undo step');
    end;
    Designer.Scene.Select(A);
    for I := 0 to Tab.Groups.Count-1 do for J := 0 to Tab.Groups[I].Items.Count-1 do
      if Tab.Groups[I].Items[J].Tag in [20..27] then Check(not Tab.Groups[I].Items[J].Enabled, 'PCB ribbon disables layout with only one selected object');
    Designer.Scene.Select(B,True); Item := Designer.Scene.Item(B); Item.Locked := True; Designer.Scene.UpdateItem(B,Item);
    for I := 0 to Tab.Groups.Count-1 do for J := 0 to Tab.Groups[I].Items.Count-1 do
      if Tab.Groups[I].Items[J].Tag in [20..27] then Check(not Tab.Groups[I].Items[J].Enabled, 'Locked objects do not falsely enable ribbon layout');
    Preview.SetRibbonActive(False); Check(not Tab.Visible, 'PCB ribbon tab is hidden when leaving the PCB page');
    Preview.Free; Check(Ribbon.Tabs.Count = 0, 'Destroying PCB host removes its ribbon tab and callbacks');
  finally Form.Free; end;
end;
procedure TestPCBLayout;
var Correction: TPointF; XGuide, YGuide: TERDPCBLayoutGuide; Targets: TArray<TRectF>;
    Moving, Board: TRectF;
  procedure Solve(Align, Spacing: Boolean; HasBoard: Boolean = False);
  begin ERDPCBSnapLayout(Moving,Targets,Board,HasBoard,Align,Spacing,6,Correction,XGuide,YGuide); end;
begin
  Board := RectF(-100,-100,100,100); Moving := RectF(24,52,44,72);
  Targets := [RectF(40,0,60,20)]; Solve(True,False);
  Check((Abs(Correction.X+4) < 0.001) and (XGuide.Kind = lgAlignment) and (YGuide.Kind = lgNone), 'Layout aligns moving right edge to target left within tolerance');
  Moving := RectF(45,52,65,72); Solve(True,False);
  Check(Abs(Correction.X+5) < 0.001, 'Layout matches centers and edges deterministically');
  Moving := RectF(70,52,90,72); Solve(True,False);
  Check((XGuide.Kind = lgNone) and (Correction.X = 0), 'Objects outside magnetic tolerance do not snap');
  Moving := RectF(106,22,126,42); Targets := nil; Solve(True,False,True);
  Check((Abs(Correction.X+6) < 0.001) and (XGuide.Kind = lgAlignment), 'Board edge participates at inclusive tolerance boundary');
  Solve(False,False,True); Check((XGuide.Kind = lgNone) and (YGuide.Kind = lgNone), 'Both magnetic modes can be disabled independently of the board');
  Targets := [RectF(0,0,20,20),RectF(80,0,100,20)]; Moving := RectF(47,0,67,20); Solve(False,True);
  Check((Abs(Correction.X-3) < 0.001) and (XGuide.Kind = lgSpacing) and
    (Abs((XGuide.Gaps[0].Y-XGuide.Gaps[0].X)-(XGuide.Gaps[1].Y-XGuide.Gaps[1].X)) < 0.001), 'Layout centers an object between neighbors with equal gap indicators');
  Targets := [RectF(0,0,20,20),RectF(40,0,60,20)]; Moving := RectF(83,0,103,20); Solve(False,True);
  Check((Abs(Correction.X+3) < 0.001) and (XGuide.Kind = lgSpacing), 'Equal spacing extends a row after the last object');
  Moving := RectF(-43,0,-23,20); Solve(False,True);
  Check(Abs(Correction.X-3) < 0.001, 'Equal spacing extends a row before its first object');
  Targets := [RectF(0,0,20,20),RectF(0,40,20,60)]; Moving := RectF(0,84,20,104); Solve(False,True);
  Check((Abs(Correction.Y+4) < 0.001) and (YGuide.Kind = lgSpacing), 'Equal spacing works vertically');
  Moving := RectF(70,84,90,104); Solve(False,True);
  Check(YGuide.Kind = lgNone, 'Unrelated rows or columns cannot create spacing snaps');
  Targets := [RectF(0,0,40,20),RectF(20,0,60,20)]; Moving := RectF(83,0,103,20); Solve(False,True);
  Check(XGuide.Kind = lgNone, 'Overlapping objects do not create negative gap candidates');
  // Alignment wins an exact tie with spacing; candidates never depend on the previous snap.
  Targets := [RectF(0,0,20,20),RectF(40,0,60,20),RectF(80,100,100,120)]; Moving := RectF(83,0,103,20); Solve(True,True);
  Check((XGuide.Kind = lgAlignment) and (Abs(Correction.X+3) < 0.001), 'Alignment wins equal-distance spacing ties');
end;
procedure TestPCBMagneticDrag;
var Form: TForm; Designer: TPCBDragProbe; Index, Second, Locked: Integer; Value, Other, FixedItem: TERDPCBDesignItem;
    P, Q: TPoint; Start, Finish: TPointF; Before: string; Stream: TMemoryStream; CopyDesigner: TERDPCBDesigner;
begin
  Form := TForm.Create(nil);
  try
    Designer := TPCBDragProbe.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,600,400);
    Designer.ScrollBars := System.UITypes.TScrollStyle.ssNone;
    Designer.SnapToGrid := False; Index := Designer.AddDesignItem(piNote,PointF(0.3,0.3));
    Designer.FitBoard; Form.Show; Designer.SetFocus;
    Designer.SnapToObjects := False; Designer.SnapToSpacing := False; Designer.SnapToGrid := True;
    Designer.Scene.ResetHistory; Before := Designer.Scene.ContentJSON; Value := Designer.Scene.Item(Index);
    P := Designer.BoardToClient(PointF(Value.X+Value.Width/2,Value.Y+Value.Height/2)); Q := Point(P.X+17,P.Y+11);
    Designer.StartAt(P); Designer.MoveTo(Q); Designer.MoveTo(Point(Q.X+9,Q.Y+5)); Designer.StopAt(Point(Q.X+9,Q.Y+5));
    Value := Designer.Scene.Item(Index);
    Check((Abs(Value.X-Round(Value.X)) < 0.001) and (Abs(Value.Y-Round(Value.Y)) < 0.001), 'Dragging an off-grid object snaps its absolute anchor onto grid');
    Designer.Undo; Check((Designer.Scene.ContentJSON = Before) and not Designer.Scene.CanUndo, 'A multi-move drag remains a single undo step');
    Value := Designer.Scene.Item(Index); P := Designer.BoardToClient(PointF(Value.X+Value.Width/2,Value.Y+Value.Height/2)); Q := Point(P.X+13,P.Y+7);
    Start := Designer.ClientToBoard(P); Finish := Designer.ClientToBoard(Q);
    Designer.StartAt(P); Designer.MoveTo(Q,True); Designer.StopAt(Q);
    Check((Abs(Designer.Scene.Item(Index).X-Value.X-(Finish.X-Start.X)) < 0.001) and
      (Abs(Designer.Scene.Item(Index).Y-Value.Y-(Finish.Y-Start.Y)) < 0.001), 'Alt bypasses grid and object snapping');
    Designer.Undo; Before := Designer.Scene.ContentJSON;
    Designer.StartAt(P); Designer.MoveTo(Q); Designer.Escape;
    Check((Designer.Scene.ContentJSON = Before) and not Designer.Scene.CanUndo, 'Escape cancels snapped dragging without adding history');
    Designer.Scene.LoadJSON('{"format":"erd-pcb-scene","version":1,"items":[]}'); Designer.SnapToGrid := False;
    Index := Designer.AddDesignItem(piNote,PointF(0,0)); Second := Designer.AddDesignItem(piNote,PointF(60,0));
    Locked := Designer.AddDesignItem(piNote,PointF(120,0)); FixedItem := Designer.Scene.Item(Locked); FixedItem.Locked := True;
    Designer.Scene.UpdateItem(Locked,FixedItem); Designer.FitBoard; Designer.Scene.SelectAll; Designer.Scene.ResetHistory;
    Value := Designer.Scene.Item(Index); Other := Designer.Scene.Item(Second); Before := Designer.Scene.ContentJSON;
    P := Designer.BoardToClient(PointF(Value.X+Value.Width/2,Value.Y+Value.Height/2)); Q := Point(P.X+19,P.Y-11);
    Designer.StartAt(P); Designer.MoveTo(Q); Designer.StopAt(Q);
    Check((Abs((Designer.Scene.Item(Index).X-Value.X)-(Designer.Scene.Item(Second).X-Other.X)) < 0.001) and
      (Abs((Designer.Scene.Item(Index).Y-Value.Y)-(Designer.Scene.Item(Second).Y-Other.Y)) < 0.001) and
      (Designer.Scene.Item(Locked).X = FixedItem.X) and (Designer.Scene.Item(Locked).Y = FixedItem.Y), 'Group dragging preserves relative positions and leaves locked selected objects fixed');
    Designer.Undo; Check((Designer.Scene.ContentJSON = Before) and not Designer.Scene.CanUndo, 'Group drag is one undo step');
    Designer.SnapToObjects := False; Designer.SnapToSpacing := False; Designer.AlignmentGuides := False; Designer.SnapTolerance := 9;
    Stream := TMemoryStream.Create; CopyDesigner := TERDPCBDesigner.Create(Form);
    try
      CopyDesigner.SnapToObjects := Designer.SnapToObjects; CopyDesigner.SnapToSpacing := Designer.SnapToSpacing;
      CopyDesigner.AlignmentGuides := Designer.AlignmentGuides; CopyDesigner.SnapTolerance := Designer.SnapTolerance;
      Stream.WriteComponent(CopyDesigner); Stream.Position := 0;
      CopyDesigner.SnapToObjects := True; CopyDesigner.SnapToSpacing := True;
      CopyDesigner.AlignmentGuides := True; CopyDesigner.SnapTolerance := 6;
      Stream.ReadComponent(CopyDesigner);
      Check(not CopyDesigner.SnapToObjects and not CopyDesigner.SnapToSpacing and not CopyDesigner.AlignmentGuides and
        (CopyDesigner.SnapTolerance = 9), 'Magnetic layout settings survive DFM streaming');
    finally CopyDesigner.Free; Stream.Free; end;
  finally Form.Free; end;
end;
procedure TestPCBObjectTree;
var Form: TForm; Designer: TERDPCBDesigner; Tree, CopyTree: TERDPCBObjectTree; Stream: TMemoryStream;
    A, B, Note: TERDPCBDesignItem; Before: string; Failed: Boolean; I, Row: Integer;
begin
  Form := TForm.Create(nil);
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form;
    Tree := TERDPCBObjectTree.Create(Form); Tree.Parent := Form; Tree.Designer := Designer;
    I := Designer.Scene.Add(piTimer,PointF(1,2)); A := Designer.Scene.Item(I);
    I := Designer.Scene.Add(piCounter,PointF(3,4)); B := Designer.Scene.Item(I);
    I := Designer.Scene.Add(piNote,PointF(5,6)); Note := Designer.Scene.Item(I);
    Designer.Scene.ClearSelection; Designer.Scene.ResetHistory; Tree.Refresh;
    Check(Tree.ItemList.ItemCount = 5, 'Object tree groups components and notes separately');
    Tree.ItemList.ItemIndex := 1; Tree.Refresh; Tree.Refresh;
    Check(Tree.ItemList.ItemIndex = 1, 'Repeated tree refresh preserves keyboard focus');
    Designer.Scene.SelectIDs([A.ID,B.ID]); Tree.Refresh;
    Check((Tree.ItemList.SelCount = 2) and not Designer.Scene.CanUndo, 'Canvas multiselection mirrors into tree without history');
    Before := Designer.Scene.ToJSON; Failed := False;
    try Designer.Scene.SelectIDs([Note.ID,'missing']); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Before = Designer.Scene.ToJSON), 'Unknown selection ID rejects atomically');
    Tree.SetSelectionLocked(True);
    Check(Designer.Scene.Item(Designer.Scene.IndexOf(A.ID)).Locked and Designer.Scene.Item(Designer.Scene.IndexOf(B.ID)).Locked, 'Tree locks entire selection');
    Designer.Scene.Undo;
    Check(not Designer.Scene.Item(Designer.Scene.IndexOf(A.ID)).Locked and not Designer.Scene.Item(Designer.Scene.IndexOf(B.ID)).Locked and not Designer.Scene.CanUndo, 'Multi-lock is one undo step');
    Designer.Scene.SelectIDs([A.ID]); Tree.RenameSelected('RenamedTimer');
    Check(Designer.Scene.Item(Designer.Scene.IndexOf(A.ID)).Name = 'RenamedTimer', 'Tree renames through scene validation');
    Before := Designer.Scene.ContentJSON; Failed := False;
    try Tree.RenameSelected(B.Name); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Before = Designer.Scene.ContentJSON), 'Duplicate tree rename preserves scene');
    Designer.Scene.Undo; Check(Designer.Scene.Item(Designer.Scene.IndexOf(A.ID)).Name = A.Name, 'Tree rename can be undone');
    Tree.SearchText := B.Name; Check(Tree.ItemList.ItemCount = 2, 'Object search filters rows and empty groups');
    Row := -1;
    for I := 0 to Tree.ItemList.ItemCount-1 do if Tree.VisibleID(I) = B.ID then Row := I;
    Check(Row >= 0, 'Search resolves object by identity');
    Tree.ItemList.ItemIndex := Row;
    Check(Designer.Scene.Item(Designer.Scene.IndexOf(B.ID)).Selected and Designer.Scene.Item(Designer.Scene.IndexOf(A.ID)).Selected, 'Filtering keeps hidden selection during list edits');
    Tree.SearchText := ''; Tree.ComponentsExpanded := False;
    Check(Tree.ItemList.ItemCount = 3, 'Collapsing components keeps note rows');
    Tree.SearchText := A.Name; Check(Tree.ItemList.ItemCount = 2, 'Search reveals matching collapsed objects');
    Tree.SearchText := ''; Tree.ComponentsExpanded := True;
    Designer.Scene.SelectIDs([A.ID]); Tree.Refresh; Tree.DeleteSelection;
    Check(Designer.Scene.IndexOf(A.ID) < 0, 'Tree delete removes unlocked selected object');
    Designer.Scene.Undo; Tree.Refresh; Check(Designer.Scene.IndexOf(A.ID) >= 0, 'Tree delete can be undone');
    Tree.SetSelectionLocked(True); Tree.DeleteSelection;
    Check(Designer.Scene.IndexOf(A.ID) >= 0, 'Tree delete preserves locked objects');
    Tree.Designer := nil; Check(Tree.ItemList.ItemCount = 0, 'Detached object tree clears borrowed model');
    Stream := TMemoryStream.Create; CopyTree := TERDPCBObjectTree.Create(Form);
    try
      Tree.SearchText := 'transient'; Tree.ComponentsExpanded := False;
      Stream.WriteComponent(Tree); Stream.Position := 0; Stream.ReadComponent(CopyTree);
      Check((CopyTree.SearchText = '') and not CopyTree.ComponentsExpanded and (CopyTree.ControlCount = 2), 'Tree DFM omits search and does not duplicate internal controls');
    finally CopyTree.Free; Stream.Free; end;
    Tree.Designer := Designer; Designer.Free;
    Check((Tree.Designer = nil) and (Tree.ItemList.ItemCount = 0), 'Designer destruction detaches tree safely');
  finally Form.Free; end;
end;
procedure TestPCBComponentPalette;
var Form: TForm; Palette, CopyPalette: TERDPCBComponentPalette; Definition: TERDPCBComponentType;
    Sink: TPCBPaletteSink; Stream: TMemoryStream; DragObject: TDragObject; Initial, Before: Integer;
    Failed: Boolean; R: TRect;
begin
  ERDPCBFindComponentType('erd.timer', Definition);
  Definition.TypeID := 'smoke.palette.alpha'; Definition.DisplayName := 'Palette Alpha'; Definition.NamePrefix := 'PaletteAlpha';
  Definition.Category := 'Palette tests'; Definition.Description := 'Amber diagnostic output'; ERDPCBRegisterComponentType(Definition);
  Definition.TypeID := 'smoke.palette.beta'; Definition.DisplayName := 'Palette Beta'; Definition.NamePrefix := 'PaletteBeta';
  Definition.Description := 'Azure diagnostic output'; ERDPCBRegisterComponentType(Definition);
  Form := TForm.Create(nil); Sink := TPCBPaletteSink.Create;
  try
    Palette := TERDPCBComponentPalette.Create(Form); Palette.Parent := Form; Palette.SetBounds(0,0,280,300);
    Initial := Length(ERDPCBComponentTypes);
    Check((Palette.VisibleCount = Initial) and Palette.AllowDrag and (Palette.SelectedTypeID = ''), 'Palette automatically enumerates registered component types');
    Palette.OnChoose := Sink.Chosen; Palette.OnSelectionChange := Sink.Selection; Palette.OnCancel := Sink.Cancelled;
    Palette.OnStartDrag := Sink.Started; Palette.OnEndDrag := Sink.Ended;
    Palette.CategoryFilter := 'Palette tests'; Check(Palette.VisibleCount = 2, 'Category filter includes matching registered types');
    Palette.SelectedTypeID := 'smoke.palette.alpha'; Palette.SearchText := 'diagnostic AMBER';
    Check((Palette.VisibleCount = 1) and (Palette.VisibleType(0).TypeID = 'smoke.palette.alpha') and
      (Palette.SelectedTypeID = 'smoke.palette.alpha') and (Sink.Choices = 0), 'Search matches all words across name/description without activating an item');
    Palette.SearchText := 'azure'; Check((Palette.VisibleCount = 1) and (Palette.SelectedTypeID = ''), 'Filtering out the selected type clears its stable ID');
    Failed := False; try Palette.SelectedTypeID := 'smoke.palette.alpha'; except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Palette.SelectedTypeID = ''), 'Hidden type cannot be activated through a stale list index');
    Palette.SelectedTypeID := 'smoke.palette.beta'; Palette.AllowDrag := False; Form.Show;
    Before := Sink.Choices; R := Palette.ItemList.ItemRect(0);
    Palette.ItemList.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(R.Left+10, R.Top+10));
    Palette.ItemList.Perform(WM_LBUTTONUP, 0, MakeLParam(R.Left+10, R.Top+10));
    Check((Sink.Choices = Before+1) and (Sink.LastID = 'smoke.palette.beta'), 'Click chooses the visible type ID');
    Before := Sink.Choices; Palette.ItemList.Perform(WM_KEYDOWN, VK_RETURN, 0);
    Check(Sink.Choices = Before+1, 'Enter activates a palette item');
    Palette.ItemList.Perform(WM_KEYDOWN, VK_ESCAPE, 0);
    Check((Sink.Cancels = 1) and (Palette.SelectedTypeID = ''), 'Escape clears the tool and notifies the host');
    Palette.SearchEditor.Editor.Perform(WM_KEYDOWN, VK_RETURN, 0);
    Check((Palette.SelectedTypeID = 'smoke.palette.beta') and (Sink.LastID = 'smoke.palette.beta'), 'Search Enter chooses the first matching type');
    DragObject := nil; Palette.ItemList.OnStartDrag(Palette.ItemList, DragObject);
    Palette.ItemList.OnEndDrag(Palette.ItemList, nil, 0, 0);
    Check((Sink.Starts = 1) and (Sink.Ends = 1) and (Sink.LastID = 'smoke.palette.beta') and
      Palette.IsDragSource(Palette.ItemList) and not Palette.IsDragSource(Palette.SearchEditor), 'Internal list drag events expose the palette and preserve selected ID');
    Palette.Enabled := False; Before := Sink.Choices; Palette.ChooseSelected;
    Check(Sink.Choices = Before, 'Disabled palette cannot activate a type'); Palette.Enabled := True;
    Palette.SearchText := 'no-such-component-9f0d'; Before := Sink.Choices; Palette.ChooseSelected;
    Check((Palette.VisibleCount = 0) and (Palette.SelectedTypeID = '') and (Sink.Choices = Before), 'Empty results cannot activate a stale item');
    Palette.SearchText := ''; Palette.CategoryFilter := ''; Palette.ItemList.ScrollTo(MaxInt);
    Check(Palette.ItemList.ScrollPos > 0, 'Virtual list scrolls when registered types exceed its viewport');
    Palette.CategoryFilter := 'Palette tests'; Palette.SelectedTypeID := 'smoke.palette.alpha';
    ERDPCBFindComponentType('smoke.palette.beta', Definition); Definition.TypeID := 'smoke.palette.gamma'; Definition.DisplayName := 'Palette Gamma'; Definition.NamePrefix := 'PaletteGamma';
    ERDPCBRegisterComponentType(Definition); Before := Sink.Changes; Palette.ReloadTypes;
    Check((Sink.Changes = Before) and (Palette.VisibleCount = 3) and (Palette.SelectedTypeID = 'smoke.palette.alpha'), 'Reload picks up newly registered types while preserving filter and selection');
    Palette.SearchText := 'amber'; Check(Palette.ItemList.ScrollPos = 0, 'Filtering resets an out-of-range scroll offset');
    Sink.Designer := TERDPCBDesigner.Create(Form); Sink.Designer.Parent := Form; Sink.Designer.SetBounds(300,0,500,400);
    Palette.ChooseSelected; Check(Sink.Designer.Placing and (Sink.Designer.PlacementTypeID = 'smoke.palette.alpha'), 'Host connects palette choice to placement independently');
    Sink.Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(100,100)); Sink.Designer.Perform(WM_LBUTTONUP, 0, MakeLParam(100,100));
    Check((Sink.Designer.Scene.Count = 1) and (Sink.Designer.Scene.Item(0).TypeID = 'smoke.palette.alpha'), 'Canvas click inserts the selected registered type');
    Palette.OnChoose := nil; Palette.OnSelectionChange := nil; Palette.OnCancel := nil; Palette.OnStartDrag := nil; Palette.OnEndDrag := nil;
    Stream := TMemoryStream.Create;
    try
      RegisterClass(TERDPCBComponentPalette); Stream.WriteComponent(Palette); Stream.Position := 0;
      CopyPalette := TERDPCBComponentPalette(Stream.ReadComponent(nil));
      try
        Check((CopyPalette.SearchText = '') and (CopyPalette.CategoryFilter = '') and (CopyPalette.SelectedTypeID = '') and
          not CopyPalette.AllowDrag and (CopyPalette.VisibleCount = Initial+1) and (CopyPalette.ControlCount = 3), 'DFM streams configuration without transient search/category/placement state');
      finally CopyPalette.Free; end;
    finally Stream.Free; end;
  finally Form.Free; Sink.Free; end;
end;

procedure TestPCBScene;
var Scene: TERDPCBScene; A, B, C: Integer; Value: TERDPCBDesignItem; Before: string; Failed: Boolean;
begin
  Scene := TERDPCBScene.Create(nil);
  try
    A := Scene.Add(piTimer, PointF(1,2)); B := Scene.Add(piCounter, PointF(4,5));
    Check((Scene.Item(A).Name = 'Timer1') and (Scene.Item(A).Value = 1000), 'Designer item defaults are typed');
    Scene.Select(A); Scene.Select(B, True); Scene.ResetHistory; Before := Scene.ToJSON;
    Scene.BeginEdit; Scene.MoveSelected(2,3); Value := Scene.Item(A); Value.Width := 20; Scene.UpdateItem(A, Value); Scene.EndEdit;
    Check(Scene.CanUndo and (Scene.Item(A).X = 3) and (Scene.Item(B).Y = 8), 'Grouped edit moves all selected objects');
    Scene.Undo; Check(Scene.ToJSON = Before, 'Undo restores a complete designer transaction');
    Scene.Redo; Check((Scene.Item(A).Width = 20) and not Scene.CanRedo, 'Redo restores resize and position');
    Before := Scene.ToJSON; Scene.BeginEdit; Scene.MoveSelected(5,5); Scene.CancelEdit;
    Check(Scene.ToJSON = Before, 'Cancelling a drag restores the entire scene');
    Value := Scene.Item(A); Value.Locked := True; Scene.UpdateItem(A, Value); Scene.MoveSelected(1,1);
    Check((Scene.Item(A).X = 3) and (Scene.Item(B).X = 7), 'Locked objects do not move with a selection');
    Value := Scene.Item(B); Value.Name := Scene.Item(A).Name; Before := Scene.ToJSON; Failed := False;
    try Scene.UpdateItem(B, Value); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ToJSON = Before), 'Duplicate component names are rejected without mutation');
    Scene.Select(B); Scene.PasteJSON(Scene.ToJSON(True)); C := Scene.Count - 1;
    Check((Scene.Item(C).ID <> Scene.Item(B).ID) and (Scene.Item(C).Name <> Scene.Item(B).Name), 'Paste creates fresh stable identities and names');
    Scene.DeleteSelected; Scene.Undo; Check(Scene.Count = 3, 'Deleted designer objects can be restored');
    Before := Scene.ToJSON; Failed := False;
    try Scene.LoadJSON(StringReplace(Before, '"width":20', '"width":-20', [])); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ToJSON = Before), 'Invalid scene loads are atomic');
    C := Scene.Add(piNote, PointF(10,10)); Value := Scene.Item(C);
    Value.Caption := 'First line' + sLineBreak + 'Second line'; Value.Width := 40; Value.Height := 25;
    Scene.UpdateItem(C, Value); Before := Scene.ToJSON; Scene.LoadJSON(Before);
    Check((Scene.Item(C).Caption = Value.Caption) and (Scene.Item(C).Width = 40),
      'Notes preserve multiline text and resizable bounds');
    Value.Pin := 'U7.2'; Failed := False;
    try Scene.UpdateItem(C, Value); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ToJSON = Before), 'Notes reject pin assignments without mutation');
    Before := Scene.ContentJSON; Scene.ClearSelection;
    Check(Scene.ContentJSON = Before, 'Selection alone does not change the host content snapshot');
    Scene.LoadJSON(Before);
    Check((Scene.SelectionCount = 0) and (Scene.ContentJSON = Before), 'Content snapshots load without restoring transient selection');
  finally Scene.Free; end;
end;

procedure TPCBHostProbe.ValidateItem(Sender: TObject; const Item: TERDPCBDesignItem);
begin
  Inc(Validations);
  if Item.Caption = 'Reject' then raise EArgumentException.Create('Host rejected this caption');
end;

procedure TPCBHostProbe.RejectCheck(Sender: TObject; Row: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
begin Inc(Validations); Accept := False; end;
procedure ValidatePCBTestText(const Value: string);
begin if Value = 'blocked' then raise EArgumentException.Create('This description is reserved'); end;
procedure TestPCBPropertySchema;
var Definition, CopyDefinition: TERDPCBComponentType; Props: TArray<TERDPCBPropertyDefinition>;
    Scene, CopyScene: TERDPCBScene; Binding: TERDPCBInspectorBinding; Form: TForm; Inspector: TERDInspector;
    Index: Integer; Item: TERDPCBDesignItem; Before, ID: string; Failed: Boolean; Root: TJSONObject;
    TextRow, CountRow, FlagRow, ChoiceRow: TERDInspectorProperty; Host: TPCBHostProbe;
  procedure Reject(const Name, Value: string);
  var Snapshot: string; Rejected: Boolean;
  begin
    Snapshot := Scene.ContentJSON; Rejected := False;
    try Scene.SetProperty(Index, Name, Value); except on E: EArgumentException do Rejected := True; end;
    Check(Rejected and (Scene.ContentJSON = Snapshot), 'Invalid property edit is atomic: ' + Name);
  end;
begin
  ERDPCBFindComponentType('erd.timer', Definition); Definition.TypeID := 'smoke.schema';
  Definition.DisplayName := 'Schema timer'; Definition.NamePrefix := 'SchemaTimer'; Definition.PropertiesJSON := '{"opaque":{"key":1}}';
  SetLength(Props, 6);
  Props[0].Name := 'description'; Props[0].Caption := 'Description'; Props[0].Category := 'General';
  Props[0].DefaultValue := 'Ready'; Props[0].Required := True; Props[0].Validate := ValidatePCBTestText;
  Props[1].Name := 'count'; Props[1].Caption := 'Count'; Props[1].Category := 'Settings'; Props[1].Kind := pkInteger;
  Props[1].DefaultValue := '3'; Props[1].HasMinimum := True; Props[1].Minimum := 0; Props[1].HasMaximum := True; Props[1].Maximum := 10;
  Props[2].Name := 'gain'; Props[2].Caption := 'Gain'; Props[2].Category := 'Settings'; Props[2].Kind := pkFloat;
  Props[2].DefaultValue := '1.5'; Props[2].HasMinimum := True; Props[2].Minimum := 0; Props[2].HasMaximum := True; Props[2].Maximum := 5;
  Props[3].Name := 'active'; Props[3].Caption := 'Active'; Props[3].Category := 'Settings'; Props[3].Kind := pkBoolean; Props[3].DefaultValue := 'True';
  Props[4].Name := 'mode'; Props[4].Caption := 'Mode'; Props[4].Category := 'Settings'; Props[4].Kind := pkChoice; Props[4].DefaultValue := 'Off';
  SetLength(Props[4].Choices, 2); Props[4].Choices[0] := 'Off'; Props[4].Choices[1] := 'On';
  Props[5].Name := 'token'; Props[5].Caption := 'Token'; Props[5].Category := 'Settings'; Props[5].DefaultValue := 'Fixed'; Props[5].ReadOnly := True;
  Definition.Properties := Props; ERDPCBRegisterComponentType(Definition);
  Props[4].Choices[0] := 'Changed'; ERDPCBFindComponentType(Definition.TypeID, CopyDefinition);
  Check(CopyDefinition.Properties[4].Choices[0] = 'Off', 'Registry owns independent copies of schema choices');
  CopyDefinition.Properties[4].Choices[0] := 'Changed again'; ERDPCBFindComponentType(Definition.TypeID, CopyDefinition);
  Check(CopyDefinition.Properties[4].Choices[0] = 'Off', 'Returned schemas cannot mutate registered definitions');
  Scene := TERDPCBScene.Create(nil); CopyScene := TERDPCBScene.Create(nil); Binding := TERDPCBInspectorBinding.Create; Form := TForm.Create(nil); Host := TPCBHostProbe.Create;
  try
    Scene.Add(piCounter, PointF(0,0)); Index := Scene.AddType(Definition.TypeID, PointF(1,2)); ID := Scene.Item(Index).ID;
    Check(ERDPCBPropertyValue(Scene.Item(Index), 'active') = 'True', 'Boolean defaults are canonical inspector values');
    Scene.ResetHistory; Scene.SetProperty(Index, 'active', 'true'); Check(not Scene.CanUndo, 'Equivalent values do not create empty undo operations');
    Reject('count', '1.5'); Reject('count', '11'); Reject('gain', 'NaN'); Reject('gain', '6');
    Reject('active', 'yes'); Reject('mode', 'Unknown'); Reject('description', ''); Reject('description', 'blocked'); Reject('token', 'Changed');
    Scene.SetProperty(Index, 'count', '7'); Scene.SetProperty(Index, 'gain', '2.25'); Scene.SetProperty(Index, 'active', 'False'); Scene.SetProperty(Index, 'mode', 'On');
    Root := TJSONObject.ParseJSONValue(Scene.Item(Index).PropertiesJSON) as TJSONObject;
    try
      Check((Root.GetValue('count') is TJSONNumber) and (Root.GetValue('active') is TJSONBool) and
        (Root.GetValue('opaque') is TJSONObject), 'Edits keep numeric/boolean JSON types and unknown metadata');
    finally Root.Free; end;
    Before := Scene.ContentJSON; Scene.ResetProperties(Index);
    Check((ERDPCBPropertyValue(Scene.Item(Index), 'count') = '3') and (ERDPCBPropertyValue(Scene.Item(Index), 'mode') = 'Off'), 'Reset restores all editable schema defaults');
    Scene.Undo; Check(Scene.ContentJSON = Before, 'Reset all is one undo operation'); Scene.Redo;
    Scene.ResetHistory; Scene.ResetProperties(Index); Check(not Scene.CanUndo, 'Resetting unchanged defaults is a no-op');
    CopyScene.LoadJSON(Scene.ToJSON); Check(CopyScene.ContentJSON = Scene.ContentJSON, 'Typed properties survive save and load');
    Before := Scene.ContentJSON; Failed := False;
    try Scene.LoadJSON(StringReplace(Scene.ToJSON, '"count":3', '"count":"wrong"', [])); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ContentJSON = Before), 'Invalid persisted property types are rejected atomically');
    Item := Scene.Item(Index); Item.PropertiesJSON := '{}'; Check(ERDPCBPropertyValue(Item, 'count') = '3', 'Missing properties use registered defaults');
    Inspector := TERDInspector.Create(Form); Inspector.Parent := Form;
    Binding.Bind(Scene, ID, Inspector);
    Check((Inspector.Categories.Count = 2) and (Inspector.Categories[1].Caption = 'Settings'), 'Binding creates property categories from metadata');
    TextRow := Inspector.Categories[0].Properties[0]; CountRow := Inspector.Categories[1].Properties[0];
    FlagRow := Inspector.Categories[1].Properties[2]; ChoiceRow := Inspector.Categories[1].Properties[3];
    Check((FlagRow.Kind = ivCheck) and FlagRow.Checked and (ChoiceRow.Kind = ivPickList) and
      (ChoiceRow.PickList.Count = 2) and not FlagRow.Modified, 'Binding chooses checkbox/list editors and tracks defaults');
    Failed := False; try Binding.Validate(TextRow, 'blocked'); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (TextRow.Level = alvAlarm) and (TextRow.Hint <> ''), 'Binding exposes validation failures on the row');
    Binding.Validate(TextRow, 'Accepted'); Check(TextRow.Level = alvNormal, 'Valid input clears a previous validation error');
    Binding.Validate(CountRow, '8'); CountRow.Value := '8'; Binding.Apply(CountRow);
    Check(CountRow.Modified and (ERDPCBPropertyValue(Scene.Item(Index), 'count') = '8'), 'Inspector edits update the scene');
    Scene.Undo; Binding.Refresh; Check(CountRow.Value = '3', 'Undo refresh restores displayed values');
    Scene.Redo; Binding.Refresh; Binding.ResetProperty(CountRow); Check(CountRow.Value = '3', 'Selected property reset updates the inspector');
    Scene.Select(0); Scene.DeleteSelected; Index := Scene.IndexOf(ID);
    CountRow.Value := '9'; Binding.Apply(CountRow);
    Check((Index = 0) and (ERDPCBPropertyValue(Scene.Item(Index), 'count') = '9'), 'Binding resolves stable IDs after indices shift');
    Item := Scene.Item(Index); Item.Locked := True; Scene.UpdateItem(Index, Item); Binding.Refresh;
    Check(CountRow.ReadOnly, 'Locked component properties become read-only'); Reject('count', '2');
    Item.Locked := False; Scene.UpdateItem(Index, Item); Binding.Refresh;
    Scene.Select(Index); Scene.DeleteSelected; Failed := False;
    try Binding.Apply(CountRow); except on E: EArgumentException do Failed := True; end;
    Check(Failed, 'A deleted inspected object cannot edit a different object'); Binding.Clear;
    CopyDefinition := Definition; CopyDefinition.TypeID := 'smoke.invalid-schema';
    SetLength(CopyDefinition.Properties, 2); CopyDefinition.Properties[1] := CopyDefinition.Properties[0]; Failed := False;
    try ERDPCBRegisterComponentType(CopyDefinition); except on E: EArgumentException do Failed := True; end;
    Check(Failed, 'Duplicate property names are rejected during registration');
    Inspector.Clear; Inspector.SetBounds(0,0,300,200);
    FlagRow := Inspector.AddCategory('Checks').AddProperty('Flag', 'True', ivCheck);
    Inspector.OnPropertyChanging := Host.RejectCheck; Form.Show; Inspector.SetFocus;
    Inspector.Perform(WM_KEYDOWN, VK_HOME, 0); Inspector.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Check(Inspector.Selected = FlagRow, 'Keyboard selects the checkbox row');
    Inspector.Perform(WM_KEYDOWN, VK_SPACE, 0);
    Check((Host.Validations = 1) and FlagRow.Checked, 'Checkbox validation runs before the displayed value changes');
    Inspector.OnPropertyChanging := nil;
  finally Binding.Free; Form.Free; Host.Free; CopyScene.Free; Scene.Free; end;
end;

procedure TPCBGroupProbe.ChangingProperty(Sender: TObject; Row: TERDInspectorProperty; var NewValue: string; var Accept: Boolean);
begin
  try Binding.Validate(Row, NewValue); except on E: EArgumentException do Accept := False; end;
end;
procedure TPCBGroupProbe.ChangedProperty(Sender: TObject; Row: TERDInspectorProperty);
begin Binding.Apply(Row); end;
procedure TPCBGroupProbe.Changed(Sender: TObject);
begin Inc(Changes); end;
procedure TPCBGroupProbe.Validate(Sender: TObject; const Item: TERDPCBDesignItem);
begin
  if (Item.ID = RejectID) and (Item.Caption = 'reject') then raise EArgumentException.Create('Second component rejected the batch');
end;
procedure TestPCBGroupInspector;
var Scene: TERDPCBScene; Form: TForm; Inspector: TERDInspector; Binding: TERDPCBInspectorBinding; Probe: TPCBGroupProbe;
    A, B: Integer; Definition: TERDPCBComponentType; Item: TERDPCBDesignItem; Row, Flag: TERDInspectorProperty;
    Before: string; Failed: Boolean; Updates: TArray<TERDPCBDesignItem>; Editor: TERDTextInput; I: Integer;
  procedure Rebind;
  begin Binding.Clear; Inspector.Clear; Binding.BindSelection(Scene, Inspector); end;
  procedure Reject(Row: TERDInspectorProperty; const Text: string);
  var Snapshot: string; Rejected: Boolean;
  begin
    Snapshot := Scene.ContentJSON; Rejected := False;
    try Binding.Validate(Row, Text); except on E: EArgumentException do Rejected := True; end;
    Check(Rejected and (Scene.ContentJSON = Snapshot), 'Group validation rejects all targets atomically');
  end;
begin
  ERDPCBFindComponentType('smoke.schema', Definition); Definition.TypeID := 'smoke.group'; Definition.NamePrefix := 'GroupTimer';
  Definition.Properties[1].Maximum := 5; Definition.Properties[1].DefaultValue := '4';
  Definition.Properties[4].Choices[0] := 'On'; Definition.Properties[4].Choices[1] := 'Other'; Definition.Properties[4].DefaultValue := 'On';
  Definition.Properties[0].Name := 'other_description';
  ERDPCBRegisterComponentType(Definition);
  Scene := TERDPCBScene.Create(nil); Form := TForm.Create(nil); Binding := TERDPCBInspectorBinding.Create; Probe := TPCBGroupProbe.Create;
  try
    Inspector := TERDInspector.Create(Form); Inspector.Parent := Form; Inspector.SetBounds(0,0,350,500);
    A := Scene.AddType('smoke.schema', PointF(0,0)); B := Scene.AddType('smoke.group', PointF(5,5));
    Scene.Select(A); Scene.Select(B, True); Rebind;
    Check(Inspector.FindProperty('Caption') = nil, 'Nonvisual group has no unused Caption editor');
    Row := Inspector.FindProperty('Count'); Flag := Inspector.FindProperty('Active');
    Check((Row <> nil) and Row.Mixed and not Row.HasDefault, 'Different values/defaults are shown as mixed');
    Check(Inspector.FindProperty('Description') = nil, 'Properties missing from one selected type are omitted');
    Check((Inspector.FindProperty('Mode').PickList.Count = 1) and
      (Inspector.FindProperty('Mode').PickList[0] = 'On'), 'Choice editors intersect options from all selected types');
    Reject(Row, '8'); Before := Scene.ContentJSON; Scene.ResetHistory;
    Binding.Validate(Row, '5'); Row.Value := '5'; Binding.Apply(Row);
    Check((ERDPCBPropertyValue(Scene.Item(A), 'count') = '5') and (ERDPCBPropertyValue(Scene.Item(B), 'count') = '5') and not Row.Mixed,
      'Applying one property updates all selected objects');
    Scene.Undo; Binding.Refresh; Check((Scene.ContentJSON = Before) and Row.Mixed and not Scene.CanUndo, 'A group property edit has exactly one undo step');
    Scene.Redo; Binding.Refresh; Binding.ResetProperty(Row);
    Check((ERDPCBPropertyValue(Scene.Item(A), 'count') = '3') and (ERDPCBPropertyValue(Scene.Item(B), 'count') = '4') and Row.Mixed,
      'Reset uses each selected type own default');
    Binding.Validate(Row, '3'); Row.Value := '3'; Binding.Apply(Row);
    Check(not Row.Mixed and (ERDPCBPropertyValue(Scene.Item(B), 'count') = '3'), 'Entering the representative value still updates the other targets');
    Binding.ResetProperty(Row);
    Scene.ResetHistory; Binding.ResetAll; Check(not Scene.CanUndo, 'Resetting unchanged group defaults adds no undo step');
    Scene.SetProperty(A, 'active', 'False'); Binding.Refresh;
    Check(Flag.Mixed, 'Mixed booleans retain their indeterminate state');
    Binding.Validate(Flag, 'True'); Flag.Value := 'True'; Binding.Apply(Flag);
    Check(not Flag.Mixed and Flag.Checked and (ERDPCBPropertyValue(Scene.Item(A), 'active') = 'True'), 'One boolean edit resolves the whole group');
    Item := Scene.Item(B); Item.Locked := True; Scene.UpdateItem(B, Item); Binding.Refresh;
    Check(Row.ReadOnly, 'Any locked target makes a group row read-only'); Reject(Row, '2');
    Flag := Inspector.FindProperty('Locked');
    Check((Flag <> nil) and Flag.Mixed and not Flag.ReadOnly, 'Mixed lock state remains editable');
    Before := Scene.ContentJSON; Scene.ResetHistory;
    Binding.Validate(Flag,'False'); Flag.Value := 'False'; Binding.Apply(Flag);
    Check(not Scene.Item(A).Locked and not Scene.Item(B).Locked, 'One group checkbox unlocks every selected component');
    Scene.Undo; Check((Scene.ContentJSON = Before) and not Scene.CanUndo, 'Group unlock is one undo step');
    Binding.Refresh; Flag.Value := 'True'; Binding.Apply(Flag);
    Check(Scene.Item(A).Locked and Scene.Item(B).Locked, 'One group checkbox locks every selected component');
    Flag.Value := 'False'; Binding.Apply(Flag);
    Item.Locked := False; Scene.UpdateItem(B, Item); Binding.Refresh;
    Before := Scene.ContentJSON; Scene.ResetHistory; Probe.Changes := 0; Probe.RejectID := Scene.Item(B).ID;
    Scene.OnChange := Probe.Changed; Scene.OnValidateItem := Probe.Validate;
    SetLength(Updates, 2); Updates[0] := Scene.Item(A); Updates[1] := Scene.Item(B);
    Updates[0].Caption := 'accepted'; Updates[1].Caption := 'reject'; Failed := False;
    try Scene.UpdateItems(Updates); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ContentJSON = Before) and (Probe.Changes = 0) and not Scene.CanUndo,
      'Failure on the second target changes neither content, history nor events');
    Updates[1].Caption := 'accepted'; Scene.UpdateItems(Updates);
    Check(Probe.Changes = 1, 'A valid batch emits one content change'); Scene.Undo;
    Check(Scene.ContentJSON = Before, 'Batch undo restores all targets'); Scene.OnChange := nil; Scene.OnValidateItem := nil;
    Scene.Select(B); Scene.DeleteSelected; Before := Scene.ContentJSON; Failed := False; Row.Value := '1';
    try Binding.Apply(Row); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ContentJSON = Before), 'A stale group target cannot partially edit remaining objects');
    Binding.Clear; Inspector.Clear; Scene.ClearSelection;
    A := Scene.AddType('erd.note.info', PointF(0,0)); B := Scene.AddType('erd.note.warning', PointF(5,5));
    Scene.Select(A); Scene.Select(B, True); Rebind; Row := Inspector.FindProperty('Type');
    Check((Row <> nil) and Row.Mixed and (Inspector.FindProperty('Enabled') = nil), 'Notes share style but have no Enabled editor');
    Binding.Validate(Row, 'Error'); Row.Value := 'Error'; Binding.Apply(Row);
    Check((Scene.Item(A).NoteType = ntError) and (Scene.Item(B).NoteType = ntError), 'Note style can be changed for a group');
    // Focusing an untouched mixed text editor must never write the placeholder.
    Item := Scene.Item(B); Item.Caption := 'Different'; Scene.UpdateItem(B, Item); Binding.Refresh;
    Row := Inspector.FindProperty('Text'); Check((Row <> nil) and Row.Mixed, 'Different note text is mixed');
    Probe.Binding := Binding; Inspector.OnPropertyChanging := Probe.ChangingProperty; Inspector.OnPropertyChanged := Probe.ChangedProperty;
    Form.Show; Inspector.SetFocus; Inspector.Perform(WM_KEYDOWN, VK_HOME, 0); Inspector.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Before := Scene.ContentJSON; Editor := nil;
    for I := 0 to Inspector.ControlCount-1 do if Inspector.Controls[I] is TERDTextInput then Editor := TERDTextInput(Inspector.Controls[I]);
    Check((Editor <> nil) and (Editor.Text = '') and (Editor.TextHint = 'Meerdere waarden'), 'Mixed editor displays a hint rather than representative text');
    Inspector.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Check((Scene.ContentJSON = Before) and Row.Mixed, 'Leaving an untouched mixed editor preserves every value');
    Inspector.Perform(WM_KEYDOWN, VK_HOME, 0); Inspector.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Editor.Text := 'temporary'; Editor.Text := ''; Inspector.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Check((Scene.Item(A).Caption = '') and (Scene.Item(B).Caption = '') and not Row.Mixed, 'Explicitly entered empty text applies to the whole group');
    Inspector.OnPropertyChanging := nil; Inspector.OnPropertyChanged := nil;
  finally Form.Free; Binding.Free; Probe.Free; Scene.Free; end;
end;

procedure TestPCBComponentTypes;
var Scene, CopyScene: TERDPCBScene; Definition, CopyDefinition: TERDPCBComponentType;
    Value: TERDPCBDesignItem; Index, BeforeCount: Integer; Failed: Boolean; NoteType: TERDPCBNoteType;
    Before, Legacy: string; Root, Obj: TJSONObject; Items: TJSONArray; Removed: TJSONPair;
    Form: TForm; Designer: TERDPCBDesigner; P: TPoint; Host: TPCBHostProbe;
begin
  Check(ERDPCBFindComponentType('erd.timer', Definition), 'Built-in types have stable registry IDs');
  Definition.TypeID := 'smoke.customtimer'; Definition.DisplayName := 'Custom timer'; Definition.NamePrefix := 'CustomTimer';
  Definition.Value := 250; Definition.Caption := 'Pulse'; Definition.PropertiesJSON := '{"repeat":3,"label":"test"}';
  ERDPCBRegisterComponentType(Definition); BeforeCount := Length(ERDPCBComponentTypes); Failed := False;
  try ERDPCBRegisterComponentType(Definition); except on E: EArgumentException do Failed := True; end;
  Check(Failed and (Length(ERDPCBComponentTypes) = BeforeCount), 'Duplicate type IDs are rejected without replacing definitions');
  Check(ERDPCBFindComponentType(Definition.TypeID, CopyDefinition), 'Registered definitions can be queried');
  CopyDefinition.Caption := 'Changed';
  ERDPCBFindComponentType(Definition.TypeID, CopyDefinition);
  Check(CopyDefinition.Caption = 'Pulse', 'Registry returns independent descriptor records');
  CopyDefinition.TypeID := 'smoke.invalid'; CopyDefinition.PropertiesJSON := '[]'; Failed := False;
  try ERDPCBRegisterComponentType(CopyDefinition); except on E: EArgumentException do Failed := True; end;
  Check(Failed and (Length(ERDPCBComponentTypes) = BeforeCount), 'Invalid default properties cannot enter the registry');
  Scene := TERDPCBScene.Create(nil); CopyScene := TERDPCBScene.Create(nil);
  try
    Index := Scene.AddType(Definition.TypeID, PointF(1,2)); Value := Scene.Item(Index);
    Check((Value.TypeID = Definition.TypeID) and (Value.Name = 'CustomTimer1') and (Value.Value = 250) and
      (Value.PropertiesJSON = Definition.PropertiesJSON), 'Registered types supply names, values and custom property defaults');
    CopyScene.LoadJSON(Scene.ToJSON);
    Check(CopyScene.ContentJSON = Scene.ContentJSON, 'Registered identity and properties round-trip');
    Scene.Select(Index); Scene.PasteJSON(Scene.ToJSON(True));
    Check((Scene.Item(1).TypeID = Definition.TypeID) and (Scene.Item(1).Name = 'CustomTimer2') and
      (Scene.Item(1).PropertiesJSON = Value.PropertiesJSON), 'Paste retains type and properties with a fresh unique name');
    Index := Scene.Add(piNote, PointF(3,4));
    for NoteType := Low(TERDPCBNoteType) to High(TERDPCBNoteType) do
    begin
      Value := Scene.Item(Index); Value.NoteType := NoteType; Scene.UpdateItem(Index, Value);
      CopyScene.LoadJSON(Scene.ToJSON);
      Check(CopyScene.Item(Index).NoteType = NoteType, 'Every note type survives serialization');
    end;
    Scene.Undo; Check(Scene.Item(Index).NoteType = ntError, 'Undo restores the previous note type');
    Scene.Redo; Check(Scene.Item(Index).NoteType = ntQuestion, 'Redo restores the new note type');
    Scene.Select(Index); Scene.PasteJSON(Scene.ToJSON(True));
    Check(Scene.Item(Scene.Count-1).NoteType = ntQuestion, 'Copied notes retain their type');
    Before := Scene.ContentJSON; Failed := False;
    try Scene.LoadJSON(StringReplace(Before, '"note_type":"Question"', '"note_type":"invalid"', []));
    except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ContentJSON = Before), 'Invalid note types fail atomically');
    Root := TJSONObject.ParseJSONValue(Scene.ToJSON) as TJSONObject;
    try
      Items := Root.GetValue<TJSONArray>('items');
      for Index := 0 to Items.Count-1 do
      begin
        Obj := TJSONObject(Items.Items[Index]); Removed := Obj.RemovePair('type_id'); Removed.Free;
        Removed := Obj.RemovePair('note_type'); Removed.Free; Removed := Obj.RemovePair('properties'); Removed.Free;
      end;
      Legacy := Root.ToJSON;
    finally Root.Free; end;
    CopyScene.LoadJSON(Legacy);
    Check((CopyScene.Item(0).TypeID = 'erd.timer') and (CopyScene.Item(2).NoteType = ntNone), 'Legacy scenes load with built-in identities and neutral notes');
    Before := Scene.ContentJSON; Failed := False;
    try Scene.AddType('missing.component', PointF(0,0)); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Scene.ContentJSON = Before), 'Unknown types cannot be inserted');
    CopyScene.LoadJSON(StringReplace(Scene.ToJSON, 'smoke.customtimer', 'missing.component', [rfReplaceAll]));
    Check(CopyScene.Item(0).TypeID = 'missing.component', 'Unregistered saved types retain metadata and base behavior');
  finally CopyScene.Free; Scene.Free; end;
  Form := TForm.Create(nil); Host := TPCBHostProbe.Create;
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,600,400);
    Designer.OnValidateDesignItem := Host.ValidateItem;
    Designer.SnapToGrid := False; Designer.FitBoard;
    Designer.BeginTypePlacement('erd.note.warning');
    Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(120,120));
    Designer.Perform(WM_LBUTTONUP, 0, MakeLParam(120,120));
    Value := Designer.Scene.Item(Designer.Scene.PrimaryIndex);
    P := Designer.BoardToClient(PointF(Value.X, Value.Y + Value.Height));
    Check((Value.NoteType = ntWarning) and (Abs(P.X-120) <= 1) and (Abs(P.Y-120) <= 1), 'Registered note placement preserves type and top-left anchoring');
    Before := Designer.Scene.ContentJSON; Value.Caption := 'Reject'; Failed := False;
    try Designer.Scene.UpdateItem(Designer.Scene.PrimaryIndex, Value); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Host.Validations > 0) and (Designer.Scene.ContentJSON = Before), 'Host validation rejects edits without replacing internal validation');
    Designer.Undo; Check(Designer.Scene.Count = 0, 'Registered placement is a single undo step');
  finally Form.Free; Host.Free; end;
end;

procedure TestDebugEditor;
var Form: TForm; Editor, Other: TCodeEditorProbe; Model: TERDDebugModel;
    Item: TERDDebugBreakpoint; ID, Revision, Session: Int64; Before: string; Baseline: TColor; Probe: TDebugCommandProbe;
begin
  Form := TForm.Create(nil); Probe := TDebugCommandProbe.Create;
  try
    Model := TERDDebugModel.Create(Form); Model.OnCommand := Probe.Execute;
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.SetBounds(0,0,500,250);
    Editor.DocumentUri := 'file:///test.ts'; Editor.Text := 'const a = 1;'+sLineBreak+'const b = 2;'+sLineBreak+'console.log(b);';
    Editor.DebugModel := Model; Editor.ShowDebugGutter := True; Editor.ShowMinimap := False;
    Other := TCodeEditorProbe.Create(Form); Other.Parent := Form; Other.SetBounds(0,260,500,200);
    Other.Text := Editor.Text; Other.DocumentUri := Editor.DocumentUri; Other.DebugModel := Model; Other.ShowDebugGutter := True;
    Form.Show; Editor.SetFocus;
    Before := Editor.Text; Editor.DebugClick(1);
    Check(Model.BreakpointAt(Editor.DocumentUri,2,Item) and Item.Enabled and (Item.Status = bpsPending), 'Gutter click creates a pending enabled breakpoint');
    Check(Editor.Text = Before, 'Breakpoint gutter does not edit or select document text');
    ID := Item.ID; Revision := Item.Revision; Session := Model.Session;
    Baseline := Editor.DebugPixel(1,True);
    Check(Model.PublishBreakpoint(Session,ID,Revision,bpsVerified,2,''), 'Matching debugger acknowledgement is accepted');
    Check((Editor.DebugPixel(1,True) = Baseline) and (Other.DebugPixel(1,True) = Baseline), 'Pending and verified breakpoints are solid in both editors');
    Model.SetExecution(dsPaused,Editor.DocumentUri,2); Baseline := Editor.DebugPixel(1,True);
    Other.DebugClick(1,True); Check(Model.BreakpointAt(Editor.DocumentUri,2,Item) and not Item.Enabled, 'Shift-click disables shared breakpoint');
    Check(Editor.DebugPixel(1,True) = Baseline, 'Execution marker takes precedence over breakpoint appearance');
    Model.SetExecution(dsRunning);
    Check(not Model.PublishBreakpoint(Session,ID,Revision,bpsVerified,2,''), 'Old configuration acknowledgement cannot overwrite a disabled breakpoint');
    Editor.SelStart := Length(Editor.Text); Editor.DebugKey(VK_F9,[]);
    Check(Model.BreakpointAt(Editor.DocumentUri,3,Item), 'F9 toggles breakpoint on caret line');
    Editor.DebugKey(VK_F9,[ssCtrl]); Check(Model.BreakpointAt(Editor.DocumentUri,3,Item) and not Item.Enabled, 'Ctrl+F9 toggles enabled state');
    Editor.DebugKey(VK_F10,[]); Check((Probe.Calls = 1) and (Probe.Command = dcStepOver), 'Editor step shortcut emits a host command');
    Baseline := Editor.DebugPixel(0,False); Model.SetExecution(dsPaused,Editor.DocumentUri,1);
    Check(Editor.DebugPixel(0,False) <> Baseline, 'Paused source line has a visible execution highlight');
    Model.SetExecution(dsRunning); Check(Editor.DebugPixel(0,False) = Baseline, 'Continue clears execution highlight');
    Model.BeginSession; Check(not Model.PublishBreakpoint(Session,ID,Item.Revision,bpsVerified,2,''), 'Previous session replies are rejected');
    Editor.Text := 'inserted'+sLineBreak+Before;
    Check(Model.BreakpointAt(Editor.DocumentUri,3,Item) and (Item.ID = ID), 'Text insertion moves breakpoint anchors by source line');
    Other.Text := Editor.Text;
    Check(Model.BreakpointAt(Editor.DocumentUri,3,Item) and (Item.ID = ID), 'Mirrored editor text synchronization does not shift breakpoint anchors twice');
    Model.ConfigureBreakpoint(ID,True,'a > 0','3','value={a}'); Model.BreakpointAt(Editor.DocumentUri,3,Item);
    Check(Model.PublishBreakpoint(Model.Session,ID,Item.Revision,bpsVerified,4,'Moved by runtime') and
      Model.DisplayBreakpointAt(Editor.DocumentUri,4,Item), 'Runtime-resolved breakpoint line is available for painting and toggling');
    Model.Free; Check((Editor.DebugModel = nil) and (Other.DebugModel = nil), 'Destroying shared model safely detaches every editor');
    Editor.DebugKey(VK_F9,[]);
  finally Probe.Free; Form.Free; end;
end;
procedure TestBreakpointPanel;
var Model: TERDDebugModel; Panel, Other: TBreakpointPanelProbe; Item: TERDDebugBreakpoint;
    FirstID, SecondID, Revision: Int64; Node: PVirtualNode; Stream: TMemoryStream;
begin
  Model := TERDDebugModel.Create(nil); Panel := TBreakpointPanelProbe.Create(nil); Other := TBreakpointPanelProbe.Create(nil);
  try
    Panel.DebugModel := Model; Other.DebugModel := Model; Panel.OnNavigate := Panel.Navigate;
    FirstID := Model.AddBreakpoint('file:///one.ts',4); SecondID := Model.AddBreakpoint('file:///two.ts',9);
    Check((Panel.RootNodeCount = 2) and (Other.RootNodeCount = 2), 'Breakpoint panels share model additions');
    Panel.SelectedID := FirstID; Panel.ConfigureSelected(True,'count > 3','5','count={count}');
    Check(Model.BreakpointAt('file:///one.ts',4,Item) and (Item.Condition = 'count > 3') and
      (Item.HitCondition = '5') and (Item.LogMessage = 'count={count}'), 'Panel configures breakpoint conditions and logpoints');
    Revision := Item.Revision; Panel.CommitCell(3,'count > 8');
    Check(Model.BreakpointAt('file:///one.ts',4,Item) and (Item.Condition = 'count > 8') and (Item.Revision > Revision), 'Inline edit updates model revision');
    Check(not Model.PublishBreakpoint(Model.Session,FirstID,Revision,bpsVerified,4,''), 'Panel edits invalidate earlier verification replies');
    Check(Model.PublishBreakpoint(Model.Session,FirstID,Item.Revision,bpsVerified,6,'Moved to executable line'), 'Breakpoint verification accepted');
    Check((Panel.SelectedID = FirstID) and (Panel.Text[Panel.FocusedNode,2] = '4 -> 6') and
      (Panel.Text[Panel.FocusedNode,7] = 'Moved to executable line'), 'Panel preserves identity and shows resolved line and backend message');
    Panel.NavigateSelected;
    Check((Panel.NavigatedUri = 'file:///one.ts') and (Panel.NavigatedLine = 6), 'Panel navigates to verified source location');
    Other.SelectedID := FirstID; Other.DeleteSelectedBreakpoints;
    Check((Panel.RootNodeCount = 1) and (Panel.SelectedID = 0), 'External deletion clears selection rather than retargeting actions');
    Panel.SelectedID := SecondID; Model.AddBreakpoint('file:///three.ts',2);
    Check(Panel.SelectedID = SecondID, 'Structural refresh preserves selected breakpoint identity');
    Node := Panel.GetFirst; while Node <> nil do begin Panel.Selected[Node] := True; Node := Panel.GetNext(Node); end;
    Panel.SetSelectedEnabled(False);
    for Item in Model.Breakpoints do Check(not Item.Enabled, 'Multi-selection disables every selected breakpoint');
    Panel.SetSelectedEnabled(True);
    for Item in Model.Breakpoints do Check(Item.Enabled, 'Multi-selection enables every selected breakpoint');
    Stream := TMemoryStream.Create;
    try Panel.OnNavigate := nil; Stream.WriteComponent(Panel); Check(Stream.Size > 0, 'Breakpoint panel streams configuration'); finally Stream.Free; end;
    Panel.DeleteSelectedBreakpoints; Check((Panel.RootNodeCount = 0) and (Length(Model.Breakpoints) = 0), 'Multi-delete uses stable identities during model refresh');
    FreeAndNil(Model); Check((Panel.DebugModel = nil) and (Other.DebugModel = nil), 'Model destruction detaches every breakpoint panel');
  finally Other.Free; Panel.Free; Model.Free; end;
end;

procedure TestCallStack;
var Form: TForm; Model: TERDDebugModel; Stack, CopyStack: TERDCallStack; Probe: TStackProbe;
    Request, Old: TERDStackRequest; Threads: TArray<TERDStackThread>; Frames: TArray<TERDStackFrame>;
    Frame: TERDStackFrame; Calls: Integer; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); Stack := TERDCallStack.Create(Form); Stack.Parent := Form; Probe := TStackProbe.Create;
  try
    Stack.DebugModel := Model; Stack.OnFrameSelected := Probe.Selected; Stack.OnNavigate := Probe.Navigate;
    Request := Stack.BeginSnapshot(Model.Session,0); Check(Request.RequestID = 0, 'Running/disconnected model cannot begin a stack snapshot');
    Model.SetExecution(dsPaused,'file:///stack.ts',4);
    SetLength(Threads,2); Threads[0].ID := 1; Threads[0].Name := 'Main'; Threads[1].ID := 2; Threads[1].Name := 'Worker';
    SetLength(Frames,3);
    Frames[0].ID := 0; Frames[0].ThreadID := 1; Frames[0].Name := 'callee'; Frames[0].DocumentUri := 'file:///stack.ts'; Frames[0].Line := 4;
    Frames[1].ID := 1; Frames[1].ThreadID := 1; Frames[1].Name := 'caller'; Frames[1].DocumentUri := 'file:///stack.ts'; Frames[1].Line := 9;
    Frames[2].ID := 2; Frames[2].ThreadID := 2; Frames[2].Name := '[native]';
    Request := Stack.BeginSnapshot(Model.Session,0);
    Check(Stack.PublishSnapshot(Request,Threads,Frames) and (Stack.RootNodeCount = 2), 'Stack snapshot groups frames under threads');
    Check((Probe.Calls = 1) and Stack.SelectedFrame(Frame) and (Frame.ID = 0), 'DAP frame zero is valid and the top frame loads scopes once');
    Check(Stack.SelectFrame(1,1) and (Probe.LastFrame.ID = 1) and (Probe.LastContext.RequestID = Request.RequestID), 'Selecting caller emits scoped frame event');
    Check((Model.ExecutionLine = 4) and (Model.ExecutionUri = 'file:///stack.ts'), 'Inspecting caller does not move execution position');
    Stack.NavigateSelected; Check((Probe.Navigations = 1) and (Probe.Line = 9), 'Caller navigates to its own source line');
    Stack.SelectFrame(2,2); Stack.NavigateSelected; Check(Probe.Navigations = 1, 'Native frame without source stays inspectable without invalid navigation');
    Frames[0].Name := 'mutated'; Check(Stack.SelectFrame(1,0) and Stack.SelectedFrame(Frame) and (Frame.Name = 'callee'), 'Stack owns a copy of published snapshot arrays');
    Old := Request; Request := Stack.BeginSnapshot(Model.Session,1);
    Check(Stack.Stale and Stack.Pending and not Stack.SelectedFrame(Frame), 'Pending refresh disables actions on retained old frames');
    Check(not Stack.PublishSnapshot(Old,Threads,Frames), 'Outdated stack replies are rejected');
    Frames[1].ID := 0; Check(not Stack.PublishSnapshot(Request,Threads,Frames) and Stack.Pending, 'Duplicate frame identities reject snapshot atomically'); Frames[1].ID := 1;
    Frames[2].ThreadID := 99; Check(not Stack.PublishSnapshot(Request,Threads,Frames), 'Unknown thread rejects snapshot'); Frames[2].ThreadID := 2;
    Check(Stack.PublishSnapshot(Request,Threads,Frames), 'Corrected pending snapshot can still be published');
    Calls := Probe.Calls; Model.SetExecution(dsRunning); Stack.SelectFrame(1,1); Stack.NavigateSelected;
    Check(Stack.Stale and not Stack.SelectedFrame(Frame) and (Probe.Calls = Calls) and (Probe.Navigations = 1), 'Continue makes old frame selection and navigation inactive');
    Model.SetExecution(dsPaused,'file:///stack.ts',4); Request := Stack.BeginSnapshot(Model.Session,2);
    Model.SetExecution(dsPaused,'file:///stack.ts',5);
    Check(not Stack.PublishSnapshot(Request,Threads,Frames), 'Changed paused location invalidates in-flight stack reply');
    Request := Stack.BeginSnapshot(Model.Session,3); Check(Stack.FailSnapshot(Request) and not Stack.Pending and Stack.Stale, 'Failed request retains only stale display data');
    Request := Stack.BeginSnapshot(Model.Session,4); Model.BeginSession;
    Check(not Stack.PublishSnapshot(Request,Threads,Frames), 'Previous debug session cannot republish stack frames');
    Model.SetExecution(dsPaused,'file:///stack.ts',4); Request := Stack.BeginSnapshot(Model.Session,0);
    Check(Stack.PublishSnapshot(Request,Threads,Frames), 'New session allows generation zero');
    RegisterClass(TERDCallStack); CopyStack := TERDCallStack.Create(Form); Stream := TMemoryStream.Create;
    try
      Stack.OnFrameSelected := nil; Stack.OnNavigate := nil; Stack.DebugModel := nil; Stream.WriteComponent(Stack); Stream.Position := 0; Stream.ReadComponent(CopyStack);
      Check((CopyStack.RootNodeCount = 0) and CopyStack.Stale, 'DFM does not retain runtime stack frames');
    finally Stream.Free; CopyStack.Free; end;
    Stack.DebugModel := Model; FreeAndNil(Model); Check(Stack.DebugModel = nil, 'Model destruction safely detaches call stack');
  finally Stack.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestLocalsPanel;
var Form: TForm; Model: TERDDebugModel; Panel, CopyPanel: TERDLocalsPanel; Probe: TLocalsProbe;
    Stack: TERDStackRequest; Frame: TERDStackFrame; Context, Old: TERDLocalsRequest;
    Scopes: TArray<TERDDebugScope>; Values: TArray<TERDInspectValue>; Value: TERDInspectValue;
    Request: TERDValueRequest; Stream: TMemoryStream; ScopeCalls: Integer;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); Panel := TERDLocalsPanel.Create(Form); Panel.Parent := Form; Probe := TLocalsProbe.Create;
  try
    Panel.DebugModel := Model; Panel.PageSize := 1; Probe.Panel := Panel;
    Panel.OnRequestScopes := Probe.Scopes; Panel.OnRequestVariables := Probe.Variables;
    Stack := Default(TERDStackRequest); Stack.Session := Model.Session; Stack.RequestID := 1;
    Frame := Default(TERDStackFrame); Frame.ThreadID := 1; Frame.Name := 'callee';
    Context := Panel.BeginFrame(Stack,Frame); Check(Context.RequestID = 0, 'Locals rejects a frame while disconnected');
    Model.SetExecution(dsPaused,'file:///locals.ts',4); Context := Panel.BeginFrame(Stack,Frame);
    Check(Panel.Pending and (Probe.ScopeCalls = 1) and (Probe.LastContext.FrameID = 0), 'Selecting frame zero requests scopes with context');
    SetLength(Scopes,2); Scopes[0].ID := 'scope.local'; Scopes[0].Name := 'Local'; Scopes[0].Reference := 100; Scopes[0].Expensive := True;
    Scopes[1].ID := 'scope.empty'; Scopes[1].Name := 'Empty';
    Check(Panel.PublishScopes(Context,Scopes) and not Panel.Pending and not Panel.Stale and (Panel.ValueTree.RootNodeCount = 2), 'Scopes publish as roots');
    Check((Probe.VariableCalls = 0) and Panel.ValueTree.ValueByID('scope.local',Value) and (Value.Reference = 100) and Value.HasChildren, 'Expensive scope is lazy and preserves variables reference');
    Check(Panel.ValueTree.RequestChildren('scope.empty') = 0, 'Reference zero is an empty non-expandable scope');
    Panel.ValueTree.RequestChildren('scope.local'); Request := Probe.LastRequest;
    Check((Probe.VariableCalls = 1) and (Probe.LastParent.Reference = 100) and (Request.Count = 1) and (Request.StartIndex = 0), 'Variables request carries parent reference and page bounds');
    SetLength(Values,1); Values[0].ID := 'scope.local.object'; Values[0].Name := 'object'; Values[0].Reference := 200; Values[0].HasChildren := True;
    Check(Panel.PublishVariables(Context,Request,Values,True), 'First variable page is accepted');
    Panel.ValueTree.RequestChildren('scope.local'); Request := Probe.LastRequest;
    Check(Request.StartIndex = 1, 'Load more starts after published variables');
    Values[0] := Default(TERDInspectValue); Values[0].ID := 'scope.local.peak'; Values[0].Name := 'Peak'; Values[0].Availability := vaUnavailable;
    Check(Panel.PublishVariables(Context,Request,Values,False) and Panel.ValueTree.ValueByID(Values[0].ID,Value) and (Value.Availability = vaUnavailable), 'Unavailable value stays distinct from measured zero');
    Panel.ValueTree.RequestChildren('scope.local.object'); Request := Probe.LastRequest;
    Check(Probe.LastParent.Reference = 200, 'Nested object uses its own variables reference');
    Old := Context; Frame.ID := 1; Frame.Name := 'caller'; Context := Panel.BeginFrame(Stack,Frame);
    Check(not Panel.PublishVariables(Old,Request,Values,False) and not Panel.PublishScopes(Old,Scopes), 'Frame switch rejects earlier scopes and child replies');
    Check(Panel.ValueTree.ValueByID('scope.local.object',Value) and (Value.Availability = vaStale), 'Old display stays visibly stale while scopes load');
    Scopes[1].ID := Scopes[0].ID; Check(not Panel.PublishScopes(Context,Scopes) and Panel.Pending, 'Invalid scope IDs are rejected atomically'); Scopes[1].ID := 'scope.empty';
    Check(Panel.FailScopes(Context,'Scope provider failed') and Panel.Stale and (Panel.ErrorText = 'Scope provider failed'), 'Scope failure exposes an error and invalidates pending request');
    Old := Context; Panel.RetryScopes; Context := Probe.LastContext;
    Check((Context.RequestID <> Old.RequestID) and Panel.Pending and not Panel.PublishScopes(Old,Scopes), 'Retry uses a fresh context token');
    Check(Panel.PublishScopes(Context,Scopes), 'Retried scope response is accepted');
    Panel.ValueTree.RequestChildren('scope.local'); Request := Probe.LastRequest;
    Check(Panel.FailVariables(Context,Request,'Variables unavailable'), 'Variable request failure is scoped');
    Panel.ValueTree.RequestChildren('scope.local'); Request := Probe.LastRequest;
    Values[0] := Default(TERDInspectValue); Values[0].ID := 'scope.local.value'; Values[0].ValueText := '7';
    Check(Panel.PublishVariables(Context,Request,Values,False), 'Failed variable request can be retried');
    Model.SetExecution(dsRunning);
    Check(Panel.Stale and not Panel.PublishVariables(Context,Request,Values,False), 'Continue invalidates variable replies');
    ScopeCalls := Probe.ScopeCalls; Panel.RetryScopes; Check(Probe.ScopeCalls = ScopeCalls, 'Retry does not reactivate a resumed frame');
    Model.SetExecution(dsPaused,'file:///locals.ts',4); Probe.Synchronous := True;
    Context := Panel.BeginFrame(Stack,Frame); Panel.ValueTree.RequestChildren('scope.local');
    Check(not Panel.Pending and Panel.ValueTree.ValueByID('scope.local.value',Value) and (Value.ValueText = '42'), 'Synchronous scope and variable providers are supported');
    Probe.Synchronous := False; Context := Panel.BeginFrame(Stack,Frame); Model.SetExecution(dsPaused,'file:///locals.ts',5);
    Check(not Panel.PublishScopes(Context,Scopes), 'Changed execution location rejects old scopes');
    Context := Panel.BeginFrame(Stack,Frame); Model.BeginSession;
    Check(not Panel.PublishScopes(Context,Scopes), 'Session change rejects old scopes');
    Stack.Session := Model.Session; Model.SetExecution(dsPaused,'file:///locals.ts',4); Context := Panel.BeginFrame(Stack,Frame);
    Check(Panel.PublishScopes(Context,nil) and not Panel.Stale and (Panel.ValueTree.RootNodeCount = 0), 'Frame without scopes shows an empty current view');
    Panel.OnRequestScopes := nil; Panel.OnRequestVariables := nil; Panel.DebugModel := nil;
    RegisterClass(TERDLocalsPanel); CopyPanel := TERDLocalsPanel.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Panel); Stream.Position := 0; Stream.ReadComponent(CopyPanel);
      Check((CopyPanel.PageSize = 1) and CopyPanel.Stale and (CopyPanel.ValueTree.RootNodeCount = 0) and (CopyPanel.ComponentCount = 2), 'DFM keeps settings without serializing internal tree or runtime context');
    finally Stream.Free; CopyPanel.Free; end;
    Panel.DebugModel := Model; FreeAndNil(Model); Check((Panel.DebugModel = nil) and Panel.Stale, 'Model destruction safely detaches locals panel');
  finally Panel.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestWatchPanel;
var Form: TForm; Panel, CopyPanel: TERDWatchPanel; Model: TERDDebugModel; Probe: TWatchProbe;
    Stack: TERDStackRequest; Frame: TERDStackFrame; Context, Old: TERDWatchContext;
    Results: TArray<TERDWatchResult>; Values: TArray<TERDInspectValue>; Value: TERDInspectValue;
    Request: TERDValueRequest; FirstID, SecondID, Calls: Integer; Key: Word; Rejected: Boolean; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); Panel := TERDWatchPanel.Create(Form); Panel.Parent := Form; Probe := TWatchProbe.Create;
  try
    Probe.Panel := Panel; Panel.DebugModel := Model; Panel.OnEvaluate := Probe.Evaluate; Panel.OnRequestVariables := Probe.Variables;
    FirstID := Panel.AddWatch('status'); SecondID := Panel.AddWatch('missing');
    Check((Panel.Watches.Count = 2) and (Probe.Calls = 0), 'Watches are configurable without evaluating while disconnected');
    Rejected := False; try Panel.AddWatch(' '); except on E: EArgumentException do Rejected := True; end;
    Check(Rejected and (Panel.Watches.Count = 2), 'Invalid expression does not insert a partial watch');
    Stack := Default(TERDStackRequest); Stack.Session := Model.Session; Stack.RequestID := 1;
    Frame := Default(TERDStackFrame); Frame.ThreadID := 1; Frame.Name := 'callee';
    Model.SetExecution(dsPaused,'file:///watches.ts',4); Context := Panel.BeginFrame(Stack,Frame);
    Check(Panel.Pending and (Probe.Calls = 1) and (Probe.LastExpressions[0].ID = FirstID) and (Probe.LastContext.FrameID = 0), 'Watch batch carries stable IDs and frame zero context');
    SetLength(Results,2); Results[0].ID := FirstID; Results[0].Value.ValueText := '{ value }'; Results[0].Value.Reference := 300; Results[0].Value.HasChildren := True;
    Results[1].ID := SecondID; Results[1].Value.Availability := vaError; Results[1].Value.ValueText := 'Unknown identifier';
    Results[1].ID := FirstID; Check(not Panel.PublishResults(Context,Results) and Panel.Pending, 'Duplicate result identities reject batch atomically'); Results[1].ID := SecondID;
    Check(Panel.PublishResults(Context,Results) and not Panel.Pending and Panel.ValueTree.ValueByID('watch:'+IntToStr(SecondID),Value) and (Value.Availability = vaError), 'Per-expression errors remain visible beside successful results');
    Panel.ValueTree.RequestChildren('watch:'+IntToStr(FirstID)); Request := Probe.LastRequest;
    Check((Probe.VariableCalls = 1) and (Request.ParentID = 'watch:'+IntToStr(FirstID)), 'Object watches request children lazily');
    SetLength(Values,1); Values[0].ID := Request.ParentID+'.value'; Values[0].Name := 'value'; Values[0].ValueText := '3';
    Check(Panel.PublishVariables(Context,Request,Values,False), 'Nested watch value is published with both tokens');
    Old := Context; Panel.SetExpression(FirstID,'status.count'); Context := Probe.LastContext;
    Check((Probe.Calls = 2) and (Context.RequestID <> Old.RequestID) and not Panel.PublishResults(Old,Results), 'Editing expression automatically reevaluates and rejects previous result');
    Results[0].Value := Default(TERDInspectValue); Results[0].Value.ValueText := '3';
    Check(Panel.PublishResults(Context,Results) and Panel.ValueTree.ValueByID('watch:'+IntToStr(FirstID),Value) and (Value.Name = 'status.count'), 'Edited expression keeps watch identity and receives new value');
    Old := Context; Frame.ID := 1; Context := Panel.BeginFrame(Stack,Frame);
    Check(not Panel.PublishVariables(Old,Request,Values,False), 'Frame switch rejects earlier watch child reply');
    Check(Panel.FailEvaluation(Context,'Provider failed') and (Panel.ErrorText = 'Provider failed'), 'Batch failure shows an error');
    Old := Context; Context := Panel.RefreshValues; Check((Context.RequestID <> Old.RequestID) and not Panel.PublishResults(Old,Results), 'Explicit refresh retries with a new token');
    Model.SetExecution(dsRunning); Calls := Probe.Calls; Panel.RefreshValues;
    Check(not Panel.Active and (Probe.Calls = Calls) and not Panel.PublishResults(Context,Results), 'Continue prevents evaluation and rejects outstanding batch');
    Panel.ExpressionEditor.Text := 'status.color'; Key := VK_RETURN; Panel.ExpressionEditor.OnKeyDown(Panel.ExpressionEditor,Key,[]);
    Check((Key = 0) and (Panel.Watches.Count = 3) and (Probe.Calls = Calls), 'Enter adds a watch without evaluating a resumed frame');
    Probe.Synchronous := True; Model.SetExecution(dsPaused,'file:///watches.ts',4); Context := Panel.BeginFrame(Stack,Frame);
    Check(not Panel.Pending and Panel.ValueTree.ValueByID('watch:'+IntToStr(FirstID),Value) and (Value.ValueText = '42'), 'Synchronous watch evaluator is supported');
    Probe.Synchronous := False; Old := Panel.RefreshValues; Panel.DeleteWatch(SecondID);
    Check((Panel.Watches.Count = 2) and not Panel.PublishResults(Old,Results), 'Deleting watch invalidates outstanding result batch');
    Context := Probe.LastContext; Model.BeginSession; Check(not Panel.PublishResults(Context,Results), 'New debug session rejects prior watch batch');
    Panel.OnEvaluate := nil; Panel.OnRequestVariables := nil; Panel.DebugModel := nil;
    RegisterClasses([TERDWatchPanel,TERDWatchItem,TERDWatchCollection]); CopyPanel := TERDWatchPanel.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Panel); Stream.Position := 0; Stream.ReadComponent(CopyPanel);
      Check((CopyPanel.Watches.Count = 2) and (CopyPanel.Watches[0].Expression = 'status.count') and not CopyPanel.Active and (CopyPanel.ComponentCount = 3), 'DFM persists expressions without runtime results or duplicate internal controls');
    finally Stream.Free; CopyPanel.Free; end;
    Panel.DebugModel := Model; FreeAndNil(Model); Check((Panel.DebugModel = nil) and not Panel.Active, 'Model destruction safely detaches watches');
  finally Panel.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestDisassemblyView;
var Form: TForm; Model: TERDDebugModel; View, CopyView: TERDDisassemblyView; Probe: TDisassemblyProbe;
    Request, Old, Forged: TERDDisassemblyRequest; Items: TArray<TERDDisassembledInstruction>;
    Item: TERDDisassembledInstruction; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); View := TERDDisassemblyView.Create(Form); View.Parent := Form; Probe := TDisassemblyProbe.Create;
  try
    View.DebugModel := Model; View.PageSize := 2; View.OnRequestInstructions := Probe.Request; View.OnNavigate := Probe.Navigate;
    Request := View.RequestInstructions(Model.Session,0,'opaque:function'); Check(Request.RequestID = 0, 'Disassembly refuses disconnected requests');
    Model.SetExecution(dsPaused,'file:///bytecode.ts',4); Request := View.RequestInstructions(Model.Session,0,'opaque:function');
    Check(View.Pending and (Probe.Calls = 1) and (Probe.Last.Count = 2), 'Disassembly provider receives bounded instruction request');
    SetLength(Items,2); Items[0].Address := 'opaque:entry'; Items[0].Instruction := 'LOAD value'; Items[0].DocumentUri := 'file:///bytecode.ts'; Items[0].Line := 4;
    Items[1].Address := 'opaque:next'; Items[1].Instruction := 'RETURN';
    Forged := Request; Inc(Forged.InstructionOffset); Check(not View.PublishInstructions(Forged,Items,Items[0].Address), 'Forged window token is rejected');
    Items[1].Address := Items[0].Address; Check(not View.PublishInstructions(Request,Items,Items[0].Address) and View.Pending, 'Duplicate instruction address rejects window atomically'); Items[1].Address := 'opaque:next';
    Check(View.PublishInstructions(Request,Items,Items[0].Address) and View.SelectedInstruction(Item) and (Item.Address = Items[0].Address), 'Opaque current address selects instruction without parsing');
    Check(View.Text[View.FocusedNode,0] <> '', 'Current instruction has a marker'); View.NavigateSelected;
    Check((Probe.Navigations = 1) and (Probe.Line = 4), 'Instruction source mapping raises navigation');
    Items[0].Instruction := 'mutated'; Check(View.SelectedInstruction(Item) and (Item.Instruction = 'LOAD value'), 'Instruction window owns a copy of provider array');
    View.SelectAddress('opaque:next'); View.NavigateSelected; Check(Probe.Navigations = 1, 'Instruction without source does not navigate');
    View.SetInstructionPointer('opaque:next'); Check(View.CurrentAddress = 'opaque:next', 'Host can update execution marker without changing source execution');
    Check(Model.ExecutionLine = 4, 'Instruction selection does not change source execution');
    Old := Request; Request := View.MovePage(1); Check((Request.InstructionOffset = 2) and not View.PublishInstructions(Old,Items,''), 'Next page supersedes old request and uses instruction offsets');
    Request := View.MovePage(-1); Check(Request.InstructionOffset = 0, 'Previous page reverses instruction offset');
    Items[0].Address := ''; Items[0].Availability := iaUnavailable;
    Check(View.PublishInstructions(Request,Items,'') and View.SelectAddress('opaque:next'), 'Unavailable placeholder is accepted without an invented address');
    Request := View.RequestInstructions(Model.Session,1,'opaque:function'); Model.SetExecution(dsRunning);
    Check(View.Stale and (View.CurrentAddress = '') and not View.PublishInstructions(Request,Items,'opaque:next'), 'Continue clears execution marker and rejects pending window');
    Model.SetExecution(dsPaused,'file:///bytecode.ts',4); Request := View.RequestInstructions(Model.Session,2,'opaque:function');
    Check(View.FailInstructions(Request,'No disassembler') and (View.ErrorText = 'No disassembler'), 'Disassembly failure is visible');
    Request := View.RequestInstructions(Model.Session,3,'opaque:function'); Model.BeginSession;
    Check(not View.PublishInstructions(Request,Items,''), 'Session change rejects previous disassembly');
    View.OnRequestInstructions := nil; View.OnNavigate := nil; View.DebugModel := nil;
    RegisterClass(TERDDisassemblyView); CopyView := TERDDisassemblyView.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(View); Stream.Position := 0; Stream.ReadComponent(CopyView);
      Check((CopyView.PageSize = 2) and CopyView.Stale and (CopyView.RootNodeCount = 0) and (CopyView.ErrorText = ''), 'DFM retains configuration without runtime disassembly or status');
    finally Stream.Free; CopyView.Free; end;
    View.DebugModel := Model; FreeAndNil(Model); Check((View.DebugModel = nil) and View.Stale, 'Model destruction safely detaches disassembly');
  finally View.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestFiberList;
var Form: TForm; Model: TERDDebugModel; List, CopyList: TERDFiberList; Probe: TFiberProbe;
    Request, Old: TERDFiberRequest; Fibers: TArray<TERDFiberInfo>; Fiber: TERDFiberInfo;
    Swap: TERDFiberInfo; Selections, Clears: Integer; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); List := TERDFiberList.Create(Form); List.Parent := Form; Probe := TFiberProbe.Create;
  try
    List.DebugModel := Model; List.OnRequestFibers := Probe.Request; List.OnFiberSelected := Probe.Selected;
    List.OnSelectionCleared := Probe.Cleared; List.OnInspectFiber := Probe.Inspect; List.OnNavigate := Probe.Navigate;
    Request := List.RequestSnapshot(Model.Session,0); Check(Request.RequestID = 0, 'Disconnected model cannot request live fibers');
    Model.SetExecution(dsRunning); Request := List.RequestSnapshot(Model.Session,0);
    Check((Request.RequestID > 0) and List.Pending and (Probe.Calls = 1), 'Fiber requests work during execution');
    SetLength(Fibers,2); Fibers[0].ID := 1; Fibers[0].State := 'RUNNING'; Fibers[0].FunctionName := 'main';
    Fibers[1].ID := 2; Fibers[1].State := 'WAITING'; Fibers[1].CallStackDepth := 3; Fibers[1].FunctionIndex := 7; Fibers[1].BytecodeOffset := 12;
    Fibers[1].ID := 1; Check(not List.PublishSnapshot(Request,Fibers) and List.Pending, 'Duplicate fiber identities reject snapshot atomically'); Fibers[1].ID := 2;
    Check(List.PublishSnapshot(Request,Fibers) and not List.Stale and (List.RootNodeCount = 2), 'Valid scheduler snapshot is published');
    Check(List.SelectFiber(2) and (Probe.Fiber.ID = 2), 'Fiber selection exposes stable ID');
    Selections := Probe.Selections; List.FocusedColumn := 2; Check(Probe.Selections = Selections, 'Column focus does not repeat fiber selection event');
    Old := Request; Request := List.RequestSnapshot(Model.Session,1);
    Check(List.SelectedFiber(Fiber) and (List.Context.RequestID = Old.RequestID), 'Polling retains last valid sample with its published context');
    Swap := Fibers[0]; Fibers[0] := Fibers[1]; Fibers[1] := Swap; Fibers[0].State := 'RUNNABLE';
    Check(List.PublishSnapshot(Request,Fibers) and List.SelectedFiber(Fiber) and (Fiber.ID = 2) and (Fiber.State = 'RUNNABLE'), 'Reordered rows retain selected fiber identity');
    Check((Probe.Selections = Selections+1) and (Probe.Last.RequestID = Request.RequestID), 'New selected sample publishes fresh detail context');
    Fibers[0].State := 'mutated'; Check(List.SelectedFiber(Fiber) and (Fiber.State = 'RUNNABLE'), 'Fiber list owns snapshot values');
    List.InspectSelected; List.NavigateSelected;
    Check((Probe.Inspections = 1) and (Probe.Navigations = 1) and (Model.State = dsRunning), 'Inspect and navigate are host events without execution changes');
    Clears := Probe.Clears; List.StateFilter := 'running';
    Check((List.RootNodeCount = 1) and not List.SelectedFiber(Fiber) and (Probe.Clears = Clears+1), 'Filtering out selection clears it without retargeting another row');
    List.StateFilter := ''; List.SearchText := 'MAIN'; Check(List.RootNodeCount = 1, 'Search is case-insensitive across symbols');
    List.SearchText := ''; List.SelectFiber(2);
    Old := Request; Request := List.RequestSnapshot(Model.Session,2); SetLength(Fibers,1); Fibers[0].ID := 1; Fibers[0].State := 'NEW_BACKEND_STATE';
    Check(not List.PublishSnapshot(Old,Fibers), 'Superseded fiber reply is rejected');
    Check(List.PublishSnapshot(Request,Fibers) and not List.SelectedFiber(Fiber), 'Vanished selected fiber does not select replacement row');
    List.SelectFiber(1); Check(List.SelectedFiber(Fiber) and (Fiber.State = 'NEW_BACKEND_STATE'), 'Unknown backend states remain intact');
    Request := List.RequestSnapshot(Model.Session,3); Fibers[0].Availability := faUnavailable;
    Check(List.PublishSnapshot(Request,Fibers) and (List.Text[List.FocusedNode,2] = '<unavailable>'), 'Unavailable details are not displayed as measured zero');
    List.NavigateSelected; Check(Probe.Navigations = 1, 'Unavailable program counter cannot navigate');
    Request := List.RequestSnapshot(Model.Session,4); Model.SetExecution(dsPaused,'file:///fibers.ts',1);
    Check(List.PublishSnapshot(Request,Fibers), 'Pause does not invalidate scheduler telemetry request');
    Request := List.RequestSnapshot(Model.Session,5); Check(List.FailSnapshot(Request,'Scheduler unavailable') and List.Stale and (List.ErrorText <> ''), 'Failed snapshot keeps stale display and error');
    Request := List.RequestSnapshot(Model.Session,6); Check(List.PublishSnapshot(Request,nil) and not List.Stale and (List.RootNodeCount = 0), 'Empty current snapshot clears fiber list');
    Request := List.RequestSnapshot(Model.Session,7); Model.BeginSession;
    Check(not List.PublishSnapshot(Request,Fibers), 'New session rejects pending prior-session telemetry');
    Model.SetExecution(dsRunning); Request := List.RequestSnapshot(Model.Session,0); Check(List.PublishSnapshot(Request,Fibers), 'New session accepts generation zero');
    List.OnRequestFibers := nil; List.OnFiberSelected := nil; List.OnSelectionCleared := nil; List.OnInspectFiber := nil; List.OnNavigate := nil; List.DebugModel := nil;
    RegisterClass(TERDFiberList); CopyList := TERDFiberList.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(List); Stream.Position := 0; Stream.ReadComponent(CopyList);
      Check(CopyList.Stale and (CopyList.RootNodeCount = 0) and (CopyList.Context.RequestID = 0), 'DFM does not persist runtime fibers or selection');
    finally Stream.Free; CopyList.Free; end;
    List.DebugModel := Model; FreeAndNil(Model); Check((List.DebugModel = nil) and List.Stale, 'Model destruction safely detaches fiber list');
  finally List.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestRuntimeMonitor;
var Form: TForm; Model: TERDDebugModel; Monitor, CopyMonitor: TERDRuntimeMonitor; Probe: TRuntimeProbe;
    Request, Old: TERDRuntimeRequest; Metrics: TArray<TERDRuntimeMetric>; Metric: TERDRuntimeMetric;
    Value: TERDInspectValue; Sample: TERDTimeSample; Stream: TMemoryStream; I: Integer;
  procedure Configure(Index: Integer; const ID, UnitText: string; Group: TERDRuntimeGroup; Number: UInt64; Cumulative: Boolean = False);
  begin Metrics[Index].ID := ID; Metrics[Index].Group := Group; Metrics[Index].UnitText := UnitText; Metrics[Index].Kind := rmUnsigned; Metrics[Index].UnsignedValue := Number; Metrics[Index].Cumulative := Cumulative; end;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); Monitor := TERDRuntimeMonitor.Create(Form); Monitor.Parent := Form; Probe := TRuntimeProbe.Create;
  try
    Monitor.DebugModel := Model; Monitor.OnRequestSnapshot := Probe.Request; Monitor.MaxSamples := 16;
    Request := Monitor.RequestSnapshot(Model.Session,0); Check(Request.RequestID = 0, 'Disconnected runtime monitor refuses live request');
    Model.SetExecution(dsRunning); Request := Monitor.RequestSnapshot(Model.Session,0);
    Check(Monitor.Pending and (Probe.Calls = 1), 'Runtime monitor samples while running');
    SetLength(Metrics,7);
    Configure(0,'heap.usedBytes','bytes',rgHeap,100); Configure(1,'heap.totalBytes','bytes',rgHeap,200);
    Configure(2,'gc.minorGCTimeMicros','us',rgGC,100,True); Configure(3,'gc.majorGCTimeMicros','us',rgGC,50,True);
    Configure(4,'scheduler.currentFiberCount','fibers',rgScheduler,3); Configure(5,'scheduler.runnableFiberCount','fibers',rgScheduler,1);
    Configure(6,'heap.objectCount','objects',rgHeap,High(UInt64));
    Metrics[0].UnitText := 'ms'; Check(not Monitor.PublishSnapshot(Request,0,Metrics) and (Monitor.ChartFor(rgHeap).Series[0].Count = 0), 'Mixed chart units reject snapshot without partial history'); Metrics[0].UnitText := 'bytes';
    Metrics[1].ID := Metrics[0].ID; Check(not Monitor.PublishSnapshot(Request,0,Metrics), 'Duplicate metric IDs reject snapshot atomically'); Metrics[1].ID := 'heap.totalBytes';
    Check(not Monitor.PublishSnapshot(Request,NaN,Metrics), 'NaN timestamp cannot enter chart history');
    Check(Monitor.PublishSnapshot(Request,0,Metrics) and not Monitor.Stale and (Monitor.SampleTime = 0), 'Complete observed snapshot is published');
    Check(Monitor.TableFor(rgHeap).ValueByID('heap.objectCount',Value) and (Value.ValueText = '18446744073709551615'), 'Unsigned counters retain exact display precision beyond Double');
    Sample := Monitor.ChartFor(rgHeap).Series[0].Sample(0); Check(Sample.Available and (Sample.Value = 100), 'Heap history contains observed byte value');
    Metrics[0].UnsignedValue := 999; Check(Monitor.MetricByID('heap.usedBytes',Metric) and (Metric.UnsignedValue = 100), 'Monitor owns copied snapshot values');
    Old := Request; Request := Monitor.RequestSnapshot(Model.Session,1); Check(not Monitor.PublishSnapshot(Old,1,Metrics), 'Late runtime snapshot is rejected');
    Check(not Monitor.PublishSnapshot(Request,0,Metrics), 'Duplicate or backwards timestamp does not append samples');
    Metrics[0].Availability := rmaUnavailable; Metrics[2].UnsignedValue := 0;
    Check(Monitor.PublishSnapshot(Request,1,Metrics), 'Unavailable metric and reset cumulative counter are valid data');
    Sample := Monitor.ChartFor(rgHeap).Series[0].Sample(1); Check(not Sample.Available, 'Unavailable metric creates a chart gap, not a measured zero');
    Sample := Monitor.ChartFor(rgGC).Series[0].Sample(1); Check(not Sample.Available, 'Cumulative counter reset creates a chart discontinuity');
    Check(Monitor.TableFor(rgGC).ValueByID('gc.minorGCTimeMicros',Value) and (Value.ValueText = '0'), 'Counter reset zero remains an exact available table value');
    Monitor.Paused := True; Request := Monitor.RequestSnapshot(Model.Session,2);
    Check(Monitor.FailSnapshot(Request,2,'Provider failed') and Monitor.Stale and (Monitor.ErrorText = 'Provider failed'), 'Provider failure marks tables stale and appends missing observation');
    Check((Monitor.ChartFor(rgHeap).Series[0].Count = 3) and not Monitor.ChartFor(rgHeap).Series[0].Sample(2).Available and (Monitor.SampleTime = 1), 'Failure preserves last successful sample time and explicit history gap');
    Metrics[0].Availability := rmaAvailable;
    for I := 3 to 22 do begin Request := Monitor.RequestSnapshot(Model.Session,I); Check(Monitor.PublishSnapshot(Request,I,Metrics), 'Bounded runtime history accepts next observation'); end;
    Check(Monitor.Paused and (Monitor.ChartFor(rgHeap).Series[0].Count = 16), 'Paused chart view continues bounded sampling');
    Monitor.MarkStale; Request := Monitor.RequestSnapshot(Model.Session,23); Monitor.PublishSnapshot(Request,23,Metrics);
    Sample := Monitor.ChartFor(rgHeap).Series[0].Sample(15); Check(not Sample.Available, 'Resume after stale interval breaks connecting line');
    Request := Monitor.RequestSnapshot(Model.Session,24); Model.BeginSession;
    Check(not Monitor.PublishSnapshot(Request,24,Metrics), 'Session change rejects old monitor response');
    Model.SetExecution(dsRunning); Request := Monitor.RequestSnapshot(Model.Session,0);
    Check(Monitor.PublishSnapshot(Request,0,Metrics) and (Monitor.ChartFor(rgHeap).Series[0].Count = 1), 'New session clears history and permits a restarted sample clock');
    Monitor.OnRequestSnapshot := nil; Monitor.DebugModel := nil; Monitor.ShowCharts := False; Monitor.WindowSeconds := 15;
    RegisterClass(TERDRuntimeMonitor); CopyMonitor := TERDRuntimeMonitor.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Monitor); Stream.Position := 0; Stream.ReadComponent(CopyMonitor);
      Check((CopyMonitor.MaxSamples = 16) and (CopyMonitor.WindowSeconds = 15) and not CopyMonitor.ShowCharts and CopyMonitor.Stale and
        (CopyMonitor.ChartFor(rgHeap).Series[0].Count = 0) and (CopyMonitor.TableFor(rgHeap).RootNodeCount = 0) and (CopyMonitor.ComponentCount = 10), 'DFM persists monitor settings without runtime data or duplicated internals');
    finally Stream.Free; CopyMonitor.Free; end;
    Monitor.DebugModel := Model; FreeAndNil(Model); Check((Monitor.DebugModel = nil) and Monitor.Stale, 'Model destruction safely detaches runtime monitor');
  finally Monitor.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestShapeInspector;
var Form: TForm; Inspector, CopyInspector: TERDShapeInspector; Model: TERDDebugModel; Probe: TShapeProbe;
    Request, Old: TERDShapeRequest; Snapshot: TERDShapeSnapshot; Info: TERDShapeInfo;
    Props: TArray<TERDShapeProperty>; Edges: TArray<TERDShapeTransition>; Value: TERDInspectValue;
    MapNode, Other: TERDShapeMapNode; Node: PVirtualNode; Stream: TMemoryStream; I, J: Integer; Overlap: TRect;
begin
  Form := TForm.Create(nil); Model := TERDDebugModel.Create(nil); Probe := TShapeProbe.Create;
  Inspector := TERDShapeInspector.Create(Form); Inspector.Parent := Form;
  try
    Inspector.DebugModel := Model; Inspector.OnRequestShape := Probe.Request; Inspector.OnNavigateShape := Probe.Navigate; Inspector.OnInspectProperty := Probe.Inspect;
    Request := Inspector.RequestShape(Model.Session,0,42); Check(Request.RequestID = 0, 'Disconnected shape inspector refuses requests');
    Model.SetExecution(dsPaused); Request := Inspector.RequestShape(Model.Session,0,42);
    Check(Inspector.Pending and (Probe.Requests = 1) and (Probe.Last.ShapeID = 42), 'Shape request carries session, generation and exact target');
    Snapshot := Default(TERDShapeSnapshot); Snapshot.Info.ID := 42; Snapshot.Info.ParentID := 0; Snapshot.Info.Depth := 1;
    Snapshot.Info.PrototypeShapeID := 7; Snapshot.Info.PropertyCount := 2; Snapshot.Info.RefCount := High(Cardinal);
    Snapshot.Info.TransitionCountAvailable := True; Snapshot.Info.TransitionCount := 5;
    SetLength(Snapshot.Properties,2); Snapshot.Properties[0].Name := ''; Snapshot.Properties[0].Offset := 0; Snapshot.Properties[0].Writable := True;
    Snapshot.Properties[1].Name := 'name'; Snapshot.Properties[1].Offset := High(Cardinal); Snapshot.Properties[1].IsAccessor := True;
    SetLength(Snapshot.Transitions,5);
    for I := 0 to 4 do begin Snapshot.Transitions[I].PropertyName := 'extra'+IntToStr(I); Snapshot.Transitions[I].TargetShapeID := 100+I; end;
    Snapshot.Registry.Available := True; Snapshot.Registry.TotalShapes := 50;
    Snapshot.Properties[1].Name := ''; Check(not Inspector.PublishSnapshot(Request,Snapshot) and Inspector.Pending, 'Duplicate property names reject snapshot without consuming request'); Snapshot.Properties[1].Name := 'name';
    Snapshot.Properties[1].Offset := 0; Check(not Inspector.PublishSnapshot(Request,Snapshot), 'Duplicate property slots reject shape layout'); Snapshot.Properties[1].Offset := High(Cardinal);
    Snapshot.Info.ID := 43; Check(not Inspector.PublishSnapshot(Request,Snapshot), 'Response for another shape cannot replace requested shape'); Snapshot.Info.ID := 42;
    Snapshot.Transitions[1].PropertyName := 'extra0'; Snapshot.Transitions[1].TargetShapeID := 100;
    Check(not Inspector.PublishSnapshot(Request,Snapshot), 'Exact duplicate transition edges reject snapshot'); Snapshot.Transitions[1].TargetShapeID := 101;
    Check(Inspector.PublishSnapshot(Request,Snapshot) and Inspector.ShapeInfo(Info) and (Info.RefCount = High(Cardinal)), 'Complete snapshot preserves exact uint32 counters');
    Snapshot.Properties[1].Name := 'mutated'; Snapshot.Transitions[0].TargetShapeID := 999;
    Props := Inspector.ShapeProperties; Edges := Inspector.ShapeTransitions;
    Check((Props[1].Name = 'name') and (Edges[0].TargetShapeID = 100), 'Shape inspector owns copied property and transition arrays');
    Props[1].Name := 'external'; Check(Inspector.PropertyTree.ValueByID('property:1',Value) and (Value.Name = 'name') and (Value.ValueText = '4294967295'), 'Property slot remains exact and getter arrays do not mutate retained snapshot');
    Check(Inspector.SummaryTree.ValueByID('maxDepth',Value) and (Value.Availability = vaUnavailable), 'Unimplemented registry statistic is unavailable rather than measured zero');
    Check(Inspector.ShapeMap.NodeCount = 6, 'Relationship map shows current, parent, prototype and first three outgoing cards');
    for I := 0 to Inspector.ShapeMap.NodeCount-1 do
    begin
      Inspector.ShapeMap.MapNode(I,MapNode);
      Check((MapNode.Bounds.Left >= 0) and (MapNode.Bounds.Right <= Inspector.ShapeMap.ClientWidth), 'Diagram cards fit the viewport');
      for J := I+1 to Inspector.ShapeMap.NodeCount-1 do begin Inspector.ShapeMap.MapNode(J,Other); Check(not IntersectRect(Overlap,MapNode.Bounds,Other.Bounds), 'Relationship cards do not overlap'); end;
    end;
    Inspector.Height := 240; Check(Inspector.ShapeMap.NodeCount = 0, 'Tiny diagram suppresses overlapping cards and requests a larger viewport'); Inspector.Height := 640;
    Inspector.ShapeMap.ActivateNode(1); Check((Probe.Navigations = 1) and (Probe.Target = 0) and (Probe.Last.ShapeID = 42), 'Visual parent card can navigate to valid root shape zero');
    Inspector.ShapeMap.ChangePage(1); Check((Inspector.ShapeMap.TransitionPage = 1) and (Inspector.ShapeMap.NodeCount = 5), 'Large transition fan-out is paged without hiding available edges');
    Inspector.ShapeMap.ActivateNode(3); Check(Probe.Target = 103, 'Second diagram page navigates correct transition target');
    Inspector.SearchText := 'name'; Check((Inspector.PropertyTree.RootNodeCount = 1) and (Inspector.TransitionTree.RootNodeCount = 0), 'Details filter searches properties and transitions');
    Node := Inspector.PropertyTree.GetFirst; Inspector.PropertyTree.FocusedNode := Node; Inspector.PropertyTree.Selected[Node] := True;
    Inspector.InspectSelectedProperty; Check((Probe.Inspections = 1) and (Probe.PropertyName = 'name'), 'Filtered property inspection returns original layout metadata');
    Old := Request; Request := Inspector.RequestShape(Model.Session,1,42);
    Check(not Inspector.PublishSnapshot(Old,Snapshot) and not Inspector.ShapeMap.ActivateNode(1), 'New request rejects late reply and disables stale diagram navigation');
    Check(Inspector.FailSnapshot(Request,'provider failed') and Inspector.Stale and (Inspector.ErrorText = 'provider failed'), 'Failure retains visibly stale data');
    Request := Inspector.RequestShape(Model.Session,2,42); Snapshot.Info.TransitionCountAvailable := False;
    Snapshot.Transitions := nil; Snapshot.TransitionsAvailability := shaUnavailable;
    Check(Inspector.PublishSnapshot(Request,Snapshot) and (Inspector.ShapeMap.NodeCount = 3), 'Unavailable transition endpoint does not invent an empty known graph');
    Inspector.SearchText := ''; Check(Inspector.TransitionTree.ValueByID('status',Value) and (Value.Availability = vaUnavailable), 'Unavailable transition data has an explicit detail status');
    Model.SetExecution(dsRunning); Check(Inspector.Stale, 'Execution-state change invalidates live shape observations');
    Request := Inspector.RequestShape(Model.Session,3,42); Model.BeginSession;
    Check(not Inspector.PublishSnapshot(Request,Snapshot), 'Session reset rejects outstanding shape snapshot');
    Model.SetExecution(dsPaused); Request := Inspector.RequestShape(Model.Session,0,42); Check(Inspector.PublishSnapshot(Request,Snapshot), 'Fresh session permits generation restart');
    Inspector.OnRequestShape := nil; Inspector.OnNavigateShape := nil; Inspector.OnInspectProperty := nil; Inspector.DebugModel := nil;
    Inspector.MaxItems := 20; Inspector.SearchText := 'name'; RegisterClass(TERDShapeInspector); CopyInspector := TERDShapeInspector.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Inspector); Stream.Position := 0; Stream.ReadComponent(CopyInspector);
      Check((CopyInspector.MaxItems = 20) and CopyInspector.Stale and (CopyInspector.SearchText = '') and (CopyInspector.ShapeMap.NodeCount = 0) and
        (CopyInspector.PropertyTree.RootNodeCount = 0) and (CopyInspector.ComponentCount = 5), 'DFM retains limits without graph data, filters or duplicate internal controls');
    finally Stream.Free; CopyInspector.Free; end;
    Inspector.DebugModel := Model; FreeAndNil(Model); Check((Inspector.DebugModel = nil) and Inspector.Stale, 'Destroyed debug model safely detaches shape inspector');
  finally Inspector.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestDebugConsole;
var Form: TForm; Console, CopyConsole: TERDDebugConsole; Model: TERDDebugModel; Probe: TConsoleProbe;
    Entry: TERDConsoleEntry; Entries: TArray<TERDConsoleEntry>; History: TArray<string>;
    Node: PVirtualNode; ID: Int64; Key: Word; Stream: TMemoryStream; Raised: Boolean;
begin
  Form := TForm.Create(nil); Console := TERDDebugConsole.Create(Form); Console.Parent := Form;
  Model := TERDDebugModel.Create(nil); Probe := TConsoleProbe.Create;
  try
    Console.BeginSession(7); Console.MaxEntries := 2;
    ID := Console.AddEntry(7,Now,dlsInfo,'stdout','first');
    Console.AddEntry(7,Now,dlsWarning,'host','second'); Console.AddEntry(7,Now,dlsError,'stderr','third');
    Check((Console.Count = 2) and (Console.EntryAt(0).Text = 'second') and (Console.EntryAt(1).ID > ID), 'Console evicts oldest entries while preserving arrival IDs');
    Check(Console.AddEntry(6,Now,dlsInfo,'stdout','late') = 0, 'Console rejects prior-session messages');
    Console.Paused := True; Console.AddEntry(7,Now,dlsDebug,'host','fourth'); Console.AddEntry(7,Now,dlsTrace,'host','fifth');
    Check((Console.PendingCount = 2) and Console.VisibleEntry(0,Entry) and (Entry.Text = 'second'), 'Paused console retains readable snapshot despite live ring eviction');
    Console.Levels := [dlsError]; Check((Console.VisibleCount = 1) and Console.VisibleEntry(0,Entry) and (Entry.Text = 'third'), 'Paused filters operate on frozen entries');
    Console.Paused := False; Check((Console.VisibleCount = 0) and (Console.PendingCount = 0), 'Resume publishes newest retained buffer using current filters');
    Console.Levels := [dlsTrace,dlsDebug,dlsInfo,dlsWarning,dlsError]; Console.SearchText := 'FIFTH'; Console.ChannelFilter := 'HOST';
    Check(Console.VisibleCount = 1, 'Console combines case-insensitive search and channel filters');
    Console.SearchText := ''; Console.ChannelFilter := ''; Console.Clear; Console.MaxEntries := 10; Console.MaxTextChars := 256;
    Console.AddEntry(7,Now,dlsInfo,'x',StringOfChar('a',150)); Console.AddEntry(7,Now,dlsInfo,'x',StringOfChar('b',150));
    Check((Console.Count = 1) and (Console.TextChars = 151) and (Console.EntryAt(0).Text[1] = 'b'), 'Text budget evicts oldest even below entry-count limit');
    Console.Clear; Console.MaxEntryChars := 16;
    Console.AddEntry(7,Now,dlsInfo,'x',StringOfChar('a',15)+Char($D83D)+Char($DE00)+'tail');
    Entry := Console.EntryAt(0); Check(Entry.Truncated and (Length(Entry.Text) = 15), 'Truncation never retains half a UTF-16 surrogate pair');
    Console.Clear; Console.MaxEntryChars := 65536; Console.MaxTextChars := 4096;
    Console.AddEntry(7,Now,dlsInfo,'stdout','one'+#13#10+'two'+#13+'three');
    Entry := Console.EntryAt(0); Check(Entry.Text = 'one'+#10+'two'+#10+'three', 'Console normalizes multiline messages without losing content');
    Node := Console.EntryTree.GetFirst; Console.EntryTree.Selected[Node] := True; Console.EntryTree.FocusedNode := Node;
    Console.OnInspectEntry := Probe.Inspect; Console.InspectSelected;
    Check((Probe.Inspections = 1) and (Probe.Text = Entry.Text) and (Pos('one'+sLineBreak+'two',Console.SelectedText) > 0), 'Copy and inspect expose full retained multiline content');
    ID := Entry.ID; Console.AddEntry(7,Now,dlsInfo,'host','next');
    Check(Console.SelectedEntry(Entry) and (Entry.ID = ID) and (Console.EntryTree.SelectedCount = 1), 'Appending preserves focused and selected entries by stable ID');
    SetLength(Entries,2); Entries[0].Session := 7; Entries[0].Timestamp := Now; Entries[0].Text := 'valid';
    Entries[1] := Entries[0]; Entries[1].Timestamp := NaN; Raised := False;
    try Console.AddEntries(Entries); except on E: EArgumentException do Raised := True; end;
    Check(Raised and (Console.Count = 2), 'Invalid batch is rejected before any entry is appended');
    Entries[1].Timestamp := Now; Console.AddEntries(Entries); Check(Console.VisibleCount = 4, 'Batch publishes all valid entries');
    Console.BeginUpdate; Console.AddEntry(7,Now,dlsInfo,'host','batched');
    Check(Console.VisibleCount = 4, 'Explicit append batch defers view rebuild'); Console.EndUpdate;
    Check(Console.VisibleCount = 5, 'EndUpdate publishes deferred entries');
    Raised := False; try Console.EndUpdate; except on E: EInvalidOperation do Raised := True; end;
    Check(Raised, 'Unbalanced update nesting is rejected');
    Console.ShowInput := True; Console.CommandEditor.Text := 'draft';
    Check(not Console.SubmitCommand('help') and (Console.CommandEditor.Text = 'draft'), 'Missing command handler retains draft and executes nothing');
    Console.OnCommand := Probe.Command; Console.HistoryLimit := 2;
    Check(Console.SubmitCommand(' help ') and (Probe.Calls = 1) and (Probe.Session = 7) and (Probe.Text = 'help'), 'Commands are trimmed and dispatched only to the host event');
    Console.SubmitCommand('help'); Check(Length(Console.CommandHistory) = 1, 'Consecutive duplicate commands do not duplicate history');
    Console.SubmitCommand('second'); Console.SubmitCommand('third'); History := Console.CommandHistory;
    Check((Length(History) = 2) and (History[0] = 'second') and (History[1] = 'third'), 'Command history retains only configured newest commands');
    Console.CommandEditor.Text := 'unfinished'; Key := VK_UP; Console.CommandEditor.OnKeyDown(Console.CommandEditor,Key,[]);
    Check((Key = 0) and (Console.CommandEditor.Text = 'third'), 'Up recalls latest command');
    Key := VK_UP; Console.CommandEditor.OnKeyDown(Console.CommandEditor,Key,[]); Check(Console.CommandEditor.Text = 'second', 'Up navigates older command');
    Key := VK_DOWN; Console.CommandEditor.OnKeyDown(Console.CommandEditor,Key,[]); Key := VK_DOWN; Console.CommandEditor.OnKeyDown(Console.CommandEditor,Key,[]);
    Check(Console.CommandEditor.Text = 'unfinished', 'Down restores unsent draft after browsing history');
    Check(not Console.SubmitCommand('a'+#10+'b'), 'Command entry refuses multiline execution');
    Console.DebugModel := Model; Check(not Console.SubmitCommand('help'), 'Disconnected model blocks command dispatch');
    Model.SetExecution(dsRunning); Check(Console.SubmitCommand('help') and (Probe.Session = Model.Session), 'Connected model session is included in host command');
    Model.BeginSession; Check(Console.ActiveSession = Model.Session, 'Console follows model session changes');
    Model.SetExecution(dsDisconnected); Check(Console.AddEntry(Model.Session,Now,dlsInfo,'host','shutdown') > 0, 'Disconnected session still accepts final shutdown logs');
    Console.OnCommand := nil; Console.OnInspectEntry := nil; Console.DebugModel := nil;
    Console.SearchText := 'shutdown'; Console.Paused := True; Console.ShowInput := True;
    RegisterClass(TERDDebugConsole); CopyConsole := TERDDebugConsole.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Console); Stream.Position := 0; Stream.ReadComponent(CopyConsole);
      Check((CopyConsole.MaxEntries = 10) and (CopyConsole.MaxTextChars = 4096) and (CopyConsole.HistoryLimit = 2) and CopyConsole.ShowInput and
        (CopyConsole.Count = 0) and not CopyConsole.Paused and (CopyConsole.SearchText = '') and (Length(CopyConsole.CommandHistory) = 0) and (CopyConsole.ComponentCount = 6), 'DFM retains configuration without logs, history, filters or duplicate internals');
    finally Stream.Free; CopyConsole.Free; end;
    Console.DebugModel := Model; FreeAndNil(Model); Check((Console.DebugModel = nil) and not Console.CommandEditor.Enabled, 'Model destruction detaches console and disables commands');
    Check(Console.BeginSession(9) and Console.CommandEditor.Enabled, 'Explicit host session restores input after model destruction');
  finally Console.Free; Probe.Free; Model.Free; Form.Free; end;
end;

procedure TestValueTreeSnapshots;
var Form: TForm; Tree, CopyTree: TERDValueTree; Probe: TValueChildrenProbe;
    Roots, Children: TArray<TERDInspectValue>; Request, Old: TERDValueRequest; Value: TERDInspectValue; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Probe := TValueChildrenProbe.Create; Stream := TMemoryStream.Create;
  try
    Tree := TERDValueTree.Create(Form); Tree.Parent := Form; Tree.PageSize := 2; Tree.OnRequestChildren := Probe.Request;
    SetLength(Roots,1); Roots[0].ID := 'object'; Roots[0].Name := 'object'; Roots[0].HasChildren := True;
    Check(Tree.LoadSnapshot('session-a',1,Roots), 'First value snapshot is accepted');
    Tree.Expanded[Tree.GetFirst] := True; Request := Probe.Last;
    Check((Probe.Calls = 1) and (Request.StartIndex = 0) and (Request.Count = 2), 'Expanding requests only the first child page');
    SetLength(Children,2); Children[0].ID := 'a'; Children[0].Name := 'a'; Children[0].ValueText := '1';
    Children[1].ID := 'b'; Children[1].Name := 'b'; Children[1].ValueText := '2';
    Check(Tree.PublishChildren(Request,Children,True) and (Tree.ChildCount[Tree.GetFirst] = 3), 'Child page adds a load-more row without eager enumeration');
    Check(not Tree.PublishChildren(Request,Children,True), 'Duplicate page reply cannot add duplicate rows');
    Tree.RequestChildren('object'); Request := Probe.Last;
    Check(Request.StartIndex = 2, 'Next request starts after the loaded page');
    SetLength(Children,1); Children[0].ID := 'c'; Children[0].Name := 'c';
    Check(Tree.PublishChildren(Request,Children,False) and (Tree.ChildCount[Tree.GetFirst] = 3), 'Final page replaces the load-more row');
    Check(Tree.RequestChildren('object') = 0, 'A fully loaded node does not request redundant pages');
    Old := Request; Check(Tree.LoadSnapshot('session-a',2,Roots), 'New generation refreshes values'); Request := Probe.Last;
    Check(not Tree.PublishChildren(Old,Children,False), 'Old snapshot pages are rejected');
    Tree.MarkStale;
    Check(not Tree.PublishChildren(Request,Children,False) and Tree.ValueByID('object',Value) and (Value.Availability = vaStale), 'Resume makes retained values stale and rejects outstanding requests');
    Probe.Synchronous := True; Probe.Calls := 0; Tree.LoadSnapshot('session-b',0,Roots);
    Tree.Expanded[Tree.GetFirst] := True;
    Check((Probe.Calls = 1) and Tree.Expanded[Tree.GetFirst] and (Tree.ChildCount[Tree.GetFirst] = 1), 'Synchronous demo provider can publish while expanding without reentering expansion');
    Tree.OnRequestChildren := nil; RegisterClass(TERDValueTree); CopyTree := TERDValueTree.Create(Form);
    try
      Stream.WriteComponent(Tree); Stream.Position := 0; Stream.ReadComponent(CopyTree);
      Check((CopyTree.RootNodeCount = 0) and (CopyTree.PageSize = 2), 'DFM keeps value-tree settings without persisting runtime nodes');
    finally CopyTree.Free; end;
  finally Stream.Free; Probe.Free; Form.Free; end;
end;
procedure TDocumentEvents.Save(Sender: TObject; const Value: TERDDocumentSaveRequest);
begin Inc(Saves); Request := Value; end;
procedure TDocumentEvents.Closing(Sender: TObject; Document: TERDDocument; var AllowClose: Boolean);
begin
  Inc(Closes); if AllowDirtyClose then AllowClose := True;
  if DestroyContentOnClose then Document.Control.Free;
end;

procedure TestDocumentHost;
var Form: TForm; Host, CopyHost: TERDDocumentHost; Events: TDocumentEvents;
    A, B: TERDDocument; Editor: TERDCodeEditor; Page: TERDPanel;
    Request, OldRequest: TERDDocumentSaveRequest; Bounds: TRect; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Host := TERDDocumentHost.Create(Form);
  Events := TDocumentEvents.Create; Stream := TMemoryStream.Create;
  try
    Host.Parent := Form; Host.Align := alClient; Form.SetBounds(20,20,900,600);
    Host.OnSave := Events.Save; Host.OnClosing := Events.Closing;
    A := Host.OpenCode('demo://a.ts','a.ts','function a() {'+#13#10+'  return 1;'+#13#10+'}');
    Editor := A.Editor; Editor.SelStart := 9; Editor.SelLength := 2;
    B := Host.OpenCode('demo://b.ts','b.ts','const b = 2;',True);
    Check((Host.DocumentCount = 2) and B.ReadOnly and B.Editor.ReadOnly,
      'Document host opens independent editors and applies read-only state');
    Check(Host.OpenCode(A.ID,'ignored','overwrite') = A, 'Opening an existing ID activates its retained document');
    Check((A.Editor = Editor) and (Editor.SelStart = 9) and (Editor.SelLength = 2) and
      (Pos('function a()',Editor.Text) = 1), 'Tab switching preserves editor, selection and source');
    Host.TabStrip.MoveTab(0,1); Host.ActivateDocument(B.ID);
    Check(Host.ActiveDocument = B, 'Stable document identity survives tab reordering');
    Check(not Host.SaveDocument(B.ID), 'Read-only documents cannot request saves');
    Editor.Text := 'changed'; Check(A.Modified, 'Source edits mark document dirty');
    Check(Host.SaveDocument(A.ID) and (Events.Saves = 1), 'Save delegates a copied source snapshot');
    Request := Events.Request; Editor.Text := 'newer';
    Check(Host.CompleteSave(Request) and A.Modified, 'Acknowledging an older revision retains newer changes');
    Check(not Host.CompleteSave(Request), 'Save acknowledgment is consumed once');
    Editor.Text := Request.Text; Check(not A.Modified, 'Returning to saved source clears dirty state');
    Editor.Text := 'unsaved'; Host.CloseDocument(A.ID);
    Check(Host.DocumentCount = 2, 'Close requests do not free controls inside tab callbacks');
    Application.ProcessMessages;
    Check((Host.FindDocument(A.ID) = A) and (Events.Closes = 1), 'Dirty close is vetoed by default');
    Host.SaveDocument(A.ID); Request := Events.Request;
    Check(Host.CompleteSave(Request) and not A.Modified, 'Successful save updates the baseline');
    Host.CloseDocument(A.ID); Application.ProcessMessages;
    Check(Host.FindDocument('demo://a.ts') = nil, 'Clean document closes after dispatch');
    OldRequest := Request; A := Host.OpenCode('demo://a.ts','a.ts','reopened');
    Host.SaveDocument(A.ID);
    Check(not Host.CompleteSave(OldRequest), 'A reply for a closed instance cannot clean a reopened ID');
    Request := Events.Request; Host.CompleteSave(Request);
    Editor := A.Editor; Host.CloseDocument(A.ID); Editor.Text := 'edit before queued close';
    Application.ProcessMessages;
    Check(Host.FindDocument('demo://a.ts') = A, 'Queued close checks the current dirty state');
    Events.AllowDirtyClose := True; Host.TabStrip.CloseTab(Host.TabStrip.TabIndex);
    Application.ProcessMessages;
    Check(Host.FindDocument('demo://a.ts') = nil, 'Host may explicitly authorize discarding a dirty document');
    Page := TERDPanel.Create(Form); Page.Parent := Form; Page.SetBounds(12,16,140,80);
    Page.Visible := False; Bounds := Page.BoundsRect;
    Host.AddDocument('pcb','PCB',Page); Host.CloseDocument('pcb'); Application.ProcessMessages;
    Check((Page.Parent = Form) and EqualRect(Page.BoundsRect,Bounds) and not Page.Visible,
      'Closing a borrowed document restores its parent, bounds and visibility');
    Host.AddDocument('pcb','PCB',Page); Events.DestroyContentOnClose := True;
    Host.CloseDocument('pcb'); Application.ProcessMessages;
    Check(Host.FindDocument('pcb') = nil, 'Closing callback may destroy borrowed content without a dangling document access');
    Events.DestroyContentOnClose := False;
    Page := TERDPanel.Create(Form); Host.AddDocument('pcb','PCB',Page); Page.Free;
    Check(Host.FindDocument('pcb') = nil, 'Destroying borrowed content removes its document safely');
    Host.OnSave := nil; Host.OnClosing := nil;
    RegisterClass(TERDDocumentHost); CopyHost := TERDDocumentHost.Create(Form);
    try
      Stream.WriteComponent(Host); Stream.Position := 0; Stream.ReadComponent(CopyHost);
      Check((CopyHost.DocumentCount = 0) and (CopyHost.ComponentCount = 2),
        'DFM excludes runtime documents and reconstructs only its internal containers');
    finally CopyHost.Free; end;
  finally Stream.Free; Events.Free; Host.OnSave := nil; Host.OnClosing := nil; Form.Free; end;
end;

procedure TestProjectTree;
var Form: TForm; Tree, CopyTree: TProjectTreeProbe; Events: TProjectEvents;
    Items, Bad: TArray<TERDProjectItem>; Item: TERDProjectItem; Node: PVirtualNode;
    R: TRect; P: TPoint; Stream: TMemoryStream; Hint: TERDHintContent; Revision: Int64; WasExpanded: Boolean;
  procedure Add(Index: Integer; const ID, ParentID, Name: string; Kind: TERDProjectItemKind; Status: TERDProjectItemStatuses);
  begin
    Items[Index].ID := ID; Items[Index].ParentID := ParentID; Items[Index].Name := Name;
    Items[Index].Uri := 'demo://'+Name; Items[Index].Kind := Kind; Items[Index].Status := Status;
    Items[Index].CanRename := Kind <> pikProject; Items[Index].CanMove := Kind <> pikProject;
  end;
begin
  Form := TForm.Create(nil); Form.SetBounds(30,30,900,700); Tree := TProjectTreeProbe.Create(Form); Tree.Parent := Form; Tree.Align := alClient;
  Events := TProjectEvents.Create; Events.Tree := Tree;
  try
    Check(Tree.Stale and (Tree.ItemCount = 0) and Tree.ParentDensity, 'Project tree starts empty and inherits ERD density');
    SetLength(Items,7); Add(0,'root','','Demo',pikProject,[]); Add(1,'src','root','src',pikFolder,[]);
    Add(2,'assets','root','assets',pikFolder,[]); Add(3,'main','src','main.ts',pikTypeScript,[pisModified,pisError]);
    Add(4,'board','src','board.ts',pikTypeScript,[]); Add(5,'template','assets','board.json',pikPCBTemplate,[pisReadOnly]);
    Add(6,'missing','assets','missing.json',pikResource,[pisMissing]);
    Check(Tree.SetItems('project',1,Items) and (Tree.ItemCount = 7) and (Tree.RootNodeCount = 1), 'Validated project snapshot builds a hierarchy');
    Items[3].Name := 'caller change'; Tree.FindItem('main',Item); Check(Item.Name = 'main.ts', 'Project snapshots copy caller records'); Items[3].Name := 'main.ts';
    Tree.OnOpenItem := Events.Open; Tree.OnRenameItem := Events.Rename; Tree.OnMoveItem := Events.Move; Events.Items := Copy(Items);
    Check(not Tree.SetItems('project',1,Items), 'Equal project revisions cannot overwrite current data');
    Bad := Copy(Items); Bad[1].ParentID := 'unknown'; Check(not Tree.SetItems('project',2,Bad), 'Missing project parents are rejected atomically');
    Bad := Copy(Items); Bad[1].ParentID := 'src'; Check(not Tree.SetItems('project',2,Bad), 'Project parent cycles are rejected');
    Bad := Copy(Items); Bad[1].ParentID := 'main'; Check(not Tree.SetItems('project',2,Bad), 'Files cannot contain child project items');
    Bad := Copy(Items); Bad[4].ID := 'main'; Check(not Tree.SetItems('project',2,Bad), 'Duplicate project IDs are rejected');
    Check((Tree.Context.Revision = 1) and (Tree.ItemCount = 7), 'Rejected project snapshots preserve state');
    Tree.SelectItem('src'); Node := Tree.FocusedNode; Tree.Expanded[Node] := False;
    Tree.SearchText := 'main'; Check((Tree.VisibleItemCount = 3) and Tree.SelectItem('main'), 'Project search retains ancestor context and reveals matches');
    Tree.ClearSelection; Tree.FocusedNode := nil; Tree.SearchText := ''; Tree.SelectItem('root'); Node := Tree.GetFirstChild(Tree.GetFirst);
    Check(not Tree.Expanded[Node], 'Clearing project search restores pre-search collapse state');
    Tree.SelectItem('main'); Check(Tree.SelectedItem(Item) and (Item.ID = 'main'), 'Project selection resolves stable IDs');
    Tree.SearchText := 'Modified'; Check((Tree.VisibleItemCount = 3) and Tree.SelectedItem(Item), 'Project status text participates in filtering'); Tree.SearchText := '';
    Form.Show; Application.ProcessMessages; Tree.SelectItem('main'); R := Tree.GetDisplayRect(Tree.FocusedNode,0,False);
    Check(Tree.HintContentAt(Tree.ClientToScreen(Point(R.Left+10,(R.Top+R.Bottom) div 2)),Hint) and
      (Hint.Title = 'main.ts') and (Pos('Modified',Hint.Description) > 0) and Hint.HasDensity, 'Project hints include status and the effective density');
    Tree.SelectItem('src'); Node := Tree.FocusedNode; WasExpanded := Tree.Expanded[Node]; R := Tree.GetDisplayRect(Node,0,False);
    SendMessage(Tree.Handle,WM_LBUTTONDBLCLK,MK_LBUTTON,LPARAM((R.Left+120) or (((R.Top+R.Bottom) div 2) shl 16)));
    Check(Tree.Expanded[Node] <> WasExpanded, 'Native project folder double-click toggles once without engine double-toggling');
    Tree.SelectItem('main');
    Tree.OpenSelected; Check(Events.Opens = 0, 'Open events wait until the native UI callback returns'); Application.ProcessMessages;
    Check((Events.Opens = 1) and (Events.LastID = 'main'), 'Project open event identifies the chosen resource');
    Tree.SelectItem('missing'); Tree.OpenSelected; Application.ProcessMessages; Check(Events.Opens = 1, 'Missing resources are not opened');
    Check(not Tree.RequestRename('template','new.json') and not Tree.RequestRename('main','../bad.ts') and
      not Tree.RequestRename('main','board.ts'), 'Project renaming rejects read-only items, path separators and sibling collisions');
    Check(not Tree.CanMoveTo('src','src') and not Tree.CanMoveTo('src','main') and not Tree.CanMoveTo('root','src') and
      not Tree.CanMoveTo('template','src'), 'Project moves reject cycles, file targets and read-only sources');
    Check(Tree.RequestRename('main','renamed.ts'), 'A valid rename queues a host request'); Tree.FindItem('main',Item);
    Check((Events.Renames = 0) and (Item.Name = 'main.ts'), 'Queued rename does not optimistically change project data');
    Application.ProcessMessages; Tree.FindItem('main',Item);
    Check((Events.Renames = 1) and (Item.Name = 'main.ts'), 'Host may decline a rename by keeping the snapshot unchanged');
    Events.Apply := True; Tree.SelectItem('main'); Tree.EditName('renamed.ts'); Application.ProcessMessages;
    Check(Tree.FindItem('main',Item) and (Item.Name = 'renamed.ts') and (Tree.Context.Revision = 2), 'Host can synchronously publish an approved inline rename from the deferred event');
    Tree.SelectItem('main'); Tree.StartMove; Tree.SelectItem('assets'); R := Tree.GetDisplayRect(Tree.FocusedNode,0,False); P := R.CenterPoint;
    Check(not Tree.DropAt(P,False,Form) and Tree.DropAt(P,True,Tree), 'Internal project drag accepts container targets and rejects foreign sources');
    Check(Events.Moves = 0, 'Project drop defers host mutation until the drag callback returns'); Application.ProcessMessages;
    Check((Events.Moves = 1) and Tree.FindItem('main',Item) and (Item.ParentID = 'assets'), 'Project drop retains the original drag ID when target selection changes');
    Tree.SelectItem('main'); Tree.StartMove; Revision := Tree.Context.Revision;
    Check(Tree.SetItems('project',Revision+1,Events.Items), 'Project revision may change during a drag'); Tree.SelectItem('src'); R := Tree.GetDisplayRect(Tree.FocusedNode,0,False);
    Check(not Tree.DropAt(R.CenterPoint,True,Tree), 'A replaced project snapshot invalidates the captured drag');
    Check(Tree.RequestRename('main','late.ts'), 'A rename can be pending'); Tree.MarkStale; Application.ProcessMessages;
    Check((Events.Renames = 2) and not Tree.RequestMove('main','src') and not Tree.SelectedItem(Item), 'Stale project state cancels queued commands and disables interactions');
    Tree.OnOpenItem := nil; Tree.OnRenameItem := nil; Tree.OnMoveItem := nil;
    RegisterClass(TProjectTreeProbe); CopyTree := TProjectTreeProbe.Create(Form); Stream := TMemoryStream.Create;
    try
      Tree.SearchText := 'main'; Stream.WriteComponent(Tree); Stream.Position := 0; Stream.ReadComponent(CopyTree);
      Check((CopyTree.ItemCount = 0) and (CopyTree.RootNodeCount = 0) and (CopyTree.SearchText = '') and CopyTree.Stale and (CopyTree.ComponentCount = 4),
        'DFM excludes runtime project snapshots, nodes, filters and queued commands');
    finally Stream.Free; CopyTree.Free; end;
    Tree.ClearProject; Check((Tree.ItemCount = 0) and (Tree.Context.ProjectID = ''), 'Project clear removes model and selection');
  finally Form.Free; Events.Free; end;
end;

procedure TestDiagnosticList;
var Form: TForm; List: TDiagnosticKeyProbe; CopyList: TERDDiagnosticList;
    Probe: TDiagnosticProbe; Values, Invalid: TArray<TERDDiagnostic>;
    Request, Old, Next: TERDDiagnosticRequest; Value: TERDDiagnostic;
    Root, Node: PVirtualNode; Stream: TMemoryStream; Key: Word; Count: Integer;
    R: TRect; Content: TERDHintContent; Text: string;
  procedure Sample(Index: Integer; const ID, Uri, MessageText: string; Severity: TERDDiagnosticSeverity; Line, Column: Integer);
  begin
    Values[Index].ID := ID; Values[Index].DocumentUri := Uri; Values[Index].MessageText := MessageText;
    Values[Index].Severity := Severity; Values[Index].Line := Line; Values[Index].Column := Column;
    Values[Index].Origin := 'fixture'; Values[Index].Code := 'E'+IntToStr(Index);
  end;
begin
  Form := TForm.Create(nil); Probe := TDiagnosticProbe.Create;
  List := TDiagnosticKeyProbe.Create(Form); List.Parent := Form; List.Align := alClient;
  Form.SetBounds(40,40,1000,720);
  try
    List.OnNavigate := Probe.Navigate; List.OnChange := Probe.Changed; List.OnSelectionChanged := Probe.Selected;
    Check(List.Stale and not List.Pending and List.GroupByFile and (List.DiagnosticCount = 0), 'Diagnostics start unavailable with no invented messages');
    Request := List.BeginSnapshot(-1,0); Check(Request.RequestID = 0, 'Invalid diagnostic session is rejected');
    Request := List.BeginSnapshot(10,0); Old := Request;
    Check(List.Pending and List.Stale and (Request.RequestID > 0), 'Diagnostics request distinguishes loading from an empty result');
    Request := List.BeginSnapshot(10,0); Check(Request.RequestID = 0, 'Duplicate diagnostics generation is rejected');
    Next := List.BeginSnapshot(10,1); SetLength(Values,5);
    Sample(0,'error1','demo://a.ts','Missing output name',dgsError,5,3);
    Sample(1,'warning1','demo://a.ts','Unused value',dgsWarning,2,7);
    Sample(2,'info1','demo://b.ts','Template information',dgsInformation,1,1);
    Sample(3,'hint1','demo://b.ts','Document this name',dgsHint,3,1);
    Sample(4,'project','','Compiler fixture',dgsInformation,0,0);
    Check(not List.PublishSnapshot(Old,Values) and (List.DiagnosticCount = 0), 'Replaced diagnostics request cannot publish');
    Check(List.PublishSnapshot(Next,Values) and not List.Stale and not List.Pending, 'Current diagnostic snapshot publishes');
    Check((List.DiagnosticCount = 5) and (List.VisibleDiagnosticCount = 5) and (List.RootNodeCount = 3), 'Diagnostics group source files and project-wide messages separately');
    Check((List.SeverityCount(dgsError) = 1) and (List.SeverityCount(dgsInformation) = 2), 'Diagnostic severity totals preserve raw snapshot counts');
    Values[0].MessageText := 'Caller mutation'; Value := List.Diagnostic(0);
    Check(Value.MessageText = 'Missing output name', 'Diagnostic snapshots own copies of records'); Values[0].MessageText := 'Missing output name';
    Check(List.SelectDiagnostic('warning1') and List.SelectedDiagnostic(Value) and (Value.Line = 2), 'Diagnostic selection uses stable IDs');
    List.NavigateSelected; Check((Probe.Navigations = 1) and (Probe.ID = 'warning1') and (Probe.Context.RequestID = Next.RequestID), 'Navigation includes snapshot identity and source range');
    List.GroupByFile := False;
    Check((List.RootNodeCount = 5) and List.SelectedDiagnostic(Value) and (Value.ID = 'warning1'), 'Flat mode preserves stable selection');
    List.GroupByFile := True; Check(List.SelectedDiagnostic(Value) and (Value.ID = 'warning1'), 'File grouping preserves stable selection');
    List.SearchText := 'output a.ts';
    Check((List.VisibleDiagnosticCount = 1) and (List.RootNodeCount = 1), 'Diagnostic search ANDs words across message and URI');
    List.Severities := [dgsWarning];
    Check((List.VisibleDiagnosticCount = 0) and (List.SeverityCount(dgsError) = 1) and (List.SeverityCount(dgsError,True) = 0), 'Diagnostic search and severity filtering compose without changing totals');
    List.SearchText := ''; Check(List.VisibleDiagnosticCount = 1, 'Severity filter survives search clearing');
    List.Severities := []; Check(List.VisibleDiagnosticCount = 0, 'Empty diagnostic severity set hides all messages');
    List.Severities := [dgsError,dgsWarning,dgsInformation,dgsHint];
    List.ClearSelection; List.FocusedNode := nil; Root := List.GetFirst; List.Expanded[Root] := False;
    List.SearchText := 'fixture'; Check(not List.Expanded[List.GetFirst], 'Collapsed file groups survive matching filter rebuilds');
    List.SearchText := ''; List.SelectDiagnostic('error1'); Check(List.Expanded[List.GetFirst], 'Selecting a child reveals its file group');
    Form.Show; Application.ProcessMessages;
    Root := List.GetFirst; Node := List.GetFirstChild(Root); R := List.GetDisplayRect(Node,0,False); Count := Probe.Navigations;
    Check(List.HintContentAt(List.ClientToScreen(Point(R.Left+10,(R.Top+R.Bottom) div 2)),Content) and
      (Content.Title = 'Error E0') and (Content.Glyph = glClose) and (Pos('Missing output name',Content.Description) > 0), 'Diagnostic row provides automatic Office hint metadata');
    Check(Probe.Navigations = Count, 'Hint queries never navigate or request data');
    Check(List.NavigateNext and (Probe.ID = 'warning1'), 'Next diagnostic follows filtered tree order');
    Key := VK_F8; List.Press(Key,[ssShift]); Check((Key = 0) and (Probe.ID = 'error1'), 'Shift+F8 navigates backward');
    List.SelectDiagnostic('project'); Count := Probe.Navigations; List.NavigateSelected;
    Check(Probe.Navigations = Count, 'Project-level messages without a source location never navigate');
    List.NavigateNext; Check(Probe.ID = 'error1', 'Next diagnostic wraps and skips locationless messages');
    List.SearchText := 'Unused'; Text := List.ExportText;
    Check((Pos('Unused value',Text) > 0) and (Pos('Missing output name',Text) = 0) and (Pos('Missing output name',List.ExportText(False)) > 0), 'Diagnostic export distinguishes filtered and complete snapshots');
    List.SearchText := ''; List.SelectDiagnostic('warning1'); Request := List.BeginSnapshot(10,2);
    Check(List.Stale and not List.SelectedDiagnostic(Value), 'Refreshing diagnostics retains visible rows but disables navigation');
    Count := Probe.Navigations; List.NavigateSelected; Check(not List.NavigateNext and (Probe.Navigations = Count), 'Stale diagnostics cannot navigate');
    Invalid := Copy(Values); Invalid[1].ID := Invalid[0].ID; Count := Probe.Changes;
    Check(not List.PublishSnapshot(Request,Invalid) and List.Pending and (List.DiagnosticCount = 5) and (Probe.Changes = Count), 'Duplicate diagnostic IDs are rejected atomically');
    Invalid := Copy(Values); Invalid[0].EndLine := 4; Check(not List.PublishSnapshot(Request,Invalid), 'Reversed diagnostic source ranges are rejected');
    Invalid := Copy(Values); Invalid[0].Column := -1; Check(not List.PublishSnapshot(Request,Invalid), 'Negative diagnostic coordinates are rejected');
    Invalid := Copy(Values); Invalid[0].MessageText := ' '; Check(not List.PublishSnapshot(Request,Invalid), 'Blank diagnostic descriptions are rejected');
    Check(List.PublishSnapshot(Request,Values) and List.SelectedDiagnostic(Value) and (Value.ID = 'warning1'), 'Valid replacement retains same-session stable selection');
    Next := List.BeginSnapshot(11,0); Check(not List.FailSnapshot(Request,'Old error'), 'Old diagnostics failure cannot replace current status');
    Check(List.FailSnapshot(Next,'Compiler offline') and List.Stale and not List.Pending and (List.ErrorText = 'Compiler offline') and (List.DiagnosticCount = 5), 'Failed diagnostics retain clearly stale previous results');
    Check(List.HintContentAt(List.ClientToScreen(Point(30,List.ClientHeight-10)),Content) and
      (Content.Title = 'Diagnostics unavailable') and (Content.Description = 'Compiler offline'), 'Unavailable diagnostics expose the failure reason in an Office hint');
    Check(not List.PublishSnapshot(Next,Values), 'Completed failed requests cannot publish late success');
    Next := List.BeginSnapshot(11,1); Check(List.PublishSnapshot(Next,nil) and not List.Stale and (List.DiagnosticCount = 0), 'Successful empty diagnostics are available, not stale');
    Next := List.BeginSnapshot(11,2); Check(List.PublishSnapshot(Next,Values), 'Later generation can publish diagnostics');
    List.SelectDiagnostic('error1'); List.ClearDiagnostics;
    Check(List.Stale and (List.DiagnosticCount = 0) and not List.SelectedDiagnostic(Value), 'Explicit diagnostics clear invalidates results and selection');
    Value := Values[1]; Values[1] := Values[2]; Values[2] := Value;
    Next := List.BeginSnapshot(11,3); List.PublishSnapshot(Next,Values); List.SelectDiagnostic('error1');
    Check(List.NavigateNext and (Probe.ID = 'warning1'), 'Grouped navigation follows displayed groups when input is interleaved');
    List.OnNavigate := nil; List.OnChange := nil; List.OnSelectionChanged := nil;
    List.SearchText := 'Unused'; List.GroupByFile := False; List.Severities := [dgsWarning];
    RegisterClass(TDiagnosticKeyProbe); CopyList := TDiagnosticKeyProbe.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(List); Stream.Position := 0; Stream.ReadComponent(CopyList);
      Check(not CopyList.GroupByFile and (CopyList.Severities = [dgsWarning]) and (CopyList.SearchText = '') and
        (CopyList.DiagnosticCount = 0) and (CopyList.RootNodeCount = 0) and CopyList.Stale, 'DFM stores configuration without snapshots, nodes or search state');
    finally Stream.Free; CopyList.Free; end;
  finally Form.Free; Probe.Free; end;
end;

procedure TestDockMouseDrop;
var Form: TForm; Host: TERDDockHost; A: TERDPanel; B: TERDEdit;
    Tabs: TERDDockTabs; Target: TPoint; Before: string; Window: TCustomForm;
  function MousePosition(const P: TPoint): LPARAM;
  begin Result := LPARAM(Cardinal(Word(P.X)) or (Cardinal(Word(P.Y)) shl 16)); end;
  function PanelTabs(const ID: string): TERDDockTabs;
  var Container: TWinControl; I: Integer;
  begin
    Result := nil; Container := Host.FindPanel(ID).Control.Parent.Parent;
    for I := 0 to Container.ControlCount-1 do
      if Container.Controls[I] is TERDDockTabs then Exit(TERDDockTabs(Container.Controls[I]));
  end;
  procedure StartDrag(const ID: string; const Destination: TPoint);
  var I: Integer; R: TRect; P: TPoint;
  begin
    Tabs := PanelTabs(ID); Check(Tabs <> nil, 'Panel exposes its docking tab strip');
    I := 0; while (I < Tabs.Tabs.Count) and (Tabs.Tabs[I].Caption <> 'Movable') do Inc(I);
    Check(I < Tabs.Tabs.Count, 'Movable panel tab exists'); R := Tabs.TabBounds(I);
    P := Point((R.Left+R.Right) div 2,(R.Top+R.Bottom) div 2);
    SendMessage(Tabs.Handle,WM_LBUTTONDOWN,MK_LBUTTON,MousePosition(P));
    P := Tabs.ScreenToClient(Destination);
    SendMessage(Tabs.Handle,WM_MOUSEMOVE,MK_LBUTTON,MousePosition(P));
  end;
  procedure ReleaseAt(const Destination: TPoint);
  begin SendMessage(Tabs.Handle,WM_LBUTTONUP,0,MousePosition(Tabs.ScreenToClient(Destination))); end;
begin
  Form := TForm.Create(nil); Form.SetBounds(30,30,1000,760);
  Host := TERDDockHost.Create(Form); Host.Parent := Form; Host.SetBounds(0,0,840,600);
  A := TERDPanel.Create(Form); B := TERDEdit.Create(Form); B.Text := 'Keep document';
  try
    Host.AddPanel('editor','Editor',A,dzCenter); Host.AddPanel('movable','Movable',B,dzLeft);
    Form.Show; Application.ProcessMessages;
    Target := Host.ClientToScreen(Point(Host.ClientWidth-8,Host.ClientHeight div 2));
    StartDrag('movable',Target); Check(Host.FindPanel('movable').Zone = dzLeft, 'Drag preview leaves docking state unchanged');
    ReleaseAt(Target);
    Check(Host.FindPanel('movable').Zone = dzLeft, 'Native mouse release queues rather than applies the drop inside the tab callback');
    Application.ProcessMessages;
    Check((Host.FindPanel('movable').Zone = dzRight) and (Host.FindPanel('movable').Mode = dpmDocked), 'Native mouse release preserves the drag through capture loss and docks on the right');
    Target := Host.ClientToScreen(Host.GroupBounds(dzCenter).CenterPoint);
    StartDrag('movable',Target); ReleaseAt(Target); Application.ProcessMessages;
    Check((Host.FindPanel('movable').Zone = dzCenter) and (Host.ActivePanel(dzCenter) = 'movable'), 'Native drag joins the center tab group');
    Before := Host.SaveLayout;
    Target := Host.ClientToScreen(Point(8,Host.ClientHeight div 2));
    StartDrag('movable',Target); SendMessage(Tabs.Handle,WM_KEYDOWN,VK_ESCAPE,0); ReleaseAt(Target); Application.ProcessMessages;
    Check(Host.SaveLayout = Before, 'Escape cancels docking without applying a later mouse release');
    StartDrag('movable',Target); SetCapture(Form.Handle); ReleaseAt(Target); ReleaseCapture; Application.ProcessMessages;
    Check(Host.SaveLayout = Before, 'Capture transferred outside mouse-up cancels the docking drag');
    // A click must not be promoted to a drop without crossing the drag threshold.
    Tabs := PanelTabs('movable'); Target := Tabs.ClientToScreen(Tabs.TabBounds(Tabs.TabIndex).CenterPoint);
    StartDrag('movable',Target); ReleaseAt(Target); Application.ProcessMessages;
    Check(Host.SaveLayout = Before, 'Clicking the selected tab performs no docking operation');
    Target := Host.ClientToScreen(Point(Host.ClientWidth+40,Host.ClientHeight+40));
    StartDrag('movable',Target); ReleaseAt(Target); Application.ProcessMessages;
    Window := Host.FloatingWindow('movable');
    Check((Window <> nil) and (Host.FindPanel('movable').Mode = dpmFloating), 'Native drop outside the host creates a floating window');
    Target := Host.ClientToScreen(Point(8,Host.ClientHeight div 2));
    StartDrag('movable',Target); ReleaseAt(Target);
    Check(Host.FloatingWindow('movable') = Window, 'Floating source survives until the queued drop is dispatched');
    Application.ProcessMessages;
    Check((Host.FindPanel('movable').Zone = dzLeft) and (Host.FindPanel('movable').Mode = dpmDocked) and
      (Host.FloatingWindow('movable') = nil) and (B.Text = 'Keep document'), 'Floating tab redocks after native release without destroying its content');
  finally Form.Free; end;
end;

procedure TestDockHost;
var Form: TForm; Host, CopyHost: TERDDockHost; Origin, B, C, D, LostParent: TERDPanel;
    A, Lost: TERDEdit; Probe: TDockProbe; Original: TRect; R, Center: TRect;
    Layout, Before, Error: string; Bad: TJSONObject; Rows: TJSONArray; Window: TCustomForm;
    Stream: TMemoryStream; Raised: Boolean;
begin
  Form := TForm.Create(nil); Form.SetBounds(30,30,1000,720); Probe := TDockProbe.Create;
  Origin := TERDPanel.Create(Form); Origin.Parent := Form;
  A := TERDEdit.Create(Form); A.Parent := Origin; A.SetBounds(4,8,150,28); A.Text := 'Retained content'; Original := A.BoundsRect;
  B := TERDPanel.Create(Form); B.Parent := Origin; C := TERDPanel.Create(Form); C.Parent := Origin; D := TERDPanel.Create(Form); D.Parent := Origin;
  Host := TERDDockHost.Create(Form); Host.Parent := Form; Host.SetBounds(0,0,900,640);
  try
    Host.OnLayoutChanged := Probe.Layout; Host.OnPanelActivated := Probe.Activate;
    Host.BeginUpdate;
    try
      Host.AddPanel('editor','Editor',A,dzCenter).Closable := False;
      Host.AddPanel('locals','Locals',B,dzLeft); Host.AddPanel('watch','Watch',C,dzLeft); Host.AddPanel('console','Console',D,dzBottom);
      Check(Probe.Layouts = 0, 'Dock batch defers layout notifications');
    finally Host.EndUpdate; end;
    Check((Probe.Layouts = 1) and (Host.PanelCount = 4) and (Host.ActivePanel(dzLeft) = 'watch'), 'Dock groups expose stable IDs and coalesce initial registration');
    Check((A.Owner = Form) and (B.Owner = Form) and (A.Text = 'Retained content'), 'Docking borrows content without changing ownership or document state');
    Host.ActivatePanel('locals'); Check(B.Visible and not C.Visible and (Probe.ID = 'locals'), 'Only selected tab content is visible in a dock group');
    Center := Host.GroupBounds(dzCenter); Check((Center.Width > 0) and (Center.Height > 0) and not IntersectRect(R,Center,Host.GroupBounds(dzLeft)), 'Dock groups reserve non-overlapping center space');
    Check(not Host.HidePanel('editor') and not Host.AutoHidePanel('editor',dzCenter), 'Non-closable panel and center auto-hide restrictions are enforced');
    Raised := False; try Host.AddPanel('locals','Duplicate',D); except on E: EArgumentException do Raised := True; end;
    Check(Raised and (Host.PanelCount = 4), 'Duplicate dock IDs reject registration without changing content');
    Check(Host.DockPanel('watch',dzRight) and (Host.FindPanel('watch').Zone = dzRight), 'Panel can move from one tab group to another');
    Check(Host.AutoHidePanel('locals',dzLeft) and not B.Visible and Host.PeekPanel('locals') and B.Visible, 'Auto-hide rail reveals retained panel content');
    Host.ClosePeek; Check((Host.PeekPanelID = '') and not B.Visible, 'Closing flyout returns content to hidden parking without destroying it');
    Check(Host.HidePanel('console') and (Host.FindPanel('console').Mode = dpmHidden) and Host.ShowPanel('console') and (Host.FindPanel('console').Mode = dpmDocked), 'Show restores a hidden panel to its previous dock state');
    Host.LeftSize := 180; Layout := Host.SaveLayout;
    Host.DockPanel('locals',dzRight,0); Host.LeftSize := 300;
    Check(Host.LoadLayout(Layout,Error) and (Host.LeftSize = 180) and (Host.FindPanel('locals').Mode = dpmAutoHide), 'Validated layout restores sizes, tab placement and auto-hide mode');
    Before := Host.SaveLayout; Bad := TJSONObject.ParseJSONValue(Before) as TJSONObject;
    try Bad.RemovePair('version').Free; Bad.AddPair('version',TJSONNumber.Create(2)); Check(not Host.LoadLayout(Bad.ToJSON,Error) and (Error <> '') and (Host.SaveLayout = Before), 'Unsupported layout version leaves all live state unchanged'); finally Bad.Free; end;
    Bad := TJSONObject.ParseJSONValue(Before) as TJSONObject;
    try Rows := Bad.Values['panels'] as TJSONArray; Rows.AddElement(TJSONObject.ParseJSONValue(Rows.Items[0].ToJSON)); Check(not Host.LoadLayout(Bad.ToJSON,Error) and (Host.SaveLayout = Before), 'Duplicate layout IDs reject the full plan before applying any changes'); finally Bad.Free; end;
    Bad := TJSONObject.ParseJSONValue(Before) as TJSONObject;
    try
      Rows := Bad.Values['panels'] as TJSONArray; TJSONObject(Rows.Items[0]).RemovePair('id').Free; TJSONObject(Rows.Items[0]).AddPair('id','future-panel');
      Check(not Host.LoadLayout(Bad.ToJSON,Error,False) and (Host.SaveLayout = Before), 'Strict layout restore refuses unknown panel IDs atomically');
      Check(Host.LoadLayout(Bad.ToJSON,Error,True) and (Host.FindPanel('future-panel') = nil), 'Compatible restore skips unknown IDs without creating controls');
    finally Bad.Free; end;
    Host.LoadLayout(Layout,Error); Form.Show;
    Host.FloatPanel('locals',Rect(100,100,600,450)); Window := Host.FloatingWindow('locals');
    Check((Window <> nil) and (Window.Owner = Host) and (B.Owner = Form), 'Floating window is owned by host while borrowed content ownership stays unchanged');
    Window.Close; Application.ProcessMessages;
    Check((Host.FindPanel('locals').Mode = dpmHidden) and (B.Owner = Form) and (Host.FloatingWindow('locals') = nil), 'Native floating close is queued and hides content safely without destroying it');
    Host.ShowPanel('locals'); Check(Host.FindPanel('locals').Mode = dpmFloating, 'Reopening a hidden floating panel restores floating mode');
    Window := Host.FloatingWindow('locals'); Window.Free; Application.ProcessMessages;
    Check((B.Owner = Form) and (Host.FindPanel('locals').Mode = dpmDocked) and (Host.FloatingWindow('locals') = nil), 'Unexpected floating-window destruction preserves borrowed content and redocks it');
    Host.FloatPanel('editor',Rect(100,100,600,450)); Host.FloatingWindow('editor').Close; Application.ProcessMessages;
    Check((Host.FindPanel('editor').Mode = dpmDocked) and (A.Text = 'Retained content'), 'Closing non-closable floating content returns it to a dock group');
    Check(Host.DropPanel('console',Host.ClientToScreen(Point(Host.Width div 2,Host.Height div 2))) and (Host.FindPanel('console').Zone = dzCenter), 'Drop targeting joins the center tab group');
    LostParent := TERDPanel.Create(Form); LostParent.Parent := Form; Lost := TERDEdit.Create(Form); Lost.Parent := LostParent;
    Host.AddPanel('lost','Lost',Lost,dzRight); LostParent.Free;
    Host.RemovePanel('lost'); Check((Lost.Parent = nil) and (Lost.Owner = Form), 'Removed content survives when its original parent no longer exists');
    Lost.Free; Host.AddPanel('temporary','Temporary',TERDPanel.Create(Form),dzTop); Host.FindPanel('temporary').Control.Free; Application.ProcessMessages;
    Check((Host.FindPanel('temporary').Control = nil) and (Host.FindPanel('temporary').Mode = dpmHidden), 'Externally destroyed content detaches safely'); Host.RemovePanel('temporary');
    Host.OnLayoutChanged := nil; Host.OnPanelActivated := nil; RegisterClass(TERDDockHost); CopyHost := TERDDockHost.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Host); Stream.Position := 0; Stream.ReadComponent(CopyHost);
      Check((CopyHost.LeftSize = 180) and (CopyHost.PanelCount = 0) and (CopyHost.ComponentCount = 18), 'DFM stores dock configuration without borrowed runtime panels or duplicate internals');
    finally Stream.Free; CopyHost.Free; end;
    Host.RemovePanel('editor'); Check((A.Parent = Origin) and (A.Align = alNone) and (A.BoundsRect = Original) and (A.Text = 'Retained content'), 'Removing a panel restores its original control placement and content');
    FreeAndNil(Host); Check((B.Parent = Origin) and (C.Parent = Origin) and (D.Parent = Origin), 'Host destruction restores all remaining borrowed controls');
  finally Host.Free; Probe.Free; Form.Free; end;
end;

procedure TestAutomaticOfficeHints;
var Form: TForm; Button: TERDButton; Tabs: TERDTabs; Ribbon: TERDRibbon; Title: TERDTitleBar;
    Action: TAction; Theme: TERDTheme; Content: TERDHintContent; Info: THintInfo;
    Provider: IERDHintProvider; R: TRect; Point: TPoint;
begin
  Form := TForm.Create(nil);
  try
    Theme := TERDTheme.Create(Form); Theme.Mode := tmDark;
    Button := TERDButton.Create(Form); Button.Parent := Form; Button.Theme := Theme;
    Action := TAction.Create(Form); Action.Caption := '&Save'; Action.Hint := 'Stores the document'; Action.ShortCut := ShortCut(VK_F2,[]); Button.Action := Action; Button.Glyph := glSave;
    Content := ERDResolveHint(Button,Button.Hint,Button.ClientToScreen(System.Types.Point(5,5)));
    Check((Content.Title = 'Save') and (Content.Description = 'Stores the document') and (Content.Shortcut = 'F2') and (Content.Glyph = glSave), 'Automatic hints combine component/action caption, description, glyph and shortcut');
    Check(Content.HasPalette and (Content.Palette.Background = Theme.Palette.Background), 'Automatic hint inherits explicit component theme without an installer');
    Button.Icon.BuiltIn := ei003Opslaan; Content := ERDResolveHint(Button,Button.Hint,Button.ClientToScreen(System.Types.Point(5,5)));
    Check((Content.Icon = Button.Icon) and Content.Icon.Available, 'Automatic hint uses the component selected icon source');
    Button.Hint := 'Explicit title|Explicit description'; Content := ERDResolveHint(Button,'Explicit description',System.Types.Point(0,0));
    Check((Content.Title = 'Explicit title') and (Content.Description = 'Explicit description'), 'VCL long-hint path recovers explicit title and description');
    Info := Default(THintInfo); Info.HintControl := Button; Info.HintStr := Button.Hint; Info.HintWindowClass := THintWindow;
    Button.Perform(CM_HINTSHOW,0,LPARAM(@Info));
    Check(Button.ShowHint and not Button.ParentShowHint and (Info.HintWindowClass = TERDHintWindow), 'ERD controls select Office hints automatically without TERDHintStyle or event wiring');
    Tabs := TERDTabs.Create(Form); Tabs.Parent := Form; Tabs.SetBounds(0,100,400,36);
    with Tabs.Tabs.Add do begin Caption := 'Document'; Hint := 'Shows this document'; Glyph := glFile; end;
    R := Tabs.TabBounds(0); Point := Tabs.ClientToScreen(System.Types.Point((R.Left+R.Right) div 2,(R.Top+R.Bottom) div 2));
    Content := ERDResolveHint(Tabs,'Shows this document',Point);
    Check((Content.Title = 'Document') and (Content.Description = 'Shows this document') and (Content.Glyph = glFile), 'Tab-item metadata supplies its own caption, description and glyph automatically');
    Ribbon := TERDRibbon.Create(Form); Ribbon.Parent := Form; Title := TERDTitleBar.Create(Form); Title.Parent := Form;
    Check(Supports(Ribbon,IERDHintProvider,Provider), 'Ribbon exposes automatic item-hint metadata'); Provider := nil;
    Check(Supports(Title,IERDHintProvider,Provider), 'Title-bar buttons expose automatic item-hint metadata'); Provider := nil;
  finally Form.Free; end;
end;

procedure TestOfficeHints;
var Style, CopyStyle: TERDHintStyle; Hint: TOfficeHintProbe; Probe: TDockProbe;
    Office, Compact, Resolved: TRect; Params: TCreateParams; Stream: TMemoryStream;
begin
  Style := TERDHintStyle.Create(nil); Hint := TOfficeHintProbe.Create(nil); Probe := TDockProbe.Create;
  try
    Check((Style.Layout = hlOffice) and (Style.Glyph = glInfo), 'Office hint layout and themed info glyph are defaults');
    Office := Hint.CalcHintRect(240,'Save layout|Saves the arrangement of all panels, their tab groups and floating windows. The layout can be restored later.',nil);
    Check((Office.Width <= 240) and (Office.Height > 52), 'Office description wraps into a measured multiline hint');
    Style.Layout := hlCompact; Compact := Hint.CalcHintRect(240,'Save layout|Saves the arrangement of all panels, their tab groups and floating windows. The layout can be restored later.',nil);
    Check(Compact.Height < Office.Height, 'Compact hint layout remains selectable');
    Style.Layout := hlOffice; Style.OnResolveHint := Probe.Resolve;
    Resolved := Hint.CalcHintRect(280,'Original',Pointer(1)); Check((Resolved.Width <= 280) and (Resolved.Height > 40), 'Hint resolver supplies title, description, shortcut and glyph without reading HintData');
    Hint.Params(Params); Check((Params.Style and (WS_BORDER or WS_DLGFRAME or WS_THICKFRAME) = 0) and (Params.ExStyle and (WS_EX_CLIENTEDGE or WS_EX_WINDOWEDGE) = 0), 'Office hints keep their single flat client border');
    Style.OnResolveHint := nil; Style.Active := False; Style.Glyph := glHelp;
    RegisterClass(TERDHintStyle); CopyStyle := TERDHintStyle.Create(nil); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Style); Stream.Position := 0; Stream.ReadComponent(CopyStyle);
      Check((CopyStyle.Layout = hlOffice) and (CopyStyle.Glyph = glHelp) and not CopyStyle.Active, 'Hint DFM preserves office layout, icon and activation configuration');
    finally Stream.Free; CopyStyle.Free; end;
  finally Hint.Free; Style.Free; Probe.Free; end;
end;

procedure TestTimeSeriesBars;
var Form: TForm; Chart, CopyChart: TERDTimeSeriesChart; Bars: TArray<TERDTimeBar>; Bar: TERDTimeBar;
    Stream: TMemoryStream; I: Integer;
begin
  Form := TForm.Create(nil); Chart := TERDTimeSeriesChart.Create(Form); Chart.Parent := Form;
  try
    Chart.Series.Add; Chart.Series.Add; Chart.WindowSeconds := 10; Chart.ChartStyle := tcsStackedBars;
    Chart.Series[0].AddSample(0,4); Chart.Series[1].AddSample(0,6);
    Chart.Series[0].AddSample(1,5); Chart.Series[1].AddSample(1,0,False);
    Chart.Series[0].AddSample(2,3); Chart.Series[1].AddSample(2,7);
    Bars := Chart.BarGeometry; Check(Length(Bars) = 4, 'Stacked chart omits the complete column when one segment is unavailable');
    Check((Bars[1].BaseValue = 4) and (Bars[1].Sample.Value = 6) and (Bars[1].SeriesIndex = 1), 'Stack geometry preserves individual value and cumulative segment baseline');
    Check(Chart.BarAt(Floor((Bars[1].Bounds.Left+Bars[1].Bounds.Right)/2),Floor((Bars[1].Bounds.Top+Bars[1].Bounds.Bottom)/2),Bar) and
      (Bar.SeriesIndex = 1) and (Bar.SampleIndex = 0) and (Bar.Sample.Value = 6), 'Hover picks the actual stacked free-memory segment');
    for I := 0 to High(Bars) do Check((Bars[I].Bounds.Width > 0) and (Bars[I].Bounds.Height > 0), 'Rendered bar rectangles have usable hit-test geometry');
    Chart.ChartStyle := tcsBars; Check(Length(Chart.BarGeometry) = 5, 'Grouped bars retain known series observations independently of a missing neighbour');
    Chart.Clear; Chart.ChartStyle := tcsStackedBars;
    Chart.Series[0].AddSample(0,1); Chart.Series[0].AddSample(2,2); Chart.Series[1].AddSample(1,3); Chart.Series[1].AddSample(2,4);
    Bars := Chart.BarGeometry; Check((Length(Bars) = 2) and (Bars[0].Sample.TimeSeconds = 2), 'Stacks align by timestamp rather than array position');
    Chart.Clear; Chart.Series[0].AddSample(0,-3); Chart.Series[1].AddSample(0,-2);
    Bars := Chart.BarGeometry; Check((Length(Bars) = 2) and (Bars[1].BaseValue = -3), 'Negative stacks accumulate below a shared zero baseline');
    Chart.Clear; Chart.Series[0].AddSample(0,0); Chart.Series[1].AddSample(0,0);
    Bars := Chart.BarGeometry; Check((Length(Bars) = 2) and (Bars[0].Bounds.Height >= 1), 'Measured zero has a baseline mark rather than a missing-data gap');
    Chart.Clear; Chart.Series[0].AddSample(0,MaxDouble); Chart.Series[1].AddSample(0,MaxDouble);
    Check(Length(Chart.BarGeometry) = 0, 'Overflowing stack totals do not create invalid geometry');
    Chart.ChartStyle := tcsBars; Bars := Chart.BarGeometry;
    Check((Length(Bars) = 2) and not IsNan(Bars[0].Bounds.Top) and not IsInfinite(Bars[0].Bounds.Top), 'Finite extreme grouped bars retain finite axis geometry');
    RegisterClass(TERDTimeSeriesChart); CopyChart := TERDTimeSeriesChart.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Chart); Stream.Position := 0; Stream.ReadComponent(CopyChart);
      Check((CopyChart.ChartStyle = tcsBars) and (CopyChart.Series.Count = 2) and (CopyChart.Series[0].Count = 0), 'DFM persists bar style without runtime samples');
    finally Stream.Free; CopyChart.Free; end;
  finally Chart.Free; Form.Free; end;
end;

procedure TestRuntimeBarTimelines;
var Form: TForm; Monitor, CopyMonitor: TERDRuntimeMonitor; Request: TERDRuntimeRequest;
    Metrics: TArray<TERDRuntimeMetric>; Sample: TERDTimeSample; Metric: TERDRuntimeMetric;
    Stream: TMemoryStream;
  procedure Publish(Generation: Integer);
  begin
    Request := Monitor.RequestSnapshot(1,Generation);
    Check(Monitor.PublishSnapshot(Request,Generation,Metrics), 'Runtime bar timeline accepts next valid observation');
  end;
begin
  Form := TForm.Create(nil); Monitor := TERDRuntimeMonitor.Create(Form); Monitor.Parent := Form;
  try
    Monitor.BarTimelines := True; SetLength(Metrics,4);
    Metrics[0].ID := 'heap.usedBytes'; Metrics[0].Group := rgHeap; Metrics[0].UnitText := 'bytes'; Metrics[0].Kind := rmUnsigned; Metrics[0].UnsignedValue := 100;
    Metrics[1] := Metrics[0]; Metrics[1].ID := 'heap.totalBytes'; Metrics[1].UnsignedValue := 200;
    Metrics[2].ID := 'gc.minorGCTimeMicros'; Metrics[2].Group := rgGC; Metrics[2].UnitText := 'us'; Metrics[2].Kind := rmUnsigned; Metrics[2].Cumulative := True; Metrics[2].UnsignedValue := High(UInt64)-100;
    Metrics[3] := Metrics[2]; Metrics[3].ID := 'gc.majorGCTimeMicros'; Metrics[3].UnsignedValue := 20;
    Publish(0);
    Check((Monitor.ChartFor(rgHeap).ChartStyle = tcsStackedBars) and (Monitor.ChartFor(rgHeap).Series[1].Sample(0).Value = 100), 'Heap bar stack uses derived free bytes, avoiding used-plus-capacity double counting');
    Check(not Monitor.ChartFor(rgGC).Series[0].Sample(0).Available, 'First cumulative GC observation establishes a baseline, not an invented duration');
    Inc(Metrics[2].UnsignedValue,7); Inc(Metrics[3].UnsignedValue,2); Publish(1);
    Sample := Monitor.ChartFor(rgGC).Series[0].Sample(1);
    Check(Sample.Available and (Sample.Value = 7), 'GC counter subtraction happens before Double conversion, retaining small increments beyond 2^53');
    Check(Monitor.MetricByID('gc.minorGCTimeMicros',Metric) and (Metric.UnsignedValue = High(UInt64)-93), 'Summary retains original exact cumulative counter');
    Request := Monitor.RequestSnapshot(1,2); Check(Monitor.FailSnapshot(Request,2,'missing sample'), 'Timeline failure records an unavailable interval');
    Inc(Metrics[2].UnsignedValue,10); Publish(3); Check(not Monitor.ChartFor(rgGC).Series[0].Sample(3).Available, 'GC duration never spans a failed observation interval');
    Inc(Metrics[2].UnsignedValue,5); Publish(4); Check(Monitor.ChartFor(rgGC).Series[0].Sample(4).Value = 5, 'Successful baseline after a gap restores interval deltas');
    Metrics[2].UnsignedValue := 0; Publish(5); Check(not Monitor.ChartFor(rgGC).Series[0].Sample(5).Available, 'Counter reset creates a gap rather than a negative GC duration');
    Metrics[2].UnsignedValue := 11; Publish(6); Check(Monitor.ChartFor(rgGC).Series[0].Sample(6).Value = 11, 'Counter reset establishes a fresh baseline');
    Metrics[2].Availability := rmaUnavailable; Publish(7); Metrics[2].Availability := rmaAvailable; Metrics[2].UnsignedValue := 20; Publish(8);
    Check(not Monitor.ChartFor(rgGC).Series[0].Sample(8).Available, 'Missing counter requires a new baseline before calculating duration');
    Metrics[2].UnsignedValue := 23; Publish(9); Check(Monitor.ChartFor(rgGC).Series[0].Sample(9).Value = 3, 'A later complete interval is available');
    Metrics[0].UnsignedValue := 250; Publish(10);
    Check(not Monitor.ChartFor(rgHeap).Series[0].Sample(10).Available and not Monitor.ChartFor(rgHeap).Series[1].Sample(10).Available, 'Inconsistent used and capacity observations do not invent a free-memory stack');
    Metrics[0].UnsignedValue := 100; Metrics[1].Availability := rmaUnavailable; Publish(11);
    Check(not Monitor.ChartFor(rgHeap).Series[0].Sample(11).Available, 'Unknown capacity creates a full heap stack gap');
    Monitor.BarTimelines := False; Check((Monitor.ChartFor(rgGC).Series[0].Count = 0) and (Monitor.ChartFor(rgHeap).ChartStyle = tcsLines), 'Changing data interpretation clears old history before switching styles');
    Metrics[1].Availability := rmaAvailable; Publish(12);
    Check(Monitor.ChartFor(rgHeap).Series[1].Sample(0).Value = 200, 'Legacy line mode continues to plot capacity and cumulative GC totals');
    Monitor.BarTimelines := True; RegisterClass(TERDRuntimeMonitor); CopyMonitor := TERDRuntimeMonitor.Create(Form); Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(Monitor); Stream.Position := 0; Stream.ReadComponent(CopyMonitor);
      Check(CopyMonitor.BarTimelines and (CopyMonitor.ChartFor(rgHeap).ChartStyle = tcsStackedBars) and (CopyMonitor.ChartFor(rgGC).Series[0].Count = 0), 'DFM preserves runtime bar configuration without history');
    finally Stream.Free; CopyMonitor.Free; end;
  finally Monitor.Free; Form.Free; end;
end;

procedure TestTimeSeriesHistory;
var Form: TForm; Chart, CopyChart: TERDTimeSeriesChart; Series: TERDTimeSeries;
    I, SeriesIndex, SampleIndex: Integer; Value: TERDTimeSample; Rejected: Boolean; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Chart := TERDTimeSeriesChart.Create(Form); Chart.Parent := Form; Chart.MaxSamples := 16;
    Series := Chart.Series.Add; Series.Caption := 'Heap'; Chart.UnitText := 'KiB';
    for I := 0 to 31 do Series.AddSample(I,I*2);
    Check((Series.Count = 16) and (Series.Sample(0).TimeSeconds = 16) and (Series.Sample(15).Value = 62), 'History ring retains only the newest samples in chronological order');
    Chart.MaxSamples := 32; for I := 32 to 47 do Series.AddSample(I,I*2); Chart.MaxSamples := 16;
    Check((Series.Count = 16) and (Series.Sample(0).TimeSeconds = 32), 'Resizing history preserves the newest observations');
    Rejected := False; try Series.AddSample(47,1); except on E: EArgumentException do Rejected := True; end;
    Check(Rejected and (Series.Count = 16), 'Out-of-order timestamps do not change history');
    Series.AddSample(48,NaN,False);
    Check(Chart.SampleNear(Chart.Width-MulDiv(12,Chart.CurrentPPI,96),SeriesIndex,SampleIndex,Value) and not Value.Available, 'Unavailable measurement remains an explicit gap rather than a measured zero');
    RegisterClass(TERDTimeSeriesChart); CopyChart := TERDTimeSeriesChart.Create(Form);
    try
      Stream.WriteComponent(Chart); Stream.Position := 0; Stream.ReadComponent(CopyChart);
      Check((CopyChart.Series.Count = 1) and (CopyChart.Series[0].Count = 0) and (CopyChart.UnitText = 'KiB'), 'DFM keeps chart series configuration without runtime history');
    finally CopyChart.Free; end;
  finally Stream.Free; Form.Free; end;
end;
procedure TestPCBCardinalAnchors;
var Bounds, Board: TRect; Source, Target: TPoint; Geometry: TERDPCBConnectionGeometry;
    I, J: Integer; Last, Previous: TPointF;
begin
  Bounds := Rect(100,100,132,132); Board := Rect(0,0,0,0);
  for I := 0 to 3 do
  begin
    case I of
      0: Source := Point(0,116); 1: Source := Point(232,116);
      2: Source := Point(116,0); else Source := Point(116,232);
    end;
    Target := ERDPCBComponentAnchor(Source,Bounds);
    case I of
      0: Check(Target = Point(100,116), 'Left connection uses side center');
      1: Check(Target = Point(132,116), 'Right connection uses side center');
      2: Check(Target = Point(116,100), 'Top connection uses side center');
      3: Check(Target = Point(116,132), 'Bottom connection uses side center');
    end;
    Geometry := ERDBuildPCBBoardConnection(Source,Target,nil,Board,Bounds,1);
    Check(Length(Geometry.Samples) >= 2, 'Every component side has a routable connection');
    Last := Geometry.Samples[High(Geometry.Samples)]; Previous := Geometry.Samples[High(Geometry.Samples)-1];
    Check((Last.X = Target.X) and (Last.Y = Target.Y), 'Curve ends exactly on the chosen side center');
    if I < 2 then Check((Previous.Y = Last.Y) and ((Previous.X-Last.X)*(Source.X-Target.X) > 0), 'Horizontal side is approached normally from outside')
    else Check((Previous.X = Last.X) and ((Previous.Y-Last.Y)*(Source.Y-Target.Y) > 0), 'Vertical side is approached normally from outside');
    for J := 0 to High(Geometry.Samples) do
      Check(not ((Geometry.Samples[J].X > Bounds.Left) and (Geometry.Samples[J].X < Bounds.Right) and
        (Geometry.Samples[J].Y > Bounds.Top) and (Geometry.Samples[J].Y < Bounds.Bottom)), 'Curve never passes through the target component');
  end;
  Board := Rect(0,100,80,132); Source := Point(78,116); Target := ERDPCBComponentAnchor(Source,Bounds);
  Geometry := ERDBuildPCBBoardConnection(Source,Target,nil,Board,Bounds,1);
  Check(Length(Geometry.Samples) >= 2, 'A component close to the PCB keeps a normal side connection');
end;
procedure TestPCBZOrderAndGrid;
var Form: TForm; Designer: TERDPCBDesigner; A, B, C, Index: Integer; Scene: TERDPCBScene;
    AID, BID, CID, Before: string; Item: TERDPCBDesignItem; Stream: TMemoryStream; CopyDesigner: TERDPCBDesigner;
begin
  Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Scene := Designer.Scene;
    A := Designer.AddDesignItem(piNote,PointF(0,0)); B := Designer.AddDesignItem(piTimer,PointF(0,0));
    C := Designer.AddDesignItem(piNote,PointF(0,0));
    AID := Scene.Item(A).ID; BID := Scene.Item(B).ID; CID := Scene.Item(C).ID;
    Scene.SelectIDs([AID,BID]); Scene.ResetHistory; Before := Scene.ContentJSON;
    Designer.ReorderSelection(zoBringToFront);
    Check((Scene.Item(0).ID = CID) and (Scene.Item(1).ID = AID) and (Scene.Item(2).ID = BID) and
      (Scene.SelectionCount = 2), 'Bring to front preserves relative order, identities and multiselection');
    Designer.Undo; Check((Scene.ContentJSON = Before) and not Scene.CanUndo, 'Z-order change has one undo step');
    Scene.SelectIDs([BID]); Designer.ReorderSelection(zoSendToBack); Check(Scene.Item(0).ID = BID, 'Send to back places component behind notes');
    Designer.ReorderSelection(zoBringForward); Check(Scene.Item(1).ID = BID, 'Bring forward moves one layer');
    Designer.ReorderSelection(zoSendBackward); Check(Scene.Item(0).ID = BID, 'Send backward moves one layer');
    Designer.SetSelectionLocked(True); Before := Scene.ContentJSON; Designer.ReorderSelection(zoBringToFront);
    Check(Scene.ContentJSON = Before, 'Locked objects cannot be reordered'); Designer.SetSelectionLocked(False);
    Designer.GridSizeX := 2; Designer.GridSizeY := 5;
    Index := Designer.AddDesignItem(piTimer,PointF(3.2,7.8)); Item := Scene.Item(Index);
    Check((Item.X = 4) and (Item.Y = 10), 'Placement snaps independently on X and Y grid');
    CopyDesigner := TERDPCBDesigner.Create(Form);
    try
      Stream.WriteComponent(Designer); Stream.Position := 0; Stream.ReadComponent(CopyDesigner);
      Check((CopyDesigner.GridSizeX = 2) and (CopyDesigner.GridSizeY = 5), 'Independent grid axes survive DFM streaming');
      CopyDesigner.GridSize := 3;
      Check((CopyDesigner.GridSizeX = 3) and (CopyDesigner.GridSizeY = 3), 'Legacy GridSize sets both axes');
    finally CopyDesigner.Free; end;
  finally Stream.Free; Form.Free; end;
end;
procedure TestSelectedTabVisibility;
var Form: TForm; Tabs: TTabProbe; I, Selected: Integer; R: TRect;
begin
  Form := TForm.Create(nil);
  try
    Tabs := TTabProbe.Create(Form); Tabs.Parent := Form; Tabs.SetBounds(0,0,340,36);
    Tabs.TabStyle := tsDocument; Tabs.ShowNewTab := True;
    for I := 0 to 5 do Tabs.Tabs.Add.Caption := 'Long document '+IntToStr(I);
    Form.Show; Tabs.TabIndex := 5; R := Tabs.TabBounds(5);
    Check((R.Left >= MulDiv(28,Tabs.CurrentPPI,96)) and
      (R.Right <= Tabs.Width-MulDiv(70,Tabs.CurrentPPI,96)), 'Selected tab clears chevron and new-tab button');
    Selected := Tabs.TabIndex; Tabs.PressAt(10,18); Tabs.ReleaseAt(10,18);
    Tabs.TabIndex := Selected; R := Tabs.TabBounds(Selected);
    Check(R.Right <= Tabs.Width-MulDiv(70,Tabs.CurrentPPI,96), 'Reselecting the current tab restores full visibility');
  finally Form.Free; end;
end;
procedure TestPCBConnections;
var Form: TForm; Designer: TERDPCBDesigner; BoardFile, Text, Before: string;
    A, B, I: Integer; Geometry: TERDPCBConnectionGeometry; Boxes: TArray<TRect>; Failed: Boolean;
    P, Q, Hit, BoardTop, BoardBottom: TPoint; TargetRect: TRect; ID: string; Role: TERDPCBPinRole; Updates: TArray<TERDPCBDesignItem>;
begin
  SetLength(Boxes, 1); Boxes[0] := Rect(40,20,60,80);
  Geometry := ERDBuildPCBConnection(Point(0,0), Point(100,100), Boxes, 1);
  Check(Length(Geometry.Samples) > 2, 'Connection geometry contains sampled curves');
  for I := 0 to High(Geometry.Samples) do
  begin
    Check(ERDPCBConnectionDistance(Geometry, Geometry.Samples[I]) < 0.01, 'Visible curve samples are selectable');
    Check(not ((Geometry.Samples[I].X > 40) and (Geometry.Samples[I].X < 60) and
      (Geometry.Samples[I].Y > 20) and (Geometry.Samples[I].Y < 80)), 'Rounded connection avoids obstacle interiors');
  end;
  Check(ERDPCBConnectionDistance(Geometry, PointF(-100,-100)) > 50, 'Distant points do not hit a connection');
  Form := TForm.Create(nil); BoardFile := TPath.GetTempFileName;
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,600,400);
    Text := '{"schema":1,"units":"mm","y_axis":"up","source_sha256":"connection-test",' +
      '"pads":[{"id":"J1.1","net":"OUT_GPIO4","reference":"J1","number":"1","position":[2,8],"polygon":[[1,7],[3,7],[3,9],[1,9]],"drill":[]},' +
      '{"id":"J2.1","net":"OUT_GPIO5","reference":"J2","number":"1","position":[8,8],"polygon":[[7,7],[9,7],[9,9],[7,9]],"drill":[]}],' +
      '"channels":[{"pin":"J1.1","gpio":"GPIO4","route":"test"},{"pin":"J2.1","gpio":"GPIO5","route":"test"}],' +
      '"components":[],"silkscreen":[],"outline":[[[0,0],[10,0],[10,10],[0,10],[0,0]]]}';
    TFile.WriteAllText(BoardFile, Text, TEncoding.UTF8); Designer.LoadBoard(BoardFile);
    A := Designer.AddDesignItem(piOutputGroup, PointF(2,2));
    B := Designer.AddDesignItem(piOutputGroup, PointF(7,2));
    Designer.AssignPin(A, prPrimary, 'J1.1'); Designer.FitBoard;
    P := Designer.BoardToClient(PointF(2,8));
    Q := Designer.BoardToClient(PointF(Designer.Scene.Item(A).X, Designer.Scene.Item(A).Y));
    TargetRect := Rect(Q.X,Q.Y-MulDiv(32,Designer.CurrentPPI,96),Q.X+MulDiv(32,Designer.CurrentPPI,96),Q.Y);
    Q := ERDPCBComponentAnchor(P,TargetRect);
    BoardTop := Designer.BoardToClient(PointF(0,10)); BoardBottom := Designer.BoardToClient(PointF(10,0));
    Hit := Designer.BoardToClient(PointF(Designer.Scene.Item(B).X,Designer.Scene.Item(B).Y));
    SetLength(Boxes,1); Boxes[0] := Rect(Hit.X,Hit.Y-MulDiv(32,Designer.CurrentPPI,96),Hit.X+MulDiv(32,Designer.CurrentPPI,96),Hit.Y);
    Geometry := ERDBuildPCBBoardConnection(P,Q,Boxes,Rect(BoardTop.X,BoardTop.Y,BoardBottom.X,BoardBottom.Y),TargetRect,Designer.CurrentPPI/96);
    Check(Length(Geometry.Samples) > 0, 'Board-aware painted connection has hit-test samples');
    Hit := Point(Round(Geometry.Samples[Length(Geometry.Samples) div 2].X),
      Round(Geometry.Samples[Length(Geometry.Samples) div 2].Y));
    Check(Designer.HitTestConnection(Hit, ID, Role) and (ID = Designer.Scene.Item(A).ID) and
      (Role = prPrimary), 'Designer hit test identifies the painted connection and role');
    Designer.SelectConnection(Designer.Scene.Item(A).ID, prPrimary);
    Designer.ReconnectSelectedPin('J2.1');
    Check(Designer.Scene.Item(A).Pin = 'J2.1', 'Reconnect changes the selected pin');
    Designer.Undo; Check(Designer.Scene.Item(A).Pin = 'J1.1', 'Reconnect is one undo operation');
    Designer.SelectConnection(Designer.Scene.Item(A).ID, prPrimary);
    Designer.MoveSelectedConnection(Designer.Scene.Item(B).ID, prPrimary);
    Check((Designer.Scene.Item(A).Pin = '') and (Designer.Scene.Item(B).Pin = 'J1.1'), 'Transfer moves a connection without duplicating it');
    Designer.Undo;
    Check((Designer.Scene.Item(A).Pin = 'J1.1') and (Designer.Scene.Item(B).Pin = ''), 'Transfer undo restores both endpoints together');
    Designer.AssignPin(B, prPrimary, 'J2.1'); Designer.SelectConnection(Designer.Scene.Item(A).ID, prPrimary);
    Before := Designer.Scene.ContentJSON; Failed := False;
    try Designer.ReconnectSelectedPin('J2.1'); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Scene.ContentJSON = Before), 'Occupied pin rejection preserves the original connection');
    Designer.DeleteSelectedConnection;
    Check((Designer.Scene.Count = 2) and (Designer.Scene.Item(A).Pin = ''), 'Disconnect retains the component');
    Designer.Undo; Check(Designer.Scene.Item(A).Pin = 'J1.1', 'Disconnect can be undone');
    SetLength(Updates, 2); Updates[0] := Designer.Scene.Item(A); Updates[1] := Designer.Scene.Item(B);
    Updates[0].Pin := 'J2.1'; Updates[1].Pin := 'J1.1'; Before := Designer.Scene.ContentJSON;
    Designer.Scene.UpdateItems(Updates);
    Check((Designer.Scene.Item(A).Pin = 'J2.1') and (Designer.Scene.Item(B).Pin = 'J1.1'), 'Batch pin validation uses the complete final scene');
    Designer.Undo; Check(Designer.Scene.ContentJSON = Before, 'Batch pin swap is one undo operation');
    Updates[0].Pin := 'J1.1'; Failed := False;
    try Designer.Scene.UpdateItems(Updates); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Scene.ContentJSON = Before), 'A conflicting batch pin assignment leaves both owners unchanged');
  finally Form.Free; System.SysUtils.DeleteFile(BoardFile); end;
end;

procedure TestPCBPlacement;
var Form: TForm; Designer, CopyDesigner: TERDPCBDesigner; Before, Index: Integer; Item: TERDPCBDesignItem;
    Client: TPoint; Failed: Boolean; Stream: TMemoryStream;
begin
  Form := TForm.Create(nil);
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,600,400);
    Designer.AddDesignItem(piNote, PointF(0,0)); Designer.FitBoard;
    Designer.SnapToGrid := False; Form.Show; Designer.SetFocus; Before := Designer.Scene.Count;
    Designer.BeginPlacement(piTimer); Designer.PreviewPlacementAt(Point(100,100));
    Check(Designer.Placing and (Designer.Scene.Count = Before), 'Placement preview does not mutate scene content');
    Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(100,100));
    Designer.Perform(WM_LBUTTONUP, 0, MakeLParam(100,100));
    Check(not Designer.Placing and (Designer.Scene.Count = Before + 1), 'Click placement is single-shot by default');
    Index := Designer.Scene.PrimaryIndex; Item := Designer.Scene.Item(Index);
    Client := Designer.BoardToClient(PointF(Item.X, Item.Y));
    Check((Abs(Client.X - 100) <= 1) and (Abs(Client.Y - MulDiv(32, Designer.CurrentPPI, 96) - 100) <= 1), 'Click placement anchors the glyph top-left at the pointer');
    Designer.Undo; Check(Designer.Scene.Count = Before, 'Inserting a component is one undo operation');
    Designer.BeginPlacement(piNote);
    Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON or MK_SHIFT, MakeLParam(120,120));
    Designer.Perform(WM_LBUTTONUP, MK_SHIFT, MakeLParam(120,120));
    Check(Designer.Placing and (Designer.Scene.Count = Before + 1), 'Shift-click keeps the placement tool armed');
    Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(200,200));
    Designer.Perform(WM_LBUTTONUP, 0, MakeLParam(200,200));
    Item := Designer.Scene.Item(Designer.Scene.PrimaryIndex);
    Client := Designer.BoardToClient(PointF(Item.X, Item.Y + Item.Height));
    Check(not Designer.Placing and (Abs(Client.X - 200) <= 1) and (Abs(Client.Y - 200) <= 1), 'Notes use their visual top-left for placement too');
    Before := Designer.Scene.Count; Designer.BeginPlacement(piCounter);
    Designer.Perform(WM_KEYDOWN, VK_ESCAPE, 0);
    Check(not Designer.Placing and (Designer.Scene.Count = Before), 'Escape cancels placement without adding an item');
    Designer.BeginPlacement(piCounter);
    Designer.Perform(WM_RBUTTONDOWN, MK_RBUTTON, MakeLParam(220,220));
    Designer.Perform(WM_RBUTTONUP, 0, MakeLParam(220,220));
    Check(not Designer.Placing and (Designer.Scene.Count = Before), 'Right-click cancels placement without changing the scene');
    Designer.BeginPlacement(piCounter); Stream := TMemoryStream.Create;
    try
      RegisterClass(TERDPCBDesigner); Stream.WriteComponent(Designer); Stream.Position := 0;
      CopyDesigner := TERDPCBDesigner(Stream.ReadComponent(nil));
      try Check(not CopyDesigner.Placing, 'Placement tools are transient and never restored from a DFM');
      finally CopyDesigner.Free; end;
    finally Stream.Free; end;
    Designer.CancelPlacement; Designer.TemplateEditMode := True; Failed := False;
    try Designer.BeginPlacement(piTimer); except on E: EArgumentException do Failed := True; end;
    Check(Failed and not Designer.Placing, 'Template authoring cannot arm scene insertion');
  finally Form.Free; end;
end;

procedure TestPCBDesignerTools;
var Form: TForm; Designer: TERDPCBDesigner; Editor: TERDMemo; I, A, B, C: Integer;
    Value: TERDPCBDesignItem; Before: string; Route: TArray<TPoint>; Boxes: TArray<TRect>;
    P, Q: TPoint;
begin
  SetLength(Boxes, 1); Boxes[0] := Rect(40, 20, 60, 80);
  Route := ERDPCBRoute(Point(0, 50), Point(100, 50), Boxes, 8);
  Check((Length(Route) >= 4) and (Route[0] = Point(0,50)) and
    (Route[High(Route)] = Point(100,50)), 'PCB route finds a detour around an obstacle');
  for I := 1 to High(Route) do
  begin
    P := Route[I-1]; Q := Route[I];
    Check((P.X = Q.X) or (P.Y = Q.Y), 'PCB detour is orthogonal before corner rounding');
    if P.X = Q.X then
      Check(not ((P.X > 40) and (P.X < 60) and (Max(P.Y,Q.Y) > 20) and (Min(P.Y,Q.Y) < 80)), 'Vertical route avoids the note interior')
    else Check(not ((P.Y > 20) and (P.Y < 80) and (Max(P.X,Q.X) > 40) and (Min(P.X,Q.X) < 60)), 'Horizontal route avoids the note interior');
  end;
  Form := TForm.Create(nil);
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Designer.SetBounds(0,0,600,400);
    A := Designer.AddDesignItem(piNote, PointF(0,0)); Designer.FitBoard; Form.Show;
    Designer.BeginNoteEdit; Editor := nil;
    for I := 0 to Designer.ControlCount - 1 do
      if Designer.Controls[I] is TERDMemo then Editor := TERDMemo(Designer.Controls[I]);
    Check((Editor <> nil) and Editor.Visible, 'Double-click note API opens an editor on the canvas');
    Value := Designer.Scene.Item(A); Editor.Text := 'Cancelled'; Designer.FinishNoteEdit(False);
    Check(Designer.Scene.Item(A).Caption = Value.Caption, 'Escape-style note cancellation preserves the original text');
    Designer.BeginNoteEdit; Editor.Text := 'Edited' + sLineBreak + 'On canvas'; Designer.FinishNoteEdit(True);
    Check(Designer.Scene.Item(A).Caption = 'Edited' + sLineBreak + 'On canvas', 'Canvas note edit commits multiline text');
    Designer.Undo; Check(Designer.Scene.Item(A).Caption = Value.Caption, 'A note edit is one undo operation');
    Designer.Scene.LoadJSON('{"format":"erd-pcb-scene","version":1,"items":[]}');
    A := Designer.AddDesignItem(piTimer, PointF(0,0)); B := Designer.AddDesignItem(piCounter, PointF(2,2));
    C := Designer.AddDesignItem(piOutputGroup, PointF(9,4)); Designer.FitBoard; Designer.Scene.SelectAll;
    Before := Designer.Scene.ToJSON; Designer.AlignSelected(paLeft);
    P := Designer.BoardToClient(PointF(Designer.Scene.Item(A).X, Designer.Scene.Item(A).Y));
    Q := Designer.BoardToClient(PointF(Designer.Scene.Item(B).X, Designer.Scene.Item(B).Y));
    Check(Abs(P.X - Q.X) <= 1, 'Align left respects fixed screen-size nonvisual glyphs');
    Designer.Undo; Check(Designer.Scene.ToJSON = Before, 'Aligning a selection is one undo operation');
    Designer.DistributeSelected(True);
    Check(Abs((Designer.Scene.Item(B).X - Designer.Scene.Item(A).X) -
      (Designer.Scene.Item(C).X - Designer.Scene.Item(B).X)) < 0.1, 'Distribution gives fixed glyphs equal gaps');
    Designer.Undo; Check(Designer.Scene.ToJSON = Before, 'Distribution is one undo operation');
    Value := Designer.Scene.Item(B); Value.Locked := True; Designer.Scene.UpdateItem(B, Value);
    Designer.AlignSelected(paLeft);
    Check(Designer.Scene.Item(B).X = Value.X, 'Alignment cannot move a locked object');
    Designer.Scene.Select(A); Designer.Scene.DeleteSelected; Designer.Scene.Undo;
    Check(Designer.Scene.Count = 3, 'Tool operations preserve subsequent delete and undo behavior');
    Designer.AddDesignItem(piNote, PointF(0,0)); Designer.FitBoard; Designer.BeginNoteEdit;
    // The finally block also exercises destruction while the canvas editor owns focus.
  finally Form.Free; end;
end;

procedure TestPCBDesigner;
var Form: TForm; Designer: TERDPCBDesigner; BoardFile, ProjectFile, Text: string;
    Index, SceneIndex, BusIndex: Integer; Item: TERDPCBDesignItem; TemplateFile: string; Failed: Boolean; Client: TPoint; Before, After: TPointF;
    Stream: TMemoryStream; CopyDesigner: TERDPCBDesigner; Pad: TERDPCBPad;
    Library: TERDPCBTemplateLibrary; LibraryDir, Imported, Duplicate, Invalid: string; Group: TERDPCBTerminal; Choices: TArray<string>; Snapshot: string;
begin
  Form := TForm.Create(nil); BoardFile := TPath.GetTempFileName; ProjectFile := TPath.GetTempFileName;
  try
    Designer := TERDPCBDesigner.Create(Form); Designer.Parent := Form; Designer.SetBounds(0, 0, 500, 300);
    Text := '{"schema":1,"units":"mm","y_axis":"up","source_sha256":"test-board",' +
      '"pads":[{"id":"U7.2","net":"OUT_GPIO4","reference":"U7","number":"2","position":[5,5],' +
      '"polygon":[[4,4],[6,4],[6,6],[4,6]],"drill":[]},' +
      '{"id":"U7.1","net":"12V","reference":"U7","number":"1","position":[2,5],' +
      '"polygon":[[1,4],[3,4],[3,6],[1,6]],"drill":[]}],' +
      '"terminals":[{"id":"U7","label":"WLED U7","signal_pin":"U7.2","members":["U7.1","U7.2"]}],' +
      '"channels":[{"pin":"U7.2","gpio":"GPIO4","route":"U6.4 to U7.2"}],' +
      '"components":[{"reference":"U7","device":"terminal","position":[5,5]}],' +
      '"silkscreen":[],"outline":[[[0,0],[10,0],[10,10],[0,10],[0,0]]]}';
    TFile.WriteAllText(BoardFile, Text, TEncoding.UTF8); Designer.LoadBoard(BoardFile);
    Check(Designer.TemplateBoardColor = RGB($17,$62,$4A), 'Older templates default to physical green solder mask');
    TemplateFile := TPath.GetTempFileName;
    try
      Designer.TemplateBoardColor := RGB($16,$40,$90); Designer.SaveTemplate(TemplateFile);
      Snapshot := TFile.ReadAllText(TemplateFile,TEncoding.UTF8);
      Check(Pos('"board_color":"#164090"',Snapshot) > 0, 'Template stores portable RGB mask color');
      Designer.LoadBoard(TemplateFile); Check(Designer.TemplateBoardColor = RGB($16,$40,$90), 'Template color survives reload');
      TFile.WriteAllText(TemplateFile,StringReplace(Snapshot,'#164090','not-a-color',[]),TEncoding.UTF8); Failed := False;
      try Designer.LoadBoard(TemplateFile); except on E: EConvertError do Failed := True; end;
      Check(Failed and (Designer.TemplateBoardColor = RGB($16,$40,$90)), 'Malformed template colors reject before changing the loaded appearance');
    finally TFile.Delete(TemplateFile); end;
    Designer.LoadBoard(BoardFile);

    Form.Show; Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(250, 150));
    Check(Designer.SelectedPin = 'U7.2', 'PCB hit testing selects a pad in board coordinates');
    Check(Designer.SelectedPad.Net = 'OUT_GPIO4', 'PCB selection retains electrical identity');
    Pad := Designer.SelectedPad; Pad.Polygon[0] := PointF(-100, -100);
    Check(Designer.SelectedPad.Polygon[0].X = 4, 'Selected pad geometry cannot mutate the imported board');
    Client := Point(325, 120); Before := Designer.ClientToBoard(Client);
    Designer.ZoomAt(2, Client); After := Designer.ClientToBoard(Client);
    Check((Abs(Before.X - After.X) < 0.001) and (Abs(Before.Y - After.Y) < 0.001), 'PCB zoom keeps the pointer on the same board position');
    Designer.PanBy(13, -8); Client := Designer.BoardToClient(PointF(5, 5));
    Check(Designer.HitTestPin(Client) = 'U7.2', 'PCB hit testing follows zoom and pan');
    Designer.Perform(WM_MOUSEMOVE, 0, MakeLParam(Client.X, Client.Y));
    Check(Designer.HotPin = 'U7.2', 'PCB hover resolves electrical pin identity');
    Designer.Perform(CM_MOUSELEAVE, 0, 0); Check(Designer.HotPin = '', 'PCB hover clears when leaving the control');
    Designer.ShowPads := False; Check(Designer.HitTestPin(Client) = '', 'Hidden pads cannot intercept selection');
    Designer.ShowPads := True;
    Designer.Perform(WM_KEYDOWN, VK_LEFT, 0);
    Check(Designer.SelectedPin = 'U7.2', 'Arrow navigation does not select raw power or MCU pins');
    Client := Designer.BoardToClient(PointF(2,5)); Check(Designer.HitTestPin(Client) = 'U7.2', 'A terminal power contact selects its programmable signal as one terminal');
    Designer.SelectedPin := 'U7.2';
    SceneIndex := Designer.AddDesignItem(piNote, PointF(12,2));
    Item := Designer.Scene.Item(SceneIndex); Item.Width := 3; Item.Height := 3; Designer.Scene.UpdateItem(SceneIndex, Item);
    Form.Show; Designer.SetFocus; Designer.SnapToGrid := False;
    Client := Designer.BoardToClient(PointF(Item.X + Item.Width / 2, Item.Y + Item.Height / 2));
    Designer.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(Client.X, Client.Y));
    Designer.Perform(WM_MOUSEMOVE, MK_LBUTTON, MakeLParam(Client.X + 20, Client.Y + 10));
    Before := PointF(Designer.Scene.Item(SceneIndex).X, Designer.Scene.Item(SceneIndex).Y);
    Check(Abs(Before.X - Item.X) > 0.001, 'Nonvisual component follows mouse drag');
    Designer.Perform(WM_LBUTTONUP, 0, MakeLParam(Client.X + 20, Client.Y + 10));
    After := PointF(Designer.Scene.Item(SceneIndex).X, Designer.Scene.Item(SceneIndex).Y);
    Check((Abs(Before.X - After.X) < 0.001) and (Abs(Before.Y - After.Y) < 0.001),
      'Releasing capture after a drop preserves the new component position');
    Designer.Undo;
    Check(Abs(Designer.Scene.Item(SceneIndex).X - Item.X) < 0.001,
      'Mouse drag is committed as one undoable edit');
    Designer.SnapToGrid := True;
    Stream := TMemoryStream.Create;
    try
      RegisterClass(TERDPCBDesigner);
      Designer.ShowLabels := False; Designer.ShowDrills := False; Designer.PadColor := clRed;
      Stream.WriteComponent(Designer); Stream.Position := 0;
      DeleteFile(BoardFile); // An embedded board must not need the original file.
      CopyDesigner := TERDPCBDesigner(Stream.ReadComponent(nil));
      try
        Check(not CopyDesigner.ShowLabels and not CopyDesigner.ShowDrills and (CopyDesigner.PadColor = clRed),
          'PCB view properties round-trip through DFM streaming');
        Check(SameValue(CopyDesigner.Zoom, Designer.Zoom), 'Embedded board loading preserves the streamed zoom');
        CopyDesigner.SelectedPin := 'U7.2';
        Check(CopyDesigner.SelectedPad.Net = 'OUT_GPIO4', 'DFM embeds physical board data independently of the source path');
        Check(CopyDesigner.Scene.Count = 1, 'DFM preserves the nonvisual designer scene');
        Check(CopyDesigner.BoardFileName = '', 'DFM does not store an absolute source-machine path');
        CopyDesigner.ClearBoard; Check(CopyDesigner.HitTestPin(Point(0, 0)) = '', 'Clearing a board removes selectable geometry');
      finally CopyDesigner.Free; end;
    finally Stream.Free; end;
    Designer.FitBoard; Designer.SelectedPin := 'U7.2';
    Index := Designer.ChannelIndex('U7.2'); Check(Index = 0, 'PCB output resolves independently of geometry');
    Designer.SetBinding(Index, 'PickA', 'showPick', 24); Designer.SaveProject(ProjectFile);
    Designer.SetBinding(Index, 'Changed', '', 1); Designer.SelectedPin := '';
    Designer.LoadProject(ProjectFile);
    Check((Designer.SelectedPin = 'U7.2') and (Designer.Channel(Index).Name = 'PickA') and
      (Designer.Channel(Index).LEDCount = 24), 'PCB project restores selection and assignments');
    Text := TFile.ReadAllText(ProjectFile, TEncoding.UTF8);
    TFile.WriteAllText(ProjectFile, StringReplace(Text, 'showPick', 'invalidAction', []), TEncoding.UTF8);
    Failed := False;
    try Designer.LoadProject(ProjectFile); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Channel(Index).Name = 'PickA') and (Designer.Channel(Index).Action = 'showPick'),
      'Invalid project load rolls back every assignment');
    TFile.WriteAllText(ProjectFile, StringReplace(Text, 'test-board', 'other-board', []), TEncoding.UTF8);
    Failed := False;
    try Designer.LoadProject(ProjectFile); except on E: EConvertError do Failed := True; end;
    Check(Failed, 'PCB project cannot silently bind to a different physical board');
    Check(Designer.Scene.Count = 1, 'Failed project load preserves designer objects');
    Designer.TemplateEditMode := True;
    Designer.SetPinCapabilities('U7.1', [pcUARTTx, pcCANTx], 'GPIO1', 'TX');
    Designer.SetPinCapabilities('U7.2', [pcUARTRx, pcCANRx], 'GPIO2', 'RX');
    Check(not Designer.SupportsPinRole(0, prPrimary) and (Length(Designer.GetAssignablePins(0, prPrimary)) = 0), 'Notes expose no pin-role choices');
    Snapshot := Designer.Scene.ContentJSON; Failed := False;
    try Designer.AssignPin(0, prPrimary, 'U7.2'); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Scene.ContentJSON = Snapshot), 'Role assignment API cannot connect a note');
    BusIndex := Designer.AddDesignItem(piUART, PointF(1,1));
    Check(Designer.SupportsPinRole(BusIndex, prSecondary) and not Designer.SupportsPinRole(BusIndex, prClock), 'UART roles are explicit in the host API');
    Choices := Designer.GetAssignablePins(BusIndex, prPrimary);
    Check((Length(Choices) = 1) and (Choices[0] = 'U7.1'), 'Primary pin choices filter incompatible capabilities');
    Designer.ValidatePinAssignment(BusIndex, prPrimary, 'U7.1');
    Check(Designer.Scene.Item(BusIndex).Pin = '', 'Pin validation is read-only');
    Designer.AssignPin(BusIndex, prPrimary, 'U7.1'); Designer.AssignPin(BusIndex, prSecondary, 'U7.2');
    Designer.AssignPin(BusIndex, prSecondary, ''); Designer.Undo;
    Check(Designer.Scene.Item(BusIndex).SecondaryPin = 'U7.2', 'Clearing a pin role is undoable');
    Snapshot := Designer.Scene.ContentJSON; Failed := False;
    try Designer.AssignPin(BusIndex, prClock, 'U7.1'); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Scene.ContentJSON = Snapshot), 'Unsupported role assignment preserves the scene');
    SceneIndex := Designer.AddDesignItem(piCANBus, PointF(2,2)); Item := Designer.Scene.Item(SceneIndex);
    Check(Length(Designer.GetAssignablePins(SceneIndex, prPrimary)) = 0, 'Pins owned by another object are omitted from API choices');
    Item.Pin := 'U7.1'; Item.SecondaryPin := 'U7.2'; Failed := False;
    try Designer.Scene.UpdateItem(SceneIndex, Item); except on E: EArgumentException do Failed := True; end;
    Check(Failed, 'Two enabled bus components cannot claim the same physical pins');
    Item := Designer.Scene.Item(BusIndex); Item.Enabled := False; Designer.Scene.UpdateItem(BusIndex, Item);
    Check(Length(Designer.GetAssignablePins(SceneIndex, prPrimary)) = 0, 'Disabled objects retain their pin reservations in host pick lists');
    Snapshot := Designer.Scene.ContentJSON; Failed := False;
    try Designer.AssignPin(SceneIndex, prPrimary, 'U7.1'); except on E: EArgumentException do Failed := True; end;
    Check(Failed and (Designer.Scene.ContentJSON = Snapshot), 'The assignment API rejects a stale occupied-pin choice');
    Item := Designer.Scene.Item(SceneIndex); Item.Pin := 'U7.1'; Item.SecondaryPin := 'U7.2'; Designer.Scene.UpdateItem(SceneIndex, Item);
    Failed := False;
    try Designer.SetPinCapabilities('U7.1', [pcDigitalOutput], '', ''); except on E: EArgumentException do Failed := True; end;
    Check(Failed, 'Template capabilities cannot invalidate existing component connections');
    TemplateFile := TPath.GetTempFileName;
    try
      Group := Designer.Terminal(0); Designer.RemoveTerminal(Group.ID);
      Check((Designer.TerminalCount = 0) and (Designer.ChannelCount = 2), 'Removing a terminal group preserves programmable pins');
      Designer.DefineTerminal(Group.ID, Group.LabelText, Group.SignalPin, Group.Members);
      Check(Designer.TerminalCount = 1, 'Terminal group contacts can be restored through the authoring API');
      Designer.SaveTemplate(TemplateFile);
      LibraryDir := TPath.GetTempFileName; System.SysUtils.DeleteFile(LibraryDir);
      Library := TERDPCBTemplateLibrary.Create(LibraryDir);
      try
        Imported := Library.ImportTemplate(TemplateFile);
        Check((Library.Count = 1) and (Library.Item(0).PinCount = 2), 'Template library imports a validated reusable board');
        Duplicate := Library.ImportTemplate(TemplateFile);
        Check((Imported <> Duplicate) and (Library.Count = 2), 'Template import never overwrites an existing file');
        Invalid := TPath.Combine(LibraryDir, 'invalid.json'); TFile.WriteAllText(Invalid, '{broken', TEncoding.UTF8);
        Library.Refresh;
        Check((Library.Count = 2) and (Library.Issues.Count = 1), 'Library reports malformed templates while retaining usable entries');
      finally Library.Free; if TDirectory.Exists(LibraryDir) then TDirectory.Delete(LibraryDir, True); end;
      CopyDesigner := TERDPCBDesigner.Create(nil);
      try
        CopyDesigner.LoadBoard(TemplateFile);
        Check((CopyDesigner.TemplateID = Designer.TemplateID) and (pcCANTx in CopyDesigner.Channel(CopyDesigner.ChannelIndex('U7.1')).Capabilities),
          'Templates preserve multifunction UART/CAN pin capabilities');
      finally CopyDesigner.Free; end;
    finally System.SysUtils.DeleteFile(TemplateFile); end;
  finally
    Form.Free; DeleteFile(BoardFile); DeleteFile(ProjectFile);
  end;
end;

procedure TestSignatureHelp;
var Form: TForm; Editor: TERDCodeEditor; Service: TLanguageServiceProbe;
    ServiceReference: IERDCodeLanguageService; Responses: IERDSignatureHelpResponses;
    Current, Old: TERDLanguageRequest; Help, Returned: TERDSignatureHelp;
    Value: TJSONValue; I: Integer; Popup: TERDSignaturePopup;
begin
  Form := TForm.Create(nil); Service := TLanguageServiceProbe.Create; ServiceReference := Service;
  try
    Editor := TERDCodeEditor.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 500, 200);
    Editor.Perform(WM_CHAR, Ord('a'), 0);
    Check(Editor.Text = 'a', 'Unconnected code editor forwards ordinary typing to the text area');
    Editor.LanguageService := ServiceReference;
    Editor.Perform(WM_CHAR, Ord('b'), 0);
    Check(Editor.Text = 'ab', 'Connected code editor forwards ordinary typing to the text area');
    Editor.Perform(WM_CHAR, Ord('.'), 0);
    Check(Editor.Text = 'ab.', 'Member completion inserts the dot before requesting suggestions');
    Editor.LanguageService := ServiceReference; Editor.Text := 'fn('; Editor.SelStart := 3; Form.Show; Editor.SetFocus;
    Check(Editor.RequestSignatureHelp and (Service.Snapshot.Kind = lrSignatureHelp), 'Signature help issues a versioned language request');
    Check(Supports(Service.Responses, IERDSignatureHelpResponses, Responses), 'Optional signature response extension is available');
    Current := Service.Snapshot;
    Value := TJSONObject.ParseJSONValue('{"activeSignature":0,"activeParameter":1,"signatures":[{"label":"fn(first: string, second: number): void","documentation":{"kind":"markdown","value":"Call documentation"},"parameters":[{"label":[3,16]},{"label":"second: number","documentation":"Second argument"}]},{"label":"fn(value: number): void","activeParameter":0,"parameters":[{"label":[3,16]}]}]}');
    try Help := ERDParseSignatureHelp(Value); finally Value.Free; end;
    Check((Help.Signatures[0].Parameters[0].Start = 3) and (Help.Signatures[0].Parameters[1].Start = 18),
      'LSP signature labels support both UTF-16 ranges and string labels');
    Check((Help.Signatures[0].Documentation = 'Call documentation') and
      (Help.Signatures[0].Parameters[1].Documentation = 'Second argument'), 'Signature and parameter documentation are preserved');
    Check(Responses.SignatureHelp(Current, Help), 'Current signature reply is accepted');
    Returned := Editor.SignatureHelp; Returned.Signatures[0].Parameters[0].Start := 99;
    Check(Editor.SignatureHelp.Signatures[0].Parameters[0].Start = 3, 'Signature getter does not expose mutable parameter arrays');
    Popup := nil;
    for I := 0 to Editor.ComponentCount - 1 do
      if Editor.Components[I] is TERDSignaturePopup then Popup := TERDSignaturePopup(Editor.Components[I]);
    Check((Popup <> nil) and Popup.IsOpen and (GetFocus = Editor.Handle), 'Parameter hint opens without taking editor focus');
    Editor.SelectSignature(1); Check(Editor.SignatureHelp.ActiveSignature = 1, 'Host can choose a signature overload');
    Editor.Perform(WM_KEYDOWN, VK_ESCAPE, 0);
    Check(not Popup.IsOpen, 'Escape closes parameter hint');
    Editor.RequestSignatureHelp; Old := Service.Snapshot; Editor.Text := 'fn(x';
    Check(not Responses.SignatureHelp(Old, Help), 'Edited document rejects stale signature replies');
    Editor.Text := 'fn'; Editor.SelStart := 2; Editor.Perform(WM_CHAR, Ord('('), 0);
    Check((Service.Snapshot.Kind = lrSignatureHelp) and (Service.Snapshot.Text = 'fn()') and
      (Service.Snapshot.Caret = 3), 'Auto-paired opening parenthesis requests final caret signature context');
    Current := Service.Snapshot; Check(Responses.SignatureHelp(Current, Default(TERDSignatureHelp)), 'Empty signature reply clears the session');
    Editor.Text := '"text'; Editor.SelStart := Length(Editor.Text); I := Service.Requests;
    Editor.Perform(WM_CHAR, Ord('('), 0);
    Check(Service.Requests = I, 'Parenthesis inside an unterminated string does not trigger parameter help');
    Editor.Text := 'fn('; Editor.SelStart := 3; Editor.RequestSignatureHelp; Current := Service.Snapshot;
    Check(Responses.SignatureHelp(Current, Help), 'Signature popup reopens before detach');
    Editor.Parent := nil; Editor.CancelLanguageRequests; Editor.Free;
    Check(not Responses.SignatureHelp(Current, Help), 'Detached signature sink rejects replies after editor destruction');
  finally Form.Free; ServiceReference := nil; end;
end;

procedure TestEditorWindowShutdown;
var Form: TForm; Editor: TCodeEditorProbe; Service: TLanguageServiceProbe;
    ServiceReference: IERDCodeLanguageService; Sink: IERDLanguageResponses;
    Snapshot: TERDLanguageRequest; Items: TArray<TERDCompletionItem>;
    Popup: TERDPopupList; Mode, I: Integer; ModelAlive: Boolean;
begin
  for Mode := 0 to 3 do
  begin
    Form := TForm.Create(nil); ModelAlive := False;
    Service := TLanguageServiceProbe.Create; ServiceReference := Service;
    try
      Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form;
      Editor.ShutdownModelAlive := @ModelAlive; Editor.Text := 'shutdown';
      Editor.LanguageService := ServiceReference; Form.Show; Editor.SetFocus;
      Editor.RequestCompletion; Sink := Service.Responses; Snapshot := Service.Snapshot;
      if Mode > 0 then
      begin
        SetLength(Items, 40);
        for I := 0 to High(Items) do
        begin
          Items[I] := Default(TERDCompletionItem); Items[I].LabelText := 'item' + IntToStr(I);
          Items[I].InsertText := Items[I].LabelText;
        end;
        Check(Sink.Completion(Snapshot, Items), 'Shutdown fixture opens completion popup');
        if Mode = 2 then
          for I := 0 to Editor.ComponentCount - 1 do
            if Editor.Components[I] is TERDPopupList then
            begin
              Popup := TERDPopupList(Editor.Components[I]);
              Popup.Perform(WM_LBUTTONDOWN, MK_LBUTTON, MakeLParam(20, 12));
              Check(GetCapture = Popup.Handle, 'Shutdown fixture holds popup mouse capture');
            end;
      end;
      Editor.RequestHover; Sink := Service.Responses; Snapshot := Service.Snapshot;
      if Mode = 3 then
      begin
        // Detaching destroys the parent/child HWNDs before cancellation.
        // Closing the popup must not recreate them through ItemIndex.
        Editor.Parent := nil;
        Editor.CancelLanguageRequests; Editor.CancelLanguageRequests;
        Check(not Editor.HandleAllocated, 'Closing detached completion never recreates its editor HWND');
      end;
      FreeAndNil(Form);
      Check(ModelAlive, 'Editor HWND is destroyed before its text model');
      Check(not Sink.Hover(Snapshot, 'late shutdown reply'), 'Response sink is detached after owner form closes');
      Application.ProcessMessages;
    finally Form.Free; Sink := nil; ServiceReference := nil; end;
  end;
end;

procedure TestCodeEditorSearch;
var Form: TForm; Editor: TCodeEditorProbe; Matches: TArray<TERDCodeSearchMatch>;
    Before: UnicodeString; Rows: Integer;
begin
  Form := TForm.Create(nil);
  try
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 460, 180);
    Form.Show; Editor.SetFocus;
    Editor.Text := 'count Count counter $count';
    Check(Editor.SetSearch('count'), 'Literal search compiles'); Matches := Editor.SearchMatches;
    Check((Length(Matches) = 4) and (Matches[0].Start = 0) and (Matches[1].Start = 6),
      'Literal search reports zero-based UTF-16 positions');
    Check(Editor.SetSearch('count', [soWholeWord]) and (Length(Editor.SearchMatches) = 2),
      'Whole-word search excludes identifier suffix and dollar prefix');
    Editor.SetSearch('count', [soWholeWord, soMatchCase]);
    Check(Length(Editor.SearchMatches) = 1, 'Case-sensitive search filters uppercase variant');
    Editor.SetSearch('count'); Editor.SelStart := 0;
    Check(Editor.FindNext and (Editor.SelStart = 0) and (Editor.SelLength = 5), 'Find selects first match');
    Check(Editor.FindNext and (Editor.SelStart = 6), 'Find advances after selected match');
    Check(Editor.FindNext(True) and (Editor.SelStart = 0), 'Previous moves backwards');
    Check(Editor.FindNext(True) and (Editor.SelStart = 21), 'Previous wraps at document beginning');
    Matches := Editor.SearchMatches; Matches[0].Start := 999;
    Check(Editor.SearchMatches[0].Start = 0, 'Search results are copied');
    Editor.SetSearch('count', [], 6, 5);
    Check((Length(Editor.SearchMatches) = 1) and (Editor.SearchMatches[0].Start = 6), 'Selection-scoped search limits full matches');
    Editor.Text := 'a.b aXb'; Editor.SetSearch('a.b');
    Check(Length(Editor.SearchMatches) = 1, 'Literal search escapes regex metacharacters');
    Editor.SetSearch('a.b', [soRegex]);
    Check(Length(Editor.SearchMatches) = 2, 'Regex search recognises metacharacters');
    Before := Editor.Text;
    Check(not Editor.SetSearch('(', [soRegex]) and (Editor.SearchError <> '') and
      (Length(Editor.SearchMatches) = 0) and (Editor.Text = Before), 'Invalid regex reports error without editing document');
    Editor.Text := 'foo foo'; Editor.SelectAll; Editor.SetSearch('foo');
    Check(Editor.ReplaceAll('bar') = 2, 'Replace All reports replacement count');
    Check(Editor.Text = 'bar bar', 'Replace All changes all occurrences'); Editor.Undo;
    Check((Editor.Text = 'foo foo') and (Editor.SelLength = 7) and
      (Length(Editor.SearchMatches) = 2), 'Replace All undo restores text, selection and search results');
    Editor.SelLength := 0; Editor.SelStart := 0; Editor.FindNext;
    Check(Editor.ReplaceCurrent('longer') and (Editor.Text = 'longer foo'), 'Replace selected match');
    Editor.Undo; Editor.ReadOnly := True;
    Check((Editor.ReplaceAll('x') = 0) and not Editor.ReplaceCurrent('x') and (Editor.Text = 'foo foo'),
      'ReadOnly blocks search replacements'); Editor.ReadOnly := False;
    Editor.MaxLength := 7;
    Check((Editor.ReplaceAll('longer') = 0) and (Editor.Text = 'foo foo'), 'MaxLength rejects oversized replacement atomically');
    Editor.MaxLength := 0; Editor.Text := 'ab'; Editor.SetSearch('(?=.)', [soRegex]);
    Check((Editor.ReplaceAll('_') = 2) and (Editor.Text = '_a_b'), 'Zero-length regex matches replace without infinite loop');
    Editor.Text := #$D83D#$DC4D#$D83C#$DFFD; Editor.SetSearch(#$D83D#$DC4D);
    Check(Length(Editor.SearchMatches) = 0, 'Search cannot select part of an emoji grapheme');
    Editor.SetSearch(Editor.Text);
    Check((Length(Editor.SearchMatches) = 1) and (Editor.SearchMatches[0].Count = 4), 'Search matches complete emoji grapheme');
    Editor.Text := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}';
    Editor.SelStart := 0; Rows := Editor.RowCount; Editor.ToggleFold(0);
    Editor.SetSearch('return'); Editor.FindNext;
    Check(Editor.RowCount = Rows, 'Finding hidden text expands its fold');
    Editor.ClearSearch; Check(Length(Editor.SearchMatches) = 0, 'Clear Search removes highlights');
  finally Form.Free; end;
end;

procedure TTextInputProbe.RebuildWindow;
begin RecreateWnd; end;

procedure TestSharedColorEmoji;
var Form: TForm; Input: TTextInputProbe; Area: TERDTextArea; Theme: TERDTheme;
    Bitmap: TBitmap; Layout: TERDTextLayoutLine; X, Y, L, R, CachedWidth: Integer;
    HitsBefore, HitsAfter, Misses: UInt64;
  function ColorPixels: Integer;
  var X, Y: Integer; C: COLORREF;
  begin
    Result := 0;
    for Y := 4 to Bitmap.Height - 5 do
      for X := 7 to Bitmap.Width - 8 do
      begin
        C := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]);
        if (GetRValue(C) > 150) and (GetGValue(C) > 160) and
          (GetBValue(C) < 100) then Inc(Result);
      end;
  end;
begin
  Check(ERDTextLayoutABIValid, 'DirectWrite and Direct2D ABI record sizes');
  Form := TForm.Create(nil); Bitmap := TBitmap.Create; Theme := TERDTheme.Create(nil);
  try
    Input := TTextInputProbe.Create(Form); Input.Parent := Form; Input.Theme := Theme;
    Input.SetBounds(8, 8, 220, 56); Input.Font.Size := 24;
    Input.Text := #$D83D#$DE00;
    Area := TERDTextArea.Create(Form); Area.Parent := Form; Area.Theme := Theme;
    Area.SetBounds(8, 72, 220, 80); Area.Font.Size := 24; Area.Text := Input.Text;
    Form.Show; Application.ProcessMessages;
    Check(IsWindow(Input.Handle), 'Shared Edit creates its window with emoji text');
    Input.RebuildWindow;
    Check(IsWindow(Input.Handle), 'Shared Edit recreates its window without a layout exception');
    Check(Input.Text = Area.Text, 'Window recreation preserves emoji text');
    Theme.Mode := tmERDesignsLight; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Shared Edit base paints coloured emoji, not monochrome glyphs');
    ERDTextRasterCacheStats(HitsBefore, Misses); Input.RenderTo(Bitmap);
    ERDTextRasterCacheStats(HitsAfter, Misses);
    Check(HitsAfter > HitsBefore, 'Unchanged emoji repaint uses the transparent glyph cache');
    Theme.Mode := tmERDesignsDark; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Emoji intrinsic colours survive dark theme');
    Area.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Multiline base inherits colour emoji');
    Input.SetFocus; Input.SelectAll; Input.RenderTo(Bitmap);
    Check(ColorPixels > 20, 'Themed selection preserves emoji colours');
    Input.PasswordChar := #$2022; Input.RenderTo(Bitmap);
    // Exclude the selection's theme accent by clearing the selection first.
    Input.SelLength := 0; Input.RenderTo(Bitmap);
    Check(ColorPixels = 0, 'Password masking does not reveal coloured emoji');
    Input.PasswordChar := #0; Input.SelLength := 0;
    Bitmap.Canvas.Font.Assign(Input.Font);
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try
      X := Layout.X(1); Y := Layout.X(3); Layout.Range(1, 2, L, R);
      Check((Y > X) and (L <= X) and (R >= Y), 'Colour emoji caret and selection use the same layout');
      Check(Layout.Width >= Y, 'Layout width contains final emoji trailing edge');
      Check(Layout.Hit(Y + 20) = 3, 'Native hit testing stops at final emoji boundary');
      CachedWidth := Layout.Width;
    finally Layout.Free; end;
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try Check(Layout.Width = CachedWidth, 'Cached layout preserves shaped width'); finally Layout.Free; end;
    Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size * 2;
    Layout := TERDTextLayoutLine.Create(Bitmap.Canvas, 'A' + #$D83D#$DE00);
    try Check(Layout.Width > CachedWidth, 'Font size changes do not reuse stale cached layout'); finally Layout.Free; end;
  finally Theme.Free; Bitmap.Free; Form.Free; end;
end;

procedure TestSyntaxAndCodeEditor;
var Provider: IERDSyntaxProvider; Analysis: TERDSyntaxAnalysis; Span: TERDSyntaxSpan;
    Source, Original: UnicodeString; I, KeywordCount, Rows, Opening, Closing: Integer;
    Form: TForm; Editor: TCodeEditorProbe; Items: TArray<TERDCodeDiagnostic>;
    Diagnostic: TERDCodeDiagnostic; Stream: TMemoryStream; Restored: TERDCodeEditor;
  function TokenKind(const Token: string): TERDSyntaxKind;
  var Item: TERDSyntaxSpan; At: Integer;
  begin
    At := Pos(Token, Source) - 1; Result := skText;
    for Item in Analysis.Spans do
      if Item.Start = At then Exit(Item.Kind);
  end;
begin
  Provider := TERDBasicSyntaxProvider.Create(clTypeScript);
  Source := 'export interface Item {' + #13#10 +
    '  title: string; // } ignored' + #13#10 +
    '  value: number;' + #13#10 + '}' + #13#10 +
    'const pattern = /[{}]/g;' + #13#10 +
    'const text = `braces { }`;' + #13#10 + 'const sum = 1+2;';
  Analysis := Provider.Analyze(Source);
  Check(TokenKind('export') = skModuleKeyword, 'TypeScript module keyword highlighting');
  Check(TokenKind('interface') = skInterfaceKeyword, 'TypeScript interface keyword highlighting');
  Check(TokenKind('string') = skKeyword, 'TypeScript type highlighting');
  Check(TokenKind('//') = skComment, 'TypeScript line comments');
  Check(TokenKind('/[{}]/g') = skString, 'TypeScript regexp hides braces from folding');
  Check(TokenKind('`braces') = skString, 'TypeScript template string highlighting');
  Check(Length(Analysis.Folds) = 1, 'TypeScript folds ignore comments, regexps and strings');
  I := Pos('1+2', Source) - 1;
  for Span in Analysis.Spans do
    if Span.Start = I then Check((Span.Kind = skNumber) and (Span.Count = 1),
      'Numeric token stops before addition operator');
  Source := 'export class Example {}'; Analysis := Provider.Analyze(Source);
  Check(TokenKind('class') = skClassKeyword, 'TypeScript class keyword highlighting');
  Source := 'Const const'; Analysis := Provider.Analyze(Source); KeywordCount := 0;
  for Span in Analysis.Spans do if Span.Kind = skKeyword then Inc(KeywordCount);
  Check(KeywordCount = 1, 'TypeScript keywords are case sensitive');
  Provider := TERDBasicSyntaxProvider.Create(clPascal);
  Source := 'BEGIN' + #13#10 + '  S := ''end''; // end' + #13#10 + 'END;';
  Analysis := Provider.Analyze(Source);
  Check((TokenKind('BEGIN') = skKeyword) and (Length(Analysis.Folds) = 1),
    'Pascal provider ignores end inside comments and strings');
  Provider := TERDBasicSyntaxProvider.Create(clJSON);
  Source := '{' + #13#10 + ' "key": [true, 1]' + #13#10 + '}';
  Analysis := Provider.Analyze(Source);
  Check((TokenKind('true') = skKeyword) and (Length(Analysis.Folds) = 1), 'JSON provider folds multiline objects');

  RegisterClass(TERDCodeEditor); RegisterClass(TCodeEditorProbe); Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form;
    Editor.SetBounds(8, 8, 480, 180); Form.Show; Editor.SetFocus;
    Original := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}' + #13#10;
    Editor.Text := Original; Editor.SelStart := 0; Rows := Editor.RowCount;
    Editor.GutterMouse(0, 1); Editor.GutterMouse(2, 1);
    Check(Editor.SelText = '  return true;' + #13#10, 'Gutter click selects a logical line including its newline');
    Editor.GutterMouse(3, 1); Editor.GutterMouse(2, 1);
    Check(Editor.SelText = '  return true;' + #13#10, 'Gutter double click keeps the whole line selected');
    Editor.GutterMouse(0, 0); Editor.GutterMouse(1, 1); Editor.GutterMouse(2, 1);
    Check(Editor.SelText = 'function run() {' + #13#10 + '  return true;' + #13#10,
      'Dragging in gutter selects whole logical lines');
    Editor.GutterMouse(0, 2); Editor.GutterMouse(1, 1); Editor.GutterMouse(2, 1);
    Check(Editor.SelText = '  return true;' + #13#10 + '}' + #13#10,
      'Upward gutter drag includes both anchor and target lines');
    Editor.GutterMouse(0, 0, True); Editor.GutterMouse(2, 0, True);
    Check(Editor.SelText = Original, 'Shift gutter click keeps the original anchor line when extending upward');
    Editor.Text := 'first' + #13#10 + 'last';
    Editor.GutterMouse(0, 1); Editor.GutterMouse(2, 1);
    Check(Editor.SelText = 'last', 'Final line without a newline selects to document end');
    Editor.Text := Original; Editor.SelStart := 0;

    Editor.ShowFoldGuides := True;
    Check(Editor.FoldPixel(True) <> Editor.GutterBackground, 'Expanded fold has a visible connector');
    Editor.ShowFoldGuides := False;
    Check(Editor.FoldPixel(True) = Editor.GutterBackground, 'Guide property hides connector');
    Editor.ShowFoldGuides := True;
    Editor.FoldMouse(0); Check(Editor.Cursor = crArrow, 'Fold gutter uses the arrow cursor');
    Check(Editor.FoldPixel(False) <> Editor.GutterBackground, 'Fold hover paints a visible fill');
    Editor.FoldMouse(1); Check(Editor.RowCount = Rows, 'Fold waits for mouse release');
    Editor.FoldMouse(0, True); Editor.FoldMouse(2, True);
    Check(Editor.RowCount = Rows, 'Releasing outside cancels fold click');
    Editor.FoldMouse(0); Editor.FoldMouse(1); Editor.FoldMouse(2);
    Check((Editor.RowCount < Rows) and (Editor.Text = Original), 'Folding changes rows without changing document');
    Editor.SelStart := Pos('return', Original) - 1;
    Check(Editor.RowCount = Rows, 'Moving caret into hidden text expands fold');
    Editor.SelectAll; Editor.IndentSelection;
    Check(Editor.Text = '  function run() {' + #13#10 + '    return true;' + #13#10 + '  }' + #13#10,
      'Indent operates on selected logical lines');
    Editor.Undo;
    Check((Editor.Text = Original) and (Editor.SelStart = 0) and (Editor.SelLength = Length(Original)),
      'Indent is one undo transaction and restores original selection');
    Editor.Text := '  const value = 1;'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_CHAR, 13, 0);
    Check(Editor.Text = '  const value = 1;' + #13#10 + '  ', 'Enter keeps leading indentation');
    Editor.Text := ''; Editor.Perform(WM_KEYDOWN, VK_TAB, 0); Editor.Perform(WM_CHAR, 9, 0);
    Check(Editor.Text = '  ', 'Tab inserts one configured indentation');
    Editor.ReadOnly := True; Editor.IndentSelection;
    Check(Editor.Text = '  ', 'ReadOnly prevents indentation'); Editor.ReadOnly := False;
    Editor.Text := Original;
    SetLength(Items, 2);
    Items[0].Start := Pos('return', Original) - 1; Items[0].Count := 6;
    Items[0].Severity := dsWarning; Items[0].MessageText := 'Host warning';
    Items[1] := Items[0]; Items[1].Severity := dsError; Items[1].MessageText := 'Host error';
    Check(Editor.PublishDiagnostics(Original, Items), 'Host diagnostics accept matching source snapshot');
    Items[0].MessageText := 'Mutated';
    Check(Editor.Diagnostics[0].MessageText = 'Host warning', 'Published diagnostics are copied');
    Check(Editor.DiagnosticAt(Items[0].Start, Diagnostic) and (Diagnostic.Severity = dsError),
      'Overlapping diagnostic lookup returns highest severity');
    Check(not Editor.PublishDiagnostics('old source', Items) and (Length(Editor.Diagnostics) = 2),
      'Stale diagnostic response rejected without replacing current ranges');
    Editor.SelStart := Length(Editor.Text); Editor.ReplaceSelection(' ');
    Check(Length(Editor.Diagnostics) = 0, 'Document edits clear outdated diagnostic offsets');
    Editor.Text := ''; Editor.Perform(WM_CHAR, Ord('('), 0);
    Check((Editor.Text = '()') and (Editor.SelStart = 1), 'Opening delimiter inserts pair and keeps caret inside');
    Editor.Perform(WM_CHAR, Ord(')'), 0);
    Check((Editor.Text = '()') and (Editor.SelStart = 2), 'Typing existing closing delimiter advances caret');
    Editor.Undo; Check(Editor.Text = '', 'Pair insertion is one undo transaction');
    Editor.Text := '()'; Editor.SelStart := 1;
    Editor.Perform(WM_KEYDOWN, VK_BACK, 0); Editor.Perform(WM_CHAR, 8, 0);
    Check(Editor.Text = '', 'Backspace between empty delimiters removes pair');
    Editor.Undo; Check((Editor.Text = '()') and (Editor.SelStart = 1), 'Paired deletion undo restores caret');
    Editor.Text := #$D83D#$DC69#$200D#$D83D#$DCBB; Editor.SelectAll;
    Editor.Perform(WM_CHAR, Ord('['), 0);
    Check((Editor.Text = '[' + #$D83D#$DC69#$200D#$D83D#$DCBB + ']') and (Editor.SelLength = 5),
      'Surround selection preserves Unicode grapheme');
    Editor.Undo; Check(Editor.SelLength = 5, 'Surround undo restores original selection');
    Editor.Text := '  const value = 1;'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_KEYDOWN, VK_HOME, 0);
    Check(Editor.SelStart = 2, 'Smart Home moves to first non-whitespace character');
    Editor.Perform(WM_KEYDOWN, VK_HOME, 0);
    Check(Editor.SelStart = 0, 'Second Home moves to logical line start');
    Editor.Text := '{}'; Editor.SelStart := 1; Editor.Perform(WM_CHAR, 13, 0);
    Check((Editor.Text = '{' + #13#10 + '  ' + #13#10 + '}') and (Editor.SelStart = 5),
      'Enter between delimiters creates indented body and closing line');
    Editor.Text := '{ "}"; // }' + #13#10 + '}'; Editor.SelStart := 0;
    Check(Editor.MatchingBrackets(Opening, Closing) and (Opening = 0) and
      (Closing = Length(Editor.Text) - 1), 'Bracket matching ignores comment and string brackets');
    Editor.Text := 'Map<string, Array<number>>'; Editor.SelStart := 3;
    Check(Editor.MatchingBrackets(Opening, Closing) and (Opening = 3) and
      (Closing = Length(Editor.Text) - 1), 'Nested generic outer angle brackets match');
    Editor.SelStart := 17;
    Check(Editor.MatchingBrackets(Opening, Closing) and (Opening = 17) and
      (Closing = Length(Editor.Text) - 2), 'Nested generic inner angle brackets match');
    Editor.Text := 'a < b && c > d'; Editor.SelStart := 2;
    Check(not Editor.MatchingBrackets(Opening, Closing), 'Spaced comparisons are not generic brackets');
    Editor.Text := 'Promise<() => void>'; Editor.SelStart := 7;
    Check(Editor.MatchingBrackets(Opening, Closing) and (Closing = Length(Editor.Text) - 1),
      'Function type arrow does not close generic arguments');
    Editor.Text := '([)]'; Editor.SelStart := 0;
    Check(not Editor.MatchingBrackets(Opening, Closing), 'Mismatched nesting is not highlighted as a valid pair');
    Editor.Text := '// comment'; Editor.SelStart := Length(Editor.Text);
    Editor.Perform(WM_CHAR, Ord('('), 0);
    Check(Editor.Text = '// comment(', 'Delimiters inside line comments are ordinary input');
    Check(Editor.KeywordColors.Count = Length(ERDTypeScriptKeywords), 'Every TypeScript keyword has a colour entry');
    TERDKeywordColor(Editor.KeywordColors.Items[0]).Color := clFuchsia;
    Editor.SelectionPadding := 4;
    Editor.Language := clJSON; Editor.TabWidth := 4;
    Stream.WriteComponent(Editor); Stream.Position := 0;
    Restored := TERDCodeEditor(Stream.ReadComponent(nil));
    try
      Check((Restored.Language = clJSON) and (Restored.TabWidth = 4), 'Code editor settings stream correctly');
      Check((Restored.KeywordColors.Count = Editor.KeywordColors.Count) and
        (TERDKeywordColor(Restored.KeywordColors.Items[0]).Color = clFuchsia), 'Per-keyword colours stream without duplicate entries');
      Check(Restored.SelectionPadding = 4, 'Selection padding streams correctly');
    finally Restored.Free; end;
  finally Stream.Free; Form.Free; end;
end;

procedure TestMultilineTextEngine;
var Form: TForm; Memo, Restored: TERDMemo; Area: TERDTextArea; P, Q: TPoint; Count: Integer;
    Stream: TMemoryStream;
begin
  RegisterClass(TERDMemo);
  Form := TForm.Create(nil); Stream := TMemoryStream.Create;
  try
    Memo := TERDMemo.Create(Form); Memo.Parent := Form;
    Memo.SetBounds(8, 8, 180, 100); Area := Memo.Editor;
    Memo.Text := 'first' + #13#10 + 'second ' + #$D83D#$DC69#$200D#$D83D#$DCBB;
    Form.Show; Memo.SetFocus; Application.ProcessMessages;
    Check(GetWindow(Area.Handle, GW_CHILD) = 0, 'Memo text engine has no Windows EDIT child');
    Check(Memo.Lines.Count = 2, 'Memo Lines reflects multiline model');
    Check(Memo.Lines.Text = Memo.Text, 'Lines.Text does not add an extra newline');
    Area.SelStart := 0; Area.Perform(WM_KEYDOWN, VK_DOWN, 0);
    Check(Area.SelStart = 7, 'Down moves to same column on next row');
    Check(GetCaretPos(P), 'Memo has a native blinking caret');
    Area.Perform(WM_KEYDOWN, VK_UP, 0); Check(GetCaretPos(Q) and (Q.Y < P.Y), 'Up changes visual row');
    Area.SelStart := Length(Memo.Text); Area.Perform(WM_KEYDOWN, VK_BACK, 0); Area.Perform(WM_CHAR, 8, 0);
    Check(Memo.Text = 'first' + #13#10 + 'second ', 'Memo Backspace removes whole ZWJ emoji');
    Area.Undo; Check(Pos(#$200D, Memo.Text) > 0, 'Memo Undo restores emoji');
    Area.Perform(WM_CHAR, 13, 0); Area.Perform(WM_CHAR, Ord('X'), 0);
    Check(Copy(Memo.Text, Length(Memo.Text) - 2, 3) = #13#10 + 'X', 'Memo Enter inserts a real newline');
    Memo.Lines.Add('added'); Check(Pos('added', Memo.Text) > 0, 'Lines.Add updates document');
    Memo.Text := StringOfChar('W', 100);
    Memo.WordWrap := True; Count := Area.Perform(EM_GETLINECOUNT, 0, 0);
    Check(Count > 1, 'Long memo line wraps to multiple visual rows');
    Memo.WordWrap := False;
    Check(Area.Perform(EM_GETLINECOUNT, 0, 0) = 1, 'Disabling wrapping restores one visual row');
    Memo.Text := StringOfChar('A', 20) + #13#10 + StringOfChar('B', 20);
    Memo.WordWrap := True;
    Area.Perform(EM_LINESCROLL, 0, 1);
    Check(Area.FirstVisibleRow >= 0, 'Memo scrolling clamps within visual row range');
    Memo.Text := 'A' + #13#10 + 'B'; Area.SelectAll;
    Area.CopyToClipboard; Check(Clipboard.AsText = Memo.Text, 'Memo clipboard preserves multiline Unicode text');
    Memo.ReadOnly := True; Area.ReplaceSelection('blocked');
    Check(Memo.Text = 'A' + #13#10 + 'B', 'ReadOnly Memo prevents replacement');
    Memo.Text := 'A' + #13#10 + 'B' + #13#10;
    Check((Memo.Lines.Count = 3) and (Memo.Lines[2] = '') and (Memo.Lines.Text = Memo.Text),
      'Memo Lines preserves the final empty logical line');
    Stream.WriteComponent(Memo); Stream.Position := 0;
    Restored := TERDMemo(Stream.ReadComponent(nil));
    try Check(Restored.Text = Memo.Text, 'Memo DFM roundtrip preserves final newline without streaming private editor');
    finally Restored.Free; end;
  finally Stream.Free; Form.Free; end;
end;
procedure TestMigratedShowcaseTextWindows;
var Form: TShowcaseForm;
  procedure Inspect(Control: TWinControl);
  var I: Integer; ClassName: array[0..63] of Char;
  begin
    GetClassName(Control.Handle, ClassName, Length(ClassName));
    Check(not SameText(ClassName, 'EDIT'), 'Showcase contains no native Windows EDIT window');
    for I := 0 to Control.ControlCount - 1 do
      if Control.Controls[I] is TWinControl then Inspect(TWinControl(Control.Controls[I]));
  end;
begin
  Form := TShowcaseForm.Create(nil);
  try Form.Show; Application.ProcessMessages; Inspect(Form);
  finally Form.Free; end;
end;

procedure TestShowcaseUnicodePreviewWindows;
var Form: TShowcaseForm; DC: HDC; R: TRect;
  procedure CheckPreview(Input: TERDTextInput);
  begin
    Check(Input <> nil, 'Unicode preview is created into showcase field');
    Check(Input.Parent = Form.InputsScroll, 'Unicode preview belongs to Inputs scroll box');
    Check(Input.Visible and Input.Enabled, 'Unicode preview is visible and enabled');
    Check(IsWindowVisible(Input.Handle), 'Unicode preview HWND is visible after selecting Inputs');
    GetClientRect(Input.Handle, R);
    Check((R.Width > 0) and (R.Height > 0), 'Unicode preview HWND has nonzero client bounds');
    Input.Repaint;
    DC := GetDC(Input.Handle);
    try
      Check(GetPixel(DC,0,0) <> GetPixel(DC,3,R.Height div 2), 'Unicode preview paints its border on screen');
    finally ReleaseDC(Input.Handle, DC); end;
    Input.SetFocus;
    Check(GetFocus = Input.Handle, 'Unicode preview accepts focus in showcase');
  end;
begin
  Form := TShowcaseForm.Create(nil);
  try
    Form.Show; Form.PageTabs.TabIndex := 1; Form.PageSelected(Form);
    Application.ProcessMessages;
    CheckPreview(Form.UnicodeTextPreview);
    CheckPreview(Form.UnicodePasswordPreview);
  finally Form.Free; end;
end;

procedure TestSelfPaintedTextInput;
var Form: TForm; Input: TERDTextInput; Bitmap: TBitmap; Other: TERDTextInput; ClassName: array[0..63] of Char;
    X, Y, DarkPixels, FocusedClickPosition: Integer; C: TColor;
    BeforeEnd, AfterEnd, RepeatedEnd: TPoint;
begin
  Form := TForm.Create(nil); Bitmap := TBitmap.Create;
  try
    Input := TERDTextInput.Create(Form); Input.Parent := Form;
    Input.SetBounds(8, 8, 300, 28); Input.Text := 'Unicode ' + #$D83D#$DC4D#$D83C#$DFFD;
    Form.Show; Input.SetFocus; Application.ProcessMessages;
    GetClassName(Input.Handle, ClassName, Length(ClassName));
    Check(not SameText(ClassName, 'EDIT'), 'Custom input is not a Windows EDIT');
    Check(GetWindow(Input.Handle, GW_CHILD) = 0, 'Custom input has no native edit child');
    Input.Perform(WM_KEYDOWN, VK_BACK, 0); Input.Perform(WM_CHAR, 8, 0);
    Check(Input.Text = 'Unicode ', 'Custom input backspace removes emoji cluster');
    Input.Perform(WM_CHAR, Ord('A'), 0);
    Check(Input.Text = 'Unicode A', 'Self-painted input accepts typing');
    Input.Undo; Check(Input.Text = 'Unicode ', 'Self-painted input undo works');
    Input.SelectAll; Input.ReplaceSelection('Visible text'); Input.RenderTo(Bitmap);
    DarkPixels := 0;
    for Y := 5 to Bitmap.Height - 6 do for X := 8 to 100 do
    begin C := ColorToRGB(Bitmap.Canvas.Pixels[X,Y]);
      if (GetRValue(C) < 120) and (GetGValue(C) < 120) and (GetBValue(C) < 120) then Inc(DarkPixels);
    end;
    Check(DarkPixels > 15, 'Self-painted input renders visible glyphs');
    Input.PasswordChar := #$2022; Input.Text := 'secret'; Input.SelectAll;
    Clipboard.AsText := 'clipboard sentinel';
    Input.CopyToClipboard;
    Check(Clipboard.AsText = 'clipboard sentinel', 'Password contents cannot be copied');
    Input.PasswordVisible := True;
    Check(Input.Text = 'secret', 'Reveal preserves underlying password text');
    Input.ReadOnly := True; Input.ReplaceSelection('changed');
    Check(Input.Text = 'secret', 'ReadOnly blocks replacement');
    Input.ReadOnly := False; Input.PasswordChar := #0; Input.PasswordVisible := False;
    Input.Text := 'ABC' + #$D83D#$DD12;
    Input.SelStart := 3; Input.SelLength := 0;
    Check(GetCaretPos(BeforeEnd), 'Caret available before final emoji');
    Input.Perform(WM_KEYDOWN, VK_RIGHT, 0);
    Check(Input.SelStart = Length(Input.Text), 'Right arrow moves past whole final emoji');
    Check(GetCaretPos(AfterEnd) and (AfterEnd.X > BeforeEnd.X), 'End caret lies after final emoji');
    Input.Perform(WM_KEYDOWN, VK_RIGHT, 0);
    Check(GetCaretPos(RepeatedEnd) and (RepeatedEnd.X = AfterEnd.X), 'Right at end preserves caret position');
    Input.Perform(WM_KEYDOWN, VK_LEFT, 0);
    Check(Input.SelStart = 3, 'Left from end moves before whole final emoji');
    Other := TERDTextInput.Create(Form); Other.Parent := Form; Other.Top := 50;
    Input.Text := 'ABCDEFGHIJ';
    Input.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LPARAM((14 shl 16) or 34));
    Input.Perform(WM_LBUTTONUP, 0, LPARAM((14 shl 16) or 34));
    FocusedClickPosition := Input.SelStart;
    Check(FocusedClickPosition > 0, 'Focused click places caret inside text');
    Other.SetFocus; Input.SelStart := Length(Input.Text);
    Input.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LPARAM((14 shl 16) or 34));
    Input.Perform(WM_LBUTTONUP, 0, LPARAM((14 shl 16) or 34));
    Check(Input.Focused and (Input.SelStart = FocusedClickPosition), 'First unfocused click places caret at clicked position');

  finally Bitmap.Free; Form.Free; end;
end;

procedure TestVerticalCaretNavigation;
var Form: TForm; Area: TERDTextArea; Source: UnicodeString; Mode, I, Before: Integer;
    Keyboard, SavedKeyboard: TKeyboardState;
begin
  for Mode := 0 to 1 do
  begin
    Form := TForm.Create(nil);
    try
      if Mode = 0 then Area := TERDTextArea.Create(Form) else Area := TERDCodeEditor.Create(Form);
      Area.Parent := Form; Area.SetBounds(0, 0, 500, 220); Source := '';
      for I := 0 to 99 do Source := Source + 'abcd' + #13#10;
      Area.Text := Source; Form.Show; Area.SetFocus; Area.SelStart := 20 * 6 + 2;
      Area.Perform(EM_LINESCROLL, 0, 20 - Area.FirstVisibleRow);
      Area.Perform(WM_KEYDOWN, VK_UP, 0);
      Check((Area.SelStart = 19 * 6 + 2) and (Area.FirstVisibleRow = 19),
        'Up moves to the offscreen previous row and reveals it');
      GetKeyboardState(SavedKeyboard); Keyboard := SavedKeyboard; Keyboard[VK_SHIFT] := $80;
      try
        SetKeyboardState(Keyboard); Area.Perform(WM_KEYDOWN, VK_UP, 0);
        Check((Area.SelStart = 18 * 6 + 2) and (Area.SelLength = 6) and (Area.FirstVisibleRow = 18),
          'Shift+Up extends selection and scrolls through the upper edge');
      finally SetKeyboardState(SavedKeyboard); end;
      Before := Area.SelStart; Area.Perform(WM_KEYDOWN, VK_PRIOR, 0);
      Check((Area.SelStart < Before) and (Area.FirstVisibleRow < 18),
        'Page Up reaches rows above the viewport');
      Area.SelStart := 20 * 6 + 2;
      Area.Perform(EM_LINESCROLL, 0, 20 - Area.VisibleRowCount + 1 - Area.FirstVisibleRow);
      Before := Area.FirstVisibleRow; Area.Perform(WM_KEYDOWN, VK_DOWN, 0);
      Check((Area.SelStart = 21 * 6 + 2) and (Area.FirstVisibleRow = Before + 1),
        'Down still reveals the next row through the lower edge');
    finally Form.Free; end;
  end;
end;

procedure TestCodeEditorNavigation;
var Form: TForm; Editor: TCodeEditorProbe; Bitmap: TBitmap; Vertical, Horizontal, Map, Thumb: TRect;
    I, Page, RowBefore, WidthWithMap: Integer; Source: UnicodeString; Stream: TMemoryStream;
    Restored: TCodeEditorProbe;
  procedure Mouse(MessageID: Cardinal; X, Y: Integer);
  begin Editor.Perform(MessageID, MK_LBUTTON, MakeLParam(X, Y)); end;
begin
  Form := TForm.Create(nil); Bitmap := TBitmap.Create; Stream := TMemoryStream.Create;
  try
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 500, 220);
    Source := '';
    for I := 0 to 39 do Source := Source + '// before' + #13#10;
    RowBefore := Length(Source);
    Source := Source + 'function foldMe() {' + #13#10 + '  return 1;' + #13#10 + '}' + #13#10;
    for I := 0 to 79 do Source := Source + '// after' + #13#10;
    Editor.Text := Source; Editor.SelStart := 0; Form.Show; Editor.SetFocus;
    Editor.Perform(EM_LINESCROLL, 0, 35);
    Check(Editor.FirstVisibleRow = 35, 'Fold fixture scrolls away from caret');
    Editor.ToggleFold(RowBefore);
    Check(Editor.FirstVisibleRow = 35, 'Collapsing keeps viewport when caret is above it');
    Editor.ToggleFold(RowBefore);
    Check(Editor.FirstVisibleRow = 35, 'Expanding keeps viewport when caret is above it');
    Source := '';
    for I := 0 to 119 do Source := Source + 'const item' + IntToStr(I) + ' = "' + StringOfChar('x', 180) + '";' + #13#10;
    Editor.Text := Source; Editor.SelStart := 0; Form.Show; Editor.SetFocus; Editor.RenderTo(Bitmap);
    Editor.GetNavigationRects(Vertical, Horizontal, Map);
    Check(not IsRectEmpty(Vertical) and not IsRectEmpty(Horizontal) and not IsRectEmpty(Map),
      'Editor exposes vertical and horizontal bars and a minimap by default');
    Check(Map.Top = Vertical.Top, 'Minimap background reaches the inner border alongside its scrollbar');
    Check(ColorToRGB(Bitmap.Canvas.Pixels[Vertical.Left + 2, Vertical.Bottom - 3]) =
      Editor.MinimapBackground, 'Vertical scrollbar track matches the visible minimap background');
    Thumb := ERDScrollThumbRect(Rect(0, 0, 12, 200), True, 0, 99, 50, 0, False, False);
    Check(Thumb.Height = 100, 'Scrollbar thumb represents the visible fraction of the content');
    Thumb := ERDScrollThumbRect(Rect(0, 0, 12, 200), True, 0, 9, 20, 0, False, False);
    Check(Thumb.Height = 200, 'Scrollbar thumb fills the track when content fits');
    WidthWithMap := Editor.ViewWidth; Editor.ShowMinimap := False;
    Check(Editor.ViewWidth > WidthWithMap, 'Hiding minimap restores editable viewport width');
    Editor.ShowMinimap := True; Page := Editor.VisibleRowCount;
    Thumb := ERDScrollThumbRect(Vertical, True, 0, Editor.RowCount - 1, Page, 0, True, True);
    Mouse(WM_LBUTTONDOWN, (Thumb.Left + Thumb.Right) div 2, (Thumb.Top + Thumb.Bottom) div 2);
    Mouse(WM_MOUSEMOVE, (Thumb.Left + Thumb.Right) div 2, Vertical.Bottom - 2);
    Mouse(WM_LBUTTONUP, (Thumb.Left + Thumb.Right) div 2, Vertical.Bottom - 2);
    Check((Editor.FirstVisibleRow > 80) and (Editor.SelStart = 0), 'Vertical thumb drags viewport without moving selection');
    Mouse(WM_LBUTTONDOWN, (Map.Left + Map.Right) div 2, Map.Top + Map.Height div 2);
    Mouse(WM_LBUTTONUP, (Map.Left + Map.Right) div 2, Map.Top + Map.Height div 2);
    Check((Editor.FirstVisibleRow > 30) and (Editor.FirstVisibleRow < 80), 'Minimap click navigates to document middle');
    RowBefore := Editor.FirstVisibleRow;
    Editor.Perform(WM_MOUSEWHEEL, MakeWParam(0, WHEEL_DELTA), 0);
    Check(Editor.FirstVisibleRow < RowBefore, 'Wheel and minimap use the same scroll position');
    Mouse(WM_LBUTTONDOWN, Horizontal.Right - 2, (Horizontal.Top + Horizontal.Bottom) div 2);
    Mouse(WM_LBUTTONUP, Horizontal.Right - 2, (Horizontal.Top + Horizontal.Bottom) div 2);
    Check(Editor.HorizontalOffset > 0, 'Horizontal scrollbar pages across long lines');
    Editor.Text := 'short'; Editor.GetNavigationRects(Vertical, Horizontal, Map);
    Check(IsRectEmpty(Horizontal), 'Horizontal scrollbar hides when the text fits');
    Check(Editor.HorizontalOffset = 0, 'Fitting text has no leftover horizontal offset');
    Editor.Text := Source;

    Editor.WordWrap := True; Editor.GetNavigationRects(Vertical, Horizontal, Map);
    Check(IsRectEmpty(Horizontal), 'Word wrapping removes horizontal scrollbar');
    Editor.WordWrap := False; Editor.FoldAll; Editor.UnfoldAll; Editor.RenderTo(Bitmap);
    Check(Editor.Text = Source, 'Scrolling and minimap leave document text intact');
    Editor.MinimapWidth := 128; Editor.ScrollBars := ssVertical;
    Stream.WriteComponent(Editor); Stream.Position := 0;
    Restored := Stream.ReadComponent(nil) as TCodeEditorProbe;
    try Check(Restored.ShowMinimap and (Restored.MinimapWidth = 128) and (Restored.ScrollBars = ssVertical),
      'Editor navigation settings stream correctly'); finally Restored.Free; end;
  finally Stream.Free; Bitmap.Free; Form.Free; end;
end;

procedure TestTitleBarMaximizeRouting;
var Form: TForm; Title: TERDTitleBar; Local, ScreenPoint, SavedCursor, Outside: TPoint; Found: Boolean;
    Bitmap: TBitmap; IdleInk, HoverInk: TColor; PixelX: Integer;
  procedure FindMaximize;
  var X: Integer;
  begin
    Found := False; Local.Y := MulDiv(16, Title.CurrentPPI, 96);
    for X := Title.Width - 1 downto 0 do
    begin
      ScreenPoint := Title.ClientToScreen(Point(X, Local.Y));
      if Title.Perform(WM_NCHITTEST, 0, MakeLParam(ScreenPoint.X, ScreenPoint.Y)) = HTMAXBUTTON then
      begin Local.X := X; Found := True; Break; end;
    end;
    Check(Found, 'Custom titlebar exposes its maximize hit target');
  end;
  procedure PressMaximize;
  begin
    FindMaximize;
    Title.Perform(WM_NCLBUTTONDOWN, HTMAXBUTTON, MakeLParam(ScreenPoint.X, ScreenPoint.Y));
  end;
begin
  Form := TForm.CreateNew(nil); Bitmap := TBitmap.Create; GetCursorPos(SavedCursor);
  try
    Form.SetBounds(100, 100, 700, 400); Form.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
    Title := TERDTitleBar.Create(Form); Title.Parent := Form; Form.Show; Application.ProcessMessages;
    FindMaximize; PixelX := Local.X - MulDiv(3, Title.CurrentPPI, 96);
    Outside := Form.ClientToScreen(Point(Form.ClientWidth div 2, Form.ClientHeight div 2));
    SetCursorPos(Outside.X, Outside.Y); Title.Perform(CM_MOUSELEAVE, 0, 0);
    Title.RenderTo(Bitmap); IdleInk := Bitmap.Canvas.Pixels[PixelX, Local.Y];
    SetCursorPos(ScreenPoint.X, ScreenPoint.Y);
    Title.Perform(WM_NCMOUSEMOVE, HTMAXBUTTON, MakeLParam(ScreenPoint.X, ScreenPoint.Y));
    Title.RenderTo(Bitmap); HoverInk := Bitmap.Canvas.Pixels[PixelX, Local.Y];
    Check(HoverInk <> IdleInk, 'Maximize button has a visible non-client hover fill');
    Title.Perform(CM_MOUSELEAVE, 0, 0); Form.Perform(WM_NCMOUSELEAVE, 0, 0);
    Title.RenderTo(Bitmap);
    Check(Bitmap.Canvas.Pixels[PixelX, Local.Y] = HoverInk, 'Hover survives client/non-client transitions while pointer stays on maximize');
    SetCursorPos(Outside.X, Outside.Y); Title.Perform(CM_MOUSELEAVE, 0, 0);
    Title.RenderTo(Bitmap);
    Check(Bitmap.Canvas.Pixels[PixelX, Local.Y] = IdleInk, 'Maximize hover clears when pointer leaves the button');
    PressMaximize;
    Check(not IsZoomed(Form.Handle), 'Custom maximize waits for release without native tracking');
    Title.Perform(WM_LBUTTONUP, 0, MakeLParam(Local.X, Local.Y));
    Check(IsZoomed(Form.Handle), 'Custom maximize executes on client release');
    PressMaximize; Title.Perform(WM_LBUTTONUP, 0, MakeLParam(Local.X, Local.Y));
    Check(not IsZoomed(Form.Handle), 'Custom restore executes on client release');
    PressMaximize; Title.Perform(WM_LBUTTONUP, 0, MakeLParam(Title.Width div 2, Local.Y));
    Check(not IsZoomed(Form.Handle), 'Releasing outside the maximize button cancels');
    PressMaximize; SetCapture(Form.Handle); ReleaseCapture;
    Title.Perform(WM_LBUTTONUP, 0, MakeLParam(Local.X, Local.Y));
    Check(not IsZoomed(Form.Handle), 'Capture loss cancels pending maximize');
    PressMaximize;
    Form.Perform(WM_NCLBUTTONUP, HTMAXBUTTON, MakeLParam(ScreenPoint.X, ScreenPoint.Y));
    Check(IsZoomed(Form.Handle), 'Non-client release executes maximize once');
    Form.WindowState := wsNormal;
  finally Bitmap.Free; Form.Free; SetCursorPos(SavedCursor.X, SavedCursor.Y); end;
end;

procedure TestFixedPasswordMaskGeometry;
var Form: TForm; Input: TERDTextInput; First, Last: TPoint; Small, Large, Revealed: Integer;
  function CaretWidth: Integer;
  begin
    Input.SelStart := 0; GetCaretPos(First);
    Input.SelStart := Length(Input.Text); GetCaretPos(Last); Result := Last.X - First.X;
  end;
begin
  Form := TForm.Create(nil);
  try
    Input := TERDTextInput.Create(Form); Input.Parent := Form; Input.SetBounds(0, 0, 440, 80);
    Input.Text := 'WWWW'; Input.PasswordChar := #$2022; Form.Show; Input.SetFocus;
    Input.Font.Height := -12; Small := CaretWidth;
    Input.Font.Height := -24; Large := CaretWidth;
    Check((Small = Large) and (Small = 4 * MulDiv(8, Input.CurrentPPI, 96)),
      'Masked password cells keep fixed width across font sizes');
    Input.PasswordVisible := True; Revealed := CaretWidth;
    Check(Revealed > Large, 'Revealed password uses the actual font character widths');
  finally Form.Free; end;
end;

procedure TestCodeEditorDetachAndDestroy;
var Form: TForm; Editor: TERDCodeEditor; Bitmap: TBitmap;
begin
  Form := TForm.Create(nil); Bitmap := TBitmap.Create;
  try
    Editor := TERDCodeEditor.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 400, 180);
    Editor.Text := StringOfChar('x', 200); Form.Show; Editor.SetFocus; Editor.RenderTo(Bitmap);
    Editor.Parent := nil;
    Editor.Perform(CM_FONTCHANGED, 0, 0);
    Editor.Free;
    Check(True, 'Detached editor tears down without recreating an orphan window');
  finally Bitmap.Free; Form.Free; end;
end;

procedure TestCodeGutterThemeContrast;
var Form: TForm; Theme: TERDTheme; Editor: TCodeEditorProbe; C: COLORREF;
    Source: UnicodeString; I: Integer;
  function Brightness(Color: COLORREF): Integer;
  begin Result := (GetRValue(Color) + GetGValue(Color) + GetBValue(Color)) div 3; end;
begin
  Form := TForm.Create(nil);
  try
    Theme := TERDTheme.Create(Form);
    Editor := TCodeEditorProbe.Create(Form); Editor.Parent := Form; Editor.Theme := Theme;
    Editor.SetBounds(0, 0, 360, 180);
    Editor.Text := 'function run() {' + #13#10 + '  return true;' + #13#10 + '}';
    Editor.ShowFoldGuides := False; Form.Show; Editor.FoldMouse(0, True);
    Theme.Mode := tmERDesignsLight;
    C := Editor.FoldPixel(True);
    Check(Brightness(C) > 180, 'Light-theme gutter stays light');
    Check(Brightness(Editor.FoldPixel(False)) > 180, 'Idle light fold button is neutral');
    Theme.Mode := tmERDesignsDark;
    C := Editor.FoldPixel(True);
    Check(Brightness(C) < 100, 'Dark-theme gutter stays dark');
    Check(Brightness(C) > Brightness(Editor.EditorBackground) + 12,
      'Dark gutter remains distinct from the actual editor text surface');
    Check((Editor.MinimapBackground <> Editor.EditorBackground) and
      (Editor.MinimapBackground <> Editor.GutterBackground),
      'Dark minimap has its own visible surface tone');
    Check(Brightness(Editor.FoldPixel(False)) < 100, 'Idle dark fold button is neutral');
    Source := 'function continued() {' + #13#10;
    for I := 0 to 39 do Source := Source + '    // continuation' + #13#10;
    Editor.Text := Source + '}'; Editor.ShowFoldGuides := True;
    Editor.SelStart := 0; Editor.Perform(EM_LINESCROLL, 0, 1);
    Check(Editor.FirstVisibleRow = 1, 'Guide fixture scrolls within an expanded fold');
    Check(Editor.TopFoldGuidePixel <> Editor.GutterBackground,
      'Continuing dark fold reaches the inner top edge above the first row');
    Check((Editor.IndentGuidePixel(0) = Editor.IndentGuidePixel(1)) and
      (Editor.IndentGuidePixel(0) <> Editor.EditorBackground),
      'First visible indentation segment matches the next row in dark mode');
    Theme.Mode := tmERDesignsLight;
    Check(Editor.TopFoldGuidePixel <> Editor.GutterBackground,
      'Continuing light fold reaches the inner top edge above the first row');
    Check((Editor.IndentGuidePixel(0) = Editor.IndentGuidePixel(1)) and
      (Editor.IndentGuidePixel(0) <> Editor.EditorBackground),
      'First visible indentation segment matches the next row in light mode');
    Source := '  function block() {' + #13#10 + '    first();' + #13#10 +
      #13#10 + '  ' + #13#10 + #9 + #13#10 + '    last();' + #13#10 + #13#10 + '  }' + #13#10 +
      #13#10 + 'topLevel();';
    for I := 0 to 19 do Source := Source + #13#10 + 'topLevel();';
    Editor.Text := Source;
    Editor.Perform(EM_LINESCROLL, 0, 2 - Editor.FirstVisibleRow);
    C := Editor.IndentGuidePixel(3);
    Check((Editor.IndentGuidePixel(0) = C) and (Editor.IndentGuidePixel(1) = C) and
      (Editor.IndentGuidePixel(2) = C) and (C <> Editor.EditorBackground),
      'Empty, spaces-only and tabs-only rows continue surrounding indentation');
    Check(Editor.IndentGuidePixel(4) = C, 'Blank row before a closing brace keeps its block guide');
    Check(Editor.IndentGuidePixel(6) = Editor.EditorBackground,
      'Blank row before top-level code does not extend a finished block');
    Theme.Mode := tmERDesignsDark;
    Check(Editor.IndentGuidePixel(0) = Editor.IndentGuidePixel(3), 'Dark theme also continues blank-row guides');
    Editor.ShowIndentGuides := False;
    Check(Editor.IndentGuidePixel(0) = Editor.EditorBackground, 'Disabling guides also hides blank-row segments');


  finally Form.Free; end;
end;

procedure RunLSPTransportFixture;
var Header, Line, Method, Uri: string; Size, I, Version: Integer; Read, Written: DWORD;
    ByteValue: Byte; Bytes: TBytes; JSON, Params, Document, Position, Range: TJSONObject;
    ResultValue: TJSONValue; MessageValue: TJSONObject; Items: TJSONArray;
  procedure Send(Value: TJSONObject);
  var Payload, Frame: TBytes; Prefix: AnsiString;
  begin
    try Payload := TEncoding.UTF8.GetBytes(Value.ToJSON); finally Value.Free; end;
    Prefix := AnsiString('Content-Length: ' + IntToStr(Length(Payload)) + #13#10#13#10);
    Frame := TEncoding.ASCII.GetBytes(string(Prefix)); I := Length(Frame);
    SetLength(Frame, I + Length(Payload)); if Length(Payload) > 0 then Move(Payload[0], Frame[I], Length(Payload));
    if not WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), Frame[0], Length(Frame), Written, nil) then RaiseLastOSError;
  end;
begin
  // This child mode is a protocol fixture, not a TypeScript semantic service.
  while True do
  begin
    Header := ''; Size := -1;
    while not Header.EndsWith(#13#10#13#10) do
    begin
      if not ReadFile(GetStdHandle(STD_INPUT_HANDLE), ByteValue, 1, Read, nil) or (Read = 0) then Exit;
      Header := Header + Char(ByteValue); if Length(Header) > 8192 then Halt(2);
    end;
    for Line in Header.Split([#13, #10]) do
      if SameText(Copy(Line, 1, 15), 'Content-Length:') then Size := StrToInt(Trim(Copy(Line, 16, MaxInt)));
    if (Size <= 0) or (Size > 16 * 1024 * 1024) then Halt(2);
    SetLength(Bytes, Size); I := 0;
    while I < Size do
    begin
      if not ReadFile(GetStdHandle(STD_INPUT_HANDLE), Bytes[I], Size - I, Read, nil) or (Read = 0) then Exit;
      Inc(I, Read);
    end;
    JSON := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetString(Bytes)) as TJSONObject;
    try
      Method := JSON.GetValue<string>('method'); Params := JSON.GetValue('params') as TJSONObject;
      if Method = 'exit' then Exit;
      if (Method = 'textDocument/didOpen') or (Method = 'textDocument/didChange') then
      begin
        Document := Params.GetValue('textDocument') as TJSONObject;
        Uri := Document.GetValue<string>('uri'); Version := Document.GetValue<Integer>('version');
        Document := TJSONObject.Create.AddPair('uri', Uri).AddPair('version', TJSONNumber.Create(Version));
        Document.AddPair('diagnostics', TJSONArray.Create);
        Send(TJSONObject.Create.AddPair('jsonrpc', '2.0').AddPair('method', 'textDocument/publishDiagnostics').AddPair('params', Document));
        Continue;
      end;
      if JSON.GetValue('id') = nil then Continue;
      ResultValue := TJSONNull.Create;
      if Method = 'initialize' then
      begin ResultValue.Free; ResultValue := TJSONObject.Create.AddPair('capabilities', TJSONObject.Create); end
      else if Method = 'textDocument/completion' then
      begin
        ResultValue.Free; Position := Params.GetValue('position') as TJSONObject;
        Range := TJSONObject.Create.AddPair('start', Position.Clone as TJSONValue).AddPair('end', Position.Clone as TJSONValue);
        Document := TJSONObject.Create.AddPair('label', 'fixtureMember').AddPair('detail', 'transport fixture');
        Document.AddPair('kind', TJSONNumber.Create(2));
        Document.AddPair('documentation', TJSONObject.Create.AddPair('kind', 'markdown').AddPair('value', 'Fixture method documentation'));
        Document.AddPair('textEdit', TJSONObject.Create.AddPair('range', Range).AddPair('newText', 'fixtureMember'));
        Items := TJSONArray.Create; Items.AddElement(Document); ResultValue := Items;
      end;
      if Method = 'textDocument/signatureHelp' then
      begin
        ResultValue.Free;
        ResultValue := TJSONObject.ParseJSONValue('{"activeSignature":0,"activeParameter":0,"signatures":[{"label":"fixture(value: string): void","parameters":[{"label":[8,21]}]}]}');
      end;
      MessageValue := TJSONObject.Create.AddPair('jsonrpc', '2.0').AddPair('id', JSON.GetValue('id').Clone as TJSONValue);
      MessageValue.AddPair('result', ResultValue); Send(MessageValue);
    finally JSON.Free; end;
  end;
end;

procedure TestStdioLanguageAdapter;
var Form: TForm; Editor: TERDCodeEditor; Service: IERDCodeLanguageService; Deadline: UInt64;
begin
  Form := TForm.Create(nil);
  try
    Editor := TERDCodeEditor.Create(Form); Editor.Parent := Form; Editor.SetBounds(0, 0, 440, 180);
    Editor.Text := 'const icon = "' + #$D83D#$DE00 + '"; icon.';
    Editor.SelStart := Length(Editor.Text); Form.Show; Editor.SetFocus;
    Service := TERDStdioLanguageService.Create(ParamStr(0));
    Editor.LanguageService := Service; Service := nil;
    Check(Editor.RequestCompletion, 'Stdio adapter queues a completion without blocking the UI');
    Deadline := GetTickCount64 + 10000;
    while (Length(Editor.Completions) = 0) and (GetTickCount64 < Deadline) do
    begin Application.ProcessMessages; CheckSynchronize(0); Sleep(1); end;
    Check((Length(Editor.Completions) = 1) and (Editor.Completions[0].LabelText = 'fixtureMember'),
      'Stdio adapter delivers parsed completion on the UI thread');
    Check((Editor.Completions[0].Kind = 2) and
      (Editor.Completions[0].Documentation = 'Fixture method documentation'),
      'Stdio adapter preserves completion kind and markup documentation text');
    Check(Editor.ApplyCompletion(0), 'Stdio completion replacement range is accepted');
    Check(Pos('fixtureMember', Editor.Text) > 0, 'UTF-8 frames preserve UTF-16 emoji replacement positions');
    Check(Editor.RequestSignatureHelp, 'Stdio adapter queues signature help');
    Deadline := GetTickCount64 + 10000;
    while (Length(Editor.SignatureHelp.Signatures) = 0) and (GetTickCount64 < Deadline) do
    begin Application.ProcessMessages; CheckSynchronize(0); Sleep(1); end;
    Check((Length(Editor.SignatureHelp.Signatures) = 1) and
      (Editor.SignatureHelp.Signatures[0].LabelText = 'fixture(value: string): void'),
      'Stdio adapter dispatches and decodes the signatureHelp method');
    Editor.LanguageService := nil; CheckSynchronize(0);
    Check(Editor.Text <> '', 'Stopping stdio adapter preserves the editor document');
  finally Form.Free; end;
end;

begin
  try
    if (ParamCount > 0) and (ParamStr(1) = 'lsp') then begin RunLSPTransportFixture; Halt(0); end;
    Application.Initialize;
    TestIconLifetimeAndStreaming;
    TestHostedStatusPanels;
    TestRibbonHostedControls;
    TestThemesAndRibbon;
    TestPopupRowsAndCommandSwitch;
    TestComboMouseChange;
    TestDemoLayoutAndSpinMargins;
    TestWindowsFocusCuesAndAutoSave;
    TestVirtualTreeThemeAndLazyNodes; TestVirtualTreeDensity; TestShowcaseDockingNavigation;
    TestCommonControls;
    TestFeedbackRegressions;
    TestSplitMenuGeometry;
    TestCheckListBox;
    TestTimePicker;
    TestDateSegmentsAndDateTime;
    TestLinkLabel;
    TestLoadingIndicator;
    TestDropZone;
    TestCommandPalette;
    TestCommandPalettePopup;
    TestRating;
    TestEditableCombo;
    TestEditPasswordAndValidation;
    TestDocumentTabs;
    TestTextPainting;
    TestFloatingPainter;
    TestUnicodeTextModel;
    TestSelfPaintedTextInput;
    TestMultilineTextEngine;
    TestSyntaxAndCodeEditor;
    TestCodeEditorNavigation;
    TestVerticalCaretNavigation;
    TestTitleBarMaximizeRouting;
    TestFixedPasswordMaskGeometry;
    TestCodeEditorDetachAndDestroy;
    TestCodeGutterThemeContrast;
    TestSharedColorEmoji;
    TestCodeEditorSearch;
    TestCodeLanguageServices;
    TestPCBScene;
    TestPCBComponentPalette;
    TestPCBObjectTree;
    TestPCBLayout;
    TestPCBBoardRouting;
    TestPCBLayoutToolbar;
    TestPCBMagneticDrag;
    TestPCBSelectionHandles;
    TestPCBValidation;
    TestPCBValidationPanel;
    TestPCBThemeContrast;
    TestPCBHistoryValidation;
    TestPCBDesigner;
    TestPCBDesignerTools;
    TestPCBPlacement;
    TestDebugEditor; TestBreakpointPanel; TestCallStack; TestLocalsPanel; TestWatchPanel; TestDisassemblyView; TestFiberList; TestRuntimeMonitor; TestShapeInspector; TestDebugConsole; TestValueTreeSnapshots; TestTimeSeriesHistory; TestTimeSeriesBars; TestRuntimeBarTimelines; TestDockHost; TestDockMouseDrop; TestDocumentHost; TestProjectTree; TestDiagnosticList; TestAutomaticOfficeHints; TestOfficeHints;
    TestPCBCardinalAnchors; TestPCBZOrderAndGrid; TestSelectedTabVisibility;
    TestPCBConnections;
    TestPCBComponentTypes;
    TestPCBPropertySchema;
    TestPCBGroupInspector;
    TestSignatureHelp;
    TestEditorWindowShutdown;
    TestStdioLanguageAdapter;
    TestWindowsEditInteractionParity;
    TestMigratedShowcaseTextWindows;
    TestTextInputEditMenu;
    TestShowcaseUnicodePreviewWindows;
    Writeln(Checks, ' VCL runtime checks passed.');
  except on E: Exception do begin Writeln(E.ClassName, ': ', E.Message); Halt(1) end end;
end.
