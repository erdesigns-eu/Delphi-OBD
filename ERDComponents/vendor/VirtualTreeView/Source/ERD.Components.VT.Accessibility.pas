unit ERD.Components.VT.Accessibility;

// ERD adaptation: namespaced VCL engine; original attribution is retained below.
// Distributed under the selected Mozilla Public License 1.1.
// Full terms and upstream provenance: ../LICENSE.MPL-1.1.txt and ../upstream.json.


// This unit implements iAccessible interfaces for the VirtualTree visual components
// and the currently focused node.
//
// Written by Marco Zehe. (c) 2007

interface

uses
  Winapi.Windows, Winapi.ActiveX, Winapi.oleacc,
  System.Classes, System.Types,
  Vcl.Controls,
  ERD.Components.VT, ERD.Components.VT.AccessibilityFactory, ERD.Components.VT.BaseTree;

type
  TERDVTVirtualTreeAccessibility = class(TInterfacedObject, IDispatch, IAccessible)
  private
    FVirtualTree: TERDVTVirtualStringTree;
  public
    constructor Create(AVirtualTree: TERDVTVirtualStringTree);
    /// Register the default accessible provider of Virtual TreeView
    class procedure RegisterDefaultAccessibleProviders();

    { IAccessibility }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
                              out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer;
                         out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
    {IDispatch}
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount: Integer; LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer;
      out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult: Pointer; ExcepInfo: Pointer;
      ArgErr: Pointer): HRESULT; stdcall;
  end;

  TERDVTVirtualTreeItemAccessibility = class(TERDVTVirtualTreeAccessibility, IAccessible)
    public
    { IAccessibility }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer;
      out pyTop: Integer; out pcxWidth: Integer;
      out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HRESULT; stdcall;
  end;

  TERDVTVTMultiColumnItemAccessibility = class(TERDVTVirtualTreeItemAccessibility, IAccessible)
  strict private
    function GetItemDescription(varChild: OleVariant; out pszDescription: WideString; IncludeMainColumn: boolean): HResult; stdcall;
  public
    { IAccessibility }
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
  end;

  TERDVTVTDefaultAccessibleProvider = class(TInterfacedObject, IVTAccessibleProvider)
  public
    function CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
  end;

  TERDVTVTDefaultAccessibleItemProvider = class(TInterfacedObject, IVTAccessibleProvider)
  public
    function CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
  end;

  TERDVTVTMultiColumnAccessibleItemProvider = class(TInterfacedObject, IVTAccessibleProvider)
  public
    function CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
  end;

implementation

uses
  System.SysUtils, System.Variants, System.Math,
  Vcl.Forms,
  ERD.Components.VT.Types;

type

/// For getting access to protected members of this class
TERDVTHackVirtualStringTree = class(TERDVTVirtualStringTree)
end;

{ TVirtualTreeAccessibility }
//----------------------------------------------------------------------------------------------------------------------
constructor TERDVTVirtualTreeAccessibility.Create(AVirtualTree: TERDVTVirtualStringTree);
// assigns the parent and current fields, and lets the control's IAccessible object know its address.
begin
  inherited Create;
  FVirtualTree := AVirtualTree;
end;

//----------------------------------------------------------------------------------------------------------------------

function TERDVTVirtualTreeAccessibility.accDoDefaultAction(varChild: OleVariant): HResult;
// a default action is not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;
//----------------------------------------------------------------------------------------------------------------------

function TERDVTVirtualTreeAccessibility.accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult;
// returns the iAccessible object at the given point, if applicable.
var
  Pt: TPoint;
  HitInfo: THitInfo;
begin
  Result := S_FALSE;
  if FVirtualTree <> nil then
  begin
//    VariantInit(pvarChild);
//    TVarData(pvarChild).VType := VT_I4;
    Pt := fVirtualTree.ScreenToClient(Point(xLeft, yTop));
    if fVirtualTree.FocusedNode <> nil then
    begin
      fVirtualTree.GetHitTestInfoAt(xLeft, yTop, false, HitInfo);
      if FVirtualTree.FocusedNode = HitInfo.HitNode then
      begin
        pvarChild := FVirtualTree.AccessibleItem;
        Result := S_OK;
        exit;
      end;
    end;
    if PtInRect(FVirtualTree.BoundsRect, Pt) then
    begin
      pvarChild := CHILDID_SELF;
      Result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.accLocation(out pxLeft: Integer;
  out pyTop: Integer; out pcxWidth: Integer;
  out pcyHeight: Integer; varChild: OleVariant): HResult;
// returns the location of the VirtualStringTree object.
var
  P: TPoint;
begin
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      P := FVirtualTree.ClientToScreen(FVirtualTree.ClientRect.TopLeft);
      pxLeft := P.X;
      pyTop := P.Y;
      pcxWidth := FVirtualTree.Width;
      pcyHeight := FVirtualTree.Height;
      Result := S_OK;
    end;
  end
  else if VarType(varchild) = VT_I4 then
  begin
    // return the location of the focused node
    if (FVirtualTree <> nil) and (FVirtualTree.AccessibleItem <> nil) then
    begin
      Result := FVirtualTree.AccessibleItem.accLocation(pxLeft, pyTop, pcxWidth, pcyHeight, CHILDID_SELF);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.accNavigate(navDir: Integer; varStart: OleVariant;
  out pvarEndUpAt: OleVariant): HResult;
// This is not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accSelection(out pvarChildren: OleVariant): HResult;
// returns the selected child ID, if any.
begin
  Result := s_false;
  if FVirtualTree <> nil then
    if fVirtualTree.FocusedNode <> nil then
    begin
      pvarChildren := 1;
      result := s_OK;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT;
// Not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.GetTypeInfoCount(
  out Count: Integer): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
// returns the iAccessible child, whicfh represents the focused item.
begin
  if varChild = CHILDID_SELF then
  begin
    ppdispChild := FVirtualTree.AccessibleItem;
    Result := S_OK;
  end
  else
    Result := E_INVALIDARG
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
// Returns the number 1 for the one child: The focused item.
begin
  pcountChildren := 1;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
// Not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
// returns the hint of the control, if assigned.
begin
  pszDescription := '';
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
      pszDescription := GetLongHint(fVirtualTree.Hint);
  end;
  if Length(pszDescription) > 0 then
    Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
// returns the child ID of 1, if assigned.
begin
  Result := s_false;
  if fVirtualTree <> nil then
  begin
    if FVirtualTree.FocusedNode <> nil then
      pvarChild := FVirtualTree.AccessibleItem
    else
      pvarChild := childid_self;
      result := S_OK;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
// Not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
                          out pidTopic: Integer): HResult;
// Returns the HelpContext ID, if present.
begin
  pszHelpFile := '';
  pidTopic := 0;
  Result := S_OK;
  if varChild = CHILDID_SELF then
    if FVirtualTree <> nil then
    begin
      pszHelpFile := Application.HelpFile;
      pidTopic := FVirtualTree.HelpContext;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
// Not supported.
begin
  pszKeyboardShortcut := '';
  Result := S_FALSE;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
// if set, returns the new published AccessibleName property.
// if not set, tries the name and class name properties.
// otherwise, returns the default text.
begin
  pszName := '';
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      if FVirtualTree.AccessibleName <> '' then
        pszName := FVirtualTree.AccessibleName
      else if FVirtualTree.Name <> '' then
        pszName := FVirtualTree.Name
      else if FVirtualTree.ClassName <> '' then
        pszName := FVirtualTree.ClassName
      else
        PSZName := FVirtualTree.DefaultText;
      result := S_OK;
    end;
  end
  else if varType(varChild) = VT_I4 then
  begin
    // return the name for the inner accessible item
    if (FVirtualTree <> nil) and (FVirtualTree.AccessibleItem <> nil) then
    begin
      Result := FVirtualTree.AccessibleItem.Get_accName(CHILDID_SELF, pszName);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
// Returns false, the tree itself does not have a parent.
var
  hParent: HWND;
begin
  Result := E_INVALIDARG;
  ppdispParent := nil;

  // Addition - Simon Moscrop 7/5/2009
  if (FVirtualTree.HandleAllocated) then
  begin
    (* return the accesible object from the 'parent' which is the window of the
       tree itself! (This doesn't initially appear correct but it seems to
       be exactly what all the other controls do! To verfify try pointing the
       ms accessibility explorer at a simple button control which has been dropped
       onto a form.
       *)
    hParent := FVirtualTree.Handle;
    RESULT := AccessibleObjectFromWindow(hParent,CHILDID_SELF,IID_IAccessible,pointeR(ppDispParent));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
// tells MSAA that it is a TreeView.
begin
  Result := S_OK;
//  VariantInit(pvarRole);
//  TVarData(pvarRole).VType := VT_I4;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
      pvarRole := ROLE_SYSTEM_OUTLINE;
  end
  else if VarType(varChild) = VT_I4 then
  begin
    // return the role of the inner accessible object
    if (FVirtualTree <> nil) and (FVirtualTree.FocusedNode <> nil) then
      pvarRole := ROLE_SYSTEM_OUTLINEITEM
    else
      RESULT := S_FALSE;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
var
  lIndexToSelect: Cardinal;
  i: Integer;
  lNode: PVirtualNode;
begin
  lIndexToSelect := varChild;
  if lIndexToSelect >= Self.FVirtualTree.TotalCount then
    Exit(E_INVALIDARG);
  lNode := FVirtualTree.GetFirst();
  for i := 0 to Integer(lIndexToSelect) - 1 do
    lNode := FVirtualTree.GetNext(lNode);
  Result := E_NOTIMPL;
  if (flagsSelect and SELFLAG_TAKEFOCUS) <> 0then begin
    FVirtualTree.FocusedNode := lNode;
    Result := S_OK;
  end;//if SELFLAG_TAKEFOCUS
  if (flagsSelect and SELFLAG_TAKESELECTION) <> 0 then begin
    FVirtualTree.ClearSelection();
    FVirtualTree.Selected[lNode] := True;
    Result := S_OK;
  end;//if SELFLAG_TAKEFOCUS
  if (flagsSelect and SELFLAG_ADDSELECTION) <> 0 then begin
    FVirtualTree.Selected[lNode] := True;
    Result := S_OK;
  end;
  if (flagsSelect and SELFLAG_REMOVESELECTION) <> 0 then begin
    FVirtualTree.Selected[lNode] := False;
    Result := S_OK;
  end;
  if (flagsSelect and SELFLAG_EXTENDSELECTION) <> 0 then begin
    TERDVTHackVirtualStringTree(FVirtualTree).HandleClickSelection(FVirtualTree.FocusedNode, lNode, [ssShift], False);
    Result := S_OK;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
// returns the state of the control.
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
begin
  Result := S_OK;
//  VariantInit(pvarState);
//  TVarData(pvarState).VType := VT_I4;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree <> nil then
    begin
      pvarState := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_HOTTRACKED;
      pvarState := pvarState or IsVisible[FVirtualTree.Visible];
      pvarState := pvarState or IsEnabled[FVirtualTree.Enabled];
    end
    else
      Result := E_INVALIDARG;
  end
  else if VarType(VarChild) = VT_I4 then
  begin
    // return the state of the inner accessible item
    if (FVirtualTree <> nil) and (FVirtualTree.AccessibleItem <> nil) then
    begin
      Result := FVirtualTree.AccessibleItem.Get_accState(CHILDID_SELF, pVarState);
    end
    else
      RESULT := E_INVALIDARG;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
// the TreeView control itself does not have a value, returning false here.
begin
  RESULT := S_FALSE;

  pszValue := '';
  if VarType(varChild) = VT_I4 then
    if varChild = CHILDID_SELF then
       Result := S_FALSE
    else if (FVirtualTree <> nil) and (FVirtualTree.AccessibleItem <> nil) then
      RESULT := FVirtualTree.AccessibleItem.Get_accValue(CHILDID_SELF,pszValue);
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
// not supported.
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
// not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeAccessibility.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
// not supported.
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

{ TVirtualTreeItemAccessibility }

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.accLocation(out pxLeft, pyTop, pcxWidth,
  pcyHeight: Integer; varChild: OleVariant): HResult;
// returns the location of the current accessible item.
var
  P: TPoint;
  DisplayRect: TRect;
begin
  Result := S_FALSE;
  if varChild = CHILDID_SELF then
  begin
    if FVirtualTree.FocusedNode <> nil then
    begin
      DisplayRect := FVirtualTree.GetDisplayRect(FVirtualTree.FocusedNode, FVirtualTree.Header.Columns.GetFirstVisibleColumn, True, False);//Use first visible column instead of -1
      P := FVirtualTree.ClientToScreen(DisplayRect.TopLeft);
      pxLeft := P.X;
      pyTop := P.Y;
      pcxWidth := DisplayRect.Right - DisplayRect.Left;
      pcyHeight := DisplayRect.Bottom - DisplayRect.Top;
      Result := S_OK;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
// the item does not have children. Returning false.
begin
  ppdispChild := nil;
  Result := S_FALSE;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
// the item itself does not have children, returning 0.
begin
  pcountChildren := 0;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
// not supported for an item.
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
begin
  // must override this or we get an infinite loop when using MS narrator
  // when navigating using the arrow keys.
  RESULT := S_FALSE;
  if FVirtualTree.FocusedNode <> nil then
  begin
    pvarChild := CHILDID_SELF;
    RESULT := S_OK;
  end;
end;

function TERDVTVirtualTreeItemAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
// the name is the node's caption.
var
  kind: TVTImageKind;
  ImgText: WideString;
begin
  pszName := '';
  Result := S_FALSE;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        for kind := ikNormal to ikOverlay do
        begin
          ImgText := FVirtualTree.ImageText[FVirtualTree.FocusedNode, Kind, FVirtualTree.Header.MainColumn];
          if ImgText <> '' then
            pszName := pszName + ImgText  + '  ';
        end;
        pszName := pszName + FVirtualTree.Text[FVirtualTree.FocusedNode, FVirtualTree.Header.MainColumn];
        result := S_OK;
      end
      else begin
        PSZName := FVirtualTree.DefaultText;
        result := S_OK;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
// tells MSAA that the VritualStringTree is its parent.
begin
  result := S_FALSE;
  if FVirtualTree <> nil then
  begin
    ppdispParent := FVirtualTree.Accessible;
    Result := S_OK;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
// tells MSAA that it is a TreeView item as opposed to the TreeView itself.
begin
  Result := S_OK;
//  VariantInit(pvarRole);
//  TVarData(pvarRole).VType := VT_I4;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      pvarRole := ROLE_SYSTEM_OUTLINEITEM;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
// Tells MSAA the state the item is in.
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
  IsChecked: array[Boolean] of Integer = (0, STATE_SYSTEM_CHECKED);
  IsExpanded: array[Boolean] of Integer = (0, STATE_SYSTEM_EXPANDED);
  IsCollapsed: array[Boolean] of Integer = (0, STATE_SYSTEM_COLLAPSED);
begin
  Result := S_OK;
//  VariantInit(pvarState);
//  TVarData(pvarState).VType := VT_I4;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
    begin
      pvarState := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_HOTTRACKED;
      pvarState := pvarState or IsVisible[FVirtualTree.Visible];
      pvarState := pvarState or IsEnabled[FVirtualTree.Enabled];
      if fVirtualTree.FocusedNode <> nil then
      begin
        pvarState := pvarState or IsChecked[csCheckedNormal = FVirtualTree.FocusedNode.CheckState];
        pvarState := pvarState or IsExpanded[VSExpanded in FVirtualTree.FocusedNode.States];
        if not (vsExpanded in FVirtualTree.FocusedNode.States) then
          pvarState:= PvarState or IsCollapsed[vsHasChildren in FVirtualTree.FocusedNode.States];
     end;
    end
    else
      Result := E_INVALIDARG;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TERDVTVirtualTreeItemAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
// for a TreeView item, the value is the nesting level number, 0-based.
begin
  pszValue := '';
  Result := S_FALSE;
  if varChild = childid_self then
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        PSZValue := IntToStr(FVirtualTree.GetNodeLevel(FVirtualTree.FocusedNode));
        result := S_OK;
      end;
end;

{ TVTMultiColumnItemAccessibility }

function TERDVTVTMultiColumnItemAccessibility.GetItemDescription(
  varChild: OleVariant; out pszDescription: WideString;
  IncludeMainColumn: boolean): HResult;
var
  I: Integer;
  ImgText: WideString;
  kind: TVTImageKind;
begin
  pszDescription := '';
  Result := S_FALSE;
  if varChild = childid_self then
  begin
    if FVirtualTree <> nil then
      if FVirtualTree.FocusedNode <> nil then
      begin
        if IncludeMainColumn then
        begin
          for kind := ikNormal to ikOverlay do
          begin
            ImgText := FVirtualTree.ImageText[FVirtualTree.FocusedNode, Kind, FVirtualTree.Header.MainColumn];
            if ImgText <> '' then
              ImgText := ImgText + '  ';
          end;
          pszDescription := ImgText + FVirtualTree.Text[FVirtualTree.FocusedNode, FVirtualTree.Header.MainColumn] + '; ';
        end;
        for I := 0 to FVirtualTree.Header.Columns.Count - 1 do
          if (FVirtualTree.Header.MainColumn <> I) and (coVisible in FVirtualTree.Header.Columns[I].Options) then
          begin
            for kind := ikNormal to ikOverlay do
            begin
              ImgText := FVirtualTree.ImageText[FVirtualTree.FocusedNode, Kind, I];
              if ImgText <> '' then
                ImgText := ImgText + '  ';
            end;
            ImgText := ImgText + FVirtualTree.Text[FVirtualTree.FocusedNode, I];
            if ImgText <> '' then
              pszDescription := pszDescription
               +FVirtualTree.Header.Columns[I].Text
               +': '
               + ImgText
               +'; ';
          end;
          if pszDescription <> '' then
            if pszDescription[Length(pszDescription)-1] = ';' then
              Delete(pszDescription, length(pszDescription)-1, 2);
        result := S_OK;
      end
      else begin
        PSZDescription := FVirtualTree.DefaultText;
        result := S_OK;
      end;
  end;
end;

function TERDVTVTMultiColumnItemAccessibility.Get_accDescription(
  varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  result := GetItemDescription(varChild, pszDescription, false)
end;

function TERDVTVTMultiColumnItemAccessibility.Get_accName(varChild: OleVariant;
  out pszName: WideString): HResult;
begin
  result := GetItemDescription(varChild, pszName, true)
end;

{ TVTDefaultAccessibleProvider }

function TERDVTVTDefaultAccessibleProvider.CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
begin
  result := TERDVTVirtualTreeAccessibility.Create(TERDVTVirtualStringTree(ATree));
end;

{ TVTDefaultAccessibleItemProvider }

function TERDVTVTDefaultAccessibleItemProvider.CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
begin
  result := TERDVTVirtualTreeItemAccessibility.Create(TERDVTVirtualStringTree(ATree));
end;

{ TVTMultiColumnAccessibleItemProvider }

function TERDVTVTMultiColumnAccessibleItemProvider.CreateIAccessible(ATree: TERDVTBaseVirtualTree): IAccessible;
begin
  result := nil;
  if TERDVTVirtualStringTree(ATree).Header.UseColumns then
    result := TERDVTVTMultiColumnItemAccessibility.Create(TERDVTVirtualStringTree(ATree));
end;

var
  DefaultAccessibleProvider: TERDVTVTDefaultAccessibleProvider;
  DefaultAccessibleItemProvider: TERDVTVTDefaultAccessibleItemProvider;
  MultiColumnAccessibleProvider: TERDVTVTMultiColumnAccessibleItemProvider;

class procedure TERDVTVirtualTreeAccessibility.RegisterDefaultAccessibleProviders();
begin
  if DefaultAccessibleProvider = nil then
  begin
    DefaultAccessibleProvider := TERDVTVTDefaultAccessibleProvider.Create;
    TERDVTVTAccessibilityFactory.GetAccessibilityFactory.RegisterAccessibleProvider(DefaultAccessibleProvider);
  end;
  if DefaultAccessibleItemProvider = nil then
  begin
    DefaultAccessibleItemProvider := TERDVTVTDefaultAccessibleItemProvider.Create;
    TERDVTVTAccessibilityFactory.GetAccessibilityFactory.RegisterAccessibleProvider(DefaultAccessibleItemProvider);
  end;
  if MultiColumnAccessibleProvider = nil then
  begin
    MultiColumnAccessibleProvider := TERDVTVTMultiColumnAccessibleItemProvider.Create;
    TERDVTVTAccessibilityFactory.GetAccessibilityFactory.RegisterAccessibleProvider(MultiColumnAccessibleProvider);
  end;
end;


initialization
  TERDVTVirtualTreeAccessibility.RegisterDefaultAccessibleProviders();

end.
