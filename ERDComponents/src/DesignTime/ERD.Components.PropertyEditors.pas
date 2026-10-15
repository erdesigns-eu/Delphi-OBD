unit ERD.Components.PropertyEditors;
interface
uses System.Classes;
procedure RegisterERDPropertyEditors(const Classes: array of TPersistentClass);
implementation
uses
  System.SysUtils, System.Types, System.TypInfo, System.Generics.Collections,
  Winapi.Windows, DesignIntf, DesignEditors,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Dialogs,
  ERD.Components.Icons, ERD.Components.IconCatalog, ERD.Components.Types,
  ERD.Components.Paint, ERD.Components.Theme, ERD.Components.PCBDesigner;
type
  TERDPCBBoardFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  TERDPCBComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
  TERDEnumProperty = class(TEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  TERDComponentReferenceProperty = class(TComponentProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  TERDIconPicker = class(TForm)
  private
    FSearch: TEdit;
    FList: TListBox;
    FOk: TButton;
    FIds: TList<TERDIconId>;
    FCurrent: TERDIconId;
    procedure FilterChanged(Sender: TObject);
    procedure DrawIconItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Accept(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Choose(var Value: TERDIconId): Boolean;
  end;
  TERDBuiltInIconProperty = class(TERDEnumProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
function TERDEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList];
end;
procedure TERDEnumProperty.GetValues(Proc: TGetStrProc);
var Info: PTypeInfo; Data: PTypeData; I: Integer;
begin
  Info := GetPropType;
  Data := GetTypeData(Info);
  for I := Data.MinValue to Data.MaxValue do Proc(GetEnumName(Info, I));
end;
function TERDComponentReferenceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList];
end;
procedure TERDComponentReferenceProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('(None)');
  Designer.GetComponentNames(GetTypeData(GetPropType), Proc);
end;
constructor TERDIconPicker.Create(AOwner: TComponent);
var Footer: TPanel; CancelButton: TButton;
begin
  inherited CreateNew(AOwner);
  Caption := 'ERD icon kiezen';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 620;
  Height := 540;
  Font.Name := 'Segoe UI';
  Font.Size := 10;
  FIds := TList<TERDIconId>.Create;
  FSearch := TEdit.Create(Self);
  FSearch.Parent := Self;
  FSearch.Align := alTop;
  FSearch.TextHint := 'Zoeken op naam of identifier';
  FSearch.OnChange := FilterChanged;
  Footer := TPanel.Create(Self);
  Footer.Parent := Self;
  Footer.Align := alBottom;
  Footer.Height := 48;
  Footer.BevelOuter := bvNone;
  FOk := TButton.Create(Self);
  FOk.Parent := Footer;
  FOk.SetBounds(400, 10, 90, 28);
  FOk.Anchors := [akRight, akTop];
  FOk.Caption := 'Selecteren';
  FOk.Default := True;
  FOk.OnClick := Accept;
  CancelButton := TButton.Create(Self);
  CancelButton.Parent := Footer;
  CancelButton.SetBounds(500, 10, 90, 28);
  CancelButton.Anchors := [akRight, akTop];
  CancelButton.Caption := 'Annuleren';
  CancelButton.Cancel := True;
  CancelButton.ModalResult := mrCancel;
  FList := TListBox.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.Style := lbOwnerDrawFixed;
  FList.ItemHeight := 32;
  FList.OnDrawItem := DrawIconItem;
  FList.OnClick := SelectionChanged;
  FList.OnDblClick := Accept;
  FilterChanged(Self);
end;
destructor TERDIconPicker.Destroy;
begin
  FIds.Free;
  inherited;
end;
procedure TERDIconPicker.FilterChanged(Sender: TObject);
var Id: TERDIconId; LabelText, Query: string;
begin
  if (FList = nil) or (FIds = nil) then Exit;
  Query := LowerCase(Trim(FSearch.Text));
  FList.Items.BeginUpdate;
  try
    FList.Clear;
    FIds.Clear;
    for Id := Low(TERDIconId) to High(TERDIconId) do
    begin
      LabelText := GetEnumName(TypeInfo(TERDIconId), Ord(Id)) + ' - ' + ERD_ICON_NAMES[Id];
      if (Query <> '') and (Pos(Query, LowerCase(LabelText)) = 0) then Continue;
      FIds.Add(Id);
      FList.Items.Add(LabelText);
      if Id = FCurrent then FList.ItemIndex := FList.Items.Count - 1;
    end;
  finally FList.Items.EndUpdate end;
  SelectionChanged(Self);
end;
procedure TERDIconPicker.DrawIconItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var P: TERDPainter; Palette: TERDThemePalette; Icon: TERDIcon; Ink: TColor;
begin
  if (Index < 0) or (Index >= FIds.Count) then Exit;
  Palette := TERDTheme.GetDefault.Palette;
  FList.Canvas.Brush.Color := clWindow;
  Ink := clWindowText;
  if odSelected in State then
  begin
    FList.Canvas.Brush.Color := clHighlight;
    Ink := clHighlightText;
  end;
  FList.Canvas.FillRect(Rect);
  Icon := TERDIcon.Create(nil);
  P := TERDPainter.Create(FList.Canvas, Palette, 96);
  try
    Icon.BuiltIn := FIds[Index];
    Icon.Draw(FList.Canvas, Palette, 96, Rect.Left + 18, (Rect.Top + Rect.Bottom) div 2,
      Palette.Accent, 1, True, glNone);
    P.Text(Rect.Left + 40, (Rect.Top + Rect.Bottom) div 2, FList.Items[Index],
      12, Ink, twRegular, taLeftJustify, Rect.Width - 48);
  finally P.Free; Icon.Free end;
end;
procedure TERDIconPicker.SelectionChanged(Sender: TObject);
begin
  FOk.Enabled := (FList.ItemIndex >= 0) and (FList.ItemIndex < FIds.Count);
end;
procedure TERDIconPicker.Accept(Sender: TObject);
begin
  if (FList.ItemIndex >= 0) and (FList.ItemIndex < FIds.Count) then
  begin
    FCurrent := FIds[FList.ItemIndex];
    ModalResult := mrOk;
  end;
end;
function TERDIconPicker.Choose(var Value: TERDIconId): Boolean;
begin
  FCurrent := Value;
  FilterChanged(Self);
  Result := ShowModal = mrOk;
  if Result then Value := FCurrent;
end;
function TERDBuiltInIconProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;
procedure TERDBuiltInIconProperty.Edit;
var Dialog: TERDIconPicker; Value: TERDIconId;
begin
  Value := TERDIconId(GetOrdValue);
  Dialog := TERDIconPicker.Create(nil);
  try
    if Dialog.Choose(Value) then
    begin
      SetOrdValue(Ord(Value));
      Designer.Modified;
    end;
  finally Dialog.Free end;
end;
function TERDPCBBoardFileProperty.GetAttributes: TPropertyAttributes;
begin Result := inherited GetAttributes + [paDialog]; end;
procedure TERDPCBBoardFileProperty.Edit;
var Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Filter := 'Normalized PCB board|*.json'; Dialog.FileName := GetStrValue;
    Dialog.Options := [ofFileMustExist, ofPathMustExist, ofEnableSizing];
    if Dialog.Execute then begin SetStrValue(Dialog.FileName); Designer.Modified; end;
  finally Dialog.Free; end;
end;
function TERDPCBComponentEditor.GetVerbCount: Integer;
begin Result := 2; end;
function TERDPCBComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of 0: Result := 'Fit board'; 1: Result := 'Clear board'; else Result := ''; end;
end;
procedure TERDPCBComponentEditor.ExecuteVerb(Index: Integer);
begin
  if not (Component is TERDPCBDesigner) then Exit;
  case Index of
    0: TERDPCBDesigner(Component).FitBoard;
    1: TERDPCBDesigner(Component).ClearBoard;
  else Exit;
  end;
  Designer.Modified;
end;
procedure RegisterERDPropertyEditors(const Classes: array of TPersistentClass);
var Seen: TList<TPersistentClass>; C: TPersistentClass;
  procedure Visit(AClass: TPersistentClass);
  var Props: PPropList; Count, I: Integer; Info: PTypeInfo; Child: TClass;
  begin
    if Seen.Contains(AClass) then Exit;
    Seen.Add(AClass);
    Count := GetPropList(AClass.ClassInfo, [tkEnumeration, tkClass], nil);
    if Count = 0 then Exit;
    GetMem(Props, Count * SizeOf(Pointer));
    try
      GetPropList(AClass.ClassInfo, [tkEnumeration, tkClass], Props);
      for I := 0 to Count - 1 do
      begin
        Info := Props^[I]^.PropType^;
        if Info.Kind = tkEnumeration then
          RegisterPropertyEditor(Info, AClass, string(Props^[I]^.Name), TERDEnumProperty)
        else
        begin
          Child := GetTypeData(Info).ClassType;
          if Child.InheritsFrom(TComponent) then
            RegisterPropertyEditor(Info, AClass, string(Props^[I]^.Name), TERDComponentReferenceProperty)
          else if Child.InheritsFrom(TPersistent) and (Copy(Child.ClassName, 1, 3) = 'TERD') then
            Visit(TPersistentClass(Child));
        end;
      end;
    finally FreeMem(Props) end;
  end;
begin
  Seen := TList<TPersistentClass>.Create;
  try
    for C in Classes do Visit(C);
    RegisterPropertyEditor(TypeInfo(TERDIconId), TERDIcon, 'BuiltIn', TERDBuiltInIconProperty);
    RegisterPropertyEditor(TypeInfo(TFileName), TERDPCBDesigner, 'BoardFileName', TERDPCBBoardFileProperty);
    RegisterComponentEditor(TERDPCBDesigner, TERDPCBComponentEditor);
  finally Seen.Free end;
end;
end.
