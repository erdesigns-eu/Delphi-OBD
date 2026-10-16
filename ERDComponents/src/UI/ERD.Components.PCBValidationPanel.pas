unit ERD.Components.PCBValidationPanel;
interface
uses System.Classes, System.SysUtils, System.Types, Winapi.Windows, Vcl.Controls, Vcl.Graphics,
  ERD.Components.Control, ERD.Components.Types, ERD.Components.Paint, ERD.Components.Edits,
  ERD.Components.ItemList, ERD.Components.PCBValidation;
type
  TERDPCBValidationFilter = (vfAll, vfErrors, vfWarnings, vfInfo);
  TERDPCBValidationNavigateEvent = procedure(Sender: TObject; const Issue: TERDPCBValidationIssue) of object;
  TERDPCBValidationPanel = class;
  TERDPCBValidationList = class(TERDCustomItemList)
  private
    FHost: TERDPCBValidationPanel;
    FPressedRow: Integer;
  protected
    function GetItemCount: Integer; override;
    function GetItemText(AIndex: Integer): string; override;
    function ItemHeight: Integer; override;
    procedure DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Rebuild;
  end;
  /// Presentation only: holds issue copies, no designer/scene references or event subscriptions.
  TERDPCBValidationPanel = class(TERDCustomControl)
  private
    FValidator: TERDPCBValidator;
    FIssues: TArray<TERDPCBValidationIssue>;
    FVisible: TArray<Integer>;
    FSearch: TERDEdit;
    FSeverity: TERDComboBox;
    FList: TERDPCBValidationList;
    FUpdating: Boolean;
    FOnNavigate: TERDPCBValidationNavigateEvent;
    FOnValidate: TERDPCBValidationRuleEvent;
    function GetSearchText: string;
    procedure SetSearchText(const Value: string);
    function GetSeverityFilter: TERDPCBValidationFilter;
    procedure SetSeverityFilter(Value: TERDPCBValidationFilter);
    procedure CustomRules(Sender: TObject; const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
    procedure FilterChanged(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActivateItem(Sender: TObject);
    procedure ListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RebuildVisible;
    function SelectedKey: string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ThemeChanged; override;
    procedure DensityChanged; override;
    procedure Validate(const Context: TERDPCBValidationContext);
    procedure SetIssues(const Values: TArray<TERDPCBValidationIssue>);
    procedure Clear;
    function IssueCount: Integer;
    function Issue(Index: Integer): TERDPCBValidationIssue;
    function SeverityCount(Severity: TERDPCBValidationSeverity): Integer;
    function VisibleIssue(Row: Integer; out Value: TERDPCBValidationIssue): Boolean;
    procedure NavigateSelected;
    property SearchEditor: TERDEdit read FSearch;
    property SeverityEditor: TERDComboBox read FSeverity;
    property ItemList: TERDPCBValidationList read FList;
  published
    property SearchText: string read GetSearchText write SetSearchText stored False;
    property SeverityFilter: TERDPCBValidationFilter read GetSeverityFilter write SetSeverityFilter default vfAll;
    property OnNavigate: TERDPCBValidationNavigateEvent read FOnNavigate write FOnNavigate;
    property OnValidate: TERDPCBValidationRuleEvent read FOnValidate write FOnValidate;
  end;
implementation
uses System.Math, System.StrUtils, Vcl.Forms;
constructor TERDPCBValidationList.Create(AOwner: TComponent);
begin inherited; FHost := AOwner as TERDPCBValidationPanel; FPressedRow := -1; BorderVisible := False; EmptyText := 'No matching validation messages'; end;
function TERDPCBValidationList.GetItemCount: Integer;
begin Result := 0; if FHost <> nil then Result := Length(FHost.FVisible); end;
function TERDPCBValidationList.GetItemText(AIndex: Integer): string;
var Value: TERDPCBValidationIssue;
begin
  Result := ''; if (AIndex < 0) or (AIndex >= GetItemCount) then Exit;
  if FHost.VisibleIssue(AIndex,Value) then Result := Value.ItemName+' '+Value.MessageText
  else Result := ERDPCBValidationSeverityName(TERDPCBValidationSeverity(-FHost.FVisible[AIndex]-1));
end;
function TERDPCBValidationList.ItemHeight: Integer;
begin Result := Max(ScaleValue(48),inherited ItemHeight); end;
procedure TERDPCBValidationList.DrawItem(APainter: TERDPainter; AIndex: Integer; const ARect: TRect; ASelected, AHot: Boolean);
var Value: TERDPCBValidationIssue; Severity: TERDPCBValidationSeverity; Ink: TColor; Icon: TERDGlyph; Text: string; X: Integer;
begin
  if not FHost.VisibleIssue(AIndex,Value) then
  begin
    Severity := TERDPCBValidationSeverity(-FHost.FVisible[AIndex]-1);
    APainter.Text(ARect.Left+ScaleValue(8),ARect.Top+ARect.Height/2,
      ERDPCBValidationSeverityName(Severity)+' ('+IntToStr(FHost.SeverityCount(Severity))+')',12,Palette.ForegroundText,twSemibold,taLeftJustify,ARect.Width-ScaleValue(16)); Exit;
  end;
  case Value.Severity of vsError: begin Ink := Palette.Danger; Icon := glClose; end;
    vsWarning: begin Ink := Palette.Warning; Icon := glAlert; end;
  else Ink := Palette.Accent; Icon := glInfo; end;
  APainter.RoundRect(ARect.Left+ScaleValue(8),ARect.Top+(ARect.Height-ScaleValue(24))/2,ScaleValue(24),ScaleValue(24),ScaleValue(4),ERDMixColor(Ink,ListBackground,0.12),clNone);
  APainter.GlyphLine(Icon,ARect.Left+ScaleValue(20),ARect.Top+ARect.Height/2,Ink,ScaleValue(96)/96);
  X := ARect.Left+ScaleValue(42);
  APainter.Text(X,ARect.Top+ARect.Height/2-ScaleValue(9),Value.MessageText,12,Palette.ForegroundText,twRegular,taLeftJustify,ARect.Right-X-ScaleValue(8));
  Text := Value.ItemName; if Text = '' then Text := 'Design';
  if Value.PropertyName <> '' then Text := Text+' / '+Value.PropertyName;
  if Value.Pin <> '' then Text := Text+' / '+Value.Pin;
  Text := Text+' - '+Value.Code;
  APainter.Text(X,ARect.Top+ARect.Height/2+ScaleValue(11),Text,11,ERDMixColor(Palette.ForegroundText,ListBackground,0.7),twRegular,taLeftJustify,ARect.Right-X-ScaleValue(8));
end;
procedure TERDPCBValidationList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin FPressedRow := -1; if Button = mbLeft then FPressedRow := ItemAt(X,Y); inherited; end;
procedure TERDPCBValidationList.Click;
begin inherited; if FPressedRow >= 0 then FHost.NavigateSelected; end;
procedure TERDPCBValidationList.Rebuild;
begin FPressedRow := -1; ItemsChanged; Invalidate; end;
constructor TERDPCBValidationPanel.Create(AOwner: TComponent);
begin
  inherited; Width := 500; Height := 240; TabStop := False;
  FValidator := TERDPCBValidator.Create; FValidator.OnValidate := CustomRules;
  FSearch := TERDEdit.Create(Self); FSearch.Parent := Self; FSearch.TextHint := 'Search validation'; FSearch.OnChange := FilterChanged; FSearch.OnKeyDown := InputKeyDown;
  FSeverity := TERDComboBox.Create(Self); FSeverity.Parent := Self; FSeverity.Items.Add('All messages'); FSeverity.Items.Add('Errors'); FSeverity.Items.Add('Warnings'); FSeverity.Items.Add('Information');
  FSeverity.ItemIndex := 0; FSeverity.OnChange := FilterChanged;
  FList := TERDPCBValidationList.Create(Self); FList.Parent := Self; FList.OnItemActivate := ActivateItem;
  FList.ShowHint := True; FList.OnMouseMove := ListMouseMove;
  ThemeChanged; Resize;
end;
destructor TERDPCBValidationPanel.Destroy;
begin
  FUpdating := True; FOnNavigate := nil; FOnValidate := nil;
  if FSearch <> nil then begin FSearch.OnChange := nil; FSearch.OnKeyDown := nil; end;
  if FSeverity <> nil then FSeverity.OnChange := nil;
  if FList <> nil then begin FList.OnItemActivate := nil; FList.OnMouseMove := nil; end;
  FValidator.Free; inherited;
end;
procedure TERDPCBValidationPanel.CustomRules(Sender: TObject; const Context: TERDPCBValidationContext; Report: TERDPCBValidationReport);
begin if Assigned(FOnValidate) then FOnValidate(Self,Context,Report); end;
function TERDPCBValidationPanel.GetSearchText: string;
begin Result := ''; if FSearch <> nil then Result := FSearch.Text; end;
procedure TERDPCBValidationPanel.SetSearchText(const Value: string);
begin FSearch.Text := Value; end;
function TERDPCBValidationPanel.GetSeverityFilter: TERDPCBValidationFilter;
begin Result := vfAll; if (FSeverity <> nil) and (FSeverity.ItemIndex >= 0) then Result := TERDPCBValidationFilter(FSeverity.ItemIndex); end;
procedure TERDPCBValidationPanel.SetSeverityFilter(Value: TERDPCBValidationFilter);
begin
  if Ord(Value) > Ord(High(TERDPCBValidationFilter)) then raise EArgumentException.Create('Unknown severity filter');
  if SeverityFilter = Value then Exit;
  FUpdating := True; try FSeverity.ItemIndex := Ord(Value); finally FUpdating := False; end; RebuildVisible;
end;
function TERDPCBValidationPanel.SelectedKey: string;
var Value: TERDPCBValidationIssue;
begin Result := ''; if (FList <> nil) and VisibleIssue(FList.ItemIndex,Value) then Result := Value.Key; end;
procedure TERDPCBValidationPanel.Validate(const Context: TERDPCBValidationContext);
var Values: TArray<TERDPCBValidationIssue>;
begin Values := FValidator.Validate(Context); SetIssues(Values); end;
procedure TERDPCBValidationPanel.SetIssues(const Values: TArray<TERDPCBValidationIssue>);
var SavedKey: string; I: Integer;
begin
  for I := 0 to High(Values) do
    if (Ord(Values[I].Severity) > Ord(High(TERDPCBValidationSeverity))) or (Trim(Values[I].Code) = '') or (Trim(Values[I].MessageText) = '') then
      raise EArgumentException.Create('Invalid validation issue');
  SavedKey := SelectedKey; FList.ClearSelection; FIssues := Copy(Values); RebuildVisible;
  for I := 0 to High(FVisible) do if (FVisible[I] >= 0) and (FIssues[FVisible[I]].Key = SavedKey) then begin FList.ItemIndex := I; Break; end;
  Invalidate;
end;
procedure TERDPCBValidationPanel.Clear;
begin SetIssues(nil); end;
function TERDPCBValidationPanel.IssueCount: Integer;
begin Result := Length(FIssues); end;
function TERDPCBValidationPanel.Issue(Index: Integer): TERDPCBValidationIssue;
begin if (Index < 0) or (Index >= Length(FIssues)) then raise EArgumentOutOfRangeException.Create('Unknown validation issue'); Result := FIssues[Index]; end;
function TERDPCBValidationPanel.SeverityCount(Severity: TERDPCBValidationSeverity): Integer;
var Value: TERDPCBValidationIssue;
begin Result := 0; for Value in FIssues do if Value.Severity = Severity then Inc(Result); end;
function TERDPCBValidationPanel.VisibleIssue(Row: Integer; out Value: TERDPCBValidationIssue): Boolean;
begin Result := (Row >= 0) and (Row < Length(FVisible)); if not Result then Exit; Result := FVisible[Row] >= 0; if Result then Value := FIssues[FVisible[Row]]; end;
procedure TERDPCBValidationPanel.RebuildVisible;
var SavedKey, Word, Haystack: string; Words: TArray<string>; Group, I, N, J, Index, GroupStart: Integer; Match: Boolean;
  procedure Append(Value: Integer);
  begin N := Length(FVisible); SetLength(FVisible,N+1); FVisible[N] := Value; end;
begin
  if FUpdating or (FList = nil) then Exit;
  SavedKey := SelectedKey; FUpdating := True;
  try
    FList.ClearSelection; FVisible := nil; Words := Trim(SearchText).Split([' ',#9],TStringSplitOptions.ExcludeEmpty);
    for Group := 0 to 2 do
    begin
      if (SeverityFilter <> vfAll) and (Ord(SeverityFilter)-1 <> Group) then Continue;
      GroupStart := Length(FVisible);
      for I := 0 to High(FIssues) do
      begin
        if Ord(FIssues[I].Severity) <> Group then Continue;
        Haystack := FIssues[I].ItemName+' '+FIssues[I].MessageText+' '+FIssues[I].Code+' '+FIssues[I].Pin+' '+FIssues[I].PropertyName;
        Match := True; for Word in Words do if not ContainsText(Haystack,Word) then begin Match := False; Break; end;
        if not Match then Continue;
        if Length(FVisible) = GroupStart then Append(-Group-1);
        Append(I); J := High(FVisible);
        while J > GroupStart+1 do
        begin
          Index := FVisible[J-1];
          if CompareText(FIssues[Index].ItemName+#9+FIssues[Index].MessageText,FIssues[I].ItemName+#9+FIssues[I].MessageText) <= 0 then Break;
          FVisible[J] := Index; Dec(J);
        end;
        FVisible[J] := I;
      end;
    end;
    FList.Rebuild; FList.ScrollTo(0); Application.CancelHint; FList.Hint := '';
    for I := 0 to High(FVisible) do if (FVisible[I] >= 0) and (FIssues[FVisible[I]].Key = SavedKey) then begin FList.ItemIndex := I; Break; end;
    if Length(FIssues) = 0 then FList.EmptyText := 'No validation messages' else FList.EmptyText := 'No matching validation messages';
  finally FUpdating := False; end;
end;
procedure TERDPCBValidationPanel.FilterChanged(Sender: TObject);
begin RebuildVisible; end;
procedure TERDPCBValidationPanel.ListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Value: TERDPCBValidationIssue; Text: string;
begin
  Text := '';
  if VisibleIssue(FList.ItemAt(X,Y),Value) then
    Text := Value.ItemName+' - '+Value.MessageText+sLineBreak+Value.Code+' / '+Value.PropertyName+' / '+Value.Pin;
  if FList.Hint <> Text then begin Application.CancelHint; FList.Hint := Text; end;
end;
procedure TERDPCBValidationPanel.ActivateItem(Sender: TObject);
begin NavigateSelected; end;
procedure TERDPCBValidationPanel.NavigateSelected;
var Value: TERDPCBValidationIssue;
begin if Enabled and VisibleIssue(FList.ItemIndex,Value) and Assigned(FOnNavigate) then FOnNavigate(Self,Value); end;
procedure TERDPCBValidationPanel.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin if Key = VK_DOWN then begin if FList.CanFocus then FList.SetFocus; Key := 0; end; end;
procedure TERDPCBValidationPanel.PaintControl(ACanvas: TCanvas);
var Painter: TERDPainter;
begin
  ACanvas.Brush.Color := Palette.Background; ACanvas.FillRect(ClientRect); Painter := TERDPainter.Create(ACanvas,Palette,ScaleValue(96));
  try Painter.Text(ScaleValue(8),ScaleValue(16),'Validation: '+IntToStr(SeverityCount(vsError))+' errors, '+IntToStr(SeverityCount(vsWarning))+' warnings, '+IntToStr(SeverityCount(vsInfo))+' information',12,Palette.ForegroundText,twSemibold,taLeftJustify,ClientWidth-ScaleValue(16));
  finally Painter.Free; end;
end;
procedure TERDPCBValidationPanel.Resize;
var Margin, H, FilterWidth, Top: Integer;
begin
  inherited; if (FSearch = nil) or (FSeverity = nil) or (FList = nil) then Exit;
  Margin := ScaleValue(8); H := ScaleValue(Max(30,Metrics.Edit)); Top := ScaleValue(32); FilterWidth := Min(ScaleValue(160),ClientWidth div 2);
  FSearch.SetBounds(Margin,Top,Max(1,ClientWidth-3*Margin-FilterWidth),H);
  FSeverity.SetBounds(Max(Margin,ClientWidth-Margin-FilterWidth),Top,Max(1,FilterWidth),H);
  Inc(Top,H+Margin); FList.SetBounds(0,Top,ClientWidth,Max(1,ClientHeight-Top));
end;
procedure TERDPCBValidationPanel.ThemeChanged;
begin inherited;
  if FSearch <> nil then begin FSearch.Theme := Theme; FSearch.ThemeChanged; end;
  if FSeverity <> nil then begin FSeverity.Theme := Theme; FSeverity.ThemeChanged; end;
  if FList <> nil then begin FList.Theme := Theme; FList.ThemeChanged; end; Resize;
end;
procedure TERDPCBValidationPanel.DensityChanged;
begin inherited;
  if FSearch <> nil then begin FSearch.ParentDensity := False; FSearch.Density := Density; end;
  if FSeverity <> nil then begin FSeverity.ParentDensity := False; FSeverity.Density := Density; end;
  if FList <> nil then begin FList.ParentDensity := False; FList.Density := Density; end; Resize;
end;
procedure TERDPCBValidationPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin end;
end.
