unit ERD.Components.DropZone;
interface
uses
  System.Classes, System.SysUtils, System.Types, System.Masks,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.ShellAPI,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint,
  ERD.Components.Buttons, ERD.Components.Icons, ERD.Components.IconCatalog;
type
  TERDDropZone = class;
  TERDFileDropEvent = procedure(Sender: TObject; Files: TStrings) of object;
  TERDFileDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FOwner: TERDDropZone;
    FCanDrop: Boolean;
    function ReadFiles(const Data: IDataObject; Files: TStrings): Boolean;
  public
    constructor Create(Owner: TERDDropZone);
    procedure Detach;
    function DragEnter(const DataObject: IDataObject; KeyState: Integer; Pt: TPoint; var Effect: Longint): HResult; stdcall;
    function DragOver(KeyState: Integer; Pt: TPoint; var Effect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObject: IDataObject; KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult; stdcall;
  end;
  /// <summary>Explorer file drop target with filtered copy feedback; never moves files.</summary>
  TERDDropZone = class(TERDInteractiveControl)
  private
    FAdapter: TERDFileDropTarget;
    FDropTarget: IDropTarget;
    FOleInitialized, FRegistered, FDragActive: Boolean;
    FAcceptDrops, FAllowDirectories, FAllowMultiple: Boolean;
    FFileMasks, FDescription: string;
    FIcon: TERDIcon;
    FOnFilesDropped: TERDFileDropEvent;
    procedure SetAcceptDrops(Value: Boolean);
    procedure SetFileMasks(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetIcon(Value: TERDIcon);
    procedure SetDragActive(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcceptsFile(const FileName: string): Boolean;
    /// <summary>Filters a list into Accepted. Returns False for empty or disallowed multiple drops.</summary>
    function FilterFiles(Files, Accepted: TStrings): Boolean;
    /// <summary>Runs the same filtering/event path used by Explorer. Caller retains Files.</summary>
    function DropFiles(Files: TStrings): Boolean;
    property DragActive: Boolean read FDragActive;
  published
    property Caption;
    property Description: string read FDescription write SetDescription;
    property Icon: TERDIcon read FIcon write SetIcon;
    property FileMasks: string read FFileMasks write SetFileMasks;
    property AcceptDrops: Boolean read FAcceptDrops write SetAcceptDrops default True;
    property AllowDirectories: Boolean read FAllowDirectories write FAllowDirectories default False;
    property AllowMultiple: Boolean read FAllowMultiple write FAllowMultiple default True;
    property AutoSize default False;
    property OnFilesDropped: TERDFileDropEvent read FOnFilesDropped write FOnFilesDropped;
    property OnClick;
  end;
implementation
constructor TERDFileDropTarget.Create(Owner: TERDDropZone);
begin inherited Create; FOwner := Owner end;
procedure TERDFileDropTarget.Detach;
begin FOwner := nil end;
function TERDFileDropTarget.ReadFiles(const Data: IDataObject; Files: TStrings): Boolean;
var Format: TFormatEtc; Medium: TStgMedium; Count, I, Size: Cardinal; Name: string;
begin
  Result := False;
  if Data = nil then Exit;
  FillChar(Format, SizeOf(Format), 0);
  Format.cfFormat := CF_HDROP;
  Format.dwAspect := DVASPECT_CONTENT;
  Format.lindex := -1;
  Format.tymed := TYMED_HGLOBAL;
  FillChar(Medium, SizeOf(Medium), 0);
  if Data.GetData(Format, Medium) <> S_OK then Exit;
  try
    if Medium.tymed <> TYMED_HGLOBAL then Exit;
    Count := DragQueryFile(HDROP(Medium.hGlobal), Cardinal(-1), nil, 0);
    I := 0;
    while I < Count do
    begin
      Size := DragQueryFile(HDROP(Medium.hGlobal), I, nil, 0);
      SetLength(Name, Size + 1);
      DragQueryFile(HDROP(Medium.hGlobal), I, PChar(Name), Size + 1);
      SetLength(Name, Size);
      Files.Add(Name);
      Inc(I);
    end;
    Result := Files.Count > 0;
  finally ReleaseStgMedium(Medium) end;
end;
function TERDFileDropTarget.DragEnter(const DataObject: IDataObject; KeyState: Integer; Pt: TPoint; var Effect: Longint): HResult;
var Files, Accepted: TStringList;
begin
  Files := TStringList.Create;
  Accepted := TStringList.Create;
  Result := S_OK;
  FCanDrop := False;
  try
    try
      if (FOwner <> nil) and ReadFiles(DataObject, Files) then FCanDrop := FOwner.FilterFiles(Files, Accepted);
    except Result := E_FAIL end;
    DragOver(KeyState, Pt, Effect);
  finally Accepted.Free; Files.Free end;
end;
function TERDFileDropTarget.DragOver(KeyState: Integer; Pt: TPoint; var Effect: Longint): HResult;
begin
  if (FOwner <> nil) and FCanDrop and FOwner.Enabled and FOwner.AcceptDrops and
    ((Effect and DROPEFFECT_COPY) <> 0) then Effect := DROPEFFECT_COPY else Effect := DROPEFFECT_NONE;
  if FOwner <> nil then FOwner.SetDragActive(Effect <> DROPEFFECT_NONE);
  Result := S_OK;
end;
function TERDFileDropTarget.DragLeave: HResult;
begin
  FCanDrop := False;
  if FOwner <> nil then FOwner.SetDragActive(False);
  Result := S_OK;
end;
function TERDFileDropTarget.Drop(const DataObject: IDataObject; KeyState: Integer; Pt: TPoint; var Effect: Integer): HResult;
var Files: TStringList; CanCopy: Boolean;
begin
  CanCopy := (Effect and DROPEFFECT_COPY) <> 0;
  Effect := DROPEFFECT_NONE;
  Result := S_OK;
  if FOwner <> nil then FOwner.SetDragActive(False);
  Files := TStringList.Create;
  try
    try
      if CanCopy and (FOwner <> nil) and ReadFiles(DataObject, Files) and FOwner.DropFiles(Files) then Effect := DROPEFFECT_COPY;
    except
      Result := E_FAIL;
      Application.HandleException(nil);
    end;
  finally Files.Free end;
end;
constructor TERDDropZone.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  FAcceptDrops := True;
  FAllowMultiple := True;
  FFileMasks := '*';
  FIcon := TERDIcon.Create(Self);
  FIcon.BuiltIn := ei018Upload;
  Caption := 'Drop files here';
  FDescription := 'or click to browse';
  SetBounds(Left, Top, 360, 140);
end;
destructor TERDDropZone.Destroy;
begin
  // Revoke while the adapter and OLE apartment are still alive.
  if HandleAllocated then DestroyWnd;
  FIcon.Free;
  inherited;
end;
procedure TERDDropZone.CreateWnd;
var Status: HResult;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  Status := OleInitialize(nil);
  FOleInitialized := (Status = S_OK) or (Status = S_FALSE);
  if not FOleInitialized then Exit;
  FAdapter := TERDFileDropTarget.Create(Self);
  FDropTarget := FAdapter;
  FRegistered := RegisterDragDrop(Handle, FDropTarget) = S_OK;
  if not FRegistered then
  begin
    FAdapter.Detach;
    FAdapter := nil;
    FDropTarget := nil;
    OleUninitialize;
    FOleInitialized := False;
  end;
end;
procedure TERDDropZone.DestroyWnd;
begin
  if FAdapter <> nil then FAdapter.Detach;
  if FRegistered then RevokeDragDrop(Handle);
  FRegistered := False;
  FAdapter := nil;
  FDropTarget := nil;
  if FOleInitialized then OleUninitialize;
  FOleInitialized := False;
  FDragActive := False;
  inherited;
end;
procedure TERDDropZone.SetAcceptDrops(Value: Boolean);
begin FAcceptDrops := Value; if not Value then SetDragActive(False); Invalidate end;
procedure TERDDropZone.SetDragActive(Value: Boolean);
begin if FDragActive = Value then Exit; FDragActive := Value; Invalidate end;
procedure TERDDropZone.SetFileMasks(const Value: string);
var Mask: string;
begin
  for Mask in Value.Split([';']) do if Trim(Mask) <> '' then MatchesMask('', Trim(Mask));
  FFileMasks := Value;
end;
procedure TERDDropZone.SetDescription(const Value: string);
begin FDescription := Value; Invalidate end;
procedure TERDDropZone.SetIcon(Value: TERDIcon);
begin FIcon.Assign(Value); Invalidate end;
procedure TERDDropZone.CMEnabledChanged(var Message: TMessage);
begin inherited; if not Enabled then SetDragActive(False); Invalidate end;
function TERDDropZone.AcceptsFile(const FileName: string): Boolean;
var Mask: string;
begin
  Result := False;
  if FileName = '' then Exit;
  if DirectoryExists(FileName) then begin Result := FAllowDirectories; Exit end;
  if not FileExists(FileName) then Exit;
  if Trim(FFileMasks) = '' then begin Result := True; Exit end;
  for Mask in FFileMasks.Split([';']) do
    if (Trim(Mask) <> '') and MatchesMask(ExtractFileName(FileName), Trim(Mask)) then Exit(True);
end;
function TERDDropZone.FilterFiles(Files, Accepted: TStrings): Boolean;
var FileName: string;
begin
  if Accepted = nil then raise EArgumentNilException.Create('Accepted');
  if Files = Accepted then raise EArgumentException.Create('Files and Accepted must be separate lists');
  Accepted.Clear;
  if (Files = nil) or not Enabled or not FAcceptDrops then Exit(False);
  for FileName in Files do if AcceptsFile(FileName) then Accepted.Add(FileName);
  Result := (Accepted.Count > 0) and (FAllowMultiple or (Accepted.Count = 1));
  if not Result then Accepted.Clear;
end;
function TERDDropZone.DropFiles(Files: TStrings): Boolean;
var Accepted: TStringList;
begin
  Accepted := TStringList.Create;
  try
    Result := FilterFiles(Files, Accepted);
    if Result and Assigned(FOnFilesDropped) then FOnFilesDropped(Self, Accepted);
  finally Accepted.Free end;
end;
procedure TERDDropZone.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Ink, Border, Fill: TColor; CY: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Ink := Palette.ForegroundText;
    Border := Palette.NeutralLight;
    Fill := Palette.GaugeFace;
    if not Enabled then Ink := P.DisabledText
    else if FDragActive then begin Fill := P.Tint(Palette.Accent, 0.15); Border := Palette.Accent end
    else if Hover then begin Fill := P.Tint(Palette.Accent, 0.05); Border := Palette.Accent end;
    P.RoundRect(P.S(3), P.S(3), Width - P.S(6), Height - P.S(6), P.S(6), Fill, clNone);
    P.DashedRoundRect(P.S(3), P.S(3), Width - P.S(6), Height - P.S(6), P.S(6), Border);
    CY := Height div 2;
    FIcon.Draw(ACanvas, Palette, ScaleValue(96), Width div 2, CY - P.S(28), Palette.Accent, 1.4, Enabled);
    P.Text(Width div 2, CY, Caption, ERD_TEXT_BODY, Ink, twSemibold, taCenter, Width - P.S(20));
    P.Text(Width div 2, CY + P.S(24), FDescription, ERD_TEXT_BODY, P.DisabledText,
      twRegular, taCenter, Width - P.S(20));
    if HasFocus then P.FocusRing(P.S(6), P.S(6), Width - P.S(12), Height - P.S(12), P.S(4));
  finally P.Free end;
end;
end.
