unit ERD.Components.PCBLibrary;
interface
uses System.Classes, System.SysUtils;
type
  TERDPCBTemplateInfo = record
    ID, Name, FileName: string;
    PinCount, TerminalCount: Integer;
  end;
  // File-backed catalog; geometry stays in templates rather than application code.
  TERDPCBTemplateLibrary = class
  private
    FDirectory: string;
    FItems: TArray<TERDPCBTemplateInfo>;
    FIssues: TStringList;
    function ReadInfo(const FileName: string): TERDPCBTemplateInfo;
  public
    constructor Create(const Directory: string);
    destructor Destroy; override;
    procedure Refresh;
    function Count: Integer;
    function Item(Index: Integer): TERDPCBTemplateInfo;
    function ImportTemplate(const SourceFile: string): string;
    property Directory: string read FDirectory;
    property Issues: TStringList read FIssues;
  end;
implementation
uses System.IOUtils, Winapi.Windows, ERD.Components.PCBDesigner;
constructor TERDPCBTemplateLibrary.Create(const Directory: string);
begin
  inherited Create; FDirectory := TPath.GetFullPath(Directory); FIssues := TStringList.Create;
end;
destructor TERDPCBTemplateLibrary.Destroy;
begin FIssues.Free; inherited; end;
function TERDPCBTemplateLibrary.Count: Integer;
begin Result := Length(FItems); end;
function TERDPCBTemplateLibrary.Item(Index: Integer): TERDPCBTemplateInfo;
begin
  if (Index < 0) or (Index >= Count) then raise EArgumentOutOfRangeException.Create('Unknown board template');
  Result := FItems[Index];
end;
function TERDPCBTemplateLibrary.ReadInfo(const FileName: string): TERDPCBTemplateInfo;
var Probe: TERDPCBDesigner;
begin
  Result := Default(TERDPCBTemplateInfo);
  // Share the designer's geometry/policy validation and legacy identity fallback.
  Probe := TERDPCBDesigner.Create(nil);
  try
    Probe.Visible := False; Probe.ParentWindow := GetDesktopWindow; Probe.LoadBoard(FileName);
    Result.FileName := TPath.GetFullPath(FileName);
    Result.ID := Probe.TemplateID; Result.Name := Probe.TemplateName;
    Result.PinCount := Probe.ChannelCount; Result.TerminalCount := Probe.TerminalCount;
  finally Probe.Free; end;
end;
procedure TERDPCBTemplateLibrary.Refresh;
var FileName: string; Info: TERDPCBTemplateInfo; N: Integer;
begin
  FItems := nil; FIssues.Clear;
  if not TDirectory.Exists(FDirectory) then Exit;
  for FileName in TDirectory.GetFiles(FDirectory, '*.json', TSearchOption.soTopDirectoryOnly) do
    try
      Info := ReadInfo(FileName); N := Length(FItems); SetLength(FItems, N + 1); FItems[N] := Info;
    except on E: Exception do FIssues.Add(ExtractFileName(FileName) + ': ' + E.Message); end;
end;
function TERDPCBTemplateLibrary.ImportTemplate(const SourceFile: string): string;
var Info: TERDPCBTemplateInfo; Base: string; N: Integer;
begin
  Info := ReadInfo(SourceFile);
  if not TDirectory.Exists(FDirectory) then TDirectory.CreateDirectory(FDirectory);
  Base := TPath.Combine(FDirectory, ChangeFileExt(ExtractFileName(Info.FileName), '.json')); Result := Base;
  if SameText(Result, Info.FileName) then begin Refresh; Exit; end;
  N := 1;
  while TFile.Exists(Result) do
  begin Result := ChangeFileExt(Base, '') + '-' + IntToStr(N) + '.json'; Inc(N); end;
  TFile.Copy(Info.FileName, Result, False); Refresh;
end;
end.
