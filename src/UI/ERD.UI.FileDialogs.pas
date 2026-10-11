//------------------------------------------------------------------------------
//  ERD.UI.FileDialogs
//
//  Themed file and folder dialog components for OBD Studio.
//
//    TOBDOpenDialog, TOBDSaveDialog        themed file dialogs.
//    TOBDOpenPictureDialog,                picture variants with preview.
//    TOBDSavePictureDialog
//    TOBDFolderDialog                      themed folder picker.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed dialogs.
//------------------------------------------------------------------------------

unit ERD.UI.FileDialogs;

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  ERD.UI.Types,
  ERD.UI.Theme,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.Chips,
  ERD.UI.Dialogs,
  ERD.UI.ShellControls;

type
  /// <summary>File dialog option.</summary>
  TOBDFileOption = (foReadOnly, foOverwritePrompt, foHideReadOnly,
    foNoChangeDir, foAllowMultiSelect, foPathMustExist, foFileMustExist,
    foCreatePrompt, foShowHidden);

  /// <summary>File dialog option set.</summary>
  TOBDFileOptions = set of TOBDFileOption;

  /// <summary>Base class shared by themed file dialogs.</summary>
  TOBDCustomFileDialog = class(TOBDCommonDialog)
  private
    FFileName: string;
    FFiles: TStringList;
    FFilter: string;
    FFilterIndex: Integer;
    FInitialDir: string;
    FDefaultExt: string;
    FOptions: TOBDFileOptions;
    FPlaces: TOBDPlaces;
    FOnSelectionChange: TNotifyEvent;
    FOnFolderChange: TNotifyEvent;
    procedure SetFilterIndex(AValue: Integer);
    procedure SetPlaces(AValue: TOBDPlaces);
  protected
    /// <summary>True for save dialogs.</summary>
    function IsSaveDialog: Boolean; virtual;
    /// <summary>True for picture dialogs.</summary>
    function IsPictureDialog: Boolean; virtual;
    /// <summary>Caption of the accept button.</summary>
    function AcceptCaption: string; virtual;
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
    /// <summary>Returns the active filter mask.</summary>
    function CurrentMask: string;
    /// <summary>Returns one extension implied by the active filter.</summary>
    function FilterExtension: string;
    /// <summary>Fires selection change.</summary>
    procedure DoSelectionChange;
    /// <summary>Fires folder change.</summary>
    procedure DoFolderChange;
  public
    /// <summary>Shows the dialog.</summary>
    function Execute: Boolean; override;
    /// <summary>Creates the dialog.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned strings and places.</summary>
    destructor Destroy; override;
    /// <summary>Selected file name.</summary>
    property FileName: string read FFileName write FFileName;
    /// <summary>Selected files.</summary>
    property Files: TStringList read FFiles;
  published
    /// <summary>Filter in VCL format: Caption|mask|Caption|mask.</summary>
    property Filter: string read FFilter write FFilter;
    /// <summary>1-based filter index.</summary>
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex
      default 1;
    /// <summary>Initial directory.</summary>
    property InitialDir: string read FInitialDir write FInitialDir;
    /// <summary>Default extension for save names.</summary>
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    /// <summary>Dialog options.</summary>
    property Options: TOBDFileOptions read FOptions write FOptions;
    /// <summary>Application places.</summary>
    property Places: TOBDPlaces read FPlaces write SetPlaces;
    /// <summary>Selection change event.</summary>
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange
      write FOnSelectionChange;
    /// <summary>Folder change event.</summary>
    property OnFolderChange: TNotifyEvent read FOnFolderChange
      write FOnFolderChange;
  end;

  /// <summary>Themed open-file dialog.</summary>
  TOBDOpenDialog = class(TOBDCustomFileDialog)
  protected
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  published
    /// <summary>Selected file.</summary>
    property FileName;
    /// <summary>Selected files.</summary>
    property Files;
    /// <summary>Theme.</summary>
    property Theme;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Title.</summary>
    property Title;
    /// <summary>OnShow.</summary>
    property OnShow;
    /// <summary>OnClose.</summary>
    property OnClose;
  end;

  /// <summary>Themed save-file dialog.</summary>
  TOBDSaveDialog = class(TOBDCustomFileDialog)
  protected
    /// <summary>True for save dialogs.</summary>
    function IsSaveDialog: Boolean; override;
    /// <summary>Accept button caption.</summary>
    function AcceptCaption: string; override;
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  published
    property FileName;
    property Files;
    property Theme;
    property Density;
    property ParentDensity;
    property Title;
    property OnShow;
    property OnClose;
  end;

  /// <summary>Themed open-picture dialog.</summary>
  TOBDOpenPictureDialog = class(TOBDOpenDialog)
  protected
    /// <summary>Picture mode.</summary>
    function IsPictureDialog: Boolean; override;
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates with a picture filter.</summary>
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Themed save-picture dialog.</summary>
  TOBDSavePictureDialog = class(TOBDSaveDialog)
  protected
    /// <summary>Picture mode.</summary>
    function IsPictureDialog: Boolean; override;
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates with a picture filter.</summary>
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary>Themed folder picker.</summary>
  TOBDFolderDialog = class(TOBDCommonDialog)
  private
    FDirectory: string;
    FInitialDir: string;
    FOptions: TOBDFileOptions;
  protected
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  public
    /// <summary>Shows the dialog.</summary>
    function Execute: Boolean; override;
  published
    /// <summary>Selected directory.</summary>
    property Directory: string read FDirectory write FDirectory;
    /// <summary>Initial directory.</summary>
    property InitialDir: string read FInitialDir write FInitialDir;
    /// <summary>Options.</summary>
    property Options: TOBDFileOptions read FOptions write FOptions;
    /// <summary>Theme.</summary>
    property Theme;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Title.</summary>
    property Title;
    /// <summary>OnShow.</summary>
    property OnShow;
    /// <summary>OnClose.</summary>
    property OnClose;
  end;

implementation

type
  TReturnEdit = class(TOBDEdit)
  strict private
    FOnReturn: TNotifyEvent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property OnReturn: TNotifyEvent read FOnReturn write FOnReturn;
  end;

  TPicturePreview = class(TOBDCustomControl)
  strict private
    FPicture: TPicture;
    FFileName: string;
    FInfo: string;
    procedure SetFileName(const AValue: string);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
  end;

  TFileDialogController = class(TComponent)
  strict private
    FDialog: TOBDCustomFileDialog;
    FWindow: TOBDDialogWindow;
    FPlaces: TOBDPlacesPane;
    FBreadcrumb: TOBDBreadcrumb;
    FList: TOBDFileList;
    FNameEdit: TReturnEdit;
    FSearchEdit: TReturnEdit;
    FTypeCombo: TOBDComboBox;
    FBanner: TOBDBanner;
    FPreview: TPicturePreview;
    FOK: TOBDButton;
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FReplacing: Boolean;
    procedure AddHistory(const APath: string);
    function CurrentDir: string;
    function TypedPath: string;
    function SelectedFilePath: string;
    procedure SetBanner(const ATitle, AText: string; AKind: TOBDBannerKind);
  public
    constructor CreateController(AOwner: TComponent;
      ADialog: TOBDCustomFileDialog; AWindow: TOBDDialogWindow);
    destructor Destroy; override;
    procedure Build;
    procedure Navigate(const APath: string; AHistory: Boolean = True);
    procedure PlacesNavigate(Sender: TObject; const APath: string);
    procedure BreadcrumbNavigate(Sender: TObject; const APath: string);
    procedure FolderOpen(Sender: TObject; const APath: string);
    procedure FileActivate(Sender: TObject; const APath: string);
    procedure SelectionChange(Sender: TObject);
    procedure SearchChange(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure NameReturn(Sender: TObject);
    procedure Accept(Sender: TObject);
    procedure Back(Sender: TObject);
    procedure Forward(Sender: TObject);
    procedure Up(Sender: TObject);
    procedure NewFolder(Sender: TObject);
    procedure ChangeName(Sender: TObject);
    procedure Replace(Sender: TObject);
  end;

  TFolderController = class(TComponent)
  strict private
    FDialog: TOBDFolderDialog;
    FWindow: TOBDDialogWindow;
    FTree: TOBDFolderTree;
    FEdit: TReturnEdit;
  public
    constructor CreateController(AOwner: TComponent; ADialog: TOBDFolderDialog;
      AWindow: TOBDDialogWindow);
    procedure Build;
    procedure TreeChange(Sender: TObject; const APath: string);
    procedure EditReturn(Sender: TObject);
    procedure NewFolder(Sender: TObject);
    procedure Accept(Sender: TObject);
  end;


function LocalFileSize(const AFileName: string): Int64;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(AFileName, faAnyFile, SR) = 0 then
  try
    Result := SR.Size;
  finally
    FindClose(SR);
  end;
end;

function LocalFormatSize(ASize: Int64): string;
begin
  if ASize < 1024 then
    Result := Format('%d B', [ASize])
  else if ASize < 1024 * 1024 then
    Result := Format('%d KB', [(ASize + 1023) div 1024])
  else
    Result := FormatFloat('0.# MB', ASize / (1024 * 1024));
end;

function StartDirectory(const AInitial, AFile: string): string;
begin
  Result := AInitial;
  if (Result = '') and (AFile <> '') then
    Result := ExtractFilePath(AFile);
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetCurrentDir;
end;

procedure TReturnEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    if Assigned(FOnReturn) then
      FOnReturn(Self);
  end;
end;

constructor TPicturePreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  Width := 220;
  Height := 300;
  TabStop := False;
end;

destructor TPicturePreview.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TPicturePreview.SetFileName(const AValue: string);
var
  S: Int64;
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
  FInfo := '';
  FPicture.Assign(nil);
  if FileExists(FFileName) then
  try
    FPicture.LoadFromFile(FFileName);
    S := LocalFileSize(FFileName);
    FInfo := Format('%d x %d   %s', [FPicture.Width, FPicture.Height,
      LocalFormatSize(S)]);
  except
    FPicture.Assign(nil);
    FInfo := '';
  end;
  Invalidate;
end;

procedure TPicturePreview.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  R: TRect;
  Scale: Double;
  W, H: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    R := Rect(ScaleValue(12), ScaleValue(12), Width - ScaleValue(12),
      Height - ScaleValue(44));
    if (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty then
    begin
      Scale := System.Math.Min(R.Width / System.Math.Max(1, FPicture.Width),
        R.Height / System.Math.Max(1, FPicture.Height));
      W := Round(FPicture.Width * Scale);
      H := Round(FPicture.Height * Scale);
      R := Rect((Width - W) div 2, R.Top + (R.Height - H) div 2,
        (Width + W) div 2, R.Top + (R.Height - H) div 2 + H);
      ACanvas.StretchDraw(R, FPicture.Graphic);
      P.Text(Width div 2, Height - ScaleValue(22), FInfo, OBD_TEXT_SMALL,
        Palette.GaugeLabel, twRegular, taCenter, Width - ScaleValue(16));
    end
    else
      P.Text(Width div 2, Height div 2, 'No preview', OBD_TEXT_BODY,
        Palette.GaugeLabel, twRegular, taCenter, Width - ScaleValue(16));
  finally
    P.Free;
  end;
end;

{ TOBDCustomFileDialog }

constructor TOBDCustomFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FPlaces := TOBDPlaces.Create(Self);
  FFilter := 'All files|*.*';
  FFilterIndex := 1;
  FOptions := [foFileMustExist, foPathMustExist];
end;

destructor TOBDCustomFileDialog.Destroy;
begin
  FPlaces.Free;
  FFiles.Free;
  inherited;
end;

procedure TOBDCustomFileDialog.SetFilterIndex(AValue: Integer);
begin
  FFilterIndex := System.Math.Max(1, AValue);
end;

procedure TOBDCustomFileDialog.SetPlaces(AValue: TOBDPlaces);
begin
  if AValue <> nil then
    FPlaces.Assign(AValue)
  else
    FPlaces.Clear;
end;

function TOBDCustomFileDialog.IsSaveDialog: Boolean;
begin
  Result := False;
end;

function TOBDCustomFileDialog.IsPictureDialog: Boolean;
begin
  Result := False;
end;

function TOBDCustomFileDialog.AcceptCaption: string;
begin
  Result := 'Open';
end;

function TOBDCustomFileDialog.DefaultTitle: string;
begin
  Result := 'Open';
end;

function TOBDCustomFileDialog.CurrentMask: string;
var
  L: TStringList;
  I: Integer;
begin
  Result := '*.*';
  L := TStringList.Create;
  try
    ExtractStrings(['|'], [], PChar(FFilter), L);
    I := (FFilterIndex - 1) * 2 + 1;
    if (I >= 0) and (I < L.Count) and (Trim(L[I]) <> '') then
      Result := Trim(L[I]);
  finally
    L.Free;
  end;
end;

function TOBDCustomFileDialog.FilterExtension: string;
var
  Mask: string;
  P: Integer;
begin
  Result := '';
  Mask := CurrentMask;
  if Pos(';', Mask) > 0 then
    Exit;
  P := Pos('*.', Mask);
  if P > 0 then
    Result := Copy(Mask, P + 1, MaxInt);
  if (Result = '.*') or (Result = '.') then
    Result := '';
end;

procedure TOBDCustomFileDialog.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TOBDCustomFileDialog.DoFolderChange;
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self);
end;

function TOBDCustomFileDialog.Execute: Boolean;
var
  W: TOBDDialogWindow;
  C: TFileDialogController;
begin
  Result := False;
  W := NewWindow;
  try
    C := TFileDialogController.CreateController(W, Self, W);
    C.Build;
    Result := RunWindow(W) = mrOk;
  finally
    W.Free;
  end;
end;

{ descendants }

function TOBDOpenDialog.DefaultTitle: string;
begin
  Result := 'Open';
end;

function TOBDSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

function TOBDSaveDialog.AcceptCaption: string;
begin
  Result := 'Save';
end;

function TOBDSaveDialog.DefaultTitle: string;
begin
  Result := 'Save as';
end;

constructor TOBDOpenPictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  Filter := GraphicFilter(TGraphic);
end;

function TOBDOpenPictureDialog.IsPictureDialog: Boolean;
begin
  Result := True;
end;

function TOBDOpenPictureDialog.DefaultTitle: string;
begin
  Result := 'Open picture';
end;

constructor TOBDSavePictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  Filter := GraphicFilter(TGraphic);
end;

function TOBDSavePictureDialog.IsPictureDialog: Boolean;
begin
  Result := True;
end;

function TOBDSavePictureDialog.DefaultTitle: string;
begin
  Result := 'Save picture as';
end;

{ TFileDialogController }

constructor TFileDialogController.CreateController(AOwner: TComponent;
  ADialog: TOBDCustomFileDialog; AWindow: TOBDDialogWindow);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  FWindow := AWindow;
  FHistory := TStringList.Create;
  FHistoryIndex := -1;
end;

destructor TFileDialogController.Destroy;
begin
  FHistory.Free;
  inherited;
end;

function TFileDialogController.CurrentDir: string;
begin
  Result := FList.Directory;
end;

procedure TFileDialogController.AddHistory(const APath: string);
begin
  while FHistory.Count - 1 > FHistoryIndex do
    FHistory.Delete(FHistory.Count - 1);
  FHistory.Add(APath);
  FHistoryIndex := FHistory.Count - 1;
end;

procedure TFileDialogController.Build;
var
  BodyH, MainW, ListW, SearchX, EditH: Integer;
  Dir, Caption: string;
  Btn: TOBDButton;
  L: TStringList;
  I: Integer;
begin
  BodyH := 486;
  MainW := 1068;
  if FDialog.IsSaveDialog then
    BodyH := 604;
  if FDialog.IsPictureDialog then
    MainW := 1228;
  FWindow.SetLayout(MainW, BodyH);
  EditH := DensityMetrics(FWindow.Density).Edit;

  Btn := TOBDButton.Create(FWindow);
  FWindow.Adopt(Btn);
  Btn.Caption := '';
  Btn.Glyph := glBack;
  Btn.Kind := bkGhost;
  Btn.OnClick := Back;
  FWindow.Place(Btn, 16, 18, 36, 34);
  Btn := TOBDButton.Create(FWindow);
  FWindow.Adopt(Btn);
  Btn.Caption := '';
  Btn.Glyph := glRedo;
  Btn.Kind := bkGhost;
  Btn.OnClick := Forward;
  FWindow.Place(Btn, 56, 18, 36, 34);
  Btn := TOBDButton.Create(FWindow);
  FWindow.Adopt(Btn);
  Btn.Caption := '';
  Btn.Glyph := glChevronUp;
  Btn.Kind := bkGhost;
  Btn.OnClick := Up;
  FWindow.Place(Btn, 96, 18, 36, 34);

  SearchX := MainW - 242;
  if FDialog.IsPictureDialog then
    SearchX := MainW - 402;
  FBreadcrumb := TOBDBreadcrumb.Create(FWindow);
  FWindow.Adopt(FBreadcrumb);
  FBreadcrumb.OnNavigate := BreadcrumbNavigate;
  FWindow.Place(FBreadcrumb, 140, 18, SearchX - 156, 34);

  if FDialog.IsSaveDialog then
  begin
    Btn := TOBDButton.Create(FWindow);
    FWindow.Adopt(Btn);
    Btn.Caption := 'New folder';
    Btn.Glyph := glNew;
    Btn.Kind := bkGhost;
    Btn.OnClick := NewFolder;
    FWindow.Place(Btn, SearchX - 116, 18, 104, 34);
  end;

  FSearchEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FSearchEdit);
  FSearchEdit.TextHint := 'Search';
  FSearchEdit.OnChange := SearchChange;
  FWindow.Place(FSearchEdit, SearchX, 18, 226, EditH);

  FPlaces := TOBDPlacesPane.Create(FWindow);
  FWindow.Adopt(FPlaces);
  FPlaces.Places := FDialog.Places;
  FPlaces.OnNavigate := PlacesNavigate;
  FWindow.Place(FPlaces, 16, 66, 196, 274);

  ListW := MainW - 244;
  if FDialog.IsPictureDialog then
    Dec(ListW, 236);
  FList := TOBDFileList.Create(FWindow);
  FWindow.Adopt(FList);
  FList.Mask := FDialog.CurrentMask;
  FList.MultiSelect := (foAllowMultiSelect in FDialog.Options) and
    not FDialog.IsSaveDialog;
  FList.ShowHidden := foShowHidden in FDialog.Options;
  FList.OnFolderOpen := FolderOpen;
  FList.OnFileActivate := FileActivate;
  FList.OnChange := SelectionChange;
  FWindow.Place(FList, 224, 66, ListW, 274);

  if FDialog.IsPictureDialog then
  begin
    FPreview := TPicturePreview.Create(FWindow);
    FWindow.Adopt(FPreview);
    FWindow.Place(FPreview, 224 + ListW + 12, 66, 220, 274);
  end;

  FWindow.AddText(16, 358, 160, 'FILE NAME');
  FNameEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FNameEdit);
  FNameEdit.Text := ExtractFileName(FDialog.FileName);
  FNameEdit.OnReturn := NameReturn;
  FWindow.Place(FNameEdit, 16, 382, MainW - 286, EditH);

  if FDialog.IsSaveDialog then
    FWindow.AddText(MainW - 266, 358, 160, 'SAVE AS TYPE')
  else
    FWindow.AddText(MainW - 266, 358, 160, 'FILE TYPE');
  FTypeCombo := TOBDComboBox.Create(FWindow);
  FWindow.Adopt(FTypeCombo);
  L := TStringList.Create;
  try
    ExtractStrings(['|'], [], PChar(FDialog.Filter), L);
    I := 0;
    while I + 1 < L.Count do
    begin
      Caption := Format('%s (%s)', [L[I], L[I + 1]]);
      FTypeCombo.Items.Add(Caption);
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
  if FTypeCombo.Items.Count = 0 then
    FTypeCombo.Items.Add('All files (*.*)');
  FTypeCombo.ItemIndex := System.Math.Min(FTypeCombo.Items.Count - 1,
    FDialog.FilterIndex - 1);
  FTypeCombo.OnChange := FilterChange;
  FWindow.Place(FTypeCombo, MainW - 266, 382, 250, EditH);

  if not (foHideReadOnly in FDialog.Options) and not FDialog.IsSaveDialog then
    FWindow.AddLeftCheck('Open read-only', foReadOnly in FDialog.Options);
  FOK := FWindow.AddButton(FDialog.AcceptCaption, bkPrimary, mrNone);
  FOK.Default := True;
  FOK.OnClick := Accept;
  Btn := FWindow.AddButton('Cancel', bkSecondary, mrCancel);
  Btn.Cancel := True;

  Dir := StartDirectory(FDialog.InitialDir, FDialog.FileName);
  Navigate(Dir);
  FWindow.InitialFocus := FNameEdit;
end;

procedure TFileDialogController.Navigate(const APath: string; AHistory: Boolean);
var
  S: string;
begin
  S := ExpandFileName(APath);
  if not DirectoryExists(S) then
    Exit;
  FList.Directory := S;
  FBreadcrumb.Path := S;
  FPlaces.Path := S;
  FSearchEdit.Text := '';
  if AHistory then
    AddHistory(S);
  FDialog.DoFolderChange;
end;

procedure TFileDialogController.PlacesNavigate(Sender: TObject;
  const APath: string);
begin
  Navigate(APath);
end;

procedure TFileDialogController.BreadcrumbNavigate(Sender: TObject;
  const APath: string);
begin
  Navigate(APath);
end;

procedure TFileDialogController.FolderOpen(Sender: TObject; const APath: string);
begin
  Navigate(APath);
end;

procedure TFileDialogController.FileActivate(Sender: TObject; const APath: string);
begin
  FNameEdit.Text := ExtractFileName(APath);
  Accept(Sender);
end;

procedure TFileDialogController.SelectionChange(Sender: TObject);
var
  I: Integer;
begin
  I := FList.ItemIndex;
  if (I >= 0) and (I < FList.FileCount) then
  begin
    if not FList.Entries[I].IsFolder then
      FNameEdit.Text := FList.Entries[I].Name;
    if FPreview <> nil then
      FPreview.FileName := FList.Entries[I].FullPath;
  end;
  FDialog.DoSelectionChange;
end;

procedure TFileDialogController.SearchChange(Sender: TObject);
begin
  FList.SearchText := FSearchEdit.Text;
end;

procedure TFileDialogController.FilterChange(Sender: TObject);
begin
  FDialog.FilterIndex := FTypeCombo.ItemIndex + 1;
  FList.Mask := FDialog.CurrentMask;
end;

function TFileDialogController.TypedPath: string;
var
  S: string;
begin
  S := Trim(FNameEdit.Text);
  if S = '' then
    Result := ''
  else if ExtractFileDrive(S) <> '' then
    Result := ExpandFileName(S)
  else
    Result := IncludeTrailingPathDelimiter(CurrentDir) + S;
end;

function TFileDialogController.SelectedFilePath: string;
var
  I: Integer;
begin
  Result := TypedPath;
  I := FList.ItemIndex;
  if (Result = '') and (I >= 0) and (I < FList.FileCount) then
    Result := FList.Entries[I].FullPath;
end;

procedure TFileDialogController.SetBanner(const ATitle, AText: string;
  AKind: TOBDBannerKind);
var
  MainW: Integer;
  B: TOBDButton;
begin
  MainW := MulDiv(FWindow.ClientWidth, 96, FWindow.PPI);
  if FBanner = nil then
  begin
    FBanner := TOBDBanner.Create(FWindow);
    FWindow.Adopt(FBanner);
    FWindow.Place(FBanner, 16, 430, MainW - 32, 64);
  end;
  FBanner.Kind := AKind;
  FBanner.Title := ATitle;
  FBanner.Text := AText;
  FBanner.Visible := True;
  if FDialog.IsSaveDialog and (AKind = bnWarning) then
  begin
    B := TOBDButton.Create(FWindow);
    FWindow.Adopt(B, FBanner);
    B.Caption := 'Change name';
    B.Kind := bkGhost;
    B.OnClick := ChangeName;
    B.SetBounds(FWindow.Dp(18), FWindow.Dp(22), FWindow.Dp(110),
      FWindow.Dp(34));
    B := TOBDButton.Create(FWindow);
    FWindow.Adopt(B, FBanner);
    B.Caption := 'Replace';
    B.Kind := bkDanger;
    B.OnClick := Replace;
    B.SetBounds(FBanner.Width - FWindow.Dp(110), FWindow.Dp(16),
      FWindow.Dp(94), FWindow.Dp(40));
  end;
end;

procedure TFileDialogController.NameReturn(Sender: TObject);
begin
  Accept(Sender);
end;

procedure TFileDialogController.Accept(Sender: TObject);
var
  P, Ext: string;
  I: Integer;
begin
  P := SelectedFilePath;
  if P = '' then
    Exit;
  if DirectoryExists(P) then
  begin
    Navigate(P);
    Exit;
  end;
  if FDialog.IsSaveDialog then
  begin
    Ext := ExtractFileExt(P);
    if Ext = '' then
    begin
      Ext := FDialog.DefaultExt;
      if Ext = '' then
        Ext := FDialog.FilterExtension;
      if (Ext <> '') and (Ext[1] <> '.') then
        Ext := '.' + Ext;
      P := P + Ext;
    end;
    if FileExists(P) and (foOverwritePrompt in FDialog.Options) and
      not FReplacing then
    begin
      SetBanner(ExtractFileName(P) + ' already exists',
        'Replace it? The existing file is overwritten.', bnWarning);
      FOK.Caption := 'Replace';
      FOK.Kind := bkDanger;
      Exit;
    end;
  end
  else if (foFileMustExist in FDialog.Options) and not FileExists(P) then
  begin
    SetBanner('File not found', 'Choose an existing file to continue.',
      bnDanger);
    Exit;
  end;
  FDialog.FFileName := P;
  FDialog.FFiles.Clear;
  if FList.MultiSelect and not FDialog.IsSaveDialog then
    FList.SelectedPaths(FDialog.FFiles);
  if FDialog.FFiles.Count = 0 then
    FDialog.FFiles.Add(P);
  for I := FDialog.FFiles.Count - 1 downto 0 do
    if FDialog.FFiles[I] = '' then
      FDialog.FFiles.Delete(I);
  FWindow.ModalResult := mrOk;
end;

procedure TFileDialogController.Back(Sender: TObject);
begin
  if FHistoryIndex > 0 then
  begin
    Dec(FHistoryIndex);
    Navigate(FHistory[FHistoryIndex], False);
  end;
end;

procedure TFileDialogController.Forward(Sender: TObject);
begin
  if FHistoryIndex < FHistory.Count - 1 then
  begin
    Inc(FHistoryIndex);
    Navigate(FHistory[FHistoryIndex], False);
  end;
end;

procedure TFileDialogController.Up(Sender: TObject);
var
  P: string;
begin
  P := ExtractFileDir(ExcludeTrailingPathDelimiter(CurrentDir));
  if (P <> '') and DirectoryExists(P) then
    Navigate(P);
end;

procedure TFileDialogController.NewFolder(Sender: TObject);
var
  Base, P: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(CurrentDir) + 'New folder';
  P := Base;
  I := 2;
  while DirectoryExists(P) do
  begin
    P := Format('%s (%d)', [Base, I]);
    Inc(I);
  end;
  ForceDirectories(P);
  FList.Refresh;
  FList.SelectByName(ExtractFileName(P));
end;

procedure TFileDialogController.ChangeName(Sender: TObject);
begin
  FReplacing := False;
  FOK.Caption := FDialog.AcceptCaption;
  FOK.Kind := bkPrimary;
  if FBanner <> nil then
    FBanner.Visible := False;
  FNameEdit.SetFocus;
  FNameEdit.SelectAll;
end;

procedure TFileDialogController.Replace(Sender: TObject);
begin
  FReplacing := True;
  Accept(Sender);
end;

{ TFolderController }

constructor TFolderController.CreateController(AOwner: TComponent;
  ADialog: TOBDFolderDialog; AWindow: TOBDDialogWindow);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  FWindow := AWindow;
end;

procedure TFolderController.Build;
var
  Btn: TOBDButton;
  Dir: string;
begin
  FWindow.SetLayout(420, 430);
  FTree := TOBDFolderTree.Create(FWindow);
  FWindow.Adopt(FTree);
  FTree.ShowHidden := foShowHidden in FDialog.Options;
  FTree.OnPathChange := TreeChange;
  FWindow.Place(FTree, 16, 16, 388, 300);
  FWindow.AddText(16, 330, 160, 'FOLDER');
  FEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FEdit);
  FEdit.OnReturn := EditReturn;
  FWindow.Place(FEdit, 16, 354, 388, DensityMetrics(FWindow.Density).Edit);
  Btn := FWindow.AddLeftButton('New folder', glNew);
  Btn.OnClick := NewFolder;
  Btn := FWindow.AddButton('Select folder', bkPrimary, mrNone);
  Btn.Default := True;
  Btn.OnClick := Accept;
  Btn := FWindow.AddButton('Cancel', bkSecondary, mrCancel);
  Btn.Cancel := True;
  Dir := StartDirectory(FDialog.InitialDir, FDialog.Directory);
  FTree.Path := Dir;
  FEdit.Text := Dir;
  FWindow.InitialFocus := FTree;
end;

procedure TFolderController.TreeChange(Sender: TObject; const APath: string);
begin
  FEdit.Text := APath;
end;

procedure TFolderController.EditReturn(Sender: TObject);
begin
  if DirectoryExists(FEdit.Text) then
    FTree.Path := FEdit.Text;
end;

procedure TFolderController.NewFolder(Sender: TObject);
var
  Base, P: string;
  I: Integer;
begin
  Base := IncludeTrailingPathDelimiter(FEdit.Text) + 'New folder';
  P := Base;
  I := 2;
  while DirectoryExists(P) do
  begin
    P := Format('%s (%d)', [Base, I]);
    Inc(I);
  end;
  ForceDirectories(P);
  FTree.Path := P;
  FEdit.Text := P;
end;

procedure TFolderController.Accept(Sender: TObject);
begin
  if DirectoryExists(FEdit.Text) then
  begin
    FDialog.FDirectory := ExpandFileName(FEdit.Text);
    FWindow.ModalResult := mrOk;
  end;
end;

{ TOBDFolderDialog }

function TOBDFolderDialog.DefaultTitle: string;
begin
  Result := 'Select folder';
end;

function TOBDFolderDialog.Execute: Boolean;
var
  W: TOBDDialogWindow;
  C: TFolderController;
begin
  Result := False;
  W := NewWindow;
  try
    C := TFolderController.CreateController(W, Self, W);
    C.Build;
    Result := RunWindow(W) = mrOk;
  finally
    W.Free;
  end;
end;

end.
