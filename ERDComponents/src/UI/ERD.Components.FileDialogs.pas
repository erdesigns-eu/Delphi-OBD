//------------------------------------------------------------------------------
//  ERD.Components.FileDialogs
//
//  Themed file and folder dialog components for Application.
//
//    TERDOpenDialog, TERDSaveDialog        themed file dialogs.
//    TERDOpenPictureDialog,                picture variants with preview.
//    TERDSavePictureDialog
//    TERDFolderDialog                      themed folder picker.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed dialogs.
//------------------------------------------------------------------------------

unit ERD.Components.FileDialogs;

interface

uses
  ERD.Components.Types,
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
  ERD.Components.Theme,
  ERD.Components.Control,
  ERD.Components.Paint,
  ERD.Components.Buttons,
  ERD.Components.Edits,
  ERD.Components.Chips,
  ERD.Components.Dialogs,
  ERD.Components.ShellControls;

type
  /// <summary>File dialog option.</summary>
  TERDFileOption = (foReadOnly, foOverwritePrompt, foHideReadOnly,
    foNoChangeDir, foAllowMultiSelect, foPathMustExist, foFileMustExist,
    foCreatePrompt, foShowHidden);

  /// <summary>File dialog option set.</summary>
  TERDFileOptions = set of TERDFileOption;

  /// <summary>Base class shared by themed file dialogs.</summary>
  TERDCustomFileDialog = class(TERDCommonDialog)
  private
    FFileName: string;
    FFiles: TStringList;
    FFilter: string;
    FFilterIndex: Integer;
    FInitialDir: string;
    FDefaultExt: string;
    FOptions: TERDFileOptions;
    FPlaces: TERDPlaces;
    FSystemIcons: Boolean;
    FOnSelectionChange: TNotifyEvent;
    FOnFolderChange: TNotifyEvent;
    procedure SetFilterIndex(AValue: Integer);
    procedure SetPlaces(AValue: TERDPlaces);
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
    property Options: TERDFileOptions read FOptions write FOptions;
    /// <summary>Application places.</summary>
    property Places: TERDPlaces read FPlaces write SetPlaces;
    /// <summary>Selection change event.</summary>
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange
      write FOnSelectionChange;
    /// <summary>Folder change event.</summary>
    property OnFolderChange: TNotifyEvent read FOnFolderChange
      write FOnFolderChange;
    /// <summary>Lets the user resize the dialog; the list grows with it.
    /// </summary>
    property Resizable default True;
    /// <summary>Shows the Windows shell icons for folders, drives and
    /// file types instead of the themed line glyphs.</summary>
    property SystemIcons: Boolean read FSystemIcons write FSystemIcons
      default True;
  end;

  /// <summary>Themed open-file dialog.</summary>
  TERDOpenDialog = class(TERDCustomFileDialog)
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
  TERDSaveDialog = class(TERDCustomFileDialog)
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
  TERDOpenPictureDialog = class(TERDOpenDialog)
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
  TERDSavePictureDialog = class(TERDSaveDialog)
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
  TERDFolderDialog = class(TERDCommonDialog)
  private
    FDirectory: string;
    FInitialDir: string;
    FOptions: TERDFileOptions;
    FSystemIcons: Boolean;
  protected
    /// <summary>Default title.</summary>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates the dialog.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Shows the dialog.</summary>
    function Execute: Boolean; override;
  published
    /// <summary>Selected directory.</summary>
    property Directory: string read FDirectory write FDirectory;
    /// <summary>Initial directory.</summary>
    property InitialDir: string read FInitialDir write FInitialDir;
    /// <summary>Options.</summary>
    property Options: TERDFileOptions read FOptions write FOptions;
    /// <summary>Lets the user resize the dialog; the tree grows with it.
    /// </summary>
    property Resizable default True;
    /// <summary>Shows the Windows shell icons for folders, drives and
    /// file types instead of the themed line glyphs.</summary>
    property SystemIcons: Boolean read FSystemIcons write FSystemIcons
      default True;
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
  TReturnEdit = class(TERDEdit)
  strict private
    FOnReturn: TNotifyEvent;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property OnReturn: TNotifyEvent read FOnReturn write FOnReturn;
  end;

  TPicturePreview = class(TERDCustomControl)
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
    FDialog: TERDCustomFileDialog;
    FWindow: TERDDialogWindow;
    FPlaces: TERDPlacesPane;
    FBreadcrumb: TERDBreadcrumb;
    FList: TERDFileList;
    FNameEdit: TReturnEdit;
    FSearchEdit: TReturnEdit;
    FTypeCombo: TERDComboBox;
    FBanner: TERDBanner;
    FBannerChange: TERDButton;
    FBannerReplace: TERDButton;
    FPreview: TPicturePreview;
    FOK: TERDButton;
    FBackButton: TERDButton;
    FForwardButton: TERDButton;
    FUpButton: TERDButton;
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FReplacing: Boolean;
    procedure AddHistory(const APath: string);
    function CurrentDir: string;
    function TypedPath: string;
    function SelectedFilePath: string;
    procedure SetBanner(const ATitle, AText: string; AKind: TERDBannerKind);
    procedure HideBanner;
    procedure UpdateNavButtons;
    function NavButton(AGlyph: TERDGlyph; AClick: TNotifyEvent; X,
      H: Integer): TERDButton;
  public
    constructor CreateController(AOwner: TComponent;
      ADialog: TERDCustomFileDialog; AWindow: TERDDialogWindow);
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
    FDialog: TERDFolderDialog;
    FWindow: TERDDialogWindow;
    FTree: TERDFolderTree;
    FEdit: TReturnEdit;
  public
    constructor CreateController(AOwner: TComponent; ADialog: TERDFolderDialog;
      AWindow: TERDDialogWindow);
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
  P: TERDPainter;
  R: TRect;
  Scale: Double;
  W, H: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
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
      P.Text(Width div 2, Height - ScaleValue(22), FInfo, ERD_TEXT_SMALL,
        Palette.GaugeLabel, twRegular, taCenter, Width - ScaleValue(16));
    end
    else
      P.Text(Width div 2, Height div 2, 'No preview', ERD_TEXT_BODY,
        Palette.GaugeLabel, twRegular, taCenter, Width - ScaleValue(16));
  finally
    P.Free;
  end;
end;

{ TERDCustomFileDialog }

constructor TERDCustomFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FPlaces := TERDPlaces.Create(Self);
  FFilter := 'All files|*.*';
  FFilterIndex := 1;
  FOptions := [foFileMustExist, foPathMustExist];
  FSystemIcons := True;
  Resizable := True;
end;

destructor TERDCustomFileDialog.Destroy;
begin
  FPlaces.Free;
  FFiles.Free;
  inherited;
end;

procedure TERDCustomFileDialog.SetFilterIndex(AValue: Integer);
begin
  FFilterIndex := System.Math.Max(1, AValue);
end;

procedure TERDCustomFileDialog.SetPlaces(AValue: TERDPlaces);
begin
  if AValue <> nil then
    FPlaces.Assign(AValue)
  else
    FPlaces.Clear;
end;

function TERDCustomFileDialog.IsSaveDialog: Boolean;
begin
  Result := False;
end;

function TERDCustomFileDialog.IsPictureDialog: Boolean;
begin
  Result := False;
end;

function TERDCustomFileDialog.AcceptCaption: string;
begin
  Result := 'Open';
end;

function TERDCustomFileDialog.DefaultTitle: string;
begin
  Result := 'Open';
end;

function TERDCustomFileDialog.CurrentMask: string;
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

function TERDCustomFileDialog.FilterExtension: string;
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

procedure TERDCustomFileDialog.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TERDCustomFileDialog.DoFolderChange;
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self);
end;

function TERDCustomFileDialog.Execute: Boolean;
var
  W: TERDDialogWindow;
  C: TFileDialogController;
begin
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

function TERDOpenDialog.DefaultTitle: string;
begin
  Result := 'Open';
end;

function TERDSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

function TERDSaveDialog.AcceptCaption: string;
begin
  Result := 'Save';
end;

function TERDSaveDialog.DefaultTitle: string;
begin
  Result := 'Save as';
end;

constructor TERDOpenPictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  Filter := GraphicFilter(TGraphic);
end;

function TERDOpenPictureDialog.IsPictureDialog: Boolean;
begin
  Result := True;
end;

function TERDOpenPictureDialog.DefaultTitle: string;
begin
  Result := 'Open picture';
end;

constructor TERDSavePictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  Filter := GraphicFilter(TGraphic);
end;

function TERDSavePictureDialog.IsPictureDialog: Boolean;
begin
  Result := True;
end;

function TERDSavePictureDialog.DefaultTitle: string;
begin
  Result := 'Save picture as';
end;

{ TFileDialogController }

constructor TFileDialogController.CreateController(AOwner: TComponent;
  ADialog: TERDCustomFileDialog; AWindow: TERDDialogWindow);
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

function ParentFolder(const ADir: string): string;
var
  S: string;
begin
  S := ExcludeTrailingPathDelimiter(ADir);
  Result := ExtractFileDir(S);
  if (Length(Result) = 2) and (Result[2] = ':') then
    Result := Result + PathDelim;
  if (Result = '') or SameText(IncludeTrailingPathDelimiter(Result),
    IncludeTrailingPathDelimiter(ADir)) then
    Result := '';
end;

function TFileDialogController.NavButton(AGlyph: TERDGlyph;
  AClick: TNotifyEvent; X, H: Integer): TERDButton;
begin
  Result := TERDButton.Create(FWindow);
  FWindow.Adopt(Result);
  Result.Caption := '';
  Result.Glyph := AGlyph;
  Result.Kind := bkGhost;
  Result.OnClick := AClick;
  FWindow.Place(Result, X, 18, 34, H);
end;

procedure TFileDialogController.Build;
const
  LIST_H = 330;
var
  BodyH, MainW, ListW, SearchX, EditH, NavH, ListY, NameY, NewW: Integer;
  Dir, Caption: string;
  Btn, NewButton: TERDButton;
  NameLabel, TypeLabel: TERDDialogText;
  L: TStringList;
  I: Integer;
begin
  MainW := 1068;
  if FDialog.IsPictureDialog then
    MainW := 1228;
  EditH := DensityMetrics(FWindow.Density).Edit;
  NavH := System.Math.Max(34, EditH);
  ListY := 18 + NavH + 14;
  NameY := ListY + LIST_H + 18;
  BodyH := NameY + 24 + EditH + 16;
  FWindow.SetLayout(MainW, BodyH);

  FBackButton := NavButton(glBack, Back, 16, NavH);
  FForwardButton := NavButton(glForward, Forward, 52, NavH);
  FUpButton := NavButton(glUp, Up, 88, NavH);

  SearchX := MainW - 242;
  if FDialog.IsPictureDialog then
    SearchX := MainW - 402;
  FBreadcrumb := TERDBreadcrumb.Create(FWindow);
  FWindow.Adopt(FBreadcrumb);
  FBreadcrumb.OnNavigate := BreadcrumbNavigate;
  NewButton := nil;
  NewW := 0;
  if FDialog.IsSaveDialog then
  begin
    NewW := MulDiv(FWindow.ButtonWidth('New folder', glNew), 96, FWindow.PPI);
    NewButton := TERDButton.Create(FWindow);
    FWindow.Adopt(NewButton);
    NewButton.Caption := 'New folder';
    NewButton.Glyph := glNew;
    NewButton.Kind := bkGhost;
    NewButton.OnClick := NewFolder;
    FWindow.Place(NewButton, SearchX - 8 - NewW, 18, NewW, NavH);
  end;
  if NewW > 0 then
    FWindow.Place(FBreadcrumb, 132, 18, SearchX - 148 - NewW, NavH)
  else
    FWindow.Place(FBreadcrumb, 132, 18, SearchX - 144, NavH);

  FSearchEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FSearchEdit);
  FSearchEdit.TextHint := 'Search';
  FSearchEdit.OnChange := SearchChange;
  FWindow.Place(FSearchEdit, SearchX, 18 + (NavH - EditH) div 2, MainW - 16 -
    SearchX, EditH);

  FPlaces := TERDPlacesPane.Create(FWindow);
  FWindow.Adopt(FPlaces);
  FPlaces.Places := FDialog.Places;
  FPlaces.SystemIcons := FDialog.SystemIcons;
  FPlaces.OnNavigate := PlacesNavigate;
  FWindow.Place(FPlaces, 16, ListY, 196, LIST_H);

  ListW := MainW - 240;
  if FDialog.IsPictureDialog then
    Dec(ListW, 232);
  FList := TERDFileList.Create(FWindow);
  FWindow.Adopt(FList);
  FList.SystemIcons := FDialog.SystemIcons;
  FList.Mask := FDialog.CurrentMask;
  FList.MultiSelect := (foAllowMultiSelect in FDialog.Options) and
    not FDialog.IsSaveDialog;
  FList.ShowHidden := foShowHidden in FDialog.Options;
  FList.OnFolderOpen := FolderOpen;
  FList.OnFileActivate := FileActivate;
  FList.OnChange := SelectionChange;
  FWindow.Place(FList, 224, ListY, ListW, LIST_H);

  if FDialog.IsPictureDialog then
  begin
    FPreview := TPicturePreview.Create(FWindow);
    FWindow.Adopt(FPreview);
    FWindow.Place(FPreview, 224 + ListW + 12, ListY, MainW - 16 -
      (224 + ListW + 12), LIST_H);
  end;

  NameLabel := FWindow.AddText(16, NameY, 160, 'FILE NAME');
  FNameEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FNameEdit);
  FNameEdit.Text := ExtractFileName(FDialog.FileName);
  FNameEdit.OnReturn := NameReturn;
  FWindow.Place(FNameEdit, 16, NameY + 24, MainW - 282, EditH);

  if FDialog.IsSaveDialog then
    TypeLabel := FWindow.AddText(MainW - 266, NameY, 160, 'SAVE AS TYPE')
  else
    TypeLabel := FWindow.AddText(MainW - 266, NameY, 160, 'FILE TYPE');
  FTypeCombo := TERDComboBox.Create(FWindow);
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
  FWindow.Place(FTypeCombo, MainW - 266, NameY + 24, 250, EditH);

  if not (foHideReadOnly in FDialog.Options) and not FDialog.IsSaveDialog then
    FWindow.AddLeftCheck('Open read-only', foReadOnly in FDialog.Options);
  FOK := FWindow.AddButton(FDialog.AcceptCaption, bkPrimary, mrNone);
  FOK.Default := True;
  FOK.OnClick := Accept;
  Btn := FWindow.AddButton('Cancel', bkSecondary, mrCancel);
  Btn.Cancel := True;

  // Anchors are set once the layout is final so a resize stretches the
  // list area and keeps the name row and the buttons at the edges.
  FBreadcrumb.Anchors := [akLeft, akTop, akRight];
  FSearchEdit.Anchors := [akTop, akRight];
  FPlaces.Anchors := [akLeft, akTop, akBottom];
  FList.Anchors := [akLeft, akTop, akRight, akBottom];
  if FPreview <> nil then
    FPreview.Anchors := [akTop, akRight, akBottom];
  NameLabel.Anchors := [akLeft, akBottom];
  FNameEdit.Anchors := [akLeft, akRight, akBottom];
  TypeLabel.Anchors := [akRight, akBottom];
  FTypeCombo.Anchors := [akRight, akBottom];
  if NewButton <> nil then
    NewButton.Anchors := [akTop, akRight];

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
  HideBanner;
  UpdateNavButtons;
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
  AKind: TERDBannerKind);
var
  Shrink, Right, BtnH, W: Integer;
begin
  if FBanner = nil then
  begin
    FBanner := TERDBanner.Create(FWindow);
    FWindow.Adopt(FBanner);
    FBanner.Visible := False;
    BtnH := FWindow.Dp(DensityMetrics(FWindow.Density).Button) +
      2 * FWindow.Dp(4);
    FBannerReplace := TERDButton.Create(FWindow);
    FWindow.Adopt(FBannerReplace, FBanner);
    FBannerReplace.Caption := 'Replace';
    FBannerReplace.Kind := bkDanger;
    FBannerReplace.AutoSize := False;
    FBannerReplace.OnClick := Replace;
    FBannerChange := TERDButton.Create(FWindow);
    FWindow.Adopt(FBannerChange, FBanner);
    FBannerChange.Caption := 'Change name';
    FBannerChange.Kind := bkGhost;
    FBannerChange.AutoSize := False;
    FBannerChange.OnClick := ChangeName;
    FBanner.Height := FWindow.Dp(64);
    W := FWindow.ButtonWidth('Replace');
    FBannerReplace.SetBounds(0, (FBanner.Height - BtnH) div 2, W, BtnH);
    W := FWindow.ButtonWidth('Change name');
    FBannerChange.SetBounds(0, (FBanner.Height - BtnH) div 2, W, BtnH);
  end;
  if not FBanner.Visible then
  begin
    // The banner takes the bottom of the list area; the panes above
    // give up the room and get it back in HideBanner.
    Shrink := FWindow.Dp(64) + FWindow.Dp(8);
    FPlaces.Height := FPlaces.Height - Shrink;
    FList.Height := FList.Height - Shrink;
    if FPreview <> nil then
      FPreview.Height := FPreview.Height - Shrink;
    Right := FList.Left + FList.Width;
    if FPreview <> nil then
      Right := FPreview.Left + FPreview.Width;
    FBanner.Anchors := [akLeft, akTop];
    FBanner.SetBounds(FPlaces.Left, FList.Top + FList.Height + FWindow.Dp(8),
      Right - FPlaces.Left, FWindow.Dp(64));
    FBanner.Anchors := [akLeft, akRight, akBottom];
  end;
  FBanner.Kind := AKind;
  FBanner.Title := ATitle;
  FBanner.Text := AText;
  FBannerReplace.Visible := FDialog.IsSaveDialog and (AKind = bnWarning);
  FBannerChange.Visible := FBannerReplace.Visible;
  FBannerReplace.Anchors := [akLeft, akTop];
  FBannerChange.Anchors := [akLeft, akTop];
  FBannerReplace.Left := FBanner.Width - FWindow.Dp(12) + FWindow.Dp(4) -
    FBannerReplace.Width;
  FBannerChange.Left := FBannerReplace.Left + 2 * FWindow.Dp(4) -
    FWindow.Dp(8) - FBannerChange.Width;
  FBannerReplace.Anchors := [akTop, akRight];
  FBannerChange.Anchors := [akTop, akRight];
  FBanner.Visible := True;
end;

procedure TFileDialogController.HideBanner;
var
  Grow: Integer;
begin
  if FReplacing or (FOK.Kind = bkDanger) then
  begin
    FReplacing := False;
    FOK.Caption := FDialog.AcceptCaption;
    FOK.Kind := bkPrimary;
  end;
  if (FBanner = nil) or not FBanner.Visible then
    Exit;
  FBanner.Visible := False;
  Grow := FWindow.Dp(64) + FWindow.Dp(8);
  FPlaces.Height := FPlaces.Height + Grow;
  FList.Height := FList.Height + Grow;
  if FPreview <> nil then
    FPreview.Height := FPreview.Height + Grow;
end;

procedure TFileDialogController.UpdateNavButtons;
begin
  FBackButton.Enabled := FHistoryIndex > 0;
  FForwardButton.Enabled := FHistoryIndex < FHistory.Count - 1;
  FUpButton.Enabled := ParentFolder(CurrentDir) <> '';
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
  P := ParentFolder(CurrentDir);
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
  HideBanner;
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
  ADialog: TERDFolderDialog; AWindow: TERDDialogWindow);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  FWindow := AWindow;
end;

procedure TFolderController.Build;
var
  Btn: TERDButton;
  FolderLabel: TERDDialogText;
  Dir: string;
  EditH: Integer;
begin
  EditH := DensityMetrics(FWindow.Density).Edit;
  FWindow.SetLayout(420, 16 + 300 + 14 + 24 + EditH + 16);
  FTree := TERDFolderTree.Create(FWindow);
  FWindow.Adopt(FTree);
  FTree.ShowHidden := foShowHidden in FDialog.Options;
  FTree.SystemIcons := FDialog.SystemIcons;
  FTree.OnPathChange := TreeChange;
  FWindow.Place(FTree, 16, 16, 388, 300);
  FolderLabel := FWindow.AddText(16, 330, 160, 'FOLDER');
  FEdit := TReturnEdit.Create(FWindow);
  FWindow.Adopt(FEdit);
  FEdit.OnReturn := EditReturn;
  FWindow.Place(FEdit, 16, 354, 388, EditH);
  FTree.Anchors := [akLeft, akTop, akRight, akBottom];
  FolderLabel.Anchors := [akLeft, akBottom];
  FEdit.Anchors := [akLeft, akRight, akBottom];
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

{ TERDFolderDialog }

constructor TERDFolderDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSystemIcons := True;
  Resizable := True;
end;

function TERDFolderDialog.DefaultTitle: string;
begin
  Result := 'Select folder';
end;

function TERDFolderDialog.Execute: Boolean;
var
  W: TERDDialogWindow;
  C: TFolderController;
begin
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
