//------------------------------------------------------------------------------
//  ERD.UI.ShellControls
//
//  Reusable themed shell building blocks for OBD Studio dialogs.
//
//    TOBDBreadcrumb  self-painted path bar with chevron segments.
//    TOBDPlacesPane  quick access, application places and drive list.
//    TOBDFolderTree  lazily-expanded directory tree.
//    TOBDFileList    details / icons directory browser.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed file dialogs.
//------------------------------------------------------------------------------

unit ERD.UI.ShellControls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShlObj,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.StrUtils,
  System.Masks,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.ItemList,
  ERD.UI.Paint;

type
  /// <summary>Event carrying a filesystem path.</summary>
  TOBDPathEvent = procedure(Sender: TObject; const APath: string) of object;

  /// <summary>Reusable shell glyph kinds.</summary>
  TOBDShellGlyphKind = (sgFolder, sgDrive, sgFile, sgImage, sgDesktop,
    sgDownload, sgDocument, sgUp, sgForward, sgList, sgGrid, sgNewFolder);

  /// <summary>Application place item.</summary>
  TOBDPlace = class(TCollectionItem)
  strict private
    FCaption: string;
    FPath: string;
    procedure SetCaption(const AValue: string);
    procedure SetPath(const AValue: string);
  public
    /// <summary>Returns the caption shown in lists.</summary>
    function DisplayName: string; override;
  published
    /// <summary>Displayed name.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Folder path.</summary>
    property Path: string read FPath write SetPath;
  end;

  /// <summary>Application places collection.</summary>
  TOBDPlaces = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(AIndex: Integer): TOBDPlace;
    procedure SetItem(AIndex: Integer; AValue: TOBDPlace);
  protected
    /// <summary>Fires the change event.</summary>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates the collection.</summary>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a place.</summary>
    function Add: TOBDPlace;
    /// <summary>Indexed place.</summary>
    property Items[AIndex: Integer]: TOBDPlace read GetItem write SetItem;
      default;
    /// <summary>Fires when the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Clickable breadcrumb path bar.</summary>
  TOBDBreadcrumb = class(TOBDCustomControl)
  strict private
    FPath: string;
    FHotIndex: Integer;
    FOnNavigate: TOBDPathEvent;
    procedure SetPath(const AValue: string);
    procedure SetHotIndex(AValue: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    /// <summary>Paints the path.</summary>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Tracks hover.</summary>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Clicks a segment.</summary>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
  public
    /// <summary>Creates the control.</summary>
    constructor Create(AOwner: TComponent); override;
  published
    /// <summary>Current path.</summary>
    property Path: string read FPath write SetPath;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Navigation event.</summary>
    property OnNavigate: TOBDPathEvent read FOnNavigate write FOnNavigate;
  end;

  /// <summary>Places pane for file dialogs.</summary>
  TOBDPlacesPane = class(TOBDCustomItemList)
  strict private
    type
      TRowKind = (rkHeader, rkFolder, rkDrive, rkDesktop, rkDownload,
        rkDocument, rkImage, rkApp);
      TRow = record
        Kind: TRowKind;
        Caption: string;
        Path: string;
      end;
  strict private
    FPlaces: TOBDPlaces;
    FPath: string;
    FRows: TArray<TRow>;
    FRowsValid: Boolean;
    FChanging: Boolean;
    FOnNavigate: TOBDPathEvent;
    procedure SetPlaces(AValue: TOBDPlaces);
    procedure SetPath(const AValue: string);
    procedure BuildRows;
    procedure PlacesChanged(Sender: TObject);
    function RowIsHeader(AIndex: Integer): Boolean;
  protected
    /// <summary>Row count.</summary>
    function GetItemCount: Integer; override;
    /// <summary>Row text.</summary>
    function GetItemText(AIndex: Integer): string; override;
    /// <summary>Sidebar fill.</summary>
    function ListBackground: TColor; override;
    /// <summary>Paints one row.</summary>
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
    /// <summary>Skips headers.</summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Navigates.</summary>
    procedure Change; override;
  public
    /// <summary>Creates the pane.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned places.</summary>
    destructor Destroy; override;
    /// <summary>Rebuilds rows.</summary>
    procedure Refresh; reintroduce;
  published
    /// <summary>Application places.</summary>
    property Places: TOBDPlaces read FPlaces write SetPlaces;
    /// <summary>Selected path.</summary>
    property Path: string read FPath write SetPath;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Navigation event.</summary>
    property OnNavigate: TOBDPathEvent read FOnNavigate write FOnNavigate;
  end;

  /// <summary>Lazily expanded folder tree.</summary>
  TOBDFolderTree = class(TOBDCustomItemList)
  strict private
    type
      TNode = record
        Caption: string;
        Path: string;
        Level: Integer;
        Expanded: Boolean;
        HasChildren: Boolean;
      end;
  strict private
    FRootPath: string;
    FPath: string;
    FShowHidden: Boolean;
    FExpanded: TStringList;
    FRows: TArray<TNode>;
    FRowsValid: Boolean;
    FChanging: Boolean;
    FOnPathChange: TOBDPathEvent;
    procedure SetRootPath(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure SetShowHidden(AValue: Boolean);
    procedure BuildRows;
    procedure Toggle(AIndex: Integer);
    function ChevronRect(AIndex: Integer): TRect;
  protected
    /// <summary>Node count.</summary>
    function GetItemCount: Integer; override;
    /// <summary>Node caption.</summary>
    function GetItemText(AIndex: Integer): string; override;
    /// <summary>Paints one node.</summary>
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
    /// <summary>Handles chevrons.</summary>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    /// <summary>Keyboard expand/collapse.</summary>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary>Updates path.</summary>
    procedure Change; override;
  public
    /// <summary>Creates the tree.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees expansion state.</summary>
    destructor Destroy; override;
    /// <summary>Rebuilds rows.</summary>
    procedure Refresh; reintroduce;
  published
    /// <summary>Root path or empty for drives.</summary>
    property RootPath: string read FRootPath write SetRootPath;
    /// <summary>Selected path.</summary>
    property Path: string read FPath write SetPath;
    /// <summary>Show hidden and system folders.</summary>
    property ShowHidden: Boolean read FShowHidden write SetShowHidden
      default False;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Path change event.</summary>
    property OnPathChange: TOBDPathEvent read FOnPathChange
      write FOnPathChange;
    /// <summary>Selection change.</summary>
    property OnChange;
  end;

  /// <summary>One file-list entry.</summary>
  TOBDFileEntry = record
    Name: string;
    FullPath: string;
    IsFolder: Boolean;
    Size: Int64;
    Modified: TDateTime;
    TypeName: string;
  end;

  /// <summary>File list view style.</summary>
  TOBDFileViewStyle = (fvDetails, fvIcons);

  /// <summary>Directory file list.</summary>
  TOBDFileList = class(TOBDCustomItemList)
  strict private
    FDirectory: string;
    FMask: string;
    FSearchText: string;
    FShowFolders: Boolean;
    FShowHidden: Boolean;
    FViewStyle: TOBDFileViewStyle;
    FSortColumn: Integer;
    FSortAscending: Boolean;
    FTileWidth: Integer;
    FEntries: TArray<TOBDFileEntry>;
    FOnFolderOpen: TOBDPathEvent;
    FOnFileActivate: TOBDPathEvent;
    procedure SetDirectory(const AValue: string);
    procedure SetMask(const AValue: string);
    procedure SetSearchText(const AValue: string);
    procedure SetShowFolders(AValue: Boolean);
    procedure SetShowHidden(AValue: Boolean);
    procedure SetViewStyle(AValue: TOBDFileViewStyle);
    procedure SetTileWidth(AValue: Integer);
    function GetFileCount: Integer;
    function GetEntry(AIndex: Integer): TOBDFileEntry;
    function ColumnAt(X: Integer): Integer;
    procedure SortEntries;
  protected
    /// <summary>Entry count.</summary>
    function GetItemCount: Integer; override;
    /// <summary>Entry name.</summary>
    function GetItemText(AIndex: Integer): string; override;
    /// <summary>Row or tile height.</summary>
    function ItemHeight: Integer; override;
    /// <summary>Tile width.</summary>
    function TileWidth: Integer; override;
    /// <summary>Header height.</summary>
    function HeaderHeight: Integer; override;
    /// <summary>Paints the header.</summary>
    procedure DrawHeader(APainter: TOBDPainter; const ARect: TRect); override;
    /// <summary>Sorts by clicked column.</summary>
    procedure HeaderClick(X, Y: Integer); override;
    /// <summary>Paints one entry.</summary>
    procedure DrawItem(APainter: TOBDPainter; AIndex: Integer;
      const ARect: TRect; ASelected, AHot: Boolean); override;
    /// <summary>Opens or activates.</summary>
    procedure DoItemActivate; override;
  public
    /// <summary>Creates the list.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Reloads entries.</summary>
    procedure Refresh; reintroduce;
    /// <summary>Appends selected paths.</summary>
    procedure SelectedPaths(AList: TStrings);
    /// <summary>Selects by display name.</summary>
    function SelectByName(const AName: string): Boolean;
    /// <summary>Visible entry count.</summary>
    property FileCount: Integer read GetFileCount;
    /// <summary>Entry by index.</summary>
    property Entries[AIndex: Integer]: TOBDFileEntry read GetEntry;
  published
    /// <summary>Directory.</summary>
    property Directory: string read FDirectory write SetDirectory;
    /// <summary>Semicolon-separated masks.</summary>
    property Mask: string read FMask write SetMask;
    /// <summary>Substring filter.</summary>
    property SearchText: string read FSearchText write SetSearchText;
    /// <summary>Show folders.</summary>
    property ShowFolders: Boolean read FShowFolders write SetShowFolders
      default True;
    /// <summary>Show hidden/system entries.</summary>
    property ShowHidden: Boolean read FShowHidden write SetShowHidden
      default False;
    /// <summary>Details or icons.</summary>
    property ViewStyle: TOBDFileViewStyle read FViewStyle write SetViewStyle
      default fvDetails;
    /// <summary>Tile width in 96-DPI pixels.</summary>
    property TileWidth96: Integer read FTileWidth write SetTileWidth
      default 132;
    /// <summary>Multi-select.</summary>
    property MultiSelect;
    /// <summary>Border.</summary>
    property BorderVisible;
    /// <summary>Density.</summary>
    property Density;
    /// <summary>Parent density.</summary>
    property ParentDensity;
    /// <summary>Selection event.</summary>
    property OnChange;
    /// <summary>Folder open event.</summary>
    property OnFolderOpen: TOBDPathEvent read FOnFolderOpen
      write FOnFolderOpen;
    /// <summary>File activate event.</summary>
    property OnFileActivate: TOBDPathEvent read FOnFileActivate
      write FOnFileActivate;
  end;

/// <summary>Draws a reusable shell glyph.</summary>
procedure OBDDrawShellGlyph(APainter: TOBDPainter; AKind: TOBDShellGlyphKind;
  CX, CY: Integer; AColor: TColor; AScale: Single = 1);

implementation

type
  TBreadPart = record
    Caption: string;
    Path: string;
    Bounds: TRect;
  end;

function CleanPath(const APath: string): string;
begin
  Result := Trim(APath);
  if Result <> '' then
    Result := ExpandFileName(Result);
end;

function SameShellPath(const A, B: string): Boolean;
begin
  Result := SameText(ExcludeTrailingPathDelimiter(CleanPath(A)),
    ExcludeTrailingPathDelimiter(CleanPath(B)));
end;

function IsHiddenAttr(AAttr: Integer): Boolean;
begin
  Result := (AAttr and (faHidden or faSysFile)) <> 0;
end;

function UserFolder(const AName: string): string;
var
  Base: string;
begin
  Base := GetEnvironmentVariable('USERPROFILE');
  if Base = '' then
    Base := GetEnvironmentVariable('HOME');
  if Base = '' then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(Base) + AName;
end;

function KnownFolder(ACsidl: Integer; const AFallback: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Result := '';
  FillChar(Buffer, SizeOf(Buffer), 0);
  if SHGetFolderPath(0, ACsidl, 0, SHGFP_TYPE_CURRENT, Buffer) = S_OK then
    Result := Buffer;
  if (Result = '') or not DirectoryExists(Result) then
    Result := AFallback;
end;

function DriveCaption(const ARoot: string): string;
var
  Vol, FS: array[0..MAX_PATH] of Char;
  Serial, MaxComp, Flags: DWORD;
  Name: string;
begin
  FillChar(Vol, SizeOf(Vol), 0);
  FillChar(FS, SizeOf(FS), 0);
  Serial := 0;
  MaxComp := 0;
  Flags := 0;
  if GetVolumeInformation(PChar(ARoot), Vol, Length(Vol), @Serial,
    MaxComp, Flags, FS, Length(FS)) and (Vol[0] <> #0) then
    Name := Vol
  else if GetDriveType(PChar(ARoot)) = DRIVE_REMOVABLE then
    Name := 'Removable disk'
  else
    Name := 'Local disk';
  Result := Format('%s (%s)', [Name, ExcludeTrailingPathDelimiter(ARoot)]);
end;

function IsPictureFile(const AName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AName));
  Result := MatchText(Ext, ['.bmp', '.dib', '.gif', '.jpg', '.jpeg', '.jpe',
    '.png', '.tif', '.tiff', '.ico', '.wmf', '.emf']);
end;

function EntryTypeName(const AName: string; AFolder: Boolean): string;
var
  Ext: string;
begin
  if AFolder then
    Exit('Folder');
  Ext := ExtractFileExt(AName);
  if Ext = '' then
    Result := 'File'
  else
    Result := UpperCase(Copy(Ext, 2, MaxInt)) + ' file';
end;

function FormatFileSize(ASize: Int64): string;
begin
  if ASize < 1024 then
    Result := Format('%d B', [ASize])
  else if ASize < 1024 * 1024 then
    Result := Format('%d KB', [(ASize + 1023) div 1024])
  else
    Result := FormatFloat('0.# MB', ASize / (1024 * 1024));
end;

procedure OBDDrawShellGlyph(APainter: TOBDPainter; AKind: TOBDShellGlyphKind;
  CX, CY: Integer; AColor: TColor; AScale: Single);
var
  S, X, Y: Single;
begin
  S := APainter.SF(AScale);
  X := CX;
  Y := CY;
  case AKind of
    sgFolder, sgNewFolder:
      begin
        APainter.RoundRect(X - 9 * S, Y - 7 * S, 7 * S, 4 * S, S,
          APainter.Tint(AColor, 0.12), AColor, 1.3 * S);
        APainter.RoundRect(X - 10 * S, Y - 3 * S, 20 * S, 12 * S, 1.5 * S,
          clNone, AColor, 1.5 * S);
        if AKind = sgNewFolder then
        begin
          APainter.Lines([OBDPointF(X + 2 * S, Y + 3 * S),
            OBDPointF(X + 8 * S, Y + 3 * S)], AColor, 1.4 * S);
          APainter.Lines([OBDPointF(X + 5 * S, Y),
            OBDPointF(X + 5 * S, Y + 6 * S)], AColor, 1.4 * S);
        end;
      end;
    sgDrive:
      begin
        APainter.RoundRect(X - 10 * S, Y - 5 * S, 20 * S, 12 * S, 2 * S,
          clNone, AColor, 1.5 * S);
        APainter.HLine(Round(X - 7 * S), Round(Y + 2 * S), Round(14 * S),
          AColor);
        APainter.Ellipse(X + 4 * S, Y + 3 * S, 2 * S, 2 * S, AColor, clNone);
      end;
    sgFile, sgDocument:
      begin
        APainter.RoundRect(X - 7 * S, Y - 10 * S, 14 * S, 20 * S, S,
          clNone, AColor, 1.4 * S);
        APainter.Lines([OBDPointF(X + 2 * S, Y - 10 * S),
          OBDPointF(X + 7 * S, Y - 5 * S)], AColor, 1.1 * S);
        APainter.HLine(Round(X - 3 * S), Round(Y), Round(7 * S), AColor);
        APainter.HLine(Round(X - 3 * S), Round(Y + 5 * S), Round(7 * S),
          AColor);
      end;
    sgImage:
      begin
        APainter.RoundRect(X - 9 * S, Y - 8 * S, 18 * S, 16 * S, 1.5 * S,
          clNone, AColor, 1.4 * S);
        APainter.Ellipse(X - 4 * S, Y - 4 * S, 3 * S, 3 * S, AColor,
          clNone);
        APainter.Lines([OBDPointF(X - 8 * S, Y + 5 * S),
          OBDPointF(X - 2 * S, Y), OBDPointF(X + 2 * S, Y + 3 * S),
          OBDPointF(X + 8 * S, Y - 3 * S)], AColor, 1.3 * S);
      end;
    sgDesktop:
      begin
        APainter.RoundRect(X - 9 * S, Y - 7 * S, 18 * S, 12 * S, S,
          clNone, AColor, 1.4 * S);
        APainter.VLine(Round(X), Round(Y + 5 * S), Round(4 * S), AColor);
        APainter.HLine(Round(X - 5 * S), Round(Y + 9 * S), Round(10 * S),
          AColor);
      end;
    sgDownload:
      begin
        APainter.Lines([OBDPointF(X, Y - 9 * S), OBDPointF(X, Y + 3 * S),
          OBDPointF(X - 5 * S, Y - 2 * S)], AColor, 1.5 * S);
        APainter.Lines([OBDPointF(X, Y + 3 * S), OBDPointF(X + 5 * S,
          Y - 2 * S)], AColor, 1.5 * S);
        APainter.HLine(Round(X - 8 * S), Round(Y + 8 * S), Round(16 * S),
          AColor);
      end;
    sgUp:
      APainter.Glyph(glChevronUp, CX, CY, AColor, AScale);
    sgForward:
      APainter.Glyph(glChevronRight, CX, CY, AColor, AScale);
    sgList:
      begin
        APainter.HLine(Round(X - 7 * S), Round(Y - 5 * S), Round(14 * S),
          AColor);
        APainter.HLine(Round(X - 7 * S), Round(Y), Round(14 * S), AColor);
        APainter.HLine(Round(X - 7 * S), Round(Y + 5 * S), Round(14 * S),
          AColor);
      end;
    sgGrid:
      begin
        APainter.FrameRect(Rect(Round(X - 8 * S), Round(Y - 8 * S),
          Round(X - S), Round(Y - S)), AColor);
        APainter.FrameRect(Rect(Round(X + S), Round(Y - 8 * S),
          Round(X + 8 * S), Round(Y - S)), AColor);
        APainter.FrameRect(Rect(Round(X - 8 * S), Round(Y + S),
          Round(X - S), Round(Y + 8 * S)), AColor);
        APainter.FrameRect(Rect(Round(X + S), Round(Y + S),
          Round(X + 8 * S), Round(Y + 8 * S)), AColor);
      end;
  end;
end;

{ TOBDPlace }

procedure TOBDPlace.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  Changed(False);
end;

procedure TOBDPlace.SetPath(const AValue: string);
begin
  if FPath = AValue then
    Exit;
  FPath := AValue;
  Changed(False);
end;

function TOBDPlace.DisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := FPath;
end;

{ TOBDPlaces }

constructor TOBDPlaces.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDPlace);
end;

procedure TOBDPlaces.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TOBDPlaces.Add: TOBDPlace;
begin
  Result := TOBDPlace(inherited Add);
end;

function TOBDPlaces.GetItem(AIndex: Integer): TOBDPlace;
begin
  Result := TOBDPlace(inherited Items[AIndex]);
end;

procedure TOBDPlaces.SetItem(AIndex: Integer; AValue: TOBDPlace);
begin
  inherited Items[AIndex] := AValue;
end;

procedure BuildBread(const APath: string; out AParts: TArray<TBreadPart>);
var
  Root, Rest, Cur, Part: string;
  DelimPos, N: Integer;
begin
  SetLength(AParts, 0);
  if APath = '' then
    Exit;
  Root := ExtractFileDrive(APath);
  if Root <> '' then
    Root := IncludeTrailingPathDelimiter(Root)
  else
    Root := IncludeTrailingPathDelimiter(APath);
  N := 1;
  SetLength(AParts, 1);
  AParts[0].Caption := ExcludeTrailingPathDelimiter(Root);
  if AParts[0].Caption = '' then
    AParts[0].Caption := Root;
  AParts[0].Path := Root;
  Cur := IncludeTrailingPathDelimiter(Root);
  Rest := Copy(ExcludeTrailingPathDelimiter(APath), Length(Cur) + 1, MaxInt);
  while Rest <> '' do
  begin
    DelimPos := Pos(PathDelim, Rest);
    if DelimPos = 0 then
    begin
      Part := Rest;
      Rest := '';
    end
    else
    begin
      Part := Copy(Rest, 1, DelimPos - 1);
      Delete(Rest, 1, DelimPos);
    end;
    if Part = '' then
      Continue;
    Cur := IncludeTrailingPathDelimiter(Cur) + Part;
    SetLength(AParts, N + 1);
    AParts[N].Caption := Part;
    AParts[N].Path := Cur;
    Inc(N);
  end;
end;

{ TOBDBreadcrumb }

constructor TOBDBreadcrumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 360;
  Height := 32;
  TabStop := False;
  FHotIndex := -1;
end;

procedure TOBDBreadcrumb.SetPath(const AValue: string);
var
  S: string;
begin
  S := CleanPath(AValue);
  if FPath = S then
    Exit;
  FPath := S;
  Invalidate;
end;

procedure TOBDBreadcrumb.SetHotIndex(AValue: Integer);
begin
  if FHotIndex = AValue then
    Exit;
  FHotIndex := AValue;
  Invalidate;
end;

procedure TOBDBreadcrumb.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  SetHotIndex(-1);
end;

procedure TOBDBreadcrumb.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  Parts: TArray<TBreadPart>;
  I, First, Pad, Chev, X, W, Total, H: Integer;
  R: TRect;
  C: TColor;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, Palette.GaugeFace);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    BuildBread(FPath, Parts);
    Pad := ScaleValue(10);
    Chev := ScaleValue(18);
    First := 0;
    while First < Length(Parts) do
    begin
      Total := 0;
      for I := First to High(Parts) do
        Inc(Total, P.TextWidth(Parts[I].Caption, OBD_TEXT_BODY) + 2 * Pad +
          Chev);
      if (First = 0) or (Total <= Width - ScaleValue(10)) then
        Break;
      Inc(First);
    end;
    X := ScaleValue(6);
    H := Height - ScaleValue(6);
    if First > 0 then
    begin
      W := P.TextWidth('...', OBD_TEXT_BODY) + 2 * Pad;
      R := Rect(X, (Height - H) div 2, X + W, (Height + H) div 2);
      if FHotIndex = 0 then
        P.FillRect(R, P.Tint(Palette.Accent, 0.12));
      P.Text(R.Left + Pad, Height div 2, '...', OBD_TEXT_BODY,
        Palette.ForegroundText, twRegular, taLeftJustify, R.Width - 2 * Pad);
      Inc(X, W + Chev);
      P.Glyph(glChevronRight, X - Chev div 2, Height div 2, Palette.Subtle,
        0.8);
    end;
    for I := First to High(Parts) do
    begin
      W := P.TextWidth(Parts[I].Caption, OBD_TEXT_BODY) + 2 * Pad;
      R := Rect(X, (Height - H) div 2, X + W, (Height + H) div 2);
      if I - First + Ord(First > 0) = FHotIndex then
        P.FillRect(R, P.Tint(Palette.Accent, 0.12));
      C := Palette.ForegroundText;
      if I = High(Parts) then
        C := P.AccentText;
      P.Text(R.Left + Pad, Height div 2, Parts[I].Caption, OBD_TEXT_BODY,
        C, twRegular, taLeftJustify, R.Width - 2 * Pad);
      Inc(X, W);
      if I < High(Parts) then
      begin
        P.Glyph(glChevronRight, X + Chev div 2, Height div 2, Palette.Subtle,
          0.8);
        Inc(X, Chev);
      end;
    end;
  finally
    P.Free;
  end;
end;

procedure TOBDBreadcrumb.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TOBDPainter;
  B: TBitmap;
  Parts: TArray<TBreadPart>;
  I, First, Pad, Chev, PX, W, Total, Index: Integer;
begin
  inherited;
  B := TBitmap.Create;
  try
    B.SetSize(1, 1);
    P := TOBDPainter.Create(B.Canvas, Palette, ScaleValue(96));
    try
      BuildBread(FPath, Parts);
      Pad := ScaleValue(10);
      Chev := ScaleValue(18);
      First := 0;
      while First < Length(Parts) do
      begin
        Total := 0;
        for I := First to High(Parts) do
          Inc(Total, P.TextWidth(Parts[I].Caption, OBD_TEXT_BODY) +
            2 * Pad + Chev);
        if (First = 0) or (Total <= Width - ScaleValue(10)) then
          Break;
        Inc(First);
      end;
      PX := ScaleValue(6);
      Index := 0;
      SetHotIndex(-1);
      if First > 0 then
      begin
        W := P.TextWidth('...', OBD_TEXT_BODY) + 2 * Pad;
        if PtInRect(Rect(PX, 0, PX + W, Height), Point(X, Y)) then
          SetHotIndex(0);
        Inc(PX, W + Chev);
        Inc(Index);
      end;
      for I := First to High(Parts) do
      begin
        W := P.TextWidth(Parts[I].Caption, OBD_TEXT_BODY) + 2 * Pad;
        if PtInRect(Rect(PX, 0, PX + W, Height), Point(X, Y)) then
        begin
          SetHotIndex(Index);
          Break;
        end;
        Inc(PX, W + Chev);
        Inc(Index);
      end;
    finally
      P.Free;
    end;
  finally
    B.Free;
  end;
end;

procedure TOBDBreadcrumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Parts: TArray<TBreadPart>;
  Target, First: Integer;
  B: TBitmap;
  P: TOBDPainter;
  I, Pad, Chev, Total: Integer;
begin
  inherited;
  if (Button <> mbLeft) or (FHotIndex < 0) then
    Exit;
  BuildBread(FPath, Parts);
  B := TBitmap.Create;
  try
    B.SetSize(1, 1);
    P := TOBDPainter.Create(B.Canvas, Palette, ScaleValue(96));
    try
      Pad := ScaleValue(10);
      Chev := ScaleValue(18);
      First := 0;
      while First < Length(Parts) do
      begin
        Total := 0;
        for I := First to High(Parts) do
          Inc(Total, P.TextWidth(Parts[I].Caption, OBD_TEXT_BODY) +
            2 * Pad + Chev);
        if (First = 0) or (Total <= Width - ScaleValue(10)) then
          Break;
        Inc(First);
      end;
    finally
      P.Free;
    end;
  finally
    B.Free;
  end;
  Target := FHotIndex + First;
  if First > 0 then
    Dec(Target);
  if (Target >= 0) and (Target <= High(Parts)) then
  begin
    SetPath(Parts[Target].Path);
    if Assigned(FOnNavigate) then
      FOnNavigate(Self, FPath);
  end;
end;

{ TOBDPlacesPane }

constructor TOBDPlacesPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlaces := TOBDPlaces.Create(Self);
  FPlaces.OnChange := PlacesChanged;
  BorderVisible := False;
  Width := 196;
  FRowsValid := False;
end;

destructor TOBDPlacesPane.Destroy;
begin
  FPlaces.Free;
  inherited;
end;

procedure TOBDPlacesPane.PlacesChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TOBDPlacesPane.SetPlaces(AValue: TOBDPlaces);
begin
  FPlaces.Assign(AValue);
  Refresh;
end;

procedure TOBDPlacesPane.Refresh;
begin
  FRowsValid := False;
  BuildRows;
  ItemsChanged;
end;

procedure TOBDPlacesPane.BuildRows;
var
  I, Mask: Integer;
  Root: string;

  procedure AddPlace(AKind: TRowKind; const ACaption, APath: string);
  var
    N: Integer;
  begin
    if (AKind <> rkHeader) and ((APath = '') or not DirectoryExists(APath)) then
      Exit;
    N := Length(FRows);
    SetLength(FRows, N + 1);
    FRows[N].Kind := AKind;
    FRows[N].Caption := ACaption;
    FRows[N].Path := APath;
  end;

begin
  if FRowsValid then
    Exit;
  SetLength(FRows, 0);
  AddPlace(rkHeader, 'QUICK ACCESS', '');
  AddPlace(rkDesktop, 'Desktop', KnownFolder(CSIDL_DESKTOPDIRECTORY,
    UserFolder('Desktop')));
  AddPlace(rkDocument, 'Documents', KnownFolder(CSIDL_PERSONAL,
    UserFolder('Documents')));
  AddPlace(rkDownload, 'Downloads', UserFolder('Downloads'));
  AddPlace(rkImage, 'Pictures', KnownFolder(CSIDL_MYPICTURES,
    UserFolder('Pictures')));
  for I := 0 to FPlaces.Count - 1 do
    AddPlace(rkApp, FPlaces[I].DisplayName, FPlaces[I].Path);
  AddPlace(rkHeader, 'THIS PC', '');
  Mask := GetLogicalDrives;
  for I := 0 to 25 do
    if (Mask and (1 shl I)) <> 0 then
    begin
      Root := Chr(Ord('A') + I) + ':\';
      AddPlace(rkDrive, DriveCaption(Root), Root);
    end;
  FRowsValid := True;
end;

function TOBDPlacesPane.GetItemCount: Integer;
begin
  BuildRows;
  Result := Length(FRows);
end;

function TOBDPlacesPane.GetItemText(AIndex: Integer): string;
begin
  BuildRows;
  if (AIndex >= 0) and (AIndex < Length(FRows)) then
    Result := FRows[AIndex].Caption
  else
    Result := '';
end;

function TOBDPlacesPane.RowIsHeader(AIndex: Integer): Boolean;
begin
  BuildRows;
  Result := (AIndex >= 0) and (AIndex < Length(FRows)) and
    (FRows[AIndex].Kind = rkHeader);
end;

function TOBDPlacesPane.ListBackground: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDPlacesPane.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  X, CY: Integer;
  Kind: TOBDShellGlyphKind;
  Ink: TColor;
begin
  BuildRows;
  if (AIndex < 0) or (AIndex >= Length(FRows)) then
    Exit;
  CY := (ARect.Top + ARect.Bottom) div 2;
  if FRows[AIndex].Kind = rkHeader then
  begin
    APainter.Caps(ARect.Left + ScaleValue(18), CY, FRows[AIndex].Caption);
    Exit;
  end;
  Kind := sgFolder;
  case FRows[AIndex].Kind of
    rkDrive:
      Kind := sgDrive;
    rkDesktop:
      Kind := sgDesktop;
    rkDownload:
      Kind := sgDownload;
    rkDocument:
      Kind := sgDocument;
    rkImage:
      Kind := sgImage;
  end;
  Ink := Palette.ForegroundText;
  if ASelected then
    Ink := APainter.AccentText;
  X := ARect.Left + ScaleValue(20);
  OBDDrawShellGlyph(APainter, Kind, X, CY, Ink, 0.82);
  APainter.Text(ARect.Left + ScaleValue(44), CY, FRows[AIndex].Caption,
    OBD_TEXT_BODY, Ink, twRegular, taLeftJustify,
    ARect.Width - ScaleValue(52));
end;

procedure TOBDPlacesPane.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if RowIsHeader(ItemAt(X, Y)) then
    Exit;
  inherited;
end;

procedure TOBDPlacesPane.Change;
var
  I, J: Integer;
begin
  if FChanging then
    Exit;
  I := ItemIndex;
  if RowIsHeader(I) then
  begin
    J := I + 1;
    while (J < GetItemCount) and RowIsHeader(J) do
      Inc(J);
    if J >= GetItemCount then
      J := -1;
    FChanging := True;
    try
      ItemIndex := J;
    finally
      FChanging := False;
    end;
    Exit;
  end;
  inherited;
  if (I >= 0) and (I < Length(FRows)) then
  begin
    FPath := FRows[I].Path;
    if Assigned(FOnNavigate) then
      FOnNavigate(Self, FPath);
  end;
end;

procedure TOBDPlacesPane.SetPath(const AValue: string);
var
  I: Integer;
begin
  FPath := CleanPath(AValue);
  BuildRows;
  for I := 0 to High(FRows) do
    if (FRows[I].Path <> '') and SameShellPath(FRows[I].Path, FPath) then
    begin
      FChanging := True;
      try
        ItemIndex := I;
      finally
        FChanging := False;
      end;
      Break;
    end;
  Invalidate;
end;

{ TOBDFolderTree }

constructor TOBDFolderTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExpanded := TStringList.Create;
  FExpanded.Sorted := True;
  FExpanded.Duplicates := dupIgnore;
  EmptyText := 'No folders';
end;

destructor TOBDFolderTree.Destroy;
begin
  FExpanded.Free;
  inherited;
end;

procedure TOBDFolderTree.SetRootPath(const AValue: string);
begin
  if FRootPath = AValue then
    Exit;
  FRootPath := CleanPath(AValue);
  Refresh;
end;

procedure TOBDFolderTree.SetShowHidden(AValue: Boolean);
begin
  if FShowHidden = AValue then
    Exit;
  FShowHidden := AValue;
  Refresh;
end;

procedure TOBDFolderTree.Refresh;
begin
  FRowsValid := False;
  BuildRows;
  ItemsChanged;
end;

function FolderHasChildren(const APath: string; AShowHidden: Boolean): Boolean;
var
  SR: TSearchRec;
  Path: string;
begin
  Result := False;
  Path := IncludeTrailingPathDelimiter(APath) + '*';
  if FindFirst(Path, faDirectory or faHidden or faSysFile, SR) <> 0 then
    Exit;
  try
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') and
        ((SR.Attr and faDirectory) <> 0) and
        (AShowHidden or not IsHiddenAttr(SR.Attr)) then
      begin
        Result := True;
        Break;
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

procedure TOBDFolderTree.BuildRows;
var
  Mask, I: Integer;

  procedure AddNode(const ACaption, APath: string; ALevel: Integer;
    AExpanded, AHasChildren: Boolean);
  var
    N: Integer;
  begin
    N := Length(FRows);
    SetLength(FRows, N + 1);
    FRows[N].Caption := ACaption;
    FRows[N].Path := APath;
    FRows[N].Level := ALevel;
    FRows[N].Expanded := AExpanded;
    FRows[N].HasChildren := AHasChildren;
  end;

  procedure AddFolder(const APath, ACaption: string; ALevel: Integer);
  var
    SR: TSearchRec;
    Child, Cap: string;
    Expanded, HasChildren: Boolean;
  begin
    if not DirectoryExists(APath) then
      Exit;
    HasChildren := FolderHasChildren(APath, FShowHidden);
    Expanded := FExpanded.IndexOf(ExcludeTrailingPathDelimiter(APath)) >= 0;
    AddNode(ACaption, APath, ALevel, Expanded, HasChildren);
    if not Expanded then
      Exit;
    if FindFirst(IncludeTrailingPathDelimiter(APath) + '*',
      faDirectory or faHidden or faSysFile, SR) <> 0 then
      Exit;
    try
      repeat
        if (SR.Name = '.') or (SR.Name = '..') or
          ((SR.Attr and faDirectory) = 0) or
          (not FShowHidden and IsHiddenAttr(SR.Attr)) then
          Continue;
        Child := IncludeTrailingPathDelimiter(APath) + SR.Name;
        Cap := SR.Name;
        AddFolder(Child, Cap, ALevel + 1);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;

begin
  if FRowsValid then
    Exit;
  SetLength(FRows, 0);
  if FRootPath <> '' then
    AddFolder(FRootPath, ExtractFileName(ExcludeTrailingPathDelimiter(
      FRootPath)), 0)
  else
  begin
    Mask := GetLogicalDrives;
    for I := 0 to 25 do
      if (Mask and (1 shl I)) <> 0 then
        AddFolder(Chr(Ord('A') + I) + ':\', Chr(Ord('A') + I) + ':', 0);
  end;
  FRowsValid := True;
end;

function TOBDFolderTree.GetItemCount: Integer;
begin
  BuildRows;
  Result := Length(FRows);
end;

function TOBDFolderTree.GetItemText(AIndex: Integer): string;
begin
  BuildRows;
  if (AIndex >= 0) and (AIndex < Length(FRows)) then
    Result := FRows[AIndex].Caption
  else
    Result := '';
end;

function TOBDFolderTree.ChevronRect(AIndex: Integer): TRect;
var
  R: TRect;
begin
  R := ItemRect(AIndex);
  Result := Rect(R.Left + ScaleValue(8 + FRows[AIndex].Level * 18), R.Top,
    R.Left + ScaleValue(30 + FRows[AIndex].Level * 18), R.Bottom);
end;

procedure TOBDFolderTree.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  X, CY: Integer;
begin
  BuildRows;
  if (AIndex < 0) or (AIndex >= Length(FRows)) then
    Exit;
  CY := (ARect.Top + ARect.Bottom) div 2;
  X := ARect.Left + ScaleValue(14 + FRows[AIndex].Level * 18);
  if FRows[AIndex].HasChildren then
  begin
    if FRows[AIndex].Expanded then
      APainter.Glyph(glChevronDown, X, CY, Palette.Subtle, 0.72)
    else
      APainter.Glyph(glChevronRight, X, CY, Palette.Subtle, 0.72);
  end;
  OBDDrawShellGlyph(APainter, sgFolder, X + ScaleValue(22), CY,
    Palette.ForegroundText, 0.78);
  APainter.Text(X + ScaleValue(42), CY, FRows[AIndex].Caption, OBD_TEXT_BODY,
    Palette.ForegroundText, twRegular, taLeftJustify,
    ARect.Right - X - ScaleValue(48));
end;

procedure TOBDFolderTree.Toggle(AIndex: Integer);
var
  Key: string;
  P: Integer;
begin
  BuildRows;
  if (AIndex < 0) or (AIndex >= Length(FRows)) or
    not FRows[AIndex].HasChildren then
    Exit;
  Key := ExcludeTrailingPathDelimiter(FRows[AIndex].Path);
  P := FExpanded.IndexOf(Key);
  if P >= 0 then
    FExpanded.Delete(P)
  else
    FExpanded.Add(Key);
  Refresh;
end;

procedure TOBDFolderTree.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  I := ItemAt(X, Y);
  if (Button = mbLeft) and (I >= 0) and PtInRect(ChevronRect(I), Point(X, Y)) then
  begin
    Toggle(I);
    Exit;
  end;
  inherited;
end;

procedure TOBDFolderTree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RIGHT) and (ItemIndex >= 0) then
  begin
    BuildRows;
    if (ItemIndex < Length(FRows)) and FRows[ItemIndex].HasChildren and
      not FRows[ItemIndex].Expanded then
    begin
      Toggle(ItemIndex);
      Key := 0;
      Exit;
    end;
  end
  else if (Key = VK_LEFT) and (ItemIndex >= 0) then
  begin
    BuildRows;
    if (ItemIndex < Length(FRows)) and FRows[ItemIndex].Expanded then
    begin
      Toggle(ItemIndex);
      Key := 0;
      Exit;
    end;
  end;
  inherited;
end;

procedure TOBDFolderTree.Change;
var
  I: Integer;
begin
  inherited;
  if FChanging then
    Exit;
  I := ItemIndex;
  if (I >= 0) and (I < Length(FRows)) then
  begin
    FPath := FRows[I].Path;
    if Assigned(FOnPathChange) then
      FOnPathChange(Self, FPath);
  end;
end;

procedure TOBDFolderTree.SetPath(const AValue: string);
var
  S, Cur, Rest, Part: string;
  DelimPos, I: Integer;
begin
  S := CleanPath(AValue);
  if S = '' then
    Exit;
  FPath := S;
  Cur := ExtractFileDrive(S);
  if Cur <> '' then
    Cur := IncludeTrailingPathDelimiter(Cur)
  else
    Cur := FRootPath;
  if Cur <> '' then
    FExpanded.Add(ExcludeTrailingPathDelimiter(Cur));
  Rest := Copy(ExcludeTrailingPathDelimiter(S), Length(Cur) + 1, MaxInt);
  while Rest <> '' do
  begin
    DelimPos := Pos(PathDelim, Rest);
    if DelimPos = 0 then
    begin
      Part := Rest;
      Rest := '';
    end
    else
    begin
      Part := Copy(Rest, 1, DelimPos - 1);
      Delete(Rest, 1, DelimPos);
    end;
    if Part = '' then
      Continue;
    Cur := IncludeTrailingPathDelimiter(Cur) + Part;
    FExpanded.Add(ExcludeTrailingPathDelimiter(Cur));
  end;
  Refresh;
  for I := 0 to High(FRows) do
    if SameShellPath(FRows[I].Path, S) then
    begin
      FChanging := True;
      try
        ItemIndex := I;
      finally
        FChanging := False;
      end;
      Break;
    end;
end;

{ TOBDFileList }

constructor TOBDFileList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMask := '*.*';
  FShowFolders := True;
  FViewStyle := fvDetails;
  FSortColumn := 0;
  FSortAscending := True;
  FTileWidth := 132;
  EmptyText := 'No files';
end;

procedure TOBDFileList.SetDirectory(const AValue: string);
var
  S: string;
begin
  S := CleanPath(AValue);
  if FDirectory = S then
    Exit;
  FDirectory := S;
  Refresh;
end;

procedure TOBDFileList.SetMask(const AValue: string);
begin
  if FMask = AValue then
    Exit;
  FMask := AValue;
  if FMask = '' then
    FMask := '*.*';
  Refresh;
end;

procedure TOBDFileList.SetSearchText(const AValue: string);
begin
  if FSearchText = AValue then
    Exit;
  FSearchText := AValue;
  Refresh;
end;

procedure TOBDFileList.SetShowFolders(AValue: Boolean);
begin
  if FShowFolders = AValue then
    Exit;
  FShowFolders := AValue;
  Refresh;
end;

procedure TOBDFileList.SetShowHidden(AValue: Boolean);
begin
  if FShowHidden = AValue then
    Exit;
  FShowHidden := AValue;
  Refresh;
end;

procedure TOBDFileList.SetViewStyle(AValue: TOBDFileViewStyle);
begin
  if FViewStyle = AValue then
    Exit;
  FViewStyle := AValue;
  Refresh;
end;

procedure TOBDFileList.SetTileWidth(AValue: Integer);
begin
  AValue := System.Math.Max(72, AValue);
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
  ItemsChanged;
end;

function TOBDFileList.GetFileCount: Integer;
begin
  Result := Length(FEntries);
end;

function TOBDFileList.GetEntry(AIndex: Integer): TOBDFileEntry;
begin
  if (AIndex >= 0) and (AIndex < Length(FEntries)) then
    Result := FEntries[AIndex]
  else
    Result := Default(TOBDFileEntry);
end;

procedure AddEntry(var AEntries: TArray<TOBDFileEntry>;
  const AEntry: TOBDFileEntry);
var
  N: Integer;
begin
  N := Length(AEntries);
  SetLength(AEntries, N + 1);
  AEntries[N] := AEntry;
end;

function MaskMatches(const AName, AMasks: string): Boolean;
var
  L: TStringList;
  I: Integer;
  M: string;
begin
  Result := False;
  L := TStringList.Create;
  try
    L.StrictDelimiter := True;
    L.Delimiter := ';';
    L.DelimitedText := AMasks;
    for I := 0 to L.Count - 1 do
    begin
      M := Trim(L[I]);
      if M = '' then
        Continue;
      if MatchesMask(AName, M) then
        Exit(True);
    end;
  finally
    L.Free;
  end;
end;

procedure TOBDFileList.Refresh;
var
  SR: TSearchRec;
  E: TOBDFileEntry;
  Base, Search: string;
begin
  SetLength(FEntries, 0);
  Base := FDirectory;
  if (Base = '') or not DirectoryExists(Base) then
  begin
    ItemsChanged;
    Exit;
  end;
  Search := LowerCase(FSearchText);
  if FindFirst(IncludeTrailingPathDelimiter(Base) + '*',
    faAnyFile or faHidden or faSysFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      if not FShowHidden and IsHiddenAttr(SR.Attr) then
        Continue;
      if (Search <> '') and not ContainsText(SR.Name, Search) then
        Continue;
      E := Default(TOBDFileEntry);
      E.Name := SR.Name;
      E.FullPath := IncludeTrailingPathDelimiter(Base) + SR.Name;
      E.IsFolder := (SR.Attr and faDirectory) <> 0;
      if E.IsFolder then
      begin
        if not FShowFolders then
          Continue;
      end
      else if not MaskMatches(SR.Name, FMask) then
        Continue;
      E.Size := SR.Size;
      E.Modified := SR.TimeStamp;
      E.TypeName := EntryTypeName(SR.Name, E.IsFolder);
      AddEntry(FEntries, E);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  SortEntries;
  ItemsChanged;
end;

function CompareEntry(const A, B: TOBDFileEntry; AColumn: Integer;
  AAscending: Boolean): Integer;
begin
  if A.IsFolder <> B.IsFolder then
  begin
    if A.IsFolder then
      Result := -1
    else
      Result := 1;
    Exit;
  end;
  case AColumn of
    1:
      if A.Modified < B.Modified then
        Result := -1
      else if A.Modified > B.Modified then
        Result := 1
      else
        Result := 0;
    2:
      Result := CompareText(A.TypeName, B.TypeName);
    3:
      if A.Size < B.Size then
        Result := -1
      else if A.Size > B.Size then
        Result := 1
      else
        Result := 0;
  else
    Result := CompareText(A.Name, B.Name);
  end;
  if not AAscending then
    Result := -Result;
end;

procedure TOBDFileList.SortEntries;
var
  I, J: Integer;
  T: TOBDFileEntry;
begin
  for I := 0 to High(FEntries) - 1 do
    for J := I + 1 to High(FEntries) do
      if CompareEntry(FEntries[I], FEntries[J], FSortColumn,
        FSortAscending) > 0 then
      begin
        T := FEntries[I];
        FEntries[I] := FEntries[J];
        FEntries[J] := T;
      end;
end;

function TOBDFileList.GetItemCount: Integer;
begin
  Result := Length(FEntries);
end;

function TOBDFileList.GetItemText(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < Length(FEntries)) then
    Result := FEntries[AIndex].Name
  else
    Result := '';
end;

function TOBDFileList.ItemHeight: Integer;
begin
  if FViewStyle = fvIcons then
    Result := ScaleValue(96)
  else
    Result := ScaleValue(Metrics.Row);
end;

function TOBDFileList.TileWidth: Integer;
begin
  if FViewStyle = fvIcons then
    Result := ScaleValue(FTileWidth)
  else
    Result := 0;
end;

function TOBDFileList.HeaderHeight: Integer;
begin
  if FViewStyle = fvDetails then
    Result := ScaleValue(Metrics.ColHead)
  else
    Result := 0;
end;

function TOBDFileList.ColumnAt(X: Integer): Integer;
var
  W: Integer;
begin
  W := ContentRect.Width;
  if X < ScaleValue(390) then
    Result := 0
  else if X < W - ScaleValue(280) then
    Result := 1
  else if X < W - ScaleValue(110) then
    Result := 2
  else
    Result := 3;
end;

procedure TOBDFileList.DrawHeader(APainter: TOBDPainter; const ARect: TRect);
const
  CAPTIONS: array[0..3] of string = ('NAME', 'DATE MODIFIED', 'TYPE', 'SIZE');
var
  Xs: array[0..3] of Integer;
  I, CY: Integer;
begin
  Xs[0] := ARect.Left + ScaleValue(20);
  Xs[1] := ARect.Left + ScaleValue(390);
  Xs[2] := ARect.Right - ScaleValue(280);
  Xs[3] := ARect.Right - ScaleValue(64);
  CY := (ARect.Top + ARect.Bottom) div 2;
  for I := 0 to 3 do
  begin
    APainter.Caps(Xs[I], CY, CAPTIONS[I]);
    if I = FSortColumn then
      if FSortAscending then
        APainter.Glyph(glChevronUp, Xs[I] + ScaleValue(62), CY,
          APainter.AccentText, 0.7)
      else
        APainter.Glyph(glChevronDown, Xs[I] + ScaleValue(62), CY,
          APainter.AccentText, 0.7);
  end;
end;

procedure TOBDFileList.HeaderClick(X, Y: Integer);
var
  C: Integer;
begin
  C := ColumnAt(X);
  if C = FSortColumn then
    FSortAscending := not FSortAscending
  else
  begin
    FSortColumn := C;
    FSortAscending := True;
  end;
  SortEntries;
  ItemsChanged;
end;

procedure TOBDFileList.DrawItem(APainter: TOBDPainter; AIndex: Integer;
  const ARect: TRect; ASelected, AHot: Boolean);
var
  E: TOBDFileEntry;
  CY, X, W: Integer;
  G: TOBDShellGlyphKind;
begin
  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
    Exit;
  E := FEntries[AIndex];
  if E.IsFolder then
    G := sgFolder
  else if IsPictureFile(E.Name) then
    G := sgImage
  else
    G := sgFile;
  CY := (ARect.Top + ARect.Bottom) div 2;
  if FViewStyle = fvIcons then
  begin
    OBDDrawShellGlyph(APainter, G, (ARect.Left + ARect.Right) div 2,
      ARect.Top + ScaleValue(32), Palette.ForegroundText, 1.4);
    APainter.WrapText(Rect(ARect.Left + ScaleValue(8), ARect.Top +
      ScaleValue(56), ARect.Right - ScaleValue(8), ARect.Bottom -
      ScaleValue(4)), E.Name, OBD_TEXT_BODY, Palette.ForegroundText,
      twRegular);
    Exit;
  end;
  X := ARect.Left + ScaleValue(24);
  OBDDrawShellGlyph(APainter, G, X, CY, Palette.ForegroundText, 0.9);
  APainter.Text(ARect.Left + ScaleValue(52), CY, E.Name, OBD_TEXT_BODY,
    Palette.ForegroundText, twRegular, taLeftJustify, ScaleValue(315));
  APainter.Text(ARect.Left + ScaleValue(390), CY, DateTimeToStr(E.Modified),
    OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taLeftJustify,
    ScaleValue(190));
  W := ARect.Right - ARect.Left;
  APainter.Text(ARect.Right - ScaleValue(280), CY, E.TypeName, OBD_TEXT_BODY,
    Palette.GaugeLabel, twRegular, taLeftJustify, ScaleValue(160));
  if not E.IsFolder then
    APainter.Text(ARect.Right - ScaleValue(20), CY, FormatFileSize(E.Size),
      OBD_TEXT_BODY, Palette.GaugeLabel, twRegular, taRightJustify,
      System.Math.Max(1, W));
end;

procedure TOBDFileList.DoItemActivate;
var
  I: Integer;
begin
  I := ItemIndex;
  if (I < 0) or (I >= Length(FEntries)) then
    Exit;
  if FEntries[I].IsFolder then
  begin
    if Assigned(FOnFolderOpen) then
      FOnFolderOpen(Self, FEntries[I].FullPath);
  end
  else if Assigned(FOnFileActivate) then
    FOnFileActivate(Self, FEntries[I].FullPath);
  inherited;
end;

procedure TOBDFileList.SelectedPaths(AList: TStrings);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  for I := 0 to High(FEntries) do
    if Selected[I] then
      AList.Add(FEntries[I].FullPath);
end;

function TOBDFileList.SelectByName(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FEntries) do
    if SameText(FEntries[I].Name, AName) then
    begin
      ItemIndex := I;
      Result := True;
      Break;
    end;
end;

end.
