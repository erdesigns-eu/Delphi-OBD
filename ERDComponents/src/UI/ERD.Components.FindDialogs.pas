//------------------------------------------------------------------------------
//  ERD.Components.FindDialogs
//
//  Themed modeless find and replace dialogs for Application.
//
//    TERDFindDialog     modeless replacement for TFindDialog.
//    TERDReplaceDialog  modeless replacement for TReplaceDialog.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed text dialogs.
//------------------------------------------------------------------------------

unit ERD.Components.FindDialogs;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  ERD.Components.Types,
  ERD.Components.Paint,
  ERD.Components.Buttons,
  ERD.Components.Edits,
  ERD.Components.Segmented,
  ERD.Components.Dialogs;

type
  /// <summary>Find and replace state and the action that raised an event.</summary>
  TERDFindOption = (
    /// <summary>Text comparison is case-sensitive.</summary>
    obfoMatchCase,
    /// <summary>The match must span a whole word.</summary>
    obfoWholeWord,
    /// <summary>The Find next button or Enter requested a search.</summary>
    obfoFindNext,
    /// <summary>The Replace button requested one replacement.</summary>
    obfoReplace,
    /// <summary>The Replace all button requested a global replacement.</summary>
    obfoReplaceAll);

  /// <summary>Set of options for <see cref="TERDFindDialog"/>.</summary>
  TERDFindOptions = set of TERDFindOption;

  /// <summary>Direction used by the next find operation.</summary>
  TERDFindDirection = (
    /// <summary>Search upwards.</summary>
    ofdUp,
    /// <summary>Search downwards.</summary>
    ofdDown);

  /// <summary>Modeless themed replacement for VCL TFindDialog.</summary>
  TERDFindDialog = class(TERDCommonDialog)
  strict private
    FFindText: string;
    FOptions: TERDFindOptions;
    FDirection: TERDFindDirection;
    FResultText: string;
    FPosition: TPoint;
    FLeft: Integer;
    FTop: Integer;
    FWindow: TERDDialogWindow;
    FFindEdit: TERDEdit;
    FReplaceEdit: TERDEdit;
    FMatchCase: TERDCheckBox;
    FWholeWord: TERDCheckBox;
    FDirectionControl: TERDSegmented;
    FResultLabel: TERDDialogText;
    FFindButton: TERDButton;
    FReplaceButton: TERDButton;
    FReplaceAllButton: TERDButton;
    FCloseButton: TERDButton;
    FClosing: Boolean;
    FOnFind: TNotifyEvent;
    procedure SetFindText(const AValue: string);
    procedure SetOptions(AValue: TERDFindOptions);
    procedure SetDirection(AValue: TERDFindDirection);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetPosition(const AValue: TPoint);
    procedure FindEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DirectionChanged(Sender: TObject);
    procedure FindClick(Sender: TObject);
    procedure ReplaceClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure WindowClose(Sender: TObject; var Action: TCloseAction);
    procedure WindowDestroy(Sender: TObject);
    procedure ApplyStoredPosition(AWindow: TERDDialogWindow);
    procedure SaveWindowPosition;
    procedure ClearWindowRefs;
    procedure UpdateResultLabel;
  protected
    /// <summary>Text used by the replace dialog subclass.</summary>
    FReplaceText: string;
    /// <summary>Replace event used by the replace dialog subclass.</summary>
    FOnReplace: TNotifyEvent;
    /// <summary>Default title used while Title is empty.</summary>
    /// <returns>Dialog title.</returns>
    function DefaultTitle: string; override;
    /// <summary>True for the replace dialog subclass.</summary>
    /// <returns>Replace mode flag.</returns>
    function IsReplaceDialog: Boolean; virtual;
    /// <summary>Builds the modeless window and all controls.</summary>
    /// <returns>Created window owned by this component.</returns>
    function BuildWindow: TERDDialogWindow; virtual;
    /// <summary>Copies component properties to the visible controls.</summary>
    procedure UpdateControls; virtual;
    /// <summary>Copies visible control state back to component properties.</summary>
    procedure UpdateFromControls; virtual;
    /// <summary>Updates action flags and fires OnFind.</summary>
    procedure DoFind; virtual;
    /// <summary>Updates action flags and fires OnReplace.</summary>
    /// <param name="AAll">True for Replace all.</param>
    procedure DoReplace(AAll: Boolean); virtual;
  public
    /// <summary>Creates a modeless find dialog component.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Closes any visible modeless window.</summary>
    destructor Destroy; override;
    /// <summary>Shows the dialog modelessly or brings it to the front.</summary>
    /// <returns>Always True when the window is shown.</returns>
    function Execute: Boolean; override;
    /// <summary>Closes the modeless window and releases the form safely.</summary>
    procedure CloseDialog;
    /// <summary>Shows a result count such as "4 matches" or "No matches".</summary>
    /// <param name="ACount">Number of matches.</param>
    procedure SetResultCount(ACount: Integer);
    /// <summary>Shows the current match within the total match count.</summary>
    /// <param name="ACurrent">Current one-based match index.</param>
    /// <param name="ATotal">Total match count.</param>
    procedure SetResult(ACurrent, ATotal: Integer);
    /// <summary>Last status text shown in the dialog.</summary>
    property ResultText: string read FResultText;
    /// <summary>Remembered top-left corner of the modeless window.</summary>
    property Position: TPoint read FPosition write SetPosition;
    /// <summary>Remembered left coordinate; -1 centres the dialog. Public,
    /// not published: TComponent streams its designer position as Left /
    /// Top.</summary>
    property Left: Integer read FLeft write SetLeft;
    /// <summary>Remembered top coordinate; -1 centres the dialog.</summary>
    property Top: Integer read FTop write SetTop;
  published
    /// <summary>Text to find.</summary>
    property FindText: string read FFindText write SetFindText;
    /// <summary>Find options and the last requested action flag.</summary>
    property Options: TERDFindOptions read FOptions write SetOptions;
    /// <summary>Search direction selected by the Up / Down segmented control.</summary>
    property Direction: TERDFindDirection read FDirection write SetDirection
      default ofdDown;
    /// <summary>Optional explicit theme. nil = default theme or VCL style.</summary>
    property Theme;
    /// <summary>Desktop or tablet sizing; follows the theme until set.</summary>
    property Density;
    /// <summary>Follows the theme density when True.</summary>
    property ParentDensity;
    /// <summary>Text in the title strip; empty = the default title.</summary>
    property Title;
    /// <summary>Fires just before the modeless window is shown.</summary>
    property OnShow;
    /// <summary>Fires after the modeless window has been closed.</summary>
    property OnClose;
    /// <summary>Fires for Find next and Enter in the find field.</summary>
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
  end;

  /// <summary>Modeless themed replacement for VCL TReplaceDialog.</summary>
  TERDReplaceDialog = class(TERDFindDialog)
  protected
    /// <summary>Default title used while Title is empty.</summary>
    /// <returns>Dialog title.</returns>
    function DefaultTitle: string; override;
    /// <summary>Marks this dialog as a replace dialog.</summary>
    /// <returns>Always True.</returns>
    function IsReplaceDialog: Boolean; override;
  published
    /// <summary>Replacement text.</summary>
    property ReplaceText: string read FReplaceText write FReplaceText;
    /// <summary>Fires for Replace and Replace all.</summary>
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
  end;

implementation

const
  FIND_DIALOG_WIDTH = 576;
  REPLACE_DIALOG_WIDTH = 460;

{ TERDFindDialog }

constructor TERDFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirection := ofdDown;
  FLeft := -1;
  FTop := -1;
  FPosition := Point(-1, -1);
  FResultText := '';
end;

destructor TERDFindDialog.Destroy;
var
  Window: TERDDialogWindow;
begin
  Window := FWindow;
  FWindow := nil;
  ClearWindowRefs;
  if Window <> nil then
  begin
    Window.OnClose := nil;
    Window.OnDestroy := nil;
    Window.Free;
  end;
  inherited Destroy;
end;

function TERDFindDialog.DefaultTitle: string;
begin
  Result := 'Find';
end;

function TERDFindDialog.IsReplaceDialog: Boolean;
begin
  Result := False;
end;

procedure TERDFindDialog.SetFindText(const AValue: string);
begin
  if FFindText = AValue then
    Exit;
  FFindText := AValue;
  if FFindEdit <> nil then
    FFindEdit.Text := AValue;
end;

procedure TERDFindDialog.SetOptions(AValue: TERDFindOptions);
begin
  if FOptions = AValue then
    Exit;
  FOptions := AValue;
  UpdateControls;
end;

procedure TERDFindDialog.SetDirection(AValue: TERDFindDirection);
begin
  if FDirection = AValue then
    Exit;
  FDirection := AValue;
  if FDirectionControl <> nil then
    FDirectionControl.ItemIndex := Ord(AValue);
end;

procedure TERDFindDialog.SetLeft(AValue: Integer);
begin
  FLeft := AValue;
  FPosition.X := AValue;
  if FWindow <> nil then
    FWindow.Left := AValue;
end;

procedure TERDFindDialog.SetTop(AValue: Integer);
begin
  FTop := AValue;
  FPosition.Y := AValue;
  if FWindow <> nil then
    FWindow.Top := AValue;
end;

procedure TERDFindDialog.SetPosition(const AValue: TPoint);
begin
  FPosition := AValue;
  FLeft := AValue.X;
  FTop := AValue.Y;
  if FWindow <> nil then
    FWindow.SetBounds(AValue.X, AValue.Y, FWindow.Width, FWindow.Height);
end;

procedure TERDFindDialog.ApplyStoredPosition(AWindow: TERDDialogWindow);
begin
  if (FLeft >= 0) and (FTop >= 0) then
  begin
    AWindow.Left := FLeft;
    AWindow.Top := FTop;
  end
  else
    AWindow.CenterOnActiveForm;
end;

procedure TERDFindDialog.SaveWindowPosition;
begin
  if FWindow = nil then
    Exit;
  FLeft := FWindow.Left;
  FTop := FWindow.Top;
  FPosition := Point(FLeft, FTop);
end;

procedure TERDFindDialog.ClearWindowRefs;
begin
  FFindEdit := nil;
  FReplaceEdit := nil;
  FMatchCase := nil;
  FWholeWord := nil;
  FDirectionControl := nil;
  FResultLabel := nil;
  FFindButton := nil;
  FReplaceButton := nil;
  FReplaceAllButton := nil;
  FCloseButton := nil;
end;

function TERDFindDialog.BuildWindow: TERDDialogWindow;
var
  Width, Fields, BodyH, Y, EditH, CheckH: Integer;
  ParentForm: TCustomForm;
  TitleText: string;
begin
  if IsReplaceDialog then
    Width := REPLACE_DIALOG_WIDTH
  else
    Width := FIND_DIALOG_WIDTH;
  Fields := 1;
  if IsReplaceDialog then
    Fields := 2;
  EditH := DensityMetrics(Density).Edit;
  CheckH := System.Math.Max(26, DensityMetrics(Density).Check + 12);
  BodyH := 16 + Fields * (18 + EditH + 12) + CheckH + 8 + 18 +
    DensityMetrics(Density).Segment + 16;
  TitleText := Title;
  if TitleText = '' then
    TitleText := DefaultTitle;
  Result := TERDDialogWindow.CreateWindow(Self, Theme, Density, TitleText);
  Result.SetLayout(Width, BodyH, True);
  Result.OnCloseButton := CloseClick;
  Result.OnClose := WindowClose;
  Result.OnDestroy := WindowDestroy;
  ParentForm := Screen.ActiveCustomForm;
  if (ParentForm <> nil) and (ParentForm <> Result) then
  begin
    Result.PopupMode := pmExplicit;
    Result.PopupParent := ParentForm;
  end;

  Y := 16;
  Result.AddText(16, Y, Width - 32, 'Find what', dtsCaps);
  FFindEdit := TERDEdit.Create(Result);
  FFindEdit.OnKeyDown := FindEditKeyDown;
  Result.Place(FFindEdit, 16, Y + 18, Width - 32, EditH);
  Inc(Y, 18 + EditH + 12);

  if IsReplaceDialog then
  begin
    Result.AddText(16, Y, Width - 32, 'Replace with', dtsCaps);
    FReplaceEdit := TERDEdit.Create(Result);
    FReplaceEdit.OnKeyDown := FindEditKeyDown;
    Result.Place(FReplaceEdit, 16, Y + 18, Width - 32, EditH);
    Inc(Y, 18 + EditH + 12);
  end;

  FMatchCase := TERDCheckBox.Create(Result);
  FMatchCase.Caption := 'Match case';
  Result.Place(FMatchCase, 16, Y, 116, CheckH);
  FWholeWord := TERDCheckBox.Create(Result);
  FWholeWord.Caption := 'Whole word only';
  Result.Place(FWholeWord, 140, Y, 160, CheckH);
  Inc(Y, CheckH + 8);

  Result.AddText(16, Y, 120, 'Direction', dtsCaps);
  FDirectionControl := TERDSegmented.Create(Result);
  FDirectionControl.Items.Add('Up');
  FDirectionControl.Items.Add('Down');
  FDirectionControl.OnChange := DirectionChanged;
  Result.Place(FDirectionControl, 16, Y + 18, 130,
    DensityMetrics(Density).Segment + 8);
  FResultLabel := Result.AddText(Width - 226, Y + 18, 210, FResultText,
    dtsSmall, DensityMetrics(Density).Segment);
  FResultLabel.Alignment := taRightJustify;

  if IsReplaceDialog then
  begin
    FFindButton := Result.AddButton('Find next', bkPrimary, mrNone);
    FFindButton.Default := True;
    FFindButton.OnClick := FindClick;
    FReplaceButton := Result.AddButton('Replace', bkSecondary, mrNone);
    FReplaceButton.OnClick := ReplaceClick;
    FReplaceAllButton := Result.AddButton('Replace all', bkSecondary, mrNone);
    FReplaceAllButton.OnClick := ReplaceClick;
    FCloseButton := Result.AddLeftButton('Close');
    FCloseButton.OnClick := CloseClick;
  end
  else
  begin
    FFindButton := Result.AddButton('Find next', bkPrimary, mrNone);
    FFindButton.Default := True;
    FFindButton.OnClick := FindClick;
    FCloseButton := Result.AddButton('Close', bkSecondary, mrNone);
    FCloseButton.Cancel := True;
    FCloseButton.OnClick := CloseClick;
  end;

  Result.InitialFocus := FFindEdit;
  UpdateControls;
end;

procedure TERDFindDialog.UpdateControls;
begin
  if FFindEdit <> nil then
    FFindEdit.Text := FFindText;
  if FReplaceEdit <> nil then
    FReplaceEdit.Text := FReplaceText;
  if FMatchCase <> nil then
    FMatchCase.Checked := obfoMatchCase in FOptions;
  if FWholeWord <> nil then
    FWholeWord.Checked := obfoWholeWord in FOptions;
  if FDirectionControl <> nil then
    FDirectionControl.ItemIndex := Ord(FDirection);
  UpdateResultLabel;
end;

procedure TERDFindDialog.UpdateFromControls;
begin
  if FFindEdit <> nil then
    FFindText := FFindEdit.Text;
  if FReplaceEdit <> nil then
    FReplaceText := FReplaceEdit.Text;
  FOptions := FOptions - [obfoMatchCase, obfoWholeWord, obfoFindNext,
    obfoReplace, obfoReplaceAll];
  if (FMatchCase <> nil) and FMatchCase.Checked then
    Include(FOptions, obfoMatchCase);
  if (FWholeWord <> nil) and FWholeWord.Checked then
    Include(FOptions, obfoWholeWord);
  if FDirectionControl <> nil then
    if FDirectionControl.ItemIndex = Ord(ofdUp) then
      FDirection := ofdUp
    else
      FDirection := ofdDown;
end;

procedure TERDFindDialog.UpdateResultLabel;
begin
  if FResultLabel <> nil then
    FResultLabel.Caption := FResultText;
end;

procedure TERDFindDialog.DirectionChanged(Sender: TObject);
begin
  if (FDirectionControl <> nil) and
    (FDirectionControl.ItemIndex = Ord(ofdUp)) then
    FDirection := ofdUp
  else
    FDirection := ofdDown;
end;

procedure TERDFindDialog.FindEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    DoFind;
  end;
end;

procedure TERDFindDialog.FindClick(Sender: TObject);
begin
  DoFind;
end;

procedure TERDFindDialog.ReplaceClick(Sender: TObject);
begin
  DoReplace(Sender = FReplaceAllButton);
end;

procedure TERDFindDialog.CloseClick(Sender: TObject);
begin
  CloseDialog;
end;

procedure TERDFindDialog.WindowClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  if not FClosing then
    CloseDialog;
end;

procedure TERDFindDialog.WindowDestroy(Sender: TObject);
begin
  if Sender = FWindow then
  begin
    SaveWindowPosition;
    FWindow := nil;
    ClearWindowRefs;
    if not FClosing then
      DoClose;
  end;
end;

procedure TERDFindDialog.DoFind;
begin
  UpdateFromControls;
  FOptions := FOptions - [obfoReplace, obfoReplaceAll];
  Include(FOptions, obfoFindNext);
  if Assigned(FOnFind) then
    FOnFind(Self);
end;

procedure TERDFindDialog.DoReplace(AAll: Boolean);
begin
  UpdateFromControls;
  FOptions := FOptions - [obfoFindNext, obfoReplace, obfoReplaceAll];
  if AAll then
    Include(FOptions, obfoReplaceAll)
  else
    Include(FOptions, obfoReplace);
  if Assigned(FOnReplace) then
    FOnReplace(Self);
end;

function TERDFindDialog.Execute: Boolean;
begin
  if FWindow <> nil then
  begin
    if FWindow.WindowState = wsMinimized then
      FWindow.WindowState := wsNormal;
    FWindow.Show;
    FWindow.BringToFront;
    if FWindow.CanFocus then
      FWindow.SetFocus;
    Exit(True);
  end;

  FWindow := BuildWindow;
  ApplyStoredPosition(FWindow);
  DoShow;
  FWindow.Show;
  Result := True;
end;

procedure TERDFindDialog.CloseDialog;
var
  Window: TERDDialogWindow;
begin
  Window := FWindow;
  if Window = nil then
    Exit;
  SaveWindowPosition;
  UpdateFromControls;
  FClosing := True;
  try
    FWindow := nil;
    ClearWindowRefs;
    Window.OnClose := nil;
    Window.OnDestroy := nil;
    Window.OnCloseButton := nil;
    DoClose;
    Window.Release;
  finally
    FClosing := False;
  end;
end;

procedure TERDFindDialog.SetResultCount(ACount: Integer);
begin
  if ACount <= 0 then
    FResultText := 'No matches'
  else if ACount = 1 then
    FResultText := '1 match'
  else
    FResultText := Format('%d matches', [ACount]);
  UpdateResultLabel;
end;

procedure TERDFindDialog.SetResult(ACurrent, ATotal: Integer);
begin
  if ATotal <= 0 then
    FResultText := 'No matches'
  else
  begin
    ACurrent := EnsureRange(ACurrent, 1, ATotal);
    FResultText := Format('%d of %d matches', [ACurrent, ATotal]);
  end;
  UpdateResultLabel;
end;

{ TERDReplaceDialog }

function TERDReplaceDialog.DefaultTitle: string;
begin
  Result := 'Replace';
end;

function TERDReplaceDialog.IsReplaceDialog: Boolean;
begin
  Result := True;
end;

end.
