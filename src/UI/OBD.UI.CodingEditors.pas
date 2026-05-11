//------------------------------------------------------------------------------
//  OBD.UI.CodingEditors
//
//  Coding-workflow editors:
//
//    TOBDCodingDiffViewer  Old vs new bytes side-by-side, hex
//                          + ASCII, byte-level highlight on
//                          changes.
//    TOBDLabelFileEditor   VAG-style label-file editor — long-
//                          coding string with named labels.
//                          TMemo-backed; host populates labels.
//    TOBDAdaptationEditor  Adaptation-channel read/write
//                          panel. Channel + value inputs with
//                          read / write buttons.
//    TOBDLongCodingEditor  VAG long-coding bit-flip editor.
//                          Each byte is split into eight
//                          checkboxes.
//    TOBDSeedKeyDebugger   Security-access seed/key trace
//                          panel: seed bytes, computed key,
//                          accept / reject result.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.CodingEditors;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>Side-by-side hex diff. Host calls
  /// <see cref="LoadDiff"/> with old + new buffers; the
  /// visual paints both columns with byte-level change
  /// highlighting.</summary>
  TOBDCodingDiffViewer = class(TOBDCustomControl)
  strict private
    FOld:    TBytes;
    FNew:    TBytes;
    FFont:   TFont;
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadDiff(const AOld, ANew: TBytes);
    procedure Clear;
    function  ChangeCount: Integer;
  published
    property MonoFont: TFont read FFont write SetFontA;
  end;

  /// <summary>VAG label-file editor (text-based). Memo-backed
  /// view of the long-coding string with named labels in a
  /// header.</summary>
  TOBDLabelFileEditor = class(TOBDCustomControl)
  strict private
    FCoding:    string;
    FLabelText: string;
    FFont:      TFont;
    procedure SetCoding(const AValue: string);
    procedure SetLabelText(const AValue: string);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Long-coding hex string (e.g. "0102FF...").
    /// </summary>
    property Coding:    string read FCoding    write SetCoding;
    /// <summary>Free-text label block (one bit per line, etc.)
    /// </summary>
    property LabelText: string read FLabelText write SetLabelText;
    property MonoFont: TFont read FFont write SetFontA;
  end;

  /// <summary>Fires when the host requests an adaptation read.
  /// </summary>
  TOBDAdaptationReadEvent = procedure(Sender: TObject;
    AChannel: Word; out AValue: Word; out AError: string) of object;

  /// <summary>Fires when the host requests an adaptation write.
  /// </summary>
  TOBDAdaptationWriteEvent = procedure(Sender: TObject;
    AChannel: Word; AValue: Word; out AError: string) of object;

  /// <summary>Adaptation-channel editor (paint-driven).
  /// Channel + value inputs + read / write hot-spots fire
  /// the matching events on click.</summary>
  TOBDAdaptationEditor = class(TOBDCustomControl)
  strict private
    FChannel:    Word;
    FValue:      Word;
    FStatus:     string;
    FFont:       TFont;
    FReadRect:   TRect;
    FWriteRect:  TRect;
    FOnRead:     TOBDAdaptationReadEvent;
    FOnWrite:    TOBDAdaptationWriteEvent;
    procedure SetChannel(AValue: Word);
    procedure SetValue(AValue: Word);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure DoRead;
    procedure DoWrite;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Channel: Word read FChannel write SetChannel default 0;
    property Value: Word read FValue write SetValue default 0;
    property MonoFont: TFont read FFont write SetFontA;
    property OnRead: TOBDAdaptationReadEvent
      read FOnRead write FOnRead;
    property OnWrite: TOBDAdaptationWriteEvent
      read FOnWrite write FOnWrite;
  end;

  /// <summary>VAG long-coding bit-flip editor. Host loads
  /// bytes via <see cref="LoadCoding"/>; each byte is split
  /// into eight bit boxes the user toggles via click. The
  /// resulting bytes are exposed via <see cref="Coding"/>.
  /// </summary>
  TOBDLongCodingEditor = class(TOBDCustomControl)
  strict private
    FBytes: TBytes;
    FFont:  TFont;
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  HitTest(X, Y: Integer; out AByte, ABit: Integer): Boolean;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadCoding(const ABytes: TBytes);
    function  Coding: TBytes;
  published
    property MonoFont: TFont read FFont write SetFontA;
  end;

  /// <summary>Security-access seed/key debugger. Host pushes
  /// the seed bytes; computed key + accept / reject status
  /// surface via <see cref="LoadResult"/>.</summary>
  TOBDSeedKeyDebugger = class(TOBDCustomControl)
  strict private
    FSeed:     TBytes;
    FKey:      TBytes;
    FAccepted: Boolean;
    FFont:     TFont;
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    function  HexOf(const AB: TBytes): string;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Loads the seed and key for display + the
    /// ECU's accept / reject verdict.</summary>
    procedure LoadResult(const ASeed, AKey: TBytes;
      AAccepted: Boolean);
    procedure Clear;
    property Seed:     TBytes  read FSeed;
    property Key:      TBytes  read FKey;
    property Accepted: Boolean read FAccepted;
  published
    property MonoFont: TFont read FFont write SetFontA;
  end;

implementation

function ColorToARGB(AColor: TColor; AAlpha: Byte = 255): ARGB; inline;
var Rgb: Cardinal;
begin
  Rgb := ColorToRGB(AColor);
  Result := MakeColor(AAlpha,
    GetRValue(Rgb), GetGValue(Rgb), GetBValue(Rgb));
end;

function HexBytes(const AB: TBytes): string;
var I: Integer;
begin
  Result := '';
  for I := 0 to High(AB) do
  begin
    if I > 0 then Result := Result + ' ';
    Result := Result + Format('%2.2X', [AB[I]]);
  end;
end;

{ ---- TOBDCodingDiffViewer ----------------------------------------- }

constructor TOBDCodingDiffViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 480;
  Height := 200;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDCodingDiffViewer.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDCodingDiffViewer.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDCodingDiffViewer.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDCodingDiffViewer.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDCodingDiffViewer.LoadDiff(const AOld, ANew: TBytes);
begin
  FOld := Copy(AOld);
  FNew := Copy(ANew);
  NotifyBindings;
  Repaint;
end;

procedure TOBDCodingDiffViewer.Clear;
begin
  SetLength(FOld, 0);
  SetLength(FNew, 0);
  NotifyBindings;
  Repaint;
end;

function TOBDCodingDiffViewer.ChangeCount: Integer;
var
  I, N: Integer;
begin
  Result := 0;
  N := System.Math.Min(Length(FOld), Length(FNew));
  for I := 0 to N - 1 do
    if FOld[I] <> FNew[I] then Inc(Result);
  Inc(Result, Abs(Length(FNew) - Length(FOld)));
end;

procedure TOBDCodingDiffViewer.PaintControl(ACanvas: TCanvas);
var
  Pad, RowH, Y, I, MaxN, ColW, OldX, NewX: Integer;
  S: string;
  Changed: Boolean;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  Pad := ScaleValue(8);
  RowH := ACanvas.TextHeight('M') + ScaleValue(2);
  ColW := (Width - 2 * Pad) div 2;
  OldX := Pad;
  NewX := Pad + ColW + ScaleValue(8);
  ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(OldX, Pad - RowH, 'old');
  ACanvas.TextOut(NewX, Pad - RowH, 'new');
  Y := Pad;
  MaxN := System.Math.Max(Length(FOld), Length(FNew));
  for I := 0 to MaxN - 1 do
  begin
    if Y + RowH > Height then Break;
    Changed := False;
    if (I < Length(FOld)) and (I < Length(FNew)) then
      Changed := FOld[I] <> FNew[I]
    else
      Changed := True;
    if I < Length(FOld) then
      S := Format('%4.4X  %2.2X', [I, FOld[I]])
    else
      S := Format('%4.4X  --', [I]);
    if Changed then ACanvas.Font.Color := Palette.Warning
    else            ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(OldX, Y, S);
    if I < Length(FNew) then
      S := Format('%4.4X  %2.2X', [I, FNew[I]])
    else
      S := Format('%4.4X  --', [I]);
    if Changed then ACanvas.Font.Color := Palette.Success
    else            ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(NewX, Y, S);
    Inc(Y, RowH);
  end;
end;

{ ---- TOBDLabelFileEditor ------------------------------------------ }

constructor TOBDLabelFileEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 200;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDLabelFileEditor.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDLabelFileEditor.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLabelFileEditor.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDLabelFileEditor.SetCoding(const AValue: string);
begin
  if FCoding = AValue then Exit;
  FCoding := AValue; NotifyBindings; Repaint;
end;

procedure TOBDLabelFileEditor.SetLabelText(const AValue: string);
begin
  if FLabelText = AValue then Exit;
  FLabelText := AValue; Repaint;
end;

procedure TOBDLabelFileEditor.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDLabelFileEditor.PaintControl(ACanvas: TCanvas);
var
  Pad, Y: Integer;
  Lines: TArray<string>;
  S: string;
begin
  Pad := ScaleValue(8);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveAccent;
  ACanvas.TextOut(Pad, Pad, 'coding: ' + FCoding);
  ACanvas.Font.Color := EffectiveForeground;
  Y := Pad + ACanvas.TextHeight('M') * 2;
  Lines := FLabelText.Split([sLineBreak]);
  for S in Lines do
  begin
    if Y + ACanvas.TextHeight('M') > Height then Break;
    ACanvas.TextOut(Pad, Y, S);
    Inc(Y, ACanvas.TextHeight('M') + ScaleValue(1));
  end;
end;

{ ---- TOBDAdaptationEditor ----------------------------------------- }

constructor TOBDAdaptationEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 110;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDAdaptationEditor.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDAdaptationEditor.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDAdaptationEditor.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDAdaptationEditor.SetChannel(AValue: Word);
begin
  if FChannel = AValue then Exit;
  FChannel := AValue; NotifyBindings; Repaint;
end;

procedure TOBDAdaptationEditor.SetValue(AValue: Word);
begin
  if FValue = AValue then Exit;
  FValue := AValue; NotifyBindings; Repaint;
end;

procedure TOBDAdaptationEditor.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDAdaptationEditor.DoRead;
var
  V: Word;
  Err: string;
begin
  V := 0;
  if Assigned(FOnRead) then
    try
      FOnRead(Self, FChannel, V, Err);
      if Err = '' then
      begin
        FValue := V;
        FStatus := Format('read OK = 0x%4.4X', [V]);
      end
      else
        FStatus := 'read error: ' + Err;
    except
      on E: Exception do
        FStatus := 'read raised: ' + E.Message;
    end
  else
    FStatus := 'no OnRead handler';
  NotifyBindings;
  Repaint;
end;

procedure TOBDAdaptationEditor.DoWrite;
var
  Err: string;
begin
  if Assigned(FOnWrite) then
    try
      FOnWrite(Self, FChannel, FValue, Err);
      if Err = '' then FStatus := 'write OK'
      else             FStatus := 'write error: ' + Err;
    except
      on E: Exception do
        FStatus := 'write raised: ' + E.Message;
    end
  else
    FStatus := 'no OnWrite handler';
  NotifyBindings;
  Repaint;
end;

procedure TOBDAdaptationEditor.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if PtInRect(FReadRect,  Point(X, Y)) then DoRead
  else if PtInRect(FWriteRect, Point(X, Y)) then DoWrite;
end;

procedure TOBDAdaptationEditor.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, ButW, ButH, Y: Integer;
  R: TGPRectF;

  procedure DrawButton(const ARect: TRect; const ACap: string;
    AAccent: TColor);
  begin
    R.X := ARect.Left;
    R.Y := ARect.Top;
    R.Width  := ARect.Width;
    R.Height := ARect.Height;
    Brush := TGPSolidBrush.Create(ColorToARGB(AAccent));
    Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
    try
      Graphics.FillRectangle(Brush, R);
      Graphics.DrawRectangle(Pen, R);
    finally
      Pen.Free;
      Brush.Free;
    end;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := FFont;
    ACanvas.Font.Color := clWhite;
    ACanvas.TextOut(
      ARect.Left + (ARect.Width  - ACanvas.TextWidth(ACap)) div 2,
      ARect.Top  + (ARect.Height - ACanvas.TextHeight(ACap)) div 2,
      ACap);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  Pad := ScaleValue(10);
  Y := Pad;
  ACanvas.TextOut(Pad, Y,
    Format('channel %d  value 0x%4.4X', [FChannel, FValue]));
  Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(8));

  ButW := ScaleValue(80);
  ButH := ScaleValue(28);
  FReadRect  := Rect(Pad, Y, Pad + ButW, Y + ButH);
  FWriteRect := Rect(Pad + ButW + ScaleValue(10), Y,
    Pad + 2 * ButW + ScaleValue(10), Y + ButH);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawButton(FReadRect,  'Read',  EffectiveAccent);
    DrawButton(FWriteRect, 'Write', Palette.Warning);
  finally
    Graphics.Free;
  end;

  if FStatus <> '' then
  begin
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y + ButH + ScaleValue(8), FStatus);
  end;
end;

{ ---- TOBDLongCodingEditor ---------------------------------------- }

constructor TOBDLongCodingEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 220;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 9;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDLongCodingEditor.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDLongCodingEditor.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLongCodingEditor.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDLongCodingEditor.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDLongCodingEditor.LoadCoding(const ABytes: TBytes);
begin
  FBytes := Copy(ABytes);
  NotifyBindings;
  Repaint;
end;

function TOBDLongCodingEditor.Coding: TBytes;
begin
  Result := Copy(FBytes);
end;

function TOBDLongCodingEditor.HitTest(X, Y: Integer;
  out AByte, ABit: Integer): Boolean;
var
  Pad, RowH, BoxW: Integer;
  Bx, By: Integer;
begin
  Result := False;
  Pad := ScaleValue(8);
  RowH := ScaleValue(20);
  BoxW := ScaleValue(28);
  if Y < Pad then Exit;
  By := (Y - Pad) div RowH;
  if (By < 0) or (By >= Length(FBytes)) then Exit;
  if X < Pad + ScaleValue(48) then Exit;
  Bx := (X - Pad - ScaleValue(48)) div BoxW;
  if (Bx < 0) or (Bx > 7) then Exit;
  AByte := By;
  ABit  := 7 - Bx;        // bit 7 leftmost
  Result := True;
end;

procedure TOBDLongCodingEditor.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  B, Bit: Integer;
begin
  inherited;
  if Button <> mbLeft then Exit;
  if HitTest(X, Y, B, Bit) then
  begin
    FBytes[B] := FBytes[B] xor (1 shl Bit);
    NotifyBindings;
    Repaint;
  end;
end;

procedure TOBDLongCodingEditor.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  Pad, RowH, BoxW, BoxH, I, Bit: Integer;
  X, Y: Integer;
  R: TGPRectF;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  Pad := ScaleValue(8);
  RowH := ScaleValue(20);
  BoxW := ScaleValue(28);
  BoxH := ScaleValue(16);
  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    for I := 0 to High(FBytes) do
    begin
      Y := Pad + I * RowH;
      if Y + BoxH > Height then Break;
      ACanvas.Font.Color := EffectiveForeground;
      ACanvas.TextOut(Pad, Y,
        Format('B%2.2d %2.2X', [I, FBytes[I]]));
      for Bit := 7 downto 0 do
      begin
        X := Pad + ScaleValue(48) + (7 - Bit) * BoxW;
        R.X := X;
        R.Y := Y;
        R.Width  := BoxW - ScaleValue(2);
        R.Height := BoxH;
        if (FBytes[I] and (1 shl Bit)) <> 0 then
          Brush := TGPSolidBrush.Create(
            ColorToARGB(EffectiveAccent))
        else
          Brush := TGPSolidBrush.Create(
            ColorToARGB(Palette.NeutralLight));
        try
          Graphics.FillRectangle(Brush, R);
        finally
          Brush.Free;
        end;
        Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
        try
          Graphics.DrawRectangle(Pen, R);
        finally
          Pen.Free;
        end;
        ACanvas.Font.Color := clBlack;
        ACanvas.TextOut(X + ScaleValue(8), Y, IntToStr(Bit));
      end;
    end;
  finally
    Graphics.Free;
  end;
end;

{ ---- TOBDSeedKeyDebugger ---------------------------------------- }

constructor TOBDSeedKeyDebugger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 100;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDSeedKeyDebugger.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDSeedKeyDebugger.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDSeedKeyDebugger.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDSeedKeyDebugger.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TOBDSeedKeyDebugger.HexOf(const AB: TBytes): string;
begin
  Result := HexBytes(AB);
end;

procedure TOBDSeedKeyDebugger.LoadResult(const ASeed, AKey: TBytes;
  AAccepted: Boolean);
begin
  FSeed := Copy(ASeed);
  FKey  := Copy(AKey);
  FAccepted := AAccepted;
  NotifyBindings;
  Repaint;
end;

procedure TOBDSeedKeyDebugger.Clear;
begin
  SetLength(FSeed, 0);
  SetLength(FKey, 0);
  FAccepted := False;
  NotifyBindings;
  Repaint;
end;

procedure TOBDSeedKeyDebugger.PaintControl(ACanvas: TCanvas);
var
  Pad, Y: Integer;
  Verdict: string;
  Col: TColor;
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  Pad := ScaleValue(8);
  Y := Pad;
  ACanvas.TextOut(Pad, Y, 'seed: ' + HexOf(FSeed));
  Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(2));
  ACanvas.TextOut(Pad, Y, 'key:  ' + HexOf(FKey));
  Inc(Y, ACanvas.TextHeight('Mg') + ScaleValue(6));
  if Length(FSeed) = 0 then Exit;
  if FAccepted then
  begin
    Col := Palette.Success;
    Verdict := 'ACCEPTED';
  end
  else
  begin
    Col := Palette.Danger;
    Verdict := 'REJECTED';
  end;
  ACanvas.Font.Color := Col;
  ACanvas.TextOut(Pad, Y, Verdict);
end;

end.
