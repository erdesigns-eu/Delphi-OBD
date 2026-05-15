//------------------------------------------------------------------------------
//  OBD.Design.Starters.Flashing
//
//  Flashing starters for the Delphi-OBD wizard:
//
//    simple-flasher   One-shot TOBDFlasher (no pipeline). Good
//                     for hosts that only need RequestDownload /
//                     TransferData / RequestTransferExit without
//                     the gate / checkpoint / signature
//                     ceremony.
//    ecu-uploader     TOBDUploader — read firmware out of an
//                     ECU into a local file.
//    j1939-memory     TOBDJ1939MemoryAccess (DM14 / DM15 / DM16
//                     framing) for heavy-duty platforms that
//                     don't carry UDS.
//
//  All three default AutoExecute = False; the host opts in.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters.Flashing;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

function PasShell(const ABody: string;
  const AContext: TOBDStarterContext): string;
const
  TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    '{BODY}'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
begin
  Result := ExpandTemplate(StringReplace(TPL, '{BODY}', ABody, []), AContext);
end;

{ ---- simple-flasher -------------------------------------------------------- }

function GenerateSimpleFlasher(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Dialogs,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Coding.Flasher;'#13#10 +
    ''#13#10 +
    '// SAFETY: TOBDFlasher is a one-shot ECU flasher. AutoExecute'#13#10 +
    '// defaults False; do not flip it without OnConfirmExecute.'#13#10 +
    '// Read docs/flashing-safety.md.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Flasher:    TOBDFlasher;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtImage:   TEdit;'#13#10 +
    '    btnPick:    TButton;'#13#10 +
    '    btnFlash:   TButton;'#13#10 +
    '    pb:         TProgressBar;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '    dlg:        TOpenDialog;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - simple flasher'''#13#10 +
    '  ClientHeight = 480; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 80; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtImage: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 460; Height = 24'#13#10 +
    '      TextHint = ''Image file (.bin / .hex / .s19)'''#13#10 +
    '    end'#13#10 +
    '    object btnPick: TButton'#13#10 +
    '      Left = 484; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Browse...'''#13#10 +
    '    end'#13#10 +
    '    object btnFlash: TButton'#13#10 +
    '      Left = 586; Top = 12; Width = 100; Height = 28'#13#10 +
    '      Caption = ''Flash'''#13#10 +
    '    end'#13#10 +
    '    object pb: TProgressBar'#13#10 +
    '      Left = 12; Top = 50; Width = 674; Height = 16'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Flasher: TOBDFlasher'#13#10 +
    '    Protocol = Protocol; AutoExecute = False'#13#10 +
    '    Left = 216; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object dlg: TOpenDialog'#13#10 +
    '    Filter = ''Firmware images (*.bin;*.hex;*.s19)|*.bin;*.hex;*.s19|All files (*.*)|*.*'''#13#10 +
    '    Left = 280; Top = 96'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- ecu-uploader ---------------------------------------------------------- }

function GenerateUploader(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Dialogs,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Coding.Uploader;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Uploader:   TOBDUploader;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtOut:     TEdit;'#13#10 +
    '    btnPick:    TButton;'#13#10 +
    '    btnRead:    TButton;'#13#10 +
    '    pb:         TProgressBar;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '    dlg:        TSaveDialog;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - ECU uploader'''#13#10 +
    '  ClientHeight = 480; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 80; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtOut: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 460; Height = 24'#13#10 +
    '      TextHint = ''Output file (.bin)'''#13#10 +
    '    end'#13#10 +
    '    object btnPick: TButton'#13#10 +
    '      Left = 484; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Browse...'''#13#10 +
    '    end'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 586; Top = 12; Width = 100; Height = 28'#13#10 +
    '      Caption = ''Read'''#13#10 +
    '    end'#13#10 +
    '    object pb: TProgressBar'#13#10 +
    '      Left = 12; Top = 50; Width = 674; Height = 16'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Uploader: TOBDUploader'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object dlg: TSaveDialog'#13#10 +
    '    Filter = ''Firmware dump (*.bin)|*.bin|All files (*.*)|*.*'''#13#10 +
    '    Left = 280; Top = 96'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- j1939-memory ---------------------------------------------------------- }

function GenerateJ1939Memory(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.J1939.MemoryAccess;'#13#10 +
    ''#13#10 +
    '// J1939 memory access (DM14 / DM15 / DM16 framing).'#13#10 +
    '// TOBDJ1939MemoryAccess is a helper class, not a TComponent;'#13#10 +
    '// instantiate it from code and feed PGN frames via the host''s'#13#10 +
    '// CAN driver.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtAddr:    TEdit;'#13#10 +
    '    edtLen:     TEdit;'#13#10 +
    '    edtData:    TEdit;'#13#10 +
    '    btnRead:    TButton;'#13#10 +
    '    btnWrite:   TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - J1939 memory access'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 720; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtAddr: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 140; Height = 24'#13#10 +
    '      TextHint = ''Address hex'''#13#10 +
    '    end'#13#10 +
    '    object edtLen: TEdit'#13#10 +
    '      Left = 164; Top = 14; Width = 80; Height = 24'#13#10 +
    '      TextHint = ''Len'''#13#10 +
    '    end'#13#10 +
    '    object edtData: TEdit'#13#10 +
    '      Left = 256; Top = 14; Width = 280; Height = 24'#13#10 +
    '      TextHint = ''Data hex (write)'''#13#10 +
    '    end'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 548; Top = 12; Width = 70; Height = 28'#13#10 +
    '      Caption = ''Read'''#13#10 +
    '    end'#13#10 +
    '    object btnWrite: TButton'#13#10 +
    '      Left = 626; Top = 12; Width = 70; Height = 28'#13#10 +
    '      Caption = ''Write'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- registration ---------------------------------------------------------- }

procedure RegisterAll;
var
  S: TOBDStarter;
begin
  S := Default(TOBDStarter);
  S.Id := 'simple-flasher';
  S.Title := 'Simple flasher (one-shot)';
  S.Description :=
    'Quick TOBDFlasher project — RequestDownload / TransferData / ' +
    'RequestTransferExit without the full pipeline ceremony. AutoExecute = ' +
    'False. Use the full flash-session starter when you need the ' +
    'voltage gate / checkpoint / signature stack.';
  S.Category := 'Flashing';
  S.Generate := GenerateSimpleFlasher;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'ecu-uploader';
  S.Title := 'ECU uploader (read firmware)';
  S.Description :=
    'Drops TOBDUploader on a form with an output-file picker and a Read ' +
    'button. Reads firmware out of an ECU into a local file, with progress.';
  S.Category := 'Flashing';
  S.Generate := GenerateUploader;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'j1939-memory';
  S.Title := 'J1939 memory access (DM14-18)';
  S.Description :=
    'Heavy-duty memory access via J1939 DM14 / DM15 / DM16 framing. ' +
    'Drops the connection chain on the form and shows where to instantiate ' +
    'TOBDJ1939MemoryAccess (a helper class, not a TComponent).';
  S.Category := 'Flashing';
  S.Generate := GenerateJ1939Memory;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
