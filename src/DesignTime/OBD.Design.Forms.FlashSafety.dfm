object OBDFlashSafetyDlg: TOBDFlashSafetyDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Delphi-OBD - flashing safety reminder'
  ClientHeight = 280
  ClientWidth = 540
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object pnlBody: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 24
    Padding.Top = 20
    Padding.Right = 24
    Padding.Bottom = 12
    TabOrder = 0
    object lblWarning: TLabel
      AlignWithMargins = True
      Left = 24
      Top = 20
      Width = 492
      Height = 200
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption =
        'This component performs destructive ECU operations. Misuse can ' +
        'permanently brick an ECU.'#13#10#13#10 +
        'Before deploying:'#13#10 +
        '   - Wire OnConfirmExecute on the host form.'#13#10 +
        '   - Leave AutoExecute = False until you really mean it.'#13#10 +
        '   - Provide a TOBDVoltageGate.'#13#10 +
        '   - Read docs/flashing-safety.md before integrating.'#13#10#13#10 +
        'Click "Open safety guide" to read the full document.'
      WordWrap = True
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 232
    Width = 540
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 24
    Padding.Top = 8
    Padding.Right = 24
    Padding.Bottom = 12
    TabOrder = 1
    object btnOpenGuide: TButton
      AlignWithMargins = True
      Left = 24
      Top = 8
      Width = 180
      Height = 28
      Align = alLeft
      Caption = 'Open safety guide...'
      TabOrder = 0
      OnClick = btnOpenGuideClick
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 432
      Top = 8
      Width = 84
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
