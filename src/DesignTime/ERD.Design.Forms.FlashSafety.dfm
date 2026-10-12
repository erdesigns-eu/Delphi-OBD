object OBDFlashSafetyDlg: TOBDFlashSafetyDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Delphi-OBD - flashing safety guide'
  ClientHeight = 600
  ClientWidth = 720
  Color = clWindow
  Constraints.MinHeight = 420
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlBody: TPanel
    Left = 0
    Top = 0
    Width = 720
    Height = 552
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
      Width = 672
      Height = 105
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 
        'This component performs destructive ECU operations. Misuse can p' +
        'ermanently brick an ECU.'#13#10#13#10'Before deploying:'#13#10'   - Wire OnConfi' +
        'rmExecute on the host form.'#13#10'   - Leave AutoExecute = False unti' +
        'l you really mean it.'#13#10'   - Provide a TOBDVoltageGate.'#13#10'   - Rea' +
        'd the full safety guide below before integrating.'
      WordWrap = True
    end
    object memGuide: TMemo
      AlignWithMargins = True
      Left = 24
      Top = 137
      Width = 672
      Height = 403
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 552
    Width = 720
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 24
    Padding.Top = 8
    Padding.Right = 24
    Padding.Bottom = 12
    TabOrder = 1
    object btnClose: TButton
      AlignWithMargins = True
      Left = 612
      Top = 8
      Width = 84
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
