object OBDLiveTestDlg: TOBDLiveTestDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Delphi-OBD - live test'
  ClientHeight = 380
  ClientWidth = 600
  Color = clWindow
  Constraints.MinHeight = 280
  Constraints.MinWidth = 480
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 12
    Padding.Right = 16
    Padding.Bottom = 8
    TabOrder = 0
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 12
      Width = 568
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = '(test title)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblTarget: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 34
      Width = 568
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = '(target)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object memLog: TMemo
    AlignWithMargins = True
    Left = 16
    Top = 68
    Width = 568
    Height = 244
    Margins.Left = 16
    Margins.Top = 4
    Margins.Right = 16
    Margins.Bottom = 4
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 332
    Width = 600
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 8
    Padding.Right = 16
    Padding.Bottom = 12
    TabOrder = 2
    object pnlStatus: TPanel
      AlignWithMargins = True
      Left = 16
      Top = 8
      Width = 240
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      Color = 14540253
      ParentBackground = False
      TabOrder = 0
      object lblStatus: TLabel
        Left = 0
        Top = 0
        Width = 240
        Height = 28
        Align = alClient
        Alignment = taCenter
        Caption = 'Idle'
        Layout = tlCenter
      end
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 488
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 2
    end
    object btnRun: TButton
      AlignWithMargins = True
      Left = 386
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Caption = 'Run'
      Default = True
      TabOrder = 1
      OnClick = btnRunClick
    end
  end
end
