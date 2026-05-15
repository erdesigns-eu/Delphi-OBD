object OBDPipelineValidateDlg: TOBDPipelineValidateDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Delphi-OBD - flash pipeline configuration'
  ClientHeight = 360
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
    object lblHeader: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 12
      Width = 568
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Configuration review'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblComponent: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 31
      Width = 568
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = '(component)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object lstIssues: TListBox
    AlignWithMargins = True
    Left = 16
    Top = 68
    Width = 568
    Height = 224
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
    ItemHeight = 15
    ParentFont = False
    TabOrder = 1
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 312
    Width = 600
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 8
    Padding.Right = 16
    Padding.Bottom = 12
    TabOrder = 2
    object btnOpenGuide: TButton
      AlignWithMargins = True
      Left = 16
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
      Left = 488
      Top = 8
      Width = 96
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
