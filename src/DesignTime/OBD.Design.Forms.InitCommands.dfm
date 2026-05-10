object OBDInitCommandsDlg: TOBDInitCommandsDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'TOBDAdapter - InitCommands'
  ClientHeight = 460
  ClientWidth = 760
  Color = clWindow
  Constraints.MinHeight = 360
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlPalette: TPanel
    Left = 0
    Top = 0
    Width = 280
    Height = 412
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 12
    Padding.Top = 12
    Padding.Right = 6
    Padding.Bottom = 8
    TabOrder = 0
    object lblPalette: TLabel
      AlignWithMargins = True
      Left = 12
      Top = 12
      Width = 262
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Common AT / ST commands (double-click to insert):'
    end
    object lstPalette: TListBox
      AlignWithMargins = True
      Left = 12
      Top = 35
      Width = 262
      Height = 369
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ItemHeight = 15
      ParentFont = False
      TabOrder = 0
      OnDblClick = lstPaletteDblClick
    end
  end
  object splPane: TSplitter
    Left = 280
    Top = 0
    Width = 6
    Height = 412
    Beveled = True
  end
  object pnlEditor: TPanel
    Left = 286
    Top = 0
    Width = 474
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 6
    Padding.Top = 12
    Padding.Right = 12
    Padding.Bottom = 8
    TabOrder = 1
    object lblEditor: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 12
      Width = 456
      Height = 15
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = 'Init script - one command per line. Lines starting with // or ; are comments.'
    end
    object memScript: TMemo
      AlignWithMargins = True
      Left = 6
      Top = 35
      Width = 456
      Height = 369
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 412
    Width = 760
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 12
    Padding.Top = 8
    Padding.Right = 12
    Padding.Bottom = 12
    TabOrder = 2
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 652
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      AlignWithMargins = True
      Left = 547
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
