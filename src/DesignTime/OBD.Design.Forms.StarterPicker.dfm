object OBDStarterPickerDlg: TOBDStarterPickerDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Delphi-OBD - new project starter'
  ClientHeight = 540
  ClientWidth = 820
  Color = clWindow
  Constraints.MinHeight = 440
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 492
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 16
    Padding.Right = 8
    Padding.Bottom = 12
    TabOrder = 0
    object lblPickHeader: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 16
      Width = 296
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Choose a starter:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object tvStarters: TTreeView
      AlignWithMargins = True
      Left = 16
      Top = 40
      Width = 296
      Height = 440
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      ShowRoot = False
      TabOrder = 0
      OnChange = tvStartersChange
    end
  end
  object splPane: TSplitter
    Left = 320
    Top = 0
    Width = 6
    Height = 492
    Beveled = True
  end
  object pnlRight: TPanel
    Left = 326
    Top = 0
    Width = 494
    Height = 492
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 16
    Padding.Right = 16
    Padding.Bottom = 12
    TabOrder = 1
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 16
      Width = 470
      Height = 22
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 4
      Align = alTop
      Caption = '(select a starter)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblDescription: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 42
      Width = 470
      Height = 100
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 12
      Align = alTop
      AutoSize = False
      Caption = ''
      WordWrap = True
    end
    object pnlInputs: TPanel
      Left = 8
      Top = 154
      Width = 470
      Height = 326
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 8
      TabOrder = 0
      object lblTarget: TLabel
        Left = 0
        Top = 12
        Width = 92
        Height = 15
        Caption = 'Target folder:'
      end
      object edtTargetDir: TEdit
        Left = 0
        Top = 32
        Width = 380
        Height = 24
        TabOrder = 0
      end
      object btnBrowse: TButton
        Left = 388
        Top = 30
        Width = 80
        Height = 28
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object lblProj: TLabel
        Left = 0
        Top = 72
        Width = 90
        Height = 15
        Caption = 'Project name:'
      end
      object edtProj: TEdit
        Left = 0
        Top = 92
        Width = 240
        Height = 24
        TabOrder = 2
        OnChange = edtProjChange
      end
      object lblUnit: TLabel
        Left = 0
        Top = 132
        Width = 100
        Height = 15
        Caption = 'Form unit name:'
      end
      object edtUnit: TEdit
        Left = 0
        Top = 152
        Width = 240
        Height = 24
        TabOrder = 3
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 492
    Width = 820
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 8
    Padding.Right = 16
    Padding.Bottom = 12
    TabOrder = 2
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 708
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnCreate: TButton
      AlignWithMargins = True
      Left = 603
      Top = 8
      Width = 96
      Height = 28
      Align = alRight
      Caption = 'Create'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = btnCreateClick
    end
  end
end
