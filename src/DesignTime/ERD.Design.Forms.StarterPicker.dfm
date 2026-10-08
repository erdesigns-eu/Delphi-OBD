object OBDStarterPickerDlg: TOBDStarterPickerDlg
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Delphi-OBD - new project starter'
  ClientHeight = 560
  ClientWidth = 820
  Color = clWindow
  Constraints.MinHeight = 460
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 820
    Height = 504
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlStepStarter: TPanel
      Left = 0
      Top = 0
      Width = 820
      Height = 504
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlStepStarterLeft: TPanel
        Left = 0
        Top = 0
        Width = 320
        Height = 504
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
          Caption = 'Step 1 of 3 - choose a starter:'
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
          Height = 452
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
        Height = 504
        Beveled = True
      end
      object pnlStepStarterRight: TPanel
        Left = 326
        Top = 0
        Width = 494
        Height = 504
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
          Margins.Bottom = 6
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
          Top = 44
          Width = 470
          Height = 448
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          AutoSize = False
          Caption = ''
          WordWrap = True
        end
      end
    end
    object pnlStepOptions: TPanel
      Left = 0
      Top = 0
      Width = 820
      Height = 504
      Align = alClient
      BevelOuter = bvNone
      Padding.Left = 16
      Padding.Top = 16
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 1
      Visible = False
      object lblOptionsHeader: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 788
        Height = 18
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alTop
        Caption = 'Step 2 of 3 - configure options:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object sbOptions: TScrollBox
        AlignWithMargins = True
        Left = 16
        Top = 42
        Width = 788
        Height = 450
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
    object pnlStepTarget: TPanel
      Left = 0
      Top = 0
      Width = 820
      Height = 504
      Align = alClient
      BevelOuter = bvNone
      Padding.Left = 16
      Padding.Top = 16
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 2
      Visible = False
      object lblTargetHeader: TLabel
        AlignWithMargins = True
        Left = 16
        Top = 16
        Width = 788
        Height = 18
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alTop
        Caption = 'Step 3 of 3 - project location and names:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pnlTargetInputs: TPanel
        AlignWithMargins = True
        Left = 16
        Top = 42
        Width = 788
        Height = 200
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 12
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblTarget: TLabel
          Left = 0
          Top = 8
          Width = 92
          Height = 15
          Caption = 'Target folder:'
        end
        object edtTargetDir: TEdit
          Left = 0
          Top = 28
          Width = 600
          Height = 24
          TabOrder = 0
        end
        object btnBrowse: TButton
          Left = 608
          Top = 26
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
          Width = 320
          Height = 24
          TabOrder = 2
        end
        object lblUnit: TLabel
          Left = 360
          Top = 72
          Width = 100
          Height = 15
          Caption = 'Form unit name:'
        end
        object edtUnit: TEdit
          Left = 360
          Top = 92
          Width = 240
          Height = 24
          TabOrder = 3
        end
      end
      object pnlSummary: TPanel
        AlignWithMargins = True
        Left = 16
        Top = 254
        Width = 788
        Height = 238
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object lblSummary: TLabel
          Align = alTop
          Caption = 'Summary'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          Height = 16
          ParentFont = False
        end
        object memSummary: TMemo
          AlignWithMargins = True
          Left = 0
          Top = 22
          Width = 788
          Height = 216
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 504
    Width = 820
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 16
    Padding.Top = 12
    Padding.Right = 16
    Padding.Bottom = 16
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 708
      Top = 12
      Width = 96
      Height = 28
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnFinish: TButton
      AlignWithMargins = True
      Left = 603
      Top = 12
      Width = 96
      Height = 28
      Align = alRight
      Caption = 'Finish'
      Enabled = False
      TabOrder = 2
      OnClick = btnFinishClick
    end
    object btnNext: TButton
      AlignWithMargins = True
      Left = 498
      Top = 12
      Width = 96
      Height = 28
      Align = alRight
      Caption = 'Next >'
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnBack: TButton
      AlignWithMargins = True
      Left = 393
      Top = 12
      Width = 96
      Height = 28
      Align = alRight
      Caption = '< Back'
      Enabled = False
      TabOrder = 0
      OnClick = btnBackClick
    end
  end
end
