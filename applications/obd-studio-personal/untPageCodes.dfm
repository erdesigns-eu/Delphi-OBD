object frmPageCodes: TfrmPageCodes
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  TabOrder = 0
  object scMain: TOBDScrollBox
    Left = 0
    Top = 0
    Width = 1208
    Height = 660
    Align = alClient
    Padding.Left = 12
    Padding.Top = 12
    Padding.Right = 12
    Padding.Bottom = 12
    TabOrder = 0
    object pnlHeader: TPanel
      Left = 12
      Top = 12
      Width = 1184
      Height = 72
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
      object lblTitle: TOBDDialogText
        Left = 0
        Top = 2
        Width = 300
        Height = 26
        Caption = 'Trouble codes'
        Style = dtsHeading
      end
      object lblSubtitle: TOBDDialogText
        Left = 0
        Top = 33
        Width = 560
        Height = 20
        Caption = 'Stored, pending and permanent faults, freeze frame and UDS details.'
        Style = dtsSubtle
      end
      object segView: TOBDSegmented
        Left = 430
        Top = 20
        Width = 278
        Height = 32
        Items.Strings = (
          'Classic OBD'
          'All modules (UDS)')
        ItemIndex = 0
        OnChange = segViewChange
        TabOrder = 0
      end
      object btnSave: TOBDButton
        Left = 720
        Top = 20
        Width = 114
        Height = 32
        Caption = 'Save report'
        Kind = bkSecondary
        Glyph = glSave
        OnClick = btnSaveClick
        TabOrder = 1
      end
      object btnClear: TOBDButton
        Left = 844
        Top = 20
        Width = 118
        Height = 32
        Caption = 'Clear codes'
        Kind = bkDangerOutline
        Glyph = glClear
        OnClick = btnClearClick
        TabOrder = 2
      end
      object btnRead: TOBDButton
        Left = 972
        Top = 20
        Width = 110
        Height = 32
        Caption = 'Read codes'
        Glyph = glRead
        OnClick = btnReadClick
        TabOrder = 3
      end
    end
    object banOffline: TOBDBanner
      AlignWithMargins = True
      Left = 12
      Top = 96
      Width = 1184
      Height = 56
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Kind = bnWarning
      Title = 'Connect to your car'
      Text = 'Open the Adapter page and connect before reading trouble codes.'
      TabOrder = 1
    end
    object pnlClassic: TPanel
      AlignWithMargins = True
      Left = 12
      Top = 164
      Width = 1184
      Height = 560
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 2
      object pnlFreeze: TPanel
        AlignWithMargins = True
        Left = 744
        Top = 0
        Width = 440
        Height = 560
        Margins.Left = 12
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 1
        object freezeView: TOBDFreezeFrameView
          Left = 0
          Top = 0
          Width = 440
          Height = 560
          Align = alClient
          DtcCode = ''
          Title = 'Freeze frame'
          Subtitle = 'Snapshot recorded when the code was stored.'
          HeaderInfo = 'Read codes to load the freeze frame.'
          CompareLive = False
          TabOrder = 0
        end
      end
      object pnlClassicLeft: TPanel
        Left = 0
        Top = 0
        Width = 732
        Height = 560
        Align = alClient
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 0
        object dtcPanel: TOBDDtcPanel
          Left = 0
          Top = 0
          Width = 732
          Height = 324
          Align = alTop
          TitleCaption = 'Codes'
          ReadCaption = 'Read codes'
          ClearCaption = 'Clear'
          ConfirmClearCaption = 'Clear now'
          CancelCaption = 'Cancel'
          ShowEcu = True
          OnReadCodes = dtcPanelReadCodes
          OnClearCodes = dtcPanelClearCodes
          OnSelect = dtcPanelSelect
          TabOrder = 0
        end
        object cardDetails: TOBDCard
          AlignWithMargins = True
          Left = 0
          Top = 336
          Width = 732
          Height = 224
          Margins.Left = 0
          Margins.Top = 12
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Title = 'Code details'
          ShowFooter = True
          FooterText = 'Causes are typical; confirm the diagnosis before replacing parts.'
          StatusEdge = ceAccent
          TabOrder = 1
          object btnWeb: TOBDButton
            Left = 582
            Top = 16
            Width = 132
            Height = 32
            Caption = 'Search the web'
            Kind = bkSecondary
            Glyph = glGlobe
            OnClick = btnWebClick
            TabOrder = 0
          end
          object kvDetails: TOBDKeyValueList
            Left = 16
            Top = 58
            Width = 698
            Height = 122
            KeyWidth = 132
            ShowCategories = True
            WordWrap = True
            TabOrder = 1
          end
        end
      end
    end
    object pnlUDS: TPanel
      AlignWithMargins = True
      Left = 12
      Top = 736
      Width = 1184
      Height = 560
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 3
      Visible = False
      object gridUDS: TOBDDataGrid
        Left = 0
        Top = 0
        Width = 1184
        Height = 350
        Align = alTop
        DataColumns = <
          item
            Caption = 'Status'
            Width = 170
          end
          item
            Caption = 'Code'
            Width = 96
          end
          item
            Caption = 'Description / failure type'
            Width = 330
          end
          item
            Caption = 'Readiness group'
            Width = 150
          end
          item
            Caption = 'WWH class'
            Width = 90
          end
          item
            Caption = 'ECU'
            Width = 90
          end>
        EmptyTitle = 'No UDS codes'
        EmptyHint = 'Read the UDS view, or this vehicle may not support it.'
        EmptyGlyph = glCodes
        OnChange = gridUDSChange
        TabOrder = 0
      end
      object banUDS: TOBDBanner
        AlignWithMargins = True
        Left = 0
        Top = 362
        Width = 1184
        Height = 58
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Kind = bnInfo
        Title = 'UDS view not read yet'
        Text = 'Use Read codes in the All modules view to try SAE J1979-2 and WWH-OBD.'
        TabOrder = 1
      end
      object cardUDSDetails: TOBDCard
        AlignWithMargins = True
        Left = 0
        Top = 432
        Width = 1184
        Height = 128
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Title = 'UDS / WWH-OBD details'
        ShowFooter = True
        FooterText = 'UDS status bits and WWH classes are reported only by newer cars.'
        StatusEdge = ceAccent
        TabOrder = 2
        object kvUDS: TOBDKeyValueList
          Left = 16
          Top = 54
          Width = 1152
          Height = 52
          KeyWidth = 132
          ColumnCount = 2
          ShowCategories = True
          WordWrap = True
          TabOrder = 0
        end
      end
    end
  end
end
