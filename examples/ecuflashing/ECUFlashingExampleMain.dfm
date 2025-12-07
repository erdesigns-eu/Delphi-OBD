object ECUFlashingForm: TECUFlashingForm
  Caption = 'ECU Flashing Example'
  ClientHeight = 700
  ClientWidth = 700
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object MainPanel: TPanel
    Align = alClient
    object TopPanel: TPanel
      Align = alTop
      Height = 50
      Color = clMaroon
      ParentBackground = False
      object TitleLabel: TLabel
        Left = 16
        Top = 12
        Caption = 'ECU Flashing Example'
        Font.Color = clWhite
        Font.Height = -24
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object ConnectionGroupBox: TGroupBox
      Left = 8
      Top = 57
      Width = 250
      Height = 90
      Caption = ' Connection '
      object PortComboBox: TComboBox
        Left = 16
        Top = 24
        Width = 145
        Height = 21
        Style = csDropDownList
      end
      object ConnectButton: TButton
        Left = 16
        Top = 54
        Width = 97
        Height = 25
        Caption = 'Connect'
        OnClick = ConnectButtonClick
      end
      object DisconnectButton: TButton
        Left = 120
        Top = 54
        Width = 97
        Height = 25
        Caption = 'Disconnect'
        OnClick = DisconnectButtonClick
      end
    end
    object FlashingGroupBox: TGroupBox
      Left = 8
      Top = 153
      Width = 684
      Height = 130
      Caption = ' Flash File '
      object FilePathEdit: TEdit
        Left = 16
        Top = 24
        Width = 545
        Height = 21
        ReadOnly = True
      end
      object BrowseButton: TButton
        Left = 570
        Top = 22
        Width = 97
        Height = 25
        Caption = 'Browse...'
        OnClick = BrowseButtonClick
      end
      object ProgressBar: TProgressBar
        Left = 16
        Top = 58
        Width = 651
        Height = 17
      end
      object FlashButton: TButton
        Left = 16
        Top = 88
        Width = 145
        Height = 25
        Caption = 'Flash ECU'
        OnClick = FlashButtonClick
      end
      object VerifyButton: TButton
        Left = 168
        Top = 88
        Width = 145
        Height = 25
        Caption = 'Verify Flash'
        OnClick = VerifyButtonClick
      end
    end
    object InfoGroupBox: TGroupBox
      Left = 8
      Top = 289
      Width = 684
      Height = 150
      Caption = ' ECU Information '
      object ECUInfoMemo: TMemo
        Left = 16
        Top = 48
        Width = 651
        Height = 89
        Font.Name = 'Courier New'
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
      end
      object GetInfoButton: TButton
        Left = 16
        Top = 16
        Width = 145
        Height = 25
        Caption = 'Get ECU Info'
        OnClick = GetInfoButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 445
      Width = 684
      Height = 247
      Font.Name = 'Courier New'
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
    end
  end
  object StatusBar: TStatusBar
    Align = alBottom
    SimplePanel = True
  end
  object OpenDialog: TOpenDialog
  end
end
