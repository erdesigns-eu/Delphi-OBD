object LINForm: TLINForm
  Caption = 'LIN Protocol Example'
  ClientHeight = 500
  ClientWidth = 700
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object MainPanel: TPanel
    Align = alClient
    object TopPanel: TPanel
      Align = alTop
      Height = 50
      Color = clNavy
      ParentBackground = False
      object TitleLabel: TLabel
        Left = 16
        Top = 12
        Caption = 'LIN Protocol Example'
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
    object ConfigGroupBox: TGroupBox
      Left = 264
      Top = 57
      Width = 250
      Height = 90
      Caption = ' Configuration '
      object VersionCombo: TComboBox
        Left = 16
        Top = 24
        Width = 217
        Height = 21
        Style = csDropDownList
      end
      object BaudRateCombo: TComboBox
        Left = 16
        Top = 54
        Width = 217
        Height = 21
        Style = csDropDownList
      end
    end
    object DataGroupBox: TGroupBox
      Left = 8
      Top = 153
      Width = 684
      Height = 120
      Caption = ' Data Operations '
      object NodeAddressEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object DataIDEdit: TEdit
        Left = 152
        Top = 24
        Width = 121
        Height = 21
      end
      object DataValueEdit: TEdit
        Left = 288
        Top = 24
        Width = 377
        Height = 21
      end
      object ReadDataButton: TButton
        Left = 16
        Top = 58
        Width = 145
        Height = 25
        Caption = 'Read Data'
        OnClick = ReadDataButtonClick
      end
      object WriteDataButton: TButton
        Left = 168
        Top = 58
        Width = 145
        Height = 25
        Caption = 'Write Data'
        OnClick = WriteDataButtonClick
      end
    end
    object FrameGroupBox: TGroupBox
      Left = 8
      Top = 279
      Width = 684
      Height = 80
      Caption = ' Frame Configuration '
      object FrameIDEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object AssignFrameButton: TButton
        Left = 152
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Assign Frame ID'
        OnClick = AssignFrameButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 365
      Width = 684
      Height = 127
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
end
