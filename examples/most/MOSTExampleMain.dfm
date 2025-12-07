object MOSTForm: TMOSTForm
  Caption = 'MOST Protocol Example'
  ClientHeight = 650
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
        Caption = 'MOST Protocol Example'
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
      Height = 120
      Caption = ' Configuration '
      object VersionCombo: TComboBox
        Left = 16
        Top = 24
        Width = 217
        Height = 21
        Style = csDropDownList
      end
      object NodeAddressEdit: TEdit
        Left = 16
        Top = 54
        Width = 217
        Height = 21
      end
      object FunctionBlockCombo: TComboBox
        Left = 16
        Top = 84
        Width = 217
        Height = 21
        Style = csDropDownList
      end
    end
    object PropertyGroupBox: TGroupBox
      Left = 8
      Top = 183
      Width = 684
      Height = 120
      Caption = ' Property Operations '
      object PropertyIDEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object PropertyValueEdit: TEdit
        Left = 152
        Top = 24
        Width = 377
        Height = 21
      end
      object GetPropertyButton: TButton
        Left = 16
        Top = 58
        Width = 145
        Height = 25
        Caption = 'Get Property'
        OnClick = GetPropertyButtonClick
      end
      object SetPropertyButton: TButton
        Left = 168
        Top = 58
        Width = 145
        Height = 25
        Caption = 'Set Property'
        OnClick = SetPropertyButtonClick
      end
    end
    object StreamingGroupBox: TGroupBox
      Left = 8
      Top = 309
      Width = 684
      Height = 90
      Caption = ' Streaming Operations '
      object ChannelIDEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object RequestChannelButton: TButton
        Left = 152
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Request Channel'
        OnClick = RequestChannelButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 405
      Width = 684
      Height = 237
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
