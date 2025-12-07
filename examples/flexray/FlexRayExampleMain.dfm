object FlexRayForm: TFlexRayForm
  Caption = 'FlexRay Protocol Example'
  ClientHeight = 600
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
        Caption = 'FlexRay Protocol Example'
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
      object SpeedCombo: TComboBox
        Left = 16
        Top = 24
        Width = 217
        Height = 21
        Style = csDropDownList
      end
      object ChannelCombo: TComboBox
        Left = 16
        Top = 54
        Width = 217
        Height = 21
        Style = csDropDownList
      end
      object ConfigureButton: TButton
        Left = 16
        Top = 84
        Width = 217
        Height = 25
        Caption = 'Configure FlexRay'
        OnClick = ConfigureButtonClick
      end
    end
    object StaticGroupBox: TGroupBox
      Left = 8
      Top = 183
      Width = 684
      Height = 90
      Caption = ' Static Segment '
      object StaticSlotEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object SendStaticButton: TButton
        Left = 152
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Send Static Frame'
        OnClick = SendStaticButtonClick
      end
    end
    object DynamicGroupBox: TGroupBox
      Left = 8
      Top = 279
      Width = 684
      Height = 90
      Caption = ' Dynamic Segment '
      object DynamicSlotEdit: TEdit
        Left = 16
        Top = 24
        Width = 121
        Height = 21
      end
      object SendDynamicButton: TButton
        Left = 152
        Top = 22
        Width = 145
        Height = 25
        Caption = 'Send Dynamic Frame'
        OnClick = SendDynamicButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 375
      Width = 684
      Height = 217
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
