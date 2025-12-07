object KWP2000Form: TKWP2000Form
  Left = 0
  Top = 0
  Caption = 'KWP2000 Protocol Example'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 581
    Align = alClient
    TabOrder = 0
    object TopPanel: TPanel
      Left = 1
      Top = 1
      Width = 798
      Height = 50
      Align = alTop
      Color = clNavy
      ParentBackground = False
      TabOrder = 0
      object TitleLabel: TLabel
        Left = 16
        Top = 12
        Width = 311
        Height = 29
        Caption = 'KWP2000 Protocol Example'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -24
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object ConnectionGroupBox: TGroupBox
      Left = 8
      Top = 57
      Width = 250
      Height = 100
      Caption = ' Connection '
      TabOrder = 1
      object PortLabel: TLabel
        Left = 16
        Top = 24
        Width = 22
        Height = 13
        Caption = 'Port'
      end
      object PortComboBox: TComboBox
        Left = 72
        Top = 21
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object ConnectButton: TButton
        Left = 16
        Top = 58
        Width = 97
        Height = 25
        Caption = 'Connect'
        TabOrder = 1
        OnClick = ConnectButtonClick
      end
      object DisconnectButton: TButton
        Left = 120
        Top = 58
        Width = 97
        Height = 25
        Caption = 'Disconnect'
        TabOrder = 2
        OnClick = DisconnectButtonClick
      end
    end
    object SessionGroupBox: TGroupBox
      Left = 264
      Top = 57
      Width = 250
      Height = 100
      Caption = ' Session Control '
      TabOrder = 2
      object SessionLabel: TLabel
        Left = 16
        Top = 24
        Width = 36
        Height = 13
        Caption = 'Session'
      end
      object SessionCombo: TComboBox
        Left = 16
        Top = 43
        Width = 217
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object StartSessionButton: TButton
        Left = 16
        Top = 70
        Width = 217
        Height = 25
        Caption = 'Start Session'
        TabOrder = 1
        OnClick = StartSessionButtonClick
      end
    end
    object SecurityGroupBox: TGroupBox
      Left = 520
      Top = 57
      Width = 272
      Height = 100
      Caption = ' Security Access '
      TabOrder = 3
      object SecurityLevelLabel: TLabel
        Left = 16
        Top = 24
        Width = 26
        Height = 13
        Caption = 'Level'
      end
      object KeyLabel: TLabel
        Left = 16
        Top = 54
        Width = 18
        Height = 13
        Caption = 'Key'
      end
      object SecurityLevelEdit: TEdit
        Left = 56
        Top = 21
        Width = 49
        Height = 21
        TabOrder = 0
        Text = '1'
      end
      object RequestSeedButton: TButton
        Left = 112
        Top = 19
        Width = 145
        Height = 25
        Caption = 'Request Seed'
        TabOrder = 1
        OnClick = RequestSeedButtonClick
      end
      object SendKeyButton: TButton
        Left = 112
        Top = 67
        Width = 145
        Height = 25
        Caption = 'Send Key'
        TabOrder = 2
        OnClick = SendKeyButtonClick
      end
      object KeyEdit: TEdit
        Left = 56
        Top = 51
        Width = 49
        Height = 21
        TabOrder = 3
      end
    end
    object DiagnosticsGroupBox: TGroupBox
      Left = 8
      Top = 163
      Width = 250
      Height = 145
      Caption = ' Diagnostics '
      TabOrder = 4
      object ReadDTCButton: TButton
        Left = 16
        Top = 24
        Width = 217
        Height = 25
        Caption = 'Read DTC'
        TabOrder = 0
        OnClick = ReadDTCButtonClick
      end
      object ClearDTCButton: TButton
        Left = 16
        Top = 55
        Width = 217
        Height = 25
        Caption = 'Clear DTC'
        TabOrder = 1
        OnClick = ClearDTCButtonClick
      end
      object ReadECUIDButton: TButton
        Left = 16
        Top = 86
        Width = 217
        Height = 25
        Caption = 'Read ECU ID'
        TabOrder = 2
        OnClick = ReadECUIDButtonClick
      end
      object TesterPresentButton: TButton
        Left = 16
        Top = 117
        Width = 217
        Height = 25
        Caption = 'Tester Present'
        TabOrder = 3
        OnClick = TesterPresentButtonClick
      end
    end
    object ECUProgrammingGroupBox: TGroupBox
      Left = 264
      Top = 163
      Width = 528
      Height = 145
      Caption = ' ECU Programming / Flashing '
      TabOrder = 5
      object AddressLabel: TLabel
        Left = 16
        Top = 24
        Width = 70
        Height = 13
        Caption = 'Start Address'
      end
      object SizeLabel: TLabel
        Left = 16
        Top = 54
        Width = 19
        Height = 13
        Caption = 'Size'
      end
      object AddressEdit: TEdit
        Left = 96
        Top = 21
        Width = 137
        Height = 21
        TabOrder = 0
        Text = '$00100000'
      end
      object SizeEdit: TEdit
        Left = 96
        Top = 51
        Width = 137
        Height = 21
        TabOrder = 1
        Text = '$00010000'
      end
      object RequestDownloadButton: TButton
        Left = 16
        Top = 86
        Width = 161
        Height = 25
        Caption = 'Request Download'
        TabOrder = 2
        OnClick = RequestDownloadButtonClick
      end
      object TransferDataButton: TButton
        Left = 184
        Top = 86
        Width = 161
        Height = 25
        Caption = 'Transfer Data'
        TabOrder = 3
        OnClick = TransferDataButtonClick
      end
      object RequestTransferExitButton: TButton
        Left = 352
        Top = 86
        Width = 161
        Height = 25
        Caption = 'Request Transfer Exit'
        TabOrder = 4
        OnClick = RequestTransferExitButtonClick
      end
    end
    object ResultMemo: TMemo
      Left = 8
      Top = 334
      Width = 784
      Height = 239
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 6
    end
    object ResultLabel: TLabel
      Left = 8
      Top = 315
      Width = 79
      Height = 13
      Caption = 'Result / Activity:'
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
